/* lexparse -- Lex and parse a text regular expression into tokens

   This is a very rudimentary framework, with only a tiny fraction of
   the flexibility of a full Grep program.  Hopefully, it is
   sufficiently capable and fair in its operation that some meaningful
   comparisons of search algorithms can be made.  */

/* Copyright (C) 1992, 1997-2002, 2004-2015 Free Software Foundation,
   Inc.
   Copyright (C) 2015 Grouse Software

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc.,
   51 Franklin Street - Fifth Floor, Boston, MA  02110-1301, USA */

/* This code is, to a significant extent, based on the code in GNU
   Grep, especially the infrastructure code in dfa.c, which has the
   attribution:
        Written July 1992 by Mike Haertel.
   Many others have contributed to the sources.  See the files AUTHORS
   and THANKS, as well as the git changelog, in the GNU Grep package
   for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

#include <config.h>

#include <assert.h>
#include "charclass.h"
#include <ctype.h>
#include "hstbm.h"
#include "lexparse.h"
#include <limits.h>
#include "misc-gettext.h"
#include "pattern-lex.h"
#include "program-trouble.h"
#include <regex.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <wctype.h>

#define NR_CHARS (UCHAR_MAX + 1)

/* Maximum size of the pattern string, in characters.  */
#define PATTERN_LEN_MAX 4096

/* Maximum name length for named classes such as "digit"
   ([:digit:]), including NUL terminator.  */
#define NAMED_CLASS_LEN_MAX      32

/* Prototype for a funtion to add a character to a set.  */
typedef void
add_char_fn_t (int ch, charclass_t *ccl);

/* Add a character, including scanning and adding all octets in the
   set where case folding applies (toupper (c) == toupper (i)).  */
static void
add_folded_char (int ch, charclass_t *ccl)
{
  wint_t wuc;
  wint_t wi;

  /* Add the character, any upper-case equivalent, plus all characters
     that map to the upper-case character to the class.  We use wide
     chars here so that we can use the locale-sensitive mapping
     function towupper.  */
  wuc = towupper ((wint_t) ch);
  charclass_setbit (ch, ccl);
  charclass_setbit ((int) wuc, ccl);
  for (wi = 0; wi < NR_CHARS; wi++)
    if (towupper (wi) == wuc)
      charclass_setbit ((int) wi, ccl);
}

static bool
add_equivalence_class (charclass_t *ccl,
                       unsigned long *p_lexleft,
                       unsigned char **p_lexptr,
                       lexparse_match_options_t *match_opt)
{
  long lexleft;
  unsigned char *lexptr;
  char regex_str[8];
  char haystack[2];
  regex_t regex_work;
  int regex_comp_flags;
  int i;
  int result;

  /* Bring reference-named parameters into local variables.  */
  lexleft = *p_lexleft;
  lexptr  = *p_lexptr;

  /* There must be at least "<octet>=]" in the character stream, for
     a single-char/byte/octet value of "<octet>", so anything
     less than 3 characters left is incorrect.  */
  if (lexleft < 3)
    program_trouble_fatal
      (_("Malformed equivalence class: No suitable"
         " '=]' end-of-class sequence found"));

  /* Check that the end-of-equivalence-class sequence "=]" appears
     after the char, and complain if not present.  */
  if ((lexptr[1] != '=') || (lexptr[2] != ']'))
    program_trouble_fatal
      (_("Malformed equivalence class: '=]'"
         " terminator not found at required location"));

  /* Prepare string for regex compilation, including NUL
     terminator.  */
  strcpy (regex_str, "[[=x=]]");

  /* Set up regex compilation flags.  */
  regex_comp_flags = REG_NOSUB | REG_EXTENDED;
  if (match_opt->case_insensitive)
    regex_comp_flags |= REG_ICASE;

  /* Work through class members, searching entire character space
     for equivalence-equal characters, and adding these characters
     to the set.  */
  regex_str[3] = lexptr[0];
  result = regcomp (&regex_work, regex_str, regex_comp_flags);
  if (result != 0)
    program_trouble_fatal
      (_("Equivalence class compilation failed: %d"), result);

  /* Haystack is always a 1-char string, so set up NUL in second
     position now.  */
  haystack[1] = '\0';

  for (i = 0; i < NR_CHARS; i++)
    {
      haystack[0] = i;
      if (regexec (&regex_work, haystack, 0, NULL, 0) == 0)
        {
          charclass_setbit (i, ccl);
        }
    }
  regfree (&regex_work);

  /* Skip over the equivalence character and the '=]' terminator.  */
  *p_lexleft = lexleft - 3;
  *p_lexptr  = lexptr + 3;

  /* Write updated pattern context to caller and report success.  */
  /* ?? Equiv classes can only appear inside conventional classes,
     e.g. "[[=e=][=^=]]"... so there must be at least one character
     to consume after we're done, or there's an error.  However,
     it's not clear that we should diagnose that case here, which
     is the practical upshot of the > 0 test below.  */
  return lexleft > 0;
}

static bool
add_named_class (charclass_t *ccl,
                 unsigned long *p_lexleft,
                 unsigned char **p_lexptr,
                 add_char_fn_t *add_char_fn)
{
  long lexleft;
  unsigned char *lexptr;
  char class_name[NAMED_CLASS_LEN_MAX];
  char *p_buf;
  wctype_t class_property;
  wint_t i;

  /* Copy pass-by-reference pattern details into local variables.  */
  lexleft = *p_lexleft;
  lexptr  = *p_lexptr;

  /* Extract name into a separate buffer so we can call wctype ().  */
  p_buf = class_name;
  for (;;)
    {
      /* There must be ":]" terminating the name, so if the pattern is
         already shorter than this, then there is a syntax error.  */
      if (lexleft < 2)
        program_trouble_fatal
          (_("Named class in pattern is missing "
             "':]' terminator"));

      /* If we've found a ":]" sequence, then the name is complete.  */
      if ((lexptr[0] == ':') && (lexptr[1] == ']'))
        break;

      /* If the name is too long, there is a syntax error.  */
      if ((p_buf - class_name) >= (sizeof (class_name) - 1))
        program_trouble_fatal
          (_("Class name is too long to be reasonable"));

      /* Okay, add the character to the buffer.  */
      *p_buf++ = *lexptr++;
      lexleft--;
    }
  *p_buf++ = '\0';

  /* Okay, we have a name, now convert it to a property token.  */
  class_property = wctype (class_name);
  if (class_property == (wctype_t) 0)
    program_trouble_fatal (_("Invalid character class name"));

  /* NOTE: POSIX says that locale-specific names may be defined.  The
     code here works with any name that wctype says is valid
     (according to the current locale), instead of exactly the class
     names documented to be supported by grep.  I'm not sure if this
     difference will be visible (the single-octet locale restriction
     might limit opportunities for differences to arise), but am
     documenting this here just in case.  */

  /* Now work through all octets, adding class members to the set.  */
  for (i = 0; i < NR_CHARS; i++)
    if (iswctype (i, class_property))
      add_char_fn ((int) i, ccl);

  /* Skip over the ':]' that we identified during the loop.  */
  lexptr  += 2;
  lexleft -= 2;

  /* Write updated pattern context to caller and report whether
     there's any more pattern text available -- no text is an error,
     as we need to see at least the class closing ']'.  */
  *p_lexleft = lexleft;
  *p_lexptr  = lexptr;
  return lexleft > 0;
}

  /* Try to replicate a subset of GNU Grep's character-class handling
     here, not supporting everything, but trying to support some
     common cases, and also trying to throw an assertion whenever a
     reasonable user would think that a feature was supported, but is
     not.

     The return boolean reports whether the class is inverted.  We
     not do the inversion here, as capturing this information at a
     high level of abstraction (our client can use the
     OP_CLASS_INVERTED opcode) can lead to better choices elsewhere,
     and also means that we don't need to assume and/or be told what
     the invariant (end-of-line) characters are.  */
static bool
handle_class_body (charclass_t *ccl,
                   unsigned long *p_lexleft,
                   unsigned char **p_lexptr,
                   lexparse_match_options_t *match_opt,
                   add_char_fn_t *add_char_fn)
{
  unsigned long lexleft;
  unsigned char *lexptr;
  int ch;
  int ch_ahead;
  bool invert_set;

  /* Receive context of pattern lexing from caller.  */
  lexleft = *p_lexleft;
  lexptr  = *p_lexptr;

  /* We use integer variables ch and ch_ahead so that when code tries
     to perform a look-ahead function as part of parsing, we can
     intercept potential accesses past the end of the pattern, and
     replace the putative character with EOF, which will send an
     unique signal to the code.  */

  if (lexleft == 0)
    program_trouble_fatal
      (_("Sorry, class body not found (no closing bracket)"));

  /* Set up other variables used to manage the set.  */
  invert_set = false;
  ch = EOF;
  ch_ahead = EOF;

  lexleft--;
  ch_ahead = *lexptr++;
  if (ch_ahead == '^')
    {
      invert_set = true;
      ch_ahead = EOF;
      if (lexleft != 0)
        {
          lexleft--;
          ch_ahead = *lexptr++;
        }
    }

  /* Treat closing bracket as a set member if it occurs in the first
     position (after optional '^' for set inversion).
     ?? CHECKME: We assume that  no case-insensitive folding applies
     to this char.  */
  if (ch_ahead == ']')
    {
      charclass_setbit ((int) ch_ahead, ccl);
      ch_ahead = EOF;
      if (lexleft != 0)
        {
          lexleft--;
          ch_ahead = *lexptr++;
        }
    }

  for (;;)
    {
      /* Advance to the next character, updating lookahead character,
         and being careful not to overstep pattern memory
         boundaries.  */
      ch = ch_ahead;
      if (ch == EOF)
        break;
      if (ch == ']')
        break;
      ch_ahead = EOF;
      if (lexleft != 0)
        {
          lexleft--;
          ch_ahead = *lexptr++;
        }

      if (ch == '[')
        {
          switch (ch_ahead)
            {
            case '=':
              if (! add_equivalence_class (ccl,
                                           &lexleft, &lexptr,
                                           match_opt))
                program_trouble_fatal
                  (_("Malformed equivalence class -- "
                     "expected something like [=e=] inside []"));
              break;
            case ':':
              if (! add_named_class (ccl,
                                     &lexleft, &lexptr,
                                     add_char_fn))
                program_trouble_fatal
                  (_("Problem dealing with named class -- "
                     "expected something like [:digit:] inside []"));
              break;
            case '.':
              program_trouble_fatal
                (_("Sorry, collation classes (e.g. [.y.]) "
                   "not supported"));
              break;
            default:
              /* Just treat '[' as an individual character, a member
                 of the class; possibly the start of a range.  */
              goto NotABracketExp;
            }

          /* Prime loop with the next character after the subset.  */
          ch_ahead = EOF;
          if (lexleft)
            {
              lexleft--;
              ch_ahead = *lexptr++;
            }
          continue;
        }

    NotABracketExp:
      /* Deal with ranges (or, if the dash comes last, treat it as
         a literal.  */
      if (ch_ahead == '-')
        {
          /* If there's not at least one more character (e.g closing
             bracket), then this is a syntax error.  */
          if (lexleft == 0)
            break;
          lexleft--;
          ch_ahead = *lexptr++;
          if (ch_ahead == ']')
            {
              /* Dash is last, and the character preceding it is not
                 in fact a member of a range, so add them directly,
                 then finish class parsing.  */
              add_char_fn ((int) '-', ccl);
              add_char_fn ((int) ch, ccl);
              goto finished_class;
            }
          else
            {
              /* We have a range from ch to ch_ahead. */
              int i;

              /* ?? FIXME:  The start or end of a range could be a
                 non-trivial component, such as an equivalence class.
                 This is an error that we do not try to detect or
                 handle at present, sigh.  */

              /* If there's no text after the range end, the syntax
                 is invalid, so check this before working on the
                 range.  */
              if (lexleft == 0)
                {
                  ch = EOF;
                  break;
                }

              /* If ord (start) > ord (end), this is an invalid
                 makerange.  */
              if (ch > ch_ahead)
                program_trouble_fatal (_("Invalid range end"));

              /* ?? FIXME:  Cannot have a non-trivial class
                 component (e.g. named class, equivalence class,
                 collation class or range) as either a start or an
                 end of a range.  Gnulib's regcomp build_range_exp
                 checks properly for these cases; we do not.  */

              /* ?? PERFORMANCE NOTE:  GNU Grep uses regcomp as a
                 very comprehensive check of the regular expression
                 before working on it in dfa/kwset/bmsearch.  We do
                 not check the expression as comprehensively; and so
                 may be artifically faster due to this (potential)
                 sloppiness.  I suspect that this difference will be
                 mostly noticeable when the haystack is very small
                 (perhaps less than a few kB); after the haystack
                 size grows a little, the search effort will tend to
                 dominate the setup/compilation/checking effort.
                 In any case, be wary of speed comparisons for small
                 files; the playing field isn't comletely equal.
                 In the longer term, this may be an area where
                 GNU Grep might benefit from optimisation.  */

              /* Enumerate each member of the range, in turn.  */
              for (i = ch; i <= ch_ahead; i++)
                add_char_fn (i, ccl);

              /* Advance past the end-of-range character.  */
              ch = ch_ahead;
              ch_ahead = EOF;
              if (lexleft != 0)
                {
                  lexleft--;
                  ch_ahead = *lexptr++;
                }

            }
          continue;
        }

      /* If no special cases apply, simply add the character itself
         to the set.  */
      add_char_fn ((int) ch, ccl);
    }

  if (ch != ']')
    program_trouble_fatal (_("Unmatched [ or [^"));

 finished_class:
  /* Ensure end-of-line bytes are not members of the class.  */
  {
    int i;

    for (i = 0; i < match_opt->nr_endline_bytes; i++)
      charclass_clrbit (match_opt->endline_bytes[i], ccl);
  }

  /* Write lexleft/lexptr pattern consumption details back to
     caller, along with the status of the set-inversion flag.  */
  *p_lexleft = lexleft;
  *p_lexptr  = lexptr;
  return invert_set;
}

/* Slightly-more-sophisticated pattern processing.  The pattern is
   analysed, and broken into a list of tokens.  Pattern_len,
   pattern and match_case_insensitive are the inputs; the token
   list (and length), plus the char classes associated with each
   pattern position are the outputs.  The last position of the
   pattern (skipchar_class) is treated as independent of earlier
   positions, as this may allow hstbm to implement some
   optimisations.

   The handling of sets of characters has gone through an
   excruciating number of iterations, and the comments and the code
   are likely to be inconsistent in various places.  At the time of
   writing, the fold_sets module is being retired in favour of
   charclass, mainly as set inclusion/exclusion tests for hstbm's
   delta3/4 arrays are easier to write with unary (bitset)
   representation than with list-of-characters representation.  */
bool
lexparse_tokenise (int pattern_len,
                   char *pattern,
                   lexparse_lex_options_t *lex_opt,
                   lexparse_match_options_t *match_opt,
                   compiled_pattern_t *compiled_pattern)
{
  charclass_t **charclass_list;
  pattern_lex_token_t *p_token;
  size_t lexleft;
  unsigned char *lexptr;
  int ch;
  int ch_ahead;
  add_char_fn_t *add_char_fn;

  /* Check that the compiled pattern points to a valid instance.  */
  compiled_pattern_check (compiled_pattern);

  /* Write default return values in case we return without producing
     a sane result.  */
  compiled_pattern->nr_tokens = 0;
  p_token = compiled_pattern->token_list;

  /* Pick up the allocated charclass list from the context.  */
  charclass_list = compiled_pattern->charclass_list;

  /* Ignore pattern if it's length is unreasonable... perhaps we
     should assert if length is negative.  */
  if ((pattern_len <= 0) || (pattern_len > PATTERN_LEN_MAX))
    {
      return false;
    }

  /* Select function to add a character to a set, so that if
     case-insensitive matching is requested, we enumerate all
     folding equivalents of a character and add all of them to the
     set.  */
  add_char_fn = charclass_setbit;
  if (match_opt->case_insensitive)
    add_char_fn = add_folded_char;

  /* Work through pattern, charater-by-character, with 1-character
     lookahead.  */
  lexptr = (unsigned char *) pattern;
  lexleft = pattern_len;

  /* Precharge the lookahead variables with the first character.  */
  ch_ahead = *lexptr++;
  lexleft--;

  /* Work through the characters of the pattern.  */
  while (ch_ahead != EOF)
    {
      charclass_t *ccl = NULL;
      charclass_index_t class_index;
      charclass_t *p_class_enduring = NULL;
      int member_count;
      bool invert_set;

      /* Classes are only inverted after explicitly processing
         charclass syntax; we still produce sets for all other
         characters, but none of them are inverted.  */
      invert_set = false;

      /* Advance to the next character, updating lookahead character,
         and being careful not to overstep pattern memory
         boundaries.  */
      ch = ch_ahead;
      ch_ahead = EOF;
      if (lexleft)
        {
          lexleft--;
          ch_ahead = *lexptr++;
        }

      /* Always treat the next pattern item  as a set, even if it's a
         singleton character, as we depend on set-based consistency
         checking to spot some cases that fall outside the simple
         static mapping table(s) used by other algorithms.  If we find
         that a set ends up only having one member, we select a
         single-character opcode, rather than a set-reference opcode,
         so we can optimise some degenerate cases, e.g. [aaa-a].  */
      ccl = charclass_alloc ();
      assert (ccl);

      switch (ch)
        {
        case '\\':
          {
            int quoted_ch;

            quoted_ch = ch;
            if (ch_ahead != EOF)
              {
                quoted_ch = ch_ahead;
                ch_ahead = EOF;
                if (lexleft)
                  {
                    lexleft--;
                    ch_ahead = *lexptr++;
                  }
              }
            add_char_fn (quoted_ch, ccl);
          }
          break;

        case '[':
          /* If '[' does not (lexically) introduce a character
             class, treat it merely as a normal (literal) character,
             subject to case-insensitivity processing as appropriate
             (we could second-guess/optimise out case insensitivity
             code if we knew that it was irrelevant, but choose not
             to, to avoid surprises if an assumption was wrong or
             turns out to be invalidated by future changes.  */
          if (! lex_opt->character_classes)
            goto normal_character;

          /* Undo lookahead before giving control to
             handle_class_body ().  */
          lexleft++;
          lexptr--;
          invert_set = handle_class_body (ccl,
                                          &lexleft, &lexptr,
                                          match_opt,
                                          add_char_fn);
          ch_ahead = EOF;
          if (lexleft != 0)
            {
              lexleft--;
              ch_ahead = *lexptr++;
            }
          break;

        case EOF:
          program_trouble_fatal (_("should not see EOF here..."));
          /* NOTREACHED */
          break;

        default:
normal_character:
          add_char_fn (ch, ccl);
          break;
        }

      /* Complete the class associated with this pattern position,
         and obtain an enduring pointer to the class.  */
      class_index = charclass_completed (ccl);
      p_class_enduring = charclass_get_pointer (class_index);
      member_count = charclass_bitcount (ccl);

      /* ?? FIXME: We should check that we don't overrun list sizes
         here... at present, we reduce the chances of this by
         significantly overallocating list space, so that, together
         with the knowledge that the initial token and charclass
         processing is very simple, means that we are safe.  This
         will need to be reviewed if this code becomes more
         sophisticated.  */

      if (invert_set)
        /* Report inverted set directly as a class token.  */
        *p_token++ = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_CHAR_CLASS_INVERTED,
                                        class_index);
      else if (member_count != 1)
        /* Report multi-member set directly as a class token.  */
        *p_token++ = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_CHAR_CLASS,
                                          class_index);
      else
        {
          /* Single-member set:  Find the individual and generate a
             single-char token.  */
          if (! charclass_next_member (ccl, 0, &ch))
            program_trouble_fatal (_("charclass member counting/reporting"
                            " is inconsistent."));
          *p_token++
            = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_CHAR_DIRECT, ch);
        }

      /* Always add the class to the list, regardless of whether:
         (a) It is only a single-character class; and/or
         (b) It is the last class of the pattern, which does not
             need to be compatible with earlier classes.
         This comprehensive list of charclasses is valuable for
         hstbm's delta 3/4 set testing.  */
      *charclass_list++ = p_class_enduring;

    }

  /* Write created token list back to caller.  */
  compiled_pattern->nr_tokens = p_token
    - compiled_pattern->token_list;

  /* ?? NOTE: We deliberately only support patterns where there is a
     one-to-one mapping between tokens and octets, so that hstbm is
     easy to support.  This is a restriction that is reasonable for
     the experimental release, but will become inadequate if this
     code is to become more general in its pattern handling.  */
  compiled_pattern->pattern_len = compiled_pattern->nr_tokens;

  /* Although not required, append a NULL pointer to the charclass
     list, as it might help a developer inspecting the code with a
     debugger (clients use pattern_len as the list length).  */
  *charclass_list = NULL;

  return true;
}

/* Convert any CHAR_CLASS_INVERTED tokens found in the token stream
   into CHAR_CLASS tokens, including inverting the member bits of
   the class (excluding the end-of-line bytes nominated in the
   options).  The token in the token list is rewritten with the
   changed opcode and the parameter changed to reference a new
   class, an inverse of the original class.  We need to create a new
   class as the original had charclass_completed performed on it,
   and so must not be altered.

   The functions returns TRUE if any classes were modified.  */
bool
lexparse_normalise_inverted_classes (lexparse_match_options_t
                                        *match_opt,
                                     compiled_pattern_t
                                        *compiled_pattern)
{
  bool class_modified;
  int i;
  pattern_lex_token_t *p_token;
  charclass_t **charclass_list;

  /* Check that the compiled pattern points to a valid instance.  */
  compiled_pattern_check (compiled_pattern);

  class_modified = false;

  /* Get working pointers to lists, offset by 1 as we increment at
     the top of the loop.  */
  p_token = compiled_pattern->token_list - 1;
  charclass_list = compiled_pattern->charclass_list - 1;

  /* Work through the list of tokens, looking for inverted
     classes.  */
  for (i = 0; i < compiled_pattern->nr_tokens; i++)
    {
      pattern_lex_token_t token;
      pattern_lex_opcode_t opcode;
      pattern_lex_param_t param;
      charclass_t *ccl;
      charclass_t *work_ccl;
      int j;
      charclass_index_t class_ref;

      /* Move to the next token/class pair in the pattern.  */
      p_token++;
      charclass_list++;

      /* Extract the opcode, and ignore the token if it isn't an
         inverted unibyte class.  */
      token = *p_token;
      opcode = PATTERN_LEX_GET_OPCODE (token);
      if (opcode != PATTERN_LEX_OP_CHAR_CLASS_INVERTED)
        continue;

      class_modified = true;

      /* Get reference, then pointer, to existing class.  */
      param = PATTERN_LEX_GET_PARAM (token);
      ccl = charclass_get_pointer (param);

      /* Create a new work class, copy the old class over, then
         use the work class to build the inverted class.  */
      work_ccl = charclass_alloc ();
      charclass_copyset (ccl, work_ccl);
      charclass_notset (work_ccl);

      /* Remove the end-of-line markers from the inverted set.  */
      for (j = 0; j < match_opt->nr_endline_bytes; j++)
        charclass_clrbit (match_opt->endline_bytes[j], work_ccl);

      /* Finish off the modified class, then edit the token stream
         to reference the changed class, and also edit the charclass
         list to reflect the change.  */
      class_ref = charclass_completed (work_ccl);
      *p_token = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_CHAR_CLASS,
                                    class_ref);
      ccl = charclass_get_pointer (class_ref);
      *charclass_list = ccl;
    }

  return class_modified;
}

/* vim:set shiftwidth=2: */
