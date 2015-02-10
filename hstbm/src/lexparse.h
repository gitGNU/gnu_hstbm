/* lexparse -- Lex and parse a text regular expression into tokens

   This is a very rudimentary framework, with only a tiny fraction of
   the flexibility of a full Grep program.  Hopefully, it is
   sufficiently capable and fair in its operation that some meaningful
   comparisons of search algorithms can be made.
*/

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

#ifndef LEXPARSE_H
#define LEXPARSE_H

#include "charclass.h"
#include "compiled-pattern.h"
#include "hstbm.h"
#include "pattern-lex.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Structures containing pattern lexing matching directives,
   packaged together to permit clients to hand them around easily.

   The lexing options refer to whether certain characters and/or
   character sequences can have special meaning, e.g. '^' for
   anchor-to-start-of-line, '|' for alternation, perhaps '\(', '\)'
   and '\1', '\2' etc. for grouping... the list goes on.   */
typedef struct lexparse_lex_options_struct {
  /* ?? multibyte locales (when lexing) */

  bool character_classes;
  /* ?? Allow inverted character classes?  (yes)  */

  /* ?? Allow equivalence classes?  (yes)  */
  /* ?? Allow collation classes (no)  */

  /* ?? Allow grep's "classic"/POSIX named classes?  (yes)  */
  /* ?? Allow locale-specific named classes?  (yes)  */

  /* ?? Allow '(' .. ')' grouping? */
  /* ?? Allow '\\''(' .. '\\'')' grouping? */
  /* ?? Allow backreferences? */

  /* ...and more could go here... */
} lexparse_lex_options_t;

/* Structure containing matching directives, packaged together to
   permit clients to hand them around easily.  The matching
   options describe how to treat cases where some pattern text can
   have ambiguity, such as whether 'a' merely matches 'a', or
   whether (case insensitive) it matches '[Aa]'.

   We anticipate that there typically be one end-of-line character,
   NL, but allow for the possibility of NUL and/or CR to be line
   terminators as well (or possibly instead).  */
#define LEXPARSE_MAX_ENDLINE_BYTES  3

/* ?? Defining what a "byte" is is becoming a real pain... most of
   the time we mean "octet". but this is not true on some
   architectures (e.g. DSP may define char as 32-bits; the C
   standard requires that sizeof (char) == 1, regardless of how many
   bits are in char (6? 7? 8? 16? 32? 64?).  */
typedef unsigned char lexparse_byte_t;

typedef struct lexparse_match_options_struct {
  bool case_insensitive;

  /* List of bytes terminating a line of text.  */
  int nr_endline_bytes;
  lexparse_byte_t endline_bytes[LEXPARSE_MAX_ENDLINE_BYTES];

  /* ?? multibyte locales (when matching)? */

  /* ...more might go here; interaction between a "lex"
     option/directive and a "match" option/directive needs to be
     handled carefully...  */

} lexparse_match_options_t;

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
extern bool
lexparse_tokenise (int pattern_len,
                   char *pattern,
                   lexparse_lex_options_t *lex_opt,
                   lexparse_match_options_t *match_opt,
                   compiled_pattern_t *compiled_pattern);

/* Convert any CHAR_CLASS_INVERTED tokens found in the token stream
   into CHAR_CLASS tokens, including inverting the member bits of
   the class (excluding the end-of-line bytes nominated in the
   options).  The token in the token list is rewritten with the
   changed opcode and the parameter changed to reference a new
   class, an inverse of the original class.  We need to create a new
   class as the original had charclass_completed performed on it,
   and so must not be altered.

   The functions returns TRUE if any classes were modified.  */
extern bool
lexparse_normalise_inverted_classes (lexparse_match_options_t
                                        *match_opt,
                                     compiled_pattern_t
                                        *compiled_pattern);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* LEXPARSE_H */
/* vim:set shiftwidth=2: */
