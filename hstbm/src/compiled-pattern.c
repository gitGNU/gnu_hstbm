/* compiled-pattern -- Broker module for users of tokenised patterns

   Copyright (C) 1988, 1998, 2000, 2002, 2004-2005, 2007-2015 Free Software
   Foundation, Inc.
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

/*
   This module acts as the central broker for all modules that work
   with compiled patterns (tokenised, plus token-related resources,
   possibly plus additional information to summarise content).

   It gathers pieces that describe patterns (token opcodes/params
   from pattern-lex, charclass lists, and, in the future, other
   pattern items such as wide-char descriptions), provides resources
   for managing memory for these resources in cases where the
   original provider doesn't supply the service or uses a different,
   inconvenient and/or hidden storage layout, and presents the
   integrated whole to consumers such as pattern
   analysis/optimisation services, and various search engines.

   This module's origins can partially, if not mostly, be traced
   back to the code in GNU Grep, especially dfa.c.  The contributors
   to that file are many and varied; an early copyright notice is:

          Written June, 1988 by Mike Haertel
          Modified July, 1988 by Arthur David Olson to assist
          BMG speedups

   In addition, many, many others have contributed to GNU Grep in
   ways that are recorded in various places, including the AUTHORS
   file and version control changelogs.  This module is a descendent
   of the GNU Grep files, and all the contributors are gratefully
   acknowledged here.

   This variant written in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.
*/

/* Define major environmental things arising from GNULib and/or
   autotools/autoconf etc.  */
#include <config.h>

#include "compiled-pattern.h"
#include <stdio.h>

/* Container for compiled_pattern struct, which adds top/bottom
   fences and an instance-tracking linked list to the struct
   advertised to the client.  This lets us issue and receive the
   structure expected by the client, but maintain hidden variables
   that might help us to detect and report problematic cases earlier
   and more directly than could happen otherwise; the hope is that
   this could improve program robustness.  */
typedef struct container_compiled_pattern_struct
               container_compiled_pattern_t;
struct container_compiled_pattern_struct {
  /* Signature (and potential canary) identifying the top of the
     structure.  */
  unsigned long top_fence_magic_nr;

  /* "Public" structure seen by the client.  We use offetof ()
     and/or type casting to convert between public and expanded
     (private?) versions of the struct.  */
  compiled_pattern_t public;

  /* Linked list of instances created by this module.  */
  container_compiled_pattern_t *next_instance;

  /* Signature/canary token for bottom of structure.  */
  unsigned long bottom_fence_magic_nr;
};
#define CTXT_TOP_FENCE_MAGIC_NR     0x6c040c32uL
#define CTXT_BOTTOM_FENCE_MAGIC_NR  0x10ddc58cuL
#define CHECK_CONTEXT(ctxt) \
  assert (((ctxt) != NULL)  \
   && ((ctxt)->top_fence_magic_nr    == CTXT_TOP_FENCE_MAGIC_NR)  \
   && ((ctxt)->bottom_fence_magic_nr == CTXT_BOTTOM_FENCE_MAGIC_NR))

/* Macro to convert a public pointer to its corresponding container
   structure.  */
#define CONVERT_PUBLIC_TO_CONTAINER(ptr) \
  ((container_compiled_pattern_t *) (((char *) (ptr))                    \
             - offsetof (container_compiled_pattern_t, public)))

/* Linked list of instances created by this module.  We discard any
   outstanding instances via atexit ().  Note that this list uses
   the container view of each instance, not the public view.  */
static container_compiled_pattern_t *module_instances_list = NULL;

#include <config.h>

#include <assert.h>
#include "charclass.h"
#include <ctype.h>
#include <limits.h>
#include "misc-gettext.h"
#include "pattern-lex.h"
#include "program-trouble.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

/* Internal buffer that holds the description text, using realloc ()
   to increase the size whenever needed.  The buffer is freed at
   program termination via an atexit () hook.  Sadly, this style of
   buffer makes this module non-thread-safe and/or non-reentrant;
   this is tolerable at present, as we anticipate that descriptions
   will not be required often, except in certain debugging
   situations, and reporting the tokens as a string gives the
   caller some more control over how the information is
   presented.  */

/* The internal buffer must be large enough to write an initial
   "number of tokens" line without any size checks.  */
#define INITIAL_BUFFER_SIZE         8192
#define BUFFER_WORK_OVERHEAD_SIZE   1024

/* Internal selector naming subtypes of character classes; used to
   allow code reuse of class display, with suitable tailoring as
   appropriate for the subclass.  */
typedef enum charclass_subtype_enum {
  charclass_subtype_char_direct,
  charclass_subtype_char_inverted,
  charclass_subtype_wchar_direct,
  charclass_subtype_wchar_inverted
} charclass_subtype_t;

/* Buffer size and memory address management variables.  */
static size_t shared_description_buf_size = 0;
static char *shared_description_buf = NULL;

/* Pointer to current place within buffer to write the next
   description.  The buffer management ensures that a user can write
   up to BUFFER_WORK_OVERHEAD_SIZE bytes before committing a
   (partial) description to the buffer; after each commit, the
   work-overhead space guarantee is made afresh.  */
static char *buf_current = NULL;

/* Let the buffer management facility know about a modest amount of
   data that we've added to the buffer.  The manager keeps track of
   buffer use, and ensures that enough space is available in the
   buffer so that reasonable users (requiring <256 chars) do not
   need to use separate work buffers.  The buffer might be relocated
   by realloc, so the "current" pointer is both an input and an
   output to this function.  */
static char *
commit_text (char *p_buf)
{
  ptrdiff_t buf_used;

  p_buf += strlen (p_buf);
  buf_used = p_buf - shared_description_buf;
  if (buf_used >= (shared_description_buf_size
                   - BUFFER_WORK_OVERHEAD_SIZE))
    {
      size_t new_buf_size;

      /* Double the buffer size, being careful to check for
         variable overflows, or errors from realloc ().  */
      new_buf_size = shared_description_buf_size * 2;
      assert (new_buf_size > shared_description_buf_size);
      shared_description_buf = realloc (shared_description_buf,
                                        new_buf_size);

      /* Okay, size increase worked.  Update global variables.  */
      assert (shared_description_buf);
      shared_description_buf_size = new_buf_size;

      /* Update local variables after buffer increase.  */
      p_buf = &shared_description_buf[buf_used];
    }

  buf_current = p_buf;
  return p_buf;
}

static char *
describe_char (char * p_buf,
               pattern_lex_param_t param)
{
  p_buf += sprintf (p_buf, "OP_CHAR_DIRECT(");

  switch (param)
    {
      /* Deal with codes that have commonly-used 1-char C escape
         sequences.  */
    case '\0': sprintf (p_buf,  "'\\0')     -- 0x%02x", param); break;
    case '\a': sprintf (p_buf,  "'\\a')     -- 0x%02x", param); break;
    case '\b': sprintf (p_buf,  "'\\b')     -- 0x%02x", param); break;
    case '\f': sprintf (p_buf,  "'\\f')     -- 0x%02x", param); break;
    case '\n': sprintf (p_buf,  "'\\n')     -- 0x%02x", param); break;
    case '\r': sprintf (p_buf,  "'\\r')     -- 0x%02x", param); break;
    case '\t': sprintf (p_buf,  "'\\t')     -- 0x%02x", param); break;
    case '\v': sprintf (p_buf,  "'\\v')     -- 0x%02x", param); break;
    case '\\': sprintf (p_buf, "'\\\\')     -- 0x%02x", param); break;
    case '\'': sprintf (p_buf,  "'\\'')     -- 0x%02x", param); break;
    case '\"': sprintf (p_buf,  "'\\\")     -- 0x%02x", param); break;
    case '\?': sprintf (p_buf,  "'\\?')     -- 0x%02x", param); break;

      /* Deal with remaining codes in groups.  */
    default:
      if (param <= UCHAR_MAX)
        {
          if (isprint (param))
            sprintf (p_buf, "'%c')      -- 0x%02x",
                     param, param);
          else
            sprintf (p_buf, "0x%02x)", param);
          break;
        }

      /* Char is larger than a byte -- use Unicode notation.  */
      if (param <= 0xffff)
        sprintf (p_buf, "U+%04x)", param);
      else
        sprintf (p_buf, "U+%06x)", param);
      break;
    }

  p_buf = commit_text (p_buf);
  buf_current = p_buf;
  return p_buf;
}

static char *
describe_class (char *p_buf,
                pattern_lex_opcode_t opcode,
                pattern_lex_param_t param)
{
  charclass_t *ccl;
  int line_members;
  int i;
  char print_ch[16 + 1 + 1 + 2];
  char *p_print_ch;
  int padding;

  ccl = charclass_get_pointer (param);

  /* Add opcode, with plus indentation for membership dump.  */
  switch (opcode)
    {
    case PATTERN_LEX_OP_CHAR_CLASS:
      p_buf += sprintf (p_buf, "OP_CHAR_CLASS(ref=%d)  ",
                        param);
      if (param < 100) *p_buf++ = ' ';
      if (param <  10) *p_buf++ = ' ';
      break;

    case PATTERN_LEX_OP_CHAR_CLASS_INVERTED:
      p_buf += sprintf (p_buf, "OP_CHAR_CLASS_INVERTED(ref=%d) ",
                        param);
      break;

    default:
      program_trouble_fatal (_(
        "Sorry, only char_direct and char_inverted classes supported"));
      /* NOTREACHED */
      break;
    }

  p_buf += sprintf (p_buf, " -- members:%d",
                    charclass_bitcount (ccl));
  p_buf += sprintf (p_buf, "\n      --");

  p_print_ch = print_ch;
  line_members = 0;

  /* Enumerate the members of the class.  */
  i = 0;
  for (;;)
    {
      /* Find the next member of the set, and exit loop if no more
         found.  */
      if (! charclass_next_member (ccl, i, &i))
        break;

      /* Have we filled up the current line? */
      if (line_members == 16)
        {
          /* Yes, add the printable-characters to the line, and then
             commit the line to the buffer management code, so that
             it can ensure there's enough space for us to keep
             going.  */
          *p_print_ch = '\0';
          p_buf += sprintf (p_buf, "  %s\n      --", print_ch);
          p_buf = commit_text (p_buf);
          line_members = 0;
          p_print_ch = print_ch;
        }

      /* No, but should we insert a spacer half-way through the
         current columns?  */
      else if (line_members == 8)
        {
          /* Yes, do this to both the hex dump and the printable-
             character list.  */
          p_buf += sprintf (p_buf, " ");
          *p_print_ch++ = ' ';
        }

      /* Append a description of the character to both the hex dump
         and the printable-character strings.  */
      line_members++;
      p_buf += sprintf (p_buf, " %02x", i);
      *p_print_ch++ = isprint (i) ? (char) i : '.';

      /* Move on to the next member, if any.  */
      i++;

    }

  /* Finish off the incomplete last line.  */
  *p_print_ch = '\0';
  padding = 3 * (16 - line_members);
  if (line_members < 8)
    padding++;
  sprintf (p_buf, "%*s  %s", padding, "", print_ch);
  p_buf = commit_text (p_buf);

  return p_buf;
}

/* Provide a human-readable description of the list of tokens,
   typically one line per token, but possibly using indentation
   and/or more lines to concisely describe related sets of
   tokens.  */
char *
compiled_pattern_describe (compiled_pattern_t *compiled_pattern)
{
  container_compiled_pattern_t *container;
  char *p_buf;
  size_t buf_available;
  int write_size;
  pattern_lex_token_t token;
  int nr_tokens;
  pattern_lex_token_t *p_tokens;

  /* Obtain the container surrounding the supplied pattern pointer,
     and do some sanity checking while converting.  */
  assert (compiled_pattern != NULL);
  container = CONVERT_PUBLIC_TO_CONTAINER (compiled_pattern);
  CHECK_CONTEXT (container);

  p_buf = shared_description_buf;
  buf_available = shared_description_buf_size;
  nr_tokens = container->public.nr_tokens;
  p_tokens = container->public.token_list;

  /* Describe the token count, which also ensures that the return
     value is always relevant to the latest call.  */
  write_size = snprintf (p_buf, buf_available,
            _("Number of tokens: %u\n"), nr_tokens);
  p_buf += write_size;
  buf_available -= write_size;

  /* Work along token list, using snprintf to write into the
     shared buffer, with p_buf the "moving finger" tracking the
     effort.  If the returned size is larger than the available
     buffer size, then the buffer is too small, we increase the
     size (using a double-up strategy), and keep working until
     the request is complete, or we die due to exhaustion.  */
  while (nr_tokens-- > 0)
    {
      pattern_lex_opcode_t opcode;
      pattern_lex_param_t param;

      /* Get the next token from the list.  */
      token = *p_tokens++;

      /* Break the token into opcode/parameter fields.  */
      opcode = PATTERN_LEX_GET_OPCODE (token);
      param  = PATTERN_LEX_GET_PARAM  (token);

      /* Write some spaces so the description of the token opcode
         (and possibly parameter) is indented.  */
      p_buf += sprintf (p_buf, "    ");

      switch (opcode)
        {
        case PATTERN_LEX_OP_CHAR_DIRECT:
          p_buf = describe_char (p_buf, param);
          break;

        case PATTERN_LEX_OP_CHAR_CLASS:
          p_buf = describe_class (p_buf, opcode, param);
          break;

        case PATTERN_LEX_OP_CHAR_CLASS_INVERTED:
          p_buf = describe_class (p_buf, opcode, param);
          break;

        case PATTERN_LEX_OP_WCHAR_DIRECT:
          program_trouble (_(
                 "WCHAR_DIRECT description not implemented yet"));
          break;

        case PATTERN_LEX_OP_WCHAR_CLASS:
          program_trouble (_(
                 "WCHAR_CLASS description not implemented yet"));
          /* ?? Maybe, eventually:
             describe_class (param,
                             &buf_available, &p_buf,
                             map_ref_to_set,
                             charclass_subtype_wchar_direct);  */
          break;

        case PATTERN_LEX_OP_WCHAR_CLASS_INVERTED:
          program_trouble (_(
   "WCHAR_CLASS_INVERTED description not immplemented yet"));
          /* ?? Maybe, eventually:
             describe_class (param,
                             &buf_available, &p_buf,
                             map_ref_to_set,
                             charclass_subtype_wchar_inverted);  */
          break;

        case PATTERN_LEX_OP_REP_MINMAX:
          /* ?? Add in extension param bits? */
          sprintf (p_buf, "REP_MINMAX(ref=%d)", param);
          break;

          /* ?? Do we have another "REPEAT" token, which is usually
             and/or should be preceded by min/max opcodes, or do we
             overlay some convention on REP_MIN and REP_MAX code
             ordering and/or make some param values special
             sentinels?  */

        case PATTERN_LEX_OP_EXTEND_PARAM:
          /* In addition to writing the raw opcode description,
             might want to set a flag noting that an extended
             parameter is being built, and adjust the presentation
             of the opcode that uses the parameter accordingly.  */
          sprintf (p_buf, " EXTEND_next_PARAM(%d)", param);
          break;

          /* Non-parameter tokens packed into an "other" opcoce.  */
        case PATTERN_LEX_OP_OTHER:
          {
            char *op_desc;

            switch (token)
              {
              case plotpe_EMPTY:      op_desc = "EMPTY()";      break;
              case plotpe_BEGLINE:    op_desc = "BEGLINE()";    break;
              case plotpe_ENDLINE:    op_desc = "ENDLINE()";    break;
              case plotpe_BEGWORD:    op_desc = "BEGWORD()";    break;
              case plotpe_ENDWORD:    op_desc = "ENDWORD()";    break;
              case plotpe_LIMWORD:    op_desc = "LIMWORD()";    break;
              case plotpe_NOTLIMWORD: op_desc = "NOTLIMWORD()"; break;
              case plotpe_QMARK:      op_desc = "QMARK()";      break;
              case plotpe_STAR:       op_desc = "STAR()";       break;
              case plotpe_PLUS:       op_desc = "PLUS()";       break;
              case plotpe_CAT:        op_desc = "CAT()";        break;
              case plotpe_OR:         op_desc = "OR()";         break;
              case plotpe_LPAREN:     op_desc = "LPAREN()";     break;
              case plotpe_RPAREN:     op_desc = "RPAREN()";     break;
              case plotpe_ANYCHAR:    op_desc = "ANYCHAR()";    break;
              case plotpe_END:        op_desc = "END()";        break;
              case plotpe_ILLEGAL:    op_desc = "ILLEGAL()";    break;
              default:
                /* (Gentle warning: Be careful about token/param
                   selection here... the switch statement is on token,
                   but the backref test here is on param...)  */
                if ((param > plotpe_BACKREF_BASE)
                         && (param <= plotpe_BACKREF_LIMIT))
                  {
                    sprintf (p_buf, "BACKREF(%d)",
                             param - plotpe_BACKREF_BASE);
                    break;
                  }

                program_trouble (
                       _("Unrecognised 'OTHER' subcode: %s\n  %d  %d"),
                       shared_description_buf, opcode, param);
              }
            strcpy (p_buf, op_desc);
          }
          break;

        default:
          program_trouble
            (_("Unrecognised opcode+param: %s\n  %d  %d"),
             shared_description_buf, opcode, param);

        }

      /* Append NL to the description, and check that the buffer's
         in good working shape.  */
      p_buf += strlen (p_buf);
      *p_buf++ = '\n';
      *p_buf   = '\0';
      p_buf = commit_text (p_buf);

    }

  /* Return description to caller.  */
  return shared_description_buf;
}

/* Allocate a compiled_pattern instance, including naming an upper
   bound on pattern size.  Other structures in the instance are
   assigned memory based on this value; regrettably, managing the
   memory footprint of the instance, shared by various modules, is
   only on an honour system, and is open to abuse.  On the upside,
   the instance bundles together a set of closely-related
   structures, and so makes life easier when used correctly.

   The function returns NULL if it is unable to serve the
   request.  */
compiled_pattern_t *
compiled_pattern_alloc (int pattern_len)
{
  container_compiled_pattern_t *container;
  size_t alloc_size;

  /* First, allocate memory for the expanded structure.  */
  alloc_size = sizeof (*container);
  container = calloc (1, alloc_size);
  assert (container);

  /* Write the top/bottom fence/canary values first, so that the
     integrity chack at the end of this function has a chance to
     pick up (unlikely) gross corruption introduced by the
     initialisation code below.  */
  container->top_fence_magic_nr    = CTXT_TOP_FENCE_MAGIC_NR;
  container->bottom_fence_magic_nr = CTXT_BOTTOM_FENCE_MAGIC_NR;

  /* Acquire memory for tokens:  We safely assume that no pattern
     will generate more tokens than its length here.  (Note that
     in other applications, such as where an endmarker might be
     appended, or perhaps extended parameters are required, this
     assumption might not be true.)  */
  alloc_size = pattern_len * sizeof (*container->public.token_list);
  container->public.token_list = calloc (1, alloc_size);
  if (! container->public.token_list)
    program_trouble_fatal
      (_("Unable to acquire memory for token list"));

  /* Similarly, acquire memory for an array of charclass pointers,
     one per pattern match token item, plus an extra pointer for
     a NULL list terminator.  (Again, we assume a very trivial
     mapping between pattern tokens and class list size.)  */
  alloc_size = (pattern_len + 1) * sizeof (charclass_t *);
  container->public.charclass_list = calloc (1, alloc_size);
  if (! container->public.charclass_list)
    program_trouble_fatal
      (_("Unable to acquire mwmory for charclass list"));

  /* Link allocated container into module's list.  */
  container->next_instance = module_instances_list;
  module_instances_list = container;

  /* Report public portion of structure to the client.  */
  CHECK_CONTEXT (container);
  return &container->public;
}

/* Discard this compiled pattern instance, releasing all acquired
   resources.  */
static void
container_discard (container_compiled_pattern_t *container)
{
  CHECK_CONTEXT (container);

  /* Okay, free up associated resources.  */
  free (container->public.token_list);
  free (container->public.charclass_list);

  /* Destroy the context fences to reduce the chance that we try to
     reuse the context by accident.  */
  container->top_fence_magic_nr    += 0x5a5a5a5auL;
  container->bottom_fence_magic_nr += 0x5a5a5a5auL;

  /* NOTE: We do not try to unlink the instance from the module at
     present; this is because this function, at present, is only
     used as a bulk-discard operation when the module is destroyed.
     If individual discard operations are allowed (function becomes
     publically visible, instead of "static" module scope), then
     this will need to be fixed.  */

  /* Finally, free the instance memory itself.  */
  free (container);
}

/* Given the public portion of a compiled pattern (which is the only
   portion that external users see), check that the container
   portion, as well as the public portion, is a valid instance of
   this module, and throw an assertion if anything is found to be
   amiss.  */
void
compiled_pattern_check (compiled_pattern_t *compiled_pattern)
{
  container_compiled_pattern_t *container;

  /* Obtain the container surrounding the supplied pattern pointer,
     and do some sanity checking while converting.  */
  assert (compiled_pattern != NULL);
  container = CONVERT_PUBLIC_TO_CONTAINER (compiled_pattern);
  CHECK_CONTEXT (container);
}

/* Release all resources acquired by this module.  */
static void
compiled_pattern_destroy_module (void)
{
  container_compiled_pattern_t *container;
  container_compiled_pattern_t *next_container;
  char *local_p_buf;

  /* Discard text scratchpad used for generating descriptions.  */
  local_p_buf = shared_description_buf;
  shared_description_buf = NULL;
  buf_current = NULL;
  if (local_p_buf)
    free (local_p_buf);

  /* Copy module's list head to a local variable, and immediately
     remove the module-wide instance reference.  */
  container = module_instances_list;
  module_instances_list = NULL;

  /* Work through the list of created instances, deleting each one
     in turn.  */
  while (container != NULL)
    {
      /* Grab next instance pointer before this instance is
         destroyed, and then use the public interface to perform the
         discard operation.  */
      next_container = container->next_instance;
      container_discard (container);
      container = next_container;
    }
}

/* Prepare module for operation.  */
void
compiled_pattern_initialise (void)
{
  /* Initialise list of created compiled_pattern instances.  */
  if (module_instances_list != NULL)
    assert (!_("Instance created before lexparse_initialise called"));
  module_instances_list = NULL;

  assert (INITIAL_BUFFER_SIZE > BUFFER_WORK_OVERHEAD_SIZE);

  /* Allocate an initial buffer for use.  */
  shared_description_buf_size = INITIAL_BUFFER_SIZE;
  shared_description_buf = malloc (shared_description_buf_size);
  assert (shared_description_buf);

  /* The buffer is used both for storing the final description, and
     a workspace where a description can be pieced together.
     buf_current points to the end of the current description, which
     is where any new description should go; in addition, the buffer
     management guarantees that BUFFER_WORK_OVERHEAD_SIZE are free
     after this pointer for description generation.  */
  buf_current = shared_description_buf;

  /* Finally, use atexit () to arrange for module cleanup on exit. */
  atexit (compiled_pattern_destroy_module);
}

/* vim:set shiftwidth=2: */
