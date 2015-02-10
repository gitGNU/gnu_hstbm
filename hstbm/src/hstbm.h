/* hstbm -- Hybrid Self-Tuning Boyer-Moore string search algorithm

   Handles patterns that are exact or which fit into certain domains
   of character folding, especially case-insensitive searches for
   unibyte locales.  */

 /* Copyright (C) 1989, 1998, 2000, 2005, 2007, 2009-2015 Free
   Software Foundation, Inc.
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

/* This code is, in part, based on the code in GNU Grep, especially
   the Boyer-Moore code in kwset.c, which has the attribution:
        Written August 1989 by Mike Haertel.
   Many others have contributed to the sources, including a recent
   (2013-14) spate of impressive speedups:  See the GNU Grep files
   AUTHORS and THANKS, as well as the git changelog, for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

#ifndef HSTBM_H
#define HSTBM_H 1

#include "charclass.h"
#include "compiled-pattern.h"
#include <limits.h>
#include "pattern-lex.h"
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Minimum and maximum pattern lengths allowed.  If a pattern is too
   long, the client may find value in searching for a subset of the
   pattern that fits within these boundaries.  */
enum
{
  HSTBM_PATTERN_LEN_MIN = 2,
  HSTBM_PATTERN_LEN_MAX = 255
};

typedef unsigned char hstbm_byte_t;

typedef size_t hstbm_len_t;

typedef struct hstbm_context_struct hstbm_context_t;

/* Prepare module for operation.  */
extern void
hstbm_initialise (void);

/* Create a new search engine instance.  */
extern hstbm_context_t *
hstbm_new (void);

/* Discard this instance, releasing all acquired resources.  */
extern void
hstbm_discard (hstbm_context_t *ctxt);

/* Set the efficiency threshold used to switch between the
   Boyer-Moore-family search algorithm and "hybrid" memchr/memchr2
   search strategies.  The value 0 is useful to disable the memchr
   searches altogether.

   NOTE: If used, this function must be called before the pattern is
   supplied, as analysis of the pattern may result in the threshold
   being forced to 0.  */
extern void
hstbm_set_efficiency_threshold (hstbm_context_t *ctxt,
                                int threshold);

/* Receive a simple pattern (a sequence of bytes), with no special
   meaning attached to any character, and prepare to perform
   searching using this pattern.  */
/* ?? Perhaps pattern should be (char *), not (hstbm_byte_t *).  */
extern bool
hstbm_pattern (hstbm_context_t *ctxt,
               const int len, const hstbm_byte_t *pattern);

/* Check that a single (octet) folding table can accommodate all of
   the folding required for the matcher pattern positions (i.e. all
   but the last position).  Returns false if this constraint cannot
   be met.  */
extern bool _GL_ATTRIBUTE_PURE
hstbm_pattern_supports_simple_folding (compiled_pattern_t
                                       *compiled_pattern);

/* Alternate pattern specification interface, utilising pattern-lex
   tokens, allowing more sophisticated patterns that are still
   compatible with this module (e.g. "abc[123]def").  */
extern bool
hstbm_pattern_compiled (hstbm_context_t *ctxt,
                        compiled_pattern_t *compiled_pattern);

/* Show details about search engine internals, for debugging.  */
void
hstbm_debug_show_context (hstbm_context_t *ctxt);

/* Main functions to receive a buffer, and to work through the
   buffer, looping for the next match, of any.  */
extern void
hstbm_buffer (hstbm_context_t *ctxt,
              hstbm_byte_t *buf_start,
              hstbm_byte_t *buf_end);

extern const hstbm_byte_t *
hstbm_search (hstbm_context_t *ctxt, hstbm_len_t initial_skip);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* HSTBM_H */
/* vim:set shiftwidth=2: */
