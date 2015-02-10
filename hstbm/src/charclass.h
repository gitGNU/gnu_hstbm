/* charclass -- Tools to create and manipulate sets of characters (octets)

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

/* Written June, 1988 by Mike Haertel
   Modified July, 1988 by Arthur David Olson to assist BMG speedups  */

/* 2014: Repackaged by "untangle" script, written by behoffski.  */

/* This module provides services to allocate, manipulate, consolidate
   and discard 256-bit vectors, used to describe 8-bit (octet) sets.
   Octet is used as the member name here, as "byte" or "char" can
   sometimes refer to different bit sizes (e.g. char -> 6 bits on some
   IBM/Cyber architectures; char -> 32 bits on some DSP architectures;
   in C, sizeof (char) == 1 by definition on all architectures).

   The connection between these "charclass" sets and set expression by
   RE tools can be non-trivial:  Many Unicode characters cannot fit
   into 8 bits, and even where octet-based code pages are used,
   nontrivial cases can appear (e.g. Code page 857, MS-DOS Turkish,
   which has both a dotted and a dotless lowercase and uppercase "I").

   On the storage side, things are slightly tricky and perhaps even
   murky at times.  The client starts by allocating a charclass,
   working on it, and then either completing it (usually) or abandoning
   it.  The working class (pun intended) is represented by a pointer.
   If not abandoned, this pointer is guaranteed to remain valid for the
   lifetime of the module.

   The module tries aggressively to eliminate duplicates; this is
   perhaps the main function of the completion step.  So, the pointer
   that represents the class after completion may not be the working
   pointer.

   In addition to the pointer method of referring to a class, the
   classes can be viewed as an array, with the first class receiving
   index 0, the second receiving index 1, and so on.  Functions are
   provided to map pointers to indexes, and vice versa.  The index
   representation is handy as it is very compact (typically much fewer
   than 24 bits), whereas pointers are architecture and OS-specific,
   and may be 64 bits or more.

   Index 0 is special; it will always represent the zero-class (no
   members set).  Users wanting to store a set of non-zeroclass classes
   (e.g. utf8) can use this property as a sentinel (a value of 0 for a
   static variable can mean "not initialised").

   Finally, there are some "gutter" bits, at least 3 on each end of
   the class, so that, to a limited extent (and especially for the
   common case of EOF == -1), bits can be set and cleared without
   causing problems, and the code does not need to include the overhead
   of checks for out-of-bound bit numbers.  These gutter bits are
   cleared when the class is completed, so EOF (for instance) should
   never be member of a class.  */


#ifndef CHARCLASS_H
#define CHARCLASS_H 1

/* Always import environment-specific configuration items first. */
#include <config.h>

#include <limits.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Define charclass as an opaque type.  */
typedef struct charclass_struct charclass_t;

/* First integer value that is greater than any unibyte char.  Each
   class can model [0 .. CHARCLASS_NOTCHAR - 1] members.  The user may
   reference a few members above or below this range, thanks to the
   explicit gutters, but the margins provided by this facility are
   small.  */
enum { CHARCLASS_NOTCHAR = 1 << CHAR_BIT };

/* Indices to valid charclasses are always non-negative; zero is
   reserved as an index to the zerocass, and all other class entities
   have positive indices.  We use ptrdiff_t rather than size_t here
   as -1 can be used as a sentinel in some places.  */
typedef ptrdiff_t charclass_index_t;

/* Index zero is special:  It is the zero-class (no bits set) class.  */
enum
{
  CHARCLASS_ZEROCLASS_INDEX = 0
};

/* Entire-module initialisation and destruction functions.  The client
   specifies starting size for the class pool.  Destroy_module releases
   all resources acquired by this module.  */

extern void
charclass_initialise (size_t initial_pool_size);

extern void
charclass_destroy_module (void);

/* Single-bit operations (test, set, clear).  */

extern bool _GL_ATTRIBUTE_PURE
charclass_tstbit (int b, charclass_t const *ccl);

extern void
charclass_setbit (int b, charclass_t *ccl);

extern void
charclass_clrbit (int b, charclass_t *ccl);

/* Range-of-bits set and clear operations.  These are easier to read,
   and also more efficient, than multiple single-bit calls.  */

extern void
charclass_setbit_range (int start, int end, charclass_t *ccl);

extern void
charclass_clrbit_range (int start, int end, charclass_t *ccl);

/* Whole-of-set operations (copy, zero, invert, compare-equal).  */

extern void
charclass_copyset (charclass_t const *src, charclass_t *dst);

extern void
charclass_zeroset (charclass_t *ccl);

extern void
charclass_notset (charclass_t *ccl);

extern int _GL_ATTRIBUTE_PURE
charclass_equal (charclass_t const *ccl1, charclass_t const *ccl2);

/* Add "unionset" and "intersectset" functions since whole-of-class
   operations tend to be reasonably expressive and self-documenting.
   In both cases, the source modifies the destination; ORed in, in
   the case of unionset; ANDed in, in the case of intersectset.  */
extern void
charclass_unionset (charclass_t const *src, charclass_t *dst);

extern void
charclass_intersectset (charclass_t const *src, charclass_t *dst);

/* Report how many members are in the set.  */
extern int _GL_ATTRIBUTE_PURE
charclass_bitcount (charclass_t *ccl);

/* Allow enumeration of all members of a set, starting from the
   given ordinal value, and working up in sequence.  If a member is
   found, its value is written to *found_member, and the function
   returns true.

   If no more members are found, the function returns false, and
   *found_member remains unchanged.  */
extern bool
charclass_next_member (charclass_t *ccl,
                       int search_start_member,
                       int *found_member);

/* Functions to allocate, complete and abandon charclasses.  Note that
   the module aggressively tries to reuse existing completed classes
   rather than create new ones.  The module returns an unique index
   that can be used to reference the module; this index supercedes the
   pointer used during the work phase, e.g.:

        work_class_pointer = charclass_alloc ();
        ...(Set/clear members as required to construct a class in
             work_class_pointer.)...
        class_index = charclass_completed (work_class_pointer);
        completed_class_pointer = charclass_get_pointer (class_index);
        ...(completed_class_pointer might not == work_class_pointer.)...

   The aggressive-reuse policy also means that completed classes must
   not undergo further modification.  Another piece of coding hygiene
   is that the pointer value used to construct the class should not
   be used once the class is either completed or abandoned.

   Allocating and then abandoning classes is useful where an
   operation requires temporary classes for a while, but these do not
   need to be maintained once the work is complete.

   2 May 2014: Note to self and others: I was using the term
   "finalise" instead of "completed" for the operation where a class
   has been constructed and is now ready to be used; I've decided to
   change the terminology after reading in various places how the
   term "finalise" is usually strongly connected with end-of-life
   operations on an object and/or class, not a ready-for-use
   operation.  This note is a reminder to myself and a hint to others
   about this change, in case vestiges of the earlier naming scheme
   slip through my edits and/or appear in the code.  */

extern charclass_t *
charclass_alloc (void);

extern charclass_index_t
charclass_completed (charclass_t *ccl);

extern void
charclass_abandon (charclass_t *ccl);

/* Functions to map between pointer references and index references
   for a charclass.  As explained above, the index is convenient as
   it is typically an array reference, and is usually not much larger
   than the number of classes that have been allocated.  */

extern charclass_t * _GL_ATTRIBUTE_PURE
charclass_get_pointer (charclass_index_t const index);

extern charclass_index_t _GL_ATTRIBUTE_PURE
charclass_get_index (charclass_t const *ccl);

/* Return a static string describing a class (Note: not
   reentrant).  */
extern char *
charclass_describe (charclass_t const *ccl);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* CHARCLASS_H */

/* vim:set shiftwidth=2: */
