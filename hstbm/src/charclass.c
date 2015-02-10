/* charclass -- Tools to create and manipulate sets of C "char"s

This module provides tools to create, modify, store and retrieve
character classes, and provides tools tuned to the needs of RE
lexical analysers.

The class itself is an opaque type, referenced by a pointer while
under construction, and later by an unique index when completed.
The module tries aggressively to reuse existing completed classes,
rather than create duplicates.  Functions are provided to map
between indexes and pointers.  Because of the deduplication effort,
the index reported for a class upon completion may map to a
different pointer than the one supplied by new ().

Classes may be shared between different lexer instances, although,
at the time of writing (10 April 2014) it is not thread-safe.  In
many cases, there might only be one class under construction at any
time, with the effort either completed or abandoned quickly.
However, this module recognises that sometimes multiple classes
might be worked on in parallel, and so explicitly marks each
allocated class area as one of "unused", "work" or "completed".
This marking is done by an array of state bytes dynamically
allocated when the pool is created.

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

/* Always import environment-specific configuration items first.  */
#include <config.h>

#include <assert.h>
#include "charclass.h"
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h> /* for EOF assert test.  */
#include <string.h>
#include <wchar.h> /* for WEOF assert test.  */
#include "xalloc.h"

/* Lower bound for size of first pool in the list.  */
/* ?? Set to 2 for pool debug;  Use 10 in production?  */
#define POOL_MINIMUM_INITIAL_SIZE          10

#ifndef MAX
# define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
# define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

/* We maintain a list-of-pools here, choosing to malloc a new slab of
   memory each time we run out, instead of a realloc strategy.  This
   is so that we can provide a guarantee to the user that any class
   pointer issued remains valid for the lifetime of the module.  */

typedef size_t pool_list_index_t;

/* Designator for each charclass in each pool.  Note that enums are
   ints by default, but we use a single unsigned char per class in our
   explicit memory allocation.  */
typedef enum
{
  STATE_UNUSED = 0,
  STATE_WORKING = 1,
  STATE_COMPLETED = 2
} charclass_state_t;

typedef struct pool_info_struct {
  /* Signature identifying top of struct, which also serves as a
     canary that may help unmask unintended damage by outsiders.  */
  unsigned long pool_top_fence_magic_nr;

  charclass_index_t first_index;
  size_t alloc;      /* ?? Use pool_list_index_t type for these?  */
  size_t used;
  charclass_t *classes;

  /* Charclass designator byte array, one per item, allocated
     dynamically.  */
  unsigned char *class_state;

  /* Signature/canary token for bottom of struct.  */
  unsigned long pool_bottom_fence_magic_nr;
} pool_t;
#define POOL_TOP_FENCE_MAGIC_NR    0xd941f60duL
#define POOL_BOTTOM_FENCE_MAGIC_NR 0x752c4485uL
#define CHECK_POOL(pool)  \
  assert (((pool) != NULL)       \
   && ((pool)->pool_top_fence_magic_nr    == POOL_TOP_FENCE_MAGIC_NR)  \
   && ((pool)->pool_bottom_fence_magic_nr == POOL_BOTTOM_FENCE_MAGIC_NR))

static pool_list_index_t pool_list_used  = 0;
static pool_list_index_t pool_list_alloc = 0;
static pool_t *pool_list = NULL;

/* While the header only guarantees a 3-bit gutter at each end of
   each class, we use an entire word (typically 32 bits) for the
   gutter, with at least 1 word placed at the start of each pool, 1
   word as a shared gutter between each class, and 1 word after the
   last class.  */

/* HPUX defines these as macros in sys/param.h.  */
#ifdef setbit
# undef setbit
#endif
#ifdef clrbit
# undef clrbit
#endif

/* This represents part of a character class.  It must be unsigned and
   at least CHARCLASS_WORD_BITS wide.  Any excess bits are zero.  */
typedef unsigned int charclass_word;

/* The number of bits used in a charclass word.  utf8_classes assumes
   this is exactly 32.  */
enum { CHARCLASS_WORD_BITS = 32 };

/* The maximum useful value of a charclass_word; all used bits are 1.  */
#define CHARCLASS_WORD_MASK \
  (((charclass_word) 1 << (CHARCLASS_WORD_BITS - 1) << 1) - 1)

/* Number of words required to hold a bit for every character.  */
enum
{
  CHARCLASS_WORDS = (CHARCLASS_NOTCHAR + CHARCLASS_WORD_BITS - 1) / CHARCLASS_WORD_BITS
};

/* Flesh out opaque charclass type given in the header  */
/* The gutter element following the class member storage also serves
   as the gutter element preceding the next class member in the list.

   Note that since the "gutter" notion explicitly permits a
   restricted set of negative indices, members need to be signed,
   not unsigned, so that arithmetic shift right can be used where
   possible (e.g. -8 >> 8 == -1, not -8 / 256 == 0).  */

struct charclass_struct {
   charclass_word members[CHARCLASS_WORDS];
   charclass_word gutter_following;
};

static int
charclass_word_bitcount (charclass_word member)
{
  unsigned long c;
  /* ?? FIXME: Might need to improve typedef if we want to deal
     with "very long" word sizes (as seen by the compiler).  This
     is why the 64-bit case below is commented out for the
     moment.  */

  /* The following code is taken from the site:
https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
     which notes that all code snippets on the page are in the
     public domain (unless otherwise noted).  I believe that, in
     the absence of a note to the contrary, that the following code
     in this function (alone) is public domain; GPL3 applies
     elsewhere in this file unless explicitly disclaimed.   */
  switch (CHARCLASS_WORD_BITS)
   {
   case 16:
     c = member & 0xffff;
     c = c - ((c >>  1) & 0x5555);
     c =     ((c >>  2) & 0x3333) + (c & 0x3333);
     c =     ((c >>  4) + c) & 0x0f0f;
     c =     ((c >>  8) + c) & 0x00ff;
     break;
   case 32:
     c = member & 0xffffffffuL;
     c = c - ((c >>  1) & 0x55555555uL);
     c =     ((c >>  2) & 0x33333333uL) + (c & 0x33333333uL);
     c =     ((c >>  4) + c) & 0x0f0f0f0fuL;
     c =     ((c >>  8) + c) & 0x00ff00ffuL;
     c =     ((c >> 16) + c) & 0x0000ffffuL;
     break;
#if 0
   case 64:
     c = member & 0xffffffffffffffffuLL;
     c = c - ((c >>  1) & 0x5555555555555555uLL);
     c =     ((c >>  2) & 0x3333333333333333uLL)
                               + (c & 0x3333333333333333uLL);
     c =     ((c >>  4) + c) & 0x0f0f0f0f0f0f0f0fuLL;
     c =     ((c >>  8) + c) & 0x00ff00ff00ff00ffuLL;
     c =     ((c >> 16) + c) & 0x0000ffff0000ffffuLL;
     c =     ((c >> 32) + c) & 0x00000000ffffffffuLL;
     break;
#endif /* 0 */
     default:
       assert (!
           "Unrecognised word size in charclass_word_bitcount");
     break;
  }

  return c;
}

/* Define class bit operations: test, set and clear a bit.

   Grrrr.  I wanted to exploit arithmetic right shift to come up
   with a really cheap and neat way of reducing small negative bit
   values, especially if b == EOF == -1, to an index of -1 that
   falls neatly into the gutter, but strict C conformance does not
   guarantee this.  The code below handles the two most likely
   scenarios, but, as with anything that is undefined, this is
   playing with fire.  */

#if INT_MAX == 2147483647
/* log2 (sizeof (int)) + log2 (CHARCLASS_WORD_BITS) */
# define INT_BITS_LOG2 5
#else
# error "Not implemented: Architectures with ints other than 32 bits"
#endif

#if ((~0 >> 1) < 0)
  /* Arithmetic shift right: Both signed and unsigned cases are ok.  */
# define ARITH_SHIFT_R_INT(b) ((b) >> INT_BITS_LOG2)
#else
  /* Avoid using right shift if b is negative.  The macro may evaluate
     b twice in some circumstances.  */
# define ARITH_SHIFT_R_INT(b) \
      (((b) < 0) ? -1 : ((b) >> INT_BITS_LOG2))
#endif

bool _GL_ATTRIBUTE_PURE
charclass_tstbit (int b, charclass_t const *ccl)
{
  return ccl->members[ARITH_SHIFT_R_INT (b)] >> b % CHARCLASS_WORD_BITS & 1;
}

void
charclass_setbit (int b, charclass_t *ccl)
{
  ccl->members[ARITH_SHIFT_R_INT (b)] |= (charclass_word) 1 << b % CHARCLASS_WORD_BITS;
}

void
charclass_clrbit (int b, charclass_t *ccl)
{
  ccl->members[ARITH_SHIFT_R_INT (b)] &= ~((charclass_word) 1
                                  << b % CHARCLASS_WORD_BITS);
}

void
charclass_setbit_range (int start, int end, charclass_t *ccl)
{
  int bit;

  /* Do nothing if the range doesn't make sense.  */
  if (end < start)
    return;
  if (start >= CHARCLASS_NOTCHAR)
    return;

  /* Clip the range to be in the interval [-1..NOTCHAR - 1] */
  start = MAX (start, -1);
  end   = MAX (end,   -1);
  /* We know start is < CHARCLASS_NOTCHAR from the test above.  */
  end   = MIN (end,   CHARCLASS_NOTCHAR - 1);

  /* ?? Could check that ccl is a valid class, but not at present.  */

  /* Okay, loop through the range, bit-by-bit, setting members.  */
  for (bit = start; bit <= end; bit++)
    ccl->members[ARITH_SHIFT_R_INT (bit)]
               |= 1U << bit % CHARCLASS_WORD_BITS;
}

void
charclass_clrbit_range (int start, int end, charclass_t *ccl)
{
  int bit;

  /* Do nothing if the range doesn't make sense.  */
  if (end < start)
    return;
  if (start >= CHARCLASS_NOTCHAR)
    return;

  /* Clip the range to be in the interval [-1..NOTCHAR - 1] */
  start = MAX (start, -1);
  end   = MAX (end,   -1);
  /* We know start is < CHARCLASS_NOTCHAR from the test above.  */
  end   = MIN (end,   CHARCLASS_NOTCHAR - 1);

  /* ?? Could check that ccl is a valid class, but not at present.  */

  /* Okay, loop through the range, bit-by-bit, clearing members.  */
  for (bit = start; bit <= end; bit++)
    ccl->members[ARITH_SHIFT_R_INT (bit)]
               &= ~(1U << bit % CHARCLASS_WORD_BITS);
}

/* Define whole-set operations: Copy, clear, invert, compare and union  */

void
charclass_copyset (charclass_t const *src, charclass_t *dst)
{
  memcpy (dst->members, src->members, sizeof (src->members));
}

void
charclass_zeroset (charclass_t *ccl)
{
  memset (ccl->members, 0, sizeof (ccl->members));
}

void
charclass_notset (charclass_t *ccl)
{
  int i;

  for (i = 0; i < CHARCLASS_WORDS; ++i)
    ccl->members[i] = CHARCLASS_WORD_MASK & ~ccl->members[i];
}

int _GL_ATTRIBUTE_PURE
charclass_equal (charclass_t const *ccl1, charclass_t const *ccl2)
{
  return memcmp (ccl1->members, ccl2->members,
       sizeof (ccl1->members)) == 0;
}

void
charclass_unionset (charclass_t const *src, charclass_t *dst)
{
  int i;

  for (i = 0; i < CHARCLASS_WORDS; ++i)
    dst->members[i] |= src->members[i];
}

void
charclass_intersectset (charclass_t const *src, charclass_t *dst)
{
  int i;

  for (i = 0; i < CHARCLASS_WORDS; ++i)
    dst->members[i] &= src->members[i];
}

/* Report how many members are in the set.  */
int _GL_ATTRIBUTE_PURE
charclass_bitcount (charclass_t *ccl)
{
  int i;
  int count;

  count = 0;
  for (i = 0; i < CHARCLASS_WORDS; ++i)
    {
      charclass_word w;

      w = ccl->members[i];
      if (w != 0)
        count += charclass_word_bitcount (w);
    }

  return count;
}

/* Allow enumeration of all members of a set, starting from the
   given ordinal value, and working up in sequence.  If a member is
   found, its value is written to *found_member, and the function
   returns true.

   If no more members are found, the function returns false, and
   *found_member remains unchanged.  */
bool
charclass_next_member (charclass_t *ccl,
                       int search_start_member,
                       int *found_member)
{
  int i;

  /* Use a simple loop for now (initially).  This code could exploit
     its knowledge of the internals, and be more efficient, but, at
     the time of writing, other areas are more pressing in terms of
     providing opportunities for performance enhancement.  */
  for (i = search_start_member; i < CHARCLASS_NOTCHAR; i++)
    if (charclass_tstbit (i, ccl))
      {
        *found_member = i;
        return true;
      }

  /* No member found.  */
  return false;
}

/* #ifdef DEBUG */

/* Nybble (4bit)-to-char conversion array for little-bit-endian
   nybbles.  */
static const char *disp_nybble = "084c2a6e195d3b7f";

/* Return a static string describing a class (Note: not reentrant).  */
char *
charclass_describe (charclass_t const *ccl)
{
  /* Define a static buffer to hold the charclass description.  */
  static char buf[32 + 9 * CHARCLASS_WORDS];
  char *p_buf = buf;
  int i;

  p_buf += sprintf (p_buf, "0x%08lx:", (unsigned long) ccl);
  for (i = 0; i < CHARCLASS_WORDS; i += 2)
    {
      int j = ccl->members[i];
      *p_buf++ = ' ';
      *p_buf++ = disp_nybble[(j >>  0) & 0x0f];
      *p_buf++ = disp_nybble[(j >>  4) & 0x0f];
      *p_buf++ = disp_nybble[(j >>  8) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 12) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 16) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 20) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 24) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 28) & 0x0f];

      j = ccl->members[i + 1];
      *p_buf++ = disp_nybble[(j >>  0) & 0x0f];
      *p_buf++ = disp_nybble[(j >>  4) & 0x0f];
      *p_buf++ = disp_nybble[(j >>  8) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 12) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 16) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 20) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 24) & 0x0f];
      *p_buf++ = disp_nybble[(j >> 28) & 0x0f];
    }
  *p_buf++ = '\0';
  return buf;
}

/* static */ void
debug_pools (const char *label, bool class_contents)
{
  pool_list_index_t pool_nr;
  size_t class_nr;

  printf ("\nPool %p debug(%s): [alloc, used: %ld %ld]\n",
          (void *) pool_list, label,
          pool_list_alloc, pool_list_used);
  for (pool_nr = 0; pool_nr < pool_list_used; pool_nr++)
    {
      pool_t *pool = &pool_list[pool_nr];
      printf (
" %3ld: first_index, alloc, used, classes: %4ld %3lu %3lu %p\n",
              pool_nr, pool->first_index, pool->alloc, pool->used,
              (void *) pool->classes);
      printf ("     class_states: ");
      for (class_nr = 0; class_nr < pool->alloc; class_nr++)
        switch (pool->class_state[class_nr]) {
          case STATE_UNUSED:    putchar ('.'); break;
          case STATE_WORKING:   putchar ('w'); break;
          case STATE_COMPLETED: putchar ('C'); break;
          default: printf ("?%02x", pool->class_state[class_nr]);
        }
      putchar ('\n');
    }

  /* If class contents requested, print them out as well.  */
  if (class_contents)
    for (pool_nr = 0; pool_nr < pool_list_used; pool_nr++)
      {
        pool_t *pool = &pool_list[pool_nr];
        for (class_nr = 0; class_nr < pool->used; class_nr++)
          printf ("%s\n",
                  charclass_describe (&pool->classes[class_nr]));
      }
}

/* #endif * DEBUG */

static pool_t *
add_new_pool (void)
{
  pool_t *prev, *pool;
  size_t pool_class_alloc;
  charclass_t *alloc_mem;

  /* If the pools list is full, use x2nrealloc to expand its size.  */
  if (pool_list_used == pool_list_alloc)
      pool_list = x2nrealloc (pool_list, &pool_list_alloc, sizeof (pool_t));

  /* Find the size of the last charclass pool in the (old) list.
     Scale up the size so that malloc activity will decrease as the
     number of pools increases.  Also, add 1 here as we knock off 1
     to use as a gutter later.  */
  prev = &pool_list[pool_list_used - 1];
  pool_class_alloc = (prev->alloc * 5 / 2) + 1;
  alloc_mem = XNMALLOC (pool_class_alloc, charclass_t);

  /* Add the new pool memory to the pool list.  */
  pool = &pool_list[pool_list_used++];

  /* Immediately set up top and bottom context canary/fences.  */
  pool->pool_top_fence_magic_nr    = POOL_TOP_FENCE_MAGIC_NR;
  pool->pool_bottom_fence_magic_nr = POOL_BOTTOM_FENCE_MAGIC_NR;

  /* Acquire class memory, including shifting the alloc pointer to
     create the gutter preceding the first class of the pool.  */
  pool->classes = alloc_mem + 1;
  pool->first_index = prev->first_index + prev->alloc;
  pool->alloc = pool_class_alloc - 1;
  pool->used = 0;
  pool->class_state = xzalloc (pool->alloc);

  CHECK_POOL (pool);

  return pool;
}

charclass_t *
charclass_alloc (void)
{
  pool_list_index_t pool_nr;
  charclass_t *class;
  pool_t *pool = NULL;
  size_t class_nr;
  size_t class_last_nr;
  charclass_word *gutter_preceding;

  /* Locate a pool with unused entries (if any).  */
  for (pool_nr = 0; pool_nr < pool_list_used; pool_nr++)
    {
      pool = &pool_list[pool_nr];

      /* Try use the earliest pool possible, first by filling in a
         hole from a withdrawn class, or by grabbing an unused class
         from the end of the list.  */
      class_last_nr = MIN (pool->used + 1, pool->alloc);
      for (class_nr = 0; class_nr < class_last_nr; class_nr++)
       {
          if (pool->class_state[class_nr] == STATE_UNUSED)
            goto found_pool_and_class;
       }
    }

  /* No space found, so prepare a new pool and make this class its
     first element.  */
  pool = add_new_pool ();
  class_nr = 0;
  /* FALLTHROUGH */

found_pool_and_class:
  /* Mark the found class state as working, zero its elements, and
     return class pointer to caller.  Zeroing is needed as this
     class may have been previously worked on, but then abandoned or
     withdrawn.  */
  pool->class_state[class_nr] = STATE_WORKING;
  if (class_nr >= pool->used)
    pool->used = class_nr + 1;
  class = &pool->classes[class_nr];

  /* Zero out the class' members, and also the gutters on each
     side.  */
  memset (class, 0, sizeof (*class));
  gutter_preceding = ((charclass_word *) class) - 1;
  *gutter_preceding = 0;

  return class;
}

pool_t * _GL_ATTRIBUTE_PURE
find_class_pool (charclass_t const *ccl)
{
  pool_list_index_t pool_nr;
  pool_t *pool = NULL;
  ptrdiff_t class_ptr_offset;

  /* Locate the pool whose memory address space covers this class.  */
  /* ?? Perhaps check &pool->classes[pool->alloc] in this first
     loop, and then check that the index is in the "used" portion
     later, so we can diagnose malformed pointers more exactly.  */
  for (pool_nr = 0; pool_nr < pool_list_used; pool_nr++)
    {
      pool = &pool_list[pool_nr];
      if ((pool->classes <= ccl) && (ccl < &pool->classes[pool->alloc]))
        goto found_pool;
    }

  /* No credible pool candidate was found.  */
  assert (! "find_class_pool: no pool found");
  return NULL;

found_pool:
  /* Make sure the class clearly lies on an array boundary within
     the pool's memory allocation.  */
  class_ptr_offset = (char *) ccl - (char *) pool->classes;
  if ((class_ptr_offset % sizeof (charclass_t)) != 0)
    {
      /* Pointer does not lie at the start of a pool member.  */
      assert (! "find_class_pool: pointer not aligned.");
      return NULL;
    }

  return pool;
}

static void
withdraw_class (charclass_t *ccl, pool_t *class_pool)
{
  pool_t *pool;
  size_t class_nr;
  int *gutter_preceding;

  /* Use pool reference if given, otherwise work back from the class
     pointer to find the associated pool.  */
  pool = (class_pool != NULL) ? class_pool : find_class_pool (ccl);

  if (pool == NULL)
    assert (! "Could not locate a pool for this charclass");

  CHECK_POOL (pool);

  /* Zero out the gutters each side of the class.  */
  ccl->gutter_following = 0;
  gutter_preceding = ((int *) ccl) - 1;
  *gutter_preceding = 0;

  /* Work out the class index in the pool.  */
  class_nr = ccl - pool->classes;
  pool->class_state[class_nr] = STATE_UNUSED;

  /* Is this the last item within the pool's class list? */
  if (class_nr == pool->used - 1)
    {
      /* Yes, reduce the pool member count by 1.  */
      pool->used--;
      return;
    }
}

/* Finish off creating a class, and report an index that can be used
   to reference the class.  */
charclass_index_t
charclass_completed (charclass_t *ccl)
{
  charclass_word *gutter_preceding;
  pool_list_index_t pool_nr;
  pool_t *pool;
  charclass_t *found = NULL;
  size_t class_nr;
  pool_t *my_pool = NULL;
  size_t my_class_nr = 0;

  /* Search all pools for a completed class matching this class,
     and, if found, use it in preference to the new one.  While
     searching, also record where the work class is located.  If we
     can't find ourselves, the pointer is invalid, and so throw an
     assertion.   */
  for (pool_nr = 0; pool_nr < pool_list_used; pool_nr++)
    {
      pool = &pool_list[pool_nr];
      for (class_nr = 0; class_nr < pool->used; class_nr++)
        {
          charclass_t *search = &pool->classes[class_nr];
          /* Have we found ourselves in the list? */
          if (search == ccl)
            {
              /* Yes, remember this place in case no duplicate is
                 found.  */
              my_pool = pool;
              my_class_nr = class_nr;
          }
          if (pool->class_state[class_nr] != STATE_COMPLETED)
            continue;
          if (charclass_equal (search, ccl))
            {
              /* Another class, completed, matches:  Use it in
                 preference to potentially creating a duplicate.  */
              withdraw_class (ccl, my_pool);
              found = search;
              goto found_matching_class;
            }
        }
    }

  /* No duplicate found... but make sure the search pointer is
     known. */
  assert (my_pool != NULL);
  assert (my_pool->class_state[my_class_nr] == STATE_WORKING);

  /* Prepare to convert the search (work) class into a completed
     class.  */
  pool = my_pool;
  class_nr = my_class_nr;
  found = &pool->classes[class_nr];
  /* FALLTHROUGH */

found_matching_class:
  /* Clear out the gutter integers each side of the class entry.  */
  gutter_preceding = found->members - 1;
  *gutter_preceding = 0;
  found->gutter_following = 0;
  pool->class_state[class_nr] = STATE_COMPLETED;

  /* Return the index of the class.  */
  return pool->first_index + class_nr;
}

void
charclass_abandon (charclass_t *ccl)
{
  withdraw_class (ccl, NULL);
}

/* Additional functions to help clients work with classes.  */

charclass_t * _GL_ATTRIBUTE_PURE
charclass_get_pointer (charclass_index_t const index)
{
  pool_list_index_t pool_nr;
  pool_t *pool;

  /* Does this class match any class we've seen previously? */
  for (pool_nr = 0; pool_nr < pool_list_used; pool_nr++)
    {
      /* Is the index inside this pool? */
      pool = &pool_list[pool_nr];
      if (pool->first_index <= index
              && index < (pool->first_index + pool->used))
        {
          /* Yes, find the pointer within the pool and return
             it.  */
          return &pool->classes[index - pool->first_index];
        }
    }

  /* The mapping above should never fail; we could return NULL, but
     we choose to abort instead.  */
  assert (!"index-to-charclass mapping failed");
  return NULL;
}

charclass_index_t _GL_ATTRIBUTE_PURE
charclass_get_index (charclass_t const *ccl)
{
  pool_t *pool;

  /* This code is similar to charclass_completed... perhaps
     merge?  */
  pool = find_class_pool (ccl);
  if (pool == NULL)
    return -1;

  /* Report the index to the caller.  */
  return pool->first_index + (ccl - pool->classes);
}

/* Functions to initialise module on startup, and to shut down and
   release acquired resources at exit.  */

void
charclass_destroy_module (void)
{
  pool_t *local_list;
  int i;
  charclass_t *alloc_mem;

  /* Move the global list head into a local variable, and
     immediately clear the global.  This is a half-hearted attempt
     to avoid race conditions; to do things properly, a
     system-wide atomic operation (locked, including multi-CPU cache
     coherency) operation should be used.  */
  local_list = pool_list;
  pool_list = NULL;

  /* If the list is already empty, finish now.  */
  if (! local_list)
    return;

  /* Discard the charclass memory associated with each pool,
     including catering for the offset used upon creation.  */
  for (i = 0; i < pool_list_used; i++)
    {
      pool_t *pool;

      pool = &local_list[i];

      /* We skipped an initial class after allocation, in order to
         give the first class in the pool a gutter, so undo the
         offset here when freeing.  */
      alloc_mem = pool->classes;
      free (alloc_mem - 1);

      /* Also free up the class_state memory.  */
      free (pool->class_state);

      /* Destroy the top & bottom fences of this pool so that some
         more bug cases (e.g. reuse-after-free) are visible.  */
      pool->pool_top_fence_magic_nr += 0x5a5a5a5auL;
      pool->pool_bottom_fence_magic_nr += 0x5a5a5a5auL;
    }


  /* Finally, free up the pool list itself.  */
  free (local_list);
  pool_list_used = 0;
}

void
charclass_initialise (size_t initial_pool_size)
{
  size_t initial_alloc;
  charclass_t *alloc_mem;
  pool_t *pool;
  charclass_t *ccl;
  charclass_index_t zeroclass_index;

  /* Usually (gnulib assumption?) EOF = WEOF = -1, but the standard
     merely states that EOF must be a negative integer, not
     necessarily -1; furthermore, the definition for (ISO C90) WEOF
     states that it need not be negative at all, let alone
     guaranteed to be -1.

     In this demonstration/prototype code, we demand that both EOF
     and WEOF be -1, as this value makes the gutters worthwhile:
     The user can be relieved of some edge case processing if -1 is
     thrown neatly into the gutter .  */
  assert (EOF == -1);
  assert (WEOF == -1);

  /* First, set up the list-of-pools structure with initial
     storage.  */
  pool_list_alloc = 4;
  pool_list = (pool_t *) xnmalloc (pool_list_alloc, sizeof (pool_t));

  /* If initial pool size is small, inflate it here as we prefer to
     waste a little memory, rather than issue many calls to
     xmalloc ().  This minimum also ensures that our double-up pool
     size strategy has a sane (perhaps overly generous?) starting
     point.  */
  initial_alloc = MAX (initial_pool_size,
                       POOL_MINIMUM_INITIAL_SIZE);

  /* Set up the first pool using our chosen first alloc size.
     Allocate an extra class, and offset the pool by this amount, in
     order to accommodate the initial gutter integer.  (Note for the
     future:  If charclass alignment becomes significant, then
     sizeof (charclass) and this offset may need to be changed,
     perhaps for SIMD instructions.)  */
  pool_list_used = 1;
  pool = &pool_list[0];
  pool->first_index = 0;
  pool->alloc = initial_alloc;
  pool->used = 0;
  alloc_mem = XNMALLOC (pool->alloc + 1, charclass_t);
  pool->classes = alloc_mem + 1;
  pool->class_state = xzalloc (pool->alloc);

  /* Immediately set up top and bottom context canary/fences for
     the always-allocated first pool in the initial list.  */
  pool->pool_top_fence_magic_nr    = POOL_TOP_FENCE_MAGIC_NR;
  pool->pool_bottom_fence_magic_nr = POOL_BOTTOM_FENCE_MAGIC_NR;

  /* Enforce the all-zeroes class to be the first class.  This is
     needed as "abandon" may leave a hole in a pool in some cases,
     and in these cases we need to ensure that no-one else picks it
     up by accident (as this would invalidate the guarantee that the
     module eliminates all duplicates, from the point of view of the
     user).  So, we set the first class to all-zeroes, and also zero
     out abandoned classes where a hole is unavoidable.  */
  /* Another reason for class 0 to be an empty-set class is that we
     explicitly let consumers exploit this property in their code,
     simplifying their operations in some situations.  */
  ccl = charclass_alloc (); /* Alloc provides an all-zeroes class.  */
  zeroclass_index = charclass_completed (ccl);
  assert (zeroclass_index == CHARCLASS_ZEROCLASS_INDEX);

  /* Check that the initial pool still passes the simple sanity
     check (it might have been compromised since the sentinels were
     set up, although such a bug and/or external interference is
     very unlikely.  */
  /* ?? Note for the purists:  If we wanted to get more titchy about
     data structures, the next step would be to add a checksum (e.g.
     the Castagnoli CRC-32) of the entire structure's contents, and
     include a check that the checksum is intact in the CHECK_POOL
     macro.  Still not perfect, but an improvement... but not
     implemented at present.   */
  CHECK_POOL (pool);

  /* Finally, use atexit () to arrange for module cleanup on exit.  */
  atexit (charclass_destroy_module);
}

/* vim:set shiftwidth=2: */
