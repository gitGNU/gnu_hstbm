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
   (2013-14) spate of impressive speedups:  See the files AUTHORS and
   THANKS, as well as the git changelog, for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

#include <config.h>

#include <assert.h>
#include "charclass.h"
#include "compiled-pattern.h"
#include <ctype.h>
#include "hstbm.h"
#include <limits.h>
#include "memchr2.h"
#include "misc-gettext.h"
#include "program-trouble.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Temporary(?) delta3/4 debug enable/disable macros.  The macro
   formulation here jars, to some extent, both syntactically and
   semantically with C's apparent nature (e.g. I tried "do {code}
   while (0)" wrapping to make the debug macro call more like an
   atomic statement, but couldn't get it to work easily).  */
#if 0
#  define DEBUG_DELTA3(code) code
#  define DEBUG_DELTA4(code) code
#else
#  define DEBUG_DELTA3(code) do {} while (0)
#  define DEBUG_DELTA4(code) do {} while (0)
#endif

#define NR_CHARS (UCHAR_MAX + 1)

/* Critical scratchpad items are (size is pattern_len unless stated
   otherwise):
     - Canonical (folded) pattern;
     - (Folding only:) Nr_aliases;
     - (Folding only:) First_alias;
     - Delta3 (size pattern_len - 1); and
     - Delta4 (size pattern_len - 2).  */
#define SCRATCHPAD_NR_CRITICAL_ITEMS   5
#define SCRATCHPAD_ADDITIONAL_SPACE 3072
#define SCRATCHPAD_BUFFER_SIZE   ( \
      (HSTBM_PATTERN_LEN_MAX * SCRATCHPAD_NR_CRITICAL_ITEMS) \
             + SCRATCHPAD_ADDITIONAL_SPACE)

#define ALIGN_TO_BLOCK_BOUNDARY(ptr, block_size)        \
  do {                                                  \
    ptrdiff_t ATBB_diff;                                \
    ptrdiff_t ATBB_mask;                                \
    ATBB_mask = (block_size) - 1;                       \
    ATBB_diff = ((char *) (ptr)) - ((char *) NULL);     \
    assert (ATBB_diff > 0);                             \
    if (ATBB_diff & ATBB_mask)                          \
      {                                                 \
        ATBB_diff &= ~ATBB_mask;                        \
        ATBB_diff += (block_size);                      \
        (ptr) = (void *) ATBB_diff;                     \
      }                                                 \
  } while (0)

/* Additional memory allocated to ensure context has good memory
   alignment in order to maximise cache effectiveness.  */
#define CONTEXT_ALIGNMENT    4096

typedef unsigned char delta_t;

struct hstbm_context_struct
{
  /* Place items with strong alignment requirements and/or heavy
     usage patterns together at the top of the struct.  The delta1
     table is the centre of memory activity, so it is placed in the
     "centre" of of the action.  The adjoining memory regions (the
     fold_table before delta1, and the small arrays carefully
     allocated to be at the start of the scratchpad buffer), are
     arranged to try and optimise cache coherency.  Some searches
     don't use the fold_table at all, but most searches are likely
     to use the scratchpad, so this is why the fold_table is placed
     before the delta1 table.  */
  hstbm_byte_t fold_table[NR_CHARS];
  delta_t delta1[NR_CHARS];

  /* We use a scratchpad here so for other search-related items, so
     that we can bunch these items closely together without using
     malloc, and so hopefully get better memory cache hit
     performance.  Given that some cache lines are 32 bytes in size,
     and that pattern lengths of 16 or less bytes are not uncommon,
     this could be quite effective... maybe.
     A second reason for the scratchpad is that it lets us avoid
     some extra malloc/free activity for small snippets of memory;
     this reduces our resource tracking requirements and so
     simplifies the code, for a relatively small (for modern
     32/64-bit architectures) memory cost.  */
  hstbm_byte_t scratchpad_buffer[SCRATCHPAD_BUFFER_SIZE];

/* Signature identifying top of struct (after alignment-critical
   elements, because I'm too tired to use offsetof () and do the
   necessary alloc mathematics right now), which also serves as a
   canary that may help unmask unintended damage by outsiders.  */
  unsigned long top_fence_magic_nr;

  /* Pointers to folding-related tables heavily used in the matching
     portion of the search.  These should all land closely together in
     the scratchpad, and so have good performance.  */
  hstbm_byte_t *canonical_pattern;
  hstbm_byte_t *nr_aliases;
  hstbm_byte_t *first_alias;

  const hstbm_byte_t *search;
  const hstbm_byte_t *pat_end;
  const hstbm_byte_t *buf_end;
  delta_t delta2;

  /* Delta3 is used after we know that the last char and the guard
     (at some other offset) have each matched.  Depending on the
     guard offset, we may be able to increase the skip we undertake
     after the first half of the match attempt (from guard-minus-1
     to start-of-pattern) bails out somewhere.  The variable points
     to a pattern-length slab of RAM in the scratchpad, deliberately
     placed close to other inner-search memory fragments in the hope
     that these will all fit in L1 cache and so be almost as fast as
     register accesses.  */
  delta_t *delta3;

  /* Delta4, like delta3, uses feedback from a failed match attempt
     to try and skip as much of the haystack as possible when
     resuming the skip search (mainly because the match search is
     hideously more expensive than the skip search, if the conditions
     for the skip search are favourable).  Here, delta4 has one extra
     piece of information that delta3 hasn't:  When using it, we know
     that the first half of the match (from guard to start of
     pattern) has succeeded, and that the mismatch occurred somewhere
     in the second half.  Therefore, we know more about how the
     haystack matches the pattern, and in an area possibly some
     distance from the end of the pattern (depending on the guard
     offset), and this can let us choose a larger skip on restart in
     some circumstances.  As with delta3, the variable points to a
     pattern-length slab of RAM in the scratchpad, deliberately
     placed close to other inner-search memory fragments in the hope
     that these will all fit in L1 cache and so be almost as fast as
     register accesses.  */
  delta_t *delta4;

  /* Byte count threshold that we use to vary search strategies if the
     B-M skip loop is (possibly) inefficient relative to other search
     strategies such as memchr and memchr2, perhaps because the latter
     has better memory-access patterns, or maybe the library can use
     hardware-specific instructions that fall outside standard C.  */
  ptrdiff_t skip_loop_efficiency_threshold;

  /* --- Remaining variables are much less (although not entirely)
         runtime-performance-sensitive. ---  */

  /* The scratchpad gives us more coherent memory accesses, but, as a
     downside, we need to do our own (very simple) management.  */
  hstbm_byte_t *scratchpad;
  int scratchpad_free;

  /* Retain the dynamic guard offset between invocations.  */
  int guard_offset;

  /* Flag detailing the match loop strategy, not the skip loop
     strategy.  */
  bool folded_match;

  /* Original memory pointer supplied by malloc (the address used to
     hold the struct may be shifted to improve memory alignment).  */
  void *malloc_ptr;

  /* Description of supplied pattern.  */
  int pat_len;

  /* Description of buffer to be searched.  */
  const hstbm_byte_t *buf;

  /* List of candidate delta2 positions for delta3/4
     calculations (for both simple and tokenised (charclass)
     patterns.  */
  delta_t *delta2_list;
  int nr_delta2_elements;

  /* Character classes associated with tokenised pattern (NULL if
     simple (untokenised) pattern provided).  */
  charclass_t **charclass_list;

  /* List of pattern match positions (excluding the final skip
     position) that are suitable for memchr and/or memchr2 scans.
     We use this list ("static guard"?) in a round-robin fashion,
     possibly in tandem with the skip loop's guard ("floating
     guard"?) to change up search strategies when the classic
     Boyer-Moore algorithm is measured to be inefficient at
     performing the search.  The hope is that this list, plus
     also using memchr and/or memchr2 on the skip guard if
     appropriate, will reduce the likelihood that pathalogical
     cases will arise that severely compromise search
     performance.  */
  delta_t *memchr_offset_list;
  int nr_memchr_offset_items;
  int current_memchr_offset_index;

  /* Flag to let us avoid the cost of initialising the fold table,
     unless it's found to be required.  */
  bool fold_table_initialised;

  /* Linked list of instances created by this module.  */
  hstbm_context_t *next_instance;

  /* Signature/canary token for bottom of struct.  */
  unsigned long bottom_fence_magic_nr;
};
#define CTXT_TOP_FENCE_MAGIC_NR     0xce1eb8fauL
#define CTXT_BOTTOM_FENCE_MAGIC_NR  0x34bbb901uL
#define CHECK_CONTEXT(ctxt) \
  assert (((ctxt) != NULL)         \
   && ((ctxt)->top_fence_magic_nr    == CTXT_TOP_FENCE_MAGIC_NR)  \
   && ((ctxt)->bottom_fence_magic_nr == CTXT_BOTTOM_FENCE_MAGIC_NR))

/* Code describing how which skip search to use after a match attempt
   doesn't succeed.  */
typedef enum skip_phase_enum
{
  SKIP_PHASE_BIG_LOOP,
  SKIP_PHASE_SMALL_LOOP,
  SKIP_PHASE_INDIVIDUAL
} skip_phase_t;

/* Linked list of instances created by this module.  We discard any
   outstanding instances via atexit ().  */
static hstbm_context_t *module_instances_list = NULL;

static hstbm_byte_t *
scratchpad_alloc (hstbm_context_t *ctxt, size_t size)
{
  hstbm_byte_t *alloc;

  CHECK_CONTEXT (ctxt);

  /* Deny request if we've run out of space.  This avoids the need
     to have more complex resource tracking, at the expense of
     forcing the caller to worry about allocation failures.  In
     practice, we make the scratchpad large enough, and perform the
     critical (and smaller) allocations before the larger (and
     optional) ones, that the critical users need not worry, and
     the non-critical users can bail out if their request fails.  */
  if (size > ctxt->scratchpad_free)
    return NULL;

  /* Allocate space from the scratchpad.  Note that we do NOT provide
     any alignment guarantees on the returned pointer; we try to pack
     allocated segments as close together as possible.  If a client
     wants some level of alignment, it must do it manually.  */
  alloc = ctxt->scratchpad;
  ctxt->scratchpad += size;
  ctxt->scratchpad_free -= size;
  return alloc;
}

hstbm_context_t *
hstbm_new (void)
{
  void *malloc_ptr;
  hstbm_context_t *new_inst;

  /* Create a new, aligned, instance, with zeroed memory.  */
  malloc_ptr = malloc (sizeof (*new_inst) + CONTEXT_ALIGNMENT);
  assert (malloc_ptr);
  new_inst = malloc_ptr;
  ALIGN_TO_BLOCK_BOUNDARY (new_inst, CONTEXT_ALIGNMENT);
  memset (new_inst, 0, sizeof (*new_inst));
  new_inst->malloc_ptr = malloc_ptr;

  /* Set up top and bottom sentinels.  */
  new_inst->top_fence_magic_nr    = CTXT_TOP_FENCE_MAGIC_NR;
  new_inst->bottom_fence_magic_nr = CTXT_BOTTOM_FENCE_MAGIC_NR;

  /* Set up scratch memory management parameters.  */
  new_inst->scratchpad = new_inst->scratchpad_buffer;
  new_inst->scratchpad_free = sizeof (new_inst->scratchpad_buffer);

  /* Use NULL as a marker that the pattern hasn't been supplied
     yet.  */
  new_inst->canonical_pattern = NULL;

  /* Assume that the skip loop must meet fairly high efficiency
     standards, otherwise we may switch to other algorithms.  At
     present, only memchr and memchr2 are implemented.  Start by
     assuming a high efficiency threshold, and knock it down if the
     pattern isn't amenable.  */
  new_inst->skip_loop_efficiency_threshold = 10 * sizeof (long);

  /* Make sure instance is intact before adding it to the list of
     known instances.  */
  CHECK_CONTEXT (new_inst);

  /* Link created instance into the instances known by this module.  */
  new_inst->next_instance = module_instances_list;
  module_instances_list = new_inst;

  /* Re-check instance integrity just before returning it to the
     caller.  */
  CHECK_CONTEXT (new_inst);

  /* Report created instance to the caller.  */
  return new_inst;
}

void
hstbm_discard (hstbm_context_t *ctxt)
{
  CHECK_CONTEXT (ctxt);

  /* Find this instance in the list of known instances, and unlink
     it from the list.  An instance at the head of the list needs
     separate code as its parent is just a pointer.  */
  CHECK_CONTEXT (module_instances_list);
  if (ctxt == module_instances_list)
    module_instances_list = ctxt->next_instance;
  else
    {
      hstbm_context_t *instance_list;

      /* Search for the instance in the linked list, and throw an
         exception if it isn't found.  */
      instance_list = module_instances_list;
      for (;;)
        {
          if (instance_list == NULL)
            program_trouble_fatal (_(
                "Tried to discard unknown hstbm instance"));
          CHECK_CONTEXT (instance_list);
          if (instance_list->next_instance == ctxt)
            break;
          instance_list = instance_list->next_instance;
        }

      /* Found the instance, now unlink it.  */
      instance_list->next_instance = ctxt->next_instance;
    }

  /* Destroy the context fences to reduce the chance we try to reuse
     the context by accident.  */
  ctxt->top_fence_magic_nr    += 0x5a5a5a5auL;
  ctxt->bottom_fence_magic_nr += 0x5a5a5a5auL;

  /* The instance pointer probably has been shifted to try and
     optimise memory hardware (caching) performance, so free the
     instance via the original pointer supplied by malloc.  */
  free (ctxt->malloc_ptr);
}

static void
finalise (void)
{
  /* Work through linked list of instances, discarding each one.  */
  while (module_instances_list != NULL)
    hstbm_discard (module_instances_list);
}

/* Prepare module for operation.  */
void
hstbm_initialise (void)
{
  /* Check that no instances were created before the module was
     initialised.  */
  assert (module_instances_list == NULL);

  /* Ensure instances are freed when we exit.  */
  atexit (finalise);

}

static bool
pattern_common_setup (hstbm_context_t *ctxt,
                      const int pat_len)
{
  hstbm_byte_t *scratch;

  CHECK_CONTEXT (ctxt);

  /* Reject the pattern if it's too large for our delta type.  */
  if (pat_len > HSTBM_PATTERN_LEN_MAX)
    {
      fprintf (stderr, "Sorry, hstbm has a pattern length limit of: %d\n",
               HSTBM_PATTERN_LEN_MAX);
      return false;
    }

  /* Reject the pattern if the length is too small.  */
  if (pat_len < HSTBM_PATTERN_LEN_MIN)
    {
      fprintf (stderr, "Sorry, pattern is too short: %d\n", pat_len);
      return false;
    }

  /* Record the original pattern length given by the caller.  */
  ctxt->pat_len = pat_len;

  /* Perform initial dealings with the critical arrays of the search:
     These are statically allocated at the top of the instance in
     order to try and force optimal memory alignment, particularly
     with an eye on optimising cache coherency.  The delta1 and fold
     tables are allocated at compile time; the others are allocated
     from the scratchpad buffer that immediately follows the first
     two arrays.  We defer setting up the fold table for now, as we
     don't always need to use it.  */
  ctxt->fold_table_initialised = false;

  /* Default to skipping pat_len bytes for non-pattern chars.  */
  memset (ctxt->delta1, pat_len, sizeof (ctxt->delta1));

  /* Acquire space for a canonical (possibly pre-processed folded)
     description of the pattern, which we always use instead of
     relying on the persistence of the caller's parameter(s).  Also,
     the search focuses on working backwards from the end of the
     pattern, so record the pattern end for convenience.  */
  ctxt->canonical_pattern = scratchpad_alloc (ctxt, pat_len);
  assert (ctxt->canonical_pattern);
  ctxt->pat_end = &ctxt->canonical_pattern[pat_len - 1];

  /* Acquire space for potential aliases of matching charaters.
     We may consult nr_aliases even where no folding is present
     (i.e. if aliases is 0, we can use memchr); alternately, in
     places where nr_aliases is 1, we may find benefit in using
     memchr2 if the BM algorithm isn't working effiently... and the
     first and only alias for each pattern item is accessed from
     first_alias.  Because all operations involving searches are
     end-of-pattern based, the pointer is shifted to the last element
     of the allocated space in each case.  */
  scratch = scratchpad_alloc (ctxt, pat_len);
  assert (scratch);
  ctxt->nr_aliases = &scratch[pat_len - 1];

  /* Allocate pattern-length-minus-1 snippets of memory from the
     scratchpad, hopefully clustered tightly together to give a high
     cache hit likelihood.  */
  /* We do this allocation here, in common_setup, mainly so that all
     the likely-high-cache-hit small (pat_len-ish-sized) arrays are
     packed tightly together, with the hope of optimising cache
     hits.  Note we don't bother initialised delta3/delta4 here.  */
  /* All haystack and pattern indexing is done using the end of the
     pattern as the reference; shift pointers as appropriate so
     that they conform to this convention (the -1 offset arises
     because the last position is handled by the skip search).  */
  /* Note: We never use delta4[-1], as the match has been completed
     by the first match loop.  So, delta4 allocation is smaller (but
     no change to the end-of-pattern offset), and we don't allocate
     any space at all if pat_len is 2.  */
  ctxt->delta3 = (delta_t *) scratchpad_alloc (ctxt, pat_len - 1);
  assert (ctxt->delta3);
  ctxt->delta3 += pat_len - 1;
  ctxt->delta4 = NULL;
  if (pat_len > 2)
    {
      ctxt->delta4 = (delta_t *) scratchpad_alloc (ctxt, pat_len - 2);
      assert (ctxt->delta4);
      ctxt->delta4 += pat_len - 1;
    }

  /* For non-folded searches, we never use first_alias... so, defer
     allocating it until after delta3 and delta4.  This is starting
     to really get marginal and/or architecture-specific and/or
     search-specific about cache performance; not sure if it's
     worth the hassle.  */
  scratch = scratchpad_alloc (ctxt, pat_len);
  assert (scratch);
  ctxt->first_alias = &scratch[pat_len - 1];

  /* Common setup succeeded.  */
  return true;
}

/* "Simple" here means no consideration of folding in any pattern
    except that implied by possible multiple places where the
    skip table (delta1) matches, i.e. delta1[b] == 0.  */
static void
pattern_setup_simple_deltas_234 (hstbm_context_t *ctxt)
{
  int i;
  int pat_len;
  hstbm_byte_t *pattern;
  delta_t *delta2_list;
  int nr_delta2_elements;

  /* Set up shorthand names for heavily-used members of the
     context.  */
  pat_len = ctxt->pat_len;
  pattern = ctxt->canonical_pattern;
  delta2_list = ctxt->delta2_list;

  /* Generate a list of *all* positions in the pattern where the
     character would match in the skip position.  We have done
     this loop previously in order to find delta2; this time, we're
     recording all candidates, as the extra knowledge of the guard
     character can eliminate some or all of the possible skips.  */
  delta2_list = scratchpad_alloc (ctxt, pat_len - 1);
  assert (delta2_list);
  ctxt->delta2_list = delta2_list;

  /* Build the list of delta2 candidates in the acquired space,
     working from the end of the pattern to the start.  */
  nr_delta2_elements = 0;
  if (ctxt->charclass_list == NULL)
    {
      /* Simple pattern:  Merely do direct byte comparisons.  */
      hstbm_byte_t last_byte;

      last_byte = pattern[pat_len - 1];
      for (i = pat_len - 2; i >= 0; i--)
        {
          if (pattern[i] == last_byte)
            delta2_list[nr_delta2_elements++] = pat_len - i - 1;
        }
    }
  else
    {
      /* Charclasses associated with each pattern position.  Use
         charclass_intersectset () to find candidates.  */
      charclass_t *work_class;
      charclass_t *last_class;

      work_class = charclass_alloc ();
      assert (work_class);
      last_class = ctxt->charclass_list[pat_len - 1];
      for (i = pat_len - 2; i >= 0; i--)
        {
          /* *Any* overlap between the class under test and the
             last class is enough to make the position a delta2
             candidate. */
          charclass_copyset (last_class, work_class);
          charclass_intersectset (ctxt->charclass_list[i],
                                  work_class);
          if (charclass_bitcount (work_class) != 0)
            delta2_list[nr_delta2_elements++] = pat_len - i - 1;
        }

      /* Finished using the work class.  */
      charclass_abandon (work_class);
    }

  /* Add a candidate being the entire pattern length; this is a
     functional sentinel that simplifies edge cases for users of
     this list.  */
  delta2_list[nr_delta2_elements++] = pat_len;
  ctxt->nr_delta2_elements = nr_delta2_elements;

  /* The delta2 value nearest the end of the pattern is the value we
     use when we don't know whether the guard or part of the pattern
     matched.  */
  ctxt->delta2 = pat_len;
  if (nr_delta2_elements > 0)
    ctxt->delta2 = delta2_list[0];

}

/******************************************************************/

/* Set the efficiency threshold used to switch between the
   Boyer-Moore-family search algorithm and "hybrid" memchr/memchr2
   search strategies.  The value 0 is useful to disable the memchr
   searches altogether.

   NOTE: If used, this function must be called before the pattern is
   supplied, as analysis of the pattern may result in the threshold
   being forced to 0.  */
void
hstbm_set_efficiency_threshold (hstbm_context_t *ctxt,
                                int threshold)
{
  CHECK_CONTEXT (ctxt);

  if (ctxt->canonical_pattern != NULL)
    assert (_(!
    "cannot set efficiency threshold after pattern has been provided"));

  ctxt->skip_loop_efficiency_threshold = threshold;
}


/* Decide upon the initial guard to use, and also, if feasible,
   set up the memchr/memchr2 hybrid efficiency variables for use
   within the main skip loop.  */
static void
pattern_setup_initial_guard (hstbm_context_t *ctxt)
{
  delta_t *delta1;
  int i;
  int len;
  hstbm_byte_t *pattern;

  /* Set up shorthand names for heavily-used members of the
     context.  */
  delta1  = ctxt->delta1;
  len     = ctxt->pat_len;
  pattern = ctxt->canonical_pattern;

  /* Default to second-to-last pattern char as the guard, but try
     to use another character if it is different from the last
     char.  */
  /* ?? REVIEWME: Use pat_end rather than pattern here, with
     negative indices, so that this code is in line with almost all
     other match body code, where an end-of-pattern-backwards-based
     formulation is used.  */
  delta1 = ctxt->delta1;
  ctxt->guard_offset = -1;
  for (i = 0; i < len - 1; i++)
    if (delta1[pattern[i]] != delta1[pattern[len - 1]])
      {
        ctxt->guard_offset = i - len + 1;
        break;
      }

  /* See if there are *any* positions where memchr or memchr2 could
     be used.  */
  for (i = 1 - len; i <= 0; i++)
    if (ctxt->nr_aliases[i] < 2)
      break;

  /* If *no* possible positions found, set efficiency threshold to 0
     so we don't test for the alternate search case, as we know that
     it will never occur.  */
  if (i > 0)
    ctxt->skip_loop_efficiency_threshold = 0;

  /* If efficiency threshold is enabled, set up list of memchr
     and/or memchr2-capable search positions.  Note that there is a
     hack in the data types here:  We use hstbm_byte_t, values
     [0..255], but we are really representing an offset from the end
     of the pattern [-1..-255].   The code that builds the list here,
     and the code that uses it in the search, needs to be careful
     about this (e.g. mixing signed/unsigned arithmetic).  */

  /* Regardless of whether the facility is ever used, initialise the
     main variables (always used in the debug output code), so that
     the numbers always look sane.  */
  ctxt->nr_memchr_offset_items = 0;
  ctxt->current_memchr_offset_index = 0;

  if (ctxt->skip_loop_efficiency_threshold > 0)
    {
      hstbm_byte_t *scratch;

      /* Acquire scratchpad memory for memchr offsets list.  As
         usual for inner-search arrays, the pointer is moved to
         point to the last member of the array, and all indexes
         (offsets) are relative to that point.  We reduce the
         allocation by 1, but don't adjust the offset, as the
         memchr code excludes the last (skip) position from its
         operation.  */
      scratch = (hstbm_byte_t *) scratchpad_alloc (ctxt, len - 1);
      assert (scratch);
      ctxt->memchr_offset_list = scratch + len - 1;

      /* Build a list, in pattern order, of places where memchr
         and/or memchr2 could be used, and set the "current" item to
         be the first member of the list.  (Note again that we
         store the positive (negated) value of a negative offset,
         such that we can fairly seamlessly use a
         one-positive-byte-per-position memory slab allocated from
         the scratchpad, instead of worrying about indexing into a
         multibyte integer array.)  */
      for (i = 1 - len; i <= 0; i++)
        {
          /* Check aliases: 0 allows memchr, 1 allows memchr2;
             anything higher, we can't work with, using standard
             code (some hardware platforms might provide mechanisms
             (e.g. SIMD instructions?) that could help).  */
          if (ctxt->nr_aliases[i] > 1)
            continue;

          /* Okay, record this position as a viable candidate.  */
          ctxt->memchr_offset_list[ctxt->nr_memchr_offset_items++]
            = (hstbm_byte_t) -i;
        }

      /* If we found that there are no viable memchr candidates
         (e.g. "[123][456][789]", then disble the memchr path by
         dropping the efficiency threshold down to 0.  */
      if (ctxt->nr_memchr_offset_items == 0)
        ctxt->skip_loop_efficiency_threshold = 0;
    }

}

/******************************************************************/

static void
optimise_delta3_for_simple_pattern (hstbm_context_t *ctxt)
{
  delta_t *delta2_list;
  int pat_len;
  const hstbm_byte_t *pat_end;
  delta_t *delta3;
  int i;
  int guard_offset;

  /* For heavily-used fields, use local variables as aliases to keep
     the code readable.  */
  pat_len     = ctxt->pat_len;
  pat_end     = ctxt->pat_end;
  delta2_list = ctxt->delta2_list;
  delta3      = ctxt->delta3;

  /* Work through each of the possible guard offset positions,
     and consider the potential delta2 matches that might fit in
     at each place.  Before working through the delta2 candidate
     list, set the default delta3 skip for that offset to be the
     entire pattern length, which is correct if we don't find any
     other suitable delta3 candidate.  */
  for (guard_offset = -1; guard_offset >= 1 - pat_len; guard_offset--)
    {
      DEBUG_DELTA3 (printf ("\nguard_offset %3d:\"%c\"",
                  guard_offset, pat_end[guard_offset]););

      /* For each potential delta2 shift, see if the guard would
         still match:
         1. If Yes, we cannot consider any greater shift, as we
            might miss a match at this position, so the potential
            delta2 shift becomes the delta3 shift for that position.
         2. If No, then we can eliminate this candidate at this
            position, and move on to the next position.  Note that
            we added an extra pat_len candidate to the list, so, in
            the absence of any other success, the combined_offset
            test is guaranteed to eventually succeed.  */
      for (i = 0; i < ctxt->nr_delta2_elements; i++)
        {
          int candidate;

          candidate = delta2_list[i];
          delta3[guard_offset] = candidate;
          DEBUG_DELTA3 (printf (" %2d:", candidate););

          /* Adding a one-time do {...} block here, even though it's
             quite easy to write the code without it, makes the
             delta3 code much closer in structure and layout to the
             delta4 code.  This is beneficial as it's easier (for
             me, at least) to understand the delta4 code once you've
             understood the delta3 code.  */
          do
            {
              int combined_offset;

              combined_offset = guard_offset - candidate;
              DEBUG_DELTA3 (printf ("%d", combined_offset););
              if (combined_offset <= -pat_len)
                  break;
              DEBUG_DELTA3 (printf ("\"%c\"",
                                    pat_end[guard_offset]););
              if (pat_end[guard_offset] != pat_end[combined_offset])
                {
                  DEBUG_DELTA3 (printf ("!=\"%c\"",
                                        pat_end[combined_offset]););
                  delta3[guard_offset] = 0;
                  break;
                }
            } while (0);
          if (delta3[guard_offset] != 0)
            break;
        }
    }
  DEBUG_DELTA3 (printf ("\n"););

  /* ?? If all delta3 items become pat_len as a result of the above
     loop, then set all delta4 items to pat_len, and tell caller
     not to bother with any delta4 analysis, as it cannot improve
     the situation.... not tried yet.  */
}

static void
optimise_delta4_for_simple_pattern (hstbm_context_t *ctxt)
{
  delta_t *delta2_list;
  int pat_len;
  const hstbm_byte_t *pat_end;
  delta_t *delta4;
  int i;
  int guard_offset;

  /* For heavily-used fields, use local variables as aliases to
     keep the code readable.  */
  pat_len     = ctxt->pat_len;
  pat_end     = ctxt->pat_end;
  delta2_list = ctxt->delta2_list;
  delta4      = ctxt->delta4;

  /* Work through each of the possible guard offset positions,
     and consider the potential delta2 matches that might fit in
     at each place (checking the entire section from the offset to
     the start of the pattern).  As noted before, offset -1 is
     irrelevant, as the match is complete by the earlier loop.  */
  for (guard_offset = -2; guard_offset >= 1 - pat_len; guard_offset--)
    {
      DEBUG_DELTA4 ({
          printf ("\nguard_offset %3d:\"%c\"",
                  guard_offset, pat_end[guard_offset]);
        });

      /* For each potential delta2 shift, see if the guard would
         still match:
         1. If Yes, we cannot consider any greater shift, as we
         might miss a match at this position, so the potential
         delta2 shift becomes the delta3 shift for that position.
         2. If No, then we can eliminate this candidate at this
         position, and move on to the next position (if any).  */
      for (i = 0; i < ctxt->nr_delta2_elements; i++)
        {
          int candidate;
          int match_pos;

          candidate = delta2_list[i];
          delta4[guard_offset] = candidate;
          DEBUG_DELTA4 (printf (" %2d:", candidate););

          for (match_pos = guard_offset;
               match_pos >= 1 - pat_len;
               match_pos--)
            {
              int part_match_offset;

              part_match_offset = match_pos - candidate;
              DEBUG_DELTA4 (printf ("%d", part_match_offset););
              if (part_match_offset <= -pat_len)
                break;
              DEBUG_DELTA4 (printf ("\"%c\"", pat_end[match_pos]););
              if (pat_end[match_pos] != pat_end[part_match_offset])
                {
                  DEBUG_DELTA4 (printf ("!=\"%c\"",
                                        pat_end[part_match_offset]););
                  delta4[guard_offset] = 0;
                  break;
                }
              else
                DEBUG_DELTA4 (printf (","););
            }
          if (delta4[guard_offset] != 0)
            break;
        }
    }

  DEBUG_DELTA4 (printf ("\n"););
}

bool
hstbm_pattern (hstbm_context_t *ctxt,
               const int len, const hstbm_byte_t *pattern)
{
  int i;

  CHECK_CONTEXT (ctxt);

  /* No charclasses are associated with simple patterns; the
     NULL or non-NULL value of charclass_list is used in some places
     to direct how the search is initialised.  */
  ctxt->charclass_list = NULL;

  /* Prepare items merely dependent on pattern length.  */
  if (! pattern_common_setup (ctxt, len))
    return false;

  /* Record the pattern text in our context.  We cannot rely on the
     caller to maintain the pattern after this call returns.  */
  memcpy (ctxt->canonical_pattern, pattern, len);

  /* The delta1 table was initialised to the pattern length in the
     common setup function; now, reduce delta for characters in the
     pattern.  */
  for (i = 0; i < len; i++)
    ctxt->delta1[pattern[i]] = len - i - 1;

  pattern_setup_simple_deltas_234 (ctxt);

  /* Is there scope for improving delta3 and/or delta4 skips over
     the skip that delta2 provides?  */
  if (ctxt->delta2 < len)
    {
      /* Yes, analyse the various guard/match combinations, choosing
         a larger skip value where possible.  */
      optimise_delta3_for_simple_pattern (ctxt);
      optimise_delta4_for_simple_pattern (ctxt);
    }
  else
    {
      /* No, merely set all delta3 and delta4 entries to the pattern
         length.  */
      for (i = 1 - len; i < -1; i++)
        {
          ctxt->delta3[i] = len;
          ctxt->delta4[i] = len;
        }
      ctxt->delta3[-1] = len;
    }

  pattern_setup_initial_guard (ctxt);

  return true;
}

/******************************************************************/

/* Code specific to token-based pattern description, especially
   including character classes.  */

/* Check that a single (octet) folding table can accommodate all of
   the folding required for all of the classes in the list  Returns
   false if this constraint cannot be met.  */
bool _GL_ATTRIBUTE_PURE
hstbm_pattern_supports_simple_folding (compiled_pattern_t
                                       *compiled_pattern)
{
  int nr_classes;
  charclass_t *work_class;
  int i;
  int j;

  /* Check that the supplied compiled pattern is valid.  */
  compiled_pattern_check (compiled_pattern);

  /* Check that the pattern token representation and octet pattern
     length match, as otherwise there is some sort of unexpected
     pattern component that we are not built to accommodate.  */
  if (compiled_pattern->pattern_len != compiled_pattern->nr_tokens)
    program_trouble_fatal (_("hstbm_pattern_supports_simple_folding: "
                             "pattern_len / nr_tokens mismatch"));

  /* Exclude the last class, as it is handled by the skip search,
     and so does not contribute to the (matcher/guard) fold
     table.  */
  nr_classes = compiled_pattern->pattern_len - 1;

  /* Acquire a work class to help with class operations.  */
  work_class = charclass_alloc ();

  for (i = 1; i < nr_classes; i++)
    {
      charclass_t *check_ccl;

      check_ccl = compiled_pattern->charclass_list[i];
      for (j = 0; j < i; j++)
        {
          charclass_t *prev_ccl;

          prev_ccl = compiled_pattern->charclass_list[j];

          /* If the new class is equal to an existing class, it is
             fully compatible.  Charclass aggressively pursues
             class-deduplication, so identical classes will usually
             have identical pointers.  */
          if (check_ccl == prev_ccl)
            continue;

          /* ?? If the following test succeeds, perhaps we should throw
             an assertion, as charclass deduplication is an important
             property... */
          if (charclass_equal (check_ccl, prev_ccl))
            continue;

          /* The classes are not equal... the only other tolerable
             relationship between an earlier class and the class
             being checked is that there are NO shared members, i.e.
             bitcount (check_ccl AND prev_ccl) == 0.  */
          charclass_copyset (check_ccl, work_class);
          charclass_intersectset (prev_ccl, work_class);
          if (charclass_bitcount (work_class) != 0)
            {
              /* Classes partially overlap... this is
                 unacceptable.  */
              charclass_abandon (work_class);
              return false;
            }
        }
    }

  /* Discard the work class that we acquired.  */
  charclass_abandon (work_class);

  /* No conflicts found between classes.  */
  return true;
}

static void
optimise_delta3_for_charclasses (hstbm_context_t *ctxt)
{
  delta_t *delta2_list;
  int pat_len;
  delta_t *delta3;
  int i;
  int guard_offset;
  charclass_t *work_class;

  /* For heavily-used fields, use local variables as aliases to keep
     the code readable.  */
  pat_len     = ctxt->pat_len;
  delta2_list = ctxt->delta2_list;
  delta3      = ctxt->delta3;

  /* Acquire a temporary charclass to perform class operations.  */
  work_class = charclass_alloc ();
  assert (work_class);

  /* Work through each of the possible guard offset positions,
     and consider the potential delta2 matches that might fit in
     at each place.  Before working through the delta2 candidate
     list, set the default delta3 skip for that offset to be the
     entire pattern length, which is correct if we don't find any
     other suitable delta3 candidate.  */
  for (guard_offset = -1; guard_offset >= 1 - pat_len; guard_offset--)
    {
      charclass_t *guard_class;

      guard_class = ctxt->charclass_list[pat_len + guard_offset - 1];

      DEBUG_DELTA3 (printf ("\nguard_offset %3d:\"%c\"",
                            guard_offset,
                            ctxt->pat_end[guard_offset]););

      /* For each potential delta2 shift, see if the guard would
         still match:
         1. If Yes, we cannot consider any greater shift, as we might
         miss a match at this position, so the potential delta2 shift
         becomes the delta3 shift for that position.
         2. If No, then we can eliminate this candidate at this
         position, and move on to the next position.  Note that we
         added an extra pat_len candidate to the list, so, in the
         absence of any other success, the combined_guard test is
         guaranteed to eventually succeed.  */
      for (i = 0; i < ctxt->nr_delta2_elements; i++)
        {
          int candidate;

          /* A position is only a candidate for a shift if every
             member of the original position is also a member at
             the shifted position.  (There may be more members
             that match at the shifted position, but that is not
             relevant here.)  */
          /* (Using the break/continue/break formulation may seem
             odd here, but the continue part becomes much more meaty
             when this code is cloned to handle the delta4 case.)  */
          candidate = delta2_list[i];
          delta3[guard_offset] = candidate;
          DEBUG_DELTA3 (printf (" %2d:", candidate););

          /* In the same fahion as the simple-pattern delta3 code
             above, we use the do {...} while 0 construct here as it
             results in code layout that is easier to adapt for the
             delta4 case.  */
          do
            {
              int combined_offset;
              charclass_t *combined_offset_class;

              combined_offset = guard_offset - candidate;
              DEBUG_DELTA3 (printf ("%d", combined_offset););
              if (combined_offset <= -pat_len)
                  break;
              DEBUG_DELTA3 (printf ("\"%c\"",
                                    ctxt->pat_end[guard_offset]););
              combined_offset_class
                = ctxt->charclass_list[pat_len + combined_offset - 1];
              charclass_copyset (combined_offset_class, work_class);
              charclass_intersectset (guard_class, work_class);
              if (charclass_bitcount (work_class) == 0)
                {
                  DEBUG_DELTA3 (printf ("!=\"%c\"",
                                        ctxt->pat_end[combined_offset]););
                  delta3[guard_offset] = 0;
                  break;
                }
              else
                DEBUG_DELTA3 (printf (","););
            } while (0);
          if (delta3[guard_offset] != 0)
            break;
        }
    }
  DEBUG_DELTA3 (printf ("\n"););

  /* We've finished working with classes, so abandon the temporary
     class that we acquired above.  */
  charclass_abandon (work_class);

  /* ?? If all delta3 items become pat_len as a result of the above
     loop, then set all delta4 items to pat_len, and tell caller
     not to bother with any delta4 analysis, as it cannot improve
     the situation.  */
  /* ?? Even if the all-pat-len case for delta3 isn't present, the
     generated delta3 table may improve on the simple-delta2
     default given to delta4... so copying delta3 to delta4 may be
     worthwhile here.  */
  /* ?? Not implemented yet.  */
}

static void
optimise_delta4_for_charclasses (hstbm_context_t *ctxt)
{
  delta_t *delta2_list;
  int pat_len;
  delta_t *delta4;
  int i;
  int guard_offset;
  charclass_t *work_class;

  /* For heavily-used fields, use local variables as aliases to keep
     the code readable.  */
  pat_len     = ctxt->pat_len;
  delta2_list = ctxt->delta2_list;
  delta4      = ctxt->delta4;

  /* Acquire a temporary charclass to perform class operations.  */
  work_class = charclass_alloc ();
  assert (work_class);

  /* Work through each of the possible guard offset positions,
     and consider the potential delta2 matches that might fit in
     at each place (checking the entire section from the offset to
     the start of the pattern).  As noted before, offset -1 is
     irrelevant, as the match is complete by the earlier loop.  */
  for (guard_offset = -2; guard_offset >= 1 - pat_len; guard_offset--)
    {
      DEBUG_DELTA4 (printf ("\nguard_offset %3d:\"%c\"",
                            guard_offset,
                            ctxt->pat_end[guard_offset]););

      /* For each potential delta2 shift, see if the guard would
         still match:
         1. If Yes, we cannot consider any greater shift, as we
            might miss a match at this position, so the potential
            delta2 shift becomes the delta4 shift for that position.
         2. If No, then we can eliminate this candidate at this
            position, and move on to the next position (if any).  */
      for (i = 0; i < ctxt->nr_delta2_elements; i++)
        {
          int candidate;
          int match_pos;

          candidate = delta2_list[i];
          delta4[guard_offset] = candidate;
          DEBUG_DELTA4 (printf (" %2d:", candidate););

          for (match_pos = guard_offset;
               match_pos >= 1 - pat_len; match_pos--)
            {
              int part_match_offset;
              charclass_t *match_pos_class;
              charclass_t *part_match_pos_class;

              part_match_offset = match_pos - candidate;
              DEBUG_DELTA4 (printf ("%d", part_match_offset););
              if (part_match_offset <= -pat_len)
                break;
              DEBUG_DELTA4 (printf ("\"%c\"",
                                    ctxt->pat_end[match_pos]););

              part_match_pos_class
                = ctxt->charclass_list[pat_len + part_match_offset - 1];
              match_pos_class
                = ctxt->charclass_list[pat_len + match_pos - 1];

              charclass_copyset (part_match_pos_class, work_class);
              charclass_intersectset (match_pos_class, work_class);
              if (charclass_bitcount (work_class) == 0)
                {
                  DEBUG_DELTA4 (printf ("~=\"%c\"",
                                        ctxt->pat_end[part_match_offset]););
                  delta4[guard_offset] = 0;
                  break;
                }
              else
                DEBUG_DELTA4 (printf (","););

            }
          if (delta4[guard_offset] != 0)
            break;
        }
    }
  DEBUG_DELTA4 (printf ("\n"););

  /* We've finished working with classes, so abandon the temporary
     class that we acquired above.  */
  charclass_abandon (work_class);
}

static void
expand_class (hstbm_context_t *ctxt,
              charclass_t *class,
              int position)
{
  int first_member;
  int class_member;
  int delta;
  ptrdiff_t nr_aliases;

  CHECK_CONTEXT (ctxt);

  /* Work out how many characters (if any) lie between this position
     and the last character of the pattern.  */
  delta = (ctxt->pat_len - 1) - position;

  /* If not done previously, initialise the fold table now with
     a 1-to-1 mapping of input characters to folded characters.  */
  if (! ctxt->fold_table_initialised)
    {
      int i;

      ctxt->fold_table_initialised = true;
      for (i = 0; i < NR_CHARS; i++)
        ctxt->fold_table[i] = (hstbm_byte_t) i;
    }

  /* First member of group is "canonical" representation.  */
  assert (charclass_next_member (class, 0, &first_member));
  ctxt->canonical_pattern[position] = first_member;
  ctxt->delta1[first_member] = delta;

  /* Next member, the first alias, is also special, as it can
     participate in memchr2 if there are only two members.  We
     allow classes that only have a single member, despite the
     expectation that they would have been optimised away by the
     lexer/parser.  */
  nr_aliases = 0;
  if (charclass_next_member (class,
                             first_member + 1,
                             &class_member))
    {
      ctxt->first_alias[-delta] = class_member;
      nr_aliases = 1;
      if (delta != 0)
        ctxt->fold_table[class_member] = first_member;
      ctxt->delta1[class_member] = delta;

      /* Work through remaining members (if any), setting up the
         character folding table and initial delta1 value.  */
      while (charclass_next_member (class,
                                    class_member + 1,
                                    &class_member))
        {
          if (delta != 0)
            ctxt->fold_table[class_member] = first_member;
          ctxt->delta1[class_member] = delta;
          nr_aliases++;
        }
    }

  /* Record how many aliases we found, so the search loop can look
     for optimisations where possible.  */
  if (nr_aliases >= NR_CHARS)
    assert (_(!"Cannot have more aliases in a class than"
              " all possible characters combined!"));
  ctxt->nr_aliases[-delta] = (hstbm_byte_t) nr_aliases;

  /* If a non-trivial class occurs anywhere except the final position
     (which is covered by the delta1 skip table), then the search
     must use a folding-aware matcher loop.  */
  if ((delta > 0) && (nr_aliases > 0))
    ctxt->folded_match = true;
}

/* Alternate pattern specification interface, utilising pattern-lex
   tokens, allowing more sophisticated patterns that are still
   compatible with this module (e.g. "abc[123]987").  */
bool
hstbm_pattern_compiled (hstbm_context_t *ctxt,
                        compiled_pattern_t *compiled_pattern)
{
  int len;
  pattern_lex_token_t *tokens;
  int i;

  CHECK_CONTEXT (ctxt);
  compiled_pattern_check (compiled_pattern);

  /* Check that the pattern token representation and octet pattern
     length match, as otherwise there is some sort of unexpected
     pattern component that we are not built to accommodate.  */
  if (compiled_pattern->pattern_len != compiled_pattern->nr_tokens)
    program_trouble_fatal (_("hstbm_pattern_compiled: "
                             "pattern_len / nr_tokens mismatch"));

  /* Record the supplied classes, as we use them for constructing
     delta3/4 shifts.  */
  ctxt->charclass_list = compiled_pattern->charclass_list;

  /* Hack here, incorrect because not all tokens map to pattern
     character positions, but allowed for now merely because we know
     that our only client, the stripped-down demonstration test rig,
     doesn't emit these tokens.  */
  len = compiled_pattern->nr_tokens;
  tokens = compiled_pattern->token_list;

  /* Prepare items merely dependent on pattern length.  */
  if (! pattern_common_setup (ctxt, len))
    return false;

  /* Work through tokens, again only handling a small subset of the
     full lexicon, but enough for demonstration purposes.  */
  for (i = 0; i < compiled_pattern->nr_tokens; i++)
    {
      pattern_lex_opcode_t opcode;
      pattern_lex_param_t param;

      opcode = PATTERN_LEX_GET_OPCODE (tokens[i]);
      param  = PATTERN_LEX_GET_PARAM  (tokens[i]);
      switch (opcode)
        {
        case PATTERN_LEX_OP_CHAR_DIRECT:
          {
            hstbm_byte_t b;

            assert (param <= NR_CHARS);
            b = (hstbm_byte_t) param;
            ctxt->delta1[b] = len - i - 1;
            ctxt->canonical_pattern[i] = b;
            ctxt->nr_aliases[i - len + 1] = 0;
          }
          break;

        case PATTERN_LEX_OP_CHAR_CLASS:
          {
            charclass_t *ccl;

            ccl = charclass_get_pointer (param);
            expand_class (ctxt, ccl, i);
          }
          break;

        default:
          program_trouble (_("Unknown token in hstbm_token_pattern"));
        }
    }

  pattern_setup_simple_deltas_234 (ctxt);

  /* Is there scope for improving delta3 and/or delta4 skips over
     the skip that delta2 provides?  */
  if (ctxt->delta2 < len)
    {
      /* Yes, analyse the various guard/match combinations, choosing
         a larger skip value where possible.  */
      optimise_delta3_for_charclasses (ctxt);
      optimise_delta4_for_charclasses (ctxt);
    }
  else
    {
      /* No, merely set all delta3 and delta4 entries to the pattern
         length.  */
      for (i = 1 - len; i < -1; i++)
        {
          ctxt->delta3[i] = len;
          ctxt->delta4[i] = len;
        }
      ctxt->delta3[-1] = len;
    }

  pattern_setup_initial_guard (ctxt);

  return true;
}

/******************************************************************/

void
hstbm_buffer (hstbm_context_t *ctxt,
              hstbm_byte_t *buf_start, hstbm_byte_t *buf_end)
{
  CHECK_CONTEXT (ctxt);

  /* Record the supplied parameters.  */
  ctxt->buf = buf_start;
  ctxt->buf_end = buf_end;

  /* Start searching at the start of the buffer.  */
  ctxt->search = &buf_start[ctxt->pat_len - 1];
}

/******************************************************************/

/* Helper function for where the "skip" search is not making fast
   progress.  This function focuses on the current ("floating")
   search guard, and/or a pre-selected "static" guard, and tries to
   use memchr and/or memchr2 to search for match candidates.  The
   mixing of these memchr-family search(es) with the more
   conventional Boyer-Moore-family search is the reason why this
   module is called the "Hybrid" Self-Tuning Boyer-Moore.

   The function deliberately focuses on the guard(s), and not the
   skip character, so as to add diversity to the character set(s)
   being searched, and so hopefully make the overall code less
   sensitive to being poisoned by pathological data.

   The function receives the search context, the current search
   position, and the guard offset as parameters, and returns a new
   proposed search position.  The return value may be NULL if no
   suitable guard was found, indicating that no match is
   feasible.  */
/* ?? REVIEWME: C89 doesn't allow "inline" functions... I think.  */
static /* inline */ const hstbm_byte_t *
hybrid_memchr_search (hstbm_context_t *ctxt,
                      const hstbm_byte_t *search,
                      int guard_offset)
{
  int first_offset;
  int second_offset;

  /* See if we can fit in two consecutive memchr scans:
      - One based on the "dynamic" guard currently chosen by the
        self-tuning component of the S-T Boyer-Moore search; and/or
      - One based on a selection from a list of candidates, compiled
        in setup_initial_guard.

     If there's *no* static candidates, we should never enter this
     function:  Search preparation logic should identify this case,
     and turn off (set to 0) the efficiency threshold.

     There's a number of things that complicate matters on top of
     the above description:
      - If there are multiple static candidates, we use a
        round-robin selection process in the hope that we are less
        likely to fall afoul of (pathalogically) unfriendly data;
      - If the guard and the current static candidate coincide, use
        the round-robin protocol to create diversity.  If there's
        only one static candidate, we can only do one search;
      - If the position for the delta has more than one alias, we
        cannot use memchr (0 aliases) or memchr2 (1 alias), and so
        must remove it as a contender here; and
      - If we have two candidates, we must search for them in order,
        (position nearer the start of the pattern first), as
        otherwise we might miss a match candidate.

     Since any feasible offset is <= 0, we use 1 as a sentinel
     to mean "no selection", in the macro SENTINEL_NO_MEMCHR_SEARCH.
  */
#define SENTINEL_NO_MEMCHR_SEARCH 1
  first_offset = SENTINEL_NO_MEMCHR_SEARCH;
  second_offset = SENTINEL_NO_MEMCHR_SEARCH;

  /* Place guard_offset into first_offset if nr_aliases for its
     position is < 2, which means that memchr or memchr2 can be
     used.  */
  if (ctxt->nr_aliases[guard_offset] < 2)
    first_offset = guard_offset;

  /* Place the next static candidate into second_offset, and move to
     the next candidate (round-robin) in the list.  */
  /* Earlier, we hacked the static offset list to fit into a single
     byte as a value 1..255, by negating the integer... so, here,
     we need to undo the hack by negating second_offset.  We do the
     negation as an independent statement, as second_offset is a
     signed type, whereas hstbm_byte_t is unsigned, and we may be
     straying into undefined territory by trying to negate it.  */
  second_offset
    = ctxt->memchr_offset_list[ctxt->current_memchr_offset_index++];
  second_offset = -second_offset;
  if (ctxt->current_memchr_offset_index
      >= ctxt->nr_memchr_offset_items)
    ctxt->current_memchr_offset_index = 0;

  /* Do the first and second offsets coincide?  */
  if (first_offset == second_offset)
    {
      /* Yes, is there more than one static candidate? */
      if (ctxt->nr_memchr_offset_items > 1)
        {
          /* Yes, use the current item (without additional
             round-robin action).  (Again, second_offset negation
             hack applies.)  */
          second_offset
            = ctxt->memchr_offset_list[ctxt->current_memchr_offset_index];
          second_offset = -second_offset;
        }
      else
        /* No, cancel the second offset.  */
        second_offset = SENTINEL_NO_MEMCHR_SEARCH;
    }

  /* Does the second offset come before the first offset?  */
  if (second_offset < first_offset)
    {
      int temp_swap_offset;

      /* Yes, swap them around so that consecutive scans are not
         vulnerable to potentially missing a match candidate.

         Note that this code also sorts the offsets, such that if
         only one scan is possible, it is guaranteed to be in
         first_offset (i.e. <= 0), and second_offset will be 1
         (SENTINEL_NO_MEMCHR_SEARCH).  */
      temp_swap_offset = first_offset;
      first_offset     = second_offset;
      second_offset    = temp_swap_offset;
    }

  /* Scan for potential match candidates using first_offset.  */
  {
    const hstbm_byte_t *scan;
    ptrdiff_t buf_remain;

    scan = search + first_offset;
    buf_remain = ctxt->buf_end - search + 1;
    if (ctxt->nr_aliases[first_offset] == 0)
      scan = memchr (scan,
                     ctxt->pat_end[first_offset],
                     buf_remain);
    else
      scan = memchr2 (scan,
                      ctxt->pat_end[first_offset],
                      ctxt->first_alias[first_offset],
                      buf_remain);

    /* NULL means no candidate found, so we cannot match.  */
    if (scan == NULL)
        return NULL;

    /* Okay, incorporate the advance from the scan into the search
       position (we subtract the offset to get an end-of-pattern
       position).  */
    search = scan - first_offset;
  }

  /* If second_offset also names a candidate, scan for it as well.  */
  if (second_offset != SENTINEL_NO_MEMCHR_SEARCH)
  {
    const hstbm_byte_t *scan;
    ptrdiff_t buf_remain;

    scan = search + second_offset;
    buf_remain = ctxt->buf_end - search + 1;
    if (ctxt->nr_aliases[second_offset] == 0)
      scan = memchr (scan,
                     ctxt->pat_end[second_offset],
                     buf_remain);
    else
      scan = memchr2 (scan,
                      ctxt->pat_end[second_offset],
                      ctxt->first_alias[second_offset],
                      buf_remain);

    /* NULL means no candidate found, so we cannot match.  */
    if (scan == NULL)
        return NULL;

    /* Okay, incorporate the advance from the scan into the search
       position (we subtract the offset to get an end-of-pattern
       position).  */
    search = scan - second_offset;
  }

  return search;
}

/******************************************************************/

/* Forward declaration of search_with_folded_match here.  We arrange
   the code like this as the folded-match case is a little easier to
   understand once the more direct exact-match code has been
   presented.  */
const hstbm_byte_t *
search_with_folded_match (hstbm_context_t *ctxt,
                          hstbm_len_t initial_skip);

const hstbm_byte_t *
hstbm_search (hstbm_context_t *ctxt, hstbm_len_t initial_skip)
{
  const hstbm_byte_t *search;
  delta_t *delta1;
  delta_t delta;
  delta_t delta2;
  hstbm_byte_t guard;
  int guard_offset;
  const hstbm_byte_t *pat_end;
  const hstbm_byte_t *sentinel;
  skip_phase_t skip_phase;

  CHECK_CONTEXT (ctxt);

  /* If case-folded matching required, use a separate search function
     so that this function can remain focussed and streamlined on the
     exact (no case-folded match portion) match implementation.  */
  if (ctxt->folded_match)
    return search_with_folded_match (ctxt, initial_skip);

  /* Set up local variables to hold or reference information heavily
     used in the inner loop(s).  These help the compiler identify
     optimisations.  */
  search  = ctxt->search + initial_skip;
  delta1  = ctxt->delta1;
  pat_end = ctxt->pat_end;
  guard_offset = ctxt->guard_offset;
  guard   = pat_end[guard_offset];
  delta2  = ctxt->delta2;

  /* Set up a near-end-of-buffer sentinel so that the big skip loop
     cannot overrun the buffer end in the worst case.  */
  sentinel = &ctxt->buf_end[-13 * ctxt->pat_len];
  skip_phase = SKIP_PHASE_BIG_LOOP;

 skip_search_bigloop_top:
  while ((sentinel - search) >= 0)
    {
      const hstbm_byte_t *iter_search_start;

      /* Monitor speed of the skip loop as it may be inefficient
         relative to alternative algorithms for some datasets.  */
      /* ?? REVIEWME: Possible pathalogical case here, due to the
         use of "goto try_match" below, which, if unsuccessful,
         jumps back to the label above.  This combination can stop
         the code from considering efficiency (memchr) alternatives
         in places where it might be desirable.  Perhaps move the
         iter_search_start variable to outside the loop, and move
         the efficiency threshold code to the top of the loop?  */
      iter_search_start = search;

      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == search[guard_offset])
            goto try_match;
          search += delta2;
        }

      /* Skip loop hasn't located a candidate, (good!), keep
         going.  */
      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == search[guard_offset])
            goto try_match;
          search += delta2;
        }

      search += delta1[*search];
      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == search[guard_offset])
            goto try_match;
          search += delta2;
        }

      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == search[guard_offset])
            goto try_match;
          search += delta2;

        }

      /* If the efficiency of the skip (delta1 plus guard) search is
         high, keep using it.  */
      if ((search - iter_search_start)
          >= ctxt->skip_loop_efficiency_threshold)
        continue;

      /* A match candidate may have sneaked in as a result of the
         last delta2 jump above.  Check for it here before letting
         the alternative (memchr) search algorithms have a go.  */
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == search[guard_offset])
            goto try_match;
        }

      /* Momentarily switch to using memchr and/or memchr2 to search
         for one or two match candidates as a way of diversifying
         the search, as we've measured that the main skip scan isn't
         efficient at present.  */
      search = hybrid_memchr_search (ctxt, search, guard_offset);
      if (search == NULL)
        {
          ctxt->guard_offset = guard_offset;
          return NULL;
        }

    }

  /* Refine the sentinel to be nearer the end, as the "small" skip
     loop below checks more often for the end-of-buffer
     condition.  */
  sentinel = &ctxt->buf_end[-3 * ctxt->pat_len];
  skip_phase = SKIP_PHASE_SMALL_LOOP;
  /* fallthrough */

 skip_search_small_loop_top:
  while ((sentinel - search) >= 0)
    {
      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta != 0)
        continue;
      if (guard == search[guard_offset])
        goto try_match;
      search += delta2;
    }

  /* If we reach here, we VERY are near the end of the buffer.
     Search remaining bytes, but check carefully for buffer end.  */
  sentinel = ctxt->buf_end;
  skip_phase = SKIP_PHASE_INDIVIDUAL;
  /* fallthrough */

 skip_search_individual_top:
  while ((sentinel - search) >= 0)
    {
      delta   = delta1[*search];
      search += delta;
      if (delta != 0)
        continue;
      if (guard == search[guard_offset])
        goto try_match;
      search += delta2;
    }

  /* Failed to find match.  */
  ctxt->search = ctxt->buf_end;
  return NULL;

 try_match:
  /* Use local scope here to yell at the compiler that the index
     variable i is temporary and should be in a register.  */
  {
    int i;
    /* Round-robin match, part 1: positions [guard - 1
                                  .. (-len) + 1].  */
    for (i = guard_offset - 1; i > -ctxt->pat_len; i--)
      if (search[i] != pat_end[i])
        {
          search += ctxt->delta3[guard_offset];
          guard_offset = i;
          goto match_failed;
        }
  }

  {
    int i;
    /* Round-robin match, part 2: positions [-1
                                  .. guard + 1].  */
    for (i = -1; i > guard_offset; i--)
      {
        if (search[i] != pat_end[i])
          {
            search += ctxt->delta4[guard_offset];
            guard_offset = i;
            goto match_failed;
          }
      }
  }

  /* Found a match.  Adjust the search position to be the first
     matching character of the pattern, and both record it as our
     position and as the result returned to the caller.  */
  ctxt->guard_offset = guard_offset;
  search -= ctxt->pat_len - 1;
  ctxt->search = search;
  return search;

 match_failed:
  guard = pat_end[guard_offset];

  /* Return to skip loop, trying to give priority to the
     high-efficiency unrolled loop variants.  */
  /* ?? Could change skip_phase to be a "computed goto", but this
     wouldn't be Standard C.  */
  /* We could clone "try_match" code for big/small/individual match
     attempts, but have decided that the risk of expanded code size
     leading to poorer cache coherency is non-trivial.  */
  switch (skip_phase)
    {
    case SKIP_PHASE_BIG_LOOP:
      goto skip_search_bigloop_top;

    case SKIP_PHASE_SMALL_LOOP:
      goto skip_search_small_loop_top;

    default:
    case SKIP_PHASE_INDIVIDUAL:
      goto skip_search_individual_top;
    }
}

#if 0
#  define BUF(where, ctxt, guard_offset)                    \
    do {                                                    \
      int buf_i; int pat_len = (ctxt)->pat_len;             \
      printf ("%c.Buf: %*s|\n%4d   ", (where),              \
              pat_len, "", (guard_offset));                 \
      for (buf_i = -pat_len; buf_i < pat_len; buf_i++)      \
        putchar (search[buf_i]);                            \
      putchar ('\n'); fflush (NULL);                        \
    } while (0)
#else
#  define BUF(where, ctxt, guard_offset)                    \
    do {} while (0)
#endif

const hstbm_byte_t *
search_with_folded_match (hstbm_context_t *ctxt,
                          hstbm_len_t initial_skip)
{
  const hstbm_byte_t *search;
  delta_t *delta1;
  hstbm_byte_t *fold_table;
  delta_t delta;
  delta_t delta2;
  hstbm_byte_t guard;
  int guard_offset;
  const hstbm_byte_t *pat_end;

  const hstbm_byte_t *sentinel;
  skip_phase_t skip_phase;

  /* Set up local variables to hold or reference information heavily
     used in the inner loop(s).  These help the compiler identify
     optimisations.  */
  search     = ctxt->search + initial_skip;
  delta1     = ctxt->delta1;
  fold_table = ctxt->fold_table;
  pat_end    = ctxt->pat_end;
  guard_offset = ctxt->guard_offset;
  guard      = pat_end[guard_offset];
  delta2     = ctxt->delta2;

  /* Set up a near-end-of-buffer sentinel so that the big skip loop
     cannot overrun the buffer end in the worst case.  */

  sentinel = &ctxt->buf_end[-13 * ctxt->pat_len];
  skip_phase = SKIP_PHASE_BIG_LOOP;

 skip_search_bigloop_top:
  while ((sentinel - search) >= 0)
    {
      const hstbm_byte_t *iter_search_start;

      /* Monitor speed of the skip loop as it may be inefficient
         relative to alternative algorithms for some datasets.  */
      iter_search_start = search;
      BUF ('A', ctxt, guard_offset);

      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == fold_table[search[guard_offset]])
            goto try_match;
          search += delta2;
        }

      /* Skip loop hasn't located a candidate, (good!), keep
         going.  */
      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == fold_table[search[guard_offset]])
            goto try_match;
          search += delta2;
        }

      search += delta1[*search];
      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == fold_table[search[guard_offset]])
            goto try_match;
          search += delta2;
        }

      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == fold_table[search[guard_offset]])
            goto try_match;
          search += delta2;
        }

      /* If the efficiency of the skip (delta1) search is high, keep
         using it.  */
      if ((search - iter_search_start)
          >= ctxt->skip_loop_efficiency_threshold)
        continue;

      /* A match candidate may have sneaked in as a result of the
         last delta2 jump above.  Check for it here before letting
         the alternative (memchr) search algorithms have a go.  */
      delta   = delta1[*search];
      search += delta;
      if (delta == 0)
        {
          if (guard == fold_table[search[guard_offset]])
            goto try_match;
        }

      /* Momentarily switch to using memchr and/or memchr2 to search
         for one or two match candidates as a way of diversifying
         the search, as we've measured that the main skip scan isn't
         efficient at present.  */
      search = hybrid_memchr_search (ctxt, search, guard_offset);
      if (search == NULL)
        {
          ctxt->guard_offset = guard_offset;
          return NULL;
        }

    }

  /* Refine the sentinel to be nearer the end, as the "small" skip
     loop below checks more often for the end-of-buffer
     condition.  */
  sentinel = &ctxt->buf_end[-3 * ctxt->pat_len];
  skip_phase = SKIP_PHASE_SMALL_LOOP;
  /* fallthrough */

 skip_search_small_loop_top:
  while ((sentinel - search) >= 0)
    {
      BUF ('B', ctxt, guard_offset);
      search += delta1[*search];
      search += delta1[*search];
      delta   = delta1[*search];
      search += delta;
      if (delta != 0)
        continue;
      if (guard == fold_table[search[guard_offset]])
        goto try_match;
      search += delta2;
    }

  /* If we reach here, we VERY are near the end of the buffer.
     Search remaining bytes, but check carefully for buffer end.  */
  sentinel = ctxt->buf_end;
  skip_phase = SKIP_PHASE_INDIVIDUAL;
  /* fallthrough */

 skip_search_individual_top:
  while ((sentinel - search) >= 0)
    {
      BUF ('C', ctxt, guard_offset);
      delta   = delta1[*search];
      search += delta;
      if (delta != 0)
        continue;
      if (guard == fold_table[search[guard_offset]])
        goto try_match;
      BUF ('D', ctxt, guard_offset);
      search += delta2;
    }

  /* Failed to find match.  */
  ctxt->search = ctxt->buf_end;
  return NULL;

 try_match:
  /* Use local scope here to yell at the compiler that the index
     variable i is temporary and should be in a register.  */
  {
    /* Round-robin match, part 1: positions [guard - 1
                                  .. (-len) + 1].  */
    int i;
    BUF ('X', ctxt, guard_offset);

    for (i = guard_offset - 1; i > -ctxt->pat_len; i--)
      if (fold_table[search[i]] != pat_end[i])
        {
          search += ctxt->delta3[guard_offset];
          guard_offset = i;
          goto match_failed;
        }
  }

  {
    /* Round-robin match, part 2: positions [-1
                                  .. guard + 1].  */
    int i;
    BUF ('Y', ctxt, guard_offset);

    for (i = -1; i > guard_offset; i--)
      if (fold_table[search[i]] != pat_end[i])
        {
          search += ctxt->delta4[guard_offset];
          guard_offset = i;
          goto match_failed;
        }
  }
  BUF ('Z', ctxt, guard_offset);

  /* Found a match.  Adjust the search position to be the first
     matching character of the pattern, and both record it as our
     position and as the result returned to the caller.  */
  ctxt->guard_offset = guard_offset;
  search -= ctxt->pat_len - 1;
  ctxt->search = search;
  return search;

 match_failed:
  guard = pat_end[guard_offset];

  /* Return to skip loop, trying to give priority to the
     high-efficiency unrolled loop variants.  */
  /* ?? Could change skip_phase to be a "computed goto", but this
     wouldn't be Standard C.  */
  /* We could clone "try_match" code for big/small/individual match
     attempts, but have decided that the risk of expanded code size
     leading to poorer cache coherency is non-trivial.  */
  switch (skip_phase)
    {
    case SKIP_PHASE_BIG_LOOP:
      goto skip_search_bigloop_top;

    case SKIP_PHASE_SMALL_LOOP:
      goto skip_search_small_loop_top;

    default:
    case SKIP_PHASE_INDIVIDUAL:
      goto skip_search_individual_top;
    }
}

/******************************************************************/

/* Debug items.  I tend to write my own diagnostics, a hangover of
   my real-time/embedded-systems background.  I should probably try
   to become proficient in a good, cross-platform debugger one of
   these days.  */

static void
show_ctxt_byte_slab (const char *label,
                     int indent,
                     hstbm_byte_t *values,
                     bool is_offsets)
{
  int i;
  const char len[] = "0123456789abcdefghijklmnopqrstuvwxyz+";

  fprintf (stderr, "- %s:", label);
  for (i = 0; i < NR_CHARS; i++)
    {
      int v;

      v = (int) values[i];
      if ((i % 64) == 0)
        fprintf (stderr, "\n%*s", indent, "");
      if (is_offsets)
        fputc (v < 36 ? len[v] : '+', stderr);
      else
        fputc (isprint (v) ? (char) v : '.', stderr);
      if ((i % 32) == 31)
        fputc (' ', stderr);
    }
  fputc ('\n', stderr);
}

static void
show_ctxt_pattern_slab (const char *label,
                        hstbm_byte_t *values,
                        int nr_items,
                        const char *format)
{
  bool MakeValuePrintable;
  int i;

  MakeValuePrintable = false;
  if (strstr (format, "   %c") != NULL)
    MakeValuePrintable = true;
  fprintf (stderr, "- %12s: ", label);
  for (i = 0; i < nr_items; i++)
    {
      int v;

      v = values[i];
      if (MakeValuePrintable)
        v = isprint (v) ? v : '.';
      fprintf (stderr, format, v);
    }
  fputc ('\n', stderr);
}

void
hstbm_debug_show_context (hstbm_context_t *ctxt)
{
  int pat_len;
  int i;
  const char *fold_table_title;

  fflush (NULL);

  pat_len = ctxt->pat_len;
  fprintf (stderr, _("\n* debug: hstbm context at %p:\n"),
           (void *) ctxt);

  fold_table_title = _("fold_table");
  if (! ctxt->folded_match)
    fold_table_title = _("fold_table (not used)");
  show_ctxt_byte_slab (fold_table_title, 4, ctxt->fold_table, false);

  show_ctxt_byte_slab (_("delta1"), 4, ctxt->delta1, true);

  show_ctxt_pattern_slab (_("Pattern"),
                          ctxt->canonical_pattern,
                          pat_len,
                          "   %c");

  fprintf (stderr,
         _("\n* Hybrid (memchr/memchr2) efficiency variables:\n"));
  fprintf (stderr, "- skip_loop_efficiency_threshold: %ld\n",
         ctxt->skip_loop_efficiency_threshold);
  show_ctxt_pattern_slab (_(" nr_aliases"),
                          &ctxt->nr_aliases[1 - pat_len],
                          pat_len,
                          " %3d");
  show_ctxt_pattern_slab (_("first_alias"),
                          &ctxt->first_alias[1 - pat_len],
                          pat_len,
                          "   %c");
  show_ctxt_pattern_slab (_("memchr_offset_list [negated]"),
                          ctxt->memchr_offset_list,
                          ctxt->nr_memchr_offset_items,
                          " %3d");
  fprintf (stderr, _("- current_memchr_offset_index: %d\n"),
         ctxt->current_memchr_offset_index);

  fprintf (stderr,
           _("\n* Guard (delta2) and match (delta 3/4) skip variables:\n"));
  fprintf (stderr, _("- delta2 candidate(s): "));
  for (i = 0; i < ctxt->nr_delta2_elements; i++)
    fprintf (stderr, " %2d", ctxt->delta2_list[i]);
  fputc ('\n', stderr);

  fprintf (stderr, _("- delta2: %d\n"), ctxt->delta2);

  show_ctxt_pattern_slab (_("delta3"),
                          &ctxt->delta3[1 - pat_len],
                          pat_len - 1,
                          " %3d");
  show_ctxt_pattern_slab (_("delta4"),
                          &ctxt->delta4[1 - pat_len],
                          pat_len - 2,
                          " %3d");

  fprintf (stderr, _("\n* context check... "));
  fflush (NULL);
  CHECK_CONTEXT (ctxt);
  fprintf (stderr, _("[OK]\n"));
}

/* vim:set shiftwidth=2: */
