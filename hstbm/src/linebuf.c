/* linebuf -- Present a line-based file in a cached memory buffer.

   Given a client that wants to deal with a multi-line file using
   linear memory buffers, this module tries to find the performance
   sweet spot between the operating system (OS), that typically maps
   the memory in medium-sized blocks (chunks of 4k or perhaps 8k), the
   CPU cache memory architecture, where overlapping reuse of a small
   buffer area leads to high cache hit rates and therefore low memory
   access latency and the application, which wants to deal with the
   file mainly at the line abstraction, wihthout having to cope with a
   buffer starting or ending part-way through a line.

   We achieve this by:
     (a) Choosing a modest buffer size, that is a small multiple of
         8192 bytes (e.g. 32768 bytes);
     (b) Having a memory block preceding the buffer proper that is
         the same size (32k).  This block serves three purposes:
         (b.1) If the buffer start and line start coincide, then a
               single EOL byte is added before the buffer, so that
               memrchr () searches for line start always succeed;
         (b.2) If part of a line is at the end of the buffer, the
               effective buffer size is shortened to exclude that line
               (while keeping the EOL at the end of the preceding
               line).  The incomplete line (again, plus preceding EOL)
               is copied to the area preceding the main buffer, so
               that the next file read into the bufer can maintain OS
               and cache optimal alignment, and the line is terminated
               by an EOL in the next buffer segment.
         (b.3) We overallocate the buffer by another block_size
               amount, as malloc does not guarantee alignment to this
               size, then add an offset so block alignment is
               assured.

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
   Grep, especially the infrastructure code in grep.c, which has the
   attribution:
        Written July 1992 by Mike Haertel.
   Many others have contributed to the sources.  See the files AUTHORS
   and THANKS, as well as the git changelog, in the GNU Grep package
   for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

#include <config.h>

/* (Note that we get memrchr from <string.h>, as a result of
   <config.h> defining _GNU_SOURCE.)  */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include "linebuf.h"
#include "misc-gettext.h"
#include "program-trouble.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Flesh out anonymous type defined in the header.  */
struct linebuf_context_struct
{
/* Signature identifying top of struct (after alignment-critical
   elements, because I'm too tired to use offsetof () and do the
   necessary alloc mathematics right now), which also serves as a
   canary that may help unmask unintended damage by outsiders.  */
  unsigned long ctxt_top_fence_magic_nr;

  /* File name, descriptor and end-of-data flag.  */
  char *filename;
  int fd;
  bool eof;

  /* Basic buffer details.  */
  void *malloc_ptr;
  size_t buf_size;
  size_t block_size;

  /* Nominal buffer start and end, aligned for OS file buffering speed
     and, hopefully, also high memory cache hit rate.  */
  linebuf_byte_t *nominal_buf_start;
  linebuf_byte_t *nominal_buf_end;

  /* Partial line at end of buffer, if any.  */
  linebuf_byte_t *residue;

  /* Signature/canary token for bottom of struct.  */
  unsigned long ctxt_bottom_fence_magic_nr;
};

#define CTXT_TOP_FENCE_MAGIC_NR     0xdc8a721auL
#define CTXT_BOTTOM_FENCE_MAGIC_NR  0xc0e5169buL
#define CHECK_CONTEXT(ctxt) \
  assert (((ctxt) != NULL)         \
   && ((ctxt)->ctxt_top_fence_magic_nr    == CTXT_TOP_FENCE_MAGIC_NR)  \
   && ((ctxt)->ctxt_bottom_fence_magic_nr == CTXT_BOTTOM_FENCE_MAGIC_NR))

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

linebuf_context_t *
linebuf_open (char *filename, size_t block_size, size_t buf_size)
{
  linebuf_context_t *new_ctxt;
  linebuf_byte_t *buf;

  /* Check that block_size is a power of two.  */
  if ((block_size & (block_size - 1)) != 0)
    program_trouble_fatal (_("block_size is not a power of two"));

  /* Check that buf_size is a multiple of the block size. */
  if ((buf_size % block_size) != 0)
    program_trouble_fatal
       (_("buf_size is not an integral multiple of block_size"));

  new_ctxt = malloc (sizeof *new_ctxt);
  assert (new_ctxt);
  memset (new_ctxt, 0, sizeof (*new_ctxt));

  /* Set up canary/sentinel magic numbers.  */
  new_ctxt->ctxt_top_fence_magic_nr    = CTXT_TOP_FENCE_MAGIC_NR;
  new_ctxt->ctxt_bottom_fence_magic_nr = CTXT_BOTTOM_FENCE_MAGIC_NR;

  new_ctxt->block_size = block_size;
  new_ctxt->buf_size   = buf_size;

  /* If filename is NULL, use standard input instead.  */
  if (filename == NULL)
    {
      new_ctxt->filename = _("(standard input)");
      new_ctxt->fd = STDIN_FILENO;
    }
  else
    {
      new_ctxt->filename = filename;
      new_ctxt->fd       = open (filename, O_RDONLY);
    }

  if (new_ctxt->fd < 0)
    {
      /* ?? FIXME (maybe): No _(...) internationalisation?  */
      program_trouble ("%s: %s", new_ctxt->filename,
                       strerror (errno));
      free (new_ctxt);
      return NULL;
    }
  new_ctxt->eof = false;

  /* Acquire buffer memory, plus leading and trailing margins.
     The block size is trebled to fulfil the requirements:
     1. Some space may be used to align to a block boundary;
     2. Some text, notably a newline, precedes the buffer text to
        reduce the edge cases that clients need to worry about; and
     3. Some text, notably a newline, follows the buffer text to
        reduce the edge cases that clients need to worry about.
     The buffer size is doubled to fulfil the requirement:
     1. If a whole buffer isn't a complete line, copy it to the
        space preceding the read area, and read another portion of
        the file.  If done properly, we should keep doing this,
        increasing buffer size as required, until a line is in
        memory.  However, this initial rig is extremely half-hearted
        about this, and bails out with an assertion if the line does
        not fit into two buffer blocks.  (Sigh.)  */
  buf = malloc (block_size * 3 + buf_size * 2);
  new_ctxt->malloc_ptr = buf;
  assert (buf);

  /* Align buffer to block size.  */
  /* ?? Unsure about negative ptrdiff value... assert for now, sigh.  */
  ALIGN_TO_BLOCK_BOUNDARY (buf, block_size);

  /* Find nominal buffer start (if no leftover text from a previous
     buffer).  */
  new_ctxt->nominal_buf_start = buf + buf_size;
  new_ctxt->nominal_buf_end   = new_ctxt->nominal_buf_start
              + buf_size - 1;

  /* Some algorithms demand that the memory block has a trailing
     newline, so write this in the trailing margin following the
     nominal buffer.  */
  new_ctxt->nominal_buf_end[1] = '\n';

  /* Start off with no residue from the previous buffer to prepend to
     the current buffer.  */
  new_ctxt->residue = NULL;

  CHECK_CONTEXT (new_ctxt);
  return new_ctxt;
}

bool
linebuf_read (linebuf_context_t *ctxt,
              linebuf_byte_t **p_buf_start,
              linebuf_byte_t **p_buf_end)
{
  linebuf_byte_t *buf_start;
  linebuf_byte_t *buf_end;
  size_t read_size_req;
  ssize_t result;

  CHECK_CONTEXT (ctxt);

  /* Return false if we have found the end of the file.  */
  if (ctxt->eof)
    return false;

  /* Default to placing buffer on nominal boundaries.  */
  buf_start = ctxt->nominal_buf_start;
  buf_end   = ctxt->nominal_buf_end;

  /* Do we have any residue from the current buffer? */
  if (ctxt->residue)
    {
      /* Yes, copy them to a place preceding the nominal buffer.  */
      ptrdiff_t size;

      size = ctxt->nominal_buf_end - ctxt->residue + 1;

      buf_start = &ctxt->nominal_buf_start[-size];
      memmove (buf_start, ctxt->residue, size);
    }

  /* Read in next block of bytes from the file.  */
  read_size_req = ctxt->buf_size;
  result = read (ctxt->fd, ctxt->nominal_buf_start, read_size_req);

  if (result == 0)
    return false;

  /* ?? Very naive:  Tread any negative return as an error.  This is
     incorrect in some situations, but we are only striving for a
     proof-of-concept implementation, not a buulet-proof one.  */
  if (result < 0)
    {
      program_trouble
        (_("error reported when reading data from file"));
      return false;
    }

  /* Did we read all the buffer bytes that were requested?  */
  if (result == read_size_req)
    {
      linebuf_byte_t *last_newline;

      /* Yes, trim off any partial line at the end of the buffer, and
         record it as residue to prepend to the next buffer.  */
      last_newline = memrchr (ctxt->nominal_buf_start, '\n',
                              read_size_req);
      if (last_newline == NULL)
        {
          /* The file has very long lines:  Update the residue, double
             the block size, allocate and align a new buffer, copy the
             old buffer (with extended residue) over to the new, free
             the old buffer, and try again.  */
          ctxt->residue += read_size_req;
          ctxt->buf_size *= 2;

          program_trouble_fatal (_(
               "sorry, crude code not able to handle long lines"));
        }

      buf_end = last_newline;
      ctxt->residue = last_newline + 1;
    }
  else if (result == 0)
    {
      ctxt->eof = true;
      return false;
    }
  else
    {
      /* Partial read: Adjust buffer end, raise end-of-file flag to
         short-cut following reads, and hack a newline onto the byte
         following the last byte of the file.  */
      /* ?? The added-newline hack may need revisiting if the file
         does not end in a newline, and/or if the write causes too
         much OS copy-on-write overhead.  */
      buf_end = &ctxt->nominal_buf_start[result - 1];
      buf_end[1] = '\n';
    }

  /* Some algorithms backtrack until they find a preceding newline,
     so ensure that the byte preceding the buffer fulfils this
     requirement.  */
  buf_start[-1] = '\n';

  /* Write buffer start and end to caller.  */
  *p_buf_start = buf_start;
  *p_buf_end   = buf_end;
  return true;
}

extern void
linebuf_close (linebuf_context_t *ctxt)
{
  int result;

  CHECK_CONTEXT (ctxt);

  result = close (ctxt->fd);
  assert (result == 0);

  /* Destroy the context fences to reduce the chance we try to reuse
     the context by accident.  */
  ctxt->ctxt_top_fence_magic_nr    += 0x5a5a5a5auL;
  ctxt->ctxt_bottom_fence_magic_nr += 0x5a5a5a5auL;

  /* Free associated resources, then free the context itself.  */
  free (ctxt->malloc_ptr);
  free (ctxt);
}

/* vim:set shiftwidth=2: */
