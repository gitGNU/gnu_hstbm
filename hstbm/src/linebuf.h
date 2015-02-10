/* linebuf -- Present a line-based file in a cached memory buffer.

   Given a client that wants to deal with a multi-line file using
   linear memory buffers, this module tries to find the performance
   sweet spot between the operating system (OS), that typically maps
   the memory in medium-sized blocks (chunks of 4k or perhaps 8k),
   the CPU cache memory architecture, where overlapping reuse of a
   small buffer area leads to high cache hit rates and therefore low
   memory access latency and the application, which wants to deal
   with the file mainly at the line abstraction, wihthout having to
   cope with a buffer starting or ending part-way through a line.

   We achieve this by:
     (a) Choosing a modest buffer size, that is a small multiple of
         8192 bytes (e.g. 32768 bytes);
     (b) Having a memory block preceding the buffer proper that is
         the same size (32k).  This block serves three purposes:
         (b.1) If the buffer start and line start conincide, then a
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
               size.

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

#ifndef LINEBUF_H
#define LINEBUF_H 1

#include <stdbool.h>
#include <stddef.h> /* obtain size_t */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct linebuf_context_struct linebuf_context_t;

typedef unsigned char linebuf_byte_t;

extern linebuf_context_t *
linebuf_open (char *filename, size_t block_size, size_t buf_size);

extern bool
linebuf_read (linebuf_context_t *ctxt,
              linebuf_byte_t **p_buf_start,
              linebuf_byte_t **p_buf_end);

extern void
linebuf_close (linebuf_context_t *ctxt);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* LINEBUF_H */
/* vim:set shiftwidth=2: */
