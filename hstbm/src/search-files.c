/* search-files -- Perform specified search across one or more files.

   Given:
     - A runtime-ready pattern matcher (hstbm), that has incorporated
       an explicit pattern, pattern interpretation options such as
       case-insensitivity, and environment selections such as locale;
     - A list of one or more files to search, collated and/or
       generated by the calling framework; and
     - Search and output options (e.g. line count, filename output,
       stop upon first match) selections:

   Perform the specified search on the files in order, and deal with
   matches according to the options (e.g. stop on first match if
   that's sufficent to satisfy the caller's request).

   Note that this file is much, much less capable than the full
   sophisticated search facilities in GNU Grep, and this may be the
   reason for some performance differences.  See the introduction in
   this module's header file for a more detailed discussion.  */

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

#include <assert.h>
#include "linebuf.h"
#include "misc-gettext.h"
#include "program-trouble.h"
#include "search-files.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tristate.h"
#include <unistd.h>

#define BLOCK_SIZE_MINIMUM      4096
#define BUFFER_SIZE_INITIAL     (BLOCK_SIZE_MINIMUM * 8)

static int
search_file (hstbm_context_t *hstbm,
             linebuf_context_t *lbuf,
             search_files_options_t *file_opt)
{
  size_t count = 0;
  size_t byte_offset = 0;

  for (;;)
  {
    linebuf_byte_t *buf_start;
    linebuf_byte_t *buf_end;
    ptrdiff_t skip;

    /* Get the next part of the file, suitably buffered at line
       boundaries, in a memory buffer.  If this file is spent (no
       more data found), break out of the buffer loop.  Otherwise,
       report the buffer to the search */
    if (! linebuf_read (lbuf, &buf_start, &buf_end))
      break;
    hstbm_buffer (hstbm, buf_start, buf_end);
    skip = 0;

    /* Find matching lines in the current buffer, until no more
       matches are found.  Parameter skip tells the search engine
       how many bytes to skip forward from the start of each
       reported match (typically, to the first byte of the next
       line in the buffer).  */
    for (;;)
    {
      const hstbm_byte_t *s;
      hstbm_byte_t *s2;

      /* Search for the next occurrence of the pattern in the
         buffer.  If the result is NULL, no match found in this
         buffer, so we break out of the inner buffer loop, so the
         outer loop can acquire the next buffer, if any.  */
      s = hstbm_search (hstbm, skip);
      if (! s)
      {
        break;
      }

      /* Memchr here assumes pattern doesn't have \n... sigh.  */
      s2 = memchr (s, '\n', buf_end - s + 1);

      /* Did we not find a terminating NL for this match?  */
      if (s2 == NULL)
        {
          /* Yes, this only happens when we match text at the end of
             the file, no LF is found after the match, and the file
             buffering hasn't appended a LF elsewhere.  We deal with
             this here by adding a LF now (we know we have space to
             do this; the write may cause a performance penalty via a
             copy-on-write page fault).
             The purist in me would prefer not to add data (the LF)
             to the end of the file, but trying to act identically to
             grep takes precedence.  As an extra editorial note, this
             code currently doesn't handle "before-context" and
             "after-context" line display; the complexity of these
             cases may argue for the newline to be added by the buffer
             management, rather than needing to cater for it here.  */
          s2 = buf_end;
          s2[1] = '\n';
        }

      skip = s2 - s + 1;

      /* Count matches, regardless of whether we've been asked to,
         as we rely on the count when reporting overall file match
         success/failure.  */
      count++;

      if (file_opt->show_lines == tristate_yes)
        {
          const hstbm_byte_t *line_start;
          int result;

          /* Find the newline preceding the line.  */
          line_start = memrchr (buf_start - 1, '\n', s - buf_start + 1);

          /* Write out filename prefix, if requested.  */
          if (file_opt->show_filename == tristate_yes)
            printf ("%s%c",
                    file_opt->filename,
                    file_opt->filename_terminator);

          /* Write out byte offset prefix, if requested.  */
          if (file_opt->show_byte_offset)
            printf ("%lu:",
                    byte_offset + (line_start - buf_start + 1));

          /* Write out the matched line.  */
          result = fwrite (line_start + 1, 1, s2 - line_start, stdout);
          assert (result > 0);
        }
    }

    /* Update byte offset to move past the buffer we've processed.  */
    byte_offset += buf_end - buf_start + 1;
  }

  if (file_opt->count_lines)
    {
      if (file_opt->show_filename == tristate_yes)
        printf ("%s%c%lu\n",
                file_opt->filename,
                file_opt->filename_terminator,
                count);
      else
        printf ("%lu\n", count);
    }

  return count ? EXIT_SUCCESS
               : EXIT_FAILURE;
}

int
search_files_exec (hstbm_context_t *hstbm,
                   search_files_options_t *file_opt,
                   int files_argc,
                   char *files_argv[])
{
  linebuf_context_t *lbuf;
  int overall_status;

  overall_status = EXIT_FAILURE;

#if DEBUG
  /* ?? Instead of BLOCK_SIZE_MINIMUM, get minimum block size from
     the system.  sysconf conforms to POSIX.1-2001.  */
  {long sz = sysconf (_SC_PAGESIZE);
    printf (_("System page size: %ld\n"), sz);}
#endif /* DEBUG */

  while (files_argc--)
    {
      char *filename;
      int file_status;

      filename = *files_argv++;

      /* Loop through specified file(s).  */
      file_opt->filename = filename;
      if (strcmp (filename, "-") == 0)
        {
          /* Explicit reference to standard input on command line.  */
          file_opt->filename = _("(standard input)");
          lbuf = linebuf_open (NULL,
                               BLOCK_SIZE_MINIMUM,
                               BUFFER_SIZE_INITIAL);
        }
      else
        lbuf = linebuf_open (filename,
                             BLOCK_SIZE_MINIMUM,
                             BUFFER_SIZE_INITIAL);

      if (lbuf)
        {
          file_status = search_file (hstbm, lbuf, file_opt);
          if (file_status == EXIT_SUCCESS)
            overall_status = file_status;
          linebuf_close (lbuf);
        }
    }

  return overall_status;
}

/* vim:set shifwidth=2: */
