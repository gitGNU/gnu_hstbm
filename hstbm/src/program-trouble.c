/* program-trouble -- Helper module to deal with serious trouble
                      in program operation.

   We want to use "success" and "failure" as high-level (abstract)
   indications of program outcomes; however, there are many
   lower-level mechanisms that can fail, and we would like to
   distinguish these failure cases from the basic two codes.

   The convention adopted here, in common with a number of the GNU
   tools, is to use 2 as a "trouble" code, along with a concise
   explanation written to stderr.

   (Success is typically an exit value if 0, defined in
   <stdlib.h>:EXIT_SUCCESS.  The simple/abstract failure code is
   exit value 1, defined in <stdlib.h>:EXIT_FAILURE.
   A program may be terminated as a result of a signal from another
   process, with the signal delevered via the kernel; typically the
   exit code uses higher values to signal this condition, perhaps
   greater than 127.)

   This module provides a compact facility to handle the "trouble"
   case, including message formatting facilities.

   The client can choose whether the program should exit (this is
   the default) or should keep going if trouble is detected, and can
   change this setting during different phases of operation.  If it
   does keep going, the client can find out if any trouble was
   encountered.

   If exit is initiated by this module, the "trouble" status code is
   named as program exit status.  */

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
   Grep.  Many people have contributed to the sources.  See the
   files AUTHORS and THANKS, as well as the git changelog, in the
   GNU Grep package for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

#include <config.h>

#include <assert.h>
#include <limits.h>
#include "misc-gettext.h"
#include "program-trouble.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TROUBLE_MESSAGE_BUF_SIZE 3072

/* Use a statically-allocated buffer to compose a trouble message,
   so that the buffer is available even if dynamic memory has been
   exhausted or corrupted in some fashion.  */
static char trouble_message_buf[TROUBLE_MESSAGE_BUF_SIZE];

/* Pointer to remainder of message buffer after program name has
   been supplied, or NULL if none given.  */
static char *p_message_buf = NULL;

/* Flags for keep_going behaviour, and to record whether trouble was
   encountered but the client selected keep_going behaviour.  */
bool keep_going = false;
bool trouble_was_encountered = false;

static void
flush_output_show_trouble_message (char *reason_format_str,
                                   va_list ap)
{
  int message_size;
  ptrdiff_t buf_available;

  /* Release any pending normal output, before emitting this error
     message.  */
  fflush (stdout);

  /* If no program name was given, set up a default name now.  */
  if (p_message_buf == NULL)
    {
      p_message_buf = trouble_message_buf;
      p_message_buf += sprintf (p_message_buf,
                                "%s",
                                _("(unknown program name): "));
    }

  /* Work out how much buffer space we have to work with.  */
  buf_available = &trouble_message_buf[sizeof (trouble_message_buf)]
    - p_message_buf;

  /* Compose a formatted failure message, and write it to stderr.  */
  message_size = vsnprintf (p_message_buf,
                            buf_available,
                            reason_format_str, ap);

  /* If message didn't fit, brutally truncate buffer.  */
  if (message_size >= buf_available)
    message_size = buf_available - 1;

  /* Add a trailing newline to the formatted output buffer, and
     write the output, including the program name (if any) to
     stderr.  (We don't care about the NUL terminator since we
     never treat the buffer as a string.)  */
  p_message_buf[message_size++] = '\n';
  fwrite (trouble_message_buf,
          1, message_size + (p_message_buf - trouble_message_buf),
          stderr);
  fflush (stderr);
}

/******************************************************************/

void
program_trouble_set_program_name (char *name)
{
  int written;

  assert (name);

  /* Copy name into the start of the message buffer.  Fail with
     an assertion if the name is unreasonably large (defined as
     taking up over half of the entire message buffer).  */
  written = snprintf (trouble_message_buf,
                      sizeof (trouble_message_buf),
                      "%s: ", name);
  if (written > (sizeof (trouble_message_buf) / 2))
    assert (!_(
        "program_trouble: Program name is unreasonably large"));

  /* Although an error return code is believed to be extremely
     unlikely, try to inform the client anyway, and then abort.  */
  if (written < 0)
    {
      char *msg;

      (void) fflush (NULL);
      msg = "Error receiving program name: ";
      (void) fwrite (msg, 1, strlen (msg), stderr);
      msg = strerror (written);
      (void) fwrite (msg, 1, strlen (msg), stderr);
      msg = "\n";
      (void) fwrite (msg, 1, strlen (msg), stderr);
      (void) fflush (stderr);
      assert (!_("program_trouble: Program name error"));
      /* NOTREACHED */
    }

  p_message_buf = &trouble_message_buf[written];
}

/* Interface function to let the client specify how to proceed when
   program_trouble is called.  If keep_going is true; the function
   call generates the report and writes it to stderr, raises an
   internal flag noting that touble was seen, but then returns
   control to the caller.  If keep_going is false, then the caller
   never receives control back; instead, after the error output is
   emitted, exit (PROGRAM_TROUBLE_STATUS_TROUBLE) is called.  */
void
program_trouble_set_keep_going_flag (bool keep_going_parameter)
{
  keep_going = keep_going_parameter;
}

/* Report a failure to stderr, using formatted output, and then
   either exit, or return, depend on the keep_going flag.  */
/* Apologies to the pedanticists out there that demand that the
   module name (program_trouble) be followed by a function/method
   name:  Error/exception handling can be awkward to read, and the
   omission of a function name here is in the hope that the client's
   code can then be read more linearly.  */
void
program_trouble (char *reason_format_str, ...)
{
  va_list ap;

  /* Generate the "trouble" message and write to stderr.  */
  va_start (ap, reason_format_str);
  flush_output_show_trouble_message (reason_format_str, ap);
  va_end (ap);

  if (keep_going)
    {
      trouble_was_encountered = true;
      return;
    }

  /* Exit with TROUBLE sentinel value.  */
  exit (PROGRAM_TROUBLE_STATUS_TROUBLE);

  /* Ensure external tools know that we never return.  */
  /* NOTREACHED */
  abort ();
}

/* Report a failure to stderr, using formatted output, and then
   exit, ignoring the keep_going flag setting.  */
void
program_trouble_fatal (char *reason_format_str, ...)
{
  va_list ap;

  /* Generate the "trouble" message and write to stderr.  */
  va_start (ap, reason_format_str);
  flush_output_show_trouble_message (reason_format_str, ap);
  va_end (ap);

  /* Exit with TROUBLE sentinel value.  */
  exit (PROGRAM_TROUBLE_STATUS_TROUBLE);

  /* Ensure external tools know that we never return.  */
  /* NOTREACHED */
  abort ();
}

/* Report whether trouble was reported (but keep_going was set, so
   the call did not exit).  There is no way to clear this status
   flag, once set.  */
bool
program_trouble_trouble_was_encountered (void)
{
  return trouble_was_encountered;
}

/* vim:set shiftwidth=2: */
