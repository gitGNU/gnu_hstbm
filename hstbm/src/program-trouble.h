/* program-trouble -- Helper module to deal with serious trouble
                      in program operation.

   We want to use "success" and "failure" as high-level (abstract)
   indications of program outcomes; however, there are many
   lower-level mechanisms that can fail, and we would like to
   distinguish these failure cases from the basic two codes.

   (Success is typically an exit value if 0, defined in
   <stdlib.h>:EXIT_SUCCESS.  The simple/abstract failure code is
   exit value 1, defined in <stdlib.h>:EXIT_FAILURE.
   A program may be terminated as a result of a signal from another
   process, with the signal delevered via the kernel; typically the
   exit code uses higher values to signal this condition, perhaps
   greater than 127.)

   The convention adopted here, in common with a number of the GNU
   tools, is to use 2 as a "trouble" code, along with a concise
   explanation written to stderr.

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


#ifndef PROGRAM_TROUBLE_H
#define PROGRAM_TROUBLE_H 1

#include <stdbool.h>

/* ?? Support _GL_ATTRIBUTE_FORMAT(spec) so that GCC can check that
   format specifiers and varargs values match.

   See <error.h> in GNULib for more on this.  */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Formally define the trouble status code with a module prefix.
   Where any of the functions below terminate program operation by
   calling exit, they specify this code as the program status
   code.  */
#define PROGRAM_TROUBLE_STATUS_TROUBLE 2

extern void
program_trouble_set_program_name (char *name);

/* Interface function to let the client specify how to proceed when
   program_trouble is called.  If keep_going is true; the function
   call generates the report and writes it to stderr, raises an
   internal flag noting that touble was seen, but then returns
   control to the caller.  If keep_going is false, then the caller
   never receives control back; instead, after the error output is
   emitted, exit (PROGRAM_TROUBLE_STATUS_TROUBLE) is called.  */
extern void
program_trouble_set_keep_going_flag (bool keep_going);

/* Report a failure to stderr, using formatted output, and then
   either exit, or return, depend on the keep_going flag.  */
extern void
program_trouble (char *reason_format_str, ...);

/* Report a failure to stderr, using formatted output, and then
   exit, ignoring the keep_going flag setting.  */
extern void
program_trouble_fatal (char *reason_format_str, ...);

/* Report whether trouble was reported (but keep_going was set, so
   the call did not exit).  There is no way to clear this status
   flag, once set.  */
extern bool
program_trouble_trouble_was_encountered (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* PROGRAM_TROUBLE_H */
/* vim:set shiftwidth=2: */
