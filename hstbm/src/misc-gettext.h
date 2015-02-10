/* misc-gettext.h -- Shim header for future gettext support.

  The HSTBM demonstration program does not support
  internationalisation to any great extent.  However, I wanted to
  write the code in an infrastructure-aware fashion, so that life
  might be easier if others wanted to improve the code.  So, this
  module provides the simple "_" macro for  wrapping strings that
  are candidates for internationalisation.  */

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
   Grep.  Many, many authors have contributed to the sources.  See
   the files AUTHORS and THANKS, as well as the git changelog, in
   the GNU Grep package for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

#ifndef MISC_GETTEXT_H
#define MISC_GETTEXT_H 1

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Use _(...) wrappers for constant strings as a shorthand for
   marking messages that may have different translations provided
   by gettext.  */

/* Initially, we don't support gettext locale translations, but this
   infrastructure paves the way for future support.  */
#undef _
#define _(String) String

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MISC_GETTEXT_H */
/* vim:set shiftwidth=2: */
