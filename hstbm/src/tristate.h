/* Tristate -- Provide a type that allows yes/no/dunno values.

   This type is useful in untangling some logic cases.  The main
   first use case is in dealing with command-line options:
     1. The option is set to "dunno" as its default, before
        commencing option processing;
     2. The user may, or may not, explicitly set the state to
        "yes" or "no" via option(s);
     3. After finishing option processing, if the value is "yes"
        or "no", obey the caller's wishes; or else
     4. "Dunno" at this point signals that the program should use
        secondary criteria to determine the option setting.  */

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

/* This code is, to some extent, based on the code in GNU Grep.

   This file was developed in 2014-2015 by behoffski of Grouse
   Software.  */

#ifndef TRISTATE_H
#define TRISTATE_H 1

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef enum tristate_enum
  {
    tristate_no    = 0,
    tristate_yes   = 1,
    tristate_dunno = 2
  } tristate_t;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* TRISTATE_H */
/* vim:set shifwidth=2: */
