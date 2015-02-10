# Makefile.am for hstbm/src.
#
# Copyright 1996, 1997, 2001, 2005, 2006, 2007, 2008, 2015 Free
# Software Foundation, Inc.
# Copyright (C) 2015 Grouse Software
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

LN = ln

AM_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS)

# Tell the linker to omit references to unused shared libraries.
AM_LDFLAGS = $(IGNORE_UNUSED_LIBRARIES_CFLAGS)

bin_PROGRAMS = h
h_SOURCES = charclass.c compiled-pattern.c \
            hstbm.c \
            lexparse.c linebuf.c \
	    main.c \
	    program-trouble.c \
	    search-files.c
noinst_HEADERS = charclass.h compiled-pattern.h \
            hstbm.h \
            lexparse.h linebuf.h \
            pattern-lex.h program-trouble.h \
            search-files.h \
            tristate.h

# Linker libraries requited by libtool modules (141014):
#         memchr2 memrchr regex xalloc
h_LDADD = $(LIBINTL) $(LIBTHREAD) $(LIB_PCRE) $(top_builddir)/gnulib/libgnu.a

localedir = $(datadir)/locale

AM_CPPFLAGS = -I$(top_srcdir)/gnulib -I$(top_builddir)/gnulib \
       -O2 -std=c90 -pedantic-errors -Wall -Werror

DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

# Use a script to run "git depend" and output the result as a header
# file that defines GIT_CURRENT_VERSION for use by main.c's version
# output.  If Git isn't installed, the script leaves the header file
# untouched.
.PHONY: always_try_to_update_git_current_version
always_try_to_update_git_current_version:
	@echo "  Update .git_current_version.h"
	@./.update_git_current_version

main.o: always_try_to_update_git_current_version
