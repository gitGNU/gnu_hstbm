dnl
dnl autoconf input file for hstbm.
dnl
dnl Copyright (C) 2014-2015 Free Software Foundation, Inc.
dnl Copyright (C) 2015 Grouse Software
dnl
dnl This file is part of the Hybrid Self-Tuning Boyer-Moore experimental
dnl program, that is very heavily based on GNU Grep.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.


AC_INIT([hstbm Prototype], [0.1], [hstbm-bugs@grouse.com.au])
AC_CONFIG_SRCDIR([src/main.c])

AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_SRCDIR(src/main.c)
AC_DEFINE([h], 1, [We are building hstbm (./h)])
AC_PREREQ(2.59)
AC_CONFIG_MACRO_DIRS([m4])

dnl Automake stuff.
AM_INIT_AUTOMAKE([1.11 no-dist-gzip dist-xz color-tests parallel-tests
                  subdir-objects])
AM_SILENT_RULES([yes]) # make --enable-silent-rules the default.

AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_CANONICAL_HOST
AC_PROG_CC
gl_EARLY
AC_PROG_INSTALL
AC_PROG_RANLIB
dnl AC_PROG_MAKE_SET

dnl Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_C_CONST
gl_INIT

AC_ARG_ENABLE([gcc-warnings],
  [AS_HELP_STRING([--enable-gcc-warnings],
                  [turn on lots of GCC warnings (for developers)])],
  [case $enableval in
     yes|no) ;;
     *)      AC_MSG_ERROR([bad value $enableval for gcc-warnings option]) ;;
   esac
   gl_gcc_warnings=$enableval],
  [if test -d "$srcdir"/.git; then
     gl_gcc_warnings=yes
   else
     gl_gcc_warnings=no
   fi]
)

# By default, argmatch should fail calling usage (EXIT_FAILURE).
AC_DEFINE([ARGMATCH_DIE], [usage (EXIT_FAILURE)],
          [Define to the function xargmatch calls on failures.])
AC_DEFINE([ARGMATCH_DIE_DECL], [void usage (int _e)],
          [Define to the declaration of the xargmatch failure function.])

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_DIRENT

dnl Checks for functions.
AC_FUNC_CLOSEDIR_VOID

AC_CHECK_FUNCS_ONCE(isascii setlocale)

dnl I18N feature
AM_GNU_GETTEXT_VERSION([0.18.2])
AM_GNU_GETTEXT([external])

dnl Some installers want to be informed if we do not use our regex.
dnl For example, if the host platform uses dynamic linking and the installer
dnl knows that the grep may be invoked on other hosts with buggy libraries,
dnl then the installer should configure --with-included-regex.
AM_CONDITIONAL([USE_INCLUDED_REGEX], [test "$ac_use_included_regex" = yes])
if test "$ac_use_included_regex" = no; then
  AC_MSG_WARN([Included lib/regex.c not used])
fi

dnl gl_FUNC_PCRE


# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h limits.h locale.h stddef.h stdint.h \
                  stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memchr memmove memset regcomp setlocale strchr])

AC_CONFIG_FILES([
  Makefile
  gnulib/Makefile
  src/Makefile
  tests/Makefile
  po/Makefile
  doc/Makefile
])

AC_OUTPUT
