/* main -- Parse command-line arguments and oversee search operation.

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

#include ".git_current_version.h"

#include <assert.h>
#include <ctype.h>
#include "charclass.h"
#include "compiled-pattern.h"
#include "hstbm.h"
#include "getopt.h"
#include "lexparse.h"
#include <locale.h>
#include "misc-gettext.h"
#include "pattern-lex.h"
#include "program-trouble.h"
#include "search-files.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tristate.h"
#include <unistd.h>

/* The initial number of character classes created by the charclass
   module.  If token parsing is used, then there is always one
   class per position, i.e. equivalent to the pattern length.  In
   addition, there are a couple of classes auto-created by the
   module itself, so allow some extra overhead.  Choose an initial
   size that covers the majority of small patterns, but scales up
   fairly quickly to be larger than HSTBM_PATTERN_LEN_MAX.  */
#define INITIAL_CHARCLASS_POOL_SIZE     36

#if 0
/* This code is archaic, but it's very valuable in showing one of the
   intermediate steps in the class handling/overlap checking finally
   implemented.  This "list of folding sets" was implemented in a
   module called (fairly obviously) fold-sets.[ch]; this
   representation was only reluctantly (and painfully) superceded by
   charclass when the delta 3/4 shift tests were identified; the
   charclass representation is much easier for checking about class
   equality/overlap possibilities.  */
static char *case_fold_C_POSIX[] =
  {
    "Aa", "Bb", "Cc", "Dd", "Ee", "Ff", "Gg", "Hh", "Ii",
    "Jj", "Kk", "Ll", "Mm", "Nn", "Oo", "Pp", "Qq", "Rr",
    "Ss", "Tt", "Uu", "Vv", "Ww", "Xx", "Yy", "Zz",
    /* Should be accepted if a class, e.g. "[5-8]": "5678", */
    /* Should be rejected (overlaps Ss and also Tt): "sT", */
    NULL
  };
#endif /* 0 */

static char *progname;

/* Briefly summarise how the program should be invoked, writing to
   stderr and exiting with a non-success status.  */
static void
short_usage (int status)
{
  fprintf (stderr, _("\
Usage: %s [OPTION]... PATTERN [FILE]...\n\
Try '%s --help' for more information.\n\
"), progname, progname);
  exit (status);
}

static void
usage (int status)
{
  if (status != EXIT_SUCCESS)
    short_usage (status);

  printf (_("Usage: %s [OPTION]... PATTERN [FILE]...\n"), progname);
  printf (_("Search for PATTERN in each FILE, and/or in standard input.\n"
            "\n"));
  printf (_(
"PATTERN is limited to a simple fixed string, apart from allowing some\n"
"limited character classes and/or case-insensitive matches; in most\n"
"cases, there must be no overlap between classes.  The pattern must\n"
"not be too short (less than %d chars); nor must it be too long (more\n"
"than %d chars).\n"),
           HSTBM_PATTERN_LEN_MIN, HSTBM_PATTERN_LEN_MAX);
    printf (_(
"Example: %s '123[abc]456' metadata intercepts.txt stockprices.xml\n"
              "\n"), progname);
    printf (_("Pattern selection and interpretation:\n"
"  -e, --regexp=PATTERN  Use PATTERN for matching\n"
"  -i, --ignore-case     Ignore case distinctions\n"
              "\n"));

    printf (_("Miscellaneous:\n"
"  -V, --version        Display version information and exit\n"
"      --help           Display this help text and exit\n"
"  -J, --show-internals Write information about the internal data\n"
"                       structures used by the program, possibly\n"
"                       including the tokenised form of the pattern,\n"
"                       to stderr (for use in debugging).\n"));
    printf (_(
"  -K, --efficiency-threshold=VALUE\n"
"                       Specify how many bytes the skip (STBM) search\n"
"                       must eliminate each iteration in order to be\n"
"                       considered efficient; if inefficient, switch to\n"
"                       the hybrid (memchr/memchr2) search algorithms to\n"
"                       try and make more progress.  The value 0 disables\n"
"                       the hybrid alternative altogether.\n"
              "\n"));

    printf (_("Output control:\n"
"  -b, --byte-offset    Print input file's 0-based byte offset of each\n"
"                       output line\n"
"  -c, --count          Print only a count of matching lines per FILE\n"
"  -H, --with-filename  Print the filename for each match\n"
"  -h, --no-filename    Suppress the filename prefix on output\n"
"  -Z, --null           Print NUL (0 byte) after FILE name\n"
              "\n"));

    printf (_(
"When FILE is -, or if no FILE is given, read standard input (there\n"
"is no -r option).  If -h and/or -H are specified, the last such\n"
"option controls filename output; otherwise, assume -H if multiple\n"
"files are specified to be searched, and -h if only one file is to\n"
"be searched.\n"
              "\n"));
    printf (_("Exit status is:\n"
"  >1 if any unexpected error occurs (note: we do not support -q);\n"
"        and/or\n"
"  2  if we identified trouble (e.g. invalid pattern syntax);\n"
"  1  if we searched everything but no matches were found; or\n"
"  0  if we found at least one match.\n"
              ));

  /* Caller explicitly asked for help, so report success.  */
  exit (EXIT_SUCCESS);
}

/* NOTE: Macro GIT_CURRENT_VERSION, used here, is defined in
   .git_current_version.h, and should always reflect the latest
   status of the program source tree.

   The makefile always queries Git and regenerates
   .git_current_version.h, using a short custom script.  Therefore,
   main.c will always be rebuilt on every build pass, regardless of
   whether it's needed or not.  */
static void
show_version (int status)
{
  printf (_("\
hstbm (Hybrid Self-Tuning Boyer-Moore) %s\n\
\n\
Copyright (C) 2015 Free Software Foundation, Inc.\n\
Copyright (C) 2015 Grouse Software\n\
License GPLv3+: GNU GPL version 3 or later\n\
                <http://gnu.org/licenses/gpl.html>.\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n\
"), GIT_CURRENT_VERSION);
  printf (_("\n\
Significant portions of this program are based on GNU Grep,\n\
written by Mike Haertel and others, see:\n\
         <http://git.sv.gnu.org/cgit/grep.git/tree/AUTHORS>\n\
\n\
This program originally created by behoffski (Brenton Hoff) of\n\
Grouse Software:\n\
        <mailto:behoffski@grouse.com.au>\n\
"));
  exit (status);
}

/* Variables that can be referenced directly by getopt.  */
static int show_help;
static int show_internals;
static int efficiency_threshold;

/* Getopt specifier -- Short options.  */
static char const short_options[] = "bce:HhiJK:VZ";

/* Getopt specifier -- Long equivalences.  */
static struct option const long_options[] =
{
  {"byte-offset",    no_argument,       NULL, 'b'},
  {"count",          no_argument,       NULL, 'c'},
  {"help",           no_argument,       &show_help, 1},
  {"ignore-case",    no_argument,       NULL, 'i'},
  {"efficiency-threshold",
                     required_argument, NULL, 'K'},
  {"no-filename",    no_argument,       NULL, 'h'},
  {"null",           no_argument,       NULL, 'Z'},
  {"regexp",         required_argument, NULL, 'e'},
  {"show-internals", no_argument,       NULL, 'J'},
  {"version",        no_argument,       NULL, 'V'},
  {"with-filename",  no_argument,       NULL, 'H'},
  {0, 0, 0, 0}
};

/* If no arguments are given, the program should search stdin.
   Supplying "-" as a fake argument simplifies the file-handling
   loop below (eliminates a few lines of duplicated code).  */
static char *fake_stdin_argv[] = {"-", NULL};

/* Folding sets, initially scoped to be inside main; now moved to be
   external so map_ref_to_set can see them to help generate token list
   diagnostic output.  */
static charclass_t **matcher_class_list;

int
main (int argc, char *argv[])
{
  hstbm_context_t *hstbm;
  char *pattern;
  int pattern_len;
  search_files_options_t files_opt;
  lexparse_lex_options_t lex_opt;
  lexparse_match_options_t match_opt;
  compiled_pattern_t *compiled_pattern;
  int status;
  int file_argc;
  char **file_argv;

  /* Initialise other modules.  */
  charclass_initialise (INITIAL_CHARCLASS_POOL_SIZE);
  compiled_pattern_initialise ();
  hstbm_initialise ();

  /* Mindlessly use the first argument as the program name.  */
  program_trouble_set_program_name (argv[0]);

  /* Initialise variables that don't fit into other categories.  */
  status = PROGRAM_TROUBLE_STATUS_TROUBLE;

  /* Set up locale.  */
  setlocale (LC_ALL, "");
  if (MB_CUR_MAX > 1)
    {
      program_trouble_fatal (_(
"sorry, locale '%s' has multibyte and/or variable-length\n"
"characters, which this program does not support."),
               setlocale (LC_ALL, NULL));
    }

  /* Set up default options.  */
  memset (&files_opt, 0, sizeof (files_opt));
  memset (&lex_opt, 0, sizeof (lex_opt));
  memset (&match_opt, 0, sizeof (match_opt));
  files_opt.show_byte_offset = false;
  files_opt.count_lines = false;
  files_opt.show_lines = true;
  files_opt.show_filename = tristate_dunno;
  files_opt.filename_terminator = ':';
  lex_opt.character_classes = true;
  match_opt.case_insensitive = false;
  match_opt.nr_endline_bytes = 1;
  match_opt.endline_bytes[0] = '\n';

  /* Set up default actions and/or argument parameters.  */
  show_help = 0;
  show_internals = 0;
  pattern = NULL;

  /* The efficiency threshold is a wild, wild ride... probably need
     to factor in pattern length and number of steps in the "big"
     skip loop of hstbm in order to get a good guess... but this is
     really, really playing with fire, and is possibly prone to very
     bad performance, given pathalogical input data.  */
  efficiency_threshold = 10 * sizeof (long);

  /* Decode program options (and prepare to deal with remaining
     arguments, if any) with the help of getopt (3).  */
  progname = argv[0];
  for (;;)
    {
      int opt;
      int longindex;

      /* Get the next option on the command line, if any.  */
      opt = getopt_long (argc, argv,
                         short_options,
                         long_options, &longindex);
      if (opt == -1)
        break;

      /* Configure program behaviour based on specified optiion.  */
      switch (opt)
        {
        case 'b': files_opt.show_byte_offset = true;      break;
        case 'c': files_opt.count_lines = true;           break;
        case 'e': pattern = argv[longindex];              break;
        case 'H': files_opt.show_filename = tristate_yes; break;
        case 'h': files_opt.show_filename = tristate_no;  break;
        case 'i': match_opt.case_insensitive = true;      break;
        case 'J': show_internals = true;                  break;
        case 'K':
          /* Parsing the number here is rudimentary, sigh.  */
          sscanf (optarg, "%d", &efficiency_threshold);
          break;
        case 'V': show_version (PROGRAM_TROUBLE_STATUS_TROUBLE); break;
        case 'Z': files_opt.filename_terminator = '\0';   break;
        case 0:   /* long options */                      break;
        default:
          short_usage (PROGRAM_TROUBLE_STATUS_TROUBLE);   break;
        }
    }

  /* Long option --help has no short equivalent, so we check for it
     via a shared variable.  */
  if (show_help)
    usage (EXIT_SUCCESS);

  /* Move post-option-processing argc and argv into private variables
     as we try not to disrupt the original arguments given to us
     (despite knowing that getopt may have rearranged things
     already).  */
  file_argc = argc - optind;
  file_argv = &argv[optind];

  if (pattern == NULL)
    {
      if (file_argc == 0)
        {
          fprintf (stderr, _("did not find a pattern argument\n\n"));
          usage (PROGRAM_TROUBLE_STATUS_TROUBLE);
        }
      file_argc--;
      pattern = *file_argv++;
    }

  /* Fail if the pattern contains an end-of-line character.  This
     test is a little heavy-handed (e.g. [^\n] might be acceptable),
     but it's intended to keep things simple for initial use.  */
  {
    int i;

    for (i = 0; i < match_opt.nr_endline_bytes; i++)
      if (strchr (pattern, match_opt.endline_bytes[i]))
        program_trouble_fatal (_("Search pattern is not allowed to"
                                 " contain an end-of-line character"));
  }

  /* Pattern text length is useful in many places.  */
  pattern_len = strlen (pattern);

  /* If we are counting lines, don't show them as well.  This
     includes overriding line-based displays such as byte offset.  */
  if (files_opt.count_lines)
    files_opt.show_lines = false;

  /* Default to simple, plain-string pattern interpretation, and
     revert to a compiled (tokenised) pattern if the plain string
     cannot represent handle the search request.  We make a special
     effort to support the plain-string interface, even though we
     could achieve the same effect by always compiling, as this may
     be beneficial to someone reusing the code.  */
  compiled_pattern = NULL;

  matcher_class_list = NULL;
  if ((strchr (pattern, '[') != NULL)
      || (strchr (pattern, '\\') != NULL)
      || match_opt.case_insensitive)
    {
      /* Allocate space for a compiled pattern.  */
      compiled_pattern = compiled_pattern_alloc (pattern_len);
      if (! lexparse_tokenise (pattern_len, pattern,
                               &lex_opt, &match_opt,
                               compiled_pattern))
        exit (PROGRAM_TROUBLE_STATUS_TROUBLE);

      if (show_internals)
        fprintf (stderr, "* show_internals:  Compiled pattern:  %s",
                 compiled_pattern_describe (compiled_pattern));

      /* If pattern_len does not match nr_tokens, then the pattern
         isn't compatible with hstbm.  The locale might be a reason;
         or maybe the pattern has variable-length components.  */
      if (compiled_pattern->nr_tokens
          != compiled_pattern->pattern_len)
        program_trouble_fatal (_("Pattern length is too complex"));
    }
  else if (show_internals)
    fprintf (stderr,
             _("* show_internals:  Plain pattern, not tokenised"));

  hstbm = hstbm_new ();
  if (hstbm == NULL)
    {
      program_trouble_fatal (_("Unable to create hstbm context"));
    }

  hstbm_set_efficiency_threshold (hstbm, efficiency_threshold);

  if (compiled_pattern == NULL)
    {
      /* ?? Perhaps pattern should be (char *), not (hstbm_byte_t *).  */
      if (! hstbm_pattern (hstbm,
                           pattern_len,
                           (hstbm_byte_t *) pattern))
        program_trouble_fatal (_("hstbm_pattern failed?!"));
    }
  else
    {
      /* Convert any/each/all inverted classes into the equivalent
         plain class, rewriting the opcode in-place in the token
         stream, and almost certainly changing the referenced class
         also.  We do this here, not during lexing, in order to
         defer breaking down abstractions until as late as possible,
         and, in the same vein, we would like to defer and/or
         minimise revealing details of abstractions such as actual
         end-of-line encodings unless required.  */
      lexparse_normalise_inverted_classes (&match_opt,
                                           compiled_pattern);

#if 0
      if (show_internals)
        fprintf (stderr,"* show_internals:  Compiled pattern "
                        "(after normalisation):  %s",
                 compiled_pattern_describe (compiled_pattern));
#endif

      /* Check that all the classes present in the matcher portion
         of the pattern have a simple relationship with each other,
         such that we can prepare a single 1-to-1 folding table to
         cover the entire matcher portion of the pattern.  */
      if (! hstbm_pattern_supports_simple_folding (compiled_pattern))
            program_trouble_fatal
              (_("sorry, there are incompatible sets of "
                 "character classes; hstbm is limited in what "
                 "it can handle"));

      /* ?? SUDDEN EDITORIAL OUTBURST:  We don't want to support
         more complex folding scenarios as the number of tables
         grows very rapidly, the density of memory accesses per
         table may plummet, and we might end up losing more in terms
         of memory coherence (slower memory performance due to a
         higher cache miss rate) than we gain in having fine-grained
         folding support; in such cases, we may be better off
         choosing a different algorithm, not this variant of the
         Boyer-Moore family...

         ...?? RESEARCHME:  It may be feasible to find a "small"
         number of folding tables, each shared across possibly
         multiple pattern positions, that gives flexibility without
         sacrificing memory density.  One impact of this would be
         that the folding of the "guard" character would reference
         a dynamic table, instead of a static one... perhaps split
         hstbm matching into plain/static-fold/dynamic-fold
         loop variants?  */

      /* Okay, present the pattern as tokens, plus the associated
         classes for each pattern position, for hstbm to analyse
         and use to prepare for matching.  */
      if (! hstbm_pattern_compiled (hstbm,
                                    compiled_pattern))
        program_trouble_fatal (_("hstbm_pattern_compiled failed?!"));
    }

  /* If given multiple filenames to search, and we haven't received
     a specific option specifying what to do, then enable prefixing
     each matching line with the filename.  */
  if ((file_argc > 1) && (files_opt.show_filename == tristate_dunno))
    files_opt.show_filename = tristate_yes;

  /* ?? Some limitations/mediocre file handling in this demo rig
     that should be addressed in a real program:
     (1, 2: Now partially fixed...)
     3. No attempt to cater for files that are dynamically growing
        while being searched;
     4. No binary/text handling; on a related note, no support for
        editing MS-DOS files to convert CR/LF line terminators to
        LF only.
     5. No detection of file type (e.g. naming a directory having
        or not having special meaning; on a related note, no
        recursion);
     6. No checking that the output file is the input file, which,
        if not detected, could, lead to matching until external OS
        limits, user resource limits and/or storage limits are
        exhausted;
     ...and this is not meant to be an exhaustive list... */

  if (file_argc == 0)
    {
      /* If no files given to search, pretend that the user typed one
         filename, "-", as we know that that will be interpreted as
         standard input, below.  This saves a small amount of code
         duplication.  */
      file_argc = 1;
      file_argv = fake_stdin_argv;
    }

  if (show_internals)
    {
      hstbm_debug_show_context (hstbm);
    }

  /* Tell program_trouble to keep going if non-fatal trouble is
     encountered.  */
  program_trouble_set_keep_going_flag (true);

  /* Execute the search on the specified file(s).  */
  status = search_files_exec (hstbm,
                              &files_opt,
                              file_argc, file_argv);

  /* If trouble was detected earlier but was not a show-stopper,
     report it now.  */
  if (program_trouble_trouble_was_encountered ())
    return (PROGRAM_TROUBLE_STATUS_TROUBLE);

  return status;
}

/* vim:set shifwidth=2: */
