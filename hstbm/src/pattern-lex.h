/* pattern-lex -- Define tokens to describe regexp search patterns.
 */

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
   Grep, especially the infrastructure code in dfa.c, which has the
   attribution:

       Written June, 1988 by Mike Haertel
       Modified July, 1988 by Arthur David Olson to assist BMG speedups

   Many others have contributed to the sources.  See the files AUTHORS
   and THANKS, as well as the git changelog, in the GNU Grep package
   for details.

   This variant was created in 2014-2015 by behoffski (Brenton Hoff)
   of Grouse Software.  */

/* Make header file idempotent.  */
#ifndef PATTERN_LEX_H
#define PATTERN_LEX_H 1

#include <config.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define PATTERN_LEX_PLOTPE_ILLEGAL              0xdfebfa5uL

/* A token consists of a longword, which we assume is at least 32
   bits wide.  The top 4 (?? perhaps more later) bits define the
   token opcode; the remaining (28) bits may be unused (in which
   case, must be set to 0), or may carry a parameter, whose meaning
   depends on the opcode.  */

/* Define field sizes, shift values and bitmasks.  */
/* ?? Note that ILLEGAL token below assumes 28-bit parameter field.  */
#define PATTERN_LEX_OPCODE_SIZE_BITS  4
#define PATTERN_LEX_PARAM_SIZE_BITS  28

#define PATTERN_LEX_OPCODE_SHIFT PATTERN_LEX_PARAM_SIZE_BITS
#define PATTERN_LEX_OPCODE_UNSHIFTED_MASK \
        ((1uL << PATTERN_LEX_OPCODE_SIZE_BITS) - 1)
#define PATTERN_LEX_OPCODE_MASK \
        (~0uL << PATTERN_LEX_OPCODE_SHIFT)
#define PATTERN_LEX_PARAM_SHIFT 0
#define PATTERN_LEX_PARAM_MASK \
        ((1uL << PATTERN_LEX_PARAM_SIZE_BITS) - 1)
/* We don't distinguish between shifted/unshifted param, as the
   bit offset is 0.  The purist in me is offended by this, but the
   pragmatist is too tired to care just now, sigh.  */

/* Typedef for a word that describes, in full or in part, a pattern
   matcher lexical token as an opcode/parameter pair, with the
   parameter interpreted according to the opcode:  Sometimes unused,
   sometimes simply being a value in its own right, and other times
   being a reference (index?) to a much larger structure.  */
typedef int pattern_lex_token_t;
typedef unsigned int pattern_lex_param_t;

/* Macro to generate token from (opcode, param) pair.  */
#define PATTERN_LEX_TOKEN(opcode, param) \
  ((pattern_lex_token_t)                 \
        ((((unsigned long) (opcode)) << PATTERN_LEX_OPCODE_SHIFT) \
         | (((unsigned long) (param)) & PATTERN_LEX_PARAM_MASK)) \
   )

/* Macros to extact opcode and parameter fields from token.  */
#define PATTERN_LEX_GET_OPCODE(token) \
  ((pattern_lex_opcode_t) (((token) >> PATTERN_LEX_OPCODE_SHIFT)  \
                           & PATTERN_LEX_OPCODE_UNSHIFTED_MASK))
#define PATTERN_LEX_GET_PARAM(token) \
  ((pattern_lex_param_t) ((token)                                 \
                           & PATTERN_LEX_PARAM_MASK))

/* ?? REVIEWME: No code available yet for packing/unpacking large
   parameters (needing more than PATTERN_LEX_PARAM_SIZE_BITS to be
   represented correctly).  Signed/unsigned params might be an issue
   here, and the type names to use on target architectures might
   also be tricky.  Not needed, so not implemented for now... */

typedef enum pattern_lex_opcode_enum
  {
    /* CHAR_DIRECT: Parameter is usually an octet, which is often,
       but not always (e.g. some DSP architectures), an unsigned
       char.  */
    PATTERN_LEX_OP_CHAR_DIRECT          =  0,

    /* CHAR_CLASS: Parameter for CHAR_CLASS is a reference (index?)
       to the unibyte-char class contents.  */
    PATTERN_LEX_OP_CHAR_CLASS           =  1,

    /* CHAR_CLASS_INVERTED: A character class, but where some or all
       of the class membership is to be inverted (inversion
       requested by leading "^" syntax).
       The inversion is not done at the tokeniser level, as higher-
       level treatment may vary based on different criteria (e.g.
       whether CR, LF, NUL and/or any other character are exempt
       from being inverted, depending on the needs of the user).  */
    PATTERN_LEX_OP_CHAR_CLASS_INVERTED  =  2,

/* ?? REVIEWME: The following opcodes are proposals (thought-bubble)
   only; they are not used in the experimental/initial release.  */

    /* WCHAR_DIRECT: Param field needs to be large enough to hold
       the largest possible unicode (wide) character (0x10ffff, 21
       bits); for a 32-bit token and 4-bit opcode, the current
       28-bit parameter field is more than adequate.  */
    PATTERN_LEX_OP_WCHAR_DIRECT         =  3,

    /* WCHAR_CLASS: Parameter for is a reference (index?) to the
       wide-char characters that are members of the class; this
       reference may describe variable-length lists of individual
       characters, and/or ranges of characters.  */
    PATTERN_LEX_OP_WCHAR_CLASS          =  4,

    /* WCHAR_CLASS_INVERTED: The parameter reference is identical to
       WCHAR_CLASS above, except that the named character(s) are
       excluded from the match.  The comments above for
       CHAR_CLASS_INVERTED, noting that different clients may have
       reasons for tailoring the inversion, also apply here.  */
    PATTERN_LEX_OP_WCHAR_CLASS_INVERTED =  5,

    /* REP_MINMAX: The parameter is a reference (index?) into an a
       array of structures, that allow more expression than can be
       put into a single 28-bit parameter (e.g. might use 64-bit
       unsigned ints for MIN and MAX, possibly plus a flag indicating
       no upper bound whatsoever).  */
    PATTERN_LEX_OP_REP_MINMAX           =  6,

    /* Extension opcode(s) could be used (?? prefix?) to enable
       representing larger values where the param field in the
       standard opcode layout is not large enough for some uses.
       This might occur in some cases, where the param field is only
       28 bits (largest unsigned value in 28 bits is a little over
       268 million).  */
    PATTERN_LEX_OP_EXTEND_PARAM         =  7,

    /* Tokens that need no parameters are collected under the OTHER
       opcode, with the parameter field naming the token type.  */
    PATTERN_LEX_OP_OTHER                =  8

    /* Other machinery, such as dfa state trackers, will probably
       eventually end up taking part or all of the namespace 10..15
       here... */

    /* ?? PROPOSAL: CLASS_ELEMENT opcode, used as an intermediate
       form when lexing and interpreting charclasses.  For example,
       ranges are prohibited to have complex start or end points,
       such as named classes, equivalence classes or other ranges.
       A lexer may find it easier to build an intermediate form,
       picking out these sub-structures in isolation (but also
       including deferring range processing in the first pass), and
       then post-processing to check the integrity of the pieces,
       and building the actual class membership.  */

  } pattern_lex_opcode_t;

/* "Other" (no-parameter) tokens.  The acronym "plotpe" is a hack,
    but the token names get to be very tedious to write out in full,
    sorry.  */

/* ?? REVIEWME: None of these are used in the initial "experimental"
   release of the code; this is an thought bubble/proposal on how the
   token stream might be set up to be handle more complex REs.  */
typedef enum pattern_lex_other_token_param_enum /* "plotpe" */
  {
    plotpe_EMPTY      = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  0),
    plotpe_BEGLINE    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  1),
    plotpe_ENDLINE    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  2),
    plotpe_BEGWORD    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  3),
    plotpe_ENDWORD    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  4),
    plotpe_LIMWORD    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  5),
    plotpe_NOTLIMWORD = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  6),
    plotpe_QMARK      = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  7),
    plotpe_STAR       = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  8),
    plotpe_PLUS       = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,  9),
    plotpe_CAT        = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 10),
    plotpe_OR         = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 11),
    plotpe_LPAREN     = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 12),
    plotpe_RPAREN     = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 13),
    plotpe_ANYCHAR    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 14),
    plotpe_END        = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 15),

    /* We put backreferences into the "other" namespace, as:
       (a) It only has a very small parameter range, and there's
           a lot of free space in the "other" parameter space; and
       (b) It does not use up one slot in the precious opcode
           namespace.  */
    plotpe_BACKREF_BASE  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 20),
    plotpe_BACKREF_LIMIT = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 29),
    plotpe_BACKREF_1  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 21),
    plotpe_BACKREF_2  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 22),
    plotpe_BACKREF_3  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 23),
    plotpe_BACKREF_4  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 24),
    plotpe_BACKREF_5  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 25),
    plotpe_BACKREF_6  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 26),
    plotpe_BACKREF_7  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 27),
    plotpe_BACKREF_8  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 28),
    plotpe_BACKREF_9  = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER, 29),

    /* The magic number for ILLEGAL must be reviewed if the layout
       of opcode/parameters is changed... the current value is
       chosen to be unlikely to interfere with bottom-up additions
       to the 28-bit parameter field space for the OTHER opcode,
       yet be so distinctive that it is easy to see during
       debugging.  */
    plotpe_ILLEGAL    = PATTERN_LEX_TOKEN (PATTERN_LEX_OP_OTHER,
                                           PATTERN_LEX_PLOTPE_ILLEGAL)
  } pattern_lex_other_token_params_t;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* ndef PATTERN_LEX_H */
/* vim:set shiftwidth=2: */
