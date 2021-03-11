/* $Header: common.h,v 2.2 93/09/21 14:31:39 nao Exp $ */
/*
 * Copyright 1991 Sony Corporation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Sony not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Sony makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * SONY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL SONY
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/*
 * Author: Naoshi Suzuki, SONY Corporation.  (nao@sm.sony.co.jp)
 */

#include "Xsj3clib.h"
#include <X11/Xlib.h>
#if defined(XlibSpecificationRelease) && XlibSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif

/*
 * define for conversion server
 */
#define SERVER_SJ3              0
#define SERVER_NUM              1

/*
 * define for conversion mode
 */
#define NoInputModeMask         (1<<16)
#define InputModeMask           (1<<17)
#define ConvedModeMask          (1<<18)
#define SelectModeMask          (1<<19)
#define DictModeMask            (1<<20)
#define AllModeMask             (InputModeMask|ConvedModeMask|SelectModeMask \
                                |DictModeMask|NoInputModeMask)
#define OutputModeMask          (ConvedModeMask|SelectModeMask)
#define FlushModeMask           (InputModeMask|ConvedModeMask)

/*
 * Define for segment status InputMode/ConvedMode
 */
#define SEG_NOCONV              (1L<<0)
#define SEG_CONVED              (1L<<1)

/*
 * Define for permittion to edit(input)
 */
#define SEG_EDIT                (1L<<0)
#define SEG_NOEDIT              (1L<<1)

/*
 * define for maximum size
 */
#define INPUT_YOMI_MAX          255
#define DICT_YOMI_MAX           31

/*
 * define for result of conversion
 */
#define CONV_FAILED             -1
#define CONV_UNFIXED            0
#define CONV_FIXED              1

/*
 * define for flags on/off
 */
#undef OFF
#define OFF                     0L
#undef ON
#define ON                      (1L<<0)
#define ONE                     (1L<<0)
#define THROUGH                 (1L<<0)
#define ALL                     (1L<<1)
#define NONE                    (1L<<1)
#define QUOTE                   (1L<<1)
#define AFTER                   (1L<<2)
#define EDIT                    (1L<<2)

/*
 * define for mode of dictionary handling
 */
#define REG_STATE               (1L<<0)
#define CLR_STATE               (1L<<1)

/*
 * define for message in DictMode(sj3)
 */
#define SJ3_DICT_INPUT          128
#define SJ3_DICT_YOMI           129
#define SJ3_DICT_HINSI          130
#define SJ3_DICT_CONFIRM        131
#define SJ3_TOUROKU_SUCCESSED   132
#define SJ3_SYOUKYO_SUCCESSED   133
#define SJ3_NO_YOMI_STR         134
#define SJ3_LONG_YOMI_STR       135

/*
 * Define for SelectMode status
 */
#define SELECT_CAND             0
#define SELECT_SYMBOL           1
#define SELECT_HINSI            2

/*
 * Define for status of dictionary handling
 */
#define DICT_INPUT              0
#define DICT_HINSI              1
#define DICT_CONFIRM            2
#define DICT_END                3

/*
 * Define for DictMode messages
 */
#define DICT_STR_MODE           0
#define DICT_STR_MSG1           1
#define DICT_STR_YOMI           2
#define DICT_STR_MSG2           3
#define DICT_STR_HINSI          4
#define DICT_STR_MSG3           5
#define DICT_STR_NUM            6

/*
 * Define for NULL
 */
#ifndef NULL
#define NULL                    0
#endif

/*
 * define for reading set-up file.
 */
#define OPEN_FAILED             1
#define READ_FAILED             -1
#define ALLOC_FAILED            -2
#define OK                      0

#define SKIP(p)         { while (*p == '\t' || *p == ' ') p++; }
#define CHECK_END(p)    { if (*p == '\n' || *p == '#') return (READ_FAILED); }

/*
 * define for reading set-up files.
 */
#define SO                      0x0e
#define SI                      0x0f
#define ESC                     0x1b
#define SS2                     0x8e
#define SS3                     0x8f
#define MASK                    0x7f
#define MSB                     0x80

/*
 * define for conversion interface function.
 */
#define FUNC_OPEN               0
#define FUNC_CLOSE              1
#define FUNC_CONV               2
#define FUNC_CANDNUM            3
#define FUNC_CANDIDATE          4
#define FUNC_REGISTER           5
#define FUNC_CLEAR              6
#define FUNC_STUDY              7
#define FUNC_STUDY2             8
#define FUNC_LOCK               9
#define FUNC_UNLOCK             10

#define FUNC_NUM                11

typedef struct _sjrctable {
    unsigned char   *key[2];
    unsigned char   *value[10];
} SjrcRec,  *Sjrctable;

typedef struct _Xsj3cCVServerIF {
    int             lang;
    int             (*func[FUNC_NUM])();
} Xsj3cCVServerIF;
