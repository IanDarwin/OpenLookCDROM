#ifndef lint
static char *rcsid = "$Header: sj3ctype.c,v 2.2 93/01/21 10:54:39 nao Exp $";
#endif
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

#include "common.h"

unsigned char kan1[4][4] = {
    0x81,   0x9f,   0xe0,   0xfc,
    0xa1,   0xfe,   0xa1,   0xfe,
    NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL
};

unsigned char kan2[4][4] = {
    0x40,   0x7e,   0x80,   0xfc,
    0xa1,   0xfe,   0xa1,   0xfe,
    NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL
};

wchar hira[2][2] = {
    0x829f, 0x82f1,
    0xa4a1, 0xa4f3
};

wchar kata[2][3] = {
    0x8340, 0x8396, 0x837f,
    0xa5a1, 0xa5f6, NULL
};

wchar zalpha[2][2] = {
    0x8260, 0x829a,
    0xa3c1, 0xa3fa
};

wchar zupper[2][2] = {
    0x8260, 0x8279,
    0xa3c1, 0xa3da
};

wchar zlower[2][2] = {
    0x8281, 0x829a,
    0xa3e1, 0xa3fa
};

wchar zplosive[2][2] = {
    0x82c1, 0x8362,
    0xa4c3, 0xa5c3
};

wchar zdakuten[2][2] = {
    0x814a, 0x814b,
    0xa1ab, 0xa1ac
};

wchar zvowel[2][4] = {
    0x829f, 0x82a8, 0x8340, 0x8349,
    0xa4a1, 0xa4aa, 0xa5a1, 0xa5aa
};
