#ifndef lint
static char *rcsid = "$Header: code.c,v 2.1 93/09/21 09:43:46 nao Exp $";
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

#define WMASK   0x7f7f
#define WMSB    0x8080

wchar _Xsj3csjis2euc();
wchar _Xsj3ceuc2sjis();
wchar _Xsj3csjis2jis();
wchar _Xsj3cjis2sjis();
wchar _Xsj3cjis2euc();
wchar _Xsj3ceuc2jis();

wchar
_Xsj3csjis2euc (c)
    register wchar          c;
{
    register int            high, low;

    if (c >= 0xf040)
        return 0;
    high = (c >> 8) & 0xff;
    low = c & 0xff;
    if (high > 0x9f)
        high -= 0x40;
    if (low > 0x9e)
        return (((high << 9) | low) - 0x5ffe);
    if (low > 0x7f)
        low--;
    return (((high << 9) | low) - 0x609f);
}

wchar
_Xsj3ceuc2sjis (c)
    register wchar          c;
{
    register int            high, low;

    high = (c >> 8) & 0xff;
    low = c & 0xff;
    if (!(high & 1))
        low -= 0x02;
    else if (low < 0xe0)
        low -= 0x61;
    else
        low -= 0x60;
    high = ((high - 0xa1) >> 1) + (high < 0xdf ? 0x81 : 0xc1);
    return ((high << 8) | low);
}

wchar
_Xsj3csjis2jis (c)
    register wchar          c;
{
    register int            high, low;

    if (c >= 0xf040)
        return 0;
    high = (c >> 8) & 0xff;
    low = c & 0xff;
    if (high > 0x9f)
        high -= 0x40;
    if (low > 0x9e)
        return (((high << 9) | low) - 0xe07e);
    if (low > 0x7f)
        low--;
    return (((high << 9) | low) - 0xe11f);
}

wchar
_Xsj3cjis2sjis (c)
    register wchar          c;
{
    register int            high, low;

    high = (c >> 8) & 0xff;
    low = c & 0xff;
    if (!(high & 1))
        low += 0x7e;
    else if (low < 0x60)
        low += 0x1f;
    else 
        low += 0x20;
    high = ((high - 0x21) >> 1) + (high < 0x5f ? 0x81 : 0xc1);
    return ((high << 8) | low);
}

wchar
_Xsj3cjis2euc (c)
    register wchar          c;
{
    return (c | WMSB);
}

wchar
_Xsj3ceuc2jis (c)
    register wchar          c;
{
    return (c & WMASK);
}
