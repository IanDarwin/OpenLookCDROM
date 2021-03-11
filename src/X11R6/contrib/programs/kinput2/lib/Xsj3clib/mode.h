/* $Header: mode.h,v 2.0 92/02/13 18:33:30 nao Exp $ */
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

extern Xsj3cEvent           _Xsj3cModeChange();
extern Xsj3cEvent           _Xsj3cModeClear();
extern Xsj3cEvent           Xsj3cModeConv();

extern void                 _Xsj3cHiraToZKata();
extern void                 _Xsj3cHKataToHira();
extern void                 _Xsj3cHKataToZKata();
extern void                 _Xsj3cZKanaToHKata();
extern void                 _Xsj3cZKataToHira();
extern void                 _Xsj3cHAlphaToZAlpha();
extern void                 _Xsj3cHAlphaToZKana();
extern void                 _Xsj3cHAlphaToHKata();
extern void                 _Xsj3cZAlphaToHAlpha();
