/* $Header: func.h,v 2.0 92/02/13 18:33:24 nao Exp $ */
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

extern Xsj3cEvent       _Xsj3cConvert();
extern Xsj3cEvent       _Xsj3cUnConvert();
extern Xsj3cEvent       _Xsj3cFix();
extern Xsj3cEvent       _Xsj3cReturn();

extern Xsj3cEvent       _Xsj3cModeHAlpha();
extern Xsj3cEvent       _Xsj3cModeZAlpha();
extern Xsj3cEvent       _Xsj3cModeHKata();
extern Xsj3cEvent       _Xsj3cModeZKata();
extern Xsj3cEvent       _Xsj3cModeHira();
extern Xsj3cEvent       _Xsj3cToHAlpha();
extern Xsj3cEvent       _Xsj3cToZAlpha();
extern Xsj3cEvent       _Xsj3cToHKata();
extern Xsj3cEvent       _Xsj3cToZKata();
extern Xsj3cEvent       _Xsj3cToHira();
extern Xsj3cEvent       _Xsj3cZenkaku();
extern Xsj3cEvent       _Xsj3cHankaku();
extern Xsj3cEvent       _Xsj3cToUpper();
extern Xsj3cEvent       _Xsj3cToLower();
extern Xsj3cEvent       _Xsj3cModeSJIS();
extern Xsj3cEvent       _Xsj3cModeEUC();
extern Xsj3cEvent       _Xsj3cModeJIS();
extern Xsj3cEvent       _Xsj3cModeKuten();
extern Xsj3cEvent       _Xsj3cCodeRollDown();
extern Xsj3cEvent       _Xsj3cModeRollDown();
extern Xsj3cEvent       _Xsj3cModeRollUp();
extern Xsj3cEvent       _Xsj3cNextMode();
extern Xsj3cEvent       _Xsj3cPrevMode();
extern Xsj3cEvent       _Xsj3cModeToggle();

extern Xsj3cEvent       _Xsj3cForward();
extern Xsj3cEvent       _Xsj3cBackward();
extern Xsj3cEvent       _Xsj3cTop();
extern Xsj3cEvent       _Xsj3cEnd();
extern Xsj3cEvent       _Xsj3cUp();
extern Xsj3cEvent       _Xsj3cDown();
extern Xsj3cEvent       _Xsj3cFirst();
extern Xsj3cEvent       _Xsj3cLast();
extern Xsj3cEvent       _Xsj3cNextPage();
extern Xsj3cEvent       _Xsj3cPrevPage();
extern Xsj3cEvent       _Xsj3cPrev();
extern Xsj3cEvent       _Xsj3cNext();
extern Xsj3cEvent       _Xsj3cSelect();
extern Xsj3cEvent       _Xsj3cCancel();

extern Xsj3cEvent       _Xsj3cExpand();
extern Xsj3cEvent       _Xsj3cShrink();

extern Xsj3cEvent       _Xsj3cBackSpace();
extern Xsj3cEvent       _Xsj3cDelete();
extern Xsj3cEvent       _Xsj3cDelAfter();

extern Xsj3cEvent       _Xsj3cStart();
extern Xsj3cEvent       _Xsj3cReConnect();
extern Xsj3cEvent       _Xsj3cReConvert();
extern Xsj3cEvent       _Xsj3cEdit();

extern Xsj3cEvent       _Xsj3cDRegBegin();
extern Xsj3cEvent       _Xsj3cDClearBegin();

extern Xsj3cEvent       _Xsj3cSymbolBegin();

extern Xsj3cEvent       _Xsj3cQuote();
extern Xsj3cEvent       _Xsj3cFlushBefore();
extern Xsj3cEvent       _Xsj3cBell();
extern Xsj3cEvent       _Xsj3cKana();
extern Xsj3cEvent       _Xsj3cSjrc();
extern Xsj3cEvent       _Xsj3cKill();
extern Xsj3cEvent       _Xsj3cNull();
extern Xsj3cEvent       _Xsj3cIgnore();

extern Xsj3cEvent       _Xsj3cUnConvSeg();
