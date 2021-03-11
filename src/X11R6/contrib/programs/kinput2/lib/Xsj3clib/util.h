/* $Header: util.h,v 2.1 93/09/21 09:41:39 nao Exp $ */
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

extern void                 Xsj3cError();
extern void                 Xsj3cWarning();

extern int                  _Xsj3cwPStowOUT();
extern int                  _Xsj3cwOUTtowPS();
extern int                  _Xsj3cmPStowPS();
extern int                  _Xsj3cmPStowOUT();
extern int                  _Xsj3cmPStowPSn();
extern int                  _Xsj3cmPStowOUTn();
extern void                 _Xsj3cwPStomPS();
extern void                 _Xsj3cwOUTtomPS();

extern int                  _Xsj3cCmp();
extern int                  _Xsj3cWcpy();
extern void                 _Xsj3cWcat();
extern int                  _Xsj3cWlen();
extern char                *_Xsj3cItoa();
extern char                *_Xsj3cXtoa();

extern void                 _Xsj3cInsertChar();
extern void                 _Xsj3cInsertWchar();
extern void                 _Xsj3cExtractChar();
extern void                 _Xsj3cStoreYomi();
extern int                  _Xsj3cStoreKanji();

extern void                 _Xsj3cFlushDcid();
extern void                 _Xsj3cClearDcid();

extern Xsj3ccMode           _Xsj3cCheckMode();

extern int                  in_lang,    out_lang;
extern int                  locked[SERVER_NUM];
extern unsigned long        KanaMask;

extern wchar                (*CodeConvFunc[4][4])();
