/* $Header: /export/xpds/X.V11R5/contrib/im/kinput2/lib/Xsj3clib/RCS/sj3ctype.h,v 2.2 1992/10/20 08:50:46 nao Exp $ */
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

#ifndef _sj3ctype_h
#define _sj3ctype_h

extern unsigned char        kan1[4][4];
extern unsigned char        kan2[4][4];
extern wchar                hira[2][2];
extern wchar                kata[2][3];
extern wchar                zalpha[2][2];
extern wchar                zupper[2][2];
extern wchar                zlower[2][2];
extern wchar                zplosive[2][2];
extern wchar                zdakuten[2][2];
extern wchar                zvowel[2][10];

#ifndef iskan1
#define iskan1(c, l)       ((kan1[l][0]<=(c) && (c)<=kan1[l][1]) \
                                || (kan1[l][2]<=(c) && (c)<=kan1[l][3]))
#endif
#ifndef iskan2
#define iskan2(c, l)        ((kan2[l][0]<=(c) && (c)<=kan2[l][1]) \
                                || (kan2[l][2]<=(c) && (c)<=kan2[l][3]))
#endif
#ifndef ishira
#define ishira(c, l)        (hira[l][0]<=(c) && (c)<=hira[l][1])
#endif
#ifndef iskata
#define iskata(c, l)        (kata[l][0]<=(c) && (c)<=kata[l][1]\
                                && (c)!=kata[l][2])
#endif
#ifndef iszalpha
#define iszalpha(c, l)      (zalpha[l][0]<=(c) && (c)<=zalpha[l][1])
#endif
#ifndef iszupper
#define iszupper(c, l)      (zupper[l][0]<=(c) && (c)<=zupper[l][1])
#endif
#ifndef iszlower
#define iszlower(c, l)      (zlower[l][0]<=(c) && (c)<=zlower[l][1])
#endif
#ifndef isplosive
#define isplosive(c, l)     ((c)==zplosive[l][0] || (c)==zplosive[l][1])
#endif
#ifndef iszdakuten
#define iszdakuten(c, l)    ((c)==zdakuten[l][0] || (c)==zdakuten[l][1])
#endif
#ifndef iszvowel
#define iszvowel(c, l)      ((zvowel[l][0]<=(c) && (c)<=zvowel[l][1]) || \
                            (zvowel[l][2]<=(c) && (c)<=zvowel[l][3]))
#endif

#ifndef issjis1
#define issjis1(x)  ((0x81<=(x) && (x)<=0x9f) || (0xe0<=(x) && (x)<=0xfc))
#endif
#ifndef issjis2
#define issjis2(x)  ((0x40<=(x) && (x)<=0x7e) || (0x80<=(x) && (x)<=0xfc))
#endif
#ifdef iskana
#undef iskana
#endif
#define iskana(x)   (0xa1<=(x) && (x)<=0xdf)
#ifndef iskana2
#define iskana2(x)  (0xa1<=(x) && (x)<=0xdf)
#endif
#ifndef iseuc
#define iseuc(x)    (0xa1<=(x) && (x)<=0xfe)
#endif
#ifndef iseuckana
#define iseuckana(x)    ((x)==0x8e)
#endif
#ifndef isjis
#define isjis(x)   (0x21<=(x) && (x)<=0x7e)
#endif

#ifndef isvowel
#define isvowel(x)  ((x)==0x61 || (x)==0x65 || (x)==0x69 || (x)==0x6f || \
    (x)==0x75 || (x)==0x41 || (x)==0x45 || (x)==0x49 || (x)==0x4f || (x)==0x55)
#endif
#ifndef isdakuten
#define isdakuten(x)  (0xde<=(x) && (x)<=0xdf)
#endif
#ifndef isdakuon
#define isdakuon(x) (((x)>=0xb6 && (x)<=0xc4) || ((x)>=0xca && (x)<=0xce))
#endif

#endif /* _sj3ctype_h */
