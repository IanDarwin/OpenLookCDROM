/*
 *	WStr.h -- header file for Wide-Character String Library
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *		ishisone@sra.co.jp
 */

/* $Id: WStr.h,v 2.1 1991/09/23 05:32:48 ishisone Rel $ */

#ifndef _WStr_h
#define _WStr_h

#ifndef WCHAR_DEFINED
#define WCHAR_DEFINED
typedef unsigned short	wchar;
#endif

/* for backward compatibility... */
#define convWStoLatin1	convJWStoLatin1
#define convLatin1toWS	convLatin1toJWS
#define convWStoJIS	convJWStoJIS
#define convJIStoWS	convJIStoJWS
#define convWStoEUC	convJWStoEUC
#define convEUCtoWS	convEUCtoJWS
#define convWStoSJIS	convJWStoSJIS
#define convSJIStoWS	convSJIStoJWS

#if __STDC__ == 1
extern int convJWStoLatin1(wchar *, unsigned char *);
extern int convLatin1toJWS(unsigned char *, wchar *);
extern int convJWStoJIS(wchar *, unsigned char *);
extern int convJIStoJWS(unsigned char *, wchar *);
extern int convJWStoEUC(wchar *, unsigned char *);
extern int convEUCtoJWS(unsigned char *, wchar *);
extern int convJWStoSJIS(wchar *, unsigned char *);
extern int convSJIStoJWS(unsigned char *, wchar *);
extern wchar *wstrcat(wchar *, wchar *);
extern wchar *wstrncat(wchar *, wchar *, int);
extern int wstrcmp(wchar *, wchar *);
extern int wstrncmp(wchar *, wchar *, int);
extern wchar *wstrcpy(wchar *, wchar *);
extern wchar *wstrncpy(wchar *, wchar *, int);
extern int wstrlen(wchar *);
extern wchar *windex(wchar *, wchar);
extern wchar *wrindex(wchar *, wchar);
#else
extern int convJWStoLatin1();
extern int convLatin1toJWS();
extern int convJWStoJIS();
extern int convJIStoJWS();
extern int convJWStoEUC();
extern int convEUCtoJWS();
extern int convJWStoSJIS();
extern int convSJIStoJWS();
extern wchar *wstrcat();
extern wchar *wstrncat();
extern int wstrcmp();
extern int wstrncmp();
extern wchar *wstrcpy();
extern wchar *wstrncpy();
extern int wstrlen();
extern wchar *windex();
extern wchar *wrindex();
#endif

#endif
