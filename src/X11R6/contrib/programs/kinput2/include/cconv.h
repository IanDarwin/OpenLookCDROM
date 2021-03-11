/* $Id: cconv.h,v 10.0 1991/10/01 07:49:35 ishisone Rel $ */
/*
 *	cconv.h -- header file for cconv library
 *		version 10.0
 */

/*
 * Copyright (c) 1988  Software Research Associates, Inc.
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

#ifndef WCHAR_DEFINED
#define WCHAR_DEFINED
typedef unsigned short	wchar;
#endif

typedef struct _ccRule	*ccRule;
typedef struct _ccBuf	*ccBuf;

/* CCDEFPATH -- default ccdef file directory */
#ifndef CCDEFPATH
#define CCDEFPATH	"/usr/lib/X11/ccdef/"
#endif

#ifdef __STDC__
extern ccRule ccParseRule(char *, void (*)());
extern ccBuf ccCreateBuf(ccRule, int, char *[], int, void (*)(), void (*)(),
			 void (*)(), void (*)(), void (*)(), caddr_t);
extern void ccFreeRule(ccRule);
extern void ccDestroyBuf(ccBuf);
extern int ccConvchar(ccBuf, XKeyPressedEvent *);
extern int ccGetMode(ccBuf);
extern wchar *ccGetModePrompt(ccBuf);
extern ccRule ccGetRule(ccBuf);
extern void ccContextAppend(ccBuf, int);
extern void ccContextDelete(ccBuf);
extern void ccContextClear(ccBuf);
extern void ccContextSet(ccBuf, wchar *);
extern void ccContextGet(ccBuf, wchar *);

extern ccBuf ccInit(char *, int, void (*)(), void (*)(), void (*)(),
		    void (*)(), void (*)(), char **, int);
extern void ccTerminate(ccBuf);
#else
extern ccRule ccParseRule();
extern ccBuf ccCreateBuf();
extern void ccFreeRule();
extern void ccDestroyBuf();
extern int ccConvchar();
extern int ccGetMode();
extern wchar *ccGetModePrompt();
extern ccRule ccGetRule();
extern void ccContextAppend();
extern void ccContextDelete();
extern void ccContextClear();
extern void ccContextSet();
extern void ccContextGet();
extern ccBuf ccInit();
extern void ccTerminate();
#endif
