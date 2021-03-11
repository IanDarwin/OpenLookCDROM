/* $Id: FontBank.h,v 1.2 1994/05/12 09:13:24 ishisone Rel $ */
/*
 * Copyright (c) 1991, 1994  Software Research Associates, Inc.
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
 */

#ifndef _FontBank_h
#define _FontBank_h

typedef struct _fb_rec_ *FontBank;	/* opaque */

extern FontBank FontBankCreate(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	char *		/* language */
#endif
);

extern void FontBankDestroy(
#if NeedFunctionPrototypes
	FontBank	/* bank */
#endif
);

extern XFontStruct ** FontBankGet(
#if NeedFunctionPrototypes
	FontBank	/* bank */,
	char *		/* fontset_base_names */,
	int *		/* num_fontsp */
#endif
);

extern void FontBankFreeFonts(
#if NeedFunctionPrototypes
	FontBank	/* bank */,
	XFontStruct **	/* fonts */,
	int		/* num_fonts */
#endif
);

#endif /* _FontBank_h */
