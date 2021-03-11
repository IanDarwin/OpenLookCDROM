/* $Id: KIFontSet.h,v 1.2 1991/09/23 04:10:34 ishisone Rel $ */
/*
 * Copyright (c) 1991  Software Research Associates, Inc.
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

#ifndef _KIFontSet_h
#define _KIFontSet_h

typedef struct {
    String pattern;		/* must be in lower case
				 * ex) "iso8859-*", "jisx0208.1983-0"
				 */
    XtPointer cldata;
} KICharSetSpec;

typedef struct {
    KICharSetSpec *specs;	/* first spec is the most preferrable
				 * charset.
				 */
    Cardinal num_specs;
} KICharSet;

typedef struct {
    XFontStruct *font;
    String charset;		/* matched KICharSetSpec's pattern */
    XtPointer cldata;		/* matched KICharSetSpec's cldata */
} KICharSetFont;

/*
 * int ExtractFontsFromFontSet(Display *dpy, String fontset_string,
 *			       KICharSet *charsets, KICharSetFont *result,
 *			       Cardinal num_charsets)
 *
 * gets appropriate fonts for each of give charsets from fontset_string.
 * fontset_string is similar to base_font_name_list in XCreateFontSet().
 * returns number of matched fonts.
 *
 * note) use CachedFreeFont() to free fonts obtained by this function.
 */
extern int ExtractFontsFromFontSet(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	String		/* fontset_string */,
	KICharSet *	/* charsets */,
	KICharSetFont *	/* result (must be allocated by the caller) */,
	Cardinal	/* num_charsets */
#endif
);

#endif
