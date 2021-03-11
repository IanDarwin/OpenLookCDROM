/* $Id: ConvDispP.h,v 1.7 1991/09/17 10:04:46 ishisone Rel $ */
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
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

#ifndef _ConvDisplayP_h
#define _ConvDisplayP_h

#include <X11/ObjectP.h>
#include "ConvDisp.h"

typedef struct {
    int		(*StringWidth)();
    int		(*LineHeight)();
    void	(*DrawString)();
    int		(*MaxChar)();
    void	(*DrawCursor)();
    void	(*GetCursorBounds)();
    void	(*SetFonts)();
} ConvDisplayClassPart;

typedef struct _ConvDisplayClassRec {
    ObjectClassPart object_class;
    ConvDisplayClassPart convDisplay_class;
} ConvDisplayClassRec;

typedef struct {
    /* resources */
    Pixel	foreground;
    Pixel	background;
    Pixmap	cursor;		/* really a bitmap */
    Position	hotx;
    Position	hoty;
    /* private */
    Boolean	cursorcreated;
    XRectangle	cursorbounds;
    GC		cursorgc;
    Boolean	cursorvisible;
} ConvDisplayPart;

typedef struct _ConvDisplayRec {
    ObjectPart  object;
    ConvDisplayPart convDisplay;
} ConvDisplayRec;

extern ConvDisplayClassRec	convDisplayClassRec;

#define XtInheritStringWidth	(int(*)())_XtInherit
#define XtInheritLineHeight	(int(*)())_XtInherit
#define XtInheritDrawString	(void(*)())_XtInherit
#define XtInheritMaxChar	(int(*)())_XtInherit
#define XtInheritDrawCursor	(void(*)())_XtInherit
#define XtInheritGetCursorBounds	(void(*)())_XtInherit
#define XtInheritSetFonts	(void(*)())_XtInherit


/* semi-public function */

typedef struct {
    Atom	registry;	/* ex) "ISO8859", "JISX0208.1983" */
    Atom	encoding;	/* ex) "0", "1" */
    XFontStruct *font;		/* return value */
} FontSpec;

extern int _CDPickupFonts(
#if NeedFunctionPrototypes
	Widget		/* widget */,
	FontSpec *	/* spec */,
	Cardinal	/* num_specs */,
	XFontStruct **	/* fonts */,
	Cardinal	/* num_fonts */
#endif
);

#endif
