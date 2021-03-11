/* $Id: WcharDispP.h,v 1.6 1991/09/17 10:08:59 ishisone Rel $ */
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

#ifndef _WcharDisplayP_h
#define _WcharDisplayP_h

#include "ConvDispP.h"
#include "WcharDisp.h"
#include "XWStr.h"


#define XtRFontMapping "FontMapping"

typedef struct {
    String charset;	/* <registry>-<encoding> ex) "JIX0208.1983-0" */
    int flag;
#define G0LCharSet	(1<<0)		/* font for G0 chars. use GL */
#define G0RCharSet	(1<<1)		/* font for G0 chars. use GR */
#define G1LCharSet	(1<<2)
#define G1RCharSet	(1<<3)
#define G2LCharSet	(1<<4)
#define G2RCharSet	(1<<5)
#define G3LCharSet	(1<<6)
#define G3RCharSet	(1<<7)
} WDCharSet;

typedef struct {
    WDCharSet *charset_specs;
    Cardinal num_specs;
} WcharDisplayClassPart;

typedef struct _WcharDisplayClassRec {
    ObjectClassPart object_class;
    ConvDisplayClassPart convDisplay_class;
    WcharDisplayClassPart wcharDisplay_class;
} WcharDisplayClassRec;

typedef struct {
    Boolean grmapping[4];
} FontMapping;

typedef struct {
    /* resources */
    XFontStruct *defaultfonts[4];	/* default fonts */
    FontMapping defaultmapping;		/* use GR or not */
    /* private state */
    WDCharSet *charset_specs;		/* same as the one in class structure.
					 * just for convenience
					 */
    FontSpec *fontspecs;
    Cardinal num_specs;
    XFontStruct *fonts[4];		/* fonts now in use */
    Boolean grmapping[4];
    Pixmap stipple;			/* Stipple Bitmap */
    XWSGC gcset_normal;
    XWSGC gcset_rev;
    GC gc_normal;
    GC gc_stipple;
    int fontheight;
    int ascent;
} WcharDisplayPart;

typedef struct _WcharDisplayRec {
    ObjectPart  object;
    ConvDisplayPart convDisplay;
    WcharDisplayPart wcharDisplay;
} WcharDisplayRec;

extern WcharDisplayClassRec wcharDisplayClassRec;


typedef struct {
    int empry;
} JpWcharDisplayClassPart;

typedef struct _JpWcharDisplayClassRec {
    ObjectClassPart object_class;
    ConvDisplayClassPart convDisplay_class;
    WcharDisplayClassPart wcharDisplay_class;
    JpWcharDisplayClassPart jpWcharDisplay_class;
} JpWcharDisplayClassRec;

typedef struct {
    int empty;
} JpWcharDisplayPart;

typedef struct _JpWcharDisplayRec {
    ObjectPart  object;
    ConvDisplayPart convDisplay;
    WcharDisplayPart wcharDisplay;
    JpWcharDisplayPart jpWcharDisplay;
} JpWcharDisplayRec;

extern JpWcharDisplayClassRec jpWcharDisplayClassRec;

#endif
