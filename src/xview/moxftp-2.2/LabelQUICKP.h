/*
* $XConsortium: LabelQUICKP.h,v 1.27 91/06/22 19:34:58 rws Exp $
*/


/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* 
 * LabelQUICKP.h - Private definitions for Label widget
 * 
 */

#ifndef _XawLabelQUICKP_h
#define _XawLabelQUICKP_h

/***********************************************************************
 *
 * LabelQUICK Widget Private Data
 *
 ***********************************************************************/

#include "LabelQUICK.h"

#if defined(XAW)
#include <X11/Xaw/SimpleP.h>
#endif

#if defined(MOTIF)
#include <Xm/XmP.h>
#if XmREVISION==2
#include <Xm/PrimitiveP.h>
#endif
#endif

#if defined(OPENWINDOW)
#include <Xol/OpenLookP.h>
#include <Xol/Primitive.h>
#include <Xol/PrimitiveP.h>
#endif

#include <X11/Xmu/Converters.h>

/* New fields for the LabelQUICK widget class record */

typedef struct {int foo;} LabelQUICKClassPart;

/* Full class record declaration */
typedef struct _LabelQUICKClassRec {
    CoreClassPart	core_class;
#if defined(XAW)
    SimpleClassPart	simple_class;
#endif
#if defined(MOTIF)
    XmPrimitiveClassPart primitive_class;
#endif
#if defined(OPENWINDOW)
    PrimitiveClassPart  primitive_class;
#endif
    LabelQUICKClassPart	label_class;
} LabelQUICKClassRec;

extern LabelQUICKClassRec labelClassRec;

/* New fields for the LabelQUICK widget record */
typedef struct {
    /* resources */
    Pixel	foreground;
#if defined(MOTIF)
    XmFontList  fontlist;
#endif
    XFontStruct	*font;
    char	*label;
    XtJustify	justify;
    Dimension	internal_width;
    Dimension	internal_height;
    Pixmap	pixmap;
    Boolean	resize;
    unsigned char encoding;
    Pixmap	left_bitmap;

    /* private state */
    GC		normal_GC;
    GC          gray_GC;
    Pixmap	stipple;
    Position	label_x;
    Position	label_y;
    Dimension	label_width;
    Dimension	label_height;
    Dimension	label_len;
    int		lbm_y;			/* where in label */
    unsigned int lbm_width, lbm_height;	 /* size of pixmap */
    int		update;
} LabelQUICKPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _LabelQUICKRec {
    CorePart	core;
#if defined(XAW)
    SimplePart	simple;
#endif
#if defined(MOTIF)
    XmPrimitivePart primitive;
#endif
#if defined(OPENWINDOW)
    PrimitivePart       primitive;
#endif
    LabelQUICKPart	label;
} LabelQUICKRec;

#define LEFT_OFFSET(lw) ((lw)->label.left_bitmap \
			 ? (lw)->label.lbm_width + (lw)->label.internal_width \
			 : 0)

#endif /* _XawLabelQUICKP_h */
