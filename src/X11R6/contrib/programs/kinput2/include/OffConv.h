/* $Id: OffConv.h,v 1.4 1991/08/20 09:22:15 ishisone Rel $ */
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

#ifndef _OffTheSpotConversion_h
#define _OffTheSpotConversion_h

/* OffTheSpotConversion widget public header file */

#include "ConvCtrl.h"

#define XtNleftMargin "leftMargin"
#define XtNrightMargin "rightMargin"

typedef struct _OffTheSpotConversionClassRec*	OffTheSpotConversionWidgetClass;
typedef struct _OffTheSpotConversionRec*		OffTheSpotConversionWidget;

typedef struct _SeparateConversionClassRec*	SeparateConversionWidgetClass;
typedef struct _SeparateConversionRec*		SeparateConversionWidget;

extern WidgetClass offTheSpotConversionWidgetClass;
extern WidgetClass separateConversionWidgetClass;

#endif
