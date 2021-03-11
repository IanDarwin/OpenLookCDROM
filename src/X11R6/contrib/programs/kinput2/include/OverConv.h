/* $Id: OverConv.h,v 1.6 1991/09/28 03:17:32 ishisone Rel $ */
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

#ifndef _OverTheSpotConversion_h
#define _OverTheSpotConversion_h

/* OverTheSpotConversion widget public header file */

#include "ConvCtrl.h"

#define XtNspotX "spotX"
#define XtNspotY "spotY"
#define XtNautoSpotForwarding "autoSpotForwarding"
#define XtCAutoSpotForwarding "AutoSpotForwarding"
#define XtNlineSpacing "lineSpacing"
#define XtCLineSpacing "LineSpacing"
#define XtNmodeLocation "modeLocation"
#define XtCModeLocation "ModeLocation"
#define XtNshrinkWindow "shrinkWindow"
#define XtCShrinkWindow "ShrinkWindow"
#define XtNignoreStatusAreaSpec "ignoreStatusAreaSpec"
#define XtCIgnoreStatusAreaSpec "IgnoreStatusAreaSpec"
#define XtNmodeBorderForeground "modeBorderForeground"
#define XtCModeBorderForeground "ModeBorderForeground"

typedef enum {
    ModeTopLeft,	/* top-left of the client window */
    ModeTopRight,	/* top-right			 */
    ModeBottomLeft,	/* bottom-left			 */
    ModeBottomRight,	/* bottom-right			 */
    ModeTrackText	/* tracks text-in-conversion	 */
} ModeLocation;

typedef struct _OverTheSpotConversionClassRec*	OverTheSpotConversionWidgetClass;
typedef struct _OverTheSpotConversionRec*		OverTheSpotConversionWidget;

extern WidgetClass overTheSpotConversionWidgetClass;

#endif
