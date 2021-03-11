/* $Id: InputConvP.h,v 1.12 1991/09/26 00:35:29 ishisone Rel $ */
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

#ifndef _InputConvP_h
#define _InputConvP_h

#include <X11/ObjectP.h>
#include "InputConv.h"

typedef struct {
    int		(*InputEvent)();
    ICString	*(*GetMode)();
    int		(*CursorPos)();
    int		(*NumSegments)();
    ICString	*(*GetSegment)();
    int		(*CompareSegment)();
    ICString	*(*GetItemList)();
    int		(*SelectItem)();
    int		(*GetConvertedString)();
    int		(*ClearConversion)();
    ICString	*(*GetAuxSegments)();
    Boolean	SupportMultipleObjects;
    /* private state */
    Boolean	NoMoreObjects;
} InputConvClassPart;

typedef struct _InputConvClassRec {
    ObjectClassPart object_class;
    InputConvClassPart inputConv_class;
} InputConvClassRec;

extern InputConvClassRec	inputConvClassRec;

typedef struct {
    /* resources */
    XtCallbackList	selectioncallback;
    XtCallbackList	modechangecallback;
    XtCallbackList	textchangecallback;
    XtCallbackList	endcallback;
    XtCallbackList	fixcallback;
    XtCallbackList	auxcallback;
    WidgetClass		displayObjClass;
} InputConvPart;

typedef struct _InputConvRec {
    ObjectPart  object;
    InputConvPart inputConv;
} InputConvRec;

#define XtInheritInputEvent 	(int(*)())_XtInherit
#define XtInheritGetMode	(ICString*(*)())_XtInherit
#define XtInheritCursorPos 	(int(*)())_XtInherit
#define XtInheritNumSegments 	(int(*)())_XtInherit
#define XtInheritGetSegment	(ICString*(*)())_XtInherit
#define XtInheritCompareSegment (int(*)())_XtInherit
#define XtInheritGetItemList	(ICString*(*)())_XtInherit
#define XtInheritSelectItem 	(int(*)())_XtInherit
#define XtInheritGetConvertedString 	(int(*)())_XtInherit
#define XtInheritClearConversion 	(int(*)())_XtInherit
#define XtInheritGetAuxSegments		(ICString*(*)())_XtInherit

#endif
