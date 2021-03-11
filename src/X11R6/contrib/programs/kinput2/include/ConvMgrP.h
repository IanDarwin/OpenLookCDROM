/* $Id: ConvMgrP.h,v 1.4 1991/04/18 16:59:16 ishisone Rel $ */
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

#ifndef _ConversionManagerP_h
#define _ConversionManagerP_h

#include <X11/CompositeP.h>
#include "ConvMgr.h"

typedef struct {
    int empty;
} ConversionManagerClassPart;

typedef struct _ConversionManagerClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ConversionManagerClassPart	conversionmanager_class;
} ConversionManagerClassRec;

extern ConversionManagerClassRec conversionmanagerClassRec;

typedef struct _convlist_ {
    Boolean	busy;
    WidgetClass	converterclass;
    WidgetClass	inputobjclass;
    WidgetClass	displayobjclass;
    Widget	converter;
    Screen	*screen;
    struct _convlist_	*next;
} ConverterRec;

typedef struct _inputobjlist_ {
    WidgetClass	inputobjclass;
    Widget	inputobj;
    struct _inputobjlist_	*next;
} InputObjRec;

typedef struct {
    /* private state */
    ConverterRec *converterlist;
    InputObjRec *inputobjlist;
} ConversionManagerPart;

typedef struct _ConversionManagerRec {
    CorePart		core;
    CompositePart	composite;
    ConversionManagerPart	convmgr;
} ConversionManagerRec;
#endif
