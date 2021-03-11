/* $Id: ICLabelP.h,v 1.4 1991/03/22 17:46:42 ishisone Rel $ */
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

#ifndef _ICLabelP_h
#define _ICLabelP_h

#include <X11/CompositeP.h>
#include "ICLabel.h"

typedef struct {
    int empty;
} ICLabelClassPart;

typedef struct _ICLabelClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ICLabelClassPart	iclabel_class;
} ICLabelClassRec;

extern ICLabelClassRec icLabelClassRec;

typedef struct {
    /* resources */
    Dimension hspace;
    Dimension vspace;
    ICString *label;
    XtJustify justify;
    Cursor cursor;
    /* private state */
    Widget displayobj;
    Dimension width;
    Dimension fontheight;
} ICLabelPart;

typedef struct _ICLabelRec {
    CorePart	core;
    CompositePart	composite;
    ICLabelPart	iclabel;
} ICLabelRec;

#endif
