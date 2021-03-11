/* $Id: CandPanelP.h,v 1.4 1991/03/22 18:13:23 ishisone Rel $ */
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

#ifndef _CandidatePanelP_h
#define _CandidatePanelP_h

#include <X11/CoreP.h>
#include "CandPanel.h"

typedef struct {
    int empty;
} CandidatePanelClassPart;

typedef struct _CandidatePanelClassRec {
    CoreClassPart		super_class;
    CompositeClassPart		composite_class;
    CandidatePanelClassPart	candidatepanel_class;
} CandidatePanelClassRec;

extern CandidatePanelClassRec candidatepanelClassRec;

typedef struct {
    /* resources */
    Pixel foreground;
    Dimension defaultwidth;
    Dimension hspace;
    Dimension vspace;
    ICString *list;
    int nstrings;
    int current;	/* カレントのアイテム番号 */
    Cursor cursor;
    XtCallbackList callback;
    /* private state */
    Widget displayobj;		/* actually, this is the only child */
    GC invgc;
    int fontheight;
    /* int ascent; */
    int maxwidth;	/* アイテムの最大長 */
    int ncolumns;
    int nrows;
} CandidatePanelPart;

typedef struct _CandidatePanelRec {
    CorePart		core;
    CompositePart	composite;
    CandidatePanelPart	cpanel;
} CandidatePanelRec;

#endif
