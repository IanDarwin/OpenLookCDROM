/* $Id: AuxPanelP.h,v 1.7 1993/09/08 01:38:20 ishisone Rel $ */
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

/* Copyright 1991 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of NEC Corporation
 * not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.  NEC 
 * Corporation makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 *
 * Author: Akira Kon, NEC Corporation.  (kon@d1.bs2.mt.nec.co.jp)
 */

#ifndef _AuxPanelP_h
#define _AuxPanelP_h

#include <X11/CoreP.h>
#include "AuxPanel.h"

typedef struct {
    int empty;
} AuxPanelClassPart;

typedef struct _AuxPanelClassRec {
    CoreClassPart		super_class;
    CompositeClassPart		composite_class;
    AuxPanelClassPart		auxpanel_class;
} AuxPanelClassRec;

extern AuxPanelClassRec auxpanelClassRec;

typedef struct {
    ICString seg;		/* セグメント */
    short redrawpos;		/* 書き直しが必要な文字の開始位置
				 * (-1 なら書き直す必要なし)
				 */
    Cardinal width;		/* 表示幅 */
} DisplaySegment;

typedef struct {
    /* resources */
    Pixel foreground;
    Dimension defaultwidth;
    Dimension hspace;
    Dimension vspace;
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

    DisplaySegment	*dispauxsegments;
    Cardinal		numauxsegments;
    Cardinal		dispauxsegmentsize;
} AuxPanelPart;

typedef struct _AuxPanelRec {
    CorePart		core;
    CompositePart	composite;
    AuxPanelPart	cpanel;
} AuxPanelRec;

#endif
