/* $Id: OffConvP.h,v 1.6 1991/09/11 07:44:00 ishisone Rel $ */
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

#ifndef _OffTheSpotConversionP_h
#define _OffTheSpotConversionP_h

#include "ConvCtrlP.h"
#include "OffConv.h"

typedef struct {
    Position	x, y;
} DisplayLocation;

typedef struct _fragment_ {
    unsigned short from;	/* 表示開始文字位置 */
    unsigned short nchars;	/* 表示文字数 */
    XRectangle region;		/* 表示領域 */
    struct _fragment_ *next;	/* 次のフラグメント */
} DisplayFragment;

typedef struct {
    ICString seg;		/* セグメント */
    short redrawpos;		/* 書き直しが必要な文字の開始位置
				 * (-1 なら書き直す必要なし)
				 */
    DisplayFragment *fragments;	/* 表示フラグメント */
} DisplaySegment;


/*
 *	off-the-spot conversion control widget data structure
 */

typedef struct {
    int empty;
} OffTheSpotConversionClassPart;

typedef struct _OffTheSpotConversionClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ShellClassPart	shell_class;
    WMShellClassPart	wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TransientShellClassPart	transient_shell_class;
    ConversionControlClassPart	conversionControl_class;
    OffTheSpotConversionClassPart offTheSpotConversion_class;
} OffTheSpotConversionClassRec;

extern OffTheSpotConversionClassRec offTheSpotConversionClassRec;

typedef struct {
    /* resources */
    Dimension	leftmargin;
    Dimension	rightmargin;
    /* private state */
    Widget	displayobj;
    Widget	selectionshell;
    Widget	selectionwidget;
    Widget	formwidget;
    Widget	modewidget;
    Widget	modeshell;
    Widget	modedisplayobj;
    Widget	canvaswidget;
    DisplayLocation	cursorlocation;
    Boolean	cursorvisible;
    DisplaySegment	*dispsegments;
    Cardinal	numsegments;
    Cardinal	dispsegmentsize;
    ICString	*candlist;
    Cardinal	numcands;
    Boolean	selectionpoppedup;
    Dimension	lineheight;
    Position	ascent;
    Widget	auxshell;
    Widget	auxwidget;
    Boolean	auxpoppedup;
} OffTheSpotConversionPart;

typedef struct _OffTheSpotConversionRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TransientShellPart	transient;	
    ConversionControlPart	ccontrol;
    OffTheSpotConversionPart	offthespot;
} OffTheSpotConversionRec;


/*
 *	separate conversion control widget data structure
 */

typedef struct {
    int empty;
} SeparateConversionClassPart;

typedef struct _SeparateConversionClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ShellClassPart	shell_class;
    WMShellClassPart	wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TransientShellClassPart	transient_shell_class;
    ConversionControlClassPart	conversionControl_class;
    OffTheSpotConversionClassPart offTheSpotConversion_class;
    SeparateConversionClassPart separateConversion_class;
} SeparateConversionClassRec;

extern SeparateConversionClassRec separateConversionClassRec;

typedef struct {
    Widget	formwidget;
} SeparateConversionPart;

typedef struct _SeparateConversionRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TransientShellPart	transient;	
    ConversionControlPart	ccontrol;
    OffTheSpotConversionPart	offthespot;
    SeparateConversionPart	separate;
} SeparateConversionRec;

#endif
