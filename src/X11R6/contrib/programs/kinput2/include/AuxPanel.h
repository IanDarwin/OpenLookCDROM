/* $Id: AuxPanel.h,v 1.6 1993/09/08 01:31:45 ishisone Rel $ */
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

#ifndef _AuxPanel_h
#define _AuxPanel_h

#include "ICtypes.h"

/*
  AuxPanel new resources:

  name			class		type		default		access
  ----------------------------------------------------------------------------
  foreground		Foreground	Pixel		default		CSG
  horizontalSpacing	Spacing		Dimension	6		CSG
  verticalSpacing	Spacing		Dimension	4		CSG
  defaultWidth		DefaultWidth	Dimension	400		CSG
  cursor		Cursor		Cursor		parent		CSG

  AuxPanel widget には実際に文字列を表示するための widget を子 widget
  として与える必要がある。この widget のクラスは ConvDisplayObject クラスの
  サブクラスでなければならない。
*/

#define XtCSpacing "Spacing"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"

#define XtCDefaultWidth "DefaultWidth"
#define XtNdefaultWidth "defaultWidth"

#define XtNcursor "cursor"

typedef struct _AuxPanelClassRec*	AuxPanelWidgetClass;
typedef struct _AuxPanelRec*		AuxPanelWidget;

extern WidgetClass auxPanelWidgetClass;

/*
 * Public Interface Functions
 */

/*
 * void APanelStart(Widget w)
 *	パネルの内容が発生したことを通知する。
 */
extern void APanelStart(
#if NeedFunctionPrototypes
	Widget		/* w */,
	ICString *	/* segments */,
	Cardinal	/* nseg */,
	Cardinal	/* curseg */,
	Cardinal	/* cursorpos */
#endif
);

/*
 * void APanelChange(Widget w)
 *	パネルの内容が変化したことを通知する。
 */
extern void APanelChange(
#if NeedFunctionPrototypes
	Widget		/* w */,
	ICString *	/* segments */,
	Cardinal	/* nseg */,
	Cardinal	/* curseg */,
	Cardinal	/* cursorpos */
#endif
);

/*
 * void APanelEnd(Widget w)
 *	パネルの内容が消滅したことを通知する。
 */
extern void APanelEnd(
#if NeedFunctionPrototypes
	Widget		/* w */
#endif
);

extern void APanelSetList(), APanelSetCurrent(), APanelMoveCurrent();

#endif
