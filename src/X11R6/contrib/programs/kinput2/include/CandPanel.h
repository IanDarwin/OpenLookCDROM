/* $Id: CandPanel.h,v 1.4 1991/03/22 18:13:39 ishisone Rel $ */
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

#ifndef _CandidatePanel_h
#define _CandidatePanel_h

#include "ICtypes.h"

/*
  CandidatePanel new resources:

  name			class		type		default		access
  ----------------------------------------------------------------------------
  foreground		Foreground	Pixel		default		CSG
  horizontalSpacing	Spacing		Dimension	6		CSG
  verticalSpacing	Spacing		Dimension	4		CSG
  list			List		Pointer		NULL		CSG
  numStrings		NumStrings	Int		0		CSG
  defaultWidth		DefaultWidth	Dimension	400		CSG
  currentItem		CurrentItem	Int		0		CSG
  cursor		Cursor		Cursor		parent		CSG
  callback		Callback	Callback	--

  CandidatePanel widget には実際に文字列を表示するための widget を子 widget
  として与える必要がある。この widget のクラスは ConvDisplayObject クラスの
  サブクラスでなければならない。
*/

#define XtCSpacing "Spacing"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"

#define XtCList "List"
#define XtNlist "list"
#define XtCNumStrings "NumStrings"
#define XtNnumStrings "numStrings"

#define XtCDefaultWidth "DefaultWidth"
#define XtNdefaultWidth "defaultWidth"

#define XtCCurrentItem "CurrentItem"
#define XtNcurrentItem "currentItem"

#define XtNcursor "cursor"

typedef struct _CandidatePanelClassRec*	CandidatePanelWidgetClass;
typedef struct _CandidatePanelRec*		CandidatePanelWidget;

extern WidgetClass candidatePanelWidgetClass;

/*
 * Public Interface Functions
 */

/*
 * void CPanelSetList(Widget w, ICString *list, int nstrings,
 *		      int current, int resize)
 *	リストを新たにセットする
 *	current にはカレントアイテムの番号を指定する
 *	resize が True ならば設定されたリストに合わせてリサイズを行なう
 *	list が NULL ならばリストは変更されないが、サイズの計算をやり直すので
 *	子 widget (ConvDisplayObject) のフォントなどのリソースを変えた時には
 *		CPanelSetList(w, NULL, 0, 0, 0);
 *	を実行するとよい
 */
extern void CPanelSetList(
#if NeedFunctionPrototypes
	Widget		/* w */,
	ICString *	/* list (or NULL) */,
	int		/* nstrings */,
	int		/* current */,
	int		/* resize */
#endif
);

/*
 * void CPanelSetCurrent(Widget w, int current)
 *	カレントアイテムを idx で指定した番号のものにセットする
 */
extern void CPanelSetCurrent(
#if NeedFunctionPrototypes
	Widget		/* w */,
	int		/* current */
#endif
);

/*
 * void CPanelMoveCurrent(Widget w, int dir)
 *	カレントアイテムを dir で指定した方向のアイテムに変える
 *	dir の値は ICtypes.h に定義されている
 */
extern void CPanelMoveCurrent(
#if NeedFunctionPrototypes
	Widget		/* w */,
	int		/* dir */
#endif
);

#endif
