/* $Id: ConvDisp.h,v 1.8 1991/03/29 15:38:14 ishisone Rel $ */
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

#ifndef _ConvDisplay_h
#define _ConvDisplay_h

#include <X11/Object.h>
#include "ICtypes.h"

/*
  ConvDisplay new resources:

  name		class		type		default			access
  ----------------------------------------------------------------------------
  foreground	Foreground	Pixel		DefaultForeground	CSG
  background	Background	Pixel		DefaultBackground	CSG
  cursorBitmap	CursorBitmap	Bitmap		*1			CSG
  hotX		HotX		Position	3			CSG
  hotY		HotY		Position	2			CSG

  note:	*1) default is a small caret

*/

#define XtNcursorBitmap "cursorBitmap"
#define XtCCursorBitmap "CursorBitmap"
#define XtNhotX "hotX"
#define XtNhotY "hotY"
#define XtCHotX "HotX"
#define XtCHotY "HotY"

typedef struct _ConvDisplayClassRec	*ConvDisplayObjectClass;
typedef struct _ConvDisplayRec		*ConvDisplayObject;

extern WidgetClass	convDisplayObjectClass;

/*
 * public interface functions
 */

/*
 * int CDStringWidth(Widget object, ICString *str, int start, int end)
 *	指定された文字列の start 文字目から end - 1 文字目までの
 *	幅 (単位はピクセル) を返す
 *	end が負の時は文字列の最後までを表す
 */
extern int CDStringWidth(
#if NeedFunctionPrototypes
	Widget		/* object */,
	ICString *	/* str */,
	int		/* start */,
	int		/* end */
#endif
);

/*
 * int CDLineHeight(Widget object, Position *ascentp)
 *	行の高さ (単位はピクセル) を返す
 *	ascentp が NULL でない時には ascent が返される
 */
extern int CDLineHeight(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Position *	/* ascentp */
#endif
);

/*
 * void CDDrawString(Widget object, Widget canvas, ICString *str,
 *		      int start, int end, int x, int y)
 *	canvas で指定される widget の座標 (x, y) から、指定された
 *	文字列の start 文字目から end - 1 文字目までを書く
 *	end が負の時は文字列の最後までを表す
 *	ただし、(x, y) は文字列の左上の座標である
 */
extern void CDDrawString(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Widget		/* canvas */,
	ICString *	/* str */,
	int		/* start */,
	int		/* end */,
	int		/* x */,
	int		/* y */
#endif
);

/*
 * int CDMaxChar(Widget object, ICString *str, int start, int width)
 *	指定された文字列の start 文字目から、ピクセル幅 width に
 *	入る文字数を返す
 */
extern int CDMaxChar(
#if NeedFunctionPrototypes
	Widget		/* object */,
	ICString *	/* str */,
	int		/* start */,
	int		/* end */
#endif
);

/*
 * void CDDrawCursor(Widget object, Widget canvas, int x, int y, int on)
 *	canvas で指定される widget の座標 (x, y) にインサートカーソルを
 *	表示する
 *	on が False の時にはカーソルを消す
 *	指定する y 座標にはフォントのベースラインを指定する
 */
extern void CDDrawCursor(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Widget		/* canvas */,
	int		/* x */,
	int		/* y */,
	int		/* on */			 
#endif
);

/*
 * void CDGetCursorBounds(Widget object, XRectangle *bounds)
 *	表示されるカーソルの表示領域を bounds に返す
 *	座標 (x, y) にカーソルを書いた時の表示領域は
 *		(x + bounds->x, y + bounds->y): 左上座標
 *		(bounds->width, bounds->height): 領域の大きさ
 *	になる
 */
extern void CDGetCursorBounds(
#if NeedFunctionPrototypes
	Widget		/* object */,
	XRectangle *	/* bounds */
#endif
);


/*
 * void CDSetFonts(Widget object, XFontStruct **fontset, Cardinal num_fonts)
 *		(note: fontset is an array of (XFontStruct *).)
 *	描画に fontset で指定されるフォントを使用するように設定する
 *	オブジェクトは fontset の中から自分の使用するキャラクタセットの
 *	フォントを選んで設定する
 *	もし、必要なキャラクタセットのフォントがなければ適当なフォントを
 *	設定する
 *	これは他のキャラクタセットのフォントに合わせて選ぶのがよいと
 *	思われるが、デフォルトフォントで間に合わせてもよい
 *	もし num_fonts が 0 であれば各オブジェクトのデフォルトフォントを
 *	使用するように設定される
 */
extern void CDSetFonts(
#if NeedFunctionPrototypes
	Widget		/* object */,
	XFontStruct **	/* fontset */,
	Cardinal	/* num_fonts */
#endif
);

/*
 * void CDSetBlockCursor(Widget object, XRectangle *shape)
 *	shape で指定されるブロックカーソルを設定する
 *	shape の指定は CDGetCursorBounds() の bounds と同じである
 */
extern void CDSetBlockCursor(
#if NeedFunctionPrototypes
	Widget		/* object */,
	XRectangle *	/* shape */
#endif
);

#endif /* _ConvDisplay_h */
