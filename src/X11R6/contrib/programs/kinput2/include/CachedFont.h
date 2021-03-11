/* $Id: CachedFont.h,v 1.2 1991/09/23 04:09:05 ishisone Rel $ */
/*
 * Copyright (c) 1991  Software Research Associates, Inc.
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

#ifndef _CachedFont_h
#define _CachedFont_h

/*
 * XFontStruct *CachedLoadQueryFontByName(Display *dpy, String name)
 *	XLoadQueryFont() とほぼ同じだが、指定されたフォントが
 *	すでにオープンされていればそれを返す。 
 */
extern XFontStruct *CachedLoadQueryFontByName(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	String		/* name */
#endif
);

/*
 * XFontStruct *CachedLoadQueryFontByProp(Display *dpy, Atom atom)
 *	CachedLoadQueryFontByName() とほぼ同じだが、フォント名ではなく、
 *	それをアトムにしたもの (つまりフォントの "FONT" プロパティの値)
 *	で指定する。
 *	これはこのアトムの値 (およびこのアトムに対応する文字列である正式な
 *	フォント名) が唯一フォントの実体に対して unique な値だと考えられる
 *	からである。
 */
extern XFontStruct *CachedLoadQueryFontByProp(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	Atom		/* atom */
#endif
);

/*
 * XFontStruct *CachedLoadFontByFontStruct(Display *dpy, XFontStruct *font)
 *	CachedFreeFont() で一度使用しなくなったフォントを再びロードするのに
 *	用いる。すでにロードされていればそのフォントのリファレンスカウントを
 *	増やすだけである。
 *	もし指定された font が CachedLoadQueryFontByName() または
 *	CachedLoadQueryFontByProp() を使用して作られたものではない時には
 *	NULL が返る。そうでない時には font の値がそのまま返される。
 */
extern XFontStruct *CachedLoadFontByFontStruct(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	XFontStruct *	/* font */
#endif
);

/*
 * void CachedFreeFont(Display *dpy, XFontStruct *font)
 *	CachedLoadQueryFontByName() または CachedLoadQueryFontByProp() で
 *	得られたフォントを使用しなくなった時に呼ぶ。
 *	XFreeFont() とほぼ同じだが、フォント情報をキャッシュしている
 *	関係上、指定されたフォントへの参照がある限りフォントは
 *	クローズされない。
 *	また、フォント情報 (XFontStruct 内の情報のうち、fid を除くもの)
 *	はたとえフォントへの参照がなくなっても保持される。フォント構造体
 *	もそのまま保持されるので、いったん CachedFreeFont() したフォントを
 *	CachedLoadFontByFontStruct() で再びロードすることもできる。
 */
extern void CachedFreeFont(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	XFontStruct *	/* font */
#endif
);

#endif
