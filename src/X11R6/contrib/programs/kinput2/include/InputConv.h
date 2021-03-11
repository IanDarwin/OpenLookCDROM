/* $Id: InputConv.h,v 1.15 1992/12/08 04:26:49 ishisone Rel $ */
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

#ifndef _InputConv_h
#define _InputConv_h

#include "ICtypes.h"
#include <X11/Object.h>

/*
  InputConv new resources:

  name			class		type		default		access
  ----------------------------------------------------------------------------
  selectionControl	Callback	Callback	NULL		C
  textChangeNotify	Callback	Callback	NULL		C
  modeChangeNotify	Callback	Callback	NULL		C
  endNotify		Callback	Callback	NULL		C
  fixNotify		Callback	Callback	NULL		C
  auxControl		Callback	Callback	NULL		C
  displayObjectClass	Class		Pointer		NULL		CG
*/

/* new resources */
#define XtNselectionControl "selectionControl"
#define XtNtextChangeNotify "textChangeNotify"
#define XtNmodeChangeNotify "modeChangeNotify"
#define XtNfixNotify "fixNotify"
#define XtNendNotify "endNotify"
#define XtNauxControl "auxControl"
#define XtNdisplayObjectClass "displayObjectClass"
#define XtCClass "Class"

typedef struct _InputConvClassRec *InputConvObjectClass;
typedef struct _InputConvRec *InputConvObject;

extern WidgetClass inputConvObjectClass;


/*
 * XtNselectionControl コールバックの call_data 構造体
 */

typedef struct {

    int command;	/* コマンド
			 *	Start: 項目選択開始リクエスト
			 *	End: 項目選択終了リクエスト
			 *	Set: カレント項目設定リクエスト
			 *	Move: カレント項目移動リクエスト
			 */
#define ICSelectionStart	1
#define ICSelectionEnd		2
#define ICSelectionSet		3
#define ICSelectionMove		4

    union {
	int selection_kind;	/* ICSelectionStart の時 */
#define ICSelectionCandidates	1
#define ICSelectionSymbols	2

	int current_item;	/* ICSelectionSet/ICSelectionEnd の時 */
				/* ICSelectionEnd の場合はコールバック
				 * 関数側で現在のカレント候補番号を代入する
				 * (カレントがなければ -1 を代入する)
				 */

	int dir;		/* ICSelectionMove の時 */
    } u;

} ICSelectionControlArg;


/*
 * ICCompareSegment で返される値
 */

#define ICSame		0	/* 全く同じ */
#define ICAttrChanged	1	/* attr のみ異なる */
#define ICStringChanged	2	/* data が異なる */
				/* attribute と data ともに異なれば
				 * (ICAttrChanged | ICStringChanged) が返る
				 */


/*
 * XtNauxControl コールバックの call_data 構造体
 */

typedef struct {

    int command;	/* コマンド
			 *	Start: 項目選択開始リクエスト
			 *	End: 項目選択終了リクエスト
			 *	Change: カレント項目設定リクエスト
			 */
#define ICAuxStart		1
#define ICAuxEnd		2
#define ICAuxChange		3

} ICAuxControlArg;

/*
 * パブリックインターフェイスファンクション
 */

/*
 * Boolean ICSupportMultipleObjects(WidgetClass objectclass)
 *	指定されたオブジェクトクラスから複数のインスタンスが生成できるか
 *	どうかを返す
 *	True なら可能だし、False ならただ一つのインスタンスしか生成できない
 *	引数が Widget ではなく WidgetClass であることに注意
 */
extern Boolean ICSupportMultipleObjects(
#if NeedFunctionPrototypes
	WidgetClass	/* objectclass */
#endif
);

/*
 * int ICInputEvent(Widget ojbect, XEvent *event)
 *	オブジェクトに入力イベントを与える
 *	エラーが起こった場合は -1 が返る
 *	エラーでなければ 0 か 1 が返る
 *	通常は 0 が返るが、もし 1 が返ってきたら、それはできるならば
 *	その入力イベントをアプリケーションに送り返した方がよいという
 *	オブジェクトからのヒントである
 */
extern int ICInputEvent(
#if NeedFunctionPrototypes
	Widget		/* object */,
	XEvent *	/* event */
#endif
);

/*
 * ICString *ICGetMode(Widget object)
 *	現在の入力モードを ICString の形式で返す
 *	データは InputConvObject のものなので勝手に値を変更したり
 *	free() してはならない
 */
extern ICString *ICGetMode(
#if NeedFunctionPrototypes
	Widget		/* object */
#endif
);

/*
 * int ICCursorPos(Widget object, Cardinal *segidx, Cardinal *offset)
 *	現在のインサートカーソル位置を返す
 *	現在カーソルがない時には 0 ある時には 1 が返る
 *	1 が返された時には、segidx にカーソルのあるセグメント番号、
 *	offset にセグメント内の位置(先頭から何文字目か) が返される
 */
extern int ICCursorPos(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Cardinal *	/* segidx */,
	Cardinal *	/* offset */
#endif
);

/*
 * int ICNumSegments(Widget object)
 *	セグメント数を返す
 */
extern int ICNumSegments(
#if NeedFunctionPrototypes
	Widget		/* object */
#endif
);

/*
 * ICString *ICGetSegment(Widget object, Cardinal n)
 *	n 番目のセグメントを返す (先頭のセグメントは n=0)
 *	指定されたセグメントが存在しなければ NULL を返す
 *	データは InputConvObject のものなので勝手に値を変更したり
 *	free() してはならない
 */
extern ICString *ICGetSegment(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Cardinal	/* n */
#endif
);

/*
 * int ICCompareSegment(Widget object, ICString *seg1, ICString *seg2,
 *			Cardinal *nchar)
 *	二つのセグメントを比較する
 *	比較結果は関数の値として返される
 *	nchar にセグメントの先頭からの一致する文字数が返される
 */
extern int ICCompareSegment(
#if NeedFunctionPrototypes
	Widget		/* object */,
	ICString *	/* seg1 */,
	ICString *	/* seg2 */,
	Cardinal *	/* nchar */
#endif
);

/*
 * ICString *ICGetItemList(Widget object, Cardinal *num_items)
 *	選択項目のリストを返す
 *	項目選択中でなければ NULL を返す
 *	num_items に項目数が返される
 *	データは InputConvObject のものなので勝手に値を変更したり
 *	free() してはならない
 *	この関数の返すリストは選択中 (ICSelectionStart のコールバックから
 *	ICSelectionEnd のコールバックまたは ICSelectItem() がコールされるまで)
 *	有効である
 */
extern ICString *ICGetItemList(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Cardinal *	/* num_items */
#endif
);

/*
 * int ICSelectItem(Widget object, int n)
 *	n 番目の項目が選択されたことをオブジェクトに知らせる
 *	n < 0 ならどの項目も選択されなかったことを示す
 *	項目選択中でなかったり指定した項目が存在しなければ -1 を返す
 *	それ以外は 0 を返す
 *	XtNselectionControl コールバック中でこの関数を呼ぶ必要はない
 *	(ICSelectionControlArg.u.current_item にカレントの項目番号を
 *	代入するだけでよい)
 */
extern int ICSelectItem(
#if NeedFunctionPrototypes
	Widget		/* object */,
	int		/* n */
#endif
);

/*
 * int ICGetConvertedString(Widget object, Atom *encoding, int *format,
 *			    int *length, XtPointer *string)
 *	変換されたテキストを string に返す
 *	encoding には、テキストのエンコーディングを指定しておく
 *	ただしこれは単なるリクエストであって、変換オブジェクトは
 *	別のエンコーディングで返してもよい
 *	encoding には実際のエンコーディングが返される
 *	変換オブジェクトは少なくとも COMPOUND_TEXT エンコーディングは
 *	サポートしなくてはならない
 *	format には 8/16/32 のいずれか、length は string のエレメント数が
 *	それぞれ返される
 *	テキストの領域は malloc されているのでこの関数を呼んだ側で
 *	free しなければならない
 *	変換テキストがない時やエラーの場合には -1、そうでなければ 0 が
 *	関数の値として返される
 *
 *	この関数は XtNfixNotify コールバックの中で使われることを想定している
 */
extern int ICGetConvertedString(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Atom *		/* encoding */,
	int *		/* format */,
	int *		/* length */,
	XtPointer *	/* string */
#endif
);

/*
 * int ICClearConversion(Widget object)
 *	強制的に (変換途中であっても) 変換テキストをクリアする
 *	何らかの理由でクリアできなかった時には -1、そうでなければ 0 が
 *	返される
 */
extern int ICClearConversion(
#if NeedFunctionPrototypes
	Widget		/* object */
#endif
);

/*
 * ICString *ICGetAuxSegments(Widget object, Cardinal *n,
 *                            Cardinal *ns, Cardinal *nc)
 *	AUX領域のセグメントを返す。
 *	指定されたセグメントが存在しなければ NULL を返す
 *	データは InputConvObject のものなので勝手に値を変更したり
 *	free() してはならない
 *      得られたセグメント数、カレントセグメント、カレントセグメント
 *      内のカーソルポジションがそれぞれ n, ns, nc にて返る。
 */
extern ICString *ICGetAuxSegments(
#if NeedFunctionPrototypes
	Widget		/* object */,
	Cardinal *	/* n */,
	Cardinal *	/* ns */,
	Cardinal *	/* nc */
#endif
);

#endif /* _InputConv_h */
