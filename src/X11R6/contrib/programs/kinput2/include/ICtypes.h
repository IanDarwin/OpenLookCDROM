/* $Id: ICtypes.h,v 1.2 1990/12/07 15:29:26 ishisone Rel $ */
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

#ifndef _ICtypes_h
#define _ICtypes_h

/*
 * アトリビュートつき文字列型 -- ICString
 *	nbytes は、ICString の data のデータ型を知らなくても
 *	コピーできるようにつけてある
 */

typedef struct {
    unsigned short nchars;	/* data の文字数 */
    unsigned short nbytes;	/* data のバイト数 */
    char *data;			/* 文字列 (エンコーディングはクラス依存) */
    int attr;			/* 文字列の属性 */
} ICString;


/*
 * ICString.attr の値
 */

/* 1. 文字列が変換途中のテキストセグメントを表している時
 * (下記の値の bitwise-or)
 */
#define ICAttrNotConverted	0	/* まだ変換されていないセグメント */
#define ICAttrConverted		1	/* 変換済みセグメント */
#define ICAttrCurrentSegment	2	/* 現在注目しているセグメント */
#define ICAttrCurrentSubSegment 4	/* 現在注目しているサブセグメント */

/* 2. それ以外、例えば現在の入力モードなどを表している時 (常にこの値) */
#define ICAttrNormalString	(-1)


/* 選択の移動方向 */
#define ICMoveLeft	1
#define ICMoveRight	2
#define ICMoveUp	3
#define ICMoveDown	4
#define ICMoveLeftMost	5
#define ICMoveRightMost	6
#define ICMoveNextPage	7
#define ICMovePrevPage	8
#define ICMoveFirst	9
#define ICMoveLast	10

#endif /* _ICtypes_h */
