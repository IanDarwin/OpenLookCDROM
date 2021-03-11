/* $Id: ConvProto.h,v 1.5 1991/10/24 04:12:41 ishisone Rel $ */
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

#ifndef _ConvProto_h
#define _ConvProto_h

/* 変換サーバのプロファイルが入るプロパティ名 */
#define CONVERSION_PROFILE	"_CONVERSION_PROFILE"

/* 変換サーバのプロファイルのプロパティと変換属性のプロパティのタイプ */
#define CONVERSION_ATTRIBUTE_TYPE	"_CONVERSION_ATTRIBUTE_TYPE"

/* プロトコルバージョン名 */
#define PROTOCOL_VERSION	"PROTOCOL-2.0"

#define CONV_ATTR(code,len)	((unsigned long)((code)<<16)+(len))

#define UPPER16U(data)		(((data)>>16)&0xffff)
#define UPPER16S(data)		((short)(((data)>>16)&0xffff))
#define LOWER16U(data)		((data)&0xffff)
#define LOWER16S(data)		((short)((data)&0xffff))

#define CODE_OF_ATTR(head)	UPPER16U(head)
#define LENGTH_OF_ATTR(head)	LOWER16U(head)

/*
 * Conversion Profile Codes
 */

#define CONVPROF_PROTOCOL_VERSION	1
#define CONVPROF_SUPPORTED_STYLES	2
#define CONVPROF_SUPPORTED_EXTENSIONS	3
#define CONVPROF_EXTENSION_DATA		4

/*
 * Standard Conversion Attribute Codes (0-255)
 */

/* 0-127: can be specified at any time (startup and during conversion) */
#define CONVATTR_NONE			0
#define CONVATTR_INDIRECT		1
#define CONVATTR_FOCUS_WINDOW		2
#define CONVATTR_SPOT_LOCATION		3
#define CONVATTR_CLIENT_AREA		4
#define CONVATTR_STATUS_AREA		5
#define CONVATTR_COLORMAP		6
#define CONVATTR_COLOR			7
#define CONVATTR_BACKGROUND_PIXMAP	8
#define CONVATTR_LINE_SPACING		9
#define CONVATTR_FONT_ATOMS		10
#define CONVATTR_CURSOR			11

/* 128-255: can be specified only at startup time */
#define CONVATTR_INPUT_STYLE		128
#define CONVATTR_EVENT_CAPTURE_METHOD	129
#define CONVATTR_USE_EXTENSION		255

/* argument for CONVATTR_INPUT_STYLE and CONVPROP_SUPPORTED_STYLES */
#define CONVARG_ROOTWINDOW		1L
#define CONVARG_OFFTHESPOT		2L
#define CONVARG_OVERTHESPOT		4L

/* argument for CONVATTR_EVENT_CAPTURE_METHOD */
#define CONVARG_NONE			0L
#define CONVARG_CREATE_INPUTONLY	1L
#define CONVARG_SELECT_FOCUS_WINDOW	2L

/*
 * プロファイルデータ / 変換属性データの表現方法
 *
 * 変換サーバの特性を表すプロファイルデータと、変換に関する属性を指定す
 * る変換属性データは共通のフォーマットを用いる。
 *
 * 個々のデータは 32bit値の配列で表現される。最初の 1ワードはヘッダで、
 * それに 0ワード以上のデータが続く。ヘッダの上位 16bit はそのプロファイ
 * ル / 変換属性のコードを表し、下位 16 bit は続くデータのワード数 
 * (32bit 単位) を表す。
 *
 *	+----------------+----------------+
 *	|  Code (16bit)  | Length (16bit) |
 *	+----------------+----------------+
 *	|              Data0              |
 *	+---------------------------------+
 *	|              .....              |
 *	+---------------------------------+
 *	|              DataN              |
 *	+---------------------------------+
 *
 * 実際のプロファイルデータや変換属性データはこのデータがいくつか連続し
 * たものである。
 */

/*
 * プロファイルデータ
 *
 * プロファイルデータ用のコードは次の 4種類が定義されている。変換属性
 * データと異なり、プライベート用のコード領域などは用意されていない。
 *
 * Protocol Version
 *	code: 1
 *	data-length: 1
 *	data[0]:
 *		CARD32: protocol version atom ("PROTOCOL-2.0")
 *
 *	データは変換サーバのプロトコルバージョンを表すアトムである。ここ
 *	で定義されているプロトコルのバージョンは "PROTOCOL-2.0" である。
 *
 * Supported Styles
 *	code: 2
 *	data-length: 1
 *	data[0]:
 *		CARD32: input styles
 *
 *	データは変換サーバがサポートする入力スタイルを表す。サポートする
 *	入力スタイルの値の bitwise-or である。
 *
 * Supported Extensions
 *	code: 3
 *	data-length: N
 *	data[0]:
 *		CARD32: extension atom 1 (Atom)
 *	...
 *	data[N-1]:
 *		CARD32: extension atom N (Atom)
 *
 *	データは変換サーバがサポートする拡張を表すアトムのリストである。
 *
 * Extension Data
 *	code: 4
 *	data-length: N
 *	data[0]:
 *		CARD32: extension atom (Atom)
 *	data[1] - data[N-1]:
 *		extension specific data
 *
 *	データは拡張独自に定義したプロファイルデータである。標準プロトコ
 *	ルとしてはデータの先頭に拡張アトム (これはSupported Extensions 
 *	に指定されたものでなければならない)をつけることを規定するだけで、
 *	その後のデータに関しては一切規定しない。
 *
 * クライアント側の無用の混乱を防ぐため、Protocol Version と Supported
 * Stylesの項目は必ずなければならない。また、Extension Data 以外はプロファ
 * イルデータの中に同じコードのデータが複数あってはならない。
 */

/*
 * 変換属性データ
 *
 * 属性コードのうち、0 から 255 までは標準プロトコルが使用するもので、現
 * 在属性が割り振られていないからといって勝手に使用してはならない。その
 * ような目的のため属性コード 256 から 65535 がプライベートコード拡張領
 * 域として用意されている。ただしこの領域の使用に当たってはあらかじめそ
 * の拡張コードを使用することを Use Extension (下記参照) を用いてあらか
 * じめ宣言する必要がある。
 *
 * 属性データの指定方法には、変換開始時に指定する方法と、変換中に指定す
 * る方法の 2通りがあるが、属性コードによっては変換開始時にしか指定でき
 * ないものがある。そこで、0-255 の標準コードのうち、0 から 127 までは変
 * 換開始時でも変換中でも指定できるもの、128 から 255 までは変換開始時に
 * しか指定できないもの、に分けてある。拡張コードについては特にこのよう
 * な区別は定めない。
 *
 * このプロトコルで定義される属性コードは次の通りである。
 *
 * -- 変換開始時にも、変換途中にも指定できるもの --
 *
 * No Operation:
 *	code: 0
 *	data-length: N (could be 0)
 *	data: anything
 *
 *	何もしない。プロパティのある部分をスキップさせるのに便利。
 *	
 * Indirect Attribute:
 *	code: 1
 *	data-length: 1
 *	data[0]:
 *		CARD32: property name (Atom)
 *
 *	指定されたプロパティに従って属性を設定する。CONVERSION_ATTRIBUTE
 * 	イベントで複数の属性データを設定したい時や、イベントに属性データが
 *	入り切らない時に使用する。
 *
 * Focus Window:
 *	code: 2
 *	data-length: 1
 *	data[0]:
 *		CARD32: focus window (Window)
 *
 *	フォーカスウィンドウを指定する。
 *
 * Spot Location:
 *	data-length: 1
 *	data[0]:
 *		INT16(upper 16bit): X
 *		INT16(lower 16bit): Y
 *
 *	スポットロケーションを指定する。ベースラインの開始点で指定する。
 *
 * Client Area:
 *	data-length: 2
 *	data[0]:
 *		INT16(upper 16bit): X
 *		INT16(lower 16bit): Y
 *	data[1]:
 *		CARD16(upper 16bit): Width
 *		CARD16(lower 16bit): Height
 *
 *	変換テキスト表示領域を指定する。
 *
 * Status Area:
 *	data-length: 2
 *	data[0]:
 *		INT16(upper 16bit): X
 *		INT16(lower 16bit): Y
 *	data[1]:
 *		CARD16(upper 16bit): Width
 *		CARD16(lower 16bit): Height
 *
 *	ステータス表示領域を指定する。
 *
 * Colormap:
 *	data-length: 1
 *	data[0]:
 *		CARD32: colormap (XID)
 *
 *	カラーマップ ID を指定する。
 *
 * Color:
 *	data-length: 2
 *	data[0]:
 *		CARD32: foreground pixel
 *	data[1]:
 *		CARD32: background pixel
 *
 *	フォアグラウンドとバックグラウンドのピクセル値を指定する。
 *
 * Background Pixmap:
 *	data-length: 1
 *	data[0]:
 *		CARD32: background pixmap (Pixmap)
 *
 *	バックグラウンドの Pixmap ID を指定する。
 *
 * Line Spacing:
 *	data-length: 1
 *	data[0]:
 *		CARD32: line spacing
 *
 *	行間を指定する。ベースライン間の距離で指定する。
 *
 * Font Atoms:
 *	data-length: N (>0)
 *	data[0]:
 *		CARD32: font atom 1 (Atom)
 *	...
 *	data[N-1]:
 *		CARD32: font atom N (Atom)
 *
 *	使用するフォントの "FONT" アトムのリストを指定する。
 *
 * Cursor:
 *	data-length: 1
 *	data[0]:
 *		CARD32: cursor (Cursor)
 *
 *	カーソル ID を指定する。
 *
 * -- 変換開始時のみ指定できるもの --
 *
 * Input Style:
 *	data-length: 1
 *	data[0]:
 *		CARD32: input style
 *
 *	入力方法を指定する。
 *	デフォルトは Root Window Style である。
 *
 * Event Capture Method:
 *	data-length: 1
 *	data[0]:
 *		CARD32: event capture method
 *
 *	クライアントウィンドウからのイベントの取得方法を指定する。デフォ
 *	ルトはクライアントウィンドウの前に InputOnly ウィンドウを作って
 *	そのキーイベントをセレクトするというものである。他の方法としては、
 *	フォーカスウィンドウ (フォーカスウィンドウが指定されていなければ
 *	クライアントウィンドウ) のキーイベントを直接セレクトする (この場
 *	合、変換中はクライアントはキーイベントを無視しなくてはならない) 
 *	ものと、何もしない、つまり変換中のクライアントはキーイベントをフ
 *	ロントエンドに SendEvent しなくてはならない、という方法がある。
 *
 * Use Extension:
 *	data-length: N
 *	data[0]:
 *		CARD32: extension atom 1 (Atom)
 *	...
 *	data[N-1]:
 *		CARD32: extension atom N (Atom)
 *
 *	この属性設定で使用される拡張を指定する。ここで指定する拡張はサー
 *	バがサポートしているもの、つまりプロファイルデータ中のSupported
 *	Extensions に書かれた拡張でなければならない。
 */

#endif
