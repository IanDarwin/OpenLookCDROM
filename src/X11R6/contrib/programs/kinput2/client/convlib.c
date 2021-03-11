/*
 *	convlib2.c -- X11 ツールキット変換入力用ライブラリ
 *
 *		ishisone@sra.co.jp
 */

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

/*
 * --インターフェイス--
 *
 * 用意されているファンクションは次の4つ (そのうち1つは backward
 * compatibility のため) だけである。残念ながら X11R5 で採り入れられる標
 * 準入力インターフェイス XIM には従っていない。しかし Xt ツールキットを
 * 利用するプログラムなら XIM よりもはるかに簡単に組み込むことができるだ
 * ろう。
 *
 * int _beginConversionWithAttributes(
 *	Widget w,		変換入力をしたい widget
 *	Atom catom,		セレクションアトム eg "_JAPANESE_CONVERSION"
 *	Atom tatom,		変換テキストタイプ eg "COMPOUND_TEXT"
 *	void (*inputproc)(),	変換テキスト入力コールバック関数
 *	void (*startendproc)(),	変換開始 / 終了 / アボート コールバック関数
 *	XtPointer client_data,	コールバック関数に渡されるデータ
 *	ArgList attrs,		変換属性リスト
 *	Cardinal nattrs		属性リストの項目数
 *	)
 *
 *	変換を開始する。もっと正確にいうと、引数 catom に指定されたセレ
 *	クションアトムのオーナー (これが変換フロントエンド、例えば
 *	kinput2 である) を探し、変換のリクエストを送る。同時に引数 attrs
 *	で指定される変換属性 (例えばカーソルの位置) をフロントエンドに通
 *	知する。変換属性については後で別に説明する。フロントエンドが存在
 *	しない時には -1 を、それ以外は 0 を返す。
 *
 *	startendproc は変換の状態の変化をアプリケーションに知らせるため
 *	のコールバックである。このファンクションは次のような形式で呼び出
 *	される。
 *
 *	(*startendproc)(Widget w, Atom catom, int state,
 *			XtPointer client_data, Window convwin)
 *
 *	w, catom, client_data は _beginConversionWithAttributes() で指定
 *	したものと同じものが渡される。state には3種類あり、それぞれ
 *		 0: フロントエンドが変換リクエストを受け付けた
 *		-1: フロントエンドが変換リクエストを拒否した
 *		 1: 変換が終了した
 *	ということを表す。state が 0 の時、convwin には実際に変換処理
 *	が行なわれるウィンドウの ID が入る。これは eventCaptureMethod 
 *	を "none" にしてクライアントに来たキーイベントをフロントエンド
 *	にフォワードする時などに使用される。
 *
 *	再び _beginConversionWithAttributes() の引数の説明に戻ってtatom 
 *	はフロントエンドから送られてくる文字列のエンコーディングの指定で
 *	ある。
 *	kinput2 プロトコルでは、フロントエンドに対して COMPOUND_TEXTエン
 *	コーディングをサポートすることしか求めていないので、他のエンコー
 *	ディングをサポートするかどうかはフロントエンドのインプリメンテー
 *	ションに依存する。従って COMPOUNT_TEXT 以外のエンコーディングを
 *	指定すると、実際に送られてくる文字列のエンコーディングが指定した
 *	ものと異なることもあり得るので注意すること。
 *
 *	確定文字列が変換フロントエンドから送られてくると inputproc に指
 *	定されたコールバックファンクションが次のような形式で呼ばれる。
 *
 *	(*inputproc)(Widget w, Atom catom,
 *			Atom proptype, int propformat,
 *			unsigned long propsize, unsigned char *propvalue,
 *			XtPointer client_data)
 *
 *	w と catom、それに client_data は 
 *	_beginConversionWithAttributes() で指定したものである。
 *
 *	proptype は文字列のエンコーディング、propformat は1文字のビット
 *	長、propsize が長さ、そして propvalue に実際の文字列データが入っ
 *	ている。実は文字列はフロントエンドから X のウィンドウプロパティ
 *	として渡されており、これらのパラメータはそのプロパティのパラメー
 *	タそのものである。従って XGetWindowProperty() の説明を参照すると
 *	各パラメータの意味がはっきりわかるだろう。
 *
 * void _changeConversionAttributes(
 *	Widget	w,
 *	Atom catom,		セレクションアトム eg "_JAPANESE_CONVERSION"
 *	ArgList attrs,		変換属性リスト
 *	Cardinal nattrs		属性リストの項目数
 *	)
 *
 *	変換中に変換属性を変化させる。例えばカーソル位置が変わったときに
 *	このファンクションでそれをフロントエンドに知らせることができる。
 *
 * void _endConversion(
 *	Widget w,
 *	Atom catom,		セレクションアトム eg "_JAPANESE_CONVERSION"
 *	Boolean throwaway	この後来た変換結果を受けとるかどうか
 *	)
 *
 *	変換を終了させる。通常、変換の終了は変換フロントエンドが行なうの
 *	で特にアプリケーションからこのファンクションを使って強制的に終了
 *	させる必要はない。
 *	引数 throwaway が True だとこのファンクションを実行した後にフロ
 *	ントエンドから送られた文字列を無視する。
 *
 * void _beginConversion(	-- provided for backward compatibility
 *	Widgete w,		変換入力をしたい widget
 *	Atom catom,		セレクションアトム eg "_JAPANESE_CONVERSION"
 *	Atom tatom,		変換テキストタイプ eg "COMPOUND_TEXT"
 *	void (*inputproc)(),	変換テキスト入力コールバック関数
 *	XtPointer client_data	コールバック関数に渡されるデータ
 *	)
 *
 *	これは backward compatibility のために用意されたファンクションで
 *	ある。このファンクションでは変換属性の指定が一切できないし、変換
 *	状態の変化も知ることができないので、できるだけ
 *	_beginConversionWithAttributes() を用いるのが望ましい。
 *
 *
 * --セレクションアトム--
 *
 * _beginConversionWithAttributes() などに指定するセレクションアトムは
 * 入力したい言語によって異なり、次のような名前になっている。
 *	"_<言語名>_CONVERSION"
 * 例えば日本語の場合は "_JAPANESE_CONVERSION" である。
 *
 *
 * --変換属性リスト--
 *
 * 変換属性は XtSetValues() などで使用されるリストと同じ形 (ArgList) である。
 * 属性として指定できるのは次の項目である。基本的にこれらの属性は XIM の
 * 仕様での定義をそのまま採用しているので、疑問点があれば XIM のドキュメントを
 * 参照してほしい。
 *
 * "inputStyle" : 値 String
 *	入力スタイルを指定する。値は
 *		"root" (root window style):	別ウィンドウによる変換
 *		"off" (off-the-spot style):	指定した変換領域内での変換
 *		"over" (over-the-spot style):	その場変換
 *	のどれかの文字列を指定する。
 *
 * "focusWindow" : 値 Window
 *	変換を行なうウィンドウを指定する。これが指定されたなかった場合に
 *	は _beginConversionWithAttributes() で指定した Widget のウィンド
 *	ウが使われるので通常は指定しなくてよい。
 *
 * "spotX", "spotY" : 値 Position
 *	スポットロケーションを指定する。これは入力スタイルが 
 *	over-the-spot の時のみ有効である。文字を書き始める位置を指定する
 *	が、spotY はベースラインの位置であることに注意。
 *	spotX、spotY のうち片方だけ指定しても無効。
 *
 * "foreground", "background" : 値 Pixel
 *	前景色、背景色を指定する。これも片方だけ指定しても無効。
 *
 * "eventCaptureMethod" : 値 String
 *	フロントエンドがどのように入力イベントをとるかを指定する。
 *		"none"		何もしない
 *		"inputOnly"	InputOnly ウィンドウによる
 *		"focusSelect"	フォーカスウィンドウのイベントを直接
 *				セレクトする
 *	である。
 *
 *	"何もしない" を指定した場合、アプリケーションは変換中にアプリケー
 *	ションに来たイベントをフロントエンドに渡して (XSendEvent() を使
 *	う) やらなければならない。このイベントのフォワード作業はこのライ
 *	ブラリではサポートしていない。従って "何もしない" を指定すること
 *	はあまりお勧めしない。
 *
 *	"フォーカスウィンドウのイベントを直接セレクトする" 場合、アプリ
 *	ケーションは変換中は sendevent フラグの立っていないキーイベント
 *	を無視しなくてはならない。sendevent フラグの立ったキーイベントは
 *	フロントエンドから戻されたイベントである可能性があり、これは無視
 *	しなくても良いが、当然のことながらセキュリティには気をつけなくて
 *	はならない。
 *
 *	"InputOnly ウィンドウによる" を指定するとフロントエンドはクライ
 *	アントのウィンドウ (これはフォーカスウィンドウではなく、
 *	_beginConversionWithAttributes() で指定した Widget のウィンドウ
 *	である) の前に透明なウィンドウを作り、そこに来たイベントを横取り
 *	する。この場合イベントはフロントエンドが勝手にとってしまうのでア
 *	プリケーションはイベントに関して何も考えなくてよい。従って方法と
 *	してはこれが一番簡単である。しかしながら例えば click-to-type 形
 *	式のウィンドウマネージャを使ったりしてキー入力のフォーカスが設定
 *	されている場合にはうまくいかない。
 *
 * "lineSpacing" : 値 int
 *	行間隔を指定する。ベースライン間の距離を指定する。
 *
 * "clientArea" : 値 XRectangle へのポインタ
 *	変換テキストの表示に使用する領域を指定する。
 *
 * "statusArea" : 値 XRectangle へのポインタ
 *	モード表示に使用する領域を指定する。
 *
 * "cursor" : 値 Cursor
 *	使用するカーソル (マウスカーソルね) を指定する。
 *
 * "fonts" : 値 NULL ターミネートされた XFontStruct * の配列
 *	使用するフォントを指定する。順番はどうでもよい。フロントエンド側
 *	で判断する。ただし XLFD に従わないフォントを指定されると、フロン
 *	トエンドでそのキャラクタセットがわからず、そのフォントが使われな
 *	いことがある。
 *
 * 上記の属性のうち、inputStyle と eventCaptureMethod は変換途中で (つまり
 * _changeConversionAttributes() で) 変更することはできない。
 */

#ifndef lint
static char	*rcsid = "$Id: convlib2.c,v 1.12 1994/06/03 09:13:19 ishisone Rel $";
#endif

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "ConvProto.h"

typedef struct {
    Display	*display;
    Atom	profileAtom;	/* "_CONVERSION_PROFILE" */
    Atom	typeAtom;	/* "_CONVERSION_ATTRIBUTE_TYPE" */
    Atom	versionAtom;	/* "PROTOCOL-2.0" */
    Atom	reqAtom;	/* "CONVERSION_REQUEST" */
    Atom	notifyAtom;	/* "CONVERSION_NOTIFY" */
    Atom	endAtom;	/* "CONVERSION_END" */
    Atom	endReqAtom;	/* "CONVERSION_END_REQUEST" */
    Atom	attrAtom;	/* "CONVERSION_ATTRIBUTE" */
    Atom	attrNotifyAtom;	/* "CONVERSION_ATTRIBUTE_NOTIFY" */
} ConversionAtoms;

typedef struct {
    Atom	convatom;
    Window	convowner;
    Atom	property;
    void	(*inputproc)();
    void	(*startendproc)();
    XtPointer	closure;
} ConversionContext;

static XContext	convertPrivContext;

#if __STDC__
/* function prototype */
static void callStart(Widget, ConversionContext *, Window);
static void callFail(Widget, ConversionContext *);
static void callEnd(Widget, ConversionContext *);
static long getInputStyle(String);
static long getCaptureMethod(String);
static ConversionAtoms *getAtoms(Widget);
static ConversionContext *getConversionContext(Widget);
static void recvConvAck(Widget, XtPointer, XEvent *, Boolean *);
static void getConv(Widget, XtPointer, XEvent *, Boolean *);
static Boolean setConvAttrProp(Widget, ArgList, Cardinal, Atom);
static int makeAttrData(Widget, ArgList, Cardinal, unsigned long **);
static Boolean checkProtocols(Display *, Window, ConversionAtoms *);
#else
static void callStart();
static void callFail();
static void callEnd();
static long getInputStyle();
static long getCaptureMethod();
static ConversionAtoms *getAtoms();
static ConversionContext *getConversionContext();
static void recvConvAck();
static void getConv();
static Boolean setConvAttrProp();
static int makeAttrData();
static Boolean checkProtocols();
#endif

static void
callStart(w, context, convwin)
Widget w;
ConversionContext *context;
Window convwin;
{
    if (context->startendproc != NULL) {
	(*context->startendproc)(w, context->convatom,
				 0, context->closure, convwin);
    }
}

static void
callFail(w, context)
Widget w;
ConversionContext *context;
{
    if (context->startendproc != NULL) {
	(*context->startendproc)(w, context->convatom,
				 -1, context->closure, None);
    }
}

static void
callEnd(w, context)
Widget w;
ConversionContext *context;
{
    if (context->startendproc != NULL) {
	(*context->startendproc)(w, context->convatom,
				 1, context->closure, None);
    }
}

static long
getInputStyle(s)
String s;
{
    String p;
    char buf[64];

    (void)strcpy(buf, s);
    for (p = buf; *p != '\0'; p++) {
	if ('A' <= *p && *p <= 'Z') *p += 'a' - 'A';
    }
    if (!strcmp(buf, "over")) return CONVARG_OVERTHESPOT;
    if (!strcmp(buf, "off")) return CONVARG_OFFTHESPOT;
    return CONVARG_ROOTWINDOW;
}

static long
getCaptureMethod(s)
String s;
{
    String p;
    char buf[64];

    (void)strcpy(buf, s);
    for (p = buf; *p != '\0'; p++) {
	if ('A' <= *p && *p <= 'Z') *p += 'a' - 'A';
    }
    if (!strcmp(buf, "none")) return CONVARG_NONE;
    if (!strcmp(buf, "focusselect")) return CONVARG_SELECT_FOCUS_WINDOW;
    return CONVARG_CREATE_INPUTONLY;
}

static ConversionAtoms *
getAtoms(w)
Widget	w;
{
    int i;
    Display *disp = XtDisplay(w);
    ConversionAtoms *cap;
    static ConversionAtoms *convatomp;
    static Cardinal ndisp = 0;
#define nalloc	2

    /*
     * アトムはディスプレイごとに違うので、
     * ディスプレイごとに作らなくてはならない
     */

    /* すでにアトムが作られているかどうか調べる */
    cap = convatomp;
    for (i = 0; i < ndisp; i++, cap++) {
	if (cap->display == disp) return cap;
    }

    /*
     * まだ作られていないので新しく作る
     */
    if (ndisp == 0) {
	/* 最初なので Context も同時に作る */
	convertPrivContext = XUniqueContext();
	convatomp = (ConversionAtoms *)
	  XtMalloc(sizeof(ConversionAtoms) * nalloc);
	cap = convatomp;
    } else if (ndisp % nalloc == 0) {
	/* サイズを増やす */
	convatomp = (ConversionAtoms *)
	  XtRealloc((char *)convatomp,
		    sizeof(ConversionAtoms) * (ndisp + nalloc));
	cap = convatomp + ndisp;
    } else {
	cap = convatomp + ndisp;
    }

    /* ディスプレイの登録 */
    cap->display = disp;

    /* Atom の作成 */
    cap->profileAtom = XInternAtom(disp, CONVERSION_PROFILE, False);
    cap->typeAtom = XInternAtom(disp, CONVERSION_ATTRIBUTE_TYPE, False);
    cap->versionAtom = XInternAtom(disp, PROTOCOL_VERSION, False);
    cap->reqAtom = XInternAtom(disp, "CONVERSION_REQUEST", False);
    cap->notifyAtom = XInternAtom(disp, "CONVERSION_NOTIFY", False);
    cap->endAtom = XInternAtom(disp, "CONVERSION_END", False);
    cap->endReqAtom = XInternAtom(disp, "CONVERSION_END_REQUEST", False);
    cap->attrAtom = XInternAtom(disp, "CONVERSION_ATTRIBUTE", False);
    cap->attrNotifyAtom = XInternAtom(disp, "CONVERSION_ATTRIBUTE_NOTIFY", False);

    ndisp++;

    return cap;
}

static ConversionContext *
getConversionContext(w)
Widget	w;
{
    ConversionContext *context;

    if (XFindContext(XtDisplay(w), XtWindow(w),
		     convertPrivContext, (caddr_t *)&context)) {
	/* error -- 多分コンテキストが見つからなかったため */
	return NULL;
    } else {
	return context;
    }
}

/* ARGSUSED */
static void
recvConvAck(w, closure, ev, junk)
Widget	w;
XtPointer	closure;
XEvent	*ev;
Boolean	*junk;	/* NOTUSED */
{
    XClientMessageEvent *cev = &(ev->xclient);
    ConversionAtoms *cap;
    ConversionContext *context;

    if (ev->type != ClientMessage) return;

    cap = getAtoms(w);
    context = getConversionContext(w);

    /* 正しいイベントかどうかチェックする */
    if (cev->window != XtWindow(w) ||
	cev->message_type != cap->notifyAtom ||
	cev->data.l[0] != context->convatom) {
	return;
    }

    /*
     * このハンドラはもう用済みなので外す
     */
    XtRemoveEventHandler(w, NoEventMask, True, recvConvAck, closure);

    if (cev->data.l[2] == None) {
	XtWarning("selection request failed");
	XDeleteContext(XtDisplay(w), XtWindow(w), convertPrivContext);
	callFail(w, context);
	XtFree((char *)context);
	return;
    }

    callStart(w, context, (Window)cev->data.l[3]);

    /*
     * PropertyNotify と CONVERSION_END 用のイベントハンドラを
     * 登録する
     */
    XtAddEventHandler(w, PropertyChangeMask, True, getConv, closure);

    /* プロパティ名をストアする */
    context->property = cev->data.l[2];
}

/* ARGSUSED */
static void
getConv(w, closure, ev, junk)
Widget	w;
XtPointer	closure;
XEvent	*ev;
Boolean	*junk;	/* NOTUSED */
{
    ConversionAtoms *cap;
    ConversionContext *context;

    /* PropertyNotify と ClientMessage 以外は無視する */
    if (ev->type != PropertyNotify && ev->type != ClientMessage) return;

    cap = getAtoms(w);
    context = getConversionContext(w);

    if (ev->type == ClientMessage) {
	XClientMessageEvent *cev = &(ev->xclient);

	/*
	 * 本当に入力終了のイベントかどうかチェックする
	 */
	if (cev->message_type == cap->endAtom &&
	    cev->format == 32 &&
	    cev->data.l[0] == context->convatom) {
	    /* ウィンドウのコンテキストを削除する */
	    XDeleteContext(XtDisplay(w), XtWindow(w),
			   convertPrivContext);
	    /* イベントハンドラを外す */
	    XtRemoveEventHandler(w, PropertyChangeMask, True,
				 getConv, closure);
	    callEnd(w, context);
	    XtFree((char *)context);
	}
    } else {			/* PropertyNotify */
	XPropertyEvent *pev = &(ev->xproperty);
	Atom proptype;
	int propformat;
	unsigned long propsize, rest;
	unsigned char *propvalue;

	if (context->property == None) return;

	/* 正しいイベントかどうかのチェック */
	if (pev->window != XtWindow(w) ||
	    pev->atom != context->property ||
	    pev->state != PropertyNewValue) {
	    return;
	}

	/* もしコールバック関数 context->inputproc が
	 * NULL ならばプロパティを削除するだけ
	 */
	if (context->inputproc == NULL) {
	    XDeleteProperty(XtDisplay(w), XtWindow(w), context->property);
	    return;
	}

	/* プロパティから変換文字列を取り出す */
	XGetWindowProperty(XtDisplay(w), XtWindow(w),
			   context->property,
			   0L, 100000L, True, AnyPropertyType,
			   &proptype, &propformat, &propsize, &rest,
			   &propvalue);

	/* プロパティのタイプ・フォーマットのチェック */
	if (proptype == None) {
	    /* プロパティが存在しなかった
	     * これは連続して何回もプロパティにデータが
	     * 入れられた時、一回の GetWindowProperty で
	     * 複数のデータをとってしまったあとに起きる
	     * 従ってこれはエラーではない
	     */
	    return;
	}

	/* コールバックを呼ぶ */
	(*context->inputproc)(w, context->convatom,
			      proptype, propformat,
			      propsize, propvalue,
			      context->closure);
    }
}

static Boolean
setConvAttrProp(w, attrs, nattrs, prop)
Widget w;
ArgList attrs;
Cardinal nattrs;
Atom prop;
{
    unsigned long *data;
    int len;

    if ((len = makeAttrData(w, attrs, nattrs, &data)) > 0) {
	XChangeProperty(XtDisplay(w), XtWindow(w),
			prop, prop, 32,
			PropModeReplace, (unsigned char *)data, len);
	XtFree((char *)data);
	return True;
    }
    return False;
}

static int
makeAttrData(w, args, nargs, datap)
Widget w;
ArgList args;
Cardinal nargs;
unsigned long **datap;
{
    unsigned long *data;
    Cardinal len;
    Boolean spotx_specified = False, spoty_specified = False;
    Boolean fore_specified = False, back_specified = False;
    Pixel savedfg, savedbg;
    Position savedx, savedy;

#define ALLOC(n) \
    data = (unsigned long *)XtRealloc((char *)data, \
				      sizeof(unsigned long)*(len+(n)))

    data = NULL;
    len = 0;
    while (nargs-- > 0) {
	if (!strcmp(args->name, "spotX")) {
	    savedx = (Position)args->value;
	    spotx_specified = True;
	} else if (!strcmp(args->name, "spotY")) {
	    savedy = (Position)args->value;
	    spoty_specified = True;
	} else if (!strcmp(args->name, "foreground")) {
	    savedfg = (Pixel)args->value;
	    fore_specified = True;
	} else if (!strcmp(args->name, "background")) {
	    savedbg = (Pixel)args->value;
	    back_specified = True;
	} else if (!strcmp(args->name, "focusWindow")) {
	    Window win = (Window)args->value;
	    ALLOC(2);
	    data[len] = CONV_ATTR(CONVATTR_FOCUS_WINDOW, 1);
	    data[len + 1] = (unsigned long)win;
	    len += 2;
	} else if (!strcmp(args->name, "inputStyle")) {
	    long style = getInputStyle((String)args->value);
	    ALLOC(2);
	    data[len] = CONV_ATTR(CONVATTR_INPUT_STYLE, 1);
	    data[len + 1] = style;
	    len += 2;
	} else if (!strcmp(args->name, "eventCaptureMethod")) {
	    long method = getCaptureMethod((String)args->value);
	    ALLOC(2);
	    data[len] = CONV_ATTR(CONVATTR_EVENT_CAPTURE_METHOD, 1);
	    data[len + 1] = method;
	    len += 2;
	} else if (!strcmp(args->name, "lineSpacing")) {
	    int spacing = (int)args->value;
	    ALLOC(2);
	    data[len] = CONV_ATTR(CONVATTR_LINE_SPACING, 1);
	    data[len + 1] = spacing;
	    len += 2;
	} else if (!strcmp(args->name, "clientArea")) {
	    XRectangle *rectp = (XRectangle *)args->value;
	    ALLOC(3);
	    data[len] = CONV_ATTR(CONVATTR_CLIENT_AREA, 2);
	    data[len + 1] = (rectp->x << 16) | (rectp->y & 0xffff);
	    data[len + 2] = (rectp->width << 16) | (rectp->height & 0xffff);
	    len += 3;
	} else if (!strcmp(args->name, "statusArea")) {
	    XRectangle *rectp = (XRectangle *)args->value;
	    ALLOC(3);
	    data[len] = CONV_ATTR(CONVATTR_STATUS_AREA, 2);
	    data[len + 1] = (rectp->x << 16) | (rectp->y & 0xffff);
	    data[len + 2] = (rectp->width << 16) | (rectp->height & 0xffff);
	    len += 3;
	} else if (!strcmp(args->name, "cursor")) {
	    Cursor cursor = (Cursor)args->value;
	    ALLOC(2);
	    data[len] = CONV_ATTR(CONVATTR_CURSOR, 1);
	    data[len + 1] = cursor;
	    len += 2;
	} else if (!strcmp(args->name, "fonts")) {
	    XFontStruct **fontp = (XFontStruct **)args->value;
	    int nfonts, nrealfonts;
	    int i;

	    for (nfonts = 0; fontp[nfonts] != NULL; nfonts++)
	        ;
	    ALLOC(nfonts + 1);
	    nrealfonts = 0;
	    for (i = 0; i < nfonts; i++) {
		unsigned long atom;
		if (XGetFontProperty(fontp[i], XA_FONT, &atom)) {
		    data[len + ++nrealfonts] = atom;
		}
	    }
	    data[len] = CONV_ATTR(CONVATTR_FONT_ATOMS, nrealfonts);
	    len += nrealfonts + 1;
	} else {
	    String params[1];
	    Cardinal num_params;

	    params[0] = args->name;
	    XtAppWarningMsg(XtWidgetToApplicationContext(w),
			    "conversionError", "invalidResource",
			    "ConversionLibraryError",
			    "_beginConversionWithAttributes: unknown resource %s",
			    params, &num_params);
	}
	args++;
    }
    if (spotx_specified && spoty_specified) {
	ALLOC(2);
	data[len] = CONV_ATTR(CONVATTR_SPOT_LOCATION, 1);
	data[len + 1] = (savedx << 16) | (savedy & 0xffff);
	len += 2;
    }
    if (fore_specified && back_specified) {
	ALLOC(3);
	data[len] = CONV_ATTR(CONVATTR_COLOR, 2);
	data[len + 1] = savedfg;
	data[len + 2] = savedbg;
	len += 3;
    }
    *datap = data;
    return len;
#undef ALLOC
}

static Boolean
checkProtocols(dpy, window, cap)
Display *dpy;
Window window;
ConversionAtoms *cap;
{
    Atom type;
    int format;
    unsigned long nitems;
    unsigned long bytesafter;
    unsigned long *data, *saveddata;
    int err;
    Boolean ret;

    data = NULL;
    err = XGetWindowProperty(dpy, window, cap->profileAtom,
			     0L, 100L, False,
			     cap->typeAtom,
			     &type, &format, &nitems,
			     &bytesafter, (unsigned char **)&data);
    if (err) return False;
    if (format != 32 || type != cap->typeAtom) {
	if (data != NULL) XtFree((char *)data);
	return False;
    }

    ret = False;
    saveddata = data;
    while (nitems > 0) {
	int code = CODE_OF_ATTR(*data);
	int len = LENGTH_OF_ATTR(*data);

	data++;
	nitems--;
	if (nitems < len) break;

	switch (code) {
	case CONVPROF_PROTOCOL_VERSION:
	    if (*data == cap->versionAtom) ret = True;
	    break;
	case CONVPROF_SUPPORTED_STYLES:
	    break;	/* XXX for now */
	default:
	    break;
	}
	data += len;
	nitems -= len;
    }
    XtFree((char *)saveddata);

    return ret;
}


/*
 *	public functions
 */

int
_beginConversionWithAttributes(w, catom, tatom, inputproc, startendproc, client_data, attrs, nattrs)
Widget w;
Atom catom;		/* Selection Atom e.g. JAPANESE_CONVERSION */
Atom tatom;		/* Property Type Atom e.g. COMPOUND_TEXT */
void (*inputproc)();	/* conversion text callback function */
void (*startendproc)();	/* conversion start/end callback function */
XtPointer client_data;	/* client_data passed to callback function */
ArgList attrs;		/* attribute data */
Cardinal nattrs;	/* number of attr args */
{
    Window owner;
    XEvent event;
    ConversionAtoms *cap;
    ConversionContext *context;
    Boolean anyattr;

    cap = getAtoms(w);

    /* 変換サーバを探す */
    if ((owner = XGetSelectionOwner(XtDisplay(w), catom)) == None) {
	/* ない
	 * もしも変換中だったら変換を中止する
	 */
	XtWarning("Conversion Server not found");
	if ((context = getConversionContext(w)) != NULL) {
	    /* イベントハンドラを外す */
	    XtRemoveEventHandler(w, NoEventMask, True, recvConvAck,
				 (XtPointer)NULL);
	    XtRemoveEventHandler(w, PropertyChangeMask, True, getConv,
				 (XtPointer)NULL);
	    /* ウィンドウのコンテキストを削除する */
	    XDeleteContext(XtDisplay(w), XtWindow(w), convertPrivContext);
	    callEnd(w, context);
	    XtFree((char *)context);
	}
	return -1;
    }

    /*
     * 今すでに変換中かどうか調べる
     * 変換中なら何もせずにリターンする…わけにはいかない
     * なぜかというと、変換サーバが何らかの事情で途中で死んだ場合
     * CONVERSION_END がクライアントに来ないことがあるからである
     * そこで、変換中の場合でも SelectionOwner を探して、それが
     * 最初に _beginConversion() が呼ばれた時と WindowID が同じか
     * どうか確認する
     * 本当は SelectionOwner になった時間もチェックしたいのだが
     * ICCCM に述べられているように、GetSelectionOwner では
     * それがわからないのであきらめる
     */
    if ((context = getConversionContext(w)) != NULL) {
	Window curOwner;
	curOwner = (catom == context->convatom) ? owner :
	  XGetSelectionOwner(XtDisplay(w), context->convatom);
	if (curOwner == context->convowner) {
	    /* 何もせずにリターン */
	    return 0;
	}
	/* SelectionOwner が変わっている
	 * これは途中で変換サーバがクラッシュしたに違いない
	 * ということで CONVERSION_END が来た時と同じような
	 * 処理をする
	 */
	/* イベントハンドラを外す
	 * CONVERSION_NOTIFY のイベントが来るまでは
	 * recvConvAck() がハンドラで、その後は
	 * getConv() がハンドラである
	 */
	XtRemoveEventHandler(w, NoEventMask, True, recvConvAck,
			     (XtPointer)NULL);
	XtRemoveEventHandler(w, PropertyChangeMask, True, getConv,
			     (XtPointer)NULL);
	/* ウィンドウのコンテキストを削除する */
	XDeleteContext(XtDisplay(w), XtWindow(w), convertPrivContext);
	callEnd(w, context);
	XtFree((char *)context);
    }

    /*
     * サーバからの CONVERSION_NOTIFY 用のイベントハンドラを
     * 登録する
     */
    XtAddEventHandler(w, NoEventMask, True, recvConvAck, (XtPointer)NULL);

    /*
     * コンテキストをつくって必要な情報を登録する
     */
    context = XtNew(ConversionContext);
    context->convatom = catom;
    context->convowner = owner;
    context->property = None;	/* これは CONVERSION_NOTIFY が来た時に
				 * 正しく設定される */
    context->inputproc = inputproc;
    context->startendproc = startendproc;
    context->closure = client_data;
    XSaveContext(XtDisplay(w), XtWindow(w),
		 convertPrivContext, (caddr_t)context);

    /*
     * 変換属性リストが指定されていればプロパティにそれを登録する
     */
    if (nattrs != 0 && attrs != NULL &&
	checkProtocols(XtDisplay(w), owner, cap)) {
	anyattr = setConvAttrProp(w, attrs, nattrs, cap->attrAtom);
    }

    /*
     * ClientMessage イベントを使って日本語入力をリクエストする
     */
    event.xclient.type = ClientMessage;
    event.xclient.window = owner;
    event.xclient.message_type = cap->reqAtom;
    event.xclient.format = 32;
    event.xclient.data.l[0] = catom;
    event.xclient.data.l[1] = XtWindow(w);
    event.xclient.data.l[2] = tatom;
    /* 結果をストアするプロパティ名は、多言語を同時に使用することを
     * 考えて、selection atom を使用することにする
     */
    event.xclient.data.l[3] = catom;
    event.xclient.data.l[4] = anyattr ? cap->attrAtom : None;
    XSendEvent(XtDisplay(w), owner, False, NoEventMask, &event);

    return 0;
}

/* this is provided for backward compatibility */
void
_beginConversion(w, catom, tatom, inputproc, client_data)
Widget	w;
Atom	catom;			/* Selection Atom e.g. JAPANESE_CONVERSION */
Atom	tatom;			/* Property Type Atom e.g. COMPOUND_TEXT */
void	(*inputproc)();		/* conversion text callback function */
XtPointer client_data;		/* client_data passed to callback function */
{
    (void)_beginConversionWithAttributes(w, catom, tatom, inputproc,
				   (void (*)())NULL, client_data,
				   (ArgList)NULL, 0);
}

void
_changeConversionAttributes(w, catom, attrs, nattrs)
Widget	w;
Atom	catom;			/* Selection Atom e.g. JAPANESE_CONVERSION */
ArgList	attrs;			/* attribute data */
Cardinal nattrs;		/* number of attr args */
{
    XEvent event;
    ConversionAtoms *cap;
    ConversionContext *context;
    unsigned long *data;
    int len;

    if (attrs == NULL || nattrs == 0) return;

    cap = getAtoms(w);
    context = getConversionContext(w);

    if (context == NULL || (catom != None && catom != context->convatom)) {
	return;
    }

    if (XGetSelectionOwner(XtDisplay(w), context->convatom) !=
	context->convowner) {
	/* 変換サーバが異なる、あるいはない */
	XtRemoveEventHandler(w, NoEventMask, True, recvConvAck,
			     (XtPointer)NULL);
	XtRemoveEventHandler(w, PropertyChangeMask, True, getConv,
			     (XtPointer)NULL);
	/* ウィンドウのコンテキストを削除する */
	XDeleteContext(XtDisplay(w), XtWindow(w), convertPrivContext);
	callEnd(w, context);
	XtFree((char *)context);
	return;
    }

    data = NULL;
    if ((len = makeAttrData(w, attrs, nattrs, &data)) == 0) return;

    event.xclient.type = ClientMessage;
    event.xclient.window = context->convowner;
    event.xclient.message_type = cap->attrNotifyAtom;
    event.xclient.format = 32;
    event.xclient.data.l[0] = context->convatom;
    event.xclient.data.l[1] = XtWindow(w);
    if (len <= 3 && len == LENGTH_OF_ATTR(data[0]) + 1) {
	int i;
	/* イベントの中に収まる */
	for (i = 0; i < len; i++) {
	    event.xclient.data.l[2 + i] = data[i];
	}
    } else {
	XChangeProperty(XtDisplay(w), XtWindow(w),
			cap->attrAtom, cap->attrAtom, 32,
			PropModeReplace, (unsigned char *)data, len);
	event.xclient.data.l[2] = CONV_ATTR(CONVATTR_INDIRECT, 1);
	event.xclient.data.l[3] = cap->attrAtom;
    }

    XSendEvent(XtDisplay(w), context->convowner, False, NoEventMask, &event);

    if (data != NULL) XtFree((char *)data);
}

void
_endConversion(w, catom, throwaway)
Widget	w;
Atom	catom;		/* Selection Atom */
Boolean	throwaway;
{
    XEvent event;
    ConversionAtoms *cap;
    ConversionContext *context;

    cap = getAtoms(w);
    context = getConversionContext(w);

    if (context == NULL || (catom != None && catom != context->convatom)) {
	return;
    }

    if (XGetSelectionOwner(XtDisplay(w), context->convatom) !=
	context->convowner) {
	/* 変換サーバが異なる、あるいはない */
	XtRemoveEventHandler(w, NoEventMask, True, recvConvAck,
			     (XtPointer)NULL);
	XtRemoveEventHandler(w, PropertyChangeMask, True, getConv,
			     (XtPointer)NULL);
	/* ウィンドウのコンテキストを削除する */
	XDeleteContext(XtDisplay(w), XtWindow(w), convertPrivContext);
	/* コールバックを呼ぶ */
	callEnd(w, context);
	XtFree((char *)context);
	return;
    }

    if (throwaway) context->inputproc = NULL;

    event.xclient.type = ClientMessage;
    event.xclient.window = context->convowner;
    event.xclient.message_type = cap->endReqAtom;
    event.xclient.format = 32;
    event.xclient.data.l[0] = context->convatom;
    event.xclient.data.l[1] = XtWindow(w);

    XSendEvent(XtDisplay(w), context->convowner, False, NoEventMask, &event);
}
