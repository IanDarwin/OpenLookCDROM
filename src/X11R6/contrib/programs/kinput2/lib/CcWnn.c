#ifndef lint
static char *rcsid = "$Id: CcWnn.c,v 1.35 1993/09/07 07:24:54 ishisone Rel $";
#endif
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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Atoms.h>
#if XtSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif
#include "CcWnnP.h"
#include <wnnerror.h>

static XtResource resources[] = {
#define offset(field) XtOffset(CcWnnObject, ccWnn.field)
    { XtNconfirmFunc, XtCFunction, XtRPointer, sizeof(int (*)()), 
       offset(confirmfunc), XtRPointer, (XtPointer)NULL },
    { XtNconfirmData, XtCConfirmData, XtRPointer, sizeof(XtPointer), 
       offset(confirmdata), XtRPointer, (XtPointer)NULL },
    { XtNjserver, XtCJserver, XtRString, sizeof(String),
	offset(jservername), XtRString, NULL },
    { XtNjserver2nd, XtCJserver, XtRString, sizeof(String),
	offset(jservername2), XtRString, NULL },
    { XtNwnnEnvname, XtCWnnEnvname, XtRString, sizeof(String),
	offset(wnnenvname), XtRString, NULL },
    { XtNwnnEnvrc, XtCWnnEnvrc, XtRString, sizeof(String),
	offset(wnnenvrcfile), XtRString, NULL },
    { XtNwnnOverrideEnv, XtCWnnOverrideEnv, XtRBoolean, sizeof(Boolean),
	offset(wnnoverrideenv), XtRString, "false" },
    { XtNccdef, XtCCcdef, XtRString, sizeof(String),
	offset(ccdeffile), XtRString, NULL },
    { XtNwnnEnv, XtCWnnEnv, XtRWnnEnv, sizeof(struct wnn_env *),
	offset(wnnenv), XtRWnnEnv, NULL},
    { XtNccRule, XtCCcRule, XtRCcRule, sizeof(ccRule),
	offset(ccrule), XtRCcRule, NULL},
    { XtNsaveInterval, XtCSaveInterval, XtRInt, sizeof(int),
	offset(saveinterval), XtRImmediate, 0 },
#undef offset
};

static void ClassInitialize();
static int buildSymbolList();
static void Initialize(), Destroy();
static Boolean SetValues();
static int InputEvent();
static ICString *GetMode();
static int CursorPos();
static int NumSegments();
static ICString *GetSegment();
static int CompareSegment();
static ICString *GetItemList();
static int SelectItem();
static int ConvertedString();
static int ClearConversion();
static ICString *GetAuxSegments();

CcWnnClassRec ccWnnClassRec = {
  { /* object fields */
    /* superclass		*/	(WidgetClass) &inputConvClassRec,
    /* class_name		*/	"CcWnn",
    /* widget_size		*/	sizeof(CcWnnRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* obj1			*/	NULL,
    /* obj2			*/	NULL,
    /* obj3			*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* obj4			*/	FALSE,
    /* obj5			*/	FALSE,
    /* obj6			*/	FALSE,
    /* obj7			*/	FALSE,
    /* destroy			*/	Destroy,
    /* obj8			*/	NULL,
    /* obj9			*/	NULL,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* obj10			*/	NULL,
    /* get_values_hook		*/	NULL,
    /* obj11			*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* obj12			*/	NULL,
    /* obj13			*/	NULL,
    /* obj14			*/	NULL,
    /* extension		*/	NULL
  },
  { /* inputConv fields */
    /* InputEvent		*/	InputEvent,
    /* GetMode			*/	GetMode,
    /* CursorPos		*/	CursorPos,
    /* NumSegments		*/	NumSegments,
    /* GetSegment		*/	GetSegment,
    /* CompareSegment		*/	CompareSegment,
    /* GetItemList		*/	GetItemList,
    /* SelectItem		*/	SelectItem,
    /* GetConvetedString	*/	ConvertedString,
    /* ClearConversion		*/	ClearConversion,
    /* GetAuxSegments		*/	GetAuxSegments,
    /* SupportMultipleObjects	*/	True,
    /* NoMoreObjects		*/	False,
  },
  { /* ccWnn fields */
    /* foo			*/	0,
  }
};

WidgetClass ccWnnObjectClass = (WidgetClass)&ccWnnClassRec;

/* cconv function table */
static char *fepfunctbl[] = {
	"convert",
	"convert-or-space",
	"convert-s",
	"unconvert",
	"next",	
	"next-s",
	"previous",
	"previous-s",
	"forward",
	"backward",
	"move-top",
	"move-bottom",
	"clear",
	"expand",
	"expand-s",
	"shrink",
	"shrink-s",
	"expand-noconv",
	"expand-noconv-s",
	"shrink-noconv",
	"shrink-noconv-s",
	"fix",
	"fix2",
	"fix-or-cr",
	"to-hankaku",
	"to-zenkaku",
	"to-hiragana",
	"to-katakana",
	"backspace",
	"delete",
	"carriage-return",
	"fix-and-write",
	"beep",
	"jiscode-begin",
	"jiscode-end",
	"kutencode-begin",
	"kutencode-end",
	"symbol-input",
	"end-conversion",
	"send-back",
};
#define FTSIZE	(sizeof(fepfunctbl) / sizeof(char *))

static void	convert();
static void	convert_sp();
static void	convert_s();
static void	unconvert();

static void	move_forward();
static void	move_backward();
static void	move_top();
static void	move_bottom();

static void	cand_next();
static void	cand_next_s();
static void	cand_prev();
static void	cand_prev_s();

static void	expand_cl();
static void	expand_cl_s();
static void	shrink_cl();
static void	shrink_cl_s();
static void	expand_cl2();
static void	expand_cl2_s();
static void	shrink_cl2();
static void	shrink_cl2_s();

static void	clear_buffer();

static void	sel_top();
static void	sel_bottom();
static void	sel_forward();
static void	sel_backward();
static void	sel_next();
static void	sel_prev();
static void	sel_select();
static void	sel_abort();

static void	fix();
static void	fix_cr();

static void	hankaku();
static void	zenkaku();

static void	hiragana();
static void	katakana();

static void	backspace();
static void	delete();

static void	bell();
static void	beep();
static void	carriageret();
static void	jiscode_begin();
static void	jiscode_end();
static void	kuten_begin();
static void	kuten_end();

static void	sym_input();
static void	convend();
static void	send_back();

/* cconv function dispatch table */
static void (*functable[][3])() = {
/* Function Name	Normal-mode	selection-mode	symbol-mode */
/* convert */		convert,	sel_forward,    beep,
/* convert-or-space */	convert_sp,	sel_forward,    beep,
/* convert-s */		convert_s,	sel_forward,    beep,
/* unconvert */		unconvert,	beep,           beep,
/* next */		cand_next,	sel_next,       sel_next,       
/* next-s */		cand_next_s,	sel_next,       sel_next,
/* previous */		cand_prev,	sel_prev,       sel_prev,
/* previous-s */	cand_prev_s,	sel_prev,       sel_prev,
/* forward */		move_forward,	sel_forward,    sel_forward,
/* backward */		move_backward,	sel_backward,   sel_backward,
/* move-top */		move_top,	sel_top,        sel_top,
/* move-bottom */	move_bottom,	sel_bottom,     sel_bottom,
/* clear */		clear_buffer,	clear_buffer,   clear_buffer,
/* expand */		expand_cl,	expand_cl,  	beep,
/* expand-s */		expand_cl_s,	expand_cl_s,	beep,
/* shrink */		shrink_cl,	shrink_cl,  	beep,
/* shrink-s */		shrink_cl_s,	shrink_cl_s,	beep,
/* expand-noconv */	expand_cl2,	expand_cl2, 	beep,
/* expand-noconv-s */	expand_cl2_s,	expand_cl2_s,	beep,
/* shrink-noconv */	shrink_cl2,	shrink_cl2, 	beep,
/* shrink-noconv-s */	shrink_cl2_s,	shrink_cl2_s,	beep,
/* fix */		fix,		fix,            fix,
/* fix2 */		fix,		fix,            fix,
/* fix-or-cr */		fix_cr,		sel_select,     sel_select,
/* to-hankaku */	hankaku,	hankaku,        beep,
/* to-zenkaku */	zenkaku,	zenkaku,        beep,
/* to-hiragana */	hiragana,	hiragana,       beep,
/* to-katakana */	katakana,	katakana,       beep,
/* backspace */		backspace,	backspace,      backspace,
/* delete */		delete,		delete,         delete,
/* carriage-return */	carriageret,	sel_select,     sel_select,
/* fix-and-write */	fix,		beep,           beep,
/* beep */		bell,		bell,           bell,
/* jiscode-begin */	jiscode_begin,	beep,		beep,
/* jiscode-end */	jiscode_end,	beep,		beep,
/* kutencode-begin */	kuten_begin,	beep,		beep,
/* kutencode-end */	kuten_end,	beep,		beep,
/* symbol-input */	sym_input,	beep,           sel_abort,
/* end-conversion */	convend,	convend,	convend,
/* send-back */		send_back,	send_back,	send_back,
};

static ICString *SymbolList;
static int NumSymbols;

static void ccInitialize(), jcInitialize();
static void createEnvError();
static int createConfirm();

static void funcDispatch();
static void defAction();
static void insChar();
static void delChar();

static void startSelection();
static void moveSelection();
static int endSelection();
static int insertSelection();

static int getSymbol();

static void normalState();

static void allocCandlist();
static void allocStrdata();
static void getAllCandidates();

static void addObject();
static void deleteObject();
static void serverDead();

static void saveData();
static void restoreData();

static void
ClassInitialize()
{
    /* symbollist を設定 */
    NumSymbols = buildSymbolList(&SymbolList);
}

static int
buildSymbolList(listp)
ICString **listp;
{
    static struct symgroup {
	int	first;
	int last;
    } symgroups[] = {
	{ 0xa1a1, 0xa2ae },	/* '　' - '〓' */
	{ 0xa2ba, 0xa2c1 },	/* '∈' - '∩' */
	{ 0xa2ca, 0xa2d0 },	/* '∧' - '∃' */
	{ 0xa2dc, 0xa2ea },	/* '∠' - '∬' */
	{ 0xa2f2, 0xa2f9 },	/* 'Å' - '¶' */
	{ 0xa2fe, 0xa2fe },	/* '◯' */
	{ 0xa4ee, 0xa4ee },	/* 'ゎ' */
	{ 0xa4f0, 0xa4f1 },	/* 'ゐ', 'ゑ' */
	{ 0xa5ee, 0xa5ee },	/* 'ヮ' */
	{ 0xa5f0, 0xa5f1 },	/* 'ヰ', 'ヱ' */
	{ 0xa5f4, 0xa5f6 },	/* 'ヴ', 'ヵ', 'ヶ' */
	{ 0xa6a1, 0xa6b8 },	/* 'Α' - 'Ω' */
	{ 0xa6c1, 0xa6d8 },	/* 'α' - 'ω' */
	{ 0xa7a1, 0xa7c1 },	/* 'А' - 'Я' */
	{ 0xa7d1, 0xa7f1 },	/* 'а' - 'я' */
	{ 0xa8a1, 0xa8c0 },	/* '─' - '╂' */
	{ -1, -1 }
    };
    struct symgroup *sgp;
    Cardinal nsyms;
    ICString *symlist, *sp;
    wchar *buf, *p;

    for (nsyms = 0, sgp = symgroups; sgp->first > 0; sgp++) {
#define LINEAR_INDEX(c)	(((((c)>>8)&0x7f)*94)+((c)&0x7f))
	nsyms += LINEAR_INDEX(sgp->last) - LINEAR_INDEX(sgp->first) + 1;
    }

    symlist = (ICString *)XtMalloc(nsyms * sizeof(ICString));
    buf = (wchar *)XtMalloc(nsyms * sizeof(wchar));

    sp = symlist;
    p = buf;
    for (sgp = symgroups; sgp->first > 0; sgp++) {
	int i;
#define NEXT_CHAR(c) ((((c)&0xff)>0xfd)?(((c)&0xff00)+0x1a1):((c)+1))
	for (i = sgp->first; i <= sgp->last; i = NEXT_CHAR(i)) {
	    sp->nchars = 1;
	    sp->nbytes = sizeof(wchar);
	    sp->data = (char *)p;
	    sp->attr = ICAttrNormalString;
	    sp++;
	    *p++ = i;
	}
    }

    *listp = symlist;
    return nsyms;
}

static int
InputEvent(w, event)
Widget w;
XEvent *event;
{
    CcWnnObject obj = (CcWnnObject)w;
    int sendback;
    int ret = 0;
    wchar *curmode;

    if (event->type != KeyPress /*&& event->type != KeyRelease*/) return 0;

    /* イベントをクライアントに送り返すかどうかの判定その 1 */
    sendback = (NumSegments(w) == 0 && obj->ccWnn.state == normal_state);

    obj->ccWnn.sendbackevent = False;
    obj->ccWnn.fixperformed = False;
    obj->ccWnn.textchanged = False;

    /* もし jserver が死んだなどの理由により obj->ccWnn.wnnenv が NULL に
     * なっていた場合、再び初期化する
     */
    if (obj->ccWnn.wnnenv == NULL) jcInitialize(obj);

    /* それでも wnnenv が NULL (まだ死んでいる) ならばエラーリターン */
    if (obj->ccWnn.wnnenv == NULL) return -1;

    wnn_errorno = 0;
    curmode = ccGetModePrompt(obj->ccWnn.ccbuf);

    (void)ccConvchar(obj->ccWnn.ccbuf, (XKeyPressedEvent *)event);

    /*
     * エラー番号をチェックし、jserver が死んでいれば環境を destroy し
     * 再び接続を試みる
     */
    if (wnn_errorno == WNN_JSERVER_DEAD) {
	WNN_JSERVER_ID *server = obj->ccWnn.wnnenv->js_id;

	XtAppWarning(XtWidgetToApplicationContext((Widget)w),
		     "ccWnn Object: jserver died");
	/* もしも入力中の文字列があればとっておく */
	saveData(obj);
	serverDead(server);
	/* secondary jserver が指定されていれば再接続を試みる */
	if (obj->ccWnn.jservername2 != NULL) jcInitialize(obj);
	if (obj->ccWnn.wnnenv == NULL) ret = -1;
    }

    /* テキストの変化をチェックする */
    if (obj->ccWnn.textchanged) {
	XtCallCallbackList(w, obj->inputConv.textchangecallback,
			   (XtPointer)NULL);
	obj->ccWnn.textchanged = False;
    }
    /* 入力モードをチェックする */
    if (wstrcmp(ccGetModePrompt(obj->ccWnn.ccbuf), curmode)) {
	sendback = 0;
	XtCallCallbackList(w, obj->inputConv.modechangecallback,
			   (XtPointer)NULL);
    }
    /* イベントをクライアントに送り返すかどうかの判定その 2 */
    if (NumSegments(w) != 0 ||
	obj->ccWnn.state != normal_state ||
	obj->ccWnn.fixperformed) {
	sendback = 0;
    }
    if (ret == 0 && (obj->ccWnn.sendbackevent || sendback)) ret = 1;

    return ret;
}

static ICString *
GetMode(w)
Widget w;
{
    CcWnnObject obj = (CcWnnObject)w;
    wchar *mode;
    static ICString icstr;

    mode = ccGetModePrompt(obj->ccWnn.ccbuf);
    icstr.data = (char *)mode;
    icstr.nchars = wstrlen(mode);
    icstr.nbytes = icstr.nchars * sizeof(wchar);
    icstr.attr = ICAttrNormalString;

    return &icstr;
}

static int
CursorPos(w, nsegp, ncharp)
Widget w;
Cardinal *nsegp;
Cardinal *ncharp;
{
    CcWnnObject obj = (CcWnnObject)w;
    jcConvBuf *jcbuf = obj->ccWnn.jcbuf;
    Cardinal nseg, nchar;

    if (jcbuf == NULL || jcIsConverted(jcbuf, jcbuf->curClause)) return 0;

    nseg = jcbuf->curClause;
    nchar = jcDotOffset(jcbuf);

    if (nseg >= jcbuf->nClause) {
	if (nseg == 0) {
	    nchar = 0;
	} else {
	    jcClause *cinfo = jcbuf->clauseInfo;
	    nseg--;
	    nchar = cinfo[nseg + 1].dispp - cinfo[nseg].dispp;
	}
    }

    if (nsegp) *nsegp = nseg;
    if (ncharp) *ncharp = nchar;

    return 1;
}

static int
NumSegments(w)
Widget w;
{
    CcWnnObject obj = (CcWnnObject)w;

    return (obj->ccWnn.jcbuf != NULL) ? obj->ccWnn.jcbuf->nClause : 0;
}

static ICString *
GetSegment(w, n)
Widget w;
Cardinal n;
{
    CcWnnObject obj = (CcWnnObject)w;
    jcConvBuf *jcbuf = obj->ccWnn.jcbuf;
    jcClause *cinfo = jcbuf->clauseInfo;
    static ICString seg;

    if (jcbuf == NULL || n >= jcbuf->nClause) return NULL;
    seg.data = (char *)cinfo[n].dispp;
    seg.nchars = cinfo[n + 1].dispp - cinfo[n].dispp;
    seg.nbytes = seg.nchars * sizeof(wchar);
    seg.attr = cinfo[n].conv ? ICAttrConverted : ICAttrNotConverted;
    if (n == jcbuf->curClause) {
	seg.attr |= ICAttrCurrentSegment;
    } else if (jcbuf->curLCStart <= n && n < jcbuf->curLCEnd) {
	seg.attr |= ICAttrCurrentSubSegment;
    }
    return &seg;
}

/* ARGSUSED */
static int
CompareSegment(w, seg1, seg2, n)
Widget w;
ICString *seg1;
ICString *seg2;
Cardinal *n;
{
    wchar *p, *q;
    int len, nsame;
    int result = 0;

    if (seg1->attr != seg2->attr) result |= ICAttrChanged;

    len = seg1->nchars > seg2->nchars ? seg2->nchars : seg1->nchars;
    nsame = 0;
    p = (wchar *)seg1->data;
    q = (wchar *)seg2->data;
    while (nsame < len && *p++ == *q++) nsame++;

    if (nsame != len || len != seg1->nchars || len != seg2->nchars)
	result |= ICStringChanged;

    if (n) *n = nsame;

    return result;
}

static ICString *
GetItemList(w, n)
Widget w;
Cardinal *n;
{
    CcWnnObject obj = (CcWnnObject)w;

    switch (obj->ccWnn.state) {
    case selection_l_state:
    case selection_s_state:
	*n = obj->ccWnn.numcand;
	return obj->ccWnn.candlist;
    case symbol_state:
	*n = obj->ccWnn.numsymbols;
	return obj->ccWnn.symbollist;
    default:
	*n = 0;
	return NULL;	/* no item available */
    }
    /* NOTREACHED */
}

static int
SelectItem(w, n)
Widget w;
int n;
{
    CcWnnObject obj = (CcWnnObject)w;
    int ret = 0;

    if (obj->ccWnn.state == normal_state) return -1;

    if (obj->ccWnn.jcbuf == NULL) {
	ret = -1;
    } else if (n >= 0) {
	ret = insertSelection(obj, n);
	if (obj->ccWnn.textchanged) {
	    XtCallCallbackList((Widget)obj,
			       obj->inputConv.textchangecallback,
			       (XtPointer)NULL);
	    obj->ccWnn.textchanged = False;
	}
    }

    obj->ccWnn.state = normal_state;
    return ret;
}

static int
ConvertedString(w, encoding, format, length, string)
Widget w;
Atom *encoding;
int *format;
int *length;
XtPointer *string;
{
    CcWnnObject obj = (CcWnnObject)w;
    jcConvBuf *jcbuf = obj->ccWnn.jcbuf;
    wchar *wbuf, *wp;
    int len, wlen;
    extern int convJWStoCT();

    if (jcbuf == NULL) return -1;

    wlen = jcbuf->displayEnd - jcbuf->displayBuf;
    if (wlen == 0) return -1;

    /*
     * jcbuf に入っている変換テキストは null ターミネートされていないので
     * まずコピーして null ターミネートする
     */
    wbuf = (wchar *)XtMalloc((wlen + 1) * sizeof(wchar));
    (void)bcopy((char *)jcbuf->displayBuf, (char *)wbuf,
		sizeof(wchar) * wlen);
    wbuf[wlen] = 0;

    /*
     * CcWnn オブジェクトは COMPOUND_TEXT エンコーディングしかサポートしない
     * COMPOUND_TEXT に変換する
     */
    *encoding = XA_COMPOUND_TEXT(XtDisplayOfObject((Widget)obj));
    *format = 8;

    /* COMPOUND_TEXT は \r が送れないので \n に変換しておく */
    for (wp = wbuf; *wp != 0; wp++) {
	if (*wp == '\r') *wp = '\n';
    }

    *length = len = convJWStoCT(wbuf, (unsigned char *)NULL, 0);
    *string = XtMalloc(len + 1);
    (void)convJWStoCT(wbuf, (unsigned char *)*string, 0);

    /* wbuf を free しておく */
    XtFree((char *)wbuf);

    return 0;
}

static int
ClearConversion(w)
Widget w;
{
    CcWnnObject obj = (CcWnnObject)w;

    if (obj->ccWnn.jcbuf == NULL) {
	return 0;	/* not -1, because it's already cleared */
    }
    clear_buffer(obj);
    XtCallCallbackList(w, obj->inputConv.textchangecallback, (XtPointer)NULL);
    return 0;
}

/* ARGSUSED */
static ICString *
GetAuxSegments(w, n, ns, nc)
Widget w;
Cardinal *n, *ns, *nc;
{
    /* CcWnn doesn't use AuxPanel */
    XtAppWarning(XtWidgetToApplicationContext(w),
		 "ccWnn Object: GetAuxSegments shouldn't be called");
    return NULL;
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    CcWnnObject obj = (CcWnnObject)new;

    obj->ccWnn.state = normal_state;
    obj->ccWnn.selectionending = False;
    obj->ccWnn.textchanged = False;
    obj->ccWnn.symbollist = SymbolList;
    obj->ccWnn.numsymbols = NumSymbols;
    obj->ccWnn.cursymbol = 0;
    obj->ccWnn.candlist = NULL;
    obj->ccWnn.candlistsize = 0;
    obj->ccWnn.numcand = 0;
    obj->ccWnn.strdata = NULL;
    obj->ccWnn.strdatasize = 0;
    obj->ccWnn.inputmode = OTHERS;
    obj->ccWnn.pendingdata = NULL;
    obj->ccWnn.fixcount = 0;

    /* 変換の初期化 */
    obj->ccWnn.createrule = False;
    obj->ccWnn.createenv = False;
    ccInitialize(obj);
    jcInitialize(obj);

    addObject(obj);
}

static void
ccInitialize(obj)
CcWnnObject obj;
{
    extern char *getenv();

    obj->ccWnn.ccbuf = NULL;
    if (obj->ccWnn.ccrule == NULL) {
	if (obj->ccWnn.ccdeffile == NULL) {
	    obj->ccWnn.ccdeffile = getenv("CC_DEF");
	    if (obj->ccWnn.ccdeffile == NULL) {
		obj->ccWnn.ccdeffile = DEF_CCDEF_FILE;
	    }
	}
	obj->ccWnn.ccrule = ccParseRule(obj->ccWnn.ccdeffile, XtWarning);
	obj->ccWnn.createrule = True;
    }

    if (obj->ccWnn.ccrule == NULL) {
	XtAppError(XtWidgetToApplicationContext((Widget)obj),
		   "CcWnn Object: cconv initialization failed.");
    }

    obj->ccWnn.ccbuf = ccCreateBuf(obj->ccWnn.ccrule, 16,
				   fepfunctbl, FTSIZE,
				   defAction, insChar, delChar,
				   funcDispatch, NULL, (caddr_t)obj);
}

static void
jcInitialize(obj)
CcWnnObject obj;
{
    obj->ccWnn.jcbuf = NULL;
    if (obj->ccWnn.wnnenv == NULL) {
	WNN_JSERVER_ID	*server;
	if (obj->ccWnn.wnnenvname == NULL) obj->ccWnn.wnnenvname = "";
	if (obj->ccWnn.wnnenvrcfile == NULL) obj->ccWnn.wnnenvrcfile = "";
	server = jiOpenServer(obj->ccWnn.jservername, 30);
	if (server == NULL && obj->ccWnn.jservername2 != NULL) {
	    /* try the secondary server */
	    server = jiOpenServer(obj->ccWnn.jservername2, 30);
	}
	if (server == NULL) {
	    obj->ccWnn.wnnenv = NULL;
	    XtAppWarning(XtWidgetToApplicationContext((Widget)obj),
			 "ccWnn Object: can't connect to jserver");
	} else {
	    obj->ccWnn.wnnenv = jiCreateEnv(server,
					    obj->ccWnn.wnnenvname,
					    obj->ccWnn.wnnoverrideenv,
					    obj->ccWnn.wnnenvrcfile,
					    createEnvError,
					    createConfirm,
					    (caddr_t)obj);
	    obj->ccWnn.createenv = True;
	}
    }
    if (obj->ccWnn.wnnenv) {
	obj->ccWnn.jcbuf = jcCreateBuffer(obj->ccWnn.wnnenv, 10, 80);
	if (obj->ccWnn.pendingdata) restoreData(obj);
    }
}

static void
createEnvError(type, s, data)
int type;
char *s;
caddr_t data;
{
    Widget w = (Widget)data;
    char line[256];

    (void)sprintf(line, "%s: %s", type == TYPE_ERROR ? "ERROR" : "WARNING", s);
    XtAppWarning(XtWidgetToApplicationContext(w), line);
}

static int
createConfirm(type, file, data)
int type;
char *file;
caddr_t data;
{
    CcWnnObject obj = (CcWnnObject)data;

    if (obj->ccWnn.confirmfunc) {
	return (*obj->ccWnn.confirmfunc)((Widget)obj, type, file);
    } else {
	return 1;
    }
}

static void
Destroy(w)
Widget w;
{
    CcWnnObject obj = (CcWnnObject)w;
    
    /* バッファの解放 */
    if (obj->ccWnn.ccbuf) ccDestroyBuf(obj->ccWnn.ccbuf);
    if (obj->ccWnn.jcbuf) jcDestroyBuffer(obj->ccWnn.jcbuf, 1);

    /* もし Initialize() 中でルール/環境を作ったのであれば解放する */
    if (obj->ccWnn.createrule) ccFreeRule(obj->ccWnn.ccrule);
    if (obj->ccWnn.createenv && obj->ccWnn.wnnenv) {
	WNN_JSERVER_ID	*server = obj->ccWnn.wnnenv->js_id;
	(void)jiDeleteEnv(obj->ccWnn.wnnenv);
	(void)jiCloseServer(server);
    }

    if (obj->ccWnn.candlist) XtFree((char *)obj->ccWnn.candlist);
    if (obj->ccWnn.strdata) XtFree((char *)obj->ccWnn.strdata);

    if (obj->ccWnn.pendingdata) XtFree((char *)obj->ccWnn.pendingdata);

    deleteObject(obj);
}

/* ARGSUSED */
static Boolean
SetValues(cur, req, wid, args, num_args)
Widget cur;
Widget req;
Widget wid;
ArgList args;
Cardinal *num_args;
{
    CcWnnObject old = (CcWnnObject)cur;
    CcWnnObject new = (CcWnnObject)wid;

    if (old->ccWnn.ccdeffile != new->ccWnn.ccdeffile ||
	old->ccWnn.wnnenv != new->ccWnn.wnnenv ||
	old->ccWnn.ccrule != new->ccWnn.ccrule) {
	XtAppWarning(XtWidgetToApplicationContext(wid),
		     "ccWnn Object: can't change resource by XtSetValues()");
    }
    return False;	     
}


/*
 *	cconv function dispatcher
 */

/* ARGSUSED */
static void
funcDispatch(func, str, nbytes, w)
int func;
unsigned char *str;
int nbytes;
caddr_t w;
{
    CcWnnObject obj = (CcWnnObject)w;

    if (func < 0 || func >= FTSIZE) return;

    wnn_errorno = 0;

    switch (obj->ccWnn.state) {
    case selection_s_state:
    case selection_l_state:
	(*functable[func][1])(obj);
	break;
    case symbol_state:
	(*functable[func][2])(obj);
	break;
    default:
	(*functable[func][0])(obj);
	break;
    }
}

/* ARGSUSED */
static void
defAction(str, nbytes, w)
unsigned char *str;
int nbytes;
caddr_t w;
{
    if (nbytes > 0) beep((CcWnnObject)w);
}

#define ZERO	0xa3b0
#define NINE	0xa3b9
#define SmallA	0xa3e1
#define SmallF	0xa3e6
#define LargeA	0xa3c1
#define LargeF	0xa3c6
static int
toHex(c)
int	c;
{
	if ('0' <= c && c <= '9')
		return c - '0';
	if ('a' <= c && c <= 'f')
		return c + 10 - 'a';
	if ('A' <= c && c <= 'F')
		return c + 10 - 'A';
	if (ZERO <= c && c <= NINE)
		return c - ZERO;
	if (SmallA <= c && c <= SmallF)
		return c + 10 - SmallA;
	if (LargeA <= c && c <= LargeF)
		return c + 10 - LargeA;
	return -1;
}

static int
toHex4(s)
wchar	*s;
{
	int	c, h, cnt, hex;

	hex = 0;
	cnt = 0;
	while (cnt < 4 && (c = *s++)) {
		if ((h = toHex(c)) < 0)
			return -1;
		hex = hex * 16 + h;
		cnt++;
	}
	if (cnt != 4)
		return -1;

	return hex;
}

static int
toKuten(s)
wchar *s;
{
	int i, c, d[4];

	for (i = 0; i < 4 && (c = *s++); i++) {
		if ((d[i] = toHex(c)) < 0 || d[i] >= 10)
			return(-1);
	}
	if (i != 4)
		return(-1);
	return((((d[0] * 10 + d[1]) << 8) | (d[2] * 10 + d[3])) + 0x2020);
}

static void
insChar(c, cldata)
int c;
caddr_t cldata;
{
    CcWnnObject obj = (CcWnnObject)cldata;
    jcConvBuf *jcbuf = obj->ccWnn.jcbuf;
    ccBuf ccbuf = obj->ccWnn.ccbuf;
    wchar	context[17];
    int h;

    normalState(obj);

    if (jcIsConverted(jcbuf, jcbuf->curClause)) {
	(void)jcBottom(jcbuf);
    }

    switch (obj->ccWnn.inputmode) {
    case KUTEN_MODE:
    case JIS_MODE:
	    /* ヘキサかどうかのテスト */
	    if ((h = toHex(c)) < 0 || (obj->ccWnn.inputmode == KUTEN_MODE && h >= 10)) {
		    beep(obj);
		    ccContextDelete(ccbuf);
		    break;
	    }
	    ccContextGet(ccbuf, context);
	    if (wstrlen(context) == 4) {
		    /* convert to KANJI */
		    c = obj->ccWnn.inputmode == KUTEN_MODE ? toKuten(context): toHex4(context);
		    if (c < 0x2121 || 0x7e7e < c || (c & 0xff) < 0x21 ||
			0x7e < (c & 0xff)) {
			    beep(obj);
			    break;
		    }
		    /* ３文字削除 -- ４文字目はまだ挿入していない */
		    jcDeleteChar(jcbuf, 1);
		    jcDeleteChar(jcbuf, 1);
		    jcDeleteChar(jcbuf, 1);
		    (void)jcInsertChar(jcbuf, c | 0x8080);
		    obj->ccWnn.textchanged = True;
		    /* コンテキストのクリア */
		    ccContextClear(ccbuf);
		    break;
	    }
	    /* fall thru */
    case OTHERS:
	    (void)jcInsertChar(jcbuf, c);
	    obj->ccWnn.textchanged = True;
	    break;
    }
}

static void
delChar(cldata)
caddr_t cldata;
{
    CcWnnObject obj = (CcWnnObject)cldata;

    if (obj->ccWnn.state != normal_state) {
	beep(obj);
	return;
    }
    ccContextDelete(obj->ccWnn.ccbuf);
    jcDeleteChar(obj->ccWnn.jcbuf, 1);
    obj->ccWnn.textchanged = True;
}

/*
 *	cconv functions
 */

/* some convenient macros */
#define JCBUF(obj)	((obj)->ccWnn.jcbuf)
#define CCBUF(obj)	((obj)->ccWnn.ccbuf)
#define HINT(obj)	((obj)->ccWnn.textchanged)

/* 変換ファンクション群
 *	convert
 *	convert-sp
 *	convert-s
 *	unconvert
 */

static void
convert_general(obj, small)
CcWnnObject obj;
int small;
{
    jcConvBuf	*jcbuf = JCBUF(obj);

    if (jcbuf->curClause == jcbuf->nClause) {
	(void)jcMove(jcbuf, small, JC_BACKWARD);
	HINT(obj) = True;
    }

    if (jcIsConverted(jcbuf, jcbuf->curClause)) {
	startSelection(obj, small);
	return;
    }

    if (jcConvert(jcbuf, small, 0, 1) < 0) beep(obj);
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}

static void
convert(obj)
CcWnnObject obj;
{
    convert_general(obj, 0);
}

static void
convert_sp(obj)
CcWnnObject obj;
{
    if (JCBUF(obj)->nClause == 0) {
	insChar(' ', (caddr_t)obj);
	fix(obj);
    } else {
	convert_general(obj, 0);
    }
}

static void
convert_s(obj)
CcWnnObject obj;
{
    convert_general(obj, 1);
}

static void
unconvert(obj)
CcWnnObject obj;
{
    if (jcUnconvert(JCBUF(obj)) < 0) beep(obj);
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}


/* カーソル移動ファンクション群
 *	move_forward
 *	move_backward
 *	move_top
 *	move_bottom
 */

static void
move_general(obj, direction)
CcWnnObject obj;
int direction;
{
    int status = -1;

    switch (direction) {
    case ICMoveLeftMost:
	status = jcTop(JCBUF(obj));
	break;
    case ICMoveRightMost:
	status = jcBottom(JCBUF(obj));
	break;
    case ICMoveRight:
	status = jcMove(JCBUF(obj), 1, JC_FORWARD);
	break;
    case ICMoveLeft:
	status = jcMove(JCBUF(obj), 1, JC_BACKWARD);
	break;
    }

    if (status < 0) beep(obj);

    ccContextClear(CCBUF(obj));

    HINT(obj) = True;
}

static void
move_forward(obj)
CcWnnObject obj;
{
    move_general(obj, ICMoveRight);
}

static void
move_backward(obj)
CcWnnObject obj;
{
    move_general(obj, ICMoveLeft);
}

static void
move_top(obj)
CcWnnObject obj;
{
    move_general(obj, ICMoveLeftMost);
}

static void
move_bottom(obj)
CcWnnObject obj;
{
    move_general(obj, ICMoveRightMost);
}


/* 候補切替えファンクション群
 *	cand_next
 *	cand_next_s
 *	cand_prev
 *	cand_prev_s
 */

static void
cand_general(obj, small, type)
CcWnnObject obj;
int small;
int type;
{
    if (jcNext(JCBUF(obj), small, type) < 0) beep(obj);
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}
    
static void
cand_next(obj)
CcWnnObject obj;
{
    cand_general(obj, 0, JC_NEXT);
}

static void
cand_next_s(obj)
CcWnnObject obj;
{
    cand_general(obj, 1, JC_NEXT);
}

static void
cand_prev(obj)
CcWnnObject obj;
{
    cand_general(obj, 0, JC_PREV);
}

static void
cand_prev_s(obj)
CcWnnObject obj;
{
    cand_general(obj, 1, JC_PREV);
}


/* 文節長変更ファンクション群
 *	expand_cl
 *	expand_cl_s
 *	expand_cl2
 *	expand_cl2_s
 *	shrink_cl
 *	shrink_cl_s
 *	shrink_cl2
 *	shrink_cl2_s
 */

static void
expand_shrink_general(obj, shrink, small, conv)
CcWnnObject obj;
int shrink;
int small;
int conv;
{
    int status;

    normalState(obj);

    if (shrink) {
	status = jcShrink(JCBUF(obj), small, conv);
    } else {
	status = jcExpand(JCBUF(obj), small, conv);
    }
    if (status < 0) beep(obj);

    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}

static void
expand_cl(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 0, 0, 1);
}

static void
expand_cl_s(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 0, 1, 1);
}

static void
shrink_cl(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 1, 0, 1);
}

static void
shrink_cl_s(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 1, 1, 1);
}

static void
expand_cl2(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 0, 0, 0);
}

static void
expand_cl2_s(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 0, 1, 0);
}

static void
shrink_cl2(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 1, 0, 0);
}

static void
shrink_cl2_s(obj)
CcWnnObject obj;
{
    expand_shrink_general(obj, 1, 1, 0);
}


/* 変換バッファクリアファンクション
 *	clear_buffer
 */

static void
clear_buffer(obj)
CcWnnObject obj;
{
    normalState(obj);
    if (jcClear(JCBUF(obj)) < 0) beep(obj);
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}


/* 候補移動ファンクション群
 *	sel_next
 *	sel_prev
 *	sel_top
 *	sel_bottom
 *	sel_forward
 *	sel_backward
 */

static void
sel_top(obj)
CcWnnObject obj;
{
    moveSelection(obj, ICMoveLeftMost);
}

static void
sel_bottom(obj)
CcWnnObject obj;
{
    moveSelection(obj, ICMoveRightMost);
}

static void
sel_forward(obj)
CcWnnObject obj;
{
    moveSelection(obj, ICMoveRight);
}

static void
sel_backward(obj)
CcWnnObject obj;
{
    moveSelection(obj, ICMoveLeft);
}

static void
sel_next(obj)
CcWnnObject obj;
{
    moveSelection(obj, ICMoveDown);
}

static void
sel_prev(obj)
CcWnnObject obj;
{
    moveSelection(obj, ICMoveUp);
}

static void
sel_select(obj)
CcWnnObject obj;
{
    endSelection(obj, False);
}

static void
sel_abort(obj)
CcWnnObject obj;
{
    endSelection(obj, True);
}

static void
fix(obj)
CcWnnObject obj;
{
    jcConvBuf *jcbuf = JCBUF(obj);

    obj->ccWnn.fixperformed = True;

    normalState(obj);
    if (jcFix(jcbuf) < 0) {
	beep(obj);
	return;
    }
    ccContextClear(CCBUF(obj));

    /* 辞書セーブの処理 */
    obj->ccWnn.fixcount++;
    if (obj->ccWnn.saveinterval > 0 &&
	obj->ccWnn.fixcount >= obj->ccWnn.saveinterval) {
	jcSaveDic(jcbuf);
	obj->ccWnn.fixcount = 0;
    }

    /* 確定の処理 */
    XtCallCallbackList((Widget)obj, obj->inputConv.fixcallback,
		       (XtPointer)NULL);	/* ??? */

    /* バッファをクリアする */
    jcClear(jcbuf);
    HINT(obj) = True;
}

static void
fix_cr(obj)
CcWnnObject obj;
{
    if (JCBUF(obj)->nClause == 0) {
	carriageret(obj);
    } else {
	fix(obj);
    }
}


static void
to_hankaku(start, end, res)
wchar *start;
wchar *end;
wchar *res;
{
    static unsigned short hiratohan[] = {	/* 全角かな <-> 半角かな */
#define D	(0xde<<8)	/* 濁音 */
#define H	(0xdf<<8)	/* 半濁音 */
	/* a */ 0xa7, 0xb1, 0xa8, 0xb2, 0xa9, 0xb3, 0xaa, 0xb4, 0xab, 0xb5,
	/* k */ 0xb6, 0xb6|D, 0xb7, 0xb7|D, 0xb8, 0xb8|D,
		0xb9, 0xb9|D, 0xba, 0xba|D,
	/* s */ 0xbb, 0xbb|D, 0xbc, 0xbc|D, 0xbd, 0xbd|D,
		0xbe, 0xbe|D, 0xbf, 0xbf|D,
	/* t */ 0xc0, 0xc0|D, 0xc1, 0xc1|D, 0xaf, 0xc2, 0xc2|D,
		0xc3, 0xc3|D, 0xc4, 0xc4|D,
	/* n */	0xc5, 0xc6, 0xc7, 0xc8, 0xc9,
	/* h */	0xca, 0xca|D, 0xca|H, 0xcb, 0xcb|D, 0xcb|H, 0xcc, 0xcc|D,
		0xcc|H, 0xcd, 0xcd|D, 0xcd|H, 0xce, 0xce|D, 0xce|H,
	/* m */ 0xcf, 0xd0, 0xd1, 0xd2, 0xd3,
	/* y */ 0xac, 0xd4, 0xad, 0xd5, 0xae, 0xd6,
	/* r */ 0xd7, 0xd8, 0xd9, 0xda, 0xdb,
	/* w */ 0xdc, 0xdc, 0xb2, 0xb4, 0xa6,
	/* n */ 0xdd
#undef D
#undef H
    };
    static struct symzenhan {
	unsigned short	zen;
	unsigned char	han;
    } kigoutohan[] = {				/* 全角記号 -> 半角記号 */
	0xa1a1, 0x20,	0xa1a2, 0xa4,	0xa1a3, 0xa1,	0xa1a4, 0x2c,
	0xa1a5, 0x2e,	0xa1a6, 0xa5,	0xa1a7, 0x3a,	0xa1a8, 0x3b,
	0xa1a9, 0x3f,	0xa1aa, 0x21,	0xa1ab, 0xde,	0xa1ac, 0xdf,
	0xa1b0, 0x5e,	0xa1b2, 0x5f,	0xa1bc, 0xb0,	0xa1bf, 0x2f,
	0xa1c1, 0x7e,	0xa1c3, 0x7c,	0xa1c6, 0x60,	0xa1c7, 0x27,
	0xa1c8, 0x22,	0xa1c9, 0x22,	0xa1ca, 0x28,	0xa1cb, 0x29,
	0xa1cc, 0x5b,	0xa1cd, 0x5d,	0xa1ce, 0x5b,	0xa1cf, 0x5d,
	0xa1d0, 0x7b,	0xa1d1, 0x7d,	0xa1d6, 0xa2,	0xa1d7, 0xa3,
	0xa1dc, 0x2b,	0xa1dd, 0x2d,	0xa1e1, 0x3d,	0xa1e3, 0x3c,
	0xa1e4, 0x3e,	0xa1ef, 0x5c,	0xa1f0, 0x24,	0xa1f3, 0x25,
	0xa1f4, 0x23,	0xa1f5, 0x26,	0xa1f6, 0x2a,	0xa1f7, 0x40,
    };
#define KIGOUSIZE	(sizeof(kigoutohan) / sizeof(struct symzenhan))
    register int c;
    
    while (start < end) {
	c = *start++;
	if (0xa1a1 <= c && c <= 0xa1fe) {		/* symbol */
	    register struct symzenhan *hi = kigoutohan + KIGOUSIZE;
	    register struct symzenhan *lo = kigoutohan;
	    register struct symzenhan *m;
	    register int dif;

	    while (lo <= hi) {
		m = lo + (hi - lo) / 2;
		if ((dif = c - m->zen) == 0) break;
		if (dif < 0) {
		    hi = m - 1;
		} else {
		    lo = m + 1;
		}
	    }
	    *res++ = (lo > hi) ? c : m->han;
	} else if (0xa3b0 <= c && c <= 0xa3b9) {	/* Numeric */
	    *res++ = c - 0xa3b0 + '0';
	} else if (0xa3c1 <= c && c <= 0xa3da) {	/* A-Z */
	    *res++ = c - 0xa3c1 + 'A';
	} else if (0xa3e1 <= c && c <= 0xa3fa) {	/* a-z */
	    *res++ = c - 0xa3e1 + 'a';
	} else if (0xa4a1 <= c && c <= 0xa4f3) {	/* ひらがな */
	    c = hiratohan[c - 0xa4a1];
	    *res++ = c & 0xff;
	    if (c & 0xff00) *res++ = c >> 8;
	} else if (0xa5a1 <= c && c <= 0xa5f3) {	/* かたかな */
	    c = hiratohan[c - 0xa5a1];
	    *res++ = c & 0xff;
	    if (c & 0xff00) *res++ = c >> 8;
	} else {
	    *res++ = c;
	}
    }
    *res = 0;	/* NULL terminate */
}

static void
to_zenkaku(start, end, res)
wchar *start;
wchar *end;
wchar *res;
{
    static wchar hantozen[] = {	/* 半角 ⇒ 全角変換表 */
	/* C0 */
	0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007,
	0x0008, 0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f,
	0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017,
	0x0018, 0x0019, 0x001a, 0x001b, 0x001c, 0x001d, 0x001e, 0x001f,
	/* ASCII */
	0xa1a1, 0xa1aa, 0xa1c9, 0xa1f4, 0xa1f0, 0xa1f3, 0xa1f5, 0xa1c7,
	0xa1ca, 0xa1cb, 0xa1f6, 0xa1dc, 0xa1a4, 0xa1dd, 0xa1a5, 0xa1bf,
	0xa3b0, 0xa3b1, 0xa3b2, 0xa3b3, 0xa3b4, 0xa3b5, 0xa3b6, 0xa3b7,
	0xa3b8, 0xa3b9, 0xa1a7, 0xa1a8, 0xa1e3, 0xa1e1, 0xa1e4, 0xa1a9,
	0xa1f7, 0xa3c1, 0xa3c2, 0xa3c3, 0xa3c4, 0xa3c5, 0xa3c6, 0xa3c7,
	0xa3c8, 0xa3c9, 0xa3ca, 0xa3cb, 0xa3cc, 0xa3cd, 0xa3ce, 0xa3cf,
	0xa3d0, 0xa3d1, 0xa3d2, 0xa3d3, 0xa3d4, 0xa3d5, 0xa3d6, 0xa3d7,
	0xa3d8, 0xa3d9, 0xa3da, 0xa1ce, 0xa1ef, 0xa1cf, 0xa1b0, 0xa1b2,
	0xa1c6, 0xa3e1, 0xa3e2, 0xa3e3, 0xa3e4, 0xa3e5, 0xa3e6, 0xa3e7,
	0xa3e8, 0xa3e9, 0xa3ea, 0xa3eb, 0xa3ec, 0xa3ed, 0xa3ee, 0xa3ef,
	0xa3f0, 0xa3f1, 0xa3f2, 0xa3f3, 0xa3f4, 0xa3f5, 0xa3f6, 0xa3f7,
	0xa3f8, 0xa3f9, 0xa3fa, 0xa1d0, 0xa1c3, 0xa1d1, 0xa1c1, 0x007f,
	/* C1 */
	0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087,
	0x0088, 0x0089, 0x008a, 0x008b, 0x008c, 0x008d, 0x008e, 0x008f,
	0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
	0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f,
	/* KANA */
	0xa1a1, 0xa1a3, 0xa1d6, 0xa1d7, 0xa1a2, 0xa1a6, 0xa5f2, 0xa5a1,
	0xa5a3, 0xa5a5, 0xa5a7, 0xa5a9, 0xa5e3, 0xa5e5, 0xa5e7, 0xa5c3,
	0xa1bc, 0xa5a2, 0xa5a4, 0xa5a6, 0xa5a8, 0xa5aa, 0xa5ab, 0xa5ad,
	0xa5af, 0xa5b1, 0xa5b3, 0xa5b5, 0xa5b7, 0xa5b9, 0xa5bb, 0xa5bd,
	0xa5bf, 0xa5c1, 0xa5c4, 0xa5c6, 0xa5c8, 0xa5ca, 0xa5cb, 0xa5cc,
	0xa5cd, 0xa5ce, 0xa5cf, 0xa5d2, 0xa5d5, 0xa5d8, 0xa5db, 0xa5de,
	0xa5df, 0xa5e0, 0xa5e1, 0xa5e2, 0xa5e4, 0xa5e6, 0xa5e8, 0xa5e9,
	0xa5ea, 0xa5eb, 0xa5ec, 0xa5ed, 0xa5ef, 0xa5f3, 0xa1ab, 0xa1ac,
	/* undefined */
	0x00e0, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7,
	0x00e8, 0x00e9, 0x00ea, 0x00eb, 0x00ec, 0x00ed, 0x00ee, 0x00ef,
	0x00f0, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x00f7,
	0x00f8, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x00fd, 0x00fe, 0x00ff,
    };
    register int c;

    while (start < end) {
	c = *start++;
	if ((0x20 <= c && c <= 0x7e) || (0xa1 <= c && c <= 0xdf)) {
	    *res++ = hantozen[c];
	} else {
	      *res++ = c;
	}
    }
    *res = 0;	/* NULL terminate */
}

static void
zenkaku_hankaku(obj, hankaku)
CcWnnObject obj;
int hankaku;
{
    jcConvBuf *jcbuf = JCBUF(obj);
    
    normalState(obj);
    
    if (jcbuf->curClause != jcbuf->nClause) {
	jcClause *cinfo = jcbuf->clauseInfo;
	wchar *ks = cinfo[jcbuf->curLCStart].kanap;
	wchar *ke = cinfo[jcbuf->curLCEnd].kanap;
	wchar buf[256];

	if (hankaku) {
	    to_hankaku(ks, ke, buf);
	} else {
	    to_zenkaku(ks, ke, buf);
	}
	if (jcChangeClause(jcbuf, buf) < 0) beep(obj);
    }
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}

static void
zenkaku(obj)
CcWnnObject obj;
{
    zenkaku_hankaku(obj, 0);
}

static void
hankaku(obj)
CcWnnObject obj;
{
    zenkaku_hankaku(obj, 1);
}

static void
hiragana_katakana(obj, type)
CcWnnObject obj;
int type;
{
    normalState(obj);

    if (jcKana(JCBUF(obj), 0, type) < 0) beep(obj);
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}

static void
hiragana(obj)
CcWnnObject obj;
{
    hiragana_katakana(obj, JC_HIRAGANA);
}

static void
katakana(obj)
CcWnnObject obj;
{
    hiragana_katakana(obj, JC_KATAKANA);
}


static void
backspace(obj)
CcWnnObject obj;
{
    switch (obj->ccWnn.state) {
    case selection_l_state:
	endSelection(obj, False);
	(void)jcMove(JCBUF(obj), 0, JC_FORWARD);
	break;
    case selection_s_state:
	endSelection(obj, False);
	(void)jcMove(JCBUF(obj), 1, JC_FORWARD);
	break;
    case symbol_state:
	endSelection(obj, False);
	break;
    }
    ccContextDelete(CCBUF(obj));
    if (jcDeleteChar(JCBUF(obj), 1) < 0) beep(obj);
    HINT(obj) = True;
}

static void
delete(obj)
CcWnnObject obj;
{
    normalState(obj);
    if (jcDeleteChar(JCBUF(obj), 0) < 0) beep(obj);
    ccContextClear(CCBUF(obj));
    HINT(obj) = True;
}


static void
bell(obj)
CcWnnObject obj;
{
    XBell(XtDisplayOfObject((Widget)obj), 0);
}

static void
beep(obj)
CcWnnObject obj;
{
    if (JCBUF(obj)->nClause == 0) return;
    bell(obj);
}

static void
jiscode_begin(obj)
CcWnnObject obj;
{
    obj->ccWnn.inputmode = JIS_MODE;
}

static void
jiscode_end(obj)
CcWnnObject obj;
{
    obj->ccWnn.inputmode = OTHERS;
}

static void
kuten_begin(obj)
CcWnnObject obj;
{
    obj->ccWnn.inputmode = KUTEN_MODE;
}

static void
kuten_end(obj)
CcWnnObject obj;
{
    obj->ccWnn.inputmode = OTHERS;
}

static void
carriageret(obj)
CcWnnObject obj;
{
    insChar('\r', (caddr_t)obj);
    fix(obj);
}


static void
convend(obj)
CcWnnObject obj;
{
    fix(obj);
    XtCallCallbackList((Widget)obj, obj->inputConv.endcallback,
		       (XtPointer)NULL);
}


static void
send_back(obj)
CcWnnObject obj;
{
    obj->ccWnn.sendbackevent = True;
}


static void
sym_input(obj)
CcWnnObject obj;
{
    ICSelectionControlArg arg;

    if (obj->ccWnn.state != normal_state) {
	beep(obj);
	return;
    }
    obj->ccWnn.state = symbol_state;

    arg.command = ICSelectionStart;
    arg.u.selection_kind = ICSelectionSymbols;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
		       (XtPointer)&arg);

    arg.command = ICSelectionSet;
    arg.u.current_item = obj->ccWnn.cursymbol;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
		       (XtPointer)&arg);
}

static int
getSymbol(obj, n)
CcWnnObject obj;
int n;
{
    int c;

    if (n < 0 || n >= obj->ccWnn.numsymbols) return -1;

    c = *(wchar *)(obj->ccWnn.symbollist[n].data);

    return c;
}

static void
startSelection(obj, small)
CcWnnObject obj;
int small;
{
    ICSelectionControlArg arg;
    int ncand, curcand;

    if (obj->ccWnn.state != normal_state) {
	beep(obj);
	return;
    }

    if (jcCandidateInfo(JCBUF(obj), small, &ncand, &curcand) < 0) {
	beep(obj);
	return;
    }

    getAllCandidates(obj, ncand);

    obj->ccWnn.numcand = ncand;
    obj->ccWnn.curcand = curcand;
    obj->ccWnn.state = small ? selection_s_state : selection_l_state;

    arg.command = ICSelectionStart;
    arg.u.selection_kind = ICSelectionCandidates;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
		       (XtPointer)&arg);

    /* set current item */
    arg.command = ICSelectionSet;
    arg.u.current_item = curcand;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
		       (XtPointer)&arg);
}

static void
moveSelection(obj, dir)
CcWnnObject obj;
int dir;
{
    ICSelectionControlArg arg;

    if (obj->ccWnn.state == normal_state) return;
    arg.command = ICSelectionMove;
    arg.u.dir = dir;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
		       (XtPointer)&arg);
}

static int
endSelection(obj, abort)
CcWnnObject obj;
int abort;
{
    ICSelectionControlArg arg;
    int selected;
    int ret = 0;

    if (obj->ccWnn.selectionending) return 0;

    if (obj->ccWnn.state == normal_state) return -1;

    arg.command = ICSelectionEnd;
    arg.u.current_item = -1;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
		       (XtPointer)&arg);

    if (!abort && (selected = arg.u.current_item) >= 0) {
	ret = insertSelection(obj, selected);
    }
    obj->ccWnn.state = normal_state;

    return ret;
}

static int
insertSelection(obj, selected)
CcWnnObject obj;
int selected;
{
    int state = obj->ccWnn.state;
    int ret = 0;

    HINT(obj) = True;

    obj->ccWnn.selectionending = True;
    if (state == symbol_state) {
	int c = getSymbol(obj, selected);
	if (c < 0) return -1;
	obj->ccWnn.cursymbol = selected;
	ccContextAppend(CCBUF(obj), c);
	insChar(c, (caddr_t)obj);
    } else {
	obj->ccWnn.curcand = selected;
	ret = jcSelect(JCBUF(obj), selected);
    }
    obj->ccWnn.selectionending = False;

    return ret;
}

static void
normalState(obj)
CcWnnObject obj;
{
    switch (obj->ccWnn.state) {
    case selection_l_state:
    case selection_s_state:
	/* 候補選択中であればカレントの候補を選択して選択モードから抜ける */
    case symbol_state:
	/* 記号入力中であればカレントの記号を選択して記号モードから抜ける */
	endSelection(obj, False);
	break;
    }
}

static void
allocCandlist(obj, n)
CcWnnObject obj;
int n;
{
    ICString *p;

    if (n <= obj->ccWnn.candlistsize) return;

    if (obj->ccWnn.candlistsize == 0) {
	p = (ICString *)XtMalloc(n * sizeof(ICString));
    } else {
	p = (ICString *)XtRealloc((char *)obj->ccWnn.candlist,
				  n * sizeof(ICString));
    }

    obj->ccWnn.candlist = p;
    obj->ccWnn.candlistsize = n;
}

static void
allocStrdata(obj, nchars)
CcWnnObject obj;
Cardinal nchars;
{
    wchar *p;

    if (nchars <= obj->ccWnn.strdatasize) return;

    if (obj->ccWnn.strdatasize == 0) {
	if (nchars < 256) nchars = 256;
	p = (wchar *)XtMalloc(nchars * sizeof(wchar));
    } else {
	if (nchars - obj->ccWnn.strdatasize < 256)
	    nchars = obj->ccWnn.strdatasize + 256;
	p = (wchar *)XtRealloc((char *)obj->ccWnn.strdata,
				 nchars * sizeof(wchar));
    }

    obj->ccWnn.strdata = p;
    obj->ccWnn.strdatasize = nchars;
}

static void
getAllCandidates(obj, ncand)
CcWnnObject obj;
int ncand;
{
    ICString *strp;
    Cardinal nchars;
    wchar *p;
    int i;
    wchar buf[256];

    allocCandlist(obj, ncand);

    nchars = 0;
    for (i = 0, strp = obj->ccWnn.candlist; i < ncand; i++, strp++) {
	(void)jcGetCandidate(obj->ccWnn.jcbuf, i, buf);
	strp->nchars = wstrlen(buf);
	strp->nbytes = strp->nchars * sizeof(wchar);
	strp->attr = ICAttrNormalString;
	allocStrdata(obj, nchars + strp->nchars);
	(void)bcopy((char *)buf, (char *)(obj->ccWnn.strdata + nchars),
		    strp->nbytes);
	nchars += strp->nchars;
    }

    p = obj->ccWnn.strdata;
    for (i = 0, strp = obj->ccWnn.candlist; i < ncand; i++, strp++) {
	strp->data = (char *)p;
	p += strp->nchars;
    }
}

/*
 * keeping list of objects
 */
typedef struct _oblist_ {
    CcWnnObject obj;
    struct _oblist_ *next;
} ObjRec;

static ObjRec *ObjList = NULL;

static void
addObject(obj)
CcWnnObject obj;
{
    ObjRec *objp = XtNew(ObjRec);

    objp->obj = obj;
    objp->next = ObjList;
    ObjList = objp;
}

static void
deleteObject(obj)
CcWnnObject obj;
{
    ObjRec *objp, *objp0;

    for (objp0 = NULL, objp = ObjList;
	 objp != NULL;
	 objp0 = objp, objp = objp->next) {
	if (objp->obj == obj) {
	    if (objp0 == NULL) {
		ObjList = objp->next;
	    } else {
		objp0->next = objp->next;
	    }
	    XtFree((char *)objp);
	    return;
	}
    }
}

static void
serverDead(server)
WNN_JSERVER_ID *server;
{
    ObjRec *objp = ObjList;

    while (objp != NULL) {
	if (objp->obj->ccWnn.wnnenv != NULL &&
	    objp->obj->ccWnn.wnnenv->js_id == server) {
	    if (objp->obj->ccWnn.jcbuf != NULL) {
		(void)jcDestroyBuffer(objp->obj->ccWnn.jcbuf, 0);
	    }
	    (void)jiDeleteEnv(objp->obj->ccWnn.wnnenv);
	    (void)jiCloseServer(server);
	    objp->obj->ccWnn.wnnenv = NULL;
	    objp->obj->ccWnn.jcbuf = NULL;
	    if (objp->obj->ccWnn.ccbuf != NULL) {
		ccContextClear(objp->obj->ccWnn.ccbuf);
	    }
	}
	objp = objp->next;
    }
}

static void
saveData(obj)
CcWnnObject obj;
{
    wchar *wbuf;
    int len;
    jcConvBuf *jcbuf = obj->ccWnn.jcbuf;

    len = jcbuf->kanaEnd - jcbuf->kanaBuf;
    if (len <= 0) return;

    wbuf = (wchar *)XtMalloc((len + 1) * sizeof(wchar));
    (void)bcopy((char *)jcbuf->kanaBuf, (char *)wbuf,
		sizeof(wchar) * (len + 1));
    wbuf[len] = 0;
    obj->ccWnn.pendingdata = wbuf;
}

static void
restoreData(obj)
CcWnnObject obj;
{
    wchar *wp = obj->ccWnn.pendingdata;

    if (wp == NULL) return;

    while (*wp != 0) {
	jcInsertChar(obj->ccWnn.jcbuf, (int)*wp++);
    }
    XtFree((char *)obj->ccWnn.pendingdata);

    obj->ccWnn.pendingdata = NULL;
    obj->ccWnn.textchanged = True;
}
