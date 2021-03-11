#ifndef lint
static char *rcsid = "$Header: Sj3.c,v 2.7 93/09/21 14:32:05 nao Exp $";
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
/*
 * Copyright 1991 Sony Corporation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Sony not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Sony makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * SONY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL SONY
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/*
 * Author: Naoshi Suzuki, SONY Corporation.  (nao@sm.sony.co.jp)
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#if XtSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif
#include <X11/Xmu/Atoms.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#ifdef X_LOCALE
#include <X11/Xlocale.h>
#else /* X_LOCALE */
#include <locale.h>
#endif /* X_LOCALE */
#include <pwd.h>
#include "Sj3P.h"

static XtResource resources[] = {
#define offset(field) XtOffset(Sj3Object, sj3.field)
    { XtNsj3serv, XtCSj3serv, XtRString, sizeof(String),
    offset(sj3serv), XtRString, NULL },
    { XtNsj3serv2, XtCSj3serv2, XtRString, sizeof(String),
    offset(sj3serv2), XtRString, NULL },
    { XtNsj3user, XtCSj3user, XtRString, sizeof(String),
    offset(sj3user), XtRString, NULL },
    { XtNrcfile, XtCRcfile, XtRString, sizeof(String),
    offset(rcfile), XtRString, NULL },
    { XtNsbfile, XtCSbfile, XtRString, sizeof(String),
    offset(sbfile), XtRString, NULL },
    { XtNrkfile, XtCRkfile, XtRString, sizeof(String),
    offset(rkfile), XtRString, NULL },
    { XtNhkfile, XtCHkfile, XtRString, sizeof(String),
    offset(hkfile), XtRString, NULL },
    { XtNzhfile, XtCZhfile, XtRString, sizeof(String),
    offset(zhfile), XtRString, NULL },
#undef offset
};

static void             ClassInitialize();
static void             Initialize(),   Destroy();
static Boolean          SetValues();
static int              InputEvent();
static ICString        *GetMode();
static int              CursorPos();
static int              NumSegments();
static ICString        *GetSegment();
static int              CompareSegment();
static ICString        *GetItemList();
static int              SelectItem();
static int              ConvertedString();
static int              ClearConversion();
static ICString        *GetAuxSegments();

Sj3ClassRec sj3ClassRec = {
  { /* object fields */
    /* superclass       */          (WidgetClass) &inputConvClassRec,
    /* class_name       */          "Sj3",
    /* widget_size      */          sizeof(Sj3Rec),
    /* class_initialize */          ClassInitialize,
    /* class_part_initialize    */  NULL,
    /* class_inited     */          FALSE,
    /* initialize       */          Initialize,
    /* initialize_hook  */          NULL,
    /* obj1             */          NULL,
    /* obj2             */          NULL,
    /* obj3             */          0,
    /* resources        */          resources,
    /* num_resources    */          XtNumber(resources),
    /* xrm_class        */          NULLQUARK,
    /* obj4             */          FALSE,
    /* obj5             */          FALSE,
    /* obj6             */          FALSE,
    /* obj7             */          FALSE,
    /* destroy          */          Destroy,
    /* obj8             */          NULL,
    /* obj9             */          NULL,
    /* set_values       */          SetValues,
    /* set_values_hook  */          NULL,
    /* obj10            */          NULL,
    /* get_values_hook  */          NULL,
    /* obj11            */          NULL,
    /* version          */          XtVersion,
    /* callback_private */          NULL,
    /* obj12            */          NULL,
    /* obj13            */          NULL,
    /* obj14            */          NULL,
    /* extension        */          NULL
  },
  { /* inputConv fields */
    /* InputEvent       */          InputEvent,
    /* GetMode          */          GetMode,
    /* CursorPos        */          CursorPos,
    /* NumSegments      */          NumSegments,
    /* GetSegment       */          GetSegment,
    /* CompareSegment   */          CompareSegment,
    /* GetItemList      */          GetItemList,
    /* SelectItem       */          SelectItem,
    /* GetConvertedString   */      ConvertedString,
    /* ClearConversion  */          ClearConversion,
    /* GetAuxSegments   */          GetAuxSegments,
    /* SupportMultipleObjects   */  True,
    /* NoMoreObjects    */          False,
  },
  { /* sj3 fields */
    /* foo              */          (int)NULL,
  }
};

WidgetClass sj3ObjectClass = (WidgetClass)&sj3ClassRec;

static void             startCandidate();
static void             startSymbol();
static void             startHinsi();
static void             moveSelection();
static int              endSelection();
static int              insertSelection();
static void             hinsiInit();
static void             symbolInit();
static void             allocCandlist();
static void             startRegistration();
static void             changeRegistration();
static void             endRegistration();
static void             setLocale();
static void             setUser();

static void             addObject();
static void             deleteObject();
static void             bell();

static ICString        *SymbolList = NULL;
static int              NumSymbol;
static ICString        *HinsiList = NULL;
static int              NumHinsi;

static int              clcount = 0;

static int              usr_code;
static char             home[256];
static char             uname[32];

/*
 * ClassInitialize()
 *  Initialize common resource.
 */
static void
ClassInitialize()
{
    setLocale();
    setUser();

    Xsj3cSetInLang(usr_code);
    Xsj3cSetOutLang(JP_EUC);
}

/*
 * setLocale()
 *  Set locale and decide file code for all set-up files.
 */
static void
setLocale()
{
    char    *loc;

#ifdef X_LOCALE
    if (loc = _Xsetlocale (LC_CTYPE, "")) {
#else /* X_LOCALE */
    if (loc = setlocale (LC_CTYPE, "")) {
#endif /* X_LOCALE */
        if (!strcmp(loc, "ja_JP.SJIS")||!strcmp(loc, "ja_JP.mscode"))
            usr_code = JP_SJIS;
        else if (!strcmp(loc, "ja_JP.jis7"))
            usr_code = JP_JIS7;
        else if (!strcmp(loc, "ja_JP.jis8"))
            usr_code = JP_JIS8;
        else 
            usr_code = JP_EUC;
    } else
        usr_code = JP_EUC;
#ifdef FORCE_SJIS
    usr_code = JP_SJIS;
#endif
#ifdef FORCE_JIS8
    usr_code = JP_JIS8;
#endif
#ifdef FORCE_JIS7
    usr_code = JP_JIS7;
#endif
}

/*
 * setUser()
 *  Set user name and home directory.
 */
static void
setUser()
{
    extern char             *getenv(),  *getlogin();
    char                    *login;
    struct passwd           *pwd,   *getpwnam(),    *getpwuid();


    if (login = getlogin())
        strcpy(uname, login);
    setpwent();
    if (!uname || *uname == '\0') {
        if (pwd = getpwuid(getuid())) {
            strcpy(uname, pwd->pw_name);
        }
    } else {
        pwd = getpwnam(uname);
    }
    if (pwd)
        strcpy(home, pwd->pw_dir);
    else
        strcpy(home, getenv("HOME"));
    endpwent();
}

/*
 * InputEvent()
 *  KeyPress event dispatch routine
 */
static int
InputEvent(w, ev)
    Widget      w;
    XEvent     *ev;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    int                     ret = 0,    len,    nbytes;
    unsigned char           *pre;
    unsigned long           modmask;
    KeySym                  ks;
    register Xsj3cEvent     value,      select,     dict;

    /* KeyPress以外は捨てる */
    if (ev->type != KeyPress)
        return ret;
    
    pre = Xsj3cGetPreeditArea(buf, &len);

    /* イベントを文字列及び KeySym に変換する */
    nbytes = XmuLookupKana((XKeyPressedEvent *)ev, (char *)pre, len, &ks, NULL);
    modmask = ev->xkey.state & 0xff;

    value = Xsj3cKeyConv(buf, nbytes, modmask, ks);

    if (select = (value & KEY_SELECT)) {
        switch (select) {
        case KEY_CAND_START:
            startCandidate(obj);
            break;
        case KEY_SYMBOL_START:
            startSymbol(obj);
            break;
        case KEY_HINSI_START:
            startHinsi(obj);
            break;
        case KEY_SELECT_RIGHT:
            moveSelection(obj, ICMoveRight);
            break;
        case KEY_SELECT_LEFT:
            moveSelection(obj, ICMoveLeft);
            break;
        case KEY_SELECT_UP:
            moveSelection(obj, ICMoveUp);
            break;
        case KEY_SELECT_DOWN:
            moveSelection(obj, ICMoveDown);
            break;
        case KEY_SELECT_FIRST:
            moveSelection(obj, ICMoveFirst);
            break;
        case KEY_SELECT_LAST:
            moveSelection(obj, ICMoveLast);
            break;
        case KEY_SELECT_NEXTP:
            moveSelection(obj, ICMoveNextPage);
            break;
        case KEY_SELECT_PREVP:
            moveSelection(obj, ICMovePrevPage);
            break;
        case KEY_SELECT_RIGHTMOST:
            moveSelection(obj, ICMoveRightMost);
            break;
        case KEY_SELECT_LEFTMOST:
            moveSelection(obj, ICMoveLeftMost);
            break;
        case KEY_SELECT_END:
            endSelection(obj, False);
            break;
        case KEY_SELECT_ABORT:
            endSelection(obj, True);
            break;
        default:
            break;
        }
    } 
    if (dict = (value & KEY_DICT)) {
        switch (dict) {
        case KEY_DICT_START:
            startRegistration(obj);
            break;
        case KEY_DICT_CHANGE:
            changeRegistration(obj);
            break;
        case KEY_DICT_REGISTER:
            Xsj3cDictRegister(buf);
            changeRegistration(obj);
            break;
        case KEY_DICT_CLEAR:
            Xsj3cDictClear(buf);
            changeRegistration(obj);
            break;
        case KEY_DICT_END:
            endRegistration(obj);
            break;
        default:
            break;
        }
    }
    if (value & KEY_CONTROL || value == KEY_NULL)
        ret = 1;
    if (value & KEY_CHANGE){
        if (value & KEY_MODE_CHANGE) {
        /* Change display mode string       */
            XtCallCallbackList(w, obj->inputConv.modechangecallback,
               (XtPointer)NULL);
        }
        if (value & KEY_TEXT_FIXED) {
        /* Fix converting strings           */
            XtCallCallbackList(w, obj->inputConv.fixcallback,
               (XtPointer)NULL);
            Xsj3cFixBuffer(buf);
        } else if (value & KEY_TEXT_FLUSH) {
        /* Fix & Input strings at same time */
            XtCallCallbackList(w, obj->inputConv.fixcallback,
               (XtPointer)NULL);
            Xsj3cFlushBuffer(buf);
        }
        if (value & KEY_TEXT_CHANGE) {
        /* Change  converting strings       */
            XtCallCallbackList(w, obj->inputConv.textchangecallback,
               (XtPointer)NULL);
        }
        if (value & KEY_HENKAN_END) {
        /* End conversion                   */
            XtCallCallbackList((Widget)obj, obj->inputConv.endcallback,
                   (XtPointer)NULL);
            Xsj3cClearBuffer(buf);
        }
    } 
    if  (value & KEY_BELL){
        bell(obj);
        return ret;
    } else if (value & KEY_RECONNECT) {
        Xsj3cConnect(buf, obj->sj3.sj3serv,
                obj->sj3.sj3serv2, obj->sj3.sj3user);
        Xsj3cClearBuffer(buf);
        return ret;
    }
    return ret;
}

static ICString *
GetMode(w)
    Widget                  w;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    int                     len;
    static ICString         icstr;

    icstr.data = (char *)Xsj3cGetModeStr(buf, &len);
    icstr.nchars = len;
    icstr.nbytes = icstr.nchars * sizeof(wchar);
    icstr.attr = ICAttrNormalString;
    return &icstr;
}

static int
CursorPos(w, nsegp, ncharp)
    Widget                  w;
    Cardinal                *nsegp;
    Cardinal                *ncharp;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;

    return(Xsj3cGetPosition(buf, nsegp, ncharp));
}

static int
NumSegments(w)
Widget                      w;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;

    return (Xsj3cGetSegNum(buf));
}

static ICString *
GetSegment(w, n)
    Widget                  w;
    Cardinal                n;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    static                  ICString seg;
    int                     len, attr;

    seg.data = (char *)Xsj3cGetSeg(buf, n, &len, &attr);
    seg.nchars = len;
    seg.nbytes = seg.nchars * sizeof(wchar);
    switch (attr) {
    case SEG_REVERSED:
        seg.attr = ICAttrConverted|ICAttrCurrentSegment;
        break;
    case SEG_UNDER_LINE:
        seg.attr = ICAttrNotConverted;
        break;
    case SEG_NORMAL:
        seg.attr = ICAttrConverted;
        break;
    default:
        seg.attr = ICAttrConverted;
        break;
    }

    return &seg;
}

/* ARGSUSED */
static int
CompareSegment(w, seg1, seg2, n)
    Widget                  w;
    ICString               *seg1;
    ICString               *seg2;
    Cardinal               *n;
{
    register unsigned char  *p,     *q;
    register int            len,    nsame = 0;
    int                     result = ICSame;

    if (seg1->attr != seg2->attr)
        result |= ICAttrChanged;

    len = seg1->nbytes > seg2->nbytes ? seg2->nbytes : seg1->nbytes;
    p = (unsigned char *)seg1->data;
    q = (unsigned char *)seg2->data;
    while (nsame < len && *p++ == *q++) nsame++;

    if (nsame != len || len != seg1->nbytes
        || len != seg2->nbytes || seg1->data != seg2->data)
        result |= ICStringChanged;

    if (n)
        *n = nsame / sizeof(wchar);

    return result;

}

static ICString *
GetItemList(w, n)
    Widget                  w;
    Cardinal               *n;
{
    Sj3Object               obj = (Sj3Object)w;
    
    switch (obj->sj3.state) {
    case candidate_state:
        *n = obj->sj3.numcand;
        return obj->sj3.candlist;
    case symbol_state:
        *n = NumSymbol;
        return obj->sj3.symbollist;
    case hinsi_state:
        *n = NumHinsi;
        return obj->sj3.hinsilist;
    default:
        *n = 0;
        return NULL;    /* no item available */
    }
    /* NOTREACHED */

}

static int
SelectItem(w, n)
    Widget                  w;
    int                     n;
{
    Sj3Object   obj = (Sj3Object)w;
    Xsj3cBuf    buf = obj->sj3.sj3buf;
    int         ret = 0,    changed = False,    flush = False;

    if (obj->sj3.state == normal_state )
        return -1;
    else if (n >= 0)
        ret = insertSelection(obj, n, &changed, &flush);
    
    switch (obj->sj3.state) {
    case candidate_state:
        Xsj3cEndCandidate(buf, changed);
        break;
    case symbol_state:
        Xsj3cEndSymbol(buf);
        break;
    case hinsi_state:
        Xsj3cEndHinsi(buf);
        break;
    default:
        XtAppWarning(XtWidgetToApplicationContext((Widget)obj),
             "sj3 Object: Unknown ConvMode state");
        break;
    }
    obj->sj3.state = normal_state;

    if (changed) {
        if (flush) {
            XtCallCallbackList((Widget)obj, obj->inputConv.fixcallback,
               (XtPointer)NULL);
            Xsj3cFlushBuffer(buf);
        }
        XtCallCallbackList((Widget)obj,
               obj->inputConv.textchangecallback,
               (XtPointer)NULL);
    }

    return ret;

}

static int
ConvertedString(w, encoding, format, length, string)
    Widget                  w;
    Atom                   *encoding;
    int                    *format;
    int                    *length;
    XtPointer              *string;
{
    Sj3Object   obj = (Sj3Object)w;
    Xsj3cBuf    buf = obj->sj3.sj3buf;
    wchar      *wbuf, *wp;
    wchar      *data;
    int         len, wlen;
    extern int  convJWStoCT();

    wlen = Xsj3cGetConvertedLength(buf);
    wbuf = (wchar *)XtMalloc((wlen + 1) * sizeof(wchar));

    if ((Xsj3cGetConvertedStr(buf, wbuf)) == NULL) {
        XtAppWarning(XtWidgetToApplicationContext(w),
             "sj3 Object: Could not get converted string");
        return -1;
    }
    *encoding = XA_COMPOUND_TEXT(XtDisplayOfObject((Widget)obj));
    *format = 8;

    for (wp = wbuf; *wp != 0; wp++) {
        if (*wp == '\r') *wp = '\n';
    }

    *length = len = convJWStoCT(wbuf, (unsigned char *)NULL, 0);
    *string = XtMalloc(len + 1);
    (void)convJWStoCT(wbuf, (unsigned char *)*string, 0);

    XtFree((char *)wbuf);
    
    return 0;
}

static int
ClearConversion(w)
    Widget                  w;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;

    Xsj3cClearBuffer(buf);
    XtCallCallbackList(w, obj->inputConv.textchangecallback, (XtPointer)NULL);
    return 0;
}

static ICString *
GetAuxSegments(w, n, ns, nc)
    Widget                  w;
    Cardinal               *n, *ns, *nc;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    register int            i;
    register Xsj3cDictMsg   p;
    register ICString      *seg;
    static ICString        *ics;

    *n = Xsj3cGetDictMsgNum(buf);
    if (!ics) {
        ics = (ICString *)XtCalloc(*n, sizeof(ICString));
    } else { 
        ics = (ICString *)XtRealloc((char *)ics, *n * sizeof(ICString));
    }
    bzero(ics, *n * sizeof(ICString));
    for (i = 0, seg = ics, p = Xsj3cGetDictMsgs(buf);
            i < *n; i++, seg++) {
        seg->data = (char *)p[i].data;
        seg->nchars = p[i].len;
        seg->nbytes = seg->nchars * sizeof(wchar);
        switch (p[i].attr) {
        case SEG_REVERSED:
            seg->attr = ICAttrConverted|ICAttrCurrentSegment;
            break;
        case SEG_UNDER_LINE:
            seg->attr = ICAttrNotConverted;
            break;
        case SEG_NORMAL:
            seg->attr = ICAttrConverted;
            break;
        default:
            seg->attr = ICAttrNotConverted;
            break;
        }
    }
    *ns = *n - 1;
    *nc = 0;

    return ics;
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget                  req;
    Widget                  new;
    ArgList                 args;
    Cardinal               *num_args;
{
    Sj3Object               obj = (Sj3Object)new;
    Xsj3cBuf                buf = NULL;
    int                     min_keycode,  max_keycode,  keysyms_per_keycode;
    register int            i,  j,  k;
    unsigned long           kanamod = 0;
    KeySym                 *keymap;
    XModifierKeymap        *modmap;

    obj->sj3.symbollist = SymbolList;
    obj->sj3.hinsilist = HinsiList;
    obj->sj3.candlist = NULL;
    obj->sj3.numcand = 0;
    obj->sj3.curcand = 0;
    obj->sj3.cursymbol = 0;
    obj->sj3.curhinsi = 0;
    obj->sj3.candlistsize = 0;
    obj->sj3.state = normal_state;
    obj->sj3.selectionending = False;

    if (!obj->sj3.sj3user || *obj->sj3.sj3user == '\0') 
        obj->sj3.sj3user = uname;

    if (!clcount++) {
        /* Get kana lock modmask   */
        XDisplayKeycodes (XtDisplayOfObject((Widget)obj),
                &min_keycode, &max_keycode);
        keymap = XGetKeyboardMapping (XtDisplayOfObject((Widget)obj),
                min_keycode, (max_keycode - min_keycode + 1),
                &keysyms_per_keycode);
        XFree(keymap);

        if (keysyms_per_keycode == 4) {
            modmap = XGetModifierMapping(XtDisplayOfObject((Widget)obj));
            k = 0;
            for (i = 0; i < 8; i++) {
                for (j = 0; j < modmap->max_keypermod; j++) {
                    if (XK_Mode_switch ==
                        XKeycodeToKeysym(XtDisplayOfObject((Widget)obj),
                            modmap->modifiermap[k], 0)) {
                        kanamod |= 1 << i;
                    }
                    k++;
                }
            }
            XFreeModifiermap(modmap);
        }

        /* Set kana lock modmask   */
        Xsj3cSetKanaMod(kanamod);

    }

    /* Making buffer for Xsj3clib   */
    buf = obj->sj3.sj3buf = Xsj3cCreateBuffer();
    if (!buf) {
        XtAppError(XtWidgetToApplicationContext(new),
            "sj3 Object: Failed to allocate buffers");
    }

    /* Read user resource customize file and set flags    */
    (void)Xsj3cRCInit(buf, obj->sj3.rcfile, home);

    /* Convertion table initialization    */
    Xsj3cInitializeTables(buf, home, obj->sj3.rkfile, obj->sj3.hkfile,
            obj->sj3.zhfile, obj->sj3.sbfile);

    /* Connect to Kana-kanji conversion server  */
    if ((Xsj3cOpen(buf, obj->sj3.sj3serv,
                obj->sj3.sj3user, False, False)) != CONNECT_OK) {
        XtAppWarning(XtWidgetToApplicationContext(new),
     "sj3 Object: Failed to connect first server, then try to second server");
        if ((Xsj3cOpen(buf, obj->sj3.sj3serv2,
                obj->sj3.sj3user, False, True)) != CONNECT_OK) {
            XtAppError(XtWidgetToApplicationContext(new),
                 "sj3 Object: Failed to connect to second server");
        }
    };
    
    addObject(obj);
}

static void
Destroy(w)
    Widget                  w;
{
    Sj3Object               obj = (Sj3Object)w;
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    
    Xsj3cClose(buf, False);
    Xsj3cFreeBuffer(buf);
    deleteObject(obj);
}

static void
symbolInit(obj)
    Sj3Object obj;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    register ICString      *strp;
    register int            i;
    register Xsj3cSymbol    p;

    for (i = 0, strp = SymbolList, p = Xsj3cGetSymbols(buf);
            i < NumSymbol; i++, strp++) {
        strp->data = (char *)p[i].data;
        strp->nchars = p[i].len;
        strp->nbytes = strp->nchars * sizeof(wchar);
        strp->attr = ICAttrNormalString;
    }
}

static void
startSymbol(obj)
    Sj3Object obj;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    ICSelectionControlArg   arg;
    
    /* Symbol list initialization   */
    if (!SymbolList) {
        NumSymbol = Xsj3cGetSymbolNum(buf, &obj->sj3.cursymbol);
        SymbolList = (ICString *)XtMalloc(NumSymbol * sizeof(ICString));
        symbolInit(obj);
        obj->sj3.symbollist = SymbolList;
    } else if (!obj->sj3.symbollist) {
        obj->sj3.symbollist = SymbolList;
    }

    obj->sj3.state = symbol_state;

    arg.command = ICSelectionStart;
    arg.u.selection_kind = ICSelectionCandidates;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);

    /* set current item */
    arg.command = ICSelectionSet;
    arg.u.current_item = obj->sj3.cursymbol;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);
}

static void
startCandidate(obj)
    Sj3Object obj;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    ICSelectionControlArg   arg;
    register ICString      *strp;
    register int            i;
    int                     ncand,  curcand;
    register Xsj3cCand      p;
    
    if ((ncand = Xsj3cGetCandidateNum(buf, &curcand)) <= 0) {
        bell(obj);
        return;
    }
    obj->sj3.curcand = curcand;
    obj->sj3.numcand = ncand;

    allocCandlist(obj, obj->sj3.numcand);
    
    for (i = 0, strp = obj->sj3.candlist, p = Xsj3cGetCandidates(buf);
            i < obj->sj3.numcand; i++, strp++) {
        strp->data = (char *)p[i].data;
        strp->nchars = p[i].len;
        strp->nbytes = strp->nchars * sizeof(wchar);
        strp->attr = ICAttrNormalString;
    }

    obj->sj3.state = candidate_state;

    arg.command = ICSelectionStart;
    arg.u.selection_kind = ICSelectionCandidates;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);

    /* set current candidate */
    arg.command = ICSelectionSet;
    arg.u.current_item = curcand;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);
}

static void
hinsiInit(obj)
    Sj3Object obj;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    register ICString      *strp;
    register int            i;
    register Xsj3cHinsi     p;

    for (i = 0, strp = HinsiList, p = Xsj3cGetHinsis(buf);
            i < NumHinsi; i++, strp++) {
        strp->data = (char *)p[i].data;
        strp->nchars = p[i].len;
        strp->nbytes = strp->nchars * sizeof(wchar);
        strp->attr = ICAttrNormalString;
    }
}

static void
startHinsi(obj) 
    Sj3Object obj;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    ICSelectionControlArg   arg;
    
    /* Hinsi list initialization    */
    if (!HinsiList) {
        NumHinsi = Xsj3cGetHinsiNum(buf, &obj->sj3.curhinsi);
        HinsiList = (ICString *)XtMalloc(NumHinsi * sizeof(ICString));
        hinsiInit(obj);
        obj->sj3.hinsilist = HinsiList;
    } else if (!obj->sj3.hinsilist) {
        obj->sj3.hinsilist = HinsiList;
    }

    obj->sj3.state = hinsi_state;

    arg.command = ICSelectionStart;
    arg.u.selection_kind = ICSelectionCandidates;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);

    /* set current item */
    arg.command = ICSelectionSet;
    arg.u.current_item = obj->sj3.curhinsi;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);
}

static void
moveSelection(obj, dir)
    Sj3Object               obj;
    int                     dir;
{
    ICSelectionControlArg   arg;

    if (obj->sj3.state == normal_state) return;
    arg.command = ICSelectionMove;
    arg.u.dir = dir;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);
}

static int
endSelection(obj, abort)
    Sj3Object               obj;
    int                     abort;
{
    ICSelectionControlArg   arg;
    int                     selected;
    int                     ret = 0,    changed = False,    flush = False;
    Xsj3cBuf                buf = obj->sj3.sj3buf;

    if (obj->sj3.selectionending)
        return 0;

    if (obj->sj3.state == normal_state)
        return -1;

    arg.command = ICSelectionEnd;
    arg.u.current_item = -1;
    XtCallCallbackList((Widget)obj, obj->inputConv.selectioncallback,
               (XtPointer)&arg);

    if (!abort && (selected = arg.u.current_item) >= 0) {
        ret = insertSelection(obj, selected, &changed, &flush);
    }

    switch (obj->sj3.state) {
    case candidate_state:
        Xsj3cEndCandidate(buf, changed);
        break;
    case symbol_state:
        Xsj3cEndSymbol(buf);
        break;
    case hinsi_state:
        Xsj3cEndHinsi(buf);
        break;
    default:
        XtAppWarning(XtWidgetToApplicationContext((Widget)obj),
             "sj3 Object: Unknow ConvMode state");
        break;
    }
    obj->sj3.state = normal_state;

    if (changed) {
        if (flush) {
            XtCallCallbackList((Widget)obj, obj->inputConv.fixcallback,
               (XtPointer)NULL);
            Xsj3cFlushBuffer(buf);
        }
        XtCallCallbackList((Widget)obj,
               obj->inputConv.textchangecallback,
               (XtPointer)NULL);
    }

    return ret;
}

/* ARGSUSED */
static Boolean
SetValues(cur, req, wid, args, num_args)
    Widget                  cur;
    Widget                  req;
    Widget                  wid;
    ArgList                 args;
    Cardinal                *num_args;
{
    return False;        
}

static int
insertSelection(obj, selected, changed, flush)
    Sj3Object               obj;
    int                     selected;
    int                    *changed;
    int                    *flush;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    int                     ret = 0;

    obj->sj3.selectionending = True;
    switch (obj->sj3.state) {
    case candidate_state:
        obj->sj3.curcand = selected;
        ret = Xsj3cSetCandidate(buf, selected, changed, flush);
        break;
    case symbol_state:
        obj->sj3.cursymbol = selected;
        ret = Xsj3cSetSymbol(buf, selected, changed, flush);
        break;
    case hinsi_state:
        obj->sj3.curhinsi = selected;
        ret = Xsj3cSetHinsi(buf, selected, changed, flush);
        break;
    }
    obj->sj3.selectionending = False;

    return ret;
}

static void
allocCandlist(obj, n)
    Sj3Object               obj;
    int                     n;
{
    ICString *p;

    if (n <= obj->sj3.candlistsize)
        return;

    if (obj->sj3.candlistsize == 0) {
        p = (ICString *)XtMalloc(n * sizeof(ICString));
    } else {
        p = (ICString *)XtRealloc((char *)obj->sj3.candlist,
                  n * sizeof(ICString));
    }

    obj->sj3.candlist = p;
    obj->sj3.candlistsize = n;
}

static void
startRegistration(obj)
    Sj3Object   obj;
{
    ICAuxControlArg         arg;

    arg.command = ICAuxStart;
    XtCallCallbackList((Widget)obj, obj->inputConv.auxcallback,
               (XtPointer)&arg);
}

static void
changeRegistration(obj)
    Sj3Object               obj;
{
    ICAuxControlArg         arg;

    arg.command = ICAuxChange;
    XtCallCallbackList((Widget)obj, obj->inputConv.auxcallback,
               (XtPointer)&arg);
}

static void
endRegistration(obj)
    Sj3Object   obj;
{
    Xsj3cBuf                buf = obj->sj3.sj3buf;
    ICAuxControlArg         arg;

    arg.command = ICAuxEnd;
    XtCallCallbackList((Widget)obj, obj->inputConv.auxcallback,
               (XtPointer)&arg);
    Xsj3cEndDict(buf);
}

/*
 * keeping list of objects
 */
typedef struct _oblist_ {
    Sj3Object               obj;
    struct _oblist_        *next;
} ObjRec;

static ObjRec *ObjList = NULL;

static void
addObject(obj)
    Sj3Object               obj;
{
    ObjRec                 *objp = XtNew(ObjRec);

    objp->obj = obj;
    objp->next = ObjList;
    ObjList = objp;
}

static void
deleteObject(obj)
    Sj3Object obj;
{
    ObjRec                 *objp, *objp0;

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
bell(obj)
    Sj3Object               obj;
{
    XBell(XtDisplayOfObject((Widget)obj), 0);
}
