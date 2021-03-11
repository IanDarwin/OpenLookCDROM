#ifndef lint
static char *rcsid = "$Header: buffer.c,v 2.6 93/09/21 09:06:58 nao Exp $";
#endif
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

#include <X11/keysym.h>
#include <stdio.h>
#include <ctype.h>
#include "common.h"
#include "util.h"
#include "func.h"
#include "segment.h"
#include "table.h"

Xsj3cBuf                    Xsj3cCreateBuffer();
void                        Xsj3cFreeBuffer();
void                        Xsj3cClearBuffer();
void                        Xsj3cFixBuffer();
void                        Xsj3cFlushBuffer();

int                         _Xsj3cSetKeyTable();
extern int                  _Xsj3cSetGuide();

static void                 _Xsj3cFreeModeStr();
static Xsj3cKeyTable       *_Xsj3cAllocKeyTable();
static Xsj3cKeyTable       *_Xsj3cGetFreeKeyTable();
static void                 _Xsj3cAddFreeKeyTable();
static void                 _Xsj3cFreeAllKeyTable();
static int                  _Xsj3cSetKeySym();
static long                 _Xsj3cGetModMask();

static Xsj3cKeyTable       *keynowtp = NULL,   *keymaxtp = NULL;
static Xsj3cKeyTable       *firstkeytp = NULL, *freekeytp = NULL;
static int                  refcount = 0;

static struct _sjrcdeftable {
    char    *key[2];
    char    *value[10];
} def_guide_table[] = {
    "guide",    "hiragana",  /* かな  */ "\202\251\202\310",
    "", "", "", "", "", "", "", "", "",
    "guide",    "zkatakana", /* 全カナ*/ "\221\123\203\112\203\151",
    "", "", "", "", "", "", "", "", "",
    "guide",    "hkatakana", /* 半カナ*/ "\224\274\203\112\203\151",
    "", "", "", "", "", "", "", "", "",
    "guide",    "halpha",/* ABC       */ "\101\102\103",
    "", "", "", "", "", "", "", "", "",
    "guide",    "zalpha",/* ＡＢＣ    */ "\202\140\202\141\202\142",
    "", "", "", "", "", "", "", "", "",
    "guide",    "sjis",  /* Shift-JIS */ "\123\150\151\146\164\55\112\111\123",
    "", "", "", "", "", "", "", "", "",
    "guide",    "euc",   /* ＥＵＣ    */ "\202\144\202\164\202\142",
    "", "", "", "", "", "", "", "", "",
    "guide",    "jis",   /* ＪＩＳ    */ "\202\151\202\150\202\162",
    "", "", "", "", "", "", "", "", "",
    "guide",    "kuten", /* 区点      */ "\213\346\223\137",
    "", "", "", "", "", "", "", "", "",
    "guide",    "toroku",   /* 登録   */ "\223\157\230\136",
    "", "", "", "", "", "", "", "", "",
    "guide",    "syoukyo",  /* 消去   */ "\217\301\213\216",
    "", "", "", "", "", "", "", "", "",
    "guide",    "kanji", /* 漢字      */ "\212\277\216\232",
    "", "", "", "", "", "", "", "", "",
    "guide",    "edit",  /* 編集      */ "\225\322\217\127",
    "", "", "", "", "", "", "", "", "",
    "guide",    "candidate",/* 一覧      */ "\210\352\227\227",
    "", "", "", "", "", "", "", "", "",
    "guide",    "symbol",/* 記号      */ "\213\114\215\206",
    "", "", "", "", "", "", "", "", "",
    "guide",    "hinsi", /* 品詞      */ "\225\151\216\214",
    "", "", "", "", "", "", "", "", "",
    "guide",    "quote", /* 引用      */ "\210\370\227\160",
    "", "", "", "", "", "", "", "", "",
};

static Xsj3cKeyTable defkeys[] = {
    "henkan",   XK_Select,  FlushModeMask,  _Xsj3cConvert,      NONE,NULL,NULL,
    "convert",  XK_Select,  FlushModeMask,  _Xsj3cConvert,      NONE,NULL,NULL,
    "muhen",    XK_Cancel,  ConvedModeMask|ShiftMask,
                                            _Xsj3cUnConvert,    NONE,NULL,NULL,
    "unconvert",XK_Cancel,  ConvedModeMask|ShiftMask,
                                            _Xsj3cUnConvert,    NONE,NULL,NULL,
    "fix",      XK_Execute, FlushModeMask|DictModeMask,    
                                            _Xsj3cFix,          NONE,NULL,NULL,
    "kettei",   XK_KP_Enter,FlushModeMask,  _Xsj3cFix,          NONE,NULL,NULL,
    "flush",    XK_Escape,  FlushModeMask,  _Xsj3cFix,          NONE,NULL,NULL,
    "return",   XK_Return,  AllModeMask,    _Xsj3cReturn,       NONE,NULL,NULL,
    "halpha",   XK_F6,      AllModeMask,    _Xsj3cModeHAlpha,   NONE,NULL,NULL,
    "zalpha",   XK_F7,      AllModeMask,    _Xsj3cModeZAlpha,   NONE,NULL,NULL,
    "hkatakana",XK_F8,      AllModeMask,    _Xsj3cModeHKata,    NONE,NULL,NULL,
    "zkatakana",XK_F9,      AllModeMask,    _Xsj3cModeZKata,    NONE,NULL,NULL,
    "hiragana", XK_F10,     AllModeMask,    _Xsj3cModeHira,     NONE,NULL,NULL,
    "tohalpha", XK_F16,     FlushModeMask|ShiftMask,
                                            _Xsj3cToHAlpha,     NONE,NULL,NULL,
    "tozalpha", XK_F17,     FlushModeMask|ShiftMask,
                                            _Xsj3cToZAlpha,     NONE,NULL,NULL,
    "tohkatakana",XK_F18,   FlushModeMask|ShiftMask,
                                            _Xsj3cToHKata,      NONE,NULL,NULL,
    "tozkatakana",XK_F19,   FlushModeMask|ShiftMask,
                                            _Xsj3cToZKata,      NONE,NULL,NULL,
    "tohiragana",XK_F20,    FlushModeMask|ShiftMask,
                                            _Xsj3cToHira,       NONE,NULL,NULL,
    "zenkaku",  XK_F4,      FlushModeMask,  _Xsj3cZenkaku,      NONE,NULL,NULL,
    "hankaku",  XK_F3,      FlushModeMask,  _Xsj3cHankaku,      NONE,NULL,NULL,
    "toupper",  XK_u,       FlushModeMask|ControlMask,
                                            _Xsj3cToUpper,      NONE,NULL,NULL,
    "tolower",  XK_l,       FlushModeMask|ControlMask,
                                            _Xsj3cToLower,      NONE,NULL,NULL,
    "sjis",     XK_F15,     NoInputModeMask|FlushModeMask|ShiftMask,
                                            _Xsj3cModeSJIS,     NONE,NULL,NULL,
    "euc",      XK_F14,     NoInputModeMask|FlushModeMask|ShiftMask,
                                            _Xsj3cModeEUC,      NONE,NULL,NULL,
    "jis",      XK_F13,     NoInputModeMask|FlushModeMask|ShiftMask,
                                            _Xsj3cModeJIS,      NONE,NULL,NULL,
    "kuten",    XK_F12,     NoInputModeMask|FlushModeMask|ShiftMask,
                                            _Xsj3cModeKuten,    NONE,NULL,NULL,
    "code",     XK_F5,      NoInputModeMask|FlushModeMask,
                                            _Xsj3cCodeRollDown, NONE,NULL,NULL,
    "toggle",   XK_Tab,     AllModeMask,    _Xsj3cModeRollDown, NONE,NULL,NULL,
    "modedown", XK_Tab,     AllModeMask,    _Xsj3cModeRollDown, NONE,NULL,NULL,
    "toggleback",XK_Tab,    AllModeMask|ShiftMask,
                                            _Xsj3cModeRollUp,   NONE,NULL,NULL,
    "modeup",   XK_Tab,     AllModeMask|ShiftMask,
                                            _Xsj3cModeRollUp,   NONE,NULL,NULL,
    "nextmode", XK_Tab,     FlushModeMask|ControlMask,
                                            _Xsj3cNextMode,     NONE,NULL,NULL,
    "prevmode", XK_Tab,     FlushModeMask|ControlMask|ShiftMask,
                                            _Xsj3cPrevMode,     NONE,NULL,NULL,
    "muhenkan", XK_Cancel,  NoInputModeMask|FlushModeMask,
                                            _Xsj3cModeToggle,   NONE,NULL,NULL,
    "right",    XK_Right,   AllModeMask,    _Xsj3cForward,      NONE,NULL,NULL,
    "forward",  XK_Right,   AllModeMask,    _Xsj3cForward,      NONE,NULL,NULL,
    "left",     XK_Left,    AllModeMask,    _Xsj3cBackward,     NONE,NULL,NULL,
    "backward", XK_Left,    AllModeMask,    _Xsj3cBackward,     NONE,NULL,NULL,
    "top",      XK_a,       FlushModeMask|DictModeMask|ControlMask,
                                            _Xsj3cTop,          NONE,NULL,NULL,
    "end",      XK_e,       FlushModeMask|DictModeMask|ControlMask,
                                            _Xsj3cEnd,          NONE,NULL,NULL,
    "bottom",   XK_e,       FlushModeMask|DictModeMask|ControlMask,
                                            _Xsj3cEnd,          NONE,NULL,NULL,
    "up",       XK_Up,      SelectModeMask, _Xsj3cUp,           NONE,NULL,NULL,
    "down",     XK_Down,    SelectModeMask, _Xsj3cDown,         NONE,NULL,NULL,
    "first",    XK_Up,      OutputModeMask|ControlMask,
                                            _Xsj3cFirst,        NONE,NULL,NULL,
    "last",     XK_Down,    OutputModeMask|ControlMask,
                                            _Xsj3cLast,         NONE,NULL,NULL,
    "nextp",    XK_Select,  SelectModeMask, _Xsj3cNextPage,     NONE,NULL,NULL,
    "prevp",    XK_Cancel,  SelectModeMask, _Xsj3cPrevPage,     NONE,NULL,NULL,
    "wrap",     XK_Down,    ConvedModeMask|ShiftMask,
                                            _Xsj3cNext,         NONE,NULL,NULL,
    "next",     XK_Down,    ConvedModeMask|ShiftMask,
                                            _Xsj3cNext,         NONE,NULL,NULL,
    "wrapback", XK_Up,      ConvedModeMask|ShiftMask,
                                            _Xsj3cPrev,         NONE,NULL,NULL,
    "prev",     XK_Up,      ConvedModeMask|ShiftMask,
                                            _Xsj3cPrev,         NONE,NULL,NULL,
    "select",   XK_Execute, SelectModeMask, _Xsj3cSelect,       NONE,NULL,NULL,
    "cancel",   XK_Escape,  SelectModeMask|DictModeMask,
                                            _Xsj3cCancel,       NONE,NULL,NULL,
    "cancel",   XK_Cancel,  FlushModeMask|ShiftMask,
                                            _Xsj3cCancel,       NONE,NULL,NULL,
    "kakucyou", XK_Up,      FlushModeMask|DictModeMask,
                                            _Xsj3cExpand,       NONE,NULL,NULL,
    "expand",   XK_Up,      FlushModeMask|DictModeMask,
                                            _Xsj3cExpand,       NONE,NULL,NULL,
    "syukusyou",XK_Down,    FlushModeMask|DictModeMask,
                                            _Xsj3cShrink,       NONE,NULL,NULL,
    "shrink",   XK_Down,    FlushModeMask|DictModeMask,
                                            _Xsj3cShrink,       NONE,NULL,NULL,
    "backspace",XK_BackSpace,FlushModeMask|SelectModeMask|DictModeMask,
                                            _Xsj3cBackSpace,    NONE,NULL,NULL,
    "delete",   XK_Delete,  FlushModeMask|SelectModeMask|DictModeMask,
                                            _Xsj3cDelete,       NONE,NULL,NULL,
    "delafter", XK_k,       FlushModeMask|ControlMask,
                                            _Xsj3cDelAfter,     NONE,NULL,NULL,
    "start",    XK_Kanji,   AllModeMask|ShiftMask,
                                            _Xsj3cStart,        NONE,NULL,NULL,
    "reconnect",XK_Kanji,   AllModeMask|ShiftMask|Mod1Mask, 
                                            _Xsj3cReConnect,    NONE,NULL,NULL,
    "saihenkan",XK_Select,  NoInputModeMask|ControlMask,
                                            _Xsj3cReConvert,    NONE,NULL,NULL,
    "recovert", XK_Select,  NoInputModeMask|ControlMask,
                                            _Xsj3cReConvert,    NONE,NULL,NULL,
    "edit",     XK_Select,  ConvedModeMask|Mod1Mask,
                                            _Xsj3cEdit,         NONE,NULL,NULL,
    "toroku",   XK_F1,      ConvedModeMask, _Xsj3cDRegBegin,    NONE,NULL,NULL,
    "register", XK_F1,      ConvedModeMask, _Xsj3cDRegBegin,    NONE,NULL,NULL,
    "syoukyo",  XK_F2,      ConvedModeMask, _Xsj3cDClearBegin,  NONE,NULL,NULL,
    "eliminate",XK_F2,      ConvedModeMask, _Xsj3cDClearBegin,  NONE,NULL,NULL,
    "symbol",   XK_Escape,  NoInputModeMask|FlushModeMask|ShiftMask, 
                                            _Xsj3cSymbolBegin,  NONE,NULL,NULL,
    "quote",    XK_q,       NoInputModeMask|FlushModeMask|ControlMask,
                                            _Xsj3cQuote,        NONE,NULL,NULL,
    "flushbefore",NoSymbol, NULL,           _Xsj3cFlushBefore,  NONE,NULL,NULL,
    "bell",     NoSymbol,   NULL,           _Xsj3cBell,         NONE,NULL,NULL,
    "sjrc",     NoSymbol,   NULL,           _Xsj3cSjrc,         NONE,NULL,NULL,
    "kill",     NoSymbol,   NULL,           _Xsj3cKill,         NONE,NULL,NULL,
    "kana",     NoSymbol,   NULL,           _Xsj3cKana,         NONE,NULL,NULL,
    "null",     NoSymbol,   NULL,           _Xsj3cNull,         NONE,NULL,NULL,
    "ignore",   NoSymbol,   NULL,           _Xsj3cIgnore,       NONE,NULL,NULL,
    NULL,       NULL,       NULL,           NULL,               NONE,NULL,NULL
};

#define KEYTBMAX    (BUFSIZ/sizeof(Xsj3cKeyTable) - 1)

/*
 * _Xsj3cAllocKeyTable()
 *  Allocate memmory for key-function conversion table.
 */
static Xsj3cKeyTable *
_Xsj3cAllocKeyTable()
{
    register Xsj3cKeyTable  *keytp;

    if (keynowtp == NULL || keynowtp > keymaxtp) {
        keytp = (Xsj3cKeyTable *)malloc(BUFSIZ);
        if (keytp == NULL)
            return (NULL);
        else
            bzero((char *)keytp, BUFSIZ);
        if (!firstkeytp)
            firstkeytp = keytp;
        keynowtp = keytp;
        keymaxtp = keynowtp + KEYTBMAX;
        keynowtp++;
    } else {
        keytp = keynowtp;
        keynowtp++;
    }
    keytp->keyword = NULL;
    keytp->ksym = NoSymbol;
    keytp->func = NULL;
    keytp->inputsame = 0;
    keytp->prev = NULL;
    keytp->next = NULL;
    return (keytp);
}

/*
 * _Xsj3cAddFreeKeyTable()
 *  Add key-function conversion table to free list.
 */
static void
_Xsj3cAddFreeKeyTable(keytp)
    register Xsj3cKeyTable  *keytp;
{
    if (!keytp)
        return;
    if (freekeytp)
        keytp->prev = freekeytp;
    else
        keytp->prev = NULL;
    freekeytp = keytp;
    keytp->keyword = NULL;
    keytp->ksym = NoSymbol;
    keytp->func = NULL;
    keytp->inputsame = 0;
    keytp->next = NULL;
}

/*
 * _Xsj3cGetFreeKeyTable()
 *  Get key-function conversion table from free list.
 */
static Xsj3cKeyTable *
_Xsj3cGetFreeKeyTable()
{
    register Xsj3cKeyTable  *keytp;

    if (keytp = freekeytp)
        freekeytp = keytp->prev;
    return (keytp);
}

static void
_Xsj3cFreeAllKeyTable()
{
    register Xsj3cKeyTable  *keytp, *keytq;

    keytq = firstkeytp;
    while (keytq) {
        keytp = keytq;
        keytq = keytp + KEYTBMAX;
        if (keytq);
            keytq = keytq->next;
        free(keytp);
    }
    firstkeytp = NULL;
    freekeytp = NULL;
}

/*
 * _Xsj3cSetKeyTable()
 *  Read sjrc's .key.[function] entry and set key-function conversion table.
 */
int
_Xsj3cSetKeyTable(buf, table)
    Xsj3cBuf                    buf;
    Sjrctable                   table;
{
    register Xsj3cKeyTable     *keytp, *keytq, *keytr;
    Xsj3cKeyTable              *keytf,  dummy;
    KeySym                      ksym;
    Xsj3ccMode                  modmask;
    Xsj3cFlag                   inputsame;

    if (!_Xsj3cSetKeySym(table->value, &ksym, &modmask, &inputsame)) 
        return 0;
    keytp = &dummy;
    keytp->next= buf->key;
    while (keytp->next != NULL) {
        keytp = keytp->next;
        if (keytp->ksym == NoSymbol ||(keytp->ksym == ksym &&
            (keytp->modmask & ~AllModeMask) == (modmask & ~AllModeMask)
            && ((keytp->modmask & AllModeMask) & modmask))) {
            keytq = keytp->next;
            keytf = keytp;
            if (keytp == buf->key) {    /* top of list  */
                buf->key = keytq;
                keytq->prev = NULL;
            } else if (!keytq) {        /* last of list */
                keytp = keytp->prev;
                keytp->next = NULL;
            } else {
                keytp = keytp->prev;
                keytp->next = keytq;
                keytq->prev = keytp;
            }
            _Xsj3cAddFreeKeyTable(keytf);
        } 
    }
    if (!table->key[1])
        return 0;
    else if (ksym == NoSymbol || _Xsj3cCmp(table->key[1], "ignore"))
        return 1;

    keytr = keytp;
    keytq = defkeys;
    while (keytq->keyword != NULL) {
        if (_Xsj3cCmp(table->key[1], keytq->keyword)) {
            if (!(keytp = _Xsj3cGetFreeKeyTable())) {
                if (!(keytp = _Xsj3cAllocKeyTable())) {
                    Xsj3cWarning("can't allocate keysym to func table");
                    return 0;
                }
            }
            keytr->next = keytp;
            keytp->prev = keytr;
            keytp->keyword = keytq->keyword;
            keytp->ksym = ksym;
            keytp->modmask = modmask;
            keytp->func = keytq->func;
            keytp->inputsame = inputsame;
            keytr = keytp;
            keytp->next = NULL;
            if (keytp->keyword == NULL) {
                Xsj3cWarning("keyword %s is not supported",table->key[1]);
                return 0;
            }
        } 
        keytq++;
    }
    return 1;
}

/*
 * _Xsj3cSetKeySym()
 *  Read values of the .key.[function] entry and set keysym
 * and modifier/conversion-mode mask.
 */
static int
_Xsj3cSetKeySym(string, ksym, modmask, inputsame)
    char          **string;
    KeySym         *ksym;
    Xsj3ccMode     *modmask;
    Xsj3cFlag      *inputsame;
{
    register char  *p;
    int             sub,    ret;

    *modmask = 0;
    *ksym = NoSymbol;
    p = *string;

    /* ModMask and KeySym field   */
    if (p) {
        if(*p == '^') { /* sj2/sj3 control keybind compatibility   */
            if (++p) {
                if (isupper(*p)) {
                    sub = *p - 'A';
                    *ksym = XK_a + sub;
                } else if (islower(*p)) {
                    sub = *p - 'a';
                    *ksym = XK_a + sub;
                } else if (*p >= '\\' && *p <= '_') {
                    sub = *p - '\\';
                    *ksym = XK_backslash + sub;
                } else if (*p >= '!' && *p <= '>') {
                    sub = *p - '!';
                    *ksym = XK_exclam + sub;
                } else {
                    Xsj3cWarning("wrong keybind in sjrc file");
                }
                *modmask = ControlMask|AllModeMask;
            }
        } else if(*p == 'X') { /* sjx keybind compatibility   */
            if (!strncmp(p, "XK_", 3)) {
                p += 3;
                *modmask = AllModeMask;
                *ksym = XStringToKeysym(p);
            } else {
                *modmask = AllModeMask;
                Xsj3cWarning("wrong keybind in sjrc file");
            }
        } else if(*p == '\033') { /* escape keybind compatibility   */
            *ksym = XK_Escape;
            *modmask = AllModeMask;
        } else if(*p == '\177') { /* delete keybind compatibility   */
            *ksym = XK_Delete;
            *modmask = AllModeMask;
        } else if ((ret = _Xsj3cGetModMask(*string)) >= 0) {
            *modmask = ret;
            if (++string)
                *ksym = XStringToKeysym(*string);
            else
                Xsj3cWarning("wrong keybind in sjrc file");
        } else {
            Xsj3cWarning("wrong keybind in sjrc file");
        }
    } else {
        *modmask = AllModeMask;
    }

    /* Through function field   */
    p = *(++string);
    if (p) {
        if (_Xsj3cCmp(p, "off"))
            *inputsame = OFF;
        else if (_Xsj3cCmp(p, "on"))
            *inputsame = ON;
        else
            *inputsame = NONE;
    } else {
        *inputsame = NONE;
    }
    return 1;
}

/*
 * _Xsj3cGetModMask()
 *  Read strings and convert to modifier/conversion-mode mask.
 */
static long
_Xsj3cGetModMask(p)
    register char  *p;
{
    char            mode[256];
    register char  *q;
    int             ret = AllModeMask,  mask = AllModeMask;
    Xsj3ccMode      conversion = 0;

    while (*p != '\0') {
        q = mode;
        while (*p != '\0' && *p != '|') {
            *q++ = *p++;
        }
        if (*p != '\0')
            p++;
        *q = '\0';
        q = mode; 
        if (*q == 'n') {
            continue;
        } else if (*q == 's') {
            ret |= ShiftMask;
            continue;
        } else if (*q == 'c') {
            ret |= ControlMask;
            continue;
        } else if (*q == 'm') {
            if (*(++q) != '\0') {
                while (*q != '\0')
                    q++;
                switch(*(--q)) {
                case '1':
                    ret |= Mod1Mask;
                    break;
                case '2':
                    ret |= Mod2Mask;
                    break;
                case '3':
                    ret |= Mod3Mask;
                    break;
                case '4':
                    ret |= Mod4Mask;
                    break;
                case '5':
                    ret |= Mod5Mask;
                    break;
                default:
                    ret |= Mod1Mask;
                    break;
                }
            } else 
                ret |= Mod1Mask;
            continue;
        } else if (*q == 'l') {
            ret |= LockMask;
            continue;
        } else if (*q == 'k') {
            ret |= KanaMask;
            continue;
        } else if (*q == 'N') {
            conversion |= NoInputModeMask;
            continue;
        } else if (*q == 'I') { 
            conversion |= InputModeMask;
            continue;
        } else if (*q == 'i') { /* for compatibility    */
            conversion |= InputModeMask;
            continue;
        } else if (*q == 'u') { /* for compatibility    */
            conversion |= ConvedModeMask;
            continue;
        } else if (*q == 'U') { /* for compatibility    */
            conversion |= ConvedModeMask;
            continue;
        } else if (*q == 'C') {
            conversion |= ConvedModeMask;
            continue;
        } else if (*q == 'v') { /* for compatibility    */
            conversion |= SelectModeMask;
            continue;
        } else if (*q == 'V') { /* for compatibility    */
            conversion |= SelectModeMask;
            continue;
        } else if (*q == 'S') {
            conversion |= SelectModeMask;
            continue;
        } else if (*q == 'o') { /* for compatibility    */
            conversion |= OutputModeMask;
            continue;
        } else if (*q == 'O') {
            conversion |= OutputModeMask;
            continue;
        } else if (*q == 'd') { /* for compatibility    */
            conversion |= DictModeMask;
            continue;
        } else if (*q == 'D') {
            conversion |= DictModeMask;
            continue;
        } else if (*q == 'f') { /* for compatibility    */
            conversion |= FlushModeMask;
            continue;
        } else if (*q == 'F') {
            conversion |= FlushModeMask;
            continue;
        } else if (*q == 'a') { /* for compatibility    */
            conversion |= AllModeMask;
            continue;
        } else if (*q == 'A') {
            conversion |= AllModeMask;
            continue;
        } else {
            Xsj3cWarning("Ilegal keybind modmask %s in sjrc file",mode);
            ret = -1;
        }
    }
    if (conversion) {
        ret &= ~mask;
        ret |= conversion;
    }
    return ret;
}

/*
 * Xsj3cCreateBuffer()
 * Allocate Xsj3cBuf type structure and initialize all flags and buffers.
 */
Xsj3cBuf
Xsj3cCreateBuffer()
{
    Xsj3cBuf            buf;
    Xsj3cKeyTable      *keytp, *keytq, *keytr;
    register int        i,      back_lang;

    /*  Allocate buffer fields  */

    if ((buf = (Xsj3cBuf)malloc(sizeof(Xsj3cBufRec))) == NULL) {
        return (Xsj3cBuf)NULL;
    }
    bzero(buf, sizeof(Xsj3cBufRec));

    /* Default definition for uninitialized field */

    buf->convmode = NoInputModeMask;
    buf->server = SERVER_SJ3;
    if ((buf->input = (Xsj3cSeg *)calloc(BUNBUFSIZ, sizeof(Xsj3cSeg)))
                == NULL) {
        return (Xsj3cBuf)NULL;
    }
    bzero(buf->input, BUNBUFSIZ * sizeof(Xsj3cSeg));
    buf->backup = NULL;
    buf->current = NULL;
    buf->segnum = 0;
    buf->backsegnum = 0;
    buf->curseg = 0;
    buf->convedsegnum = 0;

    buf->inputmode = MODE_HIRA;
    buf->dispmode = MODE_HIRA;
    
    buf->dict = NULL;

    buf->candidate = NULL;
    buf->hinsi = NULL;
    buf->symbol = NULL;

    buf->candnum = 0;
    buf->curcand = 0;
    buf->cursymbol = 0;
    buf->curhinsi = 0;
    buf->selectstatus = SELECT_CAND;
    buf->n_select = 0;
    buf->candseg = 0;

    buf->rktable = NULL;
    buf->hktable = NULL;
    buf->zhtable = NULL;
    buf->plosive = NULL;
    buf->rkdouble = NULL;

    /* Initialize common (with sj2/sj3/sjx) flags area */
    /* Make keysym to function hash table  */
    for (keytp = defkeys, i = 0; keytp->keyword != NULL; keytp++) {
        if (!(keytq = _Xsj3cGetFreeKeyTable())) {
            if (!(keytq = _Xsj3cAllocKeyTable())) {
                Xsj3cWarning("can't allocate keysym to func table");
                return (Xsj3cBuf)NULL;
            }
        }
        if (!i++) {
            buf->key = keytq;
            keytq->prev = NULL;
        } else {
            keytr->next = keytq;
            keytq->prev = keytr;
        }
        keytq->keyword = keytp->keyword;
        keytq->ksym = keytp->ksym;
        keytq->modmask = keytp->modmask;
        keytq->func = keytp->func;
        keytq->inputsame = keytp->inputsame;
        keytr = keytq;
    }
    keytq->next = NULL;

    for (i = 0; i < MODE_INROT_NUM; i++) 
        buf->inmoderot[i]= i;
    buf->inmoderotnum = MODE_INROT_NUM;
    for (i = 0; i < MODE_OUTROT_NUM; i++) 
        buf->outmoderot[i]= i;
    buf->outmoderotnum = MODE_OUTROT_NUM;
    for (i = 0; i < MODE_CODE_NUM; i++) 
        buf->defcode[i]= MODE_SJIS + i;
    buf->coderotnum = MODE_CODE_NUM;
    buf->muhenmode = MODE_HALPHA;
    buf->togglemode = MODE_HIRA;
    buf->dotoggle = ON;
    buf->throughflg = 0;

    buf->rcfile = NULL;
    back_lang = in_lang;
    in_lang = JP_SJIS;
    for (i = 0; i < MODE_STR_NUM; i++) {
        _Xsj3cSetGuide(buf, &def_guide_table[i]);
    }
    in_lang = back_lang;

    buf->gakusyuu = ON;
    buf->rkbell = OFF;
    buf->flushaconv = OFF;
    buf->sj3serv = NULL;
    buf->setnormal = NULL;
    buf->throughnext = NULL;

    /* Initialize Xsj3clib original flags area   */
    buf->sj3serv2 = NULL;
    buf->alphaconv = OFF;
    buf->backdisplay = OFF;
    buf->beginlastseg = ON;
    buf->expandkconv = SEG_CONVED;
    buf->shrinkkconv = SEG_CONVED;
    buf->expandmconv = SEG_NOCONV;
    buf->shrinkmconv = SEG_NOCONV;
    buf->shrinkall = OFF;
    buf->flushiconv = ON;
    buf->flushsconv = NONE;
    buf->flusheconv = ON;
    buf->henkanseg = ALL;
    buf->muhenseg = ONE;
    buf->delchange = ONE;
    buf->flushchange = ONE;
    for ( i = 0; i < MODE_CONV_NUM; i++) 
        buf->modeconv[i] = SEG_CONVED;
    buf->moveloop = OFF;
    buf->movebyseg = SEG_CONVED;
    buf->jumpbyseg = SEG_CONVED;
    buf->delbyseg = SEG_CONVED;
    buf->killbyseg = SEG_CONVED;
    buf->muhencurlast = OFF;
    buf->editcurlast = OFF;
    buf->flushcurlast = OFF;
    buf->convedunderline = ON;
    buf->dispmodechange = OFF;
    buf->dellastmove = ON;
    buf->kanaonly = OFF;
    buf->inputsame = ON;
    buf->cntrlsame = OFF;
    buf->selectconv = ON;
    buf->beforeconv = OFF;
    buf->lastdoubleconv = OFF;
    buf->selectcount = 0;
    buf->selectback = OFF;
    buf->candpadding = OFF;

    refcount++;
    return (buf);
}

/*
 * _Xsj3cFreeModeStr()
 *  Free memory of mode guide strings.
 */
static void
_Xsj3cFreeModeStr(buf)
    Xsj3cBuf                buf;
{
    register int            i;

    for (i = 0; i < MODE_STR_NUM; i++) {
        if (buf->modestr[i])
            free(buf->modestr[i]);
        buf->modestr[i] = NULL;
    }
}

/*
 * Xsj3cFreeBuffer()
 *  Free all data buffers.
 */
void
Xsj3cFreeBuffer(buf)
    Xsj3cBuf            buf;
{
    register int        i;

    _Xsj3cFreeModeStr(buf);
    if (!refcount)
        _Xsj3cFreeAllKeyTable();
    if (buf->input) {
        for (i = 0; i < buf->segnum + 1; i++) {
            Xsj3cFreeSegment(buf->input[i]);
            buf->input[i] = NULL;
        }
        free(buf->input);
        buf->input = NULL;
    }
    if (buf->backup) {
        for (i = 0; i < buf->backsegnum + 1; i++) {
            Xsj3cFreeSegment(buf->backup[i]);
            buf->input[i] = NULL;
        }
        free(buf->backup);
        buf->backup = NULL;
    }
    free(buf);
}

/*
 * Xsj3cClearBuffer()
 *  Clear buffers.
 */
void
Xsj3cClearBuffer(buf)
    Xsj3cBuf            buf;
{
    register int        i;

    for (i = 0; i < buf->segnum + 1; i++) {
        Xsj3cFreeSegment(buf->input[i]);
        buf->input[i] = NULL;
    }
    buf->convmode = InputModeMask;
    buf->curseg = 0;
    buf->segnum = 0;
    buf->convedsegnum = 0;
    buf->dispmode = buf->inputmode;
}

/*
 * Xsj3cFixBuffer()
 *  Fix and backup buffers.
 */
void
Xsj3cFixBuffer(buf)
    Xsj3cBuf            buf;
{
    register int        i;
    int                 j;

    if (buf->backup) {
        for (i = 0; i < buf->backsegnum + 1; i++) {
            Xsj3cFreeSegment(buf->backup[i]);
            buf->backup[i] = NULL;
        }
        free(buf->backup);
        buf->backup = NULL;
    }
    if (buf->convmode & InputModeMask) {
        j = buf->input[buf->curseg]->cur - 1;
        if (j >= 0 && buf->input[buf->curseg]->yomi[j] < 256 &&
                iscntrl((unsigned char)buf->input[buf->curseg]->yomi[j])) {
            unsigned char   tmp[RBUFSIZ];

            _Xsj3cExtractChar(buf, buf->input[buf->curseg], tmp, 1);
            _Xsj3cStoreYomi(buf, buf->input[buf->curseg], j);
            if (!buf->input[buf->curseg]->num) {
                buf->segnum--;
                Xsj3cFreeSegment(buf->input[buf->curseg]);
                buf->input[buf->curseg] = NULL;
                for (i = buf->curseg; i < buf->segnum; i++) {
                    buf->input[i] = buf->input[i + 1];
                }
                buf->input[buf->segnum] = NULL;
            }
        }
        buf->convmode = NoInputModeMask;
    } else if (buf->convmode & ~NoInputModeMask) {
        j = buf->input[buf->curseg]->dnum - 1;
        if (j >= 0 && buf->input[buf->curseg]->disp[j] < 256 &&
            iscntrl((unsigned char)buf->input[buf->curseg]->disp[j])) {
            buf->input[buf->curseg]->dnum--;
            buf->input[buf->curseg]->disp[j] = '\0';
        } 
        if (buf->convmode != SelectModeMask) /* For symbol input    */
            buf->convmode = NoInputModeMask;
    }
    buf->backup = buf->input;
    buf->backsegnum = buf->segnum;
    buf->input = NULL;
    if ((buf->input = (Xsj3cSeg *)calloc(BUNBUFSIZ,
            sizeof(Xsj3cSeg))) == NULL) {
        Xsj3cError("Cannot allocate for input buffers");
    }
    buf->curseg = 0;
    buf->segnum = 0;
    buf->convedsegnum = 0;
    buf->dispmode = buf->inputmode;
}

/*
 * Xsj3cFlushBuffer()
 *  Flush and backup buffers.
 */
void
Xsj3cFlushBuffer(buf)
    Xsj3cBuf            buf;
{
    register int        tmpsegnum;
    register Xsj3cSeg  *segp;

    segp = buf->backup;
    buf->backup = buf->input;
    buf->input = segp;
    tmpsegnum = buf->backsegnum;
    buf->backsegnum = buf->segnum;
    buf->segnum = tmpsegnum;
    buf->curseg = 0;
    buf->convmode = InputModeMask;
    buf->convedsegnum = 0;
    buf->dispmode = buf->inputmode;
}
