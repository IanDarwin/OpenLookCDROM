#ifndef lint
static char *rcsid = "$Header: sjrc.c,v 2.3 93/01/06 10:56:56 nao Exp $";
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

#include <stdio.h>
#include <ctype.h>
#include "common.h"
#include "sj3ctype.h"
#include "util.h"
#include "table.h"

void                        Xsj3cRCInit();

static int                  _Xsj3cReadRC();

extern int                  _Xsj3cSetKeyTable();
static int                  _Xsj3cSetInputMode();
static int                  _Xsj3cSetPrintMode();
static int                  _Xsj3cSetDefCode();
static int                  _Xsj3cSetMuhenkan();
static int                  _Xsj3cSetMuEdit();
static int                  _Xsj3cSetMToggle();
int                         _Xsj3cSetGuide();
static int                  _Xsj3cSetBStudy();
static int                  _Xsj3cSetRkeBell();
static int                  _Xsj3cSetFlushAfterConv();
static int                  _Xsj3cSetSj3Serv();
static int                  _Xsj3cSetNormal();
static int                  _Xsj3cSetThroughNext();

static int                  _Xsj3cSetSj3Serv2();
static int                  _Xsj3cSetFlushInConv();
static int                  _Xsj3cSetFlushSelectConv();
static int                  _Xsj3cSetFlushEndConv();
static int                  _Xsj3cSetAlphaConv();
static int                  _Xsj3cSetBackDisplay();
static int                  _Xsj3cSetSegLast();
static int                  _Xsj3cSetExpandMConv();
static int                  _Xsj3cSetShrinkMConv();
static int                  _Xsj3cSetExpandKConv();
static int                  _Xsj3cSetShrinkKConv();
static int                  _Xsj3cSetShrinkAll();
static int                  _Xsj3cSetConvSeg();
static int                  _Xsj3cSetUnconvSeg();
static int                  _Xsj3cSetDelChange();
static int                  _Xsj3cSetFlushChange();
static int                  _Xsj3cSetModeConv();
static int                  _Xsj3cSetMoveLoop();
static int                  _Xsj3cSetModeBySeg();
static int                  _Xsj3cSetJumpBySeg();
static int                  _Xsj3cSetDelBySeg();
static int                  _Xsj3cSetKillBySeg();
static int                  _Xsj3cSetMuhenCursorLast();
static int                  _Xsj3cSetEditCursorLast();
static int                  _Xsj3cSetFlushCursorLast();
static int                  _Xsj3cSetUnderLine();
static int                  _Xsj3cSetDispMChange();
static int                  _Xsj3cSetDelLastMove();
static int                  _Xsj3cSetKanaInput();
static int                  _Xsj3cSetInputSame();
static int                  _Xsj3cSetCntrlSame();
static int                  _Xsj3cSetSelectConv();
static int                  _Xsj3cSetBeforeConv();
static int                  _Xsj3cSetLastDoubleConv();
static int                  _Xsj3cSetSelectCount();
static int                  _Xsj3cSetSelectBackMove();
static int                  _Xsj3cSetCandidatePad();
static int                  _Xsj3cSetNextRCFile();
static int                  _Xsj3cSetIncludeRCFile();

static struct functable {
    char   *keyword;
    int     (*func)();
} funcs[] = {
    "key",                          _Xsj3cSetKeyTable,
    "escape",                       _Xsj3cSetKeyTable,
    "sjxkey",                       _Xsj3cSetKeyTable,
    "ki2key",                       _Xsj3cSetKeyTable,
    "initialmode",                  _Xsj3cSetInputMode,
    "defaultcode",                  _Xsj3cSetDefCode,
    "muhenkanmode",                 _Xsj3cSetMuhenkan,
    "muhenkaninedit",               _Xsj3cSetMuEdit,
    "muhenkantoggle",               _Xsj3cSetMToggle,
    "guide",                        _Xsj3cSetGuide,
    "bstudy",                       _Xsj3cSetBStudy,
    "rkerrbell",                    _Xsj3cSetRkeBell,
    "flushafterconversion",         _Xsj3cSetFlushAfterConv,
    "server",                       _Xsj3cSetSj3Serv,
    "sj3serv",                      _Xsj3cSetSj3Serv,
    "setnormal",                    _Xsj3cSetNormal,
    "throughnext",                  _Xsj3cSetThroughNext,
    "sj3serv2",                     _Xsj3cSetSj3Serv2,
    "flushinconversion",            _Xsj3cSetFlushInConv,
    "flushselectconversion",        _Xsj3cSetFlushSelectConv,
    "flushendconversion",           _Xsj3cSetFlushEndConv,
    "alphabetconversion",           _Xsj3cSetAlphaConv,
    "backdisplay",                  _Xsj3cSetBackDisplay,
    "beginconversionlast",          _Xsj3cSetSegLast,
    "expandmodeconversion",         _Xsj3cSetExpandMConv,
    "shrinkmodeconversion",         _Xsj3cSetShrinkMConv,
    "expandkanjiconversion",        _Xsj3cSetExpandKConv,
    "shrinkkanjiconversion",        _Xsj3cSetShrinkKConv,
    "shrinkall",                    _Xsj3cSetShrinkAll,
    "henkansegment",                _Xsj3cSetConvSeg,
    "muhenkansegment",              _Xsj3cSetUnconvSeg,
    "deletechangesegment",          _Xsj3cSetDelChange,
    "flushchangesegment",           _Xsj3cSetFlushChange,
    "modeconversion",               _Xsj3cSetModeConv,
    "moveloop",                     _Xsj3cSetMoveLoop,
    "movebysegment",                _Xsj3cSetModeBySeg,
    "jumpbysegment",                _Xsj3cSetJumpBySeg,
    "deletebysegment",              _Xsj3cSetDelBySeg,
    "killbysegment",                _Xsj3cSetKillBySeg,
    "muhenkancursorlast",           _Xsj3cSetMuhenCursorLast,
    "editkancursorlast",            _Xsj3cSetEditCursorLast,
    "flushkancursorlast",           _Xsj3cSetFlushCursorLast,
    "convertedunderline",           _Xsj3cSetUnderLine,
    "displaymodechange",            _Xsj3cSetDispMChange,
    "deletelastmove",               _Xsj3cSetDelLastMove,
    "kanainputonly",                _Xsj3cSetKanaInput,
    "inputsametime",                _Xsj3cSetInputSame,
    "controlsametime",              _Xsj3cSetCntrlSame,
    "printmode",                    _Xsj3cSetPrintMode,
    "beforeselectconversion",       _Xsj3cSetSelectConv,
    "beforeconversion",             _Xsj3cSetBeforeConv,
    "lastdoubleconversion",         _Xsj3cSetLastDoubleConv,
    "beforeselectcount",            _Xsj3cSetSelectCount,
    "selectbackspacemove",          _Xsj3cSetSelectBackMove,
    "candidatepadding",             _Xsj3cSetCandidatePad,
    "nextrcfile",                   _Xsj3cSetNextRCFile,
    "include",                      _Xsj3cSetIncludeRCFile,
    NULL,                           NULL
};

static int          inc_cnt;

#define END         -1
#define COMMENT     0
#define NORMAL      1
#define BAD         2
#define OTHERS      3
#define SERIAL      "nwp511"
#define SERIAL2     "nwp517"
#define SERIAL3     "nwp-511"

/*
 * Xsj3cRCInit()
 *  Decide sjrc file to read, then read it and set resources.
 */
void
Xsj3cRCInit(buf, sjrc, home)
    Xsj3cBuf            buf;
    char               *sjrc;
    char               *home;
{
    extern char        *getenv();
    register char      *p;
    SjrcRec             rctable;
    struct functable   *functp;
    FILE               *fp;
    register int        line;
    int                 status;

    if (!buf->rcfile) {
        if ((buf->rcfile = (char *)malloc(BUFSIZ)) == NULL)
            Xsj3cError("Can't allocate sjrc file");
        *buf->rcfile = '\0';

        /*  Get sjrc file and open */
        if (sjrc) {
            if ((fp = fopen(sjrc, "r")) == NULL)
                Xsj3cError("can't open sjrc file %s", sjrc);
            strcpy(buf->rcfile, sjrc);
        } else {
            if ((p = getenv("SJRC")) && *p != '\0') {
                if (*p != '/') {
                    if (home)
                        strcpy(buf->rcfile, home);
                    strcat(buf->rcfile, "/");
                }
                strcat(buf->rcfile, p);
            } else if (home) {
                strcpy(buf->rcfile, home);
                strcat(buf->rcfile, "/.sjrc");
            } else {
                strcpy(buf->rcfile, SJ3DEFPATH);
                strcat(buf->rcfile, DEF_SJRC_FILE);
            }
            if ((fp = fopen(buf->rcfile, "r")) == NULL) {
                strcpy(buf->rcfile, SJ3DEFPATH);
                strcat(buf->rcfile, DEF_SJRC_FILE);
                if ((fp = fopen(buf->rcfile, "r")) == NULL) {
                    Xsj3cError("can't open sjrc file %s", buf->rcfile);
                }
            }
        }
    } else {
        if ((fp = fopen(buf->rcfile, "r")) == NULL) {
            Xsj3cWarning("can't open sjrc file %s", buf->rcfile);
            return;
        }
    }

    /*  Read sjrc file and set buffer fields    */
    inc_cnt = 0;
    for (line = 0; (status = 
            _Xsj3cReadRC(fp, &rctable)) != END; line++)  {
        functp = funcs;
        if (status == NORMAL) {
            while (functp->keyword != NULL) {
                if (_Xsj3cCmp(rctable.key[0], functp->keyword)) {
                    if (!(*(functp->func))(buf, &rctable))
                        Xsj3cWarning("wrong format in sjrc file. %s:%d",
                                buf->rcfile, line + 1);
                    break;
                }
                functp++;
            }
        } else if (status == COMMENT || status == OTHERS) {
            continue;
        } else {
            Xsj3cWarning("bad line in sjrc file. %s:%d", buf->rcfile, line);
            continue;
        }
    }
    fclose(fp);
}

/*
 * _Xsj3cReadRC()
 *  Read sjrc file. (Non memory copy version)
 */
static int
_Xsj3cReadRC(fp, rctable)
    FILE               *fp;
    Sjrctable           rctable;
{
    static unsigned char        buf[256];
    register int                i,  j,  k,  end,    byte2 = 0,  kana = 0;

    if ((fgets (buf, sizeof(buf), fp)) == NULL) 
        return END;
    
    if (buf[0] == '\n' || buf[0] == '#' || buf[0] == '\0' )
        return COMMENT;

    for (i = 0, k = 0, end = 0; buf[i] !=  ' ' && buf[i] != '\t' && 
        buf[i] != '#' && buf[i] != '\n';) {
        for (j = i; buf[i] != ' ' && buf[i] != '\t'
                && buf[i] != '#' && buf[i] != '\n' && buf[i] != '.'; i++);
        if (j == i && buf[i] == '.' ) {
            i++;
            continue;
        }
        if (buf[i] == '#' || buf[i] == '\n' )
            return BAD;
        if (buf[i] == '\t' || buf[i] == ' ' )
            end++;
        buf[i++] = '\0';
        if (!strcmp(&buf[j],SERIAL) || !strcmp(&buf[j],SERIAL2)
            || !strcmp(&buf[j],SERIAL3))
            return OTHERS;
        rctable->key[k++] =  &buf[j];
        if (end)
            break;
    }
    if ( k < 1) 
        return BAD;
    for (; k < 2;)
        rctable->key[k++] = NULL;
    while(buf[i] == ' ' || buf[i] == '\t') i++;
    if (in_lang == JP_SJIS || in_lang == JP_EUC) {
        for (k = 0, end = 0; buf[i] != '\n';) {
            for (j = i; buf[i] != '\n' ; i+=2) {
                if (iskan1(buf[i], in_lang)) {
                    if (iskan2(buf[i + 1], in_lang))
                        continue;
                    else if (buf[i + 1] == '#' || buf[i + 1] == ' '
                            || buf[i + 1] == '\t'|| buf[i + 1] == '\n') {
                        i++;
                        break;
                    } else
                        continue;
                } else if (buf[i] == '#' || buf[i] == ' ' || buf[i] == '\t') {
                    break;
                } else if (buf[i] == '.') {
                    if (i > 0 && buf[i - 1] == '\\') {
                        continue;
                    } else
                        break;
                } else if (buf[i + 1] == '#' || buf[i + 1] == ' '
                        || buf[i + 1] == '\t' || buf[i + 1] == '\n') {
                    i++;
                    break;
                } else if (buf[i + 1] == '.') {
                    if (buf[i] == '\\') {
                        continue;
                    } else {
                        i++;
                        break;
                    }
                } else 
                    continue;
            }
            if (buf[i] == '#' || buf[i] == '\n' )
                end++;
            buf[i++] = '\0';
            rctable->value[k++] = &buf[j];
            if (end)
                break;
            while(buf[i] == ' ' || buf[i] == '\t') i++;
        }
    } else {
        for (k = 0, end = 0; buf[i] != '\n';) {
            for (j = i; buf[i] != '\n'; i++) {
                if (buf[i] == ESC) {
                    if (buf[i + 1] == '#' || buf[i + 1] == ' ' ||
                            buf[i + 1] == '\t'|| buf[i + 1] == '.' ||
                            buf[i + 1] == '\n') {
                        i++;
                        break;
                    } else if (byte2)
                        byte2 = 0;
                    else
                        byte2++;
                    i += 2;
                    continue;
                } else if (buf[i] == SO || buf[i] == SI) {
                    if (buf[i + 1] == '#' || buf[i + 1] == ' ' ||
                            buf[i + 1] == '\t'|| buf[i + 1] == '.' ||
                            buf[i + 1] == '\n') {
                        i++;
                        break;
                    } else if (buf[i] == SI)
                        kana = 0;
                    else
                        kana++;
                    continue;
                } else if (byte2 || kana)
                    continue;
                if (buf[i] == '#' || buf[i] == ' ' || buf[i] == '\t') {
                    break;
                } else if (buf[i] == '.') {
                    if (i > 0 && buf[i - 1] == '\\') {
                        continue;
                    } else
                        break;
                } else 
                    continue;
            }
            if (buf[i] == '#' || buf[i] == '\n' )
                end++;
            buf[i++] = '\0';
            rctable->value[k++] = &buf[j];
            if (end)
                break;
            while(buf[i] == ' ' || buf[i] == '\t') i++;
        }
    }
    if ( k < 1) 
        return BAD;
    for (; k < 10;)
        rctable->value[k++] = NULL;
    return NORMAL;
}

/*
 * _Xsj3cSetInputMode()
 *  Set a parameter by .initialmode entry.
 */
static int
_Xsj3cSetInputMode(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    register int    i,  j;

    for (i = 0, j = 0; i < MODE_INROT_NUM; i++, j++) {
        if (table->value[j]) {
            if (_Xsj3cCmp(table->value[j], "hiragana")) {
                buf->inmoderot[i] = MODE_HIRA;
            } else if (_Xsj3cCmp(table->value[j], "zkatakana")) {
                buf->inmoderot[i] = MODE_ZKATA;
            } else if (_Xsj3cCmp(table->value[j], "hkatakana")) {
                buf->inmoderot[i] = MODE_HKATA;
            } else if (_Xsj3cCmp(table->value[j], "halpha")) {
                buf->inmoderot[i] = MODE_HALPHA;
            } else if (_Xsj3cCmp(table->value[j], "zalpha")) {
                buf->inmoderot[i] = MODE_ZALPHA;
            } else if (_Xsj3cCmp(table->value[j], "jis")) {
                buf->inmoderot[i] = MODE_JIS;
            } else if (_Xsj3cCmp(table->value[j], "sjis")) {
                buf->inmoderot[i] = MODE_SJIS;
            } else if (_Xsj3cCmp(table->value[j], "kuten")) {
                buf->inmoderot[i] = MODE_KUTEN;
            } else if (_Xsj3cCmp(table->value[j], "euc")) {
                buf->inmoderot[i] = MODE_EUC;
            } else {
                if (!_Xsj3cCmp(table->value[j], "unbuff") 
                        && !_Xsj3cCmp(table->value[j], "buffer")
                        && !_Xsj3cCmp(table->value[j], "unbuffer")
                        && !_Xsj3cCmp(table->value[j], "direct"))
                    Xsj3cWarning("Unknown input mode \"%s\"",table->value[j]);
                i--;
                continue;
            }
        } else {
            break;
        }
    }
    if ((buf->inmoderotnum = i) > 0)
        return 1;
    else
        return 0;
}

/*
 * _Xsj3cSetPrintMode()
 *  Set a parameter by .PrintMode entry.
 */
static int
_Xsj3cSetPrintMode(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    register int    i,  j;

    for (i = 0, j = 0; i < MODE_OUTROT_NUM; i++, j++) {
        if (table->value[j]) {
            if (_Xsj3cCmp(table->value[j], "hiragana")) {
                buf->outmoderot[i] = MODE_HIRA;
            } else if (_Xsj3cCmp(table->value[j], "zkatakana")) {
                buf->outmoderot[i] = MODE_ZKATA;
            } else if (_Xsj3cCmp(table->value[j], "hkatakana")) {
                buf->outmoderot[i] = MODE_HKATA;
            } else if (_Xsj3cCmp(table->value[j], "halpha")) {
                buf->outmoderot[i] = MODE_HALPHA;
            } else if (_Xsj3cCmp(table->value[j], "zalpha")) {
                buf->outmoderot[i] = MODE_ZALPHA;
            } else if (_Xsj3cCmp(table->value[j], "hankaku")) {
                buf->outmoderot[i] = MODE_HANKAKU;
            } else if (_Xsj3cCmp(table->value[j], "zenkaku")) {
                buf->outmoderot[i] = MODE_ZENKAKU;
            } else if (_Xsj3cCmp(table->value[j], "upper")) {
                buf->outmoderot[i] = MODE_UPPER;
            } else if (_Xsj3cCmp(table->value[j], "lower")) {
                buf->outmoderot[i] = MODE_LOWER;
            } else {
                    Xsj3cWarning("Unknown print mode \"%s\"",table->value[j]);
                i--;
                continue;
            }
        } else {
            break;
        }
    }
    if ((buf->outmoderotnum = i) > 0)
        return 1;
    else
        return 0;
}

/*
 * _Xsj3cSetDefCode()
 *  Set a parameter by .defaultcode entry.
 */
static int
_Xsj3cSetDefCode(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    register int    i;

    for (i = 0; i < MODE_CODE_NUM; i++) {
        if (table->value[i]) {
            if (_Xsj3cCmp(table->value[i], "jis")) {
                buf->defcode[i] = MODE_JIS;
            } else if (_Xsj3cCmp(table->value[i], "sjis")) {
                buf->defcode[i] = MODE_SJIS;
            } else if (_Xsj3cCmp(table->value[i], "kuten")) {
                buf->defcode[i] = MODE_KUTEN;
            } else if (_Xsj3cCmp(table->value[i], "euc")) {
                buf->defcode[i] = MODE_EUC;
            } else {
                Xsj3cWarning("Unknown input code");
                break;
            }
        } else {
            break;
        }
    }
    if ((buf->coderotnum = i) > 0)
        return 1;
    else
        return 0;
}

/*
 * _Xsj3cSetMuhenkan()
 *  Set a parameter by .MuhenkanMode entry.
 */
static int
_Xsj3cSetMuhenkan(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (table->value[0]) {
        if (_Xsj3cCmp(table->value[0], "hiragana")) {
            buf->muhenmode = MODE_HIRA;
        } else if (_Xsj3cCmp(table->value[0], "zkatakana")) {
            buf->muhenmode = MODE_ZKATA;
        } else if (_Xsj3cCmp(table->value[0], "hkatakana")) {
            buf->muhenmode = MODE_HKATA;
        } else if (_Xsj3cCmp(table->value[0], "halpha")) {
            buf->muhenmode = MODE_HALPHA;
        } else if (_Xsj3cCmp(table->value[0], "zalpha")) {
            buf->muhenmode = MODE_ZALPHA;
        } else if (_Xsj3cCmp(table->value[0], "jis")) {
            buf->muhenmode = MODE_JIS;
        } else if (_Xsj3cCmp(table->value[0], "sjis")) {
            buf->muhenmode = MODE_SJIS;
        } else if (_Xsj3cCmp(table->value[0], "kuten")) {
            buf->muhenmode = MODE_KUTEN;
        } else if (_Xsj3cCmp(table->value[0], "euc")) {
            buf->muhenmode = MODE_EUC;
        } else {
            Xsj3cWarning("Invalid muhenkan mode \"%s\"",table->value[0]);
        }
    }
    return 1;
}

/*
 * _Xsj3cSetMuEdit()
 *  Set a parameter by .MuhenkanInEdit entry.
 */
static int
_Xsj3cSetMuEdit(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (table->value[0]) {
        if (_Xsj3cCmp(table->value[0], "hiragana")) {
            buf->togglemode = MODE_HIRA;
        } else if (_Xsj3cCmp(table->value[0], "zkatakana")) {
            buf->togglemode = MODE_ZKATA;
        } else if (_Xsj3cCmp(table->value[0], "hkatakana")) {
            buf->togglemode = MODE_HKATA;
        } else if (_Xsj3cCmp(table->value[0], "halpha")) {
            buf->togglemode = MODE_HALPHA;
        } else if (_Xsj3cCmp(table->value[0], "zalpha")) {
            buf->togglemode = MODE_ZALPHA;
        } else if (_Xsj3cCmp(table->value[0], "jis")) {
            buf->togglemode = MODE_JIS;
        } else if (_Xsj3cCmp(table->value[0], "sjis")) {
            buf->togglemode = MODE_SJIS;
        } else if (_Xsj3cCmp(table->value[0], "kuten")) {
            buf->togglemode = MODE_KUTEN;
        } else if (_Xsj3cCmp(table->value[0], "euc")) {
            buf->togglemode = MODE_EUC;
        } else {
            Xsj3cWarning("Unknown toggle mode");
        }
    }

    return 1;
}

/*
 * _Xsj3cSetMToggle()
 *  Set a parameter by .MuhenkanToggle entry.
 */
static int
_Xsj3cSetMToggle(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->dotoggle = ON;
    else
        buf->dotoggle = OFF;
    return 1;
}

/*
 * _Xsj3cSetGuide()
 *  Set a parameter by .guide entry.
 */
int
_Xsj3cSetGuide(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3csMode          mode;
    wchar               data[KANABUFSIZ];

    if (table->key[1] && table->value[0]) {
        if (_Xsj3cCmp(table->key[1], "hiragana")) {
            mode = MODE_HIRA;
        } else if (_Xsj3cCmp(table->key[1], "zkatakana")) {
            mode = MODE_ZKATA;
        } else if (_Xsj3cCmp(table->key[1], "hkatakana")) {
            mode = MODE_HKATA;
        } else if (_Xsj3cCmp(table->key[1], "halpha")) {
            mode = MODE_HALPHA;
        } else if (_Xsj3cCmp(table->key[1], "zalpha")) {
            mode = MODE_ZALPHA;
        } else if (_Xsj3cCmp(table->key[1], "jis")) {
            mode = MODE_JIS;
        } else if (_Xsj3cCmp(table->key[1], "sjis")) {
            mode = MODE_SJIS;
        } else if (_Xsj3cCmp(table->key[1], "kuten")) {
            mode = MODE_KUTEN;
        } else if (_Xsj3cCmp(table->key[1], "euc")) {
            mode = MODE_EUC;
        } else if (_Xsj3cCmp(table->key[1], "toroku")) {
            mode = MODE_TOROKU;
        } else if (_Xsj3cCmp(table->key[1], "register")) {
            mode = MODE_TOROKU;
        } else if (_Xsj3cCmp(table->key[1], "syoukyo")) {
            mode = MODE_SYOUKYO;
        } else if (_Xsj3cCmp(table->key[1], "eliminate")) {
            mode = MODE_SYOUKYO;
        } else if (_Xsj3cCmp(table->key[1], "kanji")) {
            mode = MODE_KANJI;
        } else if (_Xsj3cCmp(table->key[1], "edit")) {
            mode = MODE_EDIT;
        } else if (_Xsj3cCmp(table->key[1], "candidate")) {
            mode = MODE_CAND;
        } else if (_Xsj3cCmp(table->key[1], "symbol")) {
            mode = MODE_SYMBOL;
        } else if (_Xsj3cCmp(table->key[1], "hinsi")) {
            mode = MODE_HINSI;
        } else if (_Xsj3cCmp(table->key[1], "quote")) {
            mode = MODE_QUOTE;
        } else {
            Xsj3cWarning("Unknown display mode key \"%s\"",table->key[1]);
            return 0;
        }
        _Xsj3cmINtowOUT(buf->rcfile, table->value[0], data,
                &buf->modelen[mode]);
        if (!buf->modestr[mode]) {
            if (!(buf->modestr[mode]
                = (wchar *)malloc((buf->modelen[mode] + 1) * sizeof(wchar))))
                Xsj3cError("Cannot allocate for mode string");
        } else {
            if (!(buf->modestr[mode] = (wchar *)realloc(buf->modestr[mode],
                    (buf->modelen[mode] + 1) * sizeof(wchar))))
                Xsj3cError("Cannot reallocate for mode string");
        }
        _Xsj3cWcpy(buf->modestr[mode], data);
        return 1;
    } else {
        return 0;
    }
}

/*
 * _Xsj3cSetBStudy()
 *  Set a parameter by .bstudy entry.
 */
static int
_Xsj3cSetBStudy(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->gakusyuu = ON;
    else
        buf->gakusyuu = OFF;
    return 1;
}

/*
 * _Xsj3cSetFlushAfterConv()
 *  Set a parameter by .flushafterconversion entry.
 */
static int
_Xsj3cSetFlushAfterConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->flushaconv = ON;
    else
        buf->flushaconv = OFF;
    return 1;
}

/*
 * _Xsj3cSetFlushInConv()
 *  Set a parameter by .FlushInConversion entry.
 */
static int
_Xsj3cSetFlushInConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->flushiconv = ON;
    else if (_Xsj3cCmp(table->value[0],"off"))
        buf->flushiconv = OFF;
    else if (_Xsj3cCmp(table->value[0],"edit"))
        buf->flushiconv = EDIT;
    else
        buf->flushiconv = NONE;
    return 1;
}

/*
 * _Xsj3cSetFlushSelectConv()
 *  Set a parameter by .FlushSelectConversion entry.
 */
static int
_Xsj3cSetFlushSelectConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->flushsconv = ON;
    else if (_Xsj3cCmp(table->value[0],"off"))
        buf->flushsconv = OFF;
    else if (_Xsj3cCmp(table->value[0],"edit"))
        buf->flushsconv = EDIT;
    else
        buf->flushsconv = NONE;
    return 1;
}

/*
 * _Xsj3cSetFlushEndConv()
 *  Set a parameter by .FlushEndConversion entry.
 */
static int
_Xsj3cSetFlushEndConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->flusheconv = ON;
    else
        buf->flusheconv = OFF;
    return 1;
}

/*
 * _Xsj3cSetRkeBell()
 *  Set a parameter by .rkerrbell entry.
 */
static int
_Xsj3cSetRkeBell(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->rkbell = ON;
    else
        buf->rkbell = OFF;
    return 1;
}

/*
 * _Xsj3cSetSj3Serv()
 *  Set a parameter by .server entry.
 */
static int
_Xsj3cSetSj3Serv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (table->value[0]) {
        static char hostname1[32];
        _Xsj3cReadAscii(buf->rcfile, table->value[0], hostname1);
        buf->sj3serv = hostname1;
        return 1;
    }
    return 0;
}

/*
 * _Xsj3cSetNormal()
 *  Set a parameter by .SetNormal entry.
 */
static int
_Xsj3cSetNormal(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (table->value[0] && iscntrl(*table->value[0])) {
        register unsigned char  *c;
        register int             len;
        if (buf->setnormal) {
            c = buf->setnormal;
            while (*c != '\0') {
                if (*c == *table->value[0])
                    return 1;   /* same code */
                c++;
            }
            len = c - buf->setnormal;
            buf->setnormal = (unsigned char *)realloc(buf->setnormal, len + 2);
            *(buf->setnormal + len) = *table->value[0];
            *(buf->setnormal + len + 1) = '\0';
        } else {
            buf->setnormal = (unsigned char *)malloc(2);
            c = buf->setnormal;
            *c++ = *table->value[0];
            *c = '\0';
        }
        return 1;
    }
    return 0;
}

/*
 * _Xsj3cSetThroughNext()
 *  Set a parameter by .ThroughtNext entry.
 */
static int
_Xsj3cSetThroughNext(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (table->value[0] && iscntrl(*table->value[0])) {
        register unsigned char  *c;
        int                      len;
        if (buf->throughnext) {
            c = buf->throughnext;
            while (*c != '\0') {
                if (*c == *table->value[0])
                    return 1;   /* same code */
                c++;
            }
            len = c - buf->throughnext;
            buf->throughnext
                    = (unsigned char *)realloc(buf->throughnext, len + 2);
            *(buf->throughnext + len) = *table->value[0];
            *(buf->throughnext + len + 1) = '\0';
        } else {
            buf->throughnext = (unsigned char *)malloc(2);
            c = buf->throughnext;
            *c++ = *table->value[0];
            *c = '\0';
        }
        return 1;
    }
    return 0;
}

/*
 * _Xsj3cSetSj3Serv2()
 *  Set a parameter by .sj3serv2 entry.
 */
static int
_Xsj3cSetSj3Serv2(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (table->value[0]) {
        static char hostname2[32];
        _Xsj3cReadAscii(buf->rcfile, table->value[0], hostname2);
        buf->sj3serv2 = hostname2;
        return 1;
    }
    return 0;
}

/*
 * _Xsj3cSetAlphaConv()
 *  Set a parameter by .AlphabetConversion entry.
 */
static int
_Xsj3cSetAlphaConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->alphaconv = ON;
    else
        buf->alphaconv = OFF;
    return 1;
}

/*
 * _Xsj3cSetBackDisplay()
 *  Set a parameter by .BackDisplay entry.
 */
static int
_Xsj3cSetBackDisplay(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->backdisplay = ON;
    else
        buf->backdisplay = OFF;
    return 1;
}

/*
 * _Xsj3cSetSegLast()
 *  Set a parameter by .BeginConversionLast entry.
 */
static int
_Xsj3cSetSegLast(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->beginlastseg = ON;
    else if (_Xsj3cCmp(table->value[0],"off"))
        buf->beginlastseg = OFF;
    else 
        buf->beginlastseg = NONE;
    return 1;
}

/*
 * _Xsj3cSetShrinkAll()
 *  Set a parameter by .ShrinkAll entry.
 */
static int
_Xsj3cSetShrinkAll(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->shrinkall = ON;
    else
        buf->shrinkall = OFF;
    return 1;
}

/*
 * _Xsj3cSetDelChange()
 *  Set a parameter by .DeleteChangeSegment entry.
 */
static int
_Xsj3cSetDelChange(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"all"))
        buf->delchange = ALL;
    else if (_Xsj3cCmp(table->value[0],"after"))
        buf->delchange = AFTER;
    else
        buf->delchange = ONE;
    return 1;
}

/*
 * _Xsj3cSetFlushChange()
 *  Set a parameter by .FlushChangeSegment entry.
 */
static int
_Xsj3cSetFlushChange(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"all"))
        buf->flushchange = ALL;
    else
        buf->flushchange = ONE;
    return 1;
}

/*
 * _Xsj3cSetExpandKConv()
 *  Set a parameter by .ExpandKanjiConversion entry.
 */
static int
_Xsj3cSetExpandKConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->expandkconv = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetShrinkKConv()
 *  Set a parameter by .ShrinkKanjiConversion entry.
 */
static int
_Xsj3cSetShrinkKConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->shrinkkconv = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetExpandMConv()
 *  Set a parameter by .ExpandModeConversion entry.
 */
static int
_Xsj3cSetExpandMConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->expandmconv = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetShrinkMConv()
 *  Set a parameter by .ShrinkModeConversion entry.
 */
static int
_Xsj3cSetShrinkMConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->shrinkmconv = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetMoveLoop()
 *  Set a parameter by .MoveLoop entry.
 */
static int
_Xsj3cSetMoveLoop(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->moveloop = ON;
    else
        buf->moveloop = OFF;
    return 1;
}

/*
 * _Xsj3cSetJumpBySeg()
 *  Set a parameter by .JumpBySegment entry.
 */
static int
_Xsj3cSetJumpBySeg(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag       input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->jumpbyseg = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetModeBySeg()
 *  Set a parameter by .MoveBySegment entry.
 */
static int
_Xsj3cSetModeBySeg(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->movebyseg = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetDelBySeg()
 *  Set a parameter by .DeleteBySegment entry.
 */
static int
_Xsj3cSetDelBySeg(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->delbyseg = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetKillBySeg()
 *  Set a parameter by .KillBySegment entry.
 */
static int
_Xsj3cSetKillBySeg(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3cFlag           input, conved;

    if (!(table->value[0] && table->value[1])) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        input = ON;
    else
        input = OFF;
    if (_Xsj3cCmp(table->value[1],"on"))
        conved = ON;
    else
        conved = OFF;
    buf->killbyseg = (conved << 1) + input;
    return 1;
}

/*
 * _Xsj3cSetMuhenCursorLast()
 *  Set a parameter by .MuhenkanCursorLast entry.
 */
static int
_Xsj3cSetMuhenCursorLast(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->muhencurlast = ON;
    else
        buf->muhencurlast = OFF;
    return 1;
}

/*
 * _Xsj3cSetEditCursorLast()
 *  Set a parameter by .EditCursorLast entry.
 */
static int
_Xsj3cSetEditCursorLast(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->editcurlast = ON;
    else
        buf->editcurlast = OFF;
    return 1;
}

/*
 * _Xsj3cSetFlushCursorLast()
 *  Set a parameter by .FlushCursorLast entry.
 */
static int
_Xsj3cSetFlushCursorLast(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->flushcurlast = ON;
    else
        buf->flushcurlast = OFF;
    return 1;
}

/*
 * _Xsj3cSetUnconvSeg()
 *  Set a parameter by .MuhenkanSegment entry.
 */
static int
_Xsj3cSetUnconvSeg(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"all"))
        buf->muhenseg = ALL;
    else if (_Xsj3cCmp(table->value[0],"after"))
        buf->muhenseg = AFTER;
    else
        buf->muhenseg = ONE;
    return 1;
}

/*
 * _Xsj3cSetConvSeg()
 *  Set a parameter by .HenkanSegment entry.
 */
static int
_Xsj3cSetConvSeg(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"all"))
        buf->henkanseg = ALL;
    else if (_Xsj3cCmp(table->value[0],"after"))
        buf->henkanseg = AFTER;
    else
        buf->henkanseg = ONE;
    return 1;
}

/*
 * _Xsj3cSetModeConv()
 *  Set a parameter by .ModeConversion entry.
 */
static int
_Xsj3cSetModeConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    Xsj3csMode          mode;
    Xsj3cFlag           input, conved,  noinput;

    if (table->key[1] && table->value[0] && table->value[1] &&
            table->value[2]) {
        if (_Xsj3cCmp(table->key[1], "hiragana")) {
            mode = MODE_HIRA;
        } else if (_Xsj3cCmp(table->key[1], "zkatakana")) {
            mode = MODE_ZKATA;
        } else if (_Xsj3cCmp(table->key[1], "hkatakana")) {
            mode = MODE_HKATA;
        } else if (_Xsj3cCmp(table->key[1], "halpha")) {
            mode = MODE_HALPHA;
        } else if (_Xsj3cCmp(table->key[1], "zalpha")) {
            mode = MODE_ZALPHA;
        } else if (_Xsj3cCmp(table->key[1], "rollup")) {
            mode = MODE_ROLLUP;
        } else if (_Xsj3cCmp(table->key[1], "rolldown")) {
            mode = MODE_ROLLDOWN;
        } else {
            Xsj3cWarning("Unknown conversion mode");
        }
        if (_Xsj3cCmp(table->value[0],"on"))
            input = ON;
        else
            input = OFF;
        if (_Xsj3cCmp(table->value[1],"on"))
            conved = ON;
        else
            conved = OFF;
        if (_Xsj3cCmp(table->value[2],"on"))
            noinput = ON;
        else
            noinput = OFF;
        buf->modeconv[mode] = (noinput << 2) + (conved << 1) + input;
        return 1;
    } else {
        return 0;
    }
}

/*
 * _Xsj3cSetUnderLine()
 *  Set a parameter by .ConvertedUnderLine entry.
 */
static int
_Xsj3cSetUnderLine(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->convedunderline = ON;
    else
        buf->convedunderline = OFF;
    return 1;
}

/*
 * _Xsj3cSetDispMChange()
 *  Set a parameter by .DisplayModeChange entry.
 */
static int
_Xsj3cSetDispMChange(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->dispmodechange = ON;
    else
        buf->dispmodechange = OFF;
    return 1;
}

/*
 * _Xsj3cSetDelLastMove()
 *  Set a parameter by .DeleteLastMove entry.
 */
static int
_Xsj3cSetDelLastMove(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->dellastmove = ON;
    else
        buf->dellastmove = OFF;
    return 1;
}

/*
 * _Xsj3cSetKanaInput()
 *  Set a parameter by .KanaInputOnly entry.
 */
static int
_Xsj3cSetKanaInput(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->kanaonly = ON;
    else
        buf->kanaonly = OFF;
    return 1;
}

/*
 * _Xsj3cSetInputSame()
 *  Set a parameter by .InputSameTime entry.
 */
static int
_Xsj3cSetInputSame(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->inputsame = ON;
    else
        buf->inputsame = OFF;
    return 1;
}

/*
 * _Xsj3cSetCntrlSame()
 *  Set a parameter by .CntrlSameTime entry.
 */
static int
_Xsj3cSetCntrlSame(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->cntrlsame = ON;
    else
        buf->cntrlsame = OFF;
    return 1;
}

/*
 * _Xsj3cSetSelectConv()
 *  Set a parameter by .BeforeSelectConversion entry.
 */
static int
_Xsj3cSetSelectConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->selectconv = ON;
    else
        buf->selectconv = OFF;
    return 1;
}

/*
 * _Xsj3cSetBeforeConv()
 *  Set a parameter by .BeforeConversion entry.
 */
static int
_Xsj3cSetBeforeConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->beforeconv = ON;
    else
        buf->beforeconv = OFF;
    return 1;
}

/*
 * _Xsj3cSetLastDoubleConv()
 *  Set a parameter by .BeforeConversion entry.
 */
static int
_Xsj3cSetLastDoubleConv(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->lastdoubleconv = ON;
    else
        buf->lastdoubleconv = OFF;
    return 1;
}

/*
 * _Xsj3cSetSelectCount()
 *  Set a parameter by .SelectCount entry.
 */
static int
_Xsj3cSetSelectCount(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    register unsigned char     *p;

    if (!table->value[0]) 
        return 0;
    p = table->value[0];
    while (*p != '\0') {
        if (!isdigit(*p++))
            return 0;
    }
    buf->selectcount = atoi(table->value[0]);
    return 1;
}

/*
 * _Xsj3cSetSelectBackMove()
 *  Set a parameter by .SelectBackSpaceMove entry.
 */
static int
_Xsj3cSetSelectBackMove(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->selectback = ON;
    else
        buf->selectback = OFF;
    return 1;
}

/*
 * _Xsj3cSetCandidatePad()
 *  Set a parameter by .CandidatePadding entry.
 */
static int
_Xsj3cSetCandidatePad(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    if (!table->value[0]) 
        return 0;
    if (_Xsj3cCmp(table->value[0],"on"))
        buf->candpadding = ON;
    else
        buf->candpadding = OFF;
    return 1;
}

/*
 * _Xsj3cSetNextRCFile()
 *  Set a parameter by .NextRCFile entry.
 */
static int
_Xsj3cSetNextRCFile(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    char                file[BUFSIZ];

    if (table->value[0]) {
        strcpy (file, buf->rcfile);
        _Xsj3cReadAscii(file, table->value[0], buf->rcfile);
        return 1;
    }
    return 0;
}

/*
 * _Xsj3cSetIncludeRCFile()
 *  Set a parameter by .include entry.
 */
static int
_Xsj3cSetIncludeRCFile(buf, table)
    Xsj3cBuf            buf;
    Sjrctable           table;
{
    char                file[BUFSIZ],   name[BUFSIZ];
    SjrcRec             rctable;
    struct functable   *functp;
    FILE               *fp;
    register int        line;
    int                 status;

    inc_cnt++;
    if (inc_cnt > SJRC_INCLUDE_MAX) {
        Xsj3cWarning("sjrc include nesting too deep");
        return 0;
    }
    if (table->value[0]) {
        /*  Get include file and open */
        _Xsj3cReadAscii(table->value[0], table->value[0], name);
        strcpy(file, name);
        if ((fp = fopen(file, "r")) == NULL) {
            strcpy(file, SJ3DEFPATH);
            strcat(file, name);
            if ((fp = fopen(file, "r")) == NULL) {
                Xsj3cWarning("can't open include sjrc file %s", file);
                return 0;
            }
        }
    } else {
        return 0;
    }

    /*  Read sjrc file and set buffer fields    */
    for (line = 0; (status = 
            _Xsj3cReadRC(fp, &rctable)) != END; line++)  {
        functp = funcs;
        if (status == NORMAL) {
            while (functp->keyword != NULL) {
                if (_Xsj3cCmp(rctable.key[0], functp->keyword)) {
                    if (!(*(functp->func))(buf, &rctable))
                        Xsj3cWarning("wrong format in include sjrc file. %s:%d",
                                file, line + 1);
                    break;
                }
                functp++;
            }
        } else if (status == COMMENT || status == OTHERS) {
            continue;
        } else {
            Xsj3cWarning("bad line in sjrc include file. %s:%d",
                    file, line + 1);
            continue;
        }
    }
    fclose(fp);
    return 1;
}
