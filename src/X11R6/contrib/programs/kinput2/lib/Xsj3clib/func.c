#ifndef lint
static char *rcsid = "$Header: func.c,v 2.3 93/09/21 09:42:57 nao Exp $";
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

#include <ctype.h>
#include "common.h"
#include "sj3ctype.h"
#include "util.h"
#include "func.h"

extern Xsj3cCVServerIF  serverIF[SERVER_NUM];
extern void             Xsj3cRCInit();
extern int              _Xsj3cRomaConv();
extern int              _Xsj3cKanaConv();
extern Xsj3cCand        _Xsj3cCandidateInit();
extern void             _Xsj3cFlushDictMsg();
extern Xsj3cDictData    _Xsj3cCreateDictData();

Xsj3cEvent              _Xsj3cConvert();
Xsj3cEvent              _Xsj3cUnConvert();
Xsj3cEvent              _Xsj3cFix();
Xsj3cEvent              _Xsj3cReturn();

Xsj3cEvent              _Xsj3cModeHAlpha();
Xsj3cEvent              _Xsj3cModeZAlpha();
Xsj3cEvent              _Xsj3cModeHKata();
Xsj3cEvent              _Xsj3cModeZKata();
Xsj3cEvent              _Xsj3cModeHira();
Xsj3cEvent              _Xsj3cToHAlpha();
Xsj3cEvent              _Xsj3cToZAlpha();
Xsj3cEvent              _Xsj3cToHKata();
Xsj3cEvent              _Xsj3cToZKata();
Xsj3cEvent              _Xsj3cToHira();
Xsj3cEvent              _Xsj3cZenkaku();
Xsj3cEvent              _Xsj3cHankaku();
Xsj3cEvent              _Xsj3cToUpper();
Xsj3cEvent              _Xsj3cToLower();
Xsj3cEvent              _Xsj3cModeSJIS();
Xsj3cEvent              _Xsj3cModeEUC();
Xsj3cEvent              _Xsj3cModeJIS();
Xsj3cEvent              _Xsj3cModeKuten();
Xsj3cEvent              _Xsj3cCodeRollDown();
Xsj3cEvent              _Xsj3cModeRollDown();
Xsj3cEvent              _Xsj3cModeRollUp();
Xsj3cEvent              _Xsj3cNextMode();
Xsj3cEvent              _Xsj3cPrevMode();
Xsj3cEvent              _Xsj3cModeToggle();

Xsj3cEvent              _Xsj3cForward();
Xsj3cEvent              _Xsj3cBackward();
Xsj3cEvent              _Xsj3cTop();
Xsj3cEvent              _Xsj3cEnd();
Xsj3cEvent              _Xsj3cUp();
Xsj3cEvent              _Xsj3cDown();
Xsj3cEvent              _Xsj3cFirst();
Xsj3cEvent              _Xsj3cLast();
Xsj3cEvent              _Xsj3cNextPage();
Xsj3cEvent              _Xsj3cPrevPage();
Xsj3cEvent              _Xsj3cNext();
Xsj3cEvent              _Xsj3cPrev();
Xsj3cEvent              _Xsj3cSelect();
Xsj3cEvent              _Xsj3cCancel();

Xsj3cEvent              _Xsj3cExpand();
Xsj3cEvent              _Xsj3cShrink();

Xsj3cEvent              _Xsj3cBackSpace();
Xsj3cEvent              _Xsj3cDelete();
Xsj3cEvent              _Xsj3cDelAfter();

Xsj3cEvent              _Xsj3cStart();
Xsj3cEvent              _Xsj3cReConnect();
Xsj3cEvent              _Xsj3cReConvert();
Xsj3cEvent              _Xsj3cEdit();

Xsj3cEvent              _Xsj3cDRegBegin();
Xsj3cEvent              _Xsj3cDClearBegin();

Xsj3cEvent              _Xsj3cSymbolBegin();

Xsj3cEvent              _Xsj3cQuote();
Xsj3cEvent              _Xsj3cBell();
Xsj3cEvent              _Xsj3cKana();
Xsj3cEvent              _Xsj3cSjrc();
Xsj3cEvent              _Xsj3cKill();
Xsj3cEvent              _Xsj3cNull();
Xsj3cEvent              _Xsj3cIgnore();

Xsj3cEvent              _Xsj3cUnConvSeg();

static Xsj3cEvent       _Xsj3cDeleteSeg();
static Xsj3cEvent       _Xsj3cDeleteChar();
static Xsj3cEvent       _Xsj3cBackSpaceChar();
static Xsj3cEvent       _Xsj3cExpandNoConv();
static Xsj3cEvent       _Xsj3cShrinkNoConv();

/*
 * _Xsj3cConvert() [henkan/convert]
 *
 * <InputMode> Do kana-kanji conversion.
 * <ConvedMode> If character mode of current segment is zenkaku-hiragana,
 *   pop up candidate panel, else convert to zenkaku-hiragana.
 * <NoInputMode/SelectMode/DictMode> Rings bell.
 *
 * BeginConversionLast none: Set current segment out of segments. 
 * BeginConversionLast off: Set current segment to the first segment.
 * BeginConversionLast on: Set current segment to the last segment.
 * DisplayModeChange on: Change the display mode string.  
 * HenkanSegment all: Convert all segments.
 * HenkanSegment after: Convert segments after current segment.
 * HenkanSegment one: Convert current segment.
 * BeforeSelectConversion on: Convert current segment to hiragana
 *   and need one more step to display candidates.
 * BeforeConversion on: Convert current segment to hiragana before
 *   Kana-Kanji conversion.
 * BeforeSelectCount: Work like "next/wrap" before popup candidate panel
 *   at [ConvedMode].
 * LastDoubleConversion on: Before Kana-Kanji conversion, convert current
 *   segment to current character mode.
 */
Xsj3cEvent
_Xsj3cConvert(buf)
    Xsj3cBuf  buf;
{
    unsigned char               knjbuf[KANJIBUFSIZ];
    unsigned char               kanabuf[KANJIBUFSIZ];
    Xsj3cEvent                  ret = KEY_NULL;
    SJ3_BUNSETU                 bun[BUNBUFSIZ];
    int                         i,  value;
    register int                conved, begin,  end;

    if (buf->segnum && buf->curseg >= buf->segnum) {
        buf->curseg = buf->segnum - 1;
        if (buf->input[buf->curseg] &&
                (buf->input[buf->curseg]->status & SEG_CONVED))
            buf->convmode = ConvedModeMask;
        else 
            buf->convmode = InputModeMask;
        ret = KEY_TEXT_CHANGE;
    }
    if ((buf->convmode & (InputModeMask|ConvedModeMask))
            && buf->curseg < buf->segnum) {
        if (buf->convmode & ConvedModeMask) {
            if (buf->selectconv &&
                    buf->input[buf->curseg]->cursegmode != MODE_HIRA) {
                ret |= _Xsj3cModeChange(buf, MODE_HIRA, ON);
                buf->convedsegnum--;
                buf->input[buf->curseg]->status = SEG_NOCONV;
            } else {
                if (buf->selectcount > 0) {
                    int     cur;
                    if (!buf->candidate)
                        Xsj3cGetCandidateNum(buf, &cur);
                    if (buf->selectcount < buf->candnum) {
                        if (buf->n_select < buf->selectcount) {
                            buf->n_select++;
                            return(_Xsj3cNext(buf));
                        }
                    }
                }
                buf->convmode = SelectModeMask;
                ret |= KEY_CAND_START;
                if (buf->dispmodechange) {
                    buf->dispmode = MODE_CAND;
                    ret |= KEY_MODE_CHANGE;
                }
                buf->selectstatus = SELECT_CAND;
                buf->n_select = 0;
                return (ret);
            }
        } else if (buf->beforeconv) {
            register wchar  *p;
            p = buf->input[buf->curseg]->yomi;
            while (*p != '\0') {
                if (!ishira(*p, serverIF[buf->server].lang)) {
                    ret |= _Xsj3cModeChange(buf, MODE_HIRA, ON);
                    break;
                }
                p++;
            }
        } else if (buf->lastdoubleconv && *buf->rkdouble != '\0') {
            register unsigned char  *p, q;
            i = buf->input[buf->curseg]->num - 1;
            q = buf->input[buf->curseg]->yomi[i] & 0xff;
            p = buf->rkdouble;
            while (*p != '\0') {
                if (*p == q) {
                    ret |= _Xsj3cModeChange(buf,
                        buf->input[buf->curseg]->cursegmode, ON);
                    break;
                }
                p++;
            }
        }
        ret |= KEY_TEXT_CHANGE;
        switch (buf->henkanseg) {
        case ALL:
            begin = 0;
            end = buf->segnum;
            break;
        case AFTER:
            begin = buf->curseg;
            end = buf->segnum;
            break;
        case ONE:
        default:
            begin = buf->curseg;
            end = buf->curseg + 1;
            break;
        }

        conved = buf->convedsegnum;
        if (!buf->input[begin]->num) {
            return (ret);
        }
        for (i = begin; i < end; i += value) {
            if (buf->input[i]->status == SEG_CONVED) {
                value = 1;
                continue;
            }
            if (buf->input[i]->num > INPUT_YOMI_MAX || !buf->input[i]->num) {
                Xsj3cWarning("Too long or short segment[%d].", i);
                ret |= KEY_BELL;
                value = 1;
                continue;
            }
            buf->input[i]->n_roma = 0;
            buf->input[i]->n_kana = -1;
            *buf->input[i]->str = '\0';
            buf->input[i]->sp = buf->input[i]->str;
            *buf->input[i]->oldstr = '\0';
            _Xsj3cwPStomPS(buf, kanabuf, buf->input[i]->yomi);
            value = serverIF[buf->server].func[FUNC_CONV]
                    (kanabuf, bun, knjbuf, KANJIBUFSIZ);
            if (value  <= 0) {
                if (value  < 0) 
                    Xsj3cWarning("sj3serv is down. reconnect please");
                else 
                    Xsj3cWarning("Too long segment[%d]. Could not convert.", i);
                ret |= KEY_BELL;
                value = 1;
                continue;
            }
            _Xsj3cStoreKanji(buf, bun, i, value, OFF);
            buf->convedsegnum += value;
            buf->segnum += (--value);
        }
        if (buf->henkanseg != ONE || !conved) {
            switch (buf->beginlastseg) {
            case NONE:
                buf->curseg = buf->segnum;
                break;
            case ON:
                buf->curseg = buf->segnum - 1;
                break;
            case OFF:
                buf->curseg = 0;
                break;
            default:
                buf->curseg = 0;
                break;
            }
        }
        if (buf->dispmodechange) {
            buf->dispmode = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
            ret |= KEY_MODE_CHANGE;
        }
        return ret;
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cUnConvert() [muhen/unconvert]
 *
 * <InputMode/ConvedMode> Unconvert segments.
 * <SelectMode> pop down the panel candidate(symbol/hinsi) panel and
 *   unconvert segments.
 * <DictMode> Pop down Auxpanel and unconvert segments.
 * <NoInputMode> Does nothing.
 *
 * DisplayModeChange on: Change the display mode string.  
 * MuhenkanCursorLast on: Set cursor position to bottom of segment.
 * MuhenkanCursorLast off: Set cursor position to top of segment.
 * MuhenkanSegment all: Unconvert all segments.
 * MuhenkanSegment after: Unconvert segments after current segment.
 * MuhenkanSegment one: Unconvert current segment.
 */
Xsj3cEvent
_Xsj3cUnConvert(buf)
    Xsj3cBuf            buf;
{
    Xsj3cEvent          ret = KEY_NULL;

    if (buf->convmode & SelectModeMask) {
        ret |= KEY_SELECT_ABORT;
        if (buf->selectstatus == SELECT_HINSI) {
            ret |= KEY_DICT_END;
        }
    } else if (buf->convmode & DictModeMask) {
        ret |= KEY_DICT_END;
    } else if (buf->convmode & NoInputModeMask) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
    buf->convmode = InputModeMask;
    ret |= _Xsj3cUnConvSeg(buf, buf->muhenseg, buf->muhencurlast);
    _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
    return (ret);
}

/*
 * _Xsj3cUnConvSeg()
 *  Change current segment back to non converted
 * and set converion mode to InputMode.
 */
Xsj3cEvent
_Xsj3cUnConvSeg(buf, muhenseg, muhencurlast)
    Xsj3cBuf            buf;
    Xsj3cFlag           muhenseg,   muhencurlast;
{
    register int        i,  begin,  end,    unconved = 0;
    Xsj3cEvent          ret = KEY_TEXT_CHANGE;

    buf->n_select = 0;
    if (buf->candidate) 
        Xsj3cEndCandidate(buf, OFF);
    switch (muhenseg) {
    case ALL:
        begin = 0;
        end = buf->segnum;
        break;
    case AFTER:
        begin = buf->curseg;
        end = buf->segnum;
        break;
    case ONE:
    default:
        begin = buf->curseg;
        end = buf->curseg + 1;
        break;
    }
    for (i = begin; i < end; i++) {
        if (buf->input[begin]->status == SEG_NOCONV)
            unconved++;
        else
            break;
    }
    if (unconved == (end - begin)) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
    for (i = begin + 1; i < end; i++) {
        _Xsj3cWcat(buf->input[begin]->yomi, buf->input[i]->yomi);
        buf->input[begin]->num += buf->input[i]->num;
    }
    buf->input[begin]->cur = buf->input[begin]->num;
    for (i = begin + 1; i < end; i++) {
        Xsj3cFreeSegment(buf->input[i]);
        buf->input[i] = NULL;
    }
    Xsj3cFreeSegment(buf->input[buf->segnum]);
    buf->input[buf->segnum] = NULL;
    buf->segnum -= (end - begin - 1);
    buf->curseg = begin;
    buf->convedsegnum -= (end - begin);
    if (!buf->convedsegnum) {
        if (buf->gakusyuu) 
            _Xsj3cClearDcid(buf);
        if (buf->dispmodechange) {
            buf->dispmode = buf->inputmode;
            ret |= KEY_MODE_CHANGE;
        }
    } else if (buf->dispmodechange) {
        buf->dispmode = ((buf->segnum
            && buf->convedsegnum == buf->segnum) ? MODE_KANJI :
            (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
        ret |= KEY_MODE_CHANGE;
    }
    buf->input[begin]->change = OFF;
    buf->input[begin]->status = SEG_NOCONV;
    if (!(buf->movebyseg & SEG_NOCONV))
        buf->input[begin]->edit = SEG_EDIT;
    if (muhencurlast)
        buf->input[begin]->cur = buf->input[begin]->num;
    else
        buf->input[begin]->cur = 0;
    return (ret);
}

/*
 * _Xsj3cFix() [kettei/flush/fix]
 *
 * <InputMode/ConvedMode> Fix all segments.
 *    the candidate(symbol) panel.
 * <SelectMode> Select reversed candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel and pop down the panel.
 * <DictMode> (DICT_INPUT) Decide current yomi character string.
 *            (DICT_CONFIRM) Confirm to register/eliminate word.
 *            (DICT_END) Pop down Auxpanel
 * <NoInputMode> Does nothing.
 *
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cFix(buf)
    Xsj3cBuf    buf;
{
    switch (buf->convmode) {
    case SelectModeMask:
        if (buf->selectstatus == SELECT_HINSI) {
            buf->dict->status = DICT_CONFIRM;
            if (buf->dispmodechange) {
                buf->dispmode =
                    (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
                return (KEY_SELECT_END|KEY_DICT_CHANGE|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_END|KEY_DICT_CHANGE);
        } else {
            if (buf->dispmodechange) {
                buf->dispmode = buf->inputmode;
                return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE
                    |KEY_SELECT_END|KEY_MODE_CHANGE);
            } else
                return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE|KEY_SELECT_END);
        }
    case DictModeMask:
        switch (buf->dict->status) {
        case DICT_INPUT:
            if (buf->dict->seg->num > 0) {
                if (buf->dict->seg->num > DICT_YOMI_MAX) {
                    buf->dict->status = DICT_END;
                    buf->curhinsi = -1;
                    buf->dict->value = SJ3_LONG_YOMI_STR;
                    _Xsj3cFlushDictMsg(buf);
                    return(KEY_DICT_CHANGE);
                } else {
                    if (buf->dict->mode == REG_STATE) {
                        buf->dict->status = DICT_HINSI;
                        buf->selectstatus = SELECT_HINSI;
                        buf->convmode = SelectModeMask;
                        _Xsj3cFlushDictMsg(buf);
                        if (buf->dispmodechange) {
                            buf->dispmode = MODE_HINSI;
                            return (KEY_HINSI_START|KEY_MODE_CHANGE);
                        } else 
                            return (KEY_HINSI_START);
                    } else {
                        buf->dict->status = DICT_CONFIRM;
                        _Xsj3cFlushDictMsg(buf);
                        return (KEY_DICT_CHANGE);
                    }
                }
            } else {
                buf->dict->status = DICT_END;
                buf->curhinsi = -1;
                buf->dict->value = SJ3_NO_YOMI_STR;
                _Xsj3cFlushDictMsg(buf);
                return(KEY_DICT_CHANGE);
            }
        case DICT_CONFIRM:
            buf->dict->status = DICT_END;
            return (buf->dict->mode == REG_STATE ?
                    KEY_DICT_REGISTER : KEY_DICT_CLEAR);
        case DICT_END:
            if (buf->dispmodechange) {
                buf->dispmode
                    = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                return(KEY_DICT_END|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
            } else 
                return(KEY_DICT_END|KEY_TEXT_CHANGE);
        default:
#ifdef THROUGH_CONT
            return (KEY_BELL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_BELL);
            else 
                return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
        }
    case NoInputModeMask:
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    case InputModeMask:
    case ConvedModeMask:
    default:
        if (buf->dispmodechange) {
            buf->dispmode = buf->inputmode;
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
        } else 
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE);
    }
}

/*
 * _Xsj3cReturn() [return]
 *
 * <NoInputMode/InputMode> Put the carriage return after current position
 *   and fix all segments.
 * <ConvedMode> Put the carriage return after current segment
 *   and fix all segments.
 * <SelectMode> Select reversed candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel and pop down the panel.
 * <DictMode> (DICT_INPUT) Decide current yomi character string.
 *            (DICT_CONFIRM) Confirm to register/eliminate word.
 *            (DICT_END) Pop down Auxpanel
 *
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cReturn(buf)
    Xsj3cBuf        buf;
{
    unsigned char   ch[2];
    int             change_pos;
    register int    i;

    switch (buf->convmode) {
    case NoInputModeMask:
        buf->convmode = InputModeMask;
    case InputModeMask:
        if (buf->segnum == buf->curseg)
            buf->segnum++;
        ch[0] = '\r';
        ch[1] = '\0';
        change_pos = buf->input[buf->curseg]->cur;
        _Xsj3cInsertChar(buf, buf->input[buf->curseg], ch, 1);
        _Xsj3cStoreYomi(buf, buf->input[buf->curseg], change_pos);
        if (buf->dispmodechange) {
            buf->dispmode = buf->inputmode;
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
        } else
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE);
    case ConvedModeMask:
        i = buf->input[buf->curseg]->dnum;
        buf->input[buf->curseg]->disp[i++] = '\n';
        buf->input[buf->curseg]->disp[i] = '\0';
        buf->input[buf->curseg]->dnum++;
        if (buf->dispmodechange) {
            buf->dispmode = buf->inputmode;
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
        } else
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE);
    case DictModeMask:
        switch (buf->dict->status) {
        case DICT_INPUT:
            if (buf->dict->seg->num) {
                if (buf->dict->seg->num > DICT_YOMI_MAX) {
                    buf->dict->status = DICT_END;
                    buf->curhinsi = -1;
                    buf->dict->value = SJ3_LONG_YOMI_STR;
                    _Xsj3cFlushDictMsg(buf);
                    return(KEY_DICT_CHANGE);
                } else {
                    if (buf->dict->mode == REG_STATE) {
                        buf->dict->status = DICT_HINSI;
                        buf->selectstatus = SELECT_HINSI;
                        buf->convmode = SelectModeMask;
                        _Xsj3cFlushDictMsg(buf);
                        if (buf->dispmodechange) {
                            buf->dispmode = MODE_HINSI;
                            return (KEY_HINSI_START|KEY_MODE_CHANGE);
                        } else 
                            return (KEY_HINSI_START);
                    } else {
                        buf->dict->status = DICT_CONFIRM;
                        _Xsj3cFlushDictMsg(buf);
                        return (KEY_DICT_CHANGE);
                    }
                }
            } else {
                buf->dict->status = DICT_END;
                buf->curhinsi = -1;
                buf->dict->value = SJ3_NO_YOMI_STR;
                _Xsj3cFlushDictMsg(buf);
                return(KEY_DICT_CHANGE);
            }
        case DICT_CONFIRM:
            buf->dict->status = DICT_END;
            return (buf->dict->mode == REG_STATE ?
                    KEY_DICT_REGISTER : KEY_DICT_CLEAR);
        case DICT_END:
            if (buf->dispmodechange) {
                buf->dispmode
                    = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                return(KEY_DICT_END|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
            } else 
                return(KEY_DICT_END|KEY_TEXT_CHANGE);
        default:
#ifdef THROUGH_CONT
            return (KEY_BELL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_BELL);
            else 
                return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
        }
    case SelectModeMask:
        if (buf->selectstatus == SELECT_HINSI) {
            buf->dict->status = DICT_CONFIRM;
            if (buf->dispmodechange) {
                buf->dispmode =
                    (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
                return (KEY_SELECT_END|KEY_DICT_CHANGE|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_END|KEY_DICT_CHANGE);
        } else {
            if (buf->dispmodechange) {
                buf->dispmode = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                return (KEY_SELECT_END|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_END);
        }
    default:
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cModeHAlpha() [halpha]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> Change character mode
 *   to "hankaku-ascii".
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment
 *   to "hankaku-ascii".
 * ModeConversion off: Change character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeHAlpha(buf)
    Xsj3cBuf    buf;
{
    Xsj3cFlag   conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_HALPHA] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_HALPHA]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    return (_Xsj3cModeChange(buf, MODE_HALPHA, conv));
}

/*
 * _Xsj3cModeZAlpha()  [zalpha]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> Change character mode
 *   to "zenkaku-ascii".
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment
 *   to "zenkaku-ascii".
 * ModeConversion off: Change character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeZAlpha(buf)
    Xsj3cBuf    buf;
{
    Xsj3cFlag   conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_ZALPHA] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_ZALPHA]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    return (_Xsj3cModeChange(buf, MODE_ZALPHA, conv));
}

/*
 * _Xsj3cModeHKata() [hkatakana]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> Change character mode
 *   to "hankaku-katakana".
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment
 *   to "hankaku-katakana".
 * ModeConversion off: Change character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeHKata(buf) 
    Xsj3cBuf    buf;
{
    Xsj3cFlag   conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_HKATA] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_HKATA]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    return (_Xsj3cModeChange(buf, MODE_HKATA, conv));
}

/*
 * _Xsj3cModeZKata() [zkatakana]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> Change character mode
 *   to "zenkaku-katakana".
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment
 *   to "zenkaku-katakana".
 * ModeConversion off: Change character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeZKata(buf)
    Xsj3cBuf    buf;
{
    Xsj3cFlag   conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_ZKATA] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_ZKATA]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    return (_Xsj3cModeChange(buf, MODE_ZKATA, conv));
}


/*
 * _Xsj3cModeHira() [hiragana]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> Change character mode
 *   to "hiragana".
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment to "hiragana".
 * ModeConversion off: Change character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeHira(buf)
    Xsj3cBuf    buf;
{
    Xsj3cFlag   conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_HIRA] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_HIRA]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    return (_Xsj3cModeChange(buf, MODE_HIRA, conv));
}

/*
 * _Xsj3cToHAlpha() [tohalpha]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert character mode
 *   to "hankaku-ascii".
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToHAlpha(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_HALPHA, ON));
}

/*
 * _Xsj3cToZAlpha()  [tozalpha]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert character mode
 *   to "zenkaku-ascii".
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToZAlpha(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_ZALPHA, ON));
}

/*
 * _Xsj3cToHKata() [tohkatakana]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert character mode
 *   to "hankaku-katakana".
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToHKata(buf) 
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_HKATA, ON));
}

/*
 * _Xsj3cToZKata() [tozkatakana]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert character mode
 *   to "zenkaku-katakana".
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToZKata(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_ZKATA, ON));
}


/*
 * _Xsj3cToHira() [tohiragana]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert character mode
 *   to "hiragana".
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToHira(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_HIRA, ON));
}

/*
 * _Xsj3cToZenkaku() [zenkaku]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert current segment
 *   character mode to zenkaku.
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cZenkaku(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_ZENKAKU, ON));
}

/*
 * _Xsj3cToHankaku() [zenkaku]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert current segment
 *   character mode to hankaku.
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cHankaku(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_HANKAKU, ON));
}

/*
 * _Xsj3cToUpper() [toupper]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert current segment
 *   character mode to upper case.
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToUpper(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_UPPER, ON));
}

/*
 * _Xsj3cToLower() [tolower]
 *
 * <NoInputTo/InputMode/ConvedMode/DictMode> Convert current segment
 *   character mode to lower case.
 * <SelectTo> Ring bell.
 */
Xsj3cEvent
_Xsj3cToLower(buf)
    Xsj3cBuf    buf;
{
    return (_Xsj3cModeChange(buf, MODE_LOWER, ON));
}

/*
 * _Xsj3cModeSJIS() [sjis]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> If now input
 *   code is not "Shift-JIS", change to Shift-JIS code input mode,
 *   else change to initialized input mode.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cModeSJIS(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask) 
        return (KEY_BELL);
    if (buf->inputmode != MODE_SJIS) {
        buf->dispmode = buf->inputmode = MODE_SJIS;
        return (_Xsj3cModeClear(buf));
    } else {
        buf->dispmode = buf->inputmode = buf->inmoderot[0];
        return (_Xsj3cModeClear(buf));
    }
}

/*
 * _Xsj3cModeEUC() [euc]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> If now input code is not "EUC",
 *   change to EUC code input mode, else change to initialized input mode.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cModeEUC(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask) 
        return (KEY_BELL);
    if (buf->inputmode != MODE_EUC) {
        buf->dispmode = buf->inputmode = MODE_EUC;
        return (_Xsj3cModeClear(buf));
    } else {
        buf->dispmode = buf->inputmode = buf->inmoderot[0];
        return (_Xsj3cModeClear(buf));
    }
}

/*
 * _Xsj3cModeJIS() [jis]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> If now input code is not "JIS",
 *   change to JIS code input mode, else change to initialized input mode.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cModeJIS(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask) 
        return (KEY_BELL);
    if (buf->inputmode != MODE_JIS) {
        buf->dispmode = buf->inputmode = MODE_JIS;
        return (_Xsj3cModeClear(buf));
    } else {
        buf->dispmode = buf->inputmode = buf->inmoderot[0];
        return (_Xsj3cModeClear(buf));
    }
}

/*
 * _Xsj3cModeKuten() [kuten]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> If now input code is not "kuten",
 *   change to kuten code input mode, else change to initialized input mode.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cModeKuten(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask) 
        return (KEY_BELL);
    if (buf->inputmode != MODE_KUTEN) {
        buf->dispmode = buf->inputmode = MODE_KUTEN;
        return (_Xsj3cModeClear(buf));
    } else {
        buf->dispmode = buf->inputmode = buf->inmoderot[0];
        return (_Xsj3cModeClear(buf));
    }
}

/*
 * _Xsj3cCodeRollDown() [code]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode> Rotate down input code.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cCodeRollDown(buf)
    Xsj3cBuf        buf;
{
    register int    i;

    if (buf->convmode & ~SelectModeMask) {
        i = 0;
        while (i < buf->coderotnum) {
            if (buf->inputmode == buf->defcode[i++]) {
                buf->dispmode = buf->inputmode = buf->defcode[i];
                break;
            }
        }
        if (i >= buf->coderotnum) {
            buf->dispmode = buf->inputmode = buf->defcode[0];
        }
        return (_Xsj3cModeClear(buf));
    } else {
        return (KEY_BELL);
    }
}

/*
 * _Xsj3cModeRollDown() [toggle/modedown]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode>
 *   Rotate down character mode to next mode.
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment to next.
 * ModeConversion off: Rotate character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeRollDown(buf)
    Xsj3cBuf        buf;
{
    register int    i;
    Xsj3csMode      premode,    postmode;
    Xsj3cFlag       conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_ROLLDOWN] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_ROLLDOWN]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    if (conv) {
        return (_Xsj3cNextMode(buf));
    } else {
        premode = buf->inputmode;
        i = 0;
        while (i < buf->inmoderotnum) {
            if (premode == buf->inmoderot[i++]) {
                postmode = buf->inmoderot[i];
                break;
            }
        }
        if (i >= buf->inmoderotnum)
            postmode = buf->inmoderot[0];
        return(_Xsj3cModeChange(buf, postmode, OFF));
    }
}

/*
 * _Xsj3cModeRollUp() [toggleback/modeup]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode>
 *   Rotate up character mode to previous mode.
 * <SelectMode> Ring bell.
 *
 * ModeConversion on: Convert character mode of current segment to next.
 * ModeConversion off: Rotate character mode of input mode.
 */
Xsj3cEvent
_Xsj3cModeRollUp(buf)
    Xsj3cBuf        buf;
{
    register int    i;
    Xsj3csMode      premode,    postmode;
    Xsj3cFlag       conv;

    if (!buf->segnum) {
        if (buf->modeconv[MODE_ROLLDOWN] >> 2) {
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        } else
            conv = OFF;
    } else if (buf->input[buf->curseg] &&
            buf->input[buf->curseg]->status & buf->modeconv[MODE_ROLLDOWN]) {
        conv = ON;
    } else {
        conv = OFF;
    }
    if (conv) {
        return (_Xsj3cPrevMode(buf));
    } else {
        premode = buf->inputmode;
        i = buf->inmoderotnum - 1;
        while (i < buf->inmoderotnum) {
            if (premode == buf->inmoderot[i--]) {
                postmode = buf->inmoderot[i];
                break;
            }
        }
        if (i < 0)
            postmode = buf->inmoderot[buf->inmoderotnum - 1];
        return(_Xsj3cModeChange(buf, postmode, OFF));
    }
}

/*
 * _Xsj3cNextMode() [nextmode]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode>
 *   Change current segment character mode to next mode.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cNextMode(buf)
    Xsj3cBuf        buf;
{
    register int    i;
    Xsj3csMode      premode,    postmode,   aftermode = MODE_HIRA;
    Xsj3cEvent      ret;

    if (buf->convmode & NoInputModeMask) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
    premode = buf->input[buf->curseg]->cursegmode;
    i = 0;
    while (i < buf->outmoderotnum) {
        if (premode == buf->outmoderot[i++]) {
            postmode = buf->outmoderot[i];
            break;
        }
    }
    if (i >= buf->outmoderotnum) {
        aftermode = premode;
        premode = _Xsj3cCheckMode(buf, buf->input[buf->curseg]);
        i = 0;
        while (i < buf->outmoderotnum) {
            if (premode == buf->outmoderot[i++]) {
                postmode = buf->outmoderot[i];
                break;
            }
        }
        if (i >= buf->outmoderotnum)
            postmode = buf->outmoderot[0];
    }
    ret = _Xsj3cModeChange(buf, postmode, ON);
    if ((aftermode == MODE_UPPER || aftermode == MODE_LOWER)
        && (postmode == MODE_ZALPHA || postmode == MODE_HALPHA)) {
        ret |= _Xsj3cModeChange(buf, aftermode, ON);
    }
    return (ret);
}

/*
 * _Xsj3cPrevMode() [prevmode]
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode>
 *   Change current segment character mode to previous mode.
 * <SelectMode> Ring bell.
 */
Xsj3cEvent
_Xsj3cPrevMode(buf)
    Xsj3cBuf        buf;
{
    register int    i;
    Xsj3csMode      premode,    postmode,   aftermode = MODE_HIRA;
    Xsj3cEvent      ret;

    if (buf->convmode & NoInputModeMask) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
    premode = buf->input[buf->curseg]->cursegmode;
    i = buf->outmoderotnum - 1;
    while (i >= 0) {
        if (premode == buf->outmoderot[i--]) {
            postmode = buf->outmoderot[i];
            break;
        }
    }
    if (i < 0) {
        aftermode = premode;
        premode = _Xsj3cCheckMode(buf, buf->input[buf->curseg]);
        i = buf->outmoderotnum - 1;
        while (i >= 0) {
            if (premode == buf->outmoderot[i--]) {
                postmode = buf->outmoderot[i];
                break;
            }
        }
        if (i < 0)
            postmode = buf->outmoderot[buf->outmoderotnum - 1];
    }
    ret = _Xsj3cModeChange(buf, postmode, ON);
    if ((aftermode == MODE_UPPER || aftermode == MODE_LOWER)
        && (postmode == MODE_ZALPHA || postmode == MODE_HALPHA)) {
        ret |= _Xsj3cModeChange(buf, aftermode, ON);
    }
    return(ret);
}

/*
 * _Xsj3cModeToggle() [muhenkan]
 *
 * Emulating of sj2/sj3/sjx's muhenkan stroke.
 * <NoInputMode/InputMode> Change input character mode to "MuhenkanMode".
 * <ConvedMode/DictMode> Converts current segment character input mode first to
 *   hiragana, second to zenkaku-katakana , third to "MuhenkanInEdit".
 * <SelectMode> Does nothing.
 *
 * MuhenkanMode mode: Mode of toggling in InputMode after input chacter mode
 *   is changed to hiragana.
 * MuhenkanInEdit mode: Mode of toggling in ConvedMode after segment is
 *   converted to hiragana and zenkaku-katakana.
 * MuhenkanToggle off: Stop toggling in InputMode.
 */
Xsj3cEvent
_Xsj3cModeToggle(buf)
    Xsj3cBuf        buf;
{
    register int    i;
    register wchar  (*conv)();
    unsigned char  *mbs;
    Xsj3cEvent      ret;
    Xsj3cSeg        seg = buf->input[buf->curseg];

    if (buf->convmode & ~(SelectModeMask|DictModeMask)) {
        if (buf->convmode & ConvedModeMask ||((buf->convmode & InputModeMask)
                && (seg->edit & SEG_NOEDIT))) {
            if ((mbs = (unsigned char *)malloc(seg->size * 2 * sizeof(wchar)))
                    == NULL)
                Xsj3cError("Cannot allocate for mode conversion buffer");

            switch (seg->cursegmode) {
            case MODE_HIRA:
                if (seg->num != seg->dnum) {
                    ret = _Xsj3cModeChange(buf, MODE_HIRA, ON);
                } else {
                    if (conv =
                        CodeConvFunc[serverIF[buf->server].lang][out_lang]) {
                        register wchar  w1, w2;
                        for (i = 0;i < seg->dnum; i++) {
                            w1 = seg->yomi[i];
                            if (iskan1(w1 >> 8, serverIF[buf->server].lang) &&
                                iskan2(w1 & 0xff, serverIF[buf->server].lang))
                                w2 = conv(w1);
                            else 
                                w2 = w1;
                            if (w2 != seg->disp[i])
                                break;
                        }
                    } else {
                        for (i = 0;i < seg->dnum;i++) {
                            if (seg->disp[i] != seg->yomi[i])
                                break;
                        }
                    }
                    if (i < seg->dnum)
                        ret = _Xsj3cModeChange(buf, MODE_HIRA, ON);
                    else {
                        ret = _Xsj3cModeChange(buf, MODE_ZKATA, ON);
                    }
                }
                break;
            case MODE_ZKATA:
                ret = _Xsj3cModeChange(buf, buf->togglemode, ON);
                break;
            default:
                ret = _Xsj3cModeChange(buf, MODE_HIRA, ON);
                break;
            }
            free(mbs);
            return(ret);
        } else {
            if (buf->inputmode == MODE_HIRA)
                return (_Xsj3cModeChange(buf, buf->muhenmode, OFF));
            else if (buf->dotoggle)
                return (_Xsj3cModeChange(buf, MODE_HIRA, OFF));
            else {
#ifdef THROUGH_CONT
                return (KEY_NULL);
#else /* THROUGH_CONT */
                if (buf->cntrlsame)
                    return (KEY_NULL);
                else 
                    return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
            }
        }
    } else {
        return (KEY_BELL);
    }
}

/*
 * _Xsj3cForward() [right/forward]
 *
 * <InputMode/ConvedMode> Move to next segment or next character position.
 * <DictMode> Move reversed segment to next segment if not expanded.
 * <SelectMode> Move to right candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel.
 * <NoInputMode> Does nothing.
 *
 * MovebySegment on: Move by segment.
 * MovebySegment off: Move by character.
 * BeginConversionLast none: Allow to move out of segments. 
 * MoveSegmentLoop on: Loop back to the first segment.
 */
Xsj3cEvent
_Xsj3cForward(buf)
    Xsj3cBuf    buf;
{
    Xsj3cEvent  ret = KEY_TEXT_CHANGE;

    switch (buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, ON);
    case InputModeMask:
        buf->input[buf->curseg]->n_roma = 0;
        buf->input[buf->curseg]->n_kana = -1;
        *buf->input[buf->curseg]->oldstr = '\0';
        *buf->input[buf->curseg]->str = '\0';
        buf->input[buf->curseg]->sp = buf->input[buf->curseg]->str;
        if (buf->input[buf->curseg]->status & buf->movebyseg
            && (buf->input[buf->curseg]->edit & SEG_NOEDIT)) {
            if (buf->beginlastseg == NONE) {
                if (buf->curseg < buf->segnum)
                    buf->curseg++;
                else if (buf->moveloop)
                    buf->curseg = 0;
            } else {
                if (buf->curseg < buf->segnum - 1)
                    buf->curseg++;
                else if (buf->moveloop)
                    buf->curseg = 0;
            }
        } else {
            if (buf->curseg < buf->segnum
                    && buf->input[buf->curseg]->status == SEG_CONVED)
                ret |= _Xsj3cUnConvSeg(buf, ONE, OFF);
                _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
            if (buf->input[buf->curseg]->cur < buf->input[buf->curseg]->num) {
                buf->input[buf->curseg]->cur++;
            } else {
                if (buf->beginlastseg == NONE) {
                    if (buf->curseg < buf->segnum) {
                        buf->curseg++;
                        if (buf->curseg < buf->segnum)
                            buf->input[buf->curseg]->cur = 0;
                    } else if (buf->moveloop) {
                        buf->curseg = 0;
                        buf->input[buf->curseg]->cur = 0;
                    }
                } else {
                    if (buf->curseg < buf->segnum - 1) {
                        buf->curseg++;
                        buf->input[buf->curseg]->cur = 0;
                    } else if (buf->moveloop) {
                        buf->curseg = 0;
                        buf->input[buf->curseg]->cur = 0;
                    }
                }
            }
        }
        return ret;
    case DictModeMask:
        if (buf->dict->n_dict) {
            return (KEY_BELL);
        } else {
            if (buf->curseg < buf->segnum - 1 )
                buf->curseg++;
            else 
                buf->curseg = buf->segnum - 1;
            return (ret);
        }
    case SelectModeMask:
        return (KEY_SELECT_RIGHT);
    case NoInputModeMask:
    default:
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cBackward() [left/backward]
 *
 * <InputMode/ConvedMode> Move to previous segment or
 *   previous character position.
 * <DictMode> Move reversed segment to previous segment if not expanded.
 * <SelectMode> Move to left candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel.
 * <NoInputMode> Does nothing.
 *
 * MovebySegment on: Move by segment.
 * MovebySegment off: Move by character.
 * MoveSegmentLoop on: Loop back to the last segment.
 */
Xsj3cEvent
_Xsj3cBackward(buf)
    Xsj3cBuf    buf;
{
    Xsj3cEvent  ret = KEY_TEXT_CHANGE;

    switch (buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, ON);
    case InputModeMask:
        if (buf->curseg < buf->segnum) {
            buf->input[buf->curseg]->n_roma = 0;
            buf->input[buf->curseg]->n_kana = -1;
            *buf->input[buf->curseg]->oldstr = '\0';
            *buf->input[buf->curseg]->str = '\0';
            buf->input[buf->curseg]->sp = buf->input[buf->curseg]->str;
            if (buf->input[buf->curseg]->status & buf->movebyseg
                    && (buf->input[buf->curseg]->edit & SEG_NOEDIT)) {
                if (buf->curseg > 0)
                    buf->curseg--;
                else if (buf->moveloop) {
                    if (buf->beginlastseg == NONE)
                        buf->curseg = buf->segnum;
                    else 
                        buf->curseg = buf->segnum - 1;
                }
            } else {
                if (buf->curseg < buf->segnum
                        && buf->input[buf->curseg]->status == SEG_CONVED)
                    ret |= _Xsj3cUnConvSeg(buf, ONE, ON);
                    _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
                if (buf->input[buf->curseg]->cur > 0) {
                    buf->input[buf->curseg]->cur--;
                } else {
                    if (buf->curseg > 0) {
                        buf->curseg--;
                        buf->input[buf->curseg]->cur
                                = buf->input[buf->curseg]->num;
                    } else if (buf->moveloop) {
                        if (buf->beginlastseg == NONE)
                            buf->curseg = buf->segnum;
                        else {
                            buf->curseg = buf->segnum - 1;
                            buf->input[buf->curseg]->cur
                                    = buf->input[buf->curseg]->num;
                        }
                    }
                }
            }
            return (ret);
        } else if (buf->curseg > 0) {
            buf->curseg--;
            return (ret);
        } else {
            return (KEY_NULL);
        }
    case DictModeMask:
        if (buf->dict->n_dict) {
            return (KEY_BELL);
        } else {
            if (buf->curseg > 0)
                buf->curseg--;
            else 
                buf->curseg = 0;
            return (ret);
        }
    case SelectModeMask:
        return (KEY_SELECT_LEFT);
    case NoInputModeMask:
    default:
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cTop() [top]
 *
 * <InputMode/ConvedMode> Move to the first segment or top of current segment.
 * <DictMode> Move reversed segment to the first segment if not expanded.
 * <SelectMode> Move to first candidate(symbol/hinsi)
 *   in popuped candidate(symbol/hinsi) panel.
 * <NoInputMode> Does nothing.
 *
 * JumpbySegment on: Move by segment.
 * JumpbySegment off: Move by character.
 */
Xsj3cEvent
_Xsj3cTop(buf)
    Xsj3cBuf    buf;
{
    switch (buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, ON);
    case InputModeMask:
        if (buf->curseg < buf->segnum) {
            buf->input[buf->curseg]->n_roma = 0;
            buf->input[buf->curseg]->n_kana = -1;
            *buf->input[buf->curseg]->oldstr = '\0';
            *buf->input[buf->curseg]->str = '\0';
            buf->input[buf->curseg]->sp = buf->input[buf->curseg]->str;
            if (buf->input[buf->curseg]->status & buf->jumpbyseg
                    && (buf->input[buf->curseg]->edit & SEG_NOEDIT)) {
                buf->curseg = 0;
            } else if (buf->curseg < buf->segnum) {
                buf->input[buf->curseg]->cur = 0;
            }
            return KEY_TEXT_CHANGE;
        } else if (buf->segnum > 0) {
            buf->curseg = 0;
            return KEY_TEXT_CHANGE;
        } else {
            return KEY_NULL;
        }
    case DictModeMask:
        if (buf->dict->n_dict) {
            return (KEY_BELL);
        } else {
            buf->curseg = 0;
            return (KEY_TEXT_CHANGE);
        }
    case SelectModeMask:
        return (KEY_SELECT_LEFTMOST);
    case NoInputModeMask:
    default:
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cEnd() [end]
 *
 * <InputMode/ConvedMode> Move to the last segment or top of current segment.
 * <DictMode> Move reversed segment to the last segment if not expanded.
 * <SelectMode> Move to last candidate(symbol/hinsi)
 *   in popuped candidate(symbol/hinsi) panel.
 * <NoInputMode> Does nothing.
 *
 * JumpbySegment on: Move by segment.
 * JumpbySegment off: Move by character.
 * BeginConversionLast none: Allow to move out of segments. 
 */
Xsj3cEvent
_Xsj3cEnd(buf)
    Xsj3cBuf    buf;
{
    switch (buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, ON);
    case InputModeMask:
        if (buf->curseg >= buf->segnum)
            return KEY_NULL;
        buf->input[buf->curseg]->n_roma = 0;
        buf->input[buf->curseg]->n_kana = -1;
        *buf->input[buf->curseg]->oldstr = '\0';
        *buf->input[buf->curseg]->str = '\0';
        buf->input[buf->curseg]->sp = buf->input[buf->curseg]->str;
        if (buf->input[buf->curseg]->status & buf->jumpbyseg
                    && (buf->input[buf->curseg]->edit & SEG_NOEDIT)) {
            if (buf->beginlastseg == NONE)
                buf->curseg = buf->segnum;
            else 
                buf->curseg = buf->segnum - 1;
        } else {
            buf->input[buf->curseg]->cur = buf->input[buf->curseg]->num;
        }
        return KEY_TEXT_CHANGE;
    case DictModeMask:
        if (buf->dict->n_dict) {
            return (KEY_BELL);
        } else {
            buf->curseg = buf->segnum - 1;
            return (KEY_TEXT_CHANGE);
        }
    case SelectModeMask:
        return (KEY_SELECT_RIGHTMOST);
    case NoInputModeMask:
    default:
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cUp() [up]
 *
 * <SelectMode> Move to above candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel.
 * <NoInputMode/InputMode/ConvedMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cUp(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask)
        return (KEY_SELECT_UP);
    else
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
}

/*
 * _Xsj3cDown() [down]
 *
 * <SelectMode> Move to below candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel.
 * <NoInputMode/InputMode/ConvedMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cDown(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask)
        return (KEY_SELECT_DOWN);
    else
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
}

/*
 * _Xsj3cFirst() [first]
 *
 * <ConvedMode> Set top candidate the current segment.
 * <SelectMode> Move to first candidate(symbol/hinsi)
 *   in all candidate(symbol/hinsi) pages.
 * <NoInputMode/InputMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cFirst(buf)
    Xsj3cBuf    buf;
{
    int         changed,    flush;

    if (buf->convmode & ConvedModeMask) {
        if (!buf->candidate)
            if ((buf->candidate = _Xsj3cCandidateInit(buf)) == NULL)
                return (KEY_NULL);
        Xsj3cSetCandidate(buf, 0, &changed, &flush);
        return KEY_TEXT_CHANGE;
    } else if (buf->convmode & SelectModeMask) {
        return (KEY_SELECT_FIRST);
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cLast() [last]
 *
 * <ConvedMode> Set bottom candidate the current segment.
 * <SelectMode> Move to last candidate(symbol/hinsi)
 *   in all candidate(symbol/hinsi) pages.
 * <NoInputMode/InputMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cLast(buf)
    Xsj3cBuf    buf;
{
    int         changed,    flush;

    if (buf->convmode & ConvedModeMask) {
        if (!buf->candidate)
            if ((buf->candidate = _Xsj3cCandidateInit(buf)) == NULL)
                return (KEY_NULL);
        Xsj3cSetCandidate(buf, buf->candnum - 1, &changed, &flush);
        return (KEY_TEXT_CHANGE);
    } else if (buf->convmode & SelectModeMask) {
        return (KEY_SELECT_LAST);
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cNextPage() [nextp]
 * <SelectMode> Change contents of the candidate(symbol/hinsi) panel
 *   to the next page.
 * <NoInputMode/InputMode/ConvedMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cNextPage(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask)
        return (KEY_SELECT_NEXTP);
    else
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
}

/*
 * _Xsj3cPrevPage() [prevp]
 * <SelectMode> Change contents of the candidate(symbol/hinsi) panel
 *   to the previous page.
 * <NoInputMode/InputMode/ConvedMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cPrevPage(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask)
        return (KEY_SELECT_PREVP);
    else
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
}

/*
 * _Xsj3cNext() [next]
 *
 * <ConvedMode> Set next candidate the current segment.
 * <SelectMode> Move to right(next) candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel.
 * <NoInputMode/InputMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cNext(buf)
    Xsj3cBuf    buf;
{
    int         changed,    flush;

    if (buf->convmode & ConvedModeMask) {
        if (!buf->candidate)
            if ((buf->candidate = _Xsj3cCandidateInit(buf)) == NULL)
                return (KEY_NULL);
        if (buf->curcand == buf->candnum - 1)
            Xsj3cSetCandidate(buf, 0, &changed, &flush);
        else 
            Xsj3cSetCandidate(buf, buf->curcand + 1, &changed, &flush);
        return KEY_TEXT_CHANGE;
    } else if (buf->convmode & SelectModeMask) {
        return (KEY_SELECT_RIGHT);
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cPrev() [prev]
 *
 * <ConvedMode> Set previous candidate the current segment.
 * <SelectMode> Move to left(previous) candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel.
 * <NoInputMode/InputMode/DictMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cPrev(buf)
    Xsj3cBuf    buf;
{
    int         changed,    flush;

    if (buf->convmode & ConvedModeMask) {
        if (!buf->candidate)
            if ((buf->candidate = _Xsj3cCandidateInit(buf)) == NULL)
                return (KEY_NULL);
        if (!buf->curcand)
            Xsj3cSetCandidate(buf, buf->candnum - 1, &changed, &flush);
        else 
            Xsj3cSetCandidate(buf, buf->curcand - 1, &changed, &flush);
        return KEY_TEXT_CHANGE;
    } else if (buf->convmode & SelectModeMask) {
        return (KEY_SELECT_LEFT);
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cSelect() [select]
 *
 * <SelectMode> Select reversed candidate(symbol/hinsi)
 *   in the candidate(symbol/hinsi) panel and pop down the panel.
 * <NoInputMode/InputMode/ConvedMode/DictMode> Rings bell.
 *  Select now selected candidate(symbol) in the candidate(symbol) panel.
 */
Xsj3cEvent
_Xsj3cSelect(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & SelectModeMask) {
        if (buf->selectstatus == SELECT_HINSI) {
            buf->dict->status = DICT_CONFIRM;
            if (buf->dispmodechange) {
                buf->dispmode =
                    (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
                return (KEY_SELECT_END|KEY_DICT_CHANGE|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_END|KEY_DICT_CHANGE);
        } else {
            if (buf->dispmodechange) {
                buf->dispmode = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                return (KEY_SELECT_END|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_END);
        }
    } else if (buf->convmode & DictModeMask) {
        if (buf->dict->status == DICT_CONFIRM) {
            buf->dict->status = DICT_END;
            _Xsj3cFlushDictMsg(buf);
            if (buf->dict->mode == REG_STATE)
                return(KEY_DICT_REGISTER);
            else
                return(KEY_DICT_CLEAR);
        } else {
            return (KEY_BELL);
        }
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cCancel() [cancel]
 *
 * <InputMode/ConvedMode> Clear all segments.
 * <SelectMode> Pop down candidate(symbol/hinsi) panel.
 * <DictMode> (DICT_INPUT) Pop down Auxpanel.
 *            (DICT_CONFIRM/DICT_END) Return back to DICT_INPUT status.
 * <NoInputMode> Does nothing.
 *
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cCancel(buf)
    Xsj3cBuf    buf;
{
    int         i;

    switch (buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
    case InputModeMask:
        if (buf->gakusyuu)
            _Xsj3cClearDcid(buf);
        buf->convedsegnum = 0;
        for (i = 1; i < buf->segnum + 1; i++) {
            Xsj3cFreeSegment(buf->input[i]);
            buf->input[i] = NULL;
        }
        if (buf->input[0])
            Xsj3cClearSegment(buf, buf->input[0]);
        buf->convedsegnum = 0;
        buf->segnum = 0;
        buf->curseg = 0;
        if (buf->dispmodechange) {
            buf->dispmode = buf->inputmode;
            return (KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
        } else
            return (KEY_TEXT_CHANGE);
    case SelectModeMask:
        if (buf->selectstatus == SELECT_HINSI) {
            buf->dict->status = DICT_INPUT;
            _Xsj3cFlushDictMsg(buf);
            if (buf->dispmodechange) {
                buf->dispmode = (buf->dict->mode == REG_STATE ?
                    MODE_TOROKU : MODE_SYOUKYO);
                return (KEY_SELECT_ABORT|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_ABORT);
        } else {
            if (buf->dispmodechange) {
                buf->dispmode = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                return (KEY_SELECT_ABORT|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_ABORT);
        }
    case DictModeMask:
        if (buf->dict->status == DICT_CONFIRM
                || buf->dict->status == DICT_END) {
            buf->dict->status = DICT_INPUT;
            _Xsj3cFlushDictMsg(buf);
            return(KEY_DICT_CHANGE);
        } else {
            if (buf->dispmodechange) {
                buf->dispmode = (buf->convedsegnum == buf->segnum ? MODE_KANJI:
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                return (KEY_DICT_END|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
            } else {
                return (KEY_DICT_END|KEY_TEXT_CHANGE);
            }
        }
    case NoInputModeMask:
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    default:
        return (KEY_TEXT_CHANGE);
    }
}

/*
 * _Xsj3cExpand() [kakucyou/expand]
 *
 * <InputMode/ConvedMode> Expand the current segment by getting in
 *   the first character of the next segment.
 * <DictMode> Increase the reversed segments by adding the next segment.
 * <NoInputMode/SelectMode> Rings bell.  
 *   
 * ExpandModeConversion on: Change the expanded segment's character mode.
 * ExpandKanjiConversion on: Do kana-kanji conversion for current
 *   and next segment.
 */
Xsj3cEvent
_Xsj3cExpand(buf)
    Xsj3cBuf                buf;
{
    int                     nextseg = buf->curseg + 1,  value = 0;
    Xsj3cFlag               conv = 0,   modematch = 0;
    register int            i;
    unsigned char           tmp[YBUFSIZ];
    unsigned char          *kanabuf;
    unsigned char           knjbuf[KANJIBUFSIZ];
    SJ3_BUNSETU             bun[BUNBUFSIZ];

    if (buf->convmode & DictModeMask) {
        return (_Xsj3cExpandNoConv(buf));
    } else if ((buf->convmode & SelectModeMask) || nextseg >= buf->segnum
            || !buf->input[nextseg]->num) {
        /* 現文節が最後の文節の時は拡張できない。 */
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    } else if (buf->convmode & ConvedModeMask) {
        buf->n_select = 0;
        if (buf->candidate)
            Xsj3cEndCandidate(buf, OFF);
    } else if (buf->convmode & ~InputModeMask) {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }

    /* temporary のバッファを作成する   */
    if ((kanabuf = (unsigned char *)
            malloc(buf->input[buf->curseg]->size * sizeof(wchar))) == NULL)
        Xsj3cError("Cannot allocate for temporary buffer");

    /* 次文節の頭の１文字を減じ、一時バッファにコピーする   */
    buf->input[nextseg]->cur = 1;
    _Xsj3cExtractChar(buf, buf->input[nextseg], tmp, 1);
    if (buf->input[nextseg]->status & SEG_CONVED)
        buf->convedsegnum--;

    if (buf->input[nextseg]->cursegmode == buf->input[buf->curseg]->cursegmode)
        modematch++;
    if (!buf->input[nextseg]->num) {

        /* 次文節が１文字しかなかった場合、文節数を１つ減じ */
        /*  更に、次文節の領域を解放する。                  */
        Xsj3cFreeSegment(buf->input[buf->segnum]);
        buf->input[buf->segnum] = NULL;
        buf->segnum--;
        Xsj3cFreeSegment(buf->input[nextseg]);
        buf->input[nextseg] = NULL;
        if (nextseg < buf->segnum) {
            for ( i = nextseg; i < buf->segnum; i++) {
                buf->input[i] = buf->input[i + 1];
            }
        } 
        buf->input[buf->segnum] = NULL;
    } else {

        /* 次文節が２文字以上あった場合 ExpandKanjiConversion   */
        /* on の時は再度かな漢字変換変換する                    */
        if (buf->expandkconv & buf->input[nextseg]->status) {
            if (buf->input[nextseg]->num < INPUT_YOMI_MAX) {
                _Xsj3cwPStomPS(buf, kanabuf, buf->input[nextseg]->yomi);
                value = serverIF[buf->server].func[FUNC_CONV]
                        (kanabuf, bun, knjbuf, KANJIBUFSIZ);
            } else {
                Xsj3cWarning("Too long segment[num = %d]",nextseg);
            }
            if (value > 0) {
                _Xsj3cStoreKanji(buf, bun, nextseg, value, ON);
                buf->segnum += (value - 1);
                buf->convedsegnum += value;
            } else {
                if (value < 0) 
                    Xsj3cWarning("sj3serv is down. reconnect please");
                _Xsj3cStoreYomi(buf, buf->input[nextseg], 0);
                buf->input[nextseg]->status = SEG_NOCONV;
                if (!(buf->movebyseg & SEG_NOCONV))
                    buf->input[nextseg]->edit = SEG_EDIT;
            }
        } else {
            _Xsj3cStoreYomi(buf, buf->input[nextseg], 0);
            buf->input[nextseg]->status = SEG_NOCONV;
            if (!(buf->movebyseg & SEG_NOCONV))
                buf->input[nextseg]->edit = SEG_EDIT;
        }
        if (buf->gakusyuu)
            buf->input[nextseg]->change = ON;
    }

    /* カレント文節の最後に次文節の１文字を足す */
    buf->input[buf->curseg]->cur = buf->input[buf->curseg]->num;
    _Xsj3cInsertChar(buf, buf->input[buf->curseg], tmp, 1);

    if (buf->input[buf->curseg]->status & SEG_CONVED)
        buf->convedsegnum--;

    /* ExpandModeConversion on の時は合わせた部分の */
    /* 文字種を現文節の表示文字種に変換する         */
    if ((buf->expandmconv & buf->input[buf->curseg]->status) && !modematch) {
        Xsj3cSeg        seg = buf->input[buf->curseg];

        conv++;
        _Xsj3cwPStomPS(buf, kanabuf, seg->yomi);
        Xsj3cModeConv(buf, kanabuf, seg->cursegmode, seg->size);
        seg->num = _Xsj3cmPStowPSn(buf, seg->yomi, kanabuf, seg->size);
        if (seg->num > seg->size - YBUFSIZ) {
            Xsj3cResizeSegment(seg, seg->size * 2);
            seg->num = _Xsj3cmPStowPS(buf, seg->yomi, kanabuf);
        }
        seg->cur = seg->num;
    }

    /* ExpandKanjiConversion on の時はカレント  */
    /* 文節を再度かな漢字変換変換する           */
    if (buf->expandkconv & buf->input[buf->curseg]->status) {
        value = 0;
        if (!conv)
            _Xsj3cwPStomPS(buf, kanabuf, buf->input[buf->curseg]->yomi);
        if (buf->input[buf->curseg]->num < INPUT_YOMI_MAX) {
            value = serverIF[buf->server].func[FUNC_CONV]
                    (kanabuf, bun, knjbuf, KANJIBUFSIZ);
        } else {
            Xsj3cWarning("Too long segment[num = %d]",buf->curseg);
        }
        if (value > 0) {
            buf->convedsegnum++;
            buf->input[buf->curseg]->status = SEG_CONVED;
            buf->input[buf->curseg]->edit = SEG_NOEDIT;
            buf->input[buf->curseg]->dnum
                = _Xsj3cmPStowOUT(buf, buf->input[buf->curseg]->disp, knjbuf);
            if (buf->gakusyuu)
                buf->input[buf->curseg]->dcid = bun[0].dcid;
            if (value > 1) {
                int         changed,    flush;
                if ((buf->candidate = _Xsj3cCandidateInit(buf)) == NULL) {
                    Xsj3cWarning("sj3serv maybe down, or any trouble");
                } else {
                    Xsj3cSetCandidate(buf, 0, &changed, &flush);
                }
            }
        } else  {
            if (value < 0) 
                Xsj3cWarning("sj3serv is down. reconnect please");
            _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
            buf->input[buf->curseg]->status = SEG_NOCONV;
            if (!(buf->movebyseg & SEG_NOCONV))
                buf->input[buf->curseg]->edit = SEG_EDIT;
        }
    } else {
        value = 1;
        _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
        buf->input[buf->curseg]->status = SEG_NOCONV;
        if (!(buf->movebyseg & SEG_NOCONV))
            buf->input[buf->curseg]->edit = SEG_EDIT;
    }

    /* temporary のバッファを開放する   */
    free(kanabuf);

    /* 文節長学習のためのフラグを立てる */
    if (buf->gakusyuu)
        buf->input[buf->curseg]->change = ON;

    if (value > 0)
        return KEY_TEXT_CHANGE;
    else 
        return (KEY_TEXT_CHANGE|KEY_BELL);
}

/*
 * _Xsj3cExpandNoConv()
 *  Expand current segment by adding next segment.
 */
static Xsj3cEvent
_Xsj3cExpandNoConv(buf)
    Xsj3cBuf    buf;
{
    if (buf->curseg + buf->dict->n_dict < buf->segnum - 1) {
        buf->dict->n_dict++;
        return KEY_TEXT_CHANGE;
    } else {
        return KEY_BELL;
    }
}

/*
 * _Xsj3cShrink()
 *
 * <InputMode/ConvedMode> Shrink the current segment by pushing 
 *   the last character to the next segment.
 * <DictMode> Decrease reversed segments by getting off the last segment.
 * <NoInputMode/SelectMode> Rings bell.  
 *   
 * ShrinkModeConversion on: Change the next segment's character mode.
 * ShrinkKanjiConversion on: Do kana-kanji conversion for
 *   current and next segments.
 * ShrinkAll on: When there is only one character in current segment,
 *   combine with the previous segment
 *   unless current segment is the first segment.
 * ShrinkAll off: When there is only one character in current segment,
 *    ring bell.
 */
Xsj3cEvent
_Xsj3cShrink(buf)
    Xsj3cBuf                buf;
{
    int                     nextseg = buf->curseg + 1;
    int                     prevseg = buf->curseg - 1, value = 0;
    Xsj3cFlag               conv1 = 0,  conv2 = 0;
    register int            i,  size;
    unsigned char           tmp[YBUFSIZ];
    unsigned char          *kanabuf1,  *kanabuf2;
    unsigned char           knjbuf[KANJIBUFSIZ];
    SJ3_BUNSETU             bun[BUNBUFSIZ];
    Xsj3cFlag               erase_seg_flg = 0;

    if (buf->convmode & DictModeMask)
        return (_Xsj3cShrinkNoConv(buf));
    else if ((buf->convmode & (SelectModeMask|NoInputModeMask))
            || nextseg > buf->segnum)
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    else if (buf->input[buf->curseg]->num > 1 || (buf->shrinkall
            && buf->input[buf->curseg]->num > 0 && buf->curseg)) {
        /* カレント文節が２文字以上ある場合、または１文字   */
        /* ある場合で .ShrinkAll on で現文節が最初の文節で  */
        /* ない場合、カレント文節の最後の１文字を取り出す   */

        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
        buf->input[buf->curseg]->cur = buf->input[buf->curseg]->num;
        _Xsj3cExtractChar(buf, buf->input[buf->curseg], tmp, 1);

        /* .ShrinkAll on で現文節が１文字しかなかった場合   */
        /* 前文節と合節することを指示するフラグを ON にする */
        if (!buf->input[buf->curseg]->num)
            erase_seg_flg++;
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }

    /* temporary のバッファを作成する   */
    if (buf->input[nextseg])
        size = buf->input[nextseg]->size;
    else 
        size = KANABUFSIZ;
    if ((kanabuf2 = (unsigned char *)malloc(size * sizeof(wchar))) == NULL)
        Xsj3cError("Cannot allocate for temporary buffer");

    if (buf->curseg)
        size = buf->input[prevseg]->size;
    else 
        size = KANABUFSIZ;
    if ((kanabuf1 = (unsigned char *)malloc(size * sizeof(wchar))) == NULL)
        Xsj3cError("Cannot allocate for temporary buffer");


    if (buf->input[buf->curseg]->status & SEG_CONVED)
        buf->convedsegnum--;
    if (buf->shrinkall && buf->curseg && erase_seg_flg) {
        /* .ShrinkAll on で前文節と合節することを指示する   */
        /* フラグが ON の時、カレント文節から取り出した     */
        /* １文字を前文節の最後に挿入する                   */
        
        buf->input[prevseg]->cur = buf->input[prevseg]->num;
        _Xsj3cInsertChar(buf, buf->input[prevseg], tmp, 1);

        /* ShrinkModeConversion on の時は合わせた部分の */
        /* 文字種を前文節の表示文字種に変換する         */
        if ((buf->shrinkmconv & buf->input[prevseg]->status)
                && buf->input[prevseg]->cursegmode
                != buf->input[buf->curseg]->cursegmode) {
            Xsj3cSeg        seg = buf->input[prevseg];

            conv1++;
            _Xsj3cwPStomPS(buf, kanabuf1, seg->yomi);
            Xsj3cModeConv(buf, kanabuf1, seg->cursegmode, seg->size);
            seg->num = _Xsj3cmPStowPSn(buf, seg->yomi, kanabuf1, seg->size);
            if (seg->num > buf->input[buf->curseg - 1]->size - YBUFSIZ) {
                Xsj3cResizeSegment(seg, seg->size * 2);
                seg->num = _Xsj3cmPStowPS(buf, seg->yomi, kanabuf1);
            }
            seg->cur = seg->num;
        }

        /* 文節数を減じカレント文節の領域を開放する         */
        Xsj3cFreeSegment(buf->input[buf->segnum]);
        buf->input[buf->segnum] = NULL;
        buf->segnum--;
        Xsj3cFreeSegment(buf->input[buf->curseg]);
        buf->input[buf->curseg] = NULL;
        if (nextseg <= buf->segnum) {
            for ( i = buf->curseg; i < buf->segnum; i++) {
                buf->input[i] = buf->input[i + 1];
            }
        }
        buf->input[buf->segnum] = NULL;
        buf->curseg--;
    } else {
        /* カレント文節が２文字以上だった場合   */

        if (nextseg < buf->segnum) {
            /* カレント文節が最後の文節でない時、       */
            /* 取り出した１文字を次文節の先頭に挿入する */

            buf->input[nextseg]->cur = 0;
            _Xsj3cInsertChar(buf, buf->input[nextseg], tmp, 1);
            if (buf->input[nextseg]->status & SEG_CONVED)
                buf->convedsegnum--;

            /* ShrinkModeConversion on の時は分離した       */
            /* 部分の文字種を次文節の表示文字種に変換する   */
            if ((buf->shrinkmconv & buf->input[nextseg]->status)
                    && buf->input[nextseg]->cursegmode
                    != buf->input[buf->curseg]->cursegmode) {
                Xsj3cSeg        seg = buf->input[nextseg];

                conv2++;
                _Xsj3cwPStomPS(buf, kanabuf2, seg->yomi);
                Xsj3cModeConv(buf, kanabuf2, seg->cursegmode, seg->size);
                seg->num = _Xsj3cmPStowPSn(buf, seg->yomi, kanabuf2, seg->size);
                if (seg->num > seg->size - YBUFSIZ) {
                    Xsj3cResizeSegment(seg, seg->size * 2);
                    seg->num = _Xsj3cmPStowPS(buf, seg->yomi, kanabuf2);
                }
                seg->cur = seg->num;
            }
        } else {
            /* カレント文節が最後の文節の時、文節を１つ増やして */
            /* 次文節を作成し、取り出した１文字をコピーして     */
            /* カレント文節の属性を次文節にコピーする           */

            if (!buf->input[nextseg]) {
                if ((buf->input[nextseg]
                        = (Xsj3cSeg)Xsj3cCreateSegment(buf)) == NULL) {
                    Xsj3cError("Failed to allocate segment");
                }
            } else
                Xsj3cClearSegment(buf, buf->input[nextseg]);
            _Xsj3cInsertChar(buf, buf->input[nextseg], tmp, 1);
            buf->input[nextseg]->change = OFF;
            buf->input[nextseg]->edit = buf->input[buf->curseg]->edit;
            buf->input[nextseg]->status = buf->input[buf->curseg]->status;
            if (buf->input[nextseg]->status & SEG_CONVED)
                buf->convedsegnum--;
            buf->segnum++;
            buf->input[nextseg]->cursegmode
                = buf->input[buf->curseg]->cursegmode;
            buf->input[nextseg]->dcid = buf->input[buf->curseg]->dcid;
        }

        /* ShrinkKanjiConversion on の時は分離した      */
        /* 部分を合わせて次文節を再度かな漢字変換する   */
        if (buf->shrinkkconv & buf->input[nextseg]->status) {
            if (!conv2)
                _Xsj3cwPStomPS(buf, kanabuf2, buf->input[nextseg]->yomi);
            if (buf->input[nextseg]->num < INPUT_YOMI_MAX) {
                value = serverIF[buf->server].func[FUNC_CONV]
                        (kanabuf2, bun, knjbuf, KANJIBUFSIZ);
            } else {
                Xsj3cWarning("Too long segment[num = %d]",nextseg);
            }
            if (value > 0) {
                _Xsj3cStoreKanji(buf, bun, nextseg, value, ON);
                buf->segnum += (value - 1);
                buf->convedsegnum += value;
            } else {
                if (value < 0) 
                    Xsj3cWarning("sj3serv is down. reconnect please");
                _Xsj3cStoreYomi(buf, buf->input[nextseg], 0);
                buf->input[nextseg]->status = SEG_NOCONV;
                if (!(buf->movebyseg & SEG_NOCONV))
                    buf->input[nextseg]->edit = SEG_EDIT;
            }
        } else {
            _Xsj3cStoreYomi(buf, buf->input[nextseg], 0);
            buf->input[nextseg]->status = SEG_NOCONV;
            if (!(buf->movebyseg & SEG_NOCONV))
                buf->input[nextseg]->edit = SEG_EDIT;
        }
        if (buf->gakusyuu)
            buf->input[nextseg]->change = ON;
    }

    /* ExpandKanjiConversion on の時はカレント  */
    /* 文節を再度かな漢字変換変換する           */
    if (buf->shrinkkconv & buf->input[buf->curseg]->status) {
        value = 0;
        if (!conv1)
            _Xsj3cwPStomPS(buf, kanabuf1, buf->input[buf->curseg]->yomi);
        if (buf->input[buf->curseg]->num < INPUT_YOMI_MAX) {
            value = serverIF[buf->server].func[FUNC_CONV]
                    (kanabuf1, bun, knjbuf, KANJIBUFSIZ);
        } else {
            Xsj3cWarning("Too long segment[num = %d]",buf->curseg);
        }
        if (value > 0) {
            buf->convedsegnum++;
            buf->input[buf->curseg]->status = SEG_CONVED;
            buf->input[buf->curseg]->edit = SEG_NOEDIT;
            buf->input[buf->curseg]->dnum
                = _Xsj3cmPStowOUT(buf, buf->input[buf->curseg]->disp, knjbuf);
            if (buf->gakusyuu)
                buf->input[buf->curseg]->dcid = bun[0].dcid;
        } else {
            if (value < 0)
                Xsj3cWarning("sj3serv is down. reconnect please");
            _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
            buf->input[buf->curseg]->status = SEG_NOCONV;
            if (!(buf->movebyseg & SEG_NOCONV))
                buf->input[buf->curseg]->edit = SEG_EDIT;
        }
    } else {
        value = 1;
        _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
        buf->input[buf->curseg]->status = SEG_NOCONV;
        if (!(buf->movebyseg & SEG_NOCONV))
            buf->input[buf->curseg]->edit = SEG_EDIT;
    }

    /* temporary のバッファを開放する   */
    free(kanabuf1);
    free(kanabuf2);

    /* 文節長学習のためのフラグを立てる */
    if (buf->gakusyuu)
        buf->input[buf->curseg]->change = ON;

    if (value > 0)
        return KEY_TEXT_CHANGE;
    else 
        return (KEY_TEXT_CHANGE|KEY_BELL);
}

/*
 * _Xsj3cShrinkNoConv()
 *  Shrink current segment by reducing last segment.
 */
static Xsj3cEvent
_Xsj3cShrinkNoConv(buf)
    Xsj3cBuf    buf;
{
    if (buf->dict->n_dict) {
        buf->dict->n_dict--;
        return KEY_TEXT_CHANGE;
    } else {
        return (KEY_BELL);
    }
}

/*
 * _Xsj3cBackSpace()
 *
 * <InputMode/ConvedMode> Delete previous segment or character.
 * <DictMode> Delete previous character for yomi.
 * <SelectMode> Popdown the panel and delete previous segment or character.
 * <NoInputMode> Does nothing.
 *
 * DeleteBySegment on: Delete previous segment.
 * DeleteBySegment off: Delete previous character.
 * DeleteChangeSegment all: Unconvert all segments when "DeleteBySegment"
 *   is off.
 * DeleteChangeSegment one: Unconvert segments after previous segment 
 *   when "DeleteBySegment" is off.
 * DeleteChangeSegment after: Unconvert previous segment
 *   when "DeleteBySegment" is off.
 * SelectBackSpaceMove on: Move target segment to current segment.
 */
Xsj3cEvent
_Xsj3cBackSpace(buf)
    Xsj3cBuf        buf;
{
    Xsj3cEvent      ret = KEY_NULL;

    if (buf->convmode & SelectModeMask) {
        /* SelectMode の時は候補選択／記号選択    */
        /* ウィンドウをポップダウン             */
        ret |= KEY_SELECT_ABORT;
        if (buf->selectstatus == SELECT_HINSI) {
            buf->convmode = DictModeMask;
            buf->dict->status = DICT_INPUT;
        } else if (buf->selectstatus == SELECT_CAND) {
            buf->convmode = ConvedModeMask;
            if (buf->selectback)
                buf->curseg++;
        } else {
            buf->convmode = InputModeMask;
            if (!buf->segnum)
                return(ret);
        }
    } else if (buf->convmode & NoInputModeMask) {
        /* 文節(segement)数が 0のときは無視する */
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }

    switch(buf->convmode) {
    case InputModeMask:
        if ((buf->input[buf->curseg]->edit & SEG_NOEDIT) && 
                (buf->delbyseg & buf->input[buf->curseg]->status)) {
            if (buf->curseg > 0) {
                buf->curseg--;
                return(_Xsj3cDeleteSeg(buf, ret, buf->dellastmove));
            } else {
                return (KEY_BELL);
            }
        } else {
            if (buf->input[buf->curseg]->cur == 0 && buf->curseg > 0) {
                buf->curseg--;
                if ((buf->input[buf->curseg]->edit & SEG_NOEDIT) &&
                        buf->delbyseg & buf->input[buf->curseg]->status)
                    return(_Xsj3cDeleteSeg(buf, ret, buf->dellastmove));
                else {
                    ret |= _Xsj3cUnConvSeg(buf, buf->delchange, ON);
                    _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
                }
            }
            return(_Xsj3cBackSpaceChar(buf, buf->input[buf->curseg], ret)
                    & ~KEY_DICT_CHANGE);
        }
    case ConvedModeMask:
        if (buf->curseg > 0) {
            buf->curseg--;
        } else {
            return (KEY_BELL);
        }
        if (buf->delbyseg & buf->input[buf->curseg]->status) {
            return(_Xsj3cDeleteSeg(buf, ret, buf->dellastmove));
        } else {
            ret |= _Xsj3cUnConvSeg(buf, buf->delchange, ON);
            _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
            return(_Xsj3cBackSpaceChar(buf, buf->input[buf->curseg], ret)
                    & ~KEY_DICT_CHANGE);
        }
    case DictModeMask:
        /* DictMode の時  */
        if (buf->dict->status == DICT_INPUT) {
            ret = _Xsj3cBackSpaceChar(buf, buf->dict->seg, ret)
                & ~KEY_TEXT_CHANGE;
            _Xsj3cFlushDictMsg(buf);
            return(ret);
        } else {
            return(KEY_NULL);
        }
    default:
        return(ret);
    }
}

/*
 * _Xsj3cDelete()
 *
 * <InputMode/ConvedMode> Delete current segment or character.
 * <DictMode> Delete the character of current position.
 * <SelectMode> Popdown the panel and delete current segment or character.
 * <NoInputMode> Rings bell.
 *
 * DeleteBySegment on: Delete current segment.
 * DeleteBySegment off: Delete current character.
 * DeleteChangeSegment all: Unconvert all segments when "DeleteBySegment"
 *   is off.
 * DeleteChangeSegment one: Unconvert segments after current segment 
 *   when "DeleteBySegment" is off.
 * DeleteChangeSegment after: Unconvert current segment
 *   when "DeleteBySegment" is off.
 * DeleteLastMove on: Move current segment to previous
 *   after deleting last segment. 
 */
Xsj3cEvent
_Xsj3cDelete(buf)
    Xsj3cBuf        buf;
{
    Xsj3cEvent      ret = KEY_NULL;

    if (buf->convmode & SelectModeMask) {
        ret |= KEY_SELECT_ABORT;
        if (buf->selectstatus == SELECT_HINSI) {
            buf->convmode = DictModeMask;
            buf->dict->status = DICT_INPUT;
        } else if (buf->selectstatus == SELECT_CAND) {
            buf->convmode = ConvedModeMask;
        } else {
            buf->convmode = InputModeMask;
            if (buf->curseg >= buf->segnum) {
                return (ret);
            }
        }
        /* SelectMode の時は候補選択／記号選択    */
        /* ウィンドウをポップダウン             */
    } else if (buf->convmode & NoInputModeMask) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    } else if (buf->curseg >= buf->segnum) {
        /* あるいはカレント文節番号が文節数より */
        /* 小さくない時はベルを鳴らす           */
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }

    switch(buf->convmode) {
    case InputModeMask:
        if ((buf->input[buf->curseg]->edit & SEG_NOEDIT) && 
                (buf->delbyseg & buf->input[buf->curseg]->status)) {
            return(_Xsj3cDeleteSeg(buf, ret, buf->dellastmove));
        } else {
            if (buf->input[buf->curseg]->num == buf->input[buf->curseg]->cur
                    && buf->curseg < buf->segnum - 1) {
                buf->curseg++;
                if ((buf->input[buf->curseg]->edit & SEG_NOEDIT) &&
                        buf->delbyseg & buf->input[buf->curseg]->status)
                    return(_Xsj3cDeleteSeg(buf, ret, buf->dellastmove));
                else {
                    ret |= _Xsj3cUnConvSeg(buf, buf->delchange, OFF);
                    _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
                }
            }
            return(_Xsj3cDeleteChar(buf, buf->input[buf->curseg], ret)
                    & ~KEY_DICT_CHANGE);
        }
    case ConvedModeMask:
        if (buf->delbyseg & buf->input[buf->curseg]->status) {
            return(_Xsj3cDeleteSeg(buf, ret, buf->dellastmove));
        } else {
            ret |= _Xsj3cUnConvSeg(buf, buf->delchange, OFF);
            _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
            return(_Xsj3cDeleteChar(buf, buf->input[buf->curseg], ret)
                    & ~KEY_DICT_CHANGE);
        }
    case DictModeMask:
        /* DictMode の時  */
        if (buf->dict->status == DICT_INPUT) {
            ret = _Xsj3cDeleteChar(buf, buf->dict->seg, ret)
                & ~KEY_TEXT_CHANGE;
            _Xsj3cFlushDictMsg(buf);
            return(ret);
        } else {
            return(KEY_NULL);
        }
    default:
        return(ret);
    }
}

/*
 * _Xsj3cBackSpaceChar()
 *  Delete last character of string buffers.
 */
static Xsj3cEvent
_Xsj3cBackSpaceChar(buf, seg, ret)
    Xsj3cBuf                buf;
    Xsj3cSeg                seg;
    Xsj3cEvent              ret;
{
    unsigned char           tmp[YBUFSIZ];
    wchar                   wcs[RBUFSIZ];
    int                     change_pos, len;
    register int            i;

    if (seg->cur > 0) {
        /* カーソル位置より前に表示文字列が存在するとき */

        if (buf->backdisplay) {

            /* .BackDisplay  on の時    */

            change_pos = seg->cur - 1;
            if (seg->n_roma) {
                /* ひらがな／全角カタカナ／半角カタカナ入力モードの */
                /* ときでローマ字バッファに文字が残っているとき     */
                /* まず一文字削除する                               */
                _Xsj3cExtractChar(buf, seg, tmp, 1);
                change_pos = seg->cur;
                if (*seg->oldstr != '\0' && seg->value < 0) {
                    /* 退避バッファに文字があって直前のローマ字かな */
                    /* 変換の結果が変換不能の場合は退避バッファの   */
                    /* 文字をローマ字バッファにコピーする           */
                    strcpy(seg->str, seg->oldstr);
                    seg->n_roma = strlen(seg->oldstr);
                    seg->sp = seg->str;
                    seg->sp += seg->n_roma;
                    *(seg->sp) = '\0';
                    *seg->oldstr = '\0';
                } else {
                    /* ローマ字入力で直前の変換結果状態が不定のとき */
                    /* はときはローマ字バッファの半角アルファベット */
                    /* を１文字削除し、読みバッファや表示バッファの */
                    /* アルファベ ットも１文字消去する      */
                    seg->sp--;
                    *seg->sp = '\0';
                    seg->n_roma--;
                }
            } else if (seg->n_kana > 0) {
                /* かな入力でひらがな／全角カタカナ入力モードの時で */
                /* かなバッファに文字が残っている時はかなバッファ中 */
                /* のカナと読みバッファ、表示バッファ中のひらがな全 */
                /* 角カタカナを１文字消去し、更にもう１文字前の全角 */
                /* かなを半角カタカナに変換してかなバッファに入れる */
                _Xsj3cExtractChar(buf, seg, tmp, 1);
                change_pos = seg->cur;
                seg->sp = seg->str;
                *seg->str = '\0';
                if (seg->cur > 0 &&
                (ishira(seg->yomi[seg->cur - 1], serverIF[buf->server].lang) ||
                iskata(seg->yomi[seg->cur - 1], serverIF[buf->server].lang))) {
                    wcs[0] = seg->yomi[seg->cur - 1];
                    wcs[1] = '\0';
                    _Xsj3cwPStomPS(buf, tmp, wcs);
                    _Xsj3cZKanaToHKata(buf, seg->str, tmp);
                    if (isdakuon(*seg->str)
                            && strlen(seg->str) == 1) {
                        *(++seg->sp) = '\0';
                        seg->n_kana = 1;
                    } else {
                        seg->n_kana = 0;
                    }
                } else {
                    seg->n_kana = 0;
                }
            } else  {
                /* ローマ字入力時のローマ字バッファやかな入力時の   */
                /* かなバッファに文字が残っていないとき             */
                if (*seg->oldstr != '\0' &&
                iskan1(seg->yomi[seg->cur - 1] >> 8, serverIF[buf->server].lang)
                    && iskan2(seg->yomi[seg->cur - 1] & 0xff,
                    serverIF[buf->server].lang)) {
                    /* 退避バッファに文字があって削除する最後の */
                    /* 文字が全角文字のとき                     */
                    if (seg->n_kana < 0) {
                        /* ローマ字入力／コード入力の場合、１文字削 */
                        /* 除して退避バッファの文字をコピーする     */
                        strcpy(seg->str, seg->oldstr);
                        seg->n_roma = strlen(seg->oldstr);
                        _Xsj3cExtractChar(buf, seg, tmp, seg->oldlen);
                        change_pos = seg->cur;
                        if (buf->alphaconv && buf->inputmode != MODE_HKATA) {
                            _Xsj3cHAlphaToZKana(buf, tmp, seg->oldstr);
                            _Xsj3cInsertChar(buf, seg, tmp, seg->n_roma);
                        } else {
                            _Xsj3cInsertChar(buf, seg, seg->oldstr,
                                    seg->n_roma);
                        }
                        seg->sp = seg->str;
                        seg->sp += seg->n_roma;
                    } else {
                        /* かな入力の場合１文字削除して退避バッファ */
                        /* 文字をコピーしその文字を再度かな変換する */
                        _Xsj3cExtractChar(buf, seg, tmp, 1);
                        change_pos = seg->cur;
                        seg->sp = seg->str;
                        strcpy(seg->str, seg->oldstr);
                        if ((seg->value = _Xsj3cKanaConv(buf, seg, seg->str,
                                tmp, buf->inputmode)) > 0) {
                            seg->n_kana = 0;
                        } else if (seg->value == 0) {
                            seg->n_kana = 1;
                            *(++seg->sp) = '\0';
                            len = _Xsj3cmPStowPS(buf, wcs, tmp);
                            _Xsj3cInsertWchar(seg, wcs, len);
                        } else {
                            seg->n_kana = 0;
                        }
                    }
                    *seg->oldstr = '\0';
                    seg->oldlen = 0;
                } else if (*seg->oldstr != '\0') {
                    /* コード入力で半角に変換される時   */
                    /* または半カナモードの時           */
                    strcpy(seg->str, seg->oldstr);
                    seg->n_roma = strlen(seg->oldstr);
                    _Xsj3cExtractChar(buf, seg, tmp, seg->oldlen);
                    change_pos = seg->cur;
                    if (buf->alphaconv && buf->inputmode != MODE_HKATA) {
                        _Xsj3cHAlphaToZKana(buf, tmp, seg->oldstr);
                        _Xsj3cInsertChar(buf, seg, tmp, seg->n_roma);
                    } else {
                        _Xsj3cInsertChar(buf, seg, seg->oldstr,
                                seg->n_roma);
                    }
                    seg->sp = seg->str;
                    seg->sp += seg->n_roma;
                    *seg->oldstr = '\0';
                    seg->oldlen = 0;
                } else {
                    /* 退避バッファに文字が残っていないとき     */
                    _Xsj3cExtractChar(buf, seg, tmp, 1);
                    change_pos = seg->cur;
                    *seg->str = '\0';
                    seg->sp = seg->str;
                    if (seg->n_kana >= 0) {
                        if (seg->cur > 0 && (ishira(seg->yomi[seg->cur - 1],
                                serverIF[buf->server].lang) ||
                                iskata(seg->yomi[seg->cur - 1],
                                serverIF[buf->server].lang))) {
                            /* かな入力のときは更に１文字前 */
                            /* の全角かなを半角カタカナに変 */
                            /* 換してかなバッファに入れる   */
                            wcs[0] = seg->yomi[seg->cur - 1];
                            wcs[1] = '\0';
                            _Xsj3cwPStomPS(buf, tmp, wcs);
                            _Xsj3cZKanaToHKata(buf, seg->str, tmp);
                            if (isdakuon(*seg->str)
                                    && strlen(seg->str) == 1) {
                                *(++seg->sp) = '\0';
                                seg->n_kana = 1;
                            } else {   
                                seg->n_kana = 0;
                                *seg->str = '\0';
                            }
                        } else {
                            seg->n_kana = 0;
                        }
                    } else {
                        seg->n_kana = -1;
                    }
                }
            }
        } else {
            /* .BackDisplay  off の時   */

            _Xsj3cExtractChar(buf, seg, tmp, 1);
            change_pos = seg->cur;
            if (seg->n_roma) {
                /* ローマ字入力でローマ字バッファに文字がある場合  */
                seg->sp--;
                *seg->sp = '\0';
                seg->n_roma--;
            } else if (seg->n_kana >= 0) {
                /* かな入力のときは更に１文字前の全角かな   */
                /* を半角カタカナに変換して、それが濁音     */
                /* 候補だったらかなバッファに入れる         */
                *seg->str = '\0';
                seg->sp = seg->str;
                if (seg->cur > 0 && (ishira(seg->yomi[seg->cur - 1],
                        serverIF[buf->server].lang) ||
                        iskata(seg->yomi[seg->cur - 1],
                        serverIF[buf->server].lang))) {
                    wcs[0] = seg->yomi[seg->cur - 1];
                    wcs[1] = '\0';
                    _Xsj3cwPStomPS(buf, tmp, wcs);
                    _Xsj3cZKanaToHKata(buf, seg->str, tmp);
                    if (isdakuon(*seg->str)
                            && strlen(seg->str) == 1) {
                        *(++seg->sp) = '\0';
                        seg->n_kana = 1;
                    } else {   
                        seg->n_kana = 0;
                        *seg->str = '\0';
                    }
                } else {
                    seg->n_kana = 0;
                }
            } else {
                *seg->str = '\0';
                seg->sp = seg->str;
                seg->n_kana = -1;
            }
        }
        _Xsj3cStoreYomi(buf, seg, change_pos);
        if (seg->num < seg->size - KANABUFSIZ - YBUFSIZ) 
            Xsj3cResizeSegment(seg, seg->size - KANABUFSIZ);
        if (seg->num == 0 && (buf->convmode & ~DictModeMask)) {
            Xsj3cFreeSegment(buf->input[buf->segnum]);
            buf->input[buf->segnum] = NULL;
            buf->segnum--;
            Xsj3cFreeSegment(seg);
            seg = NULL;
            for (i = buf->curseg; i < buf->segnum; i++) {
                buf->input[i] = buf->input[i + 1];
            }
            buf->input[buf->segnum] = NULL;
        }
        if (buf->dispmodechange) {
            buf->dispmode = ((buf->segnum &&
                buf->convedsegnum == buf->segnum) ? MODE_KANJI :
                (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
            ret |= KEY_MODE_CHANGE;
        }
        if (buf->curseg && buf->curseg == buf->segnum && buf->dellastmove)
            buf->curseg--;

        return(ret|KEY_TEXT_CHANGE|KEY_DICT_CHANGE);
    } else {
        /* カーソル位置より前に表示文字列がないとき */
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cDeleteChar()
 *  Delete next character of string buffers.
 */
static Xsj3cEvent
_Xsj3cDeleteChar(buf, seg, ret)
    Xsj3cBuf                buf;
    Xsj3cSeg                seg;
    Xsj3cEvent              ret;
{
    unsigned char           tmp[YBUFSIZ];
    int                     change_pos;
    register int            i;

    if (seg->cur < seg->num) {
        /* カーソル位置より後ろに表示文字列が存在するとき */

        change_pos = seg->cur;
        seg->cur++;
        _Xsj3cExtractChar(buf, seg, tmp, 1);
        _Xsj3cStoreYomi(buf, seg, change_pos);
        if (seg->num < seg->size - KANABUFSIZ - YBUFSIZ) 
            Xsj3cResizeSegment(seg, seg->size - KANABUFSIZ);
        if (seg->num == 0 && (buf->convmode & ~DictModeMask)) {
            Xsj3cFreeSegment(buf->input[buf->segnum]);
            buf->input[buf->segnum] = NULL;
            buf->segnum--;
            Xsj3cFreeSegment(seg);
            seg = NULL;
            for (i = buf->curseg; i < buf->segnum; i++) {
                buf->input[i] = buf->input[i + 1];
            }
            buf->input[buf->segnum] = NULL;
            if (buf->dispmodechange) {
                buf->dispmode = ((buf->segnum &&
                    buf->convedsegnum == buf->segnum) ? MODE_KANJI :
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
                ret |= KEY_MODE_CHANGE;
            }
            if (buf->curseg && buf->curseg == buf->segnum && buf->dellastmove)
                buf->curseg--;
        }
        return(ret|KEY_TEXT_CHANGE|KEY_DICT_CHANGE);
    } else {
        /* カーソル位置より後ろに表示文字列がないとき */

        return(KEY_NULL);
    }
}

/*
 * _Xsj3cDeleteSeg()
 *  Delete current segment.
 */
static Xsj3cEvent
_Xsj3cDeleteSeg(buf, ret, move)
    Xsj3cBuf                buf;
    Xsj3cEvent              ret;
    Xsj3cFlag               move;
{
    register int            i;

    buf->n_select = 0;
    if ((buf->convmode & ConvedModeMask) && buf->candidate) 
        Xsj3cEndCandidate(buf, OFF);
    if (buf->segnum <= buf->curseg) {
        return (ret);
    }
    Xsj3cFreeSegment(buf->input[buf->segnum]);
    buf->input[buf->segnum] = NULL;
    buf->segnum--;
    if (!buf->segnum) {
        /* 文節数が 0 になったときは変換用文字   */
        /* バッファをクリアして InputMode にする */

        buf->convedsegnum = 0;
        buf->curseg = 0;
        Xsj3cClearSegment(buf, buf->input[0]);
    } else if (buf->curseg == buf->segnum) {

        if (buf->input[buf->curseg]->status & SEG_CONVED) {
            buf->convedsegnum--;
            if (!buf->convedsegnum && buf->gakusyuu) {
                _Xsj3cClearDcid(buf);
            }
        }
        Xsj3cFreeSegment(buf->input[buf->curseg]);
        buf->input[buf->curseg] = NULL;
        /* 変換中の文章の最後の文節の時は現文節をひとつ前にずらす */
        if (move)
            buf->curseg--;
    } else if (buf->curseg < buf->segnum) {
        /* 最後の文節でないの時は以降の文節を   */
        /* ひとつずつ前にずらす                 */

        if (buf->input[buf->curseg]->status & SEG_CONVED) {
            buf->convedsegnum--;
            if (!buf->convedsegnum && buf->gakusyuu) {
                _Xsj3cClearDcid(buf);
            }
        }
        Xsj3cFreeSegment(buf->input[buf->curseg]);
        buf->input[buf->curseg] = NULL;
        for (i = buf->curseg; i < buf->segnum; i++) {
            buf->input[i] = buf->input[i + 1];
        }
        buf->input[buf->segnum] = NULL;
    }
    if (buf->dispmodechange) {
        buf->dispmode = ((buf->segnum &&
            buf->convedsegnum == buf->segnum) ? MODE_KANJI :
            (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
        ret |= KEY_MODE_CHANGE;
    }
    ret |= KEY_TEXT_CHANGE;
    return(ret);
}

/*
 * _Xsj3cDelAfter()
 *
 * <InputMode/ConvedMode> Delete current segment and all segments after
 *   current segment or cursor position.
 * <DictMode> Delete all yomi strings.
 * <SelectMode> Popdown the panel and delete strings after current segment
 *  or cursor position.
 * <NoInputMode> Rings bell.
 *
 * DeleteBySegment on: Delete current segment and all segments after current.
 * DeleteBySegment off: Delete strings after current cursor position
 *   in current segment.
 * DeleteLastMove on: Move current segment to previous
 *   after deleting last segment. 
 */
Xsj3cEvent
_Xsj3cDelAfter(buf)
    Xsj3cBuf            buf;
{
    Xsj3cEvent          ret = KEY_NULL;
    register int        i,  begin;
    int                 del_num;
    unsigned char      *tmp;

    if (buf->convmode & SelectModeMask) {
        if (buf->selectstatus == SELECT_HINSI || buf->curseg >= buf->segnum) {
            if (buf->selectstatus == SELECT_HINSI)
                buf->dict->status = DICT_INPUT;
            if (buf->dispmodechange) {
                buf->dispmode =
                    (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
                return (KEY_SELECT_ABORT|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_ABORT);
        } else {
            ret |= KEY_SELECT_ABORT;
        }
        /* SelectMode の時は候補選択／記号選択    */
        /* ウィンドウをポップダウン             */
    } else if (buf->convmode & (NoInputModeMask|DictModeMask)) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    } else if (buf->curseg >= buf->segnum) {
        /* あるいはカレント文節番号が文節数より */
        /* 小さくない時はベルを鳴らす           */
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }

    ret |= KEY_TEXT_CHANGE;
    if (buf->killbyseg & buf->input[buf->curseg]->status) {
        if (buf->input[buf->curseg]->edit & SEG_NOEDIT) {
            begin = buf->curseg;
        } else {
            if (!buf->input[buf->curseg]->cur) {
                begin = buf->curseg;
            } else {
                begin = buf->curseg + 1;
                if (buf->input[buf->curseg]->cur
                        < buf->input[buf->curseg]->num) {
                    del_num = buf->input[buf->curseg]->num
                            - buf->input[buf->curseg]->cur;
                    buf->input[buf->curseg]->cur = buf->input[buf->curseg]->num;
                    if ((tmp = (unsigned char *)
                            malloc(buf->input[buf->curseg]->size
                            * sizeof(wchar))) == NULL)
                        Xsj3cError("Cannot allocate for temporary buffer");
                    _Xsj3cExtractChar(buf, buf->input[buf->curseg], tmp, del_num);
                    free(tmp);
                    _Xsj3cStoreYomi(buf, buf->input[buf->curseg],
                            buf->input[buf->curseg]->cur);
                }
            }
        }
        for (i = begin; i < buf->segnum + 1; i++) {
            if (buf->input[i]->status & SEG_CONVED)
                buf->convedsegnum--;
            Xsj3cFreeSegment(buf->input[i]);
            buf->input[i] = NULL;
        }
        buf->segnum = begin;
        if (begin == buf->curseg && buf->dellastmove && buf->curseg) 
            buf->curseg--;
        if (buf->dispmodechange) {
            buf->dispmode = ((buf->segnum &&
                buf->convedsegnum == buf->segnum) ? MODE_KANJI :
                (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
            ret |= KEY_MODE_CHANGE;
        }
        if (buf->gakusyuu && !buf->convedsegnum) 
            _Xsj3cClearDcid(buf);
    } else {
        if (buf->input[buf->curseg]->edit & SEG_NOEDIT
                || !buf->input[buf->curseg]->cur) {
            ret |= _Xsj3cDeleteSeg(buf, ret, buf->dellastmove);
        } else {
            if (buf->input[buf->curseg]->cur
                    < buf->input[buf->curseg]->num) {
                del_num = buf->input[buf->curseg]->num
                        - buf->input[buf->curseg]->cur;
                buf->input[buf->curseg]->cur = buf->input[buf->curseg]->num;
                if ((tmp = (unsigned char *)
                        malloc(buf->input[buf->curseg]->size
                        * sizeof(wchar))) == NULL)
                    Xsj3cError("Cannot allocate for temporary buffer");
                _Xsj3cExtractChar(buf, buf->input[buf->curseg], tmp, del_num);
                free(tmp);
                _Xsj3cStoreYomi(buf, buf->input[buf->curseg],
                        buf->input[buf->curseg]->cur);
            }
        }
    }

    return(ret);
}

/*
 * _Xsj3cStart()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> If now on converting,
 *   fix all segments, then end kana-kanji converting.
 *
 * FlushEndConversion on: Flush converting strings before ending.  
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cStart(buf)
    Xsj3cBuf            buf;
{
    Xsj3cEvent          ret = KEY_HENKAN_END;
    register int        i;

    if (buf->dispmodechange) {
        buf->dispmode = buf->inputmode;
    }
    switch(buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
    case InputModeMask:
        break;
    case SelectModeMask:
        if (buf->flusheconv)
            ret |= KEY_SELECT_END;
        else 
            ret |= KEY_SELECT_ABORT;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
        buf->n_select = 0;
        break;
    case DictModeMask:
        ret |= KEY_DICT_END;
        break;
    case NoInputModeMask:
        return (KEY_HENKAN_END);
    default:
        /* Not supported    */
        return (KEY_HENKAN_START);
    }
    if (buf->flusheconv) {
        ret |= KEY_TEXT_FIXED;
    } else {
        for (i = 1; i < buf->segnum + 1; i++) {
            Xsj3cFreeSegment(buf->input[i]);
            buf->input[i] = NULL;
        }
        if (buf->input[0])
            Xsj3cClearSegment(buf, buf->input[0]);
        buf->segnum = 0;
        buf->convedsegnum = 0;
        buf->curseg = 0;
    }
    return ret;
}

/*
 * _Xsj3cReConnect()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Popdown the panel
 *   and clear all segments, then reonnect to sj3serv.
 *  
 * FlushEndConversion on: Flush converting strings before reconnecting.  
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cReConnect(buf)
    Xsj3cBuf    buf;
{
    Xsj3cEvent  ret = KEY_TEXT_CHANGE;

    if (buf->gakusyuu) {
        _Xsj3cClearDcid(buf);
    }
    switch (buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
    case InputModeMask:
    case NoInputModeMask:
        ret = KEY_RECONNECT;
        break;
    case SelectModeMask:
        ret = KEY_SELECT_ABORT|KEY_RECONNECT;
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
        break;
    case DictModeMask:
        ret = KEY_DICT_END|KEY_RECONNECT;
        break;
    default:
        ret = KEY_RECONNECT;
        break;
    }
    if (buf->dispmodechange) {
        buf->dispmode = buf->inputmode;
        ret |= KEY_MODE_CHANGE;
    }
    return ret;
}

/*
 * _Xsj3cReConvert()
 *
 * <NoInputMode> If there is no string in input buffer,
 *   copy from backup buffer which saved last fixed or flushed.
 *   But there is any strings in input buffer or backup buffer is null,
 *   ring bell.
 * <InputMode/UnputMode/SelectMode/DictMode> Ring bell.
 *
 * BeginConversionLast on: Set current segment to the last one. 
 * BeginConversionLast none: Allow to move out of segments. 
 */
Xsj3cEvent
_Xsj3cReConvert(buf)
    Xsj3cBuf            buf;
{
    register int        i,  conved;

    if ((buf->convmode & NoInputModeMask) && buf->backup) {
        buf->segnum = buf->backsegnum;
        if (!buf->segnum)
#ifdef THROUGH_CONT
            return (KEY_NULL);
#else /* THROUGH_CONT */
            if (buf->cntrlsame)
                return (KEY_NULL);
            else 
                return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
        for (i = 0, conved = 0; i < buf->segnum; i++) {
            if (!buf->input[i]) {
                if ((buf->input[i]
                        = (Xsj3cSeg)Xsj3cCreateSegment(buf)) == NULL) {
                    Xsj3cError("Failed to allocate segment");
                }
            } else {
                *buf->input[i]->oldstr = '\0';
                buf->input[i]->oldlen = 0;
                *buf->input[i]->str = '\0';
                buf->input[i]->sp = buf->input[i]->str;
                buf->input[i]->change = OFF;
                buf->input[i]->n_roma = 0;
                buf->input[i]->n_kana = -1;
                buf->input[i]->value = 0;
            }
            _Xsj3cWcpy(buf->input[i]->yomi, buf->backup[i]->yomi);
            buf->input[i]->num = buf->backup[i]->num;
            buf->input[i]->cur = buf->backup[i]->cur;
            _Xsj3cWcpy(buf->input[i]->disp, buf->backup[i]->disp);
            buf->input[i]->dnum = buf->backup[i]->dnum;
            buf->input[i]->dcid = buf->backup[i]->dcid;
            buf->input[i]->edit = buf->backup[i]->edit;
            buf->input[i]->cursegmode = buf->backup[i]->cursegmode;
            if ((buf->input[i]->status = buf->backup[i]->status) == SEG_CONVED)
                conved++;
            buf->input[i]->size = buf->backup[i]->size;
        }
        buf->convedsegnum = conved;
        switch (buf->beginlastseg) {
        case NONE:
            buf->curseg = buf->segnum;
            break;
        case ON:
            buf->curseg = buf->segnum - 1;
            break;
        case OFF:
            buf->curseg = 0;
            break;
        default:
            buf->curseg = 0;
            break;
        }
        return (KEY_TEXT_CHANGE);
    } else {
        return KEY_NULL;
    }
}

/*
 * _Xsj3cEdit() [edit]
 *
 * <InputMode/ConvedMode> Unconvert segments.
 * <SelectMode> pop down the panel candidate(symbol/hinsi) panel and
 *   unconvert current segment.
 * <DictMode> Pop down Auxpanel and unconvert current segment.
 * <NoInputMode> Does nothing.
 *
 * DisplayModeChange on: Change the display mode string.  
 * EditCursorLast on: Set cursor position to bottom of segment.
 * EditCursorLast off: Set cursor position to top of segment.
 */
Xsj3cEvent
_Xsj3cEdit(buf)
    Xsj3cBuf            buf;
{
    Xsj3cEvent          ret = KEY_NULL;

    if (buf->convmode & SelectModeMask) {
        ret |= KEY_SELECT_ABORT;
        if (buf->selectstatus == SELECT_HINSI) {
            ret |= KEY_DICT_END;
        }
    } else if (buf->convmode & DictModeMask) {
        ret |= KEY_DICT_END;
    } else if (buf->convmode & NoInputModeMask) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    }
    ret |= _Xsj3cUnConvSeg(buf, ONE, buf->editcurlast);
    _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
    buf->input[buf->curseg]->edit = SEG_EDIT;
    return (ret);
}

/*
 * _Xsj3cDRegBegin()
 *
 * <InputMode/ConvedMode> Begin to registr the word in the dictionary.
 * <NoInputMode/DictMode/SelectMode> Rings bell.
 *  
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cDRegBegin(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & (ConvedModeMask|InputModeMask)) {
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, ON);
        buf->dict = _Xsj3cCreateDictData(buf, REG_STATE);
        buf->convmode = DictModeMask;
        _Xsj3cFlushDictMsg(buf);
        if (buf->dispmodechange) {
            buf->dispmode = MODE_TOROKU;
            return (KEY_DICT_START|KEY_MODE_CHANGE);
        } else
            return (KEY_DICT_START);
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cDClearBegin()
 *
 * <InputMode/ConvedMode> Begin to eliminate the word in the dictionary.
 * <NoInputMode/DictMode/SelectMode> Rings bell.
 *  
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cDClearBegin(buf)
    Xsj3cBuf    buf;
{
    if (buf->convmode & (ConvedModeMask|InputModeMask)) {
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
        buf->dict = _Xsj3cCreateDictData(buf, CLR_STATE);
        buf->convmode = DictModeMask;
        _Xsj3cFlushDictMsg(buf);
        if (buf->dispmodechange) {
            buf->dispmode = MODE_SYOUKYO;
            return (KEY_DICT_START|KEY_MODE_CHANGE);
        } else
            return (KEY_DICT_START);
    } else {
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }
}

/*
 * _Xsj3cSymbolBegin()
 *
 * <NoInputMode/InputMode/ConvedMode> Begin symbol selecting
 *   and popup the panel.
 * <DictMode/SelectMode> Ring bell.
 *  
 * DisplayModeChange on: Change the display mode string.  
 */
Xsj3cEvent
_Xsj3cSymbolBegin(buf)
    Xsj3cBuf        buf;
{
    Xsj3cEvent      ret = KEY_SYMBOL_START;

    if (buf->convmode & (InputModeMask|NoInputModeMask|ConvedModeMask)) {
        if (buf->dispmodechange) {
            buf->dispmode = MODE_SYMBOL;
            ret |= KEY_MODE_CHANGE;
        }
        buf->selectstatus = SELECT_SYMBOL;
        buf->convmode = SelectModeMask;
        return (ret);
    } else {
        return (KEY_BELL);
    }
}

/*
 * _Xsj3cFlushBefore()
 *
 * <InputMode/ConvedMode> Delete current segment and all segments after
 *   current segment or cursor position.
 * <DictMode> Delete all yomi strings.
 * <SelectMode> Popdown the panel and delete strings after current segment
 *  or cursor position.
 * <NoInputMode> Does nothing.
 *
 * FlushChangeSegment off: Fix strings before current cursor position
 * FlushChangeSegment one: Fix strings before current cursor position
 *   and unconvert one segment.
 * FlushChangeSegment all: Fix strings before current cursor position
 *   and unconvert all segments.
 * FlushCursorLast on: Set cursor position to bottom of segment.
 * FlushCursorLast off: Set cursor position to top of segment.
 */
Xsj3cEvent
_Xsj3cFlushBefore(buf)
    Xsj3cBuf        buf;
{
    Xsj3cEvent      ret = KEY_NULL;
    register int    i,  j;
    int             store_num;
    unsigned char  *tmp;
    
    if (buf->convmode & SelectModeMask) {
        if (buf->selectstatus == SELECT_HINSI || buf->curseg >= buf->segnum) {
            if (buf->selectstatus == SELECT_HINSI)
                buf->dict->status = DICT_INPUT;
            if (buf->dispmodechange) {
                buf->dispmode =
                    (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
                return (KEY_SELECT_ABORT|KEY_MODE_CHANGE);
            } else
                return (KEY_SELECT_ABORT);
        } else {
            ret |= KEY_SELECT_END;
        }
        /* SelectMode の時は候補選択／記号選択    */
        /* ウィンドウをポップダウン             */
    } else if (buf->convmode & (NoInputModeMask|DictModeMask)) {
#ifdef THROUGH_CONT
        return (KEY_NULL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_NULL);
        else 
            return (KEY_TEXT_CHANGE);   /* dummy */
#endif /* THROUGH_CONT */
    } else if (buf->curseg >= buf->segnum) {
        /* あるいはカレント文節番号が文節数より */
        /* 小さくない時はベルを鳴らす           */
#ifdef THROUGH_CONT
        return (KEY_BELL);
#else /* THROUGH_CONT */
        if (buf->cntrlsame)
            return (KEY_BELL);
        else 
            return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
    }

    if (!buf->backup) {
        if ((buf->backup = (Xsj3cSeg *)calloc(BUNBUFSIZ,
                sizeof(Xsj3cSeg))) == NULL) {
            Xsj3cError("Cannot allocate for backup buffers");
        }
    } else { 
        for (i = 0; i < buf->backsegnum + 1; i++) {
            Xsj3cFreeSegment(buf->backup[i]);
            buf->backup[i] = NULL;
        }
    }

    switch(buf->flushchange) {
    case ONE:
        ret |= _Xsj3cUnConvSeg(buf, ONE, buf->flushcurlast);
        _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
        break;
    case ALL:
        ret |= _Xsj3cUnConvSeg(buf, AFTER, buf->flushcurlast);
        _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
        buf->input[buf->curseg]->edit = SEG_EDIT;
        break;
    case OFF:
    default:
        break;
    }

    ret |= (KEY_TEXT_CHANGE|KEY_TEXT_FLUSH);
    for (i = buf->curseg, j = 0; i < buf->segnum; i++, j++) {
        buf->backup[j] = buf->input[i];
        buf->input[i] = NULL;
    }
    buf->backsegnum = j;
    Xsj3cFreeSegment(buf->input[buf->segnum]);
    buf->input[buf->segnum] = NULL;
    buf->segnum = buf->curseg;
    for (i = 0; i < buf->curseg; i++)
        if (buf->input[i]->status & SEG_CONVED)
            buf->convedsegnum--;

    if (buf->flushchange == OFF && (buf->backup[0]->edit & SEG_EDIT)
        && (buf->backup[0]->cur < buf->backup[0]->num) 
        && buf->backup[0]->cur) {
        /* .FlushChangeSegment が off でカーソルが端にないとき */

        buf->backup[0]->cur = buf->backup[0]->num;
        if ((tmp = (unsigned char *)malloc(buf->backup[0]->size
                * sizeof(wchar))) == NULL)
            Xsj3cError("Cannot allocate for temporary buffer");
        store_num = buf->backup[0]->cur;
        _Xsj3cExtractChar(buf, buf->backup[0], tmp, buf->backup[0]->cur);
        _Xsj3cStoreYomi(buf, buf->backup[0], 0);
        if (!buf->input[buf->segnum]) {
            if ((buf->input[buf->segnum]
                    = (Xsj3cSeg)Xsj3cCreateSegment(buf)) == NULL) {
                Xsj3cError("Failed to allocate segment.");
            }
        } else
            Xsj3cClearSegment(buf, buf->input[buf->segnum]);
        _Xsj3cInsertChar(buf, buf->input[buf->segnum], tmp, store_num);
        _Xsj3cStoreYomi(buf, buf->input[buf->segnum], 0);
        buf->segnum++;
        free(tmp);
    }

    if (buf->gakusyuu && !buf->convedsegnum) 
        _Xsj3cClearDcid(buf);
    if (buf->dispmodechange) {
        buf->dispmode = ((buf->segnum &&
            buf->convedsegnum == buf->segnum) ? MODE_KANJI :
            (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
        ret |= KEY_MODE_CHANGE;
    }
    return(ret);
}

/*
 * _Xsj3cQuote()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Set "through flag"
 *    and input next character unconditionally.
 */
Xsj3cEvent
_Xsj3cQuote(buf)
    Xsj3cBuf        buf;
{
    Xsj3cEvent      ret = KEY_NULL;
    
    buf->throughflg = QUOTE;
    if (buf->dispmodechange) {
        buf->dispmode = MODE_QUOTE;
        ret |= KEY_MODE_CHANGE;
    }
#ifndef THROUGH_CONT
    if (!buf->cntrlsame)
        ret |= KEY_TEXT_CHANGE;     /* dummy    */
#endif /* THROUGH_CONT */
    return (ret);
}

/*
 * _Xsj3cBell()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Rings bell.
 */
Xsj3cEvent
_Xsj3cBell(buf)
    Xsj3cBuf    buf;
{
#ifdef THROUGH_CONT
    return (KEY_BELL);
#else /* THROUGH_CONT */
    if (buf->cntrlsame)
        return (KEY_BELL);
    else 
        return (KEY_TEXT_CHANGE|KEY_BELL);  /* dummy */
#endif /* THROUGH_CONT */
}

/*
 * _Xsj3cKana()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> 
 *   Toggle keyboard input mode between ASCII and Kana.
 *
 * KanaInputOnly on: Set toggle on in initializing.
 */
Xsj3cEvent
_Xsj3cKana(buf)
    Xsj3cBuf    buf;
{
    Xsj3cEvent  ret = KEY_NULL;

    if ((buf->convmode & DictModeMask)||((buf->convmode & SelectModeMask)
            && buf->selectstatus == SELECT_HINSI)) {
        buf->dict->seg->n_roma = 0;
        buf->dict->seg->n_kana = -1;
        *buf->dict->seg->oldstr = '\0';
        *buf->dict->seg->str = '\0';
        buf->dict->seg->sp = buf->dict->seg->str;
    }
    if (buf->convmode & ~NoInputModeMask) {
        buf->input[buf->curseg]->n_roma = 0;
        buf->input[buf->curseg]->n_kana = -1;
        *buf->input[buf->curseg]->oldstr = '\0';
        *buf->input[buf->curseg]->str = '\0';
        buf->input[buf->curseg]->sp = buf->input[buf->curseg]->str;
    }
    if (buf->kanaonly)
        buf->kanaonly = OFF;
    else 
        buf->kanaonly = ON;
#ifndef THROUGH_CONT
    if (!buf->cntrlsame)
        ret |= KEY_TEXT_CHANGE;     /* dummy    */
#endif /* THROUGH_CONT */
    return ret;
}

/*
 * _Xsj3cSjrc()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Reset customize.
 *
 * NextRCFile filename: Set file name to read next.
 */
Xsj3cEvent
_Xsj3cSjrc(buf)
    Xsj3cBuf            buf;
{
    Xsj3cEvent          ret = (KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
    register int        i;

    if (buf->dispmodechange) {
        ret |= KEY_MODE_CHANGE;
        buf->dispmode = buf->inputmode;
    }
    switch(buf->convmode) {
    case ConvedModeMask:
        buf->n_select = 0;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
    case InputModeMask:
        break;
    case SelectModeMask:
        buf->n_select = 0;
        if (buf->flusheconv)
            ret |= KEY_SELECT_END;
        else 
            ret |= KEY_SELECT_ABORT;
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, OFF);
        break;
    case DictModeMask:
        ret |= KEY_DICT_END;
        break;
    case NoInputModeMask:
    default:
        break;
    }
    if (buf->flusheconv) {
        ret |= KEY_TEXT_FIXED;
        if (buf->gakusyuu)
            _Xsj3cFlushDcid(buf);
    } else {
        for (i = 1; i < buf->segnum + 1; i++) {
            Xsj3cFreeSegment(buf->input[i]);
            buf->input[i] = NULL;
        }
        if (buf->input[0])
            Xsj3cClearSegment(buf, buf->input[0]);
        buf->segnum = 0;
        buf->curseg = 0;
    }
    if (buf->setnormal) {
        free(buf->setnormal);
        buf->setnormal = NULL;
    }
    if (buf->throughnext) {
        free(buf->throughnext);
        buf->throughnext = NULL;
    }
    Xsj3cRCInit(buf, NULL, NULL);
    return ret;
}

/*
 * _Xsj3cKill()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Exit.
 */
Xsj3cEvent
_Xsj3cKill(buf)
    Xsj3cBuf    buf;
{
    /* Not Yet  */
    return (KEY_NULL);
}

/*
 * _Xsj3cNull()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Does nothing.
 */
Xsj3cEvent
_Xsj3cNull(buf)
    Xsj3cBuf    buf;
{
    return (KEY_NULL|KEY_CONTROL);
}

/*
 * _Xsj3cIgnore()
 *
 * <NoInputMode/InputMode/ConvedMode/DictMode/SelectMode> Does nothing.
 */
Xsj3cEvent
_Xsj3cIgnore(buf)
    Xsj3cBuf    buf;
{
    return (KEY_NULL|KEY_CONTROL);
}
