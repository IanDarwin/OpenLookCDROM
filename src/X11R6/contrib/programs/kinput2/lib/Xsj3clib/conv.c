#ifndef lint
static char *rcsid = "$Header: conv.c,v 2.12 93/09/21 09:43:15 nao Exp $";
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
#include "segment.h"
#include "mode.h"
#include "func.h"

extern Xsj3cCVServerIF      serverIF[SERVER_NUM];

unsigned char              *Xsj3cGetPreeditArea();
Xsj3cEvent                  Xsj3cKeyConv();

int                         _Xsj3cRomaConv();
int                         _Xsj3cKanaConv();

static Xsj3cEvent           _Xsj3cStrConv();
static Xsj3cEvent           _Xsj3cCtrlConv();
static Xsj3cEvent           _Xsj3cThrough();
static Xsj3cEvent           _Xsj3cDirect();
static int                  _Xsj3cCodeConv();

/*
 *  Xsj3cGetPreeditArea()
 * Get area for pre-edit string.
 */
unsigned char *
Xsj3cGetPreeditArea(buf, len)
    register Xsj3cBuf       buf;
    register int           *len;
{
    register Xsj3cSeg       seg;

    if (buf->convmode == DictModeMask) {
        /* 辞書モードの場合は辞書読み用の入力を行う */
        seg = buf->dict->seg;
    } else if (buf->convmode != SelectModeMask) {
        if (buf->input[buf->curseg]
                && buf->input[buf->curseg]->status == SEG_CONVED) {
            /* 現文節が変換済みの場合                   */

            buf->convmode = ConvedModeMask;
            switch (buf->flushiconv) {
            case ON:
                /* FlushInConversion on の時は現在変換中の  */
                /* 文を確定して新規の文の入力を行う         */

                if (!buf->backup) {
                    if ((buf->backup = (Xsj3cSeg *)calloc(BUNBUFSIZ,
                            sizeof(Xsj3cSeg))) == NULL) {
                        Xsj3cError("Cannot allocate for backup buffers");
                    }
                }
                if (seg = buf->backup[0]) {
                    *seg->str = '\0';
                    seg->sp = seg->str;
                    *seg->oldstr = '\0';
                    seg->oldlen = 0;
                    seg->n_roma = 0;
                    seg->n_kana = -1;
                } else
                    seg = buf->backup[0] = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                break;
            case EDIT:
                /* FlushInConversion edit の時はカレント    */
                /* 文節への入力を行う                       */

                if (!(seg = buf->input[buf->curseg])) {
                    seg = buf->input[buf->curseg]
                            = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                } else {
                    *seg->str = '\0';
                    seg->sp = seg->str;
                    *seg->oldstr = '\0';
                    seg->oldlen = 0;
                    seg->n_roma = 0;
                    seg->n_kana = -1;
                }
                break;
            case OFF:
            case NONE:
            default:
                /* FlushInConversion none/off の場合、新たな*/
                /* 文節の読み文字列の入力を行う             */

                if (seg = buf->input[buf->segnum]) {
                    Xsj3cClearSegment(buf, buf->input[buf->segnum]);
                } else {
                    seg = buf->input[buf->segnum]
                            = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                }
                break;
            }
        } else if (buf->segnum > 0) {
            /* 現文節が未変換で入力文字がある場合   */

            buf->convmode = InputModeMask;
            if (buf->input[buf->curseg]
                    && (buf->input[buf->curseg]->edit & SEG_NOEDIT)) {
                /* 文字入力を許されていない文節の場合 */
                switch (buf->flushiconv) {
                case ON:
                    /* FlushInConversion on の時は現在変換中の  */
                    /* 文を確定して新規の文の入力を行う         */

                    if (!buf->backup) {
                        if ((buf->backup = (Xsj3cSeg *)calloc(BUNBUFSIZ,
                                sizeof(Xsj3cSeg))) == NULL) {
                            Xsj3cError("Cannot allocate for backup buffers");
                        }
                    }
                    if (seg = buf->backup[0]) {
                        *seg->str = '\0';
                        seg->sp = seg->str;
                        *seg->oldstr = '\0';
                        seg->oldlen = 0;
                        seg->n_roma = 0;
                        seg->n_kana = -1;
                    } else
                        seg = buf->backup[0]
                                = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                    break;
                case EDIT:
                    if (!(seg = buf->input[buf->curseg]))
                        seg = buf->input[buf->curseg]
                                = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                    break;
                case OFF:
                case NONE:
                default:
                    if (seg = buf->input[buf->segnum]) {
                        Xsj3cClearSegment(buf, buf->input[buf->segnum]);
                    } else 
                        seg = buf->input[buf->segnum]
                                = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                }
            } else {
                /* カレント文節への入力を行う               */

                if (!(seg = buf->input[buf->curseg]))
                    seg = buf->input[buf->curseg]
                            = (Xsj3cSeg)Xsj3cCreateSegment(buf);
            }
        } else {
            /* 入力文字がない場合   */

            buf->convmode = NoInputModeMask;
            /* New segment(first segment)   */
            if (seg = buf->input[buf->curseg])
                Xsj3cClearSegment(buf, buf->input[buf->curseg]);
            else 
                seg = buf->input[buf->curseg]
                        = (Xsj3cSeg)Xsj3cCreateSegment(buf);
        }
    } else {
        /* SelectMode   */
        switch (buf->selectstatus) {
        case SELECT_CAND:
            /* 候補選択の場合 */

            switch (buf->flushsconv) {
            case ON:
                /* FlushSelectConversion on の時は現在変換  */
                /* 中の文を確定して新規の文の入力を行う     */

                if (!buf->backup) {
                    if ((buf->backup = (Xsj3cSeg *)calloc(BUNBUFSIZ,
                            sizeof(Xsj3cSeg))) == NULL) {
                        Xsj3cError("Cannot allocate for backup buffers");
                    }
                } 
                if (seg = buf->backup[0]) {
                    *seg->str = '\0';
                    seg->sp = seg->str;
                    *seg->oldstr = '\0';
                    seg->oldlen = 0;
                    seg->n_roma = 0;
                    seg->n_kana = -1;
                } else {
                    seg = buf->backup[0] = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                }
                break;
            case EDIT:
                /* FlushSelectConversion edit の場合、無    */
                /* 変換状態にしてカレント文節への入力を行う */

                if (!(seg = buf->input[buf->curseg])) {
                    seg = buf->input[buf->curseg]
                            = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                } else {
                    *seg->str = '\0';
                    seg->sp = seg->str;
                    *seg->oldstr = '\0';
                    seg->oldlen = 0;
                    seg->n_roma = 0;
                    seg->n_kana = -1;
                }
                break;
            case OFF:
            case NONE:
            default:
                /* FlushSelectConversion none/off の場合、  */
                /* 新たな文節の読み文字列の入力を行う       */

                if (seg = buf->input[buf->segnum]) {
                    Xsj3cClearSegment(buf, buf->input[buf->segnum]);
                } else {
                    seg = buf->input[buf->segnum]
                            = (Xsj3cSeg)Xsj3cCreateSegment(buf);
                }
                break;
            }
            break;
        case SELECT_HINSI:
            /* 品詞選択の場合 */

            seg = buf->dict->seg;
            break;
        case SELECT_SYMBOL: 
        default: 
            /* 記号選択の場合 */

            if (seg = buf->input[buf->segnum]) {
                Xsj3cClearSegment(buf, buf->input[buf->segnum]);
            } else {
                seg = buf->input[buf->segnum]
                        = (Xsj3cSeg)Xsj3cCreateSegment(buf);
            }
            break;
        }
    }
    *len = RBUFSIZ - (seg->sp - seg->str);
    buf->current = seg;
    return (seg->sp);
}

#define IsKanaKey(ks)   ((unsigned)(ks) < 0xff00 && ((unsigned)(ks) & 0x0400))
#define IsLatin1Key(ks) ((unsigned)(ks) < 0x0100)

/*
 *  Xsj3cKeyConv()
 * Convert string or keysym and do function.
 */
Xsj3cEvent
Xsj3cKeyConv(buf, n, mod, ks)
    Xsj3cBuf                buf;
    int                     n;
    unsigned long           mod;
    KeySym                  ks;
{
    register Xsj3cKeyTable *keytp;
    Xsj3cSeg                seg = buf->current;
    Xsj3cEvent              ret = KEY_NULL;
    unsigned char          *tmpsp;
    register int            i,  doflg;

    /* KeySym/Modifier、現在の変換モードと各ファンクション  */
    /* の対応テーブルをサーチして合致するものがあればファン */
    /* クションを実行する。                                 */
    for (keytp = buf->key, doflg = 0; keytp != NULL; keytp = keytp->next) {
                                    /* KeySym */
        if (ks == keytp->ksym &&
                                    /* Modifier Key */
                (keytp->modmask & ~AllModeMask) == mod &&
                                    /* Conversion mode */
                (keytp->modmask & buf->convmode) == buf->convmode) {
                                    /* Do function  */
            doflg++;

            /* .key.func の第３フィールドが off の時はファンクション    */
            /* のみ実行し、none の時は .InputSameTime の指定に依存する  */
            /* InputSameTime off の時はファンクション実行する           */
            if (!buf->inputsame) {
                if (keytp->inputsame != ON)
                    goto dofunc;
            } else {
                if (!keytp->inputsame)
                    goto dofunc;
            }
            break;
        }
    }

    /* LookupString の結果は NULL terminate */
    /* していないので NULL terminate させる */
    tmpsp = seg->sp;
    i = n;
    while(i--) {
        if (iscntrl(*tmpsp)) {
            if (buf->throughflg == QUOTE) {
                ret |= KEY_CONTROL;
                tmpsp++;
                continue;
            }
            /* ControlSameTime off の場合は   */
            /* ファンクションのみ実行         */
            if (doflg && !buf->cntrlsame)
                goto dofunc;
            else {
                ret = _Xsj3cCtrlConv(buf, *tmpsp);
                if (buf->cntrlsame)
                    goto dofunc;
                else
                    return (ret);
            }
        }
        tmpsp++;
    }
    *tmpsp = '\0';

    if (!n || !(IsKanaKey(ks)||(IsLatin1Key(ks)))) {
        /* テキストに変換できなかった場合は */
        /* ファンクションのみ実行           */
        goto dofunc;
    }

    switch (buf->convmode) {
    case SelectModeMask:

        switch (buf->selectstatus) {
        case SELECT_CAND:
            /* 候補選択の場合 */

            switch (buf->flushsconv) {
            case ON:
                ret |= (KEY_SELECT_END|KEY_TEXT_CHANGE);
                for (i = 1; i < buf->backsegnum + 1; i++) {
                    Xsj3cFreeSegment(buf->backup[i]);
                    buf->backup[i] = NULL;
                }
                seg->yomi[0] = '\0';
                seg->disp[0] = '\0';
                seg->num = 0;
                seg->cur = 0;
                seg->dnum = 0;
                seg->status = SEG_NOCONV;
                seg->cursegmode = buf->inputmode;
                seg->change = OFF;
                seg->edit = SEG_NOEDIT;
                bzero(&seg->dcid, sizeof(seg->dcid));
                buf->backsegnum = 1;
                ret |= KEY_TEXT_FLUSH;
                if (buf->dispmodechange) {
                    buf->dispmode = buf->inputmode;
                    ret |= KEY_MODE_CHANGE;
                }
                break;
            case EDIT:
                ret |= (KEY_SELECT_END|KEY_TEXT_CHANGE);
                if (buf->curseg == buf->segnum)
                    buf->segnum++;
                else {
                    ret |= _Xsj3cUnConvSeg(buf, ONE, buf->editcurlast);
                    _Xsj3cStoreYomi(buf, buf->input[buf->curseg], 0);
                }
                break;
            case OFF:
                ret |= (KEY_SELECT_END|KEY_TEXT_CHANGE);
                if (buf->dispmodechange) {
                    buf->dispmode = buf->convedsegnum ? MODE_EDIT :
                            buf->inputmode;
                    ret |= KEY_MODE_CHANGE;
                }
                break;
            case NONE:
            default:
                goto dofunc;
            }
            break;
        case SELECT_HINSI:
            /* 品詞選択の場合 */

            ret |= (KEY_DICT_CHANGE|KEY_SELECT_ABORT);
            buf->dict->status = DICT_INPUT;
            if (buf->dispmodechange) {
                buf->dispmode =
                    (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
                ret |= KEY_MODE_CHANGE;
            }
            break;
        case SELECT_SYMBOL:
        default:
            /* 記号選択の場合 */
            goto dofunc;
        }
        break;
    case DictModeMask:
        if (buf->dict->status != DICT_INPUT) {
            /* 辞書登録／消去モードでよみ入力時でない   */
            /* ときはを返してなにもしない。       */
            goto dofunc;
        }
        ret |= KEY_DICT_CHANGE;
        seg->edit = SEG_EDIT;
        ret = _Xsj3cStrConv(buf, seg, ks, n, ret);
        _Xsj3cFlushDictMsg(buf);
        goto dofunc;
    case ConvedModeMask:
        ret |= KEY_TEXT_CHANGE;
        if (buf->candidate)
            Xsj3cEndCandidate(buf, ON);
        switch (buf->flushiconv) {
        case ON:
            for (i = 1; i < buf->backsegnum + 1; i++) {
                Xsj3cFreeSegment(buf->backup[i]);
                buf->backup[i] = NULL;
            }
            seg->yomi[0] = '\0';
            seg->disp[0] = '\0';
            seg->num = 0;
            seg->cur = 0;
            seg->dnum = 0;
            seg->status = SEG_NOCONV;
            seg->cursegmode = buf->inputmode;
            seg->change = OFF;
            seg->edit = SEG_NOEDIT;
            bzero(&seg->dcid, sizeof(seg->dcid));
            buf->backsegnum = 1;
            ret |= KEY_TEXT_FLUSH;
            if (buf->dispmodechange) {
                buf->dispmode = buf->inputmode;
                ret |= KEY_MODE_CHANGE;
            }
            break;
        case OFF:
            buf->curseg = buf->segnum;
            buf->segnum++;
            if (buf->dispmodechange) {
                buf->dispmode = 
                    (buf->convedsegnum ? MODE_EDIT : buf->inputmode);
                ret |= KEY_MODE_CHANGE;
            }
            break;
        case EDIT:
            if (buf->curseg == buf->segnum)
                buf->segnum++;
            else {
                ret |= _Xsj3cUnConvSeg(buf, ONE, buf->editcurlast);
                _Xsj3cStoreYomi(buf, seg, 0);
            }
            break;
        case NONE:
        default:
            goto dofunc;
        }
        break;
    case InputModeMask:
        ret |= KEY_TEXT_CHANGE;
        if (buf->input[buf->curseg]->edit & SEG_NOEDIT) {
            switch (buf->flushiconv) {
            case ON:
                for (i = 1; i < buf->backsegnum + 1; i++) {
                    Xsj3cFreeSegment(buf->backup[i]);
                    buf->backup[i] = NULL;
                }
                seg->yomi[0] = '\0';
                seg->disp[0] = '\0';
                seg->num = 0;
                seg->cur = 0;
                seg->dnum = 0;
                seg->status = SEG_NOCONV;
                seg->cursegmode = buf->inputmode;
                seg->change = OFF;
                seg->edit = SEG_NOEDIT;
                bzero(&seg->dcid, sizeof(seg->dcid));
                buf->backsegnum = 1;
                ret |= KEY_TEXT_FLUSH;
                break;
            case OFF:
                buf->curseg = buf->segnum;
                buf->segnum++;
                if (buf->dispmodechange) {
                    buf->dispmode = buf->convedsegnum ? MODE_EDIT :
                            buf->inputmode;
                    ret |= KEY_MODE_CHANGE;
                }
                break;
            case EDIT:
                if (buf->curseg == buf->segnum)
                    buf->segnum++;
                break;
            case NONE:
            default:
                goto dofunc;
            }
        } else if (buf->curseg == buf->segnum)
            buf->segnum++;
        break;
    case NoInputModeMask:
        if (buf->curseg == buf->segnum)
            buf->segnum++;
        if (buf->throughflg == THROUGH) {
            ret |= _Xsj3cThrough(buf, seg, n);
            buf->throughflg = OFF;
            goto dofunc;
        }
        ret |= KEY_TEXT_CHANGE;
        break;
    default:
        Xsj3cWarning ("Unknown conversion mode");
        goto dofunc;
    }

    seg->edit = SEG_EDIT;
    ret = _Xsj3cStrConv(buf, seg, ks, n, ret);

dofunc:
    if (doflg && !(ret & KEY_TEXT_FLUSH)) {
        ret |= ((*(keytp->func))(buf));
        if (!buf->cntrlsame)
            ret |= KEY_FUNC;
    }
    return (ret);
}

/*
 *  _Xsj3cStrConv()
 *   Convert pre-edit strings and store results to yomi/disp buffer.
 */
static Xsj3cEvent
_Xsj3cStrConv(buf, seg, ks, n, ret)
    Xsj3cBuf                buf;
    Xsj3cSeg                seg;
    KeySym                  ks;
    int                     n;
    Xsj3cEvent              ret;
{
    register unsigned char *tmpsp;
    unsigned char           tmp1[YBUFSIZ];
    unsigned char           tmp2[YBUFSIZ];
    unsigned char           kanabuf[YBUFSIZ];
    wchar                   wcs[RBUFSIZ];
    register int            i;
    int                     change_pos;
    int                     change_roma = 0;

    if (buf->throughflg == QUOTE) {
        ret |= _Xsj3cDirect(buf, seg, n);
        buf->throughflg = OFF;
        return (ret);
    }
    if (seg->num > seg->size - YBUFSIZ)
        Xsj3cResizeSegment(seg, seg->size + KANABUFSIZ);

    switch (buf->inputmode) {
    case MODE_HIRA:
    case MODE_ZKATA:
    case MODE_HKATA:
        /* ひらがな／全角カタカナ／半角カタカナ入力モードの場合 */
        if (IsKanaKey(ks) || (IsLatin1Key(ks) && buf->kanaonly)) {

            /* かな入力の場合 */
            int             len;

            if (seg->n_roma) {

                /* かなバッファ(seg->str)にローマ字入力時の  */
                /* 文字列が残っている場合はそれを削除する   */
                i = n;
                tmpsp = seg->sp;
                while (i--) {
                    *(tmpsp - seg->n_roma) = *tmpsp;
                    tmpsp++;
                }
                *(tmpsp - seg->n_roma) = '\0';
                seg->sp = seg->sp - seg->n_roma;
                seg->n_roma = 0;
            }
            if (buf->inputmode == MODE_HKATA) {

                /* 半角カタカナ入力モードのときは       */
                /* 変換を行わないで読みバッファにコピー */
                if (IsKanaKey(ks))
                    strcpy(tmp1, seg->sp);
                else
                    _Xsj3cHAlphaToHKata(buf, tmp1, seg->sp);
                seg->sp = seg->str;
                *seg->oldstr= '\0';
                seg->n_kana = -1;
                change_pos = seg->cur;
                _Xsj3cInsertChar(buf, seg, tmp1, strlen(tmp1));

            } else {
                /* ひらがな／全角カタカナ入力モードの時は半角   */
                /* カタカナ→ひらがな／全角カタカナ変換を行う   */
                if (IsLatin1Key(ks)) {
                    strcpy(tmp2, seg->sp);
                    _Xsj3cHAlphaToHKata(buf, seg->sp, tmp2);
                }
                if ((seg->value = _Xsj3cKanaConv(buf, seg, seg->str,
                        tmp1, buf->inputmode)) > 0) {

                    /* 半角カタカナ→ひらがな変換が成功した場合 */
                    /* 濁音／半濁音の場合                       */
                    seg->sp = seg->str;
                    if (seg->n_kana > 0) {
                        _Xsj3cExtractChar(buf, seg, tmp2, seg->n_kana);
                        strcpy(seg->oldstr, seg->str);
                        tmpsp = seg->oldstr;
                        while(*tmpsp != '\0')
                            tmpsp++;
                        *(tmpsp - seg->n_kana) = '\0';
                    } else {
                        *seg->oldstr = '\0';
                    }
                    seg->n_kana = 0;
                } else if (seg->value == 0) {

                    /* 半角カタカナ→ひらがな／全角カタカナ */
                    /* 変換の結果が不定の場合               */
                    if (seg->n_kana > 0)
                        _Xsj3cExtractChar(buf, seg, tmp2, seg->n_kana);
                    tmpsp = seg->sp + n - 1;
                    seg->sp = seg->str;
                    *seg->sp = *tmpsp;
                    *(++seg->sp) = '\0';
                    seg->n_kana = 1;
                    *seg->oldstr = '\0';
                } else {

                    /* 半角カタカナ→ひらがな変換が成功した場合 */
                    /* 濁音／半濁音でない場合                   */
                    if (seg->n_kana > 0)
                        _Xsj3cExtractChar(buf, seg, tmp2, seg->n_kana);
                    seg->sp = seg->str;
                    *seg->oldstr = '\0';
                    seg->n_kana = 0;
                }
                change_pos = seg->cur;
                len = _Xsj3cmPStowPS(buf, wcs, tmp1);
                _Xsj3cInsertWchar(seg, wcs, len);
            }
            seg->n_roma = 0;

        } else if (IsLatin1Key(ks)) {

            /* ローマ字入力の場合 */

            if (seg->n_kana > 0) {

                /* ローマ字バッファ(seg->str)にかな入力時の */
                /* 文字列が残っている場合はそれを削除する */
                i = n;
                tmpsp = seg->sp;
                while (i--) {
                    *(tmpsp - seg->n_kana) = *tmpsp;
                    tmpsp++;
                }
                *(tmpsp - seg->n_kana) = '\0';
                seg->sp = seg->sp - seg->n_kana;
                seg->n_kana = -1;
            }

            /* 入力モードに関係なくかな変換を行う  */

            if ((seg->value = _Xsj3cRomaConv(buf->rktable, 
                        seg->str, kanabuf)) > 0) {

                /* ROMA->かな 変換が成功した場合    */
                if (buf->alphaconv) {
                    /* AlphabetConversion リソースが on   */
                    /* だったら全角を半角に変換する     */

                    _Xsj3cExtractChar(buf, seg, tmp1, seg->n_roma);
                    _Xsj3cZAlphaToHAlpha(buf, seg->oldstr, tmp1);
                } else {
                    _Xsj3cExtractChar(buf, seg, seg->oldstr, seg->n_roma);
                }
                change_pos = seg->cur;
                if (buf->inputmode == MODE_HIRA) {

                    /* ひらがな入力モードのときはそのまま */
                    /* 読みバッファにコピー  */
                    seg->oldlen = _Xsj3cmPStowPS(buf, wcs, kanabuf);
                } else if (buf->inputmode == MODE_ZKATA) {

                    /* 全角カタカナ入力モードのときは */
                    /* ひらがなを全角カタカナに変換    */
                    _Xsj3cHiraToZKata(buf, tmp1, kanabuf);
                    seg->oldlen = _Xsj3cmPStowPS(buf, wcs, tmp1);
                } else {

                    /* 半角カタカナ入力モードの時はひらがな */
                    /* ／全角カタカナを半角カタカナに変換   */
                    _Xsj3cZKanaToHKata(buf, tmp1, kanabuf);
                    seg->oldlen = _Xsj3cmPStowPS(buf, wcs, tmp1);
                }
                _Xsj3cInsertWchar(seg, wcs, seg->oldlen);
                seg->n_roma = 0;

                /* 変換できない文字がローマ字バッファに残って   */
                /* いたら（例「っ」に変換される場合等）         */
                /* かなバッファの最後にその文字を付与する       */
                if (*seg->str != '\0') {
                    _Xsj3cInsertChar(buf, seg, seg->str, strlen(seg->str));
                    seg->sp = seg->str;
                    while (*seg->sp != '\0') {
                        seg->sp++;
                        seg->n_roma++;
                    }
                    change_roma = seg->n_roma;
                } else {
                    seg->sp = seg->str;
                }

            } else if (seg->value == 0) {

                /* ROMA→かな変換の結果が不定の場合 */
                change_pos = seg->cur;
                _Xsj3cInsertChar(buf, seg, seg->sp, n);
                i = n;
                while (i--) {
                    seg->sp++;
                    seg->n_roma++;
                }
                change_roma = n;

            } else {
                /* ROMA→かな 変換の結果が不成功の場合  */
                /* 最後の入力がローマかな変換の候補と   */
                /* して有効かどうか調べる               */

                change_pos = seg->cur;
                if ((seg->value = _Xsj3cRomaConv(buf->rktable, seg->sp,
                        kanabuf)) > 0) {
                    /* 最後の入力がローマかな変換の候補として   */
                    /* 有効な場合                               */

                    if (buf->inputmode == MODE_HIRA) {

                        /* ひらがな入力モードのときはそのまま */
                        /* 読みバッファにコピー  */
                        seg->oldlen = _Xsj3cmPStowPS(buf, wcs, kanabuf);
                    } else if (buf->inputmode == MODE_ZKATA) {

                        /* 全角カタカナ入力モードのときは */
                        /* ひらがなを全角カタカナに変換    */
                        _Xsj3cHiraToZKata(buf, tmp1, kanabuf);
                        seg->oldlen = _Xsj3cmPStowPS(buf, wcs, tmp1);
                    } else {

                        /* 半角カタカナ入力モードの時はひらがな */
                        /* ／全角カタカナを半角カタカナに変換   */
                        _Xsj3cZKanaToHKata(buf, tmp1, kanabuf);
                        seg->oldlen = _Xsj3cmPStowPS(buf, wcs, tmp1);
                    }
                    _Xsj3cInsertWchar(seg, wcs, seg->oldlen);
                    seg->n_roma = 0;

                    /* 変換できない文字がローマ字バッファに残って   */
                    /* いたら（例「っ」に変換される場合等）         */
                    /* かなバッファの最後にその文字を付与する       */
                    if (*seg->str != '\0') {
                        _Xsj3cInsertChar(buf, seg, seg->str, strlen(seg->str));
                        seg->sp = seg->str;
                        while (*seg->sp != '\0') {
                            seg->sp++;
                            seg->n_roma++;
                        }
                        change_roma = seg->n_roma;
                    } else {
                        seg->sp = seg->str;
                    }
                } else if (seg->value == 0) {
                    /* ローマかな変換の候補として未定の場合 */
                    /* 最後の入力文字をローマ→かな変換用の */
                    /* バッファ(seg->str)に残す             */

                    change_pos = seg->cur;
                    _Xsj3cInsertChar(buf, seg, seg->sp, n);
                    *seg->oldstr = '\0';
                    tmpsp = seg->str;
                    *seg->str = *seg->sp;
                    seg->sp = seg->str;
                    i = n;
                    while (i--)
                        *tmpsp++ = *seg->sp++;
                    *tmpsp = '\0';
                    seg->n_roma = n;
                    change_roma = seg->n_roma;
                } else {
                    /* ローマかな変換の候補として無効な場合 */

                    _Xsj3cInsertChar(buf, seg, seg->sp, n);
                    *seg->oldstr = '\0';
                    seg->n_roma = 0;
                    *seg->str = '\0';
                    seg->sp = seg->str;

                    /* rkbell リソースが on に設定されて    */
                    /* いる場合ベルを鳴らす                 */
                    if (buf->rkbell)
                        ret |= KEY_BELL;
                    change_roma = n;
                }
            }

            /* AlphabetConversion リソースが on     */
            /* だったら半角ローマ字を全角ローマ字に変換する */
            if (buf->alphaconv && buf->inputmode != MODE_HKATA
                    && change_roma) {
                _Xsj3cExtractChar(buf, seg, tmp1, change_roma);
                _Xsj3cHAlphaToZKana(buf, tmp2, tmp1);
                _Xsj3cInsertChar(buf, seg, tmp2, change_roma); 
            } 
            /* かな入力モードでないときは -1  */
            seg->n_kana = -1;
        } else {
            return (KEY_NULL);
        }
        break;

    case MODE_HALPHA:

        /* 半角アルファベット 入力モードの場合 */
        if (IsKanaKey(ks)) 
            return (KEY_BELL);
        change_pos = seg->cur;
        _Xsj3cInsertChar(buf, seg, seg->sp, n); 
        seg->sp = seg->str;
        seg->n_roma = 0;
        seg->n_kana = -1;
        break;

    case MODE_ZALPHA:

        /* 全角アルファベット 入力モードの場合 */
        if (IsKanaKey(ks)) 
            return (KEY_BELL);
        _Xsj3cHAlphaToZAlpha(buf, tmp1, seg->sp);
        seg->sp = seg->str;
        change_pos = seg->cur;
        _Xsj3cInsertChar(buf, seg, tmp1, n); 
        seg->n_roma = 0;
        seg->n_kana = -1;
        break;

    case MODE_SJIS:
    case MODE_EUC:
    case MODE_JIS:
    case MODE_KUTEN:

        /* コード入力モードの場合 */
        tmpsp = seg->str;
        while (*tmpsp != '\0') {
            if (!isxdigit(*tmpsp)
                || (buf->inputmode == MODE_KUTEN && !isdigit(*tmpsp)))
                return (KEY_BELL);
            tmpsp++;
        }

        /* コード→ひらがな／全角カタカナ／記号／漢字の変換を行う */
        if ((seg->value = _Xsj3cCodeConv(buf, seg->str, 
                kanabuf, buf->inputmode)) > 0) {

            /* コード変換に成功した場合 */
            if (buf->alphaconv) {
                /* AlphabetConversion リソースが on だったら */
                /* 全角数字を半角数字に変換する              */
                _Xsj3cExtractChar(buf, seg, tmp1, seg->n_roma);
                _Xsj3cZAlphaToHAlpha(buf, seg->oldstr, tmp1);
            } else {
                _Xsj3cExtractChar(buf, seg, seg->oldstr, seg->n_roma);
            }
            change_pos = seg->cur;
            seg->oldlen = (strlen(kanabuf) + 1)/ sizeof(wchar);
            _Xsj3cInsertChar(buf, seg, kanabuf, seg->oldlen);
            seg->cursegmode = MODE_HIRA;
            seg->n_roma = 0;
            seg->sp = seg->str;
        } else if (seg->value == 0) {

            /* コード変換の結果が不定の場合 */
            change_pos = seg->cur;
            if (buf->alphaconv) {
                /* AlphabetConversion リソースが on だったら */
                /* 半角数字を全角数字に変換する              */
                _Xsj3cHAlphaToZKana(buf, tmp1, seg->sp);
                _Xsj3cInsertChar(buf, seg, tmp1, n); 
            } else {
                _Xsj3cInsertChar(buf, seg, seg->sp, n);
            }
            i = n;
            while (i--) {
                seg->n_roma++;
                seg->sp++;
            }
        } else {

            /* コード変換の結果が失敗の場合 */
            _Xsj3cExtractChar(buf, seg, tmp1, seg->n_roma);
            change_pos = seg->cur;
            seg->sp = seg->str;
            seg->n_roma = 0;
            if (seg->num == 0 && buf->convmode != DictModeMask) {
                Xsj3cFreeSegment(seg);
                seg = NULL;
                buf->segnum--;
                for (i = buf->curseg; i < buf->segnum; i++) {
                    buf->input[i] = buf->input[i + 1];
                }
                buf->input[buf->segnum] = NULL;
                return ret;
            }
            if (buf->rkbell)
                ret |= KEY_BELL;
        }
        seg->n_kana = -1;
        break;

    default:
        Xsj3cWarning("Illegal current mode");
        return (KEY_BELL);
    }

    /* 表示用文字列へコピーする        */
    _Xsj3cStoreYomi(buf, seg, change_pos);
    return ret;
}

/*
 * _Xsj3cKanaConv()
 *  Kana input mode dispatch routine.
 */
int
_Xsj3cKanaConv(buf, seg, hkana, zkana, mode)
    Xsj3cBuf        buf;
    Xsj3cSeg        seg;
    unsigned char  *hkana, *zkana;
    Xsj3csMode      mode;
{
    register int             len,   zlen;
    register wchar           s;

    if (mode == MODE_HIRA) {
        if (buf->alphaconv)
            _Xsj3cHankakuToHira(buf, zkana, hkana);
        else
            _Xsj3cHKataToHira(buf, zkana, hkana);
    } else {
        if (buf->alphaconv)
            _Xsj3cHankakuToZKata(buf, zkana, hkana);
        else
            _Xsj3cHKataToZKata(buf, zkana, hkana);
    }
    len = 0;
    while (*hkana != '\0') {
        hkana++;
        len++;
    }
    if (isdakuon(*(hkana - 1))) {
        return 0;
    } else {
        zlen = 0;
        while (*zkana != '\0') {
            zkana++;
            zlen++;
        }
        if (zlen > 1) {
            s = (*(zkana - 2) << 8) + (*(zkana - 1) & 0xff);
            if (seg->n_kana && (iszdakuten(s, serverIF[buf->server].lang)
                    || !isdakuten(*(hkana - 1))))
                return -1; 
            else 
                return len; 
        } else {
            return -1; 
        }
    }
}

/*
 * _Xsj3cCodeConv()
 *  Code input mode dispatch routine.
 */
static int
_Xsj3cCodeConv(buf, code, kanji, mode)
    Xsj3cBuf                    buf;
    register unsigned char     *code;
    register unsigned char     *kanji;
    Xsj3csMode                  mode;
{
    register int            i,  j,  len;
    register wchar          k,  (*conv)();
    register unsigned char  c1, c2;
    unsigned char           kbuf[4];

    if ((len = strlen(code)) < 4 && (mode == MODE_EUC || mode == MODE_KUTEN))
        return CONV_UNFIXED;
    else if ((len < 2 || len == 3) && (mode == MODE_SJIS || mode == MODE_JIS))
        return CONV_UNFIXED;

    k = 0;  
    for (j = 0; j < 4 && *code != '\0'; code++, j++) {
        if (isdigit(*code))
            i = *code - '0';
        else if (islower(*code))
            i = 10 + *code - 'a';
        else if (isupper(*code))
            i = 10 + *code - 'A';
        else
            return CONV_FAILED;
        kbuf[j] = i;
        k += i << ((3 - j) * 4);
    }
    switch (mode) {
    case MODE_SJIS:
        if (len == 4 && issjis1(c1 = (k >> 8)) && issjis2(c2 = (k & 0xff))) {
            if (conv = CodeConvFunc[JP_SJIS][serverIF[buf->server].lang]) {
                k = conv(k);
                *kanji++ = k >> 8;
                *kanji++ = k & 0xff;
                *kanji = '\0';
            } else {
                *kanji++ = c1;
                *kanji++ = c2;
                *kanji = '\0';
            }
        } else if (iskana(k >> 8)) {
            *kanji++ = k >> 8;
            *kanji = '\0';
        } else {
            if (len < 4)
                return CONV_UNFIXED;
            else 
                return CONV_FAILED;
        }
        break;
    case MODE_EUC:
        if (iseuc(k >> 8) && iseuc(k & 0xff)) {
            if (conv = CodeConvFunc[JP_EUC][serverIF[buf->server].lang])
                k = conv(k);
            *kanji++ = k >> 8;
            *kanji++ = k & 0xff;
            *kanji = '\0';
        } else if (iseuckana(k >> 8) && iskana2(k & 0xff)) {
            *kanji++ = k & 0xff;
            *kanji = '\0';
        } else {
            return CONV_FAILED;
        }
        break;
    case MODE_JIS:
        if (len == 4 && isjis(c1 = (k >> 8)) && isjis(c2 = (k & 0xff))) {
            if (conv = CodeConvFunc[JP_JIS8][serverIF[buf->server].lang]) {
                k = conv(k);
                *kanji++ = k >> 8;
                *kanji++ = k & 0xff;
                *kanji = '\0';
            } else {
                *kanji++ = c1;
                *kanji++ = c2;
                *kanji = '\0';
            }
        } else if (iskana(k >> 8)) {
            *kanji++ = k >> 8;
            *kanji = '\0';
        } else {
            if (len < 4)
                return CONV_UNFIXED;
            else 
                return CONV_FAILED;
        }
        break;
    case MODE_KUTEN:
        conv = CodeConvFunc[JP_JIS8][serverIF[buf->server].lang];
        c1 = kbuf[0] * 10 + kbuf[1];
        c2 = kbuf[2] * 10 + kbuf[3];
        k = (c1 << 8) + c2;
        k = conv(k + 0x2020);
        if (iskan1(c1 = (k >> 8), serverIF[buf->server].lang)
                && iskan2(c2 = (k & 0xff), serverIF[buf->server].lang)) {
            *kanji++ = c1;
            *kanji++ = c2;
            *kanji = '\0';
        } else {
            return CONV_FAILED;
        }
        break;
    default:
        Xsj3cWarning("Illegal mode");
        return CONV_FAILED;
    }   
        
    return CONV_FIXED;
}

#define MATCH           0
#define NOT             1

/*
 *  _Xsj3cRomaConv()
 *   Get roman characters from the second argument and put back hiragana or
 *  katakana characters to the third argument in conversion table.
 *   If roman characters (third argument) mach with the "roma" entry in table, 
 *  put back the "str" entry to the third argument and the "yomi" entry
 *  to the third argument, then it returns number of converted roman
 *  characters.
 *   If not but any letters of roman characters is match with the "roma"
 *  entry in table and the number of roman characters is less than
 *  the number of the letters of "roma" entry, put back the same to
 *  the third argument and nothing to the third argument, then it returns
 *  zero.
 *   Then last (not converted case) it returns minus 1.
 *   Any arguments must be null('\0') terminated.
 *   First argument is pointer to conversion table;
 */ 
int
_Xsj3cRomaConv(rktable, roma, yomi)
    Xsj3cRKTable               *rktable;
    register unsigned char     *roma;
    register unsigned char     *yomi;
{
    Xsj3cRKTable               *rktp;
    register unsigned char     *p,     *q;
    unsigned char               tmp[RBUFSIZ];
    int                         match, result = CONV_FAILED;                    
    register int                len;

    if (!roma || (len = strlen(roma)) == 0)
        return (CONV_FAILED);
    for (rktp = rktable; rktp != NULL; rktp = rktp->next) {
        if (!rktp->roma) {
            continue;
        }
        p = roma;
        q = rktp->roma;
        if (len > rktp->rlen) {
            if (*q++ == *p++) {
                match = MATCH;
                while (*q != '\0') {
                    if (*q != *p) {
                        match = NOT;
                        break;
                    }
                    q++;
                    p++;
                }
                if (match == NOT) {
                    continue;
                }
                if (result < 0) {
                    result = rktp->rlen;
                    strcpy(yomi, rktp->yomi);
                    strcpy(tmp, p);
                } else {
                    continue;
                }
            } else {
                continue;
            }
        } else {
            if (*p++ == *q++) {
                match = MATCH;
                while (*p != '\0') {
                    if (*p++ != *q++) {
                        match = NOT;
                        break;
                    }
                }
                if (match == NOT) {
                    continue;
                }
                if (*q != '\0') {
                    result = CONV_UNFIXED;
                    continue;
                } else if (result == CONV_UNFIXED) {
                    continue;
                } else {
                    result = rktp->rlen;
                    strcpy(yomi, rktp->yomi);
                    strcpy(tmp, rktp->str);
                }
            } else {
                continue;
            }
        }
    }
    if (result > 0)
        strcpy(roma, tmp);
    return (result);
}

/*
 * _Xsj3cCtrlConv()
 *
 * <NoInputMode> Through all control events.
 * <InputMode> Put the control code (now only HT & NL) after current position,
 *   then fix all segments.
 * <ConvedMode> Put the control code (now only HT & NL) after current segment,
 *   then fix all segments and end convertion.
 * <SelectMode/DictMode> Does Nothing.
 *
 * DisplayModeChange on: Change the display mode string.  
 * ThroughNext code: 
 * SetNormal code: 
 */
static Xsj3cEvent
_Xsj3cCtrlConv(buf, code)
    Xsj3cBuf                buf;
    unsigned char           code;
{
    unsigned char           ch[2];
    register unsigned char *c;
    int                     change_pos;
    register int            i;

    if (code == '\t' || code == '\n')
        goto compound_text;
    if (buf->throughnext) {
        c = buf->throughnext;
        while (*c != '\0') {
            if (*c == code) {
                buf->throughflg = THROUGH;
                if (buf->dispmodechange) {
                    buf->dispmode = buf->inputmode;
#ifdef THROUGH_CONT
                    return (KEY_TEXT_CLEAR|KEY_CONTROL|KEY_MODE_CHANGE);
                } else {
                    return (KEY_TEXT_CLEAR|KEY_CONTROL);
#else /* THROUGH_CONT */
                    return (KEY_NULL);
                } else {
                    return (KEY_NULL);
#endif /* THROUGH_CONT */
                }
            }
            c++;
        }
    }
    if (buf->setnormal) {
        c = buf->setnormal;
        while (*c != '\0') {
            if (*c == code) {
                if (buf->dispmodechange) {
                    buf->dispmode = buf->inputmode;
#ifdef THROUGH_CONT
                    return (KEY_TEXT_FIXED|KEY_CONTROL|KEY_HENKAN_END
                            |KEY_MODE_CHANGE);
                } else {
                    return (KEY_TEXT_FIXED|KEY_CONTROL|KEY_HENKAN_END);
#else /* THROUGH_CONT */
                    return (KEY_NULL);
                } else {
                    return (KEY_NULL);
#endif /* THROUGH_CONT */
                }
            }
            c++;
        }
    }
    /* Through all control events by XtNunusedEventCallback */
    return (KEY_NULL);

    /* Push by compound text */
compound_text:
    switch (buf->convmode) {
    case NoInputModeMask:
        buf->convmode = InputModeMask;
    case InputModeMask:
        if (buf->segnum == buf->curseg)
            buf->segnum++;
        ch[0] = code;
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
        buf->input[buf->curseg]->disp[i++] = code;
        buf->input[buf->curseg]->disp[i] = '\0';
        buf->input[buf->curseg]->dnum++;
        if (buf->dispmodechange) {
            buf->dispmode = buf->inputmode;
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE|KEY_MODE_CHANGE);
        } else
            return (KEY_TEXT_FIXED|KEY_TEXT_CHANGE);
    case DictModeMask:
    case SelectModeMask:
    default:
        return (KEY_NULL);
    }
}

/*
 *  _Xsj3cThrough()
 * Store characters to yomi/disp buffer and fix.
 */
static Xsj3cEvent
_Xsj3cThrough(buf, seg, n)
    Xsj3cBuf                buf;
    Xsj3cSeg                seg;
    int                     n;
{
    int                     change_pos;

    change_pos = seg->cur;
    _Xsj3cInsertChar(buf, seg, seg->str, n);
    _Xsj3cStoreYomi(buf, seg, change_pos);
    return (KEY_TEXT_FIXED);
}

/*
 *  _Xsj3cDirect()
 * Store characters to yomi/disp buffer.
 *
 * DisplayModeChange on: Change the display mode string.  
 */
static Xsj3cEvent
_Xsj3cDirect(buf, seg, n)
    Xsj3cBuf                buf;
    Xsj3cSeg                seg;
    int                     n;
{
    Xsj3cEvent              ret = KEY_TEXT_CHANGE;
    int                     change_pos;

    change_pos = seg->cur;
    _Xsj3cInsertChar(buf, seg, seg->str, n);
    _Xsj3cStoreYomi(buf, seg, change_pos);
    *seg->str = '\0';
    seg->sp = seg->str;
    *seg->oldstr = '\0';
    seg->oldlen = 0;
    seg->n_roma = 0;
    seg->n_kana = -1;
    if (buf->dispmodechange) {
        buf->dispmode = (buf->convedsegnum == buf->segnum ? MODE_KANJI :
                (buf->convedsegnum ? MODE_EDIT : buf->inputmode));
        ret |= KEY_MODE_CHANGE;
    }
    return (ret);
}
