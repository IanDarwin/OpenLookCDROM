#ifndef lint
static char *rcsid = "$Header: dict.c,v 2.0 92/02/13 18:33:19 nao Exp $";
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

#include "common.h"
#include "util.h"
#include "segment.h"

extern Xsj3cCVServerIF      serverIF[SERVER_NUM];
extern wchar               *_Xsj3cStoreWchar();

void                        _Xsj3cFlushDictMsg();
Xsj3cDictData               _Xsj3cCreateDictData();
void                        _Xsj3cFreeDictData();

int                         Xsj3cGetDictMsgNum();
Xsj3cDictMsg                Xsj3cGetDictMsgs();
wchar                      *Xsj3cGetDictMsg();
void                        Xsj3cDictRegister();
void                        Xsj3cDictClear();
void                        Xsj3cEndDict();

Xsj3cHinsi                  _Xsj3cHinsiInit();
int                         Xsj3cGetHinsiNum();
Xsj3cHinsi                  Xsj3cGetHinsis();
wchar                      *Xsj3cGetHinsi();
int                         Xsj3cSetHinsi();
void                        Xsj3cEndHinsi();

static Xsj3cDictMsg         _Xsj3cDictMsgInit();
static void                 _Xsj3cSetDictMsg();

static Xsj3cDictMsg         dictmsglist[SERVER_NUM] = {NULL};
static int                  dictmsgnum[SERVER_NUM] = {0};

static Xsj3cHinsi           hinsilist[SERVER_NUM] = {NULL};
static int                  hinsinum[SERVER_NUM] = {0};

typedef struct _dict_hinsi {
    int             code;
    char           *string;
} Sj3HinsiRec, *Sj3HinsiPtr;

static Sj3HinsiRec sj3_hinsi_list[] = {
    SJ3_H_NRMNOUN,      "\225\201\222\312\226\274\216\214",
    SJ3_H_PRONOUN,      "\221\343\226\274\216\214",
    SJ3_H_LNAME,        "\225\143\216\232",
    SJ3_H_FNAME,        "\226\274\221\117",
    SJ3_H_LOCNAME,      "\222\156\226\274",
    SJ3_H_PREFIC,       "\214\247\201\136\213\346\226\274",
    SJ3_H_RENTAI,       "\230\101\221\314\216\214",
    SJ3_H_CONJUNC,      "\220\332\221\261\216\214",
    SJ3_H_SUBNUM,       "\217\225\220\224\216\214",
    SJ3_H_NUMERAL,      "\220\224\216\214",
    SJ3_H_PREFIX,       "\220\332\223\252\214\352",
    SJ3_H_POSTFIX,      "\220\332\224\366\214\352",
    SJ3_H_ADVERB,       "\225\233\216\214",
    SJ3_H_ADJECT,       "\214\140\227\145\216\214",
    SJ3_H_ADJVERB,      "\214\140\227\145\223\256\216\214",
    SJ3_H_SILVERB,      "\203\124\225\317\223\256\216\214",
    SJ3_H_ZILVERB,      "\203\125\225\317\223\256\216\214",
    SJ3_H_ONEVERB,      "\210\352\222\151\223\256\216\214",
    SJ3_H_KAVERB,       "\203\112\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_GAVERB,       "\203\113\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_SAVERB,       "\203\124\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_TAVERB,       "\203\136\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_NAVERB,       "\203\151\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_BAVERB,       "\203\157\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_MAVERB,       "\203\175\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_RAVERB,       "\203\211\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_WAVERB,       "\203\217\215\163\214\334\222\151\223\256\216\214",
    SJ3_H_SINGLE,       "\222\120\212\277\216\232",
};

typedef struct _dict_status {
    Xsj3cdMode      state;
    Xsj3cdMode      value;
    char            *string;
} Sj3DictStatusRec, *Sj3DictStatusPtr;

static Sj3DictStatusRec sj3_dict_status[] = {
    REG_STATE|CLR_STATE,    SJ3_DICT_INPUT,
    " \224\315\210\315\216\167\222\350/\223\307\202\335\223\374\227\
\315:[\223\307\202\335]",
    /* 範囲指定/読み入力:[読み] */
    REG_STATE|CLR_STATE,    SJ3_DICT_YOMI,
    " [\223\307\202\335]",
    /* [読み] */
    REG_STATE|CLR_STATE,    SJ3_DICT_HINSI,
    " [\225\151\216\214]",
    /* [品詞] */
    REG_STATE|CLR_STATE,    SJ3_DICT_CONFIRM,
    " [ok ?]",
    /* [ok ?] */
    REG_STATE,              SJ3_TOUROKU_SUCCESSED,
    ":\223\157\230\136\202\265\202\334\202\265\202\275",
    /* 登録しました */
    CLR_STATE,              SJ3_SYOUKYO_SUCCESSED,
    ":\217\301\213\216\202\265\202\334\202\265\202\275",
    /* 消去しました */
    REG_STATE|CLR_STATE,    SJ3_NO_YOMI_STR,
    ":\223\307\202\335\225\266\216\232\227\361\202\252\202\240\202\350\
\202\334\202\271\202\361",
    /* 読み文字列がありません */
    REG_STATE|CLR_STATE,    SJ3_LONG_YOMI_STR,
    ":\223\307\202\335\225\266\216\232\227\361\202\252\222\267\202\267\
\202\254\202\334\202\267",
    /* 読み文字列が長すぎます */
    REG_STATE|CLR_STATE,    SJ3_DICT_ERROR,
    ":\216\253\217\221\202\326\202\314\217\221\202\253\215\236\202\335\
\202\252\217\157\227\210\202\334\202\271\202\361",
    /* 辞書への書き込みが出来ません */
    REG_STATE|CLR_STATE,    SJ3_DICT_LOCKED,
    ":\216\253\217\221\202\252\203\215\203\142\203\116\202\263\202\352\
\202\304\202\242\202\334\202\267",
    /* 辞書がロックされています */
    REG_STATE|CLR_STATE,    SJ3_BAD_YOMI_STR,
    ":\223\307\202\335\225\266\216\232\227\361\202\311\225\163\220\263\
\202\310\225\266\216\232\202\252\212\334\202\334\202\352\202\304\202\242\
\202\334\202\267",
    /* 読み文字列に不正な文字が含まれています */
    REG_STATE|CLR_STATE,    SJ3_BAD_KANJI_STR,
    ":\212\277\216\232\225\266\216\232\227\361\202\311\225\163\220\263\
\202\310\225\266\216\232\202\252\212\334\202\334\202\352\202\304\202\242\
\202\351\202\251\222\267\202\267\202\254\202\334\202\267",
    /* 漢字文字列に不正な文字が含まれているか長すぎます */
    REG_STATE|CLR_STATE,    SJ3_BAD_HINSI_CODE,
    ":\225\151\216\214\203\122\201\133\203\150\202\252\225\163\220\263\
\202\305\202\267",
    /* 品詞コードが不正です */
    REG_STATE,              SJ3_WORD_EXIST,
    ":\223\157\230\136\215\317\202\314\217\156\214\352\202\305\202\267",
    /* 登録済の熟語です */
    CLR_STATE,              SJ3_WORD_NOT_EXIST,
    ":\217\156\214\352\202\252\223\157\230\136\202\263\202\352\202\304\
\202\242\202\334\202\271\202\361",
    /* 熟語が登録されていません */
    REG_STATE,              SJ3_DOUON_FULL,
    ":\202\261\202\352\210\310\217\343\223\257\211\271\214\352\202\360\
\223\157\230\136\202\305\202\253\202\334\202\271\202\361",
    /* これ以上同音語を登録できません */
    REG_STATE,              SJ3_DICT_FULL,
    ":\202\261\202\352\210\310\217\343\216\253\217\221\202\311\223\157\
\230\136\202\305\202\253\202\334\202\271\202\361",
    /* これ以上辞書に登録できません */
    REG_STATE,              SJ3_INDEX_FULL,
    ":\202\261\202\352\210\310\217\343 INDEX \202\311\223\157\230\136\202\
\305\202\253\202\334\202\271\202\361",
    /* これ以上 INDEX に登録できません */
    REG_STATE,              SJ3_TOUROKU_FAILED,
    ":\216\253\217\221\202\326\202\314\223\157\230\136\202\311\216\270\
\224\163\202\265\202\334\202\265\202\275",
    /* 辞書への登録に失敗しました */
    CLR_STATE,              SJ3_SYOUKYO_FAILED,
    ":\216\253\217\221\202\251\202\347\202\314\217\301\213\216\202\311\
\216\270\224\163\202\265\202\334\202\265\202\275",
    /* 辞書からの消去に失敗しました */
};

/*
 * _Xsj3cCreateDictData()
 */
Xsj3cDictData
_Xsj3cCreateDictData(buf, mode)
    Xsj3cBuf            buf;
    Xsj3cdMode          mode;
{
    register Xsj3cDictData  dict;

    if (buf->dict)
        return buf->dict;
    if ((dict = (Xsj3cDictData)malloc(sizeof(Xsj3cDictDataRec))) == NULL)
        Xsj3cError("Failed to allocate data for DictMode.");
    if ((dict->seg = (Xsj3cSeg)Xsj3cCreateSegment(buf)) == NULL)
        Xsj3cError("Failed to allocate segment for dictionary.");
    if ((dict->msg = (Xsj3cDictMsg)
            calloc(DICT_STR_NUM, sizeof(Xsj3cDictMsgRec))) == NULL)
        Xsj3cError("Failed to allocate message buffer for dictionary.");
    dict->mode = mode;
    dict->status = DICT_INPUT;
    dict->value = SJ3_DICT_INPUT;
    dict->n_dict = 0;
    return (dict);
}

/*
 * _Xsj3cFreeDictData()
 */
void
_Xsj3cFreeDictData(buf)
    Xsj3cBuf            buf;
{
    if (!buf->dict)
        return ;
    Xsj3cFreeSegment(buf->dict->seg);
    buf->dict->seg = NULL;
    free (buf->dict->msg);
    free (buf->dict);
    buf->dict = NULL;
}

/*
 * _Xsj3cDictMsgInit()
 */
static Xsj3cDictMsg
_Xsj3cDictMsgInit(buf)
    Xsj3cBuf            buf;
{
    wchar               data[KANABUFSIZ];
    register int        i;

    if (dictmsglist[buf->server]) {
        return (dictmsglist[buf->server]);
    }
    switch(buf->server) {
    case SERVER_SJ3:
    default:
        dictmsgnum[SERVER_SJ3]
                = sizeof(sj3_dict_status)/sizeof(Sj3DictStatusRec);
        break;
    }
    if ((dictmsglist[buf->server] =
            (Xsj3cDictMsg)calloc(dictmsgnum[buf->server],
            sizeof(Sj3DictStatusRec))) == NULL) {
        Xsj3cError("Failed to allocate hinsi data");
    }
    switch(buf->server) {
    case SERVER_SJ3:
    default:
        for (i = 0; i < dictmsgnum[SERVER_SJ3]; i++) {
            dictmsglist[buf->server][i].len = _Xsj3cmPStowOUT(buf, data,
                    (unsigned char *)sj3_dict_status[i].string);
            dictmsglist[buf->server][i].data
                = _Xsj3cStoreWchar(data, dictmsglist[buf->server][i].len + 1);
            dictmsglist[buf->server][i].attr = SEG_NORMAL;
            if (!dictmsglist[buf->server][i].data)
                Xsj3cError("Failed to allocate hinsi data");
        }
        break;
    }
    return (dictmsglist[buf->server]);
}

/*
 * Xsj3cGetDictMsgNum()
 */
int
Xsj3cGetDictMsgNum(buf)
    Xsj3cBuf      buf;
{
    switch (buf->dict->status) {
    case DICT_INPUT:
    case DICT_HINSI:
        if (buf->dict->seg->num > 0)
            return (DICT_STR_YOMI + 1);
        else 
            return (DICT_STR_MSG1 + 1);
    case DICT_CONFIRM:
        if (buf->dict->mode == REG_STATE)
            return (DICT_STR_NUM);
        else 
            return (DICT_STR_MSG2 + 1);
    case DICT_END:
        if (buf->dict->seg->num > 0) {
            if (buf->curhinsi < 0) 
                return (DICT_STR_MSG2 + 1);
            else 
                return (DICT_STR_NUM);
        } else {
            return (DICT_STR_MSG1 + 1);
        }
    default:
        return (NULL);
    }
}

/*
 * _Xsj3cFlushDictMsg()
 */
void
_Xsj3cFlushDictMsg(buf)
    Xsj3cBuf      buf;
{
    if (!dictmsglist[buf->server]) {
        dictmsglist[buf->server] = _Xsj3cDictMsgInit(buf);
    }

    switch (buf->dict->status) {
    case DICT_INPUT:
    case DICT_HINSI:
        if (buf->dict->seg->num > 0) {
            _Xsj3cSetDictMsg(buf, DICT_STR_YOMI);
        } else {
            _Xsj3cSetDictMsg(buf, DICT_STR_MODE);
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG1);
        }
        break;
    case DICT_CONFIRM:
        if (buf->dict->mode == REG_STATE) {
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG1);
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG2);
            _Xsj3cSetDictMsg(buf, DICT_STR_HINSI);
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG3);
        } else {
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG1);
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG2);
        }
        break;
    case DICT_END:
        if (buf->dict->seg->num > 0) {
            if (buf->dict->seg->num > DICT_YOMI_MAX) {
                _Xsj3cSetDictMsg(buf, DICT_STR_MSG2);
            } else {
                if (buf->dict->mode == REG_STATE) {
                    _Xsj3cSetDictMsg(buf, DICT_STR_MSG1);
                    _Xsj3cSetDictMsg(buf, DICT_STR_MSG3);
                } else {
                    if (buf->curhinsi < 0) {
                        _Xsj3cSetDictMsg(buf, DICT_STR_MSG2);
                    } else {
                        _Xsj3cSetDictMsg(buf, DICT_STR_MSG1);
                        _Xsj3cSetDictMsg(buf, DICT_STR_MSG2);
                        _Xsj3cSetDictMsg(buf, DICT_STR_HINSI);
                        _Xsj3cSetDictMsg(buf, DICT_STR_MSG3);
                    }
                }
            }
        } else {
            _Xsj3cSetDictMsg(buf, DICT_STR_MSG1);
        }
        break;
    default:
        break;
    }
}

/*
 * Xsj3cGetDictMsgs()
 */
Xsj3cDictMsg
Xsj3cGetDictMsgs(buf)
    Xsj3cBuf    buf;
{
    return (buf->dict->msg);
}

/*
 * Xsj3cGetDictMsg()
 */
wchar *
Xsj3cGetDictMsg(buf, n, len, attr)
    Xsj3cBuf    buf;
    int         n;
    int         *len;
    int         *attr;
{
    *attr = buf->dict->msg[n].attr;
    *len = buf->dict->msg[n].len;
    return (buf->dict->msg[n].data);
}

static void
_Xsj3cSetDictMsg(buf, n)
    Xsj3cBuf    buf;
    Xsj3cdMode  n;
{
    Xsj3csMode      mode;
    register int    i;
    int             value;

    switch (n) {
    case DICT_STR_MODE:
        mode = (buf->dict->mode == REG_STATE ? MODE_TOROKU : MODE_SYOUKYO);
        buf->dict->msg[DICT_STR_MODE].data = buf->modestr[mode];
        buf->dict->msg[DICT_STR_MODE].len = buf->modelen[mode];
        buf->dict->msg[DICT_STR_MODE].attr = SEG_REVERSED;
        return;
    case DICT_STR_MSG1:
        if (buf->dict->status == DICT_INPUT) {
            value = SJ3_DICT_INPUT;
        } else {
            if (buf->dict->seg->num > 0) 
                value = SJ3_DICT_YOMI;
            else
                value = buf->dict->value;
        }
        break;
    case DICT_STR_YOMI:
        buf->dict->msg[DICT_STR_YOMI].data = buf->dict->seg->disp;
        buf->dict->msg[DICT_STR_YOMI].len = buf->dict->seg->dnum;
        buf->dict->msg[DICT_STR_YOMI].attr = SEG_UNDER_LINE;
        return;
    case DICT_STR_MSG2:
        if (buf->dict->mode == CLR_STATE && buf->dict->status == DICT_CONFIRM) {
            value = SJ3_DICT_CONFIRM;
        } else {
            if (buf->curhinsi < 0)
                value = buf->dict->value;
            else
                value = SJ3_DICT_HINSI;
        }
        break;
    case DICT_STR_HINSI:
        buf->dict->msg[DICT_STR_HINSI].data = buf->hinsi[buf->curhinsi].data;
        buf->dict->msg[DICT_STR_HINSI].len = buf->hinsi[buf->curhinsi].len;
        buf->dict->msg[DICT_STR_HINSI].attr = SEG_NORMAL;
        return;
    case DICT_STR_MSG3:
        if (buf->dict->mode == REG_STATE && buf->dict->status == DICT_CONFIRM) {
            value = SJ3_DICT_CONFIRM;
        } else {
            value = buf->dict->value;
        }
        break;
    default:
        value = buf->dict->value;
        break;
    }

    switch(buf->server) {
    case SERVER_SJ3:
    default:
        for (i = 0; i < dictmsgnum[SERVER_SJ3]; i++) {
            if ((buf->dict->mode & sj3_dict_status[i].state) &&
                    value == sj3_dict_status[i].value)
                break;
        }
        break;
    }
    if (i >= dictmsgnum[buf->server])
        Xsj3cError("Cannot find message for now status %s.",_Xsj3cItoa(value));
    else
        buf->dict->msg[n] = dictmsglist[buf->server][i];
}

void
Xsj3cDictRegister(buf)
    Xsj3cBuf        buf;
{
    wchar           disp[KANABUFSIZ];
    unsigned char   kana[KANJIBUFSIZ],  kanji[KANJIBUFSIZ];
    register int    i;

    disp[0] = '\0';
    for (i = buf->curseg; i <= buf->curseg + buf->dict->n_dict; i++) {
        _Xsj3cWcat(disp, buf->input[i]->disp);
    }
    buf->dict->n_dict = 0;
    _Xsj3cwOUTtomPS(buf, kanji, disp);
    if (buf->gakusyuu)
        _Xsj3cClearDcid(buf);
    _Xsj3cwPStomPS(buf, kana, buf->dict->seg->yomi);

    switch (buf->server) {
    case SERVER_SJ3:
    default:
        if ((buf->dict->value
                = serverIF[buf->server].func[FUNC_REGISTER](kana, kanji,
                sj3_hinsi_list[buf->curhinsi].code)) == 0) {
            buf->dict->value = SJ3_TOUROKU_SUCCESSED;
        } else if (buf->dict->value  < 0) {
            Xsj3cWarning("sj3serv is down. reconnect please");
            buf->dict->value = SJ3_TOUROKU_FAILED;
        }
        break;
    }
    _Xsj3cFlushDictMsg(buf);
}

void
Xsj3cDictClear(buf)
    Xsj3cBuf        buf;
{
    wchar           disp[KANABUFSIZ];
    unsigned char   kana[KANJIBUFSIZ],  kanji[KANJIBUFSIZ];
    register int    i;
    Sj3HinsiPtr     hp;

    disp[0] = '\0';
    for (i = buf->curseg; i <= buf->curseg + buf->dict->n_dict; i++) {
        _Xsj3cWcat(disp, buf->input[i]->disp);
    }
    buf->dict->n_dict = 0;
    _Xsj3cwOUTtomPS(buf, kanji, disp);
    if (buf->gakusyuu)
        _Xsj3cClearDcid(buf);
    _Xsj3cwPStomPS(buf, kana, buf->dict->seg->yomi);

    switch (buf->server) {
    case SERVER_SJ3:
    default:
        for (hp = sj3_hinsi_list, i = 0; hp->code > 0; hp++, i++) {
            if ((buf->dict->value
                    = serverIF[buf->server].func[FUNC_CLEAR](kana, kanji,
                    hp->code)) == 0) {
                buf->dict->value = SJ3_SYOUKYO_SUCCESSED;
                buf->curhinsi = i;
                _Xsj3cFlushDictMsg(buf);
                return;
            } else if (buf->dict->value < 0) {
                Xsj3cWarning("sj3serv is down. reconnect please");
                buf->dict->value = SJ3_SYOUKYO_FAILED;
                buf->curhinsi = -1;
                _Xsj3cFlushDictMsg(buf);
                return;
            } else if (buf->dict->value != SJ3_WORD_NOT_EXIST) {
                buf->curhinsi = -1;
                _Xsj3cFlushDictMsg(buf);
                return;
            }
        }
        break;
    }

    buf->curhinsi = -1;
    _Xsj3cFlushDictMsg(buf);
    return;
}

/*
 * Xsj3cEndDict()
 *  End dictionary mode(DictMode) and back to ConvedMode.
 */
void
Xsj3cEndDict(buf)
    Xsj3cBuf      buf;
{
    _Xsj3cFreeDictData(buf);
    buf->convmode = ConvedModeMask;
}

/*
 * _Xsj3cHinsiInit()
 *  Initialize hinsi data.
 */
Xsj3cHinsi
_Xsj3cHinsiInit(buf)
    Xsj3cBuf      buf;
{
    wchar               data[KANABUFSIZ];
    register int        i;

    if (hinsilist[buf->server]) {
        return(hinsilist[buf->server]);
    }

    switch (buf->server) {
    case SERVER_SJ3:
    default:
        hinsinum[SERVER_SJ3]= sizeof(sj3_hinsi_list)/sizeof(Sj3HinsiRec);
        break;
    }

    if ((hinsilist[buf->server]
            = (Xsj3cHinsi)calloc(hinsinum[buf->server], sizeof(Xsj3cHinsiRec)))
            == NULL) {
        Xsj3cError("Failed to allocate hinsi data");
    }

    switch (buf->server) {
    case SERVER_SJ3:
    default:
        for (i = 0; i < hinsinum[buf->server]; i++) {
            hinsilist[buf->server][i].len = _Xsj3cmPStowOUT(buf, data,
                    (unsigned char *)sj3_hinsi_list[i].string);
            hinsilist[buf->server][i].data
                = _Xsj3cStoreWchar(data, hinsilist[buf->server][i].len + 1);
            if (!hinsilist[buf->server][i].data)
                Xsj3cError("Failed to allocate hinsi data");
        }
        break;
    }

    return (hinsilist[buf->server]);
}

/*
 * Xsj3cGetHinsiNum()
 *  Return hinsi data total number.
 */
int
Xsj3cGetHinsiNum(buf, cur)
    Xsj3cBuf            buf;
    int                *cur;
{
    if (!buf->hinsi) {
        buf->hinsi = _Xsj3cHinsiInit(buf);
    }
    *cur = 0;
    return (hinsinum[buf->server]);
}

/*
 * Xsj3cGetHinsis()
 *  Return hinsi data structure.
 */
Xsj3cHinsi
Xsj3cGetHinsis(buf)
    Xsj3cBuf            buf;
{
    if (!buf->hinsi) {
        buf->hinsi = _Xsj3cHinsiInit(buf);
    }
    return (buf->hinsi);
}

/*
 * Xsj3cGetHinsi()
 *  Set the appointed (by 1st argument) hinsi to 2nd argument.
 */
wchar *
Xsj3cGetHinsi(buf, n, len)
    Xsj3cBuf            buf;
    int                 n;
    int                *len;
{
    if (!buf->hinsi) {
        buf->hinsi = _Xsj3cHinsiInit(buf);
    }
    *len = buf->hinsi[n].len;
    return (buf->hinsi[n].data);
}

/*
 * Xsj3cSetHinsi()
 *  Set the selected hinsi.
 */
int
Xsj3cSetHinsi(buf, sel_hinsi, changed, flush)
    Xsj3cBuf    buf;
    int         sel_hinsi;
    int         *changed;
    int         *flush;
{
    buf->curhinsi = sel_hinsi;
    _Xsj3cFlushDictMsg(buf);
    *changed = OFF;
    *flush = OFF;
    return 0;
}

/*
 * Xsj3cEndHinsi()
 *  End hinsi select mode(SelectMode) and back to ConvedMode.
 */
void
Xsj3cEndHinsi(buf)
    Xsj3cBuf    buf;
{
    buf->convmode = DictModeMask;
}
