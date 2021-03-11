/* Copyright 1992 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

#if !defined(lint) && !defined(__CODECENTER__)
static char rcs_id[] = "@(#) 102.1 $Id: defaultmap.c,v 6.6 1994/04/21 02:44:16 kon Exp $";
#endif	/* lint */

#include "canna.h"
#include <canna/mfdef.h>

extern int howToBehaveInCaseOfUndefKey;
extern int strokelimit;

#define DEFAULTBEHAVIOR 0

static int (*getfunc(tbl, f))()
struct funccfunc *tbl;
unsigned char f;
{
  struct funccfunc *p;

  for (p = tbl ; p->funcid || p->cfunc ; p++) {
    if (p->funcid == f) {
      return p->cfunc;
    }
  }
  return (int (*)())0;
}

static
simpleUndefBehavior(d)
uiContext d;
{
  switch (howToBehaveInCaseOfUndefKey)
    {
    case kc_through:
      d->kanji_status_return->length = -1;
      return d->nbytes;
    case kc_kakutei:
      d->nbytes = escapeToBasicStat(d, CANNA_FN_Kakutei);
      if (d->n_buffer > d->nbytes) {
	int check;
	d->buffer_return[d->nbytes] =
	  key2wchar((int)(unsigned char)d->ch, &check);
	if (check) {
	  d->nbytes++;
	}
      }
      return d->nbytes;
    case kc_kill:
      d->nbytes = escapeToBasicStat(d, CANNA_FN_Quit);
      if (d->n_buffer > d->nbytes) {
	int check;
	d->buffer_return[d->nbytes] =
	  key2wchar((int)(unsigned char)d->ch, &check);
	if (check) {
	  d->nbytes++;
	}
      }
      return d->nbytes;
    case kc_normal:
    default:
      return NothingChangedWithBeep(d);
    }
}

searchfunc(d, mode, whattodo, key, fnum)
uiContext d;
KanjiMode mode;
int whattodo;
int key;
int fnum;
{
  int (*func)();

  if (fnum == 0) {
    fnum = mode->keytbl[key];
  }
  switch (whattodo) {
  case KEY_CALL:
    /* アルファベットモードが strokelimit ストローク以上続いたら
       サーバとの接続を切る */
    if (strokelimit > 0) {
      extern KanjiModeRec alpha_mode;
      if (mode == &alpha_mode) {
	d->strokecounter++;
#ifdef DEBUG
	if (iroha_debug) {
	  fprintf(stderr, "d->strokecounter=%d\n", d->strokecounter);
	}
#endif
	if (d->strokecounter == strokelimit + 1) {
	  jrKanjiPipeError();
	}	
      }
      else {
	d->strokecounter = 0;
#ifdef DEBUG
	if (iroha_debug) {
	  fprintf(stderr, "d->strokecounter=%d\n", d->strokecounter);
	}
#endif
      }
    }
    /* いよいよ本格的な処理(ここまでは前処理) */
    if (fnum < CANNA_FN_MAX_FUNC) {
      func = getfunc(mode->ftbl, fnum);
      if (func) {
	return (*func)(d);
      }
    }
    else {
      func = getfunc(mode->ftbl, CANNA_FN_UserMode);
      if (func) {
	/* func のタイプが上と違ってて汚いなあ... */
	return (*func)(d, fnum);
      }
    }
    /* そのモードで fnum に対応する機能がない。しかたがないので、
       デフォルト機能を探す */
    func = getfunc(mode->ftbl, DEFAULTBEHAVIOR);
    if (func) {
      return (*func)(d);
    }
    else {
      return simpleUndefBehavior(d);
    }
    /* NOTREACHED */
    break;
  case KEY_CHECK:
    if (fnum >= CANNA_FN_MAX_FUNC) {
      fnum = CANNA_FN_UserMode;
    }
    return getfunc(mode->ftbl, fnum) ? 1 : 0;
    /* NOTREACHED */
    break;
  case KEY_SET: /* is not supported yet */
    return 0;
    /* NOTREACHED */
    break;
  }
  /* NOTREACHED */
}

/* 逐次読みモード用 */

CYsearchfunc(d, mode, whattodo, key, fnum)
uiContext d;
KanjiMode mode;
int whattodo;
int key;
int fnum;
{
  int (*func)();
  extern KanjiModeRec yomi_mode;

  if (fnum == 0) {
    fnum = mode->keytbl[key];
  }
  if (Yomisearchfunc(d, mode, KEY_CHECK, key, fnum)) {
    return Yomisearchfunc(d, mode, whattodo, key, fnum);
  }
  else {
    func = getfunc(yomi_mode.ftbl, fnum);
    switch (whattodo) {
    case KEY_CALL:
      if (func) {
	return (*func)(d);
      }
      else {
	return Yomisearchfunc(d, mode, whattodo, key, fnum);
      }
      /* NOTREACHED */
      break;
    case KEY_CHECK:
      return func ? 1 : 0;
      /* NOTREACHED */
      break;
    case KEY_SET:
      return 0;
      /* NOTREACHED */
      break;
    }
  }
  /* may be NOTREACHED */
  return 0;
}

#define NONE CANNA_FN_Undefined

BYTE default_kmap[256] =
{               
/* C-@ */       CANNA_FN_Mark,
/* C-a */       CANNA_FN_BeginningOfLine,
/* C-b */       CANNA_FN_Backward,
/* C-c */       CANNA_FN_BubunMuhenkan,
/* C-d */       CANNA_FN_DeleteNext,
/* C-e */       CANNA_FN_EndOfLine,
/* C-f */       CANNA_FN_Forward,
/* C-g */       CANNA_FN_Quit,
/* C-h */       CANNA_FN_DeletePrevious,
/* C-i */       CANNA_FN_Shrink,
/* C-j */       CANNA_FN_Kakutei,
/* C-k */       CANNA_FN_KillToEndOfLine,
/* C-l */       CANNA_FN_ToLower,
/* C-m */       CANNA_FN_Kakutei,
/* C-n */       CANNA_FN_Next,
/* C-o */       CANNA_FN_Extend,
/* C-p */       CANNA_FN_Prev,
/* C-q */       CANNA_FN_QuotedInsert,
/* C-r */       NONE,
/* C-s */       NONE,
/* C-t */       NONE,
/* C-u */       CANNA_FN_ToUpper,
/* C-v */       NONE,
/* C-w */       CANNA_FN_KouhoIchiran,
/* C-x */       NONE,
/* C-y */       CANNA_FN_ConvertAsHex,
/* C-z */       NONE,
/* C-[ */       NONE,
/* C-\ */       NONE,
/* C-] */       NONE,
/* C-^ */       NONE,
/* C-_ */       NONE,
/* space */     CANNA_FN_Henkan,
/* ! */         CANNA_FN_FunctionalInsert,
/* " */         CANNA_FN_FunctionalInsert,
/* # */         CANNA_FN_FunctionalInsert,
/* $ */         CANNA_FN_FunctionalInsert,
/* % */         CANNA_FN_FunctionalInsert,
/* & */         CANNA_FN_FunctionalInsert,
/* ' */         CANNA_FN_FunctionalInsert,
/* ( */         CANNA_FN_FunctionalInsert,
/* ) */         CANNA_FN_FunctionalInsert,
/* * */         CANNA_FN_FunctionalInsert,
/* + */         CANNA_FN_FunctionalInsert,
/* , */         CANNA_FN_FunctionalInsert,
/* - */         CANNA_FN_FunctionalInsert,
/* . */         CANNA_FN_FunctionalInsert,
/* / */         CANNA_FN_FunctionalInsert,
/* 0 */         CANNA_FN_FunctionalInsert,
/* 1 */         CANNA_FN_FunctionalInsert,
/* 2 */         CANNA_FN_FunctionalInsert,
/* 3 */         CANNA_FN_FunctionalInsert,
/* 4 */         CANNA_FN_FunctionalInsert,
/* 5 */         CANNA_FN_FunctionalInsert,
/* 6 */         CANNA_FN_FunctionalInsert,
/* 7 */         CANNA_FN_FunctionalInsert,
/* 8 */         CANNA_FN_FunctionalInsert,
/* 9 */         CANNA_FN_FunctionalInsert,
/* : */         CANNA_FN_FunctionalInsert,
/* ; */         CANNA_FN_FunctionalInsert,
/* < */         CANNA_FN_FunctionalInsert,
/* = */         CANNA_FN_FunctionalInsert,
/* > */         CANNA_FN_FunctionalInsert,
/* ? */         CANNA_FN_FunctionalInsert,
/* @ */         CANNA_FN_FunctionalInsert,
/* A */         CANNA_FN_FunctionalInsert,
/* B */         CANNA_FN_FunctionalInsert,
/* C */         CANNA_FN_FunctionalInsert,
/* D */         CANNA_FN_FunctionalInsert,
/* E */         CANNA_FN_FunctionalInsert,
/* F */         CANNA_FN_FunctionalInsert,
/* G */         CANNA_FN_FunctionalInsert,
/* H */         CANNA_FN_FunctionalInsert,
/* I */         CANNA_FN_FunctionalInsert,
/* J */         CANNA_FN_FunctionalInsert,
/* K */         CANNA_FN_FunctionalInsert,
/* L */         CANNA_FN_FunctionalInsert,
/* M */         CANNA_FN_FunctionalInsert,
/* N */         CANNA_FN_FunctionalInsert,
/* O */         CANNA_FN_FunctionalInsert,
/* P */         CANNA_FN_FunctionalInsert,
/* Q */         CANNA_FN_FunctionalInsert,
/* R */         CANNA_FN_FunctionalInsert,
/* S */         CANNA_FN_FunctionalInsert,
/* T */         CANNA_FN_FunctionalInsert,
/* U */         CANNA_FN_FunctionalInsert,
/* V */         CANNA_FN_FunctionalInsert,
/* W */         CANNA_FN_FunctionalInsert,
/* X */         CANNA_FN_FunctionalInsert,
/* Y */         CANNA_FN_FunctionalInsert,
/* Z */         CANNA_FN_FunctionalInsert,
/* [ */         CANNA_FN_FunctionalInsert,
/* \ */         CANNA_FN_FunctionalInsert,
/* ] */         CANNA_FN_FunctionalInsert,
/* ^ */         CANNA_FN_FunctionalInsert,
/* _ */         CANNA_FN_FunctionalInsert,
/* ` */         CANNA_FN_FunctionalInsert,
/* a */         CANNA_FN_FunctionalInsert,
/* b */         CANNA_FN_FunctionalInsert,
/* c */         CANNA_FN_FunctionalInsert,
/* d */         CANNA_FN_FunctionalInsert,
/* e */         CANNA_FN_FunctionalInsert,
/* f */         CANNA_FN_FunctionalInsert,
/* g */         CANNA_FN_FunctionalInsert,
/* h */         CANNA_FN_FunctionalInsert,
/* i */         CANNA_FN_FunctionalInsert,
/* j */         CANNA_FN_FunctionalInsert,
/* k */         CANNA_FN_FunctionalInsert,
/* l */         CANNA_FN_FunctionalInsert,
/* m */         CANNA_FN_FunctionalInsert,
/* n */         CANNA_FN_FunctionalInsert,
/* o */         CANNA_FN_FunctionalInsert,
/* p */         CANNA_FN_FunctionalInsert,
/* q */         CANNA_FN_FunctionalInsert,
/* r */         CANNA_FN_FunctionalInsert,
/* s */         CANNA_FN_FunctionalInsert,
/* t */         CANNA_FN_FunctionalInsert,
/* u */         CANNA_FN_FunctionalInsert,
/* v */         CANNA_FN_FunctionalInsert,
/* w */         CANNA_FN_FunctionalInsert,
/* x */         CANNA_FN_FunctionalInsert,
/* y */         CANNA_FN_FunctionalInsert,
/* z */         CANNA_FN_FunctionalInsert,
/* { */         CANNA_FN_FunctionalInsert,
/* | */         CANNA_FN_FunctionalInsert,
/* } */         CANNA_FN_FunctionalInsert,
/* ~ */         CANNA_FN_FunctionalInsert,
/* DEL */       NONE,
/* Nfer */      CANNA_FN_Kakutei,
/* Xfer */      CANNA_FN_Henkan,
/* Up */        CANNA_FN_Prev,
/* Left */      CANNA_FN_Backward,
/* Right */     CANNA_FN_Forward,
/* Down */      CANNA_FN_Next,
/* Insert */    CANNA_FN_KigouMode,
/* Rollup */    NONE,
/* Rolldown */  NONE,
/* Home */      NONE,
/* Help */      NONE,
/* KeyPad */	NONE,
/* 8c */        NONE,
/* 8d */        NONE,
/* 8e */        NONE,
/* 8f */        NONE,
/* S-nfer */    NONE,
/* S-xfer */    NONE,
/* S-up */      NONE,
/* S-left */    CANNA_FN_Shrink,
/* S-right */   CANNA_FN_Extend,
/* S-down */    NONE,
/* C-nfer */    NONE,
/* C-xfer */    NONE,
/* C-up */      NONE,
/* C-left */    CANNA_FN_Shrink,
/* C-right */   CANNA_FN_Extend,
/* C-down */    NONE,
/* 9c */        NONE,
/* 9d */        NONE,
/* 9e */        NONE,
/* 9f */        NONE,
/* KANASPACE */ NONE,
/* 。 */        CANNA_FN_FunctionalInsert,
/* 「 */        CANNA_FN_FunctionalInsert,
/* 」 */        CANNA_FN_FunctionalInsert,
/* 、 */        CANNA_FN_FunctionalInsert,
/* ・ */        CANNA_FN_FunctionalInsert,
/* ヲ */        CANNA_FN_FunctionalInsert,
/* ァ */        CANNA_FN_FunctionalInsert,
/* ィ */        CANNA_FN_FunctionalInsert,
/* ゥ */        CANNA_FN_FunctionalInsert,
/* ェ */        CANNA_FN_FunctionalInsert,
/* ォ */        CANNA_FN_FunctionalInsert,
/* ャ */        CANNA_FN_FunctionalInsert,
/* ュ */        CANNA_FN_FunctionalInsert,
/* ョ */        CANNA_FN_FunctionalInsert,
/* ッ */        CANNA_FN_FunctionalInsert,
/* ー */        CANNA_FN_FunctionalInsert,
/* ア */        CANNA_FN_FunctionalInsert,
/* イ */        CANNA_FN_FunctionalInsert,
/* ウ */        CANNA_FN_FunctionalInsert,
/* エ */        CANNA_FN_FunctionalInsert,
/* オ */        CANNA_FN_FunctionalInsert,
/* カ */        CANNA_FN_FunctionalInsert,
/* キ */        CANNA_FN_FunctionalInsert,
/* ク */        CANNA_FN_FunctionalInsert,
/* ケ */        CANNA_FN_FunctionalInsert,
/* コ */        CANNA_FN_FunctionalInsert,
/* サ */        CANNA_FN_FunctionalInsert,
/* シ */        CANNA_FN_FunctionalInsert,
/* ス */        CANNA_FN_FunctionalInsert,
/* セ */        CANNA_FN_FunctionalInsert,
/* ソ */        CANNA_FN_FunctionalInsert,
/* タ */        CANNA_FN_FunctionalInsert,
/* チ */        CANNA_FN_FunctionalInsert,
/* ツ */        CANNA_FN_FunctionalInsert,
/* テ */        CANNA_FN_FunctionalInsert,
/* ト */        CANNA_FN_FunctionalInsert,
/* ナ */        CANNA_FN_FunctionalInsert,
/* ニ */        CANNA_FN_FunctionalInsert,
/* ヌ */        CANNA_FN_FunctionalInsert,
/* ネ */        CANNA_FN_FunctionalInsert,
/* ノ */        CANNA_FN_FunctionalInsert,
/* ハ */        CANNA_FN_FunctionalInsert,
/* ヒ */        CANNA_FN_FunctionalInsert,
/* フ */        CANNA_FN_FunctionalInsert,
/* ヘ */        CANNA_FN_FunctionalInsert,
/* ホ */        CANNA_FN_FunctionalInsert,
/* マ */        CANNA_FN_FunctionalInsert,
/* ミ */        CANNA_FN_FunctionalInsert,
/* ム */        CANNA_FN_FunctionalInsert,
/* メ */        CANNA_FN_FunctionalInsert,
/* モ */        CANNA_FN_FunctionalInsert,
/* ヤ */        CANNA_FN_FunctionalInsert,
/* ユ */        CANNA_FN_FunctionalInsert,
/* ヨ */        CANNA_FN_FunctionalInsert,
/* ラ */        CANNA_FN_FunctionalInsert,
/* リ */        CANNA_FN_FunctionalInsert,
/* ル */        CANNA_FN_FunctionalInsert,
/* レ */        CANNA_FN_FunctionalInsert,
/* ロ */        CANNA_FN_FunctionalInsert,
/* ワ */        CANNA_FN_FunctionalInsert,
/* ン */        CANNA_FN_FunctionalInsert,
/* ゛ */        CANNA_FN_FunctionalInsert,
/* ゜ */        CANNA_FN_FunctionalInsert,
/* F1 */        NONE,
/* F2 */        NONE,
/* F3 */        NONE,
/* F4 */        NONE,
/* F5 */        NONE,
/* F6 */        NONE,
/* F7 */        NONE,
/* F8 */        NONE,
/* F9 */        NONE,
/* F10 */       NONE,
/* ea */        NONE,
/* eb */        NONE,
/* ec */        NONE,
/* ed */        NONE,
/* ee */        NONE,
/* ef */        NONE,
/* PF1 */       NONE,
/* PF2 */       NONE,
/* PF3 */       NONE,
/* PF4 */       NONE,
/* PF5 */       NONE,
/* PF6 */       NONE,
/* PF7 */       NONE,
/* PF8 */       NONE,
/* PF9 */       NONE,
/* PF10 */      NONE,
/* fa */        NONE,
/* fb */        NONE,
/* fc */        NONE,
/* fd */        NONE,
/* fe */        NONE,
/* ff */        NONE,
};
