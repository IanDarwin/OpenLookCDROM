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
static	char	rcs_id[] = "@(#) 102.1 $Id: kigo.c,v 5.11 1994/03/04 12:13:09 kon Exp $";
#endif /* lint */

#include	"canna.h"

extern SelectDirect;

#define BYTE1		84	/* JISコード表の第一バイトの数 */
#define BYTE2		94	/* JISコード表の第二バイトの数 */
#define KIGOSU		(((BYTE1 - 1) * BYTE2) + 4)
    				/* 記号の総数 */

#define KIGOSIZE	1	/* 記号候補の文字数 */
#define KIGOCOLS	2	/* 記号候補のコラム数 */
#define KIGOSPACE	2	/* 記号の間の空白文字のコラム数 */
#define KIGOWIDTH	(KIGOCOLS + KIGOSPACE)
					/* bangomaxを計算するための数 */

#define NKAKKOCHARS	1	/* JISコード表示用括弧の文字数 */
#define KAKKOCOLS       2       /* 同コラム数 */
#define NKCODECHARS	4	/* JISコード表示そのものの文字数 */
#define KCODECOLS       4       /* 同コラム数 */
/* JISコード表示全体の文字数 */
#define NKCODEALLCHARS	(NKAKKOCHARS + NKAKKOCHARS + NKCODECHARS)
/* 同コラム数 */
#define KCODEALLCOLS    (KAKKOCOLS + KAKKOCOLS + KCODECOLS)

static int kigo_curIkouho;

int
initKigoTable()
{
}

/* cfunc ichiranContext
 *
 * ichiranContext
 *
 */
static
clearKigoContext(p)
ichiranContext p;
{
  p->id = ICHIRAN_CONTEXT;
  p->svIkouho = 0;
  p->curIkouho = 0;
  p->nIkouho = 0;
  p->tooSmall = 0;
  p->curIchar = 0;
  p->allkouho = 0;
  p->glinebufp = 0;
  p->kouhoifp = (kouhoinfo *)0;
  p->glineifp = (glineinfo *)0;
  p->flags = (unsigned char)0;
}

static ichiranContext
newKigoContext()
{
  ichiranContext kcxt;

  if((kcxt = (ichiranContext)malloc(sizeof(ichiranContextRec))) == NULL) {
    jrKanjiError = "malloc (newKigoContext) できませんでした";
    return(0);
  }
  clearKigoContext(kcxt);

  return kcxt;
}


#ifdef	SOMEONE_USES_THIS
static void
freeKigoContext(kc)
ichiranContext kc;
{
  free(kc);
}
#endif	/* SOMEONE_USES_THIS */

/*
 * 記号一覧行を作る
 */
static
getKigoContext(d,
	  everyTimeCallback, exitCallback, quitCallback, auxCallback)
uiContext d;
int (*everyTimeCallback)(), (*exitCallback)();
int (*quitCallback)(), (*auxCallback)();
{
  extern KanjiModeRec kigo_mode;
  ichiranContext kc;
  int retval = 0;

  if(pushCallback(d, d->modec,
	everyTimeCallback, exitCallback, quitCallback, auxCallback) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }
  
  if((kc = newKigoContext()) == NULL) {
    popCallback(d);
    return(NG);
  }
  kc->next = d->modec;
  d->modec = (mode_context)kc;

  kc->prevMode = d->current_mode;
  d->current_mode = &kigo_mode;

  return(retval);
}

static void
popKigoMode(d)
uiContext d;
{
  ichiranContext kc = (ichiranContext)d->modec;

  d->modec = kc->next;
  d->current_mode = kc->prevMode;
  freeIchiranContext(kc);
}

/*
 * 記号一覧行に関する構造体の内容を更新する
 *
 * ・カレント候補によって kouhoinfo と glineinfo から候補一覧行を作る
 * ・カレント候補のコードをキャラクタに変換する
 *
 * 引き数	RomeStruct
 * 戻り値	なし
 */
static
makeKigoGlineStatus(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  wchar_t *gptr;
  char xxx[3];
  char *yyy = xxx;
  int  i, b1, b2;

  gptr = kc->glineifp->gldata + NKAKKOCHARS;
  
  /* カレント記号のJISコードを一覧行の中のカッコ内に入れる */
  WCstombs(xxx, kc->kouhoifp[*(kc->curIkouho)].khdata, 3);

  for(i=0; i<2; i++, yyy++) {
    b1 = (((unsigned long)*yyy & 0x7f) >> 4);
    b2 = (*yyy & 0x0f);
    *gptr++ = b1 + ((b1 > 0x09) ? ('a' - 10) : '0');
    *gptr++ = b2 + ((b2 > 0x09) ? ('a' - 10) : '0');
  }

  d->kanji_status_return->info |= KanjiGLineInfo;
  d->kanji_status_return->gline.line = kc->glineifp->gldata;
  d->kanji_status_return->gline.length = kc->glineifp->gllen;
  d->kanji_status_return->gline.revPos =
    kc->kouhoifp[*(kc->curIkouho)].khpoint;
  d->kanji_status_return->gline.revLen = KIGOSIZE;

}

/* 記号一覧用のglineinfoとkouhoinfoを作る
 *
 * ＊glineinfo＊
 *    int glkosu   : int glhead     : int gllen  : wchar_t *gldata
 *    １行の候補数 : 先頭記号が     : １行の長さ : 記号一覧行の文字列
 *                 : 何番目の記号か :
 * -------------------------------------------------------------------------
 * 0 | 6           : 0              : 24         : １☆２★３○４●５◎６□
 *
 *  ＊kouhoinfo＊
 *    int khretsu  : int khpoint  : wchar_t *khdata
 *    未使用       : 行の先頭から : 記号の文字
 *                 : 何バイト目か :
 * -------------------------------------------------------------------------
 * 0 | 0           : 0            : ☆
 * 1 | 0           : 4            : ★
 * 2 | 0           : 8            : ○
 *          :               :
 *
 * 引き数	headkouho	カレント記号候補行の先頭候補の位置
 *					(2121から何個目か(2121は０番目))
 *		uiContext
 * 戻り値	正常終了時 0
 */
static
makeKigoInfo(d, headkouho)
uiContext	d;
int		headkouho;
{
  ichiranContext kc = (ichiranContext)d->modec;
  wchar_t *gptr;
  int  i, b1, b2, lnko, cn;
  int  byte1hex = 0xa1;
  int  byte2hex = 0xa1;
  char xxx[3];

  b2 = headkouho % BYTE2;	/* JISコード表中(Ｘ軸)の位置 (点-1) */
  b1 = headkouho / BYTE2;	/* JISコード表中(Ｙ軸)の位置 (区-1) */

  xxx[2] = '\0';

#ifdef DEBUG
  if (iroha_debug) {
    printf("kigoinfo = bangomax %d, b1 %d, b2 %d\n", kc->nIkouho, b1, b2);
    printf("kigoinfo = headkouho %d, curIkouho %d\n",
	   headkouho, *(kc->curIkouho));
  }
#endif

  /* 記号一覧用のglineinfoとkouhoinfoを作る */
  gptr = kc->glinebufp;

  kc->glineifp->glhead = headkouho;
  kc->glineifp->gldata = gptr;

  /* JISコードの表示領域を一覧行中に作る */
  MBstowcs(gptr, "［", 1);
  for(i=0, gptr++; i<NKCODECHARS; i++)
    *gptr++ = ' ';
  MBstowcs(gptr++, "］", 1);

  for(cn=NKCODEALLCHARS, lnko=0;
      b1<BYTE1 && lnko<kc->nIkouho && (headkouho+lnko)<KIGOSU ; b1++) {
    for(; b2<BYTE2 && lnko<kc->nIkouho && (headkouho+lnko)<KIGOSU; b2++, lnko++) {
      
      /* 区切りになる空白をコピーする */
      if(lnko != 0) {
	MBstowcs(gptr++, "　", 1); 
	cn ++;
      }

      kc->kouhoifp[lnko].khpoint = cn;
      kc->kouhoifp[lnko].khdata = gptr;
      
      /* 候補をコピーする */
      *xxx = (char)byte1hex + b1;
      *(xxx + 1) = (char)byte2hex + b2;
      MBstowcs(gptr++, xxx, 1);
      cn ++;
    }
    b2 = 0;
  }
  *gptr = (wchar_t)0;
  kc->glineifp->glkosu = lnko;
  kc->glineifp->gllen = WStrlen(kc->glineifp->gldata);

  return(0);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 記号一覧                                                                  *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
kigoIchiranExitCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  popCallback(d);
  retval = YomiExit(d, retval);
  currentModeInfo(d);

  killmenu(d);

  return(retval);
}

static
kigoIchiranQuitCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  popCallback(d);
  currentModeInfo(d);

  return prevMenuIfExist(d);
}

KigoIchiran(d)
uiContext	d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    return NothingChangedWithBeep(d);
  }    
  
  if(makeKigoIchiran(d, CANNA_MODE_KigoMode) == NG)
    return(GLineNGReturn(d));
  else
    return(0);
}

/*
 * 記号一覧行を表示する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
makeKigoIchiran(d, major_mode)
uiContext	d;
int             major_mode;
{
  ichiranContext kc;
  int            headkouho;
  extern int quickly_escape;

  if(d->ncolumns < (KCODEALLCOLS + KIGOCOLS)) {
    NothingChangedWithBeep(d);
    jrKanjiError = "候補一覧用の幅が狭いので記号一覧できません";
    return(NG);
  }

  if(getKigoContext(d, 0, kigoIchiranExitCatch, kigoIchiranQuitCatch, 0) == NG)
    return(NG);

  kc = (ichiranContext)d->modec;
  kc->majorMode = major_mode;
  kc->minorMode = CANNA_MODE_KigoMode;
  kc->flags |= quickly_escape ? 0 : ICHIRAN_STAY_LONG;

  currentModeInfo(d);

  /* 最大記号表示数のセット */
  /* 総カラム数から "［JIS ］" 分を差し引いて計算する */
  if((kc->nIkouho =
      (((d->ncolumns - KCODEALLCOLS - KIGOCOLS) / KIGOWIDTH) + 1))
                                                  > KIGOBANGOMAX) {
    kc->nIkouho = KIGOBANGOMAX;
  }

  kc->curIkouho = &kigo_curIkouho;

  if(allocIchiranBuf(d) == NG) { /* 記号一覧モード */
    popKigoMode(d);
    popCallback(d);
    return(NG);
  }

  /* カレント候補のある記号一覧行の先頭候補と、
     一覧行中のカレント候補の位置を求める */
  if(d->curkigo) {		/* a1a1から何番目の記号か */
    headkouho = (d->curkigo / kc->nIkouho) * kc->nIkouho;
    *(kc->curIkouho) = d->curkigo % kc->nIkouho;
  } else {
    d->curkigo = 0;
    headkouho = 0;
    *(kc->curIkouho) = 0;
  }

  /* glineinfoとkouhoinfoを作る */
  makeKigoInfo(d, headkouho);

  /* kanji_status_returnを作る */
  makeKigoGlineStatus(d);

  return(0);
}

static
KigoNop(d)
uiContext	d;
{
  /* currentModeInfo でモード情報が必ず返るようにダミーのモードを入れておく */
  d->majorMode = d->minorMode = CANNA_MODE_AlphaMode;
  currentModeInfo(d);

  makeKigoGlineStatus(d);
  return 0;
}

/*
 * 記号一覧行中の次の記号に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoForwardKouho(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  int  headkouho;

  /* 次の記号にする */
  ++*(kc->curIkouho);
  
  /* 一覧表示の最後の記号だったら、次の一覧行の先頭記号をカレント記号とする */
  if((*(kc->curIkouho) >= kc->nIkouho) ||
     (kc->glineifp->glhead + *(kc->curIkouho) >= KIGOSU)) {
    headkouho  = kc->glineifp->glhead + kc->nIkouho;
    if(headkouho >= KIGOSU)
      headkouho = 0;
    *(kc->curIkouho) = 0;
    makeKigoInfo(d, headkouho);
  }

  /* kanji_status_retusrn を作る */
  makeKigoGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(0);
}

/*
 * 記号一覧行中の前の記号に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoBackwardKouho(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  int  headkouho;

  /* 前の記号にする */
  --*(kc->curIkouho);

  /* 一覧表示の先頭の記号だったら、前の一覧行の最終記号をカレント記号とする */
  if(*(kc->curIkouho) < 0) {
    headkouho  = kc->glineifp->glhead - kc->nIkouho;
    if(headkouho < 0)
      headkouho = ((KIGOSU - 1) / kc->nIkouho) * kc->nIkouho;
    makeKigoInfo(d, headkouho);
    *(kc->curIkouho) = kc->glineifp->glkosu - 1;
  }

  /* kanji_status_retusrn を作る */
  makeKigoGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(0);
}

/*
 * 前記号一覧列に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoPreviousKouhoretsu(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  int headkouho;

  /** 前候補列にする **/
  headkouho  = kc->glineifp->glhead - kc->nIkouho;
  if(headkouho < 0)
    headkouho = ((KIGOSU -1) / kc->nIkouho) * kc->nIkouho;
  makeKigoInfo(d, headkouho);

  /* *(kc->curIkouho) がカレント記号一覧の記号数より大きくなってしまったら
     最右記号をカレント候補とする */
  if(*(kc->curIkouho) >= kc->glineifp->glkosu)
    *(kc->curIkouho) = kc->glineifp->glkosu - 1;

  /* kanji_status_retusrn を作る */
  makeKigoGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(0);
}

/*
 * 次記号一覧列に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoNextKouhoretsu(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  int headkouho;

  /** 次候補列にする **/
  headkouho = kc->glineifp->glhead + kc->nIkouho;
  if(headkouho >= KIGOSU)
    headkouho = 0;
  makeKigoInfo(d, headkouho);

  /* *(kc->curIkouho) がカレント記号一覧の記号数より大きくなってしまったら
     最右記号をカレント候補とする */
  if(*(kc->curIkouho) >= kc->glineifp->glkosu)
    *(kc->curIkouho) = kc->glineifp->glkosu - 1;

  /* kanji_status_retusrn を作る */
  makeKigoGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(0);
}

/*
 * 記号一覧行中の先頭の記号に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoBeginningOfKouho(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;

  /* 候補列の先頭候補をカレント候補にする */
  *(kc->curIkouho) = 0;

  /* kanji_status_retusrn を作る */
  makeKigoGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(0);
}

/*
 * 記号一覧行中の最右の記号に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoEndOfKouho(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;

  /** 候補列の最右候補をカレント候補にする **/
  *(kc->curIkouho) = kc->glineifp->glkosu - 1;

  /* kanji_status_retusrn を作る */
  makeKigoGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(0);
}

/*
 * 記号一覧行中から選択された記号を確定する
 *
 * ・次に記号一覧した時に前回確定した記号がカレント候補となるように、
 *   確定した候補をセーブしておく
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoKakutei(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;

  /* カレント記号をセーブする */
  d->curkigo = kc->glineifp->glhead + *(kc->curIkouho);

  /* エコーストリングを確定文字列とする */
  if (d->n_buffer >= KIGOSIZE) {
    d->nbytes = KIGOSIZE;
    WStrncpy(d->buffer_return, kc->kouhoifp[*(kc->curIkouho)].khdata, 
	    d->nbytes);
    d->buffer_return[KIGOSIZE] = (wchar_t)0;
  }
  else {
    d->nbytes = 0;
  }

  if (kc->flags & ICHIRAN_STAY_LONG) {
    kc->flags |= ICHIRAN_NEXT_EXIT;
    d->status = EVERYTIME_CALLBACK;
  }
  else {
    freeIchiranBuf(kc);
    popKigoMode(d);
    GlineClear(d);

    d->status = EXIT_CALLBACK;
  }

  return(d->nbytes);
}

#ifdef	SOMEONE_USES_THIS
/*
 * 記号一覧行中の入力された番号の記号に移動する  【未使用】
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoBangoKouho(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  int num;

  /* 入力データは ０〜９ ａ〜ｆ か？ */
  if(((0x30 <= d->ch) && (d->ch <= 0x39))
     || ((0x61 <= d->ch) && (d->ch <= 0x66))) {
    if((0x30 <= d->ch) && (d->ch <= 0x39))
      num = (int)(d->ch & 0x0f);
    else if((0x61 <= d->ch) && (d->ch <= 0x66))
      num = (int)(d->ch - 0x57);
  } 
  else {
    /* 入力された番号は正しくありません */
    return NothingChangedWithBeep(d);
  }
  /* 入力データは 候補行の中に存在する数か？ */
  if(num >= kc->glineifp->glkosu) {
    /* 入力された番号は正しくありません */
    return NothingChangedWithBeep(d);
  }

  /* 候補列の先頭候補を得る */
  *(kc->curIkouho) = num;

  /* SelectDirect のカスタマイズの処理 */
  if(SelectDirect) /* ON */ {

    return(KigoKakutei(d));
  } else           /* OFF */ {
    /* kanji_status_retusrn を作る */
    makeKigoGlineStatus(d);

    return(0);
  }
}
#endif	/* SOMEONE_USES_THIS */

/*
 * 記号一覧行を消去する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0
 */
static
KigoQuit(d)
uiContext	d;
{
  ichiranContext kc = (ichiranContext)d->modec;
  BYTE fl = kc->flags;

  freeIchiranBuf(kc);
  popKigoMode(d);
  /* gline をクリアする */
  GlineClear(d);
  d->status = (fl & ICHIRAN_NEXT_EXIT) ? EXIT_CALLBACK : QUIT_CALLBACK;
  return 0;
}

#include	"kigomap.c"
