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
static	char	rcs_id[] = "@(#) 102.1 $Id: ichiran.c,v 5.23 1994/04/18 12:05:57 kon Exp $";
#endif /* lint */

#include	<errno.h>
#include	"canna.h"

extern int  errno;

extern int SelectDirect, BunsetsuKugiri, ReverseWord;
extern int QuitIchiranIfEnd, stayAfterValidate;
extern int HexkeySelect, CursorWrap;
extern int kCount;
extern int TanNextKouho();

extern int indexHankaku;
extern int indexSeparator;

static int clearIchiranContext(), IchiranKakutei();
static void getIchiranPreviousKouhoretsu();
static void getIchiranNextKouhoretsu();


#define ICHISIZE 9
static char *sbango = 
  "１　２　３　４　５　６　７　８　９　ａ　ｂ　ｃ　ｄ　ｅ　ｆ";
                                                /* 候補行の候補番号の文字列 */
static wchar_t *bango;

/*  "1.","　2.","　3.","　4.","　5.","　6.","　7.","　8.","　9.",*/
static char  *sbango2[] = {
  "1","　2","　3","　4","　5","　6","　7","　8","　9",
  };

static wchar_t *bango2[ICHISIZE];

static char *skuuhaku = "　";
static wchar_t *kuuhaku;

/* 半角表示の際のセパレーター */
extern int indexSeparator;

initIchiran()
{
  int i;
  char buf[16];

  setWStrings(&bango, &sbango, 1);

  for(i = 0; i < ICHISIZE; i++) {

    /* セパレーターの処理 */
    if(indexSeparator && 0x20<= indexSeparator && 0x80 > indexSeparator)
      sprintf(buf, "%s%c", sbango2[i], (char)indexSeparator);
    else
      sprintf(buf, "%s%c", sbango2[i], (char)DEFAULTINDEXSEPARATOR);
      
    bango2[i] = WString(buf);
  }

  setWStrings(&kuuhaku, &skuuhaku, 1);
  return 0;
}


/*
 * 一覧行表示中のカレント文節の候補を更新する
 *
 * ・カレント候補を変える。
 * ・これにともない kugiri も更新される
 *
 * 引き数	uiContext
 *              yomiContext
 */
static void
makeIchiranEchoStrCurChange(yc)
yomiContext yc;
{
  RkwXfer(yc->context, yc->curIkouho);
}

/*
 * かな漢字変換用の構造体の内容を更新する(その場のみ)
 *
 * ・一覧を呼び出す前の状態についての表示文字列を作る
 *
 * 引き数	uiContext
 *              yomiContext
 */
static
makeIchiranKanjiStatusReturn(d, env, yc)
uiContext	d;
mode_context env;
yomiContext yc;
{
  mode_context sv;

  sv = d->modec;
  d->modec = env;
  makeKanjiStatusReturn(d, yc);
  d->modec = sv;
}

#define DEC_COLUMNS(n) ((n) < 10 ? 1 : (n) < 100 ? 2 : (n) < 1000 ? 3 : 4)

/*
 * 候補行に関する構造体の内容を更新する
 *
 * ・glineinfo と kouhoinfo から候補行を作成し、カレント候補番号を反転する
 *
 * 引き数	uiContext
 * 戻り値	なし
 */
makeGlineStatus(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  wchar_t *p;
  char str[16];
  int i, cur;

  if (kCount) {
    cur = *(ic->curIkouho) + 1;
  }

  d->kanji_status_return->info |= KanjiGLineInfo;
  d->kanji_status_return->gline.line =
    ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].gldata;
  d->kanji_status_return->gline.length = 
    ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].gllen;
  d->kanji_status_return->gline.revPos = 
    ic->kouhoifp[*(ic->curIkouho)].khpoint;
  if (ReverseWord && ic->inhibit & NUMBERING) {
    p = ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].gldata +
      ic->kouhoifp[*(ic->curIkouho)].khpoint;
    for (i = 0;
	 *p != *kuuhaku && *p != ' ' && *p != 0
	 && i < ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].gllen;
	 i++) {
      p++;
    }
    d->kanji_status_return->gline.revLen = i;
  } else
    d->kanji_status_return->gline.revLen = 1;

  if (kCount && d->kanji_status_return->gline.length) {
    register int a = ic->nIkouho, b = DEC_COLUMNS(cur) + DEC_COLUMNS(a) + 2;
    sprintf(str, " %d/%d", cur, a);
    MBstowcs(d->kanji_status_return->gline.line + 
	     d->kanji_status_return->gline.length - b, str, b + 1);
    /* 以下はいらないのでは？ */
    d->kanji_status_return->gline.length
      = WStrlen(d->kanji_status_return->gline.line);
  }
}

static
ichiranEveryTimeCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  yomiContext yc;

  yc = (yomiContext)env;

  makeIchiranEchoStrCurChange(yc);
  makeIchiranKanjiStatusReturn(d, env, yc);

  return(retval);
}

static
ichiranExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  yomiContext yc;

  yc = (yomiContext)env;
  yc->kouhoCount = 0;
  /* d->curIkouhoをカレント候補とする */
  if ((retval = RkwXfer(yc->context, yc->curIkouho)) == NG) {
    if (errno == EPIPE) {
      jrKanjiPipeError();
    }
    jrKanjiError = "カレント候補を取り出せませんでした";
    /* カレント候補が取り出せないくらいでは何ともないぞ */
  }
  else {
    retval = d->nbytes = 0;
  }

  makeIchiranEchoStrCurChange(yc);
  makeIchiranKanjiStatusReturn(d, env, yc);

  freeGetIchiranList(yc->allkouho);
  
  popCallback(d);

  if (!stayAfterValidate && !d->more.todo) {
    d->more.todo = 1;
    d->more.ch = 0;
    d->more.fnum = CANNA_FN_Forward;
  }
  currentModeInfo(d);

  return(retval);
}

static
ichiranQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  yomiContext yc;

  yc = (yomiContext)env;
  yc->kouhoCount = 0;

  if ((retval = RkwXfer(yc->context, yc->curIkouho)) == NG) {
    if(errno == EPIPE) {
      jrKanjiPipeError();
    }
    jrKanjiError = "カレント候補を取り出せませんでした";
    /* カレント候補が取り出せないくらいでは何ともないぞ */
  }
  else {
    retval = d->nbytes = 0;
  }

  makeIchiranEchoStrCurChange(yc);
  makeIchiranKanjiStatusReturn(d, env, yc);

  freeGetIchiranList(yc->allkouho);

  popCallback(d);
  currentModeInfo(d);
  return(retval);
}

void
freeIchiranBuf(ic)
ichiranContext ic;
{
  if(ic->glinebufp)
    free(ic->glinebufp);
  if(ic->kouhoifp)
    free(ic->kouhoifp);
  if(ic->glineifp)
    free(ic->glineifp);
}

freeGetIchiranList(buf)
wchar_t **buf;
{
  /* 候補一覧表示行用のエリアをフリーする */
  if(buf) {
    if(*buf) {
      free(*buf);
    }
    free(buf);
  }
}

static void
popIchiranMode(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  d->modec = ic->next;
  d->current_mode = ic->prevMode;
  freeIchiranContext(ic);
}

/*
 * すべての候補を取り出して、配列にする
 */

static int makeKouhoIchiran();

wchar_t **
getIchiranList(context, nelem, currentkouho)
int context;
int *nelem, *currentkouho;
{
  wchar_t *work, *wptr, **bptr, **buf;
  RkStat st;
  int i;

  /* RkwGetKanjiList で得る、すべての候補のための領域を得る */
  if((work = (wchar_t *)malloc(ROMEBUFSIZE * sizeof(wchar_t))) == (wchar_t *)NULL) {
    jrKanjiError = "malloc (getIchiranList) できませんでした";
    return(0);
  }

  /* すべての候補を得る。
     例: けいかん → 警官@景観@掛冠@@ (@はNULL) */
  if((*nelem = RkwGetKanjiList(context, work, ROMEBUFSIZE)) < 0) {
    jrKanjiError = "すべての候補の取り出しに失敗しました";
    free(work);
    return(0);
  }

#ifdef	INHIBIT_DUPLICATION
  if (*nelem == 3) {
    wchar_t *w1, *w2, *w3;

    w1 = work;
    w2 = w1 + WStrlen(w1);
    w3 = w2 + WStrlen(w2);
    if (!WStrcmp(w1, ++w3)) {
      if (!WStrcmp(w1, ++w2))
	*nelem = 1;
      else
	*nelem = 2;
    }
  }
#endif	/* INHIBIT_DUPLICATION */

  /* makeKouhoIchiran()に渡すデータ */
  if((buf = (wchar_t **)calloc
      (*nelem + 1, sizeof(wchar_t *))) == (wchar_t **)NULL) {
    jrKanjiError = "malloc (getIchiranList) できませんでした";
    free(work);
    return(0);
  }
  for(wptr = work, bptr = buf, i = 0; *wptr && i++ < *nelem; bptr++) {
    *bptr = wptr;
    while(*wptr++)
      /* EMPTY */
      ;
  }
  *bptr = (wchar_t *)0;

  if(RkwGetStat(context, &st) == -1) {
    jrKanjiError = "ステイタスを取り出せませんでした";
    free(work);
    free(buf);
    return(0);
  }
  *currentkouho = st.candnum; /* カレント候補は何番目？ */

  return(buf);
}

/* cfunc ichiranContext
 *
 * ichiranContext 候補一覧用の構造体を作り初期化する
 *
 */
ichiranContext
newIchiranContext()
{
  ichiranContext icxt;

  if((icxt = (ichiranContext)malloc(sizeof(ichiranContextRec))) == 
     (ichiranContext)NULL) {
    jrKanjiError = "malloc (newIchiranContext) できませんでした";
    return(0);
  }
  clearIchiranContext(icxt);

  return(icxt);
}

/*
 * 候補一覧行を作る
 */
selectOne(d, buf, ck, nelem, bangomax, inhibit, currentkouho, allowcallback,
	  everyTimeCallback, exitCallback, quitCallback, auxCallback)
uiContext d;
wchar_t **buf;
int *ck;
int nelem, bangomax;
unsigned char inhibit;
int currentkouho;
int allowcallback;
int (*everyTimeCallback)(), (*exitCallback)();
int (*quitCallback)(), (*auxCallback)();
{
  extern KanjiModeRec ichiran_mode;
  ichiranContext ic;

  if (allowcallback != WITHOUT_LIST_CALLBACK &&
      d->list_func == (void (*)())0) {
    allowcallback = WITHOUT_LIST_CALLBACK;
  }

  if(pushCallback(d, d->modec,
	everyTimeCallback, exitCallback, quitCallback, auxCallback) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }
  
  if((ic = newIchiranContext()) == (ichiranContext)NULL) {
    popCallback(d);
    return(NG);
  }
  ic->majorMode = d->majorMode;
  ic->next = d->modec;
  d->modec = (mode_context)ic;

  ic->prevMode = d->current_mode;
  d->current_mode = &ichiran_mode;

  ic->allkouho = buf;
  ic->curIkouho = ck;
  ic->inhibit = inhibit;
  ic->nIkouho = nelem;

  if (allowcallback != WITHOUT_LIST_CALLBACK) {
    ic->flags |= ICHIRAN_ALLOW_CALLBACK;
    ic->inhibit |= NUMBERING;
  }

  if (allowcallback == WITHOUT_LIST_CALLBACK) {
    if (makeKouhoIchiran(d, nelem, bangomax, inhibit, currentkouho)   == NG) {
      popIchiranMode(d);
      popCallback(d);
      return(NG);
    }
  }
  else {
    d->list_func(d->client_data, CANNA_LIST_Start, buf, nelem, ck);
  }

  return(0);
}

/*
 * IchiranContext の初期化
 */
static
clearIchiranContext(p)
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
  
/*
 * 候補一覧のデータ構造体を作るための領域を確保する
 */
allocIchiranBuf(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int size;

  /* サイズの分と番号の分の領域を得る*/
  size = ic->nIkouho * (d->ncolumns + 1) * WCHARSIZE; /* えいやっ */
  if((ic->glinebufp = (wchar_t *)malloc(size)) ==  (wchar_t *)NULL) {
    jrKanjiError = "malloc (allocIchiranBuf) できませんでした";
    return(NG);
  }

  /* kouhoinfoの領域を得る */
  size = (ic->nIkouho + 1) * sizeof(kouhoinfo);
  if((ic->kouhoifp = (kouhoinfo *)malloc(size)) == (kouhoinfo *)NULL) {
    jrKanjiError = "malloc (allocIchiranBuf) できませんでした";
    free(ic->glinebufp);
    return(NG);
  }

  /* glineinfoの領域を得る */
  size = (ic->nIkouho + 1) * sizeof(glineinfo);
  if((ic->glineifp = (glineinfo *)malloc(size)) == (glineinfo *)NULL) {
    jrKanjiError = "malloc (allocIchiranBuf) できませんでした";
    free(ic->glinebufp);
    free(ic->kouhoifp);
    return(NG);
  }
  return(0);
}

/*
 * 候補一覧行を表示用のデータをテーブルに作成する
 *
 * ・glineinfo と kouhoinfoを作成する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static int
makeKouhoIchiran(d, nelem, bangomax, inhibit, currentkouho)
uiContext d;
int nelem, bangomax;
unsigned char inhibit;
int currentkouho;
{
  ichiranContext ic = (ichiranContext)d->modec;
  wchar_t **kkptr, *kptr, *gptr, *svgptr;
  int           ko, lnko, cn = 0, svcn, line = 0, dn = 0, svdn;
  int netwidth;

  netwidth = d->ncolumns -
    (kCount ? (DEC_COLUMNS(nelem) * 2 + 2/* 2は / と SP の分 */) : 0);

  ic->nIkouho = nelem;	/* 候補の数 */

  /* カレント候補をセットする */
  ic->svIkouho = *(ic->curIkouho);
  *(ic->curIkouho) += currentkouho;
  if(*(ic->curIkouho) >= ic->nIkouho)
    ic->svIkouho = *(ic->curIkouho) = 0;

  if(allocIchiranBuf(d) == NG)
    return(NG);

  if(d->ncolumns < 1) {
    ic->tooSmall = 1;
    return(0);
  }

  /* glineinfoとkouhoinfoを作る */
  /* 
   ＊glineinfo＊
      int glkosu   : int glhead     : int gllen  : wchar_t *gldata
      １行の候補数 : 先頭候補が     : １行の長さ : 候補一覧行の文字列
                   : 何番目の候補か :
   -------------------------------------------------------------------------
   0 | 6           : 0              : 24         : １新２心３進４真５神６信
   1 | 4           : 6              : 16         : １臣２寝３伸４芯

    ＊kouhoinfo＊
      int khretsu  : int khpoint  : wchar_t *khdata
      なん列目に   : 行の先頭から : 候補の文字列
      ある候補か   : 何バイト目か :
   -------------------------------------------------------------------------
   0 | 0           : 0            : 新
   1 | 0           : 4            : 心
             :                :             :
   7 | 1           : 0            : 臣
   8 | 1           : 4            : 寝
  */

  kkptr = ic->allkouho;
  kptr = *(ic->allkouho);
  gptr = ic->glinebufp;

  /* line -- 何列目か
     ko   -- 全体の先頭から何番目の候補か
     lnko -- 列の先頭から何番目の候補か
     cn   -- 列の先頭から何バイト目か */

  for(line=0, ko=0; ko<ic->nIkouho; line++) {
    ic->glineifp[line].gldata = gptr; /* 候補行を表示するための文字列 */
    ic->glineifp[line].glhead = ko;   /* この行の先頭候補は、全体でのko番目 */

    ic->tooSmall = 1;
    for (lnko = cn = dn = 0 ;
	dn < netwidth && lnko < bangomax && ko < ic->nIkouho ; lnko++, ko++) {
      ic->tooSmall = 0;
      kptr = kkptr[ko];
      ic->kouhoifp[ko].khretsu = line; /* 何行目に存在するかを記録 */
      ic->kouhoifp[ko].khpoint = cn + (lnko ? 1 : 0);
      ic->kouhoifp[ko].khdata = kptr;  /* その文字列へのポインタ */
      svgptr = gptr;
      svcn = cn;
      svdn = dn;
      /* ２種類の表示を分ける */
      if(!(inhibit & (unsigned char)NUMBERING)) {
	/* 番号をコピーする */
	if(!indexHankaku) {/* 全角 */
	  if(lnko == 0) {
	    *gptr++ = *bango; cn ++; dn +=2;
	  } else {
	    WStrncpy(gptr, bango + 1 + BANGOSIZE * (lnko - 1), BANGOSIZE);
	    cn += BANGOSIZE; gptr += BANGOSIZE, dn += BANGOSIZE*2;
	  }
	}
	else{ /* 半角 */
	  WStrcpy(gptr, bango2[lnko]);
	  if(lnko == 0) {
	    dn +=2;
	  } else {
	    dn +=4;
	  }
	  cn += WStrlen(bango2[lnko]);
	  gptr += WStrlen(bango2[lnko]);
	}
      } else {
	/* 空白をコピーする */
	if(lnko) {
	  *gptr++ = *kuuhaku; cn ++; dn +=2;
	}
      }
      /* 候補をコピーする */
      for (; *kptr && dn < netwidth ; gptr++, kptr++, cn++) {
	*gptr = *kptr;
	if (WIsG0(*gptr))
	  dn++;
	else if (WIsG1(*gptr))
	  dn += 2;
	else if (WIsG2(*gptr))
	  dn ++;
	else if (WIsG3(*gptr))
	  dn += 2;
      }

      /* カラム数よりはみだしてしまいそうになったので１つ戻す */
      if (dn >= netwidth) {
	if (lnko) {
	  gptr = svgptr;
	  cn = svcn;
	  dn = svdn;
	}
	else {
	  ic->tooSmall = 1;
	}
	break;
      }
    }
    if (ic->tooSmall) {
      return 0;
    }
    if (kCount) {
      for (;dn < d->ncolumns - 1; dn++)
	*gptr++ = ' ';
    }
    /* １行終わり */
    *gptr++ = 0;
    ic->glineifp[line].glkosu = lnko;
    ic->glineifp[line].gllen = WStrlen(ic->glineifp[line].gldata);
  }

  /* 最後にNULLを入れる */
  ic->kouhoifp[ko].khretsu = 0;
  ic->kouhoifp[ko].khpoint = 0;
  ic->kouhoifp[ko].khdata  = (wchar_t *)NULL;
  ic->glineifp[line].glkosu  = 0;
  ic->glineifp[line].glhead  = 0;
  ic->glineifp[line].gllen   = 0;
  ic->glineifp[line].gldata  = (wchar_t *)NULL;

#ifdef DEBUG
  if (iroha_debug) {
    int i;
    for(i=0; ic->glineifp[i].glkosu; i++)
      printf("%d: %s\n", i, ic->glineifp[i].gldata);
  }
#endif

  return(0);
}

tanKouhoIchiran(d, step)
uiContext d;
int step;
{
  yomiContext yc = (yomiContext)d->modec;
  ichiranContext ic;
  int nelem, currentkouho, retval = 0;
  unsigned char inhibit = 0;
  unsigned char listcallback = (unsigned char)(d->list_func ? 1 : 0);
  int netwidth;

  netwidth = d->ncolumns -
    (kCount ? (DEC_COLUMNS(9999) * 2 + 2/* 2は / と SP の分 */) : 0);

  /* 候補一覧行が狭くて候補一覧が出せない */
  if (listcallback == 0 && netwidth < 2) {
    /* tooSmall */
    return TanNextKouho(d);
  }

  /* 逐次関連 */
  yc->status |= CHIKUJI_OVERWRAP;

  /* すべての候補を取り出す */
  yc->allkouho = getIchiranList(yc->context, &nelem, &currentkouho);
  if (yc->allkouho == 0) {
    if (errno == EPIPE) {
      jrKanjiPipeError();
    }
    TanMuhenkan(d);
    makeGLineMessageFromString(d, jrKanjiError);
    return 0;
  }

  if ( !HexkeySelect ) {
    inhibit |= (unsigned char)NUMBERING;
  }

  yc->curIkouho = currentkouho;	/* 現在のカレント候補番号を保存する */
  currentkouho = step;	/* カレント候補から何番目をカレント候補とするか */

  /* 候補一覧に移行する */
  if ((retval = selectOne(d, yc->allkouho, &yc->curIkouho, nelem, BANGOMAX,
			  inhibit, currentkouho, WITH_LIST_CALLBACK,
			  ichiranEveryTimeCatch, ichiranExitCatch,
			  ichiranQuitCatch, 0)) == NG) {
    freeGetIchiranList(yc->allkouho);
    return GLineNGReturn(d);
  }

  ic = (ichiranContext)d->modec;
  if (ic->tooSmall) {
    freeGetIchiranList(yc->allkouho);
    popIchiranMode(d);
    popCallback(d);
    return TanNextKouho(d);
  }

  ic->minorMode = CANNA_MODE_IchiranMode;
  currentModeInfo(d);

  if ( !(ic->flags & ICHIRAN_ALLOW_CALLBACK) ) {
    makeGlineStatus(d);
  }
  /* d->status = EVERYTIME_CALLBACK; */

  return(retval);
}

/*
 * 候補一覧行の表示を強制終了する
 */
IchiranQuit(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int retval = 0;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    if (ic->flags & ICHIRAN_NEXT_EXIT) {
      d->list_func(d->client_data, CANNA_LIST_Select, 0, 0, 0);
    }
    else {
      d->list_func(d->client_data, CANNA_LIST_Quit, 0, 0, 0);
    }
  }
  
  if (ic->flags & ICHIRAN_NEXT_EXIT) {
    ichiranFin(d); 
    d->status = EXIT_CALLBACK;
  }
  else {
    *(ic->curIkouho) = ic->nIkouho - 1; /* ひらがな候補にする */
    ichiranFin(d); 
    d->status = QUIT_CALLBACK;
  }
  return(retval);
}

int
IchiranNop(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  /* currentModeInfo でモード情報が必ず返るようにダミーのモードを入れておく */
  d->majorMode = d->minorMode = CANNA_MODE_AlphaMode;
  currentModeInfo(d);
  if ( !(ic->flags & ICHIRAN_ALLOW_CALLBACK) ) {
    makeGlineStatus(d);
  }
  return 0;
}

/*
 * 次候補に移動する
 *
 * ・カレント候補が最終候補だったら先頭候補をカレント候補とする
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
IchiranForwardKouho(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    d->list_func(d->client_data, CANNA_LIST_Forward, 0, 0, 0);
    return 0;
  }

  /* 次候補にする (単語候補一覧状態で、最後の候補だったら一覧をやめる) */
  *(ic->curIkouho) += 1;
  if(*(ic->curIkouho) >= ic->nIkouho) {
    if(QuitIchiranIfEnd
       && (((coreContext)d->modec)->minorMode == CANNA_MODE_IchiranMode)) {
      return(IchiranQuit(d));
    } else if(CursorWrap) {
      *(ic->curIkouho) = 0;
    } else {
      *(ic->curIkouho) -= 1;
      return NothingChangedWithBeep(d);
    }
  }

  if(ic->tooSmall) { /* for bushu */
    d->status = AUX_CALLBACK;
    return 0;
  }

  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return 0;
}

/*
 * 前候補に移動する
 *
 * ・カレント候補が先頭候補だったら最終候補をカレント候補とする
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
IchiranBackwardKouho(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int mode;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    d->list_func(d->client_data, CANNA_LIST_Backward, 0, 0, 0);
    return 0;
  }

  /* 現在のモードを求める */
  if(QuitIchiranIfEnd)
    mode = ((coreContext)d->modec)->minorMode;

  /* 前候補にする (単語候補一覧状態で、最初の候補だったら一覧をやめる) */
  if(*(ic->curIkouho))
    *(ic->curIkouho) -= 1;
  else {
    if(QuitIchiranIfEnd && (mode == CANNA_MODE_IchiranMode)) {
      return(IchiranQuit(d));
    } else if(CursorWrap) {
      *(ic->curIkouho) = ic->nIkouho - 1;
    } else {
      *(ic->curIkouho) = 0;
      return NothingChangedWithBeep(d);
    }
  }

  if(ic->tooSmall) { /* for bushu */
    d->status = AUX_CALLBACK;
    return 0;
  }

  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return 0;
}

/*
 * 前候補列に移動する
 *
 * ・カレント候補を求めて候補一覧とその場の候補を表示する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
IchiranPreviousKouhoretsu(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    d->list_func(d->client_data, CANNA_LIST_Prev, 0, 0, 0);
    return 0;
  }

  if(ic->tooSmall) { /* for bushu */
    return(IchiranBackwardKouho(d));
  }

  /* 前候補列にする (*(ic->curIkouho)を求める)*/
  getIchiranPreviousKouhoretsu(d);

  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return 0;
}

/*
 * 前候補列のカレント候補を求める
 *
 * ・前候補列中の同じ候補番号のものをカレント候補とする
 * ・候補番号の同じものがない時は、その候補中の最終候補をカレント候補とする
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static void
getIchiranPreviousKouhoretsu(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int index;
  int curretsu, nretsu;

  /* カレント候補行のなかで何番目の候補かなのかを得る */
  index = *(ic->curIkouho) - 
    ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glhead;
  /* 前候補列を得る */
  curretsu = ic->kouhoifp[*(ic->curIkouho)].khretsu;
  nretsu = ic->kouhoifp[ic->nIkouho - 1].khretsu + 1;
  if(curretsu == 0) {
    if(CursorWrap)
      curretsu = nretsu;
    else {
      NothingChangedWithBeep(d);
      return;
    }
  }
  curretsu -= 1;
  /* index がカレント候補列の候補数より大きくなってしまったら
     最右候補をカレント候補とする */
  if(ic->glineifp[curretsu].glkosu <= index) 
    index = ic->glineifp[curretsu].glkosu - 1;
  /* 前候補列の同じ番号に移動する */
  *(ic->curIkouho) = index + ic->glineifp[curretsu].glhead;
  return;
}

/*
 * 次候補列に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
IchiranNextKouhoretsu(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    d->list_func(d->client_data, CANNA_LIST_Next, 0, 0, 0);
    return 0;
  }

  if(ic->tooSmall) {
    return(IchiranForwardKouho(d));
  }

  /* 次候補列にする (*(ic->curIkouho) を求める) */
  getIchiranNextKouhoretsu(d);

  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return 0;
}

/*
 * 次候補列に移動する
 *
 * ・次候補列中の同じ候補番号のものをカレント候補とする
 * ・候補番号の同じものがない時は、その候補中の最終候補をカレント候補とする
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static void
getIchiranNextKouhoretsu(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int index;
  int curretsu, nretsu;

  /* カレント候補行のなかで何番目の候補かなのかを得る */
  index = *(ic->curIkouho) - 
    ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glhead;
  /* 次候補列を得る */
  curretsu = ic->kouhoifp[*(ic->curIkouho)].khretsu;
  nretsu = ic->kouhoifp[ic->nIkouho - 1].khretsu + 1;
  curretsu += 1;
  if(curretsu >= nretsu) {
    if(CursorWrap)
      curretsu = 0;
    else {
      NothingChangedWithBeep(d);
      return;
    }
  }
  /* index がカレント候補列の候補数より大きくなってしまったら
     最右候補をカレント候補とする */
  if(ic->glineifp[curretsu].glkosu <= index) 
    index = ic->glineifp[curretsu].glkosu - 1;
  /* 前候補列の同じ番号に移動する */
  *(ic->curIkouho) = index + ic->glineifp[curretsu].glhead;
  return;
}

/*
 * 候補行中の先頭候補に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
IchiranBeginningOfKouho(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    d->list_func(d->client_data, CANNA_LIST_BeginningOfLine, 0, 0, 0);
    return 0;
  }

  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return 0;
  }

  /* 候補列の先頭候補をカレント候補にする */
  *(ic->curIkouho) = 
    ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glhead;

  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return 0;
}

/*
 * 候補行中の最右候補に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
IchiranEndOfKouho(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  if (ic->flags & ICHIRAN_ALLOW_CALLBACK &&
      d->list_func) {
    d->list_func(d->client_data, CANNA_LIST_EndOfLine, 0, 0, 0);
    return 0;
  }

  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return 0;
  }

  /* 候補列の最右候補をカレント候補にする */
  *(ic->curIkouho) = 
    ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glhead
    + ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glkosu - 1;

  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return 0;
}

/*
 * 候補行中の入力された番号の候補に移動する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */

static int getIchiranBangoKouho();

static
IchiranBangoKouho(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int zflag, retval = 0;

  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return(retval);
  }

  /* d->status = EVERYTIME_CALLBACK; */

  if (HexkeySelect && !(ic->inhibit & NUMBERING)) {
    /* 入力された番号の候補をカレント候補とする */
    if((zflag = getIchiranBangoKouho(d)) == NG)
      goto insert;

    /* SelectDirect のカスタマイズの処理 */
    if(SelectDirect) /* ON */ {
      if(zflag) /* ０が入力された */
	retval = IchiranQuit(d);
      else
	retval = IchiranKakutei(d);
    } else {          /* OFF */
      makeGlineStatus(d);
      /* d->status = EVERYTIME_CALLBACK; */
    }
    return(retval);
  }
  else {
    extern allowNextInput;
  insert:
    if(!(ic->inhibit & CHARINSERT) && allowNextInput) {
      BYTE ifl = ic->flags;
      retval = IchiranKakutei(d);
      if (ifl & ICHIRAN_STAY_LONG) {
	(void)IchiranQuit(d);
      }
      d->more.todo = 1;
      d->more.ch = d->ch;
      d->more.fnum = CANNA_FN_FunctionalInsert;
    } else {
      NothingChangedWithBeep(d);
    }
    return(retval);
  }
}

/*
 * 候補行中の入力された番号の候補に移動する
 *
 *
 * 引き数	uiContext
 * 戻り値	０が入力されたら              １を返す
 * 		１〜９、ａ〜ｆが入力されたら  ０を返す
 * 		エラーだったら              ー１を返す
 */
static int
getIchiranBangoKouho(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int num, index;

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
    return(NG);
  }
  /* 入力データは 候補行の中に存在する数か？ */
  if(num > ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glkosu) {
    /* 入力された番号は正しくありません */
    return(NG);
  }

  /* 入力された数が０で SelectDirect が ON なら読みに戻して１を返す */
  if(num == 0) {
    if(SelectDirect)
      return(1);
    else {
      /* 入力された番号は正しくありません */
      return(NG);
    }  
  } else {
    /* 候補列の先頭候補を得る */
    index = ic->glineifp[ic->kouhoifp[*(ic->curIkouho)].khretsu].glhead;
    *(ic->curIkouho) = index + (num - 1);
  }

  return(0);
}

/*
 * 候補行中から選択された候補をカレント候補とする
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static
IchiranKakutei(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;
  int retval = 0;
  wchar_t *kakuteiStrings;

  if ((ic->flags & (ICHIRAN_ALLOW_CALLBACK | ICHIRAN_STAY_LONG)) ==
      ICHIRAN_ALLOW_CALLBACK && d->list_func) {
    /* ICHIRAN_ALLOW_CALLBACK が立っていて、ICHIRAN_STAY_LONG が寝ている時で、
       かつ、d->list_func がちゃんとあるときは以下をする */
    d->list_func(d->client_data, CANNA_LIST_Select, 0, 0, 0);
  }

  kakuteiStrings = ic->allkouho[*(ic->curIkouho)];
  retval = d->nbytes = WStrlen(kakuteiStrings);
  WStrcpy(d->buffer_return, kakuteiStrings);

  if (ic->flags & ICHIRAN_STAY_LONG) {
    ic->flags |= ICHIRAN_NEXT_EXIT; 
    d->status = EVERYTIME_CALLBACK;
  }
  else {
    ichiranFin(d);

    d->status = EXIT_CALLBACK;
  }

  return(retval);
}

/*
 * 候補行表示モードから抜ける
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
ichiranFin(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec; 

  /* 候補一覧表示行用のエリアをフリーする */
  freeIchiranBuf(ic);

  popIchiranMode(d);

  /* gline をクリアする */
  GlineClear(d);
}

#include	"ichiranmap.c"


