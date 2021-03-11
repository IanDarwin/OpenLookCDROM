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
static	char	rcs_id[] = "@(#) 102.1 $Id: onoff.c,v 5.4 1994/03/01 14:06:59 kon Exp $";
#endif /* lint */

#include	<errno.h>
#include	"canna.h"

#define ICHISIZE 9

extern int kCount;

static int makeOnOffIchiran();

static wchar_t *black;
static wchar_t *white;
static wchar_t *space;

int
initOnoffTable()
{
  black = WString("◎");
  white = WString("○");
  space = WString("　");
}

static void
popOnOffMode(d)
uiContext d;
{
  ichiranContext oc = (ichiranContext)d->modec;

  d->modec = oc->next;
  d->current_mode = oc->prevMode;
  freeIchiranContext(oc);
}

/*
 * 候補一覧行を作る
 */
selectOnOff(d, buf, ck, nelem, bangomax, currentkouho, status,
	  everyTimeCallback, exitCallback, quitCallback, auxCallback)
uiContext d;
wchar_t **buf;
int *ck;
int nelem, bangomax;
int currentkouho;
unsigned char *status;
int (*everyTimeCallback)(), (*exitCallback)();
int (*quitCallback)(), (*auxCallback)();
{
  extern KanjiModeRec onoff_mode;
  ichiranContext oc;
  int retval = 0;
  ichiranContext newIchiranContext();

  if(pushCallback(d, d->modec,
	everyTimeCallback, exitCallback, quitCallback, auxCallback) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }
  
  if((oc = (ichiranContext)newIchiranContext()) == NULL) {
    popCallback(d);
    return(NG);
  }
  oc->next = d->modec;
  d->modec = (mode_context)oc;

  oc->prevMode = d->current_mode;
  d->current_mode = &onoff_mode;

  oc->allkouho = buf;
  oc->curIkouho = ck;

  if((retval = makeOnOffIchiran(d, nelem, bangomax,
			currentkouho, status))   == NG) {
    popOnOffMode(d);
    popCallback(d);
    return(NG);
  }
  return(retval);
}

/*
 * 候補一覧行を表示用のデータをテーブルに作成する
 *
 * ・glineinfo と kouhoinfoを作成する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static
makeOnOffIchiran(d, nelem, bangomax, currentkouho, status)
uiContext d;
int nelem, bangomax;
int currentkouho;
unsigned char *status;
{
  ichiranContext oc = (ichiranContext)d->modec;
  wchar_t **kkptr, *kptr, *gptr, *svgptr;
  int ko, lnko, cn = 0, svcn, line = 0, dn = 0, svdn;

  oc->nIkouho = nelem;	/* 候補の数 */

  /* カレント候補をセットする */
  oc->svIkouho = *(oc->curIkouho);
  *(oc->curIkouho) += currentkouho;
  if(*(oc->curIkouho) >= oc->nIkouho)
    oc->svIkouho = *(oc->curIkouho) = 0;

  if(allocIchiranBuf(d) == NG)
    return(NG);

  if(d->ncolumns < 1) {
    oc->tooSmall = 1;
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

  kkptr = oc->allkouho;
  kptr = *(oc->allkouho);
  gptr = oc->glinebufp;

  /* line -- 何列目か
     ko   -- 全体の先頭から何番目の候補か
     lnko -- 列の先頭から何番目の候補か
     cn   -- 列の先頭から何バイト目か */

  for(line=0, ko=0; ko<oc->nIkouho; line++) {
    oc->glineifp[line].gldata = gptr; /* 候補行を表示するための文字列 */
    oc->glineifp[line].glhead = ko;   /* この行の先頭候補は、全体でのko番目 */

    oc->tooSmall = 1;
    for(lnko = cn = dn = 0;
	dn<d->ncolumns - (kCount ? ICHISIZE + 1: 0) &&
	lnko<bangomax && ko<oc->nIkouho ; lnko++, ko++) {
      oc->tooSmall = 0;
      kptr = kkptr[ko];
      oc->kouhoifp[ko].khretsu = line; /* 何行目に存在するかを記録 */
      oc->kouhoifp[ko].khpoint = cn + (lnko ? 1 : 0);
      oc->kouhoifp[ko].khdata = kptr;  /* その文字列へのポインタ */
      svgptr = gptr;
      svcn = cn;
      svdn = dn;

      /* ◎か○をコピーする */
      if(lnko) {
	WStrncpy(gptr++, space, WStrlen(space));
	cn++; dn += 2;
      }
      if(status[ko] == 1)
	WStrncpy(gptr, black, WStrlen(black));
      else
	WStrncpy(gptr, white, WStrlen(white));	 
      cn ++; gptr++; dn +=2;
      /* 候補をコピーする */
      for(; *kptr && dn<d->ncolumns - (kCount ? ICHISIZE + 1: 0);
	  gptr++, kptr++, cn++) {
	if (((*gptr = *kptr) & 0x8080) == 0x8080) dn++;
        dn++;
      }

      /* カラム数よりはみだしてしまいそうになったので１つ戻す */
      if ((dn >= d->ncolumns - (kCount ? ICHISIZE + 1: 0)) && *kptr) {
	if (lnko) {
	  gptr = svgptr;
	  cn = svcn;
	  dn = svdn;
	}
	else {
	  oc->tooSmall = 1;
	}
	break;
      }
    }
    if (oc->tooSmall) {
      return 0;
    }
    if (kCount) {
      for (;dn < d->ncolumns - 1; dn++) {
	*gptr++ = ' ';
      }
    }
    /* １行終わり */
    *gptr++ = (wchar_t)0;
    oc->glineifp[line].glkosu = lnko;
    oc->glineifp[line].gllen = WStrlen(oc->glineifp[line].gldata);
  }
  /* 最後にNULLを入れる */
  oc->kouhoifp[ko].khretsu = 0;
  oc->kouhoifp[ko].khpoint = 0;
  oc->kouhoifp[ko].khdata  = (wchar_t *)NULL;
  oc->glineifp[line].glkosu  = 0;
  oc->glineifp[line].glhead  = 0;
  oc->glineifp[line].gllen   = 0;
  oc->glineifp[line].gldata  = (wchar_t *)NULL;

#ifdef DEBUG
  if (iroha_debug) {
    int i;
    for(i=0; oc->glineifp[i].glkosu; i++)
      printf("%d: %s\n", i, oc->glineifp[i].gldata);
  }
#endif

  return(0);
}

/*
 * カレント候補を現在と反対にする(ON→OFF, OFF→ON)
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static
OnOffSelect(d)
uiContext d;
{
  ichiranContext oc = (ichiranContext)d->modec;
  mountContext mc = (mountContext)oc->next;
  int point, retval = 0;
  wchar_t *gline;

  /* mountNewStatus を変更する (1→0, 0→1) */
  if(mc->mountNewStatus[*(oc->curIkouho)])
    mc->mountNewStatus[*(oc->curIkouho)] = 0;
  else
    mc->mountNewStatus[*(oc->curIkouho)] = 1;

  /* gline用のデータを書き換える (◎→○, ○→◎) */
  gline = oc->glineifp[oc->kouhoifp[*(oc->curIkouho)].khretsu].gldata;
  point = oc->kouhoifp[*(oc->curIkouho)].khpoint;

    *(gline+point) = ((mc->mountNewStatus[*(oc->curIkouho)]) ? *black : *white);
  makeGlineStatus(d);
  /* d->status = EVERYTIME_CALLBACK; */

  return(retval);
}

/*
 * status をそのまま返し、OnOffモードをPOPする (EXIT_CALLBACK)
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static
OnOffKakutei(d)
uiContext d;
{
  ichiranContext oc = (ichiranContext)d->modec;
  int retval = 0;
/* いらないのでは unsigned char *kakuteiStrings;*/

  /* 候補一覧表示行用のエリアをフリーする */
  freeIchiranBuf(oc);

  popOnOffMode(d);

#ifdef DEBUG
  if(iroha_debug) {
    mountContext mc = (mountContext)d->modec;
    int i;

    printf("<★mount>\n");
    for(i= 0; mc->mountList[i]; i++)
      printf("[%s][%x][%x]\n", mc->mountList[i],
	     mc->mountOldStatus[i], mc->mountNewStatus[i]);
    printf("\n");
  }
#endif

  /* gline をクリアする */
  GlineClear(d);

  d->status = EXIT_CALLBACK;

  return(retval);
}

#include	"onoffmap.c"
