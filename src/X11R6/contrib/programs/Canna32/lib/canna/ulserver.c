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
static char rcs_id[] = "@(#) 102.1 $Id: ulserver.c,v 5.15 1994/03/10 13:15:32 kon Exp $";
#endif

#include	<errno.h>
#include "canna.h"

extern int  errno;

static int serverChangeDo();

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * サーバの切り離し                                                          *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

serverFin(d)
uiContext d;
{
  int retval = 0;

  d->status = 0;
  killmenu(d);

  jrKanjiPipeError();
  
  makeGLineMessageFromString(d, "かな漢字変換サーバとの接続を切りました");
  currentModeInfo(d);

  return(retval);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * サーバの切り換え                                                          *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuServerChangeEveryTimeCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  int len, echoLen, revPos;
  static int lmachinename;
  static wchar_t *wmachinename;

  if (!wmachinename) {
    wchar_t xxx[30]; /* 30 ってのは "マシン名?[" よりは長いべということ */
    lmachinename = MBstowcs(xxx, "マシン名?[", 30);
    wmachinename = (wchar_t *)malloc((lmachinename + 1)* sizeof(wchar_t));
    if (!wmachinename) {
      return -1;
    }
    WStrcpy(wmachinename, xxx);
  }

  if((echoLen = d->kanji_status_return->length) < 0)
    return(retval);

  if (echoLen == 0) {
    d->kanji_status_return->revPos = 0;
    d->kanji_status_return->revLen = 0;
  }

  WStrncpy(d->genbuf + lmachinename,
	   d->kanji_status_return->echoStr, echoLen);
  /* echoStr == d->genbuf だとまずいので先に動かす */
  WStrncpy(d->genbuf, wmachinename, lmachinename);
  revPos = len = lmachinename;
  len += echoLen;
  d->genbuf[len++] = (wchar_t)']';

  d->kanji_status_return->gline.line = d->genbuf;
  d->kanji_status_return->gline.length = len;
  if (d->kanji_status_return->revLen) {
    d->kanji_status_return->gline.revPos =
      d->kanji_status_return->revPos + revPos;
    d->kanji_status_return->gline.revLen = d->kanji_status_return->revLen;
  }
  else { /* 反転領域がない場合 */
    d->kanji_status_return->gline.revPos = len - 1;
    d->kanji_status_return->gline.revLen = 1;
  }
  d->kanji_status_return->info &= ~(KanjiThroughInfo | KanjiEmptyInfo);
  d->kanji_status_return->info |= KanjiGLineInfo;
  echostrClear(d);
  checkGLineLen(d);

  return retval;
}

static
uuServerChangeExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 読みを pop */

  return(serverChangeDo(d, retval));
}

static
uuServerChangeQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 読みを pop */

  return prevMenuIfExist(d);
}

extern char *RkGetServerName();

serverChange(d)
uiContext d;
{
  yomiContext yc;
  int retval = 0;
  wchar_t *w;
  extern KanjiModeRec yomi_mode;
  extern defaultContext;

  d->status = 0;

  if ((yc = GetKanjiString(d, (wchar_t *)NULL, 0,
		     CANNA_ONLY_ASCII,
		     CANNA_YOMI_CHGMODE_INHIBITTED,
		     CANNA_YOMI_END_IF_KAKUTEI,
		     CANNA_YOMI_INHIBIT_ALL,
		     uuServerChangeEveryTimeCatch, uuServerChangeExitCatch,
		     uuServerChangeQuitCatch))
      == (yomiContext)0) {
    killmenu(d);
    return NoMoreMemory();
  }
  yc->minorMode = CANNA_MODE_ChangingServerMode;
  if(defaultContext != -1) {
    char *servname;
    servname = RkGetServerName();
    if (servname && (w = WString(servname)) != (wchar_t *)0) {
      RomajiStoreYomi(d, w, (wchar_t *)0);
      WSfree(w);
      yc->kRStartp = yc->kCurs = 0;
      yc->rStartp = yc->rCurs = 0;
      d->current_mode = &yomi_mode;
      makeYomiReturnStruct(d);
    }
  }
  currentModeInfo(d);

  return(retval);
}
		 
static
serverChangeDo(d, len)
uiContext d;
int len;
{
/* wchar_t で良いか？ 256 で良いか？ */
  wchar_t newServerName[256];
  wchar_t w1[512];
  char tmpServName[256];
  extern defaultContext;
  char *p;

  d->status = 0;

  if(!len)
    return(serverChange(d));

  WStrncpy(newServerName, d->buffer_return, len);
  newServerName[len] = 0;
#ifdef DEBUG
  if(iroha_debug)
    printf("iroha_server_name = [%s]\n", newServerName);
#endif

  jrKanjiPipeError();
  WCstombs(tmpServName, newServerName, 256);
  if (RkSetServerName(tmpServName) && (p = index((char *)tmpServName, '@'))) {
    char xxxx[1024];
    *p = '\0';
    sprintf(xxxx, "かな漢字変換エンジン %s は利用できません\n",
	    tmpServName);
    makeGLineMessageFromString(d, xxxx);

    RkSetServerName((char *)0);
    currentModeInfo(d);
    killmenu(d);
    return 0;
  }

  if(defaultContext == -1) {
    if((KanjiInit() != 0) || (defaultContext == -1)) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      killmenu(d);
      return(GLineNGReturn(d));
    }
    d->contextCache = -1;
  }

  p = RkGetServerName();
  if (p) { /* 絶対成功するんだけどね */
    if ((int)strlen(p) < 256) {
      MBstowcs(newServerName, p, 256);
    }
  }

  MBstowcs(w1, " のかな漢字変換サーバに接続しました", 512);
  WStrcpy((wchar_t *)d->genbuf, (wchar_t *)newServerName);
  WStrcat((wchar_t *)d->genbuf, (wchar_t *)w1);

  makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
  killmenu(d);
  currentModeInfo(d);

  return(0);
}
