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
static char rcs_id[] = "@(#) 102.1 $Id: bunsetsu.c,v 2.10 1994/03/01 13:40:47 kon Exp $";
#endif	/* lint */

#include <errno.h>
#include "canna.h"

extern int BunsetsuKugiri;

int
enterAdjustMode(d, yc)
uiContext d;
yomiContext yc;
{
  extern KanjiModeRec bunsetsu_mode;
  int i, n = 0;
  RkStat rst;

  for (i = 0 ; i < yc->curbun ; i++) {
    if (RkwGoTo(yc->context, i) == -1) {
      return makeRkError(d, "文節の移動に失敗しました");
    }
    if (RkwGetStat(yc->context, &rst) == -1) {
      return makeRkError(d, "漢字の読みを取り出せませんでした");
    }
    n += rst.ylen;
  }
  yc->kanjilen = n;
  /* カレント文節の読みの長さを取り出す */
  if (RkwGoTo(yc->context, yc->curbun) == -1) {
    return makeRkError(d, "文節の移動に失敗しました");
  }
  if (RkwGetStat(yc->context, &rst) == -1) {
    return makeRkError(d, "漢字の読みを取り出せませんでした");
  }
  yc->bunlen = rst.ylen;

  yc->tanMode = yc->curMode;
  yc->tanMinorMode = yc->minorMode;
  yc->minorMode = CANNA_MODE_AdjustBunsetsuMode;
  d->current_mode = yc->curMode = &bunsetsu_mode;
  return 0;
}

int leaveAdjustMode pro((uiContext, yomiContext));

int
leaveAdjustMode(d, yc)
uiContext d;
yomiContext yc;
{
  extern KanjiModeRec bunsetsu_mode;

  yc->bunlen = yc->kanjilen = 0;
  yc->minorMode = yc->tanMinorMode;
  d->current_mode = yc->curMode = yc->tanMode;
  return 0;
}

static
BunFullExtend(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  yc->bunlen = yc->kEndp - yc->kanjilen;
  makeKanjiStatusReturn(d, yc);
  return 0;
}

static
BunFullShrink(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  yc->bunlen = 1;
  makeKanjiStatusReturn(d, yc);
  return 0;
}

extern CursorWrap; /* 右端に行って右に行こうとしたとき左端に行くかどうか */

static
BunExtend(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->kanjilen + yc->bunlen < yc->kEndp) {
    /* まだ伸ばせる */

    yc->bunlen++;
    makeKanjiStatusReturn(d, yc);
    return 0;
  }
  else if (CursorWrap) {
    return BunFullShrink(d);
  }
  (void)NothingChangedWithBeep(d);
  return 0;
}

static
BunShrink(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->bunlen > 0) {
    /* まだ縮まる */
    int newlen = yc->bunlen;

    newlen--;
    if (newlen > 0) {
      yc->bunlen = newlen;
      makeKanjiStatusReturn(d, yc);
      return 0;
    }
    else if (CursorWrap) {
      return BunFullExtend(d);
    }
  }
  (void)NothingChangedWithBeep(d);
  return 0;
}

static
BunHenkan(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  yc->nbunsetsu = RkwResize(yc->context, yc->bunlen);
  leaveAdjustMode(d, yc);
  if (yc->nbunsetsu < 0) {
    makeRkError(d, "かな漢字変換に失敗しました");
    yc->nbunsetsu = 1/* dummy */;
    return TanMuhenkan(d);
  }
  makeKanjiStatusReturn(d, yc);
  currentModeInfo(d);
  return 0;
}

static
BunQuit(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  leaveAdjustMode(d, yc);
  makeKanjiStatusReturn(d, yc);
  currentModeInfo(d);
  return 0;
}

static
BunSelfInsert(d)
uiContext d;
{
  d->nbytes = BunQuit(d);
  d->more.todo = 1;
  d->more.ch = d->ch;
  d->more.fnum = CANNA_FN_FunctionalInsert;
  return d->nbytes;
}

static
BunQuotedInsert(d)
uiContext d;
{
  d->nbytes = BunQuit(d);
  d->more.todo = 1;
  d->more.ch = d->ch;
  d->more.fnum = CANNA_FN_QuotedInsert;
  return d->nbytes;
}

static
BunKillToEOL(d)
uiContext d;
{
  d->nbytes = BunQuit(d);
  d->more.todo = 1;
  d->more.ch = d->ch;
  d->more.fnum = CANNA_FN_KillToEndOfLine;
  return d->nbytes;
}

#include "bunmap.c"

/* 残っているお仕事

 ・逐次自動変換中の文節伸縮モード
 */
