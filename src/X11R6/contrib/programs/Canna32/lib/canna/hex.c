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
static char rcs_id[] = "@(#) 102.1 $Id: hex.c,v 5.7 1994/03/10 02:24:57 kon Exp $";
#endif /* lint */

#include "canna.h"

#define HEXPROMPT "コード: "
#define HEXPROMPTLEN  5 /* "コード: " の長さは5バイト */

static int quitHex();

extern int hexCharacterDefiningStyle;

/* cfuncdef

  hexEveryTimeCatch -- 読みを１６進入力モードで表示する関数

 */

static
hexEveryTimeCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  yomiContext yc = (yomiContext)d->modec;
  static wchar_t buf[256];
  /* ??? このようなバッファをいろいろな部分で持つのは好ましくないので、
     uiContext にまとめて持って共有して使った方が良い */
  int codelen = d->kanji_status_return->length;

  d->kanji_status_return->info &= ~(KanjiThroughInfo | KanjiEmptyInfo);

  if (codelen >= 0) {
    MBstowcs(buf, HEXPROMPT, 256);
    WStrncpy(buf + HEXPROMPTLEN, d->kanji_status_return->echoStr, codelen);
    d->kanji_status_return->gline.line = buf;
    d->kanji_status_return->gline.length = codelen + HEXPROMPTLEN;
    d->kanji_status_return->gline.revPos =
      d->kanji_status_return->revPos + HEXPROMPTLEN;
    d->kanji_status_return->gline.revLen = d->kanji_status_return->revLen;
    d->kanji_status_return->info |= KanjiGLineInfo;
    echostrClear(d);
    if (codelen == 4) { /* ４文字になったときには.... */
      if (convertAsHex(d)) {
	yc->allowedChars = CANNA_NOTHING_ALLOWED;
	*(d->kanji_status_return->echoStr = yc->kana_buffer + yc->kEndp + 1)
	  = *(d->buffer_return);
	d->kanji_status_return->revPos = d->kanji_status_return->revLen = 0;
	d->kanji_status_return->length = 1;
	retval = 0;
	if (hexCharacterDefiningStyle != HEX_USUAL) {
	  d->more.todo = 1;
	  d->more.ch = d->ch;
	  d->more.fnum = CANNA_FN_Kakutei;
	}
      }
      else {
	Beep();
	d->more.todo = 1;
	d->more.ch = d->ch;
	d->more.fnum = CANNA_FN_DeletePrevious;
      }
    }
    else {
      yc->allowedChars = CANNA_ONLY_HEX;
    }
  }
  checkGLineLen(d);
  return retval;
}

static
exitHex(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  killmenu(d);
  if (cvtAsHex(d, d->buffer_return, d->buffer_return, d->nbytes)) {
    GlineClear(d);
    popCallback(d);
    retval = YomiExit(d, 1);
    currentModeInfo(d);
    return retval;
  }
  else {
    return quitHex(d, 0, env);
  }
}

static
quitHex(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  GlineClear(d);
  popCallback(d);
  currentModeInfo(d);
  return prevMenuIfExist(d);
}

yomiContext GetKanjiString();

static
hexMode(d, major_mode)
uiContext d;
int major_mode;
{
  yomiContext yc;

  yc = GetKanjiString(d, (wchar_t *)NULL, 0,
		      CANNA_ONLY_HEX,
		      CANNA_YOMI_CHGMODE_INHIBITTED,
		      CANNA_YOMI_END_IF_KAKUTEI,
		      CANNA_YOMI_INHIBIT_ALL,
		      hexEveryTimeCatch, exitHex, quitHex);
  if (yc == (yomiContext)0) {
    return NoMoreMemory();
  }
  yc->majorMode = major_mode;
  yc->minorMode = CANNA_MODE_HexMode;
  currentModeInfo(d);
  return 0;
}

/* cfuncdef

  HexMode -- １６進入力モードになるときに呼ばれる。

 */

HexMode(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    return NothingChangedWithBeep(d);
  }    

  return hexMode(d, CANNA_MODE_HexMode);
}

