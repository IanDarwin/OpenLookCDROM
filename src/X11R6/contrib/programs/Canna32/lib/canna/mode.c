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
static char rcs_id[] = "@(#) 102.1 $Id: mode.c,v 7.12 1994/03/13 09:13:21 kon Exp $";
#endif /* lint */

#include "canna.h"
#include <canna/mfdef.h>

extern int howToReturnModeInfo;
static wchar_t numMode[2];
static wchar_t *bad = (wchar_t *)0;
struct ModeNameRecs ModeNames[CANNA_MODE_MAX_IMAGINARY_MODE];

static
char *
_sModeNames[CANNA_MODE_MAX_IMAGINARY_MODE] = {
  "      ",		/* AlphaMode */
  "[ あ ]",	/* EmptyMode */
  "[記号]",	/* KigoMode */
  "[よみ]",	/* YomiMode (モード文字列表示には使わない) */
  "[字種]",	/* JishuMode */
  "[漢字]",	/* TanKouhoMode */
  "[一覧]",	/* IchiranMode */
  "[質問]",	/* YesNoMode */
  NULL,	/* OnOffMode */
  "[文節]",    /* AdjustBunsetsuMode */
  "[逐次]",	/* ChikujiYomiMode    逐次の時の読み部分 */
  "[逐次]",    /* ChikujiHenkanMode  逐次の時の変換の部分 */

  /* Imaginary Mode */

  "[ あ ]",	/* HenkanNyuryokuMode */
  "[全あ]",	/* ZenHiraHenkanMode */
  "[半あ]",	/* HanHiraHenkanMode */
  "[全ア]",	/* ZenKataHenkanMode */
  "[半ア]",	/* HanKataHenkanMode */
  "[全英]",	/* ZenAlphaHenkanMode */
  "[半英]",	/* HanAlphaHenkanMode */
  "<全あ>",	/* ZenHiraKakuteiMode */
  "<半あ>",	/* HanHiraKakuteiMode */
  "<全ア>",	/* ZenKataKakuteiMode */
  "<半ア>",	/* HanKataKakuteiMode */
  "<全英>",	/* ZenAlphaKakuteiMode */
  "<半英>",	/* HanAlphaKakuteiMode */
  "[16進]",	/* HexMode */
  "[部首]",	/* BushuMode */
  "[拡張]",	/* ExtendMode */
  "[ ロ ]",	/* RussianMode */
  "[ ギ ]",	/* GreekMode */
  "[罫線]",	/* LineMode */
  "[変更]",	/* ChangingServerMode */
  "[変換]",	/* HenkanMethodMode */
  "[削除]",	/* DeleteDicMode */
  "[登録]",	/* TourokuMode */
  "[品詞]",	/* TourokuHinshiMode */
  "[辞書]",	/* TourokuDicMode */
  "[ ｑ ]",	/* QuotedInsertMode */
  "[編集]",	/* BubunMuhenkanMode */
  "[辞書]",  /* MountDicMode */
  };

static wchar_t * _ModeNames[CANNA_MODE_MAX_IMAGINARY_MODE];

extern extraFunc *FindExtraFunc();
#define findExtraMode(mnum) \
 FindExtraFunc((mnum) - CANNA_MODE_MAX_IMAGINARY_MODE + CANNA_FN_MAX_FUNC)

newmode *
findExtraKanjiMode(mnum)
int mnum;
{
  extern extraFunc *extrafuncp;
  extraFunc *extrafunc;
  register int fnum =
    mnum - CANNA_MODE_MAX_IMAGINARY_MODE + CANNA_FN_MAX_FUNC;

  for (extrafunc = extrafuncp; extrafunc; extrafunc = extrafunc->next) {
    if (extrafunc->fnum == fnum) {
      switch (extrafunc->keyword) {
      case EXTRA_FUNC_DEFMODE:
	return extrafunc->u.modeptr;
      default:
	return (newmode *)0;
      }
    }
  }
  return (newmode *)0;
}

extern nothermodes;

static wchar_t *
modestr(mid)
int mid;
{
  if (mid < CANNA_MODE_MAX_IMAGINARY_MODE) {
      return(ModeNames[mid].name);
  }
  else if (mid - CANNA_MODE_MAX_IMAGINARY_MODE < nothermodes) {
    extraFunc *ep = findExtraMode(mid);
    if (ep) {
      return ep->display_name;
    }
  }
  return (wchar_t *)0;
}
    
currentModeInfo(d)
uiContext d;
{
  coreContext cc = (coreContext)d->modec;

  if (d->current_mode->flags & CANNA_KANJIMODE_EMPTY_MODE) {
    d->kanji_status_return->info |= KanjiEmptyInfo;
  }

  if (howToReturnModeInfo == ModeInfoStyleIsString) {
    wchar_t *modename, *gmodename;
    if (cc->minorMode != d->minorMode) {
      modename = modestr(cc->minorMode);
      gmodename = modestr(d->minorMode);
      if (modename && (gmodename == (wchar_t *)NULL ||
		       WStrcmp(modename, gmodename))) {
	d->majorMode = cc->majorMode;
	d->minorMode = cc->minorMode;
	d->kanji_status_return->mode = modename;
	d->kanji_status_return->info |= KanjiModeInfo;
      }
    }
  }
  else {
    if (cc->majorMode != d->majorMode) {
      d->majorMode = cc->majorMode;
      d->minorMode = cc->minorMode;
      numMode[0] = (wchar_t)('@' + cc->majorMode);
      numMode[1] = (wchar_t) 0;
      d->kanji_status_return->info |= KanjiModeInfo;
      d->kanji_status_return->mode = numMode;
    }
  }
}
     
/* このファイルにはモード変更に関する操作一般が入っている。モードの変
 * 更とは、ローマ字かな変換の表面に現れるモードの変更だけではなく、全
 * く読みがない状態から、読みがある状態に移る時にも生じるものを指す。
 */

initModeNames()
{
  int i;
  
  for (i = 0 ; i < CANNA_MODE_MAX_IMAGINARY_MODE ; i++) {
    ModeNames[i].alloc = 0;
    ModeNames[i].name = _ModeNames[i] = 
      _sModeNames[i] ? WString(_sModeNames[i]) : 0;
  }
  if (!bad) {
    bad = WString("メモリが足りません");
  }
}

resetModeNames()
{
  int i;

  for (i = 0 ; i < CANNA_MODE_MAX_IMAGINARY_MODE ; i++) {
    if (ModeNames[i].alloc && ModeNames[i].name) {
      ModeNames[i].alloc = 0;
      WSfree(ModeNames[i].name);
    }
    ModeNames[i].name = _ModeNames[i];
  }
}

static
japaneseMode(d)
uiContext d;
{
  coreContext cc = (coreContext)d->modec;

  d->current_mode = cc->prevMode;
  d->modec = cc->next;
  freeCoreContext(cc);
  d->status = EXIT_CALLBACK;
}

/* cfuncdef

  JapaneseMode(d) -- モードを日本語入力モードに変える。

  ※注 この関数はαモードでしか呼んではいけない。

 */

JapaneseMode(d)
uiContext d;
{
  coreContext cc = (coreContext)d->modec;
  yomiContext yc = (yomiContext)cc->next;

  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    return NothingChangedWithBeep(d);
  }

  japaneseMode(d);
  d->kanji_status_return->length = 0;
  return 0;
}

AlphaMode(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    return NothingChangedWithBeep(d);
  }
  else {
    alphaMode(d);
    currentModeInfo(d);
    d->kanji_status_return->length = 0;
    return 0;
  }
}

HenkanNyuryokuMode(d)
uiContext d;
{
  extern KanjiModeRec empty_mode;
  yomiContext yc = (yomiContext)d->modec;

  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    return NothingChangedWithBeep(d);
  }

  yc->generalFlags &= (~CANNA_YOMI_ATTRFUNCS & ~CANNA_YOMI_IGNORE_USERSYMBOLS &
		       ~CANNA_YOMI_BASE_HANKAKU);
  d->current_mode = yc->myEmptyMode = &empty_mode;
  yc->majorMode = yc->minorMode = CANNA_MODE_EmptyMode;
  yc->myMinorMode = 0; /* 0 は AlphaMode と重なるが問題はない */
  yc->romdic = romajidic;
  EmptyBaseModeInfo(d, yc);

  if(yc->rCurs)
    return RomajiFlushYomi(d, 0, 0); /* これは暫定的 */

  d->kanji_status_return->length = 0;
  return 0;
}

queryMode(d, arg)
uiContext d;
wchar_t *arg;
{
  coreContext cc = (coreContext)d->modec;
  wchar_t *mode_str = (wchar_t *)0;
  extraFunc *ep;

  switch (howToReturnModeInfo) {
  case ModeInfoStyleIsString:
    if (d->minorMode < (BYTE)CANNA_MODE_MAX_IMAGINARY_MODE) {
      mode_str = ModeNames[d->minorMode].name;
    }
    else if (d->minorMode <
	     (BYTE)(CANNA_MODE_MAX_IMAGINARY_MODE + nothermodes)) {
      ep = findExtraMode(d->minorMode);
      if (ep) {
	mode_str = ep->display_name;
      }
    }
    if (!mode_str) {
      int ii;
      for (ii = 0; ii < 4; ii++, arg++) {
	*arg = (wchar_t)'\0';
      }
    }
    else {
      WStrcpy(arg, mode_str);
    }
    break;
  case ModeInfoStyleIsExtendedNumeric:
    arg[1] = (wchar_t)('@' + (int)cc->minorMode);
  case ModeInfoStyleIsNumeric:
    arg[0] = (wchar_t)('@' + (int)cc->majorMode);
    break;
  default:
    return(-1);
    /* NOTREACHED */
    break;
  }
  return 0;
}

/* 
 *   あるモードに対してモード表示文字列を決定する。
 *
 */

changeModeName(modeid, str)
int modeid;
char *str;
{
  extraFunc *ep;

  if (modeid == CANNA_MODE_HenkanNyuryokuMode)
    modeid = CANNA_MODE_EmptyMode;

  if (modeid >= 0) {
    if (modeid < CANNA_MODE_MAX_IMAGINARY_MODE) {
      if (ModeNames[modeid].alloc && ModeNames[modeid].name) {
	WSfree(ModeNames[modeid].name);
      }
      if (str) {
	ModeNames[modeid].alloc = 1;
	ModeNames[modeid].name = WString(str);
      }
      else {
	ModeNames[modeid].alloc = 0;
	ModeNames[modeid].name = (wchar_t *)0;
      }
    }
    else if (modeid < (CANNA_MODE_MAX_IMAGINARY_MODE + nothermodes)) {
      ep = findExtraMode(modeid);
      if (ep) {
	if (ep->display_name) {
	  WSfree(ep->display_name);
	}
	if (str) {
	  ep->display_name = WString(str);
	}
	else {
	  ep->display_name = (wchar_t *)0;
	}
      }
      else {
	return -1;
      }
    }
    return 0;
  }
  return -1;
}

