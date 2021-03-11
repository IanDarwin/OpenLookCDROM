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
static char rcs_id[] = "$Id: chikuji.c,v 2.24 1994/03/11 09:59:59 kon Exp $";
#endif

#include	"canna.h"

#ifdef MEASURE_TIME
#include	<sys/types.h>
#include	<sys/times.h>
#endif /* MEASURE_TIME */

extern int yomiInfoLevel, Gakushu, nKouhoBunsetsu, KeepCursorPosition;
extern int defaultContext, kouho_threshold;
extern KanjiModeRec tankouho_mode, cy_mode, cb_mode;
extern void makeYomiReturnStruct pro((uiContext));

int forceRomajiFlushYomi pro((uiContext));
void moveToChikujiTanMode pro((uiContext));
void moveToChikujiYomiMode pro((uiContext));

static void clearHenkanContent pro((yomiContext));

static void
clearHenkanContent(yc)
yomiContext yc;
{
  yc->allkouho = 0;
  yc->kouhoCount = yc->curIkouho = 0;
  return;
}

void
clearHenkanContext(yc)
yomiContext yc;
{
  if (yc->context >= 0) {
    RkwCloseContext(yc->context);
    yc->context = -1;
  }
  yc->nbunsetsu = yc->curbun = 0;
  clearHenkanContent(yc);
  return;
}

extern NothingChanged pro((uiContext));

/*
  restoreChikujiYomi

  引数:
    d   : uiContext
    old : 前の文節数

  やること:
   (1) 文節数が増えたら規定数以上の文節を確定させる。
   (2) そのとき、RkwRemoveBun もする。
   (3) その分、読みも削る。
   (4) 文節になった分の読みをカウント。
 */

static int
restoreChikujiYomi(d, old)
     uiContext d;
     int old;
{
  yomiContext yc = (yomiContext)d->modec;
  wchar_t *s = d->buffer_return, *e = s + d->n_buffer;
  RkStat stat;
  int len, i, j, yomilen, ll = 0, m = 0, n = 0, recalc = 0;

  d->nbytes = 0;
  yomilen = yc->kEndp - yc->cStartp;
  if (yc->nbunsetsu) {
    yc->status |= CHIKUJI_ON_BUNSETSU;
    if (yc->nbunsetsu > old) {
      recalc = 1;
    }
    if (nKouhoBunsetsu) {
      (void)cutOffLeftSide(d, yc, nKouhoBunsetsu - yc->nbunsetsu);
      if (nKouhoBunsetsu < yc->nbunsetsu) {
	n = yc->nbunsetsu - nKouhoBunsetsu;
	if (n > old) {
	  n = old; /* 前にまだ漢字になっていなかった分までは確定させない */
	}
      }
    }
    if (n > 0) { /* 確定させる文節数 */

      recalc = 1;
      for (i = 0 ; i < n ; i++) {
	if (RkwGoTo(yc->context, i) < 0 ||
	    (len = RkwGetKanji(yc->context, s, e - s)) < 0 ||
	    RkwGetStat(yc->context, &stat) == -1) {
	  return -1;
	}
	s += len;

	ll += stat.ylen;
	m += stat.klen;
      }
      d->nbytes = s - d->buffer_return;
      if (s < e) {
	*s++ = (wchar_t)'\0';
      }

      if (RkwRemoveBun(yc->context, Gakushu ? 1 : 0) == -1) {
	return -1;
      }

      /* かなバッファとかも削る */
      kPos2rPos(yc, 0, ll, (int *)0, &j);

      if (yomiInfoLevel > 0) {
	d->kanji_status_return->info |= KanjiYomiInfo;
	len = xString(yc->kana_buffer, ll, s, e);
	s += len;
	if (s < e) {
	  *s++ = (wchar_t)'\0';
	}
	if (yomiInfoLevel > 1) {
	  len = xString(yc->romaji_buffer, j, s, e);
	  s += len;
	}
	if (s < e) {
	  *s++ = (wchar_t)'\0';
	}
      }
      
      removeKana(d, yc, ll, j);

      yc->nbunsetsu -= n;
    }
    if (RkwGoTo(yc->context, yc->nbunsetsu - 1) == -1)
      return(-1);
    yc->curbun = yc->nbunsetsu - 1;
    if (old < yc->curbun) { /* せめて前のやつの右に行く */
      yc->curbun = old;
    }
  }

  if (recalc) {
    yomilen = RkwGetLastYomi(yc->context, d->genbuf, ROMEBUFSIZE);
    if (yomilen == -1) {
      return -1;
    }
    if (yomilen < yc->kEndp) { /* 必ず真では？ */
      kPos2rPos(yc, 0, yc->kEndp - yomilen, (int *)0, &j);
      yc->cStartp = yc->kEndp - yomilen;
      yc->cRStartp = j;
    }
    yc->ys = yc->ye = yc->kEndp;
  }

  if (yc->nbunsetsu) {
    moveToChikujiTanMode(d);
  }
  return(0);
}

static int
doesSupportChikuji()
{
  int a, b;

  if (defaultContext == -1) {
    if (KanjiInit() < 0 || defaultContext == -1) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      return(-1);
    }
  }
  RkGetProtocolVersion(&a, &b);
  return(a > 1);
}

int
chikujiInit(d)
uiContext d;
{
  int chikuji_f;
  d->status = 0;
  killmenu(d);

  chikuji_f = doesSupportChikuji();

  if (ToggleChikuji(d, 1) == -1) {
    if(!chikuji_f)
      jrKanjiError = "サーバが逐次自動変換をサポートしていません";
    else
      jrKanjiError = "逐次自動変換に切替えることができません";
    makeGLineMessageFromString(d, jrKanjiError);
    currentModeInfo(d);
    return(-1);
  }
  else {
    if(!chikuji_f)
      makeGLineMessageFromString(d, 
		"サーバが逐次自動変換をサポートしていません");
    else
      makeGLineMessageFromString(d, "逐次自動変換に切替えました");
    currentModeInfo(d);
    return 0;
  }
}

static int
chikujiSubstYomi(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  int n = yc->nbunsetsu, ret;

  if (yc->context == -1) { /* 「出だしだったら」の意味もある */
    if (confirmContext(d, yc) < 0) {
      return -1;
    }
    if (!doesSupportChikuji()) {
      jrKanjiError = "サーバが逐次自動変換をサポートしていません";     
      abandonContext(d, yc);
      return(-1);
    }
    if (RkwBgnBun(yc->context, 0, 1, RK_XFER << RK_XFERBITS | RK_KFER) == NG) {
    substError:
      jrKanjiError = "逐次自動変換に失敗しました";
      /* 以下は何をやっているのかしら？ */
      if (TanMuhenkan(d) == -1) {
	return -2;
      }
      return(-1);
    }
  }
  yc->nbunsetsu = RkwSubstYomi(yc->context,
			       yc->ys - yc->cStartp, yc->ye - yc->cStartp,
			       yc->kana_buffer + yc->ys, yc->kEndp - yc->ys);
  yc->ys = yc->ye = yc->kEndp;
  if (yc->nbunsetsu < 0 || (ret = restoreChikujiYomi(d, n)) < 0) {
    goto substError;
  }
  return(ret);
}

ChikujiSubstYomi(d)
  uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if ((yc->ys == yc->ye && yc->kEndp == yc->ye)
      || yc->kEndp != yc->kCurs
      || !(yc->kAttr[yc->kEndp - 1] & HENKANSUMI)) {
    /* 新しい入力がなかったり、
       最後への入力じゃなかったり、
       最後の字がローマ字かな変換されつくしていなかったりしたら */
    return 0; /* なにもしない */
  }
  return chikujiSubstYomi(d);
}

int
ChikujiTanDeletePrevious(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  int i, j, l = 0, flg = 0;
  extern chikujiRealBackspace;
  RkStat stat;

  if (yc->cRStartp < yc->rEndp) { /* ローマ字が入っている */
    flg = 1;
  }
  d->nbytes = 0;

  /* まずカレント文節から後ろを読みに戻す */
  if (forceRomajiFlushYomi(d)) {
    return d->nbytes;
  }
  if (RkwSubstYomi(yc->context, 0, yc->ye - yc->cStartp, 0, 0) == NG) {
    /* 読みで残っている分を逐次のデータから消す */
    (void)makeRkError(d, "読みに戻すことができません");
    (void)TanMuhenkan(d); /* これ要るの？ */
    return 0;
  }
  yc->ys = yc->ye = yc->cStartp;
  for (i = yc->nbunsetsu - 1 ; i >= yc->curbun ; i--) {
    /* カレント文節から後ろを読みに戻すための準備 */
    if (RkwGoTo(yc->context, i) == NG
	|| RkwGetStat(yc->context, &stat) == NG
	|| RkwStoreYomi(yc->context, 0, 0) == NG) {
      (void)makeRkError(d, "読みに戻すことができません");
      TanMuhenkan(d); /* これ、要るの？ */
      return 0;
    }
    l += stat.ylen;
  }
  yc->nbunsetsu = yc->curbun;
  if (l) {
    i = j = 0;
    do {
      ++i;
      if (yc->kAttr[yc->cStartp - i] & SENTOU) {
	for (j++ ;
	     j < yc->cRStartp && !(yc->rAttr[yc->cRStartp - j] & SENTOU) ;) {
	  j++;
	}
      }
    } while (i < l);
    yc->cStartp = (i < yc->cStartp) ? yc->cStartp - i : 0;
    yc->cRStartp = (j < yc->cRStartp) ? yc->cRStartp - j : 0;
  }
  if (KeepCursorPosition && yc->kCurs != yc->kEndp) {
    yc->kRStartp = yc->kCurs = yc->cStartp;
    yc->rStartp = yc->rCurs = yc->cRStartp;
  }
  else {
    yc->kRStartp = yc->kCurs = yc->kEndp;
    yc->rStartp = yc->rCurs = yc->rEndp;
  }
  yc->ys = yc->ye = yc->cStartp;
  clearHenkanContent(yc);
  if (yc->curbun) {
    yc->curbun--;
  }
  yc->status |= CHIKUJI_OVERWRAP;
  moveToChikujiYomiMode(d);
  makeKanjiStatusReturn(d, yc);
  if (flg && chikujiRealBackspace && !KeepCursorPosition) {
    d->more.todo = 1;
    d->more.ch = 0;
    d->more.fnum = CANNA_FN_DeletePrevious;
    /* 同じことをまたやるって感じだぞ */
  }
  return 0;
}

/*
  chikuji_restore_yomi

    cStartp, cRStartp を調整する
 */

static int
chikuji_restore_yomi(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  int l, j;

  if ((l = RkwGetLastYomi(yc->context, d->genbuf, ROMEBUFSIZE)) == -1) {
    return makeRkError(d, "未決文節を取り出せませんでした");
  }
  if (l != yc->kEndp - yc->cStartp) { /* 変わったら */
    kPos2rPos(yc, 0, yc->kEndp - l, (int *)0, &j);
    yc->cStartp = yc->kEndp - l;
    yc->cRStartp = j;
  }

  yc->ys = yc->ye = yc->cStartp;
  return 0;
}

static int
chikuji_subst_yomi(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  int l, n = yc->nbunsetsu;

  /* 読みを全部食わせる */
  l = RkwSubstYomi(yc->context, yc->ys - yc->cStartp, yc->ye - yc->cStartp,
		   yc->kana_buffer + yc->ys, yc->kEndp - yc->ys);
  yc->ys = yc->ye = yc->kEndp;
  if (l == -1) {
    jrKanjiError = "変換に失敗しました";
    (void)TanMuhenkan(d);
    return -1;
  }
  yc->nbunsetsu = l;
  if (l > n) {
    yc->curbun = n; /* 前の文節がカレント */
  }
  return chikuji_restore_yomi(d);
}

static int
ChikujiTanExtend(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  int i;

  d->nbytes = 0;
  yc->kouhoCount = 0;

  if (yc->ys < yc->kEndp || yc->ye != yc->kEndp) {
    i = yc->curbun; /* とっとく */
    if (chikuji_subst_yomi(d) == -1) {
      makeGLineMessageFromString(d, jrKanjiError);
      return TanMuhenkan(d);
    }
    if (RkwGoTo(yc->context, i) == -1) {
      (void)makeRkError(d, "文節の移動に失敗しました");
      return TanMuhenkan(d);
    }
    yc->curbun = i;
  }
  if ((yc->nbunsetsu = RkwEnlarge(yc->context)) <= 0) {
    (void)makeRkError(d, "文節の拡大に失敗しました");
    return TanMuhenkan(d);
  }
  if (chikuji_restore_yomi(d) == NG) {
    return TanMuhenkan(d);
  }
  yc->status |= CHIKUJI_OVERWRAP;
  makeKanjiStatusReturn(d, yc);
  return d->nbytes;
}

static int
ChikujiTanShrink(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  RkStat stat;
  int i;

  d->nbytes = 0;
  yc->kouhoCount = 0;
  if (yc->ys < yc->kEndp || yc->ye != yc->kEndp) {
    i = yc->curbun;
    if (chikuji_subst_yomi(d) == -1) {
      makeGLineMessageFromString(d, jrKanjiError);
      return TanMuhenkan(d);
    }
    if (RkwGoTo(yc->context, i) == -1) {
      (void)makeRkError(d, "文節の縮小に失敗しました");
      return TanMuhenkan(d);
    }
    yc->curbun = i;
  }

  if (RkwGetStat(yc->context, &stat) < 0 || stat.ylen == 1) {
    /* これ以上短くできるかどうか確認。要る？ */
    return NothingForGLine(d);
  }
  yc->nbunsetsu = RkwShorten(yc->context);
  if (yc->nbunsetsu <= 0) { /* 0 ってことあんのかなあ？ */
    (void)makeRkError(d, "文節の縮小に失敗しました");
    return TanMuhenkan(d);
  }
  if (chikuji_restore_yomi(d) == NG) {
    return TanMuhenkan(d);
  }
  yc->status |= CHIKUJI_OVERWRAP;
  makeKanjiStatusReturn(d, yc);
  return d->nbytes;
}

static int
ChikujiYomiDeletePrevious(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  RkStat stat;
  int i, j, l = 0;

  d->nbytes = 0;
  if (!(yc->cStartp < yc->kCurs)) { /* 読みがないなら */
    if (!yc->nbunsetsu) { /* 文節もない */
      return NothingChanged(d);
    }
    else {
      if (RkwSubstYomi(yc->context, 0, yc->ye - yc->cStartp, 0, 0) == NG) {
	(void)makeRkError(d, "読みに戻すことができません");
	(void)TanMuhenkan(d);
	return 0;
      }
      yc->ys = yc->ye = yc->cStartp;
      yc->curbun = yc->nbunsetsu - 1; /* ひとつ読みに戻す */
      for (i = yc->nbunsetsu - 1; i >= yc->curbun; i--) {
	if (RkwGoTo(yc->context, i) == NG ||
	    RkwGetStat(yc->context, &stat) == NG ||
	    RkwStoreYomi(yc->context, (wchar_t *)0, 0) == NG) {
	  return makeRkError(d, "読みに戻すことができません");
	}
	l += stat.ylen;
	yc->nbunsetsu--;
      }
      i = j = 0;
      do {
	++i;
	if (yc->kAttr[yc->cStartp - i] & SENTOU) {
	  for (j++ ;
	       j < yc->cRStartp && !(yc->rAttr[yc->cRStartp - j] & SENTOU) ;) {
	    j++;
	  }
	}
      } while (i < l);
      yc->kCurs = yc->kRStartp = yc->cStartp;
      yc->rCurs = yc->rStartp = yc->cRStartp;
      yc->cStartp = (i < yc->cStartp) ? yc->cStartp - i : 0;
      yc->cRStartp = (j < yc->cRStartp) ? yc->cRStartp - j : 0;
      yc->ys = yc->ye = yc->cStartp;
      clearHenkanContent(yc);
      if (yc->curbun) {
	yc->curbun--;
      }

      makeKanjiStatusReturn(d, yc);
      return 0;
    }
  }
  if (yc->kCurs - 1 < yc->ys) {
    yc->ys = yc->kCurs - 1;
  }
  if (yc->ys < 0) {
    yc->ys = 0;
  }
  KanaDeletePrevious(d);
  yc->status |= CHIKUJI_OVERWRAP;
  if (yc->kCurs <= yc->cStartp && yc->kEndp <= yc->cStartp && yc->nbunsetsu) {
    /* 文節はあるのに読みがないなら */
    if (RkwGoTo(yc->context, yc->nbunsetsu - 1) == -1) {
      return makeRkError(d, "文節の移動に失敗しました");
    }
    yc->kouhoCount = 0;
    yc->curbun = yc->nbunsetsu - 1;
    moveToChikujiTanMode(d);
    makeKanjiStatusReturn(d, yc);
  }
  else {
    moveToChikujiYomiMode(d);
    makeYomiReturnStruct(d);
    if (yc->kEndp <= yc->cStartp && !yc->nbunsetsu) {
      /* 何にもない */
      d->current_mode = yc->curMode = yc->myEmptyMode;
      d->kanji_status_return->info |= KanjiEmptyInfo;
    }
  }
  return 0;
}

static int
ChikujiHenkan(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;
  int n, tmp, idx;

  if (!yc->nbunsetsu && yc->rEndp == /* yc->cRStartp(== 0) + */ 1 &&
      (yc->kAttr[0] & SUPKEY) &&
      (idx = findSup(yc->romaji_buffer[0]))) {
    return selectKeysup(d, yc, idx - 1);
  }
  if (!doesSupportChikuji()) {
    jrKanjiError = "サーバが逐次自動変換をサポートしていません";     
    makeGLineMessageFromString(d, jrKanjiError);
    makeKanjiStatusReturn(d, yc);
    d->nbytes = 0;
    return 0;
  }
  if (yc->status & CHIKUJI_ON_BUNSETSU) {
    tmp = yc->curbun;
  }
  else {
    tmp = yc->nbunsetsu;
  }
  d->nbytes = 0;
  if (yc->kCurs < yc->ys) {
    yc->ys = yc->kCurs;
  }
  if (forceRomajiFlushYomi(d)) {
    return d->nbytes;
  }

  if (containUnconvertedKey(yc)) {
    if (yc->cmark < yc->cStartp) {
      yc->cmark = yc->cStartp;
    }
    YomiMark(d);
    yc->ys = yc->pmark;
    if (forceRomajiFlushYomi(d)) {
      return d->nbytes;
    }
  }

  if (yc->cStartp < yc->kEndp) { /* 読みがあれば */
    yc->kCurs = yc->kEndp;
    if (chikujiSubstYomi(d) < 0) {
      makeGLineMessageFromString(d, jrKanjiError);
      TanMuhenkan(d);
      return 0;
    }
    n = RkwFlushYomi(yc->context);
    if (n == -1) {
      (void)makeRkError(d, "変換に失敗しました");
      (void)TanMuhenkan(d);
      return 0;
    }
    yc->cStartp = yc->kEndp;
    yc->cRStartp = yc->rEndp;
    yc->kouhoCount = 1;
    yc->status |= CHIKUJI_ON_BUNSETSU;
    if (n > yc->nbunsetsu) {
      yc->curbun = yc->nbunsetsu;
      yc->nbunsetsu = n;
    }
  }
  if (RkwGoTo(yc->context, tmp) == -1) {
    makeRkError(d, "逐次変換に失敗しました");
    return 0;
  }
  yc->curbun = tmp;
  yc->ys = yc->ye = yc->cStartp;
  d->current_mode = yc->curMode = &tankouho_mode;
  yc->minorMode = CANNA_MODE_TankouhoMode;
  if (kouho_threshold > 0 && yc->kouhoCount >= kouho_threshold) {
    return tanKouhoIchiran(d, 0);
  }
  currentModeInfo(d);
  makeKanjiStatusReturn(d, yc);
  return d->nbytes;
}

void
moveToChikujiTanMode(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  yc->status |= CHIKUJI_ON_BUNSETSU;
  yc->minorMode = CANNA_MODE_ChikujiTanMode;
  d->current_mode = yc->curMode = &cb_mode;
  currentModeInfo(d);
}

void
moveToChikujiYomiMode(d)
     uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  yc->status &= ~CHIKUJI_ON_BUNSETSU;
  d->current_mode = yc->curMode = &cy_mode;
  EmptyBaseModeInfo(d, yc);
}

static int
generalNaive(d, fn)
uiContext d;
int (*fn)();
{
  if ((((yomiContext)d->modec)->generalFlags) &
      (CANNA_YOMI_HANKAKU | CANNA_YOMI_ROMAJI | CANNA_YOMI_BASE_HANKAKU)) {
    return (*fn)(d);
  }
  else {
    return ChikujiHenkan(d);
  }
}

extern int YomiInsert();

static int
ChikujiHenkanNaive(d)
uiContext d;
{
  return generalNaive(d, YomiInsert);
}

static int
ChikujiHenkanOrNothing(d)
     uiContext d;
{
  return generalNaive(d, NothingChanged);
}

static int
ChikujiMuhenkan(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->nbunsetsu) {
    return TanMuhenkan(d);
  }
  else if (yc->left || yc->right) {
    removeCurrentBunsetsu(d, (tanContext)yc);
    yc = (yomiContext)d->modec;
  }
  else {
    RomajiClearYomi(d);
    d->current_mode = yc->curMode = yc->myEmptyMode;
    d->kanji_status_return->info |= KanjiEmptyInfo;
  }
  makeKanjiStatusReturn(d, yc);
  return 0;
}

#include "chikujimap.c"
