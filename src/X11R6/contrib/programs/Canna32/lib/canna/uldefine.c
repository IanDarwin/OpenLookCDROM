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
static char rcs_id[] = "@(#) 102.1 $Id: uldefine.c,v 7.20 1994/03/04 07:46:04 kon Exp $";
#endif

#include	<errno.h>
#include 	"canna.h"

extern int errno;

extern HexkeySelect;

extern int uiUtilIchiranTooSmall();
extern int dicTourokuHinshiDelivery();

static int dicTourokuDo(), checkUsrDic(), dicTourokuYomi(), dicTourokuYomiDo();

static char *shinshitbl[] = {"人名",
			     "地名",
			     "団体・会社名",
			     "名詞",
			     "サ変名詞",
			     "単漢字",
			     "動詞",
			     "形容詞",
			     "形容動詞",
			     "副詞",
                             "数詞",
			     "接続詞・感動詞",
                             "連体詞",
			     "その他の固有名詞",
			     };

#define HINSHI_SZ (sizeof(shinshitbl) / sizeof(char *))

static wchar_t *hinshitbl[HINSHI_SZ];

static wchar_t *b1, *b2;

int
initHinshiTable()
{
  setWStrings(hinshitbl, shinshitbl, HINSHI_SZ);
  b1 = WString("単語?[");
  b2 = WString("]");
}

static
clearTango(d)
uiContext d;
{
  tourokuContext tc = (tourokuContext)d->modec;

  tc->tango_buffer[0] = 0;
  tc->tango_len = 0;
}

clearYomi(d)
uiContext d;
{
  tourokuContext tc = (tourokuContext)d->modec;

  tc->yomi_buffer[0] = 0;
  tc->yomi_len = 0;
}

static
clearTourokuContext(p)
tourokuContext p;
{
  p->id = TOUROKU_CONTEXT;
  p->genbuf[0] = 0;
  p->qbuf[0] = 0;
  p->tango_buffer[0] = 0;
  p->tango_len = 0;
  p->yomi_buffer[0] = 0;
  p->yomi_len = 0;
  p->curHinshi = 0;
  p->newDic = (struct dicname *)0;
  p->hcode[0] = 0;
  p->katsuyou = 0;
  p->udic = 0;
  p->delContext = 0;

  return(0);
}
  
static tourokuContext
newTourokuContext()
{
  tourokuContext tcxt;

  if((tcxt = 
      (tourokuContext)malloc(sizeof(tourokuContextRec))) == NULL) {
    jrKanjiError = "malloc (newTourokuContext) できませんでした";
    return(0);
  }
  clearTourokuContext(tcxt);

  return tcxt;
}

getTourokuContext(d)
uiContext d;
{
  tourokuContext tc;
  int retval = 0;

  if(pushCallback(d, d->modec, 0, 0, 0, 0) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }

  if((tc = newTourokuContext()) == NULL) {
    popCallback(d);
    return(NG);
  }
  tc->majorMode = d->majorMode;
  tc->next = d->modec;
  d->modec = (mode_context)tc;

  tc->prevMode = d->current_mode;

  return(retval);
}

void
popTourokuMode(d)
uiContext d;
{
  tourokuContext tc = (tourokuContext)d->modec;

  d->modec = tc->next;
  d->current_mode = tc->prevMode;
  free(tc);
}

void
freeAndPopTouroku(d)
uiContext d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  if(tc->udic)
    free(tc->udic);
  popTourokuMode(d);
  popCallback(d);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の単語の入力                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTTangoEveryTimeCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  tourokuContext tc = (tourokuContext)env;
  int len, echoLen, revPos;
  wchar_t tmpbuf[ROMEBUFSIZE];

  retval = d->nbytes = 0;

#ifdef DEBUG
  checkModec(d);
#endif
  if((echoLen = d->kanji_status_return->length) < 0 || d->more.todo)
    return(retval);

  if (echoLen == 0) {
    d->kanji_status_return->revPos = 0;
    d->kanji_status_return->revLen = 0;
  }

  if(d->kanji_status_return->info & KanjiGLineInfo &&
     d->kanji_status_return->gline.length > 0) {
    echostrClear(d);
    return 0;
  }

  WStrncpy(tmpbuf, d->kanji_status_return->echoStr, echoLen);
  tmpbuf[echoLen] = (wchar_t)'\0';

  WStrcpy(d->genbuf, b1);
  WStrcat(d->genbuf, tmpbuf);
  WStrcat(d->genbuf, b2);

  revPos = WStrlen(b1);

  len = revPos + echoLen + 1;
  WStrcpy(d->genbuf + len, tc->genbuf); /* メッセージ */

  len += WStrlen(tc->genbuf);
  tc->genbuf[0] = 0;
  d->kanji_status_return->gline.line = d->genbuf;
  d->kanji_status_return->gline.length = len;
  if (d->kanji_status_return->revLen) {
    d->kanji_status_return->gline.revPos =
      d->kanji_status_return->revPos + revPos;
    d->kanji_status_return->gline.revLen = d->kanji_status_return->revLen;
  }
  else { /* 反転領域がない場合 */
    d->kanji_status_return->gline.revPos = len - WStrlen(b2);
    d->kanji_status_return->gline.revLen = 1;
  }
  d->kanji_status_return->info |= KanjiGLineInfo;
  d->kanji_status_return->length = 0;

  echostrClear(d);
  checkGLineLen(d);

  return retval;
}

static
uuTTangoExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;

  popCallback(d); /* 読みを pop */

  tc = (tourokuContext)d->modec;

  WStrncpy(tc->tango_buffer, d->buffer_return, retval);
  tc->tango_buffer[retval] = (wchar_t)'\0';
  tc->tango_len = retval;

  return(dicTourokuYomi(d));
}

uuTTangoQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 読みを pop */

  freeAndPopTouroku(d);
  GlineClear(d);
  currentModeInfo(d);
  return prevMenuIfExist(d);
}

static
uuT2TangoEveryTimeCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  yomiContext nyc;
  int echoLen, pos, offset;
  wchar_t tmpbuf[ROMEBUFSIZE];

  nyc = (yomiContext)env;

#ifdef DEBUG
  checkModec(d);
#endif
  if(d->kanji_status_return->info & KanjiThroughInfo) {
    extern KanjiModeRec yomi_mode;
    _do_func_slightly(d, 0, (mode_context)nyc, &yomi_mode);
  } else if(retval > 0){
    /* 挿入する */
    generalReplace(nyc->kana_buffer, nyc->kAttr, &nyc->kRStartp,
		   &nyc->kCurs, &nyc->kEndp, 0, d->buffer_return,
		   retval, HENKANSUMI);
    generalReplace(nyc->romaji_buffer, nyc->rAttr, &nyc->rStartp,
		   &nyc->rCurs, &nyc->rEndp, 0, d->buffer_return,
		   retval, 0);
    nyc->rStartp = nyc->rCurs;
    nyc->kRStartp = nyc->kCurs;
  }

  d->kanji_status_return->info &= ~(KanjiThroughInfo | KanjiEmptyInfo);
  if((echoLen = d->kanji_status_return->length) < 0)
    return(retval);

  WStrncpy(tmpbuf, d->kanji_status_return->echoStr, echoLen);

  WStrncpy(d->genbuf, nyc->kana_buffer, pos = offset = nyc->kCurs);

  WStrncpy(d->genbuf + pos, tmpbuf, echoLen);
  pos += echoLen;
  WStrncpy(d->genbuf + pos, nyc->kana_buffer + offset, nyc->kEndp - offset);
  pos += nyc->kEndp - offset;
  if (d->kanji_status_return->revLen == 0 && /* 反転表示部分なしで... */
      nyc->kEndp - offset) { /* 後ろにくっつける部分があるのなら */
    d->kanji_status_return->revLen = 1;
    d->kanji_status_return->revPos = offset + echoLen;
  }
  else {
    d->kanji_status_return->revPos += offset;
  }
  d->kanji_status_return->echoStr = d->genbuf;
  d->kanji_status_return->length = pos;

  return retval;
}

/************************************************
 *  単語登録モードを抜ける際に必要な処理を行う  *
 ************************************************/
static
uuT2TangoExitCatch(d, retval, nyc)
uiContext d;
int retval;
yomiContext nyc;
/* ARGSUSED */
{
  yomiContext yc;

  popCallback(d); /* 読みを pop */

  yc = (yomiContext)d->modec;
  d->nbytes = retval = yc->kEndp;
  WStrncpy(d->buffer_return, yc->kana_buffer, retval);
  d->buffer_return[retval] = (wchar_t)'\0';

  RomajiClearYomi(d);
  popYomiMode(d);
  d->status = EXIT_CALLBACK;

  return retval;
}

static
uuT2TangoQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 読みを pop */

  popYomiMode(d);

  d->status = QUIT_CALLBACK;

  return(0);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の辞書作成                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTMakeDicYesCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  int err = 0, perr = 0;
  tourokuContext tc;
  wchar_t **dp;
  extern defaultContext;

  popCallback(d); /* yesNo をポップ */

  tc = (tourokuContext)d->modec;

  if(defaultContext < 0) {
    if((KanjiInit() < 0) || (defaultContext < 0)) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      freeAndPopTouroku(d);
      defineEnd(d);
      return(GLineNGReturn(d));
    }
  }
  /* 辞書を作る */
  if (RkwCreateDic(defaultContext,
		   (unsigned char *)tc->newDic->name, 0x80) < 0) {
    err++; if (errno == EPIPE) perr++;
    MBstowcs(d->genbuf, "辞書の生成に失敗しました", 256);
  } else if(RkwMountDic(defaultContext, tc->newDic->name, 0) < 0) {
    err++; if (errno == EPIPE) perr++;
    MBstowcs(d->genbuf, "辞書のマウントに失敗しました", 256);
  } else if(d->contextCache != -1 && 
    RkwMountDic(d->contextCache, tc->newDic->name, 0) < 0) {
    err++; if (errno == EPIPE) perr++;
    MBstowcs(d->genbuf, "辞書のマウントに失敗しました", 256);
  }

  if(err) {
    if (perr) {
      jrKanjiPipeError();
    }
    makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
    freeAndPopTouroku(d);
    defineEnd(d);
    currentModeInfo(d);
    return(0);
  }

  tc->newDic->dicflag = DIC_MOUNTED;

  /* 辞書の候補の最後に追加する */
  dp = tc->udic;
  if (dp) {
    while (*dp) {
      dp++;
    }
    *dp++ = WString(tc->newDic->name);
    *dp = 0;
  }

  return(dicTourokuTango(d, uuTTangoQuitCatch));
}

static
uuTMakeDicQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* yesNo をポップ */

  freeAndPopTouroku(d);

  return prevMenuIfExist(d);
}

static
uuTMakeDicNoCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* yesNo をポップ */

  freeAndPopTouroku(d);
  defineEnd(d);
  currentModeInfo(d);

  GlineClear(d);
  defineEnd(d);
  return(retval);
}

/*
  ユーザ辞書でマウントされているものを取り出す処理
 */
wchar_t **
getUserDicName(d)
uiContext d;
/* ARGSUSED */
{
  int nmudic; /* マウントされているユーザ辞書の数 */
  struct dicname *p;
  wchar_t **tourokup, **tp;
  extern defaultContext;

  if(defaultContext < 0) {
    if((KanjiInit() < 0) || (defaultContext < 0)) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      return(0);
    }
  }

  for (nmudic = 0, p = kanjidicnames ; p ; p = p->next) {
    if (p->dictype == DIC_USER && p->dicflag == DIC_MOUNTED) {
      nmudic++;
    }
  }

  /* return BUFFER の alloc */
  if((tourokup = (wchar_t **)calloc(nmudic + 2, sizeof(wchar_t *))) == NULL) {
    /* + 2 なのは 1 個増える可能性があるのと打ち止めマークをいれるため */
    jrKanjiError = "malloc (getUserDicName) できませんでした";
    return(0);
  }

  for (tp = tourokup + nmudic, p = kanjidicnames ; p ; p = p->next) {
    if (p->dictype == DIC_USER && p->dicflag == DIC_MOUNTED) {
      *--tp = WString(p->name);
    }
  }
  tourokup[nmudic] = (wchar_t *)0;

  return((wchar_t **)tourokup);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の単語の入力                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* 始めに呼ばれる関数 */
dicTouroku(d)
uiContext d;
{
  tourokuContext tc;

  if(dicTourokuDo(d) < 0) {
    defineEnd(d);
    return(GLineNGReturn(d));
  }

  tc = (tourokuContext)d->modec;

  /* 辞書が無ければ辞書を作る */
  if(!*tc->udic) {
    if(checkUsrDic(d) < 0) {
      defineEnd(d);
      return(GLineNGReturn(d));
    }
    else {
      return(0);
    }
  }
  return(dicTourokuTango(d, uuTTangoQuitCatch));
}

static
dicTourokuDo(d)
uiContext d;
{
  tourokuContext tc;
  wchar_t **up;
  wchar_t **getUserDicName();

  d->status = 0;

  /* ユーザ辞書でマウントされているものを取ってくる */
  if((up = getUserDicName(d)) == 0) {
    return(NG);
  }

  if (getTourokuContext(d) < 0) {
    if(up)
      free(up);
    return(NG);
  }

  tc = (tourokuContext)d->modec;

  tc->udic = up;

  return(0);
}

static struct dicname *
findUsrDic()
{
  struct dicname *res = (struct dicname *)0, *p;

  for (p = kanjidicnames ; p ; p = p->next) {
    if (p->dictype == DIC_USER) {
      res = p;
    }
  }
  return res;
}

/* 
 * マウントされている辞書のチェック
 * ・.iroha で "userdic" に記述されていてマウントされている辞書がない
 * ・.iroha で "userdic" に記述されていてマウントされていない辞書がある
 *     → 辞書を作る(作る辞書は１つだけ)
 */
static
checkUsrDic(d)
uiContext d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  coreContext ync;
  wchar_t xxxx[512];
  struct dicname *u;

  u = findUsrDic();

  if (u) {
    if (u->dicflag == DIC_MOUNT_FAILED) {
      char tmpbuf[1024];
      sprintf(tmpbuf,
	      "単語登録用辞書がありません。辞書(%s)を作成しますか?(y/n)",
	      u->name);
      makeGLineMessageFromString(d, tmpbuf);
      tc->newDic = u; /* 作る辞書 */
      if(getYesNoContext(d,
			 0, uuTMakeDicYesCatch,
			 uuTMakeDicQuitCatch, uuTMakeDicNoCatch) < 0) {
	defineEnd(d);
	return(GLineNGReturn(d));
      }
      makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
      ync = (coreContext)d->modec;
      ync->majorMode = CANNA_MODE_ExtendMode;
      ync->minorMode = CANNA_MODE_TourokuMode;
    }
  }
  else {
    MBstowcs(xxxx, "単語登録用辞書が指定されていません", 512);
    WStrcpy(d->genbuf, xxxx);
    makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
    freeAndPopTouroku(d);
    defineEnd(d);
    currentModeInfo(d);
  }

  return(0);
}

dicTourokuTango(d, quitfunc)
uiContext d;
int (*quitfunc)();
{
  tourokuContext tc = (tourokuContext)d->modec;
  yomiContext yc, yc2;
  int retval = 0;

  yc = GetKanjiString(d, 0, 0,
		      CANNA_NOTHING_RESTRICTED,
		      CANNA_YOMI_CHGMODE_INHIBITTED,
		      CANNA_YOMI_END_IF_KAKUTEI,
		      CANNA_YOMI_INHIBIT_NONE,
		      uuTTangoEveryTimeCatch, uuTTangoExitCatch,
		      quitfunc);
  if (yc == (yomiContext)0) {
    if (tc->udic) free(tc->udic);
    defineEnd(d);
    return NoMoreMemory();
  }
  yc2 = GetKanjiString(d, 0, 0,
		      CANNA_NOTHING_RESTRICTED,
		      CANNA_YOMI_CHGMODE_INHIBITTED,
		      !CANNA_YOMI_END_IF_KAKUTEI,
		      CANNA_YOMI_INHIBIT_NONE,
		      uuT2TangoEveryTimeCatch, uuT2TangoExitCatch,
		      uuT2TangoQuitCatch);
  if (yc2 == (yomiContext)0) {
    if (tc->udic) free(tc->udic);
    defineEnd(d);
    return NoMoreMemory();
  }
  yc2->generalFlags |= CANNA_YOMI_DELETE_DONT_QUIT;

  yc2->majorMode = CANNA_MODE_ExtendMode;
  yc2->minorMode = CANNA_MODE_TourokuMode;
  currentModeInfo(d);

  return(retval);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の読みの入力                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTYomiEveryTimeCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  tourokuContext tc = (tourokuContext)env;
  int len, echoLen, revPos;
  wchar_t tmpbuf[ROMEBUFSIZE];

  retval = d->nbytes = 0;

  if((echoLen = d->kanji_status_return->length) < 0)
    return(retval);

  if (echoLen == 0) {
    d->kanji_status_return->revPos = 0;
    d->kanji_status_return->revLen = 0;
  }

  /* 取りあえず echoStr が d->genbuf かもしれないので copy しておく */
  WStrncpy(tmpbuf, d->kanji_status_return->echoStr, echoLen);

  d->kanji_status_return->info &= ~(KanjiThroughInfo | KanjiEmptyInfo);
  revPos = MBstowcs(d->genbuf, "単語[", ROMEBUFSIZE);
  WStrcpy(d->genbuf + revPos, tc->tango_buffer);
  revPos += WStrlen(tc->tango_buffer);
  revPos += MBstowcs(d->genbuf + revPos, "] 読み?[", ROMEBUFSIZE - revPos);
  WStrncpy(d->genbuf + revPos, tmpbuf, echoLen);
  len = echoLen + revPos;
  d->genbuf[len++] = (wchar_t) ']';
  WStrcpy(d->genbuf + len, tc->genbuf);
  len += WStrlen(tc->genbuf);
  tc->genbuf[0] = 0;
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
  d->kanji_status_return->info |= KanjiGLineInfo;
  echostrClear(d);
  checkGLineLen(d);

  return retval;
}

static
uuTYomiExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;

  popCallback(d); /* 読みを pop */

  tc = (tourokuContext)d->modec;

  WStrncpy(tc->yomi_buffer, d->buffer_return, retval);
  tc->yomi_buffer[retval] = (wchar_t)'\0';
  tc->yomi_len = retval;

  return(dicTourokuHinshi(d));
}

static
uuTYomiQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 読みを pop */

  clearTango(d);
  clearYomi(d);

  return(dicTourokuTango(d, uuTTangoQuitCatch));
}

static
dicTourokuYomi(d)
uiContext d;
{
  return(dicTourokuYomiDo(d, uuTYomiQuitCatch));
}

static
dicTourokuYomiDo(d, quitfunc)
uiContext d;
int (*quitfunc)();
{
  yomiContext yc;
  tourokuContext tc = (tourokuContext)d->modec;
  int retval = 0;

  if(tc->tango_len < 1) {
    MBstowcs(d->genbuf, "単語を入力してください", 256);
    makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
    clearTango(d);

    return(dicTourokuTango(d, uuTTangoQuitCatch));
  }

  yc = GetKanjiString(d, 0, 0,
		      CANNA_NOTHING_RESTRICTED,
		      CANNA_YOMI_CHGMODE_INHIBITTED,
		      CANNA_YOMI_END_IF_KAKUTEI,
		      (CANNA_YOMI_INHIBIT_HENKAN | CANNA_YOMI_INHIBIT_ASHEX |
		      CANNA_YOMI_INHIBIT_ASBUSHU),
		      uuTYomiEveryTimeCatch, uuTYomiExitCatch,
		      quitfunc);
  if (yc == (yomiContext)0) {
    if (tc->udic) free(tc->udic);
    defineEnd(d);
    return NoMoreMemory();
  }
  yc->majorMode = CANNA_MODE_ExtendMode;
  yc->minorMode = CANNA_MODE_TourokuMode;
  currentModeInfo(d);

  return(retval);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の品詞の選択                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTHinshiExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;
  tourokuContext tc;
  int cur;

  d->nbytes = 0;

  popCallback(d); /* 一覧を pop */

  fc = (forichiranContext)d->modec;
  cur = fc->curIkouho;

  popForIchiranMode(d);
  popCallback(d);

  tc = (tourokuContext)d->modec;

  tc->curHinshi = cur;

  return(dicTourokuHinshiDelivery(d));
}

static
uuTHinshiQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 一覧を pop */

  popForIchiranMode(d);
  popCallback(d);

  clearYomi(d);

  return(dicTourokuYomi(d));
}

dicTourokuHinshi(d)
uiContext d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  forichiranContext fc;
  ichiranContext ic;
  unsigned char inhibit = 0;
  int currentkouho, retval = 0;

  d->status = 0;

  if(tc->yomi_len < 1) {
    MBstowcs(d->genbuf, "読みを入力してください", 256);
    makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
    return dicTourokuYomi(d);
  }

  if((retval = getForIchiranContext(d)) < 0) {
    if (tc->udic) free(tc->udic);
    defineEnd(d);
    return(GLineNGReturnTK(d));
  }

  fc = (forichiranContext)d->modec;

  /* selectOne を呼ぶための準備 */
  fc->allkouho = hinshitbl;

  fc->curIkouho = 0;
  currentkouho = 0;
  if( !HexkeySelect )
    inhibit |= ((unsigned char)NUMBERING | (unsigned char)CHARINSERT); 
  else
    inhibit |= (unsigned char)CHARINSERT; 

  if((retval = selectOne(d, fc->allkouho, &fc->curIkouho, HINSHI_SZ,
		 BANGOMAX, inhibit, currentkouho, WITH_LIST_CALLBACK,
		 0, uuTHinshiExitCatch, 
		 uuTHinshiQuitCatch, uiUtilIchiranTooSmall)) < 0) {
    popForIchiranMode(d);
    popCallback(d);
    if (tc->udic) free(tc->udic);
    defineEnd(d);
    return(GLineNGReturnTK(d));
  }

  ic = (ichiranContext)d->modec;
  ic->majorMode = CANNA_MODE_ExtendMode;
  ic->minorMode = CANNA_MODE_TourokuHinshiMode;
  currentModeInfo(d);

  /* 候補一覧行が狭くて候補一覧が出せない */
  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return(retval);
  }

  if ( !(ic->flags & ICHIRAN_ALLOW_CALLBACK) ) {
    makeGlineStatus(d);
  }

  /* d->status = ICHIRAN_EVERYTIME; */

  return(retval);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * jrKanjiControl 用                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

dicTourokuControl(d, tango, quitfunc)
uiContext d;
wchar_t *tango;
int (*quitfunc)();
{
  tourokuContext tc;

  if(dicTourokuDo(d) < 0) {
    return(GLineNGReturn(d));
  }

  tc = (tourokuContext)d->modec;

  if(!*tc->udic) {
    if(checkUsrDic(d) < 0) 
      return(GLineNGReturn(d));
    else
      return(0);
  }

  if(tango == 0 || tango[0] == 0) {
    return(dicTourokuTango(d, quitfunc));
  }

  WStrcpy(tc->tango_buffer, tango);
  tc->tango_len = WStrlen(tc->tango_buffer);

  return(dicTourokuYomiDo(d, quitfunc));
}
