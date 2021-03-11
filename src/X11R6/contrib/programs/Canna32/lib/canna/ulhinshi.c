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
static char rcs_id[] = "@(#) 102.1 $Id: ulhinshi.c,v 5.19 1994/03/10 02:30:25 kon Exp $";
#endif

#include <errno.h>
#include "canna.h"

extern int  errno;

extern int grammaticalQuestion;

extern HexkeySelect;

extern int uiUtilIchiranTooSmall();
extern int uuTTangoQuitCatch();
extern void freeAndPopTouroku();

static int tourokuYes(), tourokuNo(), makeDoushi(), uuTDicExitCatch();
static int uuTDicQuitCatch(), tangoTouroku();

static char *e_message[] = {
  /*0*/"さらに細かい品詞分けのための質問をしても良いですか?(y/n)",
  /*1*/"読みと候補を 終止形で入力してください。",
  /*2*/"読みと候補の 活用が違います。入力しなおしてください。",
  /*3*/"読みと候補を 終止形で入力してください。例) 早い",
  /*4*/"読みと候補を 終止形で入力してください。例) 静かだ",
  /*5*/"「",
  /*6*/"な」は正しいですか。(y/n)",
  /*7*/"ない」は正しいですか。(y/n)",
  /*8*/"」は正しいですか。(y/n)",
  /*9*/"する」は正しいですか。(y/n)",
  /*10*/"と」は正しいですか。(y/n)",
  /*11*/"がいい」は正しいですか。(y/n)",
  /*12*/"がある」は正しいですか。(y/n)",
  /*13*/"かな漢字変換サーバと通信できません",
  /*14*/"単語登録できませんでした",
  /*15*/"『",
  /*16*/"』",
  /*17*/"（",
  /*18*/"）を登録しました",
};

#define message_num (sizeof(e_message) / sizeof(char *))
static wchar_t *message[message_num];

static char sgyouA[] = "かがさたなばまらわ";
static char sgyouI[] = "きぎしちにびみりい";
static char sgyouU[] = "くぐすつぬぶむるう";

static wchar_t *gyouA;
static wchar_t *gyouI;
static wchar_t *gyouU;

/* 全てのメッセージを"unsigned char"から"wchar_t"に変換する */
int
initHinshiMessage()
{
  int i;

  for(i = 0; i < message_num; i++) {
    message[i] = WString(e_message[i]);
    if(!message[i]) {
      return(-1);
    }
  }
  return 0;
}

/* WSprintf(to_buf, x1, x2, from_buf)
   :WSprintf(to_buf,"x1%sx2",from_buf);
 */
static void
WSprintf(to_buf, x1, x2, from_buf)
wchar_t *to_buf, *x1, *x2, *from_buf;
{
    WStrcpy(to_buf, x1);
    WStrcat(to_buf, from_buf);
    WStrcat(to_buf, x2);
}

static void
EWStrcat(buf, xxxx)
wchar_t *buf;
char *xxxx;
{
  wchar_t x[1024];

  MBstowcs(x, xxxx, 1024);
  WStrcat(buf, x);
}

static void
EWStrcpy(buf, xxxx)
wchar_t *buf;
char *xxxx;
{
  wchar_t x[1024];
  int len;

  len = MBstowcs(x, xxxx, 1024);
  WStrncpy(buf, x, len);
  buf[len] = 0;
}

static int
EWStrcmp(buf, xxxx)
wchar_t *buf;
char *xxxx;
{
  wchar_t x[1024];

  MBstowcs(x, xxxx, 1024);
  return(WStrncmp(buf, x, WStrlen(x)));
}

static int
EWStrncmp(buf, xxxx, len)
wchar_t *buf;
char *xxxx;
int len;
/* ARGSUSED */
{
  wchar_t x[1024];

  MBstowcs(x, xxxx, 1024);
  return(WStrncmp(buf, x, WStrlen(x)));
}

int
initGyouTable()
{
  gyouA = WString(sgyouA);
  gyouI = WString(sgyouI);
  gyouU = WString(sgyouU);
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の品詞選択 〜Yes/No 共通 Quit〜                                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTHinshiYNQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d);
  
  return(dicTourokuHinshi(d));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の品詞選択 〜Yes/No 第２段階 共通コールバック〜                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTHinshi2YesCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;

  popCallback(d); /* yesNo をポップ */

  tourokuYes(d);   /* 品詞が決まれば tc->hcode にセットする */

  tc = (tourokuContext)d->modec;

  if (!tc->qbuf[0]) {
    if (tc->hcode[0]) {
      /* 品詞が決まったので、登録するユーザ辞書の指定を行う */
      return(dicTourokuDictionary(d, uuTDicExitCatch, uuTDicQuitCatch));
    }
  }
  return(retval);
}

static
uuTHinshi2NoCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;

  popCallback(d); /* yesNo をポップ */

  tourokuNo(d);   /* 品詞が決まれば tc->hcode にセットする */

  tc = (tourokuContext)d->modec;

  if (!tc->qbuf[0]) {
    if (tc->hcode[0]) {
      /* 品詞が決まったので、登録するユーザ辞書の指定を行う */
      return(dicTourokuDictionary(d, uuTDicExitCatch, uuTDicQuitCatch));
    }
  }

  return(retval);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の品詞選択 〜Yes/No 第１段階 コールバック〜                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTHinshi1YesCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;
  coreContext ync;
  
  popCallback(d); /* yesNo をポップ */

  tourokuYes(d);   /* 品詞が決まれば tc->hcode にセットする */

  tc = (tourokuContext)d->modec;

  if(tc->qbuf[0]) {
    /* 質問する */
    makeGLineMessage(d, tc->qbuf, WStrlen(tc->qbuf));
    if((retval = getYesNoContext(d,
		 0, uuTHinshi2YesCatch,
		 uuTHinshiYNQuitCatch, uuTHinshi2NoCatch)) == NG) {
      defineEnd(d);
      return(GLineNGReturnTK(d));
    }
    ync = (coreContext)d->modec;
    ync->majorMode = CANNA_MODE_ExtendMode;
    ync->minorMode = CANNA_MODE_TourokuHinshiMode;
  } else if(tc->hcode[0]) {
    /* 品詞が決まったので、登録するユーザ辞書の指定を行う */
    return(dicTourokuDictionary(d, uuTDicExitCatch, uuTDicQuitCatch));
  }

  return(retval);
}

static
uuTHinshi1NoCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;
  coreContext ync;

  popCallback(d); /* yesNo をポップ */

  tourokuNo(d);   /* 品詞が決まれば tc->hcode にセットする */

  tc = (tourokuContext)d->modec;

  if(tc->qbuf[0]) {
    /* 質問する */
    makeGLineMessage(d, tc->qbuf, WStrlen(tc->qbuf));
    if((retval = getYesNoContext(d,
		 0, uuTHinshi2YesCatch,
		 uuTHinshiYNQuitCatch, uuTHinshi2NoCatch)) == NG) {
      defineEnd(d); 
      return(GLineNGReturnTK(d));
    }
    ync = (coreContext)d->modec;
    ync->majorMode = CANNA_MODE_ExtendMode;
    ync->minorMode = CANNA_MODE_TourokuHinshiMode;
  } else if(tc->hcode[0]) {
    /* 品詞が決まったので、登録するユーザ辞書の指定を行う */
    return(dicTourokuDictionary(d, uuTDicExitCatch, uuTDicQuitCatch));
  }

  return(retval);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の品詞分けする？                                                  *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTHinshiQYesCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  tourokuContext tc;
  coreContext ync;

  popCallback(d); /* yesNo をポップ */

  tc = (tourokuContext)d->modec;

  makeGLineMessage(d, tc->qbuf, WStrlen(tc->qbuf)); /* 質問 */
  if((retval = getYesNoContext(d,
	 0, uuTHinshi1YesCatch,
	 uuTHinshiYNQuitCatch, uuTHinshi1NoCatch)) == NG) {
    defineEnd(d);
    return(GLineNGReturnTK(d));
  }
  ync = (coreContext)d->modec;
  ync->majorMode = CANNA_MODE_ExtendMode;
  ync->minorMode = CANNA_MODE_TourokuHinshiMode;

  return(retval);
}

static
uuTHinshiQNoCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* yesNo をポップ */

  return(dicTourokuDictionary(d, uuTDicExitCatch, uuTDicQuitCatch));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 単語登録の品詞選択                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static int makeHinshi();

dicTourokuHinshiDelivery(d)
uiContext	d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  coreContext ync;
  int retval = 0;

  makeHinshi(d); /* 品詞、エラーメッセージ、質問をセットしてくる */

#ifdef DEBUG
  if(iroha_debug) {
    printf("tc->genbuf=%s, tc->qbuf=%s, tc->hcode=%s\n", tc->genbuf, tc->qbuf,
	   tc->hcode);
  }
#endif
  if(tc->genbuf[0]) {
    /* 入力されたデータに誤りがあったので、
       メッセージを表示して読み入力に戻る */
    clearYomi(d);
    return(dicTourokuTango(d, uuTTangoQuitCatch));
  } else if(tc->qbuf[0] && grammaticalQuestion) {
    /* 細かい品詞分けのための質問をする */
    WStrcpy(d->genbuf, message[0]);
    if((retval = getYesNoContext(d,
		 0, uuTHinshiQYesCatch,
		 uuTHinshiYNQuitCatch, uuTHinshiQNoCatch)) == NG) {
      defineEnd(d);
      return(GLineNGReturnTK(d));
    }
    makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));
    ync = (coreContext)d->modec;
    ync->majorMode = CANNA_MODE_ExtendMode;
    ync->minorMode = CANNA_MODE_TourokuHinshiMode;
    return(retval);
  } else if(tc->hcode[0]) {
    /* 品詞が決まったので、登録するユーザ辞書の指定を行う */
    return(dicTourokuDictionary(d, uuTDicExitCatch, uuTDicQuitCatch));
  }
  return 0;
}

/*
 * 選択された品詞から次の動作を行う
 * 
 * tc->hcode	品詞
 * tc->qbuf	質問
 * tc->genbuf	エラー
 */
static int
makeHinshi(d)
uiContext	d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  int tlen, ylen, yomi_katsuyou;
  wchar_t tmpbuf[256];

  tc->hcode[0] = 0;
  tc->qbuf[0] = 0;
  tc->genbuf[0] = 0;

  tlen = tc->tango_len;
  ylen = tc->yomi_len;

  switch(tc->curHinshi) {
  case 0:  /* 人名 */
    EWStrcpy(tc->hcode, "#JN");
    break;

  case 1:  /* 地名 */
    EWStrcpy(tc->hcode, "#CN");
    break;
    
  case 2:  /* 団体・会社名 */
    EWStrcpy(tc->hcode, "#KK");
    break;

  case 3:  /* 名詞 １ */

  case 4:  /* サ変名詞 １ */
    if(tc->curHinshi == 3)
      EWStrcpy(tc->hcode, "#T35"); /* 詳細の品詞を必要としない場合 */
    else
      EWStrcpy(tc->hcode, "#T30"); /* 詳細の品詞を必要としない場合 */
    tc->katsuyou = 0;
    WSprintf(tc->qbuf, message[5], message[6], tc->tango_buffer);
    break;

  case 5:  /* 単漢字 */
    EWStrcpy(tc->hcode, "#KJ");
    break;

  case 6:  /* 動詞 １ */

    /* 入力が終止形か？ */
    tc->katsuyou = 0;
    while (tc->katsuyou<GOBISUU &&
	   tc->tango_buffer[tlen - 1] != gyouU[tc->katsuyou]) {
      tc->katsuyou++;
    }
    yomi_katsuyou = 0;
    while (yomi_katsuyou<GOBISUU &&
	   tc->yomi_buffer[ylen - 1] != gyouU[yomi_katsuyou]) {
      yomi_katsuyou++;
    }
    if((tc->katsuyou == GOBISUU) || (yomi_katsuyou == GOBISUU)){
      WStrcpy(tc->genbuf, message[1]);
      return(0);
    }
    if(tc->katsuyou != yomi_katsuyou){
      WStrcpy(tc->genbuf, message[2]);
      return(0);
    }

    /* 例外処理 */
    if(!(EWStrcmp(tc->tango_buffer, "くる"))) {
      /* カ行変格活用 */
      EWStrcpy(tc->hcode, "#KX");
      return(0);
    }
    if(tlen >= 3 &&
       !(EWStrcmp(tc->tango_buffer+tlen-3, "んずる"))) {
      /* ンザ行変格活用 */
      EWStrcpy(tc->hcode, "#NZX");
      return(0);
    }
    if(tlen >= 2 &&
       !(EWStrcmp(tc->tango_buffer+tlen-2, "ずる"))) {
      /* ザ行変格活用 */
      EWStrcpy(tc->hcode, "#ZX");
      return(0);
    }
    if(!(EWStrcmp(tc->tango_buffer, "する"))) {
      /* サ行変格活用 */
      EWStrcpy(tc->hcode, "#SX");
      return(0);
    }

    makeDoushi(d); /* 詳細の品詞を必要としない場合 */
    /* 未然形をつくる */
    WStrncpy(tmpbuf, tc->tango_buffer, tlen-1);  
    tmpbuf[tlen - 1] = gyouA[tc->katsuyou];
    tmpbuf[tlen] = (wchar_t)0;
    WSprintf(tc->qbuf, message[5], message[7], tmpbuf);
    break;

  case 7:  /* 形容詞 １ */
    tc->katsuyou = 1;
    if(tlen >= 1 && ylen >= 1 &&
       ((EWStrncmp(tc->tango_buffer+tlen-1, "い", 1) != 0) ||
	(EWStrncmp(tc->yomi_buffer+ylen-1, "い", 1) != 0))) {
      WStrcpy(tc->genbuf, message[3]);
      return(0);
    }

    EWStrcpy(tc->hcode, "#KY"); /* 詳細の品詞を必要としない場合 */
    WStrncpy(tmpbuf, tc->tango_buffer, tlen-1);  
    tmpbuf[tlen-1] = 0;
    WSprintf(tc->qbuf, message[5], message[8], tmpbuf);
    break;

  case 8:  /* 形容動詞 １ */
    tc->katsuyou = 1;
    if(tlen >= 1 && ylen >= 1 &&
       ((EWStrncmp(tc->tango_buffer+tlen-1, "だ", 1)) ||
	(EWStrncmp(tc->yomi_buffer+ylen-1, "だ", 1)))) {
      WStrcpy(tc->genbuf, message[4]);
      return(0);
    }
    EWStrcpy(tc->hcode, "#T05"); /* 詳細の品詞を必要としない場合 */
    WStrncpy(tmpbuf, tc->tango_buffer, tlen-1);  
    tmpbuf[tlen-1] = 0;  
    WSprintf(tc->qbuf, message[5], message[9], tmpbuf);
    break;

  case 9:  /* 副詞 １*/
    EWStrcpy(tc->hcode, "#F14"); /* 詳細の品詞を必要としない場合 */
    tc->katsuyou = 0;
    WSprintf(tc->qbuf, message[5], message[9], tc->tango_buffer);
    break;

  case 10: /* 数詞 */
    EWStrcpy(tc->hcode, "#NN");
    break;

  case 11: /* 接続詞・感動詞 */
    EWStrcpy(tc->hcode, "#CJ");
    break;

  case 12: /* 連体詞 */
    EWStrcpy(tc->hcode, "#RT");
    break;

  case 13: /* その他の固有名詞 */
    EWStrcpy(tc->hcode, "#KK");
    break;

  case 14:  /* 副詞 ２Ｙ */

  case 15:  /* 副詞 ２Ｎ */
    WSprintf(tc->qbuf, message[5], message[10], tc->tango_buffer);
    break;

  case 16:  /* 動詞 ２Ｙ */
    WStrncpy(tmpbuf, tc->tango_buffer, tlen-1);
    tmpbuf[tlen - 1] = gyouI[tc->katsuyou];
    tmpbuf[tlen] = (wchar_t)'\0';
    WSprintf(tc->qbuf, message[5], message[11], tmpbuf);
    break;

  case 17:  /* 動詞 ２Ｎ */
    /* 上下一段活用を作る */
    WStrncpy(tmpbuf, tc->tango_buffer, tlen-1);
    tmpbuf[tlen-1] = (wchar_t)'\0';
    WSprintf(tc->qbuf, message[5], message[11], tmpbuf);
    break;

  case 18:  /* 形容動詞 ２Ｙ */

  case 19:  /* 形容動詞 ２Ｎ */
    WStrncpy(tmpbuf, tc->tango_buffer, tlen-1);  
    tmpbuf[tlen-1] = 0;
    WSprintf(tc->qbuf, message[5], message[12], tmpbuf);
    break;
  }

  return(0);
}

static
tourokuYes(d)
uiContext	d;
{
  tourokuContext tc = (tourokuContext)d->modec;

  tc->hcode[0] = 0;
  tc->qbuf[0] = 0;
  tc->genbuf[0] = 0;

  switch(tc->curHinshi) {
  case 3:  /* 名詞 */
    EWStrcpy(tc->hcode, "#T15");   /* 色々、強力 */
    break;

  case 4:  /* サ変名詞 */
    EWStrcpy(tc->hcode, "#T10");          /* 安心、浮気 */
    break;

  case 6:  /* 動詞 */
    tc->curHinshi = 16;
    makeHinshi(d);
    break;

  case 7:  /* 形容詞 */
    EWStrcpy(tc->hcode, "#KYT");          /* きいろい */
    break;

  case 8:  /* 形容動詞 */
    tc->curHinshi = 18;
    makeHinshi(d);
    break;

  case 9:  /* 副詞 １*/
    tc->curHinshi = 14;
    makeHinshi(d);
    break;

  case 14:  /* 副詞 ２Ｙ */
    EWStrcpy(tc->hcode, "#F04");          /* ふっくら */
    break;

  case 15:  /* 副詞 ２Ｎ */
    EWStrcpy(tc->hcode, "#F06");          /* 突然 */
    break;

  case 16:  /* 動詞 ２Ｙ */
    makeDoushi(d);
    EWStrcat( tc->hcode, "r" );
    break;

  case 17:  /* 動詞 ２Ｎ */
    EWStrcpy(tc->hcode, "#KSr");          /* 生きる */
    break;

  case 18:  /* 形容動詞 ２Ｙ */
    EWStrcpy(tc->hcode, "#T10");          /* 関心だ */
    break;

  case 19:  /* 形容動詞 ２Ｎ */
    EWStrcpy(tc->hcode, "#T15");          /* 意外だ、可能だ */
    break;
  }

  return(0);
}

static
tourokuNo(d)
uiContext	d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  wchar_t test[1024];

  tc->hcode[0] = 0;
  tc->qbuf[0] = 0;
  tc->genbuf[0] = 0;

  switch( tc->curHinshi ) {
  case 3:  /* 名詞 */
    EWStrcpy(tc->hcode, "#T35");   /* 山、本 */
    EWStrcpy(test, "#T35");   /* 山、本 */
    break;

  case 4:  /* サ変名詞 */
    EWStrcpy(tc->hcode, "#T30");          /* 努力、検査 */
    break;

  case 6:  /* 動詞 */
    tc->curHinshi = 17;
    makeHinshi(d);
    break;

  case 7:  /* 形容詞 */
    EWStrcpy(tc->hcode, "#KY");           /* 美しい、早い */
    break;

  case 8:  /* 形容動詞 */
    tc->curHinshi = 19;
    makeHinshi(d);
    break;

  case 9:  /* 副詞 １*/
    tc->curHinshi = 15;
    makeHinshi(d);
    break;

  case 14:  /* 副詞 ２Ｙ */
    EWStrcpy(tc->hcode, "#F12");          /* そっと */
    break;

  case 15:  /* 副詞 ２Ｎ */
    EWStrcpy(tc->hcode, "#F14");          /* 飽くまで */
    break;

  case 16:  /* 動詞 ２Ｙ */
    makeDoushi(d);
    break;

  case 17:  /* 動詞 ２Ｎ */
    EWStrcpy(tc->hcode, "#KS");           /* 降りる */
    break;

  case 18:  /* 形容動詞 ２Ｙ */
    EWStrcpy(tc->hcode, "#T13");          /* 多慌てだ */
    break;

  case 19:  /* 形容動詞 ２Ｎ */
    EWStrcpy(tc->hcode, "#T18");          /* 便利だ、静かだ */
    break;
  }
  return(0);
}

static
makeDoushi(d)
uiContext	d;
{
  tourokuContext tc = (tourokuContext)d->modec;

    switch(tc->katsuyou){
    case  0:
      EWStrcpy( tc->hcode, "#K5" );     /* 置く */
      break;
    case  1:
      EWStrcpy( tc->hcode, "#G5" );     /* 仰ぐ */
      break;
    case  2:
      EWStrcpy( tc->hcode, "#S5" );     /* 返す */
      break;
    case  3:
      EWStrcpy( tc->hcode, "#T5" );     /* 絶つ */
      break;
    case  4:
      EWStrcpy( tc->hcode, "#N5" );     /* 死ぬ */
      break;
    case  5:
      EWStrcpy( tc->hcode, "#B5" );     /* 転ぶ */
      break;
    case  6:
      EWStrcpy( tc->hcode, "#M5" );     /* 住む */
      break;
    case  7:
      EWStrcpy( tc->hcode, "#R5" );     /* 威張る */
      break;
    case  8:
      EWStrcpy( tc->hcode, "#W5" );     /* 言う */
      break;
    }
}    

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 辞書の一覧                                                                *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuTDicExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;
  int cur;
  tourokuContext tc;

  d->nbytes = 0;

  popCallback(d); /* 一覧を pop */

  fc = (forichiranContext)d->modec;
  cur = fc->curIkouho;

  popForIchiranMode(d);
  popCallback(d);

  tc = (tourokuContext)d->modec;

  tc->workDic = cur;

  return(tangoTouroku(d));
}

static
uuTDicQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 一覧を pop */

  popForIchiranMode(d);
  popCallback(d);

  return(dicTourokuHinshi(d));
}

dicTourokuDictionary(d, exitfunc, quitfunc)
uiContext d;
int (*exitfunc)();
int (*quitfunc)();
{
  tourokuContext tc = (tourokuContext)d->modec;
  forichiranContext fc;
  ichiranContext ic;
  wchar_t **work;
  unsigned char inhibit = 0;
  int retval, upnelem = 0;

  retval = d->nbytes = 0;
  d->status = 0;

  for(work = tc->udic; *work; work++)
    upnelem++;

  if((retval = getForIchiranContext(d)) == NG) {
    if(tc->udic)
      free(tc->udic);
    defineEnd(d);
    return(GLineNGReturnTK(d));
  }
  fc = (forichiranContext)d->modec;

  /* selectOne を呼ぶための準備 */

  fc->allkouho = tc->udic;

  fc->curIkouho = 0;
  if( !HexkeySelect )
    inhibit |= ((unsigned char)NUMBERING | (unsigned char)CHARINSERT); 
  else
    inhibit |= (unsigned char)CHARINSERT;

   if((retval = selectOne(d, fc->allkouho, &fc->curIkouho, upnelem,
		 BANGOMAX, inhibit, 0, WITHOUT_LIST_CALLBACK,
		 0, exitfunc, quitfunc, uiUtilIchiranTooSmall)) == NG) {
    if(fc->allkouho)
      free(fc->allkouho);
    popForIchiranMode(d);
    popCallback(d);
    defineEnd(d);
    return(GLineNGReturnTK(d));
  }

  ic = (ichiranContext)d->modec;
  ic->majorMode = CANNA_MODE_ExtendMode;
  ic->minorMode = CANNA_MODE_TourokuDicMode;
  currentModeInfo(d);

  /* 候補一覧行が狭くて候補一覧が出せない */
  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return(retval);
  }

  makeGlineStatus(d);
  /* d->status = ICHIRAN_EVERYTIME; */

  return(retval);
}

/*
 * 単語登録を行う
 */
static
tangoTouroku(d)
uiContext	d;
{
  tourokuContext tc = (tourokuContext)d->modec;
  wchar_t ktmpbuf[256];
  wchar_t ttmpbuf[256];
  wchar_t line[ROMEBUFSIZE];
  wchar_t xxxx[1024];
  char dicname[1024];
  extern defaultContext;
  int linecnt;
  wchar_t *WStraddbcpy();

  defineEnd(d);
  if(tc->katsuyou || (EWStrncmp(tc->hcode, "#K5", 3) == 0)) {
    WStrncpy(ttmpbuf, tc->tango_buffer, tc->tango_len - 1);
    ttmpbuf[tc->tango_len - 1] = (wchar_t)0;
    WStrncpy(ktmpbuf, tc->yomi_buffer, tc->yomi_len - 1);
    ktmpbuf[tc->yomi_len - 1] = 0;
  } else {
    WStrcpy(ttmpbuf, tc->tango_buffer);
    WStrcpy(ktmpbuf, tc->yomi_buffer);
  }

  /* 辞書書き込み用の一行を作る */
  WStraddbcpy(line, ktmpbuf, ROMEBUFSIZE);
  EWStrcat(line, " ");
  WStrcat(line, tc->hcode);
  EWStrcat(line, " ");
  linecnt = WStrlen(line);
  WStraddbcpy(line + linecnt, ttmpbuf, ROMEBUFSIZE - linecnt);

  if(defaultContext == -1) {
    if((KanjiInit() < 0) || (defaultContext == -1)) {
      jrKanjiError = (char *)e_message[13];
      freeAndPopTouroku(d);
      return(GLineNGReturn(d));
    }
  }
  /* 辞書に登録する */
  WCstombs(dicname, tc->udic[tc->workDic], 1024);

  if(RkwDefineDic(defaultContext, dicname, line) != 0) {
    if(errno == EPIPE)
      jrKanjiPipeError();
    WStrcpy(d->genbuf, message[14]);
  } else {
    extern int auto_sync;

    if (auto_sync) {
      RkwSync(defaultContext, dicname);
    }
    /* 登録の完了を表示する */
    WSprintf(d->genbuf, message[15], message[16], tc->tango_buffer);
    WSprintf(xxxx, message[17], message[18], tc->yomi_buffer);
    WStrcat(d->genbuf, xxxx);
  }
  makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));

  freeAndPopTouroku(d);
  currentModeInfo(d);

  return(0); /* 単語登録完了 */
}
