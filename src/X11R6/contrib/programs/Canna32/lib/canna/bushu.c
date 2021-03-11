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
static char rcs_id[] = "@(#) 102.1 $Id: bushu.c,v 5.15 1994/03/10 02:20:13 kon Exp $";
#endif /* lint */

#include	<errno.h>
#include "canna.h"

extern int  errno;

extern wchar_t *WString();
extern HexkeySelect;

extern int makeGlineStatus(), uuslQuitCatch();
extern int uiUtilIchiranTooSmall(), uuslIchiranQuitCatch();
static int bushuHenkan(), makeBushuIchiranQuit();
static int vBushuExitCatch(), bushuQuitCatch();


#define	BUSHU_SZ	150

static
char *bushu_schar[] = { "一", "丿", "凵", "十", "卩", "刀",
			 
			 "刈（りっとう）", "力", "厂", "勹", "冂匚囗", "亠",

			 "冫", "人／仁（にんべん）", "又", "几", "八", "儿",
			 
			 "冖", "宀", "廴", "郁（おおざと)", "己", "女",

			 "彳", "口", "草（くさかんむり)", "独（けものへん）",

			 "子", "陏（こざと）", "士", "江（さんずい）", "弋",

			 "尸", "小／単（つ）", "辷（しんにょう）", "寸", "大",

			 "土", "手（てへん）", "巾", "广", "山", "夕",

			 "弓", "忙（りっしんべん）", "欠", "歹", "犬",

			 "牛／牡（うしへん）", "片", "木", "气", "毛", "心",

			 "水", "月", "爪", "日", "攵", "火",

			 "方", "戈", "点（れっか）", "殳", "穴", "石",

			 "玉", "皮", "瓦", "皿", "示", "神（しめすへん）", "白",

			 "田", "立", "禾", "目", "癶", "矢",

			 "疔（やまいだれ）", "四", "糸", "臼", "瓜", "老",

			 "缶", "衣", "初（ころもへん）", "米", "舌", "耒",

			 "竹（たけかんむり）", "血", "虎（とらかんむり）", "肉",

			 "西", "羽", "羊", "聿", "舟", "耳",

			 "虫", "赤", "足／疋", "豕", "臣",

			 "貝", "辛", "車", "見", "言", "酉", "走", "谷",

			 "角", "釆", "麦", "豆", "身", "豸", "雨", "非",

			 "金", "門", "隹", "頁", "音", "香", "革", "風",

			 "首", "食", "韋", "面", "馬", "鬼", "髟", "高",

			 "鬥", "骨", "魚", "亀", "鳥", "黒", "鹿", "鼻",

			 "齒", "記号", "その他"
			 };

static
char *bushu_skey[] =  { "いち", "の", "うけばこ", "じゅう", "ふし", "かたな",
			  "りっとう", "か", "がん", "く", "かまえ", "なべ", "に",
			  
			  "ひと", "ぬ", "つくえ", "はち", "る", "わ",
			  
			  "う", "えん", "おおざと", "おのれ", "おんな", "ぎょう",
			  
			  "ろ", "くさ", "けもの", "こ", "こざと", "さむらい",
			  
			  "し", "しき", "しゃく", "つ", "しん", "すん",
			  
			  "だい", "ど", "て", "はば", "ま", "やま",
			  
			  "ゆう", "ゆみ", "りっしん", "けつ", "いちた", "いぬ",
			  
			  "うし", "かた", "き", "きがまえ", "け", "こころ",
			  
			  "すい", "つき", "つめ", "にち", "のぶん", "ひ",
			  
			  "ほう", "ほこ", "よつてん", "るまた", "あな", "いし",
			  
			  "おう", "かわ", "かわら", "さら", "しめす", "ね",
			  
			  "しろ", "た", "たつ", "のぎ", "め", "はつ", "や",

			  "やまい", "よん", "いと", "うす", "うり", "おい",

			  "かん", "きぬ", "ころも", "こめ", "した", "すき",

			  "たけ", "ち", "とら", "にく", "にし", "はね", "ひつじ",

			  "ふで", "ふね", "みみ", "むし", "あか", "あし",

			  "いのこ", "おみ", "かい", "からい", "くるま", "けん",

			  "ごん", "さけ", "そう", "たに", "つの", "のごめ",

			  "ばく", "まめ", "み", "むじな", "あめ", "あらず",

			  "かね", "もん", "ふるとり", "ぺーじ", "おと", "こう",

			  "かく", "かぜ", "くび", "しょく", "なめし", "めん",

			  "うま", "おに", "かみ", "たかい", "とう", "ほね",

			  "うお", "かめ", "とり", "くろ", "しか", "はな",

			  "は", "きごう", "そのた"
			  };

#define	BUSHU_CNT	(sizeof(bushu_schar)/sizeof(char *))

static wchar_t *bushu_char[BUSHU_CNT];
static wchar_t *bushu_key[BUSHU_CNT];

int
initBushuTable()
{
  setWStrings(bushu_char, bushu_schar, BUSHU_CNT);
  setWStrings(bushu_key, bushu_skey, BUSHU_CNT);
}


/*
 * 部首候補のエコー用の文字列を作る
 *
 * 引き数	RomeStruct
 * 戻り値	正常終了時 0
 */
static int
makeBushuEchoStr(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  d->kanji_status_return->echoStr = ic->allkouho[*(ic->curIkouho)];
  d->kanji_status_return->length = 1;
  d->kanji_status_return->revPos = 0;
  d->kanji_status_return->revLen = 1;

  return(0);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * forichiranContext用関数                                                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * forichiranContext の初期化
 */
static
clearForIchiranContext(p)
forichiranContext p;
{
  p->id = FORICHIRAN_CONTEXT;
  p->curIkouho = 0;
  p->allkouho = 0;

  return(0);
}
  
static forichiranContext
newForIchiranContext()
{
  forichiranContext fcxt;

  if((fcxt = 
      (forichiranContext)malloc(sizeof(forichiranContextRec))) == NULL) {
    jrKanjiError = "malloc (newForIchiranContext) できませんでした";
    return(0);
  }
  clearForIchiranContext(fcxt);

  return fcxt;
}

getForIchiranContext(d)
uiContext d;
{
  forichiranContext fc;
  int retval = 0;

  if(pushCallback(d, d->modec, 0, 0, 0, 0) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }
  
  if((fc = newForIchiranContext()) == NULL) {
    popCallback(d);
    return(NG);
  }
  fc->next = d->modec;
  d->modec = (mode_context)fc;

  fc->prevMode = d->current_mode;
  fc->majorMode = d->majorMode;

  return(retval);
}

void
popForIchiranMode(d)
uiContext d;
{
  forichiranContext fc = (forichiranContext)d->modec;

  d->modec = fc->next;
  d->current_mode = fc->prevMode;
  freeForIchiranContext(fc);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 部首モード入力                                                            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
vBushuMode(d, major_mode)
uiContext d;
int major_mode;
{
  forichiranContext fc;
  ichiranContext ic;
  unsigned char inhibit = 0;
  int retval = 0;

  d->status = 0;

  if((retval = getForIchiranContext(d)) == NG) {
    killmenu(d);
    return(GLineNGReturn(d));
  }

  fc = (forichiranContext)d->modec;

  /* selectOne を呼ぶための準備 */
  fc->allkouho = bushu_char;
  fc->curIkouho = 0;
  if( !HexkeySelect )
    inhibit |= ((unsigned char)NUMBERING | (unsigned char)CHARINSERT);
  else
    inhibit |= (unsigned char)CHARINSERT;

  if((retval = selectOne(d, fc->allkouho, &fc->curIkouho, BUSHU_SZ,
		 BANGOMAX, inhibit, 0, WITH_LIST_CALLBACK,
		 0, vBushuExitCatch,
		 bushuQuitCatch, uiUtilIchiranTooSmall)) == NG) {
    killmenu(d);
    return(GLineNGReturnFI(d));
  }

  ic = (ichiranContext)d->modec;
  ic->majorMode = major_mode;
  ic->minorMode = CANNA_MODE_BushuMode;
  currentModeInfo(d);

  *(ic->curIkouho) = d->curbushu;

  /* 候補一覧行が狭くて候補一覧が出せない */
  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    killmenu(d);
    return(retval);
  }

  if ( !(ic->flags & ICHIRAN_ALLOW_CALLBACK) ) {
    makeGlineStatus(d);
  }
  /* d->status = ICHIRAN_EVERYTIME; */

  return(retval);
}

static
vBushuIchiranQuitCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  popCallback(d); /* 一覧をポップ */

  if (((forichiranContext)env)->allkouho != (wchar_t **)bushu_char) {
    /* bushu_char は static の配列だから free してはいけない。
       こう言うのってなんか汚いなあ */
    freeGetIchiranList(((forichiranContext)env)->allkouho);
  }
  popForIchiranMode(d);
  popCallback(d);

  return(vBushuMode(d, CANNA_MODE_BushuMode));
}

static
vBushuExitCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  forichiranContext fc;
  int cur, res;

  popCallback(d); /* 一覧をポップ */

  fc = (forichiranContext)d->modec;
  cur = fc->curIkouho;

  popForIchiranMode(d);
  popCallback(d);

  res = bushuHenkan(d, 1, 1, cur, vBushuIchiranQuitCatch);
  if (res < 0) {
    makeYomiReturnStruct(d);
    return 0;
  }
  return res;
}

BushuMode(d)
uiContext d;
{
  yomiContext yc = (yomiContext)d->modec;

  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    killmenu(d);
    return NothingChangedWithBeep(d);
  }    

  return(vBushuMode(d, CANNA_MODE_BushuMode));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 部首モード入力の一覧表示                                                  *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
bushuEveryTimeCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  makeBushuEchoStr(d);

  return(retval);
}

static
bushuExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  yomiContext yc;

  popCallback(d); /* 一覧をポップ */

  if (((forichiranContext)env)->allkouho != bushu_char) {
    /* bushu_char は static の配列だから free してはいけない。
       こう言うのってなんか汚いなあ */
    freeGetIchiranList(((forichiranContext)env)->allkouho);
  }
  popForIchiranMode(d);
  popCallback(d);
  yc = (yomiContext)d->modec;
  if (yc->savedFlags & CANNA_YOMI_MODE_SAVED) {
    restoreFlags(yc);
  }
  retval = YomiExit(d, retval);
  killmenu(d);
  currentModeInfo(d);

  return retval;
}

static
bushuQuitCatch(d, retval, env)
     uiContext d;
     int retval;
     mode_context env;
     /* ARGSUSED */
{
  popCallback(d); /* 一覧をポップ */

  if (((forichiranContext)env)->allkouho != (wchar_t **)bushu_char) {
    /* bushu_char は static の配列だから free してはいけない。
       こう言うのってなんか汚いなあ */
    freeGetIchiranList(((forichiranContext)env)->allkouho);
  }
  popForIchiranMode(d);
  popCallback(d);
  currentModeInfo(d);
  GlineClear(d);

  return prevMenuIfExist(d);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 部首としての変換の一覧表示                                                *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
convBushuQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
{
  popCallback(d); /* 一覧をポップ */

  if (((forichiranContext)env)->allkouho != (wchar_t **)bushu_char) {
    /* bushu_char は static の配列だから free してはいけない。
       こう言うのってなんか汚いなあ */
    freeGetIchiranList(((forichiranContext)env)->allkouho);
  }
  popForIchiranMode(d);
  popCallback(d);

  makeYomiReturnStruct(d);
  currentModeInfo(d);

  return(retval);
}

/*
 * 読みを部首として変換する
 *
 * 引き数	uiContext
 * 戻り値	正常終了時 0	異常終了時 -1
 */
ConvertAsBushu(d)
uiContext	d;
{
  yomiContext yc = (yomiContext)d->modec;
  int res;

  d->status = 0; /* clear status */
  
  if (yc->henkanInhibition & CANNA_YOMI_INHIBIT_ASBUSHU ||
      yc->right || yc->left) {
    return NothingChangedWithBeep(d);
  }

  if (yc->generalFlags & CANNA_YOMI_CHIKUJI_MODE) {
    if (!(yc->status & CHIKUJI_OVERWRAP) && yc->nbunsetsu) {
      moveToChikujiTanMode(d);
      return TanKouhoIchiran(d);
    }
    else if (yc->nbunsetsu) {
      return NothingChanged(d);
    }
  }

  d->nbytes = yc->kEndp;
  WStrncpy(d->buffer_return, yc->kana_buffer, d->nbytes);

  /* 0 は、ConvertAsBushu から呼ばれたことを示す */
  res = bushuHenkan(d, 0, 1, 0, convBushuQuitCatch);
  if (res < 0) {
    makeYomiReturnStruct(d);
    return 0;
  }
  return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 共通部                                                                    *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * 読みを部首辞書から部首変換する
 */
static
bushuBgnBun(st, yomi, length)
RkStat *st;
wchar_t *yomi;
int length;
{
  int nbunsetsu;
  extern defaultBushuContext;

  /* 連文節変換を開始する *//* 辞書にある候補のみ取り出す */
  if ((defaultBushuContext == -1)) {
    if (KanjiInit() == -1 || defaultBushuContext == -1) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      return(NG);
    }
  }

  if((nbunsetsu = RkwBgnBun(defaultBushuContext, yomi, length, 0)) == -1) {
    if(errno == EPIPE)
      jrKanjiPipeError();
    jrKanjiError = "かな漢字変換に失敗しました";
    return(NG);
  }
  
  if(RkwGetStat(defaultBushuContext, st) == -1) {
    if(errno == EPIPE)
      jrKanjiPipeError();
    jrKanjiError = "ステイタスを取り出せませんでした";
    return(NG);
  }

  return(nbunsetsu);
}

/*
 * 読みに半濁点を付加して候補一覧行を表示する
 *
 * 引き数	uiContext
 *		flag	ConvertAsBushuから呼ばれた 0
 *			BushuYomiHenkanから呼ばれた 1
 * 戻り値	正常終了時 0	異常終了時 -1
 *
 *
 * ここに来る時はまだ getForIchiranContext が呼ばれていないものとする
 */

static
bushuHenkan(d, flag, ext, cur, quitfunc)
uiContext	d;
int             flag, cur;
int             (*quitfunc)();
{
  forichiranContext fc;
  ichiranContext ic;
  unsigned char inhibit = 0;
  wchar_t *yomi, **allBushuCands;
  RkStat	st;
  int nelem, currentkouho, nbunsetsu, length, retval = 0;
  extern defaultBushuContext;
  
  wchar_t **getIchiranList();

  if(flag) {
    yomi = (wchar_t *)bushu_key[cur];
    length = WStrlen(yomi);
    d->curbushu = (short)cur;
  } else {
    d->nbytes = RomajiFlushYomi(d, d->buffer_return, d->n_buffer);
    yomi = d->buffer_return;
    length = d->nbytes;
  }

  if((nbunsetsu = bushuBgnBun(&st, yomi, length)) == NG) {
    killmenu(d);
    (void)GLineNGReturn(d);
    return -1;
  }

  if((nbunsetsu != 1) || (st.klen > 1) || (st.maxcand == 0)) {
    /* 部首としての候補がない */

    d->kanji_status_return->length = -1;

    makeBushuIchiranQuit(d, flag);
    currentModeInfo(d);

    killmenu(d);
    if(flag) {
      makeGLineMessageFromString(d, "この部首の候補はありません");
    } else {
      return(NothingChangedWithBeep(d));
    }
    return(0);
  }

  /* 候補一覧行を表示する */
  /* 0 は、カレント候補 + 0 をカレント候補にすることを示す */

  if((allBushuCands
      = getIchiranList(defaultBushuContext, &nelem, &currentkouho)) == 0) {
    killmenu(d);
    (void)GLineNGReturn(d);
    return -1;
  }

  /* 部首変換は学習しない。 */
  if(RkwEndBun(defaultBushuContext, 0) == -1) { /* 0:学習しない */
    if(errno == EPIPE)
      jrKanjiPipeError();
    jrKanjiError = "かな漢字変換の終了に失敗しました";
    freeGetIchiranList(allBushuCands);
    killmenu(d);
    (void)GLineNGReturn(d);
    return -1;
  }

  if(getForIchiranContext(d) == NG) {
    freeGetIchiranList(allBushuCands);
    killmenu(d);
    (void)GLineNGReturn(d);
    return -1;
  }

  fc = (forichiranContext)d->modec;
  fc->allkouho = allBushuCands;

  if( !HexkeySelect )
    inhibit |= (unsigned char)NUMBERING;
  fc->curIkouho = currentkouho;	/* 現在のカレント候補番号を保存する */
  currentkouho = 0;	/* カレント候補から何番目をカレント候補とするか */

  if((retval = selectOne(d, fc->allkouho, &fc->curIkouho, nelem, BANGOMAX,
			 inhibit, currentkouho, WITH_LIST_CALLBACK,
			 bushuEveryTimeCatch, bushuExitCatch,
			 quitfunc, uiUtilIchiranTooSmall)) == NG) {
    freeGetIchiranList(allBushuCands);
    killmenu(d);
    (void)GLineNGReturnFI(d);
    return -1;
  }

  ic = (ichiranContext)d->modec;

  if(!flag) { /* convertAsBushu */
    ic->majorMode = ic->minorMode = CANNA_MODE_BushuMode;
  } else {
    if(ext) {
      ic->majorMode = ic->minorMode = CANNA_MODE_BushuMode;
    } else {
      ic->majorMode = CANNA_MODE_ExtendMode;
      ic->minorMode = CANNA_MODE_BushuMode;
    }
  }
  currentModeInfo(d);

  /* 候補一覧行が狭くて候補一覧が出せない */
  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    killmenu(d);
    return(retval);
  }

  if ( !(ic->flags & ICHIRAN_ALLOW_CALLBACK) ) {
    makeGlineStatus(d);
  }
  /* d->status = EVERYTIME_CALLBACK; */

  return(retval);
}

/*
 * 候補行を消去し、部首モードから抜け、読みがないモードに移行する
 *
 * 引き数	uiContext
 *		flag	ConvertAsBushuから呼ばれた 0
 *			BushuYomiHenkanから呼ばれた 1
 * 戻り値	正常終了時 0	異常終了時 -1
 */
static
makeBushuIchiranQuit(d, flag)
uiContext	d;
int              flag;
{
  extern defaultBushuContext;

  /* 部首変換は学習しない。 */
  if(RkwEndBun(defaultBushuContext, 0) == -1) { /* 0:学習しない */
    if(errno == EPIPE)
      jrKanjiPipeError();
    jrKanjiError = "かな漢字変換の終了に失敗しました";
    return(NG);
  }

  if(flag) {
    /* kanji_status_return をクリアする */
    d->kanji_status_return->length  = 0;
    d->kanji_status_return->revLen  = 0;
    
/*
    d->status = QUIT_CALLBACK;
*/
  } else {
    makeYomiReturnStruct(d);
  }
  GlineClear(d);
  
  return(0);
}

