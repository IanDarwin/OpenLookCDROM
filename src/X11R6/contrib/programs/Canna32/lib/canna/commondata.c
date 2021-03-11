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
static char rcs_id[] = "@(#) 102.1 $Id: commondata.c,v 7.14 1994/04/12 12:55:20 kon Exp $";
#endif /* lint */

#include "canna.h"
#include <canna/mfdef.h>
#include "patchlevel.h"
     
/* この『かんな』のバージョン */

int CannaVersion = CANNA_MAJOR_MINOR;

/* デフォルトのローマ字かな変換用のバッファ */

int defaultContext = -1;
int defaultBushuContext = -1;

/* ローマ字かな変換辞書 */
/*
 * ローマ字かな変換テーブルは１個あれば良いでしょう。複数個必要なので
 * あれば RomeStruct のメンバとして入れておく必要もありましょうが...そ
 * の時はその時で考えましょう。
 */
     
struct RkRxDic *romajidic, *englishdic, *RkwOpenRoma();

/* 未定義キー打鍵時の処理のしかた */

int howToBehaveInCaseOfUndefKey = kc_normal;

/*
 * 辞書の名前を入れておく変数
 */

char saveapname[CANNA_MAXAPPNAME]; /* サーバとの接続を切るときのAP名 */

/*
 * ローマ字かな変換をしつくすかどうかのフラグ
 */

int forceKana = 0;

/*
 * 候補一覧の時に何番目の候補かを表示するかどうかのフラグ
 */

int kCount = 1;

/*
 * 候補一覧時にコールバックを禁止するかどうかのフラグ
 */

int iListCB = 0;

/*
 * irohacheck コマンドによって使われているかとか、
 * irohacheck ないでの verbose を表す値。
 */

int ckverbose = 0;

/*
 * エラーのメッセージを入れておく変数
 */

char *jrKanjiError = "";

/*
 * デバグメッセージを出すかどうかのフラグ
 */

int iroha_debug = 0;

/*
 * 始めての使用かどうかを示すフラグ
 */

int FirstTime = 1;

/*
 * ビープ音を鳴らす関数を格納するところ
 */

int (*jrBeepFunc)() = NULL;

/*
 * KC_INITIALIZE 直後に実行する機能の列
 */

BYTE *initfunc = (BYTE *)0;
int howToReturnModeInfo = ModeInfoStyleIsString;
int chikuji = 0;
int LearnNumericalType = 0;
int CursorWrap = 1;
int SelectDirect = 0;
int HexkeySelect = 1;
int Gakushu = 1;
int BreakIntoRoman = 0;
int InhibitHankakuKana = 0;
int QuitIchiranIfEnd = 0;
char *RomkanaTable = (char *)NULL;
char *EnglishTable = (char *)NULL;
/* char *Dictionary = (char *)NULL; */
struct dicname *RengoGakushu = (struct dicname *)NULL;
struct dicname *KatakanaGakushu = (struct dicname *)NULL;
struct dicname *HiraganaGakushu = (struct dicname *)NULL;

int nKouhoBunsetsu = 16;
int BunsetsuKugiri = 0;
int stayAfterValidate = 1;
int kakuteiIfEndOfBunsetsu = 0;

int ChBasedMove = 1;
int ReverseWidely = 0;
int ReverseWord = 0;
int keepCursorPosition = 0;
int KeepCursorPosition = 0;
int ChikujiContinue = 1;
int RenbunContinue = 0;
int kouho_threshold = 2;
int strokelimit = STROKE_LIMIT;

int grammaticalQuestion = 1;

int nothermodes = 0;

keySupplement keysup[MAX_KEY_SUP];
int nkeysup = 0;

int abandonIllegalPhono = 0; /* ルール外の入力を捨てるかどうか */

int hexCharacterDefiningStyle = HEX_USUAL;

/*
 * 初期化の際使用した初期化ファイル名を全てとっておくバッファ。
 * ファイル名は","で区切られる。(拡張機能で使用)
 */

char *CANNA_initfilename = (char *)0;

/*
 * バージョン
 */

int protocol_version = -1;
int server_version = -1;
char *server_name = (char *)0;

int kojin = 1;
int indexHankaku = 0;
int indexSeparator = DEFAULTINDEXSEPARATOR;
int allowNextInput = 1;
int chikujiRealBackspace = 1;
int BackspaceBehavesAsQuit = 1;
int doKatakanaGakushu = 1;
int doHiraganaGakushu = 1;
int chikuji_debug = 0;
int ignore_case = 0;
int romaji_yuusen = 0;
int auto_sync = 1;
int quickly_escape = 0;

int locale_insufficient = 0;

void (*keyconvCallback)() = (void (*)())0;

extraFunc *extrafuncp = (extraFunc *)0;
struct dicname *kanjidicnames; /* .canna で指定している辞書リスト */

/*
  デファールト値にもどす。
*/
void
restoreBindings()
{
  if (initfunc) free(initfunc);
  initfunc = (BYTE *)0;

  if (server_name) free(server_name);
  server_name = (char *)0;

  RomkanaTable = (char *)NULL;
  EnglishTable = (char *)NULL;
  romajidic = (struct RkRxDic *)0;
  englishdic = (struct RkRxDic *)0;
  RengoGakushu = (struct dicname *)NULL;
  KatakanaGakushu = (struct dicname *)NULL;
  HiraganaGakushu = (struct dicname *)NULL;
  howToBehaveInCaseOfUndefKey = kc_normal;
  CursorWrap = 1;
  SelectDirect = 0;
  HexkeySelect = 1;
  BunsetsuKugiri = 0;
/*  kanjidicname[nkanjidics = 0] = (char *)NULL; 代わりのことをしなければ */
  kanjidicnames = NULL;
  saveapname[0] = '\0';
  ChBasedMove = 1;
  ReverseWidely = 0;
  Gakushu = 1;
  QuitIchiranIfEnd = 0;
  BreakIntoRoman = 0;
  kouho_threshold = 2;
  grammaticalQuestion = 1;
  stayAfterValidate = 1;
  kakuteiIfEndOfBunsetsu = 0;  
  kCount = 1;
  hexCharacterDefiningStyle = HEX_USUAL;
  keepCursorPosition = 0;
  KeepCursorPosition = 0;
  ChikujiContinue = 1;
  RenbunContinue = 0;

  InhibitHankakuKana = 0;
  nothermodes = 0;
  protocol_version = server_version = -1;
  kojin = 1;
  indexHankaku = 0;
  indexSeparator = DEFAULTINDEXSEPARATOR;
  allowNextInput = 1;
  LearnNumericalType = 0;
  chikujiRealBackspace = 1;
  nKouhoBunsetsu = 16;
  ReverseWord = 0;
  nkeysup = 0;
  abandonIllegalPhono = 0;
  BackspaceBehavesAsQuit = 1;
  doKatakanaGakushu = 1;
  doHiraganaGakushu = 1;
  chikuji_debug = 0;
  strokelimit = STROKE_LIMIT;
  ignore_case = 0;
  romaji_yuusen = 0;
  auto_sync = 1;
  quickly_escape = 0;
  keyconvCallback = (void (*)())0;
  locale_insufficient = 0;
}
