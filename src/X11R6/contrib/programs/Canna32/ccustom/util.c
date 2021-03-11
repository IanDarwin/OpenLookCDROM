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

/* filedef

  util.c -- ユーティリティ関数を集めた。

  以下の関数がある。(追加した人はちゃんと書いといてよ)

  GlineClear         ガイドラインが消されるようなリターン値を作る
  Gline2echostr      ガイドラインで返そうとしたものをその場で返す
  echostr2Gline      その場で返そうとしたものガイドラインで返す
  echostrClear       その場が全く消されるようなリターン値を作る
  checkGLineLen      ガイドラインに表示しきれるかどうかのチェック
  NothingChanged     何も変化がないことを示すリターン値を作る
  NothingForGLine    ガイドラインに関しては何も変化がない
  NothingChangedWithBeep
                     NothingChange をしてさらにビープ音を鳴らす
  NothingForGLineWithBeep
                     NothingForGLine をしてさらにビープ音を鳴らす
  ujisncpy           EUC 文字を n バイトコピーする
  makeGLineMessage   引数の文字列をGLineに表示するようなリターン値を作る
  makeGLineMessageFromString
  		     引数のeuc文字列をGLineに表示するようなリターン値を作る
  copyYomiBuffers    他の読みコンテクストからの読みデータのコピー
  setWStrings	     文字配列の初期化を行う
  NoMoreMemory       メモリがないからエラーだよというエラー値を返す
  GLineNGReturn      エラーメッセージをガイドラインに移す
  GLineNGReturnFI    一覧モードを抜けて GLineNGReturn をする。
  GLineNGReturnTK    登録モードを抜けて GLineNGReturn をする。
  WStrlen            ワイドキャラクタ文字列の長さを求める (cf. strlen)
  WStrcat            ワイドキャラクタ文字列を加える。(cf. strcat)
  WStrcpy            ワイドキャラクタ文字列をコピーする。(cf. strcpy)
  WStrncpy           ワイドキャラクタ文字列をｎ文字コピーする。(cf. strncpy)
  WStrcmp	     ワイドキャラクタ文字列を比較する。(cf. strcmp)
  WStrncmp	     ワイドキャラクタ文字列をｎ文字比較する。(cf. strncmp)
  WToupper	     ワイドキャラクタの英字小文字を大文字に変換する。
  WWhatGPlain	     ワイドキャラクタ１文字の属するグラフィックプレーンを返す
  WIsG0              G0のワイドキャラクタ文字か？
  WIsG1              G1のワイドキャラクタ文字か？
  WIsG2              G2のワイドキャラクタ文字か？
  WIsG3              G3のワイドキャラクタ文字か？
  WGetLeft           G1、G3の左側(きたねー)
  WGetRight          G1、G3の右側(きたねー)
  MBstowcs           EUC をワイドキャラクタ文字列に変換
  CNvW2E             ワイドキャラクタを EUC に変換(チェック付き)
  WCstombs           ワイドキャラクタを EUC に変換
  WSfree	     WStringで確保した領域を開放する
  WString            EUC をワイドに変換して malloc までして返す(free 不要)
  WStringOpen        上記関数の初期化処理
  WStringClose       上記関数の終了処理

 */

static char sccs_id[] = "@(#) 102.1 $Id: util.c,v 1.6 1993/03/01 08:08:45 kon Exp $";

#define _WCHAR16 /* そうじゃないものをコンパイルする時ははずしてね */
                 /* 本当は動的に切り替えられるはず */

#include "canna.h"

#if __STDC__
#include <stdlib.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

#ifdef SOMEONE_USE_THIS
/* 誰も使っていないみたい。 */
Insertable(ch)
unsigned char ch;
{
  if ((0x20 <= ch && ch <= 0x7f) || (0xa0 <= ch && ch <= 0xff)) {
    return 1;
  }
  else {
    return 0;
  }
}
#endif /* SOMEONE_USE_THIS */

/* 
 ujisncpy -- ２バイト文字の途中で切れないようにコピーする関数

  実際にコピーするバイト数は n で指定されたバイト数かあるいはそれより
  １小さい値になる。実際にコピーしたバイト数を返す。

  SS3 には対応していない。
 */

int
ujisncpy(dest, src, n)
unsigned char *dest, *src;
int n;
{
  int i = 0;
  unsigned char c;

  while (i < n) {
    c = src[i];
    if ( ! (c & 0x80) ) { /* ASCII の場合 */
      dest[i++] = c;
    }
    else if (i + 1 < n) { /* 漢字かカナの場合、しかもコピーできる場合 */
      dest[i++] = c;
      dest[i] = src[i];
      i++;
    }
    else { /* 漢字で１バイトしかコピーできない場合 */
      return i; /* コピーしきれず、n より１小さい値で終わった。 */
    }
  }
  return i; /* n バイトコピーしきれた */
}

setWStrings(ws, s, sz)
wchar_t **ws;
unsigned char **s;
int sz;
{
  int f = sz;
  wchar_t *WString();

  for (; (f && sz) || (!f && *s); ws++, s++, sz--)
    *ws = WString(*s);
}


copyAttribute(dest, src, n)
     BYTE	*dest;
     BYTE	*src;
     int n;
{
  for (; n; n--)
    *dest++ = *src++;
}

/*
 * ワイドキャラクタオペレーション
 *
 */

wchar_t
WToupper(w)
wchar_t w;
{
  if (WIsG0(w)) {
    if ('a' <= w && w <= 'z')
      return((wchar_t) (w - 'a' + 'A'));
  } else
    return(w);
}

int
WStrlen(ws)
wchar_t *ws;
{
  int res = 0;
  while (*ws++) {
    res++;
  }
  return res;
}

wchar_t *
WStrcpy(ws1, ws2)
wchar_t *ws1, *ws2;
{
  wchar_t *ws;
  int cnt, len;

  for (ws = ws2, cnt = 0 ; *ws ; ws++, cnt++) ;
  len = cnt;
  if (ws2 < ws1 && ws1 < ws2 + cnt) {
    while (cnt--) {
      ws1[cnt] = ws2[cnt];
    }
  }
  else {
    ws = ws1;
    while (*ws2) {
      *ws++ = *ws2++;
    }
  }
  ws1[len] = (wchar_t)0;
  return ws1;
}

wchar_t *
WStrncpy(ws1, ws2, cnt)
wchar_t *ws1, *ws2;
int cnt;
{
  wchar_t *ws;

  if  (ws2 == (wchar_t *) NULL)
    return;
  if (ws2 < ws1 && ws1 < ws2 + cnt) {
    while (cnt--) {
      ws1[cnt] = ws2[cnt];
    }
  }
  else {
    int i = 0;
    ws = ws1;
    while (i++ < cnt && *ws2) {
      *ws++ = *ws2++;
    }
  }
  return ws1;
}

wchar_t *
WStrcat(ws1, ws2)
wchar_t *ws1, *ws2;
{
  wchar_t *ws;

  for (ws = ws1; *ws; ws++);
  WStrcpy(ws, ws2);
  return ws1;
}

int
WStrcmp(w1, w2)
wchar_t *w1, *w2;
{
  for (; *w1 && *w1 == *w2; w1++, w2++);
  return(*w1 - *w2);
}

int
WStrncmp(w1, w2, n)
wchar_t *w1, *w2;
int n;
{
  if (n == 0) return(0);
  for (; --n && *w1 && *w1 == *w2; w1++, w2++);
  return(*w1 - *w2);
}

/* WWhatGPlain -- どのグラフィックプレーンの文字か？

   戻り値:
     0 : G0 ASCII
     1 : G1 漢字(JISX0208)
     2 : G2 半角カタカナ(JISX0201)
     3 : G3 外字(補助漢字 JISX0212)
 */

int
WWhatGPlain(wc)
wchar_t wc;
{
#ifdef _WCHAR16
  switch (((unsigned long)wc) & 0x8080) {
  case 0x0000:
    return 0;
  case 0x8080:
    return 1;
  case 0x0080:
    return 2;
  case 0x8000:
    return 3;
  }
#else /* !_WCHAR16 */
  static char plain[4] = {0, 2, 3, 1};

  return plain[(((unsigned long)wc) >> 28) & 3];
#endif /* !_WCHAR16 */
}

int
WIsG0(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 0);
}

int
WIsG1(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 1);
}

int
WIsG2(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 2);
}

int
WIsG3(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 3);
}

/* 以下の２つの関数は２バイトまで適用可 */

int
WGetLeft(wc)
wchar_t wc;
{
  if (WIsG0(wc) || WIsG2(wc))
    return 0;
  else {
#ifdef _WCHAR16
    return ((((unsigned long)wc) >> 8) & 0x7f);
#else /* !_WCHAR16 */
    return ((((unsigned long)wc) >> 7) & 0x7f);
#endif /* !_WCHAR16 */
  }
}

int
WGetRight(wc)
wchar_t wc;
{
  if (WIsG0(wc) || WIsG2(wc))
    return 0;
  else {
    return (((unsigned long)wc) & 0x7f);
  }
}

int
MBstowcs(dest, src, destlen)
wchar_t *dest;
unsigned char *src;
int destlen;
{
  register int i, j;
  register unsigned char ec;

  for (i = 0, j = 0 ; (ec = src[i]) && j < destlen ; i++) {
    if (ec & 0x80) {
      switch (ec) {
      case 0x8e: /* SS2 */
	dest[j++] = (wchar_t)(0x80 | ((unsigned)src[++i] & 0x7f));
	break;
      case 0x8f: /* SS3 */
	dest[j++] = (wchar_t)(0x8000
			      | (((unsigned)src[i + 1] & 0x7f) << 8)
			      | ((unsigned)src[i + 2] & 0x7f));
	i += 2;
	break;
      default:
	dest[j++] = (wchar_t)(0x8080 | (((unsigned)src[i] & 0x7f) << 8)
			      | ((unsigned)src[i + 1] & 0x7f));
	i++;
	break;
      }
    }
    else {
      dest[j++] = (wchar_t)ec;
    }
  }
  if (j < destlen)
    dest[j] = (wchar_t)0;
  return j;
}

int
CNvW2E(src, srclen, dest, destlen)
wchar_t *src;
unsigned char *dest;
int srclen, destlen;
{
  register int i, j;
  register wchar_t wc;

  for (i = 0, j = 0 ; i < srclen && j + 2 < destlen ; i++) {
    wc = src[i];
    switch (wc & 0x8080) {
    case 0:
      /* ASCII */
      dest[j++] = (unsigned char)((unsigned)wc & 0x7f);
      break;
    case 0x0080:
      /* 半角カナ */
      dest[j++] = 0x8e; /* SS2 */
      dest[j++] = (unsigned char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    case 0x8000:
      /* 外字 */
      dest[j++] = 0x8f; /* SS3 */
      dest[j++] = (unsigned char)((((unsigned)wc & 0x7f00) >> 8) | 0x80);
      dest[j++] = (unsigned char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    case 0x8080:
      /* 漢字 */
      dest[j++] = (unsigned char)((((unsigned)wc & 0x7f00) >> 8) | 0x80);
      dest[j++] = (unsigned char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    }
  }
  dest[j] = (unsigned char)0;
  return j;
}

int
WCstombs(dest, src, destlen)
unsigned char *dest;
wchar_t *src;
int destlen;
{
  return CNvW2E(src, WStrlen(src), dest, destlen);
}

/* cfuncdef

  WString -- EUCからワイドキャラクタへのマッピングおよび malloc

  WString は引数の文字列をワイドキャラクタに変換し、その文字列が収まる
  だけのメモリを malloc し、その文字列を納め返す。

  利用者はこの関数で得たポインタを free する必要はあまりない。

  すなわち、この関数で得たメモリは後で WStringClose を呼び出したときに
  free される。

  そういう事情なのでこの関数を頻繁に呼び出してはいけない。今までEUCで
  初期定義できていた文字列などに留めるべきである。

  この機能を使う人は最初に WStringOpen を呼び出さなければならないが、
  ユーザインタフェースライブラリではシステムが自動的に読んでくれるの
  でその必要はない。

 */ 

static wchar_t **wsmemories = NULL;
static int nwsmemories = 0;

#define WSBLOCKSIZE 128

int
WStringOpen()
{
  return 0;
}

wchar_t *
WString(s)
unsigned char *s;
{
  int i, len;
  wchar_t *temp;

  if (wsmemories == (wchar_t **)NULL) {
    nwsmemories = WSBLOCKSIZE;
    wsmemories = (wchar_t **)calloc(nwsmemories, sizeof(wchar_t *));
    /* calloc されたメモリはクリアされている */
  }

  for (i = 0 ; i < nwsmemories && wsmemories[i] ; i++);

  if (i == nwsmemories) { /* 使い切ったので増やす */
    wsmemories = (wchar_t **)realloc(wsmemories,
				     (nwsmemories + WSBLOCKSIZE) 
				     * sizeof(wchar_t *));
    for (; i < nwsmemories + WSBLOCKSIZE ; i++) {
      wsmemories[i] = (wchar_t *)0;
    }
    i = nwsmemories;
    nwsmemories += WSBLOCKSIZE;
  }

  /* とりあえず大きくとっておいて、そのサイズを見て丁度のサイズに
     直して返す */

  len = strlen((char *)s);
  temp = (wchar_t *)malloc((len + 1) * WCHARSIZE);
  len = MBstowcs(temp, s, len);
  wsmemories[i] = (wchar_t *)malloc((len + 1) * WCHARSIZE);
  WStrncpy(wsmemories[i], temp, len);
  free(temp);
  wsmemories[i][len] = (wchar_t)0;
  return wsmemories[i];
}

int
WStringClose()
{
  int i;

  for (i = 0 ; i < nwsmemories ; i++) {
    if (wsmemories[i]) {
      free(wsmemories[i]);
    }
  }
  free(wsmemories);
  wsmemories = (wchar_t **)0;
  nwsmemories = 0;
}

WSfree(s)
wchar_t *s;
{
  int	i;
  wchar_t **t;
  for (t = wsmemories, i = nwsmemories; s != *t && i; t++, i--)
    ;
  free(*t);
  *t = (wchar_t *) 0;
}


