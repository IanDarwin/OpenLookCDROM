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

/*
 *
 *	8/16 bit String Manipulations.
 *
 *      "@(#)kanji.h	2.3    88/10/03 10:25:34"
 *      "@(#) 102.1 $Id: jrkanji.h,v 6.2 1994/01/06 10:06:11 kon Exp $"
 */

#ifndef _JR_KANJI_H_
#define _JR_KANJI_H_

#ifndef _WCHAR_T
# if defined(WCHAR_T) || defined(_WCHAR_T_) || defined(__WCHAR_T) \
  || defined(_GCC_WCHAR_T)
#  define _WCHAR_T
# endif
#endif

#include <canna/keydef.h>
#include <canna/mfdef.h>

/* どのような情報があるかを示すフラグ */

#define KanjiModeInfo   	01
#define KanjiGLineInfo  	02
#define KanjiYomiInfo		04
#define KanjiThroughInfo	010
#define KanjiEmptyInfo		020

/* KanjiControl 関係 */

#define KC_INITIALIZE		0
#define KC_FINALIZE		1
#define KC_CHANGEMODE		2
#define KC_SETWIDTH		3
#define KC_SETUNDEFKEYFUNCTION	4
#define KC_SETBUNSETSUKUGIRI    5
#define KC_SETMODEINFOSTYLE	6
#define KC_SETHEXINPUTSTYLE	7
#define KC_INHIBITHANKAKUKANA	8
#define KC_DEFINEKANJI		9
#define KC_KAKUTEI		10
#define KC_KILL			11
#define KC_MODEKEYS		12
#define KC_QUERYMODE		13
#define KC_QUERYCONNECTION	14
#define KC_SETSERVERNAME        15
#define KC_PARSE		16
#define KC_YOMIINFO		17
#define KC_STOREYOMI		18
#define KC_SETINITFILENAME	19
#define KC_DO			20
#define KC_GETCONTEXT		21
#define KC_CLOSEUICONTEXT	22
#define KC_INHIBITCHANGEMODE	23
#define KC_LETTERRESTRICTION	24
#define KC_QUERYMAXMODESTR	25
#define KC_SETLISTCALLBACK	26
#define KC_SETVERBOSE		27
#define KC_LISPINTERACTION	28
#define KC_DISCONNECTSERVER	29
#define KC_SETAPPNAME	        30
#define KC_DEBUGMODE	        31
#define KC_DEBUGYOMI	        32
#define KC_KEYCONVCALLBACK	33
#define KC_QUERYPHONO		34

#define MAX_KC_REQUEST          (KC_QUERYPHONO + 1)

#define kc_normal	0
#define kc_through	1
#define kc_kakutei	2
#define kc_kill		3

#define CANNA_NOTHING_RESTRICTED	0
#define CANNA_ONLY_ASCII		1
#define CANNA_ONLY_ALPHANUM		2	
#define CANNA_ONLY_HEX			3
#define CANNA_ONLY_NUMERIC		4
#define CANNA_NOTHING_ALLOWED		5

#ifdef IROHA_BC
#define IROHA_NOTHING_RESTRICTED	CANNA_NOTHING_RESTRICTED
#define IROHA_ONLY_ASCII		CANNA_ONLY_ASCII
#define IROHA_ONLY_ALPHANUM		CANNA_ONLY_ALPHANUM
#define IROHA_ONLY_HEX			CANNA_ONLY_HEX
#define IROHA_ONLY_NUMERIC		CANNA_ONLY_NUMERIC
#define IROHA_NOTHING_ALLOWED		CANNA_NOTHING_ALLOWED
#endif

#define CANNA_MAXAPPNAME       256

typedef struct {
    unsigned char *echoStr;    /* local echo string */
    int length;		        /* length of echo string */
    int revPos;                 /* reverse position  */
    int revLen;                 /* reverse length    */
    unsigned long info;		/* その他の情報 */
    unsigned char *mode;	/* モード情報 */
    struct {
      unsigned char *line;
      int           length;
      int           revPos;
      int           revLen;
    } gline;			/* 一覧表示のための情報 */
} jrKanjiStatus;

typedef struct {
  int  val;
  unsigned char *buffer;
  int  bytes_buffer;
  jrKanjiStatus *ks;
} jrKanjiStatusWithValue;

#ifdef _WCHAR_T

typedef struct {
    wchar_t *echoStr;		/* local echo string */
    int length;		        /* length of echo string */
    int revPos;                 /* reverse position  */
    int revLen;                 /* reverse length    */
    unsigned long info;		/* その他の情報 */
    wchar_t  *mode;		/* モード情報 */
    struct {
      wchar_t       *line;
      int           length;
      int           revPos;
      int           revLen;
    } gline;			/* 一覧表示のための情報 */
} wcKanjiStatus;

typedef struct {
  int  val;
  wchar_t *buffer;
  int  n_buffer;
  wcKanjiStatus *ks;
} wcKanjiStatusWithValue;

typedef struct {
  char *client_data;
#if __STDC__ || defined(__cplusplus)
  void (*callback_func)(char *, int, wchar_t **, int, int *);
#else
  void (*callback_func)();
#endif
} jrListCallbackStruct;

#define listCallbackStruct jrListCallbackStruct

#define CANNA_LIST_Start           0
#define CANNA_LIST_Select          1
#define CANNA_LIST_Quit            2
#define CANNA_LIST_Forward         3
#define CANNA_LIST_Backward        4
#define CANNA_LIST_Next            5
#define CANNA_LIST_Prev            6
#define CANNA_LIST_BeginningOfLine 7
#define CANNA_LIST_EndOfLine       8

#endif /* _WCHAR_T */

#define CANNA_NO_VERBOSE   0
#define CANNA_HALF_VERBOSE 1
#define CANNA_FULL_VERBOSE 2

#define CANNA_CTERMINAL 0
#define CANNA_XTERMINAL 1

#ifdef __cplusplus
extern "C" {
extern char *jrKanjiError;
}
#else
extern char *jrKanjiError;
#endif

#define wcBeepFunc jrBeepFunc

#if __STDC__ || defined(__cplusplus)
#ifdef __cplusplus
extern "C" {
#endif

int jrKanjiString(const int, const int, char *, const int, jrKanjiStatus *);
int jrKanjiControl(const int, const int, char *);

int kanjiInitialize(char ***), kanjiFinalize(char ***);
int createKanjiContext();
int jrCloseKanjiContext(const int, jrKanjiStatusWithValue *);

#ifdef _WCHAR_T
int wcKanjiString(const int, const int, wchar_t *, const int, wcKanjiStatus *);
int wcKanjiControl(const int, const int, char *);

int wcCloseKanjiContext(const int, wcKanjiStatusWithValue *);
#endif /* _WCHAR_T */

#ifdef __cplusplus
}
#endif
#else /* !(__STDC__ || defined(__cplusplus)) */

int jrKanjiString(), jrKanjiControl();

int kanjiInitialize(), kanjiFinalize(), createKanjiContext();
int jrCloseKanjiContext();

#ifdef _WCHAR_T
int wcKanjiString(), wcKanjiControl(), wcCloseKanjiContext();
#endif /* _WCHAR_T */

#endif /* !(__STDC__ || defined(__cplusplus)) */

#endif /* _JR_KANJI_H_ */

