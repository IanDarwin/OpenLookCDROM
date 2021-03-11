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
 * @(#) 102.1 $Id: canna.h,v 7.50 1994/04/21 07:00:05 kon Exp $
 */

#ifndef _CANNA_H_
#define _CANNA_H_

#include "cannaconf.h"

#include "widedef.h"

#include <stdio.h>

#if __STDC__
#include <stdlib.h>
#define pro(x) x
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#define pro(x) ()
#endif

#if defined(USG) || defined(SYSV) || defined(SVR4)
#include <string.h>
# ifndef index
# define index strchr
# endif
#else
#include <strings.h>
#endif

#include <canna/RK.h>
#include <canna/jrkanji.h>

#ifdef BIGPOINTER
#define POINTERINT long long
#else
#define POINTERINT long
#endif

#define	WCHARSIZE	(sizeof(wchar_t))

#ifdef HAVE_WCHAR_OPERATION

#ifndef JAPANESE_LOCALE
#define JAPANESE_LOCALE "japan"
#endif

#define MBstowcs mbstowcs
#define WCstombs wcstombs

#else

#define MBstowcs CANNA_mbstowcs
#define WCstombs CANNA_wcstombs

#endif

#include "sglobal.h"

#define XLookupKanji2			 IROHA_G300_XLookupKanji2
#define XKanjiControl2			 IROHA_G301_XKanjiControl2
#define XwcLookupKanji2			 IROHA_G425_XwcLookupKanji2
#define XwcKanjiControl2		 IROHA_G426_XwcKanjiControl2
#define FirstTime			 CANNA_G271_FirstTime

#define STROKE_LIMIT 500 /* ストロークで接続を切る */

#if defined(SYSV) || defined(SVR4) || __STDC__
# if defined(SYSV) || defined(SVR4)
#  include <memory.h>
# endif
# define bzero(buf, size) memset((char *)(buf), 0x00, (size))
# define bcopy(src, dst, size) memcpy((char *)(dst), (char *)(src), (size))
#endif

typedef unsigned char BYTE;

/*
 * CANNALIBDIR  -- システムのカスタマイズファイルやローマ字かな変換
 *                 テーブルが置かれるディレクトリ。
 */

#ifndef CANNALIBDIR
#define CANNALIBDIR "/usr/lib/canna"
#endif

/* flags の情報 */
#define CANNA_KANJIMODE_TABLE_SHARED	01
#define CANNA_KANJIMODE_EMPTY_MODE	02

/* func の第三引数 */
#define KEY_CALL  0
#define KEY_CHECK 1
#define KEY_SET   2

extern int searchfunc();
extern BYTE default_kmap[];

/* menuitem -- メニュー表示の項目を定義するテーブル */

typedef struct _menuitem {
  int flag; /* 下を見よ */
  union {
    struct _menustruct *menu_next; /* メニューへのポインタ */
    int fnum;    /* 機能番号 */
    char *misc;  /* その他(lisp のシンボルなど) */
  } u;
} menuitem;

#define MENU_SUSPEND 0 /* まだ決まっていない(lisp のシンボル) */
#define MENU_MENU    1 /* メニュー */
#define MENU_FUNC    2 /* 機能番号 */

/* menustruct -- メニューをしきる構造体 */

typedef struct _menustruct {
  int     nentries; /* メニューの項目の数 */
  wchar_t **titles; /* メニューの見出しリスト */
  wchar_t *titledata; /* 上のリストの実態文字列 */
  menuitem *body;   /* メニューの中身(配列) */
  int     modeid;   /* メニューのモード番号 */
  struct _menustruct *prev; /* 一つ前のメニューへのポインタ */
} menustruct;

typedef struct _menuinfo {
  menustruct *mstruct; /* どのメニューの */
  int        curnum;   /* こないだ選択された番号はこれですよ */
  struct _menuinfo *next;
} menuinfo;

/* defselection で定義された記号関係の一覧をとっておく構造体 */

typedef struct {
  wchar_t	**kigo_data;	/* 一覧表示の各要素の配列 */
  wchar_t	*kigo_str;	/* 一覧表示の全要素を入れる配列 */
  int		kigo_size;	/* 要素の数 */
  int		kigo_mode;	/* そのときのモード */
} kigoIchiran;

typedef struct _selectinfo {
  kigoIchiran	*ichiran;	/* どの一覧の */
  int		curnum;		/* 前回選択された番号 */
  struct _selectinfo *next;
} selectinfo;

/*
 * glineinfo -- 候補一覧表示のための内部情報を格納しておくための構造体。
 * それぞれのメンバは以下の意味を持つ。
 *
 * glkosu -- その行にある候補の数
 * glhead -- その行の先頭候補が、kouhoinfoの何番目か(0から数える)
 * gllen  -- その行を表示するための文字列の長さ
 * gldata -- その行を表示するための文字列へのポインタ
 */

typedef struct {
  int glkosu;
  int glhead;
  int gllen;
  wchar_t *gldata;
} glineinfo;

/*
 * kouhoinfo -- 候補一覧のための内部情報を格納しておくための構造体
 * それぞれのメンバは以下の意味を持つ。
 *
 * khretsu -- その候補がある行
 * khpoint -- その候補の行のなかでの位置
 * khdata -- その候補の文字列へのポインタ
 */

typedef struct {
  int khretsu;
  int khpoint;
  wchar_t *khdata;
} kouhoinfo;

#define ROMEBUFSIZE 	1024
#define	BANGOSIZE	2	/* 候補行中の各候補の番号のコラム数 */
#define	BANGOMAX   	9	/* １候補行中の最大候補数 */

#define	KIGOBANGOMAX   	16	/* １候補行中の最大候補数 */
#define GOBISUU		9

#define	ON		1
#define	OFF		0

#define	NG		-1

#define NO_CALLBACK	0
#define NCALLBACK	4

#define	JISHU_HIRA	0
#define JISHU_ZEN_KATA	1
#define JISHU_HAN_KATA	2
#define JISHU_ZEN_ALPHA	3
#define JISHU_HAN_ALPHA	4
#define MAX_JISHU	5

#define  SENTOU        0x01
#define  HENKANSUMI    0x02
#define  SUPKEY        0x04
#define  GAIRAIGO      0x08
#define  STAYROMAJI    0x10

/* identifier for each context structures */
#define CORE_CONTEXT       ((BYTE)0)
#define YOMI_CONTEXT       ((BYTE)1)
#define ICHIRAN_CONTEXT    ((BYTE)2)
#define FORICHIRAN_CONTEXT ((BYTE)3)
#define MOUNT_CONTEXT      ((BYTE)4)
#define TOUROKU_CONTEXT    ((BYTE)5)
#define TAN_CONTEXT	   ((BYTE)6)

typedef struct _coreContextRec {
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode; /* １つ前のモード */
  struct _coreContextRec *next;
} coreContextRec, *coreContext;

typedef coreContext mode_context;

typedef struct  _yomiContextRec {
  /* core 情報と同じ情報 */
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode;	/* １つ前のモード */
  mode_context    next;

  struct _kanjiMode *curMode;
  struct _tanContextRec	 *left, *right;

  /* ローマ字かな変換関係 */
  struct RkRxDic *romdic;	/* ローマ字かな変換辞書 */
  wchar_t   romaji_buffer[ROMEBUFSIZE];
  /* ローマ字バッファは rStartp, rEndp の２つのインデックスによって管理され
   * る。rStartp はカナに変換できなかったローマ字の最初の文字へのインデッ
   * クスであり、rEndp は新たにローマ字を入力する時に、格納すべき 
   * romaji_buffer 内のインデックスである。新たに入力されるローマ字は、
   * romaji_buffer + rEndp より先に格納され、そのローマ字をカナに変換す
   * る時は、romaji_buffer + rStartp から rEndp - rStartp バイトの文字が
   * 対象となる。 */
  int		  rEndp, rStartp, rCurs; /* ローマ字バッファのポインタ */
  wchar_t         kana_buffer[ROMEBUFSIZE];
  BYTE            rAttr[ROMEBUFSIZE], kAttr[ROMEBUFSIZE];
  int		  kEndp; /* かなバッファの最後を押えるポインタ */
  int             kRStartp, kCurs;

  /* その他のオプション */
  BYTE            myMinorMode;  /* yomiContext 固有のマイナモード */
  struct _kanjiMode *myEmptyMode;		/* empty モードはどれか */
  long		  generalFlags;		/* see below */
  long		  savedFlags;		/* 上のフラグの一部のセーブ */
  BYTE		  savedMinorMode;	/* マイナモードのセーブ */
  BYTE		  allowedChars;		/* see jrkanji.h */
  BYTE		  henkanInhibition;	/* see below */
  int             cursup;		/* ロかなの補追の時に使う */
#define SUSPCHARBIAS 100
  int             n_susp_chars;

/* from henkanContext */
  /* カナ漢字変換関係 */
  int            context;
  int		 kouhoCount;	/* 何回 henkanNext が連続して押されたか */
  wchar_t        echo_buffer[ROMEBUFSIZE];
  wchar_t        **allkouho; /* RkGetKanjiListで得られる文字列を配列にして
				とっておくところ */
  int            curbun;     /* カレント文節 */
  int		 curIkouho;  /* カレント候補 */
  int            nbunsetsu;  /* 文節の数 */

/* ifdef MEASURE_TIME */
  long		 proctime;   /* 処理時間(変換で計測する) */
  long		 rktime;     /* 処理時間(RKにかかる時間) */
/* endif MEASURE_TIME */
/* end of from henkanContext */

/* 逐次コンテキストから */
  int		 ye, ys, status;
/* 逐次コンテキストから(ここまで) */
  int		 cStartp, cRStartp; /* 逐次で読みとして残っている部分 */

/* 字種コンテキストから */
  BYTE           inhibition;
  BYTE           jishu_kc, jishu_case;
  int            jishu_kEndp, jishu_rEndp;
  short          rmark;
/* 字種コンテキストから(ここまで) */

/* adjustContext から */
  int kanjilen, bunlen;           /* 漢字部分、文節の長さ */
/* adjustContext から(ここまで) */
  struct _kanjiMode *tanMode; /* 単候補のときのモード */
  int tanMinorMode;     /*        〃            */

  /* 作業用変数 */
  int		  last_rule;		/* 前回のロかな変換に使われたルール */
  wchar_t	  *retbuf, *retbufp;
  int		  retbufsize;
  short           pmark, cmark; /* １つ前のマークと今のマーク */
  BYTE            englishtype;  /* 英語タイプ(以下を見よ) */
} yomiContextRec, *yomiContext;

/* for generalFlags */
#define CANNA_YOMI_MODE_SAVED		0x01L /* savedFlags にしか使われない */

#define CANNA_YOMI_BREAK_ROMAN		0x01L
#define CANNA_YOMI_CHIKUJI_MODE		0x02L
#define CANNA_YOMI_CHGMODE_INHIBITTED	0x04L
#define CANNA_YOMI_END_IF_KAKUTEI	0x08L
#define CANNA_YOMI_DELETE_DONT_QUIT	0x10L

#define CANNA_YOMI_IGNORE_USERSYMBOLS	0x20L
#define CANNA_YOMI_IGNORE_HENKANKEY	0x40L

#define CANNA_YOMI_BASE_CHIKUJI		0x80L /* 心は逐次 */

/* for generalFlags also used in savedFlags */

/* 以下の ATTRFUNCS にマスクされるビットは defmode の属性として使われる */
#define CANNA_YOMI_KAKUTEI		0x0100L
#define CANNA_YOMI_HENKAN		0x0200L
#define CANNA_YOMI_ZENKAKU		0x0400L
#define CANNA_YOMI_HANKAKU		0x0800L /* 実際に半角 */
#define CANNA_YOMI_HIRAGANA		0x1000L
#define CANNA_YOMI_KATAKANA		0x2000L
#define CANNA_YOMI_ROMAJI		0x4000L
#define CANNA_YOMI_JISHUFUNCS		0x7c00L
#define CANNA_YOMI_ATTRFUNCS		0x7f00L

#define CANNA_YOMI_BASE_HANKAKU		0x8000L /* 心は半角 */

/* kind of allowed input keys */
#define CANNA_YOMI_INHIBIT_NONE		0
#define CANNA_YOMI_INHIBIT_HENKAN	1
#define CANNA_YOMI_INHIBIT_JISHU	2
#define CANNA_YOMI_INHIBIT_ASHEX	4
#define CANNA_YOMI_INHIBIT_ASBUSHU	8
#define CANNA_YOMI_INHIBIT_ALL		15

/* 候補一覧のためのフラグ */
#define NUMBERING 			1
#define CHARINSERT			2

#define CANNA_JISHU_UPPER		1
#define CANNA_JISHU_CAPITALIZE		2

/* englishtype */
#define CANNA_ENG_KANA			0 /* 縮小すること */
#define CANNA_ENG_ENG1			1
#define CANNA_ENG_ENG2			2 /* 両端に空白が入っている */
#define CANNA_ENG_NO			3

/* yc->status のフラグ(逐次用) */

#define	CHIKUJI_ON_BUNSETSU		0x0001 /* 文節上にある */
#define	CHIKUJI_OVERWRAP		0x0002 /* 文節かつ読み状態？ */
#define	CHIKUJI_NULL_STATUS	        0 /* 上のを消す用 */

/* yc を使うモードの区別(優先順) */

#define adjustp(yc) (0< (yc)->bunlen)
#define jishup(yc) (0 < (yc)->jishu_kEndp)
#define chikujip(yc) ((yc)->generalFlags & CANNA_YOMI_CHIKUJI_MODE)
#define henkanp(yc) (0 < (yc)->nbunsetsu)

#define chikujiyomiremain(yc) ((yc)->cStartp < (yc)->kEndp)

typedef struct _ichiranContextRec {
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode;	/* １つ前のモード */
  mode_context    next;

  int            svIkouho;   /* カレント候補を一時とっておく(一覧表示行) */
  int            *curIkouho; /* カレント候補 */
  int            nIkouho;    /* 候補の数(一覧表示行) */
  int		 tooSmall;   /* カラム数が狭くて候補一覧が出せないよフラグ */
  int            curIchar;   /* 未確定文字列ありの単語登録の単語入力の
    							先頭文字の位置 */
  BYTE           inhibit;
  BYTE           flags;	     /* 下を見てね */
  wchar_t        **allkouho; /* RkGetKanjiListで得られる文字列を配列にして
				とっておくところ */
  wchar_t        *glinebufp; /* 候補一覧のある一行を表示するための文字
				列へのポインタ */
  kouhoinfo      *kouhoifp;  /* 候補一覧関係の情報を格納しておく構造体
				へのポインタ */
  glineinfo      *glineifp;  /* 候補一覧関係の情報を格納しておく構造体
				へのポインタ */
} ichiranContextRec, *ichiranContext;

/* フラグの意味 */
#define ICHIRAN_ALLOW_CALLBACK 1 /* コールバックをしても良い */
#define ICHIRAN_STAY_LONG    0x02 /* 選ぶと抜ける */
#define ICHIRAN_NEXT_EXIT    0x04 /* 次の quit で抜ける */


typedef struct _foirchiranContextRec {
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode;	/* １つ前のモード */
  mode_context    next;

  int            curIkouho;  /* カレント候補 */
  wchar_t        **allkouho; /* RkGetKanjiListで得られる文字列を配列にして
				とっておくところ */
  menustruct     *table;  /* 文字列と関数のテーブル */
  int            *prevcurp;  /* 前のカレント候補 */
} forichiranContextRec, *forichiranContext;

typedef struct _mountContextRec {
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode;	/* １つ前のモード */
  mode_context    next;

  BYTE            *mountOldStatus; /* マウントされているかいないか */
  BYTE            *mountNewStatus; /* マウントされているかいないか */
  char            **mountList;   /* マウント可能な辞書の一覧 */
  int            curIkouho;     /* カレント候補 */
} mountContextRec, *mountContext;

typedef struct _tourokuContextRec {
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode;	/* １つ前のモード */
  mode_context    next;

  wchar_t        genbuf[ROMEBUFSIZE];
  wchar_t        qbuf[ROMEBUFSIZE];
  wchar_t        tango_buffer[ROMEBUFSIZE];
  int            tango_len;  /* 単語登録の単語の文字列の長さ */
  wchar_t        yomi_buffer[ROMEBUFSIZE];
  int            yomi_len;   /* 単語登録の読みの文字列の長さ */
  int            curHinshi;  /* 品詞の選択 */
  int            workDic;    /* 作業用の辞書 */
  struct dicname *newDic;    /* 追加する辞書 */
  wchar_t	 hcode[16];  /* 単語登録の品詞 */
  int		 katsuyou;   /* 単語登録の動詞の活用形 */
  wchar_t        **udic;     /* 単語登録できる辞書 (辞書名) */
  int            delContext; /* 単語削除で１つの辞書をマウントする */
} tourokuContextRec, *tourokuContext;

typedef struct _tanContextRec {
  BYTE id;
  BYTE majorMode, minorMode;
  struct _kanjiMode *prevMode;	/* １つ前のモード */
  mode_context    next;

  struct _kanjiMode *curMode;
  struct _tanContextRec	 *left, *right;

  struct RkRxDic *romdic;	/* ローマ字かな変換辞書 */
  BYTE            myMinorMode;  /* yomiContext 固有のマイナモード */
  struct _kanjiMode *myEmptyMode;		/* empty モードはどれか */
  long generalFlags, savedFlags; /* yomiContext のコピー */
  BYTE		  savedMinorMode;	/* マイナモードのセーブ */
  BYTE		  allowedChars;		/* see jrkanji.h */
  BYTE		  henkanInhibition;	/* see below */

  wchar_t *kanji, *yomi, *roma;
  BYTE *kAttr, *rAttr;
} tanContextRec, *tanContext;

struct moreTodo {
  BYTE          todo; /* もっとあるの？を示す */
  BYTE          fnum; /* 関数番号。０なら次の文字で示されることをする */
  char		ch;   /* 文字 */
};

/* モード名を格納するデータの型定義 */

struct ModeNameRecs {
  int           alloc;
  wchar_t       *name;
};

/* 一覧の番号のセパレーターのデフォルトの定義 */

#define DEFAULTINDEXSEPARATOR     '.'

/* 

  uiContext はローマ字かな変換、カナ漢字変換に使われる構造体である。
  XLookupKanjiString などによる変換は、ウィンドウに分離された複数の入
  力ポートに対応しているので、入力中のローマ字の情報や、カナ漢字変換
  の様子などをそれぞれのウィンドウ毎に分離して保持しておかなければな
  らない。この構造体はそのために使われる構造体である。
 
  構造体のメンバがどのようなものがあるかは、定義を参照すること
 
 */

typedef struct _uiContext {

  /* XLookupKanjiStringのパラメタ */
  wchar_t        *buffer_return;
  int            n_buffer;
  wcKanjiStatus    *kanji_status_return;

  /* XLookupKanjiStringの戻り値である文字列の長さ */
  int		 nbytes;

  /* キャラクタ */
  char ch;

  /* セミグローバルデータ */
  int		 contextCache;	 /* 変換コンテクストキャッシュ */
  struct _kanjiMode *current_mode;
  BYTE		 majorMode, minorMode;	 /* 直前のもの */

  short		 curkigo;	 /* カレント記号(記号全般) */
  char           currussia;	 /* カレント記号(ロシア文字) */
  char           curgreek;	 /* カレント記号(ギリシャ文字) */
  char           curkeisen;	 /* カレント記号(罫線) */
  short          curbushu;       /* カレント部首名 */
  int            ncolumns;	 /* 一行のコラム数、候補一覧の時に用いられる */
  wchar_t        genbuf[ROMEBUFSIZE];	/* 汎用バッファ */
  short          strokecounter;  /* キーストロークのカウンタ
				    ローマ字モードでクリアされる */

  /* リストコールバック関連 */
  char           *client_data;   /* アプリケーション用データ */
  void           (*list_func) pro((char *, int, wchar_t **, int, int *));
                 /* リストコールバック関数 */
  /* その他 */
  char		 flags;		 /* 下を見てね */
  char		 status;	 /* どのような状態で返ったのかを示す値
				    そのモードとして、
				     ・処理中
				     ・処理終了
				     ・処理中断
				     ・その他
				    がある。(下を見よ) */

  /* コールバックチェーン */
  struct callback *cb;

  /* もっとすることがあるよという構造体 */
  struct moreTodo more;

  /* クイットチェーン */
  menustruct *prevMenu;

  /* 各メニューで選ばれた番号を記録しておく構造体へのポインタ */
  menuinfo *minfo;

  /* 各一覧で選ばれた番号を記録しておく構造体へのポインタ */
  selectinfo *selinfo;

  /* サブコンテクストへのリンク */
  mode_context   modec;		/* 全部ここにつなぐ予定 */
} uiContextRec, *uiContext;

/* uiContext の flags のビットの意味 */
#define PLEASE_CLEAR_GLINE	1	/* GLine を消してね */
#define PCG_RECOGNIZED		2	/* GLine を次は消しますよ */
#define MULTI_SEQUENCE_EXECUTED	4	/* さっきマルチシーケンスが行われた */

#define EVERYTIME_CALLBACK	0
#define EXIT_CALLBACK		1
#define QUIT_CALLBACK		2
#define AUX_CALLBACK		3

/* 
 * カナ漢字変換のための様々なキーマップテーブル 
 * キーマップテーブルは処理関数へのポインタの配列となっている。
 */

struct funccfunc {
  BYTE funcid;
  int (*cfunc) pro((struct _uiContext *));
};

typedef struct _kanjiMode {
  int (*func) pro((struct _uiContext *, struct _kanjiMode *, int, int, int));
  BYTE *keytbl;
  int flags;			/* 下を見よ */
  struct funccfunc *ftbl;
} *KanjiMode, KanjiModeRec;

struct callback {
  int (*func[NCALLBACK]) pro((struct _uiContext *, int, mode_context));
  mode_context    env;
  struct callback *next;
};

/* ローマ字かな変換辞書 */
     
extern struct RkRxDic *romajidic;
extern struct RkRxDic *RkwOpenRoma pro((unsigned char *));

/*
 * 辞書の名前を入れておく変数
 */

struct dicname {
  struct dicname *next;
  char *name;
  int dictype;
  unsigned long dicflag;
};

/* dictype には以下のいずれかが入る */
#define DIC_PLAIN 0
#define DIC_USER  1
#define DIC_BUSHU 2
#define DIC_GRAMMAR 3
#define DIC_RENGO 4
#define DIC_KATAKANA 5
#define DIC_HIRAGANA 6

/* dicflag には以下のいずれかが入る */
#define DIC_NOT_MOUNTED  0
#define DIC_MOUNTED      1
#define DIC_MOUNT_FAILED 2

extern struct dicname *kanjidicnames;

/*
 * エラーのメッセージを入れておく変数
 */

extern char *jrKanjiError;

/*
 * デバグ文を表示するかどうかのフラグ
 */

extern iroha_debug;

/*
 * キーシーケンスを発生するようなキー
 */

#define IrohaFunctionKey(key) \
  ((0x80 <= (int)(unsigned char)(key) &&  \
    (int)(unsigned char)(key) <= 0x8b) || \
   (0x90 <= (int)(unsigned char)(key) &&  \
    (int)(unsigned char)(key) <= 0x9b) || \
   (0xe0 <= (int)(unsigned char)(key) &&  \
    (int)(unsigned char)(key) <= 0xff) )

/* selectOne でコールバックを伴うかどうかを表すマクロ */

#define WITHOUT_LIST_CALLBACK 0
#define WITH_LIST_CALLBACK    1

/*
 * Rk 関数をトレースするための名前の書き換え。
 */

#ifdef DEBUG
#include "traceRK.h"
#endif /* DEBUG */

/*
 * デバグメッセージ出力用のマクロ
 */

#ifdef DEBUG
#define debug_message(fmt, x, y, z)	dbg_msg(fmt, x, y, z)
#else /* !DEBUG */
#define debug_message(fmt, x, y, z)
#endif /* !DEBUG */

/*
 * malloc のデバグ
 */

#ifdef DEBUG_ALLOC
extern char *debug_malloc pro((int));
extern int fail_malloc;
#define malloc(n) debug_malloc(n)
#endif /* DEBUG_MALLOC */

/*
 * 新しいモードを定義する構造体
 */

typedef struct {
  char           *romaji_table; /* ローマ字かな変換テーブル名(EUC) */
  struct RkRxDic *romdic;	 /* ローマ字辞書構造体 */
  int             romdic_owner;  /* ローマ字辞書を自分でOpenしたか */
  long            flags;	 /* flags for yomiContext->generalFlags */
  KanjiMode       emode;	 /* current_mode に入る構造体 */
} newmode;

/* ローマ字かな変換を補足するキーと文字の変換テーブル */

typedef struct {
  wchar_t	key;		/* キー */
  int		groupid;	/* グループid */
  int           ncand;          /* 候補の数 */
  wchar_t       **cand;         /* 候補の配列 */
  wchar_t	*fullword;	/* 候補列 (候補1@候補2@...候補n@@) */
} keySupplement;

#define MAX_KEY_SUP 64

#define HEX_USUAL     0
#define HEX_IMMEDIATE 1

#define ModeInfoStyleIsString		0
#define ModeInfoStyleIsNumeric		1
#define ModeInfoStyleIsExtendedNumeric	2
#define MaxModeInfoStyle		ModeInfoStyleIsExtendedNumeric

#define killmenu(d) ((d)->prevMenu = (menustruct *)0)
#define	defineEnd(d) killmenu(d)
#define	deleteEnd(d) killmenu(d)

/* defmode、defselection、defmenu 用の構造体 */

typedef struct _extra_func {
  int  		fnum;		/* 関数番号 */
  int		keyword;	/* 新しいモードが定義されたキーワード */
  wchar_t	*display_name;	/* モード表示名 */
  union {
    newmode 	*modeptr;	/* defmode に対応する構造体 */
    kigoIchiran	*kigoptr;	/* defselection に対応する構造体 */
    menustruct	*menuptr;	/* defmenu に対応する構造体 */
  } u;
  struct _extra_func *next;
} extraFunc;

#define EXTRA_FUNC_DEFMODE	1
#define EXTRA_FUNC_DEFSELECTION	2
#define EXTRA_FUNC_DEFMENU	3

#define tanbunMode(d, tan) /* tanContext 関連モードへの移行 */ \
  { extern KanjiModeRec tankouho_mode; (d)->current_mode = &tankouho_mode; \
    (d)->modec = (mode_context)(tan); currentModeInfo(d); }

#define freeForIchiranContext(fc) free((char *)fc)
#define freeIchiranContext(ic) free((char *)ic)
#define freeYomiContext(yc) free((char *)yc)
#define freeCoreContext(cc) free((char *)cc)

#ifndef DICHOME
#define DICHOME "/usr/lib/canna/dic"
#endif

#define DEFAULT_CANNA_SERVER_NAME "cannaserver"

#ifndef	_UTIL_FUNCTIONS_DEF_

#define	_UTIL_FUNCTIONS_DEF_

/* よくスペルミスするのでコンパイル時にひっかかるように入れる */
extern RkwGoto pro((char *, int));

extern void makeGLineMessage pro((uiContext, wchar_t *, int));
extern void makeGLineMessageFromStrings pro((uiContext, char *));
extern newmode *findExtraKanjiMode pro((int));
extern setWStrings pro((wchar_t **, char **, int));
extern WStrlen pro((wchar_t *));
extern wchar_t *WStrcat pro((wchar_t *, wchar_t *));
extern wchar_t *WStrcpy pro((wchar_t *, wchar_t *));
extern wchar_t *WStrncpy pro((wchar_t *, wchar_t *, int));
extern WStrncmp pro((wchar_t *, wchar_t *, int));
extern wchar_t *WString pro((char *));
extern prevMenuIfExist pro((uiContext));
extern showmenu pro((uiContext, menustruct *));
extern yomiContext
  newYomiContext pro((wchar_t *, int, int, int, int, int)),
  GetKanjiString pro((uiContext, wchar_t *, int, int, int, int, int,
		      int (*)(uiContext, int, mode_context),
		      int (*)(uiContext, int, mode_context),
		      int (*)(uiContext, int, mode_context)));
extern void restoreFlags pro((yomiContext));
extern void kPos2rPos pro((yomiContext, int, int, int *, int *));
extern void makeKanjiStatusReturn pro((uiContext, yomiContext));
extern wchar_t key2wchar pro((int, int *));
extern struct bukRec *internContext
  pro((unsigned int, unsigned int, uiContext));
extern void freeRomeStruct pro((uiContext));
extern void rmContext pro((unsigned int, unsigned int));
extern struct callback *pushCallback
  pro((uiContext, mode_context,
       int (*)(uiContext, int, mode_context),
       int (*)(uiContext, int, mode_context),
       int (*)(uiContext, int, mode_context),
       int (*)(uiContext, int, mode_context)));
extern void popCallback pro((uiContext));
extern void makeYomiReturnStruct pro((uiContext));
extern void moveToChikujiTanMode pro((uiContext));
extern void moveToChikujiYomiMode pro((uiContext));
extern void makeGLineMessageFromString pro((uiContext, char *));
extern void addWarningMesg pro((char *));
extern int prepareHenkanMode pro((uiContext));
extern void makeAllContextToBeClosed pro((int));
extern void Beep pro((void));
extern void freeAllMenuInfo pro((menuinfo *));
extern void freeMenu pro((menustruct *));
extern void restoreDefaultKeymaps pro((void));
extern void finExtMenu pro((void));
extern void popTourokuMode pro((uiContext));
extern void freeIchiranBuf pro((ichiranContext));
extern char *RkGetServerName pro((void));
extern void popForIchiranMode pro((uiContext));
extern void clisp_main pro((void));
extern void clisp_fin pro((void));
extern void popYomiMode pro((uiContext));
extern void freeTanContext pro((tanContext));
extern void enterJishuMode pro((uiContext, yomiContext));
extern void leaveJishuMode pro((uiContext, yomiContext));
extern void finishTanKakutei pro((uiContext));
extern void removeKana pro((uiContext, yomiContext, int, int));
extern void clearHenkanContext pro((yomiContext));
extern void doMuhenkan pro((uiContext, yomiContext));
extern void removeCurrentBunsetsu pro((uiContext, tanContext));

#endif /* _UTIL_FUNCTIONS_DEF_ */

#endif /* !_CANNA_H_ */
