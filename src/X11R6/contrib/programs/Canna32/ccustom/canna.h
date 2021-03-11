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
 * @(#) 102.1 $Id: canna.h,v 1.11 1994/05/31 13:23:15 kon Exp $
 * $Revision: 1.11 $
 * $Author: kon $
 */

#ifndef _CANNA_H_
#define _CANNA_H_

#include "widedef.h"

#include <stdio.h>

#define	WCHARSIZE	sizeof(wchar_t)

/* #include "sglobal.h" これはリリース時にはコメントをはずすこと */
#define necKanjiError jrKanjiError

/*#include "jrkanji.h"*/

#if defined(SYSV) || defined(SVR4) || __STDC__
# if defined(SYSV) || defined(SVR4)
#  include <memory.h>
# endif
# define bzero(buf, size) memset((char *)(buf), 0x00, (size))
# define bcopy(src, dst, size) memcpy((char *)(dst), (char *)(src), (size))
#endif

typedef unsigned char BYTE;

/*
 * LIBDIR  -- システムのカスタマイズファイルやローマ字かな変換テーブルが
 *            置かれるディレクトリ。
 */

#ifndef CANNALIBDIR
#define CANNALIBDIR "/usr/lib/canna"
#endif

#define XKanjiStatus          jrKanjiStatus
#define XKanjiStatusWithValue jrKanjiStatusWithValue

/* 
 * カナ漢字変換のための様々なキーマップテーブル 
 * キーマップテーブルは処理関数へのポインタの配列となっている。
 */

typedef struct {
  int (*func)();
  unsigned char *keytbl;
  int flags;			/* 下を見よ */
  int (**ftbl)();
} *KanjiMode, KanjiModeRec;

struct funccfunc {
  unsigned char funcid;
  int (*cfunc)();
};

/* flags の情報 */
#define CANNA_KANJIMODE_TABLE_SHARED	01
#define CANNA_KANJIMODE_EMPTY_MODE	02

/* func の第三引数 */
#define KEY_CALL  0
#define KEY_CHECK 1
#define KEY_SET   2

extern BYTE default_kmap[];

#define CANNA_FULL_VERBOSE 2

/* menuitem -- メニュー表示の項目を定義するテーブル */

typedef struct _menuitem {
  wchar_t *title;
  int (*func)();
  struct _menuitem *menu_next;
  int *prev_kouho;
  int minorMode;
} menuitem;

#define NEXT_MENU (int (*)())0 /* menuitem の func フィールドに入る */

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
#define	BANGOSIZE	2	/* 候補行中の各候補の番号のバイト数 */
#define	REVSIZE		2	/* 候補行中の番号のリバースのバイト数 */
#define	BANGOMAX   	9	/* １候補行中の最大候補数 */
#define KIGOSIZE	1	/* 記号候補のバイト数 */
#define GL_KUHAKUSIZE	1	/* 候補番号の空白文字のバイト数 */
#define KG_KUHAKUSIZE	1	/* 記号の間の空白文字のバイト数 */
#define KIGOCOL		(KIGOSIZE + KG_KUHAKUSIZE)
					/* bangomaxを計算するための数 */
#define	KIGOBANGOMAX   	16	/* １候補行中の最大候補数 */
#define HINSHISUU	14
#define HINSHIBUF	256
#define GOBISUU		9

#define	ON		1
#define	OFF		-1

#define	NG		-1

#define NO_CALLBACK	0
#define NCALLBACK	4

#define	JISHU_HIRA	0
#define JISHU_ZEN_KATA	1
#define JISHU_HAN_KATA	2
#define JISHU_ZEN_ALPHA	3
#define JISHU_HAN_ALPHA	4
#define MAX_JISHU	5

#define  SENTOU        1
#define  HENKANSUMI    2
#define  SUPKEY        4
#define  WARIKOMIMOJI  8
#define  SHIRIKIRE    16
#define  ATAMAKIRE    32

typedef char *mode_context;

struct callback {
  int (*func[NCALLBACK])();
  mode_context    env;
  struct callback *next;
};

/* identifier for each context structures */
#define CORE_CONTEXT       (char)0
#define YOMI_CONTEXT       (char)1
#define JISHU_CONTEXT      (char)2
#define HENKAN_CONTEXT     (char)3
#define ICHIRAN_CONTEXT    (char)4
#define FORICHIRAN_CONTEXT (char)5
#define MOUNT_CONTEXT      (char)6
#define TOUROKU_CONTEXT    (char)7
#define ADJUST_CONTEXT     (char)8
#define CHIKUJI_CONTEXT    (char)9

typedef struct _coreContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode; /* １つ前のモード */
  mode_context    next;
} coreContextRec, *coreContext;

typedef struct  _yomiContextRec {
  /* core 情報と同じ情報 */
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  /* ローマ字かな変換関係 */
  struct RkwRxDic *romdic;	/* ローマ字かな変換辞書 */
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
  KanjiMode       myEmptyMode;		/* empty モードはどれか */
  long		  generalFlags;		/* see below */
  char		  allowedChars;		/* see jrkanji.h */
  char		  henkanInhibition;	/* see below */
  int		  (*henkanCallback)();	/* 変換を行う時にこの変数にアドレス
             が設定されているならばそのアドレスの関数を通常の関数の代わりに
             呼ぶ。設定されていない時は通常の関数を呼ぶ。
               部首変換などで変換キーが押された時に特殊な処理が行われるのに
             対応するために付けた。
	       変換コールバックは引数としてどの変換関数が呼ばれたのかを示す
	     番号を伴う。*/
  int             cursup;		/* ロかなの補追の時に使う */
#define SUSPCHARBIAS 100
  int             n_susp_chars;

  /* 作業用変数 */
  wchar_t	  *retbuf, *retbufp;
  int		  retbufsize;
} yomiContextRec, *yomiContext;

/* for generalFlags */
#define CANNA_YOMI_BREAK_ROMAN		0x1L
#define CANNA_YOMI_CHGMODE_INHIBITTED	0x2L
#define CANNA_YOMI_END_IF_KAKUTEI	0x4L
#define CANNA_YOMI_DELETE_DONT_QUIT	0x8L

#define CANNA_YOMI_KAKUTEI		0x10L
#define CANNA_YOMI_HENKAN		0x20L
#define CANNA_YOMI_ZENKAKU		0x40L
#define CANNA_YOMI_HANKAKU		0x80L
#define CANNA_YOMI_HIRAGANA		0x100L
#define CANNA_YOMI_KATAKANA		0x200L
#define CANNA_YOMI_ROMAJI		0x400L
#define CANNA_YOMI_ATTRFUNCS		0x7f0L

#define CANNA_YOMI_BASE_HANKAKU		0x800L

#define CANNA_YOMI_IGNORE_USERSYMBOLS	0x1000L

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

typedef struct _henkanContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  /* カナ漢字変換関係 */
  int            context;
  int            check;
  int		 kouhoCount;	/* 何回 henkanNext が連続して押されたか */
  wchar_t        yomi_buffer[ROMEBUFSIZE];
  wchar_t        echo_buffer[ROMEBUFSIZE];
  wchar_t        **allkouho; /* RkGetKanjiListで得られる文字列を配列にして
				とっておくところ */
  int            yomilen;    /* 読みの長さ、読み自身は kana_buffer に入れ
			        られる */
  int            curbun;     /* カレント文節 */
  int		 curIkouho;  /* カレント候補 */
  int            nbunsetsu;  /* 文節の数 */
#define MAXNBUNSETSU	256
  int            kugiri[MAXNBUNSETSU]; /* 文節分けを行う時の文節くぎ
					  りの情報。 */
  int		 *kanaKugiri, *romajiKugiri, *jishubun;
  yomiContext    ycx;
/* ifdef MEASURE_TIME */
  long		 proctime;   /* 処理時間(変換で計測する) */
  long		 rktime;     /* 処理時間(RKにかかる時間) */
/* endif MEASURE_TIME */
} henkanContextRec, *henkanContext;

typedef struct _jishuContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* 前のモード */
  mode_context    next;

  unsigned char	 inhibition;
  wchar_t        jishu_buffer[ROMEBUFSIZE];
  int            jishu_len, jishu_kEndp, jishu_rEndp;
  unsigned char  kc, jishu_case;
  yomiContext    ycx;
  henkanContext    hcx;
} jishuContextRec, *jishuContext;

typedef struct _ichiranContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  int            svIkouho;   /* カレント候補を一時とっておく(一覧表示行) */
  int            *curIkouho; /* カレント候補 */
  int            nIkouho;    /* 候補の数(一覧表示行) */
  int		 tooSmall;   /* カラム数が狭くて候補一覧が出せないよフラグ */
  int            curIchar;   /* 未確定文字列ありの単語登録の単語入力の
    							先頭文字の位置 */
  unsigned char  inhibit;
  unsigned char  flags;	     /* 下を見てね */
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


typedef struct _foirchiranContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  int            curIkouho;  /* カレント候補 */
  wchar_t        **allkouho; /* RkGetKanjiListで得られる文字列を配列にして
				とっておくところ */
  menuitem       *table;  /* 文字列と関数のテーブル */
  int            *prevcurp;  /* 前のカレント候補 */
  int            (*prevfunc)();  /* 前の関数 */
  int            (*curfunc)();  /* カレント関数 */
} forichiranContextRec, *forichiranContext;

typedef struct _mountContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  unsigned char        *mountOldStatus; /* マウントされているかいないか */
  unsigned char        *mountNewStatus; /* マウントされているかいないか */
  unsigned char        **mountList;   /* マウント可能な辞書の一覧 */
  int            curIkouho;     /* カレント候補 */
} mountContextRec, *mountContext;

typedef struct _tourokuContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  wchar_t        genbuf[ROMEBUFSIZE];
  wchar_t        qbuf[ROMEBUFSIZE];
  wchar_t        tango_buffer[ROMEBUFSIZE];
  int            tango_len;  /* 単語登録の単語の文字列の長さ */
  wchar_t        yomi_buffer[ROMEBUFSIZE];
  int            yomi_len;   /* 単語登録の読みの文字列の長さ */
  int            curHinshi;  /* 品詞の選択 */
  wchar_t	 hcode[16];   /* 単語登録の品詞 */
  int		 katsuyou;   /* 単語登録の動詞の活用形 */
  wchar_t        **udic;     /* 単語登録できる辞書 (辞書名) */
  int            delContext; /* 単語削除で１つの辞書をマウントする */
} tourokuContextRec, *tourokuContext;

typedef struct _adjustContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;	/* １つ前のモード */
  mode_context    next;

  yomiContext    ycx;
  henkanContext  hcx;

  wchar_t        genbuf[ROMEBUFSIZE];
  int            kanjilen, strlen, bunlen; /* 漢字部分、全体、文節の長さ */
} adjustContextRec, *adjustContext;

typedef struct _chikujiContextRec {
  char id;
  char majorMode, minorMode;
  struct callback c;
  KanjiMode	  prevMode;
  mode_context    next;

  yomiContext    yc;
  henkanContext  hc;
  wchar_t        echo_buffer[ROMEBUFSIZE];
  int		 ye, ys;
  int		 status;
} chikujiContextRec, *chikujiContext;

#define	CHIKUJI_ON_BUNSETSU		0x0001
#define	CHIKUJI_YOMI_CHANGED		0x0002
#define	CHIKUJI_RENBUNSETSU		0x0004
#define	CHIKUJI_DOING_HENKAN		0x0008
#define	CHIKUJI_OVERWRAP		0x0010
#define	CHIKUJI_HENKAN_INHIBIT		0x0020
#define CHIKUJI_HENKAN_FAILED		0x0040
#define	CHIKUJI_NULL_STATUS		0x0000

struct moreTodo {
  char          todo; /* もっとあるの？を示す */
  char          fnum; /* 関数番号。０なら次の文字で示されることをする */
  unsigned char ch;   /* 文字 */
};

/* モード名を格納するデータの型定義 */

struct ModeNameRecs {
  int           alloc;
  wchar_t       *name;
};


/*
 * ローマ字かな変換テーブルは１個あれば良いでしょう。複数個必要なので
 * あれば RomeStruct のメンバとして入れておく必要もありましょうが...そ
 * の時はその時で考えましょう。
 */
     
extern struct RkwRxDic *romajidic, *RkwOpenRoma();

/*
 * 辞書の名前を入れておく変数
 */

#define MAX_DICS 16

extern char *kanjidicname[];
extern int  nkanjidics;

extern char *userdicname[];
extern int  nuserdics;
extern char userdicstatus[];

extern char *bushudicname[];
extern int nbushudics;

extern char *localdicname[];
extern int nlocaldics;

/*
 * エラーのメッセージを入れておく変数
 */

extern char *necKanjiError;

/*
 * デバグ文を表示するかどうかのフラグ
 */

extern iroha_debug;

/*
 * 16進コード入力を一覧行に表示するかどうかを調べる条件。
 */

#define hexGLine(plen) (d->ncolumns /2 >= plen + 4)

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
extern int fail_malloc;
#define malloc(n) debug_malloc(n)
#endif /* DEBUG_MALLOC */

#ifndef	_UTIL_FUNCTIONS_DEF_

#define	_UTIL_FUNCTIONS_DEF_

extern makeGLineMessage();
extern makeGLineMessageFromStrings();
extern setWStrings();
extern WStrlen();
extern wchar_t *WStrcat();
extern wchar_t *WStrcpy();
extern wchar_t *WStrncpy();
extern WStrncmp();
extern MBstowcs();
extern wchar_t *WString();

#endif	/* _UTIL_FUNCTIONS_DEF_ */

/*
 * 新しいモードを定義する構造体
 */

typedef struct {
  wchar_t         *display_name; /* モード表示名 */
  unsigned char   *romaji_table; /* ローマ字かな変換テーブル(EUC) */
  struct RkwRxDic *romdic;	 /* ローマ字辞書構造体 */
  long            romdic_owner;  /* ローマ字辞書を自分でOpenしたか */
  long            flags;	 /* flags for yomiContext->generalFlags */
  KanjiMode       emode;	 /* current_mode に入る構造体 */
} newmode;

#define MAX_OTHER_MODES 16

/* ローマ字かな変換を補足するキーと文字の変換テーブル */

typedef struct {
  wchar_t	key;		/* キー */
  int		groupid;	/* グループid */
  int           ncand;          /* 候補の数 */
  wchar_t       **cand;         /* 候補の配列 */
  wchar_t	*fullword;	/* 候補列 (候補1@候補2@...候補n@@) */
} keySupplement;

#define MAX_KEY_SUP 64

#ifndef	DEBUG_CHIKUJI
#define	debugging(a)
#define debuggingf(a, b)
#define debuggingw(a, b, c)
#endif	/* DEBUG_CHIKUJI */

#define HEX_USUAL     0
#define HEX_IMMEDIATE 1

#define CANNA_HALF_VERBOSE 1

#endif /* !_CANNA_H_ */
