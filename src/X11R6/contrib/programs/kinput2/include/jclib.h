/* $Id: jclib.h,v 5.0 1991/10/01 07:58:04 ishisone Rel $ */

/*
 *	jclib.h -- jclib 用ヘッダファイル (Wnn Version4 対応版)
 *		version 5.0
 *		ishisone@sra.co.jp
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _jclib_h
#define _jclib_h

#include	"jslib.h"

#ifndef WCHAR_DEFINED
#define WCHAR_DEFINED
#undef wchar
typedef unsigned short wchar;
#endif


/* 定数 */
#define JC_FORWARD	1
#define JC_BACKWARD	0
#define JC_NEXT		0
#define JC_PREV		1
#define JC_HIRAGANA	0
#define JC_KATAKANA	1

/* エラー番号 */
#define JE_NOERROR		0
#define JE_WNNERROR		1	/* jlib のエラー */
#define JE_NOCORE		2	/* メモリが確保できない */
#define JE_NOTCONVERTED		3	/* 対象文節がまだ変換されていない */
#define JE_CANTDELETE		4	/* バッファの先頭の前、あるいは
					 * 最後の次の文字を削除しようとした */
#define JE_NOSUCHCLAUSE		5	/* 指定された番号の文節が存在しない */
#define JE_CANTSHRINK		6	/* 1 文字の文節を縮めようとした */
#define JE_CANTEXPAND		7	/* 最後の文節を伸ばそうとした */
#define JE_NOCANDIDATE		8	/* 次候補がない */
#define JE_NOSUCHCANDIDATE	9	/* 指定された番号の候補が存在しない */
#define JE_CANTMOVE		10	/* バッファの先頭の前、あるいは
					 * 最後の次に移動しようとした */
#define JE_CLAUSEEMPTY		11	/* 空の文を変換しようとした */
#define JE_ALREADYFIXED		12	/* すでに確定されている文に対して
					 * 操作を行なった */

/* エラー番号 */
extern int	jcErrno;	/* エラー番号 */

/* データタイプ */
typedef struct {
	wchar	*kanap;		/* 読み文字列 */
	wchar	*fzkp;		/* 付属語の読み文字列 */
	wchar	*dispp;		/* 表示文字列 */
	int	dicno;
	int	entry;
	int	hinshi;
	int	kangovect;
	char	conv;		/* 変換済みか */
				/* 0: 未変換 1: 変換済 -1: jclibで疑似変換 */
	char	ltop;		/* 大文節の先頭か? */
	char	imabit;		/* 今使ったよビット */
} jcClause;

typedef struct {
    /* public member */
	int		nClause;	/* 文節数 */
	int		curClause;	/* カレント文節番号 */
	int		curLCStart;	/* カレント大文節開始文節番号 */
	int		curLCEnd;	/* カレント大文節終了文節番号 */
	wchar		*kanaBuf;	/* かなバッファ */
	wchar		*kanaEnd;
	wchar		*displayBuf;	/* ディスプレイバッファ */
	wchar		*displayEnd;
	jcClause	*clauseInfo;	/* 文節情報 */
	struct wnn_env	*env;
    /* private member */
	int		fixed;		/* 確定されたかどうか */
	wchar		*dot;		/* ドットの位置 */
	int		candKind;	/* 大文節の候補か小文節の候補かを表すフラグ */
	int		candClause;	/* 次候補の文節番号 */
	int		candClauseEnd;	/* 大文節の次候補の時、終了文節番号 */
	int		nCand;		/* 次候補の数 */
	int		curCand;	/* カレントの次候補番号 */
	char		*candBuf;	/* opaque */
	int		nReset;		/* 今使ったよビットを落すべき
					 * エントリの数 */
	char		*resetBuf;	/* opaque */
	int		bufferSize;	/* kanaBuf/displayBuf の大きさ */
	int		clauseSize;	/* clauseInfo の大きさ */
	int		candSize;	/* candBuf の大きさ */
	int		resetSize;	/* resetBuf の大きさ */
} jcConvBuf;

#ifdef __STDC__
extern jcConvBuf *jcCreateBuffer(struct wnn_env *env,
				 int nclause, int buffersize);
extern int jcDestroyBuffer(jcConvBuf *buf, int savedic);
extern int jcClear(jcConvBuf *buf);
extern int jcInsertChar(jcConvBuf *buf, int c);
extern int jcDeleteChar(jcConvBuf *buf, int prev);
extern int jcConvert(jcConvBuf *buf, int small, int tan, int jump);
extern int jcUnconvert(jcConvBuf *buf);
extern int jcExpand(jcConvBuf *buf, int small, int convf);
extern int jcShrink(jcConvBuf *buf, int small, int convf);
extern int jcKana(jcConvBuf *buf, int small, int kind);
extern int jcFix(jcConvBuf *buf);
extern int jcNext(jcConvBuf *buf, int small, int prev);
extern int jcCandidateInfo(jcConvBuf *buf, int small,
			   int *ncandp, int *curcandp);
extern int jcGetCandidate(jcConvBuf *buf, int n, wchar *candstr);
extern int jcSelect(jcConvBuf *buf, int n);
extern int jcDotOffset(jcConvBuf *buf);
extern int jcIsConverted(jcConvBuf *buf, int cl);
extern int jcMove(jcConvBuf *buf, int small, int dir);
extern int jcTop(jcConvBuf *buf);
extern int jcBottom(jcConvBuf *buf);
extern int jcChangeClause(jcConvBuf *buf, wchar *str);
extern int jcSaveDic(jcConvBuf *buf);
#else
extern jcConvBuf *jcCreateBuffer();
extern int jcDestroyBuffer();
extern int jcClear();
extern int jcInsertChar();
extern int jcDeleteChar();
extern int jcConvert();
extern int jcUnconvert();
extern int jcExpand();
extern int jcShrink();
extern int jcKana();
extern int jcFix();
extern int jcNext();
extern int jcCandidateInfo();
extern int jcGetCandidate();
extern int jcSelect();
extern int jcDotOffset();
extern int jcIsConverted();
extern int jcMove();
extern int jcTop();
extern int jcBottom();
extern int jcChangeClause();
extern int jcSaveDic();
#endif

#endif /* _jclib_h */
