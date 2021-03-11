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
 * RkWnn.c
 *	RK の連文節変換ライブラリに対応するもの
 *
 *	1991.9: y-morika
 */
#ifndef lint
static char rcsid[]="$Id: RKwnn.c,v 1.25 1994/01/17 05:17:04 kon Exp $";
#endif

/* WStr.h で wchar を定義しているが wchar_t とは違うので注意! */

#include	<canna/widedef.h>

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#include	<canna/RK.h>

#ifdef HAVE_WCHAR_OPERATION
#include <locale.h>
#endif

#include	<stdio.h>
#include	<signal.h>

#include	"jilib.h"
#include	"jclib.h"

#include	"WStr.h"

#ifndef RK_DEFAULT_JSERVER
#define RK_DEFAULT_JSERVER	"unix"
#endif

#define ISSK1(c) \
    (((c) >= 0x81 && (c) <= 0x9f) || ((c) >= 0xe0 && (c) <= 0xfc))

#define CKCONV(cn)  if (!ccx[(cn)].converting) return -1
#define CKCN(cn) if (cn < 0 || MAXCONTEXT <= cn || !ccx[(cn)].occupied) \
  return -1

WNN_JSERVER_ID	*server;
WNN_ENV		*wnn_env;

#define MAXCONTEXT 512

typedef struct _CannaContext {
  int           occupied;	/* このコンテクストが使われている */
  jcConvBuf	*buf;
  int		converting;	/* 変換中 */
  int		tan;		/* 連文節変換指定 */
  int		small;		/* small 0 大文節単位で移動 */
} CannaContext;			/* それ以外 小文節単位で移動 */

static CannaContext ccx[MAXCONTEXT];

static void
initCannaContexts()
{
  int i;

  for (i = 0 ; i < MAXCONTEXT ; i++) {
    ccx[i].occupied = 0;
    ccx[i].buf = (jcConvBuf *)0;
  }
}

static int wchar_type;

#define CANNA_WCTYPE_16 0  /* 16ビット表現 */
#define CANNA_WCTYPE_32 1  /* 32ビット表現 */
#define CANNA_WCTYPE_OT 99 /* その他の表現 */

/*
 WCinit() -- ワイドキャラクタとしてどれが使われているかを確認する

        この関数が呼び出されるまえに setlocale がなされていなければならない
 */

#define TYPE16A 0x0000a4a2
#define TYPE32A 0x30001222

int
WCinit()
{
  unsigned char *a = (unsigned char *)"あ"; /* 0xa4a2 */
  wchar_t wc[24];

#ifdef HAVE_WCHAR_OPERATION
  if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
    /* 多分 setlocale がなされていない */
    setlocale(LC_CTYPE, "");
    if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
      setlocale(LC_CTYPE, JAPANESE_LOCALE);
      if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
	return -1;
      }
    }
  }
  switch (wc[0]) {
  case TYPE16A:
    wchar_type = CANNA_WCTYPE_16;
    break;
  case TYPE32A:
    wchar_type = CANNA_WCTYPE_32;
    break;
  default:
    wchar_type = CANNA_WCTYPE_OT;
    break;
  }
#else /* !HAVE_WCHAR_OPERATION */
# ifdef _WCHAR16

  wchar_type = CANNA_WCTYPE_16;

# else /* !_WCHAR16 */

  if (sizeof(wchar_t) == 2) {
    wchar_type = CANNA_WCTYPE_16;
  }
  else {
    wchar_type = CANNA_WCTYPE_32;
  }

# endif /* !_WCHAR16 */
#endif /* !HAVE_WCHAR_OPERATION */

  return 0;
}

static void
WS2US(d, s, n)
wchar *d;
wchar_t *s;
int n;
{
  switch (wchar_type) {
  case CANNA_WCTYPE_16:
    while (n-- > 0) {
      *d++ = (wchar)*s++;
    }
    break;
  case CANNA_WCTYPE_32:
  default:
    while (n-- > 0) {
      wchar_t wc = *s++;
      switch (wc >> 28) {
      case 0:
	/* ASCII */
	*d++ = (wchar)wc;
	break;
      case 1:
	/* 半角カナ */
	*d++ = (wchar)(((unsigned)wc & 0x7f) | 0x0080);
	break;
      case 2:
	/* 外字 */
	*d++ = (wchar)
	  ((((unsigned)wc & 0x3f80) << 1) | ((unsigned)wc & 0x7f) | 0x8000);
	break;
      case 3:
	/* 漢字 */
	*d++ = (wchar)
	  ((((unsigned)wc & 0x3f80) << 1) | ((unsigned)wc & 0x7f) | 0x8080);
	break;
      }
    }
    break;
  }
}

static void
US2WS(d, s, n)
wchar_t *d;
wchar *s;
int n;
{
  switch (wchar_type) {
  case CANNA_WCTYPE_16:
    while (n-- > 0) {
      *d++ = (wchar_t)*s++;
    }
    break;
  case CANNA_WCTYPE_32:
  default:
    while (n-- > 0) {
      wchar uc = *s++;
      switch (uc & 0x8080) {
      case 0x0000:
	/* ASCII */
	*d++ = (wchar_t)uc;
	break;
      case 0x0080:
	/* 半角カナ */
	*d++ = (wchar_t)(((unsigned)uc & 0x7f) | (1 << 28));
	break;
      case 0x8000:
	/* 外字 */
	*d++ = (wchar_t)((((unsigned)uc & 0x7f00) >> 1) |
			 ((unsigned)uc & 0x7f) | (2 << 28));
	break;
      case 0x8080:
	/* 漢字 */
	*d++ = (wchar_t)((((unsigned)uc & 0x7f00) >> 1) |
			 ((unsigned)uc & 0x7f) | (3 << 28));
	break;
      }
    }
    break;
  }
}

static int
USlen(s)
wchar *s;
{
  wchar *e = s;

  while (*e)
    e++;

  return e - s;
}

#define JSERVER_AT "jserver@"

static char jserver_name[256] = JSERVER_AT;

/* 初期化 */
int	RkwInitialize(home)
char	*home; /* 辞書ディレクトリ */
/* ARGSUSED */
{
    extern char *getenv();
    char *p;

    (void)WCinit();
    initCannaContexts();
    if( home && home[0] != '/' ) {
      strcpy(jserver_name + strlen(JSERVER_AT) ,home);
    }
    else if (p = getenv("JSERVER")) {
      strcpy(jserver_name + strlen(JSERVER_AT) ,p);
    }
    else {
      strcpy(jserver_name + strlen(JSERVER_AT) ,RK_DEFAULT_JSERVER);
    }
    server = jiOpenServer(jserver_name + strlen(JSERVER_AT), 30);
    
    if( !server ) {
	return ( -1 );
    }
    wnn_env = jiCreateEnv(server,
			  ""/* envname */, 0/* override */,
			  ""/* wnnrcname */,
			  NULL/* callback */, NULL/* callback */,
			  0/* client data */);

    return RkwCreateContext();
}


/* 終了処理 */
void	RkwFinalize()
{
    int i;

    for (i = 0 ; i < MAXCONTEXT ; i++) {
      RkwCloseContext(i);
    }
    jiCloseServer( server );
}


/* 辞書ファイルのオープン */
/*ARGSUSED*/
int	RkwMountDic(ctx, filename, mode)
int	ctx;
char	*filename;
int	mode;
{
    return( 1 );
}

/* 変換の開始 */
int RkwBgnBun( ctx, yomi, n, mode )
int	ctx;
wchar_t	*yomi;
int	n;
int	mode;
/* ARGSUSED */
{
    jcConvBuf *buf;
    wchar	wChar[1024];

    CKCN(ctx);
    buf = ccx[ctx].buf;
    jcClear(buf);
    
    /* バッファへ文字列を代入する */
    WS2US(wChar, yomi, n);
    wChar[n] = (wchar)0;
    if( jcChangeClause(buf, wChar) == -1 )
	return( -1 );
    
    /* かな漢字変換を行なう */
    if( jcConvert(buf, 0, ccx[ctx].tan, 0) == -1 )
	return( -1 );

    ccx[ctx].converting = 1;
    return( buf->nClause );
}


/* 変換の終了 */
/*ARGSUSED*/  
int RkwEndBun(ctx,  mode)
int	ctx;
int	mode;			/* 学習指定のフラグ */
{
    jcConvBuf *buf;

    CKCN(ctx);
    buf = ccx[ctx].buf;

    jcFix(buf);
/*    jcSaveDic(buf);		/* 学習されている状態である */
    ccx[ctx].converting = 0;
    return 0;
}


/* カレント文節の変更 */
int RkwGoTo(ctx, bnum)
int	ctx;
int	bnum;
{
    jcConvBuf *buf;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    if( 0 <= bnum && bnum < buf->nClause ) {
	while (bnum != buf->curClause)
	    jcMove( buf, ccx[ctx].small, 
		   buf->curClause < bnum ? JC_FORWARD : JC_BACKWARD );
	
	return( buf->curClause );
    }
    else
	return( -1 );
}


/* カンレント文節の左移動 */
int RkwLeft(ctx)
int	ctx;
{
    jcConvBuf *buf;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    if( buf->curClause == 0 )
	jcBottom(buf);

    jcMove( buf, ccx[ctx].small, JC_BACKWARD );
    return( buf->curClause );
}


/* カレント文節の右移動 */
int RkwRight(ctx)
int	ctx;
{
    jcConvBuf *buf;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    if( buf->curClause == buf->nClause - 1 )
	jcTop(buf);
    else
	jcMove( buf, ccx[ctx].small, JC_FORWARD);
    return( buf->curClause );

}


/* カレント文節を次候補で置き換える */
int RkwNext(ctx)
int	ctx;
{
    jcConvBuf *buf;
    int		ncand, curcand;
    
    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    jcCandidateInfo(buf, ccx[ctx].small, &ncand, &curcand);
    if( curcand == ncand - 1 )	curcand = 0;
    else	curcand++;

    jcSelect( buf, curcand );
    return( curcand );
}


/* カレント文節を直前の候補で置き換える */
int RkwPrev(ctx)
int	ctx;
{
    jcConvBuf *buf;
    int		ncand, curcand;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    jcCandidateInfo(buf, ccx[ctx].small, &ncand, &curcand);
    if( 0 == curcand )	curcand = ncand - 1;
    else	curcand--;
    jcSelect(buf, curcand);
    
    return( curcand );
}


/* 漢字候補の列挙 */
int	RkwGetKanjiList(ctx, kouho, bufsize)
int	ctx;
wchar_t	*kouho;
int     bufsize;
{
    jcConvBuf *buf;
    int		ncand, curcand;
    int		j, i;
    wchar_t 	wChar[1024];
    int		len;
    
    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    jcCandidateInfo(buf, ccx[ctx].small, &ncand, &curcand);
    for ( i = 0, j = 0; j < ncand; j++ ) {	/* 文節の表示 */
	jcGetCandidate(buf, j, (wchar *)wChar);
	
	if ((len = USlen(wChar)) < bufsize - i) {
	  US2WS(kouho + i, wChar, len);
	  kouho[i + len] = (wchar_t)0;
	}
	i += len + 1;
    }
/*    jcSelect( buf, curcand ); */
    return( ncand );
}


/* 指定された候補番号をカレント候補にする */
int RkwXfer(ctx, knum)
int	ctx;
int knum;
{
    jcConvBuf *buf;
    int		ncand, curcand;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    jcCandidateInfo(buf, ccx[ctx].small, &ncand, &curcand);
    if( 0 <= knum && knum < ncand ) {
	jcSelect( buf, knum );
    }
    else 
	return( -1 );

    return( knum );
}


/* カレント文節の読みがなを長さ len にする */
int RkwResize(ctx, len)
int	ctx;
int	len;		/* len は、EUCコードでのバイト数で与える */
{
    int		len_kanap;
    int		n;
    jcConvBuf   *buf;

    CKCN(ctx);
    buf = ccx[ctx].buf;
    len_kanap = buf->clauseInfo[buf->curClause + 1].kanap
      - buf->clauseInfo[buf->curClause].kanap;
    
    if( len < len_kanap ) {
	n = len_kanap - len;
	while( n-- )
	    RkwShorten(ctx);
    }
    else if( len > len_kanap ) {
	n = len - len_kanap;
	while( n-- )
	    RkwEnlarge(ctx);
    }
    
    return( buf->nClause );
}


/* カレント文節の長さを縮める */
int RkwShorten(ctx)
int	ctx;
{
    CKCN(ctx);
    CKCONV(ctx);
    jcShrink(ccx[ctx].buf, ccx[ctx].small, 1);
    return( ccx[ctx].buf->nClause );
}


/* カレント文節の長さを伸ばす */
int RkwEnlarge(ctx)
int	ctx;
{
    CKCN(ctx);
    CKCONV(ctx);
    jcExpand(ccx[ctx].buf, ccx[ctx].small, 1);
    return( ccx[ctx].buf->nClause );
}


/* カレント文節を無変換の状態に戻す */
int RkwNfer(ctx)
int	ctx;
{
    int		ncand, curcand;
    
    CKCN(ctx);
    CKCONV(ctx);
/*    if( jcUnconvert(ccx[ctx].buf) == -1 ) */
    if (jcKana(ccx[ctx].buf, ccx[ctx].small, JC_HIRAGANA) == -1)
	return( -1 );

    jcCandidateInfo( ccx[ctx].buf, ccx[ctx].small, &ncand, &curcand );
    return( curcand );
}



/* カレント文節に対応する読みがなを領域yomiに設定し、そのアドレスを返す	*/
int	RkwGetYomi(ctx, yomi, MAX)
int	ctx;
wchar_t	*yomi;
int	MAX;
{
    jcConvBuf *buf;
    int n;
    int len, retval;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    n = buf->curClause;
    len = buf->clauseInfo[n+1].kanap - buf->clauseInfo[n].kanap;
    retval = MAX < len ? MAX : len;
    US2WS(yomi, buf->clauseInfo[n].kanap, retval);
    if (len < MAX) {
      yomi[len] = (wchar_t)0;
    }
    else {
      yomi[MAX - 1] = (wchar_t)0;
    }

    return len;
}


/* カレント文節に対応する漢字候補を領域kanjiに設定し、そのアドレスを返す */
int	RkwGetKanji(ctx, kanji, MAX)
int	ctx;
wchar_t	*kanji;
int	MAX;
{
    jcConvBuf *buf;
    int n;
    int len, retval;

    CKCN(ctx);
    CKCONV(ctx);
    buf = ccx[ctx].buf;
    n = buf->curClause;
    len = buf->clauseInfo[n+1].dispp - buf->clauseInfo[n].dispp;
    retval = (MAX < len) ? MAX : len;
    US2WS(kanji, buf->clauseInfo[n].dispp, retval);
    if (len < MAX) {
      kanji[len] = (wchar_t)0;
    }
    else {
      kanji[MAX - 1] = (wchar_t)0;
    }

    return len;
}


RkwCreateContext()
{
  int i;

  for (i = 0 ; i < MAXCONTEXT && ccx[i].occupied ; i++)
    ;
  if (i < MAXCONTEXT) {
    ccx[i].occupied = 1;
    ccx[i].small = 1;
    ccx[i].converting = 0;
    ccx[i].tan = 0;
    ccx[i].buf = jcCreateBuffer(wnn_env, 0, 0);	/* 変換バッファの作成 */
    return i;
  }
  else {
    return -1;
  }
}

RkwDuplicateContext(cn)
int cn;
/* ARGSUSED */
{
  return RkwCreateContext();
}

RkwCloseContext(cn)
int cn;
{
  CKCN(cn);
  if (ccx[cn].occupied) {
    jcDestroyBuffer(ccx[cn].buf, 1);
    ccx[cn].buf = (jcConvBuf *)0;
    ccx[cn].occupied = 0;
    return 0;
  }
  else {
    return -1;
  }
}

RkwGetStat(cn, stat)
int cn;
RkStat *stat;
{
  jcConvBuf *buf;

  CKCN(cn);
  CKCONV(cn);
  buf = ccx[cn].buf;
  stat->bunnum = buf->curClause;
  stat->candnum = buf->curCand;
  stat->maxcand = buf->nCand;
  stat->diccand = buf->nCand;
  stat->ylen = buf->clauseInfo[buf->curClause + 1].kanap - 
    buf->clauseInfo[buf->curClause].kanap;
  stat->klen = buf->clauseInfo[buf->curClause + 1].dispp -
    buf->clauseInfo[buf->curClause].dispp;
  stat->tlen = 1;/* 決め打ち */
  return 0;
}

/*
 * 一文節一単語という大胆な仮定をしている。これは将来的には直したい。
 */

RkwGetLex(cn, lex, maxlex)
int cn, maxlex;
RkLex *lex;
{
  RkStat stat;

  if (RkwGetStat(cn, &stat) < 0) return -1;

  if (maxlex < 1) return 0;

  lex->ylen = stat.ylen;
  lex->klen = stat.klen;
  lex->rownum = 0;
  lex->colnum = 0;
  return 1;
}

/* ここから下は空っぽです */
RkwSetDicPath(cn, path)
int cn;
char *path;
/* ARGSUSED */
{
    return 0;
}

RkwCreateDic(cn, dicname, mode)
int cn, mode;
unsigned char *dicname;
/* ARGSUSED */
{
    return -1;
}

RkwSync(cn, dicname, mode)
int cn, mode;
unsigned char *dicname;
/* ARGSUSED */
{
    return -1;
}

RkwGetDicList(cn, dicnames_return, MAX)
int cn;
char *dicnames_return;
int MAX;
/* ARGSUSED */
{
  return 0;
}

RkwGetMountList(cn, dicnames_return, MAX)
int cn;
char *dicnames_return;
int MAX;
/* ARGSUSED */
{
  return 0;
}

RkwUnmountDic(cn, dicname)
int cn;
unsigned char *dicname;
/* ARGSUSED */
{
  return 0;
}

RkwRemountDic(cn, dicname, where)
int cn, where;
unsigned char *dicname;
/* ARGSUSED */
{
  return 0;
}

RkwStoreYomi(cn, yomi, MAX) /* これはいるんでないの。*/
int cn, MAX;
char *yomi;
/* ARGSUSED */
{
  return -1;
}

RkwDefineDic(cn, dicname, wordrec)
int cn;
unsigned char *dicname, *wordrec;
/* ARGSUSED */
{
  return -1;
}

RkwDeleteDic(cn, dicname, wordrec)
int cn;
unsigned char *dicname, *wordrec;
/* ARGSUSED */
{
  return -1;
}

RkGetProtocolVersion(ma, mi)
int *ma, *mi;
{
  if (ma) {
    *ma = 1;
  }
  if (mi) {
    *mi = 1;
  }
  return 0;
}

RkGetServerVersion(ma, mi)
int *ma, *mi;
{
  if (ma) {
    *ma = 1;
  }
  if (mi) {
    *mi = 1;
  }
  return 0;
}

char *
RkGetServerName()
{
  return jserver_name;
}

RkwGetHinshi()
{
  return -1;
}

RkwSubstYomi()
{
  return -1;
}

RkwFlushYomi()
{
  return -1;
}

RkwRemoveBun()
{
  return -1;
}

RkwGetLastYomi()
{
  return -1;
}

/*
 *  RkwSetAppName ()
 *
 *  Description:
 *  -----------
 *  アプリケーション名の登録
 *
 *  Input:
 *  -----
 *  apname: アプリケーション名
 *
 *  Returns:
 *  -------
 *  0 or -1
 */
int
RkwSetAppName( cxnum, apname )					/* S003 */
int cxnum;
unsigned char *apname;
{
    return( -1 ) ;
}
							/* S003:begin */
int
RkSetAppName( cxnum, apname )
int cxnum;
unsigned char *apname;
{
    return( RkwSetAppName( cxnum, apname ) );
}							/* S003:end */
							/* S000:begin */


#ifdef ENGINE_SWITCH
struct rkfuncs RkFuncs = {
  RkGetProtocolVersion,
  RkGetServerName,
  RkGetServerVersion,
  RkwInitialize,
  RkwFinalize,
  RkwCreateContext,
  RkwDuplicateContext,
  RkwCloseContext,
  RkwSetDicPath,
  RkwCreateDic,
  RkwSync,
  RkwGetDicList,
  RkwGetMountList,
  RkwMountDic,
  RkwRemountDic,
  RkwUnmountDic,
  RkwDefineDic,
  RkwDeleteDic,
  RkwGetHinshi,
  RkwGetKanji,
  RkwGetYomi,
  RkwGetLex,
  RkwGetStat,
  RkwGetKanjiList,
  RkwFlushYomi,
  RkwGetLastYomi,
  RkwRemoveBun,
  RkwSubstYomi,
  RkwBgnBun,
  RkwEndBun,
  RkwGoTo,
  RkwLeft,
  RkwRight,
  RkwNext,
  RkwPrev,
  RkwNfer,
  RkwXfer,
  RkwResize,
  RkwEnlarge,
  RkwShorten,
  RkwStoreYomi,
  RkwSetAppName,
};
#endif /* ENGINE_SWITCH */
