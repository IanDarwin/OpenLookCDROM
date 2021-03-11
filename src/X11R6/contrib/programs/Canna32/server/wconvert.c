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
static char rcs_id[] = "@(#) 102.1 $Id: wconvert.c,v 6.42 1994/04/17 12:39:58 kon Exp $";
#endif

#define EXTPROTO 1

#define SIZEOFCHAR 1
#define SIZEOFSHORT 2
#define SIZEOFINT 4
#define SIZEOFLONG 4

#define HEADER_SIZE (SIZEOFCHAR * 2 + SIZEOFSHORT)

#define DEFAULTBUFSIZE 512

/*
 * MODIFICATION HISTORY
 *	S000	funahasi@oa2	Fri Oct  2 20:14:13 JST 1992
 *		- cannastatで取得できる client情報数の制限を解除
 *		- debug用関数 RkThrough()のために protocolを追加
 *		- fixed bug cannastatで AutoConvert以降の protocol使用回数が
 *		  countされていなかったのを直した
 *	S001	funahasi@oa2	Thu Oct  8 20:24:13 JST 1992
 *		- fixed bug cannastatで user_nameと host_nameが正しく表示され
 *		  なかったのを修正
 *	S002	funahasi@oa2	Tue Oct 13 15:29:20 JST 1992
 *		- protocolversion2.0以前の clientでは RkQueryDic()を呼ばない
 *		  ように仕様を変更
 *	S003	funahasi@oa2	Thu Nov  5 15:34:25 JST 1992
 *		- RkQueryDic()の引き数に user名を指定できるよう仕様を変更
 *		- RkQueryDic()の返す DicInfoの formatが変わったので合わせる
 *	S004	funahasi@oa2	Tue Nov 10 17:02:20 JST 1992
 *		- fixed bug 前回の RkQueryDic()の修正で漏れがあった
 *	S005	funahasi@oa2	Thu Dec  3 23:51:26 JST 1992
 *		- fixed bug S000で client情報数の制限を解除した時に入れた
 *		  sendbufferの残りを調べる処理が間違っていた
 *		  先にclient情報全体のサイズを調べておいて sendbufferを越える
 *		  なら sendbufferを mallocし直すように修正
 *	S006	funahasi@oa2	Fri Dec  4 19:31:12 JST 1992
 *		- fixed bug protocol ver2.0の時 client情報のサイズを調べる
 *		  のが抜けていたので修正した
 *		  sendbufferの残りを調べる処理が間違っていた
 *		- fixed bug sendbufferが protcolが扱える最大長を越える時が
 *		  あるので越えた分は切り捨てるようにする
 *	S007	funahasi@oa2	Tue Jan 12 19:49:29 JST 1993
 *		- fixed bug irw_dictionary_list()で型が合わないデータを
 *		  出力していた
 *	S008	funahasi@oa2	Fri Jan 29 17:22:04 JST 1993
 *		- gcc -Oで候補一覧を取るとコアダンプ．アライメントの問題
 */

/* LINTLIBRARY */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>

#include "net.h"
#include "IR.h"

extern int  errno;

typedef struct {
#if __STDC__
  int (*func)(ClientPtr *), (*extdat)(BYTE *);
#else
  int (*func)(), (*extdat)();
#endif
} reqproc;

extern char *WideProtoName[], *ExtensionRequest[];
extern reqproc WideVector[];
#ifdef EXTENSION
extern reqproc ExtensionWideVector[];
#endif

extern void DispDebug() ;
extern int canna_server_hi ;
extern int canna_server_lo ;
#ifdef DEBUG
#ifdef pcux_r32
static struct DicInfo * RkwQueryDic();
#endif /* pcux_r32 */
static char * conveuc();
static char null[] = "NULL";
#endif /* DEBUG */
static IRwReq	Request ;
static BYTE local_buffer[ LOCAL_BUFSIZE ];

static unsigned int
TotalWideRequestTypeCount[ W_MAXREQUESTNO ] ;

#ifdef DEBUGPROTO
static void
printproto(p, n)
char *p;
int n;
{
  int i;

  for (i = 0 ; i < n ; i++) {
    if (i) {
      if ((i %  4) == 0) printf(" ");
      if ((i % 32) == 0) printf("\n");
    }
    printf("%02x", (unsigned)((*p++) & 0xff));
  }
  printf("\n");
}

static void
probe(format, n, p)
char *format, *p;
int n;
{
  printf(format, n);
  printproto(p, n);
}
#else /* !DEBUGPROTO */
#define probe(a, b, c)
#endif /* !DEBUGPROTO */

/* VARARGS */
int PrintMsg(), Dmsg();

static
GetFirstKouho(cxnum, start, end, val_return, buf, bufsize, bufp_return)
int cxnum, start, end, *val_return, bufsize;
Ushort *buf, **bufp_return;
{
  int rest = bufsize, len, i, j;
  Ushort kanjibuf[DEFAULTBUFSIZE/*RK_DENO_SAIDAI_CHOU*/], *p = buf;

  *bufp_return = buf;
  for (i = start ; i < end ; i++) {
    len = RkwGetKanji(cxnum, kanjibuf, sizeof(kanjibuf) / sizeof(Ushort));
    if (rest <= len) {
      /* allocate する */
      int totallen = (p - buf) + len + 1;
      Ushort *new;

      for (j = i + 1 ; j < end ; j++) {
        RkwRight(cxnum);
        len = RkwGetKanji(cxnum, kanjibuf, sizeof(kanjibuf) / sizeof(Ushort));
        totallen += len + 1;
      }
      if (!(new = (Ushort *)malloc(totallen * sizeof(Ushort)))) {
        /* さすがにもうダメ */
        PrintMsg( "Memory exhausted!\n" );
        break; /* 実際の文節数より少ない値が返る */
      }
      bcopy(buf, new, (p - buf) * sizeof(Ushort));
      p += new - buf;
      *bufp_return = new;
      rest += totallen - bufsize;
      RkwGoTo(cxnum, i);
      len = RkwGetKanji(cxnum, kanjibuf, sizeof(kanjibuf) / sizeof(Ushort));
    }
    bcopy((char *)kanjibuf, (char *)p, len * sizeof(Ushort));
    p += len;
    *p++ = (Ushort)0;
    rest -= len + 1;
    RkwRight(cxnum);
    ir_debug( Dmsg(5, "%s ", (len > 0) ? conveuc(p - len - 1) : null) );
  }
  ir_debug(Dmsg(5, "\n"));
  *p++ = (Ushort)0;
  *val_return = i;
  RkwGoTo(cxnum, start); /* 先頭文節をカレント文節に戻しておく */
  return p - *bufp_return;
}

static int WriteClient();

static BYTE *
copylenstr(name, p)
char *name;
BYTE *p;
{
  int len, filledlen;

  len = strlen(name) + 1;
  filledlen = len + (len % 2);

  STOS2(filledlen, p); p += SIZEOFSHORT;
  strcpy((char *)p, name); p += len;
  if (filledlen > len) *p++ = (BYTE)0;
  return p;
}


/*
  Type1 のリプライは GetServerInfo でしか使われないためちょっと中身を
  知っているような処理(who-> の部分)にしてしまった。(^^;)
 */

static
SendType1Reply(client, majo, mino, stat, majorv, minorv, curtime,
	       nproto, protonames, protofreqs, nclients, ncontexts, who)
ClientPtr client, *who;
int majo, mino, stat, majorv, minorv, curtime, nproto, nclients, ncontexts;
unsigned int *protofreqs;
char **protonames;
{
  int namelen, fillednamelen;
  int i, j, len, requiredsize, size, clinfolen, retval;
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  char **cp;

  /* まずリプライするのに必要なバッファの長さを求めよう */
  requiredsize = 
    HEADER_SIZE
  + SIZEOFCHAR            /* 終了状態 */
  + SIZEOFCHAR            /* メジャーバージョン */
  + SIZEOFCHAR            /* マイナーバージョン */
  + SIZEOFLONG            /* 時刻 */
  + SIZEOFSHORT           /* プロトコル数 */
  + SIZEOFSHORT           /* プロトコル名の長さ */
  + 0                     /* プロトコル名(まだわからない) */
  + (nproto * SIZEOFLONG) /* プロトコル使用頻度 */
  + SIZEOFSHORT           /* クライアント数 */
  + SIZEOFSHORT           /* コンテキスト数 */
  + 0                     /* クライアント情報関連(まだわからない) */
  ;
  for (i = 0, namelen = 0, cp = protonames; i < nproto ; i++, cp++) {
    len = strlen(*cp) + 1;
    namelen += len;
  }
  namelen++;
  fillednamelen = namelen + namelen % 2;
  requiredsize += fillednamelen; /* プロトコル名(わかった) */
  clinfolen =
    SIZEOFSHORT           /* クライアント情報の長さ */
  + (5 * SIZEOFLONG)      /* ソケット番号、ユーザ管理番号、各種時間 */
  + (nproto * SIZEOFLONG) /* プロトコル使用頻度 */
  + SIZEOFSHORT           /* ユーザ名の長さ */
  + SIZEOFSHORT           /* ホスト名の長さ */
  + ((client->version_hi > 2) ? SIZEOFSHORT : 0) /* クライアント名の長さ */
  + ncontexts             /* コンテキスト管理フラグ */
  ;
  requiredsize += nclients * clinfolen;/* クライアント情報関連(半分わかった) */

  for (i = 0 ; i < nclients ; i++) {
    int ulen, hlen, clen;

    ulen = strlen(who[i]->username) + 1;
    hlen = strlen(who[i]->hostname) + 1;

    ulen += ulen % 2;
    hlen += hlen % 2;
    if (client->version_hi > 2) {
      if (who[i]->clientname) {
	clen = strlen(who[i]->clientname) + 1;
	clen += clen % 2;
      }
      else {
	clen = 2; /* 0 + 1 + ((0 + 1) % 2)、つまり strlen == 0 時の値 */
      }
    }
    else {
      clen = 0;
    }
    requiredsize += ulen + hlen + clen;
  } /* クライアント情報関連(残りも分かった) */
  /* あ〜、これで全部分かった */

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    stat = -1;
    requiredsize = HEADER_SIZE + SIZEOFCHAR;
  }
  p = bufp;
  size = requiredsize - HEADER_SIZE;

  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  STOS2(size, p);    p += SIZEOFSHORT;
  *p++ = (BYTE)stat;

  if (size > SIZEOFCHAR) {
    *p++ = (BYTE)majorv; *p++ = (BYTE)minorv;
    LTOL4(curtime, p); p += SIZEOFLONG;
    STOS2(nproto, p);  p += SIZEOFSHORT;
    STOS2(fillednamelen, p); p += SIZEOFSHORT;
    for (i = 0, cp = protonames ; i < nproto ; i++, cp++) {
      strcpy((char *)p, *cp); 
      len = strlen((char *)p) + 1;
      p += len;
    }
    *p++ = (BYTE)0;
    if (fillednamelen > namelen) *p++ = (BYTE)0;
    for (i = 0 ; i < nproto ; i++) {
      LTOL4(*protofreqs, p); protofreqs++; p += SIZEOFINT;
    }
    STOS2(nclients, p);  p += SIZEOFSHORT;
    STOS2(ncontexts, p); p += SIZEOFSHORT;

    for (i = 0 ; i < nclients ; i++, who++) {
      int clientinfolen;
      register ClientPtr awho = *who;
      BYTE *q = p;

      p += SIZEOFSHORT;
      LTOL4(awho->id, p);           p += SIZEOFLONG;
      LTOL4(awho->usr_no, p);       p += SIZEOFLONG;
      LTOL4(awho->used_time, p);    p += SIZEOFLONG;
      LTOL4(awho->idle_date, p);    p += SIZEOFLONG;
      LTOL4(awho->connect_date, p); p += SIZEOFLONG;
      for (j = 1 ; j <= nproto; j++) {
	LTOL4(awho->pcount[j], p);  p += SIZEOFLONG;
      }
      p = copylenstr(awho->username, p);
      p = copylenstr(awho->hostname, p);
      if (client->version_hi > 2) {
	if (awho->clientname) {
	  p = copylenstr(awho->clientname, p);
	}
	else {
	  p = copylenstr("", p);
	}
      }
      bzero(p, ncontexts);
      for (j = 0 ; j < awho->ncon ; j++) {
	p[awho->context_flag[j]] = 1;
      }
      p += ncontexts;
      clientinfolen = p - q - SIZEOFSHORT;
      STOS2(clientinfolen, q);
    }
  }
  
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}

static
SendType2Reply(client, majo, mino, stat)
ClientPtr client;
int majo, mino, stat;
{
  BYTE buf[HEADER_SIZE + SIZEOFCHAR], *p = buf;

  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  *p++ = (BYTE)0;    *p++ = (BYTE)1;
  *p = (BYTE)stat;

  return WriteClient(client->id, buf, sizeof(buf));
}

static
SendType4Reply(client, majo, mino, stat, len, dat)
ClientPtr client;
int majo, mino, stat, len;
int *dat;
{
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  int requiredsize = HEADER_SIZE + SIZEOFCHAR + (len * SIZEOFINT);
  int i, retval, size;

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    requiredsize = HEADER_SIZE + SIZEOFCHAR;
    len = 0;
    stat = -1;
  }
  p = bufp;
  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  size = requiredsize - HEADER_SIZE;
  STOS2(size, p); p += SIZEOFSHORT;
  *p++ = (BYTE)stat;

  for (i = 0 ; i < len ; i++) {
    LTOL4(*dat, p); dat++; p += SIZEOFLONG;
  }
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}

static
SendType4XReply(client, majo, mino, val, s1, s2, len, dat)
ClientPtr client;
int majo, mino, val, len;
char *s1, *s2;
int *dat;
{
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  int retval, size, i, slen1 = strlen(s1) + 1, slen2 = strlen(s2) + 1;
  int requiredsize = HEADER_SIZE + SIZEOFSHORT +
    slen1 + slen2 + len * SIZEOFINT;

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    val = -1;
    len = 0;
    s1 = s2 = "";
    slen1 = slen2 = 1;
    requiredsize = HEADER_SIZE + SIZEOFSHORT + slen1 + slen2;
  }
  p = bufp;
  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  size = requiredsize - HEADER_SIZE;
  STOS2(size, p); p += SIZEOFSHORT;
  *p++ = val;
  bcopy(s1, p, slen1); p += slen1;
  bcopy(s2, p, slen2); p += slen2;
  for (i = 0 ; i < len ; i++) {
    LTOL4(*dat, p); dat++; p += SIZEOFINT;
  }
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}

static
SendType5Reply(client, majo, mino, context)
ClientPtr client;
int majo, mino, context;
{
  BYTE buf[HEADER_SIZE + SIZEOFSHORT], *p = buf;

  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  *p++ = (BYTE)0;    *p++ = (BYTE)SIZEOFSHORT;
  STOS2(context, p);

  return WriteClient(client->id, buf, sizeof(buf));
}

static
namesize(names, n)
char *names;
int n;
{
  register int tmp, res = 0;

  if (n < 0) {
    return 0;
  }
  while (n--) {
    tmp = strlen(names) + 1;
    names += tmp;
    res += tmp;
  }
  return res + 1;
}

static
unamesize(names, n)
Ushort *names;
int n;
{
  register int tmp, res = 0;

  if (n < 0) {
    return 0;
  }
  while (n--) {
    tmp = ushortstrlen(names) + 1;
    names += tmp;
    res += tmp;
  }
  return res + 1;
}

static
unamesize2(names)
Ushort *names;
{
  register int tmp, res = 0;

  while (*names) {
    tmp = ushortstrlen(names) + 1;
    names += tmp;
    res += tmp;
  }
  return res + 1;
}

static
SendType6Reply(client, majo, mino, n, names, namelen)
ClientPtr client;
int majo, mino, n, namelen;
char *names;
{
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  int requiredsize, retval, size;

  requiredsize = HEADER_SIZE + SIZEOFSHORT + namelen;

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    namelen = 0;
    n = -1;
    requiredsize = HEADER_SIZE + SIZEOFSHORT;
  }
  p = bufp;
  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  size = requiredsize - HEADER_SIZE;
  STOS2(size, p); p += SIZEOFSHORT;
  STOS2(n, p);    p += SIZEOFSHORT;
  bcopy(names, p, namelen);
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}

static
SendType7Reply(client, majo, mino, n, len, dat)
ClientPtr client;
int majo, mino, n, len;
Ushort *dat;
{
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  int requiredsize = HEADER_SIZE + SIZEOFSHORT + (len * SIZEOFSHORT);
  int retval, size, i;

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    n = -1;
    len = 0;
    requiredsize = HEADER_SIZE + SIZEOFSHORT;
  }
  p = bufp;
  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  size = requiredsize - HEADER_SIZE;
  STOS2(size, p); p += SIZEOFSHORT;
  STOS2(n, p);    p += SIZEOFSHORT;
  for (i = 0 ; i < len ; i++) {
    STOS2(*dat, p); dat++; p += SIZEOFSHORT;
  }
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}

static
SendType9Reply(client, majo, mino, val, len, dat)
ClientPtr client;
int majo, mino, val, len;
int *dat;
{
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  int requiredsize = HEADER_SIZE + SIZEOFSHORT + len * SIZEOFINT;
  int retval, size, i;

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    val = -1;
    len = 0;
    requiredsize = HEADER_SIZE + SIZEOFSHORT;
  }
  p = bufp;
  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  size = requiredsize - HEADER_SIZE;
  STOS2(size, p); p += SIZEOFSHORT;
  STOS2(val, p);  p += SIZEOFSHORT;
  for (i = 0 ; i < len ; i++) {
    LTOL4(*dat, p); dat++; p += SIZEOFINT;
  }
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}

#if 0
/* Is this used? */
static
SendType10Reply(client, majo, mino, val, s1, s2, len, dat)
ClientPtr client;
int majo, mino, val, len;
char *s1, *s2;
int *dat;
{
  BYTE lbuf[DEFAULTBUFSIZE], *bufp = lbuf, *p;
  int retval, size, i, slen1 = strlen(s1) + 1, slen2 = strlen(s2) + 1;
  int requiredsize = HEADER_SIZE + SIZEOFSHORT +
    slen1 + slen2 + len * SIZEOFINT;

  if (requiredsize > sizeof(lbuf) && !(bufp = (BYTE *)malloc(requiredsize))) {
    val = -1;
    len = 0;
    s1 = s2 = "";
    slen1 = slen2 = 1;
    requiredsize = HEADER_SIZE + SIZEOFSHORT + slen1 + slen2;
  }
  p = bufp;
  *p++ = (BYTE)majo; *p++ = (BYTE)mino;
  size = requiredsize - HEADER_SIZE;
  STOS2(size, p); p += SIZEOFSHORT;
  STOS2(val, p);  p += SIZEOFSHORT;
  bcopy(s1, p, slen1); p += slen1;
  bcopy(s2, p, slen2); p += slen2;
  for (i = 0 ; i < len ; i++) {
    LTOL4(*dat, p); dat++; p += SIZEOFINT;
  }
  retval = WriteClient(client->id, bufp, requiredsize);
  if (bufp != lbuf) free((char *)bufp);
  return retval;
}
#endif

static
irw_finalize( clientp )
ClientPtr *clientp ;
{
    register ClientPtr client = *clientp;

    if (SendType2Reply(client, wFinalize, !EXTPROTO, 0) < 0) {
      return -1;
    }

    /* close処理＆後始末（コンテクストの開放等） */
    CloseDownClient( client ) ;
    *clientp = (ClientPtr)0;
    return( 0 ) ;
}

static
irw_create_context( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    int cxnum ;

    cxnum = RkwCreateContext() ;
    if (SetDicHome(client, cxnum) > 0) {
      set_cxt(client, cxnum);
    }
    else {
      RkwCloseContext(cxnum);
      cxnum =  -1;
      PrintMsg("[%s](%s) Can't set dictionary home\n",
	       client->username, WideProtoName[wCreateContext - 1]);
    }

    return SendType5Reply(client, wCreateContext, !EXTPROTO, cxnum);
}

/*
  validcontext -- コンテキストが有効かどうかを確認する。

  有効なら 1 を返し無効なら 0 を返す。

  また、無効ならメッセージを出力する。
 */

static
validcontext(cxnum, client, proto)
int cxnum, proto;
ClientPtr client;
{
  if (chk_cxt(client, cxnum)) {
    return 1;
  }
  PrintMsg("[%s](%s) Context Err[%d]\n", client->username,
	   WideProtoName[proto - 1], cxnum) ;
  return 0;
}

static
irw_duplicate_context( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    int cxnum = Request.type2.context;
    int retval = -1;

    if (validcontext(cxnum, client, wDuplicateContext)) {
        retval = RkwDuplicateContext( cxnum ) ;
	if (retval >= 0) {
	  if (!set_cxt(client, retval)) {
	    RkwCloseContext(retval);
	    retval = -1;
	  }
	}
    }

    return SendType5Reply(client, wDuplicateContext, !EXTPROTO, retval);
}

static
irw_close_context( clientp )
ClientPtr *clientp ;
{
    extern void off_cxt();
    ClientPtr client = *clientp ;
    int cxnum = Request.type2.context, stat = -1;

    if (validcontext(cxnum, client, wCloseContext)) {
	stat = (char)RkwCloseContext( cxnum ) ;
	off_cxt(client, cxnum);
    }

    return SendType2Reply(client, wCloseContext, !EXTPROTO, stat);
}

static
irw_dictionary_list( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *dicnames = (char *)local_buffer ;
    int cxnum = Request.type3.context;
    int stat = -1, max = Request.type3.buflen, retval;

    if (validcontext(cxnum, client, wGetDictionaryList)) {
      if (max <= sizeof(local_buffer) || (dicnames = malloc(max))) {
	stat = (int)RkwGetDicList( cxnum, dicnames, max ) ;
      }
    }
    retval = SendType6Reply(client, wGetDictionaryList, !EXTPROTO, stat,
			    dicnames, namesize(dicnames, stat));
    if (dicnames != (char *)local_buffer) free(dicnames);
    return retval;
}

static
irw_get_yomi( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    Ushort *yomi = (Ushort *)local_buffer ;
    int stat = -1, cxnum = Request.type6.context, size = 0, retval;
    int bunsetuno = Request.type6.number ;
    int maxyomi = Request.type6.buflen;

    if (validcontext(cxnum, client, wGetYomi)) {
	ir_debug( Dmsg(5, "maxyomi [%d]\n", maxyomi ) );

	if (maxyomi <= sizeof(local_buffer) / sizeof(Ushort) ||
	    (yomi = (Ushort *)malloc(maxyomi * sizeof(Ushort)))) {
	  if( RkwGoTo(cxnum, bunsetuno) == bunsetuno ) {
	    if ((stat = RkwGetYomi(cxnum, yomi, maxyomi)) >= 0) {
		size = (stat + 1);
	    }
	  } else {
	    PrintMsg("[%s](%s) bunsetu move failed\n",
		     client->username, WideProtoName[wGetYomi - 1]);
	  }
	}
    }
    retval = SendType7Reply(client, wGetYomi, !EXTPROTO, stat, size, yomi);
    if (yomi != (Ushort *)local_buffer) free((char *)yomi);
    return retval;
}

static
irw_define_dic( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    Ushort *data;
    char *dicname;
    int cxnum = Request.type12.context, stat = -1;

    if (validcontext(cxnum, client, wDefineWord)) {
	dicname = Request.type12.dicname ;
	data = Request.type12.datainfo;
	ir_debug( Dmsg(5, "辞書名=%s\n", (dicname)?dicname:null) );
	ir_debug( Dmsg(5, "登録するデータ[%s]\n",
		       (data)?conveuc(data):null) );

	stat = RkwDefineDic(cxnum, dicname, data);
    }

    return SendType2Reply(client, wDefineWord, !EXTPROTO, stat);
}

static
irw_delete_dic( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *dicname ;
    Ushort *data;
    int cxnum = Request.type12.context, stat = -1;

    if (validcontext(cxnum, client, wDeleteWord)) {
	dicname = Request.type12.dicname ;
	data = Request.type12.datainfo ;
	ir_debug( Dmsg(5, "辞書名=%s\n", (dicname)?dicname:null) );
	ir_debug( Dmsg(5, "削除するデータ[%s]\n",
		       (data)?conveuc(data):null) );

	stat = RkwDeleteDic(cxnum, dicname, data);
    }

    return SendType2Reply(client, wDeleteWord, !EXTPROTO, stat);
}

static
irw_get_dir_list( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *dicnames = (char *)local_buffer ;
    int cxnum = Request.type3.context, stat = -1, retval;

    if (validcontext(cxnum, client, wGetDirectoryList)) {
      if (Request.type3.buflen <= sizeof(local_buffer) ||
	  (dicnames = malloc((int)Request.type3.buflen))) {
	stat = RkwGetDirList(cxnum, dicnames, (int)Request.type3.buflen);
      }
    }	
    retval = SendType6Reply(client, wGetDirectoryList, !EXTPROTO, stat,
			    dicnames, namesize(dicnames, stat));
    if (dicnames != (char *)local_buffer) free(dicnames);
    return retval;
}

static
irw_mount_dictionary( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *dicname ;
    int cxnum = Request.type15.context, stat = -1;
    extern MMountFlag;

    if (validcontext(cxnum, client, wMountDictionary)) {
	dicname = Request.type15.dicname ;
	ir_debug( Dmsg(5, "dicname = %s\n", (dicname)?dicname:null) );

	stat = RkwMountDic(cxnum, dicname, Request.type15.mode | MMountFlag) ;
    }

    return SendType2Reply(client, wMountDictionary, !EXTPROTO, stat);
}

static
irw_umount_dictionary( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    int cxnum = Request.type15.context, stat = -1;

    if (validcontext(cxnum, client, wUnmountDictionary)) {
	ir_debug( Dmsg(5, "dicname = %s\n",
		       (Request.type15.dicname)?Request.type15.dicname:null) );

	stat = RkwUnmountDic(cxnum, Request.type15.dicname);
    }

    return SendType2Reply(client, wUnmountDictionary, !EXTPROTO, stat);
}

static
irw_rmount_dictionary( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    int cxnum = Request.type15.context, stat = -1;

    if (validcontext(cxnum, client, wRemountDictionary)) {
	ir_debug( Dmsg(5, "dicname = %s\n",
		       (Request.type15.dicname)?Request.type15.dicname:null) );

	stat = RkwRemountDic(cxnum, Request.type15.dicname,
			     Request.type15.mode);
    }

    return SendType2Reply(client, wRemountDictionary, !EXTPROTO, stat);
}

static
irw_mount_list( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *dicnames = (char *)local_buffer ;
    int cxnum = Request.type3.context, stat = -1, retval;

    if (validcontext(cxnum, client, wGetMountDictionaryList)) {
      if (Request.type3.buflen <= sizeof(local_buffer) ||
	  (dicnames = malloc((int)Request.type3.buflen))) {
	stat = RkwGetMountList(cxnum, dicnames, (int)Request.type3.buflen);
      }
    }	

    retval = SendType6Reply(client, wGetMountDictionaryList, !EXTPROTO, stat,
			    dicnames, namesize(dicnames, stat));
    if (dicnames != (char *)local_buffer) free(dicnames);
    return retval;
}

static
irw_convert( clientp )
ClientPtr *clientp ;
{
    wReq14 *req = &Request.type14 ;
    ClientPtr client = *clientp ;
    Ushort *yomi, *bufp = (Ushort *)local_buffer;
    int cxnum = Request.type14.context, yomilen, ret, stat = -1;
    int size = 0, retval;

    if (validcontext(cxnum, client, wBeginConvert)) {
	yomilen = req->datalen - SIZEOFSHORT * 2 - SIZEOFLONG;
	yomilen /= sizeof( Ushort );
	yomi = req->yomi;

	ir_debug(Dmsg(5, "読み = %s\n",
		      (yomi)?conveuc(yomi):null));

	if( (ret = RkwBgnBun( cxnum, yomi, yomilen, req->mode ))
	   >= 0 ) {
	    /* 最優先候補リストを取得する */
            size = GetFirstKouho(cxnum, 0, ret, &stat,
                                 (Ushort *)local_buffer,
                                 sizeof(local_buffer) / sizeof(Ushort),
                                 &bufp);
	} else {
	    PrintMsg( "[%s](%s) kana-kanji convert failed\n",
		     client->username, WideProtoName[wBeginConvert - 1]);
	}
    }	
    retval =
      SendType7Reply(client, wBeginConvert, !EXTPROTO, stat, size, bufp);
    if (bufp != (Ushort *)local_buffer) free((char *)bufp);
    return retval;
}

static
irw_convert_end( clientp )
ClientPtr *clientp ;
{
    wReq10 *req = &Request.type10 ;
    ClientPtr client = *clientp ;
    int mode = Request.type10.mode;
    int cxnum = Request.type10.context, len, i, stat = -1;

    if (validcontext(cxnum, client, wEndConvert)) {
      if (mode) { /* 学習するなら */
        len = req->number;
	if (len) {
	    if( RkwGoTo( cxnum, 0 ) != 0 ) {	

		PrintMsg("[%s](%s) ir_convert_end: RkwGoTo failed\n",
			 client->username, WideProtoName[wEndConvert - 1]);
	    }
	    ir_debug( Dmsg(5, "学習させる候補\n") );

	    /* カレント候補を先頭に移動クライアントが選んだ候補を */	
	    /* RKに知らせる */		
	    for( i = 0; i < len; i++ ){ 
		if( req->kouho[ i ] != RkwXfer( cxnum, req->kouho [ i ] ) ) {

		    PrintMsg("[%s](%s) irw_convert_end: RkwXfer failed\n",
			     client->username, WideProtoName[wEndConvert - 1]);
		}
		ir_debug( DebugDispKanji( cxnum, i ) );

		if( RkwRight( cxnum ) == 0 && i != (len - 1) ) { 	

		    PrintMsg("[%s](%s) irw_convert_end: RkwRight failed\n",
			     client->username, WideProtoName[wEndConvert - 1]);
		}
	    }
	    ir_debug( Dmsg(5, "\n") );
	}
      }
      stat = RkwEndBun(cxnum, mode);
    }
    return SendType2Reply(client, wEndConvert, !EXTPROTO, stat);
}

static
irw_get_kanjilist( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    Ushort *kouho = (Ushort *)local_buffer ;
    int stat = -1, cxnum = Request.type6.context, retval;
    int bunsetuno, maxkanji = Request.type6.buflen;

    if (validcontext(cxnum, client, wGetCandidacyList)) {
	bunsetuno = Request.type6.number;
	if (maxkanji <= sizeof(local_buffer) / sizeof(Ushort) ||
	    (kouho = (Ushort *)malloc(maxkanji * sizeof(Ushort)))) {
	  ir_debug( Dmsg(5, "maxkanji [%d]\n", maxkanji) );

	  if( RkwGoTo(cxnum, bunsetuno) == bunsetuno ) {
	    stat = RkwGetKanjiList(cxnum, kouho, maxkanji);
	  } else {
	    PrintMsg("[%s](%s) bunsetu move failed\n",
		     client->username, WideProtoName[wGetCandidacyList - 1]);
	  }
	}
    }
    retval = SendType7Reply(client, wGetCandidacyList, !EXTPROTO,
			    stat, unamesize(kouho, stat), kouho);
    if (kouho != (Ushort *)local_buffer) free((char *)kouho);
    return retval;
}

static
irw_resize(clientp)
ClientPtr *clientp ;
{
#define ENLARGE -1
#define SHORTEN -2
    wReq7 *req = &Request.type7 ;
    ClientPtr client = *clientp ;
    int ret, cxnum = Request.type7.context, yomilen, bunsetu, stat = 0;
    int size = 0, retval;
    Ushort *bufp = (Ushort *)local_buffer;

    if (validcontext(cxnum, client, wResizePause)) {
	bunsetu = req->number ;
	yomilen = req->yomilen ;

	RkwGoTo(cxnum, bunsetu) ;
	ir_debug( Dmsg(5, "yomilen = %d\n", yomilen) );
	ir_debug( Dmsg(5, "bunsetu = %d\n", bunsetu) );

	switch( yomilen ) {
	    case ENLARGE :
		ret = RkwEnlarge( cxnum ) ;
		break ;
	    case SHORTEN :
		ret = RkwShorten( cxnum ) ;
		break ;
	    default :
		ret = RkwResize( cxnum, yomilen ) ;
		break ;
	    }
	/* 最優先候補リストを取得する */
        size = GetFirstKouho(cxnum, bunsetu, ret, &stat,
			     (Ushort *)local_buffer,
                             sizeof(local_buffer) / sizeof(Ushort), &bufp);
    }
    retval = SendType7Reply(client, wResizePause, !EXTPROTO, stat, size, bufp);
    if (bufp != (Ushort *)local_buffer) free((char *)bufp);
    return retval;
}

static
irw_store_yomi( clientp )
ClientPtr *clientp ;
{
    wReq11 *req = &Request.type11 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type11.context, bunsetu, ret, stat = -1;
    int size = 0, len;
    Ushort *bufp = (Ushort *)local_buffer;

    if (validcontext(cxnum, client, wStoreYomi)) {
	bunsetu = req->curbun ;

	RkwGoTo( cxnum, bunsetu ) ;

	ir_debug( Dmsg(5, "読み = %s\n",
		       (req->yomi)?conveuc(req->yomi):null));

	len = ((int)req->datalen - SIZEOFSHORT * 2) / SIZEOFSHORT;
	ret = RkwStoreYomi(cxnum, req->yomi, len);
	if( ret >= 0 ){
	  if (len == 0 && ret && bunsetu >= ret)
	    bunsetu--;
	  RkwGoTo(cxnum, 0);
          size = GetFirstKouho(cxnum, 0, ret, &stat, (Ushort *)local_buffer,
                               sizeof(local_buffer) / sizeof(Ushort), &bufp);
	  RkwGoTo(cxnum, bunsetu);
	} else {
	    PrintMsg("[%s](%s) RkwStoreYomi faild\n",
		     client->username, WideProtoName[wStoreYomi - 1]);
	    stat = -1 ;
	}
    }
    ret = SendType7Reply(client, wStoreYomi, !EXTPROTO, stat, size, bufp);
    if (bufp != (Ushort *)local_buffer) free((char *)bufp);
    return ret;
}

static
irw_query_extension( clientp )
ClientPtr *clientp ;
{
    wReq17 *req = &Request.type17 ;
    ClientPtr client = *clientp ;
    char *reqname;
    int i, stat = 0 ;

    reqname = req->dicname;
    for( i = 0; *reqname; i++ ){
	if (strcmp(ExtensionRequest[i], reqname)) {
	    stat = -1;
	    break;
	}
	reqname += strlen(reqname) + 1;
    }

    return SendType2Reply(client, wQueryExtensions, !EXTPROTO, stat);
}

/*
 checkPermissionToRead

 指定した dirname、dicname に対して読み込み権があるかどうかを調査する。
 dicname に対して NULL を指定すると、ディレクトリの読み込み権だけを調
 査する。

 return value:
    0  読める(あるいはチェックの必要がない)
   -1  読めない
 */

int
checkPermissionToRead(client, dirname, dicname)
ClientPtr client;
char *dirname, *dicname;
{
  int check = 0, len;
  char *dp;

  if (*dirname) {
    int newcon;

    newcon = RkwCreateContext();
    dp = dirname;
    while (!check && *dp) {
      if (!strncmp(dp, DDUSER, DDUSERLEN) && dp[DDUSERLEN] == '/') {
	len = strlen(client->username);
	if (strncmp(dp + DDUSERLEN + 1, client->username, len)) {
	  check = 1; /* yes */
	}
	else {
	  len += DDUSERLEN + 1;
	}
      }
      else if (!strncmp(dp, DDGROUP, DDGROUPLEN) && dp[DDGROUPLEN] == '/') {
	if (!client->groupname ||
	    strncmp(dp + DDGROUPLEN + 1, client->groupname, 
		    (len = strlen(client->groupname)))) {
	  check = 1; /* yes */
	}
	else {
	  len += DDGROUPLEN + 1;
	}
      }
      else if (strncmp(dp, DDPATH, DDPATHLEN)) {
	/* システム辞書ディレクトリとも違う場合 */
	check = 1; /* yes */
      }
      else {
	len = DDPATHLEN;
      }
      if (!check && dp[len] && dp[len] != ':') {
	check = 1;
      }

      if (check && 0 <= newcon) { /* check permission */
	if (RkwSetDicPath(newcon, dp) >= 0) {
	  check = RkwChmodDic(newcon, (char *)0, RK_USR_DIR);
	  if (check >= 0 && /* ディレクトリが読めれば */
	      (check & (RK_ENABLE_READ | RK_DISABLE_READ)) ==
	      RK_ENABLE_READ) {
	    if (!dicname ||
		((check = RkwChmodDic(newcon, dicname, 0)) >= 0 &&
		 (check & (RK_ENABLE_READ | RK_DISABLE_READ)) ==
		 RK_ENABLE_READ)) {
	      check = 0; /* ok */
	      while (*dp && *dp != ':') {
		dp++;
	      }
	      if (*dp == ':') {
		dp++;
	      }
	    }
	  }
	}
      }
      else {
	dp += len + (dp[len] ? 1 : 0);
      }
    }
    if (0 <= newcon) {
      RkwCloseContext(newcon);
    }
  }
  return check ? -1 : 0;
}

#ifdef EXTENSION

/*
  insertUserSla

  dirname を与えると、user/ を各ディレクトリに挿入するプログラム
  "canna" と言うディレクトリに関してはフリーパスとする。

 */

static char *
insertUserSla(dirname, dirlen)
char *dirname;
int dirlen;
{
  int ncolon = 0;
  char *p, *q, *r, *s, *res;

  for (p = dirname ; *p ; p++) {
    if (*p == ':') {
      ncolon++;
    }
  }
  ncolon++;

  res = malloc((strlen(DDUSER) + 1) * ncolon + dirlen + 1);
  if (res) {
    for (p = dirname, q = res ; *p ;) {
      r = q;
      strcpy(q, DDUSER); q += DDUSERLEN;
      *q++ = '/';
      s = q;
      while (*p && *p != ':') {
	*q++ = *p++;
      }
      *q = '\0';
      if (!strcmp(DDPATH, s)) {
	strcpy(r, DDPATH);
	q = r + DDPATHLEN;
      }
      if (*p) {
	*q++ = *p++;
      }
    }
  }
  return res;
}


static
irw_list_dictionary( clientp )
ClientPtr *clientp ;
{
    wReq18 *req = &Request.type18 ;
    ClientPtr client = *clientp ;
    char *dicnames = (char *)local_buffer ;
    char *dirname, *dirnamelong = (char *)0;
    int cxnum = Request.type18.context, stat = -1;
    int requestsize = Request.type18.size, retval;

    if (validcontext(cxnum, client, wListDictionary)) {
      if (requestsize <= sizeof(local_buffer) ||
	  (dicnames = malloc(requestsize))) {
	dirname = req->data ;
	if (!dirname || dirname[0] != ':' ||
	    canna_version(client->version_hi, client->version_lo) <
	    canna_version(3, 1)) {
	  /* 昔のプロトコルではディレクトリ名に user/ が付いていないので
	     それの対応(W_PROTO 3.0 以前) */
	  dirnamelong = insertUserSla(dirname, strlen(dirname));
	  if (dirnamelong) {
	    dirname = dirnamelong;
	  }
	  else {
	    goto listdicdone;
	  }
	}
	else {
	  dirname++;
	}

	/* 以下、パーミッションのチェック */
	stat = ACCES;
	if (checkPermissionToRead(client, dirname, (char *)0) >= 0) {
	  stat = RkwListDic(cxnum, (unsigned char *)dirname,
			    (unsigned char *)dicnames, requestsize);
	}

      listdicdone:
	if (dirnamelong) {
	  free(dirnamelong);
	}
	ir_debug( Dmsg(5, "辞書リスト\n") );
      }
    }	

    retval = SendType6Reply(client, wListDictionary, EXTPROTO, stat,
			    dicnames, namesize(dicnames, stat));
    if (dicnames != (char *)local_buffer) free(dicnames);
    return retval;
}

static
irw_create_dictionary( clientp )
ClientPtr *clientp ;
{
    wReq15 *req = &Request.type15 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type15.context, stat = BADCONT;

    if (validcontext(cxnum, client, wCreateDictionary)) {
	ir_debug( Dmsg(5, "dicname = %s\n",
		       (req->dicname)?req->dicname:null) );

	stat = (char)RkwCreateDic(cxnum, (unsigned char *)req->dicname,
				  req->mode);
    }

    return SendType2Reply(client, wCreateDictionary, EXTPROTO, stat);
}

static
irw_chmod_dictionary( clientp )
ClientPtr *clientp;
{
  wReq15 *req = &Request.type15;
  ClientPtr client = *clientp;
  int cxnum = Request.type15.context, stat = BADCONT;
  
  if (validcontext(cxnum, client, wChmodDictionary)) {
    ir_debug(Dmsg(5, "dicname = %s\n", (req->dicname) ? req->dicname : null));

    stat = RkwChmodDic(cxnum, req->dicname, req->mode);
  }
  return SendType5Reply(client, wChmodDictionary, EXTPROTO, stat);
}

static
irw_remove_dictionary( clientp )
ClientPtr *clientp ;
{
    wReq15 *req = &Request.type15 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type15.context, stat = BADCONT;

    if (validcontext(cxnum, client, wDeleteDictionary)) {
	ir_debug( Dmsg(5, "dicname = %s\n",
		       (req->dicname)?req->dicname:null) );

	stat = (char)RkwRemoveDic(cxnum, (unsigned char *)req->dicname,
				  req->mode);
    }

    return SendType2Reply(client, wDeleteDictionary, EXTPROTO, stat);
}

static
irw_rename_dictionary( clientp )
ClientPtr *clientp ;
{
    wReq15 *req = &Request.type15 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type15.context, diclen ; 
    int stat = -1 ;

    if (validcontext(cxnum, client, wRenameDictionary)) {
	diclen = strlen(req->dicname) + 1 ;
	stat = RkwRenameDic(cxnum, (unsigned char *)req->dicname,
			    (unsigned char *)&(req->dicname)[diclen],
			    req->mode);
    }
    return SendType2Reply(client, wRenameDictionary, EXTPROTO, stat);
}

/* CopyDic ここから */

static
irw_copy_dictionary( clientp )
ClientPtr *clientp;
{
    wReq21 *req = &Request.type21;
    ClientPtr client = *clientp;
    int cxnum = Request.type21.context;
    int stat = -1;
    char *dir = req->dirname;

    if (validcontext(cxnum, client, wCopyDictionary)) {
      stat = ACCES;

      if (dir && *dir == ':') {
	dir++; /* つけるやつがいるので念のため */
      }
      if (checkPermissionToRead(client, dir, req->srcdic) >= 0) {
	stat = RkwCopyDic(cxnum, dir, req->srcdic, req->dstdic, req->mode);
      }
    }
    return SendType2Reply(client, wCopyDictionary, EXTPROTO, stat);
}

/*    ここまで    */

static
irw_get_word_text_dic( clientp )
ClientPtr *clientp ;
{
    wReq18 *req = &Request.type18 ;
    ClientPtr client = *clientp ;
    Ushort *infobuf = (Ushort *)local_buffer ;
    char *dicname, *dirname, *dirnamelong = (char *)0;
    int cxnum = Request.type18.context, stat = BADCONT;
    int dirlen, requestsize = Request.type18.size, retval;

    if (validcontext(cxnum, client, wGetWordTextDictionary)) {
	dirname = req->data ;
	dirlen = strlen(dirname) + 1 ;
	dicname = &(req->data[dirlen]) ;
	if (dirlen > 1) {
	  if (!dirname || dirname[0] != ':' ||
	      canna_version(client->version_hi, client->version_lo) <
	      canna_version(3, 1)) {
	    /* 昔のプロトコルではディレクトリ名に user/ が付いていないので
	       それの対応(W_PROTO 3.0 以前) */
	    dirnamelong = insertUserSla(dirname, dirlen);
	    if (dirnamelong) {
	      dirname = dirnamelong;
	    }
	    else {
	      goto getworddone;
	    }
	  }
	  else {
	    dirname++;
	  }
	}

	if (dicname[0]) {
	  /* 以下、パーミッションのチェック */
	  /* 最初(dicname が指定されているとき)だけチェックする。
	     最初が通らなければ２発目以降も通らないため */
	  if (checkPermissionToRead(client, dirname, dicname) < 0) {
	    stat = ACCES;
	    goto getworddone;
	  }
	}

	if (requestsize <= sizeof(local_buffer) / sizeof(Ushort) ||
	    (infobuf = (Ushort *)malloc(requestsize * sizeof(Ushort)))) {
	  stat = RkwGetWordTextDic(cxnum, (unsigned char *)dirname,
				   (unsigned char *)dicname,
				   infobuf, requestsize);
	}
      getworddone:
	if (dirnamelong) {
	  free(dirnamelong);
	}
    }
    retval = SendType7Reply(client, wGetWordTextDictionary, EXTPROTO,
			    stat, stat > 0 ? stat + 1 : 0, infobuf);
    if (infobuf != (Ushort *)local_buffer) free((char *)infobuf);
    return retval;
}

static
irw_server_stat( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp, who, *OutPut;
    int i, j, count, stat = 0, max_cx, majorv, minorv, curtime, retval, n;
    char tmpversion[24]; /* 24??? */

    OutPut = (ClientPtr *)malloc(connow_socks * sizeof(ClientPtr));

    /* サーババージョン */
    strcpy(tmpversion, SERVER_VERSION);
    majorv = atoi(strtok(tmpversion, "."));
    minorv = atoi(strtok(NULL, "."));

    /* 現在時刻 */
    curtime = time((long *)0);

    /* 接続しているクライアント数 */
    if (OutPut) {
      count = ConnectClientCount(client, OutPut, connow_socks) ;
    }
    else {
      count = 0;
    }

    /* コンテクスト数(一番大きいコンテキストを調べる) */
    max_cx = 0;
    for (i = 0 ; i < count ; i++) {
      int *contexts;

      who = OutPut[i];
      contexts = who->context_flag;
      for (j = 0, n = who->ncon ; j < n ; j++) {
	if (max_cx < contexts[j]) {
	  max_cx = contexts[j];
	}
      }
    }
    max_cx++;
    max_cx += (max_cx % SIZEOFSHORT); /* ????? */

    retval = SendType1Reply(client, wGetServerInfo, EXTPROTO,
			    stat, majorv, minorv, curtime,
			    W_REALREQUEST, WideProtoName,
			    TotalWideRequestTypeCount + 1,
			    count, max_cx, OutPut);
    if (OutPut) {
      free((char *)OutPut);
    }
    return retval;
}

extern NumberAccessControlList();
extern ACLPtr ACLHead;

static
irw_host_ctl( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *hosts, *users, *endhosts;
    register ACLPtr wp ;
    register int i, len = 0, nhost;

    if( (nhost = NumberAccessControlList()) >= 0 ) {
	ir_debug( Dmsg(5, "ホストリスト\n") );

	hosts = (char *)local_buffer;
	endhosts = hosts + sizeof(local_buffer);
	for( wp = ACLHead; wp != (ACLPtr)NULL; wp = wp->next ) {
	    len = strlen((char *)wp->hostname) + 1;
	    if (endhosts < hosts + len) {
	      nhost = -1; len = 0;
	      goto endhostctl;
	    }
	    strcpy(hosts, (char *)wp->hostname);
	    hosts += len;
	    users = (char *)wp->usernames;
	    for( i = 0; i < wp->usercnt; i++ ) {
		len = strlen(users) + 1;
		if (endhosts < hosts + len) {
		  nhost = -1; len = 0;
		  goto endhostctl;
		}
		strcpy(hosts, users);
		hosts += len;
		users += len;
	    }
	    *hosts++ = '\0';
	}
	len = hosts - (char *)local_buffer;
    }
  endhostctl:
    if (SendType6Reply(client, wGetAccessControlList, EXTPROTO, nhost,
		       (char *)local_buffer, len) < 0) {
      return -1;
    }
    /* CloseDownClient( client ) ; */
    /* *clientp = (ClientPtr)0; */
    return( 1 ) ;
}

static
irw_sync(clientp)
ClientPtr *clientp;
{
  wReq15 *req = &Request.type15 ;
  ClientPtr client = *clientp;
  int cxnum = req->context, stat = -1;

  if (validcontext(cxnum, client, wSync)) {
    stat = RkwSync(cxnum, req->dicname);
  }
  return SendType2Reply(client, wSync, EXTPROTO, stat);
}
#endif /* EXTENSION */

static
irw_get_stat( clientp )
ClientPtr *clientp ;
{
    int cxnum = Request.type7.context;
    int bunsetu = Request.type7.number;
    int kouho = Request.type7.yomilen;
    ClientPtr client = *clientp ;
    int retval = -1, len = 0;
    RkStat stat ;

    if (validcontext(cxnum, client, wGetStatus)) {

	RkwGoTo( cxnum, bunsetu ) ;
	RkwXfer( cxnum, kouho ) ;

	retval = RkwGetStat( cxnum, &stat ) ;

        if (retval >= 0) {
	  len = sizeof(RkStat) / sizeof(int);
	}
    }
    return SendType4Reply(client, wGetStatus, !EXTPROTO, retval, len,
			  (int *)&stat);
}

static
irw_get_lex( clientp )
ClientPtr *clientp ;
{
  ClientPtr client = *clientp;
  RkLex *lex = (RkLex *)local_buffer;
  int cxnum = Request.type9.context;
  int tangosu = -1, retval;

  if (validcontext(cxnum, client, wGetLex)) {
    if (Request.type9.max <= sizeof(local_buffer) / sizeof(RkLex) ||
	(lex = (RkLex *)malloc((int)Request.type9.max * sizeof(RkLex)))) {
      RkwGoTo(cxnum, (int)Request.type9.number);
      RkwXfer(cxnum, (int)Request.type9.kouho);
      tangosu = RkwGetLex(cxnum, lex, (int)Request.type9.max);
    }
  }
  retval = SendType9Reply(client, wGetLex, !EXTPROTO, tangosu,
			  (int)((tangosu > 0) ?
				tangosu * (sizeof(RkLex) / sizeof(int)) : 0),
			  (int *)lex);
  if (lex != (RkLex *)local_buffer) free((char *)lex);
  return retval;
}

/* 逐次変換で使用する関数 */
static
irw_autoconv( clientp )
ClientPtr *clientp ;
{
    wReq5 *req = &Request.type5 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type5.context, stat = -1 ;

    if (validcontext(cxnum, client, wAutoConvert)) {
	if( (stat = RkwBgnBun( cxnum, (Ushort *)NULL,
			      (int)req->size, req->mode )) < 0 ) {

	    PrintMsg( "[%s](%s) kana-kanji convert failed\n",
		     client->username, WideProtoName[wAutoConvert - 1]);
	}
    }	
    return SendType2Reply(client, wAutoConvert, !EXTPROTO, stat);
}

static
irw_subst_yomi( clientp )
ClientPtr *clientp ;
{
    wReq4 *req = &Request.type4 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type4.context, ret, stat = -1, size = 0 ;
    Ushort *bufp = (Ushort *)local_buffer;

    if (validcontext(cxnum, client, wSubstYomi)) {
	ir_debug( Dmsg(5, "読み = %s\n",
		       (req->yomi)?conveuc(req->yomi):null));

	if( (ret = RkwSubstYomi(cxnum, (int)req->begin, (int)req->end,
				req->yomi, (int)req->yomilen)) < 0) {

	    PrintMsg( "[%s](%s) kana-kanji convert failed\n",
		     client->username, WideProtoName[wSubstYomi - 1]);
	    stat = ret ;
	} else {
	    /* 最優先候補リストを取得する */
            size = GetFirstKouho(cxnum, 0, ret, &stat, (Ushort *)local_buffer,
                                 sizeof(local_buffer) / sizeof(Ushort), &bufp);
	}
    }
    ret = SendType7Reply(client, wSubstYomi, !EXTPROTO, stat, size, bufp);
    if (bufp != (Ushort *)local_buffer) free((char *)bufp);
    return ret;
}

static
irw_flush_yomi( clientp )
ClientPtr *clientp ;
{
    wReq10 *req = &Request.type10;
    ClientPtr client = *clientp ;
    int cxnum = Request.type10.context, len, i, ret, stat = -1, size = 0 ;
    Ushort *bufp = (Ushort *)local_buffer;

    if (validcontext(cxnum, client, wFlushYomi)) {
        len = req->number;
	if (len) {
	    if( RkwGoTo( cxnum, 0 ) != 0 ) {	

		PrintMsg("[%s](%s) irw_flush_yomi: RkwGoTo failed\n",
			 client->username, WideProtoName[wFlushYomi - 1]);
	    }
	    ir_debug( Dmsg(10, "RKに教える候補\n") );

	    /* カレント候補を先頭に移動 */
	    /* クライアントが選んだ候補をRKに知らせる */		
	    for( i = 0; i < len; i++ ){ 
		if ((int)req->kouho[i] != RkwXfer(cxnum, (int)req->kouho[i])) {
		    PrintMsg("[%s](%s) irw_flush_yomi: RkwXfer failed\n",
			     client->username, WideProtoName[wFlushYomi - 1]);
		}
		ir_debug( DebugDispKanji( cxnum, i ) );

		if( RkwRight( cxnum ) == 0 && i != (len - 1) ) { 	

		    PrintMsg("[%s](%s) irw_flush_yomi: RkwRight failed\n",
			     client->username, WideProtoName[wFlushYomi - 1]);
		}
	    }
	    ir_debug( Dmsg(5, "\n") );

	}
	if( (ret = RkwFlushYomi( cxnum )) < 0 ) {

	    PrintMsg( "[%s](%s) kana-kanji convert failed\n",
		     client->username, WideProtoName[wFlushYomi - 1]);
	    stat = ret ;
	} else {
	    /* 最優先候補リストを取得する */
            size = GetFirstKouho(cxnum, 0, ret, &stat, (Ushort *)local_buffer,
                                 sizeof(local_buffer) / sizeof(Ushort), &bufp);
	}
    }
    ret = SendType7Reply(client, wFlushYomi, !EXTPROTO, stat, size, bufp);
    if (bufp != (Ushort *)local_buffer) free((char *)bufp);
    return ret;
}

static
irw_get_last_yomi( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    Ushort *yomi = (Ushort *)local_buffer ;
    int cxnum = Request.type3.context, maxyomi = Request.type3.buflen;
    int stat = -1, retval;

    if (validcontext(cxnum, client, wGetLastYomi)) {
      if (maxyomi <= sizeof(local_buffer) / sizeof(Ushort) ||
	  (yomi = (Ushort *)malloc(maxyomi * sizeof(Ushort)))) {
	ir_debug( Dmsg(5, "maxyomi [%d]\n", maxyomi) );

	if( (stat = RkwGetLastYomi( cxnum, (Ushort *)yomi, maxyomi )) < 0 ) {

	    PrintMsg( "[%s](%s) kana-kanji convert failed\n",
		     client->username, WideProtoName[wGetLastYomi - 1]);
	} else {
	    /* 未決定文節の読みを取得する */
	    ir_debug(Dmsg(5, "未決文節=%s\n",
			  (yomi)?conveuc(yomi):null));

	}
      }
    }
    retval = SendType7Reply(client, wGetLastYomi, !EXTPROTO,
			    stat, stat > 0 ? stat + 1 : 0, yomi);
    if (yomi != (Ushort *)local_buffer) free((char *)yomi);
    return retval;
}

static
irw_remove_yomi( clientp )
ClientPtr *clientp ;
{
    wReq10 *req = &Request.type10 ;
    ClientPtr client = *clientp ;
    register int curbun, curkouho, i;
    int cxnum = Request.type10.context, maxbun, stat = -1;

    if (validcontext(cxnum, client, wRemoveYomi)) {
      maxbun = curbun = RkwGoTo(cxnum, (int)req->number);
      ir_debug( Dmsg(5, "学習させる候補\n") );

      /* カレント候補を先頭に移動クライアントが選んだ候補を */	
      /* ＲＫに知らせる */		
      for( i = 0; !i || (curbun != maxbun); i++ ){ 
	curkouho = req->kouho[curbun];
	if( curkouho != RkwXfer( cxnum, curkouho ) ) {
	  PrintMsg("[%s](%s) irw_remove_yomi: RkwXfer failed\n",
		   client->username, WideProtoName[wRemoveYomi - 1]);
	}
	ir_debug( DebugDispKanji( cxnum, curbun ) );

	curbun = RkwRight( cxnum );

      }
      ir_debug( Dmsg(5, "\n") );

      stat = RkwRemoveBun( cxnum, req->mode ) ;
    }
    return SendType2Reply(client, wRemoveYomi, !EXTPROTO, stat);
}

static
irw_get_simple_kanji( clientp )
ClientPtr *clientp;
{
    wReq13 *req = &Request.type13;
    ClientPtr client = *clientp ;
    Ushort *kanjis = (Ushort *)local_buffer;
    int stat = -1, cxnum = Request.type13.context;
    int requiredsize, size = 0, retval, maxyomi;

    if (validcontext(cxnum, client, wGetSimpleKanji)) {
      requiredsize = Request.type13.kouhosize + Request.type13.hinshisize;
      if (requiredsize <= sizeof(local_buffer) / sizeof(Ushort) ||
	  (kanjis = (Ushort *)malloc(requiredsize * sizeof(Ushort)))) {
	ir_debug( Dmsg(5, "maxyomi [%d]\n", requiredsize) );

	maxyomi = MIN( req->yomilen, ushortstrlen( req->yomi ) ) ;
	stat = RkwGetSimpleKanji(cxnum, req->dicname, req->yomi, maxyomi,
				kanjis, (int)Request.type13.kouhosize,
				kanjis + (int)Request.type13.kouhosize,
				(int)Request.type13.hinshisize);
	if (stat >= 0) {
	  size = unamesize2(kanjis);
	  bcopy(kanjis + (int)Request.type13.kouhosize, kanjis + size,
		size * sizeof(Ushort));
	  size += unamesize2(kanjis);
	  size += SIZEOFSHORT; /* 最初の候補数の部分 */
	}
      }
    }
    retval = SendType7Reply(client, wGetSimpleKanji, !EXTPROTO,
			    stat, size, kanjis);
    if (kanjis != (Ushort *)local_buffer) free((char *)kanjis);
    return retval;
}

static
irw_query_dictionary( clientp )
ClientPtr *clientp ;
{
  ClientPtr client = *clientp ;
  int cxnum = Request.type15.context, stat = -1;
  struct DicInfo *dicinfo = (struct DicInfo *)local_buffer;
  char *username, *usernamelong = (char *)0;	
  int retval;    /* S003 */  
  /* ここから */
  if (client->version_hi > 2 &&
      validcontext(cxnum, client, wQueryDictionary)) {
    dicinfo = (struct DicInfo *)malloc(sizeof(local_buffer));
    if (dicinfo) {
      username = Request.type15.dicname + strlen(Request.type15.dicname) + 1;
      if (username[0] != ':' ||
	  canna_version(client->version_hi, client->version_lo) <
	  canna_version(3, 1)) {
	/* 昔のプロトコルではディレクトリ名に user/ が付いていないので
	   それの対応(W_PROTO 3.0 以前) */
	usernamelong = insertUserSla(username, strlen(username));
	if (usernamelong) {
	  username = usernamelong;
	}
	else {
	  goto querydicdone;
	}
      }
      else {
	username++;
      }
      
      /* ここまで */
      ir_debug( Dmsg(5, "dicname = %s\n", Request.type15.dicname) );
      ir_debug( Dmsg(5, "username = %s\n", username) );
      
      stat = (RkwQueryDic(cxnum, (unsigned char *)username,
			  (unsigned char *)Request.type15.dicname,
			  dicinfo) < 0) ? -1 : 0;
      
      if (stat < 0) {
	dicinfo->di_dic = dicinfo->di_file = (unsigned char *)"";
      }

      ir_debug(Dmsg(5, "dic = %s\n", dicinfo->di_dic));
      ir_debug(Dmsg(5, "file = %s\n", dicinfo->di_file));
      
    }
    else {
      dicinfo->di_dic = dicinfo->di_file = (unsigned char *)"";
    }
  querydicdone:
    if (usernamelong) {
      free(usernamelong);
    }
    ir_debug( Dmsg(5, "辞書リスト\n") );
  }
  retval = SendType4XReply(client, wQueryDictionary, !EXTPROTO, stat,
			   (char *)dicinfo->di_dic, (char *)dicinfo->di_file,
			   (sizeof(struct DicInfo) - 2 * sizeof(char *)) /
			   sizeof(int),
			   (int *)&dicinfo->di_kind);
  if (dicinfo != (struct DicInfo *)local_buffer) free(dicinfo);
  return retval;
}

static
irw_get_hinshi( clientp )
ClientPtr *clientp ;
{
    wReq8 *req = &Request.type8 ;
    ClientPtr client = *clientp ;
    Ushort *dst = (Ushort *)local_buffer;
    int cxnum = Request.type8.context, retval;
    int stat = -1, requiredsize = Request.type8.size;

    if (validcontext(cxnum, client, wGetHinshi)) {
      if (requiredsize <= sizeof(local_buffer) / sizeof(Ushort) ||
	  (dst = (Ushort *)malloc(requiredsize * sizeof(Ushort)))) {
	ir_debug( Dmsg(5, "品詞情報を得る候補\n") );

	RkwGoTo(cxnum, (int)req->curbun);
	if ((int)req->curkouho != RkwXfer(cxnum, (int)req->curkouho)) {
	    PrintMsg("[%s](%s) irw_get_hinshi: RkwXfer failed\n",
		     client->username, WideProtoName[wGetHinshi - 1]);
	}

	stat = RkwGetHinshi(cxnum, dst, requiredsize);
      }
    }
    retval = SendType7Reply(client, wGetHinshi, !EXTPROTO, stat,
			    stat > 0 ? stat + 1 : 0, dst);
    if (dst != (Ushort *)local_buffer) free((char *)dst);
    return retval;
}

static
irw_store_range( clientp )
ClientPtr *clientp ;
{
    wReq11 *req = &Request.type11 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type11.context, curbun, maxbun, maxyomi, stat ;
    int size = 0, ret = -1;
    Ushort *bufp = (Ushort *)local_buffer;

    if (validcontext(cxnum, client, wStoreRange)) {
	RkwGoTo( cxnum, 0 );
	maxbun = RkwLeft( cxnum ) + 1;
	curbun = (int)req->curbun;
	RkwGoTo(cxnum, curbun);

	maxyomi = ushortstrlen( req->yomi );
	if ((ret = RkwStoreRange(cxnum, req->yomi, maxyomi)) < 0) { 
	    PrintMsg( "[%s](%s) kana-kanji convert failed\n",
		     client->username, WideProtoName[wStoreRange - 1]);
	} else {
	    /* 最優先候補を取得する */
            size = GetFirstKouho(cxnum, curbun, maxbun, &stat,
				 (Ushort *)local_buffer,
                                 sizeof(local_buffer) / sizeof(Ushort), &bufp);
	}
    }
    ret = SendType7Reply(client, wStoreRange, !EXTPROTO, stat, size, bufp);
    if (bufp != (Ushort *)local_buffer) free((char *)bufp);
    return ret;
}

static
irw_set_locale( clientp )
ClientPtr *clientp ;
{
    wReq15 *req = &Request.type15 ;
    ClientPtr client = *clientp ;
    int cxnum = Request.type15.context, stat = -1 ;

    if (validcontext(cxnum, client, wSetLocale)) {
	stat = RkwSetLocale(cxnum, (unsigned char *)req->dicname);
    }
    return SendType2Reply(client, wSetLocale, !EXTPROTO, stat);
}

static
irw_set_app_name( clientp )
ClientPtr *clientp;
{
    wReq15 *req = &Request.type15;
    ClientPtr client = *clientp;
    int cxnum = Request.type15.context, stat = -1;
    char *newname;

    if (validcontext(cxnum, client, wSetApplicationName)) {
      newname = malloc(strlen(req->dicname) + 1);
      if (newname) {
	strcpy(newname, req->dicname) ;
	if (client->clientname) {
	  free( client->clientname );
	}
	client->clientname = newname;
	stat = 0;
      }
    }
    return SendType2Reply(client, wSetApplicationName, !EXTPROTO, stat);
}
							/* S000:begin */
static
irw_notice_group_name(clientp)
ClientPtr *clientp;
{
  wReq15 *req = &Request.type15;
  ClientPtr client = *clientp;
  int cxnum = Request.type15.context, stat = -1;
  char *newname;

  ir_debug(Dmsg( 3,"group name = \"%s\"\n", req->dicname));

  newname = malloc(strlen(req->dicname) + 1);
  if (newname) {
    strcpy(newname, req->dicname) ;
    if (client->groupname) {
      free(client->groupname);
    }
    client->groupname = newname;
    stat = 0;

    if (SetDicHome(client, cxnum) <= 0) {
      stat = -1;
      PrintMsg("[%s](%s) Can't set dictionary home\n",
	       client->username, WideProtoName[wCreateContext - 1]);
    }
  }
  return SendType2Reply(client, wNoticeGroupName, !EXTPROTO, stat);
}
							/* S000:begin */
static
irw_through( clientp )
ClientPtr *clientp;
{
  ClientPtr client = *clientp;
  int cxnum = Request.type20.context, content_size, size = 0, stat = -1;
  int retval;
  char *buf = (char *)0;

  if (validcontext(cxnum, client, wThrough)) {
    buf = malloc((int)Request.type20.bufsize);
    if (buf) {
      content_size = Request.type20.datalen - (SIZEOFINT * 2 + SIZEOFSHORT);
      bcopy(Request.type20.buf, buf, content_size);
      stat = size = RkThrough(cxnum, Request.type20.command,
			      buf, content_size, (int)Request.type20.bufsize);
    }
  }
  retval = SendType6Reply(client, wThrough, EXTPROTO, stat, buf, size);
  if (buf) free(buf);
  return retval;
}							/* S000:end */

static
WriteClient( ClientFD, buf, size )
int ClientFD ;
BYTE *buf ;
int size ;
{
    register int write_stat ;
    register char *bufindex = (char *)buf;
    register int todo = size ;

    ir_debug( Dmsg(10, "WriteClient:") );
    ir_debug( DebugDump( 10, (char *)buf, size ) );
    ir_debug(probe("Write: %d\n", size, buf));

    while ( size > 0 ) {
	errno = 0;
	write_stat = write( ClientFD, bufindex, todo ) ;
	if (write_stat >= 0) {
	    size -= write_stat;
	    todo = size;
	    bufindex += write_stat;
	    continue ;
	} else if (errno == EWOULDBLOCK) {   /* pc98 */
	    continue ;
#ifdef EMSGSIZE
	} else if (errno == EMSGSIZE) {
	    if (todo > 1)
		todo >>= 1;
	    else
		continue ;
#endif
	} else {
	    /* errno set by write system call. */
	    PrintMsg( "Write Error[ %d ]\n", errno ) ;
	    return( -1 ) ;
	}
    }

    return( 0 ) ;
}

/*
 * もともとio.cに入れいていたものをここから下に置く
 */

#define READ_SIZE	    2048
#define SIZE4	4   /* sizeof( int ) */
#define SIZE8	8
#define TRY_COUNT   10
#define DATALEN_TOP (sizeof( char ) * 2)

static BYTE ReadRequestBuffer[ READ_SIZE ] ;	/* デフォルトバッファ */

static int  ReadSize = READ_SIZE ;     /* バッファサイズ */

static BYTE *readbufptr = ReadRequestBuffer ;	/* デフォルトバッファ */

int (* CallFunc)() ;

ReadWideRequestFromClient(who, status )
ClientPtr who;
int *status;	      /* read at least n from client */
{
    extern void ClientStat();
    int (* ReqCallFunc)() ;
#ifdef DEBUG
    extern char *DebugProcWide[][2] ;
#endif
    BYTE *bufptr = ReadRequestBuffer, *p;
    register wReq1 *req = &Request.type1 ;
    int client = who->id;
    int     readsize ;		    /* 一回に読み込んだサイズ */
    int     bufsize = ReadSize ;   /* バッファサイズ */
    int     bufcnt = 0 ;	    /* 読み込んだ累積サイズ */
    int     empty_count = 0 ;
    int requiredsize = HEADER_SIZE, rest = requiredsize;

    if( readbufptr != ReadRequestBuffer ) {	   /* 前回バッファを大きく */
	ir_debug( Dmsg(8, "free readbufptr.\n") ); /* した場合は，デフォルト*/

	free((char *)readbufptr);		   /* に戻す */
	readbufptr = ReadRequestBuffer ;
	ReadSize = READ_SIZE ;
    }

    while (empty_count < TRY_COUNT && rest > 0) {
	if ((readsize = read(client, (char *)bufptr, rest)) < 0) {
	  break;
	} else if ( !readsize ) {	
	    empty_count ++ ;
	    continue ;
	}
	ir_debug( Dmsg(10, "NewReadRequest:") );
	ir_debug( DebugDump( 10, (char *)bufptr, readsize ) );

	empty_count = 0;
	bufcnt += readsize;
	bufptr += readsize;
	rest -= readsize;
	
	if (requiredsize == HEADER_SIZE && HEADER_SIZE == bufcnt) {
	  p = readbufptr;
	  req->type = *p++;
	  req->none = *p++;
	  req->datalen = requiredsize = S2TOS(p);
	  rest += requiredsize;
	  requiredsize += HEADER_SIZE;
	  if (requiredsize > bufsize) {
	    readbufptr = (BYTE *)malloc(requiredsize);
	    if (readbufptr) {
	      bcopy(ReadRequestBuffer, readbufptr, bufcnt);
	      rest = requiredsize - bufcnt;
	      bufptr = readbufptr + bufcnt;
	      bufsize = requiredsize;
	    }
	    else {
	      break;
	    }
	  }
        }
    }
    if (rest > 0) {
      PrintMsg("[%s] Read request failed\n", who->username);
      ir_debug(Dmsg(5, "ReadWideRequestFromClient: Read request failed\n"));
      *status = -1;
      return 0;
    }

    ir_debug( Dmsg(5, "Client: <%s@%s> [0x%x:0x%x]\n",
		   (who->username)?(who->username):null,
		   (who->hostname)?(who->hostname):null,
		   req->type, req->none) );

    if( ((req->type > W_REALREQUEST) && !(req->none)) ||
       ((req->type > W_MAXEXTREQUESTNO) && req->none) ) {
      PrintMsg( "[%s] Request error[%d]\n", who->username, req->type ) ;
      *status = -1 ;
      return( 0 ) ;
    }
	
    /* プロトコルのタイプ毎にデータを呼んでくる関数を呼ぶ */
    if( req->none ) {
      ir_debug( Dmsg(8, "Now Call EXTENSION\n") );

#ifdef EXTENSION
      ReqCallFunc = ExtensionWideVector[req->type].extdat;
      CallFunc = ExtensionWideVector[req->type].func;
#else
      PrintMsg( "[%s] Request error[%d]\n", who->username, req->type ) ;
      SendType5Reply(client,
		     (int)Request.type1.type, (int)Request.type1.none, -1);
      *status = -1 ;
      return( 0 ) ;
#endif /* EXTENSION */
    }
    else {
	ir_debug( Dmsg(8, "Now Call %s\n", DebugProcWide[req->type][1]) );

	ReqCallFunc = WideVector[req->type].extdat;
	CallFunc = WideVector[req->type].func;
    }
    if ((* ReqCallFunc)(readbufptr)  < 0) {
      /* マイナスを返すことはない */
      PrintMsg( "[%s] Read Data failed\n", who->username ) ;
      *status = -1 ;
      return( 0 ) ;	
    }

    /* プロトコルの種類毎に統計を取る */
    if( req->type < (unsigned)W_MAXREQUESTNO ) {		/* S000 */
	who->pcount[ req->type ] ++ ;
	TotalWideRequestTypeCount[ req->type ] ++ ;
    }

    ClientStat(who, SETTIME, (int)req->type, 0);

    *status = 1 ;
    return (int)req->type;
}

static
ProcWideReq1(buf)
BYTE *buf ;
/* ARGSUSED */
{
    ir_debug( Dmsg(10, "ProcWideReq1 start!!\n") );

    return( 0 ) ;
}

static
ProcWideReq2(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq2 start!!\n") );

    buf += HEADER_SIZE; Request.type2.context = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type2.context) );

    return( 0 ) ;
}

static
ProcWideReq3(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq3 start!!\n") );

    buf += HEADER_SIZE; Request.type3.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type3.buflen = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type3.context) );
    ir_debug( Dmsg(10, "req->buflen =%d\n", Request.type3.buflen) );

    return( 0 ) ;
}

static
ProcWideReq4(buf)
BYTE *buf ;
{
    register Ushort *data;
    int i, len ;

    ir_debug( Dmsg(10, "ProcWideReq4 start!!\n") );

    buf += HEADER_SIZE; Request.type4.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type4.begin = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type4.end = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type4.yomilen = S2TOS(buf);
    Request.type4.yomi = (Ushort *)(buf += SIZEOFSHORT) ;
    len = Request.type4.datalen - SIZEOFSHORT * 4;
    for (data = Request.type4.yomi, i = 0; i < len; i++, data++)
	*data = ntohs((unsigned short)*data); /* ちょっとやだなあ */
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type4.context) );
    ir_debug( Dmsg(10, "req->begin =%d\n", Request.type4.begin) );
    ir_debug( Dmsg(10, "req->end =%d\n", Request.type4.end) );
    ir_debug( Dmsg(10, "req->yomilen =%d\n", Request.type4.yomilen) );
    ir_debug( Dmsg(10, "req->yomi =%s\n",
		   (Request.type4.yomi)?conveuc(Request.type4.yomi):
		   null) );

    return( 0 ) ;
}

static
ProcWideReq5(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq5 start!!\n") );

    buf += HEADER_SIZE; Request.type5.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type5.size = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type5.mode = L4TOL(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type5.context) );
    ir_debug( Dmsg(10, "req->size =%d\n", Request.type5.size) );
    ir_debug( Dmsg(10, "req->mode =%d\n", Request.type5.mode) );

    return( 0 ) ;
}
							/* S000:begin */
static
ProcWideReq6(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq6 start!!\n") );

    buf += HEADER_SIZE; Request.type6.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type6.number = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type6.buflen = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type6.context) );
    ir_debug( Dmsg(10, "req->number =%d\n", Request.type6.number) );
    ir_debug( Dmsg(10, "req->buflen =%d\n", Request.type6.buflen) );

    return( 0 ) ;
}

static
ProcWideReq7(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq7 start!!\n") );

    buf += HEADER_SIZE; Request.type7.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type7.number = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type7.yomilen = (short)S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type7.context) );
    ir_debug( Dmsg(10, "req->number =%d\n", Request.type7.number) );
    ir_debug( Dmsg(10, "req->yomilen =%d\n", Request.type7.yomilen) );

    return( 0 ) ;
}

static
ProcWideReq8(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq8 start!!\n") );

    buf += HEADER_SIZE; Request.type8.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type8.curbun = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type8.curkouho = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type8.size = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type8.context) );
    ir_debug( Dmsg(10, "req->curbun =%d\n", Request.type8.curbun) );
    ir_debug( Dmsg(10, "req->curkouho =%d\n", Request.type8.curkouho) );
    ir_debug( Dmsg(10, "req->size =%d\n", Request.type8.size) );

    return( 0 ) ;
}

static
ProcWideReq9(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq9 start!!\n") );

    buf += HEADER_SIZE; Request.type9.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type9.number = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type9.kouho = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type9.max = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type9.context) );
    ir_debug( Dmsg(10, "req->number =%d\n", Request.type9.number) );
    ir_debug( Dmsg(10, "req->kouho =%d\n", Request.type9.kouho) );
    ir_debug( Dmsg(10, "req->max =%d\n", Request.type9.max) );

    return( 0 ) ;
}

static
ProcWideReq10(buf)
BYTE *buf ;
{
    register int i ;

    ir_debug( Dmsg(10, "ProcWideReq10 start!!\n") );

    buf += HEADER_SIZE; Request.type10.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type10.number = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type10.mode = L4TOL(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type10.context) );
    ir_debug( Dmsg(10, "req->number =%d\n", Request.type10.number) );
    ir_debug( Dmsg(10, "req->mode =%d\n", Request.type10.mode) );

    buf += SIZEOFINT; Request.type10.kouho = (short *)buf; /* short? */
    for (i = 0; i < Request.type10.number; i++) {
      Request.type10.kouho[i] = S2TOS(buf); buf += SIZEOFSHORT;
      ir_debug(Dmsg(10, "req->kouho =%d\n", Request.type10.kouho[i]));
    }

    return( 0 ) ;
}

static
ProcWideReq11(buf)
BYTE *buf ;
{
    register Ushort *data;
    int i, len ;

    ir_debug( Dmsg(10, "ProcWideReq10 start!!\n") );

    buf += HEADER_SIZE; Request.type11.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type11.curbun = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type11.yomi = (Ushort *)buf;
    len = ((int)Request.type11.datalen - SIZEOFSHORT * 2) / SIZEOFSHORT ;
    for (data = Request.type11.yomi, i = 0; i < len; i++, data++)
	*data = ntohs( *data ); /* なんかやだ */
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type11.context) );
    ir_debug( Dmsg(10, "req->curbun =%d\n", Request.type11.curbun) );
    ir_debug( Dmsg(10, "req->yomi =%s\n",
		   (Request.type11.yomi)?conveuc(Request.type11.yomi):
		   null));

    return( 0 ) ;
}

static
ProcWideReq12(buf)
BYTE *buf ;
{
    register Ushort *data;
    int i, len ;

    ir_debug( Dmsg(10, "ProcWideReq12 start!!\n") );

    buf += HEADER_SIZE; Request.type12.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type12.datainfo = (Ushort *)buf;
    len = ushortstrlen((Ushort *)buf) + 1;
    for( data = Request.type12.datainfo, i = 0; i < len; i++, data++ )
	*data = ntohs( *data ); /* なんかやだ */
    buf += len * SIZEOFSHORT;
    Request.type12.dicname = (char *)buf;
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type12.context) );
    ir_debug( Dmsg(10, "req->datainfo =%s\n",
		   (Request.type12.datainfo)?
		   conveuc(Request.type12.datainfo):
		   null));
    ir_debug( Dmsg(10, "req->dicname =%s\n",
		   (Request.type12.dicname)?Request.type12.dicname:null) );

    return( 0 ) ;
}

static
ProcWideReq13(buf)
BYTE *buf ;
{
    register Ushort *data;
    int i ,len ;

    ir_debug( Dmsg(10, "ProcWideReq13 start!!\n") );

    buf += HEADER_SIZE; Request.type13.context = S2TOS(buf);
    len = SIZEOFSHORT ;
    buf += len;
    Request.type13.dicname = (char *)buf;
    len = strlen( (char *)buf ) + 1;
    buf += len;
    Request.type13.yomi = (Ushort *)buf;
    len = ((int)Request.type13.datalen - len - SIZEOFSHORT * 4) / SIZEOFSHORT;
    for( data = Request.type13.yomi, i = 0; i < len; i++, data++)
	*data = ntohs( *data );
    buf += (ushortstrlen((Ushort *)buf) + 1) * SIZEOFSHORT;
    Request.type13.yomilen = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type13.kouhosize = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type13.hinshisize = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type13.context) );
    ir_debug( Dmsg(10, "req->dicname =%s\n", Request.type13.dicname) );
    ir_debug( Dmsg(10, "req->yomi =%s\n",
		   (Request.type13.yomi)?conveuc(Request.type13.yomi):
		   null));
    ir_debug( Dmsg(10, "req->yomilen =%d\n", Request.type13.yomilen) );
    ir_debug( Dmsg(10, "req->kouhosize =%d\n", Request.type13.kouhosize) );
    ir_debug( Dmsg(10, "req->hinshisize =%d\n", Request.type13.hinshisize) );

    return( 0 ) ;
}

static
ProcWideReq14(buf)
BYTE *buf ;
{
    register Ushort *data;
    int i, len ;

    ir_debug( Dmsg(10, "ProcWideReq14 start!!\n") );

    buf += HEADER_SIZE; Request.type14.mode = L4TOL(buf);
    buf += SIZEOFINT;   Request.type14.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type14.yomi = (Ushort *)buf;
    len = ((int)Request.type14.datalen - SIZEOFSHORT - SIZEOFINT)
      / SIZEOFSHORT;
    for (data = Request.type14.yomi, i = 0; i < len; i++, data++)
	*data = ntohs( *data ); /* なんかやだ */

    ir_debug( Dmsg(10, "req->mode =%d\n", Request.type14.mode) );
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type14.context) );
    ir_debug( Dmsg(10, "req->yomi =%s\n",
		   (Request.type14.yomi)?conveuc(Request.type14.yomi):
		   null));

    return( 0 ) ;
}

static
ProcWideReq15(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq15 start!!\n") );

    buf += HEADER_SIZE; Request.type15.mode = L4TOL(buf);
    buf += SIZEOFINT;   Request.type15.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type15.dicname = (char *)buf;
    ir_debug( Dmsg(10, "req->mode =%d\n", Request.type15.mode) );
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type15.context) );
    ir_debug( Dmsg(10, "req->dicname =%s\n",
		   (Request.type15.dicname)?Request.type15.dicname:null) );

    return( 0 ) ;
}

static
ProcWideReq17(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq17 start!!\n") );

    buf += HEADER_SIZE;
    Request.type17.dicname = (char *)buf;
    Request.type17.mode = (char)*(buf + Request.type17.datalen - SIZEOFCHAR) ;
    ir_debug( Dmsg(10, "req->dicname =%s\n",
		   (Request.type17.dicname)?Request.type17.dicname:null) );
    ir_debug( Dmsg(10, "req->mode =%d\n", Request.type17.mode) );

    return( 0 ) ;
}

#ifdef EXTENSION
static
ProcWideReq18(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq18 start!!\n") );

    buf += HEADER_SIZE; Request.type18.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type18.data = (char *)buf;
    buf += Request.type18.datalen - SIZEOFSHORT * 2;
    Request.type18.size = S2TOS(buf);
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type18.context) );
    ir_debug( Dmsg(10, "req->data =%s\n",
		   (Request.type18.data)?Request.type18.data:null) );
    ir_debug( Dmsg(10, "req->size =%d\n", Request.type18.size) );

    return( 0 ) ;
}
#endif /* EXTENSION */

static
ProcWideReq19(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq19 start!!\n") );

    buf += HEADER_SIZE; Request.type20.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type20.command = L4TOL(buf);
    buf += SIZEOFINT;   Request.type20.bufsize = L4TOL(buf);
    buf += SIZEOFINT;   Request.type20.buf = (char *)buf;
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type20.context) );
    ir_debug( Dmsg(10, "req->command =%d\n", Request.type20.command) );
    ir_debug( Dmsg(10, "req->bufsize =%d\n", Request.type20.bufsize) );

    return( 0 ) ;
}							/* S000:end */
/* Copy Dic のため */
static
ProcWideReq20(buf)
BYTE *buf ;
{
    ir_debug( Dmsg(10, "ProcWideReq20 start!!\n") );

    buf += HEADER_SIZE; Request.type21.mode = L4TOL(buf);
    buf += SIZEOFINT;   Request.type21.context = S2TOS(buf);
    buf += SIZEOFSHORT; Request.type21.dirname = (char *)buf;
    buf += strlen((char *)buf) + 1;
    Request.type21.srcdic = (char *)buf;
    buf += strlen((char *)buf) + 1;
    Request.type21.dstdic = (char *)buf;

    ir_debug( Dmsg(10, "req->mode =%d\n", Request.type21.mode) );
    ir_debug( Dmsg(10, "req->context =%d\n", Request.type21.context) );
    ir_debug( Dmsg(10, "req->dirname =%s\n",
		   (Request.type21.dirname)?Request.type21.dirname:null) );
    ir_debug( Dmsg(10, "req->srcdic =%s\n",
		   (Request.type21.srcdic)?Request.type21.srcdic:null) );
    ir_debug( Dmsg(10, "req->dstdic =%s\n",
		   (Request.type21.dstdic)?Request.type21.dstdic:null) );

    return( 0 ) ;
}
/* ここまで */

#ifdef WIDE_PROTO

RkwSubstYomi( cxnum, ys, ye, yomi, nyomi )
int cxnum, ys, ye, nyomi;
Ushort *yomi;
{
    RkwEndBun( cxnum, 0 );
    return( RkwBgnBun( cxnum, yomi, nyomi, 0) );
}

RkwFlushYomi( cxnum )
int cxnum;
{
    RkwEndBun( cxnum, 0 );
    return( RkwBgnBun( cxnum, "フラッシュ読み", 14, 0) );
}

RkwGetLastYomi( cxnum, yomi, maxyomi )
int cxnum, maxyomi;
Ushort *yomi ;
{
    return( euc2ushort("未決文節", 8, yomi, maxyomi) );
}

RkwRemoveBun( cxnum, mode )
int cxnum, mode;
{
    return( 0 );
}

RkwSetLocale( cxnum, locale )
int cxnum;
char *locale;
{
    return( 0 );
}

static unsigned char kouho[] = "テスト候補";
static unsigned char hinshi[] = "#T35 テスト品詞";

RkwGetSimpleKanji( cxnum, dicname, yomi, maxyomi, kanjis, maxkanjis, hinshis, maxhinshis )
int cxnum, maxyomi, maxkanjis, maxhinshis;
char *dicname;
Ushort *yomi, *kanjis, *hinshis;
{
   Dmsg( 5,"RkwGetSimpleKanji( %d, %s, %s, %d, kanjis, %d, hinshis, %d )\n",
	cxnum, dicname, conveuc(yomi), maxyomi, maxkanjis, maxhinshis );
    euc2ushort( kouho, strlen( kouho ), kanjis, maxkanjis ) ;
    euc2ushort( hinshi, strlen( hinshi ), hinshis, maxhinshis ) ;

    return( 1 ) ;
}

#ifdef pcux_r32
struct DicInfo *
#endif /* pcux_r32 */
RkwQueryDic( cxnum, dicname, status )
int cxnum;
char *dicname;
struct DicInfo *status;
{
    ir_debug( Dmsg(5, "RkwQueryDic( %d, %s, status )\n", cxnum, dicname) );

    return( status ) ;
}

RkwGetHinshi( cxnum, dst, maxdst )
int cxnum, maxdst;
Ushort *dst;
{
    ir_debug( Dmsg(5, "RkwGetHinshi( %d, dst, %d )\n", cxnum, maxdst) );
    euc2ushort( "#T35 テスト", strlen("#T35 テスト"), dst, maxdst ) ;

    return( 0 ) ;
}

RkwStoreRange( cxnum, yomi, maxyomi )
int cxnum, maxyomi;
Ushort *yomi;
{
    ir_debug( Dmsg(5, "RkwStoreRange( %d, yomi, %d )\n", cxnum, maxyomi) );

    return( 0 ) ;
}

#endif /* WIDE_PROTO */

#ifdef DEBUG
char dest[CBUFSIZE];

static char *
conveuc(src)
Ushort *src;
{
    ushort2euc(src, ushortstrlen(src), dest, CBUFSIZE);
    return(dest);
}
#endif /* DEBUG */
							/* S000:begin */
/* #ifdef DEBUG_TOOL */
RkThrough( cx, command, buf, content_size, buffer_size )
int cx, command, content_size, buffer_size;
char *buf;
/* ARGSUSED */
{
    int i;
    for( i = 0; i < content_size; i++ )
	buf[i]++;
    ir_debug( Dmsg(5, "RkThrough: %d\n", content_size) );
    return( content_size );
}
/* #endif *//* DEBUG_TOOL */					/* S000:end */

extern int ir_error(), ir_initialize(), ProcReq0();

reqproc WideVector[] =
{
/* 0x00 */	{ ir_error,		   ProcReq0 },
/* 0x01 */	{ ir_initialize,	   ProcReq0 },
/* 0x02 */	{ irw_finalize,		   ProcWideReq1 },
/* 0x03 */	{ irw_create_context,	   ProcWideReq1 },
/* 0x04 */	{ irw_duplicate_context,   ProcWideReq2 },
/* 0x05 */	{ irw_close_context,	   ProcWideReq2 },
/* 0x06 */	{ irw_dictionary_list,	   ProcWideReq3 },
/* 0x07 */	{ irw_get_dir_list,	   ProcWideReq3 },
/* 0x08 */	{ irw_mount_dictionary,	   ProcWideReq15 },
/* 0x09 */	{ irw_umount_dictionary,   ProcWideReq15 },
/* 0x0a */	{ irw_rmount_dictionary,   ProcWideReq15 },
/* 0x0b */	{ irw_mount_list,	   ProcWideReq3 },
/* 0x0c */	{ irw_query_dictionary,	   ProcWideReq15 },
/* 0x0d */	{ irw_define_dic,	   ProcWideReq12 },
/* 0x0e */	{ irw_delete_dic,	   ProcWideReq12 },
/* 0x0f */	{ irw_convert,		   ProcWideReq14 },
/* 0x10 */	{ irw_convert_end,	   ProcWideReq10 },
/* 0x11 */	{ irw_get_kanjilist,	   ProcWideReq6 },
/* 0x12 */	{ irw_get_yomi,		   ProcWideReq6 },
/* 0x13 */	{ irw_subst_yomi,	   ProcWideReq4 },	
/* 0x14 */	{ irw_store_yomi,	   ProcWideReq11 },	
/* 0x15 */	{ irw_store_range,	   ProcWideReq11 },	
/* 0x16 */	{ irw_get_last_yomi,	   ProcWideReq3 },	
/* 0x17 */	{ irw_flush_yomi,	   ProcWideReq10 },	
/* 0x18 */	{ irw_remove_yomi,	   ProcWideReq10 },	
/* 0x19 */	{ irw_get_simple_kanji,	   ProcWideReq13 },	
/* 0x1a */	{ irw_resize,		   ProcWideReq7 },	
/* 0x1b */	{ irw_get_hinshi,	   ProcWideReq8 }, 
/* 0x1c */	{ irw_get_lex,		   ProcWideReq9 }, 
/* 0x1d */	{ irw_get_stat,		   ProcWideReq7 },	
/* 0x1e */	{ irw_set_locale,	   ProcWideReq15 }, 
/* 0x1f */	{ irw_autoconv,		   ProcWideReq5 }, 
/* 0x20 */	{ irw_query_extension,	   ProcWideReq17 },
/* 0x21 */	{ irw_set_app_name,	   ProcWideReq15 },
/* 0x22 */	{ irw_notice_group_name,   ProcWideReq15 },
/* 0x23 */	{ irw_through,		   ProcWideReq19 },	/* S000 */
} ;

char *ExtensionRequest[] = {
    /* Request Name */
#ifdef EXTENSION
    "GetServerInfo",		/* 0x01 */
    "GetAccessControlList",	/* 0x02 */
    "CreateDictioinary",	/* 0x03 */
    "DeleteDictioinary",	/* 0x04 */
    "RenameDictioinary",	/* 0x05 */
    "GetWordTextDictioinary",	/* 0x06 */
    "ListDictioinary",		/* 0x07 */
    "Sync",		        /* 0x08 */
    "ChmodDictioinary",         /* 0x09 */
    "CopyDictioinary",          /* 0x0a */
#endif /* EXTENSION */
    ""
} ;

#ifdef EXTENSION
reqproc ExtensionWideVector[] =
{
/* 0x00 */	{ ir_error,		   ProcReq0 },
/* 0x01 */	{ irw_server_stat,	   ProcWideReq1 },		
/* 0x02 */	{ irw_host_ctl,		   ProcWideReq1 },
/* 0x03 */	{ irw_create_dictionary,   ProcWideReq15 },
/* 0x04 */	{ irw_remove_dictionary,   ProcWideReq15 },
/* 0x05 */	{ irw_rename_dictionary,   ProcWideReq15 },
/* 0x06 */	{ irw_get_word_text_dic,   ProcWideReq18 },
/* 0x07 */	{ irw_list_dictionary,	   ProcWideReq18 },
/* 0x08 */	{ irw_sync,	           ProcWideReq15 },
/* 0x09 */	{ irw_chmod_dictionary,    ProcWideReq15 },
/* 0x0a */	{ irw_copy_dictionary,     ProcWideReq20 },
} ;
#endif /* EXTENSION */


char *WideProtoName[] = {
    "Initialize",
    "Finalize",
    "CreateContext",
    "DupricateContext",
    "CloseContext",
    "GetDictionaryList",
    "GetDirectoryList",
    "MountDictionary",
    "UnmountDictionary",
    "RemountDictionary",
    "GetMountDictionaryList",
    "QueryDictionary",
    "DefineWord",
    "DeleteWord",
    "BeginConvert",
    "EndConvert",
    "GetCandidacyList",
    "GetYomi",
    "SubstYomi",
    "StoreYomi",
    "StoreRange",
    "GetLastYomi",
    "FlushYomi",
    "RemoveYomi",
    "GetSimpleKanji",
    "ResizePause",
    "GetHinshi",
    "GetLex",
    "GetStatus",
    "SetLocale",
    "AutoConvert",
    "QueryExtensions",
    "SetApplicationName",
    "NoticeGroupName",
    "Through"							/* S000 */
} ;			

#ifdef DEBUG
char *DebugProcWide[][2] = {
    { "ir_null",		"ProcReq0" } ,
    { "ir_initialize",		"ProcReq0" },
    { "irw_finalize",		"ProcReq1" },
    { "irw_create_context",	"ProcReq1" },
    { "irw_duplicate_context",	"ProcReq2" },	
    { "irw_close_context",	"ProcReq2" },	
    { "irw_dictionary_list",	"ProcReq3" },	
    { "irw_get_dir_list",	"ProcReq3" },	
    { "irw_mount_dictionary",	"ProcReq15" },	
    { "irw_umount_dictionary",	"ProcReq15" },	
    { "irw_rmount_dictionary",	"ProcReq15" },	
    { "irw_mount_list",		"ProcReq3" },	
    { "irw_query_dictionary",	"ProcReq15" },
    { "irw_define_dic",		"ProcReq12" },	
    { "irw_delete_dic",		"ProcReq12" },	
    { "irw_convert",		"ProcReq14" },	
    { "irw_convert_end",	"ProcReq10" },	
    { "irw_get_kanjilist",	"ProcReq6" },	
    { "irw_get_yomi",		"ProcReq6" },	
    { "irw_subst_yomi",		"ProcReq4" },	
    { "irw_store_yomi",		"ProcReq11" },	
    { "irw_store_range",	"ProcReq11" },	
    { "irw_get_last_yomi",	"ProcReq3" },	
    { "irw_flush_yomi",		"ProcReq10" },	
    { "irw_remove_yomi",	"ProcReq10" },	
    { "irw_get_simple_kanji",	"ProcReq13" },	
    { "irw_resize",		"ProcReq7" },	
    { "irw_get_hinshi",		"ProcReq8" }, 
    { "irw_get_lex",		"ProcReq9" }, 
    { "irw_get_stat",		"ProcReq7" },	
    { "irw_set_locale",		"ProcReq15" }, 
    { "irw_autoconv",		"ProcReq5" }, 
    { "irw_query_extension",	"ProcReq17" },
    { "irw_set_app_name",	"ProcReq15" },
    { "irw_notice_group_name",	"ProcReq15" },
    { "irw_through",		"ProcReq19" }			/* S000 */
} ;			
#endif

/*

 おぼえ書き

 ・コンテキストの確認処理はいらないんじゃないの？あるいは間違っている時は
   BADCONT を返すべきではないの？

 */
