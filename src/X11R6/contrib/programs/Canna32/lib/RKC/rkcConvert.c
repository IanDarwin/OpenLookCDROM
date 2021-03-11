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
static char rcs_id[] = "$Id: rkcConvert.c,v 6.11 1994/04/21 02:32:07 kon Exp $";
#endif

/* LINTLIBRARY */

#include "rkcw.h"
#include "canna/RK.h"
#include "rkc.h"
#include "sglobal.h"
#include "IRproto.h"

#include <errno.h>
#include <sys/types.h>
#include <signal.h>

#if defined(USG) || defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif

/* 単語登録で辞書が作れなくなるので、とりあえずコメントアウト
#if CANNA_LIGHT
#ifdef EXTENSION
#undef EXTENSION
#endif
#endif
 */

#ifdef USE_EUC_PROTOCOL

extern int   errno ;

extern int ServerFD ;

#define SENDBUFSIZE 1024
#define RECVBUFSIZE 1024

#define PROTOBUF (16 * 8)

#define TRY_COUNT	    10

#define RkcFree( p )	{ if( (p) ) (void)free( (char *)(p) ) ; }

#ifdef LESS_SPACE_IS_IMPORTANT
#undef LTOL4
static void
LTOL4(l, p)
int l;
BYTE *p;
{
  p[0] = (l >> 24) & 0xff;
  p[1] = (l >> 16) & 0xff;
  p[2] = (l >> 8)  & 0xff;
  p[3] = l & 0xff;
}

#undef L4TOL
L4TOL(p)
BYTE *p;
{
  return (((((p[0] << 8) | p[1]) << 8) | p[2]) << 8) | p[3];
}
#endif

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

/*

  ReadServer()

   1: Succeed;
   0: Error;

  len_return: データの長さ。0 を与えれば 格納しない。

  buf は 4 Byte 以上あり、bufsize >= 4 であることを仮定している

  bufsize < requiredsize なら空読みする。

 */

static
ReadServer(buf, bufsize, requiredsize, len_return)
BYTE *buf;
int bufsize, requiredsize, *len_return;
{
  int empty_count = 0, bufcnt = 0, readlen;
  unsigned rest = (unsigned)bufsize;
  BYTE *bufptr = buf;

  errno = 0;

  empty_count = 0;
  do {
    readlen = read(ServerFD, (char *)bufptr, rest);
    if (readlen < 0) {
      if (errno == EINTR) {
	continue;
      }
      else {
	break;
      }
    }
    else if ( readlen == 0 ) {
      empty_count++;
    }
    else { /* 読んだ */
      bufcnt += readlen;
      if (requiredsize <= bufsize) {
	bufptr += readlen;
	rest -= readlen;
      }
    }
  } while (empty_count < TRY_COUNT &&
	   (bufcnt == 0 || (requiredsize && bufcnt < requiredsize)));

  if (bufcnt == 0 || (requiredsize && bufcnt < requiredsize)) {
    errno = EPIPE;
    close(ServerFD);
    return NO;
  }
 
  if (bufcnt == 0 || (requiredsize && bufcnt < requiredsize)) {
    errno = EPIPE;
    close(ServerFD);
    return NO;
  }
  else {
    probe("Read: %d\n", bufcnt, buf);
    if (len_return) *len_return = bufcnt;
    return YES;
  }
}

static
#ifndef SIGNALRETURNSINT
void
#endif
DoSomething(sig)
int sig;
/* ARGSUSED */
{
    errno = EPIPE ;
}

static int
WriteServer( Buffer, size )
unsigned char *Buffer ;
int size ;
{
    register int todo, retval = YES;
    register int write_stat;
    register unsigned char *bufindex;
#ifdef SIGNALRETURNSINT
    static int (*Sig)();
#else /* !SIGNALRETURNSINT */
    static void (*Sig)();
#endif /* !SIGNALRETURNSINT */

    errno = 0 ;
    bufindex = Buffer ;
    todo = size ;
    Sig = signal(SIGPIPE, DoSomething);
    while (size) {
	errno = 0;
	probe("Write: %d\n", todo, (char *)bufindex);
	write_stat = write(ServerFD, (char *)bufindex, (unsigned)todo);
	if (write_stat >= 0) {
	    size -= write_stat;
	    todo = size;
	    bufindex += write_stat;
	} else if (errno == EWOULDBLOCK) {   /* pc98 */
	    continue ;
	}
	else if (errno == EINTR) {
	    continue;
	}
#ifdef EMSGSIZE
	else if (errno == EMSGSIZE) {
	    if (todo > 1)
		todo >>= 1;
	    else
		continue ;
	}
#endif
	else {
	    /* errno set by write system call. */
	    close( ServerFD ) ;
	    retval = NO;
	    errno = EPIPE ;
	    break;
	}
    }
    signal(SIGPIPE, Sig);
    return retval;
}

static
SendType0Request(proto, len, name) /* Initialize */
int proto, len;
BYTE *name;
{
  BYTE lbuf[PROTOBUF], *bufp = lbuf, *p;
  int sz = 8 + len;
  int res;

  if (sz <= PROTOBUF || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(len, p);   p += SIZEOFLONG;
    strcpy((char *)p, (char *)name);
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return NO;
  }
}

static
SendTypeE1Request(proto) /* Finalize */
int proto;
{
  BYTE lbuf[4];

  LTOL4(proto, lbuf);
  return WriteServer(lbuf, sizeof(lbuf));
}

static
SendTypeE2Request(proto, con) /* IR_DUP_CON */
int proto, con;
{
  BYTE lbuf[8], *p;

  LTOL4(proto, lbuf); p = lbuf + SIZEOFLONG;
  LTOL4(con, p);
  return WriteServer(lbuf, sizeof(lbuf));
}

static
SendTypeE3Request(proto, con, val) /* IR_DIC_LIST */
int proto, con, val;
{
  BYTE lbuf[12], *p;

  LTOL4(proto, lbuf); p = lbuf + SIZEOFLONG;
  LTOL4(con, p); p += SIZEOFLONG;
  LTOL4(val, p);
  return WriteServer(lbuf, sizeof(lbuf));
}

static
SendTypeE4Request(proto, con, bun, val) /* IR_GET_YOMI */
int proto, con, bun, val;
{
  BYTE lbuf[16], *p;

  LTOL4(proto, lbuf); p = lbuf + SIZEOFLONG;
  LTOL4(con, p); p += SIZEOFLONG;
  LTOL4(bun, p); p += SIZEOFLONG;
  LTOL4(val, p);
  return WriteServer(lbuf, sizeof(lbuf));
}

static
SendTypeE5Request(proto, con, bun, val, max) /* IR_GET_LEX */
int proto, con, bun, val;
{
  BYTE lbuf[5 * SIZEOFLONG], *p;

  LTOL4(proto, lbuf); p = lbuf + SIZEOFLONG;
  LTOL4(con, p); p += SIZEOFLONG;
  LTOL4(bun, p); p += SIZEOFLONG;
  LTOL4(val, p); p += SIZEOFLONG;
  LTOL4(max, p);
  return WriteServer(lbuf, sizeof(lbuf));
}

static
SendTypeE6Request(proto, con, bun, name, nlen) /* IR_STO_YOMI */
int proto, con, bun, nlen;
BYTE *name;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int res, sz = 4 * SIZEOFLONG + nlen;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(con, p);   p += SIZEOFLONG;
    LTOL4(bun, p);   p += SIZEOFLONG;
    LTOL4(nlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)name, nlen);
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  return NO;
}

static
SendTypeE7Request(proto, cx, val) /* IR_CONV_END */
int proto, val;
RkcContext *cx;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int res, con = (int)cx->server, mbun = val * (int)cx->maxbun, i;
  int datalen = mbun * SIZEOFLONG, sz = 3 * SIZEOFLONG + datalen;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(con, p);   p += SIZEOFLONG;
    LTOL4(mbun, p);  p += SIZEOFLONG;
    for (i = 0 ; i < mbun ; i++) {
      int kn = (int)cx->bun[i].curcand;

      LTOL4(kn, p); p += SIZEOFLONG;
    }
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  return NO;
}

static
SendTypeE9Request(proto, con, name, nlen, val) /* IR_MNT_DIC */
int proto, con, nlen, val;
BYTE *name;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int res, sz = 4 * SIZEOFLONG + nlen;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(con, p);   p += SIZEOFLONG;
    LTOL4(nlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)name, nlen); p += nlen;
    LTOL4(val, p);
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  return NO;
}

static
SendTypeE10Request(proto, con, name, nlen, val, vlen) /* IR_DEF_DIC */
int proto, con, nlen, vlen;
BYTE *name, *val;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int res, sz = 4 * SIZEOFLONG + nlen + vlen;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(con, p);   p += SIZEOFLONG;
    LTOL4(nlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)name, nlen); p += nlen;
    LTOL4(vlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)val, vlen);
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  return NO;
}

static
SendTypeE11Request(proto, con, name, nlen, dest, dlen, val)
int proto, con, nlen, dlen, val;
BYTE *name, *dest;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int res, sz = 5 * SIZEOFLONG + nlen + dlen;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(con, p);   p += SIZEOFLONG;
    LTOL4(nlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)name, nlen); p += nlen;
    LTOL4(dlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)dest, dlen); p += dlen;
    LTOL4(val, p);
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  return NO;
}

static
SendTypeE12Request(proto, name, nlen, val) /* IR_QUERY_EXT */
long proto, nlen, val;
BYTE *name;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int res, sz = 3 * SIZEOFLONG + nlen;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;
    LTOL4(proto, p); p += SIZEOFLONG;
    LTOL4(nlen, p);  p += SIZEOFLONG;
    strncpy((char *)p, (char *)name, nlen); p += nlen;
    LTOL4(val, p);   p += SIZEOFLONG;
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  return NO;
}

#define RecvType0Reply RecvTypeE1Reply /* Initinalize */

static
RecvTypeE1Reply(rep) /* Finalize */
int *rep;
{
  BYTE lbuf[SIZEOFLONG];

  if (ReadServer(lbuf, sizeof(lbuf), SIZEOFLONG, 0)) {
    *rep = (int)L4TOL(lbuf);
    return YES;
  }
  return NO;
}

/* GeneralReply

  ４バイト目から４バイトのデータを拾って来ると、そこから後ろのデータ長
  が拾えるようなリプライを拾うためのルーチン。

  rep       呼出しがわに返す値を格納するアドレス。
  storefunc 拾って来たデータの格納ルーチン。
  addr      格納する先のアドレス。
  unit      addr の大きさの単位がバイトで数えていくつかと言うことを表す値。
  maxn      その単位の大きさのデータが addr にはいくつ積めるかと言うこと。
  offset     storefunc にデータを渡す時のオフセット。

 */

static
GeneralReply(rep, storefunc, addr, maxn, unit, offset)
int *rep, (*storefunc)(), maxn, unit, offset;
BYTE *addr;
{
  BYTE lbuf[RECVBUFSIZE], *bufp = lbuf, *p;
  int res, datalen, readlen, readcnt, requiredsize, retval;

  if (!ReadServer(lbuf, RECVBUFSIZE, SIZEOFLONG, &readlen)) {
    return NO;
  }
  res = L4TOL(lbuf); p = lbuf + SIZEOFLONG;
  if (res >= 0) {
    readcnt = readlen;
    readlen = 0;
    if (readcnt < 2 * SIZEOFLONG) {
      if (!ReadServer(lbuf + readcnt, RECVBUFSIZE - readcnt,
		      (int)SIZEOFLONG - readcnt, &readlen)) {
        return NO;
      }
      readcnt += readlen;
    }

    datalen = L4TOL(p); p += SIZEOFLONG;

    requiredsize = 2 * SIZEOFLONG + datalen;

    if (readcnt < requiredsize) {
      if (RECVBUFSIZE < requiredsize) {
	bufp = (BYTE *)malloc(requiredsize);
	if (!bufp) {
	  (void)ReadServer(lbuf, RECVBUFSIZE, requiredsize, 0);
	  return NO;
	}
	bcopy(lbuf, bufp, readcnt);
      }
      if (!ReadServer(bufp + readcnt, requiredsize - readcnt,
		      requiredsize - readcnt, &readlen)) {
	retval = NO;
        goto endGenRep;
      }
    }
    /* ここまでは、どちらかと言うと純粋な read */

    if (storefunc) {
      res = (*storefunc)(res, bufp + offset * SIZEOFLONG, datalen,
			 addr, maxn, unit);
    }

    *rep = res;
    retval = YES;
  endGenRep:
    if (bufp != lbuf) free((char *)bufp);
    return retval;
  }
  return NO;
}

#define RecvTypeE2Reply(rep, storefunc, addr, maxsize) \
  GeneralReply(rep, storefunc, addr, maxsize, sizeof(char), 2)

#define RecvTypeE3Reply(rep, storefunc, addr, maxn, unit) \
  GeneralReply(rep, storefunc, addr, maxn, unit, 2)

static
RecvTypeE4Reply(rep, storefunc, addr, maxn, unit) /* IR_GET_LEX */
int *rep, maxn, unit;
int (*storefunc)();
BYTE *addr;
{
  return GeneralReply(rep, storefunc, addr, maxn, unit, 1);
}


/*
 * サーバから返された第一候補列を、第一候補列バッファに格納する。
 *  したがって、bun->kanjiのポインタの指す位置は変化しない。
 */

static int
firstKouhoStore(n, cx, data, datalen)
int n, datalen;
RkcContext *cx;
BYTE *data;
/* ARGSUSED */
{
    register Ushort *return_kouho, *wp, *ewp;
    register int i, save_len ;
    Ushort *first_kouho = cx->Fkouho ;
    int length, euc_len, res = n;
    BYTE *p;

    if (n < 0) return n;

    /* コピーすべきバッファの大きさを調べる */
    for (i = 0 ; i < (int)cx->curbun ; i++) {
      first_kouho += ushortstrlen(first_kouho) + 1;
    }
    save_len = first_kouho - cx->Fkouho;

    euc_len = 0; p = data;
    for (i = (int)cx->curbun ; i < n ; i++) {
      int ulen = L4TOL(p); p += SIZEOFLONG;
      euc_len += eucchars(p, ulen);
      p += ulen;
    }

    if (!(wp = (Ushort *)malloc((save_len + euc_len + 2) * 2))) {
      /* +2 は euc2ushort が最後まで積めるか不安なため。 */
      res = -1;
    }
    else {
      return_kouho = wp; ewp = wp + save_len + euc_len + 2;
      for( first_kouho = cx->Fkouho, i = 0; i < (int)cx->curbun; i++ ) {
	(void)ushortstrcpy(wp, first_kouho);
	length = ushortstrlen(first_kouho) + 1;
	wp += length;
	first_kouho += length;
      }
      p = data;
      for (i = (int)cx->curbun ; i < n ; i++) {
	int ulen = L4TOL(p); p += SIZEOFLONG;
	wp += euc2ushort((char *)p, ulen, wp, ewp - wp);
	p += ulen;
      }

/*    *(++wp) = (Ushort)0 ; 下のほうが正しいと思う。 */
      *wp = (Ushort)0 ;
      RkcFree((char *)cx->Fkouho);
      cx->Fkouho = return_kouho ;
    }
    return res;
}

static
rkc_initialize( username )
char *username ;
{
  int reply;
  int len = strlen( (char *)username ) + 1 ;

  if (SendType0Request(IR_INIT, len, (BYTE *)username) &&
      RecvType0Reply(&reply)) {
    if (reply < 0) {
      close(ServerFD);
    }
    return reply;
  }
  return -1;
}

static
Fin_Create( request )
int request ;
{
  int reply;

  if (SendTypeE1Request(request) &&
      RecvTypeE1Reply(&reply)) {
    return reply;
  }
  return -1;
}

static
rkc_finalize()
{
  int retval = Fin_Create(IR_FIN);
  (void)close(ServerFD);
  return retval;
}

static
rkc_create_context()
{
    return( Fin_Create( IR_CRE_CON ) ) ;
}

static
Dup_Close_CX( cx_num, request )
int cx_num, request ;
{
  int reply;

  if (SendTypeE2Request(request, cx_num)&&
      RecvTypeE1Reply(&reply)) {
    return reply;
  }
  return -1;
}

static
rkc_duplicate_context( cx )
register RkcContext *cx ;
{
    return( Dup_Close_CX( (int)cx->server, IR_DUP_CON ) ) ;
}

static
rkc_close_context( cx )
register RkcContext *cx ;
{
    return( Dup_Close_CX( (int)cx->server, IR_CLO_CON ) ) ;
}

static int
dicStore(n, src, slen, dest, dmax, unit)
BYTE *src, *dest;
int n, slen, unit, dmax;
/* ARGSUSED */
{
  BYTE *p = dest, *endp = dest + dmax - 2; /* 2 for EOS */
  BYTE *wp = src, *wendp = src + slen;
  int len, i;

  for (i = 0 ; i < n && p < endp && wp < wendp ; i++) {
    len = L4TOL(wp); wp += SIZEOFLONG;
    if (endp < p + len) break;
    strcpy((char *)p, (char *)wp);
    p += len;
    wp += len;
  }
  *p = '\0';
  return i;
}

static
Dic_Dir_List( context, data, max, request )
int context, max, request ;
char *data ;
{
  int reply;

  if (SendTypeE3Request(request, context, max) &&
      RecvTypeE2Reply(&reply, dicStore, (BYTE *)data, max)) {
    return reply;
  }
  return -1;
}

static
rkc_dictionary_list( cx, dicnames, max)
register RkcContext *cx ;
unsigned char *dicnames ;
int max ;
{
  return Dic_Dir_List((int)cx->server, (char *)dicnames, max, IR_DIC_LIST);
}

static
Define_Delete_dic( cx, dicname, wordrec, request )
register RkcContext *cx ;
char *dicname, *wordrec ;
{
  int reply;

  if (SendTypeE10Request(request, (int)cx->server,
			(BYTE *)dicname, strlen((char *)dicname) + 1,
			(BYTE *)wordrec, strlen((char *)wordrec) + 1) &&
      RecvTypeE1Reply(&reply)) {
    return reply;
  }
  return -1;
}

static
rkc_define_dic( cx, dicname, wordrec)
register RkcContext *cx ;
unsigned char *dicname ;
Ushort *wordrec ;
{
  char cbuf[RK_LINE_BMAX];

  (void)ushort2euc(wordrec, ushortstrlen(wordrec), cbuf, RK_LINE_BMAX);
  return Define_Delete_dic(cx, (char *)dicname, cbuf, IR_DEF_DIC);
}

static
rkc_delete_dic( cx,  dicname, wordrec)
register RkcContext *cx ;
unsigned char *dicname ;
Ushort *wordrec ;
{
  char cbuf[RK_LINE_BMAX];

  (void)ushort2euc(wordrec, ushortstrlen(wordrec), cbuf, RK_LINE_BMAX);
  return Define_Delete_dic(cx, (char *)dicname, cbuf, IR_UNDEF_DIC);
}

static
mount_dic(req, con, dat, mod)
int req, con, mod;
char *dat;
{
  int reply, datlen = strlen((char *)dat) + 1;

  if (SendTypeE9Request(req, con, (BYTE *)dat, datlen, mod) &&
      RecvTypeE1Reply(&reply)) {
    return reply;
  }
  return -1;
}

static
rkc_mount_dictionary( cx, dicname, mode )
register RkcContext *cx ;
char *dicname ;
int  mode ;
{
  return mount_dic(IR_MNT_DIC, (int)cx->server, dicname, mode);
}

static
rkc_umount_dictionary( cx, dicname )
register RkcContext *cx ;
char *dicname ;
{
  return mount_dic(IR_UMNT_DIC, cx->server, dicname, 0);
}

static
rkc_remount_dictionary( cx, dicname, where )
register RkcContext *cx ;
unsigned char *dicname ;
int where ;
{
  int reply, datalen = strlen((char *)dicname) + 1;

  if (SendTypeE6Request(IR_RMNT_DIC, cx->server, where, 
			(BYTE *)dicname, datalen) &&
      RecvTypeE1Reply(&reply)) {
    return reply;
  }
  return -1;
}

static
rkc_mount_list( cx, data, max)
RkcContext *cx ;
unsigned char *data ;
int max ;
{
  return Dic_Dir_List(cx->server, (char *)data, max, IR_MNT_LIST);
}

rkc_get_dir_list( cx, ddname, maxddname )
RkcContext *cx ;
char *ddname ;
int maxddname ;
{
    return( Dic_Dir_List( cx->server, ddname, maxddname, IR_DIR_LIST ) ) ;
}

#define GAKUSHU 1

static
rkc_convert_end( cx, mode )
RkcContext *cx ;
int mode ;
{
  int reply, gakushu = (mode & GAKUSHU) ? 1 : 0;

  if (SendTypeE7Request(IR_CONV_END, cx, gakushu) &&
      RecvTypeE1Reply(&reply)) {
    return reply;
  }
  return -1;
}

static
convStore(n, data, datalen, contex, v, u)
int n, datalen, v, u;
BYTE *data, *contex;
/* ARGSUSED */
{
  RkcContext *cx = (RkcContext *)contex;
  int ret;

  if ((ret = firstKouhoStore(n, cx, data, datalen)) < 0 ){
    (void)rkc_convert_end( cx, 0 );
  }
  return ret;
}

static
rkc_convert( cx, yomi, length, mode )
RkcContext *cx ;
int length ,mode;
Ushort *yomi ;
{
  int reply, datalen = ushort2eucsize(yomi, length) + 1, res = -1;
  char cbuf[BUFSIZE], *bufp = cbuf;

  if (datalen <= BUFSIZE || (bufp = malloc(datalen + 2))) {
    (void)ushort2euc(yomi, length, bufp, datalen + 2);
    /* +2 は ushort2euc がちゃんと詰めてくれるか不安なため。 */

    if (SendTypeE9Request(IR_CONVERT, cx->server, (BYTE *)bufp, datalen, mode)
	&& RecvTypeE2Reply(&reply, convStore, (BYTE *)cx, 0)) {
      res = reply;
    }
    else {
      res = -1;
    }
    if (bufp != cbuf) free(bufp);
  }
  return res;
}

static
yomiStore(n, data, datalen, dest, destlen, unit)
int n, datalen, destlen, unit;
BYTE *data, *dest;
/* ARGSUSED */
{
  int len = L4TOL(data); data += SIZEOFLONG;
  if (n < len) len = n;
  return euc2ushort((char *)data, len, (Ushort *)dest, destlen);
}

static
rkc_get_yomi( cx, yomip )
register RkcContext *cx ;
Ushort *yomip ;
{
  int reply;

  if (SendTypeE4Request(IR_GET_YOMI, cx->server, (int)cx->curbun, BUFSIZE) &&
      RecvTypeE2Reply(&reply, yomiStore, (BYTE *)yomip, CBUFSIZE)) {
    return reply;
  }
  return -1;
}


static
kanlisStore(n, data, datalen, cox, v, u)
int n, datalen, v, u;
BYTE *data, *cox;
/* ARGSUSED */
{
  RkcContext *cx = (RkcContext *)cox;
  RkcBun *bun = cx->bun + (int)cx->curbun;
  int len, i;
  Ushort *kouho_list, *wp, *ewp;
  BYTE *p;

  if (n < 0) return n;

  len = 0; p = data;
  for (i = 0 ; i < n ; i++) {
    int ulen = L4TOL(p); p += SIZEOFLONG;
    len += eucchars(p, ulen);
    p += ulen;
  }

  if (len > 0 && (kouho_list = (Ushort *)malloc((len + 2) * SIZEOFSHORT))) {
    /* +2 は euc2ushort が最後まで詰められるか不安なため */
    p = data; wp = kouho_list; ewp = wp + len + 2;
    for (i = 0 ; i < n ; i++) {
      int ulen = L4TOL(p); p += SIZEOFLONG;
      wp += euc2ushort((char *)p, ulen, wp, ewp - wp);
      p += ulen;
    }
    bun->kanji = kouho_list;
  }
  return n;
}

rkc_get_kanji_list( cx )
register RkcContext *cx ;
{
  int reply;

  if (SendTypeE4Request(IR_KAN_LST, (int)cx->server, (int)cx->curbun,
			BUFSIZE) &&
      RecvTypeE2Reply(&reply, kanlisStore, (BYTE *)cx, 0)) {
    return reply;
  }
  return -1;
}

extern int _RkwGetYomi();

static
resizeStore(n, data, datalen, contex, v, u)
int n, datalen, v, u;
BYTE *data, *contex;
/* ARGSUSED */
{
  return firstKouhoStore(n, (RkcContext *)contex, data, datalen);
}

static
rkc_resize( cx, yomi_length )
register RkcContext *cx ;
int yomi_length ;
{
    Ushort cbuf[CBUFSIZE];
    register int ret, euclen = 0;
    short curbun;
    int reply;

    if( yomi_length > 0 ){
	curbun = cx->curbun;
	for( ; (cx->curbun < cx->maxbun) ; (cx->curbun)++ ) {
	    /* めっちゃ効率悪いけどしゃあないかぁ */
	    /* だいたいこいつら ushortの事なんか考えてへんやんか */
	    if( (ret = _RkwGetYomi( cx, cbuf, CBUFSIZE )) < 0 ) {
		cx->curbun = curbun;
		return( -1 ) ;
	    }
	    if( yomi_length > ret ) {
		euclen += ushort2eucsize(cbuf, ret);
		yomi_length -= ret;
	    } else {
		euclen += ushort2eucsize(cbuf, yomi_length);
		break;
	    }
	}
	cx->curbun = curbun;
    } else
	euclen = yomi_length;

    if (SendTypeE4Request(IR_RESIZE, cx->server, cx->curbun, euclen) &&
	RecvTypeE2Reply(&reply, resizeStore, (BYTE *)cx, 0)) {
      return reply;
    }
    return -1;
}

static
rkc_store_yomi( cx, yomi, max )
register RkcContext *cx ;
Ushort *yomi ;
int max ;
{
  int reply, len;
  char cbuf[BUFSIZE], *bufp = cbuf;

  len = ushort2eucsize(yomi, max);
  if (len + 2 <= BUFSIZE || (bufp = malloc(len + 2))) {
    (void)ushort2euc(yomi, max, bufp, len + 2);
    if (!SendTypeE6Request(IR_STO_YOMI, (int)cx->server, (int)cx->curbun,
			   (BYTE *)bufp, len)
	|| !RecvTypeE2Reply(&reply, resizeStore, (BYTE *)cx, 0)) {
      reply = -1;
    }
    if (bufp != cbuf) free(bufp);
  }
  else {
    reply = -1;
  }
  return reply;
}

static int RemoteDicUtilBaseProtoNumber = 0;

static
Query_Extension()
{
    if( !RemoteDicUtilBaseProtoNumber ){
	int datalen = strlen( REMOTE_DIC_UTIL ) + 1 ;

        if (SendTypeE12Request(IR_QUERY_EXT, (BYTE *)REMOTE_DIC_UTIL, datalen, 
			       MAXEXTREQUESTNO + 1) &&
	    RecvTypeE1Reply(&RemoteDicUtilBaseProtoNumber)) {
	  return RemoteDicUtilBaseProtoNumber;
        }
        return -1;
    } else {
	return( RemoteDicUtilBaseProtoNumber ) ;
    }
}

#ifdef EXTENSION
static
rkc_list_dictionary( cx, dirname, dicnames_return, size )
register RkcContext *cx ;
unsigned char *dirname, *dicnames_return ;
int size ;
{
  int reply, datalen = strlen((char *)dirname) + 1;
  int extension_base = Query_Extension() ;

  if (extension_base < 0) return -1;

  if (size < datalen) datalen = size;

  if (SendTypeE6Request(IR_LIST_DIC + extension_base,
			cx->server, size, (BYTE *)dirname, datalen) &&
      RecvTypeE2Reply(&reply, dicStore, dicnames_return, size)) {
    return reply;
  }
  return -1;
}

static
rkc_create_dictionary( cx, dicname, mode )
register RkcContext *cx ;
unsigned char *dicname ;
int mode ;
{
    int extension_base = Query_Extension() ;

    if( extension_base < 0 )
	return( -1 ) ;

    return mount_dic(IR_CREAT_DIC + extension_base, cx->server,
		     (char *)dicname, mode);
}

static
rkc_destroy_dictionary( cx, dicname )
register RkcContext *cx ;
unsigned char *dicname ;
{
    int extension_base = Query_Extension() ;

    if( extension_base < 0 )
	return( -1 ) ;

    return mount_dic(IR_DEST_DIC + extension_base, cx->server,
		     (char *)dicname, 0);
}

static
rkc_rename_dictionary( cx, dicname, newdicname, mode )
register RkcContext *cx ;
unsigned char *dicname, *newdicname ;
int mode ;
{
    int reply;
    int extension_base = Query_Extension() ;

    if( extension_base < 0 )
	return( -1 ) ;

    if (SendTypeE11Request(IR_RENAME_DIC + extension_base,
			   (int)cx->server,
			   (BYTE *)dicname, strlen((char *)dicname) + 1,
			   (BYTE *)newdicname, strlen((char *)newdicname) + 1,
			   mode) &&
	RecvTypeE1Reply(&reply)) {
      return reply;
    }
    return -1;
}

/* ARGSUSED */
static
rkc_get_text_dictionary( cx, dirname, dicname, info, infolen )	
register RkcContext *cx ;
unsigned char *dirname, *dicname ;
Ushort *info ;
int infolen ;
{
    int extension_base = Query_Extension() ;
    int ret ;

    if( extension_base < 0 )
	return( -1 ) ;

    if (SendTypeE11Request(IR_GET_WORD_DIC + extension_base,
			   (int)cx->server,
			   (BYTE *)dirname, strlen((char *)dirname) + 1,
			   (BYTE *)dicname, strlen((char *)dicname) + 1,
			   infolen) &&
	RecvTypeE2Reply(&ret, yomiStore,
			(BYTE *)info, infolen * SIZEOFSHORT)) {
      return ret;
    }
    return -1;
}

			
#endif /* EXTENSION */

/* ARGSUSED */
static
statStore(n, src, slen, dest, maxn, unit)
int n, slen, maxn, unit;
BYTE *src;
RkStat *dest;
{
  if (!(n < 0)) {
    dest->bunnum = (int)L4TOL(src);	/* bunsetsu bangou */
    src += SIZEOFLONG;
    dest->candnum = (int)L4TOL(src);	/* kouho bangou */
    src += SIZEOFLONG;
    dest->maxcand = (int)L4TOL(src);	/* sou kouho suu */
    src += SIZEOFLONG;
    dest->diccand = (int)L4TOL(src);	/* jisho ni aru kouho suu */
    src += SIZEOFLONG;
    dest->ylen = (int)L4TOL(src);	/* yomigana no nagasa (in byte) */ 
    src += SIZEOFLONG;
    dest->klen = (int)L4TOL(src);	/* kanji no nagasa (in byte) */
    src += SIZEOFLONG;
    dest->tlen = (int)L4TOL(src);	/* tango no kosuu */
  }
  return n;
}

/* ARGSUSED */
static
lexStore(n, src, slen, dest, maxn, unit)
int n, slen, maxn, unit;
BYTE *src;
RkLex *dest;
{
  int i;

  if (n > 0 && n > maxn) n = maxn;

  for (i = 0; i < n; i++, dest++) {
    dest->ylen = (int)L4TOL(src);	/* yomigana no nagasa (in byte) */ 
    src += SIZEOFLONG;
    dest->klen = (int)L4TOL(src);	/* kanji no nagasa (in byte) */
    src += SIZEOFLONG;
    dest->rownum = (int)L4TOL(src);	/* row number */
    src += SIZEOFLONG;
    dest->colnum = (int)L4TOL(src);	/* column number */
    src += SIZEOFLONG;
    dest->dicnum = (int)L4TOL(src);	/* dic number */
    src += SIZEOFLONG;
  }
  return n;
}

static
rkc_get_stat( cx, stat )
register RkcContext *cx ;
RkStat *stat ;
{
  int reply, n, i;
  Ushort cbuf[CBUFSIZE], *src_kanji;

  if (SendTypeE4Request(IR_GET_STA, cx->server, cx->curbun,
			cx->bun[cx->curbun].curcand) &&
      RecvTypeE3Reply(&reply, statStore,
		      (BYTE *)stat, 1, sizeof(RkStat) / sizeof(int))) {
    if (reply == 0) {
	stat->ylen = _RkwGetYomi(cx, cbuf, CBUFSIZE);
	switch( cx->bun[cx->curbun].flags ){
	case NOTHING_KOUHO:
	    stat->klen = stat->ylen;
	    break;
	case FIRST_KOUHO:
	    stat->klen = ushortstrlen(cx->bun[cx->curbun].kanji);
	    break;
	case NUMBER_KOUHO:
	    src_kanji = cx->bun[cx->curbun].kanji;
	    n = cx->bun[cx->curbun].curcand;
	    for (i = 0 ; i < n ; i++)
		src_kanji += ushortstrlen( src_kanji ) + 1;
	    stat->klen = ushortstrlen(src_kanji);
	    break;
	}
    }
  }
  return reply;
}

static
rkc_get_lex( cx, max, info_return )
register RkcContext *cx ;
int max ;
RkLex *info_return ;
{
    int ret, len, i, ylen, klen;
    char kbuf[BUFSIZE], ybuf[BUFSIZE];
    Ushort cbuf[CBUFSIZE], *src_kanji;
    RkcBun *bun = cx->bun + cx->curbun;
    RkLex *tango;

    if (!SendTypeE5Request(IR_GET_LEX, cx->server, cx->curbun,
			   cx->bun[cx->curbun].curcand, max) ||
        !RecvTypeE4Reply(&ret, lexStore, (BYTE *)info_return, 
			 max, sizeof(RkLex) / sizeof(int))) {
      ret = -1;
    }

    if (ret >= 0) {
	len = _RkwGetYomi(cx, cbuf, CBUFSIZE);
	(void)ushort2euc(cbuf, len, ybuf, BUFSIZE);
	switch( bun->flags ){
	case NOTHING_KOUHO:
	    src_kanji = cbuf;
	    break;
	case FIRST_KOUHO:
	    src_kanji = bun->kanji;
	    break;
	case NUMBER_KOUHO:
	    src_kanji = bun->kanji;
	    for( i = 0; i < bun->curcand; i++ )
		src_kanji += ushortstrlen( src_kanji ) + 1;
	    break;
	}
	(void)ushort2euc(src_kanji, ushortstrlen(src_kanji), kbuf, BUFSIZE);
	ylen = klen = 0;
	tango = info_return;
	for( i = 0; i < ret; i++ ){
	    len = tango->ylen;
	    tango->ylen = euc2ushort(ybuf + ylen, len, cbuf, CBUFSIZE);
	    ylen += len;
	    len = tango->klen;
	    tango->klen = euc2ushort(kbuf + klen, len, cbuf, CBUFSIZE);
	    klen += len;
	    tango++;
	}
    }
    return( ret );
}

#endif /* USE_EUC_PROTOCOL */

static int
rkc_error()
{
    return -1;
}

struct rkcproto eucproto = {
#ifdef USE_EUC_PROTOCOL
  rkc_initialize,
  rkc_finalize,
  rkc_close_context,
  rkc_create_context,
  rkc_duplicate_context,
  rkc_dictionary_list,
  rkc_define_dic,
  rkc_delete_dic,
  rkc_mount_dictionary,
  rkc_remount_dictionary,
  rkc_umount_dictionary,
  rkc_mount_list,
  rkc_convert,
  rkc_convert_end,
  rkc_get_kanji_list,
  rkc_get_stat,
  rkc_resize,
  rkc_store_yomi,
  rkc_get_yomi,
  rkc_get_lex,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
#ifdef EXTENSION
  rkc_list_dictionary,
  rkc_create_dictionary,
  rkc_destroy_dictionary,
  rkc_rename_dictionary,
  rkc_get_text_dictionary,
  rkc_error,
  rkc_error,
  rkc_error,
#endif /* EXTENSION */
#else /* !USE_EUC_PROTOCOL */
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
#ifdef EXTENSION
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
  rkc_error,
#endif /* EXTENSION */
#endif /* !USE_EUC_PROTOCOL */
};
