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
  This program contains a part of the X library.  The communication part
  of the X library is modified and built into this program.
 */
/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#if !defined(lint) && !defined(__CODECENTER__)
static char rcs_id[] = "$Id: rkcwConv.c,v 6.22 1994/05/25 08:13:23 kon Exp $";
#endif

/* LINTLIBRARY */

#include <stdio.h>

#include "rkcw.h"
#include "sglobal.h"
#include "canna/RK.h"
#include "rkc.h"

#include <errno.h>
#include <sys/types.h>
#include <signal.h>

#if __STDC__
#include <string.h>
#else
#if defined(USG) || defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif
extern char *strtok();
#endif

#include "net.h"

extern FILE  *fopen() ; 			
#if __STDC__
#include <stdlib.h>
#else
extern char  *malloc() ;
extern void  free() ;
#endif

extern char  *getenv() ;

extern unsigned long  inet_addr() ;		

extern int   errno ;

#ifndef CANNAHOSTFILE
#define CANNAHOSTFILE	    "/usr/lib/canna/cannahost"
#endif

#define RkcFree( p )	{ if( (p) ) (void)free( (char *)(p) ) ; }

#define ReqType0    0
#define ReqType1    1
#define ReqType2    2
#define ReqType3    3
#define ReqType4    4
#define ReqType5    5
#define ReqType6    6
#define ReqType7    7
#define ReqType8    8
#define ReqType9    9
#define ReqType10   10
#define ReqType11   11
#define ReqType12   12
#define ReqType13   13
#define ReqType14   14
#define ReqType15   15
#define ReqType16   16
#define ReqType17   17
#define ReqType18   18
#define ReqType19   19
#define ReqType20   20
#define ReqType21   21

#define SENDBUFSIZE 1024
#define RECVBUFSIZE 1024

int ServerFD ;

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

#ifdef UNIXCONN
/* UNIXドメインでお話する */
static int
connect_unix( number )
int number ;    
{
    struct sockaddr_un unaddr;	    /* UNIX socket address. */
    struct sockaddr *addr;	    /* address to connect to */
    int addrlen ;		 
    
    /* いろはサーバと、ＵＮＩＸドメインで接続 */
    unaddr.sun_family = AF_UNIX;
    if( number )
	sprintf( unaddr.sun_path,"%s:%d", IR_UNIX_PATH, number ) ;
    else
	strcpy( unaddr.sun_path, IR_UNIX_PATH ) ;

    addr = (struct sockaddr *)&unaddr;
    addrlen = strlen( (char *)unaddr.sun_path ) + 2;
    /*
     * Open the network connection.
     */
    if ((ServerFD = socket((int) addr->sa_family, SOCK_STREAM, 0)) >= 0){
	if( connect( ServerFD, addr, addrlen ) < 0 ) {
	    close( ServerFD ) ;
	    return( -1 ) ;
	}
    }
    return( ServerFD ) ;
}    
#endif /* UNIXCONN */

#ifdef STREAMCONN
/* ストリームパイプで いろはサーバとお話する */
static int
connect_stream_pipe( number )
int number ;
{
    char namebuf[(sizeof(IR_STREAM_PATH)) + 8];
    char buf[ sizeof(struct file *) ] ; 
    struct strbuf ctrlbuf ;
    int retfd, flags, mfd ;
        
    sprintf( namebuf, "%s%d%s", IR_STREAM_PATH, number, "R" ) ;
    
    if( (mfd = open( namebuf, O_RDWR )) < 0 )
	return( -1 ) ;

    if( (retfd = open( IROHA_STREAM, O_RDWR )) < 0 ) {
	int w = errno ;
	
	close ( mfd ) ;
	errno = w ;
	return( -1 ) ;
    }

    if( write (mfd, &mfd, 1) != 1 ) {
	int w = errno ;
	
	close( retfd ) ;
	close( mfd ) ;
	errno = w ;
	return( -1 ) ;
    }
    
    ctrlbuf.maxlen = sizeof( buf ) ;
    ctrlbuf.buf = buf ;
    flags = 0 ;
    if ( getmsg(mfd, &ctrlbuf, (struct strbuf *)NULL, &flags) < 0 ) {
	int w = errno ;
	
	close( retfd ) ;
	close( mfd ) ;
	errno = w ;
	return( -1 ) ;
    }
    
    if( putmsg(retfd, &ctrlbuf, (struct strbuf *) NULL, 0) < 0 ) {
	int w = errno ;
	
	close( retfd ) ;
	close( mfd ) ;
	errno = w ;
	return ( -1 );
    }
    close( mfd ) ;
    return( retfd ) ;
}
#endif /* STREAMCONN */

static int
connect_inet( hostname, number )
int number ;
char *hostname ;
{
    struct sockaddr_in inaddr;	    /* INET socket address. */
    unsigned long hostinetaddr;     /* result of inet_addr of arpa addr */
    struct hostent *host_ptr, workhostbuf ;
    struct servent *sp ;
    int addrlen ;		 
    char *h_addr_ptr;

    /* インターネットドメインで接続する。 */
    if( (host_ptr = gethostbyname( hostname ) )
	                                 == (struct hostent *)NULL) {
	hostinetaddr = inet_addr( hostname );
	if( hostinetaddr == (unsigned long)-1 ) {
	    /* インターネットアドレス表記が間違っている */
	    errno = EINVAL;
	    return( -1 );
	}
	
	if( !(host_ptr = gethostbyaddr( (char *)&hostinetaddr,
				 sizeof( hostinetaddr ), AF_INET )) ) {
	    host_ptr = &workhostbuf ;
	    host_ptr->h_addrtype = AF_INET ;
#ifdef SVR4
	    /* SVR4では hostent構造体が変更されている． */
	    host_ptr->h_addr_list = &h_addr_ptr;
#endif /* SVR4 */
	    host_ptr->h_addr = (char *)&hostinetaddr ;
	    host_ptr->h_length = sizeof( hostinetaddr ) ;
	}
    } else { 
	/* アドレスタイプをチェックする */
	if (host_ptr->h_addrtype != AF_INET) {
	    /* Not an Internet host! */
	    errno = EPROTOTYPE;
	    return( -1 );
	}
    }

    if( (ServerFD = socket( AF_INET, SOCK_STREAM, 0 )) < 0 ) 
	return( -1 ) ;
  
    errno = 0;
    /* /etc/servicesからポート番号を取得する */ 
    sp = getservbyname( IR_SERVICE_NAME, "tcp");
    /* データセット */
    inaddr.sin_family = host_ptr->h_addrtype;
    inaddr.sin_port = (sp ? ntohs(sp->s_port) : IR_DEFAULT_PORT) + number;
    inaddr.sin_port = htons(inaddr.sin_port);
    bcopy( host_ptr->h_addr, &inaddr.sin_addr, sizeof(inaddr.sin_addr) ) ;
    addrlen = sizeof( struct sockaddr_in ) ;
    errno = 0 ; 
    if ( connect( ServerFD, (struct sockaddr *)&inaddr, addrlen ) < 0 ) {
#ifdef nodef
	perror("connect") ;
#endif
	close( ServerFD ) ;
	return( -1 ) ;
    }
    return( ServerFD ) ;
}

#define MAX_LIST	128
static int
increment_counter( flush )
int flush ;    
{
    static int counter = 0 ;

    if( flush )
	counter = 0 ;
    else
	counter++ ;

    if( counter > MAX_LIST - 1 )
	return( -1 ) ;
    
    return( counter ) ;
}

/* 以下の順序で，指定したサーバ名のポインタリストを返す */
static void
rkc_build_cannaserver_list( list )
char **list ;
{
    char work[ MAX_HOSTNAME ], *hostp ;
    char **listp = list, *RkGetServerName(), *getenv();
    FILE *hostfp ;
    
    increment_counter( 1 ) ;
    /* 先ずは，指定が無いか見て、次に環境変数からリストを作成する*/
    if ((hostp = RkGetServerName()) != (char *)NULL ||
	(hostp = getenv( "CANNAHOST" )) != (char *)NULL) {
	char *wp, buf[ MAX_HOSTNAME ] ;

	strncpy( buf, hostp, MAX_HOSTNAME ) ;
	buf[MAX_HOSTNAME - 1] = '\0';				/* S004 */
	for( wp = strtok( buf, "," ); wp && *wp; wp = strtok( NULL, "," ) ){ /* S004 */
	    *listp = (char *)malloc(strlen(wp) + 1);
	    if (*listp) {
	      strcpy(*listp, wp);
	    }
	    if( increment_counter( 0 ) < 0 ) {
		*listp = (char *)NULL ;
		return ;
	    }
	    else {
		listp++ ;
	    }
	}							/* S004 */
    }
	
    /* CANNAHOSTFILE ファイルからリストを作成する */
    if( (hostfp = fopen( CANNAHOSTFILE, "r" )) != (FILE *)NULL ) {
	while( (hostp = fgets( work, MAX_HOSTNAME, hostfp) )
	                                       != (char *)NULL ) {
	    /* 改行文字をとる */
	    work[ strlen( hostp )-1 ] = '\0' ;
	    /* リストに格納する */
	    *listp = (char *)malloc(strlen(work) + 1);
	    if (*listp) {
	      strcpy(*listp, work);
	    }
	    if( !*listp || ( increment_counter( 0 ) < 0) ) 
		break ;
	    listp++ ;
	}
	fclose( hostfp ) ;
    }
    *listp = (char *)NULL ;
}

/* 引数に NULL ポインタを渡してはいけません。*/
/* それどころか、十分おおきな配列を渡さなければならないのだ */
rkc_Connect_Iroha_Server( hostname )
char *hostname ; 
{
    char *serverlist[ MAX_LIST ], **listp ;
    int num ;
    char *number ;

    listp = serverlist ;
    if( hostname[ 0 ] == '\0' ) {
	rkc_build_cannaserver_list( listp ) ;
	if( !*listp ) {
	    *listp = (char *)malloc(strlen("unix") + 1);
	    if (*listp) {
	      strcpy(*listp, "unix");
	    }
	    listp++ ;
	    *listp = (char *)NULL ;
	}	
    }
    else {
	*listp = (char *)malloc(strlen(hostname) + 1);
	if (*listp) {
	  strcpy(*listp, hostname);
	}
	listp++ ;
	*listp = (char *)NULL ;
    }
    
    for( listp = serverlist; *listp; listp++ ) {
	/* サーバ起動番号を取得する */
	strtok( *listp, ":" ) ;
	number = (char *)strtok( NULL, ":" ) ;
	num = number ? atoi( number ) : 0 ;
	strcpy( hostname, *listp ) ;
	if( num )
	    sprintf( hostname,"%s:%d", hostname, num ) ;

#if defined(UNIXCONN) || defined(STREAMCONN)
	if ( (strcmp( "unix", *listp ) == 0) ) {
#ifdef UNIXCONN
	    ServerFD = connect_unix( num ) ;
	}
	else { 
#else /* STREAMCONN */
	    /* いろはサーバとストリームパイプで接続 */
	    ServerFD = connect_stream_pipe( num ) ;
	}
        else {
#endif /* UNIXCONN */
#endif /* UNIXCONN || STREAMCONN */
#if defined(UNIXCONN) || defined(STREAMCONN)
	    ServerFD = connect_inet( *listp, num ) ;
        }
#endif
	if( ServerFD >= 0 )
	    break ;
    }
    listp = serverlist ;
    
    while( *listp )
	free( *listp++ ) ;
	
    return( ServerFD ) ;
}

#define HEADER_SIZE ((sizeof(char)) + (sizeof(char)) + SIZEOFSHORT)

/*
 * サーバから返された第一候補列を、第一候補列バッファに格納する。
 */

static
firstKouhoStore(n, data, len, dest)
int n, len;
BYTE *data, *dest;
{
    RkcContext *cx = (RkcContext *)dest;
    register Ushort *return_kouho, *wp ;
    register int i, save_len ;
    Ushort *first_kouho = cx->Fkouho ;
    int length ;

    if (n < 0) return n;

    /* コピーすべきバッファの大きさを調べる */
     for( save_len = i = 0; i < cx->curbun; i++ ) {
 	length = ushortstrlen( first_kouho )+1 ;
 	first_kouho += length ;
 	save_len += length ;
     }
 
    if (!(return_kouho = (Ushort *)malloc(save_len * SIZEOFSHORT + len)))
      return( -1 ) ;
 
    wp = return_kouho;
    bcopy(cx->Fkouho, wp, save_len * SIZEOFSHORT);
    wp += save_len ;
    len /= SIZEOFSHORT;
    for (i = 0 ; i < len ; i++) {
      *wp = S2TOS(data); data += SIZEOFSHORT; wp++;
    }
    RkcFree((char *)cx->Fkouho);
    cx->Fkouho = return_kouho ;
    return 0;
}

static
int
firstKouhoStore_2(n, data, len, dest)
int n, len;
BYTE *data, *dest;
{
    RkcContext *cx = (RkcContext *)dest;
    register Ushort *return_kouho, *wp ;
    register int i ;

    if (n < 0) return n;

    if( !(return_kouho = (Ushort *)malloc( len )) )
	return( -1 );

    wp = return_kouho;
    len /= SIZEOFSHORT;
    for (i = 0 ; i < len ; i++) {
	*wp = S2TOS(data); data += SIZEOFSHORT; wp++;
    }
    RkcFree((char *)cx->Fkouho);
    cx->Fkouho = return_kouho ;
    return 0;
}

#define PROTOBUF (16 * 8)

#define READ_BUF_SIZE	    (ACK_BUFSIZE + sizeof( int )*2 )
#define TRY_COUNT	    10


/* 
  buf は 4 Byte 以上あり、bufsize >= 4 であることを仮定している
 */

#define READIT(ServerFD, requiredsize, p, bufcnt, rest) /* SUPPRESS622 */\
  do {                                                               \
    int empty_count = 0;                                             \
    while (empty_count < TRY_COUNT && bufcnt < requiredsize) {       \
      readlen = read(ServerFD, p, rest);                             \
      if (readlen < 0) {                                             \
	if (errno == EINTR) {                                        \
	  continue;                                                  \
	}                                                            \
	else {                                                       \
	  break;                                                     \
	}                                                            \
      }                                                              \
      else if ( readlen == 0 ) {                                     \
	empty_count++;                                               \
      }                                                              \
      else {                                                         \
	empty_count = 0;                                             \
	bufcnt += readlen;                                           \
	p += readlen;                                                \
	rest -= readlen;                                             \
      }                                                              \
    }                                                                \
  } while (0)

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

   0: Succeed;
  -1: Error;

  ReadServer はサーバからの reply を read する。とりあえず、ReadServer
  へはバッファとバッファサイズを渡すが、ReadServer はバッファが足りな
  いと判断すると自分で malloc してそのバッファを使う。ReadServer がバッ
  ファを malloc した場合には allocptr にそのバッファへのポインタを返す。
  allocptr に 0 が渡された場合には ReadServer がバッファサイズが足りな
  いと判断した場合は、上記 malloc が行われず ReadServer はエラーリター
  ンする。

  ReadServer がエラーリターンした場合は malloc は行われていないと判断
  して良い。

  allocptr: バッファが足りなかった場合、ReadServer が alloc したバッファ
  len_return: 読んだデータの長さ。0 を与えれば 格納しない。

 */

static
ReadServer(buf, bufsize, len_return, allocptr)
BYTE *buf, **allocptr;
int bufsize, *len_return;
{
  BYTE *bufptr = buf, *p = buf, *q;
  int bufcnt = 0, rest = bufsize, readlen;
  int requiredsize = HEADER_SIZE;
  unsigned short len = (unsigned short)0;

  errno = 0;

  READIT(ServerFD, requiredsize, p, bufcnt, rest);
  if (bufcnt < requiredsize) {
    errno = EPIPE;
    close(ServerFD);
    if (allocptr && bufptr != buf) free(bufptr);
    return -1;
  }

  q = buf + 2;
  if (bufsize > 4) len = S2TOS(q);
  if (len_return) *len_return = len;
  requiredsize = len + HEADER_SIZE;
  if (bufsize < requiredsize) {
    if (allocptr && (bufptr = (BYTE *)malloc(requiredsize))) {
      bcopy(buf, bufptr, bufcnt);
      rest = requiredsize - bufcnt;
      p = bufptr + bufcnt;
    }
    else {
      return -1;
    }
  }
  if (bufcnt < requiredsize) {
    READIT(ServerFD, requiredsize, p, bufcnt, rest);
  }
  if (bufcnt < requiredsize) {
    errno = EPIPE;
    close(ServerFD);
    if (allocptr && bufptr != buf) free(bufptr);
    return -1;
  }
  else {
    if (allocptr && bufptr != buf) *allocptr = bufptr;
    probe("Read: %d\n", bufcnt, buf);
    return 0;
  }
}

static int
WriteServer( Buffer, size )
BYTE *Buffer ;
int size ;
{
    register int todo, retval = 0;
    register int write_stat;
    register BYTE *bufindex;
#ifdef SIGNALRETURNSINT
    static int (*Sig) pro((int));
#else /* !SIGNALRETURNSINT */
    static void (*Sig) pro((int));
#endif /* !SIGNALRETURNSINT */

    errno = 0 ;
    bufindex = Buffer ;
    todo = size ;
    Sig = signal(SIGPIPE, DoSomething);
    while (size) {
	errno = 0;
	probe("Write: %d\n", todo, (char *)bufindex);
	write_stat = write(ServerFD, (char *)bufindex, (int) todo);
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
	    retval = -1;
	    errno = EPIPE ;
	    break;
	}
    }
    signal(SIGPIPE, Sig);
    return retval;
}

static
SendType0Request(proto, len, name) /* Initialize */
long proto, len;
char *name;
{
  BYTE lbuf[PROTOBUF], *bufp = lbuf, *p;
  long sz = 8 + len;
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
    return -1;
  }
}

static
SendType1Request(majo, mino) /* Finalize */
int majo, mino;
{
  BYTE buf[4];

  buf[0] = (BYTE)majo;
  buf[1] = (BYTE)mino;
  buf[2] = buf[3] = (BYTE)0;

  return WriteServer(buf, sizeof(buf));
}

static
SendType2Request(majo, mino, val) /* DuplicateContext */
int majo, mino, val;
{
  BYTE buf[6], *p = buf;

  *p++ = (BYTE)majo;
  *p++ = (BYTE)mino;
  *p++ = (BYTE)0;
  *p++ = (BYTE)((sizeof(buf)) - HEADER_SIZE);
  STOS2(val, p);

  return WriteServer(buf, sizeof(buf));
}

static
SendType3Request(majo, mino, con, val) /* GetDictionaryList */
int majo, mino, con, val;
{
  BYTE buf[8], *p = buf;

  *p++ = (BYTE)majo;
  *p++ = (BYTE)mino;
  *p++ = (BYTE)0;
  *p++ = (BYTE)((sizeof(buf)) - HEADER_SIZE);
  STOS2(con, p); p += SIZEOFSHORT;
  STOS2(val, p);

  return WriteServer(buf, sizeof(buf));
}

static
SendType4Request(majo, mino, con, bgn, end, wstr, wlen) /* SubstYomi */
int majo, mino, con, bgn, end, wlen;
Ushort *wstr;
{
    int sz = HEADER_SIZE + SIZEOFSHORT * 4 + (SIZEOFSHORT * (wlen + 1));
    int len, i, retval;
    Ushort *wp;
    BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;

    if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
      p = bufp;
      *p++ = majo;
      *p++ = mino;
      len = sz - HEADER_SIZE;
      STOS2(len, p); p += SIZEOFSHORT;
      STOS2(con, p); p += SIZEOFSHORT;
      STOS2(bgn, p); p += SIZEOFSHORT;
      STOS2(end, p); p += SIZEOFSHORT;
      STOS2(wlen, p); p += SIZEOFSHORT;

      for (wp = wstr, i = 0 ; i < wlen ; wp++, i++) {
	STOS2(*wp, p); p += SIZEOFSHORT;
      }
      p[0] = p[1] = (BYTE)0;

      retval = WriteServer(bufp, sz);
      if (bufp != lbuf) free((char *)bufp);
      return retval;
    }
    else {
      return -1;
    }
}

static
SendType5Request(majo, mino, con, val, mod) /* AutoConvert */
int majo, mino, con, val, mod;
{
  BYTE buf[12], *p = buf;

  *p++ = (BYTE)majo;
  *p++ = (BYTE)mino;
  *p++ = (BYTE)0;
  *p++ = (BYTE)((sizeof(buf)) - HEADER_SIZE);
  STOS2(con, p); p += SIZEOFSHORT;
  STOS2(val, p); p += SIZEOFSHORT;
  LTOL4(mod, p);

  return WriteServer(buf, sizeof(buf));
}

static
SendType6Request(majo, mino, con, bun, val) /* GetYomi */
int majo, mino, con, val;
{
  BYTE buf[10], *p = buf;

  *p++ = (BYTE)majo;
  *p++ = (BYTE)mino;
  *p++ = (BYTE)0;
  *p++ = (BYTE)((sizeof(buf)) - HEADER_SIZE);
  STOS2(con, p); p += SIZEOFSHORT;
  STOS2(bun, p); p += SIZEOFSHORT;
  STOS2(val, p);

  return WriteServer(buf, sizeof(buf));
}

#define SendType8Request SendType9Request /* GetHinshi */

static
SendType9Request(majo, mino, con, bun, cand, val) /* GetLex */
int majo, mino, con, bun, cand, val;
{
  BYTE buf[12], *p = buf;

  *p++ = (BYTE)majo;
  *p++ = (BYTE)mino;
  *p++ = (BYTE)0;
  *p++ = (BYTE)((sizeof(buf)) - HEADER_SIZE);
  STOS2(con, p); p += SIZEOFSHORT;
  STOS2(bun, p); p += SIZEOFSHORT;
  STOS2(cand, p); p += SIZEOFSHORT;
  STOS2(val, p);

  return WriteServer(buf, sizeof(buf));
}

static
SendType10Request(majo, mino, cx, n, mod) /* EndConvert */
int majo, mino, n, mod;
RkcContext *cx;
{
    int sz = HEADER_SIZE + SIZEOFSHORT * 2 + SIZEOFLONG + (SIZEOFSHORT * n);
    int len, i, con = (int)cx->server, retval;
    BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
    RkcBun *bun;

    if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
      p = bufp;
      *p++ = majo;
      *p++ = mino;
      len = sz - HEADER_SIZE;
      STOS2(len, p); p += SIZEOFSHORT;
      STOS2(con, p); p += SIZEOFSHORT;
      STOS2(n, p); p += SIZEOFSHORT;
      LTOL4(mod, p); p += SIZEOFLONG;

      for (bun = cx->bun, i = 0 ; i < n ; bun++, i++) {
	short cand = (bun->curcand < bun->maxcand) ?  bun->curcand : 0;
	STOS2(cand, p); p += SIZEOFSHORT;
      }
      retval = WriteServer(bufp, sz);
      if (bufp != lbuf) free((char *)bufp);
      return retval;
    }
    else {
      return -1;
    }
}

static
SendType11Request(majo, mino, con, bun, wstr, wlen) /* StoreYomi */
int majo, mino, con, bun, wlen;
Ushort *wstr;
{
    int sz = HEADER_SIZE + SIZEOFSHORT * 2 + (SIZEOFSHORT * wlen);
    Ushort *wp;
    int len, i, retval;
    BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;

    if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
      p = bufp;
      *p++ = majo;
      *p++ = mino;
      len = sz - HEADER_SIZE;
      STOS2(len, p); p += SIZEOFSHORT;
      STOS2(con, p); p += SIZEOFSHORT;
      STOS2(bun, p); p += SIZEOFSHORT;

      for (wp = wstr, i = 0 ; i < wlen ; wp++, i++) {
	STOS2(*wp, p); p += SIZEOFSHORT;
      }
      p[0] = p[1] = (BYTE)0;

      retval = WriteServer(bufp, sz);
      if (bufp != lbuf) free((char *)bufp);
      return retval;
    }
    else {
      return -1;
    }
}

static
SendType12Request(majo, mino, con, wstr, str) /* DefineWord */
int majo, mino, con;
Ushort *wstr;
char *str;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int slen = strlen(str) + 1, wlen = ushortstrlen(wstr) + 1, i;
  int sz = HEADER_SIZE + SIZEOFSHORT + (SIZEOFSHORT * wlen) + slen, len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    STOS2(con, p); p += SIZEOFSHORT;
    for (i = 0 ; i < wlen ; i++) {
      Ushort wch = wstr[i];

      STOS2(wch, p); p += SIZEOFSHORT;
    }
    bcopy(str, p, slen);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

static
SendType13Request(majo, mino, con, str, wstr, wlen, mxk, mxh) 
                                                   /* GetSimpleKanji */
int majo, mino, con, wlen, mxk, mxh;
Ushort *wstr;
char *str;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int slen = strlen(str) + 1, i;
  int sz = HEADER_SIZE + SIZEOFSHORT + (SIZEOFSHORT * (wlen + 1)) + slen;
  int len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    STOS2(con, p); p += SIZEOFSHORT;
    bcopy(str, p, slen); p += slen;
    for (i = 0 ; i < wlen ; i++) {
      Ushort wch = wstr[i];

      STOS2(wch, p); p += SIZEOFSHORT;
    }
    p[0] = p[1] = (BYTE)0; p += SIZEOFSHORT;

    STOS2(wlen, p); p += SIZEOFSHORT;
    STOS2(mxk, p); p += SIZEOFSHORT;
    STOS2(mxh, p);
    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

static
SendType14Request(majo, mino, mod, con, wstr, wlen) /* BeginConvert */
int majo, mino, mod, con, wlen;
Ushort *wstr;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int sz = HEADER_SIZE + SIZEOFLONG + SIZEOFSHORT + (SIZEOFSHORT * (wlen + 1));
  int len, res;
  int i;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    LTOL4(mod, p); p += SIZEOFLONG;
    STOS2(con, p); p += SIZEOFSHORT;
    for (i = 0 ; i < wlen ; i++) {
      Ushort wch = wstr[i];

      STOS2(wch, p); p += SIZEOFSHORT;
    }
    p[0] = p[1] = (BYTE)0;

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

static
SendType15Request(majo, mino, mod, con, str) /* MountDictionary */
int majo, mino, mod, con;
char *str;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int slen = strlen(str) + 1;
  int sz = HEADER_SIZE + SIZEOFLONG + SIZEOFSHORT + slen, len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    LTOL4(mod, p); p += SIZEOFLONG;
    STOS2(con, p); p += SIZEOFSHORT;
    bcopy(str, p, slen);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

static
SendType16Request(majo, mino, mod, con, ostr, nstr) /* RenameDictionary */
int majo, mino, mod, con;
char *ostr, *nstr;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int oslen = strlen(ostr) + 1, nslen = strlen(nstr) + 1;
  int sz = HEADER_SIZE + SIZEOFLONG + SIZEOFSHORT + oslen + nslen;
  int len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    LTOL4(mod, p); p += SIZEOFLONG;
    STOS2(con, p); p += SIZEOFSHORT;
    bcopy(ostr, p, oslen); p += oslen;
    bcopy(nstr, p, nslen);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

static
SendType17Request(majo, mino, str, slen) /* QueryExtension */
int majo, mino, slen;
char *str;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int sz = HEADER_SIZE + slen, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    STOS2(slen, p); p += SIZEOFSHORT;
    bcopy(str, p, slen);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

#ifdef EXTENSION
static
SendType18Request(majo, mino, con, str1, s1len, str2, s2len, val) /* ListDictionary */
int majo, mino, con, s1len, s2len, val;
char *str1, *str2;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int sz = HEADER_SIZE + SIZEOFSHORT + s1len + s2len + SIZEOFSHORT;
  int res, len;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    STOS2(con, p); p += SIZEOFSHORT;
    bcopy(str1, p, s1len); p += s1len;
    if (str2 != (char *)0) {
      bcopy(str2, p, s2len); p += s2len;
    }
    STOS2(val, p);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}
#endif /* EXTENSION */

static
SendType19Request(majo, mino, mod, con, ustr, dstr) /* QueryDictionary */
int majo, mino, mod, con;
char *ustr, *dstr;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int uslen = strlen(ustr) + 1, dslen = strlen(dstr) + 1;
  int sz = HEADER_SIZE + SIZEOFLONG + SIZEOFSHORT + uslen + dslen;
  int len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    LTOL4(mod, p); p += SIZEOFLONG;
    STOS2(con, p); p += SIZEOFSHORT;
    bcopy(dstr, p, dslen); p += dslen;
    bcopy(ustr, p, uslen);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

static
SendType20Request(majo, mino, con, cmd, dsz, data, bsz) /* Through */
int majo, mino, con, cmd, dsz, bsz;
char *data;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int sz = HEADER_SIZE + SIZEOFSHORT + SIZEOFLONG * 2 + dsz;
  int len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    STOS2(con, p); p += SIZEOFSHORT;
    LTOL4(cmd, p); p += SIZEOFLONG;
    LTOL4(bsz, p); p += SIZEOFLONG;
    bcopy(data, p, dsz);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

/* Copy Dic のため */

static
SendType21Request(majo, mino, mod, con, dirstr, ostr, nstr) 
                                                        /* CopyDictionary */
int majo, mino, mod, con;
char *dirstr, *ostr, *nstr;
{
  BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
  int dirlen = strlen(dirstr) + 1; 
  int oslen = strlen(ostr) + 1, nslen = strlen(nstr) + 1;
  int sz = HEADER_SIZE + SIZEOFLONG + SIZEOFLONG + SIZEOFSHORT 
                                                +dirlen + oslen + nslen;
  int len, res;

  if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
    p = bufp;

    *p++ = (BYTE)majo;
    *p++ = (BYTE)mino;
    len = sz - HEADER_SIZE;
    STOS2(len, p); p += SIZEOFSHORT;
    LTOL4(mod, p); p += SIZEOFLONG;
    STOS2(con, p); p += SIZEOFSHORT;
    bcopy(dirstr, p, dirlen); p += dirlen;
    bcopy(ostr, p, oslen); p += oslen;
    bcopy(nstr, p, nslen);

    res = WriteServer(bufp, sz);
    if (bufp != lbuf) free((char *)bufp);
    return res;
  }
  else {
    return -1;
  }
}

/* ここまで */

static
RecvType0Reply(rep) /* Initialize */
int *rep;
{
  BYTE buf[4];
  long res;

  if (ReadServer(buf, 4, 0, 0) < 0) {
    return -1;
  }
  else {
    res = L4TOL(buf);
    *rep = (int)res;
    return 0;
  }
}

static
RecvType1Reply(n, vmajp, vminp) /* GetServerInfo */
int *n, *vmajp, *vminp;
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  int sz, retval;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    *n = retval = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    *n = (int)I8toI32(*p); p++;
    *vmajp = (int)I8toI32(*p); p++;
    *vminp = (int)I8toI32(*p);
    retval = 0;
    if (bufp != lbuf) free((char *)bufp);
  }
  return retval;
}

static
RecvType2Reply(rep) /* Finalize */
int *rep;
{
  BYTE buf[5];

  if (ReadServer(buf, 5, 0, 0) < 0) {
    return -1;
  }
  else {
    *rep = (int)I8toI32(buf[4]);
    return 0;
  }
}

static RecvType3Reply pro((int *, int (*)(int, BYTE *, int, BYTE *), BYTE *));

static
RecvType3Reply(n, storefunc, extdata) /* GetHinshi */
int *n, (*storefunc) pro((int, BYTE *, int, BYTE *));
BYTE *extdata;
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  int sz, datalen, retval;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    retval = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    retval = (int)I8toI32(*p++);
    datalen = sz - sizeof(char);

    if (storefunc && (*storefunc)(retval, p, datalen, extdata) < 0) {
      *n = retval = -1;
    }
    else {
      *n = retval;
      retval = 0;
    }
    if (bufp != lbuf) free((char *)bufp);
  }
  return retval;
}

static RecvType4Reply pro((int *, int (*)(int, BYTE *, BYTE *), BYTE *));

static
RecvType4Reply(n, storefunc, extdata) /* GetStatus */
int *n, (*storefunc) pro((int, BYTE *, BYTE *));
BYTE *extdata;
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  short res;
  int sz, retval;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    retval = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    res = *p++;
    retval = (int)I8toI32(res);

    if (storefunc && (*storefunc)(retval, p, extdata) < 0) {
      *n = retval = -1;
    }
    else {
      *n = retval;
      retval = 0;
    }
    if (bufp != lbuf) free((char *)bufp);
  }
  return retval;
}

static
RecvType5Reply(rep) /* CreateContext */
int *rep;
{
  BYTE buf[6], *p;
  short res;

  if (ReadServer(buf, 6, 0, 0) < 0) {
    return -1;
  }
  else {
    p = buf + 4;
    res = (short)S2TOS(p);
    *rep = (int)I16toI32(res);
    return 0;
  }
}

static
RecvType6Reply(buf, mxi, n) /* GetDictionaryList */
BYTE *buf;
int mxi, *n;
/* ARGSUSED */
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  short res;
  int sz;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    res = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    res = (short)S2TOS(p); p += SIZEOFSHORT;
    *n = (int)I16toI32(res);
 /* 次の bcopy はサーバは mxi 以内の長さしか返さないと信じてノーチェック */
    bcopy(p, buf, sz - SIZEOFSHORT);
    res = 0;
    if (bufp != lbuf) free((char *)bufp);
  }
  return res;
}

static RecvType7Reply pro((int *, int (*)(int, BYTE *, int, BYTE *), BYTE *));

static
RecvType7Reply(n, storefunc, extdata) /* BeginConvert */
int *n, (*storefunc) pro((int, BYTE *, int, BYTE *));
BYTE *extdata;
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  short res;
  int sz, datalen, retval;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    retval = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    res = (short)S2TOS(p); p += SIZEOFSHORT;
    datalen = sz - SIZEOFSHORT;
    retval = (int)I16toI32(res);

    if (storefunc && (*storefunc)(retval, p, datalen, extdata) < 0) {
      *n = retval = -1;
    }
    else {
      *n = retval;
      retval = 0;
    }
    if (bufp != lbuf) free((char *)bufp);
  }
  return retval;
}

static RecvType8Reply
  pro((int *, int (*)(int, BYTE *, BYTE *, BYTE *), BYTE *, BYTE *));

static
RecvType8Reply(n, storefunc, kdata, hdata) /* GetSimpleKanji */
int *n, (*storefunc) pro((int, BYTE *, BYTE *, BYTE *));
BYTE *kdata, *hdata;
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  short res;
  int sz, retval;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    retval = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    res = (short)S2TOS(p); p += SIZEOFSHORT;
    retval = (int)I16toI32(res);

    if (storefunc && (*storefunc)(retval, p, kdata, hdata) < 0) {
      *n = retval = -1;
    }
    else {
      *n = retval;
      retval = 0;
    }
    if (bufp != lbuf) free((char *)bufp);
  }
  return retval;
}

#define RecvType9Reply RecvType7Reply /* GetLex */

static
RecvType10Reply(n, buf, bsz) /* Through */
int *n, bsz;
char *buf;
{
  BYTE lbuf[RECVBUFSIZE], *p, *bufp = lbuf;
  int sz, retval, datalen;

  if (ReadServer(lbuf, RECVBUFSIZE, &sz, &bufp) < 0) {
    retval = -1;
  }
  else {
    p = bufp + HEADER_SIZE;
    retval = (int)L4TOL(p); p += SIZEOFLONG;
    datalen = sz - SIZEOFLONG;
    bzero(buf, bsz);
    bcopy(p, buf, datalen);
    *n = retval;
    retval = 0;
    if (bufp != lbuf) free((char *)bufp);
  }
  return retval;
}

static
rkcw_initialize( username )
char *username ;
{
  int reply;
  long len = strlen( (char *)username ) + 1 ;

  if (SendType0Request(wInitialize, len, username) == 0 &&
      RecvType0Reply(&reply) == 0) {
    if (reply < 0) {
      close(ServerFD);
    }
    return reply;
  }
  return -1;
}

static
rkcw_finalize()
{
  int reply;

  if (SendType1Request(wFinalize, 0) == 0 &&
      RecvType2Reply(&reply) == 0) {
    (void)close( ServerFD ) ;
    return reply;
  }
  return -1;
}

static
rkcw_create_context()
{
  int context;

  if (SendType1Request(wCreateContext, 0) == 0 &&
      RecvType5Reply(&context) == 0) {
    return context;
  }
  return -1;
}

static
rkcw_duplicate_context( cx )
register RkcContext *cx ;
{
  int context;

  if (SendType2Request(wDuplicateContext, 0, (int)cx->server) == 0 &&
      RecvType5Reply(&context) == 0) {
    return context;
  }
  return -1;
}

static
rkcw_close_context( cx )
register RkcContext *cx ;
{
  int reply;

  if (SendType2Request(wCloseContext, 0, (int)cx->server) == 0 &&
      RecvType2Reply(&reply) == 0) {
    return reply;
  }
  return -1;
}

static
dictionary_list(proto, con, dicnames, mxi)
int proto, con;
char *dicnames ;
int mxi;
{
  int res;

  if (SendType3Request(proto, 0, con, mxi) == 0 &&
      RecvType6Reply((BYTE *)dicnames, mxi, &res) == 0) {
    return res;
  }
  return -1;
}

static
rkcw_dictionary_list( cx, dicnames, mxi)
register RkcContext *cx ;
char *dicnames ;
int mxi ;
{
  return dictionary_list(wGetDictionaryList, (int)cx->server, dicnames, mxi);
}

static
define_dic(proto, cx, dicname, wordrec)
int proto;
register RkcContext *cx ;
char *dicname ;
Ushort *wordrec ;
{
  int reply;

  if (SendType12Request(proto, 0, (int)cx->server, wordrec, dicname) == 0 &&
      RecvType2Reply(&reply) == 0) {
    return reply;
  }
  return -1;
}

static
rkcw_define_dic( cx, dicname, wordrec)
register RkcContext *cx ;
char *dicname ;
Ushort *wordrec ;
{
  return define_dic(wDefineWord, cx, dicname, wordrec);
}

static
rkcw_delete_dic( cx,  dicname, wordrec)
register RkcContext *cx ;
char *dicname ;
Ushort *wordrec ;
{
  return define_dic(wDeleteWord, cx, dicname, wordrec);
}

static mount_dictionary pro((int, int, int, char *, int));

static
mount_dictionary(majo, mino, context, data, mode)
int majo, mino, context, mode ;
char *data;
{
  int reply;

  if (SendType15Request(majo, mino, mode, context, data) == 0 &&
      RecvType2Reply(&reply) == 0){
    return reply;
  }
  return -1;
}

static
rkcw_mount_dictionary( cx, dicname, mode )
register RkcContext *cx ;
char *dicname ;
int  mode ;
{
  return mount_dictionary(wMountDictionary, 0, (int)cx->server, dicname, mode);
}

static
rkcw_umount_dictionary( cx, dicname )
register RkcContext *cx ;
char *dicname ;
{
  return mount_dictionary(wUnmountDictionary, 0, (int)cx->server, dicname, 0);
}

static
rkcw_remount_dictionary( cx, dicname, where )
register RkcContext *cx ;
char *dicname ;
int where ;
{
  return mount_dictionary(wRemountDictionary, 0,
			  (int)cx->server, dicname, where);
}

static
rkcw_mount_list( cx, dicnames, mxi)
register RkcContext *cx ;
char *dicnames ;
int mxi;
{
  return dictionary_list(wGetMountDictionaryList,
			 (int)cx->server, dicnames, mxi);
}

#if 0 /* this is not used */
static
rkcw_get_dir_list( cx, ddname, maxddname )
register RkcContext *cx ;
char *ddname ;
int maxddname ;
{
  return dictionary_list(wGetDirectoryList, (int)cx->server,
			 ddname, maxddname);
}
#endif

static
end_convert(proto, cx, n, mod)
int proto, n, mod;
RkcContext *cx;
{
  int reply;

  if (SendType10Request(proto, 0, cx, n, mod) == 0 &&
      RecvType2Reply(&reply) == 0) {
    return reply;
  }
  return -1;
}

static
rkcw_convert_end( cx, mode )
RkcContext *cx ;
int mode ;
{
  return end_convert(wEndConvert, cx, cx->maxbun, mode);
}

static
convStore(n, data, len, dest)
int n, len;
BYTE *data, *dest;
{
  RkcContext *cx = (RkcContext *)dest;

  if (firstKouhoStore(n, data, len, (BYTE *)cx) < 0) {
    rkcw_convert_end(cx, 0); /* サーバ側もRkBgnBunを終了 */
    return -1;
  }
  return n;
}

static
rkcw_convert( cx, yomi, length, mode )
RkcContext *cx ;
int length ,mode;
Ushort *yomi ;
{
  int n;

  if (SendType14Request(wBeginConvert, 0, mode,
			(int)cx->server, yomi, length) == 0&&
      RecvType7Reply(&n, convStore, (BYTE *)cx) == 0) {
    return n;
  }
  return -1;
}

/*
 get_yomi()

 storefunc に 0 をいれたらどうなるか知らないからね。
 */

static get_yomi
  pro((int, int, int, int (*)(int, BYTE *, int, BYTE *), BYTE *));

static
get_yomi(proto, context, curbun, storefunc, yomibuf)
int proto, context, curbun, (*storefunc) pro((int, BYTE *, int, BYTE *));
BYTE *yomibuf;
{
  int n;

  if (SendType6Request(proto, 0, context, curbun, BUFSIZE) == 0 &&
      RecvType7Reply(&n, storefunc, yomibuf) == 0) {
    return n;
  }
  return -1;
}

static
yomiStore(n, data, len, destb)
int n, len;
BYTE *data;
BYTE *destb;
{
  Ushort *dest = (Ushort *)destb;

  if (!(n < 0)) {
    int i;

    len /= SIZEOFSHORT;
    for (i = 0 ; i < len ; i++) {
      *dest = S2TOS(data); data += SIZEOFSHORT; dest++;
    }
  }
  return 0;
}

static
rkcw_get_yomi( cx, yomip ) /* yomip のサイズは無限大かよ？ */
register RkcContext *cx ;
Ushort *yomip ;
{		
  return get_yomi(wGetYomi, (int)cx->server, cx->curbun,
		  yomiStore, (BYTE *)yomip);
}

static
kanjilistStore(n, data, len, dest)
int n, len;
BYTE *data, *dest;
/* ARGSUSED */
{
  Ushort *p, *wp;

  p = (Ushort *)malloc(len);
  if (p) {
    int i;

    len /= SIZEOFSHORT;
    for (wp = p, i = 0 ; i < len ; i++) {
      *wp = S2TOS(data); data += SIZEOFSHORT; wp++;
    }
    *(Ushort **)dest = p;
    return 0;
  }
  else {
    return -1;
  }
}

static
rkcw_get_kanji_list( cx )
register RkcContext *cx ;
{		
    RkcBun *bun = &cx->bun[ cx->curbun ] ;

    return get_yomi(wGetCandidacyList, (int)cx->server, cx->curbun,
		    kanjilistStore, (BYTE *)&bun->kanji);
}

static
rkcw_resize( cx, yomi_length )
register RkcContext *cx ;
int yomi_length ;
{
  int n;

  if (SendType6Request(wResizePause, 0,
		       (int)cx->server, cx->curbun, yomi_length)
      == 0 &&
      RecvType7Reply(&n, firstKouhoStore, (BYTE *)cx) == 0) {
    return n;
  }
  return -1;
}

static
store_yomi(proto, cx, yomi, maxyomi)
int proto, maxyomi;
RkcContext *cx;
Ushort *yomi ;
{
  int wlen = ushortstrlen(yomi) + 1, n;

  if (maxyomi < wlen) wlen = maxyomi;

  if (SendType11Request(proto, 0, (int)cx->server, cx->curbun, yomi, wlen)
      == 0 && 
      RecvType7Reply(&n, firstKouhoStore, (BYTE *)cx) == 0) {
    return n;
  }
  return -1;
}

static
rkcw_store_yomi(cx, yomi, maxyomi)
register RkcContext *cx ;
Ushort *yomi ;
int maxyomi;
{
  return store_yomi(wStoreYomi, cx, (Ushort *)yomi, maxyomi);
}

static
char *ExtensionRequest = {
#ifdef EXTENSION
    /* Request Name */
    "GetServerInfo\0GetAccessControlList\0CreateDictioinary\0\
DeleteDictioinary\0RenameDictioinary\0GetWordTextDictioinary\0\
ListDictioinary\0"
#else
    ""
#endif /* EXTENSION */
} ;

static
Query_Extension()
{
    int datalen = 0, reply;
    char *reqnames = ExtensionRequest;

    while( *(reqnames + datalen) ){
      datalen += strlen(reqnames + datalen) + 1;
    }
    datalen++;

    /* Request Names は '\0' を含む文字列であるためパケットに載せるべき
       大きさが判りにくいので全体の大きさを datalen に指定する． */
    if (SendType17Request(wQueryExtensions, 0, reqnames, datalen + 1) == 0 &&
	RecvType2Reply(&reply) == 0) {
      return reply;
    }
    return -1;
}

#ifdef EXTENSION
/* ARGSUSED */
static
rkcw_list_dictionary( cx, dirname, dicnames_return, size )
register RkcContext *cx ;
char *dirname, *dicnames_return ;
int size ;
{
    int extension_base = Query_Extension(), n;
    int slen = strlen((char *)dirname) + 1;

    if( extension_base < 0 )
	return( -1 ) ;

    if (SendType18Request(wListDictionary, 1, (int)cx->server, 
			  (char *)dirname, slen, (char *)0, 0, size)
	== 0 &&
        RecvType6Reply((BYTE *)dicnames_return, size, &n) == 0) {
      return n;
    }
    return -1;
}

static
rkcw_create_dictionary( cx, dicname, mode )
register RkcContext *cx ;
char *dicname ;
int mode ;
{
    int extension_base = Query_Extension() ;

    if( extension_base < 0 )
	return( -1 ) ;
    return mount_dictionary(wCreateDictionary, 1,
			    (int)cx->server, dicname, mode);
}

static
rkcw_delete_dictionary( cx, dicname, mode )
register RkcContext *cx ;
char *dicname ;
int mode;
{
    int extension_base = Query_Extension() ;

    if( extension_base < 0 )
	return( -1 ) ;
    return mount_dictionary(wDeleteDictionary, 1, (int)cx->server,
			    dicname, mode);
}

static
rkcw_rename_dictionary( cx, dic, newdic, mode )
register RkcContext *cx;
char *dic, *newdic;
int mode;
{
  int reply;

  if (SendType16Request(wRenameDictionary, 1, mode, (int)cx->server, dic,
			newdic) == 0 &&
      RecvType2Reply(&reply) == 0){
    return reply;
  }
  return -1;
}

/* Copy Dic ここから */

/*
  Protocol Version 3.2 からサポート。それ以前のサーバへは送ってはいけない。
  このチェックは rkc.c で行なうこと。
 */

static
rkcw_copy_dictionary(cx, dir, dic, newdic, mode)
register RkcContext *cx;
char *dir, *dic, *newdic;
int mode;
{
  int reply;

  if (SendType21Request(wCopyDictionary, 1, mode, 
                (int)cx->server, dir, dic, newdic) == 0 &&
                                RecvType2Reply(&reply) == 0){
    return reply;
  }
  return -1;
}
/* ここまで */
/* ARGSUSED */
static
rkcw_get_text_dictionary( cx, dirname, dicname, info, infolen )	
register RkcContext *cx ;
char *dirname, *dicname ;
Ushort *info ;
int infolen ;
{
    int extension_base = Query_Extension() ;
    int dirlen = strlen((char *)dirname) + 1;
    int diclen = strlen((char *)dicname) + 1;
    int n, retval = -1;

    if( extension_base < 0 )
	return( -1 ) ;

    if (SendType18Request(wGetWordTextDictionary, 1, (int)cx->server,
			  dirname, dirlen, dicname, diclen,
			  infolen) == 0&&
	RecvType7Reply(&n, yomiStore, (BYTE *)info) == 0) {
      retval = n;
    }
    return retval;
}

int
rkcw_get_server_info( majorp, minorp )
int *majorp, *minorp;
{
  int reply, vmajp, vminp;

  if (SendType1Request(wGetServerInfo, 1) == 0 &&
      RecvType1Reply(&reply, &vmajp, &vminp) == 0) {
    *majorp = vmajp;
    *minorp = vminp;
    return reply;
  }
  return -1;
}
#endif /* EXTENSION */

static
statusStore(n, data, dest)
int n;
BYTE *data;
RkStat *dest;
{
  if (!(n < 0)) {
    dest->bunnum = (int)L4TOL(data);	/* bunsetsu bangou */
    data += SIZEOFLONG;
    dest->candnum = (int)L4TOL(data);	/* kouho bangou */
    data += SIZEOFLONG;
    dest->maxcand = (int)L4TOL(data);	/* sou kouho suu */
    data += SIZEOFLONG;
    dest->diccand = (int)L4TOL(data);	/* jisho ni aru kouho suu */
    data += SIZEOFLONG;
    dest->ylen = (int)L4TOL(data);	/* yomigana no nagasa (in byte) */ 
    data += SIZEOFLONG;
    dest->klen = (int)L4TOL(data);	/* kanji no nagasa (in byte) */
    data += SIZEOFLONG;
    dest->tlen = (int)L4TOL(data);	/* tango no kosuu */
  }
  return 0;
}

static
rkcw_get_stat( cx, stat )
register RkcContext *cx ;
RkStat *stat ;
{
    RkcBun *bun = &cx->bun[cx->curbun];
    int n, retval = -1;

    if (SendType6Request(wGetStatus, 0, (int)cx->server,
			 cx->curbun, bun->curcand) == 0 &&
	RecvType4Reply(&n, statusStore, (BYTE *)stat) == 0) {
	retval = n;
    }
    return retval;
}

static
lexStore(n, data, dlen, dest)
int n, dlen;
BYTE *data;
RkLex *dest;
/* ARGSUSED */
{
  int i;

  for (i = 0; i < n; i++, dest++) {
    dest->ylen = (int)L4TOL(data);	/* yomigana no nagasa (in byte) */ 
    data += SIZEOFLONG;
    dest->klen = (int)L4TOL(data);	/* kanji no nagasa (in byte) */
    data += SIZEOFLONG;
    dest->rownum = (int)L4TOL(data);	/* row number */
    data += SIZEOFLONG;
    dest->colnum = (int)L4TOL(data);	/* column number */
    data += SIZEOFLONG;
    dest->dicnum = (int)L4TOL(data);	/* dic number */
    data += SIZEOFLONG;
  }
  return 0;
}

static
rkcw_get_lex( cx, mxi, info )
register RkcContext *cx;
int mxi;
RkLex *info;
{
    RkcBun *bun = &cx->bun[cx->curbun];
    int n, retval = -1;

    if (SendType9Request(wGetLex, 0, (int)cx->server,
			 cx->curbun, bun->curcand, mxi)	== 0 &&
	RecvType9Reply(&n, lexStore, (BYTE *)info) == 0) {
	retval = n;
    }
    return retval;
}

/* 逐次変換に必要な関数 */

static
rkcw_autoconv( cx, length, mode )
RkcContext *cx;
int length, mode;
{
  int reply;

  if (SendType5Request(wAutoConvert, 0, (int)cx->server, length, mode) == 0 &&
      RecvType2Reply(&reply) == 0) {
    return reply;
  }
  return -1;
}

static
rkcw_subst_yomi( cx, nbun, y_start, y_end, yomi, y_len )
register RkcContext *cx ;
int nbun, y_start, y_end, y_len ;
Ushort *yomi ;
/* ARGSUSED */
{
  int n;

  if (SendType4Request(wSubstYomi, 0, (int)cx->server, y_start, y_end,
		       yomi, y_len) == 0 &&
      RecvType7Reply(&n, firstKouhoStore_2, (BYTE *)cx) == 0) {
    return n;
  }
  return -1;
}

static
rkcw_flush_yomi( cx )
register RkcContext *cx ;
{		
  int n;

  if (SendType10Request(wFlushYomi, 0, cx, cx->maxbun, 0) == 0 &&
      RecvType7Reply(&n, firstKouhoStore, (BYTE *)cx) == 0) {
    return n;
  }
  return -1;
}

static
rkcw_get_last_yomi( cx, yomi, maxyomi )
register RkcContext *cx ;
Ushort *yomi ;
int maxyomi ;
{
  int n;

  if (SendType3Request(wGetLastYomi, 0, (int)cx->server, maxyomi) == 0 &&
      RecvType7Reply(&n, yomiStore, (BYTE *)yomi) == 0) {
    return n;
  }
  return -1;
}

static
rkcw_remove_bun( cx, mode )
RkcContext *cx ;
int mode ;
{
    register Ushort *return_kouho;
    register int i, len, stat, curbun;
    Ushort *first_kouho = cx->Fkouho;

    stat = end_convert(wRemoveYomi, cx, cx->curbun, mode);

    if( stat < 0 )
	return( -1 );

    /* 削除すべきバッファの大きさを調べる */
    len = 0;
    curbun = cx->curbun + 1;
    for( i = 0; i < curbun; i++ )
	len += ushortstrlen( first_kouho + len ) + 1;
 
    /* コピーすべきバッファの大きさを調べる */
    first_kouho += len;
    len = 0;
    for( i = curbun; i < cx->maxbun; i++ )
	len += ushortstrlen( first_kouho + len ) + 1;

    if( len > 0 ){
	if( !(return_kouho = (Ushort *)malloc( len * SIZEOFSHORT )) )
	    return( -1 );
  
	bcopy( first_kouho, return_kouho, len * SIZEOFSHORT );
	RkcFree( (char *)cx->Fkouho );
	cx->Fkouho = return_kouho;
    }
    return( stat );
}

static
simpleKanjiStore(n, data, kdest, hdest)
int n;
BYTE *data;
Ushort *kdest, *hdest;
{
  if (!(n < 0)) {
    int i;
    Ushort *wp;

    wp = kdest;
    for (i = 0; i < n; i++) {
      while( *data ){
	*wp = S2TOS(data); data += SIZEOFSHORT; wp++;
      }
      wp[0] = (Ushort)0; data += SIZEOFSHORT; wp++;
    }
    wp[0] = (Ushort)0; data += SIZEOFSHORT; wp++;

    wp = hdest;
    for (i = 0; i < n; i++) {
      while( *data ){
	*wp = S2TOS(data); data += SIZEOFSHORT; wp++;
      }
      wp[0] = (Ushort)0; data += SIZEOFSHORT; wp++;
    }
    wp[0] = (Ushort)0;
  }
  return 0;
}

static
rkcw_get_simple_kanji( cx, dic, yomi, mxy, kan, mxk, hin, mxh )
register RkcContext *cx ;
char *dic;
Ushort *yomi, *kan, *hin;
int mxy, mxk, mxh ;
{
  int n;

  if (SendType13Request(wGetSimpleKanji, 0, (int)cx->server, dic, yomi,
			mxy, mxk, mxh) == 0 &&
      RecvType8Reply(&n, simpleKanjiStore, (BYTE *)kan, (BYTE *)hin) == 0) {
    return n;
  }
  return -1;
}

BYTE *
copyS8(src, des, maxlen)
BYTE *src, *des;
int maxlen;
{
  BYTE *p = src, *q = des, *r = des + maxlen - 1; /* 1 for EOS */

  if (!q) r = q; /* don't copy */

  while (*p) {
    if (q < r) {
      *q++ = *p;
    }
    p++;
  }
  if (q) {
    *q = '\0';
  }
  return p + 1;
}

static
dicinfoStore(n, data, dest)
int n;
BYTE *data;
struct DicInfo *dest;
{
  int mlen = dest->di_count;

  if (!(n < 0)) {
    data = copyS8(data, dest->di_dic, mlen);
    data = copyS8(data, dest->di_file, mlen);
    dest->di_kind = (int)L4TOL(data); data += SIZEOFLONG;
    dest->di_form = (int)L4TOL(data); data += SIZEOFLONG;
    dest->di_count = (unsigned)L4TOL(data); data += SIZEOFLONG;
    dest->di_mode = (int)L4TOL(data); data += SIZEOFLONG;
    dest->di_time = (long)L4TOL(data);
  }
  return 0;
}

static
rkcw_query_dic(cx, usrname, dicname, info)
RkcContext *cx;
char *usrname, *dicname;
struct DicInfo *info;
{
  int reply;

  if (SendType19Request(wQueryDictionary, 0, 0, (int)cx->server,
			usrname, dicname) == 0 &&
      RecvType4Reply(&reply, dicinfoStore, (BYTE *)info) == 0) {
    return reply;
  }
  return -1;
}

static
rkcw_get_hinshi( cx, dst, mxd )
register RkcContext *cx;
Ushort *dst;
int mxd;
{
  RkcBun *bun = &cx->bun[cx->curbun];
  int reply;

  if (SendType8Request(wGetHinshi, 0, (int)cx->server, cx->curbun,
		       bun->curcand, mxd)
      == 0 && RecvType3Reply(&reply, yomiStore, (BYTE *)dst) == 0) {
    return reply;
  }
  return -1;
}


static
rkcw_store_range(cx, yomi, maxyomi)
register RkcContext *cx ;
Ushort *yomi ;
int maxyomi;
{
  return store_yomi(wStoreRange, cx, yomi, maxyomi);
}

static
rkcw_set_locale( cx, locale )
register RkcContext *cx ;
char *locale ;
{
  return mount_dictionary(wSetLocale, 0, (int)cx->server, locale, 0);
}

static
rkcw_sync(cx, dicname)
register RkcContext *cx;
char *dicname;
{
  return mount_dictionary(wSync, 1, (int)cx->server, dicname, 0);
}

static
rkcw_set_app_name( cx, apname )
register RkcContext *cx;
char *apname;
{
  return mount_dictionary(wSetApplicationName, 0, (int)cx->server, apname, 0);
}

/*
  Protocol Version 3.2 からサポート。それ以前のサーバへは送ってはいけない。
  このチェックは rkc.c で行なうこと。
 */
static
rkcw_notice_group_name(cx, groupname)
RkcContext *cx;
char *groupname;
{
  return mount_dictionary(wNoticeGroupName, 0, (int)cx->server, groupname, 0);
}

/*
  Protocol Version 3.2 からサポート。それ以前のサーバへは送ってはいけない。
  このチェックは rkc.c で行なうこと。
 */
static
rkcw_chmod_dic(cx, dicname, mode)
register RkcContext *cx;
char *dicname;
int mode;
{
  int reply;

  if (SendType15Request(wChmodDictionary, 1, mode, (int)cx->server, dicname)
      == 0 && RecvType5Reply(&reply) == 0) {
    return reply;
  }
  return -1;
}

static
rkcw_through( cx, cmd, data, datasz, bufsz )
register RkcContext *cx;
int cmd, datasz, bufsz;
char *data;
{
  int reply;

  if (SendType20Request(wThrough, 0, (int)cx->server, cmd, datasz, data, bufsz)
      == 0 && RecvType10Reply(&reply, data, bufsz) == 0) {
    return reply;
  }
  return -1;
}


struct rkcproto wideproto = {
  rkcw_initialize,
  rkcw_finalize,
  rkcw_close_context,
  rkcw_create_context,
  rkcw_duplicate_context,
  rkcw_dictionary_list,
  rkcw_define_dic,
  rkcw_delete_dic,
  rkcw_mount_dictionary,
  rkcw_remount_dictionary,
  rkcw_umount_dictionary,
  rkcw_mount_list,
  rkcw_convert,
  rkcw_convert_end,
  rkcw_get_kanji_list,
  rkcw_get_stat,
  rkcw_resize,
  rkcw_store_yomi,
  rkcw_get_yomi,
  rkcw_get_lex,
  rkcw_autoconv,
  rkcw_subst_yomi,
  rkcw_flush_yomi,
  rkcw_get_last_yomi,
  rkcw_remove_bun,
  rkcw_get_simple_kanji,
  rkcw_query_dic,
  rkcw_get_hinshi,
  rkcw_store_range,
  rkcw_set_locale,
  rkcw_set_app_name,
  rkcw_notice_group_name,
  rkcw_through,
#ifdef EXTENSION
  rkcw_list_dictionary,
  rkcw_create_dictionary,
  rkcw_delete_dictionary,
  rkcw_rename_dictionary,
  rkcw_get_text_dictionary,
  rkcw_sync,
  rkcw_chmod_dic,
  rkcw_copy_dictionary,
#endif /* EXTENSION */
};
