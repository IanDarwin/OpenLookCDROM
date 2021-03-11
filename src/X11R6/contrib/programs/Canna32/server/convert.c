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
static char rcs_id[] = "@(#) 102.1 $Id: convert.c,v 5.20 1994/05/16 10:16:08 ohmori Exp $";
#endif

/* LINTLIBRARY */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>

#include "net.h"
#include "IR.h"

#if CANNA_LIGHT
#ifdef EXTENSION
#undef EXTENSION
#endif
#else /* !CANNA_LIGHT */
#define USE_EUC_PROTOCOL
#endif /* !CANNA_LIGHT */

#define SIZEOFSHORT 2 /* for protocol */
#define SIZEOFLONG  4 /* for protocol */

#define SENDBUFSIZE 1024

#define ACK0 0
#define ACK1 1
#define ACK2 2
#define ACK3 3
#define CHECK_ACK_BUF_SIZE	(ACK_BUFSIZE + (SIZEOFLONG * 2) )

extern int  errno;

typedef struct {
#if __STDC__
  int (*func)(ClientPtr *), (*extdat)(char *, int);
#else
  int (*func)(), (*extdat)();
#endif
} oreqproc;

extern oreqproc Vector[];
#ifdef EXTENSION
extern oreqproc ExtensionVector[];
#endif /* EXTENSION */

static IRReq	Request ;
static IRAck	Acknowledge ;
static char
local_buffer[ LOCAL_BUFSIZE ],
local_buffer2[ LOCAL_BUFSIZE ] ;

unsigned int
TotalRequestTypeCount[ MAXREQUESTNO ] ;
int canna_server_hi = 0 ;
int canna_server_lo = 0 ;

getserver_version()
{
    char version[ 32 ], *buf ;

    strcpy( version, W_VERSION ) ;
    if( version[0] ) {
	if( !(buf = (char *)strtok(version, ".")) ) {
	    return ;
	}
	canna_server_hi = atoi( buf ) ;
	if( !(buf = (char *)strtok((char *)NULL, ".")) ) {
	    return ;
	}
	canna_server_lo = atoi( buf ) ;
    }
}

#ifdef USE_EUC_PROTOCOL
static int
getFirstKouho( cxnum, start, end, status, datap )
int cxnum, start, end, *status;
BYTE **datap;
{
    char *src = local_buffer2 ;
    register char *dst = (char *)*datap;
    BYTE *data_buffer = *datap;
    register int i, len, size = 0, maxsz = SENDBUFSIZE;
    Ushort cbuf[CBUFSIZE];

   ir_debug( Dmsg(5,"最優先候補リスト\n" ); )
    for( i = start; i < end; i++){
	len = RkwGetKanji( cxnum, (Ushort *)cbuf, CBUFSIZE  );
	len = ushort2euc( cbuf, len, src, LOCAL_BUFSIZE ) + 1;
	size += len;
	if( size > maxsz ) {
	    BYTE *local_bufptr;
	    int bufcnt = size - len;

	    maxsz = maxsz * 2;
	    local_bufptr = (BYTE *)malloc(maxsz);
	    if (local_bufptr) {
		ir_debug( Dmsg(8, "malloc size is %d.\n", maxsz) );

		bcopy(*datap, (char *)local_bufptr, bufcnt);
		if( *datap != data_buffer )
		    free( (char *)*datap );
		*datap = local_bufptr;
		dst = (char *)local_bufptr + bufcnt;
	    } else {
		*status = -1;
		PrintMsg( "First Kouho Buffer allocate failed!!\n" );
		return 0;	
	    }
	}
	strcpy(dst, src);
       ir_debug( Dmsg(5,"%d:[%s]", i, dst ); )
	dst += len ;
	RkwRight( cxnum ) ;
    }
   ir_debug( Dmsg(5,"\n" ); )
    *status = i;
    RkwGoTo( cxnum, start ) ;/* 先頭文節をカレント文節に戻しておく */
    return size;
}

static int
listsize(src, cnt)
char *src;
int cnt;
{
    register int i, size = 0, len = 0;

    for( i = 0; i < cnt; i++ ){
	len = strlen(src) + 1;
	size += len;
       ir_debug( Dmsg(5,"%d:[%s] ", i, src ); )
	src += len;
    }
   ir_debug( Dmsg(5,"\n" ); )
    return size;
}

#endif /* USE_EUC_PROTOCOL */

ir_error(clientp)
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *buf = local_buffer ;
    int size ;
	
    while( (size = read( client->id, (char *)buf, LOCAL_BUFSIZE )) == LOCAL_BUFSIZE ) {
#ifdef DEBUG
	Dmsg(5,"ごみデータ\n")	;
	DebugDump( 5, buf, size );
#endif
    }
    return( -1 ) ;
}

static int
CheckVersion( req, client )
Req2 *req ;
ClientPtr client ;
{
    char *data = req->name, *logname ;
    int clienthi, clientlo ;
    char *buf ;

    if( !(buf = (char *)strtok( (char *)data, "." )) )
	return( -1 ) ;
    clienthi = atoi( buf ) ;

    if( !(buf = (char *)strtok((char *)NULL, ":")) )
	return( -1 ) ;
    clientlo = atoi( buf ) ;

    if( !(logname = strtok( (char *)NULL, ":" )) )
	return( -1 ) ;

    strcpy( (char *)req->name, (char *)logname ) ;

   ir_debug( Dmsg( 5,"UserName:[%s]\n", req->name ); )
   ir_debug( Dmsg( 5,"client:hi[%d],lo[%d]\n", clienthi, clientlo ); )
   ir_debug( Dmsg( 5,"server:hi[%d],lo[%d]\n", canna_server_hi, canna_server_lo ); )
#ifndef USE_EUC_PROTOCOL
    if (clienthi < 2) {
      return RETURN_VERSION_ERROR_STAT;
    }
#endif /* USE_EUC_PROTOCOL */
    if( canna_server_hi < clienthi )
	return( RETURN_VERSION_ERROR_STAT ) ;

    client->version_hi = (short)clienthi ;
    client->version_lo = (short)clientlo ;

    if( clienthi < 3 )
	return( clientlo );
    else
	return( canna_server_lo );
}

static int WriteClient();
extern char *ProtoName[] ;

#define SendType0Reply SendTypeE1Reply

static int
SendTypeE1Reply(client, stat)
register ClientPtr client;
int stat;
{
    BYTE buf[4], *p = buf;

    LTOL4(stat, p);

    return WriteClient(client->id, buf, sizeof(buf));
}

#ifdef USE_EUC_PROTOCOL
static int
SendTypeE2Reply(client, stat, cnt, str, slen)
register ClientPtr client;
char *str;
int stat, cnt, slen;
{
    BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
    char *wp;
    int res, dlen = cnt * SIZEOFLONG + slen, sz = 2 * SIZEOFLONG + dlen;
    int nlen, i;

    if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
	p = bufp;
	LTOL4(stat, p); p += SIZEOFLONG;
	LTOL4(dlen, p); p += SIZEOFLONG;

	for (wp = str, i = 0 ; i < cnt ; wp += nlen, i++) {
	    nlen = strlen(wp) + 1;
	    LTOL4(nlen, p);  p += SIZEOFLONG;
	    strcpy((char *)p, wp); p += nlen;
	}

	res = WriteClient(client->id, bufp, sz);
	if (bufp != lbuf) free((char *)bufp);
	return res;
    }
    return -1;
}

static int
SendTypeE3Reply(client, stat, storefunc, extdata, slen)
register ClientPtr client;
int stat, slen, (*storefunc)();
BYTE *extdata;
{
    BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
    int sz = 2 * SIZEOFLONG + slen;
    int res;

    if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
	p = bufp;
	LTOL4(stat, p); p += SIZEOFLONG;
	LTOL4(slen, p); p += SIZEOFLONG;

	if (storefunc)
	    (*storefunc)(client, stat, extdata, p);

	res = WriteClient(client->id, bufp, sz);
	if (bufp != lbuf) free((char *)bufp);
	return res;
    }
    return -1;
}

/* #define SendTypeE4Reply SendTypeE3Reply */
/* IR_GET_LEXは TypeE4Replyでなく TypeE3Replyなので TypeE4はいらない */
/* IR_GET_WORD_DICは TypeE2ではない．これを TypeE4にする */

static int
SendTypeE4Reply(client, stat, cnt, infoptr, slen)
register ClientPtr client;
BYTE *infoptr;
int stat, cnt, slen;
{
    BYTE lbuf[SENDBUFSIZE], *bufp = lbuf, *p;
    int res, sz = 2 * SIZEOFLONG + slen;

    if (sz <= SENDBUFSIZE || (bufp = (BYTE *)malloc(sz))) {
	p = bufp;
	LTOL4(stat, p); p += SIZEOFLONG;
	LTOL4(slen, p); p += SIZEOFLONG;

	LTOL4( cnt, p ) ; p += SIZEOFLONG;
	bcopy( infoptr, p, cnt ) ;

	res = WriteClient(client->id, bufp, sz);
	if (bufp != lbuf) free((char *)bufp);
	return res;
    }
    return -1;
}

/* IR_SER_STATが TypeE5Replyそのものである */

#define SendTypeE5Reply(client, size) \
    WriteClient(client->id, Acknowledge.SendAckBuffer, size)

/* IR_SER_STAT2が TypeE6Replyそのものである */

#define SendTypeE6Reply SendTypeE5Reply

/* IR_HOSTは正確には TypeE2Replyではないので TypeE7Replyを作る */

#define SendTypeE7Reply SendTypeE5Reply

#endif /* USE_EUC_PROTOCOL */

ir_initialize(clientp)
ClientPtr *clientp ;
{
    extern CheckAccessControlList() ;
    Req2 *req = &Request.Request2 ;
    ClientPtr client = *clientp ;
    int cxnum, ret, stat = -1;

    /* プロトコルバージョンのチェック */
    if( (ret = CheckVersion( req, client )) < 0 ) {
	if( SendType0Reply(client, ret) < 0 ) {
	    return( -1 ) ;
	}
	CloseDownClient( client ) ;
	*clientp = (ClientPtr)0;
	return( 0 ) ;
    }

    client->username = malloc(strlen((char *)req->name ) + 1);
    if (client->username)
	strcpy( client->username, (char *)req->name ) ;
    else
      client->username = (char *)NULL ;
    
    /* コンテクスト配列の拡張チェック */
    if(!WidenClientContext(client, N_INIT_CONTEXTS)){
      PrintMsg("コンテクスト配列の拡張に失敗しました。\n");
      exit(1);
    }    

    if( ConnectionSetup( client ) > 0) {
        if (CheckAccessControlList(client->hostaddr, client->username) < 0) {
	    if (SendType0Reply(client, stat) < 0) {
	      return -1;
	    }

	    PrintMsg("[%s](%s) Can't find access control list\n", client->username, ProtoName[ req->Type -1 ] ) ;
	    CloseDownClient( client ) ;
	    *clientp = (ClientPtr)0;
	    return( 0 ) ;
	}
	cxnum = RkwCreateContext() ;
	if( cxnum >= 0 ) {
	    if (SetDicHome( client, cxnum ) > 0) {
	      set_cxt(client, cxnum);
	      stat = ((ret << 0x10) | cxnum);
	    }
	    else {
	      RkwCloseContext(cxnum);
	    }
	}
	
    }

    return SendType0Reply(client, stat);
}

#ifdef USE_EUC_PROTOCOL

ir_finalize(clientp)
register ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;

    if( SendTypeE1Reply(client, 0) < 0 )
	return( -1 ) ;

    /* close処理＆後始末（コンテクストの開放等） */
    CloseDownClient( client ) ;
    *clientp = (ClientPtr)0;
    return( 0 ) ;
}

ir_create_context(clientp)
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    int cxnum, stat = -1;

    cxnum = RkwCreateContext() ;
    if( SetDicHome( client, cxnum ) > 0 ) {
      set_cxt(client, cxnum);
      stat = cxnum;
    } else {	
	Req0 *req0 = &Request.Request0 ;

	RkwCloseContext(cxnum);
	PrintMsg("[%s](%s) Can't set dictionary home\n", client->username, ProtoName[ req0->Type -1 ] ) ;
    }
    return SendTypeE1Reply(client, stat);
}

ir_duplicate_context(clientp)
ClientPtr *clientp ;
{
    Req1 *req = &Request.Request1 ;
    ClientPtr client = *clientp ;
    int cxnum, stat = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	stat = cxnum = RkwDuplicateContext( cxnum );
	if (cxnum >= 0) {
	  if (!set_cxt(client, cxnum)) {
	    RkwCloseContext(cxnum);
	    stat = -1;
	  }
	}
    } else {
	PrintMsg("[%s](%s) Context Err[%d]\n",
		 client->username, ProtoName[ req->Type - 1 ], cxnum ) ;
    }

    return SendTypeE1Reply(client, stat);
}

ir_close_context(clientp)
ClientPtr *clientp ;
{
    extern void off_cxt();
    Req1 *req = &Request.Request1 ;
    ClientPtr client = *clientp ;
    int cxnum, stat = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	stat = RkwCloseContext(cxnum);
	off_cxt(client, cxnum);
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }

    return SendTypeE1Reply(client, stat);
}

ir_dictionary_list(clientp)
ClientPtr *clientp ;
{
    Req3 *req = &Request.Request3 ;
    ClientPtr client = *clientp ;
    char *dicnames = local_buffer ;
    int cxnum, size = 0 ;
    int ret = -1, max;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	max = MIN( req->number, LOCAL_BUFSIZE ) ;
	if( (ret = (int)RkwGetDicList(cxnum, (char *)dicnames, max)) < 0) {
	    ret = 0;
	} else {
	   ir_debug( Dmsg(5,"辞書リスト\n" ); )
	    size = listsize(dicnames, ret);
	}
    } else {
	PrintMsg("[%s](%s) Context Err[%d]\n",
		 client->username, ProtoName[ req->Type - 1 ], cxnum ) ;
    }

    return SendTypeE2Reply(client, ret, (ret < 0)? 0: ret, dicnames, size);
}

ir_get_yomi( clientp )
ClientPtr *clientp ;
{
    Req5 *req = &Request.Request5 ;
    ClientPtr client = *clientp ;
    char *yomi = local_buffer ;
    int ret = -1, cxnum ;
    int size = 0 ;
    Ushort cbuf[CBUFSIZE];

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	int bunsetuno = req->number ;
	int maxyomi = MIN( req->datalen, LOCAL_BUFSIZE ) ;

       ir_debug( Dmsg(5,"maxyomi [ %d ]\n", maxyomi ); )
	if( RkwGoTo(cxnum, bunsetuno) == bunsetuno ) {
	    ret = RkwGetYomi( cxnum, (Ushort *)cbuf, CBUFSIZE );
	    /* ushort2eucが -1を返すことはないので */
	    ret = ushort2euc(cbuf, ret, yomi, maxyomi);
	    if (ret) {
		size = ret + 1;
	    }
	} else {
	    PrintMsg("[%s](%s) bunsetu move failed\n", client->username, ProtoName[ req->Type - 1 ] ) ;
       }
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }
    return SendTypeE2Reply(client, ret, (ret > 0)? 1: 0, yomi, size);
}

ir_set_dic_path( clientp )
ClientPtr *clientp ;
/* ARGSUSED */
{
    return( 0 ) ;
}

ir_define_dic(clientp)
ClientPtr *clientp ;
{
    Req7 *req = &Request.Request7 ;
    ClientPtr client = *clientp ;
    char *dicname, *data ;
    int cxnum, ret = -1;
    Ushort cbuf[CBUFSIZE];

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	dicname = req->dicname ;
	data = req->datainfo ;
       ir_debug( Dmsg(5,"辞書名=%s\n", dicname ); )
       ir_debug( Dmsg(5,"登録するデータ[%s]\n", data );)
	euc2ushort( data, strlen( (char *)data ), cbuf, CBUFSIZE );
	ret = RkwDefineDic( cxnum, (char *)dicname, (Ushort *)cbuf );
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_delete_dic(clientp)
ClientPtr *clientp ;
{
    Req7 *req = &Request.Request7 ;
    ClientPtr client = *clientp ;
    char *dicname, *data ;
    int cxnum, ret = -1;
    Ushort cbuf[CBUFSIZE];

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	dicname = req->dicname ;
	data = req->datainfo ;
       ir_debug( Dmsg(5,"辞書名=%s\n", dicname ); )
       ir_debug( Dmsg(5,"削除するデータ[%s]\n", data ); )
	euc2ushort( data, strlen( (char *)data ), cbuf, CBUFSIZE );
	ret = RkwDeleteDic( cxnum, (char *)dicname, (Ushort *)cbuf );
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_get_dir_list(clientp)
ClientPtr *clientp ;
{
    Req3 *req = &Request.Request3 ;
    ClientPtr client = *clientp ;
    char *dicnames = local_buffer ;
    int cxnum, ret = -1, max;
    int size = 0 ;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	max = MIN( req->number, LOCAL_BUFSIZE ) ;
	
	ret = RkwGetDirList( cxnum, (char *)dicnames, max) ;
	if( ret >= 0 ) {
	   ir_debug( Dmsg(5,"辞書リスト\n" ); )
	    size = listsize(dicnames, ret);
	}
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }	
    return SendTypeE2Reply(client, ret, (ret < 0)? 0: ret, dicnames, size);
}

ir_mount_dictionary(clientp)
ClientPtr *clientp ;
{
    Req8 *req = &Request.Request8 ;
    ClientPtr client = *clientp ;
    char *dicname ;
    int cxnum, mode, ret = -1;
    extern MMountFlag;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	mode = req->mode ;
       ir_debug( Dmsg(5,"dicname = %s\n", req->data ); )
	dicname = req->data ;
	ret = RkwMountDic( cxnum, (char *)dicname, mode | MMountFlag) ;
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_umount_dictionary(clientp)
ClientPtr *clientp ;
{
    Req8 *req = &Request.Request8 ;
    ClientPtr client = *clientp ;
    int cxnum, ret = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
       ir_debug( Dmsg(5,"dicname = %s\n", req->data ); )
	ret = RkwUnmountDic( cxnum, (char *)req->data ) ;
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_rmount_dictionary(clientp)
ClientPtr *clientp ;
{
    Req9 *req = &Request.Request9  ;
    ClientPtr client = *clientp ;
    int cxnum, where ;	
    int ret = -1 ;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	where = req->number ;
       ir_debug( Dmsg(5,"dicname = %s\n", req->data ); )
	ret = RkwRemountDic( cxnum, (char *)req->data, where ) ;
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_mount_list( clientp )
ClientPtr *clientp ;
{
    Req3 *req = &Request.Request3 ;
    ClientPtr client = *clientp ;
    char *dicnames = local_buffer ;
    int cxnum, ret = -1, size = 0;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	ret = RkwGetMountList( cxnum, (char *)dicnames,
			      MIN( req->number, LOCAL_BUFSIZE ) ) ;
	if( ret < 0 ) {
	    ret = 0;
	} else {
	   ir_debug( Dmsg(5,"辞書リスト\n" ); )
	    size = listsize(dicnames, ret);
	}
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }	

    return SendTypeE2Reply(client, ret, (ret < 0)? 0: ret, dicnames, size);
}

ir_convert( clientp )
ClientPtr *clientp ;
{
    Req8 *req = &Request.Request8 ;
    ClientPtr client = *clientp ;
    int cxnum, yomilen, ret, mode ;
    int size = 0 ;
    char *data, lbuf[SENDBUFSIZE], *datap = lbuf;
    Ushort cbuf[CBUFSIZE];
    int stat = -1, len;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	mode = req->mode ;
	yomilen = req->datalen ;
	data = req->data ;
	if( yomilen + 1 <= CHECK_ACK_BUF_SIZE )
	    data[ yomilen + 1 ] = '\0' ;
       ir_debug( Dmsg(5,"読み = %s\n",data ); )
	len = euc2ushort( data, yomilen, cbuf, CBUFSIZE );
	if ((ret = RkwBgnBun( cxnum, (Ushort *)cbuf, len, mode )) >= 0) {
	    /* 最優先候補リストを取得する */
	    size = getFirstKouho(cxnum, 0, ret, &stat, (BYTE **)&datap);
	} else {
	    PrintMsg( "[%s](%s) kana-kanji convert failed\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
	    *datap = '\0' ;
	}
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
	*datap = (char)0 ;
    }	
    ret = SendTypeE2Reply(client, stat, (stat < 0)? 0: stat, datap, size);
    if (datap != lbuf) free((char *)datap);
    return ret;
}

ir_convert_end( clientp )
ClientPtr *clientp ;
{
    Req4 *req = &Request.Request4 ;
    ClientPtr client = *clientp ;
    int cxnum, len, i, mode, ret = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	len = req->number ;
	if( len ) {
	    mode = 1 ;
	    if( RkwGoTo( cxnum, 0 ) != 0 ) {	
		PrintMsg("[%s](%s) ir_convert_end: RkwGoTo failed\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
	    }
	   ir_debug( Dmsg( 5,"学習させる候補\n" ); )
	    /* カレント候補を先頭に移動クライアントが選んだ候補を */	
	    /* ＲＫに知らせる */		
	    for( i = 0; i < len; i++ ){ 
		if( req->kouho[ i ] != RkwXfer( cxnum, req->kouho [ i ] ) ) {
		    PrintMsg("[%s](%s) ir_convert_end: RkwXfer failed\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
		}
	       ir_debug( DebugDispKanji( cxnum, i ); )	
		if( RkwRight( cxnum ) == 0 && i != (len - 1) ) { 	
		    PrintMsg("[%s](%s) ir_convert_end: RkwRight failed\n", client->username, ProtoName[ req->Type - 1 ]	 ) ;
		}
	    }
	   ir_debug( Dmsg( 5,"\n" ); )
	} else {
	    mode = 0 ;
	}
	ret = RkwEndBun( cxnum, mode ) ;
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_get_kanjilist( clientp )
ClientPtr *clientp ;
{
    Req5 *req = &Request.Request5 ;
    ClientPtr client = *clientp ;
    char *kouho = local_buffer ;
    char *yomi, *data ;
    int ret = -1, size = 0, cxnum, len	;
    int bunsetuno, maxkanji ;
    Ushort cbuf[CBIGBUFSIZE], *cbufp;
    register int clen, i;
    char workbuf[CBUFSIZE];

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	bunsetuno = req->number ;
	maxkanji = MIN( req->datalen, LOCAL_BUFSIZE ) ; 
       ir_debug( Dmsg(5,"maxkanji [ %d ]\n", maxkanji ); )
	if( RkwGoTo(cxnum, bunsetuno) == bunsetuno ) {
	    ret = RkwGetKanjiList( cxnum, (Ushort *)cbuf, CBIGBUFSIZE );
	    cbufp = cbuf;
	    for( i = 0; i < ret; i++ )
		cbufp += ushortstrlen( cbufp ) + 1;
	    len = ushort2euc( cbuf, cbufp - cbuf, kouho, maxkanji );
	    clen = RkwGetYomi( cxnum, (Ushort *)cbuf, CBIGBUFSIZE );
	    clen = ushort2euc( cbuf, clen, workbuf, CBUFSIZE ) + 1;
	    maxkanji = MIN(len, (maxkanji - clen));
	    data = kouho;
	    for( i = ret = 0; i < maxkanji; i++ ){
		if( !(*data++) ){
		    ret++;
		}
	    }
	    yomi = kouho;
	    if( ret ) {
	       ir_debug( Dmsg(5,"候補リスト\n" ); )
		size = listsize( kouho, ret );
		yomi += size;
	    } else {
		*yomi = '\0'; yomi++;
	    }
	    /* 読みを最後につける*/
	    strcpy( (char *)yomi, (char *)workbuf );
	    size += clen;
	} else {
	    PrintMsg("[%s](%s) bunsetu move failed\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
       }
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }
    return SendTypeE2Reply(client, ret, (ret < 0)? 0: (ret + 1), kouho, size);
}

ir_resize(clientp)
ClientPtr *clientp ;
{
#define ENLARGE -1
#define SHORTEN -2
    Req5 *req = &Request.Request5 ;
    ClientPtr client = *clientp ;
    int ret, cxnum, yomilen, bunsetu ;
    int size = 0 ;
    BYTE lbuf[SENDBUFSIZE], *lbufp = lbuf;
    int stat = 0;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	bunsetu = req->number ;
	yomilen = req->datalen ;

	RkwGoTo(cxnum, bunsetu) ;
       ir_debug( Dmsg(5,"yomilen = %d\n",yomilen ); )
       ir_debug( Dmsg(5,"bunsetu = %d\n",bunsetu ); )
	switch( yomilen ) {
	    case ENLARGE :
		ret = RkwEnlarge( cxnum ) ;
		break ;
	    case SHORTEN :
		ret = RkwShorten( cxnum ) ;
		break ;
	    default :
		ret = RkeResize( cxnum, yomilen );
		break ;
	    }
	/* 最優先候補リストを取得する */
	size = getFirstKouho(cxnum, bunsetu, ret, &stat, &lbufp);
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }
    ret = SendTypeE2Reply(client, stat, (stat < 0)? 0: stat - bunsetu,
			  (char *)lbufp, size);
    if (lbufp != lbuf) free((char *)lbufp);
    return ret;
}

ir_store_yomi( clientp )
ClientPtr *clientp ;
{
    Req9 *req = &Request.Request9 ;
    ClientPtr client = *clientp ;
    int cxnum, bunsetu, len, ret ;
    int size = 0 ;
    char *data ;
    Ushort cbuf[CBUFSIZE];
    BYTE lbuf[SENDBUFSIZE], *lbufp = lbuf;
    int stat = 0;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	bunsetu = req->number ;
	len = req->datalen ;

	RkwGoTo( cxnum, bunsetu ) ;

	data = req->data ;
	data[ len + 1 ] = '\0' ;
       ir_debug( Dmsg(5,"読み = %s\n",data ); )
	ret = euc2ushort( data, len, cbuf, CBUFSIZE );
	if ((ret = RkwStoreYomi( cxnum, (Ushort *)cbuf, ret )) >= 0) {
	    size = getFirstKouho(cxnum, bunsetu, ret, &stat, &lbufp);
	} else {
	    PrintMsg("[%s](%s) RkwStoreYomi faild\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
	}
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }
    ret = SendTypeE2Reply(client, stat, (stat < 0)? 0: stat,
			  (char *)lbufp, size);
    if (lbufp != lbuf) free((char *)lbufp);
    return ret;
}

ir_query_extension( clientp )
ClientPtr *clientp ;
{
    extern char *ExtensionName[][2] ;
    Req12 *req = &Request.Request12 ;
    ClientPtr client = *clientp ;
    int i = 0 ;
    int status = -1 ;

    while( strlen( ExtensionName[ i ][ 0 ] ) ) {
	if( !strcmp( ExtensionName[ i ][ 0 ], (char *)req->data ) ) {
	    status = atoi( ExtensionName[ i ][ 1 ] ) ;
	    break ;
	}
	i++ ;
    }

    return SendTypeE1Reply(client, status);
}

static void iroha2canna();

#ifdef EXTENSION
ir_list_dictionary( clientp )
ClientPtr *clientp ;
{
    Req9 *req = &Request.Request9 ;
    ClientPtr client = *clientp ;
    char *dicnames = local_buffer ;
    char *dirname, *dirnamelong;
    int cxnum, size = 0, ret = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	size = MIN( req->number, LOCAL_BUFSIZE ) ;
	dirname = (char *)req->data;
	iroha2canna( dirname );
	dirnamelong = malloc(strlen(DDUSER) + strlen(dirname) + 2);
	if (dirnamelong) {
	  strcpy(dirnamelong, DDUSER);
	  strcat(dirnamelong, "/");
	  strcat(dirnamelong, dirname);

	  if (checkPermissionToRead(client, dirname, (char *)0) >= 0) {
	    ret = RkwListDic(cxnum, (unsigned char *)dirnamelong,
			     (unsigned char *)dicnames, size);
	  }
	  free(dirnamelong);
	}
	/* else ERROR because ret == -1 */
	if( ret < 0 ) {
	    ret = 0;
	} else {
	   ir_debug( Dmsg(5,"辞書リスト\n" ); )
	    size = listsize(dicnames, ret);
	}
    } else {
	PrintMsg( "[%s@%s](%s) Context Err\n", client->username, client->hostname, ProtoName[ req->Type - 1 ]	) ;
    }	

    return SendTypeE2Reply(client, ret, (ret < 0)? 0: ret, dicnames, size);
}


ir_create_dictionary( clientp )
ClientPtr *clientp ;
{
    Req8 *req = &Request.Request8 ;
    ClientPtr client = *clientp ;
    int cxnum, ret = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
       ir_debug( Dmsg(5,"dicname = %s\n", req->data ); )
	ret = RkwCreateDic(cxnum, (unsigned char *)req->data, req->mode);
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}


ir_remove_dictionary( clientp )
ClientPtr *clientp ;
{
    Req8 *req = &Request.Request8 ;
    ClientPtr client = *clientp ;
    int cxnum, ret = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
       ir_debug( Dmsg(5,"dicname = %s\n", req->data ); )
	ret = RkwRemoveDic(cxnum, (unsigned char *)req->data, 0);
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_rename_dictionary( clientp )
ClientPtr *clientp ;
{
    Req10 *req = &Request.Request10 ;
    ClientPtr client = *clientp ;
    int cxnum ; 
    int ret = -1 ;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
      ret = RkwRenameDic(cxnum, (unsigned char *)req->dicname,
			 (unsigned char *)req->textdicname, req->mode);
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }

    return SendTypeE1Reply(client, ret);
}

ir_get_word_text_dic( clientp )
ClientPtr *clientp ;
{
    Req10 *req = &Request.Request10 ;
    ClientPtr client = *clientp ;
    BYTE *infobuf = (BYTE *)local_buffer ;
    char *dicname, *dirname ;
    int cxnum, infosize, ret = -1, cnt = 0, size = SIZEOFLONG;
    Ushort cbuf[CBUFSIZE];

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	dirname = (req->diclen ? (char *)req->dicname : (char *)NULL);
	iroha2canna( dirname );
	infosize = MIN( req->mode, MAXDATA ) ;
	dicname = (char *)req->textdicname;
	if (dirname) {
	  dirname = (char *)malloc(strlen(DDUSER) + strlen(dirname) + 2);
	  if (dirname) {
	    strcpy((char *)dirname, DDUSER);
	    strcat((char *)dirname, "/");
	    strcat((char *)dirname, (char *)req->dicname);

	    if (checkPermissionToRead(client, dirname, dicname) >= 0) {
	      ret = RkwGetWordTextDic(cxnum, (unsigned char *)dirname,
				      (unsigned char *)dicname,
				      (Ushort *)cbuf, CBIGBUFSIZE);
	    }
	    free(dirname);
	  }
	}
	else {
	  ret = RkwGetWordTextDic(cxnum, (unsigned char *)dirname,
				  (unsigned char *)dicname,
				  (Ushort *)cbuf, CBIGBUFSIZE);
	}
	if (ret > 0) {
	  ret = ushort2euc( cbuf, ret, (char *)infobuf, infosize );
	}
	if( ret > 0 ) {
	    cnt = ret + 1 ;
	    size = cnt + SIZEOFLONG;
	}
    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]   ) ;
    }
    return SendTypeE4Reply(client, ret, cnt, infobuf, size);
}
#endif /* EXTENSION */

static int
storeStat(client, ret, src, dest)
ClientPtr client;
int ret;
BYTE *src, *dest;
{
    if( ret >= 0 ) {
	RkStat *stat = (RkStat *)src;
	BYTE *bufp = dest;

	LTOL4(stat->bunnum, dest);	/* bunsetsu bangou */
	dest += SIZEOFLONG;
	LTOL4(stat->candnum, dest);	/* kouho bangou */
	dest += SIZEOFLONG;
	LTOL4(stat->maxcand, dest);	/* sou kouho suu */
	dest += SIZEOFLONG;
	LTOL4(stat->diccand, dest);	/* jisho ni aru kouho suu */
	dest += SIZEOFLONG;
	LTOL4(stat->ylen, dest);	/* yomigana no nagasa (in byte) */ 
	dest += SIZEOFLONG;
	LTOL4(stat->klen, dest);	/* kanji no nagasa (in byte) */
	dest += SIZEOFLONG;
	LTOL4(stat->tlen, dest);	/* tango no kosuu */

	if( !client->version_lo ) {	      /* client version が ０ のは */
	    register int *p = (int *)bufp;
	    register int tmp1 = p[ 2 ];
	    register int tmp2 = p[ 3 ] ;
	    int i ;

	    for( i = 2; i < 5; i++ )
		p[ i ] = p[ i + 2 ] ;
	    p[ 5 ] = tmp1 ;
	    p[ 6 ] = tmp2 ;
	}
    }
    return ret;
}

ir_get_stat( clientp )
ClientPtr *clientp ;
{
    Req5 *req = &Request.Request5 ;
    ClientPtr client = *clientp ;
    int cxnum, kouho, bunsetu, ret = -1;
    int size = 0 ;
    RkStat stat ;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {
	bunsetu = req->number ;
	kouho = req->datalen ;

	RkwGoTo( cxnum, bunsetu ) ;
	RkwXfer( cxnum, kouho ) ;

	ret = RkeGetStat( cxnum, &stat );
	size = SIZEOFLONG * 7;

    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }
    return SendTypeE3Reply(client, ret, storeStat, (BYTE *)&stat, size);
}

static int
storeLex(client, tangosu, src, dest)
ClientPtr client; /* ARGSUSED */
int tangosu;
BYTE *src, *dest;
{
    if( tangosu >= 0 ) {
	register int i;
	RkLex *lex = (RkLex *)src;

	for (i = 0; i < tangosu; i++, lex++) {
	    LTOL4(lex->ylen, dest);	/* yomigana no nagasa (in byte) */ 
	    dest += SIZEOFLONG;
	    LTOL4(lex->klen, dest);	/* kanji no nagasa (in byte) */
	    dest += SIZEOFLONG;
	    LTOL4(lex->rownum, dest);	/* row number */
	    dest += SIZEOFLONG;
	    LTOL4(lex->colnum, dest);	/* column number */
	    dest += SIZEOFLONG;
	    LTOL4(lex->dicnum, dest);	/* dic number */
	    dest += SIZEOFLONG;
	}
    }
    return tangosu;
}

ir_get_lex( clientp )
ClientPtr *clientp ;
{
    Req11 *req = &Request.Request11 ;
    ClientPtr client = *clientp ;
    RkLex *lex = (RkLex *)local_buffer ;
    int     cxnum ;
    int     size = 0 ;
    int tangosu = -1;

    cxnum = req->context;
    if (chk_cxt(client, cxnum)) {

	RkwGoTo( cxnum, req->number ) ;
	RkwXfer( cxnum, req->kouho ) ;

	tangosu = RkeGetLex( cxnum, lex, MIN( req->max, LOCAL_BUFSIZE/sizeof( RkLex ) )	); 
	size = tangosu * SIZEOFLONG * 5;

    } else {
	PrintMsg( "[%s](%s) Context Err\n", client->username, ProtoName[ req->Type - 1 ]  ) ;
    }

    return SendTypeE3Reply(client, tangosu, storeLex, (BYTE *)lex, size);
}

extern void ClientStat();

#ifdef DEBUG
void
DispDebug( client )
ClientPtr client ;
{
    char    return_date[DATE_LENGH] ;
    long    wtime = client->used_time ;
    char    buf[10] ;

    (void)ClientStat( client, GETDATE, 0, return_date ) ;
    Dmsg(5,"ユーザ名         :%s\n", client->username ) ;
    Dmsg(5,"コネクトした時間 :%s\n", return_date ) ;
    Dmsg(5,"ホスト名         :%s\n", client->hostname ) ;
    sprintf( buf,"%02d:%02d:%02d", wtime/3600, (wtime%3600)/60, (wtime%3600)%60 ) ;
    Dmsg(5,"ユーザ消費時間   :%s\n\n", buf ) ;
}	
#endif

static int
SetServerVersion( buf )
char *buf ;
{
    int SendSize = strlen( SERVER_VERSION ) + 1 ;

    LTOL4( SendSize, buf ) ; buf += SIZEOFLONG ;
    /* サーバのバージョンをセットする */
    bcopy( SERVER_VERSION, buf, SendSize ) ;
    return( SendSize + SIZEOFLONG ) ;
}

ir_server_stat2( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    char *sendp = Acknowledge.SendAckBuffer ;
    char *savep ;
    register ClientPtr	    who ;
    ClientPtr *OutPut;
    int     RequestCount[ MAXREQUESTNO ] ;
    int     i, j, count, len, retval, max_cx, n;

    OutPut = (ClientPtr *)malloc(connow_socks * sizeof(ClientPtr));

    /* プロトコルバージョンセット */
    sendp += SetServerVersion( sendp ) ;

    /* 現在時刻セット */
    LTOL4( time( (long *)0 ), sendp ) ; sendp += SIZEOFLONG ;

    /* プロトコル数セット */
    LTOL4( REALREQUEST, sendp ) ; sendp += SIZEOFLONG ;

    /* プロトコル名リスト作成 */
    savep = sendp ; sendp += SIZEOFLONG ;
    for( i = 1; i < MAXREQUESTNO; i++ ) {
	RequestCount[ i - 1 ] = htonl( TotalRequestTypeCount[ i ] ) ;
	strcpy( (char *)sendp, ProtoName[ i - 1 ] ) ;
	sendp += (strlen( ProtoName[ i - 1 ] ) + 1 ) ;
    }

    /* プロトコル名リスト長セット */
    LTOL4( sendp - ( savep + SIZEOFLONG ), savep ) ;

    /* プロトコル使用頻度セット */
    bcopy( RequestCount, sendp, REALREQUEST * SIZEOFLONG ) ;
    sendp += ( REALREQUEST * SIZEOFLONG ) ;

    /* 接続しているクライアント数セット */
    if (OutPut) {
      count = ConnectClientCount(client, OutPut, connow_socks) ;
    }
    else {
      count = 0;
    }
    LTOL4( count, sendp ) ; sendp += SIZEOFLONG ;

    /* コンテクスト数をセット */
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
    max_cx += (max_cx % SIZEOFSHORT); /* ????? */

    LTOL4(max_cx, sendp ) ; sendp += SIZEOFLONG ;

    if( SendTypeE6Reply(client, sendp - Acknowledge.SendAckBuffer) < 0 ) {
      retval = -1;
      goto stat2done;
    }

    /* 各クライアント情報をセット */
    for( i = 0; i < count; i ++ ) {
	savep = sendp = Acknowledge.SendAckBuffer ;
	who = OutPut[ i ] ;
	sendp += SIZEOFLONG ;

	LTOL4( who->id, sendp ) ; sendp += SIZEOFLONG ;
	LTOL4( who->usr_no, sendp ) ; sendp += SIZEOFLONG ;
	LTOL4( who->used_time, sendp ) ; sendp += SIZEOFLONG ;
	LTOL4( who->idle_date, sendp ) ; sendp += SIZEOFLONG ;
	LTOL4( who->connect_date, sendp ) ; sendp += SIZEOFLONG ;

	/* プロトコル使用頻度情報セット */
	for( j = 1; j < MAXREQUESTNO; j++ ) {
	    LTOL4( who->pcount[ j ], sendp ) ; sendp += SIZEOFLONG ;
	}

	/* ユーザ名セット */
	if (!who->username) {
	    len = 1;
	    LTOL4( len, sendp ) ; sendp += SIZEOFLONG ;
	    bzero( sendp, len ) ; sendp += len ;
	} else {
	    len = strlen( who->username ) + 1 ;
	    LTOL4( len, sendp ) ; sendp += SIZEOFLONG ;
	    bcopy( who->username, sendp, len ) ; sendp += len ;
	};
	
	/* ホスト名セット */
	if (!who->hostname) {
	    len = 1 ;
	    LTOL4( len, sendp ) ; sendp += SIZEOFLONG ;
	    bzero( sendp, len ) ; sendp += len ;
	} else {
	    len = strlen( who->hostname ) + 1 ;
	    LTOL4( len, sendp ) ; sendp += SIZEOFLONG ;
	    bcopy( who->hostname, sendp, len ) ; sendp += len ;
	};

	/* コンテクスト管理フラグセット */
	bzero(sendp, max_cx);
	for (j = 0 ; j < who->ncon ; j++) {
	  sendp[who->context_flag[j]] = 1;
	}
	sendp += max_cx;

	/* クライアント管理情報リスト長セット */
	LTOL4( sendp - (savep + SIZEOFLONG), savep ) ;

	if( SendTypeE6Reply(client, sendp - savep) < 0 ) {
	  retval = -1;
	  goto stat2done;
	}

	who ++ ;
    }

    CloseDownClient( client ) ;
    *clientp = (ClientPtr)0;

    retval = 1;
  stat2done:
    if (OutPut) {
      free((char *)OutPut);
    }
    return retval;
}

ir_server_stat( clientp )
ClientPtr *clientp ;
{
    char *sendp = Acknowledge.SendAckBuffer ;
    ClientPtr client = *clientp ;
    register ClientPtr	    who ;
    register ClientStatPtr  Wp ;
    ClientPtr		    *OutPut;
    ClientStatRec	    *Sstat;
    int 		    i, j, count, InfoSize, SendSize, retval;
    int 		    RequestCount[ OLD_MAXREQUESTNO ] ;

    OutPut = (ClientPtr *)malloc(connow_socks * sizeof(ClientPtr));
    Sstat = (ClientStatRec *)malloc(connow_socks * sizeof(ClientStatRec));

    if (OutPut && Sstat) {
      count = ConnectClientCount(client, OutPut, connow_socks) ;
    }
    else {
      count = 0;
    }

    InfoSize = sizeof( ClientStatRec )*count ;

    Wp = Sstat ;
    for( i = 0 ; i < count; i++ ) {
	who = OutPut[ i ] ;
       ir_debug( DispDebug( who ); )
	Wp->id = htonl( who->id ) ;		
	Wp->usr_no = htonl( who->usr_no ) ;	
	Wp->used_time = htonl( who->used_time ) ;	
	Wp->idle_date = htonl( who->idle_date ) ;
	Wp->connect_date = htonl( who->connect_date ) ; 
	for( j = 0; j < OLD_MAXREQUESTNO; j++ )
	    Wp->pcount[ j ] = htonl( who->pcount[ j ] ) ;
	strncpy( Wp->username, who->username, 10 ) ;
	strncpy( Wp->hostname, who->hostname, 15 ) ;
	bzero(Wp->context_flag, OLD_MAX_CX);
	for (j = 0 ; j < who->ncon ; j++) {
	  int inde = who->context_flag[j];
	  if (inde < OLD_MAX_CX) {
	    Wp->context_flag[inde] = 1;
	  }
	}
	Wp ++ ;
    }	

    /* サーバのバージョンを通知する */
    sendp += SetServerVersion( sendp ) ;

    /* サーバの立ち上がってからの総プロトコル */
    for( i = 0; i < OLD_MAXREQUESTNO; i++ )
	RequestCount[ i ] = htonl( TotalRequestTypeCount[ i ] ) ;

    SendSize = SIZEOFLONG * OLD_MAXREQUESTNO ;
    LTOL4( SendSize, sendp ) ; sendp += SIZEOFLONG ;
    bcopy( RequestCount, sendp, SendSize ) ; sendp += SendSize ;

    /* 情報を送るクライアント数を通知する */
    LTOL4( count, sendp ) ; sendp += SIZEOFLONG ;

    /* サーバの現在の時刻を通知する */
    LTOL4( time( (long *)0 ), sendp ) ; sendp += SIZEOFLONG ;

    /* 実際にクライアント情報を通知する */
    if (Sstat) { /* 疑問 */
      bcopy( Sstat, sendp, InfoSize ) ; sendp += InfoSize ;
    }

    if( SendTypeE5Reply(client, sendp - Acknowledge.SendAckBuffer) < 0 ) {
      retval = -1;
      goto statdone;
    }

    CloseDownClient( client ) ;
    *clientp = (ClientPtr)0;
    retval = 1;
  statdone:
    if (OutPut) {
      free((char *)OutPut);
    }
    if (Sstat) {
      free((char *)Sstat);
    }
    return retval;
}

ir_host_ctl( clientp )
ClientPtr *clientp ;
{
    ClientPtr client = *clientp ;
    extern NumberAccessControlList() ;
    extern ACLPtr ACLHead ;
    char *sendp = Acknowledge.SendAckBuffer ;
    char *savep = Acknowledge.SendAckBuffer + SIZEOFLONG ;
    char *namep ;
    ACLPtr wp ;
    int cnt, i ;

    LTOL4( NumberAccessControlList(), sendp ) ; sendp += (SIZEOFLONG * 2) ;

    for( wp = ACLHead; wp != (ACLPtr)NULL; wp = wp->next ) {
	cnt = strlen( (char *)wp->hostname ) + 1 ;
	LTOL4( cnt, sendp ) ; sendp += SIZEOFLONG ;
	strcpy( (char *)sendp, (char *)wp->hostname ) ;
	sendp += cnt ;
	LTOL4( wp->usercnt, sendp ) ; sendp += SIZEOFLONG ;
	for( i = 0, namep = wp->usernames; i < wp->usercnt; i++ ) {
	    cnt = strlen( (char *)namep ) + 1 ;
	    LTOL4( cnt, sendp ) ; sendp += SIZEOFLONG ;
	    strcpy( (char *)sendp, (char *)namep ) ;
	    sendp += cnt ;
	    namep += cnt ;
	}
    }
    LTOL4( sendp - (savep + SIZEOFLONG), savep ) ;
    if( SendTypeE7Reply(client, sendp - Acknowledge.SendAckBuffer) < 0 )
	return( -1 ) ;

    CloseDownClient( client ) ;
    *clientp = (ClientPtr)0;
    return( 1 ) ;
}

#endif /* USE_EUC_PROTOCOL */

static
WriteClient( ClientFD, buf, size )
int ClientFD ;
char *buf ;
int size ;
{
    register int write_stat ;
    register char *bufindex = buf ;
    register int todo = size ;

   ir_debug( Dmsg( 10, "WriteClient:" ) ; )
   ir_debug( DebugDump( 10, buf, size ); )

    while ( size > 0 ) {
	errno = 0;
	write_stat = write( ClientFD, (char *)bufindex, todo ) ;
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

#define READ_HEADER_SIZE    SIZEOFLONG
#define READ_SIZE	    2048
#define SIZE4	4   /* sizeof( int ) */
#define SIZE8	8
#define SIZE12	12
#define SIZE16	16
#define SIZE20	20
#define TRY_COUNT   10

static char
ReadRequestBuffer[ READ_SIZE ] ;	/* デフォルトバッファ */

static int  ReadSize = READ_SIZE ;     /* バッファサイズ */

static char
*readbufptr = ReadRequestBuffer ;	/* デフォルトバッファ */

int (* CallFunc)() ;

ReadRequestFromClient(who, status )
ClientPtr who;
int *status;	      /* read at least n from client */
{
    int (* ReqCallFunc)() ;
#ifdef DEBUG
    extern char *DebugProc[][2] ;
#endif
    char *bufptr = ReadRequestBuffer ;
    register Req0 *req0 = &Request.Request0 ;
    int client = who->id, n = 0  ;
    int     readsize ;		    /* 一回に読み込んだサイズ */
    int     bufsize = ReadSize ;   /* バッファサイズ */
    int     bufcnt = 0 ;	    /* 読み込んだ累積サイズ */
    int     empty_count = 0 ;

    if( readbufptr != ReadRequestBuffer ) {	   /* 前回バッファを大きく */
       ir_debug( Dmsg( 8,"free readbufptr.\n" ); ) /* した場合は，デフォルト*/
	free( (char *)readbufptr ) ;			   /* に戻す */
	readbufptr = ReadRequestBuffer ;
	ReadSize = READ_SIZE ;
    }

    while( empty_count < TRY_COUNT ) {
	if ( (readsize = read( client, (char *)bufptr, bufsize )) < 0) {

	    if (who->username) {
		PrintMsg( "[%s] ", who->username  ) ;
	    }
	    PrintMsg( "Read request failed\n" ) ;
	    ir_debug( Dmsg( 5, "ReadRequestFromClient: Read request failed\n"); )
	    *status = -1 ;
	    return( 0 ) ;
	} else if ( readsize == 0 ) {	
	    empty_count ++ ;
	    continue ;
	}
       ir_debug( Dmsg( 10, "ReadRequest:" ) ; )
       ir_debug( DebugDump( 10, bufptr, readsize ); )
	empty_count = 0;
	bufcnt += readsize ;
	if( (n == 0) && (bufcnt >= 4) ) 
	    req0->Type = (int)L4TOL(bufptr);
	
	if (!(who->username)) {
	    ir_debug( Dmsg(5,"Client: <@%s> [%d]\n",
			   who->hostname, req0->Type ) ; );
	} else {
	    ir_debug( Dmsg(5,"Client: <%s@%s> [%d]\n",
			   who->username, who->hostname, req0->Type ) ; );
	}
	
	if( (0 > req0->Type) || 
	   ( (req0->Type > REALREQUEST) && (req0->Type < EXTBASEPROTONO) ) ||
		(req0->Type > (MAXEXTREQUESTNO+EXTBASEPROTONO)) ) {
	    if (who->username) {
		PrintMsg( "[%s] ", who->username  ) ;
	    }
	    PrintMsg( "Request error[ %d ]\n", req0->Type ) ;
	    *status = -1 ;
	    return( 0 ) ;
	}
	
	/* プロトコルのタイプ毎にデータを呼んでくる関数を呼ぶ */
	if( req0->Type >= EXTBASEPROTONO ) {
#ifdef EXTENSION
	    int request = req0->Type - EXTBASEPROTONO ;
	    ReqCallFunc = ExtensionVector[ request ].extdat;
	    CallFunc = ExtensionVector[ request ].func;
#else /* !EXTENSION */
	    extern int ProcReq0();

	    CallFunc = ir_error;
	    ReqCallFunc = ProcReq0; /* これではいけないような気がする */
#endif /* !EXTENSION */
	}
	else {
	   ir_debug( Dmsg( 8,"Now Call %s\n", DebugProc[ req0->Type ][ 1 ] ); )
	    ReqCallFunc = Vector[ req0->Type ].extdat;
	    CallFunc = Vector[ req0->Type ].func;
	}
	if( (n = (* ReqCallFunc)( readbufptr, bufcnt ))  < 0 ) {
	    if (who->username) {
		PrintMsg( "[%s] ", who->username  ) ;
	    }
	    PrintMsg( "Read Data failed\n") ;
	    *status = -1 ;
	    return( 0 ) ;	
	} else if( n == 0 ) {
	    break ;
	} else {
	    bufsize -= readsize ;
	    /* 読み込むべきデータがバッファサイズより */
	    /* 多い場合には，アロケートし直す */	
	    if( n > bufsize ) { 
		char *local_bufptr ;

		local_bufptr = (char *)malloc(bufcnt + n);
		if (local_bufptr) {
		   ir_debug( Dmsg( 8,"malloc size is %d.\n", bufcnt + n ); )
		    bcopy( readbufptr, local_bufptr, bufcnt ) ;
		    if( readbufptr != ReadRequestBuffer )
			free( (char *)readbufptr ) ;
		    readbufptr = local_bufptr ;
		    bufptr = local_bufptr + bufcnt ;
		    ReadSize = bufcnt + n ;
		    bufsize = n ;
		} else {
		    *status = -1 ;
		    if (who->username) {
			PrintMsg( "[%s] ", who->username  ) ;
		    }
		    PrintMsg( "Read Buffer allocate failed\n") ;
		    return( 0 ) ;	
		}
	    } else {
		bufptr += readsize ;
	    }
	    continue ;
	}
    }

    if( empty_count >= TRY_COUNT ) {
	*status = -1 ;
	if (who->username) {
	    PrintMsg( "[%s] ", who->username ) ;
	}
	PrintMsg( "Read Over Time!!\n") ;
	return( 0 ) ;	
    }

    /* プロトコルの種類毎に統計を取る */
    if( req0->Type < MAXREQUESTNO ) {
	who->pcount[ req0->Type ] ++ ;
	TotalRequestTypeCount[ req0->Type ] ++ ;
    }

    (void)ClientStat(who, SETTIME, req0->Type , 0);

    *status = 1 ;
    return( req0->Type ) ;
}

ProcReq0( buf, size )
char *buf ;
int size ;
/* ARGSUSED */
{
    return( 0 ) ;
}

#ifdef USE_EUC_PROTOCOL

ProcReq1( buf, size )
char *buf ;
int size ;
{
    register Req1 *req = &Request.Request1  ;

    if( size < SIZE8 )
	return( SIZE8 - size ) ;

    req->context = (int)L4TOL(buf + SIZE4);
   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
    return( 0 ) ;
}

#endif /* USE_EUC_PROTOCOL */

ProcReq2( buf, size )
char *buf ;
int size ;
{
    register Req2 *req = &Request.Request2 ;
    int needsize ;

   ir_debug( Dmsg(10,"ProcReq start!!\n" ); )
    if( (needsize = SIZE8 - size) > 0 )
	return( needsize ) ;

    req->namelen = (int)L4TOL(buf + SIZE4);
   ir_debug( Dmsg(10,"req->namelen =%d\n", req->namelen ); )

    if( (needsize = SIZE8 + req->namelen - size) > 0 )
	return( needsize ) ;

    if( req->namelen > 0 ){
	req->name = buf + SIZE8 ;
    }
   ir_debug( Dmsg(10,"req->namelen =%d\n", req->namelen ); )
   ir_debug( Dmsg(10,"req->name =%s\n", req->name ); )
    return( 0 ) ;
}

#ifdef USE_EUC_PROTOCOL

ProcReq3( buf, size )
char *buf ;
int size ;
{
    register Req3 *req = &Request.Request3 ;
    int needsize ;

    if( (needsize = SIZE12 - size ) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->number = (int)L4TOL(buf + SIZE8);
   ir_debug( Dmsg(10,"req->contest =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->number =%d\n", req->number ); )
    return( 0 ) ;
}

ProcReq4( buf, size )
char *buf ;
int size ;
{
    register Req4 *req = &Request.Request4 ;
    register int i ;
    int needsize ;

    if( (needsize = SIZE12 - size ) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->number = (int)L4TOL(buf + SIZE8);

   ir_debug( Dmsg(5,"req->number=%d\n", req->number ); )

    if( (needsize = SIZE12 + (req->number)*SIZE4 - size ) > 0 )
	return( needsize ) ;

    buf += SIZE12 ;
    req->kouho = (int *)buf ;
    for( i = 0; i < req->number; i++, buf+= SIZE4 )
	req->kouho[ i ] = (int)L4TOL(buf);

   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->number =%d\n", req->number ); )
#ifdef DEBUG
    for( i = 0; i < req->number; i++ )
       Dmsg(10,"req->kouho =%d\n", req->kouho[ i ] ) ;
#endif
    return( 0 ) ;
}

ProcReq5( buf, size )
char *buf ;
int size ;
{
    register Req5 *req = &Request.Request5 ;
    int needsize ;

    if( (needsize = SIZE16 - size ) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->number = (int)L4TOL(buf + SIZE8);
    req->datalen = (int)L4TOL(buf + SIZE12);
   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->number =%d\n", req->number ); )
   ir_debug( Dmsg(10,"req->datalen =%d\n", req->datalen ); )
    return( 0 ) ;
}

ProcReq6( buf, size )
char *buf ;
int size ;
{
    register Req6 *req = &Request.Request6 ;
    int needsize ;

    if( (needsize = SIZE16 - size ) > 0 )
	return( needsize ) ;

    req->context =(int)L4TOL(buf + SIZE4);
    req->mode = (int)L4TOL(buf + SIZE8);
    req->datalen = (int)L4TOL(buf + SIZE12);

    if( (needsize = SIZE16 + req->datalen - size ) > 0 )
	return( needsize ) ;

    req->data = buf + SIZE16 ;
   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->mode =%d\n", req->mode ); )
   ir_debug( Dmsg(10,"req->datalen =%d\n", req->datalen ); )
   ir_debug( Dmsg(10,"req->data =%s\n", req->data ); )

    return( 0 ) ;
}

ProcReq7( buf, size )
char *buf ;
int size ;
{
    register Req7 *req = &Request.Request7 ;
    int needsize ;

    if( (needsize = SIZE12 - size ) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->diclen = (int)L4TOL(buf + SIZE8);

    if( (needsize = SIZE12 + req->diclen - size ) > 0 )
	return( needsize ) ;

    req->dicname = buf + SIZE12 ;

    if( (needsize = SIZE12 + req->diclen - size ) > 0 )
	return( needsize ) ;

    req->datalen = (int)L4TOL(buf + SIZE12 + req->diclen);

    if( (needsize = SIZE16 + req->diclen + req->datalen - size ) > 0 )
	return( needsize ) ;

    req->datainfo = buf + SIZE16 + req->diclen ;
   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->diclen =%d\n", req->diclen ); )
   ir_debug( Dmsg(10,"req->dicname =%s\n", req->dicname ); )
   ir_debug( Dmsg(10,"req->datalen =%d\n", req->datalen ); )
   ir_debug( Dmsg(10,"req->datainfo =%s\n", req->datainfo ); )
    return( 0 ) ;
}

ProcReq8( buf, size )
char *buf ;
int size ;
{
    register Req8 *req = &Request.Request8 ;
    int needsize ;

    if( (needsize = SIZE12 - size ) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->datalen = (int)L4TOL(buf + SIZE8);

    if( (needsize = SIZE12 + req->datalen - size ) > 0 )
	return( needsize ) ;

    req->data = buf + SIZE12 ;

    if( (needsize = SIZE16 + req->datalen - size ) > 0 )
	return( needsize ) ;

    req->mode = (int)L4TOL(buf + SIZE12 + req->datalen);

   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->datalen =%d\n", req->datalen ); )
   ir_debug( Dmsg(10,"req->data =%s\n", req->data ); )
   ir_debug( Dmsg(10,"req->mode =%d\n", req->mode ); )
    return( 0 ) ;
}

ProcReq9( buf, size )
char *buf ;
int size ;
{
    register Req9 *req = &Request.Request9 ;
    int needsize ;

    if( (needsize = SIZE16 - size) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->number = (int)L4TOL(buf + SIZE8);
    req->datalen = (int)L4TOL(buf + SIZE12);

    if( (needsize = SIZE16 + req->datalen - size) > 0 )
	return( needsize ) ;

    req->data = buf + SIZE16 ;

   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->number =%d\n", req->number ); )
   ir_debug( Dmsg(10,"req->datalen =%d\n", req->datalen ); )
   ir_debug( Dmsg(10,"req->data =%s\n", req->data ); )
    return( 0 ) ;
}

ProcReq10( buf, size )
char *buf ;
int size ;
{
    register Req10 *req = &Request.Request10 ;
    int needsize ;

    if( (needsize = SIZE12 - size) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->diclen = (int)L4TOL(buf + SIZE8);

    if( (needsize = SIZE12 + req->diclen - size) > 0 )
	return( needsize ) ;

    req->dicname = buf + SIZE12 ;

    if( (needsize = SIZE16 + req->diclen - size) > 0 )
	return( needsize ) ;

    req->textdiclen = (int)L4TOL(buf + SIZE12 + req->diclen);

    if( (needsize = SIZE16 + req->diclen + req->textdiclen - size) > 0 )
	return( needsize ) ;

    if( req->textdiclen )
	req->textdicname = buf + SIZE16 + req->diclen ;
    else
	req->textdicname = (char *)NULL ;

    if( (needsize = SIZE20 + req->diclen + req->textdiclen - size) > 0 )
	return( needsize ) ;

    req->mode = (int)L4TOL(buf + SIZE16 + req->diclen + req->textdiclen);

   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->diclen =%d\n", req->diclen ); )
   ir_debug( Dmsg(10,"req->dicname =%s\n", req->dicname ); )
   ir_debug( Dmsg(10,"req->textdiclen =%d\n", req->textdiclen ); )
   ir_debug( Dmsg(10,"req->textdicname =%s\n", req->textdicname ); )
   ir_debug( Dmsg(10,"req->mode =%d\n", req->mode ); )

    return( 0 ) ;
}

ProcReq11( buf, size )
char *buf ;
int size ;
{
    register Req11 *req = &Request.Request11 ;
    int needsize ;

    if( (needsize = SIZE20 - size) > 0 )
	return( needsize ) ;

    req->context = (int)L4TOL(buf + SIZE4);
    req->number = (int)L4TOL(buf + SIZE8);
    req->kouho = (int)L4TOL(buf + SIZE12);
    req->max = (int)L4TOL(buf + SIZE16);
   ir_debug( Dmsg(10,"req->context =%d\n", req->context ); )
   ir_debug( Dmsg(10,"req->number =%d\n", req->number ); )
   ir_debug( Dmsg(10,"req->kouho =%d\n", req->kouho ); )
   ir_debug( Dmsg(10,"req->max =%d\n", req->max ); )
    return( 0 ) ;
}

ProcReq12( buf, size )
char *buf ;
int size ;
{
    register Req12 *req = &Request.Request12 ;
    int needsize ;

    if( (needsize = SIZE8 - size) > 0 )
	return( needsize ) ;

    req->datalen = (int)L4TOL(buf + SIZE4);

    if( (needsize = SIZE8 + req->datalen - size) > 0 )
	return( needsize ) ;

    if( req->datalen > 0 ){
	req->data = buf + SIZE8 ;
    }

    if( (needsize = SIZE8 + req->datalen - size) > 0 )
	return( needsize ) ;

    req->number = (int)L4TOL(buf + SIZE8 + req->datalen);

   ir_debug( Dmsg(10,"req->datalen =%d\n", req->datalen ); )
   ir_debug( Dmsg(10,"req->data =%s\n", req->data ); )
   ir_debug( Dmsg(10,"req->number =%d\n", req->number ); )
    return( 0 ) ;
}

static void
iroha2canna( dirnames )
char *dirnames;
{
  if (dirnames) {
    char *buf, *wp;
  
    buf = dirnames;
    
    while( *buf ){
      wp = buf + 5;
      if( !strncmp( (char *)buf, "iroha", 5 ) && (( *wp=='\0' ) || ( *wp==':' )) ){
	bcopy( "canna", buf, 5 );
	buf += 5;
      }
      while( (*buf != ':') && (*buf != '\0') )
	buf++;
      buf++;
    }
  }
}

#endif /* USE_EUC_PROTOCOL */

#ifdef DEBUG
DebugDump( level, buf, size )
int level, size ;
char *buf ;
{
    char buf1[80] ;
    char buf2[17] ;
    char c ;
    int     i, j;
    int     count = 0 ;

    Dmsg( level, " SIZE = %d\n", size ) ;
    Dmsg( level, " COUNT  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f      0123456789abcdef\n" ) ;
    for (i = 0; i < size; i += 16) {
	    bzero( buf1, 50 ) ;
	    bzero( buf2, 17 ) ;
	    for (j = 0; j < 16; j++) {
		    if( i+j >= size ) {
			strcat( (char *)buf1, "   " ) ;
			strcat( (char *)buf2, " " ) ;
		    } else {
			sprintf( (char *)buf1,
				"%s%02x ", buf1, (c = buf[i + j]) & 0xFF);
			if((unsigned)(c & 0xff) >= (unsigned)' ' &&
			   (unsigned)(c & 0xff) < (unsigned)0x7f )
			    sprintf( (char *)buf2,"%s%c", buf2, c ) ;
			else
			    sprintf( (char *)buf2,"%s.", buf2 ) ;
		    }
	    }
	    Dmsg( level," %05x %s     %s\n", count++,  buf1, buf2 );
    }

}

DebugDispKanji( cxnum, num )
int cxnum, num ;
{
    char buf[1024] ;
    Ushort cbuf[1024];
    int len;

    len = RkwGetKanji( cxnum, (Ushort *)cbuf, 1024 );
    ushort2euc( cbuf, len, buf, 1024 );
    Dmsg( 5,"%d:[%s] ", num, buf ) ;
}
#endif /* DEBUG */

#ifdef PROTO
RkwListDic( cxnum, dirname, mbuf, size )
int cxnum, size ;
char *dirname, *mbuf ;
{
    if( RkwSetDicPath( cxnum, dirname ) < 0 )
	return( -1 ) ;

    return( RkwGetDicList( cxnum, mbuf, size ) ) ;
}

RkwCreateDic( cxnum, dicname, mode )
int cxnum, mode ;
char *dicname ;
{
    return( 0 ) ;
}

RkwRemoveDic( cxnum, dicname )
int cxnum ;
char *dicname ;
{
    return( 0 ) ;
}

RkwRenameDic( cxnum, dicname, newdicname, mode )
int cxnum, mode ;
char *dicname, *newdicname ;
{
    return( 0 ) ;
}

/* CopyDic の定義  */

RkwCopyDic(cxnum, dirname, dicname, newdicname, mode)
int cxnum, mode ;
char *dirname, *dicname, *newdicname ;
{
    return( 0 ) ;
}
/* ここまで */
RkwGetWordTextDic( cxnum, dirname, dicname, info, infolen )
int cxnum, infolen ;
char *dirname, *dicname, *info ;
{
   ir_debug( Dmsg( 5,"RkwGetWordTextDic( %d, %s, %s, info, infolen )\n", cxnum, dirname, dicname ) );
    strncpy( info, "てすと #T35 テスト", infolen ) ;

    return( strlen( info ) ) ;
}

#endif /* PROTO */

oreqproc Vector[] =
{
#ifdef USE_EUC_PROTOCOL
/* 0x00 */	{ ir_error,		   ProcReq0 },
/* 0x01 */	{ ir_initialize,	   ProcReq2 },
/* 0x02 */	{ ir_finalize,		   ProcReq0 },
/* 0x03 */	{ ir_create_context,	   ProcReq0 },
/* 0x04 */	{ ir_duplicate_context,    ProcReq1 },	
/* 0x05 */	{ ir_close_context,	   ProcReq1 },	
/* 0x06 */	{ ir_dictionary_list,	   ProcReq3 },	
/* 0x07 */	{ ir_get_yomi,		   ProcReq5 },	
/* 0x08 */	{ ir_define_dic,	   ProcReq7 },	
/* 0x09 */	{ ir_delete_dic,	   ProcReq7 },	
/* 0x0a */	{ ir_set_dic_path,	   ProcReq8 },	
/* 0x0b */	{ ir_get_dir_list,	   ProcReq3 },	
/* 0x0c */	{ ir_mount_dictionary,	   ProcReq8 },	
/* 0x0d */	{ ir_umount_dictionary,    ProcReq8 },	
/* 0x0e */	{ ir_rmount_dictionary,    ProcReq9 },	
/* 0x0f */	{ ir_mount_list,	   ProcReq3 },	
/* 0x10 */	{ ir_convert,		   ProcReq8 },	
/* 0x11 */	{ ir_convert_end,	   ProcReq4 },	
/* 0x12 */	{ ir_get_kanjilist,	   ProcReq5 },	
/* 0x13 */	{ ir_resize,		   ProcReq5 },	
/* 0x14 */	{ ir_store_yomi,	   ProcReq9 },	
/* 0x15 */	{ ir_get_lex,		   ProcReq11 }, 
/* 0x16 */	{ ir_get_stat,		   ProcReq5 },	
/* 0x17 */	{ ir_server_stat,	   ProcReq0 },		
/* 0x18 */	{ ir_server_stat2,	   ProcReq0 },		
/* 0x19 */	{ ir_host_ctl,		   ProcReq0 },
/* 0x1a */	{ ir_query_extension,	   ProcReq12 }
#else /* !USE_EUC_PROTOCOL */
/* 0x00 */	{ ir_error,		   ProcReq0 },
/* 0x01 */	{ ir_initialize,	   ProcReq2 },
#if 0
/* 0x02 */	{ ir_error,		   ProcReq0 },
/* 0x03 */	{ ir_error,		   ProcReq0 },
/* 0x04 */	{ ir_error,		   ProcReq0 },	
/* 0x05 */	{ ir_error,		   ProcReq0 },	
/* 0x06 */	{ ir_error,		   ProcReq0 },	
/* 0x07 */	{ ir_error,		   ProcReq0 },	
/* 0x08 */	{ ir_error,		   ProcReq0 },	
/* 0x09 */	{ ir_error,		   ProcReq0 },	
/* 0x0a */	{ ir_error,		   ProcReq0 },	
/* 0x0b */	{ ir_error,		   ProcReq0 },	
/* 0x0c */	{ ir_error,		   ProcReq0 },	
/* 0x0d */	{ ir_error,		   ProcReq0 },	
/* 0x0e */	{ ir_error,		   ProcReq0 },	
/* 0x0f */	{ ir_error,		   ProcReq0 },	
/* 0x10 */	{ ir_error,		   ProcReq0 },	
/* 0x11 */	{ ir_error,		   ProcReq0 },	
/* 0x12 */	{ ir_error,		   ProcReq0 },	
/* 0x13 */	{ ir_error,		   ProcReq0 },	
/* 0x14 */	{ ir_error,		   ProcReq0 },	
/* 0x15 */	{ ir_error,		   ProcReq0 }, 
/* 0x16 */	{ ir_error,		   ProcReq0 },	
/* 0x17 */	{ ir_error,		   ProcReq0 },		
/* 0x18 */	{ ir_error,		   ProcReq0 },		
/* 0x19 */	{ ir_error,		   ProcReq0 },
/* 0x1a */	{ ir_error,	   	   ProcReq0 }
#endif
#endif /* !USE_EUC_PROTOCOL */
} ;

#ifdef EXTENSION
oreqproc ExtensionVector[] =
{
#ifdef USE_EUC_PROTOCOL
/* 0x00 */	{ ir_list_dictionary,	   ProcReq9 },
/* 0x01 */	{ ir_create_dictionary,    ProcReq8 },
/* 0x02 */	{ ir_remove_dictionary,    ProcReq8 },
/* 0x03 */	{ ir_rename_dictionary,    ProcReq10 },
/* 0x04 */	{ ir_get_word_text_dic,    ProcReq10 },
#else /* !USE_EUC_PROTOCOL */
/* 0x00 */	{ ir_error,	   ProcReq0 },
/* 0x01 */	{ ir_error,	   ProcReq0 },
/* 0x02 */	{ ir_error, 	   ProcReq0 },
/* 0x03 */	{ ir_error,    	   ProcReq0 },
/* 0x04 */	{ ir_error,	   ProcReq0 },
#endif /* !USE_EUC_PROTOCOL */
} ;
#endif /* EXTENSION */

char *ProtoName[] = {
    "IR_INIT",
    "IR_FIN",	
    "IR_CRE_CON",	
    "IR_DUP_CON",
    "IR_CLO_CON",
    "IR_DIC_LIST",	
    "IR_GET_YOMI",
    "IR_DEF_DIC",	
    "IR_UNDEF_DIC",	
    "IR_DIC_PATH",	
    "IR_DIR_LIST",	
    "IR_MNT_DIC",	
    "IR_UMNT_DIC",
    "IR_RMNT_DIC",
    "IR_MNT_LIST",
    "IR_CONVERT",	
    "IR_CONV_END",	
    "IR_KAN_LST",	
    "IR_RESIZE",
    "IR_STO_YOMI",		
    "IR_GET_LEX",	
    "IR_GET_STA",	
    "IR_SER_STAT",	
    "IR_SER_STAT2",	
    "IR_HOST_CTL",
    "IR_QUERY_EXT",
} ;			

#ifdef DEBUG
char *DebugProc[][2] = {
    { "ir_null",		      "ProcReq0" } ,
    { "ir_initialize",		      "ProcReq2" } ,
    { "ir_finalize",		      "ProcReq0" } ,
    { "ir_create_context",	      "ProcReq0" } ,
    { "ir_duplicate_context",	      "ProcReq1" } ,
    { "ir_close_context",	      "ProcReq1" } ,
    { "ir_dictionary_list",	      "ProcReq3" } ,
    { "ir_get_yomi",		      "ProcReq5" } ,
    { "ir_define_dic",		      "ProcReq7" } ,
    { "ir_delete_dic",		      "ProcReq7" } ,
    { "ir_set_dic_path",	      "ProcReq8" } ,
    { "ir_get_dir_list",	      "ProcReq3" } ,
    { "ir_mount_dictionary",	      "ProcReq8" } ,
    { "ir_umount_dictionary",	      "ProcReq8" } ,
    { "ir_rmount_dictionary",	      "ProcReq9" } ,
    { "ir_mount_list",		      "ProcReq3" } ,
    { "ir_convert",		      "ProcReq8" } ,
    { "ir_convert_end", 	      "ProcReq4" } ,
    { "ir_get_kanjilist",	      "ProcReq5" } ,
    { "ir_resize",		      "ProcReq5" } ,
    { "ir_store_yomi",		      "ProcReq9" } ,
    { "ir_get_lex",		      "ProcReq11"} ,
    { "ir_get_stat",		      "ProcReq5" } ,
    { "ir_server_stat", 	      "ProcReq0" } ,
    { "ir_server_stat2",	      "ProcReq0" } ,
    { "ir_host_ctl",		      "ProcReq0" } ,
    { "ir_query_extension",	      "ProcReq12" }
} ;			
#endif

char *ExtensionName[][2] = {
    /* Request Name		Start Protocol Number */					
#ifdef EXTENSION
    { REMOTE_DIC_UTIL,		"65536" }, /* 0x10000 */
#endif /* EXTENSION */
    { "",		      "" }
} ;
