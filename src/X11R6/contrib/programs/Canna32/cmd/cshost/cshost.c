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

#ifndef lint
static char sccs_id[]="@(#) NEC UNIX( PC-UX/EWS-UX ) cshost.c 2.1 91/11/11 11:17:51";
static char rcs_id[] = "$Id: cshost.c,v 4.23 1993/12/20 12:33:46 kon Exp $";
#endif

/*
 * MODIFICATION HISTORY
 *	S000	funahasi@oa2	Fri Dec  4 02:44:09 JST 1992
 *		- cshostから rkcの内部関数を呼ぶのを止める
 *		  rkcに interfaceを作ったのでそれを使用する．
 *	S001	funahasi@oa2	Fri Jan  8 16:02:31 JST 1993
 *		- fixed bug rkc_Connect_iroha_Server()を RkInitialize()に
 *		  変えたために cannahostnameが設定されなくなった．
 *	S002	funahasi@oa2	Mon Jan 11 13:04:44 JST 1993
 *		- fixed bug cannahostnameを "unix"で初期化したため環境変数
 *		  CANNAHOSTを使用できなくなった．
 *		- "-cs"optionで hostnameがない時は，usageを出力する．
 */

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#include    <stdio.h>
#include    <time.h>
#include    <errno.h>
#include    <sys/types.h>
#if __STDC__
#include    <stdlib.h>
#endif

#include    "sglobal.h"
#include    "IR.h"
#include    "net.h"

#ifdef NTOHS
#undef NTOHS
#endif

#define NTOHS( data, len ) { \
	short work ; \
	bcopy( (char *)(data), (char *)&work, sizeof( short ) ) ; \
	len = ntohs( (short)work ) ; \
}

static IRwReq cRreq ;
static IRReq iRreq ;
static int ServerFD ;
static int ProtocolVersion ;

extern int errno;

main(argc, argv)
int argc ;
char **argv ;
{
    wReq1		*creq ;
    Req0		*ireq ;
    char		*hostname = (char *)NULL ;
    char		cannahostname[256], *RkGetServerName();
    int 		i, ResevInt ;
    int 		ServerVersion ; 
    int proto_major, proto_minor, cx;				/* S000 */

    cannahostname[0] = '\0';					/* S002 */
    for( i = 1; i < argc; i++ ) {
	if((!strcmp( argv[ i ], "-cs" ) || !strcmp( argv[ i ], "-cannaserver" )
	    || !strcmp( argv[ i ], "-is" )
	    || !strcmp( argv[ i ], "-irohaserver"))) {
	  if (++i < argc) {
	    strcpy( cannahostname, argv[i] ) ;
	  } else						/* S002 */
	      usage();						/* S002 */
	} else
	    usage() ;
    }
							/* begin:S000 */
    if( (cx = RkwInitialize( cannahostname )) == -1 ){
	fprintf( stderr,"Error Disconnected %s\n", cannahostname );
	exit(2);
    }
    ServerFD = RkcGetServerFD();
    strcpy( cannahostname, RkGetServerName() );			/* S002 */
							/* end:S000 */
    printf("Connected to %s\n", cannahostname ) ;
							/* begin:S000 */
    RkGetProtocolVersion( &proto_major, &proto_minor );
    if( proto_major < 2 )
	ProtocolVersion = 1;
    else
	ProtocolVersion = 0;
							/* end:S000 */
    /*	パケット組み立て */
    if( !ProtocolVersion ) {
	creq = &cRreq.type1 ;
	creq->type = wGetAccessControlList ;
	creq->none = 0x01 ;
	creq->datalen = htons( 0 ) ;
	WriteToServer( cRreq.Buffer, sizeof( wReq1 )) ;

	CannaDispControlList() ;

	RkwFinalize();						/* S000 */
    } else {
	ireq = &iRreq.Request0 ;
	ireq->Type = htonl( IR_HOST_CTL ) ;
	WriteToServer( iRreq.Buffer, sizeof( Req0 )) ;

	IrohaDispControlList() ;
    }
}

CannaDispControlList()
{
    char    ResevBuf[ BUFSIZE ], *wp ;
    int     ResevInt, UserNum, HostNum, NameLen ;
    int     i, j ;
    short   ResevShort ;

    wp = ResevBuf ;
    ReadServer( (char *)&ResevShort, sizeof( short ) ) ;
    ReadServer( (char *)&ResevShort, sizeof( short ) ) ;
    ResevShort = ntohs( ResevShort ) ;
    ReadServer( ResevBuf, ResevShort ) ;
    NTOHS( wp, HostNum ) ;
    wp += sizeof( short ) ;
    printf("access control enabled\n" ) ;
    for( i = 0; i < HostNum; i++ ) {
	printf("HOST NAME:%s\n", wp ) ;
	wp += strlen( wp ) + 1 ;
	if( *wp ) {
	    printf("USER NAME:" ) ;
	    while( *wp ) {
		printf("%s ", wp ) ;
		wp += strlen( wp ) + 1 ;
	    }
	} else
	    printf("ALL USER" ) ;
	printf("\n\n") ;
	wp++;
    }
}

IrohaDispControlList()
{
    char    ResevBuf[ BUFSIZE ], *wp ;
    int     ResevInt, UserNum, HostNum, NameLen ;
    int     i, j ;

    wp = ResevBuf ;
    ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
    HostNum = ntohl( ResevInt ) ;
    ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
    ResevInt = ntohl( ResevInt ) ;
    ReadServer( ResevBuf, ResevInt ) ;
    printf("access control enabled\n" ) ;
    for( i = 0; i < HostNum; i++ ) {
	DATATOLEN( wp, NameLen )	; wp += sizeof( int ) ;
	printf("HOST NAME:%s\n", wp )	; wp += NameLen ;
	DATATOLEN( wp, UserNum )	; wp += sizeof( int ) ;
	if( UserNum )
	    printf("USER NAME:" ) ;
	else
	    printf("ALL USER" ) ;
	for( j = 0; j < UserNum; j++ ) {
	    DATATOLEN( wp, NameLen ) ;
	    wp += sizeof( int ) ;
	    printf("%s ", wp ) ;
	    wp += NameLen ;
	}
	printf("\n\n") ;
    }
    RkwFinalize();						/* S000 */
}
int
WriteToServer( Buffer, size )
char *Buffer ;
int size ;
{
    register int todo;
    register int write_stat;
    register char *bufindex;
    int cnt ;

    errno = 0 ;
    bufindex = Buffer ;
    todo = size ;
    while (size) {
	errno = 0;
	write_stat = write(ServerFD, bufindex, (unsigned int) todo);
	if (write_stat >= 0) {
	    size -= write_stat;
	    todo = size;
	    bufindex += write_stat;
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
	    errno = EPIPE ;
	    perror( "write faild" ) ;
	    exit( 2 ) ;
	}
    }
}

int
ReadServer( Buffer, size )
char *Buffer ;
int size ;
{
    register long bytes_read;

    if (size == 0) return;
    errno = 0;
    while ((bytes_read = read(ServerFD, Buffer, (unsigned int)size)) != size) {	
	if (bytes_read > 0) {
	    if( bytes_read == sizeof( int ) ){
		if( ntohl( *((int *)Buffer) ) == -1 ){
		    perror("ReadToServer") ;
		    exit( 1 ) ;
		}
	    }
	    size -= bytes_read;
	    Buffer += bytes_read;
	} else if (errno == EWOULDBLOCK) { /* pc98 */
	    continue ;
	} else {
	    perror("ReadToServer") ;
	    exit( 1 ) ;
	}
    }
    return( 0 ) ;
}

usage()
{
    fprintf( stderr, "usage: cshost [-cs | -cannaserver hostname]\n" ) ;
    fflush( stderr ) ;
    exit( 0 ) ;
}
