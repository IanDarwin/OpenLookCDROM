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
static char sccs_id[]="@(#) NEC UNIX( PC-UX/EWS-UX ) cannastat.c 2.1 91/11/11 11:06:30";
static char rcs_id[] = "$Id: cannastat.c,v 1.31 1993/12/20 12:35:14 kon Exp $";
#endif

/*
 * MODIFICATION HISTORY
 *	S000	funahasi@oa2	Fri Oct  2 20:14:13 JST 1992
 *		- fixed bug clientがいない時，cannastatをすると cannaserver
 *		  が Read request failedを起こす
 *		- fixed bug protocol 3.0にした時の改造もれ
 *		- fixed bug irohaに接続した時，clientとして cannastatの分まで
 *		  countしてしまう
 *	S001	funahasi@oa2	Thu Oct  8 20:19:24 JST 1992
 *		- fixed bug client名長を longで取得しようとするので，後の
 *		  dataがずれてしまう．shortで取得するのが正しい．
 *	S002	funahasi@oa2	Fri Dec  4 02:24:46 JST 1992
 *		- cannastatから rkcの内部関数を呼ぶのを止める
 *		  rkcに interfaceを作ったのでそれを使用する．
 *	S003	funahasi@oa2	Fri Jan  8 16:14:03 JST 1993
 *		- fixed bug rkc_Connect_Iroha_Server()を RkInitialize()に
 *		  変更したため cannahostnameが設定されなくなった．
 *	S004	funahasi@oa2	Mon Jan 11 13:04:44 JST 1993
 *		- fixed bug cannahostnameを "unix"で初期化したため環境変数
 *		  CANNAHOSTを使用できなくなった．
 *		- "-cs"optionで hostnameがない時は，usageを出力する．
 *	S005	funahasi@oa2	Mon Jan 11 18:12:16 JST 1993
 *		- fixed bug ServerVersionの初期化が出来ていない．
 */

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#include    <stdio.h>
#include    <time.h>
#include    <sys/types.h>
#include    <errno.h>

#include    "net.h"
#include    "sglobal.h"
#include    "IR.h"

#define PROTO	1
#define ALL	2
#define TOTAL	3

#define EATFORMAT "%-10s%4s%4s%5s%16s%9s%7s %-10s %-10s\n"
#define EADFORMAT "%-10s%4d%4d%5d%16s%9s%7s %-10s %-10s\n"

#define ATFORMAT "%-10s%5s%5s%5s%16s%9s%9s  %-s\n"
#define ADFORMAT "%-10s%5d%5d%5d%16s%9s%9s  %-s\n"

#define BTFORMAT "%7s %7s %7s %11s %11s\n"
#define BDFORMAT "%7d %7d %7d %11s %11s\n\n"

#ifdef NTOHS
#undef NTOHS
#endif

#define NTOHS( data, len ) { \
	short work ; \
	bcopy( (char *)(data), (char *)&work, sizeof( short ) ) ; \
	len = ntohs( (short)work ) ; \
}

static int ServerFD ;
static int ServerVersion ;
static int ProtocolVersion ;
static int getdatasize;						/* S002 */

static IRwReq cRreq ;
static IRReq iRreq ;
static long cur_time ;	

extern int errno;

static int
*TotalReqCount, OldTotalReqCount[ OLD_MAXREQUESTNO ],
ProtoCount,
ListSize,
ContextNum ;

static char
*ProtoList, *ContextFlag ;
char		major_version, minor_version;

main(argc, argv)
int argc ;
char **argv ;
{
    char		cannahostname[ 256 ] ;
    wReq1		*creq ;
    Req0		*ireq ;
    ClientRec		client ;		
    ClientStatRec	clientstat ;		
    int 		i, ResevInt, iConectNum ;
    char		*hostname = (char *)NULL, *Version, *Buffer ;
    int 		argflag = 0, flag = 0 ;
    short		ResevShort, cConectNum;
    char		ResevChar, *RkGetServerName();
    int proto_major, cx;					/* S002 */

    cannahostname[0] = '\0';					/* S004 */
    for( i = 1; i < argc; i++ ) {
	if((!strcmp( argv[ i ], "-cs" ) || !strcmp( argv[ i ], "-cannaserver" )
	    ||!strcmp( argv[ i ], "-is" )
	    || !strcmp( argv[ i ], "-irohaserver"))) {
	  if (++i < argc) {
	    strcpy( cannahostname, argv[i] ) ;
	  } else						/* S004 */
	      usage();						/* S004 */
	} else if( !strcmp( argv[ i ], "-p" ))	
	    argflag = PROTO ;
	else if( !strcmp( argv[ i ], "-a" ))
	    argflag = ALL ;
	else if( !strcmp( argv[ i ], "-t" ))
	    argflag = TOTAL ;
	else if( !strcmp( argv[ i ], "-v" ))
	    flag = 1 ;
	else
	    usage() ;
    }
							/* begin:S002 */
    if( (cx = RkwInitialize( cannahostname )) == -1 ){
	fprintf( stderr,"Error Disconnected %s\n", cannahostname );
	exit(2);
    }
    ServerFD = RkcGetServerFD();
    strcpy( cannahostname, RkGetServerName() );			/* S004 */

    printf("Connected to %s\n", cannahostname ) ;

    RkGetProtocolVersion( &proto_major, &ServerVersion );	/* S005 */
    if( proto_major < 2 )
	ProtocolVersion = 1;
    else
	ProtocolVersion = 0;
							/* end:S002 */
    if( !ProtocolVersion ) {
	/* パケット組み立て */
	creq = &cRreq.type1 ;

    /*
	ServerFD = rkc_Connect_Iroha_Server( cannahostname ) ;
    */

	creq->type = wGetServerInfo;
	creq->none = 0x01;
	creq->datalen = htons( 0 );

	if( WriteToServer( cRreq.Buffer, sizeof( wReq1 )) == -1 ) {
	    perror("WriteToServer") ;
	    exit( 1 ) ;
	}

	ReadServer( (char *)&ResevChar, sizeof( char ) );
	ReadServer( (char *)&ResevChar, sizeof( char ) );
	ReadServer( (char *)&ResevShort, sizeof( short ) );
	getdatasize = (unsigned short)ResevShort;		/* S002 */

	ReadServer( (char *)&ResevChar, sizeof( char ) );
	/* サーババージョンを取得する */
	ReadServer( (char *)&ResevChar, sizeof( char ) ) ;
	major_version = ResevChar;
	ReadServer( (char *)&ResevChar, sizeof( char ) ) ;
	minor_version = ResevChar;
	printf("Canna Server (Ver. %d.%d)\n", major_version, minor_version ) ;

	/* サーバのカレント時間 */
	ReadServer( (char *)&cur_time, sizeof( int ) ) ;
	cur_time = ntohl( cur_time ) ;

	/* プロトコル数 */
	ReadServer( (char *)&ResevShort, sizeof( short ) ) ;
	ProtoCount = ntohs( ResevShort ) ;
	    
	/* プロトコル名リスト長 */
	ReadServer( (char *)&ResevShort, sizeof( short ) ) ;
	ListSize = ntohs( ResevShort ) ;
	    
	/* プロトコル名リスト取得 */
	ProtoList = (char *)malloc( ListSize ) ;
	ReadServer( ProtoList, ListSize ) ;
	    
	/* プロトコル使用頻度取得 */
	TotalReqCount = (int *)calloc( ProtoCount, sizeof( int ) ) ;
	ReadServer( (char *)TotalReqCount, ProtoCount*(int)sizeof( int ) ) ;

	for( i = 0; i < ProtoCount; i++ )
	    DATATOLEN( (char *)&TotalReqCount[ i ], TotalReqCount[ i ] ) ;

	ReadServer( (char *)&cConectNum, sizeof( short ) ) ;
	cConectNum = ntohs( cConectNum ) ;

	if( cConectNum )
	    printf("Total connecting clients %d\n" , cConectNum ) ;
	else {
	    printf("No clients\n") ;
	}

	if( argflag == TOTAL ) {
	    DispProto( (ClientPtr)NULL ) ;
	    putchar( '\n' ) ;
	    RkwFinalize();					/* S002 */
	    exit( 0 ) ;
	} else if( !cConectNum ){
	    RkwFinalize();					/* S002 */
	    exit( 0 ) ;
	}

	if( !flag && (!argflag || (argflag == ALL)) ) {
	    if( !ProtocolVersion && (major_version > 2) )	/* S000 */
		printf( EATFORMAT, "USER_NAME", "ID", "NO", "U_CX", "C_TIME",
			"U_TIME", "I_TIME", "HOST_NAME", "CLIENT" ) ;
	    else
		printf( ATFORMAT, "USER_NAME", "ID", "NO", "U_CX", "C_TIME",
			"U_TIME", "I_TIME", "HOST_NAME" ) ;
	} else {
	    printf("\n") ;
	}

	ReadServer( (char *)&ResevShort, sizeof( short ) ) ;
	ContextNum = ntohs( ResevShort ) ;
	ContextFlag = (char *)malloc( ContextNum ) ;

	for( i = 0; i < cConectNum ; i++ ) {
	    if( getdatasize <= 0 ){			/* beign:S002 */
		printf( " ...Sorry, but I can't read Server status.\n" );
		break;
	    }						/* end:S002 */

	    ReadServer( (char *)&ResevShort, sizeof( short ) ) ;
	    ResevShort = ntohs( ResevShort ) ;

	    Buffer = (char *)malloc( ResevShort ) ;
	    
	    ReadServer( (char *)Buffer, ResevShort ) ;
	    
	    bzero( &client, sizeof( ClientRec ) ) ;
	    bzero( ContextFlag, ContextNum ) ;
	    CreateData( Buffer, &client ) ;

	    switch( argflag ) {
		case PROTO :
		    printf("%s\n", client.username ) ;
		    DispProto( &client ) ;
		    putchar( '\n' ) ;
		    break ;
		case ALL :
		    DispInfo( &client, flag ) ;
		    DispProto( &client ) ;
		    putchar( '\n' ) ;
		    break ;
		default :
		    DispInfo( &client, flag ) ;
		    break ;
	    }
	    free( client.username ) ;
	    free( client.hostname ) ;
	    if (major_version > 2 && client.clientname) {
	      free( client.clientname ) ;
	    }
	    free( Buffer ) ;
	}
	RkwFinalize();						/* S002 */
    } else {
	/* irohastat compatible */
	/* パケット組み立て */
	ireq = &iRreq.Request0 ;

	RkwFinalize();						/* S002 */
	ServerFD = RkcConnectIrohaServer( cannahostname );	/* S002 */

	if( ServerVersion ) {
	    ireq->Type = htonl( IR_SER_STAT2 ) ;
	} else
	    ireq->Type = htonl( IR_SER_STAT ) ;

	if( WriteToServer( iRreq.Buffer, sizeof( Req0 )) == -1 ) {
	    perror("WriteToServer") ;
	    exit( 1 ) ;
	}

	/* サーババージョンを取得する */
	ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
	ResevInt = ntohl( ResevInt ) ;
	if( Version = (char *)malloc( ResevInt ) ) {
	    ReadServer( Version, ResevInt ) ;
	    printf("Iroha Server (Ver. %s)\n", Version ) ;
	    if( strcmp( Version, "1.0" ) )
		ServerVersion = 1 ;
	    else
		ServerVersion = 0 ;
	    free( Version ) ;
	} else {
	    printf("Iroha Server (Ver. XX)\n" ) ;
	}

	if( ServerVersion ) {
	    /* サーバのカレント時間 */
	    ReadServer( (char *)&cur_time, sizeof( int ) ) ;

	    /* プロトコル数 */
	    ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
	    ProtoCount = ntohl( ResevInt ) ;
	    
	    /* プロトコル名リスト長 */
	    ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
	    ListSize = ntohl( ResevInt ) ;
	    
	    /* プロトコル名リスト取得 */
	    ProtoList = (char *)malloc( ListSize ) ;
	    ReadServer( ProtoList, ListSize ) ;
	    
	    TotalReqCount = (int *)calloc( ProtoCount, sizeof( int ) ) ;
	    ReadServer( (char *)TotalReqCount, ProtoCount*(int)sizeof( int ) ) ;
	    
	    for( i = 0; i < ProtoCount; i++ )
		DATATOLEN( (char *)&TotalReqCount[ i ], TotalReqCount[ i ] ) ;

	    ReadServer( (char *)&iConectNum, sizeof( int ) ) ;
	} else {	
	    ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
	    ResevInt = ntohl( ResevInt ) ;

	    ReadServer( (char *)OldTotalReqCount, ResevInt ) ;
	    
	    ReadServer( (char *)&iConectNum, sizeof( int ) ) ;
	    
	    ReadServer( (char *)&cur_time, sizeof( int ) ) ;
	}
	iConectNum = ntohl( iConectNum ) ;
	cur_time = ntohl( cur_time ) ;

	if( iConectNum )
	    printf("Total connecting clients %d\n" , iConectNum ) ;
	else {
	    printf("No clients\n") ;
	}

	if( argflag == TOTAL ) {
	    DispProto( (ClientPtr)NULL ) ;
	    putchar( '\n' ) ;
	    exit( 0 ) ;
	} else if( !iConectNum )
	    exit( 0 ) ;

	if( !flag && (!argflag || (argflag == ALL)) ) {
	    printf( ATFORMAT,
       "USER_NAME", "ID", "NO", "U_CX", "C_TIME", "U_TIME", "I_TIME", "HOST_NAME" ) ;
	} else {
	    printf("\n") ;
	}
	if( ServerVersion ) {
	    ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
	    ContextNum = ntohl( ResevInt ) ;
	    ContextFlag = (char *)malloc( ContextNum ) ;
	} else {
	    ContextFlag = (char *)malloc( 100 ) ;
	}
	for( i = 0; i < iConectNum ; i++ ) {
	    if( ServerVersion ) {
		ReadServer( (char *)&ResevInt, sizeof( int ) ) ;
		ResevInt = ntohl( ResevInt ) ;

		Buffer = (char *)malloc( ResevInt ) ;
	    
		ReadServer( (char *)Buffer, ResevInt ) ;
	    
		bzero( &client, sizeof( ClientRec ) ) ;
		bzero( ContextFlag, ContextNum ) ;
		CreateData( Buffer, &client ) ;
	    } else {
		bzero( &clientstat, sizeof( ClientStatRec ) ) ;
		ReadServer( (char *)&clientstat, sizeof( ClientStatRec ) ) ;
		ConvertData( &clientstat, &client ) ;
	    }

	    switch( argflag ) {
		case PROTO :
		    printf("%s\n", client.username ) ;
		    DispProto( &client ) ;
		    putchar( '\n' ) ;
		    break ;
		case ALL :
		    DispInfo( &client, flag ) ;
		    DispProto( &client ) ;
		    putchar( '\n' ) ;
		    break ;
		default :
		    DispInfo( &client, flag ) ;
		    break ;
	    }
	    if( ServerVersion ) {
		free( client.username ) ;
		free( client.hostname ) ;
		free( Buffer ) ;
	    }
	}
    }
}

DispInfo( client, flag )
register ClientPtr client ;
int flag ;
{
    static char *week[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" } ;
    char	ctime[ 15 ], utime[ 10 ], itime[ 10 ] ;
    char	name[ 10 ], host[ 15 ], appname[15];
    char	*ascdate ;
    int 	i, id, user_no, u_cx ;
    long	cdate, udate, idate ;
    struct tm	*tt, *tt_buf ;

    id = client->id ;
    user_no = client->usr_no ;
    udate = client->used_time ;

    /* コネクト時間の整形 */
    cdate = client->connect_date ;
    tt = localtime( &cdate ) ;
    if( flag ) {
	ascdate = asctime( tt ) ;
    } else {
	sprintf( ctime,"%s %d", week[ tt->tm_wday ], tt->tm_mday ) ;
	if( tt->tm_hour > 12 )
	  sprintf( ctime,"%s %2d:%02dpm", ctime, tt->tm_hour - 12, tt->tm_min ) ;
	else if( tt->tm_hour == 12 )
	  sprintf( ctime,"%s 12:%02dpm", ctime, tt->tm_min ) ;
	else if( tt->tm_hour == 0 )
	  sprintf( ctime,"%s 12:%02dam", ctime, tt->tm_min ) ;
	else
	  sprintf( ctime,"%s %2d:%02dam", ctime, tt->tm_hour, tt->tm_min ) ;
	
	sprintf( utime,"%02d:%02d:%02d", udate/3600, (udate%3600)/60
						    , (udate%3600)%60 ) ;
    }

    if (udate < 3600) {
	if (udate < 60) 
	    sprintf( utime, "      %2d", udate);
	else
	    sprintf( utime, "   %2d'%02d", udate/60, udate%60 );
    } else {
      sprintf( utime,"%02d:%02d'%02d",
		udate/3600, (udate%3600)/60, (udate%3600)%60 ) ;
    }

    idate = cur_time - client->idle_date ;
    /* １分以内は表示しない */
    if( idate < 60 )
	strcpy( itime, "" ) ;
    else if ( idate < 3600)
	sprintf (itime, "   %2d", idate / 60);
    else
	sprintf( itime,"%2d:%02d", idate/3600, (idate%3600)/60 ) ;
				
    for( i = 0, u_cx = 0; i < ContextNum ; i++ )
	if( ContextFlag[ i ] )
	    u_cx ++ ;

    if( flag ) {
	printf( "USER_NAME    : %s\n", client->username ) ;
	printf( "HOST_NAME    : %s\n", client->hostname ) ;
	if( !ProtocolVersion &&  (major_version > 2) )		/* S000 */
	    printf( "CLIENT_NAME  : %s\n", client->clientname ) ;
	printf( "CONNECT_DATE : %s", ascdate ) ;
	printf( BTFORMAT,
	"USER_ID", "USER_NO", "USED_CX", "USED_TIME","IDLE_TIME" ) ;
	printf( BDFORMAT, id, user_no, u_cx, utime, itime ) ;
    } else {
	bzero( name, 10 ) ;
	bzero( host, 15 ) ;
	bzero( appname, 15 ) ;
	strncpy( name, client->username, 9 ) ;
	if( !ProtocolVersion &&  (major_version > 2) ){		/* S000 */
	    strncpy( host, client->hostname, 10 ) ;
	    strncpy( appname, client->clientname, 10 ) ;
	    printf( EADFORMAT, name, id,
			user_no, u_cx, ctime, utime, itime, host, appname ) ;
	} else {
	    strncpy( host, client->hostname, 14 ) ;
	    printf( ADFORMAT, name, id,
			user_no, u_cx, ctime, utime, itime, host ) ;
	}
    }	
}

DispProto( client )
register ClientPtr client ;
{
    static char *OldProtoList[] = {
    "IR_INIT", "IR_FIN", "IR_CRE_CON", "IR_DUP_CON", "IR_CLO_CON",
    "IR_DIC_LIST", "IR_CRE_DIC", "IR_DEF_DIC", "IR_UNDEF_DIC", "IR_DIC_PATH",
    "IR_DIR_LIST", "IR_MNT_DIC", "IR_UMNT_DIC", "IR_RMNT_DIC", "IR_MNT_LIST",
    "IR_CONVERT", "IR_CONV_END", "IR_KAN_LST", "IR_RESIZE", "IR_STO_YOMI",
    "IR_GET_LEX", "IR_GET_STA"
    } ;
    register int i ;
    char *protoname ;
    int  *dispdata ;

    if( !ProtocolVersion ) {
	protoname = ProtoList ;
	dispdata = TotalReqCount ;

	for( i = 0; i < ProtoCount; i++ ) {
	    printf("%23s:%6d", protoname, dispdata[ i ] ) ;
	    protoname += ( strlen( protoname ) + 1 ) ;
	    if( !((i+1)%2) ) putchar('\n') ;
	}
    } else {
	if( ServerVersion ) {
	    protoname = ProtoList ;
	    dispdata = TotalReqCount ;
	} else {
	    ProtoCount = OLD_MAXREQUESTNO - 2 ;
	}

	for( i = 0; i < ProtoCount; i++ ) {
	    if( ServerVersion ) {
		printf("%12s:%6d", protoname, dispdata[ i ] ) ;
		protoname += ( strlen( protoname ) + 1 ) ;
	    } else {
		printf("%12s:%6d", OldProtoList[ i ], ntohl( OldTotalReqCount[ i+1 ] ) ) ;
	    }
	    if( !((i+1)%4) ) putchar('\n') ;
	}
    }
    putchar('\n') ;			
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
	write_stat = write(ServerFD, bufindex, (int) todo);
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
	    return( -1 ) ;
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
    getdatasize -= size;
    while ((bytes_read = read(ServerFD, Buffer, (int)size)) != size) {	
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

CreateData( readbuf, who )
char *readbuf ;
ClientPtr   who ;
{
    char *receivep = readbuf ;
    int len, j ;

    DATATOLEN( receivep, who->id ) ; receivep += sizeof( int ) ;
    DATATOLEN( receivep, who->usr_no ) ; receivep += sizeof( int ) ;
    DATATOLEN( receivep, who->used_time ) ; receivep += sizeof( int ) ;
    DATATOLEN( receivep, who->idle_date ) ; receivep += sizeof( int ) ;
    DATATOLEN( receivep, who->connect_date ) ; receivep += sizeof( int ) ;

    for( j = 0; j < ProtoCount; j++ ) {
	DATATOLEN( receivep, TotalReqCount[ j ] ) ;
	receivep += sizeof( int ) ;
    }

    if( ProtocolVersion ) {
	DATATOLEN( receivep, len ) ; receivep += sizeof( int ) ;
    } else {
	NTOHS( receivep, len ) ; receivep += sizeof( short ) ;
    }
    if( who->username = (char *)malloc( len ) )
	bcopy( receivep, who->username, len ) ; 
    receivep += len ;

    if( ProtocolVersion ) {
	DATATOLEN( receivep, len ) ; receivep += sizeof( int ) ;
    } else {
	NTOHS( receivep, len ) ; receivep += sizeof( short ) ;
    }
    if( who->hostname = (char *)malloc( len ) )
	bcopy( receivep, who->hostname, len ) ; 
    receivep += len ;

    if( !ProtocolVersion &&  (major_version > 2) ){	/* S000:begin */
	NTOHS( receivep, len ) ; receivep += sizeof( short );	/* S001 */
	if( who->clientname = (char *)malloc( len ) )
	    bcopy( receivep, who->clientname, len ) ; 
	receivep += len ;
    }							/* S000:end */

    if( ContextFlag )
	bcopy( receivep, ContextFlag, ContextNum ) ;
}

ConvertData( clientstat, client )
ClientStatPtr	clientstat ;
ClientPtr	client ;
{
    register int i ;

    client->id = ntohl( clientstat->id ) ;		
    client->usr_no = ntohl( clientstat->usr_no ) ;	
    client->used_time = ntohl( clientstat->used_time ) ;	
    client->idle_date = ntohl( clientstat->idle_date ) ;	
    client->connect_date = ntohl( clientstat->connect_date ) ;	
    client->username = clientstat->username ;
    client->hostname = clientstat->hostname ;
    for( i = 0; i < 0x18; i++ )
	OldTotalReqCount[ i ] = clientstat->pcount[ i ] ;
    bcopy( client->context_flag, ContextFlag, 100 ) ;
}


usage()
{
    fprintf( stderr, "usage: cannastat [-cs | -cannaserver hostname] [-a|-v]\n" ) ;
    fprintf( stderr, "                 [-cs | -cannaserver hostname] [-t]\n" ) ;
    fprintf( stderr, "                 [-cs | -cannaserver hostname] [-p]\n" ) ;

    fflush( stderr ) ;
    exit( 0 ) ;
}
