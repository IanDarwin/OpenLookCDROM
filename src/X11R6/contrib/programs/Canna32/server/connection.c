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
  This program contains a part of the X server.  The communication part
  of the X server is modified and built into this program.
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
static char rcs_id[] = "$Id: connection.c,v 3.26 1994/03/09 07:05:28 kon Exp $";
#endif


/* LINTLIBRARY */

#include <stdio.h>
#include <sys/types.h>
#include <errno.h>

#ifdef nec_ews_svr2 /* sys/time.h で time.h をインクルードしていない */
#include <time.h>
#endif
#include <sys/time.h>		
#include <sys/times.h>
#ifdef AIXV3
#include <sys/select.h>
#endif

#include "net.h"
#include "IR.h"

extern int errno;

long *AllSockets;	   /* select on this */
long *AllClients;	   /* available clients */
long *LastSelectMask  ;    /* mask returned from last select call */
long *clientsReadable;	   /* WaitForSomething で使うテーブル */
long WellKnownConnections;	   /* Listener mask */

char ir_default_dic_path[] = DDPATH ;
char err_hostname[] = "no host" ;

extern ClientPtr *ConnectionTranslation;
extern ClientPtr *clientReady, *newClients;
extern UserTblPtr *usertabl;
extern unsigned long connow_socks;

static struct sockaddr_un unsock;

static int open_unix_socket ()
{
    extern int PortNumberPlus  ;
    int oldUmask;
    int request;

    unsock.sun_family = AF_UNIX;
    oldUmask = umask (0);

    if ( mkdir( IR_UNIX_DIR, 0777 ) == -1 &&
	errno != EEXIST ) {
	(void)fprintf(stderr, "Can't open ");
	(void)fprintf(stderr, IR_UNIX_DIR) ;
	(void)fprintf(stderr, ", error No. %d\n", errno);
    }
    strcpy (unsock.sun_path, IR_UNIX_PATH);
    if( PortNumberPlus )
	sprintf( unsock.sun_path, "%s:%d", unsock.sun_path, PortNumberPlus ) ;
    unlink( unsock.sun_path ) ;
    if ((request = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
	perror ("Creating Unix socket");
    }
    else
    {
	if (bind(request, (struct sockaddr *)&unsock,
		 strlen(unsock.sun_path) + 2) == 0) {
	  if (listen (request, 5)) {
	    perror ("Unix Listening");
	    close(request);
	    request = -1;
	  }
	}
	else {
	  perror ("Binding Unix socket");
	  close(request);
	  request = -1;
	}
    }

    (void)umask( oldUmask ) ;
    return request;
}

void
CreateWellKnownSockets()
{
    extern int PortNumberPlus  ;
    struct sockaddr_in	insock;
    struct servent	*sp ;
    int 		retry, request ;

    CLEARBITS(AllSockets);
    CLEARBITS(AllClients);
    CLEARBITS(LastSelectMask);

   ir_debug( Dmsg(3,"今からソケットを作る\n"); )
    if ((request = open_unix_socket ()) != -1)
    {
	WellKnownConnections |= (1L << request);
	AllSockets[0] = WellKnownConnections;
    }
    else
    {
	perror ("Creating TCP socket");
    }

   ir_debug( Dmsg(3,"ＵＮＩＸドメインはできた\n"); )
    /* /etc/servicesからポート番号を取得する */
    if( (sp = getservbyname(IR_SERVICE_NAME ,"tcp")) == NULL )
    {
	fprintf(stderr,"WARNING:\n");
	fprintf(stderr,"        Port number not find on '/etc/services'.\n");
	fprintf(stderr,"        Use %d as default.\n", IR_DEFAULT_PORT);
    }

    if ((request = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    {
	perror("warning: TCP socket not created");
    }
    else
    {
#ifdef SO_REUSEADDR
	{
	    int one = 1;
	    setsockopt(request, SOL_SOCKET, SO_REUSEADDR,
		       (char *)&one, sizeof(int));
	}
#endif
	bzero ((char *)&insock, sizeof (insock));
	insock.sin_family = AF_INET;
	insock.sin_port =
	  (sp ? ntohs(sp->s_port) : IR_DEFAULT_PORT) + PortNumberPlus;
	insock.sin_port = htons(insock.sin_port);
	insock.sin_addr.s_addr = htonl(INADDR_ANY);

	retry = 5;
	while ( bind(request, (struct sockaddr *)&insock, sizeof(insock)) < 0 ) /* PC98 */
	{
	    if (--retry == 0){
		perror ("Binding MSB TCP socket");
		close(request);
		break ;
	    }
	    sleep (1);
	}
	if( retry ) {
	    if (listen (request, 5)){
		perror ("Reg TCP Listening");
		close(request);
	    }
	    else {
	        WellKnownConnections |= (1 << request);
		ir_debug( Dmsg(3,"インターネットはできた\n"); )
	    }
	}
    }


    if (WellKnownConnections == 0) {
      extern void FatalError();

      FatalError("No Listeners, nothing to do");
    }

    AllSockets[0] = WellKnownConnections;
   ir_debug( Dmsg(3,"ソケットの準備はできた\n"); )
}

/*
  WidenTables -- MAXSOCKS で多きさが管理されていた配列を大きくする

  成功したら 1 を返し、失敗したら 0 を返す。

 */

WidenTables(newminsize)
int newminsize;
{
  int i, n = connow_socks, m;
  ClientPtr *newtrans, *newready, *newptr;
  UserTblPtr *newtable;
  long *newsocks, *newclients, *newmasks, *newreadable;

  while (n <= newminsize) {
    n += ADDSOCKS;
  }
  m = (n + 31) / 32;

  newtrans = (ClientPtr *)malloc(n * sizeof(ClientPtr));
  if (newtrans) {
    newready = (ClientPtr *)malloc(n * sizeof(ClientPtr));
    if (newready) {
      newptr = (ClientPtr *)malloc(n * sizeof(ClientPtr));
      if (newptr) {
	newtable = (UserTblPtr *)malloc(n * sizeof(UserTblPtr));
	if (newtable) {
	  newsocks = (long *)malloc(m * sizeof(long));
	  if (newsocks) {
	    newclients = (long *)malloc(m * sizeof(long));
	    if (newclients) {
	      newmasks = (long *)malloc(m * sizeof(long));
	      if (newmasks) {
		newreadable = (long *)malloc(m * sizeof(long));
		if (newreadable) {
		  /* 今までのをコピーする */
		  for (i = 0 ; i < connow_socks ; i++) {
		    newtrans[i] = ConnectionTranslation[i];
		    newready[i] = clientReady[i];
		    newptr[i] = newClients[i];
		    newtable[i] = usertabl[i];
		  }
		  for (i = connow_socks ; i < n ; i++) {
		    newtrans[i] = (ClientPtr)NULL;
		    newready[i] = (ClientPtr)NULL;
		    newptr[i] = (ClientPtr)NULL;
		    newtable[i] = (UserTblPtr)NULL;
		  }
		  for (i = 0 ; i < mskcnt ; i++) {
		    newsocks[i] = AllSockets[i];
		    newclients[i] = AllClients[i];
		    newmasks[i] = LastSelectMask[i];
		    newreadable[i] = clientsReadable[i];
		  }
		  for (i = mskcnt ; i < m ; i++) {
		    newsocks[i] = 0L;
		    newclients[i] = 0L;
		    newmasks[i] = 0L;
		    newreadable[i] = 0L;
		  }

		  if (connow_socks) {
		    free((char *)ConnectionTranslation);
		    free((char *)clientReady);
		    free((char *)newClients);
		    free((char *)usertabl);
		  }
		  if (mskcnt) {
		    free((char *)AllSockets);
		    free((char *)AllClients);
		    free((char *)LastSelectMask);
		    free((char *)clientsReadable);
		  }
		  ConnectionTranslation = newtrans;
		  clientReady = newready;
		  newClients = newptr;
		  usertabl = newtable;
		  AllSockets = newsocks;
		  AllClients = newclients;
		  LastSelectMask = newmasks;
		  clientsReadable = newreadable;

		  connow_socks = n;
		  mskcnt = m;

		  return 1;
		}
		free((char *)newmasks);
	      }
	      free((char *)newclients);
	    }
	    free((char *)newsocks);
	  }
	  free((char *)newtable);
	}
	free((char *)newptr);
      }
      free((char *)newready);
    }
    free((char *)newtrans);
  }
  return 0;
}

/*****************
 * EstablishNewConnections
 *    If anyone is waiting on listened sockets, accept them.
 *    Returns a mask with indices of new clients.  Updates AllClients
 *    and AllSockets.
 *****************/

void
EstablishNewConnections(nnew)
int		    *nnew;
{
    extern ClientPtr NextAvailableClient() ;
    long readyconnections;     /* mask of listeners that are ready */
    long curconn ;		    /* fd of listener that's ready */
    long newconn ;		    /* fd of new client */


    *nnew = 0;
    readyconnections = (LastSelectMask[0] & WellKnownConnections) ;
		
    while (readyconnections)
    {
	curconn = ffs (readyconnections) - 1;
	if ((newconn = accept (curconn,
			      (struct sockaddr *) NULL,
			      (int *)NULL)) >= 0)
	{
	    if (newconn >= connow_socks && !WidenTables(newconn))
	    {
	        /* もっと大きくしなければならないのにできなかった */
	        ir_debug( 
   Dmsg(1,"Didn't make connection: Out of file descriptors for connections\n");
			 )
		close (newconn);
	    }
	    else
	    {
		ClientPtr next = (ClientPtr)NULL;

		BITSET(AllClients, newconn);
		BITSET(AllSockets, newconn);

		/* クライアント管理テーブルに登録 */
		next = NextAvailableClient(newconn);

		if (next != (ClientPtr)NULL)
		{
		   newClients[(*nnew)++] = next;
		   ConnectionTranslation[newconn] = next;
		}
		else
		{
		    /* クライアント管理テーブルに登録できなかった */
		    BITCLEAR(AllClients, newconn);
		    BITCLEAR(AllSockets, newconn);
		    PrintMsg
		      ("クライアント管理テーブルが作成出来ませんでした\n");
		    close( newconn );
		}
	    }
	}
	readyconnections &= ~(1 << curconn);
    }
}


/*****************
 * CheckConections
 *    Some connection has died, go find which one and shut it down
 *    The file descriptor has been closed, but is still in AllClients.
 *    If would truly be wonderful if select() would put the bogus
 *    file descriptors in the exception mask, but nooooo.  So we have
 *    to check each and every socket individually.
 *****************/

void
CheckConnections()
{
    long		*mask, *tmask;
    register int	curclient;
    int 		i;
    struct timeval	notime;
    ClientPtr		bad;
    int r;

    mask = (long *)malloc(mskcnt * sizeof(long));
    if (mask) {
      tmask = (long *)malloc(mskcnt * sizeof(long));
      if (tmask) {
	goto doCheck;
      }
    }
    goto checkDone;

  doCheck:
    notime.tv_sec = 0;
    notime.tv_usec = 0;

    COPYBITS(AllClients, mask);
    for (i=0; i<mskcnt; i++)
    {
	while (mask[i])
	{
	    curclient = ffs (mask[i]) - 1 + (i << 5);
	    CLEARBITS(tmask);
	    BITSET(tmask, curclient);
	    r = select (curclient + 1, (fd_set *)tmask,
			(fd_set *)NULL, (fd_set *)NULL, &notime);
	    if (r < 0)
	    {
	        bad = ConnectionTranslation[curclient];
		if (bad) {
		    CloseDownClient(bad);
		}
		else
		{
		    close(curclient);
		    BITCLEAR(AllSockets, curclient);
		    BITCLEAR(AllClients, curclient);
		}
	    }
	    BITCLEAR(mask, curclient);
	}
    }	
  checkDone:
    if (mask) {
      if (tmask) {
	free((char *)tmask);
      }
      free((char *)mask);
    }
}


AllCloseDownClients()
{
    register int i ;

    for( i = 0; i < connow_socks; i++ ) {
	if( ConnectionTranslation[ i ] != (ClientPtr)NULL ){
	    CloseDownClient( ConnectionTranslation[ i ] ) ;
	}
    }
}

/**********************
 * CloseDownClient
 *********************/

CloseDownClient(client)
ClientPtr client;
{
    extern void MountDic();
    register int connection ;
    register UserTblPtr usr ;
    register int i ;
    int *contexts;

   ir_debug( Dmsg(3,"クライアントID %d をクローズする\n", client->id ); )
    connection = client->id ;
    close( connection ) ;
    BITCLEAR(AllSockets, connection);
    BITCLEAR(AllClients, connection);
    ConnectionTranslation[ connection ] = (ClientPtr)NULL ;

    contexts = client->context_flag;
    for (i = 0 ; i < client->ncon ; i++) {
      RkwCloseContext(contexts[i]);
    }
    client->ncon = 0;

   ir_debug( Dmsg( 6,"ユーザナンバー:[%d]\n", client->usr_no ); )
    if( (usr = usertabl[ client->usr_no ]) != (UserTblPtr)NULL ) {
	if( usr->username && client->username && 
	    !strcmp( usr->username, client->username ) ) {
	   ir_debug( Dmsg( 6,"ユーザカウント:[%d]\n", usr->count ); )
	    usr->count-- ;
	    if( usr->count <= 0 ) {
	       ir_debug( Dmsg( 6,"ユーザ管理テーブルをフリーする\n" ); )
		if( usr->username )
		    free( (char *)usr->username ) ;
		if( usr )
		    free( (char *)usr ) ;
		usertabl[ client->usr_no ] = (UserTblPtr)NULL ;
		MountDic();
	    }
	}
    }
    if( client->hostname != err_hostname )
	free( (char *)client->hostname ) ;

    if( client->username )
	free( (char *)client->username ) ;

    free( (char *)client );

    for( i = 0; i < connow_socks ; i++ )
	if( usertabl[ i ] )
	    break ;

    if( i == connow_socks ) /* 全てのクライアントがいなくなったら */
	MountDic(); /* cannaをマウントし直す */
}

void
ClientStat(client, type, request_Type, return_date)
register ClientPtr client ;
int request_Type, type;
char *return_date ;
{
    static long set_time ; /* サーバを使用した時間を測る基点 */
    long	 cur_time ;

    cur_time = time( (long *)0 ) ;

    switch( type ) {
#ifdef DEBUG
	case GETDATE :
	    {
	    struct tm	 *tt ;
	    char *date ;
	    long cdate ;

	    cdate = client->connect_date ;
	    tt = localtime( &cdate ) ;
	    date = asctime( tt ) ;
	    date[24] = '\0' ;
	    if (return_date) {
	      strcpy(return_date, date);
	      strcat(return_date, " JST");
	    }
	    break ;
	    }
#endif
	case CONNECT :
	    client->connect_date = cur_time ;
	    client->used_time = 0 ;
	    client->idle_date = cur_time ;
	    break ;
	case SETTIME :
	    if( (request_Type == IR_SER_STAT) || (request_Type == IR_SER_STAT2) )
		return ;
	    set_time = cur_time ;
	    client->idle_date = 0 ;
	    break ;

	case GETTIME :
	    if( (request_Type == IR_SER_STAT) || (request_Type == IR_SER_STAT2) )
		return ;
	    client->idle_date = cur_time ;
	    client->used_time += (cur_time - set_time) ;
	    break ;

	default :
	    break ;
    }
}

GetConnectionInfo( client )
ClientPtr client ;
{
    struct sockaddr_in	from;
    struct hostent	*hp;
    char		buf[MAXDATA], *inet_ntoa();
    int 		fromlen = sizeof( struct sockaddr_in ) ;

    bzero( &from, fromlen ) ;
    client->hostaddr = IR_NO_ADDRESS;
    if (getpeername(client->id, (struct sockaddr *)&from, &fromlen) < 0) {
	PrintMsg( "getpeername error No.%d\n", errno );
	client->hostname = err_hostname ;
	return ;
    }

    if( from.sin_family == AF_INET ) {
        client->hostaddr = from.sin_addr.s_addr;
	hp = gethostbyaddr((char *)&from.sin_addr, sizeof( struct in_addr ),
							    from.sin_family);
	if ( hp )
	    strncpy( buf, hp->h_name, MAXDATA-1 ) ;
	else
	    strncpy( buf, inet_ntoa( from.sin_addr ), MAXDATA-1 ) ;
    } else {
        client->hostaddr = IR_UNIX_ADDRESS;
	if( gethostname( buf, MAXDATA - 6 ) < 0 ) {
	    client->hostname = err_hostname ;
	    return ;
	}
	strcat( buf, "(UNIX)" ) ;
    }

    client->hostname = malloc(strlen(buf) + 1);
    if (client->hostname)
	strcpy( client->hostname, buf ) ;
    else
	client->hostname = err_hostname ;
}

/* RkIntilizeの要求処理になるかな */
ConnectionSetup(client) 
ClientPtr client ;
{
    register int i ;
    register UserTblPtr usr ;
    char *name ;

    name = client->username ;
   ir_debug( Dmsg( 6,"ユーザ名[%s]\n", name ); )
    for( i = 0; i < connow_socks ; i++ ) {
	usr = usertabl[ i ] ;
	if( (usr != (UserTblPtr)NULL) ) {
	   ir_debug( Dmsg( 6,"登録済みユーザ名[%s]\n", usr->username ); )
	    if( !strcmp( usr->username, name) ){
		client->usr_no = i ;
		usr->count++ ;
	       ir_debug( Dmsg( 6,"ユーザカウント[%d]\n", usr->count ); )
		break ;
	    }	
	}
    }

    if( i == connow_socks ){
	for( i = 0; i < connow_socks; i++){
	    usr = usertabl[ i ] ;
	    if( usr == (UserTblPtr)NULL ){
		usr = (UserTblPtr)malloc( sizeof( UsrTabl ) ) ;
		if( usr == (UserTblPtr)NULL )
		    return( -1 ) ;
		bzero( (char *)usr, sizeof( UsrTabl ) ) ;
		usr->username = malloc(strlen(name) + 1);
		if (usr->username)
		    strcpy( usr->username, name ) ;
		client->usr_no = i ;
		usr->count = 1 ;
	       ir_debug( Dmsg( 6,"ユーザ[%s]を新規登録する\n", name ); )
	       ir_debug( Dmsg( 6,"ユーザナンバー[%d]\n", i ); )
		usertabl[ i ] = usr ;
		break ;
	    }
	}

	if( i == connow_socks) {
	    /* ユーザ管理テーブルが一杯 */
	    /* であるからエラー処理を入れましょう */
	   ir_debug( Dmsg( 1, "これ以上ユーザは、登録できません\n"); )
	    close( client->id ) ;
	    return( -1 ) ;
	}
    }
    return( 1 ) ;
}
