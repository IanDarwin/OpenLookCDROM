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
static char rcs_id[] = "$Id: main.c,v 2.6 1994/03/08 14:44:18 kon Exp $";
#endif

/* LINTLIBRARY */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <signal.h>
	
#include "IR.h"

#define MaxClients MAXSOCKS

ClientPtr
NextAvailableClient() ; 

ClientPtr *ConnectionTranslation, *clientReady, *newClients;
UserTblPtr *usertabl;
unsigned long connow_socks = 0, mskcnt = 0;

long start_real_time, start_user_time, start_sys_time ; 
extern void BecomeDaemon(), CreateWellKnownSockets(), ClientStat();

main(argc, argv)
int argc ;			
char *argv[] ;
{
    /* サーバを子プロセス(デーモン)として起動する */
    BecomeDaemon ( argc, argv ) ;

    /* コンテクスト数の malloc 処理 */
    if(!WidenTables(INITSOCKS)){
      PrintMsg("十分なメモリがありません。mallocに失敗しました。\n"); 
      exit(1);
    }

    /* コネクションの準備 */
    CreateWellKnownSockets() ;

    /* ディスパッチループ */
    Dispatch() ;
}


Dispatch()
{
    extern int (* CallFunc)() ;
#ifdef DEBUG
    extern char *DebugProc[][2] ;
    extern char *DebugProcWide[][2] ;
#endif
    int 		result;
    int 		request;
    ClientPtr		client;
    int 		nready, nnew;

    request = 0 ;
    while (1)
    {
       ir_debug( Dmsg(3,"WaitForSomethingの前 "); )
	WaitForSomething(&nready, &nnew);
       ir_debug( Dmsg(3,"後\n"); )
	/*****************
	 *  Establish any new connections
	 *****************/

	while (nnew--)
	{
	    client = newClients[nnew];
	    client->version_hi = (short)0 ;
	/*  ConnectionSetup(client);	*/
	}

       /*****************
	*  Handle events in round robin fashion, doing input between
	*  each round
	*****************/

	while ((nready--) > 0)
	{
	    client = clientReady[nready];
	    if ( !client )
	    {
		/* KillClient can cause this to happen */
		continue;
	    }

	    if( client->version_hi > 1 )
		request = ReadWideRequestFromClient( client, &result ) ;
	    else
		request = ReadRequestFromClient( client, &result ) ;
				
	    if (result < 0)
	    {
		CloseDownClient(client);
		client = (ClientPtr)0;
		break;
	    }
	    else if (result == 0)
	    {
		continue;
	    }

	     /* 実際のプロトコルに応じた処理（関数を呼ぶ） */

#ifdef DEBUG
	    if( client->version_hi > 1 )
		Dmsg( 3,"Now Call %s\n", DebugProcWide[ request ][ 0 ] );
	    else
		if( request < EXTBASEPROTONO )
		    Dmsg( 3,"Now Call %s\n", DebugProc[ request ][ 0 ] );
#endif
	    if( (* CallFunc)( &client ) < 0 ){
	       ir_debug( Dmsg(3,"クライアントとの通信に失敗した\n"); )
	       CloseDownClient( client ) ;
	       client = (ClientPtr)0;
	    }
	    /* クライアントの累積サーバ使用時間を設定する */
	    if (client) {
	      ClientStat(client, GETTIME, request, 0);
	    }
	}	
    }
}

/************************
 * ClientPtr NextAvailableClientID()
 *
 * OS depedent portion can't assign client id's because of CloseDownModes.
 * Returns NULL if the there are no free clients.
 *************************/

ClientPtr
NextAvailableClient( socket )
int socket ;
{
    register ClientPtr client = (ClientPtr)malloc( sizeof( ClientRec ) ) ;

    if( client == (ClientPtr)NULL )
	return( (ClientPtr)NULL ) ;

    /* クライアント構造体のゼロクリア */
    bzero( (char *)client, sizeof( ClientRec ) ) ;
    client->id = socket ;

    GetConnectionInfo( client ) ;
    ClientStat(client, CONNECT, 0/*dummy*/, 0) ;

    return( client );
}

