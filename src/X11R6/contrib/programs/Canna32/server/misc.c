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
static char rcs_id[]="@(#) $Id: misc.c,v 5.16 1994/03/11 03:52:23 kon Exp $";
#endif

/* LINTLIBRARY */

#include <stdio.h>
#include <errno.h>

#ifdef USE_VARARGS
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#endif

#ifdef pcux
#include <sys/fcntl.h>
#else
#include <fcntl.h>
#endif
#include <signal.h>
#include <sys/ioctl.h>
#include "IR.h"
#include "net.h"

#ifndef LOCKDIR
#define LOCKDIR     "/usr/spool/canna/lock"
#endif

#ifndef DICHOME
#define DICHOME     "/usr/lib/canna/dic"
#endif

#ifndef ERRDIR
#define ERRDIR      "/usr/spool/canna"
#endif

#ifndef LOCKFILE
#define LOCKFILE    ".CANNALOCK"
#endif

#define ERRFILE     "CANNA"
#define ERRFILE2    "msgs"
#define ERRSIZE     64
#define MOUNT_FILE  "premountdics"
#define ACCESS_FILE "/etc/hosts.canna"

extern void CheckConnections();

void
FatalError() ;

extern int  errno;

#ifdef DEBUG
#define LOGFILE "/tmp/canna.log"
static FILE *ServerLogFp = (FILE *)0 ;
static FILE *Fp ;
static int DebugMode = 0 ;
static int LogLevel = 0 ;
#endif

int PortNumberPlus = 0 ;
int MMountFlag = 0 ; /* メモリに辞書をロードするかしないかのフラグ */
static char *LockFile[ 64 ] ;
static char Name[64] ;
static FILE *lockfp ;

#define MAX_PREMOUNTS 20

char *PreMountTabl[MAX_PREMOUNTS] ;
int npremounts = 0;
static char *MyName ;
static unsigned long MyAddr = 0;

ACLPtr ACLHead = (ACLPtr)NULL ;

static void Reset() ;
static void parQUIT();
void MountDic() ;

char anotherServerFormat[] = "\
ERROR:\n\
\tAnother 'cannaserver' is detected.\n\
\tIf 'cannaserver' is not running, lock-file may remain accidentally.\n\
\tSo, after making sure that 'cannaserver' is not running,\n\
\tPlease execute following command.\n\n\
\trm %s\n";

#define USAGE "Usage: cannaserver [-p num] [-l num] [-d] [dichome]"
static void
Usage()
{
  FatalError(USAGE) ;
}

void
BecomeDaemon ( argc, argv )
int argc ;
char *argv[] ;	
{
    char *ddname = (char *)NULL ;
    char buf[ MAXDATA ] ;
    char    errfile[ ERRSIZE ] ;
    int     parent, parentid, i ;
    int     lockfd, errfd, context ;

    strcpy( Name, argv[ 0 ] ) ;

    for( i = 1; i < argc; i++ ) {
	if( argv[i][0] == '/' ) {
	    ddname = malloc(strlen(argv[i]) + 1);
	    if( ddname )
		strcpy( (char *)ddname, argv[ i ] ) ;
	}

	if( !strcmp( argv[i], "-p") ) {
	  if (++i < argc) {
	    PortNumberPlus = atoi( argv[i] ) ;
	  }
	  else {
	    fprintf(stderr, "%s\n", USAGE);
	    exit(2);
	    /* NOTREACHED */
	  }
	}
#ifdef RK_MMOUNT
	else if( !strcmp( argv[i], "-m") ) {
	  MMountFlag = RK_MMOUNT;
	}
#endif
    }

    if( !ddname ) {
	ddname = malloc(strlen(DICHOME) + 1);
	if( !ddname )
	    FatalError("cannaserver:Initialize failed\n") ;
	strcpy( (char *)ddname, DICHOME ) ;
    }

/* 同一マシン上で複数のサーバが起動しないようにロックファイルを作る */
    if ( mkdir( LOCKDIR, 0777 ) == -1 &&
	errno != EEXIST ) {
	(void)fprintf(stderr, "Can't open ");
	(void)fprintf(stderr, LOCKDIR) ;
	(void)fprintf(stderr, ", error No. %d\n", errno);
    }
    if( PortNumberPlus )
	sprintf( (char *)LockFile, "%s/%s:%d",
		LOCKDIR, LOCKFILE, PortNumberPlus ) ;
    else
	sprintf( (char *)LockFile, "%s/%s",
		LOCKDIR, LOCKFILE ) ;
    if( (lockfd = open( (char *)LockFile, O_CREAT|O_RDWR|O_EXCL, 00644 )) < 0){
	if( errno == EEXIST ) {
	    fprintf(stderr, anotherServerFormat, LockFile);
	} else {
	    fprintf(stderr,"%s:Can't create lock file(%s)\n", Name, LockFile) ;
	}
	exit( 1 );
    }
    close( lockfd ) ;

#ifdef DEBUG
    DebugMode = 0 ;
    ServerLogFp = stderr ;
		
    for( i = 1; i < argc; i++ ) {
	if( !strcmp( argv[ i ], "-d" )) {
	    DebugMode = 1 ;
	    LogLevel = 5 ;
	}
	
	if( !strcmp( argv[ i ], "-l" ) ) {
	  if (++i < argc) {
	    /* ログファイル作成 */
	    if( (Fp = fopen( LOGFILE, "w" ) ) != NULL ){
		LogLevel = atoi(argv[i]);
		if( LogLevel <= 0 )
		    LogLevel = 1 ;
		ServerLogFp = Fp ;
	    } else {
		perror("Can't Create Log File!!\n");
	    }
	  }
	  else {
	    Usage();
	    /* NOTREACHED */
	  }
	}
    }

#endif /* DEBUG */

    getserver_version() ;

   ir_debug( Dmsg(5, "辞書ホームディレクトリィ = %s\n", ddname ); )

    if ((context = RkwInitialize( (char *)ddname )) < 0)
	FatalError("cannaserver:Initialize failed\n") ;
    free( (char *)ddname ) ;
    RkwCloseContext( context ) ;

    if (gethostname( buf, MAXDATA ) == 0) {
      MyName = malloc(strlen(buf) + 1);
      if (MyName) {
	strcpy(MyName, buf);
      }
    }

   ir_debug( Dmsg(5, "My name is %s\n", MyName ); )

#ifdef DEBUG
    if( DebugMode ) {
	signal(SIGPIPE,  SIG_IGN) ;
	bzero(PreMountTabl, MAX_PREMOUNTS * sizeof(unsigned char *));
	MountDic();
	CreateAccessControlList() ;

	return ; /* デーモンにならない */
    }
#endif
    /*
     * FORK a CHILD
     */

    parentid = getpid() ;

    /* 標準エラー出力をエラーファイルに切り替えて、標準入出力をクローズする */
    bzero( errfile, ERRSIZE ) ;
    sprintf( errfile,"%s/%s%d%s", ERRDIR, ERRFILE, PortNumberPlus, ERRFILE2 ) ;

    if( ( errfd = open( errfile, O_CREAT | O_RDWR | O_TRUNC, 00644 ) ) < 0 ) {
	(void)fprintf(stderr, "Warning: %s: %s open faild\n", Name, errfile );
	(void)perror("");
    } else {
	if( dup2( errfd, fileno( stderr ) ) < 0 ) {
	    (void)fprintf(stderr, "Warning: %s: %s dup2 faild\n", Name, errfile );
	    (void)perror("") ;
	    close( fileno( stderr ) ) ;
	}
    }
    close( fileno( stdin ) ) ;
    close( fileno( stdout ) ) ;
    close( errfd ) ;

    bzero(PreMountTabl, MAX_PREMOUNTS * sizeof(unsigned char *));
    MountDic();

    CreateAccessControlList() ;

    signal(SIGQUIT, parQUIT);

    if ((parent = fork()) == -1) {
	PrintMsg( "Fork faild\n" );
	exit( 1 ) ;
    }
    if ( parent ) {

	pause() ;
	exit( 0 ) ;
	/* wait( (int *)0 ) ;	*/
    }

    if( (lockfp = fopen( (char *)LockFile, "w" )) == (FILE *)NULL )
	exit( 2 ) ;
    fprintf( lockfp ,"%d\n", getpid() ) ;
    fflush( lockfp ) ;
    fclose( lockfp ) ;

    /*
     * TTY の切り離し
     */
#if defined(SVR4) || defined(__convex__) || defined(__BSD_NET2__) || defined(__BSD44__)
    (void)setsid();
#else
#if defined(SYSV) || defined(linux) || defined(__OSF__)
    setpgrp();
#else
    setpgrp(0, getpid());
#endif
#endif

#ifdef TIOCNOTTY
    {
      int fd = open("/dev/tty", O_RDWR, 0);
      if (fd >= 0) {
	(void)ioctl(fd, TIOCNOTTY, (char *)0);
	(void)close(fd);
      }
    }
#endif

    /*
     * シグナル処理
     */
    signal(SIGHUP,   SIG_IGN);
    signal(SIGINT,   SIG_IGN);
    signal(SIGQUIT,  SIG_IGN);
    signal(SIGALRM,  SIG_IGN);
    signal(SIGTRAP,  Reset);
    signal(SIGILL,   Reset);
    signal(SIGPIPE,  SIG_IGN) ;
#ifdef pcux
    signal(SIGABRT,  Reset);
#endif
    signal(SIGTERM,  Reset);
#ifdef SIGEMT
    signal(SIGEMT,   Reset);
#endif
#ifdef SIGBUS
    signal(SIGBUS,   Reset);
#endif
    signal(SIGSEGV,  Reset);

    umask( 002 ) ;

    kill( parentid, SIGQUIT )  ;
}

static RemoveLockFile();

static void
CloseServer()
{
    RkwFinalize() ;
    AllCloseDownClients() ;
    RemoveLockFile() ;
}

void
FatalError(f)
    char *f;
{
    fprintf(stderr,"%s\n", f);
    CloseServer() ;
    exit(2);
    /*NOTREACHED*/
}

#define MAXARGS 10

#ifdef DEBUG

#ifndef USE_VARARGS

/* VARARGS */
Dmsg( Pri, f, s0, s1, s2, s3, s4, s5, s6, s7, s8 )
int Pri ;
char *f;
char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8 ;
{
    if (!ServerLogFp)
	ServerLogFp = stderr;
    if ( LogLevel >= Pri ) {
	fprintf(ServerLogFp , f, s0, s1, s2, s3, s4, s5, s6, s7, s8 );
	fflush( ServerLogFp ) ;
    }
}

#else /* USE_VARARGS */

#if __STDC__
Dmsg(Pri, f, ...)
int Pri;
char *f;
#else
Dmsg(va_alist)
va_dcl
#endif
{
  va_list ap;
  char *args[MAXARGS];
  int argno = 0;

#if !__STDC__
  int Pri;
  char *f;
#endif

  va_start(ap);

#if !__STDC__
  Pri = va_arg(ap, int);
  f = va_arg(ap, char *);
#endif

  while (++argno < MAXARGS && (args[argno] = va_arg(ap, char *)))
    ;
  args[MAXARGS - 1] = (char *)0;
  va_end(ap);

  if (!ServerLogFp) {
    ServerLogFp = stderr;
  }
  if (LogLevel >= Pri) {
    fprintf(ServerLogFp, f, args[0], args[1], args[2], args[3], args[4],
	    args[5], args[6], args[7], args[8]);
    fflush(ServerLogFp);
  }
}
#endif /* USE_VARARGS */
#endif

#ifndef USE_VARARGS
PrintMsg( f, s0, s1, s2, s3, s4, s5, s6, s7, s8 )
char *f;
char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8 ;
{
    long    Time ;
    char    *date ;

    Time = time( (long *)0 ) ;
    date = (char *)ctime( &Time ) ;
    date[24] = '\0' ;
    fprintf( stderr, "%s :", date ) ;
    fprintf( stderr, f, s0, s1, s2, s3, s4, s5, s6, s7, s8 );
    fflush( stderr ) ;
}
#else /* USE_VARARGS */
#if __STDC__
PrintMsg(f, ...)
char *f;
#else
PrintMsg(va_alist)
va_dcl
#endif
{
  va_list ap;
  char *args[MAXARGS];
  int argno = 0;
  long    Time;
  char    *date;
#if !__STDC__
  char *f;
#endif

  va_start(ap);

#if !__STDC__
  f = va_arg(ap, char *);
#endif

  while (++argno < MAXARGS && (args[argno] = va_arg(ap, char *)))
    ;
  args[MAXARGS - 1] = (char *)0;
  va_end(ap);

  Time = time((long *)0);
  date = (char *)ctime(&Time);
  date[24] = '\0';
  fprintf(stderr, "%s :", date);
  fprintf(stderr, f, args[0], args[1], args[2], args[3], args[4],
	  args[5], args[6], args[7], args[8]);
  fflush( stderr ) ;
}
#endif /* USE_VARARGS */

static
RemoveLockFile()
{
    if( unlink( LockFile ) < 0 )				
	PrintMsg( "Remove lockfile faild\n" ) ;
}

static void
Reset(sig)
int	sig;
{
    if( sig == SIGTERM ) {
	PrintMsg( "Cannaserver Terminated\n" ) ;
	CloseServer() ;
    } else {
	PrintMsg( "Caught a signal(%d)\n", sig ) ;
	RemoveLockFile() ;
    }
    exit(2);
}

static void
parQUIT(sig)
int    sig;
/* ARGSUSED */
{
    exit( 0 ) ;
  /* 何もしない */
}

/*
 * クライアントのマウント時間を短縮するためにあらかじめ
 * 指定された辞書をマウントしておく。
 */
void
MountDic()
{
    register int context, index = 0, good = 0;
    unsigned char   buf[ BUFSIZE ], *wp ;
    char *mesg = (char *)0;
    FILE *fp ;

    sprintf((char *)buf, "%s/%s", DICHOME, MOUNT_FILE);
    if( (fp = fopen( (char *)buf, "r" )) == (FILE *)NULL ) {
/*	PrintMsg( "Can't open %s\n", buf ) ;	*/
	return ;
    }

    while( fgets( (char *)buf, BUFSIZE, fp ) != (char *)NULL ) {
	buf[ strlen( (char *)buf )-1 ] = '\0' ;
	wp = buf ;
	if ((char)wp[0] == '#' || !strtok((char *)wp, ":"))
	    continue ;

	/* 辞書ホームディレクトリにあるディレクトリをDDPATHに設定する。*/
	context = RkwCreateContext();
	mesg = "Can't create context\n";
	if (context >= 0) {
	  mesg = "Can't set dictionary path\n";
	  if (RkwSetDicPath(context, (char *)wp) >= 0) {
	    mesg = "Too many premount dics\n";
	    if (index < MAX_PREMOUNTS) {
	      PreMountTabl[index] = malloc(strlen((char *)wp) + 1);
	      mesg = "No More Memory\n";
	      if (PreMountTabl[index]) {
		strcpy(PreMountTabl[index++], (char *)wp);
		ir_debug( Dmsg(5,"dicpath=%s\n", wp ); )
		ir_debug( Dmsg(5,"Mount Dic File is"); )
		good = 1;
	      }
	    }
	  }
	}

	if (good) {
	  while((wp = (unsigned char *)strtok((char *)NULL, ",")) !=
		(unsigned char *)NULL) {
	    RkwUnmountDic(context, (char *)wp); /* マウントし直す ? */
	    if (RkwMountDic( context, (char *)wp, MMountFlag) < 0) {
	      PrintMsg("Can't mount [%s] dictionary\n", wp);
	    }
	    ir_debug( Dmsg(5,"[ %s ]", wp ); )
	  }
	  ir_debug( Dmsg(5,"\n" ); )
	}
	else {
	  break;
	}
    }
    if (!good && mesg) {
      PrintMsg(mesg);
    }

    fclose( fp ) ;
}

static
ACLCheckHostName( currentptr )
ACLPtr	currentptr ;
{
    char *hostname = currentptr->hostname ;
    ACLPtr  wp ;

    for( wp = ACLHead; wp != (ACLPtr)NULL; wp = wp->next ) {
	if( (!strcmp( (char *)wp->hostname, (char *)hostname )) ||
	   (wp->hostaddr == currentptr->hostaddr) ) {
	    return( -1 ) ;
	}
    }
    return( 0 ) ;
}

CreateAccessControlList()
{
    char   buf[BUFSIZE];
    char   *wp, *p ;
    ACLPtr  current = (ACLPtr)NULL ;
    ACLPtr  prev = (ACLPtr)NULL ;
    FILE    *fp ;
    struct hostent *hp;
    char *hostname, *name;
    int namelen;

    hp = gethostbyname(MyName);
    if (hp) {
      MyAddr = *(unsigned long *)(hp->h_addr);
    }

    if( (fp = fopen( ACCESS_FILE, "r" )) == (FILE *)NULL )
	return( -1 ) ;

    if (ACLHead) {
      FreeAccessControlList();
    }

    while( fgets( (char *)buf, BUFSIZE, fp ) != (char *)NULL ) {
	buf[ strlen( (char *)buf )-1 ] = '\0' ;
	wp = buf ;
	if( !strtok( (char *)wp, ":" ) )
	    continue ;

	if( !(current = (ACLPtr)malloc( sizeof( ACLRec ) )) ) {
	    PrintMsg("Can't create access control list!!" ) ;	
	    fclose( fp ) ;
	    FreeAccessControlList() ;
	    return( -1 ) ;
	}

	bzero( current, sizeof( ACLRec ) ) ;

	if (!strcmp(wp, (char *)MyName)) {
	  name = "unix";
	  namelen = sizeof("unix") - 1;
	}
	else {
	  name = wp;
	  namelen = strlen(wp);
	}
	current->hostname = malloc(namelen + 1);
	if (current->hostname) {
	  strcpy(current->hostname, name);
	}

	/* AccessControlListをインターネットアドレスで管理する */
	/* hosts.cannaからホスト名を求める */
        if (strcmp((char *)current->hostname, "unix")) {
	    hostname = (char *)current->hostname;
	}
	else {
	    hostname = (char *)MyName;
	}
	/* ホスト名からインターネットアドレスを求めて ACLRecに登録する  */
	if ((hp = gethostbyname(hostname)) == (struct hostent *)NULL) {
	    /* インターネットアドレス表記が間違っているので無視する */
	    /* hostsにエントリが無いことをメッセージにだした方が良いか */
	    /* も知れない */
	    if (current->hostname)
		free((char *)current->hostname);
	    free((char *)current);
	    continue;
	}
	current->hostaddr = *(unsigned long *)(hp->h_addr);
	/* 複数のアドレスが入っていることに対応していないなあ */

	if (ACLCheckHostName(current) < 0) {
	  free((char *)current->hostname);
	  free((char *)current);
	  continue;
	}

	wp += ( strlen( (char *)wp )+1 );
	
	if( strlen( (char *)wp ) ) {
	    current->usernames = malloc(strlen(wp) + 1);
	    if (current->usernames) {
	        strcpy((char *)current->usernames, wp);
		for( p = current->usernames; *p != '\0'; p++ ) {
		    if( *p == ',' ) {
			*p = '\0' ;
			current->usercnt ++ ;
		    }
		}
		current->usercnt ++ ;
	    }
	}
	if( ACLHead ) {
	    current->prev = prev ;
	    prev->next = current ;
	} else {
	    ACLHead = current ;
	    current->prev = (ACLPtr)NULL ;
	}
	current->next = (ACLPtr)NULL ;
	prev = current ;
    }
    if( current )
	current->next = (ACLPtr)NULL ;

    fclose( fp ) ;
    return 0;
}

FreeAccessControlList() 
{
    ACLPtr  wp, tailp = (ACLPtr)NULL;

    if( !(wp = ACLHead) )
	return ;

    for( ; wp != (ACLPtr)NULL; wp = wp->next ) {
	    if( wp->hostname )
		free( wp->hostname ) ;
	    if( wp->usernames )
		free( wp->usernames ) ;
	    tailp = wp ;
    }

    for( wp = tailp; wp != (ACLPtr)NULL; wp = wp->prev ) {
	if( wp->next )
	    free( wp->next ) ;
    }
    ACLHead = (ACLPtr)NULL ;
}

CheckAccessControlList(hostaddr, username)
unsigned long hostaddr;
char *username;
{
  int i;
  char *userp;
  ACLPtr wp;
  extern char *inet_ntoa();

  if (!ACLHead) return 0;

  ir_debug(Dmsg(5, "My name is %s\n", MyName));
  ir_debug(Dmsg(5, "Check address is %s\n", inet_ntoa(hostaddr)));

  if (!hostaddr) { /* つまり、UNIX ドメインだったれば */
    hostaddr = MyAddr;
  }

  for (wp = ACLHead ; wp ; wp = wp->next) {
    /* AccessControlListで持っているインタネットアドレスと一致する
       ものをサーチする */
    if (wp->hostaddr == hostaddr) {
      if (wp->usernames) {
	for (i = 0, userp = wp->usernames ; i < wp->usercnt ; i++) {
	  if (!strcmp(userp, username)) {
	    return 0;
	  }
	  userp += strlen(userp) + 1;
	}
	return -1;
      }
      else {
	return 0;
      }
    }
  }
  return -1;
}

NumberAccessControlList()
{
  ACLPtr wp;
  int n;

  for (wp = ACLHead, n = 0; wp ; wp = wp->next) {
    n++;
  }
  return n;
}

SetDicHome( client, cxnum )
ClientPtr client ;
int cxnum ;
{
    char dichome[ 256 ] ;

    if (cxnum < 0)
	return( -1 ) ;

    if (client->username && client->username[0]) {
      if (client->groupname && client->groupname[0]) {
	sprintf(dichome, "%s/%s:%s/%s:%s",
		DDUSER, client->username,
		DDGROUP, client->groupname,
		DDPATH);
      }
      else {
	sprintf(dichome, "%s/%s:%s",
		DDUSER, client->username,
		DDPATH);
      }
    }
    else {
      strcpy(dichome, DDPATH);
    }

   ir_debug( Dmsg(5,"辞書ホームディレクトリィ：%s\n", dichome ); )
    if( RkwSetDicPath( cxnum, dichome ) == -1 ) {
	return( -1 ) ;
    }
    return( 1 ) ;
}

ConnectClientCount( client, buf, new_socks )
ClientPtr   client ;
ClientRec   *buf[] ;
unsigned long new_socks ;
{
    extern ClientPtr	   *ConnectionTranslation ;
    register ClientPtr	    who ;
    int 		    i, count ;

    bzero((char *)buf, sizeof(ClientPtr) * new_socks);
    for (i = 0, count = 0 ; i < new_socks ; i++) {
	if( ((who = ConnectionTranslation[ i ]) != (ClientPtr)NULL)
						&& ( who != client ) ) {
	    *buf = who ;
	    buf ++ ;
	    count ++ ;
	}
    }
    return( count ) ;
}
