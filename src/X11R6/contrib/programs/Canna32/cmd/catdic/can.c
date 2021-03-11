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
static char rcs[] = "@(#) 112.1 $Id: can.c,v 2.39 1994/05/17 08:44:44 hamada Exp $";
#endif

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#include <stdio.h>
#include <signal.h>
#include <canna/RK.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/types.h>
#include <grp.h>

#if defined(USG) || defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif

#if __STDC__
#define pro(x) x
#else
#define pro(x) ()
#endif

#if __STDC__ || defined(SVR4)
#include <locale.h>
#endif

#ifdef SVR4
extern  char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif

#define TRUE 1
#define FALSE 0
#define RECSZ 256
#define ERR_VALUE 1
#define BUFLEN   1024
#define	STRCMP(d, s)	strcmp((char *)(d), (char *)(s))
#define NOT_OVER_WRITE 0
#define OVER_WRITE 1

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

#if defined(USG) || defined(SYSV) || defined(SVR4) 
# ifndef index
#define   index  strchr 
# endif
# ifndef rindex
#define   rindex strrchr
# endif
#endif 

#ifdef USE_VARARGS
#if __STDC__
extern  void Message(char *,...);
#else
extern  void Message();
#endif
#else
extern  void Message();
#endif


extern	RkDeleteLine();
extern	RkDefineLine();
extern	rmDitionary();

char            init[RECSZ], *Progname;
unsigned char	*r_dic;
unsigned char   rm_dic[RECSZ];
int	cx_num, is_display, mode ,mode2;
unsigned char dicname[RECSZ];
static char *r_file;
unsigned  char dicname1[RECSZ], dicname2[RECSZ];
static int  majv, minv, protover;
static char     *hinshi ;
static char     msg_abnls[80], msg_abnl[80] , msg_sfq[80] ,msg_l[80];

#define canna_protocol_version(ma, mi) ((ma) * 1024 + (mi))
#define canna_major_version(ver) ((ver) / 1024)
#define canna_minor_version(ver) ((ver) % 1024)

static   int opt_cs ;
static   int opt_l  ;
static   int opt_r  ;
static   int opt_h  ;
static   int opt_i  ;
static   int opt_u  ;
static   int opt_s  ;
static   int opt_fq ;
static   int opt_std;
static   int opt_myg , opt_g , opt_rw , opt_v , opt_a ; 

static   char *opt_dic1 ;
static   char *opt_dic2 ;
static   char *opt_lfile ; 
static   char *opt_user , *opt_grp ; 

/*-------------------------------------------------------------*/

static int      cmd_code ; 
#define    ADD    1
#define    CAT    2
#define    CP     3
#define    DEL    4
#define    LS     5
#define    MK     6
#define    MV     7
#define    RM     8
#define    CHMOD  9
#define    SY     10

/**************************************************************/
/*                         共通関数                           */
/**************************************************************/

void
usage()
{

    switch ( cmd_code ) {

       case ADD :

    (void) fprintf(stderr, gettxt("cannacmd:53",
			  "Usage: %s [options]  remote-dic\n"),Progname);
    (void) fprintf(stderr, gettxt("cannacmd:54", " options include:\n"));
    (void) fprintf(stderr, gettxt("cannacmd:55", 
			  "\t{-cs | -cannaserver} canna-server\n"));
    (void) fprintf(stderr, gettxt("cannacmd:56", "\t-l local-file\n"));
    break ; 

       case    CAT :

    (void)fprintf(stderr,gettxt("cannacmd:71",
   			"Usage: %s [options]  remote-dic\n"), Progname);
    (void)fprintf(stderr,gettxt("cannacmd:72", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:73", 
			"        {-cs | -cannaserver} canna-server\n"));
    (void)fprintf(stderr,gettxt("cannacmd:74", "        -i | -u user-name\n"));
    (void)fprintf(stderr,gettxt("cannacmd:75", "        -l local-file\n"));
    (void)fprintf(stderr,gettxt("cannacmd:234","        -G \n"));
    (void)fprintf(stderr,gettxt("cannacmd:235","        -g group-name\n"));
    break ; 

       case    CP :

    (void)fprintf(stderr,gettxt("cannacmd:85", 
			"Usage: %s [options]  from-dic to-dic\n"), Progname);
    (void)fprintf(stderr,gettxt("cannacmd:86", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:87", 
			"        {-cs | -cannaserver} canna-server\n"));
    (void)fprintf(stderr,gettxt("cannacmd:88", "        -s\n"));
    (void)fprintf(stderr,gettxt("cannacmd:89", "        -i | -u user-name\n"));
    (void)fprintf(stderr,gettxt("cannacmd:236","        -G \n"));
    (void)fprintf(stderr,gettxt("cannacmd:237","        -g group-name\n"));
    break ; 

       case    DEL :

    (void) fprintf(stderr, gettxt("cannacmd:102", 
			  "Usage: %s [options]  remote-dic\n"),Progname);
    (void) fprintf(stderr, gettxt("cannacmd:103", " options:\n"));
    (void) fprintf(stderr, gettxt("cannacmd:104", 
			  "\t{-cs | -cannaserver} canna-server\n"));
    (void) fprintf(stderr, gettxt("cannacmd:105", "\t-l local-file\n"));
    break ; 

       case    LS :

    (void)fprintf(stderr,gettxt("cannacmd:121", 
			"Usage: %s [options]\n"), Progname);
    (void)fprintf(stderr,gettxt("cannacmd:122", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:123", 
			"        {-cs | -cannaserver} canna-server\n"));
    (void)fprintf(stderr,gettxt("cannacmd:124", "        -i\n"));
    (void)fprintf(stderr,gettxt("cannacmd:125", "        -u user-name\n"));
    (void)fprintf(stderr,gettxt("cannacmd:238", "        -G \n"));
    (void)fprintf(stderr,gettxt("cannacmd:239", "        -g group-name\n"));
    (void)fprintf(stderr,gettxt("cannacmd:240", "        -a \n"));
    (void)fprintf(stderr,gettxt("cannacmd:241", "        -l \n"));
    break ; 

       case     MK :

    (void) fprintf(stderr, gettxt("cannacmd:134", 
			  "Usage: %s [options]  remote-dic\n"),Progname);
    (void) fprintf(stderr, gettxt("cannacmd:135", " options:\n"));
    (void) fprintf(stderr, gettxt("cannacmd:136", 
			  "\t{-cs | -cannaserver} canna-server\n"));
    (void) fprintf(stderr, gettxt("cannacmd:137", "\t-s\n"));
    (void) fprintf(stderr, gettxt("cannacmd:138", "\t{- | -l local-file}\n"));
    (void) fprintf(stderr, gettxt("cannacmd:139", "\t-fq\n"));
    (void) fprintf(stderr, gettxt("cannacmd:242", "\t-G  \n"));
    break ; 

       case     MV :

    (void)fprintf(stderr,gettxt("cannacmd:155", 
			"Usage: %s [options]  from-dic to-dic\n"), Progname);
    (void)fprintf(stderr,gettxt("cannacmd:156", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:157", 
			"        {-cs | -cannaserver} canna-server\n"));
    (void)fprintf(stderr,gettxt("cannacmd:243", "        -G \n"));
    (void)fflush(stderr);
    break ; 

       case     RM :

    (void)fprintf(stderr,gettxt("cannacmd:177", 
		"Usage: %s [options]  dicname1 [dicname2...]\n"), Progname);
    (void)fprintf(stderr,gettxt("cannacmd:178", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:179", 
			"        {-cs | -cannaserver} canna-server\n"));
    (void)fprintf(stderr,gettxt("cannacmd:180", "         -fq \n"));
    (void)fprintf(stderr,gettxt("cannacmd:244", "         -G \n"));
    break ; 

       case     CHMOD :

    (void)fprintf(stderr,gettxt("cannacmd:245", 
		"Usage: %s [options]  dicname \n"), Progname);
    (void)fprintf(stderr,gettxt("cannacmd:246", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:247", 
			"        {-cs | -cannaserver} canna-server\n"));
    (void)fprintf(stderr,gettxt("cannacmd:248", 
                        "        {+|-}{r|w|rw|wr}... \n"));
    (void)fprintf(stderr,gettxt("cannacmd:249", "        -G \n"));
    break ; 

       case     SY :

    (void)fprintf(stderr,gettxt("cannacmd:999", 
		"Usage: %s [options]   \n"),Progname);
    (void)fprintf(stderr,gettxt("cannacmd:999", " options:\n"));
    (void)fprintf(stderr,gettxt("cannacmd:999", 
			"        {-cs | -cannaserver} canna-server\n"));
    break ; 

    }
    (void)fflush(stderr);
    exit(ERR_VALUE);

}

static SIGVAL
StopAll(sig)
int sig;
/* ARGSUSED */
{
#ifdef DEBUG
    Message("StopAll: Caught signal, Quit addwords and do nothing");
#endif
    (void) signal(SIGINT,  SIG_IGN);
    (void) signal(SIGQUIT, SIG_IGN);
    (void) signal(SIGTERM, SIG_IGN);
    /*
     * Shutting down and close connection with server.
     */
    (void) RkFinalize();
#ifdef DEBUG
    Message("do nothing to dictionary.");
#endif
    fprintf(stderr,"\n");
    Message(gettxt("cannacmd:232", "Process was intrrupted."));
    exit(ERR_VALUE);
}

static SIGVAL
RefreshAll(sig)
int sig;
/* ARGSUSED */
{
#ifdef DEBUG
    Message("RefreshAll: Caught signal, Shutting down addwords.");
#endif
    (void) signal(SIGINT,  SIG_IGN);
    (void) signal(SIGQUIT, SIG_IGN);
    (void) signal(SIGTERM, SIG_IGN);

    if (cmd_code == CAT ) {
	if (STRCMP(r_file,"")) {
	    unlink(r_file);
	}
    }
    /*
     * Shutting down and close old connection with server.
     */
    (void) RkUnmountDic(cx_num, (char *)r_dic);
    (void) RkFinalize();

    /*
     * Restart new connection with server and add dic.
     */
    cx_num = RkInitialize(init);
    if (cx_num == 0) {
	if (cmd_code == CP || cmd_code == MK) {
	    if (mode & KYOUSEI) {
#ifdef DEBUG
		Message("RefreshAll: Restore dictionary \"%s\".", r_dic);
#endif
		(void) RkCreateDic(cx_num, r_dic, mode);
	    } else {
#ifdef DEBUG
		Message("RefreshAll: Remove dictionary \"%s\"", r_dic);
#endif
		(void) rmDictionary(cx_num, r_dic , mode );
	    }
	}
	(void) RkFinalize();
    }
    
    Message(gettxt("cannacmd:232", "Process was intrrupted."));
    exit(ERR_VALUE);
}


int
rk_init()
{
    if ((cx_num = RkInitialize(init)) < 0 ) {
	if (init[0] != '/') {
	    (void)fprintf(stderr,gettxt("cannacmd:67", 
		"Cannot connect with cannaserver \"%s\".\n"), init);
	}
	else {
	    (void)fprintf(stderr, gettxt("cannacmd:68", 
		 "Cannot connect with cannaserver.\n"));
	}
	exit(ERR_VALUE);
    }
    return(0);
}

int 
nwcheck()
{
    int   bak ; 
    RkGetProtocolVersion(&majv, &minv);
    protover = canna_protocol_version(majv, minv);
    bak = RkGetServerVersion(&majv, &minv);
    if ( bak < 0 ) { /* サーバの状態が異常 */
	if (init[0] != '/') {
	    (void)fprintf(stderr,gettxt("cannacmd:80", 
		   "Cannaserver \"%s\" is in an abnormal state.\n"), init);
	}
	else {
	    (void)fprintf(stderr, 
	      gettxt("cannacmd:81", "Cannaserver is in an abnormal state.\n"));
	}
	RkFinalize();
	exit(ERR_VALUE);
    }
    if ( majv < 2 && minv < 2 ) { /* irohaserver && R7.1より前 */
	    if (init[0] != '/') {
		(void)fprintf(stderr, gettxt("cannacmd:82", 
	     "Cannaserver \"%s\" does not support dictionary maintenance.\n")
		      , init);
	    }
	    else {
	    (void)fprintf(stderr, gettxt("cannacmd:83", 
	   "Cannaserver does not support dictionary maintenance.\n"));
	    }
	    RkFinalize();
	    exit(ERR_VALUE);
    }
    return(0);
}

static int
ParseFile(fp)
  FILE	*fp;
{
    char line[BUFLEN], *whinp ; 
    int ret = -1 ; 
    int werr , i , hflg , hlen , lineno ;

    hflg = 0 ;
    werr = 0 ; 
    lineno = 0 ; 
    if (is_display != TRUE) {
	(void) fprintf(stderr, "\n");
    }
    if ( hinshi != NULL ) {
      hflg = 1 ; 
      hlen = strlen(hinshi);
    }
    while (fgets((char *)line, sizeof(line), fp)) {
        if (line[strlen(line) - 1] == '\n') 
	  line[strlen(line)-1] = '\0';
	lineno++ ; 
	if ( hflg ) {     /* 92.12.21 */
	    whinp = index(line,'#') ; 
	    if (whinp == NULL) continue ;
	    if (strncmp(whinp,hinshi,hlen) != 0 || 
	        (whinp[hlen] != ' ' && whinp[hlen] != '\t' && 
		 whinp[hlen] != '*' ) ) continue ;
	}

	if (strlen(line) == (BUFLEN-1) ) {
	    werr = 1; 
	}
	else {	
	  if (cmd_code == DEL) {
	    i = RkDeleteLine(cx_num, r_dic, line) ;
	  }
	  else {
	    i = RkDefineLine(cx_num, r_dic, line) ;
	  }
	  if (i == -1) {
	    Message(gettxt("cannacmd:59", "write error \"%s\""), line);
	  }
	  else if (i == -2) {
	    Message(gettxt("cannacmd:60",
		   "Nomination length is too long. \"%s\""),line);
	  }
	  else if (i == -3) {
	    Message(gettxt("cannacmd:61",
		   "Reading or nomination length is too long.\"%s\""), line);
	  }
	  if ((is_display == TRUE) &&
	     ((++ret & 0x0f) == 0)) {
	     (void) fputs(".", stderr);
	  }
        }
    }
    Message("");
    if (werr == 1) {
	Message(gettxt("cannacmd:233", "too long line at %d"),lineno);
    }                                  /* message add  93.03.18 */
    RkSync(cx_num, (char *)r_dic);
    return (0);
}

void 
dicname_chk(dic)
char  *dic ; 
{
    if ((unsigned char *)index(dic, '-' )) {
	Message(gettxt("cannacmd:64",
	       "Cannot use character \"-\" for dictionary name."));
	exit(ERR_VALUE);
    }
    if ((int)strlen((char *)dic) >= RECSZ) {
	Message(gettxt("cannacmd:65", 
	       "Dictionary name \"%s\" is too long."), dic);
	exit(ERR_VALUE);
    }
}

/*  グループ名 検索   */
char *
searchgroup()
{
    char *groupname = NULL ;
    
    struct passwd *pass = getpwuid( getuid() );
    if ( pass ) {
	struct group *grp = getgrgid(pass -> pw_gid);
	if ( grp ) {
	    groupname = grp -> gr_name;
	}
    }
    if (groupname == NULL) {
	(void)fprintf(stderr,gettxt("cannacmd:250","invalid group name\n"));
	exit(ERR_VALUE);
    }
    return(groupname);
}


/*
 * ユーザ名検索 帰り値=名前へのポインタ
 */
static char *
searchuname()
{
    char *username = NULL, *getenv(), *getlogin() ;
    
    if ( (username = getenv( "LOGNAME" )) == NULL ) {
	if( (username = getenv( "USER" )) == NULL ) {
	    if( (username = getlogin()) == NULL ) {
		struct passwd *pass = getpwuid( getuid() ) ;
		if( pass )
		  username = pass->pw_name ;
	    }
	}
    }
    if ( username == NULL ) {
	(void)fprintf(stderr,gettxt("cannacmd:251","invalid user name\n"));
	exit(ERR_VALUE);
    }
    return( username ) ;
}


/* addwords delwords で辞書に write権があるかをチェックする */
write_chk()
{
    int mode ,ret;
    mode = 0 ;
 grp:
    ret = RkChmodDic(cx_num,opt_dic1,mode) ;
    if (ret < 0) { 
	switch (ret) {
          case NOENT:
	            /* ユーザ辞書になければグループ辞書を試してみる*/
	    if (mode == 0) {   
		mode = RK_GRP_DIC ;
		goto grp ;
	    }
	    fprintf(stderr,gettxt("cannacmd:169", 
		  "Dictionary \"%s\" does not exist.\n"), opt_dic1);
	    break;
          case BADCONT:
	    fprintf(stderr,gettxt(
		  "cannacmd:176","Illegal context value was used.\n"));
	    break;
          case ACCES:
	    fprintf(stderr,gettxt(
		  "cannacmd:171", "Cannot access to dictionary.\n"));
	    break;
          case NOTALC:
	    fprintf(stderr, gettxt("cannacmd:168", "No more memory.\n"));
	    break;
          case -1:
	    return;   /* 旧サーバはチェックせずOK */
          default:
	    fprintf(stderr, gettxt("cannacmd:252",
		   "invalid return code rkchmoddic  code=%d \n"),ret);
	    break;
	}
	RkFinalize();
	exit(ERR_VALUE);
    }
    if (( ret & RK_ENABLE_WRITE)  == RK_ENABLE_WRITE ) {
	return;
    }
    fprintf(stderr,gettxt(
		  "cannacmd:171", "Cannot access to dictionary.\n"));
    RkFinalize();
    exit(ERR_VALUE);
}

/**************************************************************/
/*                        addwords                            */
/**************************************************************/

static int
Addwords(fp)
  FILE	*fp;
{
    int  ret ; 
    if (RkMountDic(cx_num, (char *)r_dic, 0) < 0 ) {
	(void) Message(gettxt("cannacmd:63", 
		      "Cannot mount dictionary \"%s\"."), r_dic);
	return (-1);
    }
    ret = ParseFile(fp);
    RkUnmountDic(cx_num, (char *)r_dic);
    return (ret);
}

add_main (argc,argv)
int   argc  ;
char  **argv;
{
    FILE	*fp;
    char	*l_file = NULL;

    mode = Rk_MWD;
    is_display = FALSE;
    hinshi = NULL ;

    scan_opt(argc,argv,&argv);

    if (opt_i || opt_u || opt_s || opt_fq||opt_std||opt_myg||opt_g) usage();
    if (opt_dic2 != NULL) usage();

    if (opt_l)  l_file = opt_lfile ; 
    r_dic = (unsigned char *)opt_dic1 ; 

    if (isatty(fileno(stdout)) == 0) {
      is_display = TRUE;
    }
    if (!l_file) {
	fp = stdin;
    } else {
        is_display = TRUE ; 
	fp = (FILE *)fopen(l_file, "r");
	if (!fp) {
	    Message(gettxt("cannacmd:66",
		   "%s: cannot open \"%s\""), Progname, l_file);
	    exit(ERR_VALUE);
	}
    }

    rk_init() ;
    write_chk();

    if (Addwords(fp) == -1) {
	if (l_file)
	    (void) fclose(fp);
	(void) RkFinalize();
	exit(ERR_VALUE);
    }
    (void) RkFinalize();
    Message(gettxt("cannacmd:69", "Addwords has done on \"%s\"."), r_dic);
    exit (0);
}


/**************************************************************/
/*                        catdic                              */
/**************************************************************/

cat_main(argc,argv)
int   argc  ;
char  **argv;
{
    FILE *fopen(), *fp = stdout;
    unsigned char dirname[RECSZ*2];  /* ユーザ名または"iroha"またはNULL*/
    unsigned char filename[RECSZ*2]; /* ファイル名またはNULL */
    static int  i , errflg ;
    unsigned char buf[BUFLEN],dicname_bk[RECSZ];  /* 92.12.15 */
    unsigned char *dirnamep;
    int dirname_offset = 0;

    hinshi = NULL ;
    if (argc < 2) usage();

    (void)strcpy((char *)dirname, "");
    (void)strcpy((char *)dicname, "");
    (void)strcpy((char *)filename, "");

    scan_opt(argc,argv,&argc);

    if ( opt_s || opt_fq || opt_std ) usage();

    if (opt_u)  {
      if (opt_i|opt_g|opt_myg) usage();
      (void)strcpy((char *)dirname, ":user/");
      dirname_offset = strlen((char *)dirname);
      (void)strcpy((char *)dirname + dirname_offset, opt_user);
    }
    if ( opt_g ) {
	if (opt_i|opt_u|opt_myg) usage();
	(void)strcpy((char *)dirname, ":group/");
	(void)strcat((char *)dirname, opt_grp);
    }
    if ( opt_myg ) {
	if (opt_i|opt_u|opt_g) usage();
	(void)strcpy((char *)dirname, ":group/");
	(void)strcat((char *)dirname, searchgroup());
    }
    if (opt_l) (void)strcpy((char *)filename,opt_lfile);
    (void)strcpy((char *)dicname,opt_dic1);
    r_dic = (unsigned char *)opt_dic1 ;     /* 93.03.01 */
    r_file = (char *)filename;

    /* まずInitializeして */
    rk_init() ;

    /*  server  new/old check  */
    nwcheck() ;

    if ( opt_i ) {
	if ( majv == 1 ) {
	    (void)strcpy((char *)dirname,"iroha");
	}
	else {
	    if (protover > canna_protocol_version(3, 1)) {
		(void)strcpy((char *)dirname,":canna");
	    }
	    else {
		(void)strcpy((char *)dirname,"canna");
	    }
	}
	dirname_offset = 0;
    }
    if (protover > canna_protocol_version(3, 1)) {
      /* ※注: 実は protocol version 3.1 は欠番 */
      dirname_offset = 0;
      if ( dirname[0] == '\0' ) {   /* オプションなしでもdirは設定する */
	  (void)strcpy((char *)dirname,":user/");
	  (void)strcat((char *)dirname,searchuname());
      }
    }
    else {
	if (opt_g || opt_myg ) {
	    fprintf(stderr, gettxt("cannacmd:253",
	   "This options or command are not supported by canna-server\n"));
	    exit(ERR_VALUE);
	}
    }
    dirnamep = dirname + dirname_offset;

    /* 92.12.15 */
    if( filename[0] != '\0' ) { /* ファイル名が指定されなければ標準出力 */
	for ( i = 1; i < argc ; i++) {
	    strncpy((char *)dicname,(char *)argv[i],RECSZ-1);
	    strcpy((char *)dicname_bk, (char *)dicname);
	    if (RkGetWordTextDic(cx_num,dirnamep,dicname_bk,buf,BUFLEN) >= 0) {
		if((fp = fopen((char *)filename,"w")) == NULL) {
		    (void)fprintf(stderr,gettxt("cannacmd:77",
			"Specified file \"%s\" cannot open.\n"),filename);
		    exit(ERR_VALUE);
		}
		else {
		    break  ; 
		}
	    }
	}
    }  

    (void) signal(SIGTERM, RefreshAll);
    (void) signal(SIGINT, RefreshAll);


    errflg = 0 ; 
    for ( i = 1 ; i < argc ; i++) {
	strncpy((char *)dicname,(char *)argv[i],RECSZ-1);
	/* 辞書に書いて */
	if(DownLoadDic(fp, dirnamep) < 0) {
	    errflg = 1 ; 
	}
    }
    (void)fclose(fp);
    RkFinalize();
    if (errflg == 1) { 
	exit(ERR_VALUE);
    }
    exit(0);
}

DownLoadDic(fp, dirname)
FILE          *fp;
unsigned char *dirname;
{
    int           ret , hlen , hflg , blen ; 
    unsigned char buf[BUFLEN] ; 
    unsigned char dicname_bk[RECSZ];
    char *whinp ; 

    hflg = 0 ; 
    strcpy((char *)dicname_bk, (char *)dicname);

    if ( hinshi != NULL ) {
      hflg = 1 ; 
      hlen = strlen(hinshi);
    }
    do {
	if ((ret = RkGetWordTextDic(cx_num,dirname,dicname_bk,buf,
				BUFLEN)) >= 0) {
	    dicname_bk[0] = '\0' ; 
	    if (!ret) {
		break;
	    }
	    if ( hflg ) {
		whinp = index((char *)buf,'#') ; 
		if (whinp == NULL) continue ; 
		if (strncmp(whinp,hinshi,hlen) != 0  || 
		    (whinp[hlen] != ' ' && whinp[hlen] != '\t'  &&
	             whinp[hlen] != '*' ))  continue ;
	    }
	    blen = strlen((char *)buf) ;
	    buf[blen+1] = '\0' ;
	    buf[blen  ] = '\n' ;
	    if (!fwrite(buf,1,blen+1,fp)) {
		(void)fprintf(stderr, gettxt("cannacmd:84", 
			   "write error \n"));
		return -1;
	    }
	} else {
	    PrintMessage(ret, dicname);
	    return -1;
	}
    } while(ret >= 0);
    return (0);
}


/**************************************************************/
/*                        cpdic                               */
/**************************************************************/

cp_main(argc,argv)
int   argc  ;
char  **argv;
{
  unsigned char dirname[RECSZ*2];      /* ユーザ名または"iroha"またはNULL*/
  int  dirname_offset = 0 , mode_cp  , ret ; 
  unsigned char *dirnamep; 
  char ans[20];
  
  mode = Rk_MWD;              /* 辞書の種類 */
  mode_cp = 0 ;

  if(argc < 3 || argc > 8) usage();
  
  (void)strcpy((char *)dirname, "");
  (void)strcpy((char *)dicname1, "");
  (void)strcpy((char *)dicname2, "");
  
  scan_opt(argc,argv,&argc);
  if ( opt_l || opt_r || opt_fq || opt_std ) usage();
  if ( opt_dic2 == NULL ) usage();
  if ( argc >= 4 ) usage();  /* 辞書が３つ以上ある */
  
  if ( opt_u ) {
      if (opt_i) usage();
      (void)strcpy((char *)dirname,"user/");
      dirname_offset = strlen((char *)dirname);
      (void)strcat((char *)dirname,opt_user);
  }
  if ( opt_s ) mode = Rk_SWD;

  if ( opt_g ) {
      if ( opt_i || opt_u ) usage();
      (void)strcpy((char *)dirname,"group/");
      (void)strcat((char *)dirname,opt_grp);
  }
  if ( opt_myg ) {
      mode_cp |= RK_GRP_DIC ; 
  }

  (void)strcpy((char *)dicname1,opt_dic1);
  (void)strcpy((char *)dicname2,opt_dic2);
  r_dic = (unsigned char *)opt_dic2 ;     /* 93.03.01 */

  /* まずInitializeして */
  rk_init() ;

  /*  server  new/old check  */
  nwcheck() ;
  

  if ( opt_i == 1 ) {
      if ( majv == 1 ) {         /* old server */
	(void)strcpy((char *)dirname,"iroha");
      }
      else {
	(void)strcpy((char *)dirname,"canna");
      }
      dirname_offset = 0;
  }

  dirnamep = dirname + dirname_offset;
  
  if (protover > canna_protocol_version(3, 1)) {

      if ( dirname[0] == '\0' ) {
	  (void)strcpy((char *)dirname,"user/");
	  (void)strcat((char *)dirname,searchuname());
      }
      dirname_offset = 0;
      dirnamep = dirname + dirname_offset;
      ret = RkCopyDic(cx_num,dirnamep,dicname1,dicname2,mode_cp);
      if (ret == EXIST ) {     /* コピー先に辞書がある */
	  if (isatty(fileno(stdin)) != 0) {
	      (void)fprintf(stderr,gettxt("cannacmd:205", 
    "Specified dictionary \"%s\" already exists. Do you overwrite it ? (y/n)"),
		      dicname2);
	      ans[0]=getchar();     
	  } else {
	      (void)fprintf(stderr,gettxt("cannacmd:206", 
	    "Specified dictionary \"%s\" already exists."),dicname);
	      (void)strcpy(ans,"n");
	  }
	  if ( ans[0] == 'y' ) {     /* 上書きする */
	      ret = RkRemoveDic(cx_num,dicname2,mode_cp);
	      if ( ret == 0) {
		  mode_cp |= KYOUSEI;
		  ret = RkCopyDic(cx_num,dirnamep,dicname1,dicname2,mode_cp);
 		  if ( ret == 0) {
		      (void)fprintf(stderr, gettxt("cannacmd:201", 
		       "Dictionary \"%s\" is overwritten."), dicname2);
		  }
	      }
	      if (ret <0) {
		  PrintMessage(ret,dicname1);
		  RkFinalize();
		  exit(1);
	      }
	  } else {                    /* 上書きしない */
	      (void)fprintf(stderr, gettxt("cannacmd:207", 
	     "Dictionary \"%s\" is not created.\n"), dicname2);
	      RkFinalize();
	      exit(1);
	  }
      }
      else {                          /* コピー先に辞書がない */ 
	  if (ret <0) {
	      PrintMessage(ret,dicname1);
	      RkFinalize();
	      exit(1);
	  }
	  (void)fprintf(stderr, gettxt("cannacmd:199", 
	       "New dictionary \"%s\" is created.\n"), dicname2);
	  (void)fprintf(stderr, gettxt("cannacmd:200", 
	       "Please change customize file."));
      }
  }
  else {                 /* 古い canna の処理 */
      if ( opt_g || opt_myg ) {
	    fprintf(stderr, gettxt("cannacmd:253",
	   "This options or command are not supported by canna-server\n"));
	    exit(ERR_VALUE);
	}

      /* 辞書作成 */
      (void) signal(SIGINT, StopAll);
      (void) signal(SIGQUIT, StopAll);
      (void) signal(SIGTERM, StopAll);

      if (makeDictionary(cx_num, dicname2, mode) < 0) {
	  RkFinalize();
	  exit(ERR_VALUE);
      }

      (void) signal(SIGINT,  RefreshAll);
      (void) signal(SIGQUIT, RefreshAll);
      (void) signal(SIGTERM, RefreshAll);

      if(CopyDic(cx_num, dirnamep, dicname1, dicname2, mode)) {
	  RkFinalize();
	  exit(ERR_VALUE);
      }
  }  

  /* finalizeする */
  RkFinalize();
  (void)fprintf(stderr,	gettxt("cannacmd:101",
	       "\n\"%s\" was copied to \"%s\".\n"), dicname1, dicname2);
  exit(0);
}


/**************************************************************/
/*                        delwords                            */
/**************************************************************/

del_main (argc,argv)
int   argc  ;
char  **argv;
{
    FILE	*fp;
    char	*l_file = NULL;

    mode = Rk_MWD;
    is_display = FALSE;
    hinshi = NULL ; 

    scan_opt(argc,argv,&argc);
    if ( opt_i || opt_u || opt_s ||opt_fq||opt_std||opt_myg||opt_g) usage();
    if ( opt_dic2 != NULL ) usage();

    if ( opt_l ) l_file = opt_lfile ;
    r_dic = (unsigned char *)opt_dic1 ;

    if (!l_file) {
	fp = stdin;
	if (isatty(fileno(stdin)) == 0) {
	    is_display = TRUE;
	}
    } else {
	is_display = TRUE;
	fp = (FILE *)fopen(l_file, "r");
	if (!fp) {
	    Message(gettxt("cannacmd:115",
		   "%s: cannot open \"%s\""), Progname, l_file);
	    exit(ERR_VALUE);
	}
    }

    /* まずInitializeして */
    rk_init() ;
    write_chk();

    if (Addwords(fp) == -1) {
	if (!l_file)
	    (void) fclose(fp);
	(void) RkFinalize();
	exit(ERR_VALUE);
    }
    (void) RkFinalize();
    Message(gettxt("cannacmd:118", "Delwords has done on \"%s\"."), r_dic);
    exit (0);
}


/**************************************************************/
/*                        lsdic                               */
/**************************************************************/

/* 辞書リストを作成します。 */
ls_main(argc,argv)
int   argc  ;
char  **argv;
{
    unsigned char *p;
    int i, arg, j;
    int bufcnt , ret;  
    char  user[RECSZ*2] ;
    unsigned char  buf[BUFLEN];
    int user_offset = 0;
    static char *accsbuf[] ={"-r-","-w-","-rw-","--"};

    strcpy(msg_abnls,gettxt("cannacmd:119",
	    "Cannaserver \"%s\" is in an abnormal state.\n"));
    strcpy(msg_abnl, gettxt("cannacmd:120", 
	    "Cannaserver is in an abnormal state.\n"));

    /*
     * ユーザ名が指定されていない時は
     * 自分の名前を探してその名前でRKを呼びます。
     */
    
    user[0] = '\0';
    mode = 0 ;
    
    scan_opt(argc,argv,&argc);
    if (opt_u) {
      strcpy(user, ":user/");
      user_offset = strlen(user);
      strcpy(user + user_offset, opt_user);
    }
    if ( opt_r || opt_s || opt_fq || opt_std) usage();
    if (opt_dic1 != NULL) usage();

    /* 引数でユーザが指定されなければ自分の辞書をプリントアウトします */
    if ((*user == '\0') && (opt_i == 0) && (opt_g == 0) && (opt_myg == 0)) {
      if (opt_u) { /* ユーザ名にNULLが渡ってきたらエラー */
	usage();
      }
      (void)strcpy(user, ":user/");
      user_offset = strlen(user);
      (void)strcpy(user + user_offset, searchuname());
    }
    
    if (opt_l) {
	if (opt_i || opt_u || opt_g || opt_a ) usage();
    }
    if (opt_a) {
	if (opt_i || opt_g || opt_myg || opt_u || opt_l ) usage();
	opt_i = 1 ; 
	opt_myg = 1 ; 
    }
    /* 辞書リスト作成 */    

    /* まずInitializeして */
    rk_init() ;

    /*  server  new/old check  */
    nwcheck() ;
  
     if ( opt_i == 1 ) {
	    if ( majv == 1 ) {    /* old server */
	      if(*user == '\0') {
		  (void)strcpy(user, "iroha");
		  user_offset = 0;
	      } else {
		  (void)strcat(user, ":iroha");
	      }
	    }
	    else {
	      if(*user == '\0') {
		  user_offset = 0;
		  if (protover > canna_protocol_version(3, 1)) {
		      (void)strcpy(user, ":canna");
		  }
		  else {
		      (void)strcpy(user, "canna");
		  }
	      } else {
		  (void)strcat(user, ":canna");
	      }
	    }
     }
    if (protover > canna_protocol_version(3, 1)) {
      /* ※注: 実は protocol version 3.1 は欠番 */
      user_offset = 0;
    }
    else {
	if ( opt_l || opt_g || opt_myg ) { 
	    fprintf(stderr, gettxt("cannacmd:253",
	   "This options or command are not supported by canna-server\n"));
	    RkFinalize();
	    exit(ERR_VALUE);
	}
    }
    if ( opt_g ) {
	if ( *user == '\0' ) {
	    (void)strcpy(user,":group/");
	    (void)strcat(user,opt_grp);
	}
	else {
	    (void)strcat(user,":group/");
	    (void)strcat(user,opt_grp);
	}
    }
    if ( opt_myg ) {
	mode = RK_GRP_DIC  ;
	if ( *user == '\0' ) {
	    (void)strcpy(user,":group/");
	    (void)strcat(user,searchgroup());
	}
	else {
	    (void)strcat(user,":group/");
	    (void)strcat(user,searchgroup());
	}
    }
    (void) signal(SIGINT, StopAll);
    (void) signal(SIGQUIT, StopAll);
    (void) signal(SIGTERM, StopAll);
    bufcnt = RkListDic(cx_num, user + user_offset, buf, BUFLEN );

    
    /*
     *    辞書一覧をプリントアウトして終わりです。
     */
    
    if (bufcnt >= 0) {
      for (p = buf, i = 0 ; i < bufcnt && *p ; i++) {
	if ( opt_l ) {
	    ret = RkChmodDic(cx_num,p,mode);
	    if (ret < 0 ) {
		(void)fprintf(stderr,gettxt("cannacmd:252",
		      "invalid return code rkchmoddic code=%d \n"),ret);
		RkFinalize();
		exit(1);
	    }
	    switch (ret) {
	      case RK_ENABLE_READ | RK_DISABLE_WRITE :
		j = 0;
		break ; 
	      case RK_ENABLE_WRITE | RK_DISABLE_READ :
		j = 1 ;
		break ; 
	      case RK_ENABLE_WRITE | RK_ENABLE_READ :
		j = 2 ;
		break ; 
	      default :
		j = 3 ;
		break ; 
	    }
	    (void)fprintf(stdout, "%s  %s\n", p,accsbuf[j]);
	}
	else {
	    (void)fprintf(stdout, "%s\n", p);
	}
        p += strlen((char *)p) + 1;
      }
      if ( i != bufcnt ) {
	(void)fprintf(stderr,gettxt("cannacmd:255",
	      "Too many dictionary  \n"));
	RkFinalize();
	exit(1);
      }
      RkFinalize();
      exit(0);
    }
    RkFinalize();
    switch ( bufcnt ) {
      case NOTALC :
	(void)fprintf(stderr, gettxt("cannacmd:130",
	     "Cannot get memory.\n"));
	break;
      case BADCONT :
	(void)fprintf(stderr,gettxt("cannacmd:131",
	    "Illegal context value was used.\n"));
	break;
      case ACCES  :
	(void)fprintf(stderr,gettxt("cannacmd:171",
	    "Cannot access to dictionary.\n"));
	break;
      default:
	if (init[0] == '/') {
	    (void)fprintf(stderr,msg_abnl);
	} else {
	    (void)fprintf(stderr,msg_abnls,init);
	}
	break;
    }
    exit(ERR_VALUE);
}


/**************************************************************/
/*                        mkdic                               */
/**************************************************************/

static int
Upload(fp, flag)
  FILE	*fp;
  int   flag;
{
    int ret=0 ; 
    (void) signal(SIGINT,  StopAll);
    (void) signal(SIGQUIT, StopAll);
    (void) signal(SIGTERM, StopAll);
    if (makeDictionary(cx_num, (unsigned char *)r_dic, mode) != 0) {
	return (-1);
    }
    (void) signal(SIGINT,  RefreshAll);
    (void) signal(SIGQUIT, RefreshAll);
    (void) signal(SIGTERM, RefreshAll);
    if (flag) {
	write_chk();
	ret = Addwords(fp);
    }
    return (ret);
}

mk_main (argc,argv)
int   argc  ;
char  **argv;
{
    FILE	*fp;
    char	*l_file = NULL;
    char        upld = '\0';
    int         i , errflg = 0;

    strcpy(msg_sfq,gettxt("cannacmd:132",
	  "Option  -s and -fq are specified at once. \n")) ;
    strcpy(msg_l,  gettxt("cannacmd:133",
	  "Option - or -l cannot be specified.\n")) ;

    mode = Rk_MWD;
    mode2 = PL_ALLOW ;
    is_display = FALSE;
    hinshi = NULL ;

    scan_opt(argc,argv,&argc);
    if ( opt_i || opt_u || opt_g ) usage();
    if ( opt_l && opt_std ) usage();
    if ( (opt_l || opt_std) && opt_dic2 != NULL ) usage(); 

    if ( opt_fq ) {
      if ( opt_s ) {
        (void) fprintf(stderr,msg_sfq);
        exit(ERR_VALUE) ;
      }  
      if ( opt_l || opt_std ) {
        (void) fprintf(stderr,msg_l);
        exit(ERR_VALUE) ;
      }
      mode = mode | PL_DIC ;
      mode2 = PL_ALLOW ; 
    }
    if ( opt_s ) mode = Rk_SWD;
    if ( opt_l || opt_std ) {
      upld++ ; 
      l_file = opt_lfile ; 
    }
    if ( opt_myg ) { mode = mode | RK_GRP_DIC ;
    }
    if (upld) {
      if (isatty(fileno(stdout)) == 0) {
	  is_display = TRUE;
      }
      if (!l_file) {
	fp = stdin;
      } else {
	is_display = TRUE ; 
	fp = (FILE *)fopen(l_file, "r");
	if (!fp) {
	  Message(gettxt("cannacmd:149", 
			 "%s: cannot open \"%s\""), Progname, l_file);
	  exit(ERR_VALUE);
	}
      }
    }

    /* まずInitializeして */
    rk_init() ;
    for ( i = 1 ; i < argc ; i++) {
	r_dic = (unsigned char *)argv[i];
	if (Upload(fp, (int)upld) == -1) {
	    errflg = 1 ; 
	}
	else {
	    if (!upld) {
		Message("");
	    }
	}
    }
    (void) RkFinalize();
    if (upld && l_file)
      (void)fclose(fp);
    if (errflg == 1) {
	exit(ERR_VALUE);
    }
    exit (0);
}


/**************************************************************/
/*                        mvdic                               */
/**************************************************************/

mv_main(argc,argv)
int   argc  ;
char  **argv;
{
  int  ret ,mode;
  char *dicname1;
  char *dicname2;
  char dic1[RECSZ];
  char dic2[RECSZ];
  ret = 0;

  (void)strcpy(msg_abnl,gettxt("cannacmd:153",
       "Cannaserver is in an abnormal state.\n"));
  (void)strcpy(msg_abnls,gettxt("cannacmd:154",
       "Cannaserver \"%s\" is in an abnormal state.\n"));

  mode = NOT_OVER_WRITE;
  scan_opt(argc,argv,&argc);
  if (opt_l || opt_r || opt_i ||opt_u||opt_s||opt_fq||opt_std||opt_g) usage();
  if (opt_dic2 == NULL) usage();
  if ( opt_myg ) mode = mode | RK_GRP_DIC ; 

  
  dicname1 = opt_dic1 ; 
  dicname2 = opt_dic2 ; 

  strncpy(dic1, dicname1, RECSZ - 1);
  strncpy(dic2, dicname2, RECSZ - 1);

  /* 辞書名が同じならエラー */
  if(!strcmp(dic1,dic2)) {
    fprintf(stderr, gettxt("cannacmd:161", 
	   "%s: %s, Dictionary name is same.\n"),Progname, dic1);
    exit(ERR_VALUE);
  }

  (void) signal(SIGINT, StopAll);
  (void) signal(SIGQUIT, StopAll);
  (void) signal(SIGTERM, StopAll);


  /* まずInitializeして */
  rk_init() ;

  ret = renameDictionary(cx_num, dic1, dic2, mode);
  RkFinalize();
  exit(ret);    
}

renameDictionary(cn, dicname1, dicname2, force)
int cn;
char *dicname1;
char *dicname2;
int force;
{
  char ans[20];
  int ret = 0;


  nwcheck() ;

  switch ( RkRenameDic( cn, dicname1, dicname2, force) ) {
  case 0 :
    fprintf(stderr,gettxt("cannacmd:166",
	  "Change dictionary \"%s\" to \"%s\".\n"),dicname1, dicname2);
    ret = 0;
    break;
  case 1 :
    fprintf(stderr,gettxt("cannacmd:167", 
	  "Overwrite dictionary \"%s\" to \"%s\".\n"),dicname1, dicname2);
    ret = 0;
    break;
  case NOTALC :
    (void)fprintf(stderr, gettxt("cannacmd:168", "No more memory.\n"));
    break;
  case NOENT :
    fprintf(stderr,gettxt("cannacmd:169", 
	  "Dictionary \"%s\" does not exist.\n"), dicname1);
    ret = 1;
    break;
  case BADF :
    fprintf(stderr,gettxt("cannacmd:170",
	   "\"%s\" or \"%s\" is binary dictionary.\n"), dicname1,dicname2);
    ret = 1;
    break;
  case ACCES :
    fprintf(stderr,gettxt("cannacmd:171", "Cannot access to dictionary.\n"));
    ret = 1;
    break;
  case BADDR :
    (void)fprintf(stderr,gettxt("cannacmd:172", 
	"dics.dir is abnormal. Cannot create dictionary file.\n"));
    ret = -1;
    break;
  case MOUNT :
  case EXIST :
    fprintf(stderr,gettxt("cannacmd:173", 
"Specified dictionary \"%s\" already exists. Do you overwrite it ? (y/n)"),
	    dicname2);
    ans[0]=getchar();          /* 92.10.28 */
    if(ans[0] == 'y') {
      force = force | OVER_WRITE  ;
      ret = renameDictionary(cn, dicname1, dicname2, force);
    }
    else {
      fprintf(stderr, gettxt("cannacmd:174",
	     "Specified dictionary \"%s\" does not overwite.\n"),dicname2);
      ret = 1;
    }
    break;
  case TXTBSY :
    fprintf(stderr,gettxt("cannacmd:175", 
	  "Dictionary \"%s\" or \"%s\" is in use. Cannot overwrite it.\n"),
	    dicname1, dicname2);
    ret = 1;
    break;
  case BADCONT :
    fprintf(stderr,gettxt("cannacmd:176","Illegal context value was used.\n"));
    ret = 1;
    break;
  default:
    if(init[0] == '/') {
      fprintf(stderr,msg_abnl );
    }
    else {
      fprintf(stderr,msg_abnls,init);
    }
    ret = 1;
    break;
  }
  return ret;
}


/**************************************************************/
/*                        rmdic                               */
/**************************************************************/

rm_main(argc,argv)
int   argc  ;
char  **argv;
{
  int  i, j , ret , undel ;
  int isflag = 0;
  int rmdone = 0; /* rm がまだなされていない */


  mode = 0  ;
  if(argc < 2) usage();

  scan_opt(argc,argv,&argc);
  if ( opt_l || opt_r || opt_i ||opt_u||opt_s||opt_std||opt_g) usage();

  if ( opt_fq ) mode = PL_DIC ; 
  if ( opt_myg ) mode = mode | RK_GRP_DIC ;

  /* まずInitializeして */
  rk_init() ;

  (void) signal(SIGINT, StopAll);
  (void) signal(SIGQUIT, StopAll);
  (void) signal(SIGTERM, StopAll);

  /* 辞書削除 */
  undel = 0 ; 
  for(j = 1; j < argc; j++) {

      strncpy((char *)rm_dic, (char *)argv[j], RECSZ - 1);
      rm_dic[RECSZ - 1] = (unsigned char)0;

      ret = rmDictionary(cx_num, rm_dic, mode) ; 
      if (  ret == -1 ) {
	RkFinalize();
	exit(ERR_VALUE);
      }
      if (  ret == -2 ) {  /* 辞書を消せなかった */
	  undel = 1 ; 
      }
      rmdone = 1; /* 実際に rm が行われた */
    }

  RkFinalize();
  if (rmdone == 0) { /* 全然 rm がなされなかったならば */
    (void)fprintf(stderr, gettxt("cannacmd:185",
	 "Dictionary is not specified.\n"));
  }
  if (undel == 0 ) {
      exit(0) ;
  }
  else {
      exit(2) ;
  }
}

/************************************************************************/
/*                    chmoddic                                          */
/************************************************************************/

ch_main(argc,argv)
int   argc  ;
char  **argv;
{
    int  ret ,mode ; 
    scan_opt(argc,argv,&argc);
    if (opt_l||opt_r||opt_s||opt_fq||opt_std||opt_g||opt_u||opt_a) usage();
    mode = opt_rw ; 
    if ( mode == 0 ) usage();
    if ( opt_dic2 != NULL ) usage();
    if ( opt_myg ) mode = mode | RK_GRP_DIC;
    rk_init();

    (void) signal(SIGINT, StopAll);
    (void) signal(SIGQUIT, StopAll);
    (void) signal(SIGTERM, StopAll);

    ret = RkChmodDic(cx_num,opt_dic1,mode);
    if (ret < 0) { 
	switch (ret) {
          case NOENT:
	    fprintf(stderr,gettxt("cannacmd:169", 
		  "Dictionary \"%s\" does not exist.\n"), opt_dic1);
	    break;
          case BADCONT:
	    fprintf(stderr,gettxt(
		  "cannacmd:176","Illegal context value was used.\n"));
	    break;
          case ACCES:
	    fprintf(stderr,gettxt(
		  "cannacmd:171", "Cannot access to dictionary.\n"));
	    break;
          case NOTALC:
	    fprintf(stderr, gettxt("cannacmd:168", "No more memory.\n"));
	    break;
          case -1:
	    fprintf(stderr, gettxt("cannacmd:253",
  	   "This options or command are not supported by canna-server\n"));
	    break;
          default:
	    fprintf(stderr, gettxt("cannacmd:252",
		   "invalid return code rkchmoddic  code=%d \n"),ret);
	    break;
	}
	RkFinalize();
	exit(ERR_VALUE);
    }
    RkFinalize();
    exit(0);
}
/************************************************************************/
/*                    syncdic                                           */
/************************************************************************/

sy_main(argc,argv)
int   argc  ;
char  **argv;
{
    int  ret ,mode ; 
    scan_opt(argc,argv,&argc);
    if (opt_l||opt_r||opt_s||opt_fq||opt_std||opt_g||opt_u||opt_a||opt_myg)
      usage();

    rk_init();
    nwcheck();

    if (protover < canna_protocol_version(3,1)) {
	fprintf(stderr, gettxt("cannacmd:253",
  	   "This options or command are not supported by canna-server\n"));
	exit(ERR_VALUE);
    }

    (void) signal(SIGINT, StopAll);
    (void) signal(SIGQUIT, StopAll);
    (void) signal(SIGTERM, StopAll);

    ret = RkSync(cx_num,NULL);
    if (ret < 0) { 
	fprintf(stderr,
	   "invalid return code rksync  code=%d \n",ret);
	RkFinalize();
	exit(ERR_VALUE);
    }
    RkFinalize();
    exit(0);
}

can_ver()
{
    rk_init();
    nwcheck();
/*    printf(gettxt("cannacmd:254","canna version is %d.%d \n",
	   protover/1024 , protover % 1024 ));*/
    printf("canna version is %d.%d \n",
	   protover/1024 , protover % 1024 );
    RkFinalize();
    exit(0) ; 
}

/************************************************************************/
/*                    main                                              */
/************************************************************************/
static struct  command {
	char *name ;
	int  (*func) pro((int, char **));
	int  cmd_code ;
}	commands[] = {
	{"addwords",add_main,1},
	{"catdic"  ,cat_main,2},
	{"cpdic"   ,cp_main,3 },
	{"delwords",del_main,4},
	{"lsdic"   ,ls_main,5 },
	{"mkdic"   ,mk_main,6 },
	{"mvdic"   ,mv_main,7 },
	{"rmdic"   ,rm_main,8 },
	{"downloaddic" ,cat_main,2 },
	{"cpuserdic"   ,cp_main,3 },
	{"lsuserdic"   ,ls_main,5 },
	{"mkuserdic"   ,mk_main,6 },
	{"uploaddic"   ,mk_main,6 },
	{"mvuserdic"   ,mv_main,7 },
	{"rmuserdic"   ,rm_main,8 },
	{"chmoddic"    ,ch_main,9 },
	{"syncdic"     ,sy_main,10}
};
#define  NCOMMANDS    (sizeof(commands) / sizeof(struct command))

main(argc,argv)
int argc ;
char **argv ; 
{
    int     i ; 
    char *p ;

#if __STDC__ || defined(SVR4)
    (void)setlocale(LC_ALL,"");
#endif 

     p = rindex(argv[0],'/');
     if ( p == NULL ) {
	 Progname = argv[0];
     }
     else {
	 Progname = p + 1 ;
     }

     (void)strcpy(init, "/usr/lib/canna/dic");  /* サーバ方式では無意味 */
     for ( i = 0; i < NCOMMANDS ; i++) {
	 if (strcmp(Progname,commands[i].name) == 0) {
	     cmd_code = commands[i].cmd_code ;
	     (*commands[i].func)(argc,argv);
	     break ; 
         }
     }
     return (0);
}

/*  オプションのチェック 
    辞書名以外のオプションはチェック後 argv から取り除く   */
scan_opt(argc,argv,argcp)
int  argc ,*argcp; 
char **argv ; 
{ 
/* この関数でチェックするもの 
       オプションの重複指定がないか
       引数を指定するオプションの引数があるか
       辞書が指定されているか(lsdicを除く)
       ３つ以上辞書がないか(rmdicを除く)
*/

static  char *options[]={"-cs","-cannaserver","-l","-hi","-h","-i","-u","-s",
  "-fq","-","-G","-g","+w","-w","+r","-r","+wr","+rw","-wr","-rw","-v","-a"};
#define NOPTIONS sizeof(options) / sizeof(char *)
#define OPT_CS   0
#define OPT_CANNASAVER  1
#define OPT_L   2
#define OPT_R   3
#define OPT_H   4
#define OPT_I   5
#define OPT_U   6
#define OPT_S   7
#define OPT_FQ  8
#define OPT_STD 9
#define OPT_MYG 10
#define OPT_G   11
#define OPT_WADD 12
#define OPT_WDEL 13
#define OPT_RADD 14
#define OPT_RDEL 15
#define OPT_WRADD 16
#define OPT_RWADD 17
#define OPT_WRDEL 18
#define OPT_RWDEL 19
#define OPT_V   20
#define OPT_A   21

int 	opt_code , i ; 
char    **p ; 

    opt_cs = opt_l = opt_r = opt_h = opt_i = opt_u = opt_s = opt_fq = 0 ;
    opt_std = opt_myg = opt_g = opt_v = opt_a = 0 ;
    opt_rw  = 0 ; 
    opt_lfile = opt_dic1 = opt_dic2 = opt_user = opt_grp = NULL ;

    p = argv + 1 ;   /* コマンド部分を除く */
    while( *p != NULL ) {
/*    printf(" argc= %d opt= %s\n",argc,*p);*/
      for (opt_code = 0 ; opt_code  < NOPTIONS ; opt_code++ ){
	if (strcmp( *p ,options[opt_code]) == 0 )  break ; 
      }
      switch(opt_code) {
        case OPT_CS :
        case OPT_CANNASAVER :
	  if (opt_cs)  usage();			/* 重複チェック */
	  if ( *(p + 1) == NULL ) usage();
	  opt_cs = 1 ; 
	  (void) strcpy(init,*(p+1));
	  shrink_opt(argc,p,2);		
	  argc -= 2 ; 
	  break ; 
        case OPT_FQ :
	  if (opt_fq) usage();
	  opt_fq = 1 ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break  ;
        case OPT_S :
	  if (opt_s) usage();
	  opt_s = 1 ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break  ;
        case OPT_L :
	  if (opt_l) usage();
	  opt_l = 1 ;
	  if (cmd_code == LS ) {
	      shrink_opt(argc,p,1);
	      argc-- ; 
	  }
	  else {
	      if ( *(p + 1) == NULL ) usage();
	      opt_lfile = *(p+1);
	      shrink_opt(argc,p,2);
	      argc -= 2 ; 
	  }
	  break ; 
        case OPT_H :
	  usage();
        case OPT_R :
	  if (opt_r) usage();
	  if ( *(p + 1) == NULL ) usage();
	  hinshi = *(p+1);
	  shrink_opt(argc,p,2);
	  argc -= 2 ; 
	  break ; 
        case OPT_I :
	  if (opt_i) usage();
	  opt_i = 1 ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break  ;
        case OPT_U :
	  if (opt_u) usage();
	  if ( *(p + 1) == NULL ) usage();
	  opt_u = 1 ; 
	  opt_user = *(p+1);
	  shrink_opt(argc,p,2);
	  argc -= 2 ; 
	  break ; 
        case OPT_STD :
	  if (opt_std) usage();
	  opt_std = 1 ; 
	  opt_lfile = NULL;
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break  ;
        case OPT_MYG :    /* -G */
	  if ( opt_myg ) usage();
	  opt_myg = 1 ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_G :      /* -g */
	  if ( opt_g ) usage();
	  if ( *(p+1) == NULL ) usage();
	  opt_g = 1 ; 
	  opt_grp = *(p+1);
	  shrink_opt(argc,p,2);
	  argc-=2 ; 
	  break ; 
        case OPT_WADD :      /* +w */
	  if ((opt_rw & RK_DISABLE_WRITE) == RK_DISABLE_WRITE ) usage();
	  opt_rw = opt_rw | RK_ENABLE_WRITE ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_WDEL :      /* -w */
	  if ((opt_rw & RK_ENABLE_WRITE) == RK_ENABLE_WRITE ) usage();
	  opt_rw = opt_rw | RK_DISABLE_WRITE ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_RADD :      /* +r */
	  if ((opt_rw & RK_DISABLE_READ) == RK_DISABLE_READ ) usage();
	  opt_rw = opt_rw | RK_ENABLE_READ ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_RDEL :      /* -r */
	  if ((opt_rw & RK_ENABLE_READ) == RK_ENABLE_READ ) usage();
	  opt_rw = opt_rw | RK_DISABLE_READ ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_WRADD :     /* +wr */
        case OPT_RWADD :     /* +rw */
	  if ((opt_rw & RK_DISABLE_READ) == RK_DISABLE_READ ) usage();
	  if ((opt_rw & RK_DISABLE_WRITE) == RK_DISABLE_WRITE ) usage();
	  opt_rw = opt_rw | RK_ENABLE_WRITE | RK_ENABLE_READ ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_WRDEL :     /* -wr */
        case OPT_RWDEL :     /* -rw */
	  if ((opt_rw & RK_ENABLE_READ) == RK_ENABLE_READ ) usage();
	  if ((opt_rw & RK_ENABLE_WRITE) == RK_ENABLE_WRITE ) usage();
	  opt_rw = opt_rw | RK_DISABLE_WRITE | RK_DISABLE_READ ; 
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        case OPT_V :
	  can_ver() ;
        case OPT_A :
	  opt_a = 1;
	  shrink_opt(argc,p,1);
	  argc-- ; 
	  break ; 
        default :
	  dicname_chk(*p);
	  if ( opt_dic1 == NULL ) {
	    opt_dic1 = *p ; 
	  }
	  else {
	    opt_dic2 = *p ; 
	  }
	  p++ ; 
      }       /* case end */
    }         /* for  end */
    if (cmd_code == LS || cmd_code == SY){
	if ( opt_dic1 != NULL ) usage();
    }
    else {
	if ( opt_dic1 == NULL)  usage();
    }

    if (cmd_code != CHMOD && opt_rw != 0) usage();

    *argcp = argc ; 
    return(0);
}

/*  argv のオプションを n 個分前に詰める */
shrink_opt(argc,argv,n)
int  argc, n ; 
char  *argv[] ;
{
    int  i ; 
    for ( i = n ; i < argc ; i++ ) {
      argv[i-n] = argv[i];
    }
}
