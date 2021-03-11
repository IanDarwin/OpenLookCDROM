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
 * rutil.c
 */
#ifndef lint
static char rcs[] = "@(#) 112.1 $Id: rutil.c,v 2.9 1994/04/27 00:28:29 kon Exp $";
#endif

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#include <stdio.h>
#include <canna/RK.h>

#ifdef USE_VARARGS
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#endif

#if __STDC__
#include <stdlib.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

#if __STDC__ || defined(SVR4)
#include <locale.h>
#endif

#ifdef SVR4
extern  char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif

#define  BUFLEN  1024        /* add 91.11.21. */ 
#define  ERR_VALUE  1

extern int RkGetWordTextDic();
extern char init[];

static char  msg_mem[80];
static char  msg_abnls[80];
static char  msg_abnl[80];
static char  msg_mnts[80];
static char  msg_mnt[80];
static char  msg_fq[80];
static char  msg_cnt[80];
static int   msg_flg = 0 ; 

msg_set(){
    
    if (msg_flg == 1 ) return ; 
    (void)strcpy(msg_mem,gettxt("cannacmd:186", "No more memory.\n"));
    (void)strcpy(msg_abnls,gettxt("cannacmd:187",
         "Cannaserver \"%s\" is in an abnormal state.\n"));
    (void)strcpy(msg_abnl,gettxt("cannacmd:188",
	 "Cannaserver is in an abnormal state.\n"));
    (void)strcpy(msg_mnts,gettxt("cannacmd:189", 
	 "Cannaserver \"%s\" does not support dictionary maintenance.\n"));
    (void)strcpy(msg_mnt,gettxt("cannacmd:190", 
 	 "Cannaserver does not support dictionary maintenance.\n"));
    (void)strcpy(msg_fq,gettxt("cannacmd:191",
         "Cannot use option -fq in irohaserver.\n"));
    (void)strcpy(msg_cnt,gettxt("cannacmd:192", 
         "Illegal context value was used.\n"));
    msg_flg = 1 ; 

}

RkDefineLine(cx_num, name, line)
int cx_num;
unsigned char *name;
char *line;
{
  unsigned int linelen = strlen(line);
  int yomilen, yomihinshilen = 0;
  char *buf = (char *)malloc(linelen + 1), *sp, *dp;
  int res = 0;

  
  if ( !buf ) {
    return -1;
  }
  sp = line;
  while (*sp == ' ' || *sp == '\t'   )
    sp++; /* 空白の読み飛ばし */

  if (!*sp || *sp == '#') /* コメント行 */
    goto endDefineLine;

  dp = buf;
  while (*sp && ( *sp != ' ' && *sp != '\t' )) { /* 読みのとりだし */
      if (*sp == '\\' && *(sp+1) ) { /* エスエープされた文字 */
	  *dp++ = *sp++ ; 
      }
      *dp++ = *sp++;
  }
  *dp++ = ' ';
  yomilen = dp - buf;

  while (*sp) {
    while (*sp == ' ' || *sp == '\t' )
      sp++; /* 空白の読み飛ばし */

    if (*sp) {
      if (*sp == '#') {
	dp = buf + yomilen;
	while (*sp && (*sp != ' ' && *sp != '\t' )) { /* 品詞と頻度のコピー */
	  *dp++ = *sp++;
	}
	*dp++ = ' ';
	yomihinshilen = dp - buf;

	while (*sp == ' ' || *sp == '\t' )
	  sp++; /* 空白の読み飛ばし */
      }
      if (yomihinshilen == 0) { /* ここまで品詞情報が出てこなかった。 */
	goto endDefineLine;
      }
      else if (*sp) { /* 候補があるのなら */
	dp = buf + yomihinshilen;
	while (*sp && ( *sp != ' ' && *sp != '\t' )) { /* 候補のコピー */
	    if (*sp == '\\' && *(sp+1) ) { /* エスエープされた文字 */
		*dp++ = *sp++ ; 
	    }
	    *dp++ = *sp++;
	}
	*dp++ = '\0'; /* ヌル文字を最後に入れる */
	res = RkDefineDic(cx_num, (char *)name, buf);
	if (res < 0) {
	  goto endDefineLine;
	}
      }
    }
  }
 endDefineLine:
  (void)free(buf);
  return res;
}

#ifdef TEST_DEFINEDIC
RkDefineDic(cx_num, name, word)
int cx_num;
char *name;
char *word;
{
  printf("☆単語の定義(辞書:%s) \"%s\"\n", name, word);
  return 0;
}

main()
{
  char buf[2048], *p;
  int c;

  p = buf;
  c = getchar();
  while (c >= 0) {
    if (c == '\n') {
      *p++ = '\0';
      RkDefineLine(0, "tempdic", buf);
      p = buf;
    }
    else {
      *p++ = c;
    }
    c = getchar();
  }
}
#endif /* TEST_DEFINEDIC */

CopyDic(cx_num, dirname, dicname1, dicname2, mode)
int            cx_num;
unsigned char  *dirname;
unsigned char  *dicname1;
unsigned char  *dicname2;
int            mode ;
{
  register int i = 0;
  int ret;
  unsigned char buf[BUFLEN];
  unsigned char dic1_bk[BUFLEN];

  msg_set();
  (void)strcpy((char *)dic1_bk, (char *)dicname1);

  do { /* RkGetWordTextDicの返り値が正の間ループする */
    if((ret = RkGetWordTextDic(cx_num, dirname, dic1_bk, buf, BUFLEN)) >= 0) {
      if(!ret)
	break;
      if(dic1_bk[0] != '\0') { /* １回目はマウントする */
	if(RkMountDic(cx_num, (char *)dicname2, 0)) {
	  
#ifdef DEBUG
	  (void)fprintf(stderr, "\nmode=%d\n", mode);
#endif
	  /* マウントに失敗した時の処理 */
	  if ((mode & KYOUSEI) == KYOUSEI) {
	    RkCreateDic(cx_num, dicname2, mode);
	  } else {
	    (void)rmDictionary(cx_num, dicname2 ,mode);
	  }
	  (void)fprintf(stderr, gettxt("cannacmd:193", 
	       "Cannot mount dictionary \"%s\".\n"),dicname2);
	  return -1;
	}
      }
      /* １行登録する */
      if (RkDefineLine(cx_num, dicname2, (char *)buf) == -1) {
	(void)fprintf(stderr,gettxt("cannacmd:194", 
	    "write error: \"%s\"\n"), buf);
	continue;
      }
      (void)strcpy((char *)buf,"");
      (void)strcpy((char *)dic1_bk, "");
      i++;
      if ((i & 0x0f) == 0) {
	(void)fputs (".", stderr);
      }
    }
    else { /* RkGetWordTextDicの返り値が負の時 */
      RkUnmountDic(cx_num,(char *)dicname2);
      (void)fprintf(stderr,"\n");
      PrintMessage(ret, dic1_bk);
      if ((mode & KYOUSEI) == KYOUSEI) {
	RkCreateDic(cx_num, dicname2, mode);
      } else {
	(void)rmDictionary(cx_num,dicname2, mode);
      }
      return -1; 
    }
  } while(ret >= 0);
  RkUnmountDic(cx_num,(char *)dicname2);
  return 0; 
}

PrintMessage(ret, dicname)
int            ret;
unsigned char  *dicname;
{
  msg_set();
  switch (ret) {
  case 0 :
    break;
  case NOENT :
    (void)fprintf(stderr, gettxt("cannacmd:195", 
	 "Dictionary \"%s\" does not exist.\n"), dicname);
    break;
  case NOTALC :
    (void)fprintf(stderr, msg_mem);
    break;
  case BADF :
    (void)fprintf(stderr, gettxt("cannacmd:196",
	 "Specified dictionary \"%s\" is binary dictionary.\n"),dicname);
    break;
  case BADDR :
    (void)fprintf(stderr, gettxt("cannacmd:197", "dics.dir is abnormal.\n"),
		  dicname);
    break;
  case NOMOUNT :
    (void)fprintf(stderr, gettxt("cannacmd:198", 
	 "Cannot mount dictionary \"%s\".\n"), dicname);
    break;
  case ACCES :
    (void)fprintf(stderr,gettxt("cannacmd:171",
	"Cannot access to dictionary.\n"));
    break;
  default:
    if (init[0] != '/') {
	(void)fprintf(stderr,msg_abnls,init);
    }
    else {
	(void)fprintf(stderr,msg_abnl);
    }
    break;
  }
}

makeDictionary(cn, dicname, mode)
int cn;
unsigned char *dicname;
int mode ;
{
  char ans[79];
  int ret = 0;

  static int  majv , minv , bak ;

  msg_set();
  /*  server  new/old check  */
  bak = RkGetServerVersion(&majv, &minv);
  if(bak) { /* サーバの状態が異常 */
    if (init[0] != '/') {
	(void)fprintf(stderr, msg_abnls, init);
    }
    else {
	(void)fprintf(stderr, msg_abnl);
    }
    ret = -1;
    return ret;
  }
  if ( majv < 2 ) { /* irohaserver */
    if(minv < 2) { /* R7.1より前 */
      if (init[0] != '/') {
	  (void)fprintf(stderr, msg_mnts, init);
      }
      else {
	  (void)fprintf(stderr, msg_mnt);
      }
      ret = -1;
      return ret;
    }
    if ((mode & PL_DIC) == PL_DIC ) {
      (void)fprintf(stderr,msg_fq);
      ret = -1 ;
      return ret ;
    }
  }

  if (( mode & PL_DIC ) != PL_DIC ) {
  switch ( RkCreateDic( cn, dicname, mode) ) {
  case 0 :
    (void)fprintf(stderr, gettxt("cannacmd:199", 
	 "New dictionary \"%s\" is created.\n"), dicname);
    (void)fprintf(stderr, gettxt("cannacmd:200", 
	 "Please change customize file."));
    ret = 0;
    break;
  case 1 :
    (void)fprintf(stderr, gettxt("cannacmd:201", 
	 "Dictionary \"%s\" is overwritten."), dicname);
    ret = 0;
    break;
  case NOTALC :
    (void)fprintf(stderr, msg_mem);
    ret = -1;
    break;
  case BADF :
    (void)fprintf(stderr, gettxt("cannacmd:202", 
	 "\"%s\" is binary dictionary. Cannot overwrite it.\n"),dicname);
    ret = -1;
    break;
  case BADDR :
    (void)fprintf(stderr,gettxt("cannacmd:203", 
	"dics.dir is abnormal. Cannot create dictionary.\n"));
    ret = -1;
    break;
  case ACCES :
    (void)fprintf(stderr,gettxt("cannacmd:204","Cannot create dictionary.\n"));
    ret = -1;
    break;
  case EXIST :
   /*
    * check 'stdin' for uploaddic.
    */
    if (isatty(fileno(stdin)) != 0) {
	(void)fprintf(stderr,gettxt("cannacmd:205", 
    "Specified dictionary \"%s\" already exists. Do you overwrite it ? (y/n)"),
		      dicname);
	fgets(ans,80,stdin);
    } else {
	(void)fprintf(stderr,gettxt("cannacmd:206", 
	    "Specified dictionary \"%s\" already exists."),dicname);
	(void)strcpy(ans,"n");
    }
    if ( ans[0] == 'y' ) {
	mode |= KYOUSEI;
	if((ret = makeDictionary(cn, dicname, mode)) != 0) {
#ifdef DEBUG
	    fprintf(stderr, "makeDictionary: cannot create %s by mode %d\n",
		    dicname, mode);
#endif
	}
    } else {
	(void)fprintf(stderr, gettxt("cannacmd:207", 
	     "Dictionary \"%s\" is not created.\n"), dicname);
	ret = -1;
    }
    break;
  case INVAL :
    (void)fprintf(stderr,gettxt("cannacmd:208", 
"Dictionary \"%s\" is different from current dic. Cannot overwrite it.\n"),
		  dicname);
    ret = -1;
    break;
  case MOUNT :
  case TXTBSY :
    (void)fprintf(stderr,gettxt("cannacmd:209", 
	"Dictionary \"%s\" is in use. Cannot overwrite it.\n"),dicname);
    ret = -1;
    break;
  case BADARG :
    (void)fprintf(stderr,gettxt("cannacmd:210", "Mode value is abnormal.\n"));
    ret = -1;
    break;
  case BADCONT :
    (void)fprintf(stderr,msg_cnt);
    ret = -1;
    break;
  default:
    if(init[0] != '/') {
      (void)fprintf(stderr, msg_abnls, init);
    }
    else {
	(void)fprintf(stderr, msg_abnl);
    }
    exit(ERR_VALUE);
    break;
  }
  }
  else {    /*  PL_DIC  */
  switch ( RkCreateDic( cn, dicname, mode) ) {
  case 0 :
    (void)fprintf(stderr, gettxt("cannacmd:211", 
  "Personal learning file of system dictionary \"%s\" is created."), dicname);
    ret = 0;
    break;
  case 1 :
    (void)fprintf(stderr, gettxt("cannacmd:212", 
"Personal learning file of system dictionary \"%s\" is overwritten."),dicname);
    ret = 0;
    break;
  case NOTALC :
    (void)fprintf(stderr, msg_mem);
    ret = -1;
    break;
  case BADF :
    (void)fprintf(stderr,gettxt("cannacmd:213", 
	"\"%s\" is text dictionary. Cannot create personal learning file.\n"),
		  dicname);
    ret = -1;
    break;
  case BADDR :
    (void)fprintf(stderr,gettxt("cannacmd:214", 
	"dics.dir is abnormal. Personal learning file is not created. \n"));
    ret = -1;
    break;
  case ACCES :
    (void)fprintf(stderr, gettxt("cannacmd:215", 
	 "Personal learning file is not created.\n"));
    ret = -1;
    break;
  case EXIST :
   /*
    * check 'stdin' for uploaddic.
    */
    if (isatty(fileno(stdin)) != 0) {
	(void)fprintf(stderr,gettxt("cannacmd:216", 
"Personal learning file of dictionary \"%s\" exists. Do you overwrite it ? (y/n)"),dicname);
	fgets(ans,80,stdin);
    } else {
	(void)fprintf(stderr,gettxt("cannacmd:217", 
    "Personal learning file of system dictionary \"%s\" already exists."),
		      dicname);
	(void)strcpy(ans,"n");
    }
    if ( ans[0] == 'y' ) {
	mode |= KYOUSEI;
	if((ret = makeDictionary(cn, dicname, mode)) != 0) {
#ifdef DEBUG
	    fprintf(stderr, "makeDictionary: cannot create %s by mode %d\n",
		    dicname, mode);
#endif
	}
    } else {
	(void)fprintf(stderr, gettxt("cannacmd:218", 
     "Personal learning file of system dictionary \"%s\" is not created.\n"),
		      dicname);
	ret = -1;
    }
    break;
  case INVAL :
    (void)fprintf(stderr,gettxt("cannacmd:219", 
"Learning file \"%s\" is different from current dic. Cannot overwrite it.\n"),
		  dicname);
    ret = -1;
    break;
  case MOUNT :
  case TXTBSY :
    (void)fprintf(stderr,gettxt("cannacmd:220", 
      "\"%s\" is in use. Cannot overwrite personal learing file.\n"),dicname);
    ret = -1;
    break;
  case BADARG :
    (void)fprintf(stderr,msg_fq);
    ret = -1;
    break;
  case BADCONT :
    (void)fprintf(stderr,msg_cnt);
    ret = -1;
    break;
  case NOENT  :   /* 91.12.03 */
    (void)fprintf(stderr, gettxt("cannacmd:221", 
	 "System dictionary \"%s\" does not exist.\n"), dicname);
    ret = -1 ; 
    break ; 
  default:
    if(init[0] != '/') {
      (void)fprintf(stderr, msg_abnls, init);
    }
    else {
	(void)fprintf(stderr, msg_abnl);
    }
    exit(ERR_VALUE);
    break;
  }
  }       /*  if end */
  return ret;
}

int
rmDictionary(cn, dicname,mode)
int cn;
unsigned  char *dicname;
int  mode ;
{
  int ret = 0;
  static int  majv , minv ,bak ;     /* add 91.11.21 */

  msg_set() ; 
#ifdef DEBUG
  (void)fprintf(stderr,"RkRemoveDic(cn=%d,dicname=%s)\n", cn, dicname);
#endif

  /*  server  new/old check  */
  bak = RkGetServerVersion(&majv, &minv);
  if(bak) { /* サーバの状態が異常 */
    if (init[0] != '/') {
      (void)fprintf(stderr, msg_abnls,init);
    }
    else {
	(void)fprintf(stderr, msg_abnl);
    }
    ret = -1;
    return ret;
  }
  if ( majv < 2 ) { /* irohaserver */
    if(minv < 2) { /* R7.1より前 */
      if (init[0] != '/') {
	(void)fprintf(stderr, msg_mnts, init);
      }
      else {
	  (void)fprintf(stderr, msg_mnt);
      }
      ret = -1;
      return ret;
    }
    if ((mode & PL_DIC) == PL_DIC ) {
      (void)fprintf(stderr, msg_fq);
      ret = -1 ;
      return ret ;
    }
  }

  if (( mode & PL_DIC ) != PL_DIC ) {
  switch (RkRemoveDic(cn, dicname, mode)) {
  case 0:
    (void)fprintf(stderr, gettxt("cannacmd:222", 
	 "Dictionary \"%s\" is deleted.\n"), dicname );
    ret = 0;
    break;
  case NOENT :
    (void)fprintf(stderr, gettxt("cannacmd:223", 
	 "Dictionary \"%s\" does not exist.\n"), dicname );
    ret = -2 ;  /* 93.03.03 */
    break;
  case BADF :
    (void)fprintf(stderr,gettxt("cannacmd:224", 
	"\"%s\" is binary dictionary. Cannot detele it.\n"),dicname);
    ret = -2;   /* 93.03.03 */
    break;
  case ACCES :
    (void)fprintf(stderr,gettxt("cannacmd:225", 
	"Cannot delete dictionary \"%s\".\n"), dicname );
    ret = -2;   /* 93.03.03 */
    break;
  case MOUNT:
  case TXTBSY:
    (void)fprintf(stderr,gettxt("cannacmd:226", 
	"Dictinary \"%s\" is in use. Cannot delete it.\n"),dicname );
    ret = -2;   /* 93.03.03 */
    break;
  case BADCONT :
    (void)fprintf(stderr, msg_cnt);
    ret = -1;
    break;
  default:
    if (init[0] != '/') {
	(void)fprintf(stderr, msg_abnls, init);
    }
    else {
	(void)fprintf(stderr, msg_abnl);
    }
    ret = -1;
    break;
  }
  }
  else {     /*  PL_DIC  */
  switch (RkRemoveDic(cn, dicname, mode)) {
  case 0:
    (void)fprintf(stderr, gettxt("cannacmd:227", 
	 "Personal learning file \"%s\" is deleted.\n"),dicname );
    ret = 0;
    break;
  case NOENT :
    (void)fprintf(stderr, gettxt("cannacmd:228", 
	 "Personal learning file \"%s\" does not exist.\n"),dicname );
    ret = -2;   /* 93.03.03 */
    break;
  case BADF :
    (void)fprintf(stderr,gettxt("cannacmd:229",
         "\"%s\" is text dictionary. Cannot delete it.\n"),dicname);
    ret = -2;   /* 93.03.03 */
    break;
  case ACCES :
    (void)fprintf(stderr,gettxt("cannacmd:230", 
        "Cannot delete personal learning file \"%s\".\n"),dicname );
    ret = -2;   /* 93.03.03 */
    break;
  case MOUNT:
  case TXTBSY:
    (void)fprintf(stderr,gettxt("cannacmd:231", 
     "Personal learning file \"%s\" is in use. Cannot delete it.\n"),dicname );
    ret = -2;   /* 93.03.03 */
    break;
  case BADCONT :
    (void)fprintf(stderr, msg_cnt);
    ret = -1;
    break;
  default:
    if (init[0] != '/') {
	(void)fprintf(stderr, msg_abnls, init);
    }
    else {
	(void)fprintf(stderr, msg_abnl);
    }
    ret = -1;
    break;
  }
  }        /*  if  end  */
  return ret;
}


#ifndef USE_VARARGS
/* VARARGS */
void
Message(fmt, a, b, c, d, e, f, g, h, i, j)
char *fmt;
/* ARGSUSED *//* d 以降は引数領域確保のためのダミー引数(不要？) */
{
    (void)fprintf(stderr, fmt, a, b, c);
    (void)fprintf(stderr, "\n");
    (void)fflush(stderr);
}

#else /* USE_VARARGS */

#if __STDC__
void
Message(const char *fmt,...)
{
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  (void)fprintf(stderr, "\n");
  (void)fflush(stderr);
}
#else /* !__STDC__ */

#define MAXARGS 5

void
Message(va_alist)
va_dcl
{
  va_list ap;
  char *args[MAXARGS];
  int argno = 0;
  char *fmt;

  va_start(ap);
  fmt = va_arg(ap, char *);

  while (argno < MAXARGS && (args[argno] = va_arg(ap, char *)) != (char *)0) {
    argno++;
  }
  args[MAXARGS - 1] = (char *)0;
  va_end(ap);

  (void)fprintf(stderr, fmt, args[0], args[1], args[2]);
  (void)fprintf(stderr, "\n");
  (void)fflush(stderr);
}
#endif /* !__STDC__ */

#endif /* USE_VARARGS */
