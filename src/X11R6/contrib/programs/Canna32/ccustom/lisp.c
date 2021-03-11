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
static char rcsid[] = "$Id: lisp.c,v 2.4 1994/01/19 02:33:45 hamada Exp $";
#endif

/* 
** main program of lisp 
*/

#include <stdio.h>
#include "lisp.h"

#include "keydef.h"
#include "mfdef.h"
#include "canna.h"
#include "symbolname.h"

extern char *malloc(), *realloc(), *calloc();

static int version = 1 ;
static FILE *outstream = (FILE *)0;

static char *celltop, *cellbtm, *freecell;
static char *memtop;

static initIS(), finIS();
static allocarea(), skipspaces(), zaplin(), prins(), isterm();
static list mkatm(), read1(), ratom(), ratom2(), rstring();
static int tyipeek(), tyi(), tyo();
static void defatms(), epush();
static void push(), pop();
static int  evpsh();
static void freearea(), print();
static list getatm(), getatmz(), newsymbol(), copystring();
static list assq(), pop1();
static list Lnvcons(), Lprogn(), Lcons(), Lread();
static list Leval(), Lprint(), Lmodestr(), Lputd(), Lxcons(), Lncons();
static list NumAcc(), StrAcc();


/* parameter stack */

static list	*stack, *sp;

/* environment stack	*/

static list	*estack, *esp;

/* oblist */

static list	*oblist;	/* oblist hashing array		*/

#define ERROR	-1
static FILE **files;
static int  filep;

/* lisp read buffer & read pointer */

static char *readbuf;		/* read buffer	*/
static char *readptr;		/* read pointer	*/

/* error functions	*/

static void	argnerr(), numerr(), error();

/* multiple values */

#define MAXVALUES 16
static list *values;	/* multiple values here	*/
static int  valuec;	/* number of values here	*/

/* symbols */

static list QUOTE, T, _LAMBDA, _MACRO, COND, USER, BUSHU, RENGO;

#include <setjmp.h>

static struct lispcenv {
  jmp_buf jmp_env;
  int     base_stack;
  int     base_estack;
} *env; /* environment for setjmp & longjmp	*/
static int  jmpenvp = MAX_DEPTH;

/* external functions

   外部関数は以下の３つ

  (1) clisp_init()  --  カスタマイズファイルを読むための準備をする

    lisp の初期化を行い必要なメモリを allocate する。

  (2) clisp_fin()   --  カスタマイズ読み込み用の領域を解放する。

    上記の初期化で得たメモリを解放する。

  (3) LLparse_by_rcfilename((char *)s) -- カスタマイズファイルを読み込む。

    s で指定されたファイル名のカスタマイズファイルを読み込んでカスタ
    マイズの設定を行う。ファイルが存在すれば 1 を返しそうでなければ
    0 を返す。オリジナルのlisp.cではYYparse_by_rcfilenameです。

 */

static void freearea();
#if __STDC__
static list getatmz(char *);
#else
static list getatmz();
#endif

int
clisp_init()
{
  int  i;

  if ( !allocarea() ) {
    return 0;
  }

  if ( !initIS() ) {
    freearea();
    return 0;
  }

  /* stack pointer initialization	*/
  sp = stack + STKSIZE;
  esp = estack + STKSIZE;
  epush(NIL);

  /* initialize read pointer	*/
  readptr = readbuf;
  *readptr = '\0';
  files[filep = 0] = stdin;

  /* oblist initialization	*/
  for (i = 0; i < BUFSIZE ; i++)
    oblist[i] = 0;

  /* symbol definitions */
  defatms();
  return 1;
}

#define UNTYIUNIT 32
static char *untyibuf = 0;
static int untyisize = 0, untyip = 0;

void
clisp_fin()
{
  finIS();

  freearea();
  if (untyisize) {
    free(untyibuf);
    untyisize = 0;
    untyibuf = (char *)0;
  }
}

extern before_parse();

int
LLparse_by_rcfilename(s)
char *s;
{
  extern ckverbose;
  int retval = 0;
  FILE *f;
  FILE *saved_outstream;

  if (jmpenvp <= 0) { /* 再帰が深すぎる場合 */
    return -1;
  }
  jmpenvp--;

  if (ckverbose >= CANNA_HALF_VERBOSE) {
    saved_outstream = outstream;
    outstream = stdout;
  }

  if (f = fopen(s, "r")) {
    if (ckverbose == CANNA_FULL_VERBOSE) {
      printf("カスタマイズファイルとして \"%s\" を用います。\n", s);
    }
    before_parse();
    files[++filep] = f;

    setjmp(env[jmpenvp].jmp_env);
    env[jmpenvp].base_stack = sp - stack;
    env[jmpenvp].base_estack = esp - estack;

    for (;;) {
      push(Lread(0));
      if (valuec > 1 && null(values[1])) {
	break;
      }
      (void)Leval(1);
    }
    fclose(f);
    retval = 1;
  }

  if (ckverbose >= CANNA_HALF_VERBOSE) {
    outstream = saved_outstream;
  }

  jmpenvp++;
  return retval;
}

#define WITH_MAIN
#ifdef WITH_MAIN

#include <signal.h>

static void
intr(sig)
int sig;
/* ARGSUSED */
{
  error("Interrupt:",NON);
  /* NOTREACHED */
}

static void intr();

void
clisp_main()
{
  if (clisp_init() == NULL) {	/* initialize data area	& etc..	*/
    fprintf(stderr, "CannaLisp: initialization failed.\n");
    exit(1);
  }

  if (jmpenvp <= 0) { /* 再帰が深すぎる場合 */
    return;
  }
  jmpenvp--;

  fprintf(stderr,"CannaLisp listener %d \n",version);

  outstream = stdout;

  setjmp(env[jmpenvp].jmp_env);
  env[jmpenvp].base_stack = sp - stack;
  env[jmpenvp].base_estack = esp - estack;

  signal(SIGINT, intr);
  for (;;) {
    prins("-> ");		/* prompt	*/
    push(Lread(0));
    if (valuec > 1 && null(values[1])) {
      break;
    }
    push(Leval(1));
    if (sp[0] == ERROR) {
      (void)pop1();
    }
    else {
      (void)Lprint(1);
      prins("\n");
    }
  }
  jmpenvp++;
  prins("\nGoodbye.\n");
  clisp_fin();
}

#endif /* WITH_MAIN */

static int longestkeywordlen;

typedef struct {
  char *seq;
  int id;
} SeqToID;

static SeqToID keywordtable[] = {
  {"Space"      ,' '},
  {"Escape"     ,'\033'},
  {"Tab"        ,'\t'},
  {"Nfer"       ,CANNA_KEY_Nfer},
  {"Xfer"       ,CANNA_KEY_Xfer},
  {"Backspace"  ,'\b'},
  {"Delete"     ,'\177'},
  {"Insert"     ,CANNA_KEY_Insert},
  {"Rollup"     ,CANNA_KEY_Rollup},
  {"Rolldown"   ,CANNA_KEY_Rolldown},
  {"Up"         ,CANNA_KEY_Up},
  {"Left"       ,CANNA_KEY_Left},
  {"Right"      ,CANNA_KEY_Right},
  {"Down"       ,CANNA_KEY_Down},
  {"Home"       ,CANNA_KEY_Home},
  {"Clear"      ,'\013'},
  {"Help"       ,CANNA_KEY_Help},
  {"Enter"      ,'\n'},
  {"Return"     ,'\r'},
/* "F1" is processed by program */
  {"F2"         ,CANNA_KEY_F2},
  {"F3"         ,CANNA_KEY_F3},
  {"F4"         ,CANNA_KEY_F4},
  {"F5"         ,CANNA_KEY_F5},
  {"F6"         ,CANNA_KEY_F6},
  {"F7"         ,CANNA_KEY_F7},
  {"F8"         ,CANNA_KEY_F8},
  {"F9"         ,CANNA_KEY_F9},
  {"F10"        ,CANNA_KEY_F10},
/* "Pf1" is processed by program */
  {"Pf2"        ,CANNA_KEY_PF2},
  {"Pf3"        ,CANNA_KEY_PF3},
  {"Pf4"        ,CANNA_KEY_PF4},
  {"Pf5"        ,CANNA_KEY_PF5},
  {"Pf6"        ,CANNA_KEY_PF6},
  {"Pf7"        ,CANNA_KEY_PF7},
  {"Pf8"        ,CANNA_KEY_PF8},
  {"Pf9"        ,CANNA_KEY_PF9},
  {"Pf10"       ,CANNA_KEY_PF10},
  {"S-Nfer"     ,CANNA_KEY_Shift_Nfer},
  {"S-Xfer"     ,CANNA_KEY_Shift_Xfer},
  {"S-Up"       ,CANNA_KEY_Shift_Up},
  {"S-Down"     ,CANNA_KEY_Shift_Down},
  {"S-Left"     ,CANNA_KEY_Shift_Left},
  {"S-Right"    ,CANNA_KEY_Shift_Right},
  {"C-Nfer"     ,CANNA_KEY_Cntrl_Nfer},
  {"C-Xfer"     ,CANNA_KEY_Cntrl_Xfer},
  {"C-Up"       ,CANNA_KEY_Cntrl_Up},
  {"C-Down"     ,CANNA_KEY_Cntrl_Down},
  {"C-Left"     ,CANNA_KEY_Cntrl_Left},
  {"C-Right"    ,CANNA_KEY_Cntrl_Right},
  {0            ,0},
};

#define charToNum(c) charToNumTbl[(c) - ' ']

static int *charToNumTbl;

typedef struct {
  int id;
  int *tbl;
} seqlines;

static seqlines *seqTbl;	/* 内部の表(実際には表の表) */
static int nseqtbl;		/* 状態の数。状態の数だけ表がある */
static int nseq;
static int seqline;

static
initIS()
{
  SeqToID *p;
  char *s;
  int i;
  seqlines seqTbls[1024];

  seqTbl = (seqlines *)0;
  seqline = 0;
  nseqtbl = 0;
  nseq = 0;
  longestkeywordlen = 0;
  for (i = 0 ; i < 1024 ; i++) {
    seqTbls[i].tbl = (int *)0;
    seqTbls[i].id = 0;
  }
  charToNumTbl = (int *)calloc('~' - ' ' + 1, sizeof(int));
  if ( !charToNumTbl ) {
    return 0;
  }

  /* まず何文字使われているかを調べる。
     nseq は使われている文字数より１大きい値である */
  for (p = keywordtable ; p->id ; p++) {
    int len = 0;
    for (s = p->seq ; *s ; s++) {
      if ( !charToNumTbl[*s - ' '] ) {
	charToNumTbl[*s - ' '] = nseq; /* 各文字にシリアル番号を振る */
	nseq++;
      }
      len ++;
    }
    if (len > longestkeywordlen) {
      longestkeywordlen = len;
    }
  }
  /* 文字数分のテーブル */
  seqTbls[nseqtbl].tbl = (int *)calloc(nseq, sizeof(int));
  if ( !seqTbls[nseqtbl].tbl ) {
    goto initISerr;
  }
  nseqtbl++;
  for (p = keywordtable ; p->id ; p++) {
    int line, nextline;
    
    line = 0;
    for (s = p->seq ; *s ; s++) {
      if (seqTbls[line].tbl == 0) { /* テーブルがない */
	seqTbls[line].tbl = (int *)calloc(nseq, sizeof(int));
	if ( !seqTbls[line].tbl ) {
	  goto initISerr;
	}
      }
      nextline = seqTbls[line].tbl[charToNum(*s)];
      /* ちなみに、charToNum(*s) は絶対に０にならない */
      if ( nextline ) {
	line = nextline;
      }
      else { /* 最初にアクセスした */
	line = seqTbls[line].tbl[charToNum(*s)] = nseqtbl++;
      }
    }
    seqTbls[line].id = p->id;
  }
  seqTbl = (seqlines *)calloc(nseqtbl, sizeof(seqlines));
  if ( !seqTbl ) {
    goto initISerr;
  }
  for (i = 0 ; i < nseqtbl ; i++) {
    seqTbl[i].id  = seqTbls[i].id;
    seqTbl[i].tbl = seqTbls[i].tbl;
  }
  return 1;

 initISerr:
  free(charToNumTbl);
  charToNumTbl = (int *)0;
  if (seqTbl) {
    free(seqTbl);
    seqTbl = (seqlines *)0;
  }
  for (i = 0 ; i < nseqtbl ; i++) {
    if (seqTbls[i].tbl) {
      free(seqTbls[i].tbl);
      seqTbls[i].tbl = (int *)0;
    }
  }
  return 0;
}

static
finIS() /* identifySequence に用いたメモリ資源を開放する */
{
  int i;

  for (i = 0 ; i < nseqtbl ; i++) {
    if (seqTbl[i].tbl) free(seqTbl[i].tbl);
    seqTbl[i].tbl = (int *)0;
  }
  free(seqTbl);
  seqTbl = (seqlines *)0;
  free(charToNumTbl);
  charToNumTbl = (int *)0;
}

/* cvariable

  seqline: identifySequence での状態を保持する変数

 */

#define CONTINUE 1
#define END	 0

static
identifySequence(c, val)
char c;
int *val;
{
  int nextline;

  if (charToNum(c) &&
      (nextline = seqTbl[seqline].tbl[charToNum(c)]) ) {
    seqline = nextline;
    if (*val = seqTbl[seqline].id) {
      seqline = 0;
      return END;
    }
    else {
      return CONTINUE; /* continue */
    }
  }
  else {
    *val = -1;
    seqline = 0;
    return END;
  }
}


static int
alloccell()
{
  POINTERINT  cellsize, odd;
  char *p;

  cellsize = CELLSIZE * sizeof(list);
  p = malloc(cellsize);
  if (p == (char *)0) {
    return 0;
  }
  memtop = p;
  odd = (pointerint)memtop % sizeof(list);
  freecell = celltop = memtop + (odd ? sizeof(list) - odd : 0);
  cellbtm = memtop + cellsize - odd;
  return 1;
}

/* うまく行かなかったら０を返す */

static
allocarea()
{
  /* まずはセル領域 */
  if ( !alloccell() ) {
    return 0;
  }

  /* スタック領域 */
  stack = (list *)calloc(STKSIZE, sizeof(list));
  if ( !stack ) {
    free(memtop);
    return 0;
  }
  estack = (list *)calloc(STKSIZE, sizeof(list));
  if ( !estack ) {
    free(memtop); free(stack);
    return 0;
  }

  /* oblist */
  oblist = (list *)calloc(BUFSIZE, sizeof(list));
  if ( !oblist ) {
    free(memtop); free(stack); free(estack);
    return 0;
  }

  /* I/O */
  files = (FILE **)calloc(MAX_DEPTH, sizeof(FILE *));
  filep = 0;
  if ( !files ) {
    free(memtop); free(stack); free(estack); free(oblist);
    return 0;
  }
  readbuf = malloc(BUFSIZE);
  if ( !readbuf ) {
    free(memtop); free(stack); free(estack); free(oblist); free(files);
    return 0;
  }

  /* jump env */
  env = (struct lispcenv *)calloc(MAX_DEPTH, sizeof(struct lispcenv));
  jmpenvp = MAX_DEPTH;
  if ( !env ) {
    free(memtop); free(stack); free(estack); free(oblist); free(files);
    free(readbuf);
    return 0;
  }

  /* multiple values returning buffer */
  values = (list *)calloc(MAXVALUES, sizeof(list));
  valuec = 1;
  if ( !values ) {
    free(memtop); free(stack); free(estack); free(oblist); free(files);
    free(readbuf); free(env);
    return 0;
  }

  return 1;
}

static void
freearea()
{
  free((char *)memtop);
  free((char *)stack);
  free((char *)estack);
  free((char *)oblist);
  free((char *)files);
  free((char *)env);
  free((char *)readbuf);
  if (values) {
    free(values);
    values = 0;
  }
}

static list
getatmz(name)
char *name;
{
  int  key;
  char *p;

  for (p = name, key = 0 ; *p ; p++)
    key += *p;
  return getatm(name,key);
}

/* mkatm -
	making symbol function	*/

static list 
mkatm(name)
char *name;
{
  list temp;
  struct atomcell *newatom;

  temp = newsymbol(name);
  newatom = symbolpointer(temp);
  newatom->value = (*name == ':') ? (list)temp : (list)UNBOUND;
  newatom->plist = NIL;			/* set null plist	*/
  newatom->ftype = UNDEF;		/* set undef func-type	*/
  newatom->func  = (list (*)())0;	/* Don't kill this line	*/
  newatom->valfunc  = (list (*)())0;	/* Don't kill this line	*/
  newatom->hlink = NIL;		/* no hash linking	*/
  newatom->mid = -1;
  newatom->fid = -1;

  return temp;
}

/* getatm -- get atom from the oblist if possible	*/

static list 
getatm(name,key)
char *name;
int  key;
{
  list p;
  struct atomcell *atomp;

  key &= 0x00ff;
  for (p = oblist[key] ; p ;) {
    atomp = symbolpointer(p);
    if (!strcmp(atomp->pname, name)) {
      return p;
    }
    p = atomp->hlink;
  }
  p = mkatm(name);
  atomp = symbolpointer(p);
  atomp->hlink = oblist[key];
  oblist[key] = p;
  return p;
}

static void
error(msg,v)
char *msg;
list v;
/* ARGSUSED */
{
  void print();

  prins(msg);
  if (v != (list)NON)
    print(v);
  prins("\n");
  sp = &stack[env[jmpenvp].base_stack];
  esp = &estack[env[jmpenvp].base_estack];
/*  epush(NIL); */
  longjmp(env[jmpenvp].jmp_env,YES);
}

static void
argnerr(msg)
char *msg;
{
  prins("incorrect number of args to ");
  error(msg, NON);
}

static void
numerr(fn,arg)
char *fn;
list arg;
{
  prins("Non-number ");
  if (fn) {
    prins("to ");
    prins(fn);
  }
  error(": ",arg);
}

static void
strerr(fn,arg)
char *fn;
list arg;
{
  prins("Non-string ");
  if (fn) {
    prins("to ");
    prins(fn);
  }
  error(": ",arg);
}

static void
argerr(fn,arg)
char *fn;
list arg;
{
  prins("Bad arg to ");
  prins(fn);
  error("  ",arg);
}

static list
Lread(n)
int n;
{
  list t, read1();
  void push();

  argnchk("read",0);
  valuec = 1;
  if ((t = read1()) == (list)ERROR) {
    readptr = readbuf;
    *readptr = '\0';
    if (files[filep] != stdin) {
      fclose(files[filep--]);
    }
    values[0] = NIL;
    values[1] = NIL;
    valuec = 2;
    return(NIL);
  }
  else {
    values[0] = t;
    values[1] = T;
    valuec = 2;
    return(t);
  }
  /* NOTREACHED */
}

static void untyi();
static list rcharacter();

static list
read1()
{
  int  c, tyi(), tyipeek();
  list p, *pp, Lncons();
  list t;
  char *eofmsg = "EOF hit in reading a list : ";

 lab:
  if ( !skipspaces() ) {
    return((list)ERROR);
  }
  switch (c = tyi()) {
  case '(':
    push(NIL);
    p = Lncons(1);	/* get a new cell	*/
    car(p) = p;
    push(p);
    pp = sp;
    
    for (;;) {
    lab2:
      if ( !skipspaces() )
	error(eofmsg,cdr(*pp));
      switch (c = tyi()) {
      case ';':
	zaplin();
	goto lab2;
      case ')':
	return(cdr(pop1()));
      case '.':
	if ( !(c = tyipeek()) )
	  error(eofmsg,cdr(*pp));
	else if ( !isterm(c) ) {
	  push(ratom2('.'));
	  push(NIL);
	  car(*pp) = cdar(*pp) = Lcons(2);
	  break;
	}
	else {
	  cdar(*pp) = read1();
	  if (cdar(*pp) == (list)ERROR)
	    error(eofmsg,cdr(*pp));
	  while (')' != (c = tyi()))
	    if ( !c )
	      error(eofmsg,cdr(*pp));
	  return(cdr(pop1()));
	}
      default:
	untyi(c);
	if ((t = read1()) == (list)ERROR)
	  error(eofmsg,cdr(*pp));
	push(t);
	push(NIL);
	car(*pp) = cdar(*pp) = Lcons(2);
      }
    }
  case '\'':
    push(QUOTE);
    if ((t = read1()) == (list)ERROR)
      error(eofmsg,NIL);
    push(t);
    push(NIL);
    push(Lcons(2));
    return Lcons(2);
  case '"':
    return rstring();
  case '?':
    return rcharacter();
  case ';':
    zaplin();
    goto lab;
  default:
    untyi(c);
    return ratom();
  }
}

/* skipping spaces function -
	if eof read then return NO	*/

static
skipspaces()
{
  int c, tyi();

  while ((c = tyi()) <= ' ') {
    if ( !c )
      return(NO);
  }
  untyi(c);
  return(YES);
}

/* skip reading until '\n' -
	if eof read then return NO	*/

static
zaplin()
{
	int c, tyi();

	while ((c = tyi()) != '\n')
		if ( !c )
			return(NO);
	return(YES);
}

static void gc();

static list
newcons()
{
  list retval;

  if (freecell + sizeof(struct cell) >= cellbtm) {
    gc();
  }
  retval = CONS_TAG | (freecell - celltop);
  freecell += sizeof(struct cell);
  return retval;
}

static list
newsymbol(name)
char *name;
{
  list retval;
  struct atomcell *temp;
  int namesize;

  namesize = strlen(name);
  namesize = (namesize / sizeof(list) + 1) * sizeof(list); /* +1は'\0'の分 */
  if (freecell + sizeof(struct atomcell) + namesize >= cellbtm) {
    gc();
  }
  temp = (struct atomcell *)freecell;
  retval = SYMBOL_TAG | (freecell - celltop);
  freecell += sizeof(struct atomcell);
  (void)strcpy(freecell, name);
  temp->pname = freecell;
  freecell += namesize;
  
  return retval;
}

static void patom();

static void
print(l)
list l;
{
	if ( !l )	/* case NIL	*/
		prins("nil");
	else if (atom(l))
		patom(l);
	else {
		tyo('(');
		print(car(l));
		for (l = cdr(l) ; l ; l = cdr(l)) {
			tyo(' ');
			if (atom(l)) {
				tyo('.');
				tyo(' ');
				patom(l);
				break;
			}
			else 
				print(car(l));
		}
		tyo(')');
	}
}



/*
** read atom
*/


static list 
ratom()
{
	list ratom2();

	return(ratom2(tyi()));
}

/* read atom with the first one character -
	check if the token is numeric or pure symbol & return proper value */

static isnum();

static list 
ratom2(a)
int  a;
{
  list mkatm(), getatmz();
  int  i, c, flag;
  char atmbuf[BUFSIZE];

  flag = NO;
  if (a == '\\') {
    flag = YES;
    a = tyi();
  }
  atmbuf[0] = a;
  for (i = 1, c = tyi(); !isterm(c) ; i++, c = tyi()) {
    if ( !c ) {
      error("Eof hit in reading symbol.", NON);
    }
    if (c == '\\') {
      flag = YES;
    }
    atmbuf[i] = c;
  }
  untyi(c);
  atmbuf[i] = '\0';
  if ( !flag && isnum(atmbuf)) {
    return(mknum(atoi(atmbuf)));
  }
  else if ( !flag && !strcmp("nil",atmbuf) ) {
    return(NIL);
  }
  else {
    return (getatmz(atmbuf));
  }
}

static list
rstring()
{
  char strb[BUFSIZE], c;
  int strp = 0;

  while ((c = tyi()) != '"') {
    if ( !c ) {
      error("Eof hit in reading a string.", NON);
    }
    if (strp < BUFSIZE) {
      if (c == '\\') {
	untyi(c);
	c = (char)(((unsigned POINTERINT)rcharacter()) & 0xff);
      }
      strb[strp++] = c;
    }
    else {
      error("Too long string read.", NON);
    }
  }
  strb[strp] = '\0';
  return copystring(strb, strp);
}

static list
rcharacter()
{
  char *tempbuf, ch;
  list retval;
  int bufp;

  tempbuf = malloc(longestkeywordlen + 1);
  if ( !tempbuf ) {
    error("read: malloc failed in reading character.", NON);
  }
  bufp = 0;

  ch = tyi();
  if (ch == '\\') {
    int code, res;

    do { /* キーワードと照合する */
      tempbuf[bufp++] = ch = tyi();
      res = identifySequence(ch, &code);
    } while (res == CONTINUE);
    if (code != -1) { /* キーワードと一致した。 */
      retval = mknum(code);
    }
    else if (bufp > 2 && tempbuf[0] == 'C' && tempbuf[1] == '-') {
      while (bufp > 3) {
	untyi(tempbuf[--bufp]);
      }
      return mknum(tempbuf[2] & (' ' - 1));
    }
    else if (bufp == 3 && tempbuf[0] == 'F' && tempbuf[1] == '1') {
      untyi(tempbuf[2]);
      return CANNA_KEY_F1;
    }
    else if (bufp == 4 && tempbuf[0] == 'P' && tempbuf[1] == 'f' &&
	     tempbuf[2] == '1') {
      untyi(tempbuf[3]);
      return CANNA_KEY_PF1;
    }
    else { /* 全然駄目 */
      while (bufp > 1) {
	untyi(tempbuf[--bufp]);
      }
      return mknum(tempbuf[0]);
    }
  }
  else {
    retval = mknum(ch);
  }

  free(tempbuf);
  return retval;
}

static isnum(name)
char *name;
{
	if (*name == '-') {
		name++;
		if ( !*name )
			return(NO);
	}
	for(; *name ; name++) {
		if (*name < '0' || '9' < *name) {
			if (*name != '.' || *(name + 1)) {
				return(NO);
			}
		}
	}
	return(YES);
}

/* tyi -- input one character from buffered stream	*/

static void
untyi(c)
char c;
{
  if (readbuf < readptr) {
    *--readptr = c;
  }
  else {
    if (untyip >= untyisize) {
      if (untyisize == 0) {
	untyibuf = malloc(UNTYIUNIT);
	if (untyibuf) {
	  untyisize = UNTYIUNIT;
	}
      }
      else {
	untyibuf = realloc(untyibuf, UNTYIUNIT + untyisize);
	if (untyibuf) {
	  untyisize += UNTYIUNIT;
	}
      }
    }
    if (untyip < untyisize) { /* それでもチェックする */
      untyibuf[untyip++] = c;
    }
  }
}

static int
tyi()
{
  char *gets(), *fgets();

  if (untyibuf) {
    int ret = untyibuf[--untyip];
    if (untyip == 0) {
      free(untyibuf);
      untyibuf = (char *)0;
      untyisize = 0;
    }
    return ret;
  }

  if (readptr && *readptr) {
    return ((int)(unsigned char)*readptr++);
  }
  else if (files[filep] == stdin) {
    int len;

    readptr = gets(readbuf);
    if ( !readptr ) {
      return NO;
    }
    else {
      len = strlen(readptr);
      readptr[len] = '\n';
      readptr[len + 1] = '\0';
      return tyi();
    }
  }
  else {
    readptr = fgets(readbuf,BUFSIZE,files[filep]);
    if (readptr) {
      return(tyi());
    }
    else {
      return(NO);
    }
  }
  /* NOTREACHED */
}

/* tyipeek -- input one character without advance the read pointer	*/

static int
tyipeek()
{
  int c = tyi();
  untyi(c);
  return c;
}

/* tyo -- output one character	*/

static tyo(c)
char c;
{
  if (outstream) {
    (void)putc(c, outstream);
  }
}
	

/* prins -
	print string	*/

static prins(s)
char *s;
{
	while (*s) {
		tyo(*s++);
	}
}


/* isterm -
	check if the character is terminating the lisp expression	*/

static isterm(c)
int  c;
{
	if (c <= ' ')
		return(YES);
	else {
		switch (c)
		{
		case '(':
		case ')':
		case ';':
			return(YES);
		default:
			return(NO);
		}
	}
}

/* push down an S-expression to parameter stack	*/

static void
push(value)
list value;
{
  if (sp <= stack) {
    error("stack over flow",NON);
  }
  else
    *--sp = value;
}

/* pop up n S-expressions from parameter stack	*/

static void 
pop(x)
int  x;
{
  if (0 < x && sp >= &stack[STKSIZE]) {
    error("stack under flow",NON);
  }
  sp += x;
}

/* pop up an S-expression from parameter stack	*/

static list 
pop1()
{
  if (sp >= &stack[STKSIZE]) {
    error("stack under flow",NON);
  }
  else
    return(*sp++);
}

static void
epush(value)
list value;
{
  if (esp <= estack) {
    error("estack over flow",NON);
  }
  else
    *--esp = value;
}

static list 
epop()
{
  if (esp >= &estack[STKSIZE]) {
    error("lstack under flow",NON);
  }
  else
    return(*esp++);
}


/*
** output function for lisp S-Expression
*/


/*
**  print atom function
**  please make sure it is an atom (not list)
**  no check is done here.
*/

static void
patom(atm)
list atm;
{
  char namebuf[BUFSIZE];

  if (constp(atm)) {
    if (numberp(atm)) {
      (void)sprintf(namebuf,"%d",xnum(atm));
      prins(namebuf);
    }
    else {		/* this is a string */
      int i, len = xstrlen(atm);
      char *s = xstring(atm);

      tyo('"');
      for (i = 0 ; i < len ; i++) {
	tyo(s[i]);
      }
      tyo('"');
    }
  }
  else {
    prins(symbolpointer(atm)->pname);
  }
}

static void markcopycell();

char *oldcelltop;
static char *oldcellp;

#define oldpointer(x) (oldcelltop + celloffset(x))

static void
gc() /* コピー方式のガーベジコレクションである */
{
  int i;
  list *p;

  oldcellp = memtop; oldcelltop = celltop;

  if ( !alloccell() ) {
    error("GC: failed in allocating new cell area.", NON);
  }

  for (i = 0 ; i < BUFSIZE ; i++) {
    markcopycell(oblist + i);
  }
  for (p = sp ; p < &stack[STKSIZE] ; p++) {
    markcopycell(p);
  }
  for (p = esp ; p < &estack[STKSIZE] ; p++) {
    markcopycell(p);
  }
  for (i = 0 ; i < valuec ; i++) {
    markcopycell(values + i);
  }
  markcopycell(&T);
  markcopycell(&QUOTE);
  markcopycell(&_LAMBDA);
  markcopycell(&_MACRO);
  markcopycell(&COND);
  markcopycell(&USER);
  markcopycell(&BUSHU);
  markcopycell(&RENGO);
  free(oldcellp);
}

static char *Strncpy();

static list
copystring(s, n)
char *s;
int n;
{
  int namesize;
  list retval;
  char *Strncpy();

  namesize = ((n + sizeof(pointerint) + 1 + 3)/ sizeof(list)) * sizeof(list);
  if (freecell + namesize >= cellbtm) { /* gc 中は起こり得ないはず */
    gc();
  }
  ((struct stringcell *)freecell)->length = n;
  (void *)Strncpy(((struct stringcell *)freecell)->str, s, n);
  ((struct stringcell *)freecell)->str[n] = '\0';
  retval = STRING_TAG | (freecell - celltop);
  freecell += namesize;
  return retval;
}

static list
copycons(l)
struct cell *l;
{
  list newcell;

  newcell = newcons();
  car(newcell) = l->head;
  cdr(newcell) = l->tail;
  return newcell;
}

static void
markcopycell(addr)
list *addr;
{
  list temp;
 redo:
  if (null(*addr) || numberp(*addr)) {
    return;
  }
  else if (alreadycopied(oldpointer(*addr))) {
    *addr = newaddr(gcfield(oldpointer(*addr)));
    return;
  }
  else if (stringp(*addr)) {
    temp = copystring(((struct stringcell *)oldpointer(*addr))->str,
		      ((struct stringcell *)oldpointer(*addr))->length);
    gcfield(oldpointer(*addr)) = mkcopied(temp);
    *addr = temp;
    return;
  }
  else if (consp(*addr)) {
    temp = copycons((struct cell *)(oldpointer(*addr)));
    gcfield(oldpointer(*addr)) = mkcopied(temp);
    *addr = temp;
    markcopycell(&car(temp));
    addr = &cdr(temp);
    goto redo;
  }
  else { /* symbol */
    struct atomcell *newatom, *oldatom;

    oldatom = (struct atomcell *)(oldpointer(*addr));
    temp = newsymbol(oldatom->pname);
    newatom = symbolpointer(temp);
    newatom->value = oldatom->value;
    newatom->plist = oldatom->plist;
    newatom->ftype = oldatom->ftype;
    newatom->func  = oldatom->func;
    newatom->fid   = oldatom->fid;
    newatom->mid   = oldatom->mid;
    newatom->valfunc  = oldatom->valfunc;
    newatom->hlink = oldatom->hlink;

    gcfield(oldpointer(*addr)) = mkcopied(temp);
    *addr = temp;

    if (newatom->value != (list)UNBOUND) {
      markcopycell(&newatom->value);
    }
    markcopycell(&newatom->plist);
    if (newatom->ftype == EXPR || newatom->ftype == MACRO) {
      markcopycell(&newatom->func);
    }
    addr = &newatom->hlink;
    goto redo;
  }
}

static list
bindall(var,par,a,e)
list var, par, a, e;
{
  list *pa, *pe, retval;

  push(a); pa = sp;
  push(e); pe = sp;
 retry:
  if (constp(var)) {
    pop(2);
    return(*pa);
  }
  else if (atom(var)) {
    push(var);
    push(par);
    push(Lcons(2));
    push(*pa);
    retval = Lcons(2);
    pop(2);
    return retval;
  }
  else if (atom(par)) {
    error("Bad macro form ",e);
  }
  else {
    push(par);
    push(var);
    *pa = bindall(car(var),car(par),*pa,*pe);
    var = cdr(pop1());
    par = cdr(pop1());
    goto retry;
  }
}

static list
Lquote()
{
	list p;

	p = pop1();
	if (atom(p))
		return(NIL);
	else
		return(car(p));
}

static list
Leval(n)
int n;
{
  list e, a, t, s, tmp, aa, *pe, *pt, *ps, *paa;
  list fn, (*cfn)(), *pfn;
  int i, j;
  argnchk("eval",1);
  e = sp[0];
  pe = sp;
  if (atom(e)) {
    if (constp(e)) {
      pop1();
      return(e);
    }
    else {
      struct atomcell *sym;

      if (t = assq(e, *esp)) {
	(void)pop1();
	return(cdr(t));
      }
      else if ((sym = symbolpointer(e))->valfunc) {
	(void)pop1();
	return (sym->valfunc)(VALGET, 0);
      }
      else {
	if ((t = (sym->value)) != (list)UNBOUND) {
	  pop1();
	  return(t);
	}
	else {
	  error("Unbound Variable: ",*pe);
	}
      }
    }
  }
  else if (constp((fn = car(e)))) {	/* not atom	*/
    error("eval: Undefined function ", fn);
  }
  else if (atom(fn)) {
    switch (symbolpointer(fn)->ftype) {
    case UNDEF:
      error("eval: Undefined function ", fn);
      break;
    case SUBR:
      cfn = symbolpointer(fn)->func;
      i = evpsh(cdr(e));
      epush(NIL);
      t = (*cfn)(i);
      epop();
      pop1();
      return (t);
    case SPECIAL:
      push(cdr(e));
      t = (*(symbolpointer(fn)->func))();
      pop1();
      return (t);
    case EXPR:
      fn = (list)(symbolpointer(fn)->func);
      aa = NIL; /* previous env won't be used */
    expr:
      if (atom(fn) || car(fn) != _LAMBDA || atom(cdr(fn))) {
	error("eval: bad lambda form ", fn);
      }
/* Lambda binding begins here ...					*/
      s = cdr(e);		/* actual parameter	*/
      t = cadr(fn);		/* lambda list		*/
      push(s); ps = sp;
      push(t); pt = sp;
      push(fn); pfn = sp;
      push(aa); paa = sp;
      i = 0;			/* count of variables	*/
      for (; consp(*ps) && consp(*pt) ; *ps = cdr(*ps), *pt = cdr(*pt)) {
	if (consp(car(*pt))) {
	  tmp = cdar(*pt);	/* push the cdr of element */
	  if (atom(tmp) || null(cdr(tmp)))
	    ;
	  else {
	    push(cdr(tmp));
	    push(T);
	    push(Lcons(2));
	    i++;
	  }
	  push(caar(*pt));
	}
	else {
	  push(car(*pt));
	}
	push(car(*ps));
	push(Leval(1));
	push(Lcons(2));
	i++;
      }
      for (; consp(*pt) ; *pt = cdr(*pt)) {
	if (atom(car(*pt)))
	  error("too few actual parameters ",*pe);
	else {
	  tmp = cdar(*pt);
	  if (atom(tmp) || null(cdr(tmp)))
	    ;
	  else {
	    push(cdr(tmp));
	    push(NIL);
	    push(Lcons(2));
	    i++;
	  }
	  push(caar(*pt));
	  tmp = cdar(*pt); /* restore for GC */
	  if (atom(tmp))
	    push(NIL);
	  else {
	    push(car(tmp));
	    push(Leval(1));
	  }
	  push(Lcons(2));
	  i++;
	}
      }
      if (null(*pt) && consp(*ps)) {
	error("too many actual arguments ",*pe);
      }
      else if (*pt) {
	push(*pt);
	for (j = 1 ; consp(*ps) ; j++) {
	  push(car(*ps));
	  push(Leval(1));
	  *ps = cdr(*ps);
	}
	push(NIL);
	for (; j ; j--) {
	  push(Lcons(2));
	}
	i++;
      }
      push(*paa);
      for (; i ; i--) {
	push(Lcons(2));
      }
/* Lambda binding finished, and a new environment is established.	*/
      epush(pop1());	/* set the new environment	*/
      push(cddr(*pfn));
      t = Lprogn();
      epop();
      pop(5);
      return (t);
    case MACRO:
      fn = (list)(symbolpointer(fn)->func);
      if (atom(fn) || car(fn) != _MACRO || atom(cdr(fn))) {
	error("eval: bad macro form ",fn);
      }
      s = cdr(e);	/* actual parameter	*/
      t = cadr(fn);	/* lambda list	*/
      push(fn);
      epush(bindall(t,s,NIL,e));
      push(cddr(pop1()));
      t = Lprogn();
      epop();
      push(t);
      push(t);
      s = Leval(1);
      if (atom(t = pop1()))
	;
      else {
	car(*pe) = car(t);
	cdr(*pe) = cdr(t);
      }
      pop1();
      return (s);
    case CMACRO:
      push(e);
      push(t = (*(symbolpointer(fn)->func))());
      push(t);
      s = Leval(1);
      if (atom(t = pop1()))
	;
      else {
	car(e) = car(t);
	cdr(e) = cdr(t);
      }
      pop1();
      return (s);
    default:
      error("eval: Unrecognized ftype used in ", fn);
      break;
    }
  }
  else {	/* fn is list (lambda expression)	*/
    aa = *esp; /* previous environment is also used */
    goto expr;
  }
}

static list
assq(e,a)
list e, a;
{
  list i;

  for (i = a ; i ; i = cdr(i)) {
    if (consp(car(i)) && e == caar(i)) {
      return(car(i));
    }
  }
  return((list)NIL);
}

/* eval each argument and push down each value to parameter stack	*/

static int
evpsh(args)
list args;
{
  int  counter;
  list temp;

  counter = 0;
  while (consp(args)) {
    push(args);
    push(car(args));
    temp = Leval(1);
    args = cdr(pop1());
    counter++;
    push(temp);
  }
  return (counter);
}

int
psh(args)
list args;
{
  int  counter;

  counter = 0;
  while (consp(args)) {
    push(car(args));
    counter++;
    args = cdr(args);
  }
  return (counter);
}

static list
Lprogn()
{
  list val, *pf;

  val = NIL;
  pf = sp;
  for (; consp(*pf) ; *pf = cdr(*pf)) {
    symbolpointer(T)->value = T;
    push(car(*pf));
    val = Leval(1);
  }
  pop1();
  return (val);
}

static list
Lcons(n)
int n;
{
	list temp;
	void gc();

	argnchk("cons",2);
	temp = newcons();
	cdr(temp) = pop1();
	car(temp) = pop1();
	return(temp);
}

static list 
Lncons(n)
int n;
{
	list temp;
	void gc();

	argnchk("ncons",1);
	temp = newcons();
	car(temp) = pop1();
	cdr(temp) = NIL;
	return(temp);
}

static list
Lxcons(n)
int n;
{
	list temp;
	void gc();

	argnchk("cons",2);
	temp = newcons();
	car(temp) = pop1();
	cdr(temp) = pop1();
	return(temp);
}

static list 
Lprint(n)
int n;
{
	void print();

	print(sp[0]);
	pop(n);
	return (T);
}

static list
Lset(n)
int n;
{
  list val, a, t, assq();
  list var;
  struct atomcell *sym;

  argnchk("set",2);
  val = pop1();
  var = pop1();
  if (!symbolp(var))
    error("set/setq: bad variable type  ",var);
  sym = symbolpointer(var);
  if (t = assq(var,*esp)) {
    return cdr(t) = val;
  }
  else if (sym->valfunc) {
    return (*(sym->valfunc))(VALSET, val);
  }
  else {
    return sym->value = val;	/* global set	*/
  }
}

static list
Lsetq()
{
  list a, *pp, Leval(), Lset();

  a = NIL;
  for (pp = sp; consp(*pp) ; *pp = cdr(*pp)) {
    push(car(*pp));
    *pp = cdr(*pp);
    if ( atom(*pp) )
      error("odd number of args to setq",NON);
    push(car(*pp));
    push(Leval(1));
    a = Lset(2);
  }
  pop1();
  return(a);
}

static int equal();

static list 
Lequal(n)
int n;
{
  argnchk("equal (=)",2);
  if (equal(pop1(),pop1()))
    return(T);
  else
    return(NIL);
}

/* null 文字で終わらない strncmp */

static int
Strncmp(x, y, len)
char *x, *y;
int len;
{
  int i;

  for (i = 0 ; i < len ; i++) {
    if (x[i] != y[i]) {
      return (x[i] - y[i]);
    }
  }
  return 0;
}

/* null 文字で終わらない strncpy */

static char *
Strncpy(x, y, len)
char *x, *y;
int len;
{
  int i;

  for (i = 0 ; i < len ; i++) {
    x[i] = y[i];
  }
  return x;
}

static int
equal(x,y)
list x, y;
{
 equaltop:
  if (x == y)
    return(YES);
  else if (null(x) || null(y))
    return(NO);
  else if (numberp(x) || numberp(y)) {
    return NO;
  }
  else if (stringp(x)) {
    if (stringp(y)) {
      return ((xstrlen(x) == xstrlen(y)) ?
	      (!Strncmp(xstring(x), xstring(y), xstrlen(x))) : 0);
    }
    else {
      return NO;
    }
  }
  else if (symbolp(x) || symbolp(y)) {
    return(NO);
  }
  else {
    if (equal(car(x), car(y))) {
      x = cdr(x);
      y = cdr(y);
      goto equaltop;
    }
    else 
      return(NO);
  }
}

static list 
Lgreaterp(n)
int n;
{
  list p;
  pointerint x, y;

  if ( !n )
    return(T);
  else {
    p = pop1();
    if (!numberp(p))
      numerr("greaterp",p);
    x = xnum(p);
    for (n-- ; n ; n--) {
      p = pop1();
      if (!numberp(p))
	numerr("greaterp",p);
      y = xnum(p);
      if (y <= x)		/* !(y > x)	*/
	return(NIL);
      x = y;
    }
    return(T);
  }
}

static list 
Llessp(n)
int n;
{
  list p;
  pointerint x, y;

  if ( !n )
    return(T);
  else {
    p = pop1();
    if (!numberp(p))
      numerr("lessp",p);
    x = xnum(p);
    for (n-- ; n ; n--) {
      p = pop1();
      if (!numberp(p))
	numerr("lessp",p);
      y = xnum(p);
      if (y >= x)		/* !(y < x)	*/
	return(NIL);
      x = y;
    }
    return(T);
  }
}

static list
Leq(n)
int n;
{
  list f;

  argnchk("eq",2);
  f = pop1();
  if (f == pop1())
    return(T);
  else
    return(NIL);
}

static list
Lcond()
{
  list *pp, t, a, e, c, Lprogn(), Leval();

  pp = sp;
  for (; consp(*pp) ; *pp = cdr(*pp)) {
    t = car(*pp);
    if (atom(t)) {
      pop1();
      return (NIL);
    }
    else {
      push(cdr(t));
      if ((c = car(t)) == T || (push(c), (a = Leval(1)))) {
	/* if non NIL */
	t = pop1();
	if (null(t)) {	/* if cdr is NIL */
	  (void)pop1();
	  return (a);
	}
	else {
	  (void)pop1();
	  push(t);
	  return(Lprogn());
	}
      }
      else {
	(void)pop1();
      }
    }
  }
  pop1();
  return (NIL);
}

static list
Lnull(n)
int n;
{
  argnchk("null",1);
  if (pop1())
    return NIL;
  else
    return T;
}

static list 
Lor()
{
  list *pp, a, t;

  for (pp = sp; consp(*pp) ; *pp = cdr(*pp)) {
    push(car(*pp));
    if (t = Leval(1)) {
      pop1();
      return(t);
    }
  }
  pop1();
  return(NIL);
}

static list 
Land()
{
  list *pp, a, t;

  t = T;
  for (pp = sp; consp(*pp) ; *pp = cdr(*pp)) {
    push(car(*pp));
    if ( !(t = Leval(1)) ) {
      pop1();
      return(NIL);
    }
  }
  pop1();
  return(t);
}

static list 
Lplus(n)
int n;
{
  list t;
  int  i;
  pointerint sum;

  i = n;
  sum = 0;
  while (i--) {
    t = sp[i];
    if ( !numberp(t) ) {
      numerr("+",t);
    }
    else {
      sum += xnum(t);
    }
  }
  pop(n);
  return(mknum(sum));
}

static list
Ltimes(n)
int n;
{
  list t;
  int  i;
  pointerint sum;

  i = n;
  sum = 1;
  while (i--) {
    t = sp[i];
    if ( !numberp(t) )
      numerr("*",t);
    else
      sum *= xnum(t);
  }
  pop(n);
  return(mknum(sum));
}

static list
Ldiff(n)
int n;
{
  list t;
  int  i;
  pointerint sum;

  if ( !n )
    return(mknum(0));
  t = sp[n - 1];
  if ( !numberp(t) )
    numerr("-",t);
  sum = xnum(t);
  if (n == 1) {
    pop1();
    return(mknum(-sum));
  }
  else {
    i = n - 1;
    while (i--) {
      t = sp[i];
      if ( !numberp(t) )
	numerr("-",t);
      else
	sum -= xnum(t);
    }
    pop(n);
    return(mknum(sum));
  }
}

static list 
Lquo(n)
int n;
{
  list t;
  int  i;
  pointerint sum;

  if ( !n )
    return(mknum(1));
  t = sp[n - 1];
  if ( !numberp(t) )
    numerr("/",t);
  sum = xnum(t);
  i = n - 1;
  while (i--) {
    t = sp[i];
    if ( !numberp(t) )
      numerr("/",t);
    else
      sum = sum / (long)xnum(t);	/* CP/M68K is bad...	*/
  }
  pop(n);
  return(mknum(sum));
}

static list 
Lrem(n)
int n;
{
  list t;
  int  i;
  pointerint sum;

  if ( !n )
    return(mknum(0));
  t = sp[n - 1];
  if ( !numberp(t) )
    numerr("%",t);
  sum = xnum(t);
  i = n - 1;
  while (i--) {
    t = sp[i];
    if ( !numberp(t) )
      numerr("%",t);
    else 
      sum = sum % (long)xnum(t);	/* CP/M68K is bad ..	*/
  }
  pop(n);
  return(mknum(sum));
}

/*
 *	Garbage Collection
 */

static list 
Lgc(n)
int n;
{
  void gc();

  argnchk("gc",0);
  gc();
  return(NIL);
}

static list
Lusedic(n)
int n;
{
  int i;
  extern char *kanjidicname[], *userdicname[], *bushudicname[];
  extern int nkanjidics, nuserdics, nbushudics;
  char **pdicname;
  int *counter, arrayp;
  list retval = NIL, temp;
  extern char *RengoGakushu;

  for (i = n ; i ; i--) {
    pdicname = kanjidicname;
    counter = &nkanjidics;
    arrayp = YES;
    temp = sp[i - 1];
    if (symbolp(temp) && i - 1 > 0) {
      if (temp == USER) {
	pdicname = userdicname;
	counter = &nuserdics;
      }
      else if (temp == BUSHU) {
	pdicname = bushudicname;
	counter = &nbushudics;
      }
      else if (temp == RENGO) {
	pdicname = &RengoGakushu;
	arrayp = NO;
      }
      i--; temp = sp[i - 1];
    }
    if (stringp(temp)) {
      if (arrayp) {
	if (*counter < MAX_DICS) {
	  pdicname[*counter] = malloc(strlen(xstring(temp)) + 1);
	  strcpy(pdicname[*counter], xstring(temp));
  	  pdicname[++*counter] = NULL;
  	  retval = T;
  	}
      }
      else {
	*pdicname = malloc(strlen(xstring(temp)) + 1);
	strcpy(*pdicname, xstring(temp));
	retval = T;
      }
    }
  }
  pop(n);
  return retval;
}

static list
Llist(n)
int n;
{
	push(NIL);
	for (; n ; n--) {
		push(Lcons(2));
	}
	return (pop1());
}

static list
Lcopysym(n)
int n;
{
  list src, dst;
  struct atomcell *dsta, *srca;

  argnchk("copy-symbol",2);
  src = pop1();
  dst = pop1();
  if (!symbolp(dst))
    error("copy-symbol: bad arg  ", dst);
  if (!symbolp(src))
    error("copy-symbol: bad arg  ", src);
  dsta = symbolpointer(dst);
  srca = symbolpointer(src);
  dsta->plist   = srca->plist;
  dsta->value   = srca->value;
  dsta->ftype   = srca->ftype;
  dsta->func    = srca->func;
  dsta->valfunc = srca->valfunc;
  dsta->mid     = srca->mid;
  dsta->fid     = srca->fid;
  return src;
}

static list
Lload(n)
int n;
{
  list p, t, Lread(), Leval(), Lncons();
  FILE *instream, *fopen();

  argnchk("load",1);
  p = pop1();
  if ( !stringp(p) )
    error("load: illegal file name  ",p);
  if ((instream = fopen(xstring(p), "r")) == NULL)
    error("load: file not found  ",p);
  prins("[load ");
  print(p);
  prins("]\n");

  if (jmpenvp <= 0) { /* 再帰が深すぎる場合 */
    return NIL;
  }
  jmpenvp--;
  files[++filep] = instream;

  setjmp(env[jmpenvp].jmp_env);
  env[jmpenvp].base_stack = sp - stack;
  env[jmpenvp].base_estack = esp - estack;

  for (;;) {
    t = Lread(0);
    if (valuec > 1 && null(values[1])) {
      break;
    }
    else {
      push(t);
      Leval(1);
    }
  }
  jmpenvp++;
  return(T);
}

static list
Lmodestr(n)
int n;
{
  list p;
  int mode;

  argnchk(S_SetModeDisp, 2);
  if ( !null(p = sp[0]) && !stringp(p) ) {
    strerr(S_SetModeDisp, p);
  }
  if (!symbolp(sp[1]) || (mode = symbolpointer(sp[1])->mid) == -1) {
    error("illegal mode ", sp[1]);
  }
  changeModeName(mode, null(p) ? 0 : xstring(p));
  pop(2);
  return p;
}

/* 機能シーケンスの取り出し */

static int
xfseq(fname, l, arr, arrsize) /* fname 機能名 */
char *fname;
list l;
unsigned char *arr;
int arrsize;
{
  int i;

  if (atom(l)) {
    if (symbolp(l) &&
	(arr[0] = (unsigned char)(symbolpointer(l)->fid)) != 255) {
      arr[1] = 0;
    }
    else {
      prins(fname);
      error(": illegal function ", l);
    }
    return 1;
  }
  else {
    for (i = 0 ; i < arrsize - 1 && consp(l) ; i++, l = cdr(l)) {
      list temp = car(l);

      if (!symbolp(temp) ||
	  (arr[i] = (unsigned char)(symbolpointer(temp)->fid)) == 255) {
	prins(fname);
	error(": illegal function ", temp);
      }
    }
    arr[i] = 0;
    return i;
  }
}

static list
Lsetkey(n)
int n;
{
  list p, l;
  int mode, slen;
  unsigned char fseq[256];
  unsigned char keyseq[256];

  argnchk(S_SetKey, 3);
  if ( !stringp(p = sp[1]) ) {
    strerr(S_SetKey, p);
  }
  if (!symbolp(sp[2]) || (mode = symbolpointer(sp[2])->mid) == -1) {
    error("illegal mode ", sp[2]);
  }
  if (xfseq(S_SetKey, sp[0], fseq, 256)) {
    slen = xstrlen(p);
    Strncpy(keyseq, xstring(p), slen);
    keyseq[slen] = 255;
    cchangeKeyfunc(mode, (unsigned)keyseq[0], /* ここは .canna のみ */
		  slen > 1 ? CANNA_FN_UseOtherKeymap :
		  (fseq[1] != 0 ? CANNA_FN_FuncSequence : fseq[0]),
		  fseq, keyseq);
  }
  pop(3);
  return p;
}

static list
Lgsetkey(n)
int n;
{
  list p, l;
  int mode, slen;
  unsigned char fseq[256];
  unsigned char keyseq[256];

  argnchk(S_GSetKey, 2);
  if ( !stringp(p = sp[1]) ) {
    strerr(S_GSetKey, p);
  }
  if (xfseq(S_GSetKey, sp[0], fseq, 256)) {
    slen = xstrlen(p);
    Strncpy(keyseq, xstring(p), slen);
    keyseq[slen] = 255;
    changeKeyfuncOfAll((unsigned)keyseq[0],
		       slen > 1 ? CANNA_FN_UseOtherKeymap :
		       (fseq[1] != 0 ? CANNA_FN_FuncSequence : fseq[0]),
		       fseq, keyseq);
    pop(2);
    return p;
  }
  else {
    pop(2);
    return NIL;
  }
}

static list
Lputd(n)
int n;
{
  list body, a;
  list sym;
  struct atomcell *symp;

  argnchk("putd",2);
  a = body = pop1();
  sym = pop1();
  symp = symbolpointer(sym);
  if (constp(sym) || consp(sym)) {
    error("putd: Function name must be a symbol : ",sym);
  }
  if (null(body)) {
    symp->ftype = UNDEF;
    symp->func = (list (*)())UNDEF;
  }
  else if (consp(body)) {
    if (car(body) == _MACRO) {
      symp->ftype = MACRO;
      symp->func = (list (*)())body;
    }
    else {
      symp->ftype = EXPR;
      symp->func = (list (*)())body;
    }
  }
  return(a);
}

static list
Ldefun()
{
  list form, res, Lputd();

  form = sp[0];
  if (atom(form)) {
    error("defun: illegal form ",form);
  }
  push(car(form));
  push(_LAMBDA);
  push(cdr(form));
  push(Lcons(2));
  Lputd(2);
  res = car(pop1());
  return (res);
}

static list
Ldefmacro()
{
  list form, res, Lputd();

  form = sp[0];
  if (atom(form)) {
    error("defmacro: illegal form ",form);
  }
  push(res = car(form));
  push(_MACRO);
  push(cdr(form));
  push(Lcons(2));
  Lputd(2);
  pop1();
  return (res);
}

static list
Lcar(n)
int n;
{
  list f;

  argnchk("car",1);
  f = pop1();
  if (!f)
    return(NIL);
  else if (atom(f))
    error("Bad arg to car ",f);
  else
    return(car(f));
}

static list
Lcdr(n)
int n;
{
  list f;

  argnchk("cdr",1);
  f = pop1();
  if (!f)
    return(NIL);
  else if (atom(f))
    error("Bad arg to cdr ",f);
  else
    return(cdr(f));
}

static list
Latom(n)
int n;
{
  list f;

  argnchk("atom",1);
  f = pop1();
  if (atom(f))
    return(T);
  else
    return(NIL);
}

static list
Llet()
{
  list lambda, args, p, q, x, l, *pp, *pq, *pl, *px;

  px = sp;
  *px = cdr(*px);
  if (atom(*px)) {
    (void)pop1();
    return(NIL);
  }
  else {
    push(NIL);
    args = Lncons(1);
    push(q = args); pq = sp;
    push(NIL);
    lambda = p = Lncons(1);
    push(lambda);

    push(p); pp = sp;
    push(*pq); pq = sp;
    push(NIL); pl = sp;
    for (*pl = car(*px) ; consp(*pl) ; *pl = cdr(*pl)) {
      if (atom(car(*pl))) {
	push(car(*pl));
	*pp = cdr(*pp) = Lncons(1);
	push(NIL);
	*pq = cdr(*pq) = Lncons(1);
      }
      else if (atom(cdar(*pl))) {
	push(caar(*pl));
	*pp = cdr(*pp) = Lncons(1);
	push(NIL);
	*pq = cdr(*pq) = Lncons(1);
      }
      else {
	push(caar(*pl));
	*pp = cdr(*pp) = Lncons(1);
	push(cadr(car(*pl)));
	*pq = cdr(*pq) = Lncons(1);
      }
    }
    pop(3);
    sp[0] = cdr(sp[0]);
    sp[1] = cdr(sp[1]);
    push(cdr(*px));
    push(Lcons(2));
    push(_LAMBDA);
    push(Lxcons(2));
    p = Lxcons(2);
    (void)pop1();
    return(p);
  }
}

/* (if con tr . falist) -> (cond (con tr) (t . falist))*/

static list
Lif()
{
  list x, *px, retval;

  x = cdr(sp[0]);
  if (atom(x) || atom(cdr(x))) {
    (void)pop1();
    return NIL;
  }
  else {
    push(x); px = sp;

    push(COND);

    push(car(x));
    push(cadr(x));
    push(Llist(2));

    push(T);
    push(cddr(*px));
    push(Lcons(2));

    retval = Llist(3);
    pop(2);
    return retval;
  }
}

static list
Lunbindkey(n)
int n;
{
  unsigned char fseq[2];
  static unsigned char keyseq[2] = {(unsigned char)CANNA_KEY_Undefine,
				      (unsigned char)255};
  int mode;
  list retval;

  argnchk(S_UnbindKey, 2);
  if (!symbolp(sp[1]) || (mode = symbolpointer(sp[1])->mid) == -1) {
    error("illegal mode ", sp[1]);
  }
  if (xfseq(S_UnbindKey, sp[0], fseq, 2)) { /* ここは.canna形式にのみ */
    cchangeKeyfunc(mode, CANNA_KEY_Undefine,
		  fseq[1] != 0 ? CANNA_FN_FuncSequence : fseq[0],
		  fseq, keyseq);

    retval = T;
  }
  else {
    retval = NIL;
  }
  pop(2);
  return retval;
}

static list
Lgunbindkey(n)
int n;
{
  unsigned char fseq[2];
  static unsigned char keyseq[2] = {(unsigned char)CANNA_KEY_Undefine,
				      (unsigned char)255};
  int mode;
  list retval;

  argnchk(S_GUnbindKey, 1);
  if (xfseq(S_GUnbindKey, sp[0], fseq, 2)) {
    changeKeyfuncOfAll(CANNA_KEY_Undefine,
		       fseq[1] != 0 ? CANNA_FN_FuncSequence : fseq[0],
		       fseq, keyseq);
    retval = T;
  }
  else {
    retval = NIL;
  }
  (void)pop1();
  return retval;
}

static void
SetString(strp, s)
unsigned char **strp;
list s;
{
  if (stringp(s)) {
    int len = strlen(xstring(s));
    *strp = (unsigned char *)malloc(len + 1);
    strcpy((char *)*strp, xstring(s));
  }
  else if (!null(s)) {
    error("string data expected ", s);
  }
}

static list
Ldefmode()
{
  list form, *sym, e, *p, fn, rd, md, us;
  extern int nothermodes;
  extern newmode OtherModes[];
  int i, j;
  KanjiMode kanjimode;

  char *hata;

  form = pop1();
  if (atom(form)) {
    error("bad form ", form);
  }
  push(car(form));
  sym = sp;
  if (!symbolp(*sym)) {
    error("symbol data expected ", *sym);
  }

  /* 引数をプッシュする */
  for (i = 0, e = cdr(form) ; i < 4 ; i++, e = cdr(e)) {
    if (atom(e)) {
      for (j = i ; j < 4 ; j++) {
	push(NIL);
      }
      break;
    }
    push(car(e));
  }
  if (consp(e)) {
    error("bad form ", form);
  }

  /* 評価する */
  for (i = 0, p = sym - 1 ; i < 4 ; i++, p--) {
    push(*p);
    push(Leval(1));
  }
  us = pop1();
  fn = pop1();
  rd = pop1();
  md = pop1();
  pop(4);

  /* シンボルの関数値としての定義 これははずす。 ccustomのみ
  symbolpointer(*sym)->mid = CANNA_MODE_MAX_IMAGINARY_MODE + nothermodes; */

  /* 確定モードのためにこれははずす。ccustomのみ
  symbolpointer(*sym)->fid = CANNA_FN_MAX_FUNC + nothermodes; */
  
  /* 確定モードのためにIDをすり替える。 ccustomのみ */
  if (!strcmp(symbolpointer(*sym)->pname, S_ZenHiraKakuteiMode)) {
    symbolpointer(*sym)->fid = CANNA_FN_ZenHiraKakuteiMode;
    symbolpointer(*sym)->mid = CANNA_MODE_ZenHiraKakuteiMode;
  }
  if (!strcmp(symbolpointer(*sym)->pname, S_ZenKataKakuteiMode)) {
    symbolpointer(*sym)->fid = CANNA_FN_ZenKataKakuteiMode;;
    symbolpointer(*sym)->mid = CANNA_MODE_ZenKataKakuteiMode;
  }
  if (!strcmp(symbolpointer(*sym)->pname, S_HanKataKakuteiMode)) {
    symbolpointer(*sym)->fid = CANNA_FN_HanKataKakuteiMode;
    symbolpointer(*sym)->mid = CANNA_MODE_HanKataKakuteiMode;
  }
  if (!strcmp(symbolpointer(*sym)->pname, S_ZenAlphaKakuteiMode)){
    symbolpointer(*sym)->fid = CANNA_FN_ZenAlphaKakuteiMode;
    symbolpointer(*sym)->mid = CANNA_MODE_ZenAlphaKakuteiMode;
  }
  if (!strcmp(symbolpointer(*sym)->pname, S_HanAlphaKakuteiMode)) {
    symbolpointer(*sym)->fid = CANNA_FN_HanAlphaKakuteiMode;;
    symbolpointer(*sym)->mid = CANNA_MODE_HanAlphaKakuteiMode;
  }

  /* デフォルトの設定 */
  OtherModes[nothermodes].display_name = (wchar_t *)NULL;
  OtherModes[nothermodes].romaji_table = (unsigned char *)0;
  OtherModes[nothermodes].romdic = (struct RkwRxDic *)0;
  OtherModes[nothermodes].romdic_owner = 0;
  OtherModes[nothermodes].flags = CANNA_YOMI_IGNORE_USERSYMBOLS;
  OtherModes[nothermodes].emode = (KanjiMode)0;

  /* モード構造体の作成 */
  kanjimode = (KanjiMode)malloc(sizeof(KanjiModeRec));
  if (kanjimode) {
/*    int searchfunc(); いらないので削除 */
    extern KanjiModeRec empty_mode;
    extern BYTE *emptymap;

/*    kanjimode->func = searchfunc; いらないので削除 */
    kanjimode->keytbl = emptymap;
    kanjimode->flags = 
      CANNA_KANJIMODE_TABLE_SHARED | CANNA_KANJIMODE_EMPTY_MODE;
    kanjimode->ftbl = empty_mode.ftbl;
    OtherModes[nothermodes].emode = kanjimode;
  }

  /* モード表示文字列 */
  if (stringp(md)) {
    OtherModes[nothermodes].display_name = WString(xstring(md));
  }
  else if ( !null(md) ) {
    error("string data expected ", md);
  }

  /* ローマ字かな変換テーブル */
  if (stringp(rd)) {
    SetString(&(OtherModes[nothermodes].romaji_table), rd);
  }
  else if ( !null(rd) ) {
    error("string data expected ", rd);
  }

  /* 実行機能 */
  {
    list l;
    long f = OtherModes[nothermodes].flags;

    for (e = fn ; consp(e) ; e = cdr(e)) {
      l = car(e);
      if (symbolp(l) && symbolpointer(l)->fid) {
	switch (symbolpointer(l)->fid) {
	case CANNA_FN_Kakutei:
	  f |= CANNA_YOMI_KAKUTEI;
	  break;
	case CANNA_FN_Henkan:
	  f |= CANNA_YOMI_HENKAN;
	  break;
	case CANNA_FN_Zenkaku:
	  f |= CANNA_YOMI_ZENKAKU;
	  break;
	case CANNA_FN_Hankaku:
	  f |= CANNA_YOMI_HANKAKU;
	  break;
	case CANNA_FN_Hiragana:
	  f |= CANNA_YOMI_HIRAGANA;
	  break;
	case CANNA_FN_Katakana:
	  f |= CANNA_YOMI_KATAKANA;
	  break;
	case CANNA_FN_Romaji:
	  f |= CANNA_YOMI_ROMAJI;
	  break;
    /* 以下はそのうちやろう */
	case CANNA_FN_ToUpper:
	  break;
	case CANNA_FN_Capitalize:
	  break;
	case CANNA_FN_ToLower:
	  break;
	}
      }
    }
    OtherModes[nothermodes].flags = f;
  }

  /* ユーザシンボルの使用の有無 */

  if (us) {
    OtherModes[nothermodes].flags &= ~CANNA_YOMI_IGNORE_USERSYMBOLS;
  }

  nothermodes++;
  return pop1();
}

static list
Ldefsym()
{
  list form, res, e, Lputd();
  int i, j, k, ncand, group;
  wchar_t cand[1024], *p, *mcand, **acand, key;
  extern nkeysup;
  extern keySupplement keysup[];

  form = sp[0];
  if (atom(form)) {
    error("illegal form ",form);
  }
  /* まず数をかぞえる */
  for (ncand = 0 ; consp(form) ; ) {
    e = car(form);
    if (!numberp(e)) {
      error("key data expected ", e);
    }
    if (null(cdr(form))) {
      error("illegal form ",sp[0]);
    }
    for (i = 0, form = cdr(form) ; consp(form) ; i++, form = cdr(form)) {
      e = car(form);
      if (!stringp(e)) {
	break;
      }
    }
    if (ncand == 0) {
      ncand = i;
    }
    else if (ncand != i) {
      error("inconsist number for each key definition ", sp[0]);
    }
  }

  group = nkeysup;

  for (form = sp[0] ; consp(form) ;) {
    if (nkeysup >= MAX_KEY_SUP) {
      error("too many symbol definitions", sp[0]);
    }
    key = (wchar_t)xnum(car(form));
    p = cand;
    for (form = cdr(form) ; consp(form) ; form = cdr(form)) {
      int len;

      e = car(form);
      if (!stringp(e)) {
	break;
      }
      len = MBstowcs(p, xstring(e), 1024 - (p - cand));
      p += len;
      *p++ = (wchar_t)0;
    }
    *p++ = (wchar_t)0;
    mcand = (wchar_t *)malloc((p - cand) * sizeof(wchar_t));
    if (mcand == 0) {
      error("insufficient memory", NON);
    }
    acand = (wchar_t **)calloc(ncand + 1, sizeof(wchar_t *));
    if (acand == 0) {
      free(mcand);
      error("insufficient memory", NON);
    }

    for (i = 0 ; i < p - cand ; i++) {
      mcand[i] = cand[i];
    }
    for (i = 0, p = mcand ; i < ncand ; i++) {
      acand[i] = p;
      while(*p++);
    }
    acand[i] = 0;
    /* 実際に格納する */
    keysup[nkeysup].key = key;
    keysup[nkeysup].groupid = group;
    keysup[nkeysup].ncand = ncand;
    keysup[nkeysup].cand = acand;
    keysup[nkeysup].fullword = mcand;
    nkeysup++;
  }
  res = car(pop1());
  return (res);
}

static list
Lsetinifunc(n)
int n;
{
  unsigned char fseq[256];
  int i, len;
  list ret = NIL;
  extern BYTE *initfunc;
  extern int InitialMode;

  argnchk(S_SetInitFunc, 1);

  len = xfseq(S_SetInitFunc, sp[0], fseq, 256);

  if (len > 0) {
    if (initfunc) free(initfunc);
    initfunc = (BYTE *)malloc(len + 1);
    if (!initfunc) {
      error("insufficient memory", NON);
    }
    for (i = 0 ; i < len ; i++) {
      initfunc[i] = fseq[i];
    }
    initfunc[i] = 0;
    ret = T;
  }
  if (initfunc[0] ==  CANNA_FN_JapaneseMode) {
    if (len == 1) {
      InitialMode = 1;
    }
    else if (len == 2) {
      switch(initfunc[1]) {
      case CANNA_FN_ZenHiraKakuteiMode:
	InitialMode = 3;
	break;
      case CANNA_FN_ZenKataKakuteiMode:
	InitialMode = 4;
	break;
      case CANNA_FN_HanKataKakuteiMode:
	InitialMode = 5;
	break;
      case CANNA_FN_ZenAlphaKakuteiMode:
	InitialMode = 6;
	break;
      case CANNA_FN_HanAlphaKakuteiMode:
	InitialMode = 7;
	break;
      }
    }
  }
  (void)pop1();
  return ret;
}

/* lispfuncend */

/* 変数アクセスのための関数 */

static list
VTorNIL(var, setp, arg)
int *var, setp;
list arg;
{
  if (setp == VALSET) {
    *var = (arg == NIL) ? OFF : ON;
    return arg;
  }
  else { /* get */
    return *var ? ON : OFF;
  }
}

static list
StrAcc(var, setp, arg)
char **var;
int setp;
list arg;
{
  if (setp == VALSET) {
    if (null(arg) || stringp(arg)) {
      if (*var) {
	free(*var);
      }
      if (stringp(arg)) {
	*var = malloc(strlen(xstring(arg)) + 1);
	if (*var) {
	  strcpy(*var, xstring(arg));
	  return arg;
	}
	else {
	  error("Not enough memory.", NON);
	}
      }
      else {
	*var = (char *)0;
	return NIL;
      }
    }
    else {
      strerr((char *)0, arg);
    }
  }
  else {
    if (*var) {
      return copystring(*var, strlen(*var));
    }
    else {
      return NIL;
    }
  }
}

static list
NumAcc(var, setp, arg)
int *var;
int setp;
list arg;
{
  if (setp == VALSET) {
    if (numberp(arg)) {
      *var = (int)xnum(arg);
      return arg;
    }
    else {
      numerr((char *)0, arg);
    }
  }
  else {
    return mknum(*var);
  }
}

/* ここから下がカスタマイズの追加等で良くいじる部分 */

/* 実際のアクセス関数 */

extern int Gakushu, CursorWrap, SelectDirect, HexkeySelect, BunsetsuKugiri;
extern int ChBasedMove, ReverseWidely, ReverseWord, QuitIchiranIfEnd;
extern int kakuteiIfEndOfBunsetsu, stayAfterValidate, BreakIntoRoman;
extern int gramaticalQuestion, forceKana, kCount, chikuji, iListCB;
extern int kouho_threshold, nKouhoBunsetsu, keepCursorPosition;
extern char *RomkanaTable, *RengoGakushu;
extern int CannaVersion, abandonIllegalPhono, hexCharacterDefiningStyle;
extern int kojin, allowNextInput,indexhankaku,ignorecase,romajiyuusen,autosync;
extern int nkeysuu , quicklyescape;

static list Vgakushu(setp, arg) int setp; list arg;
{ return VTorNIL(&Gakushu, setp, arg); }

static list Vcursorw(setp, arg) int setp; list arg;
{ return VTorNIL(&CursorWrap, setp, arg); }

static list Vselectd(setp, arg) int setp; list arg;
{ return VTorNIL(&SelectDirect, setp, arg); }

static list Vnumeric(setp, arg) int setp; list arg;
{ return VTorNIL(&HexkeySelect, setp, arg); }

static list Vbunsets(setp, arg) int setp; list arg;
{ return VTorNIL(&BunsetsuKugiri, setp, arg); }

static list Vcharact(setp, arg) int setp; list arg;
{ return VTorNIL(&ChBasedMove, setp, arg); }

static list Vreverse(setp, arg) int setp; list arg;
{ return VTorNIL(&ReverseWidely, setp, arg); }

static list VreverseWord(setp, arg) int setp; list arg;
{ return VTorNIL(&ReverseWord, setp, arg); }

static list Vquitich(setp, arg) int setp; list arg;
{ return VTorNIL(&QuitIchiranIfEnd, setp, arg); }

static list Vkakutei(setp, arg) int setp; list arg;
{ return VTorNIL(&kakuteiIfEndOfBunsetsu, setp, arg); }

static list Vstayaft(setp, arg) int setp; list arg;
{ return VTorNIL(&stayAfterValidate, setp, arg); }

static list Vbreakin(setp, arg) int setp; list arg;
{ return VTorNIL(&BreakIntoRoman, setp, arg); }

static list Vgramati(setp, arg) int setp; list arg;
{ return VTorNIL(&gramaticalQuestion, setp, arg); }

static list Vforceka(setp, arg) int setp; list arg;
{ return VTorNIL(&forceKana, setp, arg); }

static list Vkouhoco(setp, arg) int setp; list arg;
{ return VTorNIL(&kCount, setp, arg); }

static list Vauto(setp, arg) int setp; list arg;
{ return VTorNIL(&chikuji, setp, arg); }

static list Vinhibi(setp, arg) int setp; list arg;
{ return VTorNIL(&iListCB, setp, arg); }

static list Vnhenkan(setp, arg) int setp; list arg;
{ return NumAcc(&kouho_threshold, setp, arg); }

static list Vnkouhobunsetsu(setp, arg) int setp; list arg;
{
  arg = NumAcc(&nKouhoBunsetsu, setp, arg);
  if (nKouhoBunsetsu < 3 || nKouhoBunsetsu > 60)
    nKouhoBunsetsu = 16;
  return arg;
}

static list VkeepCursorPosition(setp, arg) int setp; list arg;
{ return VTorNIL(&keepCursorPosition, setp, arg); }

static list VCannaVersion(setp, arg) int setp; list arg;
{ return NumAcc(&CannaVersion, setp, arg); }

static list VProtoVer(setp, arg) int setp; list arg;
{
  extern protocol_version;

  if (protocol_version < 0) {
/*    ObtainVersion(); いらないので削除 */
  }
  return NumAcc(&protocol_version, setp, arg);
}

static list VServVer(setp, arg) int setp; list arg;
{
  extern server_version;

  if (server_version < 0) {
/*    ObtainVersion(); いらないので削除 */
  }
  return NumAcc(&server_version, setp, arg);
}

static list VAbandon(setp, arg) int setp; list arg;
{ return VTorNIL(&abandonIllegalPhono, setp, arg); }

static list Vromkana(setp, arg) int setp; list arg;
{ return StrAcc(&RomkanaTable, setp, arg); }

static list VHexStyle(setp, arg) int setp; list arg;
{ return VTorNIL(&hexCharacterDefiningStyle, setp, arg); }

static list VKojin(setp, arg) int setp; list arg;
{ return VTorNIL(&kojin, setp, arg); }

static list VAllowNext(setp, arg) int setp; list arg;
{ return VTorNIL(&allowNextInput, setp, arg); }

static list VIndexHankaku(setp, arg) int setp; list arg;
{ return VTorNIL(&indexhankaku, setp, arg); }

static list VignoreCase(setp, arg) int setp; list arg;
{ return VTorNIL(&ignorecase, setp, arg); }

static list VRomajiYuusen(setp, arg) int setp; list arg;
{ return VTorNIL(&romajiyuusen, setp, arg); }

static list VAutoSync(setp, arg) int setp; list arg;
{ return VTorNIL(&autosync, setp, arg); }

static list Vnkeytodisconnect(setp, arg) int setp; list arg;
{
  arg = NumAcc(&nkeysuu, setp, arg);
  if (nkeysuu > 5000)
    nkeysuu = 5000;
  return arg;
}

static list VQuicklyEscape(setp, arg) int setp; list arg;
{ return VTorNIL(&quicklyescape, setp, arg); }

/* Lisp の関数と C の関数の対応表 */

static struct atomdefs initatom[] = {
  {"quote"		,SPECIAL,Lquote		},
  {"setq"		,SPECIAL,Lsetq		},
  {"set"		,SUBR	,Lset		},
  {"equal"		,SUBR	,Lequal		},
  {"="			,SUBR	,Lequal		},
  {">"			,SUBR	,Lgreaterp	},
  {"<"			,SUBR	,Llessp		},
  {"progn"		,SPECIAL,Lprogn		},
  {"eq"			,SUBR	,Leq   		},
  {"cond"		,SPECIAL,Lcond		},
  {"null"		,SUBR	,Lnull		},
  {"not"		,SUBR	,Lnull		},
  {"and"		,SPECIAL,Land		},
  {"or"			,SPECIAL,Lor		},
  {"+"			,SUBR	,Lplus		},
  {"-"			,SUBR	,Ldiff		},
  {"*"			,SUBR	,Ltimes		},
  {"/"			,SUBR	,Lquo		},
  {"%"			,SUBR	,Lrem		},
  {"gc"			,SUBR	,Lgc		},
  {"load"		,SUBR	,Lload		},
  {"list"		,SUBR	,Llist		},
  {"sequence"		,SUBR	,Llist		},
  {"defun"		,SPECIAL,Ldefun		},
  {"defmacro"		,SPECIAL,Ldefmacro	},
  {"cons"		,SUBR	,Lcons		},
  {"car"		,SUBR	,Lcar		},
  {"cdr"		,SUBR	,Lcdr		},
  {"atom"		,SUBR	,Latom		},
  {"let"		,CMACRO	,Llet		},
  {"if"			,CMACRO	,Lif		},
  {"copy-symbol"	,SUBR	,Lcopysym	},
  {S_FN_UseDictionary	,SUBR	,Lusedic	},
  {S_SetModeDisp	,SUBR	,Lmodestr	},
  {S_SetKey		,SUBR	,Lsetkey	},
  {S_GSetKey		,SUBR	,Lgsetkey	},
  {S_UnbindKey		,SUBR	,Lunbindkey	},
  {S_GUnbindKey		,SUBR	,Lgunbindkey	},
  {S_DefMode		,SPECIAL,Ldefmode	},
  {S_DefSymbol		,SPECIAL,Ldefsym	},
  {S_SetInitFunc	,SUBR	,Lsetinifunc	},
  {0			,UNDEF	,0		}, /* DUMMY */
};

static void
deflispfunc()
{
  struct atomdefs *p;

  for (p = initatom ; p->symname ; p++) {
    struct atomcell *atomp;
    list temp;

    temp = getatmz(p->symname);
    atomp = symbolpointer(temp);
    atomp->ftype = p->symtype;
    if (atomp->ftype != UNDEF) {
      atomp->func = p->symfunc;
    }
  }
}

/* 変数表 */

static struct cannavardefs cannavars[] = {
  {S_VA_RomkanaTable		,Vromkana},
  {S_VA_CursorWrap		,Vcursorw},
  {S_VA_SelectDirect		,Vselectd},
  {S_VA_NumericalKeySelect	,Vnumeric},
  {S_VA_BunsetsuKugiri		,Vbunsets},
  {S_VA_CharacterBasedMove	,Vcharact},
  {S_VA_ReverseWidely		,Vreverse},
  {S_VA_ReverseWord		,VreverseWord},
  {S_VA_Gakushu			,Vgakushu},
  {S_VA_QuitIfEOIchiran		,Vquitich},
  {S_VA_KakuteiIfEOBunsetsu	,Vkakutei},
  {S_VA_StayAfterValidate	,Vstayaft},
  {S_VA_BreakIntoRoman		,Vbreakin},
  {S_VA_NHenkanForIchiran	,Vnhenkan},
  {S_VA_GramaticalQuestion	,Vgramati},
  {S_VA_ForceKana		,Vforceka},
  {S_VA_KouhoCount		,Vkouhoco},
  {S_VA_Auto			,Vauto},
  {S_VA_InhibitListCallback	,Vinhibi},
  {S_VA_nKouhoBunsetsu		,Vnkouhobunsetsu},
  {S_VA_keepCursorPosition	,VkeepCursorPosition},
  {S_VA_CannaVersion		,VCannaVersion},
  {S_VA_Abandon			,VAbandon},
  {S_VA_HexDirect		,VHexStyle},
  {S_VA_ProtocolVersion		,VProtoVer},
  {S_VA_ServerVersion		,VServVer},
  {S_VA_Kojin			,VKojin},
  {S_VA_AllowNextInput		,VAllowNext},
  {S_VA_IndexHankaku		,VIndexHankaku},
  {S_VA_ignoreCase		,VignoreCase},
  {S_VA_RomajiYuusen		,VRomajiYuusen},
  {S_VA_AutoSync		,VAutoSync},
  {S_VA_nDisconnectServer	,Vnkeytodisconnect},
  {S_VA_QuicklyEscape	        ,VQuicklyEscape},
  {0				,0},
};

static void
defcannavar()
{
  struct cannavardefs *p;

  for (p = cannavars ; p->varname ; p++) {
    symbolpointer(getatmz(p->varname))->valfunc = p->varfunc;
  }
}

/* モード表 */

static struct cannamodedefs cannamodes[] = {
  {S_AlphaMode			,CANNA_MODE_AlphaMode},
  {S_YomiganaiMode		,CANNA_MODE_EmptyMode},
  {S_YomiMode			,CANNA_MODE_YomiMode},
  {S_MojishuMode		,CANNA_MODE_JishuMode},
  {S_TankouhoMode		,CANNA_MODE_TankouhoMode},
  {S_IchiranMode		,CANNA_MODE_IchiranMode},
  {S_KigouMode			,CANNA_MODE_KigoMode},
  {S_YesNoMode			,CANNA_MODE_YesNoMode},
  {S_OnOffMode			,CANNA_MODE_OnOffMode},
  {S_ShinshukuMode		,CANNA_MODE_AdjustBunsetsuMode},

  {S_AutoYomiMode		,CANNA_MODE_ChikujiYomiMode},
  {S_AutoBunsetsuMode		,CANNA_MODE_ChikujiTanMode},

  {S_HenkanNyuuryokuMode	,CANNA_MODE_HenkanNyuryokuMode},
  {S_HexMode			,CANNA_MODE_HexMode},
  {S_BushuMode			,CANNA_MODE_BushuMode},
  {S_ExtendMode			,CANNA_MODE_ExtendMode},
  {S_RussianMode		,CANNA_MODE_RussianMode},
  {S_GreekMode			,CANNA_MODE_GreekMode},
  {S_LineMode			,CANNA_MODE_LineMode},
  {S_ChangingServerMode		,CANNA_MODE_ChangingServerMode},
  {S_HenkanMethodMode		,CANNA_MODE_HenkanMethodMode},
  {S_DeleteDicMode		,CANNA_MODE_DeleteDicMode},
  {S_TourokuMode		,CANNA_MODE_TourokuMode},
  {S_TourokuHinshiMode		,CANNA_MODE_TourokuHinshiMode},
  {S_TourokuDicMode		,CANNA_MODE_TourokuDicMode},
  {S_QuotedInsertMode		,CANNA_MODE_QuotedInsertMode},
  {S_BubunMuhenkanMode		,CANNA_MODE_BubunMuhenkanMode},
  {S_MountDicMode		,CANNA_MODE_MountDicMode},
  {S_ZenHiraHenkanMode		,CANNA_MODE_ZenHiraHenkanMode},
  {S_HanHiraHenkanMode		,CANNA_MODE_HanHiraHenkanMode},
  {S_ZenKataHenkanMode		,CANNA_MODE_ZenKataHenkanMode},
  {S_HanKataHenkanMode		,CANNA_MODE_HanKataHenkanMode},
  {S_ZenAlphaHenkanMode		,CANNA_MODE_ZenAlphaHenkanMode},
  {S_HanAlphaHenkanMode		,CANNA_MODE_HanAlphaHenkanMode},
  {S_ZenHiraKakuteiMode		,CANNA_MODE_ZenHiraKakuteiMode},
  {S_HanHiraKakuteiMode		,CANNA_MODE_HanHiraKakuteiMode},
  {S_ZenKataKakuteiMode		,CANNA_MODE_ZenKataKakuteiMode},
  {S_HanKataKakuteiMode		,CANNA_MODE_HanKataKakuteiMode},
  {S_ZenAlphaKakuteiMode	,CANNA_MODE_ZenAlphaKakuteiMode},
  {S_HanAlphaKakuteiMode	,CANNA_MODE_HanAlphaKakuteiMode},
  {0				,0},
};

static void
defcannamode()
{
  struct cannamodedefs *p;

  for (p = cannamodes ; p->mdname ; p++) {
    symbolpointer(getatmz(p->mdname))->mid = p->mdid;
  }
}

/* 機能表 */

static struct cannafndefs cannafns[] = {
  {S_FN_Undefined		,CANNA_FN_Undefined},
  {S_FN_SelfInsert		,CANNA_FN_FunctionalInsert},
  {S_FN_QuotedInsert		,CANNA_FN_QuotedInsert},
  {S_FN_JapaneseMode		,CANNA_FN_JapaneseMode},
  {S_AlphaMode			,CANNA_FN_AlphaMode},
  {S_HenkanNyuuryokuMode	,CANNA_FN_HenkanNyuryokuMode},
  {S_HexMode			,CANNA_FN_HexMode},
  {S_BushuMode			,CANNA_FN_BushuMode},
  {S_KigouMode			,CANNA_FN_KigouMode},
  {S_FN_Forward			,CANNA_FN_Forward},
  {S_FN_Backward		,CANNA_FN_Backward},
  {S_FN_Next			,CANNA_FN_Next},
  {S_FN_Prev			,CANNA_FN_Prev},
  {S_FN_BeginningOfLine		,CANNA_FN_BeginningOfLine},
  {S_FN_EndOfLine		,CANNA_FN_EndOfLine},
  {S_FN_DeleteNext		,CANNA_FN_DeleteNext},
  {S_FN_DeletePrevious		,CANNA_FN_DeletePrevious},
  {S_FN_KillToEndOfLine		,CANNA_FN_KillToEndOfLine},
  {S_FN_Henkan			,CANNA_FN_Henkan},
  {S_FN_Kakutei			,CANNA_FN_Kakutei},
  {S_FN_Extend			,CANNA_FN_Extend},
  {S_FN_Shrink			,CANNA_FN_Shrink},
  {S_ShinshukuMode		,CANNA_FN_AdjustBunsetsu},
  {S_FN_Quit			,CANNA_FN_Quit},
  {S_ExtendMode			,CANNA_FN_ExtendMode},
  {S_FN_Touroku			,CANNA_FN_Touroku},
  {S_FN_ConvertAsHex		,CANNA_FN_ConvertAsHex},
  {S_FN_ConvertAsBushu		,CANNA_FN_ConvertAsBushu},
  {S_FN_KouhoIchiran		,CANNA_FN_KouhoIchiran},
  {S_FN_BubunMuhenkan		,CANNA_FN_BubunMuhenkan},
  {S_FN_Zenkaku			,CANNA_FN_Zenkaku},
  {S_FN_Hankaku			,CANNA_FN_Hankaku},
  {S_FN_ToUpper			,CANNA_FN_ToUpper},
  {S_FN_Capitalize		,CANNA_FN_Capitalize},
  {S_FN_ToLower			,CANNA_FN_ToLower},
  {S_FN_Hiragana		,CANNA_FN_Hiragana},
  {S_FN_Katakana		,CANNA_FN_Katakana},
  {S_FN_Romaji			,CANNA_FN_Romaji},
  {S_FN_BaseHiragana		,CANNA_FN_BaseHiragana},
  {S_FN_BaseKatakana		,CANNA_FN_BaseKatakana},
  {S_FN_BaseKana		,CANNA_FN_BaseKana},	
  {S_FN_BaseEisu		,CANNA_FN_BaseEisu},	
  {S_FN_BaseZenkaku		,CANNA_FN_BaseZenkaku},
  {S_FN_BaseHankaku		,CANNA_FN_BaseHankaku},
  {S_FN_BaseKakutei		,CANNA_FN_BaseKakutei},
  {S_FN_BaseHenkan		,CANNA_FN_BaseHenkan},
  {S_FN_BaseHiraKataToggle	,CANNA_FN_BaseHiraKataToggle},
  {S_FN_BaseZenHanToggle	,CANNA_FN_BaseZenHanToggle},
  {S_FN_BaseKanaEisuToggle	,CANNA_FN_BaseKanaEisuToggle},
  {S_FN_BaseKakuteiHenkanToggle	,CANNA_FN_BaseKakuteiHenkanToggle},
  {S_FN_BaseRotateForward	,CANNA_FN_BaseRotateForward},
  {S_FN_BaseRotateBackward	,CANNA_FN_BaseRotateBackward},
  {S_FN_HenkanOrSelfInsert	,CANNA_FN_HenkanOrInsert},
  {S_FN_HenkanOrNothing	        ,CANNA_FN_HenkanOrNothing},
  {S_FN_DisconnectServer        ,CANNA_FN_DisconnectServer},
  {S_FN_ChangeServerMode        ,CANNA_FN_ChangeServerMode},
  {S_FN_ShowServer              ,CANNA_FN_ShowServer},
  {S_FN_ShowGakushu             ,CANNA_FN_ShowGakushu},
  {S_FN_ShowVersion             ,CANNA_FN_ShowVersion},
  {S_FN_ShowPhonogramFile       ,CANNA_FN_ShowPhonogramFile},
  {S_FN_ShowCannaFile           ,CANNA_FN_ShowCannaFile},
  {S_FN_SyncDic                 ,CANNA_FN_SyncDic},
  {0				,0},
};

static void
defcannafunc()
{
  struct cannafndefs *p;

  for (p = cannafns ; p->fnname ; p++) {
    symbolpointer(getatmz(p->fnname))->fid = p->fnid;
  }
}

static void
defatms()
{
  list getatmz();

  deflispfunc();
  defcannavar();
  defcannamode();
  defcannafunc();
  QUOTE		= getatmz("quote");
  T		= getatmz("t");
  _LAMBDA	= getatmz("lambda");
  _MACRO	= getatmz("macro");
  COND		= getatmz("cond");
  USER		= getatmz(":user");
  BUSHU		= getatmz(":bushu");
  RENGO		= getatmz(":rengo");
  symbolpointer(T)->value = T;
}
