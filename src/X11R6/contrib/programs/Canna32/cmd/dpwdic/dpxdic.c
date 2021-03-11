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
static char rcsid[]="@(#) 102.1 $Id: dpxdic.c,v 1.33 1992/10/16 21:11:54 kazuo Exp $";
#endif

#include	<stdio.h>
#include	<time.h>		/* 時間をとってくるため */
#include	<fcntl.h>
#include	<unistd.h>
#include	"RK.h"
#include	"RKintern.h"

#ifndef HYOUJUN_GRAM
#define HYOUJUN_GRAM "/usr/lib/canna/dic/canna/fuzokugo.d"
#endif

#ifdef	ISLASTNOD
#undef	ISLASTNOD
#define	ISLASTNOD(flag)	((flag) & 0x08000000)
#endif

#ifdef	ISWORDNOD
#undef	ISWORDNOD
#define	ISWORDNOD(flag)	((flag) & 0x04000000)
#endif


struct wrec	{			/* ワードレコードの管理用 */
    Wchar		key;
    unsigned long	offset;
};


int	invert;			/* 読みと候補を逆に出力指定するフラグ */
int	tree;			/* ディレクトリ部を出力する */
unsigned char	Pat[256], *P;
int	COUNT;
char	binname[15];		/* バイナリ辞書ファイル名 */
unsigned long	locale;

static char *
basename(name)
char	*name;
{
    char	*s = name + strlen(name);
    
    if (*s == '/')
	*s = (char)0;
    while (s-- >= name)
	if (*s == '/')
	    return ++s;
    return name;
}

void
usage( args )
char	*args;
{
    (void)fprintf(stderr,
		  "usage: %s [-i] [-D bunpou] <file name> [dictionary-name]\n",
		  basename(args));
    exit(1);
}


int
RkScanWcand1(wrec, word, maxword)
Wrec		*wrec;
struct RkWcand	*word;
int		maxword;
{
    Wrec	*wr;
    int		i;
    int		nc;
    int		ns = 0;

    wr = wrec + 1 + 2*wrec[0];
    nc = S2TOS(wr) & 0x3ff;
    wr += 5;
    for ( i = 0; i < nc; i++ ) {
        Wrec		*addr;
	unsigned	flags;
        int		rcnum;
	int		klen;
	
        addr = wr;
	flags = *wr++;
	rcnum = *wr++;
	if( rowcol256(flags) )	
	    rcnum += 256;
	klen = candlen(flags);
	wr += 2*klen;

        if ( i < maxword )
        {
	    word[i].addr = addr;
	    word[i].rcnum = rcnum;
	    word[i].klen = klen;
	    ns++;
	};
    }
    return ns;
}

static struct RkKxGram *
OpenCnj(dic)
char	*dic;
{
    int		fd;
    struct RkKxGram *gram;
    
    if ( (fd = open(dic, 0)) < 0 )
	return (struct RkKxGram *)0;

    gram = RkReadGram(fd);
    (void)close(fd);
    return gram;
}


void
PrintHdr( hdr, childname )
unsigned char	*hdr;
unsigned char	*childname;
{
    unsigned char	*s;
    char		*date;
    long		tloc;
    unsigned long	tangoLine;
    unsigned long	yomiLine;

    hdr += 4;
    s = childname;
    while( *hdr != '\0' )
	*s++ = *hdr++;
    *s++ = *hdr++;
    
    tloc = L4TOL( hdr );
    date = ctime( &tloc );
    date[strlen(date)-1] = '\0';
    hdr += 4;
    tangoLine = L4TOL( hdr );
    hdr += 4;
    yomiLine = L4TOL( hdr );
    
#ifdef	USE_LOCALE
    fprintf(stderr,
	    "%s [%s] = %d + %d \"%d\"\n",
	    childname, date, tangoLine, yomiLine, locale);
#else
    fprintf(stderr,
	    "%s [%s] = %d + %d\n",
	    childname, date, tangoLine, yomiLine);
#endif
}


/* ヘッダ部の解析 */
int ckhder( fp, hdr )
FILE		*fp;
unsigned char	*hdr;			/* 子辞書のテキストファイル名 */
{
    /* ヘッダ部の取り込み & 参照 */
    if( (fread((char *)hdr, ND_HDRSIZ, 1, fp) ) == 0 ) {
	return( -1 );
    }
    if( hdr[0] != 'H' || hdr[1] != 'N' || hdr[2] != 'D' ) {
	if( hdr[0] != 'N' || hdr[1] != 'D') {
	    fprintf(stderr, "Not header!\n", 0);
	    return( -1 );
	}
	return( -2 );	/* old dics */
    }
    return( 0 );
}

/* ４バイトの情報を読み込む */
unsigned long
gettotal( moji )
unsigned char	*moji;
{
    int		i = 0;
    unsigned char	M4[4];
    
    while( i++ < 4 && *moji ) {
	M4[i] = *moji++;
    }
    return( L4TOL(M4) );
}


/* 指定した文字と同じものだけを出力する */
int	matchpat(text)
unsigned char	*text;
{
    unsigned char	*pat;

    for ( pat = Pat; *pat; pat += strlen((char *)pat) + 1 ) {
	int	plen, tlen;
	
	plen = strlen((char *)(pat + 1));
	tlen = strlen((char *)text);
	if ( pat[0] == 'F' && tlen != plen )
	    continue;
	if ( tlen < plen )
	    continue;

	switch(pat[0]) {
	  case    'F':
	  case	'P':
	    if ( !strncmp((char *)text, (char *)&pat[1], (unsigned)plen) )
		return(1);
	    break;
	  case	'S':
	    if ( !strncmp((char *)&text[tlen-plen], (char *)&pat[1],
			  (unsigned)plen) )
		return(1);
	    break;
	  case	'I': {
	      unsigned char	*t;
	      int	w;
	      
	      for ( t = text; tlen >= plen; t += w, tlen -= w ) {
		  w = (*t&0x80) ? 2 : 1;
		  if ( !strncmp((char *)t, (char *)&pat[1], (unsigned)plen) )
		      return(1);
	      }
	      break;
	  }
	}
    }
    return(0);
}

/* dumpWord : ワードレコードを出力する
 *	readWrec	: 辞書からワードレコード部を読み込む
 *
 */
int
readWrec(dic, wrec, yomi)
FILE		*dic;
Wrec		*wrec;
unsigned char	*yomi;
{
    Wrec	*w;
    int         ylen, hi, lo;
    int		nk, flags, len;
    
    w = wrec;
    *w++ = ylen = getc(dic);		/* 読みの文字数をとる */
    while( ylen-- ) {
      *w++ = hi = getc(dic);
      *w++ = lo = getc(dic);
      
      if( hi == 0 ) {
	if (lo & 0x80) {	/* カタカナ */
	  *yomi++ = 0x8e;
	}
	*yomi++ = lo;
      }
      else {
	*yomi++ = hi;
	*yomi++ = lo;
      }
    }
    *yomi=0;
    *w++ = hi = getc(dic);
    *w++ = lo = getc(dic);/* 候補数を取り出す */
    nk = (int)((hi << 8) & 0x0300)|(int)(lo & 0x00ff);
    *w++ = getc(dic);			/* 学習ファイルのオフセット */
    *w++ = getc(dic);
    *w++ = getc(dic);
    
    while ( nk-- > 0 ) {
	*w++ = flags = getc(dic);	/* 候補の文字数を取り出す */
	*w++ = getc(dic);		/* 品詞を取り出す */
	for ( len = candlen(flags) ; len-- > 0 ; ) {
	  *w++ = getc(dic);
	  *w++ = getc(dic);
	}
    }
    return( w - wrec );			/* 残りのワードレコード */
}

int
dumpWord(dic, gram)
FILE	*dic;
struct RkKxGram	*gram;
{
    Wrec		wrec[RK_WREC_BMAX], *w;
    struct RkWcand    	word[RK_CAND_NMAX];
    unsigned char	yomi[1024];
    unsigned char	*k;
    int			sz;		/* ワードレコードのサイズ */
    int			nk;		/* 候補数 */
    int			i;
    
    w = wrec;
    sz = readWrec(dic, w, yomi);	/* サイズを返す */
    if ( Pat[0] && !matchpat(yomi) ) {
	return sz;
    }
    nk = RkScanWcand1(wrec, word, RK_CAND_NMAX);
    for ( i = 0; i < nk; i++ ) {
	Wchar   	*ep;
	Wchar		pair[RK_RCNAME_BMAX];
	unsigned char	hinshi[RK_RCNAME_BMAX];
	int		len;		/* 候補文字数 */
	int		j;
	
	ep = RkUparseGramNum(gram, word[i].rcnum, pair, sizeof(pair)/sizeof(Wchar));
	for( j = 0 ; pair + j < ep ; j++ ) {
	    hinshi[j] = pair[j]&0x00ff;
	}
	hinshi[j] = '\0';
	
	len = word[i].klen;
	if( !invert ) {		/* 読み 品詞 候補 */
	    printf("%s %s ", yomi, hinshi);
	    
	    if( len == 0 ) {
		printf(" %s", yomi);
	    }
	    else {
		for ( k = word[i].addr + 2; len-- > 0;  k+=2 ) {
		    if( *k != '\0' )
			putchar(*k);
		    putchar(*(k+1));
		}
	    }
	    putchar('\n');
	}
	else {			/* 候補 品詞 読み */
	    if( len == 0 ) {
		printf("%s", yomi);
	    }
	    else {
		for( k = word[i].addr + 2; len--; k+=2) {
		    if( *k != '\0' )
			putchar(*k);
		    putchar(*(k+1));
		}
	    }
	    printf(" %s %s\n", hinshi, yomi);
	}
    }
    return sz;
}


/* dumpDir : ディレクトリをたどって表示をする
 *
 */
unsigned char *
eucGetChar(p, s)
unsigned char *p;
Wrec *s;
{
  if (!s[0]) {
    if (s[1] & 0x80) {
      *p++ = 0x8e;
      *p++ = s[1];
    }
    else {
      *p++ = s[1];
    }
  }
  else {
    if (!(s[1] & 0x80)) {
      *p++ = 0x8f;
    }
    *p++ = s[0];
    *p++ = s[1];
  }
  return p;
}

void
showwr(gram, word, wordsiz, diroff, yomi, yomilen)
struct RkKxGram	*gram;
Wrec *word;
int diroff, wordsiz;
Wchar *yomi;
int yomilen;
{
  Wchar wyomi[1024], *wp;
  unsigned char buf[1024], *eb;
  Wrec tmprec[2];
  int len = 0, i = diroff, ylen;
  
  if (diroff < 0 || wordsiz < diroff) {
    fprintf(stderr, "★オフセットエラー : 0x%x\n", diroff);
    return;
  }

  ylen = word[i++];
  while (ylen--) {
    wyomi[len] = (Wchar)((word[i] << 8) | word[i + 1]);
    i += 2;
    len++;
  }
  wyomi[len] = (Wchar)0;
  eb = buf;
  wp = wyomi + yomilen;
  while (*wp) {
    tmprec[0] = (Wrec)(*wp >> 8);
    tmprec[1] = (Wrec)(*wp & 0xff);
    eb = eucGetChar(eb, tmprec);
    wp++;
  }
  *eb = (unsigned char)0;
  if (eb != buf) {
    printf(".%s", buf);
  }
  printf(" ");
  (void)printword(gram, word, wordsiz, diroff, 0);
  return;
}

void
showdir(gram, disp, dispsiz, word, wordsiz, diroff, yomi, yomilen)
struct RkKxGram	*gram;
unsigned char *disp, *word;
int dispsiz, wordsiz, diroff;
Wchar *yomi;
int yomilen;
{
  unsigned nextoff;
  unsigned char *ep;
  int firststep = 1, newyomi = 0;

  if (diroff < ND_DSPENT * ND_NODSIZ || dispsiz <= diroff) {
    fprintf(stderr, "★エラー : オフセットポインタ 0x%x\n", diroff);
    return;
  }

  for (;;) {
    Wchar ch;

    if (dispsiz <= diroff) {
      fprintf(stderr, "★エラー : オフセットポインタ 0x%x\n", diroff);
      return;
    }

    ch = disp[diroff] << 8 | disp[diroff + 1];
    newyomi = 0;
    if (ch) {
      Wrec buf[2];
      unsigned char ebuf[5];

      yomi[yomilen] = ch;
      buf[0] = ch >> 8;
      buf[1] = ch & 0xff;
      ep = eucGetChar(ebuf, buf);
      *ep = (unsigned char)0;
      if (!firststep) {
	int i;
	for (i = 0 ; i < yomilen ; i++) {
	  printf("  ");
	}
      }
      newyomi = 1;
      printf("%2s", ebuf);
    }
    nextoff = (((disp[diroff + 3] << 8) | disp[diroff + 4]) << 8)
      | disp[diroff + 5];
    if (disp[diroff + 2] & ND_WORD) {
      showwr(gram, word, wordsiz, (int)nextoff, yomi, yomilen + newyomi);
    }
    else {
      showdir(gram, disp, dispsiz, word, wordsiz,
	      (int)nextoff, yomi, yomilen + newyomi);
    }
    if (disp[diroff + 2] & ND_LAST) {
      break;
    }
    firststep = 0;
    diroff += ND_NODSIZ;
  }
  return;
}

Wchar
getentch(x)
int x;
{
  if (x == 0) {
    return (Wchar)0;
  }
  else if (x < 94) {
    return (Wchar)(' ' + x);
  }
  else {
    return (Wchar)(0xa4a0 + x - 94);
  }
}

showtree(dic, gram, dir, dirsiz, wrdoff)
FILE		*dic;
struct RkKxGram *gram;
unsigned char	*dir;			/* ディレクトリ部用の領域 */
unsigned long	dirsiz;
unsigned long	wrdoff;			/*  */
{
  unsigned char l4[4];
  unsigned char *wbuf = (unsigned char *)0;
  int wrdsiz, i;
  int diroff;
  Wchar yomi[256];
  int yomilen;
  unsigned char *curentry;

  printf("dispatch size = %d\n", 189 * 6);
  printf("\ndirectory size = %d(%d-%d)\n",
	 (int)dirsiz-189*6, dirsiz, 189*6 );
  
  fseek(dic, (long)wrdoff-4, 0);
  (void)fread((char *)l4, 4, 1, dic); /* ワード部のサイズを読む */
  wrdsiz = L4TOL(l4);
  wbuf = (unsigned char *)malloc(wrdsiz);
  if (!wbuf ) {
    fprintf(stderr, "Insufficient memory\n");
    exit(1);
  }
  printf("\nword size = %d\n", wrdsiz);
  (void)fread(wbuf, wrdsiz, 1, dic);
  
  for (i = 0 ; i < ND_DSPENT ; i++) {
    Wchar		ch;
    unsigned char	*ep;
    
    curentry = dir + ND_NODSIZ * i;
    if (curentry[0] || curentry[1]) {
      Wrec buf[2];
      unsigned char ebuf[5];

      ch = getentch(i);
      yomilen = 0;
      if (ch) {
	yomi[0] = ch;
	yomilen++;

	buf[0] = ch >> 8;
	buf[1] = ch & 0xff;
	ep = eucGetChar(ebuf, buf);
	*ep = (unsigned char)0;
	printf("%2s", ebuf);
      }
      diroff = (((curentry[3] << 8) | curentry[4]) << 8) | curentry[5];
      showdir(gram, dir, (int)dirsiz, wbuf, wrdsiz, diroff, yomi, yomilen);
    }
  }
}

int
dumpDir(dic, gram, dir, dirsiz, wrdoff)
FILE		*dic;
struct RkKxGram *gram;
unsigned char	*dir;			/* ディレクトリ部用の領域 */
unsigned long	dirsiz;
unsigned long	wrdoff;			/*  */
{
  showtree(dic, gram, dir, dirsiz, wrdoff);
}


/* dumpOffset : オフセット値をダンプする
 *	showdispatch	: ディスパッチ部を参照する
 *	showword	: ワード部を参照する
 *	eucGetChar	: 
 */
void
showdispatch(dir, dirsiz)
unsigned char	*dir;			/* ディレクトリ部用の領域 */
unsigned long	dirsiz;
{
  int			i, j;
  
  printf("dispatch size = %d\n", 189 * 6);
  for (i = 0 ; i < 189 ; i++) {
    j = i * 6;
    
    if (!dir[j] && !dir[j + 1]) {
      continue;
    }
    
    if (i == 0) {		/* ディパッチ未使用グループ用 */
      printf("0x%04x @  %03d こ 0x%06x\n",
	     i, (dir[j] << 8) | dir[j + 1],
	     ((dir[j + 3] << 8) | dir[j + 4] << 8) | dir[j + 5]);
    }
    else if (i < 95) {	/* アスキー用 */
      printf("0x%04x %c  %03d こ 0x%06x\n",
	     i, i + ' ',
	     (dir[j] << 8) | dir[j + 1],
	     ((dir[j + 3] << 8) | dir[j + 4] << 8) | dir[j + 5]);
    }
    else if (i < 189) {	/* ひらがな用 */
      printf("0x%04x %c%c %03d こ 0x%06x\n",
	     i, 0xa4, i - 94 + ' ' + 0x80,
	     (dir[j] << 8) | dir[j + 1],
	     ((dir[j + 3] << 8) | dir[j + 4] << 8) | dir[j + 5]);
    }
  }
  printf("\ndirectory size = %d(%d-%d)\n",
	 (int)dirsiz-189*6, dirsiz, 189*6 );
  for (j = 189 * 6 ; j < (int)dirsiz ; j += 6) {
    if (!dir[j]) {
      if (dir[j + 1] & 0x80) {
	printf("0x%04x %c%c ", j, 0x8e, dir[j + 1]);
      }
      else {
	printf("0x%04x %c  ", j, dir[j + 1] ? dir[j + 1] : ' ');
      }
    }
    else {
      printf("0x%04x %c%c ", j, dir[j], dir[j + 1]);
    }
    printf("%c%c 0x%06x\n",
	   ((dir[j + 2] & ND_LAST) ? 'L' : ' '),
	   ((dir[j + 2] & ND_WORD) ? 'W' : ' '),
	   ((dir[j + 3] << 8) | dir[j + 4] << 8) | dir[j + 5]);
  }
}

int
printword(gram, buf, wordsiz, i, f)
struct RkKxGram	*gram;
Wrec *buf;
int wordsiz, i, f;
{
  unsigned char wrb[4096], *pw;
  unsigned ncand, freqoff;
  int ylen;

  pw = wrb;

  sprintf((char *)pw, "0x%04x ", i);
  pw += strlen((char *)pw);

  ylen = buf[i++];
  while (ylen--) {
    pw = eucGetChar(pw, buf + i);
    i += 2;
  }
  *pw++ = ' ';

  ncand = ((buf[i] << 8) | buf[i + 1]) & 0x3ff;
  i += 2;
  freqoff = (((buf[i] << 8) | buf[i + 1]) << 8) | buf[i + 2];
  i += 3;

  if (!f) pw = wrb;

  sprintf((char *)pw, "%d 0x%06x ", ncand, freqoff);
  pw += strlen((char *)pw);
  while (ncand-- > 0) {
    int len;
    Wchar   	*ep;
    Wchar pair[256];
    int	rcnum;
    int	j;
    
    len = candlen( buf[i] );
    rcnum = buf[i+1];
    if( rowcol256( buf[i] ) )
      rcnum += 256;
    ep = RkUparseGramNum(gram, rcnum, pair, sizeof(pair)/sizeof(Wchar));
    
    i += 2;
    if (pw - wrb < 48) {
      for( j = 0 ; pair + j < ep ; j++ ) {
	Wrec tmprec[2];

	tmprec[0] = pair[j] >> 8;
	tmprec[1] = pair[j] & 0xff;
	pw = eucGetChar(pw, tmprec);
      }
      *pw++ = ' ';
      while (len-- > 0) {
	pw = eucGetChar(pw, buf + i);
	i += 2;
      }
      *pw++ = ' ';
    }
    else {
      *pw++ = '.';
      i += len * 2;
    }
  }
  *pw = '\0';
  printf("%s \n", wrb);
  return i;
}

void
showword(dic, wrdoff, gram )
FILE		*dic;
unsigned long	wrdoff;
struct RkKxGram *gram;
{
  unsigned char	l4[4], *buf;
  unsigned long	wrdsiz;
  int			i;
  
  fseek(dic, (int)wrdoff-4, 0);
  (void)fread(l4, 4, 1, dic); /* ワード部のサイズを読む */
  
  wrdsiz = L4TOL(l4);
  buf = (unsigned char *)malloc(wrdsiz);
  printf("\nword size = %d\n", wrdsiz);
  (void)fread(buf, wrdsiz, 1, dic);
  for (i = 0 ; i < wrdsiz ;) {
    i = printword(gram, buf, (int)wrdsiz, i, 1);
  }
  if (i != wrdsiz) {
    printf("0x%04x\n", i);
  }
}

int
dumpOffset(dic, gram, dir, dirsiz, wrdoff)
FILE		*dic;
struct RkKxGram *gram;
unsigned char	*dir;			/* ディレクトリ部用の領域 */
unsigned long	dirsiz;
unsigned long	wrdoff;			/*  */
{
  showdispatch(dir, dirsiz);
  showword(dic, wrdoff, gram );
}


main(argn, args)
int	argn;
char	**args;
{
    extern	Convert();
    struct RkKxGram *gram = (struct RkKxGram *)0;
    char	*gramname = (char *)0;	/* 文法辞書ファイル名 */
    char	*cnjname = (char *)0;
    FILE	*dic;
    int		ii;
    int		sflag = 0;		/* サイズ出力情報用フラグ */
    int		conv_flag = 0;		/* for dump old dic */
    int		is_sort = 0;
    
    if ( argn == 1 ) {
	usage( args[0] );
	exit( 1 );
    }
    
    invert = 0;
    P = Pat;
    *P = 0;
    tree = 0;
    for (ii = 1 ; ii < argn && args[ii][0] == '-' ; ii++) {
	if( !strcmp( args[ii], "-D" ) ) {	/* 文法辞書指定 */
	    if (++ii < argn && !gramname) {
		gramname = args[ii];
		continue;
	    }
	}
	else if (!strcmp( args[ii], "-d" )) {	/* 接続テーブルのみを指定 */
	    if ( ++ii < argn && !cnjname) {
		cnjname = args[ii];
		continue;
	    }
	}
	else if( !strcmp( args[ii], "-i" ) ) {	/* 読みと候補を逆に出力 */
	    invert = 1;
	    continue;
	}
	else if( !strcmp( args[ii], "-y" ) ) {	/* マッチした語のみ出力 */
	    strcpy(P + 1, args[ii] + 2);
	    continue;
	}
	else if( !strcmp( args[ii], "--" ) ) {	/* ディレクトリ、ワードサイズ出力 */
	    sflag=1;
	    continue;
	}
	else if( !strcmp( args[ii], "-t" ) ) {	/* tree構造で出力 */
	    tree = 1;
	}
	else if( !strcmp( args[ii], "-T" ) ) {	/* tree構造で出力 */
	    tree = 2;
	}
	else if (!strcmp( args[ii], "-S" ) ) {	/* 旧辞書を freq でソート */
	    is_sort = 1;
	}
	else
	    usage( args[0] );
    }
    
    /* 文法辞書の設定 */
    if ( !cnjname ) {
	if( !gramname ) {
	    gramname = HYOUJUN_GRAM;
	}
	if( !(gram = RkOpenGram(gramname, 0)) ) {
	    fprintf(stderr,
		    "Warning: can't open gramfile %s.\n",
		    gramname);
	}
    }
    else {
	if ( !(gram = OpenCnj(cnjname)) ) {
	    (void)fprintf(stderr,
			  "Warning: can't open gramfile %s.\n",
			  gramname);
	}
    }
    
    if( ii >= argn ) {
	usage( args[0] );
	exit(1);
    }
    
    if ( !(dic = fopen(args[ii], "r")) ) {
	fprintf(stderr, "%s: Can't open %s\n", args[0], args[ii]);
	exit(1);		/* ii は後で使う */
    }
    
    if( !binname[0] ) {			/* バイナリ辞書名セット */
	(void)strcpy( binname, basename(args[ii]) );
    }
    
    for (;;) {
	unsigned char	hdr[ND_HDRSIZ];
	unsigned char	key[ND_HDRSIZ];
	unsigned char	l4[4];
	unsigned char	*dir;
	unsigned long	dirsiz, wrdsiz, wrdoff, cnjsiz, count;
	int		i;

	if( (conv_flag = ckhder( dic, hdr )) < 0) {
	    break;
	};
	l4[0] = getc(dic); l4[1] = getc(dic);
	l4[2] = getc(dic); l4[3] = getc(dic);
	dirsiz = L4TOL(l4);			/* ディレクトリサイズ */
	if( dir = (unsigned char *)malloc(dirsiz) ) {
	  (void)fread(dir, dirsiz, 1, dic);
	}
	else {
#ifdef SEEK_CUR
	    fseek(dic, dirsiz, SEEK_CUR);
#else
	    fseek(dic, dirsiz, 1);
#endif
	}
	l4[0] = getc(dic); l4[1] = getc(dic);
	l4[2] = getc(dic); l4[3] = getc(dic);
	wrdsiz = L4TOL(l4);			/* ワードサイズ */
	wrdoff = ftell(dic);
	PrintHdr( hdr, key );

	if( sflag ) {	/* 各サイズの表示 */
	    fprintf(stderr, "\tdirsize %d, wrdsize %d\n", dirsiz, wrdsiz);
	}
	
	for(  i = ii+1 ; ii < argn && args[i]; i++ ) {
	    /* ii は最初の for で使ったやつ */
	    if( !strcmp((char *)key, args[i]) )  {
		if( tree == 1 ) {		/* tree表現で出力する */
		  dumpDir(dic, gram, dir, dirsiz, wrdoff);
		}
		else if( tree == 2 ) {
		  dumpOffset(dic, gram, dir, dirsiz, wrdoff);
		}
		else {			/* テキスト形式で出力する */
		    for ( count = wrdsiz; count > 0; ) {
			count -= dumpWord(dic, gram);
		    }
		}
		cnjsiz = 0;
		goto end;
	    }
	}
	cnjsiz = 0;
	
	fseek(dic, wrdoff + wrdsiz, 0);
	
	/* swd ならば、接続情報を読み飛ばす */
	if ( !strcmp((char *)&key[-3], "swd") ) {
	    l4[0] = getc(dic); l4[1] = getc(dic);
	    l4[2] = getc(dic); l4[3] = getc(dic);
	    cnjsiz = L4TOL(l4);
	    fseek(dic, cnjsiz, 1);
	}
      end:
	if( dir ) {
	    free(dir);
	}
    }
    if( gram )
      RkCloseGram( gram );
    fclose(dic);
    if (conv_flag == -2) {
	if (ii < argn)
	    Convert(args[ii], args[ii + 1], is_sort);
    };
    exit(0);
    /* NOTREACHED */
}
