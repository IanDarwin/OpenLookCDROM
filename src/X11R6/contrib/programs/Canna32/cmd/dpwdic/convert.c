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
static char rcsid[] = "$Id: convert.c,v 1.11 1993/03/01 08:26:23 kon Exp $";
#endif

#include	<stdio.h>
#ifdef SVR4
#include	<unistd.h>
#endif
#include	<fcntl.h>

#if __STDC__
#include <stdlib.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

static unsigned char *RkUparseGramNum();
static RkScanWcand(), matchpat(), readWrec();
static dumpWord(), dumpWrec();

#define	L4TOL(l4)\
	(((((((unsigned long)(l4)[0]<<8)|(l4)[1])<<8) | (l4)[2])<<8)|(l4)[3])
#define	S2TOS(s2)	(((unsigned short)(s2)[0]<<8)|(s2)[1])
#define	LTOL4(l, l4)	{\
	(l4)[0] = ((l)>>24)&255;\
	(l4)[1] = ((l)>>16)&255;\
	(l4)[2] = ((l)>>8)&255;\
	(l4)[3] = (l)&255;\
}

#define		ND_HDRSIZ		256
#define		ND_HDRMAG		(('N'<<8)|'D')
#define	ND_OFFMASK	0x001fffffL
#define	ND_LAST		0x00800000L
#define	ND_WORD		0x00400000L
#define	ND_NULLOFF	ND_OFFMASK
#define NOD2KEY(node)	(((node)>>24)&255)
#define NOD2OFF(node)	((node)&ND_OFFMASK)
#define	ISLASTNOD(node)	((node)&ND_LAST)
#define	ISWORDNOD(node)	((node)&ND_WORD)
#define	NW_MAXCAND	255
#define	NW_PREFIX	4
#define	NW_R256		0x80
#define	NW_C256		0x40
#define	NW_LEN		0x3f
#define	row256(word)	((word)&NW_R256)
#define	col256(word)	((word)&NW_C256)
#define	candlen(word)	((word)&NW_LEN)

struct RkWcand {
    unsigned char	*addr;		/* houho sentou address */
    short		rnum;		/* row number */
    short		cnum;		/* column number */
    unsigned char	klen;		/* kouho no nagasa */
    unsigned char	freq;		/* kouho hindo jouhou */
};

struct RkKxGram {
    int			ng_row;		/* row no kazu */
    int			ng_col;		/* col no kazu */
    int			ng_rowbyte;	/* row atari no byte suu */
    unsigned char	*ng_conj;	/* setuzoku gyouretu/code table */
    unsigned char	*ng_strtab;
};

#define	GetGramRow(g, r) 	((g)->ng_conj + (r)*(g)->ng_rowbyte)
#define	TestGram(cnj, col)	((cnj) && ((cnj)[((col)>>3)]&(0x80>>(col&7))))
#define	MAXPRIO		255


/*ARGSUSED*/
static unsigned char	*
RkUparseGramNum(gram, row, s)
struct RkKxGram	*gram;
register int		row;
register unsigned char	*s;
{
    if (s) {
	(void)sprintf((char *)s, "#%d", row);
	return s + strlen((char *)s);
    };
    return (unsigned char *)0;
}

static
RkScanWcand(wrec, word)
unsigned char	*wrec;
struct RkWcand	*word;
{
    register unsigned char	*w;
    register int		i, nc;
    
    for ( w = wrec; *w++;  );
    nc = *w++;
    for ( i = 0; i < nc; i++ ) {
	unsigned	flags;
	
	word->addr = w;
	flags = *w++;
	word->rnum = *w++; if ( row256(flags) ) word->rnum += 256;
	word->cnum = *w++; if ( col256(flags) ) word->cnum += 256;
	word->freq = *w++;
	word->klen = candlen(flags);
	w += candlen(flags);
	word++;
    };
    return nc;
}


static unsigned	Pak;
static unsigned char	Pat[256], *P;

static matchpat(text)
unsigned char	*text;
{
    unsigned char	*pat;

    for ( pat = Pat; *pat; pat += strlen((char *)pat) + 1 ) {
	int	plen, tlen;

	plen = strlen((char *)(pat + 1));
	tlen = strlen((char *)text);
	if ( tlen < plen )
	    continue;

	switch(pat[0]) {
	case	'P':
	    if ( !strncmp((char *)text, (char *)&pat[1], plen) )
		    return(1);
	    break;
	case	'S':
	    if ( !strncmp((char *)&text[tlen-plen], (char *)&pat[1], plen) )
		    return(1);
	    break;
	case	'I': {
	    unsigned char	*t;
	    int	w;

	    for ( t = text; tlen >= plen; t += w, tlen -= w ) {
		w = (*t&0x80) ? 2 : 1;
		if ( !strncmp((char *)t, (char *)&pat[1], plen) )
		    return(1);
	    };
	    break;
	};
	};
    };
    return(0);
}

static readWrec(dic, wrec)
FILE		*dic;
unsigned char	*wrec;
{
    unsigned char	*w;
    int			nk, flags, len;

    for ( w = wrec; *w++ = getc(dic);  );
    *w++ = nk = getc(dic);	
    while ( nk-- > 0 ) {
	flags = *w++ = getc(dic);
	*w++ = getc(dic);
	*w++ = getc(dic);
	*w++ = getc(dic);	
	for ( len = candlen(flags); len--; )
	    *w++ = getc(dic);
    };
    return w - wrec;
}

static	Gomi, Addr, Page = 1, List, Word, Old, New;

static int
_compare(x, y)
  struct RkWcand	*x, *y;
{
    register int	d = y->freq - x->freq;
    if (!d) {
	return x->addr - y->addr;
    };
    return d;
}

static
dumpWord(dic, gram, is_sort)
FILE	*dic;
struct RkKxGram	*gram;
int	is_sort;
{
    unsigned char	wrec[1024], *w, c;
    unsigned char	yomi[1024], *y;
    struct RkWcand	word[NW_MAXCAND];
    unsigned		len;
    unsigned char	*k;
    int		i, nk, sz;
    unsigned	oldrow, oldcol, oldlen;
    char		pair[256];
    int			count;

    sz = readWrec(dic, wrec);
    List++;
    if ( Addr + sz >= 1024 ) {
	Gomi += 1024 - Addr;
	Addr = 0;
	Page++;
    };
    Addr += sz;

    y = yomi;
    w = wrec;
    do {
	if ( (c = *w++) & 0x80 )
	    if ( Pak )
		*y++ = Pak;
	*y++ = c;
    } while ( c );

    if ( Pat[0] && !matchpat(yomi) ) 
	return sz;

    nk = RkScanWcand(wrec, word);
    Word += nk;
    oldrow = oldcol = oldlen = -1;
    for ( i = 0; i < nk; i++ ) {
	if ( oldrow!=word[i].rnum||
	    oldcol!=word[i].cnum||
	    oldlen!=word[i].klen) {
	    oldrow = word[i].rnum;
	    oldcol = word[i].cnum;
	    oldlen = word[i].klen;
	    Old += 4 + oldlen;
	    New += 2 + oldlen;
	    count = 0;
	}
	else {
	    Old += 4 + oldlen;
	    New += oldlen;
	    if ( !count++ )
		New += 2;
	};
    };
    oldrow = oldcol = -1;
    if (is_sort)
	(void)qsort((char *)word, (unsigned)nk,
		    sizeof(struct RkWcand), _compare);
    for ( i = 0; i < nk; i++ ) {
	if ( oldrow != word[i].rnum  || oldcol != word[i].cnum ) {
	    if (word[i].rnum > 94 || word[i].cnum > 94) {
#ifdef DEBUG
		(void)fprintf(stderr, "[%d:%d] 付属語は変換できません。\n",
			      word[i].rnum, word[i].cnum);
#endif
		continue;
	    } else {
		RkUparseGramNum(gram, word[i].rnum, (unsigned char *)pair);
	    };
	    oldrow = word[i].rnum;
	    oldcol = word[i].cnum;
	};
	printf("%s ", yomi);
	printf("%s ", pair);
	if ( len = word[i].klen ) 
	    for ( k = word[i].addr + 4; len--;  )
		putchar(*k++);
	else
	    printf("%s", yomi);
	printf("\n");
    };
    return sz;
}

Convert(file, dictionary, is_sort)
char	*file, *dictionary;
int	is_sort;
{
    struct RkKxGram	*gram = (struct RkKxGram *)0;
    FILE		*dic;

/*
  printf("usage: dpxdic [-xiu] [-Dbunpou] <file name> [dictionary-name]\n");
*/
    P = Pat;
    *P = 0;

    if ( !(dic = fopen(file, "r")) ) {
	fprintf(stderr, "cannot open %s\n", file);
	exit(1);
    }
    for (;;) {
	unsigned char	hdr[ND_HDRSIZ], *h;
	unsigned char	key[256], *d;
	unsigned char	l4[4];
	unsigned long	*dir;
	long		dirsiz, wrdsiz, wrdoff, cnjsiz, count;

	if ( !fread((char *)hdr, 1, ND_HDRSIZ, dic) )
	    break;
	if ( S2TOS(hdr) != ND_HDRMAG )
	    break;
	Pak = hdr[2];
	h = hdr + 3;
	for ( d = key; (*d = *h++) != '\n'; d++ );
	*d = 0;

	l4[0] = getc(dic); l4[1] = getc(dic);
	l4[2] = getc(dic); l4[3] = getc(dic);
	dirsiz = L4TOL(l4);
	if ( dir = (unsigned long *)malloc((unsigned)dirsiz) ) 
	    (void)fread((char *)dir, (unsigned)dirsiz, 1, dic);
	else
	    (void)fseek(dic, dirsiz, 1);
	l4[0] = getc(dic); l4[1] = getc(dic);
	l4[2] = getc(dic); l4[3] = getc(dic);
	wrdsiz = L4TOL(l4);
	wrdoff = ftell(dic);
	
	if (dictionary) {
	    if ( !strcmp((char *)key, dictionary) )  {
	        if ( !strcmp((char *)&d[-3], "swd") ) {
		    (void)fprintf(stderr, "付属語辞書は変換できません。\n");
		    exit(1);
		};
		for ( count = wrdsiz; count > 0; ) 
		    count -= dumpWord(dic, gram, is_sort);
		cnjsiz = 0;
		goto end;
	    };
	};
	if (dir) 
	    (void)free((char *)dir);
	cnjsiz = 0;
	(void)fseek(dic, wrdoff + wrdsiz, 0);
/* swd naraba, setuzoku jouhou mo yomitobasu */
	if ( !strcmp((char *)&d[-3], "swd") ) {
	    l4[0] = getc(dic); l4[1] = getc(dic);
	    l4[2] = getc(dic); l4[3] = getc(dic);
	    cnjsiz = L4TOL(l4);
	    (void)fseek(dic, cnjsiz, 1);
	};
    end:
	(void)fprintf(stderr,
	    "%s [canna dic Ver 1.1] = %ld + %ld + %ld pak %02x  SAVE %d\n",
		key, dirsiz, wrdsiz, cnjsiz, Pak, Gomi);
/*
	(void)fprintf(stderr, "%s = %ld + %ld + %ld pak %02x  SAVE %d\n",
		      key, dirsiz, wrdsiz, cnjsiz, Pak, Gomi);
*/
	;
    };
    (void)fclose(dic);
    exit(0);
}

#ifdef DEBUG
main (argc, argv)
  int argc;
  char *argv [];
{
    int	res;
    
    if (argc == 3) {
	res = Convert(argv[1], argv[2]);
    } else if (argc == 2) {
	res = Convert(argv[1], (char *)0);
    } else {
	printf("usage\n");
	exit(1);
    }
    exit(res);
}
#endif
