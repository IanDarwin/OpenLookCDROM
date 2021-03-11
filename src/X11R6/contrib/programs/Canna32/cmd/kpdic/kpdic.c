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
static char rcsid[]="@(#) 102.1 $Id: kpdic.c,v 4.26 1993/10/20 01:24:30 hamada Exp $";
#endif

#if __STDC__ || defined(SVR4)
#include <locale.h>
#endif

#ifdef SVR4
extern char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif

#if __STDC__
#include <stdlib.h>
#else
char *malloc();
#endif


#include	<stdio.h>
#include	<ctype.h>

#define		MAXKEY	4096

#define LOMASK(x)	((x)&255)

static char	fileName[256];
static int	lineNum;
static int	errCount;

	struct  def_tbl {
	    int   used  ;
	    char  *roma ; 
	    char  *kana ;
	    char  *intr ;
        }    ;
	static struct def_tbl def [] = { 
	    {0,"kk","っ","k"},
	    {0,"ss","っ","s"},
	    {0,"tt","っ","t"},
	    {0,"hh","っ","h"},
	    {0,"mm","っ","m"},
	    {0,"yy","っ","y"},
	    {0,"rr","っ","r"},
	    {0,"ww","っ","w"},
	    {0,"gg","っ","g"},
	    {0,"zz","っ","z"},
	    {0,"dd","っ","d"},
	    {0,"bb","っ","b"},
	    {0,"pp","っ","p"},
	    {0,"cc","っ","c"},
	    {0,"ff","っ","f"},
	    {0,"jj","っ","j"},
	    {0,"qq","っ","q"},
	    {0,"vv","っ","v"}
	}  ; 


/*VARARGS*/
void
alert(fmt, arg)
char	*fmt;
char	*arg;
{
    char	msg[256];
    (void)sprintf(msg, fmt, arg);
    (void)fprintf(stderr, gettxt("cannacmd:23", 
		 "#line %d %s: (WARNING) %s\n"), lineNum, fileName, msg);
    ++errCount;
}
void
fatal(fmt, arg)
char	*fmt;
char	*arg;
{
    char	msg[256];
    (void)sprintf(msg, fmt, arg);
    (void)fprintf(stderr, gettxt("cannacmd:24", 
		 "#line %d %s: (FATAL) %s\n"), lineNum, fileName, msg);
    exit(1);
}

int
getWORD(s, news, word, maxword)
unsigned char	*s, **news;
unsigned char	*word;
int		maxword;
{
    unsigned 	c;
    int	 	i;

    i = 0;
    while ( *s && *s <= ' ' )
	s++;
    while ( (c = *s) > ' ' ) {
	s++;
	if ( c == '\\' ) {
	    switch(*s) {
	    case 0:
		break;
	    case '0':
		if ( s[1] == 'x' && isxdigit(s[2]) && isxdigit(s[3]) ) {
		    unsigned char   xx[3];

		    s += 2;
		    xx[0] = *s++; xx[1] = *s++; xx[2] = 0;
		    sscanf((char *)xx, "%x", &c);
		}
		else {
		    c = 0;
		    while ( isdigit(*s) ) 
			c = 8*c + (*s++ - '0');
		};
		break;
	    default:
		c = *s++;
		break;
	    };
	};
	if ( i < maxword - 1 )
	    word[i++] = c;
    };
    word[i] = 0;
    *news = s;
    return i;
}

unsigned char
*allocs  (s)
unsigned char	*s;
{
    unsigned char	*d;

    if ( d = (unsigned char *)malloc(strlen(s) + 1) )
	 strcpy(d, s);
    return d;
}

struct roman { 
    unsigned char	*roma;
    unsigned char	*kana;
    unsigned char	*temp;
    int                 bang;
};

static void
freeallocs(roman, nKey)
struct roman *roman;
int nKey;
{
  int i;

  for (i = 0 ; i < nKey ; i++) {
    /* free them */
    free((char *)roman[i].roma); roman[i].roma = (unsigned char *)0;
    free((char *)roman[i].kana); roman[i].kana = (unsigned char *)0;
    if (roman[i].temp) {
      free((char *)roman[i].temp); roman[i].temp = (unsigned char *)0;
    }
  }
}

int
compar(p, q)
struct roman	*p, *q;
{	
    unsigned char	*s = p->roma;
    unsigned char	*t = q->roma;

    while ( *s == *t )
	if ( *s )
	    s++, t++;
	else
	    return 0;
    return ((int)*s) - ((int)*t);
}

main(argc, argv)
  int    argc ;
  char **argv ; 
{
  struct roman *roman;
  unsigned char	rule[256], *r;
  int			nKey, size;
  int			i, p;
  int                   flag_old ;
  int                   werr ;
  unsigned char	l4[4], *bangchars = 0, *pp;

  roman = (struct roman *)malloc(sizeof(struct roman) * MAXKEY);
  if (!roman) {
    fatal(gettxt("cannacmd:8", "No more memory\n"), 0); 
  }

#if __STDC__  || defined(SVR4)
    (void)setlocale(LC_ALL,"");
#endif

/* option */
    flag_old =  0 ; 
    werr = 0 ; 
    while(--argc) {
    	argv++ ;
        if (!strcmp(*argv,"-m")) {
    		flag_old = 1 ; 
        }
    }

  nKey = 0;
  size  = 0;
  while (fgets((char *)(r = rule), sizeof(rule), stdin)) {
    unsigned char	roma[256];
  
    lineNum++;
    if ( *r == '#' ) {
      continue;
    }
    if ( getWORD(r, &r, roma, sizeof(roma)) ) {
      if ( nKey < MAXKEY ) {
	for ( i = 0; i < nKey; i++ ) {
	  if ( !strcmp(roman[i].roma, roma) ) {
	    break;
	  }
	}
	if ( i < nKey ) {
	  alert(gettxt("cannacmd:25", "multiply defined key <%s>"), roma);
	  continue;
	}
	roman[nKey].roma = allocs(roma);
      }
      else {
	  freeallocs(roman, nKey);
	  free((char *)roman);
	  fatal(gettxt("cannacmd:26", 
	       "More than %d romaji rules are given."), MAXKEY);
      }
      if ( getWORD(r, &r, roma, sizeof(roma)) ) {
	roman[nKey].kana = allocs(roma);
	roman[nKey].temp = (unsigned char *)0;
	roman[nKey].bang = 0;
	if ( getWORD(r, &r, roma, sizeof(roma)) ) {
	  roman[nKey].temp = allocs(roma);
	  if ( getWORD(r, &r, roma, sizeof(roma)) ) {
	    roman[nKey].bang = 1;
	  }
	}
        size += strlen(roman[nKey].roma) + 1 +
	        strlen(roman[nKey].kana) + 1 +
	        (roman[nKey].temp ? strlen(roman[nKey].temp) : 0) + 1;

/*  add  */
	if (flag_old == 1) {
	  if (roman[nKey].temp && 0) {
	    /* free them */
	    free((char *)roman[nKey].roma);
	    free((char *)roman[nKey].kana);
	    free((char *)roman[nKey].temp);
	    roman[nKey].roma = (unsigned char *)0;
	    roman[nKey].kana = (unsigned char *)0;
	    roman[nKey].temp = (unsigned char *)0;
	    nKey--; /* ひとつ戻しておく */

	    werr = 1;
	  }
	  else {
	    p = chk_dflt((char)roman[nKey].roma[0]);
	    if (p--) {
	      if (def[p].used == 0) { /* if not used */
		if (nKey < MAXKEY) {
		  nKey++ ; 
		  roman[nKey].roma = allocs(def[p].roma);
		  roman[nKey].kana = allocs(def[p].kana);
		  roman[nKey].temp = allocs(def[p].intr);
		  size += strlen(roman[nKey].roma) + 1
		        + strlen(roman[nKey].kana) + 1
		        + strlen(roman[nKey].temp) + 1;
		  def[p].used = 1;
		}
		else {
		  freeallocs(roman, MAXKEY);
		  free((char *)roman);
		  fatal("more than %d romaji rules are given.", MAXKEY);
		}
	      }
	    }
	  }
	}

	nKey++;
      }
      else {
	if (roman[nKey].roma &&
	    roman[nKey].roma[0] == '!' &&
	    roman[nKey].roma[1] != (unsigned char)0) {
	  if (bangchars) {
	    free((char *)bangchars);
	  }
	  bangchars = allocs(roman[nKey].roma + 1);
	}
	else {
	  alert(gettxt("cannacmd:28", "syntax error"), 0);
	}
	if (roman[nKey].roma) {
	  free(roman[nKey].roma);
	  roman[nKey].roma = (unsigned char *)0;
	}
      }
    }
  }

  if ( errCount ) {
    freeallocs(roman, nKey);
    free((char *)roman);
    fatal(gettxt("cannacmd:29", "Romaji dictionary is not produced."), 0);
  }
  qsort((char *)roman, nKey, sizeof(struct roman), compar);
  putchar('K'); putchar('P');
  size += (bangchars ? strlen(bangchars) : 0) + 1;
  l4[0] = LOMASK(size >> 8); l4[1] = LOMASK(size);
  l4[2] = LOMASK(nKey >> 8); l4[3] = LOMASK(nKey);
  putchar(l4[0]); putchar(l4[1]); putchar(l4[2]); putchar(l4[3]);

  if (bangchars) {
    for (pp = bangchars ; pp && *pp ; pp++) {
      putchar(*pp);
    }
    free((char *)bangchars);
  }
  putchar('\0');

  for ( i = 0; i < nKey; i++ ) {
    r = roman[i].roma; do { putchar(*r); } while (*r++);
    r = roman[i].kana; do { putchar(*r); } while (*r++);
    if (roman[i].temp) {
      r = roman[i].temp; while (*r) putchar(*r++);
    }
    putchar(roman[i].bang); /* temp がなくて、bang が１はありえない */
  };
  freeallocs(roman, nKey);
  free((char *)roman);
  fprintf(stderr, gettxt("cannacmd:30", "SIZE %d KEYS %d\n"), size, nKey);
  if (werr == 1 ) 
    fprintf(stderr,gettxt("cannacmd:31",
	  "warning: Option -m is specified for new dictionary format.\n")) ;
  exit(0);
}

/* sub */
int
chk_dflt(c) char c ; {
    int  i,n ; 
    n = sizeof(def) / sizeof(struct def_tbl) ; 
    for (i=0; i < n ; i++) {
	if (c == def[i].intr[0]) {
	    return(i+1) ;
	}
    }
    return(0);
}


