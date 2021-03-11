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
static char rcsid[]="@(#) 112.1 $Id: crrdic.c,v 1.13 1993/07/05 13:18:05 kon Exp $";
#endif

#include	<stdio.h>
#include	<ctype.h>
#include	<canna/RK.h>

#if __STDC__
#include        <stdlib.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

#if  __STDC__ || defined(SVR4)
#include <locale.h>
#endif

#ifdef SVR4
extern char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif

#define LOMASK(x)	((x)&255)

#define		MAXKEY	1024


static char	fileName[256];
static int	lineNum;
static int	errCount;

/*VARARGS*/
void
alert(fmt, arg)
char	*fmt;
char	*arg;
{
    char	msg[256];
    (void)sprintf(msg, fmt, arg);
    (void)fprintf(stderr, gettxt("cannacmd:1", 
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
    (void)fprintf(stderr, gettxt("cannacmd:2", 
		 "#line %d %s: (FATAL) %s\n"), lineNum, fileName, msg);
    exit(1);
}

unsigned char	*
getWORD(s, word, maxword)
unsigned char	*s;
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
		    break;
		}
		else {
		    c = 0;
		    while ( isdigit(*s) ) 
			c = 8*c + (*s++ - '0');
		    break;
		};
	    default:
		c = *s++;
		break;
	    };
	};
	if ( i < maxword - 1 )
	    word[i++] = c;
    };
    word[i++] = 0;
    return s;
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
};

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

main()
{
    struct roman	roman[MAXKEY];
    unsigned char	rule[256], *r;
    int			nKey, size;
    int			i;
    unsigned char	l4[4];

#if  __STDC__ || defined(SVR4)
     (void)setlocale(LC_ALL,"");
#endif

    nKey = 0;
    size  = 0;
    while (fgets((char *)(r = rule), sizeof(rule), stdin)) {
	unsigned char	roma[256], kana[256];

	lineNum++;
	if ( *r == '#' )
	    continue;
	r = getWORD(r, roma, sizeof(roma));
	if ( roma[0] ) {
	    r = getWORD(r, kana, sizeof(kana));
	    if ( kana[0] ) {
		for ( i = 0; i < nKey; i++ ) 
		    if ( !strcmp(roman[i].roma, roma) ) 
			break;
		if ( i < nKey ) {
		    alert(gettxt("cannacmd:3", 
				 "multiply defined key <%s>"), roma);
		    continue;
		};
		if ( nKey < MAXKEY ) {
		    roman[nKey].roma = allocs(roma);
		    roman[nKey].kana = allocs(kana);
		    size += strlen(roma) + 1 + strlen(kana) + 1;
		    nKey++;
		}
		else
		    fatal(gettxt("cannacmd:4", 
			 "More than %d romaji rules are given."), MAXKEY);
	    }
	    else 
		alert(gettxt("cannacmd:5", "syntax error"), 0);
	};
    };

    if ( errCount )
	fatal(gettxt("cannacmd:6", "Romaji dictionary is not produced."), 0);
    qsort((char *)roman, nKey, sizeof(struct roman), compar);
    putchar('R'); putchar('D');
    l4[0] = LOMASK(size >> 8); l4[1] = LOMASK(size);
    l4[2] = LOMASK(nKey >> 8); l4[3] = LOMASK(nKey);
    putchar(l4[0]); putchar(l4[1]); putchar(l4[2]); putchar(l4[3]);

    for ( i = 0; i < nKey; i++ ) {
	r = roman[i].roma; do { putchar(*r); } while (*r++);
	r = roman[i].kana; do { putchar(*r); } while (*r++);
    };
    fprintf(stderr, gettxt("cannacmd:7", "SIZE %d KEYS %d\n"), size, nKey);
    exit(0);
}

