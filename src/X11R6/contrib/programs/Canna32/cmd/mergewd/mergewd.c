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
static char rcsid[]="@(#) 112.1 $Id: mergewd.c,v 1.23 1994/04/19 10:45:35 kon Exp $";
#endif

#include	"RKintern.h" 
#include	<stdio.h>

#if  __STDC__ || defined(SVR4)
#include <locale.h>
#endif

#ifdef SVR4
extern char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif

#define RkwIsGraphicChar(x) ((unsigned long)(x) > (unsigned long)' ')
#define RkwIsControlChar(x) ((unsigned long)(x) < (unsigned long)' ')

/* rec  = klen + hinshi + kanji */
/*        1    + 1      + ?     */
/* wrec = ylen  + yomi  + knum  + fqoffset + rec */
/*        1byte + ?byte + 2byte + 3byte    + rec */
#define ckrecSiz(k)		(NW_PREFIX+us_len(k))
#define ckwrecSiz(y,s)		(1+us_len(y)+2+3+s)
#ifndef AIXV3
typedef unsigned char	uchar;
#endif


int
us_len(us)
  unsigned short *us;
{
    unsigned short *ous = us;
    
    if (!us)
	return 0;
    while (*us & RK_WMASK)
	us++;
    return (us - ous);
}

Wchar *
euctous(src, srclen, dest, destlen)
  unsigned char		*src;
  Wchar			*dest;
  int			srclen, destlen;
{
    unsigned short	*a=dest;
    
    if (!src || !dest || !srclen || !destlen)
	return a;
    while (*src && (srclen-- > 0) && (destlen-- > 0)) {
	if (!(*src & 0x80) ) {
	    *dest++ = (Wchar)*src++;
	} else if (srclen-- > 0) {
	    if (*src == RK_SS2) {
		src++;
		*dest++ = (Wchar)(0x0080 | (*src++ & 0x7f));
	    } else if ((*src == RK_SS3) && (srclen-- > 0)) {
		src++;
		*dest++ = (Wchar)(0x8000 |
				  ((src[0] & 0x7f) << 8) |
				  (src[1] & (0x7f)));
		src += 2;
	    } else {
		*dest++ = (Wchar)(0x8080 |
				  ((src[0] & 0x7f) << 8) |
				  (src[1] & 0x7f));
		src += 2;
	    }
	} else {
	    break;
	}
    }
    if (destlen-- > 0) {
      *dest = (unsigned short)0;
    }
    return dest;
}



main(n, args)
int	n;
char	*args[];
{
    uchar	Yomi[RK_LINE_BMAX], Pair[RK_LINE_BMAX], Kanji[RK_LINE_BMAX*10];
    uchar	yomi[RK_LINE_BMAX], pair[RK_LINE_BMAX], kanji[RK_LINE_BMAX*10];
    uchar	S[RK_LINE_BMAX*10], *s, *d;
    uchar	fkanji[RK_LINE_BMAX*10];
    Wchar	wyomi[RK_LINE_BMAX], wkanji[RK_LINE_BMAX*10];
    int		kouho = 0;
    int		wrec;
    int		krec = 0;
    int		rec = 0;
    int		first = 0;
    FILE *fp;
    
#if  __STDC__ || defined(SVR4)
    (void)setlocale(LC_ALL,"");
#endif
    /*  first = !strcmp(args[1], "-f"); */
    Yomi[0] = Pair[0] = Kanji[0] = 0;

    if (n < 2 || !(fp = fopen(args[1], "r"))) {
      fp = stdin;
    }

    while (fgets((char *)(s = S), sizeof(S), fp)) {
      s[strlen(s) - 1] = '\0';
      if (!RkwIsGraphicChar(S[0]) || S[0] == '#' )
	continue;
      d = yomi;
      while (RkwIsGraphicChar(*d = *s++)) d++;
      *d = 0;
      while (*s && !RkwIsGraphicChar(*s)) s++;
      d = pair;
      while (RkwIsGraphicChar(*d = *s++)) d++;
      *d = 0;
      while (*s && !RkwIsGraphicChar(*s)) s++;
      d = kanji;
      while ( *d = *s++ )	d++;
      
      euctous(yomi, strlen((char *)yomi), wyomi, sizeof(yomi)/sizeof(Wchar));
      euctous(kanji, strlen((char *)kanji), wkanji, sizeof(kanji)/sizeof(Wchar));
      if ( !strcmp(Yomi, yomi) ) {
	if ( !first ) {
	  if ( strcmp(Pair, pair ) ) {     /* 品詞が異なる */
	    rec += ckrecSiz(wkanji);
	    wrec = ckwrecSiz(wyomi, rec);
	    /* ここはワードレコードの大きさは無視したい */
	    if( (kouho < RK_CAND_NMAX) && (wrec <= RK_WREC_BMAX) ) {
	      strcpy(Pair, pair);
	      strcat(Kanji, " ");
	      strcat(Kanji, Pair);
	      
	      strcat(Kanji, " ");
	      strcat(Kanji, kanji);
	      strcpy(fkanji, kanji);
	      krec += ckrecSiz(wkanji);
	    }
	    else
	      (void)fprintf(stderr, gettxt("cannacmd:32",
			   "%s: over word [%d %d]\n"), Yomi, kouho, rec);
	    kouho++;
	  }
	  else if( strcmp(fkanji,kanji )){
	    rec += ckrecSiz(wkanji);
	    wrec = ckwrecSiz(wyomi, rec);
	    /* ここはワードレコードの大きさは無視したい */
	    if( (kouho < RK_CAND_NMAX) && (wrec <= RK_WREC_BMAX*2) ) {
	      strcat(Kanji, " ");
	      strcat(Kanji, kanji);
	      strcpy(fkanji, kanji);
	      krec += ckrecSiz(wkanji);
	    }
	    else 
	      (void)fprintf(stderr, gettxt("cannacmd:33",
			   "%s: *over word [%d %d]\n"), Yomi, kouho, rec);
	    kouho++;
	  }
	}
      }
      else {
	if ( Yomi[0] ) {                 /* 読みが異なる */
	  wrec = ckwrecSiz(wyomi, rec);
	  printf("%s%s\n", Yomi, Kanji);
	  /* ここはチェックのため */
	  if( (kouho > RK_CAND_NMAX) || (wrec > RK_WREC_BMAX) )
	    (void)fprintf(stderr, gettxt("cannacmd:34",
			 "%s: over word [%d %d]\n"), Yomi, kouho, rec);
	  if (strlen(Yomi)+strlen(Kanji)+1 >= RK_LINE_BMAX ) {
	    (void)fprintf(stderr,gettxt("cannacmd:35", "too long line\n"));
	  }
	}
	strcpy(Yomi, yomi);
	strcpy(Pair, pair);
	strcpy(fkanji, kanji);
			
	strcpy(Kanji, " ");
	strcat(Kanji, Pair);
	strcat(Kanji, " ");
	strcat(Kanji, kanji);
	krec += ckrecSiz(wkanji);
	rec = 0;                    /* 修正 = krec */
	krec = 0;
	kouho = 1;
      }
    }

    if (fp != stdin) {
      fclose(fp);
    }
    
    if ( Yomi[0] ) {
      wrec = ckwrecSiz(wyomi, rec);
      printf("%s%s\n", Yomi, Kanji);
      /* ここはチェックのため */
      if( (kouho > RK_CAND_NMAX) || (wrec > RK_WREC_BMAX) )
	(void)fprintf(stderr, gettxt("cannacmd:36", "%s: over word [%d %d]\n"), Yomi, kouho, rec);
      if (strlen(Yomi)+strlen(Kanji)+1 >= RK_LINE_BMAX ) {
	(void)fprintf(stderr,gettxt("cannacmd:37", "too long line\n"));
      }
    }
    exit(0);
}
