#ident  "@(#)wcstocts.c	1.3    93/06/28 SMI"
 
/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */
 
/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */
 
#include <stdlib.h>
#include <widec.h>
#include <locale.h>
#include <stdio.h>      
#include "charset.h"

static int	makeGL(), makeGR();
#define	RESET	(-1)
/* wcstocts() converts a wide character string to the corresponding
 * Compound Text string.
 * The function currenly supports only the japanese locale.
 * The Comound Text allows two kinds of mapping of JIS X0208, GR and GL.
 * This function uses the GR mapping.
 */

int
wcstocts(cts, wcs, ncts)
     char	cts[];
     wchar_t	*wcs;
     int	ncts;
{
	wchar_t			wc;
	register wchar_t	*pwc = wcs;
	register char		*pcts = cts;
	register int		n = ncts;
	register int		i;
	int			j;

	makeGL(RESET, 0, 0); /* Reset the internal state of makeGL()... */
	makeGR(RESET, 0, 0); /* ... and makeGR().			*/

	while (wc = *pwc++)
	    switch(wcsetno(wc)) {
		  case 0:	/* ASCII to GL */
		    i = makeGL(ASCII, pcts, n);
		    if (i < 0) return -1;
		    pcts += i; n -= i;
		    if (n <= 1) return -1; /* Not enough space */
		    *pcts++ = (char)WCHAR_BYTE_OF(wc, 0); --n;
		    *pcts = 0;
		    break;

		  case 1:	/* JIS X0208 to GR */
                    if (strcmp("japanese",setlocale(LC_CTYPE,NULL)) == 0)
                        i = makeGR(JISX0208, pcts, n);
                    else if (strcmp ("korean",setlocale(LC_CTYPE,NULL)) == 0)
                        i = makeGR(KSC5601, pcts, n);   
                    else if (strcmp ("chinese",setlocale(LC_CTYPE,NULL)) == 0)
                        i = makeGR(GB2312, pcts, n);   
		    else if (strcmp("tchinese",setlocale(LC_CTYPE,NULL)) == 0)
		        i = makeGR(CNS1,pcts,n);

		    if (i < 0) return -1;
		    pcts += i; n -= i;
		    if (n <= 2) return -1; /* Not enough space */
		    *pcts++ = (char)(WCHAR_BYTE_OF(wc,1)|0x80);
		    *pcts++ = (char)(WCHAR_BYTE_OF(wc,0)|0x80);
		    n -= 2;
		    *pcts = 0;
		    break;

		  case 2:	/* JIS X0201 to GR */
		    if (strcmp("tchinese",setlocale(LC_CTYPE,NULL)) == 0) {
		      if ((WCHAR_BYTE_OF(wc,2) & 0xf0) == 0x20)
			i = makeGRP(CNS2, pcts, n, WCHAR_BYTE_OF(wc,2)-0x20);
		      else
			i = makeGRP(CNS3, pcts, n, WCHAR_BYTE_OF(wc,2)-0x20);
		      if (i < 0) return -1;
		      pcts += i; n-=i;
		      if (n <= 2) return -1; /* Not enough space */
		      *pcts++ = (char)(WCHAR_BYTE_OF(wc,1)|0x80);
		      *pcts++ = (char)(WCHAR_BYTE_OF(wc,0)|0x80);
		      n -= 2;
		    }
		    else {
		      i = makeGR(JISX0201R, pcts, n);
		      if (i < 0) return -1;
		      pcts += i; n-=i;
		      if (n <= 1) return -1; /* Not enough space */
		      *pcts++ = (char)(WCHAR_BYTE_OF(wc,0)|0x80); --n;
		    }
		    *pcts = 0;
		    break;

		  default:/* Gaiji or other junk */
		    return -1;
	    }
	
	/* Return to the initial designation state. */
	i = makeGL(ASCII, pcts, n);
	if (i < 0) return -1;
	pcts += i; n -= i;

	i = makeGR(ISO8859_1R, pcts, n);
	if (i < 0) return -1;
	pcts += i; n -= i;

	if (n > 0)
		*pcts++ = 0;

	return pcts - cts;
}

/* makeGL() and makeGR() ensures that the each codeset has
 * been designated the given character set.  If not, it stores
 * the neccessary Esc sequnce to designate the char set.
 * If successful, it returns the number of bytes stored.
 * If not enough space is available, -1 is returned.
 */
static int
makeGL(charset, to, n)
     register int	charset; /* -1 to initialize. */
     char		to[];
     int		n; /* Size of to[]. */
{
	static int	curGLCharset;
	char		seq[5];/* Mini buffer to store the Esc sequnce.*/
	register char	*pc=seq;

	if (charset == RESET){
		curGLCharset = ASCII;
		return 0;
	}
	if(charset == curGLCharset) return 0;/* Already designated. */
	else{/* Need to designated. */
		int	i;
		
		*pc++ = Esc;
		if (charset&_94S){/* 94 char set */
			*pc++=0x28;
		} else if (charset & _94M){/* multibyte 94 char set */
			*pc++ = 0x24;
			*pc++ = 0x28;
		} else {/* Unknown. */
			return -1;
		}
		*pc++ = (charset & 0xff);/* Put the "Final" char. */
		i = pc - to;
		if (i >= n) {/* Not enough space. */
			return -1;
		}
		*pc = 0;
		strncpy(to, seq, n);
		curGLCharset = charset;
		return i;
	}
}/*makeGL()*/

static int	curGRCharset;

static int
makeGR(charset, to, n)
     register int	charset; /* -1 to initialize. */
     char		to[];
     int		n; /* Size of to[]. */
{
	static int	curGRCharset;
	char		seq[5];/* Mini buffer to store the Esc sequnce.*/
	register char	*pc = seq;

	if (charset == RESET){
		curGRCharset = ISO8859_1R;
		return 0;
	}
	if (charset == curGRCharset) return 0;/* Already designated. */
	else {/* Need to designated. */
		int	i;
		
		*pc++ = Esc;
		if (charset & _94S) {/* 94 char set */
			*pc++ = 0x29;
		} else if (charset & _96S) {/* 96 char set */
			*pc++=0x2d;
		} else if(charset&_94M) {/* multibyte 94 char set */
			*pc++ = 0x24;
			*pc++ = 0x29;
		} else {/* Unknown. */
			return -1;
		}

		if (!strcmp("tchinese",setlocale(LC_CTYPE,NULL)))
		  *pc++ = 0x30; /* Put the "Final" char. */
		else
		  *pc++ = (charset & 0xff); /* Put the "Final" char. */

		i = pc - seq;
		if (i >= n) {/* Not enough space. */
			return -1;
		}
		*pc = 0;
		strncpy(to, seq, n);
		curGRCharset = charset;
		return i;
	}
}/*makeGR()*/

static int
makeGRP(charset, to, n, plane)
     register int	charset; /* -1 to initialize. */
     char		to[];
     int		n; /* Size of to[]. */
     int		plane;
{
	char		seq[5];/* Mini buffer to store the Esc sequnce.*/
	register char	*pc = seq;
	static int lastplane = -1;

	if (charset == RESET){
		curGRCharset = ISO8859_1R;
		return 0;
	}
	if (charset == curGRCharset && plane == lastplane) {
	    return 0;/* Already designated. */
	}
	else {/* Need to designated. */
		int	i;
		
		*pc++ = Esc;
		if (charset & _94S) {/* 94 char set */
			*pc++ = 0x29;
		} else if (charset & _96S) {/* 96 char set */
			*pc++=0x2d;
		} else if(charset&_94M) {/* multibyte 94 char set */
			*pc++ = 0x24; /* $ */

			if (!strcmp("tchinese",setlocale(LC_CTYPE,NULL)))
			  if (plane == 2)
			    *pc++ = ')';
			  else
			    *pc++ = ')';
			else
			  *pc++ = 0x29;

		} else {/* Unknown. */
			return -1;
		}
		if (!strcmp("tchinese",setlocale(LC_CTYPE,NULL)))
		  *pc++ = plane + 0x2f; /* Put the "Final" char. */
		else
		  *pc++ = (charset & 0xff); /* Put the "Final" char. */
		
		lastplane = plane;

		i = pc - seq;
		if (i >= n) {/* Not enough space. */
			return -1;
		}
		*pc = 0;
		strncpy(to, seq, n);
		curGRCharset = charset;
		return i;
	}
}/*makeGRP()*/


