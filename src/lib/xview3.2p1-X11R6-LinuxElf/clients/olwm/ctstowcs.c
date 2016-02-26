#ident  "@(#)ctstowcs.c	1.4    93/06/28 SMI"
 
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


/* 
 *  ctstowcs() converts a Compound Text string into the corresponding wide 
 *	character string.  
 */

int
ctstowcs(wcs, cts, nwcs)
     wchar_t	wcs[]; /* Buffer to which the result would be written. */
     char	*cts; /* The source string in the Compound Text format.*/
     int	nwcs; /* Length of wcs[], the max # of wchar_t's. */
{
	int		gl_charset=ASCII;
	int		gr_charset=ISO8859_1R;
	int		this_charset;
	int		*p_gx;
	unsigned char	c, c1, c2, c3, c4;
	wchar_t		*pwc=wcs;
	wchar_t		wc;
	unsigned char	plane;

	while(c=((unsigned char)*cts++))
	switch(c){
	      case Esc:
		switch( c1=((unsigned char)*cts++)){
		      case 0:	
			return -1;
			break;
		      case 0x28: /* A 94-glyph set to GL. */
			p_gx=&gl_charset;
			goto desig94S;
		      case 0x29: /* A 94-glyph set to GR. */
			p_gx=&gr_charset;
			/* fall into... */
		      desig94S:
			c2=((unsigned char)*cts++);
			if(c2==0){/* Premature end of CT string */
				return -1;
			}
			*p_gx=_94S+c2;
			break;

		      /*
		       * FIX_ME: case 0x2c: has been added to support
		       * JOWN2.0.x based client, but this does not
		       * mean to be good things to do!
		       */
		      case 0x2c:
		      case 0x2d: /* A 96-glyph set to GR. */
			c2=((unsigned char)*cts++);
			if(c2==0){/* Premature end of CT string */
				return -1;
			}
			gr_charset=_96S+c2;
			break;
		      case 0x24: /* A multibyte set... */
			/* next byte==0x28 ---> GL
			   next byte==0x29 ---> GR 
			   */
			c2=((unsigned char)*cts++);
			switch(c2){
			      case 0:/* Premature end of CT string */
				return -1;
			      case 0x28:
				p_gx=&gl_charset;
				goto desig94M;
			      case 0x29:
				p_gx=&gr_charset;
				/* fall into... */
			      desig94M:
				c3=((unsigned char)*cts++);
				if(c3==0){/* Premature end of CT string */
					return -1;
				}
				if (!strcmp("tchinese",setlocale(LC_CTYPE,NULL))) {
				  if (c3 == 0x31) plane = 0x22;
				  if (c3 >= 0x32 && c3 <= 0x3f) { /* reset CNS3 */
				    plane = c3 - 0xf;
				    c3 = 0x32;
				  }
				}
				*p_gx=_94M+c3;
				break;
			      default:/* Unknown Escape */
				return -1;
				break;
			}
		}
		break;
	      default:
		this_charset=(c&0x80)?gr_charset:gl_charset;
		c&=0x7f;
		/* WE ARE ASSUMING LC_CTYPE==japanese here!!*/
		switch(this_charset){
		      case ASCII:
		      case JISX0201L:
			/* Two charsets are different but JLE treates
			   them as a same char set. */
			wc=WCHAR_CS0|c;
			break;
		      case JISX0208:
			if(c4=((unsigned char)*cts++)){
#ifdef	LONG_WCHAR_T
				wc=WCHAR_CS1|(c<<7)|(c4&0x7f);
#else
				wc=WCHAR_CS1|(c<<8)|(c4&0x7f);
#endif
			}else{
				return -1;
			}
			break;
		      case JISX0201R:
			wc=WCHAR_CS2|c;
			break;
                      case KSC5601:
                      case GB2312:
                        if(c4=((unsigned char)*cts++)){
#ifdef  LONG_WCHAR_T
                                wc=WCHAR_CS1|(c<<7)|(c4&0x7f);
#else
                                wc=WCHAR_CS1|(c<<8)|(c4&0x7f);
#endif   
                        }else{
                                return -1;
                        }  
                        break;
		      case CNS1:
                        if(c4=((unsigned char)*cts++)){
                                wc=WCHAR_CS1|(c<<7)|(c4&0x7f);
                        }else{
                                return -1;
                        }  
                        break;
		      case CNS2:
                        if(c4=((unsigned char)*cts++)){
                                wc=WCHAR_CS2|(plane<<14)|(c<<7)|(c4&0x7f);
                        }else{
                                return -1;
                        }  
                        break;
		      case CNS3:
                        if(c4=((unsigned char)*cts++)){
                                wc=WCHAR_CS3|(plane<<14)|(c<<7)|(c4&0x7f);
                        }else{
                                return -1;
                        }  
                        break;
		      default:
			wc=WCHAR_CS0|'?';
		}/*switch(this_charset)*/
		*pwc++=wc;
		if(pwc>=wcs+nwcs) return nwcs;
	}
	*pwc=0; /* Terminator */
	return (pwc-wcs); /* Return # of wide chararacters. */
}
