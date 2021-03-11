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
static char rcsid[]="@(#) 112.1 $Id: dpromdic.c,v 1.20 1993/04/15 08:42:59 kon Exp $";
#endif

# include	<stdio.h>
#if  __STDC__ || defined(SVR4)
# include       <locale.h>
#endif

#ifdef SVR4
extern char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif
# include	<canna/RK.h>
/* # include       "RKintern.h"       */

extern struct RkRxDic  *RkOpenRoma() ;
void printch();

main(argc, argv)
int	argc;
char	**argv;
{
  char *filename;
  struct RkRxDic *rdic;
  unsigned char *s;
  int i, mode;
  static char  zero[] = "\\0" ;

#if  __STDC__ || defined(SVR4)
  (void)setlocale(LC_ALL,"");
#endif
  mode = 0x0004; /* 読み取り可能か？ */

  if(argc != 2){
    fprintf(stderr, gettxt("cannacmd:15", "Usage: dpromdic <file name>\n"));
    exit(1);
    /* NOTREACHED */
  }

  ++argv;
  filename = *argv;
  
  if(access(filename, mode) == -1) {
    fprintf(stderr, gettxt("cannacmd:16", "cannot open %s\n"), filename);
    exit(1);
    /* NOTREACHED */
  }

  if(rdic = RkOpenRoma(filename)) {

    if( rdic -> nr_bchars != NULL && rdic -> nr_bchars[0] ) {
      printf("!%s\n",rdic -> nr_bchars) ;
    }

    for(i=0; i<rdic->nr_nkey; i++) {
      s = rdic->nr_keyaddr[i];
      /* ローマ字 */
      for(s = rdic->nr_keyaddr[i]; *s; s++) {
	  printch(*s) ;
      }
      printf("\t");
      s++;
      /* かな */
      if ( *s ==   0  ) printf("%s",zero) ;
      else {
	for(; *s; s++) {
	  printch(*s) ;
	}
      }

      /*  intr  */
      if ( rdic -> dic == 0 ) {
	printf("\t");
	s++ ;
	for(; *s ; s++) {
	  printch(*s) ;
	}
      }
      if ( rdic->nr_brules && rdic -> nr_brules[i] == 1 ) {
	printf(" !") ;
      }
      printf("\n");
    }
    RkCloseRoma(rdic);
    exit(0);
  } else {
    fprintf(stderr, gettxt("cannacmd:17", "<%s> isn't rom-kana-table.\n"), filename);
    exit(1);
    /* NOTREACHED */
  }
}

void
printch(s)
      char s ;
{
	switch(s) {
	  case ' '    :
	  case '#'    :
	  case '\\'   : printf("\\");
	  default     : printf("%c", s);
                        break;
	}
}
