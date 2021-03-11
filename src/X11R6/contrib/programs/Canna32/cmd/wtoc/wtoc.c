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
static char rcsid[]="@(#) 112.1 $Id: wtoc.c,v 1.18 1993/05/24 12:09:58 kon Exp $";
#endif
/* wtoi.c  テキスト形式の辞書をＷｎｎから「いろは」のものに変換する。
 *	wtoi [-f hinshifile] [wnndic] [irohadic]
 */
#include	<stdio.h>
#include        <ctype.h>

#if  __STDC__ || defined(SVR4)
#include <locale.h>
#endif

#ifdef SVR4
extern char *gettxt();
#else
#define	gettxt(x,y)  (y)
#endif

#define		MAXTANGO	256
#define         MAXHINSHI       100

#ifndef AIXV3
typedef	unsigned char	uchar;
#endif


struct hin{
  char *wnn;
  char *iroha;
};

static struct hin table[] = {
  {"名詞","#T35"},{"人名","#JN"},{"地名","#CN"},{"人名&地名","#JN"},
  {"固有名詞","#KK"},{"数詞","#NN"},
  {"一動幹","#KSr"},
  {"カ行五段","#K5r"},{"ガ行五段","#G5r"},{"サ行五段","#S5r"},
  {"タ行五段","#T5r"},{"ナ行五段","#N5"},{"バ行五段","#B5r"},
  {"マ行五段","#M5r"},{"ラ行五段","#R5r"},{"ワ行五段","#W5r"},
  {"一段","#KS"},{"一段&名詞","#KSr"},{"カ行(行く)","#C5r"},
  {"サ行(する)","#SX"},{"ザ行(ずる)","#ZX"},{"サ行(する)&名詞","#T30"},
  {"来(こ)","#kxo"},{"来(き)","#kxi"},{"来(く)","#kxuru"},
  {"為(し)","#sxi"},{"為(す)","#sxuru"},{"為(せ)","#sxe"},
  {"ラ行(下さい)","#L5"},
  {"形容詞","#KY"},
  {"形容動詞","#T05"},{"形容動詞&名詞","#T05"},{"形容動詞(たる)","#F00"},
  {"副詞","#F14"},{"連体詞","#RT"},{"接続詞,感動詞","#CJ"},
  {"単漢字","#KJ"},
  {"接頭語","#PRE"},{"接尾語","#SUC"},
  {"接頭数詞","#NNPRE"},{"助数詞","#JS"},{"接頭助数詞","#JS"},
  {"接尾助数詞","#JSSUC"},
  {"接尾人名","#JNSUC"},{"接頭地名","#CNPRE"},{"接尾地名","#CNSUC1"},
  {"形容動詞化接尾語","#N2T17"},{"サ行(する)&名詞化接尾語","#N2T30"},
  {"接尾動詞","#D2T35"},{"形容詞化接尾動詞","#D2KY"},
  {"接頭語(お)","#PRE"},{"接頭語(各)","#PRE"},{"記号","#T35"}
};

char *salloc(s)
     char *s;
{
  char *new,*malloc();
  
  if (new = (char *)malloc(strlen( s ) + 1))
    strcpy(new, s);
  else{
    fprintf(stderr, gettxt("cannacmd:48", "No more memory\n"));
    exit(1);
  }
  return(new);
}

/* 品詞対応 */
char *chghinshi( hinshi, taiou, fshurui )
     char   *hinshi;
     struct hin   *taiou;
     int    fshurui;  
{
  int   shurui;
  int   i;
  
  shurui = sizeof(table)/sizeof(struct hin);
  for( i = 0 ; i < shurui ; i++) {
    if( !strcmp( hinshi , table[i].wnn ) )
      return( table[i].iroha );
  }
  for( i = 0 ; i <  fshurui ; i++) {
    if( !strcmp( hinshi , taiou[i].wnn ) ) {
      return( taiou[i].iroha );
    }
  }
  return( "#??" );
}

/* 品詞ファイル読み込み */
int read_hinshi( fp, taiou )
     FILE    *fp;
     struct  hin  *taiou;
{
  int     shurui;
  char    H[MAXTANGO];
  char    wnn[MAXTANGO],iroha[MAXTANGO];
  
  shurui = 0;
  while( fgets( H, MAXTANGO, fp ) ){
    if( 2 != sscanf( H, "%s %s", wnn, iroha ) )
      continue;
    else {
      taiou[shurui].wnn = salloc(wnn);
      taiou[shurui].iroha = salloc(iroha);
      shurui++;
    }
  }
  return ( shurui );
} 

/* 出力 */
wtoi_write( fp, yomi, hinshi, kouho, hindo )
     FILE	*fp;
     uchar   *yomi, *hinshi, *kouho;
     int	hindo;
{
  if( !strcmp(hinshi,"#kxuru") || !strcmp(hinshi,"#sxuru")){
    strcat(yomi,"る");
    strcat(kouho,"る");
  }
  if( hindo == 0 )
    fprintf( fp, "%s %s %s \n", yomi, hinshi, kouho );
  else
    fprintf( fp, "%s %s*%d %s \n", yomi, hinshi, hindo, kouho );
}

int suuji(kazu)
     char *kazu;
{
  int i;

  for(i = 0; i < strlen(kazu); i++ ){
    if(!isdigit(kazu[i]))
      return(0);
  }
  return(atoi(kazu));
}

main(argc,argv)
     int  argc;
     char *argv[]; 
{
  struct hin taiou[MAXHINSHI];
  uchar	S[MAXTANGO],y[MAXTANGO], h[MAXTANGO], k[MAXTANGO],nd[10];
  int	d,option,fshurui;
  FILE	*fph,*fpi,*fpo;
  
#if  __STDC__ || defined(SVR4)
  (void)setlocale(LC_ALL,""); 
#endif 
  option = 0;
  if( argc <= 5 ) { /* 引数は正当か？ */ 
    if( argc > 2 && !strcmp(argv[1],"-f") ) { /* 品詞ファイルを読み込むか？ */
      if( (fph = fopen( argv[2], "r" ) ) == NULL) { /* 品詞ファイルをｏｐｅｎ */
	fprintf(stderr,gettxt("cannacmd:49", "%s: cannot open %s\n"), argv[0], argv[2] );
	exit(2);
      }
      fshurui = read_hinshi( fph, taiou );
      close( fph );
      option = 1;
    }
    fpi = stdin;
    fpo = stdout;
  }
  else { /* 引数が不正 */
    fprintf(stderr,gettxt("cannacmd:50", "Usage: wtoc [-f part-of-speach table] [wnndic] [cannadic]\n"),argv[0]);
    exit(2);
  }
  if( argc >= (2 + option*2) ) { /* Ｗｎｎ辞書をオープン */
    if( (fpi = fopen( argv[1 + option*2], "r" ) ) == NULL) {
      fprintf(stderr,gettxt("cannacmd:51", "%s: cannot open %s\n"), argv[0], argv[1 + option*2] );
      exit(2);
    }
    if( argc == (3 + option*2) ) { /* いろは辞書をオープン */
      if( (fpo = fopen( argv[2 + option*2], "w" ) ) == NULL) {
	fprintf(stderr,gettxt("cannacmd:52", "%s: cannot create %s\n"), argv[0], argv[2 + option*2] );
	exit(2);
      }
    }
  }
  
  /* 主処理 */
  while( fgets( (char *)S, sizeof(S), fpi ) ) {
    if( 4 >  sscanf( (char *)S, "%s %s %s %s", y, k, h, nd ) )
      continue;
    else      
      d = suuji(nd);
      wtoi_write( fpo, y, chghinshi( h, taiou, fshurui ), k, d );
  } 
  
  fclose( fpi );
  fclose( fpo );
  exit( 0 );
}
