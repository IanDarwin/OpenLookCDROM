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
static char rcs[]="@(#) 112.1 $Id: ctow.c,v 1.18 1993/04/15 08:41:13 kon Exp $";
#endif
/* itow.c  テキスト形式の辞書を「いろは」からＷｎｎのものに変換する。
 *	itow [-f hinshifile] [irohadic] [wnndic]
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

#if __STDC__
#include        <stdlib.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

#define		MAXTANGO	256
#define         MAXHINSHI       100


static char *default_hinshi[] = {
  "名詞","副詞","#T35", "人名","#JN", "地名","#CN",
  "固有名詞","#KK", "数詞","#NN",
  "カ行五段","#K5", "ガ行五段","#G5", "サ行五段","#S5",
  "タ行五段","#T5", "ナ行五段","#N5", "バ行五段","#B5",
  "マ行五段","#M5", "ラ行五段","#R5", "ワ行五段","#W5",
  "一動幹","一段","#KS", "一段&名詞","#KSr", "カ行(行く)","#C5r",
  "サ行(する)","#SX", "ザ行(ずる)","#ZX","サ行(する)&名詞","#T30",
  "来(こ)","#kxo","来(き)","#kxi","来(く)","#kxuru",
  "為(せ)","#sxe","為(し)","#sxi","為(す)","#sxuru",
  "ラ行(下さい)","#L5",
  "カ行五段","#K5r", "ガ行五段","#G5r", "サ行五段","#S5r",
  "タ行五段","#T5r", "バ行五段","#B5r",
  "マ行五段","#M5r", "ラ行五段","#R5r", "ワ行五段","#W5r",
  "形容詞","#KY", "形容詞","#KYT", "形容詞","#KYna",
  "形容詞","#KYmi",
  "サ行(する)&名詞","#T00", "形容動詞","#T04", "形容動詞","#T06",
  "副詞","#T07", "形容動詞","#T08", "形容動詞","#T09",
  "形容動詞","#T10", "形容動詞","副詞","#T15", "副詞","#T16",
  "副詞","#T31", "副詞","#T36","副詞","#T37",
  "形容動詞","形容動詞&名詞","#T05", "副詞","形容動詞(たる)","#F00",
  "サ行(する)","形容動詞&名詞","#F01", "副詞","形容動詞&名詞","#F02",
  "形容動詞(たる)","#F03", "副詞","#F04", "副詞","#F06",
  "副詞","#F12", "副詞","#F14", "連体詞","#RT", "接続詞,感動詞","#CJ",
  "単漢字","#KJ",
  "接頭語","#PRE", "接尾語","#SUC",
  "接頭数詞","#NNPRE", "助数詞","接頭助数詞","#JS", "接尾助数詞","#JSSUC",
  "接尾人名","#JNSUC", "接頭地名","#CNPRE", "接尾地名","#CNSUC1",
  "形容動詞化接尾語","#N2T17", "サ行(する)&名詞化接尾語","#N2T30",
  "接尾動詞","#D2T35", "形容詞化接尾動詞","#D2KY",
  "接頭語","#SNPRE", "接尾地名","#CNSUC2", "接尾語","#N2T35",
  "接尾語","#K2T15", "接尾語","#K2T35", "形容化接尾語","#ND2KY",
  "形容化接尾語","#N2KYT", "形容動詞化接尾語","#N2T10",
  "形容動詞化接尾語","#N2T15","形容動詞化接尾語","#N2T18",
  "接尾語","#N2R5","接尾語","#N2K5",
  ""
};

ask_default_hinshi_size()
{
  int i;
  
  for (i = 0; strcmp(default_hinshi[i],""); i++);
  return i;
}

char *salloc(s)
char *s;
{
  char *new;
  
  if (new = (char *)malloc(strlen(s) +1))
    strcpy(new, s);
  else{
    fprintf(stderr, gettxt("cannacmd:8", "No more memory\n"));
    exit(1);
  }
  return(new);
}

/* 品詞対応 */
char *
chghinshi(hinshi, size, taiou, fsize)
char   *hinshi, **taiou;
int    size, fsize;
{
  int   i;
  char wnn_hinshi[MAXTANGO];

  wnn_hinshi[0] = '\0';
  for( i = (fsize -1); i >= 0; i -= 2) {
    if(!strcmp(hinshi ,taiou[i])) {
      strcat(wnn_hinshi, taiou[i-1]);
      strcat(wnn_hinshi," ");
    }
  }
  if (wnn_hinshi[0] != '\0') {
    wnn_hinshi[strlen(wnn_hinshi)-1] = '\0';
    return salloc(wnn_hinshi);
  }
  for (i = (size-1); i >= 0; i--) {
    if(*default_hinshi[i] == '#') {
      if(wnn_hinshi[0] != '\0')
	return salloc(wnn_hinshi);
      if(!strcmp(hinshi, default_hinshi[i])) {
	strcpy(wnn_hinshi, default_hinshi[--i]);
}
    }
    else {
      if(wnn_hinshi[0] != '\0') {
	strcat(wnn_hinshi," ");
	strcat(wnn_hinshi, default_hinshi[i]);
      }
    }
  }
  if (wnn_hinshi[0] != '\0')
    return salloc(wnn_hinshi);
  return salloc("");
}

/* 品詞ファイル読み込み */
read_hinshi(fp, taiou)
FILE  *fp;
char *taiou[MAXHINSHI];
{
  int  size;
  char H[MAXTANGO], wnn[MAXTANGO], iroha[MAXTANGO];

  size = 0;
  while(fgets(H, MAXTANGO, fp)){
    if(2 != sscanf(H, "%s %s", wnn, iroha))
      continue;
    else {
      taiou[size++] = salloc(wnn);
      taiou[size++] = salloc(iroha);
    }
  }
  taiou[size] = '\0';
  return size;
}
 
char *
get_hindo(iroha_hinshi)
char *iroha_hinshi;
{
  char *p;
  char *hindo;

  for(p = iroha_hinshi; *p; p++)
    if (*p == '*') {
      *p = '\0';
      hindo = ++p;
      return hindo;
    }
  return "0";
}

/* 出力 */
itow_write(fp, yomi, hinshi, kouho, hindo)
FILE  *fp;
char *yomi, *hinshi, *kouho, *hindo;
{
  fprintf( fp, "%s %s %s %s \n", yomi, kouho, hinshi, hindo);
}

main(argc, argv)
int  argc;
char *argv[]; 
{
  char *taiou[MAXHINSHI];
  char *nd, *hinshis, hinshi[MAXTANGO], *p;
  char S[MAXTANGO], y[MAXTANGO], h[MAXTANGO], k[MAXTANGO];
  int  hinshiSize,option,fsize = 0;
  FILE *fph,*fpi,*fpo;

#if  __STDC__ || defined(SVR4)
  (void)setlocale(LC_ALL,"");
#endif
  hinshiSize = ask_default_hinshi_size();

  option = 0;
  if( argc <= 5 ) { /* 引数は正当か？ */ 
    if( argc > 2 && !strcmp(argv[1],"-f") ) { /* 品詞ファイルを読み込むか？ */
      if( (fph = fopen( argv[2], "r" ) ) == NULL) { /* 品詞ファイルをｏｐｅｎ */
	fprintf(stderr,gettxt("cannacmd:9", "%s: cannot open %s\n"), 
		argv[0], argv[2] );
	exit(2);
      }
      fsize = read_hinshi(fph, taiou);
      close( fph );
      option = 1;
    }
    fpi = stdin;
    fpo = stdout;
  }
  else { /* 引数が不正 */
    fprintf(stderr,gettxt("cannacmd:10", 
	  "Usage: ctow [-f parts-of-speach table] [cannadic] [wnndic]\n"),
	    argv[0]);
    exit(2);
  }
  if( argc >= (2 + option*2) ) { /* いろは辞書をオープン */
    if( (fpi = fopen( argv[1 + option*2], "r" ) ) == NULL) {
      fprintf(stderr,gettxt("cannacmd:11", "%s: cannot open %s\n"),
	      argv[0], argv[1 + option*2] );
      exit(2);
    }
    if( argc == (3 + option*2) ) { /* Ｗｎｎ辞書をオープン */
      if( (fpo = fopen( argv[2 + option*2], "w" ) ) == NULL) {
	fprintf(stderr,gettxt("cannacmd:12", "%s: cannot create %s\n"),
		argv[0], argv[2 + option*2] );
	exit(2);
      }
    }
  }
  
  /* 主処理 */
  while(fgets(S, sizeof(S), fpi)) {
    if( 3 !=  sscanf(S, "%s %s %s ", y, h, k))
      continue;
    else {
      nd = get_hindo(h);
      if (k[0] == '@') /* 省略記号@を元に戻す */
	strcpy(k,y);
      if (!strcmp(h,"#kxuru") || !strcmp(h,"#sxuru")) {
	y[strlen(y)-2] = '\0'; /* 『くる』『する』を 『く』『す』にする */
	k[strlen(k)-2] = '\0';
      }
      p = hinshi;
      hinshis = chghinshi(h, hinshiSize, taiou, fsize);
      if (!strcmp(hinshis,"")) {
	fprintf(stderr,gettxt("cannacmd:13", 
	      "reading:%s nomination:%s a part of speach:%s\n"),y,k,h);
	fprintf(stderr,gettxt("cannacmd:14", 
	      "This part of speach is undefined. Cannot convert.\n"));
      }
      else {
	for ( ; *hinshis; hinshis++, p++) {
	  *p = *hinshis;
	  if (*p == ' ') {
	    *p = '\0';
	    itow_write( fpo, y, hinshi, k, nd );
	    p = hinshi;
	    p--;
	  }
	}
	*p = '\0';
	itow_write(fpo, y, hinshi, k, nd);
      }
    }
  }
  fclose( fpi );
  fclose( fpo );
  exit( 0 );
}
