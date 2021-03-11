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

static char rcs_id[] = "@(#) 102.1 $Id: itoc.c,v 1.8 1992/07/28 12:37:53 kon Exp $";

#include <stdio.h>
/*#include "canna.h"*/

#define KANJI

int IROHA_ParseError;
static int ptr, len;
extern yylineno;

extern yyparse();

static FILE *rcfile;
FILE *fopen();

#define BUF_LEN 1024
#define BACK_BUF 16

static char rbuff[BACK_BUF + BUF_LEN];
static char *buff = rbuff + BACK_BUF;
char IROHA_rcfilename[BUF_LEN] = "";

/* cfuncdef

   parse_string -- 文字列をパースする。

*/

parse_string(str)
char *str;
{
  rcfile = (FILE *)0;
  ptr = yylineno = 0;
  strcpy(buff, str);
  len = strlen(buff);
  IROHA_ParseError = 0;
  (void)yyparse();
}

/* cfuncdef

   YYparse -- カスタマイズファイルを読む。

   ファイルディスクリプタで指定されたファイルを読み込む。

*/

static
YYparse(f)
FILE *f;
{
  rcfile = f;
  ptr = len = yylineno = 0;
  IROHA_ParseError = 0;
  yyparse();
}

/* cfuncdef

  YYparse_by_rcfilename -- カスタマイズファイルを読み込む。

  IROHA_rcfilename にて指定されているカスタマイズファイルをオープンし
  読み込む。

  戻り値	１ 読み込めた、０ 読み込めなかった

*/

extern void print_defmode();

static 
YYparse_by_rcfilename()
{
  FILE *f;

  if (f = fopen(IROHA_rcfilename, "r")) {
    print_defmode("default.kp");
    YYparse(f);
    fclose(f);
    return 1;
  }
  return 0;
}

/* cfuncdef

  parse -- .iroha ファイルを探してきて読み込む。

  parse はカスタマイズファイルを探し、そのファイルをオープンしパースす
  る。

  パース中のファイルの名前を IROHA_rcfilename に入れておく。

  */

#define NAMEBUFSIZE 1024
#define RCFILENAME  ".iroha"
#define FILEENVNAME "IROHAFILE"

main(argc, argv)
int argc;
char *argv[];
{
  char *getenv();

  if (argc < 2) {
    fprintf(stderr, "usage: %s filename\n", argv[0]);
    exit(1);
  }
  strcpy(IROHA_rcfilename, argv[1]);
  if (YYparse_by_rcfilename()) {
    exit(0);
  }
  else
    exit(1);
}


addWarningMesg(buf)
char *buf;
{
  fprintf(stderr, "%s\n", buf);
}


int
  IROHA_input()
{
  while (ptr == len)
    {
      if (rcfile == (FILE *)NULL
	  || fgets(buff, BUF_LEN, rcfile) == (char *)NULL)
	return (int)NULL;
      
      yylineno++;
      
      ptr = 0;
      len = strlen(buff);
    }
#ifdef KANJI
  return ((buff[ptr] == (char)-1) ? 
	  (ptr++, -1) : (int)(unsigned char)buff[ptr++]);
#else /* not KANJI */
  return ((int)buff[ptr++]);
#endif /* not KANJI */
}

void
  IROHA_unput(c)
{
  buff[--ptr] = c;
}

void
  IROHA_output(c)
{
  putchar(c);
}
