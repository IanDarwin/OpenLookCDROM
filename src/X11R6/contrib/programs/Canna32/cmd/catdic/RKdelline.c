/* Copyright 1991 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of NEC Corporation
 * not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.  NEC 
 * Corporation makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

static	char	rcs_id[] = "@(#) 112.1 $Id: RKdelline.c,v 2.3 1994/02/02 10:21:01 hamada Exp $";

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#include <canna/RK.h>

#if __STDC__
#include <stdlib.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

RkDeleteLine(cx_num, name, line)
int cx_num;
char *name;
char *line;
{
  int linelen = strlen(line);
  int yomilen, yomihinshilen = 0;
  char *buf = (char *)malloc(linelen + 1), *sp, *dp;
  int res = 0;

  
  if ( !buf ) {
    return -1;
  }
  sp = line;
  while (*sp == ' ' || *sp == '\t')
    sp++; /* 空白の読み飛ばし */

  if (!*sp || *sp == '#') /* コメント行 */
    goto endDeleteLine;

  dp = buf;
  while (*sp && (*sp != ' ' && *sp != '\t')) { /* 読みのとりだし */
      if (*sp == '\\' && *(sp+1) ) { /* エスエープされた文字 */
	  *dp++ = *sp++ ; 
      }
      *dp++ = *sp++;
  }
  *dp++ = ' ';
  yomilen = dp - buf;

  while (*sp) {
    while (*sp == ' ' || *sp == '\t')
      sp++; /* 空白の読み飛ばし */

    if (*sp) {
      if (*sp == '#') {
	dp = buf + yomilen;
	while (*sp && (*sp != ' ' && *sp != '\t')) { /* 品詞と頻度のコピー */
	  *dp++ = *sp++;
	}
	*dp++ = ' ';
	yomihinshilen = dp - buf;

	while (*sp == ' ' || *sp == '\t')
	  sp++; /* 空白の読み飛ばし */
      }
      if (yomihinshilen == 0) { /* ここまで品詞情報が出てこなかった。 */
	goto endDeleteLine;
      }
      else if (*sp) { /* 候補があるのなら */
	dp = buf + yomihinshilen;
	while (*sp && (*sp != ' ' && *sp != '\t')) { /* 候補のコピー */
	    if (*sp == '\\' && *(sp+1)) { /* エスエープされた文字 */
		*dp++ = *sp++ ; 
	    }
	    *dp++ = *sp++;
	}
	*dp++ = '\0'; /* ヌル文字を最後に入れる */
	res = RkDeleteDic(cx_num, name, buf);
	if (res < 0) {
	  goto endDeleteLine;
	}
      }
    }
  }
 endDeleteLine:
  free(buf);
  return res;
}

#ifdef TEST_DELETEDIC
RkDeleteDic(cx_num, name, word)
int cx_num;
char *name;
char *word;
{
  printf("☆単語の定義(辞書:%s) \"%s\"\n", name, word);
  return 0;
}

main()
{
  char buf[2048], *p;
  int c;

  p = buf;
  c = getchar();
  while (c >= 0) {
    if (c == '\n') {
      *p++ = '\0';
      RkDeleteLine(0, "tempdic", buf);
      p = buf;
    }
    else {
      *p++ = c;
    }
    c = getchar();
  }
}
#endif /* TEST_DELETEDIC */
