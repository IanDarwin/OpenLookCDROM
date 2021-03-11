/* Copyright 1993 NEC Corporation, Tokyo, Japan.
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
static char rcs_id[] = "$Id: cannaindex.c,v 1.4 1993/09/25 16:16:26 kon Exp $";
#endif

#include <stdio.h>

#if defined(SYSV) || defined(USG) || defined(SVR4) || __STDC__
#include <string.h>
#define index strchr
#define allocs strdup
#else
#include <strings.h>

extern char *malloc();

static char *
allocs(s)
char *s;
{
  char *res;

  res = malloc(strlen(s) + 1);
  if (res) {
    strcpy(res, s);
  }
  return res;
}
#endif

static char *program;

static void
usage()
{
  fprintf(stderr, "Usage: %s filename\n", program);
  exit(1);
}

struct ind {
  char *yomi, *midashi;
  int page;
} entries[1024]; /* 現状は 192 なので余裕のよっちゃんである */

static
compentry(a, b)
struct ind *a, *b;
{
  int res;

  res = strcmp(a->yomi, b->yomi);
  if (!res) {
    res = a->page - b->page;
  }
  return res;
}

static void
printentries(e, n)
struct ind *e;
int n;
{
  int i, j;
  unsigned prevchar = 0, curchar;

  for (i = 0 ; i < n ;) {
    if (e[i].yomi[0] & 0x80) { /* G3 が来たらアウト!? */
      curchar = ((e[i].yomi[0] & 0xff) << 8) | (e[i].yomi[1] & 0xff);
    }
    else {
      curchar = (unsigned)e[i].yomi[0];
    }
    if (curchar != prevchar) {
      prevchar = curchar;
      if (curchar & 0xff00) {
	printf("\\indexchar{%c%c}\n", (curchar >> 8), curchar & 0xff);
      }
      else {
	printf("\\indexchar{%c}\n", curchar);
      }
    }
    for (j = i + 1 ; j < n ; j++) {
      if (strcmp(e[i].yomi, e[j].yomi) ||
	  strcmp(e[i].midashi, e[j].midashi)) {
        break;
      }
    }
    printf("\\indexentry{%s}{%d", e[i].midashi, e[i].page);
    while (++i < j) { /* i is incremented also for the beginning `for' */
      printf(", %d", e[i].page);
    }
    printf("}\n");
  }
}

static void
freeentries(e, n)
struct ind *e;
int n;
{
  int i;

  for (i = 0 ; i < n ; i++) {
    if (e[i].yomi) {
      free(e[i].yomi);
      e[i].yomi = (char *)0;
    }
    if (e[i].midashi) {
      free(e[i].midashi);
      e[i].midashi = (char *)0;
    }
  }
}

#define CHARBUFSIZE 1024

static
readindex(f, e)
FILE *f;
struct ind *e;
{
  char buf[CHARBUFSIZE], *p, *q, *r, *ebuf;
  int n = 0, pagenum;

  while (fgets(buf, sizeof(buf), f)) {
    ebuf = buf + strlen(buf) - 1;
    *ebuf = '\0'; /* chop */

    p = index(buf, '{');
    if (p) {
      q = ++p;
      while (p = index(p, '$')) {
	if (p + 2 < ebuf && *(p + 1) == '$' && *(p + 2) == '$') {
	  *p = '\0';
          p += 3;
          break;
        }
        else {
	  p++;
	}
      }
      if (p) {
	r = p;
	while (p = index(p, '$')) {
	  if (p + 2 < ebuf && *(p + 1) == '$' && *(p + 2) == '$') {
	    *p = '\0';
            p += 3;
            break;
          }
          else {
	    p++;
	  }
        } 
        if (p) {
	  p = index(p, '{');
	  if (p) {
	    p++;
	    e[n].page = atoi(p);
	    e[n].yomi = allocs(q);
	    e[n].midashi = allocs(r);
	    n++;
	  }
	}
      }
    }
  }
  return n;
}

main(argc, argv)
int argc;
char *argv[];
{
  FILE *inf;
  int pages;

  program = argv[0] + strlen(argv[0]);
  while (argv[0] < program) {
    if (*(program - 1) == '/') {
      break;
    }
    program--;
  }
  if (argc < 2) {
    usage();
  }
  inf = fopen(argv[1], "r");
  if (!inf) {
    fprintf(stderr, "%s: Can not open file \"%s\".\n", argv[1]);
    exit(1);
  }
  pages = readindex(inf, entries);
  fclose(inf);
  qsort(entries, pages, sizeof(struct ind), compentry);
  printentries(entries, pages);
  freeentries(entries, pages);
  exit(0);
}
