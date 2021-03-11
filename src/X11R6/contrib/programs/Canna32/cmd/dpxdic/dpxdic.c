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
static char rcsid[]="@(#) 102.1 $Id: dpxdic.c,v 3.2 1994/03/09 13:18:06 kon Exp $";
#endif

#include "RKintern.h"
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <time.h>
#ifdef SVR4
#include	<unistd.h>
#endif


#if	! defined( HYOUJUN_GRAM )
#define HYOUJUN_GRAM "/usr/lib/canna/dic/canna/fuzokugo.d"
#endif

int	inv = 0;
static	char	*program;
static	unsigned char ebuf[8048];

char *
basename(name)
char	*name;
{
    char	*s = name + strlen(name);
    
    if (*s == '/')
	*s = (char)0;
    while (s-- >= name)
	if (*s == '/')
	    return ++s;
    return name;
}

unsigned char *
show_a_cand(gram, wrec, or)
     struct RkKxGram	*gram;
     unsigned char	*wrec;
     unsigned		*or;
{
  unsigned	j, clen, row;
  char		*ptr, rowname[128];
  Wchar		*dst, wbuf[1024];
    
  clen = (*wrec >> 1) & 0x7f;
  row = _RkRowNumber(wrec);
  wrec += NW_PREFIX;
  if (row == 0) {
    fprintf(stderr, "bad hinshi\n");
    return(wrec);
  }
  for (j = 0, dst = wbuf; j < clen; j++) {
    Wchar wch;

    wch = bst2_to_s(wrec);
    if (wch == (Wchar)'\\' || wch == (Wchar)' ' || wch == (Wchar)'\t') {
      *dst++ = (Wchar)'\\';
    }
    *dst++ = wch;
    wrec += sizeof(Wchar);
  }
  *dst = (Wchar)0;
  if (gram) {
    if (ptr = (char *)RkGetGramName(gram, row)) {
      (void)strcpy(rowname, ptr);
      if (*or != row) {
	printf(" #%s", rowname);
	*or = row;
      }
    } else {
      fprintf(stderr, "bad hinshi\n");
      return(wrec);
    }
  } else {
    if (*or != row) {
      printf(" #%d", row);
      *or = row;
    }
  }
  ustoeuc(wbuf, dst - wbuf, ebuf, RkNumber(ebuf));
  printf(" %s", ebuf);
  return wrec;
}

unsigned char *
show_a_icand(gram, wrec)
     struct RkKxGram	*gram;
     unsigned char	*wrec;
{
  unsigned	j, clen, row;
  char		*ptr, rowname[128];
  Wchar		*dst, wbuf[1024];
    
  clen = (*wrec >> 1) & 0x7f;
  row = _RkRowNumber(wrec);
  wrec += NW_PREFIX;
  if (row == 0) {
    fprintf(stderr, "bad hinshi\n");
    exit(1);
  }
  for (j = 0, dst = wbuf; j < clen; j++) {
    *dst++ = bst2_to_s(wrec);
    wrec += sizeof(Wchar);
  }
  *dst = (Wchar)0;
  ustoeuc(wbuf, dst - wbuf, ebuf, RkNumber(ebuf));
  printf("%s", ebuf);
  if (gram && (ptr = (char *)RkGetGramName(gram, row))) {
    (void)strcpy(rowname, ptr);
    printf(" #%s", rowname);
  } else {
    printf(" #%d", row);
  }
  return wrec;
}

void
show_a_wrec(gram, wrec, yomi, n)
     struct RkKxGram	*gram;
     unsigned char	*wrec;
     Wchar		*yomi;
     unsigned		n;
{
  unsigned	i, left, nc, or;
  Wchar		*src, *dst, syomi[1024], wch;
  
  left = (*wrec >> 1) & 0x3f;
  nc = _RkCandNumber(wrec);
  if (*wrec & 0x80)
    wrec += 2;
  wrec += 2;
  for (i = 0, src = yomi, dst = syomi ; i < n ; i++) {
    wch = *src++;
    if (wch == (Wchar)'\\' || wch == (Wchar)' ' || wch == (Wchar)'\t') {
      *dst++ = (Wchar)'\\';
    }
    *dst++ = wch;
  }
  for (i = 0 ; i < left ; i++) {
    wch = bst2_to_s(wrec);
    if (wch == (Wchar)'\\' || wch == (Wchar)' ' || wch == (Wchar)'\t') {
      *dst++ = (Wchar)'\\';
    }
    *dst++ = wch;
    wrec += sizeof(Wchar);
  }
  *dst = 0;
  if (inv) {
    for (i = 0; i < nc; i++) {
      wrec = show_a_icand(gram, wrec);
      ustoeuc(syomi, dst - syomi, ebuf, RkNumber(ebuf));
      printf(" %s", ebuf);
      printf("\n");
    }
  } else {
    ustoeuc(syomi, dst - syomi, ebuf, RkNumber(ebuf));
    printf("%s", ebuf);
    or = 0;
    for (i = 0; i < nc; i++) {
      wrec = show_a_cand(gram, wrec, &or);
    }
    printf("\n");
  }
}

static int
loadDic(dic)
     struct ND	*dic;
{
  unsigned char	*buf;
  unsigned	off = dic->doff;
  unsigned	size = dic->drsz;
  int		fd = dic->fd;
  int		i;
  
  if (!dic->pgs) {
    unsigned   tblsz = dic->ttlpg * sizeof(struct NP);
    
    if (!(dic->pgs = (struct NP *)malloc(tblsz)))
      return(-1);
    for (i = 0; i < dic->ttlpg; i++) {
      dic->pgs[i].lnksz = (unsigned) 0;
      dic->pgs[i].ndsz = (unsigned) 0;
      dic->pgs[i].lvo = (unsigned) 0;
      dic->pgs[i].csn = (unsigned) 0;
      dic->pgs[i].flags = (unsigned) 0;
      dic->pgs[i].buf = (unsigned char *) 0;
    }
  }
  if (!(buf = (unsigned char *)malloc(size)))
    return(-1);
  (void)lseek(fd, (long)off, 0);
  if (read(fd, (char *)buf, size) != size)
    return(-1);
  dic->buf = buf;

  return(0);
}

static int
loadPage(dic, id)
     struct ND	*dic;
     int	id;
{
  unsigned	off = dic->doff + dic->drsz + dic->pgsz * id;
  unsigned	size = dic->pgsz;
  unsigned char	*buf;
  int		fd = dic->fd;

  if (!dic->pgs) {
    fprintf(stderr, "no page table\n");
    return(-1);
  }

  if (id >= dic->ttlpg) {
    fprintf(stderr, "ERROR: %dth page is greater than max page %d\n",
	    id, dic->ttlpg);
    return(-1);
  }
  if (!isLoadedPage(dic->pgs + id)) {
    if (!(buf = (unsigned char *)malloc(size))) {
      fprintf(stderr, "malloc failed.\n");
      return(-1);
    }
    
    (void)lseek(fd, (long)off, 0);
    if (read(fd, (char *)buf, size) != size) {
      (void)fprintf(stderr, "cannot read page %d (%d)\n", id, size);
      return(-1);
    }
    dic->pgs[id].buf = buf;
    dic->pgs[id].count = 0;
    dic->pgs[id].flags = RK_PG_LOADED;
    dic->pgs[id].ndsz = bst2_to_s(buf + 2);
    dic->pgs[id].lnksz = bst2_to_s(buf + 4);
    dic->pgs[id].lvo = bst3_to_l(buf + 7);
    dic->pgs[id].csn = bst3_to_l(buf + 10);
  }
  return(0);
}

unsigned char *
offset2ptr(dic, off, which)
     struct ND	*dic;
     unsigned	off;
     int	*which;
{
  unsigned char *p;
  int		pg;

  if (off < dic->drsz) {
    p = dic->buf + off;
    pg = -1;
  } else {
    pg = (off - dic->drsz) / dic->pgsz;
    if (loadPage(dic, pg) < 0)
      return((unsigned char *)0);
    p = dic->pgs[pg].buf + off - dic->drsz - pg * dic->pgsz;
  }
  if (which)
    *which = pg;
  return(p);
}

unsigned char *
off2ptr(dic, off, id)
     struct ND	*dic;
     unsigned	off;
     int	id;
{
  unsigned char *p;

  p = dic->pgs[id].buf + off;
  return(p);
}

int
show_nip(gram, dic, yomi, n, p, pg)
     struct RkKxGram	*gram;
     struct ND		*dic;
     Wchar		*yomi;
     unsigned		n;
     unsigned char	*p;
     int		pg;
{
  Wchar		w;
  unsigned char	*pp;
  unsigned	nn;
  unsigned	val;
  int		iw;
  int		il = 0;
    
  while (!il) {
    w = bst2_to_s(p); p += 2;
    iw = *p & WORD_NODE;
    il = *p & LAST_NODE;
    val = ((p[0] & 0x3f) << BIT_UNIT) | p[1];
    p += 2;
    if (w != (Wchar) 0) {
      yomi[n] = w;
      nn = n + 1;
    } else {
      nn = n;
    }
    yomi[nn] = (Wchar)0;
    if (iw) {
      show_a_wrec(gram, off2ptr(dic, val, pg), yomi, nn);
    } else {
      if (!(pp = off2ptr(dic, val, pg))) {
	fprintf(stderr, "bad offset in nip\n");
	exit(1);
      }
      show_nip(gram, dic, yomi, nn, pp, pg);
    }
  }
  return(0);
}

int
compit(a, b)
     unsigned char *a;
     unsigned char *b;
{
  if (*a > *b || (*a == *b) && *(a+1) >= *(b+1)) {
    return(1);
  }
  return(-1);
}

static
show_nid(gram, dic, yomi, n, ptr)
     struct RkKxGram	*gram;
     struct ND		*dic;
     Wchar		*yomi;
     unsigned		n;
     unsigned char	*ptr;
{
  unsigned char	*p;
  Wchar		wc, i;
  unsigned	val;
  int		wn;
  unsigned	nn;
  int		which;

  p = ptr;
  wc = bst2_to_s(p); p += 5;
  qsort((char *)p, (unsigned)wc, 5, compit);
  for (i = 0; i < wc; i++) {
    Wchar	w;
    
    w = bst2_to_s(p); p += 2;
    val = bst3_to_l(p); p += 3;
    if (w == (Wchar) 0xffff)
      continue;
    wn = val & 0x800000;
    val &= 0x7fffff;
    if (w != (Wchar)0) {
      yomi[n] = w;
      nn = n + 1;
    } else {
      nn = n;
    }
    yomi[nn] = (Wchar)0;
    if (wn)
      show_a_wrec(gram, offset2ptr(dic, val, &which), yomi, nn);
    else {
      unsigned char	*pp;

      if (!(pp = offset2ptr(dic, val, &which))) {
	fprintf(stderr, "bad offset\n");
	exit(1);
      }
      if (which < 0) {
	show_nid(gram, dic, yomi, nn, pp);
      } else {
	show_nip(gram, dic, yomi, nn, pp, which);
      }
    }
  }
  return(0);
}

int
getdic(dic, filenm, dmnm)
     struct ND	*dic;
     char	*filenm;
     char	*dmnm;
{
  struct HD	hd;
  int		fd, lk;
  unsigned	off, doff, err;
  unsigned char	ll[4];	

  if (!filenm)
    return(-1);
  if ((fd = open(filenm, O_RDONLY)) < 0)
    return(-1);
  for (off = 0, lk = 1, doff = 0, err = 0;
       !err && lk && _RkReadHeader(fd, &hd, off) >= 0;
       lk = dmnm ? strcmp(dmnm, (char *)hd.data[HD_DMNM].ptr) : 1) {
    if (!dmnm) {
      time_t		tloc;
      unsigned char	date[26];

      tloc = hd.data[HD_TIME].var;
      strcpy(date, ctime(&tloc));
      date[24] = 0;
      (void)fprintf(stderr, "%s [ %s ] = %d + %d\n",
		    (char *)hd.data[HD_DMNM].ptr,
		    date,
		    hd.data[HD_CAN].var,
		    hd.data[HD_REC].var);
    }
    doff = off;
    off += hd.data[HD_SIZ].var;
    if (!strncmp(".swd", (char *)(hd.data[HD_DMNM].ptr + strlen((char *)hd.data[HD_DMNM].ptr) - 4), 4)) {
      if (lseek(fd, (long)off, 0) < 0 || read(fd, (char *)ll, 4) != 4)
	err = 1;
      off += bst4_to_l(ll) + 4;
    }
  }
  if (!dmnm)
    return(0);
  if (lk)
    return(-1);
  dic->doff = doff + hd.data[HD_HSZ].var;
  dic->sz = hd.data[HD_SIZ].var;
  dic->drsz = hd.data[HD_PGOF].var - hd.data[HD_DROF].var;
  dic->pgsz = _RkCalcUnlog2(hd.data[HD_L2P].var) + 1;
  dic->ttlpg = hd.data[HD_PAG].var;
  dic->fd = fd;
  dic->buf = (unsigned char *)0;
  dic->pgs = (struct NP *)0;
    
  return(loadDic(dic));
}

main (argc, argv)
  int argc;
  char *argv [];
{
  char			*dmnm = 0;
  int			i;
  struct RkKxGram	*gram;
  Wchar			yomi[1024];
  struct ND		Dic;
  struct ND		*dic = &Dic;
  int			which;
  unsigned char		*p;
  char			*cnj = (char *)0;
  char			bn[256];
  int			fd;

  program = basename(argv[0]);
  for (i = 1; i < argc && argv[i][0] == '-'; i++) {
    if (!strcmp(argv[i], "-i") ) {
      inv = 1;
      continue;
    } else if (!strcmp(argv[i], "-D")) {
      if ( ++i < argc && !cnj) {
	cnj = argv[i];
	continue;
      }
    } else {
      (void)fprintf(stderr,
		    "usage: %s [-i] [-D bunpou] <filename> [dictionary-name]\n",
		    program);
      exit(1);
    }
  }
  if (i > argc - 1) {
    (void) fprintf(stderr,
		   "usage: %s [-i] [-D bunpou] <filename> [dictionary-name]\n",
		   program);
    exit(1);
  }
  if (!cnj) {
    if(!(gram = RkOpenGram(HYOUJUN_GRAM))) {
      (void)fprintf(stderr, "Warning: cannot open grammar file %s.\n", HYOUJUN_GRAM);
    }
  } else {
    if ((fd = open(cnj, 0)) < 0) {
      (void)fprintf(stderr, "%s: cannot open grammar file %s.\n", program, cnj);
      exit(1);
    }
    gram = RkReadGram(fd);
    close(fd);
  }
  (void)strcpy(bn, argv[i]);
  if (!(dmnm = argv[i+1])) {
    if (getdic(dic, bn, dmnm) < 0) {
      (void)fprintf(stderr, "%s: cannot read file %s\n", program, bn);
    }
    exit(1);
  }

  if (getdic(dic, bn, dmnm) < 0) {
    (void)fprintf(stderr, "%s: cannot read file %s or dictionary %s\n", program, bn, dmnm);
    exit(1);
  }
  p = offset2ptr(dic, (unsigned)0, &which);
  if (which != -1) {
    (void)fprintf(stderr, "incollect dictionary\n");
    exit(1);
  }
  show_nid(gram, dic, yomi, (unsigned)0, p);
  (void)close(dic->fd);
  fflush(stdout);
  return 0;
}
