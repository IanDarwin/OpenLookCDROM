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
static char rcsid[]="@(#) 102.1 $Id: crxdic.c,v 3.2 1994/03/09 13:17:25 kon Exp $";
#endif

#include "RKintern.h"
#include <stdio.h>
#include <sys/types.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <fcntl.h>
#if	!defined(nec_ews_svr2) && __STDC__
#include <stdlib.h>
#endif

#if	! defined( HYOUJUN_GRAM )
#define HYOUJUN_GRAM "/usr/lib/canna/dic/canna/fuzokugo.d"
#endif

#define PAGE_HDR_SIZ		14
#define MAX_PAGE_OFF		0x7fffff
#define MAXPAGE_NUM(pagesize)	(MAX_PAGE_OFF / pagesize)

#define DEF_WTYP	"W16 "

struct node {
    unsigned long	id;
    Wchar		key;
    unsigned		count;
    union {
	struct node	*n;
	unsigned char	*w;
    } ptr;
    int			page_num;
    long		page_on;
    long		page_off;
    int			wrec_bytes;
    unsigned 		size;
};

struct direc {
    unsigned char	*buf;
    unsigned		dirsiz, diroff;
    unsigned		nnode;
};

struct page {
    unsigned char	*buf;
    unsigned		dirsiz, diroff;
    unsigned		first_lvo;
    unsigned		first_csn;
    unsigned		lnksiz, lnkoff;
    unsigned		wrdsiz, wrdoff;
    unsigned		ndir, nlinks;
    unsigned		nwrecs, nnode;
    int			candnum;
};

struct wlist {
    struct node		*nd;
    struct wlist	*next;
    int			size;
};

struct dictionary {
    unsigned		MaxCand;
    unsigned		PageSize;
    unsigned		TotalRec;
    unsigned		TotalCand;
    unsigned		TotalPage;
    unsigned		Cwidth;
    unsigned		Lnd;
    unsigned		Snd;
    unsigned		PagNodeSize;
    unsigned		DirNodeSize;
    unsigned		LinkSize;
    struct node		*Node;
    struct direc	*Dir;
    struct page		*Page;
    int			pn;
    struct wlist	*Wlist;
    unsigned		rest;
    unsigned char	*hdr;
    unsigned		hdrsiz;
    unsigned		empty;
    int			type;
    char		*name;
};

struct TextDic {
  Wchar *line;
  Wchar *yomi;
};

#define is_in_dir(nd)		(((nd)->page_num == -1) ? 1 : 0)
#define is_word_node(nd)	(((nd)->count == 0) ? 1 : 0)

#define BIT_UNIT		8
#define WORD_NODE		(0x80)
#define LAST_NODE		(0x40)
#define PAG_NDVAL_LEN		2
#define DIR_NDVAL_LEN		3

#define JMWD	1
#define JSWD	2
#define JPRE	3
#define JSUC	4

#ifndef	HYOUJYUN_GRAM
#define HYOUJYUN_GRAM	"/usr/lib/canna/dic/canna/fuzokugo.d"
#endif

#define	DEFAULT_JAPANESE_LOCALE	"japan"

char	*program;
time_t	tloc;
char	outfile[1024];
char	textfile[1024];
char	*gfile = 0;
char	dicname[1024];
char	*localename = DEFAULT_JAPANESE_LOCALE;
int	search = 0;
int	type = JMWD;

extern	Wchar	*euctous();

#define MAXLINE		1024
#define MAXKOUHO       	64
#define MAXYOMI		64
#define MAXHINSHI	32

static char *
STrdup(s)
char *s;
{
  char *p = (char *)malloc(strlen(s) + 1);
  if (p) strcpy(p, s);
  return p;
}

static int
CopyLine(dst, src, len)
Wchar *dst, *src;
int len;
{
  register Wchar *p = dst;

  for (; len > 0 ; len--) {
    if (*src == (Wchar)'\\') {
      len--;
      src++;
      if (len == 0) {
	break;
      }
      /* 必要なバックスラッシュだったら付ける。
	 そうじゃなければ取り除く(正規化) */
      if (*src == (Wchar)' ' || *src == (Wchar)'\t' || *src == (Wchar)'\\') {
	*p++ = (Wchar)'\\';
      }
    }
    *p++ = *src++;
  }
  *p = (Wchar)0;
  return p - dst;
}

/*
  extractYomi -- RkwDefineDic の引数から読みを取り出す。そのとき、バック
                 スラッシュも取り除く。
 */

#define RkwIsGraphicChar(x) ((unsigned long)(x) > (unsigned long)' ')
#define RkwIsControlChar(x) ((unsigned long)(x) < (unsigned long)' ')

static Wchar *
extractYomi(wrec)
Wchar *wrec;
{
  int yomilen;
  Wchar *p, *q, *res;

  for (yomilen = 0, p = wrec ; RkwIsGraphicChar(*p) ; p++, yomilen++) {
    if (*p == (Wchar)'\\' && *(p + 1)) {
      p++;
    }
  }
  res = (Wchar *)malloc((yomilen + 1) * sizeof(Wchar));
  if (res) {
    int i;
    for (i = 0, p = wrec, q = res ; i < yomilen ; i++) {
      if (*p == (Wchar)'\\' && *(p + 1)) {
	p++;
      }
      *q++ = *p++;
    }
    *q = (Wchar)0;
  }
  return res;
}

/* open_wfile -- テキスト辞書を読み込み Wchar に直して返す。nel に行数が返る

    ついでに読みからバックスラッシュを取り除いたものを読み専用の配列に入れる。
 */
struct TextDic *
open_wfile(filename, nel)
     char	*filename;
     unsigned	*nel;
{
  FILE		*fp;
  Wchar		line[MAXLINE];
  struct TextDic *lines;
  int		i;
  unsigned char	aline[2*MAXLINE];
  unsigned	maxline;
    
  if (!(fp = fopen(filename, "r"))) {
    fprintf(stderr, "%s: cannot open %s\n", program, filename);
    exit(1);
  }
  maxline = 0;
  while (fgets((char *)aline, RkNumber(aline), fp)) {
    if (aline[0] != (unsigned char)'#') {
      maxline++;
    }
  }
  rewind(fp);
  lines = (struct TextDic *)
    calloc((unsigned int)(maxline + 1), sizeof(struct TextDic));
  if (!lines) {
    fprintf(stderr, "%s: no more space", program);
    exit(1);
  }
  i = 0;
  while (fgets((char *)aline, RkNumber(aline), fp)) {
    int	len = strlen((char *)aline);
    Wchar	*p;

    if (aline[0] == (unsigned char)'#') {
      continue;
    }
    
    while (aline[len - 1] != '\n') {
      fprintf(stderr, "%s: too long line:%s\n", program, aline);
      if (!fgets((char *)aline, RkNumber(aline), fp)) {
	lines[maxline].line = (Wchar *)0;
	*nel = maxline;
	return lines;
      }
      len = strlen((char *)aline);
    }
    aline[--len] = 0;
    if (i == maxline) {
      fprintf(stderr, "%s: too many lines\n", program);
      exit(1);
    }
    p = euctous(aline, len, line, RkNumber(line));
    len = p - line;
    if (!(p = (Wchar *)calloc((unsigned int)(len + 1), sizeof(Wchar)))) {
      fprintf(stderr, "%s: no more space\n", program);
      exit(1);
    }
    len = CopyLine(p, line, len);
    lines[i].line = p;
    lines[i].yomi = extractYomi(p);
    if (!lines[i].yomi) {
      fprintf(stderr, "%s: no more space\n", program);
      exit(1);
    }
    i++;
  }
  lines[maxline].line = (Wchar *)0;
  *nel = maxline;
  return lines;
}

unsigned char	*
nhash(buf, key, size, unit)
     unsigned char	*buf;
     Wchar		key;
     unsigned		size;
     unsigned		unit;
{
  unsigned char	*p;
  int		i, j;

  i = ((int)key) % size;
  p = buf + unit * i;
  for (j = 0; j < size && (*p != 0xff || *(p+1) != 0xff) ; j++) {
    i = (i + 1) % size;
    p = buf + unit * i;
  }
  if (j == size) {
    fprintf(stderr, "%s: hash table overflow\n", program);
    exit(1);
  }
  return(p);
}

void
fil_pnd(dst, c, nd, val, islast, size, unit)
     unsigned char	*dst;
     int		c;
     struct node	*nd;
     unsigned long	val;
     int		islast;
     unsigned		size;
     unsigned		unit;
{
    unsigned char	*ptr;

    dst +=  c * unit;
    s_to_bst2(nd->key, dst); dst += 2;
    ptr = dst;
    *dst++ = (unsigned char)(val >> BIT_UNIT) & 0x3f;
    *dst++ = (unsigned char)(val & 0xff);
    if (is_word_node(nd)) {
	*ptr |= WORD_NODE;
    } else {
	*ptr &= ~WORD_NODE;
    }
    if (islast) {
	*ptr |= LAST_NODE;
    } else {
	*ptr &= ~LAST_NODE;
    }
}

void
fil_dnd(dst, nd, val, size, unit)
     unsigned char	*dst;
     struct node	*nd;
     unsigned long	val;
     unsigned		size;
     unsigned		unit;
{
  dst = nhash(dst, nd->key, size, unit);
  s_to_bst2(nd->key, dst);
  dst += 2;
  l_to_bst3(val, dst);
  if (is_word_node(nd)) {
    *dst |= WORD_NODE;
  } else {
    *dst &= ~WORD_NODE;
  }
  dst += 3;
}

unsigned long
fil_dic(nd, dic, pg)
     struct node	*nd;
     struct dictionary	*dic;
     int		*pg;
{
  unsigned	page_num;
  struct page	*P;
  struct direc	*D;
  unsigned char	*dst, *tmp;
  unsigned	nid;
  unsigned long	val, cval;
  int		cpg;
  int		i, j;
    
  page_num = nd->page_num;
  P = &dic->Page[page_num];
  D = dic->Dir;
  if (is_word_node(nd)) {
    dst = P->buf + P->wrdoff;
    val = P->wrdoff;
    P->wrdoff += nd->wrec_bytes;
    memcpy((char *)dst, (char *)nd->ptr.w, (unsigned)nd->wrec_bytes);
    P->lnkoff += dic->LinkSize;
    *pg = nd->page_num;
    if (nd->page_num != -1)
      val += dic->PageSize * nd->page_num + D->dirsiz;
    return(val);
  } else {
    nid = getp(nd);
    if (nd->page_num == -1) {
      val = D->diroff;
      dst = D->buf + D->diroff;
      D->diroff += dic->DirNodeSize * (nid + 1);
      if (D->dirsiz < nd->page_off) {
	fprintf(stderr, "page offset overflow.\n");
	exit(1);
      }
      s_to_bst2(nid, dst); dst += 2;
      l_to_bst3(0, dst); dst += 3;
      tmp = dst;
      for (i = 0; i < nid; i++) {
	for (j = 0; j < dic->DirNodeSize; j++)
	  *dst++ = 0xff;
      }
      for (i = 0; i < nd->count; i++) {
	struct node	*child = &nd->ptr.n[i];
	
	cval = fil_dic(child, dic, &cpg);
	fil_dnd(tmp, child, cval, nid, dic->DirNodeSize);
      }
      *pg = -1;
      return val;
    } else {
      val = P->diroff;
      dst = P->buf + val;
      P->diroff += dic->PagNodeSize * nd->count;
      tmp = dst;
      for (i = 0; i < nd->count; i++) {
	struct node	*child = &nd->ptr.n[i];
	int		lflag = 0;
	
	cval = fil_dic(child, dic, &cpg);
	if (cpg != nd->page_num) {
	  fprintf(stderr, "illegal page number %d, %d\n", cpg, nd->page_num);
	  exit(1);
	}
	cval -= dic->PageSize * nd->page_num + D->dirsiz;
	if (i == nd->count - 1)
	  lflag = 1;
	fil_pnd(tmp, i, child, cval, lflag, nd->count, dic->PagNodeSize);
      }
      *pg = nd->page_num;
      val += dic->PageSize * nd->page_num + D->dirsiz;
      return(val);
    }
  }
}

struct page *
alloc_page(dic, pn)
     struct dictionary	*dic;
     unsigned		pn;
{
    struct page	*P;
    int		i;
    unsigned	psize;
    
    P = dic->Page;
    psize = dic->PageSize;
    for (i = 0; i < pn; i++) {
	unsigned char	*ptr;
	
	if (!(ptr = (unsigned char *)calloc(1, psize))) {
	    fprintf(stderr, "no space\n", i);
	    exit(1);
	}
	P[i].buf = ptr;
	P[i].diroff = PAGE_HDR_SIZ;
	P[i].lnkoff = P[i].dirsiz;
	P[i].wrdoff = P[i].dirsiz + P[i].lnksiz;
    }
    return P;
}

void
alloc_dir(dic)
  struct dictionary	*dic;
{
    struct direc	*D = dic->Dir;
    int			sz = D->dirsiz;
    unsigned char	*p;

    if (!(p = (unsigned char *)malloc((unsigned)sz))) {
	fprintf(stderr, "no space\n");
	exit(1);
    }
    memset((char *)p, ~0, (unsigned)sz);
    D->buf = p;
}

struct wlist *
append_wlist(dic, tail, nd)
  struct dictionary	*dic;
  struct wlist		*tail;
  struct node		*nd;
{
    struct wlist	*w;
    
    if (!tail)
	tail = dic->Wlist;
    while (tail->next)
	tail = tail->next;
    if (!(w = (struct wlist *)calloc(1, sizeof(struct wlist)))) {
	fprintf(stderr, "no space\n");
	exit(1);
    }
    tail->next = w;
    w->next = 0;
    w->nd = nd;
    w->size = nd->size;
    dic->rest++;
    return w;
}

static int
is_overflow_page(dic, pg, pn, size)
  struct dictionary	*dic;
  struct page		*pg;
  unsigned		pn, size;
{
    unsigned	total;
    
    if (pn != -1) {
      total = pg[pn].dirsiz + pg[pn].lnksiz + pg[pn].wrdsiz + size;
      if (dic->PageSize <= total)
	return 1;
    }
    return 0;
}

static int atop = 1;

int
assign_to_page(dic, nd, page_num, is_pn_indir)
     struct dictionary	*dic;
     struct node	*nd;
     unsigned		page_num;
     int		is_pn_indir;
{
    struct page		*P;
    struct direc	*D;
    int			i, nid;
    unsigned		pn;
    
    P = dic->Page;
    D = dic->Dir;

    if (is_word_node(nd)) {
	if (is_pn_indir) {
	    append_wlist(dic, (struct wlist *)0, nd);
	    for (pn = 0; pn < dic->TotalPage; pn++) {
		if (!is_overflow_page(dic, P, pn, nd->size))
		    break;
	    }
	    if (pn == dic->TotalPage) {
	      fprintf(stderr, "error:too many pages %d, %d\n", pn, nd->size);
	      exit(1);
	    }
	} else {
	    pn = page_num;
	}
	nd->page_num = pn;
	nd->page_off = P[pn].dirsiz + P[pn].lnksiz + P[pn].wrdsiz;
	P[pn].lnksiz += dic->LinkSize;
	P[pn].wrdsiz += nd->wrec_bytes;
	P[pn].nlinks++;
	P[pn].nwrecs++;
	P[pn].candnum += _RkCandNumber(nd->ptr.w);
	return page_num;
    } else {
	nid = getp(nd);
	if (nd->size >= dic->PageSize || atop) {
	    atop = 0;
	    nd->page_num = -1;
	    nd->page_off = D->dirsiz;
	    D->dirsiz += dic->DirNodeSize * (nid + 1);
	    D->nnode += nid + 1;
	    dic->empty += (nid - nd->count) * dic->DirNodeSize;
	    for (i = 0; i < nd->count; i++) {
		struct node	*child = &nd->ptr.n[i];
		
		page_num = assign_to_page(dic,
					  child,
					  page_num,
					  1);
	    }
	    return page_num;
	} else {
	    if (!is_pn_indir) {
		pn = page_num;
		if (is_overflow_page(dic, P, page_num, nd->size)) {
		  fprintf(stderr, "error:can't allocate in page %d\n", pn);
		  exit(1);
		}
	    } else {
	      for (pn = 0; pn < dic->TotalPage; pn++) {
		if (!is_overflow_page(dic, P, pn, nd->size))
		  break;
	      }
	      if (pn == dic->TotalPage) {
		nd->page_num = -1;
		nd->page_off = D->dirsiz;
		D->dirsiz += dic->DirNodeSize * (nid + 1);
		D->nnode += nid + 1;
		dic->empty += (nid - nd->count) * dic->DirNodeSize;
		for (i = 0; i < nd->count; i++) {
		  struct node	*child = &nd->ptr.n[i];
		  
		  page_num = assign_to_page(dic,
					    child,
					    page_num,
					    1);
		}
		return page_num;
	      }
	    }
	    P[pn].ndir++;
	    nd->page_num = pn;
	    nd->page_off = P[pn].dirsiz;
	    P[pn].dirsiz += dic->PagNodeSize * nd->count;
	    dic->empty += (nd->count - nd->count) * dic->PagNodeSize;
	    P[pn].nnode += nd->count;
	    for (i = 0; i < nd->count; i++) {
		pn = assign_to_page(dic, &nd->ptr.n[i], pn, 0);
	    }
	  }
    }
    return page_num;
}

void
calculate_dic_status(dic)
  struct dictionary	*dic;
{
    int		i, totalcand = 0, snd = 0;
    
    for (i = 0; i < dic->TotalPage; i++) {
	struct page	*P = &dic->Page[i];
	
	if (P->dirsiz == PAGE_HDR_SIZ && !P->lnksiz && !P->wrdsiz)
	    break;
	P->first_csn = totalcand;
	P->first_lvo = 0;
	totalcand += dic->Page[i].candnum;
	snd +=  dic->Page[i].nnode;
    }
    dic->TotalPage = i;
    dic->TotalCand = totalcand;
    dic->Snd = snd;
    dic->Lnd = dic->Dir->nnode;
}

void
fil_ltab(gram, dic)
     struct dictionary	*dic;
     struct RkKxGram	*gram;
{
  unsigned long	first_lvo, pwo, lvo, csn;
  int			i, pn;
  first_lvo = 0;
  for (pn = 0; pn < dic->TotalPage; pn++) {
    struct page	*P;
    unsigned char	*dst;
    unsigned char	*wrec;
    unsigned		wlen;
    unsigned char	*ptr;
    
    P = &dic->Page[pn];
    ptr = dst =  P->buf + P->dirsiz;
    wrec = dst + P->lnksiz;
    pwo = wrec - P->buf;
    csn = P->first_csn;
    P->first_lvo = first_lvo;
    lvo = 0;
    for (i = 0; i < P->nwrecs; i++) {
      unsigned	nc, lnksiz;

      wlen = _RkWordLength(wrec);
      nc = _RkCandNumber(wrec);
      lnksiz = (unsigned long)nc*(_RkCalcLog2(nc+1)+1);
      *ptr++ = (pwo >> 6) & 0xff;
      *ptr++ = ((pwo << 2) & 0xfc) | ((lvo >> 13) & 0x03);
      *ptr++ = (lvo >> 5) & 0xff;
      *ptr++ = ((lvo << 3) & 0xf8) | ((csn >> 8) & 0x07);
      *ptr++ = csn & 0xff;
      P->nlinks++;
      lvo += lnksiz;
      first_lvo += lnksiz;
      csn += nc;
      pwo += wlen;
      wrec += wlen;
    }
  }
}

void
fil_page_header(dic)
     struct dictionary	*dic;
{
  int		pn;
  unsigned char	*dst;
  
  for (pn = 0; pn < dic->TotalPage; pn++) {
    struct page	*P = &dic->Page[pn];
    
    dst = P->buf;
    s_to_bst2(pn, dst); dst += 2;
    s_to_bst2(P->nnode, dst); dst += 2;
    s_to_bst2(P->nwrecs, dst); dst += 2;
    *dst++ = 0;
    l_to_bst3(P->first_lvo, dst); dst += 3;
    l_to_bst3(P->first_csn, dst); dst += 3;
    *dst++ = 0;
  }
}
struct node *
build_tree(parent, dic, gram, wrec_ptr, d, top, bot, dir_nodes)
  struct node		*parent;
  struct dictionary	*dic;
  struct RkKxGram	*gram;
  struct TextDic	*wrec_ptr;
  unsigned		d, top, bot;
  unsigned		*dir_nodes;
{
    int			F1 = top;
    int			F2 = bot;
    unsigned		f;
    struct node		*dir;
    int			i, k;
    int			left;
    int			size;
    
    *dir_nodes = 0;
    while (top < bot) {
	if (!wrec_ptr[top].yomi) {
	    fprintf(stderr, "Line number mismatch.\n");
	    exit(1);
	}
	for (f = top + 1; f < bot; f++)
	    if (wrec_ptr[top].yomi[d] != wrec_ptr[f].yomi[d])
		break;
	top = f;
	(*dir_nodes)++;
	if (!*dir_nodes) {
	    fprintf(stderr, "fatal error found: n nodes overflowed!!\n");
	    exit(1);
	}
    }
    if (!*dir_nodes) {
	fprintf(stderr, "found no directory\n");
	exit(1);
    }
    ;
    if (!(dir = (struct node *)calloc(*dir_nodes, (sizeof(struct node))))) {
	fprintf(stderr, "no space\n");
	exit(1);
    }
    k = 0;
    top = F1; bot = F2;
    while (top < bot) {
	for (f = top + 1; f < bot; f++) {
	    if (wrec_ptr[top].yomi[d] != wrec_ptr[f].yomi[d]) {
		break;
	    }
	}
	dir[k].key = wrec_ptr[top].yomi[d];
	if (top + 1 == f) {
	    unsigned char	*wrec, *dst, localbuf[RK_WREC_BMAX];
	    unsigned		sz;
	    
	    dir[k].count = 0;
	    dir[k].ptr.w = 0;
	    {
		int	j;
		
		for (j = d, left = 0 ; wrec_ptr[top].yomi[j]; j++, left++) {
		    if (!wrec_ptr[top].yomi[j]) {
		      break;
		    }
		}
		if (left > 0)
		    left--;
	    }
	    dst = RkParseWrec(gram,
			      wrec_ptr[top].line,
			      left,
			      localbuf,
			      sizeof(localbuf));
	    if (!dst) {
	        fprintf(stderr, "Error in RkParseWrec\n");
		exit(1);
	    }
	    sz = dst - localbuf;
	    dir[k].wrec_bytes = sz;
	    if (!(wrec = (unsigned char *)malloc(sz))) {
		fprintf(stderr, "no space\n");
		exit(1);
	    }
	    dir[k].ptr.w = wrec;
	    memcpy((char *)wrec, (char *)localbuf, sz);
	    size = dir[k].wrec_bytes + dic->LinkSize;
	} else {
	    dir[k].ptr.n = build_tree(&dir[k],
				      dic,
				      gram,
				      wrec_ptr,
				      d + 1,
				      top,
				      f,
				      &dir[k].count);
	    dir[k].wrec_bytes = 0;
	    size = dic->PagNodeSize * dir[k].count;
	    for (i = 0; i < dir[k].count; i++) {
		struct node	*child = &dir[k].ptr.n[i];
		
		size += child->size;
	    }
	}
	dir[k].size = size;
	if (dir[k].size >= dic->PageSize) {
	    dir[k].page_num = -1;
	}
	top = f;
	k++;
    }
    return dir;
}

static
struct node *
creat_tree(dic, gram)
     struct dictionary	*dic;
     struct RkKxGram	*gram;
{
  int			i;
  struct TextDic	*top;
  unsigned		nnodes, nel;
  struct node		*dir, *topnode;
    
  if (!(topnode = (struct node *)calloc(1, sizeof(struct node)))) {
    fprintf(stderr, "no space\n");
    exit(1);
  }
  if (!(top = open_wfile(textfile, &nel))) {
    fprintf(stderr, "cannot open file %s\n", textfile);
    exit(1);
  }
  dic->TotalRec = nel;
  if (!(dir = build_tree(topnode, dic, gram, top, 0, 0, nel, &nnodes))) {
    fprintf(stderr, "no space\n");
    exit(1);
  }
  topnode->key = 0xff;
  topnode->count = nnodes;
  topnode->ptr.n = dir;
  topnode->page_num = -1;
  topnode->wrec_bytes = 0;
  for (topnode->size = 0, i = 0; i < nnodes; i++) {
    topnode->size += dir[i].size;
  }
  (void)assign_to_page(dic, topnode, 0, 1);
  calculate_dic_status(dic);
  for (i = 0; i < nel; i++) {
    if (top[i].line) {
      free((char *)top[i].line);
    }
    if (top[i].yomi) {
      free((char *)top[i].yomi);
    }
  }
  free((char *)top);
  return topnode;
}

struct dictionary *
init_dic(name, dictype, maxpage)
     char	*name;
     int	dictype;
     unsigned	maxpage;
{
  struct dictionary	*dic;
  int			i;
  
  if (!(dic = (struct dictionary *)malloc(sizeof(struct dictionary)))
       || !(dic->Dir = (struct direc *)malloc(sizeof(struct direc)))
       || !(dic->Wlist = (struct wlist *)malloc(sizeof(struct wlist)))
       || !(dic->Page = (struct page *)malloc(maxpage*sizeof(struct page)))
    ) {
    fprintf(stderr, "no space\n");
    exit(1);
  }
  dic->Dir->buf = 0;
  dic->Dir->dirsiz = dic->Dir->diroff = 0;
  dic->Dir->nnode = 0;
  dic->Wlist->nd = (struct node *)0;
  dic->Wlist->next = (struct wlist *)0;
  dic->Wlist->size = 0;
  dic->TotalPage = maxpage;
  for (i = 0; i < dic->TotalPage; i++) {
    dic->Page[i].buf = (unsigned char *)0;
    dic->Page[i].diroff = dic->Page[i].dirsiz = PAGE_HDR_SIZ;
    dic->Page[i].lnksiz = dic->Page[i].wrdsiz =
      dic->Page[i].wrdoff = dic->Page[i].nwrecs =
	dic->Page[i].nnode = dic->Page[i].ndir =
	  dic->Page[i].nlinks = dic->Page[i].nwrecs =
	    dic->Page[i].candnum = 0;
    dic->Page[i].first_lvo = dic->Page[i].first_csn = -1;
  }
  dic->MaxCand = _RkCalcUnlog2(11);
  dic->PageSize = _RkCalcUnlog2(13) + 1;
  dic->TotalRec = 0;
  dic->TotalCand = 0;
  dic->Cwidth = 2;
  dic->PagNodeSize = dic->Cwidth + PAG_NDVAL_LEN;
  dic->DirNodeSize = dic->Cwidth + DIR_NDVAL_LEN;
  dic->LinkSize = 5;
  dic->Lnd = dic->Snd = 0;
  dic->rest = 0;
  dic->hdr = 0;
  dic->hdrsiz = 0;
  dic->empty = 0;
  strcat(name, dictype == JMWD ? ".mwd" : ".swd");
  dic->name = name;
  dic->type = dictype;
  return dic;
}

setHeader(hd, jdic_type, Wwidth, Wtype,
	  notice, copyright, LocaleName,
	  dmname, codmname, HSize, Size, Doff, Poff,
	  totalCand, totalRec, totalPage, Lnd, Snd)
     
     struct HD	*hd;
     int	jdic_type;
     unsigned	Wwidth, HSize, Size, Doff, Poff;
     char	*notice, *copyright, *LocaleName, *dmname, *codmname;
     char 	*Wtype;
     unsigned	totalRec;
     unsigned	totalCand;
     unsigned	totalPage;
     unsigned	Lnd;
     unsigned	Snd;
{
  int		i;
  unsigned char s[5];

  if (!hd)
    return(-1);

  for (i = 0; i < HD_MAXTAG; i++) {
    hd->data[i].ptr = 0;
    hd->flag[i] = 0;
  }
  hd->data[HD_MAG].var = bst4_to_l("CDIC");
  hd->flag[HD_MAG] = -1;
  hd->data[HD_VER].var = bst4_to_l("R3.0");
  hd->flag[HD_VER] = -1;
  hd->data[HD_TIME].var = tloc = time(0);
  hd->flag[HD_TIME] = -1;
  hd->data[HD_DMNM].ptr = (unsigned char *)STrdup(dmname);
  hd->flag[HD_DMNM] = strlen(dmname);
  hd->data[HD_LANG].ptr = (unsigned char *)STrdup(LocaleName);
  hd->flag[HD_LANG] = strlen(LocaleName);
  hd->data[HD_WWID].var = Wwidth;
  hd->flag[HD_WWID] = -1;
  strcpy((char *)s, Wtype);
  hd->data[HD_WTYP].var = bst4_to_l(s);
  hd->flag[HD_WTYP] = -1;
  if (notice) {
    hd->data[HD_NOTE].ptr = (unsigned char *)STrdup(notice);
    hd->flag[HD_NOTE] = strlen(notice);
  }
  if (copyright) {
    hd->data[HD_COPY].ptr = (unsigned char *)STrdup(copyright);
    hd->flag[HD_COPY] = strlen(copyright);
  }
  hd->data[HD_TYPE].var = bst4_to_l(DEF_TYPE);
  hd->flag[HD_TYPE] = -1;
  hd->data[HD_HSZ].var = HSize;
  hd->flag[HD_HSZ] = -1;
  hd->data[HD_SIZ].var = Size;
  hd->flag[HD_SIZ] = -1;
  
  hd->data[HD_DROF].var = Doff;
  hd->flag[HD_DROF] = -1;
    
  hd->data[HD_PGOF].var = Poff;
  hd->flag[HD_PGOF] = -1;

  hd->data[HD_L2P].var = 13;
  hd->flag[HD_L2P] = -1;

  hd->data[HD_L2C].var = 11;
  hd->flag[HD_L2C] = -1;
    
  hd->data[HD_REC].var = totalRec;
  hd->flag[HD_REC] = -1;
    
  hd->data[HD_CAN].var = totalCand;
  hd->flag[HD_CAN] = -1;
    
  hd->data[HD_PAG].var = totalPage;
  hd->flag[HD_PAG] = -1;
    
  hd->data[HD_LND].var = Lnd;
  hd->flag[HD_LND] = -1;
    
  hd->data[HD_SND].var = Snd;
  hd->flag[HD_SND] = -1;
    
  return(0);
}

static void
makeHeader(dic)
     struct dictionary	*dic;
{
  unsigned char		*buf;
  unsigned		size;
  struct HD		hd;
    
  setHeader(&hd,
	    dic->type,
	    dic->Cwidth,
	    DEF_WTYP,
	    (char *)0,
	    (char *)0,
	    DEFAULT_JAPANESE_LOCALE,
	    dic->name,
	    "",
	    0, 0, 0, 0,
	    dic->TotalCand,
	    dic->TotalRec, 
	    dic->TotalPage,
	    dic->Lnd,
	    dic->Snd
	    );
  if (!(buf = _RkCreateHeader(&hd, &size))) {
    fprintf(stderr, "no space\n");
    exit(1);
  }

  free((char *)buf);

  setHeader(&hd,
	    dic->type,
	    dic->Cwidth,
	    DEF_WTYP,
	    (char *)0,
	    (char *)0,
	    DEFAULT_JAPANESE_LOCALE,
	    dic->name, "", size,
	    size + dic->TotalPage * dic->PageSize + dic->Dir->dirsiz,
	    size,
	    size + dic->Dir->dirsiz,
	    dic->TotalCand,
	    dic->TotalRec, 
	    dic->TotalPage,
	    dic->Lnd,
	    dic->Snd
	    );
  if (!(buf = _RkCreateHeader(&hd, &size))) {
    fprintf(stderr, "no space.\n");
    exit(1);
  }
  dic->hdr = buf;
  dic->hdrsiz = size;
  return;
}


static void
write_file(out, dic)
     char		*out;
     struct dictionary	*dic;
{
  int	i, fd;
    
  unlink(out);
  if ((fd = open(out, (O_CREAT | O_RDWR | O_APPEND), 0644)) < 0) {
    fprintf(stderr, "can't create %s\n", out);
    exit(1);
  }
  
  makeHeader(dic);
  
  if (dic->hdr)
    if (write(fd, (char *)dic->hdr, dic->hdrsiz) != dic->hdrsiz) {
      fprintf(stderr, "%s: cannot write\n", program);
      close(fd);
      exit(1);
    }
  
  if (write(fd, (char *)dic->Dir->buf, dic->Dir->dirsiz) != dic->Dir->dirsiz) {
    fprintf(stderr, "%s: cannot write\n", program);
    close(fd);
    exit(1);
  }
  for (i = 0; i < dic->TotalPage; i++) {
    struct page	*P = &dic->Page[i];
    
    if (write(fd, (char *)P->buf, dic->PageSize) != dic->PageSize) {
      fprintf(stderr, "%s: cannot write\n", program);
      close(fd);
      exit(1);
    }
  }
  close(fd);
}

static void
usage()
{
  fprintf(stderr, "usage: crxdic [option] -o dicfile text\n");
  fprintf(stderr, "\toptions:\n");
  fprintf(stderr, "\t-D cnj.bits\n");
  fprintf(stderr, "\t-n dicname\n");
  fprintf(stderr, "\t-m \n");
  fprintf(stderr, "\t-s \n");
  exit(1);
}

static char *
basename(name)
     char	*name;
{
  char	*s = name + strlen(name);
  
  while (s-- > name)
    if (*s == '/')
      return(++s);
  return(name);
}

static void
parse_arg(argc, argv)
     int argc;
     char *argv [];
{
  int		i;
  
  for (i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-D")) {
      if (++i < argc) {
	gfile = argv[i];
	continue;
      }
    } else if (!strcmp(argv[i], "-s")) {
      type = JSWD;
      continue;
    } else if (!strcmp(argv[i], "-m")) {
      type= JMWD;
      continue;
    } else if (!strcmp(argv[i], "-o") && !outfile[0]) {
      if (++i < argc) {
	strcpy(outfile, argv[i]);
	continue;
      }
    } else if (!strcmp(argv[i], "-n") && !dicname[0]) {
      if (++i < argc) {
	strcpy(dicname, argv[i]);
	continue;
      }
    } else if (!textfile[0]) {
      strcpy(textfile, argv[i]);
      continue;
    }
    usage();
  }
  if (!textfile[0] || !outfile[0])
    usage();
}

getp(nd)
     struct node	*nd;
{
  int	n, k;
  
  if ((n = nd->count * 1.2) == 1)
    return(2);
  n += (n % 2) ? 1 : 2;
 loop:
  for (k = 3; k * k <= n; k += 2)
    if (!(n % k)) {
      n += 2;
      goto loop;
    }
  return(n);
}

main (argc, argv)
     int	argc;
     char	**argv;
{
  struct dictionary	*dic;
  struct node		*topnd;
  int			fd, pg, i;
  struct RkKxGram	*gram;
  char			date[26], tempfile[1024];
  
  program = basename(argv[0]);
  textfile[0] = dicname[0] = outfile[0] = 0;
  parse_arg(argc, argv);
  (void)strcpy(tempfile, basename(textfile));
  for (i = strlen(tempfile), dicname[i] = 0; i--;)
    if (tempfile[i] == '.')
      dicname[i] = 0;
    else
      dicname[i] = tempfile[i];
  if (!dicname[0])
    usage();

  if (!gfile) {
    if(!(gram = RkOpenGram(HYOUJUN_GRAM))) {
      fprintf(stderr, "Warning: cannot open grammar file %s.\n", HYOUJUN_GRAM);
      exit(1);
    }
  } else {
    if ((fd = open(gfile, 0)) < 0 || !(gram = RkReadGram(fd))) {
      fprintf(stderr, "%s: cannot open grammar file %s.\n", program, gfile);
      exit(1);
    }
    close(fd);
  }

  if (!(dic = init_dic(dicname, type, 1024))) {
    fprintf(stderr, "no space.\n");
    exit(1);
  }
  topnd = creat_tree(dic, gram);
  alloc_dir(dic);
  alloc_page(dic, dic->TotalPage);
  (void)fil_dic(topnd, dic, &pg);
  fil_ltab(gram, dic);
  fil_page_header(dic);
  if (!outfile[0]) {
    strcpy(outfile, dicname);
    strcat(outfile, ".d");
  }
  write_file(outfile, dic);
  strcpy(date, ctime( &tloc ));
  date[24] = 0;
  (void)fprintf(stderr, "%s [ %s ] = %d + %d\n",
		dicname, date, dic->TotalCand, dic->TotalRec);
  return(0);
}

