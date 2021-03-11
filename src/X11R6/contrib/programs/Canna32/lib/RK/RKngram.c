/* Copyright 1994 NEC Corporation, Tokyo, Japan.
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

#if !defined(lint) && !defined(__CODECENTER__)
static char rcsid[]="$Id: RKngram.c,v 2.13 1994/06/01 06:54:27 misao Exp $";
#endif

#include	"RKintern.h"

#include	<stdio.h>
#ifdef SVR4
#include	<unistd.h>
#endif
#if defined(USG) || defined(SYSV) || defined(SVR4)
#include	<string.h>
#else
#include	<strings.h>
#endif
#include <memory.h>

extern unsigned char *ustoeuc();

void
RkCloseGram(gram)
     struct RkKxGram	*gram;
{
  if (gram->ng_conj)
    (void)free((char *)gram->ng_conj);
  if (gram->ng_strtab)
    (void)free((char *)gram->ng_strtab);
  (void)free((char *)gram);
}

static
char	**
gram_to_tab(gram)
     struct RkKxGram	*gram;
{
  char	**top, *str;
  int	i;
  
  if (!(top = (char **) calloc(gram->ng_rowcol + 1, sizeof(char *)))) {
    RkSetErrno(RK_ERRNO_ENOMEM);
    return 0;
  }
  str = gram->ng_conj + (gram->ng_rowbyte * gram->ng_rowcol);
  for (i = 0; i < gram->ng_rowcol; i++) {
    top[i] = str;
    str += strlen(str) + 1;
  };
  top[gram->ng_rowcol] = (char *)0;
  return top;
}

/* RkGetGramSize -- gram_conj に入れているメモリの大きさを返す */
static int
RkGetGramSize(gram)
struct RkKxGram *gram;
{
  char *str;
  int i;

  str = gram->ng_conj + (gram->ng_rowbyte * gram->ng_rowcol);
  for (i = 0; i < gram->ng_rowcol; i++) {
    str += strlen(str) + 1;
  }
  return str - gram->ng_conj;
}

struct RkKxGram *
RkReadGram(fd)
     int	fd;
{
  struct RkKxGram	*gram = (struct RkKxGram *)0;
  unsigned char		l4[4];
  unsigned long		sz, rc;
    
  RkSetErrno(RK_ERRNO_EACCES);
  if (!(read(fd, (char *)l4, 4) < 4 || (sz = L4TOL(l4)) < 5
	|| read(fd, (char *)l4, 4) < 4 || (rc =  L4TOL(l4)) < 1)) {
    gram = (struct RkKxGram *)calloc(1, sizeof(struct RkKxGram));
    RkSetErrno(RK_ERRNO_ENOMEM);
    if (gram) {
      gram->ng_conj = (char *)malloc((unsigned)(sz - 4));
      if (gram->ng_conj) {
	if (read(fd, gram->ng_conj, (unsigned)(sz - 4)) == (sz - 4)) {
	  gram->ng_rowcol = rc;
	  gram->ng_rowbyte = (gram->ng_rowcol + 7) / 8;
	  gram->ng_strtab = gram_to_tab(gram);
	  if (gram->ng_strtab) {
	    return gram;
	  }
	}
	else {
	  /* EMPTY */
	  RkSetErrno(0);
	}
	free(gram->ng_conj);
      }
      free(gram);
    }
  }
  return (struct RkKxGram *)0;
}

struct RkKxGram	*
RkOpenGram(mydic)
     char	*mydic;
{
  struct RkKxGram	*gram;
  struct HD		hd;
  int			fd;
  unsigned		off;
  int			lk;
  
  if ((fd = open(mydic, 0)) < 0)
    return (struct RkKxGram *)0;
  for (off = 0, lk = 1; lk && _RkReadHeader(fd, &hd, off) >= 0;) {
    off += hd.data[HD_SIZ].var;
    if (!strncmp(".swd",
		 (char *)(hd.data[HD_DMNM].ptr
			  + strlen((char *)hd.data[HD_DMNM].ptr) - 4),
		 4)) {
      lk = 0;
      if (lseek(fd, off, 0) < 0) {
	lk = 1;
	RkSetErrno(RK_ERRNO_EACCES);
      }
      break;
    }
    _RkClearHeader(&hd);
  }
  _RkClearHeader(&hd);
  if (lk) {
    close(fd);
    return((struct RkKxGram *)0);
  }
  gram = RkReadGram(fd);
  (void)close(fd);
  return gram;
}

struct RkKxGram	*
RkDuplicateGram(ogram)
struct RkKxGram *ogram;
{
  struct RkKxGram *gram = (struct RkKxGram *)0;
    
  gram = (struct RkKxGram *)calloc(1, sizeof(struct RkKxGram));
  if (gram) {
    int siz = RkGetGramSize(ogram);
    gram->ng_rowcol = ogram->ng_rowcol;
    gram->ng_rowbyte = ogram->ng_rowbyte;
    gram->ng_conj = (char *)malloc(siz);
    if (gram->ng_conj) {
      bcopy(ogram->ng_conj, gram->ng_conj, siz);
      gram->ng_strtab = gram_to_tab(gram);
      if (gram->ng_strtab) {
	return gram;
      }
      free(gram->ng_conj);
    }
    free((char *)gram);
  }
  RkSetErrno(RK_ERRNO_ENOMEM);
  return (struct RkKxGram *)0;
}

int
_RkWordLength(wrec)
     unsigned char	*wrec;
{
  int	wl;
    
  wl = ((wrec[0] << 5) & 0x20) | ((wrec[1] >> 3) & 0x1f);
  if (wrec[0] & 0x80)
    wl |= ((wrec[2] << 5) & 0x1fc0);
  return(wl);
}

int
_RkCandNumber(wrec)
     unsigned char	*wrec;
{
  int	nc;
  
  nc = (wrec)[1] & 0x07;
  if (wrec[0] & 0x80)
    nc |= ((wrec[3] << 3) & 0x0ff8);
  return(nc);
}

int
RkGetGramNum(gram, name)
     struct RkKxGram	*gram;
     char			*name;
{
  int	row;
  int	max = gram->ng_rowcol;
    
  if (gram->ng_strtab) {
    for (row = 0; row < max; row++) {
      if (!strcmp(gram->ng_strtab[row], (char *)name))
	return(row);
    }
  }
  return(-1);
}

static Wchar *
skip_space(src)
     Wchar	*src;
{
  while (*src) {
    if (!rk_isspace(*src))
      break;
    src++;
  }
  return(src);
}

static
skip_until_space(src, next)
Wchar	*src, **next;
{
  int len = 0;

  while (*src) {
    if (rk_isspace(*src)) {
      break;
    }
    else if (*src == RK_ESC_CHAR) {
      if (!*++src) {
	break;
      }
    }
    src++;
    len++;
  }
  *next = src;
  return len;
}

static int
wstowrec(gram, src, dst, maxdst, yomilen, wlen, lucks)
     struct RkKxGram	*gram;
     Wchar		*src;
     Wrec		*dst;
     unsigned		maxdst;
     unsigned		*yomilen, *wlen;
     unsigned		*lucks;
{
  Wrec		*odst = dst;
  Wchar		*yomi, *kanji;
  int		klen, ylen, ncand, row = 0, step = 0, spec = 0;
  unsigned	frq;
    
  lucks[0] = lucks[1] = 0;
  ncand = 0;
  *yomilen = *wlen = 0;
  yomi = skip_space(src);
  ylen = skip_until_space(yomi, &src);
  if (!ylen || ylen > RK_LEFT_KEY_WMAX)
    return(0);
  while (*src) {
    if (*src == (Wchar)'\n')
      break;
    if (*(src = skip_space(src)) == (Wchar)'#') {
      src = RkParseGramNum(gram, src, &row);
      if (!is_row_num(gram, row))
	break;
      if (*src == (Wchar)'#')
	continue;
      if (*src == (Wchar)'*') {
	for (src++, frq = 0; (Wchar)'0' <= *src && *src <= (Wchar)'9'; src++)
	  frq = 10 * frq + *src - (Wchar)'0';
	if (step < 2  && frq < 6000)
	  lucks[step] = _RkGetTick(0) - frq;
      }
      src = skip_space(src);
      spec++;
    }
    if (!spec)
      return(0);
    kanji = src;
    klen = skip_until_space(src, &src);
    if (klen == 1 && *kanji == (Wchar)'@') {
      klen = ylen;
      kanji = yomi;
    }
    if (!klen || klen > RK_LEN_WMAX)
      return(0);
    if (dst + klen * sizeof(Wchar) > odst + maxdst)
      return(0);
    step++;
    *dst++ = (Wrec)(((klen << 1) & 0xfe) | ((row >> 8) & 0x01));
    *dst++ = (Wrec)(row & 0xff);
    for (; klen > 0 ; klen--, kanji++) {
      if (*kanji == RK_ESC_CHAR) {
	if (!*++kanji) {
	  break;
	}
      }
      *dst++ = (Wrec)((*kanji >> 8) & 0xff);
      *dst++ = (Wrec)(*kanji & 0xff);
    }
    ncand++;
  }
  if (ncand) {
    *wlen = (unsigned)(dst - odst);
    *yomilen= ylen;
  }
  return ncand;
}

static Wrec *
fil_wc2wrec_flag(wrec, wreclen, ncand, yomi, ylen, left)
     Wrec	*wrec;
     Wchar	*yomi;
     unsigned	*wreclen, ylen, ncand, left;
{
  extern Wchar	uniqAlnum();
  Wrec		*owrec = wrec;
  Wchar		tmp;
  int		wlen = *wreclen, i;
  
  if ((ncand > 7) || (wlen > 0x3f)) {
    wlen += 2;
    *wrec = 0x80;
  } else {
    *wrec = 0;
  }
  *wrec++ |= (Wrec)(((left << 1) & 0x7e) | ((wlen >> 5) & 0x01));
  *wrec++ = (Wrec)(((wlen << 3) & 0xf8) | (ncand & 0x07));
  if (*owrec & 0x80) {
    *wrec++ = (Wrec)(((wlen >> 5) & 0xfe) | (ncand >> 11) & 0x01);
    *wrec++ = (Wrec)((ncand >> 3) & 0xff);
  }
  if (left) {
    int offset = ylen - left; /* ディレクトリ部に入っている読みの長さ */
    if (offset < 0)
      return((Wrec *)0);
    for (i = 0 ; i < offset ; i++) {
      if (*yomi == RK_ESC_CHAR) {
	yomi++;
      }
      yomi++;
    }
    for (i = 0 ; i < left ; i++) {
      tmp = *yomi++;
      if (tmp == RK_ESC_CHAR) {
	tmp = *yomi++;
      }
      tmp = uniqAlnum(tmp);
      *wrec++ = (tmp >> 8) & 0xff;
      *wrec++ = tmp & 0xff;
    }
  }
  *wreclen = wlen;
  return wrec;
}

static Wrec *
fil_wrec_flag(wrec, wreclen, ncand, yomi, ylen, left)
     Wrec	*wrec, *yomi;
     unsigned	*wreclen, ylen, ncand, left;
{
  extern Wchar	uniqAlnum();
  Wrec		*owrec = wrec;
  Wchar		tmp;
  int		wlen = *wreclen, i;
  
  if ((ncand > 7) || (wlen > 0x3f)) {
    wlen += 2;
    *wrec = 0x80;
  } else {
    *wrec = 0;
  }
  *wrec++ |= (Wrec)(((left << 1) & 0x7e) | ((wlen >> 5) & 0x01));
  *wrec++ = (Wrec)(((wlen << 3) & 0xf8) | (ncand & 0x07));
  if (*owrec & 0x80) {
    *wrec++ = (Wrec)(((wlen >> 5) & 0xfe) | (ncand >> 11) & 0x01);
    *wrec++ = (Wrec)((ncand >> 3) & 0xff);
  }
  if (left) {
    if (ylen < left)
      return((Wrec *)0);
    yomi += (ylen - left) * sizeof(Wchar);
    for (i = 0; i < left; i++) {
      tmp = uniqAlnum((Wchar)((yomi[2*i] << 8) | yomi[2*i + 1]));
      *wrec++ = (tmp >> 8) & 0xff;
      *wrec++ = tmp & 0xff;
    }
  }
  *wreclen = wlen;
  return wrec;
}

Wrec *
RkParseWrec(gram, src, left, dst, maxdst)
     struct RkKxGram	*gram;
     Wchar		*src;
     unsigned		left;
     unsigned char	*dst;
     unsigned		maxdst;
{
  unsigned char	localbuffer[RK_WREC_BMAX];
  unsigned	wreclen, wlen, ylen, nc;
  unsigned	lucks[2];

  if (!(nc = wstowrec(gram, src, localbuffer,
		      sizeof(localbuffer)/sizeof(Wchar), &ylen,
			  &wlen, lucks)))
    return (unsigned char *)0;
  
  if ((wreclen = 2 + (left * sizeof(Wchar)) + wlen) > maxdst)
    return (unsigned char *)0;
  if (left > ylen)
    return (unsigned char *)0;
  dst = fil_wc2wrec_flag(dst, &wreclen, nc, src, ylen, left);
  if (dst) {
    (void)memcpy((char *)dst, (char *)localbuffer, wlen);
    return(dst + wlen);
  } else
    return(dst);
}

Wrec *
RkParseOWrec(gram, src, dst, maxdst, lucks)
     struct RkKxGram	*gram;
     Wchar		*src;
     unsigned char	*dst;
     unsigned		maxdst;
     unsigned		*lucks;
{
  Wrec localbuffer[RK_WREC_BMAX];
  unsigned	wreclen, wlen, ylen, nc;
    
  if (!(nc = wstowrec(gram, src, localbuffer,
		      sizeof(localbuffer)/sizeof(Wchar), &ylen, &wlen, lucks)))
    return (unsigned char *)0;
  wreclen = 2 + (ylen * sizeof(Wchar)) + wlen;
  if (wreclen > maxdst)
    return (unsigned char *)0;
  dst = fil_wc2wrec_flag(dst, &wreclen, nc, src, ylen, ylen);
  if (dst) {
    (void)memcpy((char *)dst, (char *)localbuffer, wlen);
    return(dst + wlen);
  }
  else {
    return(dst);
  }
}

Wchar *
RkParseGramNum(gram, src, row)
     struct RkKxGram	*gram;
     Wchar 		*src;
     int		*row;
{
  int		rnum;
  Wchar		*ws, wcode[RK_LINE_BMAX];
  unsigned char	*str, code[RK_LINE_BMAX];
    
  code[0] = 0;
  *row = -1;
  if (*src++ != (Wchar)'#')
    return --src;
  if (rk_isdigit(*src)) {
    for (ws = wcode; *src; *ws++ = *src++) {
      if (!rk_isdigit(*src) || rk_isspace(*src))
	break;
    }
    *ws = (Wchar)0;
    str = ustoeuc(wcode, (ws - wcode), code, sizeof(code));
    code[str - code] = (unsigned char)0;
    rnum = atoi((char *)code);
    if (is_row_num(gram, rnum))
      *row = rnum;
    return src;
  }
  if (rk_isascii(*src)) {
    for (ws = wcode; *src; *ws++ = *src++) {
      if (!rk_isascii(*src) || rk_isspace(*src) || *src == (Wchar)'*')
	break;
    }
    *ws = (unsigned char)0;
    str = ustoeuc(wcode, (ws - wcode), code, sizeof(code));
    code[str - code] = (unsigned char)0;
    rnum = RkGetGramNum(gram, (char *)code);
    if (is_row_num(gram, rnum))
      *row = rnum;
  }
  return src;
}

unsigned char *
RkGetGramName(gram, row)
     struct RkKxGram	*gram;
     int		row;
{
  if (gram && gram->ng_strtab)
    if (is_row_num(gram, row))
      return (unsigned char *)gram->ng_strtab[row];
  return 0;
}

Wchar *
RkUparseGramNum(gram, row, dst, maxdst)
     struct RkKxGram	*gram;
     int		row;
     Wchar		*dst;
     int		maxdst;
{
  unsigned char	*name, *p;
    
  name = (unsigned char *)RkGetGramName(gram, row);
  if (name) {
    int len = strlen((char *)name);
    if (len + 1 < maxdst) {
      *dst++ = (Wchar)'#';
      p = name;
      while (*p) {
	*dst++ = (Wchar)*p++;
      }
      *dst = (Wchar)0;
      return dst;
    }
    return (Wchar *)0;
  } else {
    int keta, uni, temp = row;
    
    for (keta = 1, uni = 1; temp > 9; keta++) {
      temp /= 10;
      uni *= 10;
    }
    if (dst + keta + 1 < (dst + maxdst)) {
      *dst++ = '#';
      while (keta--) {
	temp = row / uni;
	*dst++ = ('0' + temp);
	row -= temp * uni;
	uni /= 10;
      }
      *dst = (Wchar)0;
      return dst;
    }
    return (Wchar *)0;
  }
}

int
_RkRowNumber(wrec)
     unsigned char	*wrec;
{
  int	row;
  
  row = (int)wrec[1];
  if (wrec[0] & 0x01)
    row += 256;
  return row;
}

Wchar	*
_RkUparseWrec(gram, src, dst, maxdst, lucks, add)
     struct RkKxGram	*gram;
     Wrec		*src;
     Wchar		*dst;
     int		maxdst;
     unsigned		*lucks;
     int		add;
{
  unsigned 	luck = _RkGetTick(0), val;
  unsigned char	*wrec = src;
  int 		num, ncnd, ylen, row, oldrow, i, l, oh = 0;
  Wchar		*endp = dst + maxdst, *endt, luckw[5], wch;
    
  endt = (Wchar *)0;
  ylen = (*wrec >> 1) & 0x3f;
  ncnd = _RkCandNumber(wrec);
  if (*wrec & 0x80)
    wrec += 2;
  wrec += 2;
  for (i = 0; i < ylen; i++) {
    if (endp <= dst)
      return(endt);
    wch = (Wchar)((wrec[0] << 8) | wrec[1]);
    if (wch == RK_ESC_CHAR || wch == (Wchar)' ' || wch == (Wchar)'\t') {
      *dst++ = RK_ESC_CHAR;
    }
    *dst++ = wch;
    wrec += 2;
  }
  oldrow = -1;
  for (i = 0; i < ncnd; i++, oh = 0) {
    unsigned	clen;
    
    clen = (*wrec >> 1) & 0x7f;
    row = _RkRowNumber(wrec);
    wrec += NW_PREFIX;
    if (oldrow != row) {
      *dst++ = (Wchar)' ';
      if (!(dst = RkUparseGramNum(gram, (int)row, dst, endp - dst)))
 	break;
      oldrow = row;
      if (endp <= dst)
	break;
      oh++;
    }
    if (add && i < 2 && lucks[i]) {
      if (!oh) {
	*dst++ = (Wchar)' ';
	if (!(dst = RkUparseGramNum(gram, (int)row, dst, endp - dst))) 
	  break;
	oh++;
      }
      if (endp <= dst)
	break;
      if (luck - lucks[i] < (unsigned)6000) {
	*dst++ = (Wchar)'*';
	val = luck - lucks[i];
	for (l = 0; val && l < 5; l++, val /= 10) {
	  num = val % 10;
	  luckw[l] = (Wchar)(num + '0');
	}
	if (!l) {
	  luckw[l] = (Wchar)'0';
	  l++;
	}
	if (endp <= dst + l + 1)
	  break;
	while (l)
	  *dst++ = luckw[--l];
      }
    }
    if (endp <= dst)
      break;
    *dst++ = (Wchar)' ';
    if (clen != 0) {
      if (endp <= (dst + clen + 1))
	break;
      while (clen--) {
	wch = bst2_to_s(wrec);
	if (wch == RK_ESC_CHAR || wch == (Wchar)' ' || wch == (Wchar)'\t') {
	  *dst++ = RK_ESC_CHAR;
	}
	*dst++ = wch;
	wrec += sizeof(Wchar);
      }
    } else {
      if (endp <= dst)
	break;
      *dst++ = (Wchar)'@';
    }
    if (dst < endp - 1)
      endt = dst;
  }

  if (add && i == ncnd || !add && endt && endt < endp - 1) {
    *endt = (Wchar)0;
    return(endt);
  }
  return((Wchar *)0);
}

Wchar	*
RkUparseWrec(gram, src, dst, maxdst, lucks)
     struct RkKxGram	*gram;
     Wrec		*src;
     Wchar		*dst;
     int		maxdst;
     unsigned		*lucks;
{
  return(_RkUparseWrec(gram, src, dst, maxdst, lucks, 0));
}

struct TW *
RkCopyWrec(src)
     struct TW	*src;
{
  struct TW	*dst;
  unsigned int	sz;
  
  sz = _RkWordLength(src->word);
  if (sz) {
    dst = (struct TW *)malloc(sizeof(struct TW));
    if (dst) {
      dst->word = (unsigned char *)malloc(sz);
      if (dst->word) {
	(void)memcpy(dst->word, src->word, sz);
	dst->lucks[0] = src->lucks[0];
	dst->lucks[1] = src->lucks[1];
      } else {
	free(dst);
	dst = (struct TW *)0;
      }
    }
  }
  return(dst);
}

int
RkScanWcand(wrec, word, maxword)
     Wrec		*wrec;
     struct RkWcand	*word;
     int		maxword;
{
  int	i, l, nc, ns = 0;

  nc = _RkCandNumber(wrec);
  l = (*wrec >> 1) & 0x3f;
  if (*wrec & 0x80)
    wrec += 2;
  wrec += 2 + l * 2;
  for (i = 0; i < nc; i++) {
    int		rcnum;
    int		klen;
	
    klen = (*wrec >> 1) & 0x7f;
    rcnum = _RkRowNumber(wrec);
    if (i < maxword) {
      word[i].addr = wrec;
      word[i].rcnum = rcnum;
      word[i].klen = klen;
      ns++;
    };
    wrec += 2 * klen + 2;
  }
  return ns;
}


int
RkUniqWcand(wc, nwc)
     struct RkWcand	*wc;
     int		nwc;
{
  int		i, j, nu;
  Wrec		*a;
  unsigned 	k, r;
    
  for (nu = 0, j = 0; j < nwc; j++) {
    k = wc[j].klen;
    r = wc[j].rcnum;
    a = wc[j].addr + NW_PREFIX;
    for (i = 0; i < nu; i++)
      if (wc[i].klen == k && wc[i].rcnum == r && 
	  !memcmp(wc[i].addr + NW_PREFIX, a, 2 * k))
	break;
    if (nu <= i) 
      wc[nu++] = wc[j];
  }
  return(nu);
}

static struct TW *
RkWcand2Wrec pro((Wrec *, struct RkWcand *, int, unsigned *));

static struct TW *
RkWcand2Wrec(key, wc, nc, lucks)
     Wrec		*key;
     struct RkWcand	*wc;
     int		nc;
     unsigned		*lucks;
{
  unsigned char	localbuffer[RK_WREC_BMAX], *dst = localbuffer;
  int		i, j;
  unsigned	ylen, sz;
  struct TW	*tw = (struct TW *)0;
  Wrec		*wrec, *a;

  if ((ylen = (*key >> 1) & 0x3f) <= 0)
    return(tw);
  if (nc <= 0 || RK_CAND_NMAX < nc)
    return(tw);
  for (i = sz = 0; i < nc; i++) {
    if (wc[i].rcnum > 511)
      return(tw);
    if (wc[i].klen > NW_LEN)
      return(tw);
    sz += (wc[i].klen * 2) + NW_PREFIX;
  };
  if (*key & 0x80)
    key += 2;
  key += 2;
  sz += 2 + ylen * 2;
  tw = (struct TW *)malloc(sizeof(struct TW));
  if (tw) {
    dst = fil_wrec_flag(dst, &sz, (unsigned)nc, key, ylen, ylen);
    if (dst) {
      wrec = (Wrec *)malloc((unsigned)(sz));
      if (wrec) {
	(void)memcpy((char *)wrec, (char *)localbuffer, dst - localbuffer);
	dst = wrec + (dst - localbuffer);
	for ( i = 0; i < nc; i++ ) {
	  *dst++ = (Wrec)(((wc[i].klen << 1) & 0xfe)
			  | ((wc[i].rcnum >> 8) & 0x01));
	  *dst++ = (Wrec)(wc[i].rcnum & 0xff);
	  a = wc[i].addr + NW_PREFIX;
	  for (j = wc[i].klen * 2; j--;) 
	    *dst++ = *a++;
	}
	tw->lucks[0] = lucks[0];
	tw->lucks[1] = lucks[1];
	tw->word = wrec;
      } else {
	(void)free(tw);
	tw = (struct TW *)0;
      }
    }
  }
  return(tw);
}

int
RkUnionWcand(wc1, nc1, wlen1, wc2, nc2)
     struct RkWcand	*wc1, *wc2;
     int		wlen1, nc1, nc2;
{
  int		i, j, nu;
  Wrec		*a;
  unsigned 	k, r;

  nc1 = RkUniqWcand(wc1, nc1);
  nu = nc1;
  for (j = 0; j < nc2 && nu < wlen1; j++) {
    k = wc2[j].klen;
    r = wc2[j].rcnum;
    a = wc2[j].addr + NW_PREFIX;
    for (i = 0; i < nu; i++) {
      if (wc1[i].klen == k && wc1[i].rcnum == r &&
	  !memcmp(wc1[i].addr + NW_PREFIX, a, 2 * k))
	break;
    }
    if (nu <= i) 
      wc1[nu++] = wc2[j];
  }
  return(nu);
}

int
RkSubtractWcand(wc1, nc1, wc2, nc2, lucks)
     struct RkWcand	*wc1;
     struct RkWcand	*wc2;
     int		nc1;
     int		nc2;
     unsigned		*lucks;
{
  int		i, j, nu;
  Wrec		*a;
  unsigned 	k, r;

  nc1 = RkUniqWcand(wc1, nc1);
  for (nu = i = 0; i < nc1; i++) {
    k = wc1[i].klen;
    r = wc1[i].rcnum;
    a = wc1[i].addr + NW_PREFIX;
    for (j = 0; j < nc2; j++) {
      if (wc2[j].klen == k && wc2[j].rcnum == r &&
	  !memcmp(wc2[j].addr + NW_PREFIX, a, 2 * k))
	break;
    }
    if (nc2 <= j) {
      if (nu == 0 && i == 1) {
	lucks[0] = lucks[1];
	lucks[1] = 0;
      }
      wc1[nu++] = wc1[i];
    } else {
      if (nu < 2) {
	lucks[0] = lucks[1];
	lucks[1] = 0;
      }
    }
  }
  return(nu);
}

struct TW *
RkSubtractWrec(tw1, tw2)
     struct TW	*tw1;
     struct TW	*tw2;
{
  struct RkWcand	wc1[RK_CAND_NMAX], wc2[RK_CAND_NMAX];
  int			nc1, nc2, nc, ylen;
  Wrec			*a, *b, *wrec1 = tw1->word, *wrec2 = tw2->word;
  
  if ((ylen = (*wrec1 >> 1) & 0x3f) != ((*wrec2 >> 1) & 0x3f))
    return((struct TW *)0);
  if (*(a = wrec1) & 0x80)
    a += 2;
  a += 2;
  if (*(b = wrec2) & 0x80)
    b += 2;
  b += 2;
  while (ylen--) {
    if (*a++ != *b++ || *a++ != *b++) {
      return((struct TW *)0);
    }
  }
  nc1 = RkScanWcand(wrec1, wc1, RkNumber(wc1));
  nc2 = RkScanWcand(wrec2, wc2, RkNumber(wc2));
  nc = RkSubtractWcand(wc1, nc1, wc2, nc2, tw1->lucks);
  if (nc <= 0)
    return((struct TW *)0);
  else 
    return(RkWcand2Wrec(wrec1, wc1, nc, tw1->lucks));
}

struct TW *
RkUnionWrec(tw1, tw2)
     struct TW	*tw1;
     struct TW	*tw2;
{
  struct RkWcand	wc1[2*RK_CAND_NMAX], wc2[RK_CAND_NMAX];
  Wrec			*wrec2 = tw2->word;
  Wrec			*wrec1 = tw1->word;
  int			nc1, nc2, nc;

  if (((*wrec1 >> 1) & 0x3f) != ((*wrec2 >> 1) & 0x3f))
    return (struct TW *)0;
  nc1 = RkScanWcand(wrec1, wc1, RkNumber(wc1));
  nc2 = RkScanWcand(wrec2, wc2, RkNumber(wc2));
  nc  = RkUnionWcand(wc1, nc1, RkNumber(wc1), wc2, nc2);
  if (RK_CAND_NMAX < nc)
    return (struct TW *)0;
  else
    return(RkWcand2Wrec(wrec1, wc1, nc, tw1->lucks));
}


