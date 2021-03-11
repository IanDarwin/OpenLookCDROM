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
static char rcsid[]="@(#)$Id: RKutil.c,v 1.35 1994/06/01 06:54:39 misao Exp $ $Author: misao $ $Revision: 1.35 $ $Data$";
#endif

#include "RKintern.h"
#include <stdio.h>

static char	*Hdrtag[] = {
  HD_TAG_MAG,
  HD_TAG_VER,
  HD_TAG_TIME,
  HD_TAG_REC,
  HD_TAG_CAN,
  HD_TAG_L2P,
  HD_TAG_L2C,
  HD_TAG_PAG,
  HD_TAG_LND,
  HD_TAG_SND,
  HD_TAG_SIZ,
  HD_TAG_HSZ,
  HD_TAG_DROF,
  HD_TAG_PGOF,
  HD_TAG_DMNM,
  HD_TAG_CODM,
  HD_TAG_LANG,
  HD_TAG_WWID,
  HD_TAG_WTYP,
  HD_TAG_COPY,
  HD_TAG_NOTE,
  HD_TAG_TYPE,
  0,
};

static char	essential_tag[] = {
    HD_TIME,
    HD_DMNM,
    HD_LANG,
    HD_WWID,
    HD_WTYP,
    HD_TYPE,
};

static	unsigned char	localbuffer[RK_MAX_HDRSIZ];

int
uslen(us)
     Wchar	*us;
{
  Wchar *ous = us;
  
  if (!us)
    return 0;
  while (*us & RK_WMASK)
    us++;
  return (us - ous);
}

void
usncopy(dst, src, len)
     Wchar	*dst, *src;
     int	len;
{
  while (len-- > 0 && (*dst++ = *src++)) /* EMPTY */;
}

unsigned char *
ustoeuc(src, srclen, dest, destlen)
     Wchar		*src;
     unsigned char	*dest;
     int		srclen, destlen;
{
    if (!src || !dest || !srclen || !destlen)
	return dest;
    while (*src && --srclen >= 0 && --destlen >= 0) {
	if (us_iscodeG0(*src)) {
	    *dest++ = (unsigned char)*src++;
	} else if (us_iscodeG2(*src)) {
	    *dest++ = RK_SS2;
	    *dest++ = (unsigned char)*src++;
	    destlen--;
	} else if (destlen > 2) {
	  if (us_iscodeG3(*src)) {
	    *dest++ = RK_SS3;
	  }
	  *dest++ = (unsigned char)(*src >> 8);
	  *dest++ = (unsigned char)(*src++ | 0x80);
	  destlen--;
	};
    };
    *dest = (unsigned char)0;
    return dest;
}

Wchar *
euctous(src, srclen, dest, destlen)
     unsigned char	*src;
     Wchar		*dest;
     int		srclen, destlen;
{
  Wchar	*a = dest;
    
  if (!src || !dest || !srclen || !destlen)
    return(a);
  while (*src && (srclen-- > 0) && (destlen-- > 0)) {
    if (!(*src & 0x80) ) {
      *dest++ = (Wchar)*src++;
    } else if (srclen-- > 0) {
      if (*src == RK_SS2) {
	src++;
	*dest++ = (Wchar)(0x0080 | (*src++ & 0x7f));
      } else if ((*src == RK_SS3) && (srclen-- > 0)) {
	src++;
	*dest++ = (Wchar)(0x8000 | ((src[0] & 0x7f) << 8) | (src[1] & (0x7f)));
	src += 2;
      } else {
	*dest++ = (Wchar)(0x8080 | ((src[0] & 0x7f) << 8) | (src[1] & 0x7f));
	src += 2;
      }
    } else {
      break;
    }
  }
  if (destlen-- > 0)
    *dest = (Wchar)0;
  return dest;
}

static FILE	*log = (FILE *)0;

void
_Rkpanic(fmt, p, q, r)
     char	*fmt;
/* VARARGS2 */
{
  char	msg[RK_LINE_BMAX];
  extern void exit();
  
  (void)sprintf(msg, fmt, p, q, r);
  (void)fprintf(log ? log : stderr, "%s\n", msg);
  (void)fflush(log);
  exit(1);
}

int
_RkCalcUnlog2(x)
     int	x;
{
  return((1 << x) - 1);
}

int 
_RkCalcLog2(n)
     int n;
{
  int	lg2;
  
  n--;
  for (lg2 = 0; n > 0; lg2++)
    n >>= 1;
  return(lg2);
}

Wchar
uniqAlnum(c)
     Wchar c;
{
  return((0xa3a0 < c && c < 0xa3ff) ? (Wchar)(c & 0x7f) : c);
}

void
_RkClearHeader(hd)
     struct HD	*hd;
{
  int	i;
    
  if (hd) {
    for (i = 0; i < HD_MAXTAG; i++) {
      if (hd->flag[i] > 0) {
	(void)free(hd->data[i].ptr);
      }
    }
  }
}

int
_RkReadHeader(fd, hd, off_from_top)
     int	fd;
     struct HD	*hd;
     unsigned	off_from_top;
{
  unsigned char	*src;
  unsigned long	len, off, hdrsize;
  int		i;

  for (i = 0; i < HD_MAXTAG; i++) {
    hd->data[i].var = 0;
    hd->flag[i] = 0;
  }
  /* 次の off_from_top の計算がうさんくさいぞ */
  if (lseek(fd, off_from_top, 0) < 0) {
    RkSetErrno(RK_ERRNO_EACCES);
    goto read_err;
  }
  if ((hdrsize = read(fd, (char *)localbuffer, RK_MAX_HDRSIZ)) <= 0) {
    RkSetErrno(RK_ERRNO_EACCES);
    goto read_err;
  }
  for (src = localbuffer; src < (localbuffer + hdrsize);) {
    if (isEndTag(src))
      break;
    for (i = 0; i < HD_MAXTAG; i++) {
      if (!strncmp((char *)src, Hdrtag[i],  HD_TAGSIZ))
	break;
    }
    if (i == HD_MAXTAG)
      goto read_err;
    src += HD_TAGSIZ;
    len = bst4_to_l(src);
    src += HD_TAGSIZ;
    off = bst4_to_l(src);
    src += HD_TAGSIZ;
    if (hd->flag[i] != 0)
      goto read_err;
    if (len == 0) {
      hd->flag[i] = -1;
      hd->data[i].var = off;
    } else {
      hd->flag[i] = len;
      if (!(hd->data[i].ptr = (unsigned char *)malloc(len + 1))) {
	RkSetErrno(RK_ERRNO_NOMEM);
	goto read_err;
      }
      if (off < hdrsize) {
	(void)memcpy(hd->data[i].ptr, localbuffer + off, len);
      } else {
	if (lseek(fd, (long)(off_from_top + off), 0) < 0) {
	  RkSetErrno(RK_ERRNO_EACCES);
	  goto read_err;
	}
	if (read(fd, (char *)hd->data[i].ptr, (unsigned)len) != len) {
	  RkSetErrno(RK_ERRNO_EACCES);
	  goto read_err;
	}
      }
      hd->data[i].ptr[len] = 0;
    }
  }
  return 0;
 read_err:
  for (i = 0; i < HD_MAXTAG; i++) {
    if (hd->flag[i] > 0)
      (void)free(hd->data[i].ptr);
    hd->flag[i] = 0;
    hd->data[i].var = 0;
  }
  return -1;
}

unsigned char *
_RkCreateHeader(hd, size)
     struct HD	*hd;
     unsigned	*size;
{
  unsigned char	*tagdst, *datadst, *ptr;
  int		i, j;
  unsigned long	len, off;

  if (!hd)
    return 0;
  tagdst = localbuffer;
  datadst = localbuffer + HD_MAXTAG * HD_MIN_TAGSIZ;
  datadst += sizeof(long);
  for (i = 0; i < HD_MAXTAG; i++) {
    for (j = 0; j < sizeof(essential_tag); j++) {
      if (essential_tag[j] == i) {
	break;
      }
    }
    (void)memcpy(tagdst, Hdrtag[i], HD_TAGSIZ);
    tagdst += HD_TAGSIZ;
    if (hd->flag[i] == -1) {
      len = 0;
      off = hd->data[i].var;
    } else if (hd->flag[i] > 0) {
      len = hd->flag[i];
      off = datadst - localbuffer;
      (void)memcpy(datadst, hd->data[i].ptr, len);
      datadst += len;
    } else {
      len = 0;
      off = 0;
    }
    l_to_bst4(len, tagdst); tagdst += HD_TAGSIZ;
    l_to_bst4(off, tagdst); tagdst += HD_TAGSIZ;
  }
  *tagdst++ = 0; *tagdst++ = 0; *tagdst++ = 0; *tagdst++ = 0;
  *size = datadst - localbuffer;
  if (!(ptr = (unsigned char *)malloc(*size))) {
    return 0;
  }
  (void)memcpy(ptr, localbuffer, *size);
  return ptr;
}

unsigned
_RkGetTick(mode)
     int	mode;
{
  static unsigned time = 10000;
  return(mode ? time++ : time);
}

int
set_hdr_var(hd, n, var)
     struct HD		*hd;
     int		n;
     unsigned long	var;
{
    if (!hd)
	return -1;
    hd->data[n].var = var;
    hd->flag[n] = -1;
    return 0;
}

_RkGetLink(dic, pgno, off, lvo, csn)
     struct ND	*dic;
     int	pgno;
     unsigned	off;
     unsigned	*lvo;
     unsigned	*csn;
{
  struct NP	*pg = dic->pgs + pgno;
  unsigned char	*p;
  unsigned	i;

  for (i = 0, p = pg->buf + 14 + 4 * pg->ndsz; i < pg->lnksz; i++, p += 5) {
    if (thisPWO(p) == off) {
      *lvo = pg->lvo + thisLVO(p);
      *csn = pg->csn + thisCSN(p);
      return(0);
    }
  }
  return(-1);
}

unsigned
_RkGetOffset(dic, pos)
     struct ND		*dic;
     unsigned char	*pos;
{
  struct NP	*pg;
  unsigned char	*p;
  unsigned	i;
  unsigned	lvo;
#if 0 /* csn is not used */
  unsigned	csn;
#endif

  for (i = 0; i < dic->ttlpg; i++) {
    if (dic->pgs[i].buf) {
      if (dic->pgs[i].buf < pos && pos < dic->pgs[i].buf + dic->pgsz)
	break;
    }
  }
  if (i == dic->ttlpg) {
    return(0);
  }
  pg = dic->pgs + i;
  for (i = 0, p = pg->buf + 14 + 4 * pg->ndsz; i < pg->lnksz; i++, p += 5) {
    if (pos - pg->buf == thisPWO(p)) {
      lvo = pg->lvo + thisLVO(p);
#if 0 /* csn is not used */
      csn = pg->csn + thisCSN(p);
#endif
      return(lvo);
    }
  }
  _Rkpanic("Cannot get Offset", 0, 0, 0);
}

int
HowManyChars(yomi, len)
     Wchar	*yomi;
     int	len;
{
  int chlen, bytelen;

  for (chlen = 0, bytelen = 0; bytelen < len; chlen++) {
    Wchar ch = yomi[chlen];
    
    if (us_iscodeG0(ch))
      bytelen++;
    else if (us_iscodeG3(ch))
      bytelen += 3;
    else
      bytelen += 2;
  }
  return(chlen);
}

int
HowManyBytes(yomi, len)
     Wchar	*yomi;
     int	len;
{
  int chlen, bytelen;

  for (chlen = 0, bytelen = 0; chlen < len; chlen++) {
    Wchar ch = yomi[chlen];

    if (us_iscodeG0(ch))
      bytelen++;
    else if (us_iscodeG3(ch))
      bytelen += 3;
    else {
      bytelen += 2;
    }
  }
  return(bytelen);
}

#ifdef TEST

printWord(w)
struct nword *w;
{
  printf("[0x%x] Y=%d, K=%d, class=0x%x, flg=0x%x, lit=%d, prio=%d, kanji=",
	 w, w->nw_ylen, w->nw_klen, w->nw_class, w->nw_flags,
	 w->nw_lit, w->nw_prio);
  if (w->nw_kanji) {
    int i, klen = w->nw_left ? w->nw_klen - w->nw_left->nw_klen : w->nw_klen;
    char *p = w->nw_kanji + 2;

    for (i = 0 ; i < klen ; i++) {
      printf("%c%c", p[0], p[1]);
      p += 2;
    }
  }
  printf("\n");
}

showWord(w)
struct nword *w;
{
  struct nword *p, *q;

  printf("next:\n");
  for (p = w ; p ; p = p->nw_next) {
    printWord(p);
    for (q = p->nw_left ; q ; q = q->nw_left) {
      printWord(q);
    }
    printf("\n");
  }
}

#endif /* TEST */
