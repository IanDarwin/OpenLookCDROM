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
static char rcs_id[] = "$Id: RKpermdic.c,v 2.17 1994/06/01 06:54:35 misao Exp $";
#endif

#include	"RKintern.h"

#ifdef SVR4
#include	<unistd.h>
#endif
#if defined(USG) || defined(SYSV) || defined(SVR4)
#include	<string.h>
#else
#include	<strings.h>
#endif

#define dm_xdm	dm_extdata.ptr
#define df_fdes	df_extdata.var

extern	unsigned	_RkCalcLVO();
extern	Wchar		uniqAlnum();


static int
openDF(df, dfnm, w)
     struct DF	*df;
     char	*dfnm;
     int        *w;
{
  int		fd;
  struct HD	hd;
  struct ND	nd, *xnd;
  struct DM	*dm, *dmh;
  unsigned	off;
  unsigned char	ll[4];
  int		count = 0, err;
    
  *w = 0;
  if ((fd = open(dfnm, 0)) == -1) 
    return -1;

  for (off = 0, err = 0; !err && _RkReadHeader(fd, &hd, off) >= 0;) {

    if (hd.flag[HD_CODM] > 0) {
      _RkClearHeader(&hd);
      break;
    }
    nd.time = hd.data[HD_TIME].var;
    nd.rec = hd.data[HD_REC].var;
    nd.can = hd.data[HD_CAN].var;
    nd.doff = off + hd.data[HD_HSZ].var;
    nd.sz = hd.data[HD_SIZ].var;
    nd.drsz = hd.data[HD_PGOF].var - hd.data[HD_DROF].var;
    nd.pgsz = _RkCalcUnlog2(hd.data[HD_L2P].var) + 1;
    nd.ttlpg = hd.data[HD_PAG].var;
    nd.fd = fd;
    nd.buf = (unsigned char *)0;
    nd.pgs = (struct NP *)0;
    off += hd.data[HD_SIZ].var;
    if (!strncmp(".swd",
		 (char *)(hd.data[HD_DMNM].ptr
			  + strlen((char *)hd.data[HD_DMNM].ptr) - 4),
		 4)) {
      if (lseek(fd, off, 0) < 0 || read(fd, (char *)ll, 4) != 4)
	err++;
      off += bst4_to_l(ll) + 4;
    }
    dmh = &df->df_members;
    for (dm = dmh->dm_next; dm != dmh; dm = dm->dm_next) {
      if (!strcmp((char *)dm->dm_dicname, (char *)hd.data[HD_DMNM].ptr)) {
	if (!dm->dm_xdm) {
	  if (!(xnd = (struct ND *)malloc(sizeof(struct ND))))
	    break;
	  dm->dm_xdm = (pointer)xnd;
	  *xnd = nd;
	  dm->dm_flags |= DM_EXIST;
	  dm->dm_offset = xnd->doff;
	  count++;
	  break;
	}
      }
    }
    _RkClearHeader(&hd);
  }
  _RkClearHeader(&hd);
  df->df_size = off;
  if (!count) {
    (void)close(fd);
    return -1;
  }
  return (df->df_fdes = (int)fd);
}

int
_Rkpopen(dm, dfnm, mode, gram)
     struct DM	*dm;
     char	*dfnm;
     int	mode;
     struct RkKxGram *gram; /* ARGSUSED */
{
  struct DF	*df;
  struct DD	*dd;
  struct ND	*xdm;
  int 		writable, i, fd;
  
  if (!(df = dm->dm_file) || !(dd = df->df_direct))
    return -1;
  if (!df->df_rcount) {
    if ((df->df_fdes = (long)openDF(df, dfnm, &writable)) < 0)
      return -1;
    if (writable)
      df->df_flags |= DF_WRITABLE;
    else
      df->df_flags &= ~DF_WRITABLE;
    df->df_flags |= DF_EXIST;
    dd->dd_rcount++;
  }
  if (!(dm->dm_flags & DM_EXIST))
    return -1;
  df->df_rcount++;
  xdm = (struct ND *)dm->dm_xdm;
  fd = df->df_fdes;
  
  if (!(xdm->buf = (unsigned char *)malloc(xdm->drsz))) {
    return(-1);
  }
  if (!(xdm->pgs = (struct NP *)malloc(sizeof(struct NP) * xdm->ttlpg))) {
    (void)free((char *)xdm->buf);
    xdm->buf = (unsigned char *)0;
    return(-1);
  }
  for (i = 0; i < xdm->ttlpg; i++) {
    xdm->pgs[i].lnksz = (unsigned) 0;
    xdm->pgs[i].ndsz = (unsigned) 0;
    xdm->pgs[i].lvo = (unsigned) 0;
    xdm->pgs[i].csn = (unsigned) 0;
    xdm->pgs[i].flags = (unsigned) 0;
    xdm->pgs[i].count = 0;
    xdm->pgs[i].buf = (unsigned char *) 0;
  }
  (void)lseek(fd, xdm->doff, 0);
  if (read(fd, (char *)xdm->buf, xdm->drsz) != xdm->drsz) {
    (void)free((char *)xdm->pgs);
    (void)free((char *)xdm->buf);
    xdm->buf = (unsigned char *)0;
    xdm->pgs = (struct NP *)0;
    return(-1);
  }
  if (dm->dm_class == ND_SWD) {
    struct RkKxGram *gram;

    lseek(fd, xdm->doff + xdm->drsz + xdm->ttlpg * xdm->pgsz, 0);
    gram = RkReadGram(fd);
    if (gram) {
      dm->dm_gram = (struct RkGram *)malloc(sizeof(struct RkGram));
      if (dm->dm_gram) {
	dm->dm_gram->gramdic = gram;
	dm->dm_gram->P_BB  = RkGetGramNum(gram, "BB");
	dm->dm_gram->P_NN  = RkGetGramNum(gram, "NN");
	dm->dm_gram->P_T00 = RkGetGramNum(gram, "T00");
	dm->dm_gram->P_T30 = RkGetGramNum(gram, "T30");
	dm->dm_gram->P_T35 = RkGetGramNum(gram, "T35");
	dm->dm_gram->refcount = 1;
	goto next;
      }
      RkCloseGram(gram);
    }
  }

 next:
  if ((mode & DM_WRITABLE) && (df->df_flags & DF_WRITABLE)) {
    dm->dm_flags |= DM_WRITABLE;
  }
  return 0;
}

int	
_Rkpclose(dm, dfnm, gram)
     struct DM	*dm;
     char		*dfnm;
     struct RkKxGram *gram; /* ARGSUSED */
{
  struct DF	*df = dm->dm_file;
  struct ND	*xdm = (struct ND *)dm->dm_xdm;
  int		i;

  _RkKillCache(dm);
  if (dm->dm_gram) {
    dm->dm_gram->refcount--;
    if (dm->dm_gram->refcount == 0) {
      (void)RkCloseGram(dm->dm_gram->gramdic);
      free((char *)dm->dm_gram);
    }
  }
  if (xdm) {
    if (xdm->pgs) {
      for (i = 0; i < xdm->ttlpg; i++)
	if (xdm->pgs[i].flags & RK_PG_LOADED) {
	  if (xdm->pgs[i].buf) {
	    (void)free((char *)xdm->pgs[i].buf);
	  }
	  xdm->pgs[i].flags &= ~RK_PG_LOADED;
	}
      (void)free((char *)xdm->pgs);
      xdm->pgs = (struct NP *)0;
    }
    if (xdm->buf) {
      (void)free((char *) xdm->buf);
      xdm->buf = (unsigned char *)0;
    }
  }

  if (--df->df_rcount == 0)  {
    int		fd = (int)df->df_fdes;
    struct DM	*dmh, *ddm;
    
    (void)close(fd);
    dmh = &df->df_members;
    for (ddm = dmh->dm_next; ddm != dmh; ddm = ddm->dm_next) {
      xdm = (struct ND *)ddm->dm_xdm;
      if (xdm) {
	(void)free((char *)xdm);
	ddm->dm_xdm = (pointer)0;
      }
    }
  }
}

static
unsigned char *
assurep(dic, id)
     struct ND	*dic;
     int	id;
{
  unsigned	off = dic->doff + dic->drsz + dic->pgsz * id;
  unsigned	size = dic->pgsz;
  unsigned char	*buf;
  int		fd = dic->fd;

  if (!dic->pgs)
    return((unsigned char *)0);
  if (id >= dic->ttlpg)
    return((unsigned char *)0);
  if (!isLoadedPage(dic->pgs + id)) {
    if (!(buf = (unsigned char *)malloc(size)))
      return((unsigned char *)0);
    (void)lseek(fd, off, 0);
    if (read(fd, buf, size) != size) {
      free((char *)buf);
      return((unsigned char *)0);
    }
    dic->pgs[id].buf = buf;
    dic->pgs[id].count = 0;
    dic->pgs[id].flags |= RK_PG_LOADED;
    dic->pgs[id].ndsz = bst2_to_s(buf + 2);
    dic->pgs[id].lnksz = bst2_to_s(buf + 4);
    dic->pgs[id].lvo = bst3_to_l(buf + 7);
    dic->pgs[id].csn = bst3_to_l(buf + 10);
    return(buf);
  } else  {
    return(dic->pgs[id].buf);
  }
}

int
_RkEql(a, b, n)
     Wchar		*a;
     unsigned char	*b;
     int		n;
{
  Wchar	c, d;
  for (; n-- > 0; b += 2) {
    c = uniqAlnum(*a++);
    d = (*b << 8) | *(b+1);
    if (c != d)
      return(0);
  }
  return(1);
}

static
readThisCache(dm, xdm, pgno, val, key, cur, ylen, nread, mc, nc, cf)
     struct DM		*dm;
     struct ND		*xdm;
     int		pgno;
     unsigned		val;
     Wchar		*key;
     int		cur;
     int		ylen;
     struct nread	*nread;
     int		mc;
     int		nc;
     int		*cf;
{
  int		remlen;
  unsigned char	*wrec1, *wrec;

  if (xdm->pgs[pgno].buf) {
    if (*(wrec1 = wrec = xdm->pgs[pgno].buf + val) & 0x80)
      wrec1 += 2;
    wrec1 += 2;
    remlen = (*wrec >> 1) & 0x3f;
    if (_RkEql(key + cur, (unsigned char *)wrec1, remlen)) {
      if (remlen + cur > ylen)
	(*cf)++;
      else if (nc < mc) {
	nread[nc].cache = _RkReadCache(dm, (long)wrec);
	if (nread[nc].cache) {
	  if (_RkGetLink(xdm, pgno, val, &nread[nc].offset, &nread[nc].csn) < 0) {
	    _RkDerefCache(nread[nc].cache);
	    return(0);
	  }
	  nread[nc].nk = cur + remlen;
	  nc++;
	} else
	  (*cf)++;
      } else
	(*cf)++;
    }
  }
  return(nc);
}

static int
SearchInPage(dm, xdm, pgno, buf, val, key, cur, ylen, nread, mc, nc, cf)
     struct DM		*dm;
     struct ND		*xdm;
     unsigned char	*buf;
     int		pgno;
     unsigned		val;
     Wchar		*key;
     int		cur;
     int		ylen;
     struct nread	*nread;
     int		mc;
     int		nc;
     int		*cf;
{
  Wchar		kv, wc;
  unsigned char	*pos = buf + val;

  if (!*pos && !*(pos + 1)) {
    val = ((*(pos + 2) & 0x3f) << BIT_UNIT) | *(pos + 3);
    nc = readThisCache(dm, xdm, pgno, val, key,
		       cur, ylen, nread, mc, nc, cf);
    if (*(pos + 2) & LAST_NODE)
      return(nc);
    pos += 4;
  }
  if (cur == ylen) {
    (*cf)++;
    return(nc);
  }
  kv = uniqAlnum(*(key + cur));
  for (wc = bst2_to_s(pos); wc != kv; pos += 4, wc = bst2_to_s(pos)) {
    if (*(pos + 2) & LAST_NODE)
      return(nc);
  }
  val = ((*(pos + 2) & 0x3f) << BIT_UNIT) | *(pos + 3);
    cur++;
  if (*(pos + 2) & WORD_NODE)
    nc = readThisCache(dm, xdm, pgno, val, key,
		       cur, ylen, nread, mc, nc, cf);
  else
    nc = SearchInPage(dm, xdm, pgno, buf, val, key,
		      cur, ylen, nread, mc, nc, cf);
  return(nc);
}

static int
SearchInDir(dm, xdm, pos, key, cur, ylen, nread, mc, nc, cf)
     struct DM		*dm;
     struct ND		*xdm;
     unsigned char	*pos;
     Wchar		*key;
     int		cur;
     int		ylen;
     struct nread	*nread;
     int		mc;
     int		nc;
     int		*cf;
{
  Wchar		kv, wc, nw;
  unsigned	val;
  int		next, pgno, iw;
  unsigned char	*p;

  nw = bst2_to_s(pos);
  pos += 5;
  if (!*pos && !*(pos + 1)) {
    val = bst3_to_l(pos + 2);
    if (val & ~VMASK) {
      val &= VMASK;
      pgno = (val - xdm->drsz) / xdm->pgsz;
      val -= pgno * xdm->pgsz + xdm->drsz;
      if (assurep(xdm, pgno))
	nc = readThisCache(dm, xdm, pgno, val, key,
			   cur, ylen, nread, mc, nc, cf);
    }
  }
  if (cur == ylen) {
    (*cf)++;
    return(nc);
  }
  kv = uniqAlnum(*(key + cur));
  next = (int)(kv % nw);
  do {
    p = pos + (((Wchar) next++) % nw) * 5;
    if ((wc = bst2_to_s(p)) == 0xffff)
      return(nc);
  } while (wc != kv);
  val = bst3_to_l(p + 2);
  cur++;
  iw = (val & ~VMASK);
  val &= VMASK;
  if (iw) {
    pgno = (val - xdm->drsz) / xdm->pgsz;
    val -= pgno * xdm->pgsz + xdm->drsz;
    if (assurep(xdm, pgno))
      nc = readThisCache(dm, xdm, pgno, val, key,
			 cur, ylen, nread, mc, nc, cf);
  } else {
    if (val < xdm->drsz)
      nc = SearchInDir(dm, xdm, xdm->buf + val, key,
		       cur, ylen, nread, mc, nc, cf);
    else {
      pgno = (val - xdm->drsz) / xdm->pgsz;
      val -= pgno * xdm->pgsz + xdm->drsz;
      p = assurep(xdm, pgno);
      if (p)
	nc = SearchInPage(dm, xdm, pgno, p, val, key,
			  cur, ylen, nread, mc, nc, cf);
    }
  }
  return(nc);
}

int		
_Rkpsearch(cx, dm, key, n, nread, mc, cf)
     struct RkContext	*cx;
     struct DM		*dm;
     Wchar		*key;
     int		n;
     struct nread	*nread;
     int		mc;
     int		*cf;
/* ARGSUSED */
{
  struct ND	*xdm;

  *cf = 0;
  xdm = (struct ND *)dm->dm_xdm;
  if (xdm) {
    if (xdm->buf)
      return(SearchInDir(dm, xdm, xdm->buf, key, 0, n, nread, mc, 0, cf));
  }
  return(0);
}

int	
_Rkpio(dm, cp, io)
     struct DM		*dm;
     struct ncache	*cp;
     int		io;
/* ARGSUSED */
{
  if (io == 0) {
    cp->nc_word = (Wrec *)cp->nc_address;
    cp->nc_flags |= NC_NHEAP;
  }
  return 0;
}

#if 0 /* 使われていないのでとりあえずコメントにする */
static void
ch_perm(qm, offset, size, num)
     struct DM  *qm;
     unsigned   offset;
     int        size, num;
{
  unsigned char	tmp[8192];
  
  if (num > 0) {
    _RkCopyBits(tmp, 0, size, qm->dm_qbits, offset, num);
    _RkCopyBits(qm->dm_qbits, offset + 0, size,
		qm->dm_qbits, offset + num*size, 1);
    _RkCopyBits(qm->dm_qbits, offset + size, size, tmp, 0, num);
  }
}
#endif

int	
_Rkpctl(dm, qm, what, arg, gram)
     struct DM	*dm;
     struct DM	*qm;
     Wchar	*arg;
     int	what;
     struct RkKxGram *gram;
{
  Wrec		wrec[2048];
  Wchar         key[64];
  int		nc, cf = 0;
  struct ND	*xdm;
  unsigned	lucks[2];
  

  if (!dm  || !qm || (qm && !qm->dm_qbits))
    return (-1);

  if ((qm->dm_flags & (DM_WRITABLE | DM_WRITEOK)) ==
      (DM_WRITABLE | DM_WRITEOK)) {
    /* (writable and write ok) */
  
    if (RkParseOWrec(gram, arg, wrec, RkNumber(wrec), lucks)) {
      Wrec	    *p, *q, *kanji;
      Wchar         *wkey;
      struct nread  nread[128];
      int	    maxcache = RkNumber(nread);
      int           ylen, klen, cnum, y_off = 2, k_off;
      

      ylen = (wrec[0] >> 1) & 0x3f;
      if (wrec[0] & 0x80)
	y_off += 2;
      p = wrec + y_off;
      q = p + (ylen * 2);
      for (wkey = key; p < q ; wkey++) {
	*wkey = (*p << 8) | *(p + 1);
	p += 2;
      }
      *(key+ylen) = 0;
      
      /* 品詞、漢字情報の取り出し */
      k_off = y_off + ylen * 2;
      klen = (wrec[k_off] >> 1) & 0x7f;
      cnum = ((wrec[k_off] & 0x01) << 8) | wrec[k_off+1];
      kanji = wrec + k_off + 2;
      
      nc = -1;
      xdm = (struct ND *)dm->dm_xdm;
      if (xdm) {
	if (xdm->buf)
	  nc = SearchInDir(dm, xdm, xdm->buf, key, 0, ylen, nread, 
			   maxcache, 0, &cf);
      } 
      
      if (nc > 0) {
	struct nread	*thisRead;
	struct ncache	*thisCache;
	unsigned char	*wp;
	int             nk, nl, pre;
	unsigned	offset;
	unsigned	permutation[RK_CAND_NMAX];
	int		bitSize, fnum = -1, nnum, i;
	
	for (i = 0 ; i < nc ; i++) {
	  if (nread[i].nk == ylen) {
	    break;
	  }
	} 
	/* 使わない単語候補はあらかじめ _RkDerefCache する */
	for (pre = 0 ; pre < nc ; pre++) {
	  if (pre != i) {
	    thisRead = nread + pre;
	    thisCache = thisRead->cache;
	    _RkDerefCache(thisCache);
	  }
	} 

	if (i < nc) {
	  thisRead = nread + i;
	  thisCache = thisRead->cache;
	  wp = thisCache->nc_word;
	
	  nk = _RkCandNumber(wp);
	  nl = (*wp >> 1) & 0x3f;
	  if (*wp & 0x80)
	    wp += 2;
	  wp += 2 + nl *2;
	
	/* ここの部分で辞書の何番目にでてくるか (fnum) を探す */
	  for (i = 0; i < nk; i++) {
	    unsigned char	*kp;
	  
	    nl = (*wp >> 1) & 0x7f;               /* 候補長 */
	    nnum = ((*wp & 0x01) << 8) | *(wp+1); /* 品詞番号 */
	    if (nl == klen && nnum == cnum) {
	      int lc;
	      
	      for (lc = 0, kp = wp + 2; lc < klen*2; lc++) {
		if (*(kanji+lc) != *(kp+lc))
		  break;
	      }
	      if (lc == klen*2) {
		fnum = i;
		break;
	      }
	    }
	    wp += 2 + nl*2;
	  }
	
	  offset = thisRead->offset;
	  if (fnum >= 0 && fnum < nk && 0 < thisRead->nk && 
	      thisRead->nk <= ylen && thisRead->nk <= RK_KEY_WMAX)  {
	    int	   ecount, cval, i, dn = -1, ndel = 0;
	  
	    bitSize = _RkCalcLog2(nk + 1) + 1;
	    _RkUnpackBits(permutation, qm->dm_qbits, offset, bitSize, nk);
	    switch (what) {
	    case DST_DoDefine:
	      for (ecount = cval = i = 0; i < nk; i++) {
		if (permutation[i]/2 >  nk) {
		  ecount++;
		  break;
		};
		cval += permutation[i];
		if (nk == permutation[i]/2 && dn < 0)
		  dn = i;
		if (fnum == permutation[i]/2) {
		  ndel = -1;
		  dn = i;
		}
	      }
	      break;
	    case DST_DoDelete:
	      for (ecount = cval = i = 0; i < nk; i++) {
		if (permutation[i]/2 >  nk) {
		  ecount++;
		  break;
		};
		cval += permutation[i];
		if (fnum == permutation[i]/2)
		  dn = i;
	      };
	      break;
	    }	  
	    if (ecount || cval < (nk-1)*(nk-2)) {
	      for (i = 0; i < nk; i++)
		permutation[i] = 2*i;
	      _RkPackBits(qm->dm_qbits, offset, bitSize, permutation, nk);
	    } else {
	      if (dn >= 0) {
		if (!ndel) {
		  switch (what) {
		  case DST_DoDefine:
		    _RkSetBitNum(qm->dm_qbits, offset, bitSize, dn, fnum*2);
/*		  ここは並び順を変更する関数だがとりあえずコメントにする。
                 ch_perm(qm, offset, bitSize, dn);
*/
		    break;
		  case DST_DoDelete:
		    _RkSetBitNum(qm->dm_qbits, offset, bitSize, dn, nk*2);
		    break;
		  }
		  qm->dm_flags |= DM_UPDATED;
		}
		_RkDerefCache(thisCache);
		return (0);
	      }
	    }
	  }
	  _RkDerefCache(thisCache);
	}
      }
    }
  }
  return (-1);
}  

int	
_Rkpsync(cx, dm, qm)
     struct RkContext *cx;
     struct DM	*dm, *qm;
{
  struct DF	*df;
  struct DD     *dd;
  char	*file;
  
  if (qm) {
    df = qm->dm_file;
    dd = df->df_direct;
    file = _RkCreatePath(dd, df->df_link);
    if (file) {
      int i;
      i = FQsync(cx, dm, qm, file);
      (void)free(file);
      return (i);
    }
  }
  return (0);
}
