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
static char rcsid[]="$Id: RKnword.c,v 2.14 1994/06/01 06:54:32 misao Exp $";
#endif

/* LINTLIBRARY */
#include	"RKintern.h"
#include	"RK.h"

#if defined(DEBUG_NWORD) || defined(RK_DEBUG)
/* 
 * debug aids
 */
#define		D_CONC	1
#define		D_PARSE	2
#define		D_SUCC	4
#define		D_SUBST	4
/*
int		debug_flags = D_CONC|D_PARSE|D_SUCC;
int		debug_flags = D_PARSE|D_SUCC;
*/
int		debug_flags = 0;
static void	dumpSimpleWordRec(), dumpWordRec(), dumpAllBunq();
static void	dumpXQH(), dumpXQ();

#else
#define rk_debug(file, fmt, a, b, c)
#endif

extern	void	usncopy();

static void
clearWord(w, bb)			/* make word empty */
     struct nword	*w;
     int bb;
{
  if (w) {
    w->nw_cache = (struct ncache *)0;
    w->nw_rowcol = bb; /* 文節 */
    w->nw_klen = w->nw_ylen = 0;
    w->nw_class = ND_EMP;
    w->nw_flags = 0;
    w->nw_lit = 0;
    w->nw_prio = 0;
    w->nw_left = w->nw_next = (struct nword *)0;
    w->nw_kanji = (Wrec *)0;
  }
}

/*ARGSUSED*/
static void
setWord(w, rc, lit, yomi, ylen, kanji, klen, bb)
     struct nword	*w;
     int		rc;
     int		lit;
     Wchar		*yomi;
     int		ylen;
     Wrec		*kanji;
     int		klen;
     int		bb;
{
  clearWord(w, bb);
  w->nw_rowcol = rc;
  w->nw_klen = klen;
  w->nw_ylen = ylen;
  w->nw_class = 0;
  w->nw_flags = 0;
  w->nw_lit = lit;
  w->nw_kanji = kanji;
}

/* allocWord
 *	allocate a fresh word
 */
/*ARGSUSED*/
static
struct nword *
allocWord(st, bb)
     struct nstore *st;
     int bb;
{
  struct nword 	*new_word;
   
  if (!SX.word) {
    struct nword	*new_page;
    int			i;
#define	NW_PAGESIZE	1024
    new_page = (struct nword *)malloc(sizeof(struct nword)*NW_PAGESIZE);
    if (new_page) {
      SX.page_in_use++;
      new_page[0].nw_next = SX.page;
      SX.page = &new_page[0];
      SX.word = &new_page[1];
      for (i = 1; i + 1 < NW_PAGESIZE; i++)
	new_page[i].nw_next = &new_page[i + 1];
      new_page[i].nw_next = (struct nword *)0;
    };
  };
  new_word = SX.word;
  if (new_word) {
    SX.word = new_word->nw_next;
    clearWord(new_word, bb);
    st->word_in_use++; 
    SX.word_in_use++; 
  };
  return new_word;
}

static void
derefWord(word)			/* decrease the reference counter */
     struct nword 	*word;
{
  for (; word; word = word->nw_next) 
    if (word->nw_cache)
      (void)_RkDerefCache(word->nw_cache);
}

/*ARGSUSED*/
static void	
killWord(st, word)			/* dispose the unsed words */
struct nstore	*st;
struct nword	*word;
{
  struct nword *p, *q;

  if (word) {
    for (p = q = word; p; q = p, p = p->nw_next) {
      if (!p->nw_cache && p->nw_kanji) {
        _Rkpanic("killWord this would never happen addr ", 0, 0, 0);
	(void)free((char *)p->nw_kanji);
      };
     st->word_in_use--;
     SX.word_in_use--;
    }
    q->nw_next = SX.word;
    SX.word = word;
  }
}

static void	
freeWord(st, word)			/* freeWord = derefWord + killWord */
struct nstore 	*st;
struct nword 	*word;
{
  derefWord(word);
  killWord(st, word);
}

void	
_RkFreeBunq(st)			/* freeWord = derefWord + killWord */
     struct nstore *st;
{
  struct nbun *bunq = &st->bunq[st->curbun];
  
  freeWord(st, bunq->nb_cand);
  bunq->nb_cand = (struct nword *)0;
  bunq->nb_yoff = 0;
  bunq->nb_curlen = bunq->nb_maxcand = bunq->nb_curcand = 0;
  bunq->nb_flags = (unsigned char)0;
  return;
}

extern unsigned	searchRut();
extern int	entryRut();

static
struct nword	*
concWord(st, p, q, loc, bb) 		/* create the concatinated word p+q */
  struct nstore		*st;
  struct nword		*p, *q;	/* prefix word list, and right word */
  int			loc;
  int			bb;
{
    struct nword	conc;
    struct nword	*pq;
    struct nword	*t;
    int			count;

/* check limit conditions */
    count = 0;
    for (t = p; t; t = t->nw_left)
	count++;
    if (((unsigned long)(p->nw_klen + q->nw_klen) > RK_LEN_WMAX) ||
         ((unsigned long)(p->nw_ylen + q->nw_ylen) > RK_LEN_WMAX) ||
	 (count >= RK_CONC_NMAX))
	return (struct nword *)0;
/* create a concatinated word temoprally */
    conc = *q;
    conc.nw_klen  += p->nw_klen;
    conc.nw_ylen  += p->nw_ylen;
    conc.nw_flags = p->nw_flags&(NW_PRE|NW_SUC|NW_SWD);
    conc.nw_prio = p->nw_prio;
    conc.nw_next = (struct nword *)0;
    conc.nw_left = p;
    switch(q->nw_class)  {
/* kakko, kutouten ha setuzoku kankei ni eikyou sinai */
    case	ND_OPN:
    case	ND_CLS:
	conc.nw_rowcol = p->nw_rowcol;
	if (p->nw_class != ND_EMP) {
	    conc.nw_class = p->nw_class;
	    conc.nw_flags = p->nw_flags;
	} else  {
	    conc.nw_class = q->nw_class;
	    conc.nw_flags = q->nw_flags;
	};
	break;
    case	ND_PUN:
    /* avoid punctionations where prohibited */
#ifdef BUNMATU
        if (!CanSplitWord(p) && !CanBunmatu(p))
#else
        if (!CanSplitWord(p))
#endif
	    return (struct nword *)0;
    /* don't remove loc check or you get stuck when a punctionation comes */
        if (loc > 0 && p->nw_class == ND_EMP)
	    return (struct nword *)0;
	conc.nw_rowcol = p->nw_rowcol;
	conc.nw_class = ND_SWD;
	break;
    case	ND_MWD:
	conc.nw_flags |= NW_MWD;
	conc.nw_prio = q->nw_prio;
	break;
    case	ND_SWD:
	if (!(conc.nw_flags&NW_SWD)) 
	    conc.nw_flags |= NW_SWD;
	break;
    case	ND_PRE:
	conc.nw_flags |= NW_PRE;
	break;
    case	ND_SUC:
	conc.nw_flags |= NW_SUC;
	break;
    };
/* cache no sanshoudo wo kousinn suru */
    pq = allocWord(st, bb);
    if (pq) {
	*pq = conc;
        p->nw_flags |= NW_FOLLOW;
        if (pq->nw_cache)
	  _RkEnrefCache(pq->nw_cache);
    };
    return pq;
}

/* clearQue
 *	clear word tree queue 
 */
static void 
clearQue(xq)
struct nqueue	*xq;
{
  xq->tree = (struct nword *)0;
  xq->maxlen = 0;
  xq->status = 0;
}
/* RkFreeQue
 *	free word tree stored in [s, e)
 */
void
_RkFreeQue(st, s, e)
struct nstore *st;
int 	      s;
int	      e;
{
  struct nqueue *xq = st->xq;

  while (s < e) {
    if (xq[s].tree) 
      freeWord(st, xq[s].tree);
    clearQue(&xq[s]);
    s++;
  };
}

/*
 * Literal
 */	
static
int
cvtNum(dst, maxdst, src, maxsrc, format)
     Wchar	*dst;
     int	maxdst;
     Wchar	*src;
     int	maxsrc;
     int	format;
{
  return RkwCvtSuuji(dst, maxdst, src, maxsrc, format - 1);
}

static
int
cvtAlpha(dst, maxdst, src, maxsrc, format)
     Wchar	*dst;
     int	maxdst;
     Wchar	*src;
     int	maxsrc;
     int	format;
{
    switch(format) {
#ifdef ALPHA_CONVERSION
    case 1: 	return RkwCvtZen(dst, maxdst, src, maxsrc);
    case 2: 	return RkwCvtHan(dst, maxdst, src, maxsrc);
    case 3: 	return -1;
#else
    case 1: 	return RkwCvtNone(dst, maxdst, src, maxsrc);
    case 2: 	return -1;
#endif
    default: 	return 0;
    }
}

static
int
cvtHira(dst, maxdst, src, maxsrc, format)
     Wchar	*dst;
     int	maxdst;
     Wchar	*src;
     int	maxsrc;
     int	format;
{
  switch(format) {
  case 1: 	return RkwCvtHira(dst, maxdst, src, maxsrc);
  case 2: 	return RkwCvtKana(dst, maxdst, src, maxsrc);
  default: 	return 0;
  }
}

static
int
cvtLit(dst, maxdst, src, maxsrc, format, mode)
     Wchar		*dst;
     int		maxdst;
     Wchar		*src;
     int		maxsrc;
     int		format;
     unsigned long	mode;
{
  switch(format >> 4) {
  case LIT_NUM:
    if (mode & RK_MAKE_KANSUUJI)
      return cvtNum(dst, maxdst, src, maxsrc, format&15);
    else
      return RkwCvtNone(dst, maxdst, src, maxsrc);
  case LIT_ALPHA: 	return cvtAlpha(dst, maxdst, src, maxsrc, format&15);
  case LIT_HIRA: 	return cvtHira(dst, maxdst, src, maxsrc, format&15);
  default:		return 0;
  }
}

/* setLit
 *	create the literals as many as the context requires 
 */
static
struct nword	*
setLit(cx, word, maxword, rc, src, srclen, format)
     struct RkContext	*cx;
     struct nword	*word;
     int		maxword;
     int		rc;
     Wchar		*src;
     int		srclen;
     int		format;
{
  struct nword	*w = word;
  int 		dstlen;
  unsigned long	mode;
  
  if (!cx->litmode)
    return 0;
  for (mode = cx->litmode[format]; mode; mode >>= RK_XFERBITS)
    if (w < word + maxword) {
      int	code = MAKELIT(format, mode&RK_XFERMASK);

      dstlen = cvtLit((Wchar *)0, 9999, src, srclen, code, cx->concmode);
      if (0 < dstlen  && dstlen <= RK_LEN_WMAX) 
	setWord(w++, rc, code, src, srclen, (Wrec *)0, dstlen, cx->gram->P_BB);
      if (dstlen < 0) 
	setWord(w++, rc, code, src, srclen, (Wrec *)0, srclen, cx->gram->P_BB);
    }
  return (struct nword *) w;
}

static
struct nword	*
readWord(cx, yy, ys, ye, class, nword, maxword, doflush, douniq)
     struct RkContext	*cx;
     int		yy, ys, ye;
     int		class;
     struct nword	*nword;
     int		maxword;
     int		doflush;
     int		douniq;
{
  Wchar		*key = cx->store->yomi + yy;
  struct nword	*wrds;
  struct MD	*head = cx->md[class], *md;
  struct nread	nread[128];
  int		maxcache = RkNumber(nread);
  
  wrds = nword;
  for (md = head->md_next; md != head; md = md->md_next) {
    struct DM		*dm = md->md_dic;
    struct DM		*qm = md->md_freq;
    struct nword	*pp, *qq;
    int			c, nc, num, cf = 0, nl;
    
    if (maxword <=  0)
      break;
    if (!dm)
      continue;
    if (qm && !qm->dm_qbits)
      qm = (struct DM *)0;
    nc = DST_SEARCH(cx, dm, key, ye, nread, maxcache, &cf);
    for (c = 0; c < nc; c++) {
      struct nread	*thisRead = nread + c;
      struct ncache	*thisCache = thisRead->cache;
      unsigned char	*wp = thisCache->nc_word;
      unsigned		offset;
      int		nk, cnt = 1;
      unsigned		permutation[RK_CAND_NMAX];
      unsigned char	*candidates[RK_CAND_NMAX];
      unsigned		csnb;
      int		bitSize;

      nk = _RkCandNumber(wp);
      nl = (*wp >> 1) & 0x3f;
      if (!doflush && (cf || thisRead->nk > ye || thisRead->nk > RK_KEY_WMAX))
	cx->poss_cont++;
      if (*wp & 0x80)
	wp += 2;
      wp += 2 + nl *2;
      csnb = thisRead->csn;
      offset = thisRead->offset;
      if (ys < thisRead->nk && thisRead->nk <= ye && thisRead->nk <= RK_KEY_WMAX)  {
	for (num = 0; num < nk; num++) {
	  candidates[num] = wp;
	  wp += 2 * ((*wp >> 1) & 0x7f) + 2;
	};
	if (qm) {
	  int	ecount, cval, i;
	  
	  bitSize = _RkCalcLog2(nk + 1) + 1;
	  _RkUnpackBits(permutation, qm->dm_qbits, offset, bitSize, nk);
	  for (ecount = cval = i = 0; i < nk; i++) {
	    if (permutation[i]/2 >  nk) {
	      ecount++;
	      break;
	    };
	    cval += permutation[i];
	  }
	  if (ecount || cval < (nk-1)*(nk-2)) {
	    for (i = 0; i < nk; i++)
	      permutation[i] = 2*i;
	    _RkPackBits(qm->dm_qbits, offset, bitSize, permutation, nk);
	  };
	};
	pp = wrds;
	for (num = 0; num < nk; num++) {
	  unsigned	permed;
	  
	  if (maxword <=  0)
	    break;
	  if (qm) {
	    if ((permed = permutation[num]/2) > nk) {
	      break;
	    }  else if (permed == nk)
	      continue;
	  } else
	    permed = num;
	  wp = candidates[permed];
	  clearWord(wrds, cx->gram->P_BB);
	  wrds->nw_kanji = wp;
	  wrds->nw_freq = qm;
	  wrds->nw_rowcol = _RkRowNumber(wp);
	  wrds->nw_cache = thisCache;
	  wrds->nw_ylen = thisRead->nk;
	  wrds->nw_klen = (*wp >> 1) & 0x7f;
	  wrds->nw_class = class;
	  wrds->nw_csn = csnb + permed;
	  wrds->nw_prio = 0;
	  if (class == ND_MWD) {
	    if (qm && qm->dm_rut) {
	      if (cnt)
		cnt = wrds->nw_prio = searchRut(qm->dm_rut, wrds->nw_csn);
	    } else if (DM2TYPE(dm)) {
	      if (num < 2)
		wrds->nw_prio = ((struct TW *)thisCache->nc_address)->lucks[num];
	    }
	    if (wrds->nw_prio) {
	      long    t;
	      
	      t = _RkGetTick(0) - wrds->nw_prio;
	      wrds->nw_prio = (0 <= t && t < 0x2000) ? (0x2000 - t) << 4 : 0;
	    };
	    switch(num) {
	    case 0: wrds->nw_prio += 15; break;
	    case 1: wrds->nw_prio += 11; break;
	    case 2: wrds->nw_prio += 7; break;
	    case 3: wrds->nw_prio += 3; break;
	    };
	    wrds->nw_prio |= 0x01;
	  };
	  if (douniq) {
	    for (qq = pp; qq < wrds; qq++)
	      if (qq->nw_rowcol == wrds->nw_rowcol)
		break;
	    if (qq < wrds)
	      continue;
	  }
	  _RkEnrefCache(thisCache);
	  wrds++;
	  maxword--;
	};
      };
      _RkDerefCache(thisCache);
    };
    maxcache -= nc;
  };
  return(wrds);
}

/* makeWord
 *	jisho ni nai katakana, suuji, tokushu moji wo tango to minasu
 */
/*ARGSUSED*/
static
struct nword	*
makeWord(cx, yy, ys, ye, class, word, maxword, doflush, douniq)
     struct RkContext	*cx;
     int		yy, ys, ye;
     int		class;		/* word class */
     struct nword	*word;
     int		maxword;
     int		doflush;
     int		douniq;
{
  struct nstore	*st = cx->store;
  Wchar		*key = st->yomi + yy;
  Wchar		*k, *z;
  struct nword	*w = word;
  Wchar		c;
  int			clen;
  int			hinshi = cx->gram->P_BB;
  int			literal = -1;
  int			punct = 0;
  int			gobeyond = 0;
  
  if (ye <= 0)
    return w;
  z = (k = key) + ye;
  /* sentou moji wo yomu */
  c = *k++;
  clen = 1;
  if (us_iscodeG0(c)) {		/* ascii string */
    if ('0' <= c && '9' >= c) {	/* numeral */
      if (!(cx->concmode & RK_MAKE_EISUUJI)) {
	doflush++;
      } else {
	for (; k < z; k++, clen++)
	  if (clen >= RK_KEY_WMAX || !('0' <= *k && *k <= '9')) {
	    doflush++;
	    break;
	  };
      }
      hinshi = cx->gram->P_NN; literal = LIT_NUM;
    } else {				/* others */
      if (!(cx->concmode & RK_MAKE_EISUUJI)) {
	doflush++;
      } else {
	for (; k < z; k++, clen++)
	  if (clen >= RK_KEY_WMAX || !us_iscodeG0(*k)) {
	    doflush++;
	    break;
	  };
      }
      hinshi = cx->gram->P_T35; literal = LIT_ALPHA;
    }
  } else if (us_iscodeG1(c)) {
    if (0xb000 <= c) {		/* kanji string */
      for (; k < z; k++, clen++)
	if (clen >= RK_KEY_WMAX || *k < 0xb000) {
	  doflush++;
	  break;
	};
      hinshi = cx->gram->P_T00;
    } else if (0xa1a2 <= c && c <= 0xa1db) {
      /*
       *	now multiple punctiation characters constitute a single punct 
       */
      for (; k < z; k++, clen++)
	if (clen >= RK_KEY_WMAX || !(0xa1a2 <= *k && *k <= 0xa1db)) {
	  doflush++;
	  break;
	};
      switch(c) {
      case	0xa1a2:	case	0xa1a3:	case	0xa1a4:
      case	0xa1a5: case	0xa1a6:	case	0xa1a7:
      case	0xa1a8:	case	0xa1a9:	case	0xa1aa:
      case	0xa1c4:
	punct = ND_PUN;
	break;
      case	0xa1c6:	case	0xa1c8:	case	0xa1ca:
      case	0xa1cc: case	0xa1ce:
      case	0xa1d0:	case	0xa1d2:	case	0xa1d4:
      case	0xa1d6:	case	0xa1d8:	case	0xa1da:
	punct = ND_OPN;
	break;
      case	0xa1c7:	case	0xa1c9:	case	0xa1cb:
      case	0xa1cd:	case	0xa1cf:	case	0xa1d1:
      case	0xa1d3:	case	0xa1d5:	case	0xa1d7:
      case	0xa1d9:	case	0xa1db:
	punct = ND_CLS;
	break;
      default:
	hinshi = cx->gram->P_T00;
	doflush++;
      };
    } else if (0xa3b0 <= c && c <= 0xa3b9) {	/* suuji */
      if (!(cx->concmode & RK_MAKE_EISUUJI)) {
	doflush++;
      } else {
	for (; k < z; k++, clen++) 
	  if (clen >= RK_KEY_WMAX || !(0xa3b0 <= *k && *k <= 0xa3b9)) {
	    doflush++;
	    break;
	  };
      }
      hinshi = cx->gram->P_NN; literal = LIT_NUM;
    } else if ((0xa3c1 <= c && c <= 0xa3da)
	       || (0xa3e1 <= c && c <= 0xa3fa)) {	/* eiji */
      if (!(cx->concmode & RK_MAKE_EISUUJI)) {
	doflush++;
      } else {
	for (; k < z; k++, clen++)
	  if (clen >= RK_KEY_WMAX
	      || !((0xa3c1 <= (c = *k) && c <= 0xa3da)
		   || (0xa3e1 <= c && c <= 0xa3fa))) {
	    doflush++;
	    break;
	  };
      }
      hinshi = cx->gram->P_T35; literal = LIT_ALPHA;
    } else if (0xa5a1 <= c && c <= 0xa5f6) {	/* zenkaku katakana */
      for (; k < z; k++, clen++) 
	if (clen >= RK_KEY_WMAX ||
	    ((0xa5a1 > (c = *k) || c > 0xa5f6) &&
	     (0xa1a1 > c || c > 0xa1f6))) {
	  doflush++;
	  break;
	};
      hinshi = cx->gram->P_T30;
    } else if (0xa4a1 <= c && c <= 0xa4f3) {	/* hiragana */
      for (; k < z; k++, clen++) {
	if (clen >= RK_KEY_WMAX) {
	  doflush++;
	  break;
	};
	switch (*k) {
	case 0xa4a1: case 0xa4a3: case 0xa4a5:
	case 0xa4a7: case 0xa4a9:
	case 0xa4e3: case 0xa4e5: case 0xa4e7:
	case 0xa4c3: case 0xa4f3:
	case 0xa1ab: case 0xa1ac: case 0xa1b3:
	case 0xa1b4: case 0xa1b5: case 0xa1b6:
	case 0xa1bc:
	  continue;
	default:
	  doflush++;
	  gobeyond++;
	  goto hira;
	};
      };
    hira:
      hinshi = cx->gram->P_T35;
    } else {
      doflush++;
      hinshi = cx->gram->P_T35;
    };
  } else if (us_iscodeG2(c)) {	/* hankaku katakana */
    for (; k < z; k++, clen++)
      if (clen >= RK_KEY_WMAX || !us_iscodeG2(*k)) {
	doflush++;
	break;
      };
    hinshi = cx->gram->P_T30;
  } else {
    doflush++;
    hinshi = cx->gram->P_T35;
  }
  if ((ys <= clen && clen <= ye) || gobeyond) {
    if (class == ND_MWD || punct) {
      if (!doflush && !gobeyond)
	cx->poss_cont++;
      if (literal != -1) {
	if (doflush) 
	  w= setLit(cx, w, maxword, hinshi, key, clen, literal);
      } else if (w < word + maxword) {
	  if (doflush) {
	    setWord(w++, hinshi, 0, key, clen, (Wrec *)0,
		    clen, cx->gram->P_BB);
	    if (punct)
	      w[-1].nw_class = punct;
	  };
	}
    }
  }
  return w;
}

static int
determinate(y1, y2, l)
     Wrec	*y1, *y2;
     int	l;
{
  if ((int)*y1 > l)
    return(0);
  for (l = *y1, y1 += 2; l; l--) {
    if (*y1++ != *y2++ || *y1++ != *y2++) {
      return(0);
    }
  }
  return(1);
}

static
int
positive(y1, y2, l)
     Wrec	*y1, *y2;
     int	l;
{
  l = (int)*y1 < l ? (int)*y1 : l;
  for (y1 += 2; l; l--) {
    if (*y1++ != *y2++ || *y1++ != *y2++) {
      return(0);
    }
  }
  return(1);
}

static
void
cancelNVE(nv, p)
     struct NV	*nv;
     struct NVE *p;
{
  unsigned char	*s = p->data;

  nv->csz -= *s * 2 + 2;
  nv->cnt--;
  p->right->left = p->left;
  p->left->right = p->right;
  (void)free((char *)s);
  (void)free((char *)p);
}

static
struct NVE *
newNVE(nv, y, l, v)
     struct NV	*nv;
     Wrec	*y;
     int	l;
     int	v;
{
  unsigned short	w;
  struct NVE		*p, **q, *r;
  struct NVE		*nve;
  unsigned char		*s;

  nve = (struct NVE *)calloc(1, sizeof(struct NVE));
  if (nve) {
    s = (unsigned char *)malloc(l * 2 + 2);
    if (s) {
      nve->data = s;
      *s++ = l;
      *s++ = v;
      memcpy(s, y, l * 2);
      nv->csz += l * 2 + 2;
      nv->cnt++;
      while ((p = nv->head.right) != &nv->head && nv->csz >= nv->sz) {
	w = bst2_to_s(p->data + 2);
	q =  nv->buf + w % nv->tsz;
	while ((r = *q) != (struct NVE *)0) {
	  if (r == p) {
	    *q = r->next;
	    cancelNVE(nv, p);
	    break;
	  } else
	    q = &r->next;
	}
      }
      if (nv->csz >= nv->sz) {
	nv->csz -= l * 2 + 2;
	nv->cnt--;
	(void)free((char *)nve->data);
	(void)free((char *)nve);
	return((struct NVE *)0);
      }
    } else {
      (void)free((char *)nve);
      nve = (struct NVE *)0;
    }
  }
  return(nve);
}

int
_RkRegisterNV(nv, yomi, len, half)
     struct NV	*nv;
     Wrec	*yomi;
     int	len;
     int	half;
{
  unsigned short	v;
  struct NVE		*p, **q, **r;

  if (nv && nv->tsz && nv->buf) {
    v = bst2_to_s(yomi);
    q = r = nv->buf + v % nv->tsz;
    for (p = *q; p; p = *q) {
      if (positive(p->data, yomi, len)) {
	*q = p->next;
	cancelNVE(nv, p);
      } else {
	q = &p->next;
      }
    }
    p = newNVE(nv, yomi, len, half);
    if (p) {
      p->next = *r;
      *r = p;
      p->left = nv->head.left;
      p->left->right = p;
      p->right = &nv->head;
      nv->head.left = p;
    }
  }
  return(0);
}

/* parseWord
 *	bunsestu no ki wo seichou saseru.
 */
static int
parseWord(cx, yy, ys, ye, class, xqh, maxclen, doflush, douniq)
     struct RkContext	*cx;
     int		yy, ys, ye;
     int		class;
     struct nword	*xqh[];	/* indexed by nw_ylen */
     int		maxclen;	/* saishou yomi no nagasa */
     int		doflush;
     int		douniq;
{
  struct RkKxGram	*gram = cx->gram->gramdic;
  int			clen;
  static unsigned	classmask[] = { /* 後ろにつながるクラス */
    (1 << ND_SWD) | (1 << ND_SUC),	/* MWD --> SUC | SWD */
    (1 << ND_SWD),			/* SWD --> SWD */
    (1 << ND_MWD) | (1 << ND_SWD),	/* PRE --> MWD | SWD */
    (1 << ND_SWD),			/* SUC --> SWD */
    (1 << ND_MWD) | (1 << ND_SWD) | (1 << ND_PRE),/* EMP --> MWD | SWD | PRE */
  };

  for (clen = 0; (clen <= maxclen && clen < ye); clen++) {
    struct nword	*tail[256];
    struct nword	right[64*16];
    int			sameLen;
    int			t;
    struct nword	*p, *q, *r;
    int			ys1, ye1;

    /* 読みの長さ clen の単語のうち、後ろに class で指定された単語が
       つながる可能性があるものをリストアップし、tail に記録する */
    for (p = xqh[clen], sameLen = 0; p; p = p->nw_next) {
      if (classmask[p->nw_class] & (1<<class)) {
	/* p の後ろに class の単語がつながる可能性がある */
	if (sameLen < RkNumber(tail)) { /* まだ tail にあきがある */
	  tail[sameLen++] = p;
	}
      }
    }
    if (!sameLen)
      continue;
    ys1 = ys - clen; if (ys1 < 0)  ys1 = 0;
    ye1 = ye - clen;
    r = readWord(cx, yy + clen, ys1, ye1, class,
		 right, RkNumber(right) - 1, doflush, douniq);
    if (Is_Word_Make(cx)) 
      r = makeWord(cx, yy + clen, ys1, ye1, class,
		   r, (int)RkNumber(right) -1 - (r - right), doflush, douniq);
    for (t = 0; t < sameLen; t++) {
      unsigned char	*cj;
      p = tail[t];
      cj = (unsigned char *)(gram ? GetGramRow(gram, p->nw_rowcol) : 0);
      for (q = right; q < r; q++)  
	if (Is_Word_Connect(cx) &&
	    (q->nw_class >= ND_OPN || !cj || TestGram(cj, q->nw_rowcol)))  {
	  struct nword	*pq = concWord(cx->store, p, q, clen, cx->gram->P_BB);
	  if (pq) {
	    struct nword	*s;
	    if (gram && !IsShuutan(gram, pq->nw_rowcol)) {
#ifdef BUNMATU
		/* 文章末となりうるか? */
		if (IsBunmatu(gram, pq->nw_rowcol)) {
		    DoBunmatu(pq);
		    /* 文章末でないなら文節終端にはならない */
		    if (!(yy + pq->nw_ylen == cx->store->nyomi ||
			  q->nw_class >= ND_OPN))
			DontSplitWord(pq);
		    else DoSplitWord(pq);
		} else DontSplitWord(pq);
#else
		DontSplitWord(pq);
#endif
	    }
	    if ((unsigned long)maxclen < (unsigned long)pq->nw_ylen) {
	      while (++maxclen < (int)pq->nw_ylen)
		xqh[maxclen] = (struct nword *)0;
	      xqh[maxclen] = pq;
	    }
	    else {
	      s = xqh[pq->nw_ylen];
	      if (s) {
		while (s->nw_next)
		  s = s->nw_next;
		s->nw_next = pq;
	      }
	      else 
		xqh[pq->nw_ylen] = pq;
	    }
	    pq->nw_next = (struct nword *)0;
	  }
	}
    }
    for (q = right; q < r; q++) 
      if (q->nw_cache)
	_RkDerefCache(q->nw_cache);
    if (!gram)
      goto done;
  }
 done:
  return maxclen;
}

/*ARGSUSED*/
static int
doParse(cx, yy, ys, ye, xqh, maxclen, doflush, douniq)
     struct RkContext	*cx;
     int		yy, ys, ye;
     struct nword	*xqh[];
     int		maxclen;
     int		doflush;
     int		douniq;
{
  maxclen = parseWord(cx, yy, ys, ye, ND_PRE, xqh, maxclen, doflush, douniq);
  maxclen = parseWord(cx, yy, ys, ye, ND_MWD, xqh, maxclen, doflush, douniq);
  maxclen = parseWord(cx, yy, ys, ye, ND_SUC, xqh, maxclen, doflush, douniq);
  maxclen = parseWord(cx, yy, ys, ye, ND_SWD, xqh, maxclen, doflush, douniq);
  return maxclen;
}

/* getKanji
 *	get kanji in reverse order 
 */
Wchar *
_RkGetKanji(cw, key, mode)
     unsigned long	mode;
     struct nword	*cw;
     Wchar		*key;
{
  Wrec			 *str;
  static Wchar		tmp[RK_LEN_WMAX+1];
  Wchar	 		*p = tmp;
  int		   	klen, ylen;
  struct nword		*lw = cw->nw_left;

  klen = cw->nw_klen - lw->nw_klen;
  ylen = cw->nw_ylen - lw->nw_ylen;
/* nw_cache --> nw_kanji !nw_lit */
/* !nw_cache --> !nw_kanji nw_lit */

  if (cw->nw_cache) {
    if ((*(cw->nw_kanji) >> 1) & 0x7f) {
      str = cw->nw_kanji + NW_PREFIX;
      for (; klen-- ; str += 2)
	*p++ = S2TOS(str);
    return tmp;
    } else
      return key;
  } else if (cw->nw_kanji) {
    _Rkpanic("_RkGetKanji\n", 0, 0, 0);
    str = cw->nw_kanji + NW_PREFIX;
    for (; klen-- ; str += 2)
      *p++ = S2TOS(str);
    return tmp;
  } else if (cw->nw_lit) {	
    if (cvtLit(tmp, klen + 1, key, ylen, cw->nw_lit, mode) > 0)
      return tmp;
    else
      return key;
  } else
    return key;
}

static
int
getKanji(w, key, d, mode)
     struct nword	*w;
     Wchar		*key;
     Wchar		*d;
     unsigned long	mode;
{
  struct nword	*cw, *lw;
  int			hash, klen;
  
  hash = 0;
  for (cw = w; cw; cw = lw) {
    Wchar	*s, *t;
    
    if (!(lw = cw->nw_left))
      continue;
    klen = (cw->nw_klen - lw->nw_klen);
    s = _RkGetKanji(cw, key + lw->nw_ylen, mode);
    t = s + klen;
    /* copy */
    while (s < t) {
      *d++ = *--t;
	hash += *t;
    }
  }
  return hash;
}

/* uniqWord
 *	unique word list
 */
static
uniqWord(key, words, ylen, mode)
     Wchar		*key;
     struct nword	*words;
     unsigned		ylen;
     unsigned long	mode;
{
  struct nword	*p;
  int			uniq[16], heap[512];
  int			hp = 0;
  
  if (!key || ylen <= 0)
    return;
  /* clear hash table */
  uniq[ 0] = uniq[ 1] = uniq[ 2] = uniq[ 3] = 
    uniq[ 4] = uniq[ 5] = uniq[ 6] = uniq[ 7] = 
      uniq[ 8] = uniq[ 9] = uniq[10] = uniq[11] = 
	uniq[12] = uniq[13] = uniq[14] = uniq[15] = -1;
  for (p = words; p; p = p->nw_next) 
    if (CanSplitWord(p) && p->nw_ylen == ylen) {
      int			wsize;
      /* compute word size */
      wsize = (2*p->nw_klen + sizeof(heap[0])-1)/sizeof(heap[0]);
      if (hp + 1 + wsize < RkNumber(heap)) {
	int	hno, h;
	/* put kanji string without EOS */
	heap[hp + wsize] = 0;
	hno = getKanji(p, key, (Wchar *)&heap[hp + 1], mode)&15;
	/* search on the hash list */
	for (h = uniq[hno]; h >= 0; h = heap[h&0xffff]) 
	  if ((h >> 16) == p->nw_klen) { /* same length */
	    int *p1 = &heap[(h&0xffff) + 1];
	    int *p2 = &heap[hp + 1];
	    int		 i;
	    /* compare by word */
	    switch(wsize) {
	    case 3:	if (*p1++ != *p2++) goto next;
	    case 2:	if (*p1++ != *p2++) goto next;
	    case 1:	if (*p1++ != *p2++) goto next;
	    case 0:	break;
	    default:
	      for (i = wsize; i--;)
		if (*p1++ != *p2++) goto next;
	      break;
	    }
	    /* match */
	    DontSplitWord(p);
	    goto  done;
	  next:
	    continue;
	  }
	/* enter new entry */
	heap[hp + 0] = uniq[hno];
	uniq[hno] = (p->nw_klen<<16)|hp;
	hp += 1 + wsize;
      }
    done:
      continue;
    }
}

/* sortWord
 *	word list wo sort suru
 */
struct compRec {
    struct nword	*word;
    int			prio;
};

static
int
compword(x, y)
     struct compRec *x, *y;
{
  int	d = y->word->nw_prio - x->word->nw_prio;
  return d ? d : x->prio - y->prio;
}

static
struct nword	*
sortWord(words)
     struct nword	*words;
{
  unsigned 			nwords, pos, neg;
  int				i, p, n;
  struct compRec	*wptr;
  struct nword		*w;
/* count number of words */
  pos = neg = 0;
  for (w = words; w; w = w->nw_next)
    if (w->nw_prio > 0) 
      pos++;
    else
      neg++;
  nwords = pos + neg;
  if (nwords <= 0)
    return words;
  /* sort word list using work space if possible */
  wptr = (struct compRec *)malloc(sizeof(struct compRec)*nwords);
  if (wptr) {
    p = 0;
    n = pos;
    /* store pointers */
    for (w = words; w; w = w->nw_next)
      if (w->nw_prio > 0) {	/* positive list */
	wptr[p].word = w;
	wptr[p].prio = p;
	p++;
      } else {			/* negative list && null word */
	wptr[n].word = w;
	n++;
      }
    /* positive list no sakusei */
    if (pos > 1)
	(void)qsort((char *)wptr, (int)pos,
		    sizeof(struct compRec), compword);
    for (i = 1; i < nwords; i++) 
      wptr[i - 1].word->nw_next = wptr[i].word;
    words = wptr[0].word;
    (void)free((char *)wptr);
  }
  return words;
}

static
struct nword	*
height2list(height, maxclen)
     struct nword *height[];
     int maxclen;
{
  int			i;
  struct nword		*e, *p, *head, *tail;
  
  e = height[0];
  tail = (struct nword *)0;
  for (i = 1; i <= maxclen; i++)
    if (height[i]) {
      for (p = height[i] ; p->nw_next ;) {
	p = p->nw_next;
      }
      if (tail)
	tail->nw_next = height[i];
      else
	head = height[i];
      tail = p;
    }
  if (tail) 
    tail->nw_next = e;
  else
    head = e;
  return head;
}
static
void
list2height(height, maxclen, parse)
     struct nword	*height[];
     int	maxclen;
     struct nword	*parse;
{
  int		i;
  struct nword	*p, *q;
  
  for (i = 0; i <= maxclen; i++)
    height[i] = (struct nword *)0;
  for (p = parse; p; p = p->nw_next) 
    if ((unsigned long)p->nw_ylen <= (unsigned long)maxclen && !height[p->nw_ylen])
      height[p->nw_ylen] = p;
  for (i = 0; i <= maxclen; i++)
    if (height[i]) {
      for (p = height[i] ; (q = p->nw_next) != (struct nword *)0; p = q) {
	if (q->nw_ylen != i) {
	  p->nw_next = (struct nword *)0;
	  break;
	}
      }
    }
}

/* parseBun
 *	key yori hajimaru bunsetsu wo kaiseki suru
 */
static
struct nword	*
parseBun(cx, yy, ys, ye, doflush, douniq, maxclen)
     struct RkContext	*cx;
     int		yy, ys, ye;	/* kaiseki seiyaku */
     int		doflush;
     int		douniq;		/* unique shori sitei */
     int		*maxclen;	/* bunsetu saidai moji suu */
{
  struct nstore	*st = cx->store;
  struct nword	**xqh = st->xqh;

#ifdef TEST
  printf("parseBun[yy = %d, ys = %d, ye = %d]\n", yy, ys, ye);
#endif
    
  xqh[0] = allocWord(st, cx->gram->P_BB);
  if (xqh[0]) {
    *maxclen = doParse(cx, yy, ys, ye, xqh, 0, doflush, douniq);
    return  height2list(xqh, *maxclen);
  } else {	/* kaiseki funou */
    *maxclen = 0;
    return  (struct nword *)0;
  }
}

static 
void
storeBun(cx, yy, ys, ye, bun)
     struct RkContext	*cx;
     int			yy, ys, ye;
     struct nbun	*bun;
{
  struct nword	*full;
  struct nword	*w;
  int		maxclen;
  
  full = sortWord(parseBun(cx, yy, ys, ye, 1, 0, &maxclen));
  bun->nb_cand = full;
  bun->nb_yoff = yy;
/* kouho wo unique ni suru */
  uniqWord(cx->store->yomi + yy, full, bun->nb_curlen, cx->concmode);
  bun->nb_curcand = 0;
  bun->nb_maxcand = 0;
  for (w = full; w; w = w->nw_next) {
    if (CanSplitWord(w) && w->nw_ylen == bun->nb_curlen)
      bun->nb_maxcand++;
  }
}

/* 
 * SPLIT
 */
struct splitParm {
  unsigned	u2;
  int		l2;
};

static
void
evalSplit(suc, ul)
     struct nword	*suc;
     struct splitParm	*ul;
{
  struct nword	*p;
  int		l2;
  unsigned	u2;
  
  l2 = 0;
  u2 = 0;
  for (p = suc; p; p = p->nw_next)  
  {
    if (!CanSplitWord(p) || (p->nw_flags & NW_SUC))
      continue;
    if ((unsigned long)l2 < (unsigned long)p->nw_ylen) 
      l2 = p->nw_ylen;
    if (u2 < p->nw_prio)
      u2 = p->nw_prio;
  };
  ul->l2 = l2;
  ul->u2 = u2;
}

static
int	
calcSplit(cx, yy, top, xq, maxclen, flush)
     struct RkContext	*cx;
     int		yy;
     struct nword	*top;
     struct nqueue	xq[];		/* indexed by nw_ylen */
     int		maxclen;
     int		flush;
{
  unsigned		L, L1 = 0, L2;
  unsigned		U2;
  struct nword	*w;
  struct splitParm	ul2[256 /* not a problem */ ];
  int			i;
  int			maxary = RkNumber(ul2) - 1;
  struct nstore		*st = cx->store;
  struct NVE		*p, **r;

  L2 = st->nyomi - yy;
  if (cx->nv && cx->nv->tsz && cx->nv->buf) {
    r = cx->nv->buf + *(st->yomi + yy) % cx->nv->tsz;
    for (p = *r; p; p = p->next) {
      if (determinate(p->data, (Wrec *)(st->yomi + yy), L2)) {
	if (*(p->data+1) > L1)
	  L1 = *(p->data + 1);
      }
    }
  }
  if (L1)
    return(L1);
  L = (L1 = 1)+ (L2 = 0);
  U2 = (unsigned)0;
  if (maxary > maxclen)
    maxary = maxclen;
  for (i = 0; i <= maxary; i++)
    ul2[i].l2 = ul2[i].u2 = 0;
  for (w = top; w; w = w->nw_next) {
    int				l, l1;
    struct splitParm		ul;
    /* ichido ni 2tu tukomono ha yameru */
    if (!CanSplitWord(w)) 
      continue;
    if ((w->nw_flags & NW_PRE) && (w->nw_flags & NW_SUC))  
      continue;
    /* mijikasugiru/bunsetumatu ni narenai  monoha hazusu */
    l1 = w->nw_ylen;
    if (l1 <= 0)
      continue;
    /* shuujoushi ha bun no tochuu deha tukanai */
    if (flush && yy + w->nw_ylen == cx->store->nyomi) {
      L1 = l1;
      break;
    }
    /* migi donari no bunsetsu wo kaiseki */
    if (l1 <= maxary) {
      if (!ul2[l1].l2) 
	evalSplit(xq[l1].tree, &ul2[l1]);
      ul = ul2[l1];
    } else 
      evalSplit(xq[l1].tree, &ul);
    /* hikaku */
    l = l1 + ul.l2;
    if (((L < l)) ||
	((L == l) &&  (U2 < ul.u2)) ||
	((L == l) &&  (U2 == ul.u2) && (L2 < ul.l2))
	)
    {
      L = l;
      L1 = l1;
      L2 = ul.l2;
      U2 = ul.u2;
    }
  }
  return L1;
}

static
int
splitBun(cx, yy, ys, ye)
     struct RkContext	*cx;
     int			yy, ys, ye;
{
  struct nstore			*st = cx->store;
  struct nqueue	*xq = st->xq;
  struct nword		*w;
  int		 		maxclen;
  int				i, count, junk;
  
/* create the initial bun-tree table */
    xq[0].tree = parseBun(cx, yy, ys, ye, 1, 1, &maxclen);

#ifdef TEST
  {
    printf("show splitBun [yy = %d, ys = %d, ye = %d, clen = %d]\n",
	   yy, ys, ye, maxclen);
#if 1
    showWord(xq[0].tree);
#endif
  }
#endif

    for (i = 1; i <= maxclen; i++)
	clearQue(&xq[i]);
/* create the following buns from every possible position */
    for (w = xq[0].tree; w; w = w->nw_next) {
	 if (CanSplitWord(w) && !xq[w->nw_ylen].tree) {
	     int	len = w->nw_ylen;
	     int	ys1 = (ys >= len) ? (ys - len) : 0;
	     int	ye1 = (ye - len);

	     xq[w->nw_ylen].tree = parseBun(cx, yy+len, ys1, ye1, 1, 1, &junk);
	   };
     };

/* compute the proper bunsetu length */
    count = calcSplit(cx, yy, xq[0].tree, xq, maxclen, 1);
    _RkFreeQue(st, 0, st->maxxq + 1);

#ifdef TEST
  printf("End SplitBun\n");
#endif

    return count;
}

/* parseQue
 *	queue jou de bunsetu wo kaiseki suru.
 */
static
int
parseQue(cx, maxq, yy, ys, ye, doflush)
     struct RkContext	*cx;
     int		maxq;
     int		yy, ys, ye;
     int		doflush;
{
  struct nstore		 *st = cx->store;
  struct nqueue *xq = st->xq;
  struct nword	 **xqh = st->xqh;
  int		 i, j;
  
/* put a new seed to start an analysis. */
    if (!xq[0].tree) {
	xq[0].tree = allocWord(st, cx->gram->P_BB);
	xq[0].maxlen = 0;
	xq[0].status = 0;
    }
/* try to extend each tree in the queue. */
  for (i = 0; i <= maxq; i++) {
    if (xq[i].tree) {
      int old = cx->poss_cont;
      list2height(xqh, xq[i].maxlen, xq[i].tree);
      xq[i].maxlen = doParse(cx, yy, ys, ye, xqh, xq[i].maxlen, doflush, 1);
      /* set up new analysis points */
      for (j = 0; j <= xq[i].maxlen; j++) 
	if (xqh[j] && !xq[i+j].tree) {
	  xq[i+j].tree = allocWord(st, cx->gram->P_BB);
	  xq[i+j].maxlen = 0;
	  xq[i+j].status = 0;
	  xq[i+j].status = 0x80;
	}
      xq[i].tree = height2list(xqh, xq[i].maxlen);
      if (cx->poss_cont != old)
         xq[i].status |=  0x80;
      else
         xq[i].status &= ~0x80;
    }
    ++yy;
    if (--ys < 0)  ys = 0;
    --ye;
  }
}

/* Que2Bun
 *	queue kara bunsetu wo toridasu.
 */
static
int
IsStableQue(cx, c, doflush)
     struct RkContext	*cx;
     int		c;
     int		doflush;
{
  struct nqueue	*xq = cx->store->xq;
  struct nword	*w;

  if (doflush)
  {
    if (xq[c].maxlen <= 0) 
      return 0;
    else
      return 1;
  };
  if (xq[c].maxlen <= 0) 
    return(!c ? 0 : 1);

  for (w = xq[c].tree; w; w = w->nw_next)
  {
     if (xq[c + w->nw_ylen].status)
       return 0;
     if (!c && w->nw_ylen && !IsStableQue(cx, c + w->nw_ylen, doflush))
       return 0;
  };
  return 1;
}

static
int
Que2Bun(cx, yy, ys, ye, doflush)
     struct RkContext	*cx;
     int		yy, ys, ye;
     int		doflush;
{
  struct nstore	*st = cx->store;
  struct nqueue	*xq = st->xq;
  unsigned	i;
  struct NVE	*p, **r;

  if (doflush)
    for (i = 0; i <= st->maxxq; i++)
      xq[i].status = 0;
  while (IsStableQue(cx, 0, doflush)) {
    struct nbun	*bun = &st->bunq[st->maxbun];
    int				count;

    i = 0;
    if (!doflush) {
      if (cx->nv && cx->nv->tsz && cx->nv->buf) {
	r = cx->nv->buf + *(st->yomi + yy) % cx->nv->tsz;
	for (p = *r; p; p = p->next) {
	  if (positive(p->data, (Wrec *)(st->yomi + yy), st->nyomi - yy)) {
	    if (*(p->data + 1) > i)
	      i = *(p->data + 1);
	  }
	}
      }
      if (i > st->nyomi - yy)
	break;
    }
    if ((count = calcSplit(cx, yy, xq[0].tree, xq, xq[0].maxlen, 1)) > 0) {
      /* shift queue to left */
      _RkFreeQue(st, 0, count);
      for (i = count; i <= st->maxxq; i++) {
	xq[i-count] = xq[i];
	clearQue(&xq[i]);
      };
      bun->nb_curlen = count;
      storeBun(cx, (int)bun->nb_yoff, 0, ye, bun);
      st->maxbun++;
      st->bunq[st->maxbun].nb_yoff = yy + bun->nb_curlen;
    }
    yy = yy + bun->nb_curlen;
    ys = ys - bun->nb_curlen;
    ye = ye - bun->nb_curlen;
  }
  return st->maxbun;
}

/* _RkRenbun2
 *	current bunsetsu kara migi wo saihenkan suru 
 */
int
_RkRenbun2(cx, firstlen)
     struct RkContext	*cx;
     int		firstlen;  /* bunsetsu chou sitei(ow 0) */
{
  struct nstore		*st = cx->store;
  struct nbun	*bun = &st->bunq[st->curbun];
  int			count;
  int			yy, ys, ye;		/* yomi kensaku hani */
  int			oldcurbun = st->curbun;
  int			uyomi;
  int			i;
  
  yy = bun->nb_yoff;
  ys = 0;
  ye = st->nyomi - bun->nb_yoff;
/* release queue */
  uyomi = st->nyomi - st->bunq[st->maxbun].nb_yoff;
  if (IS_XAUTCTX(cx)) {
    if (uyomi >= 0)
      _RkFreeQue(st, 0, uyomi+1);
  };
/* 
 *
 */
  for (count = 0; ye > 0; count++) 
  {
/* sudeni kaiseki zumi deareba, sono kekka wo mochiiru */
    if (count && !uyomi) 
    {
      int	b, c;
      for (b = st->curbun; b < st->maxbun; b++)
	if (st->bunq[b].nb_yoff == yy) {
	  /* dispose inbetween bun-trees  */
	  for (c = st->curbun; c < b; c++) {
	    freeWord(st, st->bunq[c].nb_cand);
	    st->bunq[c].nb_cand = (struct nword *)0;
	  }
	  /* shift bunq forward */
	  while (b < st->maxbun) 
	    st->bunq[st->curbun++] = st->bunq[b++];
	  goto	exit;
	}
    }
/* dispose the current bun-tree */
    if (st->curbun < st->maxbun) {
      freeWord(st, bun->nb_cand);
      bun->nb_cand = (struct nword *)0;
    }
    /* compute the length of bun */
    if (st->curbun >= st->maxbunq)	/* too many buns */
      bun->nb_curlen = ye;
    else {
      if (firstlen) { 			/* length specified */
	bun->nb_curlen = firstlen;
	firstlen = 0;
      } else {
      /* destroy */
	bun->nb_curlen = splitBun(cx, yy, ys, ye);
	if (!bun->nb_curlen)		/* fail to split */
	  bun->nb_curlen = ye;
      }
    }
/* set up bun (xqh is destroyed */
    storeBun(cx, yy, ys, ye, bun);
#if defined(TEST) && 0
	showWord(bun->nb_cand);
#endif
    yy += bun->nb_curlen;
    if ((ys -= (int)bun->nb_curlen) < 0)
      ys = 0;
    ye -= bun->nb_curlen;
    bun++;
    st->curbun++;
  }
/* free the remaining bun-trees */
  while (st->maxbun > st->curbun) {
    freeWord(st, st->bunq[--st->maxbun].nb_cand);
    st->bunq[st->maxbun].nb_cand = (struct nword *)0;
  }
/* do final settings */
 exit:
    st->maxbun = st->curbun;
    st->curbun = oldcurbun;
    st->bunq[st->maxbun].nb_yoff = 0;
/* i hate this fake, ... */
    for (i = 0; i < st->maxbun; i++)
      st->bunq[st->maxbun].nb_yoff += st->bunq[i].nb_curlen;
/* this case will never happen */
    if (0 != (st->nyomi - st->bunq[st->maxbun].nb_yoff))
	_Rkpanic("Renbun2: uyomi destroyed %d %d\n", 
		st->nyomi, st->bunq[st->maxbun].nb_yoff, 0);
    bun = &st->bunq[st->maxbun];
    if (IS_XAUTCTX(cx) && uyomi > 0)
    {
      _RkSubstYomi(cx, 0, uyomi, st->yomi + bun->nb_yoff, uyomi);
      st->curbun = oldcurbun;
    };
    return st->maxbun;
}

/* RkSubstYomi
 */
int
_RkSubstYomi(cx, ys, ye, yomi, newLen)
     struct RkContext	*cx;
     int		ys;
     int		ye;
     Wchar		*yomi;
     int		newLen;
{
  struct nstore		*st = cx->store;
  extern struct nstore	*_RkReallocBunStorage();
  struct nbun	*bun;
  struct nqueue		*xq;
  struct nword		**xqh;
  int			i, j;
  int			count;
  int			yf;
  int			cs, ce, cf;
  Wchar			*d, *s, *be;
  int			nbun;
  int			new_size;

  yf = ys + newLen;
  cs = ys;
  ce = ye;
  /*
   * STEP 0:	reallocate resources if needed 
   *		youmigana buffer should be reallocated as well.
   */
  new_size = st->nyomi + (newLen - (ye - ys));
  if (new_size>st->maxyomi || new_size > st->maxbunq || new_size > st->maxxq)
  {
      st = _RkReallocBunStorage(st, (int)(new_size*1.2+10));
      if (!st)
	  return -1;
      cx->store = st;
  };
  /*
   * STEP 1:	update yomigana buffer 
   */
  /* move unchanged text portion [ye, ...) */
  bun = &st->bunq[st->maxbun];
  be = st->yomi + bun->nb_yoff;
  xq = st->xq;
  xqh = st->xqh;
  count = (st->nyomi - bun->nb_yoff) - ye;
  if (yf < ye) {	/* shrunk */
    d = be + yf;
    s = be + ye;
    while (count--) *d++ = *s++;
  } else if (ye < yf) {	/* enlarged */
    d = (s = st->yomi + st->nyomi) + count;
    while (count--)
      *--d = *--s;
  }
  /* replace the new text in [ys, yf) */
  usncopy(be + ys, yomi, newLen);
  st->nyomi += (yf - ye);
  cf = yf;
  /*
   *  STEP 2:	remove affected words from XQ
   */
/* Trim the words which terminate in [cs, ...) */

  for (i = 0; i < cs; i++) 
    if (xq[i].tree && cs - i <= xq[i].maxlen) {
      list2height(xqh, xq[i].maxlen, xq[i].tree);
      for (j = cs - i; j < xq[i].maxlen; j++) 
	if (xqh[j + 1]) {
	  freeWord(st, xqh[j + 1]);
	  xqh[j + 1] = (struct nword *)0;
	}
      xq[i].maxlen = 0;
      for (j = cs - i ; j >= 0 && !xqh[j] ;) {
	j--;
      }
      if (j > 0)
	xq[i].maxlen = j;
      else {
	xq[i].maxlen = 0;
	if (!j) {
	  freeWord(st, xqh[0]);
	  xqh[0] = (struct nword *)0;
	}
      }
      xq[i].tree = height2list(xqh, xq[i].maxlen);
      xq[i].status = 0;
    }
  /*  Kill the whole trees in  [cs, ce) and shift XQ to fill it. */
  _RkFreeQue(st, cs, ce);
  if (cf < ce) 
    for (i = cf, j = ce; j <= st->maxxq; i++, j++) {
      xq[i] = xq[j];
      clearQue(&xq[j]);
    }
  if (ce < cf) 
    for (i = st->maxxq, j = st->maxxq - (cf - ce); j >= ce; i--, j--) {
      xq[i] = xq[j];
      clearQue(&xq[j]);
    }
  /*
   * STEP 3	restore queues by parsing yomigana after ys.
   */
  nbun = st->maxbun;
  count = (st->nyomi - bun->nb_yoff) - ys;
  while (count > 0) {
    int		yy;
    yy = st->bunq[st->maxbun].nb_yoff;
    ys = st->nyomi - yy - count;
    parseQue(cx, cf-1, yy, ys, ys + 1, 0);
    nbun = Que2Bun(cx, yy, ys, ys + 1, 0);
    ys++;
    count--;
  }
  st->curbun = 0;
  return nbun;
}

/* RkFlushYomi
 */
int
_RkFlushYomi(cx)
     struct RkContext	*cx;
{
    int		yy = cx->store->bunq[cx->store->maxbun].nb_yoff;
    int		ys = cx->store->nyomi - yy;
    int		ret;

    parseQue(cx, cx->store->maxxq, yy, ys, ys, 1);
    if ((ret = Que2Bun(cx, yy, ys, ys, 1)) != -1)
      cx->store->curbun = 0;
    return(ret);
}

/* _RkLearnBun
 *	bunsetu jouho wo motoni gakushuu suru 
 *	sarani, word wo kaihou suru
 */
static
void	blkcpy(d, s, e)
     unsigned char	*d;
     unsigned char	*s, *e;
{	while (s < e)	*d++ = *s++;	}

static
void	
doLearn(cx, thisW)
     struct RkContext	*cx;
     struct nword	*thisW;
{
  struct nword	*leftW;

  for (; (leftW = thisW->nw_left) != (struct nword *)0 ; thisW = leftW) {
    struct ncache	*thisCache = thisW->nw_cache;

    if (thisCache) {
      struct DM		*dm = thisCache->nc_dic;
      struct DM		*qm = thisW->nw_freq;
      unsigned char	*wp;
      int		ncands;
      int		nl;
      int		offset;
      int		i;
      int		current;
      unsigned char	*candidates[RK_CAND_NMAX];
      unsigned		_RkGetOffset();

      cx->time = _RkGetTick(1);
      if (thisCache->nc_flags & NC_ERROR)
	continue;
      if (!(wp = thisCache->nc_word))
	continue;
      ncands = _RkCandNumber(wp);
      nl = (*wp >> 1) & 0x3f;
      if (qm && qm->dm_qbits)
	offset = _RkGetOffset((struct ND *)dm->dm_extdata.var, wp);
      else 
	offset = 0;
      if (*wp & 0x80)
	wp += 2;
      wp += 2 + nl * 2;
      for (i = 0;  i < ncands;  i++) {
	candidates[i] = wp;
	wp += 2 * ((*wp >> 1) & 0x7f) + 2;
      };
      if (thisCache->nc_count)
	continue;
      if (qm && qm->dm_qbits) {
	unsigned 	permutation[RK_CAND_NMAX];
	int		bits;
	
	if (!(qm->dm_flags & DM_WRITABLE))
	  continue;
	bits = _RkCalcLog2(ncands + 1) + 1;
	_RkUnpackBits(permutation, qm->dm_qbits, offset, bits, ncands);
	for (current = 0; current < ncands; current++)
	  if (ncands > permutation[current]/2 && 
	      candidates[permutation[current]/2] == thisW->nw_kanji)
	    break;
	if (current < ncands) {
	  entryRut(qm->dm_rut, thisW->nw_csn, cx->time);
	  if (0 < current) {
	    unsigned char	tmp[8192];
	    
	    _RkCopyBits(tmp, 0, bits,
			qm->dm_qbits, offset, current);
	    _RkCopyBits(qm->dm_qbits, offset + 0, bits,
			qm->dm_qbits, offset + current*bits, 1);
	    _RkCopyBits(qm->dm_qbits, offset + bits, bits, 
			tmp, 0, current);
	  };
	  qm->dm_flags |= DM_UPDATED;
	}
      } else {
	if (!(dm->dm_flags & DM_WRITABLE)) 
	  continue;
	for (current = 0; current < ncands; current++)
	  if (candidates[current] == thisW->nw_kanji)
	    break;
	if (DM2TYPE(dm)) {
	  if (current) {
	    unsigned char	*t = candidates[0];
	    unsigned char	*l = candidates[current];
	    unsigned char	*c = l + 2 * ((*l >> 1) & 0x7f) + 2;
	    unsigned char	tmp[RK_WREC_BMAX];
	    
	    ((struct TW *)thisCache->nc_address)->lucks[1]
	      = ((struct TW *)thisCache->nc_address)->lucks[0];
	    blkcpy(tmp, t, l);
	    blkcpy(t, l, c);
	    blkcpy(t + (c - l), tmp, tmp + (l - t));
	    thisCache->nc_flags |= NC_DIRTY;
	  }
	  ((struct TW *)thisCache->nc_address)->lucks[0] = cx->time;
	  dm->dm_flags |= DM_UPDATED;
	}
      }
    }
  }
}

void
_RkLearnBun(cx, cur, mode)
     struct RkContext	*cx;
     int		cur, mode;
{
  struct nstore	*st = cx->store;
  struct nbun	*bun = &st->bunq[cur];
  struct nword	*w;
  int		count = bun->nb_curcand;
  Wchar		*yomi = st->yomi + bun->nb_yoff;
  int		ylen;
  int		pos;

  derefWord(bun->nb_cand);
  if (mode) {
    if (bun->nb_flags & RK_REARRANGED) {
      ylen = bun->nb_curlen
	+ (cur < st->maxbun - 1 ? (bun + 1)->nb_curlen : 0);
      pos = bun->nb_curlen;
      if (ylen < 32) {
	Wrec yomwrec[32 * sizeof(Wchar)], *dp = yomwrec;
	Wchar *ey = yomi + ylen, *p;
	for (p = yomi ; p < ey ; p++) {
	  *dp++ = (unsigned)*p >> 8;
	  *dp++ = (unsigned)*p & 0x0ff;
	}
	_RkRegisterNV(cx->nv, yomwrec, ylen, pos);
      }
    }
    for (w = bun->nb_cand; w; w = w->nw_next) {
      if (CanSplitWord(w) && w->nw_ylen == bun->nb_curlen) {
	if (count-- <= 0) {
	  doLearn(cx, w);
	    break;
	}
      }
    }
  }
  killWord(st, bun->nb_cand);
}

