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
static char rcsid[]="$Id: RKtempdic.c,v 2.19 1994/06/01 06:54:37 misao Exp $";
#endif
/*LINTLIBRARY*/

#include	"RKintern.h"

#include	<stdio.h>
#if defined(USG) || defined(SYSV) || defined(SVR4)
# include	<string.h>
# ifndef rindex
#  define rindex strrchr
# endif
#else
# include	<strings.h>
#endif

#ifdef sony_news
#include <sys/types.h>
#endif

#if __STDC__ || defined(SVR4)
#include <time.h>
#define TIME_T time_t
#else
#define TIME_T long
#endif

#define	dm_td	dm_extdata.ptr

static void
freeTD(td)
     struct TD	*td;
{
  int	i;
  for (i = 0; i < td->td_n; i++) {
    struct TN	*tn = &td->td_node[i];
    if (IsWordNode(tn)) {
      (void)free((char *)tn->tn_word->word);
      (void)free((char *)tn->tn_word);
    } else
      freeTD(tn->tn_tree);
  }
  if (td) {
    if (td->td_node)
      (void)free((char *)td->td_node);
    (void)free((char *)td);
  }
}

/* newTD: allocates a fresh node */
static TD *
newTD()
{
  struct TD	*td;

  td = (struct TD *)malloc(sizeof(struct TD));
  if (td) {
    td->td_n = 0;
    td->td_max = 1;
    if (!(td->td_node = (struct TN *)calloc(td->td_max, sizeof(struct TN)))) {
      freeTD(td);
      return((struct TD *)0);
    }
  }
  return(td);
}

/*
 * INSERT
 */
static TN *
extendTD(tdic, key, tw)
     struct TD		*tdic;
     Wchar		key;
     struct TW		*tw;
{
  int		i, j;
  struct TN	*tp;
  struct TW	*ntw;
    
  if (!(ntw = RkCopyWrec(tw)))
    return (struct TN *)0;
  tp = tdic->td_node;
  if (tdic->td_n >= tdic->td_max) {
    if (!(tp = (struct TN *)calloc(tdic->td_max + 1, sizeof(struct TN)))) {
      (void)free((char *)ntw->word);
      (void)free((char *)ntw);
      return (struct TN *)0;
    }
    for (i = 0; i < tdic->td_n; i++)
      tp[i] = tdic->td_node[i];
    (void)free((char *)tdic->td_node);
    tdic->td_max++;
    tdic->td_node = tp;
  }
  for (i = 0; i < tdic->td_n; i++)
    if (key < tp[i].tn_key)
      break;
  for (j = tdic->td_n; j > i; j--)
    tp[j] = tp[j - 1];
  tp[i].tn_flags = TN_WORD|TN_WDEF;
  tp[i].tn_key = key;
  tp[i].tn_word = ntw;
  tdic->td_n++;
  return(tp + i);
}

static
yomi_equal(x, y, n)
     Wrec *x, *y;
     int n;
{
  int l;

  if ((l = (*y >> 1) & 0x3f) == ((*x >> 1) & 0x3f)) {
    if (*y & 0x80)
      y += 2;
    if (*x & 0x80)
      x += 2;
    x += (n << 1) + 2;
    y += (n << 1) + 2;
    for (; n < l ; n++) {
      if (*x++ != *y++ || *x++ != *y++)
	return(0);
    }
    return(1);
  }
  return(0);
}

static
Wchar
nthKey(w, n)
     Wrec	*w;
     int	n;
{
  if (n < (int)((*w >> 1) & 0x3f)) {
    if (*w & 0x80)
      w += 2;
    w += (n << 1) + 2;
    return((Wchar)((w[0] << 8) | w[1]));
  } else
    return((Wchar)0);
}

/*
 * defineTD -- テキスト辞書に定義する
 *
 * 引数
 *    dm    辞書
 *    tab   テキスト辞書ポインタ
 *    n     何文字目か(単位:文字)
 *    newW  登録するワードレコード
 *    nlen  不明
 */

static TN *
defineTD(dm, tab, n, newTW, nlen)
     struct DM	*dm;
     struct TD	*tab;
     int	n;
     struct TW	*newTW;
     int	nlen;
{
  int		i;
  Wchar		key;
  struct TN	*tn;
  struct ncache	*cache;
  struct TW	*mergeTW, *oldTW;
  Wrec		*oldW, *newW = newTW->word;

  key = nthKey(newW, n); n++;
  tn = tab->td_node;
  for (i = 0; i < tab->td_n && tn->tn_key <= key; i++, tn++) {
    if (key == tn->tn_key) {
      if (IsWordNode(tn)) {
	struct TD	*td;

	oldTW = tn->tn_word;
	oldW = oldTW->word;
	if (!key|| yomi_equal(newW, oldW, n)) {
	  if ((cache = _RkFindCache(dm, (long)oldTW)) && cache->nc_count > 0)
	    return((struct TN *)0);
	  if (!(mergeTW = RkUnionWrec(newTW, oldTW)))
	    return((struct TN *)0);
	  tn->tn_word = mergeTW;
	  (void)free((char *)oldW);
	  (void)free((char *)oldTW);
	  tn->tn_flags |= TN_WDEF;
	  if (cache) {
	    _RkRehashCache(cache, (long)tn->tn_word);
	    cache->nc_word = mergeTW->word;
	  }
	  return tn;
	}
	if (!(td = newTD()))
	  return((struct TN *)0);
	td->td_n = 1;
	key = nthKey(oldW, n);
	td->td_node[0].tn_key = key;
	td->td_node[0].tn_flags |= TN_WORD;
	td->td_node[0].tn_word = oldTW;
	tn->tn_flags &= ~TN_WORD;
	tn->tn_tree = td;
      }
      return defineTD(dm, tn->tn_tree, n, newTW, nlen);
    }
  }
  return extendTD(tab, key, newTW);
}

static
enterTD(dm, td, gram, word)
     struct DM		*dm;
     struct TD		*td;
     struct RkKxGram	*gram;
     Wchar		*word;
{
  Wrec		wrec[RK_LINE_BMAX*10];
  struct TW	tw;

  if (word[0] != (Wchar)'#') {
    if (RkParseOWrec(gram, word, wrec, RkNumber(wrec), tw.lucks)) {
      struct TN	*tn;
      
      tw.word = wrec;
      tn = defineTD(dm, td, 0, &tw, RkNumber(wrec));
      if (tn) {
	tn->tn_flags &= ~(TN_WDEF|TN_WDEL);
	return(0);
      } else {
	return(-1);
      }
    }
  }
  return(1);
}

/*
 * DELETE
 */
static
shrinkTD(td, key)
     struct TD *td;
     Wchar key;
{
  int		i;
  struct TN	*tn = td->td_node;
    
  for (i = 0; i < td->td_n; i++) {
    if (key == tn[i].tn_key) {
      while (++i < td->td_n) tn[i - 1] = tn[i];
      td->td_n--;
      break;
    }
  }
}

/*
 * deleteTD -- テキスト辞書から単語定義を取り除く
 *
 * 引数
 *    dm    辞書
 *    tab   テキスト辞書
 *    n     何文字目か
 *    newW  定義するワードレコード
 */
static int
deleteTD(dm, tab, n, newW)
     struct DM	*dm;
     struct TD	**tab;
     int	n;
     Wrec	*newW;
{
  struct TD	*td = *tab;
  int		i;
  Wchar		key;
    
  key = nthKey(newW, n); n ++;
  for (i = 0; i < td->td_n; i++) {
    struct TN	*tn = &td->td_node[i];
    
    if (key == tn->tn_key ) {
      if (IsWordNode(tn)) {
	struct TW	*oldTW = tn->tn_word;
	Wrec		*oldW = oldTW->word;
	
	if (!key || yomi_equal(newW, oldW, n)) {
	  struct ncache	*cache = _RkFindCache(dm, (long)oldTW);
	  
	  if (!cache || cache->nc_count <= 0) {
	    struct TW	*subW, newTW;

	    newTW.word = newW;
	    subW = RkSubtractWrec(oldTW, &newTW);
	    (void)free((char *)oldW);
	    (void)free((char *)oldTW);
	    if (subW) {
	      tn->tn_word = subW;
	      tn->tn_flags |= TN_WDEL;
	      if (cache) {
		_RkRehashCache(cache, (long)subW);
		cache->nc_word = subW->word;
	      }
	      return(0);
	    } else {
	      if (cache)
		_RkPurgeCache(cache);
	      shrinkTD(td, key);
	    }
	  }
	}
      } else
	if (deleteTD(dm, &tn->tn_tree, n, newW))
	  shrinkTD(td, key);
      if (td->td_n <= 0) {
	(void)free((char *)(td->td_node));
	(void)free((char *)td);
	*tab = (struct TD *)0;
	return(1);
      } else
	return(0);
    }
  }
  return(0);
}

/*
 * OPEN
 */
/*ARGSUSED*/
int
_Rktopen(dm, file, mode, gram)
     struct DM	*dm;
     char	*file;
     int	mode;
     struct RkKxGram	*gram;
{
  struct DF	*df = dm->dm_file;
  struct DD	*dd = df->df_direct;
  struct TD	*xdm;
  FILE		*f;
  char		line[RK_LINE_BMAX*10];
  long		offset;
  int		ecount;
    
  if (!df->df_rcount) {
    if (close(open(file, 0)))
      return(-1);
    df->df_flags |= DF_EXIST;
    dm->dm_flags |= DM_EXIST;
    if (!close(open(file, 2)))
      dm->dm_flags |= DM_WRITABLE;
  }
  if (!(dm->dm_flags & DM_EXIST))
    return(-1);
  if (!(f = fopen(file, "r"))) {
    df->df_flags &= ~DF_EXIST;
    dm->dm_flags &= ~DM_EXIST;
    return(-1);
  };
  if (!(xdm = newTD()))
    return -1;
  offset = 0;
  ecount = 0;
  while (fgets((char *)line, sizeof(line), f)) {
    Wchar	wcline[RK_LINE_BMAX*10];
    int		sz;
    
    offset += strlen(line);
    sz = RkCvtWide(wcline, RkNumber(wcline), line, strlen(line));
    if (sz < RkNumber(wcline) - 1) {
      if ( enterTD(dm, xdm, gram, wcline) < 0 )
	ecount++;
    } else
      ecount++;
  }
  (void)fclose(f);
  dm->dm_offset = 0;
  df->df_size = offset;
  if (ecount) {		
    freeTD((struct TD *)xdm);
    dm->dm_td = (pointer)0;
    dm->dm_flags &= ~DM_EXIST;
    return(-1);
  } else {
    dm->dm_td = (pointer)xdm;
    if (df->df_rcount++ == 0)
      dd->dd_rcount++;
    return 0;
  };
}

/*
 * CLOSE
 */
static int
writeTD(td, gram, fdes)
     struct TD		*td;
     struct RkKxGram	*gram;
     int		fdes;
{
  int	i;
  int	ecount = 0;
    
  for (i = 0; i < td->td_n; i++) {
    struct TN		*tn = &td->td_node[i];
    unsigned char	line[RK_LINE_BMAX*3];
    Wchar		wcline[RK_WREC_BMAX], *wc;
    int			sz;
    
    if (IsWordNode(tn)) {
      wc = _RkUparseWrec(gram, tn->tn_word->word, wcline,
			 RkNumber(wcline), tn->tn_word->lucks, 1);
      if (wc) {
	sz = RkCvtNarrow((char *)0, 9999, wcline, wc - wcline);
	if (sz > RK_LINE_BMAX
	    && !(wc = _RkUparseWrec(gram, tn->tn_word->word, wcline,
				    RkNumber(wcline), tn->tn_word->lucks, 0)))
	  ecount++;
	else {
	  sz = RkCvtNarrow((char *)line, RK_LINE_BMAX, wcline, wc - wcline);
	  line[sz++] = '\n';
	  line[sz] = 0;
	  if (write(fdes, line, sz) != sz)
	    ecount++;
	}
      } else
	ecount++;
    } else
      ecount += writeTD(tn->tn_tree, gram, fdes);
  }
  return(ecount);
}

int	
_Rktclose(dm, file, gram)
     struct DM	*dm;
     char	*file;
     struct RkKxGram	*gram;
{
  struct DF	*df = dm->dm_file;
  struct TD	*xdm = (struct TD *)dm->dm_td;
  int		fdes;
  int		ecount;
    
  _RkKillCache(dm);
  if (dm->dm_flags & DM_UPDATED) {
    char	backup[RK_PATH_BMAX];
    char	*p = rindex(file, '/');
    
    if (p) {
      strcpy(backup, file);
      p++;
      backup[p - file] = '#';
      strcpy(&backup[p-file + 1], p);
    };
    
    fdes = creat(backup, (unsigned)0666);
    ecount = 0;
    if (fdes >= 0) {
      char	header[RK_LINE_BMAX];
      char	whattime[RK_LINE_BMAX];
      TIME_T	tloc;
      int	n;
      
      tloc = time(0);
      strcpy(whattime, ctime(&tloc));
      whattime[strlen(whattime)-1] = 0;
      (void)strcpy(header, "#*DIC ");
      (void)strcat(header, dm->dm_nickname);
      (void)strcat(header, " [");
      (void)strcat(header, whattime);
      (void)strcat(header, "] \n");
      n = strlen(header);
      if (write(fdes, header, n) != n)
	ecount++;
      ecount += writeTD(xdm, gram, fdes);
      (void)close(fdes);
    } else
      ecount++;

    if (!ecount) {
#ifdef HAVE_RENAME
      rename(backup, file);
#else
      unlink(file);
      if (link(backup, file) == 0) {
	unlink(backup);
      }
#endif
    }
    dm->dm_flags &= ~DM_UPDATED;
  };
  freeTD((struct TD *)xdm);
  --df->df_rcount;
  return 0;
}

int
_Rktsearch(cx, dm, key, n, nread, maxcache, cf)
     struct RkContext	*cx;
     struct DM		*dm;
     Wchar		*key;
     int		n;
     struct nread	*nread;
     int		maxcache;
     int		*cf;
{
  Wchar		uniqAlnum();
  struct TD	*xdm = (struct TD *)dm->dm_td;
  int		nc = 0;
  int		i, j;
    
  *cf = 0;
  for (j = 0; j < n;) {
    Wchar	k = uniqAlnum(key[j++]);
    struct TN	*tn;
    
    tn = xdm->td_node;
    for (i = 0; i < xdm->td_n && tn->tn_key <= k; i++, tn++) {
      if (k == tn->tn_key) {
	if (IsWordNode(tn)) {
	  Wrec	*w;
	  int	l;

	  w = tn->tn_word->word;
	  l = (*w >> 1) & 0x3f;
	  if (*w & 0x80)
	    w += 2;
	  w += 2;
	  if (_RkEql(key, w, l)) {
	    if (l > n)
	      (*cf)++;
	    else if (nc < maxcache) {
	      nread[nc].cache = _RkReadCache(dm, (long)tn->tn_word);
	      if (nread[nc].cache) {
		nread[nc].nk = l;
		nread[nc].csn = 0;
		nread[nc].offset = 0;
		nc++;
	      } else
		(*cf)++;
	    } else 
	      (*cf)++;
	  }
	  return nc;
	} else {
	  struct TD	*ct = tn->tn_tree;
	  struct TN	*n0 = &ct->td_node[0];
	  
	  if (ct->td_n && !n0->tn_key) {
	    unsigned char	*w;
	    int			l;
	    w = n0->tn_word->word;
	    l = (*w >> 1) & 0x3f;
	    if (*w & 0x80)
	      w += 2;
	    w += 2;
	    if (_RkEql(key, w, l)) {
	      if (l > n)
		(*cf)++;
	      else if (nc < maxcache) {
		nread[nc].cache = _RkReadCache(dm, (long)n0->tn_word);
		if (nread[nc].cache) {
		  nread[nc].nk = l;
		  nread[nc].csn = 0;
		  nread[nc].offset = 0;
		  nc++;
		} else
		  (*cf)++;
	      } else
		(*cf)++;
	    }
	  }
	  xdm = ct;
	  goto	cont;
	}
      }
    }
    break;
  cont:
    ;
  }
  if (n <= 0)
    cx->poss_cont++;
  return nc;
}

/*
 * IO
 */
/*ARGSUSED*/
int	
_Rktio(dm, cp, io)
     struct DM		*dm;
     struct ncache	*cp;
     int		io;
{
  if (io == 0) {
    cp->nc_word = ((struct TW *)cp->nc_address)->word;
    cp->nc_flags |= NC_NHEAP;
  }
  return 0;
}

/*
 * CTL
 */
int	
_Rktctl(dm, qm, what, arg, gram)
     struct DM	*dm;
     struct DM	*qm; /* no use : dummy*/
     int	what;
     Wchar	*arg;
     struct RkKxGram	*gram;
/* ARGSUSED */
{
  struct TD	*xdm = (struct TD *)dm->dm_td;
  int		status = 0;
  struct TW	tw;
  Wrec		wrec[2048];
  unsigned	lucks[2];
  
  switch(what) {
  case DST_DoDefine:
    if ((dm->dm_flags & (DM_WRITABLE | DM_WRITEOK)) !=
	(DM_WRITABLE | DM_WRITEOK)) /* !(writable and write ok) */
      status = -1;
    else if (!RkParseOWrec(gram, arg, wrec, RkNumber(wrec), lucks))
      status = -1;
    else {
      tw.word = wrec;
      status = defineTD(dm, xdm, 0, &tw, RkNumber(wrec)) ? 0 : -1;
      dm->dm_flags |= DM_UPDATED;
    }
    break;
  case DST_DoDelete:
    if ((dm->dm_flags & (DM_WRITABLE | DM_WRITEOK)) !=
	(DM_WRITABLE | DM_WRITEOK)) /* !(writable and write ok) */
      status = -1;
    else if (!RkParseOWrec(gram, arg, wrec, RkNumber(wrec), lucks))
      status = -1;
    else if (deleteTD(dm, &xdm, 0, wrec)) {
      xdm = newTD();
      dm->dm_td = (pointer)xdm;
    }
    dm->dm_flags |= DM_UPDATED;
    break;
  }
  return status;
}

int
_Rktsync(cx, dm, qm)
     struct RkContext *cx;
     struct DM	*dm, *qm;
/* ARGSUSED */
{
  struct RkKxGram  *gram = cx->gram->gramdic;
  struct DF	*df_p;
  struct DD     *dd_p;
  struct TD	*xdm = (struct TD *)dm->dm_td;
  int		fdes;
  int		ecount;
  char	        *file;

  df_p = dm->dm_file;
  dd_p = df_p->df_direct;
  file  = _RkCreatePath(dd_p, df_p->df_link);
  if (file) {
    if (dm->dm_flags & DM_UPDATED) {
      char	backup[RK_PATH_BMAX];
      char	*p = rindex(file, '/');
      
      if (p) {
	strcpy(backup, file);
	p++;
	backup[p - file] = '#';
	strcpy(&backup[p-file + 1], p);
      };

      fdes = creat(backup, (unsigned)0666);
      ecount = 0;
      if (fdes >= 0) {
	char	header[RK_LINE_BMAX];
	char	whattime[RK_LINE_BMAX];
	TIME_T	tloc;
	int	n;
	
	tloc = time(0);
	strcpy(whattime, ctime(&tloc));
	whattime[strlen(whattime)-1] = 0;
	(void)strcpy(header, "#*DIC ");
	(void)strcat(header, dm->dm_nickname);
	(void)strcat(header, " [");
	(void)strcat(header, whattime);
	(void)strcat(header, "] \n");
	n = strlen(header);
	if (write(fdes, header, n) != n)
	  ecount++;
	ecount += writeTD(xdm, gram, fdes);
	(void)close(fdes);
      } else
	ecount++;
      
      if (!ecount) {
#ifdef HAVE_RENAME
	rename(backup, file);
#else
	unlink(file);
	if (link(backup, file) == 0) {
	  unlink(backup);
	}
#endif
	dm->dm_flags &= ~DM_UPDATED;
      } else {
	(void)free(file);
	return (-1);
      }
    };
    (void)free(file);
    return (0);
  } 
  return (-1);
}
