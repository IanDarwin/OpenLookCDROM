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

/*
static char rcsid[]="$Id: RKncache.c,v 1.13 1994/06/01 06:54:25 misao Exp $";
*/

#include	"RKintern.h"

#define	NCHASH		101
#define	hash(x)		((int)((x)%NCHASH))

static struct ncache	Nchash[NCHASH];
static struct ncache	Ncfree;

#define ainserttop(p) { \
(p)->nc_anext = Ncfree.nc_anext; (p)->nc_aprev = &Ncfree; \
Ncfree.nc_anext->nc_aprev = (p); Ncfree.nc_anext = (p); \
}

#define ainsertbottom(p) { \
(p)->nc_anext = &Ncfree; (p)->nc_aprev = Ncfree.nc_aprev; \
Ncfree.nc_aprev->nc_anext = (p); Ncfree.nc_aprev = (p); \
}

#define	aremove(p)	{\
(p)->nc_anext->nc_aprev = (p)->nc_aprev; \
(p)->nc_aprev->nc_anext = (p)->nc_anext; (p)->nc_anext = (p)->nc_aprev = (p);\
}

#define	hremove(p)	{\
(p)->nc_hnext->nc_hprev = (p)->nc_hprev; \
(p)->nc_hprev->nc_hnext = (p)->nc_hnext; (p)->nc_hnext = (p)->nc_hprev = (p);\
}

int	
_RkInitializeCache(size)
     int	size;
{
  register struct RkParam	*sx = &SX;
  int				i;

  sx->maxcache = size;
  if (!(sx->cache = (struct ncache *)calloc((unsigned)size, sizeof(struct ncache))))
    return -1;
  for (i = 0; i < size ; i++) {
    sx->cache[i].nc_anext = &sx->cache[i+1];
    sx->cache[i].nc_aprev = &sx->cache[i-1];
    sx->cache[i].nc_hnext = sx->cache[i].nc_hprev = &sx->cache[i];
    sx->cache[i].nc_count = 0;
  };
  Ncfree.nc_anext = &sx->cache[0];
  sx->cache[sx->maxcache - 1].nc_anext = &Ncfree;
  Ncfree.nc_aprev = &sx->cache[sx->maxcache - 1];
  sx->cache[0].nc_aprev = &Ncfree;
  for (i = 0; i < NCHASH; i++) 
    Nchash[i].nc_hnext = Nchash[i].nc_hprev = &Nchash[i];
  return 0;
}

void
_RkFinalizeCache()
{
  register struct RkParam	*sx = &SX;
  
  if (sx->cache) 
    (void)free((char *)sx->cache);
  sx->cache = (struct ncache *)0;
}

static
int
flushCache(dm, cache)
     struct DM		*dm;
     struct ncache	*cache;
{
  if (cache->nc_word) {
    if (dm && (cache->nc_flags & NC_DIRTY)) {
      DST_WRITE(dm, cache);
    };
    cache->nc_flags &= ~NC_DIRTY;
    return 0;
  };
  return -1;
}

static
struct ncache	*newCache(ndm, address)
     register struct DM		*ndm;
     register long		address;
{
  register struct ncache	*new;

  if ((new = Ncfree.nc_anext) != &Ncfree) {
    (void)flushCache(new->nc_dic, new);
    aremove(new);
    hremove(new);
    new->nc_dic = ndm;
    new->nc_word = (unsigned char *)0;
    new->nc_flags  = 0;
    new->nc_address = address;
    new->nc_count = 0;
    return(new);
  };
  return (struct ncache *)0;
}

int
_RkRelease()
{
  register struct ncache	*new;

  for (new = Ncfree.nc_anext; new != &Ncfree; new = new->nc_anext) {
    if (!new->nc_word || (new->nc_flags & NC_NHEAP))
      continue;
    (void)flushCache(new->nc_dic, new);
    hremove(new);
    new->nc_dic = (struct DM *)0;
    new->nc_flags  = 0;
    new->nc_word = (unsigned char *)0;
    new->nc_address = 0;
    new->nc_count = 0;
    return 1;
  };
  return 0;
}

/*
int
_RkEnrefCache(cache)
     struct ncache *cache;
{
  static int count = 0;
  fprintf(stderr, "_RkEnrefCache(0x%08x), %d\n", cache, ++count);
  return(cache->nc_count++);
}
*/

void
_RkDerefCache(cache)
     struct ncache *cache;
{
  struct DM	*dm = cache->nc_dic;
/*
  static int count = 0;
  fprintf(stderr, "_RkDeref(0x%08x), %d\n", cache, ++count);
*/

  if (cache->nc_count <= 0) {
    _Rkpanic("wrong cache count %s %d#%d",
	     dm ? dm->dm_dicname : "-", cache->nc_address, cache->nc_count);
  };
  if (--cache->nc_count) {
    aremove(cache);
    if (cache->nc_flags & NC_ERROR) {
      ainserttop(cache);
    } else {
      ainsertbottom(cache);
    };
  };
  return;
}

void	
_RkPurgeCache(cache)
     struct ncache	*cache;
{
  hremove(cache);
  aremove(cache);
  ainserttop(cache);
}

void	
_RkKillCache(dm)
     struct DM	*dm;
{
  struct ncache		*cache;
  int			i;

  for (i = 0, cache = SX.cache; i < SX.maxcache; i++, cache++) {
    if (dm == cache->nc_dic) {
      (void)flushCache(dm, cache);
      _RkPurgeCache(cache);
    };
  };
}

struct ncache	*
_RkFindCache(dm, addr)
     struct DM	*dm;
     long	addr;
{
  register struct ncache	*head, *cache;

  head = &Nchash[hash(addr)];
  for (cache = head->nc_hnext; cache != head; cache = cache->nc_hnext)  
    if (cache->nc_dic == dm && cache->nc_address == addr) 
      return cache;
  return (struct ncache *)0;
}

void
_RkRehashCache(cache, addr)
     struct ncache	*cache;
     long		addr;
{
  struct ncache	*head;

  if ((head = &Nchash[hash(addr)]) != &Nchash[hash(cache->nc_address)]) {
    hremove(cache);
    cache->nc_hnext = head->nc_hnext;
    cache->nc_hprev = head;
    head->nc_hnext->nc_hprev = cache;
    head->nc_hnext = cache;
  };
  cache->nc_address = addr;
}

struct ncache	*
_RkReadCache(dm, addr)
     struct DM	*dm;
     long	addr;
{
  register struct ncache	*head, *cache;

  head = &Nchash[hash(addr)];
  for (cache = head->nc_hnext; cache != head; cache = cache->nc_hnext) {
    if (cache->nc_dic == dm && cache->nc_address == addr) {
      aremove(cache);
      if (cache != head->nc_hnext) {
	hremove(cache);
	cache->nc_hnext = head->nc_hnext;
	cache->nc_hprev = head;
	head->nc_hnext->nc_hprev = cache;
	head->nc_hnext = cache;
      }
      _RkEnrefCache(cache);
      return(cache);
    };
  };
  cache = newCache(dm, addr);
  if (cache) {
    if (DST_READ(dm, cache)) {
      ainserttop(cache);
      return (struct ncache *)0;
    } else {
      cache->nc_hnext = head->nc_hnext;
      cache->nc_hprev = head;
      head->nc_hnext->nc_hprev = cache;
      head->nc_hnext = cache;
      _RkEnrefCache(cache);
      return(cache);
    };
  } else {
    return (struct ncache *)0;
  };
}
