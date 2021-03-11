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
 *
 * RKnheap.c
 */
#ifndef lint
static char rcsid[]="$Id: RKnheap.c,v 1.6 1994/06/01 06:54:29 misao Exp $";
#endif
/*LINTLIBRARY*/
/* nheap.c
 *	jisho yori yomikomareta tango record wo kannri suru heap
 * method:
 *	buddy hou wo saiyou
 *	bunkatu: 2 bunkatu
 */

#include	"RKintern.h"

#ifdef OLD
#define		MAXHBLK		256	/* heap de atukaeru saidai block chou */
#define		LOG2BLK		8	/* up to 256 byte */
#endif
#define		MAXHBLK		1024	/* heap de atukaeru saidai block chou */
#define		LOG2BLK		10	/* up to 256 byte */
#define		HEAPUNIT	16	/* >= sizeof(struct block) */

static int		Remain;
/* block
 *	douitu size no free block wo kanri suru header
 *	souhoukou list ni yori hyougenn
 */
struct block	{
	struct block	*bk_next;
	struct block	*bk_prev;
};

/* Heap
 *	jisho no tango jouhou ga tumerareru
 *	alloc sareta heap no sentou niha sono nagasa ga 1byte de settei sareru
 *	1bit = HEAPUNIT byte, 1byte = HEAPUNIT*8 byte no heap ni taiou suru
 *	SNX.heap
 *	SNX.maxheap
 * HAM
 *	heap no allocation joukyou wo kiroku suru
 *	heap ha HEAPUNIT byte wo tanni(unit) tosite tukau
 *	unit ga siyousareteireba, SNX.ham no taiou suru bit ga 1 to naru
 *	SNX.ham
 *	SNX.maxham
 */
/* Hpool
 *	heap de siyou sareteinai block wo kanri suru
 *	Hpool[i] niha nagasa 1<<i no block ga tunagareru
 *	sitagatte, _Rklog2(HEAPUNIT) ika ha siyou sinai 
 */
static struct block	Hpool[LOG2BLK + 1]; 
/* mask table
 *	SNX.ham no kousinn ni riyou saretu teisuu
 */
static unsigned char	smask[] =
{	0xff, 0x7f, 0x3f, 0x1f, 0x0f, 0x07, 0x03, 0x01,	};
static unsigned char	emask[] =
{	0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff, };

/* _Rklog2 */
static
int	_Rklog2(n)
unsigned	n;
{
    int	x;

    for ( x = 0; n > (1<<x); x++ );
    return x;
}

/* _RkHset
 *	[s, e) kan ni taiou suru SNX.ham wo siyouchuu to suru
 */
static
int	_RkHset(s, e)
unsigned char	 *s, *e;
{
    register struct RkParam	*sx = &SNX;
    int	sp, ep;
    int	sb, eb;
    unsigned char	sm, em;

    sp = (s - sx->heap)/HEAPUNIT;	ep = (e - sx->heap - 1)/HEAPUNIT;
    sb = sp/8;			eb = ep/8;
    sm = smask[sp&7];		em = emask[ep&7];
    if ( sb == eb ) 
	    sx->ham[sb] |= (sm&em);
    else {
	    int	i;

	    sx->ham[sb] |= sm;
	    for ( i = sb + 1; i < eb; i++ ) sx->ham[i] = -1;
	    sx->ham[eb] |= em;
    };
}

/* _RkHclr
 *	[s, e) kan ni taiou suru SNX.ham wo kaihou suru
 */
static
int	_RkHclr(s, e)
unsigned char	 *s, *e;
{
    register struct RkParam	*sx = &SNX;
    int	sp, ep;
    int	sb, eb;
    unsigned char	sm, em;

    sp = (s - sx->heap)/HEAPUNIT;	ep = (e - sx->heap - 1)/HEAPUNIT;
    sb = sp/8;			eb = ep/8;
    sm = smask[sp&7];		em = emask[ep&7];
    if ( sb == eb ) 
	    sx->ham[sb] &= ~(sm&em);
    else {
	    int	i;

	    sx->ham[sb] &= ~sm;
	    for ( i = sb + 1; i < eb; i++ ) sx->ham[i] = 0;
	    sx->ham[eb] &= ~em;
    };
}

/* _RkHtst
 *	sitei sareta block no siyoujoukyou wo hantei
 */
static
int	_RkHtst(s, e)
unsigned char	 *s, *e;
{
    register struct RkParam	*sx = &SNX;

    int	sp, ep;
    int	sb, eb;
    unsigned char	sm, em;

    sp = (s - sx->heap)/HEAPUNIT;	ep = (e - sx->heap - 1)/HEAPUNIT;
    sb = sp/8;			eb = ep/8;
    sm = smask[sp&7];		em = emask[ep&7];
    if ( sb == eb ) 
	    return(sx->ham[sb] & (sm&em));
    else {
	    int	i;

	    if ( sx->ham[sb] & sm ) return(1);
	    for ( i = sb + 1; i < eb; i++ )
		    if ( sx->ham[i] )
			    return(1);
	    return( sx->ham[eb] & em );
    };
}

/* _Rkenque
 *	Hpool he block wo tuika 
 */
static
_Rkenque(h, e)
struct block	*h, *e;
{
    e->bk_next = h;
    e->bk_prev = h->bk_prev;
    h->bk_prev->bk_next = e;
    h->bk_prev = e;
}

/* _Rkdeque
 *	block list kara hazusu 
 */
static
_Rkdeque(e)
struct block	*e;
{
    e->bk_prev->bk_next = e->bk_next;
    e->bk_next->bk_prev = e->bk_prev;
}
/* _RkInitializeHeap
 */
int
_RkInitializeHeap(size)
int	size;
{
    register struct RkParam	*sx = &SNX;
    int	i;

    sx->maxheap = (size/MAXHBLK + 1)*MAXHBLK;
    if ( !(sx->heap = (unsigned char *)malloc((unsigned int)sx->maxheap)) )
	return -1;
/* block list wo kuu ni suru */
    for ( i = 0; i <= LOG2BLK; i++ )
	Hpool[i].bk_next = Hpool[i].bk_prev = &Hpool[i];
/* MAXHBLK tanni ni free block wo tukuru */
    for ( i = 0; i < sx->maxheap; i += MAXHBLK )
	_Rkenque(&Hpool[LOG2BLK], (struct block *)(sx->heap + i));

/* allocation map wo kuu ni suru */
    sx->maxham = (sx->maxheap/HEAPUNIT)/8;
    if ( !(sx->ham = (unsigned char *)malloc((unsigned int)sx->maxham)) )
	return -1;
    _RkHclr(sx->heap, sx->heap + sx->maxheap);
    Remain = sx->maxheap;
    return 0;
}
/* _RkFinalizeHeap
 */
void
_RkFinalizeHeap()
{
    register struct RkParam	*sx = &SNX;

    if ( sx->heap )
	(void)free((char *)sx->heap);
    sx->heap = (unsigned char *)0;

    if ( sx->ham )
	(void)free((char *)sx->ham);
    sx->ham = (unsigned char *)0;
}

/* _RkNewHeap
 *	heap yori n byte wo kakunou dekiru ryouiki wo motomeru
 */
unsigned char	*
_RkNewHeap(n)
unsigned	n;
{
    int	pw, p;
    int	lg;

/* size ryouiki wo kakuho site, allocate sareru block no ookisa wo motomeru */
    n++;
    if ( n < HEAPUNIT )
        n = HEAPUNIT;
    do {
    /* n wo koeru saishou no 2 no beki wo motomeru */
	for ( pw = 1, lg = 0; n > pw; pw *= 2, lg++ );
    /* chiisai monokara block wo sagasu */
	for ( p = pw; p <= MAXHBLK; p *= 2, lg++ ) {
	    struct block	*b;
	    unsigned char		*h;

	    b = Hpool[lg].bk_next;
	    h = (unsigned char *)b;
	    if ( b != &Hpool[lg] ) {	/* block ga atta  */
		int	s;

	    /* list kara hazusu */
		_Rkdeque(b);
	    /* fuyou na bubun wo kaesu */
		for ( s = p; s > pw; s /= 2 ) 
		    _Rkenque(&Hpool[--lg],
			  (struct block *)((unsigned char *)b + s/2));
		_RkHset(h, h + pw);
		Remain -= pw;
		RkDebug("H#NEW %08x: %d", h, pw, 0);
		*h++ = _Rklog2(pw);
		return(h);
	    };
	};
    } while ( _RkRelease() );
/* heap exausted */
    return  (unsigned char *)0;
}

/* _RkFreeHeap
 */
void	
_RkFreeHeap(p)
unsigned char	*p;
{
    register struct RkParam	*sx = &SNX;
    unsigned	pw;
    int		off;
    unsigned char		*op = p;

/* size wo motomeru */
    pw = 1<<*(--p);
    RkDebug("H#FREE %08x: %d", p+1, pw, 0);
    off = p - sx->heap;
/* system errors */
    if ( off < 0 || off >= sx->maxheap ) 
	_Rkpanic("wrong heap addr %d@%d", pw, off, 0);
    if ( !_RkHtst(p, p + 1) ) 
	_Rkpanic("free busy block %d@%d", pw, off, 0);
/* kaihou suru */
    _RkHclr(p, p + pw);
    Remain += pw;
    for ( ; pw < MAXHBLK; pw *= 2 ) {
	    struct block	*b;
	    int	lg;
	    int	bd;

	    lg = _Rklog2(pw);
	    bd = (p - sx->heap)^pw;
    /* block ga shiyouchuu naraba yameru */
	    if ( _RkHtst(sx->heap + bd, sx->heap + (bd + pw)) )
		    goto	done;
	    for ( b = Hpool[lg].bk_next; b != &Hpool[lg]; b = b->bk_next ) {
		    if ( ((unsigned char *)b + pw) == p ) {/* b:front half */
			    _Rkdeque(b);
			    p = (unsigned char *)b;
			    goto	next;
		    }
		    else
		    if ( (unsigned char *)b == (p + pw) ) {/* b: rear half */
			    _Rkdeque(b);
			    goto	next;
		    };
	    };
    /* setuzoku dekiru mono ga nakatta */
	    break;
    next:;
    };
done:
    _Rkenque(&Hpool[_Rklog2(pw)], (struct block *)p);
}

void
_RkShowHeap()
{
    register struct RkParam	*sx = &SNX;
    int				i;

    printf("Heap Block %d bytes left\n", Remain);
    for ( i = 0; i < sx->maxham; i++ )
	    printf(" %02x", sx->ham[i]);
    printf("\n");

    for ( i = 0; i <= LOG2BLK; i++ ) {
	struct block *b;
	unsigned char	**h;

	printf("LG %d:", i);
	for ( b = Hpool[i].bk_next; b != &Hpool[i]; b = b->bk_next) {
		unsigned char	*h = (unsigned char *)b;

		printf("\t[%d, %d)", h - sx->heap, (h - sx->heap)+ (1<<i));
	};
	printf("\n");
    };
}
#ifdef RK_DEBUG
/*
main()
{
    unsigned char	*h;
    int	sz;
    unsigned char	*ptr[10000];
    int	i;

    _RkInitializeHeap(2000);
    _Rkshowmap();
    for ( i = 0; i < 50; i++ ) {
	int	sz;

	ptr[i] = _RkNewHeap(sz = rand()%64);
	printf("No %d SZ %d\n", i, sz);
	if ( !ptr[i] )
	    break;
	_Rkshowmap();
    };
    while ( --i >= 0 ) {
	_RkFreeHeap(ptr[i]);
	_Rkshowmap();
    };
	    
}
*/
#endif
