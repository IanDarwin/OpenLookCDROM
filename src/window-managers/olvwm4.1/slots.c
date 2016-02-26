/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)slots.c	1.4 olvwm version 07 Jan 1994"
#endif

/*
 * Based on
#ident	"@(#)slots.c	26.8	91/09/14 SMI"
 *
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "st.h"
#include "olwm.h"
#include "win.h"
#include "globals.h"
#include "list.h"
#include "slots.h"

#define PLUSINFINITY 0xfffffff	/* REMIND fix this for architecture-independence */

/***************************************************************************
* Local functions
***************************************************************************/

#define ROUNDUP(n,d) (((n)+(d)-1)/(d))
/* #define ROUND(n,d) ((0.5+((float)(n)/(float)(d)))*(int)(d)) */
#define ROUND(n,d) (((((n)%(d))>((d)/2))?(((n)/(d))+1):((n)/(d)))*(d))

#define SLOTOFFSET(ig,ma,mi) 	(((ma))*(ig)->mislots+(mi))
#define AOCCUPANCY(ig,ma,mi) 	((ig->occupancy+SLOTOFFSET((ig),(ma),(mi))))
#define OCCUPANCY(ig,ma,mi) 	(*(AOCCUPANCY((ig),(ma),(mi))))

#define ISVERT(so) (((so)==SOBottomToTop)||((so)==SOTopToBottom))
#define ISHORZ(so) (((so)==SOLeftToRight)||((so)==SORightToLeft))
#define ISDECREASING(so) (((so)==SORightToLeft)||((so)==SOBottomToTop))

#ifdef NOT
static char *
sotochar(so)
SlotOrder so;
{
	if (so == SOTopToBottom)
		return ("SOTopToBottom");
	if (so == SOBottomToTop)
		return ("SOBottomToTop");
	if (so == SOLeftToRight)
		return ("SOLeftToRight");
	if (so == SORightToLeft)
		return ("SORightToLeft");
	return("SOIllegal");
}
#endif

#ifdef DEBUG
/* dumpocc -- print the occupancy count table to stderr
 */
static void
dumpocc(iconGrid)
IconGrid	*iconGrid;
{
	int ima, imi;

	fprintf(stderr,"dump of occupancy table:\n");
	for (ima = 0; ima<iconGrid->maslots; ima++)
	    for (imi = 0; imi<iconGrid->mislots; imi++)
		if (OCCUPANCY(iconGrid,ima,imi)!=0)
		    fprintf(stderr,"%x: occ[%d,%d]/[%d] = %d\n",
			    AOCCUPANCY(iconGrid,ima,imi),ima,imi,
			    SLOTOFFSET(iconGrid,ima,imi),
			    OCCUPANCY(iconGrid,ima,imi));
	fprintf(stderr,"end of dump of occupancy table\n");
}
#endif


/* setMajorMinor -- set two output variables from two input variables,
 * based on the major/minor settings.
 */
static void
setMajorMinor(iconGrid,r,c,pma,pmi)
IconGrid *iconGrid;
int r, c, *pma, *pmi;
{
	if (ISVERT(iconGrid->SOmajor))
	{
	    *pma = r;
	    *pmi = c;
	}
	else
	{
	    *pma = c;
	    *pmi = r;
	}
}

#ifdef NOT
/* setAbsolute -- set two output variables from two input variables, based
 * on major/minor settings; this is the inverse of setMajorMinor.
 */
static void
setAbsolute(iconGrid,ma,mi,pr,pc)
IconGrid *iconGrid;
int ma, mi;
int *pr, *pc;
{
    if (ISVERT(iconGrid->SOmajor))
    {
	*pr = ma;
	*pc = mi;
    }
    else
    {
	*pc = ma;
	*pr = mi;
    }
}
#endif

/* incrRegion -- increment the occupancy count of a region
 */
static void
incrRegion(iconGrid,slot,val)
IconGrid *iconGrid;
IconSlot *slot;
int val;
{
	int ii,jj;

	for (ii = slot->ma; 
	    (ii>=0)&&(ii<iconGrid->maslots)&&(ii<slot->ma+slot->maw); ii++)
	    for (jj = slot->mi; 
		(jj>=0)&&(jj<iconGrid->mislots)&&(jj<slot->mi+slot->miw); jj++)
	    {
		if ((OCCUPANCY(iconGrid,ii,jj) += val) < iconGrid->minoccupancy)
		    iconGrid->minoccupancy = OCCUPANCY(iconGrid,ii,jj);
	    }
}

/* findMinMaxRgn -- find the minimum and maximum occupancies of a region
 */
static void
findMinMaxRgn(iconGrid,ma, mi, nmajor, nminor, pminrgn, pmaxrgn)
IconGrid *iconGrid;
int ma, mi, nmajor, nminor, *pminrgn, *pmaxrgn;
{
	int min, max;
	int ii, jj;
	int occ;

	min = PLUSINFINITY;
	max = -1;

	for (ii=ma; ii<ma+nmajor; ii++)
	    for (jj = mi; jj<mi+nminor; jj++)
	    {
		occ = OCCUPANCY(iconGrid,ii,jj);
		if (occ < min)
		    min = occ;
		if (occ > max)
		    max = occ;
	    }
	*pminrgn = min;
	*pmaxrgn = max;
}

static IconSlot *
incrDynamicSlots(iconGrid,nmajor, nminor)
IconGrid *iconGrid;
int nmajor, nminor;
{
    	int minoccrgn; /* smallest occupancy found for a region of given size */
    	int minfound, minfoundrgn; 	/* smallest occupancy found for one slot
				         * and a region respectively; used to reset
				         * search values */
    	int ma, mi;	/* indices in major and minor directions */
    	int maxrgn, minrgn;	  /* max and min occupancies in a region */
	IconSlot *slot = MemNew(IconSlot);

	minoccrgn = iconGrid->minoccupancy;
	slot->positioned = False;

	for (;;)	/* will actually only iterate twice */
	{
	    minfound = minfoundrgn = PLUSINFINITY;
	    for (ma = 0; (ma <= iconGrid->maslots-nmajor); ma++)
	    {
	        for (mi = 0; (mi <= iconGrid->mislots-nminor); mi++)
	        {
		    findMinMaxRgn(iconGrid,ma,mi,nmajor,nminor,&minrgn,&maxrgn);
		    if (minrgn < minfound)
			minfound = minrgn;
		    if (maxrgn < minfoundrgn)
			minfoundrgn = maxrgn;
		    if (maxrgn <= minoccrgn)
		    {
			slot->ma = ma;
			slot->mi = mi;
			slot->maw = nmajor;
			slot->miw = nminor;
			incrRegion(iconGrid,slot,1);
			return slot;
		    }
	        }
	    }
	    /* no regions available with occupancy <= known minimum */
	    if (minfound > iconGrid->minoccupancy)  /* increase slot minimum */
		iconGrid->minoccupancy = minfound;
	    minoccrgn = minfoundrgn;		/* increase local region min */
	}
}

/* setIconPos -- given a slot, position the icon frame window accordingly.
 */
static void 
setIconPos(win, slot)
WinIconFrame *win;
IconSlot *slot;
{
    int x,y;
    IconGrid *iconGrid = win->core.client->scrInfo->iconGrid;

    if (iconGrid->SOmajor == SOTopToBottom)
    {
	y = ICON_GRID_HEIGHT*slot->ma;
	if (iconGrid->SOminor == SOLeftToRight)
	{
	    x = ICON_GRID_WIDTH*slot->mi;
	}
	else
	{
	    x = iconGrid->pixhoriz - slot->mi*ICON_GRID_WIDTH - win->core.width;
	}
    }
    else if (iconGrid->SOmajor == SOBottomToTop)
    {
	y = iconGrid->pixvert - ICON_GRID_HEIGHT*slot->ma - win->core.height;
	if (iconGrid->SOminor == SOLeftToRight)
	{
	    x = ICON_GRID_WIDTH*slot->mi;
	}
	else
	{
	    x = iconGrid->pixhoriz - slot->mi*ICON_GRID_WIDTH - win->core.width;
	}
    }
    else if (iconGrid->SOmajor == SOLeftToRight)
    {
	x = ICON_GRID_WIDTH*slot->ma;
	if (iconGrid->SOminor == SOTopToBottom)
	{
	    y = ICON_GRID_HEIGHT*slot->mi;
	}
	else
	{
	    y = iconGrid->pixvert - slot->mi*ICON_GRID_HEIGHT - win->core.height;
	}
    }
    else if (iconGrid->SOmajor == SORightToLeft)
    {
	x = iconGrid->pixhoriz - ICON_GRID_WIDTH*slot->ma - win->core.width;
	if (iconGrid->SOminor == SOTopToBottom)
	{
	    y = ICON_GRID_HEIGHT*slot->mi;
	}
	else
	{
	    y = iconGrid->pixvert - slot->mi*ICON_GRID_HEIGHT - win->core.height;
	}
    }
    (WinFunc(win,core.newposfunc))(win, x, y);
}

/* slotsIntersect -- check to see if the two slots intersect.
 */
static Bool
slotsIntersect(slot1,slot2)
IconSlot *slot1, *slot2;
{
    if ((slot1->ma > (slot2->ma + slot2->maw)) ||
	(slot1->mi > (slot2->mi + slot2->miw)) ||
	(slot2->ma > (slot1->ma + slot1->maw)) ||
	(slot2->mi > (slot1->mi + slot1->miw)))
	return False;
    else
	return True;
}

/* 
 * bounceOverlappingIcons -- given an icon and its slot(s), move any icons that
 *                           in those slots to new positions.
 */
static void 
bounceOverlappingIcons(win, slot)
WinIconFrame *win;
IconSlot *slot;
{
    IconGrid     *iconGrid = win->core.client->scrInfo->iconGrid;
    List         *l = iconGrid->iconList;
    List         *move = NULL_LIST;
    WinIconFrame *w;

    /* First, make a list of the icons that need to move
     * Must do this in two steps, since the iconList will change when we
     * start moving icons
     */
    for (w = ListEnum(&l); w != NULL; w = ListEnum(&l))
    {
	/* if the slots overlap, move to new one */
	if (w != win && slotsIntersect(w->iconslot, slot))
	    move = ListCons(w,move);
    }

    l = move;
    for (w = ListEnum(&l); w != NULL; w = ListEnum(&l))
    {
	/* free its slot */
	SlotFree(w);

	/* set this as auto-positioned now */
	w->fManuallyPositioned = False;

	/* allocate a new slot for it */
	SlotAlloc(w, False, GRV.FSnapToGrid);
	(WinFunc(w,core.setconfigfunc))(w->core.client->dpy, w);
    }
    ListDestroy(move);
}

/* incrPositionedSlots -- an icon has been explicitly positioned; create an
 * iconslot record and increment the occupancy count of the affected slots.
 */
static IconSlot *
incrPositionedSlots(iconGrid,x,y,w,h,snaptogrid)
IconGrid *iconGrid;
int x,y,w,h;
Bool snaptogrid;
{
	IconSlot *slot = MemNew(IconSlot);
#define ROUNDPOS(v,w) (snaptogrid?ROUND((v),(w)):(v))
#define ROUNDIF(v,w) (ROUNDPOS(v,w)/(w))
#define ROUNDDIM(p,s,w) (ROUNDUP(((ROUNDPOS(p,w))%(w))+(s),(w)))

	if (iconGrid->SOmajor == SOTopToBottom)
	{
	    slot->ma = ROUNDIF(y,ICON_GRID_HEIGHT);
	    slot->maw = ROUNDDIM(y,h,ICON_GRID_HEIGHT);
	    if (iconGrid->SOminor == SOLeftToRight)
	    {
		slot->mi = ROUNDIF(x,ICON_GRID_WIDTH);
	        slot->miw = ROUNDDIM(x,w,ICON_GRID_WIDTH);
	    }
	    else
	    {
		slot->mi = ROUNDIF(iconGrid->pixhoriz-(x+w),ICON_GRID_WIDTH);
	        slot->miw = ROUNDDIM(iconGrid->pixhoriz-(x+w),w,ICON_GRID_WIDTH);
	    }
	}
	else if (iconGrid->SOmajor == SOBottomToTop)
	{
	    slot->ma = ROUNDIF(iconGrid->pixvert-(y+h),ICON_GRID_HEIGHT);
	    slot->maw = ROUNDDIM(iconGrid->pixvert-(y+h),h,ICON_GRID_HEIGHT);
	    slot->miw = w;
	    if (iconGrid->SOminor == SOLeftToRight)
	    {
		slot->mi = ROUNDIF(x,ICON_GRID_WIDTH);
	        slot->miw = ROUNDDIM(x,w,ICON_GRID_WIDTH);
	    }
	    else
	    {
		slot->mi = ROUNDIF(iconGrid->pixhoriz-(x+w),ICON_GRID_WIDTH);
	        slot->miw = ROUNDDIM(iconGrid->pixhoriz-(x+w),w,ICON_GRID_WIDTH);
	    }
	}
	else if (iconGrid->SOmajor == SOLeftToRight)
	{
	    slot->ma = ROUNDIF(x,ICON_GRID_WIDTH);
	    slot->maw = ROUNDDIM(x,w,ICON_GRID_WIDTH);
	    if (iconGrid->SOminor == SOTopToBottom)
	    {
		slot->mi = ROUNDIF(y,ICON_GRID_HEIGHT);
		slot->miw = ROUNDDIM(y,h,ICON_GRID_HEIGHT);
	    }
	    else
	    {
		slot->mi = ROUNDIF(iconGrid->pixvert-(y+h),ICON_GRID_HEIGHT);
		slot->miw = ROUNDDIM(iconGrid->pixvert-(y+h),h,ICON_GRID_HEIGHT);
	    }
	}
	else if (iconGrid->SOmajor == SORightToLeft)
	{
	    slot->ma = ROUNDIF(iconGrid->pixhoriz-(x+w),ICON_GRID_WIDTH);
	    slot->maw = ROUNDDIM(iconGrid->pixhoriz-(x+w),w,ICON_GRID_WIDTH);
	    if (iconGrid->SOminor == SOTopToBottom)
	    {
		slot->mi = ROUNDIF(y,ICON_GRID_HEIGHT);
		slot->miw = ROUNDDIM(y,h,ICON_GRID_HEIGHT);
	    }
	    else
	    {
		slot->mi = ROUNDIF(iconGrid->pixvert-(y+h),ICON_GRID_HEIGHT);
		slot->miw = ROUNDDIM(iconGrid->pixvert-(y+h),h,ICON_GRID_HEIGHT);
	    }
	}
	if (slot->ma < 0) slot->ma = 0;
	if (slot->ma >= iconGrid->maslots) slot->ma = iconGrid->maslots-1;
	if (slot->mi < 0) slot->mi = 0;
	if (slot->mi >= iconGrid->mislots) slot->mi = iconGrid->mislots-1;
	slot->positioned = True;
	incrRegion(iconGrid,slot,1);
	return slot;
}


/***************************************************************************
* Global functions
***************************************************************************/

/*
 * SlotInit -- 
 */
IconGrid *
SlotInit(dpy,screenno)
Display *dpy;
int	screenno;
{
	IconGrid	*iconGrid;
	int		occLen;

	iconGrid = MemNew(IconGrid);
	iconGrid->iconList = NULL_LIST;
	iconGrid->pixhoriz = DisplayWidth(dpy,screenno);
	iconGrid->pixvert = DisplayHeight(dpy,screenno);
	iconGrid->slotshoriz = iconGrid->pixhoriz/ICON_GRID_WIDTH;
	iconGrid->slotsvert = iconGrid->pixvert/ICON_GRID_HEIGHT;
	occLen = iconGrid->slotshoriz * iconGrid->slotsvert * sizeof(int);
	iconGrid->occupancy = MemAlloc(occLen);
	iconGrid->minoccupancy = 0;
	
	SlotSetLocations(dpy,iconGrid);
	
	return iconGrid;
}

/* SlotAlloc - given a sized and possibly positioned icon window, allocate 
 * the appropriate slots for it.  If the window is positioned, 
 * True should be passed for the second parameter, and the x,y
 * position will be honoured.  If the window is not positioned, it
 * will be positioned by this function to the appropriate slots(s).
 * If snaptogrid is true and positioned is true, the given position is
 * modified so that the upper left corner of the icon is at the closest
 * icon grid point.  Modifies the icon structure to assign it the icon
 * slot.
 * Returns an IconSlot pointer if successful; NULL otherwise.
 *
 * If the window is not positioned, the x and y coordinates are used as
 * offsets to the position icon; this allows particular icons to be located`
 * in a particular logical screen within the virtual desktop.
 */
struct _iconSlot * 
SlotAlloc(winicon, positioned, snaptogrid)
WinIconFrame *winicon;
Bool positioned;
Bool snaptogrid;
{
    int nhoriz, nvert;	/* number of slots occupied, horizontally & vertically */
    int nmajor, nminor;	/* number of slots occupied, in major & minor directions */
    IconSlot *slot;
    IconGrid *iconGrid = winicon->core.client->scrInfo->iconGrid;
    int		scr_x, scr_y;
    int		dw, dh;

    dw = DisplayWidth(winicon->core.client->dpy, winicon->core.client->screen);
    dh = DisplayHeight(winicon->core.client->dpy, winicon->core.client->screen);
    scr_x = (winicon->core.x + ICON_HORZBORDER) / dw;
    scr_y = (winicon->core.y + ICON_VERTBORDER) / dh;
    if (winicon->core.x < -ICON_HORZBORDER)
	scr_x--;
    if (winicon->core.y < -ICON_VERTBORDER)
	scr_y--;
    winicon->core.x -= scr_x * dw;
    winicon->core.y -= scr_y * dh;

    iconGrid->iconList = ListCons(winicon, iconGrid->iconList);

    if (positioned)
    {
	slot = incrPositionedSlots(iconGrid, winicon->core.x, winicon->core.y,
		winicon->core.width, winicon->core.height, snaptogrid);
	winicon->iconslot = slot;
	if (snaptogrid)
	    setIconPos(winicon, slot);
	else
	    (WinFunc(winicon,core.newposfunc))(winicon,winicon->core.x, winicon->core.y);
	/* any icons that are covered by this icon shall be bounced
	   to new, open icon slots.  However, don't bother trying if there 
	   are no open spots! */
	if (GRV.UniqueIconSlots && (iconGrid->minoccupancy == 0))
	    bounceOverlappingIcons(winicon, slot);
    }
    else
    {
        nhoriz = ROUNDUP(winicon->core.width, ICON_GRID_WIDTH);
        nvert = ROUNDUP(winicon->core.height, ICON_GRID_HEIGHT);
	setMajorMinor(iconGrid, nvert, nhoriz, &nmajor, &nminor);
	slot = incrDynamicSlots(iconGrid, nmajor, nminor);
	winicon->iconslot = slot;
	setIconPos(winicon,slot);
    }
    if (scr_x || scr_y) {
        (WinFunc(winicon,core.newposfunc))(winicon,
					   winicon->core.x + scr_x * dw,
					   winicon->core.y + scr_y * dh);
    }
#ifdef DEBUG
    dumpocc(iconGrid);
#endif
    return slot;
}

/* SlotFree -- An icon is going away, so its references to slots should also go
 * away.  Returns True iff the free was successful.
 * Note that if this code is changed, the similar code in SlotSetLocations
 * may need to be changed also.
 */
SlotFree(winicon)
WinIconFrame *winicon;
{
	IconGrid	*iconGrid = winicon->core.client->scrInfo->iconGrid;

	if (winicon->iconslot == NULL)
	{
		return;
	}

	incrRegion(iconGrid,winicon->iconslot,-1);
	MemFree(winicon->iconslot);
	winicon->iconslot = NULL;

	iconGrid->iconList =
		ListDestroyCellByValue(winicon, iconGrid->iconList);
#ifdef DEBUG
	dumpocc(iconGrid);
#endif
}

/* sets the order in which slots are allocated for icons which are
 * not explicitly positioned.  The order is gotten from the global
 * resource vector.
 * For example, the AlongBottom order is expressed as
 * major BottomToTop, minor LeftToRight.  The major and minor orders
 * cannot be both vertical or horizontal.  Any icons which were 
 * automatically positioned are repositioned to equivalent positions
 * in the new order.
 */
/*ARGSUSED*/
SlotSetLocations(dpy,iconGrid)
Display *dpy;
IconGrid *iconGrid;
{
	List *lauto, *lpos;
	List **l;
	WinIconFrame *win;
	int ima, imi;

	/* set up the new order of things */
        switch (GRV.IconPlacement)
        {
        case AlongTop:
		iconGrid->SOmajor = SOTopToBottom;
		iconGrid->SOminor = SOLeftToRight;
                break;

        case AlongTopRL:
		iconGrid->SOmajor = SOTopToBottom;
		iconGrid->SOminor = SORightToLeft;
                break;

        case AlongBottom:
		iconGrid->SOmajor = SOBottomToTop;
		iconGrid->SOminor = SOLeftToRight;
                break;

        case AlongBottomRL:
		iconGrid->SOmajor = SOBottomToTop;
		iconGrid->SOminor = SORightToLeft;
                break;

        case AlongLeft:
		iconGrid->SOmajor = SOLeftToRight;
		iconGrid->SOminor = SOTopToBottom;
                break;

        case AlongLeftBT:
		iconGrid->SOmajor = SOLeftToRight;
		iconGrid->SOminor = SOBottomToTop;
                break;

        case AlongRight:
		iconGrid->SOmajor = SORightToLeft;
		iconGrid->SOminor = SOTopToBottom;
                break;

        case AlongRightBT:
		iconGrid->SOmajor = SORightToLeft;
		iconGrid->SOminor = SOBottomToTop;
                break;
        }

	setMajorMinor(iconGrid,iconGrid->slotsvert,iconGrid->slotshoriz,
			&(iconGrid->maslots),&(iconGrid->mislots));

	if (iconGrid->iconList == NULL_LIST)
	{
		for (ima = 0; ima<iconGrid->maslots; ima++)
		    for (imi = 0; imi<iconGrid->mislots; imi++)
			OCCUPANCY(iconGrid,ima,imi)=0;
		return;
	}

	/* partition existing icons */
	lauto = NULL_LIST;
	lpos = NULL_LIST;
	for (l = &iconGrid->iconList; *l != NULL; l = &((*l)->next))
	{
	    win = (*l)->value;
	    if (!win->iconslot->positioned)
	    {
		lauto = ListCons(win,lauto);
	    }
	    else
	    {
		lpos = ListCons(win,lpos);
	    }
	    /* this is the basics of a SlotFree */
	    MemFree(win->iconslot);
	    win->iconslot = NULL;
	}
	ListDestroy(iconGrid->iconList);
	iconGrid->iconList = NULL_LIST;

	for (ima = 0; ima<iconGrid->maslots; ima++)
	    for (imi = 0; imi<iconGrid->mislots; imi++)
		OCCUPANCY(iconGrid,ima,imi)=0;

	/* place positioned icons */
	for (l = &lpos; *l != NULL; l = &((*l)->next))
	{
	    win = (*l)->value;
	    SlotAlloc(win,True,GRV.FSnapToGrid);
	    (WinFunc(win,core.setconfigfunc))(win->core.client->dpy,win);
	}
	ListDestroy(lpos);

	/* place auto-positioned icons */
	for (l = &lauto; *l != NULL; l = &((*l)->next))
	{
	    win = (*l)->value;
	    /* only place iconified windows if FreeIconSlots is in effect */
	    if (win->core.client->wmState == IconicState || !GRV.FreeIconSlots){
	        SlotAlloc(win,False,GRV.FSnapToGrid);
	        (WinFunc(win,core.setconfigfunc))(win->core.client->dpy,win);
	    }
	}
	ListDestroy(lauto);
}
