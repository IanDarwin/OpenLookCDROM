/*
 * $NCD$
 *
 * Copyright 1992 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of NCD. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  NCD. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * NCD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NCD.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, Network Computing Devices
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Converters.h>
#include <stdio.h>
#include <ctype.h>
#include "CardsP.h"

#define offset(field)	XtOffsetOf(CardsRec, cards.field)

#define COLOR_UNSET 10

static XtResource resources[] = {
    {XtNroundCards, XtCRoundCards, XtRBoolean, sizeof (Boolean),
     offset(round_cards), XtRImmediate, (XtPointer) True},
    {XtNoverlap, XtCOverlap, XtRCardsOverlap, sizeof (CardsOverlap),
     offset(overlap), XtRImmediate, (XtPointer) CardsOverlapNeither},
    {XtNsmallCards, XtCSmallCards, XtRBoolean, sizeof (Boolean),
     offset(small_cards), XtRImmediate, (XtPointer) False},
    {XtNuseTile, XtCUseTile, XtRBoolean, sizeof (Boolean),
     offset(use_tile), XtRImmediate, (XtPointer) False},
    {XtNback, XtCBack, XtRBitmap, sizeof (Pixmap),
     offset(back), XtRString, (XtPointer) "playing_card" },
    {XtNtrademark, XtCTrademark, XtRBitmap, sizeof (Pixmap),
     offset(trademark), XtRString, (XtPointer) "spade_lg" },
    {XtNobverseColor, XtNobverseColor, XtRPixel, sizeof (Pixel),
     offset(obverse_color), XtRString, (XtPointer) "white"},
    {XtNbackground, XtCBackground, XtRPixel, sizeof (Pixel),
     offset(empty_color), XtRString, XtDefaultBackground},
    {XtNblackColor, XtCBlackColor, XtRPixel, sizeof (Pixel),
     offset(black_color), XtRString, (XtPointer) "black"},
    {XtNredColor, XtCRedColor, XtRPixel, sizeof (Pixel),
     offset(red_color), XtRString, (XtPointer) "red"},
    {XtNinverseColor, XtCInverseColor, XtRPixel, sizeof (Pixel),
     offset(inverse_color), XtRString, (XtPointer) "Sea Green"},
    {XtNcolor, XtCColor, XtRBoolean, sizeof (Boolean),
     offset(color), XtRImmediate, (XtPointer) COLOR_UNSET },
};

#undef offset
#undef hoffset

#include	<X11/Xmu/Drawing.h>

#define	ROUND_W	7
#define	ROUND_H	7
#define INSET 1

/* substitue gray1 for Green on mono */
#define gray_width 2
#define gray_height 2
static char gray_bits[] = {0x01, 0x02};

#include	"rank.bm"
#include	"face.bm"
#include	"suit.bm"
#include	"playing_card"

#define NUM_RANKS   13
#define NUM_SUITS   4
#define CARDS_PER_DECK 52
static Pixmap	rank_map[NUM_RANKS],	    rank_r_map[NUM_RANKS];
static Pixmap	rank_map_red[NUM_RANKS],    rank_r_map_red[NUM_RANKS];
static Pixmap	suit_map[NUM_SUITS],	suit_r_map[NUM_SUITS];
static Pixmap	suit_sm_map[NUM_SUITS],	suit_sm_r_map[NUM_SUITS];
static Pixmap	suit_lg_map[NUM_SUITS];
static Pixmap	jack_map[NUM_SUITS], queen_map[NUM_SUITS], king_map[NUM_SUITS];
static unsigned int	trademark_width, trademark_height;

#include	"cards.bm"

static Pixmap	card_bitmaps[CARDS_PER_DECK];
static Pixmap	gray_bitmap, gray_pixmap;

#define SuperClass  ((HandWidgetClass)&handClassRec)

static void ClassInitialize (), Initialize (), Destroy ();
static Boolean	SetValues ();

CardsClassRec	cardsClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) SuperClass,
    /* class_name		*/	"Cards",
    /* widget_size		*/	sizeof(CardsRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	XtInheritExpose,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
  { /* hand fields */
    /* ignore                   */	0
  },
  { /* cards fields */
    /* ignore			*/	0
  },
};

WidgetClass cardsWidgetClass = (WidgetClass) &cardsClassRec;

static void DisplayCallback ();

static Status 
GetSize (dpy, p, widthp, heightp)
    Display *dpy;
    Pixmap  p;
    unsigned int    *widthp, *heightp;
{
    int	    x, y, borderWidth, depth;
    Window  root;

    return XGetGeometry (dpy, p, &root, &x, &y, widthp, heightp, &borderWidth, &depth);
}

#define SMALL_CARD_HEIGHT   60
#define SMALL_CARD_WIDTH    40
#define LARGE_CARD_HEIGHT   123
#define LARGE_CARD_WIDTH    79

#define	FACECARD_WIDTH	47
#define	FACECARD_HEIGHT	92

#define	RANK_WIDTH	9
#define	RANK_HEIGHT	14

#define	RANK_LOC_X	4
#define	RANK_LOC_Y	7

#define	SMALL_LOC_X	4
#define	SMALL_LOC_Y	(RANK_HEIGHT + RANK_LOC_Y + 3)

#define	MID_CARD_X	(LARGE_CARD_WIDTH/2)
#define	MID_CARD_Y	(LARGE_CARD_HEIGHT/2)

#define	CARD_COL1_X	(3 * LARGE_CARD_WIDTH/10)
#define	CARD_COL2_X	(LARGE_CARD_WIDTH/2)
#define	CARD_COL3_X	(7 * LARGE_CARD_WIDTH/10)

/* 5 diff rows for the two main columns */
/* 1 and 5 are top and bottom, 3 is the middle */
/* 2 & 4 are for the 10 & 9 */
#define	CARD_ROW1_Y	(LARGE_CARD_HEIGHT/5)
#define	CARD_ROW2_Y	(2 * LARGE_CARD_HEIGHT/5)
#define	CARD_ROW3_Y	(LARGE_CARD_HEIGHT/2)
#define	CARD_ROW4_Y	(LARGE_CARD_HEIGHT - 2 * LARGE_CARD_HEIGHT/5)
#define	CARD_ROW5_Y	(LARGE_CARD_HEIGHT - LARGE_CARD_HEIGHT/5)

/* between 1 & 3, 3 & 5 */
#define	CARD_SEVEN_Y	(7 * LARGE_CARD_HEIGHT/20)
#define	CARD_EIGHT_Y	(LARGE_CARD_HEIGHT - 7 * LARGE_CARD_HEIGHT/20)

/* between rows 1 & 2, 4 & 5 */
#define	CARD_TEN_Y1	(3 * LARGE_CARD_HEIGHT/10)
#define	CARD_TEN_Y2	(LARGE_CARD_HEIGHT - 3 * LARGE_CARD_HEIGHT/10)

/* card positioning */
#define	CARD_INSET_X	10
#define	CARD_INSET_Y	(LARGE_CARD_HEIGHT/8)

#define CARD_WIDTH(w)	((w)->cards.small_cards ? SMALL_CARD_WIDTH : LARGE_CARD_WIDTH)
#define CARD_HEIGHT(w)	((w)->cards.small_cards ? SMALL_CARD_HEIGHT : LARGE_CARD_HEIGHT)
			    
#define ScreenNo(w) XScreenNumberOfScreen (XtScreen (w))
			    
static int  cardsRef = 0;

static void make_card_maps ();

static void
setSizeVars (req, new)
    CardsWidget	req, new;
{
    Arg	args[10];
    int	i = 0;
    int	width = CARD_WIDTH(req);
    int	height = CARD_HEIGHT(req);
    int	col_offset, row_offset;
    int	display_x, display_y, display_width, display_height;
    Boolean row_major;
    Boolean row_major_set;

    XtSetArg (args[i], XtNcardWidth, width); i++;
    XtSetArg (args[i], XtNcardHeight, height); i++;
    display_x = 0;
    display_y = 0;
    display_width = width;
    display_height = height;
    new->cards.offset_other = XkwHandDefaultOffset;
    row_major_set = False;
    if (req->cards.overlap & CardsOverlapHorizontal)
    {
	row_major = True;
	row_major_set = True;
	if (req->cards.small_cards)
	{
	    col_offset = 16;
	    new->cards.offset_other = 8;
	}
	else
	{
	    col_offset = RANK_LOC_X + RANK_WIDTH + RANK_WIDTH / 3;
	    new->cards.offset_other = ROUND_W + LARGE_CARD_HEIGHT / 30;
	}
	if (req->cards.round_cards && !req->cards.small_cards)
	{
	    display_x = ROUND_W;
	    display_width -= display_x * 2;
	}
    }
    else
    {
	if (req->cards.round_cards && !req->cards.small_cards)
	{
	    display_y = ROUND_H;
	    display_height -= display_y * 2;
	}
	col_offset = width + width / 10;
    }
    if (req->cards.overlap & CardsOverlapVertical) 
    {
	if (!row_major_set)
	{
	    row_major = False;
	    row_major_set = True;
	}
	if (req->cards.small_cards)
	{
	    row_offset = 20;
	    if (!row_major)
		new->cards.offset_other = 10;
	}
	else
	{
	    row_offset = SMALL_LOC_Y + spade_sm_height;
	    if (!row_major)
		new->cards.offset_other = ROUND_H + LARGE_CARD_HEIGHT/30;
	}
    }
    else
	row_offset = height + width / 10;
    XtSetArg (args[i], XtNcolOffset, col_offset); i++;
    XtSetArg (args[i], XtNrowOffset, row_offset); i++;
    XtSetArg (args[i], XtNinternalBorderWidth, width / 20); i++;
    XtSetArg (args[i], XtNdisplayX, display_x); i++;
    XtSetArg (args[i], XtNdisplayY, display_y); i++;
    XtSetArg (args[i], XtNdisplayWidth, display_width); i++;
    XtSetArg (args[i], XtNdisplayHeight, display_height); i++;
    if (row_major_set)
    {
	XtSetArg (args[i], XtNrowMajor, row_major); i++;
    }
    XtSetValues (new, args, i);
}

static Bool
CvtStringToCardsOverlap (dpy, args, num_args, from, to, converter_data)
    Display	*dpy;
    XrmValue	*args;
    Cardinal	*num_args;
    XrmValue	*from, *to;
    XtPointer	*converter_data;
{
    char    *s = (char *) from->addr;
    CardsOverlap    *result = (CardsOverlap *) to->addr;

    if (!strcmp (s, "neither"))
	*result = CardsOverlapNeither;
    else if (!strcmp (s, "vertical"))
	*result = CardsOverlapVertical;
    else if (!strcmp (s, "horizontal"))
	*result = CardsOverlapHorizontal;
    else if (!strcmp (s, "both"))
	*result = CardsOverlapBoth;
    else
	return FALSE;
    return TRUE;
}


static void
ClassInitialize()
{
    XtSetTypeConverter ( XtRString, XtRCardsOverlap, CvtStringToCardsOverlap,
		    NULL, (Cardinal)0, XtCacheNone, 
 		    (XtDestructor)NULL );
}

#define CardOffset(w,card)  ((card)->suit > CardsSpade ? (w)->cards.offset_other : XkwHandDefaultOffset)

XtPointer
CardsAddCard (gw, card, row, col)
    Widget	    gw;
    CardsCardPtr    card;
    int		    row, col;
{
    CardsWidget	w = (CardsWidget) gw;
    return HandAddCard (gw, (XtPointer) card, row, col, CardOffset(w,card));
}

void
CardsReplaceCard (gw, data, card)
    Widget	    gw;
    XtPointer	    data;
    CardsCardPtr    card;
{
    CardsWidget	w = (CardsWidget) gw;
    HandReplaceCard (gw, data, (XtPointer) card, CardOffset(w,card));
}

#define UsualSuspects(w)	Display *dpy = XtDisplay ((Widget) w); \
				Window	window = XtWindow ((Widget) w)
				
#define CardGC(w,suit) GC cardgc = ((suit) == CardsSpade || \
				    (suit) == CardsClub) ? \
				    (w)->cards.blackgc : (w)->cards.redgc; \
		       Boolean *hasp = ((suit) == CardsSpade || \
					(suit) == CardsClub) ? \
					&(w)->cards.blackHasClip : \
					&(w)->cards.redHasClip
				    
#define GetScreen(w)		int screen = ScreenNo(w)
    			    
#define SetClip(dpy,gc,clip,has)    if (((clip) != NULL) != *(has)) {\
					if (*(has) = ((clip) != NULL)) \
					    XSetClipRectangles(dpy, gc, 0, 0, \
						clip, 1, YXBanded); \
					else \
					    XSetClipMask(dpy,gc,None); \
				    }
CheckCopyPlane(dpy, src, dst, gc, width, height, dstx, dsty, clip)
    Display	*dpy;
    Pixmap	src;
    Window	dst;
    GC		gc;
    int		width, height, dstx, dsty;
    XRectangle	*clip;
{
    int	    srcx = 0, srcy = 0;

    if (clip)
    {
	if (dstx < clip->x) {
	    srcx = clip->x - dstx;
	    dstx += srcx;
	    width -= srcx;
	}
	if (clip->x + clip->width < dstx + width)
	    width = clip->x + clip->width - dstx;
	if (dsty < clip->y) {
	    srcy = clip->y - dsty;
	    dsty += srcy;
	    height -= srcy;
	}
	if (clip->y + clip->height < dsty + height)
	    height = clip->y + clip->height - dsty;
    }
    if (width > 0 && height > 0)
	XCopyPlane (dpy, src, dst, gc, srcx, srcy, 
		    width, height, dstx, dsty, 1);
}
    
static void
GetGCs (w)
    CardsWidget	w;
{
    UsualSuspects(w);
    GetScreen(w);
    XGCValues	gcv;
    XtGCMask	mask, tmask;
    unsigned	width, height;
    
    mask = GCForeground | GCBackground | GCGraphicsExposures;
    gcv.graphics_exposures = False;
    /* white GC */
    gcv.foreground = w->cards.obverse_color;
    gcv.background = w->cards.black_color;
    w->cards.whitegc = XtGetGC (w, mask, &gcv);
    w->cards.whiteHasClip = False;
    /* black GC */
    gcv.foreground = w->cards.black_color;
    gcv.background = w->cards.obverse_color;
    gcv.line_width = INSET;
    if (INSET == 1)
	gcv.line_width = 0;
    w->cards.blackgc = XtGetGC (w, mask | GCLineWidth, &gcv);
    w->cards.blackHasClip = False;
    
    /* empty GC */
    gcv.foreground = w->cards.empty_color;
    gcv.background = w->cards.black_color;
    w->cards.emptygc = XtGetGC (w, mask, &gcv);

    /* red GC */
    if (w->cards.color)
    {
	gcv.foreground = w->cards.red_color;
	gcv.background = w->cards.obverse_color;
	w->cards.redgc = XtGetGC (w, mask, &gcv);
    }
    else
    {
	if (!gray_bitmap)
	    gray_bitmap = XCreateBitmapFromData (dpy, RootWindow (dpy, screen),
						 gray_bits, gray_width, gray_height);
	gcv.foreground = w->cards.black_color;
	gcv.background = w->cards.obverse_color;
	gcv.stipple = gray_bitmap;
	gcv.fill_style = FillOpaqueStippled;
	tmask = mask | GCFillStyle | GCStipple;
	w->cards.redgc = XtGetGC (w, mask | GCFillStyle | GCStipple,
				    &gcv);
    }
    w->cards.redHasClip = False;
    
    /* back GC */
    if (w->cards.back)
    {
	gcv.stipple = w->cards.back;
	GetSize (dpy, w->cards.back, &width, &height);
    }
    else
    {
	gcv.stipple = XCreateBitmapFromData (dpy, RootWindow (dpy, screen),
    	     playing_card_bits, playing_card_width, playing_card_height);
	width = playing_card_width;
	height = playing_card_height;
    }
    w->cards.back_delta_x = (CARD_WIDTH(w) - width) / 2;
    w->cards.back_delta_y = (CARD_HEIGHT(w) - height) / 2;
    
    if (w->cards.color) 
	gcv.foreground = w->cards.inverse_color;
    else
	gcv.foreground = w->cards.black_color;
    gcv.background = w->cards.obverse_color;
    gcv.fill_style = FillOpaqueStippled;
    tmask = mask | GCFillStyle | GCStipple;
    if (w->cards.use_tile)
    {
	GC  tgc = XtGetGC (w, tmask, &gcv);
	
	if (w->cards.backTile)
	    XFreePixmap (dpy, w->cards.backTile);
	w->cards.backTile = XCreatePixmap (dpy, RootWindow (dpy, screen),
					   width, height, w->core.depth);
	XFillRectangle (dpy, w->cards.backTile, tgc, 0, 0, width, height);
	XtReleaseGC (w, tgc);
	gcv.tile = w->cards.backTile;
	gcv.fill_style = FillTiled;
	tmask = mask | GCFillStyle | GCTile;
    }
    w->cards.backgc = XtGetGC (w, tmask, &gcv);
    w->cards.backHasClip = False;
}

static void
ReleaseGCs (w)
    CardsWidget	w;
{
    XtReleaseGC (w, w->cards.whitegc);
    XtReleaseGC (w, w->cards.blackgc);
    XtReleaseGC (w, w->cards.emptygc);
    XtReleaseGC (w, w->cards.redgc);
    XtReleaseGC (w, w->cards.backgc);
}

static void 
Initialize (greq, gnew)
    Widget  greq, gnew;
{
    CardsWidget	req = (CardsWidget) greq,
		new = (CardsWidget) gnew;
    XGCValues	gcv;
    XtGCMask	mask;
    unsigned	width, height;
    Display	*dpy;
    int		screen;
    
    dpy = XtDisplay (new);
    screen = ScreenNo(new);
    
    if (new->cards.color == COLOR_UNSET)
    {
	if (DisplayCells (dpy, screen) > 2)
	    new->cards.color = True;
	else
	    new->cards.color = False;
    }
    
    new->cards.redgc = 0;
    new->cards.blackgc = 0;
    new->cards.whitegc = 0;
    new->cards.backgc = 0;
    new->cards.backTile = 0;

    setSizeVars (req, new);
    
    XtAddCallback (gnew, XtNdisplayCallback, DisplayCallback, (XtPointer) gnew);

    GetGCs (new);
    if (cardsRef++ == 0)
	make_card_maps (new);
}

static void 
Destroy (gw)
    Widget  gw;
{
    CardsWidget	w = (CardsWidget) gw;

    ReleaseGCs (w);
    if (--cardsRef == 0) {
	/* destroy_card_maps (w); */
	;
    }
}

static Boolean
SetValues (gcur, greq, gnew)
    Widget  gcur, greq, gnew;
{
    CardsWidget	cur = (CardsWidget) gcur,
		req = (CardsWidget) greq,
 		new = (CardsWidget) gnew;
    if (req->cards.small_cards != cur->cards.small_cards ||
	req->cards.overlap != cur->cards.overlap)
    {
	setSizeVars (req, new);
	return TRUE;
    }
    if (new->cards.blackgc)
    {
	ReleaseGCs (cur);
	GetGCs (new);
    }
    return TRUE;
}

/*
 * make a 'red' pixmap by copying the bits and then stippling
 * white over it
 */

static Pixmap
make_red_map(w, bits, width, height)
    CardsWidget	w;
    char	*bits;
    int		width, height;
{
    UsualSuspects(w);
    GetScreen(w);
    Pixmap	tmpmap, newmap;
    static  GC	depth1gc;
    XGCValues	gcv;

    tmpmap = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				   bits, width, height);

    newmap = XCreatePixmap(dpy, RootWindow(dpy, screen), width, height, 1);

    if (!depth1gc)
    {
	gcv.stipple = gray_bitmap;
	gcv.foreground = 1;
	gcv.background = 0;
	gcv.fill_style = FillStippled;
	depth1gc = XCreateGC (dpy, newmap, 
			      GCBackground|GCStipple|GCFillStyle, &gcv);
    }
    XSetForeground (dpy, depth1gc, 1);
    XCopyPlane (dpy, tmpmap, newmap, depth1gc,
		0, 0, width, height, 0, 0, 1);
    
    XSetForeground (dpy, depth1gc, 0);
    XFillRectangle(dpy, newmap, depth1gc, 0, 0, width, height);
    XFreePixmap(dpy, tmpmap);

    return (newmap);
}

static void flip_bits (), rot_180 ();

static void
make_card_maps(w)
    CardsWidget	w;
{
    UsualSuspects(w);
    GetScreen(w);
    unsigned char	*new_bits;
    CardsRank		rank;
    CardsSuit		suit;
    int			r;
    int	i;

    for (rank = CardsAce; rank <= CardsKing; rank++)	
    {
	r = CardsRankToInt (rank);
	rank_map[r] = XCreateBitmapFromData(dpy, 
						 RootWindow(dpy, screen),
						 rank_bits[r], rank_width, rank_height);

	if (w->cards.color)
	    rank_map_red[r] = XCreateBitmapFromData(dpy, 
							 RootWindow(dpy, screen),
							 rank_bits[r], rank_width, rank_height);

	else
	    rank_map_red[r] = make_red_map(w, rank_bits[r],
						rank_width, rank_height);

	new_bits = (unsigned char *) calloc(sizeof(rank_bits[r]), 1);
	rot_180((unsigned char *)rank_bits[r], new_bits, 
		rank_width, rank_height);
	rank_r_map[r] = XCreateBitmapFromData(dpy, 
						   RootWindow(dpy, screen),
						   new_bits, rank_width, rank_height);
	if (w->cards.color)
	    rank_r_map_red[r] = XCreateBitmapFromData(dpy, 
							   RootWindow(dpy, screen),
							   new_bits, rank_width, rank_height);
	else
	    rank_r_map_red[r] = make_red_map(w, (char *)new_bits, 
						  rank_width, rank_height);
	free((char *)new_bits);
    }

    i = CardsSuitToInt (CardsSpade);
    /* make all the card bitmaps */
    suit_map[i] = XCreateBitmapFromData(dpy, 
					RootWindow(dpy, screen),
					spade_bits, spade_width, spade_height);

    new_bits = (unsigned char *) calloc(sizeof(spade_bits), 1);
    flip_bits((unsigned char *)spade_bits, new_bits, spade_width, 
	      spade_height);
    suit_r_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					  new_bits, spade_width, spade_height);
    free((char *)new_bits);

    suit_sm_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					   spade_sm_bits, spade_sm_width, spade_sm_height);

    new_bits = (unsigned char *) calloc(sizeof(spade_sm_bits), 1);
    flip_bits((unsigned char *)spade_sm_bits, new_bits, spade_sm_width,
	      spade_sm_height);
    suit_sm_r_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					     new_bits, spade_sm_width, spade_sm_height);
    free((char *)new_bits);

    if (w->cards.trademark)
    {
	suit_lg_map[i] = w->cards.trademark;
	GetSize (dpy, w->cards.trademark, &trademark_width, &trademark_height);
    }
    else
    {
	suit_lg_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					   spade_lg_bits, spade_lg_width, spade_lg_height);
	trademark_width = spade_lg_width;
	trademark_height = spade_lg_height;
    }

    jack_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					jack_s_bits, jack_s_width, jack_s_height);

    queen_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					 queen_s_bits, queen_s_width, queen_s_height);

    king_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					king_s_bits, king_s_width, king_s_height);

    i = CardsSuitToInt(CardsHeart);
    /* make all the card bitmaps */
    new_bits = (unsigned char *) calloc(sizeof(heart_bits), 1);
    flip_bits((unsigned char *)heart_bits, new_bits, heart_width, 
	      heart_height);

    if (w->cards.color)	{
	suit_map[i] = XCreateBitmapFromData(dpy, 
					    RootWindow(dpy, screen),
					    heart_bits, heart_width, heart_height);
	suit_r_map[i] = XCreateBitmapFromData(dpy, 
					      RootWindow(dpy, screen),
					      new_bits, heart_width, heart_height);
    } else	{
	suit_map[i] = make_red_map(w, heart_bits, heart_width, 
				   heart_height);
	suit_r_map[i] = make_red_map(w, (char *)new_bits, heart_width, 
				     heart_height);
    }

    free((char *)new_bits);

    new_bits = (unsigned char *) calloc(sizeof(heart_sm_bits), 1);
    flip_bits((unsigned char *)heart_sm_bits, new_bits, heart_sm_width, 
	      heart_sm_height);
    suit_sm_r_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					     new_bits, heart_sm_width, heart_sm_height);

    if (w->cards.color)	{
	suit_sm_map[i] = XCreateBitmapFromData(dpy, 
					       RootWindow(dpy, screen),
					       heart_sm_bits, heart_sm_width, heart_sm_height);
	suit_sm_r_map[i] = XCreateBitmapFromData(dpy, 
						 RootWindow(dpy, screen),
						 new_bits, heart_sm_width, heart_sm_height);
    } else	{
	suit_sm_map[i] = make_red_map(w, heart_sm_bits, heart_sm_width, 
				      heart_height);
	suit_sm_r_map[i] = make_red_map(w, (char *)new_bits, 
					heart_sm_width, heart_sm_height);
    }
    free((char *)new_bits);

    suit_lg_map[i] = suit_map[i];

    if (w->cards.color)	
    {
	jack_map[i] = XCreateBitmapFromData(dpy, 
					    RootWindow(dpy, screen),
					    jack_h_bits, jack_h_width, jack_h_height);

	queen_map[i] = XCreateBitmapFromData(dpy, 
					     RootWindow(dpy, screen),
					     queen_h_bits, queen_h_width, queen_h_height);

	king_map[i] = XCreateBitmapFromData(dpy, 
					    RootWindow(dpy, screen),
					    king_h_bits, king_h_width, king_h_height);
    } else	{
	jack_map[i] = make_red_map(w, jack_h_bits, jack_h_width, 
				   jack_h_height);

	queen_map[i] = make_red_map(w, queen_h_bits, queen_h_width, 
				    queen_h_height);

	king_map[i] = make_red_map(w, king_h_bits, king_h_width, 
				   king_h_height);
    }


    i = CardsSuitToInt(CardsDiamond);
    
    /* make all the card bitmaps */
    new_bits = (unsigned char *) calloc(sizeof(diamond_bits), 1);
    flip_bits((unsigned char *)diamond_bits, new_bits, diamond_width, 
	      diamond_height);

    if (w->cards.color)	{
	suit_map[i] = XCreateBitmapFromData(dpy, 
					    RootWindow(dpy, screen),
					    diamond_bits, diamond_width, diamond_height);
	suit_r_map[i] = XCreateBitmapFromData(dpy, 
					      RootWindow(dpy, screen),
					      new_bits, diamond_width, diamond_height);
    } else	{
	suit_map[i] = make_red_map(w, diamond_bits, diamond_width, 
				   diamond_height);
	suit_r_map[i] = make_red_map(w, (char *)new_bits, diamond_width, 
				     diamond_height);
    }

    free((char *)new_bits);

    new_bits = (unsigned char *) calloc(sizeof(diamond_sm_bits), 1);
    flip_bits((unsigned char *)diamond_sm_bits, new_bits, 
	      diamond_sm_width, diamond_sm_height);
    suit_sm_r_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					     new_bits, diamond_sm_width, diamond_sm_height);

    if (w->cards.color)	{
	suit_sm_map[i] = XCreateBitmapFromData(dpy, 
					       RootWindow(dpy, screen),
					       diamond_sm_bits, diamond_sm_width, diamond_sm_height);
	suit_sm_r_map[i] = XCreateBitmapFromData(dpy, 
						 RootWindow(dpy, screen),
						 new_bits, diamond_sm_width, diamond_sm_height);
    } else	{
	suit_sm_map[i] = make_red_map(w, diamond_sm_bits, diamond_sm_width, 
				      diamond_height);
	suit_sm_r_map[i] = make_red_map(w, (char *)new_bits, 
					diamond_sm_width, diamond_sm_height);
    }
    free((char *)new_bits);

    suit_lg_map[i] = suit_map[i];

    if (w->cards.color)	{
	jack_map[i] = XCreateBitmapFromData(dpy, 
					    RootWindow(dpy, screen),
					    jack_d_bits, jack_d_width, jack_d_height);

	queen_map[i] = XCreateBitmapFromData(dpy, 
					     RootWindow(dpy, screen),
					     queen_d_bits, queen_d_width, queen_d_height);

	king_map[i] = XCreateBitmapFromData(dpy, 
					    RootWindow(dpy, screen),
					    king_d_bits, king_d_width, king_d_height);
    } else	{
	jack_map[i] = make_red_map(w, jack_d_bits, jack_d_width, 
				   jack_d_height);

	queen_map[i] = make_red_map(w, queen_d_bits, queen_d_width, 
				    queen_d_height);

	king_map[i] = make_red_map(w, king_d_bits, king_d_width, 
				   king_d_height);
    }

    i = CardsSuitToInt(CardsClub);
    /* make all the card bitmaps */
    suit_map[i] = XCreateBitmapFromData(dpy, 
					RootWindow(dpy, screen),
					club_bits, club_width, club_height);

    new_bits = (unsigned char *) calloc(sizeof(club_bits), 1);
    flip_bits((unsigned char *)club_bits, new_bits, club_width, 
	      club_height);
    suit_r_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					  new_bits, club_width, club_height);
    free((char *)new_bits);

    suit_sm_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					   club_sm_bits, club_sm_width, club_sm_height);

    new_bits = (unsigned char *) calloc(sizeof(club_sm_bits), 1);
    flip_bits((unsigned char *)club_sm_bits, new_bits, club_sm_width, 
	      club_sm_height);
    suit_sm_r_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					     new_bits, club_sm_width, club_sm_height);
    free((char *)new_bits);

    suit_lg_map[i] = suit_map[i];


    jack_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					jack_c_bits, jack_c_width, jack_c_height);

    queen_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					 queen_c_bits, queen_c_width, queen_c_height);

    king_map[i] = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					king_c_bits, king_c_width, king_c_height);

    for (i = 0; i < CARDS_PER_DECK; i++)	
    {
	card_bitmaps[i] = XCreateBitmapFromData(dpy, 
						RootWindow(dpy, screen),
						card_bits[i], 
						SMALL_CARD_WIDTH, SMALL_CARD_HEIGHT);
    }
}

/*
 * paints individual card
 */

static void paint_small_card (), paint_large_card ();
static void draw_king (), draw_queen (), draw_jack ();
static void draw_pip (), draw_did (), draw_eight_pips ();
static void draw_center_pip ();
static void draw_rank ();

static void
OutlineCard (w, x, y, clip)
    CardsWidget	w;
    int		x, y;
    XRectangle	*clip;
{
    UsualSuspects(w);
    int	width, height;

    width = CARD_WIDTH(w);
    height = CARD_HEIGHT(w);
    SetClip (dpy, w->cards.blackgc, clip, &w->cards.blackHasClip);
    if (w->cards.round_cards && !w->cards.small_cards)
    {
	XmuDrawRoundedRectangle (dpy, window, w->cards.blackgc,
				 x + INSET/2, y + INSET/2,
				 width - INSET, height - INSET,
				 ROUND_W, ROUND_H);
    }
    else
    {
	XDrawRectangle (dpy, window, w->cards.blackgc,
			x + INSET/2, y + INSET/2,
			width - INSET, height - INSET);
    }
}

static void
FillCard (w, x, y, gc, hasp, clip)
    CardsWidget	w;
    int		x, y;
    GC		gc;
    Boolean	*hasp;
    XRectangle	*clip;
{
    UsualSuspects(w);
    int	width, height;
    
    width = CARD_WIDTH(w);
    height = CARD_HEIGHT(w);
    SetClip (dpy, gc, clip, hasp);
    if (w->cards.round_cards && !w->cards.small_cards)
    {
	XmuFillRoundedRectangle(dpy, window, gc,
				x + INSET, y + INSET, 
				width - 2*INSET, height - 2 * INSET,
				ROUND_W, ROUND_H);
    }
    else
    {
	XFillRectangle(dpy, window, gc,
		       x + INSET, y + INSET, 
		       width - 2*INSET, height - 2*INSET);
    }
    OutlineCard (w, x, y, clip);
}

    
static void
DisplayCallback (gw, closure, data)
    Widget	gw;
    XtPointer	closure;
    XtPointer	data;
{
    CardsWidget	    w = (CardsWidget) gw;
    Display	    *dpy = XtDisplay (w);
    Window	    window = XtWindow (w);
    HandDisplayPtr  display = (HandDisplayPtr) data;
    CardsCardPtr    card = (CardsCardPtr) display->private;
    int		    x, y, width, height;
    XRectangle	    *clip;
    
    x = display->x;
    y = display->y;
    width = CARD_WIDTH(w);
    height = CARD_HEIGHT(w);
    clip = NULL;
    if (display->clipped)
	clip = &display->clip;
    switch (card->suit) {
    case CardsEmpty:
	FillCard (w, x, y, w->cards.emptygc, &w->cards.emptyHasClip, clip);
	break;
    case CardsBack:
	/* change the origin so cards will have the same back anywhere
	 * on the table
	 */
	
	XSetTSOrigin(dpy, w->cards.backgc, 
		     x + w->cards.back_delta_x, y + w->cards.back_delta_y);
	FillCard (w, x, y, w->cards.backgc, &w->cards.backHasClip, clip);
	break;
    case CardsNone:
	break;
    default:
	if (w->cards.small_cards)
	    paint_small_card (w, x, y, card->rank, card->suit, clip);
	else
	    paint_large_card (w, x, y, card->rank, card->suit, clip);
	break;
    }
    SetClip (dpy, w->cards.redgc, (XRectangle *) 0, &w->cards.redHasClip);
    SetClip (dpy, w->cards.blackgc, (XRectangle *) 0, &w->cards.blackHasClip);
    SetClip (dpy, w->cards.whitegc, (XRectangle *) 0, &w->cards.whiteHasClip);
    SetClip (dpy, w->cards.emptygc, (XRectangle *) 0, &w->cards.emptyHasClip);
    SetClip (dpy, w->cards.backgc, (XRectangle *) 0, &w->cards.backHasClip);
}

static void
paint_large_card(w, x, y, rank, suit, clip)
    CardsWidget	w;
    int		x,y;
    CardsRank	rank;
    CardsSuit	suit;
    XRectangle	*clip;
{
    UsualSuspects(w);

    FillCard (w, x, y, w->cards.whitegc, &w->cards.whiteHasClip, clip);
    
    switch (rank)
    {
    case CardsKing:
	draw_king(w, suit, x, y, clip);
	break;
    case CardsQueen:
	draw_queen(w, suit, x, y, clip);
	break;
    case CardsJack:
	draw_jack(w, suit, x, y, clip);
	break;

    case Cards10:
	draw_pip(w, suit, MID_CARD_X + x, CARD_TEN_Y1 + y, clip);
	draw_did(w, suit, MID_CARD_X + x, CARD_TEN_Y2 + y, clip);
	draw_eight_pips(w, suit, x, y, clip);
	break;

    case Cards9:
	draw_pip(w, suit, x + MID_CARD_X, y + MID_CARD_Y, clip);
	draw_eight_pips(w, suit, x, y, clip);
	break;

    case Cards8:
	draw_did(w, suit, x + MID_CARD_X, y + CARD_EIGHT_Y, clip);
	/* fall thru */
    case Cards7:
	draw_pip(w, suit, MID_CARD_X + x, CARD_SEVEN_Y + y, clip);
	/* fall thru */
    case Cards6:
	draw_pip(w, suit, x + CARD_COL1_X, y + CARD_ROW1_Y, clip);

	draw_pip(w, suit, x + CARD_COL3_X, y + CARD_ROW1_Y, clip);

	draw_pip(w, suit, x + CARD_COL1_X, y + CARD_ROW3_Y, clip);
	draw_did(w, suit, x + CARD_COL1_X, y + CARD_ROW5_Y, clip);

	draw_pip(w, suit, x + CARD_COL3_X, y + CARD_ROW3_Y, clip);
	draw_did(w, suit, x + CARD_COL3_X, y + CARD_ROW5_Y, clip);
	break;

    case Cards5:
	draw_pip(w, suit, x + MID_CARD_X, y + MID_CARD_Y, clip);
	/* fall thru */
    case Cards4:
	draw_pip(w, suit, x + CARD_COL1_X, y + CARD_ROW1_Y, clip);
	draw_did(w, suit, x + CARD_COL1_X, y + CARD_ROW5_Y, clip);

	draw_pip(w, suit, x + CARD_COL3_X, y + CARD_ROW1_Y, clip);
	draw_did(w, suit, x + CARD_COL3_X, y + CARD_ROW5_Y, clip);
	break;

    case Cards3:
	draw_pip(w, suit, x + MID_CARD_X, y + MID_CARD_Y, clip);
	/* fall thru */
    case Cards2:
	draw_pip(w, suit, x + MID_CARD_X, y + CARD_ROW1_Y, clip);
	draw_did(w, suit, x + MID_CARD_X, y + CARD_ROW5_Y, clip);
	break;
    case CardsAce:
	draw_center_pip(w, suit, x + MID_CARD_X, y + MID_CARD_Y, clip);
	break;
    }

    draw_rank(w, x, y, rank, suit, clip);
}

/*
 * NOTE -- for all the pip drawers except the one that actually plots the
 * bits, the location is the card's location.  the drawer's take the
 * pip's center as location.
 */

/*
 * draws right-side-up pip
 *
 * location is for center of pip
 */
 
static void
draw_pip(w, suit, x, y, clip)
    CardsWidget w;
    CardsSuit	suit;
    int		x, y;
    XRectangle	*clip;
{
    UsualSuspects (w);
    CardGC (w, suit);
    int	    width, height;
    int	    s = CardsSuitToInt(suit);

    switch(suit)	
    {
    case CardsSpade:
	width = spade_width;
	height = spade_height;
	break;
    case CardsDiamond:
	x++;
	width = diamond_width;
	height = diamond_height;
	break;
    case CardsHeart:
	y++;
	width = heart_width;
	height = heart_height;
	break;
    case CardsClub:
	y++;
	width = club_width;
	height = club_height;
	break;
    default:
	return;
    }
    CheckCopyPlane(dpy, suit_map[s], window, cardgc, 
		   width, height,
		   x - width/2, y - height/2, clip);
}

/*
 * draws upside-down pip
 *
 * location is for center of pip
 */

static void
draw_did(w, suit, x, y, clip)
    CardsWidget	w;
    CardsSuit	suit;
    int		x,y;
    XRectangle	*clip;
{
    UsualSuspects (w);
    CardGC (w,suit);
    int	    width, height;
    int	    s = CardsSuitToInt(suit);

    switch(suit)	
    {
    case CardsSpade:
	width = spade_width;
	height = spade_height;
	break;
    case CardsDiamond:
	x++;
	width = diamond_width;
	height = diamond_height;
	break;
    case CardsHeart:
	y++;
	width = heart_width;
	height = heart_height;
	break;
    case CardsClub:
	y++;
	width = club_width;
	height = club_height;
	break;
    }
    CheckCopyPlane(dpy, suit_r_map[s], window, cardgc, 
	       width, height, x - width/2, y - height/2, clip);
}

/*
 * draws big center pip
 */
 
static void
draw_center_pip(w, suit, x, y, clip)
    CardsWidget	w;
    CardsSuit	suit;
    int	x,y;
    XRectangle	*clip;
{
    UsualSuspects(w);
    int	width, height;
    CardGC(w,suit);
    int	s = CardsSuitToInt(suit);

    switch(suit)	
    {
    case CardsSpade:
	width = trademark_width;
	height = trademark_height;
	break;
    case CardsDiamond:
	width = diamond_width;
	height = diamond_height;
	break;
    case CardsHeart:
	width = heart_width;
	height = heart_height;
	break;
    case CardsClub:
	width = club_width;
	height = club_height;
	break;
    }
    CheckCopyPlane(dpy, suit_lg_map[s], window, cardgc, 
		   width, height, x - width/2, y - height/2, clip);
}

static void
draw_eight_pips(w, suit, x, y, clip)
    CardsWidget	w;
    CardsSuit	suit;
    int		x,y;
    XRectangle	*clip;
{
    draw_pip(w, suit, x + CARD_COL1_X, y + CARD_ROW1_Y, clip);

    draw_pip(w, suit, x + CARD_COL3_X, y + CARD_ROW1_Y, clip);

    draw_pip(w, suit, x + CARD_COL1_X, y + CARD_ROW2_Y, clip);
    draw_did(w, suit, x + CARD_COL1_X, y + CARD_ROW4_Y, clip);
    draw_did(w, suit, x + CARD_COL1_X, y + CARD_ROW5_Y, clip);

    draw_pip(w, suit, x + CARD_COL3_X, y + CARD_ROW2_Y, clip);
    draw_did(w, suit, x + CARD_COL3_X, y + CARD_ROW4_Y, clip);
    draw_did(w, suit, x + CARD_COL3_X, y + CARD_ROW5_Y, clip);
}

static void
draw_jack(w, suit, x, y, clip)
    CardsWidget	w;
    CardsSuit	suit;
    int		x,y;
    XRectangle	*clip;
{
    UsualSuspects(w);
    CardGC(w, suit);
    int	s = CardsSuitToInt(suit);

    CheckCopyPlane(dpy, jack_map[s], window, cardgc, 
	       FACECARD_WIDTH, FACECARD_HEIGHT,
	       x + (LARGE_CARD_WIDTH - FACECARD_WIDTH)/2, 
	       y + (LARGE_CARD_HEIGHT - FACECARD_HEIGHT)/2, clip);

    SetClip (dpy, cardgc, clip, hasp);
    XDrawRectangle(dpy, window, cardgc,
		   x + (LARGE_CARD_WIDTH - FACECARD_WIDTH)/2, 
		   y + (LARGE_CARD_HEIGHT - FACECARD_HEIGHT)/2,
		   FACECARD_WIDTH, FACECARD_HEIGHT);
}

static void
draw_queen(w, suit, x, y, clip)
    CardsWidget	w;
    CardsSuit	suit;
    int		x,y;
    XRectangle	*clip;
{
    UsualSuspects(w);
    CardGC(w, suit);
    int	s = CardsSuitToInt(suit);
    
    CheckCopyPlane(dpy, queen_map[s], window, cardgc,
	       FACECARD_WIDTH, FACECARD_HEIGHT,
	       x + (LARGE_CARD_WIDTH - FACECARD_WIDTH)/2, 
	       y + (LARGE_CARD_HEIGHT - FACECARD_HEIGHT)/2, clip);

    SetClip (dpy, cardgc, clip, hasp);
    XDrawRectangle(dpy, window, cardgc,
		   x + (LARGE_CARD_WIDTH - FACECARD_WIDTH)/2, 
		   y + (LARGE_CARD_HEIGHT - FACECARD_HEIGHT)/2,
		   FACECARD_WIDTH, FACECARD_HEIGHT);
}

static void
draw_king(w, suit, x, y, clip)
    CardsWidget	w;
    CardsSuit	suit;
    int		x,y;
    XRectangle	*clip;
{
    UsualSuspects(w);
    CardGC(w, suit);
    int	s = CardsSuitToInt(suit);
	
    CheckCopyPlane(dpy, king_map[s], window, cardgc,
		   FACECARD_WIDTH, FACECARD_HEIGHT,
		   x + (LARGE_CARD_WIDTH - FACECARD_WIDTH)/2, 
		   y + (LARGE_CARD_HEIGHT - FACECARD_HEIGHT)/2, clip);

    SetClip (dpy, cardgc, clip, hasp);
    XDrawRectangle(dpy, window, cardgc,
		   x + (LARGE_CARD_WIDTH - FACECARD_WIDTH)/2, 
		   y + (LARGE_CARD_HEIGHT - FACECARD_HEIGHT)/2,
		   FACECARD_WIDTH, FACECARD_HEIGHT);
}

static void
draw_rank(w, x, y, rank, suit, clip)
    CardsWidget	w;
    int		x, y;
    CardsRank	rank;
    CardsSuit	suit;
    XRectangle	*clip;
{
    UsualSuspects(w);
    int	width, height;
    int	r, s;
    CardGC(w, suit);
    Pixmap  top, bottom;

    r = CardsRankToInt(rank);
    s = CardsSuitToInt(suit);
    if (suit == CardsHeart || suit == CardsDiamond)
    {
	top = rank_map_red[r];
	bottom = rank_r_map_red[r];
    }
    else
    {
	top = rank_map[r];
	bottom = rank_r_map[r];
    }
    CheckCopyPlane(dpy, top, window, cardgc,
	       RANK_WIDTH, RANK_HEIGHT,
	       x + RANK_LOC_X, y + RANK_LOC_Y, clip);

    CheckCopyPlane(dpy, bottom, window, cardgc,
	       RANK_WIDTH, RANK_HEIGHT,
	       x + (LARGE_CARD_WIDTH - RANK_WIDTH - RANK_LOC_X), 
	       y + (LARGE_CARD_HEIGHT - RANK_HEIGHT - RANK_LOC_Y), clip);
    
    switch (suit)	
    {
    case CardsSpade:
	width = spade_sm_width;
	height = spade_sm_height;
	break;
    case CardsHeart:
	width = heart_sm_width;
	height = heart_sm_height;
	break;
    case CardsDiamond:
	x++;	/* offset the smaller width */
	width = diamond_sm_width;
	height = diamond_sm_height;
	break;
    case CardsClub:
	width = club_sm_width;
	height = club_sm_height;
	break;
    }
    CheckCopyPlane(dpy, suit_sm_map[s], window, cardgc,
	       width, height,
	       x + SMALL_LOC_X, y + SMALL_LOC_Y, clip);

    CheckCopyPlane(dpy, suit_sm_r_map[s], window, cardgc,
	       width, height,
	       x + (LARGE_CARD_WIDTH - width - SMALL_LOC_X), 
	       y + (LARGE_CARD_HEIGHT - height - SMALL_LOC_Y), clip);
}

static void
paint_small_card(w, x, y, rank, suit, clip)
    CardsWidget	w;
    int		x, y;
    CardsRank	rank;
    CardsSuit	suit;
    XRectangle	*clip;
{
    UsualSuspects(w);
    int	card_number;
    GC	cardgc;
    Boolean *hasp;

    if (suit == CardsSpade || suit == CardsClub)
    {
	cardgc = w->cards.blackgc;
	hasp = &w->cards.blackHasClip;
    }
    else if (w->cards.color) {
	cardgc = w->cards.redgc;
	hasp = &w->cards.redHasClip;
    }
    else
    {
	cardgc = w->cards.whitegc;
	hasp = &w->cards.whiteHasClip;
    }

    /* this is messy cause these are just straight xsol cards */
    switch (suit)	
    {
    case CardsSpade:
	card_number = 3 * NUM_RANKS;
	break;
    case CardsHeart:
	card_number = 0;
	break;
    case CardsClub:
	card_number = 2 * NUM_RANKS;
	break;
    case CardsDiamond:
	card_number = NUM_RANKS;
	break;
    }
    card_number += CardsRankToInt (rank);

    CheckCopyPlane(dpy, card_bitmaps[card_number], window, cardgc, 
	       SMALL_CARD_WIDTH, SMALL_CARD_HEIGHT, x, y, clip);
}

static unsigned char _reverse_byte[0x100] = {
	0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
	0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
	0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
	0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
	0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
	0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
	0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
	0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
	0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
	0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
	0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
	0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
	0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
	0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
	0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
	0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
	0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
	0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
	0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
	0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
	0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
	0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
	0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
	0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
	0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
	0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
	0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
	0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
	0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
	0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
	0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
	0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};

#define S(x,y) src[(H-1-(y))*W+(x)]
#define D(x,y) dst[(H-1-(y))*W+(x)]

static void
flip_bits(src, dst, W, H)
    unsigned char   *src, *dst;
    int		    W, H;
{
    int	x, y;

    W = (W + 7)/8;
    for (y = 0; y < H; y++)	{
	for (x = 0; x < W; x++)	{
	    D (x, y) = S (x, H - 1 - y);
	}
    }
}

static void
rot_180(src, dst, W, H)
    unsigned char   *src, *dst;
    int		    W, H;
{
    int		    x, y;
    int		    width = W;
    unsigned char   *new;
    int		    bit;

    W = (W + 7)/8;
    for (y = 0; y < H; y++) {
	for (x = 0; x < W; x++) {
	    D (x, y) = _reverse_byte[S (W - 1 - x, H - 1 - y)];
	}
    }

    /* shift it over */
    new = (unsigned char *)calloc((unsigned)W*H, (unsigned)1);
    for (y = 0; y < H; y++)	{
	for (x = 0; x < W*8; x++)	{
	    bit = (*(dst + (x + (W*8 - width))/8 + y * W)
		   & (1 << ((x + (W*8 - width)) % 8))) ? 1 : 0;
	    *(new + x/8 + y*W) = (bit << (x%8)) | 
	    (*(new + x/8 + y*W) & ~(1 << (x%8)));
	}
    }
    bcopy((char *)new, (char *)dst, W*H);
    free((char *)new);
}
