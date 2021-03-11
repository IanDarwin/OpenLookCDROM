/*
 * drawcard.c
 *
 * display cards on the table
 */

# include	"mille.h"
# include	"uiXt.h"
# include	"card.h"
# include	"background.h"

extern char	_go_bits[], go_label_bits [], go_mask_bits[];
extern char	_stop_bits[], stop_label_bits [], stop_mask_bits[];
extern char	_right_bits[], right_label_bits[], right_mask_bits[];
extern char	_speed_bits[], speed_mask_bits[];
extern char	_end_bits[], end_mask_bits[];
extern char	_accident_bits[], accident_label_bits [], accident_mask_bits[];
extern char	_repairs_bits[], repairs_label_bits [], repairs_mask_bits[];
extern char	_ace_bits[], ace_label_bits [], ace_mask_bits[];
extern char	_flat_bits[], flat_label_bits[], flat_mask_bits[];
extern char	_spare_bits[], spare_label_bits [], spare_mask_bits[];
extern char	_puncture_bits[], puncture_label_bits [], puncture_mask_bits[];
extern char	_out_bits[], out_label_bits[], out_mask_bits[];
extern char	_gas_bits[], gas_mask_bits[];
extern char	_extra_bits[], extra_label_bits [], extra_mask_bits[];
extern char	miles_mask_bits[];
extern char	_25_bits[], _50_bits[], _75_bits[], _100_bits[], _200_bits[];

extern char	deck_both_bits[];

struct card color_cards[NUM_CARDS] = {
fill_bits,	WHITE_COLOR,	miles_mask_bits,	BLACK_COLOR,
_25_bits,	BLUE_COLOR,	0,			0,	"25",	0,

fill_bits,	WHITE_COLOR,	miles_mask_bits,	BLACK_COLOR,
_50_bits,	BLUE_COLOR,	0,			0,	"50",	0,

fill_bits,	WHITE_COLOR,	miles_mask_bits,	BLACK_COLOR,
_75_bits,	BLUE_COLOR,	0,			0,	"75",	0,

fill_bits,	WHITE_COLOR,	miles_mask_bits,	BLACK_COLOR,
_100_bits,	BLUE_COLOR,	0,			0,	"100",	0,

fill_bits,	WHITE_COLOR,	miles_mask_bits,	BLACK_COLOR,
_200_bits,	BLUE_COLOR,	0,			0,	"200",	0,

fill_bits,	WHITE_COLOR,	out_mask_bits,		BLACK_COLOR,
_out_bits,	RED_COLOR,	out_label_bits,		RED_COLOR,	"Panne D'Essence",	0,

fill_bits,	WHITE_COLOR,	flat_mask_bits,		BLACK_COLOR,
_flat_bits,	RED_COLOR,	flat_label_bits,	RED_COLOR,	"Crev\351",	0,

fill_bits,	WHITE_COLOR,	accident_mask_bits,	BLACK_COLOR,
_accident_bits,	RED_COLOR,	accident_label_bits,	RED_COLOR,	"Accident",	0,

fill_bits,	WHITE_COLOR,	stop_mask_bits,		BLACK_COLOR,
_stop_bits,	RED_COLOR,	stop_label_bits,	RED_COLOR,	"Stop!",	0,

fill_bits,	WHITE_COLOR,	speed_mask_bits,	BLACK_COLOR,
_speed_bits,	RED_COLOR,	0,			0,		"Limite De Vitesse",	0,

fill_bits,	WHITE_COLOR,	gas_mask_bits,		BLACK_COLOR,
_gas_bits,	GREEN_COLOR,	0,			0,		"Essence",	0,

fill_bits,	WHITE_COLOR,	spare_mask_bits,	BLACK_COLOR,
_spare_bits,	GREEN_COLOR,	spare_label_bits,	GREEN_COLOR,	"Roue de secours",	0,

fill_bits,	WHITE_COLOR,	repairs_mask_bits,	BLACK_COLOR,
_repairs_bits,	GREEN_COLOR,	repairs_label_bits,	GREEN_COLOR,	"R\351parations",	0,

fill_bits,	WHITE_COLOR,	go_mask_bits,		BLACK_COLOR,
_go_bits,	GREEN_COLOR,	go_label_bits,		GREEN_COLOR,	"Roulez",	0,

fill_bits,	WHITE_COLOR,	end_mask_bits,		BLACK_COLOR,
_end_bits,	GREEN_COLOR,	0,			0,	"Fin de limite",	0,

fill_bits,	WHITE_COLOR,	extra_mask_bits,	BLACK_COLOR,
_extra_bits,	BLUE_COLOR,	extra_label_bits,	BLUE_COLOR,	"Citerne d'essence",	0,

fill_bits,	WHITE_COLOR,	puncture_mask_bits,	BLACK_COLOR,
_puncture_bits,	BLUE_COLOR,	puncture_label_bits,	BLUE_COLOR,	"Increvable",	0,

fill_bits,	WHITE_COLOR,	ace_mask_bits,		BLACK_COLOR,
_ace_bits,	BLUE_COLOR,	ace_label_bits,		BLUE_COLOR,	"As du volant",	0,

fill_bits,	WHITE_COLOR,	right_mask_bits,	BLACK_COLOR,
_right_bits,	RED_COLOR,	right_label_bits,	RED_COLOR,	"V\351hicule prioritaire",	0,
};

struct card	*cards;

extern char	deck_red_bits[], deck_blue_bits[], deck_mask_bits[];

struct card color_deck = {
	fill_bits,		GREEN_COLOR,
	deck_mask_bits,		BLACK_COLOR,
	0,			0,
	0,			0,
	"",
	0,
};

struct card mono_deck = {
	fill_bits,		GREY_COLOR,
	deck_mask_bits,		BLACK_COLOR,
	0,			0,
	0,			0,
	"",
	0,
};

extern char	blank_bits[];

struct card color_blank = { blank_bits, GREY_COLOR,	0,0,0,0,0,0, "", 0 };

struct card mono_blank = { blank_bits, WHITE_COLOR,	0,0,0,0,0,0, "", 0 };

struct card	deck;
struct card	blank;

BitmapBitsPut (dpy, d, gc, mask, mask_gc, bits)
Display		*dpy;
Drawable	d;
Pixmap		mask;
GC		gc, mask_gc;
char		*bits;
{
	XGCValues	gc_values;
	XImage		ximage;

	ximage.height = HEIGHT;
	ximage.width = WIDTH;
	ximage.depth = 1;
	ximage.bits_per_pixel = 1;
	ximage.xoffset = 0;
	ximage.format = XYPixmap;
	ximage.data = bits;
	ximage.byte_order = LSBFirst;
	ximage.bitmap_unit = 8;
	ximage.bitmap_bit_order = LSBFirst;
	ximage.bitmap_pad = 8;
	ximage.bytes_per_line = (WIDTH+7) / 8;
	XPutImage (dpy, mask, mask_gc, &ximage, 0, 0, 0, 0, WIDTH, HEIGHT);
	gc_values.clip_mask = mask;
	XChangeGC (dpy, gc, GCClipMask, &gc_values);
	XFillRectangle (dpy, d, gc, 0, 0, WIDTH, HEIGHT);
	gc_values.clip_mask = None;
	XChangeGC (dpy, gc, GCClipMask, &gc_values);
}


init_card (card, back_gc)
struct card	*card;
GC		back_gc;
{
	int		i;
	GC		mask_gc;
	Pixmap		mask;
	int		len, width;
	int		x;

	mask = XCreatePixmap (dpy, DefaultRootWindow (dpy), WIDTH, HEIGHT, 1);

	mask_gc = XCreateGC (dpy, mask, 0, 0);

	card->bits = XCreatePixmap(dpy, DefaultRootWindow(dpy), WIDTH, HEIGHT,
				      DefaultDepth(dpy, screen));
	XFillRectangle (dpy, card->bits, back_gc,
			0, 0, WIDTH, HEIGHT);
	for (i = 0; i < MAXBITMAPS && card->init[i].bits != 0; i++) {
		BitmapBitsPut (dpy, card->bits,
				colorMap[card->init[i].color].gc,
				mask, mask_gc,
				card->init[i].bits);
	}
	len = strlen (card->label);
	width = XTextWidth (font, card->label, len);
	x = WIDTH/20;
	if (x + width > (WIDTH - x)) {
		x = (WIDTH - width) / 2;
	}
	XDrawString (dpy, card->bits, text_gc, x,
		     HEIGHT / 20 + font->ascent, card->label, len);
	x = WIDTH - WIDTH/20 - width;
	if (x < WIDTH/20) {
		x = (WIDTH - width) / 2;
	}
	XDrawString (dpy, card->bits, text_gc, x,
		      HEIGHT - HEIGHT/20 - font->descent,
		      card->label, len);
	XFreeGC (dpy, mask_gc);
	XFreePixmap (dpy, mask);
}

Pixmap	card_border;

center_text(d, gc, f, y, l)
    Drawable	    d;
    GC		    gc;
    XFontStruct	    *f;
    int		    y;
    char	    *l;
{
    int	    len, width;
    int	    x;

    len = strlen (l);
    width = XTextWidth (f, l, len);
    x = (WIDTH - width) / 2;
    XDrawString (dpy, d, gc, x, y, l, len);
}

init_cards (back_gc)
GC	back_gc;
{
	int		i;
	GC		gc;
	XGCValues	gc_values;
	Pixmap		rest;
	GC		mask_gc;
	XGCValues	mask_values;
	Pixmap		mask;

	setbuf(stderr, NULL);

	card_border = XCreateBitmapFromData (dpy, DefaultRootWindow(dpy), fill_bits, WIDTH, HEIGHT);
	rest = XCreateBitmapFromData (dpy, DefaultRootWindow (dpy), deck_mask_bits, WIDTH, HEIGHT);
	gc_values.function = GXor;
	gc = XCreateGC (dpy, card_border, GCFunction, &gc_values);
	XCopyArea (dpy, rest, card_border, gc, 0, 0, WIDTH, HEIGHT, 0, 0);
	XFreePixmap (dpy, rest);
	XFreeGC (dpy, gc);

	init_card (&blank, back_gc);
	init_card (&deck, back_gc);
	gc_values.font = backFont->fid;
	gc_values.foreground = colorMap[RED_COLOR].pixel;
	XCreateGC (dpy, deck.bits, GCFont|GCForeground, &gc_values);
	center_text (deck.bits, gc, backFont, HEIGHT / 2 - backFont->descent,
		     "Mille");
	XSetForeground (dpy, gc, colorMap[BLUE_COLOR].pixel);
	center_text (deck.bits, gc, backFont, HEIGHT / 2 + backFont->ascent,
		     "Bornes");
	XFreeGC (dpy, gc);
	
	for (i = 0; i < (NUM_CARDS - 1); i++) {
		init_card (&cards[i], back_gc);
	}
}

init_color_cards ()
{
	deck = color_deck;
	blank = color_blank;
	cards = color_cards;
	init_cards (colorMap[GREY_COLOR].gc);
}


init_mono_cards()
{
	deck = mono_deck;
	blank = mono_blank;
	cards = color_cards;
	init_cards (colorMap[WHITE_COLOR].gc);
}

displayCard (d, w, card, x, y, clip)
Display	*d;
Window	w;
int	card;
int	x, y;
XRectangle  *clip;
{
    if (card < 0 || card >= NUM_CARDS) {
	drawIm (d, w, &blank, x, y, clip);
    } else {
	drawIm (d, w, &cards[card], x, y, clip);
    }
}

static int	oldx, oldy;
static GC	copyGC;
int		clip_cards = 1;

drawIm (d, w, c, x, y, clip)
Display		*d;
Window		w;
struct card	*c;
int		x, y;
XRectangle	*clip;
{
    int	i;
    XGCValues	gc_values;
    int	mask;
    int	srcx, srcy;
    int	dstx, dsty;
    int	width, height;

    if (!c->bits)
	init_card (c);
    if (!copyGC)
    {
	mask = GCGraphicsExposures;
	if (clip_cards) {
	    gc_values.clip_mask = card_border;
	    gc_values.clip_x_origin = x;
	    gc_values.clip_y_origin = y;
	    mask |= GCClipXOrigin|GCClipYOrigin|GCClipMask;
	}
	gc_values.graphics_exposures = False;
	copyGC = XCreateGC (d, w, mask, &gc_values);
    }
    else
    {
	if (clip_cards && (oldx != x || oldy != y))
	{
	    gc_values.clip_x_origin = x;
	    gc_values.clip_y_origin = y;
	    XChangeGC (d, copyGC, GCClipXOrigin|GCClipYOrigin, &gc_values);
	}
    }
    srcx = 0;
    srcy = 0;
    dstx = x;
    dsty = y;
    width = WIDTH;
    height = HEIGHT;
    if (clip) {
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
    XCopyArea(d, c->bits, w, copyGC, srcx, srcy, width, height, dstx, dsty);
    oldx = x;
    oldy = y;
}
