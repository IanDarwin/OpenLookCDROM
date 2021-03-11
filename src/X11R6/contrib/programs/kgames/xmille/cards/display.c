# include	<X/Xlib.h>
# include	"background.h"
# include	"color.h"

struct color {
	char	*name;
	int	pixel;
};

struct color colorMap[NUM_COLOR] = {
	"black",	0,
	"white",	0,
	"red",		0,
	"green",	0,
	"light gray",	0,
	"blue",		0,
};

struct card_init {
	short	*bits;
	short	*mask;
	int	color;
};

extern short	go_bits[], go_mask_bits[];
extern short	stop_bits[], stop_mask_bits[];
extern short	right_bits[], right_mask_bits[];
extern short	speed_bits[], speed_mask_bits[];
extern short	end_bits[], end_mask_bits[];
extern short	accident_bits[], accident_mask_bits[];
extern short	repairs_bits[], repairs_mask_bits[];
extern short	ace_bits[], ace_mask_bits[];
extern short	flat_bits[], flat_mask_bits[];
extern short	spare_bits[], spare_mask_bits[];
extern short	puncture_bits[], puncture_mask_bits[];
extern short	out_bits[], out_mask_bits[];
extern short	gas_bits[], gas_mask_bits[];
extern short	extra_bits[], extra_mask_bits[];
extern short	miles_mask_bits[];
extern short	_25_bits[], _50_bits[], _75_bits[], _100_bits[], _200_bits[];

struct card_init card_inits[] = {
{
	go_bits,
	go_mask_bits,
	GREEN_COLOR,
},
{
	stop_bits,
	stop_mask_bits,
	RED_COLOR,
},
{
	right_bits,
	right_mask_bits,
	RED_COLOR,
},
{
	speed_bits,
	speed_mask_bits,
	RED_COLOR,
},
{
	end_bits,
	end_mask_bits,
	GREEN_COLOR,
},
{
	accident_bits,
	accident_mask_bits,
	RED_COLOR,
},
{
	repairs_bits,
	repairs_mask_bits,
	GREEN_COLOR,
},
{
	ace_bits,
	ace_mask_bits,
	BLUE_COLOR,
},
{
	flat_bits,
	flat_mask_bits,
	RED_COLOR,
},
{
	spare_bits,
	spare_mask_bits,
	GREEN_COLOR,
},
{
	puncture_bits,
	puncture_mask_bits,
	BLUE_COLOR,
},
{
	out_bits,
	out_mask_bits,
	RED_COLOR,
},
{
	gas_bits,
	gas_mask_bits,
	GREEN_COLOR,
},
{
	extra_bits,
	extra_mask_bits,
	BLUE_COLOR,
},
{
	_25_bits,
	miles_mask_bits,
	BLUE_COLOR,
},
{
	_50_bits,
	miles_mask_bits,
	BLUE_COLOR,
},
{
	_75_bits,
	miles_mask_bits,
	BLUE_COLOR,
},
{
	_100_bits,
	miles_mask_bits,
	BLUE_COLOR,
},
{
	_200_bits,
	miles_mask_bits,
	BLUE_COLOR,
},
};

# define NUM_CARDS	(sizeof (card_inits) / sizeof (card_inits[0]))

struct plane {
	Bitmap	bits;
	int	pixel;
};

struct card {
	int	nPlanes;
	struct plane	planes[5];
};

struct card cards[NUM_CARDS];

Window	w;

# define WINDOW_WIDTH	1000
# define WINDOW_HEIGHT	700

main ()
{
	Color	hardware_color, exact_color;
	XEvent	rep;
	Bitmap	fill;
	Pixmap	background;
	Pixmap	border;
	int	i;

	XOpenDisplay ("");
	for (i = 0; i < NUM_COLOR; i++) {
		XGetColor (colorMap[i].name, &hardware_color, &exact_color);
		colorMap[i].pixel = hardware_color.pixel;
	}
	fill = XStoreBitmap (WIDTH, HEIGHT, fill_bits);
	for (i = 0; i < NUM_CARDS; i++) {
		cards[i].planes[2].bits = XStoreBitmap (WIDTH, HEIGHT, card_inits[i].bits);
		cards[i].planes[2].pixel = colorMap[card_inits[i].color].pixel;
		cards[i].planes[1].bits = XStoreBitmap (WIDTH, HEIGHT, card_inits[i].mask);
		cards[i].planes[1].pixel = colorMap[BLACK_COLOR].pixel;
		cards[i].planes[0].bits = fill;
		cards[i].planes[0].pixel = colorMap[WHITE_COLOR].pixel;
		cards[i].nPlanes = 3;
	}
	background = XMakePixmap (0, colorMap[GREY_COLOR].pixel, colorMap[WHITE_COLOR].pixel);
	border = XMakePixmap (0, colorMap[WHITE_COLOR].pixel, colorMap[GREY_COLOR].pixel);
	w = XCreateWindow (RootWindow, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT, 1,
		WhitePixmap, background);
	XMapWindow (w);
	XSelectInput (w, ExposeWindow);
	displayAll ();
	for (;;) {
		XNextEvent (&rep);
		switch (rep.type) {
		case ExposeWindow:
			displayAll ();
			break;
		}
	}
}

displayAll ()
{
	struct card	*card;
	int		x, y;
	int		i;

	x = 10;
	y = 10;
	for (i = 0; i < NUM_CARDS; i++) {
		displayOne (&cards[i], x, y);
		x += WIDTH + 20;
		if (x + WIDTH > WINDOW_WIDTH) {
			x = 10;
			y += HEIGHT + 20;
		}
	}
	XFlush ();
}

displayOne (card, x, y)
struct card	*card;
int		x, y;
{
	int	i;

	for (i = 0; i < card->nPlanes; i++) {
		XPixFill (w, x, y, WIDTH, HEIGHT,
			card->planes[i].pixel, card->planes[i].bits, GXcopy, AllPlanes);
	}
}
