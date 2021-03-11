# include	<X11/Xlib.h>
# include	"color.h"

extern	Display	*dpy;
extern	int	screen;
extern	Window	xwindow;
extern	XFontStruct	*font, *backFont;
extern	Pixmap	fill;
extern	GC	text_gc, xor_gc;

# define MAXBITMAPS	4

struct card {
	struct card_init {
		char	*bits;
		int	color;
	} init[MAXBITMAPS];
	char	*label;
	Pixmap	bits;
};

extern struct card	*cards;
extern struct card	deck;
extern struct card	blank;

struct safety_offset {
	int	x;
	int	y;
};

extern struct safety_offset	safety_offsets[4];

# define PAD_CARD	(5)
# define MILE_OFFSET	(5)
# define PAD_TEXT	(20)
# define DIST_HEIGHT	(15)
# define DIST_WIDTH	((WIDTH + PAD_CARD) * 5 - PAD_CARD)
# define DIST_MARK	(4)

# define COMP_HAND_X	(PAD_CARD)
# define COMP_HAND_Y	(-HEIGHT + font->descent)
# define COMP_DIST_TX	(PAD_CARD + (WIDTH + PAD_CARD) * 2)
# define COMP_DIST_TY	(PAD_CARD)
# define COMP_DIST_MX	(COMP_DIST_TX)
# define COMP_DIST_MY	(COMP_DIST_TY + PAD_TEXT)
# define COMP_DIST_X	(COMP_DIST_MX)
# define COMP_DIST_Y	(COMP_DIST_MY + DIST_MARK + 1)

# define COMP_PLAY_X	PAD_CARD
# define COMP_PLAY_Y	(COMP_DIST_Y + DIST_HEIGHT + PAD_CARD)

# define COMP_SAFE_X	(COMP_PLAY_X + ((WIDTH + PAD_CARD) * 7))
# define COMP_SAFE_Y	COMP_PLAY_Y

# define COMP_CARD_TX	PAD_CARD
# define COMP_CARD_TY	(COMP_PLAY_Y + HEIGHT + 6 * MILE_OFFSET + PAD_CARD)
# define COMP_CARD_X	COMP_CARD_TX
# define COMP_CARD_Y	(COMP_CARD_TY + PAD_TEXT)

# define MESS_X		(PAD_CARD)
# define MESS_Y		(COMP_PLAY_Y + HEIGHT + 6 * MILE_OFFSET + PAD_CARD + PAD_TEXT)
# define MESS_W		(150)
# define MESS_H		(font->ascent + font->descent)
# define PROMPT_X	MESS_X
# define PROMPT_Y	(MESS_Y + PAD_TEXT)
# define PROMPT_W	(MESS_W)
# define PROMPT_H	(MESS_H)
# define ERROR_X	PROMPT_X
# define ERROR_Y	(PROMPT_Y + PAD_TEXT)
# define ERROR_W	(MESS_W)
# define ERROR_H	(MESS_H)

# define QUIT_X		(MESS_X)
# define QUIT_Y		(ERROR_Y + PAD_TEXT + PAD_CARD)
# define SAVE_X		(QUIT_X + 75)
# define SAVE_Y		(QUIT_Y)

# define DISCARD_TX	(MESS_X + MESS_W + PAD_CARD)
# define DISCARD_TY	(MESS_Y - PAD_TEXT)
# define DISCARD_X	(DISCARD_TX)
# define DISCARD_Y	(DISCARD_TY + PAD_TEXT)

# define DECK_TX	(DISCARD_X + WIDTH + PAD_CARD)
# define DECK_TY	(DISCARD_TY)
# define DECK_X		(DECK_TX)
# define DECK_Y		(DISCARD_Y)

# define SCORE_W	(150)
# define SCORE_H	(font->ascent + font->descent)
# define SCORE_N	13
# define SCORE_X	(DECK_X + WIDTH + PAD_CARD + SCORE_W)
# define SCORE_Y	(DECK_TY)

# define HUM_DIST_TX	(COMP_DIST_TX)
# define HUM_DIST_TY	(SCORE_Y + SCORE_N * SCORE_H + PAD_CARD)
# define HUM_DIST_MX	(HUM_DIST_TX)
# define HUM_DIST_MY	(HUM_DIST_TY + PAD_TEXT)
# define HUM_DIST_X	(HUM_DIST_MX)
# define HUM_DIST_Y	(HUM_DIST_MY + DIST_MARK + 1)

# define HUM_PLAY_X	PAD_CARD
# define HUM_PLAY_Y	(HUM_DIST_Y + DIST_HEIGHT + PAD_CARD)
# define HUM_SAFE_X	(HUM_PLAY_X + ((WIDTH + PAD_CARD) * 7))
# define HUM_SAFE_Y	(HUM_PLAY_Y)

# define HUM_HAND_X	PAD_CARD
# define HUM_HAND_Y	(HUM_PLAY_Y + HEIGHT + 6 * MILE_OFFSET + PAD_CARD)

# define WINDOW_WIDTH	(HUM_SAFE_X + (WIDTH + PAD_CARD) * 2)
# define WINDOW_HEIGHT	(HUM_HAND_Y + HEIGHT + PAD_CARD)
