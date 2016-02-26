/*
 *	Copyright 1988, Mark Holm
 *			Exceptions
 *
 *	Acknowledgments to Dorothy Robinson for her artistic
 *	 abilities in drawing the icons and to Jim Batch for
 *	 technical support and graphical concepts (which I abandoned in favor
 *       of the easy way out).
 *
 *	Permission is given to copy and distribute for non-profit purposes.
 *
 *	Revision History
 *	----------------
 *
 *	1.0 Initial release
 *
 *	1.1 removed non-working reverse video cursor code
 *	    added blank tiles for hidden tiles to stop cheating.
 *
 *	1.2 incorporated sun fix for panel messages and added new
 * 	    shuffling algorithm.
 */

#ifndef lint
static char *rcs = "$header$ Copyright 1988 Mark Holm";
#endif !lint

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sun/fbio.h>
#include <suntool/sunview.h>
#include <suntool/panel.h>
#include <suntool/canvas.h>
#include <suntool/icon.h>
#include <sunwindow/notify.h>
#include <pixrect/pixrect.h>

#include "mahjongg.h"

void			die();
void			build_image();
void			place_tiles();
Pixrect			*color_button();

#ifdef RELEASE_1
/* Black and white closed icon image */

static short		icon_image[] = {
#include "mahjongg.icon"
};
#else
extern short icon_image[];
#endif
DEFINE_ICON_FROM_IMAGE(icon, icon_image); /* Black and white icon */
struct icon	*cicon;			  /* storage for color icon */

/* externals */

extern void		quit_proc();
extern void		undo_proc();
extern void		new_proc();
extern void		again_proc();
extern void		help_proc();
extern void		board_num_proc();
extern void		play_back_proc();
extern void		play_event_proc();
extern void		panel_msg();

extern short		stick_image[];
extern int		undo_count;

/* overlap globals */


Frame			main_frame;
Panel			play_panel, message_panel;
Panel_item		TL_hundred;
Panel_item		TL_ten;
Panel_item		TL_one;
Panel_item		message;
Panel_item		tile[144];
Panel_item		tiles_left[3];
Cursor			play_cursor;
boolean			BandW = FALSE;
Tiles			*board[144];
Selected		selected[2];
Selected		last_item;
int			tile_count;
char			state[256];

/* local globals */

Pixwin			*frame_pw;
Panel_item		tile_message;
Panel_item		cp1;
Panel_item		cp2;
Panel_item		cp3;
Panel_item		cp4;
Panel_item		help;
Panel_item		again;
Panel_item		new;
Panel_item		quit;
Panel_item		undo;
Panel_item		board_num;
struct timeval		*tp;
struct timezone		*tz;
int			seed;

/* define color map */

#include "color.h"

main(argc, argv)
int argc;
char **argv;
{
    struct pixfont	*panel_font;
    struct pixfont	*dummy_font;
	   int		 i;
	   int		 middle;
    struct fbtype	 fb;
	   int		 open_stat;

    /* determine type of frame buffer and set BandW accordingly */

    open_stat = open("/dev/fb", O_RDONLY);
    (void) ioctl(open_stat, FBIOGTYPE, &fb);

    if ( (fb.fb_type == FBTYPE_SUN1BW) ||
	 (fb.fb_type == FBTYPE_SUN2BW) )
		BandW = TRUE;

    /* initialize random number generator seed */

    tp = (struct timeval *) malloc(sizeof(struct timeval));
    tz = (struct timezone *) malloc(sizeof(struct timezone));
    gettimeofday(tp, tz);
    (void) initstate((unsigned) (tp->tv_sec % 255), state, 256); /* initialize random state array */
    seed = RANDOM(20011);
    free(tp);
    free(tz);

    /* create main frame */
   
    main_frame = window_create(NULL, FRAME,
			       FRAME_SHOW_LABEL, FALSE,
			       FRAME_SUBWINDOWS_ADJUSTABLE, FALSE,
			       FRAME_ICON, &icon,
			       FRAME_ARGC_PTR_ARGV, &argc, argv,
			       WIN_HEIGHT, FRAME_Y_MAX,
			       WIN_WIDTH, FRAME_X_MAX,
			       0);

    /* parse arguments */
    for(argc--, argv++; argc > 0; argc--, argv++)
	if (argv[0][0] = '-')
	    switch (argv[0][1]) {
	    case 'c': /* force color */
		BandW = FALSE;
		break;
	    case 'b': /* force black and white */
		BandW = TRUE;
		break;
	    case 'n': /* set board number */
		if(argc-- == 0)
		    die("Usage: mahjongg [-b] [-c] [-n #]\n", 0);
		argv++;
    		sscanf(argv[0] , "%d", &seed);
		if (seed > 20011) {
		    printf("Board numbers must be < 20011");
		    seed %= 20011;
		}
		break;
	    default:
		die("Usage: mahjongg [-b] [-c] [-n #]\n", 0);
		break;
	    }
	else
	    die("Usage: mahjongg [-b] [-c] [-n #]\n", 0);
	    
    /* if color then apply color icon to window icon */

    if(!BandW) {
	cicon = (struct icon *) window_get(main_frame, FRAME_ICON);
	cicon->ic_mpr = &cicon_image;
     }

    /* get pixwin to apply color maps */

    frame_pw  = (Pixwin *) window_get(main_frame, WIN_PIXWIN, 0);

    /* apply color maps to frame pixwin */

    pw_setcmsname(frame_pw, "mahjongg");
    pw_putcolormap(frame_pw, 0, MAX_COLORS+1, red, green, blue);
    if (BandW)
    	pw_putcolormap(frame_pw, 0, 1, &(red[WHITE]), &(green[WHITE]), &(blue[WHITE]));

    /* set inheritable colors */

    window_set(main_frame, FRAME_INHERIT_COLORS, TRUE, 0);

    /* set up the panel on the right hand side */
    
    dummy_font = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.r.7");
    panel_font = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.b.14");
    message_panel = window_create(main_frame, PANEL,
			  WIN_HEIGHT, MESS_Y_MAX,
			  WIN_WIDTH, MESS_X_MAX,
			  WIN_X, 0,
			  WIN_Y, 0,
			  WIN_FONT, dummy_font,
			  0);

    /* determine middle of panel */

    middle = (MESS_X_MAX / 2);

    /* create tile counters */

    TL_hundred = panel_create_item(message_panel, PANEL_MESSAGE,
				 PANEL_LABEL_IMAGE,
				   (BandW) ? &NUM1 : &cNUM1,
				 PANEL_ITEM_Y, ATTR_ROW(7),
				 PANEL_ITEM_X, X_LOC,
				 0);
    TL_ten = panel_create_item(message_panel, PANEL_MESSAGE,
				 PANEL_LABEL_IMAGE,
				   (BandW) ? &NUM4 : &cNUM4,
				 PANEL_ITEM_Y, ATTR_ROW(7),
				 PANEL_ITEM_X, X_LOC +  W_BASE_TILE,
				 0);
    TL_one = panel_create_item(message_panel, PANEL_MESSAGE,
				 PANEL_LABEL_IMAGE,
				   (BandW) ? &NUM4 : &cNUM4,
				 PANEL_ITEM_Y, ATTR_ROW(7),
				 PANEL_ITEM_X, X_LOC + (W_BASE_TILE * 2),
				 0);

    /* create game label messages */

    cp1 = panel_create_item(message_panel, PANEL_MESSAGE,
			     PANEL_LABEL_FONT, panel_font,
			     PANEL_LABEL_STRING, "MAHJONGG",
			     PANEL_ITEM_Y, ATTR_ROW(1) - 11,
			     PANEL_ITEM_X, middle - 65,
			     0);
    cp2 = panel_create_item(message_panel, PANEL_MESSAGE,
			     PANEL_LABEL_FONT, dummy_font,
			     PANEL_LABEL_STRING, "Copyright 1988",
			     PANEL_ITEM_Y, ATTR_ROW(2),
			     PANEL_ITEM_X, middle - 70,
			     0);
    cp3 = panel_create_item(message_panel, PANEL_MESSAGE,
			     PANEL_LABEL_FONT, dummy_font,
			     PANEL_LABEL_STRING, "Mark A. Holm",
			     PANEL_ITEM_Y, ATTR_ROW(3),
			     PANEL_ITEM_X, middle - 65,
			     0);
    cp3 = panel_create_item(message_panel, PANEL_MESSAGE,
			     PANEL_LABEL_FONT, dummy_font,
			     PANEL_LABEL_STRING, "Exceptions",
			     PANEL_ITEM_Y, ATTR_ROW(4),
			     PANEL_ITEM_X, middle - 60,
			     0);
    tile_message = panel_create_item(message_panel, PANEL_MESSAGE,
                                     PANEL_LABEL_FONT, panel_font,
                                     PANEL_LABEL_STRING, "Tiles Remaining",
                                     PANEL_ITEM_Y, ATTR_ROW(5),
                                     PANEL_ITEM_X,  X_LOC + 20,
                                     0);

    /* create seed item */

    board_num = panel_create_item(message_panel, PANEL_TEXT,
                                     PANEL_LABEL_FONT, panel_font,
                                     PANEL_VALUE_FONT, panel_font,
                                     PANEL_LABEL_STRING, "Board Number : ",
				     PANEL_VALUE, "",
                                     PANEL_ITEM_Y, ATTR_ROW(6),
                                     PANEL_ITEM_X,  middle,
				     PANEL_NOTIFY_PROC, board_num_proc,
                                     0);

    /* create control buttons */

    help = panel_create_item(message_panel, PANEL_BUTTON,
				 PANEL_ITEM_Y, ATTR_ROW(8),
				 PANEL_ITEM_X, middle,
                                 PANEL_LABEL_IMAGE,
                                   color_button(panel_button_image(message_panel,
								  "HELP",
								  6,
                                     			          panel_font),
						CYAN),
				 PANEL_NOTIFY_PROC, help_proc,
				 0);

    again = panel_create_item(message_panel, PANEL_BUTTON,
                                 PANEL_LABEL_IMAGE,
                                   color_button(panel_button_image(message_panel,
								  "AGAIN",
								  6,
                                     			          panel_font),
						YELLOW),
				 PANEL_NOTIFY_PROC, again_proc,
				 0);

    new = panel_create_item(message_panel, PANEL_BUTTON,
                                 PANEL_LABEL_IMAGE,
                                   color_button(panel_button_image(message_panel,
								  "NEW",
								  6,
                                     			          panel_font),
						GREEN),
				 PANEL_NOTIFY_PROC, new_proc,
				 0);

    undo = panel_create_item(message_panel, PANEL_BUTTON,
                                 PANEL_LABEL_IMAGE,
                                   color_button(panel_button_image(message_panel,
								  "UNDO",
								  6,
                                     			          panel_font),
						MAGENTA),
				 PANEL_NOTIFY_PROC, undo_proc,
				 PANEL_SHOW_ITEM, TRUE,
				 0);

    quit = panel_create_item(message_panel, PANEL_BUTTON,
                                 PANEL_LABEL_IMAGE,
                                   color_button(panel_button_image(message_panel,
								  "QUIT",
								  6,
                                     			          panel_font),
						RED),
				 PANEL_NOTIFY_PROC, quit_proc,
				 0);

    /* place conceled message */

    message = panel_create_item(message_panel, PANEL_MESSAGE,
                                 PANEL_LABEL_FONT, panel_font,
                                 PANEL_ITEM_Y, ATTR_ROW(10),
				 PANEL_ITEM_X, middle,
				 PANEL_LABEL_STRING, "",
				 PANEL_SHOW_ITEM, FALSE,
				 0);

     /* create cursor for play panel*/

    play_cursor = cursor_create(CURSOR_IMAGE, &stick,
				CURSOR_XHOT, 8,
				CURSOR_YHOT, 8,
				0);

    if (!BandW) {

	cursor_set(play_cursor, CURSOR_OP,
				 (PIX_SRC^PIX_DST) | PIX_COLOR( YELLOW ),
				0);
     }

    /* set up panel for the play */
    
    play_panel = window_create(main_frame, PANEL,
			   WIN_CONSUME_PICK_EVENTS,
			    WIN_NO_EVENTS, WIN_MOUSE_BUTTONS, WIN_UP_EVENTS, WIN_LEFT_KEYS, 0,
			   WIN_HEIGHT, PLAY_Y_MAX,
			   WIN_WIDTH, PLAY_X_MAX,
			   WIN_X, 0,
			   WIN_Y, MESS_Y_MAX + BORDER,
			   WIN_CURSOR, play_cursor,
			   PANEL_PAINT, PANEL_NONE,
			   PANEL_BACKGROUND_PROC, play_back_proc,
			   PANEL_EVENT_PROC, play_event_proc,
			   CANVAS_RETAINED, TRUE,
			   0);

	window_set(message_panel, WIN_INPUT_DESIGNEE,
			   window_get(play_panel, WIN_DEVICE_NUMBER),
			   0);

    /* build board image */

    build_image(FALSE);
    place_tiles(TRUE);

    /* start main processing */
    		  
    window_main_loop(main_frame);
    exit(0);
}


/* die because of some UNIX error */
void die(message, pperr)
char *message;
int pperr;
{
    fprintf(stderr, message);
    if (pperr)
	perror("mahjongg");
    exit(1);
}

void place_tiles(newboard)
boolean		newboard;

{
    int 	i;
    int 	j;
    int 	k;
    int 	x_loc;
    int 	y_loc;

    /* check if not new and destroy existing panel buttons */

    if (!newboard)
	for(i = 0; i < 144; i++)
		panel_destroy_item(tile[i]);

    /* place tiles */

        /* row 1 level 1 */

	for(i = 0,
	    x_loc = COL2;
	    i < 12;
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW1,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

	/* row 2 level 1 */

	for(x_loc = COL4,
	    j = 0;
	    j < 8;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW2,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

	/* row 3 level 1 */

	for(x_loc = COL3,
	    j = 0;
	    j < 10;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW3,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

	/* row 4 1/2 level 1 */

	/* Left */

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 COL1,
				 		PANEL_ITEM_Y,
						 ROW4pt5,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

		i++; /* increment tile counter */

	/* row 4 level 1 */

	for(x_loc = COL2,
	    j = 0;
	    j < 12;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW4,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

	/* row 5 level 1 */

	for(x_loc = COL2,
	    j = 0;
	    j < 12;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW5,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);
	/* row 4 1/2 level 1 */

	/* Right */

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 COL14,
				 		PANEL_ITEM_Y,
						 ROW4pt5,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

		i++; /* increment tile counter */

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 COL15,
				 		PANEL_ITEM_Y,
						 ROW4pt5,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

		i++; /* increment tile counter */

	/* row 6 level 1 */

	for(x_loc = COL3,
	    j = 0;
	    j < 10;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW6,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

	/* row 7 level 1 */

	for(x_loc = COL4,
	    j = 0;
	    j < 8;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW7,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

        /* row 8 level 1 */

	for(j = 0,
	    x_loc = COL2;
	    j < 12;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  (board[i]->top_free) ?
						    board[i]->image :
						      (BandW) ? &BLANK : &cBLANK,
						PANEL_ITEM_X,
						 x_loc,
				 		PANEL_ITEM_Y,
						 ROW8,
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

	/* rows 1-6 level 2 */

	for(y_loc = ROW2 - B_TILE_SHADOW,
	    j = 0;
	    j < 6;
	    j++,
	    y_loc += H_BASE_TILE) 

		for(x_loc = COL5 - S_TILE_SHADOW,
		    k = 0;
		    k < 6;
		    i++,
		    k++,
		    x_loc += W_BASE_TILE) 

    			tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
							PANEL_LABEL_IMAGE,
							  (board[i]->top_free) ?
							    board[i]->image :
							      (BandW) ? &BLANK : &cBLANK,
							PANEL_ITEM_X,
							 x_loc,
				 			PANEL_ITEM_Y,
							 y_loc,
							PANEL_SHOW_ITEM,
							 TRUE,
							PANEL_CLIENT_DATA,
							 (caddr_t) board[i],
				 			0);

	/* rows 1-4 level 3 */

	for(y_loc = ROW3 - (B_TILE_SHADOW * 2),
	    j = 0;
	    j < 4;
	    j++,
	    y_loc += H_BASE_TILE) 

		for(x_loc = COL6 - (S_TILE_SHADOW * 2),
		    k = 0;
		    k < 4;
		    i++,
		    k++,
		    x_loc += W_BASE_TILE) 

    			tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
							PANEL_LABEL_IMAGE,
							  (board[i]->top_free) ?
							    board[i]->image :
							      (BandW) ? &BLANK : &cBLANK,
							PANEL_ITEM_X,
							 x_loc,
				 			PANEL_ITEM_Y,
							 y_loc,
							PANEL_SHOW_ITEM,
							 TRUE,
							PANEL_CLIENT_DATA,
							 (caddr_t) board[i],
				 			0);

	/* rows 1-2 level 4 */

	for(y_loc = ROW4 - (B_TILE_SHADOW * 3),
	    j = 0;
	    j < 2;
	    j++,
	    y_loc += H_BASE_TILE) 

		for(x_loc = COL7 - (S_TILE_SHADOW * 3),
		    k = 0;
		    k < 2;
		    i++,
		    k++,
		    x_loc += W_BASE_TILE) 

    			tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
							PANEL_LABEL_IMAGE,
							  board[i]->image,
							PANEL_ITEM_X,
							 x_loc,
				 			PANEL_ITEM_Y,
							 y_loc,
							PANEL_SHOW_ITEM,
							 TRUE,
							PANEL_CLIENT_DATA,
							 (caddr_t) board[i],
				 			0);

	/* Cap tile */

    		tile[i] = panel_create_item(play_panel, PANEL_BUTTON,
						PANEL_LABEL_IMAGE,
						  board[i]->image,
						PANEL_ITEM_X,
						 COL7pt5 - (S_TILE_SHADOW * 4),
				 		PANEL_ITEM_Y,
						 ROW4pt5 - (B_TILE_SHADOW * 4),
						PANEL_SHOW_ITEM,
						 TRUE,
						PANEL_CLIENT_DATA,
						 (caddr_t) board[i],
				 		0);

    /* paint panel */

    panel_paint(play_panel, PANEL_NO_CLEAR);

   /* clear stand_by message  and release input mask */

   panel_msg((char *)0, &stick);

}

void build_image(oldimage)
boolean oldimage;

{
int 	i;
int 	j;
boolean ok;
boolean dir;
char	seed_text[80];
int   tmp_pool[144];
int   tmp_tiles_left;
int   pos;

	/* initialize selected structures */

        selected[0].filled = FALSE;
        selected[1].filled = FALSE;
        last_item.filled   = FALSE;
        undo_count   	   = -1;

        panel_set(message,   PANEL_SHOW_ITEM,
			       FALSE,
			      0);
        panel_set(TL_hundred, PANEL_LABEL_IMAGE,
			       (BandW) ? &NUM1 : &cNUM1,
			      PANEL_SHOW_ITEM,
			       TRUE,
			      0);
        panel_set(TL_ten    , PANEL_LABEL_IMAGE,
			       (BandW) ? &NUM4 : &cNUM4,
			      PANEL_SHOW_ITEM,
			       TRUE,
			      0);
        panel_set(TL_one    , PANEL_LABEL_IMAGE,
			       (BandW) ? &NUM4 : &cNUM4,
			      0);

	/* display current seed in text item */

    	sprintf(seed_text, "%d", seed);
    	panel_set(board_num, PANEL_VALUE, seed_text, 0);

	/* show stand_by message while building image  and grab all input */

	panel_msg( "Building board. Please wait.", &waiting);

	/* initialize random number counter */

	(void) srandom(seed);

	tile_count = 144;

	/* initialize  tmp_pool */
        for(j = 0; j < 136; j++) tmp_pool[j] = j/4; /* tiles * 4 */
        for(j=136; j<144; j++) tmp_pool[j] = j- 102; /* flowers & seasons */
                                                   /* 136 --> 34 */
                                                   /* 143 --> 41 */
        tmp_tiles_left = 144;



	/* assign values to each location. Board is built from upper left *
	 * to lower right, bottom to top. Exception are left tile for row *
	 * 4pt5 is before rows 4 and 5, and right tiles for row 4.5 are   *
	 * after rows 4 and 5                                             */

	for(j = 0; j < 144; j++) {
	    if (board[j] == NULL) /* intialize array */
		board[j] = (Tiles *) malloc(sizeof(Tiles));

	    if (!oldimage) { /* not repeating last board */

	       /* Shuffle/pick tile */

               pos = RANDOM(tmp_tiles_left);
               i= tmp_pool[pos];
               for(;pos<tmp_tiles_left; pos++) tmp_pool[pos] = tmp_pool[pos+1];
               tmp_tiles_left--;

		/* all flowers and all seasons */

		board[j]->value = (i >= 34) ? ((i >= 38) ? 35 : 34) : i;
		switch(i) {

		case 0: board[j]->image = (BandW) ? &DOT1 : &cDOT1;
			break;
		case 1: board[j]->image = (BandW) ? &DOT2 : &cDOT2;
			break;
		case 2: board[j]->image = (BandW) ? &DOT3 : &cDOT3;
			break;
		case 3: board[j]->image = (BandW) ? &DOT4 : &cDOT4;
			break;
		case 4: board[j]->image = (BandW) ? &DOT5 : &cDOT5;
			break;
		case 5: board[j]->image = (BandW) ? &DOT6 : &cDOT6;
			break;
		case 6: board[j]->image = (BandW) ? &DOT7 : &cDOT7;
			break;
		case 7: board[j]->image = (BandW) ? &DOT8 : &cDOT8;
			break;
		case 8: board[j]->image = (BandW) ? &DOT9 : &cDOT9;
			break;
		case 9: board[j]->image = (BandW) ? &BAM1 : &cBAM1;
			break;
		case 10: board[j]->image = (BandW) ? &BAM2 : &cBAM2;
			break;
		case 11: board[j]->image = (BandW) ? &BAM3 : &cBAM3;
			break;
		case 12: board[j]->image = (BandW) ? &BAM4 : &cBAM4;
			break;
		case 13: board[j]->image = (BandW) ? &BAM5 : &cBAM5;
			break;
		case 14: board[j]->image = (BandW) ? &BAM6 : &cBAM6;
			break;
		case 15: board[j]->image = (BandW) ? &BAM7 : &cBAM7;
			break;
		case 16: board[j]->image = (BandW) ? &BAM8 : &cBAM8;
			break;
		case 17: board[j]->image = (BandW) ? &BAM9 : &cBAM9;
			break;
		case 18: board[j]->image = (BandW) ? &CHA1 : &cCHA1;
			break;
		case 19: board[j]->image = (BandW) ? &CHA2 : &cCHA2;
			break;
		case 20: board[j]->image = (BandW) ? &CHA3 : &cCHA3;
			break;
		case 21: board[j]->image = (BandW) ? &CHA4 : &cCHA4;
			break;
		case 22: board[j]->image = (BandW) ? &CHA5 : &cCHA5;
			break;
		case 23: board[j]->image = (BandW) ? &CHA6 : &cCHA6;
			break;
		case 24: board[j]->image = (BandW) ? &CHA7 : &cCHA7;
			break;
		case 25: board[j]->image = (BandW) ? &CHA8 : &cCHA8;
			break;
		case 26: board[j]->image = (BandW) ? &CHA9 : &cCHA9;
			break;
		case 27: board[j]->image = (BandW) ? &GRED : &cGRED;
			break;
		case 28: board[j]->image = (BandW) ? &REDD : &cREDD;
			break;
		case 29: board[j]->image = (BandW) ? &WHTD : &cWHTD;
			break;
		case 30: board[j]->image = (BandW) ? &EAST : &cEAST;
			break;
		case 31: board[j]->image = (BandW) ? &WEST : &cWEST;
			break;
		case 32: board[j]->image = (BandW) ? &SOUT : &cSOUT;
			break;
		case 33: board[j]->image = (BandW) ? &NORT : &cNORT;
			break;
		case 34: board[j]->image = (BandW) ? &AUT : &cAUT;
			break;
		case 35: board[j]->image = (BandW) ? &SUM : &cSUM;
			break;
		case 36: board[j]->image = (BandW) ? &SPR : &cSPR;
			break;
		case 37: board[j]->image = (BandW) ? &WIN : &cWIN;
			break;
		case 38: board[j]->image = (BandW) ? &ORC : &cORC;
			break;
		case 39: board[j]->image = (BandW) ? &MUM : &cMUM;
			break;
		case 40: board[j]->image = (BandW) ? &BAM : &cBAM;
			break;
		case 41: board[j]->image = (BandW) ? &PLM : &cPLM;
			break;
		}

	}
	/* establish default values */

		board[j]->left_free     = FALSE;
		board[j]->right_free    = FALSE;
		board[j]->top_free      = TRUE;
		board[j]->left_next[0]  = j - 1;
		board[j]->left_next[1]  = 999;
		board[j]->right_next[0] = j + 1;
		board[j]->right_next[1] = 999;
		board[j]->covered[0]    = 999;
		board[j]->covered[1]    = 999;
		board[j]->covered[2]    = 999;
		board[j]->covered[3]    = 999;
		board[j]->removed       = FALSE;

	/* setup special cases */

		switch (j) {
		case 139:
		case 141: 
			board[j]->top_free = FALSE;
		case 0:
		case 12:
		case 20:
		case 30:
		case 57:
		case 67:
		case 75:
		case 87:
		case 93:
		case 99:
		case 105:
		case 111:
		case 117:
		case 123:
		case 127:
		case 131:
		case 135:
			board[j]->left_free = TRUE;
			board[j]->left_next[0] = 999;
			break;
		case 140:
		case 142:
			board[j]->top_free = FALSE;
		case 11:
		case 19:
		case 29:
		case 56:
		case 66:
		case 74:
		case 86:
		case 92:
		case 98:
		case 104:
		case 110:
		case 116:
		case 122:
		case 126:
		case 130:
		case 134:
		case 138:
			board[j]->right_free = TRUE;
			board[j]->right_next[0] = 999;
			break;
		case 143:
			board[j]->right_free = TRUE;
			board[j]->left_next[0] = 999;
			board[j]->left_free = TRUE;
			board[j]->right_next[0] = 999;
			board[j]->covered[0] = 139;
			board[j]->covered[1] = 140;
			board[j]->covered[2] = 141;
			board[j]->covered[3] = 142;
			break;
		case 42:
			board[j]->right_next[0] = 55;
			break;
		case 43:
			board[j]->left_next[0] = 30;
			break;
		case 55:
			board[j]->left_next[1] = 42;
			break;
		}
			
	}

	/* special case (did not fit in above) */

	board[30]->right_next[1] = 43;

	/* set top_free flags  and covered pointers */

	for(i = 87, j = 13; i < 143; i++, j++) {
		board[i]->covered[0] = j;
		board[j]->top_free = FALSE;
		switch(j) {
			case 97:
			case 103:
			case 109:
			case 129:
				 j += 2;
				 break;
			case 18:
			case 64:
				 j += 3;
				 break;
			case 27:
			case 39:
				 j += 6;
				 break;
			case 51:
				 j += 7;
				 break;
			case 73:
				 j += 20;
				 break;
			case 115:
				 j += 12;
				 break;
		}
	}
}

/* This is the routine that returns the colored button image */

Pixrect *color_button(pr,color)
  struct pixrect *pr;
  int color;
{
  struct pixrect *color_pr;

  if(pr == NULL)
        return(NULL);

  /* if running in b/w mode return same pixrect */
  if (BandW)
        return(pr);

  /* make new pixrect */
  color_pr = mem_create(pr->pr_size.x, pr->pr_size.y, 8);

  /* copy pr to color_pr with color added */
  pr_rop(color_pr, 0, 0, pr->pr_size.x, pr->pr_size.y,
        PIX_SRC | PIX_COLOR( color ),
        pr,0,0);

  return(color_pr);
}
