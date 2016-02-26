/*
 * XView port of Mahjongg done by Stan K Tazuma, 8/91-9/91
 * Copyright 1991, 1992.
 *
 * $Header: /home/sirius/skt/cmds/src/sun/mj/RCS/mahjongg.c,v 2.7 92/12/21 18:54:49 skt Exp $
 *
 *	Revision History
 *	----------------
 *
 *	2.0 First release of XView version - 12/20/91 by SKT
 *	2.1 Second release of XView version - 12/21/92 by SKT
 *		- No major changes.  One small change in mahjongg.c
 *		  to make a small improvement in memory requirements.
 *		  A fix to icons.c to use the proper mahjongg.icon file
 *		  if SWAP is defined.
 *
 */
static char Release_info[] = "Release 2.1";

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

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/icon.h>
#include <xview/svrimage.h>
#include <xview/cms.h>
#include <xview/font.h>
#include <xview/cursor.h>

#include "mahjongg.h"

void			die();
void			build_image();
void			place_tiles();

Icon icon;

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
extern void		message_panel_event_proc();

extern short		stick_image[];
extern short		confirm_image[];
extern short		wait_image[];

extern int		undo_count;

/* overlap globals */

Frame			main_frame;
Panel			message_panel;
Canvas			play_canvas;
Xv_Window		Play_area;
Panel_item		TL_hundred;
Panel_item		TL_ten;
Panel_item		TL_one;
Panel_item		message;
Panel_item		tile[144];

Xv_Cursor		play_cursor;
Server_image		play_cursor_si;
Server_image		stick_cursor_si;
Server_image		confirm_cursor_si;
Server_image		wait_cursor_si;

#define NUM_ICON_COUNT		10
Pixrect *Num_icon_mpr[NUM_ICON_COUNT];
Server_image Num_icon_si[NUM_ICON_COUNT];

/* 42 game tiles plus 1 for the blank tile (which comes at the end) */
#define TILE_ICON_COUNT		(42 + 1)
Pixrect *Tile_icon_mpr[TILE_ICON_COUNT];
Server_image Tile_icon_si[TILE_ICON_COUNT];

boolean Use_num_server_images = TRUE;
boolean Use_tile_server_images = TRUE;

boolean Use_panel_control_cms = FALSE;
boolean Use_colored_buttons = TRUE;
boolean Use_canvas_control_cms = FALSE;
#define panel_color(c)	(Use_panel_control_cms ? CMS_CONTROL_COLORS + c : c)
boolean Use_canvas_dynamic_cms = TRUE;	/* use a dynamic CMS rather than a
					 * static one */

boolean Invert_tiles = FALSE;		/* if true, tiles will be inverted;
					 * used for BandW mode only */
boolean Switch_bg_fg = FALSE;		/* if true, the bg and fg colors in
					 * the canvas will be switched; only
					 * used for BandW mode */
boolean Use_error_bells = FALSE;	/* if true, then the bell will be
					 * sounded when the player makes an
					 * error during play */

boolean			BandW = FALSE;	/* if true, operate in Black and
					 * White mode */

Tile			*board[144];
Tile			board_tile[144]; 
	/*
	 * Not the best way to do this, but I didn't want to
	 * change all the code that assumed that board[] was a pointer.
	 * The SunView code used malloc() for creating space for the
	 * tiles.  Since the space is never freed, and we always need to
	 * have a struct for each tile, I decided to just statically allocate
	 * the space.  Each board[i] is initialized to board_tile[i] in
	 * the build_image() routine.
	 */

Selected		selected[2];
int			tile_count;
char			state[256];

int	Display_depth;


/* local globals */

Panel_item		board_num;
int			seed;

char *Usage_msg =
"Usage: mahjongg [-b] [-c] [-n #] [-P] [-PB] [-B] [-BB]\n\
\t\t[-C] [-D] [-N] [-T] [-E]\n";

/* define color map */

#define NUM_COLORS	8
#define Mahjongg_colors		"black", "red", "green", "yellow", \
				"blue", "magenta", "cyan", "white"
#define BLACK	0
#define RED	1
#define GREEN	2
#define YELLOW	3
#define BLUE	4
#define MAGENTA	5
#define CYAN	6
#define WHITE	7

char *MjC[] = { Mahjongg_colors };
#define Mahjongg_color_list	MjC[BLACK], MjC[RED], MjC[GREEN], MjC[YELLOW],\
				MjC[BLUE], MjC[MAGENTA], MjC[CYAN], MjC[WHITE]

Cms	mahjongg_panel_cms;
Cms	mahjongg_canvas_cms;
Cms	mahjongg_icon_cms;
unsigned char	Cms_inverse_table[256];
Cms create_aligned_cms();

void repaint_proc();
boolean Clear_msg = FALSE;		/* used only by build_image() and
					 * place_tiles() to determine whether
					 * or not to clear a certain message
					 * and re-set the mouse cursor */

main(argc, argv)
int argc;
char **argv;
{
    int i;
    int middle;
    int button_x;			/* used for positioning buttons */
    time_t tp;

    /* initialize random number generator seed */
    time(&tp);
    (void) initstate((unsigned) (tp % 255), state, 256); /* initialize random state array */
    seed = RANDOM(20011);

    /* check for -help request before calling xv_init() */
    if (argc > 1 &&
		(strcmp(argv[1], "-help") == 0 ||
		 strcmp(argv[1], "-WH")   == 0)) {
	print_help();
    }

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    /*
     * Want to determine BandW before creating the frame, in order to
     * use it to control the color stuff.
     *
     * But can't parse arguments until after the xv_init(), since it may
     * have to strip out XView arguments.  Also don't want to process
     * arguments before the random number stuff is initialized above.
     */

    /* parse arguments */
    for (argc--, argv++; argc > 0; argc--, argv++)
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
		    die(Usage_msg, 0);
		argv++;
    		sscanf(argv[0] , "%d", &seed);
		if (seed > 20011) {
		    printf("Board numbers must be < 20011");
		    seed %= 20011;
		}
		break;
	    case 'N':	/* use pixrects for the numbers in the panel */
		Use_num_server_images = FALSE;
		fprintf(stderr,
"Note: you'll see warning messages when -N is used, but just ignore them.\n");
		break;
	    case 'T':	/* use pixrects for the tiles */
		Use_tile_server_images = FALSE;
		break;
	    case 'P':	/* use a control cms for the panel */
		Use_panel_control_cms = TRUE;
		if (argv[0][2] == 'B')
		    Use_colored_buttons = FALSE;
		break;
	    case 'B':	/* like 'b', but use a black background */
		/* can do -B or -BB (black on black) */
		BandW = TRUE;
		if (argv[0][2] != 'B')
		    Invert_tiles = TRUE;
		Switch_bg_fg = TRUE;
		break;
	    case 'C':	/* use a control cms for the canvas */
		Use_canvas_control_cms = TRUE;
		Use_canvas_dynamic_cms = FALSE;
		break;
	    case 'D':	/* don't use a dynamic cms for the canvas */
		Use_canvas_dynamic_cms = FALSE;
		break;
	    case 'E':	/* ring the bell on player errors */
		Use_error_bells = TRUE;
		break;

	    default:
		die(Usage_msg, 0);
		break;
	    }
	else
	    die(Usage_msg, 0);

    /* determine type of frame buffer and set BandW accordingly */
    Display_depth = get_display_depth();
    if (Display_depth < 2)
	BandW = TRUE;

    if (BandW) {
	Use_canvas_control_cms = FALSE;
	Use_canvas_dynamic_cms = FALSE;
	Use_colored_buttons = FALSE;
    }

    /* create icon */
    if (BandW) {
	icon = xv_create(XV_NULL, ICON,
			ICON_IMAGE,	&icon_image_pr,
			NULL);
    }
    else {
	/* if color then apply color icon to window icon */
	icon = xv_create(XV_NULL, ICON,
			ICON_IMAGE,	&cicon_image,
			NULL);
    }

    if (! BandW) {
	/* set up colormap segments and related stuff */

	setup_cms();

	get_cms_inverse_table(mahjongg_canvas_cms);
     }

    init_local_mem_rop();	/* set up a routine needed for pw_read()
				 * to work */

    /* create main frame */
   
    main_frame = xv_create(NULL, FRAME,
				/*
				 * Don't do this on the XView version; it
				 * takes away the name stripe at the top of
				 * the window.  The name stripe is useful
				 * because of the quick iconize button.
				 * Instead, just set the FRAME_LABEL to the
				 * empty string, to get the same effect.
				FRAME_SHOW_LABEL,	FALSE,
				*/
				FRAME_LABEL,		"",

				/*
				 * Set the X,Y coord's of the tool. If this
				 * isn't done, the tool may come up with part
				 * of the canvas window off-screen.  In this
				 * situation the high-lighting (color
				 * inversion) of the tiles happens
				 * noticeably slower.  This was a tricky one
				 * to figure out.
				 */
				XV_X,			20,
				XV_Y,			20,

				FRAME_ICON,		icon,
				XV_HEIGHT,		FRAME_Y_MAX,
				XV_WIDTH,		FRAME_X_MAX,

				/* have to set WIN_CMS when creating the frame,
				 * otherwise the icon doesn't get the right
				 * colors (so can't create the FRAME first,
				 * then set WIN_CMS later with xv_set()) */
				(BandW ? ATTR_NOP1 : WIN_CMS),
							mahjongg_icon_cms,
				NULL);

    /* set up the panel on the right hand side */

    message_panel = xv_create(main_frame, PANEL,
			XV_HEIGHT,	MESS_Y_MAX,
			XV_WIDTH,	MESS_X_MAX,
			WIN_X,		0,
			WIN_Y,		0,
			(BandW ? ATTR_NOP1 : WIN_CMS),	mahjongg_panel_cms,

			/*
			 * The PANEL_EVENT_PROC is used to provide the same
			 * behavior that the SunView version provided by
			 * making use of WIN_INPUT_DESIGNEE (see commented
			 * out code below where the use of
			 * WIN_INPUT_DESIGNEE is shown).
			 * Also required for simulating this behavior is
			 * code in event.c that manipulates the event
			 * masks and changes PANEL_BACKGROUND_PROC
			 */
			PANEL_EVENT_PROC,	message_panel_event_proc,

			XV_HELP_DATA,		"mahjongg:message_panel",
			NULL);

    /* set up the icons */
    init_num_icon_mprs();
    init_tile_icon_mprs();

    if (BandW && Invert_tiles)
	invert_tile_icon_mprs();

    if (Use_canvas_control_cms)
	shift_tile_icon_mprs();

    if (Use_tile_server_images)
	init_tile_icon_server_images();

    if (Use_panel_control_cms)
	shift_num_icon_mprs();

    if (Use_num_server_images)
	init_num_icon_server_images();

    /* create tile counters */

    TL_hundred = xv_create(message_panel, PANEL_MESSAGE,
			PANEL_LABEL_IMAGE,	Use_num_server_images ?
						   (Xv_opaque) Num_icon_si[1] :
						   (Xv_opaque) Num_icon_mpr[1],
			XV_Y,			100,
			XV_X,			X_LOC,
			XV_HELP_DATA,		"mahjongg:tiles_remaining",
			NULL);

    TL_ten = xv_create(message_panel, PANEL_MESSAGE,
			PANEL_LABEL_IMAGE,	Use_num_server_images ?
						   (Xv_opaque) Num_icon_si[4] :
						   (Xv_opaque) Num_icon_mpr[4],
			XV_Y,			100,
			XV_X,			X_LOC + W_BASE_TILE,
			XV_HELP_DATA,		"mahjongg:tiles_remaining",
			NULL);

    TL_one = xv_create(message_panel, PANEL_MESSAGE,
			PANEL_LABEL_IMAGE,	Use_num_server_images ?
						   (Xv_opaque) Num_icon_si[4] :
						   (Xv_opaque) Num_icon_mpr[4],
			XV_Y,			100,
			XV_X,			X_LOC + (W_BASE_TILE * 2),
			XV_HELP_DATA,		"mahjongg:tiles_remaining",
			NULL);


    /* create game label messages */

    /* determine middle of panel */

    middle = (MESS_X_MAX / 2);

    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"MAHJONGG",
				PANEL_LABEL_BOLD,	TRUE,
				XV_Y,			10,
				XV_X,			middle - 63,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);

    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"Copyright 1988",
				XV_Y,			25,
				XV_X,			middle - 71,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);

    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"Mark A. Holm",
				XV_Y,			40,
				XV_X,			middle - 65,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);

    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"Exceptions",
				XV_Y,			55,
				XV_X,			middle - 58,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);

    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"Tiles Remaining",
				XV_Y,			75,
				XV_X,			X_LOC + 38,
				XV_HELP_DATA,	"mahjongg:tiles_remaining",
				NULL);

    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"Ported to XView in",
				XV_Y,			25,
				XV_X,			middle + 290,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);
    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	"9/91 by S. K. Tazuma",
				XV_Y,			40,
				XV_X,			middle + 290,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);
    xv_create(message_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING,	Release_info,
				XV_Y,			55,
				XV_X,			middle + 290,
				XV_HELP_DATA,		"mahjongg:credits",
				NULL);

    /* create seed item */

    board_num = xv_create(message_panel, PANEL_TEXT,
				PANEL_LABEL_STRING,	"Board Number : ",
				PANEL_VALUE,		"",
				PANEL_VALUE_DISPLAY_LENGTH,	7,
				XV_Y,			95,
				XV_X,			middle,
				PANEL_NOTIFY_PROC,	board_num_proc,
				XV_HELP_DATA,		"mahjongg:board_number",
				NULL);

    /* create control buttons */

#define ROW_FOR_BUTTONS		120
    button_x = middle;
    xv_create(message_panel, PANEL_BUTTON,
				XV_Y,			ROW_FOR_BUTTONS,
				XV_X,			button_x,
				PANEL_LABEL_STRING,	"HELP",
				(Use_colored_buttons
					? PANEL_ITEM_COLOR : ATTR_NOP1),
							panel_color(CYAN),
				PANEL_NOTIFY_PROC,	help_proc,
				XV_HELP_DATA,		"mahjongg:help_button",
				NULL);

    xv_create(message_panel, PANEL_BUTTON,
				XV_Y,			ROW_FOR_BUTTONS,
				XV_X,			button_x += 55,
				PANEL_LABEL_STRING,	"AGAIN",
				(Use_colored_buttons
					? PANEL_ITEM_COLOR : ATTR_NOP1),
							panel_color(YELLOW),
				PANEL_NOTIFY_PROC,	again_proc,
				XV_HELP_DATA,		"mahjongg:again_button",
				NULL);

    xv_create(message_panel, PANEL_BUTTON,
				XV_Y,			ROW_FOR_BUTTONS,
				XV_X,			button_x += 63,
				PANEL_LABEL_STRING,	"NEW",
				(Use_colored_buttons
					? PANEL_ITEM_COLOR : ATTR_NOP1),
							panel_color(GREEN),
				PANEL_NOTIFY_PROC,	new_proc,
				XV_HELP_DATA,		"mahjongg:new_button",
				NULL);

    xv_create(message_panel, PANEL_BUTTON,
				XV_Y,			ROW_FOR_BUTTONS,
				XV_X,			button_x += 51,
				PANEL_LABEL_STRING,	"UNDO",
				(Use_colored_buttons
					? PANEL_ITEM_COLOR : ATTR_NOP1),
							panel_color(MAGENTA),
				PANEL_NOTIFY_PROC,	undo_proc,
				XV_HELP_DATA,		"mahjongg:undo_button",
				NULL);

    xv_create(message_panel, PANEL_BUTTON,
				XV_Y,			ROW_FOR_BUTTONS,
				XV_X,			button_x += 61,
				PANEL_LABEL_STRING,	"QUIT",
				(Use_colored_buttons
					? PANEL_ITEM_COLOR : ATTR_NOP1),
							panel_color(RED),
				PANEL_NOTIFY_PROC,	quit_proc,
				XV_HELP_DATA,		"mahjongg:quit_button",
				NULL);

    /* place concealed message */

    message = xv_create(message_panel, PANEL_MESSAGE,
				XV_Y,			150,
				XV_X,			middle,
				PANEL_LABEL_STRING,	"",
				XV_SHOW,		FALSE,
				XV_HELP_DATA,		"mahjongg:message_area",
				NULL);

    /* set up canvas for the play */

    play_canvas = xv_create(main_frame, CANVAS,
			XV_HEIGHT,		PLAY_Y_MAX,
			XV_WIDTH,		PLAY_X_MAX,
			WIN_X,			0,
			WIN_Y,			MESS_Y_MAX + BORDER,

			/* can be done here or on the canvas
			 * paint window */
			(BandW ? ATTR_NOP1 : WIN_CMS),	mahjongg_canvas_cms,

			(Use_canvas_dynamic_cms ?
					WIN_DYNAMIC_VISUAL : ATTR_NOP1),
						TRUE,

			/*
			 * Switch background/foreground if B & W ; we also
			 * invert all the tiles (see above call to
			 * invert_tile_icon_mprs()), so that we
			 * get white tiles on a black background.  Otherwise
			 * we would get white tiles on a white background.
			 */
			(Switch_bg_fg ? WIN_BACKGROUND_COLOR : ATTR_NOP1), 1,
			(Switch_bg_fg ? WIN_FOREGROUND_COLOR : ATTR_NOP1), 0,

			CANVAS_REPAINT_PROC,	repaint_proc,
			CANVAS_RETAINED,	TRUE,
			XV_HELP_DATA,		"mahjongg:help",
			NULL);

    Play_area = canvas_paint_window(play_canvas);

    init_cursors();

    set_mj_cursor(play_cursor);

    xv_set(Play_area,
		WIN_EVENT_PROC,		play_event_proc,
		WIN_CONSUME_EVENTS,
				WIN_NO_EVENTS,
				WIN_REPAINT,
				WIN_MOUSE_BUTTONS,
				WIN_ASCII_EVENTS,
					/* consume ASCII events only for the
					 * purpose of ignoring them in the
					 * event proc.  If you try to ignore
					 * them via WIN_IGNORE_EVENTS, then
					 * the events get re-directed to the
					 * panel, which I didn't want. */
				NULL,
		WIN_IGNORE_EVENTS,
				/* only want button-down mouse events */
				WIN_UP_EVENTS,
#if JUST_A_COMMENT
				WIN_ASCII_EVENTS,
					/* if ASCII events are ignored by
					 * the canvas, the events get sent
					 * automatically (by XView) to the
					 * panel.  For this program, I don't
					 * think that effect is desired, so
					 * instead of ignoring these events
					 * here, I'll ignore them in the event
					 * handler. */
#endif
				NULL,
		NULL);

    /* start main processing */

    xv_main_loop(main_frame);
    exit(0);
}

/*
 * do_repaint is called from event.c
 */
void
do_repaint()
{
    place_tiles(TRUE);
}

/*
 * With XView version 2.0, the repaint_proc always gets called either
 * four (usually) or two times before the window first appears.
 * Since this problem appears to be fixed under XView 3.0, and you
 * only get one repaint at start-up time, I took out special code
 * in this routine to avoid the extra repaints.  The extra repaints
 * don't hurt anything--it's just a waste of CPU cycles. 
 *     Note that because of dynamic link libraries, it wouldn't do to
 * use ifdef's for this problem.  A single binary could run with either
 * XView 2.0 or 3.0 libraries.
 */
void
repaint_proc(canvas, play_area, repaint_area)
Canvas canvas;
Xv_Window play_area;		/* the canvas paint window */
Rectlist *repaint_area;
{
    static int call_counter = 0;

    if (call_counter < 1) {
	++call_counter;
	build_image(FALSE);
    }

    place_tiles(TRUE);
}


/* die because of some UNIX error */
void
die(message, pperr)
char *message;
int pperr;
{
    fprintf(stderr, message);
    if (pperr)
	perror("mahjongg");
    exit(1);
}

void
place_tiles(newboard)
boolean		newboard;
{
    int 	i;
    int 	j;
    int 	k;
    int 	x_loc;
    int 	y_loc;

    /* check if not new and destroy existing panel buttons */

    /* place tiles */

        /* row 1 level 1 */

	for(i = 0,
	    x_loc = COL2;
	    i < 12;
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW1);

	/* row 2 level 1 */

	for(x_loc = COL4,
	    j = 0;
	    j < 8;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW2);

	/* row 3 level 1 */

	for(x_loc = COL3,
	    j = 0;
	    j < 10;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW3);

	/* row 4 1/2 level 1 */

	/* Left */

		put_tile(board[i], COL1, ROW4pt5);

		i++; /* increment tile counter */

	/* row 4 level 1 */

	for(x_loc = COL2,
	    j = 0;
	    j < 12;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW4);

	/* row 5 level 1 */

	for(x_loc = COL2,
	    j = 0;
	    j < 12;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW5);

	/* row 4 1/2 level 1 */

	/* Right */

		put_tile(board[i], COL14, ROW4pt5);

		i++; /* increment tile counter */

		put_tile(board[i], COL15, ROW4pt5);

		i++; /* increment tile counter */

	/* row 6 level 1 */

	for(x_loc = COL3,
	    j = 0;
	    j < 10;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW6);

	/* row 7 level 1 */

	for(x_loc = COL4,
	    j = 0;
	    j < 8;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW7);

        /* row 8 level 1 */

	for(j = 0,
	    x_loc = COL2;
	    j < 12;
	    j++,
	    i++,
	    x_loc += W_BASE_TILE) 

		put_tile(board[i], x_loc, ROW8);

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

			put_tile(board[i], x_loc, y_loc);

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

			put_tile(board[i], x_loc, y_loc);

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

			put_tile(board[i], x_loc, y_loc);

	/* Cap tile */

		put_tile(board[i],
				COL7pt5 - (S_TILE_SHADOW * 4),
				ROW4pt5 - (B_TILE_SHADOW * 4));

    /* need this especially for a screen refresh */
    if (selected[0].in_preview_mode && ! selected[0].tileptr->removed)
	begin_preview(&selected[0]);
    if (selected[1].in_preview_mode && ! selected[1].tileptr->removed)
	begin_preview(&selected[1]);

    /*
     * The panel_msg() call below shouldn't be done when just refreshing
     * the screen.  It should only be done when this routine
     * is called after a build_image() call.  This would happen when the
     * program first starts up, and after an "AGAIN" or "NEW" selection.
     */
    if (Clear_msg) {
	/* clear stand_by message  and release input mask */

       panel_msg((char *)0, play_cursor_si, FALSE);
       Clear_msg = FALSE;
    }
}

void
build_image(oldimage)
boolean oldimage;
{
    int i;
    int j;
    char seed_text[80];
    Tile_val tmp_pool[144];
    int tmp_tiles_left;
    int pos;

	/* initialize selected structures */

        selected[0].in_preview_mode = FALSE;
        selected[1].in_preview_mode = FALSE;
        undo_count   	   = -1;

        xv_set(message, XV_SHOW, FALSE, 0);

	if (Use_num_server_images) {
	    xv_set(TL_hundred, 
			PANEL_LABEL_IMAGE,	Num_icon_si[1],
			XV_SHOW,		TRUE,
			0);
	    xv_set(TL_ten, 
			PANEL_LABEL_IMAGE,	Num_icon_si[4],
			XV_SHOW,		TRUE,
			0);
	    xv_set(TL_one, 
			PANEL_LABEL_IMAGE,	Num_icon_si[4],
			XV_SHOW,		TRUE,
			0);
	}
	else {
	    xv_set(TL_hundred, 
			PANEL_LABEL_IMAGE,	Num_icon_mpr[1],
			XV_SHOW,		TRUE,
			0);
	    xv_set(TL_ten, 
			PANEL_LABEL_IMAGE,	Num_icon_mpr[4],
			XV_SHOW,		TRUE,
			0);
	    xv_set(TL_one, 
			PANEL_LABEL_IMAGE,	Num_icon_mpr[4],
			0);
	}

	/* display current seed in text item */

    	sprintf(seed_text, "%d", seed);
	xv_set(board_num, PANEL_VALUE, seed_text, 0);

	/* show stand_by message while building image  and grab all input */

	panel_msg("Building board. Please wait.", wait_cursor_si, FALSE);
	Clear_msg = TRUE;

	/* initialize random number counter */

	(void) srandom(seed);

	tile_count = 144;

	/* initialize  tmp_pool */
        for(j = 0; j < 136; j++) tmp_pool[j] = j/4; /* tiles * 4 */
        for(j=136; j<144; j++) tmp_pool[j] = j- 102; /* flowers & seasons */
                                                   /* 136 --> 34 */
                                                   /* 143 --> 41 */
        tmp_tiles_left = 144;


	if (board[0] == NULL) {
	    /* intialize array */
	    for (i = 0; i < 144; i++) {
		board[i] = &board_tile[i];
	    }
	}

	/* assign values to each location. Board is built from upper left *
	 * to lower right, bottom to top. Exception are left tile for row *
	 * 4pt5 is before rows 4 and 5, and right tiles for row 4.5 are   *
	 * after rows 4 and 5                                             */

	for (j = 0; j < 144; j++) {

	    if (!oldimage) { /* not repeating last board */

	       /* Shuffle/pick tile */

               pos = RANDOM(tmp_tiles_left);

               i= tmp_pool[pos];

               for( ; pos < tmp_tiles_left; pos++)
			tmp_pool[pos] = tmp_pool[pos+1];

               tmp_tiles_left--;

		/* all flowers and all seasons */

		board[j]->value = (i >= 34) ? ((i >= 38) ? 35 : 34) : i;
		board[j]->image = i;
	    }
	/* establish default values */

		board[j]->left_free     = FALSE;
		board[j]->right_free    = FALSE;
		board[j]->top_free      = TRUE;
		board[j]->top_covered	= FALSE;
		board[j]->left_next[0]  = j - 1;
		board[j]->left_next[1]  = DONT_CARE;
		board[j]->right_next[0] = j + 1;
		board[j]->right_next[1] = DONT_CARE;
		board[j]->covered[0] = DONT_CARE;
		board[j]->covered[1] = DONT_CARE;
		board[j]->covered[2] = DONT_CARE;
		board[j]->covered[3] = DONT_CARE;
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
			board[j]->left_next[0] = DONT_CARE;
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
			board[j]->right_next[0] = DONT_CARE;
			break;
		case 143:
			board[j]->right_free = TRUE;
			board[j]->left_next[0] = DONT_CARE;
			board[j]->left_free = TRUE;
			board[j]->right_next[0] = DONT_CARE;
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
		board[j]->top_covered = TRUE;
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

setup_cms()
{
    mahjongg_icon_cms = (Cms) xv_create(NULL, CMS,
		CMS_SIZE,		NUM_COLORS,
		CMS_NAME,		"mahjongg_icon_cms",
		CMS_NAMED_COLORS,	Mahjongg_color_list,
					NULL,
		NULL);

    if (Use_panel_control_cms)
	mahjongg_panel_cms = (Cms) xv_create(NULL, CMS,
		CMS_CONTROL_CMS,	TRUE,
		CMS_SIZE,		CMS_CONTROL_COLORS + NUM_COLORS + 1,
		CMS_NAME,		"mahjongg_panel_cms",
		CMS_NAMED_COLORS,	Mahjongg_color_list,
					"black", /* last is foreground color */
					NULL,
		NULL);
    else
	mahjongg_panel_cms = (Cms) xv_create(NULL, CMS,
		CMS_SIZE,		NUM_COLORS,
		CMS_NAME,		"mahjongg_panel_cms",
		CMS_NAMED_COLORS,	Mahjongg_color_list, NULL,
		NULL);

    mahjongg_canvas_cms = NULL;
    if (Use_canvas_dynamic_cms) {
	mahjongg_canvas_cms = (Cms) create_aligned_cms(NUM_COLORS);
	if (mahjongg_canvas_cms == NULL) {
	    fprintf(stderr, "Error, couldn't create dynamic colormap\n");
	    fprintf(stderr, "Trying regular colormap...\n");
	    Use_canvas_dynamic_cms = FALSE;
	}
	else {
	    /* name the cms and set the colors */
	    xv_set(mahjongg_canvas_cms,
		CMS_NAME,		"mahjongg_canvas_cms",
		CMS_NAMED_COLORS,	Mahjongg_color_list, NULL,
		NULL);
	}
    }

    if (mahjongg_canvas_cms == NULL) {
	mahjongg_canvas_cms = (Cms) xv_create(NULL, CMS,
		(Use_canvas_control_cms ? CMS_CONTROL_CMS : ATTR_NOP1),	TRUE,
		CMS_SIZE,		Use_canvas_control_cms ?
					       CMS_CONTROL_COLORS + NUM_COLORS :
						NUM_COLORS,
		CMS_NAME,		"mahjongg_canvas_cms",
		CMS_NAMED_COLORS,	Mahjongg_color_list,
					NULL,
		NULL);
    }
}

put_tile(tileptr, x_loc, y_loc)
Tile *tileptr;
int x_loc;
int y_loc;
{
    tileptr->x_loc = x_loc;
    tileptr->y_loc = y_loc;

    if (tileptr->removed)
	return;

    if (! tileptr->top_covered)
	if (Use_tile_server_images)
	    pw_write(Play_area, x_loc, y_loc, 64, 64,
			PIX_SRC, Tile_icon_si[tileptr->image], 0, 0);
	else
	    pw_write(Play_area, x_loc, y_loc, 64, 64,
			PIX_SRC, Tile_icon_mpr[tileptr->image], 0, 0);
    else
	if (Use_tile_server_images)
	    pw_write(Play_area, x_loc, y_loc, 64, 64,
			PIX_SRC, Tile_icon_si[BLANK_I], 0, 0);
	else
	    pw_write(Play_area, x_loc, y_loc, 64, 64,
			PIX_SRC, Tile_icon_mpr[BLANK_I], 0, 0);
}

invert_pixrect(pr)
Pixrect *pr;
{
    if (PIXRECT_IMAGE_DEPTH(pr) <= 1)
	invert_mono_image( PIXRECT_IMAGE_DATA_PTR(pr),
			PIXRECT_IMAGE_SIZE(pr) );
    else
	invert_color_image( PIXRECT_IMAGE_DATA_PTR(pr),
			PIXRECT_IMAGE_SIZE(pr) );
}

invert_mono_image(p, buflen)
unsigned char *p;
int buflen;
{
    int i;

    for (i = 0; i < buflen; i++, p++)
	*p = ~(*p);
}

invert_color_image(p, buflen)
unsigned char *p;
int buflen;
{
    int i, j;
    static unsigned long *colors = NULL;

    if (colors == NULL) {
	colors = (unsigned long *) xv_get(mahjongg_canvas_cms, CMS_INDEX_TABLE);
    }
    if (Use_canvas_control_cms) {
	for (i = 0; i < buflen; i++, p++) {
	    /* will have a color in the range 4-11; and want to invert in
	     * that range only (i.e., 4 inverts to 11, 5 inverts to 10, ...
	     */
	    *p = (7 + CMS_CONTROL_COLORS) - (*p - CMS_CONTROL_COLORS);
	}
    }
    else {
	for (i = 0; i < buflen; i++, p++)
	    *p = 7 - *p;
    }
}

shift_pixrect_colors(pr)
Pixrect *pr;
{
    if (PIXRECT_IMAGE_DEPTH(pr) <= 1)
	return;

    shift_image_colors( PIXRECT_IMAGE_DATA_PTR(pr),
			PIXRECT_IMAGE_SIZE(pr) );
}

/*
 * this routine assumes the colors are already in the proper range
 */
shift_image_colors(p, buflen)
unsigned char *p;
int buflen;
{
    int i;

    for (i = 0; i < buflen; i++, p++)
	*p += CMS_CONTROL_COLORS;
}

get_cms_inverse_table(cms)
Cms cms;
{
    unsigned long *colors;
    int i;
    extern unsigned char Cms_inverse_table[];
    int cms_size;

    colors = (unsigned long *) xv_get(cms, CMS_INDEX_TABLE);
    cms_size = xv_get(cms, CMS_SIZE);

    for (i = 0; i < cms_size; i++)
	Cms_inverse_table[colors[i]] = i;
}

convert_physical_to_logical_colors(p, buflen)
unsigned char *p;
int buflen;
{
    int i;
    extern unsigned char Cms_inverse_table[];

    for (i = 0; i < buflen; i++, p++)
	*p = Cms_inverse_table[*p];
}

convert_physical_to_logical_colors_pixrect(p)
Pixrect *p;
{
    convert_physical_to_logical_colors(
		PIXRECT_IMAGE_DATA_PTR(p),
		PIXRECT_IMAGE_SIZE(p));
}

init_num_icon_mprs()
{
    if (BandW) {
	Num_icon_mpr[0] = &NUM0;
	Num_icon_mpr[1] = &NUM1;
	Num_icon_mpr[2] = &NUM2;
	Num_icon_mpr[3] = &NUM3;
	Num_icon_mpr[4] = &NUM4;
	Num_icon_mpr[5] = &NUM5;
	Num_icon_mpr[6] = &NUM6;
	Num_icon_mpr[7] = &NUM7;
	Num_icon_mpr[8] = &NUM8;
	Num_icon_mpr[9] = &NUM9;
    }
    else {
	Num_icon_mpr[0] = &cNUM0;
	Num_icon_mpr[1] = &cNUM1;
	Num_icon_mpr[2] = &cNUM2;
	Num_icon_mpr[3] = &cNUM3;
	Num_icon_mpr[4] = &cNUM4;
	Num_icon_mpr[5] = &cNUM5;
	Num_icon_mpr[6] = &cNUM6;
	Num_icon_mpr[7] = &cNUM7;
	Num_icon_mpr[8] = &cNUM8;
	Num_icon_mpr[9] = &cNUM9;
    }
}

shift_num_icon_mprs()
{
    int i;

    for (i = 0; i < NUM_ICON_COUNT; i++)
	shift_pixrect_colors(Num_icon_mpr[i]);
}

init_num_icon_server_images()
{
    int i;

    if (BandW) {
	for (i = 0; i < NUM_ICON_COUNT; i++) {
	    Num_icon_si[i] = (Server_image) xv_create(NULL, SERVER_IMAGE,
		    XV_WIDTH,		64,
		    XV_HEIGHT,		64,
		    SERVER_IMAGE_DEPTH,	1,
		    SERVER_IMAGE_BITS,
				PIXRECT_IMAGE_DATA_PTR(Num_icon_mpr[i]),
		    NULL);
	}
    }
    else {
	for (i = 0; i < NUM_ICON_COUNT; i++) {
	    Num_icon_si[i] = (Server_image) xv_create(NULL, SERVER_IMAGE,
		    XV_WIDTH,		64,
		    XV_HEIGHT,		64,
		    SERVER_IMAGE_DEPTH,	8,
		    SERVER_IMAGE_COLORMAP,	"mahjongg_panel_cms",
		    SERVER_IMAGE_BITS,
				PIXRECT_IMAGE_DATA_PTR(Num_icon_mpr[i]),
		    NULL);
	}
    }
}

init_tile_icon_mprs()
{
    if (BandW) {
	Tile_icon_mpr[BAM1_I] = &BAM1;
	Tile_icon_mpr[BAM2_I] = &BAM2;
	Tile_icon_mpr[BAM3_I] = &BAM3;
	Tile_icon_mpr[BAM4_I] = &BAM4;
	Tile_icon_mpr[BAM5_I] = &BAM5;
	Tile_icon_mpr[BAM6_I] = &BAM6;
	Tile_icon_mpr[BAM7_I] = &BAM7;
	Tile_icon_mpr[BAM8_I] = &BAM8;
	Tile_icon_mpr[BAM9_I] = &BAM9;

	Tile_icon_mpr[DOT1_I] = &DOT1;
	Tile_icon_mpr[DOT2_I] = &DOT2;
	Tile_icon_mpr[DOT3_I] = &DOT3;
	Tile_icon_mpr[DOT4_I] = &DOT4;
	Tile_icon_mpr[DOT5_I] = &DOT5;
	Tile_icon_mpr[DOT6_I] = &DOT6;
	Tile_icon_mpr[DOT7_I] = &DOT7;
	Tile_icon_mpr[DOT8_I] = &DOT8;
	Tile_icon_mpr[DOT9_I] = &DOT9;

	Tile_icon_mpr[CHA1_I] = &CHA1;
	Tile_icon_mpr[CHA2_I] = &CHA2;
	Tile_icon_mpr[CHA3_I] = &CHA3;
	Tile_icon_mpr[CHA4_I] = &CHA4;
	Tile_icon_mpr[CHA5_I] = &CHA5;
	Tile_icon_mpr[CHA6_I] = &CHA6;
	Tile_icon_mpr[CHA7_I] = &CHA7;
	Tile_icon_mpr[CHA8_I] = &CHA8;
	Tile_icon_mpr[CHA9_I] = &CHA9;

	Tile_icon_mpr[EAST_I] = &EAST;
	Tile_icon_mpr[WEST_I] = &WEST;
	Tile_icon_mpr[SOUT_I] = &SOUT;
	Tile_icon_mpr[NORT_I] = &NORT;

	Tile_icon_mpr[GRED_I] = &GRED;
	Tile_icon_mpr[REDD_I] = &REDD;
	Tile_icon_mpr[WHTD_I] = &WHTD;

	Tile_icon_mpr[AUT_I] = &AUT;
	Tile_icon_mpr[SPR_I] = &SPR;
	Tile_icon_mpr[SUM_I] = &SUM;
	Tile_icon_mpr[WIN_I] = &WIN;

	Tile_icon_mpr[BAM_I] = &BAM;
	Tile_icon_mpr[MUM_I] = &MUM;
	Tile_icon_mpr[ORC_I] = &ORC;
	Tile_icon_mpr[PLM_I] = &PLM;

	Tile_icon_mpr[BLANK_I] = &BLANK;
    }
    else {
	Tile_icon_mpr[BAM1_I] = &cBAM1;
	Tile_icon_mpr[BAM2_I] = &cBAM2;
	Tile_icon_mpr[BAM3_I] = &cBAM3;
	Tile_icon_mpr[BAM4_I] = &cBAM4;
	Tile_icon_mpr[BAM5_I] = &cBAM5;
	Tile_icon_mpr[BAM6_I] = &cBAM6;
	Tile_icon_mpr[BAM7_I] = &cBAM7;
	Tile_icon_mpr[BAM8_I] = &cBAM8;
	Tile_icon_mpr[BAM9_I] = &cBAM9;

	Tile_icon_mpr[DOT1_I] = &cDOT1;
	Tile_icon_mpr[DOT2_I] = &cDOT2;
	Tile_icon_mpr[DOT3_I] = &cDOT3;
	Tile_icon_mpr[DOT4_I] = &cDOT4;
	Tile_icon_mpr[DOT5_I] = &cDOT5;
	Tile_icon_mpr[DOT6_I] = &cDOT6;
	Tile_icon_mpr[DOT7_I] = &cDOT7;
	Tile_icon_mpr[DOT8_I] = &cDOT8;
	Tile_icon_mpr[DOT9_I] = &cDOT9;

	Tile_icon_mpr[CHA1_I] = &cCHA1;
	Tile_icon_mpr[CHA2_I] = &cCHA2;
	Tile_icon_mpr[CHA3_I] = &cCHA3;
	Tile_icon_mpr[CHA4_I] = &cCHA4;
	Tile_icon_mpr[CHA5_I] = &cCHA5;
	Tile_icon_mpr[CHA6_I] = &cCHA6;
	Tile_icon_mpr[CHA7_I] = &cCHA7;
	Tile_icon_mpr[CHA8_I] = &cCHA8;
	Tile_icon_mpr[CHA9_I] = &cCHA9;

	Tile_icon_mpr[EAST_I] = &cEAST;
	Tile_icon_mpr[WEST_I] = &cWEST;
	Tile_icon_mpr[SOUT_I] = &cSOUT;
	Tile_icon_mpr[NORT_I] = &cNORT;

	Tile_icon_mpr[GRED_I] = &cGRED;
	Tile_icon_mpr[REDD_I] = &cREDD;
	Tile_icon_mpr[WHTD_I] = &cWHTD;

	Tile_icon_mpr[AUT_I] = &cAUT;
	Tile_icon_mpr[SPR_I] = &cSPR;
	Tile_icon_mpr[SUM_I] = &cSUM;
	Tile_icon_mpr[WIN_I] = &cWIN;

	Tile_icon_mpr[BAM_I] = &cBAM;
	Tile_icon_mpr[MUM_I] = &cMUM;
	Tile_icon_mpr[ORC_I] = &cORC;
	Tile_icon_mpr[PLM_I] = &cPLM;

	Tile_icon_mpr[BLANK_I] = &cBLANK;
    }
}

invert_tile_icon_mprs()
{
    int i;

    for (i = 0; i < TILE_ICON_COUNT; i++)
	invert_pixrect(Tile_icon_mpr[i]);
}

shift_tile_icon_mprs()
{
    int i;

    for (i = 0; i < TILE_ICON_COUNT; i++)
	shift_pixrect_colors(Tile_icon_mpr[i]);
}

init_tile_icon_server_images()
{
    int i;

    if (BandW) {
	for (i = 0; i < TILE_ICON_COUNT; i++) {
	    Tile_icon_si[i] = (Server_image) xv_create(NULL, SERVER_IMAGE,
		    XV_WIDTH,		64,
		    XV_HEIGHT,		64,
		    SERVER_IMAGE_DEPTH,	1,
		    SERVER_IMAGE_BITS,
				PIXRECT_IMAGE_DATA_PTR(Tile_icon_mpr[i]),
		    NULL);
	}
    }
    else {
	for (i = 0; i < TILE_ICON_COUNT; i++) {
	    Tile_icon_si[i] = (Server_image) xv_create(NULL, SERVER_IMAGE,
		    XV_WIDTH,		64,
		    XV_HEIGHT,		64,
		    SERVER_IMAGE_DEPTH,	8,
		    SERVER_IMAGE_COLORMAP,	"mahjongg_canvas_cms",
		    SERVER_IMAGE_BITS,
				PIXRECT_IMAGE_DATA_PTR(Tile_icon_mpr[i]),
		    NULL);
	}
    }
}

/*
 * Given an x,y coordinate, find the tile which has been selected, if any.
 * Could set up a linked list that keeps track of which tiles are free,
 * and search only that.  But it's even cheaper (less memory) to just
 * use the information that's already in the Tile data structure to determine
 * what tiles to look at.  Given the linked list manipulation, etc., it's
 * questionable whether or not that would be worth the trouble.
 */
find_tile(x, y)
int x, y;
{
   int i;

   for (i = 143; i >= 0; i--) {
	if (
		! (board[i]->removed)
	    &&
#if JUST_A_COMMENT
	/*
	 * if this code is used, then some strange effects will be seen:
	 * a tile which really isn't being pointed at can be selected, because
	 * of the fact that we're only looking at tiles which have the left
	 * or right free
	 */
	      (
		board[i]->left_free
		||
		board[i]->right_free
	      )
	    &&
#endif
		board[i]->x_loc <= x
	    &&
		board[i]->y_loc <= y
	    &&
		x <= board[i]->x_loc + 64
	    &&
		y <= board[i]->y_loc + 64
	    )

	    return(i);
    }

    return(-1);
}

print_help()
{
#define prt(s)	fprintf(stderr, s)
    prt(Usage_msg);
    prt("-b\tforce black and white mode (bg/fg bi-color)\n");
    prt("-c\tforce color mode\n");
    prt("-n #\tspecify seed number\n");
    prt("-P\tconfigure control panel to use normal background color\n");
    prt("-PB\tlike -P, but with uniformly colored buttons\n");
    prt("-B\tlike -b, but uses black (or bg color) background\n");
    prt("\t-B and -BB below only work when used with a color display\n");
    prt("-BB\tlike -B, but with black (or fg color) tiles\n");
    prt("-C\tconfigure game area to use normal background color\n");
    prt("    warning: -C forces the -D option described below\n");
    prt("-D\tdon't use direct color manipulation for tile highlighting\n");
    prt("\t-D minimizes usage of color map entries but is slower\n");
    prt("-N\tuse pixrects instead of server images for panel icons\n");
    prt("\t-N not recommended, but reduces server memory usage\n");
    prt("-T\tuse pixrects instead of server images for tile icons\n");
    prt("\t-T reduces server memory usage but slows screen updates\n");

    prt("-E\tenable use of error bells\n");

    prt("==========================================================\n");
    if (isatty(fileno(stdin))
		&& isatty(fileno(stdout)) && isatty(fileno(stderr))) {
	/* don't want to do this if they re-direct output to a file */
	char buf[80];
	fputs("Enter Return to see XView help, or 'q' and Return to quit: ",
			stdout);
	fgets(buf, sizeof(buf), stdin);
	if (buf[0] == 'q')
	    exit(0);
    }
}

init_cursors()
{
    wait_cursor_si = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		16,
		XV_HEIGHT,		16,
		SERVER_IMAGE_BITS,	wait_image,
		NULL);
    confirm_cursor_si = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		16,
		XV_HEIGHT,		16,
		SERVER_IMAGE_BITS,	confirm_image,
		NULL);

    stick_cursor_si = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		16,
		XV_HEIGHT,		16,
		SERVER_IMAGE_BITS,	stick_image,
		NULL);

    play_cursor_si = stick_cursor_si;

    play_cursor = (Xv_Cursor) xv_create(XV_NULL, CURSOR,
		CURSOR_IMAGE,	play_cursor_si,
		CURSOR_XHOT,	0,
		CURSOR_YHOT,	0,
		CURSOR_OP,	PIX_SRC ^ PIX_DST,
			/* XView interprets this CURSOR_OP as being
			 * the CURSOR_IMAGE with a mask behind it and
			 * also around it, colored in the background
			 * color; the cursor is opaque */
		NULL);

    if (! BandW) {
	Xv_singlecolor bg, fg;

	bg.red = 0, bg.green = 0, bg.blue = 0;
	fg.red = 0, fg.green = 255, fg.blue = 0;

	xv_set(play_cursor,
		CURSOR_BACKGROUND_COLOR,	&bg,
		CURSOR_FOREGROUND_COLOR,	&fg,
		NULL);
    }
}

/*
 * Figure out the depth of the display.  Kind of weird code, but it
 * works.  Create a frame and then destroy it, just in order to
 * determine the depth.  The reason why we can't just do an xv_create()
 * of the main_frame, followed by an xv_set() of the colormap segment,
 * is that the colormap segment must be set at create time or else
 * the icon doesn't appear right.
 */
get_display_depth()
{
    Frame frame;
    int depth;

    frame = xv_create(XV_NULL, FRAME, NULL);

    depth = xv_get(frame, WIN_DEPTH);

    xv_destroy(frame);

    return(depth);
}

set_mj_cursor(play_cursor)
Xv_Cursor play_cursor;
{
    xv_set(Play_area, WIN_CURSOR, play_cursor, NULL);
    xv_set(message_panel, WIN_CURSOR, play_cursor, NULL);
    xv_set(main_frame, WIN_CURSOR, play_cursor, NULL);
}

/*
 * Create a cms that is "properly aligned".  For 8 colors, a properly
 * aligned cms is one that begins at a multiple of 8 (0, 8, 16, ...).
 * This kind of cms is needed when you want to change the plane mask
 * in order to restrict bit manipulations (raster-ops) to only
 * certain planes.  For this reason, the number of colors would ordinarily
 * be a power of 2.
 */
Cms
create_aligned_cms(num_colors)
int num_colors;
{
#define MAX_TRIES	3
    unsigned long *cms_index_table;
    Cms cms;
    Cms dummy_cms;
    Cms destroy_list[MAX_TRIES * 2];	/* will hold on to at most 2 cms's
					 * per try; we hold on to cms's so
					 * that the colorcells in those cms's
					 * don't get in the way */
    int destroy_list_index = -1;
    int i;
#define DESTROY_CMS(cms)	xv_destroy_status(cms, DESTROY_CLEANUP)
	/*
	 * Need DESTROY_CMS() to immediately clear out the CMS.
	 * xv_destroy_status() was found by poking around in
	 * the XView source code.  Before doing that, I tried each of:
	 * xv_destroy(), xv_destroy_safe(), xv_destroy_check(), and
	 * xv_destroy_immediate().  None of these did what I wanted (they
	 * didn't release the CMS immediately, and subsequent creates of CMS's
	 * reflected the fact that the previous ones hadn't been released).
	 */
#define DESTROY_LIST_ADD(cms)	destroy_list[++destroy_list_index] = cms
#define DESTROY_LIST_DESTROY()	for (i = 0; i <= destroy_list_index; i++) \
					DESTROY_CMS(destroy_list[i]);
    int tries;
    int skip;
    int first_index = 0;
    int cms_size;
    boolean found;
    boolean ok;

    for (tries = 0; tries < MAX_TRIES; tries++) {
	cms = (Cms) xv_create(XV_NULL, CMS,
		CMS_TYPE,		XV_DYNAMIC_CMS,
		CMS_SIZE,		num_colors,
		NULL);

	cms_index_table = (unsigned long *) xv_get(cms, CMS_INDEX_TABLE);
	cms_size = xv_get(cms, CMS_SIZE);

	/*
	 * The code below finds the first color in the
	 * table that is a multiple of num_colors.
	 * Can't assume that all the real pixel values for the
	 * colors acquired above are sequential and contiguous.
	 * But I am assuming that the first color found that is a
	 * multiple of num_colors is a reasonable starting place
	 * to look.  The requirement that all the colors be sequential
	 * and contiguous will be checked later in this routine.
	 */
	found = FALSE;
	for (skip = 0; skip < cms_size; skip++) {
	    if ((cms_index_table[skip] % num_colors) == 0) {
		found = TRUE;
		break;
	    }
	}
	if (! found) {
	    DESTROY_LIST_ADD(cms);
	    continue;
	}

	dummy_cms = NULL;
	if (skip != 0) {
	    /*
	     * Allocate a cms with just those colorcells that need
	     * to be skipped.  This will get them out of way so that
	     * we can get the colorcells we need.  This dummy cms
	     * will be destroyed later.
	     */
	    DESTROY_CMS(cms);
	    dummy_cms = xv_create(XV_NULL, CMS,
				CMS_TYPE,	XV_DYNAMIC_CMS,
				CMS_SIZE,	skip,
				NULL);
	    DESTROY_LIST_ADD(dummy_cms);
	    continue;		/* try again */
	}

	/*
	 * Verify that all the real pixel values are contiguous.
	 *
	 * (The obvious way would be to subtract the first and last
	 * pixel values and see if their difference is correct, but
	 * this assumes that the intervening pixel values are OK.
	 * I decided not to trust this assumption--best
	 * to be certain by checking every pixel value.)
	 */
	ok = TRUE;
	for (i = 0; i < num_colors - 1; i++) {
	    if (cms_index_table[first_index + i] + 1 !=
		cms_index_table[first_index + i + 1]) {

		ok = FALSE;
		break;
	    }
	}
	if (ok) {
	    DESTROY_LIST_DESTROY();
	    return(cms);		/* SUCCESS */
	}
	else {
	    /*
	     * Need to acquire some of the color cells and hold on to
	     * them, so that they don't get in our way the next time
	     * around the loop.
	     */
	    found = FALSE;
	    for (skip = 1; skip < cms_size; skip++) {
		if ((cms_index_table[skip] % num_colors) == 0) {
		    found = TRUE;
		    break;
		}
	    }
	    if (found) {
		DESTROY_CMS(cms);
		dummy_cms = (Cms) xv_create(XV_NULL, CMS,
				CMS_TYPE,		XV_DYNAMIC_CMS,
				CMS_SIZE,		skip,
				NULL);

		DESTROY_LIST_ADD(dummy_cms);
	    }
	    else {
		/* hold on to it til we succeed or fail, so that the
		 * colorcells on this cms don't get in our way
		 */
		DESTROY_LIST_ADD(cms);
	    }
	}
    }
    DESTROY_LIST_DESTROY();
    return(NULL);			/* FAILURE */
}
