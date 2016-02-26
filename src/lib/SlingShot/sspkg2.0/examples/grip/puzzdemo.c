/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) puzzdemo.c 1.6 92/06/23 
 */
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/cms.h>
#include <xview/panel.h>
#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/array.h>
#include <sspkg/grip.h>


static Xv_singlecolor colors[] = {
	{255, 255, 255},
	{0, 0, 0},
	{170, 68, 0},
	{170, 170, 204},
	{204, 102, 0},
	{204, 204, 204},
	{238, 0, 0},
	{238, 204, 170},
	{238, 238, 238},
	{170, 0, 0},
	{0, 136, 204},
	{238, 153, 153},
	{136, 68, 0},
	{136, 136, 136},
	{170, 102, 0},
	{238, 170, 0},
	{64, 64, 192}, /* the empty piece only */
	{160, 64, 64}, /* the background color only */
};


#define		IMAGE_DEPTH 8
#define		IMAGE_WIDTH 320
#define		IMAGE_HEIGHT 200

#define 	SUBIMAGE_WIDTH 64
#define 	SUBIMAGE_HEIGHT 50

#define		N_HORI	(IMAGE_WIDTH/SUBIMAGE_WIDTH)
#define		N_VERT	(IMAGE_HEIGHT/SUBIMAGE_HEIGHT)

#define		PUZZLE_COLUMN_GAP 2
#define		PUZZLE_ROW_GAP 2

/* The position of the "empty" piece when solved. */
#define		EMPTY_X	0
#define		EMPTY_Y	3

#define		PUZZLE_CMS_NAME	"puzzle-cms"

static unsigned short image_bits[IMAGE_WIDTH*IMAGE_HEIGHT] = {
#include "../icons/bully.icon.aa"
#include "../icons/bully.icon.ab"
#include "../icons/bully.icon.ac"
#include "../icons/bully.icon.ad"
#include "../icons/bully.icon.ae"
#include "../icons/bully.icon.af"
};

static unsigned short icon_bits[64*64] = {
#include "../icons/puzzle_demo.icon"
};

Frame		frame;
Panel		panel;
Canvas_shell	canshell;
Array_tile	puzzle_tile;
Grip 		grip_above, grip_below, grip_left, grip_right;
Cms		cms;
Drawimage	solved[N_HORI][N_VERT];
Server_image	svr_image[N_HORI][N_VERT];

#define	get_solved_piece(x, y)  solved[x][y]

void		setup_images();
void		scramble_proc();
void		solve_proc();
void		show_hidden_pieces();
void		grip_moved();
void		grip_dbl_clk();
void		piece_dbl_clk();
void		set_empty_spot();
void		free_server_images();


main(argc, argv)
	int             argc;
	char           *argv[];
{
	Icon icon;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	cms = (Cms) xv_create(XV_NULL, CMS,
			CMS_TYPE, XV_DYNAMIC_CMS,
			CMS_SIZE, 18,
			CMS_COLORS, colors,
			CMS_NAME, PUZZLE_CMS_NAME,
			NULL);

	if(!cms) {
		printf("Color is required for %s.\n", argv[0]);
		exit(1);
	}

	icon = (Icon) xv_create(XV_NULL, ICON,
			ICON_IMAGE, xv_create(XV_NULL, SERVER_IMAGE,
					XV_WIDTH, 64,
					XV_HEIGHT, 64,
					SERVER_IMAGE_COLORMAP, PUZZLE_CMS_NAME,
					SERVER_IMAGE_DEPTH, 8,
					SERVER_IMAGE_BITS, icon_bits,
					NULL),
			NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "Double click or slide pieces to move them.",
			FRAME_ICON, icon,
			NULL);

	panel = (Panel) xv_create(frame, PANEL, 
			NULL);

	(void) xv_create(panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Scramble",
			PANEL_NOTIFY_PROC, scramble_proc,
			NULL);

	(void) xv_create(panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Solve",
			PANEL_NOTIFY_PROC, solve_proc,
			NULL);

	window_fit(panel);

	canshell = (Canvas_shell) xv_create(frame, CANVAS_SHELL,
			XV_X, 0,
			WIN_BELOW, panel,
			WIN_CMS, cms,
			WIN_FOREGROUND_COLOR, 0,
			WIN_BACKGROUND_COLOR, 17,
			NULL);

	/*
	 * Create the working tile.
	 */
	puzzle_tile = (Array_tile) xv_create(canshell, ARRAY_TILE,
			ARRAY_TILE_N_COLUMNS, N_HORI,
			ARRAY_TILE_N_ROWS, N_VERT,
			ARRAY_TILE_COLUMN_GAP, PUZZLE_COLUMN_GAP,
			ARRAY_TILE_ROW_GAP, PUZZLE_ROW_GAP,
			NULL);

	setup_images();
	create_grips();
	set_empty_spot();

	xv_set(panel,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		NULL);

	/* size the canvas to the size of the array_tile */
	xv_set(canshell, 
		XV_WIDTH, xv_get(puzzle_tile, XV_WIDTH),
		XV_HEIGHT, xv_get(puzzle_tile, XV_HEIGHT),
		NULL);

	xv_set(canshell, 
		CANVAS_SHELL_BATCH_REPAINT, TRUE,
		NULL);

	window_fit(frame);

	srand(getpid());

	xv_main_loop(frame);
	free_server_images();
}


void
setup_images()
{
	int 		x;
	int 		y;
	Pixmap		p;
	Display		*dpy;
	Server_image    big_image;
	Server_image 	tmp_svr_image;
	Drawimage	tmp;

	dpy = (Display*) xv_get(canshell, XV_DISPLAY);

	/* create one big image */
	
	big_image = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
			SERVER_IMAGE_COLORMAP, PUZZLE_CMS_NAME,
			XV_WIDTH, IMAGE_WIDTH,
			XV_HEIGHT, IMAGE_HEIGHT,
			SERVER_IMAGE_DEPTH, IMAGE_DEPTH,
			SERVER_IMAGE_BITS, image_bits,
			NULL);

	/* slice it up into smaller images */

	for (y=0; y< N_VERT; y++)
	  for (x=0; x< N_HORI; x++) {

		/*
		 * The array_tile automatically positions the images
		 * in correct order on creation.  This will be turned off 
		 * later to allow explicit positioning of each child.
		 * Key data is used to keep track of the original position.
		 */
		if((x == EMPTY_X) && (y == EMPTY_Y)) {

		  tmp = xv_create(puzzle_tile, DRAWRECT,
			RECTOBJ_BG, 16,
			XV_WIDTH, SUBIMAGE_WIDTH,
			XV_HEIGHT, SUBIMAGE_HEIGHT,
			NULL);

		} else {

		  p = XCreatePixmap(
			dpy,
			(Drawable) xv_get(canshell, XV_XID), 
			SUBIMAGE_WIDTH, SUBIMAGE_HEIGHT, IMAGE_DEPTH);

		  XCopyArea(
			dpy,
			(Drawable) xv_get(big_image, SERVER_IMAGE_PIXMAP),
			p,
			DefaultGC(dpy, (int)xv_get(canshell, SCREEN_NUMBER)),
			SUBIMAGE_WIDTH*x, SUBIMAGE_HEIGHT*y,
			SUBIMAGE_WIDTH, SUBIMAGE_HEIGHT,
			0, 0);

		  tmp_svr_image = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
			SERVER_IMAGE_PIXMAP, p,
			XV_WIDTH, SUBIMAGE_WIDTH,
			XV_HEIGHT, SUBIMAGE_HEIGHT,
			NULL);

		  svr_image[x][y] = tmp_svr_image;

		  tmp = xv_create(puzzle_tile, DRAWIMAGE,
			RECTOBJ_SINGLE_CLICK_PROC, piece_dbl_clk,
			DRAWIMAGE_IMAGE1, tmp_svr_image,
			DRAWIMAGE_IMAGE2, tmp_svr_image,
			NULL);

		}

		solved[x][y] = tmp;

#ifdef DEBUG_POSITIONS
		{
		char str[1000];
		sprintf(str, "(%d, %d)", x, y);
		xv_create(tmp, DRAWTEXT,
			DRAWTEXT_STRING, str,
			RECTOBJ_FG, 1,
			NULL);
		}
#endif

	  }
	
	/* 
	 * Turn off automatic positioning for the puzzle tile, because 
	 * from here on out, each piece will be explicitly positioned.
	 */
	xv_set(puzzle_tile,
		ARRAY_TILE_AUTO_LAYOUT, FALSE,
		NULL);

	xv_destroy(big_image);

}

create_grips()
{
	grip_above = (Grip) xv_create(canshell, GRIP, 
				XV_SHOW, FALSE, 
				GRIP_SLIDE_X, FALSE,
				GRIP_DONE_PROC, grip_moved,
				RECTOBJ_SINGLE_CLICK_PROC, grip_dbl_clk,
				NULL);
	grip_below = (Grip) xv_create(canshell, GRIP, 
				XV_SHOW, FALSE, 
				GRIP_SLIDE_X, FALSE,
				GRIP_DONE_PROC, grip_moved,
				RECTOBJ_SINGLE_CLICK_PROC, grip_dbl_clk,
				NULL);
	grip_left = (Grip) xv_create(canshell, GRIP,
				XV_SHOW, FALSE, 
				GRIP_SLIDE_Y, FALSE,
				GRIP_DONE_PROC, grip_moved,
				RECTOBJ_SINGLE_CLICK_PROC, grip_dbl_clk,
				NULL);
	grip_right = (Grip) xv_create(canshell, GRIP,
				XV_SHOW, FALSE, 
				GRIP_SLIDE_Y, FALSE,
				GRIP_DONE_PROC, grip_moved,
				RECTOBJ_SINGLE_CLICK_PROC, grip_dbl_clk,
				NULL);
}


position_grip_above(empty_x, empty_y)
	int empty_x, empty_y;
{
	xv_set(grip_above, 
		XV_X, empty_x,
		XV_Y, empty_y - (PUZZLE_ROW_GAP + SUBIMAGE_HEIGHT),
		NULL);
}

position_grip_right(empty_x, empty_y)
	int empty_x, empty_y;
{
	xv_set(grip_right, 
		XV_X, empty_x + PUZZLE_COLUMN_GAP + SUBIMAGE_WIDTH,
		XV_Y, empty_y,
		NULL);
}

position_grip_below(empty_x, empty_y)
	int empty_x, empty_y;
{
	xv_set(grip_below, 
		XV_X, empty_x,
		XV_Y,  empty_y + PUZZLE_ROW_GAP + SUBIMAGE_HEIGHT,
		NULL);
}

position_grip_left(empty_x, empty_y)
	int empty_x, empty_y;
{
	xv_set(grip_left, 
		XV_X, empty_x - (PUZZLE_COLUMN_GAP + SUBIMAGE_WIDTH),
		XV_Y, empty_y,
		NULL);
}


void
set_empty_spot()
{
	Drawimage empty;
	int	row, column;
	int	empty_x, empty_y;
	Drawimage tmp;

	xv_set(canshell, 
		CANVAS_SHELL_DELAY_REPAINT, TRUE, 
		NULL);

	empty = get_solved_piece(EMPTY_X, EMPTY_Y);
	column = (int) xv_get(puzzle_tile, ARRAY_TILE_COLUMN, empty);
	row = (int) xv_get(puzzle_tile, ARRAY_TILE_ROW, empty);
	empty_x = (int) xv_get(empty, XV_X);
	empty_y = (int) xv_get(empty, XV_Y);

	if(row > 0) {
		tmp = (Drawimage) xv_get(puzzle_tile, 
				ARRAY_TILE_POSITION, column, row-1);
		xv_set(grip_above, 
			DRAWIMAGE_IMAGE1, xv_get(tmp, DRAWIMAGE_IMAGE1),
			DRAWIMAGE_IMAGE2, xv_get(tmp, DRAWIMAGE_IMAGE2),
			XV_SHOW, TRUE,
			GRIP_MAX_Y, empty_y + SUBIMAGE_HEIGHT,
			GRIP_MIN_Y, empty_y - (PUZZLE_ROW_GAP+SUBIMAGE_HEIGHT),
			NULL);
		position_grip_above(empty_x, empty_y);
		xv_set(tmp, XV_SHOW, FALSE, NULL);
	} else
		xv_set(grip_above, XV_SHOW, FALSE, NULL);


	if(column < N_HORI-1) {
		tmp = (Drawimage) xv_get(puzzle_tile, 
				ARRAY_TILE_POSITION, column+1, row);
		xv_set(grip_right, 
			DRAWIMAGE_IMAGE1, xv_get(tmp, DRAWIMAGE_IMAGE1),
			DRAWIMAGE_IMAGE2, xv_get(tmp, DRAWIMAGE_IMAGE2),
			XV_SHOW, TRUE,
			GRIP_MAX_X, empty_x + (PUZZLE_COLUMN_GAP+SUBIMAGE_WIDTH)
					+ SUBIMAGE_WIDTH,
			GRIP_MIN_X, empty_x,
			NULL);
		position_grip_right(empty_x, empty_y);
		xv_set(tmp, XV_SHOW, FALSE, NULL);
	} else
		xv_set(grip_right, XV_SHOW, FALSE, NULL);


	if(row < N_VERT-1) {
		tmp = (Drawimage) xv_get(puzzle_tile, 
				ARRAY_TILE_POSITION, column, row+1);
		xv_set(grip_below, 
			DRAWIMAGE_IMAGE1, xv_get(tmp, DRAWIMAGE_IMAGE1),
			DRAWIMAGE_IMAGE2, xv_get(tmp, DRAWIMAGE_IMAGE2),
			XV_SHOW, TRUE,
			GRIP_MAX_Y, empty_y + (PUZZLE_ROW_GAP+SUBIMAGE_HEIGHT) 
				+ SUBIMAGE_HEIGHT,
			GRIP_MIN_Y, empty_y,
			NULL);
		position_grip_below(empty_x, empty_y);
		xv_set(tmp, XV_SHOW, FALSE, NULL);
	} else
		xv_set(grip_below, XV_SHOW, FALSE, NULL);


	if(column > 0) {
		tmp = (Drawimage) xv_get(puzzle_tile, 
				ARRAY_TILE_POSITION, column-1, row);
		xv_set(grip_left, 
			DRAWIMAGE_IMAGE1, xv_get(tmp, DRAWIMAGE_IMAGE1),
			DRAWIMAGE_IMAGE2, xv_get(tmp, DRAWIMAGE_IMAGE2),
			XV_SHOW, TRUE,
			GRIP_MAX_X, empty_x + SUBIMAGE_WIDTH,
			GRIP_MIN_X, empty_x-(PUZZLE_COLUMN_GAP+SUBIMAGE_WIDTH),
			NULL);
		position_grip_left(empty_x, empty_y);
		xv_set(tmp, XV_SHOW, FALSE, NULL);
	} else
		xv_set(grip_left, XV_SHOW, FALSE, NULL);

	xv_set(canshell, 
		CANVAS_SHELL_DELAY_REPAINT, FALSE, 
		NULL);
}


void
scramble_proc(item, event)
	Panel_item 	item;
	Event		*event;
{
	int 	i;
	int	new_x, new_y;
	int	current_x, current_y;

	/*
	 * The following is a matter of preference. If uncommented, it will
	 * not show the shuffling as the puzzle is scrambled.  Make sure
	 * the xv_set at the end is also uncommented if this is changed.
	 */
	/*
	xv_set(canshell, 
		CANVAS_SHELL_DELAY_REPAINT, TRUE, 
		NULL);
	*/

	show_hidden_pieces();

	new_x = current_x = xv_get(puzzle_tile, ARRAY_TILE_COLUMN, 
			get_solved_piece(EMPTY_X, EMPTY_Y));
	new_y = current_y = xv_get(puzzle_tile, ARRAY_TILE_ROW, 
			get_solved_piece(EMPTY_X, EMPTY_Y));

	/*
	 * Lame scramble...
	 */
	for (i=0; i < 100; i++) {

		switch ((rand()>>7) % 4) {
		  case 0:
			new_x++;
			if(new_x == N_HORI)
				new_x = N_HORI-1;
			break;
		  case 1:
			new_x--;
			if(new_x < 0)
				new_x = 1;
			break;
		  case 2:
			new_y++;
			if(new_y == N_VERT)
				new_y = N_VERT-1;
			break;
		  case 3:
			new_y--;
			if(new_y < 0)
				new_y = 1;
			break;
		}

		swap_pieces(current_x, current_y, new_x, new_y);
		current_x = new_x;
		current_y = new_y;
	}

	set_empty_spot();

	xv_set(frame, 
		FRAME_LEFT_FOOTER, "Click or slide pieces to move them.",
		NULL);

	/*
	xv_set(canshell, 
		CANVAS_SHELL_DELAY_REPAINT, FALSE,
		NULL);
	*/
}


void
solve_proc(item, event)
	Panel_item 	item;
	Event		*event;
{
	/* This could have more flash, ala the X11 puzzle program. */
	int x, y;

	xv_set(canshell, 
		CANVAS_SHELL_DELAY_REPAINT, TRUE,
		NULL);

	show_hidden_pieces();

	for (y=0; y< N_VERT; y++)
	  for (x=0; x< N_HORI; x++)
		xv_set(puzzle_tile, 
			ARRAY_TILE_POSITION, get_solved_piece(x,y), x, y,
			NULL);

	set_empty_spot();

	xv_set(canshell, 
		CANVAS_SHELL_DELAY_REPAINT, FALSE,
		NULL);
}


void
show_hidden_pieces()
{
	Drawimage	empty;
	int		column;
	int		row;

	empty = get_solved_piece(EMPTY_X, EMPTY_Y);
	column = (int) xv_get(puzzle_tile, ARRAY_TILE_COLUMN, empty);
	row = (int) xv_get(puzzle_tile, ARRAY_TILE_ROW, empty);

	if(row > 0)
		xv_set(xv_get(puzzle_tile, ARRAY_TILE_POSITION, column, row-1),
			XV_SHOW, TRUE,
			NULL);

	if(column < N_HORI-1)
		xv_set(xv_get(puzzle_tile, ARRAY_TILE_POSITION, column+1, row),
			XV_SHOW, TRUE,
			NULL);

	if(row < N_VERT-1)
		xv_set(xv_get(puzzle_tile, ARRAY_TILE_POSITION, column, row+1),
			XV_SHOW, TRUE,
			NULL);

	if(column > 0)
		xv_set(xv_get(puzzle_tile, ARRAY_TILE_POSITION, column-1, row),
			XV_SHOW, TRUE,
			NULL);
}



swap_pieces(x1, y1, x2, y2)
	int	x1, y1;
	int	x2, y2;
{
	Drawimage piece1, piece2;
	/*
	 * Put the piece at (x1, y1) at (x2, y2) and the piece at 
	 * (x2, y2) at (x1, y1).
	 */

	piece1 = (Drawimage) xv_get(puzzle_tile, 
				ARRAY_TILE_POSITION, x1, y1);

	piece2 = (Drawimage) xv_get(puzzle_tile, 
				ARRAY_TILE_POSITION, x2, y2);

	xv_set(puzzle_tile, 
		ARRAY_TILE_POSITION, piece1, x2, y2,
		ARRAY_TILE_POSITION, piece2, x1, y1,
		NULL);
}


check_finished()
{
	int x, y;

	for (y=0; y< N_VERT; y++)
	  for (x=0; x< N_HORI; x++)
		if(xv_get(puzzle_tile, ARRAY_TILE_POSITION, x, y) !=
		   get_solved_piece(x, y))
			return;
	xv_set(frame, 
		FRAME_LEFT_FOOTER, "Congratulations!",
		NULL);

}


void 
grip_moved(paint_window, event, canvas_shell, grip)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
{
	Drawimage	empty;
	int		empty_x, empty_y;
	int		column, row;

	empty = get_solved_piece(EMPTY_X, EMPTY_Y);
	column = (int) xv_get(puzzle_tile, ARRAY_TILE_COLUMN, empty);
	row = (int) xv_get(puzzle_tile, ARRAY_TILE_ROW, empty);
	empty_x = (int) xv_get(empty, XV_X);
	empty_y = (int) xv_get(empty, XV_Y);

	/* 
	 * Check to see if mouse is over the empty spot,
	 * otherwise put it back where it started.
	 */

	if(grip == grip_above) {
		if (event_y(event) > empty_y) {
			show_hidden_pieces();
			swap_pieces(column, row, column, row-1);
			set_empty_spot();
			check_finished();
		} else
			position_grip_above(empty_x, empty_y);
	}

	if(grip == grip_right) {
		if (event_x(event) < empty_x+SUBIMAGE_WIDTH) {
			show_hidden_pieces();
			swap_pieces(column, row, column+1, row);
			set_empty_spot();
			check_finished();
		} else
			position_grip_right(empty_x, empty_y);
	}

	if(grip == grip_below) {
		if(event_y(event) < empty_y+SUBIMAGE_HEIGHT) {
			show_hidden_pieces();
			swap_pieces(column, row, column, row+1);
			set_empty_spot();
			check_finished();
		} else
			position_grip_below(empty_x, empty_y);
	} 

	if(grip == grip_left) {
		if(event_x(event) > empty_x) {
			show_hidden_pieces();
			swap_pieces(column, row, column-1, row);
			set_empty_spot();
			check_finished();
		} else
			position_grip_left(empty_x, empty_y);
	}
}


void 
grip_dbl_clk(paint_window, event, canvas_shell, grip)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
{
	Drawimage	empty;
	int		column, row;

	empty = get_solved_piece(EMPTY_X, EMPTY_Y);
	column = (int) xv_get(puzzle_tile, ARRAY_TILE_COLUMN, empty);
	row = (int) xv_get(puzzle_tile, ARRAY_TILE_ROW, empty);

	show_hidden_pieces();

	if(grip == grip_above)
		swap_pieces(column, row, column, row-1);

	if(grip == grip_right)
		swap_pieces(column, row, column+1, row);

	if(grip == grip_below)
		swap_pieces(column, row, column, row+1);

	if(grip == grip_left)
		swap_pieces(column, row, column-1, row);

	set_empty_spot();
	check_finished();
}


void 
piece_dbl_clk(paint_window, event, canvas_shell, drawimage)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Drawimage	drawimage;
{
	Drawimage	empty;
	int		empty_column, empty_row;
	int		column, row;
	int		i;

	empty = get_solved_piece(EMPTY_X, EMPTY_Y);
	empty_column = (int) xv_get(puzzle_tile, ARRAY_TILE_COLUMN, empty);
	empty_row = (int) xv_get(puzzle_tile, ARRAY_TILE_ROW, empty);

	column = (int) xv_get(puzzle_tile, ARRAY_TILE_COLUMN, drawimage);
	row = (int) xv_get(puzzle_tile, ARRAY_TILE_ROW, drawimage);

	if((column != empty_column) && (row != empty_row))
		return;

	show_hidden_pieces();

	if(column == empty_column) {
		if(row>empty_row)
			for(i=empty_row; i< row; i++)
				swap_pieces(column, i, column, i+1);
		else
			for(i=empty_row; i> row; i--)
				swap_pieces(column, i, column, i-1);
	}
	if(row == empty_row) {
		if(column>empty_column)
			for(i=empty_column; i< column; i++)
				swap_pieces(i, row, i+1, row);
		else
			for(i=empty_column; i> column; i--)
				swap_pieces(i, row, i-1, row);
	} 
	set_empty_spot();
	check_finished();
}


void
free_server_images()
{
	int	x, y;
/*
 * The drawimage doesn't automatically destroy the server image
 * attached to it when it is destroyed.  This does that task, and
 * also frees the pixmap associated with the server image.
 */
	for (y=0; y< N_VERT; y++)
	  for (x=0; x< N_HORI; x++) {
		if((x == EMPTY_X) && (y == EMPTY_Y))
			continue;
		xv_destroy( svr_image[x][y] );
	  }
}

