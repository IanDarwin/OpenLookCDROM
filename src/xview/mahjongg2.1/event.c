/*
 * XView port of Mahjongg done by Stan K. Tazuma, 8/91-9/91
 * Copyright 1991.
 *
 * $Header: /home/sirius/skt/cmds/src/sun/mj/RCS/event.c,v 2.5 91/12/22 17:12:37 skt Exp $
 *
 */

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
 */

/*      This file has the event handlers for the background and
 *       tiles in the play panel
 */

#include <stdio.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cursor.h>

#include "mahjongg.h"

/* external definitions */

extern Frame		main_frame;
extern Panel		message_panel;
extern Canvas		play_canvas;
extern Xv_Window	Play_area;
extern Panel_item	TL_hundred;
extern Panel_item	TL_ten;
extern Panel_item	TL_one;
extern Panel_item	message;
extern Panel_item	tile[144];
extern Panel_item	board_num;
extern Xv_Cursor	play_cursor;

extern Server_image	play_cursor_si;
extern Server_image	stick_cursor_si;
extern Server_image	confirm_cursor_si;
extern Server_image	wait_cursor_si;

extern boolean		BandW;
extern Tile		*board[144];
extern int		tile_count;
extern int		seed;
extern Selected		selected[2];

extern Pixrect *Num_icon_mpr[];
extern Server_image Num_icon_si[];

extern boolean Use_num_server_images;
extern boolean Use_canvas_dynamic_cms;
extern boolean Use_error_bells;

/* local globals */

Selected		undo_tiles[72][2];
int			undo_count;		/* gets initialized to -1 in
						 * mahjongg.c/build_image() */
boolean			help_mode = FALSE;
boolean			query_mode = FALSE;

void play_event_proc();
void play_back_proc();
void handle_event();
void message_panel_event_proc();

boolean In_special_event_mode = FALSE;	/* use this to avoid unnecessary
					 * setting or clearing of attributes */

/*******************************************/

void
panel_msg(string, cursor_image, chg_event_handling)
char		*string;
Server_image	cursor_image;
boolean		chg_event_handling;
{
    static char	*old_string = (char *) NULL;

    if (string) {
	if (old_string == string) {
	    /*
	     * Save some CPU cycles and some client-to-server I/O
	     * by not updating the message line when it
	     * already shows what we want.  This would occur when help
	     * mode is being used, because there is a repeated sequence
	     * of calls to help_proc(), and it repeatedly calls this
	     * routine with the same parameters.
	     */
	    xv_set(message, XV_SHOW, TRUE, NULL);
	}
	else {
	    if (old_string)
		xv_set(message, XV_SHOW, FALSE, NULL);

	    xv_set(message, PANEL_LABEL_STRING,
		  string, XV_SHOW, TRUE, NULL);
	}

	if (chg_event_handling) {
	    set_special_event_handling();
	}
	else {
	    clr_special_event_handling();
	}
    }
    else {
	xv_set(message, XV_SHOW, FALSE, NULL);

	/* always turn off special event processing if the string arg is 0 */

	clr_special_event_handling();
    }

    xv_set(play_cursor, CURSOR_IMAGE, cursor_image, NULL);

    set_mj_cursor(play_cursor);

    old_string = string;
}

set_special_event_handling()
{
    if (In_special_event_mode)
	return;

    xv_set(message_panel,
			PANEL_BACKGROUND_PROC,	play_back_proc,
			WIN_IGNORE_EVENTS,
				WIN_ASCII_EVENTS,
				WIN_UP_EVENTS,
				LOC_DRAG,
				PANEL_EVENT_CANCEL,
				NULL,
			NULL);
    In_special_event_mode = TRUE;
}

clr_special_event_handling()
{
    if (!In_special_event_mode)
	return;

    xv_set(message_panel,
			PANEL_BACKGROUND_PROC,	NULL,
			WIN_CONSUME_EVENTS,
				WIN_ASCII_EVENTS,
				WIN_UP_EVENTS,
				LOC_DRAG,
				PANEL_EVENT_CANCEL,
				NULL,
			NULL);
    In_special_event_mode = FALSE;
}

void
help_proc()
{
    int 		i;
    Tile 		*data[2];
    static int	parse[2] = { 0, 0 };

    if (! (selected[0].in_preview_mode))
    {
	if (! (help_mode)) { /* Just starting. init and recall */
	    help_mode = TRUE;
	    parse[0] = 143;
	    parse[1] = 142;
	}

	for ( ; parse[0] >= 0; parse[0]--)
	{
	    if (!((board[parse[0]]->top_free &&		    /* uncovered */
		(board[parse[0]]->left_free || board[parse[0]]->right_free) && /* open */
		(!(board[parse[0]]->removed))))) {  /* not already used */

		continue; /* not available go to next */

	    }

	    for(; parse[1] >= 0; parse[1]--) { /* check for second tile */

		if ((board[parse[0]]->value == board[parse[1]]->value) &&	/* right value */
		    (parse[0] != parse[1]) &&					/* different item */
		    (board[parse[1]]->top_free &&				/* uncovered */
		    (board[parse[1]]->left_free || board[parse[1]]->right_free) && /* open */
		    (!(board[parse[1]]->removed)))) { /* not already used */

		    /* Found a match, show it */

		    /* flag found items */
		    selected[0].in_preview_mode = TRUE;
		    selected[1].in_preview_mode = TRUE;
		    selected[0].tileptr = board[parse[0]];
		    selected[1].tileptr = board[parse[1]];

		    begin_preview(&selected[0]);
		    begin_preview(&selected[1]);

		    panel_msg("Show next move? [Y] [] [N]", confirm_cursor_si,
			TRUE);

		    parse[1]--; /* do loop step */
		    return; /* all done this rotation */
		}
	    } /* else go to next */

	    parse[1] = parse[0] - 2; /* going around again */
	}

	/* no more moves beep and show message */

	help_mode = FALSE;
	mj_window_bell(main_frame);
	panel_msg("No more moves.", play_cursor_si, FALSE);
    }
    else { /* search for available match */

	if (selected[1].in_preview_mode) { /* deselect last choice */

	    /* cancel preview of selected tiles */
	    cancel_preview(&selected[1]);

	    /* Clean up selected's variables */
	    selected[1].in_preview_mode = FALSE;
	}

	if (!query_mode) {
	    query_mode = TRUE;
	    parse[0] = 143;
	}

	data[0] = selected[0].tileptr;

	for(i = parse[0]; i >= 0; i--) {

	    if ((board[i]->value == data[0]->value) && /* right value */
		(board[i] != selected[0].tileptr) &&	/* different item */
		(board[i]->top_free &&		    /* uncovered */
		(board[i]->left_free || board[i]->right_free) && /* open */
		(!(board[i]->removed)))) { /* not already used */

		/* found one */

		selected[1].in_preview_mode = TRUE;
		selected[1].tileptr = board[i];

		/* turn on preview */

		begin_preview(&selected[1]);

		/* set confirm message */

		panel_msg("Please confirm. [Y] [NEXT] [N]", confirm_cursor_si,
			TRUE);

		/* return to sender */

		parse[0] = i - 1;
		return;
	    }
	}

	query_mode = FALSE;
	selected[0].in_preview_mode = FALSE;
	cancel_preview(&selected[0]);
	mj_window_bell(main_frame);
	panel_msg("No match.", play_cursor_si, FALSE);
    }
}

void
remove_tiles(REMOVE)
boolean	REMOVE;
{
    Tile *data[2];
    int i;
    int tiles_left_hun;
    int tiles_left_ten;
    int tiles_left_one;

    if (REMOVE) {
	/* get data from items to be removed */
	data[0] = selected[0].tileptr;
	data[1] = selected[1].tileptr;
    } else {
	/* get data from items to be replaced */
	data[0] = undo_tiles[undo_count][0].tileptr;
	data[1] = undo_tiles[undo_count][1].tileptr;
    }

    /* adjust adjacent tiles */
    for(i = 0; i < 2 && data[0]->left_next[i] != DONT_CARE;
		board[data[0]->left_next[i]]->right_free = REMOVE, i++);

    for(i = 0; i < 2 && data[1]->left_next[i] != DONT_CARE;
		board[data[1]->left_next[i]]->right_free = REMOVE, i++);

    for(i = 0; i < 2 && data[0]->right_next[i] != DONT_CARE;
		board[data[0]->right_next[i]]->left_free = REMOVE, i++);

    for(i = 0; i < 2 && data[1]->right_next[i] != DONT_CARE;
		board[data[1]->right_next[i]]->left_free = REMOVE, i++);

    /* adjust covered tiles and images */
    for (i = 0; i < 4 && data[0]->covered[i] != DONT_CARE; i++)
	board[data[0]->covered[i]]->top_free = REMOVE;

    for (i = 0; i < 4 && data[1]->covered[i] != DONT_CARE; i++) 
	board[data[1]->covered[i]]->top_free = REMOVE;

    if (data[0]->covered[0] != DONT_CARE && data[0]->covered[1] == DONT_CARE)
	board[data[0]->covered[0]]->top_covered = ! REMOVE;
    if (data[1]->covered[0] != DONT_CARE && data[1]->covered[1] == DONT_CARE)
	board[data[1]->covered[0]]->top_covered = ! REMOVE;

    /* set removed flags */
    data[0]->removed = REMOVE;
    data[1]->removed = REMOVE;

    if (REMOVE) {
	/* turn off preview */
	/* use reverse order, otherwise it won't work in some cases */
	cancel_preview(&selected[1]);
	cancel_preview(&selected[0]);
    } else  /* check to see if previewing an item and un-preview and select */
	if (selected[0].in_preview_mode) {
	    cancel_preview(&selected[0]);
	    selected[0].in_preview_mode = FALSE;
	}

    /* fix playing field */
    if (REMOVE) {
	clear_and_repaint();
    }
    else {
	do_repaint();
    }

    /* deselect tiles */
    selected[0].in_preview_mode = FALSE;
    selected[1].in_preview_mode = FALSE;

    /* fix tile counter */
    tile_count += (REMOVE) ? -2 : 2;

    tiles_left_hun = tile_count / 100;
    tiles_left_ten = (tile_count - (tiles_left_hun * 100)) / 10;
    tiles_left_one = tile_count - (tiles_left_hun * 100) - (tiles_left_ten * 10);

    /* display hundreds tile by own status */
    xv_set(TL_hundred, XV_SHOW, tiles_left_hun, 0);

    /* display tens tile by own status ored with hundreds status */
    xv_set(TL_ten, XV_SHOW, tiles_left_hun || tiles_left_ten, 0);

    if (Use_num_server_images) {
	xv_set(TL_ten, PANEL_LABEL_IMAGE, Num_icon_si[tiles_left_ten], 0);

	/* only need even tiles */
	if ((tiles_left_one % 2) == 0)
	    xv_set(TL_one, PANEL_LABEL_IMAGE, Num_icon_si[tiles_left_one], 0);
    }
    else {
	xv_set(TL_ten, PANEL_LABEL_IMAGE, Num_icon_mpr[tiles_left_ten], 0);

	/* only need even tiles */
	if ((tiles_left_one % 2) == 0)
	    xv_set(TL_one, PANEL_LABEL_IMAGE, Num_icon_mpr[tiles_left_one], 0);
    }

    if (REMOVE) {

	/* update undo_count */
	undo_count++;

	/* update removed array */
	undo_tiles[undo_count][0].in_preview_mode = TRUE;
	undo_tiles[undo_count][0].tileptr = selected[0].tileptr;

	undo_tiles[undo_count][1].in_preview_mode = TRUE;
	undo_tiles[undo_count][1].tileptr = selected[1].tileptr;

	/* remove confirm message */
	panel_msg((char *) NULL, play_cursor_si, FALSE);

	/* check for clean board and congrat them */

	if ( tiles_left_hun == 0 && tiles_left_ten == 0 && tiles_left_one == 0) 
		xv_set(message, PANEL_LABEL_STRING,
			"Congratulations!! Press 'AGAIN' or 'NEW'",
			XV_SHOW, TRUE,
			0);

    } else { /* decrement undo_count */
	undo_tiles[undo_count][0].in_preview_mode = FALSE;
	undo_tiles[undo_count][1].in_preview_mode = FALSE;
	undo_count--;
    }
}

void
message_panel_event_proc(window, event)
Xv_Window	window;
Event		*event;
{
    static boolean event_was_button_press = FALSE;
			/* if TRUE, then the event was a button press over
			 * a panel button.  (The only way this routine gets
			 * called is if one of the panel items which accepts
			 * events gets an event; at this point in time,
			 * that is either a button or a text item).
			 */
    if (help_mode || query_mode) {
	if (!event_is_down(event))
	    return;
	
	if (event_id(event) == MS_LEFT ||
	    event_id(event) == MS_MIDDLE ||
	    event_id(event) == MS_RIGHT
	    ) {
	    event_was_button_press = TRUE;
	    handle_event(window, event);
	    return;
	}
	/* ignore the event if we get here */
    }
    else {
	if (event_was_button_press) {
	    /*
	     * We have to make sure we ignore the button release event
	     * that may or may not occur after turning events back on.
	     * This depends on whether the mouse cursor is over
	     * a panel button when the mouse button is released.
	     * Once we have ignored that particular button release event,
	     * all is well and we can go back to the normal operating mode.
	     * An easy way to accomplish this feat is to just ignore all
	     * up events until after we get a down event.  This scheme
	     * will work for both ASCII events or mouse events coming in.
	     */
	    if (!event_is_down(event))
		return;

	    event_was_button_press = FALSE;
	}
	panel_default_handle_event(window, event);
    }
}

void
play_back_proc(window, event)
Xv_Window	window;
Event		*event;
{
    if (!event_is_down(event))
	return;

    if (event_id(event) == MS_LEFT ||
	event_id(event) == MS_MIDDLE ||
	event_id(event) == MS_RIGHT
	) {

	handle_event(window, event);
    }
}

void
handle_event(window, event)
Xv_Window	window;
Event		*event;
{
    if ((event_id(event) == MS_MIDDLE)
		&& selected[0].in_preview_mode && !help_mode) {

	help_proc();

    } else {

	query_mode = FALSE;

	if (selected[1].in_preview_mode) { /* doing confirm  or next help */

	    switch (event_id(event)) {

	    case MS_LEFT:
		/* confirmed selection */
		if (help_mode) { /* do next help */

		    /* cancel preview of selected tiles */
		    /*
		     * use reverse order in order to deal with the case where
		     * the first tile selected is "adjacent" to the second tile
		     * selected, such that they visually overlap (this includes
		     * cases where one is on a higher level than the other)
		     */
		    cancel_preview(&selected[1]);
		    cancel_preview(&selected[0]);

		    /* Clean up selected's variables */
		    selected[0].in_preview_mode = FALSE;
		    selected[1].in_preview_mode = FALSE;

		    /* do next help */
		    help_proc();

		} else { /* confirmed selection. remove them */

		    remove_tiles(TRUE);

		}
		break;

	    case MS_RIGHT:
		/* refused selection */

		/* cancel preview of selected tiles */
		/*
		 * use reverse order in order to deal with the case where
		 * the first tile selected is "adjacent" to the second tile
		 * selected, such that they visually overlap (this includes
		 * cases where one is on a higher level than the other)
		 */
		cancel_preview(&selected[1]);
		cancel_preview(&selected[0]);

		/* Clean up selected's variables */
		selected[0].in_preview_mode = FALSE;
		selected[1].in_preview_mode = FALSE;

		/* remove confirm message */
		panel_msg((char *) NULL, play_cursor_si, FALSE);

		help_mode = FALSE;	/* may or may not be in help_mode at
					 * this point, but make sure we
					 * are not */
		break;
	    }
	}
    }
}

void
play_event_proc(window, event)
Xv_Window	window;
Event		*event;
{
    Tile	*data;
    int	value;
    int	i = -1;

    if (!event_is_down(event))
	return;

    if (	event_id(event) != MS_LEFT
	   &&	event_id(event) != MS_MIDDLE
	   &&	event_id(event) != MS_RIGHT) {

	return;
    }

    /* check to see if in help_mode */

    if (help_mode || query_mode) {
	play_back_proc(window, event);
	return;
    }

    /* check to see if just confirming */

    if (selected[1].in_preview_mode) {
	play_back_proc(window, event);
	return;
    }

    /*
     * The following used to be done only in play_back_proc(), back in the
     * SunView version when both a PANEL_EVENT_PROC and a PANEL_BACKGROUND_PROC
     * were specified as follows:
     *				PANEL_EVENT_PROC, play_event_proc,
     *				PANEL_BACKGROUND_PROC, play_back_proc,
     * In the SunView version, a middle mouse button that occurred over
     * a PANEL_BUTTON_IMAGE went to this play_event_proc routine, and
     * eventually passed through to the switch statement that detects
     * the MS_MIDDLE event.  If the
     * middle mouse button occurred over blank space in the panel, then
     * the PANEL_BACKGROUND_PROC (play_back_proc) must have been invoked,
     * and the proper results occurred.
     *
     * Since the logic of this routine has been changed to kick out if the
     * tile search doesn't find anything, I put the "if" below, which
     * gives an early detection of a request for help.  I think this is
     * better anyway.  Makes more sense to handle it right up front here,
     * than do all the checks and stuff after here.
     */

    if (	(event_id(event) == MS_MIDDLE)
		&& selected[0].in_preview_mode
		&& !help_mode) {
	help_proc();
	return;
    }

    /*
     * Clear the message line here because if in help_mode or query_mode,
     * we don't need or want to clear the message line.  We only want to
     * clear it after one of the messages I added where only a
     * mj_window_bell() call used to be.  In those situations,
     * we are no longer in help_mode.
     */
    xv_set(message, XV_SHOW, FALSE, NULL);

    if (event_id(event) != MS_LEFT)
	return;

    /*
     * Could put in code here to do a range check on the
     * event_x(event) and event_y(event) coordinates, so that we don't
     * waste time checking events beyond the valid range.  Could also
     * add code to update the min and max, as tiles are removed.
     * But this would add complexity to what is a simple search technique.
     */

    i = find_tile(event_x(event), event_y(event));

    /* get data from item selected */

    if (i >= 0 && i <= 143)
	data = board[i];
    else {
	return;		/* couldn't find any tile at that event location,
			 * so ignore the event and return */
    }

    value = data->value;

    /* Left button down begin selection */
    if ( data->top_free && (data->left_free || data->right_free)) {

	if (!(selected[0].in_preview_mode)) {

	    /* fill first selection if empty */
	    selected[0].tileptr = board[i];
	    selected[0].in_preview_mode = TRUE;
	    begin_preview(&selected[0]);

	} else {

	    if (board[i] == selected[0].tileptr) { /* deselect item */

		selected[0].in_preview_mode = FALSE;
		cancel_preview(&selected[0]);

	    } else {
		data = selected[0].tileptr;
		if ( value == data->value) {
		    /* fill second and show confirm message */

		    selected[1].tileptr = board[i];
		    selected[1].in_preview_mode = TRUE;
		    begin_preview(&selected[1]);

		    panel_msg("Please confirm. [Y] [] [N]",
			    confirm_cursor_si, TRUE);
		} else { /* beep at them */

		    panel_msg("Not a valid match; try again.",
			    play_cursor_si, FALSE);
		    mj_window_bell(main_frame);
		}
	    }
	}
    } else { /* beep at them */
	panel_msg("Tile not playable; try again.", play_cursor_si, FALSE);
	mj_window_bell(main_frame);
    }
}

void
quit_proc()
{
    xv_destroy_safe(main_frame);
    exit(0);
}

void
new_proc()
{
    seed = random() % 20011;
    build_image(FALSE);
    place_tiles(FALSE);
}

void
again_proc()
{
    build_image(TRUE);
    place_tiles(FALSE);
}

void
undo_proc()
{
    if (undo_count < 0) {
	panel_msg("No more to undo.", play_cursor_si, FALSE);
	mj_window_bell(main_frame);
    }
    else
	remove_tiles(FALSE);
}

void
board_num_proc()
{
    sscanf((char *) xv_get(board_num, PANEL_VALUE), "%d", &seed);
    build_image(FALSE);
    place_tiles(FALSE);
}

begin_preview(selptr)
Selected *selptr;
{
    extern Pixrect *xv_mem_create();
    static Pixrect *p = NULL;
    Tile *tileptr = selptr->tileptr;
    static boolean plane_mask_set = FALSE;

    if (BandW) {
	if (! plane_mask_set) {
	    set_plane_mask(Play_area, 0x1);
	    plane_mask_set = TRUE;
	}

	pw_rop(Play_area, tileptr->x_loc, tileptr->y_loc,
		64, 64, PIX_NOT(PIX_SRC),
		Play_area, tileptr->x_loc, tileptr->y_loc);
    }
    else if (Use_canvas_dynamic_cms) {
	if (! plane_mask_set) {
	    set_plane_mask(Play_area, 0x7);
	    plane_mask_set = TRUE;
	}

	pw_rop(Play_area, tileptr->x_loc, tileptr->y_loc,
		64, 64, PIX_NOT(PIX_SRC),
		Play_area, tileptr->x_loc, tileptr->y_loc);
    }
    else {
	if (p == NULL) {
	    p = xv_mem_create(64, 64, 8);
	    if (p == NULL) {
		fprintf(stderr, "begin_preview: xv_mem_create returned NULL\n");
		return;
	    }
	}
	if (selptr->si_before_preview == NULL) {
	    selptr->si_before_preview =
			(Server_image) xv_create(XV_NULL, SERVER_IMAGE,
				XV_WIDTH,		64,
				XV_HEIGHT,		64,
				SERVER_IMAGE_DEPTH,	8,
				SERVER_IMAGE_COLORMAP,	"mahjongg_canvas_cms",
				NULL);
	    if (selptr->si_before_preview == NULL) {
		fprintf(stderr,
			"begin_preview: error, couldn't create server image\n");
		return;
	    }
	}

	/* save the "before" image into a server image, for fast update
	 * if the user does a cancel on the tile */

	pw_read(selptr->si_before_preview, 0, 0, 64, 64, PIX_SRC,
		    Play_area, tileptr->x_loc, tileptr->y_loc);

	/* pull the "before" image into a local pixrect */
	pw_read(p, 0, 0, 64, 64, PIX_SRC,
		    selptr->si_before_preview, 0, 0);

	convert_physical_to_logical_colors_pixrect(p);

	invert_pixrect(p);

	/* put back the modified pixrect */
	pw_write(Play_area, tileptr->x_loc, tileptr->y_loc, 64, 64, PIX_SRC,
		    p, 0, 0);
    }
}

cancel_preview(selptr)
Selected *selptr;
{
    Tile *tileptr = selptr->tileptr;

    if (BandW) {
	pw_rop(Play_area, tileptr->x_loc, tileptr->y_loc,
		64, 64, PIX_NOT(PIX_SRC),
		Play_area, tileptr->x_loc, tileptr->y_loc);
    }
    else if (Use_canvas_dynamic_cms) {
	pw_rop(Play_area, tileptr->x_loc, tileptr->y_loc,
		64, 64, PIX_NOT(PIX_SRC),
		Play_area, tileptr->x_loc, tileptr->y_loc);
    }
    else {
	pw_write(Play_area, tileptr->x_loc, tileptr->y_loc, 64, 64, PIX_SRC,
		selptr->si_before_preview, 0, 0);
    }
}

clear_and_repaint()
{
    /*
     * This code clears just the two tiles that have been removed.
     * The two tiles are the only areas that really have to be cleared
     * for a proper update to the window, when the board is re-drawn.
     * The result is a much smoother update to the screen than if
     * the whole window is cleared.
     */
    pw_writebackground(Play_area,
	    selected[0].tileptr->x_loc, selected[0].tileptr->y_loc,
	    64, 64,
	    PIX_CLR);
    pw_writebackground(Play_area,
	    selected[1].tileptr->x_loc, selected[1].tileptr->y_loc,
	    64, 64,
	    PIX_CLR);
    do_repaint();
}

/*
 * local_mem_rop() is never called by any of the routines here.  It will
 * be called by XView internal code, after the address of it is stuffed
 * into an XView internal data structure.
 *
 * See the init_local_mem_rop() routine below for more info.
 */
local_mem_rop(dpr, dx, dy, width, height, op, spr, sx, sy)
Pixrect *dpr;
int dx, dy, width, height;
int op;
Pixrect *spr;
int sx, sy;
{
    extern struct pixrectops mem_ops;

    /* we support a direct one-to-one copy of a memory
     * pixrect to another memory pixrect */
    if (op == PIX_SRC
	&& dpr->pr_ops == &mem_ops	/* is a memory pixrect */
	&& spr->pr_ops == &mem_ops	/* is a memory pixrect */
	) {

	/*
	dpr->pr_ops = spr->pr_ops;
	*/
	/* depth should be the same
	dpr->pr_depth = spr->pr_depth;
	*/
	/* don't change height and width
	dpr->pr_height = height;
	dpr->pr_width = width;
	*/
	mpr_d(dpr)->md_linebytes = mpr_d(spr)->md_linebytes;
	mpr_d(dpr)->md_offset = mpr_d(spr)->md_offset;
	mpr_d(dpr)->md_primary = mpr_d(spr)->md_primary;
	mpr_d(dpr)->md_flags = mpr_d(spr)->md_flags;
	bcopy(PIXRECT_IMAGE_DATA_PTR(spr), 
	    PIXRECT_IMAGE_DATA_PTR(dpr),
	    (width * height * PIXRECT_IMAGE_DEPTH(spr))/8);
    }
    else {
	fprintf(stderr, "local_mem_rop: NO-OP\n");
    }
}

/*
 * This routine stuffs the address of a local function into an internal
 * XView data structure.  This isn't necessarily the cleanest way to
 * accomplish what I wanted, but it was the easiest.  Had to do it
 * because while XView supports most of the code to read images from the
 * display, it stops short by not providing the code for copying pixrects.
 * The hooks are there, but instead of providing the code, XView
 * puts in stub routines which tell the user that the feature is not
 * supported.  So without this local_mem_rop stuff, pw_read()'s would
 * not work.  When and if XView does support the code, this routine and
 * also local_mem_rop() won't be needed.
 */
init_local_mem_rop()
{
    extern struct pixrectops mem_ops;

    mem_ops.pro_rop = local_mem_rop;
}

/*
 * Set up the canvas paint window with a bit mask of 'n' for drawing purposes.
 * This allows us to do a pixmap color inversion entirely on the
 * server, without reading the pixmap data back to the client side.
 *
 * This operation must be done on the canvas paint window, not the canvas.
 */
set_plane_mask(window, n)
Xv_Window window;
int n;
{
    pw_putattributes(window, &n);
}

mj_window_bell(window)
Xv_Window window;
{
    if (Use_error_bells)
	window_bell(window);
}
