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
#ifndef lint
static char *rcs = "$header$ Copyright 1988 Mark A. Holm";
#endif !lint

/*      This file has the event handlers for the background and
 *       tiles in the play panel
 */

#include <stdio.h>
#include <sys/types.h>
#include <suntool/sunview.h>
#include <suntool/panel.h>
#include <sunwindow/notify.h>
#include <pixrect/pixrect.h>

#include "mahjongg.h"

/* external definitions */

extern Frame		main_frame;
extern Panel		play_panel, message_panel;
extern Panel_item	TL_hundred;
extern Panel_item	TL_ten;
extern Panel_item	TL_one;
extern Panel_item	message;
extern Panel_item	tile[144];
extern Panel_item	tiles_left[3];
extern Panel_item	board_num;
extern Cursor		play_cursor;

extern boolean		BandW;
extern Tiles		*board[144];
extern int		tile_count;
extern int		seed;
extern Selected		selected[2];

/* local globals */

Selected		undo_tiles[144][2];
int			undo_count;
boolean			help_mode = FALSE;
boolean			query_mode = FALSE;

/*******************************************/

void panel_msg(string, cursor_image)
char		*string;
Pixrect		*cursor_image;

{
    static char	*old_string = (char *)0;

    if (string) {

	if (old_string)
	    panel_set(message,PANEL_SHOW_ITEM, FALSE, 0);

	panel_set(message, PANEL_LABEL_STRING,
		  string, PANEL_SHOW_ITEM, TRUE, 0);

	window_set(message_panel, WIN_IGNORE_PICK_EVENTS,
				    WIN_MOUSE_BUTTONS, 0, 0);
	window_set(message_panel, WIN_IGNORE_KBD_EVENT,
				    WIN_ASCII_EVENTS, 0, 0);
	cursor_set(play_cursor, CURSOR_IMAGE, cursor_image, 0);
	window_set(play_panel, WIN_CURSOR, play_cursor, 0);

    } else {

	panel_set(message,PANEL_SHOW_ITEM, FALSE, 0);
	window_set(message_panel, WIN_CONSUME_PICK_EVENTS,
				    WIN_MOUSE_BUTTONS, 0, 0);
	window_set(message_panel, WIN_CONSUME_KBD_EVENT,
				    WIN_ASCII_EVENTS, 0, 0);
	cursor_set(play_cursor, CURSOR_IMAGE, cursor_image, 0);
	window_set(play_panel, WIN_CURSOR, play_cursor, 0);

    }

    old_string = string;

}

void help_proc(item, event)
Panel_item	item;
Event		*event;

{
    int 		i;
    Tiles 		*data[2];
    static int	parse[2] = { 0, 0 };

    if(!(selected[0].filled))
    {
	if(!(help_mode)) { /* Just starting. init and recall */

	    help_mode = TRUE;
	    parse[0] = 143;
	    parse[1] = 142;

	}

	for(; parse[0] >= 0; parse[0]--)
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
		    selected[0].filled = TRUE;
		    selected[1].filled = TRUE;

		    /* fake in some data */
		    selected[0].item = tile[parse[0]];
		    selected[0].event.ie_locx = ((int) panel_get(tile[parse[0]], PANEL_ITEM_X) + 10);
		    selected[0].event.ie_locy = ((int) panel_get(tile[parse[0]], PANEL_ITEM_Y) + 10);
		    selected[0].event.ie_time.tv_sec = event->ie_time.tv_sec;
		    selected[0].event.ie_time.tv_usec = event->ie_time.tv_usec;

		    selected[1].item = tile[parse[1]];
		    selected[1].event.ie_locx = ((int) panel_get(tile[parse[1]], PANEL_ITEM_X) + 10);
		    selected[1].event.ie_locy = ((int) panel_get(tile[parse[1]], PANEL_ITEM_Y) + 10);
		    selected[1].event.ie_time.tv_sec = event->ie_time.tv_sec;
		    selected[1].event.ie_time.tv_usec = event->ie_time.tv_usec;

		    /* Muppet news flash */
		    panel_begin_preview(selected[0].item, selected[0].event);
		    panel_begin_preview(selected[1].item, selected[1].event);

		    panel_msg("Show next move? [Y] [] [N]", &confirm);

		    parse[1]--; /* do loop step */
		    return; /* all done this rotation */

		}

	    } /* else go to next */

	    parse[1] = parse[0] - 2; /* going around again */

	}

	/* no more moves beep and show message */

	help_mode = FALSE;
	window_bell(main_frame);
	panel_msg((char *)0, &stick);

    } else { /* search for available match */

	if (selected[1].filled) { /* deselect last choice */

	    /* cancel preview of selected tiles */
	    panel_cancel_preview(selected[1].item, selected[1].event);
	    /* Clean up selected's variables */
	    selected[1].filled = FALSE;

	}

	if (!query_mode) {

	    query_mode = TRUE;
	    parse[0] = 143;

	}

	data[0] = (Tiles *) panel_get(selected[0].item, PANEL_CLIENT_DATA);

	for(i = parse[0]; i >= 0; i--) {

	    if ((board[i]->value == data[0]->value) && /* right value */
		(tile[i] != selected[0].item) &&    /* different item */
		(board[i]->top_free &&		    /* uncovered */
		(board[i]->left_free || board[i]->right_free) && /* open */
		(!(board[i]->removed)))) { /* not already used */

		/* found one */

		/* fake in some selected data */

		selected[1].item = tile[i];
		selected[1].event.ie_locx = ((int) panel_get(tile[i], PANEL_ITEM_X) + 10);
		selected[1].event.ie_locy = ((int) panel_get(tile[i], PANEL_ITEM_Y) + 10);
		selected[1].event.ie_time.tv_sec = 10; /*sounds good */
		selected[1].event.ie_time.tv_usec = 10; /*sounds good */

		selected[1].filled = TRUE;

		/* turn on preview */

		panel_begin_preview(selected[1].item, selected[1].event);

		/* set confirm message */

		panel_msg("Please confirm. [Y] [NEXT] [N]", &confirm);

		/* return to sender */

		parse[0] = i - 1;
		return;

	    }
	}

	query_mode = FALSE;
	selected[0].filled = FALSE;
	panel_cancel_preview(selected[0].item, selected[0].event);
	panel_msg((char *)0, &stick);
	window_bell(main_frame);

    }
}

void remove_tiles(REMOVE)
boolean	REMOVE;

{
Tiles	*data[2];
int	 i;
int	 tiles_left_hun;
int	 tiles_left_ten;
int	 tiles_left_one;
Pixwin  *pw;
Rect    *r;

    if (REMOVE) {
	/* get data from items to be removed */
	data[0] = (Tiles *) panel_get(selected[0].item, PANEL_CLIENT_DATA);
	data[1] = (Tiles *) panel_get(selected[1].item, PANEL_CLIENT_DATA);

    } else {
	/* get data from items to be replaced */
	data[0] = (Tiles *) panel_get(undo_tiles[undo_count][0].item, PANEL_CLIENT_DATA);
	data[1] = (Tiles *) panel_get(undo_tiles[undo_count][1].item, PANEL_CLIENT_DATA);
    }

    /* adjust adjacent tiles */
    for(i = 0; i < 2 && data[0]->left_next[i] != 999; board[data[0]->left_next[i]]->right_free = REMOVE, i++);
    for(i = 0; i < 2 && data[1]->left_next[i] != 999; board[data[1]->left_next[i]]->right_free = REMOVE, i++);
    for(i = 0; i < 2 && data[0]->right_next[i] != 999; board[data[0]->right_next[i]]->left_free = REMOVE, i++);
    for(i = 0; i < 2 && data[1]->right_next[i] != 999; board[data[1]->right_next[i]]->left_free = REMOVE, i++);

    /* adjust covered tiles and images */
    for(i = 0; i < 4 && data[0]->covered[i] != 999; board[data[0]->covered[i]]->top_free = REMOVE, i++)
	panel_set(tile[data[0]->covered[i]], PANEL_LABEL_IMAGE, (REMOVE || data[0]->covered[i] >= 139) ? board[data[0]->covered[i]]->image : (BandW) ? &BLANK : &cBLANK, 0);
    for(i = 0; i < 4 && data[1]->covered[i] != 999; board[data[1]->covered[i]]->top_free = REMOVE, i++) 
	panel_set(tile[data[1]->covered[i]], PANEL_LABEL_IMAGE, (REMOVE || data[1]->covered[i] >= 139) ? board[data[1]->covered[i]]->image : (BandW) ? &BLANK : &cBLANK, 0);

    /* set removed flags */
    data[0]->removed = REMOVE;
    data[1]->removed = REMOVE;

    if (REMOVE) {
	/* turn off preview */
	panel_cancel_preview(selected[0].item, selected[0].event);
	panel_cancel_preview(selected[1].item, selected[1].event);
    } else  /* check to see if previewing an item and un-preview and select */
	if (selected[0].filled) {
	    panel_cancel_preview(selected[0].item, selected[0].event);
	    selected[0].filled = FALSE;
	}

    /* fix playing field */
    panel_paint(play_panel, PANEL_NONE);
    panel_set((REMOVE) ? selected[0].item : undo_tiles[undo_count][0].item, PANEL_SHOW_ITEM, !REMOVE, 0);
    panel_set((REMOVE) ? selected[1].item : undo_tiles[undo_count][1].item, PANEL_SHOW_ITEM, !REMOVE, 0);
    panel_paint(play_panel, PANEL_NO_CLEAR);

    /* deselect tiles */
    selected[0].filled = FALSE;
    selected[1].filled = FALSE;

    /* fix tile counter */
    tile_count += (REMOVE) ? -2 : 2;

    tiles_left_hun = tile_count / 100;
    tiles_left_ten = (tile_count - (tiles_left_hun * 100)) / 10;
    tiles_left_one = tile_count - (tiles_left_hun * 100) - (tiles_left_ten * 10);

    /* display hundreds tile by own status */
    panel_set(TL_hundred, PANEL_SHOW_ITEM, tiles_left_hun, 0);

    /* display tens tile by own status ored with hundreds status */
    panel_set(TL_ten, PANEL_SHOW_ITEM, tiles_left_hun || tiles_left_ten, 0);

    switch(tiles_left_ten) {
	case 0:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM0 : &cNUM0, 0);
		break;
	case 1:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM1 : &cNUM1, 0);
		break;
	case 2:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM2 : &cNUM2, 0);
		break;
	case 3:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM3 : &cNUM3, 0);
		break;
	case 4:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM4 : &cNUM4, 0);
		break;
	case 5:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM5 : &cNUM5, 0);
		break;
	case 6:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM6 : &cNUM6, 0);
		break;
	case 7:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM7 : &cNUM7, 0);
		break;
	case 8:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM8 : &cNUM8, 0);
		break;
	case 9:
		panel_set(TL_ten, PANEL_LABEL_IMAGE, (BandW) ? &NUM9 : &cNUM9, 0);
		break;
	}

    switch(tiles_left_one) { /* only need even tiles */
	case 0:
		panel_set(TL_one, PANEL_LABEL_IMAGE, (BandW) ? &NUM0 : &cNUM0, 0);
		break;
	case 2:
		panel_set(TL_one, PANEL_LABEL_IMAGE, (BandW) ? &NUM2 : &cNUM2, 0);
		break;
	case 4:
		panel_set(TL_one, PANEL_LABEL_IMAGE, (BandW) ? &NUM4 : &cNUM4, 0);
		break;
	case 6:
		panel_set(TL_one, PANEL_LABEL_IMAGE, (BandW) ? &NUM6 : &cNUM6, 0);
		break;
	case 8:
		panel_set(TL_one, PANEL_LABEL_IMAGE, (BandW) ? &NUM8 : &cNUM8, 0);
		break;
	}

    if (REMOVE) {

	/* update undo_count */
	undo_count++;

	/* update removed array */
	undo_tiles[undo_count][0].item = selected[0].item;
	undo_tiles[undo_count][0].event.ie_locx = selected[0].event.ie_locx;
	undo_tiles[undo_count][0].event.ie_locy = selected[0].event.ie_locy;
	undo_tiles[undo_count][0].event.ie_time.tv_sec = selected[0].event.ie_time.tv_sec;
	undo_tiles[undo_count][0].event.ie_time.tv_usec = selected[0].event.ie_time.tv_usec;
	undo_tiles[undo_count][0].filled = TRUE;

	undo_tiles[undo_count][1].item = selected[1].item;
	undo_tiles[undo_count][1].event.ie_locx = selected[1].event.ie_locx;
	undo_tiles[undo_count][1].event.ie_locy = selected[1].event.ie_locy;
	undo_tiles[undo_count][1].event.ie_time.tv_sec = selected[1].event.ie_time.tv_sec;
	undo_tiles[undo_count][1].event.ie_time.tv_usec = selected[1].event.ie_time.tv_usec;
	undo_tiles[undo_count][1].filled = TRUE;

	/* remove confirm message */

	panel_msg((char *)0, &stick);

	/* check for clean board and congrat them */

	if ( tiles_left_hun == 0 && tiles_left_ten == 0 && tiles_left_one == 0) 
		panel_set(message, PANEL_LABEL_STRING,
			    "Congratulations!! Press 'AGAIN' or 'NEW'",
			   PANEL_SHOW_ITEM,
			    TRUE, 0);

    } else { /* decrement undo_count */
	undo_tiles[undo_count][0].filled = FALSE;
	undo_tiles[undo_count][1].filled = FALSE;
	undo_count--;
    }
}

void play_back_proc(where, event)
Panel		 where;
Event		*event;
{
    if (!event_is_down(event))
	return;

    if ((event_id(event) == MS_MIDDLE) && selected[0].filled && !help_mode) {

	help_proc();

    } else {

	query_mode = FALSE;

	if (selected[1].filled) { /* doing confirm  or next help */

	    switch (event_id(event)) {

	    case MS_LEFT:
		/* confirmed selection */
		if (help_mode) { /* do next help */

		    /* cancel preview of selected tiles */
		    panel_cancel_preview(selected[0].item, selected[0].event);
		    panel_cancel_preview(selected[1].item, selected[1].event);
		    /* Clean up selected's variables */
		    selected[0].filled = FALSE;
		    selected[1].filled = FALSE;

		    /* do next help */
		    help_proc(selected[0].item, event);

		} else { /* confirmed selection. remove them */

		    remove_tiles(TRUE);

		}
		break;

	    case MS_RIGHT:
		/* refused selection */

		/* cancel preview of selected tiles */
		panel_cancel_preview(selected[0].item, selected[0].event);
		panel_cancel_preview(selected[1].item, selected[1].event);
		/* Clean up selected's variables */
		selected[0].filled = FALSE;
		selected[1].filled = FALSE;

		/* remove confirm message */

		panel_msg((char *)0, &stick);

		/* if in help mode toggle out */
		if (help_mode)
		    help_mode = FALSE; 

		break;

	    }
	}
    }
}

void play_event_proc(item, event)
Panel_item		 item;
Event			*event;

{
    Tiles	*data;
    int	value;
    int	i;
    int	x;
    int	y;

    if (!event_is_down(event))
	return;

    /* check to see if in help_mode */

    if (help_mode || query_mode) {
	play_back_proc(play_panel, event);
	return;
    }

    /* check to see if just confirming */

    if (selected[1].filled) {
	play_back_proc(play_panel, event);
	return;
    }

    /* translate item to top level available */

    if ((event_id(event) == MS_LEFT) &&
	((ROW2 - B_TILE_SHADOW) <= event->ie_locy) &&
	(event->ie_locy <= (ROW2 - B_TILE_SHADOW + (6 * W_BASE_TILE))) &&
	((COL5 - S_TILE_SHADOW) <= event->ie_locx) &&
	(event->ie_locx <= (COL5 - S_TILE_SHADOW + (6 * H_BASE_TILE))) ) { /* in overlap area, check for stacks */

	for(i = 143; i > 86 ; i--) { /* check from top to bottom */

	    x = (int) panel_get(tile[i], PANEL_ITEM_X);
	    y = (int) panel_get(tile[i], PANEL_ITEM_Y);

	    if ((x <= event->ie_locx) &&
		(event->ie_locx <= x + W_BASE_TILE) &&
		(y <= event->ie_locy) &&
		(event->ie_locy <= y + H_BASE_TILE)) { /* right spot */

		if ( !(board[i]->removed) ) {

		    item = tile[i]; /* got it */
		    break;

		} else if (i != 143) {   /* look on next layer down */
					 /* take first covered tile and add 1 for loop */
		     i = board[i]->covered[0] + 1;
		}

	    } /* wrong location. try again */

	} /* next loop */

    }

    /* get data from item selected */

    data = (Tiles *) panel_get(item, PANEL_CLIENT_DATA);
    value = data->value;

    switch(event_id(event)) {

	case MS_LEFT: 
	/* Left button down begin selection */
	if ( data->top_free && (data->left_free || data->right_free)) {

	    if (!(selected[0].filled)) {

		/* fill first selection if empty */
		selected[0].item = item;
		selected[0].event.ie_locx = event->ie_locx;
		selected[0].event.ie_locy = event->ie_locy;
		selected[0].event.ie_time.tv_sec = event->ie_time.tv_sec;
		selected[0].event.ie_time.tv_usec = event->ie_time.tv_usec;

		selected[0].filled = TRUE;
		panel_begin_preview(selected[0].item, selected[0].event);

	    } else {

		if (item == selected[0].item) { /* deselect item */

		    selected[0].filled = FALSE;
		    panel_cancel_preview(selected[0].item, selected[0].event);

		} else {
		    data = (Tiles *) panel_get(selected[0].item, PANEL_CLIENT_DATA);
		    if ( value == data->value) {
			/* fill second and show confirm message */

			selected[1].item = item;
			selected[1].event.ie_locx = event->ie_locx;
			selected[1].event.ie_locy = event->ie_locy;
			selected[1].event.ie_time.tv_sec = event->ie_time.tv_sec;
			selected[1].event.ie_time.tv_usec = event->ie_time.tv_usec;

			selected[1].filled = TRUE;
			panel_begin_preview(selected[1].item, selected[1].event);

			panel_msg("Please confirm. [Y] [] [N]", &confirm);

		    } else { /* beep at them */

			window_bell(main_frame);

		    }
		}
	    }

	} else { /* beep at them */

	    window_bell(main_frame);

	}
	break;

	case MS_MIDDLE:
	if (selected[0].filled) { /* request for help */

	    help_proc();

	}
	break;

	/* and all else shall pass */
    }
}

void quit_proc()
{
    window_destroy(main_frame);
}

void new_proc()
{

    seed = random() % 20011;
    build_image(FALSE);
    place_tiles(FALSE);
}

void again_proc()
{
    build_image(TRUE);
    place_tiles(FALSE);
}

void undo_proc()

{
    if(undo_count < 0)
	window_bell(main_frame);
    else
	remove_tiles(FALSE);
}

void board_num_proc()
{
    sscanf((char *) panel_get(board_num, PANEL_VALUE), "%d", &seed);
    build_image(FALSE);
    place_tiles(FALSE);
}
