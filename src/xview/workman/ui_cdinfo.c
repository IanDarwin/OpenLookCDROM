/*
 * @(#)ui_cdinfo.c	1.8	16 Jun 1995
 *
 * ui_cdinfo.c - handle the user interface elements in the CD Info popup.
 */
static char *ident = "@(#)ui_cdinfo.c	1.8 16 Jun 1995";

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include "workman_ui.h"
#include "struct.h"

void	continued();
void	setup_itimer();
void	init_stats();
void	avoid_track();
void	keep_settings();
void	cd_volume();
void	set_default_volume();
char *	listentry();
char *	trackname();
int *	get_playlist();
void	kill_stats();
void	insert_into_playlist();
void	scoot_stuff();
int	switch_playlists();

extern Panel_item quitbutton;
extern Rect	*track_rect;
extern int	add_height, small_height, basic_spacing;

/*
 * Global object definitions.
 */
extern window1_objects	*Workman_window1;
extern popup1_objects	*Workman_popup1;
extern about_objects	*Workman_about;
extern goodies_objects	*Workman_goodies;
extern plpopup_objects	*Workman_plpopup;

extern int num_names, num_nalloc, my_cdname, my_artist;
extern int cur_track, cur_pos_abs, cur_pos_rel, cur_tracklen, cur_cdlen,
	cur_ntracks, cur_nsections, cur_lasttrack;
extern enum cd_modes cur_cdmode;
extern int cur_frame;
extern char *cur_cdname, *cur_artist;
extern int displayed_track, pop_track, *pop_list, pop_listsize, pl_item,
	pl_listnum;
extern char *empty;
extern int dont_retry, dismiss_button, info_modified;
extern void (*text_event_handler)();
extern unsigned char *speaker_bits[];

/*
 * Stash the current settings away in memory (in preparation for changing
 * tracks, for instance, or before a save.)
 */
void
keep_settings(ip)
	window1_objects	*ip;
{
	popup1_objects	*pu = Workman_popup1;

	stash_cdinfo(xv_get(pu->artist, PANEL_VALUE),
		xv_get(pu->cdname, PANEL_VALUE),
		xv_get(pu->autoplay, PANEL_VALUE),
		xv_get(pu->playmode, PANEL_VALUE), NULL);
	if (pop_track > 0)
		stash_trkinfo(pop_track, xv_get(pu->trackname, PANEL_VALUE),
			xv_get(pu->trackoptions, PANEL_VALUE) & 1,
			xv_get(pu->trackoptions, PANEL_VALUE) & 2);
}

/*
 * Callback function for Apply button.
 */
void
save_config(item, event)
	Panel_item	item;
	Event		*event;
{
	keep_settings(Workman_window1);
	save();
	info_modified = 0;
}

/*
 * Notify callback function for `delete'.
 */
void
delete_from_playlist(item, event)
	Panel_item	item;
	Event		*event;
{
	plpopup_objects *ip = Workman_plpopup;
	int i;
	
	info_modified = 1;

	if (pl_item >= 0)
	{
		xv_set(ip->playlist, PANEL_LIST_SELECT, pl_item, FALSE,
			PANEL_LIST_DELETE, pl_item, NULL);
		for (i = pl_item; i < pop_listsize; i++)
			pop_list[i] = pop_list[i + 1];
		if (--pop_listsize)
		{
			if (pl_item == pop_listsize)
				pl_item--;
			xv_set(ip->playlist, PANEL_LIST_SELECT, pl_item,
				TRUE, NULL);
			cd->lists[pl_listnum].list = pop_list;
		}
		else
		{
			pl_item = -1;
			xv_set(ip->delete, PANEL_INACTIVE, TRUE, NULL);
			free(pop_list);
			pop_list = NULL;
			cd->lists[pl_listnum].list = pop_list;
		}
	}
}

/*
 * Notify callback function for `artist'.
 */
Panel_setting
update_title(item, event)
	Panel_item	item;
	Event		*event;
{
	new_trackname_display(cur_track > 0 ? trackname(cur_track - 1)[0] ?
		trackname(cur_track - 1) : NULL : "",
		cur_track);

	if (event == NULL)
		return ((Panel_setting)NULL);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `tracklist'.
 */
int
update_trackname(item, string, client_data, op, event, row)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
	int		row;
{
	popup1_objects *ip = Workman_popup1;
	int	options;
	char	*name;

	switch(op) {
	case PANEL_LIST_OP_DESELECT:
	case PANEL_LIST_OP_VALIDATE:
		if (! pop_track)	/* workaround for bug 1090204 */
			break;
		options = xv_get(ip->trackoptions, PANEL_VALUE);
		name = (char *) xv_get(ip->trackname, PANEL_VALUE);
		stash_trkinfo(row + 1, name, options & 1, (options & 2) >> 1);
		xv_set(ip->tracklist, PANEL_LIST_STRING, row, listentry(row),
			NULL);
		if (cur_track - 1 == row)
			new_trackname_display(name, cur_track);
		if (op == PANEL_LIST_OP_DESELECT)
		{
			xv_set(ip->trackname, PANEL_VALUE, empty, NULL);
			pop_track = 0;
		}
		break;

	case PANEL_LIST_OP_SELECT:
		xv_set(ip->trackname, PANEL_VALUE, trackname(row), NULL);
		xv_set(ip->trackoptions, PANEL_VALUE, (get_avoid(row) ? 2 : 0) |
			(get_contd(row) ? 1 : 0), NULL);
		pop_track = row + 1;
		if (xv_get(ip->whichvolume, PANEL_VALUE))
		{
			xv_set(ip->defaultvolume, PANEL_VALUE,
				get_default_volume(row + 1), NULL);
			set_default_volume(ip->defaultvolume,
				get_default_volume(row + 1), NULL);
		}
		break;

	case PANEL_LIST_OP_DELETE:
		break;
	}

	return XV_OK;
}

/*
 * Notify callback function for `trackname'.
 */
Panel_setting
name_entered(item, event)
	Panel_item	item;
	Event		*event;
{
	popup1_objects *ip = Workman_popup1;
	int	next_track = 0;
	Panel_setting	retval;
	
	switch (event_action(event))
	{
		case '\n':
		case '\r':
		case '\033':
			retval = PANEL_NONE;
			next_track = 1;
			break;
		case '\t':
			retval = PANEL_NEXT;
			break;
		default:
			retval = panel_text_notify(item, event);
	}

/* If a track was selected, save the current settings and go to the next. */
	if (pop_track)
	{
		if (next_track)
		{
			next_track = pop_track;
			if (next_track == cur_ntracks)
				next_track = 0;
			xv_set(ip->tracklist, PANEL_LIST_SELECT, pop_track - 1,
				FALSE, NULL);
			update_trackname(ip->tracklist, listentry(pop_track -
				1), NULL, PANEL_LIST_OP_DESELECT, event,
				pop_track - 1);
			xv_set(ip->tracklist, PANEL_LIST_SELECT, next_track,
				TRUE, NULL);
			update_trackname(ip->tracklist, listentry(next_track),
				NULL, PANEL_LIST_OP_SELECT, event, next_track);
		}
		else
			update_trackname(ip->tracklist, listentry(pop_track -
				1), NULL, PANEL_LIST_OP_VALIDATE, event,
				pop_track - 1);
	}

	return (retval);
}

/*
 * Notify callback function for `defaultvolume'.
 */
void
set_default_volume(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	popup1_objects *ip = Workman_popup1;
	static int	old_image = -1;
	Xv_opaque	old_serverimage;
	int		max = xv_get(item, PANEL_MAX_VALUE);
	int		new_image;
	static int	old_volume = -1;

	/*
	 * Make the real volume track the default volume while the user is
	 * sliding the default slider.
	 */
	if (event != NULL)
	{
		info_modified = 1;

		if (event_is_up(event))
		{
			if (old_volume != -1)
				xv_set(Workman_window1->volume, PANEL_VALUE,
					old_volume, NULL);
			old_volume = -1;
		}
		else
		{
			if (old_volume == -1)
				old_volume = xv_get(Workman_window1->volume,
					PANEL_VALUE);
			if (value)
				xv_set(Workman_window1->volume, PANEL_VALUE,
					value - 1, NULL);
		}
		figure_volume(Workman_window1);
	}

	/* we want this to look sort of logarithmic */
	if (value)
	{
		default_volume(xv_get(ip->whichvolume, PANEL_VALUE) * pop_track,
			value);
		new_image = value / (max / 8);
		value = (max * max) - ((max - value) * (max - value));
	}
	else
	{
		new_image = -1;
		default_volume(xv_get(ip->whichvolume, PANEL_VALUE) * pop_track,
			0);
	}

	/* maybe show a new icon... */
	if (new_image > 7)
		new_image = 7;
	if (old_image > -1)
		old_serverimage = xv_get(ip->defaultspeaker, PANEL_LABEL_IMAGE);
	if (new_image != old_image && new_image > -1)
	{
		xv_set(ip->defaultspeaker, PANEL_LABEL_IMAGE,
			xv_create(XV_NULL, SERVER_IMAGE, SERVER_IMAGE_DEPTH, 1,
			XV_WIDTH, 16, XV_HEIGHT, 15, SERVER_IMAGE_X_BITS,
			speaker_bits[new_image], NULL), NULL);
		if (old_image > -1)
			xv_destroy(old_serverimage);
		else
		{
			xv_set(ip->nonemsg, XV_SHOW, FALSE, NULL);
			xv_set(ip->defaultspeaker, XV_SHOW, TRUE, NULL);
		}
		old_image = new_image;
	}
	if (new_image != old_image && new_image == -1)
	{
		xv_destroy(old_serverimage);
		xv_set(ip->defaultspeaker, XV_SHOW, FALSE, NULL);
		xv_set(ip->nonemsg, XV_SHOW, TRUE, NULL);
		old_image = -1;
	}
}

/*
 * Notify callback function for `whichvolume'.
 */
void
set_which_volume(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	popup1_objects *ip = Workman_popup1;
	int	vol;

	if (value == 1 && ! pop_track)
	{
		xv_set(item, PANEL_VALUE, 0, NULL);
		value = 0;
	}

	xv_set(item, PANEL_DEFAULT_VALUE, (value + 1) % 2, NULL);
	xv_set(ip->defaultvolume, PANEL_VALUE, vol = get_default_volume(value ?
		pop_track : 0), NULL);
	set_default_volume(ip->defaultvolume, vol, NULL);
}

/*
 * Event notify procedure for text fields, so the PANEL_NOTIFY_PROC gets
 * called when the user clicks on another field.
 */
void
text_event_p(item, event)
	Panel_item	item;
	Event		*event;
{
	Panel_setting   (*fp)();
	int               e;
	char             *pns;
	Panel_setting     level;
	static Panel_item last_item = (Panel_item)NULL;

	e = event_id(event);

/* call last_item's PANEL_NOTIFY_PROC if user mouse clicks off it
   but don't do this if PANEL_NOTIFY_PROC already gets triggered via kbd */
	if ( (item != last_item) && (event_action(event) == ACTION_SELECT) &&
		(last_item != (Panel_item)NULL) )
	{
		level = (Panel_setting) xv_get(last_item, PANEL_NOTIFY_LEVEL);
		switch (level)
		{
		case PANEL_NONE:
			break;
		case PANEL_NON_PRINTABLE:
			break;
		case PANEL_SPECIFIED:
			pns = (char *)xv_get(last_item, PANEL_NOTIFY_STRING);
			if ( strchr( pns, (char)e ) != NULL)
				break;
		case PANEL_ALL:
		default:
			fp = (Panel_setting (*)())
				xv_get(last_item, PANEL_NOTIFY_PROC);
			(*fp)(last_item, event);
			break;
		}
	}

	/* save last item in static var */
	last_item = item;

	(text_event_handler)(item, event);
}

/*
 * Notify callback function for `buttonpl'.
 */
void
popup1_buttonpl_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	popup1_objects *ip = Workman_popup1;
	
	if (dismiss_button && item == ip->buttonpl ||
		xv_get(Workman_plpopup->plpopup, FRAME_CMD_PUSHPIN_IN) == FALSE)
	{
		xv_set(Workman_plpopup->plpopup, FRAME_CMD_PUSHPIN_IN, TRUE,
			NULL);
		xv_set(Workman_plpopup->plpopup, XV_SHOW, TRUE, NULL);
	}
	else
		xv_set(Workman_plpopup->plpopup, FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_SHOW, FALSE, NULL);
	xv_set(Workman_plpopup->plpopup, XV_KEY_DATA, FRAME_CMD_PUSHPIN_IN,
		FALSE, NULL);
}

/*
 * Notify callback function for `button8'.
 */
void
cdinfo_reset(item, event)
	Panel_item	item;
	Event		*event;
{
	enum cd_modes	old_cdmode = cur_cdmode;
	
	kill_stats(Workman_window1);
	wipe_cdinfo();
	load();
	init_stats(Workman_window1);
	show_stats(Workman_window1);
	if (old_cdmode == PAUSED)
	{
		cur_cdmode = PAUSED;
		xv_set(Workman_window1->mode, PANEL_VALUE, PAUSED, NULL);
	}
	info_modified = 0;
}
