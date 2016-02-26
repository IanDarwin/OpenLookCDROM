/*
 * @(#)display.c	1.65 1/11/94
 *
 * display.c - update the status indicators and other display elements.
 */
static char *ident = "@(#)display.c	1.65 1/11/94";

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

extern int num_names, num_nalloc;
extern int cur_track, cur_pos_abs, cur_pos_rel, cur_tracklen, cur_cdlen,
	cur_ntracks, cur_nsections, cur_lasttrack;
extern enum cd_modes cur_cdmode;
extern int cur_frame;
extern char *cur_cdname, *cur_artist;
extern int displayed_track, pop_track, *pop_list, pop_listsize, pl_item,
	pl_listnum;
extern char *empty;
extern int min_lines;
extern int mark_a, mark_b;
extern int manual_volume;
extern int window_is_open;
extern int dont_retry;
extern int dismiss_button;
extern int show_titles;

/*
 * Change the icon label. 
 */
void
icon_label(string)
	char	*string;
{
	Xv_opaque	icon;

	icon = xv_get(Workman_window1->window1, FRAME_ICON);
	xv_set(icon, ICON_LABEL, string, NULL);
	xv_set(Workman_window1->window1, FRAME_ICON, icon, NULL);
}

/*
 * Center the track list.
 */
void
center_tracks()
{
	Rect *tracks;
	int x, cwidth;
	
	cwidth = xv_get(Workman_window1->controls1, XV_WIDTH);

	tracks = (Rect *) xv_get(Workman_window1->tracks, PANEL_ITEM_RECT);
	x = (cwidth - tracks->r_width) / 2;
	if (x < 0)
		x = 0;
	xv_set(Workman_window1->tracks, PANEL_ITEM_X, x, NULL);
}

static Xv_opaque *tracknames = NULL;

/*
 * Center all the read-only titles.
 */
void
center_titles()
{
	Rect *message;
	int x, cwidth, i;
	
	cwidth = xv_get(Workman_window1->controls1, XV_WIDTH);

	for (i = 0; num_nalloc && (i < num_nalloc); i++)
		if (tracknames[i] != XV_NULL)
		{
			message = (Rect *) xv_get(tracknames[i],
							PANEL_ITEM_RECT);
			x = (cwidth - message->r_width) / 2;
			if (x < 0)
				x = 0;
			xv_set(tracknames[i], PANEL_ITEM_X, x, NULL);
		}
}

/*
 * Find a polite place to split up a title line.  Search from right
 * to left for each of these:
 *
 * 1. One of ",;-):]}" unless next char is the same (split point is next char)
 * 2. One of "([{" unless prev char is the same
 * 3. " "
 *
 * If none are found, split at the last character.
 */
int
find_polite_split(name, right)
	char	*name;
	int	right;
{
	int	i;

	for (i = right; i >= right/2; i--)
		if ((name[i] == '-' || name[i] == ')' || name[i] == ':' ||
				name[i] == ']' || name[i] == '}' ||
				name[i] == ',' || name[i] == ';') &&
				name[i] != name[i + 1])
			return (i + 1);
	
	for (i = right; i > right/3; i--)
		if ((name[i] == '(' || name[i] == '[' || name[i] == '{') &&
				name[i] != name[i - 1])
			return (i);
	
	for (i = right; i >= 0; i--)
		if (name[i] == ' ')
			return (i);
	
	return (right);
}

/*
 * Put a line of track title onscreen.  If the line is too big to fit,
 * split it up, attempting to do the split in a polite place if possible.
 */
void
set_trackname(y, tnum, name, bold)
	int	*y, *tnum, bold;
	char	*name;
{
	char	c;
	int	i, jump;
	int	maxwidth = (int) xv_get(Workman_window1->controls1, XV_WIDTH) -
			basic_spacing * 2;

	if (name == NULL)
		return;
	
	if (*tnum == num_nalloc)
	{
		if (tracknames == NULL)
			tracknames = (Xv_opaque *)malloc(++num_nalloc *
				sizeof(Xv_opaque));
		else
			tracknames = (Xv_opaque *)realloc(tracknames,
				++num_nalloc * sizeof(Xv_opaque));
		if (tracknames == NULL)
		{
			perror("set_trackname: tracknames");
			exit(1);
		}

		tracknames[*tnum] = xv_create(Workman_window1->controls1,
			PANEL_MESSAGE, PANEL_ITEM_X, 0,
			XV_SHOW, FALSE, XV_HELP_DATA, "workman:tracknames",
			NULL);
		if (tracknames[*tnum] == XV_NULL)
		{
			perror("set_trackname: xv_create");
			exit(1);
		}
	}

	xv_set(tracknames[*tnum], PANEL_LABEL_STRING, name, PANEL_LABEL_BOLD,
		bold, PANEL_ITEM_Y, *y, NULL);
	
	/*
	 * Wrap the name if it's too big.
	 */
	if ((int) xv_get(tracknames[*tnum], XV_WIDTH) > maxwidth)
	{
		i = (int) strlen(name) / 2;
		jump = i / 2 + 1;

		while (jump > 0)
		{
			c = name[i];
			name[i] = '\0';
			xv_set(tracknames[*tnum], PANEL_LABEL_STRING, name,
				NULL);
			if ((int) xv_get(tracknames[*tnum], XV_WIDTH) <
								maxwidth)
			{
				name[i] = c;
				i += jump;
			}
			else
			{
				name[i] = c;
				i -= jump;
			}

			jump /= 2;
		}

		i = find_polite_split(name, i - 1);
		c = name[i];
		name[i] = '\0';
		xv_set(tracknames[*tnum], PANEL_LABEL_STRING, name, NULL);
		name[i] = c;

		*y += (int) xv_get(Workman_about->message3, XV_HEIGHT) +
			basic_spacing / 2;
		(*tnum)++;

		set_trackname(y, tnum, name + i + (c == ' '), bold);
	}
	else
	{
		*y += (int) xv_get(Workman_about->message3, XV_HEIGHT) +
			basic_spacing / 2;
		(*tnum)++;
	}
}

/*
 * Preprocess a track name.  Split display lines up into 3 arrays of char*s,
 * one for the CD title, one for the artist, and one for the track title.
 * The program returns the three arrays and the number of elements in each.
 * "spec" should be 0 to disallow special characters (+/@) and only stick
 * entries in the title array.
 *
 * Note: the caller should free each array[0] and array when it's done.
 *
 * This routine is a mess.  It does way, way, way too much malloc()ing.
 * And only fools tread willingly in the realm of the hideous triple pointer.
 */
void
split_trackname(name, artist, nartist, cdname, ncdname, title, ntitle, spec)
	char	*name, ***artist, ***cdname, ***title;
	int	*nartist, *ncdname, *ntitle, spec;
{
	char	*c, *tmpbuf = NULL, ***list;
	int	*nlist;

	if (spec)
	{
		*nartist = 0;
		*ncdname = 0;
		*cdname = NULL;
		*artist = NULL;
	}
	*ntitle = 0;
	*title = NULL;

	/* Copy the name into a temporary buffer so we can mutilate it */
	strmcpy(&tmpbuf, name);
	name = tmpbuf;

	while (*name != '\0')
	{
		if (*name == '+' && spec)
		{
			list = cdname;
			nlist = ncdname;
			name++;
		}
		else if (*name == '@' && spec)
		{
			list = artist;
			nlist = nartist;
			name++;
		}
		else
		{
			list = title;
			nlist = ntitle;
		}

		if (*list == NULL)
		{
			/*
			 * There can't be more elements than strlen(name)/2+1,
			 * since it takes two characters just to delimit an
			 * element.  So we allocate that many elements to
			 * begin with.
			 */
			*list = (char **) malloc(sizeof (char *) *
				((int) strlen(name) / 2 + 1));

			/*
			 * Each array's first element is really a pointer
			 * to a bunch of strings, one after the other,
			 * comprising that array's elements; the remaining
			 * elements point into that buffer.  Since the
			 * combined length of those strings can never
			 * exceed the remaining length of "name", it's
			 * safe to allocate that many characters at the
			 * start and not worry about growing the buffer
			 * later on.  This also makes freeing all the
			 * buffers easier.
			 */
			if (*list != NULL)
				**list = (char *) malloc(strlen(name) + 1);
		}
		else
		{
			/*
			 * Add another element to an array.  The new element
			 * will point just past the previous one's terminating
			 * '\0' (see above).
			 */
			(*list)[*nlist] = (*list)[*nlist - 1] +
					strlen((*list)[*nlist - 1]) + 1;
		}
			
		if (*list == NULL || **list == NULL)
		{
			perror("Couldn't expand list");
			exit(1);
		}

		/* Search for a "//". */
		for (c = name; c[0] != '\0'; c++)
			if (c[0] == '/' && c[1] == '/')
				break;

		/* No "//"?  End of the line! */
		if (c[0] == '\0')
		{
			strcpy((*list)[(*nlist)++], name);
			break;
		}

		c[0] = '\0';
		strcpy((*list)[(*nlist)++], name);
		name = &c[2];
	}

	free(tmpbuf);
}

/*
 * Search backwards for a particular line of a title/CD name/artist.
 * Pass '@', '+', or '-' to determine which.
 */
char *
backtitle(track, c, line)
	int	track, line;
	char	c;
{
	int	i, nartist = 0, ncdname = 0, ntrack = 0, *count;
	char	*p = NULL, **artist = NULL, **cdname = NULL, **title, ***list;
	static char *newname = NULL;

	if (newname != NULL)
		free(newname);
	newname = NULL;

	if (c == '+')
	{
		list = &cdname;
		count = &ncdname;
	}
	else if (c == '@')
	{
		list = &artist;
		count = &ntrack;
	}
	else
	{
		list = &title;
		count = &ntrack;
	}

	for (i = track - 2; i > -1; i--)
	{
		split_trackname(trackname(i), &artist, &nartist, &cdname,
			&ncdname, &title, &ntrack, 1);
		
		if (*count <= line)
		{
			p = "";
			break;
		}

		if (*list && (*list)[line][0])
		{
			p = (*list)[line];
			break;
		}

		if (nartist)
		{
			free(artist[0]);
			free(artist);
		}
		if (ncdname)
		{
			free(cdname[0]);
			free(cdname);
		}
		if (ntrack)
		{
			free(title[0]);
			free(title);
		}
	}

	if (p == NULL)
		p = "";
	
	strmcpy(&newname, p);

	if (nartist)
	{
		free(artist[0]);
		free(artist);
	}
	if (ncdname)
	{
		free(cdname[0]);
		free(cdname);
	}
	if (ntrack)
	{
		free(title[0]);
		free(title);
	}

	return (newname);
}

/*
 * Update the track name, using "//" as a line delimiter.  Grow the number
 * of tracks as necessary; never actually get rid of a message object, but
 * rather hide all the unused ones.
 */
void
new_trackname_display(title, track)
	char	*title;
	int	track;
{
	int	tnum = 0, i, new_height, y, nartist, ncdname, ntitle;
	char	**artist, **cdname, **trackname, *scratch;
	static int old_height = 10, max_height = 0;

	if (! show_titles)
		return;

	if (title == NULL)
		title = show_titles > 0 ? "Unknown track name" : "";

	for (i = 0; i < num_nalloc; i++)
		if (tracknames[i] != XV_NULL)
			xv_set(tracknames[i], XV_SHOW, FALSE, NULL);

	split_trackname(title, &artist, &nartist, &cdname, &ncdname,
		&trackname, &ntitle, 1);

	y = 10;

	if (nartist)
		for (i = 0; i < nartist; i++)
			set_trackname(&y, &tnum, artist[i][0] == '\0' ?
				backtitle(track, '@', i) : artist[i], TRUE);
	else
	{
		scratch = (char *)xv_get(Workman_popup1->artist, PANEL_VALUE);
		if (! scratch[0] && show_titles > 0)
			scratch = "Unknown artist";
		split_trackname(scratch, NULL, NULL, NULL, NULL,
			&artist, &nartist, 0);
		for (i = 0; i < nartist; i++)
			set_trackname(&y, &tnum, artist[i], TRUE);
	}

	if (artist != NULL)
	{
		y += basic_spacing / 2;
		free(artist[0]);
		free(artist);
	}

	if (ncdname)
		for (i = 0; i < ncdname; i++)
			set_trackname(&y, &tnum, cdname[i][0] == '\0' ?
				backtitle(track, '+', i) : cdname[i], TRUE);
	else
	{
		scratch = (char *)xv_get(Workman_popup1->cdname, PANEL_VALUE);
		if (! scratch[0] && show_titles > 0)
			scratch = "Unknown CD name";
		split_trackname(scratch, NULL, NULL, NULL, NULL,
			&cdname, &ncdname, 0);
		for (i = 0; i < ncdname; i++)
			set_trackname(&y, &tnum, cdname[i], TRUE);
	}

	if (cdname != NULL)
	{
		y += basic_spacing / 2;
		free(cdname[0]);
		free(cdname);
	}

	if (ntitle)
	{
		for (i = 0; i < ntitle; i++)
			set_trackname(&y, &tnum,
				trackname[i][0] == '\0' ?
				backtitle(track, '-', i) : trackname[i], FALSE);
		y += basic_spacing / 2;
		free(trackname[0]);
		free(trackname);
	}

	/*
	 * Now figure out how much to grow or shrink the display.  Always
	 * grow it if necessary; only shrink it if min_lines (-l option)
	 * is >= 0 and there were more than that many title lines before.
	 */
	tnum -= 2;	/* cd name and artist don't count at this point */

	if (min_lines < 0)
		if (tnum < num_names)
			new_height = old_height;
		else
		{
			new_height = y;
			num_names = tnum;
		}
	else
		if (tnum < min_lines)
		{
			if (show_titles > 0 || min_lines || tnum != -2)
				new_height = (min_lines + 2) *
					((int)xv_get(Workman_about->message3,
					XV_HEIGHT) + basic_spacing / 2) +
					(basic_spacing / 2) * 3 + 10;
			else
				new_height = 10;
			num_names = min_lines;
		}
		else
		{
			new_height = y;
			num_names = tnum;
		}

	scoot_stuff(new_height - old_height, 1);

	center_titles();

	for (i = 0; i < tnum + 2; i++)
		if (tracknames[i] != XV_NULL)	/* which it had better be */
			xv_set(tracknames[i], XV_SHOW, TRUE, NULL);
	
	old_height = new_height;
	if (new_height > max_height)
		max_height = new_height;
}

/* A bunch of numbers for button labels. */
char **numbered_buttons = NULL;

/*
 * Update the per-track information.  This is called once every time we
 * see we're on a new track.
 */
void
new_track(ip)
window1_objects *ip;
{
	char	scratch[20];

	if (cur_track <= 0)
		return;
	new_trackname_display(trackname(cur_track - 1)[0] ?
		trackname(cur_track - 1) : NULL, cur_track);
	xv_set(ip->songpos, PANEL_MIN_VALUE, 0, PANEL_MAX_VALUE, 
		tracklen(cur_track - 1), PANEL_INACTIVE, FALSE, PANEL_VALUE,
		cur_frame < cd->trk[cur_track - 1].start ? 0 : cur_pos_rel,
		NULL);
	sprintf(scratch, "%2d:%02d", tracklen(cur_track - 1) / 60,
		tracklen(cur_track - 1) % 60);
	xv_set(ip->tracklen, PANEL_LABEL_STRING, scratch, NULL);
	xv_set(ip->cdgauge, PANEL_MIN_VALUE, 0, PANEL_MAX_VALUE, get_runtime(),
		PANEL_INACTIVE, FALSE, NULL);
	sprintf(scratch, "%2d:%02d", get_runtime() / 60, get_runtime() % 60);
	xv_set(ip->cdlen, PANEL_LABEL_STRING, scratch, NULL);
	xv_set(ip->tracks, PANEL_VALUE, cur_track - 1, PANEL_CHOICE_STRING,
		cur_track - 1, numbered_buttons[cur_track - 1], NULL);
	xv_set(ip->cdgauge, PANEL_VALUE, cur_pos_abs, NULL);
	xv_set(Workman_goodies->split, PANEL_INACTIVE,
		cd->trk[cur_track - 1].data, NULL);
	xv_set(Workman_goodies->delete, PANEL_INACTIVE,
		cd->trk[cur_track - 1].section < 2, NULL);
	sprintf(scratch, "Track %d", cd->trk[cur_track - 1].track);
	if (cd->trk[cur_track - 1].section)
		sprintf(scratch + strlen(scratch), ".%d",
			cd->trk[cur_track - 1].section);
	icon_label(scratch);
	figure_volume(ip);
	displayed_track = cur_track;
}

/*
 * Update all the moving status indicators.
 */
void
show_stats(ip)
window1_objects *ip;
{
	static char	trk_time[6], abs_time[6];
	static enum cd_modes	old_cdmode = UNKNOWN;
	int		pos;

/* If we're on a different track than we used to be, update the track info */
	if (displayed_track != cur_track && cur_cdmode != EJECTED)
		new_track(ip);

/* Update the current play mode */
	if (old_cdmode != cur_cdmode)
	{
		old_cdmode = cur_cdmode;
		xv_set(ip->mode, PANEL_VALUE, cur_cdmode, NULL);
	}

	if (! window_is_open)
		return;

/* Update the track timer and slider */
	if (displayed_track == -1)
		cur_tracklen = cur_cdlen;

	if (xv_get(Workman_goodies->timemode_track, PANEL_VALUE) == 0)
		if (cur_track > 0 && cur_frame < cd->trk[cur_track - 1].start)
			(void) sprintf(trk_time, "-%1d:%02d", cur_pos_rel / 60,
				cur_pos_rel % 60);
		else
			(void) sprintf(trk_time, "%02d:%02d", cur_pos_rel / 60,
				cur_pos_rel % 60);
	else
	{
		pos = tracklen(cur_track - 1) - cur_pos_rel;
		if (pos < 0)	/* transitioning between tracks... */
			pos = 0;
		(void) sprintf(trk_time, "%02d:%02d", pos / 60, pos % 60);
	}

	if (xv_get(Workman_goodies->timemode_cd, PANEL_VALUE) == 0)
		(void) sprintf(abs_time, "%02d:%02d", cur_pos_abs / 60,
			cur_pos_abs % 60);
	else
		(void) sprintf(abs_time, "%02d:%02d", (get_runtime() -
			cur_pos_abs) / 60, (get_runtime() - cur_pos_abs) % 60);

	if (strcmp(trk_time, (char *)xv_get(ip->tracktimer,PANEL_LABEL_STRING)))
		xv_set(ip->tracktimer, PANEL_LABEL_STRING, trk_time, NULL);
	if ((cur_pos_rel % 5) == 0 && xv_get(ip->songpos, PANEL_VALUE) !=
								cur_pos_rel)
		xv_set(ip->songpos, PANEL_VALUE, (cur_track > 0 && cur_frame <
			cd->trk[cur_track - 1].start) ? 0 : cur_pos_rel, NULL);

/* Update the CD gauge */
	if (strcmp(abs_time, (char *)xv_get(ip->cdtimer, PANEL_LABEL_STRING)))
		xv_set(ip->cdtimer, PANEL_LABEL_STRING, abs_time, NULL);
	if ((cur_pos_rel % 10) == 0 && xv_get(ip->cdgauge, PANEL_VALUE) !=
								cur_pos_abs)
		xv_set(ip->cdgauge, PANEL_VALUE, cur_pos_abs, NULL);

/* Are we past the beginning of the a-b repeat block? */
	if (mark_a && cur_frame > mark_a)
	{
		if (xv_get(Workman_goodies->b, PANEL_INACTIVE) == TRUE)
			xv_set(Workman_goodies->b, PANEL_INACTIVE, FALSE, NULL);
	}
	else
		if (xv_get(Workman_goodies->b, PANEL_INACTIVE) == FALSE)
		{
			xv_set(Workman_goodies->b, PANEL_INACTIVE, TRUE, NULL);
			if (! xv_get(Workman_goodies->abrepeat, PANEL_VALUE))
				xv_set(Workman_goodies->blabel, PANEL_INACTIVE,
					TRUE, NULL);
		}

	if (xv_get(Workman_goodies->a, PANEL_INACTIVE) ==
	    (cur_cdmode == PLAYING || cur_cdmode == PAUSED))
	{
		xv_set(Workman_goodies->a, PANEL_INACTIVE,
		       cur_cdmode != PLAYING && cur_cdmode != PAUSED, NULL);
		if (cur_cdmode != PLAYING && cur_cdmode != PAUSED)
			xv_set(Workman_goodies->b, PANEL_INACTIVE, TRUE, NULL);
	}
}

/*
 * Populate the numbered buttons.  This has to be done in a fairly stupid
 * manner since XView doesn't copy choice strings to its own buffers.
 */
void
fill_buttons()
{
	Xv_opaque t = Workman_window1->tracks;
	int	i, oldheight;
	char	temp[20];

	xv_set(t, XV_SHOW, FALSE, NULL);

	if (numbered_buttons != NULL)
	{
		for (i = 0; numbered_buttons[i]; i++)
			free(numbered_buttons[i]);
		free(numbered_buttons);
	}

	numbered_buttons = (char **) calloc(cur_ntracks + 1, sizeof(char **));
	if (numbered_buttons == NULL)
	{
		perror("fill_buttons");
		exit(1);
	}

	track_rect = (Rect *) xv_get(t, PANEL_ITEM_RECT);
	oldheight = track_rect->r_height;

	xv_set(t, PANEL_CHOICE_STRINGS, " ", NULL, NULL);

	for (i = 0; i < cur_ntracks; i++)
	{
		if (cd->trk[i].section)
			sprintf(temp, "%2d.%d", cd->trk[i].track,
				cd->trk[i].section);
		else
			sprintf(temp, "%*d", cur_nsections ? 3 : 2,
				cd->trk[i].track);
		
		numbered_buttons[i] = (char *) malloc(strlen(temp) + 1);
		if (numbered_buttons[i] == NULL)
		{
			perror("fill_buttons");
			exit(1);
		}
		strcpy(numbered_buttons[i], temp);

		xv_set(t, PANEL_CHOICE_STRING, i, numbered_buttons[i], NULL);
	}

	numbered_buttons[i] = NULL;

	xv_set(t, PANEL_CHOICE_NROWS, 1, NULL);
	i = 1;
	while (xv_get(t, XV_WIDTH) + 5 >= xv_get(Workman_window1->controls1,
								XV_WIDTH))
		xv_set(t, PANEL_CHOICE_NROWS, ++i, NULL);

	center_tracks();

	track_rect = (Rect *) xv_get(t, PANEL_ITEM_RECT);
	add_height = track_rect->r_height - small_height;
	scoot_stuff(track_rect->r_height - oldheight, 0);

	xv_set(t, XV_SHOW, TRUE, NULL);
}

/*
 * Set the CD length gauge to the right length for the whole CD.
 */
void
reset_cdlen(ip)
window1_objects *ip;
{
	char	scratch[16];

	xv_set(ip->cdgauge, PANEL_MIN_VALUE, 0, PANEL_MAX_VALUE, get_runtime(),
		PANEL_INACTIVE, FALSE, NULL);
	sprintf(scratch, "%2d:%02d", get_runtime() / 60, get_runtime() % 60);
	xv_set(ip->cdlen, PANEL_LABEL_STRING, scratch, NULL);
}

/*
 * Fill up the track scrolling list and the track number menu.
 */
void
fill_lists()
{
	popup1_objects	*pu = Workman_popup1;
	plpopup_objects	*pl = Workman_plpopup;
	int i;

	xv_set(pu->tracklist, XV_SHOW, FALSE, NULL);
	for (i = 0; i < cur_ntracks; i++)
	{
		xv_set(xv_get(pl->playlist, PANEL_ITEM_MENU), MENU_APPEND_ITEM,
			xv_create(XV_NULL, MENUITEM, MENU_NOTIFY_PROC,
			insert_into_playlist, MENU_RELEASE,
			MENU_STRING, numbered_buttons[i], XV_KEY_DATA, 1234, i +
			1, NULL), NULL);
		xv_set(pu->tracklist, PANEL_LIST_INSERT, i, NULL);
		xv_set(pu->tracklist, PANEL_LIST_STRING, i, listentry(i), NULL);
	}

	xv_set(pu->tracklist, XV_SHOW, TRUE, NULL);

	xv_set(xv_get(pl->playlist, PANEL_ITEM_MENU), MENU_NCOLS,
		xv_get(Workman_window1->tracks, PANEL_CHOICE_NROWS), NULL);
}

/*
 * Initialize all the status indicators (a new CD has been inserted.)
 * This only initializes the static values; the rest are done by show_stats().
 */
void
init_stats(ip)
window1_objects *ip;
{
	popup1_objects	*pu = Workman_popup1;
	plpopup_objects	*pl = Workman_plpopup;
	int i;
	Panel_setting update_title();

	reset_cdlen(ip);
	xv_set(ip->mode, PANEL_VALUE, 4, NULL);
	icon_label("Stop");
	xv_set(pu->artist, PANEL_VALUE, cur_artist, PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(pu->cdname, PANEL_VALUE, cur_cdname, PANEL_INACTIVE, FALSE,
		NULL);
	for (i = 0; i < num_nalloc; i++)
		if (tracknames[i] != XV_NULL)
			xv_set(tracknames[i], PANEL_INACTIVE, FALSE, NULL);
	xv_set(ip->volume, PANEL_INACTIVE, FALSE, NULL);
	xv_set(ip->songpos, PANEL_INACTIVE, FALSE, NULL);
	update_title(pu->cdname, NULL);
	xv_set(pu->tracklist, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->trackname, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->trackoptions, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->autoplay, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pl->playlist, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pl->playlists, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pl->listname, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pl->button7, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->whichvolume, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->defaultvolume, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->defaultspeaker, PANEL_INACTIVE, FALSE, NULL);
	xv_set(Workman_goodies->balance, PANEL_INACTIVE, FALSE, NULL);
	xv_set(Workman_goodies->indexscan, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->playmode, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->button1, PANEL_INACTIVE, FALSE, NULL);
	xv_set(pu->button8, PANEL_INACTIVE, FALSE, NULL);
	fill_buttons(ip);
	fill_lists();

	xv_set(ip->tracks, PANEL_CHOOSE_ONE, TRUE, PANEL_CHOOSE_NONE, TRUE,
		PANEL_VALUE, -1, NULL);
	xv_set(pu->whichvolume, PANEL_VALUE, 0, PANEL_DEFAULT_VALUE, 1, NULL);
	xv_set(pu->defaultvolume, PANEL_VALUE, get_default_volume(0), NULL);
	set_default_volume(pu->defaultvolume, get_default_volume(0), NULL);
	xv_set(pu->tracklist, PANEL_LIST_SELECT, 0, TRUE, NULL);
	update_trackname(pu->trackname, NULL, trackname(0),
		PANEL_LIST_OP_SELECT, NULL, 0);

	if (cd->lists != NULL)
	{
		for (i = 0; cd->lists[i].name != NULL; i++)
		{
			xv_set(ip->shuffle, PANEL_CHOICE_STRING, i + 2,
				cd->lists[i].name, NULL);
			xv_set(pl->playlists, PANEL_LIST_INSERT, i,
				PANEL_LIST_STRING, i, cd->lists[i].name, NULL);
		}

		(void) switch_playlists(pl->playlists, NULL, NULL,
			PANEL_LIST_OP_SELECT, NULL, 0);
	}
	else
		pop_listsize = 0;

	xv_set(ip->shuffle, PANEL_VALUE, get_playmode(), PANEL_DEFAULT_VALUE,
		(get_playmode() + 1) % (cd->lists == NULL ? 2 : i + 2), NULL);
	next_playmode_default(ip->shuffle, get_playmode(), NULL);

	xv_set(pu->autoplay, PANEL_VALUE, get_autoplay(), NULL);
	xv_set(pu->playmode, PANEL_VALUE, get_playmode(), NULL);

	displayed_track = -1;
	cur_cdmode = STOPPED;
}

/*
 * Clean out the lists/menus that contain track numbers.
 */
void
cleanout_lists()
{
	popup1_objects	*pu = Workman_popup1;
	plpopup_objects	*pl = Workman_plpopup;
	Menu			m;
	int i;

	xv_set(pu->tracklist, XV_SHOW, FALSE, NULL);
	i = xv_get(pu->tracklist, PANEL_LIST_NROWS);
	m = (Menu) xv_get(pl->playlist, PANEL_ITEM_MENU);
	while (--i > -1)
	{
		xv_set(m, MENU_REMOVE, i + 2, NULL);
		xv_set(pu->tracklist, PANEL_LIST_DELETE, i, NULL);
	}
	xv_set(pu->tracklist, XV_SHOW, TRUE, NULL);
}

/*
 * CD has been ejected.  Remove all the status information.
 */
void
kill_stats(ip)
window1_objects *ip;
{
	popup1_objects	*pu = Workman_popup1;
	plpopup_objects	*pl = Workman_plpopup;
	int		i;

	/* If stats are already killed, don't kill 'em again */
	if (xv_get(ip->songpos, PANEL_INACTIVE) == TRUE)
		return;

	xv_set(ip->songpos, PANEL_INACTIVE, TRUE, PANEL_VALUE, 0, NULL);
	xv_set(ip->cdgauge, PANEL_INACTIVE, TRUE, PANEL_VALUE, 0, NULL);
	xv_set(pu->artist, PANEL_INACTIVE, TRUE, PANEL_VALUE, "No artist",
		NULL);
	xv_set(pu->cdname, PANEL_INACTIVE, TRUE, PANEL_VALUE, "No CD name",
		NULL);
	new_trackname_display("", 0);
	for (i = 0; i < num_nalloc; i++)
		if (tracknames[i] != XV_NULL)
			xv_set(tracknames[i], PANEL_INACTIVE, TRUE, NULL);

	xv_set(ip->volume, PANEL_INACTIVE, TRUE, NULL);

	icon_label("No CD");

	cleanout_lists();

	xv_set(pl->playlist, XV_SHOW, FALSE, NULL);
	i = xv_get(pl->playlist, PANEL_LIST_NROWS);
	while (--i > -1)
		xv_set(pl->playlist, PANEL_LIST_DELETE, i, NULL);
	xv_set(pl->playlist, XV_SHOW, TRUE, NULL);

	i = xv_get(pl->playlists, PANEL_LIST_NROWS);
	while (--i > -1)
	{
		xv_set(pl->playlists, PANEL_LIST_DELETE, i, NULL);
	}

	xv_set(pu->tracklist, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->trackname, PANEL_INACTIVE, TRUE, PANEL_VALUE, empty, NULL);
	xv_set(pu->trackoptions, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->autoplay, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->playlist, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->playlists, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->listname, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->delete, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->button5, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->button6, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pl->button7, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->balance, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->a, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->b, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->alabel, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->blabel, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->abrepeat, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->split, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->delete, PANEL_INACTIVE, TRUE, NULL);
	xv_set(Workman_goodies->indexscan, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->whichvolume, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->defaultvolume, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->defaultspeaker, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->playmode, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->button1, PANEL_INACTIVE, TRUE, NULL);
	xv_set(pu->button8, PANEL_INACTIVE, TRUE, NULL);
	xv_set(ip->tracks, PANEL_CHOICE_STRINGS, "No CD in drive", NULL, NULL);
	xv_set(ip->tracktimer, PANEL_LABEL_STRING, "00:00", NULL);
	xv_set(ip->cdtimer, PANEL_LABEL_STRING, "00:00", NULL);
	xv_set(ip->tracklen, PANEL_LABEL_STRING, "0:00", NULL);
	xv_set(ip->cdlen, PANEL_LABEL_STRING, "0:00", NULL);
	xv_set(ip->shuffle, PANEL_CHOICE_STRINGS, "Normal", "Shuffle",
		NULL, PANEL_VALUE, 0, PANEL_DEFAULT_VALUE, 1, NULL);
	center_tracks();

	if (track_rect != NULL)
		scoot_stuff(-add_height, 0);

	pop_track = 0;
	pl_item = -1;
	displayed_track = 0;
	mark_a = mark_b = 0;
	manual_volume = 0;
}

/*
 * Event interpose function.  Update window_is_open and a bunch of the
 * quiescent displays.  Check for a CD if we aren't polling the player
 * and there wasn't a CD before.
 */
Notify_value
check_open(f, event, arg, type)
	Frame			f;
	Notify_event		event;
	Notify_arg		arg;
	Notify_event_type 	type;
{
	Notify_value	val;
	int		was_opened = window_is_open;
	int		old_retry;

	val = (Notify_value) notify_next_event_func(f, event, arg, type);
	window_is_open = ! xv_get(f, FRAME_CLOSED);

	if (window_is_open && ! was_opened)
	{
		show_stats(Workman_window1);
		xv_set(Workman_window1->cdgauge, PANEL_VALUE, cur_pos_abs,
			NULL);
		xv_set(Workman_window1->songpos, PANEL_VALUE, cur_pos_rel,
			NULL);

		if (dont_retry && cur_cdmode == EJECTED)
		{
			old_retry = dont_retry;
			dont_retry = 0;
			handle_timer(Workman_window1->window1, 0);
			dont_retry = old_retry;
		}
	}

	return (val);
}

/*
 * Scoot stuff in the main window up or down, as appropriate.  Pass a code for
 * the topmost thing to scoot (0 = sliders, 1 = tracks) and the distance to
 * scoot, with negative meaning scoot up.
 */
void
scoot_stuff(distance, topmost)
	int	distance, topmost;
{
#define ip Workman_window1
	if (distance < 0)
	{
		xv_set(ip->window1, XV_HEIGHT, xv_get(ip->window1,
			XV_HEIGHT) + distance, NULL);
		if (topmost >= 1)
			xv_set(ip->tracks, PANEL_ITEM_Y, xv_get(ip->tracks,
				PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->tracktimer, PANEL_ITEM_Y, xv_get(ip->tracktimer,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->songpos, PANEL_ITEM_Y, xv_get(ip->songpos,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->tracklen, PANEL_ITEM_Y, xv_get(ip->tracklen,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->speaker, PANEL_ITEM_Y, xv_get(ip->speaker,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->volume, PANEL_ITEM_Y, xv_get(ip->volume,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->mode, PANEL_ITEM_Y, xv_get(ip->mode,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->repeat, PANEL_ITEM_Y, xv_get(ip->repeat,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->shuffle, PANEL_ITEM_Y, xv_get(ip->shuffle,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->cdtimer, PANEL_ITEM_Y, xv_get(ip->cdtimer,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->cdgauge, PANEL_ITEM_Y, xv_get(ip->cdgauge,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->cdlen, PANEL_ITEM_Y, xv_get(ip->cdlen,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->button4, PANEL_ITEM_Y, xv_get(ip->button4,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->button3, PANEL_ITEM_Y, xv_get(ip->button3,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->button2, PANEL_ITEM_Y, xv_get(ip->button2,
			PANEL_ITEM_Y) + distance, NULL);
		if (dismiss_button)
			xv_set(quitbutton, PANEL_ITEM_Y, xv_get(quitbutton,
				PANEL_ITEM_Y) + distance, NULL);
	}
	else if (distance > 0)
	{
		xv_set(ip->window1, XV_HEIGHT, xv_get(ip->window1,
			XV_HEIGHT) + distance, NULL);
		if (dismiss_button)
			xv_set(quitbutton, PANEL_ITEM_Y, xv_get(quitbutton,
				PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->button4, PANEL_ITEM_Y, xv_get(ip->button4,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->button3, PANEL_ITEM_Y, xv_get(ip->button3,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->button2, PANEL_ITEM_Y, xv_get(ip->button2,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->cdtimer, PANEL_ITEM_Y, xv_get(ip->cdtimer,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->cdgauge, PANEL_ITEM_Y, xv_get(ip->cdgauge,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->cdlen, PANEL_ITEM_Y, xv_get(ip->cdlen,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->volume, PANEL_ITEM_Y, xv_get(ip->volume,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->speaker, PANEL_ITEM_Y, xv_get(ip->speaker,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->mode, PANEL_ITEM_Y, xv_get(ip->mode,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->shuffle, PANEL_ITEM_Y, xv_get(ip->shuffle,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->repeat, PANEL_ITEM_Y, xv_get(ip->repeat,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->tracktimer, PANEL_ITEM_Y, xv_get(ip->tracktimer,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->songpos, PANEL_ITEM_Y, xv_get(ip->songpos,
			PANEL_ITEM_Y) + distance, NULL);
		xv_set(ip->tracklen, PANEL_ITEM_Y, xv_get(ip->tracklen,
			PANEL_ITEM_Y) + distance, NULL);
		if (topmost >= 1)
			xv_set(ip->tracks, PANEL_ITEM_Y, xv_get(ip->tracks,
				PANEL_ITEM_Y) + distance, NULL);
	}
}
#undef ip
