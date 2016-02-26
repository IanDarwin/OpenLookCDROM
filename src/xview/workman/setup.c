#include <xview/xview.h>
#include <xview/font.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include "workman_ui.h"

static char *ident = "@(#)setup.c	1.29 04 Jun 1995";

/*
 * Define a few macros to make relative positioning easier.
 */
#define top(item) ((int) xv_get(item, XV_Y))
#define hei(item) ((int) xv_get(item, XV_HEIGHT))
#define bot(item) (top(item) + hei(item))
#define lef(item) ((int) xv_get(item, XV_X))
#define wid(item) ((int) xv_get(item, XV_WIDTH))
#define rig(item) (lef(item) + wid(item))

#define put_down(t, s, off) xv_set(t, XV_Y, bot(s) + off, NULL)
#define put_right(t, s, off) xv_set(t, XV_X, rig(s) + off, NULL)
#define center_x(t, s) xv_set(t, XV_X, (lef(s) + rig(s) - wid(t)) / 2, NULL)
#define center_y(t, s) xv_set(t, XV_Y, (top(s) + bot(s) - hei(t)) / 2, NULL)

extern void change_track(),
	show_cdinfo(),
	show_db_prefs(),
	window1_button3_notify_callback(),
	window1_button4_notify_callback(),
	popup1_buttonpl_notify_callback(),
	popup1_button1_notify_callback(),
	goodies_playnewcds_notify_callback(),
	goodies_abrepeat_notify_callback(),
	plpopup_button7_notify_callback(),
	change_mode(), delete_from_playlist(), playlist_notify(),
	delete_playlist(), rename_playlist(), switch_playlists(),
	slide_balance(), section_start(), section_end(), change_pos(),
	next_stopmode(), set_volume(), next_playmode_default(),
	update_title(), update_trackname(), name_entered(),
	set_which_volume(), set_default_volume(), save_config(),
	cdinfo_reset(), split_track(), delete_track(), index_scan();

int	basic_spacing = 0;
int	small_buttons = -1;
int	big_spaces = 0;		/* Flag: spaces are as big as digits */

/* Make the window a particular size initially so we can position it. */
#define Dft_Size 50

#include "bitmaps/icon"

/*
 * Create the main window and its contents.
 */
window1_objects *
window1_objects_init(ip, owner)
	window1_objects	*ip;
	Xv_opaque		owner;
{
	Xv_opaque	image, image_mask, speaker_image, mode_image0,
			mode_image1, mode_image2, mode_image3, mode_image4,
			mode_image5;
	Display		*dpy;
	Xv_Screen	screen;
	Panel_item	item;
	int		space, screen_no, swidth, sheight, x;
#include "bitmaps/iconmask"
#include "bitmaps/loud.icon"
#include "bitmaps/rew.button"
#include "bitmaps/play.button"
#include "bitmaps/ff.button"
#include "bitmaps/pause.button"
#include "bitmaps/stop.button"
#include "bitmaps/eject.button"
#include "bitmaps/rew.button.small"
#include "bitmaps/play.button.small"
#include "bitmaps/ff.button.small"
#include "bitmaps/pause.button.small"
#include "bitmaps/stop.button.small"
#include "bitmaps/eject.button.small"

	if ((ip = (window1_objects *) calloc(1,
				sizeof(window1_objects))) == NULL)
		return (NULL);

	image = xv_create(XV_NULL, SERVER_IMAGE, SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, icon_bits, XV_WIDTH, 64, XV_HEIGHT, 64,
		NULL);
	image_mask = xv_create(XV_NULL, SERVER_IMAGE, SERVER_IMAGE_DEPTH,
		1, SERVER_IMAGE_X_BITS, iconmask_bits, XV_WIDTH, 64,
		XV_HEIGHT, 64, NULL);
	ip->window1 = xv_create(owner, FRAME, XV_KEY_DATA, INSTANCE,
		ip, XV_LABEL, "WorkMan", FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE, FRAME_ICON,
		xv_create(XV_NULL, ICON, ICON_IMAGE, image,
		ICON_MASK_IMAGE, image_mask, XV_LABEL, "Wait...",
		NULL), XV_WIDTH, Dft_Size, XV_HEIGHT, Dft_Size, NULL);
	dpy = (Display *) xv_get(ip->window1, XV_DISPLAY);
	screen = (Xv_Screen) xv_get(ip->window1, XV_SCREEN);
	screen_no = (int) xv_get(screen, SCREEN_NUMBER);
	swidth = DisplayWidth(dpy, screen_no);
	sheight = DisplayHeight(dpy, screen_no);

	ip->controls1 = xv_create(ip->window1, PANEL, XV_KEY_DATA,
		INSTANCE, ip, XV_HELP_DATA, "workman:controls1",
		XV_WIDTH, 10, XV_HEIGHT, 10, WIN_BORDER,
		FALSE, NULL);

	/* Create a dummy message item to get spacing information. */
	item = xv_create(ip->controls1, PANEL_MESSAGE,
		PANEL_LABEL_STRING, " ", PANEL_LABEL_BOLD, TRUE, NULL);
	if (! basic_spacing)
		basic_spacing = hei(item) / 2;
	xv_destroy(item);

	if (basic_spacing < 7)
		small_buttons = 1;
	else
		small_buttons = 0;
	small_buttons = defaults_get_boolean("workman.smallButtons",
		"Workman.SmallButtons", small_buttons);

	ip->tracks = xv_create(ip->controls1, PANEL_CHOICE,
		XV_HELP_DATA, "workman:tracks",
		XV_X, 10, XV_Y, 10 + basic_spacing * 2,
		PANEL_LAYOUT, PANEL_HORIZONTAL, PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, change_track, NULL);

	ip->tracktimer = xv_create(ip->controls1, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:tracktimer",
		XV_X, 6, PANEL_LABEL_STRING, "00:00", PANEL_LABEL_BOLD,
		FALSE, NULL);
	put_down(ip->tracktimer, ip->tracks, basic_spacing * 2);

	ip->songpos = xv_create(ip->controls1, PANEL_SLIDER,
		XV_HELP_DATA, "workman:songpos",
		PANEL_TICKS, 0,
		PANEL_DIRECTION, PANEL_HORIZONTAL,
		PANEL_SLIDER_END_BOXES, FALSE,
		PANEL_SHOW_RANGE, FALSE,
		PANEL_SHOW_VALUE, FALSE,
		PANEL_MIN_VALUE, 0,
		PANEL_MAX_VALUE, 100,
		PANEL_VALUE, 0,
		PANEL_INACTIVE, TRUE,
		PANEL_NOTIFY_PROC, change_pos,
		NULL);
	xv_set(ip->songpos, XV_Y, top(ip->tracktimer), NULL);
	put_right(ip->songpos, ip->tracktimer, 10);

	ip->tracklen = xv_create(ip->controls1, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:tracklen",
		XV_Y, top(ip->tracktimer),
		PANEL_LABEL_STRING, "00:00",
		PANEL_LABEL_BOLD, FALSE,
		NULL);

	speaker_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, loud_bits,
		XV_WIDTH, 16,
		XV_HEIGHT, 15,
		NULL);
	ip->speaker = xv_create(ip->controls1, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:speaker",
		XV_X, 12,
		PANEL_LABEL_IMAGE, speaker_image,
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	put_down(ip->speaker, ip->tracktimer, basic_spacing);

	ip->volume = xv_create(ip->controls1, PANEL_SLIDER,
		XV_HELP_DATA, "workman:volume",
		XV_X, 10,
		PANEL_TICKS, 0, PANEL_DIRECTION, PANEL_VERTICAL,
		PANEL_SLIDER_END_BOXES, FALSE, PANEL_SHOW_RANGE, FALSE,
		PANEL_SHOW_VALUE, FALSE, PANEL_MIN_VALUE, 0,
		PANEL_MAX_VALUE, 32, PANEL_VALUE, 0,
		PANEL_NOTIFY_PROC, set_volume, NULL);
	put_down(ip->volume, ip->speaker, 3);

	mode_image0 = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, small_buttons ?
			rew_button_small_bits : rew_button_bits,
		XV_WIDTH, small_buttons ? 16 : 32,
		XV_HEIGHT, 16,
		NULL);
	mode_image1 = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, small_buttons ?
			play_button_small_bits : play_button_bits,
		XV_WIDTH, small_buttons ? 16 : 32,
		XV_HEIGHT, 16,
		NULL);
	mode_image2 = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, small_buttons ?
			ff_button_small_bits : ff_button_bits,
		XV_WIDTH, small_buttons ? 16 : 32,
		XV_HEIGHT, 16,
		NULL);
	mode_image3 = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, small_buttons ?
			pause_button_small_bits : pause_button_bits,
		XV_WIDTH, small_buttons ? 16 : 32,
		XV_HEIGHT, 16,
		NULL);
	mode_image4 = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, small_buttons ?
			stop_button_small_bits : stop_button_bits,
		XV_WIDTH, small_buttons ? 16 : 32,
		XV_HEIGHT, 16,
		NULL);
	mode_image5 = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, small_buttons ?
			eject_button_small_bits : eject_button_bits,
		XV_WIDTH, small_buttons ? 16 : 32,
		XV_HEIGHT, 16,
		NULL);
	ip->mode = xv_create(ip->controls1, PANEL_CHOICE,
		XV_HELP_DATA, "workman:mode",
		PANEL_CHOICE_NROWS, 2,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, change_mode,
		PANEL_CHOICE_IMAGES,
			mode_image0,
			mode_image1,
			mode_image2,
			mode_image3,
			mode_image4,
			mode_image5,
			NULL,
		PANEL_VALUE, 4,
		NULL);
	put_right(ip->mode, ip->speaker, 8);
	xv_set(ip->mode, XV_Y, (bot(ip->speaker) + top(ip->speaker)) / 2, NULL);

	/* Align the volume slider with the mode selector. */
	xv_set(ip->volume, PANEL_SLIDER_WIDTH, bot(ip->mode) - top(ip->volume) -
		10, NULL);

	ip->repeat = xv_create(ip->controls1, PANEL_CHOICE,
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_HELP_DATA, "workman:repeat",
		XV_Y, top(ip->mode),
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_NOTIFY_PROC, next_stopmode,
		PANEL_CHOICE_STRINGS,
			"Stop",
			"Repeat",
			"Eject",
			NULL,
		PANEL_DEFAULT_VALUE, 0,
		NULL);
	put_right(ip->repeat, ip->mode, 8);

	ip->shuffle = xv_create(ip->controls1, PANEL_CHOICE,
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_HELP_DATA, "workman:shuffle",
		XV_X, lef(ip->repeat),
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_NOTIFY_PROC, next_playmode_default,
		PANEL_CHOICE_STRINGS,
			"Normal",
			"Shuffle",
			NULL,
		PANEL_DEFAULT_VALUE, 0,
		NULL);
	put_down(ip->shuffle, ip->repeat, 0);

	ip->button3 = xv_create(ip->controls1, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button3",
		PANEL_LABEL_STRING, "About...",
		PANEL_NOTIFY_PROC, window1_button3_notify_callback,
		NULL);

/*
	ip->dbmenu = xv_create(NULL, MENU,
		MENU_ACTION_ITEM, "This CD...", show_cdinfo,
		MENU_ACTION_ITEM, "Preferences...", show_db_prefs,
		NULL);
*/

	ip->button2 = xv_create(ip->controls1, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button2",
		PANEL_LABEL_STRING, "CD Info...",
/*
		PANEL_ITEM_MENU, ip->dbmenu,
*/		PANEL_NOTIFY_PROC, show_cdinfo,
		NULL);
	
	ip->button4 = xv_create(ip->controls1, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button4",
		PANEL_LABEL_STRING, "Goodies...",
		PANEL_NOTIFY_PROC, window1_button4_notify_callback,
		NULL);

	/* Now we can figure out how wide the window should be. */
	space = wid(ip->button2) + wid(ip->button3) + wid(ip->button4);
	if (rig(ip->shuffle) > space)
		xv_set(ip->controls1, XV_WIDTH, rig(ip->shuffle) + 7, NULL);
	else
	{
		xv_set(ip->controls1, XV_WIDTH, space + 10, NULL);
		xv_set(ip->shuffle, XV_X, space - wid(ip->shuffle) + 10, NULL);
		xv_set(ip->repeat, XV_X, lef(ip->shuffle), NULL);
		xv_set(ip->mode, XV_X, (lef(ip->shuffle) - rig(ip->volume) -
			wid(ip->mode)) / 2 + rig(ip->volume), NULL);
	}

	/* And knowing that, we can position the songpos slider. */
	xv_set(ip->tracklen, XV_X, wid(ip->controls1) - wid(ip->tracklen) -
		10, NULL);
	xv_set(ip->songpos, PANEL_SLIDER_WIDTH, lef(ip->tracklen) -
		lef(ip->songpos) - 20, NULL);

	ip->cdtimer = xv_create(ip->controls1, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:cdtimer",
		PANEL_LABEL_STRING, "00:00",
		XV_X, lef(ip->tracktimer), PANEL_LABEL_BOLD, FALSE, NULL);
	put_down(ip->cdtimer, ip->volume, basic_spacing / 2 + 1);

	ip->cdlen = xv_create(ip->controls1, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:cdlen",
		XV_Y, top(ip->cdtimer), XV_X, lef(ip->tracklen),
		PANEL_LABEL_BOLD, FALSE,
		NULL);

	ip->cdgauge = xv_create(ip->controls1, PANEL_GAUGE,
		XV_HELP_DATA, "workman:cdgauge",
		XV_X, lef(ip->songpos) - 2, XV_Y, top(ip->cdtimer) + 2,
		PANEL_GAUGE_WIDTH, xv_get(ip->songpos, PANEL_SLIDER_WIDTH) - 5,
		PANEL_TICKS, 0,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_DIRECTION, PANEL_HORIZONTAL,
		PANEL_SHOW_RANGE, FALSE,
		PANEL_MIN_VALUE, 0,
		PANEL_MAX_VALUE, 100,
		PANEL_VALUE, 0,
		PANEL_INACTIVE, TRUE,
		NULL);

	space = (wid(ip->controls1) - space) / 4;
	xv_set(ip->button3, XV_X, space, XV_Y, bot(ip->cdtimer) +
		basic_spacing, NULL);
	xv_set(ip->button2, XV_X, rig(ip->button3) + space, XV_Y,
		top(ip->button3), NULL);
	xv_set(ip->button4, XV_X, rig(ip->button2) + space, XV_Y,
		top(ip->button3), NULL);

	xv_set(ip->controls1, XV_HEIGHT, bot(ip->button2) + 5, NULL);
	window_fit(ip->window1);

	PANEL_EACH_ITEM(ip->controls1, item)
		xv_set(item, XV_KEY_DATA, INSTANCE, ip, NULL);
	PANEL_END_EACH;

	/*
	 * Now adjust the window's position in case it was started with
	 * -geometry -0+10 or similar.  Unfortunately, to do this we have
	 * to make assumptions about the window manager's decorations.
	 */
	space = (int) xv_get(ip->window1, XV_WIDTH);
	x = (int) xv_get(ip->window1, XV_X);
	if (space + x > swidth)
		xv_set(ip->window1, XV_X, x + Dft_Size - space, NULL);
	space = (int) xv_get(ip->window1, XV_HEIGHT);
	x = (int) xv_get(ip->window1, XV_Y);
	if (space + x > sheight)
		xv_set(ip->window1, XV_Y, x + Dft_Size - space, NULL);

	return (ip);
}

/*
 * Set up the CD Info popup.
 */
popup1_objects *
popup1_objects_init(ip, owner)
	popup1_objects	*ip;
	Xv_opaque		owner;
{
	int	space, spwidth, zerowidth;
	Xv_Font	font;
	Panel_item item;

	if ((ip = (popup1_objects *) calloc(1,
				sizeof(window1_objects))) == NULL)
		return (NULL);

	ip->popup1 = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_LABEL, "CD Information",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(ip->popup1, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);

	ip->controls2 = xv_create(ip->popup1, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "workman:controls2",
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, 10000,
		XV_HEIGHT, 10000,
		WIN_BORDER, FALSE,
		NULL);

	/*
	 * In order to properly do spacing on the track list, we need to
	 * determine the width of spaces and digits.  In most fonts, a space
	 * is half the width of a digit.  But in monospace fonts the widths
	 * are the same.
	 */
	font = (Xv_Font) xv_get(ip->controls2, XV_FONT);
	spwidth = xv_get(font, FONT_CHAR_WIDTH, ' ');
	zerowidth = xv_get(font, FONT_CHAR_WIDTH, '0');

	if (spwidth == zerowidth)
		big_spaces = 1;

	ip->artist = xv_create(ip->controls2, PANEL_TEXT,
		XV_HELP_DATA, "workman:artist",
		XV_Y, 8,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Artist:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, update_title,
		NULL);

	ip->cdname = xv_create(ip->controls2, PANEL_TEXT,
		XV_HELP_DATA, "workman:cdname",
		XV_X, 10,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Disc Title:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, update_title,
		NULL);
	put_down(ip->cdname, ip->artist, basic_spacing);

	xv_set(ip->artist, PANEL_VALUE_X, (int) xv_get(ip->cdname,
		PANEL_VALUE_X), NULL);

	ip->tracklist = xv_create(ip->controls2, PANEL_LIST,
		XV_HELP_DATA, "workman:tracklist",
		XV_X, 4,
		PANEL_LIST_DISPLAY_ROWS, 6,
		PANEL_LIST_TITLE, "Tracks",
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_INACTIVE, TRUE,
		PANEL_NOTIFY_PROC, update_trackname,
		NULL);
	put_down(ip->tracklist, ip->cdname, basic_spacing);

	ip->trackname = xv_create(ip->controls2, PANEL_TEXT,
		XV_HELP_DATA, "workman:trackname",
		XV_X, 5,
		PANEL_VALUE_DISPLAY_LENGTH, 35,
		PANEL_VALUE_STORED_LENGTH, 500,
		PANEL_LABEL_STRING, "Name:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_INACTIVE, TRUE,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, name_entered,
		NULL);
	put_down(ip->trackname, ip->tracklist, basic_spacing);

	xv_set(ip->tracklist, PANEL_LIST_WIDTH, wid(ip->trackname) + 10, NULL);

	ip->trackoptions = xv_create(ip->controls2, PANEL_TOGGLE,
		XV_HELP_DATA, "workman:trackoptions",
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_STRINGS,
			"Continuation of previous track",
			"Don't play this track",
			NULL,
		PANEL_VALUE, 0,
		PANEL_INACTIVE, TRUE,
		NULL);
	put_down(ip->trackoptions, ip->trackname, 5);

	ip->whichvolume = xv_create(ip->controls2, PANEL_CHOICE,
		PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_HELP_DATA, "workman:whichvolume",
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Default Volume for this",
		PANEL_NOTIFY_PROC, set_which_volume,
		PANEL_CHOICE_STRINGS,
			"Disc",
			"Track",
			NULL,
		PANEL_DEFAULT_VALUE, 0,
		NULL);
	put_down(ip->whichvolume, ip->trackoptions, 5);

	ip->defaultvolume = xv_create(ip->controls2, PANEL_SLIDER,
		XV_HELP_DATA, "workman:defaultvolume",
		PANEL_SLIDER_WIDTH, 2,
		PANEL_TICKS, 0,
		PANEL_DIRECTION, PANEL_HORIZONTAL,
		PANEL_SLIDER_END_BOXES, FALSE,
		PANEL_SHOW_RANGE, FALSE,
		PANEL_SHOW_VALUE, FALSE,
		PANEL_MIN_VALUE, 0,
		PANEL_MAX_VALUE, 32,
		PANEL_VALUE, 0,
		PANEL_NOTIFY_PROC, set_default_volume,
		NULL);

	/* This will get changed to an image later, but use text for now */
	ip->defaultspeaker = xv_create(ip->controls2, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:defaultspeaker",
		PANEL_LABEL_STRING, "None",
		PANEL_LABEL_BOLD, FALSE,
		XV_SHOW, FALSE,
		NULL);

	ip->nonemsg = xv_create(ip->controls2, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:defaultspeaker",
		PANEL_LABEL_STRING, "None",
		PANEL_LABEL_BOLD, FALSE,
		XV_X, xv_get(ip->defaultspeaker, XV_X),
		NULL);

	space = wid(ip->whichvolume) + wid(ip->defaultspeaker) * 2 +
		wid(ip->defaultvolume) + 20;
	if (space > wid(ip->tracklist))
	{
		xv_set(ip->controls2, XV_WIDTH, space + 10, NULL);
		xv_set(ip->tracklist, PANEL_LIST_WIDTH, space + 3 -
			wid(ip->tracklist) + (int) xv_get(ip->tracklist,
			PANEL_LIST_WIDTH), NULL);
	}
	else
	{
		xv_set(ip->controls2, XV_WIDTH, wid(ip->tracklist) + 7, NULL);
		space += 20;
	}

	center_x(ip->trackoptions, ip->controls2);

	space = (wid(ip->controls2) - space) / 2;
	xv_set(ip->whichvolume, XV_X, space, NULL);
	xv_set(ip->defaultvolume, XV_X, rig(ip->whichvolume),
		PANEL_SLIDER_WIDTH, wid(ip->defaultspeaker) * 2 - 15,
		XV_Y, (top(ip->whichvolume) + bot(ip->whichvolume) -
		hei(ip->defaultvolume)) / 2 + 2, NULL);
	xv_set(ip->defaultspeaker, XV_X, rig(ip->defaultvolume) + 5,
		XV_Y, top(ip->defaultspeaker) + hei(ip->defaultvolume) / 6,
		NULL);
	xv_set(ip->nonemsg, XV_X, xv_get(ip->defaultspeaker, XV_X),
		XV_Y, xv_get(ip->defaultspeaker, XV_Y),
		NULL);

	ip->playmode = xv_create(ip->controls2, PANEL_CHOICE,
		XV_HELP_DATA, "workman:playmode",
		XV_X, lef(ip->whichvolume),
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Default Play Mode",
		PANEL_CHOICE_STRINGS,
			"Normal",
			"Shuffle",
			"From List",
			NULL,
		NULL);
	put_down(ip->playmode, ip->defaultvolume, basic_spacing / 2);

	ip->autoplay = xv_create(ip->controls2, PANEL_TOGGLE,
		XV_HELP_DATA, "workman:autoplay",
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_STRING, 0, "Play this CD automatically",
		PANEL_VALUE, 0,
		NULL);
	put_down(ip->autoplay, ip->playmode, basic_spacing / 2);
	center_x(ip->autoplay, ip->controls2);

	ip->button1 = xv_create(ip->controls2, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button1",
		PANEL_LABEL_STRING, "Apply",
		PANEL_NOTIFY_PROC, save_config,
		NULL);
	put_down(ip->button1, ip->autoplay, (basic_spacing * 3) / 2);
	xv_set(ip->button1, XV_X, wid(ip->controls2) / 2 - wid(ip->button1) - 5,
		NULL);

	ip->button8 = xv_create(ip->controls2, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button8",
		XV_X, rig(ip->button1) + 10,
		XV_Y, top(ip->button1),
		PANEL_LABEL_STRING, "Reset",
		PANEL_NOTIFY_PROC, cdinfo_reset,
		NULL);

	ip->buttonpl = xv_create(ip->controls2, PANEL_BUTTON,
		XV_HELP_DATA, "workman:buttonpl",
		XV_Y, top(ip->button1),
		PANEL_LABEL_STRING, "Playlists...",
		PANEL_NOTIFY_PROC, popup1_buttonpl_notify_callback,
		NULL);
	xv_set(ip->buttonpl, XV_X, wid(ip->controls2) - wid(ip->buttonpl) - 10,
		NULL);

	xv_set(ip->controls2, XV_HEIGHT, bot(ip->button1) + 5, NULL);
	window_fit(ip->popup1);

	PANEL_EACH_ITEM(ip->controls2, item)
		xv_set(item, XV_KEY_DATA, INSTANCE, ip, NULL);
	PANEL_END_EACH;

	return (ip);
}

/*
 * Create the About window and its contents.
 */
about_objects *
about_objects_init(ip, owner)
	about_objects	*ip;
	Xv_opaque		owner;
{
	Panel_item	item;
	int		space;
	Xv_opaque               sink_image, message7_image;
#include "bitmaps/sink0"

	if ((ip = (about_objects *) calloc(1,
				sizeof(about_objects))) == NULL)
		return (NULL);

	ip->about = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_LABEL, "About WorkMan",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(ip->about, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);

	ip->controls3 = xv_create(ip->about, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "workman:controls3",
		XV_X, 0,
		XV_Y, 0,
		WIN_BORDER, FALSE,
		NULL);

	ip->message1 = xv_create(ip->controls3, PANEL_MESSAGE,
		XV_Y, 8,
		PANEL_LABEL_STRING, "WorkMan 1.3",
		PANEL_LABEL_BOLD, TRUE, NULL);
	ip->message2 = xv_create(ip->controls3, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "\"The Civilized CD Player\"",
		PANEL_LABEL_BOLD, FALSE, NULL);

	message7_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, icon_bits,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);
	ip->message7 = xv_create(ip->controls3, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		PANEL_LABEL_IMAGE, message7_image,
		PANEL_LABEL_BOLD, TRUE,
		NULL);

	sink_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, sink0_bits,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);
	ip->sink = xv_create(ip->controls3, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:sink",
		PANEL_LABEL_IMAGE, sink_image,
		PANEL_LABEL_BOLD, TRUE,
		NULL);

	ip->message3 = xv_create(ip->controls3, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "Written by Steven Grimm",
		PANEL_LABEL_BOLD, TRUE, NULL);
	ip->message4 = xv_create(ip->controls3, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "koreth@Hyperion.COM",
		PANEL_LABEL_BOLD, FALSE, NULL);
	ip->message5 = xv_create(ip->controls3, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "Send mail if you'd like to be",
		PANEL_LABEL_BOLD, FALSE, NULL);
	ip->message6 = xv_create(ip->controls3, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "notified of future revisions!",
		PANEL_LABEL_BOLD, FALSE, NULL);

	ip->drive = xv_create(ip->controls3, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "MMMMMMMM MMMMMMMMM",
		PANEL_LABEL_BOLD, FALSE,
		NULL);

	space = 0;
	PANEL_EACH_ITEM(ip->controls3, item)
		xv_set(item, XV_KEY_DATA, INSTANCE, ip, NULL);
		if (wid(item) > space)
			space = wid(item);
	PANEL_END_EACH;

	xv_set(ip->drive, PANEL_LABEL_STRING, "Unknown drive type", NULL);

	space += 10;
	xv_set(ip->controls3, XV_WIDTH, space, NULL);
	center_x(ip->message1, ip->controls3);
	center_x(ip->message2, ip->controls3);
	center_x(ip->message3, ip->controls3);
	center_x(ip->message4, ip->controls3);
	center_x(ip->message5, ip->controls3);
	center_x(ip->message6, ip->controls3);
	center_x(ip->drive, ip->controls3);
	xv_set(ip->message7, XV_X, space / 2 - wid(ip->message7) - 5, NULL);

	put_down(ip->message2, ip->message1, basic_spacing);
	put_down(ip->message7, ip->message2, basic_spacing);
	xv_set(ip->sink, XV_X, space / 2 + 5, XV_Y, top(ip->message7), NULL);
	put_down(ip->message3, ip->sink, basic_spacing);
	put_down(ip->message4, ip->message3, basic_spacing);
	put_down(ip->message5, ip->message4, basic_spacing * 2);
	put_down(ip->message6, ip->message5, basic_spacing);
	put_down(ip->drive, ip->message6, basic_spacing * 2);

	xv_set(ip->controls3, XV_HEIGHT, bot(ip->drive) + 5, NULL);
	window_fit(ip->about);

	return (ip);
}

/*
 * We've discovered the drive type, so update the About popup.
 */
void
about_set_drivetype(vendor, model, rev)
	char	*vendor, *model, *rev;
{
	extern about_objects    *Workman_about;
	char			drivetype[21];

	sprintf(drivetype, "%s %s", vendor, model);
	xv_set(Workman_about->drive, PANEL_LABEL_STRING, drivetype, NULL);
	center_x(Workman_about->drive, Workman_about->controls3);
}

/*
 * Create the Goodies window and its contents.
 */
goodies_objects *
goodies_objects_init(ip, owner)
	goodies_objects	*ip;
	Xv_opaque		owner;
{
	Panel_item	item;
	Xv_opaque	phones_image;
#include "bitmaps/phones0"
	int		space;

	if ((ip = (goodies_objects *) calloc(1,
				sizeof(goodies_objects))) == NULL)
		return (NULL);

	ip->goodies = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_LABEL, "Goodies",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(ip->goodies, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);

	ip->controls4 = xv_create(ip->goodies, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "workman:controls4",
		XV_X, 0,
		XV_Y, 0,
		WIN_BORDER, FALSE,
		NULL);

	ip->balance = xv_create(ip->controls4, PANEL_SLIDER,
		XV_HELP_DATA, "workman:balance",
		XV_Y, 6,
		PANEL_SLIDER_WIDTH, basic_spacing * 5,
		PANEL_TICKS, 3,
		PANEL_LABEL_STRING, "Balance:",
		PANEL_DIRECTION, PANEL_HORIZONTAL,
		PANEL_SLIDER_END_BOXES, FALSE,
		PANEL_SHOW_RANGE, FALSE,
		PANEL_SHOW_VALUE, FALSE,
		PANEL_MIN_VALUE, 0,
		PANEL_MAX_VALUE, 20,
		PANEL_VALUE, 10,
		PANEL_NOTIFY_PROC, slide_balance,
		NULL);

	phones_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_X_BITS, phones0_bits,
		XV_WIDTH, 16,
		XV_HEIGHT, 22,
		NULL);
	ip->phones = xv_create(ip->controls4, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:phones",
		XV_Y, 6,
		PANEL_LABEL_IMAGE, phones_image,
		PANEL_LABEL_BOLD, TRUE,
		NULL);

	ip->timemode_track = xv_create(ip->controls4, PANEL_CHOICE,
		XV_HELP_DATA, "workman:timemode",
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Track time display:",
		PANEL_CHOICE_STRINGS,
			"Elapsed",
			"Remaining",
			NULL,
		PANEL_VALUE, 0,
		NULL);
	put_down(ip->timemode_track, ip->balance, basic_spacing);

	ip->timemode_cd = xv_create(ip->controls4, PANEL_CHOICE,
		XV_HELP_DATA, "workman:timemode",
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "CD time display:",
		PANEL_CHOICE_STRINGS,
			"Elapsed",
			"Remaining",
			NULL,
		PANEL_VALUE, 0,
		NULL);
	put_down(ip->timemode_cd, ip->timemode_track, basic_spacing);

	ip->playnewcds = xv_create(ip->controls4, PANEL_TOGGLE,
		XV_HELP_DATA, "workman:playnewcds",
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_NOTIFY_PROC, goodies_playnewcds_notify_callback,
		PANEL_CHOICE_STRING, 0, "Auto-play unknown CDs",
		PANEL_VALUE, 0,
		NULL);
	put_down(ip->playnewcds, ip->timemode_cd, basic_spacing * 2);

	ip->abrepeat = xv_create(ip->controls4, PANEL_TOGGLE,
		XV_HELP_DATA, "workman:abrepeat",
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_NOTIFY_PROC, goodies_abrepeat_notify_callback,
		PANEL_CHOICE_STRING, 0, "Repeat section of CD:",
		PANEL_VALUE, 0,
		NULL);
	put_down(ip->abrepeat, ip->playnewcds, basic_spacing * 2);

	ip->a = xv_create(ip->controls4, PANEL_BUTTON,
		XV_HELP_DATA, "workman:a",
		PANEL_LABEL_STRING, "Start",
		PANEL_NOTIFY_PROC, section_start,
		NULL);
	put_down(ip->a, ip->abrepeat, basic_spacing);

	ip->alabel = xv_create(ip->controls4, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:alabel",
		PANEL_LABEL_STRING, "Track: 00.00 Time: 00:00",
		PANEL_LABEL_BOLD, FALSE,
		PANEL_INACTIVE, TRUE,
		NULL);
	center_y(ip->alabel, ip->a);

	ip->b = xv_create(ip->controls4, PANEL_BUTTON,
		XV_HELP_DATA, "workman:b",
		PANEL_LABEL_STRING, "End",
		PANEL_NOTIFY_PROC, section_end,
		NULL);
	put_down(ip->b, ip->a, basic_spacing / 2);

	ip->blabel = xv_create(ip->controls4, PANEL_MESSAGE,
		XV_HELP_DATA, "workman:alabel", 
		PANEL_LABEL_STRING, "Track: 00.00 Time: 00:00",
		PANEL_LABEL_BOLD, FALSE, 
		PANEL_INACTIVE, TRUE, 
		NULL); 
	center_y(ip->blabel, ip->b);

	ip->split = xv_create(ip->controls4, PANEL_BUTTON,
		XV_HELP_DATA, "workman:splittrk",
		PANEL_LABEL_STRING, "Split",
		PANEL_NOTIFY_PROC, split_track,
		NULL);
	put_down(ip->split, ip->b, basic_spacing);

	ip->delete = xv_create(ip->controls4, PANEL_BUTTON,
		XV_HELP_DATA, "workman:deletetrk",
		PANEL_LABEL_STRING, "Delete",
		PANEL_NOTIFY_PROC, delete_track,
		PANEL_INACTIVE, TRUE,
		NULL);
	put_down(ip->delete, ip->b, basic_spacing);

	ip->indexscan = xv_create(ip->controls4, PANEL_BUTTON,
		XV_HELP_DATA, "workman:indexscan",
		PANEL_LABEL_STRING, "Scan for index marks",
		PANEL_NOTIFY_PROC, index_scan,
		NULL);
	put_down(ip->indexscan, ip->split, basic_spacing);

	space = wid(ip->a) + wid(ip->alabel) + basic_spacing;
	PANEL_EACH_ITEM(ip->controls4, item)
		xv_set(item, XV_KEY_DATA, INSTANCE, ip, NULL);
		if (wid(item) > space)
			space = wid(item);
	PANEL_END_EACH;

	space += 10;
	xv_set(ip->controls4, XV_WIDTH, space, XV_HEIGHT,
		bot(ip->indexscan) + 5, NULL);
	window_fit(ip->goodies);

	center_x(ip->timemode_cd, ip->controls4);
	center_x(ip->timemode_track, ip->controls4);
	center_x(ip->playnewcds, ip->controls4);
	center_x(ip->abrepeat, ip->controls4);
	xv_set(ip->balance, XV_X, space / 2 - (wid(ip->balance) +
		wid(ip->phones) + basic_spacing) / 2, NULL);
	put_right(ip->phones, ip->balance, basic_spacing);
	xv_set(ip->a, XV_X, space / 2 - (wid(ip->a) + wid(ip->alabel) +
		basic_spacing) / 2, NULL);
	put_right(ip->alabel, ip->a, basic_spacing);
	center_x(ip->b, ip->a);
	xv_set(ip->blabel, XV_X, lef(ip->alabel), NULL);

	space = wid(ip->split) + wid(ip->delete) + basic_spacing * 3;
	space = (wid(ip->controls4) - space) / 2;
	xv_set(ip->split, XV_X, space, NULL);
	put_right(ip->delete, ip->split, basic_spacing * 2);

	center_x(ip->indexscan, ip->controls4);

	return (ip);
}

/*
 * Create the menu for the Playlists popup.
 */
Xv_opaque
trackmenu_create(ip, owner)
	caddr_t         ip;
	Xv_opaque       owner;
{
	Xv_opaque       obj;

	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
		XV_KEY_DATA, INSTANCE, ip,
		MENU_TITLE_ITEM, owner ? "" : "Tracks",
		MENU_GEN_PIN_WINDOW, owner, "Tracks",
		NULL);
	return obj;
}

/*
 * Create the Playlists window and its contents.
 */
plpopup_objects *
plpopup_objects_init(ip, owner)
	plpopup_objects	*ip;
	Xv_opaque		owner;
{
	Panel_item	item;

	if ((ip = (plpopup_objects *) calloc(1,
				sizeof(plpopup_objects))) == NULL)
		return (NULL);

	ip->plpopup = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_LABEL, "Playlists",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		NULL);
	xv_set(xv_get(ip->plpopup, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);

	ip->controls5 = xv_create(ip->plpopup, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "workman:controls5",
		XV_X, 0,
		XV_Y, 0,
		WIN_BORDER, FALSE,
		NULL);

	ip->playlists = xv_create(ip->controls5, PANEL_LIST,
		XV_HELP_DATA, "workman:playlists",
		XV_X, 6,
		XV_Y, 8,
		PANEL_LIST_WIDTH, basic_spacing * 22,
		PANEL_LIST_DISPLAY_ROWS, 4,
		PANEL_LIST_TITLE, "Playlists",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, switch_playlists,
		NULL);

	ip->listname = xv_create(ip->controls5, PANEL_TEXT,
		XV_HELP_DATA, "workman:listname",
		XV_Y, 8 + basic_spacing * 2,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 20,
		PANEL_LABEL_STRING, "Name:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		NULL);
	put_right(ip->listname, ip->playlists, basic_spacing);

	ip->button7 = xv_create(ip->controls5, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button7",
		PANEL_LABEL_STRING, "Create new list",
		PANEL_NOTIFY_PROC, plpopup_button7_notify_callback,
		NULL);
	put_down(ip->button7, ip->listname, basic_spacing);
	center_x(ip->button7, ip->listname);

	ip->button6 = xv_create(ip->controls5, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button6",
		PANEL_LABEL_STRING, "Rename this list",
		PANEL_NOTIFY_PROC, rename_playlist,
		NULL);
	put_down(ip->button6, ip->button7, basic_spacing);
	center_x(ip->button6, ip->listname);

	ip->button5 = xv_create(ip->controls5, PANEL_BUTTON,
		XV_HELP_DATA, "workman:button5",
		PANEL_LABEL_STRING, "Delete this list",
		PANEL_NOTIFY_PROC, delete_playlist,
		NULL);
	xv_set(ip->button5, XV_Y, bot(ip->playlists) - hei(ip->button5), NULL);
	center_x(ip->button5, ip->listname);

	ip->playlist = xv_create(ip->controls5, PANEL_LIST,
		XV_HELP_DATA, "workman:playlist",
		XV_X, lef(ip->playlists),
		PANEL_LIST_WIDTH, rig(ip->listname) - lef(ip->playlists),
		PANEL_LIST_DISPLAY_ROWS, 5,
		PANEL_LIST_TITLE, "Playlist (add tracks from popup menu)",
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_INACTIVE, TRUE,
		PANEL_ITEM_MENU,
			trackmenu_create((caddr_t) ip, ip->plpopup),
		PANEL_NOTIFY_PROC, playlist_notify,
		NULL);
	put_down(ip->playlist, ip->playlists, basic_spacing);

	xv_set(ip->controls5, XV_WIDTH, rig(ip->playlist) + 2, NULL);

	ip->delete = xv_create(ip->controls5, PANEL_BUTTON,
		XV_HELP_DATA, "workman:delete",
		PANEL_LABEL_STRING, "Delete track",
		PANEL_INACTIVE, TRUE,
		PANEL_NOTIFY_PROC, delete_from_playlist,
		NULL);
	put_down(ip->delete, ip->playlist, basic_spacing);
	center_x(ip->delete, ip->controls5);

	xv_set(ip->controls5, XV_HEIGHT, bot(ip->delete) + 5, NULL);
	window_fit(ip->plpopup);

	PANEL_EACH_ITEM(ip->controls5, item)
		xv_set(item, XV_KEY_DATA, INSTANCE, ip, NULL);
	PANEL_END_EACH;

	return (ip);
}

/*
 * Create the database preferences window and its contents.
db_prefs_objects *
db_prefs_init(ip, owner)
	db_prefs_objects	*ip;
	Xv_opaque		owner;
{
	if ((ip = (db_prefs_objects *) calloc(1,
				sizeof(db_prefs_objects))) == NULL)
		return (NULL);
	
	ip->window = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_LABEL, "Databases",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		NULL);
	
	ip->panel = xv_get(ip->window, FRAME_CMD_PANEL);
	xv_set(ip->panel,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "workman:dbprefs-panel",
		XV_X, 0,
		XV_Y, 0,
		WIN_BORDER, FALSE,
		NULL);
	
	ip->filelist = xv_create(ip->panel, PANEL_LIST,
		XV_HELP_DATA, "workman:dbprefs-filelist",
		XV_X, 6,
		XV_Y, 8,
		PANEL_LIST_WIDTH, basic_spacing * 40
 */
