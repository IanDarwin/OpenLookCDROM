/*
 * @(#)workman_stubs.c	1.142	16 Jun 1995
 *
 * workman_stubs.c - Notify and event callback function stubs.
 */

static char *ident = "@(#)workman_stubs.c	1.142\t16 Jun 1995";

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <signal.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/screen.h>
#include <xview/notice.h>
#include <xview/defaults.h>
#include "workman_ui.h"
#include "struct.h"

/* Linux doesn't have a SIGEMT */
#ifndef SIGEMT
#define SIGEMT SIGUNUSED
#endif

void	quit();
void	continued();
void	setup_itimer();
void	init_stats();
void	avoid_track();
void	keep_settings();
void	cd_volume();
void	keep_cd_open();
void	figure_volume();
void	set_default_volume();
void	text_event_p();
void	next_stopmode();
void	new_cd_inserted();
char *	listentry();
char *	trackname();
int *	get_playlist();
void	make_initial_playlist();
void	kill_stats();
void	start_repeating();
void	add_playlist();
int	switch_playlists();
void	stop_repeating();
Notify_value check_open(), byebye(), sigusr1(), sigusr2(), sigquit(),
	sigttin(), sigttou(), sigemt();
void	show_cdinfo(),
	popup1_buttonpl_notify_callback(),
	window1_button4_notify_callback(),
	window1_button3_notify_callback();
Panel_item	quitbutton;
char	*getenv(), *WMstrdup();

char	*pidfile = "/tmp/.wm_pid";
char *	empty = "";
extern char *cd_device;

Rect	*track_rect = NULL;
Xv_Notice wannasave, mountedfs;
int	confirmsave;
int	add_height, small_height;
int	min_lines = -1;
int	dont_retry = 0;
int	reverse_threshold = 2;
void	(*text_event_handler)();

window1_objects	*Workman_window1;
popup1_objects	*Workman_popup1;
about_objects	*Workman_about;
goodies_objects	*Workman_goodies;
plpopup_objects	*Workman_plpopup;

extern int cur_track, cur_pos_abs, cur_pos_rel, cur_tracklen, cur_cdlen,
	cur_ntracks, cur_lasttrack, cur_firsttrack, cur_listno;
extern enum cd_modes cur_cdmode;
extern int cur_frame;
extern int cd_fd;
extern int exit_on_eject, suppress_locking;
extern int found_in_db, found_in_rc;
extern int min_volume, max_volume;
extern int intermittent_dev;
extern char *cur_cdname, *cur_artist, cur_contd, cur_avoid;
int cur_playnew = -1;
int displayed_track = -1;		/* Track whose info is onscreen */
int pop_track = 0;			/* Track being edited in popup */
int *pop_list = NULL;			/* Our notion of the playlist */
int pop_listsize = 0;			/* List size, including 0 */
int pl_item = -1;			/* Playlist item selected */
int pl_listnum = -1;			/* Number of current playlist */
int my_artist = 0, my_cdname = 0;
int num_names = 0, num_nalloc = 0;
int cur_balance = 10;
int manual_volume = 0;		/* Has the user changed the volume by hand? */
int cur_stopmode = -1;
int mark_a = 0, mark_b = 0;
int window_is_open;
int was_repeating = 0;
int info_modified = 0;
int show_titles = 1;
#if defined(hpux) || defined(__bsdi__)
int dismiss_button = 1;
#else
int dismiss_button = 0;
#endif

Attr_attribute	INSTANCE;

#ifdef __sony_news
/* XXX -- anyone know why XView messes up on NEWS without this? */
static int error_handler(void *d, void *e) { return (0); }
#endif

main(argc, argv)
	int	argc;
	char	**argv;
{
	int		c;
	FILE		*fp;
	char		*name;
        extern char     *optarg, *rcfile, *dbfiles;
	extern int	keep_open;

#if defined(SVR4) && !defined(__sony_news) /* { */
        if (setreuid(-1, getuid()) < 0)
        {
                perror("setreuid");
                exit(-1);
        }
#endif /* } */
        
	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
#ifdef __sony_news
	XSetErrorHandler(&error_handler);
#endif
	INSTANCE = xv_unique_key();

	/*
	 * Initialize the list of database locations.  Precedence is
	 * command-line then resource then environment.
	 */
	rcfile = getenv("WORKMANRC");
	rcfile = defaults_get_string("workman.db.personal",
		"Workman.Db.Personal", rcfile);
	if (rcfile)
		rcfile = WMstrdup(rcfile);
	dbfiles = getenv("WORKMANDB");
	dbfiles = defaults_get_string("workman.db.shared",
		"Workman.Db.Shared", dbfiles);
	if (dbfiles)
		dbfiles = WMstrdup(dbfiles);

	reverse_threshold = defaults_get_integer("workman.reverseThreshold",
		"Workman.ReverseThreshold", reverse_threshold);

	while ((c = getopt(argc, argv, "s:p:dc:ol:eXnbV:hCD:")) != EOF)
		switch (c) {
		case 'p':
			pidfile = optarg;
			break;
		case 'b':
			dismiss_button = ! dismiss_button;
			break;
		case 'd':
			if (! show_titles--)
				min_lines = 0;
			break;
		case 'e':
			if (dont_retry == 0)
				dont_retry = 2;
			else
				dont_retry = 1;
			break;
		case 'c':
			cd_device = optarg;
			break;
		case 'o':
			keep_open = 0;
			break;
		case 'l':
			min_lines = atoi(optarg);
			break;
		case 'X':
			exit_on_eject = 1;
			break;
		case 'n':
			suppress_locking = 1;
			break;
		case 'V':
			min_volume = atoi(optarg);
			break;
		case 'C':
			intermittent_dev = 1;
			break;
		case 's':
			send_signal(optarg, pidfile);
			break;
		case 'D':
			dbfiles = optarg;
			break;
		case 'h':
		default:
			fprintf(stderr,
"usage: %s [-bCdehnoX] [-s cmd] [-p file] [-c device] [-l N] [-V n]\n\
\t\t[-s back|fwd|pause|play|stop|eject|mute|go] [-D file]\n\
  -b  put dismiss buttons on windows\n\
  -c  use alternate device (default = %s)\n\
  -d  don't display title information\n\
  -D  filename(s) of global database\n\
  -e  don't check for CD insertion when no CD is present\n\
  -h  display this message\n", argv[0], cd_device ? cd_device : "[none]");
			fprintf(stderr,
"  -l  leave room for at least N lines of track title\n\
  -n  don't use file locking when updating database (dangerous)\n\
  -o  don't run background job to keep device open (Solaris 2.0/2.1)\n\
  -p  write process ID to another file (default = %s)\n\
  -s  send command to running WorkMan\n\
  -C  close device after eject\n\
  -V  minimum volume setting (default = %d)\n\
  -X  exit when CD is ejected\n", pidfile, min_volume);
			exit(1);
		}

	/*
	 * Initialize the user-interface components.
	 */
	Workman_window1 = window1_objects_init(NULL, NULL);
	Workman_popup1 = popup1_objects_init(NULL, Workman_window1->window1);
	Workman_about = about_objects_init(NULL, Workman_window1->window1);
	Workman_goodies = goodies_objects_init(NULL, Workman_window1->window1);
	Workman_plpopup = plpopup_objects_init(NULL, Workman_window1->window1);

	/* Cook the database filenames, or construct them if none were given. */
	split_workmandb();

	if (keep_open)
		if (fork() == 0)
			keep_cd_open();
		else
			wait(NULL);

	/*
	 * Fill up the PID-file.
	 */
	fp = fopen(pidfile, "w");
	if (fp != NULL)
	{
		fprintf(fp, "%d\n", getpid());
		fflush(fp);
		fchmod(fileno(fp), 0666);
		fclose(fp);
	}
	else
	{
		fprintf(stderr, "Warning: ");
		perror(pidfile);
	}

	if (dismiss_button)
	{
		Panel_item	button;
		int		spacing;

		xv_create(Workman_plpopup->controls5, PANEL_BUTTON,
			XV_X, 10, XV_Y, (int) xv_get(Workman_plpopup->delete,
			XV_Y), PANEL_LABEL_STRING, "Dismiss", PANEL_NOTIFY_PROC,
			popup1_buttonpl_notify_callback, NULL);
		xv_create(Workman_popup1->controls2, PANEL_BUTTON,
			XV_X, 10, XV_Y, (int) xv_get(Workman_popup1->buttonpl,
			XV_Y), PANEL_LABEL_STRING, "Dismiss", PANEL_NOTIFY_PROC,
			show_cdinfo, NULL);
#define ip Workman_window1
		/* Squish the main window buttons down some. */
		quitbutton = xv_create(ip->controls1, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Quit", PANEL_NOTIFY_PROC,
			quit, NULL);
		xv_set(ip->button3, PANEL_LABEL_STRING, "About", NULL);
		xv_set(ip->button2, PANEL_LABEL_STRING, "CD Info", NULL);
		xv_set(ip->button4, PANEL_LABEL_STRING, "Goodies", NULL);
		spacing = ((int) xv_get(ip->controls1, XV_WIDTH) - (
			(int) xv_get(ip->button2, XV_WIDTH) +
			(int) xv_get(ip->button3, XV_WIDTH) +
			(int) xv_get(ip->button4, XV_WIDTH) +
			(int) xv_get(quitbutton, XV_WIDTH))) / 5;
		xv_set(ip->button3, XV_X, spacing, NULL);
		xv_set(ip->button2, XV_X, (int) xv_get(ip->button3, XV_WIDTH) +
			(int) xv_get(ip->button3, XV_X) + spacing, NULL);
		xv_set(ip->button4, XV_X, (int) xv_get(ip->button2, XV_WIDTH) +
			(int) xv_get(ip->button2, XV_X) + spacing, NULL);
		xv_set(quitbutton, XV_Y, (int) xv_get(Workman_window1->button2,
			XV_Y), XV_X, (int) xv_get(ip->controls1, XV_WIDTH) -
			(int) xv_get(quitbutton, XV_WIDTH) - spacing, NULL);
#undef ip
		button = xv_create(Workman_about->controls3, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Dismiss", PANEL_NOTIFY_PROC,
			window1_button3_notify_callback, NULL);
		xv_set(Workman_about->about, XV_HEIGHT,
			(int) xv_get(button, XV_HEIGHT) + 5 +
			(int) xv_get(Workman_about->about, XV_HEIGHT),
			NULL);
		xv_set(button, XV_X, ((int) xv_get(Workman_about->about,
			XV_WIDTH) - (int) xv_get(button, XV_WIDTH)) / 2,
			XV_Y, (int) xv_get(Workman_about->drive, XV_Y) +
			(int) xv_get(Workman_about->drive, XV_HEIGHT) + 5,
			NULL);
		button = xv_create(Workman_goodies->controls4, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Dismiss", PANEL_NOTIFY_PROC,
			window1_button4_notify_callback, NULL);
		xv_set(Workman_goodies->goodies, XV_HEIGHT,
			(int) xv_get(button, XV_HEIGHT) +
			(int) xv_get(Workman_goodies->goodies, XV_HEIGHT),
			NULL);
		xv_set(button, XV_X, ((int) xv_get(Workman_goodies->goodies,
			XV_WIDTH) - (int) xv_get(button, XV_WIDTH)) / 2,
			XV_Y, (int) xv_get(Workman_goodies->indexscan, XV_Y) +
			(int) xv_get(Workman_goodies->indexscan, XV_HEIGHT) + 5,
			NULL);
	}

	srand(getpid());
	xv_set(Workman_window1->songpos, PANEL_INACTIVE, FALSE, NULL);
	kill_stats(Workman_window1);
	track_rect = (Rect *)xv_get(Workman_window1->tracks, PANEL_ITEM_RECT);

	xv_set(Workman_goodies->abrepeat, PANEL_INACTIVE, TRUE, NULL);

	/* Initialize some stuff Guide won't do. */
	xv_set(Workman_popup1->defaultvolume, PANEL_NOTIFY_LEVEL, PANEL_ALL,
		NULL);
	xv_set(Workman_window1->songpos, PANEL_NOTIFY_LEVEL, PANEL_ALL,
		PANEL_JUMP_DELTA, 5, NULL);
	xv_set(Workman_about->about, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(Workman_popup1->popup1, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(Workman_goodies->goodies, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(Workman_plpopup->plpopup, FRAME_CMD_PUSHPIN_IN, FALSE,
		XV_KEY_DATA, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(Workman_popup1->whichvolume, PANEL_DEFAULT_VALUE, 1, NULL);
	text_event_handler = (void (*)())xv_get(Workman_popup1->artist,
		PANEL_EVENT_PROC);
	xv_set(Workman_popup1->cdname, PANEL_EVENT_PROC, text_event_p, NULL);
	xv_set(Workman_popup1->artist, PANEL_EVENT_PROC, text_event_p, NULL);
	xv_set(Workman_popup1->trackname, PANEL_EVENT_PROC, text_event_p, NULL);
	small_height = track_rect->r_height;
	next_stopmode(Workman_window1->repeat, cur_stopmode, NULL);
	setup_itimer(Workman_window1, 5);

	/*
	 * Attempt to lay out the popups somewhat decently.  About goes at
	 * the upper left, goodies to its right.
	 * CD Info is positioned the first time it appears.
	 */
	xv_set(Workman_goodies->goodies, XV_X,
		c = ((int) xv_get(Workman_about->about, XV_WIDTH) + 15), NULL);
	xv_set(Workman_plpopup->plpopup, XV_X,
		(int) xv_get(Workman_goodies->goodies, XV_WIDTH) + c + 15,
		NULL);

	window_is_open = ! xv_get(Workman_window1->window1, FRAME_CLOSED);
	notify_interpose_event_func(Workman_window1->window1, check_open,
		NOTIFY_SAFE);
	notify_interpose_destroy_func(Workman_window1->window1, byebye);
	notify_set_signal_func(Workman_window1->window1, sigusr1, SIGUSR1,
		NOTIFY_SYNC);
	notify_set_signal_func(Workman_window1->window1, sigusr2, SIGUSR2,
		NOTIFY_SYNC);
	notify_set_signal_func(Workman_window1->window1, sigquit, SIGQUIT,
		NOTIFY_SYNC);
	notify_set_signal_func(Workman_window1->window1, sigttin, SIGTTIN,
		NOTIFY_SYNC);
	notify_set_signal_func(Workman_window1->window1, sigttou, SIGTTOU,
		NOTIFY_SYNC);
	notify_set_signal_func(Workman_window1->window1, sigemt, SIGEMT,
		NOTIFY_SYNC);

	wannasave = xv_create(Workman_window1->window1, NOTICE,
		NOTICE_MESSAGE_STRINGS, "WorkMan alert!", "",
		"You have changed this CD's information,",
		"but you didn't save your changes.", NULL, NOTICE_BUTTON_YES,
		"Save changes", NOTICE_BUTTON_NO, "Discard changes",
		NOTICE_STATUS, &confirmsave, NULL);
	mountedfs = xv_create(Workman_window1->window1, NOTICE,
		NOTICE_MESSAGE_STRINGS, "WorkMan alert!", "",
		"This CD contains a mounted filesystem.",
		"Please run 'umount' before ejecting",
		"or nasty things may happen.", NULL, NOTICE_BUTTON,
		"Okay", 0, NULL);

	/*
	 * Turn control over to XView.
	 */
	xv_main_loop(Workman_window1->window1);

	unlink(pidfile);
	exit(0);
}

static int time_wanted = -1;

#include "bitmaps/sink0"
#include "bitmaps/sink1"
#include "bitmaps/sink2"
#include "bitmaps/sink3"
#include "bitmaps/sink4"
#include "bitmaps/sink5"
#include "bitmaps/sink6"
#include "bitmaps/sink7"

static unsigned char *sink_bits[8] = { 
	sink0_bits, sink1_bits, sink2_bits, sink3_bits,
	sink4_bits, sink5_bits, sink6_bits, sink7_bits
};

/*
 * Timer handler.  This is called twice a second and updates the clocks and
 * gauges and such.
 */
Notify_value
handle_timer(c, w)
Notify_client	c;
int		w;
{
	window1_objects	*ip = Workman_window1;
	static int	new_image = 0, initted_volume = 0;
	static enum	cd_modes old_cdmode;
	Xv_opaque	old_image;

	if (xv_get(ip->mode, PANEL_VALUE) != 5 || ! dont_retry)
		switch (cd_status()) {
		case 0:		/* No CD in drive */
			cur_cdmode = EJECTED;
			if (old_cdmode != EJECTED)
			{
				if (!xv_get(ip->tracks, PANEL_INACTIVE))
				{
					keep_settings(ip);
					kill_stats(ip);
					wipe_cdinfo();
				}
				xv_set(ip->mode, PANEL_VALUE, 5, NULL);
			}
			break;
		case 1:		/* CD in drive, what state is it in? */
			if (update_everything(ip))
				return (handle_timer(c, w));

			if (cur_cdmode != STOPPED && cur_cdmode != PAUSED ||
						old_cdmode != cur_cdmode)
				show_stats(ip);

			break;

		case 2:		/* CD has just been inserted. */
			new_cd_inserted(ip);
			break;
		}

	old_cdmode = cur_cdmode;

	if (window_is_open && xv_get(Workman_about->about, XV_SHOW))
	{
		old_image = xv_get(Workman_about->sink, PANEL_LABEL_IMAGE);
		xv_set(Workman_about->sink, PANEL_LABEL_IMAGE,
			xv_create(XV_NULL, SERVER_IMAGE, SERVER_IMAGE_DEPTH, 1,
				XV_WIDTH, 64, XV_HEIGHT, 64,
				SERVER_IMAGE_X_BITS,
				sink_bits[new_image], NULL), NULL);
		xv_destroy(old_image);
		new_image = (new_image + 1) & 7;
	}

	return (NOTIFY_DONE);
}

/*
 * The CD is in the drive.  Update all the necessary screen elements and handle
 * end-of-track and so forth.  Returns 1 if handle_timer() should get the CD
 * status again immediately because of unusual conditions such as the user
 * fiddling with the track-position slider.
 */
update_everything(ip)
	window1_objects *ip;
{
	if (cur_cdmode == TRACK_DONE)		/* Done with track... */
	{
donewithcd:
		if (xv_get(Workman_goodies->abrepeat, PANEL_VALUE))
		{
			play_chunk(mark_a, mark_b);
			return (1);
		}
		if (was_repeating)
		{
			was_repeating = 0;
			play_chunk(mark_b, cur_lasttrack >= cur_ntracks ?
				(cd->length - 1) * 75 :
				cd->trk[cur_lasttrack].start-1);
			return (1);
		}

		play_next_entry();
		if (cd_status() != 1)
			return (1);
		if (cur_cdmode == STOPPED)	/* Done with CD */
		{
			xv_set(Workman_goodies->abrepeat,
				PANEL_VALUE, FALSE, NULL);
			switch (xv_get(ip->repeat, PANEL_VALUE))
			{
			case 1:
				make_playlist(xv_get(ip->shuffle,
					PANEL_VALUE), 0);
				play_next_entry();
				break;
			case 2:
				keep_settings(ip);
				if (info_modified)
				{
					xv_set(wannasave, XV_SHOW, TRUE, NULL);
					if (confirmsave)
						save_config(NULL, NULL);
					info_modified = 0;
				}
				if (eject_cd() == 0)
				{
					setup_itimer(ip, 5);
					kill_stats(ip);
				}
				break;
			default:
				icon_label("Stop");
				xv_set(ip->tracks, PANEL_VALUE, -1, NULL);
				xv_set(ip->tracklen, PANEL_LABEL_STRING,
					"0:00", NULL);
				cur_pos_abs = cur_pos_rel = 0;
				cur_tracklen = 0;
				new_trackname_display("", 0);
				xv_set(Workman_goodies->delete,
					PANEL_INACTIVE, TRUE, NULL);
				xv_set(Workman_goodies->split,
					PANEL_INACTIVE, TRUE, NULL);
				reset_cdlen(ip);
				displayed_track = -1;
				cur_track = -1;
			}
		}
	}

	/* We're at the end of the previous track. */
	if (cur_firsttrack != -1 && cur_track < cur_firsttrack)
		cur_track = cur_firsttrack;

	/* The slider has been moved... */
	if (time_wanted > -1 && cur_cdmode == PLAYING)
	{
		play_from_pos(time_wanted);
		time_wanted = -2;
		return (1);
	}
	if (time_wanted == -2)
	{
		time_wanted = -1;
		xv_set(ip->cdgauge, PANEL_VALUE, cur_pos_abs, NULL);
	}

	/* We've hit the start of a track we don't want. */
	if (cur_lasttrack != -1 && cur_track > cur_lasttrack)
		goto donewithcd;

	return (0);
}

Defaults_pairs autoplay_modes[] = {
	"never",	0,
	"normal",	1,
	"always",	2,
	NULL,		1
};

/*
 * A new CD has just been inserted, or possibly we're just starting up and
 * there was already a CD in the drive.  Make all the necessary preparations.
 * Called from handle_timer().
 */
void
new_cd_inserted(ip)
	window1_objects	*ip;
{
	static int	initted_volume = 0;
	static int	autoplay = -1;
	int		volume, max;

	if (! initted_volume)
	{
		initted_volume = 1;

		/*
		 * The function is being called for the first time.
		 *
		 * Set the volume to the resource-specified initial volume.
		 * If that's not available try to read the current volume from
		 * the hardware and set the slider accordingly.
		 */
		volume = defaults_get_integer("workman.initialVolume",
				"Workman.InitialVolume",
				read_initial_volume(100));
		if (volume < 0)
			volume = 0;
		if (volume > 100)
			volume = 100;
		/*
		 * Scale the value to the resolution of the volume knob.
		 */
		max = xv_get(ip->volume, PANEL_MAX_VALUE);
		volume = (volume * max) / 100;

		xv_set(ip->volume, PANEL_VALUE, volume,
			PANEL_NOTIFY_LEVEL, PANEL_ALL, NULL);
		xv_set(Workman_goodies->balance, PANEL_NOTIFY_LEVEL, PANEL_ALL,
			PANEL_VALUE, cur_balance, NULL);
		cd_volume(volume, cur_balance, max);
	}

	info_modified = 0;
	if (dont_retry > 1)
		dont_retry = 0;
	setup_itimer(ip, 0);
	init_stats(ip);
	xv_set(ip->repeat, PANEL_VALUE, cur_stopmode, NULL);
	xv_set(Workman_goodies->playnewcds, PANEL_VALUE, cur_playnew, NULL);
	show_stats(ip);
	cd_status();

	/* See if the user has specified an insertion policy in his resources.*/
	autoplay = defaults_get_enum("workman.autoPlay",
				"Workman.AutoPlay", autoplay_modes);

	if (autoplay == 2 || (autoplay == 1 && ((cur_playnew && !found_in_rc) ||
				get_autoplay())) || cur_cdmode == PLAYING)
		make_initial_playlist(get_playmode());
}

/*
 * Make an initial playlist.  If the CD was already playing, skip forward in
 * the list to an entry where the current track would be playing (except in
 * Shuffle mode; in that case, start a new random list beginning with
 * the current track.)
 */
void
make_initial_playlist(playmode)
	int	playmode;
{
	if (cur_cdmode == PLAYING)
	{
		if (playmode == 1)
		{
			make_playlist(1, cur_track);
			cur_listno = 1;
		}
		else
		{
			make_playlist(playmode, 0);
			pl_find_track(cur_track);
		}
	}
	else
	{
		make_playlist(get_playmode(), 0);
		play_next_entry();
	}
}

/*
 * Set up the interval timers.
 */
void
setup_itimer(ip, interval)
	window1_objects *ip;
	int		interval;
{
	static struct itimerval it;

	it.it_value.tv_sec = 0;
	it.it_value.tv_usec = 500000;
	it.it_interval.tv_sec = interval;
	it.it_interval.tv_usec = interval ? 0 : 500000;
	notify_set_itimer_func(ip->window1, handle_timer, ITIMER_REAL,
		&it, NULL);
}

/*
 * Notify callback function for `mode'.
 */
void
change_mode(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	window1_objects *ip = Workman_window1;
	int	track, playmode;

	if (cur_cdmode == EJECTED && dont_retry)
		handle_timer(NULL, NULL);
	if (cur_cdmode == EJECTED || (cur_track == -1 && (value == 0 || value == 2)))
	{
		xv_set(ip->mode, PANEL_VALUE, cur_cdmode, NULL);
		return;
	}

	switch (value) {
	case 0:		/* back a track */
		if (cur_cdmode == PLAYING)
		{
			if (cur_pos_rel >= reverse_threshold)
				time_wanted = 0;
			else
			{
				play_prev_track();
				cd_status();
			}
		}
		else if (cur_track > 1)
		{
			cur_track--;
			time_wanted = 0;
			cur_pos_rel = time_wanted;
			cur_frame = cd->trk[cur_track - 1].start +
				time_wanted * 75;
			cur_pos_abs = cur_frame / 75;
		}

		if (cur_cdmode == PLAYING || cur_cdmode == STOPPED)
			xv_set(ip->mode, PANEL_VALUE, cur_cdmode, NULL);

		if (xv_get(Workman_goodies->abrepeat, PANEL_VALUE))
			xv_set(Workman_goodies->abrepeat, PANEL_VALUE, FALSE,
				NULL);
		displayed_track = -1;
		if (cur_track < cur_firsttrack)
			cur_track = cur_firsttrack;
		show_stats(ip);
		break;

	case 1:		/* play */
		if (cur_cdmode == PAUSED)
		{
			pause_cd();
			show_stats(ip);
			break;
		}
		if (cur_cdmode == STOPPED)
		{
			/* XXX should call make_initial_playlist() */
			track = xv_get(ip->tracks, PANEL_VALUE) + 1;
			playmode = xv_get(ip->shuffle, PANEL_VALUE);
			if (playmode == 1)
				make_playlist(1, track);
			else
			{
				make_playlist(playmode, 0);
				if (track)
				{
					pl_find_track(track);
					cur_track = track;
					cur_cdmode = PLAYING;
					play_from_pos(0);
					displayed_track = -1;
				}
			}
		}
		if (cur_cdmode != PLAYING)
			play_next_entry();
		cd_status();

		/* We're at the end of the previous track. */
		if (cur_track < cur_firsttrack)
			cur_track = cur_firsttrack;

		if (displayed_track == -1)
			new_track(ip);
		break;

	case 2:		/* forward a track */
		if (cur_cdmode == PLAYING)
		{
			play_next_track();
			if (cur_cdmode == STOPPED)
				goto stopped;
			cd_status();
		}
		else if (cur_track < cur_ntracks)
		{
			cur_track++;
			time_wanted = 0;
			cur_pos_rel = time_wanted;
			cur_frame = cd->trk[cur_track - 1].start +
				time_wanted * 75;
			cur_pos_abs = cur_frame / 75;
		}

		if (cur_cdmode == PLAYING || cur_cdmode == STOPPED)
			xv_set(ip->mode, PANEL_VALUE, cur_cdmode, NULL);

		if (xv_get(Workman_goodies->abrepeat, PANEL_VALUE))
			xv_set(Workman_goodies->abrepeat, PANEL_VALUE, FALSE,
				NULL);
		if (cur_track < cur_firsttrack)
			cur_track = cur_firsttrack;
		show_stats(ip);
		break;

	case 3:		/* pause */
		pause_cd();
		show_stats(ip);
		break;
	case 4:		/* stop */
		stop_cd();
		cd_status();
stopped:
		cur_pos_abs = cur_pos_rel = 0;
		cur_tracklen = 0;
		new_trackname_display("", 0);
		reset_cdlen(ip);
		icon_label("Stop");
		xv_set(ip->tracks, PANEL_VALUE, -1, NULL);
		xv_set(ip->tracklen, PANEL_LABEL_STRING, "0:00", NULL);
		xv_set(Workman_goodies->abrepeat, PANEL_VALUE, FALSE, NULL);
		xv_set(Workman_goodies->split, PANEL_INACTIVE, TRUE, NULL);
		xv_set(Workman_goodies->delete, PANEL_INACTIVE, TRUE, NULL);
		displayed_track = -1;
		cur_track = -1;
		break;
	case 5:		/* eject */
		keep_settings(ip);

		if (info_modified)
		{
			xv_set(wannasave, XV_SHOW, TRUE, NULL);
			if (confirmsave)
				save_config(NULL, NULL);
			info_modified = 0;
		}

		switch (eject_cd()) {
		case 0:
			setup_itimer(ip, 5);
			kill_stats(ip);
			/*
			 * need to call wipe_cdinfo() because the one in
			 * handle_timer() won't be used with -e -e
			 */
			wipe_cdinfo();
			break;
		case 1:
			xv_set(ip->mode, PANEL_VALUE, 4, NULL);
			break;	/* XXX - should display an error popup */
		case 2:
			xv_set(ip->mode, PANEL_VALUE, 4, NULL);
			xv_set(mountedfs, XV_SHOW, TRUE, NULL);
			break;
		}

		break;
	}
}

/*
 * Notify callback function for `button2'.  Show the CD Info popup.
 */
void
show_cdinfo(item, event)
	Panel_item	item;
	Event		*event;
{
	window1_objects *ip = Workman_window1;
	int		cdi_width, cdi_height, wm_width, wm_x, c;
	static int	positioned = 0;
	Xv_Screen	screen;
	Display		*dpy;

	/*
	 * CD Info is positioned (badly) at this point.  It goes to the right
	 * of the main window if it'll fit, to the left if not, aligned with
	 * the top of the main window as closely as possible.  This is not
	 * as nice as it could be, but is probably as nice as it's gonna get.
	 *
	 * XXX We make (BAD BAD BAD) assumptions about the size of the window
	 *	decorations so things line up right under olwm.
	 */
	if (! positioned)
	{
		positioned = 1;
		dpy = (Display *) xv_get(ip->window1, XV_DISPLAY);
		screen = (Xv_Screen) xv_get(ip->window1, XV_SCREEN);
		c = (int) xv_get(screen, SCREEN_NUMBER);
		cdi_width = (int) xv_get(Workman_popup1->popup1, XV_WIDTH);
		cdi_height = (int) xv_get(Workman_popup1->popup1, XV_HEIGHT);
		wm_width = (int) xv_get(ip->window1, XV_WIDTH);
		wm_x = (int) xv_get(ip->window1, XV_X);
		if (wm_width + cdi_width + 10 + (int) xv_get(ip->window1,
				XV_X) > DisplayWidth(dpy, c))
			xv_set(Workman_popup1->popup1, XV_X, wm_x - cdi_width -
				20 < 0 ? 0 : wm_x - cdi_width - 20, NULL);
		else
			xv_set(Workman_popup1->popup1, XV_X, wm_x + wm_width +
				10, NULL);
		if ((int) xv_get(ip->window1, XV_Y) + cdi_height >
							DisplayHeight(dpy, c))
			xv_set(Workman_popup1->popup1, XV_Y,
				DisplayHeight(dpy, c) - cdi_height - 28, NULL);
		else
			xv_set(Workman_popup1->popup1, XV_Y, xv_get(ip->
				window1, XV_Y) - 25, NULL);
	}
	
	if (dismiss_button && item == ip->dbmenu ||
		xv_get(Workman_popup1->popup1, FRAME_CMD_PUSHPIN_IN) == FALSE)
	{
		xv_set(Workman_popup1->popup1, FRAME_CMD_PUSHPIN_IN, TRUE,
			NULL);
		xv_set(Workman_popup1->popup1, XV_SHOW, TRUE, NULL);
		if (xv_get(Workman_plpopup->plpopup, XV_KEY_DATA,
							FRAME_CMD_PUSHPIN_IN))
			xv_set(Workman_plpopup->plpopup, XV_SHOW, TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE, XV_KEY_DATA,
				FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	}
	else
	{
		xv_set(Workman_popup1->popup1, FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_SHOW, FALSE, NULL);
		xv_set(Workman_plpopup->plpopup, FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_KEY_DATA, FRAME_CMD_PUSHPIN_IN,
			xv_get(Workman_plpopup->plpopup, FRAME_CMD_PUSHPIN_IN),
			XV_SHOW, FALSE, NULL);
	}
}

/*
 * Notify callback function for `tracks'.
 */
void
change_track(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	window1_objects *ip = Workman_window1;
	
	if (cur_cdlen > 0 && cur_cdmode != EJECTED)
	{
		if (value == -1)
		{
			if (cur_cdmode == PLAYING || cur_cdmode == PAUSED)
				xv_set(item, PANEL_VALUE, cur_track - 1, NULL);
			else
			{
				xv_set(Workman_goodies->split, PANEL_INACTIVE,
					TRUE, NULL);
				xv_set(Workman_goodies->delete, PANEL_INACTIVE,
					TRUE, NULL);
				cur_track = -1;
			}
		}
		else
			cur_track = value + 1;

		if (cur_cdmode == PLAYING)
		{
			pl_find_track(cur_track);
			play_from_pos(0);
			cd_status();
			if (cur_track < cur_firsttrack)
				cur_track = cur_firsttrack;
		}
		new_track(ip);
	}
}

/*
 * Notify callback function for `songpos'.
 */
void
change_pos(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	char	time[6];
	int	value_left;

	time_wanted = value;

	if (cur_cdmode == STOPPED && cur_track > 0)
	{
		if (! xv_get(Workman_goodies->timemode_track, PANEL_VALUE))
			sprintf(time, "%02d:%02d", value / 60, value % 60);
		else
		{
			value_left = tracklen(cur_track - 1) - value;
			if (value < 0)
				value = 0;
			sprintf(time, "%02d:%02d", value_left / 60,
							value_left % 60);
		}

		xv_set(Workman_window1->tracktimer, PANEL_LABEL_STRING, time,
			NULL);
		cur_pos_rel = time_wanted;
		cur_frame = cd->trk[cur_track - 1].start + time_wanted * 75;
		cur_pos_abs = cur_frame / 75;
	}

	if (xv_get(Workman_goodies->abrepeat, PANEL_VALUE))
		xv_set(Workman_goodies->abrepeat, PANEL_VALUE, FALSE, NULL);
}

/*
 * Notify callback function for `shuffle'.
 */
void
next_playmode_default(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	int	newdefault;
	
	if (value == 0)
		newdefault = 1;
	else
		if (cd->lists == NULL || cd->lists[value - 1].name == NULL)
			newdefault = 0;
		else
			newdefault = value + 1;

	xv_set(item, PANEL_DEFAULT_VALUE, newdefault, NULL);
}

/*
 * Notify callback function for `playlist'.
 */
int
playlist_notify(item, string, client_data, op, event, row)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
	int		row;
{
	plpopup_objects *ip = Workman_plpopup;
	
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		pl_item = -1;
		xv_set(ip->delete, PANEL_INACTIVE, TRUE, NULL);
		break;

	case PANEL_LIST_OP_SELECT:
		pl_item = row;
		xv_set(ip->delete, PANEL_INACTIVE, FALSE, NULL);
		break;

	case PANEL_LIST_OP_VALIDATE:
	case PANEL_LIST_OP_DELETE:
		break;
	}
	return XV_OK;
}

#include "bitmaps/loud0.icon"
#include "bitmaps/loud1.icon"
#include "bitmaps/loud2.icon"
#include "bitmaps/loud3.icon"
#include "bitmaps/loud4.icon"
#include "bitmaps/loud5.icon"
#include "bitmaps/loud6.icon"
#include "bitmaps/loud.icon"

unsigned char *speaker_bits[8] = {
	loud0_bits, loud1_bits, loud2_bits, loud3_bits,
	loud4_bits, loud5_bits, loud6_bits, loud_bits
};

/*
 * Notify callback function for `volume'.
 */
void
set_volume(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	window1_objects *ip = Workman_window1;
	static int	old_image = 7;
	Xv_opaque	old_serverimage;
	int		max = xv_get(item, PANEL_MAX_VALUE);
	int		new_image;

	manual_volume = 1;

	cd_volume(value, cur_balance, max);

	/* maybe show a new icon... */
	new_image = value / (max / 8);

	if (new_image > 7)
		new_image = 7;
	if (new_image != old_image)
	{
		old_serverimage = xv_get(ip->speaker, PANEL_LABEL_IMAGE);
		xv_set(ip->speaker, PANEL_LABEL_IMAGE, xv_create(XV_NULL,
			SERVER_IMAGE, SERVER_IMAGE_DEPTH, 1, XV_WIDTH, 16,
			XV_HEIGHT, 15, SERVER_IMAGE_X_BITS,
			speaker_bits[new_image], NULL), NULL);
		xv_destroy(old_serverimage);
		old_image = new_image;
	}
}

/*
 * Figure out the proper volume for this track and set it.  If the user has
 * touched the manual volume knob, use that setting instead of any default.
 *
 * XXX defaults should still affect the volume depending on how much the
 * user changed it manually.
 */
void
figure_volume(ip)
	window1_objects *ip;
{
	int volume = 0, old_manual = manual_volume;

	if (! manual_volume)
	{
		if (cur_track)
			volume = get_default_volume(cur_track);
		if (! volume)
			volume = get_default_volume(0);
	}
	if (! volume)
		volume = xv_get(ip->volume, PANEL_VALUE);
	xv_set(ip->volume, PANEL_VALUE, volume, NULL);
	set_volume(ip->volume, volume, NULL);
	manual_volume = old_manual;
}

/*
 * Notify callback function for `button3'.
 */
void
window1_button3_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	window1_objects *ip = Workman_window1;
	
	if (dismiss_button && item == ip->button3 ||
		xv_get(Workman_about->about, FRAME_CMD_PUSHPIN_IN) == FALSE)
	{
		xv_set(Workman_about->about, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		xv_set(Workman_about->about, XV_SHOW, TRUE, NULL);
	}
	else
		xv_set(Workman_about->about, FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_SHOW, FALSE, NULL);
}

/*
 * Notify callback function for `button4'.
 */
void
window1_button4_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	window1_objects *ip = Workman_window1;
	
	if (dismiss_button && item == ip->button4 ||
		xv_get(Workman_goodies->goodies, FRAME_CMD_PUSHPIN_IN) == FALSE)
	{
		xv_set(Workman_goodies->goodies, FRAME_CMD_PUSHPIN_IN, TRUE,
			NULL);
		xv_set(Workman_goodies->goodies, XV_SHOW, TRUE, NULL);
	}
	else
		xv_set(Workman_goodies->goodies, FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_SHOW, FALSE, NULL);
}

#include "bitmaps/phonesl3"
#include "bitmaps/phonesl2"
#include "bitmaps/phonesl1"
#include "bitmaps/phones0"
#include "bitmaps/phonesr1"
#include "bitmaps/phonesr2"
#include "bitmaps/phonesr3"

static unsigned char *phone_bits[7] = {
	phonesl3_bits, phonesl2_bits, phonesl1_bits, phones0_bits,
	phonesr1_bits, phonesr2_bits, phonesr3_bits
};

/*
 * Notify callback function for `balance'.
 */
void
slide_balance(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	goodies_objects *ip = Workman_goodies;
	static int	old_image = 3;
	Xv_opaque	old_serverimage;
	int		max = xv_get(item, PANEL_MAX_VALUE);
	int		new_image;

	new_image = value / (max / 6);

	/* maybe show a new icon... */
	if (new_image > 6)
		new_image = 6;
	if (new_image != old_image)
	{
		old_serverimage = xv_get(ip->phones, PANEL_LABEL_IMAGE);
		xv_set(ip->phones, PANEL_LABEL_IMAGE, xv_create(XV_NULL,
			SERVER_IMAGE, SERVER_IMAGE_DEPTH, 1, XV_WIDTH, 16,
			XV_HEIGHT, 20, SERVER_IMAGE_X_BITS,
			phone_bits[new_image], NULL), NULL);
		xv_destroy(old_serverimage);
		old_image = new_image;
	}

	cur_balance = value;
	figure_volume(Workman_window1);
}

/*
 * Notify callback function for `repeat'.
 * Change the current stopmode; then select a new default value for the
 * choice item.  Wrap around when we hit the end of the playlists.
 */
void
next_stopmode(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	cur_stopmode = value;
	xv_set(item, PANEL_DEFAULT_VALUE, (value + 1) % 3, NULL);
}

/*
 * Notify callback function for `abrepeat'.
 */
void
goodies_abrepeat_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	if (value & 1)
		start_repeating(item, value, event);
	else
		stop_repeating(item, value, event);
}

/*
 * User-defined action for `abrepeat'.
 */
void
start_repeating(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	cur_firsttrack = cur_lasttrack = -1;
	play_chunk(mark_a, mark_b);
}

/*
 * Set one of the A-B repeat time messages.  "which" should be 0 for the A
 * timer and 1 for the B timer.
 */
void
set_abtimer(which, frame)
	int		which;
	int		frame;
{
	char	buf[30];
	int	tnum, relpos;

	if (frame < cd->trk[0].start || frame > cd->length * 75)
		return;

	for (tnum = 1; tnum < cur_ntracks; tnum++)
		if (frame < cd->trk[tnum].start)
			break;
	tnum--;
	relpos = (frame - cd->trk[tnum].start) / 75;

	if (cd->trk[tnum].section)
		sprintf(buf, "Track: %02d.%02d Time: %02d:%02d",
			cd->trk[tnum].track, cd->trk[tnum].section,
			relpos / 60, relpos % 60);
	else
		sprintf(buf, "Track: %02d      Time: %02d:%02d",
			cd->trk[tnum].track, relpos / 60, relpos % 60);

	if (which)
	{
		if (mark_a && frame <= mark_a)
			return;

		mark_b = frame;
		xv_set(Workman_goodies->blabel, PANEL_LABEL_STRING, buf, NULL);
	}
	else
	{
		if (mark_b && frame >= mark_b)
		{
			mark_b = 0;
			return;
		}

		mark_a = frame;
		xv_set(Workman_goodies->alabel, PANEL_INACTIVE, FALSE,
			PANEL_LABEL_STRING, buf, NULL);
	}

	if (mark_a && mark_b && mark_a < mark_b)
	{
		xv_set(Workman_goodies->abrepeat, PANEL_INACTIVE, FALSE, NULL);
		xv_set(Workman_goodies->blabel, PANEL_INACTIVE, FALSE, NULL);
	}
}

/*
 * Notify callback function for `a'.
 */
void
section_start(item, event)
	Panel_item	item;
	Event		*event;
{
	goodies_objects *ip = Workman_goodies;
	
	set_abtimer(0, cur_frame);
	xv_set(ip->blabel, PANEL_INACTIVE, TRUE, NULL);
	mark_b = 0;
	xv_set(ip->abrepeat, PANEL_VALUE, FALSE, PANEL_INACTIVE, TRUE, NULL);
}

/*
 * Notify callback function for `b'.
 */
void
section_end(item, event)
	Panel_item	item;
	Event		*event;
{
	set_abtimer(1, cur_frame);
}

/*
 * Notify callback function for `button6'.
 */
void
rename_playlist(item, event)
	Panel_item	item;
	Event		*event;
{
	plpopup_objects *ip = Workman_plpopup;
	char	*name = (char *) xv_get(ip->listname, PANEL_VALUE);
	int	i;

	info_modified = 1;

	if (name[0] == '\0' || pl_listnum == -1)
		return;

	for (i = 0; cd->lists[i].name != NULL; i++)
		if (i != pl_listnum && ! strcmp(name, cd->lists[i].name))
			break;

	if (cd->lists[i].name != NULL)
	{
		notice_prompt(ip->plpopup, event, NOTICE_FOCUS_XY,
			event_x(event), event_y(event), NOTICE_MESSAGE_STRINGS,
			"The name", name, "is already being used",
			NULL, NOTICE_BUTTON, "Comprendo", 101, NULL);
		return;
	}

	strmcpy(&cd->lists[pl_listnum].name, name);
	xv_set(ip->playlists, PANEL_LIST_STRING, pl_listnum, name, NULL);
	xv_set(Workman_window1->shuffle, PANEL_CHOICE_STRING, pl_listnum + 2,
		name, NULL);
}

/*
 * User-defined action for `button7'.
 * Add a new playlist to the system.  If the user has specified a name, use
 * it; otherwise make up a lettered name ("List X") based on the list's
 * position in the list of lists.
 */
void
add_playlist(item, event)
	Panel_item	item;
	Event		*event;
{
	plpopup_objects *ip = Workman_plpopup;
	char	*name = (char *) xv_get(ip->listname, PANEL_VALUE);
	char	fakename[sizeof("List XXX")];
	int	i;
	char	c;

	info_modified = 1;

	if (name[0] == '\0')
	{
		name = fakename;
		strcpy(name, "List A");
		if (cd->lists != NULL && cd->lists[0].name != NULL)
			for (c = 'A'; c < 'z'; c++)
			{
				name[sizeof("List")] = c;
				for (i = 0; cd->lists[i].name != NULL; i++)
					if (! strcmp(name, cd->lists[i].name))
						break;
				if (! cd->lists[i].name)
					break;
				if (c == 'Z')
					c = 'a' - 1;
			}
	}
	else if (cd->lists != NULL)
		for (i = 0; cd->lists[i].name != NULL; i++)
			if (! strcmp(name, cd->lists[i].name))
				break;

	if (cd->lists != NULL && cd->lists[i].name != NULL)
	{
		notice_prompt(ip->plpopup, event, NOTICE_FOCUS_XY,
			event_x(event), event_y(event), NOTICE_MESSAGE_STRINGS,
			"The name", name, "is already being used",
			NULL, NOTICE_BUTTON, "Comprendo", 101, NULL);
		return;
	}

	/* Make the list itself internally. */
	if (new_list(cd, name) == NULL)
	{
		perror("new_list");
		exit(1);
	}

	/* Add the list to the scrolling list of playlists. */
	i = (int) xv_get(ip->playlists, PANEL_LIST_NROWS);
	xv_set(ip->playlists, PANEL_LIST_INSERT, i, PANEL_LIST_STRING,
		i, name, PANEL_LIST_SELECT, i, TRUE, NULL);
	switch_playlists(ip->playlists, NULL, NULL, PANEL_LIST_OP_SELECT,
		NULL, i);

	/* ...And to the play mode choice item on the main window. */
	xv_set(Workman_window1->shuffle, PANEL_CHOICE_STRING, i + 2, name,
		NULL);
	if (xv_get(Workman_window1->shuffle, PANEL_DEFAULT_VALUE) == 0)
		xv_set(Workman_window1->shuffle, PANEL_DEFAULT_VALUE, i + 2,
			NULL);
}

/*
 * Notify callback function for `button5'.
 */
void
delete_playlist(item, event)
	Panel_item	item;
	Event		*event;
{
	plpopup_objects *ip = Workman_plpopup;
	int	nlists = xv_get(ip->playlists, PANEL_LIST_NROWS);
	int	shuf, i;
	
	info_modified = 1;

	if (pl_listnum >= 0)
	{
		xv_set(ip->playlists, PANEL_LIST_DELETE, pl_listnum, NULL);
		free(cd->lists[pl_listnum].name);
		if (cd->lists[pl_listnum].list != NULL)
			free(cd->lists[pl_listnum].list);

		for (i = pl_listnum; i < nlists; i++)
			cd->lists[i] = cd->lists[i + 1];

		shuf = xv_get(Workman_window1->shuffle, PANEL_VALUE);
		if (--nlists)
		{
			if (pl_listnum == nlists)
				pl_listnum--;
			xv_set(ip->playlists, PANEL_LIST_SELECT, pl_listnum,
				TRUE, NULL);
			switch_playlists(ip->playlists, NULL, NULL,
				PANEL_LIST_OP_SELECT, NULL, pl_listnum);
		}
		else
		{
			pl_listnum = -1;
			free(cd->lists);
			cd->lists = NULL;
			switch_playlists(ip->playlists, NULL, NULL,
				PANEL_LIST_OP_DESELECT, NULL, 0);
		}
		xv_set(Workman_window1->shuffle, XV_SHOW, FALSE,
			PANEL_CHOICE_STRINGS, "Normal", "Shuffle", NULL, NULL);
		for (i = 0; i < nlists; i++)
			xv_set(Workman_window1->shuffle, PANEL_CHOICE_STRING,
				i + 2, cd->lists[i].name, NULL);

		if (shuf > pl_listnum + 1)
			shuf--;
		xv_set(Workman_window1->shuffle, PANEL_VALUE, shuf, XV_SHOW,
			TRUE, NULL);
		next_playmode_default(Workman_window1->shuffle, shuf, NULL);
	}
}

/*
 * Notify callback function for `button7'.
 */
void
plpopup_button7_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	add_playlist(item, event);
}

/*
 * Insert a track into the playlist.  This is the notify procedure for the
 * dynamically-built track number menu's items.
 */
void
insert_into_playlist(menu, item)
	Menu		menu;
	Menu_item	item;
{
	plpopup_objects *ip = Workman_plpopup;
	int	trackno;

	if (pl_listnum == -1)
		return;

	info_modified = 1;

	trackno = (int) xv_get(item, XV_KEY_DATA, 1234);
	if (pop_list == NULL)
		pop_list = (int *)malloc(sizeof (int) * 2);
	else
		pop_list = (int *)realloc(pop_list, sizeof (int) *
			(pop_listsize + 2));
	if (pop_list == NULL)
	{
		perror("malloc");
		exit(1);
	}

	xv_set(ip->playlist, PANEL_LIST_INSERT, pop_listsize,
		PANEL_LIST_STRING, pop_listsize,
		listentry(trackno - 1), PANEL_LIST_SELECT,
		pop_listsize, TRUE, NULL);
	xv_set(ip->delete, PANEL_INACTIVE, FALSE, NULL);

	pl_item = pop_listsize;
	pop_list[pop_listsize++] = trackno;
	pop_list[pop_listsize] = 0;
	cd->lists[pl_listnum].list = pop_list;
}

/*
 * Notify callback function for `playlists'.
 */
int
switch_playlists(item, string, client_data, op, event, row)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
	int		row;
{
	plpopup_objects *ip = Workman_plpopup;
	int	i, *thislist;
	
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		xv_set(ip->playlist, PANEL_LIST_DELETE_ROWS, 0,
			xv_get(ip->playlist, PANEL_LIST_NROWS), NULL);
		xv_set(ip->delete, PANEL_INACTIVE, TRUE, NULL);
		xv_set(ip->button5, PANEL_INACTIVE, TRUE, NULL);
		xv_set(ip->button6, PANEL_INACTIVE, TRUE, NULL);
		pop_list = NULL;
		pop_listsize = 0;
		pl_listnum = -1;
		pl_item = -1;
		break;

	case PANEL_LIST_OP_SELECT:
		xv_set(ip->button5, PANEL_INACTIVE, FALSE, NULL);
		xv_set(ip->button6, PANEL_INACTIVE, FALSE, NULL);
		/* If there's stuff in the list already (how?), delete it. */
		if (xv_get(ip->playlist, PANEL_LIST_NROWS) != 0)
			xv_set(ip->playlist, PANEL_LIST_DELETE_ROWS, 0,
				xv_get(ip->playlist, PANEL_LIST_NROWS), NULL);
		thislist = cd->lists[row].list;
		if (thislist != NULL && thislist[0])
		{
			xv_set(ip->playlist, XV_SHOW, FALSE, NULL);
			for (i = 0; thislist[i]; i++)
				xv_set(ip->playlist, PANEL_LIST_INSERT, i,
					PANEL_LIST_STRING, i,
					listentry(thislist[i] - 1), NULL);
			xv_set(ip->playlist, XV_SHOW, TRUE, PANEL_LIST_SELECT,
				i - 1, TRUE, NULL);
			xv_set(ip->delete, PANEL_INACTIVE, FALSE, NULL);
			pop_list = thislist;
			pop_listsize = i;
			pl_item = 0;
		}
		else
		{
			pl_item = -1;
			pop_list = NULL;
			pop_listsize = 0;
			xv_set(ip->delete, PANEL_INACTIVE, TRUE, NULL);
		}
		pl_listnum = row;
		break;

	case PANEL_LIST_OP_VALIDATE:
	case PANEL_LIST_OP_DELETE:
		break;
	}
	return XV_OK;
}

/*
 * User-defined action for `abrepeat'.
 */
void
stop_repeating(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	was_repeating = 1;
	cur_lasttrack = cur_ntracks;
}

/*
 * Split the current track at the current position.
 */
void
split_track(item, event)
	Panel_item	item;
	Event		*event;
{
	int	listno;

	if (cur_frame < 1)
		return;
	
	if (! split_trackinfo(cur_frame))
		return;

	info_modified = 1;

	if (cur_track != -1 && pop_track > cur_track)
		pop_track++;

	fill_buttons();
	cleanout_lists();
	fill_lists();
	if (pl_listnum >= 0)
	{
		listno = pl_listnum;
		switch_playlists(Workman_plpopup->playlists, NULL, NULL,
			PANEL_LIST_OP_DESELECT, NULL, pl_listnum);
		switch_playlists(Workman_plpopup->playlists, NULL, NULL,
			PANEL_LIST_OP_SELECT, NULL, listno);
	}

	if (pop_track)
		xv_set(Workman_popup1->tracklist, PANEL_LIST_SELECT,
			pop_track - 1, TRUE, NULL);

	if (cur_track != -1)
		new_track(Workman_window1);
}

void
delete_track(item, event)
	Panel_item	item;
	Event		*event;
{
	int	listno;

	if (cur_track < 1)
		return;
	
	if (! remove_trackinfo(cur_track - 1))
		return;
	
	info_modified = 1;

	if (pop_track > cur_track)
		pop_track--;
	
	fill_buttons();
	cleanout_lists();
	fill_lists();
	if (pl_listnum >= 0)
	{
		listno = pl_listnum;
		switch_playlists(Workman_plpopup->playlists, NULL, NULL,
			PANEL_LIST_OP_DESELECT, NULL, pl_listnum);
		switch_playlists(Workman_plpopup->playlists, NULL, NULL,
			PANEL_LIST_OP_SELECT, NULL, listno);
	}

	if (pop_track)
	{
		if (pop_track == cur_track)
		{
			xv_set(Workman_popup1->trackname, PANEL_VALUE, "",
				NULL);
			pop_track = 0;
		}
		xv_set(Workman_popup1->tracklist, PANEL_LIST_SELECT,
			pop_track - 1, TRUE, NULL);
	}

	new_track(Workman_window1);
}

void
index_scan(item, event)
	Panel_item	item;
	Event		*event;
{
	int	track, index;

	if (cur_cdmode != STOPPED)
	{
		change_mode(NULL, STOPPED, NULL);
		cur_cdmode = STOPPED;
		xv_set(Workman_window1->mode, PANEL_VALUE, STOPPED, NULL);
	}

	for (track = 1; track <= cd->ntracks; track++)
	{
		cur_frame = 0;
		index = 2;
		while (cur_frame = find_trkind(track, index, cur_frame))
		{
			cur_track = -1;
			split_track(item, event);
			index++;
		}
	}

	stop_cd();
}

/*
 * Called when the user quits.
 */
Notify_value
byebye(c, s)
	Notify_client	c;
	Destroy_status	s;
{
	if (s == DESTROY_CHECKING && cur_cdmode != EJECTED)
	{
		keep_settings(Workman_window1);
		if (info_modified)
		{
			xv_set(wannasave, XV_SHOW, TRUE, NULL);
			if (confirmsave)
				save_config(NULL, NULL);
			info_modified = 0;
		}
	}
	else if (s == DESTROY_CLEANUP)
		return (notify_next_destroy_func(c, s));

	return (NOTIFY_DONE);
}

/*
 * Quit programmatically.  This will cause byebye() to be called, and
 * the main loop to exit.
 */
void
quit()
{
	xv_destroy_safe(Workman_window1->window1);
}

/*
 * Notify callback function for `playnewcds'.
 */
void
goodies_playnewcds_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	cur_playnew = value & 1;
}

/*
 * Handle SIGUSR1 (to pause the CD), SIGUSR2 (to play the CD), SIGTTIN
 * (to go back a track), SIGTTOU (forward a track), SIGEMT (eject CD), and
 * SIGQUIT (to stop the CD).
 */
Notify_value
sigusr1(client, sig, when)
	Notify_client		client;
	int			sig;
	Notify_signal_mode	when;
{
	if (cur_cdmode == PLAYING)
	{
		change_mode(NULL, PAUSED, NULL);
		cur_cdmode = PAUSED;
		xv_set(Workman_window1->mode, PANEL_VALUE, PAUSED, NULL);
	}

	return (NOTIFY_DONE);
}

Notify_value
sigusr2(client, sig, when)
	Notify_client		client;
	int			sig;
	Notify_signal_mode	when;
{
	if (cur_cdmode == STOPPED || cur_cdmode == PAUSED)
	{
		change_mode(NULL, PLAYING, NULL);
		cur_cdmode = PLAYING;
		xv_set(Workman_window1->mode, PANEL_VALUE, PLAYING, NULL);
	}

	return (NOTIFY_DONE);
}

Notify_value
sigquit(client, sig, when)
	Notify_client		client;
	int			sig;
	Notify_signal_mode	when;
{
	if (cur_cdmode == PLAYING || cur_cdmode == PAUSED)
	{
		change_mode(NULL, STOPPED, NULL);
		cur_cdmode = STOPPED;
		xv_set(Workman_window1->mode, PANEL_VALUE, STOPPED, NULL);
	}

	return (NOTIFY_DONE);
}

Notify_value
sigttin(client, sig, when)
	Notify_client		client;
	int			sig;
	Notify_signal_mode	when;
{
	change_mode(NULL, BACK, NULL);

	return (NOTIFY_DONE);
}

Notify_value
sigttou(client, sig, when)
	Notify_client		client;
	int			sig;
	Notify_signal_mode	when;
{
	change_mode(NULL, FORWARD, NULL);

	return (NOTIFY_DONE);
}

Notify_value
sigemt(client, sig, when)
	Notify_client		client;
	int			sig;
	Notify_signal_mode	when;
{
	change_mode(NULL, EJECTED, NULL);
	if (cur_cdmode == EJECTED)
		xv_set(Workman_window1->mode, PANEL_VALUE, 5, NULL);

	return (NOTIFY_DONE);
}

/*
 * Return the value of the "Play new CDs" button.
 */
get_playnew()
{
	return (xv_get(Workman_goodies->playnewcds, PANEL_VALUE));
}

/*
 * Send a signal to a running WorkMan and exit.
 */
send_signal(cmd, pidfile)
	char	*cmd;
	char	*pidfile;
{
	FILE	*fp;
	int	pid = 0;

	fp = fopen(pidfile, "r");
	if (fp == NULL)
	{
		perror(pidfile);
		exit(1);
	}

	fscanf(fp, "%d", &pid);
	if (pid == 0)
	{
		fprintf(stderr, "No pid in %s\n", pidfile);
		exit(1);
	}

	if (cmd[0] == 'b')
		kill(pid, SIGTTIN);
	else if (cmd[0] == 'p' && cmd[1] == 'l' || cmd[0] == 'g')
		kill(pid, SIGUSR2);
	else if (cmd[0] == 'f')
		kill(pid, SIGTTOU);
	else if (cmd[0] == 'p' && cmd[1] == 'a' || cmd[0] == 'm')
		kill(pid, SIGUSR1);
	else if (cmd[0] == 's')
		kill(pid, SIGQUIT);
	else if (cmd[0] == 'e')
		kill(pid, SIGEMT);
	else
	{
		fprintf(stderr, "Invalid command.  Use -h for help.\n");
		exit(1);
	}

	exit(0);
}
