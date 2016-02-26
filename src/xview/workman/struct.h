/* @(#)struct.h	1.17 2/24/94 */

/*
 * Structure for a single track.  This is pretty much self-explanatory --
 * one of these exists for each track on the current CD.
 */
struct trackinfo {
	char	*songname;	/* Name of song, dynamically allocated */
	char	*otherdb;	/* Unrecognized info for this track */
	char	*otherrc;
	int	length;		/* Length of track in seconds or Kbytes */
	int	start;		/* Starting position (f+s*75+m*60*75) */
	int	volume;		/* Per-track volume (1-32, 0 to disable) */
	int	track;		/* Physical track number */
	int	section;	/* Section number (0 if track not split) */
	int	contd;		/* Flag: continuation of previous track */
	int	avoid;		/* Flag: don't play this track. */
	int	data;		/* Flag: data track */
};

/*
 * Structure for internal playlist management.  The internal playlist is
 * simply the list of track ranges that are being played currently.  This
 * is built whenever the CD starts playing; it's used in normal and shuffle
 * modes as well as playlist mode.
 *
 * The "starttime" element represents how much time has elapsed by the time
 * we get to this entry.  For instance, if the list begins with a 5-minute
 * track and a 3-minute track, the third entry would have a starttime of 8
 * minutes.  This is used so that the elapsed play time can be displayed
 * even in shuffle or playlist modes.
 *
 * The last member of the list has a start track of 0, and its starttime is
 * the total playing time of the playlist (which will usually be overestimated,
 * since we don't play leadouts in some cases.)
 */
struct play {
	int	start;		/* Start track, or 0 if end of list */
	int	end;		/* last track plus 1 */
	int	starttime;	/* Number of seconds elapsed previously */
};

/*
 * Structure for playlists (as seen by the user.)  This is simply a name
 * followed by a zero-terminated list of track numbers to play.  The list
 * is terminated by a NULL name.
 */
struct playlist {
	char	*name;		/* Name of this playlist */
	int	*list;		/* List of tracks */
};

struct cdinfo_wm {
	char	artist[84];	/* Artist's name */
	char	cdname[84];	/* Disc's name */
	int	ntracks;	/* Number of tracks on the disc */
	int	length;		/* Total running time in seconds */
	int	autoplay;	/* Start playing CD immediately */
	int	playmode;	/* How to play the CD */
	int	volume;		/* Default volume (1-32, 0 for none) */
	struct trackinfo *trk;	/* struct trackinfo[ntracks] */
	struct playlist *lists;	/* User-specified playlists */
	char	*whichdb;	/* Which database is this entry from? */
	char	*otherdb;	/* Unrecognized lines from this entry */
	char	*otherrc;
	char	*user;		/* Name of originating user */
	struct cdinfo *next;	/* For browsers, etc. */
};

/* The global variable "cd" points to the struct for the CD that's playing. */
extern struct cdinfo_wm *cd;

struct playlist *new_list();

enum cd_modes	{
	UNKNOWN = -1,
	BACK = 0, TRACK_DONE = 0,
	PLAYING = 1,
	FORWARD = 2,
	PAUSED=3,
	STOPPED=4,
	EJECTED=5
};

/*
 * Drive descriptor structure.  Used for access to low-level routines.
 */
struct wm_drive {
	int	fd;		/* File descriptor, if used by platform */
	char	vendor[16];	/* Vendor name */
	char	model[24];	/* Drive model */
	void	*aux;		/* Pointer to optional platform-specific info */
	void	*daux;		/* Pointer to optional drive-specific info */

	int	(*init)();
	int	(*get_trackcount)();
	int	(*get_cdlen)();
	int	(*get_trackinfo)();
	int	(*get_drive_status)();
	int	(*get_volume)();
	int	(*set_volume)();
	int	(*pause)();
	int	(*resume)();
	int	(*stop)();
	int	(*play)();
	int	(*eject)();
};

/*
 * Each platform has to define generic functions, so may as well declare
 * them all here to save space.
 */
int     gen_init(),
	gen_get_trackcount(),
	gen_get_cdlen(),
	gen_get_trackinfo(),
	gen_get_drive_status(),
	gen_get_volume(),
	gen_set_volume(),
	gen_pause(),
	gen_resume(),
	gen_stop(),
	gen_play(),
	gen_eject();
struct wm_drive *find_drive_struct();
