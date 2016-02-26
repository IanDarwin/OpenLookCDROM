/*
 * @(#)cdrom.c	1.11	04 Jun 1995
 *
 * Interface between most of WorkMan and the low-level CD-ROM library
 * routines defined in plat_*.c and drv_*.c.  The goal is to have no
 * platform- or drive-dependent code here.
 */
static char *ident = "@(#)cdrom.c	1.11 04 Jun 1995";

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

#include "struct.h"

extern struct wm_drive generic_proto, toshiba_proto, sony_proto,
	toshiba33_proto;

/*
 * The supported drive types are listed here.  NULL means match anything.
 * The first match in the list is used, and substring matches are done (so
 * put long names before their shorter prefixes.)
 */
struct drivelist {
	char		*ven;
	char		*mod;
	char		*rev;
	struct wm_drive	*proto;
} drives[] = {
{	"TOSHIBA",	"XM-3501",	NULL,	&toshiba_proto		},
{	"TOSHIBA",	"XM-3401",	NULL,	&toshiba_proto		},
{	"TOSHIBA",	"XM-3301",	NULL,	&toshiba_proto		},
{	"SONY",		"CDU-8012",	NULL,	&sony_proto		},
{	NULL,		NULL,		NULL,	&generic_proto		}
};

void *malloc();
char *strchr();

extern struct play *playlist;
extern struct cdinfo_wm thiscd, *cd;

/*
 * Solaris 2.2 will remove the device out from under us.  Getting an ENOENT
 * is therefore sometimes not a problem.
 */
int	intermittent_dev = 0;

/*
 * Do we want to keep the CD device open after quitting by default?
 */
int	keep_open = 0;

char	*cd_device = NULL;

extern int cur_track, cur_index, cur_lasttrack, cur_firsttrack, cur_pos_abs,	
	cur_frame, cur_pos_rel, cur_tracklen, cur_cdlen, cur_ntracks,	
	cur_nsections, cur_listno, cur_stopmode, exit_on_eject,
	cur_balance;
extern enum cd_modes cur_cdmode;
extern char *cur_artist, *cur_cdname, *cur_trackname;
extern char	cur_contd, cur_avoid;

struct wm_drive	drive = { -1, "", "", NULL, NULL };

/*
 * Figure out which prototype drive structure we should be using based
 * on the vendor, model, and revision of the current drive.
 */
struct wm_drive *
find_drive_struct(vendor, model, rev)
	char	*vendor, *model, *rev;
{
	struct drivelist	*d;

	for (d = drives; d; d++)
	{
		if (d->ven != NULL && strncmp(d->ven, vendor, strlen(d->ven)) ||
		    d->mod != NULL && strncmp(d->mod, model, strlen(d->mod)) ||
		    d->rev != NULL && strncmp(d->rev, rev, strlen(d->rev)))
			continue;
		
		if (d->proto->vendor[0] == '\0')
			strcpy(d->proto->vendor, vendor);
		if (d->proto->model[0] == '\0')
			strcpy(d->proto->model, model);

		return (d->proto);
	}

	return (NULL);	/* this means the list is badly terminated. */
}

/*
 * read_toc()
 *
 * Read the table of contents from the CD.  Return a pointer to a cdinfo_wm
 * struct containing the relevant information (minus artist/cdname/etc.)
 * This is a static struct.  Returns NULL if there was an error.
 *
 * XXX allocates one trackinfo too many.
 */
struct cdinfo_wm *
read_toc()
{
	struct playlist		*l;
	int			i, pos;

	if ((drive.get_trackcount)(&drive, &thiscd.ntracks) < 0)
	{
		perror("trackcount");
		return (NULL);
	}

	thiscd.artist[0] = thiscd.cdname[0] = '\0';
	thiscd.whichdb = thiscd.otherrc = thiscd.otherdb = thiscd.user = NULL;
	thiscd.length = 0;
	thiscd.autoplay = thiscd.playmode = thiscd.volume = 0;

	/* Free up any left-over playlists. */
	if (thiscd.lists != NULL)
	{
		for (l = thiscd.lists; l->name != NULL; l++)
		{
			free(l->name);
			free(l->list);
		}
		free(thiscd.lists);
		thiscd.lists = NULL;
	}

	if (thiscd.trk != NULL)
		free(thiscd.trk);

	thiscd.trk = malloc((thiscd.ntracks + 1) * sizeof(struct trackinfo));
	if (thiscd.trk == NULL)
	{
		perror("malloc");
		return (NULL);
	}

	for (i = 0; i < thiscd.ntracks; i++)
	{
		if ((drive.get_trackinfo)(&drive, i + 1, &thiscd.trk[i].data,
					&thiscd.trk[i].start) < 0)
		{
			perror("CD track info read");
			return (NULL);
		}

		thiscd.trk[i].avoid = thiscd.trk[i].data;
		thiscd.trk[i].length = thiscd.trk[i].start / 75;

		thiscd.trk[i].songname = thiscd.trk[i].otherrc =
		thiscd.trk[i].otherdb = NULL;
		thiscd.trk[i].contd = 0;
		thiscd.trk[i].volume = 0;
		thiscd.trk[i].track = i + 1;
		thiscd.trk[i].section = 0;
	}

	if ((drive.get_cdlen)(&drive, &thiscd.trk[i].start) < 0)
	{
		perror("CD length read");
		return (NULL);
	}
	thiscd.trk[i].length = thiscd.trk[i].start / 75;

/* Now compute actual track lengths. */
	pos = thiscd.trk[0].length;

	for (i = 0; i < thiscd.ntracks; i++)
	{
		thiscd.trk[i].length = thiscd.trk[i+1].length - pos;
		pos = thiscd.trk[i+1].length;
		if (thiscd.trk[i].data)
			thiscd.trk[i].length = (thiscd.trk[i + 1].start -
				thiscd.trk[i].start) * 2;
		if (thiscd.trk[i].avoid)
			strmcpy(&thiscd.trk[i].songname, "DATA TRACK");
	}

	thiscd.length = thiscd.trk[thiscd.ntracks].length;

	return (&thiscd);
}

/*
 * cd_status()
 *
 * Return values:
 *
 *	0	No CD in drive.
 *	1	CD in drive.
 *	2	CD has just been inserted (TOC has been read)
 *
 * Updates cur_track, cur_pos_rel, cur_pos_abs and other variables.
 */
int
cd_status()
{
	static enum cd_modes	oldmode = UNKNOWN;
	enum cd_modes		mode;
	int			status, trackno;
	int			ret = 1;

	/* Open the drive.  This returns 1 if the device isn't ready. */
	status = wmcd_open(&drive);
	if (status < 0)
		return (status);
	if (status > 0)
		return (0);

	/* If the user hit the stop button, don't pass PLAYING as oldmode. */
	if (cur_cdmode == STOPPED)
		oldmode = STOPPED;

	if ((drive.get_drive_status)(&drive, oldmode, &mode, &cur_frame,
					&trackno, &cur_index) < 0)
	{
		perror("CD get drive status");
		return (-1);
	}
	oldmode = mode;

	if (mode == EJECTED || mode == UNKNOWN)
	{
		cur_cdmode = EJECTED;
		cur_track = -1;
		cur_cdlen = cur_tracklen = 1;
		cur_pos_abs = cur_pos_rel = cur_frame = 0;

		if (exit_on_eject)
			exit(0);

		return (0);
	}

	/* If there wasn't a CD before and there is now, learn about it. */
	if (cur_cdmode == EJECTED)
	{
		cur_pos_rel = cur_pos_abs = 0;

		if ((cd = read_toc()) == NULL)
			if (exit_on_eject)
				exit(-1);
			else
				return (-1);

		cur_nsections = 0;
		cur_ntracks = cd->ntracks;
		cur_cdlen = cd->length;
		load();
		cur_artist = cd->artist;
		cur_cdname = cd->cdname;
		cur_cdmode = STOPPED;
		ret = 2;
	}

	switch (mode) {
	case PLAYING:
	case PAUSED:
		cur_pos_abs = cur_frame / 75;

		/* Only look up the current track number if necessary. */
		if (cur_track < 1 || cur_frame < cd->trk[cur_track-1].start ||
				cur_frame >= (cur_track >= cur_ntracks ?
				(cur_cdlen + 1) * 75 :
				cd->trk[cur_track].start))
		{
			cur_track = 0;
			while (cur_track < cur_ntracks && cur_frame >=
					cd->trk[cur_track].start)
				cur_track++;
		}
		if (cur_track >= 1 && trackno > cd->trk[cur_track-1].track)
			cur_track++;
		/* Fall through */

	case UNKNOWN:
		if (mode == UNKNOWN)
		{
			mode = STOPPED;
			cur_lasttrack = cur_firsttrack = -1;
		}
		/* Fall through */

	case STOPPED:
		if (cur_track >= 1 && cur_track <= cur_ntracks)
		{
			cur_trackname = cd->trk[cur_track-1].songname;
			cur_avoid = cd->trk[cur_track-1].avoid;
			cur_contd = cd->trk[cur_track-1].contd;
			cur_pos_rel = (cur_frame -
				cd->trk[cur_track-1].start) / 75;
			if (cur_pos_rel < 0)
				cur_pos_rel = -cur_pos_rel;
		}

		if (playlist != NULL && playlist[0].start)
		{
			cur_pos_abs -= cd->trk[playlist[cur_listno-1].
				start - 1].start / 75;
			cur_pos_abs += playlist[cur_listno-1].starttime;
		}
		if (cur_pos_abs < 0)
			cur_pos_abs = cur_frame = 0;

		if (cur_track < 1)
			cur_tracklen = cd->length;
		else
			cur_tracklen = cd->trk[cur_track-1].length;
		/* Fall through */

	case TRACK_DONE:
		cur_cdmode = mode;
		break;
	}

	return (ret);
}

/*
 * cd_volume(vol, bal, max)
 *
 * Set the volume levels.  "vol" and "bal" are the volume and balance knob
 * settings, respectively.  "max" is the maximum value of the volume knob
 * (the balance knob is assumed to always go from 0 to 20.)
 */
void
cd_volume(vol, bal, max)
	int	vol, bal, max;
{
	int	left, right;

/*
 * Set "left" and "right" to volume-slider values accounting for the
 * balance setting.
 *
 * XXX - the maximum volume setting is assumed to be in the 20-30 range.
 */
	if (bal < 9)
		right = vol - (9 - bal) * 2;
	else
		right = vol;
	if (bal > 11)
		left = vol - (bal - 11) * 2;
	else
		left = vol;

	left = (left * 100 + max - 1) / max;
	right = (right * 100 + max - 1) / max;
	if (left > 100)
		left = 100;
	if (right > 100)
		right = 100;

	(void) (drive.set_volume)(&drive, left, right);
}

/*
 * pause_cd()
 *
 * Pause the CD, if it's in play mode.  If it's already paused, go back to
 * play mode.
 */
void
pause_cd()
{
	if (cur_cdmode == EJECTED)	/* do nothing if there's no CD! */
		return;

	switch (cur_cdmode) {
	case PLAYING:		/* playing */
		cur_cdmode = PAUSED;
		(drive.pause)(&drive);
		break;

	case PAUSED:		/* paused */
		cur_cdmode = PLAYING;
		(drive.resume)(&drive);
	}
}

/*
 * stop_cd()
 *
 * Stop the CD if it's not already stopped.
 */
void
stop_cd()
{
	if (cur_cdmode == EJECTED)
		return;

	if (cur_cdmode != STOPPED)
	{
		cur_lasttrack = cur_firsttrack = -1;
		cur_cdmode = STOPPED;
		(drive.stop)(&drive);
		cur_track = 1;
	}
}

/*
 * play_chunk(start, end)
 *
 * Play the CD from one position to another (both in frames.)
 */
void
play_chunk(start, end)
	int start, end;
{
	if (cur_cdmode == EJECTED || cd == NULL)
		return;

	end--;
	if (start >= end)
		start = end-1;

	(drive.play)(&drive, start, end);
}

/*
 * play_cd(starttrack, pos, endtrack)
 *
 * Start playing the CD or jump to a new position.  "pos" is in seconds,
 * relative to start of track.
 */
void
play_cd(start, pos, end)
int start, pos, end;
{
	if (cur_cdmode == EJECTED || cd == NULL)
		return;

	cur_firsttrack = start;
	start--;
	end--;
	cur_lasttrack = end;

	play_chunk(cd->trk[start].start + pos * 75, end >= cur_ntracks ?
		cur_cdlen * 75 : cd->trk[end].start - 1);
}

/*
 * Set the offset into the current track and play.  -1 means end of track
 * (i.e., go to next track.)
 */
void
play_from_pos(pos)
	int	pos;
{
	if (pos == -1)
		if (cd)
			pos = cd->trk[cur_track - 1].length - 1;
	if (cur_cdmode == PLAYING)
		play_cd(cur_track, pos, playlist[cur_listno-1].end);
}

/*
 * Eject the current CD, if there is one, and set the mode to 5.
 *
 * Returns 0 on success, 1 if the CD couldn't be ejected, or 2 if the
 * CD contains a mounted filesystem.
 */
eject_cd()
{
	int	status;

	if (cur_cdmode == EJECTED)	/* Already ejected! */
		return (0);

	status = (drive.eject)(&drive);
	if (status < 0)
		if (status == -3)
			return (2);
		else
			return (1);
	
	if (exit_on_eject)
		exit(0);

	cur_track = -1;
	cur_cdlen = cur_tracklen = 1;
	cur_pos_abs = cur_pos_rel = cur_frame = 0;
	cur_cdmode = EJECTED;

	return (0);
}

/*
 * find_trkind(track, index)
 *
 * Start playing at a particular track and index, optionally using a particular
 * frame as a starting position.  Returns a frame number near the start of the
 * index mark if successful, 0 if the track/index didn't exist.
 *
 * This is made significantly more tedious (though probably easier to port)
 * by the fact that CDROMPLAYTRKIND doesn't work as advertised.  The routine
 * does a binary search of the track, terminating when the interval gets to
 * around 10 frames or when the next track is encountered, at which point
 * it's a fair bet the index in question doesn't exist.
 */
find_trkind(track, index, start)
	int	track, index, start;
{
	int	top = 0, bottom, current, interval, ret = 0, i;

	if (cur_cdmode == EJECTED || cd == NULL)
		return;

	for (i = 0; i < cur_ntracks; i++)
		if (cd->trk[i].track == track)
			break;
	bottom = cd->trk[i].start;

	for (; i < cur_ntracks; i++)
		if (cd->trk[i].track > track)
			break;

	top = i == cur_ntracks ? (cd->length - 1) * 75 : cd->trk[i].start;

	if (start > bottom && start < top)
		bottom = start;

	current = (top + bottom) / 2;
	interval = (top - bottom) / 4;

	do {
		play_chunk(current, current + 75);

		if (cd_status() != 1)
			return (0);
		while (cur_frame < current)
			if (cd_status() != 1 || cur_cdmode != PLAYING)
				return (0);
			else
				susleep(1);

		if (cd->trk[cur_track - 1].track > track)
			break;

		if (cur_index >= index)
		{
			ret = current;
			current -= interval;
		}
		else
			current += interval;
		interval /= 2;
	} while (interval > 2);

	return (ret);
}

/*
 * Simulate usleep() using select().
 */
susleep(usec)
	int	usec;
{
	struct timeval	tv;

	timerclear(&tv);
	tv.tv_sec = usec / 1000000;
	tv.tv_usec = usec % 1000000;
	return (select(0, NULL, NULL, NULL, &tv));
}

/*
 * Read the initial volume from the drive, if available.  Set cur_balance to
 * the balance level (0-20, 10=centered) and return the proper setting for
 * the volume knob.
 *
 * "max" is the maximum value of the volume knob.
 */
read_initial_volume(max)
	int max;
{
	int	left, right;

	if ((drive.get_volume)(&drive, &left, &right) < 0 || left == -1)
		return (max);

	left = (left * max + 99) / 100;
	right = (right * max + 99) / 100;

	if (left < right)
	{
		cur_balance = (right - left) / 2 + 11;
		if (cur_balance > 20)
			cur_balance = 20;

		return (right);
	}
	else if (left == right)
	{
		cur_balance = 10;
		return (left);
	}
	else
	{
		cur_balance = (right - left) / 2 + 9;
		if (cur_balance < 0)
			cur_balance = 0;

		return (left);
	}
}

/*
 * Prototype wm_drive structure, with generic functions.  The generic functions
 * will be replaced with drive-specific functions as appropriate once the drive
 * type has been sensed.
 */
struct wm_drive generic_proto = {
	-1,			/* fd */
	"\0       ",		/* vendor */
	"\0               ",	/* model */
	NULL,			/* aux */
	NULL,			/* daux */

	gen_init,		/* functions... */
	gen_get_trackcount,
	gen_get_cdlen,
	gen_get_trackinfo,
	gen_get_drive_status,
	gen_get_volume,
	gen_set_volume,
	gen_pause,
	gen_resume,
	gen_stop,
	gen_play,
	gen_eject
};
