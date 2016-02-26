/*
 * @(#)plat_bsd386.c	1.6	12/26/93
 *
 * BSD/386-specific drive control routines.
 */
static char *ident = "@(#)plat_bsd386.c	1.6 12/26/93";

#ifdef __bsdi__

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <string.h>
#include <sys/cdrom.h>
#ifdef SOUNDBLASTER
# include <i386/isa/sblast.h>
#endif

#include "struct.h"

/*
 * Since we can't sense the drive type with libcdrom anyway, and since the
 * library doesn't provide "pause" or "resume" functions, use the daux field
 * to point to the frame number at which we paused.
 */
struct pause_info {
	int	frame;
	int	endframe;
};
#define	PAUSE_FRAME	(((struct pause_info *) d->daux)->frame)
#define	END_FRAME	(((struct pause_info *) d->daux)->endframe)
#define CUR_CD		((struct cdinfo *) d->aux)

void *malloc();

extern char	*cd_device;

#ifdef SOUNDBLASTER
	int	min_volume = 0;
	int	max_volume = 15;
	int	min_volume_drive = 10;	/* Toshiba drive does low values. */
	int	max_volume_drive = 255;
#else /* not SOUNDBLASTER, libcdrom only */
	int	min_volume = 10;
	int	max_volume = 255;
#endif

/*
 * Initialize the drive.  A no-op for the generic driver.
 */
int
gen_init(d)
	struct wm_drive	*d;
{
	return (0);
}

/*
 * Get the number of tracks on the CD.
 */
int
gen_get_trackcount(d, tracks)
	struct wm_drive	*d;
	int		*tracks;
{
	*tracks = CUR_CD->ntracks;

	return (0);
}

/*
 * Get the start time and mode (data or audio) of a track.
 */
int
gen_get_trackinfo(d, track, data, startframe)
	struct wm_drive	*d;
	int		track, *data, *startframe;
{
	*data = (CUR_CD->tracks[track - 1].control & 4) ? 1 : 0;
	*startframe = CUR_CD->tracks[track - 1].start_frame;

	return (0);
}

/*
 * Get the number of frames on the CD.
 */
int
gen_get_cdlen(d, frames)
	struct wm_drive	*d;
	int		*frames;
{
	*frames = CUR_CD->total_frames;

	return (0);
}

/*
 * Get the current status of the drive: the current play mode, the absolute
 * position from start of disc (in frames), and the current track and index
 * numbers if the CD is playing or paused.
 */
int
gen_get_drive_status(d, oldmode, mode, pos, track, index)
	struct wm_drive	*d;
	enum cd_modes	oldmode, *mode;
	int		*pos, *track, *index;
{
	struct cdstatus	status;
	extern enum cd_modes cur_cdmode;

	/* If we can't get status, the CD is ejected, so default to that. */
	*mode = EJECTED;

	/* Is the device open? */
	if (d->aux == NULL)
	{
		switch (wmcd_open(d)) {
		case -1:	/* error */
			return (-1);

		case 1:		/* retry */
			return (0);
		}
	}

	if (cdstatus (CUR_CD, &status) < 0)
	{
		*mode = TRACK_DONE;	/* waiting for next track. */
		return (0);
	}

#define DOPOS \
		*pos = status.abs_frame; \
		*track = status.track_num; \
		*index = status.index_num

	switch (status.state) {
	case cdstate_playing:
		*mode = PLAYING;
		DOPOS;
		break;

	case cdstate_stopped:
		/* the MITSUMI drive doesn't have a "paused" state,
		   so it always comes here and not to the paused section.
		   The PAUSE_FRAME stuff below (in gen_pause())
		   fakes out the paused state. */
		if (oldmode == PLAYING) {
		    *mode = TRACK_DONE;
		    break;
		} else if (cur_cdmode != PAUSED) {
		    *mode = STOPPED;
		    DOPOS;
		    break;
		}
		/* fall through if paused */

	case cdstate_paused:
		/* the SCSI2 code in the cdrom library only pauses with
		   cdstop(); it never truly stops a disc (until an in-progress
		   play reaches the end).  So it always comes here. */
		if (cur_cdmode == STOPPED) {
		    *mode = STOPPED;
		    DOPOS;
		    break;
		}
		if (oldmode == PLAYING || oldmode == PAUSED) {
			*mode = PAUSED;
			DOPOS;
		} else {
		    *mode = STOPPED;
		    DOPOS;
		}
		break;

	default:
		*mode = STOPPED;
	}

	return (0);
}

/*
 * Return a volume value suitable for passing to the CD-ROM drive.  "vol"
 * is a volume slider setting; "max" is the slider's maximum value.
 */
static int
scale_volume(vol, max)
	int	vol, max;
{
	/* on Toshiba XM-3401B drive, and on soundblaster, this works fine. */
	return ((vol * (max_volume - min_volume)) / max + min_volume);
}

/*
 * Set the volume level for the left and right channels.  Their values
 * range from 0 to 100.
 */
int
gen_set_volume(d, left, right)
	struct wm_drive	*d;
	int		left, right;
{
	left = scale_volume(left, 100);
	right = scale_volume(right, 100);

#ifdef SOUNDBLASTER
	/* Send a Mixer IOCTL */
	if (d->fd >= 0) {
		struct sb_mixer_levels levels;
		if (ioctl(d->fd, MIXER_IOCTL_READ_LEVELS, &levels) == 0) {
			levels.cd.l = left < 0 ? 0 : left;
			levels.cd.r = right < 0 ? 0 : right;
			(void) ioctl(d->fd, MIXER_IOCTL_SET_LEVELS, &levels);
		} else
			perror("SoundBlaster mixer read failed");
	} else
#endif
	/* NOTE: the cdvolume2() call is an addition to the cdrom library.
	   Pick it up from the archives on bsdi.com */
	cdvolume2 (CUR_CD, left < 0 ? 0 : left > 255 ? 255 : left,
		   right < 0 ? 0 : right > 255 ? 255 : right);

	return (0);
}

/*
 * Pause the CD.  This is a bit of a trick since there's no cdpause()
 * function in the library.  We fake it by saving the frame number
 * and stopping.
 */
int
gen_pause(d)
	struct wm_drive	*d;
{
	struct cdstatus	status;

	if (cdstatus(d->aux, &status) < 0)
		return (-1);
	if (status.state != cdstate_playing)
		PAUSE_FRAME = CUR_CD->tracks[0].start_frame;
	else
		PAUSE_FRAME = status.abs_frame;
	if (cdstop(d->aux) < 0)
		return (-1);

	return (0);
}

/*
 * Resume playing the CD (assuming it was paused.)
 */
int
gen_resume(d)
	struct wm_drive	*d;
{
	int	status;

	status = (d->play)(d, PAUSE_FRAME, END_FRAME);
	PAUSE_FRAME = 0;
	return (status);
}

/*
 * Stop the CD.
 */
int
gen_stop(d)
	struct wm_drive *d;
{
    return cdstop(d->aux);
}

/*
 * Play the CD from one position to another (both in frames.)
 */
int
gen_play(d, start, end)
	struct wm_drive	*d;
	int		start, end;
{
	END_FRAME = end;
	if (cdplay(d->aux, start, end) < 0)
		return (-1);
	else
		return (0);
}

/*
 * Eject the current CD, if there is one.
 */
int
gen_eject(d)
	struct wm_drive	*d;
{
	cdeject(d->aux);
	cdclose(d->aux);
	d->aux = NULL;
	free(d->daux);
	d->daux = NULL;

	return (0);
}

/*
 * unscale_volume(cd_vol, max)
 *
 * Given a value between min_volume and max_volume, return the volume slider
 * value needed to achieve that value.
 *
 * Rather than perform floating-point calculations to reverse the above
 * formula, we simply do a binary search of scale_volume()'s return values.
 */
static int
unscale_volume(cd_vol, max)
	int	cd_vol, max;
{
	int	vol = 0, top = max, bot = 0, scaled;

	while (bot <= top)
	{
		vol = (top + bot) / 2;
		scaled = scale_volume(vol, max);
		if (cd_vol == scaled)
			break;
		if (cd_vol < scaled)
			top = vol - 1;
		else
			bot = vol + 1;
	}
	
	if (vol < 0)
		vol = 0;
	else if (vol > max)
		vol = max;

	return (vol);
}

/*
 * Read the initial volume from the drive, if available.  Each channel
 * ranges from 0 to 100, with -1 indicating data not available.
 */
int
gen_get_volume(d, left, right)
	struct wm_drive	*d;
	int		*left, *right;
{
	/* Most systems can't seem to do this... */
	*left = *right = -1;

#ifdef SOUNDBLASTER
	/* Send a Mixer IOCTL */
	if (d->fd >= 0) {
	    struct sb_mixer_levels levels;
	    if (ioctl(d->fd, MIXER_IOCTL_READ_LEVELS, &levels) == 0) {
		*left = unscale_volume(levels.cd.l, 100);
		*right = unscale_volume(levels.cd.r, 100);
	    } else
		perror("SoundBlaster mixer read failed");
	}
#endif

	return (0);
}

/*
 * Send an arbitrary SCSI command to a device.
 */
int
wm_scsi(d, cdb, cdblen, retbuf, retbuflen, getreply)
	struct wm_drive	*d;
	unsigned char	*cdb;
	int		cdblen;
	void		*retbuf;
	int		retbuflen;
	int		getreply;
{
	/* Don't know how to do SCSI passthrough... */
	return (-1);
}

#ifdef SOUNDBLASTER
int	sb_fd = -3;			/* patchable from external programs */
#endif

/*
 * Open the CD device.  We can't determine the drive type under BSD/386.
 */
int
wmcd_open(d)
	struct wm_drive	*d;
{
	void	*aux = NULL, *daux = NULL;
	int fd = -1;

	if (d->aux)	/* Device already open? */
		return (0);

	if ((aux = cdopen(cd_device)) == NULL)
	{
		fprintf(stderr, "No cdrom found by libcdrom\n");
		exit(1);
	}

	if ((daux = malloc(sizeof(struct pause_info))) == NULL)
		return (-1);

#ifdef SOUNDBLASTER
	if (sb_fd != -3 && sb_fd < 0)
	    fd = open ("/dev/sb_mixer", O_RDWR, 0);
	if (fd < 0) {
	    if (sb_fd != -3)
		fprintf (stderr,"SoundBlaster mixer not found/disabled; will use direct CD volume control\n");
	    max_volume = max_volume_drive;
	    min_volume = min_volume_drive;
	}
#endif

	/* Now fill in the relevant parts of the wm_drive structure. */
	*d = *(find_drive_struct("", "", ""));
	d->aux = aux;
	d->daux = daux;
	d->fd = fd;
	PAUSE_FRAME = 0;
	END_FRAME = 0;

	(d->init)(d);

	return (0);
}

void
keep_cd_open() { }

#endif /* __bsdi__ */
