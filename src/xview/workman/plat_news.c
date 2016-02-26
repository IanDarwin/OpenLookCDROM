/*
 * @(#)plat_news.c	1.5	1/1/94
 *
 * Sony NEWS-specific drive control routines.
 */
static char *ident = "@(#)plat_news.c	1.5 1/1/94";

#ifdef __sony_news

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <ustat.h>
#include <CD.h>

#define	DEFAULT_CD_DEVICE	"/dev/rsd/b0i6u0p2\0"

#include "struct.h"

void *malloc();
char *strchr();

extern char	*cd_device;
extern int	intermittent_dev;

int	min_volume = 128;
int	max_volume = 255;

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
	struct CD_Capacity	cc;

	if (CD_GetCapacity(d->fd, &cc))
		return (-1);

	*tracks = cc.etrack - 1;
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
	struct CD_TOCinfo	hdr;
	struct CD_TOCdata	ent;

	hdr.strack = track;
	hdr.ntrack = 1;
	hdr.data = &ent;
	if (CD_ReadTOC(d->fd, &hdr))
		return (-1);

	*data = (ent.control & 4) ? 1 : 0;
	*startframe = ent.baddr;

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
	int		tmp;

	if ((d->get_trackcount)(d, &tmp))
		return (-1);

	return (gen_get_trackinfo(d, tmp + 1, &tmp, frames));
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
	struct CD_Status		sc;

	/* If we can't get status, the CD is ejected, so default to that. */
	*mode = EJECTED;

	/* Is the device open? */
	if (d->fd < 0)
	{
		switch (wmcd_open(d)) {
		case -1:	/* error */
			return (-1);

		case 1:		/* retry */
			return (0);
		}
	}

	/* Disc is ejected.  Close the device. */
	if (CD_GetStatus(d->fd, &sc))
	{
		WMclose(d->fd);
		d->fd = -1;
		return (0);
	}

	switch (sc.status) {
	case CDSTAT_PLAY:
		*mode = PLAYING;
		*track = sc.tno;
		*index = sc.index;
		*pos = sc.baddr;
		break;

	case CDSTAT_PAUSE:
		if (oldmode == PLAYING || oldmode == PAUSED)
		{
			*mode = PAUSED;
			*track = sc.tno;
			*index = sc.index;
			*pos = sc.baddr;
		}
		else
			*mode = STOPPED;
		break;

	case CDSTAT_STOP:
		if (oldmode == PLAYING)
		{
			*mode = TRACK_DONE;	/* waiting for next track. */
			break;
		}
		/* fall through */

	default:
		*mode = STOPPED;
		break;
	}

	return (0);
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
	/* NEWS can't adjust volume! */
	return (0);
}

/*
 * Pause the CD.
 */
int
gen_pause(d)
	struct wm_drive	*d;
{
	CD_Pause(d->fd);

	return (0);
}

/*
 * Resume playing the CD (assuming it was paused.)
 */
int
gen_resume(d)
	struct wm_drive	*d;
{
	CD_Restart(d->fd);

	return (0);
}

/*
 * Stop the CD.
 */
int
gen_stop(d)
	struct wm_drive *d;
{
	CD_Stop(d->fd);

	return (0);
}

/*
 * Play the CD from one position to another (both in frames.)
 */
int
gen_play(d, start, end)
	struct wm_drive	*d;
	int		start, end;
{
	struct CD_PlayAddr		msf;

	msf.addrmode			= CD_MSF;
	msf.addr.msf.startmsf.min	= start / (60*75);
	msf.addr.msf.startmsf.sec	= (start % (60*75)) / 75;
	msf.addr.msf.startmsf.frame	= start % 75;
	msf.addr.msf.endmsf.min		= end / (60*75);
	msf.addr.msf.endmsf.sec		= (end % (60*75)) / 75;
	msf.addr.msf.endmsf.frame	= end % 75;

	if (CD_Play(d->fd, &msf))
	{
		printf("play_chunk(%d,%d)\n",start,end);
		printf("msf = %d:%d:%d %d:%d:%d\n",
			msf.addr.msf.startmsf.min,
			msf.addr.msf.startmsf.sec,
			msf.addr.msf.startmsf.frame,
			msf.addr.msf.endmsf.min,
			msf.addr.msf.endmsf.sec,
			msf.addr.msf.endmsf.frame);
		perror("CD_Play");
		return (-1);
	}

	return (0);
}

/*
 * Eject the current CD, if there is one.
 */
int
gen_eject(d)
	struct wm_drive	*d;
{
	struct stat	stbuf;
	struct ustat	ust;

	if (fstat(d->fd, &stbuf) != 0)
		return (-2);

	/* Is this a mounted filesystem? */
	if (ustat(stbuf.st_rdev, &ust) == 0)
		return (-3);

	if (CD_AutoEject(d->fd))
		return (-1);

	/* Close the device if it needs to vanish. */
	if (intermittent_dev)
	{
		WMclose(d->fd);
		d->fd = -1;
	}

	return (0);
}

/*
 * Close the CD device.
 */
int
WMclose(fd)
	int	fd;
{
	int	ret;

	ret = CD_Close(fd);
	susleep(3000000);
	return (ret);
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
	/* Suns, HPs, Linux, NEWS can't read the volume; oh well */
	*left = *right = -1;

	return (0);
}

/*
 * Pass SCSI commands to the device.
 */
int
wm_scsi(d, cdb, cdblen, buf, buflen, getreply)
	struct wm_drive	*d;
	unsigned char	*cdb, *buf;
	int		cdblen, buflen, getreply;
{
	/* NEWS can't do SCSI passthrough... or can it? */
	return (-1);
}

/*
 * Open the CD device and figure out what kind of drive is attached.
 */
int
wmcd_open(d)
	struct wm_drive	*d;
{
	int		fd;
	static int	warned = 0;
	char		vendor[9], model[17], rev[5];

	if (d->fd >= 0)		/* Device already open? */
		return (0);

	intermittent_dev = 1;
	if (cd_device == NULL)
		cd_device = DEFAULT_CD_DEVICE;

	if ((d->fd = CD_Open(cd_device, 0)) < 0)
	{
		/* Solaris 2.2 volume manager moves links around */
		if (errno == ENOENT && intermittent_dev)
			return (0);

		if (errno == EACCES)
		{
			if (!warned)
			{
				char	realname[MAXPATHLEN];

				if (realpath(cd_device, realname) == NULL)
				{
					perror("realpath");
					return (0);
				}

				fprintf(stderr,
		"As root, please run\n\nchmod 666 %s\n\n%s\n", realname,
		"to give yourself permission to access the CD-ROM device.");
				warned++;
			}
		}
		else if (errno != EIO)	/* defined at top */
		{
			perror(cd_device);
			exit(1);
		}

		/* No CD in drive. */
		return (1);
	}

	if (warned)
	{
		warned = 0;
		fprintf(stderr, "Thank you.\n");
	}

	/* Now fill in the relevant parts of the wm_drive structure. */
	fd = d->fd;

	/* Figure out the drive type, if possible */
	vendor[0] = model[0] = rev[0] = '\0';
	wm_scsi_get_drive_type(d, vendor, model, rev);
	*d = *(find_drive_struct(vendor, model, rev));
	d->fd = fd;

	(d->init)(d);

	return (0);
}

void
keep_cd_open() { }

#endif
