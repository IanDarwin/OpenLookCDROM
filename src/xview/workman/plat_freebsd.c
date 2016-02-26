/*
 * plat_freebsd.c
 *
 * FreeBSD-specific drive control routines.
 *
 * Todd Pfaff, 3/20/94
 *
 */
static char *ident = "@(#)plat_freebsd.c	1.2 2/20/95";

#if defined(__FreeBSD__) || defined(__NetBSD__)

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/mount.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/cdio.h>
#include <sys/scsiio.h>
#ifdef __NetBSD__
#define MSF_MINUTES 1
#define MSF_SECONDS 2
#define MSF_FRAMES 3
#include "/sys/scsi/scsi_all.h"
#include "/sys/scsi/scsi_cd.h"
#else
#include <scsi/scsi_all.h>
#include <scsi/scsi_cd.h>
#endif

#include "struct.h"

#define DEFAULT_CD_DEVICE       "/dev/rcd0d"

void *malloc();

int	min_volume = 10;
int	max_volume = 255;

extern char	*cd_device;


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
	struct ioc_toc_header	hdr;

	if (ioctl(d->fd, CDIOREADTOCHEADER, &hdr) == -1)
		return (-1);

	*tracks = hdr.ending_track - hdr.starting_track + 1;

	return (0);
}

/*
 * Get the start time and mode (data or audio) of a track.
 *
 * XXX - this should get cached, but that means keeping track of ejects.
 */
int
gen_get_trackinfo(d, track, data, startframe)
	struct wm_drive	*d;
	int		track, *data, *startframe;
{
	struct ioc_read_toc_entry	toc;
	struct cd_toc_entry		toc_buffer;

	bzero((char *)&toc_buffer, sizeof(toc_buffer));
	toc.address_format = CD_MSF_FORMAT;
	toc.starting_track = track;
	toc.data_len = sizeof(toc_buffer);
	toc.data = &toc_buffer;

	if (ioctl(d->fd, CDIOREADTOCENTRYS, &toc))
		return (-1);

	*data = ((toc_buffer.control & 0x4) != 0);

#ifdef __NetBSD__
	*startframe = toc_buffer.addr[MSF_MINUTES]*60*75 +
		toc_buffer.addr[MSF_SECONDS] * 75 +
		toc_buffer.addr[MSF_FRAMES];
#else
	*startframe = toc_buffer.addr.msf.minute*60*75 +
		toc_buffer.addr.msf.second * 75 +
		toc_buffer.addr.msf.frame;
#endif

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
	struct ioc_toc_header		hdr;
	int status;

#define LEADOUT 0xaa			/* see scsi.c.  what a hack! */
	return gen_get_trackinfo(d, LEADOUT, &tmp, frames);
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
	struct ioc_read_subchannel	sc;
	struct cd_sub_channel_info	scd;

	/* If we can't get status, the CD is ejected, so default to that. */
	*mode = EJECTED;

	sc.address_format	= CD_MSF_FORMAT;
	sc.data_format		= CD_CURRENT_POSITION;
	sc.track		= 0;
	sc.data_len		= sizeof(scd);
	sc.data			= (struct cd_sub_channel_info *)&scd;

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

	if (ioctl(d->fd, CDIOCREADSUBCHANNEL, &sc)) {
#ifdef __NetBSD__
	    /* we need to release the device so the kernel will notice
	       reloaded media */
	    (void) close(d->fd);
	    d->fd = -1;
#endif
		return (0);	/* ejected */
	}

	switch (scd.header.audio_status) {
	case CD_AS_PLAY_IN_PROGRESS:
		*mode = PLAYING;
dopos:
#ifdef __NetBSD__
		*pos = scd.what.position.absaddr[MSF_MINUTES] * 60 * 75 +
			scd.what.position.absaddr[MSF_SECONDS] * 75 +
			scd.what.position.absaddr[MSF_FRAMES];
#else
		*pos = scd.what.position.absaddr.msf.minute * 60 * 75 +
			scd.what.position.absaddr.msf.second * 75 +
			scd.what.position.absaddr.msf.frame;
#endif
		*track = scd.what.position.track_number;
		*index = scd.what.position.index_number;
		break;

	case CD_AS_PLAY_PAUSED:
		if (oldmode == PLAYING || oldmode == PAUSED)
		{
			*mode = PAUSED;
			goto dopos;
		}
		else
			*mode = STOPPED;
		break;

	case CD_AS_PLAY_COMPLETED:
		*mode = TRACK_DONE; /* waiting for next track. */
		break;

	case CD_AS_NO_STATUS:
	case 0:
		*mode = STOPPED;
		break;
	}

	return (0);
}

/*
 * scale_volume(vol, max)
 *
 * Return a volume value suitable for passing to the CD-ROM drive.  "vol"
 * is a volume slider setting; "max" is the slider's maximum value.
 *
 * On Sun and DEC CD-ROM drives, the amount of sound coming out the jack
 * increases much faster toward the top end of the volume scale than it
 * does at the bottom.  To make up for this, we make the volume scale look
 * sort of logarithmic (actually an upside-down inverse square curve) so
 * that the volume value passed to the drive changes less and less as you
 * approach the maximum slider setting.  The actual formula looks like
 *
 *     (max^2 - (max - vol)^2) * (max_volume - min_volume)
 * v = --------------------------------------------------- + min_volume
 *                           max^2
 *
 * If your system's volume settings aren't broken in this way, something
 * like the following should work:
 *
 *	return ((vol * (max_volume - min_volume)) / max + min_volume);
 */
static int
scale_volume(vol, max)
	int	vol, max;
{
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
	struct ioc_vol vol;

	if (left < 0)	/* don't laugh, I saw this happen once! */
	    left = 0;
	if (right < 0)
	    right = 0;
	left = scale_volume(left, 100);
	right = scale_volume(right, 100);

	bzero((char *)&vol, sizeof(vol));

	vol.vol[LEFT_PORT] = left;
	vol.vol[RIGHT_PORT] = right;

	if (ioctl(d->fd, CDIOCSETVOL, &vol))
		return (-1);

	return (0);
}

/*
 * Pause the CD.
 */
int
gen_pause(d)
	struct wm_drive	*d;
{
	return (ioctl(d->fd, CDIOCPAUSE));
}

/*
 * Resume playing the CD (assuming it was paused.)
 */
int
gen_resume(d)
	struct wm_drive	*d;
{
	return (ioctl(d->fd, CDIOCRESUME));
}

/*
 * Stop the CD.
 */
int
gen_stop(d)
	struct wm_drive *d;
{
	return (ioctl(d->fd, CDIOCSTOP));
}

/*
 * Play the CD from one position to another (both in frames.)
 */
int
gen_play(d, start, end)
	struct wm_drive	*d;
	int		start, end;
{
	struct ioc_play_msf	msf;

	msf.start_m	= start / (60*75);
	msf.start_s	= (start % (60*75)) / 75;
	msf.start_f	= start % 75;
	msf.end_m	= end / (60*75);
	msf.end_s	= (end % (60*75)) / 75;
	msf.end_f	= end % 75;

	if (ioctl(d->fd, CDIOCSTART))
		return (-1);

	if (ioctl(d->fd, CDIOCPLAYMSF, &msf))
		return (-2);

	return (0);
}

/*
 * Eject the current CD, if there is one.
 */
int
gen_eject(d)
	struct wm_drive	*d;
{
	/* On some systems, we can check to see if the CD is mounted. */
	struct stat	stbuf;
	struct statfs	buf;
	int rval;

	if (fstat(d->fd, &stbuf) != 0)
		return (-2);

	/* Is this a mounted filesystem? */
	if (fstatfs(stbuf.st_rdev, &buf) == 0)
		return (-3);

#ifdef __NetBSD__
	rval = ioctl(d->fd, CDIOCALLOW);
	if (rval == 0)
#endif
	    rval = ioctl(d->fd, CDIOCEJECT);
#ifdef __NetBSD__
	if (rval == 0)
	    rval = ioctl(d->fd, CDIOCPREVENT);
#endif
	return rval;
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
	struct ioc_vol vol;

	if (d->fd >= 0)
	{
		bzero((char *)&vol, sizeof(vol));

		if (ioctl(d->fd, CDIOCGETVOL, &vol))
			*left = *right = -1;
		else
		{
			*left = unscale_volume(vol.vol[LEFT_PORT], 100);
			*right = unscale_volume(vol.vol[RIGHT_PORT], 100);
		}
	}
	else
		*left = *right = -1;

	return (0);
}


/*
 * Send an arbitrary SCSI command to a device.
 *
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
	char vendor[9], model[17], rev[5];

	if (d->fd >= 0)		/* Device already open? */
		return (0);

	if (cd_device == NULL)
		cd_device = DEFAULT_CD_DEVICE;

	d->fd = open(cd_device, 0);
	if (d->fd < 0)
	{
		if (errno == EACCES)
		{
			if (!warned)
			{
				fprintf(stderr,
		"As root, please run\n\nchmod 666 %s\n\n%s\n", cd_device,
		"to give yourself permission to access the CD-ROM device.");
				warned++;
			}
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

	vendor[0] = model[0] = rev[0] = '\0';

	*d = *(find_drive_struct(vendor, model, rev));

	(d->init)(d);

	d->fd = fd;

	return (0);
}

void
keep_cd_open() { }

#endif
