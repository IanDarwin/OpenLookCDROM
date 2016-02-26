/*
 * @(#)plat_linux.c	1.8	5/14/94
 *
 * Linux-specific drive control routines.  Very similar to the Sun module.
 */
static char *ident = "@(#)plat_linux.c	1.8 5/14/94";

#ifdef linux

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <linux/cdrom.h>

#include "struct.h"

#define DEFAULT_CD_DEVICE	"/dev/sr0"

#define max(a,b) ((a) > (b) ? (a) : (b))
 
#ifdef LINUX_SCSI_PASSTHROUGH
# define SCSI_IOCTL_SEND_COMMAND 1
#endif

void *malloc();
char *strchr();

int	min_volume = 128;
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
	struct cdrom_tochdr	hdr;

	if (ioctl(d->fd, CDROMREADTOCHDR, &hdr))
		return (-1);
	
	*tracks = hdr.cdth_trk1;
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
	struct cdrom_tocentry	entry;

	entry.cdte_track = track;
	entry.cdte_format = CDROM_MSF;

	if (ioctl(d->fd, CDROMREADTOCENTRY, &entry))
		return (-1);
	
	*startframe =	entry.cdte_addr.msf.minute * 60 * 75 +
			entry.cdte_addr.msf.second * 75 +
			entry.cdte_addr.msf.frame;
	*data = entry.cdte_ctrl & CDROM_DATA_TRACK ? 1 : 0;
	
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

	return (gen_get_trackinfo(d, CDROM_LEADOUT, &tmp, frames));
}


/* Alarm signal handler. */
static void do_nothing(x) int x; { x++; }

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
	struct cdrom_subchnl		sc;
	struct itimerval		old_timer, new_timer;

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

	sc.cdsc_format = CDROM_MSF;

	if (ioctl(d->fd, CDROMSUBCHNL, &sc))
		return (0);

	switch (sc.cdsc_audiostatus) {
	case CDROM_AUDIO_PLAY:
		*mode = PLAYING;
		*track = sc.cdsc_trk;
		*index = sc.cdsc_ind;
		*pos = sc.cdsc_absaddr.msf.minute * 60 * 75 +
			sc.cdsc_absaddr.msf.second * 75 +
			sc.cdsc_absaddr.msf.frame;
		break;

	case CDROM_AUDIO_PAUSED:
	case CDROM_AUDIO_NO_STATUS:
	case CDROM_AUDIO_INVALID: /**/
		if (oldmode == PLAYING || oldmode == PAUSED)
		{
			*mode = PAUSED;
			*track = sc.cdsc_trk;
			*index = sc.cdsc_ind;
			*pos = sc.cdsc_absaddr.msf.minute * 60 * 75 +
				sc.cdsc_absaddr.msf.second * 75 +
				sc.cdsc_absaddr.msf.frame;
		}
		else
			*mode = STOPPED;
		break;

	case CDROM_AUDIO_COMPLETED:
		*mode = TRACK_DONE; /* waiting for next track. */
		break;

	default:
		*mode = UNKNOWN;
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
	return ((max * max - (max - vol) * (max - vol)) *
		(max_volume - min_volume) / (max * max) + min_volume);
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
	struct cdrom_volctrl v;

/* Adjust the volume to make up for the CD-ROM drive's weirdness. */
	left = scale_volume(left, 100);
	right = scale_volume(right, 100);

	v.channel0 = v.channel2 = left < 0 ? 0 : left > 255 ? 255 : left;
	v.channel1 = v.channel3 = right < 0 ? 0 : right > 255 ? 255 : right;

	return (ioctl(d->fd, CDROMVOLCTRL, &v));
}

/*
 * Pause the CD.
 */
int
gen_pause(d)
	struct wm_drive	*d;
{
	return (ioctl(d->fd, CDROMPAUSE));
}

/*
 * Resume playing the CD (assuming it was paused.)
 */
int
gen_resume(d)
	struct wm_drive	*d;
{
	return (ioctl(d->fd, CDROMRESUME));
}

/*
 * Stop the CD.
 */
int
gen_stop(d)
	struct wm_drive *d;
{
	return (ioctl(d->fd, CDROMSTOP));
}

/*
 * Play the CD from one position to another (both in frames.)
 */
int
gen_play(d, start, end)
	struct wm_drive	*d;
	int		start, end;
{
	struct cdrom_msf		msf;

	msf.cdmsf_min0 = start / (60*75);
	msf.cdmsf_sec0 = (start % (60*75)) / 75;
	msf.cdmsf_frame0 = start % 75;
	msf.cdmsf_min1 = end / (60*75);
	msf.cdmsf_sec1 = (end % (60*75)) / 75;
	msf.cdmsf_frame1 = end % 75;

	if (ioctl(d->fd, CDROMSTART))
		return (-1);
	if (ioctl(d->fd, CDROMPLAYMSF, &msf))
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
	struct stat	stbuf;
	struct ustat	ust;

	if (fstat(d->fd, &stbuf) != 0)
		return (-2);

	/* Is this a mounted filesystem? */
	if (ustat(stbuf.st_rdev, &ust) == 0)
		return (-3);

	if (ioctl(d->fd, CDROMEJECT))
		return (-1);

	return (0);
}

/*
 * Keep the CD open all the time.
 */
void
keep_cd_open()
{
	int	fd;
	struct flock	fl;
	extern	end;

	for (fd = 0; fd < 256; fd++)
		close(fd);

	if (fork())
		exit(0);

	if ((fd = open("/tmp/cd.lock", O_RDWR | O_CREAT, 0666)) < 0)
		exit(0);
	fl.l_type = F_WRLCK;
	fl.l_whence = 0;
	fl.l_start = 0;
	fl.l_len = 0;
	if (fcntl(fd, F_SETLK, &fl) < 0)
		exit(0);

	if (open(cd_device, 0) >= 0)
	{
		brk(&end);
		pause();
	}

	exit(0);
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
#ifdef LINUX_SCSI_PASSTHROUGH

        char *cmd;
	int cmdsize;

	cmdsize = 2 * sizeof(int);
	if (retbuf)
	  {
	    if (getreply) cmdsize += max(cdblen, retbuflen);
	    else cmdsize += (cdblen + retbuflen);
	  }
	else cmdsize += cdblen;
	  
	cmd = malloc(cmdsize);
	if (cmd == NULL)
	  return (-1);

	((int*)cmd)[0] = cdblen + ((retbuf && !getreply) ? retbuflen : 0);
	((int*)cmd)[1] = ((retbuf && getreply) ? retbuflen : 0);

	memcpy(cmd + 2*sizeof(int), cdb, cdblen);
	if (retbuf && !getreply)
	  memcpy(cmd + 2*sizeof(int) + cdblen, retbuf, retbuflen);
	
	if (ioctl(d->fd, SCSI_IOCTL_SEND_COMMAND, cmd))
	  {
	    free(cmd);
	    return (-1);
	  }
	
	if (retbuf && getreply)
	  memcpy(retbuf, cmd + 2*sizeof(int), retbuflen);

	free(cmd);
	return 0;

#else
	return (-1);
#endif
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

#ifdef LINUX_SCSI_PASSTHROUGH
	char vendor[9], model[17], rev[5];
#endif
 
	if (cd_device == NULL)
		cd_device = DEFAULT_CD_DEVICE;

	if (d->fd >= 0)		/* Device already open? */
		return (0);
	
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
		else if (errno != ENXIO)
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

#ifdef LINUX_SCSI_PASSTHROUGH
  
	/* Can we figure out the drive type? */
	vendor[0] = model[0] = rev[0] = '\0';
	wm_scsi_get_drive_type(d, vendor, model, rev);

	*d = *(find_drive_struct(vendor, model, rev));
	about_set_drivetype(d->vendor, d->model, rev);

#else
 
	*d = *(find_drive_struct("", "", ""));
 
#endif

	d->fd = fd;

	(d->init)(d);

	return (0);
}

#endif /* linux */
