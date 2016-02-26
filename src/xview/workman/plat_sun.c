/*
 * @(#)plat_sun.c	1.11	04 Jun 1995
 *
 * Sun-specific drive control routines.
 */
static char *ident = "@(#)plat_sun.c	1.11 04 Jun 1995";

#ifdef sun

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <ustat.h>
#include <unistd.h>
#include <signal.h>
#ifdef solbourne
# include <mfg/dklabel.h>
# include <mfg/dkio.h>
# include <sys/unistd.h>
# include <dev/srvar.h>
#else /* A real Sun */
# ifdef SYSV
#  include <sys/cdio.h>
#  include <sys/scsi/impl/uscsi.h>
# else
#  include <sys/buf.h>
#  include <sun/dkio.h>
#  include <scsi/targets/srdef.h>
#  include <scsi/impl/uscsi.h>
#  include <scsi/generic/commands.h>
# endif
#endif

#include "struct.h"

void *malloc();
char *strchr();
char *realpath();

int	min_volume = 128;
int	max_volume = 255;

extern char	*cd_device;
extern int	intermittent_dev, keep_open;

/*
 * find_cdrom
 *
 * Determine the name of the CD-ROM device.
 *
 * Use the first of /vol/dev/aliases/cdrom0, /dev/rdsk/c0t6d0s2, and /dev/rsr0
 * that exists.  (Check for /vol/dev/aliases, not cdrom0, since it won't be
 * there if there's no CD in the drive.)  This is done so a single SunOS 4.x
 * binary can be used on any 4.x or higher Sun system.
 */
void
find_cdrom()
{
	if (access("/vol/dev/aliases", X_OK) == 0)
	{
		/* Volume manager.  Device might not be there. */
		intermittent_dev = 1;
		cd_device = "/vol/dev/aliases/cdrom0";
	}
	else if (access("/dev/rdsk/c0t6d0s2", F_OK) == 0)
	{
		/* Solaris 2.x w/o volume manager.  Run keep_cd_open(). */
		keep_open = 1;
		cd_device = "/dev/rdsk/c0t6d0s2";
	}
	else if (access("/dev/rsr0", F_OK) == 0)
		cd_device = "/dev/rsr0";
	else
	{
		fprintf(stderr, "Couldn't find a CD device!\n");
		exit(1);
	}
}

/*
 * Initialize the drive.  A no-op for the generic driver.
 */
int
gen_init(d)
	struct wm_drive	*d;
{
	codec_init();
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
	struct sigaction		old_sig, new_sig;

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

	/*
	 * Solaris 2.2 hangs on this ioctl if someone else ejects the CD.
	 * So we schedule a signal to break out of the hang if the call
	 * takes an unreasonable amount of time.  The signal handler and
	 * timer are restored immediately to avoid interfering with XView.
	 */
	if (intermittent_dev)
	{
		/*
		 * First clear out the timer so XView's signal doesn't happen
		 * while we're diddling with the signal handler.
		 */
		timerclear(&new_timer.it_interval);
		timerclear(&new_timer.it_value);
		setitimer(ITIMER_REAL, &new_timer, &old_timer);

		/*
		 * Now install the no-op signal handler.
		 */
		new_sig.sa_handler = do_nothing;
		memset(&new_sig.sa_mask, 0, sizeof(new_sig.sa_mask));
		new_sig.sa_flags = 0;
		if (sigaction(SIGALRM, &new_sig, &old_sig))
			perror("sigaction");

		/*
		 * And finally, set the timer.
		 */
		new_timer.it_value.tv_sec = 2;
		setitimer(ITIMER_REAL, &new_timer, NULL);
	}

	sc.cdsc_format = CDROM_MSF;

	if (ioctl(d->fd, CDROMSUBCHNL, &sc))
	{
		if (intermittent_dev)
		{
			sigaction(SIGALRM, &old_sig, NULL);
			setitimer(ITIMER_REAL, &old_timer, NULL);

			/* If the device can disappear, let it do so. */
			close(d->fd);
			d->fd = -1;
		}

		return (0);
	}

	if (intermittent_dev)
	{
		sigaction(SIGALRM, &old_sig, NULL);
		setitimer(ITIMER_REAL, &old_timer, NULL);
	}

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
	case CDROM_AUDIO_INVALID:
	case CDROM_AUDIO_NO_STATUS:
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

	/* CD ejected manually during play. */
	case CDROM_AUDIO_ERROR:
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
 * Set the volume level for the left and right channels.  Their values
 * range from 0 to 100.
 */
int
gen_set_volume(d, left, right)
	struct wm_drive	*d;
	int		left, right;
{
	struct cdrom_volctrl v;

	left = (left * (max_volume - min_volume)) / 100 + min_volume;
	right = (right * (max_volume - min_volume)) / 100 + min_volume;

	v.channel0 = left < 0 ? 0 : left > 255 ? 255 : left;
	v.channel1 = right < 0 ? 0 : right > 255 ? 255 : right;

	return (ioctl(d->fd, CDROMVOLCTRL, &v));
}

/*
 * Pause the CD.
 */
int
gen_pause(d)
	struct wm_drive	*d;
{
	codec_stop();
	return (ioctl(d->fd, CDROMPAUSE));
}

/*
 * Resume playing the CD (assuming it was paused.)
 */
int
gen_resume(d)
	struct wm_drive	*d;
{
	codec_start();
	return (ioctl(d->fd, CDROMRESUME));
}

/*
 * Stop the CD.
 */
int
gen_stop(d)
	struct wm_drive *d;
{
	codec_stop();
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

	codec_start();
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

	/* Close the device if it needs to vanish. */
	if (intermittent_dev)
	{
		close(d->fd);
		d->fd = -1;
	}

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
	*left = *right = -1;

	return (wm_scsi2_get_volume(d, left, right));
}

#ifndef solbourne
/*
 * Send an arbitrary SCSI command out the bus and optionally wait for
 * a reply if "retbuf" isn't NULL.
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
	char			x;
	struct uscsi_cmd	cmd;

	memset(&cmd, 0, sizeof(cmd));
	cmd.uscsi_cdb = (void *) cdb;
	cmd.uscsi_cdblen = cdblen;
	cmd.uscsi_bufaddr = retbuf ? retbuf : (void *)&x;
	cmd.uscsi_buflen = retbuf ? retbuflen : 0;
	cmd.uscsi_flags = USCSI_ISOLATE | USCSI_SILENT;
	if (getreply)
		cmd.uscsi_flags |= USCSI_READ;
	
	if (ioctl(d->fd, USCSICMD, &cmd))
		return (-1);
	
	if (cmd.uscsi_status)
		return (-1);
	
	return (0);
}
#else

int wm_scsi() { return (-1); }

#endif

/*
 * Open the CD device and figure out what kind of drive is attached.
 */
int
wmcd_open(d)
	struct wm_drive	*d;
{
	int		fd;
	static int	warned = 0;
	char		vendor[32], model[32], rev[32];

	if (cd_device == NULL)
		find_cdrom();

	if (d->fd >= 0)		/* Device already open? */
		return (0);
	
	d->fd = open(cd_device, 0);
	if (d->fd < 0)
	{
		/* Solaris 2.2 volume manager moves links around */
		if (errno == ENOENT && intermittent_dev)
			return (1);

		if (errno == EACCES)
		{
			if (!warned)
			{
				char	realname[MAXPATHLEN];

				if (realpath(cd_device, realname) == NULL)
				{
					perror("realpath");
					return (-1);
				}

				fprintf(stderr,
		"As root, please run\n\nchmod 666 %s\n\n%s\n", realname,
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

	/* Can we figure out the drive type? */
	vendor[0] = model[0] = rev[0] = '\0';
	if (wm_scsi_get_drive_type(d, vendor, model, rev))
		if (errno == EPERM)
		{
			/*
			 * Solaris 2.4 seems to refuse to do USCSICMD ioctls
			 * when not running as root.  SunOS 4.x allows it
			 * as an unprivileged user, though.
			 */
			fprintf(stderr,
"Warning: WorkMan can't adapt itself to your drive unless it runs as root.\n");
		}
		else
		{
			fprintf(stderr,
"Warning: WorkMan couldn't determine drive type (not a SCSI drive?)\n");
		}

	*d = *(find_drive_struct(vendor, model, rev));
	about_set_drivetype(d->vendor, d->model, rev);
	d->fd = fd;

	(d->init)(d);

	return (0);
}

/*
 * The following code activates the internal CD audio passthrough on
 * SPARCstation 5 systems (and possibly others.)
 *
 * Thanks to <stevep@ctc.ih.att.com>.
 */

#if !defined(SYSV) || !defined(CODEC)
/* not solaris, so don't do anything real */
codec_init() { return 0; }
codec_start() { return 0; }
codec_stop() { return 0; }
#else

#include <sys/ioctl.h>
#include <sys/audioio.h>
#include <stdlib.h>

#ifndef AUDIO_INTERNAL_CD_IN
#define AUDIO_INTERNAL_CD_IN	0x4
#endif

static char* devname = 0;
static char* ctlname = 0;
static int ctl_fd = -1;

codec_init()
{
register int i;
char* ctlname;
audio_info_t foo;
audio_device_t aud_dev;

    if (!(devname = getenv("AUDIODEV"))) devname = "/dev/audio";
    ctlname = strcat(strcpy(malloc(strlen(devname) + 4), devname), "ctl");
    if ((ctl_fd = open(ctlname, O_WRONLY, 0)) < 0) {
	perror(ctlname);
	return -1;
    }
    if (ioctl(ctl_fd, AUDIO_GETDEV, &aud_dev) < 0) {
	close(ctl_fd);
	ctl_fd = -1;
	return -1;
    }
    if (strcmp(aud_dev.name, "SUNW,CS4231")) {
	close(ctl_fd);
	ctl_fd = -1;
	return 0;					/* but it's okay */
    }
    AUDIO_INITINFO(&foo);
    foo.record.port = AUDIO_INTERNAL_CD_IN;
    foo.monitor_gain = AUDIO_MAX_GAIN;
    ioctl(ctl_fd, AUDIO_SETINFO, &foo);
    return 0;
}

static
kick_codec()
{
audio_info_t foo;
int dev_fd;
int retval = 0;

    if ((dev_fd = open(devname, O_WRONLY, 0)) < 0) {
	perror(devname);
	return -1;
    }
    AUDIO_INITINFO(&foo);
    foo.play.sample_rate = 44100;
    foo.play.channels = 2;
    foo.play.precision = 16;
    foo.play.encoding = AUDIO_ENCODING_LINEAR;
    if ((retval = ioctl(dev_fd, AUDIO_SETINFO, &foo)) < 0) perror(devname);
    close(dev_fd);
    return retval;
}

codec_start()
{
audio_info_t foo;

    if (ctl_fd < 0) return 0;
    if (ioctl(ctl_fd, AUDIO_GETINFO, &foo) < 0) return -1;

    if (foo.play.channels != 2) return kick_codec();
    if (foo.play.encoding != AUDIO_ENCODING_LINEAR) return kick_codec();
    if (foo.play.precision != 16) return kick_codec();
    if (foo.play.sample_rate != 44100) return kick_codec();
    if (foo.record.channels != 2) return kick_codec();
    if (foo.record.encoding != AUDIO_ENCODING_LINEAR) return kick_codec();
    if (foo.record.precision != 16) return kick_codec();
    if (foo.record.sample_rate != 44100) return kick_codec();
    if (foo.monitor_gain != AUDIO_MAX_GAIN) return kick_codec();
    if (foo.record.port != AUDIO_INTERNAL_CD_IN) return kick_codec();

    return 0;
}

codec_stop() { return 0; }

#endif
#endif
