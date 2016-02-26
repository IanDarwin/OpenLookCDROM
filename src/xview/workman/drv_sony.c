/*
 * @(#)drv_sony.c	1.1	12/21/93
 *
 * Vendor-specific drive control routines for Sony CDU-8012 series.
 */
static char *ident = "@(#)drv_sony.c	1.1 12/21/93";

#include <stdio.h>
#include <errno.h>
#include "struct.h"

#define PAGE_AUDIO		0x0e

static int	sony_init(), sony_set_volume(), sony_get_volume();

extern int	min_volume, max_volume;

struct wm_drive sony_proto = {
	-1,			/* fd */
	"Sony",			/* vendor */
	"CDU-8012",		/* model */
	NULL,			/* aux */
	NULL,			/* daux */

	sony_init,		/* functions... */
	gen_get_trackcount,
	gen_get_cdlen,
	gen_get_trackinfo,
	gen_get_drive_status,
	sony_get_volume,
	sony_set_volume,
	gen_pause,
	gen_resume,
	gen_stop,
	gen_play,
	gen_eject
};

/*
 * Initialize the driver.
 */
static int
sony_init(d)
	struct wm_drive	*d;
{
	min_volume = 128;
	max_volume = 255;
}

/*
 * On the Sony CDU-8012 drive, the amount of sound coming out the jack
 * increases much faster toward the top end of the volume scale than it
 * does at the bottom.  To make up for this, we make the volume scale look
 * sort of logarithmic (actually an upside-down inverse square curve) so
 * that the volume value passed to the drive changes less and less as you
 * approach the maximum slider setting.  Additionally, only the top half
 * of the volume scale is valid; the bottom half is all silent.  The actual
 * formula looks like
 *
 *     max^2 - (max - vol)^2   max
 * v = --------------------- + ---
 *            max * 2           2
 *
 * Where "max" is the maximum value of the volume scale, usually 100.
 */
static int
scale_volume(vol, max)
	int	vol, max;
{
	vol = (max*max - (max - vol) * (max - vol)) / max;
	return ((vol + max) / 2);
}

/*
 * Given a value between min_volume and max_volume, return the standard-scale
 * volume value needed to achieve that hardware value.
 *
 * Rather than perform floating-point calculations to reverse the above
 * formula, we simply do a binary search of scale_volume()'s return values.
 */
static int
unscale_volume(cd_vol, max)
	int	cd_vol, max;
{
	int	vol = 0, top = max, bot = 0, scaled;

	cd_vol = (cd_vol * 100 + (max_volume - 1)) / max_volume;

	while (bot <= top)
	{
		vol = (top + bot) / 2;
		scaled = scale_volume(vol, max);
		if (cd_vol <= scaled)
			top = vol - 1;
		else
			bot = vol + 1;
	}
	
	/* Might have looked down too far for repeated scaled values */
	if (cd_vol < scaled)
		vol++;

	if (vol < 0)
		vol = 0;
	else if (vol > max)
		vol = max;

	return (vol);
}

/*
 * Get the volume.  Sun's CD-ROM driver doesn't support this operation, even
 * though their drive does.  Dumb.
 */
static int
sony_get_volume(d, left, right)
	struct wm_drive	*d;
	int		*left, *right;
{
	unsigned char	mode[16];

	/* Get the current audio parameters first. */
	if (wm_scsi_mode_sense(d, PAGE_AUDIO, mode))
		return (-1);

	*left = unscale_volume(mode[9], 100);
	*right = unscale_volume(mode[11], 100);

	return (0);
}

/*
 * Set the volume using the wacky scale outlined above.  The Sony drive
 * responds to the standard set-volume command.
 */
static int
sony_set_volume(d, left, right)
	struct wm_drive	*d;
	int		left, right;
{
	left = scale_volume(left, 100);
	right = scale_volume(right, 100);
	return (gen_set_volume(d, left, right));
}
