/*
 * @(#)drv_toshiba.c	1.6	03 Jun 1995
 *
 * Vendor-specific drive control routines for Toshiba XM-3401 series.
 */
static char *ident = "@(#)drv_toshiba.c	1.6 03 Jun 1995";

#include <stdio.h>
#include <errno.h>
#include "struct.h"

#define	SCMD_TOSH_EJECT		0xc4

static int	tosh_init(), tosh_eject(), tosh_set_volume(), tosh_get_volume();
int		wm_scsi2_get_volume(), wm_scsi2_set_volume();

struct wm_drive toshiba_proto = {
	-1,			/* fd */
	"Toshiba",		/* vendor */
	"",			/* model */
	NULL,			/* aux */
	NULL,			/* daux */

	tosh_init,		/* functions... */
	gen_get_trackcount,
	gen_get_cdlen,
	gen_get_trackinfo,
	gen_get_drive_status,
	tosh_get_volume,
	tosh_set_volume,
	gen_pause,
	gen_resume,
	gen_stop,
	gen_play,
	tosh_eject,
};

/*
 * Initialize the driver.
 */
static int
tosh_init(d)
	struct wm_drive	*d;
{
	extern int	min_volume;

	min_volume = 0;
}

/*
 * Send the Toshiba code to eject the CD.
 */
static int
tosh_eject(d)
	struct wm_drive *d;
{
	return (sendscsi(d, NULL, 0, 0, SCMD_TOSH_EJECT, 1, 0,0,0,0,0,0,0,0));
}

/*
 * Set the volume.  The low end of the scale is more sensitive than the high
 * end, so make up for that by transforming the volume parameters to a square
 * curve.
 */
static int
tosh_set_volume(d, left, right)
	struct wm_drive	*d;
	int		left, right;
{
	left = (left * left * left) / 10000;
	right = (right * right * right) / 10000;
	return (gen_set_volume(d, left, right));
}

/*
 * Undo the transformation above using a binary search (so no floating-point
 * math is required.)
 */
static int
unscale_volume(cd_vol, max)
	int	cd_vol, max;
{
	int	vol = 0, top = max, bot = 0, scaled;

	/*cd_vol = (cd_vol * 100 + (max_volume - 1)) / max_volume;*/

	while (bot <= top)
	{
		vol = (top + bot) / 2;
		scaled = (vol * vol) / max;
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
 * Get the volume.
 */
static int
tosh_get_volume(d, left, right)
	struct wm_drive	*d;
	int		*left, *right;
{
	int		status;

	status = gen_get_volume(d, left, right);
	if (status < 0)
		return (status);
	*left = unscale_volume(*left, 100);
	*right = unscale_volume(*right, 100);

	return (0);
}
