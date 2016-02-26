#ifndef lint
static char rcsid[] =
    "@(#) $Header: rcons_font.c,v 1.5 91/10/25 20:43:34 torek Exp $ (LBL)";
#endif

#ifdef KERNEL
#include "sys/param.h"
#include "sys/kernel.h"
#include "sys/fbio.h"
#include "sys/device.h"
#include "machine/fbvar.h"
#else
#include <sys/types.h>
#include "myfbdevice.h"
#endif

#include "raster.h"

#ifdef FANCY_BSD_CURSOR
#include "bsdcursor.h"
#endif

void
rcons_font(fb)
	register struct fbdevice *fb;
{
	/* XXX really rather get this from the prom */
	fb->fb_font = raster_defaultfont();

#ifdef FANCY_BSD_CURSOR
	/* Fancy BSD cursor */
	fb->fb_cursor = &bsdcursor;
#endif

	/* Get distance to top and bottom of font from font origin */
	fb->fb_font_ascent = -(fb->fb_font->chars)['a'].homey;
}
