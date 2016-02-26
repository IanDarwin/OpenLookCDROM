/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <rasterfile.h>
#include <pixrect/pixrect_hs.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Raster.h"

boolean rasoutline_drawn;
int ras_x, ras_y;

void
ras_drawoutline(Raster *ras)
{
	int vx, vy;
	int w, h, d;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	ras->getsize(&w, &h, &d);
	if ((w <= 0) || (h <= 0))
		;
	else if (w == 1) {
		XDrawLine(dpy, pg_cpm, pg_xgc, ras_x, ras_y, ras_x, ras_y+h-1);
		XDrawLine(dpy, pg_cwin, pg_xgc, ras_x-vx, ras_y-vy, ras_x-vx, ras_y+h-1-vy);
	}
	else if (h == 1) {
		XDrawLine(dpy, pg_cpm, pg_xgc, ras_x, ras_y, ras_x+w-1, ras_y);
		XDrawLine(dpy, pg_cwin, pg_xgc, ras_x-vx, ras_y-vy, ras_x+w-1-vx, ras_y-vy);
	}
	else {
		XDrawLine(dpy, pg_cpm, pg_xgc, ras_x, ras_y, ras_x+w-1, ras_y);
		XDrawLine(dpy, pg_cwin, pg_xgc, ras_x-vx, ras_y-vy, ras_x+w-1-vx, ras_y-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, ras_x+w-1, ras_y+1, ras_x+w-1, ras_y+h-2);
		XDrawLine(dpy, pg_cwin, pg_xgc, ras_x+w-1-vx, ras_y+1-vy, ras_x+w-1-vx, ras_y+h-2-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, ras_x, ras_y+h-1, ras_x+w-1, ras_y+h-1);
		XDrawLine(dpy, pg_cwin, pg_xgc, ras_x-vx, ras_y+h-1-vy, ras_x+w-1-vx, ras_y+h-1-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, ras_x, ras_y+1, ras_x, ras_y+h-2);
		XDrawLine(dpy, pg_cwin, pg_xgc, ras_x-vx, ras_y+1-vy, ras_x-vx, ras_y+h-2-vy);
	}
	rasoutline_drawn= !rasoutline_drawn;
	return;
}
