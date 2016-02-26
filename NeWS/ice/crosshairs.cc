/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

boolean crosshairs_drawn;
int crosshairs_x, crosshairs_y;

void
drawcrosshairs()
{
	int vx, vy;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	XDrawLine(dpy, pg_cpm, pg_xgc, crosshairs_x, 0, crosshairs_x, pg_pixheight-1);
	XDrawLine(dpy, pg_cpm, pg_xgc, 0, crosshairs_y, pg_pixwidth-1, crosshairs_y);
	XDrawLine(dpy, pg_cwin, pg_xgc, crosshairs_x-vx, 0, crosshairs_x-vx, pg_pixheight-1-vy);
	XDrawLine(dpy, pg_cwin, pg_xgc, 0, crosshairs_y-vy, pg_pixwidth-1-vx, crosshairs_y-vy);

	crosshairs_drawn= !crosshairs_drawn;
	return;
}
