#include "header.h"

/*
 *  Magnifying glass		(for Sun-3 workstations)
 *
 *  Copyright 1986, 1987 by Scott Schwartz
 *  This program may be copied with the provision that this copyright 
 *  message remains, and that further distribution of this code and it's 
 *  derivatives is not subject to additional restrictions.
 * 
 *  Lots of changes, but no Copyright, by Mark Weiser.
 * 
 *  compile with -lsuntool -lsunwindow -lpixrect
 *
 */


/* global vars */
static struct pixrect	*tmpdst=NULL;


quit_mag()
{
 MY_pr_destroy(tmpdst);
}


init_mag()
{
 MY_pr_destroy(tmpdst);
 tmpdst = my_mem_create(SCREEN_MAX_X, SCREEN_MAX_Y, image_depth);
}


/*
 * pw_mag copies a magnified view of spr to dpw using pixel replication.
 * the arguments are the same as those to the pw_rop library call, except
 * that magnification is passed instead of raster-op.
 */
void pw_mag(dpw, dx, dy, w, h, mag, spr, sx, sy)
	Pixwin *dpw;	/* destination pixwin */
	int dx, dy;  	/* destination x,y */
	int w, h;	/* width and height of block to copy */
	int mag; 	/* magnification */
	struct pixrect *spr;	/* source pixrect */
	int sx,sy;	/* location in source to copy from */
{
	/* locals */
	register short jmax = h/mag + 1, imax = w/mag + 1;
	register short x, y, delta;

	struct pixrect r;	/* holds the size of the drawing region when */
			/* gaining access to the screen */
	r.pr_size.x = w;
	r.pr_size.y = h;

	for (x = 0; x < imax; x += 1) {
		for (delta = 0; delta < mag; delta += 1) {
			pr_rop(tmpdst, x*mag+delta, 0, 1, jmax, PIX_SRC|PIX_DONTCLIP, spr, sx+x, sy);
		}
	}
	for (y = jmax; y >= 0; y -= 1) {
		for (delta = 0; delta < mag; delta += 1) {
			pr_rop(tmpdst, 0, y*mag+delta, w, 1, PIX_SRC|PIX_DONTCLIP, tmpdst, 0, y);
		}
	}

	/* draw */
	pw_rop(dpw, dx, dy, w, h, PIX_SRC, tmpdst, 0, 0);
}
