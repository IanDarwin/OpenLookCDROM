/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);

int
copy_grobj(Grobj *a, Grobj *b)
{
	float fx, fy, h, v, r;
	int ix, iy, dtk;
	Path *clip;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;

	a->getloc(&fx, &fy, &ix, &iy);
	a->getscale(&h, &v);
	r= a->getrotation();
	clip= a->getclip();
	a->getdtk(&dtk, &rdtk, &gdtk, &bdtk);

	b->setfloc(fx, fy, pg_dpi);
	b->setscale(h, v);
	b->setrotation(r);
	b->setclip(clip);
	(void) b->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	b->setdtkpix(dtkpix);

	return GROBJ_SUCCESS;
}
