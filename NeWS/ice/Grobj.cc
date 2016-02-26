/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <X11/Xlib.h>

#include "Grobj.h"

extern "C" {
void		bcopy(char *, char *, int);
}

Grobj::Grobj()
{
	x= y= 0.;
	px= py= 0;
	type= GROBJ_UNDEFINED;
	hscale= vscale= 1.;
	rotation= 0.;
	clip= (Path *) 0;
	sequence= 0; 
	dtk= GROBJ_GLOBALDTK;
	reddtk= greendtk= bluedtk= (unsigned char) 255;
	dtkpix= (unsigned long) 0;
	sortid= 0;
	parent= rootparent= (Grobj *) 0;
	iotag= GROBJ_NULLIOTAG;
}

void
Grobj::setfloc(float fx, float fy, float dpi)
{
	x= fx;
	y= fy;
	px= (int) (dpi*x);
	py= (int) (dpi*y);
}

void
Grobj::setploc(int ix, int iy, float dpi)
{
	float ipp;

	ipp= 1./dpi;
	x= ipp*((float) ix);
	y= ipp*((float) iy);
	px= ix;
	py= iy;
}

int
Grobj::settype(int t)
{
	switch (t) {
	case GROBJ_PSDOC:
	case GROBJ_RASTER:
	case GROBJ_INTOBJ:
	case GROBJ_COMPOSITE:
		type= t;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

void
Grobj::setrotation(float r)
{
	rotation= r;
	while (rotation < 0.)
		rotation+= 360.;
	while (rotation >= 360.)
		rotation-= 360.;
}

void
Grobj::sortsequence(Grobj **list)
{
	Grobj *prev, *next, *currseq;
	unsigned char ucp[4], ucn[4];
	int id;

	if (*list == (Grobj *) 0)
		return;

	/* first pass -- put list in ascending sequence order */
	for (next= *list, prev= (Grobj *) 0; next != (Grobj *) 0; ) {
		int prevseq, nextseq;
		Grobj *g;

		/* initial condition */
		if (prev == (Grobj *) 0) {
			prev= next;
			prevseq= next->getsequence();
			next= (Grobj *) next->succ();
		}
		else {
			nextseq= next->getsequence();
			if (nextseq >= prevseq) {
				prevseq= nextseq;
				prev= next;
				next= (Grobj *) next->succ();
			}
			else {
				next->unlink();
				for (g= *list; g->getsequence() <= nextseq; g= (Grobj *) g->succ());
				next->link(g, DLNK_INSERT);
				next= (Grobj *) prev->succ();
			}
		}
	}
	*list= (Grobj *) prev->head();

	/* second pass -- ensure that all Psdocs and Intobjs
	   precede all Rasters with the same sequence number */
	for (next= currseq= *list; next != (Grobj *) 0; ) {
		prev= next;
		next= (Grobj *) next->succ();

		/* terminating condition */
		if (next == (Grobj *) 0) {
			*list= (Grobj *) prev->head();
			break;
		}

		/* change in sequence number */
		if (next->getsequence() > prev->getsequence()) {
			currseq= next;
			continue;
		}

		/* Raster preceding non-Raster */
		if ((prev->gettype() == GROBJ_RASTER) &&
		    (next->gettype() != GROBJ_RASTER)) {
			next->unlink();
			next->link(prev, DLNK_INSERT);
			if (currseq == prev)
				currseq= next;
			else
				next= currseq;
		}
	}

	/* third pass -- sort all Psdocs and Intobjs with the
	   same sequence number by dump transparency key */
	ucp[0]= ucn[0]= 0;
	for (next= currseq= *list; next != (Grobj *) 0; ) {
		int pt, nt, pdtk, ndtk;
		unsigned int ppix, npix;

		prev= next;
		next= (Grobj *) next->succ();

		/* terminating condition */
		if (next == (Grobj *) 0) {
			*list= (Grobj *) prev->head();
			break;
		}

		/* change in sequence number */
		if (next->getsequence() > prev->getsequence()) {
			currseq= next;
			continue;
		}

		/* two adjacent non-Rasters */
		if (((pt= prev->gettype()) != GROBJ_RASTER) &&
		    ((nt= next->gettype()) != GROBJ_RASTER)) {
			prev->getdtk(&pdtk, &ucp[3], &ucp[2], &ucp[1]);
			bcopy((char *) ucp, (char *) &ppix, sizeof(int));
			next->getdtk(&ndtk, &ucn[3], &ucn[2], &ucn[1]);
			bcopy((char *) ucn, (char *) &npix, sizeof(int));
			if (npix < ppix) {
				next->unlink();
				next->link(prev, DLNK_INSERT);
				if (currseq == prev)
					currseq= next;
				else
					next= currseq;
			}
		}
	}

	/* fourth pass -- set all sortids */
	for (id= 0, currseq= *list; currseq != (Grobj *) 0; id++, currseq= (Grobj *) currseq->succ())
		currseq->setsortid(id);

	return;
}

int
Grobj::setdtk(int d, unsigned char r, unsigned char g, unsigned char b)
{
	switch (d) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
	case GROBJ_OTHERDTK:
		dtk= d;
		reddtk= r;
		greendtk= g;
		bluedtk= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

void
Grobj::setparent(Grobj *p)
{
	parent= p;
	return;
}

void
Grobj::setrootparent(Grobj *r)
{
	rootparent= r;
	return;
}
