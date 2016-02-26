/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <NeWS/psio.h>
#include <NeWS/psmacros.h>

#include "Path.h"

extern "C" {
int		psio_flushbuf(char, PSFILE *);
int		psio_write(char *, int, int, PSFILE *);
char *		strcpy(char *, char *);
int		strlen(char *);
}

Path::Path()
{
	name= (char *) 0;
	vertices= 0;
	xvertices= yvertices= (float *) 0;
	closure= PATH_OPEN;
	visibility= PATH_INVISIBLE;
	references= 0;
	display= (Display *) 0;
	fullgc= None;
	fgpix= 0;
	redfg= greenfg= bluefg= 0;
}

Path::Path(Dlnk **listhd, char *nm, Display *dpy, int d)
{
	Pixmap pm;
	XGCValues gcv;

	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	if (nm == (char *) 0)
		name= (char *) 0;
	else if (strlen(nm) == 0)
		name= (char *) 0;
	else {
		if ((name= new char[strlen(nm)+1]) != (char *) 0)
			(void) strcpy(name, nm);
	}

	vertices= 0;
	xvertices= yvertices= (float *) 0;
	closure= PATH_OPEN;
	visibility= PATH_INVISIBLE;
	references= 0;
	display= dpy;
	fullgc= None;
	depth= d;
	fgpix= 1;
	redfg= greenfg= bluefg= 0;

	/* temporary pixmap is created just so we can create GCs */
	if ((pm= XCreatePixmap(display, DefaultRootWindow(display), (unsigned) 1, (unsigned) 1, (unsigned) depth)) == None) {
		delete name;
		name= (char *) 0;
		return;
	}

	gcv.function= GXcopy;
	gcv.foreground= fgpix;
	gcv.plane_mask= AllPlanes;
	if ((fullgc= XCreateGC(display, pm, (GCFunction | GCForeground | GCPlaneMask), &gcv)) == None) {
		delete name;
		name= (char *) 0;
		XFreePixmap(display, pm);
		return;
	}
	XFreePixmap(display, pm);
}

int
Path::setname(char *n)
{
	delete name;
	if (n == (char *) 0)
		name= (char *) 0;
	else if (strlen(n) == 0)
		name= (char *) 0;
	else {
		if ((name= new char[strlen(n)+1]) == (char *) 0)
			return PATH_FAILURE;
		(void) strcpy(name, n);
	}
	return PATH_SUCCESS;
}

int
Path::setvertices(int n, float *x, float *y)
{
	if (n < 0)
		return PATH_FAILURE;

	delete xvertices;
	delete yvertices;
	vertices= n;
	xvertices= x;
	yvertices= y;
	return PATH_SUCCESS;
}

int
Path::setclosure(int c)
{
	switch (c) {
	case PATH_OPEN:
	case PATH_CLOSED:
		closure= c;
		return PATH_SUCCESS;
	default:
		return PATH_FAILURE;
	}
}

int
Path::setvisibility(int v)
{
	switch (v) {
	case PATH_INVISIBLE:
	case PATH_VISIBLE:
		visibility= v;
		return PATH_SUCCESS;
	default:
		return PATH_FAILURE;
	}
}

int
Path::setreferences(int r)
{
	switch (r) {
	case PATH_REFINCR:
		references++;
		return PATH_SUCCESS;
	case PATH_REFDECR:
		references--;
		if (references < 0)
			references= 0;
		return PATH_SUCCESS;
	default:
		return PATH_FAILURE;
	}
}

int
Path::setdump(int d)
{
	switch (d) {
	case PATH_DUMP:
	case PATH_NODUMP:
		dump= d;
		return PATH_SUCCESS;
	default:
		return PATH_FAILURE;
	}
}

int
Path::setfg(int fg, unsigned long fgp, unsigned char r, unsigned char g, unsigned char b)
{
	Pixmap pm;
	XGCValues gcv;

	switch (fg) {
	case PATH_BLACKFG:
	case PATH_WHITEFG:
	case PATH_OTHERFG:
		break;
	default:
		return PATH_FAILURE;
	}

	foreground= fg;
	fgpix= fgp;
	redfg= r;
	greenfg= g;
	bluefg= b;

	if (fullgc != None)
		XFreeGC(display, fullgc);

	/* temporary pixmap is created just so we can create fullgc */
	if ((pm= XCreatePixmap(display, DefaultRootWindow(display), (unsigned) 1, (unsigned) 1, (unsigned) depth)) == None)
		return PATH_FAILURE;

	gcv.function= GXcopy;
	gcv.foreground= fgpix;
	gcv.plane_mask= AllPlanes;
	if ((fullgc= XCreateGC(display, pm, (GCFunction | GCForeground | GCPlaneMask), &gcv)) == None) {
		XFreePixmap(display, pm);
		return PATH_FAILURE;
	}
	XFreePixmap(display, pm);
	return PATH_SUCCESS;
}

void
Path::draw(int flags, FILE *fp)
{
	char psbuf[128];
	int i;

	if (fp == (FILE *) 0) {
		(void) strcpy(psbuf, "newpath\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		(void) sprintf(psbuf, "%6.4f %6.4f moveto\n", (float) (xvertices[0]*72.), (float) (yvertices[0]*72.));
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		for (i= 1; i < vertices; i++) {
			(void) sprintf(psbuf, "%6.4f %6.4f lineto\n", (float) (xvertices[i]*72.), (float) (yvertices[i]*72.));
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}

		if (flags & PATH_DRAWCLOSE) {
			(void) sprintf(psbuf, "closepath\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
		if (flags & PATH_DRAWCLIP) {
			(void) sprintf(psbuf, "clip\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}

		if (flags & PATH_DRAWSTROKE) {
			(void) sprintf(psbuf, "stroke\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
		else if (flags & PATH_DRAWFILL) {
			(void) sprintf(psbuf, "fill\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}

		if (!(flags & PATH_DRAWNONEWPATH)) {
			(void) sprintf(psbuf, "newpath\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
	}
	else {
		fprintf(fp, "newpath\n");
		fprintf(fp, "%6.4f %6.4f moveto\n", (float) (xvertices[0]*72.), (float) (yvertices[0]*72.));
		for (i= 1; i < vertices; i++)
			fprintf(fp, "%6.4f %6.4f lineto\n", (float) (xvertices[i]*72.), (float) (yvertices[i]*72.));

		if (flags & PATH_DRAWCLOSE)
			fprintf(fp, "closepath\n");
		if (flags & PATH_DRAWCLIP)
			fprintf(fp, "clip\n");

		if (flags & PATH_DRAWSTROKE)
			fprintf(fp, "stroke\n");
		else if (flags & PATH_DRAWFILL)
			fprintf(fp, "fill\n");

		if (!(flags & PATH_DRAWNONEWPATH))
			fprintf(fp, "newpath\n");
	}
	return;
}

void
Path::x11draw(int dpi, int h, Pixmap pm)
{
	int x0, x1, y0, y1, i;

	x0= (int) (xvertices[0]*dpi);
	y0= h-1-((int) (yvertices[0]*dpi));
	for (i= 1; i < vertices; i++, x0= x1, y0= y1) {
		x1= (int) (xvertices[i]*dpi);
		y1= h-1-((int) (yvertices[i]*dpi));
		XDrawLine(display, pm, fullgc, x0, y0, x1, y1);
	}
	if ((vertices > 1) && (closure == PATH_CLOSED)) {
		x1= (int) (xvertices[0]*dpi);
		y1= h-1-((int) (yvertices[0]*dpi));
		XDrawLine(display, pm, fullgc, x0, y0, x1, y1);
	}
	return;
}

int
Path::icedump(FILE *fp)
{
	int i;

	if (fp == (FILE *) 0)
		return PATH_FAILURE;

	fprintf(fp, "%%%%ICE-Path: Begin %s\n", name);

	fprintf(fp, "%%%%ICE-Path: Flags %1d %1d\n", closure, visibility);

	fprintf(fp, "%%%%ICE-Path: FG %1d %1d %1d %1d\n", foreground, (int) redfg, (int) greenfg, (int) bluefg);

	fprintf(fp, "%%%%ICE-Path: Vert %1d\n", vertices);
	for (i= 0; i < vertices; i++)
		fprintf(fp, "%%%%ICE-Path: Vert+ %6.4f %6.4f\n", xvertices[i], yvertices[i]);

	fprintf(fp, "%%%%ICE-Path: End\n");
	return PATH_SUCCESS;
}

Path::~Path()
{
	delete name;
	delete xvertices;
	delete yvertices;
}
