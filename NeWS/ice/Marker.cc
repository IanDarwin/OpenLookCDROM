/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <NeWS/psio.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "Marker.h"

extern "C" {
char *		gconvert(double, int, int, char *);
int		pprintf( ... );
int		ps_checkfor(PSFILE *, int, int);
int		ps_currenttag(PSFILE *, int, int *);
int		psio_flush(PSFILE *);
int		psio_flushbuf(char, PSFILE *);
int		psio_write(char *, int, int, PSFILE *);
char *		strcat(char *, char*);
char *		strcpy(char *, char *);
int		strlen(char *);
}

Marker::Marker()
{
	markertype= MARKER_GLOBALTYPE;
	mrktype= MARKER_SQUARE;
	size= MARKER_GLOBALSIZE;
	radius= 5.;
	boundarymode= MARKER_OPAQUEBNDM;
	boundarywd= MARKER_GLOBALBNDWIDTH;
	bndwidth= 2.;
	boundarycolor= MARKER_GLOBALBND;
	redbnd= greenbnd= bluebnd= 0;
	fillmode= MARKER_TRANSPARENTFILLM;
	fillcolor= MARKER_GLOBALFILL;
	redfill= greenfill= bluefill= 255;

	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_MARKER);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(0);
}

Marker::Marker(Dlnk **listhd, char *nm, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	(void) setname(nm);

	markertype= MARKER_GLOBALTYPE;
	mrktype= MARKER_SQUARE;
	size= MARKER_GLOBALSIZE;
	radius= 5.;
	boundarymode= MARKER_OPAQUEBNDM;
	boundarywd= MARKER_GLOBALBNDWIDTH;
	bndwidth= 2.;
	boundarycolor= MARKER_GLOBALBND;
	redbnd= greenbnd= bluebnd= 0;
	fillmode= MARKER_TRANSPARENTFILLM;
	fillcolor= MARKER_GLOBALFILL;
	redfill= greenfill= bluefill= 255;

	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_MARKER);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Marker::setmarkertype(int mt, int t)
{
	switch (mt) {
	case MARKER_GLOBALTYPE:
	case MARKER_SQUARE:
	case MARKER_TRIANGLE:
	case MARKER_CIRCLE:
	case MARKER_CROSS:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (t) {
	case MARKER_SQUARE:
	case MARKER_TRIANGLE:
	case MARKER_CIRCLE:
	case MARKER_CROSS:
		markertype= mt;
		mrktype= t;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Marker::setsize(int s, float r)
{
	switch (s) {
	case MARKER_GLOBALSIZE:
	case MARKER_OTHERSIZE:
		break;
	default:
		return GROBJ_FAILURE;
	}
	if (r <= 0.)
		return GROBJ_FAILURE;

	size= s;
	radius= r;
	return GROBJ_SUCCESS;
}

int
Marker::setboundary(int bm, int bw, float w, int bc, unsigned char r, unsigned char g, unsigned char b)
{
	switch (bm) {
	case MARKER_OPAQUEBNDM:
	case MARKER_TRANSPARENTBNDM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (bw) {
	case MARKER_GLOBALBNDWIDTH:
	case MARKER_OTHERBNDWIDTH:
		if (w < 0.)
			return GROBJ_FAILURE;
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (bc) {
	case MARKER_BLACKBND:
		boundarymode= bm;
		boundarywd= bw;
		bndwidth= w;
		boundarycolor= bc;
		redbnd= greenbnd= bluebnd= 0;
		return GROBJ_SUCCESS;
	case MARKER_WHITEBND:
		boundarymode= bm;
		boundarywd= bw;
		bndwidth= w;
		boundarycolor= bc;
		redbnd= greenbnd= bluebnd= 255;
		return GROBJ_SUCCESS;
	case MARKER_GLOBALBND:
	case MARKER_OTHERBND:
		boundarymode= bm;
		boundarywd= bw;
		bndwidth= w;
		boundarycolor= bc;
		redbnd= r;
		greenbnd= g;
		bluebnd= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Marker::setfill(int fm, int fc, unsigned char r, unsigned char g, unsigned char b)
{
	switch (fm) {
	case MARKER_TRANSPARENTFILLM:
	case MARKER_OPAQUEFILLM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (fc) {
	case MARKER_WHITEFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= greenfill= bluefill= 255;
		return GROBJ_SUCCESS;
	case MARKER_BLACKFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= greenfill= bluefill= 0;
		return GROBJ_SUCCESS;
	case MARKER_GLOBALFILL:
	case MARKER_OTHERFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= r;
		greenfill= g;
		bluefill= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Marker::draw(FILE *outfp)
{
	int tag, rendererr;
	float fx, fy, rot, hs, vs;
	int x, y, flags;
	int bnd, fill;
	float rb, gb, bb, rf, gf, bf;
	char psbuf[128];
	Path *cpth;

	if ((boundarymode != MARKER_OPAQUEBNDM) && (mrktype == MARKER_CROSS))
		return GROBJ_SUCCESS;
	else if ((boundarymode != MARKER_OPAQUEBNDM) && (fillmode != MARKER_OPAQUEFILLM))
		return GROBJ_SUCCESS;

	rendererr= 0;
	if (outfp == (FILE *) 0) {

		/* discard any unread NeWS tags */
		while (ps_check_PostScript_event() == 1)
			ps_read_PostScript_event(&tag);

		news_traperrors();
	}
	else
		fprintf(outfp, "save\n");

	if ((cpth= getclip()) != (Path *) 0) {
		flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
		cpth->draw(flags, outfp);
	}

	getloc(&fx, &fy, &x, &y);
	fx*= 72.;
	fy*= 72.;
	if ((fx != 0.) || (fy != 0.)) {

		/* NeWS server limitation --
		   the real magic number seems to be 0.03125,
		   but let's not push our luck */
		if ((fx >= 0.) && (fx < 0.05))
			fx= 0.05;
		else if ((fx <= 0.) && (fx > -0.05))
			fx= -0.05;
		if ((fy >= 0.) && (fy < 0.05))
			fy= 0.05;
		else if ((fy <= 0.) && (fy > -0.05))
			fy= -0.05;

		if (outfp == (FILE *) 0)
			news_translate(fx, fy);
		else
			fprintf(outfp, "%6.4f %6.4f translate\n", fx, fy);
	}

	/* rotate as necessary */
	rot= getrotation();
	if (outfp == (FILE *) 0)
		news_rotate(rot);
	else
		fprintf(outfp, "%6.4f rotate\n", rot);

	/* scale appropriately */
	getscale(&hs, &vs);
	if (outfp == (FILE *) 0)
		news_scale(hs, vs);
	else
		fprintf(outfp, "%6.4f %6.4f scale\n", hs, vs);

	if (boundarymode == MARKER_OPAQUEBNDM) {
		bnd= 1;
		rb= ((float) redbnd)/255.;
		gb= ((float) greenbnd)/255.;
		bb= ((float) bluebnd)/255.;
	}
	else
		bnd= 0;

	if (fillmode == MARKER_OPAQUEFILLM) {
		fill= 1;
		rf= ((float) redfill)/255.;
		gf= ((float) greenfill)/255.;
		bf= ((float) bluefill)/255.;
	}
	else
		fill= 0;

	if (outfp == (FILE *) 0) {
		switch (mrktype) {
		case MARKER_SQUARE:
			news_drawsquare(radius, bnd, bndwidth, rb, gb, bb, fill, rf, gf, bf);
			break;
		case MARKER_TRIANGLE:
			news_drawtriangle(radius, bnd, bndwidth, rb, gb, bb, fill, rf, gf, bf);
			break;
		case MARKER_CIRCLE:
			news_drawcircle(radius, bnd, bndwidth, rb, gb, bb, fill, rf, gf, bf);
			break;
		case MARKER_CROSS:
			news_drawcross(radius, bnd, bndwidth, rb, gb, bb);
			break;
		}
	}
	else {
		switch (mrktype) {
		case MARKER_SQUARE:
			fprintf(outfp, "newpath\n");
			fprintf(outfp, "%6.4f neg %6.4f neg moveto\n", radius, radius);
			fprintf(outfp, "%6.4f 2 mul 0 rlineto\n", radius);
			fprintf(outfp, "0 %6.4f 2 mul rlineto\n", radius);
			fprintf(outfp, "%6.4f 2 mul neg 0 rlineto\n", radius);
			fprintf(outfp, "closepath\n");
			if (fill) {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rf, gf, bf);
				fprintf(outfp, "gsave\n");
				fprintf(outfp, "fill\n");
				fprintf(outfp, "grestore\n");
			}
			if (bnd) {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rb, gb, bb);
				fprintf(outfp, "%6.4f setlinewidth\n", bndwidth);
				fprintf(outfp, "stroke\n");
			}
			break;
		case MARKER_TRIANGLE:
			fprintf(outfp, "newpath\n");
			fprintf(outfp, "%6.4f neg %6.4f neg moveto\n", radius, radius);
			fprintf(outfp, "%6.4f 2 mul 0 rlineto\n", radius);
			fprintf(outfp, "%6.4f neg %6.4f 2 mul rlineto\n", radius, radius);
			fprintf(outfp, "closepath\n");
			if (fill) {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rf, gf, bf);
				fprintf(outfp, "gsave\n");
				fprintf(outfp, "fill\n");
				fprintf(outfp, "grestore\n");
			}
			if (bnd) {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rb, gb, bb);
				fprintf(outfp, "%6.4f setlinewidth\n", bndwidth);
				fprintf(outfp, "stroke\n");
			}
			break;
		case MARKER_CIRCLE:
			fprintf(outfp, "newpath\n");
			fprintf(outfp, "0 0 %6.4f 0 360 arc\n", radius);
			if (fill) {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rf, gf, bf);
				fprintf(outfp, "gsave\n");
				fprintf(outfp, "fill\n");
				fprintf(outfp, "grestore\n");
			}
			if (bnd) {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rb, gb, bb);
				fprintf(outfp, "%6.4f setlinewidth\n", bndwidth);
				fprintf(outfp, "stroke\n");
			}
			break;
		case MARKER_CROSS:
			if (bnd) {
				fprintf(outfp, "newpath\n");
				fprintf(outfp, "%6.4f neg 0 moveto\n", radius);
				fprintf(outfp, "%6.4f 2 mul 0 rlineto\n", radius);
				fprintf(outfp, "0 %6.4f neg moveto\n", radius);
				fprintf(outfp, "0 %6.4f 2 mul rlineto\n", radius);
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", rb, gb, bb);
				fprintf(outfp, "%6.4f setlinewidth\n", bndwidth);
				fprintf(outfp, "stroke\n");
			}
			break;
		}
	}

	if (outfp == (FILE *) 0) {
		(void) strcpy(psbuf, " false 1 setfileinputtoken stop\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		psio_flush(PostScript);

		news_syncserver();

		while (ps_check_PostScript_event() == 1) {
			ps_read_PostScript_event(&tag);
			if (tag == RENDERERR_TAG)
				rendererr= 1;
		}
	}
	else
		fprintf(outfp, "restore\n");

	if (rendererr)
		return GROBJ_FAILURE;
	else
		return GROBJ_SUCCESS;
}

int
Marker::icedump(FILE *fp)
{
	float fx, fy, r, h, v;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Mrk: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Mrk: Loc %6.4f %6.4f\n", fx, fy);

	r= getrotation();
	getscale(&h, &v);
	fprintf(fp, "%%%%ICE-Mrk: Trans %6.4f %6.4f %6.4f\n", r, h, v);

	fprintf(fp, "%%%%ICE-Mrk: Type %1d %1d\n", markertype, mrktype);

	fprintf(fp, "%%%%ICE-Mrk: Size %1d %6.4f\n", size, radius);

	fprintf(fp, "%%%%ICE-Mrk: Bnd %1d %1d %6.4f %1d %1d %1d %1d\n", boundarymode, boundarywd, bndwidth, boundarycolor, (int) redbnd, (int) greenbnd, (int) bluebnd);

	fprintf(fp, "%%%%ICE-Mrk: Fill %1d %1d %1d %1d %1d\n", fillmode, fillcolor, (int) redfill, (int) greenfill, (int) bluefill);

	if ((pth= getclip()) != (Path *) 0)
		fprintf(fp, "%%%%ICE-Mrk: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Mrk: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Mrk: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Mrk: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Mrk: End\n");
	return GROBJ_SUCCESS;
}

Marker::~Marker()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) 0)
		(void) cpth->setreferences(PATH_REFDECR);
}
