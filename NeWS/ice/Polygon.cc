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

#include "Polygon.h"

extern "C" {
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

Polygon::Polygon()
{
	vertices= 0;
	xvertices= yvertices= (float *) 0;
	closure= POLYGON_CLOSED;
	boundarymode= POLYGON_OPAQUEBNDM;
	boundarywd= POLYGON_GLOBALBNDWIDTH;
	bndwidth= 2.;
	boundarycolor= POLYGON_GLOBALBND;
	redbnd= greenbnd= bluebnd= 0;
	fillmode= POLYGON_TRANSPARENTFILLM;
	fillcolor= POLYGON_GLOBALFILL;
	redfill= greenfill= bluefill= 255;

	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_POLYGON);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(0);
}

Polygon::Polygon(Dlnk **listhd, char *nm, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	(void) setname(nm);

	vertices= 0;
	xvertices= yvertices= (float *) 0;
	closure= POLYGON_CLOSED;
	boundarymode= POLYGON_OPAQUEBNDM;
	boundarywd= POLYGON_GLOBALBNDWIDTH;
	bndwidth= 2.;
	boundarycolor= POLYGON_GLOBALBND;
	redbnd= greenbnd= bluebnd= 0;
	fillmode= POLYGON_TRANSPARENTFILLM;
	fillcolor= POLYGON_GLOBALFILL;
	redfill= greenfill= bluefill= 255;

	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_POLYGON);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Polygon::setvertices(int n, float *x, float *y)
{
	if (n < 0)
		return GROBJ_FAILURE;

	delete xvertices;
	delete yvertices;
	vertices= n;
	xvertices= x;
	yvertices= y;
	return GROBJ_SUCCESS;
}

int
Polygon::setclosure(int c)
{
	switch (c) {
	case POLYGON_CLOSED:
	case POLYGON_OPEN:
		closure= c;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Polygon::setboundary(int bm, int bw, float w, int bc, unsigned char r, unsigned char g, unsigned char b)
{
	switch (bm) {
	case POLYGON_OPAQUEBNDM:
	case POLYGON_TRANSPARENTBNDM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (bw) {
	case POLYGON_GLOBALBNDWIDTH:
	case POLYGON_OTHERBNDWIDTH:
		if (w < 0.)
			return GROBJ_FAILURE;
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (bc) {
	case POLYGON_BLACKBND:
		boundarymode= bm;
		boundarywd= bw;
		bndwidth= w;
		boundarycolor= bc;
		redbnd= greenbnd= bluebnd= 0;
		return GROBJ_SUCCESS;
	case POLYGON_WHITEBND:
		boundarymode= bm;
		boundarywd= bw;
		bndwidth= w;
		boundarycolor= bc;
		redbnd= greenbnd= bluebnd= 255;
		return GROBJ_SUCCESS;
	case POLYGON_GLOBALBND:
	case POLYGON_OTHERBND:
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
Polygon::setfill(int fm, int fc, unsigned char r, unsigned char g, unsigned char b)
{
	switch (fm) {
	case POLYGON_TRANSPARENTFILLM:
	case POLYGON_OPAQUEFILLM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (fc) {
	case POLYGON_WHITEFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= greenfill= bluefill= 255;
		return GROBJ_SUCCESS;
	case POLYGON_BLACKFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= greenfill= bluefill= 0;
		return GROBJ_SUCCESS;
	case POLYGON_GLOBALFILL:
	case POLYGON_OTHERFILL:
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
Polygon::draw(FILE *outfp)
{
	int tag, rendererr;
	float fx, fy, rot, hs, vs;
	int x, y, flags, i;
	float r, g, b;
	char psbuf[128];
	Path *cpth;

	if ((closure != POLYGON_CLOSED) && (boundarymode != POLYGON_OPAQUEBNDM))
		return GROBJ_SUCCESS;
	if ((boundarymode != POLYGON_OPAQUEBNDM) && (fillmode != POLYGON_OPAQUEFILLM))
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

	if (outfp == (FILE *) 0) {
		(void) strcpy(psbuf, "newpath 0 0 moveto\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
	}
	else
		fprintf(outfp, "newpath 0 0 moveto\n");
	for (i= 1; i < vertices; i++) {
		if (outfp == (FILE *) 0) {
			(void) sprintf(psbuf, "%4.2f %4.2f lineto\n", 72.*xvertices[i], 72.*yvertices[i]);
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
		else
			fprintf(outfp, "%4.2f %4.2f lineto\n", 72.*xvertices[i], 72.*yvertices[i]);
	}

	if (closure == POLYGON_CLOSED) {
		if (outfp == (FILE *) 0) {
			(void) strcpy(psbuf, "closepath\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
		else
			fprintf(outfp, "closepath\n");

		if (fillmode == POLYGON_OPAQUEFILLM) {
			r= ((float) redfill)/255.;
			g= ((float) greenfill)/255.;
			b= ((float) bluefill)/255.;
			if (outfp == (FILE *) 0) {
				news_setrgbcolor(r, g, b);
				(void) strcpy(psbuf, "gsave fill grestore\n");
				psio_write(psbuf, 1, strlen(psbuf), PostScript);
			}
			else {
				fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
				fprintf(outfp, "gsave fill grestore\n");
			}
		}
	}

	if (boundarymode == POLYGON_OPAQUEBNDM) {
		r= ((float) redbnd)/255.;
		g= ((float) greenbnd)/255.;
		b= ((float) bluebnd)/255.;
		if (outfp == (FILE *) 0) {
			news_setrgbcolor(r, g, b);
			news_setlinewidth(bndwidth);
			(void) strcpy(psbuf, "stroke\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
		else {
			fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
			fprintf(outfp, "%6.4f setlinewidth\n", bndwidth);
			fprintf(outfp, "stroke\n");
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
Polygon::icedump(FILE *fp)
{
	float fx, fy, r, h, v;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Poly: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Poly: Loc %6.4f %6.4f\n", fx, fy);

	r= getrotation();
	getscale(&h, &v);
	fprintf(fp, "%%%%ICE-Poly: Trans %6.4f %6.4f %6.4f\n", r, h, v);

	fprintf(fp, "%%%%ICE-Poly: Cls %1d\n", closure);

	fprintf(fp, "%%%%ICE-Poly: Vert %1d\n", vertices);
	for (ix= 0; ix < vertices; ix++)
		fprintf(fp, "%%%%ICE-Poly: Vert+ %6.4f %6.4f\n", xvertices[ix], yvertices[ix]);

	fprintf(fp, "%%%%ICE-Poly: Bnd %1d %1d %6.4f %1d %1d %1d %1d\n", boundarymode, boundarywd, bndwidth, boundarycolor, (int) redbnd, (int) greenbnd, (int) bluebnd);

	fprintf(fp, "%%%%ICE-Poly: Fill %1d %1d %1d %1d %1d\n", fillmode, fillcolor, (int) redfill, (int) greenfill, (int) bluefill);

	if ((pth= getclip()) != (Path *) 0)
		fprintf(fp, "%%%%ICE-Poly: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Poly: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Poly: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Poly: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Poly: End\n");
	return GROBJ_SUCCESS;
}

Polygon::~Polygon()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) 0)
		(void) cpth->setreferences(PATH_REFDECR);
	delete xvertices;
	delete yvertices;
}
