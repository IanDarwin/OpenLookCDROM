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

#include "Rectangle.h"

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

Rectangle::Rectangle()
{
	width= height= 0.;
	boundarymode= RECTANGLE_OPAQUEBNDM;
	boundarywd= RECTANGLE_GLOBALBNDWIDTH;
	bndwidth= 2.;
	linestyle= RECTANGLE_SOLID;
	dashstyle= RECTANGLE_SIMPLEDASH;
	dashpattern= (float *) 0;
	dashpatternlen= 0;
	dashpatternoffset= 0.;
	boundarycolor= RECTANGLE_GLOBALBND;
	redbnd= greenbnd= bluebnd= 0;
	fillmode= RECTANGLE_TRANSPARENTFILLM;
	fillcolor= RECTANGLE_GLOBALFILL;
	redfill= greenfill= bluefill= 255;

	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_RECTANGLE);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(0);
}

Rectangle::Rectangle(Dlnk **listhd, char *nm, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	(void) setname(nm);

	width= height= 0.;
	boundarymode= RECTANGLE_OPAQUEBNDM;
	boundarywd= RECTANGLE_GLOBALBNDWIDTH;
	bndwidth= 2.;
	linestyle= RECTANGLE_SOLID;
	dashstyle= RECTANGLE_SIMPLEDASH;
	dashpattern= (float *) 0;
	dashpatternlen= 0;
	dashpatternoffset= 0.;
	boundarycolor= RECTANGLE_GLOBALBND;
	redbnd= greenbnd= bluebnd= 0;
	fillmode= RECTANGLE_TRANSPARENTFILLM;
	fillcolor= RECTANGLE_GLOBALFILL;
	redfill= greenfill= bluefill= 255;

	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_RECTANGLE);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Rectangle::setsize(float w, float h)
{
	if ((w < 0.) || (h < 0.))
		return GROBJ_FAILURE;

	width= w;
	height= h;
	return GROBJ_SUCCESS;
}

int
Rectangle::setboundary(int bm, int bc, unsigned char r, unsigned char g, unsigned char b)
{
	switch (bm) {
	case RECTANGLE_OPAQUEBNDM:
	case RECTANGLE_TRANSPARENTBNDM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (bc) {
	case RECTANGLE_BLACKBND:
		boundarymode= bm;
		boundarycolor= bc;
		redbnd= greenbnd= bluebnd= 0;
		return GROBJ_SUCCESS;
	case RECTANGLE_WHITEBND:
		boundarymode= bm;
		boundarycolor= bc;
		redbnd= greenbnd= bluebnd= 255;
		return GROBJ_SUCCESS;
	case RECTANGLE_GLOBALBND:
	case RECTANGLE_OTHERBND:
		boundarymode= bm;
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
Rectangle::setwidth(int bw, float w)
{
	switch (bw) {
	case RECTANGLE_GLOBALBNDWIDTH:
	case RECTANGLE_OTHERBNDWIDTH:
		if (w < 0.)
			return GROBJ_FAILURE;
		boundarywd= bw;
		bndwidth= w;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Rectangle::setlinestyle(int l)
{
	switch (l) {
	case RECTANGLE_SOLID:
	case RECTANGLE_DASHED:
		linestyle= l;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Rectangle::setdashstyle(int ds, float *dp, int dpl, float dpo)
{
	switch (ds) {
	case RECTANGLE_SIMPLEDASH:
	case RECTANGLE_COMPLEXDASH:
		break;
	default:
		return GROBJ_FAILURE;
	}
	if (dpl < 2)
		return GROBJ_FAILURE;
	if (dpo < 0.)
		return GROBJ_FAILURE;

	delete dashpattern;
	dashstyle= ds;
	dashpattern= dp;
	dashpatternlen= dpl;
	dashpatternoffset= dpo;
	return GROBJ_SUCCESS;
}

int
Rectangle::setfill(int fm, int fc, unsigned char r, unsigned char g, unsigned char b)
{
	switch (fm) {
	case RECTANGLE_TRANSPARENTFILLM:
	case RECTANGLE_OPAQUEFILLM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (fc) {
	case RECTANGLE_WHITEFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= greenfill= bluefill= 255;
		return GROBJ_SUCCESS;
	case RECTANGLE_BLACKFILL:
		fillmode= fm;
		fillcolor= fc;
		redfill= greenfill= bluefill= 0;
		return GROBJ_SUCCESS;
	case RECTANGLE_GLOBALFILL:
	case RECTANGLE_OTHERFILL:
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

char *
Rectangle::patternstr()
{
	char *buf, smallbuf[10];
	char *str;
	int i;

	str= (char *) 0;
	if ((buf= new char[(10*dashpatternlen)+1]) == (char *) 0)
		return str;
	bzero(buf, (10*dashpatternlen)+1);

	for (i= 0; i < dashpatternlen; i++) {
		(void) gconvert((double) ((float) *(dashpattern+i)), 5, 0, smallbuf);
		(void) strcat(buf, smallbuf);
		if (i < dashpatternlen-1)
			(void) strcat(buf, " ");
	}
	if ((str= new char[strlen(buf)+1]) == (char *) 0) {
		delete buf;
		return str;
	}
	(void) strcpy(str, buf);
	delete buf;

	return str;
}

int
Rectangle::draw(FILE *outfp)
{
	int tag, rendererr;
	float fx, fy, rot;
	int x, y, flags;
	float r, g, b;
	char psbuf[128], *dps;
	Path *cpth;

	if ((boundarymode != RECTANGLE_OPAQUEBNDM) && (fillmode != RECTANGLE_OPAQUEFILLM))
		return GROBJ_SUCCESS;

	if ((boundarymode == RECTANGLE_OPAQUEBNDM) && (linestyle == RECTANGLE_DASHED)) {
		if ((dps= patternstr()) == (char *) 0)
			return GROBJ_FAILURE;
	}

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

	if (outfp == (FILE *) 0) {
		(void) strcpy(psbuf, "newpath 0 0 moveto\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		(void) sprintf(psbuf, "%4.2f 0 rlineto\n", 72.*width);
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		(void) sprintf(psbuf, "0 %4.2f rlineto\n", 72.*height);
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		(void) sprintf(psbuf, "%4.2f 0 rlineto\n", -72.*width);
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
	}
	else {
		fprintf(outfp, "newpath 0 0 moveto\n");
		fprintf(outfp, "%4.2f 0 rlineto\n", 72.*width);
		fprintf(outfp, "0 %4.2f rlineto\n", 72.*height);
		fprintf(outfp, "%4.2f 0 rlineto\n", -72.*width);
	}

	if (outfp == (FILE *) 0) {
		(void) strcpy(psbuf, "closepath\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
	}
	else
		fprintf(outfp, "closepath\n");

	if (fillmode == RECTANGLE_OPAQUEFILLM) {
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

	if (boundarymode == RECTANGLE_OPAQUEBNDM) {
		r= ((float) redbnd)/255.;
		g= ((float) greenbnd)/255.;
		b= ((float) bluebnd)/255.;
		if (outfp == (FILE *) 0) {
			news_setrgbcolor(r, g, b);
			news_setlinewidth(bndwidth);
			news_setlinecap(0);
			if (linestyle == RECTANGLE_DASHED) {
				(void) sprintf(psbuf, "[%s] %4.2f setdash\n", dps, dashpatternoffset);
				psio_write(psbuf, 1, strlen(psbuf), PostScript);
			}
			(void) strcpy(psbuf, "stroke\n");
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
		else {
			fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
			fprintf(outfp, "%6.4f setlinewidth\n", bndwidth);
			fprintf(outfp, "0 setlinecap\n");
			if (linestyle == RECTANGLE_DASHED)
				fprintf(outfp, "[%s] %4.2f setdash\n", dps, dashpatternoffset);
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

	if ((boundarymode == RECTANGLE_OPAQUEBNDM) && (linestyle == RECTANGLE_DASHED))
		delete dps;

	if (rendererr)
		return GROBJ_FAILURE;
	else
		return GROBJ_SUCCESS;
}

int
Rectangle::icedump(FILE *fp)
{
	float fx, fy, r;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Rect: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Rect: Loc %6.4f %6.4f %6.4f %6.4f\n", fx, fy, width, height);

	r= getrotation();
	fprintf(fp, "%%%%ICE-Rect: Rot %6.4f\n", r);

	fprintf(fp, "%%%%ICE-Rect: Bnd %1d %1d %1d %1d %1d\n", boundarymode, boundarycolor, (int) redbnd, (int) greenbnd, (int) bluebnd);

	fprintf(fp, "%%%%ICE-Rect: Line %1d %6.4f %1d\n", boundarywd, bndwidth, linestyle);

	fprintf(fp, "%%%%ICE-Rect: Dash %1d %6.4f %1d", dashstyle, dashpatternoffset, dashpatternlen);
	for (ix= 0; ix < dashpatternlen; ix++)
		fprintf(fp, " %6.4f", dashpattern[ix]);
	fprintf(fp, "\n");

	fprintf(fp, "%%%%ICE-Rect: Fill %1d %1d %1d %1d %1d\n", fillmode, fillcolor, (int) redfill, (int) greenfill, (int) bluefill);

	if ((pth= getclip()) != (Path *) 0)
		fprintf(fp, "%%%%ICE-Rect: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Rect: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Rect: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Rect: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Rect: End\n");
	return GROBJ_SUCCESS;
}

Rectangle::~Rectangle()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) 0)
		(void) cpth->setreferences(PATH_REFDECR);
	delete dashpattern;
}
