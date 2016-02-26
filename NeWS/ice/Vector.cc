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

#include "Vector.h"

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

Vector::Vector()
{
	ex= ey= 0.;
	pex= pey= 0;
	linewd= VECTOR_GLOBALWIDTH;
	width= 1.;
	linestyle= VECTOR_SOLID;
	dashstyle= VECTOR_SIMPLEDASH;
	dashpattern= (float *) 0;
	dashpatternlen= 0;
	dashpatternoffset= 0.;
	capstyle= VECTOR_BUTTCAP;
	pointer= VECTOR_NOPTR;
	pointerstyle= VECTOR_OPENPTR;
	pointerwd= 10.;
	pointerolen= 10.;
	pointerilen= 10.;
	foreground= VECTOR_GLOBALFG;
	redfg= greenfg= bluefg= 0;
	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_VECTOR);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(0);
}

Vector::Vector(Dlnk **listhd, char *nm, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	(void) setname(nm);

	ex= ey= 0.;
	pex= pey= 0;
	linewd= VECTOR_GLOBALWIDTH;
	width= 1.;
	linestyle= VECTOR_SOLID;
	dashstyle= VECTOR_SIMPLEDASH;
	dashpattern= (float *) 0;
	dashpatternlen= 0;
	dashpatternoffset= 0.;
	capstyle= VECTOR_BUTTCAP;
	pointer= VECTOR_NOPTR;
	pointerstyle= VECTOR_OPENPTR;
	pointerwd= 10.;
	pointerolen= 10.;
	pointerilen= 10.;
	foreground= VECTOR_GLOBALFG;
	redfg= greenfg= bluefg= 0;
	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_VECTOR);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

void
Vector::setfend(float fx, float fy, float dpi)
{
	ex= fx;
	ey= fy;
	pex= (int) (dpi*ex);
	pey= (int) (dpi*ey);
}

void
Vector::setpend(int ix, int iy, float dpi)
{
	float ipp;

	ipp= 1./dpi;
	ex= ipp*((float) ix);
	ey= ipp*((float) iy);
	pex= ix;
	pey= iy;
}

int
Vector::setwidth(int lw, float w)
{
	switch (lw) {
	case VECTOR_GLOBALWIDTH:
	case VECTOR_OTHERWIDTH:
		if (w < 0.)
			return GROBJ_FAILURE;
		linewd= lw;
		width= w;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Vector::setlinestyle(int l)
{
	switch (l) {
	case VECTOR_SOLID:
	case VECTOR_DASHED:
		linestyle= l;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Vector::setdashstyle(int ds, float *dp, int dpl, float dpo)
{
	switch (ds) {
	case VECTOR_SIMPLEDASH:
	case VECTOR_COMPLEXDASH:
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
Vector::setcapstyle(int c)
{
	switch (c) {
	case VECTOR_BUTTCAP:
	case VECTOR_ROUNDCAP:
	case VECTOR_SQUARECAP:
		capstyle= c;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Vector::setptrstyle(int p, int ps, float pw, float po, float pi)
{
	switch (p) {
	case VECTOR_NOPTR:
	case VECTOR_ORIGPTR:
	case VECTOR_TERMPTR:
	case VECTOR_BOTHPTR:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (ps) {
	case VECTOR_OPENPTR:
	case VECTOR_CLOSEDPTR:
		break;
	default:
		return GROBJ_FAILURE;
	}

	if ((pw < 0.) || (po < 0.) || (pi < 0.))
		return GROBJ_FAILURE;

	pointer= p;
	pointerstyle= ps;
	pointerwd= pw;
	pointerolen= po;
	pointerilen= pi;

	return GROBJ_SUCCESS;
}

int
Vector::setforeground(int fg, unsigned char r, unsigned char g, unsigned char b)
{
	switch (fg) {
	case VECTOR_BLACKFG:
		foreground= fg;
		redfg= greenfg= bluefg= 0;
		return GROBJ_SUCCESS;
	case VECTOR_WHITEFG:
		foreground= fg;
		redfg= greenfg= bluefg= 255;
		return GROBJ_SUCCESS;
	case VECTOR_GLOBALFG:
	case VECTOR_OTHERFG:
		foreground= fg;
		redfg= r;
		greenfg= g;
		bluefg= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

char *
Vector::patternstr()
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
Vector::draw(FILE *outfp)
{
	int tag, rendererr;
	float xstart, ystart, xstop, ystop;
	int x, y, flags;
	float r, g, b;
	char psbuf[128], *dps;
	Path *cpth;

	if (linestyle == VECTOR_DASHED) {
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

	getloc(&xstart, &ystart, &x, &y);
	getend(&xstop, &ystop, &x, &y);

	/* foreground color */
	r= ((float) redfg)/255.;
	g= ((float) greenfg)/255.;
	b= ((float) bluefg)/255.;
	if (outfp == (FILE *) 0)
		news_setrgbcolor(r, g, b);
	else
		fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);

	/* line attributes */
	if (outfp == (FILE *) 0) {
		news_setlinecap(capstyle);
		if (linestyle == VECTOR_DASHED) {
			(void) sprintf(psbuf, "[%s] %4.2f setdash\n", dps, dashpatternoffset);
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		}
	}
	else {
		fprintf(outfp, "%1d setlinecap\n", capstyle);
		if (linestyle == VECTOR_DASHED)
			fprintf(outfp, "[%s] %4.2f setdash\n", dps, dashpatternoffset);
	}

	(void) sprintf(psbuf, "ICEVectorDict begin\n%6.4f %6.4f %6.4f %6.4f %4.2f %1d %1d %4.2f %4.2f %4.2f Vector\nend\n", xstart, ystart, xstop, ystop, width, pointer, pointerstyle, pointerwd, pointerolen, pointerilen);
	if (outfp == (FILE *) 0)
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
	else
		fprintf(outfp, "%s", psbuf);

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

	if (linestyle == VECTOR_DASHED)
		delete dps;

	if (rendererr)
		return GROBJ_FAILURE;
	else
		return GROBJ_SUCCESS;
}

int
Vector::icedump(FILE *fp)
{
	float fx, fy;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Vec: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Vec: Loc %6.4f %6.4f %6.4f %6.4f\n", fx, fy, ex, ey);

	fprintf(fp, "%%%%ICE-Vec: Line %1d %6.4f %1d %1d\n", linewd, width, linestyle, capstyle);

	fprintf(fp, "%%%%ICE-Vec: Dash %1d %6.4f %1d", dashstyle, dashpatternoffset, dashpatternlen);
	for (ix= 0; ix < dashpatternlen; ix++)
		fprintf(fp, " %6.4f", dashpattern[ix]);
	fprintf(fp, "\n");

	fprintf(fp, "%%%%ICE-Vec: Ptr %1d %1d %4.2f %4.2f %4.2f\n", pointer, pointerstyle, pointerwd, pointerolen, pointerilen);

	fprintf(fp, "%%%%ICE-Vec: FG %1d %1d %1d %1d\n", foreground, (int) redfg, (int) greenfg, (int) bluefg);

	if ((pth= getclip()) != (Path *) 0)
		fprintf(fp, "%%%%ICE-Vec: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Vec: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Vec: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Vec: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Vec: End\n");
	return GROBJ_SUCCESS;
}

Vector::~Vector()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) 0)
		(void) cpth->setreferences(PATH_REFDECR);
	delete dashpattern;
}
