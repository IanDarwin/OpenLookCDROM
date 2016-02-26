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

#include "Path.h"
#include "Psdoc.h"

extern "C" {
void		bcopy(char *, char *, int);
int		pprintf( ... );
int		ps_checkfor(PSFILE *, int, int);
int		ps_currenttag(PSFILE *, int, int *);
int		psio_flush(PSFILE *);
int		psio_flushbuf(char, PSFILE *);
int		psio_write(char *, int, int, PSFILE *);
char *		strcpy(char *, char *);
int		strlen(char *);
}

extern FILE *	doc_open(char *, char *);

static char psbuf[PSDOC_MAXLINELEN+1];

Psdoc::Psdoc()
{
	name= (char *) 0;
	(void) settype(GROBJ_PSDOC);
	setsequence(0);
}

Psdoc::Psdoc(Dlnk **listhd, char *f, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	if (f == (char *) 0)
		name= (char *) 0;
	else if (strlen(f) == 0)
		name= (char *) 0;
	else {
		if ((name= new char[strlen(f)+1]) != (char *) 0)
			(void) strcpy(name, f);
	}

	(void) settype(GROBJ_PSDOC);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Psdoc::draw(FILE *outfp)
{
	FILE *infp;
	int x, y;
	float fx, fy, rot, hs, vs;
	Path *cpth;
	int tag, rendererr, flags;

	if (name == (char *) 0)
		return GROBJ_FAILURE;

	if ((infp= doc_open(name, "r")) == (FILE *) NULL)
		return GROBJ_FAILURE;

	if (outfp == (FILE *) 0) {

		/* discard any unread NeWS tags */
		while (ps_check_PostScript_event() == 1)
			ps_read_PostScript_event(&tag);

		news_traperrors();
	}
	else 
		fprintf(outfp, "ICEInitExt\n");

	if ((cpth= getclip()) != (Path *) 0) {
		flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
		cpth->draw(flags, outfp);
	}

	/* shift origin to object location */
	getloc(&fx, &fy, &x, &y);
	fx*= 72.;
	fy*= 72.;
	if ((fx != 0.) || (fy != 0.)) {

		/* NeWS server limitation --
		   the real magic number seems to be 0.03125,
		   but lets not push our luck */
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

	/* read from file */
	while (fgets(psbuf, PSDOC_MAXLINELEN+1, infp) != (char *) NULL) {
		if (outfp == (FILE *) 0)
			psio_write(psbuf, 1, strlen(psbuf), PostScript);
		else
			fputs(psbuf, outfp);
	}
	if (outfp == (FILE *) 0) {
		(void) strcpy(psbuf, " false 1 setfileinputtoken stop\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		psio_flush(PostScript);

		news_syncserver();

		rendererr= 0;
		while (ps_check_PostScript_event() == 1) {
			ps_read_PostScript_event(&tag);
			if (tag == RENDERERR_TAG)
				rendererr= 1;
		}
	}
	else
		fprintf(outfp, "ICETermExt\n");

	(void) fclose(infp);

	if ((outfp == (FILE *) 0) && (rendererr))
		return GROBJ_FAILURE;
	else
		return GROBJ_SUCCESS;
}

int
Psdoc::icedump(FILE *fp)
{
	float fx, fy, r, h, v;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Psd: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Psd: Loc %6.4f %6.4f\n", fx, fy);

	r= getrotation();
	getscale(&h, &v);
	fprintf(fp, "%%%%ICE-Psd: Trans %6.4f %6.4f %6.4f\n", r, h, v);

	if ((pth= getclip()) != (Path *) NULL)
		fprintf(fp, "%%%%ICE-Psd: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Psd: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Psd: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Psd: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Psd: End\n");
	return GROBJ_SUCCESS;
}

Psdoc::~Psdoc()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) NULL)
		(void) cpth->setreferences(PATH_REFDECR);
	delete name;
}
