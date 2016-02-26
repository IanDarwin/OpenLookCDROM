/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <NeWS/psio.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "Axis.h"

extern "C" {
char *		gconvert(double, int, int, char *);
int		pprintf( ... );
int		ps_checkfor(PSFILE *, int, int);
int		ps_currenttag(PSFILE *, int, int *);
int		pscanf( ... );
int		psio_flush(PSFILE *);
int		psio_flushbuf(char, PSFILE *);
int		psio_write(char *, int, int, PSFILE *);
char *		strcat(char *, char*);
char *		strcpy(char *, char *);
int		strlen(char *);
}

#define PI		((double) 3.1415926536)
#define LOG10(x)	((double) (log(x)*.434294))
#define POW10(x)	((double) (exp(x*2.302585)))

static double ptenarray[77]= {
	1e-38, 1e-37, 1e-36, 1e-35, 1e-34, 1e-33, 1e-32, 1e-31, 1e-30,
	1e-29, 1e-28, 1e-27, 1e-26, 1e-25, 1e-24, 1e-23, 1e-22, 1e-21,
	1e-20, 1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14, 1e-13, 1e-12,
	1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2,
	1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10,
	1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21,
	1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29, 1e30, 1e31, 1e32,
	1e33, 1e34, 1e35, 1e36, 1e37, 1e38
};
static double eps;
static int ptenindex;

static float lastlabelpos;

Axis::Axis()
{
	ex= ey= 0.;
	pex= pey= 0;
	origin= 0.;
	terminus= 100.;
	subdiv= 10;
	linlog= AXIS_LINEAR;
	axiswd= AXIS_GLOBALWIDTH;
	axiswidth= 1.;
	tickloc= AXIS_STDLOC;
	ptickht= 7.;
	stickht= 5.;
	ttickht= 3.;
	tickwd= AXIS_GLOBALWIDTH;
	tickwidth= 1.;
	line= AXIS_GLOBALLINE;
	redline= greenline= blueline= 0;
	fontloc= AXIS_STDLOC;
	font= AXIS_GLOBALFONT;
	fontname= (char *) 0;
	fontsz= AXIS_GLOBALFONTSZ;
	fontsize= 12.;
	fontorient= AXIS_FONTORIENT0;
	fontoff= 0.;
	label= AXIS_GLOBALLABEL;
	redlabel= greenlabel= bluelabel= 0;
	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_AXIS);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(0);
}

Axis::Axis(Dlnk **listhd, char *nm, int seq)
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
	origin= 0.;
	terminus= 100.;
	subdiv= 10;
	linlog= AXIS_LINEAR;
	axiswd= AXIS_GLOBALWIDTH;
	axiswidth= 1.;
	tickloc= AXIS_STDLOC;
	ptickht= 7.;
	stickht= 5.;
	ttickht= 3.;
	tickwd= AXIS_GLOBALWIDTH;
	tickwidth= 1.;
	line= AXIS_GLOBALLINE;
	redline= greenline= blueline= 0;
	fontloc= AXIS_STDLOC;
	font= AXIS_GLOBALFONT;
	fontname= (char *) 0;
	fontsz= AXIS_GLOBALFONTSZ;
	fontsize= 12.;
	fontorient= AXIS_FONTORIENT0;
	fontoff= 0.;
	label= AXIS_GLOBALLABEL;
	redlabel= greenlabel= bluelabel= 0;
	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_AXIS);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

void
Axis::setfend(float fx, float fy, float dpi)
{
	ex= fx;
	ey= fy;
	pex= (int) (dpi*ex);
	pey= (int) (dpi*ey);
}

void
Axis::setpend(int ix, int iy, float dpi)
{
	float ipp;

	ipp= 1./dpi;
	ex= ipp*((float) ix);
	ey= ipp*((float) iy);
	pex= ix;
	pey= iy;
}

int
Axis::setotl(double o, double t, int l)
{
	switch (l) {
	case AXIS_LOG:
		if ((o < 0.) || (t < 0.))
			return GROBJ_FAILURE;
		break;
	case AXIS_LINEAR:
		break;
	default:
		return GROBJ_FAILURE;
	}

	if (t < o)
		return GROBJ_FAILURE;

	origin= o;
	terminus= t;
	linlog= l;

	setumap(o, t);
	return GROBJ_SUCCESS;
}

int
Axis::setsubdiv(int s)
{
	if (s < 0)
		return GROBJ_FAILURE;
	subdiv= s;
	return GROBJ_SUCCESS;
}

int
Axis::setaxiswidth(int aw, float w)
{
	switch (aw) {
	case AXIS_GLOBALWIDTH:
	case AXIS_OTHERWIDTH:
		if (w < 0.)
			return GROBJ_FAILURE;
		axiswd= aw;
		axiswidth= w;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Axis::settick(int tl, float l, float m, float s, int tw, float w)
{
	switch (tl) {
	case AXIS_STDLOC:
	case AXIS_ALTLOC:
	case AXIS_NOLOC:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (tw) {
	case AXIS_GLOBALWIDTH:
	case AXIS_OTHERWIDTH:
		break;
	default:
		return GROBJ_FAILURE;
	}

	if (w < 0.)
		return GROBJ_FAILURE;

	tickloc= tl;
	ptickht= l;
	stickht= m;
	ttickht= s;
	tickwd= tw;
	tickwidth= w;
	return GROBJ_SUCCESS;
}

int
Axis::setline(int l, unsigned char r, unsigned char g, unsigned char b)
{
	switch (l) {
	case AXIS_BLACKLINE:
		line= l;
		redline= greenline= blueline= 0;
		return GROBJ_SUCCESS;
	case AXIS_WHITELINE:
		line= l;
		redline= greenline= blueline= 255;
		return GROBJ_SUCCESS;
	case AXIS_GLOBALLINE:
	case AXIS_OTHERLINE:
		line= l;
		redline= r;
		greenline= g;
		blueline= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Axis::setlabel(int l, unsigned char r, unsigned char g, unsigned char b)
{
	switch (l) {
	case AXIS_BLACKLABEL:
		label= l;
		redlabel= greenlabel= bluelabel= 0;
		return GROBJ_SUCCESS;
	case AXIS_WHITELABEL:
		label= l;
		redlabel= greenlabel= bluelabel= 255;
		return GROBJ_SUCCESS;
	case AXIS_GLOBALLABEL:
	case AXIS_OTHERLABEL:
		label= l;
		redlabel= r;
		greenlabel= g;
		bluelabel= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Axis::setfont(int f, char *n)
{
	switch (f) {
	case AXIS_GLOBALFONT:
	case AXIS_OTHERFONT:
		break;
	default:
		return GROBJ_FAILURE;
	}

	delete fontname;
	if (n == (char *) 0)
		fontname= (char *) 0;
	else if (strlen(n) == 0)
		fontname= (char *) 0;
	else {
		if ((fontname= new char[strlen(n)+1]) == (char *) 0)
			return GROBJ_FAILURE;
		(void) strcpy(fontname, n);
	}
	font= f;
	return GROBJ_SUCCESS;
}

int
Axis::setfontsize(int fs, float s)
{
	switch (fs) {
	case AXIS_GLOBALFONTSZ:
	case AXIS_OTHERFONTSZ:
		fontsz= fs;
		fontsize= s;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Axis::setlabelattr(int fl, int fort, float foff)
{
	switch (fl) {
	case AXIS_STDLOC:
	case AXIS_ALTLOC:
	case AXIS_NOLOC:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (fort) {
	case AXIS_FONTORIENT0:
	case AXIS_FONTORIENT90:
	case AXIS_FONTORIENT180:
	case AXIS_FONTORIENT270:
		break;
	default:
		return GROBJ_FAILURE;
	}

	fontloc= fl;
	fontorient= fort;
	fontoff= foff;

	return GROBJ_SUCCESS;
}

int
Axis::draw(FILE *outfp)
{
	int tag, rendererr;
	int x, y, flags;
	float fx, fy, tx, ty;
	double dx, dy, h, rot;
	double orig, term;
	double axislen, uval, cnt, subdivsz;
	float psx0, psx1;
	float r, g, b;
	char psbuf[128];
	Path *cpth;

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

	/* shift origin to object location */
	getloc(&fx, &fy, &x, &y);
	tx= fx*72.;
	ty= fy*72.;
	if ((tx != 0.) || (ty != 0.)) {

		/* NeWS server limitation --
		   the real magic number seems to be 0.03125,
		   but lets not push our luck */
		if ((tx >= 0.) && (tx < 0.05))
			tx= 0.05;
		else if ((tx <= 0.) && (tx > -0.05))
			tx= -0.05;
		if ((ty >= 0.) && (ty < 0.05))
			ty= 0.05;
		else if ((ty <= 0.) && (ty > -0.05))
			ty= -0.05;

		if (outfp == (FILE *) 0)
			news_translate(tx, ty);
		else
			fprintf(outfp, "%6.4f %6.4f translate\n", tx, ty);
	}

	/* rotate as necessary */
	dx= fabs((double) (ex-fx));
	dy= fabs((double) (ey-fy));
	h= hypot(dx, dy);
	psmax= h*72.;
	rot= asin((double) (dy/h));
	rot= fabs(rot);
	if (ey > fy) {
		if (ex == fx)
			rot= PI/2.;
		else if (ex < fx)
			rot= PI-rot;
	}
	else if (ey == fy) {
		if (ex >= fx)
			rot= 0.;
		else
			rot= PI;
	}
	else if (ey < fy) {
		if (ex > fx)
			rot= (PI*2.)-rot;
		if (ex == fx)
			rot= (3.*PI)/2.;
		else if (ex < fx)
			rot= PI+rot;
	}
	rot= (double) ((180.*rot)/PI);
	if (outfp == (FILE *) 0)
		news_rotate((float) rot);
	else
		fprintf(outfp, "%6.4f rotate\n", (float) rot);

	/* access font */
	if (fontloc != AXIS_NOLOC) {
		if (outfp == (FILE *) 0)
			news_font(fontname, fontsize);
		else {
			fprintf(outfp, "/%s findfont %4.2f scalefont setfont\n", fontname, fontsize);
			fprintf(outfp, "/ICEnumht %4.2f def\n", (float) (fontsize*.6));
		}
	}

	orig= (double) origin;
	term= (double) terminus;

	if (linlog == AXIS_LOG)

		/* find smallest power of 10 greater than starting point */
		for (ptenindex= 76; ptenarray[ptenindex] > orig; ptenindex--);

	else {

		/* find greatest power of 10 less than axis length */
		axislen= term-orig;
		for (ptenindex= 76; ptenarray[ptenindex] >= axislen; ptenindex--);
	}

	/* uval will move from orig through term */
	uval= 0.;

	/* cnt will count the minor tick marks between major tick marks */
	cnt= ((linlog == AXIS_LINEAR) ? ((double) subdiv) : 0.);

	/* minor tick mark every subdivsz units */
	subdivsz= ptenarray[ptenindex]/(double) subdiv;

	/* used to compare doubles for equality in epseq */
	eps= .5*ptenarray[ptenindex-2];    

	if (orig != 0.) {
		while (1) {
			if (linlog == AXIS_LOG)
				cnt+= 1.;

			/* step uval by ptenarray[ptenindex] until it passes orig */
			if (orig > 0.) {
				uval+= ptenarray[ptenindex];
				if (uval > orig)
					break;
			}
			else {
				uval-= ptenarray[ptenindex];
				if (uval < orig)
					break;
			}
		}
		if (linlog == AXIS_LINEAR) {

			/* (if log axis, uval and cnt are already set) */
			if (orig < 0.)
				cnt= 0.;
			while (1) {
				/* now move uval in the opposite direction
			   	by steps of subdivsz until it passes orig
			   	again. uval will then equal the tick mark
			   	just before or just after orig          */
				if (orig > 0.) {
					uval-= subdivsz;
					cnt-= 1.;
					if (uval-eps < orig)
						break;
				}
				else {
					uval+= subdivsz;
					cnt+= 1.;
					if (uval+eps > orig)
						break;
				}
			}
			if (orig > 0.) {
				/* now move uval in the original
			   	direction by subdivsz       */
				uval+= subdivsz;
				cnt+= 1.;
			}
		}
	}

	/* draw axis line */
	psx0= u2ps(orig);
	psx1= u2ps(term);
	r= ((float) redline)/255.;
	g= ((float) greenline)/255.;
	b= ((float) blueline)/255.;
	if (outfp == (FILE *) 0) {
		news_setrgbcolor(r, g, b);
		news_setlinewidth(axiswidth);
		news_drawline((float) (-tickwidth/2.), (float) 0., (float) (psmax+(tickwidth/2.)), (float) 0.);
		news_setlinewidth(tickwidth);
	}
	else {
		fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
		fprintf(outfp, "%4.2f setlinewidth\n", axiswidth);
		fprintf(outfp, "newpath\n");
		fprintf(outfp, "%6.4f 0 moveto\n", (float) (-tickwidth/2.));
		fprintf(outfp, "%6.4f 0 lineto\n", (float) (psmax+(tickwidth/2.)));
		fprintf(outfp, "stroke\n");
		fprintf(outfp, "%4.2f setlinewidth\n", tickwidth);
	}

	/* draw major tick mark at start point */
	drawptick(orig, psx0, 1, outfp);

	if (linlog == AXIS_LOG) {
		while (uval <= term+eps) {
			psx0= u2ps(uval);
			if (cnt == 10.) {
				drawptick(uval, psx0, 0, outfp);
				cnt= 1.;
				ptenindex++;
			}
			else
				drawstick(psx0, outfp);
			uval+= ptenarray[ptenindex];
			cnt+= 1.;
		}
	}
	else {
		while (uval <= term+eps) {
			psx0= u2ps(uval);
			if (epseq(cnt, (double) subdiv, .1)) {
				/* at major tick mark */
				drawptick(uval, psx0, 0, outfp);
				cnt= 1.;
			}
			else {	
				/* output middle tick mark if subdiv even
			   	and halfway between major tick marks */
				if (epseq(cnt, (double) subdiv*.5, .1))
					drawstick(psx0, outfp);
				else
					drawttick(psx0, outfp);
				cnt+= 1.;
			}
			uval+= subdivsz;
		}
	}

	/* draw major tick mark at end point */
	drawptick(term, psx1, 0, outfp);

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

void
Axis::drawptick(double uval, float psval, int force, FILE *fp)
{
	char buf[30], prefix[2];
	float r, g, b;
	float psy0, psy1, psx, psy;
	float numht, mint, maxt, strwidth;

	/*  plot tick */
	if (tickloc != AXIS_NOLOC) {
		r= ((float) redline)/255.;
		g= ((float) greenline)/255.;
		b= ((float) blueline)/255.;
		if (tickloc == AXIS_STDLOC) {
			psy0= -ptickht;
			psy1= 0.;
		}
		else if (tickloc == AXIS_ALTLOC) {
			psy0= 0.;
			psy1= ptickht;
		}
		if (fp == (FILE *) 0) {
			news_setrgbcolor(r, g, b);
			news_drawline(psval, psy0, psval, psy1);
		}
		else {
			fprintf(fp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
			fprintf(fp, "newpath\n");
			fprintf(fp, "0 0 moveto\n");
			fprintf(fp, "%6.4f %6.4f moveto\n", psval, psy0);
			fprintf(fp, "%6.4f %6.4f lineto\n", psval, psy1);
			fprintf(fp, "stroke\n");
		}
	}

	if (fontloc == AXIS_NOLOC)
		return;

	r= ((float) redlabel)/255.;
	g= ((float) greenlabel)/255.;
	b= ((float) bluelabel)/255.;

	/* create label */
	if (epseq(0., uval, ptenarray[ptenindex-5]))
		uval= 0.;
	(void) gconvert(uval, 8, 0, buf);
	if (fp == (FILE *) 0)
		news_strwidth(buf, &strwidth);
	else
		fprintf(fp, "/ICEcurrstrwidth (%s) stringwidth pop def\n", buf);

	psx= psval;
	numht= fontsize*.6;
	mint= 0.;
	if (mint > ptickht)
		mint= ptickht;
	if (mint > stickht)
		mint= stickht;
	if (mint > ttickht)
		mint= ttickht;
	if (mint < 0.)
		mint= -mint;
	maxt= 0.;
	if (maxt < ptickht)
		maxt= ptickht;
	if (maxt < stickht)
		maxt= stickht;
	if (maxt < ttickht)
		maxt= ttickht;

	switch (tickloc) {
	case AXIS_STDLOC:
		if (fontloc == AXIS_STDLOC)
			psy= -maxt-fontoff;
		else
			psy= mint+fontoff;
		break;
	case AXIS_ALTLOC:
		if (fontloc == AXIS_STDLOC)
			psy= -mint-fontoff;
		else
			psy= maxt+fontoff;
		break;
	case AXIS_NOLOC:
		if (fontloc == AXIS_STDLOC)
			psy= -fontoff;
		else
			psy= fontoff;
		break;
	}

	if (fp == (FILE *) 0) {
		switch (fontorient) {
		case AXIS_FONTORIENT0:
			if (fontloc == AXIS_STDLOC) {
				psx-= strwidth/2.;
				psy-= numht;
			}
			else
				psx-= strwidth/2.;
			if (!force && (lastlabelpos >= psx))
				return;
			lastlabelpos= psx+strwidth;
			news_setrgbcolor(r, g, b);
			news_show(psx, psy, buf);
			break;
		case AXIS_FONTORIENT90:
			if (fontloc == AXIS_STDLOC) {
				psx+= numht/2.;
				psy-= strwidth;
			}
			else
				psx+= numht/2.;
			if (!force && (lastlabelpos >= psx-numht))
				return;
			lastlabelpos= psx;
			ps_gsave();
			news_setrgbcolor(r, g, b);
			news_translate(psx, psy);
			news_rotate((float) 90.);
			news_show((float) 0., (float) 0., buf);
			ps_grestore();
			break;
		case AXIS_FONTORIENT180:
			if (fontloc == AXIS_STDLOC)
				psx+= strwidth/2.;
			else {
				psx+= strwidth/2.;
				psy+= numht;
			}
			if (!force && (lastlabelpos >= psx-strwidth))
				return;
			lastlabelpos= psx;
			ps_gsave();
			news_setrgbcolor(r, g, b);
			news_translate(psx, psy);
			news_rotate((float) 180.);
			news_show((float) 0., (float) 0., buf);
			ps_grestore();
			break;
		case AXIS_FONTORIENT270:
			if (fontloc == AXIS_STDLOC)
				psx-= numht/2.;
			else {
				psx-= numht/2.;
				psy+= strwidth;
			}
			if (!force && (lastlabelpos >= psx))
				return;
			lastlabelpos= psx+numht;
			ps_gsave();
			news_setrgbcolor(r, g, b);
			news_translate(psx, psy);
			news_rotate((float) 270.);
			news_show((float) 0., (float) 0., buf);
			ps_grestore();
			break;
		}
	}
	else {
		if (force)
			strcpy(prefix, "");
		else
			strcpy(prefix, "\t");
		switch (fontorient) {
		case AXIS_FONTORIENT0:
			if (!force)
				fprintf(fp, "ICElastlabelpos %6.4f ICEcurrstrwidth 2 div sub lt {\n", psx);
			fprintf(fp, "%s%4.2f %4.2f %4.2f setrgbcolor\n", prefix, r, g, b);
			if (fontloc == AXIS_STDLOC)
				fprintf(fp, "%s%6.4f ICEcurrstrwidth 2 div sub %6.4f moveto\n", prefix, psx, (float) (psy-numht));
			else
				fprintf(fp, "%s%6.4f ICEcurrstrwidth 2 div sub %6.4f moveto\n", prefix, psx, psy);
			fprintf(fp, "%s(%s) show\n", prefix, buf);
			fprintf(fp, "%s/ICElastlabelpos %6.4f ICEcurrstrwidth 2 div add def\n", prefix, psx);
			if (!force)
				fprintf(fp, "} if\n");
			break;
		case AXIS_FONTORIENT90:
			if (!force)
				fprintf(fp, "ICElastlabelpos %6.4f lt {\n", (float) (psx-(numht/2.)));
			fprintf(fp, "%sgsave\n", prefix);
			fprintf(fp, "%s%4.2f %4.2f %4.2f setrgbcolor\n", prefix, r, g, b);
			if (fontloc == AXIS_STDLOC)
				fprintf(fp, "%s%6.4f %6.4f ICEcurrstrwidth sub translate\n", prefix, (float) (psx+(numht/2.)), psy);
			else
				fprintf(fp, "%s%6.4f %6.4f translate\n", prefix, (float) (psx+(numht/2.)), psy);
			fprintf(fp, "%s90 rotate 0 0 moveto (%s) show\n", prefix, buf);
			fprintf(fp, "%s/ICElastlabelpos %6.4f def\n", prefix, (float) (psx+(numht/2.)));
			fprintf(fp, "%sgrestore\n", prefix);
			if (!force)
				fprintf(fp, "} if\n");
			break;
		case AXIS_FONTORIENT180:
			if (!force)
				fprintf(fp, "ICElastlabelpos %6.4f ICEcurrstrwidth 2 div sub lt {\n", psx);
			fprintf(fp, "%sgsave\n", prefix);
			fprintf(fp, "%s%4.2f %4.2f %4.2f setrgbcolor\n", prefix, r, g, b);
			if (fontloc == AXIS_STDLOC)
				fprintf(fp, "%s%6.4f ICEcurrstrwidth 2 div add %6.4f translate\n", prefix, psx, psy);
			else
				fprintf(fp, "%s%6.4f ICEcurrstrwidth 2 div add %6.4f translate\n", prefix, psx, (float) (psy+numht));
			fprintf(fp, "%s180 rotate 0 0 moveto (%s) show\n", prefix, buf);
			fprintf(fp, "%s/ICElastlabelpos %6.4f ICEcurrstrwidth 2 div add def\n", prefix, psx);
			fprintf(fp, "%sgrestore\n", prefix);
			if (!force)
				fprintf(fp, "} if\n");
			break;
		case AXIS_FONTORIENT270:
			if (!force)
				fprintf(fp, "ICElastlabelpos %6.4f lt {\n", (float) (psx-(numht/2.)));
			fprintf(fp, "%sgsave\n", prefix);
			fprintf(fp, "%s%4.2f %4.2f %4.2f setrgbcolor\n", prefix, r, g, b);
			if (fontloc == AXIS_STDLOC)
				fprintf(fp, "%s%6.4f %6.4f translate\n", prefix, (float) (psx-(numht/2.)), psy);
			else
				fprintf(fp, "%s%6.4f %6.4f ICEcurrstrwidth add translate\n", prefix, (float) (psx-(numht/2.)), psy);
			fprintf(fp, "%s270 rotate 0 0 moveto (%s) show\n", prefix, buf);
			fprintf(fp, "%s/ICElastlabelpos %6.4f def\n", prefix, (float) (psx+(numht/2.)));
			fprintf(fp, "%sgrestore\n", prefix);
			if (!force)
				fprintf(fp, "} if\n");
			break;
		}
	}
	return;
}

void
Axis::drawstick(float psval, FILE *fp)
{
	float r, g, b, psy0, psy1;

	if (tickloc == AXIS_NOLOC)
		return;

	r= ((float) redline)/255.;
	g= ((float) greenline)/255.;
	b= ((float) blueline)/255.;
	if (tickloc == AXIS_STDLOC) {
		psy0= -stickht;
		psy1= 0.;
	}
	else if (tickloc == AXIS_ALTLOC) {
		psy0= 0.;
		psy1= stickht;
	}
	if (fp == (FILE *) 0) {
		news_setrgbcolor(r, g, b);
		news_drawline(psval, psy0, psval, psy1);
	}
	else {
		fprintf(fp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
		fprintf(fp, "newpath\n");
		fprintf(fp, "0 0 moveto\n");
		fprintf(fp, "%6.4f %6.4f moveto\n", psval, psy0);
		fprintf(fp, "%6.4f %6.4f lineto\n", psval, psy1);
		fprintf(fp, "stroke\n");
	}
	return;
}

void
Axis::drawttick(float psval, FILE *fp)
{
	float r, g, b, psy0, psy1;

	if (tickloc == AXIS_NOLOC)
		return;

	r= ((float) redline)/255.;
	g= ((float) greenline)/255.;
	b= ((float) blueline)/255.;
	if (tickloc == AXIS_STDLOC) {
		psy0= -ttickht;
		psy1= 0.;
	}
	else if (tickloc == AXIS_ALTLOC) {
		psy0= 0.;
		psy1= ttickht;
	}
	if (fp == (FILE *) 0) {
		news_setrgbcolor(r, g, b);
		news_drawline(psval, psy0, psval, psy1);
	}
	else {
		fprintf(fp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
		fprintf(fp, "newpath\n");
		fprintf(fp, "0 0 moveto\n");
		fprintf(fp, "%6.4f %6.4f moveto\n", psval, psy0);
		fprintf(fp, "%6.4f %6.4f lineto\n", psval, psy1);
		fprintf(fp, "stroke\n");
	}
	return;
}

void
Axis::setumap(double u1, double u2)
{
	if (linlog == AXIS_LOG) {
		umin= LOG10(u1);
		umax= LOG10(u2);
	}
	else {
		umin= u1;
		umax= u2;
	}
	udiff= umax-umin;
	return;
}

float
Axis::u2ps(double u)
{
	double a, b;

	if (linlog == AXIS_LOG)
		a= LOG10(u);
	else
		a= u;
	b= ((a-umin)/udiff)*psmax;
	return (float) b;
}

float
Axis::ps2u(double ps)
{
	double a, factor;

	factor= ps/psmax;
	a= factor*udiff+umin;
	if (linlog == AXIS_LOG)
		a= POW10(a);
	return (float) a;
}

int
Axis::epseq(double x, double y, double e)
/* equal comparison of doubles */
{
	if ((x < y-e) || (x > y+e))
		return 0;
	else
		return 1;
}

int
Axis::icedump(FILE *fp)
{
	float fx, fy;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Axis: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Axis: Loc %6.4f %6.4f %6.4f %6.4f\n", fx, fy, ex, ey);

	fprintf(fp, "%%%%ICE-Axis: OTL %.6e %.6e %d\n", origin, terminus, linlog);

	fprintf(fp, "%%%%ICE-Axis: AF %1d %1d %4.2f\n", subdiv, axiswd, axiswidth);

	fprintf(fp, "%%%%ICE-Axis: TF %1d %4.2f %4.2f %4.2f %1d %4.2f\n", tickloc, ptickht, stickht, ttickht, tickwd, tickwidth);

	fprintf(fp, "%%%%ICE-Axis: Line %1d %1d %1d %1d\n", line, (int) redline, (int) greenline, (int) blueline);

	fprintf(fp, "%%%%ICE-Axis: Font %1d %1d %s\n", fontloc, font, fontname);

	fprintf(fp, "%%%%ICE-Axis: FF %1d %4.2f %1d %4.2f\n", fontsz, fontsize, fontorient, fontoff);

	fprintf(fp, "%%%%ICE-Axis: Label %1d %1d %1d %1d\n", label, (int) redlabel, (int) greenlabel, (int) bluelabel);

	if ((pth= getclip()) != (Path *) 0)
		fprintf(fp, "%%%%ICE-Axis: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Axis: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Axis: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Axis: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Axis: End\n");
	return GROBJ_SUCCESS;
}

Axis::~Axis()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) 0)
		(void) cpth->setreferences(PATH_REFDECR);
	delete fontname;
}
