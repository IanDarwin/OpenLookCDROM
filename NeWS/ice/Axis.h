/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __AXIS_CLASS__
#define __AXIS_CLASS__

#include "Intobj.h"

/* these must begin at 0! */
#define AXIS_LINEAR		(0)
#define AXIS_LOG		(1)

/* these must begin at 0! */
#define AXIS_GLOBALWIDTH	(0)
#define AXIS_OTHERWIDTH		(1)

/* these must begin at 0! */
#define AXIS_STDLOC		(0)
#define AXIS_ALTLOC		(1)
#define AXIS_NOLOC		(2)

/* these must begin at 0! */
#define AXIS_GLOBALFONT		(0)
#define AXIS_OTHERFONT		(1)

/* these must begin at 0! */
#define AXIS_GLOBALFONTSZ	(0)
#define AXIS_OTHERFONTSZ	(1)

/* these must begin at 0! */
#define AXIS_FONTORIENT0	(0)
#define AXIS_FONTORIENT90	(1)
#define AXIS_FONTORIENT180	(2)
#define AXIS_FONTORIENT270	(3)

/* these must begin at 0! */
#define AXIS_GLOBALLINE		(0)
#define AXIS_BLACKLINE		(1)
#define AXIS_WHITELINE		(2)
#define AXIS_OTHERLINE		(3)

/* these must begin at 0! */
#define AXIS_GLOBALLABEL	(0)
#define AXIS_BLACKLABEL		(1)
#define AXIS_WHITELABEL		(2)
#define AXIS_OTHERLABEL		(3)

class Axis : public Intobj {

	float ex, ey;
	int pex, pey;
	double origin;
	double terminus;
	int linlog;
	int subdiv;
	int axiswd;
	float axiswidth;
	int tickloc;
	float ptickht, stickht, ttickht;
	int tickwd;
	float tickwidth;
	int fontloc;
	int font;
	char *fontname;
	int fontsz;
	float fontsize;
	int fontorient;
	float fontoff;
	int line;
	unsigned char redline, greenline, blueline;
	int label;
	unsigned char redlabel, greenlabel, bluelabel;
	double umin, umax, udiff;
	double psmax;
	void drawptick(double, float, int, FILE *);
	void drawstick(float, FILE *);
	void drawttick(float, FILE *);
	int epseq(double, double, double);
	void setumap(double, double);
	float u2ps(double);
	float ps2u(double);

public:
	/* constructors */
	Axis();
	Axis(Dlnk **, char *, int);

	void getend(float *fx, float *fy, int *ix, int *iy)
					{ *fx= ex; *fy= ey; *ix= pex; *iy= pey; }
	void setfend(float, float, float);
	void setpend(int, int, float);
	void getotl(double *o, double *t, int *l) { *o= origin; *t= terminus;
						  *l= linlog; }
	int setotl(double, double, int);
	int getsubdiv()				{ return subdiv; }
	int setsubdiv(int);
	void getaxiswidth(int *aw, float *w)	{ *aw= axiswd; *w= axiswidth; }
	int setaxiswidth(int, float);
	void gettick(int *tl, float *pt, float *st, float *tt, int *tw, float *w)
						{ *tl= tickloc; *pt= ptickht;
						  *st= stickht; *tt= ttickht;
						  *tw= tickwd; *w= tickwidth; }
	int settick(int, float, float, float, int, float);
	void getfont(int *f, char **n)		{ *f= font; *n= fontname; }
	int setfont(int, char *);
	void getfontsize(int *fs, float *s)	{ *fs= fontsz; *s= fontsize; }
	int setfontsize(int, float);
	void getlabelattr(int *fl, int *fo, float *foff)
						{ *fl= fontloc; *fo= fontorient;
						  *foff= fontoff; }
	int setlabelattr(int, int, float);
	void getline(int *l,
			   unsigned char *r, unsigned char *g, unsigned char *b)
						{ *l= line; *r= redline;
						  *g= greenline; *b= blueline; }
	int setline(int, unsigned char, unsigned char, unsigned char);
	void getlabel(int *l,
			   unsigned char *r, unsigned char *g, unsigned char *b)
						{ *l= label; *r= redlabel;
						  *g= greenlabel; *b= bluelabel; }
	int setlabel(int, unsigned char, unsigned char, unsigned char);

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Axis();
};

#endif __AXIS_CLASS__
