/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __CURVE_CLASS__
#define __CURVE_CLASS__

#include "Intobj.h"

/* these must begin at 0! */
#define CURVE_GLOBALWIDTH	(0)
#define CURVE_OTHERWIDTH	(1)

/* these must begin at 0! */
#define CURVE_SOLID		(0)
#define CURVE_DASHED		(1)

/* these must begin at 0! */
#define CURVE_SIMPLEDASH	(0)
#define CURVE_COMPLEXDASH	(1)

/* these must begin at 0! */
#define CURVE_BUTTCAP		(0)
#define CURVE_ROUNDCAP		(1)
#define CURVE_SQUARECAP	(2)

/* these must begin at 0! */
#define CURVE_GLOBALFG		(0)
#define CURVE_BLACKFG		(1)
#define CURVE_WHITEFG		(2)
#define CURVE_OTHERFG		(3)

class Curve : public Intobj {

	float ex, ey;
	int pex, pey;
	float c1x, c1y;
	int pc1x, pc1y;
	float c2x, c2y;
	int pc2x, pc2y;
	int linewd;
	float width;
	int linestyle;
	int dashstyle;
	float *dashpattern;
	int dashpatternlen;
	float dashpatternoffset;
	int capstyle;
	int foreground;
	unsigned char redfg, greenfg, bluefg;

public:
	/* constructors */
	Curve();
	Curve(Dlnk **, char *, int);

	void getend(float *fx, float *fy, int *ix, int *iy)
						{ *fx= ex; *fy= ey;
						  *ix= pex; *iy= pey; }
	void setfend(float, float, float);
	void setpend(int, int, float);
	void getcontrol1(float *fx, float *fy, int *ix, int *iy)
						{ *fx= c1x; *fy= c1y;
						  *ix= pc1x; *iy= pc1y; }
	void setfcontrol1(float, float, float);
	void setpcontrol1(int, int, float);
	void getcontrol2(float *fx, float *fy, int *ix, int *iy)
						{ *fx= c2x; *fy= c2y;
						  *ix= pc2x; *iy= pc2y; }
	void setfcontrol2(float, float, float);
	void setpcontrol2(int, int, float);
	void getwidth(int *lw, float *w)	{ *lw= linewd; *w= width; }
	int setwidth(int, float);
	int getlinestyle()			{ return linestyle; }
	int setlinestyle(int);
	void getdashstyle(int *ds, float **dp, int *dpl, float *dpo)
						{ *ds= dashstyle;
						  *dp= dashpattern;
						  *dpl= dashpatternlen;
						  *dpo= dashpatternoffset; }
	int setdashstyle(int, float *, int, float);
	int getcapstyle()			{ return capstyle; }
	int setcapstyle(int);
	void getforeground(int *fg,
			   unsigned char *r, unsigned char *g, unsigned char *b)
						{ *fg= foreground; *r= redfg;
						  *g= greenfg; *b= bluefg; }
	int setforeground(int, unsigned char, unsigned char, unsigned char);
	char *patternstr();

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Curve();
};

#endif __CURVE_CLASS__
