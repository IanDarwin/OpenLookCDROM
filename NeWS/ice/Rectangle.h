/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __RECTANGLE_CLASS__
#define __RECTANGLE_CLASS__

#include "Intobj.h"

/* these must begin at 0! */
#define RECTANGLE_OPAQUEBNDM		(0)
#define RECTANGLE_TRANSPARENTBNDM	(1)

/* these must begin at 0! */
#define RECTANGLE_GLOBALBNDWIDTH	(0)
#define RECTANGLE_OTHERBNDWIDTH		(1)

/* these must begin at 0! */
#define RECTANGLE_SOLID			(0)
#define RECTANGLE_DASHED		(1)

/* these must begin at 0! */
#define RECTANGLE_SIMPLEDASH		(0)
#define RECTANGLE_COMPLEXDASH		(1)

/* these must begin at 0! */
#define RECTANGLE_TRANSPARENTFILLM	(0)
#define RECTANGLE_OPAQUEFILLM		(1)

/* these must begin at 0! */
#define RECTANGLE_GLOBALBND		(0)
#define RECTANGLE_BLACKBND		(1)
#define RECTANGLE_WHITEBND		(2)
#define RECTANGLE_OTHERBND		(3)

/* these must begin at 0! */
#define RECTANGLE_GLOBALFILL		(0)
#define RECTANGLE_WHITEFILL		(1)
#define RECTANGLE_BLACKFILL		(2)
#define RECTANGLE_OTHERFILL		(3)

class Rectangle : public Intobj {

	float width, height;
	int boundarymode;
	int boundarywd;
	float bndwidth;
	int linestyle;
	int dashstyle;
	float *dashpattern;
	int dashpatternlen;
	float dashpatternoffset;
	int boundarycolor;
	unsigned char redbnd, greenbnd, bluebnd;
	int fillmode;
	int fillcolor;
	unsigned char redfill, greenfill, bluefill;

public:
	/* constructors */
	Rectangle();
	Rectangle(Dlnk **, char *, int);

	void getsize(float *w, float *h)
						{ *w= width; *h= height; }
	int setsize(float, float);
	void getboundary(int *bm, int *bc,
			 unsigned char *r, unsigned char *g, unsigned char *b)
						{ *bm= boundarymode;
						  *bc= boundarycolor;
						  *r= redbnd; *g= greenbnd;
						  *b= bluebnd; }
	int setboundary(int, int, unsigned char, unsigned char, unsigned char);
	void getwidth(int *bw, float *w)	{ *bw= boundarywd; *w= bndwidth; }
	int setwidth(int, float);
	int getlinestyle()			{ return linestyle; }
	int setlinestyle(int);
	void getdashstyle(int *ds, float **dp, int *dpl, float *dpo)
						{ *ds= dashstyle;
						  *dp= dashpattern;
						  *dpl= dashpatternlen;
						  *dpo= dashpatternoffset; }
	int setdashstyle(int, float *, int, float);
	void getfill(int *fm, int *fc,
		     unsigned char *r, unsigned char *g, unsigned char *b)
						{ *fm= fillmode;
						  *fc= fillcolor;
						  *r= redfill; *g= greenfill;
						  *b= bluefill; }
	int setfill(int, int, unsigned char, unsigned char, unsigned char);
	char *patternstr();

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Rectangle();
};

#endif __RECTANGLE_CLASS__
