/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __POLYGON_CLASS__
#define __POLYGON_CLASS__

#include "Intobj.h"

/* these must begin at 0! */
#define POLYGON_CLOSED			(0)
#define POLYGON_OPEN			(1)

/* these must begin at 0! */
#define POLYGON_GLOBALBNDWIDTH		(0)
#define POLYGON_OTHERBNDWIDTH		(1)

/* these must begin at 0! */
#define POLYGON_OPAQUEBNDM		(0)
#define POLYGON_TRANSPARENTBNDM		(1)

/* these must begin at 0! */
#define POLYGON_TRANSPARENTFILLM	(0)
#define POLYGON_OPAQUEFILLM		(1)

/* these must begin at 0! */
#define POLYGON_GLOBALBND		(0)
#define POLYGON_BLACKBND		(1)
#define POLYGON_WHITEBND		(2)
#define POLYGON_OTHERBND		(3)

/* these must begin at 0! */
#define POLYGON_GLOBALFILL		(0)
#define POLYGON_WHITEFILL		(1)
#define POLYGON_BLACKFILL		(2)
#define POLYGON_OTHERFILL		(3)

class Polygon : public Intobj {

	int vertices;
	float *xvertices;
	float *yvertices;
	int closure;
	int boundarymode;
	int boundarywd;
	float bndwidth;
	int boundarycolor;
	unsigned char redbnd, greenbnd, bluebnd;
	int fillmode;
	int fillcolor;
	unsigned char redfill, greenfill, bluefill;

public:
	/* constructors */
	Polygon();
	Polygon(Dlnk **, char *, int);

	void getvertices(int *n, float **x, float **y)
						{ *n= vertices;
						  *x= xvertices; *y= yvertices; }
	int setvertices(int, float *, float *);
	int getclosure()			{ return closure; }
	int setclosure(int);
	void getboundary(int *bm, int *bw, float *w, int *bc,
			 unsigned char *r, unsigned char *g, unsigned char *b)
						{ *bm= boundarymode;
						  *bw= boundarywd; *w= bndwidth;
						  *bc= boundarycolor;
						  *r= redbnd; *g= greenbnd;
						  *b= bluebnd; }
	int setboundary(int, int, float, int, unsigned char, unsigned char,
		        unsigned char);
	void getfill(int *fm, int *fc,
		     unsigned char *r, unsigned char *g, unsigned char *b)
						{ *fm= fillmode;
						  *fc= fillcolor;
						  *r= redfill; *g= greenfill;
						  *b= bluefill; }
	int setfill(int, int, unsigned char, unsigned char, unsigned char);

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Polygon();
};

#endif __POLYGON_CLASS__
