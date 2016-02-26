/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __MARKER_CLASS__
#define __MARKER_CLASS__

#include "Intobj.h"

/* these must begin at 0! */
#define MARKER_GLOBALTYPE	(0)
#define MARKER_SQUARE		(1)
#define MARKER_TRIANGLE		(2)
#define MARKER_CIRCLE		(3)
#define MARKER_CROSS		(4)

/* these must begin at 0! */
#define MARKER_GLOBALSIZE	(0)
#define MARKER_OTHERSIZE	(1)

/* these must begin at 0! */
#define MARKER_GLOBALBNDWIDTH	(0)
#define MARKER_OTHERBNDWIDTH	(1)

/* these must begin at 0! */
#define MARKER_OPAQUEBNDM	(0)
#define MARKER_TRANSPARENTBNDM	(1)

/* these must begin at 0! */
#define MARKER_TRANSPARENTFILLM	(0)
#define MARKER_OPAQUEFILLM	(1)

/* these must begin at 0! */
#define MARKER_GLOBALBND	(0)
#define MARKER_BLACKBND		(1)
#define MARKER_WHITEBND		(2)
#define MARKER_OTHERBND		(3)

/* these must begin at 0! */
#define MARKER_GLOBALFILL	(0)
#define MARKER_WHITEFILL	(1)
#define MARKER_BLACKFILL	(2)
#define MARKER_OTHERFILL	(3)

class Marker : public Intobj {

	int markertype;
	int mrktype;
	int size;
	float radius;
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
	Marker();
	Marker(Dlnk **, char *, int);

	void getmarkertype(int *mt, int *t)	{ *mt= markertype; *t= mrktype; }
	int setmarkertype(int, int);
	void getsize(int *s, float *r)		{ *s= size; *r= radius; }
	int setsize(int, float);
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
	~Marker();
};

#endif __MARKER_CLASS__
