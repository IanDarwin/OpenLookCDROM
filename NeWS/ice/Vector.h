/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __VECTOR_CLASS__
#define __VECTOR_CLASS__

#include "Intobj.h"

/* these must begin at 0! */
#define VECTOR_GLOBALWIDTH	(0)
#define VECTOR_OTHERWIDTH	(1)

/* these must begin at 0! */
#define VECTOR_SOLID		(0)
#define VECTOR_DASHED		(1)

/* these must begin at 0! */
#define VECTOR_SIMPLEDASH	(0)
#define VECTOR_COMPLEXDASH	(1)

/* these must begin at 0! */
#define VECTOR_BUTTCAP		(0)
#define VECTOR_ROUNDCAP		(1)
#define VECTOR_SQUARECAP	(2)

/* these must begin at 0! */
#define VECTOR_NOPTR		(0)
#define VECTOR_ORIGPTR		(1)
#define VECTOR_TERMPTR		(2)
#define VECTOR_BOTHPTR		(3)

/* these must begin at 0! */
#define VECTOR_OPENPTR		(0)
#define VECTOR_CLOSEDPTR	(1)

/* these must begin at 0! */
#define VECTOR_GLOBALFG		(0)
#define VECTOR_BLACKFG		(1)
#define VECTOR_WHITEFG		(2)
#define VECTOR_OTHERFG		(3)

class Vector : public Intobj {

	float ex, ey;
	int pex, pey;
	int linewd;
	float width;
	int linestyle;
	int dashstyle;
	float *dashpattern;
	int dashpatternlen;
	float dashpatternoffset;
	int capstyle;
	int pointer;
	int pointerstyle;
	float pointerwd;
	float pointerolen;
	float pointerilen;
	int foreground;
	unsigned char redfg, greenfg, bluefg;

public:
	/* constructors */
	Vector();
	Vector(Dlnk **, char *, int);

	void getend(float *fx, float *fy, int *ix, int *iy)
						{ *fx= ex; *fy= ey;
						  *ix= pex; *iy= pey; }
	void setfend(float, float, float);
	void setpend(int, int, float);
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
	void getptrstyle(int *p, int *ps, float *pw, float *po, float *pi)
						{ *p= pointer;
						  *ps= pointerstyle;
						  *pw= pointerwd; 
						  *po= pointerolen;
						  *pi= pointerilen; }
	int setptrstyle(int, int, float, float, float);
	void getforeground(int *fg,
			   unsigned char *r, unsigned char *g, unsigned char *b)
						{ *fg= foreground; *r= redfg;
						  *g= greenfg; *b= bluefg; }
	int setforeground(int, unsigned char, unsigned char, unsigned char);
	char *patternstr();

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Vector();
};

#endif __VECTOR_CLASS__
