/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __PATH_CLASS__
#define __PATH_CLASS__

#include <local/Dlnk.h>

#define PATH_SUCCESS		(0)
#define PATH_FAILURE		(1)

/* these must begin at 0! */
#define PATH_OPEN		(0)
#define PATH_CLOSED		(1)

/* these must begin at 0! */
#define PATH_INVISIBLE		(0)
#define PATH_VISIBLE		(1)

#define PATH_REFINCR		(1)
#define PATH_REFDECR		(2)

#define PATH_DUMP		(1)
#define PATH_NODUMP		(2)

/* these must begin at 0! */
#define PATH_BLACKFG		(0)
#define PATH_WHITEFG		(1)
#define PATH_OTHERFG		(2)

#define PATH_DRAWCLOSE		(0x00000001)
#define PATH_DRAWCLIP		(0x00000002)
#define PATH_DRAWSTROKE		(0x00000004)
#define PATH_DRAWFILL		(0x00000008)
#define PATH_DRAWNONEWPATH	(0x00000010)

class Path : public Dlnk {

	char *name;
	int vertices;
	float *xvertices;
	float *yvertices;
	int closure;
	int visibility;
	int references;
	int dump;
	Display *display;
	int depth;
	int foreground;
	unsigned char redfg, greenfg, bluefg;
	unsigned long fgpix;
	GC fullgc;

public:

	/* constructor */
	Path();
	Path(Dlnk **, char *, Display *, int);

	char *getname()				{ return name; }
	int setname(char *);
	void getvertices(int *n, float **x, float **y)
						{ *n= vertices;
						  *x= xvertices; *y= yvertices; }
	int setvertices(int, float *, float *);
	int getclosure()			{ return closure; }
	int setclosure(int);
	int getvisibility()			{ return visibility; }
	int setvisibility(int);
	int getreferences()			{ return references; }
	int setreferences(int);
	int getdump()				{ return dump; }
	int setdump(int);
	void getfg(int *fg, unsigned long *fgp, unsigned char *r,
		   unsigned char *g, unsigned char *b)
						{ *fg= foreground; *fgp= fgpix;
						  *r= redfg; *g= greenfg;
						  *b= bluefg; }
	int setfg(int, unsigned long, unsigned char, unsigned char, unsigned char);

	void draw(int, FILE *);
	void x11draw(int, int, Pixmap);
	int icedump(FILE *);

	/* destructor */
	~Path();
};

#endif __PATH_CLASS__
