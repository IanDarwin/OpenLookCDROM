/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __RASTER_CLASS__
#define __RASTER_CLASS__

#include "Grobj.h"

/* these must begin at 0! */
#define RASTER_ULORIG		(0)
#define RASTER_LLORIG		(1)
#define RASTER_LRORIG		(2)
#define RASTER_URORIG		(3)

/* these must begin at 0! */
#define RASTER_FULL		(0)
#define RASTER_OUTLINE		(1)

/* these must begin at 0! */
#define RASTER_GLOBALFG		(0)
#define RASTER_BLACKFG		(1)
#define RASTER_WHITEFG		(2)
#define RASTER_OTHERFG		(3)

/* these must begin at 0! */
#define RASTER_GLOBALBG		(0)
#define RASTER_WHITEBG		(1)
#define RASTER_BLACKBG		(2)
#define RASTER_OTHERBG		(3)

struct intensity {
	int ival;
	int imin;
	int imax;
};

class Raster : public Grobj {

	char *filenm;
	int width, height, depth;
	int pixrep;
	int origloc;
	int drawmode;
	Display *display;
	Visual *visual;
	int foreground;
	unsigned char redfg, greenfg, bluefg;
	unsigned long fgpix;
	int background;
	unsigned char redbg, greenbg, bluebg;
	unsigned long bgpix;
	GC fullgc;
	unsigned long outline;
	GC outlinegc;
	Colormap cmap;
	XImage *image;
	intensity *redint, *greenint, *blueint;
	struct rasterfile sunras;
	colormap_t suncmap;

public:
	/* constructors */
	Raster();
	Raster(Dlnk **, char *, Display *, Visual *, unsigned long, int, int);

	char *getname()				{ return filenm; }
	void getsize(int *w, int *h, int *d)	{ *w= width; *h= height; *d= depth; }
	void getrassize(int *w, int *h, int *d)	{ *w= sunras.ras_width;
						  *h= sunras.ras_height;
						  *d= sunras.ras_depth; }
	int getpixrep()				{ return pixrep; }
	int setpixrep(int);
	int getorigloc()			{ return origloc; }
	int setorigloc(int);
	int getdrawmode()			{ return drawmode; }
	int setdrawmode(int);
	Colormap getcolormap()			{ return cmap; }
	void getfg(int *fg, unsigned long *fgp, unsigned char *r,
		   unsigned char *g, unsigned char *b)
						{ *fg= foreground; *fgp= fgpix;
						  *r= redfg; *g= greenfg;
						  *b= bluefg; }
	int setfg(int, unsigned long, unsigned char, unsigned char, unsigned char);
	void getbg(int *bg, unsigned long *bgp, unsigned char *r,
		   unsigned char *g, unsigned char *b)
						{ *bg= background; *bgp= bgpix;
						  *r= redbg; *g= greenbg;
						  *b= bluebg; }
	int setbg(int, unsigned long, unsigned char, unsigned char, unsigned char);

	int bldimage();
	int bldhistcmap(Pixrect *);
	unsigned long pseudopix(unsigned long);
	void copy(Drawable);
	int dump(Pixrect *, int, int);
	int icedump(FILE *);

	/* destructor */
	~Raster();
};

#endif __RASTER_CLASS__
