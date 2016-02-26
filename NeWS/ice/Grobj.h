/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __GROBJ_CLASS__
#define __GROBJ_CLASS__

#include <local/Dlnk.h>

#include "Path.h"

#define GROBJ_SUCCESS		(0)
#define GROBJ_FAILURE		(1)

#define GROBJ_UNDEFINED		(0)

#define GROBJ_PSDOC		(1)
#define GROBJ_RASTER		(2)
#define GROBJ_INTOBJ		(3)
#define GROBJ_COMPOSITE		(4)

/* these must begin at 0! */
#define GROBJ_GLOBALDTK		(0)
#define GROBJ_WHITEDTK		(1)
#define GROBJ_BLACKDTK		(2)
#define GROBJ_OTHERDTK		(3)

#define GROBJ_NULLIOTAG		(0)

class Grobj : public Dlnk {

	float x, y;
	int px, py;
	int type;
	float hscale, vscale;
	float rotation;
	Path *clip;
	int sequence;
	int dtk;
	unsigned char reddtk, greendtk, bluedtk;
	unsigned long dtkpix;
	int sortid;
	Grobj *parent;
	Grobj *rootparent;
	int iotag;

public:

	/* constructor */
	Grobj();

	void getloc(float *fx, float *fy, int *ix, int *iy)
						{ *fx= x; *fy= y;
						  *ix= px; *iy= py; }
	void setfloc(float, float, float);
	void setploc(int, int, float);
	int gettype()				{ return type; }
	int settype(int);
	void getscale(float *h, float *v)	{ *h= hscale; *v= vscale; }
	void setscale(float h, float v)		{ hscale= h; vscale= v; }
	float getrotation()			{ return rotation; }
	void setrotation(float);
	Path *getclip()				{ return clip; }
	void setclip(Path *p)			{ clip= p; }
	int getsequence()			{ return sequence; }
	void setsequence(int s)			{ sequence= s; }
	void sortsequence(Grobj **);
	void getdtk(int *d, unsigned char *r, unsigned char *g, unsigned char *b)
						{ *d= dtk; *r= reddtk;
						  *g= greendtk; *b= bluedtk; }
	int setdtk(int, unsigned char, unsigned char, unsigned char);
	unsigned long getdtkpix()		{ return dtkpix; }
	void setdtkpix(unsigned long p)		{ dtkpix= p; }
	int getsortid()				{ return sortid; }
	void setsortid(int s)			{ sortid= s; }
	Grobj *getparent()			{ return parent; }
	void setparent(Grobj *);
	Grobj *getrootparent()			{ return rootparent; }
	void setrootparent(Grobj *);
	int getiotag()				{ return iotag; }
	void setiotag(int t)			{ iotag= t; }

	virtual int icedump(FILE *)= 0;

};

#endif __GROBJ_CLASS__
