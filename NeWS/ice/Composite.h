/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __COMPOSITE_CLASS__
#define __COMPOSITE_CLASS__

#include "Grobj.h"

#define COMPOSITE_DUMP		(1)
#define COMPOSITE_NODUMP	(2)

class Composite : public Grobj {

	char *name;
	int nchildren;
	Grobj **children;
	int natoms;
	Grobj **atoms;
	int dump;

public:
	/* constructors */
	Composite(Dlnk **, char *);

	char *getname()				{ return name; }
	void getchildren(int *nc, Grobj ***c)	{ *nc= nchildren; *c= children; }
	int setchildren(int, Grobj **);
	void getatoms(int *na, Grobj ***a)	{ *na= natoms; *a= atoms; }
	int getdump()				{ return dump; }
	int setdump(int);

	int icedump(FILE *);

	/* destructor */
	~Composite();
};

#endif __COMPOSITE_CLASS__
