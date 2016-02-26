/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __PSDOC_CLASS__
#define __PSDOC_CLASS__

#include "Grobj.h"

#define PSDOC_MAXLINELEN		(8192)

class Psdoc : public Grobj {

	char *name;

public:
	/* constructors */
	Psdoc();
	Psdoc(Dlnk **, char *, int);

	char *getname()					{ return name; }

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Psdoc();
};

#endif __PSDOC_CLASS__
