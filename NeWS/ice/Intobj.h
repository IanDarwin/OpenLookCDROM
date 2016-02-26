/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __INTOBJ_CLASS__
#define __INTOBJ_CLASS__

#include "Grobj.h"

#define INTOBJ_UNDEFINED		(0)
#define INTOBJ_TEXT			(1)
#define INTOBJ_VECTOR			(2)
#define INTOBJ_CURVE			(3)
#define INTOBJ_MARKER			(4)
#define INTOBJ_RECTANGLE		(5)
#define INTOBJ_POLYGON			(6)
#define INTOBJ_AXIS			(7)

class Intobj : public Grobj {

	int intobjtype;
	char *name;

public:
	/* constructors */
	Intobj();
	Intobj(Dlnk **, char *, int, int);

	int getintobjtype()				{ return intobjtype; }
	int setintobjtype(int);
	char *getname()					{ return name; }
	int setname(char *);

	virtual int draw(FILE *)= 0;
	virtual int icedump(FILE *)= 0;

	/* destructor */
	~Intobj();
};

#endif __INTOBJ_CLASS__
