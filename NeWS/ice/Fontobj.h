/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __FONTOBJ_CLASS__
#define __FONTOBJ_CLASS__

#include <local/Dlnk.h>

#define FONTOBJ_SUCCESS		(0)
#define FONTOBJ_FAILURE		(1)

class Fontobj : public Dlnk {

	char *fontname;

public:

	/* constructor */
	Fontobj();
	Fontobj(Dlnk **, char *);

	char *getfont()			{ return fontname; }
	int setfont(char *);

	/* destructor */
	~Fontobj();


};

#endif __FONTOBJ_CLASS__
