/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __PATHDIR_CLASS__
#define __PATHDIR_CLASS__

#include <local/Dlnk.h>

#define PATHDIR_SUCCESS		(0)
#define PATHDIR_FAILURE		(1)

class Pathdir : public Dlnk {

	char *dirname;

public:

	/* constructor */
	Pathdir(Dlnk **, char *);

	char *getdirname()		{ return dirname; }

	/* destructor */
	~Pathdir();
};

#endif __PATHDIR_CLASS__
