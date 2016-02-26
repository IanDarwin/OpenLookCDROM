/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __PATHDUP_CLASS__
#define __PATHDUP_CLASS__

#include <local/Dlnk.h>

#define PATHDUP_SUCCESS		(0)
#define PATHDUP_FAILURE		(1)

class Pathdup : public Dlnk {

	char *oldname;
	char *newname;

public:

	/* constructor */
	Pathdup();
	Pathdup(Dlnk **, char *, char *);

	void getnames(char **o, char **n)	{ *o= oldname; *n= newname; }

	/* destructor */
	~Pathdup();


};

#endif __PATHDUP_CLASS__
