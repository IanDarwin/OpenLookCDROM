/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include "Pathdup.h"

extern "C" {
char *		strcpy(char *, char *);
int		strlen(char *);
}

Pathdup::Pathdup()
{
	oldname= (char *) 0;
	newname= (char *) 0;
}

Pathdup::Pathdup(Dlnk **objhd, char *o, char *n)
{
	if (objhd == (Dlnk **) 0)
		;
	else if (*objhd == (Dlnk *) 0)
		*objhd= (Dlnk *) this;
	else
		link(*objhd, DLNK_TAIL);

	oldname= o;
	newname= n;
}

Pathdup::~Pathdup()
{
	delete oldname;
	delete newname;
}
