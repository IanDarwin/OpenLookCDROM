/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include "Fontobj.h"

extern "C" {
char *		strcpy(char *, char *);
int		strlen(char *);
}

Fontobj::Fontobj()
{
	fontname= (char *) 0;
}

Fontobj::Fontobj(Dlnk **objhd, char *f)
{
	if (objhd == (Dlnk **) 0)
		;
	else if (*objhd == (Dlnk *) 0)
		*objhd= (Dlnk *) this;
	else
		link(*objhd, DLNK_TAIL);


	if (f == (char *) 0)
		fontname= (char *) 0;
	else if (strlen(f) == 0)
		fontname= (char *) 0;
	else {
		if ((fontname= new char[strlen(f)+1]) != (char *) 0)
			(void) strcpy(fontname, f);
	}
}

Fontobj::~Fontobj()
{
	delete fontname;
}
