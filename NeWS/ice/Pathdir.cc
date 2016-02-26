/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include "Pathdir.h"

Pathdir::Pathdir(Dlnk **listhd, char *n)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	dirname= n;

	return;
}

Pathdir::~Pathdir()
{
	delete dirname;
}
