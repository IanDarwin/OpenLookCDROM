/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <X11/Xlib.h>

#include "Composite.h"

extern "C" {
char *		strcpy(char *, char *);
int		strlen(char *);
}

Composite::Composite(Dlnk **listhd, char *n)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	if (n == (char *) 0)
		name= (char *) 0;
	else if (strlen(n) == 0)
		name= (char *) 0;
	else {
		if ((name= new char[strlen(n)+1]) != (char *) 0)
			(void) strcpy(name, n);
	}

	nchildren= 0;
	natoms= 0;
	children= atoms= (Grobj **) 0;
	(void) settype(GROBJ_COMPOSITE);
}

int
Composite::setchildren(int nc, Grobj **c)
{
	int i, na, nca;
	Grobj **a, **ca;

	if (nc < 0)
		return GROBJ_FAILURE;
	else if (nc == 0) {
		nchildren= 0;
		if (children != (Grobj **) 0)
			delete children;
		children= (Grobj **) 0;
		return GROBJ_SUCCESS;
	}

	/* non-empty child list */
	if (c == (Grobj **) 0)
		return GROBJ_FAILURE;

	/* determine number of child atoms */
	for (i= na= 0; i < nc; i++) {
		if (c[i] == (Grobj *) 0)
			return GROBJ_FAILURE;
		if (c[i]->gettype() == GROBJ_COMPOSITE) {
			((Composite *) c[i])->getatoms(&nca, &ca);
			na+= nca;
		}
		else
			na++;
	}

	/* create pointers to all child atoms */
	if ((a= new Grobj *[na]) == (Grobj **) 0)
		return GROBJ_FAILURE;
	for (i= na= 0; i < nc; i++) {
		if (c[i]->gettype() == GROBJ_COMPOSITE) {
			int j;

			((Composite *) c[i])->getatoms(&nca, &ca);
			for (j= 0; j < nca; j++)
				a[na++]= ca[j];
		}
		else
			a[na++]= c[i];
	}

	delete children;
	delete atoms;

	nchildren= nc;
	children= c;
	natoms= na;
	atoms= a;

	return GROBJ_SUCCESS;
}

int
Composite::setdump(int d)
{
	switch (d) {
	case COMPOSITE_DUMP:
	case COMPOSITE_NODUMP:
		dump= d;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Composite::icedump(FILE *fp)
{
	int i;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Cmp: Begin %1d %1d %s\n", getiotag(), nchildren, name);

	for (i= 0; i < nchildren; i++) {
		if (children[i]->gettype() == GROBJ_COMPOSITE)
			fprintf(fp, "%%%%ICE-Cmp: CC %1d\n", children[i]->getiotag());
		else
			fprintf(fp, "%%%%ICE-Cmp: AC %1d\n", children[i]->getiotag());
	}

	fprintf(fp, "%%%%ICE-Cmp: End\n");
	return GROBJ_SUCCESS;
}

Composite::~Composite()
{
	delete name;
	delete children;
	delete atoms;
}
