/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <X11/Xlib.h>

#include "Intobj.h"

extern "C" {
char *		strcpy(char *, char *);
int		strlen(char *);
}

Intobj::Intobj()
{
	name= (char *) 0;
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);

	(void) settype(GROBJ_INTOBJ);
	setsequence(0);
}

Intobj::Intobj(Dlnk **listhd, char *n, int t, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		((Dlnk *) this)->link(*listhd, DLNK_TAIL);

	if (n == (char *) 0)
		name= (char *) 0;
	else if (strlen(n) == 0)
		name= (char *) 0;
	else {
		if ((name= new char[strlen(n)+1]) != (char *) 0)
			(void) strcpy(name, n);
	}

	switch (t) {
	case INTOBJ_TEXT:
	case INTOBJ_VECTOR:
	case INTOBJ_CURVE:
	case INTOBJ_MARKER:
	case INTOBJ_RECTANGLE:
	case INTOBJ_POLYGON:
	case INTOBJ_AXIS:
		intobjtype= t;
		break;
	default:
		intobjtype= INTOBJ_UNDEFINED;
	}

	(void) settype(GROBJ_INTOBJ);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Intobj::setintobjtype(int t)
{

	switch (t) {
	case INTOBJ_UNDEFINED:
	case INTOBJ_TEXT:
	case INTOBJ_VECTOR:
	case INTOBJ_CURVE:
	case INTOBJ_MARKER:
	case INTOBJ_RECTANGLE:
	case INTOBJ_POLYGON:
	case INTOBJ_AXIS:
		intobjtype= t;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Intobj::setname(char *n)
{
	delete name;
	if (n == (char *) 0)
		name= (char *) 0;
	else if (strlen(n) == 0)
		name= (char *) 0;
	else {
		if ((name= new char[strlen(n)+1]) == (char *) 0)
			return GROBJ_FAILURE;
		(void) strcpy(name, n);
	}
	return GROBJ_SUCCESS;
}

Intobj::~Intobj()
{
	delete name;
}
