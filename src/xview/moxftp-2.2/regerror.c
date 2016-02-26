#include "regexp.h"
#include <X11/Intrinsic.h>
#include <stdio.h>

void
regerror(s)
char *s;
{
	char *cp = NULL;

 	cp = XtMalloc(strlen(s)+sizeof("regexp(3): %s  "));
	sprintf(cp, "regexp(3): %s", s);
	Set_Status(cp);
	XtFree(cp);
}
