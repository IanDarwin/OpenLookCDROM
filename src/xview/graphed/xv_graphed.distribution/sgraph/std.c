/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

#include "std.h"
#include "sgraph.h"
#include "varargs.h"


char	*strsave (s)
char	*s;
{
	char	*saved_s;
	
	if (s != NULL) {
		saved_s = (char *)mymalloc(strlen(s)+1);
		strcpy (saved_s, s);
	} else {
		saved_s = NULL;
	}

	return saved_s;
}


Attributes	make_attr (va_alist)
va_dcl
{
	va_list 	args;
	Attributes	attr;
	Attributes_type	attr_type;
	
	va_start (args);
	
	attr_type = va_arg (args, Attributes_type);
	
	switch (attr_type) {
	    case ATTR_FLAGS :
		attr.flags = va_arg (args, int);
		break;
	    case ATTR_DATA :
		attr.data = va_arg (args, char *);
		break;
	}
	
	va_end (args);
	return attr;
}
