#ifndef lint
static char	sccsid[] = "@(#)group_data.c	2.4 91/08/14 Copyright 1991 Sun Microsystems";
#endif

#include	"group_impl.h"

Xv_pkg	group_pkg = {
	"Group",			/* Package name */
	ATTR_PKG_UNUSED_LAST - 2,	/* Package ID */
	sizeof(Group_public),		/* Size of public struct */
	XV_GENERIC_OBJECT,		/* Subclass of XV_GENERIC_OBJECT */
	group_init,
	group_set,
	group_get,
	group_destroy,
	NULL				/* No xv_find() */
};

