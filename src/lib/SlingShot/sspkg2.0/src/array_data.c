/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)array_data.c 1.2 92/03/02";
#endif
#endif

#include <sspkg/array.h>

Pkg_private int array_tile_init();
Pkg_private Xv_opaque array_tile_set_avlist();
Pkg_private Xv_opaque array_tile_get_attr();
Pkg_private int array_tile_destroy();

Xv_pkg          array_tile_pkg = {
	"Array_tile",
	ATTR_ARRAY_TILE,
	sizeof(Array_tile_struct),
	&rectobj_pkg,
	array_tile_init,
	array_tile_set_avlist,
	array_tile_get_attr,
	array_tile_destroy,
	NULL
};
