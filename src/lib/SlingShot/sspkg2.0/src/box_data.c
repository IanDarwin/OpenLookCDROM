/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)box_data.c 1.3 92/02/18";
#endif
#endif

#include "box_impl.h"

Pkg_private int cbox_init();
Pkg_private Xv_opaque cbox_set_avlist();
Pkg_private Xv_opaque cbox_get_attr();
Pkg_private int cbox_destroy();

Xv_pkg	cbox_pkg = {
	"Box",
	ATTR_BOX,
	sizeof(Box_struct),
	&rectobj_pkg,
	cbox_init,
	cbox_set_avlist,
	cbox_get_attr,
	cbox_destroy,
	NULL
};

Pkg_private int box_init();
Pkg_private Xv_opaque box_set_avlist();
Pkg_private Xv_opaque box_get_attr();
Pkg_private int box_destroy();

Xv_pkg	box_pkg = {
	"CBox",
	ATTR_BOX,
	sizeof(Cbox_struct),
	&rectobj_pkg,
	box_init,
	box_set_avlist,
	box_get_attr,
	box_destroy,
	NULL
};

