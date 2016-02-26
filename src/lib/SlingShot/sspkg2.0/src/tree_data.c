/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)tree_data.c 1.1 91/08/31";
#endif
#endif

#include <sspkg/tree.h>
#include "tree_impl.h"

Pkg_private int tree_init();
Pkg_private Xv_opaque tree_set_avlist();
Pkg_private Xv_opaque tree_get_attr();
Pkg_private int tree_destroy();

Xv_pkg          tree_pkg = {
	"Tree",
	ATTR_TREE,
	sizeof(Tree_struct),
	&rectobj_pkg,
	tree_init,
	tree_set_avlist,
	tree_get_attr,
	tree_destroy,
	NULL
};
