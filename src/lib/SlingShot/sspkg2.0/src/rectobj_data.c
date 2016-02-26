/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)rectobj_data.c 1.3 91/11/17";
#endif
#endif

#include <sspkg/rectobj.h>
#include "rectobj_impl.h"

Pkg_private int rectobj_init();
Pkg_private Xv_opaque rectobj_set_avlist();
Pkg_private Xv_opaque rectobj_get_attr();
Pkg_private int rectobj_destroy();

Xv_pkg          rectobj_pkg = {
	"Rectobj",
	ATTR_RECTOBJ,
	sizeof(Rectobj_struct),
	&xv_generic_pkg,
	rectobj_init,
	rectobj_set_avlist,
	rectobj_get_attr,
	rectobj_destroy,
	NULL
};


Pkg_private int 	bag_init();
Pkg_private Xv_opaque	bag_set_avlist();
Pkg_private Xv_opaque	bag_get_attr();
Pkg_private int 	bag_destroy();

Xv_pkg bag_pkg = {
	"Bag", 
	ATTR_RECTOBJ,
	sizeof(Rectobj_struct), /* has no private data of it's own */
	&rectobj_pkg,
	bag_init,
	bag_set_avlist,
	bag_get_attr,
	bag_destroy,
	NULL
};


#include <sspkg/canshell.h>
#include "canshell_impl.h"

Pkg_private int 	canvas_shell_init();
Pkg_private Xv_opaque	canvas_shell_set_avlist();
Pkg_private Xv_opaque	canvas_shell_get_attr();
Pkg_private int 	canvas_shell_destroy();

Xv_pkg canvas_shell_pkg = {
	"CanvasShell", 
	ATTR_CANVAS_SHELL,
	sizeof(Canvas_shell_struct),
	&xv_canvas_pkg,
	canvas_shell_init,
	canvas_shell_set_avlist,
	canvas_shell_get_attr,
	canvas_shell_destroy,
	NULL
};

