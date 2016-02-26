/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)drawobj_data.c 1.6 92/07/08";
#endif
#endif

#include <xview/font.h>
#include <sspkg/drawobj.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include "dtext_impl.h"
#include "dimage_impl.h"


Pkg_private int drawrect_init();
Pkg_private Xv_opaque drawrect_set_avlist();
Pkg_private Xv_opaque drawrect_get_attr();
Pkg_private int drawrect_destroy();

Xv_pkg          drawrect_pkg = {
	"Drawrect",
	ATTR_DRAWOBJ,
	sizeof(Drawrect_struct),
	&rectobj_pkg,
	drawrect_init,
	drawrect_set_avlist,
	drawrect_get_attr,
	drawrect_destroy,
	NULL
};


Pkg_private int drawline_init();
Pkg_private Xv_opaque drawline_set_avlist();
Pkg_private Xv_opaque drawline_get_attr();
Pkg_private int drawline_destroy();

Xv_pkg          drawline_pkg = {
	"Drawline",
	ATTR_DRAWOBJ,
	sizeof(Drawline_struct),
	&rectobj_pkg,
	drawline_init,
	drawline_set_avlist,
	drawline_get_attr,
	drawline_destroy,
	NULL
};


Pkg_private int drawimage_init();
Pkg_private Xv_opaque drawimage_set_avlist();
Pkg_private Xv_opaque drawimage_get_attr();
Pkg_private int drawimage_destroy();

Xv_pkg          drawimage_pkg = {
	"Drawimage",
	ATTR_DRAWOBJ,
	sizeof(Drawimage_struct),
	&rectobj_pkg,
	drawimage_init,
	drawimage_set_avlist,
	drawimage_get_attr,
	drawimage_destroy,
	NULL
};
/* Shared with drawicon, in here for shared lib allocation. */
Drawimage_info  *drawicon_private_diinfo; 


Pkg_private int drawtext_init();
Pkg_private Xv_opaque drawtext_set_avlist();
Pkg_private Xv_opaque drawtext_get_attr();
Pkg_private int drawtext_destroy();

Xv_pkg          drawtext_pkg = {
	"Drawtext",
	ATTR_DRAWOBJ,
	sizeof(Drawtext_struct),
	&rectobj_pkg,
	drawtext_init,
	drawtext_set_avlist,
	drawtext_get_attr,
	drawtext_destroy,
	NULL
};
/* Shared with drawicon, in here for shared lib allocation. */
Drawtext_info   *drawicon_private_dtinfo;


Pkg_private int drawicon_init();
Pkg_private Xv_opaque drawicon_set_avlist();
Pkg_private Xv_opaque drawicon_get_attr();
Pkg_private int drawicon_destroy();

Xv_pkg          drawicon_pkg = {
	"Drawicon",
	ATTR_DRAWOBJ,
	sizeof(Drawicon_struct),
	&rectobj_pkg,
	drawicon_init,
	drawicon_set_avlist,
	drawicon_get_attr,
	drawicon_destroy,
	NULL
};


Pkg_private int drawarea_init();
Pkg_private Xv_opaque drawarea_set_avlist();
Pkg_private Xv_opaque drawarea_get_attr();
Pkg_private int drawarea_destroy();

Xv_pkg          drawarea_pkg = {
	"Drawarea",
	ATTR_DRAWOBJ,
	sizeof(Drawarea_struct),
	&rectobj_pkg,
	drawarea_init,
	drawarea_set_avlist,
	drawarea_get_attr,
	drawarea_destroy,
	NULL
};


Pkg_private int 	tacho_init();
Pkg_private Xv_opaque	tacho_set_avlist();
Pkg_private Xv_opaque	tacho_get_attr();
Pkg_private int 	tacho_destroy();

Xv_pkg tacho_pkg = {
	"Tacho", 
	ATTR_DRAWOBJ,
	sizeof(Tacho_struct),
	&rectobj_pkg,
	tacho_init,
	tacho_set_avlist,
	tacho_get_attr,
	tacho_destroy,
	NULL
};


Pkg_private int 	clockobj_init();
Pkg_private Xv_opaque	clockobj_set_avlist();
Pkg_private Xv_opaque	clockobj_get_attr();
Pkg_private int 	clockobj_destroy();

Xv_pkg clockobj_pkg = {
	"Clock",
	ATTR_DRAWOBJ,
	sizeof(Clockobj_struct),
	&drawarea_pkg,
	clockobj_init,
	clockobj_set_avlist,
	clockobj_get_attr,
	clockobj_destroy,
	NULL
};


Pkg_private int		grip_init();
Pkg_private Xv_opaque	grip_set_avlist();
Pkg_private Xv_opaque	grip_get_attr();
Pkg_private int		grip_destroy();

Xv_pkg grip_pkg = {
	"Grip",
	ATTR_DRAWOBJ,
	sizeof(Grip_struct),
	&drawimage_pkg,
	grip_init,
	grip_set_avlist,
	grip_get_attr,
	grip_destroy,
	NULL
};


Pkg_private int		grip_temp_init();

Xv_pkg grip_temp_pkg = {
	"Temp_grip",
	ATTR_DRAWOBJ,
	sizeof(Grip_struct),
	&drawimage_pkg,
	grip_temp_init,
	grip_set_avlist,
	grip_get_attr,
	grip_destroy,
	NULL
};

