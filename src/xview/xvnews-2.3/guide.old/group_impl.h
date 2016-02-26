/*
 * @(#)group_impl.h	2.9 91/08/14 Copyright 1991 Sun Microsystems
 */

#ifndef guide_group_impl_DEFINED

#include	"group.h"

typedef struct {
	Xv_object		public_self;
	GROUP_TYPES		group_type;
	Xv_opaque		*members;
	int			cols;
	GROUP_COLUMN_ALIGNMENTS	col_alignment;
	int			rows;
	GROUP_ROW_ALIGNMENTS	row_alignment;
	int			hspacing;
	int			vspacing;
	Xv_opaque		anchor_obj;	 /* Object anchored to */
	GROUP_COMPASS_POINTS	anchor_point;	 /* Point on anchor obj */
	GROUP_COMPASS_POINTS	reference_point; /* Point on group */
	int			hoffset;
	int			voffset;
	Rect			group_rect;
	Rect			value_rect;
	int			initial_x;
	int			initial_y;
	unsigned int		flags;
} Group_private;

Pkg_private int		group_init();
Pkg_private Xv_opaque	group_set();
Pkg_private Xv_opaque	group_get();
Pkg_private int		group_destroy();

#define	GROUP_PUBLIC(item)	XV_PUBLIC(item)
#define	GROUP_PRIVATE(item)	XV_PRIVATE(Group_private, Group_public, item)

typedef enum {
        CREATED		= (1L << 0),
        LAYOUT		= (1L << 1),
        INACTIVE	= (1L << 2),
        SHOWING		= (1L << 3),
        ROWFIRST	= (1L << 4),
        COLFIRST	= (1L << 5)
} GROUP_FLAGS;

#endif guide_group_impl_DEFINED
