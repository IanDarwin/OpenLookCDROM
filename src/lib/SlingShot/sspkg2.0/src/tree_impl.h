/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef TREE_IMPL_DEFINED
#define TREE_IMPL_DEFINED

/* @(#)tree_impl.h 1.5 92/03/02 */

#include <sspkg/rectobj.h>
#include <sspkg/tree.h>

typedef struct tree_info {
	Rectobj 	root_node;
	Tree_layout	layout;
	Listnode	*line_heap_list;
	short		parent_distance;
	short		border;
	unsigned char	manage_self;
} Tree_info;

#define TREE_PRIVATE(tree)    XV_PRIVATE(Tree_info, Tree_struct, tree)

/*
 *
 * This tree layout code is derived from source published in IEEE Software,
 * July 1990, "Drawing Dynamic Trees".
 *
 * Notes on the data structures...
 *
 *  polygon.upper.head             polygon.upper.tail
 *                   |                              |
 *                   V	                            V
 *                   polyline-> polyline->   ....   polyline-> NULL
 *
 *
 *  polygon.lower.head             polygon.lower.tail
 *                   |                              |
 *                   V	                            V
 *                   polyline-> polyline->   ....   polyline-> NULL
 *
 * The object lies in between the lower and upper polygons.  
 * I.e. contour.upper.head starts at the (0, 0) position and
 * contour.lower.head starts at (0, <object height + 2 * BORDER_WIDTH>).
 * The dx and dy fields of each polyline define the contour in a stepwise
 * fashion.
 * The offset field defines the relative position of each child.  The
 * first child is relative to the parent, all other children are relative
 * to the previous sibling.  Perhaps a better term for contour is outline.
 *
 * The top, bottom, and depth fields define the bounding box of the
 * nodes and its children.  Top and bottom are relative to the center
 * of the node, so (bottom - top) is the height.
 */

typedef struct line {
	short dx, dy;
	struct line *link;
} Polyline;

struct polygon {
	struct {
		Polyline *head;
		Polyline *tail;
	} lower, upper;
};

typedef struct tnode {
	struct	{short x,y;} offset;
	struct	polygon contour;
	Rectobj_list *children;
	Rectobj_list siblings;	/* siblings of same "tree" parent */
	Rectobj	from_link;
	Rectobj line_to;
	short	top, bottom;	/* used to calculate breadth */
	short	depth;
	short	set_initial_position;
} Tree_layout_data;

#endif
