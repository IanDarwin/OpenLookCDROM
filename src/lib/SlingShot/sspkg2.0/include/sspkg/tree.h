/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef TREE_DEFINED
#define TREE_DEFINED

/* @(#) tree.h 1.3 92/10/30  */

#include <sspkg/rectobj.h>

typedef Xv_opaque	Tree;

extern Xv_pkg		tree_pkg;
#define TREE		&tree_pkg

typedef struct {
	Rectobj_struct	parent_data;
	Xv_opaque	private_data;
}		Tree_struct;


#define ATTR_TREE 	ATTR_PKG_UNUSED_LAST-16
#define TREE_ATTR(type, ordinal)	ATTR(ATTR_TREE, type, ordinal)

typedef enum {
	TREE_ADD_LINK		= TREE_ATTR(ATTR_OPAQUE_PAIR, 1),
	TREE_UNLINK		= TREE_ATTR(ATTR_OPAQUE, 2),
	TREE_LINK_FROM		= TREE_ATTR(ATTR_OPAQUE, 3),
	TREE_LINK_TO_LIST	= TREE_ATTR(ATTR_OPAQUE, 4),
	TREE_PARENT_DISTANCE	= TREE_ATTR(ATTR_INT, 5),
	TREE_LAYOUT		= TREE_ATTR(ATTR_ENUM, 6),
	TREE_BORDER		= TREE_ATTR(ATTR_INT, 7),
	TREE_DRAWLINE		= TREE_ATTR(ATTR_OPAQUE, 8),
} Tree_attr;

typedef enum {
	TREE_LAYOUT_HORIZONTAL,
	TREE_LAYOUT_VERTICAL,
} Tree_layout;

#endif
