/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef BOX_H_DEFINED
#define BOX_H_DEFINED

/* @(#) box.h 1.7 92/10/30  */

#include <xview/generic.h>

/*
 * types 
 */
typedef Xv_opaque Cbox;
typedef Xv_opaque Box;


/*
 * packages
 */
extern Xv_pkg   	cbox_pkg;
#define CBOX		&cbox_pkg

extern Xv_pkg   	box_pkg;
#define BOX		&box_pkg

/*
 * attributes
 */
#define ATTR_BOX	ATTR_PKG_UNUSED_LAST-12
#define BOX_ATTR(type, ordinal)		ATTR(ATTR_BOX, type, ordinal)

typedef enum {

	BOX_GAP			= BOX_ATTR(ATTR_SHORT,		1),
	BOX_LAYOUT		= BOX_ATTR(ATTR_ENUM,		2),

} Box_attr;

typedef enum {
	BOX_LAYOUT_VERTICAL,
	BOX_LAYOUT_HORIZONTAL,
} Box_layout;

#endif

