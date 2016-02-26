/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef GRIP_DEFINED
#define GRIP_DEFINED

/* @(#) grip.h 1.3 92/10/30  */

#include <sspkg/rectobj.h>

/*
 * types 
 */
typedef Xv_opaque Grip;
typedef Xv_opaque Temp_grip;


/*
 * packages
 */
extern Xv_pkg		grip_pkg;
#define GRIP		&grip_pkg

extern Xv_pkg		grip_temp_pkg;
#define TEMP_GRIP	&grip_temp_pkg


/*
 * attributes
 */
#define ATTR_GRIP      ATTR_PKG_UNUSED_LAST-15
#define GRIP_ATTR(type, ordinal)       ATTR(ATTR_GRIP, type, ordinal)

typedef enum {

	GRIP_SLIDE_X		= GRIP_ATTR(ATTR_BOOLEAN,	60),
	GRIP_SLIDE_Y		= GRIP_ATTR(ATTR_BOOLEAN,	61),
	GRIP_EXCEED_PARENT_DIMS	= GRIP_ATTR(ATTR_BOOLEAN,	62),
	GRIP_MOVE_PROC		= GRIP_ATTR(ATTR_FUNCTION_PTR,	63),
	GRIP_DONE_PROC		= GRIP_ATTR(ATTR_FUNCTION_PTR,	64),
	GRIP_MAX_X		= GRIP_ATTR(ATTR_SHORT,		65),
	GRIP_MAX_Y		= GRIP_ATTR(ATTR_SHORT,		66),
	GRIP_MIN_X		= GRIP_ATTR(ATTR_SHORT,		67),
	GRIP_MIN_Y		= GRIP_ATTR(ATTR_SHORT,		68),
	GRIP_RUBBER_STYLE	= GRIP_ATTR(ATTR_ENUM,		69),
	GRIP_BTN_DOWN_X		= GRIP_ATTR(ATTR_INT,		70),
	GRIP_BTN_DOWN_Y		= GRIP_ATTR(ATTR_INT,		71),
	GRIP_IMMEDIATE		= GRIP_ATTR(ATTR_BOOLEAN,	72),

} Grip_attr;


EXTERN_FUNCTION(int default_grip_move_proc, (Xv_window, Event*, Canvas_shell, Grip, short*, short*));

typedef enum {
	GRIP_RUBBER_NONE,
	GRIP_RUBBER_RECT,
	GRIP_RUBBER_VLINE,
	GRIP_RUBBER_VLINE_PAIR,
	GRIP_RUBBER_HLINE,
	GRIP_RUBBER_HLINE_PAIR,
	GRIP_RUBBER_CROSSHAIRS,
} Grip_rubber_style;

#endif

