/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef CANVAS_SHELL_DEFINED
#define CANVAS_SHELL_DEFINED

/* @(#) canshell.h 1.15 92/10/30  */

#include <sspkg/rectobj.h>

extern Xv_pkg		canvas_shell_pkg;
#define CANVAS_SHELL	&canvas_shell_pkg

typedef Rectobj_struct Canvas_shell_struct;

#define ATTR_CANVAS_SHELL	ATTR_PKG_UNUSED_LAST-13

#define CANVAS_SHELL_ATTR(type, ordinal)	\
				ATTR(ATTR_CANVAS_SHELL, type, ordinal)

typedef enum {

	CANVAS_SHELL_FONT	     = CANVAS_SHELL_ATTR(ATTR_OPAQUE, 1),
	CANVAS_SHELL_DELAY_REPAINT   = CANVAS_SHELL_ATTR(ATTR_BOOLEAN, 2),
	CANVAS_SHELL_BATCH_REPAINT   = CANVAS_SHELL_ATTR(ATTR_BOOLEAN, 3),
	CANVAS_SHELL_EVENT_PROC	     = CANVAS_SHELL_ATTR(ATTR_FUNCTION_PTR, 4),
	CANVAS_SHELL_AUTO_DROP_SITE  = CANVAS_SHELL_ATTR(ATTR_BOOLEAN, 5),
	CANVAS_SHELL_DROP_SITE_KEY   = CANVAS_SHELL_ATTR(ATTR_OPAQUE, 6),

} Canvas_shell_attr;

#define CANVAS_SHELL_DROP_SITE       XV_KEY_DATA, CANVAS_SHELL_DROP_SITE_KEY

EXTERN_FUNCTION(void canvas_shell_resize_proc, (Canvas_shell, int, int) );
EXTERN_FUNCTION(void canvas_shell_split_proc, (Xv_window, Xv_window, int) );

#endif
