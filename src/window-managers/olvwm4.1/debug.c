#ifdef IDENT
#ident	"@(#)debug.c	26.11	93/06/28 SMI"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *	Sun design patents pending in the U.S. and foreign countries. See
 *	LEGAL_NOTICE file for terms of the license.
 */


#include <errno.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "i18n.h"
#include "olwm.h"
#include "win.h"
#include "debug.h"


static char *eventNames[] = {
	"<EventZero>",
	"<EventOne>",
	"KeyPress",
	"KeyRelease",
	"ButtonPress",
	"ButtonRelease",
	"MotionNotify",
	"EnterNotify",
	"LeaveNotify",
	"FocusIn",
	"FocusOut",
	"KeymapNotify",
	"Expose",
	"GraphicsExpose",
	"NoExpose",
	"VisibilityNotify",
	"CreateNotify",
	"DestroyNotify",
	"UnmapNotify",
	"MapNotify",
	"MapRequest",
	"ReparentNotify",
	"ConfigureNotify",
	"ConfigureRequest",
	"GravityNotify",
	"ResizeRequest",
	"CirculateNotify",
	"CirculateRequest",
	"PropertyNotify",
	"SelectionClear",
	"SelectionRequest",
	"SelectionNotify",
	"ColormapNotify",
	"ClientMessage",
	"MappingNotify"
};

void
DebugEvent(ep, str)
	XEvent *ep;
	char *str;
{
	(void)fprintf(stderr, "%s:%s - ", str, eventNames[ep->type]);
	(void)fflush(stderr);
}


static char *typeNames[] = {
	"Frame",
	"Icon",
	"Resize",
	"Pushpin",
	"Button",
	"Pane",
	"IconPane",
	"Colormap",
	"Menu",
	"PinMenu",
	"NoFocus",
	"Root",
	"Busy"
};

void
DebugWindow(win)
	WinGeneric *win;
{
	if (win == NULL) {
		(void)fprintf(stderr, "other window - ");
	} else {
		(void)fprintf(stderr, "win %x (self %lu) %s - ",
		        win, win->core.self, typeNames[win->core.kind]);
	}
	
	(void)fflush(stderr);
}
