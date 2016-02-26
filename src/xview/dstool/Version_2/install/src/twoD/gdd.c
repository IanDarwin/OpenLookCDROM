/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef lint
static char	sccsid[] = "@(#)gdd.c	1.9 90/05/27 Copyright 1989 Sun Microsystems";
#endif

#include <portability.h>

/*
 * Drag-n-drop interface functions.
 */

#include <sys/param.h>
#include <sys/types.h>

#ifndef HAS_NO_TIMEB_H
#include <sys/timeb.h>
#endif

#include <xview/xview.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <gdd.h>

#define	OK	0
#define	ERROR	-1

/*
 * Data that are passed from the sender to the receiver.
 */
typedef struct {
	XID             xid;
	int             x;
	int             y;
	Window          window;
	Atom            property;
}               Drag_message;

/*
 * Get the name of the file being dragged and dropped.  Sets the file name
 * and returns 0 if successful, otherwise returns -1.
 */
int
gdd_get_drag_name(window, name)
	Xv_window       window;			/* event window */
	char           *name;			/* MAXPATHLEN length buffer */
{
	Drag_message   *msg = (Drag_message *) xv_get(window, WIN_MESSAGE_DATA);
	Display        *display = (Display *) XV_DISPLAY_FROM_WINDOW(window);
	long            long_offset = 0L;
	long            long_length = (long) (MAXPATHLEN / 4);
	int             delete_flag = TRUE;
	Atom            actual_type;
	int             actual_format;
	u_long          nitems;
	u_long          bytes_after;
	char           *prop; 
/*	unsigned char  *prop; *//* changed 9/13 due to paw report from ANSI C compiler */

	if (msg == NULL || msg->property == None)
		return ERROR;

	if (XGetWindowProperty(display, msg->window, msg->property,
			       long_offset, long_length, delete_flag,
			       AnyPropertyType, &actual_type, &actual_format,
			       &nitems, &bytes_after, &prop) != Success)
		return ERROR;

	strcpy(name, prop);
	XFree(prop);
	return OK;
}

/*
 * Send the name of a file being dragged and dropped.  Returns 0 if successful,
 * otherwise -1.
 */
int
gdd_set_drag_name(source_win, dest_win, x, y, name)
	Xv_window	source_win;		/* source window */
	Xv_window	dest_win;		/* destination window */
	int		x;			/* drop x */
	int		y;			/* drop y */
	char	       *name;			/* file name */
	/* unsigned char	 *name;	*/		/* file name *//* changed 9/13 due to paw report from ANSI C compiler */
{
	Drag_message	msg;
	Display        *display = (Display *) XV_DISPLAY_FROM_WINDOW(source_win);
	Window		source_xid = (Window) xv_get(source_win, XV_XID);
	Window		dest_xid = (Window) xv_get(dest_win, XV_XID);
	Atom		drag_atom = XInternAtom(display, "DRAG_DROP", FALSE);

	if (name == NULL)
		return ERROR;
	
	XChangeProperty(display, source_xid, drag_atom, XA_STRING, 8,
			PropModeReplace, name, strlen(name) + 1);

	msg.xid	     = XV_POINTER_WINDOW;
	msg.x	     = x;
	msg.y	     = y;
	msg.window   = source_xid;
	msg.property = drag_atom;

	xv_send_message(source_win, dest_xid, "XV_DO_DRAG_LOAD", 32, &msg, /* changed 9/13 due to paw report from ANSI C compiler */
/*	xv_send_message(source_win, dest_xid, "XV_DO_DRAG_LOAD", 32, (Xv_opaque) &msg,*/
			sizeof (msg));
	return OK;
}
