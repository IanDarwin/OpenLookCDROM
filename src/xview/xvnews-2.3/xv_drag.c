/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
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

#include <stdio.h>
#include <unistd.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/notice.h>
#include <xview/xv_xrect.h>
#include <xview/cursor.h>
#include <xview/svrimage.h>
#include <xview/fullscreen.h>
#include <xview/dragdrop.h>
#include <X11/X.h>
#include <X11/Xatom.h>

#include "xvnews_ui.h"
#include "xvnews.h"

extern struct globals	*Global;

STATIC_FUNCTION( int in_frame, (Frame, Event *));

extern void drag_feedback(frame, ip)
Frame	frame;
xvnews_xvnews_window_objects    *ip;
{
	Fullscreen      view_fullscreen;
        Xv_Cursor	feedback_cursor;
        Event           local_event;
        int             data[5];
        XID             dest_win_xid;
        char            atom_name[32];
	char		tmpfile[80];
	static char	tfile[] = "/tmp/.xvnews.file";
        Atom            drop_atom;

	feedback_cursor = Global->copyso_cursor;

	sprintf(tmpfile, "%s.%d", tfile, getpid());
	view_fullscreen = xv_create(0, FULLSCREEN,
		FULLSCREEN_INPUT_WINDOW, frame,
		WIN_CURSOR, feedback_cursor,
		WIN_CONSUME_EVENTS,
		WIN_MOUSE_BUTTONS,
		LOC_DRAG,
		LOC_MOVEWHILEBUTDOWN,
		0,
		FULLSCREEN_SYNC,        FALSE,
		0);

	while (input_readevent(frame, &local_event)) {
		if (event_id(&local_event) == MS_LEFT) {
			data[1] = event_x(&local_event);
			data[2] = event_y(&local_event);
			break;
		}
	}
	xv_destroy(view_fullscreen);

	if (!(dest_win_xid = win_pointer_under(frame, event_x(&local_event),
		event_y(&local_event)))) {
		xvnews_err(ip, "Drag and Drop failed!\n");
		return;
	}

	if (in_frame(ip->xvnews_window, &local_event)) {
		xvnews_err(ip, "Drag and Drop failed!\n");
		return;
	}

	Global->dragging = 1;

	sprintf(atom_name, "XVNEWS.DROP%d", (int)time(0));
 
	drop_atom = XInternAtom((Display *)xv_get(frame, XV_DISPLAY),
			atom_name,
			FALSE);

	XChangeProperty((Display *)xv_get(frame, XV_DISPLAY),
			xv_get(frame, XV_XID),
			drop_atom, XA_STRING, 8, PropModeReplace,
			(unsigned char *)tmpfile, (int)strlen(tmpfile)+1);

	data[0] = XV_POINTER_WINDOW;
	data[3] = (int) xv_get(frame, XV_XID);
	data[4] = drop_atom;
	xv_send_message(frame, dest_win_xid, "XV_DO_DRAG_LOAD",
		32, (Xv_opaque *) data, 20);

	xvnews_err(ip, "Drag and Drop completed\n");
}

STATIC int in_frame(frame, event)
	Frame	frame;
	Event 	*event;
{
	Rect	rect;
	int	newx, newy;
	Frame	rframe = xv_get(frame, XV_ROOT);

	if (!xv_get(frame, XV_SHOW, NULL))
		return(0);

	frame_get_rect(frame, &rect);
	win_translate_xy(frame, rframe, event_x(event), event_y(event), 
		&newx, &newy);
	if (	(newx > rect.r_left) && 
		(newx < (rect.r_left + rect.r_width)) &&
		(newy > rect.r_top) &&
		(newy < (rect.r_top + rect.r_height)))
		return(1);
	else
		return(0);
}

extern void drag_dnd_drop(frame, ip)
Frame	frame;
xvnews_xvnews_window_objects    *ip;
{
	Drag_drop	dnd;
	char	*data, *get_article();

	Global->dragging = 1;

	dnd =  xv_create(ip->controls2,DRAGDROP,NULL);

	xv_set(dnd, DND_CURSOR, Global->copyso_cursor,
		DND_TYPE, DND_COPY, XV_NULL);

	data = get_article(ip->article_window);

	xv_create(dnd,SELECTION_ITEM,
			SEL_TYPE, (Atom)XA_STRING,
			SEL_DATA,(Xv_opaque)data,
			NULL);
	switch (dnd_send_drop(dnd) ) {
		case XV_OK:
			xvnews_err(ip, "Drag and Drop completed\n");
			break;
		default:
			xvnews_err(ip, "Drag and Drop failed!\n");
	}

	free(data);
}

char *
get_article(txt)
Textsw	*txt;
{
	int             textsize;
        char            *textbuffer;

        xv_set((Textsw)txt, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, 0);
        textsize= (Textsw_index)xv_get((Textsw)txt, TEXTSW_INSERTION_POINT);
        textbuffer = (char *) malloc(textsize + 10);
        if (textbuffer == NULL) {
                printf("Malloc failed!\n");
                return(NULL);
        }
        memset(textbuffer, '\0', textsize + 9);
        xv_get((Textsw)txt, TEXTSW_CONTENTS, 0, textbuffer, textsize);

	return textbuffer;
}
