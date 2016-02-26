#ifndef lint
static char    *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/apex_help.c,v 1.1 93/01/06 03:27:14 gounares Exp Locker: gounares $";

#endif

/*
 * $Log:	apex_help.c,v $
 * Revision 1.1  93/01/06  03:27:14  gounares
 * Initial revision
 * 
 */

/*
 * apex_help.c
 * 
 * routines for the online help sub-system of apeX
 * 
 * Alex Gounares 1993
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include "qmark_bmp"
#include "apex_help.h"

void            misc_help_proc();
extern int      CLIENT_DATA_KEY;

void
set_help_button(frame, panel, help_string)
    Frame           frame;
    Panel           panel;
    char          **help_string;
{

	static Server_image qmark_image;

	if (!qmark_image) {
		qmark_image = (Server_image) xv_create(NULL, SERVER_IMAGE,
			XV_HEIGHT, qmark_bmp_height,
			XV_WIDTH, qmark_bmp_width,
			SERVER_IMAGE_X_BITS, qmark_bmp_bits,
			SERVER_IMAGE_DEPTH, 1,
			NULL);
	}
	xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE, qmark_image,
		PANEL_CLIENT_DATA, help_string,
		PANEL_NOTIFY_PROC, misc_help_proc,
		XV_X, (int) xv_get(panel, XV_WIDTH) - (qmark_bmp_width + 10),
		XV_Y, 3,
		XV_KEY_DATA, CLIENT_DATA_KEY, frame,
		NULL);
}

void
misc_help_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	char          **data;
	Panel           help_panel;
	Frame           frame,
	                help_frame;
	char           *szTitle[256];

	data = (char **) xv_get(item, PANEL_CLIENT_DATA);

	/* don't let the help button cause the popup to disappear! :) */

	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);

	frame = (Frame) xv_get(item, XV_KEY_DATA, CLIENT_DATA_KEY);

	sprintf(szTitle, "apeX Instant-Help for %s", *data++);

	help_frame = (Frame) xv_create(frame, FRAME_CMD,
		FRAME_LABEL, szTitle,
		NULL);

	help_panel = xv_get(help_frame, FRAME_CMD_PANEL, NULL);

	xv_set(help_panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_ITEM_Y_GAP, 2,
		NULL);

	for (; data != NULL && *data != NULL; data += 2) {
		xv_create(help_panel, PANEL_MESSAGE,
			PANEL_LABEL_STRING, *(data + 1),
			PANEL_LABEL_BOLD, (**data == 'B') ? TRUE : FALSE,
			NULL);
	}

	window_fit(help_panel);
	window_fit(help_frame);

	xv_set(help_frame, XV_SHOW, TRUE, NULL);
}
