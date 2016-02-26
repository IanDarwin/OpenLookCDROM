/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 * 	@(#) drop_ex.c 1.5 92/05/06 
 */
#include <string.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/sel_pkg.h>
#include <xview/dragdrop.h>
#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/array.h>
#include <sspkg/box.h>

Frame		frame;
Canvas_shell	shell;
Box		box;
Selection_requestor sel_req;
Drawtext	transfered_text;

void	my_drop_proc();
void	do_drop();


main(argc, argv)
	int             argc;
	char           *argv[];
{
	Bag		bag;
	Array_tile	atile;
	int		i;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			FRAME_SHOW_FOOTER,	True,
			NULL);

	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL,
			CANVAS_SHELL_AUTO_DROP_SITE, TRUE,
			NULL);
	
	box = (Box) xv_create(shell, BOX, 
			BOX_LAYOUT, BOX_LAYOUT_VERTICAL,
			RECTOBJ_BORDER, 10,
			NULL);

	atile = (Array_tile) xv_create(box, ARRAY_TILE,
			ARRAY_TILE_N_COLUMNS, 4,
			ARRAY_TILE_COLUMN_GAP, 40,
			ARRAY_TILE_ALIGN, ARRAY_TILE_ALIGN_WEST,
			ARRAY_TILE_VLINES, TRUE,
			ARRAY_TILE_HLINES, TRUE,
			NULL);

	(void) xv_create(atile, DRAWTEXT,
		DRAWTEXT_STRING, "Drop Site 1",
		RECTOBJ_DROP_PROC, my_drop_proc,
		RECTOBJ_ACCEPTS_DROP, TRUE,
		RECTOBJ_SELECTABLE, FALSE,
		NULL);

	(void) xv_create(atile, DRAWTEXT,
		DRAWTEXT_STRING, "Drop Site 2",
		RECTOBJ_DROP_PROC, my_drop_proc,
		RECTOBJ_ACCEPTS_DROP, TRUE,
		RECTOBJ_SELECTABLE, FALSE,
		NULL);

	(void) xv_create(atile, DRAWTEXT,
		DRAWTEXT_STRING, "Drop Site 3",
		RECTOBJ_DROP_PROC, my_drop_proc,
		RECTOBJ_ACCEPTS_DROP, TRUE,
		RECTOBJ_SELECTABLE, FALSE,
		NULL);

	(void) xv_create(atile, DRAWTEXT,
		DRAWTEXT_STRING, "Drop Site 4",
		RECTOBJ_DROP_PROC, my_drop_proc,
		RECTOBJ_ACCEPTS_DROP, TRUE,
		RECTOBJ_SELECTABLE, FALSE,
		NULL);

	transfered_text = (Drawtext) xv_create(box, DRAWTEXT,
		RECTOBJ_SELECTABLE, FALSE,
		DRAWTEXT_STRING, " ", 
		NULL);

	/* size the canvas to the size of enclosed objects */
	xv_set(shell, 
		XV_WIDTH, xv_get(box, XV_WIDTH),
		XV_HEIGHT, xv_get(box, XV_HEIGHT),
		NULL);

	sel_req = xv_create(shell, SELECTION_REQUESTOR, NULL);

	window_fit(frame);

	xv_main_loop(frame);
	exit(0);
}


void
my_drop_proc(paint_window, event, canvas_shell, drawtext)
	Xv_window       paint_window;
	Event           *event;
	Canvas_shell    canvas_shell;
	Drawtext	drawtext;
{
	char	str[80];

	switch(event_action(event)) {
		case ACTION_DRAG_PREVIEW:
			if(event_id(event) == LOC_WINENTER) {
				strcpy(str, "Entered ");
				xv_set(drawtext, 
					RECTOBJ_PREDROP_HIGHLIGHT, 
					NULL);
			} else
			if(event_id(event) == LOC_WINEXIT) {
				strcpy(str, "Exited ");
				xv_set(drawtext, 
					RECTOBJ_PREDROP_NORMAL,
					NULL);
			} else
			if(event_id(event) == LOC_DRAG) {
				sprintf(str, "Drag over (%d, %d) ",
					event_x(event), event_y(event));
			}
		break;

		case ACTION_DRAG_COPY:
			do_drop(event, drawtext);
			strcpy(str, "Action drag copy: ");
			xv_set(drawtext, 
				RECTOBJ_PREDROP_NORMAL,
				NULL);
			break;

		case ACTION_DRAG_MOVE:
			do_drop(event, drawtext);
			strcpy(str, "Action drag move: ");
			xv_set(drawtext, 
				RECTOBJ_PREDROP_NORMAL,
				NULL);
			break;

		case ACTION_DRAG_LOAD:
			do_drop(event, drawtext);
			strcpy(str, "Action drag load: ");
			xv_set(drawtext, 
				RECTOBJ_PREDROP_NORMAL,
				NULL);
			break;
	}
	strcat(str, (char*) xv_get(drawtext, DRAWTEXT_STRING));
	xv_set(frame, 
		FRAME_LEFT_FOOTER, str, 
		NULL);
}


void
do_drop(event, object)
	Event *event;
{
	int length, format;
	char *string;
	char str[60];

	if (dnd_decode_drop(sel_req, event) != XV_ERROR) {
		xv_set(sel_req, 
			SEL_TYPE, XA_STRING, 
			NULL);

		string = (char *)xv_get(sel_req, SEL_DATA, &length, &format);

		if (length != SEL_ERROR) {
			/* assert(string) */
			if(strlen(string) > (size_t)40) {
				strncpy(str, string, 40);
				strcat(str, "...");
			} else
				strcpy(str, string);

			xv_set(transfered_text,
				DRAWTEXT_STRING, str,
				NULL);
				
			free (string);
		}

		if (event_action(event) == ACTION_DRAG_MOVE) {
			int             length, format;

			xv_set(sel_req, SEL_TYPE_NAME, "DELETE", 0);
			(void) xv_get(sel_req, SEL_DATA, &length, &format);
		}

		dnd_done(sel_req);
	}
}


