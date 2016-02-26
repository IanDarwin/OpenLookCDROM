/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 * 	@(#) select.c 1.3 92/07/14 
 *	
 *	Some code adapted from xview/contrib/examples/selection/sel_hold.c
 */
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/sel_pkg.h>
#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/array.h>

Frame		frame;
Canvas_shell	shell;
Xv_Server	server;

Selection_owner sel;
Selection_item  sel_targets;

Drawtext clicked_object;

void	single_click();
void	selected_proc();

int	selection_convert_proc();
void	selection_done_proc();


#define ATOM(name)      (Atom)xv_get(server, SERVER_ATOM, name)

main(argc, argv)
	int             argc;
	char           *argv[];
{
	Array_tile	atile;
	int		i;
	Atom		targets[5];

	server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);


	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			FRAME_SHOW_FOOTER,	True,
			NULL);

	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL,
			NULL);
	
	atile = (Array_tile) xv_create(shell, ARRAY_TILE,
			ARRAY_TILE_N_COLUMNS, 2,
			ARRAY_TILE_ALIGN, ARRAY_TILE_ALIGN_WEST,
			NULL);

	/* 
	 * Create some objects inside the array_tile.
	 * They will be automatically positioned by the
	 * array_tile.
	 */

	for(i=0;i<20;i++) {
		char str[40];
		sprintf(str, "Text%d", i);
		(void) xv_create(atile, DRAWTEXT,
			DRAWTEXT_STRING, str,
			RECTOBJ_SINGLE_CLICK_PROC, single_click,
			RECTOBJ_SELECTION_PROC, selected_proc,
			NULL);
	}

	/* size the canvas to the size of the array_tile */
	xv_set(shell, 
		XV_WIDTH, xv_get(atile, XV_WIDTH),
		XV_HEIGHT, xv_get(atile, XV_HEIGHT),
		NULL);

	window_fit(frame);

	sel = xv_create(shell, SELECTION_OWNER,
		SEL_CONVERT_PROC,	selection_convert_proc,
		SEL_DONE_PROC,		selection_done_proc,
		NULL);

	targets[0] = ATOM("TARGETS");
	targets[1] = ATOM("TIMESTAMP");
	targets[2] = ATOM("LENGTH");
	targets[3] = ATOM("STRING");
	targets[4] = ATOM("DELETE");

	sel_targets = xv_create(sel, SELECTION_ITEM,
			SEL_TYPE_NAME, 		"TARGETS",
			SEL_FORMAT,		32,
			SEL_LENGTH,		5,
			SEL_DATA,		(Xv_opaque)targets,
			NULL);

	xv_set(shell,
		RECTOBJ_SELECTION_OWNER, sel,
		NULL);

	xv_main_loop(frame);
	exit(0);
}


void
single_click(paint_window, event, canvas_shell, drawtext)
	Xv_window       paint_window;
	Event           *event;
	Canvas_shell    canvas_shell;
	Drawtext	drawtext;
{
	clicked_object = drawtext;
	printf("clicked on %s\n", xv_get(drawtext, DRAWTEXT_STRING));
}

void
selected_proc(drawtext, add)
	Drawtext	drawtext;
	int		add;
{
	printf("%s %s %s selected list.\n",
		(add ? "Added" : "Deleted"),
		xv_get(drawtext, DRAWTEXT_STRING),
		(add ? "to" : "from"));
}


/* xview selection callbacks */


int
selection_convert_proc(sel, target, data, length, format)
	Selection_owner sel;
	Atom           *target;
	Xv_opaque      *data;
	unsigned long  *length;
	int            *format;
{

	if (*target == ATOM("LENGTH")) {
		static unsigned long len;
		char           *contents;

		contents = (char *) xv_get(clicked_object, DRAWTEXT_STRING);
		len = strlen(contents);

		*target = ATOM("INTEGER");
		*format = 32;
		*length = 1;
		*data = (Xv_opaque) & len;
		return (True);
	} 

	if (*target == ATOM("STRING")) {
		char           *contents;

		contents = (char *) xv_get(clicked_object, DRAWTEXT_STRING);

		*target = ATOM("STRING");
		*format = 8;
		*length = strlen(contents);
		*data = (Xv_opaque) strdup(contents);
		return (True);
	}

	if (*target == ATOM("DELETE")) {
		xv_destroy_safe(clicked_object);
		*target = ATOM("NULL");
		*format = 32;
		*length = 0;
		*data = (Xv_opaque) NULL;
		return (True);
	}

	return (sel_convert_proc(sel, target, data, length, format));
}


void
selection_done_proc(sel, data, target)
	Selection_owner      sel;
	Xv_opaque           *data;
	Atom                 target;
{
	if(target == ATOM("STRING"))
		free((char *)data);
}

