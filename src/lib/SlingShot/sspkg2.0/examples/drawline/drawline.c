/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) drawline.c 1.5 92/10/23 
 */
#ifndef lint
#ifdef sccs
static char	sccsid[] = "@(#)lt.c 1.3 91/03/11";
#endif
#endif

#include <xview/xview.h>
#include <xview/panel.h>
#include <sspkg/drawobj.h>
#include <sspkg/canshell.h>
#include <sspkg/grip.h>


Frame		frame;
Panel		panel;
Canvas_shell	shell;
Grip		g0, g1;
Drawline 	l0;
Drawline	help0, help1;
Drawline	helptext;

int		use_endpoint0 = TRUE;
int		use_endpoint1 = TRUE;

int	style_change();
int	use_end();
void	angle_change();
void	length_change();
void	inset_change();
int	grip_move_proc();


main(argc, argv)
	int             argc;
	char           *argv[];
{
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	frame = (Frame) xv_create(NULL, FRAME, 
			FRAME_LABEL, argv[0],
			NULL);

	panel = xv_create(frame, PANEL,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	(void) xv_create(panel, PANEL_CHOICE, 
		PANEL_LABEL_STRING, "Styles",
		PANEL_CHOICE_STRINGS, "None", "Simple", "Hollow", "Filled", 0,
		PANEL_NOTIFY_PROC, style_change,
		PANEL_VALUE, 0,
		NULL);

	(void) xv_create(panel, PANEL_CHOICE, 
		PANEL_LABEL_STRING, "End Point",
		PANEL_CHOICE_STRINGS, "0", "1", "Both", 0,
		PANEL_NOTIFY_PROC, use_end,
		PANEL_VALUE, 2,
		NULL);

	(void) xv_create(panel, PANEL_SLIDER,
		PANEL_LABEL_STRING, "Angle",
		PANEL_VALUE, 60,
		PANEL_MAX_VALUE, 360,
		PANEL_NOTIFY_PROC, angle_change,
		NULL);

	(void) xv_create(panel, PANEL_SLIDER,
		PANEL_LABEL_STRING, "Arrow Length",
		PANEL_VALUE, 10,
		PANEL_MAX_VALUE, 100,
		PANEL_NOTIFY_PROC, length_change,
		NULL);

	(void) xv_create(panel, PANEL_SLIDER,
		PANEL_LABEL_STRING, "Arrow Inset Length",
		PANEL_VALUE, 7,
		PANEL_MAX_VALUE, 100,
		PANEL_NOTIFY_PROC, inset_change,
		NULL);


	window_fit_height(panel);

	shell = xv_create(frame, CANVAS_SHELL, NULL);

	g0 = xv_create(shell, GRIP,
		RECTOBJ_FG, 0,
		GRIP_MOVE_PROC, grip_move_proc,
		XV_X, 90,
		XV_Y, 30,
		XV_WIDTH, 20,
		XV_HEIGHT, 20,
		NULL);

	g1 = xv_create(shell, GRIP,
		RECTOBJ_FG, 0,
		GRIP_MOVE_PROC, grip_move_proc,
		XV_X, 90,
		XV_Y, 190,
		XV_WIDTH, 20,
		XV_HEIGHT, 20,
		NULL);

	l0 = xv_create(shell, DRAWLINE, 
		DRAWLINE_X, 0, 100,
		DRAWLINE_Y, 0, 40,
		DRAWLINE_X, 1, 100,
		DRAWLINE_Y, 1, 200,
		NULL);

	help0 = xv_create(shell, DRAWLINE, 
		DRAWLINE_X, 0, 110,
		DRAWLINE_Y, 0, 43,
		DRAWLINE_ARROW_STYLE, 0, ARROW_FILLED, 
		DRAWLINE_X, 1, 300,
		DRAWLINE_Y, 1, 120,
		NULL);

	help1 = xv_create(shell, DRAWLINE, 
		DRAWLINE_X, 0, 110,
		DRAWLINE_Y, 0, 197,
		DRAWLINE_ARROW_STYLE, 0, ARROW_FILLED, 
		DRAWLINE_X, 1, 300,
		DRAWLINE_Y, 1, 125,
		NULL);

	helptext = xv_create(shell, DRAWTEXT,
		XV_X, 305,
		XV_Y, 117,
		DRAWTEXT_STRING, "Click and drag near the endpoints of the line.",
		NULL);

	window_fit_height(shell);
	window_fit(frame);
	xv_main_loop(frame);
}


int
style_change(item, event)
	Panel_item	item;
	Event		*event;
{
	switch(xv_get(item, PANEL_VALUE)) {
	  case 0:
		if(use_endpoint0)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 0, ARROW_NONE, 
				NULL);

		if(use_endpoint1)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 1, ARROW_NONE, 
				NULL);
		break;

	  case 1:
		if(use_endpoint0)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 0, ARROW_SIMPLE, 
				NULL);
		if(use_endpoint1)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 1, ARROW_SIMPLE, 
				NULL);
		break;

	  case 2:
		if(use_endpoint0)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 0, ARROW_HOLLOW, 
				NULL);
		if(use_endpoint1)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 1, ARROW_HOLLOW, 
				NULL);
		break;
	  case 3:
		if(use_endpoint0)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 0, ARROW_FILLED, 
				NULL);
		if(use_endpoint1)
			xv_set(l0, 
				DRAWLINE_ARROW_STYLE, 1, ARROW_FILLED, 
				NULL);
		break;
	}
	return XV_OK;
}

int
use_end(item, event)
	Panel_item	item;
	Event		*event;
{
	switch(xv_get(item, PANEL_VALUE)) {
	  case 0:
		use_endpoint0 = TRUE;
		use_endpoint1 = FALSE;
		break;
	  case 1:
		use_endpoint0 = FALSE;
		use_endpoint1 = TRUE;
		break;
	  case 2:
		use_endpoint0 = TRUE;
		use_endpoint1 = TRUE;
		break;
	}
}

void
angle_change(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	if(use_endpoint0)
		xv_set(l0,
			DRAWLINE_ARROW_ANGLE, 0, value * 64,
			NULL);
	if(use_endpoint1)
		xv_set(l0,
			DRAWLINE_ARROW_ANGLE, 1, value * 64,
			NULL);
}



void
length_change(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	if(use_endpoint0)
		xv_set(l0,
			DRAWLINE_ARROW_LENGTH, 0, value,
			NULL);
	if(use_endpoint1)
		xv_set(l0,
			DRAWLINE_ARROW_LENGTH, 1, value,
			NULL);
}



void
inset_change(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	if(use_endpoint0)
		xv_set(l0,
			DRAWLINE_ARROW_INSET_LENGTH, 0, value,
			NULL);
	if(use_endpoint1)
		xv_set(l0,
			DRAWLINE_ARROW_INSET_LENGTH, 1, value,
			NULL);
}



int
grip_move_proc(paint_window, event, canvas_shell, grip, new_x, new_y)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
	short		*new_x;
	short		*new_y;
{
	static int removehelp = TRUE;

	if(removehelp) {
		removehelp = FALSE;
		xv_destroy( help0 );
		xv_destroy( help1 );
		xv_destroy( helptext );
	}

	if(grip == g0)
		xv_set(l0,
			DRAWLINE_X, 0, *new_x+10,
			DRAWLINE_Y, 0, *new_y+10,
			NULL);
			
	if(grip == g1)
		xv_set(l0,
			DRAWLINE_X, 1, *new_x+10,
			DRAWLINE_Y, 1, *new_y+10,
			NULL);
			
	return(TRUE);
}

