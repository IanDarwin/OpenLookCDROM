/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) drawrect.c 1.6 92/11/06 
 */

#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/cms.h>
#include <sspkg/canshell.h> 
#include <sspkg/drawobj.h> 
 
Frame		frame;
Canvas_shell	shell;
Drawrect	drawrect;
Panel_item	dwidth;
Panel_item	dheight;

#define ATTR_KEY XV_KEY_DATA, attr_key
int attr_key;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	Panel		panel;
	Panel_item	ptext;
	Drawtext	drawtext;
	Cms		cms;
	void		show_values();
	void		set_opaque();
	Panel_item	create_int_attr_item();

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	attr_key = xv_unique_key();
 
	cms = (Cms) xv_create(XV_NULL, CMS,
		CMS_CONTROL_CMS, TRUE,
		CMS_SIZE, CMS_CONTROL_COLORS + 1,
		CMS_NAMED_COLORS, "black", 0,
		NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			NULL);
 
	panel = (Panel) xv_create(frame, PANEL,
			PANEL_LAYOUT, PANEL_VERTICAL,
			NULL);

	dwidth = create_int_attr_item(panel, "XV_WIDTH", 
		XV_WIDTH, 300);
	dheight = create_int_attr_item(panel, "XV_HEIGHT", 
		XV_HEIGHT, 300);
	(void) create_int_attr_item(panel, "DRAWRECT_BORDER1", 
		DRAWRECT_BORDER1, 100);
	(void) create_int_attr_item(panel, "DRAWRECT_BORDER2", 
		DRAWRECT_BORDER2, 100);
	(void) create_int_attr_item(panel, "DRAWRECT_BORDER3", 
		DRAWRECT_BORDER3, 100);

	(void) xv_create(panel, PANEL_CHOICE,
			PANEL_LABEL_STRING, "DRAWOBJ_FILLED:",
			PANEL_CHOICE_STRINGS, "FALSE", "TRUE", NULL,
			PANEL_NOTIFY_PROC, set_opaque,
			PANEL_VALUE, 1,
			NULL);

	window_fit(panel);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
			WIN_CMS, cms,
			XV_X, 0,
			XV_Y, 0,
			CANVAS_MIN_PAINT_WIDTH, 100,
			CANVAS_MIN_PAINT_HEIGHT, 40,
			XV_HEIGHT, 100,
			WIN_BELOW, panel,
			NULL);

	drawrect = (Drawrect) xv_create(shell, DRAWRECT,
			DRAWOBJ_FILLED, TRUE,
			XV_X, 10,
			XV_Y, 10,
			NULL);

	/* something to fill the space */
	(void) xv_create(drawrect, DRAWTEXT,
			DRAWTEXT_STRING, "sample",
			NULL);

	show_values();

	window_fit(shell);
	window_fit(frame);
	xv_set(panel, 
		XV_WIDTH, WIN_EXTEND_TO_EDGE, 
		NULL);

	xv_main_loop(frame); 
} 
 


void
show_values()
{
	xv_set(dwidth, 
		PANEL_VALUE, xv_get(drawrect, XV_WIDTH), 
		NULL);
	xv_set(dheight, 
		PANEL_VALUE, xv_get(drawrect, XV_HEIGHT), 
		NULL);
}


void
set_int_attr(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	int attr;

	attr = xv_get(item, ATTR_KEY);
	xv_set(drawrect, 
		attr, value,
		NULL);
	show_values();
}

Panel_item
create_int_attr_item(panel, label, attr, max)
	Panel panel;
	char *label;
	int attr;
	int max;
{
	return (Panel_item) xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, label,
			PANEL_MAX_VALUE, max,
			PANEL_NOTIFY_PROC, set_int_attr,
			ATTR_KEY, attr,
			NULL);
}



void
set_opaque(item, event)
	Panel_item	item;
	Event		*event;
{
	xv_set(drawrect, 
		DRAWOBJ_FILLED, xv_get(item, PANEL_VALUE),
		NULL);
}

