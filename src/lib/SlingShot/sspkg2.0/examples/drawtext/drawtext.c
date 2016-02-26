/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) drawtext.c 1.6 92/07/07 
 */

#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <sspkg/canshell.h> 
#include <sspkg/drawobj.h> 
 
#define STARTUP_FONT "courier"
#define STARTUP_STRING "Sample Drawtext Object"

Panel_setting	text_change_proc();
Panel_setting	font_change_proc();
void		set_just();
void		set_width();
void		set_height();
void		set_editable();
void		set_show_underline();
void		set_length();
void		update_panel();

Frame		frame;
Canvas_shell	shell;
Drawtext	drawtext;
Panel_item	width_slider, height_slider;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	Panel		panel;
	Panel_item	ptext;
	Xv_Font		startup_font;
	Drawrect	drawrect;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
 
	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			NULL);
 
	panel = (Panel) xv_create(frame, PANEL,
			PANEL_LAYOUT, PANEL_VERTICAL,
			NULL);

	(void) xv_create(panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "DRAWTEXT_STRING:",
			PANEL_VALUE, STARTUP_STRING,
			PANEL_VALUE_DISPLAY_LENGTH, 50,
			PANEL_NOTIFY_PROC, text_change_proc,
			NULL);

	width_slider = xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "XV_WIDTH",
			PANEL_MAX_VALUE, 800,
			PANEL_NOTIFY_PROC, set_width,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			NULL);

	height_slider = xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "XV_HEIGHT",
			PANEL_MAX_VALUE, 400,
			PANEL_NOTIFY_PROC, set_height,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			NULL);

	(void) xv_create(panel, PANEL_CHOICE,
			PANEL_LABEL_STRING, "DRAWTEXT_JUSTIFY:",
			PANEL_CHOICE_STRINGS, 
				"DRAWTEXT_JUSTIFY_LEFT",
				"DRAWTEXT_JUSTIFY_CENTER",
				"DRAWTEXT_JUSTIFY_RIGHT",
				NULL,
			PANEL_NOTIFY_PROC, set_just,
			NULL);

	(void) xv_create(panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "DRAWTEXT_FONT:",
			PANEL_VALUE, STARTUP_FONT,
			PANEL_VALUE_DISPLAY_LENGTH, 50,
			PANEL_NOTIFY_PROC, font_change_proc,
			NULL);

	(void) xv_create(panel, PANEL_CHOICE,
			PANEL_LABEL_STRING, "DRAWTEXT_EDITABLE", 
			PANEL_CHOICE_STRINGS, "FALSE", "TRUE", NULL,
			PANEL_NOTIFY_PROC, set_editable,
			NULL);

	(void) xv_create(panel, PANEL_CHOICE,
			PANEL_LABEL_STRING, "DRAWTEXT_SHOW_UNDERLINE", 
			PANEL_CHOICE_STRINGS, "FALSE", "TRUE", NULL,
			PANEL_NOTIFY_PROC, set_show_underline,
			NULL);

	(void) xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "DRAWTEXT_LENGTH", 
			PANEL_MAX_VALUE, 50,
			PANEL_NOTIFY_PROC, set_length,
			NULL);

	window_fit(panel);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
			XV_X, 0,
			XV_Y, 0,
			CANVAS_MIN_PAINT_WIDTH, 100,
			CANVAS_MIN_PAINT_HEIGHT, 40,
			XV_HEIGHT, 100,
			WIN_BELOW, panel,
			NULL);

	startup_font = (Xv_Font) xv_find(frame, FONT, 
			FONT_NAME, STARTUP_FONT,
			NULL);

	drawrect = (Drawrect) xv_create(shell, DRAWRECT,
			BAG_AUTO_SHRINK, TRUE,
			XV_X, 10,
			XV_Y, 10,
			NULL);

	drawtext = (Drawtext) xv_create(drawrect, DRAWTEXT,
			DRAWTEXT_STRING, STARTUP_STRING,
			DRAWTEXT_FONT, startup_font,
			NULL);

	window_fit(shell);
	window_fit(frame);
	xv_set(panel, 
		XV_WIDTH, WIN_EXTEND_TO_EDGE, 
		NULL);

	update_panel();

	xv_main_loop(frame); 
} 
 


Panel_setting
text_change_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	char		*new_string;

	new_string = (char*) xv_get(item, PANEL_VALUE);

	if(new_string && *new_string)
		xv_set(drawtext,
			DRAWTEXT_STRING, new_string,
			NULL);

	update_panel();

	return PANEL_NEXT;
}

Panel_setting
font_change_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	char	*fontname;
	Xv_Font newfont = 0;

	fontname = (char*) xv_get(item, PANEL_VALUE);

	if(fontname && *fontname) {
		newfont = (Xv_Font) xv_find(frame, FONT, 
			FONT_NAME, fontname,
			NULL);
		if(!newfont) 
			return PANEL_NONE;
	}

	xv_set(drawtext,
		DRAWTEXT_FONT, newfont,
		NULL);

	update_panel();

	return PANEL_NEXT;
}


void
set_just(item, event)
	Panel_item	item;
	Event		*event;
{
	Drawtext_justify_style style;

	switch(xv_get(item, PANEL_VALUE)) {
		case 0:
			style = DRAWTEXT_JUSTIFY_LEFT;
			break;
		case 1:
			style = DRAWTEXT_JUSTIFY_CENTER;
			break;
		case 2:
			style = DRAWTEXT_JUSTIFY_RIGHT;
			break;
	}
	xv_set(drawtext,
		DRAWTEXT_JUSTIFY, style,
		NULL);
}


void
set_width(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xv_set(drawtext, 
		XV_WIDTH, value,
		NULL);
	xv_set(width_slider,
		PANEL_VALUE, xv_get(drawtext, XV_WIDTH),
		NULL);
}



void
set_height(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xv_set(drawtext, 
		XV_HEIGHT, value,
		NULL);
	xv_set(height_slider,
		PANEL_VALUE, xv_get(drawtext, XV_HEIGHT),
		NULL);
}


void
set_editable(item, event)
	Panel_item	item;
	Event		*event;
{
	xv_set(drawtext, 
		DRAWTEXT_EDITABLE, xv_get(item, PANEL_VALUE),
		NULL);
	update_panel();
}


void
set_show_underline(item, event)
	Panel_item	item;
	Event		*event;
{
	xv_set(drawtext, 
		DRAWTEXT_SHOW_UNDERLINE, xv_get(item, PANEL_VALUE),
		NULL);
	update_panel();
}

void
set_length(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xv_set(drawtext, 
		DRAWTEXT_LENGTH, value,
		NULL);
	xv_set(width_slider,
		PANEL_VALUE, xv_get(drawtext, XV_WIDTH),
		NULL);
}


void
update_panel()
{
	xv_set(height_slider,
		PANEL_VALUE, xv_get(drawtext, XV_HEIGHT),
		NULL);

	xv_set(width_slider,
		PANEL_VALUE, xv_get(drawtext, XV_WIDTH),
		NULL);
}

