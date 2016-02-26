/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) clockobj.c 1.9 92/10/23 
 */

#include <math.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/cms.h>
#include <sspkg/canshell.h> 
#include <sspkg/drawobj.h> 
#include <sspkg/box.h> 

Frame		frame;
Canvas_shell	shell;
Clockobj	my_clock;
Cbox		cbox;
Panel_item	hour_slider, min_slider;


#define		WHITE   (0 + CMS_CONTROL_COLORS)
#define		COLOR1  (1 + CMS_CONTROL_COLORS)
#define		COLOR2  (2 + CMS_CONTROL_COLORS)
#define		COLOR3  (3 + CMS_CONTROL_COLORS)
#define		BLACK   (4 + CMS_CONTROL_COLORS)
 

main(argc, argv)
	int	argc;
	char	*argv[];
{
	Panel		panel;
	Panel_item	ptext;
	Cms		cms;
	void		set_hour();
	void		set_min();
	void		set_width();
	void		set_height();
	void		clock_move_proc();
	void		set_movable();
	void		resize();

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
 
	cms = (Cms) xv_create(XV_NULL, CMS,
			CMS_CONTROL_CMS, TRUE,
			CMS_SIZE, CMS_CONTROL_COLORS + 5,
			CMS_NAMED_COLORS, 
					"white", 
					"grey", 
					"blue", 
					"aquamarine", 
					"black", 
					0,
			NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			NULL);
 
	panel = (Panel) xv_create(frame, PANEL,
			WIN_CMS, cms,
			PANEL_LAYOUT, PANEL_VERTICAL,
			NULL);

	hour_slider = (Panel_item) xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "CLOCKOBJ_HR",
			PANEL_VALUE, 12,
			PANEL_MIN_VALUE, 1,
			PANEL_MAX_VALUE, 12,
			PANEL_NOTIFY_PROC, set_hour,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			NULL);

	min_slider = (Panel_item) xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "CLOCKOBJ_MIN",
			PANEL_VALUE, 0,
			PANEL_MAX_VALUE, 59,
			PANEL_NOTIFY_PROC, set_min,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			NULL);

	(void) xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "XV_WIDTH",
			PANEL_VALUE, 200,
			PANEL_MAX_VALUE, 200,
			PANEL_NOTIFY_PROC, set_width,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			NULL);

	(void) xv_create(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "XV_HEIGHT",
			PANEL_VALUE, 200,
			PANEL_MAX_VALUE, 200,
			PANEL_NOTIFY_PROC, set_height,
			PANEL_NOTIFY_LEVEL, PANEL_ALL,
			NULL);

	(void) xv_create(panel, PANEL_CHOICE,
			PANEL_LABEL_STRING, "CLOCKOBJ_MOVABLE",
			PANEL_VALUE, 1,
			PANEL_CHOICE_STRINGS, "FALSE", "TRUE", NULL,
			PANEL_NOTIFY_PROC, set_movable,
			NULL);

	window_fit(panel);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
			WIN_CMS, cms,
			XV_X, 0,
			XV_Y, 0,
			CANVAS_MIN_PAINT_WIDTH, 250,
			CANVAS_MIN_PAINT_HEIGHT, 250,
			XV_HEIGHT, 250,
			WIN_BELOW, panel,
			NULL);

	cbox = (Cbox) xv_create(shell, CBOX, NULL);

	my_clock = (Clockobj) xv_create(cbox, CLOCKOBJ,
			XV_X, 40, 
			XV_Y, 20, 
			XV_WIDTH, 200,
			XV_HEIGHT, 200,
			CLOCKOBJ_MIN, 1,
			CLOCKOBJ_HR, 1,
			CLOCKOBJ_MOVABLE, TRUE,
			CLOCKOBJ_MOVE_PROC, clock_move_proc,
			NULL);

	VClear(my_clock);
	VSetColor(my_clock, WHITE);
	VFillArc(my_clock, 0, 0, 10000, 10000, 0, 360*64);
	VSetColor(my_clock, BLACK);
	VDrawArc(my_clock, 0, 0, 10000, 10000, 0, 360*64);

	xv_set(xv_get(my_clock, CLOCKOBJ_HR_DRAWAREA),
			RECTOBJ_FG, BLACK,
			RECTOBJ_BG, COLOR3,
			NULL);

	xv_set(xv_get(my_clock, CLOCKOBJ_MIN_DRAWAREA),
			RECTOBJ_FG, BLACK,
			RECTOBJ_BG, COLOR2,
			NULL);

	/* 
	 * bug: the hands should repaint when setting fg_color, but
	 * they dont, so set them afterwords. 
	 */
	xv_set(my_clock, 
			CLOCKOBJ_MIN, 0,
			CLOCKOBJ_HR, 12,
			NULL);

	window_fit(shell);
	xv_set(shell,
		CANVAS_RESIZE_PROC, resize,
		CANVAS_SHELL_BATCH_REPAINT, TRUE,
		NULL);
	window_fit(frame);
	xv_set(panel, 
		XV_WIDTH, WIN_EXTEND_TO_EDGE, 
		NULL);

	xv_main_loop(frame); 
} 

void
resize(canvas_shell, w, h)
	Canvas_shell canvas_shell;
	int		w;
	int		h;
{
	xv_set(cbox,
		XV_WIDTH, w,
		XV_HEIGHT, h,
		NULL);
}


void
set_hour(item, value, event)
	Panel_item      item;
	int             value;
	Event           *event;
{
	xv_set(my_clock,
		CLOCKOBJ_HR, value,
		NULL);
}

void
set_min(item, value, event)
	Panel_item      item;
	int             value;
	Event           *event;
{
	xv_set(my_clock,
		CLOCKOBJ_MIN, value,
		NULL);
}

void
set_width(item, value, event)
	Panel_item      item;
	int             value;
	Event           *event;
{
	xv_set(my_clock,
		XV_WIDTH, value,
		NULL);
}


void
set_height(item, value, event)
	Panel_item      item;
	int             value;
	Event           *event;
{
	xv_set(my_clock,
		XV_HEIGHT, value,
		NULL);
}

void
set_movable(item, event)
	Panel_item	item;
	Event		*event;
{
	xv_set(my_clock,
		CLOCKOBJ_MOVABLE, xv_get(item, PANEL_VALUE),
		NULL);
}

void
clock_move_proc(clockobj, hour, min, done)
	Clockobj	clockobj;
	int		hour;
	int		min;
	int		done;
{
	if(!done)
		return;
	xv_set(hour_slider, 
		PANEL_VALUE, hour,
		NULL);

	xv_set(min_slider, 
		PANEL_VALUE, min,
		NULL);
}

