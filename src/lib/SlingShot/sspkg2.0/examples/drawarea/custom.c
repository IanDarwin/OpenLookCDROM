/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) custom.c 1.2 92/10/19 
 */

#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <sspkg/canshell.h> 
#include <sspkg/rectobj.h> 
#include <sspkg/drawobj.h> 
#include <sspkg/disp_list.h> 
 
void		resize();
void		set_data();

Drawarea	d1;
int		key;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	Frame		frame;
	Font		font;
	Canvas_shell	shell;
	void		DrawCustomString();
	void		single_click();

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	font = (Font) xv_create(XV_NULL, FONT,
		FONT_NAME, "Helvetica-22",
		NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME, 
		FRAME_LABEL, argv[0],
		NULL);
 
	key = xv_unique_key();

	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
		XV_WIDTH,  400,
		XV_HEIGHT, 300,
		NULL);

	d1 = (Drawarea) xv_create(shell, DRAWAREA,
		DRAWAREA_MAP_EVENTS, DRAWAREA_MAP_LAST,
		XV_WIDTH, xv_get(shell, XV_WIDTH),
		XV_HEIGHT, xv_get(shell, XV_HEIGHT),
		RECTOBJ_SINGLE_CLICK_PROC, single_click,
		NULL);

	DrawCustomString(d1, 5000.0, 2500.0, "Hello World");
	VSetFont(d1, font);
	DrawCustomString(d1, 5000.0, 7500.0, "Hello World");

	xv_set(shell,
		CANVAS_RESIZE_PROC, resize,
		NULL);

	window_fit(frame);
	xv_main_loop(frame); 
} 
 
void
single_click(paint_window, event, canvas_shell, rectobj)
	Xv_window       paint_window;
	Event           *event;
	Canvas_shell    canvas_shell;
	Rectobj		rectobj;
{
	printf("Clicked on text in drawarea.\n");
}


void
resize(shell, width, height)
	Canvas_shell	shell;
	int		width;
	int		height;
{
	xv_set(d1,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL);
}


/*
 * Custom display list functions as described in Programmer's handbook.
 */

typedef struct {
	Display_list_cmd cmd;
	double          virtual_x, virtual_y;
	int             string_length;
	char           *string;
} CustomString;


static void 
CustomStringRender(dl_arg, ptr)
	Display_list_arg *dl_arg;
	CustomString   *ptr;
{
	int             x, y;
	int             center_x, center_y;

	x = dl_convert_rx(dl_arg, ptr->virtual_x);
	y = dl_convert_ry(dl_arg, ptr->virtual_y);

	center_x = x - XTextWidth(dl_arg->font_info,
		ptr->string, ptr->string_length) / 2;
	center_y = y + dl_arg->font_info->ascent -
		(dl_arg->font_info->ascent + dl_arg->font_info->descent) / 2;

	XDrawString(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		center_x, center_y,
		ptr->string,
		ptr->string_length);
}

static int 
CustomStringMapEvent(dl_arg, ptr, event)
	Display_list_arg *dl_arg;
	CustomString   *ptr;
	Event          *event;
{
	Rect            rect;

	rect.r_width = XTextWidth(dl_arg->font_info,
				ptr->string, ptr->string_length);
	rect.r_height = dl_arg->font_info->ascent +
				dl_arg->font_info->descent;

	rect.r_left = dl_convert_rx(dl_arg, ptr->virtual_x) - rect.r_width/2;
	rect.r_top = dl_convert_ry(dl_arg, ptr->virtual_y) -
		(dl_arg->font_info->ascent + dl_arg->font_info->descent) / 2;

	if (rect_includespoint(&rect, event_x(event), event_y(event)))
		return TRUE;	/* The user clicked on this text. */
	else
		return FALSE;	/* Did not click on this text. */
}

void 
DrawCustomString(drawarea, x, y, str)
	Drawarea        drawarea;
	double          x, y;
	char           *str;
{
	CustomString   *ptr;
	static Display_list_vec vector = {
		CustomStringRender,
		CustomStringMapEvent,
		NULL,
	};

	if (!str)
		return;
	ptr = (CustomString *) display_list_append(drawarea,
				&vector, sizeof(CustomString));

	ptr->virtual_x = x;
	ptr->virtual_y = y;
	ptr->string_length = strlen(str);
	ptr->string = str;
}

