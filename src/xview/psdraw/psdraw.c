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
static char     sccsid[] = "@(#)psdraw.c	2.5 91/10/15 Copyright 1991 Sun Microsystems";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xvps/pscanvas.h>
#include <xvps/pscan_ps.h>
#include <math.h>
#include "psdraw_ui.h"
#include <NeWS/psmacros.h>
#include "psdraw.h"

Attr_attribute	INSTANCE;

typedef enum {
	POINT,
	LINE,
	RECTANGLE,
	CIRCLE,
	ELLIPSE,
	TEXT
} DRAW_MODES;

typedef enum {
	SOLID_LINE,
	OUTLINE,
	TWO_TONE,
} LINE_STYLES;

typedef enum {
	SOLID,
	DASH,
	DOT,
	DASHDOT,
} DASH_MODES;

typedef struct {
	int	set;
	int	x;
	int	y;
} point;

psdraw_win_objects	*PSdraw_win;
psdraw_popup_objects	*PSdraw_props;
NeWStoken		Canvas_token;

void		get_initial_values();

DRAW_MODES	Current_mode;
float		Line_color;
float		Line_width;
LINE_STYLES	Line_style;
int		Fill;
float		Fill_color;
DASH_MODES	Line_dash;
int		Hold_mode;
point		Current_point;

void		set_current_point();
void		unset_current_point();
void		draw_point();
void		draw_line();
void		draw_rect();
void		draw_circle();
void		draw_ellipse();  
void		draw_text();

void
main(argc, argv)
	int	argc;
	char	**argv;
{
	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);
	INSTANCE = xv_unique_key();
	
	/*
	 * Initialize user interface components.
	 */
	PSdraw_win = psdraw_win_objects_initialize(NULL, NULL);
	PSdraw_props = psdraw_popup_objects_initialize(NULL, PSdraw_win->win);
	Canvas_token = (NeWStoken) xv_get(PSdraw_win->canvas, PSCANVAS_NEWSTOKEN);

	get_initial_values();

	/*
	 * Turn control over to XView.
	 */
	xv_main_loop(PSdraw_win->win);
	exit(0);
}

/*
 * Get the initial values from the props sheet.
 */
void
get_initial_values()
{
	Current_mode = (DRAW_MODES)xv_get(PSdraw_win->mode, PANEL_VALUE);
	Line_color = xv_get(PSdraw_props->line_color, PANEL_VALUE) / 10.0;
	Line_width = (float)xv_get(PSdraw_props->line_width, PANEL_VALUE);
	Line_style = (LINE_STYLES)xv_get(PSdraw_props->line_style, PANEL_VALUE);
	Fill = (int)xv_get(PSdraw_props->fill, PANEL_VALUE);
	Fill_color = xv_get(PSdraw_props->fill_color, PANEL_VALUE) / 10.0;
	Line_dash = (DASH_MODES)xv_get(PSdraw_props->line_dash, PANEL_VALUE);
	Hold_mode = (int)xv_get(PSdraw_props->hold_mode, PANEL_VALUE);

	ps_setlinewidth(Line_width);
	ps_setlinejoin((int)xv_get(PSdraw_props->line_join, PANEL_VALUE));
	ps_setlinecap((int)xv_get(PSdraw_props->line_cap, PANEL_VALUE));
	ps_setmiterlimit((float)atof((char *)xv_get(PSdraw_props->miter_limit, PANEL_VALUE)));
}

/*
 * Notify callback function for `props_button'.
 */
void
props_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xv_set(PSdraw_props->popup, XV_SHOW, TRUE, WIN_FRONT, 0);
}

/*
 * Notify callback function for `clear_button'.
 */
void
clear_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	ps_setcanvas(Canvas_token);
	ps_fillcanvas(1);
	ps_flush_PostScript();
}

/*
 * Notify callback function for `mode'.
 */
void
mode_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Current_mode = (DRAW_MODES) value;

	if (Current_mode != TEXT)
		xv_set(PSdraw_win->text, PANEL_INACTIVE, TRUE, 0);
	else
		xv_set(PSdraw_win->text, PANEL_INACTIVE, FALSE, 0);
}

/*
 * Notify callback function for `line_color'.
 */
void
line_color_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Line_color = value / 10.0;
}

/*
 * Notify callback function for `fill'.
 */
void
fill_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Fill = value;
}

/*
 * Notify callback function for `line_style'.
 */
void
style_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Line_style = (LINE_STYLES)value;
}

/*
 * Notify callback function for `fill_color'.
 */
void
fill_color_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Fill_color = value / 10.0;
}

/*
 * Notify callback function for `line_join'.
 */
void
line_join_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	ps_setlinejoin(value);

	if (value)
		xv_set(PSdraw_props->miter_limit, PANEL_INACTIVE, TRUE, 0);
	else
		xv_set(PSdraw_props->miter_limit, PANEL_INACTIVE, FALSE, 0);
}

/*
 * Notify callback function for `line_cap'.
 */
void
line_cap_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	ps_setlinecap(value);
}

/*
 * Notify callback function for `line_width'.
 */
void
width_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Line_width = (float) value;
	ps_setlinewidth(Line_width);
}

/*
 * Notify callback function for `miter_limit'.
 */
Panel_setting
miter_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	float	miter_limit = (float)atof((char *)xv_get(item, PANEL_VALUE));

	if (miter_limit <= 1.0) {
		miter_limit = 1.0;
		xv_set(item, PANEL_VALUE, "1", NULL);
	}

	ps_setmiterlimit((float)atof((char *)xv_get(item, PANEL_VALUE)));

	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `line_dash'.
 */
void
dash_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Line_dash = (DASH_MODES) value;
}

/*
 * Notify callback function for `special_mode'.
 */
void
special_proc(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	Hold_mode = value;
}

/*
 * Event callback function for `canvas'.
 */
Notify_value
canvas_events(win, event, arg, type)
	Xv_window	win;
	Event		*event;
	Notify_arg      arg;
	Notify_event_type type;
{
	if (!event_is_down(event))
		return notify_next_event_func(win, event, arg, type);

	ps_setcanvas(Canvas_token);

	switch (event_action(event)) {
		case ACTION_SELECT:
			unset_current_point();

			switch (Current_mode) {
			case POINT:
				draw_point(event_x(event), event_y(event));
				break;
			case TEXT:
				draw_text(event_x(event), event_y(event));
				break;
			default:
				set_current_point(event_x(event), event_y(event));
				break;
			}
			break;

		case LOC_DRAG:
			if (!Hold_mode)
				break;

			if (!(action_select_is_down(event) ||
			      action_adjust_is_down(event)))
				break;

		case ACTION_ADJUST:
			switch (Current_mode) {
			case POINT:
				draw_point(event_x(event), event_y(event));
				break;
			case LINE:
				draw_line(event_x(event), event_y(event));
				break;
			case RECTANGLE:
				draw_rect(event_x(event), event_y(event));
				break;
			case CIRCLE:
				draw_circle(event_x(event), event_y(event));
				break;
			case ELLIPSE:
				draw_ellipse(event_x(event), event_y(event));
				break;
			case TEXT:
				draw_text(event_x(event), event_y(event));
				break;
			default:
				break;
			}
			break;

		default:
			break;
	}

	ps_flush_PostScript();
	return notify_next_event_func(win, (Notify_event) event, arg, type);
}

void
set_current_point(x, y)
	int	x;
	int	y;
{
	Current_point.x = x;
	Current_point.y = y;
	Current_point.set = TRUE;
}

void
unset_current_point()
{
	Current_point.set = FALSE;
}

void
draw_point(x, y)
	int	x;
	int	y;
{
	int	height;

	height = Line_width / 2;
	set_current_point(x, y);
	ps_draw_rect(x - height, y - height, x + height, y + height);
	ps_setlinedash(SOLID);
	ps_line_stroke(Line_style, Line_color);
	unset_current_point();
}

void
draw_line(x, y)
	int	x;
	int	y;
{

	if (Current_point.set) {
		ps_draw_line(Current_point.x, Current_point.y, x, y);
		ps_setlinedash(Line_dash);
		ps_line_stroke(Line_style, Line_color);
	}

	set_current_point(x, y);
}

void
draw_rect(x, y)
	int	x;
	int	y;
{
	if (Current_point.set) {
		ps_draw_rect(Current_point.x, Current_point.y, x, y);
		if (Fill) {
			ps_gsave();
			ps_fill_color(Fill_color);
			ps_grestore();
		}

		ps_gsave();
		ps_setlinedash(Line_dash);
		ps_line_stroke(Line_style, Line_color);
		ps_grestore();

		if (!Hold_mode)
			unset_current_point();
	}
}

void
draw_circle(x, y)
	int	x;
	int	y;
{
	double	xlen;
	double	ylen;
	int	radius;

	if (Current_point.set) {
		xlen = (double)(Current_point.x - x);
		ylen = (double)(Current_point.y - y);
		radius = (int)sqrt((double)(xlen*xlen + ylen*ylen));
		ps_draw_circle(Current_point.x, Current_point.y, radius);
		if (Fill) {
			ps_gsave();
			ps_fill_color(Fill_color);
			ps_grestore();
		}

		ps_gsave();
		ps_setlinedash(Line_dash);
		ps_line_stroke(Line_style, Line_color);
		ps_grestore();

		if (!Hold_mode)
			unset_current_point();
	}
}

void
draw_ellipse(x, y)
	int	x;
	int	y;
{
	if (Current_point.set) {
		ps_draw_ellipse(Current_point.x, Current_point.y, x, y);
		if (Fill) {
			ps_gsave();
			ps_fill_color(Fill_color);
			ps_grestore();
		}

		ps_gsave();
		ps_setlinedash(Line_dash);
		ps_line_stroke(Line_style, Line_color);
		ps_grestore();

		if (!Hold_mode)
			unset_current_point();
	}
}

void
draw_text(x, y)
	int	x;
	int	y;
{
	char	*font = (char *)xv_get(PSdraw_props->font, PANEL_CHOICE_STRING,
				xv_get(PSdraw_props->font, PANEL_VALUE));
	int	font_size = (int) xv_get(PSdraw_props->font_size, PANEL_VALUE);
	char	*s = (char *) xv_get(PSdraw_win->text, PANEL_VALUE);

	if (s && *s) {
		ps_draw_text(x, y, font_size, font, s);
		if (!Hold_mode)
			unset_current_point();
	}
}

/*
 * Repaint callback function for `canvas'.
 */
void
canvas_repaint(pscanvas, newstoken, display, xid, rects)
	PScanvas	pscanvas;
	NeWStoken	newstoken;
	Display		*display;
	Window		xid;
	Xv_xrectlist	*rects;
{
	xv_set(pscanvas, PSCANVAS_CLIPRECTS, rects, 0);
	ps_fillcanvas(1);
	xv_set(pscanvas, PSCANVAS_CLIPRECTS, NULL, 0);
}
