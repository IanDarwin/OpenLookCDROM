/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)bkg_event.c 1.14 92/11/12";
#endif
#endif

#include <X11/Xlib.h>
#include <xview/openmenu.h>
#include <xview/win_input.h>
#include <xview/canvas.h>
#include "canshell_impl.h"
#include <sspkg/rectobj.h>



int
count_buttons_down(event)
	Event *event;
{
	int down = 0;
	if( event_right_is_down(event) )        down++;
	if( event_left_is_down(event) )         down++;
	if( event_middle_is_down(event) )       down++;
	return down;
}


static int startx, starty, lastx, lasty;
static short rubber_adjust_selection;
static GC xor_gc;


static void
draw_rubberband(canvas_shell)
	Canvas_shell canvas_shell;
{
	Xv_window xv_win;
	int	x1, y1;
	int	x2, y2;
	int	width, height;
	Canvas_shell_info *csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);

	x1 = MIN(startx, lastx);
	y1 = MIN(starty, lasty);
	x2 = MAX(startx, lastx);
	y2 = MAX(starty, lasty);

	width = x2 - x1;
	height= y2 - y1;

	CANVAS_EACH_PAINT_WINDOW(canvas_shell, xv_win)
	    XDrawRectangle(csinfo->shared_info.dpy, xv_get(xv_win, XV_XID), 
			xor_gc,
			x1, y1, width, height);
	CANVAS_END_EACH
}


typedef struct {
	Event	*event;
	Rect	enclosing_rect;
} Rubber_info;

static void *
rubberband_select_rectobj(rectobj, info)
	Rectobj rectobj;
	Rubber_info *info;
{
	Rect *rect;
	
	if(xv_get(rectobj, RECTOBJ_SELECTABLE) && xv_get(rectobj, XV_SHOW)) {
	  rect = (Rect*) xv_get(rectobj, XV_RECT);
	  if(rect_includesrect(&info->enclosing_rect, rect)) {
		if(rubber_adjust_selection) {
			if( xv_get(rectobj, RECTOBJ_SELECTED) ) {
				/* del from */
				rectobj_del_from_selected_list(rectobj, 
					info->event);
				rectobj_set_paint_style(rectobj,
					info->event, RECTOBJ_NORMAL);
			} else {
				rectobj_add_to_selected_list(rectobj, 
					FALSE, info->event);
				rectobj_set_paint_style(rectobj,
					info->event, RECTOBJ_HIGHLIGHT);
			}
		} else {
			rectobj_add_to_selected_list(rectobj, FALSE,
				info->event);
			rectobj_set_paint_style(rectobj,
				info->event, RECTOBJ_HIGHLIGHT);
		}
	  }
	}
	return (void*) 0;
}


/*
 * when the rubberband is released, check all the items to see if
 * they were within the selected area.  If the middle button was
 * used, flip the states.
 */
static void
rubberband_select(canvas_shell, event)
	Canvas_shell 	canvas_shell;
	Event		*event;
{
	Rubber_info info;
	int	x2, y2;

	info.enclosing_rect.r_left = MIN(startx, lastx);
	info.enclosing_rect.r_top  = MIN(starty, lasty);
	x2 = MAX(startx, lastx);
	y2 = MAX(starty, lasty);

	info.event = event;
	info.enclosing_rect.r_width = x2 - info.enclosing_rect.r_left;
	info.enclosing_rect.r_height= y2 - info.enclosing_rect.r_top;

	traverse_rectobj_tree(canvas_shell, 
		rubberband_select_rectobj, &info);
}


static void
rubber_event_move_proc(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{
	Canvas_shell_info	*csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);

	if(event_action(event) == LOC_DRAG) {
			draw_rubberband(canvas_shell);
			lastx = event_x(event);
			lasty = event_y(event);
			draw_rubberband(canvas_shell);
			return;
	}

	if(event_is_button(event)) {
		if(event_is_down(event)) {
			/*
			 * Don't handle more than one button down,
			 * reset when a second one goes down.
			 */
			draw_rubberband(canvas_shell);
			XFreeGC(csinfo->shared_info.dpy, xor_gc);
			rectobj_set_event_grab(canvas_shell, 0, 0, 0);
			return;
		}
	} else {
		if(event_is_iso(event)) {
			/* reset on keyboard events */
			draw_rubberband(canvas_shell);
			XFreeGC(csinfo->shared_info.dpy, xor_gc);
			rectobj_set_event_grab(canvas_shell, 0, 0, 0);
		}
		return; /* ignore non-button events */
	}

	/* assert(event_is_button(event) && event_is_up(event)); */
	/* assert(event_action(event) == ACTION_SELECT) ||
		  event_action(event) == ACTION_ADJUST)); */

	draw_rubberband(canvas_shell);
	rubberband_select(canvas_shell, event);
	XFreeGC(csinfo->shared_info.dpy, xor_gc);
	rectobj_set_event_grab(canvas_shell, 0, 0, 0);
}


void
background_event_proc(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{
	Canvas_shell_info	*csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);

	if(!event_is_button(event))
		return;

	if(!event_is_down(event))
		return;

	switch(event_action(event)) {
		case ACTION_MENU:
			rectobj_menu_show(paint_window, event, rectobj);
			return;

		case ACTION_SELECT:
			/* clear the selection */
			rectobj_add_to_selected_list(NULL, TRUE, event);
			rubber_adjust_selection = FALSE;
			break;

		case ACTION_ADJUST:
			rubber_adjust_selection = TRUE;
			break;

		default:
			return;
	}

	xor_gc = XCreateGC(csinfo->shared_info.dpy,
			xv_get(paint_window, XV_XID), 0, 0);
	XSetForeground(csinfo->shared_info.dpy, xor_gc,
			xv_get(canvas_shell, WIN_FOREGROUND_COLOR));
	XSetFunction(csinfo->shared_info.dpy, xor_gc, GXxor);

	lastx = startx = event_x(event);
	lasty = starty = event_y(event);

	rectobj_set_event_grab(canvas_shell, rectobj, 
		rubber_event_move_proc, NULL);
}


void
rectobj_menu_show(w, e, rectobj)
	Xv_Window	w;
	Event		*e;
	Rectobj 	rectobj;
{
	Menu 	menu;

	if(rectobj_upsearch(rectobj, &menu, RECTOBJ_MENU))
		menu_show(menu, w, e, NULL);
}


void
rectobj_help_show(w, e, rectobj)
	Xv_Window	w;
	Event		*e;
	Rectobj		rectobj;
{
	char *help_data;
	
	if(rectobj_upsearch(rectobj, (Xv_opaque*)&help_data, XV_HELP_DATA))
		xv_help_show(w, help_data, e);
}

