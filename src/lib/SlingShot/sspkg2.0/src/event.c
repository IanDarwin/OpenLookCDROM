/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)event.c 1.31 92/11/10";
#endif
#endif


#include <xview/win_input.h>
#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <assert.h>


static
is_dbl_click(then, now)
	struct timeval *then;
	struct timeval *now;
{
	struct timeval delta;
	static int multiclicktimeout = 0;
 
	delta.tv_sec = now->tv_sec - then->tv_sec;
	if ((delta.tv_usec = now->tv_usec - then->tv_usec) < 0) {
		delta.tv_usec += 1000000;
		delta.tv_sec -= 1;
	}

	if(multiclicktimeout == 0) {
		multiclicktimeout = defaults_get_integer(
				"slingshot.doubleclicktime",
				"SlingShot.DoubleClickTime", 5);
	}
	/* compare the delta against the multi-click timeout */
	/* (a value between 2 and 10 tenths of second) */
	if((delta.tv_sec*10 + delta.tv_usec/100000) <= multiclicktimeout)
		return TRUE;
	return FALSE;
}
 

#define call_proc(paint_window, event, canvas_shell, rectobj, attr, opt)\
	{								\
	  Proc_ptr callback;						\
									\
	  callback = (Proc_ptr) xv_get(rectobj, attr);			\
	  if(callback)							\
		(callback)(paint_window, event, canvas_shell, rectobj, opt);\
	}


#define ABS(a)		(((a) < 0) ? -(a) : (a))

typedef struct {
	struct timeval	prev_time;
	Rectobj		object;
	int		btn_down_x;
	int		btn_down_y;
	short		selectable;	/*bool*/
	short		pre_selected;	/*bool*/
	short		adjust;		/*bool*/
	Event		*event;		/*only valid for start_drag*/
} Click_info;

static	Click_info	click_info;


void
rectobj_redo_start_drag(rectobj, warp)
	Rectobj	rectobj;
	int	warp; /* adjust btn_down_x,y to pos of click object if true */
{
	Proc_ptr callback;
	callback = (Proc_ptr) xv_get(rectobj, RECTOBJ_START_DRAG_PROC);
	if(callback)
		(callback)(click_info.event->ie_win,
			click_info.event, 
			(Canvas_shell) xv_get(rectobj, RECTOBJ_CANVAS), 
			rectobj, 
			click_info.btn_down_x - (warp ?
				xv_get(click_info.object, RECTOBJ_X) : 0),
			click_info.btn_down_y - (warp ?
				xv_get(click_info.object, RECTOBJ_Y) : 0),
			click_info.adjust);
}


static void
wait_for_select_up(paint_window, event, canvas_shell, grab_arg)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	void		*grab_arg;
{
	/*
	 * Called after the button down, and until it goes up.
	 * The mouse is grabbed by the default, so this will always
	 * get a btn up event.
	 * This may handle multiple events, awaiting either
	 *	1) multiple drag events ending up outside the rectobj,
	 *	2) a single button up event.
	 *
	 * Miscellaneous events, like keyboard events, are unexpected
	 * so in this case assume that something happened and release
	 * the canvas-grab.
	 *
	 * Double click times are measured between up events.
	 */

	if(event_action(event) == LOC_DRAG) {
		static int	threshold;
		int		dist_x;
		int		dist_y;
	
		if(threshold == 0)
			threshold = defaults_get_integer(
				"slingshot.clickmovethreshold",
				"SlingShot.ClickMoveThreshold", 
				5);

		/*
		 * TODO Feature: if a modifier is down, do drag
		 * immediately (ala cmd in olwm) instead of waiting
		 * for it to exceed threshold.
		 * or should that be the default if no dbl_click proc is
		 * specified?
		 */

		dist_x = ABS(event_x(event) - click_info.btn_down_x);
		dist_y = ABS(event_y(event) - click_info.btn_down_y);

		if(((dist_x > threshold) || (dist_y > threshold)) &&
		   xv_get(click_info.object, RECTOBJ_DRAGGABLE)) {
		    Proc_ptr callback;
		    click_info.event = event;
		    callback = (Proc_ptr) xv_get(click_info.object, 
			RECTOBJ_START_DRAG_PROC);
		    if(callback) {
			/* Call back should install grab of its own. */
			rectobj_set_event_grab(canvas_shell, 0, 0, 0);
			(callback)(paint_window, event, canvas_shell, 
				click_info.object, 
				click_info.btn_down_x, click_info.btn_down_y,
				click_info.adjust);
		    }
		}
		return;
	}

	if(event_is_button(event) && event_is_down(event)) {
		/*
		 * Don't handle situation where user has clicked 
		 * more than one button.
		 * All "selectable" objects are on selected list when in this 
		 * function, so set object's state to highlight (otherwise 
		 * unhighlight).
		 */
		rectobj_set_paint_style(click_info.object, event, 
			(click_info.selectable ?
				RECTOBJ_HIGHLIGHT : RECTOBJ_NORMAL));
		rectobj_set_event_grab(canvas_shell, 0, 0, 0);
		return;
	}

	if(event_action(event) == ACTION_SELECT) {
	    assert(event_is_up(event));
	    /*
	     * Test for double click.  If this is the first up event
	     * the prev_time should be (0, 0) which will guarantee that
	     * no double click operation occurs.
	     * Non selectable objects do not double click.
	     */
	    if(click_info.selectable && 
	       is_dbl_click(&click_info.prev_time, &event_time(event))) {
		/* reset time to avoid triple click */
		click_info.prev_time.tv_sec = 0;
		click_info.prev_time.tv_usec = 0;

		call_proc(paint_window, event, canvas_shell, 
			click_info.object, RECTOBJ_DBL_CLICK_PROC, 0);
	    } else {
	    	/* Reset the timer for the next time through. */
		click_info.prev_time = event_time(event);

		call_proc(paint_window, event, canvas_shell, 
			click_info.object, RECTOBJ_SINGLE_CLICK_PROC, 0);
	    }

	    rectobj_set_paint_style(click_info.object, event,
			(click_info.selectable ?
				RECTOBJ_HIGHLIGHT : RECTOBJ_NORMAL));
	    rectobj_set_event_grab(canvas_shell, 0, 0, 0);
	    return;
	}


	if(event_action(event) == ACTION_ADJUST) {
	    assert(event_is_up(event));

	    if(click_info.pre_selected) {
		rectobj_del_from_selected_list(click_info.object, event);
	    	rectobj_set_paint_style(click_info.object, event,
			RECTOBJ_NORMAL);
	    } else {
		call_proc(paint_window, event, canvas_shell, 
			click_info.object, RECTOBJ_SINGLE_CLICK_PROC, 0);
		rectobj_set_paint_style(click_info.object, event,
			(click_info.selectable ?
				RECTOBJ_HIGHLIGHT : RECTOBJ_NORMAL));
	    }
	    rectobj_set_event_grab(canvas_shell, 0, 0, 0);
	    return;
	}

	/*
	 * Other events may occur here, such as WIN_NO_EXPOSE, KBD_USE, etc.
	 * 
	 * To handle the myraid of cases, make the assumption 
	 * that misc button & keyboard events should remove
	 * the canvas-shell-grab and otherwise assume that
	 * everything else is okay.
	 */
	if(!event_is_button(event) && !event_is_iso(event))
		return; /* else fall through and release grab */

	rectobj_set_paint_style(click_info.object, event, RECTOBJ_NORMAL);
	rectobj_set_event_grab(canvas_shell, 0, 0, 0);
}


static void
action_mouse_down(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{
	/*
	 * Handle 1 event: the start of a selection/highlight/state change.
	 *	check XV_SHOW to make sure.
	 *	record click time and object that was clicked on.
	 *	install handler for up click.
	 *	add to selected list/set stacking position
	 */

	if(!xv_get(rectobj, XV_SHOW))
		/*
		 * Sanity.  Events shouldn't get mapped to unpainted objects.
		 */ 
		return;

	if(click_info.object != rectobj) {
		/*
		 * This object was not previously clicked on.  
		 */
		click_info.object = rectobj;
		click_info.prev_time.tv_sec = 0;
		click_info.prev_time.tv_usec = 0;
	}
	click_info.btn_down_x = event_x(event);
	click_info.btn_down_y = event_y(event);
	click_info.pre_selected = xv_get(rectobj, RECTOBJ_SELECTED);
	click_info.selectable = xv_get(rectobj, RECTOBJ_SELECTABLE);

	if(event_action(event) == ACTION_SELECT) {
		/* 
		 * This test controls user interactions:
		 * some people like it so that clicking on a pre_selected
		 * object clears all others, and some don't.  It may
		 * be possible to set this as a resource option and
		 * check it here.
		 */
		if(click_info.pre_selected == FALSE)
			rectobj_add_to_selected_list(rectobj, TRUE, event);
		click_info.adjust = FALSE;
	} else {
		if(click_info.pre_selected == FALSE)
			rectobj_add_to_selected_list(rectobj, 
				(xv_get(rectobj, RECTOBJ_EXCLUSIVE_SELECT) ?
					TRUE:
					FALSE),
				event);
		click_info.adjust = TRUE;
	}

	rectobj_set_paint_style(rectobj, event, RECTOBJ_PREHIGHLIGHT);
	rectobj_set_event_grab(canvas_shell, rectobj, wait_for_select_up, NULL);
}


void
rectobj_selection_event_proc(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{
	switch(event_action(event)) {

	case ACTION_MENU:
		if (event_is_down(event))
			rectobj_menu_show(paint_window, event, rectobj);
		return;

	case ACTION_SELECT:
	case ACTION_ADJUST:
		if(event_is_down(event))
			action_mouse_down(paint_window, event, 
					canvas_shell, rectobj, FALSE);
		return;

	default:
		;		/* other events here */
	}
}


int
start_dbl_click(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{	
	/*
	 * For backward compatability: some 1.0 clients used this function.
	 */
	rectobj_selection_event_proc(
		paint_window, event, canvas_shell, rectobj);
	return TRUE;
}


typedef struct {
	Rectobj	object;
	char	pre_selected;
} Button_event_info;

static void
wait_for_button_up(paint_window, event, canvas_shell, grab_arg)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	void		*grab_arg;
{
	Button_event_info *binfo = (Button_event_info*)grab_arg;
	Rectobj new;

	if(event_action(event) == ACTION_SELECT) {
		if(event_is_up(event)) {
		  if(binfo->object == event_to_rectobj(canvas_shell, event)) {
			call_proc(paint_window, event, canvas_shell, 
				binfo->object, RECTOBJ_SINGLE_CLICK_PROC, 0);

			rectobj_set_paint_style(binfo->object, event, 
				RECTOBJ_NORMAL);
	 	  }
		  rectobj_set_event_grab(canvas_shell, 0, 0, 0);
		}
		return;
	}

	if(event_action(event) == LOC_DRAG) {
		new = event_to_rectobj(canvas_shell, event);
		if((new == binfo->object) && (binfo->pre_selected == FALSE)) {
			binfo->pre_selected = TRUE;
			rectobj_set_paint_style(binfo->object, event,
				RECTOBJ_PREHIGHLIGHT);
		} else
		if((new != binfo->object) && (binfo->pre_selected == TRUE)) {
			binfo->pre_selected = FALSE;
			rectobj_set_paint_style(binfo->object, event,
				RECTOBJ_NORMAL);
		}
		return; 
	} 
	/* 
	 * As above, don't fall through and release grab 
	 * on misc events.
	 */
	if(!event_is_button(event) && !event_is_iso(event))
		return;

	rectobj_set_paint_style(binfo->object, event, RECTOBJ_NORMAL);
	rectobj_set_event_grab(canvas_shell, 0, 0, 0);
}


void
rectobj_button_event_proc(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{
	static Button_event_info	info;

	switch(event_action(event)) {

	case ACTION_MENU:
		if (event_is_down(event)) {
			rectobj_set_paint_style(rectobj, event,
				RECTOBJ_PREHIGHLIGHT);
			rectobj_menu_show(paint_window, event, rectobj);
			/* Issues:
			 * menu_show returns immediately! 
			 * How is the menu positioned ala openlook?
			 */
			rectobj_set_paint_style(rectobj, event, RECTOBJ_NORMAL);
		}
		return;

	case ACTION_SELECT:
		if(event_is_down(event)) {
			rectobj_set_paint_style(rectobj, event,
				RECTOBJ_PREHIGHLIGHT);
			info.object = rectobj;
			info.pre_selected = TRUE;
			rectobj_set_event_grab(canvas_shell, rectobj,
				wait_for_button_up, &info);
		}
		return;

	default:
		;		/* other events here */
	}
}


typedef struct {
	Rectobj	object;
	int	desired_state;
	int	current_state;
	int	initial_style;
} Toggle_event_info;

static void
wait_for_toggle_up(paint_window, event, canvas_shell, grab_arg)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	void		*grab_arg;
{
	Toggle_event_info *tinfo = (Toggle_event_info*)grab_arg;
	Rectobj new;

	if(event_action(event) == ACTION_SELECT) {
		if(event_is_up(event)) {
		  if(tinfo->object == event_to_rectobj(canvas_shell, event)) {
			call_proc(paint_window, event, canvas_shell, 
			 tinfo->object, RECTOBJ_SINGLE_CLICK_PROC, 
				xv_get(tinfo->object, RECTOBJ_TOGGLE_STATE));
	 	  }
		  rectobj_set_event_grab(canvas_shell, 0, 0, 0);
		}
		return;
	}

	if(event_action(event) == LOC_DRAG) {
		new = event_to_rectobj(canvas_shell, event);
		if(new == tinfo->object) {
			if(tinfo->current_state != tinfo->desired_state) {
				rectobj_set_paint_style(tinfo->object, event,
					tinfo->desired_state);
				tinfo->current_state = tinfo->desired_state;
			}
			return;
		}
		/* new != tinfo->object */
		tinfo->current_state = 
			tinfo->desired_state == RECTOBJ_NORMAL ?
			RECTOBJ_HIGHLIGHT : RECTOBJ_NORMAL;
		rectobj_set_paint_style(tinfo->object, event,
			tinfo->current_state);
		return; 
	}
	/* 
	 * As above, don't fall through and release grab 
	 * on misc events.
	 */
	if(!event_is_button(event) && !event_is_iso(event))
		return;

	/* something unexpected, return to initial state */
	rectobj_set_paint_style(tinfo->object, event, tinfo->initial_style);
	rectobj_set_event_grab(canvas_shell, 0, 0, 0);
}

void
rectobj_toggle_event_proc(paint_window, event, canvas_shell, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Rectobj		rectobj;
{
	static Toggle_event_info	info;
	int state;

	switch(event_action(event)) {

	case ACTION_MENU:
		if (event_is_down(event))
			rectobj_menu_show(paint_window, event, rectobj);
		return;

	case ACTION_SELECT:
		if(event_is_down(event)) {
			state = xv_get(rectobj, RECTOBJ_TOGGLE_STATE);
			rectobj_set_paint_style(rectobj, event,
				state ? RECTOBJ_NORMAL : RECTOBJ_HIGHLIGHT);
			info.initial_style = 
				state ? RECTOBJ_HIGHLIGHT : RECTOBJ_NORMAL;
			info.object = rectobj;
			info.desired_state = 
			info.current_state = 
				state ? RECTOBJ_NORMAL : RECTOBJ_HIGHLIGHT;
			rectobj_set_event_grab(canvas_shell, rectobj,
				wait_for_toggle_up, &info);
		}
		return;

	default:
		;		/* other events here */
	}
}

