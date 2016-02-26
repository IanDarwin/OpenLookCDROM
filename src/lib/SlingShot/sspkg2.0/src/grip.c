/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char sccsid[] = "@(#)grip.c 1.34 92/11/10";
#endif
#endif

#include <xview/svrimage.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include <sspkg/drawobj.h>
#include <sspkg/grip.h>


typedef struct grip_info {
	int			(*grip_move_proc)();
	void			(*grip_done_proc)();
	short			slide_x;
	short			slide_y;
	short			exceed_parent;
	short			min_x;
	short			min_y;
	short			max_x;
	short			max_y;
	short			immediate;
	Grip_rubber_style	rubber_style;
} Grip_info;

typedef struct {
	Grip	grip;
	int	btn_down_x;
	int	btn_down_y;
	int	adjust;
	int	mouse_offset_x;
	int	mouse_offset_y;

	int	last_x;
	int	last_y;
	GC	xor_gc;
	int	first_time;
} Grip_move_info;

static	Grip_move_info	grip_move_info;

#define GRIP_PRIVATE(grip)	XV_PRIVATE(Grip_info, Grip_struct, grip)

extern	void	drawimage_paint_proc();
	int	grip_grab_proc();
extern	void	rectobj_selection_event_proc();
	void	grip_event_proc();
	void	grip_start_drag_proc();
static	void	grip_draw_rubberband();

#define is_temp_grip(_grip_) (((Xv_base*)(_grip_))->pkg == TEMP_GRIP)

#ifdef __STDC__
#define char unsigned char /* preprocessor hack to get ansi to be quiet */
#endif
#include "grip_image.xbm"
#ifdef __STDC__
#undef char
#endif

/*ARGSUSED*/
Pkg_private int
grip_temp_init(parent, grip, avlist)
	Xv_opaque	parent;
	Grip		grip;
	Attr_avlist	avlist;
{
	Grip_info	*ginfo;
	Grip_struct	*grip_object;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(grip);

	static Rectobj_ops rectobj_ops = {
		1000,
		drawimage_paint_proc,
		grip_event_proc,
		rectobj_map_event_proc,
		rectobj_set_geometry_proc,
		rectobj_manage_child_proc,
		NULL,			/* add child */
		NULL,			/* del child */
		NULL,			/* new parent */
		grip_start_drag_proc,
	};

	ginfo = xv_alloc(Grip_info);
	grip_object = (Grip_struct*)grip;
	grip_object->private_data = (Xv_opaque) ginfo;

	ginfo->slide_x = TRUE;
	ginfo->slide_y = TRUE;
	ginfo->grip_move_proc = default_grip_move_proc;

	ginfo->max_x = 32000;
	ginfo->max_y = 32000;
	ginfo->rubber_style = GRIP_RUBBER_NONE;

	rinfo->rectobj_ops = &rectobj_ops;
	RF_UNSET(rinfo, RF_SELECTABLE);

	return(XV_OK);
}


/*ARGSUSED*/
Pkg_private int
grip_init(parent, grip, avlist)
	Xv_opaque	parent;
	Grip		grip;
	Attr_avlist	avlist;
{
	static Server_image grip_image = NULL;
	grip_temp_init(parent, grip, NULL);

	if(grip_image == NULL) {
		grip_image = xv_create(XV_NULL, SERVER_IMAGE,
			XV_WIDTH, grip_image_width,
			XV_HEIGHT, grip_image_height,
			SERVER_IMAGE_X_BITS, grip_image_bits,
			NULL);
	}

	xv_set(grip, 
		DRAWIMAGE_IMAGE1, grip_image,
		NULL);

	return(XV_OK);
}


Pkg_private Xv_opaque
grip_set_avlist(grip, avlist)
	Grip			grip;
	register Attr_avlist	avlist;
{
        register Drawobj_attr attr;
        register Grip_info *ginfo = GRIP_PRIVATE(grip);

	while (attr = (Drawobj_attr) * avlist++)
	  switch (attr) {

		case GRIP_SLIDE_X:
			ginfo->slide_x = (short)*avlist++;
			break;

		case GRIP_SLIDE_Y:
			ginfo->slide_y = (short)*avlist++;
			break;

		case GRIP_EXCEED_PARENT_DIMS:
			ginfo->exceed_parent = (short)*avlist++;
			break;

		case GRIP_MOVE_PROC:
			ginfo->grip_move_proc = (int (*) ())*avlist++;
			break;

		case GRIP_DONE_PROC:
			ginfo->grip_done_proc = (void (*) ())*avlist++;
			break;

		case GRIP_MAX_X:
			ginfo->max_x = (short)*avlist++;
			break;

		case GRIP_MAX_Y:
			ginfo->max_y = (short)*avlist++;
			break;

		case GRIP_MIN_X:
			ginfo->min_x = (short)*avlist++;
			break;

		case GRIP_MIN_Y:
			ginfo->min_y = (short)*avlist++;
			break;

		case GRIP_RUBBER_STYLE:
			ginfo->rubber_style = (Grip_rubber_style) *avlist++;
			break;

		case GRIP_BTN_DOWN_X:
			grip_move_info.btn_down_x = (int) *avlist++;
			break;

		case GRIP_BTN_DOWN_Y:
			grip_move_info.btn_down_y = (int) *avlist++;
			break;

		case GRIP_IMMEDIATE:
			ginfo->immediate = (short) *avlist++;
			break;

		case XV_END_CREATE:
			if(is_temp_grip(grip))
			  /* at create only */
			  rectobj_redo_start_drag(grip, 
				(ginfo->rubber_style == GRIP_RUBBER_NONE));
			break;

		default:
			avlist = attr_skip(attr, avlist);
	  }

	return(XV_OK);
}


/*ARGSUSED*/
Pkg_private Xv_opaque
grip_get_attr(grip, status, which_attr, avlist)
	Grip		grip;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Grip_info  *ginfo = GRIP_PRIVATE(grip);

	switch (which_attr) {

		case GRIP_SLIDE_X:
			return (Xv_opaque) ginfo->slide_x;

		case GRIP_SLIDE_Y:
			return (Xv_opaque) ginfo->slide_y;

		case GRIP_EXCEED_PARENT_DIMS:
			return (Xv_opaque) ginfo->exceed_parent;

		case GRIP_MOVE_PROC:
			return (Xv_opaque) ginfo->grip_move_proc;

		case GRIP_DONE_PROC:
			return (Xv_opaque) ginfo->grip_done_proc;

		case GRIP_MAX_X:
			return (Xv_opaque) ginfo->max_x;

		case GRIP_MAX_Y:
			return (Xv_opaque) ginfo->max_y;

		case GRIP_MIN_X:
			return (Xv_opaque) ginfo->min_x;

		case GRIP_MIN_Y:
			return (Xv_opaque) ginfo->min_y;

		case GRIP_RUBBER_STYLE:
			return (Xv_opaque) ginfo->rubber_style;

		case GRIP_BTN_DOWN_X:
			return (Xv_opaque) grip_move_info.btn_down_x;

		case GRIP_BTN_DOWN_Y:
			return (Xv_opaque) grip_move_info.btn_down_y;

		case GRIP_IMMEDIATE:
			return (Xv_opaque) ginfo->immediate;

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}

/*ARGSUSED*/
Pkg_private int
grip_destroy(grip, status)
	Grip		grip;
	Destroy_status	status;
{
	Grip_info	*ginfo = GRIP_PRIVATE(grip);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(ginfo);
	return XV_OK;
}

int
default_grip_move_proc(paint_window, event, canvas_shell, grip, new_x, new_y)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip grip;
	short *new_x;
	short *new_y;
{
	return TRUE;
}

static void
grip_move(paint_window, event, canvas_shell, grab_arg)
        Xv_window       paint_window;
        Event           *event;
        Canvas_shell    canvas_shell;
        void            *grab_arg;
{
	Grip_move_info	*gminfo = (Grip_move_info*)grab_arg;
	Grip_info	*ginfo = GRIP_PRIVATE(gminfo->grip);
	Rectobj_info	*rinfo;
	short		new_x;
	short		new_y;
	Rect		pr;
	Display		*dpy;
	XEvent		xevent;
	short		xpos, ypos;
	short		border;

	if(event_is_up(event)) {
	  if((gminfo->adjust && event_action(event) == ACTION_ADJUST) ||
	     (!gminfo->adjust && event_action(event) == ACTION_SELECT)) {

		rectobj_set_event_grab(canvas_shell, 0, 0, 0);

		if(ginfo->rubber_style != GRIP_RUBBER_NONE) {
			grip_draw_rubberband(gminfo);
			XFreeGC((Display*) xv_get(canvas_shell, XV_DISPLAY), 
				gminfo->xor_gc);
		}

		if(ginfo->grip_done_proc) {
			(ginfo->grip_done_proc)(paint_window, event, 
				canvas_shell, gminfo->grip, 
				gminfo->last_x, gminfo->last_y);
		}

		if(is_temp_grip(gminfo->grip))
			xv_destroy_safe(gminfo->grip);
		else
		if(!xv_get(gminfo->grip, RECTOBJ_SELECTABLE))
			rectobj_set_paint_style(gminfo->grip, event, 
				RECTOBJ_NORMAL);

		return;
	  }
	} else 
	if(!gminfo->first_time && 
	   ((event_action(event) != LOC_DRAG) || !ginfo->grip_move_proc))
		return;
	/*
	 * handle loc drag events
	 */

	dpy = (Display*) xv_get(canvas_shell, XV_DISPLAY);

	xpos = event_x(event);
	ypos = event_y(event);

	/*
	 * Compress move events.
	 */
	while(XEventsQueued(dpy, QueuedAlready)) {
		XNextEvent(dpy, &xevent);
		if(xevent.type == MotionNotify) {
			xpos = xevent.xmotion.x;
			ypos = xevent.xmotion.y;
		} else {
			XPutBackEvent(dpy, &xevent);
			break;
		}
	}

	rinfo = RECTOBJ_PRIVATE(gminfo->grip);

	if(ginfo->rubber_style == GRIP_RUBBER_NONE) {
		new_x = xpos - gminfo->mouse_offset_x;
		new_y = ypos - gminfo->mouse_offset_y;
	} else {
		new_x = xpos - rinfo->rect.r_width/2;
		new_y = ypos - rinfo->rect.r_height/2;
	}

	/*
	 * anchor vertical/horizontal?
	 */
	if(!ginfo->slide_x)
		new_x = rinfo->rect.r_left;

	if(!ginfo->slide_y)
		new_y = rinfo->rect.r_top;

	/*
	 * clip to parent
	 */
	pr = RECTOBJ_PRIVATE(rinfo->parent)->rect;
	border = RECTOBJ_PRIVATE(rinfo->parent)->border;
	if(!ginfo->exceed_parent) {
		/*
		 * Be careful: border is factored in when xv_set is done,
		 * but not when looking at rinfo->rect.
		 */

		if(new_x < pr.r_left + border)
			new_x = pr.r_left;
		else
		if(new_x + rinfo->rect.r_width > 
				pr.r_left + pr.r_width - border)
			new_x = pr.r_left + pr.r_width - border -
				rinfo->rect.r_width;

		if(new_y < pr.r_top + border)
			new_y = pr.r_top;
		else
		if(new_y + rinfo->rect.r_height > 
				pr.r_top + pr.r_height - border)
			new_y = pr.r_top + pr.r_height - border -
				rinfo->rect.r_height;
	}


	/*
	 * check min/max
	 */
	if(new_x < ginfo->min_x) 
		new_x = ginfo->min_x;
	if(new_x > ginfo->max_x - rinfo->rect.r_width)
		new_x = ginfo->max_x - rinfo->rect.r_width;

	if(new_y < ginfo->min_y)
		new_y = ginfo->min_y;
	if(new_y > ginfo->max_y - rinfo->rect.r_height)
		new_y = ginfo->max_y - rinfo->rect.r_height;


	if((new_x == rinfo->rect.r_left) && (new_y == rinfo->rect.r_top)) {
		/*
		 * No change, return.
		 */
		if(gminfo->first_time) {
			gminfo->last_x = new_x;
			gminfo->last_y = new_y;
			grip_draw_rubberband(gminfo);
		}
		return;
	}


	/*
	 * Transform new_x and new_y into coordinate system relative 
	 * to parent.
	 */
	new_x = new_x - pr.r_left - border;
	new_y = new_y - pr.r_top - border;

	/*
	 * Call the grip_move_proc.  The client can disapprove of the
	 * new position by returning FALSE, or it may override the
	 * by scribbling into the new_x and new_y arguments.
	 */
	if((ginfo->grip_move_proc)(paint_window, event, canvas_shell,
			gminfo->grip, &new_x, &new_y) == TRUE)  {

		if((new_x + pr.r_left != rinfo->rect.r_left) ||
		   (new_y + pr.r_top  != rinfo->rect.r_top)) {
			if(ginfo->rubber_style == GRIP_RUBBER_NONE) {
				xv_set(gminfo->grip, 
					XV_X, new_x,
					XV_Y, new_y, 
					NULL);
			} else {
				/* erase */
				if(!gminfo->first_time)
					grip_draw_rubberband(gminfo);
				xv_set(gminfo->grip, 
					XV_X, new_x,
					XV_Y, new_y, 
					NULL);
				gminfo->last_x = new_x + pr.r_left + border;
				gminfo->last_y = new_y + pr.r_top + border;
				rectobj_flush_repaint(TRUE);
				/* paint */
				grip_draw_rubberband(gminfo);
			}
		}
	}
}


void
grip_start_drag_proc(paint_window, event, canvas_shell, grip, btn_down_x, btn_down_y, adjust)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
	int		btn_down_x;
	int		btn_down_y;
	int		adjust;
{
	Grip_info		*ginfo = GRIP_PRIVATE(grip);
	Rect			*start_rect;
 
	start_rect		= (Rect*) xv_get(grip, XV_RECT);
	grip_move_info.grip		= grip;
	grip_move_info.adjust		= adjust;

	if(ginfo->rubber_style == GRIP_RUBBER_NONE) {
		grip_move_info.btn_down_x = btn_down_x;
		grip_move_info.btn_down_y = btn_down_y;
		if(is_temp_grip(grip)) {
			grip_move_info.mouse_offset_x = btn_down_x;
			grip_move_info.mouse_offset_y = btn_down_y;
		} else {
			grip_move_info.mouse_offset_x = 
					btn_down_x - start_rect->r_left;
			grip_move_info.mouse_offset_y = 
					btn_down_y - start_rect->r_top;
		}
	} else {
		Display *dpy = (Display *) xv_get(canvas_shell, XV_DISPLAY);

		if(is_temp_grip(grip)) {
			grip_move_info.btn_down_x = 
				btn_down_x + start_rect->r_width/2;
			grip_move_info.btn_down_y = 
				btn_down_y + start_rect->r_height/2;
		} else {
			grip_move_info.btn_down_x = 
				start_rect->r_left + start_rect->r_width/2;
			grip_move_info.btn_down_y =
				start_rect->r_top + start_rect->r_height/2;
		}
		grip_move_info.mouse_offset_x = 0;
		grip_move_info.mouse_offset_y = 0;
   
		grip_move_info.xor_gc = XCreateGC(dpy, 
				(XID) xv_get(paint_window, XV_XID),
				0, 0);
		XSetForeground(dpy, grip_move_info.xor_gc,
			(long) xv_get(canvas_shell, WIN_FOREGROUND_COLOR));
		XSetFunction(dpy, grip_move_info.xor_gc, GXxor);
	}

	rectobj_set_event_grab(canvas_shell, grip, grip_move, &grip_move_info);

	grip_move_info.first_time = TRUE;
	grip_move(paint_window, event, canvas_shell, (void*)&grip_move_info);
	grip_move_info.first_time = FALSE;
}


void
grip_event_proc(paint_window, event, canvas_shell, grip)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
{
	Grip_info *ginfo = GRIP_PRIVATE(grip);
	Proc_ptr  proc;

	if(ginfo->immediate && event_is_down(event) && 
	  ((event_action(event) == ACTION_SELECT) || 
	   (event_action(event) == ACTION_ADJUST))) {
		proc = (Proc_ptr) xv_get(grip, RECTOBJ_START_DRAG_PROC);
		if(proc) {
			(proc)(paint_window, event, canvas_shell, grip, 
				event_x(event), event_y(event), 
				(event_action(event) == ACTION_ADJUST));
			return;
		}
	} 
	rectobj_selection_event_proc(paint_window, event, 
		canvas_shell, grip);
}



static void
grip_draw_rubberband(gminfo)
	Grip_move_info	*gminfo;
{
	Xv_window xv_win;
	int     x1, y1;
	int     x2, y2;
	int     width, height;
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(gminfo->grip);
	Rect	*parent_rect;
	int	half_width = rinfo->rect.r_width/2;
	int	half_height = rinfo->rect.r_height/2;

	parent_rect = &RECTOBJ_PRIVATE(rinfo->parent)->rect;

	CANVAS_EACH_PAINT_WINDOW(rinfo->shared_info->canvas_shell, xv_win)
		switch(GRIP_PRIVATE(gminfo->grip)->rubber_style) {
		case GRIP_RUBBER_RECT:
			x1=MIN(gminfo->btn_down_x, gminfo->last_x+half_width);
			y1=MIN(gminfo->btn_down_y, gminfo->last_y+half_height);
			x2=MAX(gminfo->btn_down_x, gminfo->last_x+half_width);
			y2=MAX(gminfo->btn_down_y, gminfo->last_y+half_height);

			width = x2 - x1;
			height= y2 - y1;

			XDrawRectangle(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				x1, y1, width, height);
			break;

		case GRIP_RUBBER_VLINE_PAIR:
			y1 = parent_rect->r_top;
			y2 = y1 + parent_rect->r_height;
			XDrawLine(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				gminfo->btn_down_x, y1, 
				gminfo->btn_down_x, y2);
			if(gminfo->btn_down_x != gminfo->last_x + half_width)
				XDrawLine(
					rinfo->shared_info->dpy, 
					(XID) xv_get(xv_win, XV_XID),
					gminfo->xor_gc,
					gminfo->last_x + half_width, y1, 
					gminfo->last_x + half_width, y2);
			break;

		case GRIP_RUBBER_VLINE:
			y1 = parent_rect->r_top;
			y2 = y1 + parent_rect->r_height;
			XDrawLine(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				gminfo->last_x + half_width, y1, 
				gminfo->last_x + half_width, y2);
			break;

		case GRIP_RUBBER_HLINE_PAIR:
			x1 = parent_rect->r_left;
			x2 = x1 + parent_rect->r_width;
			XDrawLine(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				x1, gminfo->btn_down_y,
				x2, gminfo->btn_down_y);
			if(gminfo->btn_down_y != gminfo->last_y + half_height)
				XDrawLine(
					rinfo->shared_info->dpy, 
					(XID) xv_get(xv_win, XV_XID),
					gminfo->xor_gc,
					x1, gminfo->last_y + half_height,
					x2, gminfo->last_y + half_height);
			break;

		case GRIP_RUBBER_HLINE:
			x1 = parent_rect->r_left;
			x2 = x1 + parent_rect->r_width;
			XDrawLine(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				x1, gminfo->last_y + half_height,
				x2, gminfo->last_y + half_height);
			break;

		case GRIP_RUBBER_CROSSHAIRS:
			x1 = parent_rect->r_left;
			x2 = x1 + parent_rect->r_width;
			XDrawLine(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				x1, gminfo->last_y + half_height,
				x2, gminfo->last_y + half_height);
			y1 = parent_rect->r_top;
			y2 = y1 + parent_rect->r_height;
			XDrawLine(
				rinfo->shared_info->dpy, 
				(XID) xv_get(xv_win, XV_XID),
				gminfo->xor_gc,
				gminfo->last_x + half_width, y1, 
				gminfo->last_x + half_width, y2);
			break;

		/* unimplemented...
		case GRIP_RUBBER_NORTH:
			break;

		case GRIP_RUBBER_SOUTH:
			break;

		case GRIP_RUBBER_EAST:
			break;

		case GRIP_RUBBER_WEST:
			break;
		*/
		}

	CANVAS_END_EACH
}

