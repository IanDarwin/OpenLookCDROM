/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)canshell.c 1.50 92/11/08";
#endif
#endif

#include <X11/X.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <xview/cms.h>
#include <xview/font.h>
#include <xview/scrollbar.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/dragdrop.h>
#include <memory.h>
#include "canshell_impl.h"

Pkg_private int 	canvas_shell_init();
Pkg_private Xv_opaque	canvas_shell_set_avlist();
Pkg_private Xv_opaque	canvas_shell_get_attr();
Pkg_private int 	canvas_shell_destroy();

	void	canvas_shell_geometry_manage_proc();
static	void	canvas_shell_update_color();
	void	canvas_shell_repaint_proc();
	void	canvas_shell_event_proc();
	void	canvas_shell_canvas_event_proc();
static	void	canvas_shell_create_drop_site();
static	void	canvas_shell_set_drop_region();
static	void	canvas_shell_alloc_batch_pixmap();

static	Listnode	*canvas_shells;
static	short	no_pixmap_batching;


/*ARGSUSED*/
Pkg_private int
canvas_shell_init(parent, canvas_shell, avlist)
	Xv_opaque	parent;
	Canvas_shell	canvas_shell;
	Attr_avlist	avlist;
{
	Canvas_shell_info	*csinfo;
	Canvas_shell_struct	*canvas_shell_object;
	Shared_info		*sinfo;
	extern void		background_event_proc();
	static short		initialized;
	static Rectobj_ops rectobj_ops = {
		1000,
		rectobj_paint_proc,
		rectobj_event_proc,
		rectobj_map_event_proc,
		rectobj_set_geometry_proc,
		rectobj_manage_child_proc,
		NULL, /* add child */
		NULL, /* del child */
	};
		
	csinfo = xv_alloc(Canvas_shell_info);
	canvas_shell_object = (Canvas_shell_struct*)canvas_shell;
	canvas_shell_object->private_data = (Xv_opaque) csinfo;
	rectobj_internal_init(canvas_shell, &csinfo->rectobj_info);

	/*
	 * Assume it is not painted until we get the first expose
	 * event in repaint proc.  xv_set can override.
	 */
	RF_UNSET(&csinfo->rectobj_info, RF_PAINTED);
	csinfo->rectobj_info.rectobj_ops = (Rectobj_ops*) &rectobj_ops;

	xv_set(canvas_shell, 
		OPENWIN_SPLIT,
			OPENWIN_SPLIT_INIT_PROC, canvas_shell_split_proc, 
			NULL,
		CANVAS_REPAINT_PROC, canvas_shell_repaint_proc,
		CANVAS_RESIZE_PROC, canvas_shell_resize_proc,
		WIN_EVENT_PROC, canvas_shell_canvas_event_proc,
		CANVAS_X_PAINT_WINDOW, TRUE,
		CANVAS_AUTO_EXPAND, TRUE,
		CANVAS_AUTO_SHRINK, TRUE,
		CANVAS_FIXED_IMAGE, FALSE, /* set forget gravity */
		CANVAS_RETAINED, FALSE,
		WIN_CONSUME_EVENTS, WIN_MAP_NOTIFY, NULL,
		CANVAS_PAINTWINDOW_ATTRS,
			WIN_CONSUME_EVENTS,
				WIN_UP_EVENTS, 
				WIN_MOUSE_BUTTONS,
				LOC_DRAG,
				MS_LEFT,
				ACTION_HELP,
				NULL,
			WIN_EVENT_PROC, canvas_shell_event_proc,
			NULL,
		RECTOBJ_EVENT_PROC, background_event_proc,
		/* RECTOBJ_PAINT_PROC, */
		RECTOBJ_MANAGE_CHILD_PROC, canvas_shell_geometry_manage_proc,
		RECTOBJ_SELECTABLE, FALSE,
		NULL);

	/* initialize shared_info */
	sinfo = &csinfo->shared_info;
	sinfo->canvas_shell = canvas_shell;
	sinfo->dpy = (Display*) xv_get(canvas_shell, XV_DISPLAY);
	sinfo->screen_number = (int) xv_get( 
			xv_get(canvas_shell, XV_SCREEN), SCREEN_NUMBER);
	canvas_shell_update_color(canvas_shell, sinfo, (Cms) NULL);

	/* Default font comes from the window class. */
	sinfo->font = (Xv_opaque) xv_get(canvas_shell, XV_FONT);
	sinfo->font_info = (XFontStruct*) xv_get(sinfo->font, FONT_INFO);

	csinfo->rectobj_info.shared_info = sinfo;

	canvas_shells = list_concat(canvas_shells, &csinfo->listnode);
	csinfo->listnode.handle = (void*)csinfo;

	if(!initialized) {
		if(!defaults_get_boolean(
				"slingshot.batching",
				"SlingShot.Batching",
				TRUE))
			no_pixmap_batching = TRUE;

		initialized = TRUE;
	}

	return(XV_OK);
}


Pkg_private Xv_opaque
canvas_shell_set_avlist(canvas_shell, avlist_arg)
	Canvas_shell	canvas_shell;
	Attr_avlist	avlist_arg;
{
	Canvas_shell_info *csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);
	register Canvas_shell_attr attr;
	Attr_attribute	avlist_array[ATTR_STANDARD_SIZE];
	Attr_avlist	avlist;
	extern	int	rectobj_global_invocation_level;

	/*
	 * 1) Copy the attribute list.
	 * 2) Consume attributes that the rectobj shouldn't see.
	 * 3) Call the rectobj set function.
	 */
	
	/*
	 * Find the end of the list (using lame algorithm because
	 * XView doesn't specify a list length)
	 */
	avlist = avlist_arg;
	while (attr = (Canvas_shell_attr) * avlist) {
		if(attr == CANVAS_RESIZE_PROC &&
		  (Proc_ptr)*(avlist+1) != canvas_shell_resize_proc) {
			ATTR_CONSUME(*avlist);
			csinfo->resize_proc = (Proc_ptr)*(avlist+1);
		}
		avlist = attr_next(avlist);
	}

	memcpy( (char*)avlist_array, (char*)avlist_arg, 
		(int)avlist - (int)avlist_arg + sizeof attr);
	
	avlist = avlist_array;
	while (attr = (Canvas_shell_attr) * avlist) {
	  switch(attr) {

	  case WIN_CMS:
		ATTR_CONSUME(*avlist);
		canvas_shell_update_color(canvas_shell, 
			&csinfo->shared_info, (Cms) *(avlist+1));
		avlist = attr_next(avlist);
		break;

	  case CANVAS_SHELL_FONT:
		ATTR_CONSUME(*avlist);
		/* 
		 * This should only be a create-only attr.  It is
		 * be settable if there are no objects in the shell.
		 *
		 * The font should never be NULL.  If so, use
		 * that from the window.
		 */
		csinfo->shared_info.font = ( *(avlist+1) ?
			(Xv_opaque) *(avlist+1) :
			(Xv_opaque) xv_get(canvas_shell, XV_FONT));
		csinfo->shared_info.font_info = (XFontStruct*) 
			xv_get(csinfo->shared_info.font, FONT_INFO);
		avlist = attr_next(avlist);
		break;

	  case CANVAS_SHELL_EVENT_PROC:
		ATTR_CONSUME(*avlist);
		csinfo->misc_event_proc = (int(*)()) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case CANVAS_SHELL_AUTO_DROP_SITE:
		ATTR_CONSUME(*avlist);
		csinfo->auto_drop_site = (char) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case WIN_FOREGROUND_COLOR:
	  case RECTOBJ_FG:
		ATTR_CONSUME(*avlist);
		csinfo->shared_info.win_fg = (short) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case WIN_BACKGROUND_COLOR:
	  case RECTOBJ_BG:
		ATTR_CONSUME(*avlist);
		csinfo->shared_info.win_bg = (short) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case RECTOBJ_BG2:
		ATTR_CONSUME(*avlist);
		if((short)*(avlist+1) < csinfo->shared_info.num_colors)
			csinfo->shared_info.bg2 = (short) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case RECTOBJ_BG3:
		ATTR_CONSUME(*avlist);
		if((short)*(avlist+1) < csinfo->shared_info.num_colors)
			csinfo->shared_info.bg3 = (short) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case RECTOBJ_WHITE:
		ATTR_CONSUME(*avlist);
		if((short)*(avlist+1) < csinfo->shared_info.num_colors)
			csinfo->shared_info.white = (short) *(avlist+1);
		avlist = attr_next(avlist);
		break;

	  case RECTOBJ_PARENT:
	  case XV_OWNER:
		ATTR_CONSUME(*avlist);
		avlist = attr_next(avlist);
		break;

	  case CANVAS_SHELL_DELAY_REPAINT:
		ATTR_CONSUME(*avlist);
		rectobj_set_delay_repaint(canvas_shell, 
			*(avlist+1));
		avlist = attr_next(avlist);
		break;

	  case CANVAS_SHELL_BATCH_REPAINT:
		ATTR_CONSUME(*avlist);
		if(!no_pixmap_batching) {
			csinfo->batch_repaint = (char)*(avlist+1);
			canvas_shell_alloc_batch_pixmap(canvas_shell, csinfo,
				xv_get(canvas_shell, XV_WIDTH), 
				xv_get(canvas_shell, XV_HEIGHT), 
				xv_get(canvas_shell, WIN_DEPTH)); 
		}
		avlist = attr_next(avlist);
		break;

	  case XV_END_CREATE:
		ATTR_CONSUME(*avlist);
		/* 
		 * Duplicate what rectobj-set-avlist would do, but
		 * without the rectobj-add_to_parent-list, because
		 * the parent may not be a rectobj.
		 *
		 * Also, xv_get the height because the resize proc
		 * is not called on init.
		 */
		RF_UNSET(&csinfo->rectobj_info, RF_STATE_INIT);
		RF_SET(&csinfo->rectobj_info, 
			(RF_STATE_CREATED|RF_REPAINT|RF_CLEAR));
		csinfo->rectobj_info.invocation_level = 1;
		csinfo->rectobj_info.rect.r_width = 
				(short) xv_get(canvas_shell, XV_WIDTH);
		csinfo->rectobj_info.rect.r_height = 
				(short) xv_get(canvas_shell, XV_HEIGHT);
		if(csinfo->auto_drop_site) {
			canvas_shell_create_drop_site(xv_get(canvas_shell, 
				CANVAS_NTH_PAINT_WINDOW, 0));
			canvas_shell_set_drop_region(canvas_shell, 
				&csinfo->rectobj_info.rect);
		}
		break;

	  default:
		if(ATTR_PKG(attr) != ATTR_RECTOBJ)
			ATTR_CONSUME(*avlist);
		avlist = attr_next(avlist);
		break;
	  }
	}

	rectobj_set_avlist(canvas_shell, (Attr_avlist) avlist_array);

	if(rectobj_finish_set1(canvas_shell))
		rectobj_finish_set2(canvas_shell);

	return XV_OK;
}


/*ARGSUSED*/
Pkg_private Xv_opaque
canvas_shell_get_attr(canvas_shell, status, which_attr, avlist)
	Canvas_shell	canvas_shell;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Canvas_shell_info *csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);

	switch(which_attr) {
	  case RECTOBJ_PARENT:
		return (Xv_opaque) 0;

	  case CANVAS_SHELL_DELAY_REPAINT:
		return (Xv_opaque) csinfo->delay_repaint;

	  case CANVAS_SHELL_BATCH_REPAINT:
		return (Xv_opaque) csinfo->batch_repaint;

	  case CANVAS_SHELL_AUTO_DROP_SITE:
		return (Xv_opaque) csinfo->auto_drop_site;

	  case RECTOBJ_FG:
		return (Xv_opaque) csinfo->shared_info.win_fg;

	  case RECTOBJ_BG:
		return (Xv_opaque) csinfo->shared_info.win_bg;

	  case RECTOBJ_BG2:
		return (Xv_opaque) csinfo->shared_info.bg2;

	  case RECTOBJ_BG3:
		return (Xv_opaque) csinfo->shared_info.bg3;

	  case RECTOBJ_WHITE:
		return (Xv_opaque) csinfo->shared_info.white;

	  case CANVAS_SHELL_FONT:
		return (Xv_opaque) csinfo->shared_info.font;

	  case CANVAS_SHELL_EVENT_PROC:
		return (Xv_opaque) csinfo->misc_event_proc;

	  case CANVAS_RESIZE_PROC:
		return (Xv_opaque) csinfo->resize_proc;

	  default:
		break;
	}
	if(ATTR_PKG(which_attr) == ATTR_RECTOBJ)
		  return( rectobj_get_attr(canvas_shell, 
				status, which_attr, avlist) );

	*status = XV_ERROR;
	return (Xv_opaque) 0;
}


/*ARGSUSED*/
Pkg_private int
canvas_shell_destroy(canvas_shell, status)
	Canvas_shell	canvas_shell;
	Destroy_status	status;
{
	Canvas_shell_info *csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	canvas_shells = list_unlink_node(&csinfo->listnode);

	rectobj_destroy(canvas_shell, status);
	/* no need to free csinfo, it is freed by the rectobj */

	return XV_OK;
}


/*ARGSUSED*/
void
canvas_shell_split_proc(origview, newview, pos)
	Xv_window origview, newview;
	int pos;
{
	Xv_window		paint_window;
	Canvas_shell		canvas_shell;

	paint_window = (Xv_window) xv_get(newview, 
			CANVAS_VIEW_PAINT_WINDOW);

	canvas_shell = (Canvas_shell) xv_get(paint_window, 
			CANVAS_PAINT_CANVAS_WINDOW);

	xv_set(paint_window,
		WIN_CONSUME_EVENTS,
			WIN_UP_EVENTS, 
			WIN_MOUSE_BUTTONS,
			LOC_DRAG,
			MS_LEFT,
			ACTION_HELP,
			NULL,
		WIN_EVENT_PROC, canvas_shell_event_proc,
		NULL);


	if( CANVAS_SHELL_PRIVATE(canvas_shell) ->auto_drop_site )
	canvas_shell_create_drop_site(paint_window);
}


/*ARGSUSED*/
void
canvas_shell_repaint_proc(canvas_shell, paint_window, dpy, win, xrects)
	Canvas_shell	canvas_shell;
	Xv_Window	paint_window;	/* unused */
	Display		*dpy;
	Window		win;
	Xv_xrectlist	*xrects;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(canvas_shell);

	/*
	 * Painted flag is initially set to false so that painting
	 * doesn't occur before window is mapped.  If this is called
	 * then painting is initiated, make sure that the flag is set.
	 */
	RF_SET(rinfo, RF_PAINTED);
	(rinfo->rectobj_ops->paint_proc) (canvas_shell, dpy, win, xrects);
}


void
canvas_shell_event_proc(paint_window, event, arg)
	Xv_window	paint_window;
	Event		*event;
	Notify_arg	arg;
{
	Canvas_shell		canvas_shell;
	Canvas_shell_info	*csinfo;
	Rectobj			rectobj;
	Rectobj_info		*rinfo;
	static void		rectobj_show_rects();
	
	canvas_shell = (Canvas_shell) xv_get(paint_window, CANVAS_PAINT_CANVAS_WINDOW);
	csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);

	csinfo->delay_repaint++;
	if(csinfo->grab_event_proc) {
		(csinfo->grab_event_proc) (paint_window, event, canvas_shell, 
					csinfo->grab_arg);
		rectobj_set_delay_repaint(canvas_shell, FALSE);
		return;
	}

	if(csinfo->misc_event_proc)
		if((csinfo->misc_event_proc)(paint_window, event, canvas_shell))
			return;


	if(event_shift_is_down(event) && 
	   event_ctrl_is_down(event)  &&
	   event_action(event) == ACTION_SELECT) {
		rectobj_set_delay_repaint(canvas_shell, FALSE);
		rectobj_show_rects(paint_window, event, canvas_shell, 
			&csinfo->shared_info);
		return;
	}

	rectobj = event_to_rectobj(canvas_shell, event);
	if(!rectobj) {
		rectobj_set_delay_repaint(canvas_shell, FALSE);
		return;
	}

	switch(event_action(event)) {
		case ACTION_DRAG_PREVIEW:
		case ACTION_DRAG_COPY:
		case ACTION_DRAG_MOVE:
		case ACTION_DRAG_LOAD:
			rectobj_process_drop_event(canvas_shell, event, 
					canvas_shell, rectobj);
			rectobj_set_delay_repaint(canvas_shell, FALSE);
			return;

		case ACTION_HELP:
			rectobj_help_show(paint_window, event, rectobj);
			rectobj_set_delay_repaint(canvas_shell, FALSE);
			return;
	}

	rinfo = RECTOBJ_PRIVATE(rectobj);
	if(rinfo->rectobj_ops->event_proc)
		(rinfo->rectobj_ops->event_proc) 
			(paint_window, event, canvas_shell, rectobj);
	rectobj_set_delay_repaint(canvas_shell, FALSE);
}


void
canvas_shell_canvas_event_proc(canvas_shell, event, arg)
	Canvas_shell	canvas_shell;
	Event		*event;
	Notify_arg	arg;
{
	extern void	rectobj_lose_selection();

	if(event_action(event) == SEL_CLEAR) {
		/*
		 * Catch the selection clear action so the application 
		 * doesn't need to handle it in it's sel_lose_proc.
		 */
		XSelectionClearEvent	*sce;
		sce = (XSelectionClearEvent*) event_xevent(event);

		/*
		 * This check is necessary so that other selections,
		 * such as clipboard and secondary, don't reset
		 * primary selections.
		 */
		if((sce->type == SelectionClear) &&
		   (sce->selection == XA_PRIMARY)) {
			rectobj_lose_selection();
			rectobj_flush_repaint(FALSE);
		}
	} else 
	if(event_id(event) == WIN_MAP_NOTIFY) {
		Canvas_shell_info *csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);
		csinfo->repaint_rect.r_width = 0;
		csinfo->repaint_rect.r_height = 0;
		csinfo->win_mapped = TRUE;
	}
}


Rectobj
event_to_rectobj(canvas_shell, event)
	Rectobj	canvas_shell;
	Event	*event;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(canvas_shell);

	return( (rinfo->rectobj_ops->map_event_proc)(canvas_shell, event) );
}


/*
 * For right now, this is really dumb.  It maintains one rect that contains
 * the union of all areas that need repainting.  If it proves that something
 * more sophisticated is needed, this will be replaced transparently.
 */
void
rectobj_repaint_rect(rectobj, rect, clear)
	Rectobj		rectobj;
	Rect		*rect;
	int		clear;
{
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(rectobj);
	Canvas_shell_info	*csinfo;
 
	if(!rinfo->shared_info)
		return;

	csinfo = CANVAS_SHELL_PRIVATE(rinfo->shared_info->canvas_shell);

	if(!rect)
		rect = &rinfo->rect;

	if(!(RF_IS_SET(rinfo, RF_PAINTED)))
		return;

	if((rect->r_width == 0) || (rect->r_height == 0))
		return;

	if(clear)
		csinfo->repaint_clear = TRUE;

	csinfo->repaint_rect = rect_bounding( &csinfo->repaint_rect, rect );

#ifdef REPAINT_DEBUG
	{
	GC gc;

	gc = XCreateGC(rinfo->shared_info->dpy, 
		xv_get(rinfo->shared_info->canvas_shell, XV_XID),
		0, 0);

	/* batching must be turned off */
	xv_set(rinfo->shared_info->canvas_shell,CANVAS_SHELL_BATCH_REPAINT,0,0);

	XSetForeground(rinfo->shared_info->dpy, gc, 1);
	XFillRectangle(rinfo->shared_info->dpy,
		xv_get(xv_get(rinfo->shared_info->canvas_shell, 
			CANVAS_NTH_PAINT_WINDOW, 0), XV_XID),
		gc,
		rect->r_left, rect->r_top,
		rect->r_width-1, rect->r_height-1);
	XFreeGC(rinfo->shared_info->dpy, gc);
	XFlush(rinfo->shared_info->dpy);
	}
#endif
	
	return;
}
 

void
rectobj_flush_repaint(force)
	int force;
{
	Xv_window		xv_win;
	Xv_xrectlist		xrects;
	Canvas_shell_info	*csinfo;
	Listnode		*node;

	node = canvas_shells;

#ifdef REPAINT_DEBUG
	usleep(3000);
#endif 

	list_for(node) {

	  csinfo = (Canvas_shell_info*) list_handle(node);

	  if((csinfo->repaint_rect.r_width == 0) ||
	    (csinfo->repaint_rect.r_height == 0) ||
	    (csinfo->win_mapped == FALSE)	 ||
	    (!force && csinfo->delay_repaint))
		continue;

	  xrects.count = 1;
	  /* different structs, but equivalent */
	  xrects.rect_array[0] = *(XRectangle*) &csinfo->repaint_rect;

	/* isn't repaint_clear almost always set? -- it may be worthless */

	  if(csinfo->batch_pixmap) {
	    GC gc;

	    gc = XCreateGC(csinfo->shared_info.dpy, csinfo->batch_pixmap, 0, 0);
	    XSetForeground(csinfo->shared_info.dpy, gc, 
	      csinfo->shared_info.pixels[ xv_get(
		csinfo->shared_info.canvas_shell, WIN_BACKGROUND_COLOR)]);

	    /* turn off no expose events - useless when copying from pixmap. */
	    XSetGraphicsExposures(csinfo->shared_info.dpy, gc, False);

	    if(csinfo->repaint_clear) 
		XFillRectangle(csinfo->shared_info.dpy, 
			csinfo->batch_pixmap,
			gc,
			csinfo->repaint_rect.r_left,
			csinfo->repaint_rect.r_top,
			csinfo->repaint_rect.r_width,
			csinfo->repaint_rect.r_height);

	    (csinfo->rectobj_info.rectobj_ops->paint_proc) 
			(csinfo->shared_info.canvas_shell, 
			csinfo->shared_info.dpy, 
			csinfo->batch_pixmap,
			&xrects);

	    CANVAS_EACH_PAINT_WINDOW(csinfo->shared_info.canvas_shell, xv_win)
		XCopyArea(csinfo->shared_info.dpy,
			csinfo->batch_pixmap,
			xv_get(xv_win, XV_XID), 
			gc,
			csinfo->repaint_rect.r_left,
			csinfo->repaint_rect.r_top,
			csinfo->repaint_rect.r_width,
			csinfo->repaint_rect.r_height,
			csinfo->repaint_rect.r_left,
			csinfo->repaint_rect.r_top);
	    CANVAS_END_EACH
	    XFreeGC(csinfo->shared_info.dpy, gc);

	  } else {
	    CANVAS_EACH_PAINT_WINDOW(csinfo->shared_info.canvas_shell, xv_win)

		if(csinfo->repaint_clear) 
			XClearArea(csinfo->shared_info.dpy, 
				xv_get(xv_win, XV_XID),
				csinfo->repaint_rect.r_left,
				csinfo->repaint_rect.r_top,
				csinfo->repaint_rect.r_width,
				csinfo->repaint_rect.r_height,
				FALSE);

		(csinfo->rectobj_info.rectobj_ops->paint_proc) 
			(csinfo->shared_info.canvas_shell, 
			csinfo->shared_info.dpy, 
			xv_get(xv_win, XV_XID), 
			&xrects);

	    CANVAS_END_EACH
	  }

	  csinfo->repaint_clear = FALSE;
	  csinfo->repaint_rect.r_width = 0;
	  csinfo->repaint_rect.r_height = 0;
	}
}


void
rectobj_invalidate_repaint(rectobj, rect)
	Rectobj rectobj;
	Rect	*rect;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Canvas_shell_info *csinfo;

	/* Cancel the repaint, use only if you know what you're doing */
 
	if(!rinfo->shared_info) {
		if(rect)
			rect->r_left = rect->r_top =
			rect->r_width = rect->r_height = 0;
		return;
	}

	csinfo = CANVAS_SHELL_PRIVATE(rinfo->shared_info->canvas_shell);
	if(rect)
		*rect = csinfo->repaint_rect;
	csinfo->repaint_rect.r_width = 0;
	csinfo->repaint_rect.r_height = 0;
  	csinfo->repaint_clear = FALSE;

}


void
rectobj_set_delay_repaint(rectobj, flag)
	Rectobj	rectobj;
	int	flag;
{
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(rectobj);
	Canvas_shell_info	*csinfo;
	extern int 		rectobj_global_invocation_level;

	if(!rinfo->shared_info)
		return;

	csinfo = CANVAS_SHELL_PRIVATE(rinfo->shared_info->canvas_shell);

	if(flag)
		csinfo->delay_repaint++;
	else
		csinfo->delay_repaint--;

	if((!csinfo->delay_repaint) && (!rectobj_global_invocation_level))
		rectobj_flush_repaint(FALSE);
}
 

static void
rectobj_paint_outlines(rectobj, dpy, win, gc)
	Rectobj		rectobj;
	Display		*dpy;
	Window		win;
	GC		gc;
{
	Rectobj_list	*list;
	Rectobj		child;
	Rectobj_info	*rinfo;

	list = (Rectobj_list*)xv_get(rectobj, RECTOBJ_CHILDREN);
	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		rinfo = RECTOBJ_PRIVATE(child);
		XDrawRectangle(dpy, win, gc,
			rinfo->rect.r_left,
			rinfo->rect.r_top,
			MAX((short) rinfo->rect.r_width-1, 0),
			MAX((short) rinfo->rect.r_height-1, 0));
		rectobj_paint_outlines(child, dpy, win, gc);
	}
}        

 
static void
canshell_wait_select_up(paint_window, event, canvas_shell, grab_arg)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	void		*grab_arg;
{
	Rect r;

	if((!event_is_up(event)) || (event_action(event) != ACTION_SELECT))
		return;

	rectobj_set_event_grab(canvas_shell, canvas_shell, 0, 0);

	r.r_left = r.r_top = 0; r.r_width = 30000; r.r_height = 30000;
	rectobj_repaint_rect(canvas_shell, &r, TRUE);
	rectobj_flush_repaint(FALSE);
}


static void
rectobj_show_rects(paint_window, event, canvas_shell, sh_info)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Shared_info	*sh_info;
{
	Xv_window	xv_win;
	GC		gc;
	static char	dashes[2] = {2, 1};
	Inputmask	im;

	gc = XCreateGC(sh_info->dpy, xv_get(canvas_shell, XV_XID), 0, 0);
	XSetForeground(sh_info->dpy, gc, pixel_fg(sh_info, -1));
	XSetBackground(sh_info->dpy, gc, pixel_bg(sh_info, -1));
	XSetLineAttributes(sh_info->dpy, gc, 0, 
			LineDoubleDash, CapButt, JoinBevel);
	XSetDashes(sh_info->dpy, gc, 0, dashes, 2);

	CANVAS_EACH_PAINT_WINDOW(canvas_shell, xv_win)
		rectobj_paint_outlines(canvas_shell,
			sh_info->dpy,
			xv_get(xv_win, XV_XID),
			gc);
	CANVAS_END_EACH

	XFreeGC(sh_info->dpy, gc);
	XFlush(sh_info->dpy);
	
	rectobj_set_event_grab(canvas_shell, canvas_shell, 
		canshell_wait_select_up, 0);
}


static	int	alloc_pixmap_error;
static	int	(*prev_handler)();

static int
alloc_pixmap_error_handler(dpy, err)
	Display *dpy;
	XErrorEvent *err;
{
	/*
	 * Handle the BadAlloc error gracefully.  Things will continue,
	 * albeit less smoothly.
	 */

	if(err->request_code == X_CreatePixmap)
		alloc_pixmap_error = TRUE;
	else
		(prev_handler)(dpy, err);
	return (0); /* Xlib ignores return value */
}

static void
canvas_shell_alloc_batch_pixmap(canvas_shell, csinfo, width, height)
	Canvas_shell		canvas_shell;
	Canvas_shell_info	*csinfo;
	int			width;
	int			height;
{
	/* 
	 * Pixmaps aren't resizable, so they have to be destroyed and
	 * recreated when the size changes.
	 */

	if(no_pixmap_batching)
		return;

	if(csinfo->batch_pixmap) {
		XFreePixmap(csinfo->shared_info.dpy, csinfo->batch_pixmap);
		csinfo->batch_pixmap = NULL;
	}

	if(!csinfo->batch_repaint)
		return;

	prev_handler = XSetErrorHandler(alloc_pixmap_error_handler);
	alloc_pixmap_error = FALSE;
	csinfo->batch_pixmap = XCreatePixmap(csinfo->shared_info.dpy, 
			xv_get(canvas_shell, XV_XID), 
			width, height,
			xv_get(canvas_shell, WIN_DEPTH)); 
	XSync(csinfo->shared_info.dpy, FALSE);
	/*
	 * Must detect errors by setting a flag because when in sync mode, 
	 * the return value will be a non-valid resource id and assigned after 
	 * the error function is called.  In async mode, this order is reversed.
	 */
	if(alloc_pixmap_error)
		csinfo->batch_pixmap = NULL;
	(void) XSetErrorHandler(prev_handler);
}


void
canvas_shell_resize_proc(canvas_shell, width, height)
	Canvas_shell	canvas_shell;
	int		width;
	int		height;
{
	static int		recursion_guard;
	Canvas_shell_info	*csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(canvas_shell);
	Xv_window		paint_window;
	Xv_drop_site		drop_site;


	if(recursion_guard == TRUE)
		return;
	csinfo->delay_repaint++;
	recursion_guard = TRUE;
	if(csinfo->resize_proc)
		(csinfo->resize_proc)(canvas_shell, width, height);
	recursion_guard = FALSE;
	rinfo->rect.r_width = width;
	rinfo->rect.r_height = height;

	canvas_shell_alloc_batch_pixmap(canvas_shell, csinfo, width, height);
	if(csinfo->auto_drop_site)
		canvas_shell_set_drop_region(canvas_shell, &rinfo->rect);

	/* wait for xview to repaint after resize */
	rectobj_invalidate_repaint(canvas_shell, NULL);

        rectobj_set_delay_repaint(canvas_shell, FALSE);
}


void
canvas_shell_geometry_manage_proc(parent, child, child_new_rect, child_old_rect)
	Rectobj	parent;
	Rectobj	child;
	Rect	*child_new_rect;
	Rect	*child_old_rect;
{
	rectobj_set_geometry(child, child_new_rect);
}


static void
canvas_shell_update_color(canvas_shell, sh_info, cms)
	Canvas_shell	canvas_shell;
	Shared_info	*sh_info;
	Cms		cms;
{
	if(cms == (Cms) NULL)
		cms = (Cms) xv_get(canvas_shell, WIN_CMS);
	sh_info->cms = (Xv_opaque) cms;
	sh_info->num_colors = xv_get(cms, CMS_SIZE);
	sh_info->pixels = (unsigned long*) xv_get(cms, CMS_INDEX_TABLE);
	sh_info->control_cms = (int) xv_get(cms, CMS_CONTROL_CMS);
	if(sh_info->control_cms) {
		sh_info->win_bg = 0;
		sh_info->win_fg = sh_info->num_colors-1;
		sh_info->bg2 = 1;
		sh_info->bg3 = 2;
		sh_info->white = 3;
	} else {
		sh_info->win_fg = (short) 
			xv_get(canvas_shell, WIN_FOREGROUND_COLOR);
		sh_info->win_bg = (short) 
			xv_get(canvas_shell, WIN_BACKGROUND_COLOR);
		sh_info->bg2 = sh_info->win_bg;
		sh_info->bg3 = sh_info->win_fg;
		sh_info->white = sh_info->win_fg;
	}
}


void
rectobj_set_event_grab(canvas_shell, grab_rectobj, grab_event_proc, grab_arg)
	Canvas_shell	canvas_shell;
	Rectobj		grab_rectobj;
	Proc_ptr	grab_event_proc;
	void		*grab_arg;
{
	Canvas_shell_info	*csinfo = CANVAS_SHELL_PRIVATE(canvas_shell);
	Rectobj_info		*rinfo;

	csinfo->grab_event_proc = grab_event_proc;
	csinfo->grab_arg = grab_arg;
	csinfo->grab_rectobj = grab_rectobj;

	if(!grab_rectobj)
		return;

	if(grab_event_proc)
		RF_SET( RECTOBJ_PRIVATE(grab_rectobj), RF_HAS_EVENT_GRAB);
	else
		RF_UNSET( RECTOBJ_PRIVATE(grab_rectobj), RF_HAS_EVENT_GRAB);
}


static void
canvas_shell_set_drop_region(canvas_shell, r)
	Canvas_shell	canvas_shell;
	Rect		*r;
{
	Xv_drop_site	drop_site;
	Xv_window	paint_window;
	

	CANVAS_EACH_PAINT_WINDOW(canvas_shell, paint_window)

		drop_site = (Xv_drop_site) xv_get(paint_window, 
				CANVAS_SHELL_DROP_SITE_KEY);

		if(drop_site)
		  xv_set(drop_site,
			DROP_SITE_DELETE_REGION_PTR, NULL,
			DROP_SITE_REGION, r,
			NULL);

	CANVAS_END_EACH
}


static void
canvas_shell_create_drop_site(paint_window)
	Xv_window	paint_window;
{
	Xv_drop_site		drop_site;

	drop_site = (Xv_drop_site) xv_create(paint_window, DROP_SITE_ITEM,
		DROP_SITE_ID, paint_window,
		DROP_SITE_REGION, xv_get(paint_window, XV_RECT),
		DROP_SITE_EVENT_MASK, (DND_ENTERLEAVE|DND_MOTION),
		NULL);
	xv_set(paint_window,
		CANVAS_SHELL_DROP_SITE_KEY, drop_site,
		NULL);
}

