/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)drawtext.c 1.36 92/11/11";
#endif
#endif

#include <X11/Xlib.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/win_input.h>
#include <xview/font.h>
#include <xview/panel.h>
#include <string.h>
#include <sspkg/drawobj.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include <sspkg/canshell.h>
#include "dtext_impl.h"


	void	drawtext_paint_proc();
	void	drawtext_edit_single_click();
	void	drawtext_set_geometry_proc();

/*ARGSUSED*/
Pkg_private int
drawtext_init(parent, drawtext, avlist)
	Xv_opaque	parent;
	Drawtext	drawtext;
	Attr_avlist	avlist;
{
	Drawtext_info	*dinfo;
	Rectobj_info 	*rinfo = RECTOBJ_PRIVATE(drawtext);
	Drawtext_struct	*drawtext_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		drawtext_paint_proc,
		rectobj_selection_event_proc,
		rectobj_map_event_proc,
		drawtext_set_geometry_proc,
		NULL,			/* manage child */
		NULL,			/* add child */
		NULL,			/* del child */
		NULL,			/* new parent */
		NULL,			/* start drag */
		NULL,			/* style change */
		drawtext_edit_single_click,
	};

	if(drawicon_private_dtinfo)
		dinfo = drawicon_private_dtinfo; /* created by drawicon */
	else {
		dinfo = xv_alloc(Drawtext_info);
		dinfo->justify = DRAWTEXT_JUSTIFY_LEFT;
	}

	drawtext_object = (Drawtext_struct*) drawtext;
	drawtext_object->private_data = (Xv_opaque) dinfo;

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;
	RF_SET(rinfo, RF_SELECTABLE);

	return(XV_OK);
}


Pkg_private Xv_opaque
drawtext_set_avlist(drawtext, avlist)
	Drawtext		drawtext;
	register Attr_avlist	avlist;
{
        register Drawobj_attr attr;
        register Drawtext_info	*dinfo = DRAWTEXT_PRIVATE(drawtext);
	register Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawtext);
	int	calc_rect = FALSE;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
		    xv_super_set_avlist(drawtext, &drawtext_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(drawtext);
			return(set_result);
		}
	}

	while (attr = (Drawobj_attr) * avlist++)
	  switch (attr) {

		case DRAWTEXT_STRING:
		case DRAWTEXT_STRING_PTR:
		case DRAWTEXT_FONT:
		case DRAWTEXT_JUSTIFY:
		case DRAWTEXT_SHOW_UNDERLINE:
		case DRAWTEXT_LENGTH:
		case DRAWTEXT_EDITABLE:
		case XV_WIDTH:
		case XV_HEIGHT:
			drawtext_set_attr(dinfo, rinfo, attr, (void*)*avlist++);
			RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			calc_rect = TRUE;
			break;

		case XV_OWNER:
		case RECTOBJ_PARENT:
			if(rinfo->shared_info)
			    drawtext_set_attr(dinfo, rinfo, 
				DRAWTEXT_FONT, (void*)rinfo->shared_info->font);
			avlist++;
			calc_rect = TRUE;
			break;

		case XV_END_CREATE:
			if(!dinfo->font_info && rinfo->shared_info)
				dinfo->font_info= rinfo->shared_info->font_info;
	   		calc_rect = TRUE; /* force calculation of the rect */
			break;

		default:
			avlist = attr_skip(attr, avlist);

	  }

	if(calc_rect)
		drawtext_calc_rect(drawtext);

	if(rectobj_finish_set1(drawtext))
		rectobj_finish_set2(drawtext);

	return(XV_SET_DONE);
}


void
drawtext_set_attr(dinfo, rinfo, attr, value)
	Drawtext_info	*dinfo;
	Rectobj_info	*rinfo;
	Attr_attribute	attr;
	void		*value;
{

	switch(attr) {
		case DRAWTEXT_STRING:
			if(dinfo->free_string && dinfo->string)
				free(dinfo->string);
			if(value && *(char *) value) {
				dinfo->string = strdup((char*) value);
				dinfo->strlen = strlen(dinfo->string);
				dinfo->free_string = TRUE;
			} else {
				dinfo->string = NULL;
				dinfo->strlen = 0;
				dinfo->free_string = FALSE;
			}
			break;

		
		case DRAWTEXT_STRING_PTR:
			dinfo->free_string = FALSE;
			if(value && *(char *) value) {
				dinfo->string = (char*) value;
				dinfo->strlen = strlen(dinfo->string);
			} else {
				dinfo->string = NULL;
				dinfo->strlen = 0;
			}
			break;

		case DRAWTEXT_FONT:
			dinfo->font = (Xv_font)value;
			if(dinfo->font) {
				dinfo->font_info = (XFontStruct*)
					xv_get(dinfo->font, FONT_INFO);
			} else {
			  /*
			   * If setting it to NULL, use canvas_shell's
			   * font (if there is a canvas_shell).
			   */
			  if(rinfo && rinfo->shared_info) {
				dinfo->font_info=rinfo->shared_info->font_info;
			  } else {
				dinfo->font_info = NULL;
			  }
			}
			break;

		case DRAWTEXT_JUSTIFY:
			dinfo->justify = (Drawtext_justify_style) value;
			break;

		case DRAWTEXT_EDITABLE:
			if(value)
				dinfo->editable = TRUE;
			else
				dinfo->editable = FALSE;
			break;

		case DRAWTEXT_SHOW_UNDERLINE:
			if(value)
				dinfo->show_underline = TRUE;
			else
				dinfo->show_underline = FALSE;
			break;

		case DRAWTEXT_LENGTH:
			dinfo->disp_length = (short) value;
			break;

		case XV_WIDTH:
			dinfo->set_width = (short) value;
			break;

		case XV_HEIGHT:
			dinfo->set_height = (short) value;
			break;
	}
}


void
drawtext_calc_rect(drawtext)
	Drawtext	drawtext;
{
	register Drawtext_info  *dinfo = DRAWTEXT_PRIVATE(drawtext);
	register Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawtext);

	/*
	 * set_width and set_height fields are the values set by the application
	 *   this establishes the min size for the object.
	 * min_width and min_height are calculated size.
	 * text_width is measured length of string  (string is centered with)
	 */

	if(dinfo->font_info) {

	  rinfo->min_height = 
		dinfo->font_info->ascent + dinfo->font_info->descent;
	  if(dinfo->editable || dinfo->show_underline)
		rinfo->min_height += 2; /* space for the underline */

	  if(rinfo->shared_info)
		/* sneak a function from the panel */
		  rinfo->min_width = panel_col_to_x(
			dinfo->font ? dinfo->font : rinfo->shared_info->font,
			dinfo->disp_length);
	  else
		/* this doesn't work as expected for some reason, but still
		 * punt when there is no font handle
		 */
	  	rinfo->min_width = 
			dinfo->font_info->max_bounds.width * dinfo->disp_length;

	  if(dinfo->string) {
		dinfo->text_width = 
			(short) XTextWidth(dinfo->font_info, dinfo->string, 
						dinfo->strlen);
		rinfo->min_width = MAX(rinfo->min_width, 
			(unsigned short) dinfo->text_width);
	  } else
		dinfo->text_width = 0;

	  rinfo->rect.r_width = 
		MAX(rinfo->min_width, (unsigned short) dinfo->set_width);

	  rinfo->rect.r_height= 
		MAX(rinfo->min_height, (unsigned short) dinfo->set_height);
	}
}


/*ARGSUSED*/
Pkg_private Xv_opaque
drawtext_get_attr(drawtext, status, which_attr, avlist)
	Drawtext		drawtext;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Drawtext_info  *dinfo = DRAWTEXT_PRIVATE(drawtext);

	switch (which_attr) {

		case DRAWTEXT_STRING:
		case DRAWTEXT_STRING_PTR:
			return (Xv_opaque) dinfo->string;

		case DRAWTEXT_FONT:
			return (Xv_opaque) dinfo->font;

		case DRAWTEXT_JUSTIFY:
			return (Xv_opaque) dinfo->justify;

		case DRAWTEXT_EDITABLE:
			return (Xv_opaque) dinfo->editable;

		case DRAWTEXT_SHOW_UNDERLINE:
			return (Xv_opaque) dinfo->show_underline;

		case DRAWTEXT_LENGTH:
			return (Xv_opaque) dinfo->disp_length;

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}


/*ARGSUSED*/
Pkg_private int
drawtext_destroy(drawtext, status)
	Drawtext		drawtext;
	Destroy_status	status;
{
	Drawtext_info	*dinfo = DRAWTEXT_PRIVATE(drawtext);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawtext);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	/*
	 * have to do this before the free so callbacks can
	 * xv_get string if necessary.
	 */
	rectobj_del_from_selected_list(drawtext, NULL);

	if(rinfo->shared_info && rinfo->shared_info->focus_drawtext == drawtext)
		drawtext_finish_edit(drawtext, FALSE);

	if(dinfo->free_string && dinfo->string)
		free(dinfo->string);
	free(dinfo);
	return XV_OK;
}


/*ARGSUSED*/
Pkg_private void
drawtext_paint_proc(drawtext, dpy, win, xrects)
        Drawtext drawtext;
        Display *dpy;
        Window  win;
        Xv_xrectlist *xrects;
{
	Drawtext_info *dinfo = DRAWTEXT_PRIVATE(drawtext);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawtext);
	GC		gc;
	register int	x, y;

	gc = XCreateGC(dpy, win, 0, 0);

	/* assert (rinfo->shared_info); */
	/* assert (dinfo->font_info); */

	if(!dinfo->font_info)
		return;

	XSetFont(dpy, gc, dinfo->font_info->fid);

	if(xrects && xrects->count)
		XSetClipRectangles(dpy, gc,
				0, 0,
				xrects->rect_array,
				xrects->count,
				Unsorted);

	if(HIGHLIGHT_RECTOBJ(rinfo)) {
		XSetForeground(dpy, gc, 
			pixel_fg(rinfo->shared_info, rinfo->fg_color));

		/*
		 * Use XFillRectangle because the text may be smaller than 
		 * the rect of the object.
		 */
		XFillRectangle(dpy, win, gc,
			rinfo->rect.r_left,
 			rinfo->rect.r_top,
 			rinfo->rect.r_width,
 			rinfo->rect.r_height);

		XSetForeground(dpy, gc, 
			pixel_bg(rinfo->shared_info, rinfo->bg_color));

		XSetBackground(dpy, gc, 
			pixel_fg(rinfo->shared_info, rinfo->fg_color));
	} else {
		XSetForeground(dpy, gc, 
			pixel_bg(rinfo->shared_info, rinfo->bg_color));

		XFillRectangle(dpy, win, gc,
			rinfo->rect.r_left,
 			rinfo->rect.r_top,
 			rinfo->rect.r_width,
 			rinfo->rect.r_height);

		XSetForeground(dpy, gc, 
			pixel_fg(rinfo->shared_info, rinfo->fg_color));

		XSetBackground(dpy, gc, 
			pixel_bg(rinfo->shared_info, rinfo->bg_color));
	} 

	if(dinfo->string) {
		int height;

		x = rinfo->rect.r_left;

		if(dinfo->justify == DRAWTEXT_JUSTIFY_RIGHT)
			x += (rinfo->rect.r_width - dinfo->text_width);
		else 
		if(dinfo->justify == DRAWTEXT_JUSTIFY_CENTER)
			x += (rinfo->rect.r_width/2 - dinfo->text_width/2);

		height = dinfo->font_info->ascent + dinfo->font_info->descent;
		if(dinfo->editable || dinfo->show_underline)
			height += 2;
		y = rinfo->rect.r_top + dinfo->font_info->ascent +
			(rinfo->rect.r_height - height)/2;

		XDrawImageString(dpy, win, gc, 
			x, y,
			dinfo->string, dinfo->strlen);

		if(dinfo->show_underline) {
			XDrawLine(dpy, win, gc,
				rinfo->rect.r_left, 
				y+dinfo->font_info->descent+1,
				rinfo->rect.r_left+rinfo->rect.r_width,
				y+dinfo->font_info->descent+1);
		}
	}
	XFreeGC(dpy, gc);
}



void
drawtext_finish_edit(drawtext, use_new_string)
	Drawtext drawtext;
	int use_new_string;
{
        char            *new_string;
	Rectobj_info	*rinfo;

	if(!drawtext)
		return;
	
	rinfo = RECTOBJ_PRIVATE(drawtext);
	if(!rinfo->shared_info)
		return;

	if(use_new_string) {
	        new_string = (char*) xv_get(rinfo->shared_info->edit_item, 
				PANEL_VALUE);

		if(new_string && *new_string)
			xv_set(drawtext,
				DRAWTEXT_STRING, new_string,
				NULL);
	}

	xv_destroy_safe(rinfo->shared_info->edit_item);
	xv_destroy_safe(rinfo->shared_info->edit_panel);

	rinfo->shared_info->focus_drawtext = 
	rinfo->shared_info->edit_panel = 
	rinfo->shared_info->edit_item = NULL;
}

/* pick a "random" key */
#define FOCUS_DRAWTEXT_KEY XV_KEY_DATA, (1<<20)

Panel_setting
drawtext_end_edit_proc(item, event)
        Panel_item      item;
        Event           *event;
{
	Drawtext focus_drawtext = xv_get(item, FOCUS_DRAWTEXT_KEY);

	if(focus_drawtext)
		drawtext_finish_edit(focus_drawtext, TRUE);
        return PANEL_NEXT;
}


static Notify_value
drawtext_edit_interpose_notify(client, event, arg, type)
	Notify_client		client;
	Event			*event;
	Notify_arg		arg;	
	Notify_event_type	type;
{
	Drawtext	drawtext;
	Rectobj_info	*rinfo;

	drawtext = xv_get(client, FOCUS_DRAWTEXT_KEY);

	if(drawtext) {
	  rinfo = RECTOBJ_PRIVATE(drawtext);

	  if(rinfo->shared_info) {
		if(event_id(event) == WIN_MAP_NOTIFY)
			/* 
			 * Wait for mapping notify before setting the focus. 
			 */
			xv_set(rinfo->shared_info->edit_panel,
				WIN_SET_FOCUS,
				NULL);

		if(event_id(event) == KBD_DONE)
			drawtext_finish_edit(drawtext, TRUE);
	  }
	}

	return(notify_next_event_func(client, (Notify_event) event, arg, type));
}


Panel_item
drawtext_start_edit(paint_window, drawtext)
	Xv_window	paint_window;
	Drawimage	drawtext;
{
	Drawtext_info	*dinfo = DRAWTEXT_PRIVATE(drawtext);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawtext);
	int	height;
	int	y;

	if(rinfo->shared_info && rinfo->shared_info->focus_drawtext)
		drawtext_finish_edit(rinfo->shared_info->focus_drawtext, TRUE);

	rinfo->shared_info->focus_drawtext = drawtext;

	rinfo->shared_info->edit_panel = xv_create(paint_window, PANEL, 
		XV_SHOW,	FALSE,
		WIN_FONT,	(dinfo->font ? dinfo->font :
                                (rinfo->shared_info ? rinfo->shared_info->font :                                xv_find(NULL, FONT, NULL))), 
		WIN_HEIGHT,	rinfo->rect.r_height,
		WIN_WIDTH,	rinfo->rect.r_width,
		WIN_X, 		rinfo->rect.r_left,
		WIN_Y, 		rinfo->rect.r_top,
		FOCUS_DRAWTEXT_KEY,	drawtext,
	/*
	 *	use rectobj_upsearch
	 *	XV_HELP_DATA,	xv_get(drawtext, XV_HELP_DATA),
	 */
		NULL);

	xv_set(rinfo->shared_info->edit_panel,
		WIN_CMS, rinfo->shared_info->cms,
		WIN_FOREGROUND_COLOR, rinfo->shared_info->win_fg,
		WIN_BACKGROUND_COLOR, rinfo->shared_info->win_bg,
		WIN_CONSUME_EVENTS, WIN_MAP_NOTIFY, NULL,
		NULL);


	height = dinfo->font_info->ascent + dinfo->font_info->descent +2;
	y = (rinfo->rect.r_height - height)/2;

	rinfo->shared_info->edit_item = 
		xv_create(rinfo->shared_info->edit_panel, PANEL_TEXT,
			XV_X,		0,
			XV_Y,		y,
			PANEL_VALUE_DISPLAY_LENGTH, MAX(dinfo->strlen, 
						dinfo->disp_length),
			PANEL_VALUE_STORED_LENGTH, 255,
			PANEL_VALUE,		dinfo->string,
			PANEL_NOTIFY_PROC,	drawtext_end_edit_proc,
		/*	XV_HELP_DATA,	xv_get(drawtext, XV_HELP_DATA),*/
			FOCUS_DRAWTEXT_KEY,	drawtext,
			NULL);


	notify_interpose_event_func(rinfo->shared_info->edit_panel,
		drawtext_edit_interpose_notify, NOTIFY_SAFE);

	xv_set(rinfo->shared_info->edit_panel,
		XV_SHOW, TRUE,
		NULL);
	return rinfo->shared_info->edit_item;
}


void
drawtext_edit_single_click(paint_window, event, canvas_shell, drawtext)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Drawtext	drawtext;
{
	Drawtext_info	*dinfo = DRAWTEXT_PRIVATE(drawtext);
	Panel_item edititem;

	if(dinfo->editable && event_action(event) == ACTION_SELECT) {
		edititem = drawtext_start_edit(paint_window, drawtext);
		/*
		xv_set(edititem,
			PANEL_TEXT_SELECT_LINE,
			NULL);
		*/
	}
}


void
drawtext_set_geometry_proc(drawtext, newrect, oldrect)
	Drawtext	drawtext;
	Rect	*newrect;
	Rect	*oldrect;
{
	Drawtext_info	*dinfo = DRAWTEXT_PRIVATE(drawtext);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawtext);
	int	height;
	int	y;

	if(rinfo->shared_info && 
	   rinfo->shared_info->focus_drawtext == drawtext) {
		xv_set(rinfo->shared_info->edit_panel,
			WIN_HEIGHT,	newrect->r_height,
			WIN_WIDTH,	newrect->r_width,
			WIN_X, 		newrect->r_left,
			WIN_Y, 		newrect->r_top,
			NULL);

		height = dinfo->font_info->ascent + 
			dinfo->font_info->descent +2;
		y = (rinfo->rect.r_height - height)/2;

		xv_set(rinfo->shared_info->edit_item,
			XV_X,		0,
			XV_Y,		y,
			PANEL_VALUE_DISPLAY_LENGTH, MAX(dinfo->strlen, 
						dinfo->disp_length),
			NULL);
	}
}

