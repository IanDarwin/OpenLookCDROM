/*
 * (c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved. See
 * LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#) drawarea.c 1.43 92/11/08 ";
#endif
#endif

#include <xview/font.h>
#include <xview/cms.h>
#include <xview/svrimage.h>
#include <sspkg/canshell.h>
#include <sspkg/drawobj.h>
#include <sspkg/disp_list.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"

	void	drawarea_paint_proc();
	void	drawarea_render_display_list();
	Rectobj	drawarea_map_event_proc();
	Xv_Font drawarea_get_font();

typedef struct {
	GC              gc;
	Xv_Font		font;

	int             allocated;
	int             used;
	int             displayed;
	int		min_alloc;
	Display_list_cmd *cmds;
	Drawarea_map_event_mode	map_mode;

	Xv_opaque	mark_key;

	double          right_x, left_x, upper_y, lower_y;
} Drawarea_info;

#define DRAWAREA_PRIVATE(drawarea)      \
                XV_PRIVATE(Drawarea_info, Drawarea_struct, drawarea)

#define INIT_MIN_ALLOC	100

Pkg_private int
drawarea_init(parent, drawarea, avlist)
	Xv_opaque       parent;
	Drawarea        drawarea;
	Attr_avlist     avlist;
{
	extern void	rectobj_simple_style_change_proc();
	Drawarea_info  *dinfo;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawarea);
	Drawarea_struct *drawarea_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		drawarea_paint_proc,
		rectobj_selection_event_proc,
		drawarea_map_event_proc,
		rectobj_set_geometry_proc,
		NULL,			/* manage child */
		NULL,			/* add child */
		NULL,			/* del child */
		NULL,			/* new parent */
		NULL,			/* start drag */
		rectobj_simple_style_change_proc,
	};
		
	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	dinfo = xv_alloc(Drawarea_info);
	dinfo->gc = NULL;
	dinfo->displayed = 0;

	dinfo->upper_y = 0.0;
	dinfo->lower_y = 10000.0;
	dinfo->left_x = 0.0;
	dinfo->right_x = 10000.0;

	dinfo->allocated = 
	dinfo->min_alloc = INIT_MIN_ALLOC;
	dinfo->cmds = (Display_list_cmd *) malloc( INIT_MIN_ALLOC );
	dinfo->map_mode = DRAWAREA_MAP_ANY;

	drawarea_object = (Drawarea_struct *) drawarea;
	drawarea_object->private_data = (Xv_opaque) dinfo;

	return (XV_OK);
}


Pkg_private     Xv_opaque
drawarea_set_avlist(drawarea, avlist)
	Drawarea        drawarea;
	register Attr_avlist avlist;
{
	register Drawobj_attr attr;
	register Drawarea_info *dinfo = DRAWAREA_PRIVATE(drawarea);
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawarea);
	double          dvalue;

	if (*avlist != XV_END_CREATE) {
		Xv_opaque       set_result;
		set_result =
			xv_super_set_avlist(drawarea, &drawarea_pkg, avlist);
		if (set_result != XV_OK) {
			rectobj_reset_set_info(drawarea);
			return (set_result);
		}
	}
	while (attr = (Drawobj_attr) * avlist++)
		switch (attr) {

		case DRAWAREA_LEFT_X:
			dvalue = *((double *) *avlist++);

			if (dvalue != dinfo->left_x) {
				dinfo->left_x = dvalue;
				RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			}
			break;

		case DRAWAREA_RIGHT_X:
			dvalue = *((double *) *avlist++);

			if (dvalue != dinfo->right_x) {
				dinfo->right_x = dvalue;
				RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			}
			break;

		case DRAWAREA_UPPER_Y:
			dvalue = *((double *) *avlist++);

			if (dvalue != dinfo->upper_y) {
				dinfo->upper_y = dvalue;
				RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			}
			break;

		case DRAWAREA_LOWER_Y:
			dvalue = *((double *) *avlist++);

			if (dvalue != dinfo->lower_y) {
				dinfo->lower_y = dvalue;
				RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			}
			break;

		case DRAWAREA_MIN_ALLOC:
			dinfo->min_alloc = (int) *avlist;
			if(dinfo->min_alloc == 0)
				/*
				 * Setting this to zero means use the current 
				 * amount in use as the min alloc point.
				 */
				dinfo->min_alloc = dinfo->used;
			else 
			  if(dinfo->min_alloc > dinfo->allocated) {
				dinfo->cmds = (Display_list_cmd *)
					realloc(dinfo->cmds, dinfo->min_alloc);
				dinfo->allocated = dinfo->min_alloc;
			}
			break;

		case DRAWAREA_MAP_EVENTS:
			dinfo->map_mode = (Drawarea_map_event_mode) *avlist++;
			break;

		case XV_FONT:
			dinfo->font = (Xv_Font) *avlist++;
			RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			break;

		case XV_END_CREATE:
			if(rectobj_finish_set1(drawarea))
				rectobj_finish_set2(drawarea);
			return (XV_OK);

		default:
			avlist = attr_skip(attr, avlist);
		}

	if(rectobj_finish_set1(drawarea))
		rectobj_finish_set2(drawarea);

	return (XV_SET_DONE);
}


/* ARGSUSED */
Pkg_private     Xv_opaque
drawarea_get_attr(drawarea, status, which_attr, avlist)
	Drawarea        drawarea;
	int            *status;
	register Attr_attribute which_attr;
	Attr_avlist     avlist;
{
	static double   return_value;

	Drawarea_info  *dinfo = DRAWAREA_PRIVATE(drawarea);
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawarea);

	switch (which_attr) {

	case DRAWAREA_LEFT_X:
		return_value = dinfo->left_x;
		return ((Xv_opaque) & return_value);

	case DRAWAREA_RIGHT_X:
		return_value = dinfo->right_x;
		return ((Xv_opaque) & return_value);

	case DRAWAREA_UPPER_Y:
		return_value = dinfo->upper_y;
		return ((Xv_opaque) & return_value);

	case DRAWAREA_LOWER_Y:
		return_value = dinfo->lower_y;
		return ((Xv_opaque) & return_value);

	case DRAWAREA_MIN_ALLOC:
		return (Xv_opaque) dinfo->min_alloc;

	case DRAWAREA_MAP_EVENTS:
		return (Xv_opaque) dinfo->map_mode;

	case DRAWAREA_LAST_MARK_KEY:
		return (Xv_opaque) dinfo->mark_key;

	case XV_FONT:
		return (Xv_opaque) dinfo->font;
		
	default:
		*status = XV_ERROR;
		return (Xv_opaque) 0;
	}
}

/* ARGSUSED */
Pkg_private int
drawarea_destroy(drawarea, status)
	Drawarea        drawarea;
	Destroy_status  status;
{
	Drawarea_info  *dinfo = DRAWAREA_PRIVATE(drawarea);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(dinfo->cmds);
	free(dinfo);
	return XV_OK;
}


/* ARGSUSED */
Pkg_private void
drawarea_paint_proc(drawarea, dpy, win, xrects)
	Drawarea        drawarea;
	Display        *dpy;
	Window          win;
	Xv_xrectlist   *xrects;
{
	Drawarea_info  *dinfo = DRAWAREA_PRIVATE(drawarea);
	Xv_xrectlist   repaint_area;

	if(xrects && xrects->count) {
		int i, j;
		Rect *r = &RECTOBJ_PRIVATE(drawarea)->rect;

		/*
		 * Create a new clipping rect array that does not
		 * go beyond the bounds of the drawarea.
		 */
		for(i=0, j=0; i < xrects->count; i++, j++) {
			rect_intersection((Rect*) &xrects->rect_array[i], r,
				&repaint_area.rect_array[j]);
			if(repaint_area.rect_array[j].width == 0 ||
			   repaint_area.rect_array[j].height == 0)
				j--;
		}

		if(j==0)
			return; /* did not intersect */
		repaint_area.count = j;

	} else {
		repaint_area.count = 1;
		repaint_area.rect_array[0] = *(XRectangle *)
			&RECTOBJ_PRIVATE(drawarea)->rect;
	}

	drawarea_render_display_list(drawarea,
			      dpy,
			      win,
			      &repaint_area,
			      0);	/* flag indicates full repaint */

	rectobj_paint_children(drawarea, dpy, win, &repaint_area);
}


static void
drawarea_clear_dl_cmds(drawarea)
	Drawarea	drawarea;
{
	Drawarea_info	*dinfo = DRAWAREA_PRIVATE(drawarea);

	if(dinfo->allocated != dinfo->min_alloc)
		dinfo->cmds = (Display_list_cmd *) 
			realloc(dinfo->cmds, dinfo->min_alloc);
	dinfo->allocated = dinfo->min_alloc;
	dinfo->displayed = 0;
	dinfo->used = 0;
}


void
VClear(drawarea)
	Drawarea	drawarea;
{
	drawarea_clear_dl_cmds(drawarea);
	if ( RECTOBJ_PRIVATE(drawarea) ->shared_info)
		rectobj_repaint_rect(drawarea, NULL, TRUE);
}


Display_list_cmd *
display_list_append(drawarea, vec, size)
	Drawarea	drawarea;
	Display_list_vec *vec;
	int		size;
{
	Drawarea_info	*dinfo = DRAWAREA_PRIVATE(drawarea);
	Display_list_cmd *cmd;
	
	if(size & 0x7)
		/* all display list entries are quad word aligned */
		size = (size & 0xfffffff8) + 0x8;

	if(dinfo->allocated < dinfo->used + size) {
		dinfo->allocated += size + 100;
		dinfo->cmds = (Display_list_cmd *) 
			realloc(dinfo->cmds, dinfo->allocated);
	}

	cmd = (Display_list_cmd *) ((char *)dinfo->cmds + dinfo->used);
	dinfo->used += size;

	cmd->vec = vec;
	cmd->size = size;

	return (cmd);
}


static void
drawarg_arg_init(drawarea, dl_arg)
	Drawarea		drawarea;
	Display_list_arg	*dl_arg;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawarea);
	Drawarea_info	*dinfo = DRAWAREA_PRIVATE(drawarea);

	dl_arg->drawarea = drawarea;
	dl_arg->canvas_shell = ((rinfo->shared_info) ? 
			rinfo->shared_info->canvas_shell: 
			XV_NULL);

	dl_arg->rect = rinfo->rect;
	dl_arg->x_m = (double) rinfo->rect.r_width /
			(dinfo->right_x - dinfo->left_x);
	dl_arg->x_b = (double) rinfo->rect.r_left - 
			dl_arg->x_m * dinfo->left_x;
	dl_arg->y_m = (double) rinfo->rect.r_height /
			(dinfo->lower_y - dinfo->upper_y);
	dl_arg->y_b = (double) rinfo->rect.r_top -
			dl_arg->y_m * dinfo->upper_y;
	dl_arg->index = 0;
	dl_arg->font = drawarea_get_font(drawarea, dinfo->font);
	dl_arg->font_info = (XFontStruct*) xv_get(dl_arg->font, FONT_INFO);
	dl_arg->current_mark_key = NULL;
}


Xv_Font
drawarea_get_font(drawarea, font)
	Drawarea	drawarea;
	Xv_Font		font;
{
	Drawarea_info *dinfo;
	Rectobj_info *rinfo;

	/* 
	 * Font should first come from first non-null of:
	 * display_list setting, drawarea font, shared_info (canvas_shell) font.
	 * This only evaluates only the last two.
	 */

	if(font)
		return font;

	dinfo = DRAWAREA_PRIVATE(drawarea);
	if(dinfo->font)
		return dinfo->font;

	rinfo = RECTOBJ_PRIVATE(drawarea);
	if(rinfo->shared_info) 
		/*
		 * shared_info->font should never be NULL, 
		 * as maintained by canvas_shell.
		 * If not attached to a canvas_shell, this is meaningless.
		 */
		return rinfo->shared_info->font; 
	return NULL;
}


void
drawarea_render_display_list(drawarea, dpy, win, xrects, partial_flag)
	Drawarea	drawarea;  
	Display		*dpy;
	XID		win;
	Xv_xrectlist	*xrects;
	int		partial_flag;
{
	Drawarea_info  *dinfo = DRAWAREA_PRIVATE(drawarea);
	Rectobj_info   *rinfo = RECTOBJ_PRIVATE(drawarea);
	Display_list_cmd *cmd;
	GC              gc = dinfo->gc;
	Display_list_arg dl_arg;
	int             i;

	if(!rinfo->shared_info)
		return;

	drawarg_arg_init(drawarea, &dl_arg);

	if (gc == NULL) {
		/* GC must remain around to do VFlush processing correctly */
		dinfo->gc = XCreateGC(dpy, win, 0, 0);
	}

	if (!gc || !partial_flag) {
		XGCValues gcv;

		gc = dinfo->gc;
		/*
		XSetForeground(dpy, gc,
				pixel_fg(rinfo->shared_info, rinfo->fg_color));
		XSetBackground(dpy, gc,
				pixel_bg(rinfo->shared_info, rinfo->bg_color));

		XSetFont(dpy, gc, (XID) xv_get(dl_arg.font, XV_XID));
		*/
		gcv.foreground = pixel_fg(rinfo->shared_info, rinfo->fg_color);
		gcv.background = pixel_bg(rinfo->shared_info, rinfo->bg_color);
		gcv.font = (XID) xv_get(dl_arg.font, XV_XID);
		gcv.line_width = 0;
		gcv.line_style = LineSolid;
		gcv.fill_style = FillSolid;
		/* for some reason, it chokes on these.
		gcv.tile = None;
		gcv.stipple = None;
		*/
		XChangeGC(dpy, gc, (long) (
			GCForeground|
			GCBackground|
			GCFont|
			GCLineWidth|
			GCLineStyle|
			GCFillStyle), &gcv);
		dl_arg.cms = rinfo->shared_info->cms;
	}

	if(xrects && xrects->count)
		XSetClipRectangles(dpy, gc,
				0, 0,
				xrects->rect_array,
				xrects->count,
				Unsorted);

	dl_arg.dpy = dpy;
	dl_arg.win_xid = win;
	dl_arg.gc = gc;

	i = (partial_flag ? dinfo->displayed : 0);
	while(i < dinfo->used) {
		cmd = (Display_list_cmd *) ((char *) dinfo->cmds + i);
		if(cmd->vec && cmd->vec->render) {
			dl_arg.index = i;
			(cmd->vec->render) (&dl_arg, cmd);
		}
		i += cmd->size;
	}

	dinfo->displayed = dinfo->used;
}


Rectobj
drawarea_map_event_proc(drawarea, event)
	Drawarea drawarea;
	Event	*event;
{
	register Drawarea_info	*dinfo = DRAWAREA_PRIVATE(drawarea);
	register Display_list_cmd *cmd;
	Display_list_arg dl_arg;
	register int	i;
	Rectobj		return_val;
	int		mapped;

	return_val = rectobj_map_event_proc(drawarea, event);

	if((return_val != drawarea) || (dinfo->map_mode == DRAWAREA_MAP_ANY))
		return return_val;

	drawarg_arg_init(drawarea, &dl_arg);
	dinfo->mark_key = NULL;
	mapped = FALSE;
	i = 0;
	while(i < dinfo->used) {
		cmd = (Display_list_cmd *) ((char *) dinfo->cmds + i);
		if(cmd->vec && cmd->vec->map_event) {
			dl_arg.index = i;
			if((cmd->vec->map_event)(&dl_arg, cmd, event)) {
				dinfo->mark_key = dl_arg.current_mark_key;
				if(dinfo->map_mode == DRAWAREA_MAP_FIRST)
					return drawarea;
				mapped = TRUE;
			}
		}
		i += cmd->size;
	}
	return (mapped ? drawarea : NULL);
}


Display_list_cmd *
drawarea_init_traverse(drawarea, dl_arg)
	Drawarea	drawarea;
	Display_list_arg *dl_arg;
{
	drawarg_arg_init(drawarea, dl_arg);
	return (Display_list_cmd *) DRAWAREA_PRIVATE(drawarea)->cmds;
}


Display_list_cmd *
drawarea_next_traverse(dl_arg)
	Display_list_arg *dl_arg;
{
	Drawarea_info	*dinfo = DRAWAREA_PRIVATE(dl_arg->drawarea);
	Display_list_cmd *current;

	if(dl_arg->index < dinfo->used) {
	  current = (Display_list_cmd*) ((char*)dinfo->cmds + dl_arg->index);
	  dl_arg->index += current->size;
	  if(dl_arg->index < dinfo->used)
		return (Display_list_cmd*) ((char*)dinfo->cmds + dl_arg->index);
	}
	return (Display_list_cmd *) NULL;
}


double
dl_convert_i2rx(drawarea, x)
        Drawarea drawarea;
        int     x;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawarea);
	Drawarea_info *dinfo = DRAWAREA_PRIVATE(drawarea);
	double scaletounit;

	scaletounit = (double)(x-rinfo->rect.r_left)/(double)rinfo->rect.r_width;
	if(dinfo->right_x > dinfo->left_x)
		return scaletounit *
			(dinfo->right_x-dinfo->left_x) + dinfo->left_x;
	else
		return dinfo->left_x - 
			scaletounit * (dinfo->left_x-dinfo->right_x);
}

double
dl_convert_i2ry(drawarea, y)
        Drawarea drawarea;
        int     y;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawarea);
	Drawarea_info *dinfo = DRAWAREA_PRIVATE(drawarea);
	double scaletounit;

	scaletounit = (double)(y-rinfo->rect.r_top)/(double)rinfo->rect.r_height;
	if(dinfo->lower_y > dinfo->upper_y)
		return scaletounit *
			(dinfo->lower_y-dinfo->upper_y) + dinfo->upper_y;
	else
		return dinfo->upper_y - 
			scaletounit * (dinfo->upper_y-dinfo->lower_y);
}

