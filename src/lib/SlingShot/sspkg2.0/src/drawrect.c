/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)drawrect.c 1.27 92/06/23";
#endif
#endif

#include <X11/Xlib.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/win_input.h>
#include <sspkg/drawobj.h>
#include <sspkg/canshell.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"


#define DRAWRECT_IS_OPAQUE	(1<<0)

#define DRAWRECT_FLAGS	( DRAWRECT_IS_OPAQUE )

typedef struct drawrect_info {
	unsigned char	flags;
	short		bg2;
	short		bg3;
	short		white;
	unsigned short	border1;
	unsigned short	border2;
	unsigned short	border3;
} Drawrect_info;

#define DRAWRECT_PRIVATE(drawrect)	\
		XV_PRIVATE(Drawrect_info, Drawrect_struct, drawrect)


Pkg_private int 	drawrect_init();
Pkg_private Xv_opaque	drawrect_set_avlist();
Pkg_private Xv_opaque	drawrect_get_attr();
Pkg_private int 	drawrect_destroy();

	void	drawrect_paint_proc();

/*ARGSUSED*/
Pkg_private int
drawrect_init(parent, drawrect, avlist)
	Xv_opaque	parent;
	Drawrect	drawrect;
	Attr_avlist	avlist;
{
	Drawrect_info	*dinfo;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawrect);
	Drawrect_struct	*drawrect_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		drawrect_paint_proc,
		rectobj_event_proc,
		rectobj_map_event_proc,
		rectobj_set_geometry_proc,
		bag_manage_child_proc,
		bag_add_child_proc,
		bag_del_child_proc,
	};
		

	dinfo = xv_alloc(Drawrect_info);
	drawrect_object = (Drawrect_struct*) drawrect;
	drawrect_object->private_data = (Xv_opaque) dinfo;

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	dinfo->border2 = 1;
	dinfo->bg2 = -1;
	dinfo->bg3 = -1;
	dinfo->white = -1;
	FLAG_SET(dinfo->flags, DRAWRECT_IS_OPAQUE);

	rinfo->border = (dinfo->border1 + dinfo->border2 + dinfo->border3);
	rinfo->rect.r_width = 
	rinfo->rect.r_height =
	rinfo->min_width = 
	rinfo->min_height = 
		rinfo->border*2;
        RF_SET(rinfo, BAG_ANCHORED_FLAG);

	return(XV_OK);
}


Pkg_private Xv_opaque
drawrect_set_avlist(drawrect, avlist)
	Drawrect		drawrect;
	register Attr_avlist	avlist;
{
        register Drawobj_attr attr;
        register Drawrect_info *dinfo = DRAWRECT_PRIVATE(drawrect);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawrect);
	short	check_sizes = FALSE;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
		    xv_super_set_avlist(drawrect, &drawrect_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(drawrect);
			return(set_result);
		}
	}


	while (attr = (Drawobj_attr) * avlist++)
	  switch (attr) {

		case DRAWOBJ_FILLED:
			if(*avlist++)
			  FLAG_SET(dinfo->flags, DRAWRECT_IS_OPAQUE);
			else
			  FLAG_UNSET(dinfo->flags, DRAWRECT_IS_OPAQUE, 
					DRAWRECT_FLAGS);
			RF_SET(rinfo, (RF_REPAINT|RF_CLEAR)); 
			break;

		case DRAWRECT_BORDER1:
			dinfo->border1 = (short)*avlist++;
			check_sizes = TRUE;
			break;

		case DRAWRECT_BORDER2:
			dinfo->border2= (short)*avlist++;
			check_sizes = TRUE;
			break;

		case DRAWRECT_BORDER3:
			dinfo->border3= (short)*avlist++;
			check_sizes = TRUE;
			break;

		case RECTOBJ_BG2:
			dinfo->bg2 = (short) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case RECTOBJ_BG3:
			dinfo->bg3 = (short) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case RECTOBJ_WHITE:
			dinfo->white = (short) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case BAG_ANCHORED:
			bag_set_anchored(drawrect, *avlist);
			avlist++;
			break;

		case BAG_AUTO_SHRINK:
			if(*avlist++)
				RF_SET(rinfo, BAG_AUTO_SHRINK_FLAG);
			else
				RF_UNSET(rinfo, BAG_AUTO_SHRINK_FLAG);
			break;

		default:
			avlist = attr_skip(attr, avlist);
	  }

	if(check_sizes) {
		short old_border;
		short b;

		old_border = rinfo->border;
		rinfo->border= dinfo->border1 + dinfo->border2 + dinfo->border3;
		if(old_border != rinfo->border)
			bag_set_border(drawrect, rinfo->border, old_border);

		RF_SET(rinfo, (RF_REPAINT|RF_CLEAR)); 
	}
	if(rectobj_finish_set1(drawrect))
		rectobj_finish_set2(drawrect);

	return(XV_SET_DONE);
}


/*ARGSUSED*/
Pkg_private Xv_opaque
drawrect_get_attr(drawrect, status, which_attr, avlist)
	Drawrect		drawrect;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Drawrect_info  *dinfo = DRAWRECT_PRIVATE(drawrect);

	switch (which_attr) {

		case DRAWOBJ_FILLED:
			return (Xv_opaque) 
				FLAG_TRUE(dinfo->flags, DRAWRECT_IS_OPAQUE);

		case DRAWRECT_BORDER1:
			return (Xv_opaque) dinfo->border1;

		case DRAWRECT_BORDER2:
			return (Xv_opaque) dinfo->border2;

		case DRAWRECT_BORDER3:
			return (Xv_opaque) dinfo->border3;

		case RECTOBJ_BG2:
			return (Xv_opaque) dinfo->bg2;

		case RECTOBJ_BG3:
			return (Xv_opaque) dinfo->bg3;

		case RECTOBJ_WHITE:
			return (Xv_opaque) dinfo->white;

		case BAG_ANCHORED:
			return (Xv_opaque) (RF_TRUE(RECTOBJ_PRIVATE(drawrect), 
						BAG_ANCHORED_FLAG));

		case BAG_AUTO_SHRINK:
			return (Xv_opaque) (RF_TRUE(RECTOBJ_PRIVATE(drawrect), 
						BAG_AUTO_SHRINK_FLAG));
 
		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}

/*ARGSUSED*/
Pkg_private int
drawrect_destroy(drawrect, status)
	Drawrect		drawrect;
	Destroy_status	status;
{
	Drawrect_info	*dinfo = DRAWRECT_PRIVATE(drawrect);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(dinfo);
	return XV_OK;
}


static void
paint_up(r, dinfo, dpy, win, gc)
	Rect	*r;
	Drawrect_info *dinfo;
	Display *dpy;
        Window  win;
	GC	gc;
{
	unsigned short i;

	for(i=0; i < dinfo->border2; i++) {
		XDrawLine(dpy, win, gc,
			r->r_left+i, r->r_top+r->r_height-i,
			r->r_left+i, r->r_top+i);
		XDrawLine(dpy, win, gc,
			r->r_left+i, r->r_top+i,
			r->r_left+r->r_width-i-1, r->r_top+i);
	}
}

static void
paint_down(r, dinfo, dpy, win, gc)
	Rect	*r;
	Drawrect_info *dinfo;
	Display *dpy;
        Window  win;
	GC	gc;
{
	unsigned short i;

	for(i=0; i < dinfo->border2; i++) {
		XDrawLine(dpy, win, gc,
			r->r_left+r->r_width-i, r->r_top+i,
			r->r_left+r->r_width-i, r->r_top+r->r_height-i);
		XDrawLine(dpy, win, gc,
			r->r_left+r->r_width-i, r->r_top+r->r_height-i,
			r->r_left+i+1, r->r_top+r->r_height-i);
	}
}

/*ARGSUSED*/
Pkg_private void
drawrect_paint_proc(drawrect, dpy, win, xrects)
        Drawrect drawrect;
        Display *dpy;
        Window  win;
        Xv_xrectlist *xrects;
{
	Drawrect_info *dinfo = DRAWRECT_PRIVATE(drawrect);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawrect);
	GC gc;
	long	p1;
	long	p2;
	short offset;

	gc = XCreateGC(dpy, win, 0, 0);
	if(xrects && xrects->count)
		XSetClipRectangles(dpy, gc,
			0, 0,
			xrects->rect_array,
			xrects->count,
			Unsorted);

	if(FLAG_IS_SET(dinfo->flags, DRAWRECT_IS_OPAQUE)) {
		offset = dinfo->border1+dinfo->border2;
		if(HIGHLIGHT_RECTOBJ(rinfo))
			XSetForeground(dpy, gc, 
				pixel_bg2(rinfo->shared_info, dinfo->bg2));
		else
			XSetForeground(dpy, gc, 
				pixel_bg(rinfo->shared_info, rinfo->bg_color));

		XFillRectangle(dpy, win, gc,
			rinfo->rect.r_left + offset,
			rinfo->rect.r_top + offset,
			rinfo->rect.r_width - offset - offset,
			rinfo->rect.r_height - offset - offset);
	}

	if(dinfo->border2 > 0) {
		Rect r;

		r.r_left= rinfo->rect.r_left + dinfo->border1;
		r.r_top= rinfo->rect.r_top + dinfo->border1;
		r.r_width= rinfo->rect.r_width-dinfo->border1-dinfo->border1-1;
		r.r_height=rinfo->rect.r_height-dinfo->border1-dinfo->border1-1;

		if(rinfo->shared_info->num_colors < 3) {
			/* 2D */
			p1 = p2 =
				pixel_fg(rinfo->shared_info, rinfo->fg_color);

		} else {
		  if(HIGHLIGHT_RECTOBJ(rinfo)) {
			/* 3D selected */
			p2 = pixel_white(rinfo->shared_info, dinfo->white);
			p1 = pixel_bg3(rinfo->shared_info, dinfo->bg3);
		  } else {
			/* 3D normal */
			p1 = pixel_white(rinfo->shared_info, dinfo->white);
			p2 = pixel_bg3(rinfo->shared_info, dinfo->bg3);
		  }
		}

		XSetForeground(dpy, gc, p1);
		paint_up(&r, dinfo, dpy, win, gc);
		XSetForeground(dpy, gc, p2);
		paint_down(&r, dinfo, dpy, win, gc);
	}

	XFreeGC(dpy, gc);

        rectobj_paint_children(drawrect, dpy, win, xrects);
}

 
