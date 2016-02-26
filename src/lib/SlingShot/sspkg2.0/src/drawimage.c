/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)drawimage.c 1.24 92/07/08";
#endif
#endif

#include <X11/Xlib.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/win_input.h>
#include <sspkg/drawobj.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include "dimage_impl.h"
#include <sspkg/canshell.h>

Pkg_private int 	drawimage_init();
Pkg_private Xv_opaque	drawimage_set_avlist();
Pkg_private Xv_opaque	drawimage_get_attr();
Pkg_private int 	drawimage_destroy();

	void	drawimage_paint_proc();
	Rectobj	drawimage_map_event_proc();
static	void	change_image();

/*ARGSUSED*/
Pkg_private int
drawimage_init(parent, drawimage, avlist)
	Xv_opaque	parent;
	Drawimage		drawimage;
	Attr_avlist	avlist;
{
	Drawimage_info		*dinfo;
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(drawimage);
	Drawimage_struct	*drawimage_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		drawimage_paint_proc,
		rectobj_selection_event_proc,
		drawimage_map_event_proc,
		NULL,			/* set geometry */
		NULL,			/* manage child */
		NULL,			/* add child */
		NULL,			/* del child */
	};

	if(drawicon_private_diinfo)
		dinfo = drawicon_private_diinfo; /* created by drawicon */
	else
		dinfo = xv_alloc(Drawimage_info);
	drawimage_object = (Drawimage_struct*) drawimage;
	drawimage_object->private_data = (Xv_opaque) dinfo;

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;
	RF_SET(rinfo, RF_SELECTABLE);

	return(XV_OK);
}


Pkg_private Xv_opaque
drawimage_set_avlist(drawimage, avlist)
	Drawimage		drawimage;
	register Attr_avlist	avlist;
{
        register Drawobj_attr attr;
        register Drawimage_info *dinfo = DRAWIMAGE_PRIVATE(drawimage);
        register Rectobj_info  *rinfo = RECTOBJ_PRIVATE(drawimage);
	short	check_sizes = FALSE;
	short	end_create;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result = 
		    xv_super_set_avlist(drawimage, &drawimage_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(drawimage);
			return(set_result);
		}
		end_create = FALSE;
	} else
		end_create = TRUE;

	while (attr = (Drawobj_attr) * avlist++)
	  switch (attr) {

	    case DRAWIMAGE_IMAGE1:
	    case DRAWIMAGE_IMAGE2:
		check_sizes = TRUE;	/* warning... fall through */
	    case DRAWIMAGE_IMAGE1_MASK:
	    case DRAWIMAGE_IMAGE2_MASK:
		drawimage_set_attr(dinfo, attr, (void*)*avlist++);
		break;

	    case RECTOBJ_MIN_WIDTH:
	    case RECTOBJ_MIN_HEIGHT:
	    case XV_WIDTH:
	    case XV_HEIGHT:
		avlist++;
		check_sizes= TRUE;
		break;

	    default:
		avlist = attr_skip(attr, avlist);

	  }

	if(check_sizes || end_create)
		drawimage_calc_rect(drawimage);

	if(rectobj_finish_set1(drawimage))
		rectobj_finish_set2(drawimage);

	if(end_create)
		return(XV_OK);
	
	return(XV_SET_DONE);
}


void
drawimage_set_attr(dinfo, attr, value)
	Drawimage_info *dinfo;
	Attr_attribute	attr;
	void		*value;
{
	switch(attr) {
	    case DRAWIMAGE_IMAGE1:
		change_image(&dinfo->image1, (Server_image) value);
		break;

	    case DRAWIMAGE_IMAGE1_MASK:
		dinfo->image1.mask= (Server_image) value;
		break;

	    case DRAWIMAGE_IMAGE2:
		change_image(&dinfo->image2, (Server_image) value);
		break;

	    case DRAWIMAGE_IMAGE2_MASK:
		dinfo->image2.mask= (Server_image) value;
		break;
	}
}

void
drawimage_calc_rect(drawimage)
	Drawimage drawimage;
{
        register Drawimage_info *dinfo = DRAWIMAGE_PRIVATE(drawimage);
        register Rectobj_info  *rinfo = RECTOBJ_PRIVATE(drawimage);

	rinfo->min_width = 
		MAX(dinfo->image1.width, dinfo->image2.width);
	rinfo->rect.r_width = 
		MAX((unsigned short) rinfo->rect.r_width, rinfo->min_width);

	rinfo->min_height = 
		MAX(dinfo->image1.height, dinfo->image2.height);
	rinfo->rect.r_height= 
		MAX((unsigned short) rinfo->rect.r_height, rinfo->min_height);

	RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
}


/*ARGSUSED*/
Pkg_private Xv_opaque
drawimage_get_attr(drawimage, status, which_attr, avlist)
	Drawimage	drawimage;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Drawimage_info  *dinfo = DRAWIMAGE_PRIVATE(drawimage);

	switch (which_attr) {

		case DRAWIMAGE_IMAGE:
			return (Xv_opaque) dinfo->image1.image;

		case DRAWIMAGE_IMAGE1_MASK:
			return (Xv_opaque) dinfo->image1.mask;

		case DRAWIMAGE_IMAGE2:
			return (Xv_opaque) dinfo->image2.image;

		case DRAWIMAGE_IMAGE2_MASK:
			return (Xv_opaque) dinfo->image2.mask;

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}

/*ARGSUSED*/
Pkg_private int
drawimage_destroy(drawimage, status)
	Drawimage		drawimage;
	Destroy_status	status;
{
	Drawimage_info	*dinfo = DRAWIMAGE_PRIVATE(drawimage);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(dinfo);
	return XV_OK;
}


/*ARGSUSED*/
Pkg_private void
drawimage_paint_proc(drawimage, dpy, win, xrects)
        Drawimage drawimage;
        Display *dpy;
        Window  win;
        Xv_xrectlist *xrects;
{
	Drawimage_info *dinfo = DRAWIMAGE_PRIVATE(drawimage);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawimage);
	Drawimage_image *image;
	GC gc;
	int highlighted;
	static	void	render_image();

	gc = XCreateGC(dpy, win, 0, 0);

	XSetBackground(dpy, gc, pixel_bg(rinfo->shared_info, rinfo->bg_color));
	XSetForeground(dpy, gc, pixel_fg(rinfo->shared_info, rinfo->fg_color));
	/* turn off NoExpose events */
	XSetGraphicsExposures(dpy, gc, False);

	if(xrects && xrects->count)
		XSetClipRectangles(dpy, gc,
			0, 0,
			xrects->rect_array,
			xrects->count,
			Unsorted);

	highlighted = HIGHLIGHT_RECTOBJ(rinfo);

	if(highlighted && (dinfo->image2.image))
		render_image(rinfo, &dinfo->image2, dpy, win, gc);
	else
	if(dinfo->image1.image) {
		render_image(rinfo, &dinfo->image1, dpy, win, gc);
		if(highlighted)
		    XDrawRectangle(dpy, win, gc,
			rinfo->rect.r_left, rinfo->rect.r_top,
			rinfo->rect.r_width-1, rinfo->rect.r_height-1);
	}

	XFreeGC(dpy, gc);
}


static void
render_image(rinfo, image, dpy, win, gc)
	Rectobj_info	*rinfo;
	Drawimage_image *image;
	Display		*dpy;
        Window  	win;
	GC		gc;
{
	Drawable	pixmap;
	int		left;
	int		top;

	pixmap = (Drawable) xv_get(image->image, SERVER_IMAGE_PIXMAP);
	left = rinfo->rect.r_left + rinfo->rect.r_width/2 - image->width/2;
	top = rinfo->rect.r_top + rinfo->rect.r_height/2 - image->height/2;
	if(image->mask) {
		XSetClipMask(dpy, gc, 
			(Drawable) xv_get(image->mask, SERVER_IMAGE_PIXMAP));
		XSetClipOrigin(dpy, gc, left, top);
	}
	if(image->depth == 1)
		XCopyPlane(dpy,
			pixmap,
			win,
			gc, 
			0, 0,
			(int) image->width, (int) image->height,
			(int) left, (int) top,
			1);
	else
		XCopyArea(dpy,
			pixmap,
			win,
			gc, 
			0, 0,
			(int) image->width, (int) image->height,
			(int) left, (int) top);
}


static void
change_image(image, svr_image)
	Drawimage_image	*image;
	Server_image	svr_image;
{
	image->image = svr_image;
	if(svr_image == 0) {
		image->width = 0;
		image->height = 0;
		image->depth = 0;
	} else {
		image->width = (short) xv_get(svr_image, XV_WIDTH);
		image->height = (short) xv_get(svr_image, XV_HEIGHT);
		image->depth = (short) xv_get(svr_image, SERVER_IMAGE_DEPTH);
	}
}


Rectobj
drawimage_map_event_proc(drawimage, event)
	Rectobj		drawimage;
	Event		*event;
{
        Drawimage_info  *dinfo = DRAWIMAGE_PRIVATE(drawimage);
        Rectobj_info    *rinfo = RECTOBJ_PRIVATE(drawimage);
	Drawimage_image	*image = NULL;

	if(!RF_IS_SET(rinfo, RF_PAINTED))
		return 0;

	if(HIGHLIGHT_RECTOBJ(rinfo)) {
		if(dinfo->image2.mask)
			image = &dinfo->image2;
	} else {
		if(dinfo->image1.mask)
			image = &dinfo->image1;
	}

	if(image) {
		XImage *xim;
		int 	x;
		int	y;

		/* event position to coordinate system of image */
		x = event_x(event) - rinfo->rect.r_left -
			(rinfo->rect.r_width/2 - image->width/2);
		y = event_y(event) - rinfo->rect.r_top -
			(rinfo->rect.r_height/2 - image->height/2);

		/* check for outside pixmap to avoid BadMatch */
		if(x < 0 || y < 0 || x >= image->width || y >= image->height)
			return 0;

		/*
		 * Assumptions: 
		 *	the mask is always a 1 bit deep pixmap.
		 *	never called when not on display (shared_info != NULL)
		 */
		xim = XGetImage(rinfo->shared_info->dpy,
			(Drawable) xv_get(image->mask, SERVER_IMAGE_PIXMAP),
			x, y,
			(unsigned int) 1, (unsigned int) 1,
			(unsigned long) 1,
			XYPixmap);

		if(xim) {
			if(XGetPixel(xim, 0, 0)) {
				XDestroyImage(xim);
				return drawimage;
			}
			XDestroyImage(xim);
		}

	} else {
		if(rect_includespoint(&rinfo->rect, 
				event_x(event), event_y(event)))
			return drawimage;
	}

	return 0;
}

