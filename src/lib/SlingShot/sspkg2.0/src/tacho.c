/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char	sccsid[] = "@(#)tacho.c 1.16 92/11/12";
#endif
#endif


#include <X11/Xlib.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include <math.h>

Pkg_private int 	tacho_init();
Pkg_private Xv_opaque	tacho_set_avlist();
Pkg_private Xv_opaque	tacho_get_attr();
Pkg_private int 	tacho_destroy();

	void 	tacho_paint_proc();
static	void	tacho_update_pointer();
	void	tacho_set_geometry_proc();

typedef struct tacho_info {
	int             value;
	int             min_value;
	int             max_value;

	short		bg2;
	short		arc_height;
	XPoint		points[4];	/* the last pointer drawn */
	/*short		erase_pointer;*/
	double		centerx, centery;
} Tacho_info;

#define TACHO_PRIVATE(item)    XV_PRIVATE(Tacho_info, Tacho_struct, item)

#define OUTER_RADIAL_PERCENT	.85
/*#define INNER_RADIAL_PERCENT	.23*/
#define INNER_RADIAL_PERCENT	.05
#define BASE_PERCENT		.03


#ifdef NO_SINCOS
#define SINCOS(_a,_b,_c)	_b = sin(_a); _c = cos(_a);
#else
#define SINCOS(_a,_b,_c)	sincos(_a, &_b, &_c)
#endif


/* ARGSUSED */
Pkg_private int
tacho_init(parent, tacho, avlist)
	Xv_opaque	parent;
	Tacho		tacho;
	Attr_avlist     avlist;
{
	Tacho_info	*tinfo;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(tacho);
	Tacho_struct	*tacho_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		tacho_paint_proc,
		rectobj_selection_event_proc,
		rectobj_map_event_proc,
		tacho_set_geometry_proc,
		NULL,			/* manage child */
		NULL,			/* add child */
		NULL,			/* del child */
	};

	tinfo = xv_alloc(Tacho_info);
	tacho_object = (Tacho_struct*) tacho;
	tacho_object->private_data = (Xv_opaque) tinfo;

	tinfo->min_value = 0;
	tinfo->max_value = 100;

	rinfo->rect.r_width = 50;
	rinfo->rect.r_height = 40;
	tinfo->arc_height = 40;
	tinfo->bg2 = -1;
	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	return XV_OK;
}


Pkg_private     Xv_opaque
tacho_set_avlist(tacho, avlist)
	Tacho tacho;
	register Attr_avlist avlist;
{
	register Drawobj_attr	attr;
	register Tacho_info	*tinfo = TACHO_PRIVATE(tacho);
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(tacho);
	short			range_changed = FALSE;
	short			value_changed = FALSE;
	int			value;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
			xv_super_set_avlist(tacho, &tacho_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(tacho);
			return(set_result);
		}
	}

	while (attr = (Drawobj_attr) * avlist++)
		switch (attr) {

		case TACHO_VALUE:
			value = *avlist++;
			value_changed = TRUE;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case TACHO_MIN_VALUE:
			range_changed = TRUE;
			tinfo->min_value = (int) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case TACHO_MAX_VALUE:
			range_changed = TRUE;
			tinfo->max_value = (int) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case RECTOBJ_BG2:
			tinfo->bg2 = (short) *avlist++;
			if(rinfo->shared_info &&
			  tinfo->bg2 >= rinfo->shared_info->num_colors)
				tinfo->bg2 = -1;
			break;

		case XV_END_CREATE:
			break;

		default:
			avlist = attr_skip(attr, avlist);
		}

	if (range_changed) {
		tinfo->value = MIN(tinfo->max_value, tinfo->value);
		tinfo->value = MAX(tinfo->min_value, tinfo->value);
	}
	if (value_changed) {
		if (value < tinfo->min_value)
			tinfo->value = tinfo->min_value;
		else if (value > tinfo->max_value)
			tinfo->value = tinfo->max_value;
		else
			tinfo->value = value;
	}

	if(rectobj_finish_set1(tacho))
		rectobj_finish_set2(tacho);

	return (XV_SET_DONE);
}


/* ARGSUSED */
Pkg_private     Xv_opaque
tacho_get_attr(tacho, status, which_attr, avlist)
	Tacho		tacho;
	int            *status;
	register Attr_attribute which_attr;
	va_list         avlist;
{
	Tacho_info     *tinfo = TACHO_PRIVATE(tacho);

	switch (which_attr) {
	case TACHO_VALUE:
		return (Xv_opaque) tinfo->value;
	case TACHO_MIN_VALUE:
		return (Xv_opaque) tinfo->min_value;
	case TACHO_MAX_VALUE:
		return (Xv_opaque) tinfo->max_value;

	default:
		*status = XV_ERROR;
		return (Xv_opaque) 0;
	}
}


/* ARGSUSED */
Pkg_private int
tacho_destroy(tacho, status)
	Tacho		tacho;
	Destroy_status	status;
{
	Tacho_info     *tinfo = TACHO_PRIVATE(tacho);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(tinfo);
	return XV_OK;
}


static void
tacho_decorate(tinfo, rinfo, dpy, xid, gc)
	Tacho_info     *tinfo;
	Rectobj_info   *rinfo;
	Display        *dpy;
	XID             xid;
	GC		gc;
{
	XSetForeground(dpy, gc, 
		pixel_bg(rinfo->shared_info, rinfo->bg_color));
	XFillArc(dpy, xid, gc,
		rinfo->rect.r_left+2, rinfo->rect.r_top,
		rinfo->rect.r_width-3, rinfo->rect.r_height*2-2,
		0 * 64, 180 * 64);
	XSetForeground(dpy, gc, 
		pixel_fg(rinfo->shared_info, rinfo->fg_color));
	XDrawArc(dpy, xid, gc,
		 rinfo->rect.r_left+1, rinfo->rect.r_top+1,
		 rinfo->rect.r_width-2, rinfo->rect.r_height*2-3,
		 0 * 64, 180 * 64);
	XDrawLine(dpy, xid, gc,
		rinfo->rect.r_left+1,
		rinfo->rect.r_top+rinfo->rect.r_height-1,
		rinfo->rect.r_left + rinfo->rect.r_width-2, 
		rinfo->rect.r_top+rinfo->rect.r_height-1);
}



static void
tacho_update_pointer(tinfo, rinfo, dpy, xid, gc, erase)
	Tacho_info     *tinfo;
	Rectobj_info   *rinfo;
	Display        *dpy;
	XID             xid;
	GC		gc;
	int             erase;
{

	double          sine, cosine;
	double          x, y;	/* cosine and sine scaled to size of rect */
	double          xmiddle, ymiddle;

	/* printf("tacho_update_pointer to %d\n", tinfo->value); */

	if (erase) {
		XSetForeground(dpy, gc, 
			pixel_bg(rinfo->shared_info, rinfo->bg_color));
		XFillPolygon(dpy, xid, gc, 
			tinfo->points, 3, Convex, CoordModeOrigin);
		XDrawLines(dpy, xid, gc, 
			tinfo->points, 4, CoordModeOrigin);
	}

	SINCOS( (double) (tinfo->value - tinfo->min_value) / 
		(double) (tinfo->max_value - tinfo->min_value) * M_PI,
	        sine, cosine);

	/*
	 * scale it to containing circle.
	 */
	x = -(cosine * rinfo->rect.r_width / 2);
	y = -(sine * rinfo->rect.r_height);

	/*
	 * calc tip of pointer
	 */
	tinfo->points[0].x = irint(x * OUTER_RADIAL_PERCENT + tinfo->centerx);
	tinfo->points[0].y = irint(y * OUTER_RADIAL_PERCENT + tinfo->centery);

	/*
	 * calc point on base of triangle that splits it
	 */
	xmiddle = x * INNER_RADIAL_PERCENT + tinfo->centerx;
	ymiddle = y * INNER_RADIAL_PERCENT + tinfo->centery;

	/*
	 * calc left and right of (xmiddle, ymiddle)
	 */
	tinfo->points[1].x = irint(xmiddle +
			 (sine * BASE_PERCENT * rinfo->rect.r_width / 2));
	tinfo->points[1].y = irint(ymiddle -
			  (cosine * BASE_PERCENT * tinfo->arc_height));

	tinfo->points[2].x = irint(xmiddle -
			 (sine * BASE_PERCENT * rinfo->rect.r_width / 2));
	tinfo->points[2].y = irint(ymiddle +
			  (cosine * BASE_PERCENT * tinfo->arc_height));

	tinfo->points[3].x = tinfo->points[0].x;
	tinfo->points[3].y = tinfo->points[0].y;
	/*tinfo->erase_pointer = TRUE;*/

	XSetForeground(dpy, gc, 
		pixel_bg2(rinfo->shared_info, tinfo->bg2));
	XFillPolygon(dpy, xid, gc, tinfo->points, 3, Convex, CoordModeOrigin);

	XSetForeground(dpy, gc, 
			pixel_fg(rinfo->shared_info, rinfo->fg_color));
	XDrawLines(dpy, xid, gc, tinfo->points, 4, CoordModeOrigin);
}


Pkg_private void
tacho_paint_proc(tacho, dpy, win, xrects)
	Tacho tacho;
	Display *dpy;
	Window win;
        Xv_xrectlist *xrects;
{
	Tacho_info	*tinfo = TACHO_PRIVATE(tacho);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(tacho);
	GC	gc;

	gc = XCreateGC(dpy, win, 0, 0);
	
	if(xrects && xrects->count)
		XSetClipRectangles(dpy, gc,
			0, 0,
			xrects->rect_array,
			xrects->count,
			Unsorted);

	tacho_decorate(tinfo, rinfo, dpy, win, gc);
	tacho_update_pointer(tinfo, rinfo, dpy, win, gc, FALSE);
	XFreeGC(dpy, gc);
	rectobj_paint_children(tacho, dpy, win, xrects);
}


/*ARGSUSED*/
void
tacho_set_geometry_proc(tacho, newrect, oldrect)
	Tacho tacho;
	Rect  *newrect;
	Rect  *oldrect;
{
	Tacho_info *tinfo = TACHO_PRIVATE(tacho);
	double extra_height;

	/*tinfo->erase_pointer = FALSE;*/
	tinfo->arc_height = newrect->r_height - 1;

	/*
	 * shave off some height because the base of the pointer actually
	 * swings below 0 and 180 degrees.
	 */
	extra_height = 1 + irint(
			      (double) (tinfo->arc_height * BASE_PERCENT));
	tinfo->arc_height -= extra_height;

	tinfo->centerx = (double) newrect->r_left + (double) newrect->r_width/2;
	tinfo->centery = newrect->r_top + tinfo->arc_height;
}

