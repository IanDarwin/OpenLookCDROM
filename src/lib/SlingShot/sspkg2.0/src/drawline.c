/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)drawline.c 1.21 92/10/19";
#endif
#endif

#include <math.h>
#include <X11/Xlib.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/win_input.h>
#include <sspkg/drawobj.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include <sspkg/canshell.h>


typedef struct {
	Arrow_style	style;
	int		length;
	int		inset_length;
	int		angle_degrees;	/* degrees * 64 */
	double		angle;		/* in radians */
	short		tip_ax, tip_ay;
	short		tip_bx, tip_by;
	short		interior_x, interior_y;
} Arrow_head;

typedef struct drawline_info {
	short		x[2], y[2];
	Arrow_head	arrow[2];
	short		rect_calculated;
} Drawline_info;

#define DRAWLINE_PRIVATE(drawline)	\
		XV_PRIVATE(Drawline_info, Drawline_struct, drawline)


#ifdef NO_SINCOS
#define SINCOS(_a,_b,_c)	_b = sin(_a); _c = cos(_a);
#else
#define SINCOS(_a,_b,_c)	sincos(_a, &_b, &_c)
#endif


Pkg_private int 	drawline_init();
Pkg_private Xv_opaque	drawline_set_avlist();
Pkg_private Xv_opaque	drawline_get_attr();
Pkg_private int 	drawline_destroy();

void	drawline_paint_proc();
void	drawline_set_geometry_proc();
void	drawline_calc_arrow_tips();
void	drawline_calc_rect();

Rectobj	drawline_map_event_proc();

#define DEG64_TO_RAD( degrees )	( (degrees) * M_PI / (360 * 64) )

/*ARGSUSED*/
Pkg_private int
drawline_init(parent, drawline, avlist)
	Xv_opaque	parent;
	Drawline		drawline;
	Attr_avlist	avlist;
{
	Drawline_info	*dinfo;
	Drawline_struct	*drawline_object;

	static Rectobj_ops rectobj_ops = {
		1000,
		drawline_paint_proc,
		rectobj_event_proc,
		drawline_map_event_proc,
		drawline_set_geometry_proc,
		NULL,			/* manage child */
		NULL,			/* add child */
		NULL,			/* del child */
	};


	dinfo = xv_alloc(Drawline_info);
	drawline_object = (Drawline_struct*) drawline;
	drawline_object->private_data = (Xv_opaque) dinfo;

	dinfo->arrow[0].style = ARROW_NONE;
	dinfo->arrow[0].length = 10;
	dinfo->arrow[0].inset_length = 7;

	dinfo->arrow[0].angle_degrees = 30*64;
	dinfo->arrow[0].angle = M_PI/6;

	dinfo->arrow[1] = dinfo->arrow[0];

	RECTOBJ_PRIVATE(drawline)->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	return(XV_OK);
}


Pkg_private Xv_opaque
drawline_set_avlist(drawline, avlist)
	Drawline		drawline;
	register Attr_avlist	avlist;
{
        register Drawobj_attr attr;
        register Drawline_info *dinfo = DRAWLINE_PRIVATE(drawline);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawline);
	Rectobj_info *parent_rinfo;
	register int	arg_int;
	int		state_changed = FALSE;
	int		end;
	int		before;
	Arrow_style	new_style;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
		    xv_super_set_avlist(drawline, &drawline_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(drawline);
			return(set_result);
		}
	}

	while (attr = (Drawobj_attr) * avlist++)
	  switch (attr) {

		case DRAWLINE_X0:
			arg_int = (int) *avlist++;
			before = dinfo->x[0];
			dinfo->x[0] = arg_int;
			if(before != arg_int)
				state_changed = TRUE;
			break;

		case DRAWLINE_Y0:
			arg_int = (int) *avlist++;
			before = dinfo->y[0];
			dinfo->y[0] = arg_int;
			if(before != arg_int)
				state_changed = TRUE;
			break;

		case DRAWLINE_X1:
			arg_int = (int) *avlist++;
			before = dinfo->x[1];
			dinfo->x[1] = arg_int;
			if(before != arg_int)
				state_changed = TRUE;
			break;

		case DRAWLINE_Y1:
			arg_int = (int) *avlist++;
			before = dinfo->y[1];
			dinfo->y[1] = arg_int;
			if(before != arg_int)
				state_changed = TRUE;
			break;

		case DRAWLINE_X:
			end = (int) *avlist++;
			arg_int = (int) *avlist++;

			/* add range checking */

			before = dinfo->x[end];

			if(!rinfo->parent)
				dinfo->x[end] = arg_int;
			else {
				parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
				dinfo->x[end] = 
					parent_rinfo->rect.r_left + arg_int;
			}
			if(before != arg_int)
				state_changed = TRUE;
			break;

		case DRAWLINE_Y:
			end = (int) *avlist++;
			arg_int = (int) *avlist++;

			/* add range checking */

			before = dinfo->y[end];

			if(!rinfo->parent)
				dinfo->y[end] = arg_int;
			else {
				parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
				dinfo->y[end] = 
					parent_rinfo->rect.r_top + arg_int;
			}
			if(before != arg_int)
				state_changed = TRUE;
			break;

		case DRAWLINE_ARROW_STYLE:
			end = (int) *avlist++;
			new_style = (Arrow_style) *avlist++;
			if((end != 0) && (end !=1))
				break;
			if(new_style != dinfo->arrow[end].style)
				state_changed = TRUE;
			dinfo->arrow[end].style = new_style;
			break;

		case DRAWLINE_ARROW_ANGLE:
			end = (int) *avlist++;
			arg_int = (int) *avlist++;
			if((end != 0) && (end !=1))
				break;
			if(arg_int != dinfo->arrow[end].angle_degrees)
				state_changed = TRUE;
			dinfo->arrow[end].angle_degrees = arg_int;
			dinfo->arrow[end].angle = DEG64_TO_RAD(arg_int);
			break;

		case DRAWLINE_ARROW_LENGTH:
			end = (int) *avlist++;
			arg_int = (int) *avlist++;
			if((end != 0) && (end !=1))
				break;
			if(arg_int != dinfo->arrow[end].length)
				state_changed = TRUE;
			dinfo->arrow[end].length = arg_int;
			break;

		case DRAWLINE_ARROW_INSET_LENGTH:
			end = (int) *avlist++;
			arg_int = (int) *avlist++;
			if((end != 0) && (end !=1))
				break;
			if(arg_int != dinfo->arrow[end].inset_length)
				state_changed = TRUE;
			dinfo->arrow[end].inset_length = arg_int;
			break;

		default:
			avlist = attr_skip(attr, avlist);

	  }

	if(state_changed) {
		drawline_calc_arrow_tips(&dinfo->arrow[0],
				dinfo->x[0], dinfo->y[0],
				dinfo->x[1], dinfo->y[1]);
		drawline_calc_arrow_tips(&dinfo->arrow[1],
				dinfo->x[1], dinfo->y[1],
				dinfo->x[0], dinfo->y[0]);
		drawline_calc_rect(dinfo, rinfo);
		dinfo->rect_calculated = TRUE;
		RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
	}

	if(rectobj_finish_set1(drawline)) {
		rectobj_finish_set2(drawline);
		dinfo->rect_calculated = FALSE;
	}

	return(XV_SET_DONE);
}


void
drawline_calc_arrow_tips(ahead, x0, y0, x1, y1)
	Arrow_head	*ahead;
	short		x0, y0;
	short		x1, y1;
{
	double	line_angle;
	double	sine, cosine;

	if(ahead->style == ARROW_NONE)
		return;

	if(((y1 - y0) == 0) && ((x1 -x0) == 0))
		line_angle = M_PI;
	else
		line_angle = atan2( (double)(y1 - y0), (double)(x1 - x0) );

	SINCOS(line_angle + ahead->angle, sine, cosine);
	ahead->tip_ax = cosine * ahead->length + x0;
	ahead->tip_ay = sine * ahead->length + y0;

	SINCOS(line_angle - ahead->angle, sine, cosine);
	ahead->tip_bx = cosine * ahead->length + x0;
	ahead->tip_by = sine * ahead->length + y0;

	if(ahead->style == ARROW_SIMPLE) {
		ahead->interior_x = x0;
		ahead->interior_y = y0;
	} else {
		SINCOS(line_angle, sine, cosine);
		ahead->interior_x = cosine * ahead->inset_length + x0;
		ahead->interior_y = sine * ahead->inset_length + y0;
	}
}


void
drawline_calc_rect(dinfo, rinfo)
	Drawline_info	*dinfo;
	Rectobj_info	*rinfo;
{
/* calculate the bounding box of a line, including arrow heads */
	short	xmin, ymin;
	short	xmax, ymax;

	xmin = MIN(dinfo->x[0], dinfo->x[1]);
	ymin = MIN(dinfo->y[0], dinfo->y[1]);
	xmax = MAX(dinfo->x[0], dinfo->x[1]);
	ymax = MAX(dinfo->y[0], dinfo->y[1]);

	if(dinfo->arrow[0].style != ARROW_NONE) {

		xmin =	MIN(dinfo->arrow[0].tip_ax, 
			MIN(dinfo->arrow[0].tip_bx, 
			MIN(dinfo->arrow[0].interior_x, 
				xmin)));

		ymin =	MIN(dinfo->arrow[0].tip_ay, 
			MIN(dinfo->arrow[0].tip_by, 
			MIN(dinfo->arrow[0].interior_y, 
				ymin)));

		xmax =	MAX(dinfo->arrow[0].tip_ax, 
			MAX(dinfo->arrow[0].tip_bx, 
			MAX(dinfo->arrow[0].interior_x, 
				xmax)));

		ymax =	MAX(dinfo->arrow[0].tip_ay, 
			MAX(dinfo->arrow[0].tip_by,
			MAX(dinfo->arrow[0].interior_y, 
				ymax)));
	}

	if(dinfo->arrow[1].style != ARROW_NONE) {

		xmin =	MIN(dinfo->arrow[1].tip_ax, 
			MIN(dinfo->arrow[1].tip_bx, 
			MIN(dinfo->arrow[1].interior_x, 
				xmin)));

		ymin =	MIN(dinfo->arrow[1].tip_ay, 
			MIN(dinfo->arrow[1].tip_by, 
			MIN(dinfo->arrow[1].interior_y, 
				ymin)));

		xmax =	MAX(dinfo->arrow[1].tip_ax, 
			MAX(dinfo->arrow[1].tip_bx, 
			MAX(dinfo->arrow[1].interior_x, 
				xmax)));

		ymax =	MAX(dinfo->arrow[1].tip_ay, 
			MAX(dinfo->arrow[1].tip_by,
			MAX(dinfo->arrow[1].interior_y, 
				ymax)));
	}

	rinfo->rect.r_left  = xmin;
	rinfo->rect.r_top   = ymin;
	/* zero width lines are nasty */
	rinfo->min_width =
	rinfo->rect.r_width = MAX((xmax - xmin)+1, 1);
	rinfo->min_height =
	rinfo->rect.r_height= MAX((ymax - ymin)+1, 1);
}


/*ARGSUSED*/
Pkg_private Xv_opaque
drawline_get_attr(drawline, status, which_attr, avlist)
	Drawline		drawline;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Drawline_info	*dinfo = DRAWLINE_PRIVATE(drawline);
	Rectobj_info	*rinfo;
	Rectobj_info	*parent_rinfo;
	int		end;
	short		parent_offset;

	switch (which_attr) {

		case DRAWLINE_X0:
			return (Xv_opaque) dinfo->x[0];

		case DRAWLINE_Y0:
			return (Xv_opaque) dinfo->y[0];

		case DRAWLINE_X1:
			return (Xv_opaque) dinfo->x[1];

		case DRAWLINE_Y1:
			return (Xv_opaque) dinfo->y[1];

		case DRAWLINE_X:
			end = (int)*avlist;
			if((end != 0) && (end != 1))
				break;

			rinfo = RECTOBJ_PRIVATE(drawline);
			if(!rinfo->parent)
				parent_offset = 0;
			else {
				parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
				parent_offset = parent_rinfo->rect.r_left;
			}
			return (Xv_opaque) dinfo->x[end] - parent_offset;

		case DRAWLINE_Y:
			end = (int)*avlist;
			if((end != 0) && (end != 1))
				break;

			rinfo = RECTOBJ_PRIVATE(drawline);
			if(!rinfo->parent)
				parent_offset = 0;
			else {
				parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
				parent_offset = parent_rinfo->rect.r_top;
			}
			return (Xv_opaque) dinfo->y[end] - parent_offset;

		case DRAWLINE_ARROW_STYLE:
			end = (int)*avlist;
			if((end != 0) && (end != 1))
				break;
			return (Xv_opaque)dinfo->arrow[end].style;

		case DRAWLINE_ARROW_ANGLE:
			end = (int)*avlist;
			if((end != 0) && (end != 1))
				break;
			return (Xv_opaque) dinfo->arrow[end].angle_degrees;

		case DRAWLINE_ARROW_LENGTH:
			end = (int)*avlist;
			if((end != 0) && (end != 1))
				break;
			return (Xv_opaque) dinfo->arrow[end].length;

		case DRAWLINE_ARROW_INSET_LENGTH:
			end = (int)*avlist;
			if((end != 0) && (end != 1))
				break;
			return (Xv_opaque) dinfo->arrow[end].inset_length;

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}


/*ARGSUSED*/
Pkg_private int
drawline_destroy(drawline, status)
	Drawline		drawline;
	Destroy_status	status;
{
	Drawline_info	*dinfo = DRAWLINE_PRIVATE(drawline);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(dinfo);
	return XV_OK;
}

void
drawline_paint_arrow(ahead, dpy, win, gc, x, y, fg, bg)
	Arrow_head	*ahead;
	Display		*dpy;
	Window		win;
	GC		gc;
	short		x, y;
{
	XPoint	point[5];

	point[0].x = ahead->tip_ax;
	point[0].y = ahead->tip_ay;

	point[1].x = x;
	point[1].y = y;

	point[2].x = ahead->tip_bx;
	point[2].y = ahead->tip_by;

	point[3].x = ahead->interior_x;
	point[3].y = ahead->interior_y;

	point[4] = point[0];

	if(ahead->style == ARROW_SIMPLE) {
		XDrawLines(dpy, win, gc, point, 3, CoordModeOrigin);
		return;
	}

	if(ahead->style == ARROW_HOLLOW) {
		XSetForeground(dpy, gc, bg);
		XFillPolygon(dpy, win, gc, point, 4,
				Nonconvex, CoordModeOrigin);
		XSetForeground(dpy, gc, fg);
		XDrawLines(dpy, win, gc, point, 5, CoordModeOrigin);
		return;
	}
	XDrawLines(dpy, win, gc, point, 5, CoordModeOrigin);
	XFillPolygon(dpy, win, gc, point, 5, Nonconvex, CoordModeOrigin);
}


/*ARGSUSED*/
Pkg_private void
drawline_paint_proc(drawline, dpy, win, xrects)
        Drawline drawline;
        Display *dpy;
        Window  win;
        Xv_xrectlist *xrects;
{
	Drawline_info *dinfo = DRAWLINE_PRIVATE(drawline);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawline);
	GC gc;

	gc = XCreateGC(dpy, win, 0, 0);

        if(xrects && xrects->count)
                XSetClipRectangles(dpy, gc,
                                0, 0,
                                xrects->rect_array,
                                xrects->count,
                                Unsorted);

	XSetForeground(dpy, gc, pixel_fg(rinfo->shared_info, rinfo->fg_color));

	XDrawLine(dpy, win, gc, dinfo->x[0], dinfo->y[0],
				dinfo->x[1], dinfo->y[1]);

	if(dinfo->arrow[0].style != ARROW_NONE)
		drawline_paint_arrow(&dinfo->arrow[0], dpy, win, gc, 
				dinfo->x[0], dinfo->y[0], 
				pixel_fg(rinfo->shared_info, rinfo->fg_color),
				pixel_bg(rinfo->shared_info, rinfo->bg_color));

	if(dinfo->arrow[1].style != ARROW_NONE)
		drawline_paint_arrow(&dinfo->arrow[1], dpy, win, gc,
				dinfo->x[1], dinfo->y[1],
				pixel_fg(rinfo->shared_info, rinfo->fg_color),
				pixel_bg(rinfo->shared_info, rinfo->bg_color));

	XFreeGC(dpy, gc);

        rectobj_paint_children(drawline, dpy, win, xrects);
}


#ifdef DRAWLINE_HIT_DETECTION
static int 
PntOnLine(px,py,qx,qy,tx,ty)
	int px, py, qx, qy, tx, ty;
{
#define ABS(a)          (((a)<0) ? -(a) : (a))
	/* 
	 * Adapted from:
	 * A Fast 2D Point-On-Line Test
	 * by Alan Paeth
	 * from "Graphics Gems", Academic Press, 1990
	*/
	/*
	 * given a line through P:(px,py) Q:(qx,qy) and T:(tx,ty)
	 * return 0 if T is not on the line through      <--P--Q-->
	 *        1 if T is on the open ray ending at P: <--P
	 *        2 if T is on the closed interior along:   P--Q
	 *        3 if T is on the open ray beginning at Q:    Q-->
	 *
	 * Example: consider the line P = (3,2), Q = (17,7). A plot
	 * of the test points T(x,y) (with 0 mapped onto '.') yields:
	 *
	 *     8| . . . . . . . . . . . . . . . . . 3 3
	 *  Y  7| . . . . . . . . . . . . . . 2 2 Q 3 3    Q = 2
	 *     6| . . . . . . . . . . . 2 2 2 2 2 . . .
	 *  a  5| . . . . . . . . 2 2 2 2 2 2 . . . . .
	 *  x  4| . . . . . 2 2 2 2 2 2 . . . . . . . .
	 *  i  3| . . . 2 2 2 2 2 . . . . . . . . . . .
	 *  s  2| 1 1 P 2 2 . . . . . . . . . . . . . .    P = 2
	 *     1| 1 1 . . . . . . . . . . . . . . . . .
	 *      +--------------------------------------
	 *        1 2 3 4 5 X-axis 10        15      19
	 *
	 * Point-Line distance is normalized with the Infinity Norm
	 * avoiding square-root code and tightening the test vs the
	 * Manhattan Norm. All math is done on the field of integers.
	 * The latter replaces the initial ">= MAX(...)" test with
	 * "> (ABS(qx-px) + ABS(qy-py))" loosening both inequality
	 * and norm, yielding a broader target line for selection.
	 * The tightest test is employed here for best discrimination
	 * in merging collinear (to grid coordinates) vertex chains
	 * into a larger, spanning vectors within the Lemming editor.
	 */

	if ( ABS((qy-py)*(tx-px)-(ty-py)*(qx-px)) >=
		(MAX(ABS(qx-px), ABS(qy-py)))) 		return(0);
	if (((qx<px)&&(px<tx)) || ((qy<py)&&(py<ty)))	return(1);
	if (((tx<px)&&(px<qx)) || ((ty<py)&&(py<qy)))	return(1);
	if (((px<qx)&&(qx<tx)) || ((py<qy)&&(qy<ty)))	return(3);
	if (((tx<qx)&&(qx<px)) || ((ty<qy)&&(qy<py)))	return(3);
	return(2);
}
#endif

Rectobj	
drawline_map_event_proc(drawline, event)
	Drawline	drawline;
	Event		*event;
{
	Drawline_info	*dinfo = DRAWLINE_PRIVATE(drawline);

#ifdef DRAWLINE_HIT_DETECTION
	if(PntOnLine(	dinfo->x[0], dinfo->y[0],
			dinfo->x[1], dinfo->y[1],
			event_x(event), event_y(event)))
		return (Rectobj) drawline;
#endif
	return (Rectobj) 0;
}

void
drawline_set_geometry_proc(drawline, newrect, oldrect)
	Rectobj         drawline;
	Rect           *newrect;
	Rect           *oldrect;
{

	/*
	 * Following needs to be done if the geo change did not originate 
	 * from xv_set on this object.
	 */

	Drawline_info  *dinfo = DRAWLINE_PRIVATE(drawline);
	int             delta_x = newrect->r_left - oldrect->r_left;
	int             delta_y = newrect->r_top - oldrect->r_top;

	if(dinfo->rect_calculated == TRUE)
		return;

	dinfo->x[0] += delta_x;
	dinfo->y[0] += delta_y;

	dinfo->x[1] += delta_x;
	dinfo->y[1] += delta_y;

	dinfo->arrow[0].tip_ax += delta_x;
	dinfo->arrow[0].tip_ay += delta_y;
	dinfo->arrow[0].tip_bx += delta_x;
	dinfo->arrow[0].tip_by += delta_y;
	dinfo->arrow[0].interior_x += delta_x;
	dinfo->arrow[0].interior_y += delta_y;

	dinfo->arrow[1].tip_ax += delta_x;
	dinfo->arrow[1].tip_ay += delta_y;
	dinfo->arrow[1].tip_bx += delta_x;
	dinfo->arrow[1].tip_by += delta_y;
	dinfo->arrow[1].interior_x += delta_x;
	dinfo->arrow[1].interior_y += delta_y;

}


