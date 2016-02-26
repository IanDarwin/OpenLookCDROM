/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char	sccsid[] = "@(#)clock.c 1.23 92/10/30";
#endif
#endif


#include <math.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/disp_list.h>
#include <sspkg/grip.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"

Pkg_private int 	clockobj_init();
Pkg_private Xv_opaque	clockobj_set_avlist();
Pkg_private Xv_opaque	clockobj_get_attr();
Pkg_private int 	clockobj_destroy();

#ifdef NO_SINCOS
#define SINCOS(_a,_b,_c)	_b = sin(_a); _c = cos(_a);
#else
#define SINCOS(_a,_b,_c)	sincos(_a, &_b, &_c)
#endif

typedef struct {
	Drawarea        hr_hand;
	Drawarea        min_hand;

	int             hr;
	int             min;

	short		hr_color;
	short		min_color;

	char		_24hour;
	char		movable;
	Proc_ptr	move_proc;
} Clockobj_info;

static	void	set_hr();
static	void	set_min();
	void	clockobj_set_geometry_proc();
	void	clockobj_start_drag_proc();

#define CLOCKOBJ_PRIVATE(obj)    XV_PRIVATE(Clockobj_info, Clockobj_struct, obj)


/* ARGSUSED */
Pkg_private int
clockobj_init(parent, clockobj, avlist)
	Xv_opaque	parent;
	Clockobj	clockobj;
	Attr_avlist     avlist;
{
	Clockobj_info	*cinfo;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(clockobj);
	Clockobj_struct	*clockobj_object;
	extern	void	drawarea_paint_proc();
	static Rectobj_ops rectobj_ops = {
		1000,
		drawarea_paint_proc,
		rectobj_selection_event_proc,
		rectobj_map_event_proc,
		clockobj_set_geometry_proc,
		NULL,			/* manage child */
		NULL,			/* add child */
		NULL,			/* del child */
		NULL,			/* new parent */
	};
	static double min_scale = -1;
	static double max_scale = 1;
	static Rectobj_ops handops;
	static int init = FALSE;

	cinfo = xv_alloc(Clockobj_info);
	clockobj_object = (Clockobj_struct*) clockobj;
	clockobj_object->private_data = (Xv_opaque) cinfo;

	VDrawArc(clockobj, 0, 0, 10000, 10000, 0, 360*64);

	if(!init) {
		Drawarea tmp;
		init = TRUE;
		tmp = xv_create(XV_NULL, DRAWAREA, NULL);
		handops = *(Rectobj_ops *) xv_get(tmp, RECTOBJ_OPS);
		handops.start_drag_proc = clockobj_start_drag_proc;
		xv_destroy(tmp);
	}

	cinfo->min_hand = (Drawarea) xv_create(clockobj, DRAWAREA,
                        DRAWAREA_LEFT_X, &min_scale,
                        DRAWAREA_RIGHT_X, &max_scale,
                        DRAWAREA_UPPER_Y, &min_scale,
                        DRAWAREA_LOWER_Y, &max_scale,
			RECTOBJ_OPS, &handops,
                        NULL);

	cinfo->hr_hand = (Drawarea) xv_create(clockobj, DRAWAREA,
                        DRAWAREA_LEFT_X, &min_scale,
                        DRAWAREA_RIGHT_X, &max_scale,
                        DRAWAREA_UPPER_Y, &min_scale,
                        DRAWAREA_LOWER_Y, &max_scale,
			RECTOBJ_START_DRAG_PROC, clockobj_start_drag_proc,
			RECTOBJ_OPS, &handops,
                        NULL);

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;
	RF_UNSET(rinfo, RF_RESTACK_CHILDREN);
	return XV_OK;
}


Pkg_private     Xv_opaque
clockobj_set_avlist(clockobj, avlist)
	Clockobj clockobj;
	register Attr_avlist avlist;
{
	register Drawobj_attr	attr;
	register Clockobj_info	*cinfo = CLOCKOBJ_PRIVATE(clockobj);
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(clockobj);
	extern int rectobj_global_invocation_level;

	if(rinfo->invocation_level == 0)
		rectobj_reset_set_info(clockobj);
	rinfo->invocation_level++;
	rectobj_global_invocation_level++;

	if (*avlist != XV_END_CREATE) {
		Xv_opaque set_result = 
			xv_super_set_avlist(clockobj, &clockobj_pkg, avlist);
		if (set_result != XV_OK)
			return (set_result);
	}

	while (attr = (Drawobj_attr) * avlist++)
		switch (attr) {

		case CLOCKOBJ_HR:
			if(cinfo->hr != (int) *avlist) {
				cinfo->hr = (int) *avlist;
				if(cinfo->hr > (cinfo->_24hour?24:12))
					cinfo->hr = (cinfo->_24hour?24:12);
				if(cinfo->hr <= 0)
					cinfo->hr = 12;
				set_hr(cinfo);
				RF_SET(rinfo, RF_REPAINT);
			}
			avlist++;
			break;

		case CLOCKOBJ_MIN:
			if(cinfo->min != (int) *avlist) {
				cinfo->min = (int) *avlist;
				if(cinfo->min > 59) 
					cinfo->min = 59;
				if(cinfo->min < 0)
					cinfo->min = 0;
				set_min(cinfo);
				RF_SET(rinfo, RF_REPAINT);
			}
			avlist++;
			break;

		case CLOCKOBJ_24_HOUR:
			cinfo->_24hour = (char) *avlist++;
			break;

		case CLOCKOBJ_HR_DRAWAREA:
		case CLOCKOBJ_MIN_DRAWAREA:
			/* error, cannot set these */
			break;

		case CLOCKOBJ_MOVABLE:
			if(cinfo->movable = (char) *avlist++) {
			  xv_set(cinfo->hr_hand,
				DRAWAREA_MAP_EVENTS, DRAWAREA_MAP_FIRST,
				NULL);
			  xv_set(cinfo->min_hand,
				DRAWAREA_MAP_EVENTS, DRAWAREA_MAP_FIRST,
				NULL);
			} else {
			  xv_set(cinfo->hr_hand,
				DRAWAREA_MAP_EVENTS, DRAWAREA_MAP_ANY,
				NULL);
			  xv_set(cinfo->min_hand,
				DRAWAREA_MAP_EVENTS, DRAWAREA_MAP_ANY,
				NULL);
			}
			break;

		case CLOCKOBJ_MOVE_PROC:
			cinfo->move_proc = (Proc_ptr)*avlist++;
			break;

		case XV_END_CREATE:
			/*
			 * need to set these here because shared_info is not
			 * correct at time of init function.
			 */
			set_hr(cinfo);
			set_min(cinfo);
			break;

		default:
			avlist = attr_skip(attr, avlist);
		}

	if(rectobj_finish_set1(clockobj))
		rectobj_finish_set2(clockobj);

	return (XV_SET_DONE);
}


/* ARGSUSED */
Pkg_private     Xv_opaque
clockobj_get_attr(clockobj, status, which_attr, avlist)
	Clockobj	clockobj;
	int            *status;
	register Attr_attribute which_attr;
	va_list         avlist;
{
	Clockobj_info     *cinfo = CLOCKOBJ_PRIVATE(clockobj);

	switch (which_attr) {

	case CLOCKOBJ_HR:
		return (Xv_opaque) cinfo->hr;
	case CLOCKOBJ_MIN:
		return (Xv_opaque) cinfo->min;
	case CLOCKOBJ_24_HOUR:
		return (Xv_opaque) cinfo->_24hour;
	case CLOCKOBJ_HR_DRAWAREA:
		return (Xv_opaque) cinfo->hr_hand;
	case CLOCKOBJ_MIN_DRAWAREA:
		return (Xv_opaque) cinfo->min_hand;
	case CLOCKOBJ_MOVABLE:
		return (Xv_opaque) cinfo->movable;
	case CLOCKOBJ_MOVE_PROC:
		return (Xv_opaque) cinfo->move_proc;

	default:
		*status = XV_ERROR;
		return (Xv_opaque) 0;
	}
}


/* ARGSUSED */
Pkg_private int
clockobj_destroy(clockobj, status)
	Clockobj	clockobj;
	Destroy_status	status;
{
	Clockobj_info     *cinfo = CLOCKOBJ_PRIVATE(clockobj);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(cinfo);
	return XV_OK;
}


static void
calc_points_for_angle(angle, points)
	double          angle;
	DPoint		points[];
{
	double          sine, cosine;
	double          leg_sine, leg_cosine;

/*
	SINCOS(angle, sine, cosine);
	points[0].x = sine;
	points[0].y = cosine;

	SINCOS(angle + M_PI * 3 / 4, sine, cosine);
	points[1].x = sine / 5;
	points[1].y = cosine / 5;

	SINCOS(angle - M_PI * 3 / 4, sine, cosine);
	points[2].x = sine / 5;
	points[2].y = cosine / 5;
*/

	SINCOS(angle, sine, cosine);
	points[0].x = sine;
	points[0].y = cosine;

	SINCOS(angle + M_PI * 7 / 8, leg_sine, leg_cosine);
	points[1].x = sine + leg_sine / 5;
	points[1].y = cosine + leg_cosine / 5;

	points[2].x = leg_sine / 10;
	points[2].y = leg_cosine / 10;

	SINCOS(angle - M_PI * 7 / 8, leg_sine, leg_cosine);
	points[3].x = leg_sine / 10;
	points[3].y = leg_cosine / 10;

	points[4].x = sine + leg_sine / 5;
	points[4].y = cosine + leg_cosine / 5;
}

static void
set_hr(cinfo)
	Clockobj_info	*cinfo;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(cinfo->hr_hand);
        DPoint		points[5];

	if(cinfo->_24hour)
		calc_points_for_angle(
			-M_PI*(	(double)cinfo->hr / (double)12 +
	 			(double)cinfo->min / (double)(30*12)) - M_PI,
			points);
	else
		calc_points_for_angle( 
			-M_PI*(	(double)cinfo->hr / (double)6 +
	 			(double)cinfo->min / (double)(30*12)) - M_PI,
			points);

	VClear(cinfo->hr_hand);
	if((rinfo->bg_color == -1) && (rinfo->shared_info))
		VSetColor(cinfo->hr_hand, rinfo->shared_info->win_bg);
	else
		VSetColor(cinfo->hr_hand, rinfo->bg_color);
	DFillPoly(cinfo->hr_hand, points, 5);
	VSetColor(cinfo->hr_hand, rinfo->fg_color);
	DDrawPoly(cinfo->hr_hand, points, 5);
}

static void
set_min(cinfo)
	Clockobj_info	*cinfo;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(cinfo->min_hand);
	DPoint		points[5];

	calc_points_for_angle( -M_PI*(double)cinfo->min/(double)30-M_PI,
				points);
	VClear(cinfo->min_hand);
	if((rinfo->bg_color == -1) && (rinfo->shared_info))
		VSetColor(cinfo->min_hand, rinfo->shared_info->win_bg);
	else
		VSetColor(cinfo->min_hand, rinfo->bg_color);

	DFillPoly(cinfo->min_hand, points, 5);
	VSetColor(cinfo->min_hand, rinfo->fg_color);
	DDrawPoly(cinfo->min_hand, points, 5);
	set_hr(cinfo);
}

void
clockobj_set_geometry_proc(clockobj, newrect, oldrect)
	Clockobj	clockobj;
        Rect            *newrect;
        Rect            *oldrect;
{
	Clockobj_info     *cinfo = CLOCKOBJ_PRIVATE(clockobj);
	Rect r;

	r.r_left = newrect->r_left+newrect->r_width/8;
	r.r_top = newrect->r_top+newrect->r_height/8;
	r.r_width = newrect->r_width - newrect->r_width/4;
	r.r_height = newrect->r_height - newrect->r_height/4;
        rectobj_set_geometry(cinfo->hr_hand, &r);

	r.r_left = newrect->r_left+4;
	r.r_top = newrect->r_top+4;
	r.r_width = newrect->r_width-8;
	r.r_height = newrect->r_height-8;
        rectobj_set_geometry(cinfo->min_hand, &r);
}


static Clockobj tmp_clockobj;
static Drawarea tmp_hand;

static int
clockobj_move_hand_proc(paint_window, event, canvas_shell, grip, new_x, new_y)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
	short		*new_x;
	short		*new_y;
{
	Clockobj_info *cinfo = CLOCKOBJ_PRIVATE(tmp_clockobj);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(tmp_hand);
	int y, x;
	double tmp, arctangent;
	int hour, min;

	x = event_x(event) - 
		(rinfo->rect.r_left + rinfo->rect.r_width/2);
	y = event_y(event) - 
		(rinfo->rect.r_top + rinfo->rect.r_height/2);

	if(x<0)
		arctangent = atan((double)y/(double)x)+M_PI;
	else
	if(x>0)
		arctangent = atan((double)y/(double)x);
	else
	if(x==0) {
		if(y==0)
			return FALSE;
		if(y<0)
			arctangent = M_PI*3/2;
		else
			arctangent = M_PI/2;
	}
	arctangent += M_PI/2; /* make it run from 0 to 2 pi */

	if(cinfo->hr_hand == tmp_hand) {
		/* moving hour hand */
		hour = (int)((arctangent+M_PI/12)*6/M_PI);
		min = ((arctangent-(double)hour*2*M_PI/12)*120);
		if(min<0) {
			hour--;
			min += 60;
		}
	} else {
		/* moving minute hand */
		min = (int)((arctangent+M_PI/60)*29.5/M_PI);
		hour = cinfo->hr;
		if(cinfo->min > 45 && min < 15)
			hour = (hour==12) ? 1 : hour+1;
		else
		if(cinfo->min < 15 && min > 45)
			hour--;
	}
	if(hour == 0) 
		hour = 12;

	if(cinfo->move_proc && (cinfo->hr != hour || cinfo->min != min))
		(cinfo->move_proc)(tmp_clockobj, hour, min, FALSE);

	if(cinfo->hr != hour || cinfo->min != min)
		xv_set(tmp_clockobj,
			CLOCKOBJ_HR, hour,
			CLOCKOBJ_MIN, min, 
			NULL);
	/*
	 * Don't care about update grip position, so return false.
	 * Also prevents repaint on every event.
	 */
	return FALSE;
}

static void
clockobj_move_done(paint_window, event, canvas_shell, grip, last_x, last_y)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		grip;
	int		last_x;
	int		last_y;
{
	Clockobj_info *cinfo = CLOCKOBJ_PRIVATE(tmp_clockobj);

	if(cinfo->move_proc)
		(cinfo->move_proc)(tmp_clockobj, cinfo->hr, cinfo->min, TRUE);
}

void
clockobj_start_drag_proc(paint_window, event, canvas_shell, clockhand, btn_down_x, btn_down_y, adjust)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip		clockhand;
	int		btn_down_x;
	int		btn_down_y;
	int		adjust;
{
	Clockobj_info *cinfo;

	tmp_clockobj = 	xv_get(clockhand, RECTOBJ_PARENT);
	cinfo = CLOCKOBJ_PRIVATE(tmp_clockobj);
	if(!cinfo->movable)
		return;
	tmp_hand = clockhand;
	(void) xv_create(clockhand, TEMP_GRIP,
		GRIP_EXCEED_PARENT_DIMS, TRUE, /* make sure we get all events */
		GRIP_MOVE_PROC, clockobj_move_hand_proc,
		GRIP_DONE_PROC, clockobj_move_done,
		NULL);
}

