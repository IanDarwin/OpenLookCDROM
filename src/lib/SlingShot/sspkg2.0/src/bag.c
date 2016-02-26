/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)bag.c 1.20 92/06/24";
#endif
#endif

#include "rectobj_impl.h"

#define BAG_NEW_CHILD RF_MISC_FLAG3

/*ARGSUSED*/
Pkg_private int
bag_init(parent, rectobj, avlist)
	Xv_opaque	parent;
	Rectobj		rectobj;
	Attr_avlist	avlist;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);

	static Rectobj_ops rectobj_ops = {
		1000,
		rectobj_paint_proc,
		rectobj_event_proc,
		rectobj_map_event_proc,
		rectobj_set_geometry_proc,
		bag_manage_child_proc,
		bag_add_child_proc,
		bag_del_child_proc,
	};

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;
	RF_SET(rinfo, BAG_ANCHORED_FLAG);

	return(XV_OK);
}


Pkg_private Xv_opaque
bag_set_avlist(bag, avlist)
	Bag			bag;
	register Attr_avlist    avlist;
{
	register Rectobj_attr	attr;
	register Rectobj_info	*rinfo = RECTOBJ_PRIVATE(bag);
	short 			old_border = rinfo->border;

	if (*avlist != XV_END_CREATE) {
		Xv_opaque       set_result;
		set_result = xv_super_set_avlist(bag, &bag_pkg, avlist);
		if (set_result != XV_OK) {
			rectobj_reset_set_info(bag);
			return (set_result);
		}
	}

	while (attr = (Rectobj_attr) * avlist++)
	  switch (attr) {
 
		case BAG_ANCHORED:
			bag_set_anchored(bag, *avlist);
			avlist++;
			break;

		case BAG_AUTO_SHRINK:
			if(*avlist++)
				RF_SET(rinfo, BAG_AUTO_SHRINK_FLAG);
			else
				RF_UNSET(rinfo, BAG_AUTO_SHRINK_FLAG);
			break;

		case RECTOBJ_BORDER:
			bag_set_border(bag, *avlist, old_border);
			avlist++;
			break;

		default:
			avlist = attr_skip(attr, avlist);
	}

	if(rectobj_finish_set1(bag))
		rectobj_finish_set2(bag);

	return (XV_SET_DONE);
}


/* ARGSUSED */
Pkg_private     Xv_opaque
bag_get_attr(bag, status, which_attr, avlist)
	Bag     	   bag;
	int            *status;
	register Attr_attribute which_attr;
	Attr_avlist     avlist;
{
	Rectobj_info   *rinfo = RECTOBJ_PRIVATE(bag);
 
	switch (which_attr) {
		case BAG_ANCHORED:
			return (Xv_opaque) RF_TRUE(rinfo, BAG_ANCHORED_FLAG);

		case BAG_AUTO_SHRINK:
			return (Xv_opaque) RF_TRUE(rinfo, BAG_AUTO_SHRINK_FLAG);

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}


/*ARGSUSED*/
Pkg_private int
bag_destroy(bag, status)
        Bag		bag;
        Destroy_status  status;
{
        return XV_OK;
}
 

static void
bag_calc_rect(rinfo, r)
	Rectobj_info	*rinfo;
	Rect		*r;
{
	int w, h;

	rectobj_min_enclosing_rect(rinfo->children, r);

	w = rinfo->rect.r_width;
	h = rinfo->rect.r_height;

	r->r_left -= rinfo->border;
	r->r_top -= rinfo->border;
	r->r_width += (rinfo->border*2);
	r->r_height += (rinfo->border*2);

	if(RF_IS_SET(rinfo, BAG_ANCHORED_FLAG)) {
		if(rinfo->rect.r_left < r->r_left) {
			r->r_width += (r->r_left - rinfo->rect.r_left);
			r->r_left = rinfo->rect.r_left;
		}
		if(rinfo->rect.r_top < r->r_top) {
			r->r_height += (r->r_top - rinfo->rect.r_top);
			r->r_top = rinfo->rect.r_top;
		}
	}

	if(!RF_IS_SET(rinfo, BAG_AUTO_SHRINK_FLAG)) {
		if(w > r->r_width)
			r->r_width = w;
		if(h > r->r_height)
			r->r_height = h;
	}

	rinfo->min_width = r->r_width;
	rinfo->min_height = r->r_height;
}


void
bag_set_anchored(bag, value)
	Bag	bag;
	int	value;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(bag);
	Rect 	r;

	if (value)
	    RF_SET(rinfo, BAG_ANCHORED_FLAG);
	else {
	    RF_UNSET(rinfo, BAG_ANCHORED_FLAG);
	    if(rinfo->children && RF_IS_SET(rinfo, BAG_AUTO_SHRINK_FLAG)) {
		bag_calc_rect(rinfo, &r);

		if(rectcmp(&r, &rinfo->rect)) {
			if(rectobj_geometry_manage(bag, &r))
				rectobj_delta_move_children(bag, 
					rinfo->rect.r_left - r.r_left,
					rinfo->rect.r_width - r.r_width);
		}
	    }
	}
}


void
bag_set_border(bag, border, old_border)
	Bag	bag;
	short	border;
	short	old_border;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(bag);
        Rect		r;
	short		delta_x;
	short		delta_y;

	if(rinfo->children) {
		r.r_left = rinfo->rect.r_left;
		r.r_top = rinfo->rect.r_top;
		r.r_width = rinfo->rect.r_width + (border - old_border)*2;
		r.r_height = rinfo->rect.r_height + (border - old_border)*2;

		rinfo->min_width += (border - old_border)*2;
		rinfo->min_height += (border - old_border)*2;

		delta_x = (rinfo->rect.r_left - rinfo->old_rect.r_left) +
			  (border - old_border);
		delta_y = (rinfo->rect.r_top - rinfo->old_rect.r_top) +
			  (border - old_border);
		(void) rectobj_geometry_manage(bag, &r);
		rectobj_delta_move_children(bag, delta_x, delta_y);
	} else {
		/* no children */
		rinfo->min_width =
		rinfo->min_height = border*2;

		if(RF_IS_SET(rinfo, BAG_AUTO_SHRINK_FLAG)) {
			rinfo->rect.r_width = 
			rinfo->rect.r_height = border*2;
		} else {
			if(border*2 > rinfo->rect.r_width)
				rinfo->rect.r_width = border*2;
			if(border*2 > rinfo->rect.r_height)
				rinfo->rect.r_height = border*2;
		}
	}
}


/* 
 * don't use xview's rect_right, rect_bottom because things are
 * relative and we don't need "-1" 
 */
#define RECT_RIGHT(_r_)		((_r_).r_left + (_r_).r_width)
#define RECT_BOTTOM(_r_)	((_r_).r_top + (_r_).r_height)
#define RECTP_RIGHT(_r_)	((_r_)->r_left + (_r_)->r_width)
#define RECTP_BOTTOM(_r_)	((_r_)->r_top + (_r_)->r_height)

void
bag_manage_child_proc(parent, child, child_new_rect, child_old_rect)
	Rectobj			parent;
	Rectobj			child;
	Rect			*child_new_rect;
	Rect			*child_old_rect;
{
	Rect	r;
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(parent);

	/* 
	 * 1) Anchored & newrect is outside parent (auto shrink doesnt matter)
	 *		Always expand.
	 *
	 * 2) Anchored & newrect is inside parent & auto shrink is false
	 *		No checking necessary.  no bag size change.
	 *
	 * 3) Anchored & newrect is inside parent
	 *		Check all children.  oldrect may be on edge.
	 *
	 * 4) UnAnchored without auto shrink
	 *
	 * 5) UnAnchored & auto shrink
	 * 		old rect may have been on edge, 
	 *		(easiest to) check all children
	 *
	 */

	if(RF_IS_SET(rinfo, BAG_NEW_CHILD)) {
		RF_UNSET(rinfo, BAG_NEW_CHILD);
		child_new_rect->r_left += rinfo->border;
		child_new_rect->r_top  += rinfo->border;
	} else {
		if(child_new_rect->r_left != child_old_rect->r_left)
			child_new_rect->r_left += rinfo->border;
		if(child_new_rect->r_top != child_old_rect->r_top)
			child_new_rect->r_top += rinfo->border;
	}

	if(RF_IS_SET(rinfo, BAG_ANCHORED_FLAG)) {
		short no_expand = FALSE;
		r = rinfo->rect;

		/* fast algorithm to expand to fit child */
		/* EAST */
		if((unsigned)RECTP_RIGHT(child_new_rect) > 
				RECT_RIGHT(rinfo->rect) - rinfo->border) {
			r.r_width += 
				(RECTP_RIGHT(child_new_rect) -
				RECT_RIGHT(rinfo->rect)) +
				rinfo->border;
			rinfo->min_width = r.r_width;
		} else
			no_expand = TRUE;


		/* SOUTH */
		if((unsigned)RECTP_BOTTOM(child_new_rect) > 
				RECT_BOTTOM(rinfo->rect) - rinfo->border) {
			r.r_height += 
				(RECTP_BOTTOM(child_new_rect) -
				RECT_BOTTOM(rinfo->rect)) +
				rinfo->border;
			rinfo->min_height = r.r_height;
		} else
			no_expand = TRUE;


		if(no_expand == TRUE)
			bag_calc_rect(rinfo, &r);

	} else {
		bag_calc_rect(rinfo, &r);
	}

	if(rectcmp(&r, &rinfo->rect)) {
		if(rectobj_geometry_manage(parent, &r))
			rectobj_delta_move_children(parent, 
				rinfo->rect.r_left - r.r_left,
				rinfo->rect.r_top - r.r_top);
	}
	rectobj_set_geometry(child, child_new_rect);
}


void
bag_add_child_proc(parent, child, rect)
	Rectobj parent;
	Rectobj child;
	Rect	*rect;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(parent);

	RF_SET(rinfo, BAG_NEW_CHILD);
}


void
bag_del_child_proc(parent, child, old_rect)
	Rectobj parent;
	Rectobj child;
	Rect	*old_rect;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(parent);
	Rect r;

	if(!RF_IS_SET(rinfo, BAG_AUTO_SHRINK_FLAG))
		return;

	if(rinfo->children) {
		bag_calc_rect(rinfo, &r);

		if(rectobj_geometry_manage(parent, &r))
			rectobj_delta_move_children(parent, 
				rinfo->rect.r_left - r.r_left,
				rinfo->rect.r_width - r.r_width);
	} else {
		/* no children */
		r.r_left = rinfo->rect.r_left;
		r.r_top = rinfo->rect.r_top;
		r.r_width =
		r.r_height =
		rinfo->min_width =
		rinfo->min_height = rinfo->border*2;
		(void) rectobj_geometry_manage(parent, &r);
	}
}


