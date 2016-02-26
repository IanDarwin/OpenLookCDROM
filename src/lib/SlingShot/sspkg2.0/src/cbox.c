/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)cbox.c 1.11 92/05/06";
#endif
#endif

#include "rectobj_impl.h"
#include "box_impl.h"


	void	cbox_manage_child_proc();
	void	cbox_set_geometry_proc();
static	void	cbox_set_children_rects();

/*ARGSUSED*/
Pkg_private int
cbox_init(parent, rectobj, avlist)
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
		cbox_set_geometry_proc,
		cbox_manage_child_proc,
		NULL, /* add child */
		NULL, /* cbox del child */
	};

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	return(XV_OK);
}


Pkg_private Xv_opaque
cbox_set_avlist(cbox, avlist)
	Cbox			cbox;
	register Attr_avlist    avlist;
{
	register Rectobj_attr	attr;
	int			relayout = FALSE;
	int			old_border = RECTOBJ_PRIVATE(cbox)->border;

	if (*avlist != XV_END_CREATE) {
		Xv_opaque       set_result;
		set_result = xv_super_set_avlist(cbox, &cbox_pkg, avlist);
		if (set_result != XV_OK) {
			rectobj_reset_set_info(cbox);
			return (set_result);
		}
	}

	while (attr = (Rectobj_attr) * avlist++)
	  switch (attr) {

		case RECTOBJ_BORDER:
			relayout = TRUE;
			avlist++;
			break;

		default:
			avlist = attr_skip(attr, avlist);
		}


	if(rectobj_finish_set1(cbox)) {
		if(relayout) {
			Rectobj_info *rinfo = RECTOBJ_PRIVATE(cbox);

			rinfo->rect.r_width += (rinfo->border - old_border) *2;
			rinfo->rect.r_height += (rinfo->border - old_border) *2;
			rectobj_geometry_manage(cbox, &rinfo->rect);
			cbox_set_children_rects(rinfo);
		}
		rectobj_finish_set2(cbox);
	}

	return (XV_SET_DONE);
}


/* ARGSUSED */
Pkg_private     Xv_opaque
cbox_get_attr(cbox, status, which_attr, avlist)
	Cbox		cbox;
	int            *status;
	register Attr_attribute which_attr;
	Attr_avlist     avlist;
{
	*status = XV_ERROR;
	return (Xv_opaque) 0;
}


/*ARGSUSED*/
Pkg_private int
cbox_destroy(cbox, status)
        Cbox		cbox;
        Destroy_status  status;
{
        return XV_OK;
}
 

static void
cbox_set_children_rects(rinfo)
	Rectobj_info *rinfo;
{
	Rectobj_list	*list;
	Rectobj_info	*child_rinfo;
	Rectobj		child;
	Rect		*r;
	int		b2;

	list = rinfo->children;
	r = &rinfo->rect;
	b2 = rinfo->border + rinfo->border;

	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		child_rinfo = RECTOBJ_PRIVATE(child);

		/* calc child rect */
		if(RF_IS_SET(child_rinfo, RF_RESIZABLE)) {
			child_rinfo->rect.r_left = r->r_left + rinfo->border;
			child_rinfo->rect.r_top= r->r_top + rinfo->border;
			child_rinfo->rect.r_width = r->r_width - b2;
			child_rinfo->rect.r_height = r->r_height - b2;
		} else {
			child_rinfo->rect.r_left = 
				r->r_left + r->r_width/2 - 
				child_rinfo->rect.r_width/2;
			child_rinfo->rect.r_top = 
				r->r_top + r->r_height/2 - 
				child_rinfo->rect.r_height/2;
		}
		rectobj_set_geometry(child, &child_rinfo->rect);
	}
}


void
cbox_manage_child_proc(cbox, child, child_new_rect, child_old_rect)
	Rectobj			cbox;
	Rectobj			child;
	Rect			*child_new_rect;
	Rect			*child_old_rect;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(cbox);
	Rect		r;
	int		b2;

	r = rinfo->rect;
	b2 = rinfo->border + rinfo->border;
	if(child_new_rect->r_width > r.r_width - b2 ||
	  child_new_rect->r_height > r.r_height - b2) {
		/* child is larger than parent. */

		if(child_new_rect->r_width > r.r_width - b2)
			r.r_width = child_new_rect->r_width + b2;

		if(child_new_rect->r_height > r.r_height - b2)
			r.r_height = child_new_rect->r_height + b2;

		(void) rectobj_geometry_manage(cbox, &r);
		cbox_set_children_rects(rinfo);
	} else {
		/* calc child rect */
		if(RF_IS_SET(RECTOBJ_PRIVATE(child), RF_RESIZABLE)) {
			child_new_rect->r_left = r.r_left + rinfo->border;
			child_new_rect->r_top= r.r_top + rinfo->border;
			child_new_rect->r_width = r.r_width - b2;
			child_new_rect->r_height = r.r_height - b2;
		} else {
			child_new_rect->r_left = 
				r.r_left + r.r_width/2 - 
				child_new_rect->r_width/2;
			child_new_rect->r_top = 
				r.r_top + r.r_height/2 - 
				child_new_rect->r_height/2;
		}
		rectobj_set_geometry(child, child_new_rect);
	}
}


/* ARGSUSED */
void
cbox_set_geometry_proc(cbox, newrect, oldrect)
	Rectobj	cbox;
	Rect	*newrect;
	Rect	*oldrect;
{
	if((newrect->r_width == oldrect->r_width) &&
	   (newrect->r_height == oldrect->r_height)) {
		rectobj_move_children(cbox);
		return;
	} else {
		cbox_set_children_rects(RECTOBJ_PRIVATE(cbox));
	}
}


