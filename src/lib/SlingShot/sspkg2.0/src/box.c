/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)box.c 1.13 92/05/15";
#endif
#endif

#include "rectobj_impl.h"
#include "box_impl.h"

	void	box_manage_child_proc();
	void	box_set_geometry_proc();
	void	box_add_child_proc();
	void	box_del_child_proc();

typedef struct {
	Rectobj_list	*children;
	Box_layout	layout;
	unsigned short	gap;
} Box_info;


#define BOX_PRIVATE(box)    XV_PRIVATE(Box_info, Box_struct, box)
#define get_layout_data(_child_) \
		(Rectobj_list*) RECTOBJ_PRIVATE(_child_)->layout_data

static	void	box_set_children_rects();
static	void	box_calc_rect();

/*ARGSUSED*/
Pkg_private int
box_init(parent, rectobj, avlist)
	Xv_opaque	parent;
	Rectobj		rectobj;
	Attr_avlist	avlist;
{
	Rectobj_info 	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Box_info 	*binfo;
	Box_struct	*box_object;

	static Rectobj_ops rectobj_ops = {
		1000,
		rectobj_paint_proc,
		rectobj_event_proc,
		rectobj_map_event_proc,
		box_set_geometry_proc,
		box_manage_child_proc,
		box_add_child_proc,
		box_del_child_proc,
	};

	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	binfo = xv_alloc(Box_info);
	binfo->gap = 1;
	binfo->layout = BOX_LAYOUT_HORIZONTAL;
	box_object = (Box_struct*) rectobj;
	box_object->private_data = (Xv_opaque) binfo;

	return(XV_OK);
}


Pkg_private Xv_opaque
box_set_avlist(box, avlist)
	Box			box;
	register Attr_avlist    avlist;
{
	register Rectobj_attr	attr;
	Box_info		*binfo = BOX_PRIVATE(box);
	Rectobj_info		*rinfo = RECTOBJ_PRIVATE(box);
	int			relayout = FALSE;

	if (*avlist != XV_END_CREATE) {
		Xv_opaque       set_result;
		set_result = xv_super_set_avlist(box, &box_pkg, avlist);
		if (set_result != XV_OK) {
			rectobj_reset_set_info(box);
			return (set_result);
		}
	}

	while (attr = (Rectobj_attr) * avlist++)
	  switch (attr) {
 
		case RECTOBJ_BORDER:
			relayout = TRUE;
			avlist++;
			break;

		case BOX_GAP:
			relayout = TRUE;
			binfo->gap = (short) *avlist++;
			break;

		case BOX_LAYOUT:
			relayout = TRUE;
			binfo->layout = (Box_layout) *avlist++;
			break;

		case XV_END_CREATE:
			box_calc_rect(rinfo, binfo, &rinfo->rect);
			break;

		default:
			avlist = attr_skip(attr, avlist);
	}

	if(rectobj_finish_set1(box)) {
		if(relayout) {
			/* what about create? */
 
			box_calc_rect(rinfo, binfo, &rinfo->rect);
			rectobj_geometry_manage(box, &rinfo->rect);
			box_set_children_rects(rinfo, binfo);
		}
		rectobj_finish_set2(box);
	}

	return (XV_SET_DONE);
}


/* ARGSUSED */
Pkg_private     Xv_opaque
box_get_attr(box, status, which_attr, avlist)
	Box		box;
	int            *status;
	register Attr_attribute which_attr;
	Attr_avlist     avlist;
{
	switch (which_attr) {

		case BOX_GAP:
			return (Xv_opaque) BOX_PRIVATE(box)->gap;

		case BOX_LAYOUT:
			return (Xv_opaque) BOX_PRIVATE(box)->layout;

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}


/*ARGSUSED*/
Pkg_private int
box_destroy(box, status)
        Box		box;
        Destroy_status  status;
{
	Box_info *binfo = BOX_PRIVATE(box);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	rectobj_destroy_children(box);
	free(binfo);

        return XV_OK;
}
 

static void
box_set_children_rects(rinfo, binfo)
	Rectobj_info	*rinfo;
	Box_info	*binfo;
{
	Listnode	*list;
	int		offset;
	Rectobj		child;
	Rect		*rect;

	offset = rinfo->border;
	offset += (binfo->layout == BOX_LAYOUT_HORIZONTAL ? 
			rinfo->rect.r_left :
			rinfo->rect.r_top);

	list = (Listnode*) binfo->children;
	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		rect = &RECTOBJ_PRIVATE(child)->rect;
		if(binfo->layout == BOX_LAYOUT_HORIZONTAL) {
			rect->r_left = offset;
			rect->r_top = rinfo->rect.r_top +
				rinfo->rect.r_height/2 - rect->r_height/2;
			offset += rect->r_width;
		} else {
			rect->r_top = offset;
			rect->r_left = rinfo->rect.r_left +
				rinfo->rect.r_width/2 - rect->r_width/2;
			offset += rect->r_height;
		}

		rectobj_set_geometry(child, rect);

		offset += binfo->gap;
	}
}


static void
box_calc_rect(rinfo, binfo, rect)
	Rectobj_info	*rinfo;
	Box_info	*binfo;
	Rect		*rect;
{
	Listnode	*list;
	int		used_space;
	int		max_other_dim;
	int		b2;
	Rectobj_info	*child_rinfo;

	max_other_dim = used_space = b2 = rinfo->border + rinfo->border;
	if(rinfo->n_children)
		used_space += (rinfo->n_children - 1) * binfo->gap;

	list = (Listnode*) binfo->children;
	list_for(list) {
		child_rinfo = RECTOBJ_PRIVATE(RECTOBJ_LIST_HANDLE(list));
		if(binfo->layout == BOX_LAYOUT_HORIZONTAL) {
			used_space += child_rinfo->rect.r_width;
			max_other_dim = MAX(max_other_dim, 
				child_rinfo->rect.r_height + b2);
		} else {
			used_space += child_rinfo->rect.r_height;
			max_other_dim = MAX(max_other_dim, 
				child_rinfo->rect.r_width + b2);
		}
	}

	if(binfo->layout == BOX_LAYOUT_HORIZONTAL) {
		rect->r_width = used_space;
		rect->r_height = max_other_dim;
	} else {
		rect->r_width = max_other_dim;
		rect->r_height = used_space;
	}
}


void
box_manage_child_proc(box, child, child_new_rect, child_old_rect)
	Rectobj			box;
	Rectobj			child;
	Rect			*child_new_rect;
	Rect			*child_old_rect;
{
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(box);
	Box_info	*binfo = BOX_PRIVATE(box);

	box_calc_rect(rinfo, binfo, &rinfo->rect);
	rectobj_geometry_manage(box, &rinfo->rect);
	box_set_children_rects(rinfo, binfo);
}


/* ARGSUSED */
void
box_set_geometry_proc(box, newrect, oldrect)
	Rectobj	box;
	Rect	*newrect;
	Rect	*oldrect;
{
	Box_info	*binfo = BOX_PRIVATE(box);

	if((newrect->r_width == oldrect->r_width) &&
	   (newrect->r_height == oldrect->r_height)) {
		rectobj_move_children(box);
		return;
	} else {
		box_set_children_rects(RECTOBJ_PRIVATE(box), binfo);
	}
}


/* ARGSUSED */
void
box_add_child_proc(box, child, rect)
	Box	box;
	Rectobj child;
	Rect	*rect;
{
	Box_info *binfo = BOX_PRIVATE(box);
	Rectobj_list	*listnode;

	listnode = xv_alloc(Rectobj_list);
	listnode->handle = (void*) child;
	RECTOBJ_PRIVATE(child)->layout_data = listnode;
	binfo->children = list_concat(binfo->children, listnode);
}


/* ARGSUSED */
void
box_del_child_proc(box, child, old_rect)
	Box	box;
	Rectobj	child;
	Rect	*old_rect;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(box);
	Box_info	*binfo = BOX_PRIVATE(box);

	binfo->children = list_first(list_delete_node( 
	 		(Listnode*) get_layout_data(child)));

	box_calc_rect(rinfo, binfo, &rinfo->rect);
	rectobj_geometry_manage(box, &rinfo->rect);
	box_set_children_rects(rinfo, binfo);
}

