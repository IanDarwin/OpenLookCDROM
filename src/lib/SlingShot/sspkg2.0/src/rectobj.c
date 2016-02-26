/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)rectobj.c 1.80 92/11/08";
#endif
#endif

#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/win_input.h>
#include <sspkg/rectobj.h>
#include <sspkg/patchlevel.h>
#include <limits.h>
#include "rectobj_impl.h"

static char *sspkg_patchlevel = 
"SlingShot extensions SSPKG_VERSION (Patch level SSPKG_PATCHLEVEL)";

Pkg_private int 	rectobj_init();
Pkg_private Xv_opaque	rectobj_set_avlist();
Pkg_private Xv_opaque	rectobj_get_attr();
Pkg_private int 	rectobj_destroy();

static	void	rectobj_set_class_func();
static	void	rectobj_add_to_parent_list(); 

int	rectobj_global_invocation_level = 0;


/*
 * This struct provides a indexed version of Rectobj_ops.
 * Two restrictions: 
 *	this type must be castable to Rectobj_ops;
 * 	the following macro depends on the attribute values (as
 *	defined in rectobj.h) to be sequential.
 */

typedef struct {
	int		ref_count;
	Proc_ptr	proc[1]; /* array is actually larger than this */
} Rectobj_ops_array;

#define RECTOBJ_PROCS_ATTR_INDEX(attr)	\
		(ATTR_ORDINAL(attr) - ATTR_ORDINAL(RECTOBJ_PAINT_PROC))


/*ARGSUSED*/
Pkg_private int
rectobj_init(parent, rectobj, avlist)
	Xv_opaque	parent;
	Rectobj		rectobj;
	Attr_avlist	avlist;
{
	Rectobj_info	*rinfo;
	Rectobj_struct	*rectobj_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		rectobj_paint_proc,
		rectobj_event_proc,
		rectobj_map_event_proc,
		rectobj_set_geometry_proc,
		rectobj_manage_child_proc,
		NULL, /* add child */
		NULL, /* del child */
	};

	rinfo = xv_alloc(Rectobj_info);
	rectobj_object = (Rectobj_struct*) rectobj;
	rectobj_object->private_data = (Xv_opaque) rinfo;

	rectobj_internal_init(rectobj, rinfo);
	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;
	rinfo->parent = parent;

	if(parent) {
		Rectobj_info	*parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
		rinfo->rect.r_left = rinfo->old_rect.r_left = 
			parent_rinfo->rect.r_left;
		rinfo->rect.r_top = rinfo->old_rect.r_top = 
			parent_rinfo->rect.r_top;
	}
	return(XV_OK);
}


void
rectobj_internal_init(rectobj, rinfo)
	Rectobj		rectobj;
	Rectobj_info	*rinfo;
{
	rinfo->listnode.handle = (void*)rectobj;
	RF_SET(rinfo,	(RF_PAINTED | 
			RF_STATE_INIT | 
			RF_MANAGE_CHILDREN |
			RF_RESTACK_CHILDREN |
			RF_DRAGGABLE |
			RF_ACCEPTS_DROP));
	rinfo->fg_color = -1; /* use default color */
	rinfo->bg_color = -1;
	rinfo->invocation_level = 1000;	/* arbitrarily high number */
}


Pkg_private Xv_opaque
rectobj_set_avlist(rectobj, avlist)
	Rectobj			rectobj;
	register Attr_avlist	avlist;
{
        register Rectobj_attr	attr;
        register Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj_info 		*alt_rinfo;
	short			check_size = FALSE;

	if(rinfo->invocation_level == 0)
		rectobj_reset_set_info(rectobj);

	rinfo->invocation_level++;
	rectobj_global_invocation_level++;

	while (attr = (Rectobj_attr) * avlist++)
	  switch (attr) {

		case XV_X:
			if(!rinfo->parent)
			  rinfo->rect.r_left = (short)*avlist;
			else
			  rinfo->rect.r_left = 
				RECTOBJ_PRIVATE(rinfo->parent)->rect.r_left
					+ (short)*avlist;
			avlist++;
			break;

		case XV_Y:
			if(!rinfo->parent)
			  rinfo->rect.r_top = (short)*avlist;
			else
			  rinfo->rect.r_top = 
				RECTOBJ_PRIVATE(rinfo->parent)->rect.r_top
					+ (short)*avlist;
			avlist++;
			break;

		case RECTOBJ_X:
			rinfo->rect.r_left	=(short)*avlist++;
			break;
		case RECTOBJ_Y:
			rinfo->rect.r_top 	=(short)*avlist++;
			break;

		case RECTOBJ_BORDER:
			rinfo->border		= (short)*avlist++;
			break;

		case XV_WIDTH:
			rinfo->rect.r_width	=(short)*avlist++;
			check_size = TRUE;
			break;
		case XV_HEIGHT:
			rinfo->rect.r_height	=(short)*avlist++;
			check_size = TRUE;
			break;

		case XV_RECT:
			rinfo->rect 		=*(Rect*)(*avlist++);
			check_size = TRUE;
			break;

		case RECTOBJ_MIN_WIDTH:
			rinfo->min_width	=(unsigned short)*avlist++;
			check_size = TRUE;
			break;

		case RECTOBJ_MIN_HEIGHT:
			rinfo->min_height	=(unsigned short)*avlist++;
			check_size = TRUE;
			break;

		case RECTOBJ_RESIZABLE:
			if(*avlist++)
				RF_SET(rinfo, RF_RESIZABLE);
			else
				RF_UNSET(rinfo, RF_RESIZABLE);
			break;

		case XV_SHOW:
			if(*avlist++)
				RF_SET(rinfo, RF_PAINTED);
			else
				RF_UNSET(rinfo, RF_PAINTED);
			RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			break;

		case RECTOBJ_PARENT:
		case XV_OWNER:
			if(RF_IS_SET(rinfo, RF_STATE_INIT)) {
				rinfo->parent = (Rectobj)*avlist++;
				break;
			}
			if(rinfo->parent) {
			  rectobj_repaint_rect(rectobj, NULL, TRUE);
			  alt_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
			  alt_rinfo->children = (Rectobj_list*) list_first(
					list_unlink_node(&rinfo->listnode));
			  alt_rinfo->n_children--;
			  rinfo->previous_parent = rinfo->parent;
			}
			rinfo->parent = (Rectobj)*avlist++;
			rectobj_add_to_parent_list(rinfo);
			if(!rinfo->parent)
				/* not attached, so it can't be selected */
				rectobj_del_from_selected_list(rectobj, NULL);
			rectobj_repaint_rect(rectobj, NULL, TRUE);
			break;

		case RECTOBJ_SELECTED:
			if(*avlist++) {
				rectobj_add_to_selected_list(rectobj, 
					FALSE, NULL);
				rectobj_set_paint_style(rectobj, NULL, 
					RECTOBJ_HIGHLIGHT);
			} else {
				rectobj_del_from_selected_list(rectobj, NULL);
				rectobj_set_paint_style(rectobj, NULL, 
					RECTOBJ_NORMAL);
			}
			break;

		case RECTOBJ_SELECTABLE:
			if( *avlist++)
				RF_SET(rinfo, RF_SELECTABLE);
			else {
				RF_UNSET(rinfo, RF_SELECTABLE);
				rectobj_del_from_selected_list(rectobj, NULL);
			}
			break;

		case RECTOBJ_TOGGLE_STATE:
			rectobj_set_paint_style(rectobj, NULL,
			  (*avlist++) ? RECTOBJ_HIGHLIGHT : RECTOBJ_NORMAL);
			break;

		case RECTOBJ_EXCLUSIVE_SELECT:
			if(*avlist++)
				RF_SET(rinfo, RF_EXCLUSIVE_SELECT);
			else
				RF_UNSET(rinfo, RF_EXCLUSIVE_SELECT);
			break;

		case RECTOBJ_DRAGGABLE:
			if(*avlist++)
				RF_SET(rinfo, RF_DRAGGABLE);
			else
				RF_UNSET(rinfo, RF_DRAGGABLE);
			break;

		case RECTOBJ_ACCEPTS_DROP:
			if(*avlist++)
				RF_SET(rinfo, RF_ACCEPTS_DROP);
			else
				RF_UNSET(rinfo, RF_ACCEPTS_DROP);
			break;

		case RECTOBJ_FG:
			rinfo->fg_color = (short) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case RECTOBJ_BG:
			rinfo->bg_color = (short) *avlist++;
			RF_SET(rinfo, RF_REPAINT);
			break;

		case RECTOBJ_NORMAL:
		case RECTOBJ_HIGHLIGHT:
		case RECTOBJ_PREHIGHLIGHT:
		case RECTOBJ_PREDROP_NORMAL:
		case RECTOBJ_PREDROP_HIGHLIGHT:
			rectobj_set_paint_style(rectobj, NULL, attr);
			break;

		case RECTOBJ_PAINT_PROC:
		case RECTOBJ_EVENT_PROC:
		case RECTOBJ_MAP_EVENT_PROC:
		case RECTOBJ_SET_GEOMETRY_PROC:
		case RECTOBJ_ADD_CHILD_PROC:
		case RECTOBJ_DEL_CHILD_PROC:
		case RECTOBJ_MANAGE_CHILD_PROC:
		case RECTOBJ_NEW_PARENT_PROC:
		case RECTOBJ_START_DRAG_PROC:
		case RECTOBJ_STYLE_CHANGE_PROC:
		case RECTOBJ_SINGLE_CLICK_PROC:
		case RECTOBJ_DBL_CLICK_PROC:
		case RECTOBJ_DROP_PROC:
		case RECTOBJ_SELECTION_PROC:
			rectobj_set_class_func(rinfo, 
				RECTOBJ_PROCS_ATTR_INDEX(attr),
				(Proc_ptr) *avlist++);
			break;


		case RECTOBJ_OPS: {
			Rectobj_ops *previous;
			previous = rinfo->rectobj_ops;
			rinfo->rectobj_ops = (Rectobj_ops*) *avlist++;
			rinfo->rectobj_ops->ref_count++;
			if(previous->ref_count == 1)
				free(previous);
			else
				previous->ref_count--;
			}
			break;

		case RECTOBJ_GEOMETRY_SILENT:
			if(*avlist++)
				RF_SET(rinfo, RF_GEOMETRY_SILENT);
			else
				RF_UNSET(rinfo, RF_GEOMETRY_SILENT);
			break;

		case RECTOBJ_MANAGE_CHILDREN:
			if(*avlist++)
				RF_SET(rinfo, RF_MANAGE_CHILDREN);
			else
				RF_UNSET(rinfo, RF_MANAGE_CHILDREN);
			RF_SET(rinfo, RF_REPAINT);
			break;

		case RECTOBJ_RESTACK_CHILDREN:
			if(*avlist++)
				RF_SET(rinfo, RF_RESTACK_CHILDREN);
			else
				RF_UNSET(rinfo, RF_RESTACK_CHILDREN);
			break;


		case RECTOBJ_STACKING_POSITION:
			rectobj_set_stacking_position(rectobj, (int)*avlist++);
			rectobj_repaint_rect(rectobj, NULL, TRUE);
			break;

		case XV_END_CREATE:
			RF_UNSET(rinfo, RF_STATE_INIT);
			RF_SET(rinfo, (RF_STATE_CREATED|RF_REPAINT|RF_CLEAR));
			rinfo->invocation_level = 1;
			rectobj_add_to_parent_list(rinfo);
			break;

		default:
			avlist = attr_skip(attr, avlist);

	  }

	if(check_size) {
		if((unsigned short) rinfo->rect.r_width < rinfo->min_width)
			rinfo->rect.r_width = rinfo->min_width;
		if((unsigned short) rinfo->rect.r_height < rinfo->min_height)
			rinfo->rect.r_height = rinfo->min_height;
	}

	if(((Xv_base*)rectobj)->pkg == RECTOBJ) {
		if(rectobj_finish_set1(rectobj))
			rectobj_finish_set2(rectobj);
	}

	return(XV_OK);
}



void
rectobj_finish_set(rectobj)
	Rectobj rectobj;
{
	if(rectobj_finish_set1(rectobj))
		rectobj_finish_set2(rectobj);
}


int
rectobj_finish_set1(rectobj)
	Rectobj rectobj;
{
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);

	rinfo->invocation_level--;
	if((rinfo->invocation_level != 0) || RF_IS_SET(rinfo, RF_STATE_INIT)) {
		rectobj_global_invocation_level--;
		return FALSE;
	}

	if(rinfo->parent != rinfo->previous_parent) {

	  if(rinfo->previous_parent) {
		Rectobj_info *tmp = RECTOBJ_PRIVATE(rinfo->previous_parent);
		if(tmp->rectobj_ops->del_child_proc)
		  (tmp->rectobj_ops->del_child_proc)
			(rinfo->previous_parent, rectobj, &rinfo->old_rect);
	  }

	  rinfo->layout_data = 0;

	  if(rinfo->parent) {
		Rectobj_info *tmp = RECTOBJ_PRIVATE(rinfo->parent);
		if(tmp->rectobj_ops->add_child_proc)
		(tmp->rectobj_ops->add_child_proc)
			(rinfo->parent, rectobj, &rinfo->rect);
		/*
		 * If the geometry of the child has not changed,
		 * set this flag to force it to be managed.
		 */
		RF_SET(rinfo, RF_FORCE_GEO_MANAGE);
	  }

	  rinfo->previous_parent = rinfo->parent;
	}
	if(RF_IS_SET(rinfo, RF_STATE_CREATED))
		return TRUE;
	rectobj_global_invocation_level--;
	return FALSE;
}



void
rectobj_finish_set2(rectobj)
	Rectobj rectobj;
{
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);

	/*
	 * If the object didn't call the geometry manager on it's own
	 * behalf after the finish1 call, do it for the object.
	 * Dont go through rectobj geometry manage interface because it 
	 * sets RF_GEO_PROC_IS_CALLER which is appropriate in this case.
	 * Geometry managers are not called after del_child... because
	 * parent has changed.
	 * Geometry managers are called after add_child, however.
	 */
	if (!RF_IS_SET(rinfo, (RF_GEO_MANAGER_CALLED|RF_GEOMETRY_SILENT)) &&
	   rinfo->parent && 
	   ((rectcmp(&rinfo->rect, &rinfo->old_rect)) ||
	    (RF_IS_SET(rinfo, RF_FORCE_GEO_MANAGE)))) {
		Rectobj_info *tmp = RECTOBJ_PRIVATE(rinfo->parent);

		if(tmp->rectobj_ops->manage_child_proc)
			(tmp->rectobj_ops->manage_child_proc) 
				(rinfo->parent, rectobj, 
					&rinfo->rect, &rinfo->old_rect);
	}

	if(RF_IS_SET(rinfo, RF_REPAINT))
		rectobj_repaint_rect(rectobj, &rinfo->rect, 
			RF_IS_SET(rinfo, RF_CLEAR));

	rectobj_global_invocation_level--;
	if(!rectobj_global_invocation_level) 
	  	rectobj_flush_repaint(FALSE);
	rectobj_reset_set_info(rectobj);
}


void
rectobj_reset_set_info(rectobj)
	Rectobj rectobj;
{
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);

	/* reinitialize for next call to set function on this object */
	rinfo->invocation_level = 0;
	rinfo->old_rect = rinfo->rect;
	RF_UNSET(rinfo, (
			RF_GEO_MANAGER_CALLED	|
			RF_REPAINT		|
			RF_CLEAR		|
			RF_FORCE_GEO_MANAGE	));
			
}


/*ARGSUSED*/
Pkg_private Xv_opaque
rectobj_get_attr(rectobj, status, attr, avlist)
	Rectobj		rectobj;
	int		*status;
	register Attr_attribute attr;
	Attr_avlist	avlist;
{
	Rectobj_info   *rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj_info   *parent_rinfo;

	switch (attr) {

		case XV_X: {
			int parent_x;

			if(!rinfo->parent)
				parent_x = 0;
			else {
				parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
				parent_x = parent_rinfo->rect.r_left;
			}
			return (Xv_opaque) rinfo->rect.r_left - parent_x;
		}

		case XV_Y: {
			int parent_y;

			if(!rinfo->parent)
				parent_y = 0;
			else {
				parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
				parent_y = parent_rinfo->rect.r_top;
			}
			return (Xv_opaque) rinfo->rect.r_top - parent_y;
		}

		case RECTOBJ_X:
			return (Xv_opaque) rinfo->rect.r_left;
		case RECTOBJ_Y:
			return (Xv_opaque) rinfo->rect.r_top;
		case RECTOBJ_BORDER:
			return (Xv_opaque) rinfo->border;

		case XV_WIDTH:
			return (Xv_opaque) rinfo->rect.r_width;
		case XV_HEIGHT:
			return (Xv_opaque) rinfo->rect.r_height;

		case XV_RECT:
			return (Xv_opaque) &rinfo->rect;

		case RECTOBJ_MIN_WIDTH:
			return (Xv_opaque) rinfo->min_width;
		case RECTOBJ_MIN_HEIGHT:
			return (Xv_opaque) rinfo->min_width;

		case RECTOBJ_RESIZABLE:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_RESIZABLE));

		case RECTOBJ_NORMAL:
			return (Xv_opaque) (RF_IS_SET(rinfo,
				(RF_HIGHLIGHT		| 
				 RF_PREHIGHLIGHT	| 
				 RF_PREDROP_HIGHLIGHT	)) == 0);

		case RECTOBJ_HIGHLIGHT:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_HIGHLIGHT));

		case RECTOBJ_PREHIGHLIGHT:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_PREHIGHLIGHT));

		case RECTOBJ_PREDROP_HIGHLIGHT:
			return (Xv_opaque) (RF_TRUE(rinfo, 
						RF_PREDROP_HIGHLIGHT));

		case RECTOBJ_PREDROP_NORMAL:
			return (Xv_opaque) (!RF_TRUE(rinfo, 
						RF_PREDROP_HIGHLIGHT));

		case RECTOBJ_PARENT:
		case XV_OWNER:
			return (Xv_opaque) rinfo->parent;

		case RECTOBJ_N_CHILDREN:
			return (Xv_opaque) rinfo->n_children;

		case RECTOBJ_CANVAS:
			if(rinfo->shared_info)
			  return (Xv_opaque) rinfo->shared_info->canvas_shell;
			return (Xv_opaque) 0;

		case RECTOBJ_CHILDREN:
			return (Xv_opaque) rinfo->children;

		case RECTOBJ_NTH_CHILD: {
			int i, n;
			Rectobj_list *node;

			n = (int)*avlist;
			node = list_first(rinfo->children);

			for(i=0; ((i < n) && (node != NULL)); i++)
				node = list_next(node);
			return (Xv_opaque) RECTOBJ_LIST_HANDLE(node);
			}

		case RECTOBJ_STACKING_POSITION: {
			Rectobj_list *list;
			int i;

			if(!rinfo->parent)
				return (Xv_opaque) 0;
			list = RECTOBJ_PRIVATE(rinfo->parent)->children;

			i = 0;
			while(list) {
				if(RECTOBJ_LIST_HANDLE(list) == rectobj)
					break;
				list = list_next(list);
				i++;
			}
			return (Xv_opaque) i;
			}

		case RECTOBJ_SELECTED:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_SELECTED));

		case RECTOBJ_SELECTABLE:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_SELECTABLE));

		case RECTOBJ_TOGGLE_STATE:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_HIGHLIGHT));

		case RECTOBJ_EXCLUSIVE_SELECT:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_EXCLUSIVE_SELECT));
		case RECTOBJ_DRAGGABLE:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_DRAGGABLE));

		case RECTOBJ_ACCEPTS_DROP:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_ACCEPTS_DROP));

		case RECTOBJ_FG:
			return (Xv_opaque) rinfo->fg_color;

		case RECTOBJ_BG:
			return (Xv_opaque) rinfo->bg_color;

		case RECTOBJ_OPS:
			return (Xv_opaque) rinfo->rectobj_ops;

		case RECTOBJ_PAINT_PROC:
		case RECTOBJ_EVENT_PROC:
		case RECTOBJ_MAP_EVENT_PROC:
		case RECTOBJ_SET_GEOMETRY_PROC:
		case RECTOBJ_ADD_CHILD_PROC:
		case RECTOBJ_DEL_CHILD_PROC:
		case RECTOBJ_MANAGE_CHILD_PROC:
		case RECTOBJ_NEW_PARENT_PROC:
		case RECTOBJ_START_DRAG_PROC:
		case RECTOBJ_STYLE_CHANGE_PROC:
		case RECTOBJ_SINGLE_CLICK_PROC:
		case RECTOBJ_DBL_CLICK_PROC:
		case RECTOBJ_DROP_PROC:
		case RECTOBJ_SELECTION_PROC:
			return (Xv_opaque)
				(((Rectobj_ops_array*)(rinfo->rectobj_ops))
					->proc[RECTOBJ_PROCS_ATTR_INDEX(attr)]);

		case RECTOBJ_GEOMETRY_SILENT:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_GEOMETRY_SILENT));

		case RECTOBJ_MANAGE_CHILDREN:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_MANAGE_CHILDREN));

		case RECTOBJ_RESTACK_CHILDREN:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_RESTACK_CHILDREN));

		case XV_SHOW:
			return (Xv_opaque) (RF_TRUE(rinfo, RF_PAINTED));

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}


/*ARGSUSED*/
Pkg_private int
rectobj_destroy(rectobj, status)
	Rectobj		rectobj;
	Destroy_status	status;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Canvas_shell	canvas_shell;

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;


	if(rinfo->shared_info) {
		canvas_shell = rinfo->shared_info->canvas_shell;
		rectobj_set_delay_repaint(canvas_shell, TRUE);
	} else {
		canvas_shell = (Canvas_shell) 0;
	}
	rectobj_del_from_selected_list(rectobj, NULL);

	rectobj_repaint_rect(rectobj, &rinfo->rect, TRUE);
	RF_SET(rinfo, RF_STATE_DESTROYING);
	RF_UNSET(rinfo, RF_STATE_CREATED);
	
	if(RF_IS_SET(rinfo, RF_HAS_EVENT_GRAB))
		rectobj_set_event_grab(canvas_shell, rectobj, 0, 0);

	rectobj_destroy_children(rectobj);

	if(rinfo->parent)
		/*
		 * This set is risky because subclasses free
		 * their private data and then their set function
		 * is called.  This hasn't cause problems yet, but
		 * another solution may be necessary.
		 */
		xv_set(rectobj, RECTOBJ_PARENT, XV_NULL, NULL);

	if(canvas_shell)
		rectobj_set_delay_repaint(canvas_shell, FALSE);

	if(rinfo->rectobj_ops->ref_count == 1)
		free(rinfo->rectobj_ops);
	else
		rinfo->rectobj_ops->ref_count--;

	free(rinfo);
	return XV_OK;
}

static void
rectobj_add_to_parent_list(rinfo) 
	Rectobj_info *rinfo;
{
	Rectobj_info *parent_rinfo;
	void *set_shared_info();

	/* add adjustment for stacking order here */
	if(rinfo->parent) {
		parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);
		parent_rinfo->children = (Rectobj_list*) list_concat(
			parent_rinfo->children, &rinfo->listnode);
		parent_rinfo->n_children++;
	}
	(void) traverse_rectobj_tree( RECTOBJ_PUBLIC(rinfo), set_shared_info, 
		(rinfo->parent ? parent_rinfo->shared_info: NULL));
}


/*
 * make a depth first recursive traversal of the rectobj tree,
 * call a function on each node.  If the function ever returns
 * non-zero,  then stop the traversal, returning result to caller.
 */
void *
traverse_rectobj_tree(rectobj, function, arg)
	Rectobj rectobj;
	void	*(*function)();
	void	*arg;
{
	Rectobj_list *node;
	Rectobj	child;
	void 	*result;

	node = (Rectobj_list*)xv_get(rectobj, RECTOBJ_CHILDREN);

	list_for(node) {
		child = RECTOBJ_LIST_HANDLE(node);
		result = traverse_rectobj_tree(child, function, arg);
		if(result)
			return result;
	}
	return((function)(rectobj, arg));
}


static void *
set_shared_info(rectobj, shared_info)
	Rectobj		rectobj;
	Shared_info	*shared_info;
{
	Rectobj_info    *rinfo = RECTOBJ_PRIVATE(rectobj);

	rinfo->shared_info = shared_info;

	if(shared_info) {
		/* 
		 * Limitation: objects will not know when ancestory changes,
		 * colors, fonts, etc. will not get updated when moving to a
		 * new canvas shell.
		 */
		if(rinfo->fg_color >= shared_info->num_colors)
			rinfo->fg_color = -1;

		if(rinfo->bg_color >= shared_info->num_colors)
			rinfo->bg_color = -1;
	}
	return (void*) NULL;
}


static void
rectobj_set_class_func(rinfo, which, fn)
	Rectobj_info	*rinfo;
	int		which;
	Proc_ptr	fn;
{
	if(rinfo->rectobj_ops->ref_count != 1) {
		/*
		 * Shared rectobj_ops:  copy and set private to this rectobj.
		 */
		Rectobj_ops	*tmp;
		tmp = (Rectobj_ops*) malloc(sizeof(Rectobj_ops));
		*tmp = *(rinfo->rectobj_ops);
		tmp->ref_count = 1;
		rinfo->rectobj_ops = tmp;
	}

	((Rectobj_ops_array*)(rinfo->rectobj_ops))->proc[which] = fn;
}


void
rectobj_destroy_children(rectobj)
	Rectobj rectobj;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	rectobj_global_invocation_level++; /* don't repaint for each destroy */
	while(rinfo->children)
		xv_destroy( RECTOBJ_LIST_HANDLE(rinfo->children) );
	rectobj_global_invocation_level--;
}


void
rectobj_paint_children(rectobj, dpy, win, xrects)
	Rectobj	rectobj;
	Display *dpy;
	Window 	win;
	Xv_xrectlist *xrects;
{
	Rectobj_list	*node;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);

	node = rinfo->children;
	list_for(node) {
	    rectobj_paint_child(RECTOBJ_LIST_HANDLE(node), dpy, win, xrects);
	}
}


void
rectobj_paint_child(rectobj, dpy, win, xrects)
	Rectobj		rectobj;
	Display		*dpy;
	Window		win;
	Xv_xrectlist	*xrects;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	register int i;

	if(RF_IS_SET(rinfo, RF_PAINTED)) {
	  for(i=0; i<xrects->count; i++)
	    if(rect_intersectsrect(&rinfo->rect, 
			(Rect*)(&xrects->rect_array[i]))) {
		(rinfo->rectobj_ops->paint_proc)(rectobj, dpy, win, xrects);
		return;
	    }
	}
}


int
rectobj_geometry_manage(rectobj, newrect)
	Rectobj rectobj;
	Rect	*newrect;
{
	register Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);
	Rect 	saverect;

	if(RF_IS_SET(rinfo, RF_GEOMETRY_SILENT))
		return 0;	/* ignored */

	if(newrect != NULL)
		rinfo->rect = *newrect;

	if((rectcmp(&rinfo->rect, &rinfo->old_rect)) ||
	   RF_IS_SET(rinfo, RF_FORCE_GEO_MANAGE)) {

	  /* rect has changed, inform the parent */
	  if(rinfo->parent) {
	    Rectobj_info *tmp = RECTOBJ_PRIVATE(rinfo->parent);

	    saverect = rinfo->rect;

	    RF_SET(rinfo, (RF_GEO_PROC_IS_CALLER | RF_GEO_MANAGER_CALLED));

	    if(tmp->rectobj_ops->manage_child_proc)
	    	(tmp->rectobj_ops->manage_child_proc)
			(rinfo->parent, rectobj, 
			&rinfo->rect, &rinfo->old_rect);

	    RF_UNSET(rinfo, RF_GEO_PROC_IS_CALLER);

	    /* return non-zero if rect is not exactly as asked for */
	    return rectcmp(&saverect, &rinfo->rect);

	  } else {
		rectobj_set_geometry(rectobj, &rinfo->rect);
	  }
	}
	return 0;
}


int rectobj_hack_no_old_rect_repaint = FALSE;/* until I sort this out... */

void
rectobj_set_geometry(child, newrect)
	Rectobj	child;
	Rect	*newrect;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(child);

	if(newrect == NULL)
		newrect = &rinfo->rect;
	else
		rinfo->rect = *newrect;
	
	if( !rectcmp(newrect, &rinfo->old_rect) )
		return;

	if(!rectobj_hack_no_old_rect_repaint)
		rectobj_repaint_rect(child, &rinfo->old_rect, TRUE);

	rectobj_repaint_rect(child, newrect, FALSE);

	if((! RF_IS_SET(rinfo, RF_GEO_PROC_IS_CALLER) ) &&
		(rinfo->rectobj_ops->set_geometry_proc))
		(rinfo->rectobj_ops->set_geometry_proc) (child, newrect, 
			&rinfo->old_rect);

	rinfo->old_rect = rinfo->rect;

	if(rectobj_hack_no_old_rect_repaint)
		rectobj_hack_no_old_rect_repaint = FALSE;
}

/*
 * Blanket move of all children, relative to how the parent has moved.
 * Not useful after call to rectobj-geometry-manage because old_rect
 * is equivalent to new_rect after that call.
 */
void
rectobj_move_children(parent)
	Rectobj	parent;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(parent);
	Rectobj_list	*node;
	Rectobj		child;
	Rectobj_info	*child_rinfo;
	Rect		rect;
	int		delta_x;
	int		delta_y;

	if(!rectcmp(&rinfo->rect, &rinfo->old_rect))
		return;

	delta_x = rinfo->rect.r_left - rinfo->old_rect.r_left;
	delta_y = rinfo->rect.r_top - rinfo->old_rect.r_top;

	if(delta_x == 0 && delta_y == 0)
		return;

	node = rinfo->children;
	list_for(node) {
		child = RECTOBJ_LIST_HANDLE(node);
		child_rinfo = RECTOBJ_PRIVATE(child);

		rect.r_left = child_rinfo->rect.r_left + delta_x;
		rect.r_top  = child_rinfo->rect.r_top  + delta_y;
		rect.r_width = child_rinfo->rect.r_width;
		rect.r_height = child_rinfo->rect.r_height;

		rectobj_set_geometry(child, &rect);
	}
}


void
rectobj_delta_move_children(parent, delta_x, delta_y)
	Rectobj	parent;
	int		delta_x;
	int		delta_y;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(parent);
	Rectobj_list	*node;
	Rectobj		child;
	Rectobj_info	*child_rinfo;
	Rect		rect;

	if(delta_x == 0 && delta_y == 0)
		return;

	node = rinfo->children;
	list_for(node) {
		child = RECTOBJ_LIST_HANDLE(node);
		child_rinfo = RECTOBJ_PRIVATE(child);

		rect.r_left = child_rinfo->rect.r_left + delta_x;
		rect.r_top  = child_rinfo->rect.r_top  + delta_y;
		rect.r_width = child_rinfo->rect.r_width;
		rect.r_height = child_rinfo->rect.r_height;

		rectobj_set_geometry(child, &rect);
	}
}


void
rectobj_min_enclosing_rect(children, r)
	Rectobj_list	*children;
	Rect		*r;
{
	Rectobj_info *rinfo;
	short x0, y0, x1, y1;

	x0 = y0 = SHRT_MAX; /* from limits.h 0x7FFF */
	x1 = y1 = 0;

	list_for(children) {
		rinfo = RECTOBJ_PRIVATE(RECTOBJ_LIST_HANDLE(children));
		x0 = MIN(rinfo->rect.r_left, x0);
		y0 = MIN(rinfo->rect.r_top, y0);
		x1 = MAX(rinfo->rect.r_left + rinfo->rect.r_width, x1);
		y1 = MAX(rinfo->rect.r_top + rinfo->rect.r_height, y1);
	}
	r->r_left = x0;
	r->r_top = y0;
	r->r_width = x1 - x0;
	r->r_height = y1 - y0;
}


void
rectobj_fit(rectobj)
	Rectobj rectobj;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Rect		r;

	if(rinfo->children) {
		rectobj_min_enclosing_rect(rinfo->children, &r);
		(void) xv_set(rectobj,
			XV_WIDTH, (r.r_left + r.r_width) - rinfo->rect.r_left,
			XV_HEIGHT, (r.r_top + r.r_height) - rinfo->rect.r_top,
			NULL);
	} else {
		(void) xv_set(rectobj,
			XV_WIDTH, 0,
			XV_HEIGHT, 0,
			NULL);
	}
}


void
rectobj_paint_proc(rectobj, dpy, win, xrects)
	Rectobj	rectobj;
	Display *dpy;
	Window 	win;
	Xv_xrectlist *xrects;
{
	rectobj_paint_children(rectobj, dpy, win, xrects);
}


void
rectobj_event_proc(paint_window, event, canvas, rectobj)
	Xv_window	paint_window;
	Event		*event;
	Canvas		canvas;
	Rectobj		rectobj;	
{
	background_event_proc(paint_window, event, canvas, rectobj);
}


Rectobj
rectobj_map_event_proc(rectobj, event)
	Rectobj		rectobj;
	Event		*event;
{
	/*
	 * General strategy for mapping events to children/self:
	 *
	 * 1) check if object is painted and event is in bounds.
	 *	If not, return NULL so other objects can be tested.
	 * 2) check if event maps to children.  Unless painted in
	 *	some odd order, children are given priority.
	 * 3) check if event maps to self.  Unless opaque, the parent
	 * 	should be returned.
	 * Children are checked in order reverse the order of painting.
	 */
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj		return_val;
	Rectobj_list	*node;
	Rectobj		child;

	if(!(RF_IS_SET(rinfo, RF_PAINTED)) ||
	    (!rect_includespoint(&rinfo->rect, event_x(event), event_y(event))))
		return NULL;

	node = rinfo->children;

	list_rof(node) {
	  child = RECTOBJ_LIST_HANDLE(node);
	  rinfo = RECTOBJ_PRIVATE(child);
	  if((!(RF_IS_SET(rinfo, RF_PAINTED))) ||
	    (!rect_includespoint(&rinfo->rect, event_x(event), event_y(event))))
		continue;

	  if((rinfo->rectobj_ops->map_event_proc) &&
	     (return_val=(rinfo->rectobj_ops->map_event_proc)(child, event)))
		return return_val;
	}

	return rectobj;
}


/* ARGSUSED */
void
rectobj_set_geometry_proc(rectobj, newrect, oldrect)
	Rectobj	rectobj;
	Rect	*newrect;
	Rect	*oldrect;
{
	rectobj_move_children(rectobj);
}

/* ARGSUSED */
void
rectobj_manage_child_proc(parent, child, child_new_rect, child_old_rect)
	Rectobj			parent;
	Rectobj			child;
	Rect			*child_new_rect;
	Rect			*child_old_rect;
{
	rectobj_set_geometry(child, child_new_rect);
}


Rectobj
rectobj_upsearch(rectobj, ptr, attr, arg)
	Rectobj		rectobj;
	Xv_opaque	*ptr;
	Attr_attribute	attr;
	int		arg;
{
	if(!rectobj) {
		*ptr = NULL;
		return NULL;
	}

	while(1) {
		*ptr = (Xv_opaque) xv_get(rectobj, attr, arg);
		if(*ptr)
			return rectobj;
		if(!(rectobj = (Rectobj) xv_get(rectobj, RECTOBJ_PARENT))) {
			*ptr = NULL;
			return NULL;
		}
	}
}


void
rectobj_set_stacking_position(rectobj, pos)
	Rectobj rectobj;
	int	pos;
{
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj_info *parent_rinfo;
	Rectobj_list *alt_list;
	Rectobj_list *node;
	int	i;

	if(!rinfo->parent)
		return;
	parent_rinfo = RECTOBJ_PRIVATE(rinfo->parent);

	node = list_find(parent_rinfo->children, (void*)rectobj);
	if(!node)	/* This will be TRUE will creating. */
		return;

	parent_rinfo->children = alt_list = list_first(list_unlink_node(node));

	i = 0;
	while((i < pos) && (alt_list)) {
		alt_list = list_next(alt_list);
		i++;
	}

	if((i>0) && (!alt_list))
		(void)list_concat(parent_rinfo->children, node);
	else
		parent_rinfo->children = list_first(
			list_insert_before(alt_list, node));
	/* Caller should clear area w/ rectobj_repaint_rect() or set flags. */
}


void
rectobj_set_paint_style(rectobj, event, attr)
	Rectobj		rectobj;
	Event		*event;
	Attr_attribute	attr;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	unsigned long	previous_flags = rinfo->flags;
	int		default_repaint = TRUE;

	switch(attr) {
		case RECTOBJ_NORMAL:
			if(RF_IS_SET(rinfo, (RF_HIGHLIGHT | 
					RF_PREHIGHLIGHT |
					RF_PREDROP_HIGHLIGHT))) {
				RF_UNSET(rinfo, (RF_HIGHLIGHT | 
						RF_PREHIGHLIGHT |
						RF_PREDROP_HIGHLIGHT));
			}
			break;

		case RECTOBJ_PREHIGHLIGHT:
			if(!RF_IS_SET(rinfo, RF_PREHIGHLIGHT)) {
				if(RF_IS_SET(rinfo, RF_HIGHLIGHT))
					default_repaint = FALSE;
				RF_SET(rinfo, RF_PREHIGHLIGHT);
				RF_UNSET(rinfo, RF_HIGHLIGHT);
			}
			break;

		case RECTOBJ_HIGHLIGHT:
			if(!RF_IS_SET(rinfo, RF_HIGHLIGHT)) {
				if(RF_IS_SET(rinfo, RF_PREHIGHLIGHT))
					default_repaint = FALSE;
				RF_SET(rinfo, RF_HIGHLIGHT);
				RF_UNSET(rinfo, RF_PREHIGHLIGHT);
			}
			break;

		case RECTOBJ_PREDROP_HIGHLIGHT:
			if(!RF_IS_SET(rinfo, RF_PREDROP_HIGHLIGHT)) {
				RF_SET(rinfo, RF_PREDROP_HIGHLIGHT);
			}
			break;

		case RECTOBJ_PREDROP_NORMAL:
			if(RF_IS_SET(rinfo, RF_PREDROP_HIGHLIGHT)) {
				RF_UNSET(rinfo, RF_PREDROP_HIGHLIGHT);
			}
			break;
	}

	if((previous_flags ^ rinfo->flags)) {
		if(rinfo->rectobj_ops->style_change_proc) {
			/* 
			 * Group leader is responsible for propagating state 
			 * change to children.
			 * It's also responsible for repainting in the
			 * non-default case.  This is usually expected 
			 * to be done as a side effect of setting some 
			 * attribute.
			 */
			(rinfo->rectobj_ops->style_change_proc) 
				(rectobj, event, attr, default_repaint);
		} else {
			/* 
			 * Default repaint procs don't paint anything 
			 * different for pre-highlight and highlight,
			 * so in these cases, ignore the need to repaint.
			 */
			if(default_repaint)
				rectobj_repaint_rect(rectobj, NULL, TRUE);
		}
	}
}


void
rectobj_simple_style_change_proc(rectobj, event, attr, default_repaint)
	Rectobj 	rectobj;
	Event		*event;
	Attr_attribute	attr;
	int		default_repaint;
{
	/* For objects that have one appearance, main purpose of this
	 * is to reduce flashing.
	 */
	return;
}


void
rectobj_recursive_style_change_proc(rectobj, event, attr, default_repaint)
	Rectobj 	rectobj;
	Event		*event;
	Attr_attribute	attr;
	int		default_repaint;
{
	/*
	 * Changes style recursively to children
	 */
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj_info	*child_rinfo;
	Rectobj_list	*node;
	Rectobj		child;

	node = rinfo->children;
	list_for(node) {
		child = RECTOBJ_LIST_HANDLE(node);
		child_rinfo = RECTOBJ_PRIVATE(child);

		/* turn all highlight flags off */
		child_rinfo->flags &= (RF_FLAG_MASK ^ 
			(RF_HIGHLIGHT|RF_PREHIGHLIGHT|RF_PREDROP_HIGHLIGHT));

		/* make highlight bits same as parents */
		child_rinfo->flags |= rinfo->flags &
			(RF_HIGHLIGHT|RF_PREHIGHLIGHT|RF_PREDROP_HIGHLIGHT);

		if(child_rinfo->children)
			/* recurse */
			rectobj_recursive_style_change_proc(
				child, event, attr, FALSE);
	}
	if(default_repaint)
		rectobj_repaint_rect(rectobj, NULL, TRUE);
}

#ifdef EXAMPLES

/* ARGSUSED */
void
rectobj_add_child_proc(parent, child, rect)
	Rectobj parent;
	Rectobj child;
	Rect	*rect;
{
	;
}

/* ARGSUSED */
void
rectobj_del_child_proc(parent, child, old_rect)
	Rectobj parent;
	Rectobj child;
	Rect	*old_rect;
{
	;
}


#endif

