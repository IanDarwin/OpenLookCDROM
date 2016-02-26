/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char sccsid[] = "@(#)tree.c 1.20 92/11/08";
#endif
#endif

#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <sspkg/tree.h>
#include "tree_impl.h"
#include "rectobj_impl.h"
#include <sspkg/drawobj.h>

Pkg_private int 	tree_init();
Pkg_private Xv_opaque	tree_set_avlist();
Pkg_private Xv_opaque	tree_get_attr();
Pkg_private int 	tree_destroy();

	void	tree_paint_proc();
	void	tree_set_geometry_proc();
	void	tree_manage_child_proc();
	void	tree_add_child_proc();
	void	tree_del_child_proc();
static	void	tree_link_child();
static	void	tree_unlink_child();
static	void	tree_set_show_flag();
static	void	tree_layout_resize();

#define get_layout_data_private( _rinfo_ ) \
	(Tree_layout_data *) ( _rinfo_ )  ->  layout_data

#define get_layout_data( _rectobj_ ) \
	(Tree_layout_data *) (( RECTOBJ_PRIVATE( _rectobj_ ))  ->  layout_data)

/*ARGSUSED*/
Pkg_private int
tree_init(parent, tree, avlist)
	Xv_opaque	parent;
	Tree		tree;
	Attr_avlist	avlist;
{
	Tree_info	*tinfo;
	Tree_struct	*tree_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		tree_paint_proc,
		rectobj_event_proc,
		rectobj_map_event_proc,
		tree_set_geometry_proc,
		tree_manage_child_proc,
		tree_add_child_proc,
		tree_del_child_proc,
	};

	tinfo = xv_alloc(Tree_info);
	tree_object = (Tree_struct*) tree;
	tree_object->private_data = (Xv_opaque) tinfo;

	RECTOBJ_PRIVATE(tree)->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	tinfo->parent_distance = 30;
	tinfo->layout = TREE_LAYOUT_HORIZONTAL;
	tinfo->border = 4;

	return(XV_OK);
}


Pkg_private Xv_opaque
tree_set_avlist(tree, avlist)
	Tree			tree;
	register Attr_avlist	avlist;
{
	register Tree_attr attr;
	register Tree_info *tinfo = TREE_PRIVATE(tree);
	Rectobj_list	*link;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
		    xv_super_set_avlist(tree, &tree_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(tree);
			return(set_result);
		}
	}

	while (attr = (Tree_attr) * avlist++)
	  switch (attr) {
	    case TREE_ADD_LINK:
		tree_link_child(tree, *avlist, *(avlist+1));
		avlist += 2;
		tinfo->manage_self= TRUE;
		break;

	    case TREE_UNLINK:
		tree_unlink_child(*avlist++);
		tinfo->manage_self= TRUE;
		break;

	    case TREE_PARENT_DISTANCE:
		tinfo->parent_distance = (unsigned short)*avlist++;
		tinfo->manage_self= TRUE;
		break;

	    case TREE_LAYOUT:
		tinfo->layout = (Tree_layout) *avlist++;
		tinfo->manage_self= TRUE;
		break;

	    case TREE_BORDER:
		tinfo->border = (unsigned short) *avlist++;
		tinfo->manage_self= TRUE;
		break;

	    case XV_END_CREATE:
		tinfo->root_node = xv_create(tree, RECTOBJ,
			RECTOBJ_SELECTABLE, FALSE, 
			RECTOBJ_GEOMETRY_SILENT, TRUE,
			NULL);
		tinfo->manage_self= FALSE;
		break;

	    default:
		avlist = attr_skip(attr, avlist);

	  }

	if(rectobj_finish_set1(tree)) {
		if(tinfo->manage_self &&
		   RF_IS_SET(RECTOBJ_PRIVATE(tree), RF_MANAGE_CHILDREN))
			tree_layout_resize(tree);
		rectobj_finish_set2(tree);
	}

	return(XV_SET_DONE);
}


/*ARGSUSED*/
Pkg_private Xv_opaque
tree_get_attr(tree, status, which_attr, avlist)
	Tree		tree;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Tree_info  *tinfo = TREE_PRIVATE(tree);
	Tree_layout_data *layout_data;
	Xv_opaque	arg;

	switch (which_attr) {

		case TREE_LINK_FROM:
			arg = (Xv_opaque)*avlist;
			if(arg != tree) {
			    layout_data = get_layout_data(arg);
			    if(layout_data) {
			      if(layout_data->from_link == tinfo->root_node)
			        return (Xv_opaque) tree;
			      return (Xv_opaque) layout_data->from_link;
			    }
			}
			return (Xv_opaque) 0;

		case TREE_LINK_TO_LIST:
			arg = (Xv_opaque)*avlist;
			if(arg == tree)
			    layout_data = get_layout_data(tinfo->root_node);
			else
			    layout_data = get_layout_data(arg);
			if(layout_data)
				return (Xv_opaque) layout_data->children;
			return (Xv_opaque) 0;

		case TREE_PARENT_DISTANCE:
			return (Xv_opaque) tinfo->parent_distance;

		case TREE_LAYOUT:
			return (Xv_opaque) tinfo->layout;

		case TREE_BORDER:
			return (Xv_opaque) tinfo->border;

		case TREE_DRAWLINE:
			arg = (Xv_opaque)*avlist;
			if((arg != tree) &&
			   (layout_data = get_layout_data(arg)))
				return (Xv_opaque) layout_data->line_to;
			return (Xv_opaque) 0;

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}

/*ARGSUSED*/
Pkg_private int
tree_destroy(tree, status)
	Tree		tree;
	Destroy_status	status;
{
	Tree_info	*tinfo = TREE_PRIVATE(tree);
	
	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	rectobj_destroy_children(tree);

	free(tinfo);
	return XV_OK;
}


static void
tree_link_child(tree, from, to)
	Tree	tree;
	Rectobj	from;
	Rectobj to;
{
	Tree_info *tinfo = TREE_PRIVATE(tree);
	Tree_layout_data *layout_data;
	Tree_layout_data *layout_data_to;
	Rectobj_info	 *rinfo_from;
	Rectobj_info	 *rinfo_to;

	if(from == tree)
		from = tinfo->root_node;
	rinfo_from = RECTOBJ_PRIVATE(from);
	layout_data = get_layout_data_private(rinfo_from);
	if(!layout_data)
		return;
	rinfo_to = RECTOBJ_PRIVATE(to);
	layout_data_to = get_layout_data_private(rinfo_to);

	if(!layout_data_to) {
		/*
		 * If layout_data_to is zero, then a tree isn't set as 
		 * RECTOBJ_PARENT.  Instead of failing, force the parent
		 * to be as it should by inheriting from's parent.
		 * This will cause the layout data to be created in
		 * tree_add_child_proc below.
		 *
		 * There is another case (not handled): the parent for
		 * each is set, but is not the same.
		 */
		xv_set(to, RECTOBJ_PARENT, tree, NULL);
		layout_data_to = get_layout_data_private(rinfo_to);
	}
	layout_data->children = list_concat(
		layout_data->children, &layout_data_to->siblings);
	if(layout_data_to->set_initial_position == 0)
		layout_data_to->set_initial_position = 1;
	layout_data_to->from_link = from;

	if(RF_IS_SET(rinfo_from, RF_PAINTED))
		RF_SET(rinfo_to, RF_PAINTED);
	else
		RF_UNSET(rinfo_to, RF_PAINTED);

	tree_set_show_flag(layout_data_to->children, 
			RF_IS_SET(rinfo_from, RF_PAINTED));
}

static void
tree_unlink_child(child)
	Rectobj		child;
{
	Tree_layout_data *layout_data_to;
	Tree_layout_data *layout_data_from;
	Rectobj_info	 *rinfo_from;
	Rectobj_info	 *rinfo_to;
	Listnode	*node;

	rinfo_to = RECTOBJ_PRIVATE(child);
	layout_data_to = get_layout_data_private(rinfo_to);
	if(!layout_data_to)
		return;

	if(!layout_data_to->from_link)
		return;	/* not linked to anything */

	rinfo_from = RECTOBJ_PRIVATE(layout_data_to->from_link);
	layout_data_from = get_layout_data_private(rinfo_from);
	if(!layout_data_from)
		return;
	node = list_find(layout_data_from->children, (void*)child);
	layout_data_from->children = (Rectobj_list*) list_first(
						list_unlink_node(node));
	rectobj_repaint_rect(child, NULL, TRUE);
	rectobj_repaint_rect(layout_data_to->line_to, NULL, TRUE);

	layout_data_to->from_link = NULL;

	if(RF_IS_SET(rinfo_from, RF_PAINTED))
		RF_SET(rinfo_to, RF_PAINTED);
	else
		RF_UNSET(rinfo_to, RF_PAINTED);

	tree_set_show_flag(layout_data_to->children, 
			RF_IS_SET(rinfo_from, RF_PAINTED));
}


static void
tree_paint_nodes(list, dpy, win, xrects)
	Rectobj_list	*list;
	Display		*dpy;
	Window		win;
	Xv_xrectlist	*xrects;
{
	Rectobj		child;
	Tree_layout_data *layout_data;

	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		layout_data = get_layout_data(child);
		rectobj_paint_child(layout_data->line_to, dpy, win, xrects);
		rectobj_paint_child(child, dpy, win, xrects);
		if(layout_data->children)
			tree_paint_nodes(layout_data->children, dpy, win, xrects);
	}
}

void
tree_paint_proc(tree, dpy, win, xrects)
	Tree tree;
	Display *dpy;
	Window win;
	Xv_xrectlist *xrects;
{
	Tree_info	*tinfo = TREE_PRIVATE(tree);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(tree);
	Rectobj_list	*list;
	Rectobj		child;
	Tree_layout_data *layout_data;

	if(!RF_IS_SET(rinfo, RF_MANAGE_CHILDREN))
		return;

	layout_data = get_layout_data(tinfo->root_node);
	list = layout_data->children;
	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		layout_data = get_layout_data(child);
		tree_paint_nodes(layout_data->children, dpy, win, xrects);
	}

	layout_data = get_layout_data(tinfo->root_node);
	list = layout_data->children;
	list_for(list) {
		rectobj_paint_child(RECTOBJ_LIST_HANDLE(list), dpy, win, xrects);
	}

#ifdef TREE_DEBUG_ROUTINES
	border = tinfo->border;
	layout = tinfo->layout;
	line_heap_list = tinfo->line_heap_list;
	tree_paint_contour(tinfo->root_node, dpy, win);
#endif

}

static void
tree_set_show_flag(list, how)
	Rectobj_list	*list;
	int		how;
{
	Rectobj		child;
	Rectobj_info	*rinfo;
	Tree_layout_data *layout_data;

	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		rinfo = RECTOBJ_PRIVATE(child);
		if(how)
			RF_SET(rinfo, RF_PAINTED);
		else
			RF_UNSET(rinfo, RF_PAINTED);
		layout_data = get_layout_data_private(rinfo);
		if(layout_data->children)
			tree_set_show_flag(layout_data->children, how);
	}
}


void tree_set_geometries();
void tree_set_xy();
void tree_set_links();
void tree_move_links();
void tree_calc_positions();
void tree_attach_parent();
Polyline *tree_line();
void tree_layout();
void tree_layout_leaf();
int tree_join();
int tree_merge();
int tree_offset();
Polyline *tree_bridge();
void tree_calc_breadth_depth();
void tree_set_breadth_depth();

/* These are copied from tree private to avoid passing them around */
static Listnode *line_heap_list;
static Tree_layout layout;
static unsigned short parent_distance;
static unsigned short border;

#ifdef TREE_DEBUG_ROUTINES
static void tree_paint_contour();
static void tree_recurse_paint_contour();
static void tree_paint_sub_contour();
#endif


static void
tree_layout(tinfo)
	Tree_info *tinfo;
{
	parent_distance = tinfo->parent_distance;
	layout = tinfo->layout;
	border = tinfo->border;

#ifndef TREE_DEBUG_ROUTINES
	if(tinfo->line_heap_list) {
		list_for(tinfo->line_heap_list)
			free(list_handle(tinfo->line_heap_list));
		list_destroy(tinfo->line_heap_list);
		tinfo->line_heap_list = line_heap_list = NULL;
	}
#endif
	if(tinfo->root_node) {
		tree_calc_positions(tinfo->root_node);
		tinfo->line_heap_list = line_heap_list;
	}
	line_heap_list = NULL;
}


static void
tree_set_geometries(tinfo, rect)
	Tree_info	*tinfo;
	Rect		*rect;
{
	Rect root_rect;
	Tree_layout_data *t;

	layout = tinfo->layout;

	t = get_layout_data(tinfo->root_node);
	root_rect.r_width = root_rect.r_height = 0;

	if(tinfo->layout == TREE_LAYOUT_HORIZONTAL) {
		root_rect.r_top = rect->r_top - t->top +
			(rect->r_height - (t->bottom - t->top))/2;
		root_rect.r_left = rect->r_left;
	} else {
		root_rect.r_left = rect->r_left - t->top +
			(rect->r_width - (t->bottom - t->top))/2;
		root_rect.r_top = rect->r_top;
	}
	
	rectobj_set_geometry(tinfo->root_node, &root_rect);
	tree_set_xy(tinfo->root_node, root_rect.r_left, root_rect.r_top);
	tree_set_links(tinfo->root_node);
}



static void
tree_set_xy(rectobj, x, y)
	Rectobj rectobj;
	int x, y;
{
	Rectobj_info	*rinfo;
	Rectobj_list	*children;
	Rectobj		child;
	Rect		new_rect;
	Tree_layout_data *t;
	extern int	rectobj_hack_no_old_rect_repaint;

	t = get_layout_data(rectobj);
	if(!t)
		return;
	children = t->children;

	list_for(children) {
		child = RECTOBJ_LIST_HANDLE(children);
		rinfo= RECTOBJ_PRIVATE(child);
		t = get_layout_data_private(rinfo);
		if(!t)
			continue;
		new_rect.r_width = rinfo->rect.r_width;
		new_rect.r_height= rinfo->rect.r_height;
		if(layout == TREE_LAYOUT_HORIZONTAL) {
			x+= t->offset.x;
			y+= t->offset.y;
		} else { 
			y+= t->offset.x;
			x+= t->offset.y;
		}
		new_rect.r_left = x;
		new_rect.r_top  = y;

		if(t->set_initial_position == 1) {
			/* to reduce flashing when positioning initially */
			t->set_initial_position = 2;
			rectobj_hack_no_old_rect_repaint = TRUE;
		}

		rectobj_set_geometry(child, &new_rect);
		tree_set_xy(child, x, y);
	}
}


static void
tree_move_links(rectobj)
	Rectobj rectobj;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj_list	*list;
	Tree_layout_data *t;
	Rect		*rect, *parent_rect;


	t = get_layout_data_private(rinfo);
	if(!t)
		return;

	if(t->from_link) {
	  rect = &rinfo->rect;
	  parent_rect = &(RECTOBJ_PRIVATE(t->from_link)->rect);

	  if(layout == TREE_LAYOUT_HORIZONTAL)
	    xv_set(t->line_to, 
		DRAWLINE_X0, parent_rect->r_left + parent_rect->r_width + 1,
		DRAWLINE_Y0, parent_rect->r_top  + parent_rect->r_height/2,
		DRAWLINE_X1, rect->r_left - 1,
		DRAWLINE_Y1, rect->r_top + rect->r_height /2,
		NULL);
	  else
	    xv_set(t->line_to, 
		DRAWLINE_X0, parent_rect->r_left  + parent_rect->r_width/2,
		DRAWLINE_Y0, parent_rect->r_top + parent_rect->r_height + 1,
		DRAWLINE_X1, rect->r_left + rect->r_width /2,
		DRAWLINE_Y1, rect->r_top - 1,
		NULL);
	}

	list = t->children;
	list_for(list)
		tree_move_links(RECTOBJ_LIST_HANDLE(list));
}


static void
tree_set_links(rectobj)
	Rectobj	rectobj;
{
	Rectobj_list *list;
	Tree_layout_data *t;
	Rectobj_list *list1;

	/*
	 * this steps down two levels before moving the lines appropriately
	 * because the root rectobj is a proxy for the tree, and since
	 * the lines are owned by the 'to' node, we don't want this
	 * invisible node to be pointed at by the first level objects.
	 */

	t = get_layout_data(rectobj);
	if(!t)
		return;
	list = t->children; 
	list_for(list) {
		t = get_layout_data(RECTOBJ_LIST_HANDLE(list));
		if(!t)
			continue;
		list1 = t->children;
		list_for(list1) 
			tree_move_links(RECTOBJ_LIST_HANDLE(list1));
	}
}

static void 
tree_calc_positions(rectobj)
	Rectobj rectobj;
{
	Rectobj_list *children;
	Tree_layout_data *t;
	Rect	*rect;

	t = get_layout_data(rectobj);
	if(!t)
		return;
	children = t->children;
	rect = &(RECTOBJ_PRIVATE(rectobj)->rect);

	t->offset.x = t->offset.y = 0;
	memset((char*)&t->contour, 0, sizeof(struct polygon));

	/* depth first traversal */

	if(t->children) {
		list_for(children)
			tree_calc_positions(RECTOBJ_LIST_HANDLE(children));
		tree_attach_parent(rect, t);
	} else 
		tree_layout_leaf(rect, t);
}


static void 
tree_layout_leaf(rect, t)
	Rect *rect;
	Tree_layout_data *t;
{
	if(layout == TREE_LAYOUT_HORIZONTAL) {
		t->contour.upper.tail= tree_line(rect->r_width + 2*border, 0, 
						(Polyline*)0);
		t->contour.upper.head= t->contour.upper.tail;
		t->contour.lower.tail= tree_line(0, -rect->r_height - 2*border, 
						(Polyline*)0);
		t->contour.lower.head= tree_line(rect->r_width + 2*border, 0, 
						t->contour.lower.tail);
	} else {
		t->contour.upper.tail= tree_line(rect->r_height + 2*border, 0, 
						(Polyline*)0);
		t->contour.upper.head= t->contour.upper.tail;
		t->contour.lower.tail= tree_line(0, -rect->r_width - 2*border, 
						(Polyline*)0);
		t->contour.lower.head= tree_line(rect->r_height + 2*border, 0, 
						t->contour.lower.tail);
	}
	tree_set_breadth_depth(rect, t);
}

static void 
tree_attach_parent(rect, t)
	Rect *rect;
	Tree_layout_data *t;
{
	int h;
	int x;
	int y1;	/* 'y' position of upper head */
	int y2; /* 'y' position of lower head */
	Tree_layout_data *t1;

	h = tree_join(t);
	x= border + parent_distance;
	
	if(layout == TREE_LAYOUT_HORIZONTAL) {
		y2= (h - rect->r_height)/2 - border;
		y1= y2 + rect->r_height + 2*border-h;
	} else {
		y2= (h - rect->r_width)/2 - border;
		y1= y2 + rect->r_width + 2*border-h;
	}

	t1 = get_layout_data(RECTOBJ_LIST_HANDLE(t->children));

	/*
	 * Shift the first child to position relative to parent
	 * The other children will follow because their offsets are 
	 * relative to the first child.
	 */
	if(layout == TREE_LAYOUT_HORIZONTAL)
		t1->offset.x = x+rect->r_width;
	else
		t1->offset.x = x+rect->r_height;

	t1->offset.y = y1;

	/*
	 * look out here, the t->contour.*.head pointers were previously 
	 * changed by the join function.  Fig. 8b shows how these new lines
	 * are added, so the parent links to the child's contour.
	 */
	if(layout == TREE_LAYOUT_HORIZONTAL) {
	  t->contour.upper.head = 
	    tree_line(rect->r_width, 0,tree_line(x,y1,t->contour.upper.head));
	  t->contour.lower.head =
	    tree_line(rect->r_width, 0, tree_line(x,y2,t->contour.lower.head));
	} else {
	  t->contour.upper.head = 
	    tree_line(rect->r_height, 0,tree_line(x,y1,t->contour.upper.head));
	  t->contour.lower.head =
	    tree_line(rect->r_height, 0, tree_line(x,y2,t->contour.lower.head));
	}
	tree_calc_breadth_depth(rect, t);
}

static int 
tree_join(t)
	Tree_layout_data *t;
{
	Tree_layout_data *c;
	int		d, h, sum;
	Rectobj_list	*children;
	Rectobj_info	*child_rinfo;

	children= t->children;

	child_rinfo = RECTOBJ_PRIVATE(RECTOBJ_LIST_HANDLE(children));

	/*
	 * the parent's contour is set to the first child's contour.
	 */
	c = get_layout_data_private(child_rinfo);
	t->contour= c->contour;

	if(layout == TREE_LAYOUT_HORIZONTAL) 
		sum= h= child_rinfo->rect.r_height + 2*border;
	else
		sum= h= child_rinfo->rect.r_width + 2*border;

	children = list_next(children);
	/* 
	 * For the rest of the children...
	 * The call to tree_merge() grows the contour of the parent by 
	 * adding each child's contour in turn.
	 * The offset settings line up the children horizontally, and 
	 * bump them vertically.
	 */
	for( ; children; children = list_next(children)) {

		child_rinfo = RECTOBJ_PRIVATE(RECTOBJ_LIST_HANDLE(children));
		c = get_layout_data_private(child_rinfo);
		if(!c)
			continue;
		/*
		 */
		d= tree_merge(&t->contour, &c->contour);
		c->offset.y= d+h;
		c->offset.x= 0;

		if(layout == TREE_LAYOUT_HORIZONTAL) 
		  h= child_rinfo->rect.r_height + 2*border;
		else
		  h= child_rinfo->rect.r_width + 2*border;

		sum+= d+h;
	}
	return sum;
}



static int 
tree_merge(c1, c2)
	struct polygon *c1, *c2;
{
	int x, y, total, d;
	Polyline *lower, *upper, *b;

	x= y= total= 0;
	upper= c1->lower.head;
	lower= c2->upper.head;

	/*
	 * tree_merge() walks through two contours and prevents overlap.
	 * The lower contour of the upper node is compared against the
	 * upper contour of the lower node.
	 */
	while(lower && upper) { 		/* compute offset total */

		d= tree_offset(x,y,lower->dx, lower->dy, upper->dx, upper->dy);
		y+= d;
		total+= d;

		if(x+lower->dx<=upper->dx){
			y+= lower->dy;
			x+= lower->dx;
			lower = lower->link;
		} else {
			y-= upper->dy;
			x-= upper->dx;
			upper= upper->link;
		}
	}
						/* store result in c1 */
	if (lower) {
		b= tree_bridge(c1->upper.tail, 0, 0, lower, x, y);
		c1->upper.tail= (b->link) ? c2->upper.tail : b;
		c1->lower.tail= c2->lower.tail;
	} else {
		b= tree_bridge(c2->lower.tail, x, y, upper, 0, 0);
		if(!b->link)
			c1->lower.tail = b;
	}
	c1->lower.head= c2->lower.head;

	return total;
}


static int 
tree_offset(p1, p2, a1, a2, b1, b2)
	int p1, p2, a1, a2, b1, b2;
{
	int d, s, t;

	if(b1 <= p1 || p1+a1<=0) 
		return 0;

	t= b1*a2 - a1*b2;
	if(t > 0) {
		if(p1 < 0) {
			s= p1*a2; 
			d= s/a1 - p2;
		}
		else if (p1 > 0) {
			s= p1*b2;
			d= s/b1 - p2;
		}
		else
			d= -p2;
	}
	else {
		if (b1<p1+a1) {
			s= (b1-p1) * a2;
			d= b2 - (p2 + s/a1);
		}
		else if(b1 > p1+a1) {
			s=(a1+p1)*b2;
			d= s/b1 - (p2+a2);
		}
		else 
			d= b2-(p2+a2);
	}
	return MAX(0,d);
}


static Polyline *
tree_bridge(line1, x1, y1, line2, x2, y2)
	Polyline *line1, *line2;
	int x1, y1, x2, y2;
{
	int dy, dx, s;
	Polyline *r;

	dx= x2+line2->dx - x1;
	if(line2->dx == 0)
		dy= line2->dy;
	else {
		s= dx*line2->dy;
		dy= s/line2->dx;
	}
	r= tree_line(dx, dy, line2->link);
	line1->link = tree_line(0,y2+line2->dy-dy-y1, r);
	return r;
}


#define HEAPSIZE 64
/*
 * assemble a polyline and return it.
 *
 * This scheme of allocating and tracking memory references is overly
 * elaborate because the tree layout algorithm makes spagetti out of 
 * the links between these critters.  This also saves much time when 
 * gc'ing the mess.
 *
 * The article indicates that there's a 6 to 1 ratio in polylines to
 * tree nodes, so the heap size is, by rule of thumb 10, times that size.
 */
static Polyline *
tree_line(dx, dy, link)
	short dx, dy;
	Polyline *link;
{
	Polyline *val;
	static int heap_used;

	if(!line_heap_list || heap_used >= HEAPSIZE) {
	  line_heap_list= list_concat(list_alloc_node(), line_heap_list);
	  line_heap_list->handle= (Polyline*)calloc(HEAPSIZE, sizeof(Polyline));
	  heap_used = 0;
	}

	val= (Polyline*)list_handle(line_heap_list)+heap_used;
	heap_used++;

	val->dx= dx;
	val->dy= dy;
	val->link= link;
	return(val);
}


static void
tree_calc_breadth_depth(rect, t)
	Rect	*rect;	/* parent rect */
	Tree_layout_data *t;
{
	Tree_layout_data *t1;
	Rectobj_list 	*children 	= t->children;
	int 		offsety 	= 0;
	int 		offsetx 	= 0;
	int		diff;		/* parent-child size difference */
	Rectobj_info	*child_rinfo;

	tree_set_breadth_depth(rect, t);

	list_for(children) {
		child_rinfo = RECTOBJ_PRIVATE(RECTOBJ_LIST_HANDLE(children));
		t1 = get_layout_data_private(child_rinfo);

		if(layout == TREE_LAYOUT_HORIZONTAL)
		  diff = (child_rinfo->rect.r_height - rect->r_height)/2;
		else
		  diff = (child_rinfo->rect.r_width - rect->r_width)/2;

		offsety += t1->offset.y;
		offsetx += t1->offset.x;

		if(t->top > t1->top + offsety + diff)
			t->top = t1->top + offsety + diff;
		if(t->bottom < t1->bottom + offsety + diff)
			t->bottom = t1->bottom + offsety + diff;
		if(t->depth < t1->depth + offsetx)
			t->depth = t1->depth + offsetx;
	}
}


static void
tree_set_breadth_depth(rect, t)
	Rect	*rect;
	Tree_layout_data *t;
{
	int half;

	if(layout == TREE_LAYOUT_HORIZONTAL) {
		half = rect->r_height/2;
		t->depth = rect->r_width + border*2;
		t->bottom = rect->r_height - half + border;
	} else {
		half = rect->r_width/2;
		t->depth = rect->r_height + border*2;
		t->bottom = rect->r_width - half + border;
	}
	t->top = -(half + border);
}


static void
tree_layout_resize(tree)
	Tree		tree;
{
	Tree_info	*tinfo = TREE_PRIVATE(tree);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(tree);
	Tree_layout_data *t;
	short		sizex;
	short		sizey;
	int		new_size;

	tree_layout(tinfo);
	t = get_layout_data(tinfo->root_node);
	
	new_size = FALSE;

	if(tinfo->layout == TREE_LAYOUT_HORIZONTAL) {
		sizey = rinfo->rect.r_height;
		sizex = rinfo->rect.r_width;
	} else {
		sizex = rinfo->rect.r_height;
		sizey = rinfo->rect.r_width;
	}

	if(((t->bottom - t->top) > sizey) ||
	   (sizey > (t->bottom - t->top) + tinfo->parent_distance)) {
		/*
		 * Increase or decrease to keep size of tree in 
		 * relation to size of children
		 */
		sizey = (t->bottom - t->top) + tinfo->parent_distance;
		new_size = TRUE;
	}
	if((t->depth > sizex) || 
	   (sizex > t->depth + tinfo->parent_distance)) {
		sizex = t->depth + tinfo->parent_distance;
		new_size = TRUE;
	}
	if(new_size) {
		if(tinfo->layout == TREE_LAYOUT_HORIZONTAL) {
		    rinfo->rect.r_width	= MAX(sizex, rinfo->rect.r_width);
		    rinfo->rect.r_height= MAX(sizey, rinfo->rect.r_height);
		} else {
		    rinfo->rect.r_width	= MAX(sizey, rinfo->rect.r_width);
		    rinfo->rect.r_height= MAX(sizex, rinfo->rect.r_height);
		}
		(void) rectobj_geometry_manage(tree, &rinfo->rect);
	}
	tree_set_geometries(tinfo, &rinfo->rect);
	tinfo->manage_self = FALSE;
}


void
tree_set_geometry_proc(tree, newrect, oldrect)
	Tree	tree;
	Rect	*newrect;
	Rect	*oldrect;
{
	Tree_info	*tinfo = TREE_PRIVATE(tree);

	if(RF_IS_SET(RECTOBJ_PRIVATE(tree), RF_MANAGE_CHILDREN)) {
		tree_layout(tinfo);
		tree_set_geometries(tinfo, newrect);
	}
}


void 
tree_manage_child_proc(tree, child, child_newrect, child_oldrect)
	Tree	tree;
	Rectobj	child;
	Rect	*child_newrect;
	Rect	*child_oldrect;
{
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(tree);
	Tree_layout_data *t;

	if((!RF_IS_SET(rinfo, RF_MANAGE_CHILDREN)) 	||
	  (!(t = get_layout_data(child)))		||
	  (!t->from_link)  /* not linked up */	) {
		rectobj_set_geometry(child, child_newrect);
		return;
	}
	tree_layout_resize(tree);
}


void 
tree_add_child_proc(tree, child, rect)
	Tree	tree;
	Rectobj	child;
	Rect	*rect;
{
	Tree_layout_data *t;
	Rectobj_info	*child_rinfo;
	static short	managed_child = TRUE;

	/*
	 * Nodes of the tree have layout_data, drawlines that
	 * connect the nodes don't.
	 */
	if(managed_child) {
		t = xv_alloc(Tree_layout_data);
		t->siblings.handle = (void*) child;
		managed_child = FALSE;
		t->line_to = xv_create(tree, DRAWLINE,
				RECTOBJ_SELECTABLE, FALSE,
				RECTOBJ_GEOMETRY_SILENT, TRUE,
				NULL);
		managed_child= TRUE;
		child_rinfo = RECTOBJ_PRIVATE(child);
		child_rinfo->layout_data = (void*) t;
	}
}

void 
tree_del_child_proc(tree, child, oldrect)
	Tree	tree;
	Rectobj	child;
	Rect	*oldrect;
{
	Tree_info	*tinfo = TREE_PRIVATE(tree);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(tree);
	Tree_layout_data *t;
	Rectobj_list 	*list;

	t = get_layout_data(child);
	if(!t)
		return;
	rectobj_set_delay_repaint(tree, TRUE);
	tree_unlink_child(child);
 	xv_destroy(t->line_to);

	/* if destroying a child, also destroy the nodes below it in tree */
	if(RF_IS_SET(RECTOBJ_PRIVATE(child), RF_STATE_DESTROYING))
	  while(list = t->children)
		xv_destroy(RECTOBJ_LIST_HANDLE(list));

	free(t);

  	RECTOBJ_PRIVATE(child)->layout_data = (void*) 0;

	/* don't want to do layout during self destruction... */
	if(RF_IS_SET(rinfo, RF_MANAGE_CHILDREN) &&
	  RF_IS_SET(rinfo, RF_STATE_CREATED) &&
	  (tinfo->root_node != child))
		tree_layout_resize(tree);

	rectobj_set_delay_repaint(tree, FALSE);
}


#ifdef TREE_DEBUG_ROUTINES

void tree_recurse_paint_contour();
void tree_paint_sub_contour();

Display *dpy;
GC      gc;
Window  win;
 
static void
tree_paint_contour(rectobj, dpy_arg, win_arg)
	Rectobj rectobj;
	Display *dpy_arg;
	Window  win_arg;
{
	Shared_info *sinfo;

	if(!line_heap_list)
		return;
	
	sinfo = (Shared_info*) RECTOBJ_PRIVATE(rectobj)->shared_info;
	dpy = dpy_arg;
	win = win_arg;
	gc = XCreateGC(dpy, xv_get(sinfo->canvas_shell, XV_XID), 0, 0);
	XSetForeground(dpy, gc, 
			xv_get(sinfo->canvas_shell, WIN_FOREGROUND_COLOR));
	tree_recurse_paint_contour(rectobj);
	XFreeGC(dpy, gc);
}

/* 
 * recursive -- show contours of all children 
 */
static void
tree_recurse_paint_contour(rectobj)
	Rectobj rectobj;
{
	Tree_layout_data *t;
	Rectobj_list *children;

	tree_paint_sub_contour(rectobj);
	t = get_layout_data(rectobj);
	if(!t)
		return;
	children = t->children;
	list_for(children)
		tree_recurse_paint_contour(RECTOBJ_LIST_HANDLE(children));
}

/*
 * show contour associated with this node only 
 */
static void
tree_paint_sub_contour(rectobj)
	Rectobj rectobj;
{
	Tree_layout_data *t;
	Polyline *p;
	int x,y,h;

	int loop = 0;

	t = get_layout_data( rectobj );
	if(!t)
		return;
	x = xv_get(rectobj, RECTOBJ_X);
	y = xv_get(rectobj, RECTOBJ_Y);
	if(layout == TREE_LAYOUT_HORIZONTAL) {
		h = xv_get(rectobj, XV_HEIGHT);
	} else {
		h = xv_get(rectobj, XV_WIDTH);
	}

	XSetLineAttributes(dpy, gc, 0, LineOnOffDash, CapButt, JoinBevel);
	/* draw the bounding box of the sub tree */
	if(layout == TREE_LAYOUT_HORIZONTAL) {
		XDrawRectangle(dpy, win, gc, 
			x-border, y + h/2 + t->top,
			t->depth, t->bottom - t->top -1);
	} else {
		XDrawRectangle(dpy, win, gc, 
			x + h/2 + t->top, y-border,
			t->bottom - t->top -1, t->depth);
	}
	XSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinBevel);

	if(layout == TREE_LAYOUT_VERTICAL)
		return;	/* no contour debug code for vertical */

	y = y - border;

	for(p=t->contour.upper.head;p;p=p->link) {
		XDrawLine(dpy, win, gc, x, y, x+p->dx, y+p->dy);
		x= x+p->dx;
		y= y+p->dy;
		loop++;
		if(loop>500) {
			printf("debug: loop in the links\n");
			break;
		}
	}
	loop = 0;
	x = xv_get(rectobj, RECTOBJ_X);
	y = xv_get(rectobj, RECTOBJ_Y) + xv_get(rectobj, XV_HEIGHT)+border;
	for(p=t->contour.lower.head;p;p=p->link) {
		XDrawLine(dpy, win, gc, x, y, x+p->dx, y+p->dy);
		x= x+p->dx;
		y= y+p->dy;
		loop++;
		if(loop>500) {
			printf("debug: loop in the links\n");
			break;
		}
	}
}

#endif TREE_DEBUG_ROUTINES

