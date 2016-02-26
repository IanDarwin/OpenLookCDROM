/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char sccsid[] = "@(#)array.c 1.16 92/10/29";
#endif
#endif

#include <X11/Xlib.h>
#include <xview/xv_xrect.h>
#include "rectobj_impl.h"
#include <sspkg/array.h>


#define CF_UNMANAGED 	(1<<0)
#define CF_NEW_CHILD	(1<<1)
#define CF_FLAG_MASK	(CF_UNMANAGED | CF_NEW_CHILD)

typedef struct array_layout_data {
	short	column;
	short	row;
	short	cflags;
} Array_layout_data;

#define AF_AUTO_POSITION	(1<<0)
#define AF_RELAYOUT		(1<<1)
#define AF_VLINES		(1<<2)
#define AF_HLINES		(1<<3)
#define AF_FLAG_MASK		(AF_AUTO_POSITION 	| \
				AF_RELAYOUT 		| \
				AF_VLINES		| \
				AF_HLINES		)

typedef struct array_tile_info {

	short	column_gap;
	short	row_gap;  
	short	column_width;
	short	row_height;

	short	n_columns;
	short	n_rows;
	short	array_size; /* n_columns * n_rows */
	short	n_managed;/* number of managed children */

	Array_tile_layout layout;
	Array_tile_align align;

	short	aflags;
	short	auto_layout;	/* use aflags */
	short	relayout;	/* use aflags */

	Rectobj	*arrayp;

} Array_tile_info;

#define ARRAY_TILE_PRIVATE(array_tile)    XV_PRIVATE(Array_tile_info, Array_tile_struct, array_tile)


Pkg_private int 	array_tile_init();
Pkg_private Xv_opaque	array_tile_set_avlist();
Pkg_private Xv_opaque	array_tile_get_attr();
Pkg_private int 	array_tile_destroy();

	void	array_tile_paint_proc();
	Rectobj	array_tile_map_event_proc();
	void	array_tile_set_geometry_proc();
	void	array_tile_manage_child_proc();
	void	array_tile_del_child_proc();
	void	array_tile_add_child_proc();

static	void	array_tile_compact();
static	void	calc_array_rect_size();
static	void	calc_child_row_column();
static	void	calc_child_rect();
static	void	set_children_rect();
static	void	array_tile_reset_dimensions();
static	void	array_tile_unmanage_child();
static	Rectobj	*position_to_childp();
static	void	rebuild_arrayp();
static	int	position_child();
static	void	position_children();
static	void	array_tile_shrink();

#define get_layout_data_private( _rinfo_ )	\
		(Array_layout_data *) ( _rinfo_ )->layout_data
#define get_layout_data(_rectobj_) 		\
		(Array_layout_data*)(RECTOBJ_PRIVATE( _rectobj_ )->layout_data)
#define SET_ROW 1
#define SET_COLUMN 2


/*ARGSUSED*/
Pkg_private int
array_tile_init(parent, array_tile, avlist)
	Xv_opaque	parent;
	Array_tile		array_tile;
	Attr_avlist	avlist;
{
	Array_tile_info	*ainfo;
	Array_tile_struct	*array_tile_object;
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(array_tile);
	static Rectobj_ops rectobj_ops = {
		1000,
		array_tile_paint_proc,
		rectobj_event_proc,
		array_tile_map_event_proc,
		array_tile_set_geometry_proc,
		array_tile_manage_child_proc,
		array_tile_add_child_proc,
		array_tile_del_child_proc,
	};

	ainfo = xv_alloc(Array_tile_info);
	array_tile_object = (Array_tile_struct*) array_tile;
	array_tile_object->private_data = (Xv_opaque) ainfo;

	ainfo->column_gap 	= 15;
	ainfo->row_gap 		= 15;
	ainfo->column_width 	= 40;
	ainfo->row_height	= 20;
	ainfo->layout		= ARRAY_TILE_LAYOUT_ROW;
	ainfo->align		= ARRAY_TILE_ALIGN_CENTER;
	FLAG_SET(ainfo->aflags,	AF_AUTO_POSITION);
	array_tile_reset_dimensions(ainfo, 4, 1);
	calc_array_rect_size(ainfo, &rinfo->rect);
	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

	return(XV_OK);
}


Pkg_private Xv_opaque
array_tile_set_avlist(array_tile, avlist)
	Array_tile			array_tile;
	register Attr_avlist	avlist;
{
        register Array_tile_attr attr;
        register Array_tile_info *ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info		 *rinfo = RECTOBJ_PRIVATE(array_tile);
	Rectobj_info 		 *child_rinfo;
	int			 rows;
	int			 columns;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
		    xv_super_set_avlist(array_tile, ARRAY_TILE, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(array_tile);
			return(set_result);
		}
	}

	while (attr = (Array_tile_attr) * avlist++)
	  switch (attr) {

		case ARRAY_TILE_N_COLUMNS:
			columns = (int)*avlist++;
			if(columns < 1)
				columns = 1;
			array_tile_reset_dimensions(ainfo, columns, 
					ainfo->n_rows);
			rebuild_arrayp(array_tile);
			calc_array_rect_size(ainfo, &rinfo->rect);
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			break;

		case ARRAY_TILE_N_ROWS:
			rows = (int)*avlist++;
			if(rows < 1)
				rows = 1;
			array_tile_reset_dimensions(ainfo, ainfo->n_columns, 
				rows);
			rebuild_arrayp(array_tile);
			calc_array_rect_size(ainfo, &rinfo->rect);
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			break;

		case ARRAY_TILE_COLUMN_GAP:
			ainfo->column_gap = (int)*avlist++;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			calc_array_rect_size(ainfo, &rinfo->rect);
			break;

		case ARRAY_TILE_ROW_GAP:
			ainfo->row_gap = (int)*avlist++;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			calc_array_rect_size(ainfo, &rinfo->rect);
			break;
 
		case ARRAY_TILE_COLUMN_WIDTH:
			ainfo->column_width = (int)*avlist++;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			calc_array_rect_size(ainfo, &rinfo->rect);
			break;

		case ARRAY_TILE_ROW_HEIGHT:
			ainfo->row_height = (int)*avlist++;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			calc_array_rect_size(ainfo, &rinfo->rect);
			break;

		case ARRAY_TILE_COLUMN:
			(void) position_child(array_tile, ainfo, 
				(Rectobj) *avlist, 
				(int) *(avlist+1), 0, 
				SET_COLUMN);
			avlist+=2;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			break;
			
		case ARRAY_TILE_ROW:
			(void) position_child(array_tile, ainfo, 
				(Rectobj) *avlist, 
				0, (int) *(avlist+1), 
				SET_ROW);
			avlist+=2;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			break;
			
		case ARRAY_TILE_POSITION:
			(void) position_child(array_tile, ainfo, 
				(Rectobj) *avlist, 
				(int) *(avlist+1), (int) *(avlist+2), 
				SET_ROW|SET_COLUMN);
			avlist+=3;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			break;

		case ARRAY_TILE_LAYOUT:
			ainfo->layout = (Array_tile_layout)*avlist++;
			FLAG_SET(ainfo->aflags, AF_RELAYOUT);
			break;

		case ARRAY_TILE_AUTO_POSITION:
			if(*avlist++) {
				FLAG_SET(ainfo->aflags, 
					AF_RELAYOUT|AF_AUTO_POSITION);
				array_tile_compact(ainfo);
			} else
			  FLAG_UNSET(ainfo->aflags, 
					(AF_RELAYOUT|AF_AUTO_POSITION),
					AF_FLAG_MASK);
			break;

		case ARRAY_TILE_VLINES:
			if(*avlist++)
				FLAG_SET(ainfo->aflags, AF_VLINES);
			else
				FLAG_UNSET(ainfo->aflags, AF_VLINES,
					AF_FLAG_MASK);
			RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			break;

		case ARRAY_TILE_HLINES:
			if(*avlist++)
				FLAG_SET(ainfo->aflags, AF_HLINES);
			else
				FLAG_UNSET(ainfo->aflags, AF_HLINES,
					AF_FLAG_MASK);
			RF_SET(rinfo, (RF_REPAINT|RF_CLEAR));
			break;


		case ARRAY_TILE_ALIGN:
			ainfo->align = (Array_tile_align)*avlist++;
			ainfo->relayout = TRUE;
			break;

		case XV_END_CREATE:
			break;

	    default:
		avlist = attr_skip(attr, avlist);

	  }

	if(rectobj_finish_set1(array_tile)) {
		/*
		if(!RF_IS_SET(rinfo, RF_MANAGE_CHILDREN))
			return;
		*/

		if(FLAG_IS_SET(ainfo->aflags, AF_RELAYOUT)) {
			position_children(ainfo);
			set_children_rect(array_tile);
			FLAG_TOGGLE(ainfo->aflags, AF_RELAYOUT);
		}
		rectobj_finish_set2(array_tile);
	}

	return(XV_SET_DONE);
}


/*ARGSUSED*/
Pkg_private Xv_opaque
array_tile_get_attr(array_tile, status, which_attr, avlist)
	Array_tile		array_tile;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Array_tile_info *ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj		child;
	Rectobj_info	*child_rinfo;

	switch (which_attr) {
		case ARRAY_TILE_COLUMN_GAP:
			return (Xv_opaque) ainfo->column_gap;

		case ARRAY_TILE_ROW_GAP:
			return (Xv_opaque) ainfo->row_gap;

		case ARRAY_TILE_COLUMN_WIDTH:
			return (Xv_opaque) ainfo->column_width;

		case ARRAY_TILE_ROW_HEIGHT:
			return (Xv_opaque) ainfo->row_height;

		case ARRAY_TILE_N_COLUMNS:
			return (Xv_opaque) ainfo->n_columns;

		case ARRAY_TILE_N_ROWS:
			return (Xv_opaque) ainfo->n_rows;

		case ARRAY_TILE_COLUMN:
			child = (Rectobj)*avlist;
			child_rinfo = RECTOBJ_PRIVATE(child);
			return (Xv_opaque) 
				((Array_layout_data*)child_rinfo->layout_data)
					->column;

		case ARRAY_TILE_ROW:
			child = (Rectobj)*avlist;
			child_rinfo = RECTOBJ_PRIVATE(child);
			return (Xv_opaque) 
				((Array_layout_data*)child_rinfo->layout_data)
					->row;

		case ARRAY_TILE_POSITION: {
			Array_layout_data layout_data;
			Rectobj		*childp;

			layout_data.column = (int)*avlist;
			layout_data.row = (int)*(avlist+1);
			layout_data.cflags = 0;
			if(childp = position_to_childp(ainfo, &layout_data))
				return (Xv_opaque) *childp;
			return (Xv_opaque) NULL;
			}

		case ARRAY_TILE_LAYOUT:
			return (Xv_opaque) ainfo->layout;


		case ARRAY_TILE_ALIGN:
			return (Xv_opaque) ainfo->align;

		case ARRAY_TILE_AUTO_POSITION:
			return (Xv_opaque) 
				FLAG_TRUE(ainfo->aflags, AF_AUTO_POSITION);

		case ARRAY_TILE_VLINES:
			return (Xv_opaque) 
				FLAG_TRUE(ainfo->aflags, AF_VLINES);

		case ARRAY_TILE_HLINES:
			return (Xv_opaque) 
				FLAG_TRUE(ainfo->aflags, AF_HLINES);

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}


/*ARGSUSED*/
Pkg_private int
array_tile_destroy(array_tile, status)
	Array_tile		array_tile;
	Destroy_status	status;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(array_tile);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	/*
	 * setting this flag before destroying avoids having to rework
	 * the whole array as children are destroyed.
	 */
	FLAG_UNSET(ainfo->aflags, AF_AUTO_POSITION, AF_FLAG_MASK);
	rectobj_destroy_children(array_tile);
	free(ainfo->arrayp);
	free(ainfo);

	return XV_OK;
}


/*
 * calculate the dimensions of the array_tile object.
 */
static void
calc_array_rect_size(ainfo, rect)
	Array_tile_info	*ainfo;
	Rect		*rect;
{
	rect->r_width = ainfo->column_gap +
		ainfo->n_columns * (ainfo->column_gap + ainfo->column_width);

	rect->r_height = ainfo->row_gap +
		ainfo->n_rows * (ainfo->row_gap + ainfo->row_height);
}


/*
 * calculate a child's position
 */
static void
calc_child_rect(array_tile, child, rect)
	Array_tile	array_tile;
	Rectobj		child;
	Rect		*rect;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(array_tile);
	Rectobj_info	*child_rinfo = RECTOBJ_PRIVATE(child);
	Array_layout_data *layout_data = get_layout_data_private(child_rinfo);

	rect->r_left = 
	  rinfo->rect.r_left +
	  ainfo->column_gap + 
	  layout_data->column * (ainfo->column_gap + ainfo->column_width);

	switch(ainfo->align) {
		case ARRAY_TILE_ALIGN_NORTHWEST:
		case ARRAY_TILE_ALIGN_SOUTHWEST:
		case ARRAY_TILE_ALIGN_WEST:
			break;

		case ARRAY_TILE_ALIGN_NORTH:
		case ARRAY_TILE_ALIGN_SOUTH:
		case ARRAY_TILE_ALIGN_CENTER:
			rect->r_left +=
			  (ainfo->column_width/2 - child_rinfo->rect.r_width/2);
			break;

		case ARRAY_TILE_ALIGN_NORTHEAST:
		case ARRAY_TILE_ALIGN_EAST:
		case ARRAY_TILE_ALIGN_SOUTHEAST:
			rect->r_left +=
			  (ainfo->column_width - child_rinfo->rect.r_width);
	}

	rect->r_top = 
		rinfo->rect.r_top +
		ainfo->row_gap + 
		layout_data->row * (ainfo->row_gap + ainfo->row_height);

	switch(ainfo->align) {
		case ARRAY_TILE_ALIGN_NORTH:
		case ARRAY_TILE_ALIGN_NORTHEAST:
		case ARRAY_TILE_ALIGN_NORTHWEST:
			break;

		case ARRAY_TILE_ALIGN_EAST:
		case ARRAY_TILE_ALIGN_WEST:
		case ARRAY_TILE_ALIGN_CENTER:
			rect->r_top +=
			  (ainfo->row_height/2 - child_rinfo->rect.r_height/2);
			break;

		case ARRAY_TILE_ALIGN_SOUTH:
		case ARRAY_TILE_ALIGN_SOUTHWEST:
		case ARRAY_TILE_ALIGN_SOUTHEAST:
			rect->r_top +=
			  (ainfo->row_height - child_rinfo->rect.r_height);
	}
}


/*
 * calculate the row-column position of a child, given its position.
 */
static void
calc_child_row_column(ainfo, layout_data, n)
	Array_tile_info		*ainfo;
	Array_layout_data 	*layout_data;
	int			n;
{
	/* given an index into the array, calculate the row/column position */

	switch(ainfo->layout) {
	  case ARRAY_TILE_LAYOUT_COLUMN:
		layout_data->column	= n / ainfo->n_rows;
		layout_data->row	= n % ainfo->n_rows;
		break;

	  case ARRAY_TILE_LAYOUT_ROW:
		layout_data->column	= n % ainfo->n_columns;
		layout_data->row	= n / ainfo->n_columns;
		break;
	}
}


/*
 * return a pointer to the child at a row column position
 */
static Rectobj *
position_to_childp(ainfo, layout_data)
	Array_tile_info	*ainfo;
	Array_layout_data *layout_data;
{
	int i;

	if(	(FLAG_IS_SET(layout_data->cflags, CF_UNMANAGED))||
		(layout_data->row < 0) 				||
		(layout_data->column < 0) 			||
		(layout_data->row >= ainfo->n_rows) 		||
		(layout_data->column >= ainfo->n_columns))
			return (Rectobj*) NULL;

	switch(ainfo->layout) {
	  case ARRAY_TILE_LAYOUT_ROW:
		i = ainfo->n_columns*layout_data->row + layout_data->column;
		break;

	  case ARRAY_TILE_LAYOUT_COLUMN:
		i = ainfo->n_rows*layout_data->column + layout_data->row;
		break;
	}
	return ainfo->arrayp+i;
}


/*
 * packs all children as tightly as possible into upper left corner.
 */
static void
array_tile_compact(ainfo)
	Array_tile_info	*ainfo;
{
	int		hole;	/* index to holes */
	int		index;	/* index to next non-hole */
	Rectobj		child;

	/* make a compact array out of a sparse one */

	if(!FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION))
		return;

	for( index =0, hole=0; hole < ainfo->array_size; hole++) {

		/* skip until a hole is found */
		if(*(ainfo->arrayp+hole) != (Rectobj) NULL)
			continue;

		/* get next non-hole */
		index = hole;
		while(*(ainfo->arrayp+index) == (Rectobj) NULL) {
			index++;
			if(index >= ainfo->array_size)
				return; /* all done */
		}

		/* move the rectobj at index to hole */
		child = *(ainfo->arrayp+index);
		*(ainfo->arrayp+index) = (Rectobj) NULL;
		*(ainfo->arrayp+hole) = child;

		calc_child_row_column(ainfo, get_layout_data(child), hole);
	}
}


/*
 * change the number of rows or the number of columns of the array_tile.
 * call rebuild_arrayp after this to restore the children's position.
 */
static void
array_tile_reset_dimensions(ainfo, columns, rows)
	Array_tile_info	*ainfo;
	int		columns;
	int		rows;
{
	Array_tile_info	tmp_ainfo;
	int i;

	tmp_ainfo = *ainfo;
	tmp_ainfo.n_columns	= MAX(1, columns);
	tmp_ainfo.n_rows 	= MAX(1, rows);

	tmp_ainfo.array_size = rows * columns;

	if(FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION)	&& 
	   ((tmp_ainfo.n_rows < ainfo->n_rows)		|| 
	    (tmp_ainfo.n_columns < ainfo->n_columns)))
		position_children(&tmp_ainfo);

	if(tmp_ainfo.array_size < ainfo->array_size)
		for(i=tmp_ainfo.array_size; i<ainfo->array_size; i++)
			array_tile_unmanage_child(ainfo, *(ainfo->arrayp+i));

	if(ainfo->arrayp) {

	  ainfo->arrayp = (Rectobj*) realloc(
			ainfo->arrayp, tmp_ainfo.array_size * sizeof(Rectobj));

	  if(ainfo->array_size < tmp_ainfo.array_size)
		  memset((char*)(ainfo->arrayp + ainfo->array_size), 0,
		    (tmp_ainfo.array_size - ainfo->array_size)*sizeof(Rectobj));

	} else
	  ainfo->arrayp = (Rectobj*) calloc(
				tmp_ainfo.array_size, sizeof(Rectobj));

	ainfo->n_columns= tmp_ainfo.n_columns;
	ainfo->n_rows 	= tmp_ainfo.n_rows;
	ainfo->array_size = tmp_ainfo.array_size;
}


/*
 * restore ainfo -> arrayp from the layout data of the array_tile's children.
 */
static void
rebuild_arrayp(array_tile)
	Array_tile	array_tile;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_list	*list;
	Array_layout_data *layout_data;
	Rectobj		child;
	Rectobj		*childp;

	/* zero arrayp out */
	memset( (char*) ainfo->arrayp, 0, ainfo->array_size * sizeof(Rectobj) );
	list = RECTOBJ_PRIVATE(array_tile)->children;

	/* rebuild arrayp from layout_data attached to children */
	list_for(list) {
		child = RECTOBJ_LIST_HANDLE(list);
		layout_data = get_layout_data(child);
		if(FLAG_IS_SET(layout_data->cflags, CF_UNMANAGED))
			continue;
		if((layout_data->row < 0) 		||
		   (layout_data->row >= ainfo->n_rows)	||
		   (layout_data->column < 0)		||
		   (layout_data->column >= ainfo->n_columns)) {
			array_tile_unmanage_child(ainfo, child);
			continue;
		}
		if(childp = position_to_childp(ainfo, layout_data))
			*childp = child;
	}
	array_tile_compact(ainfo);
}


/*
 * remove a child from the set of managed children.
 */
static void
array_tile_unmanage_child(ainfo, child)
	Array_tile_info	*ainfo;
	Rectobj		child;
{
	Array_layout_data *layout_data;
	Rectobj	*childp;

	if(!child)
		return;
	layout_data = get_layout_data(child);

	if(FLAG_IS_SET(layout_data->cflags, CF_UNMANAGED))
		return;
	if(childp = position_to_childp(ainfo, layout_data))
		*childp = (Rectobj) NULL;
	FLAG_SET(layout_data->cflags, CF_UNMANAGED);
	layout_data->row = layout_data->column = -1;
	ainfo->n_managed--;
}


/*
 * explicitly position a child.
 */
static int
position_child(array_tile, ainfo, child, column, row, which)
	Array_tile	array_tile;
	Array_tile_info	*ainfo;
	Rectobj		child;
	int		column;
	int		row;
	int		which;
{
	Array_layout_data *layout_data;
	Rectobj		*childp;
	Rectobj		*newposp;
	int		geometry_change = FALSE;
	int		previously_unmanaged;
	
	if(!child)
		return;
	layout_data = get_layout_data(child);
	if(!layout_data)
		return;

	previously_unmanaged = FLAG_IS_SET(layout_data->cflags, CF_UNMANAGED);

	if(!previously_unmanaged)
		/* clear out the last position that the child was at */
		if(childp = position_to_childp(ainfo, layout_data))
			*childp = (Rectobj) NULL;

	if(which & SET_COLUMN) {
		if((column < 0) || (column >= ainfo->n_columns)) {
			array_tile_unmanage_child(ainfo, child);
			array_tile_shrink(array_tile, child);
			rectobj_repaint_rect(child, NULL, TRUE);
			return;
		}
		layout_data->column = column;
	}

	if(which & SET_ROW) {
		if((row < 0) || (row >= ainfo->n_rows)) {
			array_tile_unmanage_child(ainfo, child);
			array_tile_shrink(array_tile, child);
			rectobj_repaint_rect(child, NULL, TRUE);
			return;
		}
		layout_data->row = row;
	}

	if(FLAG_IS_SET(layout_data->cflags, CF_NEW_CHILD))
		calc_child_row_column(ainfo, layout_data, ainfo->n_managed);

	if(previously_unmanaged) {
		ainfo->n_managed++;
		FLAG_TOGGLE(layout_data->cflags, CF_UNMANAGED);
	}

	if(FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION) && 
	  (ainfo->n_managed > ainfo->array_size)) {
		switch(ainfo->layout) {
		  case ARRAY_TILE_LAYOUT_ROW:
			array_tile_reset_dimensions(ainfo, 
					ainfo->n_columns, ainfo->n_rows+1);
			break;
		  case ARRAY_TILE_LAYOUT_COLUMN:
			array_tile_reset_dimensions(ainfo, 
					ainfo->n_columns+1, ainfo->n_rows);
			break;
		}
		rebuild_arrayp(array_tile);
		geometry_change = TRUE;
	}

	/* 
	 * If there is an object at the destination spot, remove it.
	 */
	if(	(newposp = position_to_childp(ainfo, layout_data)) && 
		(*newposp != child))
		array_tile_unmanage_child(ainfo, *newposp);

	*newposp = child;

	if(FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION) && 
	   !FLAG_IS_SET(layout_data->cflags, CF_NEW_CHILD))
		array_tile_compact(ainfo);

	if(previously_unmanaged) {
		Rect *child_rect = &(RECTOBJ_PRIVATE(child)->rect);
		if(child_rect->r_width > ainfo->column_width) {
			ainfo->column_width = child_rect->r_width;
			geometry_change = TRUE;
		}

		if(child_rect->r_height > ainfo->row_height) {
			ainfo->row_height = child_rect->r_height;
			geometry_change = TRUE;
		} 
	}

	if(geometry_change) {
		FLAG_SET(ainfo->aflags, AF_RELAYOUT);
		calc_array_rect_size(ainfo,
			&(RECTOBJ_PRIVATE(array_tile)->rect));
	}
	return geometry_change;
}


/*
 * set the row-column of all children according to their position in the array
 */
static void
position_children(ainfo)
	Array_tile_info	*ainfo;
{
	int 	i;
	Rectobj	child;

	for(i=0; i<ainfo->array_size; i++) {
		if(child = *(ainfo->arrayp+i))
			calc_child_row_column(ainfo, get_layout_data(child), i);
	}
}


static void
set_children_rect(array_tile)
	Array_tile	array_tile;
{
	Array_tile_info *ainfo = ARRAY_TILE_PRIVATE(array_tile);
	int i;
	Rectobj	child;
	Rectobj_info *rinfo;
 
	for(i=0; i<ainfo->array_size; i++)
		if(child = *(ainfo->arrayp+i)) {
			rinfo = RECTOBJ_PRIVATE(child);
			calc_child_rect(array_tile, child, &rinfo->rect);
			rectobj_set_geometry(child, &rinfo->rect);
		}
}


void
array_tile_paint_proc(array_tile, dpy, win, xrects)
        Array_tile array_tile;
        Display *dpy;
        Window win;
        Xv_xrectlist *xrects;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	register int 	i;

	/*
	if(!RF_IS_SET(RECTOBJ_PRIVATE(array_tile), RF_MANAGE_CHILDREN))
		return;
	*/

	for(i=0; i < ainfo->array_size; i++)
	    if(*(ainfo->arrayp+i))
		rectobj_paint_child(*(ainfo->arrayp+i), dpy, win, xrects);

	if(FLAG_IS_SET(ainfo->aflags, (AF_VLINES|AF_HLINES))) {
		/* 
		 * lines always paint, regardless of intersection with
		 * xrects.  Assumes clipping in the server is fairly cheap.
		 */
		Rectobj_info 	*rinfo = RECTOBJ_PRIVATE(array_tile);
		int		x, y, x0, y0;
		int		xlen, ylen;
		GC 		gc;

		gc = XCreateGC(dpy, win, 0, 0);

		if(xrects && xrects->count)
			XSetClipRectangles(dpy, gc,
				0, 0,
				xrects->rect_array,
				xrects->count,
				Unsorted);
 
		XSetForeground(dpy, gc, 
			pixel_fg(rinfo->shared_info, rinfo->fg_color));

		x0 = rinfo->rect.r_left + ainfo->column_gap / 2;
		y0 = rinfo->rect.r_top + ainfo->row_gap / 2;
		xlen = rinfo->rect.r_width - ainfo->column_gap - 1;
		ylen = rinfo->rect.r_height - ainfo->row_gap - 1;

		XDrawRectangle(dpy, win, gc,
				x0, y0, 
				xlen, ylen);

		if(FLAG_IS_SET(ainfo->aflags, AF_VLINES)) {
			x = x0;
			y = y0;
			for(i=1; i<ainfo->n_columns; i++) {
				x += ainfo->column_gap + ainfo->column_width;
				XDrawLine(dpy, win, gc,
					x, y,
					x, y+ylen);
			}
		}

		if(FLAG_IS_SET(ainfo->aflags, AF_HLINES)) {
			x = x0;
			y = y0;
			for(i=1; i<ainfo->n_rows; i++) {
				y += ainfo->row_gap + ainfo->row_height;
				XDrawLine(dpy, win, gc,
					x, y,
					x+xlen, y);
			}
		}

		XFreeGC(dpy, gc);
	}
}


Rectobj
array_tile_map_event_proc(rectobj, event)
	Rectobj         rectobj;
	Event          *event;
{
	Rectobj_list   *node;
	Rectobj         child;
	Rectobj_info   *rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj         return_val;

	if (!(RF_IS_SET(rinfo, RF_PAINTED)))
		return 0;

	node = rinfo->children;

	list_rof(node) {
		child = RECTOBJ_LIST_HANDLE(node);
		rinfo = RECTOBJ_PRIVATE(child);
		if ((!(RF_IS_SET(rinfo, RF_PAINTED))) ||
		    (!rect_includespoint(&rinfo->rect,
					 event_x(event), event_y(event))))
			continue;

		if ((FLAG_IS_SET((get_layout_data_private(rinfo))->cflags, CF_UNMANAGED)))
			continue;

		if (return_val = (rinfo->rectobj_ops->map_event_proc)
		    (child, event))
			return return_val;
	}
	return rectobj;

}


void
array_tile_set_geometry_proc(array_tile, newrect, oldrect)
	Array_tile	array_tile;
	Rect		*newrect;
	Rect		*oldrect;
{
	/*
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(array_tile);
	*/

	/* adapt to size changes... */

	set_children_rect(array_tile);
}


void
array_tile_manage_child_proc(array_tile, child, child_newrect, child_oldrect)
	Array_tile	array_tile;
	Rectobj		child;
	Rect		*child_newrect;
	Rect		*child_oldrect;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(array_tile);
	Array_layout_data *layout_data = get_layout_data(child);
	int	geo_manage = FALSE;


	if(FLAG_IS_SET(layout_data->cflags, CF_NEW_CHILD)) {
		/* 
		 * Position a new child.
		 */
		geo_manage = position_child(array_tile, ainfo, child, 0, 0, 0);
		FLAG_TOGGLE(layout_data->cflags, CF_NEW_CHILD);
	} else {
		if(FLAG_IS_SET(layout_data->cflags, CF_UNMANAGED)) {
			/* 
			 * The child isn't managed, give it want it wants and
			 * return.  It will not affect others until it 
			 * is managed.
			 */
			rectobj_set_geometry(child, child_newrect);
			return;
		}
		/*
		 * A old child has been resized, see if it effects 
		 * layout of other children.
		 */
		if(child_newrect->r_width > ainfo->column_width) {
			ainfo->column_width = child_newrect->r_width;
			geo_manage = TRUE;
		}

		if(child_newrect->r_height > ainfo->row_height) {
			ainfo->row_height = child_newrect->r_height;
			geo_manage = TRUE;
		} 
	}

	if(geo_manage || FLAG_IS_SET(ainfo->aflags, AF_RELAYOUT)) {
		Rect	rect;

		rect = rinfo->rect;
		calc_array_rect_size(ainfo, &rect);
		rectobj_geometry_manage(array_tile, &rect);
		set_children_rect(array_tile);
	} else {
		calc_child_rect(array_tile, child, child_newrect);
		rectobj_set_geometry(child, child_newrect);
	}
}


void
array_tile_add_child_proc(array_tile, child)
	Array_tile	array_tile;
	Rectobj		child;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info	*child_rinfo = RECTOBJ_PRIVATE(child);
	Array_layout_data* layout_data;

	layout_data =
	child_rinfo->layout_data = (void*) xv_alloc(Array_layout_data);

	FLAG_SET(layout_data->cflags, CF_UNMANAGED);
	layout_data->column = -1;
	layout_data->row = -1;

	if(FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION))
		FLAG_SET(layout_data->cflags, CF_NEW_CHILD);
}


void
array_tile_del_child_proc(array_tile, del_child)
	Array_tile	array_tile;
	Rectobj		del_child;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	Rectobj_info	*del_child_rinfo;
	Array_layout_data *layout_data;
	Rectobj		*childp;
	short		unmanaged;

	del_child_rinfo = RECTOBJ_PRIVATE(del_child);
	layout_data = get_layout_data_private(del_child_rinfo);
	unmanaged = FLAG_IS_SET(layout_data->cflags, CF_UNMANAGED);
	array_tile_unmanage_child(ainfo, del_child);
	free(layout_data);
	if(unmanaged || !FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION))
		return;
	array_tile_shrink(array_tile, del_child);
	position_children(ainfo);
	rectobj_geometry_manage(array_tile, NULL);
	set_children_rect(array_tile);
}


static void
array_tile_shrink(array_tile, child)
	Array_tile	array_tile;
	Rectobj		child;
{
	Array_tile_info	*ainfo = ARRAY_TILE_PRIVATE(array_tile);
	int rows, columns;

	if(!FLAG_IS_SET(ainfo->aflags, AF_AUTO_POSITION))
		return;

	array_tile_compact(ainfo);

	/* see if the array_tile should shrink */

	columns = ainfo->n_columns;
	rows = ainfo->n_rows;

	if(ainfo->layout == ARRAY_TILE_LAYOUT_COLUMN)
		columns = ((ainfo->n_managed-1)/ainfo->n_rows)+1;
	else
		rows = ((ainfo->n_managed-1)/ainfo->n_columns)+1;

	if((columns != ainfo->n_columns) || (rows != ainfo->n_rows)) {
		array_tile_reset_dimensions(ainfo, columns, rows);
		rebuild_arrayp(array_tile);
		calc_array_rect_size(ainfo,
			 &(RECTOBJ_PRIVATE(array_tile)->rect));
	}
}

