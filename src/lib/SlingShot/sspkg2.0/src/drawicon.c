/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)drawicon.c 1.24 92/11/11";
#endif
#endif

#include <X11/Xlib.h>
#include <xview/rect.h>
#include <xview/xv_xrect.h>
#include <xview/win_input.h>
#include <xview/font.h>
#include <sspkg/drawobj.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"
#include <sspkg/canshell.h>
#include "dimage_impl.h"
#include "dtext_impl.h"


typedef struct drawicon_info {
	Drawimage	image;
	Drawtext	text;
	int		gap;
} Drawicon_info;

#define DRAWICON_PRIVATE(drawicon)	\
		XV_PRIVATE(Drawicon_info, Drawicon_struct, drawicon)

#define LAYOUT_VERTICAL RF_MISC_FLAG1


Pkg_private int 	drawicon_init();
Pkg_private Xv_opaque	drawicon_set_avlist();
Pkg_private Xv_opaque	drawicon_get_attr();
Pkg_private int 	drawicon_destroy();

Rectobj		drawicon_map_event_proc();
void 		drawicon_set_geometry_proc();
void		drawicon_manage_child_proc();
void		drawicon_single_click_proc();

/*ARGSUSED*/
Pkg_private int
drawicon_init(parent, drawicon, avlist)
	Xv_opaque	parent;
	Drawicon	drawicon;
	Attr_avlist	avlist;
{
	Drawicon_info	*dinfo;
	Rectobj_info 	*rinfo = RECTOBJ_PRIVATE(drawicon);
	Drawicon_struct	*drawicon_object;
	static Rectobj_ops rectobj_ops = {
		1000,
		rectobj_paint_proc,
		rectobj_selection_event_proc,
		drawicon_map_event_proc,
		drawicon_set_geometry_proc,
		drawicon_manage_child_proc,
		NULL,			/* add child */
		NULL,			/* del child */
		NULL,			/* new parent */
		NULL,			/* start_drag */
		rectobj_recursive_style_change_proc,
		drawicon_single_click_proc,
	};

	dinfo = xv_alloc(Drawicon_info);

	/*
	 * Create private parts of text and image so attrs can be set
	 * prior to real creation. 
	 */
	drawicon_private_diinfo = xv_alloc(Drawimage_info);
	drawicon_private_dtinfo = xv_alloc(Drawtext_info);
	/* following is only init necessary! */
	drawicon_private_dtinfo->justify = DRAWTEXT_JUSTIFY_LEFT;

	drawicon_object = (Drawicon_struct*) drawicon;
	drawicon_object->private_data = (Xv_opaque) dinfo;

	RF_SET(rinfo, (RF_SELECTABLE | LAYOUT_VERTICAL));
	rinfo->rectobj_ops = (Rectobj_ops*)&rectobj_ops;

/*
	dinfo->text = xv_create(drawicon, DRAWTEXT, 
			RECTOBJ_SELECTABLE, FALSE,
			RECTOBJ_GEOMETRY_SILENT, TRUE,
			NULL);

	dinfo->image = xv_create(drawicon, DRAWIMAGE, 
			RECTOBJ_SELECTABLE, FALSE,
			RECTOBJ_GEOMETRY_SILENT, TRUE,
			NULL);
*/
	dinfo->gap = 1;

	return(XV_OK);
}


Pkg_private Xv_opaque
drawicon_set_avlist(drawicon, avlist)
	Drawicon		drawicon;
	register Attr_avlist	avlist;
{
        register Drawobj_attr attr;
        register Drawicon_info *dinfo = DRAWICON_PRIVATE(drawicon);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawicon);
	short		recalc_text_size = FALSE;
	short		recalc_image_size = FALSE;
	static int	text_color = -1, image_color = -1;

	if(*avlist != XV_END_CREATE) {
		Xv_opaque set_result;
		set_result =
		  xv_super_set_avlist(drawicon, &drawicon_pkg, avlist);
		if(set_result != XV_OK) {
			rectobj_reset_set_info(drawicon);
			return(set_result);
		}
	}

	while (attr = (Drawobj_attr) * avlist++)
	  switch (attr) {

		case DRAWICON_TEXT:
		case DRAWICON_IMAGE:
			/* get only */
			avlist++;
			break;

		case DRAWTEXT_STRING:
		case DRAWTEXT_STRING_PTR:
		case DRAWTEXT_FONT:
		case DRAWTEXT_JUSTIFY:
		case DRAWTEXT_SHOW_UNDERLINE:
		case DRAWTEXT_LENGTH:
		case DRAWTEXT_EDITABLE:
			recalc_text_size = TRUE;
			/* handle repaint and resizing here... */
			if(RF_IS_SET(rinfo, RF_STATE_CREATED)) 
				drawtext_set_attr(DRAWTEXT_PRIVATE(dinfo->text),
					RECTOBJ_PRIVATE(dinfo->text),
					attr, (void*) *avlist);
			else
				drawtext_set_attr(drawicon_private_dtinfo,
					NULL,
					attr, (void*) *avlist);
			avlist++;
			break;

		case DRAWIMAGE_IMAGE1:
		case DRAWIMAGE_IMAGE2:
			recalc_image_size = TRUE;
		case DRAWIMAGE_IMAGE1_MASK:
		case DRAWIMAGE_IMAGE2_MASK:
			if(RF_IS_SET(rinfo, RF_STATE_CREATED)) 
				drawimage_set_attr(
					DRAWIMAGE_PRIVATE(dinfo->image),
					attr, (void*) *avlist);
			else
				drawimage_set_attr(drawicon_private_diinfo,
					attr, (void*) *avlist);
			avlist++;
			break;

		case DRAWICON_IMAGE_COLOR:
			if(RF_IS_SET(rinfo, RF_STATE_CREATED)) {
				xv_set(dinfo->image, 
					RECTOBJ_FG, *avlist, 
					NULL);
			} else {
				image_color = (int)*avlist;
			}
			avlist++;
			break;

		case DRAWICON_TEXT_COLOR:
			if(RF_IS_SET(rinfo, RF_STATE_CREATED)) {
				xv_set(dinfo->text, 
					RECTOBJ_FG, *avlist, 
					NULL);
			} else {
				text_color = (int)*avlist;
			}
			avlist++;
			break;

		case RECTOBJ_BG:
		case RECTOBJ_FG:
			if(RF_IS_SET(rinfo, RF_STATE_CREATED)) {
				xv_set(dinfo->text, 
					attr, *avlist,
					NULL);
				xv_set(dinfo->image, 
					attr, *avlist, 
					NULL);
			} else {
				if(attr == RECTOBJ_FG)
					image_color = text_color = (int)*avlist;
			}
			avlist++;
			break;

		case RECTOBJ_SELECTED: {
			Rectobj_info *rdtinfo = RECTOBJ_PRIVATE(dinfo->text);
			Rectobj_info *rdiinfo = RECTOBJ_PRIVATE(dinfo->image);
			
			/* this should be changed... */
			if(*avlist) {
			  RF_SET(rdtinfo, RF_SELECTED);
			  RF_SET(rdiinfo, RF_SELECTED);
			} else {
			  RF_UNSET(rdtinfo, RF_SELECTED);
			  RF_UNSET(rdiinfo, RF_SELECTED);
			}

			avlist++;
			}
			break;

		case DRAWICON_GAP:
			dinfo->gap = (int) *avlist++;
			break;

		case DRAWICON_LAYOUT_VERTICAL:
			if(*avlist++)
				RF_SET(rinfo, LAYOUT_VERTICAL);
			else
				RF_UNSET(rinfo, LAYOUT_VERTICAL);
			break;

		case XV_OWNER:
		case RECTOBJ_PARENT:
			if(rinfo->shared_info)
			  drawtext_set_attr(DRAWTEXT_PRIVATE(dinfo->text),
				RECTOBJ_PRIVATE(dinfo->text),
				DRAWTEXT_FONT, (void*)rinfo->shared_info->font);
			avlist++;
			recalc_text_size = TRUE;
			break;

		case XV_END_CREATE: {
        		Rectobj_info    *tinfo;
		        Rectobj_info    *iinfo;

			dinfo->text = xv_create(drawicon, DRAWTEXT, 
				RECTOBJ_SELECTABLE, FALSE,
				RECTOBJ_GEOMETRY_SILENT, TRUE,
				RECTOBJ_FG, text_color,
				NULL);

			dinfo->image = xv_create(drawicon, DRAWIMAGE, 
				RECTOBJ_SELECTABLE, FALSE,
				RECTOBJ_GEOMETRY_SILENT, TRUE,
				RECTOBJ_FG, image_color,
				NULL);

        		tinfo = RECTOBJ_PRIVATE(dinfo->text);
		        iinfo = RECTOBJ_PRIVATE(dinfo->image);

			if(RF_IS_SET(rinfo, LAYOUT_VERTICAL)) {
				rinfo->rect.r_width = MAX(tinfo->rect.r_width, 
					iinfo->rect.r_width);
				rinfo->rect.r_height= tinfo->rect.r_height + 
					iinfo->rect.r_height + dinfo->gap;
			} else {
				rinfo->rect.r_height= MAX(tinfo->rect.r_height, 
					iinfo->rect.r_height);
				rinfo->rect.r_width = tinfo->rect.r_width + 
					iinfo->rect.r_width + dinfo->gap;
			}

			RF_UNSET(tinfo, RF_GEOMETRY_SILENT);
			RF_UNSET(iinfo, RF_GEOMETRY_SILENT);
			drawicon_private_dtinfo = NULL;
			drawicon_private_diinfo = NULL;
			text_color = image_color = -1;
			}
			break;

		default:
			avlist = attr_skip(attr, avlist);

	  }


	if(RF_IS_SET(rinfo, RF_STATE_CREATED)) {
		if(recalc_text_size)
			drawtext_calc_rect(dinfo->text);
		if(recalc_image_size)
			drawimage_calc_rect(dinfo->image);
		if(recalc_image_size || recalc_image_size) {
        		Rectobj_info    *tinfo;
		        Rectobj_info    *iinfo;

        		tinfo = RECTOBJ_PRIVATE(dinfo->text);
		        iinfo = RECTOBJ_PRIVATE(dinfo->image);

			if(RF_IS_SET(rinfo, LAYOUT_VERTICAL)) {
				rinfo->rect.r_width = MAX(tinfo->rect.r_width, 
					iinfo->rect.r_width);
				rinfo->rect.r_height= tinfo->rect.r_height + 
					iinfo->rect.r_height + dinfo->gap;
			} else {
				rinfo->rect.r_height= MAX(tinfo->rect.r_height, 
					iinfo->rect.r_height);
				rinfo->rect.r_width = tinfo->rect.r_width + 
					iinfo->rect.r_width + dinfo->gap;
			}
		}
	}

	if(rectobj_finish_set1(drawicon))
		rectobj_finish_set2(drawicon);
	return(XV_SET_DONE);
}


/*ARGSUSED*/
Pkg_private Xv_opaque
drawicon_get_attr(drawicon, status, which_attr, avlist)
	Drawicon		drawicon;
	int		*status;
	register Attr_attribute which_attr;
	Attr_avlist	avlist;
{
	Drawicon_info	*dinfo = DRAWICON_PRIVATE(drawicon);
	Rectobj_info	*rinfo = RECTOBJ_PRIVATE(drawicon);

	switch (which_attr) {

		case DRAWICON_TEXT:
			return (Xv_opaque) dinfo->text;

		case DRAWICON_IMAGE:
			return (Xv_opaque) dinfo->image;

		case DRAWIMAGE_IMAGE1:
		case DRAWIMAGE_IMAGE2:
		case DRAWIMAGE_IMAGE1_MASK:
		case DRAWIMAGE_IMAGE2_MASK:
 			/* faster than: xv_get(dinfo->image, which_attr); */
			return (Xv_opaque)
				drawimage_get_attr(dinfo->image, status,
					which_attr, 0);

		case DRAWTEXT_STRING:
		case DRAWTEXT_STRING_PTR:
		case DRAWTEXT_FONT:
		case DRAWTEXT_JUSTIFY:
		case DRAWTEXT_SHOW_UNDERLINE:
		case DRAWTEXT_LENGTH:
		case DRAWTEXT_EDITABLE:
			/* faster than: xv_get(dinfo->text, which_attr); */
			return (Xv_opaque) 
				drawtext_get_attr(dinfo->text, status, 
					which_attr, 0);

		case DRAWICON_TEXT_COLOR:
			return (Xv_opaque) xv_get(dinfo->text, RECTOBJ_FG);

		case DRAWICON_IMAGE_COLOR:
			return (Xv_opaque) xv_get(dinfo->image, RECTOBJ_FG);

		case DRAWICON_GAP:
			return (Xv_opaque) dinfo->gap;

		case DRAWICON_LAYOUT_VERTICAL:
			return (Xv_opaque) (RF_TRUE(rinfo, LAYOUT_VERTICAL));

		default:
			*status = XV_ERROR;
			return (Xv_opaque) 0;
	}
}

/*ARGSUSED*/
Pkg_private int
drawicon_destroy(drawicon, status)
	Drawicon		drawicon;
	Destroy_status	status;
{
	Drawicon_info	*dinfo = DRAWICON_PRIVATE(drawicon);
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(drawicon);

	if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
		return XV_OK;

	free(dinfo);
	return XV_OK;
}


Rectobj
drawicon_map_event_proc(rectobj, event)
	Rectobj		rectobj;
	Event		*event;
{
	Drawicon_info	*dinfo = DRAWICON_PRIVATE(rectobj);
	Rectobj_info    *rinfo = RECTOBJ_PRIVATE(rectobj);
	Rectobj         return_val;
	Rectobj_info	*tmp;

	return_val = rectobj_map_event_proc(rectobj, event);
	if(return_val == rectobj)
		return 0;
	else
		return rectobj;
}

static void
set_child_positions(drawicon, newrect)
	Drawicon	drawicon;
	Rect		*newrect;
{
	Drawicon_info	*dinfo = DRAWICON_PRIVATE(drawicon);
        Rectobj_info    *child_rinfo;
	Rect		r;
	int		center;
	unsigned short	icon_size;

	if(RF_IS_SET(RECTOBJ_PRIVATE(drawicon), LAYOUT_VERTICAL)) {
		center = newrect->r_width/2 + newrect->r_left;

		child_rinfo = RECTOBJ_PRIVATE(dinfo->image);
		r = child_rinfo->rect;
		r.r_left = center - child_rinfo->rect.r_width/2;
		r.r_top = newrect->r_top;
		icon_size = r.r_height;
		rectobj_set_geometry(dinfo->image, &r);
	
		child_rinfo = RECTOBJ_PRIVATE(dinfo->text);
		r = child_rinfo->rect;
		r.r_left = center - child_rinfo->rect.r_width/2;
		r.r_top = newrect->r_top + icon_size + dinfo->gap;
		rectobj_set_geometry(dinfo->text, &r);
	} else {
		center = newrect->r_height/2 + newrect->r_top;

		child_rinfo = RECTOBJ_PRIVATE(dinfo->image);
		r = child_rinfo->rect;
		r.r_top = center - child_rinfo->rect.r_height/2;
		r.r_left = newrect->r_left;
		icon_size = r.r_width;
		rectobj_set_geometry(dinfo->image, &r);
	
		child_rinfo = RECTOBJ_PRIVATE(dinfo->text);
		r = child_rinfo->rect;
		r.r_top = center - child_rinfo->rect.r_height/2;
		r.r_left = newrect->r_left + icon_size + dinfo->gap;
		rectobj_set_geometry(dinfo->text, &r);
	}
}


void
drawicon_set_geometry_proc(drawicon, newrect, oldrect)
	Drawicon	drawicon;
	Rect		*newrect;
	Rect		*oldrect;
{
	set_child_positions(drawicon, newrect);
}


void
drawicon_manage_child_proc(drawicon, child, child_new_rect, child_old_rect)
        Rectobj drawicon;
        Rectobj child;
        Rect    *child_new_rect;
        Rect    *child_old_rect;
{
	Drawicon_info	*dinfo = DRAWICON_PRIVATE(drawicon);
        Rectobj_info    *rinfo = RECTOBJ_PRIVATE(drawicon);
        Rectobj_info    *tinfo = RECTOBJ_PRIVATE(dinfo->text);
        Rectobj_info    *iinfo = RECTOBJ_PRIVATE(dinfo->image);
	Rect		r;

	r.r_left= rinfo->rect.r_left;
	r.r_top = rinfo->rect.r_top;
	if(RF_IS_SET(RECTOBJ_PRIVATE(drawicon), LAYOUT_VERTICAL)) {
		r.r_width = MAX(tinfo->rect.r_width, iinfo->rect.r_width);
		r.r_height= tinfo->rect.r_height + iinfo->rect.r_height 
			+ dinfo->gap;
	} else {
		r.r_height = MAX(tinfo->rect.r_height, iinfo->rect.r_height);
		r.r_width = tinfo->rect.r_width + iinfo->rect.r_width 
			+ dinfo->gap;
	}

	if( (rinfo->rect.r_width != r.r_width) ||
	    (rinfo->rect.r_height!= r.r_height) )
		rectobj_geometry_manage(drawicon, &r);
	set_child_positions(drawicon, &rinfo->rect);
}


void
drawicon_single_click_proc(paint_window, event, canvas_shell, drawicon)
	Xv_window       paint_window;
	Event          *event;
	Canvas_shell    canvas_shell;
	Drawicon	drawicon;
{
	Drawicon_info  *dinfo = DRAWICON_PRIVATE(drawicon);
	Rectobj_info   *rinfo = RECTOBJ_PRIVATE(dinfo->text);

	if(RF_IS_SET(RECTOBJ_PRIVATE(drawicon), LAYOUT_VERTICAL)) {
		if(event_y(event) >= rinfo->rect.r_top)
			drawtext_edit_single_click(
				paint_window, event, canvas_shell, dinfo->text);
	} else {
		if(event_x(event) >= rinfo->rect.r_left)
			drawtext_edit_single_click(
				paint_window, event, canvas_shell, dinfo->text);
	}
}

