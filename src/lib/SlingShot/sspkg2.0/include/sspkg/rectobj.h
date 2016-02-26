/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef RECTOBJ_DEFINED
#define RECTOBJ_DEFINED

/* @(#) rectobj.h 1.57 92/11/10  */

#include <xview/win_input.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <sspkg/list.h>

typedef Xv_opaque       Canvas_shell;
typedef	Xv_opaque 	Rectobj;
typedef	Xv_opaque 	Bag;

extern	Xv_pkg   	rectobj_pkg;
#define RECTOBJ 	&rectobj_pkg

extern	Xv_pkg   	bag_pkg;
#define BAG	 	&bag_pkg

typedef struct {
	union {
		Xv_generic_struct 	generic_parent_data;
		Xv_canvas		canvas_parent_data;
	} parent_data;
	Xv_opaque       private_data;
} Rectobj_struct;

#define ATTR_RECTOBJ			ATTR_PKG_UNUSED_LAST-10
#define RECTOBJ_ATTR(type, ordinal)	ATTR(ATTR_RECTOBJ, type, ordinal)

typedef enum {

	RECTOBJ_X		= RECTOBJ_ATTR(ATTR_INT,		1),
	RECTOBJ_Y		= RECTOBJ_ATTR(ATTR_INT,		2),
	RECTOBJ_MIN_WIDTH	= RECTOBJ_ATTR(ATTR_INT,		3),
	RECTOBJ_MIN_HEIGHT	= RECTOBJ_ATTR(ATTR_INT,		4),
	RECTOBJ_RESIZABLE	= RECTOBJ_ATTR(ATTR_BOOLEAN,		5),
	RECTOBJ_CANVAS		= RECTOBJ_ATTR(ATTR_OPAQUE, 		6),
	RECTOBJ_FG		= RECTOBJ_ATTR(ATTR_INT, 		7),
	RECTOBJ_BG		= RECTOBJ_ATTR(ATTR_INT,		8),
	RECTOBJ_BG2		= RECTOBJ_ATTR(ATTR_INT,		9),
	RECTOBJ_BG3		= RECTOBJ_ATTR(ATTR_INT,		10),
	RECTOBJ_WHITE		= RECTOBJ_ATTR(ATTR_INT,		11),
	RECTOBJ_BORDER		= RECTOBJ_ATTR(ATTR_SHORT,		12),
	RECTOBJ_CHILDREN	= RECTOBJ_ATTR(ATTR_OPAQUE, 		13),
	RECTOBJ_PARENT		= RECTOBJ_ATTR(ATTR_OPAQUE, 		14),
	RECTOBJ_NTH_CHILD 	= RECTOBJ_ATTR(ATTR_INT,		15),
	RECTOBJ_N_CHILDREN 	= RECTOBJ_ATTR(ATTR_INT,		16),
	RECTOBJ_STACKING_POSITION = RECTOBJ_ATTR(ATTR_INT,		17),
	RECTOBJ_RESTACK_CHILDREN = RECTOBJ_ATTR(ATTR_BOOLEAN,		18),

	RECTOBJ_SELECTED	= RECTOBJ_ATTR(ATTR_BOOLEAN, 		19),
	RECTOBJ_SELECTABLE 	= RECTOBJ_ATTR(ATTR_BOOLEAN, 		20),
	RECTOBJ_TOGGLE_STATE	= RECTOBJ_ATTR(ATTR_BOOLEAN,		21),

	RECTOBJ_EXCLUSIVE_SELECT= RECTOBJ_ATTR(ATTR_BOOLEAN,		22),

	/* paint styles */
	RECTOBJ_NORMAL		= RECTOBJ_ATTR(ATTR_NO_VALUE,		23),
	RECTOBJ_HIGHLIGHT	= RECTOBJ_ATTR(ATTR_NO_VALUE,		24),
	RECTOBJ_PREHIGHLIGHT	= RECTOBJ_ATTR(ATTR_NO_VALUE,		25),
	RECTOBJ_PREDROP_NORMAL	= RECTOBJ_ATTR(ATTR_NO_VALUE,		26),
	RECTOBJ_PREDROP_HIGHLIGHT= RECTOBJ_ATTR(ATTR_NO_VALUE,		27),

	RECTOBJ_GEOMETRY_SILENT	= RECTOBJ_ATTR(ATTR_BOOLEAN,		28),
	RECTOBJ_MANAGE_CHILDREN	= RECTOBJ_ATTR(ATTR_BOOLEAN,		29),

        RECTOBJ_DRAGGABLE	= RECTOBJ_ATTR(ATTR_BOOLEAN, 		30), 
        RECTOBJ_ACCEPTS_DROP	= RECTOBJ_ATTR(ATTR_BOOLEAN, 		31),

	RECTOBJ_MENU_KEY	= RECTOBJ_ATTR(ATTR_OPAQUE,		32),
	RECTOBJ_SEL_OWNER_KEY	= RECTOBJ_ATTR(ATTR_OPAQUE,		33),

	BAG_ANCHORED		= RECTOBJ_ATTR(ATTR_BOOLEAN,		34),
	BAG_AUTO_SHRINK		= RECTOBJ_ATTR(ATTR_BOOLEAN,		35),

	RECTOBJ_OPS		= RECTOBJ_ATTR(ATTR_OPAQUE,		100),
	RECTOBJ_PAINT_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR, 	101),
	RECTOBJ_EVENT_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	102),
	RECTOBJ_MAP_EVENT_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	103),
	RECTOBJ_SET_GEOMETRY_PROC = RECTOBJ_ATTR(ATTR_FUNCTION_PTR, 	104),
	RECTOBJ_MANAGE_CHILD_PROC =RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	105),
	RECTOBJ_ADD_CHILD_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	106),
	RECTOBJ_DEL_CHILD_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	107),
	RECTOBJ_NEW_PARENT_PROC = RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	108),
	RECTOBJ_START_DRAG_PROC = RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	109),
	RECTOBJ_STYLE_CHANGE_PROC = RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	110),
	RECTOBJ_SINGLE_CLICK_PROC = RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	111),
	RECTOBJ_DBL_CLICK_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR,	112),
        RECTOBJ_DROP_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR, 	113),
        RECTOBJ_SELECTION_PROC	= RECTOBJ_ATTR(ATTR_FUNCTION_PTR, 	114),

} Rectobj_attr;

#define RECTOBJ_FOREGROUND_COLOR	RECTOBJ_FG
#define RECTOBJ_BACKGROUND_COLOR	RECTOBJ_BG
#define RECTOBJ_MENU 			XV_KEY_DATA, RECTOBJ_MENU_KEY
#define RECTOBJ_SELECTION_OWNER		XV_KEY_DATA, RECTOBJ_SEL_OWNER_KEY


typedef	void	(*Proc_ptr) ();

typedef struct {
	int             ref_count;
	Proc_ptr        paint_proc;
	Proc_ptr	event_proc;
	Rectobj		(*map_event_proc) ();
	Proc_ptr        set_geometry_proc;
	Proc_ptr        manage_child_proc;
	Proc_ptr        add_child_proc;
	Proc_ptr        del_child_proc;
	Proc_ptr        new_parent_proc;
	Proc_ptr	start_drag_proc;
	Proc_ptr	style_change_proc;
	Proc_ptr	single_click_proc;
	Proc_ptr	dbl_click_proc;
	Proc_ptr	drop_proc;
	Proc_ptr	selection_proc;
} Rectobj_ops;


typedef Listnode Rectobj_list;
#define RECTOBJ_LIST_HANDLE(_list) (Rectobj)list_handle(_list)


/* public functions */
EXTERN_FUNCTION(void	*traverse_rectobj_tree, (Rectobj, void*(*)(), void*));
EXTERN_FUNCTION(void	rectobj_fit, (Rectobj));
EXTERN_FUNCTION(Rectobj	rectobj_upsearch, (Rectobj, Xv_opaque*, Attr_attribute, int));

/* subclass utilities */
EXTERN_FUNCTION(Rectobj	event_to_rectobj, (Rectobj, Event*));
EXTERN_FUNCTION(void 	rectobj_paint_children, (Rectobj, Display*, Window, Xv_xrectlist*));
EXTERN_FUNCTION(void	rectobj_paint_child, (Rectobj, Display*, Window, Xv_xrectlist*));
EXTERN_FUNCTION(void	rectobj_repaint_rect, (Rectobj, Rect*, int));
EXTERN_FUNCTION(void	rectobj_flush_repaint, (int));
EXTERN_FUNCTION(void	rectobj_invalidate_repaint, (Rectobj, Rect*));
EXTERN_FUNCTION(void	rectobj_finish_set, (Rectobj));
EXTERN_FUNCTION(int	rectobj_finish_set1, (Rectobj));
EXTERN_FUNCTION(void	rectobj_finish_set2, (Rectobj));
EXTERN_FUNCTION(void	rectobj_reset_set_info, (Rectobj));
EXTERN_FUNCTION(void	rectobj_min_enclosing_rect, (Rectobj_list*, Rect*));
EXTERN_FUNCTION(int	rectobj_geometry_manage, (Rectobj, Rect*));
EXTERN_FUNCTION(void	rectobj_set_geometry, (Rectobj, Rect*));
EXTERN_FUNCTION(void	rectobj_move_children, (Rectobj));
EXTERN_FUNCTION(void	rectobj_delta_move_children, (Rectobj, int, int));
EXTERN_FUNCTION(void	rectobj_set_delay_repaint, (Rectobj, int));
EXTERN_FUNCTION(void	rectobj_destroy_children, (Rectobj));
EXTERN_FUNCTION(void	rectobj_set_event_grab, (Canvas_shell, Rectobj, Proc_ptr, void*));
EXTERN_FUNCTION(void	rectobj_set_paint_style, (Rectobj, Event*, Attr_attribute));
EXTERN_FUNCTION(void	rectobj_set_stacking_position, (Rectobj, int));
EXTERN_FUNCTION(void	rectobj_simple_style_change_proc, (Rectobj, Event*, Attr_attribute, int));
EXTERN_FUNCTION(void	rectobj_recursive_style_change_proc, (Rectobj, Event*, Attr_attribute, int));

/* event handling utilities */
EXTERN_FUNCTION(void	rectobj_selection_event_proc, (Xv_window, Event*, Canvas_shell, Rectobj));
EXTERN_FUNCTION(void	rectobj_button_event_proc, (Xv_window, Event*, Canvas_shell, Rectobj));
EXTERN_FUNCTION(Rectobj_list *rectobj_get_selected_list, (void));
EXTERN_FUNCTION(void	rectobj_add_to_selected_list, (Rectobj, int, Event*));
EXTERN_FUNCTION(void	rectobj_del_from_selected_list, (Rectobj, Event*));
EXTERN_FUNCTION(void	rectobj_menu_show, (Xv_Window, Event*, Rectobj));
EXTERN_FUNCTION(void	rectobj_help_show, (Xv_Window, Event*, Rectobj));
EXTERN_FUNCTION(void	rectobj_process_drop_event, (Xv_window, Event*, Canvas_shell, Rectobj));
EXTERN_FUNCTION(void	rectobj_redo_start_drag, (Rectobj, int));


#endif

