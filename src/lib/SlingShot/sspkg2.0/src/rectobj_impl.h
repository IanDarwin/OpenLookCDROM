/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef RECTOBJ_IMPL_DEFINED
#define RECTOBJ_IMPL_DEFINED

/* @(#)rectobj_impl.h 1.50 92/11/07 */

#include <xview/rect.h>
#include <xview/base.h>
#include <sspkg/rectobj.h>
#include "shared_info.h"

/* bit field macros */
#define FLAG_IS_SET(_var_, _flag_)	\
			((_var_) & (_flag_))

#define FLAG_SET(_var_, _flag_)		\
			((_var_) |= (_flag_))

#define FLAG_UNSET(_var_, _flag_, _mask_)\
			((_var_) &= ((_mask_) ^ (_flag_)))

#define FLAG_TOGGLE(_var_, _flag_)\
			((_var_) ^= (_flag_))

#define FLAG_TRUE(_var_, _flag_)	\
			(((_var_) & (_flag_)) == (_flag_))

/* rectobj flags */
#define RF_PAINTED		(1<<0)
#define RF_SELECTABLE		(1<<1)
#define RF_SELECTED		(1<<2)
#define RF_RESIZABLE		(1<<3)
#define RF_DRAGGABLE		(1<<4)
#define RF_ACCEPTS_DROP		(1<<5)
#define RF_REPAINT		(1<<6)	/* private */
#define RF_CLEAR		(1<<7)	/* private */
#define RF_STATE_INIT		(1<<8)	/* private */
#define RF_STATE_CREATED	(1<<9)	/* private */
#define RF_STATE_DESTROYING	(1<<10)	/* private */
#define RF_GEOMETRY_SILENT	(1<<11)
#define RF_GEO_PROC_IS_CALLER	(1<<12)	/* private */
#define RF_GEO_MANAGER_CALLED	(1<<13)	/* private */
#define RF_MANAGE_CHILDREN	(1<<14)
#define RF_HAS_EVENT_GRAB	(1<<15) /* private */
#define RF_MISC_FLAG1		(1<<16) 
#define RF_MISC_FLAG2		(1<<17) 
#define RF_MISC_FLAG3		(1<<18) 
#define RF_MISC_FLAG4		(1<<19) 
#define RF_MISC_CHILD_FLAG1	(1<<20)
#define RF_MISC_CHILD_FLAG2	(1<<21)
#define RF_HIGHLIGHT		(1<<22)
#define RF_PREHIGHLIGHT		(1<<23)
#define RF_PREDROP_HIGHLIGHT	(1<<24)
#define RF_EXCLUSIVE_SELECT	(1<<25)
#define RF_RESTACK_CHILDREN	(1<<26)
#define RF_FORCE_GEO_MANAGE	(1<<27)

#define RF_FLAG_MASK  		(\
	RF_PAINTED 		|\
	RF_SELECTABLE		|\
	RF_SELECTED		|\
	RF_RESIZABLE		|\
	RF_DRAGGABLE		|\
	RF_ACCEPTS_DROP		|\
	RF_REPAINT 		|\
	RF_CLEAR 		|\
	RF_STATE_INIT		|\
	RF_STATE_CREATED	|\
	RF_STATE_DESTROYING	|\
	RF_GEOMETRY_SILENT	|\
	RF_GEO_PROC_IS_CALLER	|\
	RF_GEO_MANAGER_CALLED	|\
	RF_MANAGE_CHILDREN	|\
	RF_HAS_EVENT_GRAB	|\
	RF_MISC_FLAG1		|\
	RF_MISC_FLAG2		|\
	RF_MISC_FLAG3		|\
	RF_MISC_FLAG4		|\
	RF_MISC_CHILD_FLAG1	|\
	RF_MISC_CHILD_FLAG2	|\
	RF_HIGHLIGHT		|\
	RF_PREHIGHLIGHT		|\
	RF_PREDROP_HIGHLIGHT	|\
	RF_EXCLUSIVE_SELECT	|\
	RF_RESTACK_CHILDREN	|\
	RF_FORCE_GEO_MANAGE	)

#define RF_IS_SET(_rinfo_, _flag_)	\
		FLAG_IS_SET((_rinfo_)->flags, (_flag_))

#define RF_SET(_rinfo_, _flag_)		\
		FLAG_SET((_rinfo_)->flags, (_flag_))

#define RF_UNSET(_rinfo_, _flag_)	\
		FLAG_UNSET((_rinfo_)->flags, (_flag_), RF_FLAG_MASK)

#define RF_TOGGLE(_rinfo_, _flag_)	\
		FLAG_TOGGLE((_rinfo_)->flags, (_flag_))

#define RF_TRUE(_rinfo_, _flag_)	\
		FLAG_TRUE((_rinfo_)->flags, (_flag_))


typedef struct rectobj_info {
	Rectobj_list   *children;
	Rectobj_list	listnode;	/* link to siblings */
	Shared_info	*shared_info;
	Rectobj         parent;
	unsigned short	n_children;

	unsigned long	flags;		/* must be >= 32 bits */

	Rect            rect;
	unsigned short	border;
	unsigned short	min_width;
	unsigned short	min_height;
	short		fg_color;
	short		bg_color;

	Rectobj_ops	*rectobj_ops;

	/* for while in context of xv_set */
	Rect		old_rect;
	Rectobj		previous_parent;
	unsigned short	invocation_level;

	void		*layout_data;

} Rectobj_info;

extern void	rectobj_internal_init();
extern void	rectobj_paint_proc();
extern void	rectobj_event_proc();
extern Rectobj	rectobj_map_event_proc();
extern void	rectobj_manage_child_proc();
extern void	rectobj_set_geometry_proc();
extern void	rectobj_add_child_proc();
extern void	rectobj_del_child_proc();
extern void	rectobj_finish_set_proc();
extern void	rectobj_set_delay_repaint();

extern void	bag_set_geometry_proc();
extern void	bag_manage_child_proc();
extern void	bag_add_child_proc();
extern void	bag_del_child_proc();
extern void	bag_set_anchored();
extern void	bag_set_border();

#define BAG_ANCHORED_FLAG	RF_MISC_FLAG1
#define BAG_AUTO_SHRINK_FLAG	RF_MISC_FLAG2

#define rectcmp(_r1, _r2) (memcmp((char*)(_r1), (char*)(_r2), sizeof(Rect)))

#define HIGHLIGHT_RECTOBJ(rinfo) (RF_IS_SET((rinfo), \
		(RF_PREHIGHLIGHT | RF_HIGHLIGHT | RF_PREDROP_HIGHLIGHT)))

#define RECTOBJ_PRIVATE(rectobj)	XV_PRIVATE(Rectobj_info, Rectobj_struct, rectobj)
#define RECTOBJ_PUBLIC(rectobj_info)	(Rectobj)((rectobj_info)->listnode.handle)
#endif
