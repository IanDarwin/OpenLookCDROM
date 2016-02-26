/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include "lbcont_cui.h"


/*
 * Notify callback function for `lbch0crv'.
 */
Panel_setting
lbcont_rw_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);

	lbcont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcalgcrv'.
 */
void
lbcalgcrv_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
}

/*
 * Notify callback function for `lbchmxcrv'.
 */
Panel_setting
lbcont_lbcontpu_lbchmxcrv_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcangcrv'.
 */
Panel_setting
lbcont_lbcontpu_lbcangcrv_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcdhcrv'.
 */
Panel_setting
lbcont_lbcontpu_lbcdhcrv_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcdhjac'.
 */
Panel_setting
lbcont_lbcontpu_lbcdhjac_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcmaxit'.
 */
Panel_setting
lbcmaxit_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int	value = (int) xv_get(item, PANEL_VALUE);

	lbcont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcepscrv'.
 */
Panel_setting
lbcont_lbcontpu_lbcepscrv_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcmodit'.
 */
Panel_setting
lbcmodit_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int	value = (int) xv_get(item, PANEL_VALUE);

	lbcont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcepscrs'.
 */
Panel_setting
lbcont_lbcontpu_lbcepscrs_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcepszer'.
 */
Panel_setting
lbcont_lbcontpu_lbcepszer_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbciprsng'.
 */
Panel_setting
lbcipsrng_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int	value = (int) xv_get(item, PANEL_VALUE);

	lbcont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbcepsext'.
 */
Panel_setting
lbcont_lbcontpu_lbcepsext_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	lbcont_lbcontpu_objects *ip = (lbcont_lbcontpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbcont_rw_notify(item, event);
	return panel_text_notify(item, event);
}

/* callback for FRAME_DONE */
void
    lbcont_done_proc(frame)
Frame
    frame;
{
    lbcont_close();
}	
