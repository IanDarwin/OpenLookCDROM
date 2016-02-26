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
#include <constants.h>
#include "lbmain_cui.h"
#include "lbmain.h"


lbmain_locbif_pu_objects	*Lbmain_locbif_pu;
extern struct Lbmain_Ds         lbmain_ds;


int
lbmain_sel_notify(item, string, client_data, op, event, row)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
	int		row;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		lbmain_data_refresh();
		break;

	case PANEL_LIST_OP_SELECT:
		lbmain_data_refresh();
		break;

	case PANEL_LIST_OP_VALIDATE:
		fprintf(stderr, "lbmain: lbmain_sel_notify: PANEL_LIST_OP_VALIDATE: %s\n",string);
		break;

	case PANEL_LIST_OP_DELETE:
		fprintf(stderr, "lbmain: lbmain_sel_notify: PANEL_LIST_OP_DELETE: %s\n",string);
		break;
	}
	
	return XV_OK;
}

/*
 * Notify callback function for `lbcontpu'.
 */
void
lbmain_cont(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbcont_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	
}

/*
 * Notify callback function for `lbsolverp'.
 */
void
lbmain_solvr_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbsolver_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	
}

/*
 * Notify callback function for `lbdisplayp'.
 */
void
lbmain_dsply_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbdisplay_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	
}

/*
 * Notify callback function for `lbdirect'.
 */
Panel_setting
lbmain_dir_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	lbmain_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbfilerd'.
 */
void
lbrdfile_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lb_rd_file();
	lbmain_data_refresh();
}

/*
 * Notify callback function for `lbfilename'.
 */
Panel_setting
lbfname_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);

	lbmain_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `lbforw'.
 */
void
lbforw_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbmain_ds.Locbif_Dir = LB_FORWARD;
	locbif_mem_go();

}

/*
 * Notify callback function for `lbcontinue'.
 */
void
lbcontinue_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbmain_ds.Locbif_Dir = LB_CONTINUE;
	locbif_mem_go();
}

/*
 * Notify callback function for `lbback'.
 */
void
lbback_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbmain_ds.Locbif_Dir = LB_BACKWARD;
	locbif_mem_go();
}

/*
 * Notify callback function for `lbbifmode'.
 */
void
lbmode_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lboption_build( value );
	lbmain_data_refresh();
}


/*
 * Notify callback function for `lbpause'.
 */
void
lbmain_locbif_pu_lbpause_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
}



/*
 * Notify callback function for `lbstatepu'.
 */
void
lbcurst_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbmain_locbif_pu_objects *ip = (lbmain_locbif_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	lbmain_data_refresh();
	lbstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);
}

/* callback for FRAME_DONE */
void
    lbmain_done_proc(frame)
Frame
    frame;
{
    lbmain_close();
}	
