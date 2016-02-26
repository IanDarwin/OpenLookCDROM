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
#include "lbsolver_cui.h"


lbsolver_lbsolverpu_objects	*Lbsolver_lbsolverpu;


/*
 * Notify callback function for `lbsitmap'.
 */
Panel_setting
lbs_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lbsolver_lbsolverpu_objects *ip = (lbsolver_lbsolverpu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);

	lbsolver_data_refresh();
	return panel_text_notify(item, event);
}


/* callback for FRAME_DONE */
void
    lbsolver_done_proc(frame)
Frame
    frame;
{
    lbsolver_close();
}	
