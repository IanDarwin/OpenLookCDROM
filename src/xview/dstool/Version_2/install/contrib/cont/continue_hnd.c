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
#include <gdd.h>
#include <constants.h>
#include <pm.h>
#include "continue_cui.h"
#include "continue.h"

extern struct  Cont_Cntl_Ds             cont_ds;
continue_cont_pu_objects        	*continue_cont_pu;


/*
 * Done callback function for `cont_pu'.
 */
void
cont_done(frame)
	Frame		frame;
{
	extern int 
	  cont_close();

        cont_close();
	xv_set(frame, XV_SHOW, FALSE, 0);
}

/*
 * Notify callback function for `mode'.
 */
void
cont_mode_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	pm( PUT, "Cont.Mode", value, NULL);
	cont_data_refresh();
}

/*
 * Notify callback function for `forward'.
 */
void
forwd_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	pm(PUT, "Cont.Direction", FORWARD, NULL);
	cont_proc();		/* makes call to data refresh */
}

/*
 * Notify callback function for `continue'.
 */
void
contnu_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	pm(PUT, "Cont.Direction", CONTINUE, NULL);
	cont_proc();
}

/*
 * Notify callback function for `back'.
 */
void
backwd_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	pm(PUT, "Cont.Direction", BACKWARD, NULL);
	cont_proc();
}


/*
 * Notify callback function for `check'.
 */
void
check_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	cont_ds.Check_Switch = (value==0) ? FALSE: TRUE;
	cont_data_refresh();
}

/*
 * Notify callback function for `clear'.
 */
void
clr_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	fputs("continue: clr_notify\n", stderr);
}


/*
 * Notify callback function for `statepu'.
 */
void
statepu_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	cntstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);
}

/*
 * Notify callback function for `iters'.
 */
Panel_setting
iters_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `settings'.
 */
void
settings_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	if(value==0)
	   {cont_ds.Cont_Hide_Settings = TRUE;
	    xv_set(continue_cont_pu->cont_pu, XV_HEIGHT, 295, NULL);}
	else
	   {cont_ds.Cont_Hide_Settings = FALSE;
	    xv_set(continue_cont_pu->cont_pu, XV_HEIGHT, 507, NULL);}
}

/*
 * Notify callback function for `parafix'.
 */
void
parafix_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	pm( PUT, "Cont.Vary_Switch", value, NULL);
	cont_data_refresh();
}

/*
 * Notify callback function for `jacupdate'.
 */
void
jacupdate_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	cont_data_refresh();
}

/*
 * Notify callback function for `abserr'.
 */
Panel_setting
abserr_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `relerr'.
 */
Panel_setting
relerr_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `minstp'.
 */
Panel_setting
minstp_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `maxstp'.
 */
Panel_setting
maxstp_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `htan'.
 */
Panel_setting
htan_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `debuglvl'.
 */
Panel_setting
debuglvl_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	continue_cont_pu_objects	*ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	cont_data_refresh();
	return panel_text_notify(item, event);
}


/*
 * Notify callback function for `contpara'.
*/
int
contpara_notify(item, value, event)
Panel_item      item;
int             value;
Event           *event;
{
	continue_cont_pu_objects        *ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	cont_data_refresh();

}

/*
 * Notify callback function for `augparam'.
*/
int
augparam_notify(item, value, event)
Panel_item      item;
int             value;
Event           *event;
{
	continue_cont_pu_objects        *ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	cont_data_refresh();

}


/*
 * Notify callback function for `search'.  
 */
void
search_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	pm(PUT, "Cont.Search", TRUE, NULL);
	cont_proc();
	pm(PUT, "Cont.Search", FALSE, NULL);	
}



/*
 * Notify callback function for `cview'.
 */
void
cview_notify(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
	continue_cont_pu_objects *ip = (continue_cont_pu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	/* short	i;
	for (i = 0; i < 3; i++) {
	    if (value & 01) fprintf(stderr, "\t%dth item selected\n", i);
	    value >>= 1; } */

	/* ith bit on ==> want to plot to windows with i params;
	   eg, 0101 ==> plot to windows with 0 or 2 params */
	pm(PUT, "Cont.Plot_Type", (int) value, NULL); 
	cont_data_refresh();
}

