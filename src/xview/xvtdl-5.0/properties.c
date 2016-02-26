/*
 * $Id: properties.c,v 2.0 1992/09/15 11:44:53 jipping Exp $
 * **********************************************************************
 *
 *  Properties.c ==> Routines that implement the properties window.
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1992 by Mike Jipping and Hope College
 *
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 * $Log: properties.c,v $
 * Revision 2.0  1992/09/15  11:44:53  jipping
 * Release 4.0 beta:
 *    * Added deadline properties.
 *    * added default priority prop
 *    * Changed the look and feel to a "property sheet" type of interface
 *
 * Revision 1.1  1992/07/27  18:43:12  jipping
 * Initial revision
 *
 */

#include "globaldefs.h"

props_props_frame_objects	*props_props_frame;

/*
 * **********************************************************************
 * Initialize the print window...by calling the routine set up by GUIDE.
 */
void initialize_props ()
{
	props_props_frame = props_props_frame_objects_initialize(NULL, tdlist);
	if (!has_color) {
		xv_set(props_props_frame->fore_color, PANEL_INACTIVE, TRUE, 0);
		xv_set(props_props_frame->fore_chooser, PANEL_INACTIVE, TRUE, 0);
		xv_set(props_props_frame->back_color, PANEL_INACTIVE, TRUE, 0);
		xv_set(props_props_frame->back_chooser, PANEL_INACTIVE, TRUE, 0);
	}
}

/*
 * **********************************************************************
 * This routine sets the items that control the property settings --
 * based on the settings of the global property variables.
 */
void set_global_props()
{
	int l1, l2, l3;

	/*
    * Sorting Properties
    */

	l1 = (int)sort_order[0] - 48;
	l2 = (int)sort_order[1] - 48;
	l3 = (int)sort_order[2] - 48;

	xv_set(props_props_frame->sort_level1,
			 PANEL_VALUE, l1,
			 0);
	xv_set(props_props_frame->sort_level2,
			 PANEL_VALUE, l2,
			 PANEL_INACTIVE, l1==3?TRUE:FALSE,
			 0);
	xv_set(props_props_frame->sort_level3,
			 PANEL_VALUE, l3,
			 PANEL_INACTIVE, l2==3?TRUE:FALSE,
			 0);

 	if (EQUAL(priority_listing, "ascending")) {
		xv_set(props_props_frame->priority_direction, PANEL_VALUE, 0, 0);
	} else {
		xv_set(props_props_frame->priority_direction, PANEL_VALUE, 1, 0);
	}
 	if (EQUAL(chron_listing, "ascending")) {
		xv_set(props_props_frame->chron_direction, PANEL_VALUE, 0, 0);
	} else {
		xv_set(props_props_frame->chron_direction, PANEL_VALUE, 1, 0);
	}

   /*
    * Logging properties
    */
	if (logging == 0) {
		log_level = LOG_NEVER;
		xv_set(props_props_frame->log_preference, PANEL_VALUE, LOG_NEVER, 0);
		xv_set(props_props_frame->log_info, PANEL_INACTIVE, TRUE, 0);
		xv_set(props_props_frame->log_filename, PANEL_INACTIVE, TRUE, 0);
	} else {
		if (EQUAL(log_preference, "atquit")) {
			log_level = LOG_AT_QUIT;
			xv_set(props_props_frame->log_preference, PANEL_VALUE, LOG_AT_QUIT, 0);
		   xv_set(props_props_frame->log_info, PANEL_INACTIVE, TRUE, 0);
			xv_set(props_props_frame->log_filename, PANEL_INACTIVE, FALSE, 0);
		} else {
			log_level = LOG_AT_CHECKED;
			xv_set(props_props_frame->log_preference, PANEL_VALUE, LOG_AT_CHECKED, 0);
		   xv_set(props_props_frame->log_info,
					 PANEL_INACTIVE, FALSE,
					 PANEL_VALUE,    log_info_level,
					 0);
			xv_set(props_props_frame->log_filename, PANEL_INACTIVE, FALSE, 0);
		}
	}
	xv_set(props_props_frame->log_filename, PANEL_VALUE, log_file, 0);

   /*
    *  Printing defaults
    */
	xv_set(props_props_frame->def_print_dest, 
			 PANEL_VALUE, EQUAL(default_print_dest, "printer")?0:1,
			 0);
	xv_set(props_props_frame->def_printer, PANEL_VALUE, default_printer, 0);
	xv_set(props_props_frame->def_filename, PANEL_VALUE, print_file, 0);
	xv_set(props_props_frame->def_print_mode, PANEL_VALUE, postscriptmode, 0);

	/*
	 *  Miscellaneous defaults
    */
	xv_set(props_props_frame->def_priority, PANEL_VALUE, default_priority, 0);
	if (EQUAL(on_propagation, "delete")) {
		xv_set(props_props_frame->propagation_behavior, PANEL_VALUE, 0, 0);
	} else {
		xv_set(props_props_frame->propagation_behavior, PANEL_VALUE, 1, 0);
	} 	
	xv_set(props_props_frame->fore_color, PANEL_VALUE, fgcolor, 0);
	xv_set(props_props_frame->back_color, PANEL_VALUE, bgcolor, 0);

	/*
    *  Deadline properties
    */
	xv_set(props_props_frame->deadline_actions,
			 PANEL_VALUE, default_deadline.actions,
			 0);
	xv_set(props_props_frame->deadline_delete_time,
			 PANEL_VALUE, default_deadline.delete_time,
			 0);
	xv_set(props_props_frame->deadline_delete_units,
			 PANEL_VALUE, default_deadline.delete_units,
			 0);
	xv_set(props_props_frame->deadline_up_increment,
			 PANEL_VALUE, default_deadline.priority_up_units,
			 0);
	xv_set(props_props_frame->deadline_down_increment,
			 PANEL_VALUE, default_deadline.priority_down_units,
			 0);
	xv_set(props_props_frame->deadline_on_mail_to,
			 PANEL_VALUE, default_deadline.mail_on,
			 0);
	xv_set(props_props_frame->deadline_after_mail_to,
			 PANEL_VALUE, default_deadline.mail_after,
			 0);
	xv_set(props_props_frame->deadline_move_after,
			 PANEL_VALUE, default_deadline.move_time,
			 0);
	xv_set(props_props_frame->deadline_move_units,
			 PANEL_VALUE, default_deadline.move_units,
			 0);
}

/*
 * **********************************************************************
 * Callback routine for the property settings menu item.  Opens the
 * properties window.
 */

Menu_item open_properties(item, op)
Menu_item	item;
Menu_generate	op;
{
	int height;
	
	set_global_props();

	xv_set(props_props_frame->props_category, PANEL_VALUE, 0, 0);
	xv_set(props_props_frame->sorting_panel, XV_SHOW, TRUE, 0);
	xv_set(props_props_frame->logging_panel, XV_SHOW, FALSE, 0);
	xv_set(props_props_frame->printing_panel, XV_SHOW, FALSE, 0);
	xv_set(props_props_frame->deadline_panel, XV_SHOW, FALSE, 0);
	xv_set(props_props_frame->other_panel, XV_SHOW, FALSE, 0);
	height = xv_get(props_props_frame->sorting_panel, XV_HEIGHT) +
		      xv_get(props_props_frame->props_control_panel, XV_HEIGHT);

	xv_set(props_props_frame->props_frame,
			 XV_HEIGHT, height,
			 
			 XV_SHOW, TRUE,
			 0);
	return(item);
}

/*
 * **********************************************************************
 * Callback routine for property category selector.  Activates the
 * correct property panel and sets the height of the window.
 */
void change_prop_category(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	props_props_frame_objects *ip =
		(props_props_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int height;

	switch (value) {
   	case 0:
			xv_set(props_props_frame->sorting_panel, XV_SHOW, TRUE, 0);
			xv_set(props_props_frame->logging_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->printing_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->deadline_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->other_panel, XV_SHOW, FALSE, 0);
			height = xv_get(props_props_frame->sorting_panel, XV_HEIGHT) +
		            xv_get(props_props_frame->props_control_panel, XV_HEIGHT);
			xv_set(ip->props_frame, XV_HEIGHT, height, 0);
			break;

	   case 1:
			xv_set(props_props_frame->sorting_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->logging_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->printing_panel, XV_SHOW, TRUE, 0);
			xv_set(props_props_frame->deadline_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->other_panel, XV_SHOW, FALSE, 0);
			height = xv_get(props_props_frame->printing_panel, XV_HEIGHT) +
		            xv_get(props_props_frame->props_control_panel, XV_HEIGHT);
			xv_set(ip->props_frame, XV_HEIGHT, height, 0);
			break;

	   case 2:
			xv_set(props_props_frame->sorting_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->logging_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->printing_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->deadline_panel, XV_SHOW, TRUE, 0);
			xv_set(props_props_frame->other_panel, XV_SHOW, FALSE, 0);
			height = xv_get(props_props_frame->deadline_panel, XV_HEIGHT) +
		            xv_get(props_props_frame->props_control_panel, XV_HEIGHT);
			xv_set(ip->props_frame, XV_HEIGHT, height, 0);
			break;

		case 3:
			xv_set(props_props_frame->sorting_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->logging_panel, XV_SHOW, TRUE, 0);
			xv_set(props_props_frame->printing_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->deadline_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->other_panel, XV_SHOW, FALSE, 0);
			height = xv_get(props_props_frame->logging_panel, XV_HEIGHT) +
		            xv_get(props_props_frame->props_control_panel, XV_HEIGHT);
			xv_set(ip->props_frame, XV_HEIGHT, height, 0);
			break;

	   case 4:
			xv_set(props_props_frame->sorting_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->logging_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->printing_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->deadline_panel, XV_SHOW, FALSE, 0);
			xv_set(props_props_frame->other_panel, XV_SHOW, TRUE, 0);
			height = xv_get(props_props_frame->other_panel, XV_HEIGHT) +
		            xv_get(props_props_frame->props_control_panel, XV_HEIGHT);
			xv_set(ip->props_frame, XV_HEIGHT, height, 0);
			break;
	}
}

/*
 * **********************************************************************
 * Callback routine for the "Log Preference:" item.  Activates or
 * deactivates items on the properties window.
 */
void set_log_preference(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	props_props_frame_objects *ip =
		(props_props_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	switch (value) {
	   case LOG_AT_CHECKED:
		   xv_set(ip->log_info, PANEL_INACTIVE, FALSE, 0);
			xv_set(ip->log_filename, PANEL_INACTIVE, FALSE, 0);
			break;
			
	   case LOG_AT_QUIT:
		   xv_set(ip->log_info, PANEL_INACTIVE, TRUE, 0);
			xv_set(ip->log_filename, PANEL_INACTIVE, FALSE, 0);
			break;
			
	   case LOG_NEVER:
		   xv_set(ip->log_info, PANEL_INACTIVE, TRUE, 0);
			xv_set(ip->log_filename, PANEL_INACTIVE, TRUE, 0);
			break;
	}

}

/*
 * **********************************************************************
 * Callback for the sorting level 1 item.  Plays around with the values
 * and "inactivity" of the items below it.
 */
void change_level1(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	props_props_frame_objects *ip =
		(props_props_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (value) {
	   case 0:
	   case 1:
		   xv_set(ip->sort_level2, PANEL_INACTIVE, FALSE, 0);
			break;

		case 2:
		case 3:
		   xv_set(ip->sort_level2, PANEL_VALUE, 3, 0);
		   xv_set(ip->sort_level2, PANEL_INACTIVE, TRUE, 0);
		   xv_set(ip->sort_level3, PANEL_VALUE, 3, 0);
		   xv_set(ip->sort_level3, PANEL_INACTIVE, TRUE, 0);
	}
	
}

/*
 * **********************************************************************
 * Callback for the sorting level 2 item.  Plays around with the value
 * and "inactivity" of the item below it.
 */
void change_level2(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	props_props_frame_objects *ip =
		(props_props_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (value) {
	   case 0:
	   case 1:
		   xv_set(ip->sort_level3, PANEL_INACTIVE, FALSE, 0);
			break;

		case 2:
		case 3:
		   xv_set(ip->sort_level3, PANEL_VALUE, 3, 0);
		   xv_set(ip->sort_level3, PANEL_INACTIVE, TRUE, 0);
	}
}

/*
 * **********************************************************************
 * This routine sets the values of the global variables that control
 * the properties to the xvtdl program.  It does so by examining the
 * values of the window items.
 */
void apply_props(ip)
props_props_frame_objects *ip;
{
	int mode;

   /*
	 * Sorting Props
    */

	sort_order[0] = (char)(xv_get(props_props_frame->sort_level1, PANEL_VALUE)+48);
	sort_order[1] = (char)(xv_get(props_props_frame->sort_level2, PANEL_VALUE)+48);
	sort_order[2] = (char)(xv_get(props_props_frame->sort_level3, PANEL_VALUE)+48);
	if (xv_get(ip->priority_direction, PANEL_VALUE) == 0) {
		strcpy(priority_listing, "ascending");
	} else {
		strcpy(priority_listing, "descending");
	}
	if (xv_get(ip->chron_direction, PANEL_VALUE) == 0) {
		strcpy(chron_listing, "ascending");
	} else {
		strcpy(chron_listing, "descending");
	}

	/*
    *  Log stuff
    */
	log_level = xv_get(ip->log_preference, PANEL_VALUE);
	logging = ! (log_level == LOG_NEVER);
	if (logging) {
		if (log_level == 0) {
			strcpy(log_preference, "atchecked");
		} else {
			strcpy(log_preference, "atquit");
		}
	}
	log_info_level = xv_get(ip->log_info, PANEL_VALUE);
	if (log_info_level == LOG_TIMESTAMP) {
		strcpy(log_info_pref, "timestamp");
	} else {
		strcpy(log_info_pref, "userspec");
	}
	strcpy(log_file, (char *)xv_get(ip->log_filename, PANEL_VALUE));

   /*
    *  Priority stuff 
    */
	default_priority = xv_get(ip->def_priority, PANEL_VALUE);

   /*
    *  Propagation behavior
    */
	if (xv_get(ip->propagation_behavior, PANEL_VALUE) == 0) {
		strcpy(on_propagation, "delete");
	} else {
		strcpy(on_propagation, "retain");
	} 

   /*
    *  Colors
    */
	strcpy(fgcolor, (char *)xv_get(ip->fore_color, PANEL_VALUE));
	strcpy(bgcolor, (char *)xv_get(ip->back_color, PANEL_VALUE));

	/*
    *  Printer properties
    */
	mode = xv_get(ip->def_print_dest, PANEL_VALUE);
	if (mode) {
		strcpy(default_print_dest, "file");
		xv_set(print_print_base->print_or_file, PANEL_VALUE, 1, 0);
	} else {
		strcpy(default_print_dest, "printer");
		xv_set(print_print_base->print_or_file, PANEL_VALUE, 0, 0);
	}
	strcpy(print_file, (char *)xv_get(ip->def_filename, PANEL_VALUE));
	xv_set(print_print_base->filename, PANEL_VALUE, print_file, 0);
	strcpy(default_printer, (char *)xv_get(ip->def_printer, PANEL_VALUE));
	xv_set(print_print_base->printer, PANEL_VALUE, default_printer, 0);
	postscriptmode = xv_get(ip->def_print_mode, PANEL_VALUE);
	xv_set(print_print_base->postscript, PANEL_VALUE, 1, 0);
	if (postscriptmode) {
		xv_set(print_print_base->scale, PANEL_INACTIVE, FALSE, 0);
	}

   /*
    *  Default deadline settings
    */
	default_deadline.actions =
		xv_get(ip->deadline_actions, PANEL_VALUE);
	default_deadline.delete_time =
		xv_get(ip->deadline_delete_time, PANEL_VALUE);
	default_deadline.delete_units =
		xv_get(ip->deadline_delete_units, PANEL_VALUE);
	default_deadline.priority_up_units =
		xv_get(ip->deadline_up_increment, PANEL_VALUE);
	default_deadline.priority_down_units =
		xv_get(ip->deadline_down_increment, PANEL_VALUE);
	strcpy(default_deadline.mail_on,
			 (char *)xv_get(ip->deadline_on_mail_to, PANEL_VALUE));
	strcpy(default_deadline.mail_after,
			 (char *)xv_get(ip->deadline_after_mail_to, PANEL_VALUE));
	default_deadline.move_time =
		xv_get(ip->deadline_move_after, PANEL_VALUE);
	default_deadline.move_units =
		xv_get(ip->deadline_move_units, PANEL_VALUE);
	
	set_xdefaults();
}

/*
 * **********************************************************************
 * Callback routine for the "Reset" button on the properties window.
 */
void reset_props(item, event)
Panel_item item; 
Event      *event;
{
	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
	load_xdefaults();
	set_global_props();
}

/*
 * **********************************************************************
 * Callback routine for the Done button on the properties window.
 */
void close_props(item, event)
Panel_item item; 
Event      *event;
{
	props_props_frame_objects *ip =
		(props_props_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	apply_props(ip);
	save_xdefaults();
	display_list(curr_month, curr_day, curr_year);

	xv_set(ip->props_frame,
			 FRAME_CMD_PUSHPIN_IN, FALSE, 
			 XV_SHOW, FALSE,
			 0);
}
