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
/*
 * print_handlers.c - Notify and event callback function stubs.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>
#include <pm.h>
#include <constants.h>
#include "print_ui.h"

/*
 * Notify callback function for `print_button'.  
 */
void
print_butt_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  print_win_objects	*ip = (print_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

  if(!valid_twoD_id(*((int *)pm(GET, "Print.Owner", NULL)) ))
    error_notice(ip->pan, "Invalid view window.  Select `Print...' from valid option menu");
  
  print_read_window();
  print_data_refresh();
  print_update_range();   /* Note: uses twoD struct */
  switch( print_go(FALSE) )
    {
    case 1:
      if (!error_notice_option(ip->pan, "Specified files exists.", "Cancel Save", "Overwrite File") )
	print_go(TRUE);
      break;
    case -1:
      error_notice(ip->pan, "Printing not successful.  Cannot open file or pipe.");
      break;
    case -2:
      error_notice(ip->pan, "Broken pipe error. Check printer name and try again?");
      break;
    }
}

/*
 * Notify callback function for `destination'.
 */
void
dest_handler(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  print_win_objects	*ip = (print_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  char			name[SIZE_OF_DIR_PLUS_FNAME];

  if (value)
    {
      pm(PUT, "Print.Printer_Name", (char *)xv_get(ip->dir,PANEL_VALUE), NULL);
      xv_set(ip->dir, PANEL_LABEL_STRING, "Directory:",
	     PANEL_VALUE, (char *)pm(GET, "Print.Directory", name, NULL),
	     NULL);
    }
  else
    {
      pm(PUT, "Print.Directory", (char *)xv_get(ip->dir,PANEL_VALUE), NULL);
      xv_set(ip->dir, PANEL_LABEL_STRING, "Printer:",
	     PANEL_VALUE, (char *)pm(GET, "Print.Printer_Name", name, NULL),
	     NULL);
    }
  xv_set(ip->name, PANEL_INACTIVE, (value? FALSE: TRUE), NULL);
}


/*
 * Notify callback function for text field type panel item.
 */
Panel_setting
print_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
        print_read_window();
	complete_filename("Print");
	print_data_refresh();
	if (event_action(event) == '\t' )
	  return( PANEL_NONE );	/* don't advance cursor if TAB */
	else
	  return panel_text_notify(item, event);
}

/*
 * Notify callback function for setting type panel item.
 */
void
print_setting_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
        print_read_window();
	print_data_refresh();
}


/*
 * Notify callback function for `reset'.  
 */
void
print_reset_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  print_reset();
  print_data_refresh();
}
