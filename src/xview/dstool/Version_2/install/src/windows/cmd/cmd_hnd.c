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
#include <xview/cms.h>

#include <ui_init.h>
#include <modellib.h>
#include <constants.h>
/* #include <twoD.h>
#include <twoD_opt.h> */
#include <pm.h>

/*
 * Menu handler for `viewmenu (3-D image)'.
 */
Menu_item
threeD_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	/* if (op == MENU_NOTIFY) threeD_open(-1,DEFAULT_WIN_CONFIG,0,0,0,0);  */
	return item;
}


/*
 * Notify function for each item in a pinnable menu.
 */
void
pinned_menu_notify(menu, item)
	Menu		menu;
	Menu_item	item;
{
	Xv_opaque	pin_window = (Xv_opaque) xv_get(menu, MENU_PIN_WINDOW);
	void	(*menu_notify)() = (void (*)()) xv_get(menu, MENU_GEN_PROC);
	void	(*item_notify)() = (void (*)()) xv_get(item, MENU_GEN_PROC);
	
	if (pin_window && xv_get(pin_window, XV_SHOW)) {
		if (menu_notify)
			(*menu_notify)(menu, MENU_NOTIFY);
		if (item_notify)
			(*item_notify)(item, MENU_NOTIFY);
		if (item_notify)
			(*item_notify)(item, MENU_NOTIFY_DONE);
		if (menu_notify)
			(*menu_notify)(menu, MENU_NOTIFY_DONE);
	}
}




/*
 * Menu handler for `modelmenu (Item)'.
 */
Menu_item
models_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  int	i;
  cmd_win_objects *ip = (cmd_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  extern	void	load_model();
  
  if (op == MENU_NOTIFY ) 
    {
      i = (int) xv_get(item, XV_KEY_DATA, MODEL_MENU_ITEM_KEY, NULL);
      xv_set(ip->modelname,
	     PANEL_LABEL_STRING, DS_Sel[i].DS_Name,
	     PANEL_LABEL_BOLD, 1,  
	     NULL);
      pm(PUT, "Model.Load_Number", i,
	 EXEC, "Model.Load", NULL);
    }
  return item;
}

/*
 * Used by batch (e.g. load/tcl) command to open cmd window.
 */
Menu_item
    cmd_batch_handler(item, op)
Menu_item	
    item;
Menu_generate	
    op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    cmd_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Cmd",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		cmd_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		cmd_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}
