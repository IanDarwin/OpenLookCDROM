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
#include <malloc.h>
#include <xview/xview.h>
#include <xview/alert.h>
#include <xview/panel.h>
#include <xview/notify.h>

#include <modellib.h>
#include <constants.h>
#include <memory.h>
#include "ui_init.h"
#include "sys_panels.h"
#include "user_panels.h"
#include "cmd_ui.h"

/* this is global for ownership */
cmd_win_objects *cmd_ip = NULL;


/* 
 *  cmd_open()  displays the cmd window, creating it if necessary.
 *  Last Modified: 15 Feb 1991  fjw
 */

cmd_open(use_default,left,top,width,height)
     int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
     int   left;           /* position (on screen) of left edge of window.  */
     int   top;            /* position (on screen) of top edge of window.   */
     int   width;          /* width of window;  width <= 0 means use default width */
     int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  extern Notify_value sigint_func();
  void cmd_field_manager(), cmd_data_refresh();

  if (!cmd_ip)
    {
      cmd_ip = cmd_win_objects_initialize(NULL, NULL);
      register_win_and_type("Cmd",cmd_ip->win,BASE_WINDOW);
      cmd_field_manager();
    }
  if (use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      /* get the current configuration */
      frame_get_rect(cmd_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      /* set the new configuration */
      frame_set_rect(cmd_ip->win,rect);
      free(rect);
    }

  mark_window_open("Cmd");
/*  xv_set(cmd_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);*/
  xv_set(cmd_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
  notify_set_signal_func( cmd_ip->win , sigint_func, 
			 SIGINT, NOTIFY_ASYNC);
}

int
cmd_close()
{
    mark_window_closed("Cmd");
    if(cmd_ip) {
/*	xv_set(cmd_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);*/
        xv_set(cmd_ip->win, XV_SHOW, FALSE, NULL);	
    }
}



/* routine to install model names into window system */

win_install_models()
{
  Menu	dscatmenu, *modelmenu;
  Menu_item	temp;
  caddr_t		*ip;
  int	i;

  extern Menu_item	models_handler();


  ip = (caddr_t *) xv_get(cmd_ip->pan, XV_KEY_DATA, 
			  INSTANCE, NULL);

  /* assign memory */
  modelmenu = (Menu *) malloc((unsigned) N_DS_Category*sizeof(Menu));


  /* create model menus */
  for (i=0; i<N_DS_Category; i++) {
    /* create submenu */
    modelmenu[i] = cmd_modelmenu_create(ip , NULL);
    /* remove default item and destroy it! */
    temp = (Menu_item) xv_get(modelmenu[i], MENU_NTH_ITEM, 1, NULL);
    xv_set(modelmenu[i], MENU_REMOVE_ITEM, temp, NULL);
    xv_destroy(temp);
    /* install submenu into the ds category menu */
    temp = (Menu_item) xv_create(NULL, MENUITEM,
				 XV_KEY_DATA, INSTANCE, ip,
				 MENU_STRING,	DS_Category[i],
				 MENU_PULLRIGHT,	modelmenu[i],
				 NULL);
    dscatmenu = (Menu) xv_get(cmd_ip->modelbutton, 
			      PANEL_ITEM_MENU, NULL);
    xv_set(dscatmenu, MENU_APPEND_ITEM, temp, NULL);
  }
  
  /* now fill up the model menu's */
  for (i=0; i<N_DS; i++) {
    /* create menu item */
    temp = (Menu_item) xv_create(NULL, MENUITEM,
				 XV_KEY_DATA, INSTANCE, ip,
				 XV_KEY_DATA, MODEL_MENU_ITEM_KEY, i,
				 MENU_STRING, DS_Sel[i].DS_Name,
				 MENU_GEN_PROC, models_handler,
				 NULL);
    /* add to appropriate submenu */
    xv_set(modelmenu[DS_Sel[i].Category], MENU_APPEND_ITEM, temp, NULL);
  }
  
  /* now deallocate memory */
  free( (char *) modelmenu );
}


void
  cmd_field_manager()
{
  cmd_win_objects *ip = cmd_ip;
  int n;

  if(!ip) return;

  n = *((int *) pm(GET, "Model.Load_Number", NULL));
  xv_set(ip->modelname, 
	 PANEL_LABEL_STRING,	DS_Sel[n].DS_Name,
	 PANEL_LABEL_BOLD, 1,
	 NULL);

}

void
  cmd_data_refresh()
{
  void update_stored_points();
  cmd_win_objects *ip = cmd_ip;
  int n;

  if(!ip) return;

  update_stored_points();
}



/* procedure to get the number of points in the 
   trajectory memory object and display it on the 
   cmd window */
void
  update_stored_points()
{
  memory m;
  int n=0;
  char strng[20];

  /* look at all memory objects */
  m = (memory) pm(GET, "Memory.Traj", NULL);
  n += memory_stored_points(m);
  m = (memory) pm(GET, "Memory.Mult", NULL);
  n += memory_stored_points(m);
  m = (memory) pm(GET, "Memory.Sel_Pt", NULL);
  n += memory_stored_points(m);
  m = (memory) pm(GET, "Memory.Param", NULL);
  n += memory_stored_points(m);
  m = (memory) pm(GET, "Memory.Fixed", NULL);
  n += memory_stored_points(m);
  m = (memory) pm(GET, "Memory.Cont", NULL);
  n += memory_stored_points(m);

  sprintf(strng,"%d pts",n);
  xv_set(cmd_ip->stored_points, PANEL_LABEL_STRING, strng, NULL);
}



add_panels(obj, n, pans)
     Xv_opaque obj;
     int n;
     struct Panel_Def *pans;
{
  int i;
  Menu_item mi;

  for (i=0; i<n; i++)
    {
      if ((int) strlen(pans[i].name) > 0)
	{
	  mi = (Menu_item) xv_create(NULL, MENUITEM,
				     MENU_STRING, pans[i].name,
				     MENU_GEN_PROC, pans[i].handler,
				     MENU_RELEASE,
				     NULL);
	  xv_set(obj, MENU_APPEND_ITEM, mi, NULL);
	}
    }
}


/*
 * Perform software signal SIGINT interrupt handeling
 */

Notify_value
sigint_func(client, sig, when)
Notify_client           client;
int                     sig;
Notify_signal_mode      when;
{
  int	interrupt_type;

  reset_interrupt();
  interrupt_type = notice_prompt(
                     cmd_ip->win, NULL,
                     NOTICE_NO_BEEPING, TRUE,
                     NOTICE_MESSAGE_STRINGS,
                     "Action: ", NULL,
                     NOTICE_FOCUS_XY, 20, 100,
                     NOTICE_BUTTON_YES, "Interrupt",
                     NOTICE_BUTTON,     "Quit dstool", 2,
                     NOTICE_BUTTON_NO,  "Cancel",
                     NULL);

  if (interrupt_type == 1)
    set_interrupt();
  else if( interrupt_type == 2 )
      exit( -1 );

  return( NOTIFY_DONE );
}
