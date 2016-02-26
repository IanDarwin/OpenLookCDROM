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
/* oned.c 
 * 
 * standard routines for oned window
 *
 */
#include <stdio.h>
#include <xview/xview.h>

#include <pm.h>
#include <ui_init.h>
#include <constants.h>
#include "oned_ui.h"

static oned_win_objects *oned_ip = NULL;

/* 
 * oned_open()
 *
 * displays the oned window, creating it if necessary.
 */
oned_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void oned_field_manager(), oned_install();

  if (oned_ip == NULL)
    {
      /* create the window, make the owner the dstool cmd panel */
      oned_ip = oned_win_objects_initialize(NULL, cmd_ip->win);
      oned_install();
      register_win_and_type("One",oned_ip->win,POPUP_WINDOW);
      oned_field_manager();
    }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(oned_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(oned_ip->win,rect);
      free(rect);
    }

  /* show the window */
  mark_window_open("One");
  xv_set(oned_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(oned_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}


int
oned_close()
{
    mark_window_closed("One");
    if(oned_ip) {
	xv_set(oned_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(oned_ip->win, XV_SHOW, FALSE, NULL);	
    }
}

/*
 * oned_field_manager()
 *
 * manager for the custom panel items
 */
void
  oned_field_manager()
{
  extern void oned_reset();
  int n_var, flag;

  
  /* exit now if the window has not been created */
  if (oned_ip == NULL) return;
  
  oned_reset();

  /* find out the number of variables */
  n_var = *((int *) pm(GET, "Model.Varb_Dim", NULL))-1;

  /* is it a mapping? */
  flag = *((int *) pm(GET, "Model.Mapping_Flag", NULL));

  if (flag && (n_var ==1))  /* it is a one-dim mapping! */
    {
      xv_set(oned_ip->points, PANEL_INACTIVE, FALSE, NULL);
      xv_set(oned_ip->iter, PANEL_INACTIVE, FALSE, NULL);
      xv_set(oned_ip->sketch, PANEL_INACTIVE, FALSE, NULL);
      xv_set(oned_ip->clear, PANEL_INACTIVE, FALSE, NULL);
      xv_set(oned_ip->diagonal, PANEL_INACTIVE, FALSE, NULL);
      xv_set(oned_ip->forward, PANEL_INACTIVE, FALSE, NULL);
      xv_set(oned_ip->backward, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->contin, PANEL_INACTIVE, FALSE, NULL);
    }
  else
    {
      xv_set(oned_ip->points, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->iter, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->sketch, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->clear, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->diagonal, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->forward, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->backward, PANEL_INACTIVE, TRUE, NULL);
      xv_set(oned_ip->contin, PANEL_INACTIVE, TRUE, NULL);
    }

  /* write data into the fields */
  oned_data_refresh();

}


/*
 * oned_read_window()
 *
 * routine to read data from all items in the oned window
 */
int
  oned_read_window()
{

  /* if no window, then nothing to read */
  if (oned_ip == NULL) return;

  /* read text fields */
  pm(PUT, "OneD.Points", xv_get(oned_ip->points, PANEL_VALUE), NULL);
  pm(PUT, "OneD.Iter", xv_get(oned_ip->iter, PANEL_VALUE), NULL);
}

/*
 * oned_data_refresh()
 *
 * routine to refresh the data in all items in the oned window
 */
int
  oned_data_refresh()
{

  /* if no window, then nothing to refresh */
  if (oned_ip == NULL) return;

  /* refresh text fields */
  xv_set(oned_ip->points, PANEL_VALUE, *((int *) pm(GET, "OneD.Points", NULL)),
	 NULL);
  xv_set(oned_ip->iter, PANEL_VALUE, *((int *) pm(GET, "OneD.Iter", NULL)),
	 NULL);

}


/*
 * oned_handler()
 *
 * Menu handler for `panelmenu (One-D Maps...)'.
 */
Menu_item
  oned_handler(item, op)
Menu_item item;
Menu_generate op;
{
  int locn[4];

  if (op == MENU_NOTIFY)
    {
      if (item != (Menu_item) NULL)
	oned_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else 
	{
	  pm(GET_LIST, "Win.Locn.One", 0, 3, locn, NULL);
	  if ((locn[0] != NO_LOCATION))
	    oned_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	  else
	    oned_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
    }

  return item;
}
