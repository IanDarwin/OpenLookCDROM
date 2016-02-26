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
/* porbit.c 
 * 
 * standard routines for porbit window
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <xview/xview.h>

#include <pm.h>
#include <ui_init.h>
#include <constants.h>
#include "porbit_ui.h"

static porbit_win_objects *porbit_ip = NULL;

/* 
 * porbit_open()
 *
 * displays the porbit window, creating it if necessary.
 */
porbit_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;
  if (porbit_ip == NULL)
    {
      /* create the window, make the owner the dstool cmd panel */
      porbit_ip = porbit_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Periodic",porbit_ip->win,POPUP_WINDOW);
      porbit_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(porbit_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(porbit_ip->win,rect);
      free(rect);
    }

  /* show the window */
  mark_window_open("Periodic");
  xv_set(porbit_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(porbit_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

int
porbit_close()
{
    mark_window_closed("Periodic");
    if(porbit_ip) {
	xv_set(porbit_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(porbit_ip->win, XV_SHOW, FALSE, NULL);
    }
}

/*
 * porbit_field_manager()
 *
 * manager for the custom panel items
 */
porbit_field_manager()
{
  int i,n_var,n_param,status=0;
  char name[MAX_LEN_VARB_NAME];
  
  /* initialize data structure for the porbit window for the new dynamical system */
  status = porbit_init();

  /* exit now if the window has not been created */
  if (porbit_ip == NULL || status != 0) return(status);
  
  /* destroy the custom items */
  if (porbit_ip->var) xv_destroy(porbit_ip->var);
  if (porbit_ip->param) xv_destroy(porbit_ip->param);
  
  /* find out the number of variables */
  n_var = *((int *) pm(GET, "Model.Varb_Dim", NULL))-1;
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

  /* set up the settings item choices */
  porbit_ip->var = porbit_win_var_create(porbit_ip, porbit_ip->pan, n_var);
  for (i=0; i<n_var; i++)
    {
      pm(GET, "Model.Varb_Names", i, name, NULL);
      xv_set(porbit_ip->var, PANEL_CHOICE_STRING, i, name, NULL);
    } 
  porbit_ip->param = porbit_win_param_create(porbit_ip, porbit_ip->pan, n_param);
  for (i=0; i<n_param; i++)
    {
      pm(GET, "Model.Param_Names", i, name, NULL);
      xv_set(porbit_ip->param, PANEL_CHOICE_STRING, i, name, NULL);
    } 

  /* make window tall enough  */
  window_fit(porbit_ip->pan);
  window_fit(porbit_ip->win);

  /* write data into the fields */
  porbit_data_refresh();

  return(status);
}


double porbit_value;
double porbit_stepsize;
int porbit_numsteps, porbit_period, porbit_algorithm;
int porbit_param, porbit_var;

int
  porbit_init()
{
  porbit_period = 1;
  porbit_value = 0.0;
  porbit_stepsize = 0.01;
  porbit_numsteps = 10;
  porbit_var = 0;
  porbit_param = 0;
  porbit_algorithm = 0;

  return(0);
}

/*
 * porbit_read_window()
 *
 * routine to read data from all items in the porbit window
 */
int
  porbit_read_window()
{
  char *strng;
/*  double atof(); */

  /* if no window, then nothing to read */
  if (porbit_ip == NULL) return;

  /* read text fields */
  strng = (char *) xv_get(porbit_ip->value, PANEL_VALUE);
  porbit_value = atof(strng);
  porbit_period = xv_get(porbit_ip->period, PANEL_VALUE);
  strng = (char *) xv_get(porbit_ip->stepsize, PANEL_VALUE);
  porbit_stepsize = atof(strng);
  strng = (char *) xv_get(porbit_ip->numsteps, PANEL_VALUE);
  porbit_numsteps = atoi(strng);

  /* read setting items */
  porbit_var = xv_get(porbit_ip->var, PANEL_VALUE);
  porbit_param = xv_get(porbit_ip->param, PANEL_VALUE);
  porbit_algorithm = xv_get(porbit_ip->algorithm, PANEL_VALUE);
}

/*
 * porbit_data_refresh()
 *
 * routine to refresh the data in all items in the porbit window
 */
int
  porbit_data_refresh()
{
  int format;
  char strng[20];

  /* if no window, then nothing to refresh */
  if (porbit_ip == NULL) return;

  /* read information on precision of doubles */
  format = *((int *) pm(GET, "Defaults.Precision", NULL));

  /* refresh text fields */
  sprintf(strng, "%.*lg",format,porbit_value);
  xv_set(porbit_ip->value, PANEL_VALUE, strng, NULL);
  xv_set(porbit_ip->period, PANEL_VALUE, porbit_period, NULL);
  sprintf(strng, "%.*lg",format,porbit_stepsize);
  xv_set(porbit_ip->stepsize, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%ld",porbit_numsteps);
  xv_set(porbit_ip->numsteps, PANEL_VALUE, strng, NULL);

  /* refresh setting items */
  xv_set(porbit_ip->var, PANEL_VALUE, porbit_var, NULL);
  xv_set(porbit_ip->param, PANEL_VALUE, porbit_param, NULL);
  xv_set(porbit_ip->algorithm, PANEL_VALUE, porbit_algorithm, NULL);
}

/*
 * porbit_handler()
 *
 * Menu handler for `panelmenu (Pdouble...)'.
 */
Menu_item
  porbit_handler(item, op)
Menu_item item;
Menu_generate op;
{

    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    porbit_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Periodic",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		porbit_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		porbit_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
/*  if (op == MENU_NOTIFY) {
      porbit_open();
    } */
  return item;
}
