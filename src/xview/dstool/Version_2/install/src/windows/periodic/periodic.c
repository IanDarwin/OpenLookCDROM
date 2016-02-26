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
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>

#include <constants.h>
#include <defaults.h>
#include <ui_init.h>
#include "periodic_ui.h"
#include <pm.h>
#include <memory.h>

static periodic_win_objects *periodic_ip = NULL;


/*
 * Menu handler for `panelmenu (Periodic...)'.
 */
Menu_item
periodic_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    periodic_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Fixed",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		periodic_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		periodic_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}


/* 
 *  periodic_open()  displays the periodic window, creating it if necessary.
 *  Last Modified: 15 Feb 1991  fjw
 */

periodic_open(use_default,left,top,width,height)
     int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
     int   left;           /* position (on screen) of left edge of window.  */
     int   top;            /* position (on screen) of top edge of window.   */
     int   width;          /* width of window;  width <= 0 means use default width */
     int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void periodic_field_manager(), periodic_data_refresh();

  if (!periodic_ip)
    {
      periodic_ip = periodic_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Fixed",periodic_ip->win,POPUP_WINDOW);
      periodic_field_manager();
    }
  if (use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect)); 
      /* get the current configuration */
      frame_get_rect(periodic_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      /* set the new configuration */
      frame_set_rect(periodic_ip->win,rect);
      free(rect);
    }

  /* turn off unimplemented features */
  xv_set(periodic_ip->twoDman, PANEL_INACTIVE, TRUE, NULL);
  
  mark_window_open("Fixed");
  xv_set(periodic_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(periodic_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
  periodic_data_refresh();
  
}





/* ------------------------------------------------
   procedure to reset fields to defaults settings
   on periodic window
   ------------------------------------------------- */
void
  periodic_field_manager()
{

  void periodic_data_refresh();
  periodic_data_refresh();
}





/* ------------------------------------------------
   procedure to update info on periodic window

   ------------------------------------------------ */
void
    periodic_data_refresh()
{
  int 		flag;
  char		strng[20];
  int		format = *((int *) pm( GET, "Defaults.Precision", NULL) );
  periodic_win_objects *ip = periodic_ip;

  if (!ip) return;

  flag = *( (int *) pm( GET, "Model.Mapping_Flag", NULL));
  if (flag)
    sprintf(strng, "%ld",*((int *) pm(GET, "Fixed.Map_Period", NULL)));
  else
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Fixed.Vf_Period", NULL)));
  
  xv_set(ip->period, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%d",*((int *) pm(GET, "Fixed.Found", NULL)));
  xv_set(ip->found, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%d",*((int *) pm(GET, "Fixed.Mc_Guesses", NULL)));
  xv_set(ip->mc, PANEL_VALUE, strng, NULL);
  xv_set(ip->algorithm, PANEL_VALUE,*((int *) pm(GET,  "Fixed.Algorithm", NULL)),NULL);
  xv_set(ip->guess, PANEL_VALUE, *((int *) pm(GET, "Fixed.Guess", NULL)),NULL);
  sprintf(strng, "%d", *((int *) pm(GET, "Fixed.Num_Iters", NULL)));
  xv_set(ip->fxpt_iters, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Fixed.Dups", NULL)));
  xv_set(ip->duplicate, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Fixed.Var_Conv", NULL)));
  xv_set(ip->variable, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Fixed.Funct_Conv", NULL)));
  xv_set(ip->function, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Fixed.FD_Step", NULL)));
  xv_set(ip->fd_step, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Fixed.Eigen_Dist", NULL)));
  xv_set(ip->distance, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%d", *((int *) pm(GET, "Fixed.Stab_Points", NULL)));
  xv_set(ip->stabpts, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%d", *((int *) pm(GET, "Fixed.Stab_Steps", NULL)));
  xv_set(ip->stabsteps, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%d", *((int *) pm(GET, "Fixed.Unstab_Points", NULL)));
  xv_set(ip->unstabpts, PANEL_VALUE, strng, NULL);
  sprintf(strng, "%d", *((int *) pm(GET, "Fixed.Unstab_Steps", NULL)));
  xv_set(ip->unstabsteps, PANEL_VALUE, strng, NULL);

  if (!(*(int *)pm(GET, "Fixed.Setting", NULL))) 
    {
      xv_set(ip->win, XV_HEIGHT, 190, NULL);
      xv_set(ip->fxpt_iters, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->duplicate, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->variable, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->function, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->fd_step, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->distance, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->stabpts, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->stabsteps, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->unstabpts, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->unstabsteps, PANEL_INACTIVE, TRUE, NULL);
    }
  else
    {
      xv_set(ip->fxpt_iters, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->duplicate, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->variable, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->function, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->fd_step, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->distance, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->stabpts, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->stabsteps, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->unstabpts, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->unstabsteps, PANEL_INACTIVE, FALSE, NULL);
      window_fit_height(ip->pan);
      window_fit_height(ip->win);
    }

  return;
}




/*
 *  procedure to read all information form the periodic window
 *  and to put it in the postmaster.
 *
 */
periodic_read_window()
{
  periodic_win_objects *ip = periodic_ip;
  double temp;
  char *strng;
  int flag;

  if (!ip) return;

  /* period */
  strng = (char *) xv_get(ip->period, PANEL_VALUE);
  flag = *((int *) pm(GET, "Model.Mapping_Flag",NULL));
  if (flag)
    {
      flag = atoi(strng);
      if (flag < 1) flag = 1;
      pm(PUT, "Fixed.Map_Period", flag, NULL);
    }
  else
    {
      temp = atof(strng);
      if (temp < 0.0) temp = 0.0;
      pm(PUT, "Fixed.Vf_Period", temp, NULL);
    }

  /* algorithm */
  flag = (int) xv_get(ip->algorithm,PANEL_VALUE);
  pm(PUT, "Fixed.Algorithm", flag, NULL);

  /* guess */
  flag = (int) xv_get(ip->guess,PANEL_VALUE);
  pm(PUT, "Fixed.Guess", flag, NULL);

 /* show */
  flag = (int) xv_get(ip->show, PANEL_VALUE);
  pm(PUT, "Fixed.Setting", flag, NULL);

  /* mc number */
  strng = (char *) xv_get(ip->mc,PANEL_VALUE);
  pm(PUT, "Fixed.Mc_Guesses", atoi(strng), NULL);

  /* duplicate */
  strng = (char *)xv_get(ip->duplicate,PANEL_VALUE);
  pm(PUT, "Fixed.Dups", atof(strng), NULL);

  /* fxpt_iters */
  strng = (char *) xv_get(ip->fxpt_iters,PANEL_VALUE);
  pm(PUT, "Fixed.Num_Iters", atoi(strng), NULL);

  /* variable */
  strng = (char *) xv_get(ip->variable,PANEL_VALUE);
  pm(PUT, "Fixed.Var_Conv", atof(strng), NULL);

  /* function */
  strng = (char *) xv_get(ip->function,PANEL_VALUE);
  pm(PUT, "Fixed.Funct_Conv", atof(strng), NULL);

  /* fd_step */
  strng = (char *) xv_get(ip->fd_step,PANEL_VALUE);
  pm(PUT, "Fixed.FD_Step", atof(strng), NULL);

  /* distance */
  strng = (char *) xv_get(ip->distance,PANEL_VALUE);
  pm(PUT, "Fixed.Eigen_Dist", atof(strng), NULL);

  /* stabpts */
  strng = (char *) xv_get(ip->stabpts,PANEL_VALUE);
  pm(PUT, "Fixed.Stab_Points", atoi(strng), NULL);

  /* stabsteps */
  strng = (char *) xv_get(ip->stabsteps,PANEL_VALUE);
  pm(PUT, "Fixed.Stab_Steps", atoi(strng), NULL);

  /* unstabpts */
  strng = (char *) xv_get(ip->unstabpts,PANEL_VALUE);
  pm(PUT, "Fixed.Unstab_Points", atoi(strng), NULL);

  /* unstabsteps */
  strng = (char *) xv_get(ip->unstabsteps,PANEL_VALUE);
  pm(PUT, "Fixed.Unstab_Steps", atoi(strng), NULL);

}

int
periodic_close()
{
    mark_window_closed("Fixed");
    if(periodic_ip) {
	xv_set(periodic_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(periodic_ip->win, XV_SHOW, FALSE, NULL);	
    }
}
