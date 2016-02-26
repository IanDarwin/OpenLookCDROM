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
/* trial.c 
 * 
 * standard routines for trial window
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <xview/xview.h>

#include <pm.h>
#include <ui_init.h>
#include <constants.h>
#include "trial_cui.h"

static trial_win_objects *trial_ip = NULL;

/* 
 * trial_open(use_default,left,top,width,height)
 *
 * displays the trial window, creating it if necessary.
 */
trial_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  
    *rect;

  if (trial_ip == NULL)
    {
      /* create the window, make the owner the dstool cmd panel */
      trial_ip = trial_win_objects_initialize(NULL, cmd_ip->win);
      
      /* The following 5 lines are for Version 2.0 only. */
      /* Add this window to the "Win_Names" postmaster object so that
	 routines such as saveload know of its existence. */
      register_win_and_type("Trial",trial_ip->win,POPUP_WINDOW);
      /* Initialize any postmaster fields special to this window */
      /*    trial_install(); */ 

      trial_field_manager();
    }

  /* These lines allow the configuration of the window to be 
     restored in Version 2.0 */
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(trial_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(trial_ip->win,rect);
      free(rect);
    }


  /* show the window */
  mark_window_open("Trial");
  xv_set(trial_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(trial_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

int
trial_close()
{
    mark_window_closed("Trial");
    if(trial_ip) {
	xv_set(trial_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(trial_ip->win, XV_SHOW, FALSE, NULL);	
    }
}

/*
 * trial_field_manager()
 *
 * manager for the custom panel items
 */
trial_field_manager()
{
  int	i,n_varb,status=0;
  char name[MAX_LEN_VARB_NAME];
  static int total_fields=0;
  

  /* exit now if the window has not been created */
  if (trial_ip == NULL || status != 0) return(status);

  /* initialize data structure for the trial window for the new dynamical system */
  status = trial_init();
  
  /* destroy the custom items */
  if (total_fields > 0)  
    {
      for (i=0; i<total_fields; i++) 
        {
          xv_destroy(trial_ip->varb_value[i]);
        }
      cfree((char *) trial_ip->varb_value);
      xv_destroy(trial_ip->setting1);
    }
  
  /* find out the number of variables */
  n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL ));
  total_fields = n_varb;

  /* allocate memory for the array of text fields */
  trial_ip->varb_value = (Xv_opaque *) calloc(n_varb, sizeof(Xv_opaque));
  if (trial_ip->varb_value == NULL)
    {
      total_fields = 0;
      system_mess_proc(1,"trial_field_manager: Memory allocation error.");
      return(-1);
    }

  /* create the window text fields, and write the label into them */
  for(i=0; i<n_varb; i++) 
    {
      trial_ip->varb_value[i] = trial_win_varb_value_create(trial_ip,trial_ip->pan,i);
      pm(GET, "Model.Varb_Names", i, name, NULL);
      xv_set(trial_ip->varb_value[i], PANEL_LABEL_STRING, name, NULL);
    }

  /* set up the settings item choices */
  trial_ip->setting1 = trial_win_setting1_create(trial_ip, trial_ip->pan, n_varb);
  for (i=0; i<n_varb; i++)
    {
      pm(GET, "Model.Varb_Names", i, name, NULL);
      xv_set(trial_ip->setting1, PANEL_CHOICE_STRING, i, name, NULL);
    }	

  /* make window tall enough  */
  window_fit(trial_ip->pan);
  window_fit(trial_ip->win);

  /* write data into the fields */
  trial_data_refresh();

  return(status);
}

double *trial_varb_value = NULL;
int trial_choice;

int
  trial_init()
{
  int i,n_varb;


  n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));


  /* free up previous storage, if any */
  if (trial_varb_value != NULL) cfree((char *) trial_varb_value);

  /* create storage for operations, and set initial values */
  trial_varb_value = (double *) calloc(n_varb,sizeof(double));
  if (trial_varb_value == NULL)
    {
      system_mess_proc(1,"trial_init: Memory Allocation failed.");
      return(-1);
    }

  /* initialize the values for the text fields */
  for (i=0; i<n_varb; i++) 
    {
      trial_varb_value[i] = 0.0;
    }	      

  /* reset the setting to choice 0 */
  trial_choice = 0;

  return(0);
}

/*
 * trial_read_window()
 *
 * routine to read data from all items in the trial window
 */
int
  trial_read_window()
{
  int i, n_varb;
  char *strng;

  /* if no window, then nothing to read */
  if (trial_ip == NULL) return;

  n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));

  /* read text fields */
  for (i=0; i<n_varb; i++)
    {
      strng = (char *) xv_get(trial_ip->varb_value[i], PANEL_VALUE);
      trial_varb_value[i] = atof(strng);
    }

  /* read setting item */
  trial_choice = xv_get(trial_ip->setting1, PANEL_VALUE);
}

/*
 * trial_data_refresh()
 *
 * routine to refresh the data in all items in the trial window
 */
int
  trial_data_refresh()
{
  int format, i, n_varb;
  char strng[20];

  /* if no window, then nothing to refresh */
  if (trial_ip == NULL) return;

  /* read information on precision of doubles */
  format = *((int *) pm(GET, "Defaults.Precision", NULL));

  n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));

  /* refresh text fields */
  for(i=0; i<n_varb; i++) 
    {
      sprintf(strng, "%.*lg",format,trial_varb_value[i]);
      xv_set(trial_ip->varb_value[i], PANEL_VALUE, strng, NULL);
    }

  /* refresh setting items */
  xv_set(trial_ip->setting1, PANEL_VALUE, trial_choice, NULL);
}

/*
 * trial_handler() Version 1.1
 *
 * Menu handler for `panelmenu (Trial...)'.
 */
/*
Menu_item
  trial_handler(item, op)
Menu_item item;
Menu_generate op;
{
  if (op == MENU_NOTIFY)
    {
      trial_open();
    }
  return item;
}
*/

/*
 * trial_handler()
 *
 * Menu handler for `panelmenu (Trial...)'.
 */

Menu_item
  trial_handler(item, op)
Menu_item item;
Menu_generate op;
{
  int locn[4];

  if (op == MENU_NOTIFY)
    {
      if (item != (Menu_item) NULL)
	trial_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else 
	{
	  pm(GET_LIST, "Win.Locn.Trial", 0, 3, locn, NULL);
	  if ((locn[0] != NO_LOCATION))
	    trial_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	  else
	    trial_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
    }

  return item;
}
