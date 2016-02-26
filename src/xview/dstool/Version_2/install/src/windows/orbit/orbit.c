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
#include "ui_init.h"
#include <defaults.h>
#include <memory.h>
#include "orbit_cui.h"

static orbit_win_objects *orbit_ip = NULL;

/*
 * Menu handler for `panelmenu (Orbit...)'.
 */
Menu_item
  orbit_handler(item, op)
Menu_item	item;
Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    orbit_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Orbits",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		orbit_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		orbit_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  if(op == MENU_NOTIFY)
    orbit_open(DEFAULT_WIN_CONFIG,0,0,0,0);
  return item;
}



/* 
 *  orbit_open()  displays the orbit window, creating it if necessary.
 */
orbit_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void orbit_field_manager();

  if (!orbit_ip)
    {
      orbit_ip = orbit_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Orbits",orbit_ip->win,POPUP_WINDOW);
      orbit_field_manager();
    }
  if (use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      /* get the current configuration */
      frame_get_rect(orbit_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      /* set the new configuration */
      frame_set_rect(orbit_ip->win,rect);
      free(rect);
    }
  mark_window_open("Orbits");
  xv_set(orbit_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(orbit_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
}



/*
 * orbit_field_manager()  field manager for the orbit window
 */
void
  orbit_field_manager()
{
  int n_varb, n_funct, i, offset, start;
  char name[MAX_LEN_VARB_NAME], name1[MAX_LEN_VARB_NAME+6]; 
      /* 6 = strlen("Fixed ") */
  orbit_win_objects *ip = orbit_ip;
  static int total_fields=0;
  void orbit_data_refresh();
  
  if (!ip) return;

  if (total_fields != 0)
    {
      for (i=0; i<total_fields; i++)
	xv_destroy(ip->stop_event[i]);
      cfree(ip->stop_event);
      xv_destroy(ip->stop_cond);
    }

  /* find out the number of variables */
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_funct = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  if ( *((int *)pm(GET, "Model.Mapping_Flag", NULL)) )    /* mapping */
    {
      total_fields = n_varb + n_funct - 1;
      ip->stop_event = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));
      xv_set(ip->stepsize, PANEL_INACTIVE, TRUE, NULL);
      ip->stop_cond = orbit_win_stop_cond_create(ip,ip->pan,2);
      xv_set(ip->stop_cond, PANEL_VALUE, 0, NULL);
      start = 0;
      offset = n_varb-1;
    }
  else					                /* vector field */
    {  
      total_fields = n_varb+n_funct;
      ip->stop_event = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));
      xv_set(ip->stepsize, PANEL_INACTIVE, FALSE, NULL);
      ip->stop_event[0] = orbit_win_stop_event_create(ip,ip->pan,0);
      pm(GET, "Model.Varb_Names", n_varb-1, name, NULL);
      xv_set(ip->stop_event[0], PANEL_LABEL_STRING, name, NULL);
      ip->stop_cond = orbit_win_stop_cond_create(ip,ip->pan,4);
      pm(GET, "Model.Varb_Names", n_varb-1, name, NULL);
      sprintf(name1,"Fixed %s",name);
      xv_set(ip->stop_cond, PANEL_VALUE, 0, 
	     PANEL_CHOICE_STRING, 2, name1, 
	     PANEL_CHOICE_STRING, 3, "Poincare Section", 
	     NULL);
      start = 1;
      offset = n_varb;
    }

  for (i=start; i<offset; i++)
     {
      ip->stop_event[i] = orbit_win_stop_event_create(ip,ip->pan,i);
      pm(GET, "Model.Varb_Names", i-start, name, NULL);
      xv_set(ip->stop_event[i], PANEL_LABEL_STRING, name, NULL);
     }
  for (i=offset; i<total_fields; i++)
     {
      ip->stop_event[i] = orbit_win_stop_event_create(ip,ip->pan,i);
      pm(GET, "Model.Funct_Names", i-offset, name, NULL);
      xv_set(ip->stop_event[i], PANEL_LABEL_STRING, name, NULL);
     }
  

  /* make window tall enough  */
  window_fit(ip->pan);
  window_fit(ip->win);

  /* make the unimplemented panel items inactive */
  xv_set(orbit_ip->interrupt, PANEL_INACTIVE, TRUE, NULL);
  
  orbit_data_refresh();
  return;
}


/*
 * orbit_data_refresh()  refreshes the orbit window with postmaster data
 */
void
    orbit_data_refresh()
{
  char    strng[20];
  int n,i,n_varb, n_funct, max, *ivector(),*sc, total_fields, offset;
  double *dvector(),*scv;
  int		format = *((int *) pm( GET, "Defaults.Precision", NULL) );
  
  orbit_win_objects *ip = orbit_ip;

  if (!ip) return;

  /* find out the number of variables */
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_funct = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  if ( *((int *)pm(GET, "Model.Mapping_Flag", NULL)) )
       total_fields = n_varb+n_funct-1;
  else
       total_fields = n_varb+n_funct;

  max = (n_varb > n_funct) ? n_varb : n_funct;
  sc = ivector(0,max-1);
  scv = dvector(0,max-1);
  for(i=0; i<max; i++)
    {
     sc[i] = 0; scv[i] = 0.0;
    }

  /* set  propagation mode */
  n = *((int *) pm(GET, "Flow.Stopping_Condition", NULL));
  if(n==PROP_NSTEP) 
     n = 0;
  else if( n==PROP_FSTOP )
     n = 1;
  else if( n==PROP_TF )
     n = 2;
  else if( n==PROP_POINCARE)  /* paw  4/14/92 */
     n = 3;
  else
     n = 0;			/* must fix this default! mrm */

 
  xv_set(ip->stop_cond, PANEL_VALUE, n, NULL);

  orbit_toggle_skip_field(n); /* n corresponds to index in stopping condition
			       settings. */


  sprintf(strng,"%d",*((int *) pm( GET, "Flow.Total_Iterates", NULL)));
  xv_set(ip->stop, PANEL_VALUE,strng, NULL);
  sprintf(strng,"%d",*((int *) pm( GET, "Flow.Start_Save_Points", NULL)));
  xv_set(ip->start, PANEL_VALUE,strng, NULL);
  sprintf(strng,"%d",*((int *) pm( GET, "Flow.Skip_Size", NULL)));
  xv_set(ip->skip, PANEL_VALUE,strng, NULL);
  sprintf(strng,"%.*lg", format, *((double *) pm( GET, "Flow.Stepsize", NULL)));
  xv_set(ip->stepsize, PANEL_VALUE,strng, NULL);

  /* write to the stopping event fields */
 
   if (n==0)
    {
      for (i=0; i<total_fields; i++)
	xv_set(ip->stop_event[i], PANEL_INACTIVE, TRUE, NULL);
    }
  else if (n==1 || n==3)   /* n==1  OR  n==3   turn time off, turn vars/fns on  */       /* paw  4/14/92 */
    {
      for (i=0; i<total_fields; i++)
	xv_set(ip->stop_event[i], PANEL_INACTIVE, FALSE, NULL);
    }
  else
    {
      xv_set(ip->stop_event[0], PANEL_INACTIVE, FALSE, NULL);
      for (i=1; i<total_fields; i++)
	xv_set(ip->stop_event[i], PANEL_INACTIVE, TRUE, NULL);
    }
  
  
  pm(GET_LIST, "Flow.Varb_Events", 0, n_varb-1, sc, NULL);
  pm(GET_LIST, "Flow.Varb_Event_Values", 0, n_varb-1, scv, NULL);
  if ( *((int *)pm(GET, "Model.Mapping_Flag", NULL)) )
    {
     for (i=0; i<n_varb-1; i++)
       {
        if (sc[i])
	   sprintf(strng,"%.*lg",format,scv[i]);
        else sprintf(strng,"");
        xv_set(ip->stop_event[i], PANEL_VALUE, strng, NULL);
       }
     offset = n_varb-1;
    }
  else
    {
     if(n==1 || n==3)				/* paw  4/14/92 */
	xv_set(ip->stop_event[0], PANEL_INACTIVE, TRUE, NULL);
        for (i=0; i<n_varb-1; i++)
          {
           if (sc[i])
	      sprintf(strng,"%.*lg",format,scv[i]);
           else sprintf(strng,"");
           xv_set(ip->stop_event[i+1], PANEL_VALUE, strng, NULL);
          }
     if(n==2)
       {
        if (sc[n_varb-1])
          sprintf(strng,"%.*lg",format,scv[n_varb-1]);
        else sprintf(strng,"");
        xv_set(ip->stop_event[0], PANEL_VALUE, strng, NULL);
       }
     offset = n_varb;
    }
  pm(GET_LIST, "Flow.Funct_Events", 0, n_funct-1, sc, NULL);
  pm(GET_LIST, "Flow.Funct_Event_Values", 0, n_funct-1, scv, NULL);
  for (i=0; i<n_funct; i++)
    {
      if (sc[i])
	sprintf(strng,"%.*lg",format,scv[i]);
      else sprintf(strng,"");
      xv_set(ip->stop_event[i+offset], PANEL_VALUE, strng, NULL);
    }

  free_ivector(sc,0,max-1);
  free_dvector(scv,0,max-1);
  return;
}



/*
 * orbit_read_window()  copies data from orbit window into postmaster
 */
void
  orbit_read_window()
{
  int n_varb, n_funct, i, max, *sc, *ivector(), offset, start;
  char *value,*pstr;
  double *scv, *dvector();
  orbit_win_objects *ip = orbit_ip;
  extern double strtod();

  if (!ip) return;

  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_funct = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  max = (n_varb > n_funct) ? n_varb : n_funct;
  sc = ivector(0,max-1);
  scv = dvector(0,max-1);
  for(i=0; i<max; i++)
    {
     sc[i] = 0; scv[i] = 0.0;
    }

  value = (char *) xv_get(ip->stop, PANEL_VALUE);
  if ( (i=atoi(value)) < 0) i=0;				/* paw  4/14/92 */
  pm(PUT, "Flow.Total_Iterates", i, NULL);

  value = (char *) xv_get(ip->start, PANEL_VALUE);
  if ( (i=atoi(value)) < 0) i=0; 				/* paw  4/14/92 */
  pm(PUT, "Flow.Start_Save_Points", i, NULL);

  value = (char *) xv_get(ip->skip, PANEL_VALUE);
  if ( (i=atoi(value)) < 0) i=0;
  pm(PUT, "Flow.Skip_Size", i, NULL);

  value = (char *) xv_get(ip->stepsize, PANEL_VALUE);
  pm(PUT, "Flow.Stepsize", atof(value), NULL);

  switch((int) xv_get(ip->stop_cond, PANEL_VALUE)) {
    case 0:
      pm(PUT, "Flow.Stopping_Condition", PROP_NSTEP, NULL);
      break;
    case 1:
      pm(PUT, "Flow.Stopping_Condition", PROP_FSTOP, NULL);
      break;
    case 2:
      pm(PUT, "Flow.Stopping_Condition", PROP_TF, NULL);
      break;
    case 3:
      pm(PUT, "Flow.Stopping_Condition", PROP_POINCARE, NULL);
      break;  }


  if ( *((int *)pm(GET, "Model.Mapping_Flag", NULL)) )
     {
      start = 0;
      offset = n_varb-1; 
     }
  else
     {
      if((int) xv_get(ip->stop_cond, PANEL_VALUE)==2)
	 {
          value = (char *) xv_get(ip->stop_event[0], PANEL_VALUE);
          scv[n_varb-1] = strtod(value, &pstr);
          sc[n_varb-1] = (value == pstr) ? 0 : 1;
          pm(PUT, "Flow.Final_Time", scv[n_varb-1], NULL);
         }
      start = 1;
      offset = n_varb;
     }

  for (i=0; i<n_varb-1; i++)
    {
     value = (char *) xv_get(ip->stop_event[i+start], PANEL_VALUE);
     scv[i] = strtod(value, &pstr);
     sc[i] = (value == pstr) ? 0 : 1;
    }
  pm(PUT_LIST, "Flow.Varb_Events", 0, n_varb-1, sc, NULL);
  pm(PUT_LIST, "Flow.Varb_Event_Values", 0, n_varb-1, scv, NULL);

  for (i=0; i<n_funct; i++)
    {
      value = (char *) xv_get(ip->stop_event[i+offset], PANEL_VALUE);
      scv[i] = strtod(value, &pstr);
      sc[i] = (value == pstr) ? 0 : 1;
    }
  pm(PUT_LIST, "Flow.Funct_Events", 0, n_funct-1, sc, NULL);
  pm(PUT_LIST, "Flow.Funct_Event_Values", 0, n_funct-1, scv, NULL);

  free_ivector(sc,0,max-1);
  free_dvector(scv,0,max-1);
}


/*
 * clear_all_orbits()  clears the trajectory memory object and refreshes 
 *                     the viewing windows
 */
clear_all_orbits()
{
  clear_win_data(-1);
}


/* --------------------------------------------------------------------

   clear_win_data()

   proc used to reset memory object selectively according to the 
   number of dynamical system parameters of interest to the
   calling procedure.  If passed a -1, it will clear all orbits.
   If passed an integer other than -1, 0, 1, or 2
   proc will reset all memory objects and reset all screens

   mrm

   -------------------------------------------------------------------- */
clear_win_data( num_sel_param )
int	num_sel_param;
{
  void cmd_data_refresh();

  switch ( num_sel_param )
    {
    case 0: 
      pm(
	 INIT, "Memory.Traj", TRAJ_MEMORY,
	 INIT, "Memory.Mult", MULT_MEMORY,
	 INIT, "Memory.Sel_Pt", SEL_PT_MEMORY,
	 INIT, "Memory.Cont", CONT_MEMORY,
	 PUT , "Flow.Direction", FORWARD, 
	 NULL);
      reset_color();
      break;
    case 1:
      pm(INIT, "Memory.Sel_Pt", SEL_PT_MEMORY,
	 INIT, "Memory.Cont", CONT_MEMORY,
	 NULL);
      break;
    case 2:
      pm(INIT, "Memory.Param", PARAM_MEMORY,
	 INIT, "Memory.Cont", CONT_MEMORY,
	       NULL);
      break; 
    case -1:
      pm(INIT, "Memory.Traj", TRAJ_MEMORY,
	 /* INIT, "Memory.Mult", MULT_MEMORY, */
	 PUT, "Flow.Direction", FORWARD, NULL);
      reset_color();
      break;
    default:
      pm(INIT, "Memory.Traj",  TRAJ_MEMORY,
	 INIT, "Memory.Mult", MULT_MEMORY,
	 INIT, "Memory.Param", PARAM_MEMORY,
	 /* INIT, "Memory.Fixed" , FIXPT_MEMORY, */
	 INIT, "Memory.Sel_Pt", SEL_PT_MEMORY,
	 INIT, "Memory.Cont", CONT_MEMORY,
	 PUT, "Flow.Direction", FORWARD, 
	 NULL);
      reset_color();
	    break;
    }
  
  cmd_data_refresh();
  refresh_all_win();
}



/* Toggle between "Max Steps" and "Plot" for skip field depending
   on whether or not we are in the Poincare panel case.
*/
int
    orbit_toggle_skip_field(settings_index)
int
    settings_index;
{
    orbit_win_objects 
	*ip = orbit_ip;

    static int
	old_settings_index = 0;

    if (!ip) 
	return;

    if (settings_index != old_settings_index) {
	if (settings_index == 3) /* Poincare section case */ {
	    xv_set(ip->skip,
		   PANEL_LABEL_STRING,"Max Steps:",
		   PANEL_VALUE_X, 87,
		   NULL);
	    xv_set(ip->stepsize,
		   PANEL_VALUE_X, 230,
		   NULL);
	    pm(PUT, "Flow.Skip_Size", TOTAL_ITERATES, NULL);
	}
	else if (old_settings_index == 3){
	    xv_set(ip->skip,PANEL_LABEL_STRING,"Plot:",
		   PANEL_VALUE_X, 50,
		   NULL);
	    xv_set(ip->stepsize,
		   PANEL_VALUE_X, 210,
		   NULL);
	    pm(PUT, "Flow.Skip_Size", SKIP_SIZE, NULL);
	}
    }
    old_settings_index = settings_index;
}

int
orbit_close()
{
    mark_window_closed("Orbits");
    if(orbit_ip) {
	xv_set(orbit_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(orbit_ip->win, XV_SHOW, FALSE, NULL);	
    }
}
