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
#include <math.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>
#include <xview/cms.h>

#include <constants.h>
#include <defaults.h>
#include "ui_init.h"
#include <pm.h>
#include <mult_proj.h>
#include "mult_cui.h"
#include "../twoD/twoD.h"

static mult_win_objects *mult_ip = NULL;


/*
 * Menu handler for `panelmenu (Mult...)'.
 */
Menu_item
mult_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    mult_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Multiple",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		mult_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		mult_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}


/* 
 *  mult_open()  displays the defaults window, creating it if necessary.
 *
 */
mult_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void mult_field_manager();

  if (!mult_ip) 
    {
      mult_ip = mult_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Multiple",mult_ip->win,POPUP_WINDOW);
      mult_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      /* get the current configuration */
      frame_get_rect(mult_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      /* set the new configuration */
      frame_set_rect(mult_ip->win,rect);
      free(rect);
  }
  mark_window_open("Multiple");
  xv_set(mult_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(mult_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}



/* ------------------------------------------------------
   manager for the multiple panel fields

   ------------------------------------------------------ */
void
  mult_field_manager()
{
  int	i,n_varb,n_param,n_funct;
  char name[MAX_LEN_VARB_NAME];
  static int total_fields=0;
  void mult_data_refresh();

  if (!mult_ip) return;
  
  /* see if panel has already been opened, if so destroy old stuff */
  if (total_fields != 0)  
    {
      for (i=0; i<total_fields; i++) 
	{
	  xv_destroy(mult_ip->radius[i]);
	  xv_destroy(mult_ip->points[i]);
	}
      cfree(mult_ip->radius);
      cfree(mult_ip->points);
    }
  else
    {
      /* set up user transformations, only on creation! */
      mult_ip->transformation = mult_win_transformation_create(mult_ip, mult_ip->pan, NUM_MULT_PROJS+1);
      for (i=0; i<NUM_MULT_PROJS; i++)
	xv_set(mult_ip->transformation, PANEL_CHOICE_STRING,i+1, MULT_PROJS[i].name, NULL);
    }
  
  
  /* find out the number of variables */
  get_n_all_types(&n_varb, &n_param, &n_funct);
  total_fields = n_varb+n_param;

  /* allocate memory for them */
  mult_ip->radius = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));
  mult_ip->points = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));

  /* create the window text fields */
  for(i=0; i<n_varb; i++) 
    {
      mult_ip->radius[i] = mult_win_radius_create(mult_ip,mult_ip->pan,i);
      mult_ip->points[i] = mult_win_points_create(mult_ip,mult_ip->pan,i);
      pm(GET, "Model.Varb_Names", i, name, NULL);
      xv_set(mult_ip->radius[i], PANEL_LABEL_STRING, name, NULL);
    }
  for(i=0; i<n_param; i++) 
    {
      mult_ip->radius[n_varb+i] = mult_win_radius_create(mult_ip,mult_ip->pan,n_varb+i);
      mult_ip->points[n_varb+i] = mult_win_points_create(mult_ip,mult_ip->pan,n_varb+i);
      pm(GET, "Model.Param_Names", i, name, NULL);
      xv_set(mult_ip->radius[n_varb+i], PANEL_LABEL_STRING, name, NULL);
    }

  /* make window tall enough  */
  window_fit(mult_ip->pan);
  window_fit(mult_ip->win);

  /* write data into the fields */
  mult_data_refresh();

  return;
}


/* ------------------------------------------------
   procedure to update info in the multiple panel


   ------------------------------------------------ */
void
    mult_data_refresh()
{
  int	i, n_varb, n_param, n_funct;
  char	strng[20];
  int format = *((int *) pm(GET, "Defaults.Precision", NULL));

  if (!mult_ip) return;

  get_n_all_types(&n_varb, &n_param, &n_funct);

  for(i=0; i<n_varb+n_param; i++) 
    {
      sprintf(strng, "%.*lg",format, 
	      *((double *) pm(GET, "Mult.Radius", i, NULL)));
      xv_set(mult_ip->radius[i], PANEL_VALUE, strng, NULL);
      sprintf(strng, "%ld", 
	      *((int *) pm(GET, "Mult.Points", i, NULL)));
      xv_set(mult_ip->points[i], PANEL_VALUE, strng, NULL);
    }
  
  xv_set(mult_ip->images, PANEL_VALUE, 
	 *((int *) pm(GET, "Mult.Images", NULL)), NULL);
  xv_set(mult_ip->loadchoice, PANEL_VALUE, 
	 *((int *) pm(GET, "Mult.Load_Choice", NULL)), NULL);
  xv_set(mult_ip->transformation, PANEL_VALUE, 
	 *((int *) pm(GET, "Mult.Transformation", NULL)), NULL);
  sprintf(strng, "%.*lg",format,
	  *((double *) pm(GET, "Mult.Trans_Param", NULL)));
  xv_set(mult_ip->transparam, PANEL_VALUE, strng, NULL);

  /* update info string */
  i = *((int *) pm(GET, "Mult.Total", NULL));
  if (i==0)
    {
      xv_set(mult_ip->info, PANEL_LABEL_STRING, "No points loaded",NULL);
    }
  else
    {
      sprintf(strng,"%ld initial points loaded",i);
      xv_set(mult_ip->info, PANEL_LABEL_STRING, strng, NULL);
    }
}



mult_read_window()
{
  int i,n_varb,n_param,n_funct,iv;
  char *strng;

  if (!mult_ip) return;

  get_n_all_types(&n_varb,&n_param,&n_funct);

  for (i=0; i<n_varb+n_param; i++)
    {
      strng = (char *) xv_get(mult_ip->radius[i], PANEL_VALUE,NULL);
      pm(PUT, "Mult.Radius", i, fabs(atof(strng)), NULL);
      strng = (char *) xv_get(mult_ip->points[i], PANEL_VALUE,NULL);
      iv = atoi(strng);
      if (iv<1) iv = 1;
      pm(PUT, "Mult.Points", i, iv, NULL);
    }
  pm(PUT, "Mult.Images", (int) xv_get(mult_ip->images, PANEL_VALUE), NULL);
  pm(PUT, "Mult.Load_Choice", (int) xv_get(mult_ip->loadchoice, PANEL_VALUE), NULL);
  pm(PUT, "Mult.Transformation", (int) xv_get(mult_ip->transformation, PANEL_VALUE), NULL);
  strng = (char *) xv_get(mult_ip->transparam, PANEL_VALUE, NULL);
  pm(PUT, "Mult.Trans_Param", atof(strng), NULL);
  
}



/*
 * do_mult(window_number,mult_type,x1,y1,x2,y2)
 *         window_number where selection occurred
 *         mult_type 1 for FORWARD, 2 for BACKWARD
 *         x1,y1,x2,y2 pixel coordinates of selected rectangle/region
 *
 * routine for selecting box from mouse, loading points, and plotting
 *
 * last modified: 8/9/91  paw
 */
int
  do_mult(window_number,mult_type,x1,y1,x2,y2)
int window_number,mult_type,x1,y1,x2,y2;
{
  double pxtorx(),pytory();
  int t,n_varb,n_param,n_funct;
  int xtype,ytype,xindex,yindex;
  double xmin,xmax,ymin,ymax,xrad,x,yrad,y;

  get_n_all_types(&n_varb,&n_param,&n_funct);
  xtype = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type;
  ytype = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type;
  xindex = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index;
  yindex = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index;

  if ((xtype != PHASE_SPACE_VARB && xtype != PARAMETER_VARB) ||
      (ytype != PHASE_SPACE_VARB && ytype != PARAMETER_VARB) )
    {
      system_mess_proc(1,"do_mult: Cannot set initial conditions for functions.");
      return(1);
    }

  if (x1>x2)
    {
      t = x2;
      x2 = x1;
      x1 = t;
    }
  if (y1>y2)
    {
      t = y2;
      y2 = y1;
      y1 = t;
    }
  
  xmin = pxtorx(x1,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  xmax = pxtorx(x2,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  ymax = pytory(y1,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);
  ymin = pytory(y2,		      
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);
  xrad = (xmax-xmin) / 2.0;
  yrad = (ymax-ymin) / 2.0;
  x = (xmin+xmax) / 2.0;
  y = (ymin+ymax) / 2.0;

  orbit_read_window();
  orbit_data_refresh();
  prop_read_window();
  prop_data_refresh();
  selected_read_window();
  mult_read_window();

  if (xtype == PHASE_SPACE_VARB)
    {
      pm(PUT, "Mult.Radius", xindex, xrad,
	 PUT, "Selected.Varb_Ic", xindex, x, NULL);
    }
  else if (xtype == PARAMETER_VARB)
    {
      pm(PUT, "Mult.Radius", xindex+n_varb, xrad,
	 PUT, "Selected.Param_Ic", xindex, x, NULL);
    }

  if (ytype == PHASE_SPACE_VARB)
    {
      pm(PUT, "Mult.Radius", yindex, yrad,
	 PUT, "Selected.Varb_Ic", yindex, y, NULL);
    }
  else if (ytype == PARAMETER_VARB)
    {
      pm(PUT, "Mult.Radius", yindex+n_varb, yrad,
	 PUT, "Selected.Param_Ic", yindex, y, NULL);
    }

  sel_data_refresh();
  pm(EXEC, "Mult.Load", NULL);
  mult_data_refresh();
  if (mult_type == 1) pm(EXEC, "Mult.Forwards", NULL);
  else if (mult_type == 2) pm(EXEC, "Mult.Backwards", NULL);
  cmd_data_refresh();
  return(0);
}

int
mult_close()
{
    mark_window_closed("Multiple");
    if(mult_ip) {
	xv_set(mult_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(mult_ip->win, XV_SHOW, FALSE, NULL);	
    }
}
