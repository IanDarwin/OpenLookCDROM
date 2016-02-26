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
#include "ui_init.h"
#include "defaults_cui.h"  

static defaults_win_objects *defaults_ip = NULL;
  
/*
 * Menu handler for `settingsmenu (Defaults...)'.
 */
Menu_item
defaults_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    def_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Defaults",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		def_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		def_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}


/* 
  def_open()  displays the defaults window, creating it if necessary.
*/  
def_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void def_field_manager();
  
  if (!defaults_ip)
    {
      defaults_ip = defaults_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Defaults",defaults_ip->win,POPUP_WINDOW);

      def_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      /* get the current configuration */
      frame_get_rect(defaults_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      /* set the new configuration */
      frame_set_rect(defaults_ip->win,rect);
      free(rect);
    }
  mark_window_open("Defaults");
  xv_set(defaults_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(defaults_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
}


/* ------------------------------------------------------
   manager for the defaults panel fields
   
   ------------------------------------------------------ */
void
def_field_manager()
{
  int	i,n_varb, n_param, n_funct,n;
  char name[MAX_LEN_VARB_NAME];
  defaults_win_objects	*ip = defaults_ip;
  static int total_fields=0;
  
  if (!ip) return;
  
  if (total_fields != 0)  /* see if panel has already been opened, if so destroy old stuff */
    {
      for (i=0; i<total_fields; i++) 
	{
	  xv_destroy(ip->varmin[i]);
	  xv_destroy(ip->varmax[i]);
	}
      cfree(ip->varmin);
      cfree(ip->varmax);
    }
  
  get_n_all_types(&n_varb, &n_param, &n_funct);/* find out the number of variables, params, functions */
  total_fields = n_varb + n_param + n_funct;
  
  ip->varmin = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));/* allocate memory for them */
  ip->varmax = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));
  
  for(i=0; i<total_fields; i++)  /* create the window text fields */
    {
      ip->varmin[i] = defaults_win_varmin_create(ip,ip->pan,i);
      ip->varmax[i] = defaults_win_varmax_create(ip,ip->pan,i);
    }
  
  for(i=0; i<n_varb; i++)   /* write the names into the field */
    {
      pm(GET, "Model.Varb_Names", i, name, NULL);
      xv_set(ip->varmin[i], PANEL_LABEL_STRING, name, NULL);
    }
  n = n_varb;
  for(i=0; i<n_param; i++) 
    {
      pm(GET, "Model.Param_Names", i, name, NULL);
      xv_set(ip->varmin[n+i], PANEL_LABEL_STRING, name, NULL);
    }
  n += n_param;
  for(i=0; i<n_funct; i++) 
    {
      pm(GET, "Model.Funct_Names", i, name, NULL);
      xv_set(ip->varmin[n+i], PANEL_LABEL_STRING, name, NULL);
    }      
  
  window_fit(ip->pan);  /* make window tall enough  */
  window_fit(ip->win);  
  
  def_data_refresh(); /* write data into the fields */
  
  xv_set(ip->coordinates, PANEL_INACTIVE, TRUE, NULL);  /* for now, make unused fields inactive */
  xv_set(ip->clip, PANEL_INACTIVE, TRUE, NULL);
  
  return;
}


/* ------------------------------------------------
   procedure to update info in the defaults panel 
   
   ------------------------------------------------ */
int
    def_data_refresh()
{
  int	i, n_varb, n_param, n_funct, n, sym, sym_size, format;
  char	strng[20];
  defaults_win_objects  *ip = defaults_ip;
  
  
  if (!ip) return MINOR_ERROR;
  
  xv_set(ip->clip, PANEL_VALUE, *((int *) pm(GET, "Defaults.Clipping", NULL)), NULL);
  xv_set(ip->record, PANEL_VALUE, *((int *) pm(GET, "Defaults.Recording", NULL)), NULL);
  format = *((int *) pm(GET, "Defaults.Precision", NULL));
  xv_set(ip->precision, PANEL_VALUE, format, NULL);
  
  sprintf(strng,"%ld", *((int *) pm(GET, "Defaults.Disp_Points", NULL)));
  xv_set(ip->disp_points, PANEL_VALUE, strng, NULL);
  
  sprintf(strng,"%.*lg",format,*((double *) pm(GET, "Flow.Diverg_Cutoff", NULL)));
  xv_set(ip->diverging, PANEL_VALUE, strng, NULL);
  
  sym_size = *((int *) pm(GET, "Defaults.Size_Index", NULL));
  sym = *((int *) pm(GET, "Defaults.Symbol_Index", NULL));
  xv_set(ip->size, PANEL_VALUE, sym_size, NULL);
  xv_set(ip->symbol, PANEL_VALUE, sym, NULL);
  
  get_n_all_types(&n_varb, &n_param, &n_funct);
  
  for(i=0; i<n_varb; i++) {
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Defaults.Varb_Min",
						    i, NULL)));
    xv_set(ip->varmin[i], PANEL_VALUE, strng, NULL);
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Defaults.Varb_Max",
						    i, NULL)));
    xv_set(ip->varmax[i], PANEL_VALUE, strng, NULL);
  }
  
  n = n_varb;
  for(i=0; i<n_param; i++) {
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Defaults.Param_Min",
						    i, NULL)));
    xv_set(ip->varmin[n+i], PANEL_VALUE, strng, NULL);
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Defaults.Param_Max",
						    i, NULL)));
    xv_set(ip->varmax[n+i], PANEL_VALUE, strng, NULL);
  }
  
  n += n_param;
  for(i=0; i<n_funct; i++) {
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Defaults.Funct_Min", i, NULL)));
    xv_set(ip->varmin[n+i], PANEL_VALUE, strng, NULL);
    sprintf(strng, "%.*lg", format, *((double *) pm(GET, "Defaults.Funct_Max", i, NULL)));
    xv_set(ip->varmax[n+i], PANEL_VALUE, strng, NULL);
  }
  return 0;
}




def_read_window()
{
  int i,n,n_varb,n_param,n_funct,sym,sym_size;
  char *strng;
  defaults_win_objects *ip = defaults_ip;
  
  if (!ip) return;

  pm(PUT, "Defaults.Recording", (int) xv_get(ip->record, PANEL_VALUE), NULL);
  pm(PUT, "Defaults.Clipping", (int) xv_get(ip->clip, PANEL_VALUE), NULL);
  
  n = ((int) xv_get(ip->precision, PANEL_VALUE) > MAX_PRECISION)? 
    MAX_PRECISION:(int) xv_get(ip->precision, PANEL_VALUE);
  pm (PUT, "Defaults.Precision", n, NULL);
  
  strng = (char *) xv_get(ip->disp_points, PANEL_VALUE);
  pm (PUT, "Defaults.Disp_Points", atoi(strng), NULL);
  
  strng = (char *) xv_get(ip->diverging, PANEL_VALUE);
  pm (PUT, "Flow.Diverg_Cutoff", atof(strng), NULL);
  
  sym = (int) xv_get(ip->symbol, PANEL_VALUE);
  sym_size = (int) xv_get(ip->size, PANEL_VALUE);
  pm (PUT, "Defaults.Symbol_Index", sym, NULL);
  pm (PUT, "Defaults.Size_Index", sym_size, NULL);
  
  get_n_all_types(&n_varb,&n_param,&n_funct);
  for (i=0; i<n_varb; i++)
    {
      strng = (char *) xv_get(ip->varmin[i], PANEL_VALUE);
      pm(PUT, "Defaults.Varb_Min", i, atof(strng),NULL);
      strng = (char *) xv_get(ip->varmax[i], PANEL_VALUE);
      pm(PUT, "Defaults.Varb_Max", i, atof(strng),NULL);
    }
  n = n_varb;
  for (i=0; i<n_param; i++)
    {
      strng = (char *) xv_get(ip->varmin[n+i], PANEL_VALUE);
      pm(PUT, "Defaults.Param_Min", i, atof(strng),NULL);
      strng = (char *) xv_get(ip->varmax[n+i], PANEL_VALUE);
      pm(PUT, "Defaults.Param_Max", i, atof(strng),NULL);
    }
  n += n_param;
  for (i=0; i<n_funct; i++)
    {
      strng = (char *) xv_get(ip->varmin[n+i], PANEL_VALUE);
      pm(PUT, "Defaults.Funct_Min", i, atof(strng),NULL);
      strng = (char *) xv_get(ip->varmax[n+i], PANEL_VALUE);
      pm(PUT, "Defaults.Funct_Max", i, atof(strng),NULL);
    }
  
}

int
defaults_close()
{
    mark_window_closed("Defaults");
    if(defaults_ip) {
	xv_set(defaults_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(defaults_ip->win, XV_SHOW, FALSE, NULL);	
    }
}
