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
#include <ui_init.h>
#include <defaults.h>
#include <pm.h>
#include "selected_cui.h"

static selected_win_objects *selected_ip = NULL;

/*
 * Menu handler for `settingsmenu (Selected...)'.
 */
Menu_item
   selected_handler(item, op)
Menu_item	
    item;
Menu_generate	
    op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    sel_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Selected",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		sel_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		sel_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}


/* -------------------------------------------------------------------------- 
    sel_open()  displays the selected window, creating it if necessary.

   -------------------------------------------------------------------------- */

int
    sel_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void selected_read_window(), sel_data_refresh(), sel_field_manager();

  if (!selected_ip)
    {
      selected_ip = selected_win_objects_initialize(NULL, cmd_ip->win);

      register_win_and_type("Selected",selected_ip->win,POPUP_WINDOW);

      sel_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(selected_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(selected_ip->win,rect);
      free(rect);
    }
  mark_window_open("Selected");
  xv_set(selected_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(selected_ip->win, XV_SHOW, TRUE, WIN_FRONT, 0);
}

int
sel_close()
{
    mark_window_closed("Selected");
    if(selected_ip) {
	xv_set(selected_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(selected_ip->win, XV_SHOW, FALSE, NULL);
    }
}


	
/* -------------------------------------------------------------------------------
   manager for the select panel fields

   ------------------------------------------------------------------------------- */

void
sel_field_manager()
{
  int		i;
  static int	n_varb, n_param, n_total=0;
  char 		name[MAX_LEN_VARB_NAME];
  selected_win_objects    *ip = selected_ip;
  void funct_field_manager(), sel_data_refresh();
	  
  if(!ip) return;
  funct_field_manager();
	
  if (n_total != 0)				  /* this panel has already been opened */
    {
      for (i=0;i<n_total;i++)
	{
	  xv_destroy(ip->vari[i]);
	  xv_destroy(ip->varf[i]); 
	}
      cfree(ip->vari);
      cfree(ip->varf); 
    }
	  
  n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));  /* get dimen of phase & parameter */
  n_param = *((int *) pm( GET, "Model.Param_Dim", NULL )); /* spaces from the postmaster */
  n_total = n_varb + n_param;			           /* total dimension */
	  
  ip->vari = (Xv_opaque *) calloc(n_total, sizeof(Xv_opaque)); /* allocate pointer for the numeric fields */
  ip->varf = (Xv_opaque *) calloc(n_total, sizeof(Xv_opaque));
	  
  for(i=0;i<n_varb;i++)
    {
      if (!ip->vari[i])
	ip->vari[i] = selected_win_vari_create(ip, ip->pan, i);
      if (!ip->varf[i])
	ip->varf[i] = selected_win_varf_create(ip, ip->pan, i);
      pm( GET, "Model.Varb_Names", i, name , NULL);
      xv_set(ip->vari[i], PANEL_LABEL_STRING, name , NULL);
    }
  for(i=0;i<n_param;i++)
    {
      if (!ip->vari[n_varb+i])
	ip->vari[n_varb+i] = selected_win_vari_create(ip, ip->pan, n_varb+i);
      if (!ip->varf[n_varb+i])
	ip->varf[n_varb+i] = selected_win_varf_create(ip, ip->pan, n_varb+i);
      pm( GET, "Model.Param_Names", i, name , NULL);
      xv_set(ip->vari[n_varb+i], PANEL_LABEL_STRING, name , NULL); 
    }

  window_fit(ip->pan);
  window_fit(ip->win);

  sel_data_refresh();
}
	


/* ------------------------------------------------------------------------------------
   proc used to update data fields in the selected panel points option panel
   ------------------------------------------------------------------------------------ */
void
    sel_data_refresh()
{
  int			i, format;
  char			strng[25];
  static int		n_varb, n_param;
  selected_win_objects    *ip;
  void funct_data_refresh();

  funct_data_refresh();
  ip = selected_ip;
  if(!ip) return;
  
  n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));    /* get dimen of phase and parameter, */
  n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));   /* spaces from the postmaster */
  format = *((int *) pm( GET, "Defaults.Precision", NULL )); /* get number of digits to display */
  
  for(i=0;i<n_varb;i++)
    {
      sprintf(strng,"%.*lg",format, *((double *) pm( GET, "Selected.Varb_Ic", i, NULL)));
      xv_set(ip->vari[i], PANEL_VALUE,strng, NULL);
      sprintf(strng,"%.*lg",format, *((double *) pm( GET, "Selected.Varb_Fc", i, NULL)));
      xv_set(ip->varf[i], PANEL_VALUE,strng, NULL);
    }
  for(i=0;i<n_param;i++)
    {
      sprintf(strng, "%.*lg", format, *((double *) pm( GET, "Selected.Param_Ic", i, NULL)));
      xv_set(ip->vari[i+n_varb], PANEL_VALUE,strng, NULL); 
      sprintf(strng, "%.*lg", format, *((double *) pm( GET, "Selected.Param_Fc", i, NULL)));
      xv_set(ip->varf[i+n_varb], PANEL_VALUE,strng, NULL); 
    }
}

void
selected_read_window()
{
  int 		i,n_varb,n_param;
  char 		*value;
  selected_win_objects *ip = selected_ip;
  
  if (!ip) return;
  
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  
  for(i=0; i<n_varb; i++)
    {
      value = (char *) xv_get(ip->vari[i], PANEL_VALUE);
      pm(PUT, "Selected.Varb_Ic", i, atof(value),NULL); 
    }
  
  for (i=0; i<n_param; i++)
    {
      value = (char *) xv_get(ip->vari[n_varb+i], PANEL_VALUE);
      pm(PUT, "Selected.Param_Ic", i, atof(value),NULL); 
    }
}




