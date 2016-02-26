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

#include <constants.h>
#include <ui_init.h>  
#include "function_cui.h"

static function_win_objects  *function_ip = NULL;

/*
 * Menu handler for `settingsmenu (Function...)'.
 */
Menu_item
function_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    funct_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Function",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		funct_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		funct_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;

}


/* 
funct_open()  displays the function window, creating it if necessary.
*/
funct_open(use_default,left,top,width,height)
int	use_default; 	/* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int	left;		/* position (on screen) of left edge of window.  */
int	top;		/* position (on screen) of top edge of window.   */
int	width;		/* width of window;  width <= 0 means use default width */
int	height;		/* height of window; height <= 0 means use default width */
{
  Rect	*rect;
  void funct_field_manager();

  if (!function_ip) 
    {
      function_ip = function_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Function",function_ip->win,POPUP_WINDOW);
      funct_field_manager();
    }
  if (use_default == SET_WIN_CONFIG) 
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(function_ip->win,rect); /* get the current configuration */
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if (width>0) rect->r_width = (short) width;
      if (height>0) rect->r_height = (short) height;
      frame_set_rect(function_ip->win,rect); /* set the new configuration */
      free(rect);
    }
  mark_window_open("Function");
  xv_set(function_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(function_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
}

/* 
funct_field_manager() manager for the function panel fields
*/
void
funct_field_manager()
{
  void funct_data_refresh();

  int			i;
  static int		total_fields=0;
  char 			name[MAX_LEN_VARB_NAME];
  function_win_objects	*ip = function_ip;

  if (!ip) return;

  if (total_fields != 0) 
    {						  /*see if panel has already been opened, if so destroy old stuff */
      for (i=0; i<total_fields; i++) 
	{
	  xv_destroy(ip->funi[i]);
	  xv_destroy(ip->funf[i]);
	}
      cfree(ip->funi);
      cfree(ip->funf);
    }
	
  total_fields = *((int *) pm( GET, "Model.Funct_Dim", NULL)); /* find out the number of functions */
	
  ip->funi= (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque)); /* allocate memory for them */
  ip->funf = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));

  for (i=0; i<total_fields; i++) 
    {						  /* create the window text fields and add labels */
      if (!ip->funi[i])
	ip->funi[i] = function_win_funi_create(ip,ip->pan,i);
      if (!ip->funf[i])
	ip->funf[i] = function_win_funf_create(ip,ip->pan,i);
      pm(GET, "Model.Funct_Names", i, name, NULL);
      xv_set(ip->funi[i], PANEL_LABEL_STRING, name, NULL);
    }
	
  window_fit(ip->pan);		  /* make window tall enough  */
  window_fit(ip->win);

  funct_data_refresh();		  /* write data into the fields */
}


/*
funct_data_refresh() procedure to update info in the function panel initial/final section
*/
void
  funct_data_refresh()
{
  int			i, f_dim, v_dim, p_dim,format;
  double       		*func, *varb, *param;
  char			strng[20];
  double 	       	*dvector();
  int			get_ds_func();
  function_win_objects  *ip;

  ip = function_ip;
  if ( (!ip) || !(valid_func() ) )
    return;

  f_dim = *((int *) pm(GET, "Model.Funct_Dim", NULL)); 
  v_dim = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *) pm(GET, "Model.Param_Dim", NULL));
  format = *((int *) pm(GET, "Defaults.Precision", NULL));

  func = dvector(0,f_dim-1);		  /* allocate temp space for impt quantities */ 
  varb = dvector(0,v_dim-1);
  param = dvector(0,p_dim-1);

  pm(GET_LIST, "Selected.Varb_Ic",0,v_dim-1,varb,NULL); /* first the initial conditions */
  pm(GET_LIST, "Selected.Param_Ic",0,p_dim-1,param,NULL); 
  get_ds_func(func, varb, param);		  /* get the function values. */
  for (i=0; i<f_dim; i++) 
    {						  /* record function values */
      sprintf(strng, "%.*lg",format, func[i]);
      xv_set(ip->funi[i], PANEL_VALUE, strng, NULL);
    }
  pm(GET_LIST, "Selected.Varb_Fc",0,v_dim-1,varb,NULL); /* now final conditions */
  pm(GET_LIST, "Selected.Param_Fc",0,p_dim-1,param,NULL);
  get_ds_func(func, varb, param);
  for (i=0; i<f_dim; i++) 
    {
      sprintf(strng, "%.*lg",format, func[i]);
      xv_set(ip->funf[i], PANEL_VALUE, strng, NULL);
    }
	
  free_dvector(func,0,f_dim-1);
  free_dvector(varb,0,v_dim-1);
  free_dvector(param,0,p_dim-1);
}


int
function_close()
{
    mark_window_closed("Function");
    if(function_ip) {
	xv_set(function_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(function_ip->win, XV_SHOW, FALSE, NULL);
    }
}

void
    function_done_proc(frame)
Frame
    frame;
{
    function_close();
}	
