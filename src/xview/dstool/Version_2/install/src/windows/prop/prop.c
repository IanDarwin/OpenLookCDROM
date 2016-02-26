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
#include <malloc.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>

#include <ui_init.h>
#include "prop_cui.h"
#include <constants.h>
#include <defaults.h>
#include <pm.h>
#include <prop.h>


int	first=TRUE, n_ifields=0, n_dfields=0, n_sel_items=0;

static prop_win_objects *prop_ip = NULL;

/* -------------------------------------------------------------------------- 
    prop_open()  displays the prop window, creating it if necessary.

   -------------------------------------------------------------------------- */
prop_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void prop_field_manager();
	
  if (!prop_ip)
    {
      prop_ip = prop_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Prop",prop_ip->win,POPUP_WINDOW);
      prop_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(prop_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(prop_ip->win,rect);
      free(rect);
    }
  mark_window_open("Prop");
  xv_set(prop_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(prop_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}
	
int
prop_close()
{
    mark_window_closed("Prop");
    if(prop_ip) {
	xv_set(prop_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(prop_ip->win, XV_SHOW, FALSE, NULL);	
    }
}	
	
/* -------------------------------------------------------------------------
   manager for the prop panel fields

   ------------------------------------------------------------------------- */
void
prop_field_manager()
{
  int			i, j;
  static char		*map_jac_choices[2][3]={ "Forw Diff","Cen Diff"," ",
						 "Explicit","Forw Diff","Cen Diff" };
  extern void		rebuild_rw_fields();
  prop_win_objects    	*ip = prop_ip;


  if(!ip) return;
	
  if (!first)	   /* this panel has already been opened */
      if (ip->prop_select) xv_destroy(ip->prop_select);

  if( *((int *) pm( GET, "Model.Mapping_Flag", NULL )) ) 
    {
      Int_Algol.Panel_Sel_Value = 0;
      j = ( *((int *) pm( GET, "Model.Jacobian_Flag", NULL )) )? 2:1;
      ip->prop_select = prop_select_create(ip, ip->pan,j+1);
      xv_set(ip->prop_select,PANEL_LABEL_STRING, "Jacobian: ",NULL);
      for (i=0; i<=j; i++)
	 xv_set(ip->prop_select, PANEL_CHOICE_STRING, i, 
		map_jac_choices[j-1][i], NULL);
    }
  else 
    {
      Int_Algol.Panel_Sel_Value = Int_Cur_Choice;
      ip->prop_select = prop_select_create(ip, ip->pan,N_Int);
      xv_set(ip->prop_select,PANEL_LABEL_STRING, "Integrator: ",NULL);
      for (i=0; i<N_Int; i++)
	xv_set(ip->prop_select, PANEL_CHOICE_STRING,i, 
	       Int_Sel[i].Int_Name, NULL);
    }

  rebuild_rw_fields();

}



/* -------------------------------------------------------------------------------
   rebuild custom read/write data fields

   ------------------------------------------------------------------------------- */

void
rebuild_rw_fields()
{
  int			i, j;
  char			*strg;
  prop_win_objects    	*ip = prop_ip;
  void prop_data_refresh();


  if (!first)		/* this panel has already been opened */
    {
      for (i=0;i<n_ifields;i++)
	xv_destroy(ip->ifields[i]);
      for (i=0;i<n_dfields;i++)
	xv_destroy(ip->dfields[i]); 
      for (i=0;i<n_sel_items;i++)
	xv_destroy(ip->prop_usr_select[i]); 
      if(n_sel_items>0) cfree(ip->prop_usr_select);
      if(n_ifields>0) cfree(ip->ifields);
      if(n_dfields>0) cfree(ip->dfields); 
    }

  first = FALSE;
  n_ifields = Int_Algol.Num_Ifields;
  n_dfields = Int_Algol.Num_Dfields;
  n_sel_items = Int_Algol.Num_Sel_Items;
  if(n_ifields>0) ip->ifields = (Xv_opaque *) calloc(Int_Algol.Num_Ifields, sizeof(Xv_opaque));	/* allocate pointer for */
  if(n_dfields>0) ip->dfields = (Xv_opaque *) calloc(Int_Algol.Num_Dfields, sizeof(Xv_opaque));	/* the numeric fields   */
  if(n_sel_items>0) ip->prop_usr_select = (Xv_opaque *) calloc(Int_Algol.Num_Sel_Items, sizeof(Xv_opaque));
	  
	
  for(i=0;i<Int_Algol.Num_Ifields;i++)
    {
      if (!ip->ifields[i])
	ip->ifields[i] = prop_win_ifields_create(ip, ip->pan, i, Int_Algol.Num_Sel_Items*30+73);
      xv_set(ip->ifields[i], PANEL_LABEL_STRING, Int_Algol.Ifield_Names[i] , NULL);
    }
	
  for(i=0;i<Int_Algol.Num_Dfields; i++)
    {
      if (!ip->dfields[i])
	ip->dfields[i] = prop_win_dfields_create(ip, ip->pan, i, Int_Algol.Num_Sel_Items*30+73);
      xv_set(ip->dfields[i], PANEL_LABEL_STRING, Int_Algol.Dfield_Names[i] , NULL);
    }

  for(i=0;i<Int_Algol.Num_Sel_Items; i++)
    {
      if (!ip->prop_usr_select[i])
	ip->prop_usr_select[i] = prop_usr_select_create(ip, ip->pan, i);
      xv_set(ip->prop_usr_select[i], PANEL_LABEL_STRING, Int_Algol.Sel_Labels[i] , NULL);
      xv_set(ip->prop_usr_select[i], PANEL_CHOICE_NROWS, Int_Algol.Num_Sel_Choices, NULL);
      for (j=0; j<Int_Algol.Num_Sel_Choices[i]; j++)
	{
	  strg = Int_Algol.Sel_Choices[i][j];
	  xv_set(ip->prop_usr_select[i], PANEL_CHOICE_STRING, j, strg, NULL);
	} 
    }
  window_fit(ip->pan);
  window_fit(ip->win);
	  
  prop_data_refresh();

}



/* -----------------------------------------------------------------------
   proc used to update data fields in the prop panel
   ----------------------------------------------------------------------- */
void
    prop_data_refresh()
{
  int		i;
  char		strng[20];
  int		format = *((int *) pm( GET, "Defaults.Precision", NULL) );
  prop_win_objects      *ip = prop_ip;
  
  if(!ip) return;
  
  xv_set(ip->prop_select, PANEL_VALUE, (int) Int_Algol.Panel_Sel_Value, NULL);

  for(i=0;i<Int_Algol.Num_Ifields;i++)
    {
      sprintf(strng,"%ld", Int_Algol.Ifields[i]);
      xv_set(ip->ifields[i], PANEL_VALUE,strng, NULL);
    }
  
  for(i=0;i<Int_Algol.Num_Dfields;i++)
    {
      sprintf(strng,"%.*lg", format, Int_Algol.Dfields[i]);
      xv_set(ip->dfields[i], PANEL_VALUE,strng, NULL);
    }
  
  for(i=0; i<Int_Algol.Num_Sel_Items; i++)
      xv_set(ip->prop_usr_select[i], PANEL_VALUE, Int_Algol.Sel_Values[i], NULL);
  return;
}



void
prop_read_window()
{
  int 			i;
  char 		        *value;
  prop_win_objects 	*ip = prop_ip;

  
  if (!ip) return;

  Int_Algol.Panel_Sel_Value = (int) xv_get(ip->prop_select, PANEL_VALUE, NULL);
  
  for(i=0; i<Int_Algol.Num_Ifields; i++)
    {
      value = (char *) xv_get(ip->ifields[i], PANEL_VALUE, NULL);
      Int_Algol.Ifields[i] = atoi(value);
    }
  
  for(i=0; i<Int_Algol.Num_Dfields; i++)
    {
      value = (char *) xv_get(ip->dfields[i], PANEL_VALUE, NULL);
      Int_Algol.Dfields[i] = atof(value);
    }

  for(i=0; i<Int_Algol.Num_Sel_Items; i++)
    {
      Int_Algol.Sel_Values[i] = (int) xv_get(ip->prop_usr_select[i], PANEL_VALUE, NULL);
    }
}

