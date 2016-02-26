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

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <ui_init.h>
#include <pm.h>
#include <xview/cms.h>

#include <constants.h>
#include <user_panels.h>

#include "lbcont_cui.h"
#include "lbcont.h"
#include "../lbmain_cui.h"

extern lbmain_locbif_pu_objects        *Lbmain_locbif_pu;

struct Lbcont_Ds        	lbcont_ds;
lbcont_lbcontpu_objects 	*Lbcont_lbcontpu;

lbcont_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;
  Menu_item    lbcont_batch_handler();
  int	       lbcont_field_manager();

  if (!Lbcont_lbcontpu)
    {
      Lbcont_lbcontpu = lbcont_lbcontpu_objects_initialize(NULL, Lbmain_locbif_pu->locbif_pu);
      register_win_and_type("Lbcont",Lbcont_lbcontpu->lbcontpu,POPUP_WINDOW);
      register_window("Lbcont",lbcont_batch_handler,lbcont_field_manager);
      lbcont_field_manager();
    }

  lbcont_panel_refresh();

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Lbcont_lbcontpu->lbcontpu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Lbcont_lbcontpu->lbcontpu,rect);
      free(rect);
    }
  mark_window_open("Lbcont");
  xv_set(Lbcont_lbcontpu->lbcontpu, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Lbcont_lbcontpu->lbcontpu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

	
int
lbcont_close()
{
    mark_window_closed("Lbcont");
    if(Lbcont_lbcontpu) {
	xv_set(Lbcont_lbcontpu->lbcontpu, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(Lbcont_lbcontpu->lbcontpu, XV_SHOW, FALSE, NULL);	
    }
}	


lbcont_field_manager()
{
  int	        i, n_varb, n_param;

  if (!Lbcont_lbcontpu) return(-1);
}


lbcont_init()
{
  lbcont_ds.Maxit = 7;
  lbcont_ds.Modit = 2;
  lbcont_ds.Iprsng = 1;
  lbcont_ds.H0crv = 0.1;
  lbcont_ds.Hmxcrv = 1.0;
  lbcont_ds.Angcrv = 10.0;
  lbcont_ds.Dhcrv = 1.0e-7;
  lbcont_ds.Dhjac = 1.0e-7;
  lbcont_ds.Epscrv = 1.0e-4;
  lbcont_ds.Epscrs = 1.0e-3;
  lbcont_ds.Epszer = 1.0e-3;
  lbcont_ds.Epsext = 1.0e-3;
  lbcont_ds.Algcrv = 2;
}


int
lbcont_panel_refresh()
{
  int	  i, n_varb, n_param;
  char    strng[20];

/*  
  n_varb = *((int *) pm(GET, Traj_Ds_Object, Varb_Dim, NULL)) - 1;
  n_param = *((int *) pm(GET, Traj_Ds_Object, Param_Dim, NULL));
*/
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

  
  xv_set( Lbcont_lbcontpu->lbcmaxit,PANEL_VALUE, lbcont_ds.Maxit, NULL);
  xv_set( Lbcont_lbcontpu->lbcmodit,PANEL_VALUE, lbcont_ds.Modit, NULL);
  xv_set( Lbcont_lbcontpu->lbciprsng,PANEL_VALUE, lbcont_ds.Iprsng, NULL);
   
  sprintf(strng, "%12.5lg", lbcont_ds.H0crv );
  xv_set( Lbcont_lbcontpu->lbch0crv,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Hmxcrv );
  xv_set( Lbcont_lbcontpu->lbchmxcrv,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Angcrv );
  xv_set( Lbcont_lbcontpu->lbcangcrv,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Dhcrv );
  xv_set( Lbcont_lbcontpu->lbcdhcrv,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Dhjac );
  xv_set( Lbcont_lbcontpu->lbcdhjac,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Epscrv );
  xv_set( Lbcont_lbcontpu->lbcepscrv,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Epscrs );
  xv_set( Lbcont_lbcontpu->lbcepscrs,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Epszer );
  xv_set( Lbcont_lbcontpu->lbcepszer,PANEL_VALUE, strng, NULL);
  sprintf(strng, "%12.5lg", lbcont_ds.Epsext );
  xv_set( Lbcont_lbcontpu->lbcepsext,PANEL_VALUE, strng, NULL);
  
  sprintf(strng, "%d", lbcont_ds.Algcrv );
  xv_set( Lbcont_lbcontpu->lbcalgcrv,PANEL_VALUE, lbcont_ds.Algcrv, NULL);
   

}



int
lbcont_data_refresh()
{ 
/*  int		atoi(); */
/*  double	atof(); */

  lbcont_ds.Maxit  = (int)  xv_get( Lbcont_lbcontpu->lbcmaxit, PANEL_VALUE, NULL) ;
  lbcont_ds.Modit  = (int)  xv_get( Lbcont_lbcontpu->lbcmodit, PANEL_VALUE, NULL) ;
  lbcont_ds.Iprsng = (int)  xv_get( Lbcont_lbcontpu->lbciprsng, PANEL_VALUE, NULL) ;
  lbcont_ds.Algcrv = (int)  xv_get( Lbcont_lbcontpu->lbcalgcrv, PANEL_VALUE, NULL) ; 

  lbcont_ds.H0crv  = atof((char *)  xv_get( Lbcont_lbcontpu->lbch0crv, PANEL_VALUE, NULL)) ;
  lbcont_ds.Hmxcrv = atof((char *)  xv_get( Lbcont_lbcontpu->lbchmxcrv, PANEL_VALUE, NULL)) ;
  lbcont_ds.Angcrv = atof((char *)  xv_get( Lbcont_lbcontpu->lbcangcrv, PANEL_VALUE, NULL)) ;
  lbcont_ds.Dhcrv  = atof((char *)  xv_get( Lbcont_lbcontpu->lbcdhcrv, PANEL_VALUE, NULL)) ;
  lbcont_ds.Dhjac  = atof((char *)  xv_get( Lbcont_lbcontpu->lbcdhjac, PANEL_VALUE, NULL)) ;
  lbcont_ds.Epscrv = atof((char *)  xv_get( Lbcont_lbcontpu->lbcepscrv, PANEL_VALUE, NULL)) ;
  lbcont_ds.Epscrs = atof((char *)  xv_get( Lbcont_lbcontpu->lbcepscrs, PANEL_VALUE, NULL)) ;
  lbcont_ds.Epszer = atof((char *)  xv_get( Lbcont_lbcontpu->lbcepszer, PANEL_VALUE, NULL)) ;
  lbcont_ds.Epsext = atof((char *)  xv_get( Lbcont_lbcontpu->lbcepsext, PANEL_VALUE, NULL)) ;

  lbcont_panel_refresh();
}

/*
* Panel open routine
*/
Menu_item
lbcont_batch_handler(item, op)
Menu_item       item;
Menu_generate   op;
{

    int
	locn[4];
    if (op == MENU_NOTIFY) {
      /* open Locbif main window if not already done */
      if (!pm_exists_entry("Win.Open_Status.LOCBIF"))
	lbmain_open(DEFAULT_WIN_CONFIG,0,0,0,0);

      if (item != (Menu_item) NULL)
	lbcont_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else {
	pm(GET_LIST,"Win.Locn.Lbcont",0,3,locn,NULL);
	if ((locn[0] != NO_LOCATION))
	  lbcont_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	else
	  lbcont_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      }
    }
/*  if (op == MENU_NOTIFY)
      lbcont_open(DEFAULT_WIN_CONFIG,0,0,0,0);*/

      return item;
}
