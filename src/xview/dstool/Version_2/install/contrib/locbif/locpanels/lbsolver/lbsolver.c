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

#include "lbsolver_cui.h"
#include "lbsolver.h"
#include "../lbmain_cui.h"

extern lbmain_locbif_pu_objects        *Lbmain_locbif_pu;

lbsolver_lbsolverpu_objects	*Lbsolver_lbsolverpu;
struct Lbsolver_Ds              lbsolver_ds;

lbsolver_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;
  Menu_item    lbsolver_batch_handler();
  int	       lbsolver_field_manager();


  if (!Lbsolver_lbsolverpu)
    {
      Lbsolver_lbsolverpu = lbsolver_lbsolverpu_objects_initialize(NULL, Lbmain_locbif_pu->locbif_pu);
      register_win_and_type("Lbsolver",Lbsolver_lbsolverpu->lbsolverpu,POPUP_WINDOW);
      register_window("Lbsolver",lbsolver_batch_handler,lbsolver_field_manager);
      lbsolver_field_manager();
    }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Lbsolver_lbsolverpu->lbsolverpu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Lbsolver_lbsolverpu->lbsolverpu,rect);
      free(rect);
    }
  mark_window_open("Lbsolver");
  xv_set(Lbsolver_lbsolverpu->lbsolverpu, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Lbsolver_lbsolverpu->lbsolverpu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

int
lbsolver_close()
{
    mark_window_closed("Lbsolver");
    if(Lbsolver_lbsolverpu) {
	xv_set(Lbsolver_lbsolverpu->lbsolverpu, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(Lbsolver_lbsolverpu->lbsolverpu, XV_SHOW, FALSE, NULL);	
    }
}	

lbsolver_field_manager()
{
  if (!Lbsolver_lbsolverpu) return(-1);
	 
  lbsolver_init(); 
  lbsolver_panel_refresh();
}


lbsolver_init()
{
  lbsolver_ds.Itmap = 1;
  lbsolver_ds.Tint = 6.28;
  lbsolver_ds.H0int = 0.1;
  lbsolver_ds.Hmxint = 10.0;
  lbsolver_ds.Dhint = 0.1e-6;
  lbsolver_ds.Epsint = 0.1e-5;
  lbsolver_ds.Epsrel = 0.1e-8;
  lbsolver_ds.Solver = 1.0;
  lbsolver_ds.Isec = 1;
  lbsolver_ds.Irhs = 0;
  lbsolver_ds.Iorbit = 0;
}


int
  lbsolver_panel_refresh()
{
  char    strng[20];
   
  sprintf(strng, "  %d", lbsolver_ds.Itmap );
  xv_set( Lbsolver_lbsolverpu->lbsitmap,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbsolver_ds.Isec );
  xv_set( Lbsolver_lbsolverpu->lbsisec,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbsolver_ds.Irhs );
  xv_set( Lbsolver_lbsolverpu->lbsirhs,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbsolver_ds.Iorbit );
  xv_set( Lbsolver_lbsolverpu->lbsiorbit,PANEL_VALUE, strng, NULL);
		
  sprintf(strng, " %12.5lg", lbsolver_ds.Tint );
  xv_set( Lbsolver_lbsolverpu->lbstint,PANEL_VALUE, strng, NULL);
  sprintf(strng, " %12.5lg", lbsolver_ds.H0int );
  xv_set( Lbsolver_lbsolverpu->lbsh0int,PANEL_VALUE, strng, NULL);
  sprintf(strng, " %12.5lg", lbsolver_ds.Hmxint );
  xv_set( Lbsolver_lbsolverpu->lbshmxint,PANEL_VALUE, strng, NULL);
  sprintf(strng, " %12.5lg", lbsolver_ds.Dhint );
  xv_set( Lbsolver_lbsolverpu->lbsdhint,PANEL_VALUE, strng, NULL);
  sprintf(strng, " %12.5lg", lbsolver_ds.Epsint );
  xv_set( Lbsolver_lbsolverpu->lbsepsint,PANEL_VALUE, strng, NULL);
  sprintf(strng, " %12.5lg", lbsolver_ds.Epsrel );
  xv_set( Lbsolver_lbsolverpu->lbsepsrel,PANEL_VALUE, strng, NULL);
  sprintf(strng, " %12.5lg", lbsolver_ds.Solver );
  xv_set( Lbsolver_lbsolverpu->lbssolver,PANEL_VALUE, strng, NULL);

}



int
lbsolver_data_refresh()
{ 
/*  int		atoi(); */
/*  double	atof(); */

  lbsolver_ds.Itmap  = atoi((char *)  xv_get( Lbsolver_lbsolverpu->lbsitmap, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Isec   = atoi((char *)  xv_get( Lbsolver_lbsolverpu->lbsisec, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Irhs   = atoi((char *)  xv_get( Lbsolver_lbsolverpu->lbsirhs, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Iorbit = atoi((char *)  xv_get( Lbsolver_lbsolverpu->lbsiorbit, PANEL_VALUE, NULL)) ;

  lbsolver_ds.Tint   = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbstint, PANEL_VALUE, NULL)) ;
  lbsolver_ds.H0int  = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbsh0int, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Hmxint = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbshmxint, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Dhint  = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbsdhint, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Epsint = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbsepsint, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Epsrel = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbsepsrel, PANEL_VALUE, NULL)) ;
  lbsolver_ds.Solver = atof((char *)  xv_get( Lbsolver_lbsolverpu->lbssolver, PANEL_VALUE, NULL)) ;

  lbsolver_panel_refresh();
}


/*
* Panel open routine
*/
Menu_item
lbsolver_batch_handler(item, op)
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
	lbsolver_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else {
	pm(GET_LIST,"Win.Locn.Lbsolver",0,3,locn,NULL);
	if ((locn[0] != NO_LOCATION))
	  lbsolver_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	else
	  lbsolver_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      }
    }
/*  if (op == MENU_NOTIFY)
      lbsolver_open(DEFAULT_WIN_CONFIG,0,0,0,0);*/

      return item;
}
