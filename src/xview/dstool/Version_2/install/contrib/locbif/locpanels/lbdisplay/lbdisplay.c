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
#include <xview/cms.h>
#include <ui_init.h>
#include <pm.h>
#include <xview/cms.h>

#include <constants.h>
#include <user_panels.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include "lbdisplay_cui.h"
#include "lbdisplay.h"
#include "../lbmain_cui.h"

extern lbmain_locbif_pu_objects        *Lbmain_locbif_pu;

lbdisplay_lbdisplaypu_objects	*Lbdisplay_lbdisplaypu;
struct Lbdisplay_Ds              lbdisplay_ds;

lbdisplay_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;

  Menu_item    lbdisplay_batch_handler();
  int	       lbdisplay_field_manager();

  if (!Lbdisplay_lbdisplaypu)
    {
      Lbdisplay_lbdisplaypu = lbdisplay_lbdisplaypu_objects_initialize(NULL, Lbmain_locbif_pu->locbif_pu);
      register_win_and_type("Lbdisplay",Lbdisplay_lbdisplaypu->lbdisplaypu,POPUP_WINDOW);
      register_window("Lbdisplay",lbdisplay_batch_handler,lbdisplay_field_manager);
      lbdisplay_field_manager();
    }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Lbdisplay_lbdisplaypu->lbdisplaypu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Lbdisplay_lbdisplaypu->lbdisplaypu,rect);
      free(rect);
    }
  mark_window_open("Lbdisplay");
  xv_set(Lbdisplay_lbdisplaypu->lbdisplaypu, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Lbdisplay_lbdisplaypu->lbdisplaypu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}


int
lbdisplay_close()
{
    mark_window_closed("Lbdisplay");
    if(Lbdisplay_lbdisplaypu) {
	xv_set(Lbdisplay_lbdisplaypu->lbdisplaypu, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(Lbdisplay_lbdisplaypu->lbdisplaypu, XV_SHOW, FALSE, NULL);	
    }
}	

lbdisplay_field_manager()
{
  if (!Lbdisplay_lbdisplaypu) return(-1);
	 
  lbdisplay_init(); 
  lbdisplay_panel_refresh();
}


lbdisplay_init()
{
  lbdisplay_ds.Soldot = 1; 
  lbdisplay_ds.Isound = 0;
  lbdisplay_ds.Iflash = 50;
  lbdisplay_ds.Messag = 0;
  lbdisplay_ds.Maxnpt = 500;
  lbdisplay_ds.Init   = 0;
}


int
  lbdisplay_panel_refresh()
{
  char    strng[20];

  sprintf(strng, "  %d", lbdisplay_ds.Soldot );
  xv_set( Lbdisplay_lbdisplaypu->lbdsoldot,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbdisplay_ds.Isound );
  xv_set( Lbdisplay_lbdisplaypu->lbdisound,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbdisplay_ds.Iflash );
  xv_set( Lbdisplay_lbdisplaypu->lbdiflash,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbdisplay_ds.Messag );
  xv_set( Lbdisplay_lbdisplaypu->lbdmessag,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbdisplay_ds.Maxnpt );
  xv_set( Lbdisplay_lbdisplaypu->lbdmaxnpt,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", lbdisplay_ds.Init );
  xv_set( Lbdisplay_lbdisplaypu->lbdinit,PANEL_VALUE, strng, NULL);
   
}



int
lbdisplay_data_refresh()
{ 
/*  int		atoi(); */
/*  double	atof(); */

  lbdisplay_ds.Soldot  = atoi((char *)  xv_get( Lbdisplay_lbdisplaypu->lbdsoldot, PANEL_VALUE, NULL)) ;
  lbdisplay_ds.Isound  = atoi((char *)  xv_get( Lbdisplay_lbdisplaypu->lbdisound, PANEL_VALUE, NULL)) ;
  lbdisplay_ds.Iflash  = atoi((char *)  xv_get( Lbdisplay_lbdisplaypu->lbdiflash, PANEL_VALUE, NULL)) ;
  lbdisplay_ds.Messag  = atoi((char *)  xv_get( Lbdisplay_lbdisplaypu->lbdmessag, PANEL_VALUE, NULL)) ;
  lbdisplay_ds.Maxnpt  = atoi((char *)  xv_get( Lbdisplay_lbdisplaypu->lbdmaxnpt, PANEL_VALUE, NULL)) ;
  lbdisplay_ds.Init    = atoi((char *)  xv_get( Lbdisplay_lbdisplaypu->lbdinit  , PANEL_VALUE, NULL)) ;

  lbdisplay_panel_refresh();
}

/*
* Panel open routine
*/
Menu_item
lbdisplay_batch_handler(item, op)
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
	lbdisplay_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else {
	pm(GET_LIST,"Win.Locn.Lbdisplay",0,3,locn,NULL);
	if ((locn[0] != NO_LOCATION))
	  lbdisplay_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	else
	  lbdisplay_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      }
    }
/*  if (op == MENU_NOTIFY)
      lbdisplay_open(DEFAULT_WIN_CONFIG,0,0,0,0);*/

      return item;
}
