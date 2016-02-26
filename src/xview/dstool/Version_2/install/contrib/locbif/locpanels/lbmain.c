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
#include <xview/cms.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <pm.h>
#include <ui_init.h>

#include <constants.h>
#include <user_panels.h>

#include "lbmain_cui.h"
#include "lbmain.h"
#include "lbstate/lbstate_cui.h"

struct Lbmain_Ds        			lbmain_ds;
lbmain_locbif_pu_objects        		*Lbmain_locbif_pu;
extern lcstate_locstatepu_objects               *Lcstate_locstatepu;

lbmain_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
    Rect  *rect;
    extern void lb_install();
    if (!Lbmain_locbif_pu) {
      Lbmain_locbif_pu = lbmain_locbif_pu_objects_initialize(NULL, cmd_ip->win);
/*      register_win_and_type("One",Lbmain_locbif_pu->locbif_pu,POPUP_WINDOW);*/
      register_win_and_type("LOCBIF",Lbmain_locbif_pu->locbif_pu,POPUP_WINDOW);
      lb_install();
      lbmain_field_manager();
    }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Lbmain_locbif_pu->locbif_pu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Lbmain_locbif_pu->locbif_pu,rect);
      free(rect);
    }
  mark_window_open("LOCBIF");
  xv_set(Lbmain_locbif_pu->locbif_pu, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Lbmain_locbif_pu->locbif_pu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

int
lbmain_close()
{
    mark_window_closed("LOCBIF");
    if(Lbmain_locbif_pu) {
	xv_set(Lbmain_locbif_pu->locbif_pu, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(Lbmain_locbif_pu->locbif_pu, XV_SHOW, FALSE, NULL);
    }
}



lbmain_field_manager()
{
  char          *name[MAX_LEN_VARB_NAME];
  int	        i, offset=0, n_varb, n_param, n_total_sel;
  static int	num_rows = 4;

  if (!Lbmain_locbif_pu) return(-1);



  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL))-1;
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  n_total_sel = n_varb + n_param;

/*
  pm( PUT, "Lb_Control.Lb_Fc", n_varb, NULL);
  pm( PUT, "Lb_Control.Lb_Param_Fc", n_param, NULL );
*/

  pm( INIT, "Lb_Control.Lb_Fc", n_varb, NULL);
  pm( INIT, "Lb_Control.Lb_Param_Fc", n_param, NULL );

  lb_reset();			/* above could be in this procedure */

  if(Lbmain_locbif_pu->lbparamsel)
     {
      xv_destroy(Lbmain_locbif_pu->lbparamsel);
      release_locbif();
     }
  allocate_locbif();

  Lbmain_locbif_pu->lbparamsel = lbmain_locbif_pu_lbparamsel_create(Lbmain_locbif_pu, Lbmain_locbif_pu->lb_main_cntl, n_total_sel);
  
  for(i=0; i<n_varb; i++)
	{
         pm(GET, "Model.Varb_Names", i, name, NULL);
	 if(offset < num_rows)
            {
	     xv_set(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_STRING, i, name, NULL);
	     offset += 1;
            }
         else
	     xv_set(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_INSERT, i, PANEL_LIST_STRING,i, name, NULL);
	}

  for(i=0; i<n_param; i++)
	{
         pm(GET, "Model.Param_Names", i, name, NULL);
	 if(offset < num_rows)
            {
	     xv_set(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_STRING, n_varb + i, name, NULL);
	     offset += 1;
            }
         else
	     xv_set(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_INSERT, n_varb + i, PANEL_LIST_STRING, n_varb + i, name, NULL);
	}

  if(Lcstate_locstatepu) lbstate_field_manager();
	 
  lbmain_init(); 
  lbmain_panel_refresh();
  lbsolver_init();
  lbdisplay_init();
  lbcont_init();
}


lbmain_init()
{
  strcpy(lbmain_ds.Fname,"init.dat");
  strcpy(lbmain_ds.Dirname,"./");
  strcpy(lbmain_ds.Actname," ");
  lbmain_ds.Line = 0;
  lbmain_ds.Locbif_Mode = LB_EQUIL;
}


int
lbmain_panel_refresh()
{
  int	  i, n_varb, n_param;
  char    strng[20];

  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

  xv_set( Lbmain_locbif_pu->lbfilename, PANEL_VALUE, lbmain_ds.Fname, NULL);
  xv_set( Lbmain_locbif_pu->lbdirect, PANEL_VALUE, lbmain_ds.Dirname, NULL);
  xv_set( Lbmain_locbif_pu->lbactvarb, PANEL_VALUE, lbmain_ds.Actname, NULL);
  xv_set( Lbmain_locbif_pu->lbmode, PANEL_VALUE, lbmain_ds.Line, NULL);
  
  xv_set(Lbmain_locbif_pu->lbparamsel, XV_SHOW, FALSE, NULL);
  for(i=0; i<n_varb+n_param; i++)
     {
      xv_set(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_SELECT, i, FALSE, NULL);  
      if(lbmain_ds.active[i]) 
	xv_set(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_SELECT, i, TRUE, NULL);  
     }
  xv_set(Lbmain_locbif_pu->lbparamsel, XV_SHOW, TRUE, NULL);
}



int
lbmain_data_refresh()
{ 
/*  char          *name[MAX_LEN_VARB_NAME]; */
  char          name[MAX_LEN_VARB_NAME];
  int		i, index, n_varb, n_param; /* atoi(); */
/*  double	atof(); */

  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

  strcpy( lbmain_ds.Fname, ((char *)  xv_get( Lbmain_locbif_pu->lbfilename, PANEL_VALUE, NULL)) );
  strcpy( lbmain_ds.Dirname, ((char *)  xv_get( Lbmain_locbif_pu->lbdirect, PANEL_VALUE, NULL)) );
  strcpy( lbmain_ds.Actname, ((char *)  xv_get( Lbmain_locbif_pu->lbactvarb, PANEL_VALUE, NULL)) ); 
  lbmain_ds.Line = (int) xv_get( Lbmain_locbif_pu->lbmode, PANEL_VALUE, NULL);

  index = (int) xv_get( Lbmain_locbif_pu->lbbifmode, PANEL_VALUE, NULL);
  if(index==0)
    lbmain_ds.Locbif_Mode = LB_EQUIL;
  else if (index ==1)
    lbmain_ds.Locbif_Mode = LB_FPTS;
  else if (index ==2)
    lbmain_ds.Locbif_Mode = LB_PERIODIC_AUTO;
  else if (index ==3)
    lbmain_ds.Locbif_Mode = LB_PERIODIC_NAUTO;
  else
    lbmain_ds.Locbif_Mode = LB_EQUIL;         /* temp:  change to error message panel (mrm) */

  xv_set(Lbmain_locbif_pu->lbparamsel, XV_SHOW, FALSE, NULL);
  strcpy(lbmain_ds.Actname," "); 
  for(i=0; i<n_varb+n_param; i++)
     {
      lbmain_ds.active[i] = FALSE;
      if( xv_get(Lbmain_locbif_pu->lbparamsel, PANEL_LIST_SELECTED, i) )
       {
	lbmain_ds.active[i] = TRUE;
        if(i<n_varb)
           pm(GET, "Model.Varb_Names", i, name, NULL);
        else pm(GET, "Model.Param_Names", i-n_varb, name, NULL);

	strcat(lbmain_ds.Actname," ");
	strcat(lbmain_ds.Actname,name);
       }
     }
  xv_set(Lbmain_locbif_pu->lbparamsel, XV_SHOW, TRUE, NULL);


  lbmain_panel_refresh();
}

/*
* Menu handler for `panelmenu (locbif...)'.
*/
Menu_item
lbmain_handler(item, op)
Menu_item       item;
Menu_generate   op;
{
    int
	locn[4];


    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    lbmain_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Locbif",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		lbmain_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		lbmain_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}

/*  if (op == MENU_NOTIFY)
      lbmain_open(DEFAULT_WIN_CONFIG,0,0,0,0); */

      return item;
}


lboption_build( mode )
int	mode;
{
  

  if(Lbmain_locbif_pu->lbmode)
     xv_destroy(Lbmain_locbif_pu->lbmode);

  Lbmain_locbif_pu->lbmode = lbmain_locbif_pu_lbmode_create(Lbmain_locbif_pu, Lbmain_locbif_pu->lb_main_cntl, mode);

}
