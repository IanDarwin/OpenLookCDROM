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

#include "lbstate_cui.h"
#include "lbstate.h"
#include "../lbmain_cui.h"
#include "../lbmain.h"

#define COL_WIDTH   165
#define START_COL   75 

lcstate_locstatepu_objects             *Lcstate_locstatepu;
extern struct Lbmain_Ds                lbmain_ds;
extern lbmain_locbif_pu_objects        *Lbmain_locbif_pu;
struct Lbstate_Ds                      lbstate_ds;

lbstate_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;
  Menu_item    lbstate_batch_handler();
  int	       lbstate_field_manager();

  if (!Lcstate_locstatepu)
    {
      Lcstate_locstatepu = lcstate_locstatepu_objects_initialize(NULL, Lbmain_locbif_pu->locbif_pu);
      register_win_and_type("Lbstate",Lcstate_locstatepu->locstatepu,POPUP_WINDOW);
      register_window("Lbstate",lbstate_batch_handler,lbstate_field_manager);
      lbstate_init();
      lbstate_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Lcstate_locstatepu->locstatepu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Lcstate_locstatepu->locstatepu,rect);
      free(rect);
    }
  mark_window_open("Lbstate");
  xv_set(Lcstate_locstatepu->locstatepu, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Lcstate_locstatepu->locstatepu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

int
lbstate_close()
{
    mark_window_closed("Lbstate");
    if(Lcstate_locstatepu) {
	xv_set(Lcstate_locstatepu->locstatepu, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(Lcstate_locstatepu->locstatepu, XV_SHOW, FALSE, NULL);	
    }
}	


lbstate_field_manager()
{
  int	x = START_COL, status;

  if (!Lcstate_locstatepu) return(-1);

  if(lbmain_ds.Locbif_Mode == LB_EQUIL)
     xv_set(Lcstate_locstatepu->lcsel, PANEL_CHOICE_STRING, 3, "Eigenvalues", NULL);
  else if((lbmain_ds.Locbif_Mode == LB_FPTS)||(lbmain_ds.Locbif_Mode == LB_PERIODIC_AUTO) ||(lbmain_ds.Locbif_Mode == LB_PERIODIC_NAUTO))
     xv_set(Lcstate_locstatepu->lcsel, PANEL_CHOICE_STRING, 3, "Floquet Mult", NULL);

  lbc_destroy();

  status = lbc_state_build( &x );
  status = lbc_param_build( &x );
  status = lbc_func_build( &x );
  status = lbc_char_build( &x );

  window_fit(Lcstate_locstatepu->lccntl);
  window_fit(Lcstate_locstatepu->locstatepu);
  lbstate_panel_refresh();
}


lbc_destroy()
{
  int	i;

  if( lbstate_ds.Num_State_Fields != 0 )
    {
     for(i=0; i<lbstate_ds.Num_State_Fields; i++)
       xv_destroy(Lcstate_locstatepu->lcstate[i]);
     cfree(Lcstate_locstatepu->lcstate);
     lbstate_ds.Num_State_Fields = 0;
    }

  if( lbstate_ds.Num_Param_Fields != 0 )
    {
     for(i=0; i<lbstate_ds.Num_Param_Fields; i++)
       xv_destroy(Lcstate_locstatepu->lcparm[i]);
     cfree(Lcstate_locstatepu->lcparm);
     lbstate_ds.Num_Param_Fields = 0;
    }

  if( lbstate_ds.Num_Func_Fields != 0 )
    {
     for(i=0; i<lbstate_ds.Num_Func_Fields; i++)
       xv_destroy(Lcstate_locstatepu->lcfunc[i]);
     cfree(Lcstate_locstatepu->lcfunc);
     lbstate_ds.Num_Func_Fields = 0;
    }

  if( lbstate_ds.Num_Char_Fields != 0 )
    {
     for(i=0; i<lbstate_ds.Num_Char_Fields; i++)
       xv_destroy(Lcstate_locstatepu->lccharact[i]);
     cfree(Lcstate_locstatepu->lccharact);
     lbstate_ds.Num_Char_Fields = 0;
    }
}



lbc_state_build( x )
int	*x;
{
  int     i;
/*  char    *name[MAX_LEN_VARB_NAME], *label[10]; */
  char    name[MAX_LEN_VARB_NAME], label[10];
/*
  int     n_varb = *((int *) pm(GET, Traj_Ds_Object, Varb_Dim, NULL)) - 1;
*/
  int     n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;


  if(n_varb == 0 || lbstate_ds.State_On == FALSE) return(0);

  Lcstate_locstatepu->lcstate = (Xv_opaque *) calloc(n_varb, sizeof(Xv_opaque));
  lbstate_ds.Num_State_Fields = n_varb;

  for(i=0; i<n_varb; i++)
     {
/*
      pm(GET, Traj_Ds_Object, Varb_Names, i, name, NULL);
*/
      pm(GET, "Model.Varb_Names", i, name, NULL);
      strncpy(label,name,10);
      Lcstate_locstatepu->lcstate[i] = 
	 lcstate_locstatepu_lcstate_create(Lcstate_locstatepu, Lcstate_locstatepu->lccntl, *x, i*18+120, label);
     }

  *x += COL_WIDTH;

  return(0);
}



lbc_param_build( x )
int	*x;
{
  int     i;
/*  char    *name[MAX_LEN_VARB_NAME], *label[10]; */
  char    name[MAX_LEN_VARB_NAME], label[10];

/* int     n_param = *((int *) pm(GET, Traj_Ds_Object, Param_Dim, NULL)); */

  int     n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  if(n_param == 0 || lbstate_ds.Param_On == FALSE) return(0);

  Lcstate_locstatepu->lcparm = (Xv_opaque *) calloc(n_param, sizeof(Xv_opaque));
  lbstate_ds.Num_Param_Fields = n_param;

  for(i=0; i<n_param; i++)
     {
/*    pm(GET, Traj_Ds_Object, Param_Names, i, name, NULL); */
      pm(GET, "Model.Param_Names", i, name, NULL);
      strncpy(label,name,10);
      Lcstate_locstatepu->lcparm[i] = 
	 lcstate_locstatepu_lcparm_create(Lcstate_locstatepu, Lcstate_locstatepu->lccntl, *x, i*18+120, label);
     }

  *x += COL_WIDTH;

  return(0);
}

lbc_func_build( x )
int	*x;
{
  int     i;
/*  char    *name[MAX_LEN_VARB_NAME], *label[10]; */
  char    name[MAX_LEN_VARB_NAME], label[10];
/*  int     n_func = *((int *) pm(GET, Traj_Ds_Object, Function_Dim, NULL)); */
  int     n_func = *((int *) pm( GET, "Model.Funct_Dim", NULL));

  if(n_func == 0 || lbstate_ds.Func_On == FALSE) return(0);

  Lcstate_locstatepu->lcfunc = (Xv_opaque *) calloc(n_func, sizeof(Xv_opaque));
  lbstate_ds.Num_Func_Fields = n_func;

  for(i=0; i<n_func; i++)
     {
/*    pm(GET, Traj_Ds_Object, Function_Names, i, name, NULL);   */
      pm(GET, "Model.Funct_Names", i, name, NULL);
      strncpy(label,name,10);
      Lcstate_locstatepu->lcfunc[i] = 
	 lcstate_locstatepu_lcfunc_create(Lcstate_locstatepu, Lcstate_locstatepu->lccntl, *x, i*18+120, label);
     }

  *x += COL_WIDTH;

  return(0);
}

lbc_char_build( x )
int	*x;
{
  int     i, offset=30;		/* offset required 'cause this col has no label */

/*  int     n_varb = *((int *) pm(GET, Traj_Ds_Object, Varb_Dim, NULL)) - 1;  */
  int     n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;

  if(n_varb == 0 || lbstate_ds.Char_On == FALSE) return(0);

  Lcstate_locstatepu->lccharact = (Xv_opaque *) calloc(n_varb, sizeof(Xv_opaque));
  lbstate_ds.Num_Char_Fields = n_varb;

  for(i=0; i<n_varb; i++)
     {
      Lcstate_locstatepu->lccharact[i] = 
	 lcstate_locstatepu_lccharact_create(Lcstate_locstatepu, Lcstate_locstatepu->lccntl, *x-offset, i*18+120);
     }

  return(0);
}



lbstate_init()
{
  lbstate_ds.State_On = FALSE;
  lbstate_ds.Param_On = FALSE;
  lbstate_ds.Func_On  = FALSE;
  lbstate_ds.Char_On  = FALSE;

  lbstate_ds.Num_State_Fields = 0;
}


extern struct cmn_real{ double rvc[51];                 /* these common blocks MUST be aligned with */
			double rpc[40];                 /* their locbif counterpart common blocks!  */
			double rlim[4]; };

extern struct cmn_real real_ ;

extern struct cmn_mesblk_{ char textcm[60];};

extern struct cmn_mesblk_ mesblk_ ;


int
  lbstate_panel_refresh()
{
  int	i, j, offset=0, index, prtswitch;
  char  strng[40], message[60];
  char  rl[12], imag[12];
/*
  int   n_varb = *((int *) pm(GET, Traj_Ds_Object, Varb_Dim, NULL)) - 1;
  int   n_param = *((int *) pm(GET, Traj_Ds_Object, Param_Dim, NULL));
  int   n_func = *((int *) pm(GET, Traj_Ds_Object, Function_Dim, NULL));
*/

  int   n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1 ;
  int   n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  int   n_func = *((int *) pm( GET, "Model.Funct_Dim", NULL));


  strncpy( message, mesblk_.textcm, 58);
  fprintf(stderr,"%s \n",message);

  if (!Lcstate_locstatepu) return(-1);

  if(lbstate_ds.State_On)
      for(i=offset; i<n_varb; i++)
        {
         sprintf(strng, " %12.5lg", real_.rvc[i] );
         xv_set(Lcstate_locstatepu->lcstate[i], PANEL_VALUE, strng, NULL);
        }

  offset = n_varb;
  if(lbstate_ds.Param_On)
      for(i=offset; i<offset+n_param; i++)
        {
         sprintf(strng, " %12.5lg", real_.rvc[i] );
         xv_set(Lcstate_locstatepu->lcparm[i-offset], PANEL_VALUE, strng, NULL);
        }

  offset += n_param;
  if(lbstate_ds.Func_On)
      for(i=offset; i<offset+n_func; i++)
        {
         sprintf(strng, " %12.5lg", real_.rvc[i] );
         xv_set(Lcstate_locstatepu->lcfunc[i-offset], PANEL_VALUE, strng, NULL);
        }

  offset += n_func+1;
  if(lbstate_ds.Char_On)
      for(i=0; i<n_varb; i++)
        {
         sprintf(rl, "%-8.3g", real_.rvc[i+offset]);
         sprintf(imag, "%-8.3g", real_.rvc[i+offset+n_varb]);
         sprintf(strng,"(%10s",rl);
	 index = (int) strspn(imag,"0123456789+-E.");
	 if(lbmain_ds.Locbif_Mode == LB_EQUIL)
	    {
	     strcat(strng," + ");
	     strncat(strng,imag,index+1);
	     strcat(strng," i )");
            }
         else
	    {
	     strcat(strng," , ");
	     strncat(strng,imag,index+1);
	     strcat(strng," )");
	    }
         xv_set(Lcstate_locstatepu->lccharact[i], PANEL_VALUE, strng, NULL); 
        }

  xv_set(Lcstate_locstatepu->lcstatus, PANEL_VALUE, message, NULL);
}


update_mess_on_panel()
{
  fprintf(stderr,"%s \n",mesblk_.textcm);

  if (!Lcstate_locstatepu) return(-1);
  xv_set(Lcstate_locstatepu->lcstatus, PANEL_VALUE, mesblk_.textcm, NULL);

  return(NO_ERROR);
}




int
lbstate_data_refresh()
{ 
}


/*
* Panel open routine
*/
Menu_item
lbstate_batch_handler(item, op)
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
	lbstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else {
	pm(GET_LIST,"Win.Locn.Lbstate",0,3,locn,NULL);
	if ((locn[0] != NO_LOCATION))
	  lbstate_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	else
	  lbstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      }
    }
/*  if (op == MENU_NOTIFY)
      lbstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);*/

      return item;
}
