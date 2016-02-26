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

#include "../continue_cui.h"
#include "../continue.h"
#include "contstate_cui.h"
#include "contstate.h"

#define COL_WIDTH   165
#define START_COL   75 

cntstate_objects                       *Cntstate;
struct Cntstate_Ds                     cntstate_ds;
extern continue_cont_pu_objects        *continue_cont_pu;
extern struct  Cont_Cntl_Ds            cont_ds;

cntstate_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;
  Menu_item    cntstate_batch_handler();
  int	       cntstate_field_manager();
  

  if (!Cntstate)
    {
      Cntstate = cntstate_objects_initialize(NULL, continue_cont_pu->cont_pu);
      register_win_and_type("Contstate",Cntstate->cntstatepu,POPUP_WINDOW);
      register_window("Contstate",cntstate_batch_handler,cntstate_field_manager);
      cntstate_init();
      cntstate_field_manager();
    }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Cntstate->cntstatepu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Cntstate->cntstatepu,rect);
      free(rect);
    }
  mark_window_open("Contstate");
  xv_set(Cntstate->cntstatepu, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Cntstate->cntstatepu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

	
int
contstate_close()
{
    mark_window_closed("Contstate");
    if(Cntstate) {
	xv_set(Cntstate->cntstatepu, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(Cntstate->cntstatepu, XV_SHOW, FALSE, NULL);	
    }
}	


cntstate_field_manager()
{
  int	x = START_COL, status;

  if (!Cntstate) return(-1);

  cntstate_destroy();

  status = cnt_state_build( &x );
  status = cnt_param_build( &x );
  status = cnt_func_build( &x );
  status = cnt_char_build( &x );

  window_fit(Cntstate->cntl);
  window_fit(Cntstate->cntstatepu);
  cntstate_panel_refresh();
}


cntstate_destroy()
{
  int	i;

  if( cntstate_ds.Num_State_Fields != 0 )
    {
     for(i=0; i<cntstate_ds.Num_State_Fields; i++)
       xv_destroy(Cntstate->state[i]);
     cfree(Cntstate->state);
     cntstate_ds.Num_State_Fields = 0;
    }

  if( cntstate_ds.Num_Param_Fields != 0 )
    {
     for(i=0; i<cntstate_ds.Num_Param_Fields; i++)
       xv_destroy(Cntstate->parm[i]);
     cfree(Cntstate->parm);
     cntstate_ds.Num_Param_Fields = 0;
    }

  if( cntstate_ds.Num_Func_Fields != 0 )
    {
     for(i=0; i<cntstate_ds.Num_Func_Fields; i++)
       xv_destroy(Cntstate->func[i]);
     cfree(Cntstate->func);
     cntstate_ds.Num_Func_Fields = 0;
    }

  if( cntstate_ds.Num_Char_Fields != 0 )
    {
     for(i=0; i<cntstate_ds.Num_Char_Fields; i++)
       xv_destroy(Cntstate->charact[i]);
     cfree(Cntstate->charact);
     cntstate_ds.Num_Char_Fields = 0;
    }
}



cnt_state_build( x )
int	*x;
{
  int     i;
/*  char    *name[MAX_LEN_VARB_NAME], *label[10]; */
  char    name[MAX_LEN_VARB_NAME], label[10];
  int     n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;

  if(n_varb == 0 || cntstate_ds.State_On == FALSE) return(0);

  Cntstate->state = (Xv_opaque *) calloc(n_varb, sizeof(Xv_opaque));
  cntstate_ds.Num_State_Fields = n_varb;

  for(i=0; i<n_varb; i++)
     {
      pm(GET, "Model.Varb_Names", i, name, NULL);
      strncpy(label,name,10);
      Cntstate->state[i] = 
	 cntstate_state_create(Cntstate, Cntstate->cntl, *x, i*18+120, label);
     }

  *x += COL_WIDTH;

  return(0);
}


cnt_param_build( x )
int	*x;
{
  int     i;
/*  char    *name[MAX_LEN_VARB_NAME], *label[10]; */
  char    name[MAX_LEN_VARB_NAME], label[10];
  int     n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

  if(n_param == 0 || cntstate_ds.Param_On == FALSE) return(0);

  Cntstate->parm = (Xv_opaque *) calloc(n_param, sizeof(Xv_opaque));
  cntstate_ds.Num_Param_Fields = n_param;

  for(i=0; i<n_param; i++)
     {
      pm(GET, "Model.Param_Names", i, name, NULL);
      strncpy(label,name,10);
      Cntstate->parm[i] = 
	 cntstate_parm_create(Cntstate, Cntstate->cntl, *x, i*18+120, label);
     }

  *x += COL_WIDTH;

  return(0);
}

cnt_func_build( x )
int	*x;
{
  int     i;
/*  char    *name[MAX_LEN_VARB_NAME], *label[10]; */
  char    name[MAX_LEN_VARB_NAME], label[10];
  int     n_func = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  if(n_func == 0 || cntstate_ds.Func_On == FALSE) return(0);

  Cntstate->func = (Xv_opaque *) calloc(n_func, sizeof(Xv_opaque));
  cntstate_ds.Num_Func_Fields = n_func;

  for(i=0; i<n_func; i++)
     {
      pm(GET, "Model.Funct_Names", i, name, NULL);
      strncpy(label,name,10);
      Cntstate->func[i] = 
	 cntstate_func_create(Cntstate, Cntstate->cntl, *x, i*18+120, label);
     }

  *x += COL_WIDTH;

  return(0);
}

cnt_char_build( x )
int	*x;
{
  int     i, offset=30;		/* offset required 'cause this col has no label */

  int     n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;

  if(n_varb == 0 || cntstate_ds.Char_On == FALSE) return(0);

  Cntstate->charact = (Xv_opaque *) calloc(n_varb, sizeof(Xv_opaque));
  cntstate_ds.Num_Char_Fields = n_varb;

  for(i=0; i<n_varb; i++)
     {
      Cntstate->charact[i] = 
	 cntstate_charact_create(Cntstate, Cntstate->cntl, *x-offset, i*18+120);
     }

  return(0);
}



cntstate_init()
{
  cntstate_ds.State_On = FALSE;
  cntstate_ds.Param_On = FALSE;
  cntstate_ds.Func_On  = FALSE;
  cntstate_ds.Char_On  = FALSE;

  cntstate_ds.Num_State_Fields = 0;
}



int
  cntstate_panel_refresh()
{
  int		i, j, *iwork, *ivector(), ierror, index;
  char  	strng[40];
  char  	rl[12], imag[12];
  double	*dvector(), *state, *parameters, *funct_val, **jac1, **jac2, *wr, *wi, *dwork, **dmatrix();

  int   n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
  int   n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  int   n_func = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  if (!Cntstate) return(-1);

  if ( ! (state = dvector(0,n_varb)) ) return(-1);
  if ( ! (parameters = dvector(0,n_param))  ) return(-1);

  pm( GET_LIST, "Cont.Fc", 0, n_varb-1, state, NULL);
  pm( GET_LIST, "Cont.Param_Fc", 0, n_param-1, parameters, NULL);

  if(cntstate_ds.State_On)
      for(i=0; i<n_varb; i++)
        {
         sprintf(strng, " %12.5lg", state[i] );
         xv_set(Cntstate->state[i], PANEL_VALUE, strng, NULL);
        }

  if(cntstate_ds.Param_On)
      for(i=0; i<n_param; i++)
        {
         sprintf(strng, " %12.5lg", parameters[i] );
         xv_set(Cntstate->parm[i], PANEL_VALUE, strng, NULL);
        }

  if(cntstate_ds.Func_On)
     {
      if ( ! (funct_val = dvector(0,n_func))  ) return(-1);
      get_ds_func(funct_val, state, parameters);           
      for(i=0; i<n_func; i++)
        {
         sprintf(strng, " %12.5lg", funct_val[i] );
         xv_set(Cntstate->func[i], PANEL_VALUE, strng, NULL);
        }
      free_dvector(funct_val,0,n_func);
     }

  if(cntstate_ds.Char_On)
     {
      wr = dvector(0,n_varb);
      wi = dvector(0,n_varb);
      iwork = ivector(0,1000);
      dwork = dvector(0,1000);

      jac1 = dmatrix(0,n_varb+1,0,n_varb+1);
      jac2 = dmatrix(0,n_varb+1,0,n_varb+1);

      get_Dxf( jac2, n_varb, state, parameters );

      for(i=0; i<n_varb; i++)
        for(j=0; j<n_varb; j++)
           jac1[i+1][j+1] = jac2[i][j];
      ierror = rg(n_varb,n_varb,jac1,&wr[0]-1,&wi[0]-1,0,NULL,iwork,dwork);
/*   if(ierror != 0) return(ierror); */

      for(i=0; i<n_varb; i++)
        {
         sprintf(rl, "%-8.4g", wr[i]);
         sprintf(imag, "%-8.4g", wi[i]);
         sprintf(strng,"(%10s",rl);
	 index = (int) strspn(imag,"0123456789-+E.");
	 strcat(strng," + ");
	 strncat(strng,imag,index+1);
	 strcat(strng," i )");
         xv_set(Cntstate->charact[i], PANEL_VALUE, strng, NULL); 
        }
      free_dvector(wr,0,n_varb);
      free_dvector(wi,0,n_varb);
      free_ivector(iwork,0,1000);
      free_dvector(dwork,0,1000);
      free_dmatrix(jac1,0,n_varb+1,0,n_varb+1);
      free_dmatrix(jac2,0,n_varb+1,0,n_varb+1);
     }

/*  xv_set(Cntstate->status, PANEL_VALUE, mesblk_.textcm, NULL); */

  free_dvector(state,0,n_varb);
  free_dvector(parameters,0,n_param);

}



int
cntstate_data_refresh()
{ 
}

/*
 * Used by batch (e.g. load) command to open prop window.
 */
Menu_item
    cntstate_batch_handler(item, op)
Menu_item	
    item;
Menu_generate	
    op;
{
    extern int pm_exists_entry();
    int
	locn[4];
    if (op == MENU_NOTIFY) {
      /* open continuation window if not already done */
      if (!pm_exists_entry("Win.Open_Status.Continuation"))
	cont_open(DEFAULT_WIN_CONFIG,0,0,0,0);

      if (item != (Menu_item) NULL)
	cntstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else {
	pm(GET_LIST,"Win.Locn.Contstate",0,3,locn,NULL);
	if ((locn[0] != NO_LOCATION))
	  cntstate_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	else
	  cntstate_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      }
    }
  return item;
}
