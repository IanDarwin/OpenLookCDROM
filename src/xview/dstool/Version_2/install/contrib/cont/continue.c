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
#include <xview/xview.h>
#include <xview/panel.h>

#include <ui_init.h>
#include <pm.h>
#include <memory.h>
#include <constants.h>
#include "continue_cui.h"
#include "continue_def.h"

struct	Cont_Cntl_Ds 		cont_ds;      
continue_cont_pu_objects        *continue_cont_pu=NULL;

int n_varb=0, n_param=0,n_total_sel=0;


cont_open(use_default,left,top,width,height)

int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  extern void cont_install();

  if (!continue_cont_pu)
    {
      continue_cont_pu = continue_cont_pu_objects_initialize(NULL, NULL);
      register_win_and_type("Continuation",continue_cont_pu->cont_pu,BASE_WINDOW);
      cont_install();
      cont_init();
      cont_field_manager();
 }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(continue_cont_pu->cont_pu,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(continue_cont_pu->cont_pu,rect);
      free(rect);
    }


  mark_window_open("Continuation");
  xv_set(continue_cont_pu->cont_pu, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}



/*
* Menu handler for `panelmenu (Continuation...)'.
*/
Menu_item
cont_handler(item, op)
Menu_item       item;
Menu_generate   op;
{

    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    cont_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Continuation",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		cont_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		cont_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}

 /*   if (op == MENU_NOTIFY)
      cont_open(DEFAULT_WIN_CONFIG,0,0,0,0); */
    return item;
}

int
cont_close()
{
    mark_window_closed("Continuation");
    if(continue_cont_pu) {
	xv_set(continue_cont_pu->cont_pu, XV_SHOW, FALSE, NULL);
    }
}



cont_field_manager()
{
	static int	first = TRUE;
	int		i, *ivector();
	char 		*name[MAX_LEN_VARB_NAME];
	extern		void cont_reset();
	  
	if (!continue_cont_pu) return(-1);
	
	cont_reset();

        if(!first)
	  {
	   if(n_param>0) free_ivector(cont_ds.Active_Param,0,n_param-1);
	   if(continue_cont_pu->contpara)  xv_destroy(continue_cont_pu->contpara);
	   if(continue_cont_pu->augparam)  xv_destroy(continue_cont_pu->augparam);
          }
        else
	   first = FALSE;

	n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
	n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
	n_total_sel = n_varb-1+Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

	pm( INIT, "Cont.Fc", n_varb, NULL);
	pm( INIT, "Cont.Param_Fc", n_param, NULL);

	if(n_param <= 0) return(-2);
	
	continue_cont_pu->contpara = contpara_create(continue_cont_pu, continue_cont_pu->cont_cntl, n_total_sel);
	continue_cont_pu->augparam = augparam_create(continue_cont_pu, continue_cont_pu->cont_cntl);
	xv_set(continue_cont_pu->contpara, PANEL_VALUE, 0, NULL);
	pm( PUT, "Cont.Param", n_varb, NULL);
	for (i=0; i<n_varb-1; i++)					/* leave time out! */
	    {
	     pm(GET, "Model.Varb_Names", i, name, NULL);
	     xv_set(continue_cont_pu->contpara, PANEL_CHOICE_STRING,i, name, NULL);
	    }

        if (n_param < Cont_Sel[Cont_Cur_Choice].Num_Req_Param)
	   return(-2);

	cont_ds.Active_Param = ivector(0,n_param-1);
	for(i=0; i<n_param; i++)
	    {
	     pm(GET, "Model.Param_Names", i, name, NULL);
	     xv_set(continue_cont_pu->augparam,
		    PANEL_LIST_INSERT, i,
		    PANEL_LIST_STRING,i, name, NULL);
	    }

	for(i=0; i<Cont_Sel[Cont_Cur_Choice].Num_Req_Param; i++)
	    cont_ds.Active_Param[i] = TRUE;
	for(i=Cont_Sel[Cont_Cur_Choice].Num_Req_Param; i<n_param; i++)
	    cont_ds.Active_Param[i] = FALSE;

	cont_panel_refresh();
}


int
build_sel_par()
{
	int	i, offset = 0;
	char 	*name[MAX_LEN_VARB_NAME];
	
        for(i=n_varb-1; i<n_total_sel; i++)
	   xv_set(continue_cont_pu->contpara, PANEL_CHOICE_STRING, i, " ", NULL);
        for(i=0; i<n_param; i++)
	     if(cont_ds.Active_Param[i])
		{
	         pm(GET, "Model.Param_Names", i, name, NULL);
	         xv_set(continue_cont_pu->contpara, PANEL_CHOICE_STRING, n_varb-1+offset, name, NULL);
		 ++offset;
                }
        for(i=n_param-1; i>=0; i--)
	    xv_set(continue_cont_pu->augparam,PANEL_LIST_SELECT,i,cont_ds.Active_Param[i],NULL);
}
	

cont_init()
{
  cont_ds.Check_Switch = FALSE;
  cont_ds.Cont_Hide_Settings = TRUE;
  cont_ds.Debug_Level = 0;
  cont_ds.Continue_Mem_Ptr = (memory) pm(GET, "Memory.Cont", NULL);
  
  pm( CLEAR, "Cont.Stpsize", "Cont.Iters", "Cont_Control", 
     "Cont.Direction", "Cont.Param", "Cont.Vary_Switch",
     "Cont.Jac_Update", "Cont.Abserr", "Cont.Relerr",
     "Cont.Maxstp", "Cont.Minstp", "Cont.Target", 
     "Cont.Search", "Cont.Plot_Type", NULL); 
  pm(PUT, "Cont.Stpsize", 0.01, 
     PUT, "Cont.Mode", 0, 
     PUT, "Cont.Iters", 500,
     PUT, "Cont.Direction", FORWARD, 
     PUT, "Cont.Param", 0, 
     PUT, "Cont.Vary_Switch", 0,
     PUT, "Cont.Jac_Update", 0, 
     PUT, "Cont.Abserr", 1.0e-5,
     PUT, "Cont.Relerr",1.0e-5, 
     PUT, "Cont.Maxstp", 0.1, 
     PUT, "Cont.Minstp", 0.001, 
     PUT, "Cont.Target", 0.0, 
     PUT, "Cont.Search", FALSE, 
     PUT, "Cont.Plot_Type", 7,  
     NULL);
}


int
cont_panel_refresh()
{
	int	n_varb, n_param, offset=0;
 	char	strng[20], *name[MAX_LEN_VARB_NAME];

	if(!continue_cont_pu) return(-1); 

	xv_set( continue_cont_pu->mode, PANEL_VALUE, *((int *)pm(GET, "Cont.Mode", NULL)) , NULL);
	xv_set( continue_cont_pu->parafix, PANEL_VALUE, *((int *)pm(GET, "Cont.Vary_Switch", NULL)), NULL);
	xv_set( continue_cont_pu->jacupdate, PANEL_VALUE, *((int *)pm(GET, "Cont.Jac_Update", NULL)),NULL);
	xv_set( continue_cont_pu->cview, PANEL_VALUE, *((int *)pm(GET, "Cont.Plot_Type", NULL)),NULL); 
	sprintf(strng, "%d", *( (int *) pm( GET, "Cont.Iters", NULL)) );
  	xv_set(  continue_cont_pu->iters,PANEL_VALUE, strng, NULL); 
	sprintf(strng, "%8.3lg", *( (double *) pm( GET, "Cont.Stpsize", NULL)) );
  	xv_set(  continue_cont_pu->htan,PANEL_VALUE, strng, NULL); 
	sprintf(strng, "%8.3lg", *( (double *) pm( GET, "Cont.Abserr", NULL)) );
  	xv_set(  continue_cont_pu->abserr,PANEL_VALUE, strng, NULL); 
	sprintf(strng, "%8.3lg", *( (double *) pm( GET, "Cont.Relerr", NULL)) );
  	xv_set(  continue_cont_pu->relerr,PANEL_VALUE, strng, NULL); 
	sprintf(strng, "%8.3lg", *( (double *) pm( GET, "Cont.Maxstp", NULL)) );
  	xv_set(  continue_cont_pu->maxstp,PANEL_VALUE, strng, NULL); 
	sprintf(strng, "%8.3lg", *( (double *) pm( GET, "Cont.Minstp", NULL)) );
  	xv_set(  continue_cont_pu->minstp,PANEL_VALUE, strng, NULL); 
	sprintf(strng, "%8d", cont_ds.Debug_Level  );  
  	xv_set(  continue_cont_pu->debuglvl,PANEL_VALUE, strng, NULL); 
	xv_set( continue_cont_pu->contpara, PANEL_VALUE, *((int *) pm( GET, "Cont.Param", NULL)), NULL); 
	build_sel_par();
}


int
cont_data_refresh()
{
 	char	strng[20];
	int	i, value; /* atoi()*/
/*	double	atof(); */

	if(!continue_cont_pu) return(-1); 

	value = (int) xv_get( continue_cont_pu->check, PANEL_VALUE, NULL);
	cont_ds.Check_Switch = (value==0) ? FALSE:TRUE;
	value = (int) xv_get( continue_cont_pu->settings, PANEL_VALUE, NULL);
	value = atoi((char *) xv_get(continue_cont_pu->debuglvl,PANEL_VALUE, NULL));
	cont_ds.Debug_Level = value; 

	pm( PUT, "Cont.Mode", (int) xv_get( continue_cont_pu->mode, PANEL_VALUE, NULL), NULL);           
	Cont_Cur_Choice = (int) xv_get( continue_cont_pu->mode, PANEL_VALUE, NULL);
	pm( PUT, "Cont.Jac_Update", (int) xv_get( continue_cont_pu->jacupdate, PANEL_VALUE, NULL), NULL);
	pm( PUT, "Cont.Plot_Type", (int) xv_get( continue_cont_pu->cview, PANEL_VALUE, NULL), NULL); 

	pm( PUT, "Cont.Iters", atoi((char *) xv_get(continue_cont_pu->iters,PANEL_VALUE, NULL)) , NULL);
	pm( PUT, "Cont.Stpsize", atof((char *) xv_get(continue_cont_pu->htan,PANEL_VALUE, NULL)), NULL);
	pm( PUT, "Cont.Abserr", atof((char *) xv_get(continue_cont_pu->abserr,PANEL_VALUE, NULL)), NULL);
	pm( PUT, "Cont.Relerr", atof((char *) xv_get(continue_cont_pu->relerr,PANEL_VALUE, NULL)), NULL);
	pm( PUT, "Cont.Maxstp", atof((char *) xv_get(continue_cont_pu->maxstp,PANEL_VALUE, NULL)), NULL);
	pm( PUT, "Cont.Minstp", atof((char *) xv_get(continue_cont_pu->minstp,PANEL_VALUE, NULL)), NULL);

	value = 0;
	xv_set(continue_cont_pu->augparam, XV_SHOW, FALSE, NULL);
	for(i=n_param-1; i>=0; i--)
	   if( xv_get(continue_cont_pu->augparam, PANEL_LIST_SELECTED, i) ) ++value;
        if( value > Cont_Sel[Cont_Cur_Choice].Num_Req_Param )
	    error_notice( continue_cont_pu->cont_cntl, "Too many parameters selected. Delete a selection prior to next choice.");
        else
	   {
	    for(i=n_param-1; i>=0; i--)
	      {
	       cont_ds.Active_Param[i]= FALSE;
	       if( (int) xv_get(continue_cont_pu->augparam, PANEL_LIST_SELECTED, i) ) cont_ds.Active_Param[i]= TRUE;
              }
           }
	xv_set(continue_cont_pu->augparam, XV_SHOW, TRUE, NULL);

	pm( PUT, "Cont.Param", (int) xv_get( continue_cont_pu->contpara, PANEL_VALUE, NULL), NULL);
	pm( PUT, "Cont.Vary_Switch", (int) xv_get( continue_cont_pu->parafix, PANEL_VALUE, NULL), NULL);

	cont_panel_refresh();
}


