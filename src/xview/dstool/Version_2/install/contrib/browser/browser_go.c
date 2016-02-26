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
/*
 * browser_go.c
 *
 */

/* for highlighting */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/svrimage.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <gcm.h>
#include <gdd.h>

#include <constants.h>  
#include <ui_init.h>
#include <twoD.h>
#include <twoD_ip.h>

#include <stdio.h>
#include <pm.h>
#include <memory.h>


/*
 * browser_copy_go()
 * 
 * procedure to copy browser panel point
 * to selected point
 */
void
  browser_copy_go()
{
  int i, n, code;
  
  code = *((int *) pm(GET, "Browser.Set_Code", NULL));

  if (code & 1)
    {
      /* do variables */
      n = *((int *) pm(GET, "Model.Varb_Dim", NULL));
      for (i=0; i<n; i++)
	pm(PUT, "Selected.Varb_Ic", i,
	   *((double *) pm(GET, "Browser.Vars", i, NULL)), NULL);
    }
  if (code & 2)
    {
      /* do parameters */
      n = *((int *) pm(GET, "Model.Param_Dim", NULL));
      for (i=0; i<n; i++)
	pm(PUT, "Selected.Param_Ic", i,
	   *((double *) pm(GET, "Browser.Params", i, NULL)), NULL);
    }
}


void
browser_output_go()
{
  int i, n, format = *((int *) pm(GET, "Defaults.Precision", NULL));
  int code = *((int *) pm(GET, "Browser.Set_Code", NULL));

  if (code & 1)
    {
      /* do variables */
      n = *((int *) pm(GET, "Model.Varb_Dim", NULL));
      for (i=0; i<n; i++)
	fprintf(stdout, "%.*lg ", format, 
		*((double *) pm(GET, "Browser.Vars", i, NULL)));
    }
  if (code & 2)
    {
      /* do parameters */
      n = *((int *) pm(GET, "Model.Param_Dim", NULL));
      for (i=0; i<n; i++)
	fprintf(stdout, "%.*lg ", format, 
		*((double *) pm(GET, "Browser.Params", i, NULL)));
    }
  if (code & 4)
    {
      /* do colors */
      for (i=0; i<3; i++)
	fprintf(stdout, "%ld ", *((int *) pm(GET, "Browser.Color", i, NULL)));
    }
  fprintf(stdout,"\n");
}


/*
 * browser_delete_go()
 *
 * procedure to delete the selected flow
 * from the selected memory object on the browser panel
 *
 */
void
browser_delete_go()
{
  memory m = (memory) pm(GET, "Browser.Memory", NULL);
  memory_delete_flow(m, *((int *) pm(GET, "Browser.Flow_Num", NULL)));
}


/* ****************************************
 * code for highlighting point
 *
 * **************************************** */

static int cross_hairs_drawn = FALSE;

/*
 * browser_highlight_draw()
 *
 * procedure to highlight in the view windows the point
 * which is selected by the browser panel
 *
 */
browser_highlight_draw()
{
  int get_max_twoD(), valid_twoD_id();
  int i, index, n = get_max_twoD(), x, y, xmin, ymin, xmax, ymax;
  int funcs_set = FALSE;
  int nvars, nparams, nfuncs;
  double *vars, *params, *funcs, *dvector();

  if (cross_hairs_drawn) return;

  get_n_all_types(&nvars, &nparams, &nfuncs);
  vars = dvector(0, nvars-1);
  params = dvector(0, nparams-1);
  funcs = dvector(0, nfuncs-1);
  pm(GET_LIST, "Browser.Vars", 0, nvars-1, vars, NULL);
  pm(GET_LIST, "Browser.Params", 0, nparams-1, params, NULL);

  /* loop over valid windows */
  for (i=0; i<= n; i++)
    if (valid_twoD_id(i))
      {
	xmin = TwoD_Ds[i]->TwoD_Win_Ds->P_Origin_X;
	ymin = TwoD_Ds[i]->TwoD_Win_Ds->P_Origin_Y;
	xmax = TwoD_Ds[i]->TwoD_Win_Ds->Px_Max;
	ymax = TwoD_Ds[i]->TwoD_Win_Ds->Py_Max;
	index = TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Index;
	switch ( TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Type )
	  {
	  case PHASE_SPACE_VARB:
	    x = rxtopix(vars[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max,
			xmin,xmax);
	    break;
	  case PARAMETER_VARB:
	    x = rxtopix(params[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max,
			xmin,xmax);
	    break;    
	  case FUNCTION_VARB:
	    if (funcs_set == FALSE) {
	      get_ds_func(funcs, vars, params);
	      funcs_set = TRUE;
	    }
	    x = rxtopix(funcs[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max,
			xmin,xmax);
	    break;
	  default:
	    x = xmin-1;
	  }  
	index = TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Index;
	switch ( TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Type ) 
	  {
	  case PHASE_SPACE_VARB:
	    y = rytopix(vars[index], 
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Max,
			ymin,ymax);
	    break;
	  case PARAMETER_VARB:
	    y = rytopix(params[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Max,
			ymin,ymax);
	    break;
	  case FUNCTION_VARB:
	    if (funcs_set == FALSE) {
	      get_ds_func(funcs, vars, params);
	      funcs_set = TRUE;
	    }
	    y = rytopix(funcs[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Max,
			ymin,ymax);
	    break;
	  default:
	    y = ymin-1;
	  }

	draw_crosshair( (Xv_window) get_twoD_handle(i,CANVAS_PW_HANDLE),
		       x, y, xmin, ymin, xmax, ymax);
      }
  cross_hairs_drawn = TRUE;

  free_dvector(vars, 0, nvars-1);
  free_dvector(params, 0, nparams-1);
  free_dvector(funcs, 0, nfuncs-1);
  return;
}

/*
 * browser_highlight_erase()
 *
 * procedure to un highlight in the view windows the point
 * which is selected by the browser panel
 *
 */
browser_highlight_erase()
{
  int get_max_twoD(), valid_twoD_id();
  int i, index, n= get_max_twoD(), x, y, xmin, ymin, xmax, ymax;
  int funcs_set = FALSE;
  int nvars, nparams, nfuncs;
  double *vars, *params, *funcs, *dvector();

  if (cross_hairs_drawn == FALSE) return;

  get_n_all_types(&nvars, &nparams, &nfuncs);
  vars = dvector(0, nvars-1);
  params = dvector(0, nparams-1);
  funcs = dvector(0, nfuncs-1);
  pm(GET_LIST, "Browser.Vars", 0, nvars-1, vars, NULL);
  pm(GET_LIST, "Browser.Params", 0, nparams-1, params, NULL);

  /* loop over valid windows */
  for (i=0; i<=n; i++)
    if (valid_twoD_id(i))
      {
	xmin = TwoD_Ds[i]->TwoD_Win_Ds->P_Origin_X;
	ymin = TwoD_Ds[i]->TwoD_Win_Ds->P_Origin_Y;
	xmax = TwoD_Ds[i]->TwoD_Win_Ds->Px_Max;
	ymax = TwoD_Ds[i]->TwoD_Win_Ds->Py_Max;
	index = TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Index;
	switch ( TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Type )
	  {
	  case PHASE_SPACE_VARB:
	    x = rxtopix(vars[index], 
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max,
			xmin,xmax);
	    break;
	  case PARAMETER_VARB:
	    x = rxtopix(params[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max,
			xmin,xmax);
	    break;    
	  case FUNCTION_VARB:
	    if (funcs_set == FALSE) {
	      get_ds_func(funcs, vars, params);
	      funcs_set = TRUE;
	    }
	    x = rxtopix(funcs[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max,
			xmin,xmax);
	    break;
	  default:
	    x = xmin-1;
	  }  
	index = TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Index;
	switch ( TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Type ) 
	  {
	  case PHASE_SPACE_VARB:
	    y = rytopix(vars[index], 
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Max,
			ymin,ymax);
	    break;
	  case PARAMETER_VARB:
	    y = rytopix(params[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Max,
			ymin,ymax);
	    break;    
	  case FUNCTION_VARB:
	    if (funcs_set == FALSE) {
	      get_ds_func(funcs, vars, params);
	      funcs_set = TRUE;
	    }
	    y = rytopix(funcs[index],
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Min,
			TwoD_Ds[i]->TwoD_Win_Ds->Vert_Max,
			ymin,ymax);
	    break;
	  default:
	    y = ymin-1;
	  }


	draw_crosshair( (Xv_window) get_twoD_handle(i,CANVAS_PW_HANDLE),
		       x, y, xmin, ymin, xmax, ymax);
      }
  cross_hairs_drawn = FALSE;
  return;
}

int
draw_crosshair(win, x, y, xmin, ymin, xmax, ymax)
Xv_window win;
int x, y, xmin, ymin, xmax, ymax;
{
  Display *display = (Display *)xv_get(win, XV_DISPLAY);
  XID paint_xid = (XID)xv_get(win, XV_XID);
  twoD_win_objects      *ip = (twoD_win_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);
  int window_number = get_twoD_number(ip);
  GC gc = TwoD_Ds[window_number]->TwoD_Win_Ds->Canvas_Gc;
  unsigned long get_default_foreground(); 

  XSetForeground(display, gc, get_default_foreground(window_number));
  XSetFunction(display, gc, GXxor);  /* Set logical func = XOR (double draw = erase) */
  if (x>=xmin && x<=xmax) 
    XDrawLine(display,paint_xid,gc,x,ymin,x,ymax);
  if (y>=ymin && y<=ymax) 
    XDrawLine(display,paint_xid,gc,xmin,y,xmax,y);
  XSetFunction(display, gc, GXcopy);

}
