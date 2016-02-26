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
 * oned_go.c
 *
 */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/svrimage.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <gcm.h>
#include <gdd.h>

#include <stdio.h>
#include <pm.h>
#include <constants.h>
#include <twoD.h>
#include <twoD_ip.h>
#include <manifold.h>

void
oned_sketch()
{
  int get_max_twoD(), valid_twoD_id(), *ivector(), (*ds)();
  int nvars, nparams, nfuncs, i, j, pts, iters, n, first_point;
  double *dvector(), *vars, *params, *f, *temp, inc, oldx, oldy;
  Manifold manifold;

  /* allocate memory and load up values */
  get_n_all_types(&nvars, &nparams, &nfuncs);
  manifold.periodic_varb = ivector(0,nvars-1);
  manifold.period_start = dvector(0,nvars-1);
  manifold.period_end = dvector(0,nvars-1);
  manifold.type = *((int *) pm( GET, "Manifold.Type", NULL ));
  pm( GET_LIST, "Manifold.Periodic_Varb", 0, nvars-2, 
     manifold.periodic_varb, NULL);
  pm( GET_LIST, "Manifold.Period_Start", 0, nvars-2, 
     manifold.period_start, NULL);
  pm( GET_LIST, "Manifold.Period_End", 0, nvars-2, 
     manifold.period_end, NULL);
  vars = dvector(0, nvars-1);
  f = dvector(0, nvars-1);
  temp = dvector(0, nvars-1);
  params = dvector(0, nparams-1);
  pm(GET_LIST, "Selected.Varb_Ic", 0, nvars-1, vars, NULL);
  pm(GET_LIST, "Selected.Param_Ic", 0, nparams-1, params, NULL);
  iters = *((int *) pm(GET, "OneD.Iter", NULL));
  ds = (int (*)()) pm(GET, "Model.DS_Def", NULL);

  /* search for view windows which are x vs x */
  for (n=get_max_twoD(), i=0; i<=n; i++)
    if (valid_twoD_id(i) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Index == 0) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Index == 0))
      {
	/* sketch in this window */
	vars[0] = TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min;
	inc = TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max;
	pts = TwoD_Ds[i]->TwoD_Win_Ds->Px_Max - 
	  TwoD_Ds[i]->TwoD_Win_Ds->P_Origin_X;
	inc = (inc-vars[0]) / pts;
	first_point = TRUE;
	while (pts >=0)
	  {
	    /* plot a point */
	    iter_forw( ds, iters, f, vars, params, nvars, 1.0, 
		      temp, &manifold);
	    if (!first_point) 
	      twoD_drawline(i, -SYS_GREEN, oldx, oldy, vars[0], f[0]);
	    else first_point = FALSE;
	    oldx = vars[0];
	    oldy = f[0];
	    pts--;
	    vars[0] += inc;
	  }
	
      }

  /* free memory */
  free_dvector(vars, 0, nvars-1);
  free_dvector(f, 0, nvars-1);
  free_dvector(temp, 0, nvars-1);
  free_dvector(params, 0, nparams-1);
  free_dvector(manifold.period_end, 0, nvars-1);
  free_dvector(manifold.period_start, 0, nvars-1);
  free_dvector(manifold.periodic_varb, 0, nvars-1);
}

void
oned_diagonal()
{
  int i, n = get_max_twoD(), getmax_twoD(), valid_twoD_id();
  double x, y;

  /* search for view windows which are x vs x */
  for (i=0; i<=n; i++)
    if (valid_twoD_id(i) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Index == 0) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Index == 0))
      {
	/* sketch in this window */
	x = TwoD_Ds[i]->TwoD_Win_Ds->Hor_Min;
	y = TwoD_Ds[i]->TwoD_Win_Ds->Hor_Max;
	twoD_drawline(i, -SYS_BLUE, x, x, y, y);
      }
}

void
oned_clear()
{
  int getmax_twoD(), valid_twoD_id();
  int i, n = get_max_twoD();

  /* search for view windows which are x vs x */
  for (i=0; i<=n; i++)
    if (valid_twoD_id(i) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Index == 0) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Index == 0))
      {
	/* refresh the window */
	refresh_win_id(i);
      }
  reset_color();
}

void
oned_backward()
{
  system_mess_proc(1,"oned_backward");
}

void
oned_forward()
{
  oned_go(FORWARD);
}

void
oned_continue()
{
  oned_go(CONTINUE);
}

oned_go(direction)
int direction;
{
  int get_max_twoD(), valid_twoD_id(), *ivector(), get_alt_color();
  int nvars, nparams, nfuncs, i, j, pts, iters, n = get_max_twoD();
  double *vars, *params, *f, *temp, *dvector(), diverging;
  int (*ds)(), color, no_windows = TRUE;
  Manifold manifold;

  /* we have not coded for backward iteration */
  if (direction == BACKWARD) return;

  /* do memory allocations */
  get_n_all_types(&nvars, &nparams, &nfuncs);
  manifold.periodic_varb = ivector(0,nvars-1);
  manifold.period_start = dvector(0,nvars-1);
  manifold.period_end = dvector(0,nvars-1);
  vars = dvector(0, nvars-1);
  f = dvector(0, nvars-1);
  temp = dvector(0, nvars-1);
  params = dvector(0, nparams-1);

  /* load the data */
  manifold.type = *((int *) pm( GET, "Manifold.Type", NULL ));
  pm( GET_LIST, "Manifold.Periodic_Varb", 0, nvars-2, 
     manifold.periodic_varb, NULL);
  pm( GET_LIST, "Manifold.Period_Start", 0, nvars-2, 
     manifold.period_start, NULL);
  pm( GET_LIST, "Manifold.Period_End", 0, nvars-2, 
     manifold.period_end, NULL);
  if (direction != CONTINUE) bump_color();
  iters = *((int *) pm(GET, "OneD.Iter", NULL));
  ds = (int (*)()) pm(GET, "Model.DS_Def", NULL);
  color = get_alt_color();
  diverging = *((double *) pm(GET, "Flow.Diverg_Cutoff", NULL));

  /* search for view windows which are x vs x */
  for (i=0; i<=n; i++)
    if (valid_twoD_id(i) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Hor_Index == 0) &&
	(TwoD_Ds[i]->TwoD_Win_Ds->Active_Ver_Index == 0))
      {
	/* get Xwindow stuff */
	Xv_Window win =  (Xv_window) get_twoD_handle(i,CANVAS_PW_HANDLE);
	Display *display = (Display *)xv_get(win, XV_DISPLAY);
	XID paint_xid = (XID)xv_get(win, XV_XID);
	GC gc = TwoD_Ds[i]->TwoD_Win_Ds->Canvas_Gc;

	no_windows = FALSE;

	if (direction == CONTINUE)
	  {
	    pm(GET_LIST, "Selected.Varb_Fc", 0, nvars-1, vars, NULL);
	    pm(GET_LIST, "Selected.Param_Fc", 0, nparams-1, params, NULL);
	  }
	else
	  {
	    pm(GET_LIST, "Selected.Varb_Ic", 0, nvars-1, vars, NULL);
	    pm(GET_LIST, "Selected.Param_Ic", 0, nparams-1, params, NULL);
	  }
	pts = *((int *) pm(GET, "OneD.Points", NULL));
	if (!finite(vars[0]) || vars[0] > diverging || vars[0] < -diverging)
	  {
	    system_mess_proc(1,
		     "Orbits appear to diverge off to an infinity! Stop!");
	    pts = 0;
	  }
	while (pts > 0)
	  {
	    /* copy f(x) to x */
	    iter_forw( ds, iters, f, vars, params, nvars, 1.0, 
		      temp, &manifold);
	    twoD_prim_drawline(i, display, paint_xid, gc, color,
				 vars[0], vars[0], vars[0], f[0]);
	    twoD_prim_drawline(i, display, paint_xid, gc, color,
			       vars[0], f[0], f[0], f[0]);
	    for (j=0; j<nvars; j++) vars[j]=f[j];
	    pts--;
	    if (!finite(vars[0]) || vars[0] > diverging || 
		vars[0] < -diverging)
	      {
		system_mess_proc(1,
			"Orbits appear to diverge off to an infinity! Stop!");
		pts = 0;
	      }
	  }
      }

  if (no_windows==FALSE)
    {
      /* update selected point window */
      pm(PUT_LIST, "Selected.Varb_Fc", 0, nvars-1, f, NULL);
      pm(PUT_LIST, "Selected.Param_Fc", 0, nparams-1, params, NULL);
    }

  /* free memory */
  free_dvector(vars, 0, nvars-1);
  free_dvector(f, 0, nvars-1);
  free_dvector(temp, 0, nvars-1);
  free_dvector(params, 0, nparams-1);
  free_dvector(manifold.period_end, 0, nvars-1);
  free_dvector(manifold.period_start, 0, nvars-1);
  free_dvector(manifold.periodic_varb, 0, nvars-1);
}


int
twoD_prim_drawline(window_number, display, paint_xid, gc, 
		   color, x1, y1, x2, y2)
int window_number;
Display *display;
XID paint_xid;
GC gc;
int color;
double x1,y1,x2,y2;
{
  int px1, py1, px2, py2;
  unsigned long get_default_foreground(); 

  px1 = rxtopix(x1, TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  py1 = rytopix(y1, TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);
  px2 = rxtopix(x2, TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  py2 = rytopix(y2, TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);
  if (color >= 0)
    XSetForeground(display, gc, TwoD_Ds[window_number]->TwoD_Win_Ds->
		   TwoD_Traj_Colors[(int) TwoD_Ds[window_number]->
				    TwoD_Win_Ds->ColorTable[color]]);
  else
    XSetForeground(display, gc, TwoD_Ds[window_number]->TwoD_Win_Ds->
		   TwoD_Sys_Colors[-color]);
  XDrawLine(display,paint_xid,gc,px1,py1,px2,py2);
  XSetFunction(display, gc, GXcopy);
}


int
twoD_drawline(window_number, color, x1, y1, x2, y2)
int window_number;
int color;
double x1,y1,x2,y2;
{
  Xv_Window win =  (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
  Display *display = (Display *)xv_get(win, XV_DISPLAY);
  XID paint_xid = (XID)xv_get(win, XV_XID);
  GC gc = TwoD_Ds[window_number]->TwoD_Win_Ds->Canvas_Gc;

  twoD_prim_drawline(window_number, display, paint_xid, gc, 
		     color, x1, y1, x2, y2);
}
