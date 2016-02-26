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
#include <math.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <gcm.h>
#include <gdd.h>
#include <xview/cms.h>

#include <constants.h>
#include <ui_init.h>
#include "twoD.h"

int
plot_point(window_number,x,y,color)
int	window_number,			/* window index to draw point */
	color;				/* colormap index of plotted point */
double	x, y;				/* absolute coordinates of point to be plotted */

{
	int px,py;					/* pixel coordinates of plotted point */
	static  int		win_last_call = -1;	/* stash window numbers to save time on multiple calls */
	static	Xv_window	paint_window;		/* cash id data to save time on multiple calls to a window */
	static	Display         *dpy;			/*  "  */
	static	Window          xwin;			/*  "  */
	static	GC              gc;			/*  "  */

	paint_window = (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
	dpy = (Display *)xv_get(paint_window, XV_DISPLAY);
	xwin = (Window) xv_get(paint_window, XV_XID);
	gc =  TwoD_Ds[window_number]->TwoD_Win_Ds->Canvas_Gc;

	px = rxtopix( x, TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min, 
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X, 
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
	py = rytopix( y, TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);

	if( px > TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max || 
	    py > TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max ||
	    px < TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X || 
	    py < TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y )	return;

	XSetForeground(dpy,gc,TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[color]);
	XDrawPoint(dpy,xwin,gc,px,py);
	win_last_call = window_number;

}



int
rxtopix(x,x_min,x_max,p_origin_x,px_max)
double	x,	x_min,	x_max;
int	p_origin_x,	px_max;
{

	/*  if (x>x_max | x<x_min ) return(0);   */
	return( (int) floor( p_origin_x + ((x-x_min)/(x_max-x_min))*(px_max-p_origin_x)) );
}

int
rytopix(y,y_min,y_max,p_origin_y,py_max)
double	y,	y_min,	y_max;
int	p_origin_y,	py_max;
{

	/*  if (y>y_max | y<y_min ) return(0);  */
	return( (int) floor( p_origin_y + ((y_max-y)/(y_max-y_min))*(py_max-p_origin_y)) );
}

double
pxtorx(px,x_min,x_max,p_origin_x,px_max)
int	px, p_origin_x,px_max;
double  x_min,x_max;
{
	return( (double) x_min + (( (double) px-p_origin_x )/( (double) px_max-p_origin_x))*(x_max-x_min) );
}


double
pytory(py,y_min,y_max,p_origin_y,py_max)
int	py, p_origin_y,py_max;
double	y_min,y_max;
{
	return( (double) y_min + (( (double) py_max - py)/( (double) py_max-p_origin_y))*(y_max-y_min) );
}




int
plot_symbol(window_number,x,y,color,symbol)
int	window_number,			/* window index to draw point */
	color,				/* colormap index of plotted point */
	symbol;				/* symbol key */
double	x, y;				/* absolute coordinates of point to be plotted */

{ 
	int px,py,i;					/* pixel coordinates of plotted point */
	static  int		win_last_call = -1;	/* stash window numbers to save time on multiple calls */
	static	Xv_window	paint_window;		/* cash id data to save time on multiple calls to a window */
	static	Display         *dpy;			/*  "  */
	static	Window          xwin;			/*  "  */
	static	GC              gc;			/*  "  */

	static	int		med_point_x[]={0,1,0,1}, med_point_y[]={0,1,1,0}, med_point_pixels=4;
	static	int		large_point_x[]={-1,0,1,-1,0,1,-1,0,1},
				large_point_y[]={-1,-1,-1,0,0,0,1,1,1}, large_point_pixels=9;
	static	int		huge_point_x[]={-2,-2,-2,-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1,2,2,2},
				huge_point_y[]={-1,0,1,-2,-1,0,1,2,-2,-1,0,1,2,-2,-1,0,1,2,-1,0,1},huge_point_pixels=21;
	static	int		small_tri_x[]={0,-1,0,1,-2,-1,0,1,2}, small_tri_y[]={-1,0,0,0,1,1,1,1,1},
				small_tri_pixels=9;
	static	int		med_tri_x[]={-3,-2,-1,0,1,2,3,-2,-1,0,1,2,-1,0,1,0},
				med_tri_y[]={2,2,2,2,2,2,2,1,1,1,1,1,0,0,0,-1},
				med_tri_pixels=16;
	static	int		large_tri_x[]={-3,-2,-1,0,1,2,3,-2,-1,0,1,2,-2,-1,0,1,2,-1,0,1,-1,0,1,0,0},
				large_tri_y[]={3,3,3,3,3,3,3,2,2,2,2,2,1,1,1,1,1,0,0,0,-1,-1,-1,-2,-3},
				large_tri_pixels=25;
	static	int		huge_tri_x[]={-5,-4,-3,-2,-1,0,1,2,3,4,5,-4,-3,-2,-1,0,1,2,3,4,-4,-3,-2,-1,0,1,2,3,4,
					      -3,-2,-1,0,1,2,3,-2,-1,0,1,2,-2,-1,0,1,2,-1,0,1,-1,0,1,0,0},
				huge_tri_y[]={4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,
					      1,1,1,1,1,1,1,0,0,0,0,0,-1,-1,-1,-1,-1,-2,-2,-2,-3,-3,-3,-4,-5},
				huge_tri_pixels=54;
	static	int		small_cross_x[]={0,0,-2,-1,0,1,2,0,0}, small_cross_y[]={-2,-1,0,0,0,0,0,1,2}, small_cross_pixels=9;
	static	int		med_cross_x[]={0,0,0,-3,-2,-1,0,1,2,3,0,0,0},
				med_cross_y[]={-3,-2,-1,0,0,0,0,0,0,0,1,2,3}, med_cross_pixels=13;
	static	int		large_cross_x[]={0,0,0,0,-4,-3,-2,-1,0,1,2,3,4,0,0,0,0},
				large_cross_y[]={-4,-3,-2,-1,0,0,0,0,0,0,0,0,0,1,2,3,4}, large_cross_pixels=17;
	static	int		huge_cross_x[]={0,0,0,0,0,0,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,0,0,0,0,0,0},
				huge_cross_y[]={-6,-5,-4,-3,-2,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6},
				huge_cross_pixels=25;
	static	int		small_box_x[]={-1,-1,-1,-1,0,1,2,2,2,2,1,0}, small_box_y[]={1,0,-1,-2,-2,-2,-2,-1,0,1,1,1},
				small_box_pixels=12;
	static	int		med_box_x[]={-2,-2,-2,-2,-2,-1,0,1,2,2,2,2,2,1,0,-1},
				med_box_y[]={2,1,0,-1,-2,-2,-2,-2,-2,-1,0,1,2,2,2,2},med_box_pixels=16;
	static	int		large_box_x[]={-3,-3,-3,-3,-3,-3,-3,-2,-1,0,1,2,3,3,3,3,3,3,3,2,1,0,-1,-2},
				large_box_y[]={3,2,1,0,-1,-2,-3,-3,-3,-3,-3,-3,-3,-2,-1,0,1,2,3,3,3,3,3,3},
				large_box_pixels=24;
	static	int		huge_box_x[]={-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-4,-3,-2,-1,0,
					       1,2,3,4,5,5,5,5,5,5,5,5,5,5,5,4,3,2,1,0,-1,-2,-3,-4},
				huge_box_y[]={5,4,3,2,1,0,-1,-2,-3,-4,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,
					       -4,-3,-2,-1,0,1,2,3,4,5,5,5,5,5,5,5,5,5,5},
				huge_box_pixels=40;

	paint_window = (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
	dpy = (Display *)xv_get(paint_window, XV_DISPLAY);
	xwin = (Window) xv_get(paint_window, XV_XID);
	gc =  TwoD_Ds[window_number]->TwoD_Win_Ds->Canvas_Gc;


	px = rxtopix( x, TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min, 
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X, 
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
	py = rytopix( y, TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
			 TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);

	if( px > TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max || 
	    py > TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max ||
	    px < TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X || 
	    py < TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y )	return;

	if(color>=0)
		XSetForeground(dpy,gc,TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[
			       (int) TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color] ]); 
	else
		XSetForeground(dpy,gc,TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[-color]);

	switch (symbol) 
	       {
		case SMALL_POINT:
			XDrawPoint(dpy,xwin,gc,px,py);
			break;
		case MED_POINT:
			for(i=0;i<med_point_pixels;i++)
			    XDrawPoint(dpy,xwin,gc,px+med_point_x[i],py+med_point_y[i]);
			break;
		case LARGE_POINT:
			for(i=0;i<large_point_pixels;i++)
			   XDrawPoint(dpy,xwin,gc,px+large_point_x[i],py+large_point_y[i]);
			break;
		case HUGE_POINT:
			for(i=0;i<huge_point_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+huge_point_x[i],py+huge_point_y[i]);
			break;
		case SMALL_TRI: 
			for(i=0;i<small_tri_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+small_tri_x[i],py+small_tri_y[i]);
			break;
		case MED_TRI:    
			for(i=0;i<med_tri_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+med_tri_x[i],py+med_tri_y[i]);
			break;
		case LARGE_TRI:    
			for(i=0;i<large_tri_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+large_tri_x[i],py+large_tri_y[i]);
			break;
		case HUGE_TRI:    
			for(i=0;i<huge_tri_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+huge_tri_x[i],py+huge_tri_y[i]);
			break;
		case SMALL_CROSS: 
			for(i=0;i<small_cross_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+small_cross_x[i],py+small_cross_y[i]);
			break;
		case MED_CROSS: 
			for(i=0;i<med_cross_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+med_cross_x[i],py+med_cross_y[i]);
			break;
		case LARGE_CROSS: 
			for(i=0;i<large_cross_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+large_cross_x[i],py+large_cross_y[i]);
			break;
		case HUGE_CROSS: 
			for(i=0;i<huge_cross_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+huge_cross_x[i],py+huge_cross_y[i]);
			break;
		case SMALL_BOX: 
			for(i=0;i<small_box_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+small_box_x[i],py+small_box_y[i]);
			break;
		case MED_BOX: 
			for(i=0;i<med_box_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+med_box_x[i],py+med_box_y[i]);
			break;
		case LARGE_BOX: 
			for(i=0;i<large_box_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+large_box_x[i],py+large_box_y[i]);
			break;
		case HUGE_BOX: 
			for(i=0;i<huge_box_pixels;i++)
			       XDrawPoint(dpy,xwin,gc,px+huge_box_x[i],py+huge_box_y[i]);
			break;
               }
	win_last_call = window_number;

}

