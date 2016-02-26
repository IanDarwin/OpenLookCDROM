/* window division related controls */

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/cursor.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <xview/textsw.h>
#include <xview/server.h>
#include <xview/seln.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/xv_error.h>

#include <math.h>

#include "robot.h"

#define FACTOR	5 /* factor by which the demo-canvas is smaller than
		 * the full size one */

extern canvas;

float xborder_r;
float xborder_l;
float yborder_u;
float yborder_d;
float x_size;
float y_size;

static GC              gc_demo;
static XGCValues       gcvalues;
static Window		xwin;
static unsigned int	width, height;
static int		big_width, small_width;
static Canvas		split_canvas;


char	string[40];

int	i; /* multiple use integer variable */
Bool	Changed;

Frame	split_frame;

Panel_item	divide_x, divide_y, divide_button;
Panel_item	select_x, select_y, select_button;
Panel_item	window_gap_slider, window_gap_x_slider, window_gap_y_slider;

Panel_item	xlow, xhigh, ylow, yhigh;

int divx, divy, selx, sely;
float startx, starty, endx, endy, stepx, stepy;


void
flip_demo()
{

/* make sure canvas is set to 1/5 of the actual plotting canvas */
	if(xv_get(split_canvas, XV_WIDTH) != xv_get(canvas, XV_WIDTH)/FACTOR){
		xv_set(split_canvas, 
			XV_WIDTH, xv_get(canvas, XV_WIDTH)/FACTOR,
			NULL);
	}
	if(xv_get(split_canvas, XV_HEIGHT) != xv_get(canvas, XV_HEIGHT)/FACTOR){
		xv_set(split_canvas, 
			XV_HEIGHT, xv_get(canvas, XV_HEIGHT)/FACTOR,
			NULL);
	}
	xv_set(split_canvas, XV_X, xv_get(split_panel, XV_WIDTH)
			- xv_get(split_canvas, XV_WIDTH),
		NULL);
	gshow_();
}

/* show how current plot area is split up, gaps, and present plotting
 * area */
void
demo_gaps()
{

float xgap;
float ygap;
int plotx, ploty;
unsigned int widthx, widthy;

int	j;


	selx = xv_get(select_x, PANEL_VALUE);
	sely = xv_get(select_y, PANEL_VALUE);

	divx = xv_get(divide_x, PANEL_VALUE);
	divy = xv_get(divide_y, PANEL_VALUE);

/* No point in selecting if there's only one option */

	if(divx == 1 && divy == 1)
		xv_set(select_button, PANEL_INACTIVE, TRUE, NULL);
	else
		xv_set(select_button, PANEL_INACTIVE, FALSE, NULL);

	if(divx == 1){
		xv_set(select_x, PANEL_INACTIVE, TRUE, NULL);
	}
	else{
		xv_set(select_x, PANEL_INACTIVE, FALSE, NULL);
	}
	if(divy == 1){
		xv_set(select_y, PANEL_INACTIVE, TRUE, NULL);
	}
	else{
		xv_set(select_y, PANEL_INACTIVE, FALSE, NULL);
	}

	xgap = (float) xv_get(window_gap_x_slider, PANEL_VALUE);
	ygap = (float) xv_get(window_gap_y_slider, PANEL_VALUE);
	xgap = xgap/100.;
	ygap = ygap/100.;

	xborder_l = (float)xv_get(xlow, PANEL_VALUE)/100.;
	xborder_r = (float)xv_get(xhigh, PANEL_VALUE)/100.;
	yborder_u = (float)xv_get(yhigh, PANEL_VALUE)/100.;
	yborder_d = (float)xv_get(ylow, PANEL_VALUE)/100.;


		
	sprintf(string, "Sub-window = (%d, %d)", 
		selx,
		sely);

	xv_set(split_frame,
		FRAME_LEFT_FOOTER, string,
		NULL);
	sprintf(string, "of %d (X)  by %d (Y)", 
		divx,
		divy);

	xv_set(split_frame,
		FRAME_RIGHT_FOOTER, string,
		NULL);

/* draw a very crude representation of window splitting */

	XClearWindow(dpy, xwin);

	/* edge of page 
	XDrawLine(dpy, xwin, gc_demo, (int) size, 0, (int) size, (int) size);*/

	x_size = (float) xv_get(split_canvas, XV_WIDTH);
	y_size = (float) xv_get(split_canvas, XV_HEIGHT);

	startx = x_size * xborder_l;
	starty = y_size * (1.0 - yborder_u);

	endx = x_size * xborder_r;
	endy = y_size * (1.0 - yborder_d);



	stepx = (endx - startx)/divx;
	stepy = (endy - starty)/divy;


	widthx = (int) ((float) stepx * (1. - 2.0 * xgap));
	widthy = (int) ((float) stepy * (1. - 2.0 * ygap));



	for(i = 1; i <= divx; i++){
	   for(j = 1; j <= divy; j++){

		plotx = startx + stepx * (i-1);
		plotx = plotx + (int) ((float) stepx * xgap);
		ploty = endy - stepy * j;
		ploty = ploty + (int) ((float) stepy * ygap);

		XDrawRectangle(dpy, xwin, gc_demo, 
			plotx, ploty, widthx, widthy);
		if(i == selx && j == sely){
			XFillRectangle(dpy, xwin, gc_demo, 
				plotx, ploty, widthx, widthy);
		}
	   }
	}
}



void pusechk_(xlo, ylo, xhi, yhi)
float	*xlo, *ylo, *xhi, *yhi;
{
	Changed = FALSE;
	i = (int) (*xlo);
	if(xv_get(xlow, PANEL_VALUE) != i){
		xv_set(xlow, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
	i = (int) (*ylo);
	if(xv_get(ylow, PANEL_VALUE) != i){
		xv_set(ylow, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
	i = (int) (*xhi);
	if(xv_get(xhigh, PANEL_VALUE) != i){
		xv_set(xhigh, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
	i = (int) (*yhi);
	if(xv_get(yhigh, PANEL_VALUE) != i){
		xv_set(yhigh, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
	if(Changed)demo_gaps();
}
	



void
gapcheck_(xgap, ygap)
float	*xgap;
float	*ygap;
{
#define	FUDGEIT	0.001

	Changed = FALSE;
	i = (int) ((*xgap * 100.) + FUDGEIT);
	if(xv_get(window_gap_x_slider, PANEL_VALUE) != i){
		xv_set(window_gap_x_slider, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
	i = (int) ((*ygap * 100.) + FUDGEIT);
	if(xv_get(window_gap_y_slider, PANEL_VALUE) != i){
		xv_set(window_gap_y_slider, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
/* set generic gap slider to minimum (kind of a kludge) */
	i = MIN(xv_get(window_gap_x_slider, PANEL_VALUE), 
		xv_get(window_gap_y_slider, PANEL_VALUE));
	if(xv_get(window_gap_slider, PANEL_VALUE) != i){
		xv_set(window_gap_slider, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}

	if(Changed)demo_gaps();
}

void
divcheck_(x, y)
float	*x;
float	*y;
{
	Changed = FALSE;
	i = (int) *x;
	if(xv_get(divide_x, PANEL_VALUE) != i){
		if(i > xv_get(divide_x, PANEL_MAX_VALUE))
			xv_set(divide_x, PANEL_MAX_VALUE, i, NULL);
		xv_set(divide_x, PANEL_VALUE, i, NULL);
		xv_set(select_x, PANEL_MAX_VALUE, i, NULL);
		Changed = TRUE;
	}

	i = (int) *y;
	if(xv_get(divide_y, PANEL_VALUE) != i){
		if(i > xv_get(divide_y, PANEL_MAX_VALUE))
			xv_set(divide_y, PANEL_MAX_VALUE, i, NULL);
		xv_set(divide_y, PANEL_VALUE, i, NULL);
		xv_set(select_y, PANEL_MAX_VALUE, i, NULL);
		Changed = TRUE;
	}
	if(Changed)demo_gaps();
}

/* check whether panel settings have correct values */
void
selcheck_(x, y)
float *x;
float *y;
{
	Changed = FALSE;
	i = (int) *x;
	if(xv_get(select_x, PANEL_VALUE) != i){
		xv_set(select_x, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}

	i = (int) *y;
	if(xv_get(select_y, PANEL_VALUE) != i){
		xv_set(select_y, PANEL_VALUE, i, NULL);
		Changed = TRUE;
	}
	if(Changed)demo_gaps();
}




void
divide_window()
{
	xv_set(select_x, PANEL_MAX_VALUE, xv_get(divide_x, PANEL_VALUE), NULL);
	xv_set(select_y, PANEL_MAX_VALUE, xv_get(divide_y, PANEL_VALUE), NULL);
	sprintf(inst, "DIVWINDOW %d %d",
		xv_get(divide_x, PANEL_VALUE),
		xv_get(divide_y, PANEL_VALUE));
	to_robot();
	demo_gaps();

}

void
select_window()
{

	sprintf(inst, "SELWINDOW %d %d",
		xv_get(select_x, PANEL_VALUE),
		xv_get(select_y, PANEL_VALUE));
	to_robot();
	demo_gaps();
}

void
window_gap(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
int	gap;

	gap = xv_get(item, PANEL_VALUE);

	if(item == window_gap_slider){
		sprintf(inst, "WINDOWGAP %d", gap);
		xv_set(window_gap_x_slider, PANEL_VALUE, gap, NULL);
		xv_set(window_gap_y_slider, PANEL_VALUE, gap, NULL);
	}

	else if(item == window_gap_x_slider){
		sprintf(inst, "XWINDOWGAP %d", gap);
	}
	else if(item == window_gap_y_slider){
		sprintf(inst, "YWINDOWGAP %d", gap);
	}
	else{
		printf("Error in window_gap proc\n");

	}	
	to_robot();
	demo_gaps();
}

void
split_canvas_proc(window, event)
	Xv_Window       window;
	Event          *event;
{
float	ix, iy;
int	iwind, jwind;

	switch (event_action(event)) {
	case ACTION_SELECT:
	  if(event_is_down(event)){
		ix = (float) event_x(event);
		iy = (float) event_y(event);
/* decode which window this is */
		selx = xv_get(select_x, PANEL_VALUE);
		sely = xv_get(select_y, PANEL_VALUE);

	xborder_l = (float)(xv_get(xlow, PANEL_VALUE))/100.;
	xborder_r = (float)(xv_get(xhigh, PANEL_VALUE))/100.;
	yborder_u = (float)(xv_get(yhigh, PANEL_VALUE))/100.;
	yborder_d = (float)(xv_get(ylow, PANEL_VALUE))/100.;

		/* ix = ix - xborder_l;
		iy = iy - yborder_u; */

		divx = xv_get(divide_x, PANEL_VALUE);
		divy = xv_get(divide_y, PANEL_VALUE);
	x_size = (float) xv_get(split_canvas, XV_WIDTH);
	y_size = (float) xv_get(split_canvas, XV_HEIGHT);


	startx = x_size * xborder_l;
	starty = y_size * (1 - yborder_u);
	endx = x_size * xborder_r;
	endy = y_size * (1 - yborder_d);

		/* return if clicked outside the box */
		if(ix > endx || iy > endy || 
		    ix < startx || iy < starty) return;

		stepx = (endx - startx)/divx;
		stepy = (endy - starty)/divy;

		iwind = (int) ((ix - startx)/ stepx) + 1;
		jwind = (int) (iy - starty)/ stepy;
		jwind = divy - jwind;

		/* select window if different from the current one */
		if(iwind != selx || jwind != sely){
			sprintf(inst, "SELWINDOW %d %d", iwind, jwind);
			to_robot();
		}
	  }


		break;
	default:
		return;
	}

}

void
page_use()
{
	sprintf(inst, "PAGEUSE %d %d %d %d",
		xv_get(xlow, PANEL_VALUE),
		xv_get(ylow, PANEL_VALUE),
		xv_get(xhigh, PANEL_VALUE),
		xv_get(yhigh, PANEL_VALUE));
	to_robot();
	demo_gaps();
}

void
demo_canvas_off()
{
	xv_set(canvas_paint_window(split_canvas),
	WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
		NULL);

}

void
demo_canvas_on()
{
	xv_set(canvas_paint_window(split_canvas),
		WIN_CONSUME_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask | 
			ExposureMask | KeyPressMask | VisibilityChangeMask,
		NULL);

}


void
make_split_frame(frame)
Frame	frame;
{

	split_frame = xv_create(frame, FRAME_CMD,
			FRAME_LABEL, "Sub-Window Controls",
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_SHOW_FOOTER, TRUE,
			NULL);
	split_panel = xv_get(split_frame, FRAME_CMD_PANEL);

	xv_set(split_panel, 
			PANEL_LAYOUT, PANEL_VERTICAL,
			XV_HELP_DATA, "robot:split_panel",
			NULL);

	divide_x = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "No. of X windows",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	1,
				PANEL_MAX_VALUE,	10,
				XV_HELP_DATA, "robot:divide_x",
				NULL);
	divide_y = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "No. of Y Windows",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	1,
				PANEL_MAX_VALUE,	10,
				XV_HELP_DATA, "robot:divide_y",
				NULL);
	divide_button = xv_create(split_panel, PANEL_BUTTON,
				PANEL_LABEL_STRING, "Divide Plot Area",
				PANEL_NOTIFY_PROC, divide_window,
				XV_HELP_DATA, "robot:divide",
				NULL);

	select_x = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Select Window (X)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	1,
				PANEL_MAX_VALUE,	1,
				XV_HELP_DATA, "robot:select_x",
				NULL);
	select_y = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Select Window (Y)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	1,
				PANEL_MAX_VALUE,	1,
				XV_HELP_DATA, "robot:select_y",
				NULL);
	select_button = xv_create(split_panel, PANEL_BUTTON,
				PANEL_LABEL_STRING, "Select Window",
				PANEL_NOTIFY_PROC, select_window,
				PANEL_INACTIVE, TRUE,
				XV_HELP_DATA, "robot:select",
				NULL);

	window_gap_slider = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Window Gap (%)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	0,
				PANEL_MAX_VALUE,	49,
				PANEL_NOTIFY_PROC, window_gap,
				XV_HELP_DATA, "robot:window_gap",
				NULL);
	window_gap_x_slider = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "X Window Gap (%)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	0,
				PANEL_MAX_VALUE,	49,
				PANEL_NOTIFY_PROC, window_gap,
				XV_HELP_DATA, "robot:window_gap_x",
				NULL);
	window_gap_y_slider = xv_create(split_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Y Window Gap (%)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MIN_VALUE,	0,
				PANEL_MAX_VALUE,	49,
				PANEL_NOTIFY_PROC, window_gap,
				XV_HELP_DATA, "robot:window_gap_y",
				NULL);


	xv_create(split_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Page Use",
			PANEL_NOTIFY_PROC, page_use,
			XV_HELP_DATA, "robot:page_use",
			NULL);


	xlow = xv_create(split_panel, PANEL_SLIDER,
				PANEL_LABEL_STRING, "Min. X",
				PANEL_MIN_VALUE, 0,
				PANEL_MAX_VALUE, 100,
				PANEL_DIRECTION, PANEL_VERTICAL,
				PANEL_SLIDER_END_BOXES,	TRUE,
				XV_HELP_DATA, "robot:split_min_x",
				NULL);
	xv_set(split_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

	ylow = xv_create(split_panel, PANEL_SLIDER,
				PANEL_LABEL_STRING, "Min. Y",
				PANEL_MIN_VALUE, 0,
				PANEL_MAX_VALUE, 100,
				PANEL_DIRECTION, PANEL_VERTICAL,
				PANEL_SLIDER_END_BOXES,	TRUE,
				XV_HELP_DATA, "robot:split_min_y",
				NULL);
	xhigh = xv_create(split_panel, PANEL_SLIDER,
				PANEL_LABEL_STRING, "Max. X",
				PANEL_MIN_VALUE, 0,
				PANEL_MAX_VALUE, 100,
				PANEL_DIRECTION, PANEL_VERTICAL,
				PANEL_SLIDER_END_BOXES,	TRUE,
				XV_HELP_DATA, "robot:split_max_x",
				NULL);
	yhigh = xv_create(split_panel, PANEL_SLIDER,
				PANEL_LABEL_STRING, "Max. Y",
				PANEL_MIN_VALUE, 0,
				PANEL_MAX_VALUE, 100,
				PANEL_DIRECTION, PANEL_VERTICAL,
				PANEL_SLIDER_END_BOXES,	TRUE,
				XV_HELP_DATA, "robot:split_max_y",
				NULL);



if(not_open_look){

           xv_set(split_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	
	   xv_create(split_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}


	window_fit(split_panel);

	split_canvas = (Canvas) xv_create(split_frame, CANVAS,
				  XV_HELP_DATA, "robot:split_canvas",
				  WIN_X,    0,
				  XV_HEIGHT, 142,
				  XV_WIDTH, 138,
				  NULL);
	xv_set(split_canvas, XV_X, xv_get(split_panel, XV_WIDTH)
			- xv_get(split_canvas, XV_WIDTH),
		NULL);
	xv_set(canvas_paint_window(split_canvas),
		 WIN_EVENT_PROC, split_canvas_proc,
		NULL);

	demo_canvas_on();
 

	window_fit(split_frame);



	gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
	gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
	gc_demo = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		       (GCForeground | GCBackground), &gcvalues);
	xwin = xv_get(canvas_paint_window(split_canvas), XV_XID);


}


void
show_split()
{
	xv_set(split_frame, XV_SHOW, TRUE,
			   FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
/* update sub-window demo */
	demo_gaps();
}


