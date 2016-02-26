/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) darea2.c 1.9 92/10/23 
 */

#include <xview/frame.h>
#include <xview/svrimage.h>
#include <xview/scrollbar.h>
#include <sspkg/canshell.h> 
#include <sspkg/rectobj.h> 
#include <sspkg/drawobj.h> 
#include <sspkg/disp_list.h> 
 

#define FAR_VISIBLE 8.0		/* amount of far that is visible at a time */
#define NEAR_VISIBLE 16.0	/* amount of near that is visible at a time */
#define FAR_WIDTH 10.0		/* max "width" of far */
#define NEAR_WIDTH 35.0		/* max "width" of near */


Canvas_shell	shell;
Drawarea	near; /* the close mountain range */
Drawarea	far; /* the distant mountain range */
Scrollbar	sbar;


void		resize();
Notify_value	monitor_scroll();
void		set_view();
void		construct_mountains();


main(argc, argv)
	int	argc;
	char	*argv[];
{
	Frame		frame;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);


	frame = (Frame) xv_create(XV_NULL, FRAME, 
		FRAME_LABEL, argv[0],
		XV_WIDTH, 700,
		XV_HEIGHT, 100,
		NULL);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
		CANVAS_RESIZE_PROC, resize,
		CANVAS_SHELL_BATCH_REPAINT, TRUE,
		NULL);

	/*
	 * The last created is on top, so put the distant
	 * mount range behind by creating it first.
	 */
	far = (Drawarea) xv_create(shell, DRAWAREA,
		NULL);

	near = (Drawarea) xv_create(shell, DRAWAREA,
		NULL);

	construct_mountains(near, far);

	sbar = (Scrollbar) xv_create(shell, SCROLLBAR,
		SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
		NULL);

	notify_interpose_event_func(xv_get(sbar, SCROLLBAR_NOTIFY_CLIENT),
		monitor_scroll, NOTIFY_SAFE);

	window_fit(frame);

	xv_set(sbar,
		SCROLLBAR_PIXELS_PER_UNIT, 100,
		SCROLLBAR_OBJECT_LENGTH, 100,
		SCROLLBAR_VIEW_LENGTH, 1,
		SCROLLBAR_PAGE_LENGTH, 5,
		SCROLLBAR_VIEW_START, 0,
		NULL);

	xv_main_loop(frame); 
} 
 

void
resize(shell, width, height)
	Canvas_shell	shell;
	int		width;
	int		height;
{
	xv_set(near,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL);
	set_view(near, NEAR_VISIBLE, NEAR_WIDTH, 0.0);

	xv_set(far,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL);
	set_view(far, FAR_VISIBLE, FAR_WIDTH, 0.0);

	if(sbar)
	  xv_set(sbar,
		SCROLLBAR_OBJECT_LENGTH, 100,
		SCROLLBAR_VIEW_START, 0,
		NULL);
}


Notify_value
monitor_scroll(client, event, sbar, type)
	Notify_client   client;
	Event          *event;
	Scrollbar       sbar;
	Notify_event_type type;
{
	double scrollto;

	if (event_id(event) == SCROLLBAR_REQUEST) {
		scrollto = ((double) xv_get(sbar, SCROLLBAR_VIEW_START) /
			    (double) (xv_get(sbar, SCROLLBAR_OBJECT_LENGTH)-
			    	xv_get(sbar, SCROLLBAR_VIEW_LENGTH)));
		xv_set(shell, CANVAS_SHELL_DELAY_REPAINT, TRUE, NULL);
		set_view(far, FAR_VISIBLE, FAR_WIDTH, scrollto);
		set_view(near, NEAR_VISIBLE, NEAR_WIDTH, scrollto);
		xv_set(shell, CANVAS_SHELL_DELAY_REPAINT, FALSE, NULL);

		return NOTIFY_DONE;
	}
	return notify_next_event_func(client, (Notify_event) event, sbar, type);
}


#define gray_width 2
#define gray_height 2
static unsigned char gray_bits[] = {
   0x01, 0x02};


void
construct_mountains(near, far)
	Drawarea near;
	Drawarea far;
{
	Server_image gray_image;
	/*
	 * Doubles cannot be passed through xv_set, 
	 * so DRAWAREA_RIGHT_X uses a pointer to a double.
	 */
	double right;
	double left;
	double top;
	double bottom;

	/* semi-randomly chosen points for a horizon */
	static DPoint horizon_far[] = {
		/* points are (x, y) */
		{0.0, 0.0,},
		{0.0, 0.4,},
		{1.0, 0.6,},
		{2.0, 0.7,},
		{3.0, 0.45,},
		{4.0, 0.3,},
		{5.0, 0.2,},
		{6.0, 0.25,},
		{7.0, 0.51,},
		{8.0, 0.7,},
		{9.0, 0.5,},
		{FAR_WIDTH, 0.45,},
		{FAR_WIDTH, 0.0,},
	};

	/* semi randomly chosen points for a horizon */
	static DPoint horizon_near[] = {
		/* points are (x, y) */
		{0.0, 0.0,},
		{0.0, 0.1,},
		{1.0, 0.15,},
		{2.0, 0.3,},
		{3.0, 0.49},
		{4.0, 0.3,},
		{5.0, 0.14,},
		{6.0, 0.1,},
		{7.0, 0.2,},
		{8.0, 0.35,},
		{9.0, 0.25,},
		{10.0, 0.45,},
		{11.0, 0.25},
		{12.0, 0.2},
		{13.0, 0.13},
		{14.0, 0.18},
		{15.0, 0.3},
		{16.0, 0.35},
		{17.0, 0.55},
		{18.0, 0.35},
		{19.0, 0.2},
		{20.0, 0.1},
		{21.0, 0.3},
		{22.0, 0.5},
		{23.0, 0.27},
		{24.0, 0.45},
		{25.0, 0.25},
		{26.0, 0.13},
		{27.0, 0.05},
		{28.0, 0.02},
		{29.0, 0.04},
		{30.0, 0.1},
		{31.0, 0.3},
		{32.0, 0.6},
		{33.0, 0.3},
		{34.0, 0.2},
		{NEAR_WIDTH, 0.2},
		{NEAR_WIDTH, 0.0},
	};


	right = FAR_VISIBLE;
	left = 0.0;
	top = 1.0;
	bottom = 0.0;
	xv_set(far,
		DRAWAREA_RIGHT_X, &right,
		DRAWAREA_LEFT_X, &left,
		DRAWAREA_UPPER_Y, &top,
		DRAWAREA_LOWER_Y, &bottom,
		NULL);

	right = NEAR_VISIBLE;
	xv_set(near,
		DRAWAREA_RIGHT_X, &right,
		DRAWAREA_LEFT_X, &left,
		DRAWAREA_UPPER_Y, &top,
		DRAWAREA_LOWER_Y, &bottom,
		NULL);

	gray_image = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
                XV_WIDTH, gray_width,
                XV_HEIGHT, gray_height,
                SERVER_IMAGE_X_BITS, gray_bits,
                NULL);

	VSetStipple(far, gray_image);
	VSetFillStyle(far, FillOpaqueStippled);
	DFillPoly(far, horizon_far, sizeof(horizon_far)/sizeof(DPoint));

	DFillPoly(near, horizon_near, sizeof(horizon_near)/sizeof(DPoint));
}

void
set_view(drawarea, visible, width, scrollto)
	Drawarea drawarea;
	double	visible;
	double	width;
	double 	scrollto;
{
	double left;
	double right;
	double scrollable;

	scrollable = width - visible;
	left = scrollable * scrollto;
	right = left + visible;

	xv_set(drawarea,
		DRAWAREA_LEFT_X, &left,
		DRAWAREA_RIGHT_X, &right,
		NULL);
}

