/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) nonlin.c 1.1 92/06/23 
 */

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/rect.h>
#include <xview/svrimage.h>
#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/grip.h>
#include <math.h>


static unsigned short smiley_bits[] = {
#include "../icons/smiley.icon"
};

static unsigned short frown_bits[] = {
#include "../icons/frown.icon"
};


int  limit_move_proc();

#define START_X 20
#define START_Y 65
#define MAX_X  200


main(argc, argv)
	int             argc;
	char           *argv[];
{
	Frame		frame;
	Canvas_shell	canvas_shell;
	Server_image	smiley, frown;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME, NULL);

	smiley = xv_create( XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 32,
		XV_HEIGHT, 20,
		SERVER_IMAGE_BITS, smiley_bits,
		NULL);

	frown = xv_create( XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 32,
		XV_HEIGHT, 20,
		SERVER_IMAGE_BITS, frown_bits,
		NULL);

	canvas_shell = xv_create(frame, CANVAS_SHELL,
		XV_WIDTH,  270,
		XV_HEIGHT, 200,
		NULL);

	(void) xv_create(canvas_shell, GRIP, 
		XV_Y, START_Y,
		XV_X, START_X,
		DRAWIMAGE_IMAGE1, smiley,
		DRAWIMAGE_IMAGE2, frown,
		GRIP_MOVE_PROC, limit_move_proc,
		NULL);

	window_fit(frame);
	xv_main_loop(frame);
}




int
limit_move_proc(paint_window, event, canvas_shell, grip, new_x, new_y)
	Xv_window	paint_window;
	Event		*event;
	Canvas_shell	canvas_shell;
	Grip grip;
	short *new_x;
	short *new_y;
{
	int tmp;

	tmp = *new_x - START_X;
	tmp = MAX(0, tmp);
	tmp = MIN(MAX_X, tmp);	/* *new_x is now in range of 0..100 */

	*new_y = (50 * sin((double)(tmp * M_PI)/MAX_X)) + START_Y ;
	*new_x = tmp + START_X;

	return TRUE;
}


