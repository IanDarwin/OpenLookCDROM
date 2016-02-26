/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) drawimage.c 1.5 92/10/23 
 */

#include <xview/frame.h>
#include <xview/svrimage.h>
#include <sspkg/canshell.h> 
#include <sspkg/drawobj.h> 

 
static unsigned short svrimage1_bits[] = {
#include "../icons/demo1.icon"
};

static unsigned short svrimage2_bits[] = {
#include "../icons/demo2.icon"
};

static unsigned short svrimagemask_bits[] = {
#include "../icons/demomask.icon"
};


Frame		frame;
Canvas_shell	shell;
Drawimage	drawimage;

#define ATTR_KEY XV_KEY_DATA, attr_key
int attr_key;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	Server_image	svrimagemask;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	attr_key = xv_unique_key();

	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			NULL);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
			CANVAS_MIN_PAINT_WIDTH, 100,
			CANVAS_MIN_PAINT_HEIGHT, 40,
			XV_HEIGHT, 100,
			XV_WIDTH, 400,
			NULL);

	svrimagemask = (Server_image) xv_create(XV_NULL, SERVER_IMAGE, 
					XV_WIDTH, 64,
					XV_HEIGHT, 64,
					SERVER_IMAGE_BITS, svrimagemask_bits,
					NULL),

	drawimage = (Drawimage) xv_create(shell, DRAWIMAGE,
			XV_X, 10,
			XV_Y, 10,
			DRAWIMAGE_IMAGE1,
				xv_create(XV_NULL, SERVER_IMAGE, 
					XV_WIDTH, 64,
					XV_HEIGHT, 64,
					SERVER_IMAGE_BITS, svrimage1_bits,
					NULL),
			DRAWIMAGE_IMAGE1_MASK, svrimagemask,
			DRAWIMAGE_IMAGE2,
				xv_create(XV_NULL, SERVER_IMAGE, 
					XV_WIDTH, 64,
					XV_HEIGHT, 64,
					SERVER_IMAGE_BITS, svrimage2_bits,
					NULL),
			DRAWIMAGE_IMAGE2_MASK, svrimagemask,
			NULL);

	(void) xv_create(shell, DRAWTEXT,
			XV_X, 90,
			XV_Y, 10,
			DRAWTEXT_STRING_PTR, "A non-rectangular Drawimage",
			NULL);

	window_fit(shell);
	window_fit(frame);
	xv_main_loop(frame); 
}
 


