/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) hist_stdin.c 1.2 92/09/08 
 */

#include <xview/frame.h>
#include <xview/font.h>
#include <sspkg/canshell.h> 
#include <sspkg/box.h> 
#include <sspkg/rectobj.h> 
#include <sspkg/drawobj.h> 
 
void		resize();

Drawarea	hist;

/* input format:
 *	int+
 * e.g.:
 *	123 129 140 150 144 120 110
 *	107 112 130 133 122 112 98
 */


main(argc, argv)
	int	argc;
	char	*argv[];
{
	Frame		frame;
	Canvas_shell	shell;
	Box		tmpbox;
	Xv_Font		labelfont;
	Xv_Font		labelfont2;
	int		i;
	double left =	0.0;
	double right=	1.0;
	double upper = 1.0;
	double lower = 0.0;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME, 
		FRAME_LABEL, argv[0],
		XV_WIDTH, 800,
		XV_HEIGHT, 400,
		NULL);
 
/*
	labelfont = xv_find(NULL, FONT,
		FONT_NAME, "helvetica-24",
		NULL);

	labelfont2 = xv_find(NULL, FONT,
		FONT_NAME, "helvetica-18",
		NULL);
*/

	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
		XV_WIDTH, 450,
		XV_HEIGHT, 200,
		NULL);

	hist = (Drawarea) xv_create(shell, DRAWAREA,
		RECTOBJ_RESIZABLE, TRUE,
		DRAWAREA_LEFT_X, &left,
		DRAWAREA_RIGHT_X, &right,
		DRAWAREA_UPPER_Y, &upper,
		DRAWAREA_LOWER_Y, &lower,
		XV_WIDTH, xv_get(shell, XV_WIDTH),
		XV_HEIGHT, xv_get(shell, XV_HEIGHT),
		NULL);

	xv_set(shell,
		CANVAS_RESIZE_PROC, resize,
		NULL);

	read_input();

	window_fit(frame);
	xv_main_loop(frame); 
} 

 
read_input()
{
	int count = 0;
	double top = 0;
	double right = 0;
	double num;
	char string[1024];

	VClear(hist);

	while(scanf("%s", string) == 1) {
		num = (double) atoi(string);
		if(num > top)
			top = num;

		DFillRectangle(hist, .0 + count, 0.0, 1.0, num);
		count++;
	}

	right = (double) count;
	xv_set(hist,
		DRAWAREA_UPPER_Y, &top,
		DRAWAREA_RIGHT_X, &right,
		NULL);
}


void
resize(shell, width, height)
	Canvas_shell	shell;
	int		width;
	int		height;
{
	xv_set(hist,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL);
}

