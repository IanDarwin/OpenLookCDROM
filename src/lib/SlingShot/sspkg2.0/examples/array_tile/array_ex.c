/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) array_ex.c 1.2 92/10/15 
 */

#include <xview/frame.h>
#include <sspkg/canshell.h> 
#include <sspkg/drawobj.h> 
#include <sspkg/array.h> 
#include <sspkg/box.h> 

Frame		frame;
Canvas_shell	shell;
Clockobj	my_clock;


main(argc, argv)
	int	argc;
	char	*argv[];
{
	Array_tile at;
	Box	b1, b2;
	Drawtext dt;
	Clockobj cl;
	int	i,j;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
 
	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			NULL);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
			CANVAS_MIN_PAINT_WIDTH, 250,
			CANVAS_MIN_PAINT_HEIGHT, 250,
			NULL);

	b1= (Box)xv_create(shell, BOX,
			RECTOBJ_BORDER, 10,
			BOX_LAYOUT, BOX_LAYOUT_VERTICAL,
			NULL);

	(void) xv_create(b1, DRAWTEXT,
			DRAWTEXT_STRING_PTR, "Title",
			DRAWTEXT_JUSTIFY, DRAWTEXT_JUSTIFY_CENTER,
			RECTOBJ_SELECTABLE, FALSE,
			XV_WIDTH, 200,
			NULL);

	b2= (Box)xv_create(b1, BOX,
			RECTOBJ_BORDER, 10,
			BOX_GAP, 0,
			BOX_LAYOUT, BOX_LAYOUT_HORIZONTAL,
			NULL);


	for(i=0; i<10; i++ ){
		at = (Array_tile) xv_create(b2, ARRAY_TILE,
			ARRAY_TILE_N_COLUMNS, 1,
			ARRAY_TILE_N_ROWS, 10,
			ARRAY_TILE_COLUMN_GAP, 0,
			ARRAY_TILE_ROW_GAP, 0,
			ARRAY_TILE_COLUMN_WIDTH, 0,
			ARRAY_TILE_HLINES, TRUE,
			ARRAY_TILE_ALIGN, ARRAY_TILE_ALIGN_WEST,
			NULL);

		if(i != 0) 
		  for(j=0; j<10;j++) {
			char str[40];
			sprintf(str, "(%d, %d)", j, i-1);
			dt = (Drawtext) xv_create(at, DRAWTEXT,
				DRAWTEXT_STRING, str,
				DRAWTEXT_LENGTH, i,
				XV_HEIGHT, 20,
				XV_WIDTH, 40+i*4, /* do some variable widths */
				NULL);
		  }
		else
		  for(j=0; j<10; j++) {
			cl = (Clockobj) xv_create(at, CLOCKOBJ,
				XV_HEIGHT, 20,
				XV_WIDTH, 20,
				CLOCKOBJ_MIN, j*5,
				CLOCKOBJ_HR, 10,
				NULL);
		  }
	}


	xv_set(shell, 
		XV_WIDTH, xv_get(b1, XV_WIDTH),
		XV_HEIGHT, xv_get(b1, XV_HEIGHT),
		NULL);

	window_fit(frame);

	xv_main_loop(frame); 
} 


