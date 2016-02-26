/*
 * Copyright 1989 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */
#include <X11/Xlib.h>
#include <stdio.h>

extern Display *display;
extern int screen_num;

/*
draw_box(left, top, right, bottom)
int left, top, right, bottom;
{
	GC gcontext;
	XPoint points[4];
	int npoints = 4;
	int mode = CoordModeOrigin;

	points[0].x = left;
	points[0].y = top;

	points[1].x = left;
	points[1].y = bottom;

	points[2].x = right;
	points[2].y = bottom;

	points[3].x = right;
	points[3].y = top;

        gcontext = XCreateGC(display, RootWindow(display,screen_num), 
			0, NULL);  

	XSetForeground(display,gcontext,BlackPixel(display,screen_num));
	XDrawLines(display, RootWindow(display, screen_num), gcontext, 
			points, npoints, mode);


	XFlush(display);
}
*/

undraw_box(gc, left, top, right, bottom)
GC gc;
int left,top,right,bottom;
	{
	draw_box(gc, left,top,right,bottom);
	}

draw_box(gc, left, top, right, bottom)
GC gc;
int left, top, right, bottom;
{

	XSetForeground(display, gc, WhitePixel(display, screen_num) ^ BlackPixel(display, screen_num));
	XDrawRectangle(display, RootWindow(display,screen_num), gc, left, 
			top, right - left, bottom - top);
}

/*
draw_box(left,top,right,bottom)
int left,top,right,bottom;
{
	Vertex corner[5];
	int vertexcount = 5;
	int bwidth = 1, bheight = 1; 
	int pixel = WhitePixel;
	int func = GXxor;
	int planes = 1;
	
	corner[0].x = left;
	corner[0].y = top;
	corner[0].flags = 0;
	
	corner[1].x = left;
	corner[1].y = bottom;
	corner[1].flags = 0;
	
	corner[2].x = right;
	corner[2].y = bottom;
	corner[2].flags = 0;
	
	corner[3].x = right;
	corner[3].y = top;
	corner[3].flags = 0;
	
	corner[4].x = left;
	corner[4].y = top;
	corner[4].flags = 0;
	
	XDraw(RootWindow, corner, vertexcount , bwidth, bheight, pixel, 
			func, planes);
	XFlush(display);
}
*/
