/* $XConsortium: init_win.c,v 5.3 94/04/17 20:44:13 rws Exp $ */

/***********************************************************

Copyright (c) 1989,1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989,1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <stdio.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>

#define	WIN_X	200
#define	WIN_Y  	200
#define	WIN_W  	400
#define	WIN_H  	400

#define	CONNECT_TO_DISPLAY(name) \
	if (!(appl_display = XOpenDisplay(name))) { \
		perror( "Cannot open display\n"); \
		exit(-1); \
	} 

Display *appl_display;
Window appl_window;

void init_window();

void
init_window()
{
	XSetWindowAttributes xswa;
	Colormap map;
	unsigned int bw = 1;
	char *display_name = NULL;
	int bg_pix, bd_pix;
	Visual visual;
	XEvent exposureEvent;

	CONNECT_TO_DISPLAY(display_name);

	map = XDefaultColormap(appl_display, DefaultScreen(appl_display));
			 
	bg_pix = BlackPixel(appl_display, DefaultScreen(appl_display));
	bd_pix = WhitePixel(appl_display, DefaultScreen(appl_display));
						  
	xswa.backing_store = NotUseful;
	xswa.event_mask = ExposureMask | StructureNotifyMask;
	xswa.background_pixel = bg_pix;
	xswa.border_pixel = bd_pix;
	visual.visualid = CopyFromParent;
	appl_window = XCreateWindow(appl_display,
		RootWindow(appl_display, DefaultScreen(appl_display)),
		WIN_X, WIN_Y, WIN_W, WIN_H, bw,
		DefaultDepth(appl_display, DefaultScreen(appl_display)),
		InputOutput, &visual,
		CWEventMask | CWBackingStore | CWBorderPixel | CWBackPixel,
		&xswa);

	XChangeProperty(appl_display,
		appl_window, XA_WM_NAME, XA_STRING, 8,
		PropModeReplace, (unsigned char *)"BeachBall", 5);
	XMapWindow(appl_display, appl_window);
						 
	/** sync and wait for exposure event **/
	XSync(appl_display, 0);
	XWindowEvent(appl_display,
		appl_window, ExposureMask, &exposureEvent);
}
