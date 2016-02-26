/*
 * xcms.c
 *
 * Written by David Flanagan.  Copyright 1991, O'Reilly && Associates.
 *
 *    The X Consortium, and any party obtaining a copy of these files from
 *    the X Consortium, directly or indirectly, is granted, free of charge, a
 *    full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *    nonexclusive right and license to deal in this software and
 *    documentation files (the "Software"), including without limitation the
 *    rights to use, copy, modify, merge, publish, distribute, sublicense,
 *    and/or sell copies of the Software, and to permit persons who receive
 *    copies from any such party to do so.  This license includes without
 *    limitation a license to do the foregoing actions under any patents of
 *    the party supplying this software to the X Consortium.
 *
 * This program demonstrates the use of the Xcms color allocation
 * functions and of the Xcms TekHVC color space gamut querying
 * functions.  Invoke it with a Hue and a Chroma as command line
 * arguments, and it will allocate and display 10 colors at that
 * Hue and Chroma, with Value equally spaced between the minimum
 * and maximum legal Values at that Hue and Chroma.  Hue must be
 * between 0.0 and 360.0, and the maximum legal Chroma varies
 * with Hue.  A Chroma of 30.0 or less should always be legal, and
 * in no case may it exceed 100.0.
 */

#include <stdio.h>    
#include <X11/Xlib.h>
#include <X11/Xcms.h>    

/*
 * This routine allocates n shades of the color with specified Hue and
 * Chroma.  The Value of each shade will be equally spaced between the
 * minimum and maximum Values of the device gamut for the given Hue and
 * Chroma.
 */
Status AllocShades(dpy, cmap, hue, chroma, pixels, n)
Display *dpy;
Colormap cmap;
double hue, chroma;
long *pixels;          /* RETURN */
int n;
{
    XcmsColor color;
    XcmsCCC ccc;
    int i;
    double minv, maxv;
    double deltav;
    
    ccc = XcmsCCCOfColormap(dpy, cmap);
    
    if (XcmsTekHVCQueryMinV(ccc, hue, chroma, &color) == XcmsFailure)
	return XcmsFailure;
    else
	minv = color.spec.TekHVC.V;
    
    if (XcmsTekHVCQueryMaxV(ccc, hue, chroma, &color) == XcmsFailure)
	return XcmsFailure;
    else
	maxv = color.spec.TekHVC.V;
    
    if (n > 1) deltav = (maxv - minv)/(n-1);
    else deltav = maxv - minv;
    
    for(i=0; i < n; i++) {
	color.format = XcmsTekHVCFormat;
	color.spec.TekHVC.H = hue;
	color.spec.TekHVC.C = chroma;
	color.spec.TekHVC.V = minv + i*deltav;
	if (XcmsAllocColor(dpy, cmap, &color, XcmsRGBFormat) == XcmsFailure)
	    return XcmsFailure;
	pixels[i] = color.pixel;
    }
    return XcmsSuccess;
}


void main(argc, argv)
int argc;
char *argv[];
{
    Display *dpy;
    int scrn;
    Window win;
    GC gc;
    Colormap cmap;
    XWindowAttributes wa;
    XEvent event;
    long pixels[10];
    double hue, chroma;
    int i;

    if (argc != 3) {
	(void) fprintf(stderr, "Usage: %s Hue Chroma\n", argv[0]);
	exit(1);
    }

    (void) sscanf(argv[1], "%lg", &hue);
    (void) sscanf(argv[2], "%lg", &chroma);
    
    if ((dpy = XOpenDisplay(NULL)) == NULL) {
	(void) fprintf(stderr, "%s: Can't open display.\n", argv[0]);
	exit(1);
    }
    scrn = DefaultScreen(dpy);
    win = XCreateSimpleWindow(dpy, RootWindow(dpy, scrn), 10, 10, 200, 400, 1,
			      BlackPixel(dpy, scrn), WhitePixel(dpy, scrn));
    gc = XCreateGC(dpy, win, 0, NULL);

    XGetWindowAttributes(dpy, win, &wa);
    cmap = wa.colormap;
    
    if (AllocShades(dpy, cmap, hue, chroma, pixels, 10) == XcmsFailure) {
	(void) fprintf(stderr, "%s: Couldn't allocate colors.\n", argv[0]);
	exit(1);
    }
    
    XSelectInput(dpy, win, ExposureMask);
    XMapWindow(dpy, win);

    while(1) {
	XNextEvent(dpy, &event);
	switch(event.type) {
	case Expose:
	    for(i=0; i<10; i++) {
		XSetForeground(dpy,gc,pixels[i]);
		XFillRectangle(dpy, win, gc, 0, 40*i, 200, 40);
	    }
	    break;
	}
    }
}



