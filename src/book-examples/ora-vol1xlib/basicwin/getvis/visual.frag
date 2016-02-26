#include <X11/Xlib.h>
#include <X11/Xutil.h>

visual()
{
Display *display;
Colormap colormap;
Window window;
int screen;
	.
	.
	.
XVisualInfo vTemplate;    /* template of the visual we want */
XVisualInfo *visualList;  /* list of XVisualInfo structs that match */
int visualsMatched;       /* number of visuals that match */
	.
	.
	.
/* 
 * Set up the XVisualInfo template so that it returns 
 * a list of all the visuals of depth 8 defined on the 
 * current screen by the X server 
 */
vTemplate.screen = screen;
vTemplate.depth = 8; 
.XX "XGetVisualInfo, example using"
visualList = XGetVisualInfo (display, VisualScreenMask | 
		VisualDepthMask, &vTemplate, &visualsMatched);
if ( visualsMatched == 0 )
	fatalError ("No matching visuals\en");

/*
 * Create a colormap for a window using the first of the visuals
 * in the list ov XVisualInfo structs returned by XGetVisualInfo.
 */
.XX "XCreateColormap, example using"
colormap = XCreateColormap (display, RootWindow(display, screen), 
	visualList[0].visual, AllocNone);
	.
	.
	.
window = XCreateWindow (display, RootWindow(display, screen),
	x, y, width, height, border_width, vTemplate.depth,
	InputOutput, visualList[0].visual, mask, &attributes);
XSetWindowColormap(display, window, colormap);
.XX "XSetWindowColormap, example using"

/* All done with visual information.  Free it.  */

XFree(visualList);
.XX "XFree, example using"
	.
	.
	.
} /* end routine */
.Pe
