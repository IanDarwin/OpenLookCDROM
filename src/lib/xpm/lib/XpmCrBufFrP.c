/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrBufFrP.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Scan a pixmap and possibly its mask and create an XPM buffer               *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"
#ifdef VMS
#include "sys$library:string.h"
#else
#if defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif
#endif

int
XpmCreateBufferFromPixmap(display, buffer_return, pixmap, shapemask,
			  attributes)
    Display *display;
    char **buffer_return;
    Pixmap pixmap;
    Pixmap shapemask;
    XpmAttributes *attributes;
{
    XImage *image = NULL;
    XImage *shapeimage = NULL;
    unsigned int width = 0;
    unsigned int height = 0;
    int ErrorStatus;
    unsigned int dum;
    int dummy;
    Window win;

    /*
     * get geometry
     */
    if (attributes && attributes->valuemask & XpmSize) {
	width = attributes->width;
	height = attributes->height;
    } else {
	if (pixmap)
	    XGetGeometry(display, pixmap, &win, &dummy, &dummy,
			 &width, &height, &dum, &dum);
	else if (shapemask)
	    XGetGeometry(display, shapemask, &win, &dummy, &dummy,
			 &width, &height, &dum, &dum);
    }

    /*
     * get the images
     */
    if (pixmap)
	image = XGetImage(display, pixmap, 0, 0, width, height,
			  AllPlanes, ZPixmap);
    if (shapemask)
	shapeimage = XGetImage(display, shapemask, 0, 0, width, height,
			       AllPlanes, ZPixmap);

    /*
     * create the buffer from them
     */
    ErrorStatus = XpmCreateBufferFromImage(display, buffer_return,
					   image, shapeimage, attributes);
    if (image)
	XDestroyImage(image);
    if (shapeimage)
	XDestroyImage(shapeimage);

    return (ErrorStatus);
}
