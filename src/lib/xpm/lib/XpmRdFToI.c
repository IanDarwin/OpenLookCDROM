/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmRdFToI.c:                                                                *
*                                                                             *
*  XPM library                                                                *
*  Parse an XPM file and create the image and possibly its mask               *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmReadFileToImage(display, filename, image_return,
		   shapeimage_return, attributes)
    Display *display;
    char *filename;
    XImage **image_return;
    XImage **shapeimage_return;
    XpmAttributes *attributes;
{
    xpmData mdata;
    int ErrorStatus;
    xpmInternAttrib attrib;

    /*
     * initialize return values
     */
    if (image_return)
	*image_return = NULL;
    if (shapeimage_return)
	*shapeimage_return = NULL;

    xpmInitAttributes(attributes);

    if ((ErrorStatus = xpmReadFile(filename, &mdata)) != XpmSuccess)
	return (ErrorStatus);

    xpmInitInternAttrib(&attrib);

    ErrorStatus = xpmParseData(&mdata, &attrib, attributes);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = xpmCreateImage(display, &attrib, image_return,
				     shapeimage_return, attributes);
    if (ErrorStatus >= 0)
	xpmSetAttributes(&attrib, attributes);
    else if (attributes)
	XpmFreeAttributes(attributes);

    xpmFreeInternAttrib(&attrib);
    xpmDataClose(&mdata);

    return (ErrorStatus);
}
