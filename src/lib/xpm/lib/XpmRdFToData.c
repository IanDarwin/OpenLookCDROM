/* Copyright 1990,91 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmRdFToData.c:                                                             *
*                                                                             *
*  XPM library                                                                *
*  Parse an XPM file and create an array of strings corresponding to it.      *
*                                                                             *
*  Developed by Dan Greening dgreen@cs.ucla.edu / dgreen@sti.com              *
\*****************************************************************************/

#include "xpmP.h"

int
XpmReadFileToData(filename, data_return)
    char *filename;
    char ***data_return;
{
    xpmData mdata;
    XpmAttributes attributes;
    xpmInternAttrib attrib;
    int ErrorStatus;

    attributes.valuemask = XpmReturnPixels | XpmReturnInfos
      | XpmReturnExtensions;

    /*
     * initialize return values
     */
    if (data_return) {
	*data_return = NULL;
    }
    if ((ErrorStatus = xpmReadFile(filename, &mdata)) != XpmSuccess)
	return (ErrorStatus);
    xpmInitInternAttrib(&attrib);

    ErrorStatus = xpmParseData(&mdata, &attrib, &attributes);

    if (ErrorStatus == XpmSuccess) {
	/* maximum of allocated pixels will be the number of colors */
	attributes.pixels =
	  (Pixel *) XpmMalloc(sizeof(Pixel) * attrib.ncolors);
	attrib.xcolors = (XColor *) XpmMalloc(sizeof(XColor) * attrib.ncolors);

	if (!attributes.pixels || !attrib.xcolors)
	    ErrorStatus = XpmNoMemory;
	else {
	    int i;

	    for (i = 0; i < attrib.ncolors; i++) {
		/* Fake colors */
		attrib.xcolors[i].pixel = attributes.pixels[i] = i + 1;
	    }
	    xpmSetAttributes(&attrib, &attributes);
	    if (!(attrib.colorStrings =
		  (char **) XpmMalloc(attributes.ncolors * sizeof(char *))))
		ErrorStatus = XpmNoMemory;
	    else {
		attrib.ncolors = attributes.ncolors;
		attributes.mask_pixel = attrib.mask_pixel;
		for (i = 0; i < attributes.ncolors; i++)
		    attrib.colorStrings[i] = attributes.colorTable[i][0];
	    }
	}
    }
    if (ErrorStatus == XpmSuccess)
	ErrorStatus = xpmCreateData(data_return, &attrib, &attributes);
    attrib.ncolors = 0;
    XpmFreeAttributes(&attributes);
    xpmFreeInternAttrib(&attrib);
    xpmDataClose(&mdata);

    return (ErrorStatus);
}
