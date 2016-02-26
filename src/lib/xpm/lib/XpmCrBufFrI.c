/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrBufFrI.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Scan an image and possibly its mask and create an XPM buffer               *
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

LFUNC(WriteTransparentColor, int, (char **dataptr, unsigned int *data_size,
				   unsigned int *used_size,
				   char **colors, unsigned int cpp,
				   unsigned int mask_pixel,
				   char ***colorTable));

LFUNC(WriteOtherColors, int, (char **dataptr, unsigned int *data_size,
			      unsigned int *used_size,
			      char **colors, XColor * xcolors,
			      unsigned int ncolors, unsigned int cpp,
			      unsigned int mask_pixel, char ***colorTable,
			      unsigned int ncolors2, Pixel * pixels,
			      char *rgb_fname));

LFUNC(WritePixels, void, (char *dataptr, unsigned int *used_size,
			  unsigned int width, unsigned int height,
			  unsigned int cpp, unsigned int *pixels,
			  char **colors));

LFUNC(WriteExtensions, void, (char *dataptr, unsigned int *used_size,
			      XpmExtension * ext, unsigned int num));

LFUNC(ExtensionsSize, int, (XpmExtension * ext, unsigned int num));
LFUNC(CommentsSize, int, (XpmAttributes * attributes));

int
XpmCreateBufferFromImage(display, buffer_return, image, shapeimage, attributes)
    Display *display;
    char **buffer_return;
    XImage *image;
    XImage *shapeimage;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    xpmInternAttrib attrib;

    /*
     * initialize return values
     */
    if (buffer_return)
	*buffer_return = NULL;

    xpmInitInternAttrib(&attrib);

    /*
     * Scan image then create data
     */
    ErrorStatus = xpmScanImage(display, image, shapeimage,
			       attributes, &attrib);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = xpmCreateBuffer(buffer_return, &attrib, attributes);

    xpmFreeInternAttrib(&attrib);

    return (ErrorStatus);
}


#undef RETURN
#define RETURN(status) \
  { if (ptr) \
	XpmFree(ptr); \
    return(status); }

int
xpmCreateBuffer(buffer_return, attrib, attributes)
    char **buffer_return;
    xpmInternAttrib *attrib;
    XpmAttributes *attributes;
{
    /* calculation variables */
    int ErrorStatus;
    char buf[BUFSIZ];
    unsigned int extensions = 0, ext_size = 0;
    unsigned int infos = 0, offset, l, cmt_size = 0;
    char *ptr = NULL, *p;
    unsigned int ptr_size, used_size;

    *buffer_return = NULL;

    infos = attributes && (attributes->valuemask & XpmInfos);
    extensions = attributes && (attributes->valuemask & XpmExtensions)
	&& attributes->nextensions;

    /* compute the extensions and comments size */
    if (extensions)
	ext_size = ExtensionsSize(attributes->extensions,
				  attributes->nextensions);
    if (infos)
	cmt_size = CommentsSize(attributes);

    /*
     * write the header line
     */
    sprintf(buf, "/* XPM */\nstatic char * image_name[] = {\n");
    used_size = strlen(buf);
    ptr_size = used_size + ext_size + cmt_size + 1;
    ptr = (char *) XpmMalloc(ptr_size);
    if (!ptr)
	return XpmNoMemory;
    strcpy(ptr, buf);

    /*
     * write the values line
     */
    if (infos && attributes->hints_cmt) {
	sprintf(ptr + used_size, "/*%s*/\n", attributes->hints_cmt);
	used_size += strlen(attributes->hints_cmt) + 5;
    }
    sprintf(buf, "\"%d %d %d %d", attrib->width, attrib->height,
	    attrib->ncolors, attrib->cpp);
    l = strlen(buf);

    if (attributes && (attributes->valuemask & XpmHotspot)) {
	sprintf(buf + l, " %d %d",
		attributes->x_hotspot, attributes->y_hotspot);
	l = strlen(buf);
    }
    if (extensions) {
	sprintf(buf + l, " XPMEXT");
	l = strlen(buf);
    }
    sprintf(buf + l, "\",\n");
    l = strlen(buf);
    ptr_size += l;
    p = (char *) XpmRealloc(ptr, ptr_size);
    if (!p)
	RETURN(XpmNoMemory);
    ptr = p;
    strcpy(ptr + used_size, buf);
    used_size += l;

    /*
     * write colors
     */
    if (infos && attributes->colors_cmt) {
	sprintf(ptr + used_size, "/*%s*/\n", attributes->colors_cmt);
	used_size += strlen(attributes->colors_cmt) + 5;
    }
    /* transparent color */
    if (attrib->mask_pixel != UNDEF_PIXEL) {
	ErrorStatus =
	    WriteTransparentColor(&ptr, &ptr_size, &used_size,
				  attrib->colorStrings, attrib->cpp,
				  (infos ? attributes->mask_pixel : 0),
				  (infos ? attributes->colorTable : NULL));
	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);

	offset = 1;
    } else
	offset = 0;

    /* other colors */
    ErrorStatus =
	WriteOtherColors(&ptr, &ptr_size, &used_size,
			 attrib->colorStrings + offset,
			 attrib->xcolors + offset, attrib->ncolors - offset,
			 attrib->cpp, (infos ? attributes->mask_pixel : 0),
			 (infos ? attributes->colorTable : NULL),
			 (infos ? attributes->ncolors : 0),
			 (infos ? attributes->pixels : NULL),
			 (attributes &&
			  (attributes->valuemask & XpmRgbFilename) ?
			  attributes->rgb_fname : NULL));
    if (ErrorStatus != XpmSuccess)
	RETURN(ErrorStatus);

    /*
     * now we know the exact size we needed, realloc the data 4 = 1 (for
     * '"') + 3 (for '",\n') 1 = - 2 is because the last line does not end
     * with ',\n' + 3 (for '};\n')
     */
    ptr_size += attrib->height * (attrib->width * attrib->cpp + 4) + 1;

    p = (char *) XpmRealloc(ptr, ptr_size);
    if (!p)
	RETURN(XpmNoMemory);
    ptr = p;

    /*
     * print pixels
     */
    if (infos && attributes->pixels_cmt) {
	sprintf(ptr + used_size, "/*%s*/\n", attributes->pixels_cmt);
	used_size += strlen(attributes->pixels_cmt) + 5;
    }
    WritePixels(ptr + used_size, &used_size, attrib->width, attrib->height,
		attrib->cpp, attrib->pixelindex, attrib->colorStrings);

    /*
     * print extensions
     */
    if (extensions)
	WriteExtensions(ptr + used_size, &used_size,
			attributes->extensions, attributes->nextensions);

    /* close the array */
    sprintf(ptr + used_size, "};\n");

    *buffer_return = ptr;

    return (XpmSuccess);
}


static int
WriteTransparentColor(dataptr, data_size, used_size,
		      colors, cpp, mask_pixel, colorTable)
    char **dataptr;
    unsigned int *data_size;
    unsigned int *used_size;
    char **colors;
    unsigned int cpp;
    unsigned int mask_pixel;
    char ***colorTable;
{
    char buf[BUFSIZ];
    unsigned int key, l;
    char *s, *s2;

    buf[0] = '"';
    strncpy(buf + 1, *colors, cpp);
    s = buf + 1 + cpp;

    if (colorTable && mask_pixel != UNDEF_PIXEL) {
	for (key = 1; key <= NKEYS; key++) {
	    if (s2 = colorTable[mask_pixel][key]) {
		sprintf(s, "\t%s %s", xpmColorKeys[key - 1], s2);
		s += strlen(s);
	    }
	}
	sprintf(s, "\",\n");
    } else
	sprintf(s, "\tc %s\",\n", TRANSPARENT_COLOR);

    l = strlen(buf);
    s = (char *) XpmRealloc(*dataptr, *data_size + l);
    if (!s)
	return (XpmNoMemory);
    *data_size += l;
    strcpy(s + *used_size, buf);
    *used_size += l;
    *dataptr = s;
    return (XpmSuccess);
}

static int
WriteOtherColors(dataptr, data_size, used_size, colors, xcolors, ncolors, cpp,
		 mask_pixel, colorTable, ncolors2, pixels, rgb_fname)
    char **dataptr;
    unsigned int *data_size;
    unsigned int *used_size;
    char **colors;
    XColor *xcolors;
    unsigned int ncolors;
    unsigned int cpp;
    unsigned int mask_pixel;
    char ***colorTable;
    unsigned int ncolors2;
    Pixel *pixels;
    char *rgb_fname;
{
    char buf[BUFSIZ];
    unsigned int a, b, c, d, key, l;
    char *s, *s2, *colorname;
    xpmRgbName rgbn[MAX_RGBNAMES];
    int rgbn_max = 0;

    /* read the rgb file if any was specified */
    if (rgb_fname)
	rgbn_max = xpmReadRgbNames(rgb_fname, rgbn);

    *buf = '"';
    for (a = 0; a < ncolors; a++, colors++, xcolors++) {
	s = buf + 1;
	strncpy(s, *colors, cpp);
	s += cpp;

	c = 1;
	if (colorTable) {
	    d = 0;
	    for (b = 0; b < ncolors2; b++) {
		if (b == mask_pixel) {
		    d = 1;
		    continue;
		}
		if (pixels[b - d] == xcolors->pixel)
		    break;
	    }
	    if (b != ncolors2) {
		c = 0;
		for (key = 1; key <= NKEYS; key++) {
		    if (s2 = colorTable[b][key]) {
			sprintf(s, "\t%s %s", xpmColorKeys[key - 1], s2);
			s += strlen(s);
		    }
		}
	    }
	}
	if (c) {
	    colorname = NULL;
	    if (rgbn_max)
		colorname = xpmGetRgbName(rgbn, rgbn_max, xcolors->red,
					  xcolors->green, xcolors->blue);
	    if (colorname)
		sprintf(s, "\tc %s", colorname);
	    else
		sprintf(s, "\tc #%04X%04X%04X",
			xcolors->red, xcolors->green, xcolors->blue);
	    s += strlen(s);
	}
	strcpy(s, "\",\n");
	l = strlen(buf);
	s = (char *) XpmRealloc(*dataptr, *data_size + l);
	if (!s)
	    return (XpmNoMemory);
	*data_size += l;
	strcpy(s + *used_size, buf);
	*used_size += l;
	*dataptr = s;
    }
    xpmFreeRgbNames(rgbn, rgbn_max);
    return (XpmSuccess);
}

static void
WritePixels(dataptr, used_size, width, height, cpp, pixels, colors)
    char *dataptr;
    unsigned int *used_size;
    unsigned int width;
    unsigned int height;
    unsigned int cpp;
    unsigned int *pixels;
    char **colors;
{
    char *s = dataptr;
    unsigned int x, y, h;

    h = height - 1;
    for (y = 0; y < h; y++) {
	*s++ = '"';
	for (x = 0; x < width; x++, pixels++) {
	    strncpy(s, colors[*pixels], cpp);
	    s += cpp;
	}
	strcpy(s, "\",\n");
	s += 3;
    }
    /* duplicate some code to avoid a test in the loop */
    *s++ = '"';
    for (x = 0; x < width; x++, pixels++) {
	strncpy(s, colors[*pixels], cpp);
	s += cpp;
    }
    *s++ = '"';
    *used_size += s - dataptr;
}

static int
ExtensionsSize(ext, num)
    XpmExtension *ext;
    unsigned int num;
{
    unsigned int x, y, a, size;
    char **line;

    size = 0;
    for (x = 0; x < num; x++, ext++) {
	/* 11 = 10 (for ',\n"XPMEXT ') + 1 (for '"') */
	size += strlen(ext->name) + 11;
	a = ext->nlines;
	for (y = 0, line = ext->lines; y < a; y++, line++)
	    /* 4 = 3 (for ',\n"') + 1 (for '"') */
	    size += strlen(*line) + 4;
    }
    /* 13 is for ',\n"XPMENDEXT"' */
    return size + 13;
}

static void
WriteExtensions(dataptr, used_size, ext, num)
    char *dataptr;
    unsigned int *used_size;
    XpmExtension *ext;
    unsigned int num;
{
    unsigned int x, y, a;
    char **line;
    char *s = dataptr;

    for (x = 0; x < num; x++, ext++) {
	sprintf(s, ",\n\"XPMEXT %s\"", ext->name);
	s += strlen(ext->name) + 11;
	a = ext->nlines;
	for (y = 0, line = ext->lines; y < a; y++, line++) {
	    sprintf(s, ",\n\"%s\"", *line);
	    s += strlen(*line) + 4;
	}
    }
    strcpy(s, ",\n\"XPMENDEXT\"");
    *used_size += s - dataptr + 13;
}

static int
CommentsSize(attributes)
    XpmAttributes *attributes;
{
    int size = 0;

    /* 5 = 2 (for "/_*") + 3 (for "*_/\n") */
    if (attributes->hints_cmt)
	size += 5 + strlen(attributes->hints_cmt);

    if (attributes->colors_cmt)
	size += 5 + strlen(attributes->colors_cmt);

    if (attributes->pixels_cmt)
	size += 5 + strlen(attributes->pixels_cmt);

    return size;
}
