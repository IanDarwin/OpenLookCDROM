/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* misc.c:                                                                     *
*                                                                             *
*  XPM library                                                               *
*  Miscellaneous utilities                                                    *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"
#ifdef VMS
#include "sys$library:stat.h"
#include "sys$library:fcntl.h"
#else
#include <sys/stat.h>
#include <fcntl.h>
#endif

/*
 * Free the computed color table
 */
void
xpmFreeColorTable(colorTable, ncolors)
    char ***colorTable;
    int ncolors;
{
    int a, b;
    char ***ct, **cts;

    if (colorTable) {
	for (a = 0, ct = colorTable; a < ncolors; a++, ct++)
	    if (*ct) {
		for (b = 0, cts = *ct; b <= NKEYS; b++, cts++)
		    if (*cts)
			XpmFree(*cts);
		XpmFree(*ct);
	    }
	XpmFree(colorTable);
    }
}


/*
 * Intialize the xpmInternAttrib pointers to Null to know
 * which ones must be freed later on.
 */
void
xpmInitInternAttrib(attrib)
    xpmInternAttrib *attrib;
{
    attrib->ncolors = 0;
    attrib->colorTable = NULL;
    attrib->pixelindex = NULL;
    attrib->xcolors = NULL;
    attrib->colorStrings = NULL;
    attrib->mask_pixel = UNDEF_PIXEL;
}


/*
 * Free the xpmInternAttrib pointers which have been allocated
 */
void
xpmFreeInternAttrib(attrib)
    xpmInternAttrib *attrib;
{
    unsigned int a, ncolors;
    char **sptr;

    if (attrib->colorTable)
	xpmFreeColorTable(attrib->colorTable, attrib->ncolors);
    if (attrib->pixelindex)
	XpmFree(attrib->pixelindex);
    if (attrib->xcolors)
	XpmFree(attrib->xcolors);
    if (attrib->colorStrings) {
	ncolors = attrib->ncolors;
	for (a = 0, sptr = attrib->colorStrings; a < ncolors; a++, sptr++)
	    if (*sptr)
		XpmFree(*sptr);
	XpmFree(attrib->colorStrings);
    }
}


/*
 * Free array of extensions
 */
void
XpmFreeExtensions(extensions, nextensions)
    XpmExtension *extensions;
    int nextensions;
{
    unsigned int i, j, nlines;
    XpmExtension *ext;
    char **sptr;

    if (extensions) {
	for (i = 0, ext = extensions; i < nextensions; i++, ext++) {
	    if (ext->name)
		XpmFree(ext->name);
	    nlines = ext->nlines;
	    for (j = 0, sptr = ext->lines; j < nlines; j++, sptr++)
		if (*sptr)
		    XpmFree(*sptr);
	    if (ext->lines)
		XpmFree(ext->lines);
	}
	XpmFree(extensions);
    }
}


/*
 * Return the XpmAttributes structure size
 */

int 
XpmAttributesSize()
{
    return sizeof(XpmAttributes);
}

/*
 * Init returned data to free safely later on
 */
void
xpmInitAttributes(attributes)
    XpmAttributes *attributes;
{
    if (attributes) {
	attributes->pixels = NULL;
	attributes->npixels = 0;
	attributes->colorTable = NULL;
	attributes->ncolors = 0;
	attributes->hints_cmt = NULL;
	attributes->colors_cmt = NULL;
	attributes->pixels_cmt = NULL;
	attributes->nextensions = 0;
	attributes->extensions = NULL;
    }
}

/*
 * Free the XpmAttributes structure members
 * but the structure itself
 */
void
XpmFreeAttributes(attributes)
    XpmAttributes *attributes;
{
    if (attributes) {
	if (attributes->valuemask & XpmReturnPixels && attributes->pixels) {
	    XpmFree(attributes->pixels);
	    attributes->pixels = NULL;
	    attributes->npixels = 0;
	}
	if (attributes->valuemask & XpmInfos) {
	    if (attributes->colorTable) {
		xpmFreeColorTable(attributes->colorTable, attributes->ncolors);
		attributes->colorTable = NULL;
		attributes->ncolors = 0;
	    }
	    if (attributes->hints_cmt) {
		XpmFree(attributes->hints_cmt);
		attributes->hints_cmt = NULL;
	    }
	    if (attributes->colors_cmt) {
		XpmFree(attributes->colors_cmt);
		attributes->colors_cmt = NULL;
	    }
	    if (attributes->pixels_cmt) {
		XpmFree(attributes->pixels_cmt);
		attributes->pixels_cmt = NULL;
	    }
	    if (attributes->pixels) {
		XpmFree(attributes->pixels);
		attributes->pixels = NULL;
		attributes->npixels = 0;
	    }
	}
	if (attributes->valuemask & XpmReturnExtensions
	    && attributes->nextensions) {
	    XpmFreeExtensions(attributes->extensions, attributes->nextensions);
	    attributes->nextensions = 0;
	    attributes->extensions = NULL;
	}
	attributes->valuemask = 0;
    }
}


/*
 * Store into the XpmAttributes structure the required informations stored in
 * the xpmInternAttrib structure.
 */
void
xpmSetAttributes(attrib, attributes)
    xpmInternAttrib *attrib;
    XpmAttributes *attributes;
{
    if (attributes) {
	if (attributes->valuemask & XpmReturnInfos) {
	    attributes->cpp = attrib->cpp;
	    attributes->ncolors = attrib->ncolors;
	    attributes->colorTable = attrib->colorTable;

	    attrib->ncolors = 0;
	    attrib->colorTable = NULL;
	}
	attributes->width = attrib->width;
	attributes->height = attrib->height;
	attributes->valuemask |= XpmSize;
    }
}

#ifdef NEED_STRDUP
/*
 * in case strdup is not provided by the system here is one
 * which does the trick
 */
char *
strdup(s1)
    char *s1;
{
    char *s2;
    int l = strlen(s1) + 1;

    if (s2 = (char *) XpmMalloc(l))
	strncpy(s2, s1, l);
    return s2;
}

#endif

/*
 *  File / Buffer utilities
 */
int
XpmReadFileToBuffer(filename, buffer_return)
    char *filename;
    char **buffer_return;
{
    int fd, fcheck, len;
    char *ptr;
    struct stat stats;
    FILE *fp;

    *buffer_return = NULL;

    fd = open(filename, O_RDONLY);
    if (fd < 0)
	return XpmOpenFailed;

    if (fstat(fd, &stats)) {
	close(fd);
	return XpmOpenFailed;
    }
    fp = fdopen(fd, "r");
    if (!fp) {
	close(fd);
	return XpmOpenFailed;
    }
    len = (int) stats.st_size;
    ptr = (char *) XpmMalloc(len + 1);
    if (!ptr) {
	fclose(fp);
	return XpmNoMemory;
    }
    fcheck = fread(ptr, len, 1, fp);
    fclose(fp);
    if (fcheck != 1) {
	XpmFree(ptr);
	return XpmOpenFailed;
    }
    ptr[len] = '\0';
    *buffer_return = ptr;
    return XpmSuccess;
}

int
XpmWriteFileFromBuffer(filename, buffer)
    char *filename;
    char *buffer;
{
    int fcheck, len;
    FILE *fp = fopen(filename, "w");

    if (!fp)
	return XpmOpenFailed;

    len = strlen(buffer);
    fcheck = fwrite(buffer, len, 1, fp);
    fclose(fp);
    if (fcheck != 1)
	return XpmOpenFailed;

    return XpmSuccess;
}
