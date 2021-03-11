/* cmuwm.c - class description for ATK raster interface to image */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
static char cmuwm_rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/cmuwm.c,v 1.15 1994/03/21 19:18:31 rr2b Exp $";
#endif

#include <andrewos.h>
#include <image.ih>
#include <dataobj.ih>
#include <filetype.ih>
#include <raster.ih>
#include <rastimg.ih>
#include <cmuwm.h>
#include <cmuwm.eh>

int 
cmuwm__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{
    FILE *f;
    int r = 0;
    long objectID;
    char *objectName;

    if (!(f = fopen(fullname, "r")))
	return(0);

    if(objectName = filetype_Lookup(f, fullname, &objectID, NULL))
	r = !(strcmp(objectName, "raster"));
    fclose(f);
    return r;
}

int
cmuwm__Load( image, fullname, fp )
    struct cmuwm *image;
    char *fullname;
    FILE *fp;
{
    FILE *f;
    struct raster *raster = raster_New();
    long objectID;
    char *objectName;

    if(raster==NULL) {
	fprintf(stderr, "Couldn't create raster to load image from.\n");
	return -1;
    }
    
    if((f = fp) == 0) {
	if (! (f = fopen(fullname, "r"))) {
	    fprintf(stderr, "Couldn't open file %s for reading.\n", fullname);
	    return(-1);
	}
    }

    if(objectName = filetype_Lookup(f, fullname, &objectID, NULL)) {
	long status = 0;
	if((status = raster_Read(raster, f, objectID)) == dataobject_NOREADERROR && raster_GetPix(raster)!=NULL) {
	    int i;
	    int w = raster_GetWidth(raster), h = raster_GetHeight(raster);
	    int widthBytes = ((w)/8) + ((w)%8 ? 1 : 0);
	    short *buf=(short *)malloc(widthBytes+(widthBytes&1));
	    char *ibuf;
	    if(buf==NULL) {
		raster_Destroy(raster);
		return -1;
	    }
	    bzero(buf,widthBytes+(widthBytes&1));
	    cmuwm_newBitImage(image, w, h);
	    ibuf=cmuwm_Data(image);
	    for(i=0;i<h;i++) {
		rasterimage_GetRow(raster_GetPix(raster), 0, i, w, buf);
		bcopy(buf, ibuf, widthBytes);
		ibuf+=widthBytes;
	    }
	    raster_Destroy(raster);
	    free(buf);
	}
	else {
	    fprintf(stderr, "cmuwm: Read error (%d)\n", status);
	    fclose(f);
	    return(-1);
	}
    }
    else {
	fprintf(stderr, "objectName: %s\n", objectName);
	fclose(f);
	return(-1);
    }
    if (fp == NULL) /* If fp is NULL, this means we opened filename and so must also close it. */
	fclose(f);
    return(0);
}

long
cmuwm__Read( self, file, id )
    struct cmuwm *self;
    FILE *file;
    long id;
{
    if(cmuwm_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else return(dataobject_BADFORMAT);
}

long
cmuwm__Write( self, file, writeID, level )
    struct cmuwm *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
cmuwm__WriteNative( self, file, filename )
    struct cmuwm *self;
    FILE *file;
    char *filename;
{
    struct raster *ras;
    FILE *f;
    int w = cmuwm_Width(self), h = cmuwm_Height(self);
    long status;

    if((f = file) == 0) {
	if (! (f = fopen(filename, "w"))) {
	    fprintf(stderr, "Couldn't open file %s for writing.\n", filename);
	    return(-1);
	}
    }

    if (cmuwm_Type(self) != IBITMAP) /* If the image, which isn't the original because we duplicated the original to self, isn't of type IBITMAP, dither it down. */
	cmuwm_Dither(self);

    if((ras = raster_Create(w, h)) && raster_GetPix(ras)) {
	int widthBytes = w/8 + (w%8 ? 1 : 0);
	int i;
	char *ibuf=cmuwm_Data(self);
	short *buf=(short *)malloc(widthBytes+(widthBytes&1));
	if(buf==NULL) {
	    if(file==NULL) fclose(f);
	    raster_Destroy(ras);
	    return -1;
	}
	for(i=0;i<h;i++) {
	    bcopy(ibuf, buf, widthBytes);
	    rasterimage_SetRow(raster_GetPix(ras), 0, i, w, buf);
	    ibuf+=widthBytes;
	}
	if((status = raster_Write(ras, f, raster_UniqueID(ras), -1)) != raster_UniqueID(ras)) {
	    if(file==NULL) fclose(f);
	    fprintf(stderr, "raster: Write error (%d)\n", status);
	    return(-1);
	}
	raster_Destroy(ras);
	if (file == NULL) /* this means we opened filename; so we must also close it. */
	    fclose(f);
    }
    return(0);
}
