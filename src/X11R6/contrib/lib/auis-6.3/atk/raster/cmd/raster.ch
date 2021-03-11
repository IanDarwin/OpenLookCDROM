/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/*

	A raster  refers to a rasterimage containing the raw bits.  It can specify
	scaling, cropping, reflection, rotation, and inversion of the image from the raw bits.

	For a description of the data stream, see itc/be2/raster/rasterspec.d
*/

#include <rect.h>

#include <rastimg.ih>	/* because some macros call routines from it */

#define  raster_PROGRAMMERVERSION    1

/* status values for notify observer */
#define  raster_BITSCHANGED    2  /* the parameters in the
					struct raster have changed */
#define  raster_BOUNDSCHANGED  3  /* the raster is viewing a different
					part of the rasterimage */

class raster : dataobject[dataobj]  {

overrides:

	Read(/* struct raster *self, */ FILE *file, long id)returns long;
	Write(/* struct raster *self, */ FILE *file, long id, long level)
			returns long;
	ReadOtherFormat(FILE *file, char *fmt, char *encoding, char *description) returns boolean;
	WriteOtherFormat(FILE *file, long writeid, int level, int usagetype, char *boundary) returns long;
	ObservedChanged(/* struct raster *self, */ struct rasterimage *pix, long status);
	SetAttributes( struct attributes *attributes );

methods:

	SetPix(/* struct raster *self, */ struct rasterimage *pix);
		/* changes to refer to a new rasterimage;  handles observing */
	Resize(/* struct raster *self, */ long width, long height);
		/* changes the size of the raster;  resets subraster to whole */
	ReadSubRaster(/* struct raster *self, */ FILE *file, struct rectangle *r)
				returns long;
		/* read from file into the subraster with upper left pixel at (x, y) */
	WriteSubRaster(/* struct raster *self, */ FILE *file, long objectid, 
			struct rectangle *subraster)  returns long;
		/* write the subraster at (x,y) with given width and height to file */
	WriteShare(/* struct raster *self, */ FILE *file, struct rectangle *subraster);
		/* write a "share" record for the indicated 'subraster' of 'self' */

macromethods:

	Clear()		(rasterimage_Clear(self->pix))
	GetWidth()	(rasterimage_GetWidth(self->pix))
	GetHeight()	(rasterimage_GetHeight(self->pix))
	GetPix()		(self->pix)

classprocedures:

	InitializeObject(/* struct classhdr *ClassID, */ struct raster *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID, */ struct raster *self);
	Create(/* struct classhdr *ClassID, */ long width, long height)
			returns struct raster *;
		/* creates a raster object with a rasterimage having
		space for a raster of the given width and height */

data:

	struct rasterimage *pix;	/* the image storage */
	boolean	readOnly;
	struct rectangle subraster;	/* choose subraster of rasterimage */

	/* the raster may be a negative image or reflection of the bits in the
	    underlying raster: */
	char  options;
		/* bits of 'option' value */
#define  raster_INVERT	(1<<0)	/* exchange black and white */
#define  raster_FLIP	(1<<1)	/* exch top and bottom */
#define  raster_FLOP	(1<<2)	/* exch left and right */
#define  raster_ROTATE	(1<<3)	/* rotate 90 clockwise */

#define raster_UNITSCALE  136533	 /*0x20000 print at nominal screen size */
	/*  default print scaling is 0x10000 which means 1/2 screen size */

	long xScale, yScale;	/* scaling for printing */
};

