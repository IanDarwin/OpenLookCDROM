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
  *	pixelimg.H
  */

 

/*
/* Pixel Image -- a simple data structure for blasting bits 

*/

/*	The bits of a pixel image are stored rowwise.  Each row is rounded up to
	a multiple of 16 bits (halfword alignment).  The bits in each word are 
	left to right in their screen order;  the high order bit in the even numbered 
	byte in the word is the leftmost of the bits on the screen.  	The rightmost
	displayed bit is the low order bit of the odd numbered byte of the word.
	A bit value of 1 means foreground color; a 0 means background color.
	(The foreground is black: red=0, green=0, blue=0.)
*/


#include <point.h>
#include <rect.h>


#define WHITEBYTE	0
#define BLACKBYTE	0xFF
#define WHITEHALFWORD  ((WHITEBYTE <<8) | WHITEBYTE)
#define BLACKHALFWORD  ((BLACKBYTE <<8) | BLACKBYTE)
#define pixelimage_DATACHANGED 1

class pixelimage[pixelimg] : observable[observe] {

overrides:

	NotifyObservers(/* struct pixelimage *self, */ long status);
		/* Notifies observers and then clears ChangedRect
			and Resized.
		    The client of pixelimage must call NotifyObservers. */

methods:

	Resize(/* struct pixelimage *self, */ long width, long height);
		/* Changes the width and height of the image.  The upper left
		corners of the old and new images are aligned. */
	Clone(/* struct pixelimage *self */) returns struct pixelimage *;
		/* make a new pixelimage with all fields the same
			(both point to same bits array) */
	Clear(/* struct pixelimage *self */);
		/* set all bits of the raster to WHITE */
	GetPixel(/* struct pixelimage *self, */ long x, long y) returns long;
		/*  return the value of an individual bit.  */
	SetPixel(/* struct pixelimage *self, */ long x, long y, long pixelValue);
		/* set the value of the addressed bit to 'pixelValue' */

	GetRow(/* struct pixelimage *self, */ long x, long y, long length, short *dest)
			returns long;
		/* stores in dest the length bits of self from the row starting at (x,y)
			This routine shifts the bits so the bit in (x,y) is the 
			leftmost bit in dest and others follow in byte order. */
	SetRow(/* struct pixelimage *self, */ long x, long y, long length, short *src)
			returns long;
		/* stores the first length bits of src into the row of self at (x,y)
			This routine shifts the bits so the leftmost bit of src
			is stored at (x,y) and others follow in byte order. */
	GetColumn(/* struct pixelimage *self, */ long x, long y, long length, short *dest)
			returns long;
		/* stores in dest the length bits of self from the column starting at (x,y)
			The bit in (x,y) is the leftmost bit in dest
			and others follow in byte order. */
	SetColumn(/* struct pixelimage *self, */ long x, long y, long length, short *src)
			returns long;
		/* stores the first length bits of src into the column of self at (x,y)
			The leftmost bit of src is stored at (x,y)
			and others follow in byte order. */

	/* * * * * *
		the following MODIFY their 'sub' parameter to lie entirely within the
		rectangle devoted to the raster. They may set the width or height
		of the rectangle to a negative value 
	* * * * * */

	PaintSubraster(/* struct pixelimage *self, */ struct rectangle *sub, long byte);
		/* sets the indicated Subraster to the color given by 'byte', which is
			WHITEBYTE or BLACKBYTE */
	GraySubraster(/* struct pixelimage *self, */ struct rectangle *sub, long level);
		/* sets the indicated Subraster to the gray level given by 'byte',
			which is in the range from 1 t0 16. */
	InvertSubraster(/* struct pixelimage *self, */ struct rectangle *sub);
		/* change ones to zeros and zeros to ones in indicated subraster */
	MirrorLRSubraster(/* struct pixelimage *self, */ struct rectangle *sub);
		/* reflect the subraster about its vertical axis */
	MirrorUDSubraster(/* struct pixelimage *self, */ struct rectangle *sub);
		/* reflect the subraster about its horizontal axis */
	GetRotatedSubraster(/* struct pixelimage *self, */ struct rectangle *sub,
			struct pixelimage *target);
		/* copies the subraster to the target raster, interchanging rows 
		    and columns.  If the target is too small in either dimension, 
		    it is Resized. */
	GetScaledSubraster(/* struct pixelimage *self, */ struct rectangle *sub,
			 long NewW, long NewH, struct pixelimage *target);
		/* Scales the subraster by scaling factors and puts result into
		    target. Target is resized as needed. */
	BlitSubraster(/* struct pixelimage *self, */ long x, long y, struct pixelimage 			*source, struct rectangle *sub,  int function);
		/* Blit the subraster given within the source into self starting at
		    the (x, y) coordinates (as an offset from the origin of self) using
		    the function given. */

macromethods:

	GetHeight()	((self)->numRows)
	GetWidth()	((self)->pixelsPerRow)
	GetRowWidth()	((self)->RowWidth)
	GetBitsPtr()	((self)->bits)
	SetBitsPtr(ptr)	((self)->bits = (ptr))
	SetRO(v)		((self)->ReadOnly = (v))
	GetRO()		((self)->ReadOnly)
	GetResized()	((self)->Resized)
	GetChanged()	(&(self->ChangedRect))

classprocedures:

	InitializeObject(/* struct classhdr *ClassID, */ struct pixelimage*self) returns boolean;
		/* initializes to having 'bits' == NULL */
	FinalizeObject(/* struct classhdr *ClassID, */ struct pixelimage*self);
		/* discards the 'bits' value if non-NULL */
	Create(long width, long height) returns struct pixelimage *;
		/* creates a pixelimage object with 'bits' for a raster
			of the given width and height */
	ClipRange(long *tstart, long *tlength, long start, long length);
		/* tstart...tstart+tlength is a range of integers.  The txxxx values
		are modified, if necessary, so they denote only that portion
		of their original range that lies within start...start+length */		

data:
	long numRows;		/* Number of rows of image's pixels */
	long pixelsPerRow;	/* Number of pixels per row */
	long RowWidth;		/* Number of bytes per row.
					(Will be even.)  */
	unsigned char *bits;		/* Array of the bits */

	struct rectangle ChangedRect;  /* bound the most recently changed subraster */
	boolean Resized;		/* another value for NotifyObservers */
	boolean ReadOnly;		/* when True, the raster bits are locked */
};

/* Predefined constants for use in BlitSubraster(). These are defined exactly the same way as the transfer constants in class graphic. See graphic.{ch,doc} for details. */
#define	pixelimage_SOURCE	0xCC
#define	pixelimage_DEST		0xAA

/* Some synonyms for ease of use */
#define	pixelimage_COPY		pixelimage_SOURCE
#define	pixelimage_OR		(pixelimage_SOURCE | pixelimage_DEST)
#define	pixelimage_XOR		(pixelimage_SOURCE ^ pixelimage_DEST)
#define	pixelimage_AND		(pixelimage_SOURCE & pixelimage_DEST)
#define	pixelimage_WHITE	0x00
#define	pixelimage_BLACK	0xFF
#define	pixelimage_INVERT	((~pixelimage_DEST)&0xFF)

