/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
 * Copyright 1989, 1990, 1991 Jim Frost
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  The author makes no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 * USE OR PERFORMANCE OF THIS SOFTWARE.
 */

typedef unsigned long  Pixel;     /* what X thinks a pixel is */
typedef unsigned short Intensity; /* what X thinks an RGB intensity is */
typedef unsigned char  byte;      /* byte type */

typedef struct rgbmap {
  unsigned int  size;       /* size of RGB map */
  unsigned int  used;       /* number of colors used in RGB map */
  unsigned int  compressed; /* image uses colormap fully */
  Intensity    *red;        /* color values in X style */
  Intensity    *green;
  Intensity    *blue;
} RGBMap;

#define IBITMAP		0 /* image is a bitmap */
#define IGREYSCALE	1 /* image is 8-bit Greyscale */
#define IRGB		2 /* image is RGB */
#define ITRUE		3 /* image is true color */

#define NUM_IMAGE_TYPES 4

#define BITMAPP(self)		((self)->type == IBITMAP)
#define GREYSCALEP(self)	((self)->type == IGREYSCALE)
#define RGBP(self)		((self)->type == IRGB)
#define TRUEP(self)		((self)->type == ITRUE)

#define TRUE_RED(PIXVAL)   (((PIXVAL) & 0xff0000) >> 16)
#define TRUE_GREEN(PIXVAL) (((PIXVAL) & 0xff00) >> 8)
#define TRUE_BLUE(PIXVAL)  ((PIXVAL) & 0xff)
#define RGB_TO_TRUE(R,G,B) \
  ((((R) & 0xff00) << 8) | ((G) & 0xff00) | ((B) >> 8))

unsigned long doMemToVal();
unsigned long doValToMem();
unsigned long doMemToValLSB();
unsigned long doValToMemLSB();

/* special case 1-byte transfers so they're inline
 */

#define memToVal(PTR,LEN)    ((LEN) == 1 ? (unsigned long)(*(PTR)) : \
			      doMemToVal(PTR,LEN))
#define memToValLSB(PTR,LEN) ((LEN) == 1 ? (unsigned long)(*(PTR)) : \
			      doMemToValLSB(PTR,LEN))
#define valToMem(VAL,PTR,LEN)    ((LEN) == 1 ? \
				  (unsigned long)(*(PTR) = (byte)(VAL)) : \
				  doValToMem(VAL,PTR,LEN))
#define valToMemLSB(VAL,PTR,LEN) ((LEN) == 1 ? \
				  (unsigned long)(*(PTR) = (byte)(VAL)) : \
				  (int)doValToMemLSB(VAL,PTR,LEN))

/* this returns the (approximate) intensity of an RGB triple
 */

#define colorIntensity(R,G,B) \
  (RedIntensity[(R) >> 8] + GreenIntensity[(G) >> 8] + BlueIntensity[(B) >> 8])

class image : dataobject [dataobj] {

overrides:
  Read( FILE *file, long id ) returns long;
  Write( FILE *file, long writeID, int level ) returns long;
  ViewName() returns char *;
  WriteOtherFormat(FILE *file, long writeid, int level, int usagetype, char *boundary) returns long;
  ReadOtherFormat(FILE *file, char *fmt, char *encoding, char *description) returns boolean;

classprocedures:
  InitializeClass() returns boolean;
  InitializeObject( struct image *self ) returns boolean;
  FinalizeObject( struct image *self ) returns void;
  depthToColors( int n ) returns int;
  colorsToDepth( int n ) returns int;

methods:
  Reset() returns void;
  SetSaveFormatString( char *saveformat );
  newRGBMapData( unsigned int size ) returns void;
  freeRGBMapData() returns void;
  newBitImage( unsigned int width, unsigned int height ) returns void;
  newGreyImage( unsigned int width, unsigned int height, unsigned int depth ) returns void;
  newRGBImage( unsigned int width, unsigned int height, unsigned int depth ) returns void;
  newTrueImage( unsigned int width, unsigned int height ) returns void;
  freeImageData() returns void;
  Dither() returns struct image *;
  Brighten( unsigned int percent ) returns void;
  Normalize() returns struct image *;
  Gray() returns void;
  Bit2Grey() returns struct image *;
  Compress() returns void;
  Zoom( unsigned int xzoom, unsigned int yzoom ) returns struct image *;
  Reduce( unsigned int n ) returns struct image *;
  Expand() returns struct image *;
  GammaCorrect( float disp_gam ) returns void;
  Halftone() returns struct image *;
  Duplicate( struct image *target ) returns void;
  WriteNative( FILE *file, char *filename ) returns long;

  SendBeginData(FILE *file, long writeID, int level) returns long;
  SendImageData(FILE *file) returns long;
  SendEndData(FILE *file, long writeID, int id) returns long;

  GetBeginData(FILE *file, long id) returns long;
  GetImageData(FILE *file) returns long;
  GetEndData(FILE *file, long id) returns long;

  /* The following method should be overridden by subclasses */
  Load( char *fullname, FILE *fp ) returns int;

macromethods:
  Type() ((self)->type)
  Data() ((self)->data)
  Width() ((self)->width)
  Height() ((self)->height)
  Depth() ((self)->depth)
  Pixlen() ((self)->pixlen)
  BitsPerPixel() ((self)->bitsperpixel)
  RGBSize() ((self)->rgb->size)
  RGBUsed() ((self)->rgb->used)
  RedMap() ((self)->rgb->red)
  GreenMap() ((self)->rgb->green)
  BlueMap() ((self)->rgb->blue)
  RedPixel(redIndex) (*((self)->rgb->red + redIndex))
  GreenPixel(greenIndex) (*((self)->rgb->green + greenIndex))
  BluePixel(blueIndex) (*((self)->rgb->blue + blueIndex))
  GetOriginX() (0)
  GetOriginY() (0)
  SetJPEGSaveQuality(q) ((self)->jpegSaveQuality = (q))
  GetJPEGSaveQuality() ((self)->jpegSaveQuality)
  SaveFormatString() ((self)->saveformatstring)

data:
  unsigned int type;  /* type of image */
  RGBMap *rgb; /* RGB map of image if IRGB type */
  unsigned int width;  /* width of image in pixels */
  unsigned int height; /* height of image in pixels */
  unsigned int depth;  /* depth of image in bits if IRGB type */
  unsigned int pixlen; /* length of pixel (in bytes) if IRGB type */
  unsigned int bitsperpixel; /* length of pixel (in bits) if IRGB type */
  byte *data; /* data rounded to full byte for each row */
  boolean inited; /* has the image been transformed into an XImage? */
  int jpegSaveQuality;
  char *origData;
  long origDataSize;
  long lastModified;
  char *saveformatstring;
};

#define image_NEW (1<<5)
#define image_CHANGED (1<<6)
