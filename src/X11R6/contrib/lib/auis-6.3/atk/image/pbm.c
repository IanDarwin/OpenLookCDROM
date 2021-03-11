/* pbm.ch - class description for interface from PBM format to image */
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

/*
 * portable bit map (pbm) format images
 *
 * jim frost 09.27.89
 *
 * patched by W. David Higgins (wdh@mkt.csd.harris.com) to support
 * raw-format PBM files.
 *
 * patched by Ian MacPhedran (macphed@dvinci.usask.ca) to support
 * PGM and PPM files (03-July-1990)
 */

#include <andrewos.h>
#include <image.ih>
#include <pbm.h>
#include <pbm.eh>

/* SUPPRESS 558 */

static int IntTable[256];
static unsigned int Initialized = 0;

#define NOTINT  -1
#define COMMENT -2
#define SPACE   -3
#define NEWLINE -4

#define BADREAD    0 /* read error */
#define NOTPBM     1 /* not a pbm file */
#define PBMNORMAL  2 /* pbm normal type file */
#define PBMCOMPACT 3 /* pbm compacty type file */
#define PBMRAWBITS 4 /* pbm raw bits type file */
#define PGMNORMAL  5 /* pgm normal type file */
#define PGMRAWBITS 6 /* pgm raw bytes type file */
#define PPMNORMAL  7 /* ppm normal type file */
#define PPMRAWBITS 8 /* ppm raw bytes type file */

static void initializeTable()
{ unsigned int a;

  for (a = 0; a < 256; a++)
    IntTable[a] = NOTINT;
 
  IntTable['#'] = COMMENT;
  IntTable['\n'] = NEWLINE;
  IntTable['\r'] = IntTable['\t'] = IntTable[' '] = SPACE;
  IntTable['0'] = 0;
  IntTable['1'] = 1;
  IntTable['2'] = 2;
  IntTable['3'] = 3;
  IntTable['4'] = 4;
  IntTable['5'] = 5;
  IntTable['6'] = 6;
  IntTable['7'] = 7;
  IntTable['8'] = 8;
  IntTable['9'] = 9;
  Initialized = 1;
}

static int pbmReadChar(f)
     FILE *f;
{ int c;

  if ((c = fgetc(f)) == EOF) {
    fclose(f);
    return(-1);
  }
  if (IntTable[c] == COMMENT)
    do {
      if ((c = fgetc(f)) == EOF)
	return(-1);
    } while (IntTable[c] != NEWLINE);
  return(c);
}

static int pbmReadInt(f)
     FILE *f;
{ int c, value;

  for (;;) {
    if ((c = pbmReadChar(f)) < 0)
      return(-1);
    if (IntTable[c] >= 0)
      break;
  };

  value= IntTable[c];
  for (;;) {
    if ((c = pbmReadChar(f)) < 0)
      return(-1);
    if (IntTable[c] < 0)
      return(value);
    value = (value * 10) + IntTable[c];
  }
}

static int isPBM(f, name, width, height, maxval)
     FILE         *f;
     char         *name;
     unsigned int *width, *height, *maxval;
{ byte buf[4];

  if(!Initialized)
    initializeTable();

  if(fread(buf, sizeof(byte), 2, f) != 2)
    return(NOTPBM);
  if (memToVal((byte *)buf, 2) == memToVal((byte *)"P1", 2)) {
    if (((*width = pbmReadInt(f)) < 0) || ((*height = pbmReadInt(f)) < 0))
      return(NOTPBM);
    *maxval = 1;
    return(PBMNORMAL);
  }
  if (memToVal((byte *)buf, 2) == memToVal((byte *)"P4", 2)) {
    if (((*width = pbmReadInt(f)) < 0) || ((*height = pbmReadInt(f)) < 0))
      return(NOTPBM);
    *maxval = 1;
    return(PBMRAWBITS);
  }
  if (memToVal(buf, 2) == 0x2a17) {
    if (fread(buf, sizeof(byte), 4, f) != 4)
      return(NOTPBM);
    *width = memToVal((byte *)buf, 2);
    *height = memToVal((byte *)(buf + 2), 2);
    *maxval = 1;
    return(PBMCOMPACT);
  }
  if (memToVal(buf, 2) == memToVal((byte *)"P2", 2)) {
    if (((*width = pbmReadInt(f)) < 0) || ((*height = pbmReadInt(f)) < 0))
      return(NOTPBM);
    *maxval = pbmReadInt(f);
    return(PGMNORMAL);
  }
  if (memToVal(buf, 2) == memToVal((byte *)"P5", 2)) {
    if (((*width = pbmReadInt(f)) < 0) || ((*height = pbmReadInt(f)) < 0))
      return(NOTPBM);
    *maxval = pbmReadInt(f);
    return(PGMRAWBITS);
  }
  if (memToVal(buf, 2) == memToVal((byte *)"P3", 2)) {
    if (((*width = pbmReadInt(f)) < 0) || ((*height = pbmReadInt(f)) < 0))
      return(NOTPBM);
    *maxval = pbmReadInt(f);
    return(PPMNORMAL);
  }
  if (memToVal(buf, 2) == memToVal((byte *)"P6", 2)) {
    if (((*width = pbmReadInt(f)) < 0) || ((*height = pbmReadInt(f)) < 0))
      return(NOTPBM);
    *maxval = pbmReadInt(f);
    return(PPMRAWBITS);
  }
  return(NOTPBM);
}

int 
pbm__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ FILE *f;
  unsigned int  width, height, maxval, ret;

  if(!(f = fopen(fullname, "r")))
    return(0);

  ret = isPBM(f, fullname, &width, &height, &maxval, (unsigned int)1);
  fclose(f);
  return(ret != NOTPBM);
}

int
pbm__Load( self, fullname, fp )
    struct pbm *self;
    char *fullname;
    FILE *fp;
{ FILE         *f;
  int           pbm_type;
  unsigned int  x, y;
  unsigned int  width, height, maxval, depth;
  unsigned int  linelen;
  byte          srcmask, destmask;
  byte         *destptr, *destline;
  int           src, size;
  unsigned int  numbytes, numread;
  int           red, grn, blu;

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open pbm file %s.\n", fullname);
	  return(-1);
      }
  }

  pbm_type = isPBM(f, fullname, &width, &height, &maxval);
  if (pbm_type == NOTPBM) {
    fclose(f);
    return(-1);
  }
  switch (pbm_type) {
      case PBMNORMAL:
	  pbm_newBitImage(self, width, height);
	  linelen = (width / 8) + (width % 8 ? 1 : 0);
	  destline = pbm_Data(self);
	  for (y = 0; y < height; y++) {
	      destptr = destline;
	      destmask = 0x80;
	      for (x = 0; x < width; x++) {
		  do {
		      if ((src = pbmReadChar(f)) < 0) {
			  fclose(f);
			  return(-1);
		      }
		      if (IntTable[src] == NOTINT) {
			  printf("%s: Bad image data\n", fullname);
			  fclose(f);
			  return(-1);
		      }
		  } while (IntTable[src] < 0);
	
		  switch (IntTable[src]) {
		      case 1:
			  *destptr |= destmask;
		      case 0:
			  if (! (destmask >>= 1)) {
			      destmask = 0x80;
			      destptr++;
			  }
			  break;
		      default:
			  printf("%s:Bad image data\n", fullname);
			  fclose(f);
			  return(-1);
		  }
	      }
	      destline += linelen;
	  }
	  break;

      case PBMRAWBITS:
	  pbm_newBitImage(self, width, height);
	  destline = pbm_Data(self);
	  linelen = (width + 7) / 8;
	  numbytes = linelen * height;
	  srcmask = 0;		/* force initial read */
	  numread = 0;
	  for (y = 0; y < height; y++) {
	      destptr = destline;
	      destmask = 0x80;
	      if (srcmask != 0x80) {
		  srcmask = 0x80;
		  if ((numread < numbytes) && ((src = fgetc(f)) == EOF)) {
		      fclose(f);
		      return(-1);
		  }
		  numread++;
	      }
	      for (x = 0; x < width; x++) {
		  if (src & srcmask)
		      *destptr |= destmask;
		  if (! (destmask >>= 1)) {
		      destmask = 0x80;
		      destptr++;
		  }
		  if (! (srcmask >>= 1)) {
		      srcmask = 0x80;
		      if ((numread < numbytes) && ((src = fgetc(f)) == EOF)) {
			  fclose(f);
			  return(-1);
		      }
		      numread++;
		  }
	      }
	      destline += linelen;
	  }
	  break;

      case PBMCOMPACT:
	  pbm_newBitImage(self, width, height);
	  destline = pbm_Data(self);
	  linelen = (width / 8) + (width % 8 ? 1 : 0);
	  srcmask = 0x80;
	  destmask = 0x80;
	  if ((src = fgetc(f)) == EOF) {
	      fclose(f);
	      return(-1);
	  }
	  numread = 1;
	  numbytes = width * height;
	  numbytes = (numbytes / 8) + (numbytes % 8 ? 1 : 0);
	  for (y = 0; y < height; y++) {
	      destptr= destline;
	      destmask = 0x80;
	      for (x = 0; x < width; x++) {
		  if (src & srcmask)
		      *destptr |= destmask;
		  if (! (destmask >>= 1)) {
		      destmask = 0x80;
		      destptr++;
		  }
		  if (! (srcmask >>= 1)) {
		      srcmask = 0x80;
		      if ((numread < numbytes) && ((src= fgetc(f)) == EOF)) {
			  fclose(f);
			  return(-1);
		      }
		      numread++;
		  }
	      }
	      destline += linelen;
	  }
	  break;

      case PGMRAWBITS:
	  depth = image_colorsToDepth(maxval);
	  if (depth > 8)
	      pbm_newTrueImage(self, width, height);
	  else {
	      pbm_newRGBImage(self, width, height, depth);
	      for (y = 0; y <= maxval; y++)
	      { /* As in sunraster.c, use simple ramp for grey scale */
		  pbm_RedPixel(self, y) = pbm_GreenPixel(self, y) = pbm_BluePixel(self, y) = 
		    PM_SCALE(y, maxval, 0xffff);
	      }
	      pbm_RGBSize(self) = pbm_RGBUsed(self) = maxval+1;
	  }
	  size = height * width;

	  switch (pbm_Type(self)) {
	      case IRGB:
/* read in the image in a chunk
 */
		  if(fread(pbm_Data(self), sizeof(byte), size, f) != size) {
		      fclose(f);
		      pbm_Reset(self);
		      return(-1);
		  }
/* convert image values
 */
		  destptr = pbm_Data(self);
		  for (y = 0; y < size; y++)
		      *(destptr++) = PM_SCALE(*destptr, maxval, 0xff);
		  break;

	      case ITRUE:
		  for (y = 0; y < size; y++) {
		      if ((src = fgetc(f)) == EOF) {
			  fclose(f);
			  pbm_Reset(self);
			  return(-1);
		      }
		      src = PM_SCALE(src, maxval, 0xff);
		      *(destptr++) = src; /* red */
		      *(destptr++) = src; /* green */
		      *(destptr++) = src; /* blue */
		  }
		  break;
	  }
	  break;

      case PGMNORMAL:
	  depth = image_colorsToDepth(maxval);
	  if (depth > 8)
	      pbm_newTrueImage(self, width, height);
	  else {
	      pbm_newRGBImage(self, width, height, depth);
	      for (y = 0; y <= maxval; y++)
	      { /* As in sunraster.c, use simple ramp for grey scale */
		  pbm_RedPixel(self, y) = pbm_GreenPixel(self, y) = pbm_BluePixel(self, y) = 
		    PM_SCALE(y, maxval, 0xffff);
	      }
	      pbm_RGBSize(self) = pbm_RGBUsed(self) = maxval+1;
	  }
	  destptr = pbm_Data(self);
	  size = height * width;
	  for (y = 0; y < size; y++) {
	      if ((src = pbmReadInt(f)) < 0) {
		  fclose(f);
		  return(-1);
	      }
	      else {
		  src = PM_SCALE(src, maxval, 0xff);
		  if (pbm_Type(self) == ITRUE) {
		      *(destptr++) = src; /* red */
		      *(destptr++) = src; /* green */
		      *(destptr++) = src; /* blue */
		  }
		  else
		      *(destptr++) = src;
	      }
	  }
	  break;

      case PPMRAWBITS:
/* this is nice because the bit format is exactly what we want except
 * for scaling.
 */

	  pbm_newTrueImage(self, width, height);
	  size = height * width * 3;
	  if (fread(pbm_Data(self), sizeof(byte), size, f) != size) {
	      fclose(f);
	      pbm_Reset(self);
	      return(-1);
	  }
	  destptr = pbm_Data(self);
	  for (y = 0; y < size; y++) {
	      *destptr = PM_SCALE(*destptr, maxval, 0xff);
	      destptr++;
	  }
	  break;
      case PPMNORMAL:
	  pbm_newTrueImage(self, width, height);
	  size = height * width;
	  destptr = pbm_Data(self);
	  for (y = 0; y < size; y++) {
	      if (((red = pbmReadInt(f)) == EOF) ||
		  ((grn = pbmReadInt(f)) == EOF) ||
		  ((blu = pbmReadInt(f)) == EOF)) 
	      {
		  fclose(f);
		  return(-1);
	      }
	      *(destptr++) = PM_SCALE(red, maxval, 0xff);
	      *(destptr++) = PM_SCALE(grn, maxval, 0xff);
	      *(destptr++) = PM_SCALE(blu, maxval, 0xff);
	  }
	  break;
  }
  fclose(f);
  return(0);
}

long
pbm__Read( self, file, id )
    struct pbm *self;
    FILE *file;
    long id;
{
    if(pbm_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
pbm__Write( self, file, writeID, level )
    struct pbm *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
pbm__WriteNative( self, file, filename )
    struct pbm *self;
    FILE *file;
    char *filename;
{


}
