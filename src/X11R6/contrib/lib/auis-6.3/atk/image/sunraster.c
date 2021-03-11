/* sunraster.c - class description for interface from Sun raster format to image */
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

/* sunraster.c:
 *
 * sun rasterfile image type
 *
 * jim frost 09.27.89
 *
 * Copyright 1989, 1991 Jim Frost.
 * See included file "copyright.h" for complete copyright information.
 */
#ifndef _JIM_COPYRIGHT_
/*
 * Copyright 1989, 1993 Jim Frost
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

#ifndef __SABER__
static char *Copyright= "Copyright 1989, 1993 Jim Frost";
#endif
#define _JIM_COPYRIGHT_
#endif

#include <andrewos.h>
#include <image.ih>
#include <sunraster.h>
#include <sunraster.eh>

/* SUPPRESS 558 */
/* SUPPRESS 560 */

static void babble(name, header)
     char           *name;
     struct rheader *header;
{
  printf("%s is a", name);
  switch (memToVal(header->type, 4)) {
  case ROLD:
    printf("n old-style");
    break;
  case RSTANDARD:
    printf(" standard");
    break;
  case RRLENCODED:
    printf(" run-length encoded");
    break;
  case RRGB:
    printf(" RGB"); /* RGB format instead of BGR */
    break;
  case RTIFF:
    printf(" TIFF");
    break;
  case RIFF:
    printf(" RIFF");
    break;
  default:
    printf(" unknown-type");
  }
  printf(" %dx%d", memToVal(header->width, 4), memToVal(header->height, 4));

  switch (memToVal(header->depth, 4)) {
  case 1:
    printf(" monochrome");
    break;
  case 8:
    printf(" 8 plane %s",
	   memToVal(header->maplen, 4) > 0 ? "color" : "greyscale");
    break;
  case 24:
    printf(" 24 plane color");
    break;

  case 32:
    /* isn't it nice how the sunraster.h file doesn't bother to mention that
     * 32-bit depths are allowed?
     */
    printf(" 32 plane color");
    break;
  }
  printf(" Sun rasterfile\n");
}

int 
sunraster__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ FILE          *f;
  struct rheader  header;
  int             r;

  if(!(f = fopen(fullname, "r"))) {
    perror("sunrasterIdent");
    return(0);
  }
  switch (fread((byte*)&header, sizeof(struct rheader), 1, f)) {
      case -1:
	  perror("sunrasterIdent");
	  r= 0;
	  break;

      case sizeof(struct rheader):
	  if (memToVal(header.magic, 4) != RMAGICNUMBER) {
	      r = 0;
	      break;
	  }
	  babble(fullname, &header);
	  r = 1;
	  break;

      default:
	  r = 0;
	  break;
  }
  fclose(f);
  return(r);
}

/* read either rl-encoded or normal image data
 */

static void sunread(f, buf, len, enc)
     FILE        *f;
     byte         *buf;
     unsigned int  len;
     unsigned int  enc;  /* true if encoded file */
{ static byte repchar, remaining= 0;

  /* rl-encoded read
   */

  if (enc) {
    while (len--)
      if (remaining) {
	remaining--;
	*(buf++)= repchar;
      }
      else {
	if (fread(&repchar, sizeof(byte), 1, f) != 1) {
	  printf("sunrasterLoad: Bad read on image data\n");
	  exit(1);
	}
	if (repchar == RESC) {
	  if (fread(&remaining, sizeof(byte), 1, f) != 1) {
	    printf("sunrasterLoad: Bad read on image data\n");
	    exit(1);
	  }
	  if (remaining == 0)
	    *(buf++)= RESC;
	  else {
	    if (fread(&repchar, sizeof(byte), 1, f) != 1) {
	      printf("sunrasterLoad: Bad read on image data\n");
	      exit(1);
	    }
	    *(buf++)= repchar;
	  }
	}
	else
	  *(buf++)= repchar;
      }
  }

/* normal read
 */

  else {
    if (fread(buf, sizeof(byte), len, f) < len) {
      printf("sunrasterLoad: Bad read on image data\n");
      exit(1);
    }
  }
}

int
sunraster__Load( self, fullname, fp )
    struct sunraster *self;
    char *fullname;
    FILE *fp;
{ FILE          *f;
  struct rheader  header;
  unsigned int    mapsize;
  byte           *map;
  byte           *mapred, *mapgreen, *mapblue;
  unsigned int    depth;
  unsigned int    linelen;   /* length of raster line in bytes */
  unsigned int    fill;      /* # of fill bytes per raster line */
  unsigned int    enc;
  byte            fillchar;
  byte           *lineptr;
  unsigned int    x, y, size;

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open sunraster file %s.\n", fullname);
	  return(-1);
      }
  }
  switch (size = fread((byte*)&header, sizeof(struct rheader), 1, f)) {
      case -1:
	  fclose(f);
	  return(-1);

      case 1 /* sizeof(struct rheader) */:
	  if (memToVal(header.magic, 4) != RMAGICNUMBER) {
	      fclose(f);
	      return(-1);
	  }
	  break;

      default:
	  fclose(f);
	  return(-1);
  }

/* get an image to put the data in
 */

  depth = memToVal(header.depth, 4);
  switch(depth) {
      case 1:
	  sunraster_newBitImage(self, memToVal(header.width, 4), memToVal(header.height, 4));
	  break;
      case 8:
	  sunraster_newRGBImage(self, memToVal(header.width, 4), memToVal(header.height, 4), memToVal(header.depth, 4));
	  break;
      case 24:
      case 32:
	  sunraster_newTrueImage(self, memToVal(header.width, 4), memToVal(header.height, 4));
	  break;
      default:
	  printf("sunrasterLoad: Bad depth %d (only 1, 8, 24 are valid)\n", depth);
	  exit(1);
  }

/* set up the colormap
 */

  if (depth == 1)
    linelen = (sunraster_Width(self) / 8) + (sunraster_Width(self) % 8 ? 1 : 0);
  else
    linelen = sunraster_Width(self) * sunraster_Pixlen(self);
  fill = (linelen % 2 ? 1 : 0);
  /*
   *  Handle color...
   */
  if (mapsize = memToVal(header.maplen, 4)) {
    map = malloc(mapsize);
    if (fread(map, sizeof(byte), mapsize, f) < mapsize) {
      return(-1);
    }
    mapsize /= 3;
    mapred = map;
    mapgreen = mapred + mapsize;
    mapblue = mapgreen + mapsize;
    if (sunraster_RGBSize(self) == 0)
	sunraster_newRGBMapData(self, mapsize);
    for (y = 0; y < mapsize; y++) {
	sunraster_RedPixel(self, y) = (*(mapred++) << 8);
	sunraster_GreenPixel(self, y) = (*(mapgreen++) << 8);
	sunraster_BluePixel(self, y) = (*(mapblue++) << 8);
    }
    free(map);
    sunraster_RGBUsed(self) = mapsize;
  }

  /*
   *  Handle 8-bit greyscale via a simple ramp function...
   */
  else if (depth == 8) {
    mapsize = 256*3;
    map = malloc(mapsize);
    for (y = 0; y < 256; y += 1) {
      map[y] = map[256+y] = map[2*256+y] = y;
    }
    mapsize /= 3;
    mapred = map;
    mapgreen = mapred + mapsize;
    mapblue = mapgreen + mapsize;
    if (sunraster_RGBSize(self) == 0)
	sunraster_newRGBMapData(self, mapsize);
    for (y = 0; y < mapsize; y++) {
	sunraster_RedPixel(self, y) = (*(mapred++) << 8);
	sunraster_GreenPixel(self, y) = (*(mapgreen++) << 8);
	sunraster_BluePixel(self, y) = (*(mapblue++) << 8);
    }
    free(map);
    sunraster_RGBUsed(self) = mapsize;
  }
  /* 24-bit and 32-bit handle themselves.  currently we don't support
   * a colormap for them.
   */

  enc = (memToVal(header.type, 4) == RRLENCODED);
  lineptr = sunraster_Data(self);

  /* if it's a 32-bit image, we read the line and then strip off the
   * top byte of each pixel to get truecolor format
   */

  if (depth >= 24) {
    byte *buf, *bp;

    buf = malloc(sunraster_Width(self) * (depth == 24 ? 3 : 4));
    for ( y= 0; y < sunraster_Height(self); y++) {
      sunread(f, buf, sunraster_Width(self) * (depth == 24 ? 3 : 4), enc);
      bp = buf;
      if (depth == 24) {
	   for (x = 0; x < sunraster_Width(self); x++) {
		*(lineptr++) = *(bp + 2); /* red */
		*(lineptr++) = *(bp + 1); /* green */
		*(lineptr++) = *bp;       /* blue */
		bp += 3;
	   }
      }
      else {
	   for (x = 0; x < sunraster_Width(self); x++) {
		*(lineptr++) = *(bp + 3); /* red */
		*(lineptr++) = *(bp + 2); /* green */
		*(lineptr++) = *(bp + 1); /* blue */
		bp += 4;
	   }
      }
      if (fill)
	sunread(f, &fillchar, fill, enc);
    }
    free(buf);
  }
  else {
    for (y = 0; y < sunraster_Height(self); y++) {
      sunread(f, lineptr, linelen, enc);
      lineptr += linelen;
      if (fill)
	sunread(f, &fillchar, fill, enc);
    }
  }
  fclose(f);
  return(0);
}

long
sunraster__Read( self, file, id )
    struct sunraster *self;
    FILE *file;
    long id;
{
    if(sunraster_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
sunraster__Write( self, file, writeID, level )
    struct sunraster *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
sunraster__WriteNative( self, file, filename )
    struct sunraster *self;
    FILE *file;
    char *filename;
{
return(0);
}
