/* xwd.ch - class description for interface from X Window Dump format to image */
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

/* xwd.c:
 *
 * XWD file reader.  unfortunately the bozo who thought up this format didn't
 * define anything at all that we can use as an identifier or even to tell
 * what kind of machine dumped the format.  what this does is read the
 * header and look at several fields to decide if this *might* be an XWD
 * file and if it is what byte order machine wrote it.
 *
 * jim frost 07.24.90
 *
 * Copyright 1990 Jim Frost.  See included file "copyright.h" for complete
 * copyright information.
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
#include <xwd.h>
#include <X11/X.h>
#include <xwd.eh>

/* SUPPRESS 558 */

/* this reads the header and does the magic to determine if it is indeed
 * an XWD file.
 */

static int isXWD(name, f, header)
     char      *name;
     FILE      *f;
     XWDHeader *header;
{ GenericXWDHeader  gh;
  int               a;

  if (fread((byte *)&gh, 1, sizeof(GenericXWDHeader), f) != sizeof(GenericXWDHeader)) {
      printf("Couldn't get GenericXWDHeader\n");
    return(0);
  }

  /* first try -- see if XWD version number matches in either MSB or LSB order
   */

  if (memToVal(gh.file_version, 4) != XWD_VERSION) {
      printf("version mismatch: %d, %d\n", memToVal(gh.file_version, 4), XWD_VERSION);
    return(0);
  }

  /* convert fields to fill out header.  things we don't care about
   * are commented out.
   */

  header->header_size= memToVal(gh.header_size, 4);
  header->file_version= memToVal(gh.file_version, 4);
  header->pixmap_format= memToVal(gh.pixmap_format, 4);
  header->pixmap_depth= memToVal(gh.pixmap_depth, 4);
  header->pixmap_width= memToVal(gh.pixmap_width, 4);
  header->pixmap_height= memToVal(gh.pixmap_height, 4);
  header->xoffset= memToVal(gh.xoffset, 4);
  header->byte_order= memToVal(gh.byte_order, 4);
  header->bitmap_unit= memToVal(gh.bitmap_unit, 4);
  header->bitmap_bit_order= memToVal(gh.bitmap_bit_order, 4);
  header->bitmap_pad= memToVal(gh.bitmap_pad, 4);
  header->bits_per_pixel= memToVal(gh.bits_per_pixel, 4);
  header->bytes_per_line= memToVal(gh.bytes_per_line, 4);
  header->visual_class= memToVal(gh.visual_class, 4);
/*header->red_mask= memToVal(gh.red_mask, 4);*/
/*header->green_mask= memToVal(gh.green_mask, 4);*/
/*header->blue_mask= memToVal(gh.blue_mask, 4);*/
/*header->bits_per_rgb= memToVal(gh.bits_per_rgb, 4);*/
  header->colormap_entries= memToVal(gh.colormap_entries, 4);
  header->ncolors= memToVal(gh.ncolors, 4);
/*header->window_width= memToVal(gh.window_width, 4);*/
/*header->window_height= memToVal(gh.window_height, 4);*/
/*header->window_x= memToVal(gh.window_x, 4);*/
/*header->window_y= memToVal(gh.window_y, 4);*/
/*header->window_bdrwidth= memToVal(gh.window_bdrwidth, 4);*/

  /* if header size isn't either 100 or 104 bytes, this isn't an XWD file
   */

  if (header->header_size < sizeof(GenericXWDHeader)) {
      printf("header size is wrong.\n");
    return(0);
  }

  for (a = header->header_size - sizeof(GenericXWDHeader); a; a--)
    fgetc(f);

  /* look at a variety of the XImage fields to see if they are sane.  if
   * they are, this passes our tests.
   */

  switch (header->pixmap_format) {
  case XYBitmap:
  case XYPixmap:
  case ZPixmap:
    break;
      printf("unsupported pixmap format\n");
    return(0);
  }

  switch (header->visual_class) {
  case StaticGray:
  case GrayScale:
  case StaticColor:
  case PseudoColor:

    /* the following are unsupported but recognized
     */

  case TrueColor:
  case DirectColor:
    break;
  default:
      printf("unsupported visual class\n");
    return(0);
  }

  return(1);
}

int 
xwd__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ FILE     *f;
  XWDHeader  header;
  int ret;

  if (! (f = fopen(fullname, "r")))
    return(0);
  ret = isXWD(fullname, f, &header);
  fclose(f);
  return(ret);
}

void
loadXYBitmap( self, fullname, f, header )
    struct xwd *self;
    char *fullname;
    FILE *f;
    XWDHeader  header;
{ int    dlinelen;       /* length of scan line in data file */
  int    ilinelen;       /* length of line within image structure */
  int    unit;           /* # of bytes in a bitmap unit */
  int    xoffset;        /* xoffset within line */
  int    xunits;         /* # of units across the whole scan line */
  int    trailer;        /* # of bytes in last bitmap unit on a line */
  int    shift;          /* # of bits to shift last byte set */
  int    x, y;           /* horizontal and vertical counters */
  byte  *line;           /* input scan line */
  byte  *dptr, *iptr;    /* image data pointers */
  unsigned long (*loader)(); /* unit loading function */

  xwd_newBitImage(self, header.pixmap_width, header.pixmap_height);
  ilinelen = (header.pixmap_width / 8) + (header.pixmap_width % 8 ? 1 : 0);
  if (header.bitmap_unit > 7)     /* supposed to be 8, 16, or 32 but appears */
    unit = header.bitmap_unit / 8; /* to often be the byte count.  this will */
  else                            /* accept either. */
    unit = header.bitmap_unit;
  xoffset = (header.xoffset / (unit * 8)) * unit;
  if (header.bytes_per_line)
    dlinelen = header.bytes_per_line;
  else
    dlinelen = unit * header.pixmap_width;
  xunits = (header.pixmap_width / (unit * 8)) +
    (header.pixmap_width % (unit * 8) ? 1 : 0);
  trailer = unit - ((xunits * unit) - ilinelen);
  xunits--; /* we want to use one less than the actual # of units */
  shift = (unit - trailer) * 8;
  if (header.byte_order == MSBFirst)
    loader = doMemToVal;
  else
    loader = doMemToValLSB;
  line = (byte *)malloc(dlinelen);

  for (y = 0; y < header.pixmap_height; y++) {
    if (fread((byte *)line, 1, dlinelen, f) != dlinelen) {
      fprintf(stderr,
	      "%s: Short read while reading data! (returning partial image)\n",
	      fullname);
      free(line);
      return;
    }
    dptr = line + xoffset;
    iptr = xwd_Data(self) + (y * ilinelen);

    if (header.bitmap_bit_order == LSBFirst)
      flipBits(line, dlinelen);

    for (x = 0; x < xunits; x++) {
      valToMem(loader(dptr, unit), iptr, unit);
      dptr += unit;
      iptr += unit;
    }

    /* take care of last unit on this line
     */

    valToMem(loader(dptr, unit) >> shift, iptr, trailer);
  }

  free(line);
}

/* this is a lot like the above function but OR's planes together to
 * build the destination.  1-bit images are handled by XYBitmap.
 */

void
loadXYPixmap( self, fullname, f, header )
    struct xwd *self;
    char *fullname;
    FILE *f;
    XWDHeader header;
{ int plane;
  int    dlinelen;       /* length of scan line in data file */
  int    ilinelen;       /* length of line within image structure */
  int    unit;           /* # of bytes in a bitmap unit */
  int    unitbits;       /* # of bits in a bitmap unit */
  int    unitmask;       /* mask for current bit within current unit */
  int    xoffset;        /* xoffset within data */
  int    xunits;         /* # of units across the whole scan line */
  int    x, x2, y;       /* horizontal and vertical counters */
  int    index;          /* index within image scan line */
  byte  *line;           /* input scan line */
  byte  *dptr, *iptr;    /* image data pointers */
  unsigned long pixvals; /* bits for pixels in this unit */
  unsigned long mask;
  unsigned long (*loader)(); /* unit loading function */

  xwd_newRGBImage(self, header.pixmap_width, header.pixmap_height, header.pixmap_depth);
  ilinelen = xwd_Width(self) * xwd_Pixlen(self);
  if (header.bitmap_unit > 7)     /* supposed to be 8, 16, or 32 but appears */
    unit = header.bitmap_unit / 8; /* to often be the byte count.  this will */
  else                            /* accept either. */
    unit = header.bitmap_unit;
  unitbits = unit * 8;
  unitmask = 1 << (unitbits - 1);
  xoffset = (header.xoffset / unitbits) * unit;
  if (header.bytes_per_line)
    dlinelen = header.bytes_per_line;
  else
    dlinelen = unit * header.pixmap_width;
  xunits = (header.pixmap_width / (unit * 8)) +
    (header.pixmap_width % (unit * 8) ? 1 : 0);
  if (header.byte_order == MSBFirst)
    loader = doMemToVal;
  else
    loader = doMemToValLSB;
  line = (byte *)malloc(dlinelen);

  /* for each plane, load in the bitmap and or it into the image
   */

  for (plane = header.pixmap_depth; plane > 0; plane--) {
    for (y = 0; y < header.pixmap_height; y++) {
      if (fread((byte *)line, 1, dlinelen, f) != dlinelen) {
	fprintf(stderr,
		"%s: Short read while reading data! (returning partial image)\n",
		fullname);
	free(line);
	return;
      }
      dptr = line + xoffset;
      iptr = xwd_Data(self) + (y * ilinelen);
      index = 0;

      if (header.bitmap_bit_order == LSBFirst)
	flipBits(line, dlinelen);
      
      for (x = 0; x < xunits; x++) {
	pixvals = loader(dptr, unit);
	mask = unitmask;
	for (x2 = 0; x2 < unitbits; x2++) {
	  if (pixvals & mask)
	    valToMem(memToVal(iptr + index, xwd_Pixlen(self)) | (1 << plane), iptr + index, xwd_Pixlen(self));
	  index += xwd_Pixlen(self);
	  if (index > ilinelen) {
	    x = xunits;
	    break;
	  }
	  if (! (mask >>= 1))
	    mask = unitmask;
	}
	dptr += unit;
      }
    }
  }

  free(line);
}

/* this loads a ZPixmap format image.  note that this only supports depths
 * of 4, 8, 16, 24, or 32 bits as does Xlib.  You gotta 6-bit image,
 * you gotta problem.  1-bit images are handled by XYBitmap.
 */

void
loadZPixmap( self, fullname, f, header )
    struct xwd *self;
    char *fullname;
    FILE *f;
    XWDHeader header;
{ int    dlinelen;       /* length of scan line in data file */
  int    ilinelen;       /* length of scan line in image file */
  int    depth;          /* depth rounded up to 8-bit value */
  int    pixlen;         /* length of pixel in bytes */
  int    x, y;           /* horizontal and vertical counters */
  byte  *line;           /* input scan line */
  byte  *dptr, *iptr;    /* image data pointers */
  unsigned long pixmask; /* bit mask within pixel */
  unsigned long pixel;   /* pixel we're working on */
  unsigned long (*loader)(); /* unit loading function */

  xwd_newRGBImage(self, header.pixmap_width, header.pixmap_height,
		     header.pixmap_depth);

  /* for pixmaps that aren't simple depths, we round to a depth of 8.  this
   * is what Xlib does, be it right nor not.
   */

  if ((header.pixmap_depth != 4) && (header.pixmap_depth % 8))
    depth = header.pixmap_depth + 8 - (header.pixmap_depth % 8);
  else
    depth = header.pixmap_depth;

  pixmask = 0xffffffff >> (32 - header.pixmap_depth);
  pixlen = xwd_Pixlen(self);
  if (header.bytes_per_line)
    dlinelen = header.bytes_per_line;
  else
    dlinelen = depth * header.pixmap_width;
  ilinelen = xwd_Width(self) * xwd_Pixlen(self);
  if (header.byte_order == MSBFirst)
    loader = doMemToVal;
  else
    loader = doMemToValLSB;

  line = (byte *)malloc(dlinelen);

  for (y = 0; y < header.pixmap_height; y++) {
    if (fread((byte *)line, 1, dlinelen, f) != dlinelen) {
      fprintf(stderr,
	      "%s: Short read while reading data! (returning partial image)\n",
	      fullname);
      free(line);
      return;
    }
    dptr = line;
    iptr = xwd_Data(self) + (y * ilinelen);

    if (header.bitmap_bit_order == LSBFirst)
      flipBits(line, dlinelen);

    for (x = 0; x < header.pixmap_width; x++) {
      switch (depth) {
      case 4:
	pixel = memToVal(dptr, 1);
	if (header.bitmap_bit_order == LSBFirst) { /* nybbles are reversed */
	  valToMem(pixel & 0xf, iptr++, 1);        /* by flipBits */
	  if (++x < header.pixmap_width)
	    valToMem(pixel >> 4, iptr++, 1);
	}
	else {
	  valToMem(pixel >> 4, iptr++, 1);
	  if (++x < header.pixmap_width)
	    valToMem(pixel & 0xf, iptr++, 1);
	}
	break;
      case 8:
	pixel = ((unsigned long)*(dptr++)) & pixmask; /* loader isn't needed */
	valToMem(pixel, iptr++, 1);
	break;
      case 16:
      case 24:
      case 32:
	valToMem(loader(dptr, pixlen) & pixmask, iptr, pixlen);
	dptr += pixlen;
	iptr += pixlen;
	break;
      default:
	fprintf(stderr,
		"%s: ZPixmaps of depth %d are not supported (sorry).\n",
		fullname, header.pixmap_depth);
	free(line);
	return;
      }
    }
  }

  free(line);
}

int
xwd__Load( self, fullname, fp )
    struct xwd *self;
    char *fullname;
    FILE *fp;
{ FILE      *f;
  XWDHeader  header;
  int        cmaplen;
  XWDColor  *cmap;
  int        a;

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open xwd file %s.\n", fullname);
	  return(-1);
      }
  }
  if (! isXWD(fullname, f, &header)) {
    fclose(f);
    printf("image not an Xwd!\n");
    return(-1);
  }

  /* complain if we don't understand the visual
   */

  switch (header.visual_class) {
  case StaticGray:
  case GrayScale:
  case StaticColor:
  case PseudoColor:
    break;
  case TrueColor:
  case DirectColor:
    fprintf(stderr, "Unsupported visual type, sorry\n");
    exit(1);
  }

  if ((header.pixmap_width == 0) || (header.pixmap_height == 0)) {
    fprintf(stderr, "Zero-size image -- header might be corrupted.\n");
    exit(1);
  }

  /* read in colormap
   */

  cmaplen = header.ncolors * sizeof(XWDColor);
  cmap = (XWDColor *)malloc(cmaplen);
  if (fread((byte *)cmap, 1, cmaplen, f) != cmaplen) {
    fprintf(stderr, "Short read in colormap!\n");
    return(-1);
  }

  /* any depth 1 image is basically a XYBitmap so we fake it here
   */

  if (header.pixmap_depth == 1)
    header.pixmap_format= XYBitmap;

  /* we can't realistically support images of more than depth 16 with the
   * RGB image format so this nukes them for the time being.
   */

  if (header.pixmap_depth > 16) {
    fprintf(stderr,
	    "%s: Sorry, cannot load images deeper than 16 bits (yet)\n",
	    fullname);
    return(-1);
  }

  switch (header.pixmap_format) {
  case XYBitmap:
    loadXYBitmap(self, fullname, f, header);
    fclose(f);
    return(0); /* we used to goof w/ the cmap but we gave up */
  case XYPixmap:
    loadXYPixmap(self, fullname, f, header);
    break;
  case ZPixmap:
    loadZPixmap(self, fullname, f, header);
    break;
  }
  fclose(f);

  /* load the colormap.  we should probably use pixval instead of the color
   * number but the value seems pretty system-dependent and most colormaps
   * seem to be just dumped in order.
   */

  xwd_RGBUsed(self) = header.ncolors;
  for (a = 0; a < header.ncolors; a++) {
      int i = memToVal(cmap[a].pixel, 4);

      xwd_RedPixel(self, i) = memToVal(cmap[a].red, 2);
      xwd_GreenPixel(self, i) = memToVal(cmap[a].green, 2);
      xwd_BluePixel(self, i) = memToVal(cmap[a].blue, 2);
  }

  free((byte *)cmap);
  return(0);
}

long
xwd__Read( self, file, id )
    struct xwd *self;
    FILE *file;
    long id;
{
    if(xwd_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
xwd__Write( self, file, writeID, level )
    struct xwd *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
xwd__WriteNative( self, file, filename )
    struct xwd *self;
    FILE *file;
    char *filename;
{
return(0);
}
