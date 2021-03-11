/* xpixmap.c - class description for interface from Xpixmap format to image */
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

/* xpixmap.c:
 *
 * XPixMap format file read and identify routines.  these can handle any
 * "format 1" XPixmap file with up to BUFSIZ - 1 chars per pixel.  it's
 * not nearly as picky as it might be.
 *
 * unlike most image loading routines, this is X specific since it
 * requires X color name parsing.  to handle this we have global X
 * variables for display and screen.  it's ugly but it keeps the rest
 * of the image routines clean.
 *
 * Copyright 1989 Jim Frost.  See included file "copyright.h" for complete
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
#include <xpixmap.eh>

/* SUPPRESS 530 */
/* SUPPRESS 560 */

extern Display *Disp; /* X display, null if in "identify" mode */
extern int      Scrn; /* X screen number */

#define XPM_FORMAT 1

static void corrupted(fullname, f)
     char  *fullname;
     FILE *f;
{
  fclose(f);
  printf("%s: X Pixmap file is corrupted\n", fullname);
  return(-1);
}

int
xpixmap__Load( self, fullname, fp )
    struct xpixmap *self;
    char *fullname;
    FILE *fp;
{ FILE          *f;
  char           buf[BUFSIZ];
  char           what[BUFSIZ];
  char          *p;
  char          *imagetitle;
  unsigned int   value;
  unsigned int   format;  /* image format */
  unsigned int   w, h;    /* image dimensions */
  unsigned int   cpp;     /* chars per pixel */
  unsigned int   ncolors; /* number of colors */
  unsigned int   depth;   /* depth of image */
  char         **ctable;  /* color table */
  XColor         xcolor;
  unsigned int   a, b, x, y;
  int            c;
  byte          *dptr;

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open xpixmap file %s.\n", fullname);
	  return(-1);
      }
  }

  /* read #defines until we have all that are necessary or until we
   * get an error
   */

  format= w= h= ncolors= 0;
  for (;;) {
    if (! fgets((byte *)buf, BUFSIZ - 1, f)) {
      fclose(f);
      return(-1);
    }
    if (!strncmp(buf, "#define", 7)) {
      if (sscanf(buf, "#define %s %d", what, &value) != 2) {
	fclose(f);
	return(-1);
      }
      if (! (p= rindex(what, '_')))
	p= what;
      else
	p++;
      if (!strcmp(p, "format"))
	format= value;
      else if (!strcmp(p, "width"))
	w= value;
      else if (!strcmp(p, "height"))
	h= value;
      else if (!strcmp(p, "ncolors"))
	ncolors= value;

      /* this one is ugly
       */

      else if (!strcmp(p, "pixel")) { /* this isn't pretty but it works */
	if (p == what)
	  continue;
	*(--p)= '\0';
	if (!(p= rindex(what, '_')) || (p == what) || strcmp(++p, "per"))
	  continue;
	*(--p)= '\0';
	if (!(p= rindex(what, '_')))
	  p= what;
	if (strcmp(++p, "chars"))
	  continue;
	cpp= value;
      }
    }
    else if ((sscanf(buf, "static char * %s", what) == 1) &&
	     (p= rindex(what, '_')) && !strcmp(++p, "colors[]"))
      break;
  }

  if ((format != XPM_FORMAT) || !w || !h || !ncolors || !cpp) {
    fclose(f);
    return(-1);
  }

  for (depth= 1, value= 2; value < ncolors; value <<= 1, depth++)
    ;
  xpixmap_newRGBImage(self, w, h, depth);
  xpixmap_RGBUsed(self)= ncolors;

  /* read the colors array and build the image colormap
   */

  ctable= (char **)malloc(sizeof(char *) * ncolors);
  xcolor.flags= DoRed | DoGreen | DoBlue;
  for (a= 0; a < ncolors; a++) {
 
    /* read pixel value
     */

    *(ctable + a)= (char *)malloc(cpp);
    while (((c= fgetc(f)) != EOF) && (c != '"'))
      ;
    if (c == EOF) {
	corrupted(fullname, f);
	return(-1);
    }
    for (b= 0; b < cpp; b++) {
      if ((c= fgetc(f)) == '\\')
	c= fgetc(f);
     if (c == EOF) {
	corrupted(fullname, f);
	return(-1);
     }
      *(*(ctable + a) + b)= (char)c;
    }
    if (((c= fgetc(f)) == EOF) || (c != '"')) {
      corrupted(fullname, f);
      return(-1);
    }

    /* read color definition and parse it
     */

    while (((c= fgetc(f)) != EOF) && (c != '"'))
      ;
    if (c == EOF) {
      corrupted(fullname, f);
      return(-1);
    }
    for (b= 0; ((c= fgetc(f)) != EOF) && (c != '"'); b++) {
      if (c == '\\')
	c= fgetc(f);
      if (c == EOF) {
	  corrupted(fullname, f);
	  return(-1);
      }
      buf[b]= (char)c;
    }
    buf[b]= '\0';

    if (Disp) {
      if (! XParseColor(Disp, DefaultColormap(Disp, Scrn), buf, &xcolor)) {
	printf("%s: %s: Bad color name\n", fullname, buf);
	return(-1);
      }
      xpixmap_RedPixel(self, a) = xcolor.red;
      xpixmap_GreenPixel(self, a) = xcolor.green;
      xpixmap_BluePixel(self, a) = xcolor.blue;
    }
  }

  for (;;) {
    if (! fgets((byte *)buf, BUFSIZ - 1, f)) {
      corrupted(fullname, f);
      return(-1);
    }
    if (sscanf(buf, "static char * %s", what) == 1)
      break;
  }

  if (p= rindex(what, '_'))
    p++;
  else
    p= what;
  if (strcmp(p, "pixels[]")) {
    corrupted(fullname, f);
    return(-1);
  }

  /* read in image data
   */

  dptr = xpixmap_Data(self);
  for (y= 0; y < h; y++) {
    while (((c= fgetc(f)) != EOF) && (c != '"'))
      ;
    for (x= 0; x < w; x++) {
      for (a= 0; a < cpp; a++) {
	if ((c= fgetc(f)) == '\\')
	  c= fgetc(f);
	if (c == EOF) {
	  corrupted(fullname, f);
	  return(-1);
	}
	buf[a]= (char)c;
      }
      for (a= 0; a < ncolors; a++)
	if (!strncmp(*(ctable + a), buf, cpp))
	  break;
      if (a == ncolors) { /* major uncool */
	fclose(f);
	printf("%s: Pixel data doesn't match color data\n", fullname);
	return(-1);
      }
      valToMem((unsigned long)a, dptr, xpixmap_Pixlen(self));
      dptr += xpixmap_Pixlen(self);
    }
    if ((c= fgetc(f)) != '"') {
      corrupted(fullname, f);
      return(-1);
    }
  }
  fclose(f);
  return(0);
}

int xpixmapIdent(fullname)
     char *fullname;
{ struct xpixmap *self = xpixmap_New();

  if (xpixmap_Load(self, fullname, NULL) == 0) {
    xpixmap_Destroy(self);
    return(1);
  }
  return(0);
}

long
xpixmap__Read( self, file, id )
    struct xpixmap *self;
    FILE *file;
    long id;
{
    if(xpixmap_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
xpixmap__Write( self, file, writeID, level )
    struct xpixmap *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
xpixmap__WriteNative( self, file, filename )
    struct xpixmap *self;
    FILE *file;
    char *filename;
{
return(0);
}
