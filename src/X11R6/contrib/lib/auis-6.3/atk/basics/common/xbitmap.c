/* xbitmap.c - class description for interface from Xbitmap format to image */
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

/* xbitmap.c:
 *
 * at one time this was XRdBitF.c.  it bears very little resemblence to it
 * now.  that was ugly code.  this is cleaner, faster, and more reliable
 * in most cases.
 *
 * jim frost 10.06.89
 *
 * Copyright, 1987, Massachusetts Institute of Technology
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
#ifndef _MIT_COPYRIGHT_
/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef __SABER__
static char *MitCopyright=
  "Copyright 1989 Massachusetts Institute of Technology";
#endif
#define _MIT_COPYRIGHT_
#endif


#include <andrewos.h>
#include <image.ih>
#include <ctype.h>
#include <xbitmap.eh>

/* SUPPRESS 560 */

#define MAX_SIZE 255

static short        HexTable[256];  /* conversion value */
static unsigned int Initialized= 0; /* easier to fill in at run time */

#define b0000 0 /* things make more sense if you see them by bit */
#define b0001 1
#define b0010 2
#define b0011 3
#define b0100 4
#define b0101 5
#define b0110 6
#define b0111 7
#define b1000 8
#define b1001 9
#define b1010 10
#define b1011 11
#define b1100 12
#define b1101 13
#define b1110 14
#define b1111 15

#define HEXSTART -1
#define HEXDELIM -2
#define HEXBAD   -3

/* build a hex digit value table with the bits inverted
 */

static void initHexTable()
{ int a;

  for (a= 0; a < 256; a++)
    HexTable[a]= HEXBAD;

  HexTable['0']= b0000;
  HexTable['1']= b1000;
  HexTable['2']= b0100;
  HexTable['3']= b1100;
  HexTable['4']= b0010;
  HexTable['5']= b1010;
  HexTable['6']= b0110;
  HexTable['7']= b1110;
  HexTable['8']= b0001;
  HexTable['9']= b1001;
  HexTable['A']= b0101; HexTable['a']= HexTable['A'];
  HexTable['B']= b1101; HexTable['b']= HexTable['B'];
  HexTable['C']= b0011; HexTable['c']= HexTable['C'];
  HexTable['D']= b1011; HexTable['d']= HexTable['D'];
  HexTable['E']= b0111; HexTable['e']= HexTable['E'];
  HexTable['F']= b1111; HexTable['f']= HexTable['F'];
  HexTable['x']= HEXSTART;
  HexTable['\r']= HEXDELIM;
  HexTable['\n']= HEXDELIM;
  HexTable['\t']= HEXDELIM;
  HexTable[' ']= HEXDELIM;
  HexTable[',']= HEXDELIM;
  HexTable['}']= HEXDELIM;

  Initialized = 1;
}

/* read a hex value and return its value
 */

static int nextInt(f)
     FILE *f;
{ int c;
  int value= 0;
  int shift= 0;
    
  for (;;) {
    c= fgetc(f);
    if (c == EOF)
      return(-1);
    else {
      c= HexTable[c & 0xff];
      switch(c) {
	  case HEXSTART:
	      shift = 0; /* reset shift counter */
	      break;
	  case HEXDELIM:
	      if (shift)
		  return(value);
	      break;
	  case HEXBAD:
	      return(-1);
	  default:
	      value += (c << shift);
	      shift += 4;
      }
    }
  }
}

int
xbitmap__Load( self, fullname, fp)
    struct xbitmap *self;
    char *fullname;
    FILE *fp;
{ FILE         *f;
  char          line[MAX_SIZE];
  char          name_and_type[MAX_SIZE];
  char         *type;
  unsigned int  value;
  int           v10p;
  unsigned int  linelen, dlinelen;
  unsigned int  x, y;
  unsigned int  w = 0, h = 0;
  byte         *dataptr;

  if (!Initialized)
    initHexTable();

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open xbitmap file %s.\n", fullname);
	  return(-1);
      }
  }

/* get width/height values */

  while (fgets((byte *)line, MAX_SIZE, f)) {
      if (strlen(line) == MAX_SIZE-1) {
	  fclose(f);
	  return(-1);
      }

/* width/height/hot_x/hot_y scanning
 */

    if (sscanf(line,"#define %s %u\n", name_and_type, &value) == 2) {
      if (!(type = rindex(name_and_type, '_')))
	  type = name_and_type;
      else
	  type++;

      if (!strncmp("width", type, 5))
	  w = value;
      if (!strncmp("height", type, 6))
	  h = value;
    }

/* if start of data, determine if it's X10 or X11 data and break
 */

    if (sscanf(line, "static short %s = {\n", name_and_type) == 1) {
	v10p = 1;
	break;
    }
    if ((sscanf(line,"static unsigned char %s = {\n", name_and_type) == 1) ||
	(sscanf(line, "static char %s = {\n", name_and_type) == 1)) {
	v10p = 0;
	break;
    }
  }

  if (!w || !h) {
      fclose(f);
      return(-1);
  }
  xbitmap_newBitImage(self, w, h);

  /* get title of bitmap if any
   */

  if ((type = rindex(name_and_type, '_')) && !strncmp("bits[]", type + 1, 6))
      *type= '\0';
    
/* read bitmap data
 */

  linelen = (w / 8) + (w % 8 ? 1 : 0); /* internal line length */
  if (v10p) {
      dlinelen = (w / 8) + (w % 16 ? 2 : 0);
      dataptr = xbitmap_Data(self);
      for (y = 0; y < h; y++) {
	  for (x = 0; x < dlinelen; x++) {
	      if ((value = nextInt(f)) < 0) {
		  xbitmap_Reset(self);
		  fclose(f);
		  return(-1);
	      }
	      *(dataptr++) = value >> 8;
	      if (++x < linelen)
		  *(dataptr++) = value & 0xff;
	  }
      }
  }
  else {
      dataptr = xbitmap_Data(self);
      for (y = 0; y < h; y++)
	  for (x = 0; x < linelen; x++) {
	      if ((value = nextInt(f)) < 0) {
		  fclose(f);
		  return(-1);
	      }
	      *(dataptr++) = (byte) value;
	  }
  }

  fclose(f);
  return(0);
}

/* this is the easiest way to do this.  it's not likely we'll have mondo
 * x bitmaps anyway given their size
 */

int 
xbitmap__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ struct xbitmap *self;

  if (xbitmap_Load(self = xbitmap_New(), fullname, NULL) == 0) {
    xbitmap_Destroy(self);
    return(1);
  }
  return(0);
}

long
xbitmap__Read( self, file, id )
    struct xbitmap *self;
    FILE *file;
    long id;
{
    if(xbitmap_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
xbitmap__Write( self, file, writeID, level )
    struct xbitmap *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
xbitmap__WriteNative( self, file, filename )
    struct xbitmap *self;
    FILE *file;
    char *filename;
{
return(0);
}
