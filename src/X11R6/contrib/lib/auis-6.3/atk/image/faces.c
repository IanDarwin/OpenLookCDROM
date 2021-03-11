/* faces.c - class description for interface from Bennet Yee's face format to image */
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
 * faces format image loader
 *
 * jim frost 07.06.89
 *
 * Copyright 1989 Jim Frost.
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
#include <faces.eh>

/* SUPPRESS 560 */

static short HexTable[256];  /* conversion value */
static unsigned int Initialized = 0; /* easier to fill in at run time */

#define HEXIGNORE -1
#define HEXBAD    -2

/* build a hex digit value table with the bits inverted
 */

static void initHexTable()
{ int a;

  for (a = 0; a < 256; a++)
    HexTable[a] = HEXBAD;

  HexTable['0']= 0x0;
  HexTable['1']= 0x1;
  HexTable['2']= 0x2;
  HexTable['3']= 0x3;
  HexTable['4']= 0x4;
  HexTable['5']= 0x5;
  HexTable['6']= 0x6;
  HexTable['7']= 0x7;
  HexTable['8']= 0x8;
  HexTable['9']= 0x9;
  HexTable['A']= 0xa; HexTable['a']= HexTable['A'];
  HexTable['B']= 0xb; HexTable['b']= HexTable['B'];
  HexTable['C']= 0xc; HexTable['c']= HexTable['C'];
  HexTable['D']= 0xd; HexTable['d']= HexTable['D'];
  HexTable['E']= 0xe; HexTable['e']= HexTable['E'];
  HexTable['F']= 0xf; HexTable['f']= HexTable['F'];
  HexTable['\r']= HEXIGNORE;
  HexTable['\n']= HEXIGNORE;
  HexTable['\t']= HEXIGNORE;
  HexTable[' ']= HEXIGNORE;

  Initialized = 1;
}

/* read a hex value and return its value
 */

static int nextInt(f, len)
     FILE        *f;
     unsigned int  len;
{ int c;
  int value= 0;
  int count;

  len <<= 1;
  for (count = 0; count < len;) {
    c = fgetc(f);
    if (c == EOF)
      return(-1);
    else {
      c = HexTable[c & 0xff];
      switch(c) {
      case HEXIGNORE:
	break;
      case HEXBAD:
	return(-1);
      default:
	value = (value << 4) + c;
	count++;
      }
    }
  }
  return(value);
}

int
faces__Load( faces, fullname, fp )
    struct faces *faces;
    char *fullname;
    FILE *fp;
{ 
  FILE         *f;
  char          fname[BUFSIZ];
  char          lname[BUFSIZ];
  char          buf[BUFSIZ];
  unsigned int  w, h, d, iw, ih, id;
  unsigned int  x, y;
  int           value;
  unsigned int  linelen;
  byte         *lineptr, *dataptr;

  if (!Initialized)
    initHexTable();

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open faces file %s.\n", fullname);
	  return(-1);
      }
  }

  w = h = d = 0;
  iw = ih = id = 0;
  *fname = *lname = (char) 0;
  while (fgets((byte *)buf, BUFSIZ - 1, f)) {
    if (! strcmp(buf, "\n"))
      break;
    if (!strncmp(buf, "FirstName:", 10))
      strcpy(fname, buf + 11);
    else if (!strncmp(buf, "LastName:", 9))
      strcpy(lname, buf + 10);
    else if (!strncmp(buf, "Image:", 6)) {
      if (sscanf(buf + 7, "%d%d%d", &iw, &ih, &id) != 3) {
	printf("%s: Bad Faces Project image\n", fullname);
	exit(1);
      }
    }
    else if (!strncmp(buf, "PicData:", 8)) {
      if (sscanf(buf + 9, "%d%d%d", &w, &h, &d) != 3) {
	printf("%s: Bad Faces Project image\n", fullname);
	exit(1);
      }
    }
  }
  if (!w || !h || !d) {
      if(!id || !ih || !id) {
	  fclose(f);
	  printf("couldn't get dimensions\n");
	  return(-1);
      }
      else {
	  w = iw;
	  h = ih;
	  d = id;
      }
  }

  faces_newRGBImage(faces, w, h, d);
  fname[strlen(fname) - 1] = ' ';
  strcat(fname, lname);
  fname[strlen(fname) - 1] = '\0';

  /* image is greyscale; build RGB map accordingly
   */

  for (x = 0; x < faces_RGBSize(faces); x++)
      faces_RedPixel(faces, x) = 
	faces_GreenPixel(faces, x) = 
	faces_BluePixel(faces, x) = (65536 / faces_RGBSize(faces)) * x;
  faces_RGBUsed(faces) = faces_RGBSize(faces);

  /* read in image data
   */

  linelen = w * faces_Pixlen(faces);
  lineptr = faces_Data(faces) + (h * linelen);
  for (y = 0; y < h; y++) {
    lineptr -= linelen;
    dataptr = lineptr;
    for (x = 0; x < w; x++) {
      if ((value = nextInt(f, faces_Pixlen(faces))) < 0) {
	printf("%s: Bad Faces Project image data\n", fullname);
	exit(1);
      }
      *(dataptr++)= value;
    }
  }
  fclose(f);
  return(0);
}

int 
faces__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ struct faces *faces = faces_New();

  if (faces_Load(faces, fullname, NULL) == 0) {
    faces_Destroy(faces);
    return(1);
  }
  return(0);
}

long
faces__Read( self, file, id )
    struct faces *self;
    FILE *file;
    long id;
{
    if(faces_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
faces__Write( self, file, writeID, level )
    struct faces *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
faces__WriteNative( self, file, filename )
    struct faces *self;
    FILE *file;
    char *filename;
{
return(0);
}
