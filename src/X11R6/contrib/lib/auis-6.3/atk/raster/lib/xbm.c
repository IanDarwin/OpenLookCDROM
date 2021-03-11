/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
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
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/lib/RCS/xbm.c,v 1.12 1993/05/05 19:49:43 susan Exp $";
#endif


/*  
  Enable import and export of X Bitmap
  paul@athena.mit.edu  5/90		
  "...it becomes natural, like a third sense." -- Homer Simpson 
  most of this code was stolen from the old 'x2wm' and wm2x' programs
*/		

#include <andrewos.h>
#include <class.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#include <netinet/in.h>	    
#include <pixelimg.ih>	
#include <dataobj.ih>
#include <xbm.eh>

#if !defined(vax)
/*
 * Macros for number representation conversion.
 */
#define	ntohl(x)	(x)
#define	ntohs(x)	(x)
#define	htonl(x)	(x)
#define	htons(x)	(x)
#endif

/* 
 Here is a sample X  bitmap file.  The last two defines -- for the "hot spot" for cursor tracking -- may be omitted in some files:

#define smiley_width 16
#define smiley_height 16
#define smiley_x_hot 8
#define smiley_y_hot 7
static char smiley_bits[] = {
   0xc0, 0x07, 0x30, 0x18, 0x08, 0x20, 0x04, 0x40, 0x44, 0x44, 0x02, 0x80,
   0x02, 0x80, 0x02, 0x80, 0x22, 0x88, 0x62, 0x8c, 0xc4, 0x47, 0x04, 0x40,
   0x08, 0x20, 0x30, 0x18, 0xc0, 0x07, 0x00, 0x00};

The following table is for bit-order conversion -- the low-order bits of the bitmap bytes are actually the leftmost pixels */

static char flipbits[256] = {
    0x00,0x80,0x40,0xc0,0x20,0xa0,0x60,0xe0,0x10,0x90,0x50,0xd0,0x30,0xb0,0x70,0xf0,
    0x08,0x88,0x48,0xc8,0x28,0xa8,0x68,0xe8,0x18,0x98,0x58,0xd8,0x38,0xb8,0x78,0xf8,
    0x04,0x84,0x44,0xc4,0x24,0xa4,0x64,0xe4,0x14,0x94,0x54,0xd4,0x34,0xb4,0x74,0xf4,
    0x0c,0x8c,0x4c,0xcc,0x2c,0xac,0x6c,0xec,0x1c,0x9c,0x5c,0xdc,0x3c,0xbc,0x7c,0xfc,
    0x02,0x82,0x42,0xc2,0x22,0xa2,0x62,0xe2,0x12,0x92,0x52,0xd2,0x32,0xb2,0x72,0xf2,
    0x0a,0x8a,0x4a,0xca,0x2a,0xaa,0x6a,0xea,0x1a,0x9a,0x5a,0xda,0x3a,0xba,0x7a,0xfa,
    0x06,0x86,0x46,0xc6,0x26,0xa6,0x66,0xe6,0x16,0x96,0x56,0xd6,0x36,0xb6,0x76,0xf6,
    0x0e,0x8e,0x4e,0xce,0x2e,0xae,0x6e,0xee,0x1e,0x9e,0x5e,0xde,0x3e,0xbe,0x7e,0xfe,
    0x01,0x81,0x41,0xc1,0x21,0xa1,0x61,0xe1,0x11,0x91,0x51,0xd1,0x31,0xb1,0x71,0xf1,
    0x09,0x89,0x49,0xc9,0x29,0xa9,0x69,0xe9,0x19,0x99,0x59,0xd9,0x39,0xb9,0x79,0xf9,
    0x05,0x85,0x45,0xc5,0x25,0xa5,0x65,0xe5,0x15,0x95,0x55,0xd5,0x35,0xb5,0x75,0xf5,
    0x0d,0x8d,0x4d,0xcd,0x2d,0xad,0x6d,0xed,0x1d,0x9d,0x5d,0xdd,0x3d,0xbd,0x7d,0xfd,
    0x03,0x83,0x43,0xc3,0x23,0xa3,0x63,0xe3,0x13,0x93,0x53,0xd3,0x33,0xb3,0x73,0xf3,
    0x0b,0x8b,0x4b,0xcb,0x2b,0xab,0x6b,0xeb,0x1b,0x9b,0x5b,0xdb,0x3b,0xbb,0x7b,0xfb,
    0x07,0x87,0x47,0xc7,0x27,0xa7,0x67,0xe7,0x17,0x97,0x57,0xd7,0x37,0xb7,0x77,0xf7,
    0x0f,0x8f,0x4f,0xcf,0x2f,0xaf,0x6f,0xef,0x1f,0x9f,0x5f,0xdf,0x3f,0xbf,0x7f,0xff,
};

static short hexTable[256];		/* conversion value */
static boolean initialized = FALSE;	/* easier to fill in at run time */
  
/* from X11R5 XRdBitF.c */
static NextInt (fstream)
    FILE *fstream;
{
    int	ch;
    int	value = 0;
    int gotone = 0;
    int done = 0;
    
    /* loop, accumulate hex value until find delimiter  */
    /* skip any initial delimiters found in read stream */

    while (!done) {
	ch = getc(fstream);
	if (ch == EOF) {
	    value	= -1;
	    done++;
	} else {
	    /* trim high bits, check type and accumulate */
	    ch &= 0xff;
	    if (isascii(ch) && isxdigit(ch)) {
		value = (value << 4) + hexTable[ch];
		gotone++;
	    } else if ((hexTable[ch]) < 0 && gotone)
	      done++;
	}
    }
    return value;
}

/* from X11R5 XRdBitF.c */
static void initHexTable()
{
    /*
     * We build the table at run time for several reasons:
     *
     *     1.  portable to non-ASCII machines.
     *     2.  still reentrant since we set the init flag after setting table.
     *     3.  easier to extend.
     *     4.  less prone to bugs.
     */
    hexTable['0'] = 0;	hexTable['1'] = 1;
    hexTable['2'] = 2;	hexTable['3'] = 3;
    hexTable['4'] = 4;	hexTable['5'] = 5;
    hexTable['6'] = 6;	hexTable['7'] = 7;
    hexTable['8'] = 8;	hexTable['9'] = 9;
    hexTable['A'] = 10;	hexTable['B'] = 11;
    hexTable['C'] = 12;	hexTable['D'] = 13;
    hexTable['E'] = 14;	hexTable['F'] = 15;
    hexTable['a'] = 10;	hexTable['b'] = 11;
    hexTable['c'] = 12;	hexTable['d'] = 13;
    hexTable['e'] = 14;	hexTable['f'] = 15;

    /* delimiters of significance are flagged w/ negative value */
    hexTable[' '] = -1;	hexTable[','] = -1;
    hexTable['}'] = -1;	hexTable['\n'] = -1;
    hexTable['\t'] = -1;
	
    initialized = TRUE;
}


long xbm__ReadImage(ClassID, file, pix)

struct classhdr *ClassID;
FILE *file;		
struct pixelimage *pix;	
{
unsigned char *location;
long width, height, byte, value,bytewidth, i;
char name[64], bits[64], *t;

    if (initialized == FALSE) initHexTable();

/* get width (in pixels) and height fields from top of bitmap file */
    for (i=0; i<2; i++) 
    {
	if (fscanf(file," #define %s %d ", name, &value) == 2)
	{

	    if ((t = rindex(name, '_')) == 0)
		t = name;
	    else
		t++;
	    if (!strcmp("width", t))
		width = value;
	    if (!strcmp("height", t))
		height = value;
	} 
	else
	{
	    return dataobject_BADFORMAT;	
	}
    }


   /* determine width in bytes, and initialize pixelimage */
    bytewidth = (width + 7) / 8;
    pixelimage_Resize(pix, bytewidth * 8, height);
    location = pixelimage_GetBitsPtr(pix);

    /* skip past hot spot defines  and 'static char ...' line */
    while(fscanf(file, "#define %s %s \n", bits, bits) == 2)
	;

     while(fgetc(file) != '\n')
	  ;
     /* read each byte listed, flipping its bit order via table */

   for (i=0; i < bytewidth * height; i++)
   {
       if ((byte = NextInt(file)) == EOF)
       {
	   return dataobject_BADFORMAT;	
       }
       *location++ = flipbits[(unsigned char)byte];

       /* pixelimage_create rounds row widths to short-word (16-bit) multiples for more efficient handling. Bitmap files only round to nearest byte (8 bits), so if the row ends somewhere in the first 8 bits of a word, pad that row out to the next 16-bit multiple */

       if ( i % bytewidth == (bytewidth - 1) &&
	    0 < width % 16 && 8 >= width % 16)
	   *location++ = 0x00; 
   }

   /* Trim off this whole-word padding on right-hand edge of picture */
   pixelimage_Resize(pix, width, height);
   /* Finish */
   pixelimage_NotifyObservers(pix, pixelimage_DATACHANGED);
   return dataobject_NOREADERROR;
}

void xbm__WriteImage(ClassID, file, pix, sub)

struct classhdr *ClassID;
register FILE *file;		
register struct pixelimage *pix;
register struct rectangle *sub;
{

    char c, *title = "raster";
    unsigned char *location;
    long left, top, width, height, nbytes, bytewidth, i;

    /* Get Image and detemine size of rectangle to be written */

    location = pixelimage_GetBitsPtr(pix);
    rectangle_GetRectSize(sub, &left, &top, &width, &height);

    /* Determine size of rows and size of image in bytes */

    bytewidth = (width + 7) / 8;
    nbytes = bytewidth * height;

    /* Write out X Bitmap header, with the arbitrary title 'raster'.
      Note that no hot spot is defined. */
    fprintf(file, "#define %s_width %d\n", title, width);
    fprintf(file, "#define %s_height %d\n", title, height);
    fprintf(file, "static char %s_bits[] = {",title);


    /* Write out the image, in one-byte chunks, padded to nearest whole byte, with appropriate punctuation per above example */

    for (i = 0; i < nbytes; i++)
    {
	if ((i % 12) == 0)
	    fprintf(file,"\n   ");
	c = flipbits[(unsigned char) *location++];
	fprintf(file, "0x%02x", (unsigned char) c);
	if (i < nbytes - 1)
	    fprintf(file,", ");


	/*pixelimage_create rounds row widths to short-word multiples (16 bits) for more efficient handling.
	    Bitmap files only round to nearest byte (8 bits), so
	      if the row ends somewhere in the first 8 bits of a word, the bitmap file cannot have a second empty byte to pad that word out -- we must skip the one at  the end of the row or the bitmap file's image will be skewed right by one pixel per subsequent line */

	if ( i % bytewidth == (bytewidth - 1) &&
	    0 < width % 16 && 8 >= width % 16)
	    *location++;
    }
    fprintf(file,"};");
    fprintf(file,"\n");
}
