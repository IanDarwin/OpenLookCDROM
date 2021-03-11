/* pcx.ch - class description for interface from PCX format to image */
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
** pcx.c - load a ZSoft PC Paintbrush (PCX) file for use inside xloadimage
**
**	Tim Northrup <tim@BRS.Com>
**	Adapted from code by Jef Poskanzer (see Copyright below).
**
**	Version 0.1 --  4/25/91 -- Initial cut
**
**  Copyright (c) 1991 Tim Northrup
**	(see file "tgncpyrght.h" for complete copyright information)
 */
#ifndef _TGN_COPYRIGHT_

/****
  Copyright 1991 Tim Northrup

  Permission to use, copy, modify, distribute, and sell this
  software and its documentation for any purpose is hereby granted
  without fee, provided that the above copyright notice appear in
  all copies and that both that copyright notice and this
  permission notice appear in supporting documentation. The
  author makes no representations about the suitability of this
  software for any purpose. It is provided "as is" without express
  or implied warranty.

  THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
  OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
****/

#ifndef __SABER__
static char *TGNCopyright = "Copyright (C) 1991 Tim Northrup";
#endif
#define _TGN_COPYRIGHT_
#endif

/*
** Copyright (C) 1988 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** This program (pcxtopbm) is based on the pcx2rf program by:
**   Mike Macgirvin
**   Stanford Relativity Gyro Program GP-B
**   Stanford, Calif. 94503
**   ARPA: mike@relgyro.stanford.edu
*/

#include <andrewos.h>
#include <stdio.h>
#include <image.ih>
#include <pcx.eh>

#define PCX_MAGIC 0x0a			/* first byte in a PCX image file */

static void PCX_LoadImage();		/* Routine to load a PCX file */


/*
**  pcxIdent
**
**	Identify passed file as a PC Paintbrush image or not
**
**	Returns 1 if file is a PCX file, 0 otherwise
*/

int 
pcx__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{
    FILE *f;
    int ret;
    int xmin;
    int xmax;
    int ymin;
    int ymax;
    unsigned char pcxhd[128];

    ret = 0;

    if(!(f = fopen(fullname, "r")))
	return(0);

    /* Read .pcx header. */
    if (fread(pcxhd, sizeof(unsigned char), 128, f) == 128) {
	if (pcxhd[0] == PCX_MAGIC) {
	    /* Calculate raster header and swap bytes. */
	    xmin = pcxhd[4] + ( 256 * pcxhd[5] );
	    ymin = pcxhd[6] + ( 256 * pcxhd[7] );
	    xmax = pcxhd[8] + ( 256 * pcxhd[9] );
	    ymax = pcxhd[10] + ( 256 * pcxhd[11] );
	    xmax = xmax - xmin + 1;
	    ymax = ymax - ymin + 1;
	    ret = 1;
	}
    }
    fclose(f);
    return(ret);
}


/*
**  pcxLoad
**
**	Load PCX Paintbrush file into an Image structure.
**
**	Returns pointer to allocated struct if successful, NULL otherwise
*/

int
pcx__Load( pcx, fullname, fp )
    struct pcx *pcx;
    char *fullname;
    FILE *fp;
{
    FILE *f;
    unsigned char pcxhd[128];
    int cnt, b;
    int xmin;
    int xmax;
    int ymin;
    int ymax;
    int bytes_per_row;

    /* Open input file. */
    if((f = fp) == 0) {
	if (! (f = fopen(fullname, "r"))) {
	    fprintf(stderr, "Couldn't open pcx file %s.\n", fullname);
	    return(-1);
	}
    }

    /* Read .pcx header. */
    if (fread(pcxhd, sizeof(unsigned char), 128, f) != 128) {
	fclose(f);
	return(-1);
    }

    if ((pcxhd[0] != PCX_MAGIC) || (pcxhd[1] > 5)) {
	fclose(f);
	return(-1);
    }

    if (pcxhd[65] > 1) {
	return(-1);
    }

    /* Calculate raster header and swap bytes. */
    xmin = pcxhd[4] + ( 256 * pcxhd[5] );
    ymin = pcxhd[6] + ( 256 * pcxhd[7] );
    xmax = pcxhd[8] + ( 256 * pcxhd[9] );
    ymax = pcxhd[10] + ( 256 * pcxhd[11] );
    xmax = xmax - xmin + 1;
    ymax = ymax - ymin + 1;
    bytes_per_row = pcxhd[66] + ( 256 * pcxhd[67] );
    
    /* Allocate pbm array. */
    pcx_newBitImage(pcx, xmax,ymax);

    /* Read compressed bitmap. */
    PCX_LoadImage( f, bytes_per_row, pcx, ymax );
    fclose( f );
    return(0);
}


/*
**  PCX_LoadImage
**
**	Load PC Paintbrush file into the passed Image structure.
**
**	Returns no value (void function)
*/

static void PCX_LoadImage (f,bytes_per_row,pcx,rows)
    FILE *f;
    int bytes_per_row;
    struct pcx *pcx;
    int rows;
{
/* Goes like this: Read a byte.  If the two high bits are set,
 ** then the low 6 bits contain a repeat count, and the byte to
 ** repeat is the next byte in the file.  If the two high bits are
 ** not set, then this is the byte to write.
 */

    register unsigned char *ptr;
    int row = 0;
    int bytes_this_row = 0;
    int b, i, cnt;

    ptr = &(pcx_Data(pcx)[0]);
    while ((b = fgetc(f)) != EOF) {
	if ((b & 0xC0) == 0xC0) {
/* have a repetition count -- mask flag bits 
 */
	    cnt = b & 0x3F;
	    b = fgetc(f);
	    if (b == EOF)
		printf("Error in PCX file: unexpected EOF\n");
	}
	else {
	    cnt = 1;		/* no repeating this one */
	}
	for ( i = 0; i < cnt; i++ ) {
	    if ( row >= rows ) {
		printf("Warning: junk after bitmap data ignored\n");
		return;
	    }
	    *ptr++ = (unsigned char) (255 - b);
	    if (++bytes_this_row == bytes_per_row) {
		/* start of a new line */
		row++;
		bytes_this_row = 0;
	    }
	}
    }
    return;
}

long
pcx__Read( self, file, id )
    struct pcx *self;
    FILE *file;
    long id;
{
    if(pcx_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
pcx__Write( self, file, writeID, level )
    struct pcx *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
pcx__WriteNative( self, file, filename )
    struct pcx *self;
    FILE *file;
    char *filename;
{
return(0);
}
