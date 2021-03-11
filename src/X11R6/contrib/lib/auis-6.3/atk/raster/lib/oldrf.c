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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/lib/RCS/oldrf.c,v 2.8 1992/12/15 21:40:02 rr2b R6tape $";
#endif


 


/*  oldrf.c

	oldRF package

	Routines for reading and writing rasters in the form 
	defined by rastfile.h, long ago.

	There is a 14 or 16 byte header followed by the bits.
	Some files are even 18 bytes longer than required.  These seem 
	to have a 16 byte header.

 */

#include <stdio.h>
#include <andrewos.h> /* sys/types.h */
#include <sys/stat.h>
#include <netinet/in.h>

#include <rastfile.h>

#include <class.h>
#include <oldrf.eh>
#include <pixelimg.ih>	
#include <dataobj.ih>

#define DEBUG(x)  {printf x; fflush(stdout);}

#define SWAB(v) ( ((v>>8) & 0xFF) | ((v&0xFF)<<8) )
#define SWAL(v) ( (SWAB(v)<<16)  |  SWAB(v>>16) )

static unsigned char masks[] = {0xFF, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE};



/* oldRF__WriteRow(file, row, length)  
		Writes to 'file' the encoded version of the 'length' bytes
		beginning at byte with address 'row'.
*/
	void
oldRF__WriteRow(ClassID, file, row, length)
	struct classhdr *ClassID;
	FILE *file;
	register unsigned char *row;
	long length;
{
	unsigned char buf[BUFBITS>>3];	/* temp for inversion */
	unsigned char *sx;			/* where to get bytes */
	unsigned char *dx;			/* where to put them */
	register long W = (length+7)>>3;	/* total number of bytes */

	/* convert colors */
	for (sx = row, dx = buf; sx < row+W; )
		*dx++ = ~*sx++;
	
	fwrite(buf, W, 1, file);
}


/* oldRF__ReadRow(file, row, length) 
	Copies one row of bits from 'file' to 'pix'
	returns 0 for success.  -1 for failure
*/
	long
oldRF__ReadRow(ClassID, file, row, length)
	struct classhdr *ClassID;
	register FILE *file;			/* where to get bytes from */
	register unsigned char *row;		/* where to put them */
	register long length;		/* how many bits in row must be filled */
{
	register long W = (length+7)>>3;/* number of bytes */
	unsigned char savebyte;

	savebyte = *(row+W-1);		/* save last byte */
	if (fread(row, W, 1, file) < 0) /* read the bytes */
		return dataobject_PREMATUREEOF;  /* report error, if any */

	/* convert colors */
	if (((unsigned long)row) & 3) {
		/* do first partial word */
		unsigned char *x;
		for (x = row; ((unsigned long)x)&3; x++)
			*x = ~*x;
	}
	{
		/* do full words */
		unsigned long *lx;
		for (lx = (unsigned long *)(((unsigned long)(row+W-4))&(~3)); 
				lx >= (unsigned long *)row; lx --)
			*lx = ~*lx;
	}
	if (((unsigned long)(row+W))&3) {
		unsigned char *x;
		for (x = row+W; ((unsigned long)(x--))&3; )
			*x = ~*x;
	}

	if (length & 0x7) {
		/* fix the last byte if length is not a multiple of 8 bits */
		register long mask = masks[length & 0x7];
		register unsigned char *loc = row+W-1;
		*loc = (*loc & mask) | (savebyte & ~mask);
	}
	return dataobject_NOREADERROR;
}



/* oldRF__ReadImage(file, pix) 
	Read a raster image from 'file' and put it in 'pix' 
		return error code
*/
	long
oldRF__ReadImage(ClassID, file, pix)
	struct classhdr *ClassID;
	register FILE *file;		/* where to get bits from */
	register struct pixelimage *pix;/* where to put them */
{
	struct stat st;			/* buffer to read stat info */
	struct RasterHeader hdr;	/* buffer to read file header */
	register unsigned char *where;		/* where to store next row */
	register long row, W;		/* count rows;  byte length of row */

	if (fread(&hdr, 14, 1, file) < 0)   /* read the header */
		  return dataobject_PREMATUREEOF;

	if (hdr.Magic == RasterMagic) {
		/* it is a proper raster file */
	}
	else if (hdr.Magic == SWAL(RasterMagic)) {
		/* it is a raster file with swapped bytes */
		hdr.width = SWAL(hdr.width);
		hdr.height = SWAL(hdr.height);
	}
	else {
		fprintf(stderr, "File starts w/ F1, but magic # is 0x%lx\n",
			hdr.Magic);
		fflush(stderr);
		return dataobject_BADFORMAT;
	}

	W = (hdr.width+7)>>3;

	/* kludge to test for files written with a sixteen byte header   XXX */
	if (fstat(fileno(file), &st) < 0)  return -2;
	if (st.st_size != 14 + hdr.height * W)
		/* assume it is a 16 byte header
		     read and discard 2 bytes */
		fread(&hdr.depth, 2, 1, file);

	pixelimage_Resize(pix, hdr.width, hdr.height);
	where = pixelimage_GetBitsPtr(pix);
	W = (W+1) & ~1;		/* round up to a multiple of 16 bits */
	for (row = 0; row < hdr.height; row ++, where += W)
		oldRF_ReadRow(file, where, hdr.width);
	pixelimage_NotifyObservers(pix, pixelimage_DATACHANGED);
	return dataobject_NOREADERROR;
}

/* oldRF__WriteImage(file, pix, sub) 
	Write a raster image from 'pix' to 'file'
*/
	void
oldRF__WriteImage(ClassID, file, pix, sub)
	struct classhdr *ClassID;
	register FILE *file;		/* where to put bits  */
	register struct pixelimage *pix;/* where to get them from */
	register struct rectangle *sub;
{
	long left, top, width, height;
	long buf[BUFBITS>>5];
	struct RasterHeader hdr;	/* buffer to write file header */
	long row;

	rectangle_GetRectSize(sub, &left, &top, &width, &height);

	hdr.Magic = htonl(RasterMagic);
	hdr.width = htonl(width);
	hdr.height = htonl(height);
	hdr.depth = htons(1);

	fwrite(&hdr, 14, 1, file);

	for (row = top;  row < top + height; row++) {
		pixelimage_GetRow(pix, left, row, width, buf);
		oldRF_WriteRow(file, (unsigned char *)buf, width);
	}
}

