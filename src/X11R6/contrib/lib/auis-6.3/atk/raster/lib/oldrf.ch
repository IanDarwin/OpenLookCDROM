/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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


 


/*  oldrf.H

	oldRF package

	Routines for reading and writing rasters in the old ITC format
	as defined by rastfile.h

	The data stream begins with a 14 or 16 byte header,
	which is followed by the bits of the data.

 */

#include <pixelimg.ih>

#define  oldRF_PROGRAMMERVERSION    1

#define BUFBITS (1<<17)

package oldRF[oldrf]  {

classprocedures:

	WriteRow(FILE *file,  unsigned char *byteaddr,  long nbytes);
		/* write to 'file' the oldRF form representation of the 'nbytes'
		    bytes starting at location 'byteaddr' */
	
	ReadRow(FILE *file,  unsigned char *row,  long length) returns long;
		/* Reads from 'file' the encoding of bytes to fill in 'row'.  Row will be
		    truncated or padded (with WHITE) to exactly 'length' bytes.
		*/
	ReadImage(FILE *file, struct pixelimage *pix) returns long;
		/* Read a raster image from 'file' and put it in 'pix' 
			return error code */
	WriteImage(FILE *file, struct pixelimage *pix, struct rectangle *sub);
		/* Write a raster image to 'file' from 'pix'  */

};

