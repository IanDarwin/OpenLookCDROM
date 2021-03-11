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


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1987, 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */

/*  rasterio.H

	rasterio package

	Routines for reading and writing rasters in .raster form
	(BE2 rasters version 2.)

	For data stream definition, see itc/be2/raster/spec.d

	We do not implement rasterio_ReadImage and rasterio_WriteImage
	because they would depend on raster.ih and rastimg.ih.
	Since raster.c depends on rasterio.ih, there would be a circularity
	and there would be no reasonable order in which to install 
	itc/be2/raster/raster and itc/be2/raster/libraster.

 */

#define  rasterio_PROGRAMMERVERSION    1

package rasterio

{

classprocedures:

	WriteRow(FILE *file, unsigned char *byteaddr, long nbytes);
		/* write to 'file' the .raster form representation of the 'nbytes'
		    bytes starting at location 'byteaddr' */
	
	ReadRow(FILE *file, unsigned char *row, long length) returns long;
		/* Reads from 'file' the encoding of bytes to fill in 'row'.  Row will be
		    truncated or padded (with WHITE) to exactly 'length' bytes.
		    Returns the code that terminated the row.  This may be 
			'|'  	correct end of line
			'\0' 	if the length was satisfied (before a terminator)
			EOF 	if the file ended
			'\'  '{' 	other recognized ends.
		*/

	ReadImage(FILE *file, struct pixelimage *pix) returns long;
		/* Read a raster image from 'file' and put it in 'pix' 
			return error code */

	WriteImage(FILE *file, struct pixelimage *pix, struct rectangle *sub);
		/* Write a raster image to 'file' from 'pix'  */

};

