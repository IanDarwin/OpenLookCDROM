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


 

/*  xwdio.ch

        xwdio package

        Routines for reading and writing rasters in X Window Dump (V7)
        as defined by XWDFile.h

 */

#include <pixelimg.ih>

#define  xwdio_PROGRAMMERVERSION    1

#define BUFBITS (1<<17)

package xwdio[xwdio]  {

    classprocedures:
      WriteRow(FILE *file,  unsigned char *byteaddr,  long nbytes);
    /* Write 'nbytes' bytes starting at 'byteaddr'
      into 'file' in X Window Dump image format,
     after the header is written. */

    ReadRow(FILE *file,  unsigned char *row,  long nbytes) returns long;
    /* Read 'nbytes' bytes from 'file' into
      positions starting at 'row', after the
      header is read; there is usually some
	padding in the last byte of each row,
      but the image is resized after all rows
      are read to remove it. */
    ReadImage(FILE *file, struct pixelimage *pix) returns long;
    /* Read an X Window Dump  from 'file' and
      put it in 'pix,' return error code */
    WriteImage(FILE *file, struct pixelimage *pix, struct rectangle *sub);
    /* Write an X Window Dump  to 'file' from  subrectangle 'sub' of pixelimage 'pix'  */

};

