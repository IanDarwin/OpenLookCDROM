/* sunraster.h
 *
 * this describes the header for Sun rasterfiles.  if you have SunOS, a
 * better description is in /usr/include/rasterfile.h.  this is used
 * instead to improve portability and to avoid distribution problems.
 *
 * Copyright 1989 Jim Frost.  See included file "copyright.h" for complete
 * copyright information.
 */

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


struct rheader {
  unsigned char magic[4];   /* magic number */
  unsigned char width[4];   /* width of image in pixels */
  unsigned char height[4];  /* height of image in pixels */
  unsigned char depth[4];   /* depth of each pixel */
  unsigned char length[4];  /* length of the image in bytes */
  unsigned char type[4];    /* format of file */
  unsigned char maptype[4]; /* type of colormap */
  unsigned char maplen[4];  /* length of colormap in bytes */
};

/* following the header is the colormap (unless maplen is zero) then
 * the image.  each row of the image is rounded to 2 bytes.
 */

#define RMAGICNUMBER 0x59a66a95 /* magic number of this file type */

/* these are the possible file formats
 */

#define ROLD       0 /* old format, see /usr/include/rasterfile.h */
#define RSTANDARD  1 /* standard format */
#define RRLENCODED 2 /* run length encoding to compress the image */
#define RRGB       3 /* RGB-format instead of BGR in 24 or 32-bit mode */
#define RTIFF      4 /* TIFF <-> rasterfile */
#define RIFF       5 /* IFF (TAAC) <-> rasterfile */

/* these are the possible colormap types.  if it's in RGB format,
 * the map is made up of three byte arrays (red, green, then blue)
 * that are each 1/3 of the colormap length.
 */

#define RNOMAP  0 /* no colormap follows the header */
#define RRGBMAP 1 /* rgb colormap */
#define RRAWMAP 2 /* raw colormap; good luck */

#define RESC 128 /* run-length encoding escape character */

