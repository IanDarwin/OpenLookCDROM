/****************************************************************
 * mac.h:
 *
 * adapted from code by Patrick Naughton (naughton@sun.soe.clarkson.edu)
 *
 * macin.h
 * Mark Majhor
 * August 1990
 *
 * routines for reading MAC files
 *
 * Copyright 1990 Mark Majhor (see the included file
 * "mrmcpyrght.h" for complete copyright information)
 *
 ****************************************************************/

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


# define MAC_MAGIC	0x0

typedef unsigned char BYTE;	/* 8 bits unsigned		*/

/*
 * macin return codes
 */
#define MACIN_SUCCESS       0   /* success */

#define MACIN_ERR_BAD_SD   -1   /* bad screen descriptor */
#define MACIN_ERR_BAD_SIG  -2   /* bad signature */
#define MACIN_ERR_EOD      -3   /* unexpected end of raster data */
#define MACIN_ERR_EOF      -4   /* unexpected end of input stream */
#define MACIN_ERR_FAO      -5   /* file already open */
#define MACIN_ERR_IAO      -6   /* image already open */
#define MACIN_ERR_NFO      -7   /* no file open */
#define MACIN_ERR_NIO      -8   /* no image open */

static int macin_open_image();
static int macin_close_file();
#if 0
static int macin_fatal();
#endif

#define	MAC_HDR_LEN	512
#define ADD_HDR_LEN	128
#define	MAX_LINES	720
#define	BYTES_LINE	72
