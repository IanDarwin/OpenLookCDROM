/*
 *	
 *	
 *	FILE:		scan3117.h
 *	
 *	PURPOSE:	scan3117.h contains the information needed to use
 *			the subroutines provided in the scan3117.c package.
 *
 *	AUTHOR:		Paul G. Crumley
 *			pgc@andrew.itc.cmu.edu
 *	
 *	SITE:		Information Technology Center
 *			Carnegie Mellon University
 *			Pittsburgh, PA  15213
 *			U.S.A.
 *			412/268-6700
 *	
 *	OWNER:		This program is the property of Carnegie Mellon
 *			University.
 *
 *			This work is supported by the National Science
 *			Foundation under Grant No. ASC-8617695.  (the
 *			EXPRES project)
 *
 *	USAGE:		(C)Copyright 1988 by Carnegie Mellon University
 *
 *			Permission to use, copy, modify, and distribute
 *			these programs and their documentation for any
 *			purpose and without fee is hereby granted,
 *			provided that this copyright and permission notice
 *			appear on all copies and supporting documentation,
 *			that the name Carnegie Mellon University not be
 *			used in advertising or publicity pertaining to
 *			distribution of the programs without specific
 *			prior permission, and distribution is by
 *			permission of Carnegie Mellon University.
 *
 *	WARRANTY:	Carnegie Mellon University makes no representations
 *			about the suitability of this software for any
 *			purpose.  It is provided as, without express
 *			or implied warranty.
 *			
 *
 *	CREATION DATE:	January 20, 1988
 *	
 *
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


/*
 *	
 *	version	  date		person		    Prose
 *	------	--------    ------------------	-----------------------------
 *	00.00	19880120    Paul G. Crumley	first pass at this code.
 *	00.01	19880217    Paul G. Crumley	modify for AIX.
 *		
 *	01.00	1988????    Paul G. Crumley	first public release.
 *	01.01	19881027    Paul G. Crumley	remove AIX support and 
 *						modify for 6152.
 *						added SCAN_ERROR_DEV.
 *
 *	
 *	
 */

/*
 *	These publicly visible routines are:
 *	
 *	int ScanInit(LEVEL, CONTRAST, DITHER, RESOLUTION, 
 *	    XSTART, XEND, YSTART, YEND, DEBUG, BufferParams)
 *	    int LEVEL;	    level for white/black discrimination
 *	    int CONTRAST;	    range for dithering
 *	    int DITHER;	    type of dithering (0 for no dithering)
 *	    int RESOLUTION;    0 or 1 for 120 or 240 pixel/ih respectively
 *	    int XSTART;	    left side of scanned area
 *	    int XEND;	    right side of scanned area
 *	    int YSTART;	    top side of scanned area
 *	    int YEND;	    bottom side of scanned area
 *	    int DEBUG;	    message level (0 for quiet)
 *	    struct ScanInitParamsType BufferParams;info about the needed buffer
 *
 *	Returns SCAN_ERROR_NO_ERROR for success, some other SCAN_ERROR_?? 
 *	for failure.
 *
 *
 *	int Scan(Buffer)
 *	    char Buffer[];	where to place data
 *
 *	Returns SCAN_ERROR_NO_ERROR for success, SCAN_ERROR_NOT_INIT
 *	for failure.
 *
 *
 *	int ScanClose()
 *
 *	Returns SCAN_ERROR_NO_ERROR for success, SCAN_ERROR_NOT_INIT
 *	for failure.
 *
 *
 *
 *
 */


struct ScanInitParamsType {
    int Size;	 /* size of buffer in char */
    int XPixels;    /* number of pixels in x dimension */
    int YPixels;    /* number of pixels in y dimension */
};



int ScanInit();
int Scan();
int ScanClose();



/* 
 * error values
 */

#define SCAN_ERROR_NO_ERROR	  0 /* no error */
#define SCAN_ERROR_NO_CARD	 -1 /* no adapter card */
#define SCAN_ERROR_BAD_CARD	 -2 /* bad adapter card */
#define SCAN_ERROR_OPEN		 -3 /* problem serializing access */
#define SCAN_ERROR_DEV		 -4 /* /dev/scan3117 not found */
#define SCAN_ERROR_LEVEL	-10 /* bad value for LEVEL */
#define SCAN_ERROR_CONTRAST	-11 /* bad value for CONSTRAST */
#define SCAN_ERROR_DITHER	-12 /* bad value for DITHER */
#define SCAN_ERROR_RESOLUTION	-13 /* bad value for RESOLUTION */
#define SCAN_ERROR_XSTART	-14 /* bad value for XSTART */
#define SCAN_ERROR_XEND		-15 /* bad value for XEND */
#define SCAN_ERROR_XSTART_XEND	-16 /* XEND < XSTART */
#define SCAN_ERROR_YSTART	-17 /* bad value for YSTART */
#define SCAN_ERROR_YEND		-18 /* bad value for YEND */
#define SCAN_ERROR_YSTART_YEND	-19 /* YEND < YSTART */
#define SCAN_ERROR_INIT		-20 /* must be uninitialized for ths call */
#define SCAN_ERROR_NOT_INIT	-21 /* must be initialized for this call */

/*
 *  used for dithering
 */


/********
 ********   THESE SIZES MUST BE CHANGED IF THE BELOW ARRAYS ARE CHANGED
 ********/
#define DITHER_SLOPE_SIZE   20
#define DITHER_ORDER_SIZE   11
/********
 ********
 ********/
#ifdef I_SCAN3117
int DITHER_SLOPE[20]= {   0,    79,   158,   240,   325, 
			     414,   510,   613,   727,   854,
			    1000,  1171,  1376,  1632,  1963, 
			    2414,  3078,  4165,  6314, 12706};

unsigned char DITHER_ORDER[][16]= {{ 8,  8,  8,  8,	    /* DITHER = 0 */
                                 8,  8,  8,  8,	    /* no dither */
				 8,  8,  8,  8,
				 8,  8,  8,  8},

			       { 7,  9,  8,  1,	    /* DITHER = 1 */
				11, 14, 15,  6,	    /* upper left spiral */
				10, 13, 12,  3,
				 2,  5,  4,  0},

			       { 3,  5,  9,  0,	    /* DITHER = 2 */
				11, 15, 14,  6,	    /* spiral */
				 7, 12, 13, 10,
				 1,  8,  4,  2},

			       { 8,  6,  7, 14,	    /* DITHER = 3 */
				 4,  1,  0,  9,	    /* inverse of 1 */
				 5,  2,  3, 12,
				13, 10, 11, 15},

			       {12, 10,  8, 15,	    /* DITHER = 4 */
				 5,  0,  1, 10,	    /* inverse of 2 */
				 9,  3,  2,  6,
				14,  8, 11, 13},

			       { 0,  8,  2, 10,	    /* DITHER = 5 */
				12,  4, 14,  6,	    /* Bayer */
				 3, 11,  1,  9,
				15,  7, 13,  5},

			       { 0,  1,  5,  6,	    /* DITHER = 6 */
				 2,  4,  7, 12,	    /* triangle */
       				 3,  8, 11, 13,
				 9, 10, 14, 15},

			       { 0, 11, 10,  9,	    /* DITHER = 7 */
				 1, 15, 14,  8,	    /* square */
				 2, 12, 13,  7,
				 3,  4,  5,  6},

			       { 0,  1,  5,  6,	    /* DITHER = 8 */
				 3,  8, 11, 13,	    /* horiz lines */
				 2,  4,  7, 12,
				 9, 10, 14, 15},

			       {15,  7,  2, 14,	    /* DITHER = 9 */
				11,  1,  0,  8,	    /* random 1 */
				12,  6, 10,  9,
				13,  4,  3,  5},

			       { 3,  4,  5,  7,	    /* DITHER = 10 */
				13, 12, 10,  8,	    /* random 2 */
				11,  6,  0, 15,
				 2, 14,  1,  9}
			      };
#endif


/*
 *	maximum and minimum values for [X,Y]START & [X,Y]END 
 */
#define SCAN_MIN_X	    0
#define SCAN_MAX_X	 8500
#define SCAN_MIN_Y	    0
#define SCAN_MAX_Y	11000


/*
 *	maximum and minimum values for DITHER, LEVEL & CONTRAST
 */
#define SCAN_MIN_DITHER	    0
#define SCAN_MAX_DITHER	    (DITHER_ORDER_SIZE - 1)
#define SCAN_MIN_LEVEL	    0
#define SCAN_MAX_LEVEL	  100
#define SCAN_MIN_CONTRAST   0
#define SCAN_MAX_CONTRAST 100




