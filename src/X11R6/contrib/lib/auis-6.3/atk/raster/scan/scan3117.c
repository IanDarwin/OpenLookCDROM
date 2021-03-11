/*
 *	
 *	
 *	FILE:		scan3117.c
 *	
 *	PURPOSE:	scan3117.c contains a set of subroutines that are
 *			used to scan images using the IBM 3117 scanner
 *			attached to an RT/PC.
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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/scan/RCS/scan3117.c,v 1.11 1992/12/15 21:40:02 rr2b R6tape $";
#endif


/*
 *	
 *	version	  date		person		    Prose
 *	------	--------    ------------------	-----------------------------
 *	00.00	19880120    Paul G. Crumley	first pass at this code
 *	00.01	19880217    Paul G. Crumley	modify for AIX
 *		
 *	01.00	1988????    Paul G. Crumley	first public release
 *	01.01	19881027    Paul G. Crumley	remove AIX support and 
 *						modify for 6152.
 *	
 *	
 */

/*
 *	Of all the routines in this package, three are publicly visible.  
 *	These publicly visible routines are:
 *	
 *	int ScanInit(LEVEL, CONTRAST, DITHER, RESOLUTION,
 *			XSTART, XEND, YSTART, YEND, DEBUG, BufferParams)
 *	    int	LEVEL;	    level for white/black discrimination
 *	    int	CONTRAST;   range for dithering
 *	    int	DITHER;	    type of dithering (0 for no dithering)
 *	    int	RESOLUTION; 0 or 1 for 120 or 240 pixels/inch respectively
 *	    int XSTART;	    left side of scanned area (in 1/1000s inch)
 *	    int XEND;	    right side of scanned area (in 1/1000s inch)
 *	    int YSTART;	    top side of scanned area (in 1/1000s inch)
 *	    int YEND;	    bottom side of scanned area (in 1/1000s inch)
 *	    int DEBUG;	    debug mode (0 is quiet)
 *	    struct ScanInitParamsType BufferParams;
 *
 *	Returns 0 for success, -1 for failure.  On failure 
 *	BufferParams is not valid.
 *
 *
 *	int Scan(BUFFER)
 *	    unsigned char BUFFER[];	where to place data
 *
 *	returns 0 for success, -1 for failure.
 *
 *
 *	int ScanClose()
 *
 *	returns 0 for success, -1 for failure.
 *
 *
 *
 *
 */

/*
 * include the needed files
 */

#include <andrewos.h>
#include <config.h>
#ifdef SUPPORTTED_PLATFORM


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef AIX
#include <sys/hwdbus.h>
#include <fcntl.h>
#else
#include <sys/file.h>
#endif



#include <i3117.h>
#define I_SCAN3117
#include <scan3117.h>


static struct Scanner_type *Scanner;	/* pointer to scanner buffer */
static unsigned	char *ScannerCntl;	/* pointer to scanner control register */


/* used with SendCommand function */
#define	WAIT	0
#define NOWAIT	1


/*
 *  used to indicate whether the Param*'s are valid
 */

#define NOT_INITIALIZED	    0
#define INITIALIZED	    1

static int   Initialized = NOT_INITIALIZED;


/* 
 *  used to get at the IO bus
 */

static int busfd;			/* used when accessing I/O bus */
static int devfd;			/* used to serialize access */


/*
 *   derived scan parameters
 */

static int ParamXStart;
static int ParamXEnd;
static int ParamYStart;
static int ParamYEnd;
static int ParamHRes;
static int ParamVRes;
static int ParamThreshold[16];


/*
 *  debugging things
 */
 
static int DebugMode;


/* 
 *	tell the C compiler about these private functions
 */

static void HomeScanner();
static int SendCommand();


/* 
 *	Public functions
 */

int
ScanInit(LEVEL, CONTRAST, DITHER, RESOLUTION, 
	    XSTART, XEND, YSTART, YEND, DEBUG, BufferParams)
/*
 *	Test for the presence of a usable 3117 scanner, initialize it with the 
 *	passed parameters, and return 0 if OK, -1 if an error was found.  The 
 *	structure BufferParams will be filled in for the caller.
 */
    int LEVEL;	    /* forced to 0 - 100 */
    int CONTRAST;   /* forced to 0 - 100 */
    int DITHER;	    /* 0 (no dither), 1 - 4 for different patterns */
    int RESOLUTION; /* 0 or 1 for 120 or 240 pixel/inch respectively */
    int	XSTART;		/* forced to 0 -  8500 */
    int	XEND;		/* forced to 0 -  8500 */
    int	YSTART;		/* forced to 0 - 11000 */
    int	YEND;		/* forced to 0 - 11000 */
    int DEBUG;		/* 0 for quiet */
    struct ScanInitParamsType *BufferParams; /* info about needed buffer */

{
    int ThisRes;
    int Point;
    int Slope;
    int i;
    struct stat	statbuf;	/* needed for stat(...) call */

#ifdef AIX
    struct hwdbase hwb;
#endif

    DebugMode =	DEBUG;				/* remember what kind of debugging */
    if (DebugMode != 0) {
	fprintf(stderr, "scan:  debugging mode set to %d.\n", DebugMode);
	fflush(stderr);
    }


    busfd = open("/dev/bus", O_RDWR, 0);	/* get access to I/O bus */
    if (stat("/dev/scan3117", &statbuf)	!= 0) {		    /* does file exist? */
	return(SCAN_ERROR_DEV);
    }
    devfd = open("/dev/scan3117", O_RDONLY | O_EXCL, 0);    /* is it in use? */
    if (devfd < 0) {
	return(SCAN_ERROR_OPEN);
    }

#ifdef AIX
    ioctl(busfd, HWDBASE, &hwb);
    Scanner = (struct Scanner_type *) (hwb.hwdmem + 
					    SCANNER_MEM_BUFFER_ADDR);
    ScannerCntl = (char *) (hwb.hwdio + SCANNER_IO_TAG_ADDR);
#else
#define IOMemBase 0xf4000000
#define IOIOBase 0xf0000000
    Scanner = (struct Scanner_type *) (IOMemBase + 
					SCANNER_MEM_BUFFER_ADDR);
    ScannerCntl = (unsigned char *) (IOIOBase + SCANNER_IO_TAG_ADDR);
#endif


    if (DebugMode != 0) {
	fprintf(stderr, "id1 = 0x%2x, id2 = 0x%2x\n", Scanner->id1, Scanner->id2);
	fflush(stderr);
    }
    if ((Scanner->id1 == 0xf0) && (Scanner->id2 == 0x0f)) {	
	if (DebugMode != 0) {
	    fprintf(stderr, "about to init scanner adapter\n");
	    fflush(stderr);
	}
	if (SendCommand(CMD_INIT, WAIT) != 0) {
	    return(SCAN_ERROR_BAD_CARD);
	}
    } else {
	    return(SCAN_ERROR_NO_CARD);
    }

    /* if we got this far, the hardware is OK, now deal with the parameters */
    
    if (RESOLUTION == 0) {
	ParamHRes = 0;
	ParamVRes = 0;
	ThisRes = 120;
    } else if (RESOLUTION == 1) {
	ParamHRes = 1;
	ParamVRes = 1;
	ThisRes = 240;
    } else {
	return(SCAN_ERROR_RESOLUTION);
    }

    if (XSTART >= XEND) {
	return(SCAN_ERROR_XSTART_XEND);
    }
    if (YSTART >= YEND) {
	return(SCAN_ERROR_YSTART_YEND);
    }

    if (XSTART < SCAN_MIN_X) {
	return(SCAN_ERROR_XSTART);
    }
    if (XEND > SCAN_MAX_X) {
	return(SCAN_ERROR_XEND);
    }
    if (YSTART < SCAN_MIN_Y) {
	return(SCAN_ERROR_YSTART);
    }
    if (YEND > SCAN_MAX_Y) {
	return(SCAN_ERROR_YEND);
    }

    ParamXStart	= ((XSTART * ThisRes) / 1000);  /* try to include area */
    ParamXEnd = (((XEND * ThisRes) / 1000) + 7);/* note: Xs in bytes */
    ParamYStart	= ((YSTART * ThisRes) / 1000);
    ParamYEnd = ((YEND * ThisRes) / 1000);

    if (RESOLUTION == 0) {
        if (ParamXStart < MIN_H_DATA_0) {
	    ParamXStart = MIN_H_DATA_0;
	}
	if (ParamXEnd > MAX_H_DATA_0) {
	    ParamXEnd = MAX_H_DATA_0;
	}
	if (ParamYStart < MIN_V_DATA_0) {
	    ParamYStart = MIN_V_DATA_0;
	}
	if (ParamYEnd > MAX_V_DATA_0) {
	    ParamYEnd = MAX_V_DATA_0;
	}
    } else {
        if (ParamXStart < MIN_H_DATA_1) {
	    ParamXStart = MIN_H_DATA_1;
	}
	if (ParamXEnd > MAX_H_DATA_1) {
	    ParamXEnd = MAX_H_DATA_1;
	}
	if (ParamYStart < MIN_V_DATA_1) {
	    ParamYStart = MIN_V_DATA_1;
	}
	if (ParamYEnd > MAX_V_DATA_1) {
	    ParamYEnd = MAX_V_DATA_1;
	}
    }

    ParamXStart /= 8;		/* must be in bytes */
    ParamXEnd /= 8;

    BufferParams->XPixels = (ParamXEnd - ParamXStart + 1) * 8;
    BufferParams->YPixels = ParamYEnd - ParamYStart + 1;
    BufferParams->Size = (BufferParams->YPixels * BufferParams->XPixels) / 8;

    if (RESOLUTION == 0) {	/* fix up because Y is always in 240/inch */
	ParamYStart *= 2;
	ParamYEnd *= 2;
    }

    /* compute the dither matrix */
    
    if (LEVEL < SCAN_MIN_LEVEL) {
	return(SCAN_ERROR_LEVEL);
    }
    if (LEVEL > SCAN_MAX_LEVEL) {
	return(SCAN_ERROR_LEVEL);
    }

    Point = MIN_THRESH + ((MAX_THRESH - MIN_THRESH) * LEVEL) / 100;

    if ((CONTRAST < SCAN_MIN_CONTRAST) || (CONTRAST > SCAN_MAX_CONTRAST)) {
	return(SCAN_ERROR_CONTRAST);
    }

    if ((DITHER < SCAN_MIN_DITHER) || (DITHER > SCAN_MAX_DITHER)) {
	return(SCAN_ERROR_DITHER);
    }

    /* value of Slope is * 1000 */

    if (CONTRAST == SCAN_MAX_CONTRAST) {  /* special case for MAX value */
	Slope = (SCAN_MAX_CONTRAST - 1) * DITHER_SLOPE_SIZE;
    } else {
	Slope = CONTRAST * DITHER_SLOPE_SIZE;
    }
    Slope = DITHER_SLOPE[Slope/SCAN_MAX_CONTRAST] * 4;/* normalize slope */

    for (i=0; i<16; i++) {
	ParamThreshold[i] = 
	    (Slope * (DITHER_ORDER[DITHER][i] - 8) / 1000) + Point;
	if (ParamThreshold[i] < MIN_THRESH) {
	    ParamThreshold[i] = MIN_THRESH;
	}
	if (ParamThreshold[i] > MAX_THRESH) {
	    ParamThreshold[i] = MAX_THRESH;
	} 
    }

    if (DebugMode != 0) {
        fprintf(stderr, "value of ParamXStart is: %d\n", ParamXStart);
        fprintf(stderr, "value of ParamXEnd is: %d\n", ParamXEnd);
        fprintf(stderr, "value of ParamYStart is: %d\n", ParamYStart);
        fprintf(stderr, "value of ParamYEnd is: %d\n", ParamYEnd);
        fprintf(stderr, "value of ParamHRes is: %d\n", ParamHRes);
        fprintf(stderr, "value of ParamVRes is: %d\n", ParamVRes);
        fprintf(stderr, "value of  ParamThreshold[16] is:");
        for (i=0; i<16; i++) {
	    fprintf(stderr, " %d = 0x%x\n", i, ParamThreshold[i]);
        }
	fprintf(stderr, "\n");
	fflush(stderr);
    }

    Initialized = INITIALIZED;
    return(SCAN_ERROR_NO_ERROR);
}


int 
Scan(buffer)
/*
 *	This routine will operate the 3117 to capture the image and place the
 *	image in the passed buffer.
 *
 *	This routine may be called many times without having to call ScanInit()
 *	and ScanClose() each time.
 */
    unsigned char    buffer[];

{
int buffer_index;
int i;
int j;

    if (DebugMode != 0) {
        fprintf(stderr, "entering Scan()\n");
	fflush(stderr);
    }

    if (Initialized != INITIALIZED) {
	return(SCAN_ERROR_NOT_INIT);
    }

    if (SendCommand(CMD_TOP, WAIT) != 0) {
	HomeScanner();
	return(SCAN_ERROR_BAD_CARD);
    }

    Scanner->hres = ParamHRes;
    Scanner->vres = ParamVRes;
    for (i=0; i<16; i++) {
	Scanner->threshold[i] = ParamThreshold[i];
    }
    Scanner->start_low = ParamYStart & 0x00ff;
    Scanner->start_high = (ParamYStart & 0xff00) >> 8;
    Scanner->end_low = ParamYEnd & 0x00ff;
    Scanner->end_high = (ParamYEnd & 0xff00) >> 8;

    buffer_index = 0;		/* start at beginning of buffer */

    if (SendCommand(CMD_SCAN, WAIT) != 0) {
	HomeScanner();
	return(SCAN_ERROR_BAD_CARD);
    }
    while ((Scanner->data_vol) > 0) {
	for (i=0; i < (Scanner->data_vol); i++) {
	    for (j=ParamXStart; j <= (ParamXEnd); j++) {
		buffer[buffer_index++] = 
			(Scanner->line_buffer[i].line_data[j]);
	    }
	}
	if (SendCommand(CMD_MORE, WAIT) != 0) {
	    HomeScanner();
	    return(SCAN_ERROR_BAD_CARD);
	}
    }

    if (DebugMode != 0) {
        fprintf(stderr, "leaving Scan() with no error\n");
	fflush(stderr);
    }

    HomeScanner();
    return(SCAN_ERROR_NO_ERROR);
}


int
ScanClose()
/*
 *	This routine is used to relinquish one's control of the 3117.
 */

{
    close(devfd);
    close(busfd);

    if (Initialized != INITIALIZED) {
	return(SCAN_ERROR_NOT_INIT);
    }
    Initialized = NOT_INITIALIZED;
    return(SCAN_ERROR_NO_ERROR);
}


/*
 *	Private functions
 */


static void
HomeScanner()
/*
 *	Return the scanner bed to the home position.
 */

{
    (void) SendCommand(CMD_HOME, NOWAIT);
    return;
}


static int
SendCommand(COMMAND, WAITRET)
/*
 *	Send a command to the scanner adapter.  Wait until the adapter
 *	is ready before sending the command and optionally, wait for 
 *	the command to complete before returning.
 */
    int COMMAND;	/* command to send to 3117 adapter */
    int WAITRET;	/* WAIT to wait for status, NOWAIT to just return */

{
    if (DebugMode != 0) {
	fprintf(stderr, "into SendCommand COMMAND = 0x%x, WAITRET = 0x%x\n", COMMAND, WAITRET);
	fprintf(stderr, "into SendCommand *ScannerCntl = 0x%x\n", (*ScannerCntl));
	fflush(stderr);
    }
    /* wait until ready */
        while (((*ScannerCntl) & (CNTL_COMMAND | CNTL_STATUS)) == CNTL_COMMAND) {
	    if (DebugMode != 0) {
		fprintf(stderr, "waiting for adapter, *ScannerCntl = 0x%x\n", (int) (*ScannerCntl));
		fflush(stderr);
	    }
	}   

    if (DebugMode != 0) {
	fprintf(stderr, "ACKing adapter\n");
	fflush(stderr);
}

    *ScannerCntl = CNTL_ACK;

    /* send command */
    Scanner->command = COMMAND;
    *ScannerCntl = CNTL_COMMAND;
    if (WAITRET == WAIT) {
	if (DebugMode != 0) {
	    fprintf(stderr, "waiting for command to comptete\n");
	    fflush(stderr);
	}
        while (((*ScannerCntl) & (CNTL_COMMAND | CNTL_STATUS)) != CNTL_STATUS) {
	    if (DebugMode != 0) {
		fprintf(stderr, "waiting for command to comptete, *ScannerCntl = 0x%2x\n", (int) (*ScannerCntl));
		fflush(stderr);
	    }
	} 				/* just wait */
        return (Scanner->status);
    } else {
	return (SCAN_ERROR_NO_ERROR);
    }
}


#endif /* #ifdef SUPPORTTED_PLATFORM */
