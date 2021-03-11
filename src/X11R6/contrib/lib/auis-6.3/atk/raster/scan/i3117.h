/*
 *	
 *	
 *	FILE:		i3117.h
 *	
 *	PURPOSE:	i3117.n contains the hardware description of 
 *			the IBM 3117 scanner adapter.  This description
 *			is for the regular 3117 adapter, not the serial
 *			adapter using the 3117 expansion adapter.
 *
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
 *	CREATION DATE:	February 27, 1988
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
 *	00.00	19880227    Paul G. Crumley	first pass at this code
 *		
 *	01.00	1988????    Paul G. Crumley	first public release
 *	01.01	19881027    Paul G. Crumley	remove AIX support and 
 *						modify for 6152.
 *	
 *	
 */

/*
 * don't insert this file more than once.
 */
#ifndef TK_INCLUDED_i3117
#define TK_INCLUDED_i3117 1

/*
 *	the addresses for the 3117 scanner in memory and IO space
 */
#define	SCANNER_MEM_BUFFER_ADDR	    0x000de000
#define SCANNER_IO_TAG_ADDR	    0x00000228
#define SCANNER_IO_INTR_ADDR	    0x000002f7

/*
 *	these bits are located in the port addressed by SCANNER_IO_ADDR
 *	WO = Write Only, WR = Write Read, RO = Read Only
 */
#define CNTL_ACK	    0x01	/* WO acknowledge status */
#define CNTL_COMMAND	    0x02	/* WR execute command */
#define CNTL_INTR_EN	    0x04	/* WO enable interrupts */
#define CNTL_INTR_ST	    0x08	/* RO status of interrupts */
#define CNTL_STATUS	    0x10	/* RO status available */

/*
 *	valid commands for the adapter
 */
#define CMD_FLAG	    0x00	/* copy flag value (?) */
#define	CMD_ID		    0x01	/* retrieve scanner info */
#define CMD_INIT	    0x02	/* initialize adapter */
#define CMD_HOME	    0x03	/* move bed to home position */
#define CMD_TOP		    0x04	/* move bed to top position */
#define CMD_SCAN	    0x05	/* start the scan operation */
#define CMD_MORE	    0x06	/* continue the scan operation */

/*
 *	maximum and minimum values for the dither threshold matrix  
 */
#define MIN_THRESH	    0x00	/* minimum value for threshold */
#define MAX_THRESH	    0x3f	/* maximum value for threshold */


/*
 *	maximum and minimum values for size parameters for 
 *	RESOLUTION = [0, 1].
 */
#define MIN_H_DATA_0	    0x0000
#define MIN_H_DATA_1	    0x0000
#define MAX_H_DATA_0	    0x03f7
#define MAX_H_DATA_1	    0x07f7
#define MIN_V_DATA_0	    0x0000
#define MIN_V_DATA_1	    0x0000
#define MAX_V_DATA_0	    0x057b
#define MAX_V_DATA_1	    0x0af7


/*
 *	What the scanner hardware looks like to C.
 */

struct Scanner_type {
    unsigned char id1;		/* must be 0xf0 */
    unsigned char id2;		/* must be 0x0f */
    unsigned char command;		/* command sent to adapter */
    unsigned char flag;
    unsigned char hres;
    unsigned char vres;
    unsigned char threshold[16];
    unsigned char start_low;
    unsigned char start_high;
    unsigned char end_low;
    unsigned char end_high;
    unsigned char reserved1[6];
    unsigned char status;		/* what happened */
    unsigned char data_vol;		/* how many line_buffer's were filled */
    unsigned char reserved2[94];
    unsigned char reserved3[128];
    struct {
	unsigned char junk;
	unsigned char line_data[255];	/* where the data is put */
    } line_buffer[30];
};


/*
 *	endif TK_INCLUDED_i3117
 */
#endif
