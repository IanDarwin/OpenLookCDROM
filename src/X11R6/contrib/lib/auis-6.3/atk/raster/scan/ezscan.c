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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/scan/RCS/ezscan.c,v 1.10 1992/12/15 21:40:02 rr2b R6tape $";
#endif


/* ***************************************************************** 

	ezscan.c - a simple interface to exercise the 3117 scanner

	author		Paul Crumley CMU-ITC 

        Using an IBM 3117 scanner write an ATK raster file 

	NOTE:  THIS ONLY RUNS ON RT/PCs!




	This code existed in many forms before this.  This program
	just starts with version 4.00.

	4.00	19880414	pgc	first release
	4.01	19880415	pgc	fixed header size
  
  	5.00	19880513	pgc	now writes files in ATK raster format
	5.01	19880516	pgc	reverse the way level and contrast work and alter IDS code
	5.02	19881027	pgc	remove dependence on site.h



   ***************************************************************** */
#include <config.h>
#ifdef SUPPORTTED_PLATFORM

#include <stdio.h>
#include <sys/file.h>

#include <scan3117.h>




/*
  arguments
*/

char *NArg;
FILE *NHandle;
int RArg;
int DArg;
int LArg;
int CArg;
int XStartArg;
int XEndArg;
int YStartArg;
int YEndArg;
int VArg;
int QArg;
int FArg;


int    ParseArgs();
void   SetScanParameters();
void   Usage();
int   WriteImage();
int   WriteRow();


main(ARGC, ARGV)
int   ARGC;
char *ARGV[];

{
struct ScanInitParamsType SIP;
int    rc;
unsigned char   *buffer_ptr;

    if ((rc = (ParseArgs(ARGC, ARGV))) != 0) {
	if (rc = 1) {
	    Usage();
	    exit(1);
	} else {
	    fprintf(stderr, "scan:  parameter error\n");
	    Usage();
	    exit(2);
	}
	if (QArg == 0) {
	    fprintf(stderr, "scan:  (Version 5.02)\n");
	}
    }


    if ((FArg < 0) || FArg > 1) {
	fprintf(stderr, "scan:  invalid value for -f parameter\n");
	Usage();
	exit(2);
  }

    if (NArg != NULL) {	/* must have defined a name */
	if ((NHandle = fopen(NArg, "w")) == (FILE *) NULL) {
	    fprintf(stderr, "scan:  error openning file %s\n", NArg);
	    Usage();
	    exit(4);
	}
    }

    if ((rc = ScanInit(LArg, CArg, DArg, RArg,
		XStartArg, XEndArg, YStartArg, YEndArg, VArg,
		&SIP)) != SCAN_ERROR_NO_ERROR) {
	switch (rc) {
	    case SCAN_ERROR_DEV:
		fprintf(stderr,	"scan:	system is not configured for a 3117\n");
		exit(8);
		break;

	    case SCAN_ERROR_OPEN:
		fprintf(stderr,	"scan:	scanner	in use...try again  later\n");
		exit(8);
		break;

	    case SCAN_ERROR_NO_CARD:
	    case SCAN_ERROR_BAD_CARD:
		fprintf(stderr, "scan:  unable to find a usable scanner\n");
		exit(8);
		break;

	    case SCAN_ERROR_LEVEL:
		fprintf(stderr,	"scan:	invalid	value for -l parameter\n");
		Usage();
		break;

	    case SCAN_ERROR_CONTRAST:
		fprintf(stderr,	"scan:	invalid	value for -c parameter\n");
		Usage();
		break;

	    case SCAN_ERROR_DITHER:
		fprintf(stderr,	"scan:	invalid	value for -d parameter\n");
		Usage();
		break;

	    case SCAN_ERROR_RESOLUTION:
		fprintf(stderr,	"scan:	invalid	value for -r parameter\n");
		Usage();
		break;

	    case SCAN_ERROR_XSTART:
	    case SCAN_ERROR_XEND:
	    case SCAN_ERROR_XSTART_XEND:
		fprintf(stderr,	"scan:	invalid	value for -x parameter\n");
		Usage();
		break;

	    case SCAN_ERROR_YSTART:
	    case SCAN_ERROR_YEND:
	    case SCAN_ERROR_YSTART_YEND:
		fprintf(stderr,	"scan:	invalid	value for -y parameter\n");
		Usage();
		break;

	    default:
		fprintf(stderr, "scan:  internal programming error 2.... please notify system administrator\n");
		exit(128);
	}
	exit(2);
    }

    if ((buffer_ptr = (unsigned char *) malloc(SIP.Size)) == (unsigned char *) NULL) {
	fprintf(stderr, "scan:  error allocating memory!\n");
	exit(16);
    }

    if ((rc = Scan(buffer_ptr)) != SCAN_ERROR_NO_ERROR) {
	switch (rc) {
	    case SCAN_ERROR_NOT_INIT:
		fprintf(stderr, "scan:  internal programming error 3.... please notify system administrator\n");
		exit(128);
		break;

	    case SCAN_ERROR_BAD_CARD:
		fprintf(stderr, "scan:  the scanner is not functioning properly... please notify system administrator\n");
		exit(8);
		break;

	    default:
		fprintf(stderr, "unexpected error %d from Scan function! please notify system administrator\n", rc);
		exit(8);
	}
    }

    if (FArg == 0) {
	if (WriteImage(NHandle, buffer_ptr, SIP.YPixels, (SIP.Size / SIP.YPixels)) != 0) {
	    fprintf(stderr, "scan:  error writing data to file %s\n", NArg);
	    exit(4);
	}
    } else {
	if(WriteIDS(NHandle, buffer_ptr, SIP.YPixels, SIP.XPixels, SIP.Size, RArg) != 0) {
	    fprintf(stderr, "scan:  error writing data to file %s\n", NArg);
	    exit(4);
	}
    }

    if (fclose(NHandle) != 0) {
	fprintf(stderr, "error closing file %s\n", NArg);
	exit(8);
    }

    if (ScanClose() != SCAN_ERROR_NO_ERROR) {
	fprintf(stderr, "error closing scanner!\n");
	exit(8);
    }

}


int 
ParseArgs(ARGC, ARGV)
int   ARGC;
char *ARGV[];

{
int index;
int i, j;

    SetScanParameters();	/* set defaults */

    for (index = 1; index < ARGC; index++) {
	if ('-' != *ARGV[index]) {  /* looks like a file name */
	    NArg = ARGV[index];
	} else {
	    switch (*(ARGV[index] + 1)) {
		case 'r':
		    if (sscanf((ARGV[index] + 2), "%d", &i) > 0) {
			if (i == 120) {
			    RArg = 0;
			} else if (i == 240) {
			    RArg = 1;
			} else {
			    RArg = 2;	/* force an error */
			}
		    }
		    break;
		case 'd':
		    if (sscanf((ARGV[index] + 2), "%d", &i) > 0) {
			DArg = i;
		    }
		    break;
		case 'l':
		    if (sscanf((ARGV[index] + 2), "%d", &i) > 0) {
			LArg = SCAN_MAX_LEVEL - i;
		    }
		    break;
		case 'c':
		    if (sscanf((ARGV[index] + 2), "%d", &i) > 0) {
			CArg = SCAN_MAX_CONTRAST - i;
		    }
		    break;
		case 'x':
		    if (sscanf((ARGV[index] + 2), "%d:%d", &i, &j) > 0) {
			XStartArg = i;
			XEndArg = j;
		    }
		    break;
		case 'y':
		    if (sscanf((ARGV[index] + 2), "%d:%d", &i, &j) > 0) {
			YStartArg = i;
			YEndArg = j;
		    }
		    break;
		case 'v':
		    if (sscanf((ARGV[index] + 2), "%d", &i) > 0) {
			VArg = i;
		    }
		    break;
		case 'h':
		    return(1);
		    /* break; */
		case 'q':
		    QArg = 1;
		    break;
		case 'f':
		    if (sscanf((ARGV[index] + 2), "%d", &i) > 0) {
			FArg = i;
		    }
		    break;
		default:
		    goto error;
	    } /* switch */
	}

    }

    return(0);

error:
    return(-1);
}


void 
SetScanParameters()

{
    NArg	= NULL;
    NHandle	= stdout;
    RArg	= 0;		/* select 120 pixel/inch */
    DArg	= SCAN_MIN_DITHER;
    LArg	= (SCAN_MIN_LEVEL + SCAN_MAX_LEVEL) / 2;
    CArg	= (SCAN_MIN_CONTRAST + SCAN_MAX_CONTRAST) / 2;
    XStartArg	= SCAN_MIN_X;	/* start top left corner */
    XEndArg	= SCAN_MAX_X;	/* make sure it is a multiple of 8 */
    YStartArg	= SCAN_MIN_Y;	/* as above */
    YEndArg	= SCAN_MAX_Y;	/* as above */
    VArg	= 0;		/* turn off debugging */
    QArg	= 0;		/* don't be quiet */
    FArg	= 0;		/* ATK format */
}


void 
Usage()

{
    fprintf(stderr,
"\n");
    fprintf(stderr, 
"scan (Version 5.02) -- scanning program for IBM 3117 scanners\n");
    fprintf(stderr, 
"\n");
    fprintf(stderr, 
"usage:  scan [-r#] [-d#] [-l#] [-c#] [-x#:#] [-y#:#] [-v#] [-h] [-q] [-f#] [output file name]\n");
    fprintf(stderr, 
"\n");
    fprintf(stderr,
"               -r#              -- resolution in pixels/inch [%d or %d]\n", 
120, 240);
    fprintf(stderr,
"               -d#              -- dithering mode  [%d - %d]\n", 
SCAN_MIN_DITHER, SCAN_MAX_DITHER);
    fprintf(stderr, 
"               -l#              -- level [%d - %d]\n",
SCAN_MIN_LEVEL, SCAN_MAX_LEVEL);
    fprintf(stderr, 
"               -c#              -- contrast [%d - %d]\n",
SCAN_MIN_CONTRAST, SCAN_MAX_CONTRAST);
    fprintf(stderr, 
"               -x#:#            -- H range in 1/1000 inch [%d - %d]\n",
SCAN_MIN_X, SCAN_MAX_X);
    fprintf(stderr, 
"               -y#:#            -- V range in 1/1000 inch [%d - %d]\n",
SCAN_MIN_Y, SCAN_MAX_Y);
    fprintf(stderr,
"               -v#              -- verbose mode [%d - %d]\n",
0, 1);
    fprintf(stderr,
"               -h               -- display this help message\n"
);
    fprintf(stderr,
"               -q               -- display only error messages\n"
);
    fprintf(stderr,
"               -f#              -- format 0=ATK, 1=IDS\n"
);
    fprintf(stderr, 
"               output file name -- where to send file\n");
    fprintf(stderr, 
"           NOTE: there are no spaces within the parameters.\n");
    fprintf(stderr, 
"           defaults are:\n");
    fprintf(stderr, 
"               -r               -- %d\n",
120);
    fprintf(stderr, 
"               -d               -- %d\n",
SCAN_MIN_DITHER);
    fprintf(stderr, 
"               -l               -- %d\n",
(SCAN_MIN_LEVEL + SCAN_MAX_LEVEL) / 2);
    fprintf(stderr, 
"               -c               -- %d\n",
(SCAN_MIN_CONTRAST + SCAN_MAX_CONTRAST) / 2);
    fprintf(stderr, 
"               -x               -- %d:%d\n",
SCAN_MIN_X, SCAN_MAX_X);
    fprintf(stderr, 
"               -y               -- %d:%d\n",
SCAN_MIN_Y, SCAN_MAX_Y);
    fprintf(stderr,
"               -v               -- %d\n",
0);
    fprintf(stderr,
"               -q               -- off\n"
);
    fprintf(stderr,
"               -f               -- %d\n",
0);
    fprintf(stderr, 
"               output file name -- STDOUT\n");
    fprintf(stderr, 
"           Repeated arguments cause the last value encountered to be used.\n");
    fprintf(stderr, 
"See the documentation in the help system for additional information.\n");
    fprintf(stderr, 
"pgc 881027\n");
}


/*
ATK format write functions.
*/



static unsigned char hex[16][32] = {
	"000102030405060708090a0b0c0d0e0f",
	"101112131415161718191a1b1c1d1e1f",
	"202122232425262728292a2b2c2d2e2f",
	"303132333435363738393a3b3c3d3e3f",
	"404142434445464748494a4b4c4d4e4f",
	"505152535455565758595a5b5c5d5e5f",
	"606162636465666768696a6b6c6d6e6f",
	"707172737475767778797a7b7c7d7e7f",
	"808182838485868788898a8b8c8d8e8f",
	"909192939495969798999a9b9c9d9e9f",
	"a0a1a2a3a4a5a6a7a8a9aaabacadaeaf",
	"b0b1b2b3b4b5b6b7b8b9babbbcbdbebf",
	"c0c1c2c3c4c5c6c7c8c9cacbcccdcecf",
	"d0d1d2d3d4d5d6d7d8d9dadbdcdddedf",
	"e0e1e2e3e4e5e6e7e8e9eaebecedeeef",
	"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
};



int
WriteImage(file, buffer, rows, rowsize)
/*
Write a ATK format file for the image.
*/
FILE *file;		/* where to send the bits */
unsigned char buffer[];	/* where the bits are */
int rows;		/* number of rows of image data */
int rowsize;	/* number of bytes per row */

#define RASTERVERSION	2

{
int rowcount;

    if (fprintf(file, "\\begindata{%s,%d}\n", "raster", 1) < 0) {
	return(-1);
    }
    if (fprintf(file, "%d %d %d %d ", RASTERVERSION, 0, 65536, 65536) < 0) {
	return(-1);
    }

    if (fprintf(file, "%d %d %d %d\n", 0, 0, (rowsize * 8), rows) < 0) {	/* subraster */
	return(-1);
    }
    if (fprintf(file, "bits %d %d %d\n", 1, (rowsize * 8), rows) < 0) {
	return(-1);
    }

    for (rowcount = 0; rowcount < rows; rowcount++) {
	if (WriteRow(file, &buffer[(rowcount * rowsize)], rowsize) != 0) {
	    return(-1);
	}
    }

    if(fprintf(file, "\\enddata{%s, %d}\n", "raster", 1) < 0) {
	return(-1);
    }
    
    return(0);
}





int
WriteRow(file, byteaddr, nbytes)
	FILE *file;
	unsigned char *byteaddr;
	int nbytes;
{
	unsigned char curbyte;		/* byte enqueued for output */
	unsigned char c;		/* incoming byte */
	long curcnt;			/* number of occurrences of curbyte */
	unsigned char *bend;		/* addr of byte following the row */
	long hexval;			/* temp. for outgoing byte code */
	unsigned char savechar;		/* save character replaced */
	long outcnt;			/* count bytes output to this column */
	long colcnt;			/* count columns per line */

#define WHITEBYTE 0x00
#define BLACKBYTE 0xff
	

/* codes for data stream */
#define WHITEZERO	'f'
#define WHITETWENTY	'z'
#define BLACKZERO	'F'
#define BLACKTWENTY	'Z'
#define OTHERZERO	0x1F



	bend = byteaddr + nbytes;
	savechar = *bend;
	*bend = *(bend-1) ^ 0x3;	/* make *bend differ from *(bend-1) to
				ensure flushing a code for *(bend-1) 
				WARNING: modifies and restores the pixelimage bits XXX*/

	outcnt = 0;
	colcnt = 0;
	curbyte = *byteaddr++;		/* get first byte */
	curcnt = 1;		/* and count it */
	while (byteaddr <= bend) {
		c = *byteaddr++;	/* get next byte */
		if (c == curbyte)
			/* same as enqueued byte, just count it */
			curcnt++;
		else {
			/* flush the enqueued byte */

			/* output spacing */
			if (outcnt >= 13) {
				if ((++colcnt & 0x3) == 0)
					fputc('\n', file), fputc(' ', file);
				else	fputc('\t', file);
				outcnt = 0;
			}
			/* generate the encoding */
			switch (curbyte) {
			case WHITEBYTE:
				while (curcnt > 20) 
					fputc(WHITETWENTY, file),
					outcnt++, curcnt -= 20;
				fputc(WHITEZERO + curcnt, file), outcnt++;
				break;
			case BLACKBYTE:
				while (curcnt > 20) 
					fputc(BLACKTWENTY, file),
					outcnt++, curcnt -= 20;
				fputc(BLACKZERO + curcnt, file), outcnt++;
				break;
			default:
				/* get two byte code for curbyte's value */
				hexval = *(((unsigned short *)hex) + curbyte);

				while (curcnt > 16)
					fputc(OTHERZERO+16, file),
					fputc(hexval>>8, file),
					fputc(hexval, file), outcnt += 3,
					curcnt -= 16;
				if (curcnt > 1)
					fputc(OTHERZERO+curcnt, file), outcnt++;
				else ;  /* the byte written will represent a single instance */
				fputc(hexval>>8, file), fputc(hexval, file), outcnt += 2;
				break;
			}
			/* enqueue the new incoming byte */
			curbyte = c;
			curcnt = 1;
		}
	}
	fputc(' ', file);  fputc('|', file);  fputc('\n', file);	/* end of row indication */

	*bend = savechar;			/* restore the modified byte */
	
	return(0);	/* for now assume no error, writing trailer will catch too big a file */
}





/*
IBM's IDS format write functions
*/

struct {
    unsigned char	IDS_BS_0;	/* begin segment ID */
    unsigned char	IDS_BS_1;	/* length of params */
    unsigned char	IDS_BS_2;
    unsigned char	IDS_BS_3;
    unsigned char	IDS_BS_4;
    unsigned char	IDS_BS_5;
} IDS_BS = {0x70, 0x04, 0x00, 0x00, 0x00, 0x00};
#define IDS_BS_LEN 6

struct {
    unsigned char	IDS_ES_0;	/* end segment ID */
    unsigned char	IDS_ES_1;	/* length of params */
} IDS_ES = {0x71, 0x00};
#define IDS_ES_LEN 2

struct {
    unsigned char	IDS_BIC_0;	/* begin image content ID */
    unsigned char	IDS_BIC_1;	/* length of params */
    unsigned char	IDS_BIC_2;	/* IDS format */
} IDS_BIC = {0x91, 0x01, 0xff};
#define IDS_BIC_LEN 3

struct {
    unsigned char	IDS_EIC_0;	/* end image content ID */
    unsigned char	IDS_EIC_1;	/* length of params */
} IDS_EIC = {0x91, 0x00};
#define IDS_EIC_LEN 2

struct {
    unsigned char	IDS_IS_0;	/* image size ID */
    unsigned char	IDS_IS_1;	/* length of params */
    unsigned char	IDS_IS_2;	/* units of 10 inches */
    unsigned char	IDS_IS_3;	/* H resolution MSB */
    unsigned char	IDS_IS_4;	/* H resolution LSB */
    unsigned char	IDS_IS_5;	/* V resolution MSB */
    unsigned char	IDS_IS_6;	/* V resolution LSB */
    unsigned char	IDS_IS_7;	/* H size MSB */
    unsigned char	IDS_IS_8;	/* H size LSB */
    unsigned char	IDS_IS_9;	/* V size MSB */
    unsigned char	IDS_IS_10;	/* V size LSB */
} IDS_IS = {0x94, 0x09, 0x00};
#define IDS_IS_LEN 11

struct {
    unsigned char	IDS_IE_0;	/* image encoding ID */
    unsigned char	IDS_IE_1;	/* length of params */
    unsigned char	IDS_IE_2;	/* uncompressed */
    unsigned char	IDS_IE_3;	/* RIDIC recording */
} IDS_IE = {0x95, 0x02, 0x03, 0x01};
#define IDS_IE_LEN 4

struct {
    unsigned char	IDS_ID_0;	/* image data ID MSB */
    unsigned char	IDS_ID_1;	/* image data ID LSB */
    unsigned char	IDS_ID_2;	/* length of data MSB */
    unsigned char	IDS_ID_3;	/* length of data LSB */
} IDS_ID = {0xfe, 0x92};
#define IDS_ID_LEN 4








int
WriteIDS(file, buffer, YPixels, XPixels, Size, Resolution)
/*
Write an IBM IDS format file for the image.
*/
FILE *file;		/* where to send the bits */
unsigned char buffer[];	/* where the bits are */
int YPixels;	/* number of rows of image data */
int XPixels;	/* number of pixels per row in image */
int Size;		/* number of bytes per row */
int Resolution;	/* 0 = 120 pixels/inch, 1 = 240 */


{
int rowcount;
int rowsize;

    rowsize = Size / YPixels;

/* fill in the IDS_IS and IDS_ID header structures */

    if (Resolution == 0) {
	IDS_IS.IDS_IS_3 = 0x04;    /* set for 1200 pixel/10 inch */
	IDS_IS.IDS_IS_4 = 0xb0;
	IDS_IS.IDS_IS_5 = 0x04;
	IDS_IS.IDS_IS_6 = 0xb0;
    } else {
	IDS_IS.IDS_IS_3 = 0x09;    /* set for 2400 pixel/10 inch */
	IDS_IS.IDS_IS_4 = 0x60;
	IDS_IS.IDS_IS_5 = 0x09;
	IDS_IS.IDS_IS_6 = 0x60;
    }

    IDS_IS.IDS_IS_7 = (XPixels	>> 8) &	0xff;	/* H size MSB */
    IDS_IS.IDS_IS_8 = XPixels & 0xff;		/* H size LSB */
    IDS_IS.IDS_IS_9 = (YPixels	>> 8) &	0xff;	/* V size MSB */
    IDS_IS.IDS_IS_10 =	YPixels	& 0xff;		/* V size LSB */


    IDS_ID.IDS_ID_2 = (rowsize >> 8) & 0xff;	/* row size in bytes */
    IDS_ID.IDS_ID_3 = rowsize & 0xff;


/* Begin Segment */
    if (fwrite(&IDS_BS, IDS_BS_LEN, 1, file) < 1) {  
	return(-1);
    }

/* Begin Image Content */
    if (fwrite(&IDS_BIC, IDS_BIC_LEN, 1, file) < 1) {
	return(-1);
    }

/* Image Size */
    if (fwrite(&IDS_IS,	IDS_IS_LEN, 1, file) < 1) {
	return(-1);
    }

/* Image Encoding */
    if (fwrite(&IDS_IE,	IDS_IE_LEN, 1, file) < 1) {
	return(-1);
    }

  
    for (rowcount = 0; rowcount < YPixels; rowcount++) {
	if (fwrite(&IDS_ID, IDS_ID_LEN, 1, file) < 1) {
	    return(-1);
	}
	if (fwrite(&buffer[(rowcount * rowsize)], rowsize, 1, file) < 0) {
	    return(-1);
	}
    }


/* End Image Content */
    if (fwrite(&IDS_EIC, IDS_EIC_LEN, 1, file) < 1) {
	return(-1);
    }

/* End Segment */
    if (fwrite(&IDS_ES, IDS_ES_LEN, 1, file) < 1) {  
	return(-1);
    }

return(0);
} 


#else /* #ifdef SUPPORTTED_PLATFORM */

#include <stdio.h>

main()
{
    printf("\nscan:   The scan command is not supported on this machine.\n");
    exit(128);
}

#endif /* #ifdef SUPPORTTED_PLATFORM */


