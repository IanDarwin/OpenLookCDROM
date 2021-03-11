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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/lib/RCS/rasterio.c,v 2.9 1992/12/15 21:40:02 rr2b R6tape $";
#endif


 


/*  rasterio.c

	rasterio package

	Routines for reading and writing rasters in .raster form
	(BE2 rasters version 2.)

	Known problems:
		We could fix ReadRow to not check length before each code byte.

 */
#include <stdio.h>

#include <class.h>
#include <rasterio.eh>
#include <pixelimg.ih>

#define class_StaticEntriesOnly
#include <dataobj.ih>		/* for read return values */
#undef class_StaticEntriesOnly

/* codes for data stream */
#define WHITEZERO	'f'
#define WHITETWENTY	'z'
#define BLACKZERO	'F'
#define BLACKTWENTY	'Z'
#define OTHERZERO	0x1F


/* WriteRow table for conversion of a byte value to two character hex representation */

static unsigned char hex[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};



/* rasterio__WriteRow(file, byteaddr, nbytes)  
		Writes to 'file' the encoded version of the 'nbytes' bytes
		beginning at byte with address 'byteaddr'.

		The general scheme is that no byte is output until it has spent
		at least one cycle in 'curbyte'.  Each incoming byte (in 'c')
		is compared against 'curbyte' and if they match the only 
		action is to increment the counter 'curcnt'.  If they 
		do not match, an output code is generated for 'curbyte'
		and its value is replaced by the incoming byte.
		To force the last byte out, the byte following it is temporarily
		replaced with a code guaranteed to be different.

		The data is encoded with 4 columns to a line and lines
		usually have about fifteen bytes.  Column entries can be longer
		if long runs of similar bytes are encountered.
*/
	void
rasterio__WriteRow(ClassID, file, byteaddr, nbytes)
	struct classhdr *ClassID;
	FILE *file;
	unsigned char *byteaddr;
	long nbytes;
{
	unsigned char curbyte;	/* byte enqueued for output */
	unsigned char c;		/* incoming byte */
	long curcnt;			/* number of occurrences of curbyte */
	unsigned char *bend;	/* addr of byte following the row */
	unsigned char savechar;		/* save character replaced */
	long outcnt;			/* count bytes output to this column */
	long colcnt;			/* count columns per line */
	
	bend = byteaddr + nbytes;
	savechar = *bend;
	*bend = *(bend-1) ^ 0x3;	/* make *bend differ from *(bend-1) to
				ensure flushing a code for *(bend-1) 
				WARNING: modifies and restores the pixelimage bits XXX*/

	outcnt = colcnt = 0;
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
				while (curcnt > 16)
					fputc(OTHERZERO+16, file),
					fputc(hex[curbyte / 16], file),
					fputc(hex[curbyte & 15], file),
				        outcnt += 3,
					curcnt -= 16;
				if (curcnt > 1)
					fputc(OTHERZERO+curcnt, file), outcnt++;
				else ;  /* the byte written will represent a single instance */
				fputc(hex[curbyte / 16], file),
				fputc(hex[curbyte & 15], file),
				outcnt += 2;
				break;
			}
			/* enqueue the new incoming byte */
			curbyte = c;
			curcnt = 1;
		}
	}
	fputc(' ', file);  fputc('|', file);  fputc('\n', file);	/* end of row indication */

	*bend = savechar;			/* restore the modified byte */
}

/* rasterio__ReadRow(file, row, length) 
	Reads from 'file' the encoding of bytes to fill in 'row'.  Row will be
	truncated or padded (with WHITE) to exactly 'length' bytes.

	See itc/be2/raster/spec.d for a description of the encoding.

	Returns the code that terminated the row.  This may be
			'|'  	correct end of line
			'\0' 	if the length was satisfied (before a terminator)
			EOF 	if the file ended
			'\'  '{' 	other recognized ends. 
	The '|' is the expected end and pads the row with WHITE.
	The '\' and '{' are error conditions and may indicate the
	beginning of some other portion of the data stream.
	If the terminator is '\' or '{', it is left at the front of the input.
	'|' is gobbled up.
*/
/* macros to generate case entries for switch statement */
#define case1(v) case v
#define case4(v) case v: case (v)+1: case (v)+2: case(v)+3
#define case6(v) case4(v): case ((v)+4): case ((v)+5)
#define case8(v) case4(v): case4((v)+4)

	long
rasterio__ReadRow(ClassID, file, row, length)
	struct classhdr *ClassID;
	register FILE *file;		/* where to get them from */
	register unsigned char *row;	/* where to put bytes */
	register long length;	/* how many bytes in row must be filled */
{
	/* Each input character is processed by the central loop.  There are 
		some input codes which require two or three characters for completion; 
		these are handled by advancing the state machine.  Errors are not 
		processed; instead the state machine is reset to the Ready state 
		whenever a character unacceptable to the curent state is read.  */
	enum stateCode {
			Ready, 		/* any input code is allowed */
			HexDigitPending,	/* have seen the first of a hex digit pair */
			RepeatPending, 	/* repeat code has been seen:
					must be followed by two hex digits */
			RepeatAndDigit};	/* have seen repeat code and its first
					following digit */
	enum stateCode InputState;	/* current state */
	register c;		/* the current input character */
	register long repeatcount = 0;	/* current repeat value */
	register long hexval;	/* current hex value */
	long pendinghex = 0;		/* the first of a pair of hex characters */
	
	/* We cannot exit when length becomes zero because we need to check 
	to see if a row ending character follows.  Thus length is checked only when we get
	a data generating byte.  If length then is zero, we ungetc the byte */

	InputState = Ready;
	while ((c=getc(file)) != EOF) 
				switch (c) {

	case8(0x0):
	case8(0x8):
	case8(0x10):
	case8(0x18):
	case1(' '):
		/* control characters and space are legal and completely ignored */
		break;
	case1(0x40):	/* '@' */
	case1(0x5B):	/* '[' */
	case4(0x5D):	/*  ']'  '^'  '_'  '`' */
	case4(0x7D):	/* '}'  '~'  DEL  0x80 */
	default:		/* all above 0x80 */
		/* error code:  Ignored at present.  Reset InputState. */
		InputState = Ready;
		break;

	case1(0x7B):	/* '{' */
	case1(0x5C):	/* '\\' */
		/* illegal end of line:  exit anyway */
		ungetc(c, file);		/* retain terminator in stream */
		/* DROP THROUGH */
	case1(0x7C):	/* '|' */
		/* legal end of row: may have to pad  */
		while (length-- > 0)
			*row++ = WHITEBYTE;
		return c;
	
	case1(0x21):
	case6(0x22):
	case8(0x28):
		/* punctuation characters:  repeat byte given by two succeeding hex chars */
		if (length <= 0) {
			ungetc(c, file);
			return('\0');
		}
		repeatcount = c - OTHERZERO;
		InputState = RepeatPending;
		break;

	case8(0x30):
	case8(0x38):
		/* digit (or following punctuation)  -  hex digit */
		hexval = c - 0x30;
		goto hexdigit;
	case6(0x41):
		/* A ... F    -  hex digit */
		hexval = c - (0x41 - 0xA);
		goto hexdigit;
	case6(0x61):
		/* a ... f  - hex digit */
		hexval = c - (0x61 - 0xA);
		goto hexdigit;

	case8(0x67):
	case8(0x6F):
	case4(0x77):
		/* g ... z   -   multiple WHITE bytes */
		if (length <= 0) {
			ungetc(c, file);
			return('\0');
		}
		repeatcount = c - WHITEZERO;
		hexval = WHITEBYTE;
		goto store;
	case8(0x47):
	case8(0x4F):
	case4(0x57):
		/* G ... Z   -   multiple BLACK bytes */
		if (length <= 0) {
			ungetc(c, file);
			return('\0');
		}
		repeatcount = c - BLACKZERO;
		hexval = BLACKBYTE;
		goto store;

hexdigit:
		/* process a hex digit.  Use InputState to determine
			what to do with it. */
		if (length <= 0) {
			ungetc(c, file);
			return('\0');
		}
		switch(InputState) {
		case Ready:
			InputState = HexDigitPending;
			pendinghex = hexval << 4;
			break;
		case HexDigitPending:
			hexval |= pendinghex;
			repeatcount = 1;
			goto store;
		case RepeatPending:
			InputState = RepeatAndDigit;
			pendinghex = hexval << 4;
			break;
		case RepeatAndDigit:
			hexval |= pendinghex;
			goto store;
		}
		break;

store:
		/* generate byte(s) into the output row 
			Use repeatcount, depending on state.  */
		if (length < repeatcount) 
			/* reduce repeat count if it would exceed
				available space */
			repeatcount = length;
		length -= repeatcount;	/* do this before repeatcount-- */
		while (repeatcount-- > 0)
				*row++ = hexval;
		InputState = Ready;
		break;

	} /* end of while( - )switch( - ) */
	return EOF;
}
#undef case1
#undef case4
#undef case6
#undef case8

/* rasterio__ReadImage(file, pix) 
	Read a raster image from 'file' and put it in 'pix' 
		return error code
*/
	long
rasterio__ReadImage(ClassID, file, pix)
	struct classhdr *ClassID;
	register FILE *file;			/* where to get bits from */
	register struct pixelimage *pix;	/* where to put them */
{
	register unsigned char *byteaddr;	/* where to store next row */
	register long row, W, nbytesfromfile;	/* count rows;  byte length of row */
	long version, options, xscale, yscale, xoffset, yoffset, subwidth, subheight;
	char keyword[6];
	long discardid, objectid;		/* id read for the incoming pixel image */
	long tc;				/* temp */
	long width, height;			/* dimensions of image */
	long result;


	if (fscanf(file, "\\begindata{raster,%ld", &discardid) != 1
				|| getc(file) != '}' || getc(file) != '\n') 
		return dataobject_NOTBE2DATASTREAM;

	fscanf(file, " %d ", &version);
	if (version < 2) 
		return dataobject_BADFORMAT;

	/* ignore all these features: */
	fscanf(file, " %u %ld %ld %ld %ld %ld %ld",  
		&options, &xscale, &yscale, &xoffset, 
		&yoffset, &subwidth, &subheight);

	/* scan to end of line in case this is actually something beyond V2 */
	while (((tc=getc(file)) != '\n') && (tc != '\\') && (tc != EOF)) {}

	/* read the keyword */
	fscanf(file, " %5s", keyword);
	if (strcmp(keyword, "bits") != 0)
		return dataobject_BADFORMAT;

	fscanf(file, " %d %d %d ", &objectid, &width, &height);

	if (width < 1 || height < 1 || width > 1000000 || height > 1000000) 
		return dataobject_BADFORMAT;

	pixelimage_Resize(pix, width, height);
	W = pixelimage_GetRowWidth(pix);
	nbytesfromfile = (width+7)>>3;
	byteaddr = pixelimage_GetBitsPtr(pix);
	result = dataobject_NOREADERROR;
	for (row = 0;   row < height;   row++, byteaddr += W) {
		long c = rasterio_ReadRow(file, byteaddr, nbytesfromfile);
		if (c != '|') {
			result = (c == EOF) 
				? dataobject_PREMATUREEOF
				: dataobject_BADFORMAT;
			break;
		}
	}
	pixelimage_NotifyObservers(pix, pixelimage_DATACHANGED);

	while (! feof(file) && getc(file) != '\\') {};	/* scan for \enddata */
	if (result == dataobject_NOREADERROR &&
			fscanf(file, "enddata{raster,%d", &discardid) != 1
				|| getc(file) != '}' || getc(file) != '\n') 
		result = dataobject_MISSINGENDDATAMARKER;

	return result;
}

#define DEFAULTSCALE (1<<16)
#define RASTERVERSION 2

/* rasterio__WriteImage(file, pix, sub) 
	Write a raster image from 'pix' to 'file'
*/
	void
rasterio__WriteImage(ClassID, file, pix, sub)
	struct classhdr *ClassID;
	register FILE *file;		/* where to put bits  */
	register struct pixelimage *pix;/* where to get them from */
	register struct rectangle *sub;
{
	long left, top, width, height;
	long buf[1000];
	int id = 91;			/* dummy identifier */
	register long nbytestofile;
	register long row, bottom;

	rectangle_GetRectSize(sub, &left, &top, &width, &height);

	fprintf(file, "\\begindata{raster,%d}\n", id);
	fprintf(file, "%ld %ld %ld %ld ", RASTERVERSION, 
			0, DEFAULTSCALE, DEFAULTSCALE);
	fprintf(file, "%ld %ld %ld %ld\n",
		 0, 0, width, height);	/* subraster */
	fprintf(file, "bits %ld %ld %ld\n", id, width, height);

	nbytestofile = (width+7)>>3;
	bottom = top + height;
	for (row = top; row < bottom; row++) {
		pixelimage_GetRow(pix, left, row, width, buf);
		rasterio_WriteRow(file, (unsigned char *)buf, nbytestofile);
	}

	fprintf(file, "\\enddata{raster, %d}\n", id);
}


