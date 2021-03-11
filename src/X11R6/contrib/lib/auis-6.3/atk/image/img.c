/* img.c - class description for interface from IMG Bit Image format to image */

/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

/****
  Copyright 1991 Tim Northrup

  Permission to use, copy, modify, distribute, and sell this
  software and its documentation for any purpose is hereby granted
  without fee, provided that the above copyright notice appear in
  all copies and that both that copyright notice and this
  permission notice appear in supporting documentation. The
  author makes no representations about the suitability of this
  software for any purpose. It is provided "as is" without express
  or implied warranty.

  THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
  OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
****/

#include <andrewos.h>
#include <stdio.h>
#include <image.ih>
#include <img.h>
#include <img.eh>

static int Debug = 0;				/* Set to 1 for info */

static unsigned char *BitRow;			/* working row of bits */
static int RowRepeat;				/* number of repititons */
static int RowCount;				/* current row */
static int ColCount;				/* current column */

static int IMG_ReadHeader();			/* Read file header */
static void IMG_WriteByte();			/* Write output byte */

/*
**  imgIdent
**
**	Identify passed file as a GEM Bit Image or not
**
**	Returns 1 if file is a GEM Image, 0 otherwise
*/

int 
img__Ident( classID, fullname)
    struct classheader classID;
    char *fullname;
{
	FILE *f;				/* Input file */
	IMG_Header header;			/* GEM Image header info */
	int ret = 0;			/* Return value */

	if ( ! (f = fopen(fullname, "r")))
		return(0);

	if (IMG_ReadHeader(f,&header) == 0) {
		printf("%s is a %dx%d GEM Bit Image\n", fullname,
		       header.llen, header.lines);
		ret = 1;
	      }

	fclose(f);

	return(ret);
}

/*
**  imgLoad
**
**	Load GEM Image file into an Image structure.
**
**	Returns pointer to allocated struct if successful, NULL otherwise
*/

int
img__Load ( img, fullname, fp )
    struct img *img;
    char *fullname;
    FILE *fp;
{
	register int i;				/* Random index */
	FILE *f;				/* Input file */
	short creps;				/* Repetition counter */
	short ictr;				/* Secondary index counter */
	unsigned int bpl;			/* Bytes per scanline */
	IMG_Header header;			/* GEM Image header */
	unsigned char inbuf[MAX_LITERAL], ichr;	/* Input buffer/char */

	if((f = fp) == 0) {
	    if (! (f = fopen(fullname, "r"))) {
		fprintf(stderr, "Couldn't open img file %s.\n", fullname);
		return(-1);
	    }
	}

	if ((i = IMG_ReadHeader(f,&header)) != 0) {
		fclose(f);
		switch (i) {
		case 1: /* short read in header */
		case 2: /* not a GEM image */
		  break;
		case 3:
		  printf("  Color GEM images are not supported (yet)\n");
		  break;
		case 4:
		  printf("  GEM image is too large\n");
		  break;
		default:
		  printf("  Unsupported GEM image pattern length\n");
		  break;
		}
		return(-1);
	      }

	img_newBitImage(img, header.llen, header.lines);

	BitRow = &(img_Data(img)[0]);

	bpl = header.llen / 8;				/* bytes per line */
	ColCount = 0;					/* starting column */
	RowCount = 0;					/* starting row */
	RowRepeat = 1;					/* default repeat */

	while (RowCount < header.lines) {

		/* While the last line has not been reached ... */

		i = fgetc(f);
		if (i == EOF) {
			printf("GEM Image Failure: premature EOF\n");
			break;
			}

		if (i == BYTE_FLAG) {		/* scanline */
			i = fgetc(f);
			if (i == EOF) {
				printf("GEM Image Failure: premature EOF\n");
				break;
				}
			if (i == BYTE_SLREPEAT) {	/* repeat scanline */
				if (ColCount != 0) {
				    printf("GEM Image Failure: corrupted\n");
				    break;
				    }
				i = fgetc(f);		/* SLFLAG byte (0xFF) */
				if (i != BYTE_SLFLAG) {
				    printf("GEM Image Failure: corrupted\n");
				    break;
				    }
				RowRepeat = fgetc(f);   /* repeat count */
				}
			else {			/* repeat pattern_len */
				creps = i;
				fread(inbuf, sizeof(unsigned char), header.patlen, f);
				for (i = 0 ; i < creps ; i++)
				    for (ictr = 0 ; ictr < header.patlen ; ictr++)
					IMG_WriteByte(inbuf[ictr],header.llen,bpl);
				}
			}
		else if (i == BYTE_LITERAL) {	/* literal follows */
			creps = fgetc(f);		/* literal length */
			fread(inbuf,sizeof(unsigned char), creps, f);		/* literal */
			for (ictr = 0 ; ictr < creps ; ictr++)
				IMG_WriteByte(inbuf[ictr],header.llen,bpl);
			}
		else {			/* monochrome mode bits */
			creps = RUN_LENGTH(i);
			ichr = (RUN_COLOR(i) == 1) ?
					BYTE_BLACK : BYTE_WHITE;
			for (i = 0 ; i < creps ; i++)
				IMG_WriteByte(ichr,header.llen,bpl);
			}
		}

	fclose(f);

	return(0);
}

/*
**  IMG_ReadHeader
**
**	Read in IMG file header information, skip extras.
**
**	Returns 0 if successful
**		1 if not enough file for header record
**		2 if header data is nonsensical/invalid
**		3 if image is color (unsupported)
**		4 if image is too wide ( > 8K bits!!)
**		5 if pattern length is too big ( > 255)
*/

static int IMG_ReadHeader (f,h)

    FILE      *f;
    IMG_Header *h;
{
	register int tlen;			/* total to read in */
	register int rlen;			/* read lengths */
	unsigned char junkbuffer[MAX_SCANLINE];	/* scrap buffer */

	tlen = fread(junkbuffer, sizeof(unsigned char), (DEF_HLEN * 2), f);
	if (tlen != (DEF_HLEN * 2))
		return(1);			/* not enough data */

	/* convert from big-endian to machine specific */

	h->vers   = junkbuffer[1] + (256 * junkbuffer[0]);
	h->hlen   = junkbuffer[3] + (256 * junkbuffer[2]);
	h->colors = junkbuffer[5] + (256 * junkbuffer[4]);
	h->patlen = junkbuffer[7] + (256 * junkbuffer[6]);
	h->pixw   = junkbuffer[9] + (256 * junkbuffer[8]);
	h->pixh   = junkbuffer[11] + (256 * junkbuffer[10]);
	h->llen   = junkbuffer[13] + (256 * junkbuffer[12]);
	h->lines  = junkbuffer[15] + (256 * junkbuffer[14]);

	/* sanity check; if fields don't look right, it's probably not a GEM
	 * image.
	 */

	if ((h->vers != DEF_VERSION) ||
	    (h->hlen < DEF_HLEN) ||
	    (h->colors < 0) || (h->colors > 256) ||
	    (h->pixw < 1) ||
	    (h->pixh < 1) ||
	    (h->llen < 1) || (h->llen > (MAX_SCANLINE * 8)) ||
	    (h->lines < 1) || (h->lines > 8192) ||
	    (h->patlen < 0) || (h->patlen > MAX_LITERAL))
	  return(2);

	if (Debug) {
		fprintf(stderr,"Header Information:\n");
		fprintf(stderr,"\tIMG Version:  %d\n",h->vers);
		fprintf(stderr,"\t Header Len:  %d\n",h->hlen);
		fprintf(stderr,"\t     Colors:  %d\n",h->colors);
		fprintf(stderr,"\tPattern Len:  %d\n",h->patlen);
		fprintf(stderr,"\t Pixel Size:  %d x %d (microns=1000th mm)\n",
				h->pixw,h->pixh);
		fprintf(stderr,"\t Image Size:  %d x %d (pixels)\n",
				h->llen,h->lines);
		}

	/* validity check on particular fields
	 */

	if (h->colors != 1)
		return(3);
	if (h->llen > (MAX_SCANLINE * 8))
		return(4);
	if (h->patlen > MAX_LITERAL)
		return(5);

	/* make life easier if not on an even boundary */
	if (h->llen % 8) {
		h->llen += (8 - (h->llen % 8));
		if (Debug)
			fprintf(stderr,"Image expanded to %d pixels wide\n",
				h->llen);
		}

	/* skip additional header information if present */
	if (h->hlen > DEF_HLEN) {
		tlen = ((h->hlen - DEF_HLEN) * 2);
		if (Debug)
			fprintf(stderr,"%d bytes of extra header skipped",tlen);
		for ( ; tlen > 0 ; tlen -= rlen) {
			rlen = (tlen > MAX_SCANLINE) ? MAX_SCANLINE : tlen;
			fread(junkbuffer, sizeof(unsigned char), rlen, f);
		}
	}

	return(0);
}



/*
**  IMG_WriteByte
**
**	Add byte to image; if end of scanline, may need to replicate it
**
**	Returns no value (void function)
*/

static void IMG_WriteByte (c,cols,bpl)

    unsigned char c;
    register int cols;
    unsigned int bpl;
{
	register int i;
	register unsigned char *ptr;
	register unsigned char *ptr2;

	BitRow[ColCount] = c;

	if (++ColCount >= bpl) {			/* end of scanline */
		ptr2 = BitRow + bpl;
		RowCount++;			/* count one already out */
		while (--RowRepeat > 0) {
			for (ptr = BitRow, i = 0 ; i < bpl ; i++, ptr++)
				*ptr2++ = *ptr;
			RowCount++;
			}
		BitRow = ptr2;
		ColCount = 0;
		RowRepeat = 1;
		}

	return;
}

long
img__Read( self, file, id )
    struct img *self;
    FILE *file;
    long id;
{
    if(img_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
img__Write( self, file, writeID, level )
    struct img *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
img__WriteNative( self, file, filename )
    struct img *self;
    FILE *file;
    char *filename;
{
return(0);
}
