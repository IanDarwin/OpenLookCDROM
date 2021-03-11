/* g3.ch - class description for interface from G3 fax format to image */
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

/* g3topbm.c - read a Group 3 FAX file and produce a portable bitmap
**
** Copyright (C) 1989 by Paul Haeberli <paul@manray.sgi.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

/** g3.c - read a Group 3 FAX file and product a bitmap
 **
 ** Adapted from Paul Haeberli's <paul@manray.sgi.com> G3 to Portable Bitmap 
 ** code.
 **
 ** modified by jimf on 09.18.90 to fail on any load error.  this was done
 ** to cut down on the false positives caused by a lack of any read ID
 ** string.  the old errors are currently ifdef'ed out -- if you want 'em
 ** define ALLOW_G3_ERRORS.
 **/

/* Edit History

04/15/91   8 nazgul	Sanity check line widths
			Doing a zclose on all failures
04/14/91   1 schulert	add <sys/types> for SYSV systems
04/13/91   6 nazgul	Handle reinvocation on the same file
04/13/91   5 nazgul	Bug fix to retry with bitreversed, and do not double allocate on multiple calls
04/12/91   4 nazgul	Spot faxes that do not have a 000 header
			Handle faxes that have the bytes in the wrong order
07/03/90   2 nazgul	Added recovery for premature EOF
*/

#include <sys/types.h>
#include <sys/file.h>

#include <image.ih>
#include <g3.h>
#include <g3.eh>

/* SUPPRESS 530 */
/* SUPPRESS 558 */
/* SUPPRESS 560 */

/****
 **
 ** Local defines
 **
 ****/

#define BITS_TO_BYTES(bits)	(bits/8)+((bits-((bits/8)*8)?1:0))
#define TABSIZE(tab) (sizeof(tab)/sizeof(struct tableentry))
#ifdef VMS
#define cols vmscols
#endif

/****
 **
 ** Local variables
 **
 ****/

static int g3_eof = 0;
static int g3_eols;
static int g3_rawzeros;
static int g3_Xrawzeros;
static int	maxlinelen;
static int	rows, cols;
static int g3_error = 0;
static int g3_verb;
static int curbit;

#define MAX_ERRORS	20

/****
 **
 ** Local tables
 **
 ****/

tableentry *whash[HASHSIZE];
tableentry *bhash[HASHSIZE];

int	g3_addtohash(hash, te, n, a, b)
	tableentry	*hash[];
	tableentry	*te;
	int	n, a, b;
{
	unsigned int pos;

	while (n--) {
		pos = ((te->length+a)*(te->code+b))%HASHSIZE;
		if (hash[pos] != 0) {
#ifdef ALLOW_G3_ERRORS
			fprintf(stderr, "G3: Hash collision during initialization.\n");
			exit(1);
#else
			++g3_error;
			return(-1);
#endif
			}
		hash[pos] = te;
		te++;
	}
}

tableentry	*g3_hashfind(hash, length, code, a, b)
	tableentry	*hash[];
	int	length, code;
	int	a, b;
{
	unsigned int pos;
	tableentry *te;

	pos = ((length+a)*(code+b))%HASHSIZE;
	if (pos >= HASHSIZE) {
#ifndef ALLOW_G3_ERRORS
		fprintf(stderr, "G3: Bad hash position, length %d code %d pos %d.\n", 
			length, code, pos);
		exit(2);
#else
		++g3_error;
		return(NULL);
#endif
		}
	te = hash[pos];
	return ((te && te->length == length && te->code == code) ? te : 0);
}

int	g3_getfaxrow(fd, bitrow)
	ZFILE	*fd;
	byte	*bitrow;
{
        int col;
	int curlen, curcode, nextbit;
	int count, color;
	tableentry *te;

	/* First make the whole row white... */
	bzero((char *) bitrow, maxlinelen); /* was memset -- jimf 09.11.90 */

	col = 0;
	g3_rawzeros = 0;
	curlen = 0;
	curcode = 0;
	color = 1;
	count = 0;
	while (!g3_eof) {
		if (col >= MAXCOLS) {
#ifdef ALLOW_G3_ERRORS
			if (g3_verb) fprintf(stderr, "G3: Input row %d is too long, skipping to EOL.\n", rows);
			g3_skiptoeol(fd);
			++g3_error;
			return (col); 
#else
			return(-1);
#endif
			}
		do {
			if (g3_eof) return 0;
			if (g3_rawzeros >= 11) {
				nextbit = g3_rawgetbit(fd);
				if (nextbit) {
					if ( col == 0 )
						/* 6 consecutive EOLs mean end of document */
						g3_eof = (++g3_eols >= 5);
					else
						g3_eols = 0;

					return (col); 
					}
				}
			else
				nextbit = g3_rawgetbit(fd);

			curcode = (curcode<<1) + nextbit; 
			curlen++;
			} while (curcode <= 0);

		/* No codewords are greater than 13 bytes */
		if (curlen > 13) {
#ifdef ALLOW_G3_ERRORS
			if (g3_verb) fprintf(stderr, "G3: Bad code word at row %d, col %d (len %d code 0x%2.2x), skipping to EOL.\n", rows, col, curlen, curcode );
			g3_skiptoeol(fd);
			++g3_error;
			return (col);
#else
			return(-1);
#endif
			}
		if (color) {
			/* White codewords are at least 4 bits long */
			if (curlen < 4)
				continue;
			te = g3_hashfind(whash, curlen, curcode, WHASHA, WHASHB);
			}
		else {
			/* Black codewords are at least 2 bits long */
			if (curlen < 2)
				continue;
			te = g3_hashfind(bhash, curlen, curcode, BHASHA, BHASHB);
		}
		if (!te)
			continue;
		switch (te->tabid) {
			case TWTABLE:
			case TBTABLE:
				count += te->count;
				if (col+count > MAXCOLS) 
					count = MAXCOLS-col;
				if (count > 0) {
					if (color) {
						col += count;
						count = 0;
						}
					else
						g3_bitson(bitrow, col, count);
					}
				curcode = 0;
				curlen = 0;
				color = !color;
				break;
			case MWTABLE:
			case MBTABLE:
				count += te->count;
				curcode = 0;
				curlen = 0;
				break;
			case EXTABLE:
				count += te->count;
				curcode = 0;
				curlen = 0;
				break;
			default:
				fprintf(stderr, "G3: Bad table id from table entry.\n");
#ifndef ALLOW_G3_ERRORS
				exit(3);
#else
				++g3_error;
				return(-1);
#endif
			}
		}
	return (0);
}

int	g3_skiptoeol(fd)
	FILE	*fd;
{
	while (g3_rawzeros<11 && !g3_eof)
		(void) g3_rawgetbit(fd);
	while(!g3_rawgetbit(fd) && !g3_eof);
	return(0);
}

int	g3_rawgetbit(fd)
	FILE	*fd;
{
	int	b;
	static int	shdata;

	if (curbit >= 8) {
		shdata = fgetc(fd);
		if (shdata == EOF) {
#ifdef ALLOW_G3_ERRORS
		        if (g3_verb) fprintf(stderr, "G3: Premature EOF at line %d.\n", rows);
			g3_eols = 5;
			g3_eof = 1;
			++g3_error;
			return 0;
#else
			return(-1);
#endif
			}
		curbit = 0;
		}
	if (shdata & bmask[curbit]) {
		g3_Xrawzeros = g3_rawzeros;
		g3_rawzeros = 0;
		b = 1;
		}
	else {
		g3_rawzeros++;
		b = 0;
		}
	curbit++;
    return b;
}

int	g3_bitson(b, c, n)
	bit	*b;
	int	c, n;
{
	int	i, col;
	bit	*bP;
	static int	bitmask[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };

	bP = b;
	col = c;
	bP+=(c/8);
	i = (c - ((c/8)*8));
	while(col <= (c+n)) { 
		for(;col <= (c+n) && i < 8; i++) {
			*bP |= bitmask[i];
			col++;
			}
		i = 0;
		bP++;
		}
	return(0);
}

/* All G3 images begin with a G3 EOL codeword which is eleven binary 0's
 * followed by one binary 1.  There could be up to 15 0' so that the image
 * starts on a char boundary.
 */
/*
 * They are all *supposed* to, but in fact some don't.  In fact pbmtog3 doesn't seem
 * to generate them.  So if that fails, we'll also try reading a line and seeing if
 * we get any errors.  Note that this means we had to move the call to g3_ident
 * to after the hash table init.  -nazgul
 */

int	g3_ident(fd)
	FILE	*fd;
{

    int		ret = 0, col1, col2, i;
    byte	*tmpline;
    int		reverse = 0;
    struct cache	*dataptr;
    int			bufptr;
    
    g3_verb = 0;
    tmpline = (byte *) malloc(maxlinelen);

    /* In case this got reset by a previous pass through here */
    for (i = 0; i < 8; ++i) {
	bmask[7-i] = 1 << i;
    }

tryagain:
    curbit = 8;
    g3_Xrawzeros = 0;
    g3_eof = g3_eols = rows = cols = 0;
    
    /* If we have the zeros we're off to a good start, otherwise, skip some lines */
    for (g3_rawzeros = 0; !g3_rawgetbit(fd) && !g3_eof;);
    if (g3_Xrawzeros >= 11 && g3_Xrawzeros <= 15) {
	fd->dataptr = fd->data;
	fd->bufptr = 0;
	curbit = 8;
	g3_skiptoeol(fd);
	if (!g3_error) g3_skiptoeol(fd);
	if (!g3_error) g3_skiptoeol(fd);
	if (!g3_error) g3_skiptoeol(fd);
    } else ret = 1;

    /* Now get two lines and make sure they are the same length.  If not give up.
     * Note that it is possible for this to give false positives (value.o on a Sun IPC
     * did) but it's unlikely enough that I think we're okay.
     */
    
    dataptr = fd->dataptr;
    bufptr = fd->bufptr;
    if (!g3_error) col1 = g3_getfaxrow(fd, tmpline);
    if (!g3_error) col2 = g3_getfaxrow(fd, tmpline);
    if (!g3_error && col1 == col2 && col1 != 0) ret = 1;
    else ret = 0;
    /* if (ret) printf("%d = %d\n", col1, col2); */
    fd->dataptr = dataptr;
    fd->bufptr = bufptr;
    curbit = 8;

    /* This bogus hack is to accomodate some fax modems which apparently use a chip
     * with a different byte order.  We simply try again with the table reversed.
     */
    if (!ret && !reverse) {
	rows = cols = g3_error = 0;
	fd->dataptr = fd->data;
	fd->bufptr = 0;
	g3_Xrawzeros = 0;
	for (i = 0; i < 8; ++i) {
	    bmask[i] = 1 << i;
	}
	reverse = 1;
	goto tryagain;
    }
    g3_eols = rows = cols = 0;
    free(tmpline);

    return(ret);
}

int
g3__Load( g3, fullname, fp )
    struct g3 *g3;
    char *fullname;
    FILE *fp;
{

	FILE	*fd;
	int i, col;
	byte	*currline;
	static int firstTime = 1;

	if((f = fp) == 0) {
	    if (! (f = fopen(fullname, "r"))) {
		fprintf(stderr, "Couldn't open g3 file %s.\n", fullname);
		return(-1);
	    }
	}

	if (firstTime) {
	    firstTime = 0;
	    
	    /* Initialize and load the hash tables */
	    for ( i = 0; i < HASHSIZE; ++i )
	      whash[i] = bhash[i] = (tableentry *) 0;
	    g3_addtohash(whash, twtable, TABSIZE(twtable), WHASHA, WHASHB);
	    g3_addtohash(whash, mwtable, TABSIZE(mwtable), WHASHA, WHASHB);
	    g3_addtohash(whash, extable, TABSIZE(extable), WHASHA, WHASHB);
	    g3_addtohash(bhash, tbtable, TABSIZE(tbtable), BHASHA, BHASHB);
	    g3_addtohash(bhash, mbtable, TABSIZE(mbtable), BHASHA, BHASHB);
	    g3_addtohash(bhash, extable, TABSIZE(extable), BHASHA, BHASHB);
	}
	
	g3_eof = g3_eols = 0;
	curbit = 8;	/* Reset on multiple reads */

	/* Calulate the number of bytes needed for maximum number of columns 
	 * (bits), create a temprary storage area for it.
	 */
	maxlinelen = BITS_TO_BYTES(MAXCOLS);

	if (!g3_ident(fd)) {
	    fclose(fd);
	    return(-1);
	}
	g3_verb = verbose;

	g3_newBitImage(g3, MAXCOLS, MAXROWS);

	currline = g3_Data(g3);
	cols = 0;
	for (rows = 0; rows < MAXROWS; ++rows) {
		col = g3_getfaxrow(fd, currline);
#ifndef ALLOW_G3_ERRORS
		if (col < 0) {
		  g3_Reset(g3);
		  fclose(fd);
		  return(-1);
		}
#else
		if (g3_error > MAX_ERRORS) {
		  g3_Reset(g3);
		  fclose(fd);
		  return(-1);
	      }		    
#endif
		if (g3_eof)
			break;
		if (col > cols)
			cols = col;
		currline += BITS_TO_BYTES(cols);
		}

	fclose(fd);
	g3_Width(g3) = cols;
	g3_Height(g3) = rows;
	if (!g3_Width(g3) || !g3_Height(g3)) { /* sanity check */
		g3_Reset(g3);
		return(-1);
	}

    return(0);
}

/* originally this used only g3_ident to determine if it was a G3 image, but
 * it was always getting false positives so now it loads the whole image in
 * to see if it's reasonable.
 */
int	g3Ident( fullname )
	char	*fullname;
{ struct g3 *g3 = g3_New();

	g3_verb = 0;
	if (g3_Load(g3, fullname, NULL) == 0) {
		g3_Destroy(g3);
		return(1);
	}
	return(0);
}

long
g3__Read( self, file, id )
    struct g3 *self;
    FILE *file;
    long id;
{
    if(g3_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
g3__Write( self, file, writeID, level )
    struct g3 *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
g3__WriteNative( self, file, filename )
    struct g3 *self;
    FILE *file;
    char *filename;
{
return(0);
}
