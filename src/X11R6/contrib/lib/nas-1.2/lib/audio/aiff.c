/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)aiff.c,v 1.5 1994/06/07 18:03:45 greg Exp $
 */

#include <stdio.h>
#include <malloc.h>
#include <audio/Aos.h>
#include <math.h>
#include <audio/aiff.h>
#include <audio/fileutil.h>

#define Err()		{ AiffCloseFile(ai); return NULL; }
#define readID(_f)	fread(_f, sizeof(AIFF_ID), 1, ai->fp)
#define cmpID(_x, _y)	strncmp((char *) (_x), (char *) (_y), sizeof(AIFF_ID))
#define PAD2(_x)	(((_x) + 1) & ~1)

/**
 * Copyright (C) 1988-1991 Apple Computer, Inc.
 * All rights reserved.
 *
 * Warranty Information
 *  Even though Apple has reviewed this software, Apple makes no warranty
 *  or representation, either express or implied, with respect to this
 *  software, its quality, accuracy, merchantability, or fitness for a
 *  particular purpose.  As a result, this software is provided "as is,"
 *  and you, its user, are assuming the entire risk as to its quality
 *  and accuracy.
 *
 * This code may be used and freely distributed as long as it includes
 * this copyright notice and the above warranty information.
 *
 * Machine-independent I/O routines for IEEE floating-point numbers.
 *
 * NaN's and infinities are converted to HUGE_VAL or HUGE, which
 * happens to be infinity on IEEE machines.  Unfortunately, it is
 * impossible to preserve NaN's in a machine-independent way.
 * Infinities are, however, preserved on IEEE machines.
 *
 * These routines have been tested on the following machines:
 *    Apple Macintosh, MPW 3.1 C compiler
 *    Apple Macintosh, THINK C compiler
 *    Silicon Graphics IRIS, MIPS compiler
 *    Cray X/MP and Y/MP
 *    Digital Equipment VAX
 *
 *
 * Implemented by Malcolm Slaney and Ken Turkowski.
 *
 * Malcolm Slaney contributions during 1988-1990 include big- and little-
 * endian file I/O, conversion to and from Motorola's extended 80-bit
 * floating-point format, and conversions to and from IEEE single-
 * precision floating-point format.
 *
 * In 1991, Ken Turkowski implemented the conversions to and from
 * IEEE double-precision format, added more precision to the extended
 * conversions, and accommodated conversions involving +/- infinity,
 * NaN's, and denormalized numbers.
 */

#ifndef HUGE_VAL
#define HUGE_VAL HUGE
#endif						/* HUGE_VAL */

/*
 * C O N V E R T   T O   I E E E   E X T E N D E D
 */

#define FloatToUnsigned(f)  \
    ((AuUint32)(((AuInt32)(f - 2147483648.0)) + 2147483647L + 1))

static void
ConvertToIeeeExtended(num, bytes)
double          num;
char           *bytes;
{
    int             sign;
    int             expon;
    double          fMant,
                    fsMant;
    AuUint32   hiMant,
                    loMant;

    if (num < 0)
    {
	sign = 0x8000;
	num *= -1;
    }
    else
    {
	sign = 0;
    }

    if (num == 0)
    {
	expon = 0;
	hiMant = 0;
	loMant = 0;
    }
    else
    {
	fMant = frexp(num, &expon);
	if ((expon > 16384) || !(fMant < 1))
	{			/* Infinity or NaN */
	    expon = sign | 0x7FFF;
	    hiMant = 0;
	    loMant = 0;		/* infinity */
	}
	else
	{			/* Finite */
	    expon += 16382;
	    if (expon < 0)
	    {			/* denormalized */
		fMant = ldexp(fMant, expon);
		expon = 0;
	    }
	    expon |= sign;
	    fMant = ldexp(fMant, 32);
	    fsMant = floor(fMant);
	    hiMant = FloatToUnsigned(fsMant);
	    fMant = ldexp(fMant - fsMant, 32);
	    fsMant = floor(fMant);
	    loMant = FloatToUnsigned(fsMant);
	}
    }

    bytes[0] = expon >> 8;
    bytes[1] = expon;
    bytes[2] = hiMant >> 24;
    bytes[3] = hiMant >> 16;
    bytes[4] = hiMant >> 8;
    bytes[5] = hiMant;
    bytes[6] = loMant >> 24;
    bytes[7] = loMant >> 16;
    bytes[8] = loMant >> 8;
    bytes[9] = loMant;
}

/*
 * C O N V E R T   F R O M   I E E E   E X T E N D E D
 */

#define UnsignedToFloat(u)    \
     (((double)((AuInt32)(u - 2147483647L - 1))) + 2147483648.0)

/****************************************************************
 * Extended precision IEEE floating-point conversion routine.
 ****************************************************************/

static double
ConvertFromIeeeExtended(bytes)
char           *bytes;
{
    double          f;
    AuInt32            expon;
    AuUint32   hiMant,
                    loMant;

    expon = ((bytes[0] & 0x7F) << 8) | (bytes[1] & 0xFF);
    hiMant = ((AuUint32) (bytes[2] & 0xFF) << 24)
	| ((AuUint32) (bytes[3] & 0xFF) << 16)
	| ((AuUint32) (bytes[4] & 0xFF) << 8)
	| ((AuUint32) (bytes[5] & 0xFF));
    loMant = ((AuUint32) (bytes[6] & 0xFF) << 24)
	| ((AuUint32) (bytes[7] & 0xFF) << 16)
	| ((AuUint32) (bytes[8] & 0xFF) << 8)
	| ((AuUint32) (bytes[9] & 0xFF));

    if (expon == 0 && hiMant == 0 && loMant == 0)
    {
	f = 0;
    }
    else
    {
	if (expon == 0x7FFF)
	{			/* Infinity or NaN */
	    f = HUGE_VAL;
	}
	else
	{
	    expon -= 16383;
	    f = ldexp(UnsignedToFloat(hiMant), expon -= 31);
	    f += ldexp(UnsignedToFloat(loMant), expon -= 32);
	}
    }

    if (bytes[0] & 0x80)
	return -f;
    else
	return f;
}

static int
readChunk(c, fp)
AiffChunk      *c;
FILE           *fp;
{
    int             status;
    char            n;

    if ((status = fread(c, sizeof(AiffChunk), 1, fp)))
	if (LITTLE_ENDIAN)
	    swapl(&c->ckSize, n);

    return status;
}

AiffInfo       *
AiffOpenFileForReading(name)
_AiffConst char *name;
{
    AiffInfo       *ai;
    AiffChunk       ck;
    AIFF_ID         id;
    AuInt32            fileSize;

    if (!(ai = (AiffInfo *) malloc(sizeof(AiffInfo))))
	return NULL;

    ai->comment = NULL;
    ai->dataOffset = ai->writing = 0;

    if (!(ai->fp = fopen(name, "r")))
	Err();

    if (!readChunk(&ck, ai->fp) ||
	cmpID(&ck.ckID, AIFF_FormID) ||
	!readID(&id) ||
	cmpID(&id, AIFF_AiffID))
	Err();

    fileSize = PAD2(ck.ckSize) - sizeof(AIFF_ID);

    while (fileSize > sizeof(AiffChunk))
    {
	if (!readChunk(&ck, ai->fp))
	    Err();

	fileSize -= sizeof(AiffChunk) + PAD2(ck.ckSize);

	/* common chunk */
	if (!cmpID(&ck.ckID, AIFF_CommonID))
	{
	    char            rate[AIFF_SizeofExtended];

	    ai->channels = FileReadS(ai->fp, LITTLE_ENDIAN);
	    ai->numSamples = FileReadL(ai->fp, LITTLE_ENDIAN);
	    ai->bitsPerSample = FileReadS(ai->fp, LITTLE_ENDIAN);

	    if (!fread(rate, AIFF_SizeofExtended, 1, ai->fp))
		Err();

	    ai->sampleRate = ConvertFromIeeeExtended(rate);
	}
	/* sound data chunk */
	else if (!cmpID(&ck.ckID, AIFF_SoundDataID))
	{
	    AuInt32            blockSize,
	                    offset;

	    offset = FileReadL(ai->fp, LITTLE_ENDIAN);
	    blockSize = FileReadL(ai->fp, LITTLE_ENDIAN);

	    if (offset)
		fseek(ai->fp, offset, 1);

	    ai->dataOffset = ftell(ai->fp);
	    ai->dataSize = ck.ckSize;

	    /* seek past the data */
	    fseek(ai->fp, PAD2(ck.ckSize) - 8 - offset, 1);
	}
	/* sound data chunk */
	else if (!cmpID(&ck.ckID, AIFF_CommentID))
	{
	    unsigned short  numComments;

	    numComments = FileReadS(ai->fp, LITTLE_ENDIAN);

	    if (numComments)
	    {
		AuUint32   timeStamp;
		AIFF_MARKER_ID  marker;
		unsigned short  count;

		timeStamp = FileReadL(ai->fp, LITTLE_ENDIAN);
		marker = FileReadS(ai->fp, LITTLE_ENDIAN);
		count = FileReadS(ai->fp, LITTLE_ENDIAN);

		if (count)
		{
		    if (!(ai->comment = (char *) malloc(count)) ||
			!fread(ai->comment, count, 1, ai->fp))
			Err();

		    if (count & 1)
			fgetc(ai->fp);	/* eat the pad byte */
		}

		ck.ckSize -= AIFF_SizeofCommentChunk + PAD2(count);
	    }
	    else
		ck.ckSize -= 2;

	    if (ck.ckSize)
		fseek(ai->fp, ck.ckSize, 1);	/* skip the other comments */
	}
	else
	    /* skip unknown chunk */
	    fseek(ai->fp, PAD2(ck.ckSize), 1);
    }

    if (!ai->dataOffset)
	Err();

    ai->numSamples = ai->dataSize / ai->channels / (ai->bitsPerSample >> 3);

    if (!ai->comment)
	ai->comment = FileCommentFromFilename(name);

    AiffRewindFile(ai);
    return ai;
}

AiffInfo       *
AiffOpenFileForWriting(name, ai)
_AiffConst char *name;
AiffInfo       *ai;
{
    int             n;
    char            rate[AIFF_SizeofExtended];

    ai->writing = ai->dataSize = 0;

    if (!(ai->fp = fopen(name, "w")) ||
	!fwrite(AIFF_FormID, sizeof(AIFF_ID), 1, ai->fp))
	Err();

    ai->sizeOffset = ftell(ai->fp);

    if (!FileWriteL(0, ai->fp, LITTLE_ENDIAN) ||
	!fwrite(AIFF_AiffID, sizeof(AIFF_ID), 1, ai->fp))
	Err();

    ai->fileSize = sizeof(AIFF_ID);

    if ((n = strlen(ai->comment)))
    {
	int             size;

	n++;
	size = AIFF_SizeofCommentChunk + PAD2(n);

	if (!fwrite(AIFF_CommentID, sizeof(AIFF_ID), 1, ai->fp) ||
	    !FileWriteL(size, ai->fp, LITTLE_ENDIAN) ||
	/* one comment */
	    !FileWriteS(1, ai->fp, LITTLE_ENDIAN) ||
	/* XXX: maybe we should use the real time */
	    !FileWriteL(0, ai->fp, LITTLE_ENDIAN) ||
	/* no marker */
	    !FileWriteS(0, ai->fp, LITTLE_ENDIAN) ||
	/* comment length */
	    !FileWriteS(n, ai->fp, LITTLE_ENDIAN) ||
	    !fwrite(ai->comment, n, 1, ai->fp))
	    Err();

	if (n & 1)
	    fputc(0, ai->fp);	/* pad the comment */

	ai->fileSize += sizeof(AiffChunk) + size;
    }

    ConvertToIeeeExtended((double) ai->sampleRate, rate);

    if (!fwrite(AIFF_CommonID, sizeof(AIFF_ID), 1, ai->fp) ||
	!FileWriteL(AIFF_SizeofCommonChunk, ai->fp, LITTLE_ENDIAN) ||
	!FileWriteS(ai->channels, ai->fp, LITTLE_ENDIAN) ||
	!FileWriteL(ai->numSamples, ai->fp, LITTLE_ENDIAN) ||
	!FileWriteS(ai->bitsPerSample, ai->fp, LITTLE_ENDIAN) ||
	!fwrite(rate, AIFF_SizeofExtended, 1, ai->fp))
	Err();

    ai->fileSize += sizeof(AiffChunk) + AIFF_SizeofCommonChunk;

    if (!fwrite(AIFF_SoundDataID, sizeof(AIFF_ID), 1, ai->fp))
	Err();

    ai->dataOffset = ftell(ai->fp);

    if (!FileWriteL(0, ai->fp, LITTLE_ENDIAN) ||
    /* offset */
	!FileWriteL(0, ai->fp, LITTLE_ENDIAN) ||
    /* block size */
	!FileWriteL(0, ai->fp, LITTLE_ENDIAN))
	Err();

    ai->fileSize += sizeof(AiffChunk) + AIFF_SizeofSoundDataChunk;

    ai->writing = 1;
    return ai;
}

int
AiffCloseFile(ai)
AiffInfo       *ai;
{
    int             status = 0;

    if (ai->fp)
    {
	if (ai->writing)
	{
	    if (ai->dataSize & 1)
		fputc(0, ai->fp);	/* pad the data */

	    fseek(ai->fp, ai->sizeOffset, 0);
	    FileWriteL(ai->fileSize + PAD2(ai->dataSize), ai->fp,
		       LITTLE_ENDIAN);
	    fseek(ai->fp, ai->dataOffset, 0);
	    FileWriteL(ai->dataSize, ai->fp, LITTLE_ENDIAN);
	}

	status = fclose(ai->fp);
    }

    if (ai->comment)
	free(ai->comment);

    free(ai);
    return status;
}

int
AiffReadFile(p, n, ai)
char           *p;
int             n;
AiffInfo       *ai;
{
    return fread(p, 1, n, ai->fp);
}

int
AiffWriteFile(p, n, ai)
char           *p;
int             n;
AiffInfo       *ai;
{
    ai->dataSize += n;
    return fwrite(p, 1, n, ai->fp);
}

int
AiffRewindFile(ai)
AiffInfo       *ai;
{
    return fseek(ai->fp, ai->dataOffset, 0);
}
