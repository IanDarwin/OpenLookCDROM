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
 * $NCDId: @(#)8svx.c,v 1.2 1994/04/07 20:27:52 greg Exp $
 */

#include <stdio.h>
#include <malloc.h>
#include <audio/Aos.h>
#include <math.h>
#include <audio/8svx.h>
#include <audio/fileutil.h>

#define Err()		{ SvxCloseFile(si); return NULL; }
#define readID(_f)	fread(_f, sizeof(SVX_ID), 1, si->fp)
#define readByte(_f)	fread(_f, 1, 1, si->fp)
#define cmpID(_x, _y)	strncmp((char *) (_x), (char *) (_y), sizeof(SVX_ID))

static int
readChunk(c, fp)
SvxChunk       *c;
FILE           *fp;
{
    int             status;
    char            n;

    if ((status = fread(c, sizeof(SvxChunk), 1, fp)))
	if (LITTLE_ENDIAN)
	    swapl(&c->ckSize, n);

    return status;
}

SvxInfo        *
SvxOpenFileForReading(name)
_SvxConst char *name;
{
    SvxInfo        *si;
    SvxChunk        ck;
    SVX_ID          id;
    AuInt32         fileSize;

    if (!(si = (SvxInfo *) malloc(sizeof(SvxInfo))))
	return NULL;

    si->comment = NULL;
    si->dataOffset = si->writing = 0;

    if (!(si->fp = fopen(name, "r")))
	Err();

    if (!readChunk(&ck, si->fp) ||
	cmpID(&ck.ckID, SVX_FormID) ||
	!readID(&id) ||
	cmpID(&id, SVX_8svxID))
	Err();

    fileSize = ck.ckSize - sizeof(SVX_ID);

    while (fileSize > sizeof(SvxChunk))
    {
	if (!readChunk(&ck, si->fp))
	    Err();

	fileSize -= sizeof(SvxChunk) + ck.ckSize;

	/* Voice8Header chunk */
	if (!cmpID(&ck.ckID, SVX_VhdrID))
	{
	    AuUint8         c;

	    si->numSamples = FileReadL(si->fp, LITTLE_ENDIAN);
	    FileReadL(si->fp, LITTLE_ENDIAN);
	    FileReadL(si->fp, LITTLE_ENDIAN);
	    si->sampleRate = FileReadS(si->fp, LITTLE_ENDIAN);
	    readByte(&c);
	    readByte(&c);

	    if (c)
		Err();

	    FileReadL(si->fp, LITTLE_ENDIAN);
	}
	/* sound data chunk */
	else if (!cmpID(&ck.ckID, SVX_BodyID))
	{
	    si->dataOffset = ftell(si->fp);
	    si->dataSize = ck.ckSize;

	    /* seek past the data */
	    fseek(si->fp, ck.ckSize, 1);
	}
	/* NAME chunk */
	else if (!cmpID(&ck.ckID, SVX_NameID))
	{
	    if (!(si->comment = (char *) malloc(ck.ckSize + 1)) ||
		!fread(si->comment, ck.ckSize, 1, si->fp))
		Err();

	    si->comment[ck.ckSize] = 0;
	}
	else
	    /* skip unknown chunk */
	    fseek(si->fp, ck.ckSize, 1);
    }

    if (!si->dataOffset)
	Err();

    si->numSamples = si->dataSize;

    if (!si->comment)
	si->comment = FileCommentFromFilename(name);

    SvxRewindFile(si);
    return si;
}

SvxInfo        *
SvxOpenFileForWriting(name, si)
_SvxConst char *name;
SvxInfo        *si;
{
    int             n;

    si->writing = si->dataSize = 0;

    if (!(si->fp = fopen(name, "w")) ||
	!fwrite(SVX_FormID, sizeof(SVX_ID), 1, si->fp))
	Err();

    si->sizeOffset = ftell(si->fp);

    if (!FileWriteL(0, si->fp, LITTLE_ENDIAN) ||
	!fwrite(SVX_8svxID, sizeof(SVX_ID), 1, si->fp))
	Err();

    si->fileSize = sizeof(SVX_ID);

    if ((n = strlen(si->comment)))
    {
	if (!fwrite(SVX_NameID, sizeof(SVX_ID), 1, si->fp) ||
	    !FileWriteL(n, si->fp, LITTLE_ENDIAN) ||
	    !fwrite(si->comment, n, 1, si->fp))
	    Err();

	si->fileSize += sizeof(SvxChunk) + n;
    }

    if (!fwrite(SVX_VhdrID, sizeof(SVX_ID), 1, si->fp) ||
	!FileWriteL(SVX_SizeofVhdrChunk, si->fp, LITTLE_ENDIAN) ||
	!FileWriteL(si->numSamples, si->fp, LITTLE_ENDIAN) ||
	!FileWriteL(0, si->fp, LITTLE_ENDIAN) ||
	!FileWriteL(0, si->fp, LITTLE_ENDIAN) ||
	!FileWriteS(si->sampleRate, si->fp, LITTLE_ENDIAN) ||
	!FileWriteS(0, si->fp, LITTLE_ENDIAN) ||
	!FileWriteL(SVX_MaxVolume, si->fp, LITTLE_ENDIAN))
	Err();

    si->fileSize += sizeof(SvxChunk) + SVX_SizeofVhdrChunk;

    if (!fwrite(SVX_BodyID, sizeof(SVX_ID), 1, si->fp))
	Err();

    si->dataOffset = ftell(si->fp);

    if (!FileWriteL(0, si->fp, LITTLE_ENDIAN))
	Err();

    si->fileSize += sizeof(SvxChunk);

    si->writing = 1;
    return si;
}

int
SvxCloseFile(si)
SvxInfo        *si;
{
    int             status = 0;

    if (si->fp)
    {
	if (si->writing)
	{
	    fseek(si->fp, si->sizeOffset, 0);
	    FileWriteL(si->fileSize + si->dataSize, si->fp, LITTLE_ENDIAN);
	    fseek(si->fp, si->dataOffset, 0);
	    FileWriteL(si->dataSize, si->fp, LITTLE_ENDIAN);
	}

	status = fclose(si->fp);
    }

    if (si->comment)
	free(si->comment);

    free(si);
    return status;
}

int
SvxReadFile(p, n, si)
char           *p;
int             n;
SvxInfo        *si;
{
    return fread(p, 1, n, si->fp);
}

int
SvxWriteFile(p, n, si)
char           *p;
int             n;
SvxInfo        *si;
{
    si->dataSize += n;
    return fwrite(p, 1, n, si->fp);
}

int
SvxRewindFile(si)
SvxInfo        *si;
{
    return fseek(si->fp, si->dataOffset, 0);
}
