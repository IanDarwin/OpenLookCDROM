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
 * $NCDId: @(#)snd.c,v 1.19 1994/04/07 20:30:28 greg Exp $
 */

#include <stdio.h>
#include <malloc.h>
#include <audio/Aos.h>
#include <audio/snd.h>
#include <audio/fileutil.h>

#define Err() { SndCloseFile(si); return NULL; }

int
SndReadFile(p, n, si)
char           *p;
int             n;
SndInfo        *si;
{
    return fread(p, 1, n, si->fp);
}

int
SndWriteFile(p, n, si)
char           *p;
int             n;
SndInfo        *si;
{
    si->h.dataSize += n;
    return fwrite(p, 1, n, si->fp);
}

int
SndCloseFile(si)
SndInfo        *si;
{
    int             status = 0;

    if (si->fp && si->fp != stdin && si->fp != stdout)
    {
	if (si->writing)
	{
	    if (LITTLE_ENDIAN)
	    {
		char            n;

		swapl(&si->h.dataSize, n);
	    }

	    fseek(si->fp, 8, 0);
	    fwrite(&si->h.dataSize, sizeof(si->h.dataSize), 1, si->fp);
	}

	status = fclose(si->fp);
    }

    if (si->comment)
	free(si->comment);

    free(si);

    return status;
}

int
SndRewindFile(si)
SndInfo        *si;
{
    return (si->fp == stdin ? -1 : fseek(si->fp, si->h.dataOffset, 0));
}

SndInfo        *
SndOpenFileForReading(name)
_SndConst char *name;
{
    SndInfo        *si;
    int             size;

    if (!(si = (SndInfo *) malloc(sizeof(SndInfo))))
	return NULL;

    si->comment = NULL;
    si->writing = 0;

    si->fp = !strcmp("-", name) ? stdin : fopen(name, "r");

    if (!si->fp ||
	fread(&si->h, 1, sizeof(SndHeader), si->fp) != sizeof(SndHeader))
	Err();

    /* do byte swapping if necessary */
    if (LITTLE_ENDIAN)
    {
	char            n;

	swapl(&si->h.magic, n);
	swapl(&si->h.dataOffset, n);
	swapl(&si->h.dataSize, n);
	swapl(&si->h.format, n);
	swapl(&si->h.sampleRate, n);
	swapl(&si->h.tracks, n);
    }

    if (si->h.magic != SND_MAGIC_NUM)
	Err();

    size = si->h.dataOffset - sizeof(SndHeader);

    if (size)
    {
	if (!(si->comment = (char *) malloc(size + 1)))
	    Err();

	if (fread(si->comment, 1, size, si->fp) != size)
	    Err();

	*(si->comment + size) = 0;
    }
    else
	si->comment = FileCommentFromFilename(name);


    if (si->fp != stdin)
    {
	AuUint32 size;

	fseek(si->fp, 0, 2);
	size = ftell(si->fp) - si->h.dataOffset;
	SndRewindFile(si);

	if (si->h.dataSize == SND_DATA_SIZE_UNKNOWN || size < si->h.dataSize)
	    si->h.dataSize = size;
    }

    return si;
}

SndInfo        *
SndOpenFileForWriting(name, si)
_SndConst char *name;
SndInfo        *si;
{
    int             commentLen;
    SndHeader       h;

    si->writing = 0;
    commentLen = si->comment ? strlen(si->comment) : 0;
    si->h.magic = SND_MAGIC_NUM;
    si->h.dataOffset = sizeof(SndHeader) + commentLen;

    si->fp = !strcmp("-", name) ? stdout : fopen(name, "w");

    if (si->fp)
    {
	h = si->h;

	if (LITTLE_ENDIAN)
	{			/* header is big-endian */
	    char            n;

	    swapl(&h.magic, n);
	    swapl(&h.dataOffset, n);
	    swapl(&h.dataSize, n);
	    swapl(&h.format, n);
	    swapl(&h.sampleRate, n);
	    swapl(&h.tracks, n);
	}

	if (fwrite(&h, 1, sizeof(SndHeader), si->fp) == sizeof(SndHeader))
	    if (!commentLen ||
		fwrite(si->comment, 1, commentLen, si->fp) == commentLen)
	    {
		si->writing = 1;
		si->h.dataSize = 0;
		return si;
	    }

	Err();
    }

    return NULL;
}
