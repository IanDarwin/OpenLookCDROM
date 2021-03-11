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
 * $NCDId: @(#)voc.c,v 1.11 1994/04/07 20:32:20 greg Exp $
 */

#include	<stdio.h>
#include 	<malloc.h>
#include	<audio/Aos.h>
#include	<audio/voc.h>
#include	<audio/fileutil.h>

#define Err() { VocCloseFile(vi); return NULL; }

#define ReadSize(_n)							      \
{									      \
    _n = fgetc(vi->fp);							      \
    _n += fgetc(vi->fp) << 8;						      \
    _n += fgetc(vi->fp) << 16;						      \
}

#define WriteSize(_n)							      \
{									      \
    fputc(_n, vi->fp);							      \
    fputc((_n) >> 8, vi->fp);						      \
    fputc((_n) >> 16, vi->fp);						      \
}

VocInfo        *
VocOpenFileForReading(name)
_VocConst char *name;
{
    VocInfo        *vi;
    int             c,
                    extended = AuFalse;
    AuUint32        n;
    char           buf[VOC_ID_SIZE];

    if (!(vi = (VocInfo *) malloc(sizeof(VocInfo))))
	return NULL;

    vi->comment = NULL;
    vi->dataOffset = vi->writing = 0;
    vi->tracks = 1;

    if (!(vi->fp = fopen(name, "r")))
	Err();

    if (!fread(buf, VOC_ID_SIZE, 1, vi->fp) ||
	strncmp(buf, VOC_ID, VOC_ID_SIZE) ||
	FileReadS(vi->fp, BIG_ENDIAN) != VOC_DATA_OFFSET ||
	FileReadS(vi->fp, BIG_ENDIAN) != VOC_VERSION ||
	FileReadS(vi->fp, BIG_ENDIAN) != VOC_VERSION_CHK)
	Err();

    do
	switch (c = fgetc(vi->fp))
	{
	  case VOC_TERMINATOR:
	    break;
	  case VOC_DATA:
	    ReadSize(vi->dataSize);
	    vi->dataSize -= 2;

	    if (!extended)
	    {
		vi->sampleRate = 1000000 / (256 - fgetc(vi->fp));
		vi->compression = fgetc(vi->fp);
	    }
	    else
		fseek(vi->fp, 2, 1);

	    vi->dataOffset = ftell(vi->fp);
	    fseek(vi->fp, vi->dataSize, 1);
	    break;
	  case VOC_TEXT:
	    ReadSize(n);
	    if (!(vi->comment = (char *) malloc(n)) ||
		!fread(vi->comment, n, 1, vi->fp))
		Err();
	    break;
	  case VOC_EXTENDED:
	    ReadSize(n);

	    if (n == 4)
	    {
		extended = AuTrue;
		n = fgetc(vi->fp);
		n += fgetc(vi->fp) << 8;
		vi->sampleRate = 256000000 / (65536 - n);
		vi->compression = fgetc(vi->fp);
		n = fgetc(vi->fp);

		if (n == 0 || n == 1)
		    vi->tracks = n + 1;
		else
		    Err();		       /* an unknown mode */

		vi->sampleRate /= vi->tracks;
	    }
	    else
		Err();			       /* ??? */
	    break;
	  case VOC_CONTINUE:
	  case VOC_SILENCE:
	  case VOC_MARKER:
	  case VOC_REPEAT:
	  case VOC_REPEAT_END:
	    ReadSize(n);
	    fseek(vi->fp, n, 1);
	    break;
	  default:
	    Err();
	}
    while (c != VOC_TERMINATOR);

    if (!vi->dataOffset)
	Err();

    if (!vi->comment)
	vi->comment = FileCommentFromFilename(name);

    VocRewindFile(vi);
    return vi;
}

VocInfo        *
VocOpenFileForWriting(name, vi)
_VocConst char *name;
VocInfo        *vi;
{
    int             n;

    vi->writing = vi->dataSize = 0;

    if (!(vi->fp = fopen(name, "w")) ||
	!fwrite(VOC_ID, VOC_ID_SIZE, 1, vi->fp) ||
	!FileWriteS(VOC_DATA_OFFSET, vi->fp, BIG_ENDIAN) ||
	!FileWriteS(VOC_VERSION, vi->fp, BIG_ENDIAN) ||
	!FileWriteS(VOC_VERSION_CHK, vi->fp, BIG_ENDIAN))
	Err();

    if ((n = strlen(vi->comment)))
    {
	fputc(VOC_TEXT, vi->fp);
	n++;
	WriteSize(n);

	if (!fwrite(vi->comment, n, 1, vi->fp))
	    Err();
    }

    if (vi->tracks == 2)
    {
	fputc(VOC_EXTENDED, vi->fp);
	WriteSize(4);
	n = 65536 - (256000000 / (vi->sampleRate * 2));
	fputc(n, vi->fp);
	fputc(n >> 8, vi->fp);
	fputc(0, vi->fp);
	fputc(1, vi->fp);
    }

    fputc(VOC_DATA, vi->fp);
    vi->dataOffset = ftell(vi->fp);
    WriteSize(0);
    fputc(256 - (1000000 / vi->sampleRate), vi->fp);
    fputc(0, vi->fp);

    vi->writing = 1;
    return vi;
}

int
VocCloseFile(vi)
VocInfo        *vi;
{
    int             status = 0;

    if (vi->fp)
    {
	if (vi->writing && vi->dataOffset)
	{
	    fputc(VOC_TERMINATOR, vi->fp);
	    fseek(vi->fp, vi->dataOffset, 0);
	    vi->dataSize += 2;
	    WriteSize(vi->dataSize);
	}

	status = fclose(vi->fp);
    }

    if (vi->comment)
	free(vi->comment);

    free(vi);
    return status;
}

int
VocReadFile(p, n, vi)
char           *p;
int             n;
VocInfo        *vi;
{
    return fread(p, 1, n, vi->fp);
}

int
VocWriteFile(p, n, vi)
char           *p;
int             n;
VocInfo        *vi;
{
    vi->dataSize += n;
    return fwrite(p, 1, n, vi->fp);
}

int
VocRewindFile(vi)
VocInfo        *vi;
{
    return fseek(vi->fp, vi->dataOffset, 0);
}
