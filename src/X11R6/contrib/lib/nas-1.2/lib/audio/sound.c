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
 * $NCDId: @(#)sound.c,v 1.23 1994/05/26 16:23:55 greg Exp $
 */

/*
 * generic sound handling routines
 */

#define _SOUND_C_

#include	<stdio.h>
#include 	<malloc.h>
#include	<audio/Aos.h>
#include	<audio/audio.h>
#include	<audio/sound.h>

static int
SndToSoundFormat(fmt)
int             fmt;
{
    switch (fmt)
    {
      case SND_FORMAT_ULAW_8:
	return AuFormatULAW8;
      case SND_FORMAT_LINEAR_8:
	return AuFormatLinearUnsigned8;
      case SND_FORMAT_LINEAR_16:
	return AuFormatLinearSigned16MSB;
      default:
	return AuNone;
    }
}

static int
SoundToSndFormat(fmt)
int             fmt;
{
    switch (fmt)
    {
      case AuFormatULAW8:
	return SND_FORMAT_ULAW_8;
      case AuFormatLinearUnsigned8:
	return SND_FORMAT_LINEAR_8;
      case AuFormatLinearSigned16MSB:
	return SND_FORMAT_LINEAR_16;
      default:
	return SND_FORMAT_UNSPECIFIED;
    }
}

static int
sndToSound(s)
Sound           s;
{
    SndInfo        *p = (SndInfo *)s->formatInfo;
    SndHeader      *h = &p->h;

    SoundFileFormat(s) = SoundFileFormatSnd;
    SoundDataFormat(s) = SndToSoundFormat(h->format);

    if (SoundDataFormat(s) == AuNone)
	return 0;

    SoundSampleRate(s) = h->sampleRate;
    SoundNumTracks(s) = h->tracks;
    SoundComment(s) = p->comment;
    SoundNumSamples(s) = h->dataSize == SND_DATA_SIZE_UNKNOWN ?
	SoundUnknownNumSamples :
	h->dataSize / SoundNumTracks(s) / SoundBytesPerSample(s);

    return 1;
}

static int
soundToSnd(s)
Sound           s;
{
    SndInfo        *si;

    if (!(si = (SndInfo *) malloc(sizeof(SndInfo))))
	return 0;

    si->comment = SoundComment(s);
    si->h.format = SoundToSndFormat(SoundDataFormat(s));
    si->h.dataSize =
	SoundNumSamples(s) == SoundUnknownNumSamples ? SND_DATA_SIZE_UNKNOWN :
	SoundNumSamples(s);
    si->h.sampleRate = SoundSampleRate(s);
    si->h.tracks = SoundNumTracks(s);

    s->formatInfo = (void *) si;
    return 1;
}

static int
vocToSound(s)
Sound           s;
{
    VocInfo        *p = (VocInfo *)s->formatInfo;

    SoundFileFormat(s) = SoundFileFormatVoc;
    SoundDataFormat(s) = AuFormatLinearUnsigned8;
    SoundSampleRate(s) = p->sampleRate;
    SoundNumTracks(s) = p->tracks;
    SoundComment(s) = p->comment;
    SoundNumSamples(s) = p->dataSize / SoundNumTracks(s) /
	SoundBytesPerSample(s);
    return 1;
}

static int
soundToVoc(s)
Sound           s;
{
    VocInfo        *vi;

    if (!(vi = (VocInfo *) malloc(sizeof(VocInfo))))
	return 0;

    vi->comment = SoundComment(s);
    vi->sampleRate = SoundSampleRate(s);
    vi->tracks = SoundNumTracks(s);

    s->formatInfo = (void *) vi;
    return 1;
}

static int
WaveToSoundFormat(wi)
WaveInfo       *wi;
{
    if (wi->bitsPerSample == 8)
	return AuFormatLinearUnsigned8;

    if (wi->bitsPerSample == 16)
	return AuFormatLinearSigned16LSB;

    return -1;
}

static int
waveToSound(s)
Sound           s;
{
    WaveInfo       *wi = (WaveInfo *)s->formatInfo;

    SoundFileFormat(s) = SoundFileFormatWave;
    SoundDataFormat(s) = WaveToSoundFormat(wi);
    SoundSampleRate(s) = wi->sampleRate;
    SoundNumTracks(s) = wi->channels;
    SoundComment(s) = wi->comment;
    SoundNumSamples(s) = wi->numSamples;

    return 1;
}

static int
soundToWave(s)
Sound           s;
{
    WaveInfo       *wi;

    if (!(wi = (WaveInfo *) malloc(sizeof(WaveInfo))))
	return 0;

    wi->comment = SoundComment(s);
    wi->sampleRate = SoundSampleRate(s);
    wi->channels = SoundNumTracks(s);
    wi->bitsPerSample = AuSizeofFormat(SoundDataFormat(s)) << 3;

    s->formatInfo = (void *) wi;
    return 1;
}

static int
AiffToSoundFormat(ai)
AiffInfo       *ai;
{
    if (ai->bitsPerSample == 8)
	return AuFormatLinearSigned8;

    if (ai->bitsPerSample == 16)
	return AuFormatLinearSigned16MSB;

    return -1;
}

static int
aiffToSound(s)
Sound           s;
{
    AiffInfo       *ai = (AiffInfo *)s->formatInfo;

    SoundFileFormat(s) = SoundFileFormatAiff;
    SoundDataFormat(s) = AiffToSoundFormat(ai);
    SoundSampleRate(s) = ai->sampleRate;
    SoundNumTracks(s) = ai->channels;
    SoundComment(s) = ai->comment;
    SoundNumSamples(s) = ai->numSamples;

    return 1;
}

static int
soundToAiff(s)
Sound           s;
{
    AiffInfo       *ai;

    if (!(ai = (AiffInfo *) malloc(sizeof(AiffInfo))))
	return 0;

    ai->comment = SoundComment(s);
    ai->sampleRate = SoundSampleRate(s);
    ai->channels = SoundNumTracks(s);
    ai->bitsPerSample = AuSizeofFormat(SoundDataFormat(s)) << 3;

    s->formatInfo = (void *) ai;
    return 1;
}

static int
svxToSound(s)
Sound           s;
{
    SvxInfo        *si = (SvxInfo *)s->formatInfo;

    SoundFileFormat(s) = SoundFileFormatSvx;
    SoundDataFormat(s) = AuFormatLinearSigned8;
    SoundSampleRate(s) = si->sampleRate;
    SoundNumTracks(s) = 1;
    SoundComment(s) = si->comment;
    SoundNumSamples(s) = si->numSamples;

    return 1;
}

static int
soundToSvx(s)
Sound           s;
{
    SvxInfo        *si;

    if (!(si = (SvxInfo *) malloc(sizeof(SvxInfo))))
	return 0;

    si->comment = SoundComment(s);
    si->sampleRate = SoundSampleRate(s);

    s->formatInfo = (void *) si;
    return 1;
}

Sound
SoundOpenFileForReading(name)
_SoundConst char *name;
{
    Sound           s;
    int             i;

    if (!(s = (Sound) malloc(sizeof(SoundRec))))
	return NULL;

    SoundComment(s) = NULL;

    for (i = 0; i < SoundNumFileFormats; i++)
	if ((s->formatInfo = (SoundFileInfo[i].openFileForReading) (name)))
	{
	    if (!(SoundFileInfo[i].toSound) (s))
	    {
		SoundCloseFile(s);
		return NULL;
	    }
	    break;
	}

    if (i == SoundNumFileFormats)
    {
	SoundCloseFile(s);
	return NULL;
    }

    return s;
}

Sound
SoundOpenFileForWriting(name, s)
_SoundConst char *name;
Sound           s;
{
    if (SoundFileFormat(s) != SoundFileFormatNone &&
	(SoundFileInfo[SoundFileFormat(s)].openFileForWriting) (name,
							     s->formatInfo))
	return s;

    return NULL;
}

int
SoundReadFile(p, n, s)
char           *p;
int             n;
Sound           s;
{
    return (SoundFileInfo[SoundFileFormat(s)].readFile) (p, n, s->formatInfo);
}

int
SoundWriteFile(p, n, s)
char           *p;
int             n;
Sound           s;
{
    return (SoundFileInfo[SoundFileFormat(s)].writeFile) (p, n, s->formatInfo);
}

int
SoundRewindFile(s)
Sound           s;
{
    return (SoundFileInfo[SoundFileFormat(s)].rewindFile) (s->formatInfo);
}

int
SoundCloseFile(s)
Sound           s;
{
    int             status = 0;

    if (s->formatInfo)
	status = (SoundFileInfo[SoundFileFormat(s)].closeFile) (s->formatInfo);
    else if (SoundComment(s))
	free(SoundComment(s));

    free(s);
    return status;
}

Sound
SoundCreate(fileFormat, dataFormat, numTracks, sampleRate, numSamples, comment)
int             fileFormat,
                dataFormat,
                numTracks,
                sampleRate,
                numSamples;
_SoundConst char *comment;
{
    Sound           s;

    if (!(s = (Sound) malloc(sizeof(SoundRec))))
	return NULL;

    SoundFileFormat(s) = fileFormat;
    SoundDataFormat(s) = dataFormat;
    SoundNumTracks(s) = numTracks;
    SoundSampleRate(s) = sampleRate;
    SoundNumSamples(s) = numSamples;

    if (comment)
    {
	char           *p;

	if ((p = (char *) malloc(strlen(comment) + 1)))
	{
	    strcpy(p, comment);
	    SoundComment(s) = p;
	}
	else
	{
	    free(s);
	    return NULL;
	}
    }
    else
    {
	char           *p;

	if ((p = (char *) malloc(1)))
	{
	    *p = 0;
	    SoundComment(s) = p;
	}
	else
	{
	    free(s);
	    return NULL;
	}
    }

    s->formatInfo = NULL;

    if (SoundFileFormat(s) != SoundFileFormatNone)
	if (!SoundValidateDataFormat(s) ||
	    !(SoundFileInfo[SoundFileFormat(s)].fromSound) (s))
	{
	    free(SoundComment(s));
	    free(s);
	    return NULL;
	}

    return s;
}

int
SoundStringToFileFormat(s)
_SoundConst char *s;
{
    int             i;

    for (i = 0; i < SoundNumFileFormats; i++)
	if (!strcasecmp(s, SoundFileInfo[i].string))
	    break;

    return i == SoundNumFileFormats ? -1 : i;
}

int
SoundAbbrevToFileFormat(s)
_SoundConst char *s;
{
    int             i;

    for (i = 0; i < SoundNumFileFormats; i++)
	if (!strcasecmp(s, SoundFileInfo[i].abbrev))
	    break;

    return i == SoundNumFileFormats ? -1 : i;
}
