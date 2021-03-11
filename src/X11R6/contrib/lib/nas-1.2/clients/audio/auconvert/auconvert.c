/**
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:	Greg Renda <greg@ncd.com>
 * 		Network Computing Devices, Inc.
 * 		350 North Bernardo Ave.
 * 		Mountain View, CA  94043
 *
 * $NCDId: @(#)auconvert.c,v 1.8 1994/04/07 18:09:30 greg Exp $
 */

#include <stdio.h>
#include <signal.h>
#include <audio/Aos.h>		/* for string and other os stuff */
#include <audio/Afuncs.h>	/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/soundlib.h>

#define USAGE "\
usage: auconvert [options] [<input file>] [<output file>]\n\
where options include:\n\n\
    -file format                   file format\n\
    -data format                   data format\n\
    -rate <sample rate>            sampling rate\n\
    -comment string                comment\n\
    -raw <data format> <tracks>    input file is raw data\n\
    -volume percent|\"max\"          volume percent or \"max\" volume\n\
"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define abs(a) ((a) < 0 ? -(a) : a)

#define OPTION(_o) (!strncmp(arg, _o, strlen(_o)))
#define GETARG(_d, _s)							      \
do									      \
{									      \
    if (--argc)								      \
    {									      \
	arg = *(++argv);						      \
	_d = _s;							      \
    }									      \
    else								      \
	usage();							      \
} while(0)

static struct
{
    char           *string;
    int             value;
}
                dataFormats[] =
{
    "ulaw8", AuFormatULAW8,
    "linu8", AuFormatLinearUnsigned8,
    "lins8", AuFormatLinearSigned8,
    "lins16m", AuFormatLinearSigned16MSB,
    "linu16m", AuFormatLinearUnsigned16MSB,
    "lins16l", AuFormatLinearSigned16LSB,
    "linu16l", AuFormatLinearUnsigned16LSB,
};

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static void
usage()
{
    int             i;

    fprintf(stderr, USAGE);

    fprintf(stderr, "\nFile Formats:\n\n");

    for (i = 0; i < SoundNumFileFormats; i++)
	fprintf(stderr, "%10s - %s\n", SoundFileFormatToAbbrev(i),
		SoundFileFormatToString(i));

    fprintf(stderr, "\nData Formats:\n\n");

    for (i = 0; i < sizeof(dataFormats) / sizeof(dataFormats[0]); i++)
	fprintf(stderr, "%10s - %s\n", dataFormats[i].string,
		AuFormatToString(dataFormats[i].value));

    fprintf(stderr, "\nNote that each file format may only support selected data formats\n");

    exit(1);
}

static int
convertDataFormat(s)
char           *s;
{
    int             f,
                    i;

    for (i = 0; i < sizeof(dataFormats) / sizeof(dataFormats[0]); i++)
	if (!strcasecmp(s, dataFormats[i].string))
	    return dataFormats[i].value;

    usage();
}

static int
convertFileFormat(s)
char           *s;
{
    int             f;

    f = SoundAbbrevToFileFormat(s);

    if (f == -1)
	usage();

    return f;
}

static void
adjustVolume(count, data, v)
int             count;
short          *data;
float           v;
{
    while (count--)
	*data++ *= v;
}

static void
maxVolume(count, data)
int             count;
short          *data;
{
    int             n = count;
    short          *p = data,
                    m = 0;
    float           scale;

    while (n--)
    {
	m = max(m, abs(*p));
	*p++;
    }

    scale = 32767.0 / m;

    fprintf(stderr, "volume scaled by %f\n", scale);
    adjustVolume(count, data, scale);
}

static void
rateConvert(in, out, numBytes, data)
Sound           in,
                out;
int             numBytes;
char           *data;
{
    int             size = SoundBytesPerSample(out),
                    phase = 0;
    char           *last;

    if (!(last = (char *) malloc(size)))
	fatalError("Malloc error");

    while (numBytes)
    {
	while (phase >= 0)
	{
	    if (!numBytes)
	    {
		free(last);
		return;
	    }

	    phase -= SoundSampleRate(out);
	    bcopy(data, last, size);
	    data += size;
	    numBytes -= size;
	}

	phase += SoundSampleRate(in);

	if (SoundWriteFile(last, size, out) != size)
	    fatalError("Error writing output file");
    }

    free(last);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    FILE           *fp;
    char           *arg,
                   *inputFile = NULL,
                   *outputFile = NULL,
                   *comment = NULL,
                   *volume = "100",
                   *mktemp(),
                   *outName;
    int             fileFormat = -1,
                    dataFormat = -1,
                    vol,
                    rawFormat = -1,
                    rawTracks,
                    outputDataFormat;
    AuUint32        rate = 0,
                    numBytes;
    Sound           in,
                    out;
    short          *data;

    while (--argc)
    {
	arg = *(++argv);

	if (OPTION("-f"))
	    GETARG(fileFormat, convertFileFormat(arg));
	else if (OPTION("-d"))
	    GETARG(dataFormat, convertDataFormat(arg));
	else if (OPTION("-raw"))
	{
	    GETARG(rawFormat, convertDataFormat(arg));
	    GETARG(rawTracks, atoi(arg));
	}
	else if (OPTION("-rat"))
	    GETARG(rate, atoi(arg));
	else if (OPTION("-c"))
	    GETARG(comment, arg);
	else if (OPTION("-v"))
	    GETARG(volume, arg);
	else if (OPTION("-?") || OPTION("-h"))
	    usage();
	else if (inputFile)
	    if (outputFile)
		usage();
	    else
		outputFile = arg;
	else
	    inputFile = arg;
    }

    if (!inputFile)
	inputFile = "-";

    if (rawFormat == -1)
    {
	if (!(in = SoundOpenFileForReading(inputFile)))
	    fatalError("Can't open input file %s", inputFile);
    }
    else
    {
	if (fileFormat == -1)
	    fatalError("You must specify a file format for raw files");

	if (!rate)
	    fatalError("You must specify a sampling rate for raw files");

	if (!(fp = fopen(inputFile, "r")))
	    fatalError("Can't open input file %s", inputFile);

	fseek(fp, 0, 2);
	numBytes = ftell(fp);
	rewind(fp);

	in = SoundCreate(SoundFileFormatNone, rawFormat, rawTracks, rate,
			 numBytes / rawTracks / AuSizeofFormat(rawFormat),
			 NULL);
	
	if (!in)
	{
	    fprintf(stderr, "%s file format does not support %s data.\n",
		    SoundFileFormatToString(fileFormat),
		    AuFormatToString(rawFormat));
	    exit(1);
	}
    }

    numBytes = SoundNumSamples(in) * SoundNumTracks(in) * sizeof(short);

    if (!(data = (short *) malloc(numBytes)))
	fatalError("Malloc error");

    if (rawFormat == -1)
    {
	if (SoundReadFile((char *) data, SoundNumBytes(in), in) !=
	    SoundNumBytes(in))
	    fatalError("Error reading input file");
    }
    else if (fread(data, 1, SoundNumBytes(in), fp) != SoundNumBytes(in))
	fatalError("Error reading input file");

    if (AuConvertDataToShort(SoundDataFormat(in), SoundNumBytes(in), data))
	fatalError("Error converting input data");

    if (!strncasecmp(volume, "max", 3))
	maxVolume(numBytes / sizeof(short), data);
    else if ((vol = atoi(volume)) != 100)
	adjustVolume(numBytes / sizeof(short), data, (float) vol / 100);

    outputDataFormat = dataFormat == -1 ? SoundDataFormat(in) : dataFormat;

    if (fileFormat == -1)
	fileFormat = SoundFileFormat(in);

    if (!SoundValidDataFormat(fileFormat, outputDataFormat))
    {
	fprintf(stderr, "%s file format does not support %s data.\n",
		SoundFileFormatToString(fileFormat),
		AuFormatToString(outputDataFormat));
	exit(1);
    }

    out = SoundCreate(fileFormat, outputDataFormat, SoundNumTracks(in),
		      rate ? rate : SoundSampleRate(in),
		      SoundUnknownNumSamples,
		      comment ? comment : SoundComment(in));

    if (!out)
	fatalError("Error creating output sound");

    if (AuConvertShortToData(outputDataFormat, numBytes, data))
	fatalError("Error converting output data");

    if (!outputFile && strcmp(inputFile, "-"))
    {
	if (!(outName = (char *) malloc(strlen(inputFile) + 7)))
	    fatalError("Malloc error");

	sprintf(outName, "%sXXXXXX", inputFile);
	outName = mktemp(outName);
    }
    else
	outName = outputFile;

    if (!SoundOpenFileForWriting(outName, out))
	fatalError("Can't open output file %s", outName);

    numBytes = numBytes / sizeof(short) * SoundBytesPerSample(out);

    if (SoundSampleRate(in) == SoundSampleRate(out))
    {
	if (SoundWriteFile((char *) data, numBytes, out) != numBytes)
	    fatalError("Error writing output file");
    }
    else
	rateConvert(in, out, numBytes, data);

    if (SoundCloseFile(out))
	fatalError("Error closing output file");

    if (!outputFile)
	if (rename(outName, inputFile))
	    fatalError("Error renaming temp file");

    exit(0);
}
