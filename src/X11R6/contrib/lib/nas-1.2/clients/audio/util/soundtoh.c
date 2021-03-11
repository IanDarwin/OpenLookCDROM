/*
 * Copyright 1993 Greg Renda and Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
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
 * Author: Greg Renda greg@ncd.com
 * 
 * Network Computing Devices, Inc 350 North Bernardo Ave Mountain View, CA 94043
 * 
 * $NCDId: @(#)soundtoh.c,v 1.2 1994/04/07 20:23:32 greg Exp $
 */

#include <stdio.h>
#include <malloc.h>
#include <audio/Aos.h>			/* for string and other os stuff */
#include <audio/Afuncs.h> 		/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/sound.h>

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static char    *
convertQuotes(str)
char           *str;
{
    int             len = strlen(str) * 2 + 1;	/* more then enough for
						 * string */
    char           *new,
                   *d,
                   *s;

    s = str;
    d = new = (char *) malloc(len);
    if (!new)
	return new;
    while (*s)
    {
	if (*s == '"')
	    *d++ = '\\';
	*d++ = *s++;
    }
    free(str);
    return new;
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    Sound           s;
    char           *p,
                   *name = argv[1];
    int             i;

    if (argc != 2)
	fatalError("usage: soundtoh filename");

    if (!(s = SoundOpenFileForReading(name)))
	fatalError("Can't open file \"%s\"", name);

    if ((p = strrchr(name, '.')))
	*p = 0;

    if ((p = strrchr(name, '/')))
	name = p + 1;

    printf("/* %s */\n\n", name);
    printf("#define %sDataFormat\t%s\n", name,
	   AuFormatToDefine(SoundDataFormat(s)));
    printf("#define %sSampleRate\t%d\n", name, SoundSampleRate(s));
    printf("#define  %sNumTracks\t%d\n", name, SoundNumTracks(s));
    printf("#define %sNumSamples \\\n\
    (sizeof(%sSamples) / sizeof(%sSamples[0]) / %sNumTracks)\n",
	   name, name, name, name);
    p = convertQuotes(SoundComment(s));
    printf("\nstatic char *%sComment = \"%s\";\n\n", name, p);
    free(p);

    if (SoundBytesPerSample(s) == 2)
    {
	unsigned short  sample;
	int             endian = 1,
	                swap;

	if (*(char *) &endian)	/* little endian */
	{
	    printf("/* data generated for a little endian machine */\n");
	    swap = (SoundDataFormat(s) == AuFormatLinearSigned16MSB ||
		    SoundDataFormat(s) == AuFormatLinearUnsigned16MSB);
	}
	else
	{
	    printf("/* data generated for a big endian machine */\n");
	    swap = (SoundDataFormat(s) == AuFormatLinearSigned16LSB ||
		    SoundDataFormat(s) == AuFormatLinearUnsigned16LSB);
	}

	printf("static unsigned short %sSamples[] =\n{", name);

	for (i = 0; i < SoundNumSamples(s); i++)
	{
	    if (!SoundReadFile((char *)&sample, SoundBytesPerSample(s), s))
		break;

	    if (!(i % 8))
		printf("\n    ");

	    printf("0x%04x, ", swap ?
		((sample >> 8) & 0xff) | ((sample << 8) & 0xff00) : sample);
	}

	printf("\n};\n");
    }
    else if (SoundBytesPerSample(s) == 1)
    {
	unsigned char   sample;

	printf("static unsigned char %sSamples[] =\n{", name);

	for (i = 0; i < SoundNumSamples(s); i++)
	{
	    if (!SoundReadFile((char *)&sample, SoundBytesPerSample(s), s))
		break;

	    if (!(i % 8))
		printf("\n    ");

	    printf("0x%02x, ", sample);
	}

	printf("\n};\n");
    }

    SoundCloseFile(s);
    exit(0);
}
