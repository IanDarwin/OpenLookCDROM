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
 * $NCDId: @(#)auplay.c,v 1.19 1993/06/15 01:04:58 greg Exp $
 */

/*
 * auplay -- a trivial program for playing audio files.
 */

#include	<stdio.h>
#include	<audio/audiolib.h>
#include	<audio/soundlib.h>

static AuServer *aud;
static int      volume = 100,
                infoflag = 0,
                playflag = 1;
static char    *progname;

static void
usage()
{
    fprintf(stderr,
       "Usage:  %s [-iI] [-audio servername] [-volume percent] files ...\n",
	    progname);
    exit(1);
}

static void
do_file(fname)
char           *fname;
{
    Sound       s;

    if (infoflag)
    {
	s = SoundOpenFileForReading(fname);

	if (s)
	{
	    printf("%15s %.80s\n", "Filename:", fname);
	    printf("%15s %.80s\n", "File Format:", SoundFileFormatString(s));
	    printf("%15s %.80s\n", "Data Format:",
		   AuFormatToString(SoundDataFormat(s)));
	    printf("%15s %d\n", "Tracks:", SoundNumTracks(s));
	    printf("%15s %d Hz\n", "Frequency:", SoundSampleRate(s));
	    printf("%15s %.2f seconds\n", "Duration:",
		   (float) SoundNumSamples(s) / SoundSampleRate(s));
	    printf("\n%s\n", SoundComment(s));
	    SoundCloseFile(s);
	}
	else
	{
	    fprintf(stderr, "Couldn't open file \"%s\"\n", fname);
	    return;
	}
    }

    if (playflag && !AuSoundPlaySynchronousFromFile(aud, fname, volume))
	fprintf(stderr, "Couldn't play file \"%s\"\n", fname);
}

main(argc, argv)
int             argc;
char          **argv;
{
    int             i,
                    numfnames;
    char           *auservername = NULL;
    AuBool          did_file = AuFalse;

    progname = argv[0];

    argc--;
    numfnames = argc;
    argv++;

    while (argv && argv[0] && *argv[0] == '-')
    {
	if (!strncmp(argv[0], "-a", 2))
	{
	    if (argv[1])
	    {
		argv++;
		numfnames--;
		auservername = argv[0];
	    }
	    else
		usage();
	}
	else if (!strncmp(argv[0], "-v", 2))
	{
	    if (argv[1])
	    {
		argv++;
		numfnames--;
		volume = atoi(argv[0]);
	    }
	    else
		usage();
	}
	else if (!strncmp(argv[0], "-i", 2))
	{
	    infoflag = 1;
	}
	else if (!strncmp(argv[0], "-I", 2))
	{
	    infoflag = 1;
	    playflag = 0;
	}
	else
	    usage();
	argv++;
	numfnames--;
    }

    if (playflag)
    {
	aud = AuOpenServer(auservername, 0, NULL, 0, NULL, NULL);
	if (!aud)
	{
	    fprintf(stderr, "Can't connect to audio server\n");
	    exit(-1);
	}
    }

    for (i = 0; i < numfnames; i++)
    {
	do_file(argv[i]);
	did_file = AuTrue;
    }

    if (!did_file)		/* must want stdin */
	do_file("-");

    if (playflag)
	AuCloseServer(aud);

    exit(0);
}
