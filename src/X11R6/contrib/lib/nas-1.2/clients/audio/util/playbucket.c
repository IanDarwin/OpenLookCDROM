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
 * $NCDId: @(#)playbucket.c,v 1.1 1994/01/18 20:09:15 greg Exp $
 */

#include <stdio.h>
#include <string.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>

#define USAGE "usage: playbucket [-a audioServer] [-v volume] file"

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
doneCB(aud, handler, ev, data)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
AuPointer       data;
{
    AuBool         *done = (AuBool *) data;

    *done = AuTrue;
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    Sound           s;
    char           *audioServer = NULL,
                   *volume = "100",
                  **arg = &argv[1];
    AuServer       *aud;
    AuBucketAttributes *list,
                    b;
    int             n;
    AuBucketID      id;
    AuBool          done = AuFalse;
    AuStatus        status;
    AuEvent         ev;

    while (argc > 1 && arg[0][0] == '-')
	switch (arg[0][1])
	{
	  case 'a':
	    audioServer = arg[1];
	    arg += 2;
	    argc -= 2;
	    break;
	  case 'v':
	    volume = arg[1];
	    arg += 2;
	    argc -= 2;
	    break;
	  default:
	    fatalError(USAGE);
	}

    if (argc != 2)
	fatalError(USAGE);

    if (!(s = SoundOpenFileForReading(*arg)))
	fatalError("Can't open file %s", *arg);

    if (!(aud = AuOpenServer(audioServer, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    AuSetString(AuBucketDescription(&b),
		AuStringLatin1, strlen(SoundComment(s)), SoundComment(s));

    list = AuListBuckets(aud, AuCompCommonDescriptionMask, &b, &n, NULL);

    SoundCloseFile(s);

    if (!n)
    {
	AuSetCloseDownMode(aud, AuCloseDownRetainPermanent, NULL);
	id = AuSoundCreateBucketFromFile(aud, *arg, AuAccessAllMasks, NULL,
					 NULL);
    }
    else
    {
	id = AuBucketIdentifier(list);
	AuFreeBucketAttributes(aud, n, list);
    }

    AuSoundPlayFromBucket(aud, id, AuNone,
			AuFixedPointFromFraction(atoi(volume), 100), doneCB,
			  (AuPointer) &done, 1, NULL, NULL, NULL, &status);

    while (!done)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }

    AuCloseServer(aud);
    return 0;
}
