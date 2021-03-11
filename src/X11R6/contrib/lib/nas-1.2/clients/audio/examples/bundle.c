/**
 * Plays stereo data on a mono device or mono data on a stereo device
 * using one of the following flows:
 *
 *                                             Track 0
 *                               ++=== BUNDLER --------
 * STEREO Tracks 0,1             ||                    \____           MONO
 * BUCKET =========== MULT .5 ===++                     ____ SUM ---- DEVICE
 *                               ||            Track 1 /
 *                               ++=== BUNDLER --------
 *
 *
 *  MONO                                STEREO
 * BUCKET ---------- BUNDLER ========== DEVICE
 *
 * $NCDId: @(#)bundle.c,v 1.2 1994/05/09 18:41:58 greg Exp $
 */

#include <stdio.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static AuBool
EventHandler(aud, ev, handler)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
{
    AuBool         *done = (AuBool *) handler->data;
    AuElementNotifyEvent *event = (AuElementNotifyEvent *) ev;

    if (ev->type == AuEventTypeElementNotify &&
	event->kind == AuElementNotifyKindState &&
	event->cur_state == AuStateStop)
	*done = AuTrue;

    return AuTrue;
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    char           *file = argv[1];
    AuServer       *aud;
    Sound           s;
    AuBucketID      bucket;
    AuBucketAttributes *ba;
    int             i,
                    toTracks;
    AuDeviceID      device = AuNone;
    AuFlowID        flow;
    AuElement       elements[6];
    AuInputTrack    track[2];
    unsigned short  inputs[2];
    AuEventHandlerRec *handler;
    AuBool          done = AuFalse;

    if (argc < 2)
	fatalError("usage: bundle filename");

    if (!(s = SoundOpenFileForReading(file)))
	fatalError("Can't open %s");

    toTracks = SoundNumTracks(s) == 1 ? 2 : 1;
    SoundCloseFile(s);

    if (!(aud = AuOpenServer(NULL, 0, NULL, 0, NULL, NULL)))
	exit(1);

    /* look for an output device */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if ((AuDeviceKind(AuServerDevice(aud, i)) ==
	     AuComponentKindPhysicalOutput) &&
	    AuDeviceNumTracks(AuServerDevice(aud, i)) == toTracks)
	{
	    device = AuDeviceIdentifier(AuServerDevice(aud, i));
	    break;
	}

    if (device == AuNone)
	fatalError("Couldn't find an output device");

    bucket = AuSoundCreateBucketFromFile(aud, file, AuAccessAllMasks, &ba,
					 NULL);

    if (!(flow = AuCreateFlow(aud, NULL)))
	fatalError("Couldn't create flow");

    AuMakeElementImportBucket(&elements[0], AuBucketSampleRate(ba), bucket,
			      AuUnlimitedSamples, 0, 0, NULL);

    if (toTracks == 1)
    {
	AuMakeElementMultiplyConstant(&elements[1], 0,
				      AuFixedPointFromFraction(1, 2));

	AuMakeInputTrack(&track[0], 1, 0);
	AuMakeElementBundle(&elements[2], 1, &track[0]);

	AuMakeInputTrack(&track[1], 1, 1);
	AuMakeElementBundle(&elements[3], 1, &track[1]);

	inputs[0] = 2;
	inputs[1] = 3;
	AuMakeElementSum(&elements[4], 2, inputs);

	AuMakeElementExportDevice(&elements[5], 4, device,
				  AuBucketSampleRate(ba),
				  AuUnlimitedSamples, 0, NULL);

	AuSetElements(aud, flow, AuTrue, 6, elements, NULL);
    }
    else
    {
	AuMakeInputTrack(&track[0], 0, 0);
	AuMakeInputTrack(&track[1], 0, 0);
	AuMakeElementBundle(&elements[1], 2, track);

	AuMakeElementExportDevice(&elements[2], 1, device,
				  AuBucketSampleRate(ba),
				  AuUnlimitedSamples, 0, NULL);

	AuSetElements(aud, flow, AuTrue, 3, elements, NULL);
    }

    AuFreeBucketAttributes(aud, 1, ba);

    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerIDMask,
					   0, flow, EventHandler,
					   (AuPointer) &done)))
	fatalError("Couldn't register event handler");

    AuStartFlow(aud, flow, NULL);

    while (!done)
    {
	AuEvent         ev;

	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }

    AuUnregisterEventHandler(aud, handler);
    AuDestroyFlow(aud, flow, NULL);
    AuDestroyBucket(aud, bucket, NULL);
    AuCloseServer(aud);
    return 0;
}
