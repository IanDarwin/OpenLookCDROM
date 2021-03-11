/**
 * playFromBckt - loads a sound file into a bucket and plays it
 *
 * usage: playFromBckt file [bucket offset] [number of samples]
 *
 * Demonstrates the usage of some low-level audio library functions
 *
 * $NCDId: @(#)playFromBckt.c,v 1.3 1994/05/09 18:41:37 greg Exp $
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
    AuServer       *aud;
    AuBucketID      bucket;
    AuBucketAttributes *ba;
    AuDeviceID      device = AuNone;
    AuFlowID        flow;
    AuElement       elements[2];
    AuEventHandlerRec *handler;
    AuBool          done = AuFalse;
    AuUint32        samples = AuUnlimitedSamples;
    AuInt32         offset = 0;
    char           *file = argv[1];
    int             i;

    if (argc < 2)
	fatalError("usage: playFromBckt filename [bucket offset] [number of samples]");

    argc -= 2;

    if (argc)
    {
	offset = atoi(argv[2]);
	argc--;
    }

    if (argc)
    {
	samples = atoi(argv[3]);
	argc--;
    }

    if (!(aud = AuOpenServer(NULL, 0, NULL, 0, NULL, NULL)))
	exit(1);

    if (!(bucket = AuSoundCreateBucketFromFile(aud, file, AuAccessAllMasks, &ba,
					       NULL)))
	exit(1);

    /* look for an output device */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if ((AuDeviceKind(AuServerDevice(aud, i)) ==
	     AuComponentKindPhysicalOutput) &&
	 AuDeviceNumTracks(AuServerDevice(aud, i)) == AuBucketNumTracks(ba))
	{
	    device = AuDeviceIdentifier(AuServerDevice(aud, i));
	    break;
	}

    if (device == AuNone)
	fatalError("Couldn't find an output device");

    if (!(flow = AuCreateFlow(aud, NULL)))
	fatalError("Couldn't create flow");

    AuMakeElementImportBucket(&elements[0], AuBucketSampleRate(ba), bucket,
			      samples, offset, 0, NULL);
    AuMakeElementExportDevice(&elements[1], 0, device, AuBucketSampleRate(ba),
			      AuUnlimitedSamples, 0, NULL);

    AuSetElements(aud, flow, AuTrue, 2, elements, NULL);

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
