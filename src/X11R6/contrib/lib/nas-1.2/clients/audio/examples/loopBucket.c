/**
 * loopBucket - loads a sound file into a bucket loops it continuously
 *
 * usage: loopBucket file
 *
 * Demonstrates the usage of some low-level audio library functions
 * and actions.
 *
 * $NCDId: @(#)loopBucket.c,v 1.2 1994/05/09 18:42:38 greg Exp $
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
    AuElementAction actions[2];
    AuBool          done = AuFalse;
    char           *file = argv[1];
    int             i;

    if (argc < 2)
	fatalError("usage: loopBucket filename");

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

    AuMakeChangeStateAction(&actions[0], AuStateStop, AuStateStart,
			    AuReasonEOF, flow, AuElementAll, AuStateStart);

    AuMakeElementImportBucket(&elements[0], AuBucketSampleRate(ba), bucket,
			      AuUnlimitedSamples, 0, 1, actions);
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
