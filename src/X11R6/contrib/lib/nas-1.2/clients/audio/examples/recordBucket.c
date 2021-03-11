/**
 * Example of recording to a bucket
 *
 * This program creates a bucket and records into it.  The input
 * device's current gain and line mode are used.  You may want to use
 * aupanel to adjust these.  Once the recording has finished, you can
 * use audemo to play the bucket.  The bucket will be automatically
 * destroyed when this program exits.
 *
 * $NCDId: @(#)recordBucket.c,v 1.1 1994/04/28 23:00:36 greg Exp $
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
    AuServer       *aud;
    AuBool          done;
    AuEvent         ev;
    AuDeviceAttributes *da = NULL;
    AuBucketID      bucket;
    int             i;

    if (!(aud = AuOpenServer(NULL, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't open audio server");

    if (!(bucket = AuCreateBucket(aud, AuFormatULAW8, 1, AuAccessAllMasks,
				  8000, 80000, NULL, NULL)))
	fatalError("Error creating bucket");

    /* look for a one track input device */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if ((AuDeviceKind(AuServerDevice(aud, i)) ==
	     AuComponentKindPhysicalInput) &&
	    AuDeviceNumTracks(AuServerDevice(aud, i)) == 1)
	{
	    da = AuServerDevice(aud, i);
	    break;
	}

    if (!da)
	fatalError("Couldn't find appropriate input device");

    printf("Press return to begin recording\n");
    getchar();

    AuSoundRecordToBucket(aud, bucket, AuDeviceIdentifier(da), AuDeviceGain(da),
			  doneCB, (AuPointer) &done, AuDeviceLineMode(da),
			  NULL, NULL, NULL);

    while (!done)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }

    printf("Press return to quit\n");
    getchar();
}
