/**
 * playSimul - plays multiple files simultaneously
 *
 * usage: playSimul [-v <volume in percent>] file1 file2 file3 ...
 *
 * Demonstrates AuSoundPlayFromFile()
 *
 * $NCDId: @(#)playSimul.c,v 1.2 1994/05/04 17:30:58 greg Exp $
 */

#include <stdio.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>

static void
doneCB(aud, handler, ev, data)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
AuPointer       data;
{
    int            *count = (int *) data;

    (*count)--;
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    AuServer       *aud;
    AuEvent         ev;
    int             volume = 100,
                    count = 0;
    char          **arg = &argv[1];

    if (!(aud = AuOpenServer(NULL, 0, NULL, 0, NULL, NULL)))
	exit(1);

    argc--;

    while (argc--)
    {
	if (arg[0][0] == '-' && arg[0][1] == 'v')
	{
	    if (argc)
	    {
		arg++;
		argc--;
		volume = atoi(arg);
	    }
	}
	else if (AuSoundPlayFromFile(aud, *arg, AuNone,
				     AuFixedPointFromFraction(volume, 100),
				     doneCB, (AuPointer) &count, NULL, NULL,
				     NULL, NULL))
	    count++;

	arg++;
    }

    while (count)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }

    AuCloseServer(aud);
    return 0;
}
