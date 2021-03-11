/**
 * playFile - a simple Network Audio System audio file player
 *
 * usage: playFile [-v <volume in percent>] file
 *
 * Demonstrates AuSoundPlaySynchronousFromFile()
 *
 * $NCDId: @(#)playFile.c,v 1.1 1994/04/28 23:00:21 greg Exp $
 */

#include <stdio.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>

main(argc, argv)
int             argc;
char          **argv;
{
    char           *file = argv[1];
    int             volume = 100;
    AuServer       *aud;

    if (argc == 4)
    {
	if (argv[1][0] == '-' && argv[1][1] == 'v')
	{
	    volume = atoi(argv[2]);
	    file = argv[3];
	}
	else
	    exit(1);
    }
    else if (argc != 2)
	exit(1);

    if (!(aud = AuOpenServer(NULL, 0, NULL, 0, NULL, NULL)))
	exit(1);

    return AuSoundPlaySynchronousFromFile(aud, file, volume) ? 0 : 1;
}
