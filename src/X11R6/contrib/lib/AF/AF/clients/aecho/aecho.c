/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <AF/AFlib.h>
#include <sys/file.h>
#include <limits.h>
#include <math.h>

#define BUFSIZE  8000			/* buffer size, in bytes */


/* convert data type to sample size.  Should this be in the AF include
   file somewhere for everyone to use?  See the type encodings in 'audio.h'
*/
int sample_sizes[] = {
        1,              /* MU255 */
        1,              /* ALAW */
        2,              /* Linear PCM, 16 bits, -1.0 <= x < 1.0 */
        2,              /* Linear PCM, 32 bits, -1.0 <= x < 1.0 */
        1,              /* G.721, 64Kbps to/from 32Kbps. */
        1,              /* G.723, 64Kbps to/from 32Kbps. */
        0
};

/*
 * Find a suitable default device (the first device not connected to the phone),
 *  returns device number.
 *
 * Returns -1 if no suitable device can be found.
 */
int FindDefaultDevice(AFAudioConn *aud)
{
        AFDeviceDescriptor *aDev;
        int     i;

        for(i=0; i<ANumberOfAudioDevices(aud); i++) {
                aDev = AAudioDeviceDescriptor(aud, i);
                if(aDev->inputsFromPhone == 0 && aDev->outputsToPhone == 0)
                        return i;
        }
        return -1;
}

/*
 * Show program usage and exit with an erorr.
 */
usage(void)
{
	fprintf(stderr, "usage: aecho {-d #}\n");
	exit(1);
}

int main(int argc, char **argv)
{
	ATime playtime, rectime;
	AC ac;
	AFAudioConn *aud;
	int i;
	int device = -1;
	unsigned char *playbuf;		/* play sample buffer */
	unsigned char *recbuf;		/* record sample buffer */
    	int ssize;			/* sample size, in bytes */
    	int srate;                      /* sample rate, per second */
	AEncodeType type;
	int channels;
	int infd, outfd;		/* input and output file descriptors */
	int nbytes;
	float	toffset = 0.125;	/* start time offset, in seconds */

/*
 * Parse the command line.
 */
	for(i=1; i<argc; i++) {
		if ( strcmp( argv[i], "-d" ) == 0) {
			if(++i < argc)
				device = atoi(argv[i]);
			else {
				fprintf(stderr,"%s: missing device\n", argv[0]);
				exit(1);
			}
		} else
			usage();
	}

/*
 * Open connection to audio server.
 */
	if((aud = AFOpenAudioConn("")) == NULL) {
		fprintf(stderr, "%s: can't open server connection.\n", argv[0]);
		exit(1);
	}
	infd = fileno(stdin);
	outfd = fileno(stdout);

	if(device < 0)
		device = FindDefaultDevice(aud);

/*
 *  Set up audio context, find sample size and sample rate.
 */
	ac = AFCreateAC(aud, device, 0, NULL);
	srate = ac->device->recSampleFreq;
	type = ac->device->recBufType;
	channels = ac->device->recNchannels;
	ssize = sample_sizes[type] * channels;

/*
 * Allocate buffers for playback and record.
 */
	if((playbuf = (unsigned char *)malloc(BUFSIZE*ssize)) == NULL) {
 		fprintf(stderr, "Couldn't allocate play buffer\n");
		exit(1);
	}
	if((recbuf = (unsigned char *)malloc(BUFSIZE*ssize)) == NULL) {
 		fprintf(stderr, "Couldn't allocate record buffer\n");
		exit(1);
	}
	if((nbytes = read(infd, playbuf, BUFSIZE*ssize)) <= 0)
		exit(0);

	playtime = rectime = AFGetTime(ac) + (toffset * srate);
	(void)AFPlaySamples(ac, playtime, nbytes, playbuf);
	playtime += nbytes / ssize;

/*
 * Repeat alternating play and record.
 */
	for(;;) {
		if((nbytes = read(infd, playbuf, BUFSIZE*ssize)) <= 0)
			break;

		(void)AFPlaySamples(ac, playtime, nbytes, playbuf);
		playtime += nbytes / ssize;

		(void)AFRecordSamples(ac, rectime, nbytes, recbuf, ABlock);
		if(write(outfd, recbuf, nbytes) != nbytes)
			fprintf(stderr, "write failed\n");
		rectime += nbytes / ssize;
	}

/*
 *  Finish up with last bit of record samples.
 */
	nbytes = (playtime - rectime) * ssize;
	(void)AFRecordSamples(ac, rectime, nbytes, recbuf, ABlock);
	if(write(outfd, recbuf, nbytes) != nbytes)
		fprintf(stderr, "write failed\n");

	AFCloseAudioConn(aud);
	exit(0);
	/*NOTREACHED*/
}
