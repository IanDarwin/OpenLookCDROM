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
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include <sys/file.h>
#include <limits.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>

#define DEFAULT_FILE "bob.snd"		/* Needs SOUND_PATH */

#define BUFSIZE  2.0			/* buffer size, in seconds */
int flushflag = 0;
int bufsize;				/* buffer size, in seconds */

int int_flag = 0;
void (*old_handler)(int);

void handle_int(int xxx)
{
  /* notify main loop of the requested exit */
  int_flag = 1;
  /* assure that we don't wait around */
  flushflag = 0;
  /* restore original handler, so that a second ^C will force an exit */
  signal(SIGINT, old_handler);
}

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

#ifndef PATH_MAX
#define PATH_MAX 1024	/* Sun doesn't have this in limits.h */
#endif

int
open_sound_file(const char *progname, const char *filename)
{
    int fd;
    char *spath, *next;
    char fullname[PATH_MAX];

    if ((fd = open(filename, O_RDONLY, 0)) >= 0)
	return(fd);

    spath = getenv("SOUND_PATH");
    while (spath) {
	next = strchr(spath, ':');
	if (next)
	    *next++ = 0;
	sprintf(fullname,"%s/%s",spath,filename);
	if ((fd = open(fullname, O_RDONLY, 0)) >= 0)
	    return(fd);
	spath = next;
    }

    fprintf(stderr, "%s: Can't open file %s or %s\n",
	    progname,filename,fullname);
    return(-1);
}

main(int argc, char **argv)
{
    ATime t;
    ATime nact;
    AFAudioConn *aud;
    AC ac;
    AFSetACAttributes attributes;
    int fd;
    int device;
    int nbytes;
    unsigned char *buf;
    int unitSize;		/* sample size, in bytes */
    int srate;			/* sample rate, per second */
    int units;			/* number of units */
    int nsamples;
    char *filename=(char *)NULL;
    char *audname = "";
    char *spath;
    int i;
    float toffset = 0.1;		/* seconds delay to start play */
    int gain;
    unsigned int channels;
    int endianflag = 0;

    device = -1;
    attributes.preempt = Mix;
    attributes.start_timeout = 0;
    attributes.end_silence = 0;
    attributes.play_gain = 0;
    attributes.rec_gain =  0;
    attributes.type = UNKNOWN_ENCODETYPE;

    /* Parse the command line */
    for ( i = 1; i < argc; i++ ) {
	if ( strcmp( argv[i], "-d" ) == 0) {
	    if(++i < argc)
		device = atoi(argv[i]);
	    else {
		fprintf(stderr,"%s: missing device\n", 
			argv[0]);
		exit(1);
	    }
	} else if ( strcmp( argv[i], "-server" ) == 0) {
	    if(++i < argc)
		audname = argv[i];
	    else {
		fprintf(stderr,"%s: missing server name\n", 
			argv[0]);
		exit(1);
	    }
	} else if ( strcmp( argv[i], "-t" ) == 0) {
	    if(++i < argc)
		if (sscanf(argv[i],"%f",&toffset) != 1) {
		    fprintf(stderr,"%s: missing time offset\n",
			    argv[0]);
		    exit(1);
		}
	} else if ( strcmp( argv[i], "-c" ) == 0) {
	    filename = DEFAULT_FILE;
	} else if ( strcmp( argv[i], "-g" ) == 0) {
	    if(++i < argc) {
		if (sscanf(argv[i],"%d",&gain) != 1) {
		    fprintf(stderr,"%s: missing gain value\n",
			    argv[0]);
		    exit(1);
		}
		attributes.play_gain = gain;
	    }
	} else if ( strcmp( argv[i], "-e" ) == 0) {
	    if(++i < argc) {
		attributes.type = AFFindEncodeType(argv[i]);
		if (attributes.type == UNKNOWN_ENCODETYPE
		  || AF_sample_sizes[attributes.type].samps_per_unit == 0) {
		    fprintf(stderr,"%s: unknown encoding type\n",
			    argv[i]);
		    AFPrintKnownEncodeTypes();
		    exit(1);
		}
	    }
	} else if ( strcmp( argv[i], "-f" ) == 0) {
	    flushflag = 1;
	} else if ( strcmp( argv[i], "-b" ) == 0) {
	    attributes.endian = ABigEndian;
	    endianflag = ACEndian;
	} else if ( strcmp( argv[i], "-l" ) == 0) {
	    attributes.endian = ALittleEndian;
	    endianflag = ACEndian;
	} else {
	    filename = argv[i];
	}
    }

    /* either open sound file or use stdin */
    if (filename != (char *) NULL) {
	fd = open_sound_file(argv[0], filename);
	if (fd < 0)
	  exit(1);
    } else
	fd = 0;

    if ( (aud = AFOpenAudioConn(audname)) == NULL) {
	fprintf(stderr, "%s: can't open connection.\n", argv[0]);
	exit(1);
    }

/* set up audio context, find sample size and sample rate */
    if((device < 0) || (device >= aud->ndevices))
	device = FindDefaultDevice(aud);
    if (attributes.type == UNKNOWN_ENCODETYPE)
      attributes.type = aud->devices[device].playBufType;
    ac = AFCreateAC(aud, device, (ACPlayGain | ACEncodingType | endianflag), 
		    &attributes);
    AFSync(aud, 0);	/* Make sure we confirm encoding type support. */
    srate = ac->device->playSampleFreq;
    channels = ac->device->playNchannels;
    unitSize = BytesPerUnit(attributes.type) * channels;
    bufsize = (srate / SampsPerUnit(attributes.type)) * BUFSIZE;

/* allocate play buffer */
    if((buf = (unsigned char *)malloc(bufsize*unitSize)) == NULL) {
	fprintf(stderr, "Couldn't allocate play buffer\n");
	exit(1);
    }

    if((nbytes = read(fd, buf, bufsize*unitSize)) <= 0) 
	exit(0);

    old_handler = signal(SIGINT, handle_int);

    t = AFGetTime(ac) + (toffset * srate);
    do{

	units = nbytes / unitSize;
	nsamples = units * SampsPerUnit(attributes.type);
	nact = AFPlaySamples(ac, t, units*unitSize, buf);
/*  printf("time %d bytes %d units %d unitSize %d samps %d\n", t, nbytes,
       units, unitSize, nsamples); */

	t += nsamples;
	/* AF holds samples from nact to t */
	if (int_flag) break;
    } while ( (nbytes = read(fd, buf, bufsize*unitSize)) > 0);
    if (int_flag)
    {
      AFSilence(attributes.type, buf, bufsize);
      attributes.preempt = 1;
      AFChangeACAttributes(ac, ACPreemption, &attributes);
      while (nact < t)
	{
	  nsamples = bufsize * SampsPerUnit(attributes.type);
	  (void) AFPlaySamples(ac, nact, bufsize*unitSize, buf);
	  nact += nsamples;
	}
    }
    else if (flushflag) {
	while ((((int) AFGetTime(ac)) - ((int) t)) < 0) 
		sleep(1);
    }
    AFCloseAudioConn(aud);
    return 0;
}
