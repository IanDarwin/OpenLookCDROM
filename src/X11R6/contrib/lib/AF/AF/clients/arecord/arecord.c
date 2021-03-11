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
#include <AF/AFlib.h>
#include <AF/AFUtils.h>
#include <sys/file.h>
#include <limits.h>
#include <math.h>
#include <signal.h>

#define BUFSIZE  0.5			/* buffer size, in seconds */
#define	MSTOTICKS(x)	((x)<<3)
int bufsize;				/* buffer size, in samples */

double silent_time, silent_level;
int silent_time_flag = 0;
int silent_level_flag = 0;
int print_power_flag = 0;
double silent_run = 0.0;
int doneflag = 0;

/* When a SIGINT arrives, we just set the done flag and return.
   Later, in the main loop, the flag is checked and the program
   exits cleanly.  A second SIGINT will kill the program. */
void handle_int(int xxx)
{
  doneflag = 1;
  signal(SIGINT, SIG_DFL);
}


/*
 * Find a suitable default device (the first device not connected to 
 * the phone), returns device number.
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


main(int argc, char **argv)
{
	ATime t;
	AC ac;
	AFAudioConn *aud;
	FILE *f;
	int i;
	int device;
	double spow;
	double gain;
	unsigned char *buf;		/* record sample buffer */
	int unitSize;                   /* bytes per encoding unit */
	int unitsPerSecond;             /* encode units per second */
    	int srate;                      /* sample rate, per second */
	int units = INT_MAX;		/* number of units */
   	int nsamples;		        /* number of samples */
	AEncodeType type;
	int channels;
	char *audname = "";
	char *filename=(char *)NULL;
	AFSetACAttributes attributes;
	int endianflag = 0;
	float	length = -1.0;		/* length, in seconds */
	float	toffset = 0.125;	/* start time offset, in seconds */

	signal(SIGINT, handle_int);     /* set up ^C handler */
	attributes.rec_gain =  0;
	attributes.type = UNKNOWN_ENCODETYPE;

	device = -1;

	/* Parse the command line. 					*/
	for ( i = 1; i < argc; i++ ) {
	  if ( strcmp( argv[i], "-server" ) == 0) {
	    if(++i < argc)
	      audname = argv[i];
	    else {
	      fprintf(stderr,"%s: missing audio server\n", argv[0]);
	      exit(1);
	    }
	  } 
	  else if ( strcmp( argv[i], "-d" ) == 0) {
	    if(++i < argc)
	      device = atoi(argv[i]);
	    else {
	      fprintf(stderr,"%s: missing device\n", argv[0]);
	      exit(1);
	    }
	  } 
	  else if ( strcmp( argv[i], "-e" ) == 0) {
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
	  } 
	  else if ( strcmp( argv[i], "-g" ) == 0) {
	    if(++i < argc) {
	      if (sscanf(argv[i],"%d",&gain) != 1) {
		fprintf(stderr,"%s: missing gain value\n",
			argv[0]);
		exit(1);
	      }
	      attributes.rec_gain = gain;
	    }
	  }
	  else if ( strcmp( argv[i], "-l" ) == 0) {
	    if(++i < argc)
	      if (sscanf(argv[i],"%f",&length) != 1) {
		fprintf(stderr,"%s: missing time length\n",
			argv[0]);
		
		exit(1);
	      }
	  } 
	  else if ( strcmp( argv[i], "-t" ) == 0) {
	    if(++i < argc)
	      if (sscanf(argv[i],"%f",&toffset) != 1) {
		fprintf(stderr,"%s: missing time offset\n",
			argv[0]);
		exit(1);
	      }
	  } 
	  else if ( strcmp( argv[i], "-silentlevel" ) == 0) {
	    if(++i < argc)
	      if (sscanf(argv[i],"%lf",&silent_level) == 1)
		silent_level_flag = 1;
	    
	      else {
		fprintf(stderr,"%s: missing silentlevel\n",
			argv[0]);
		exit(1);
	      }
	  }
	  else if ( strcmp( argv[i], "-B" ) == 0) {
	    attributes.endian = ABigEndian;
	    endianflag = ACEndian;
	  } 
	  else if ( strcmp( argv[i], "-L" ) == 0)  {
	    attributes.endian = ALittleEndian;
	    endianflag = ACEndian;
	  }
	  else if ( strcmp( argv[i], "-printpower" ) == 0)
	    print_power_flag = 1;
	  else if ( strcmp( argv[i], "-silenttime" ) == 0) {
	    if(++i < argc)
	      if (sscanf(argv[i],"%lf",&silent_time) == 1)
		silent_time_flag = 1;
	    
	      else {
		fprintf(stderr,"%s: missing silenttime\n",
			argv[0]);
		exit(1);
	      }
	  } 
	  else {
	    filename = argv[i];
	  }
	}
	
	if (filename != (char *) NULL) {
	  if ( (f = fopen(filename, "w")) == NULL) {
	    fprintf(stderr, 
		    "%s: Can't open file %s\n", argv[0], filename);
	    exit(1);
	  }
	} else {
	  f = stdout;
	}
	if (silent_level_flag && !silent_time_flag)
	  {
	    silent_time = 3.0;
	    silent_time_flag = 1;
	  }
	if (silent_time_flag && !silent_level_flag)
	  {
	    silent_level = -60.0;
	    silent_level_flag = 1;
	  }
	if ( (aud = AFOpenAudioConn(audname)) == NULL) {
	  fprintf(stderr, "%s: can't open connection.\n", argv[0]);
	  exit(1);
	}
	
	/* set up audio context, find sample size and sample rate */
	if(device < 0)
	  device = FindDefaultDevice(aud);
	if (attributes.type == UNKNOWN_ENCODETYPE)
	  attributes.type = aud->devices[device].playBufType;
	
	ac = AFCreateAC(aud, device, 
			ACRecordGain | ACEncodingType | endianflag, 
			&attributes);
	srate = ac->device->recSampleFreq;
	channels = ac->device->recNchannels;
	unitSize = AF_sample_sizes[attributes.type].bytes_per_unit * channels;
	unitsPerSecond = srate / 
	  AF_sample_sizes[attributes.type].samps_per_unit;
	if(length >= 0)
		units = unitsPerSecond * length;
	bufsize = BUFSIZE * srate;

/* allocate record buffer */
	if((buf = (unsigned char *)malloc(bufsize*unitSize)) == NULL) {
 		fprintf(stderr, "Couldn't allocate record buffer\n");
		exit(1);
	}

	t = AFGetTime(ac) + (toffset * srate);
	while (units > 0) {
		int nb = (units > bufsize) ? bufsize : units;
		if (doneflag) break; /* ^C */
		AFRecordSamples(ac, t, nb * unitSize, buf, ABlock);
		spow = AFLintodB(AFPower(buf, nb, attributes.type));
		t += nb * AF_sample_sizes[attributes.type].samps_per_unit;
		units -= nb;
		if (fwrite(buf, unitSize, nb, f) != nb) {
			fprintf(stderr, "%s: write error to file\n", argv[0]);
			exit(1);
		}
		fflush(f);
		if (silent_level_flag)
		{
		  if ((spow < silent_level) && (nb > 0))
		    silent_run += ((double) (nb * 
			      AF_sample_sizes[attributes.type].samps_per_unit)
				   / ((double) srate));
		  
		  if (silent_run >= silent_time)
		    break;
		  
		  if (spow > silent_level) 
		    silent_run = 0.0;
		}
		if (print_power_flag)
		{
			fprintf(stderr, " %6.2f\n", spow);
			fflush(stderr);
		}
	}
	AFCloseAudioConn(aud);
	return 0;
}
