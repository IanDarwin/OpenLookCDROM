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
/* apass.c
 * L. Stewart 1/21/93
 *
 * This is an AudioFile client program which reads audio from one
 * server and writes it to another one.
 *
 * The program addresses two issues:
 * 1.  clock drift
 *     The sample rates on the input and output, while nominally the same
 *	will drift slowly with respect to one another because the sampling
 *	may be clocked by different clock sources with slightly different
 *	frequencies.
 *
 *	The program establishes a timing window, when the clocks drift
 *	by more than <anti-jitter> seconds, then the source and sink
 *	are resynchronized to the nominal delay.  This resynchronization
 *	is brute force, and may result in an audible blip, but it should
 *	be OK for many purposes.
 *
 * 2.	parameter update
 *	Until multithreaded systems are more widely available, it will
 *	be necessary to construct applications from single threaded
 *	programs. This makes it difficult for a program to handle both real
 *	time audio AND watch for user interface events.  (It is possible,
 *	but messy.)
 *	It is also advantageous for the user-interface to run as an
 *	entirely separate program.
 *
 *	This program addresses this problem by communicating with 
 *	a user interface via a parameter file.  The mechanism is simple,
 *	the UI modifies the parameter file and sends a SIGUSR1 signal
 *	to apass.  Apass notices the signal and rereads the parameter file.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>
#include <signal.h>

#define SLIPHIST 4

typedef struct {
  AC fac;
  AC tac;			/* from and to audio contexts */
  AFAudioConn *faud;
  AFAudioConn *taud;     /* from and to audio connections */
  ATime ft;
  ATime tt;
  ATime factt;
  ATime tactt;
  int fdevice;
  int tdevice;
  AFSetACAttributes attributes;
  AEncodeType type;
  int sample_rate;
  int bytes_per_unit;
  int samps_per_unit;
  int bufsize_bytes;
  int bufsize_samples;
  int bufsize_units;
  void *buffer;
  int delay_in_samples;
  int delay_upper_limit;	/* nominal + aj			      */
  int delay_lower_limit;	/* nominal - aj			      */
  int slip, sliphist[SLIPHIST], nextslip;
} StateRec;

StateRec forw, back;

int flag_log = 0;               /* if set, write entries to log       */
int flag_update = 0;		/* set by SIGUSR1 to trigger update   */
int flag_full_duplex = 0;       /* copy sound in both directions      */
int flag_delay_change = 0;      /* set if delay is changed            */
float param_delay = 0.2;	/* seconds delay from input to output */
float param_aj = 0.1;		/* anti-jitter tolerance	      */
int param_gain;			/* dB                                 */
float bufsize_time;

/* When a SIGUSR1 arrives, we just set the update flag and return.
   Later, in the main loop, the flag is checked and the update
   is processed synchronously. */

void handle_int(int xxx)
{
  flag_update = 1;
  signal(SIGUSR1, handle_int);
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


void CalculateFlowConstraints(StateRec *dir)
{
  int i;
  dir->bufsize_samples = bufsize_time * (float) dir->sample_rate;
  dir->bufsize_units = dir->bufsize_samples / dir->samps_per_unit;
  
  /* make the buffer a multiple of the encoding unit size */
  dir->bufsize_samples = dir->bufsize_units * dir->samps_per_unit;
  
  dir->bufsize_bytes = dir->bufsize_units * dir->bytes_per_unit;
  
  /* next three lines compute nominal, min and max param_delay after
     the recording delay is factored out */
  dir->delay_in_samples = (param_delay - bufsize_time) * 
    (float) dir->sample_rate;
  dir->delay_lower_limit = dir->delay_in_samples - 
    (param_aj * (float) dir->sample_rate);
  dir->delay_upper_limit = dir->delay_in_samples + 
    (param_aj * (float) dir->sample_rate);
  
  for (i = 0; i < SLIPHIST; i += 1)
    dir->sliphist[i] = dir->delay_in_samples;
  dir->nextslip = 0;
}

void CalculateConstraints()
{
  int i;
  if (param_aj < 0.0) param_aj = 0.0;
  if (param_aj > 1.0) param_aj = 1.0;
  if (bufsize_time < 0.1) 
    {
      bufsize_time = 0.1;
      flag_delay_change = 1;
    }
  if (bufsize_time > 0.5)
    {
      bufsize_time = 0.5;
      flag_delay_change = 1;
    }
  if (param_delay < (param_aj + bufsize_time)) 
    {
      param_delay = (param_aj + bufsize_time);
      flag_delay_change = 1;
    }
  if (param_delay > 3.0) 
    {
      param_delay = 3.0;
      flag_delay_change = 1;
    }
  
  CalculateFlowConstraints(&forw);        
  if (flag_full_duplex) CalculateFlowConstraints(&back);
}


/* update routine, called from main loop to read (reread) the
   parameter file.  getupdate modifies the global control
   variables which are then used in later passes around the main
   loop. */

void getupdate(char *filename)
{
  char buf[256], key[256];
  float value;
  FILE *f;
  int mflag = 0;
  f = fopen(filename, "r");
  if (f != NULL)
    {
      while (!feof(f) && !ferror(f))
	{
	  buf[0] = 0;
	  fgets(buf, 256, f);
	  sscanf(buf, "%s %f", key, &value);
	  if (strcmp(key, "gain") == 0)
	    {
	      param_gain = value;	
	      AFSetOutputGain(forw.tac, param_gain);
	      if (flag_full_duplex) AFSetOutputGain(back.tac, param_gain);
	    }
	  if (strcmp(key, "delay") == 0)
	    {
	      param_delay = value;
	      flag_delay_change = 1;
	      mflag = 1;
	    }
	  if (strcmp(key, "buffering") == 0)
	    {
	      bufsize_time = value;
	      flag_delay_change = 1;
	      mflag = 1;
	    }
	  if (strcmp(key, "aj") == 0)
	    {
	      param_aj = value;
	      mflag = 1;
	    }
	}
    }
  if (mflag) CalculateConstraints();
  if (flag_delay_change) 
    {
      forw.tt = AFGetTime(forw.tac) + forw.delay_in_samples;
      if (flag_full_duplex) 
	back.tt = AFGetTime(back.tac) + back.delay_in_samples;
      flag_delay_change = 0;
    }
  fclose(f);
  flag_update = 0;
}

/* initialize all fields except faud, taud, fac, tac, fdevice, and tdevice */
void InitStateRec(char *program, StateRec *dir)
{
  /* fac, tac: already initialized */
  /* ft, tt, factt, tactt: initialized later */
  /* fdevice, tdevice: already initialized */
  /* attributes: already initialized */

  /* verify that source and destination devices have the same nominal
     sample rate */
  AoD(dir->fac->device->recSampleFreq == dir->tac->device->playSampleFreq,
      "%s: cannot change sample rate from %d to %d\n", 
      program,
      dir->fac->device->recSampleFreq, 
      dir->tac->device->playSampleFreq);

  /* verify that source and destination devices have the same number
     of channels */
  AoD(dir->fac->device->recNchannels == dir->tac->device->playNchannels,
      "%s: in and out devices have different numbers of channels\n", 
      program);
  
  /* if not set by the -e flag, set encoding type from the from device */
  if (dir->attributes.type == UNKNOWN_ENCODETYPE)
    dir->attributes.type = dir->faud->devices[dir->fdevice].recBufType;

  dir->type = dir->attributes.type;
  
  dir->sample_rate = dir->fac->device->recSampleFreq;
  
  dir->bytes_per_unit = BytesPerUnit(dir->type)
    * dir->fac->device->recNchannels;

  dir->samps_per_unit = SampsPerUnit(dir->type);

  /* Allocate the maximum buffersize, so we don't have to reallocate
     if the size is changed at runtime */

  dir->bufsize_samples = dir->sample_rate / 2;  /* 1/2 second */
  dir->bufsize_units = dir->bufsize_samples / dir->samps_per_unit;
  dir->bufsize_bytes = dir->bufsize_units * dir->bytes_per_unit;

  AoD((dir->buffer = (void *) malloc(dir->bufsize_bytes)) != NULL,
       "Couldn't allocate buffer\n");

  dir->delay_in_samples = -1;
  dir->delay_upper_limit = -1;
  dir->delay_lower_limit = -1;
  dir->nextslip = 0;

}


main(int argc, char **argv)
{
  char *faf = (char *) NULL;
  char *taf = (char *) NULL;
  AC fac;
  AC tac;			/* from and to audio contexts */
  
  char *filename=(char *)NULL;        /* param file name */
  int i;
  
  forw.attributes.preempt = Mix;
  forw.attributes.start_timeout = 0;
  forw.attributes.end_silence = 0;
  forw.attributes.play_gain = 0;
  forw.attributes.rec_gain =  0;
  forw.attributes.type = UNKNOWN_ENCODETYPE;
  forw.fdevice = -1;
  forw.tdevice = -1;

  /* Parse the command line */
  for ( i = 1; i < argc; i++ ) {
    if ( strcmp( argv[i], "-id" ) == 0) 
      {
	AoD(++i < argc, "%s: -id missing device\n", argv[0]);
	forw.fdevice = atoi(argv[i]);
      }
    else if ( strcmp( argv[i], "-od" ) == 0)
      {
	AoD(++i < argc, "%s: -od missing device\n", argv[0]);
	forw.tdevice = atoi(argv[i]);
      }
    else if ( strcmp( argv[i], "-ia" ) == 0)
      {
	AoD(++i < argc, "%s: -ia missing server\n", argv[0]);
	faf = argv[i];
      }
    else if ( strcmp( argv[i], "-oa" ) == 0)
      {
	AoD(++i < argc, "%s: -oa missing server\n", argv[0]);
	taf = argv[i];
      } 
    else if ( strcmp( argv[i], "-delay" ) == 0)
      {
	AoD((++i < argc) &&
	    (sscanf(argv[i], "%f", &param_delay) == 1),
	    "%s: missing delay\n", argv[0]);
      }
    else if ( strcmp( argv[i], "-aj" ) == 0)
      {
	AoD((++i < argc) &&
	    (sscanf(argv[i], "%f", &param_aj) == 1),
	    "%s: missing anti-jitter value\n", argv[0]);
      }
    else if ( strcmp( argv[i], "-buffering") == 0)
      {
	AoD((++i < argc) &&
	    (sscanf(argv[i], "%f", &bufsize_time) == 1),
	    "%s: missing buffering value\n", argv[0]);
      } 
    else if ( strcmp( argv[i], "-g") == 0)
      {
	AoD((++i < argc) &&
	    (sscanf(argv[i], "%f", &param_gain) == 1),
	    "%s: missing gain value\n", argv[0]);
	forw.attributes.play_gain = param_gain;
      }
    else if ( strcmp( argv[i], "-log") == 0)
      {
	flag_log = 1;
      }
    else if ( strcmp( argv[i], "-e") == 0) 
      {
	AoD(++i < argc, "%s: missing encoding type\n", argv[0]);
	forw.attributes.type = AFFindEncodeType(argv[i]);
	if (forw.attributes.type == UNKNOWN_ENCODETYPE) {
	  fprintf(stderr,"%s: unknown encoding type\n",
		  argv[i]);
	  AFPrintKnownEncodeTypes();
	  exit(1);
	}
      } 
    else if ( strcmp( argv[i], "-full_duplex" ) == 0)
      {
	flag_full_duplex = 1;
      }
    else if ( strcmp( argv[i], "-f" ) == 0)
      {
	AoD(++i < argc, "%s: missing parameter filename\n", argv[0]);
	filename = argv[i];
      }
    else
      { 
	AoD(0, "%s: unknown command line option %s\n", argv[0], argv[i]);
      }
  }
  
  /* attempt to open either filename or fullname (try filename first) */
  if (filename != NULL)
    {
      FILE *paramfile;
      AoD((paramfile = fopen(filename, "r")) != NULL,
	  "%s: Can't open file %s\n", argv[0], filename);
      fclose(paramfile);
      signal(SIGUSR1, handle_int);
    }
  
  /* open connections to the two audio servers */
  forw.faud = AFOpenAudioConn(faf);
  if (faf == NULL) faf = "<null>";
  AoD(forw.faud != NULL, "%s: can't open connection to %s.\n", argv[0], faf);

  forw.taud = AFOpenAudioConn(taf);
  if (taf == NULL) taf = "<null>";
  AoD(forw.taud != NULL, "%s: can't open connection to %s.\n", argv[0], taf);

  /* select devices for the from and two devices */
  if (forw.fdevice < 0) forw.fdevice = FindDefaultDevice(forw.faud);
  if (forw.tdevice < 0) forw.tdevice = FindDefaultDevice(forw.taud);

  if (forw.attributes.type == UNKNOWN_ENCODETYPE) 
    forw.attributes.type = forw.faud->devices[forw.fdevice].recBufType;

  /* create AC's for the from and to devices */
  forw.fac = AFCreateAC(forw.faud, forw.fdevice, 
			ACRecordGain | ACPlayGain | ACEncodingType, 
			&forw.attributes);

  forw.tac = AFCreateAC(forw.taud, forw.tdevice, 
			ACRecordGain | ACPlayGain | ACEncodingType, 
			&forw.attributes);

  InitStateRec(argv[0], &forw);
  if (flag_full_duplex)
    {
      back.faud = forw.taud;
      back.taud = forw.faud;
      back.fac = forw.tac;
      back.tac = forw.fac;
      back.fdevice = forw.tdevice;
      back.tdevice = forw.fdevice;
      back.type = forw.type;
      InitStateRec(argv[0], &back);
    }
  CalculateConstraints();

  /* get starting times for the two servers */
  
  forw.ft = AFGetTime(forw.fac);
  back.ft = AFGetTime(forw.tac);
  forw.tt = back.ft + forw.bufsize_samples + forw.delay_in_samples;
  back.tt = forw.ft + forw.bufsize_samples + forw.delay_in_samples;

  for (;;) {
    if (flag_update) getupdate(filename);
    OneWay(&forw);
    if (flag_full_duplex) OneWay(&back);
  }
}

OneWay(StateRec *dir)
{
  int i;
  dir->factt = AFRecordSamples(dir->fac, dir->ft, 
			       dir->bufsize_bytes,
			       dir->buffer, ABlock);
  /* If we are not slipping, then factt should be approximately
     ft + bufsize_samples */
  dir->slip = dir->factt - (dir->ft + dir->bufsize_samples);
  if (flag_log && ((dir->slip < -400) || (dir->slip > 400)))
    printf("record slip %d\n", dir->slip);
  dir->tactt = AFPlaySamples(dir->tac, dir->tt,
			     dir->bufsize_bytes, 
			     dir->buffer);
  /* If we are not slipping, then tactt should be approximately
     tt - delay_in_samples */
  
  dir->sliphist[dir->nextslip++] = dir->tt - dir->tactt;
  if (dir->nextslip >= SLIPHIST) dir->nextslip = 0;
  dir->slip = 0;
  for (i = 0; i < SLIPHIST; i += 1) dir->slip += dir->sliphist[i];
  dir->slip /= SLIPHIST;
  if ((dir->slip < dir->delay_lower_limit) || 
      (dir->slip >= dir->delay_upper_limit))
    {
      dir->tt = dir->tactt + dir->delay_in_samples;
      if (flag_log) 
	fprintf(stdout, "resync from %d to %d\n", dir->factt, dir->tactt);
    }
  dir->ft += dir->bufsize_samples;
  dir->tt += dir->bufsize_samples;
}

