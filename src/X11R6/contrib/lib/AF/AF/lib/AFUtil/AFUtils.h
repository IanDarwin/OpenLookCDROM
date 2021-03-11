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

#ifndef	AFUTILS_H
#define	AFUTILS_H

#include "audio.h"
#include "AFClientTypes.h"

#ifdef __cplusplus                    /* do not leave open across includes */
extern "C" {                                  /* for C++ V2.0 */
#endif

/************************************************************/
/* Tables for doing conversions and gain adjustments        */
/************************************************************/

extern AF_CARD8 AF_comp_u[];   /* -8192 to +8191 index, 14 bit index. */
extern AF_CARD8 AF_comp_a[];   /* -8192 to +8191 index, 14 bit index. */
  /* AF_comp_u and AF_comp_a are indexed by taking the low 14 bits
   *     of a signed value between -8192 and +8191,
   * from a 16 bit integer betweeb -32768 and 32767
   *   AF_comp_u[(AF_INT16 >> 2) & 0x3fff]
   * from a full-range 32 bit integer
   *   AF_comp_u[(AF_INT32 >> 18) & 0x3fff]
   */

extern AF_INT16 AF_exp_u[];	/* Expands ulaw to +/- 8191, 8 bit index. */
extern AF_INT16 AF_exp_a[];	/* Expands alaw to +/- 8191, 8 bit index. */

extern AF_INT16 AF_cvt_u2s[];	/* expands ulaw to +/- 32767, 8 bit index */
extern AF_INT16 AF_cvt_a2s[];	/* expands alaw to +/- 32767, 8 bit index */

extern float AF_cvt_u2f[];	/* expands ulaw to +/- 1.0, 8 bit index */
extern float AF_cvt_a2f[];	/* expands alaw to +/- 1.0, 8 bit index */

extern AF_CARD8 AF_cvt_u2a[];	/* Convert ulaw to alaw.	*/
extern AF_CARD8 AF_cvt_a2u[];	/* Convert alaw to ulaw.	*/

extern float AF_power_uf[];	/* power of ulaw sample, scaled to [0..1.0] */
extern float AF_power_af[];	/* power of alaw sample, scaled to [0..1.0] */

extern AF_CARD8 AF_mix_u[];        /* 16 bit index (2 bytes of
					   ulaw) result is mix */
extern AF_CARD8 AF_mix_a[];        /* 16 bit index (2 bytes of
					   alaw) result is mix */
/* There is an array of 256 byte tables, which offer gain adjustment
 * in 1 dB steps between AF_gain_min_u and AF_gain_max_u
 */
extern int AF_gain_min_u;
extern int AF_gain_max_u;
extern AF_CARD8 *AF_gain_table_u[]; /* array of ulaw gain tables */
  /* To get a table:  table = AF_gain_table[gain-AF_gain_min] */

extern int AF_gain_min_a;
extern int AF_gain_max_a;
extern AF_CARD8 *AF_gain_table_a[]; /* array of ulaw gain tables */
  /* To get a table:  table = AF_gain_table[gain-AF_gain_min] */

extern AF_INT16 AF_sine_int[];      /* 1024 location table with
				         one cycle of a sine wave,
					 scaled to +/- 32767 */

extern float AF_sine_float[];    /* 1024 floats, scaled to +/- 1.0 */

/* audio.h contains the master enumeration of AF datatypes.  This
 * section contains information about those types.
 */

struct AFSampleTypes {
  unsigned int bytes_per_unit;
  unsigned int samps_per_unit;
  char *name;
};

extern struct AFSampleTypes AF_sample_sizes[];     
/* array indexed by the AEncodeType in audio.h */

/* field extraction macros for SampleSizes */
#define BytesPerUnit(type) AF_sample_sizes[(type)].bytes_per_unit
#define SampsPerUnit(type) AF_sample_sizes[(type)].samps_per_unit

/*************************************************/
/*  Useful subroutines                           */
/*************************************************/

extern void AFMakeGainTableU(AF_CARD8 *table, double dBgain);
/* construct a 256 byte u-law translation table with the given gain */

extern void AFMakeGainTableA(AF_CARD8 *table, double dBgain);
/* construct a 256 byte a-law translation table with the given gain */

extern void AoD();
/* ANSI prototype would be extern void AoD(int bool, char *errmsg, ...);
   but we can't use it because not many compilers understand the
   full standard "..." format for variable argument lists */
/* This procedure is "Assert Or Die".  If the boolean is true,
   then AoD returns, otherwise it interprets the rest of its
   arguments as a format string and arguments for fprintf(stderr)
   and then it calls exit(1); */

extern int AFTonePair(double f1, double dBgain1, double f2, double dBgain2,
	    double sampleRate, AEncodeType encodeType,
	    int gainramp, AF_CARD8 *buffer, int length);
/* generate a tone pair into a buffer.  The
   two frequencies are f1 and f2.  Each tone will be dBgain
   relative the "digital milliwatt", which in turn is 3.16 dB
   down from digital clipping level.  The tones will ramp up to
   full volume in gainramp samples and ramp down at the end.
   The procedure will return 0 if the encodeType is not
   supported, otherwise it will return 1. */

extern double AFSingleTone(double freq, double peak, double phase, 
		    float *buffer, int length);
/* generate a floating point tone into a buffer, with a given peak value.
   The initial phase is given and the final phase is returned.  
   (With the phase represented as a number in [0..1024). The value is added
   into the buffer.   The "freq" parameter should be frequency divided
   by samples per second. - necessarily in the range 0 to 0.5 to avoid
   aliasing. */

void AFSilence(AEncodeType encoding, void *buffer, int nUnits);
/* fill a block with silence of a type appropriate to the
   specified encoding. length is in encoding units */

extern double AFdBtoLin(double dB);
/* convert from decibels to a linear value suitable for multiplying
   waveform values. out = 10**(dB / 20.0) */

extern double AFLintodB(double lin);
/* convert a linear value to decibels out =  20.0 log(lin) */

/* procedures to help with handling encoding types */

/* Translate a string into an encoding type. Returns UNKNOWN_ENCODETYPE
 * if this is not possible
 */
extern AEncodeType AFFindEncodeType(char *type);

/* List known encoding types on standard errror */
extern void AFPrintKnownEncodeTypes();

/*
 * Compute power for signal in buffer.  Currently supports only
 * scalar encoding types
 */  
extern double AFPower(AF_CARD8 *buf, int nUnits, AEncodeType fmt);

#ifdef __cplusplus                    /* Close the C++ V2.0 brace. */
}
#endif

#endif

