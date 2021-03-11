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
#include <math.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>

#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

unsigned char dmw[8] = { 0x1e, 0x0d, 0x0d, 0x1e, 0x9e, 0x8d, 0x8d, 0x9e };

double refpow()
{
  int i;
  double ref;
  ref = 0.0;
  for (i = 0; i < 8; i += 1) ref += AF_power_uf[dmw[i]];
  ref /= 8.0;
  return (ref);
}

void main(argc,argv)
int argc;
char *argv[];
{
  int i, ccittflag, typeflag, shcount;
  double bpow, ref, samp;
  int blocksize, bytesread;
  unsigned char *buf;
  double minpow;
  short int *shp;
  blocksize = 8000;   /* try one second */
  ccittflag = 0;
  /* Parse the command line. */
  typeflag = MU255;
  for ( i = 1; i < argc; i++ ) 
    {
      if ( strcmp( argv[i], "-bs" ) == 0)
	{
	  AoD((++i < argc) &&
	      (sscanf(argv[i],"%d",&blocksize) == 1),
	      "%s: -bs missing value\n", argv[0]);
	  LIMIT(100, blocksize, 100000);
	} 
      else if ( strcmp( argv[i], "-ccitt" ) == 0)
	{
	  ccittflag = 1;
	} 
      else if ( strcmp( argv[i], "-e" ) == 0)
	{
	  AoD((++i < argc), "%s: -e missing value\n", argv[0]);
	  typeflag = AFFindEncodeType(argv[i]);
	  if (typeflag == UNKNOWN_ENCODETYPE) {
	    AFPrintKnownEncodeTypes();
	    exit(1);
	  }
	} 
      else
	{
	}
    }
  buf = (unsigned char *) malloc(blocksize);
  shp = (short int *) buf;
  if (ccittflag ) ref = refpow();
  else
    {
      /* 3.16 dB below digital clipping */
      ref = 1.0 * 0.707 * pow(10.0, -3.16 / 20.0);
      ref = ref * ref;
    }
  minpow = pow(10.0, -8.0);
  for (;;)
  {
    if (feof(stdin)) break;
    bytesread = fread(buf, sizeof(unsigned char), blocksize, stdin);
    AoD(bytesread >= 0, "fread failed\n");
    if (bytesread == 0) break;
    bpow = 0.0;
    bpow = AFPower(buf, bytesread / BytesPerUnit(typeflag), typeflag);
    if (bpow < minpow) bpow = minpow;
    bpow = 10.0 * log10(bpow/ref);
    fprintf(stdout, "%6.2f dBm\n", bpow);
    if (bytesread < blocksize) break;
  }
}
