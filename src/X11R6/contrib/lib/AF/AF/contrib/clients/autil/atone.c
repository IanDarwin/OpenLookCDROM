/* aftone.c
 * put a tone on stdout in ulaw format
 */
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

int main(argc,argv)
int argc;
char *argv[];
{
  int i, blocks;
  int blocksize = 1000;
  unsigned char *buf;
  float *fbuf;
  double freq, time, phase, dBgain, peak, maxsin;
  time = -1.0;
  freq = 1000.0;
  dBgain = -10.0;
  phase = 0.0;
  maxsin = 8031.0;
  /* Parse the command line. 					*/
  for ( i = 1; i < argc; i++ ) 
    {
      if ( strcmp( argv[i], "-freq" ) == 0)
	{
	  AoD((++i < argc) &&
	      (sscanf(argv[i],"%Lf",&freq) == 1),
	      "%s: -freq missing value\n", argv[0]);
	} 
      else if ( strcmp( argv[i], "-power" ) == 0)
	{
	  AoD((++i < argc) &&
	      (sscanf(argv[i],"%Lf",&dBgain) == 1),
	      "%s: -power missing value\n", argv[0]);
	} 
      else if ( strcmp( argv[i], "-phase" ) == 0)
	{
	  AoD((++i < argc) &&
	      (sscanf(argv[i],"%Lf",&phase) == 1),
	      "%s: -phase missing value\n", argv[0]);
	  LIMIT(0, phase, 1024);
	} 
      else if ( strcmp( argv[i], "-peak" ) == 0)
	{
	  AoD((++i < argc) &&
	      (sscanf(argv[i],"%Lf",&maxsin) == 1),
	      "%s: -peak missing value\n", argv[0]);
	} 
      else if ( strcmp( argv[i], "-time" ) == 0)
	{
	  AoD((++i < argc) &&
	      (sscanf(argv[i],"%Lf",&time) == 1),
	      "%s: -freq missing value\n", argv[0]);
	} 
      else
	{
	  AoD(0, "%s: unknown command line item %s\n", argv[i]);
	}
    }
  buf = (unsigned char *) malloc(blocksize);
  fbuf = (float *) malloc(blocksize * sizeof(float));
  peak = maxsin * pow(10.0, (dBgain - 3.16) * 0.05);
  freq = freq / 8000.0;
  if (time < 0.0) blocks = -1;
  else blocks = (time * 8000.0) / blocksize;
  for (;;)
  {
    if (blocks == 0) break;
    if (blocks > 0) blocks -= 1;
    bzero(fbuf, sizeof(float) * blocksize);
    phase = AFSingleTone(freq, peak, phase, fbuf, blocksize);
    for (i = 0; i < blocksize; i += 1) 
      buf[i] = AF_comp_u[((int) fbuf[i]) & 0x3fff];
    fwrite(buf, sizeof(unsigned char), blocksize, stdout);
  }
  fflush(stdout);
  return(0);
}
