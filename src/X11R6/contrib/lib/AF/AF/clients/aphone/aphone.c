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
#include <string.h>
#include <stdlib.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>
#include <stdio.h>

/* aphone.c
   L. Stewart
   Feb 2, 1993
 */

/* dialit [-on duration] [-off duration] [-db db] number */


main (int argc, char **argv)
{
  char *number;
  int i, status;
  int device = -1;
  char *af = (char *)NULL;
  AFAudioConn *aud;
  AC ac;

  /* Parse the command line */
  for ( i = 1; i < argc; i++ ) {
	if ( strcmp( argv[i], "-af" ) == 0)
	  {
            AoD(++i < argc, "%s: -af missing server\n", argv[0]);
            af = argv[i];
	  }
	else if ( strcmp( argv[i], "-d" ) == 0)
	  {
	    AoD((++i < argc) &&
		(sscanf(argv[i],"%d",&device) == 1),
		"%s: missing device \n", argv[0]);
	  }
	else 
	  {
	    number = argv[i];
	  }
      }
  
  aud = AFOpenAudioConn(af);
  if (af == (char *)NULL) af = "<null>";
  AoD(aud != NULL, "%s, can't open connection to %s\n", argv[0], af);
  
  if (device == -1)
    {
      /* set up audio context, find sample size and sample rate */
      ac = AFCreatePhoneAC(aud, 0L, 0L);
      AoD(ac != NULL, "%s: no phone device available\n", argv[0]);
    }
  else
    {
      ac = AFCreateAC(aud, device, 0L , 0L);
      AoD(ac != NULL, "%s: cannot create AC for device %d\n", argv[0], device);
    }

  status = AFDialPhone(ac, number);
  AFFreeAC(ac);
  AFCloseAudioConn(aud);
  AoD(status >= 0, "%s: dialing failed code %d\n", argv[0], status);
  return 0;
}

