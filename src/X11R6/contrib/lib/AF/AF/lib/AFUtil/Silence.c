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

#include "AFUtils.h"
#include <string.h>

/* fill a block with silence of a type appropriate to the
   specified encoding */

static unsigned char gsmsilence[33] = {
  0xd8, 0x20, 0xa2, 0xe1, 0x5a, 0x50, 0x00, 0x49,
  0x24, 0x92, 0x49, 0x24, 0x50, 0x00, 0x49, 0x24,
  0x92, 0x49, 0x24, 0x50, 0x00, 0x49, 0x24, 0x92,
  0x49, 0x24, 0x50, 0x00, 0x49, 0x24, 0x92, 0x49,
  0x24 };

void AFSilence(AEncodeType encoding, void *buffer, int nUnits)
{
  int nBytes = nUnits * BytesPerUnit(encoding);
  if (nUnits <= 0) return;
  switch (encoding)
    {
    case MU255:  
      memset(buffer, 0xff, nBytes);
      break;
    case ALAW: 
      memset(buffer, 0xd5, nBytes);
      break;
    case LIN16:
      memset(buffer, 0, nBytes);
      break;
    case LIN32:
      memset(buffer, 0, nBytes);
      break;
    case IEEES:
      memset(buffer, 0, nBytes);
      break;
    case IEEED:
      memset(buffer, 0, nBytes);
      break;
    case IMA:
      memset(buffer, 0x00, nBytes);
      break;
    case G721:
      memset(buffer, 0xff, nBytes);
      break;
    case CRLADPCM2:
      memset(buffer, 0xcc, nBytes);
      break;
    case CRLADPCM3:
      {
	AF_CARD8 *ucp = (AF_CARD8 *) buffer;
	if (BytesPerUnit(CRLADPCM3) != 3) 
	  AoD(0, "Bad size for CRLADPCM3 unit\n");
	while (nUnits--) { 
	  *ucp++ = 0x38; 
	  *ucp++ = 0x8e; 
	  *ucp++ = 0xe3; 
	}
	break;
      }
    case CRLADPCM4:
      memset(buffer, 0xf0, nBytes);
      break;
    case GSM:
      {
	AF_CARD8 *ucp = buffer;
	if (BytesPerUnit(GSM) != 33) AoD(0, "Bad size for GSM unit\n");
	while (nUnits--) { 
	  bcopy((char *) gsmsilence, (char *) ucp, 33);
	  ucp += 33;
	}
      }
      break;
    default:
      AoD(0, "AFSilence, unimplemented encoding %d\n", encoding);
    }
}
