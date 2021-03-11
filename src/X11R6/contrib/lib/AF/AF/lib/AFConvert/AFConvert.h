/*static char rcsid[] = "$RCSfile: AFConvert.h,v $, $Revision: 1.1 $, $Date: 1994/02/01 16:46:28 $";*/

/************************************************************
Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, provided 
that the above copyright notice appear in all copies and that both that 
copyright notice and this permission notice appear in supporting 
documentation, and that the names of Digital not be used in 
advertising or publicity pertaining to distribution of the software without 
specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

/************************************************************************
 *
 * FILE NAME		: $RCSfile: AFConvert.h,v $
 * AUTHOR		: $Author: stewart $
 * DATE			: $Date: 1994/02/01 16:46:28 $
 * REVISION		: $Revision: 1.1 $
 * DESCRIPTION		: General purpose audio conversion routines
 *
 * REVISION HISTORY
 *
 * $Log: AFConvert.h,v $
 * Revision 1.1  1994/02/01  16:46:28  stewart
 * Initial revision
 *
 * Revision 1.1  1993/12/10  13:29:54  stewart
 * Initial revision
 */

#ifndef _AFCONVERT_H_
#define _AFCONVERT_H_

#include "audio.h"

/* The AFConvert_Data structure is used to hold the state of
 * a compression or decompression.  If a new client request arrives
 * whose ATime matches next, and the encode type is the same, then
 * the state is valid.  Otherwise not.
 */

typedef struct _AFConvert_Data *foo; /* dummy typedef */

typedef enum {
  ALLOC = 0,   /* allocate and init any data structures */
  RESET = 1,   /* reset any data structures on time jump */
  DEALLOC = 2  /* cleanup and release any data structures */
  } AFConvertCmd;

/* ConvertInitProc is called with one of the above commands */

typedef void (*ConvertInitProc)(struct _AFConvert_Data *cd, AFConvertCmd cmd);
typedef void (*ConvertProc)(struct _AFConvert_Data *cd,
			    void *in, void * out, int units);

typedef struct _AFConvert_Data {
  AEncodeType fromType, toType;
  ATime next;
  int nChannels;          /* 1 == MONO, 2 == interleaved stereo, etc. */
  void *buffer;         /* data holding buffer, 1 sec at uncompressed size */
  ConvertInitProc convertInitProc;
  ConvertProc convertProc;
  void *state;
} AFConvert_Data;

/* array of initialization procs */
extern ConvertInitProc muInit[];
extern ConvertProc muConvert[];

extern ConvertInitProc linInit[];
extern ConvertProc linConvert[];

/* returns 1 if able to fill in the conversion and init procs */
extern int AFConvert_InitConvert(AFConvert_Data *cd);

#endif

