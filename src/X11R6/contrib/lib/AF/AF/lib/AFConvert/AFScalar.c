static char     rcsid[] = "$RCSfile: AFScalar.c,v $, $Revision: 1.3 $, $Date: 1994/03/29 07:26:49 $";

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
 * FILE NAME		: $RCSfile: AFScalar.c,v $
 * AUTHOR		: $Author: tml $
 * DATE			: $Date: 1994/03/29 07:26:49 $
 * REVISION		: $Revision: 1.3 $
 * DESCRIPTION		: Convert among scalar audio formats
 *
 * REVISION HISTORY
 *
 * $Log: AFScalar.c,v $
 * Revision 1.3  1994/03/29  07:26:49  tml
 * bug fixes found while doing j300
 *
 * Revision 1.2  1994/02/15  13:56:51  tml
 * eliminate Amd dependency.
 *
 * Revision 1.1  1994/02/01  16:46:28  stewart
 * Initial revision
 *
 * Revision 1.1  1993/12/10  13:29:54  stewart
 * Initial revision
 *
 ************************************************************************/

#include "AFScalar.h"
#include "AFClientTypes.h"
#include "audio.h"
#include "AFUtils.h"

static void
table11(AF_CARD8 *in, AF_CARD8 *out, AF_CARD8 *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[in[i]];
}

static void
table12(AF_CARD8 *in, short int *out, short int *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[in[i]];
}

static void
table14s(AF_CARD8 *in, AF_INT32 *out, AF_INT16 *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[in[i]] << 16;
}

static void
table1ff(AF_CARD8 *in, float *out, float *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[in[i]];
}


static void
table1df(AF_CARD8 *in, double *out, float *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[in[i]];
}

static void
table21(AF_INT16 *in, AF_CARD8 *out, AF_CARD8 *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[(in[i] >> 2) & 0x3fff];
}

static void
table41(AF_INT32 *in, AF_CARD8 *out, AF_CARD8 *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) out[i] = table[(in[i] >> 18) & 0x3fff];
}

static void
tablef1(float *in, AF_CARD8 *out, AF_CARD8 *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) 
    out[i] = table[(((int) (in[i] * 32767.0)) >> 2) & 0x3fff];
}

static void
tabled1(double *in, AF_CARD8 *out, AF_CARD8 *table, int count)
{
  int i;
  for (i = 0; i < count; i += 1) 
    out[i] = table[(((int) (in[i] * 32767.0)) >> 2) & 0x3fff];
}


void AFConvert_InitScalar(AFConvert_Data *cd, AFConvertCmd cmd)
{
  cd->state = NULL;
}

void AFConvert_ConvertScalar(
			   AFConvert_Data *cd,
			   void	        *ucpIn,		/* Input buffer */
			   void	        *ucpOut,	/* Output buffer */
			   int		iCnt		/* Number of units */
			   )
{
  AEncodeType intype = cd->fromType;
  int insize = AF_sample_sizes[intype].bytes_per_unit;
  int i;

  AEncodeType outtype = cd->toType;
  /* multichannel */
  iCnt *= cd->nChannels;

  /* conversions */
  if (intype == outtype)
    bcopy(ucpIn, ucpOut, iCnt * insize);
  else
    switch (intype)
      {
      case MU255:
	switch (outtype)
	  {
	  case ALAW:
	    table11((AF_CARD8 *) ucpIn, (AF_CARD8 *) ucpOut, AF_cvt_u2a, iCnt);
	    break;
	  case LIN16:
	    table12((AF_CARD8 *) ucpIn, (AF_INT16 *) ucpOut, AF_cvt_u2s, iCnt);
	    break;
	  case LIN32:
	    table14s((AF_CARD8 *) ucpIn, (AF_INT32 *) ucpOut, AF_cvt_u2s, iCnt);
	    break;
	  case IEEES:
	    table1ff((AF_CARD8 *) ucpIn, (float *) ucpOut, AF_cvt_u2f, iCnt);
	    break;
	  case IEEED:
	    table1df((AF_CARD8 *) ucpIn, (double *) ucpOut, AF_cvt_u2f, iCnt);
	    break;
	  }
	break;
      case ALAW:
	switch (outtype)
	  {
	  case MU255:
	    table11((AF_CARD8 *) ucpIn, (AF_CARD8 *) ucpOut, AF_cvt_a2u, iCnt);
	    break;
	  case LIN16:
	    table12((AF_CARD8 *) ucpIn, (AF_INT16 *) ucpOut, AF_cvt_a2s, iCnt);
	    break;
	  case LIN32:
	    table14s((AF_CARD8 *) ucpIn, (AF_INT32 *) ucpOut, AF_cvt_a2s, iCnt);
	    break;
	  case IEEES:
	    table1ff((AF_CARD8 *) ucpIn, (float *) ucpOut, AF_cvt_a2f, iCnt);
	    break;
	  case IEEED:
	    table1df((AF_CARD8 *) ucpIn, (double *) ucpOut, AF_cvt_a2f, iCnt);
	    break;
	  }
	break;
      case LIN16:
	switch (outtype)
	  {
	  case MU255:
	    table21((AF_INT16 *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_u, iCnt);
	    break;
	  case ALAW:
	    table21((AF_INT16 *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_a, iCnt);
	    break;
	  case LIN32:
	    for (i = 0; i < iCnt; i += 1) 
	      ((AF_INT32 *) ucpOut)[i] = (((AF_INT16 *) ucpIn)[i]) << 16;
	    break;
	  case IEEES:
	    for (i = 0; i < iCnt; i += 1) 
	      ((float *) ucpOut)[i] = 
		((float) ((AF_INT16 *) ucpIn)[i]) * (1.0 / 32768.0);
	    break;
	  case IEEED:
	    for (i = 0; i < iCnt; i += 1) 
	      ((double *) ucpOut)[i] = 
		((double) ((AF_INT16 *) ucpIn)[i]) * (1.0 / 32768.0);
	    break;
	  }
	break;
      case LIN32:
	switch (outtype)
	  {
	  case MU255:
	    table41((AF_INT32 *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_u, iCnt);
	    break;
	  case ALAW:
	    table41((AF_INT32 *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_a, iCnt);
	    break;
	  case LIN16:
	    for (i = 0; i < iCnt; i += 1) 
	      ((AF_INT16 *) ucpOut)[i] = (((AF_INT32 *) ucpIn)[i]) >> 16;
	    break;
	  case IEEES:
	    for (i = 0; i < iCnt; i += 1) 
	      ((float *) ucpOut)[i] = 
		((float) ((AF_INT32 *) ucpIn)[i]) * (1.0 / 2147483648.0);
	    break;
	  case IEEED:
	    for (i = 0; i < iCnt; i += 1) 
	      ((double *) ucpOut)[i] = 
		((double) ((AF_INT32 *) ucpIn)[i]) * (1.0 / 2147483648.0);
	    break;
	  }
	break;
      case IEEES:
	switch (outtype)
	  {
	  case MU255:
	    tablef1((float *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_u, iCnt);
	    break;
	  case ALAW:
	    tablef1((float *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_a, iCnt);
	    break;
	  case LIN16:
	    {
	      for (i = 0; i < iCnt; i += 1) 
		((AF_INT16 *) ucpOut)[i] = 
		  ((((float *) ucpIn)[i]) * 32767.0);
	    }
	    break;
	  case LIN32:
	    {
	      for (i = 0; i < iCnt; i += 1) 
		((AF_INT32 *) ucpOut)[i] = 
		  (AF_INT32) ((((float *) ucpIn)[i]) * 2147483647.0);
	    }
	    break;
	  case IEEED:
	    {
	      for (i = 0; i < iCnt; i += 1) 
		((double *) ucpOut)[i] = ((float *) ucpIn)[i];
	    }
	    break;
	  }
	break;
      case IEEED:
	switch (outtype)
	  {
	  case MU255:
	    tabled1((double *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_u, iCnt);
	    break;
	  case ALAW:
	    tabled1((double *) ucpIn, (AF_CARD8 *) ucpOut, AF_comp_a, iCnt);
	    break;
	  case LIN16:
	    {
	      for (i = 0; i < iCnt; i += 1) 
		((AF_INT16 *) ucpOut)[i] = 
		  (AF_INT16) ((((double *) ucpIn)[i]) * 32767.0);
	    }
	    break;
	  case LIN32:
	    {
	      for (i = 0; i < iCnt; i += 1) 
		((AF_INT32 *) ucpOut)[i] = 
		  (AF_INT32) ((((double *) ucpIn)[i]) * 2147483647.0);
	    }
	    break;
	  case IEEES:
	    {
	      for (i = 0; i < iCnt; i += 1) 
		((float *) ucpOut)[i] = ((double *) ucpIn)[i];
	    }
	    break;
	  }
	break;
      }
}
