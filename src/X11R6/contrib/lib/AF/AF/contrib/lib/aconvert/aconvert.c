static char rcsid[] = "$RCSfile: aconvert.c,v $, $Revision: 1.27 $, $Date: 1994/06/03 18:06:09 $";

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

/************************************************************************
 *
 * FILE NAME		: $RCSfile: aconvert.c,v $
 * AUTHOR		: $Author: tml $
 * DATE			: $Date: 1994/06/03 18:06:09 $
 * REVISION		: $Revision: 1.27 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: aconvert.c,v $
 * Revision 1.27  1994/06/03  18:06:09  tml
 * copyright
 *
 * Revision 1.26  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.25  1994/06/01  19:30:00  jg
 * change header file name to meet PC standards
 *
 * Revision 1.24  1993/12/08  23:23:39  wecker
 * Added new AGC code and preload
 *
 * Revision 1.24  1993/12/02  22:07:41  wecker
 * Better AGC algorithm
 *
 * Revision 1.23  1993/11/29  21:36:57  wecker
 * Fixed allocation bug
 *
 * Revision 1.22  1993/10/25  20:33:53  wecker
 * *** empty log message ***
 *
 * Revision 1.21  1993/10/20  18:09:30  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.20  1993/10/20  17:33:52  wecker
 * Added copyrights
 *
 * Revision 1.19  1993/10/19  23:17:29  wecker
 * Changed to Imakefile build
 *
 * Revision 1.18  1993/10/15  17:29:35  wecker
 * Converted to AF_cvt_u2s
 *
 * Revision 1.17  1993/09/01  18:21:50  wecker
 * Fixed init problem with dpFilter
 *
 * Revision 1.16  1993/08/31  20:44:50  wecker
 * Added IMA code
 *
 * Revision 1.15  1993/08/30  17:26:05  wecker
 * Rewrote setjmp code to stack jmpbufs
 *
 * Revision 1.14  1993/08/28  18:28:17  wecker
 * Fixed bugs in ADPCM buffer sizes and 3bit unit counting
 *
 * Revision 1.13  1993/08/28  16:17:51  wecker
 * Added TONE type and split tones/sweep. Fixed ASCII files
 *
 * Revision 1.12  1993/08/27  22:44:27  wecker
 * Fixed problems with filters and multi-channels
 *
 * Revision 1.11  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.10  1993/08/23  22:02:20  wecker
 * Fully tested version of Rasta-PLP
 *
 * Revision 1.9  1993/08/23  20:45:12  wecker
 * Added Rasta-PLP code
 *
 * Revision 1.8  1993/08/22  18:00:34  wecker
 * "Rewrote
 *
 * Revision 1.7  1993/08/21  20:29:43  wecker
 * Split aconvert into aconvert.c and init.c
 *
 * Revision 1.6  1993/08/21  20:07:50  wecker
 * Added SPHERE file format
 *
 * Revision 1.5  1993/08/21  15:43:54  wecker
 * better filter code
 *
 * Revision 1.4  1993/08/20  19:16:08  wecker
 * Something is wrong with filterSize... this is a checkpoint
 *
 * Revision 1.2  1993/08/10  22:45:56  wecker
 * Full callable version
 *
 * Revision 1.1  1993/08/09  22:29:46  wecker
 * Initial revision
 *
 * Revision 1.11  1993/06/25  21:24:19  wecker
 * Added STEREO on INPUT
 *
 * Revision 1.10  1993/06/25  20:44:43  wecker
 * Added STEREO, LEFT and RIGHT support
 *
 * Revision 1.9  1993/06/25  19:15:54  wecker
 * New filter code
 *
 * Revision 1.8  1993/06/25  17:26:00  wecker
 * ADPCM code now works
 *
 * Revision 1.7  1993/06/24  20:53:14  wecker
 * Complete re-write with variable rate changing
 *
 * Revision 1.6  1993/06/19  16:56:30  wecker
 * patched some problems.
 *
 * Revision 1.5  1993/06/18  21:43:06  wecker
 * add -h and -l switches
 *
 * Revision 1.4  1993/06/18  20:50:52  wecker
 * Full Sinc/Kaiser filtering
 *
 * Revision 1.3  1993/06/17  20:25:05  wecker
 * *** empty log message ***
 *
 * Revision 1.2  1993/06/17  15:55:44  wecker
 * Added -m and -d
 *
 * Revision 1.1  1993/06/17  15:34:19  wecker
 * Initial revision
 *
 * Revision 1.1  1993/06/16  19:15:14  wecker
 * Initial revision
 *
 ************************************************************************/

#include "aconvP.h"

Jmp* tpAConvertJmpBuf = NULL;

static char	caErrMsg[256]  = "";

void AConvertUsage(char* va_alist,...)
{
  char*		cpFmt;
  va_list	ap;

  va_start(ap);
  cpFmt = va_arg(ap,char*);
  vsprintf(caErrMsg,cpFmt,ap);
  if (tpAConvertJmpBuf) longjmp(tpAConvertJmpBuf->tCurr,1);
  fprintf(stderr,"ERROR: %s, (INTERNAL ERROR: EXITING)\n",caErrMsg);
  exit(-1);
}

char* AConvertError()			/* Return message from last error */
{
  return caErrMsg;
}

#define	SINC(n)	(((n) == 0.0) ? 1.0 : (sin((n) * M_PI) / ((n) * M_PI)))
#define BETA	2.120

static double Bessel(double dX)			/* compute 0-order Bessel */
{
# define BesselEPSILON 1E-21			/* Max error acceptable */

  double 	dSum, dU, dHalfx, dTemp;
  int 		iN;

  dSum = dU = iN = 1;
  dHalfx = dX/2.0;
  do {
    dTemp = dHalfx/(double)iN;
    iN += 1;
    dTemp *= dTemp;
    dU *= dTemp;
    dSum += dU;
  } while (dU >= BesselEPSILON*dSum);
  return(dSum);
}

/* Create a low pass filter with WIDTH entries */
static void CreateFilter(Ctxt* tpC)
{
  int		i,j;
  int		iWidth = tpC->iFilterSize;
  double	dIBeta = 1.0 / Bessel(BETA);

  /* Allocate the filters */
  tpC->dpFilter = (double*)calloc(sizeof(double),iWidth*NZ);
  if (!tpC->dpFilter) AConvertUsage("Can't allocate filter array!");

  /* Fill in the filter table */
  for (i=0; i<iWidth*NZ; i++)
  {
    double	dTmp	= (double)i / (double)(iWidth*NZ);
    double	dKaiser	= Bessel(BETA*sqrt(1.0-dTmp*dTmp)) * dIBeta;
    double	dVal	= ((double)i/(double)NZ);
      
    tpC->dpFilter[i]      = SINC(dVal) * dKaiser;
  }
  
#if DEBUG > 1
  for (i=0; i<iWidth*NZ; i += NZ/4)
      fprintf(stderr,"FILTER[%4d] = %11.7lf\n",i,tpC->dpFilter[i]);
#endif
}

static void ExpandBuf(
  Ctxt*		tpC,
  int 		iCnt,
  u_char* 	ucpBuf,
  short* 	spBuf)
{
  switch (tpC->tInp.eType)
  {
  case ULAW8:
    while (iCnt--) *spBuf++ = (short)AF_cvt_u2s[*ucpBuf++];
    break;
  case ALAW8:
    while (iCnt--) *spBuf++ = (short)AF_cvt_a2s[*ucpBuf++];
    break;
  default:
    break;
  }
}

static void CompressBuf(
  Ctxt* 	tpC,
  int 		iCnt,
  short* 	spBuf,
  u_char* 	ucpBuf)
{
  while (iCnt--)
  {
    int	val = *spBuf++ >> 2;

    if (val > 8191)
    {
      val = 8191;
      tpC->iTruncated++;
    }
    else if (val < -8191)
    {
      val = -8191;
      tpC->iTruncated++;
    }
    
    switch (tpC->tOut.eType)
    {
    case ULAW8:
      *ucpBuf++ = AF_comp_u[val & 0x3FFF];
      break;
    case ALAW8:
      *ucpBuf++ = AF_comp_a[val & 0x3FFF];
      break;
    default:
      AConvertUsage("Unknown conversion type");
      break;
    }
  }
}

static void WhiteNoise(int iCnt,short* spBuf)
{
  while (iCnt--)
  {
    short sNum	= (short)random();

    *spBuf++ = sNum >> 2;
  }
}

static void FlipBuf(int iCnt,short* spBuf)
{
# define swap_half(a) (((a & 0xff) << 8) | ((unsigned short)(a) >> 8))

  while (iCnt--)
    *spBuf++ = swap_half(*spBuf);
}

static void SetVolume(
  Ctxt*		tpC,
  int		iCnt,
  short* 	spBuf)
{
  double dVol = tpC->dGain;

  while (iCnt--)
  {
    double	dVal = (double)*spBuf;

    dVal *= dVol;
    if (dVal >  32767.0) {
      dVal =  32767.0;
      tpC->iTruncated++;
    }
    if (dVal < -32767.0) {
      dVal = -32767.0;
      tpC->iTruncated++;
    }
    *spBuf++ = (short)dVal;
  }
}

static void DoAGC(
  Ctxt*		tpC,
  int 		iCnt,
  short* 	spBuf)
{
  int		i		= iCnt;
  short*	spVal		= spBuf;
  int		iMaxVal		= 0;
  static int	iLastMax	= 2000;

  while (i--)
  {
    short	sVal = *spVal++;

    sVal = ABS(sVal);
    if (sVal > iMaxVal) iMaxVal = sVal;
  }

  /* New global maximum */
  if (iMaxVal > tpC->sMaxVal) tpC->sMaxVal = iMaxVal;

  /* Time to pick a new multiplier? */
  if (iMaxVal > iLastMax) {
    iLastMax 	  = iMaxVal;
    tpC->dAGCval = 32767.0 / (double)iLastMax;
  }
  else if (iMaxVal > 500 && iMaxVal < (iLastMax >> 1)) {
    iLastMax	 *= 3;
    iLastMax 	 += iMaxVal;
    iLastMax	>>= 2;
    tpC->dAGCval = 32767.0 / (double)iLastMax;
  }

  while (iCnt--) {
    double dVal = tpC->dAGCval * (double)*spBuf;

    if (dVal < -32768.0) {
      tpC->iTruncated++;
      *spBuf++ = -32768;
    }
    else if (dVal > 32767.0) {
      tpC->iTruncated++;
      *spBuf++ = 32767;
    }
    else *spBuf++ = (short)dVal;
  }
}

static int VoxBuf(
  Ctxt*		tpC,
  int 		iCnt,
  short* 	spBuf)
{
  int		iCount	 = iCnt;
  int		iGoodCnt = 0;
  
  while (iCnt--)
  {
    if (*spBuf > 100 || *spBuf < -100) iGoodCnt++;
    spBuf++;
  }

  if (iGoodCnt > 10)
  {
    tpC->iSilCnt = 0;
    return iCount;
  }
  if (tpC->iSilCnt++ < 1) return iCount;

  return 0;
}

static void ShowStats(
  Ctxt*		tpC,
  int 		iCnt,
  short* 	spBuf)
{
  if (iCnt && spBuf)
  {
    int iLoop	= iCnt;
    int	iSum	= 0;

    while (iLoop--)
    {
      int	iVal = *spBuf++;

      iVal	= ABS(iVal);
      iSum     += iVal;
      if (iVal > tpC->iMaxVal) tpC->iMaxVal = iVal;
    }
    tpC->iAvgVal = (tpC->iAvgVal + (iSum / iCnt)) >> 1;
  }
  else
  {
    fprintf(stderr,"  Total   Input  samples: %7d (x %2d bits)\n",
      tpC->iInpSamps,tpC->tInp.iBits);
    fprintf(stderr,"  Total   Output samples: %7d (x %2d bits)\n",
      tpC->iOutSamps,tpC->tOut.iBits);
    fprintf(stderr,"  Maximum Output   (abs): %7d\n",tpC->iMaxVal);
    fprintf(stderr,"  Average Output   (abs): %7d\n",tpC->iAvgVal);
    if (tpC->iTruncated)
      fprintf(stderr,"  Num truncated         : %7d\n",tpC->iTruncated);
    if (tpC->sMaxVal != 1)
      fprintf(stderr,"  AGC multiplier        : %7.3f\n",tpC->dAGCval);
  }
}

static void SweepFreq(
  Ctxt*		tpC,
  int 		iCnt,
  short* 	spBuf)
{
  while (iCnt--)
  {
    int		i;
    short	sVal = (short)(sin(tpC->dIdx) * 8191.0 + 0.5);

    *spBuf++ = sVal;
    tpC->dIdx	+= tpC->dInc;
    while (tpC->dIdx  < 0.0)        tpC->dIdx += 2.0 * M_PI;
    while (tpC->dIdx >= 2.0 * M_PI) tpC->dIdx -= 2.0 * M_PI;
    if (tpC->dAdd != 0.0) {
      tpC->dInc	+= tpC->dAdd;
      if (tpC->dInc <= 0.1 || tpC->dInc >= M_PI) tpC->dAdd = -tpC->dAdd;
    }
  }
}

static int RateChange(				/* return number of dst samps*/
  Ctxt*		tpC,
  int 	 	iCount,				/* number in source buffer */
  short* 	spSrc,				/* source buffer */
  short* 	spDst)				/* destination buffer */
{
  double*		dpBuf;
  short*		spBuf;
  int			iCnt   		= 0;
  int			i,j;
  int			iL		= NZ;
  Bool			bLower		= (tpC->dRho < 1.0) ? bTRUE : bFALSE;

  /* Take care of initialization */
  if (!tpC->dpDbl)
  {
    tpC->dpDbl = (double*)malloc((iCount+2*tpC->iFilterSize)*sizeof(double));
    if (!tpC->dpDbl) AConvertUsage("Can't allocate double data");
    for (i=0; i<2*tpC->iFilterSize; i++) tpC->dpDbl[i] = (double)spSrc[0];
    tpC->iP		= 0;
  }

  /* First copy in source information */
  dpBuf 	= &tpC->dpDbl[2*tpC->iFilterSize];
  spBuf 	= spSrc;
  for (i=0; i<iCount; i++) *dpBuf++ = (double)*spBuf++;

  /* Now compute the interpolated/filtered points */
  spBuf		 = spDst;
  dpBuf		 = &tpC->dpDbl[tpC->iFilterSize];

  /*@@@ if (bLower) iL = (int)((double)iL * tpC->dRho); @@@*/

  /* Handle special case of skipping over a sample at end of buffer */
  i = 0;
  while (tpC->iP >= NZ && i < iCount) {
    tpC->iP	-= NZ;
    ++i;
    dpBuf++;
  }

  while (i<iCount) {
    double	dVal	= 0.0;
    int		iPos;

    iPos 		= tpC->iP;
    /*@@@ if (bLower) iPos    = (int)((double)iPos * tpC->dRho); @@@*/

    /* Now add up backward filter terms */
    for (j=0; j<tpC->iFilterSize; j++) {

#if DEBUG > 0
      fprintf(stderr,
	"dVal=%10.3lf += dpBuf[%2d]=%10.3lf * filt[%4d]=%10.3lf\n",
	dVal,-j,dpBuf[-j],iPos,tpC->dpFilter[iPos]);
#endif

      dVal	+= dpBuf[-j] * tpC->dpFilter[iPos];
      iPos	+= iL;
    }

    iPos 		  = NZ - tpC->iP;
    /*@@@ if (bLower) iPos    = (int)((double)iPos * tpC->dRho); @@@*/

    /* Now add up forward filter terms */
    for (j=1; j<=tpC->iFilterSize; j++) {

#if DEBUG > 0
      fprintf(stderr,
	"dVal=%10.3lf += dpBuf[%2d]=%10.3lf * filt[%4d]=%10.3lf\n",
	dVal,j,dpBuf[j],iPos,tpC->dpFilter[iPos]);
#endif

      dVal	+= dpBuf[j] * tpC->dpFilter[iPos];
      iPos	+= iL;
    }

    /* Fill in the return buffer */
    if (dVal > 32767.0) {
      dVal = 32767.0;
      tpC->iTruncated++;
    } else if (dVal < -32768.0) {
      dVal = -32768.0;
      tpC->iTruncated++;
    }
    *spBuf++	 = (short)dVal;
    iCnt++;

#if DEBUG > 0
    fprintf(stderr,"====== %4d: sVal = %6d (dVal = %10.3lf)\n",
      i,spBuf[-1],dVal);
#endif

    /* Figure out where to jump for the next sample */
    tpC->iP	+= tpC->iIncr;
    while (tpC->iP >= NZ && i < iCount) {
      tpC->iP	-= NZ;
      ++i;
      dpBuf++;
    }
  }

  /* Copy down unused data for next time */
  dpBuf -= tpC->iFilterSize;
  for (i=0; i<tpC->iFilterSize*2; i++) tpC->dpDbl[i] = *dpBuf++;

  return iCnt;
}

# define	FLIPBUFS	tpBuf3=tpBuf2;tpBuf2=tpBuf1;tpBuf1=tpBuf3

static int	iBufLen		= 0;
static int	iMult		= 1;
static int	iUnitSize	= 1;
static Buf*	tpBuf1		= NULL;
static Buf*	tpBuf2		= NULL;
static Buf*	tpBuf3;

int AConvert0(Ctxt* tpC,int iChan)
{
  int		iTmp;
  double	dTmp;

  INITUSAGE(-1);

  /* Are we already initialized? */
  if (tpC->bFirstTime == bFALSE) RETURN(0);

  /* Fill in initial values */
  tpC->bFirstTime	= bFALSE;
  tpC->dAGCval		= 1.0;
  tpC->sMaxVal		= 1;
  iMult			= 1;
  iUnitSize		= 1;
  tpC->iSilCnt		= 0;
  dTmp			= pow(2.0,1/12.0);
  iTmp			= iChan + (8 - tpC->tInp.iSrcChans) / 2;
  switch (iTmp) {
    case 0:	dTmp = pow(dTmp, 0.0);	break;
    case 1:	dTmp = pow(dTmp, 4.0);	break;
    case 2:	dTmp = pow(dTmp, 7.0);	break;
    case 3:	dTmp = pow(dTmp,12.0);	break;
    case 4:	dTmp = pow(dTmp,16.0);	break;
    case 5:	dTmp = pow(dTmp,19.0);	break;
    case 6:	dTmp = pow(dTmp,24.0);	break;
    case 7:	dTmp = pow(dTmp,28.0);	break;
  }
  dTmp			/= 6.0;
  tpC->dInc 		 = dTmp * M_PI;
  if (tpC->tInp.eType == TONE)	tpC->dAdd = 0.0;
  else				tpC->dAdd = dTmp * -0.0002;
  /* Make sure we have a good max number */
  if (tpC->iMaxInp == 0) {
    if (tpC->tInp.eType == WHITE ||
	tpC->tInp.eType == SWEEP ||
	tpC->tInp.eType == TONE)    tpC->iMaxInp = 64000;
    else			    tpC->iMaxInp = -1;
  }

  /* Init ADPCM if necessary */
  if (tpC->tInp.eType == ADPCM) AConvertADPCMinit(0,tpC->tInp.iBits,8,3);
  if (tpC->tOut.eType == ADPCM) AConvertADPCMinit(1,tpC->tOut.iBits,8,3);

  /* Init IMA if necessary */
  if (tpC->tInp.eType == IMAPCM) AConvertInitIMA(&tpC->tInp);
  if (tpC->tOut.eType == IMAPCM) AConvertInitIMA(&tpC->tOut);

  /* Create the appropriate filters */
  if (tpC->iFilterSize < 1) 	     tpC->iFilterSize = 1;
  if (tpC->iFilterSize > MAXFILTSIZ) tpC->iFilterSize = MAXFILTSIZ;
  tpC->dRho = (double)tpC->tOut.iFreq / (double)tpC->tInp.iFreq;
  if (tpC->dRho > (double)MAXRATECHANGE)
    AConvertUsage("Can't change rate by more than a factor of %d",
		  MAXRATECHANGE);
  tpC->iIncr 		= (int)(0.5 + (double)NZ / tpC->dRho);
  if (tpC->dRho != 1.0) CreateFilter(tpC);

  /* Make sure we have multipliers for buffer sizes */
  if (tpC->tInp.iFreq > tpC->tOut.iFreq)
       iTmp = (tpC->tInp.iFreq / tpC->tOut.iFreq) + 1;
  else iTmp = (tpC->tOut.iFreq / tpC->tInp.iFreq) + 1;
  if (iTmp > iMult) iMult = iTmp;
  iTmp = tpC->tInp.iSamps * 2;
  if (tpC->tInp.iBytes > iTmp)	iTmp		= tpC->tInp.iBytes;
  if (iTmp > iUnitSize) 	iUnitSize	= iTmp;
  iTmp = tpC->tOut.iSamps * 2;
  if (tpC->tOut.iBytes > iTmp)	iTmp		= tpC->tOut.iBytes;
  if (iTmp > iUnitSize) 	iUnitSize	= iTmp;
  if (iBufLen == 0) {
    iBufLen	= 1024;
    tpBuf1	= (Buf*)malloc(sizeof(Buf));
    if (!tpBuf1) AConvertUsage("Can't alloc initial buffer union ");
    tpBuf1->ucp	= (u_char*)malloc(iBufLen);
    if (!tpBuf1->ucp) AConvertUsage("Can't alloc inital buf1");
    tpBuf2	= (Buf*)malloc(sizeof(Buf));
    if (!tpBuf2) AConvertUsage("Can't alloc initial buffer union 2");
    tpBuf2->ucp	= (u_char*)malloc(iBufLen);
    if (!tpBuf2->ucp) AConvertUsage("Can't alloc inital buf2");
  }
  RETURN(0);
}

int AConvert1(				/* 0=MAXINP -1=ERROR else DstCnt */
  Ctxt* 	tpC,			/* Current context */
  int		iCnt,			/* # of input units */
  void*		vpSrc,			/* Buffer of input data */
  short**	sppDst)			/* Returned pointer to output data */
{
  Bool		bRemoteBuf = bFALSE;
  short*	spSrc = (short*)vpSrc;

  INITUSAGE(-1);

  /* Make sure the buffers are big enough */
  if (iBufLen < iCnt*iMult*iUnitSize) {
    iBufLen	= iCnt*iMult*iUnitSize;
    if (!tpBuf1->ucp) tpBuf1->ucp = (u_char*)malloc(iBufLen);
    else	      tpBuf1->ucp = (u_char*)realloc(tpBuf1->ucp,iBufLen);
    if (!tpBuf1->ucp) AConvertUsage("Can't alloc initial buffer 1");
    if (!tpBuf2->ucp) tpBuf2->ucp = (u_char*)malloc(iBufLen);
    else	      tpBuf2->ucp = (u_char*)realloc(tpBuf2->ucp,iBufLen);
    if (!tpBuf2->ucp) AConvertUsage("Can't alloc initial buffer 2");
  }

  /* Take care of the "we're done" case */
  if (tpC->iMaxInp == 0) RETURN(0);

  /* Handle `fake' inputs */
  if (tpC->tInp.eType == WHITE) 	WhiteNoise(iCnt,tpBuf1->sp);
  else if (tpC->tInp.eType == SWEEP ||
	   tpC->tInp.eType == TONE) 	SweepFreq(tpC,iCnt,tpBuf1->sp);

  /* Use `real' input */
  else {
    /* If it was ADPCM expand it to linear */
    if (tpC->tInp.eType == ADPCM)
    {
      iCnt = AConvertADPCMuncompress(
	      0,iCnt*tpC->tInp.iBytes,(u_char*)spSrc,tpBuf2->sp);
      FLIPBUFS;
    }

    /* Maybe it's IMA encoded data */
    else if (tpC->tInp.eType == IMAPCM)
    {
      iCnt = AConvertUncompressIMA(
	      &tpC->tInp,iCnt,(u_char*)spSrc,tpBuf2->sp);
      if (iCnt < 0) RETURN(iCnt);
      FLIPBUFS;
    }

    /* If it was ULAW/ALAW 8bit data, expand it to linear 16bit */
    else if (tpC->tInp.eType == ULAW8 || tpC->tInp.eType == ALAW8) 
    {
      ExpandBuf(tpC,iCnt,(u_char*)spSrc,tpBuf2->sp);
      FLIPBUFS;
    }

    /* Else... use the remote buffer */
    else bRemoteBuf = bTRUE;
  }

  /* Update amount to read */
  tpC->iInpSamps += iCnt;
  if (tpC->iMaxInp > 0) {
    tpC->iMaxInp   -= iCnt;
    if (tpC->iMaxInp < 0) tpC->iMaxInp = 0;
  }

  /* Do Byte Swap */
  if (tpC->bFlip == bTRUE) FlipBuf(iCnt,bRemoteBuf ? spSrc : tpBuf1->sp);

  /* Return what we did */
  *sppDst = bRemoteBuf ? spSrc : tpBuf1->sp;
  RETURN(iCnt);
}

int AConvert2(				/* 0=MAXINP -1=ERROR else DstCnt */
  Ctxt* 	tpC,			/* Current context */
  int		iCnt,			/* # of input units */
  short*	spSrc,			/* Buffer of input data */
  short**	sppDst)			/* Returned pointer to output data */
{
  Bool	bRemoteBuf = bTRUE;

  /* Do volume */
  if (tpC->dGain != 1.0) SetVolume(tpC,iCnt,spSrc);

  /* Do vox */
  if (tpC->bDoVox) iCnt = VoxBuf(tpC,iCnt,spSrc);

  /* Do AGC */
  if (tpC->bAGC) DoAGC(tpC,iCnt,spSrc);

  /* Perform rate change */
  if (tpC->dRho != 1.0) {
    iCnt = RateChange(tpC,iCnt,spSrc,tpBuf2->sp);
    bRemoteBuf = bFALSE;
    FLIPBUFS;
  }

  /* Handle *CEP* output */
  if (tpC->tOut.eType == CEPSTRUM || tpC->tOut.eType == MELCEP) {
    AConvertUsage("cepstrum and melcep not implemented yet");

  /* Handle *PLP* output */
  } else if (tpC->tOut.eType == RASTA || tpC->tOut.eType == PLP) {
    iCnt = AConvertRasta(tpC,iCnt,bRemoteBuf ? spSrc : tpBuf1->sp,tpBuf2->fp);
    bRemoteBuf = bFALSE;
    FLIPBUFS;
  }

  /* Do stats */
  tpC->iOutSamps	+= iCnt;
  if (tpC->bDoStats) ShowStats(tpC,iCnt,bRemoteBuf ? spSrc : tpBuf1->sp);

  /* Return what we did */
  *sppDst = bRemoteBuf ? spSrc : tpBuf1->sp;
  return iCnt;
}

int AConvert3(				/* 0=MAXINP -1=ERROR else DstCnt */
  Ctxt* 	tpC,			/* Current context */
  int		iCnt,			/* # of input units */
  short*	spSrc,			/* Buffer of input data */
  void*		vppDst)			/* Returned pointer to output data */
{
  Bool		bRemoteBuf = bFALSE;
  short**	sppDst = (short**)vppDst;

  /* Now convert output */
  if (tpC->tOut.eType == ULAW8 || tpC->tOut.eType == ALAW8) 
  {
    CompressBuf(tpC,iCnt,spSrc,tpBuf2->ucp);
    FLIPBUFS;
  }

  /* Maybe its ADPCM data */
  else if (tpC->tOut.eType == ADPCM)
  {
    iCnt  = AConvertADPCMcompress(1,iCnt,spSrc,tpBuf2->ucp);
    iCnt /= tpC->tOut.iBytes;
    FLIPBUFS;
  }

  /* Maybe its IMA data */
  else if (tpC->tOut.eType == IMAPCM)
  {
    iCnt  = AConvertCompressIMA(&tpC->tOut,iCnt,spSrc,tpBuf2->ucp);
    if (iCnt < 0) RETURN(iCnt);
    FLIPBUFS;
  }

  /* Otherwise... don't do anything */
  else bRemoteBuf = bTRUE;

  *sppDst = bRemoteBuf ? spSrc : tpBuf1->sp;
  return iCnt;
}

void AConvertStats(AConvertCtxt* tpACC)
{
  Ctxt*		tpC = (Ctxt*)tpACC;

  INITUSAGE( );
  if (!tpC->bDoStats) {
    FREEUSAGE;
    return;
  }

  if (tpC->tapCtxt[0]) {
    int		i;
    for (i=0; i < tpC->tInp.iDstChans; i++) {
      fprintf(stderr,"CHANNEL %d:\n");
      ShowStats(tpC->tapCtxt[i],0,NULL);
    }
  } else ShowStats(tpC,0,NULL);

  FREEUSAGE;
  return;
}

void AConvertClose(AConvertCtxt* tpACC)
{
  int		i;
  Ctxt*		tpC = (Ctxt*)tpACC;

  if (!tpC) return;

  if (tpC->dpDbl) 	free(tpC->dpDbl);
  if (tpC->dpFilter)	free(tpC->dpFilter);
  if (tpC->cpInpFile) 	free(tpC->cpInpFile);
  if (tpC->cpOutFile) 	free(tpC->cpOutFile);
  if (tpC->dpFFT)	free(tpC->dpFFT);
  if (tpC->fpBuf1) 	free(tpC->fpBuf1);
  if (tpC->fpBuf2) 	free(tpC->fpBuf2);

  /* Make sure all sub-contexts are gone */
  for (i=0; i<8; i++)
    if (tpC->tapCtxt[i])
      free(tpC->tapCtxt[i]);

  free(tpC);
  return;
}
