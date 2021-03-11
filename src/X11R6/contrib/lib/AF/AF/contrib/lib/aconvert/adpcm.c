static char rcsid[] = "$RCSfile: adpcm.c,v $, $Revision: 1.9 $, $Date: 1994/06/03 18:06:09 $";
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

/*
 * adpcm.c
 * Written by tml.
 * Initial creation date 5-Jul-91.
 * Revision history:
 *	 5-Jul-91 tml	initial verion, supports 24Kbps.
 *	 7-Jul-91 tml	add 32Kbps and 16Kbps to 24Kbps implementation.
 *	 8-Jul-91 tml	add this header.  Clean-up cruft.
 *	 9-Jul-91 tml/lcs fix several bugs.  
 *	25-Jun-93 dbw   rewrite for inclusion in aconvert
 *
 *  The original version of this program is modeled after,
 *	"Adaptive Differential Pulse Code Modulation Coding",
 *	JR Boddie, JD Johnston, RE Crochiere, JL Flanagan, CA McGonegal,
 *	JW Upton, and DA Berkley, BSTJ, Vol 60, No 7, Sept. 1981.
 */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include <sys/types.h>

#ifndef DEBUG
#define DEBUG	0
#endif

/* Miscellaneous macros. */
#define	LIMIT(low, val, high)	if (val < (low)) val = (low); \
      	      	      	      	else if (val > (high)) val = (high)

#define	BETA	(0.85)			    /* After reference.		    */
#define	GAMMA	(0.98)			    /* After reference.		    */
#define	NORM	(64.0)			    /* step size creation.	    */
#define	TABLE_SIZE  64			    /* After reference.		    */
#define	ULIMIT  ( 1.0 * TABLE_SIZE/2.0 - 1.0)
#define	LLIMIT  (-1.0 * TABLE_SIZE/2.0)

#define ADPCMMAX	2	    /* Number of PCM global states */
typedef struct sADPCM
{
  int		iQSize;		    /* Quantization size, in bits.	    */
  int		iStepUp;	    /* stepsize table increment */
  int		iStepDown;	    /* stepsize table decrement */
  float		faSS[TABLE_SIZE];   /* index 0 to 63, not 1 to 64   */
  float		faSF[TABLE_SIZE];
  int		(*QuantizePtr)(float);
  void		(*TransmitPtr)(int);
  int		(*GetCodePtr)(int*);
  float		fMLimit;
  int		iInpLen;	    /* bytes left in input buffer */
  int		iOutLen;	    /* where are we in output buffer */
  short* 	spBuf;	    	    /* buffer for shorts */
  u_char* 	ucpBuf;	    	    /* buffer for bytes */
  float		fP;		    /* previous */
  int		iIndex;
  float		fD;
  int		iVal;
  int		iHave;
  int		iLeft;
} ADPCM;			    /* global states */

static ADPCM	taADPCM[ADPCMMAX];
static ADPCM*	tpCur;		    /* current state */


static void Leave(char* cpMsg)
{
  fprintf(stderr,"\nERROR(adpcm): %s\n",cpMsg);
  exit(1);
}

/*
 * Customized quantization procedures.
 */

static int iaQ[16]= { -8,-7,-6,-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5, 6, 7 };
static int *ipQ	  = &iaQ[8];

static int Quantize3(float fES)
{
  int iES;
  
  fES += 4.0;
  LIMIT(0.0, fES, 7.0);
  iES = fES;
  iES -= 4;
  return ( ipQ[iES] );
}

static int Quantize4(float fES)
{
  int iES;
  
  fES += 8.0;
  LIMIT(0.0, fES, 15.0);
  iES = fES;
  iES -= 8;
  return ( ipQ[iES] );
}

static int Quantize2(float fES)
{
  int iES;
  
  fES += 2.0;
  LIMIT(0.0, fES, 3.0);
  iES = fES;
  iES -= 2;
  return ( ipQ[iES] );
}

/*
 * Customized code transmission procedures.
 */

static void Transmit2(int iCode)
{
  tpCur->iVal |= (iCode << (2*tpCur->iHave));
  tpCur->iHave++;
  
  if(tpCur->iHave == 4) {
    tpCur->ucpBuf[tpCur->iOutLen++] = tpCur->iVal & 0xFF;
    tpCur->iHave	= 0;
    tpCur->iVal		= 0;
  }
}

static void Transmit3(int iCode)
{
  tpCur->iVal |= (iCode << (3*tpCur->iHave));
  tpCur->iHave++;
  
  if(tpCur->iHave == 8) {
    tpCur->ucpBuf[tpCur->iOutLen++] = tpCur->iVal & 0xFF;
    tpCur->ucpBuf[tpCur->iOutLen++] = (tpCur->iVal >> 8) & 0xFF;
    tpCur->ucpBuf[tpCur->iOutLen++] = (tpCur->iVal >> 16)& 0xFF;
    tpCur->iHave 	= 0;
    tpCur->iVal 	= 0;
  }
}

static void Transmit4(int iCode)
{
  tpCur->iVal |= (iCode << (4*tpCur->iHave));
  tpCur->iHave++;
  
  if(tpCur->iHave == 2) {
    tpCur->ucpBuf[tpCur->iOutLen++] = tpCur->iVal & 0xFF;
    tpCur->iHave 	= 0;
    tpCur->iVal 	= 0;
  }
}

/*
 * Customized code reception procedures.
 */
static int GetCode2(int* ipRetVal)
{
  int   	i;
  static int 	iaDequant2[4]={0,1,-2,-1};
  
  if(tpCur->iLeft == 0) {
    if (tpCur->iInpLen == 0) return 0;
    tpCur->iVal		= *tpCur->ucpBuf++;
    tpCur->iInpLen--;
    tpCur->iLeft 	= 4;
  }
  i = tpCur->iVal & 0x03; 
  tpCur->iVal >>= 2;
  tpCur->iLeft--;
  *ipRetVal = iaDequant2[i];
  return 1;
}

static int GetCode3(int* ipRetVal)
{
  int		i,i0,i1,i2;
  static int 	iaDequant3[8]={0,1,2,3,-4,-3,-2,-1};
  
  if(tpCur->iLeft <= 0) {
    if (tpCur->iLeft == 0) {
      if (tpCur->iInpLen == 0) return 0;
      i0		= *tpCur->ucpBuf++;
      tpCur->iInpLen--;
      tpCur->iLeft--;
    }
    if (tpCur->iLeft == -1) {
      if (tpCur->iInpLen == 0) return 0;
      i1		= *tpCur->ucpBuf++;
      tpCur->iInpLen--;
      tpCur->iLeft--;
    }
    if (tpCur->iLeft == -2) {
      if (tpCur->iInpLen == 0) return 0;
      i2		= *tpCur->ucpBuf++;
      tpCur->iInpLen--;
      tpCur->iLeft = 8;
      tpCur->iVal  = ((i2&0x0FF)<<16) | ((i1&0x0FF)<<8) | (i0&0x0FF);
    }
  }
  i 		= tpCur->iVal & 0x07;
  tpCur->iVal >>= 3;
  tpCur->iLeft--;
  *ipRetVal = iaDequant3[i];
  return 1;
}

static int GetCode4(int* ipRetVal)
{
  int		i;
  static int 	iaDequant4[16]={0,1,2,3,4,5,6,7,-8,-7,-6,-5,-4,-3,-2,-1};
  
  if(tpCur->iLeft == 0) {
    if (tpCur->iInpLen == 0) return 0;
    tpCur->iVal		= *tpCur->ucpBuf++;
    tpCur->iInpLen--;
    tpCur->iLeft 	= 2;
  }
  i 		= tpCur->iVal & 0x0f;
  tpCur->iVal >>= 4;
  tpCur->iLeft--;
  *ipRetVal	= iaDequant4[i];
  return 1;
}

/*
 * Ordinary mu-law I/O.
 */
static int GetSample(int* ipLin)
{
  int 	iLin;
  
  if (tpCur->iInpLen == 0) return 0;
  iLin	= *tpCur->spBuf++;
  tpCur->iInpLen--;
  LIMIT(-8031, iLin, 8031);
  *ipLin = iLin;
  return 1;
}

static void Output(int iLin)
{
  unsigned char ucMu;
  
  LIMIT(-8031, iLin, 8031);
  tpCur->spBuf[tpCur->iOutLen++] = iLin;
}

/*
 * Compressor.
 *  Input:
 *	iQSize.
 *	step size and scale factor tables.
 *	Gets linear samples using getSample procedure.
 *  Output
 *	2,3, or 4 bit code words.  1 per sample.
 *  I/O
 *	Gets mu-law samples using getSample.
 *	Writes code words using customized "transmit" procedures.
 */
static void Compress()
{	
  int 		lin;
  float		s;
  float		e;
  float		es;
  int 		icode;
  float		eshat;
  float		ehat;
  float		shat;

  for (;;) {
    if (!GetSample(&lin)) break;    /* -8031..8031 */
    s		 = lin;
    s  		*= NORM;	    /* -500000..500000 */
    
    e   	 = s - tpCur->fP; /* .15 of s for a constant input */
    es  	 = tpCur->faSF[tpCur->iIndex] * e;
    icode 	 = tpCur->QuantizePtr(es); /* -8..+7 for iQSize == 3 */
    
    /* follows a copy of the decoder */
    eshat 	 = ((float)icode) + 0.5; /* -7.5..7.5 */
    ehat 	 = tpCur->faSS[tpCur->iIndex] * eshat;
    
    /* predictor */
    shat 	 = tpCur->fP + ehat;
    tpCur->fP	 = BETA * shat;
    
    if (fabs((double)eshat) >= tpCur->fMLimit)
      tpCur->fD = GAMMA * tpCur->fD + tpCur->iStepUp;
    else
      tpCur->fD = GAMMA * tpCur->fD - tpCur->iStepDown;

    LIMIT(LLIMIT, tpCur->fD, ULIMIT);
    
    tpCur->iIndex = ((int)tpCur->fD) + TABLE_SIZE/2;

    /* decoder output is shat */
    tpCur->TransmitPtr(icode & ((1<<tpCur->iQSize)-1));
  }
}


/*
 * Decompressor.
 *  Input:
 *	iQSize.
 *	step size and scale factor tables.
 *	2,3, or 4 bit code words.  1 per sample.
 *  Output
 *	reconstructed samples.
 *  I/O
 *	Gets code words using customized "getCode" procedures.
 *	Writes reconstructed  samples using output (which converts to mu-law.)
 */
static void UnCompress()
{
  int 	iCode;
  float	eshat;
  float	ehat;
  float	shat;
  int	lin;

  for(;;) {
    if (!tpCur->GetCodePtr(&iCode)) break; /* get 2's complement of code */
    eshat = (float)iCode + 0.5;
    ehat = tpCur->faSS[tpCur->iIndex] * eshat;
    
    /* predictor */
    shat = tpCur->fP + ehat;
    tpCur->fP = BETA * shat;
    
    if (fabs((double) eshat) >= tpCur->fMLimit)
      tpCur->fD = GAMMA * tpCur->fD + tpCur->iStepUp;
    else
      tpCur->fD = GAMMA * tpCur->fD - tpCur->iStepDown;
    LIMIT(LLIMIT, tpCur->fD, ULIMIT);
    
    tpCur->iIndex = ((int)tpCur->fD) + TABLE_SIZE/2;
    shat /= NORM;
    lin = shat;
    Output(lin);
  }
}


static float faNF[5]={0.0,0.0,4.0,2.0,1.0};  /* Normalize based on iQSize.*/

/*
 * Step size and scale factor tables.
 */
void AConvertADPCMinit(int iIdx,int iQSize,int iStepUp,int iStepDown)
{
  int i;

  if (iIdx >= ADPCMMAX) Leave("Too many global ADPCM states");
  tpCur				= &taADPCM[iIdx];
  tpCur->iQSize			= iQSize;
  tpCur->iStepUp		= iStepUp;
  tpCur->iStepDown		= iStepDown;
  
  /* iQSize 4, factor of 1, iQSize 3, factor of 2, iQSize 2, factor of 4 */
  tpCur->faSS[0] = 1.0 * NORM * faNF[iQSize] * 8.0;
  tpCur->faSF[0] = 1.0/tpCur->faSS[0];
  for(i=1;i<TABLE_SIZE;++i){
    tpCur->faSS[i] = tpCur->faSS[0] * pow(1.0902, (double)i);
    tpCur->faSF[i] = 1.0/tpCur->faSS[i];
  }

  /* Set up support routines */
  if (iQSize == 3){
    tpCur->QuantizePtr = Quantize3;
    tpCur->TransmitPtr = Transmit3;
    tpCur->GetCodePtr  = GetCode3;
    tpCur->fMLimit     = 2.5;
  } else if (iQSize == 4){
    tpCur->QuantizePtr = Quantize4;
    tpCur->TransmitPtr = Transmit4;
    tpCur->GetCodePtr  = GetCode4;
    tpCur->fMLimit     = 4.5;
  } else{
    tpCur->QuantizePtr = Quantize2;
    tpCur->TransmitPtr = Transmit2;
    tpCur->GetCodePtr  = GetCode2;
    tpCur->fMLimit     = 1.5;
  }

  tpCur->fP	 = 0.0;
  tpCur->iIndex	 = TABLE_SIZE / 2;
  tpCur->fD	 = 0.0;
  tpCur->iVal	 = 0;
  tpCur->iHave	 = 0;
  tpCur->iLeft	 = 0;
}

/*
 * Compress a buffer
 */
int AConvertADPCMcompress(
  int 		iIdx,
  int 		iCnt,
  short* 	spInpBuf,
  u_char* 	ucpOutBuf)
{
  if (iIdx >= ADPCMMAX) Leave("Too many global ADPCM states");
  tpCur			= &taADPCM[iIdx];
  tpCur->iInpLen	= iCnt;
  tpCur->spBuf		= spInpBuf;
  tpCur->iOutLen	= 0;
  tpCur->ucpBuf		= ucpOutBuf;
  Compress();
  return tpCur->iOutLen;
}

/*
 * UnCompress a buffer
 */
int AConvertADPCMuncompress(
  int 		iIdx,
  int 		iCnt,
  u_char* 	ucpInpBuf,
  short* 	spOutBuf)
{
  if (iIdx >= ADPCMMAX) Leave("Too many global ADPCM states");
  tpCur			= &taADPCM[iIdx];
  tpCur->iInpLen	= iCnt;
  tpCur->ucpBuf		= ucpInpBuf;
  tpCur->iOutLen	= 0;
  tpCur->spBuf		= spOutBuf;
  UnCompress();
  return tpCur->iOutLen;
}
