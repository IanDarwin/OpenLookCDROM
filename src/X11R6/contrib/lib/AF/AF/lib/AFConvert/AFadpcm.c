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

#include <math.h>
#include <sys/types.h>
#include <audioproto.h>
#include "AFConvert.h"
#include <AFlib.h>
#include <AFUtils.h>
#include "AFadpcm.h"
#include <malloc.h>

/* Miscellaneous macros. */
#define	LIMIT(low, val, high)	if (val < (low)) val = (low); \
      	      	      	      	else if (val > (high)) val = (high)

#define	BETA	(0.85)			    /* After reference.		    */
#define	GAMMA	(0.98)			    /* After reference.		    */
#define	NORM	(64.0)			    /* step size creation.	    */
#define	TABLE_SIZE  64			    /* After reference.		    */
#define	ULIMIT  ( 1.0 * TABLE_SIZE/2.0 - 1.0)
#define	LLIMIT  (-1.0 * TABLE_SIZE/2.0)

typedef struct sADPCM
{
  int		iQSize;		    /* Quantization size, in bits.	    */
  int		iStepUp;	    /* stepsize table increment */
  int		iStepDown;	    /* stepsize table decrement */
  float		faSS[TABLE_SIZE];   /* index 0 to 63, not 1 to 64   */
  float		faSF[TABLE_SIZE];
  int		(*QuantizePtr)(float);
  void		(*TransmitPtr)(struct sADPCM *, int);
  int		(*GetCodePtr)(struct sADPCM *);
  float		fMLimit;
  int		iOutLen;	    /* where are we in output buffer */
  u_char* 	extBuf;	    	    /* buffer for shorts */
  u_char* 	ucpBuf;	    	    /* buffer for bytes */
  float		fP;		    /* previous */
  int		iIndex;
  float		fD;
  int		iVal;
  int		iHave;
  int		iLeft;
} ADPCM;			    /* global states */


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

static void Transmit2(ADPCM *tpCur, int iCode)
{
  tpCur->iVal |= (iCode << (2*tpCur->iHave));
  tpCur->iHave++;
  
  if(tpCur->iHave == 4) {
    tpCur->ucpBuf[tpCur->iOutLen++] = tpCur->iVal & 0xFF;
    tpCur->iHave	= 0;
    tpCur->iVal		= 0;
  }
}

static void Transmit3(ADPCM *tpCur, int iCode)
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

static void Transmit4(ADPCM *tpCur, int iCode)
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
static int GetCode2(ADPCM *tpCur)
{
  int   	i;
  static int 	iaDequant2[4]={0,1,-2,-1};
  
  if(tpCur->iLeft == 0) {
    tpCur->iVal		= *tpCur->ucpBuf++;
    tpCur->iLeft 	= 4;
  }
  i = tpCur->iVal & 0x03; 
  tpCur->iVal >>= 2;
  tpCur->iLeft--;
  return iaDequant2[i];
}

static int GetCode3(ADPCM *tpCur)
{
  int		i,i0,i1,i2;
  static int 	iaDequant3[8]={0,1,2,3,-4,-3,-2,-1};
  
  if(tpCur->iLeft <= 0) {
    if (tpCur->iLeft == 0) {
      i0		= *tpCur->ucpBuf++;
      tpCur->iLeft--;
    }
    if (tpCur->iLeft == -1) {
      i1		= *tpCur->ucpBuf++;
      tpCur->iLeft--;
    }
    if (tpCur->iLeft == -2) {
      i2		= *tpCur->ucpBuf++;
      tpCur->iLeft = 8;
      tpCur->iVal  = ((i2&0x0FF)<<16) | ((i1&0x0FF)<<8) | (i0&0x0FF);
    }
  }
  i 		= tpCur->iVal & 0x07;
  tpCur->iVal >>= 3;
  tpCur->iLeft--;
  return iaDequant3[i];
}

static int GetCode4(ADPCM *tpCur)
{
  int		i;
  static int 	iaDequant4[16]={0,1,2,3,4,5,6,7,-8,-7,-6,-5,-4,-3,-2,-1};
  
  if(tpCur->iLeft == 0) {
    tpCur->iVal		= *tpCur->ucpBuf++;
    tpCur->iLeft 	= 2;
  }
  i 		= tpCur->iVal & 0x0f;
  tpCur->iVal >>= 4;
  tpCur->iLeft--;
  return iaDequant4[i];
}

/*
 * Ordinary mu-law I/O.
 */
static int GetSample(ADPCM *tpCur, AEncodeType aet, int nc)
{
  int 	iLin;
  
  switch(aet) {
  case ALAW:
	iLin = AF_exp_a[*tpCur->extBuf];
	tpCur->extBuf += nc;
	break;
  case MU255:
	iLin = AF_exp_u[*tpCur->extBuf];
	tpCur->extBuf += nc;
	break;
  case LIN16:
	iLin = *((INT16 *)tpCur->extBuf) >> 2;
	tpCur->extBuf += nc * sizeof(INT16);
	break;
  case LIN32:
	iLin = *((INT32 *)tpCur->extBuf) >> 18;
	tpCur->extBuf += nc * sizeof(INT32);
	break;
  default:
	ErrorF("Unknown encoding type in adpcm output, %d\n",aet);
	break;
  }

  LIMIT(-8031, iLin, 8031);
  return iLin;
}

static void Output(ADPCM *tpCur, AEncodeType aet, int nc, int iLin)
{
  LIMIT(-8031, iLin, 8031);
  switch(aet) {
  case ALAW:
	*tpCur->extBuf = AF_comp_a[iLin & 0x03fff];
	tpCur->extBuf += nc;
	break;
  case MU255:
	*tpCur->extBuf = AF_comp_u[iLin & 0x03fff];
	tpCur->extBuf += nc;
	break;
  case LIN16:
	*((INT16 *)tpCur->extBuf) = iLin * 4;
	tpCur->extBuf += nc * sizeof(INT16);
	break;
  case LIN32:
	*((INT32 *)tpCur->extBuf) = iLin * 4;
	tpCur->extBuf += nc * sizeof(INT32);
	break;
  default:
	ErrorF("Unknown encoding type in adpcm output, %d\n",aet);
	break;
  }
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
static void Compress(AFConvert_Data *acd,
		u_char *ucpInpBuf,
		u_char *ucpOutBuf,
		int iUnits
		)
{	
  int 		lin;
  float		s;
  float		e;
  float		es;
  int 		icode;
  float		eshat;
  float		ehat;
  float		shat;
  int		u_index;
  int		c_index;
  int		i;
  int		oll;
  u_char	*pi,*po,*pia;
  ADPCM*	tpCur;

  pi = ucpInpBuf;
  po = ucpOutBuf;
  oll = (iUnits * AF_sample_sizes[acd->fromType].samps_per_unit) / 
      AF_sample_sizes[acd->toType].samps_per_unit;
  for(u_index=0;u_index<oll;u_index++) {
    pia = pi;
    for(c_index=0;c_index<acd->nChannels;c_index++) {
      tpCur		= ((ADPCM *) acd->state) + c_index;
      tpCur->extBuf	= pia;
      tpCur->ucpBuf	= po;
      tpCur->iOutLen	= 0;

      /* Compress a single unit. */
      for (i=0;i<AF_sample_sizes[acd->toType].samps_per_unit;i++) {

	lin = GetSample(tpCur, acd->fromType, acd->nChannels);
					   /* Returns -8031..8031 */
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
	tpCur->TransmitPtr(tpCur, icode & ((1<<tpCur->iQSize)-1));
      }
      pia += (AF_sample_sizes[acd->fromType].bytes_per_unit/
	    AF_sample_sizes[acd->fromType].samps_per_unit);
      po += AF_sample_sizes[acd->toType].bytes_per_unit;
    }
    pi += acd->nChannels * (AF_sample_sizes[acd->fromType].bytes_per_unit/
	    AF_sample_sizes[acd->fromType].samps_per_unit) * 
		AF_sample_sizes[acd->toType].samps_per_unit;
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
static void UnCompress(AFConvert_Data *acd, 
		u_char *ucpInpBuf,
		u_char *ucpOutBuf,
		int iUnits
		)
{
  int 	iCode;
  float	eshat;
  float	ehat;
  float	shat;
  int	lin;
  ADPCM *tpCur;
  u_char	*pi,*po,*poa;
  int i;
  int u_index,c_index;

  pi = ucpInpBuf;
  po = ucpOutBuf;
  for(u_index=0;u_index<iUnits;u_index++) {
    poa = po;
    for(c_index=0;c_index<acd->nChannels;c_index++) {

      tpCur		= ((ADPCM *) acd->state) + c_index;
      tpCur->ucpBuf	= pi;
      tpCur->extBuf	= poa;
      tpCur->iOutLen	= 0;

      for (i=0;i<AF_sample_sizes[acd->fromType].samps_per_unit;i++) {

	iCode = tpCur->GetCodePtr(tpCur); /* get 2's complement of code */
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
	Output(tpCur, acd->toType, acd->nChannels, lin);
      }
      pi += AF_sample_sizes[acd->fromType].bytes_per_unit;
      poa += (AF_sample_sizes[acd->toType].bytes_per_unit/
	    AF_sample_sizes[acd->toType].samps_per_unit);
    } 
    po += acd->nChannels * (AF_sample_sizes[acd->toType].bytes_per_unit/
	    AF_sample_sizes[acd->toType].samps_per_unit) * 
		AF_sample_sizes[acd->fromType].samps_per_unit;
  }
}


static float faNF[5]={0.0,0.0,4.0,2.0,1.0};  /* Normalize based on iQSize.*/

/*
 * Step size and scale factor tables.
 */
static void int_AConvertADPCMinit(AFConvert_Data *acd, ADPCM *tpCur,
			      int iQSize,int iStepUp,int iStepDown)
{
  int i;

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
void AConvertADPCMcompress(
  AFConvert_Data *acd,
  void* 	ucpInpBuf,
  void* 	ucpOutBuf,
  int 		iUnits
  )
{
  Compress(acd, ucpInpBuf, ucpOutBuf, iUnits);
}

/*
 * UnCompress a buffer
 */
void AConvertADPCMuncompress(
  AFConvert_Data *acd,
  void* 	ucpInpBuf,
  void* 	ucpOutBuf,
  int 		iUnits
   )
{
  UnCompress(acd, ucpInpBuf, ucpOutBuf, iUnits);
}

void
AConvertADPCMInit(AFConvert_Data *acd, AFConvertCmd cmd)
{
  int i;
  int q;

  ADPCM *tpCur = (ADPCM *)acd->state;

  if (cmd == ALLOC) {
	tpCur = (ADPCM *) malloc(acd->nChannels * sizeof(ADPCM));
	AoD(tpCur != (ADPCM *)NULL,
		"Could not allocate memory for convert state\n");
	acd->state = (u_char *)tpCur;
  }
  if ((cmd == ALLOC) || (cmd == RESET)) {
    /* Ugh. */
    switch(acd->fromType) {
    case CRLADPCM2:
      q = 2; break;
    case CRLADPCM3:
      q = 3; break;
    case CRLADPCM4:
      q = 4; break;
    default:
      break;
    }
    switch(acd->toType) {
    case CRLADPCM2:
      q = 2; break;
    case CRLADPCM3:
      q = 3; break;
    case CRLADPCM4:
      q = 4; break;
    default:
      break;
    }
    
    for(i=0;i<acd->nChannels;i++)
      int_AConvertADPCMinit(acd, ((ADPCM *) acd->state) + i, q, 8, 3);  
  }
  if (cmd == DEALLOC) {
    if (acd->state != NULL) free(acd->state);
    acd->state = NULL;
  }
}
