/*static char rcsid[] = "$RCSfile: aconvP.h,v $, $Revision: 1.20 $, $Date: 1994/06/03 17:13:28 $";*/

/************************************************************************
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
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
 ************************************************************************/

/************************************************************************
 *
 * FILE NAME		: $RCSfile: aconvP.h,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:13:28 $
 * REVISION		: $Revision: 1.20 $
 * DESCRIPTION		: General purpose audio conversion routines
 *
 * REVISION HISTORY
 *
 * $Log: aconvP.h,v $
 * Revision 1.20  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.19  1994/01/28  16:26:04  tml
 * add ttypes to shut-up the build on mips, apparently sys/stat.h depends
 * on sys/types but does not include it....
 * 	tom
 *
 * Revision 1.18  1993/12/08  23:23:39  wecker
 * Added new AGC code and preload
 *
 * Revision 1.18  1993/12/03  21:47:14  wecker
 * Added scaling of Energy and PTP
 *
 * Revision 1.17  1993/10/26  21:38:14  tml
 * endif fix
 *
 * Revision 1.16  1993/10/25  20:33:53  wecker
 * *** empty log message ***
 *
 * Revision 1.15  1993/10/20  18:09:30  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.14  1993/10/20  17:33:52  wecker
 * Added copyrights
 *
 * Revision 1.13  1993/10/19  23:17:29  wecker
 * Changed to Imakefile build
 *
 * Revision 1.12  1993/10/15  16:13:26  wecker
 * Added PTP and ZC to coefficients for PLP and RASTA
 *
 * Revision 1.13  1993/08/31  20:44:50  wecker
 * Added IMA code
 *
 * Revision 1.12  1993/08/30  17:26:05  wecker
 * Rewrote setjmp code to stack jmpbufs
 *
 * Revision 1.11  1993/08/28  16:17:51  wecker
 * Added TONE type and split tones/sweep. Fixed ASCII files
 *
 * Revision 1.10  1993/08/27  22:44:27  wecker
 * Fixed problems with filters and multi-channels
 *
 * Revision 1.9  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.8  1993/08/23  22:02:20  wecker
 * Fully tested version of Rasta-PLP
 *
 * Revision 1.7  1993/08/23  20:45:12  wecker
 * Added Rasta-PLP code
 *
 * Revision 1.6  1993/08/22  18:00:34  wecker
 * "Rewrote
 *
 * Revision 1.5  1993/08/21  20:07:50  wecker
 * Added SPHERE file format
 *
 * Revision 1.4  1993/08/21  15:43:54  wecker
 * better filter code
 *
 * Revision 1.3  1993/08/20  19:16:08  wecker
 * Something is wrong with filterSize... this is a checkpoint
 *
 * Revision 1.2  1993/08/20  15:27:33  wecker
 * New callable interface
 *
 * Revision 1.1  1993/08/10  22:46:17  wecker
 * Initial revision
 *
 *
 ************************************************************************/

#ifndef _ACONVERTP_H_
#define _ACONVERTP_H_

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include <setjmp.h>
#include <varargs.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sphere/header.h>
#include <sphere/sp.h>
#include "aconvert.h"
#include "adpcm.h"

#ifndef DEBUG
#define DEBUG	0
#endif

#ifdef ABS
#undef ABS
#endif
#define ABS(x)	((x) >= 0 ? (x) : -(x))

#define		BUFFERSIZE	1000
#define 	MAXFILTSIZ	100
#define		MAXRATECHANGE	10
#define		NZ		512

typedef struct Jmp_s {
  struct Jmp_s*	tpPrev;
  jmp_buf	tCurr;
} Jmp;

#define	IFUSAGE \
  Jmp		tJmp;\
  tJmp.tpPrev		= tpAConvertJmpBuf;\
  tpAConvertJmpBuf	= &tJmp;\
  if (setjmp(tJmp.tCurr))

#define	INITUSAGE(r) IFUSAGE return r

#define FREEUSAGE tpAConvertJmpBuf=tpAConvertJmpBuf->tpPrev
#define RETURN(r) return((FREEUSAGE),r)

typedef enum Bool_e { bFALSE=0, bTRUE=1 } Bool;
typedef enum Format_e	{ RAW, SPHERE, HTK, ASCII } Format;
typedef enum Type_e {
  ULAW8, ALAW8, LINEAR, ADPCM, IMAPCM, SWEEP, WHITE, TONE, RASTA, PLP,
  CEPSTRUM, MELCEP
} Type;

typedef struct Data_s
{
  Format	eFormat;		/* External file format */
  Type		eType;			/* Data type */
  int		iBits;			/* Number of bits per sample */
  int		iBytes;			/* Number of bytes per unit */
  int		iSamps;			/* Number of samples per unit */
  int		iCoeffs;		/* Number of coeffs for FFTs */
  int		iWindow;		/* Window of samples for FFTs */
  int		iIncr;			/* Increment of samps for FFTs */
  float		fExpon;			/* Exponent for FFTs */
  int		iFreq;			/* Frequency in samples/second */
  int		iSrcChans;		/* Total # of source chans */
  int		iDstChans;		/* Total # of destination chans */
  u_char	ucaDstMsk[8];		/* Destination channel masks */
  u_char	ucaDstCnt[8];		/* # of sources in destination */
  int		iOut;			/* IMA previous sample */
  int		iLevel;			/* IMA previous level */
  int		iLast;			/* IMA previous output */
} Data;

typedef union Buf_u 
{
  u_char	*ucp;
  short		*sp;
  float		*fp;
} Buf;

typedef struct Quant_s {
  int iaOut[8];
} Quant;

typedef struct Ctxt_s {

  /* Statistical info */
  int		iTruncated;		/* # of units truncated */
  int		iInpSamps;		/* # of units in */
  int		iOutSamps;		/* # of units out */
  int		iMaxVal;		/* Maximum of absolute values */
  int		iAvgVal;		/* average of absolute values */

  /* I/O formats */
  Data		tInp;			/* Input data  [8khz /8bit  ULAW]*/
  Data		tOut;			/* Output data [16khz/16bit LINEAR]*/

  /* command line options */
  Bool		bFirstTime;		/* Is this the first call? */
  Bool		bAGC;			/* Do AGC? [n]*/
  Bool		bFlip;			/* Flip data after reading? [n] */
  double	dGain;			/* Gain multiplier [1.0]*/
  double	dRho;			/* Frequency ratio [1.0]*/
  int		iSkipHeader;		/* # of bytes to skip [0]*/
  int		bPreLoad;		/* Pre-load input file */
  Bool		bCopyHeader;		/* Copy skipped header to output? [n]*/
  int		iMaxInp;		/* Max # of input samples [0]*/
  Bool		bDoStats;		/* Compute statistics? [n]*/
  Bool		bDoVox;			/* Silence compress input data? [n]*/
  int		iFilterSize;		/* Filter size [10]*/
  char*		cpInpFile;		/* Input file name to use  (or NULL) */
  char*		cpOutFile;		/*  special case: "-" for stdin/out */

  double*	dpFilter;
  double	dAGCval;
  short		sMaxVal;
  int		iSilCnt;
  double 	dInc;			/* SWEEP next x increment */
  double 	dAdd;			/* SWEEP next frequency */
  double 	dIdx;			/* SWEEP current x */
  double*	dpDbl;
  int	 	iP;
  int		iIncr;

  struct Ctxt_s* tapCtxt[8];		/* Multi channel sub-contexts */

  double*	dpFFT;			/* Temp for Rasta-PLP */
  int		iPoints;		/* npoints for Rasta-PLP */
  int		iPoints2;		/* npoints2 for Rasta-PLP */
  int		iBufSaved;		/* buffered dpDbl values */
  float*	fpBuf1;			/* main data buffer (v1) */
  float*	fpBuf2;			/* main data buffer (v2) */
} Ctxt;

/*********************** HTK definitions *********************************/
typedef enum HTKsampKind_e {
  WAVEFORM=0,
  LPC=1,
  LPREFC=2,
  LPCEPSTRA=3,
  LPDELCEP=4,
  IREFC=5,
  MFCC=6,
  FBANK=7,
  MELSPEC=8,
  ANON
} HTKsampKind;

#define HTK_ENERGY	00100
#define HTK_NULLE	00200
#define HTK_DELTA	00400
#define HTK_ACCEL	01000
#define HTK_BASE	00077

typedef struct {
  int	iNumSamples;
  int	iSampPeriod;
  short	iSampSize;
  short	iSampKind;
} HTKheader;

/************************ Internal routines ******************************/

extern Jmp*	tpAConvertJmpBuf;

int AConvertParse(			/* Parse argc/argv */
  Ctxt*		tpC,			/* Current context */
  int 		argc,
  char** 	argv);

int AConvertInitChans(			/* return iMaxChans (calls AConvert0)*/
  Ctxt*		tpC);			/* Current Context */

/* Aconvert phases: */
int AConvert0(				/* Do initializations */
  Ctxt*		tpC,			/* Context to initialize */
  int		iChan);			/* Current channel */
int AConvert1(				/* 0=MAXINP -1=ERROR else DstCnt */
  Ctxt* 	tpC,			/* Current context */
  int		iCnt,			/* # of input units */
  void*		vpSrc,			/* Buffer of input data */
  short**	sppDst);		/* Returned pointer to output data */
int AConvert2(				/* 0=MAXINP -1=ERROR else DstCnt */
  Ctxt* 	tpC,			/* Current context */
  int		iCnt,			/* # of input units */
  short*	spSrc,			/* Buffer of input data */
  short**	sppDst);		/* Returned pointer to output data */
int AConvert3(				/* 0=MAXINP -1=ERROR else DstCnt */
  Ctxt* 	tpC,			/* Current context */
  int		iCnt,			/* # of input units */
  short*	spSrc,			/* Buffer of input data */
  void*		vppDst);		/* Returned pointer to output data */

void AConvertUsage(char* va_alist,...);

/* Rasta-PLP routines */
void  AConvertFFT(
  Ctxt*		tpC,
  float*	real,
  float*	power,
  long		ll,
  long		m);
int AConvertRasta(			/* Returns vector count */
  Ctxt*   	tpC,
  int     	iCnt,
  short*  	spBuf,
  float*  	fpBuf);

/* IMA routines */
void AConvertInitIMA(Data* tpD);		/* Initialize IMA */
int AConvertCompressIMA(		/* Return number of units (error: -1)*/
  Data* 	tpD,			/* Current context */
  int		iCnt,			/* Number of input samples */
  short*	spInp,			/* Input buffer */
  u_char*	ucpOut);		/* Output buffer */
int AConvertUncompressIMA(		/* Return number of units (error: -1)*/
  Data* 	tpD,			/* Current context */
  int		iCnt,			/* Number of input units */
  u_char*	ucpInp,			/* Input buffer */
  short*	spOut);			/* Output buffer */

#endif
