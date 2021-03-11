static char rcsid[] = "$RCSfile: init.c,v $, $Revision: 1.19 $, $Date: 1994/06/03 17:13:28 $";

/************************************************************************
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *                        All Rights Reserved
 *		Maynard, Massachusetts
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
 * FILE NAME		: $RCSfile: init.c,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:13:28 $
 * REVISION		: $Revision: 1.19 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: init.c,v $
 * Revision 1.19  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.18  1994/06/01  19:30:00  jg
 * change header file name to meet PC standards
 *
 * Revision 1.17  1994/01/28  17:13:22  stewart
 * Renamed BytesPerUnit to avoid AFConvert library conflict
 *
 * Revision 1.16  1993/12/08  23:23:39  wecker
 * Added new AGC code and preload
 *
 * Revision 1.16  1993/12/03  21:47:14  wecker
 * Added scaling of Energy and PTP
 *
 * Revision 1.15  1993/10/25  20:33:53  wecker
 * *** empty log message ***
 *
 * Revision 1.14  1993/10/20  18:09:30  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.13  1993/10/20  17:33:52  wecker
 * Added copyrights
 *
 * Revision 1.12  1993/10/19  23:17:29  wecker
 * Changed to Imakefile build
 *
 * Revision 1.11  1993/10/15  16:13:26  wecker
 * Added PTP and ZC to coefficients for PLP and RASTA
 *
 * Revision 1.10  1993/09/01  18:21:50  wecker
 * Fixed init problem with dpFilter
 *
 * Revision 1.9  1993/08/31  20:44:50  wecker
 * Added IMA code
 *
 * Revision 1.8  1993/08/30  17:26:05  wecker
 * Rewrote setjmp code to stack jmpbufs
 *
 * Revision 1.7  1993/08/29  20:06:22  wecker
 * Started adding man pages
 *
 * Revision 1.6  1993/08/28  16:17:51  wecker
 * Added TONE type and split tones/sweep. Fixed ASCII files
 *
 * Revision 1.5  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.4  1993/08/23  22:02:20  wecker
 * Fully tested version of Rasta-PLP
 *
 * Revision 1.3  1993/08/23  20:45:12  wecker
 * Added Rasta-PLP code
 *
 * Revision 1.2  1993/08/22  18:00:34  wecker
 * "Rewrote
 *
 * Revision 1.1  1993/08/21  20:29:21  wecker
 * Initial revision
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

AConvertCtxt* AConvertAlloc()
{
  int		i;
  Ctxt*		tpC   = (Ctxt*)malloc(sizeof(Ctxt));

  /* Set up an error context */
  INITUSAGE(NULL);

  /* Make sure we got the data */
  if (!tpC) AConvertUsage("Can't create context");

  /* Now fill in the defaults */
  tpC->tInp.eFormat	= RAW;
  tpC->tInp.eType	= ULAW8;
  tpC->tInp.iBits	= 8;
  tpC->tInp.iCoeffs	= 11;
  tpC->tInp.iWindow	= 320;
  tpC->tInp.iIncr	= 160;
  tpC->tInp.fExpon	= 0.6;
  tpC->tInp.iBytes	= 1;
  tpC->tInp.iSamps	= 1;
  tpC->tInp.iFreq	= 8000;
  tpC->tInp.iSrcChans	= 1;
  tpC->tInp.iDstChans	= 1;
  tpC->tInp.ucaDstMsk[0]= 1;
  tpC->tInp.ucaDstCnt[0]= 1;

  tpC->tOut.eFormat	= RAW;
  tpC->tOut.eType	= ULAW8;
  tpC->tOut.iBits	= 8;
  tpC->tOut.iCoeffs	= 11;
  tpC->tOut.iWindow	= 320;
  tpC->tOut.iIncr	= 160;
  tpC->tOut.fExpon	= 0.6;
  tpC->tOut.iBytes	= 1;
  tpC->tOut.iSamps	= 1;
  tpC->tOut.iFreq	= 8000;
  tpC->tOut.iSrcChans	= 1;
  tpC->tOut.iDstChans	= 1;
  tpC->tOut.ucaDstMsk[0]= 1;
  tpC->tOut.ucaDstCnt[0]= 1;

  tpC->bFirstTime	= bTRUE;
  tpC->bAGC		= bFALSE;
  tpC->bFlip		= bFALSE;
  tpC->dGain		= 1.0;
  tpC->dRho		= 1.0;
  tpC->iSkipHeader	= 0;
  tpC->bPreLoad		= bFALSE;
  tpC->bCopyHeader	= bFALSE;
  tpC->iMaxInp		= 0;
  tpC->bDoStats		= bFALSE;
  tpC->bDoVox		= bFALSE;
  tpC->iTruncated	= 0;
  tpC->iFilterSize	= 10;
  tpC->iInpSamps	= 0;
  tpC->iOutSamps	= 0;
  tpC->iMaxVal		= -99999;
  tpC->iAvgVal		= 0;
  tpC->cpInpFile	= NULL;
  tpC->cpOutFile	= NULL;
  tpC->dIdx 		= 0;
  tpC->dpDbl		= NULL;
  tpC->dpFilter		= NULL;
  tpC->fpBuf1		= NULL;
  tpC->fpBuf2		= NULL;

  for (i=0; i<8; i++)   tpC->tapCtxt[i] = NULL;

  tpC->dpFFT		= NULL;
  tpC->iPoints		= 0;
  tpC->iPoints2		= 0;
  RETURN((AConvertCtxt*)tpC);
}

static Bool BigEndian()
{
  union 
  {
    char	ca[2];
    short	sa;
  } u;
  
  u.ca[0] = 0x12;
  u.ca[1] = 0x34;
  if (u.sa == 0x1234)	return bTRUE;
  else			return bFALSE;
}

static int Bytes_Per_Unit(int iBits)
{
  int	iBytes = 1;

  while ((iBytes << 3) % iBits) iBytes++;
  return iBytes;
}

#define	COEFFBITS(n)	((n)*sizeof(float)*8)

static void ParseChans(Ctxt* tpC,char* cpStr,Bool bInp)
{
  int	i;
  char* cpVal;
  Data*	tpData;

  /* Set up the right context to work in */
  if (bInp) tpData = &tpC->tInp;
  else	    tpData = &tpC->tOut;

  /* First get the total number of source channels */
  cpVal 		= strchr(cpStr,':');
  if (cpVal) *cpVal++ = '\0';

  if (!isdigit(*cpStr)) AConvertUsage("Channel count must be a digit");
  if (cpStr[1] != '\0') AConvertUsage("Channel count must be 1 digit");
  tpData->iSrcChans = *cpStr - '0';
  if (tpData->iSrcChans < 1 || tpData->iSrcChans > 8)
    AConvertUsage("Bad number of channels: %d",tpData->iSrcChans);

  /* Take care of the default cases */
  tpData->iDstChans	= tpData->iSrcChans;
  for (i=0; i<tpData->iDstChans; i++) {
    tpData->ucaDstMsk[i] = 1 << i;
    tpData->ucaDstCnt[i] = 1;
  }
  if (bInp) {
    tpC->tOut.iSrcChans    = tpData->iDstChans;
    tpC->tOut.iDstChans    = 1;
    tpC->tOut.ucaDstCnt[0] = tpC->tOut.iSrcChans;
    for (i=0; i<tpC->tOut.iSrcChans; i++)
      tpC->tOut.ucaDstMsk[0] |= 1 << i;
  } else if (tpData->iSrcChans != tpC->tInp.iDstChans)
    AConvertUsage("Input dest chans (%d) != Output src chans (%d)",
		    tpC->tInp.iDstChans,tpData->iSrcChans);

  if ((!cpVal) || !cpVal[0]) return;

  /* Parse the input info */
  for (i=0; i<8; i++) {
    tpData->ucaDstMsk[i] = 0;
    tpData->ucaDstCnt[i] = 0;
  }
  for (i=0,cpStr = strtok(cpVal,":"); cpStr; cpStr = strtok(NULL,":"),i++) {
    while (*cpStr) {
      int	iChan;

      if (i >= 8) AConvertUsage("Can't have more than 8 channels");
      if (!isdigit(*cpStr)) AConvertUsage("Channel must be a digit");
      iChan = *cpStr++ - '0';
      if (iChan < 1 || iChan > tpData->iSrcChans)
	AConvertUsage("Bad input mix channel: %d",iChan);
      tpData->ucaDstMsk[i] |= 1 << (iChan-1);
      tpData->ucaDstCnt[i]++;
    }
  }
  tpData->iDstChans = i;
  if (!tpData->iDstChans) AConvertUsage("Can't have 0 channels output");

  /* Take care of output defaults */
  if (bInp) {
    tpC->tOut.iSrcChans = tpC->tInp.iDstChans;
    tpC->tOut.iDstChans = tpC->tInp.iDstChans;
    for (i=0; i<tpC->tOut.iDstChans; i++)
      tpC->tOut.ucaDstMsk[i] = 1 << (i % tpC->tInp.iDstChans);
  }
}

typedef struct Init1_s {
  char*		cpName;
  Type		eType;
  int		iBits;
  Bool		bInp;
  Bool		bOut;
} Init1;

static Init1 taInit1[] = {
  "ulaw",	ULAW8,	   8,	bTRUE, bTRUE,
  "alaw",	ALAW8,	   8,	bTRUE, bTRUE,
  "adpcm",	ADPCM,	   4,	bTRUE, bTRUE,
  "ima",	IMAPCM,	   4,	bTRUE, bTRUE,
  "linear",	LINEAR,	  16,	bTRUE, bTRUE,
  "sweep",	SWEEP,	  16,	bTRUE, bFALSE,
  "tone",	TONE,	  16,	bTRUE, bFALSE,
  "white",	WHITE,	  16,	bTRUE, bFALSE,
  "rasta",	RASTA,	  16,	bFALSE,bTRUE,
  "plp",	PLP,	  16,	bFALSE,bTRUE,
  "cepstrum",	CEPSTRUM, 16,	bFALSE,bTRUE,
  "melcep",	MELCEP,	  16,	bFALSE,bTRUE,
  NULL,		ULAW8,	  16,	bFALSE,bFALSE
};

typedef struct Init2_s {
  char*		cpName;
  Format	eFormat;
} Init2;

static Init2 taInit2[] = {
  "raw",	RAW,
  "sphere",	SPHERE,
  "htk",	HTK,
  "ascii",	ASCII,
  NULL,		RAW
};

static void ParseIO(Ctxt* tpC,char* cpStr,Bool bInp)
{
  int		iLen;
  char  	caBuf[256];
  Init1*	tpInit1;
  Init2*	tpInit2;
  Data*		tpData;

  /* Get a local copy of the string */
  caBuf[0] = '-';
  strcpy(&caBuf[1],cpStr);

  /* First handle the file name */
  if (caBuf[1] == ',') cpStr = strtok(caBuf,",");
  else		       cpStr = strtok(&caBuf[1],",");
  if (!cpStr) cpStr = "-";
  if (bInp) {
    tpData	= &tpC->tInp;
    if (tpC->cpInpFile) free(tpC->cpInpFile);
    tpC->cpInpFile = NULL;
    if (strcmp(cpStr,"-BUFFER-")) {
      tpC->cpInpFile = (char*)malloc(strlen(cpStr)+1);
      if (!tpC->cpInpFile) AConvertUsage("Can't allocate input file name");
      strcpy(tpC->cpInpFile,cpStr);
    }
  } else {
    tpData	= &tpC->tOut;
    if (tpC->cpOutFile) free(tpC->cpOutFile);
    tpC->cpOutFile = NULL;
    if (strcmp(cpStr,"-BUFFER-")) {
      tpC->cpOutFile = (char*)malloc(strlen(cpStr)+1);
      if (!tpC->cpOutFile) AConvertUsage("Can't allocate output file name");
      strcpy(tpC->cpOutFile,cpStr);
    }
  }

  /* Now walk through any arguments */
  while ((cpStr = strtok(NULL,",")) != NULL) {
    char*	cpVal;

    /* find the option value */
    cpVal = strchr(cpStr,'=');
    if (cpVal) *cpVal++ = '\0';
    else {
      cpVal = cpStr;
      if (isdigit(cpVal[0])) cpStr = "rate";
      else		     cpStr = NULL;
      if (!cpStr) {
	iLen = strlen(cpVal);
	for (tpInit1 = taInit1; tpInit1->cpName; tpInit1++) {
	  if (!strncmp(cpVal,tpInit1->cpName,iLen)) {
	    cpStr = "type";
	    break;
	  }
	}
      }
      if (!cpStr) {
	iLen = strlen(cpVal);
	for (tpInit2 = taInit2; tpInit2->cpName; tpInit2++) {
	  if (!strncmp(cpVal,tpInit2->cpName,iLen)) {
	    cpStr = "format";
	    break;
	  }
	}
      }
      if (!cpStr) AConvertUsage("Missing '=' in option: %s",cpStr);
    }

    /* See if we can match the option */
    switch (cpStr[0]) {
      case 'b':	tpData->iBits	= atoi(cpVal); break;
      case 'c': ParseChans(tpC,cpVal,bInp); break;
      case 'e':	tpData->fExpon	= (float)atof(cpVal); break;
      case 'f':
	/* Find a matching format */
	iLen = strlen(cpVal);
	for (tpInit2 = taInit2; tpInit2->cpName; tpInit2++) {
	  if (!strncmp(cpVal,tpInit2->cpName,iLen)) break;
	}
	if (!tpInit2->cpName) AConvertUsage("Unknown format type: %s",cpVal);
	tpData->eFormat	= tpInit2->eFormat;
	break;
      case 'i':	tpData->iIncr	= atoi(cpVal); break;
      case 'p':	tpData->iCoeffs	= atoi(cpVal); break;
      case 'r':	tpData->iFreq	= (int)(atof(cpVal) * 1000.0); break;
      case 's':	tpData->iWindow	= atoi(cpVal); break;
      case 't':
	/* Find a matching type */
	iLen = strlen(cpVal);
	for (tpInit1 = taInit1; tpInit1->cpName; tpInit1++) {
	  if (!strncmp(cpVal,tpInit1->cpName,iLen)) break;
	}
	if (!tpInit1->cpName) AConvertUsage("Unknown IO type: %s",cpVal);
	if (bInp) {
	  if (!tpInit1->bInp)
	    AConvertUsage("Can't use %s for input",tpInit1->cpName);
	} else {
	  if (!tpInit1->bOut)
	    AConvertUsage("Can't use %s for output",tpInit1->cpName);
	}
	tpData->eType	= tpInit1->eType;
	tpData->iBits	= tpInit1->iBits;
	break;
      default: AConvertUsage("Unknown I/O option (%s)",cpStr);
    }
  }

  /* Now make sure everything is legal */
  if (tpData->eType == ADPCM)
  {
    if (tpData->iBits != 4 && tpData->iBits != 3 && tpData->iBits != 2)
      AConvertUsage("ADPCM requires 2,3 or 4 bit width");
  }
  else if (tpData->eType == IMAPCM)
  {
    if (tpData->iBits != 4) AConvertUsage("IMAPCM requires 4 bit width");
  }
  else if (tpData->eType == ULAW8 || tpData->eType == ALAW8) 
  {
    if (tpData->iBits != 8)
      AConvertUsage("ULAW/ALAW require 8 bit data width");
  }
  else if (tpData->eType == PLP || tpData->eType == RASTA ||
  	   tpData->eType == CEPSTRUM || tpData->eType == MELCEP) {
    tpData->iBits = COEFFBITS(tpData->iCoeffs);
  }
  else if (tpData->iBits != 16)
    AConvertUsage("LINEAR modes requires 16 bit width");
  
  tpData->iBytes = Bytes_Per_Unit(tpData->iBits);
  tpData->iSamps = (tpData->iBytes << 3) / tpData->iBits;
}

static void Init(			/* Initialize values by name */
  Ctxt*		tpC,			/* Current context */
  char*		cpName,			/* Item to set */
  char* 	cpVal)			/* Value to set */
{
  int		iLen;

  if (!cpName) AConvertUsage("Name parameter missing");
  if (!cpVal)  AConvertUsage("Value parameter missing");

  iLen = strlen(cpName);
  if (!strncmp(cpName,"agc",iLen)) 	   tpC->bAGC	    = atoi(cpVal);
  else if (!strncmp(cpName,"byteSwap",iLen)) tpC->bFlip     = atoi(cpVal);
  else if (!strncmp(cpName,"copy",iLen))   tpC->bCopyHeader = atoi(cpVal);
  else if (!strncmp(cpName,"filter",iLen)) tpC->iFilterSize = atoi(cpVal);
  else if (!strncmp(cpName,"gain",iLen))   tpC->dGain       = atof(cpVal);
  else if (!strncmp(cpName,"input",iLen))  ParseIO(tpC,cpVal,bTRUE);
  else if (!strncmp(cpName,"jump",iLen))   tpC->iSkipHeader = atoi(cpVal);
  else if (!strncmp(cpName,"load",iLen))   tpC->bPreLoad    = atoi(cpVal);
  else if (!strncmp(cpName,"max",iLen))    tpC->iMaxInp	    = atoi(cpVal);
  else if (!strncmp(cpName,"output",iLen)) ParseIO(tpC,cpVal,bFALSE);
  else if (!strncmp(cpName,"stats",iLen))  tpC->bDoStats    = atoi(cpVal);
  else if (!strncmp(cpName,"vox",iLen))	   tpC->bDoVox	    = atoi(cpVal);
  else AConvertUsage("Unknown option: %s",cpName);
}

int AConvertInit(			/* Initialize values by name */
  AConvertCtxt*	tpACC,			/* Current context */
  char*		cpName,			/* Item to set */
  char* 	cpVal)			/* Value to set */
{
  Ctxt*		tpC = (Ctxt*)tpACC;

  INITUSAGE(-1);
  Init(tpC,cpName,cpVal);
  RETURN(0);
}

extern char*		optarg;
extern int		optind;
extern int		opterr;
extern int		optopt;

int AConvertParse(			/* Parse argc/argv */
  Ctxt*		tpC,			/* Current context */
  int 		argc,
  char** 	argv)
{
  int		iChr;

  IFUSAGE {
    fprintf(stderr,"\n");
    fprintf(stderr,"ERROR   : %s\n",AConvertError());
    fprintf(stderr,"Usage   : aconvert [options] [INP] [OUT]\n\
======= Options:\n\
 -a             Auto Gain Control (AGC) -b         Byte swap input data\n\
 -c             Copy jumped raw header  -f 10      Order of LP filter\n\
 -g 1.0         Gain multiplier         -j 0       Jump over header bytes\n\
 -l             Load entire file        -m 0       Max inp cnt (0=til EOF)\n\
 -s             Output stats on stderr  -v         Vox input\n\
======= INP/OUT Format: name[,option...]\n\
 name           -=stdin/stdout or file  c[hans]=1[:1] Channel mix specs\n\
 b[its]=8       Bits per sample         p[arams]=11   ener+ptp+zc+coeffs/vec\n\
 i[ncr]=160     FFT Samples/increment   [r[ate]=]8.0  Frequency in khz\n\
 s[amples]=320  FFT Samples/vector      e[xpon]=0.6   FFT Peak enhancement\n\
 [f[ormat]=]raw ascii,htk,raw,sphere    [t[ype]=]ulaw:\n\
   adpcm    I/O  2,3 or [4] bits        alaw     I/O  8 bit data\n\
   cepstrum  O   ener+ptp+zc+coeffs     linear   I/O  linear 16 bit data\n\
   melcep    O   ener+ptp+zc+coeffs     plp       O   ener+ptp+zc+coeffs\n\
   rasta     O   ener+ptp+zc+coeffs     sweep     I   fake for -m units\n\
   ulaw     I/O  ULAW 8 bit data        white     I   fake for -m units\n\
   tone      I   fake for -m units      ima      I/O  4bit adpcm algorithm\n\
======= Example:\n\
  aconvert inp.snd,t=ulaw,r=8 out.au,t=linear,r=44.1,c=1:1:1\n");
      RETURN(-1);
    }

  /* Parse command line options */
  while ((iChr = getopt(argc,argv,"abcf:g:j:lm:sv,:")) != EOF) {
    if (iChr == ',') {
      optind--;
      break;
    }
    switch (iChr) {
      case 'a':	Init(tpC,"a","1"); 	break;
      case 'b': Init(tpC,"b","1");	break;
      case 'c':	Init(tpC,"c","1");	break;
      case 'f':	Init(tpC,"f",optarg);	break;
      case 'g':	Init(tpC,"g",optarg);	break;
      case 'j':	Init(tpC,"j",optarg);	break;
      case 'l': Init(tpC,"l","1");	break;
      case 'm':	Init(tpC,"m",optarg);	break;
      case 's':	Init(tpC,"s","1");	break;
      case 'v':	Init(tpC,"v","1");	break;
      default : AConvertUsage("Unknown option: -%c",iChr);
    }
  }

  /* Gather up file names */
  if (optind < argc) Init(tpC,"i",argv[optind++]);
  if (optind < argc) Init(tpC,"o",argv[optind++]);
  if (optind < argc) AConvertUsage("Too many arguments");

  RETURN(0);
}

static char* AllocInit(AEncodeType eType,int* ipBits)
{
  switch (eType) {
    case MU255:		*ipBits = 8;	return "ulaw";
    case ALAW:		*ipBits = 8;	return "alaw";
    case LIN16:		*ipBits = 16;	return "linear";
    case LIN32:		*ipBits = 32;	return "linear";
    case CRLADPCM2:	*ipBits = 2;	return "adpcm";
    case CRLADPCM3:	*ipBits = 3;	return "adpcm";
    case CRLADPCM4:	*ipBits = 4;	return "adpcm";
    case IMA:		*ipBits = 4;	return "ima";
    default:		AConvertUsage("Unsupported encoding (%d)",eType);
  }
  return NULL;
}

AConvertCtxt* AConvertAllocInit(	/* Allocate/init a new context */
  AEncodeType	eInpType,		/* Input AF audio type */
  int		iInpFreq,		/* Input frequency in samples/sec */
  int		iInpChans,		/* Number of input channels */
  AEncodeType	eOutType,		/* Output AF audio type */
  int		iOutFreq,		/* Output frequency in samples/sec */
  int		iOutChans)		/* Number of output channels */
{
  int		iInpBits;
  int		iOutBits;
  char*		cpInpType;
  char*		cpOutType;
  char		caStr[256];
  char		caStr2[20];
  AConvertCtxt*	tpACC	= AConvertAlloc();
  Ctxt*		tpC     = (Ctxt*)tpACC;

  /* Set up an error context */
  INITUSAGE(NULL);

  /* Make sure we got the data */
  if (!tpC) RETURN(tpC);

  /* Figure out the parameter translation */
  cpInpType = AllocInit(eInpType,&iInpBits);
  cpOutType = AllocInit(eOutType,&iOutBits);

  /* Set up and execute command strings */
  sprintf(caStr,"-BUFFER-,type=%s,bits=%d,rate=%.4f,format=raw,chans=%d",
	    cpInpType,iInpBits,(double)iInpFreq/1000.0,iInpChans);
  Init(tpC,"input",caStr);

  /* Handle default channel conversions */
  if (iOutChans == 2 && iInpChans == 1) strcpy(caStr2,"1:1:1");
  else if (iOutChans == 1 && iInpChans == 2) strcpy(caStr2,"2:12");
  else sprintf(caStr2,"%d",iOutChans);

  sprintf(caStr,"-BUFFER-,type=%s,bits=%d,rate=%.4f,format=raw,chans=%s",
	    cpOutType,iOutBits,(double)iOutFreq/1000.0,caStr2);
  Init(tpC,"output",caStr);

  RETURN(tpACC);
}

