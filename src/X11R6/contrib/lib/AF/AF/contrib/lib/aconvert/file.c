static char rcsid[] = "$RCSfile: file.c,v $, $Revision: 1.11 $, $Date: 1994/06/03 17:13:28 $";

/************************************************************************
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
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
 * FILE NAME		: $RCSfile: file.c,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:13:28 $
 * REVISION		: $Revision: 1.11 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: file.c,v $
 * Revision 1.11  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.10  1994/06/01  19:30:00  jg
 * change header file name to meet PC standards
 *
 * Revision 1.9  1993/12/08  23:23:39  wecker
 * Added new AGC code and preload
 *
 * Revision 1.9  1993/12/03  21:47:14  wecker
 * Added scaling of Energy and PTP
 *
 * Revision 1.8  1993/10/25  20:33:53  wecker
 * *** empty log message ***
 *
 * Revision 1.7  1993/10/20  18:09:30  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.6  1993/10/20  17:33:52  wecker
 * Added copyrights
 *
 * Revision 1.5  1993/10/19  23:17:29  wecker
 * Changed to Imakefile build
 *
 * Revision 1.4  1993/10/15  16:13:26  wecker
 * Added PTP and ZC to coefficients for PLP and RASTA
 *
 * Revision 1.5  1993/08/31  20:44:50  wecker
 * Added IMA code
 *
 * Revision 1.4  1993/08/30  17:26:05  wecker
 * Rewrote setjmp code to stack jmpbufs
 *
 * Revision 1.3  1993/08/28  16:17:51  wecker
 * Added TONE type and split tones/sweep. Fixed ASCII files
 *
 * Revision 1.2  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.1  1993/08/26  12:17:06  wecker
 * Initial revision
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

static int GetSphereInt(
  struct header_t* tpHeader,
  char* 	   cpName)
{
  int	iType,iSize;
  long	lData;

  if (sp_get_field(tpHeader,cpName,&iType,&iSize) >= 0 &&
      iType == T_INTEGER &&
      sp_get_data(tpHeader,cpName,(char*)&lData,&iSize) >= 0) return lData;
  return -1;
}

static void PutSphereInt(
  struct header_t* tpHeader,
  char* 	   cpName,
  int		   iVal)
{
  long	lData = iVal;

  if (sp_add_field(tpHeader,cpName,T_INTEGER,(char*)&lData) < 0)
    AConvertUsage("Can't add field to output header");
}

static void PutSphereStr(
  struct header_t* tpHeader,
  char* 	   cpName,
  char*		   cpVal)
{
  if (sp_add_field(tpHeader,cpName,T_STRING,cpVal) < 0)
    AConvertUsage("Can't add field to output header");
}

static void WriteHTKHeader(Ctxt* tpC,int iNumSamples,FILE* fpOut)
{
  HTKheader	tHeader;
  HTKsampKind	eSampKind;

  /* Set up the header info (fill in NumSamples later) */
  tHeader.iNumSamples  = iNumSamples;
  tHeader.iSampPeriod  = 10000000 / tpC->tOut.iFreq;
  if (tpC->tOut.iBytes == 1 || tpC->tOut.iBytes == 2) {
    eSampKind 	  	= WAVEFORM;
    tHeader.iSampKind	= eSampKind;
    tHeader.iSampSize   = tpC->tOut.iBytes;
    tHeader.iSampPeriod = 10000000 / tpC->tOut.iFreq;
  } else {
    switch (tpC->tOut.eType) {
      case RASTA:	eSampKind = LPC;	break;
      case PLP:		eSampKind = LPC;	break;
      case CEPSTRUM:	eSampKind = LPCEPSTRA;	break;
      case MELCEP:	eSampKind = MFCC;	break;
      default: AConvertUsage("Unsupported output HTK type");
    }
    tHeader.iSampKind	 = eSampKind;
    tHeader.iSampSize    = tpC->tOut.iCoeffs * 4;
    tHeader.iSampPeriod *= tpC->tOut.iIncr;
  }

  /* write out the header */
  if (fwrite(&tHeader,sizeof(tHeader),1,fpOut) != 1)
    AConvertUsage("Can't write HTK header");
}

static void InitIO(Ctxt* tpC,FILE* fpInp,FILE* fpOut)
{
  struct header_t* tpHeader = NULL;

  if (tpC->tInp.eFormat == ASCII) {
    if (tpC->iSkipHeader) AConvertUsage("No headers allowed in ASCII");

  /* Handle header commands on raw input */
  } else if (tpC->tInp.eFormat == RAW) {

    /* Make sure copy is legal */
    if (tpC->bCopyHeader) {
      if (tpC->tOut.eFormat != RAW)
	AConvertUsage("Can only copy header to a RAW output file");
    }

    /* skip any input header bytes */
    if (tpC->iSkipHeader) {
      u_char	ucaBuf[2048];

      while (tpC->iSkipHeader)
      {	
	int iCnt = sizeof(ucaBuf) >= tpC->iSkipHeader ? tpC->iSkipHeader
					      	      : sizeof(ucaBuf);

	iCnt = fread(ucaBuf,1,iCnt,fpInp);
	if (iCnt <= 0) AConvertUsage("EOF while reading header");

	/* Copy the header if desired */
	if (tpC->bCopyHeader == bTRUE) fwrite(ucaBuf,1,iCnt,fpOut);

	tpC->iSkipHeader -= iCnt;
      }
    }

  /* Handle sphere header input */
  } else if (tpC->tInp.eFormat == SPHERE) {
    int		     iData;
    char*	     cpErrMsg;

    tpHeader = sp_open_header(fpInp,TRUE,&cpErrMsg);
    if (tpHeader == HDRNULL) AConvertUsage("Can't read header (%s)",cpErrMsg);

    /* Get fields of interest */
    if (GetSphereInt(tpHeader,"channel_count") == 2) {
      tpC->tInp.iSrcChans 	= 2;
      tpC->tInp.iDstChans 	= 2;
      tpC->tInp.ucaDstMsk[0]	= 1;
      tpC->tInp.ucaDstMsk[1]	= 2;
      tpC->tInp.ucaDstCnt[0]	= 1;
      tpC->tInp.ucaDstCnt[1]	= 1;
    }
    if ((iData = GetSphereInt(tpHeader,"sample_rate")) >= 0) {
      tpC->tInp.iFreq = iData;
      tpC->dRho = (double)tpC->tOut.iFreq / (double)tpC->tInp.iFreq;
    }
    if ((iData = GetSphereInt(tpHeader,"sample_n_bytes")) >= 0) {
      tpC->tInp.iBytes = iData;
      tpC->tInp.iSamps = 1;
      if (iData == 2 && tpC->tInp.eType == ULAW8) tpC->tInp.eType = LINEAR;
    }
    if ((iData = GetSphereInt(tpHeader,"sample_sig_bits")) >= 0)
      tpC->tInp.iBits  = iData;

    if (sp_close_header(tpHeader) < 0)
      AConvertUsage("Error closing input header");

  /* Handle htk header input */
  } else if (tpC->tInp.eFormat == HTK) {
    HTKheader	tHeader;
    HTKsampKind	eSampKind;

    /* read in the header */
    if (fread(&tHeader,sizeof(tHeader),1,fpInp) != 1)
      AConvertUsage("Can't read HTK header");

    /* make sure this is a waveform */
    eSampKind = tHeader.iSampKind;
    if (eSampKind != WAVEFORM)
      AConvertUsage("Can only read HTK waveform files (%o)",
		      tHeader.iSampKind);

    /* set whatever we can */
    tpC->tInp.iFreq  = 10000000 / tHeader.iSampPeriod;
    tpC->tInp.iBytes = tHeader.iSampSize;
    tpC->tInp.iBits  = tHeader.iSampSize * 8;
    tpC->tInp.iSamps = 1;
    tpC->iMaxInp     = tHeader.iNumSamples;

  /* Unknown input header type */
  } else AConvertUsage("Unknown input header type");

  /* Take care of output header creation (RAW already done) */
  if (tpC->tOut.eFormat == RAW || tpC->tOut.eFormat == ASCII) return;

  if (tpC->tOut.eFormat == SPHERE) {
    long	lBytes,lData;

    tpHeader = sp_create_header();
    if (tpHeader == HDRNULL) AConvertUsage("Couldn't create output header");
    PutSphereInt(tpHeader,"channel_count",tpC->tOut.iDstChans);
    PutSphereInt(tpHeader,"sample_rate",tpC->tOut.iFreq);
    PutSphereInt(tpHeader,"sample_n_bytes",tpC->tOut.iBytes);
    PutSphereStr(tpHeader,"sample_byte_format","01");
    PutSphereInt(tpHeader,"sample_sig_bits",tpC->tOut.iBits);
    if (sp_write_header(fpOut,tpHeader,&lBytes,&lData) < 0)
      AConvertUsage("Can't write output header");
    if (sp_close_header(tpHeader) < 0)
      AConvertUsage("Error closing output header");

  /* Handle htk header output */
  } else if (tpC->tOut.eFormat == HTK) WriteHTKHeader(tpC,0,fpOut);
  else AConvertUsage("Unknown output header type");
}

int AConvertFiles(
  int		argc,
  char**	argv)
{
  AConvertCtxt* tpACC;
  Ctxt*		tpC;
  FILE*		fpInp;
  FILE*		fpOut;
  int		iCnt;
  u_char*	ucpSrc;
  u_char*	ucpDst;
  int		iBufferSize = BUFFERSIZE;

  /* Set up an error context */
  INITUSAGE(-1);

  /* Make sure we have a context to work with */
  tpACC		= AConvertAlloc();
  if (!tpACC) RETURN(-1);
  tpC		= (Ctxt*)tpACC;

  /* Parse the command line arguments */
  if (AConvertParse(tpC,argc,argv)) RETURN(-1);

  /* Open any needed files */
  if (tpC->cpInpFile == NULL ||
      (tpC->cpInpFile[0] == '-' && tpC->cpInpFile[1] == '\0')) fpInp = stdin;
  else {
    struct stat sStat;

    fpInp = fopen(tpC->cpInpFile,"r");
    if (!fpInp) AConvertUsage("Can't open input file: %s",tpC->cpInpFile);
    if (tpC->bPreLoad) {
      if (fstat(fileno(fpInp),&sStat))
	AConvertUsage("Can't stat input file: %s",tpC->cpInpFile);
      iBufferSize  = sStat.st_size;
      iBufferSize /= tpC->tInp.iBytes * tpC->tInp.iSrcChans;
    }
  }
  if (tpC->cpOutFile == NULL ||
      (tpC->cpOutFile[0] == '-' && tpC->cpOutFile[1] == '\0')) fpOut = stdout;
  else {
    fpOut = fopen(tpC->cpOutFile,"w");
    if (!fpOut) AConvertUsage("Can't open output file: %s",tpC->cpOutFile);
  }

  /* Init Context */
  if (AConvertInitChans(tpC) == -1) RETURN(-1);

  /* Init Headers */
  InitIO(tpC,fpInp,fpOut);

  /* Do data */
  ucpSrc = (u_char*)malloc(tpC->tInp.iBytes*tpC->tInp.iSrcChans*iBufferSize);
  if (!ucpSrc) AConvertUsage("Can't allocate input file buffer");
  while (1) {
    /* Special case "fake" inputs */
    if (tpC->tInp.eType == SWEEP || tpC->tInp.eType == WHITE ||
	tpC->tInp.eType == TONE)
      	 iCnt = iBufferSize;

    /* Take care of ASCII input */
    else if (tpC->tInp.eFormat == ASCII) {
      int	i;
      int	iVal;
      float	fVal;
      u_char*	ucpBuf = ucpSrc;

      for (iCnt=0; iCnt<iBufferSize; iCnt++) {
	int	iVal;
	float	fVal;

	if (tpC->tInp.iBytes == 1) {
	  for (i=0; i<tpC->tInp.iSrcChans; i++) {
	    if (fscanf(fpInp,"%x",&iVal) != 1) break;
	    *ucpBuf++ = (u_char)iVal;
	  }
	  if (i != tpC->tInp.iSrcChans) break;
	}
	else if (tpC->tInp.iBytes == 2) {
	  for (i=0; i<tpC->tInp.iSrcChans; i++) {
	    if (fscanf(fpInp,"%d",&iVal) != 1) break;
	    *((short*)ucpBuf) = (short)iVal;
	    ucpBuf += 2;
	  }
	  if (i != tpC->tInp.iSrcChans) break;
	}
	else {
	  for (i=0; i<tpC->tInp.iCoeffs * tpC->tInp.iSrcChans; i++) {
	    if (fscanf(fpInp,"%f",&fVal) != 1) break;
	    *((float*)ucpBuf) = fVal;
	    ucpBuf += sizeof(float);
	  }
	  if (i != tpC->tInp.iCoeffs * tpC->tInp.iSrcChans) break;
	}
      }
    }

    /* Handle everyone else */
    else iCnt = fread(ucpSrc,tpC->tInp.iBytes * tpC->tInp.iSrcChans,
			iBufferSize,fpInp);

    if (iCnt == 0) break;
    if (iCnt  < 0) AConvertUsage("Error reading input file");
    iCnt = AConvert(tpACC,iCnt,ucpSrc,&ucpDst);
    if (iCnt == 0) break;
    if (iCnt <  0) RETURN(iCnt);

    /* Take care of ASCII output */
    if (tpC->tOut.eFormat == ASCII) {
      int	i,j,k;
      int	iVal;
      float	fVal;
      u_char*	ucpBuf = ucpDst;

      for (i=0; i<iCnt; i++) {
	int	iVal;
	float	fVal;

	for (j=0; j<tpC->tOut.iDstChans; j++) {
	  if (tpC->tOut.iBytes == 1) {
	    iVal    = *ucpBuf++;
	    if (j) fprintf(fpOut," %x",iVal);
	    else   fprintf(fpOut,"%x",iVal);
	  }
	  else if (tpC->tOut.iBytes == 2) {
	    iVal    = *((short*)ucpBuf);
	    ucpBuf += 2;
	    if (j) fprintf(fpOut," %d",iVal);
	    else   fprintf(fpOut,"%d",iVal);
	  }
	  else {
	    for (k=0; k<tpC->tOut.iCoeffs; k++) {
	      fVal    = *((float*)ucpBuf);
	      ucpBuf += sizeof(float);
	      if (k) fprintf(fpOut," %f",fVal);
	      else   fprintf(fpOut,"%f",fVal);
	    }
	    fprintf(fpOut,"\n");
	  }
	}
	if (tpC->tOut.iBytes <= 2) fprintf(fpOut,"\n");
      }
    }

    /* Take care of everyone else */
    else if (fwrite(ucpDst,tpC->tOut.iBytes * tpC->tOut.iDstChans,iCnt,fpOut)
	    != iCnt) AConvertUsage("Error writing output file");
  }

  /* Do Stats */
  AConvertStats(tpACC);

  /* Close Files */

  if (fpOut && tpC->tOut.eFormat == HTK) {
    rewind(fpOut);
    WriteHTKHeader(tpC,tpC->iOutSamps,fpOut);
  }
  if (fpInp != stdin)  fclose(fpInp);
  if (fpOut != stdout) fclose(fpOut);

  AConvertClose(tpACC);

  RETURN(0);
}

