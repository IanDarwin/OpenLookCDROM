static char rcsid[] = "$RCSfile: chans.c,v $, $Revision: 1.12 $, $Date: 1994/06/03 17:13:28 $";

/************************************************************************
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation, 
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
 * FILE NAME		: $RCSfile: chans.c,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:13:28 $
 * REVISION		: $Revision: 1.12 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: chans.c,v $
 * Revision 1.12  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.11  1994/06/01  19:30:00  jg
 * change header file name to meet PC standards
 *
 * Revision 1.10  1993/10/25  20:33:53  wecker
 * *** empty log message ***
 *
 * Revision 1.9  1993/10/20  18:09:30  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.8  1993/10/20  17:33:52  wecker
 * Added copyrights
 *
 * Revision 1.7  1993/10/19  23:17:29  wecker
 * Changed to Imakefile build
 *
 * Revision 1.6  1993/10/15  16:13:26  wecker
 * Added PTP and ZC to coefficients for PLP and RASTA
 *
 * Revision 1.6  1993/08/30  17:26:05  wecker
 * Rewrote setjmp code to stack jmpbufs
 *
 * Revision 1.5  1993/08/28  18:28:17  wecker
 * Fixed bugs in ADPCM buffer sizes and 3bit unit counting
 *
 * Revision 1.4  1993/08/28  16:17:51  wecker
 * Added TONE type and split tones/sweep. Fixed ASCII files
 *
 * Revision 1.3  1993/08/27  22:44:27  wecker
 * Fixed problems with filters and multi-channels
 *
 * Revision 1.2  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.1  1993/08/25  19:37:50  wecker
 * Initial revision
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

static void DeMuxChans(
  Ctxt*		tpC,
  int 		iCnt,
  u_char* 	ucpSrc,
  u_char* 	ucpDst)
{
  int		iChan,iByte;
  u_char*	ucpaBuf[8];

  /* Set up output pointer channels */
  for (iChan=0; iChan<tpC->tInp.iSrcChans; iChan++)
    ucpaBuf[iChan] = &ucpDst[iChan * iCnt * tpC->tInp.iBytes];

  /* De-Mux the channels */
  while (iCnt--)
    for (iChan=0; iChan<tpC->tInp.iSrcChans; iChan++)
      for (iByte=0; iByte < tpC->tInp.iBytes; iByte++)
	*ucpaBuf[iChan]++ = *ucpSrc++;
}

static void MixChans(
  Data*		tpData,
  int		iChan,
  int 		iCnt,
  short* 	spSrc,
  short* 	spDst)
{
  int		iSrc,iDst;
  short*	spaBuf[8];

  /* Set up output pointer channels */
  for (iDst=0; iDst<tpData->iDstChans; iDst++)
    spaBuf[iDst] = &spDst[iDst * iCnt];

  /* mix in/out the desired channels */
  while (iCnt--) {
    for (iDst=0; iDst<tpData->iDstChans; iDst++) {
      if (iChan == 0) *spaBuf[iDst] = 0;
      if (tpData->ucaDstCnt[iDst] && (tpData->ucaDstMsk[iDst] & 1<<iChan))
	*spaBuf[iDst] += *spSrc / tpData->ucaDstCnt[iDst];
      spaBuf[iDst]++;
    }
    spSrc++;
  }
}

static void MuxChans(
  Ctxt*		tpC,
  int		iChan,
  int 		iCnt,
  u_char* 	ucpSrc,
  u_char* 	ucpDst)
{
  int	iByte;
  int	iIncr = (tpC->tOut.iDstChans-1) * tpC->tOut.iBytes;

  /* Point to the first position */
  ucpDst += iChan * tpC->tOut.iBytes;

  /* Mux the channel */
  while (iCnt--) {
    for (iByte=0; iByte < tpC->tOut.iBytes; iByte++)
      *ucpDst++ = *ucpSrc++;
    ucpDst += iIncr;
  }
}

static int		iBufLen		= 0;
static int		iMult		= 1;
static int		iUnitSize	= 1;
static Buf*		tpBuf1		= NULL;
static Buf*		tpBuf2		= NULL;

int AConvertInitChans(			/* return iMaxChans */
  Ctxt*		tpC)			/* Current Context */
{
  int			iMaxChans	= 1;

  INITUSAGE(-1);

  if (tpC->tInp.iSrcChans > iMaxChans) iMaxChans = tpC->tInp.iSrcChans;
  if (tpC->tInp.iDstChans > iMaxChans) iMaxChans = tpC->tInp.iDstChans;
  if (tpC->tOut.iSrcChans > iMaxChans) iMaxChans = tpC->tOut.iSrcChans;
  if (tpC->tOut.iDstChans > iMaxChans) iMaxChans = tpC->tOut.iDstChans;

  /* Special case single channel */
  if (iMaxChans == 1) {
    if (tpC->bFirstTime == bTRUE && AConvert0(tpC,0)) RETURN(-1);
    RETURN(iMaxChans);
  }
  
  /* Make sure multi-channels are init'd */
  if (tpC->tapCtxt[0] == NULL) {
    int		i,j,iTmp;

    if (iMaxChans > 8) AConvertUsage("Can't handle more than 8 channels");
    for (i=0; i<iMaxChans; i++) {
      tpC->tapCtxt[i] = AConvertAlloc();
      if (!tpC->tapCtxt[i]) RETURN(-1);
      *tpC->tapCtxt[i] 			= *tpC;
      tpC->tapCtxt[i]->bFirstTime	= bTRUE;
      for (j=0; j<i; j++) tpC->tapCtxt[i]->tapCtxt[j] = NULL;

      if (AConvert0(tpC->tapCtxt[i],i)) RETURN(-1);
    }

    /* Make sure we have multipliers for buffer sizes */
    if (tpC->tInp.iFreq > tpC->tOut.iFreq)
         iTmp = (tpC->tInp.iFreq / tpC->tOut.iFreq) + 1;
    else iTmp = (tpC->tOut.iFreq / tpC->tInp.iFreq) + 1;
    if (iTmp > iMult) iMult = iTmp;
    iTmp = tpC->tInp.iSamps * 2;
    if (tpC->tInp.iBytes > iTmp)	iTmp		= tpC->tInp.iBytes;
    if (iTmp > iUnitSize) 		iUnitSize	= iTmp;
    iTmp = tpC->tOut.iSamps * 2;
    if (tpC->tOut.iBytes > iTmp)	iTmp		= tpC->tOut.iBytes;
    if (iTmp > iUnitSize) 		iUnitSize	= iTmp;

    if (!tpBuf1) {
      tpBuf1		= (Buf*)malloc(sizeof(Buf));
      if (!tpBuf1) AConvertUsage("Can't alloc initial buffer union ");
      tpBuf1->ucp	= NULL;
      tpBuf2		= (Buf*)malloc(sizeof(Buf));
      if (!tpBuf2) AConvertUsage("Can't alloc initial buffer union 2");
      tpBuf2->ucp	= NULL;
    }
  }
  RETURN(iMaxChans);
}

int AConvert(				/* -1=ERROR 0=MAXINP else DstCnt */
  AConvertCtxt* tpACC,			/* Current context */
  int		iCnt,			/* # of units */
  void*		vpSrc,			/* Buffer of data */
  void*		vppDst)			/* Output buffer pointer */
{
  int			i;
  u_char*		ucpSrc 		= (u_char*)vpSrc;
  u_char**		ucppDst		= (u_char**)vppDst;
  Ctxt*			tpC    		= (Ctxt*)tpACC;
  int			iPrvCnt;
  int			iOutCnt;
  u_char*		ucpTmp;
  short*		spTmp;
  int			iMaxChans	= AConvertInitChans(tpC);

  INITUSAGE(-1);

  /* Make sure we don't start with an error */
  if (iMaxChans < 0) RETURN(-1);

  /* Special case the single channel case */
  if (iMaxChans == 1) {
    short*		spTmp1;
    short*		spTmp2;

    iCnt = AConvert1(tpC,iCnt,vpSrc,&spTmp1);
    if (iCnt <= 0) RETURN(iCnt);
    iCnt = AConvert2(tpC,iCnt,spTmp1,&spTmp2);
    if (iCnt <= 0) RETURN(iCnt);
    iCnt = AConvert3(tpC,iCnt,spTmp2,ucppDst);
    RETURN(iCnt);
  }

  /* Make sure the buffers are big enough */
  if (iBufLen < iCnt*iMult*iUnitSize*iMaxChans) {
    iBufLen	= iCnt*iMult*iUnitSize*iMaxChans;
    if (!tpBuf1->ucp) tpBuf1->ucp = (u_char*)malloc(iBufLen);
    else	      tpBuf1->ucp = (u_char*)realloc(tpBuf1->ucp,iBufLen);
    if (!tpBuf1->ucp) AConvertUsage("Can't alloc initial buffer 1");
    if (!tpBuf2->ucp) tpBuf2->ucp = (u_char*)malloc(iBufLen);
    else	      tpBuf2->ucp = (u_char*)realloc(tpBuf2->ucp,iBufLen);
    if (!tpBuf2->ucp) AConvertUsage("Can't alloc initial buffer 2");
  }

  /*De-Mux the input channels*/
  DeMuxChans(tpC,iCnt,ucpSrc,tpBuf1->ucp);

  /* Convert each channel to shorts */
  for (i=0; i<tpC->tInp.iSrcChans; i++) {
    iOutCnt = AConvert1(tpC->tapCtxt[i],iCnt,
		&tpBuf1->ucp[i*iCnt*tpC->tInp.iBytes],&spTmp);
    if (iOutCnt <= 0) RETURN(iOutCnt);
    if (i == 0) iPrvCnt = iOutCnt;
    else if (iOutCnt != iPrvCnt) AConvertUsage("Differing channel sizes");
    MixChans(&tpC->tInp,i,iOutCnt,spTmp,tpBuf2->sp);
  }
  iCnt = iOutCnt;

  /*Perform conversions on each channel and re-mix*/
  for (i=0; i<tpC->tInp.iDstChans; i++) {
    iOutCnt = AConvert2(tpC->tapCtxt[i],iCnt,&tpBuf2->sp[i * iCnt],&spTmp);
    if (iOutCnt <= 0) RETURN(iOutCnt);
    if (i == 0) iPrvCnt = iOutCnt;
    else if (iOutCnt != iPrvCnt) AConvertUsage("Differing channel sizes");
    MixChans(&tpC->tOut,i,iOutCnt,spTmp,tpBuf1->sp);
  }
  iCnt = iOutCnt;

  /* Now do the back end encoding */
  for (i=0; i<tpC->tOut.iDstChans; i++) {
    iOutCnt = AConvert3(tpC->tapCtxt[i],iCnt,&tpBuf1->sp[i * iCnt],&ucpTmp);
    if (iOutCnt <= 0) RETURN(iOutCnt);
    if (i == 0) iPrvCnt = iOutCnt;
    else if (iOutCnt != iPrvCnt) AConvertUsage("Differing channel sizes");
    MuxChans(tpC,i,iOutCnt,ucpTmp,tpBuf2->ucp);
  }
  iCnt = iOutCnt;

  /* Return the results */
  *ucppDst = tpBuf2->ucp;
  RETURN(iCnt);
}

