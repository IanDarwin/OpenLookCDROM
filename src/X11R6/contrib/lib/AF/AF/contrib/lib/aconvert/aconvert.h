/*static char rcsid[] = "$RCSfile: aconvert.h,v $, $Revision: 1.13 $, $Date: 1994/06/03 17:13:28 $";*/

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
 * FILE NAME		: $RCSfile: aconvert.h,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:13:28 $
 * REVISION		: $Revision: 1.13 $
 * DESCRIPTION		: General purpose audio conversion routines
 *
 * REVISION HISTORY
 *
 * $Log: aconvert.h,v $
 * Revision 1.13  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.12  1993/11/05  19:27:32  tml
 * need aflib header.
 *
 * Revision 1.11  1993/10/26  21:38:05  tml
 * endif fix
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
 * Revision 1.7  1993/10/19  23:18:44  wecker
 * Switched to Imake build
 *
 * Revision 1.6  1993/10/15  16:13:26  wecker
 * Added PTP and ZC to coefficients for PLP and RASTA
 *
 * Revision 1.6  1993/08/30  15:24:52  wecker
 * Added man pages
 *
 * Revision 1.5  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.4  1993/08/23  20:45:12  wecker
 * Added Rasta-PLP code
 *
 * Revision 1.3  1993/08/21  20:07:50  wecker
 * Added SPHERE file format
 *
 * Revision 1.2  1993/08/20  15:27:33  wecker
 * New callable interface
 *
 * Revision 1.1  1993/08/10  22:46:17  wecker
 * Initial revision
 *
 *
 ************************************************************************/

#ifndef _ACONVERT_H_
#define _ACONVERT_H_

#include <sys/types.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>

typedef void	AConvertCtxt;		/* Opaque handle */

/*************************** Routines **************************************/

char* AConvertError();			/* Return message from last error */

AConvertCtxt* AConvertAlloc();		/* Allocate a new context */

AConvertCtxt* AConvertAllocInit(	/* Allocate/init a new context */
  AEncodeType	eInpType,		/* Input AF audio type */
  int		iInpFreq,		/* Input frequency in samples/sec */
  int		iInpChans,		/* Number of input channels */
  AEncodeType	eOutType,		/* Output AF audio type */
  int		iOutFreq,		/* Output frequency in samples/sec */
  int		iOutChans);		/* Number of output channels */

int AConvertInit(			/* Parse one argument */
  AConvertCtxt*	tpACC,			/* Current context */
  char*		cpName,			/* Name of item (see below) */
  char*		cpVal);			/* Value for item (see below) */

/* Legal AConvertInit parameters:
	"agc"		"1" or "0"
	"byteSwap"	"1" or "0"
	"base"		"base-input-frequency"
	"copy"		"1" or "0"
	"filter"	"size-of-filter"
	"gain"		"gain.multipler"
	"input"		"name[,opt...]"
	"jump"		"bytes-to-skip-in-header"
	"max"		"max-input-count"
	"output"	"name[,opt...]"
	"stats"		"1" or "0"
	"vox"		"1" or "0"
 */

int AConvertFiles(			/* Convert from command line specs */
  int		argc,
  char**	argv);

int AConvert(				/* 0=MAXINP -1=ERROR else DstCnt */
  AConvertCtxt* tpACC,			/* Current context */
  int		iCnt,			/* # of input units */
  void*		vpSrc,			/* Buffer of input data */
  void*		vppDst);		/* Returned pointer to output data */

void AConvertStats(			/* print out stats (if bDoStats) */
  AConvertCtxt* tpACC);

void AConvertClose(			/* close a context */
  AConvertCtxt* tpACC);

#endif
