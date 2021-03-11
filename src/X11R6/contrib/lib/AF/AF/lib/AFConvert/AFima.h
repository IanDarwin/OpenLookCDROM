/*static char rcsid[] = "$RCSfile: AFima.h,v $, $Revision: 1.1 $, $Date: 1994/02/01 16:46:28 $";*/

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
 * FILE NAME		: $RCSfile: AFima.h,v $
 * AUTHOR		: $Author: stewart $
 * DATE			: $Date: 1994/02/01 16:46:28 $
 * REVISION		: $Revision: 1.1 $
 * DESCRIPTION		: IMA audio compression
 *
 * REVISION HISTORY
 *
 * $Log: AFima.h,v $
 * Revision 1.1  1994/02/01  16:46:28  stewart
 * Initial revision
 *
 * Revision 1.1  1993/12/10  13:29:54  stewart
 * Initial revision
 */

#ifndef _AFIMA_H_
#define _AFIMA_H_

#include "audio.h"
#include "AFConvert.h"

typedef struct {
  int iOut;			/* IMA previous sample */
  int iLevel;			/* IMA previous level */
  int iLast;			/* IMA previous output */
} IMAState;

extern
void AFConvert_InitIMA(AFConvert_Data *cd, AFConvertCmd cmd);	/* Init IMA */

extern
void AFConvert_CompressIMA(
			   AFConvert_Data *cd,
			   void	*ucpIn,		        /* Input buffer */
			   void	*ucpOut,	        /* Output buffer */
			   int		iCnt		/* Number of units */
			   );

extern
void AFConvert_UncompressIMA(
			     AFConvert_Data *cd,	/* Current context */
			     void	*ucpIn, 	/* Input buffer */
			     void	*ucpOut,	/* Output buffer */
			     int	iCnt		/* Number of units */
			     );
#endif
