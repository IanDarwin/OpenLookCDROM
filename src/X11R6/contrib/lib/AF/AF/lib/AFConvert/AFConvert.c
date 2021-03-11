static char     rcsid[] = "$RCSfile: AFConvert.c,v $, $Revision: 1.4 $, $Date: 1994/03/29 08:44:37 $";

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
 * FILE NAME		: $RCSfile: AFConvert.c,v $
 * AUTHOR		: $Author: tml $
 * DATE			: $Date: 1994/03/29 08:44:37 $
 * REVISION		: $Revision: 1.4 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: AFConvert.c,v $
 * Revision 1.4  1994/03/29  08:44:37  tml
 * lin32 changes
 *
 * Revision 1.3  1994/03/29  07:28:56  tml
 * add wesw's changes. prepare to add j300 changes
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
#include "AFClientTypes.h"
#include "AFConvert.h"
#include "AFima.h"
#include "AFScalar.h"
#include "AFadpcm.h"
#include "AFgsm.h"

ConvertInitProc muInit[] = {
  /*    MU255 = 0              */ AFConvert_InitScalar,
  /*    ALAW = 1               */ AFConvert_InitScalar,
  /*    LIN16 = 2              */ AFConvert_InitScalar,
  /*    LIN32 = 3              */ AFConvert_InitScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_InitIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMInit,
  /*    CRLADPCM3 = 9          */ AConvertADPCMInit,
  /*    CRLADPCM4 = 10         */ AConvertADPCMInit,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_InitScalar,
  /*    IEEED = 14             */ AFConvert_InitScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_InitGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};
ConvertProc muCompress[] = {
  /*    MU255 = 0              */ AFConvert_ConvertScalar,
  /*    ALAW = 1               */ AFConvert_ConvertScalar,
  /*    LIN16 = 2              */ AFConvert_ConvertScalar,
  /*    LIN32 = 3              */ AFConvert_ConvertScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_CompressIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMcompress,
  /*    CRLADPCM3 = 9          */ AConvertADPCMcompress,
  /*    CRLADPCM4 = 10         */ AConvertADPCMcompress,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_ConvertScalar,
  /*    IEEED = 14             */ AFConvert_ConvertScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_CompressGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

ConvertProc muUncompress[] = {
  /*    MU255 = 0              */ AFConvert_ConvertScalar,
  /*    ALAW = 1               */ AFConvert_ConvertScalar,
  /*    LIN16 = 2              */ AFConvert_ConvertScalar,
  /*    LIN32 = 3              */ AFConvert_ConvertScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_UncompressIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMuncompress,
  /*    CRLADPCM3 = 9          */ AConvertADPCMuncompress,
  /*    CRLADPCM4 = 10         */ AConvertADPCMuncompress,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_ConvertScalar,
  /*    IEEED = 14             */ AFConvert_ConvertScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_UncompressGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

ConvertInitProc linInit[] = {
  /*    MU255 = 0              */ AFConvert_InitScalar,
  /*    ALAW = 1               */ AFConvert_InitScalar,
  /*    LIN16 = 2              */ AFConvert_InitScalar,
  /*    LIN32 = 3              */ AFConvert_InitScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_InitIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMInit,
  /*    CRLADPCM3 = 9          */ AConvertADPCMInit,
  /*    CRLADPCM4 = 10         */ AConvertADPCMInit,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_InitScalar,
  /*    IEEED = 14             */ AFConvert_InitScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_InitGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

ConvertProc linCompress[] = {
  /*    MU255 = 0              */ AFConvert_ConvertScalar,
  /*    ALAW = 1               */ AFConvert_ConvertScalar,
  /*    LIN16 = 2              */ AFConvert_ConvertScalar,
  /*    LIN32 = 3              */ AFConvert_ConvertScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_CompressIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMcompress,
  /*    CRLADPCM3 = 9          */ AConvertADPCMcompress,
  /*    CRLADPCM4 = 10         */ AConvertADPCMcompress,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_ConvertScalar,
  /*    IEEED = 14             */ AFConvert_ConvertScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_CompressGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

ConvertProc linUncompress[] = {
  /*    MU255 = 0              */ AFConvert_ConvertScalar,
  /*    ALAW = 1               */ AFConvert_ConvertScalar,
  /*    LIN16 = 2              */ AFConvert_ConvertScalar,
  /*    LIN32 = 3              */ AFConvert_ConvertScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_UncompressIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMuncompress,
  /*    CRLADPCM3 = 9          */ AConvertADPCMuncompress,
  /*    CRLADPCM4 = 10         */ AConvertADPCMuncompress,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_ConvertScalar,
  /*    IEEED = 14             */ AFConvert_ConvertScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_UncompressGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};


ConvertInitProc lin32Init[] = {
  /*    MU255 = 0              */ AFConvert_InitScalar,
  /*    ALAW = 1               */ AFConvert_InitScalar,
  /*    LIN16 = 2              */ AFConvert_InitScalar,
  /*    LIN32 = 3              */ AFConvert_InitScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_InitIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMInit,
  /*    CRLADPCM3 = 9          */ AConvertADPCMInit,
  /*    CRLADPCM4 = 10         */ AConvertADPCMInit,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_InitScalar,
  /*    IEEED = 14             */ AFConvert_InitScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_InitGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

ConvertProc lin32Compress[] = {
  /*    MU255 = 0              */ AFConvert_ConvertScalar,
  /*    ALAW = 1               */ AFConvert_ConvertScalar,
  /*    LIN16 = 2              */ AFConvert_ConvertScalar,
  /*    LIN32 = 3              */ AFConvert_ConvertScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_CompressIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMcompress,
  /*    CRLADPCM3 = 9          */ AConvertADPCMcompress,
  /*    CRLADPCM4 = 10         */ AConvertADPCMcompress,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_ConvertScalar,
  /*    IEEED = 14             */ AFConvert_ConvertScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_CompressGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

ConvertProc lin32Uncompress[] = {
  /*    MU255 = 0              */ AFConvert_ConvertScalar,
  /*    ALAW = 1               */ AFConvert_ConvertScalar,
  /*    LIN16 = 2              */ AFConvert_ConvertScalar,
  /*    LIN32 = 3              */ AFConvert_ConvertScalar,
  /*    G721 = 4               */ NULL,
  /*    G723 = 5               */ NULL,
  /*    UNKNOWN_ENCODETYPE = 6 */ NULL,
  /*    IMA = 7                */ AFConvert_UncompressIMA,
  /*    CRLADPCM2 = 8          */ AConvertADPCMuncompress,
  /*    CRLADPCM3 = 9          */ AConvertADPCMuncompress,
  /*    CRLADPCM4 = 10         */ AConvertADPCMuncompress,
  /*    LPC10 = 11             */ NULL,
  /*    CELP1016 = 12          */ NULL,
  /*    IEEES = 13             */ AFConvert_ConvertScalar,
  /*    IEEED = 14             */ AFConvert_ConvertScalar,
  /*    G722_8 = 15            */ NULL,
  /*    MPEG = 16              */ NULL,
  /*    GSM = 17               */ AFConvert_UncompressGSM,
  /*    VSELP = 18             */ NULL,
  /*    LIN8U = 19             */ NULL,
  /*    LIN8 = 20              */ NULL
};

int AFConvert_InitConvert(AFConvert_Data *cd)
{
  if (cd->toType == MU255) {
    cd->convertInitProc = muInit[cd->fromType];
    cd->convertProc = muUncompress[cd->fromType];
  }
  else if (cd->toType == LIN16) {
    cd->convertInitProc = linInit[cd->fromType];
    cd->convertProc = linUncompress[cd->fromType];
  }
  else if (cd->toType == LIN32) {
    cd->convertInitProc = lin32Init[cd->fromType];
    cd->convertProc = lin32Uncompress[cd->fromType];
  }
  else if (cd->fromType == MU255) {
    cd->convertInitProc = muInit[cd->toType];
    cd->convertProc = muCompress[cd->toType];
  }
  else if (cd->fromType == LIN16) {
    cd->convertInitProc = linInit[cd->toType];
    cd->convertProc = linCompress[cd->toType];
  }
  else if (cd->fromType == LIN32) {
    cd->convertInitProc = lin32Init[cd->toType];
    cd->convertProc = lin32Compress[cd->toType];
  }
  return  ((cd->convertInitProc != NULL) && (cd->convertProc != NULL));
}



