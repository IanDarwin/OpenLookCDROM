/* @(#)$Header: /crl/audio/AF/extensions/lib/RCS/AFTime.c,v 1.1 1993/10/19 14:45:33 tml Exp $ */
/***********************************************************
Copyright 1993 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
#define NEED_REPLIES
#include "Alibint.h"
#include "AFext.h"
#include "extutil.h"
#include "aftimestr.h"

static AFExtensionInfo _aftime_info_data;
static AFExtensionInfo *aftime_info = &_aftime_info_data;
static /* const */ char *aftime_extension_name = AFTIMENAME;

#define AFTimeCheckExtension(aud,i,val) \
  AFextCheckExtension (aud, i, aftime_extension_name, val)

/*****************************************************************************
 *                                                                           *
 *			   private utility routines                          *
 *                                                                           *
 *****************************************************************************/

static int close_audioconn();
static /* const */ AFExtensionHooks aftime_extension_hooks = {
    NULL,				/* free_ac */
    close_audioconn,			/* close_audioconn */
    NULL,				/* wire_to_event */
    NULL,				/* event_to_wire */
    NULL,				/* error */
    NULL				/* error_string */
};

static AFEXT_GENERATE_FIND_AUDIOCONN (find_audioconn, aftime_info,
				      aftime_extension_name,
				      &aftime_extension_hooks,
				      AFTimeNumberEvents, NULL)

static AFEXT_GENERATE_CLOSE_AUDIOCONN (close_audioconn, aftime_info)


/*****************************************************************************
 *                                                                           *
 *		      public AFTime Extension routines                       *
 *                                                                           *
 *****************************************************************************/

ABool AFTimeQueryExtension (aud, event_basep, error_basep)
    AFAudioConn *aud;
    int *event_basep, *error_basep;
{
    AFExtAudioConnInfo *info = find_audioconn (aud);

    if (AFextHasExtension(info)) {
	*event_basep = info->codes->first_event;
	*error_basep = info->codes->first_error;
	return aTrue;
    } else {
	return aFalse;
    }
}


AStatus AFEGetTime(aud, AFTime)
register AFAudioConn *aud;
ATime *AFTime;
{
    AFExtAudioConnInfo *info = find_audioconn (aud);
    register aGetAFTimeReq *req;
    aGetAFTimeReply rep;

    AFTimeCheckExtension(aud,info,0)

    LockConnection(aud);
    GetReq(GetAFTime, req);
    req->reqType = info->codes->major_opcode;
    req->aftimeReqType = A_GetAFTime;
    if (!_AReply(aud, (aReply *) &rep, 0, aTrue)) {
	UnlockConnection(aud);
	SyncHandle();
	return 0;
    }
    *AFTime = rep.time;
    UnlockConnection(aud);
    SyncHandle();
    return 1;
}

