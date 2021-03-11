#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/RCS/aftime.c,v 1.2 1993/10/26 21:43:11 tml Exp $";
#endif
#endif
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
#include "audio.h"
#include "audioproto.h"
#include "misc.h"
#include "extnsionst.h"
#include "diastruct.h"
#define _AFTIME_SERVER_  /* don't want AFlib structures */
#include "aftimestr.h"

static unsigned char AFTimeReqCode = 0;
static int AFTime();
extern int SProcSimpleReq();
static void  AFTimeReset();

/***********************************************************
 * The AFTime extension is really a test of the extension
 * mechanism.  It provides no additional information beyond
 * what is normally given in the AudioFile protocol.
 ***********************************************************/
/****************
 * AFTimeExtensionInit
 *
 * Called from InitExtensions in main() or from QueryExtension() if the
 * extension is dynamically loaded.
 *
 * AFTime has no events or errors (other than the core errors)
 ****************/

void
AFTimeExtensionInit()
{
    ExtensionEntry *extEntry, *AddExtension();

    extEntry = AddExtension(AFTIMENAME, 0, 0, AFTime, SProcSimpleReq,
			    AFTimeReset, StandardMinorOpcode);
    if (extEntry) {
	AFTimeReqCode = extEntry->base;
    }
    else {
	FatalError("AFTimeExtensionInit: AddExtensions failed\n");
    }
}

static int
AFTime(client)
    register ClientPtr client;
{
    aGetAFTimeReply rep;
    register int n;

    REQUEST(aGetAFTimeReq);
    REQUEST_SIZE_MATCH(aGetAFTimeReq);

    rep.type = A_Reply;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.time = GetTimeInMillis();
    if (client->swapped) {
	swaps(&rep.sequenceNumber, n);
	swapl(&rep.length, n);
	swapl(&rep.time, n);
    }
    WriteToClient(client, sizeof(aGetAFTimeReply), (char *)&rep);
    return (client->noClientException);
}

/*ARGSUSED*/
static void
AFTimeReset(extEntry)
ExtensionEntry  *extEntry;
{
}
