#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/extensions/lib/RCS/extutil.c,v 1.2 1993/11/11 16:05:30 tml Exp $";
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
/*
 * $XConsortium: extutil.c,v 1.10 91/07/12 10:28:36 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Jim Fulton, MIT X Consortium
 */
/*
 * 		       Alib Extension-Writing Utilities
 * 
 * This package contains utilities for writing the client API for various
 * protocol extensions.
 * 
 *  Routines include:
 * 
 *         AFextCreateExtension		Called once per extension
 *         AFextDestroyExtension	If no longer using extension
 *         AFextAddAudioDevice		Add another audio device
 *         AFextRemoveAudioDevice	Remove an audio device
 *         AFextFindAudioDevice		Is an audio device open
 * 
 * In addition, the following Alib-style interfaces are provided:
 * 
 *         AFSetExtensionErrorHandler	Establish an extension error handler
 *         AFMissingExtension		Raise an error about missing extension
 */

#include <stdio.h>
#include "Alibint.h"
#include "AFext.h"
#include "extutil.h"


/*
 * AFextCreateExtension - Return an extension descriptor containing context
 * information for this extension.  This object is passed to all AFext
 * routines.
 */
AFExtensionInfo *AFextCreateExtension ()
{
    register AFExtensionInfo *info =
      (AFExtensionInfo *) Xmalloc (sizeof (AFExtensionInfo));

    if (info) {
	info->head = NULL;
	info->cur = NULL;
	info->naudioconns = 0;
    }
    return info;
}


/*
 * AFextDestroyExtension - Free memory the given extension descriptor.
 */
void AFextDestroyExtension (info)
    AFExtensionInfo *info;
{
    info->head = NULL;			/* to catch refs after this */
    info->cur = NULL;
    info->naudioconns = 0;
    Xfree ((char *) info);
}



/*
 * AFextAddAudioConn - Add an audio connection to this extension.
 */
AFExtAudioConnInfo *AFextAddAudioConn (extinfo, aud, ext_name, hooks, nevents, data)
    AFExtensionInfo *extinfo;
    AFAudioConn *aud;
    char *ext_name;
    AFExtensionHooks *hooks;
    int nevents;
    caddr_t data;
{
    AFExtAudioConnInfo *audinfo;

    audinfo = (AFExtAudioConnInfo *) Xmalloc (sizeof (AFExtAudioConnInfo));
    if (!audinfo) return NULL;
    audinfo->next = extinfo->head;
    audinfo->aud = aud;
    audinfo->data = data;
    audinfo->codes = AFInitExtension (aud, ext_name);

    /*
     * If the server has the extension, then we can initialize the 
     * appropriate function vectors.
     */
    if (audinfo->codes) {
	int i, j;

	for (i = 0, j = audinfo->codes->first_event; i < nevents; i++, j++) {
	    AFESetWireToEvent (aud, j, hooks->wire_to_event);
	    AFESetEventToWire (aud, j, hooks->event_to_wire);
	}
	if (hooks->free_ac)
	  AFESetFreeAC (aud, audinfo->codes->extension, hooks->free_ac);
	if (hooks->close_audioconn)
	  AFESetCloseAudioConn (aud, audinfo->codes->extension, 
			     hooks->close_audioconn);
	if (hooks->error)
	  AFESetError (aud, audinfo->codes->extension, hooks->error);
	if (hooks->error_string)
	  AFESetErrorString (aud, audinfo->codes->extension,
			    hooks->error_string);
    }

    /*
     * Now, chain it onto the list.
     */
    extinfo->head = audinfo;
    extinfo->cur = audinfo;
    extinfo->naudioconns++;
    return audinfo;
}


/*
 * AFextRemoveAudioConn - Remove the indicated audio connection from the
 * extension object.
 */
int AFextRemoveAudioConn (extinfo, aud)
    AFExtensionInfo *extinfo;
    AFAudioConn *aud;
{
    AFExtAudioConnInfo *audinfo, *prev;

    /*
     * Locate this audio connection and its back link so that it can be removed.
     */
    prev = NULL;
    for (audinfo = extinfo->head; audinfo; audinfo = audinfo->next) {
	if (audinfo->aud == aud) break;
	prev = audinfo;
    }
    if (!audinfo) return 0;		/* hmm, actually an error */

    /*
     * Remove the audio connection from the list; handles going to zero.
     */
    if (prev)
	prev->next = audinfo->next;
    else
	extinfo->head = audinfo->next;

    extinfo->naudioconns--;
    if (audinfo == extinfo->cur) extinfo->cur = NULL;  /* flush cache */

    Xfree ((char *) audinfo);
    return 1;
}


/*
 * AFextFindAudioConn - Look for an audio connection in this extension;
 * keeps a cache of the most-recently used for efficiency.
 */
AFExtAudioConnInfo *AFextFindAudioConn (extinfo, aud)
    AFExtensionInfo *extinfo;
    AFAudioConn *aud;
{
    register AFExtAudioConnInfo *audinfo;

    /*
     * See if this was the most recently accessed audio connection.
     */
    if ((audinfo = extinfo->cur)&& audinfo->aud == aud) return audinfo;


    /*
     * Look for audio connection in the list.
     */
    for (audinfo = extinfo->head; audinfo; audinfo = audinfo->next) {
	if (audinfo->aud == aud) {
	    extinfo->cur = audinfo;	/* cache most recently used */
	    return audinfo;
	}
    }

    return NULL;
}



static int _default_exterror (aud, ext_name, reason)
    AFAudioConn *aud;
    char *ext_name;
    char *reason;
{
    fprintf (stderr, "AFlib:  extension \"%s\" %s on audio connection \"%s\".\n",
	     ext_name, reason, AudioConnString(aud));
    return 0;
}


/*
 * AFSetExtensionErrorHandler - Sets the handler that gets called when a 
 * requested extension is referenced.
 */

extern int (*_AFExtensionErrorFunction)();

int (*AFSetExtensionErrorHandler(handler))()
    int (*handler)();
{
    int (*oldhandler)() = _AFExtensionErrorFunction;

    _AFExtensionErrorFunction = (handler ? handler :
				_default_exterror);
    return oldhandler;
}


/*
 * AFMissingExtension - Call the extension error handler.
 */
int AFMissingExtension (
    AFAudioConn *aud,
    const char *ext_name)
{
    int (*func)() = (_AFExtensionErrorFunction ?
		     _AFExtensionErrorFunction : _default_exterror);

    if (!ext_name) ext_name = AF_EXTENSION_UNKNOWN;
    return (*func) (aud, ext_name, AF_EXTENSION_MISSING);
}
