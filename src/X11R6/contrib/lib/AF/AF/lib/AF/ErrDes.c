/*
 * $XConsortium: XErrDes.c,v 11.38 89/12/11 19:09:02 rws Exp $
 */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
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
 */

#include <stdio.h>
#include <AF/Aos.h>
#include "Alibint.h"

static char *_AErrorList[] = {
    /* No error           */	"no error",
    /* ABadRequest        */	"ABadRequest",
    /* ABadValue	  */	"ABadValue",
    /* ABadAtom           */    "ABadAtom",
    /* ABadMatch	  */	"ABadMatch",
    /* ABadAccess         */	"ABadAccess",
    /* ABadAlloc	  */	"ABadAlloc",
    /* ABadAC             */    "ABadAC",
    /* ABadIDChoice       */    "ABadIDChoice",
    /* ABadName           */    "ABadName",
    /* ABadLength         */	"ABadLength",
    /* ABadImplementation */	"ABadImplementation",
    /* ABadDevice         */    "ABadDevice"
};
static int _AErrorListSize = sizeof(_AErrorList);

void
AFGetErrorText(
    register AFAudioConn *aud,
    register int code,
    char *buffer,
    int nbytes)
{
    char buf[32];
    register _AFExtension *ext;
    char *errmsg = "";

    if (nbytes == 0) return;
    sprintf(buf, "%d", code);
    if (code <= (_AErrorListSize/ sizeof (char *)) && code > 0) 
      errmsg = _AErrorList[code];
    AFGetErrorDatabaseText(aud, "AFProtoError", buf, errmsg, buffer, nbytes);
    ext = aud->ext_procs;
    while (ext) {		/* call out to any extensions interested */
 	if (ext->error_string != NULL) 
 	    (*ext->error_string)(aud, code, &ext->codes, buffer, nbytes);
 	ext = ext->next;
    }    
    return;
}

void
AFGetErrorDatabaseText(
    AFAudioConn *aud,
    register const char *name,
    register const char *type,
    const char *defaultp,
    char *buffer,
    int nbytes)
{
	int size;
	size = strlen(defaultp) + strlen(name) + strlen(type) + 6;
        if (size <= nbytes)
	  sprintf(buffer, "%s (%s): %s", name, type, defaultp);
	else if ((strlen(type) + 1) < nbytes)
	  sprintf(buffer, "%s", type);
	else if (nbytes >= 1)
	  buffer[0] = '\0';
}
