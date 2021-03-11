/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)AuErrDes.c,v 1.5 1994/04/07 20:39:34 greg Exp $
 */

#include "Alibint.h"

#include <audio/Aos.h>

static _AuConst char * _AuConst _AuErrorList[] = {
    /* No error	*/		"no error",
    /* AuBadRequest */		"BadRequest",
    /* AuBadValue */		"BadValue",
    /* AuBadDevice */		"BadDevice",
    /* AuBadBucket */		"BadBucket",
    /* AuBadFlow */		"BadFlow",
    /* AuBadElement */		"BadElement",
    /* empty */			"no error",
    /* AuBadMatch */		"BadMatch",
    /* empty */			"no error",
    /* AuBadAccess */		"BadAccess",
    /* AuBadAlloc */		"BadAlloc",
    /* empty */  		"no error",
    /* AuBadConnection */  	"BadConnection",
    /* AuBadIDChoice */		"BadIDChoice",
    /* AuBadName */		"BadName",
    /* AuBadLength */		"BadLength",
    /* AuBadImplementation */	"BadImplementation",
};

void
AuGetErrorText(aud, code, buffer, nbytes)
    register AuServer *aud;
    register int code;
    char *buffer;
    int nbytes;
{
    char buf[150];
    register _AuExtension *ext;
    _AuExtension *bext = (_AuExtension *)NULL;

    if (nbytes == 0) return;
    if (code <= AuBadImplementation && code > 0) {
	sprintf(buf, "%d", code);
	AuGetErrorDatabaseText(aud, "AuProtoError", buf, _AuErrorList[code],
			      buffer, nbytes);
    } else
	buffer[0] = '\0';
    ext = aud->ext_procs;
    while (ext) {		/* call out to any extensions interested */
 	if (ext->error_string != NULL) 
 	    (*ext->error_string)(aud, code, &ext->codes, buffer, nbytes);
	if (ext->codes.first_error &&
	    ext->codes.first_error < code &&
	    (!bext || ext->codes.first_error > bext->codes.first_error))
	    bext = ext;
 	ext = ext->next;
    }    
    if (!buffer[0] && bext) {
	sprintf(buf, "%s.%d", bext->name, code - bext->codes.first_error);
	AuGetErrorDatabaseText(aud, "AuProtoError", buf, "", buffer, nbytes);
    }
    if (!buffer[0])
	sprintf(buffer, "%d", code);
    return;
}

void
#if NeedFunctionPrototypes
/*ARGSUSED*/
AuGetErrorDatabaseText(
    AuServer *aud,
    register _AuConst char *name,
    register _AuConst char *type,
    _AuConst char *defaultp,
    char *buffer,
    int nbytes)
#else
/*ARGSUSED*/
AuGetErrorDatabaseText(aud, name, type, defaultp, buffer, nbytes)
    AuServer *aud;
    register char *name, *type;
    char *defaultp;
    char *buffer;
    int nbytes;
#endif
{
    int len = (defaultp ? strlen(defaultp) : 0);

    if (len >= nbytes) len = nbytes - 1;
    if (len < 0) len = 0;
    buffer[0] = '\0';
    if (defaultp && len)
	(void) strncpy (buffer, defaultp, len);
    buffer[len] = '\0';
}
