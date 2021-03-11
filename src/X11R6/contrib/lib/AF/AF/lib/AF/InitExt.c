#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AF/RCS/InitExt.c,v 1.4 1993/11/12 16:19:16 tml Exp $";
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
/* $XConsortium: XInitExt.c,v 11.28 91/01/08 14:41:05 gildea Exp $ */
/* Copyright  Massachusetts Institute of Technology 1987

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include <AF/Alibint.h>
#include <AF/Aos.h>
#include <stdio.h>


/*
 * This routine is used to link a extension in so it will be called
 * at appropriate times.
 */

AFExtCodes *AFInitExtension (
	AFAudioConn *aud,
	const char *name)
{
	AFExtCodes codes;	/* temp. place for extension information. */
	register _AFExtension *ext;/* need a place to build it all */
	if (!AFQueryExtension(aud, name, 
		&codes.major_opcode, &codes.first_event,
		&codes.first_error)) return (NULL);

	LockConnection (aud);
	if (! (ext = (_AFExtension *) Xcalloc (1, sizeof (_AFExtension))) ||
	    ! (ext->name = (char *) Xmalloc((unsigned) strlen(name) + 1))) {
	    if (ext) Xfree((char *) ext);
	    UnlockConnection(aud);
	    return (AFExtCodes *) NULL;
	}
	codes.extension = aud->ext_number++;
	ext->codes = codes;
	(void) strcpy(ext->name, name);

	/* chain it onto the audio connection list */	
	ext->next = aud->ext_procs;
	aud->ext_procs = ext;
	UnlockConnection (aud);

	return (&ext->codes);		/* tell him which extension */
}

AFExtCodes *AFAddExtension (AFAudioConn *aud)
{
    register _AFExtension *ext;

    LockConnection (aud);
    if (! (ext = (_AFExtension *) Xcalloc (1, sizeof (_AFExtension)))) {
	UnlockConnection(aud);
	return (AFExtCodes *) NULL;
    }
    ext->codes.extension = aud->ext_number++;

    /* chain it onto the audio connection list */
    ext->next = aud->ext_procs;
    aud->ext_procs = ext;
    UnlockConnection (aud);

    return (&ext->codes);		/* tell him which extension */
}

static _AFExtension *AFLookupExtension (
	register AFAudioConn *aud	/* audio connection */,
	register int extension		/* extension number */
	)
{
	register _AFExtension *ext = aud->ext_procs;
	while (ext != NULL) {
		if (ext->codes.extension == extension) return (ext);
		ext = ext->next;
	}
	return (NULL);
}

AFAddToExtensionList(AFExtData **structure, AFExtData *ext_data)
{
    ext_data->next = *structure;
    *structure = ext_data;
}

AFExtData *AFFindOnExtensionList(AFExtData **structure, int number)
{
    AFExtData *ext;

    ext = *structure;
    while (ext && (ext->number != number))
	ext = ext->next;
    return ext;
}

/*
 * Routines to hang procs on the extension structure.
 */
int (*AFESetFreeAC(
    AFAudioConn* aud,
    int extension,
    int (*proc) (
	      AFAudioConn*		/* audio connection */,
              AC			/* audio context */,
              AFExtCodes*		/* codes */
            )
))(
#ifndef	mips
AFAudioConn*, AC, AFExtCodes*
#endif
)
{
	register _AFExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = AFLookupExtension (aud, extension)) == NULL) return (NULL);
	LockConnection(aud);
	oldproc = e->free_AC;
	e->free_AC = proc;
	UnlockConnection(aud);
	return (oldproc);
}

int (*AFESetCloseAudioConn(
    AFAudioConn* aud,
    int	extension,
    int (*proc) (
	      AFAudioConn*		/* audio connection */,
              AFExtCodes*		/* codes */
            )
))(
#ifndef	mips
AFAudioConn*, AFExtCodes*
#endif
)
{
	register _AFExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = AFLookupExtension (aud, extension)) == NULL) return (NULL);
	LockConnection(aud);
	oldproc = e->close_audioconn;
	e->close_audioconn = proc;
	UnlockConnection(aud);
	return (oldproc);
}

int (*AFESetWireToEvent(
    AFAudioConn* aud,
    int event_number,
    ABool (*proc) (
	       AFAudioConn*		/* audio connection */,
               AFEvent*			/* re */,
               aEvent*			/* event */
             )
))(
#ifndef mips
AFAudioConn*, AFEvent*, aEvent*
#endif
)
{
	register ABool (*oldproc)(AFAudioConn*, AFEvent*, aEvent*);

	if (proc == NULL) proc = _AUnknownWireEvent;
	LockConnection (aud);
	oldproc = aud->event_vec[event_number];
	aud->event_vec[event_number] = proc;
	UnlockConnection (aud);
	return (oldproc);
}

AStatus (*AFESetEventToWire(
    AFAudioConn* aud,
    int	event_number,
    int (*proc) (
	      AFAudioConn*		/* audio connection */,
              AFEvent*			/* re */,
              aEvent*			/* event */
            )
))(
#ifndef	mips
AFAudioConn*, AFEvent*, aEvent*
#endif
)
{
	register AStatus (*oldproc)(AFAudioConn*, AFEvent*, aEvent*);

	if (proc == NULL) proc = _AUnknownNativeEvent;
	LockConnection (aud);
	oldproc = aud->wire_vec[event_number];
	aud->wire_vec[event_number] = proc;
	UnlockConnection(aud);
	return (oldproc);
}

int (*AFESetError(
    AFAudioConn* aud,
    int	extension,
    int (*proc) (
	      AFAudioConn*		/* audio connection */,
              aError*			/* err */,
              AFExtCodes*		/* codes */,
              int*			/* ret_code */
            )		/* proc */    
))(
#ifndef	mips
AFAudioConn*, aError*, AFExtCodes*, int*
#endif
)
{
	register _AFExtension *e;	/* for lookup of extension */
	register int (*oldproc)(AFAudioConn*, aError*, AFExtCodes*, int*);

	if ((e = AFLookupExtension (aud, extension)) == NULL) return (NULL);
	LockConnection(aud);
	oldproc = e->error;
	e->error = proc;
	UnlockConnection(aud);
	return (oldproc);
}

char* (*AFESetErrorString(
    AFAudioConn* aud,
    int extension,
    char* (*proc) (
	        AFAudioConn*		/* audio connection */,
                int			/* code */,
                AFExtCodes*		/* codes */,
                char*			/* buffer */,
                int			/* nbytes */
              )
))(
#ifndef mips
AFAudioConn*, int, AFExtCodes*, char*, int
#endif
)
{
	register _AFExtension *e;	/* for lookup of extension */
	register char *(*oldproc)(AFAudioConn*, int, AFExtCodes*, char*, int);

	if ((e = AFLookupExtension (aud, extension)) == NULL) return (NULL);
	LockConnection(aud);
	oldproc = e->error_string;
	e->error_string = proc;
	UnlockConnection(aud);
	return (oldproc);
}
