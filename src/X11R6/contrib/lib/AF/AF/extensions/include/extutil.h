/* @(#)$Header: /crl/audio/AF/extutil.h,v 1.1 1993/10/26 21:37:57 tml Exp $ */
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
 * $XConsortium: extutil.h,v 1.11 89/12/09 21:12:33 rws Exp $
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

#ifndef _EXTUTIL_H_
#define _EXTUTIL_H_

/*
 * We need to keep a list of open devices since the AFlib device list isn't
 * public.  We also have to per-device info in a separate block since it isn't
 * stored directly in the AudioDevice structure.
 */
typedef struct _AFExtAudioConnInfo {
    struct _AFExtAudioConnInfo *next;	/* keep a linked list */
    AFAudioConn *aud;			/* which audio connection this is */
    AFExtCodes *codes;			/* the extension protocol codes */
    caddr_t data;			/* extra data for extension to use */
} AFExtAudioConnInfo;

typedef struct _AFExtensionInfo {
    AFExtAudioConnInfo *head;		/* start of list */
    AFExtAudioConnInfo *cur;		/* most recently used */
    int naudioconns;			/* number of audio connections */
} AFExtensionInfo;

typedef struct _AFExtensionHooks {
    int (*free_ac)();
    int (*close_audioconn)();
    ABool (*wire_to_event)();
    AStatus (*event_to_wire)();
    int (*error)();
    char *(*error_string)();
} AFExtensionHooks;

extern AFExtensionInfo *AFextCreateExtension();
extern void XextDestroyExtension();
extern AFExtAudioConnInfo *AFextAddAudioConn();
extern int XextRemoveAudioConn();
extern AFExtAudioConnInfo *AFextFindAudioConn();

#define AFextHasExtension(i) ((i) && ((i)->codes))
#define AFextCheckExtension(aud,i,name,val) \
  if (!AFextHasExtension(i)) { AFMissingExtension (aud, name); return val; }
#define AFextSimpleCheckExtension(aud,i,name) \
  if (!AFextHasExtension(i)) { AFMissingExtension (aud, name); return; }


/*
 * helper macros to generate code that is common to all extensions; caller
 * should prefix it with static if extension source is in one file; this
 * could be a utility function, but have to stack 6 unused arguments for 
 * something that is called many, many times would be bad.
 */
#define AFEXT_GENERATE_FIND_AUDIOCONN(proc,extinfo,extname,hooks,nev,data) \
AFExtAudioConnInfo *proc (aud) \
    register AFAudioConn *aud; \
{ \
    AFExtAudioConnInfo *audinfo; \
    if (!extinfo) { if (!(extinfo = AFextCreateExtension())) return NULL; } \
    if (!(audinfo = AFextFindAudioConn (extinfo, aud))) \
      audinfo = AFextAddAudioConn (extinfo,aud,extname,hooks,nev,data); \
    return audinfo; \
}

#define AFEXT_GENERATE_CLOSE_AUDIOCONN(proc,extinfo) \
int proc (aud, codes) \
    AFAudioConn *aud; \
    AFExtCodes *codes; \
{ \
    return AFextRemoveAudioConn (extinfo, aud); \
}

#define AFEXT_GENERATE_ERROR_STRING(proc,extname,nerr,errl) \
char *proc (aud, code, codes, buf, n) \
    AFAudioConn  *aud; \
    int code; \
    AFExtCodes *codes; \
    char *buf; \
    int n; \
{  \
    code -= codes->first_error;  \
    if (code >= 0 && code < nerr) { \
	char tmp[256]; \
	sprintf (tmp, "%s.%d", extname, code); \
	AFGetErrorDatabaseText (aud, "AFProtoError", tmp, errl[code], buf, n); \
	return buf; \
    } \
    return (char *)0; \
}

#endif
