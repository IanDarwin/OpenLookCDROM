/* @(#)$Header: /crl/audio/AF/server/include/RCS/extnsionst.h,v 1.2 1993/11/12 17:06:04 tml Exp $ */
/* $XConsortium: extnsionst.h,v 1.9 89/08/31 18:41:12 rws Exp $ */
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
/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

******************************************************************/
#ifndef EXTENSIONSTRUCT_H
#define EXTENSIONSTRUCT_H 

typedef struct _ExtensionEntry {
    int index;
    void (* CloseDown)();	/* called at server shutdown */
    char *name;               /* extension name */
    int base;                 /* base request number */
    int eventBase;            
    int eventLast;
    int errorBase;
    int errorLast;
    int num_aliases;
    char **aliases;
    pointer extPrivate;
    unsigned short (* MinorOpcode)();	/* called for errors */
} ExtensionEntry;

extern void (* EventSwapVector[128]) ();

typedef void (* ExtensionLookupProc)();

typedef struct _ProcEntry {
    char *name;
    ExtensionLookupProc proc;
} ProcEntryRec, *ProcEntryPtr;

typedef struct _AudioDeviceProcEntry {
    int num;
    ProcEntryPtr procList;
} AudioDeviceProcEntry;

extern void InitExtensions();
extern int ProcQueryExtension();
extern int ProcListExtensions();
extern ExtensionEntry *AddExtension();
extern ABool AddExtensionAlias();
extern ExtensionLookupProc LookupProc();
extern ABool RegisterProc();
extern ABool RegisterAudioDeviceProc();
extern unsigned short MinorOpcodeOfRequest();
extern unsigned short StandardMinorOpcode();

#endif
