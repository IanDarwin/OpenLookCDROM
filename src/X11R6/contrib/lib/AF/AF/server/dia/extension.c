#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/server/dia/RCS/extension.c,v 1.7 1993/11/12 17:10:25 tml Exp $";
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
/* $XConsortium: extension.c,v 1.49 91/12/10 11:26:36 rws Exp $ */

#include "audio.h"
#include "Amd.h"
#include "misc.h"
#include "acstruct.h"
#include "diastruct.h"
#include "audiodev.h"
#include "extnsionst.h"

#define EXTENSION_BASE  128
#define EXTENSION_EVENT_BASE  64
#define LAST_EVENT  128
#define LAST_ERROR 255

AudioDeviceProcEntry AuxillaryAudioDeviceProcs[MAXDEVICES];

static ExtensionEntry **extensions = (ExtensionEntry **)NULL;
extern int (* ProcVector[]) ();
extern int (* SwappedProcVector[]) ();
extern void (* ReplySwapVector[256]) ();
extern void WriteEventsToClient();

int lastEvent = EXTENSION_EVENT_BASE;
static int lastError = AFirstExtensionError;
static int NumExtensions = 0;

ExtensionEntry *AddExtension(name, NumEvents, NumErrors, MainProc, 
			      SwappedMainProc, CloseDownProc, MinorOpcodeProc)
    char *name;
    int NumEvents;
    int NumErrors;
    int (* MainProc)();
    int (* SwappedMainProc)();
    void (* CloseDownProc)();
    unsigned short (* MinorOpcodeProc)();
{
    int i;
    register ExtensionEntry *ext, **newexts;

    if (!MainProc || !SwappedMainProc || !CloseDownProc || !MinorOpcodeProc)
        return((ExtensionEntry *) NULL);
    if ((lastEvent + NumEvents > LAST_EVENT) || 
	        (unsigned)(lastError + NumErrors > LAST_ERROR))
        return((ExtensionEntry *) NULL);

    ext = (ExtensionEntry *) xalloc(sizeof(ExtensionEntry));
    if (!ext)
	return((ExtensionEntry *) NULL);
    ext->name = (char *)xalloc(strlen(name) + 1);
    ext->num_aliases = 0;
    ext->aliases = (char **)NULL;
    if (!ext->name)
    {
	xfree(ext);
	return((ExtensionEntry *) NULL);
    }
    strcpy(ext->name,  name);
    i = NumExtensions;
    newexts = (ExtensionEntry **) xrealloc(extensions,
					   (i + 1) * sizeof(ExtensionEntry *));
    if (!newexts)
    {
	xfree(ext->name);
	xfree(ext);
	return((ExtensionEntry *) NULL);
    }
    NumExtensions++;
    extensions = newexts;
    extensions[i] = ext;
    ext->index = i;
    ext->base = i + EXTENSION_BASE;
    ext->CloseDown = CloseDownProc;
    ext->MinorOpcode = MinorOpcodeProc;
    ProcVector[i + EXTENSION_BASE] = MainProc;
    SwappedProcVector[i + EXTENSION_BASE] = SwappedMainProc;
    if (NumEvents)
    {
        ext->eventBase = lastEvent;
	ext->eventLast = lastEvent + NumEvents;
	lastEvent += NumEvents;
    }
    else
    {
        ext->eventBase = 0;
        ext->eventLast = 0;
    }
    if (NumErrors)
    {
        ext->errorBase = lastError;
	ext->errorLast = lastError + NumErrors;
	lastError += NumErrors;
    }
    else
    {
        ext->errorBase = 0;
        ext->errorLast = 0;
    }
    return(ext);
}

ABool AddExtensionAlias(alias, ext)
    char *alias;
    ExtensionEntry *ext;
{
    char *name;
    char **aliases;

    aliases = (char **)xrealloc(ext->aliases,
				(ext->num_aliases + 1) * sizeof(char *));
    if (!aliases)
	return FALSE;
    ext->aliases = aliases;
    name = (char *)xalloc(strlen(alias) + 1);
    if (!name)
	return FALSE;
    strcpy(name,  alias);
    ext->aliases[ext->num_aliases] = name;
    ext->num_aliases++;
    return TRUE;
}

unsigned short
StandardMinorOpcode(client)
    ClientPtr client;
{
    return ((aReq *)client->requestBuffer)->data;
}

unsigned short
MinorOpcodeOfRequest(client)
    ClientPtr client;
{
    unsigned char major;

    major = ((aReq *)client->requestBuffer)->reqType;
    if (major < EXTENSION_BASE)
	return 0;
    major -= EXTENSION_BASE;
    if (major >= NumExtensions)
	return 0;
    return (*extensions[major]->MinorOpcode)(client);
}

CloseDownExtensions()
{
    register int i,j;

    for (i = NumExtensions - 1; i >= 0; i--)
    {
	(* extensions[i]->CloseDown)(extensions[i]);
	NumExtensions = i;
	xfree(extensions[i]->name);
	for (j = extensions[i]->num_aliases; --j >= 0;)
	    xfree(extensions[i]->aliases[j]);
	xfree(extensions[i]->aliases);
	xfree(extensions[i]);
    }
    xfree(extensions);
    extensions = (ExtensionEntry **)NULL;
    lastEvent = EXTENSION_EVENT_BASE;
    lastError = AFirstExtensionError;
    for (i=0; i<MAXDEVICES; i++)
    {
	register AudioDeviceProcEntry *apentry = &AuxillaryAudioDeviceProcs[i];

	while (apentry->num)
	{
	    apentry->num--;
	    xfree(apentry->procList[apentry->num].name);
	}
	xfree(apentry->procList);
	apentry->procList = (ProcEntryPtr)NULL;
    }
}



int
ProcQueryExtension(client)
    ClientPtr client;
{
    aQueryExtensionReply reply;
    int i, j;
    REQUEST(aQueryExtensionReq);

    REQUEST_FIXED_SIZE(aQueryExtensionReq, stuff->nbytes);
    
    reply.type = A_Reply;
    reply.length = 0;
    reply.major_opcode = 0;
    reply.sequenceNumber = client->sequence;

    if ( ! NumExtensions )
        reply.present = aFalse;
    else
    {
        for (i=0; i<NumExtensions; i++)
	{
            if ((strlen(extensions[i]->name) == stuff->nbytes) &&
                 !strncmp((char *)&stuff[1], extensions[i]->name,
			  (int)stuff->nbytes))
                 break;
	    for (j = extensions[i]->num_aliases; --j >= 0;)
	    {
		if ((strlen(extensions[i]->aliases[j]) == stuff->nbytes) &&
		     !strncmp((char *)&stuff[1], extensions[i]->aliases[j],
			      (int)stuff->nbytes))
		     break;
	    }
	    if (j >= 0) break;
	}
        if (i == NumExtensions)
            reply.present = aFalse;
        else
        {            
            reply.present = aTrue;
	    reply.major_opcode = extensions[i]->base;
	    reply.first_event = extensions[i]->eventBase;
	    reply.first_error = extensions[i]->errorBase;
	}
    }
    WriteReplyToClient(client, sizeof(aQueryExtensionReply), &reply);
    return(client->noClientException);
}

int
ProcListExtensions(client)
    ClientPtr client;
{
    aListExtensionsReply reply;
    char *bufptr, *buffer;
    int total_length = 0;

    REQUEST(aReq);
    REQUEST_SIZE_MATCH(aReq);

    reply.type = A_Reply;
    reply.nExtensions = NumExtensions;
    reply.length = 0;
    reply.sequenceNumber = client->sequence;
    buffer = NULL;

    if ( NumExtensions )
    {
        register int i, j;

        for (i=0;  i<NumExtensions; i++)
	{
	    total_length += strlen(extensions[i]->name) + 1;
	    reply.nExtensions += extensions[i]->num_aliases;
	    for (j = extensions[i]->num_aliases; --j >= 0;)
		total_length += strlen(extensions[i]->aliases[j]) + 1;
	}
        reply.length = (total_length + 3) >> 2;
	buffer = bufptr = (char *)ALLOCATE_LOCAL(total_length);
	if (!buffer)
	    return(ABadAlloc);
        for (i=0;  i<NumExtensions; i++)
        {
	    int len;
            *bufptr++ = len = strlen(extensions[i]->name);
	    bcopy(extensions[i]->name, bufptr,  len);
	    bufptr += len;
	    for (j = extensions[i]->num_aliases; --j >= 0;)
	    {
		*bufptr++ = len = strlen(extensions[i]->aliases[j]);
		bcopy(extensions[i]->aliases[j], bufptr,  len);
		bufptr += len;
	    }
	}
    }
    WriteReplyToClient(client, sizeof(aListExtensionsReply), &reply);
    if (reply.length)
    {
        WriteToClient(client, total_length, buffer);
    	DEALLOCATE_LOCAL(buffer);
    }
    return(client->noClientException);
}


ExtensionLookupProc 
LookupProc(name, pAC)
    char *name;
    ACPtr pAC;
{
    register int i;
    register AudioDeviceProcEntry *apentry;
    apentry  = &AuxillaryAudioDeviceProcs[pAC->aDev->index];
    if (apentry->num)    
    {
        for (i = 0; i < apentry->num; i++)
            if (strcmp(name, apentry->procList[i].name) == 0)
                return(apentry->procList[i].proc);
    }
    return (ExtensionLookupProc)NULL;
}

ABool
RegisterProc(name, pAC, proc)
    char *name;
    ACPtr pAC;
    ExtensionLookupProc proc;
{
    return RegisterAudioDeviceProc(name, pAC->aDev, proc);
}

ABool
RegisterAudioDeviceProc(name, aDev, proc)
    char *name;
    AudioDevicePtr aDev;
    ExtensionLookupProc proc;
{
    register AudioDeviceProcEntry *apentry;
    register ProcEntryPtr procEntry = (ProcEntryPtr)NULL;
    char *newname;
    int i;

    apentry = &AuxillaryAudioDeviceProcs[aDev->index];
    /* first replace duplicates */
    if (apentry->num)
    {
        for (i = 0; i < apentry->num; i++)
            if (strcmp(name, apentry->procList[i].name) == 0)
	    {
                procEntry = &apentry->procList[i];
		break;
	    }
    }
    if (procEntry)
        procEntry->proc = proc;
    else
    {
	newname = (char *)xalloc(strlen(name)+1);
	if (!newname)
	    return FALSE;
	procEntry = (ProcEntryPtr)
			    xrealloc(apentry->procList,
				     sizeof(ProcEntryRec) * (apentry->num+1));
	if (!procEntry)
	{
	    xfree(newname);
	    return FALSE;
	}
	apentry->procList = procEntry;
        procEntry += apentry->num;
        procEntry->name = newname;
        strcpy(newname, name);
        procEntry->proc = proc;
        apentry->num++;        
    }
    return TRUE;
}
