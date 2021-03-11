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
/* $Header: /crl/audio/AF/server/include/RCS/diastruct.h,v 1.14 1994/02/22 17:19:51 stewart Exp $ */

#ifndef DIASTRUCT_H
#define DIASTRUCT_H

#include <dia.h>
#include <resource.h>
#include <ac.h>
#include <audioproto.h>
#include <os.h>

/*
 * 	direct-mapped hash table, used by resource manager to store
 *      translation from client ids to server addresses.
 */

typedef struct _TimeStamp {
    unsigned long	months;			/* really ~49.7 days */
    unsigned long	milliseconds;
} TimeStamp;

#ifdef DEBUG
#define MAX_REQUEST_LOG 100
#endif

typedef struct _Client {
    int index;
    AMask clientAsMask;
    pointer requestBuffer;
    pointer osPrivate;			/* for OS layer, including scheduler */
    ABool swapped;
    void (* pSwapReplyFunc)();		
    AID	errorValue;
    int sequence;
    int closeDownMode;
    int clientGone;
    int noClientException;       /* this client died or needs to be killed*/
    ACPtr lastAC;
    AContext lastACID;    
    pointer devicePrivate[MAXDEVICES];
    int (**requestVector)();
/*    unsigned char cdata[MAX_REQUEST_SIZE]; */
    unsigned char crdata[MAX_REQUEST_SIZE];
    AMask selectMask[MAXDEVICES]; /* which events this client wants */
#ifdef DEBUG
    unsigned char requestLog[MAX_REQUEST_LOG];
    int requestLogIndex;
#endif
} ClientRec;

extern TimeStamp currentTime;
extern void CloseDownClient();

extern TimeStamp ClientTimeToServerTime();
extern void UpdateCurrentTime();
extern void UpdateCurrentTimeIf();

#endif
