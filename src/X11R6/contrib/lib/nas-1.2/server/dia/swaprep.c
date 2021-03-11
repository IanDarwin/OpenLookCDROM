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
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)swaprep.c,v 1.3 1994/04/07 20:57:06 greg Exp $
 */
/************************************************************
Some portions derived from: 

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

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

#include <audio/audio.h>
#include <audio/Aproto.h>
#include "misc.h"
#include "dixstruct.h"

extern void WriteToClient();

void
Swap32Write(pClient, size, pbuf)
    ClientPtr	pClient;
    int		size;  /* in bytes */
    register long *pbuf;
{
    register int i;
    register char n;

    size >>= 2;
    for(i = 0; i < size; i++)
    /* brackets are mandatory here, because "swapl" macro expands
       to several statements */
    {   
	swapl(&pbuf[i], n);
    }
    (void)WriteToClient(pClient, size << 2, (char *) pbuf);
}

void
CopySwap32Write(pClient, size, pbuf)
    ClientPtr	pClient;
    int		size;   /* in bytes */
    long	*pbuf;
{
    int bufsize = size;
    long *pbufT;
    register long *from, *to, *fromLast, *toLast;
    long tmpbuf[1];
    
    /* Allocate as big a buffer as we can... */
    while (!(pbufT = (long *) ALLOCATE_LOCAL(bufsize)))
    {
        bufsize >>= 1;
	if (bufsize == 4)
	{
	    pbufT = tmpbuf;
	    break;
	}
    }
    
    /* convert lengths from # of bytes to # of longs */
    size >>= 2;
    bufsize >>= 2;

    from = pbuf;
    fromLast = from + size;
    while (from < fromLast) {
	int nbytes;
        to = pbufT;
        toLast = to + min (bufsize, fromLast - from);
        nbytes = (toLast - to) << 2;
        while (to < toLast) {
            /* can't write "cpswapl(*from++, *to++)" because cpswapl is a macro
	       that evaulates its args more than once */
	    cpswapl(*from, *to);
            from++;
            to++;
	    }
	(void)WriteToClient (pClient, nbytes, (char *) pbufT);
	}

    if (pbufT != tmpbuf)
	DEALLOCATE_LOCAL ((char *) pbufT);
}

void
CopySwap16Write(pClient, size, pbuf)
    ClientPtr	pClient;
    int		size;   /* in bytes */
    short	*pbuf;
{
    int bufsize = size;
    short *pbufT;
    register short *from, *to, *fromLast, *toLast;
    short tmpbuf[2];
    
    /* Allocate as big a buffer as we can... */
    while (!(pbufT = (short *) ALLOCATE_LOCAL(bufsize)))
    {
        bufsize >>= 1;
	if (bufsize == 4)
	{
	    pbufT = tmpbuf;
	    break;
	}
    }
    
    /* convert lengths from # of bytes to # of shorts */
    size >>= 1;
    bufsize >>= 1;

    from = pbuf;
    fromLast = from + size;
    while (from < fromLast) {
	int nbytes;
        to = pbufT;
        toLast = to + min (bufsize, fromLast - from);
        nbytes = (toLast - to) << 1;
        while (to < toLast) {
            /* can't write "cpswaps(*from++, *to++)" because cpswaps is a macro
	       that evaulates its args more than once */
	    cpswaps(*from, *to);
            from++;
            to++;
	    }
	(void)WriteToClient (pClient, nbytes, (char *) pbufT);
	}

    if (pbufT != tmpbuf)
	DEALLOCATE_LOCAL ((char *) pbufT);
}

void
WriteSConnSetupPrefix(pClient, pcsp)
    ClientPtr		pClient;
    auConnSetupPrefix	*pcsp;
{
    auConnSetupPrefix	cspT;

    cspT.success = pcsp->success;
    cspT.lengthReason = pcsp->lengthReason;
    cpswaps(pcsp->majorVersion, cspT.majorVersion);
    cpswaps(pcsp->minorVersion, cspT.minorVersion);
    cpswaps(pcsp->length, cspT.length);
    (void)WriteToClient(pClient, sizeof(cspT), (char *) &cspT);
}
