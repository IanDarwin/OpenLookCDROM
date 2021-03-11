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
 * $NCDId: @(#)io.c,v 1.4 1994/05/27 03:10:44 greg Exp $
 */
/***********************************************************
Some portions derived from:

Copyright 1987, 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
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
/*****************************************************************
 * i/o functions
 *
 *   WriteToClient, ReadRequestFromClient
 *   InsertFakeRequest, ResetCurrentRequest
 *
 *****************************************************************/

#include <stdio.h>
#include <audio/audio.h>
#include <audio/Aproto.h>
#include <audio/Aos.h>
#include <errno.h>
#if !defined(AMOEBA) && !defined(_MINIX)
#include <sys/param.h>
#include <sys/uio.h>
#endif
#include "os.h"
#include "osdep.h"
#include "opaque.h"
#include "dixstruct.h"
#include "misc.h"

#ifndef SCO
#define		_OSWriteV	writev
#endif /* SCO */

#if !defined(AMOEBA) && !defined(_MINIX)
/* check for both EAGAIN and EWOULDBLOCK, because some supposedly POSIX
 * systems are broken and return EWOULDBLOCK when they should return EAGAIN
 */
#if defined(EAGAIN) && defined(EWOULDBLOCK)
#define ETEST(err) (err == EAGAIN || err == EWOULDBLOCK)
#else
#ifdef EAGAIN
#define ETEST(err) (err == EAGAIN)
#else
#define ETEST(err) (err == EWOULDBLOCK)
#endif
#endif

extern void MarkClientException();
extern long ClientsWithInput[];
extern long ClientsWriteBlocked[];
extern long OutputPending[];
#endif /* AMOEBA || _MINIX */
extern int ConnectionTranslation[];
extern Bool NewOutputPending;
extern Bool AnyClientsWriteBlocked;
static Bool CriticalOutputPending;
static int timesThisConnection = 0;
#ifndef AMOEBA
static ConnectionInputPtr FreeInputs = (ConnectionInputPtr)NULL;
static ConnectionOutputPtr FreeOutputs = (ConnectionOutputPtr)NULL;
static OsCommPtr AvailableInput = (OsCommPtr)NULL;

static ConnectionInputPtr AllocateInputBuffer();
static ConnectionOutputPtr AllocateOutputBuffer();
#endif /* AMOEBA */

extern int errno;

#define get_req_len(req,cli) ((cli)->swapped ? \
			      lswaps((req)->length) : (req)->length)

#define MAX_TIMES_PER         10

/*****************************************************************
 * ReadRequestFromClient
 *    Returns one request in client->requestBuffer.  Return status is:
 *
 *    > 0  if  successful, specifies length in bytes of the request
 *    = 0  if  entire request is not yet available
 *    < 0  if  client should be terminated
 *
 *    The request returned must be contiguous so that it can be
 *    cast in the dispatcher to the correct request type.  Because requests
 *    are variable length, ReadRequestFromClient() must look at the first 4
 *    bytes of a request to determine the length (the request length is
 *    always the 3rd and 4th bytes of the request).  
 *
 *    Note: in order to make the server scheduler (WaitForSomething())
 *    "fair", the ClientsWithInput mask is used.  This mask tells which
 *    clients have FULL requests left in their buffers.  Clients with
 *    partial requests require a read.  Basically, client buffers
 *    are drained before select() is called again.  But, we can't keep
 *    reading from a client that is sending buckets of data (or has
 *    a partial request) because others clients need to be scheduled.
 *****************************************************************/

#define YieldControl()				\
        { isItTimeToYield = TRUE;		\
	  timesThisConnection = 0; }
#ifndef _MINIX
#define YieldControlNoInput()			\
        { YieldControl();			\
	  BITCLEAR(ClientsWithInput, fd); }
#else
#define YieldControlNoInput()			\
	{ YieldControl();			\
	  ASIO_FD_CLR(fd, ASIO_READ, &CompletedFdSet); }
#endif
#define YieldControlDeath()			\
        { timesThisConnection = 0; }

    /* lookup table for adding padding bytes to data that is read from
       or written to the audio connection.  */
static int padlength[4] = {0, 3, 2, 1};

#if !defined(AMOEBA) && !defined(_MINIX)
int
ReadRequestFromClient(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci = oc->input;
    int fd = oc->fd;
    register int gotnow, needed;
    int result;
    register auReq *request;
    Bool need_header;

    if (AvailableInput)
    {
	if (AvailableInput != oc)
	{
	    register ConnectionInputPtr aci = AvailableInput->input;
	    if (aci->size > BUFWATERMARK)
	    {
		xfree(aci->buffer);
		xfree(aci);
	    }
	    else
	    {
		aci->next = FreeInputs;
		FreeInputs = aci;
	    }
	    AvailableInput->input = (ConnectionInputPtr)NULL;
	}
	AvailableInput = (OsCommPtr)NULL;
    }
    if (!oci)
    {
	if (oci = FreeInputs)
	{
	    FreeInputs = oci->next;
	}
	else if (!(oci = AllocateInputBuffer()))
	{
	    YieldControlDeath();
	    return -1;
	}
	oc->input = oci;
    }
    oci->bufptr += oci->lenLastReq;

    need_header = FALSE;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if (gotnow < sizeof(auReq))
    {
	needed = sizeof(auReq);
	need_header = TRUE;
    }
    else
    {
	request = (auReq *)oci->bufptr;
	needed = get_req_len(request, client);
	client->req_len = needed;
	needed <<= 2;
    }
    if (gotnow < needed)
    {
	oci->lenLastReq = 0;
	if (needed > MAXBUFSIZE)
	{
	    YieldControlDeath();
	    return -1;
	}
	if ((gotnow == 0) ||
	    ((oci->bufptr - oci->buffer + needed) > oci->size))
	{
	    if ((gotnow > 0) && (oci->bufptr != oci->buffer))
		bcopy(oci->bufptr, oci->buffer, gotnow);
	    if (needed > oci->size)
	    {
		char *ibuf;

		ibuf = (char *)xrealloc(oci->buffer, needed);
		if (!ibuf)
		{
		    YieldControlDeath();
		    return -1;
		}
		oci->size = needed;
		oci->buffer = ibuf;
	    }
	    oci->bufptr = oci->buffer;
	    oci->bufcnt = gotnow;
	}
	result = read(fd, oci->buffer + oci->bufcnt, 
		      oci->size - oci->bufcnt); 
	if (result <= 0)
	{
#if !defined(SYSV386) || !defined(SVR4)
	    if ((result < 0) && ETEST(errno))
	    {
		YieldControlNoInput();
		return 0;
	    }
#endif
	    YieldControlDeath();
	    return -1;
	}
	oci->bufcnt += result;
	gotnow += result;
	/* free up some space after huge requests */
	if ((oci->size > BUFWATERMARK) &&
	    (oci->bufcnt < BUFSIZE) && (needed < BUFSIZE))
	{
	    char *ibuf;

	    ibuf = (char *)xrealloc(oci->buffer, BUFSIZE);
	    if (ibuf)
	    {
		oci->size = BUFSIZE;
		oci->buffer = ibuf;
		oci->bufptr = ibuf + oci->bufcnt - gotnow;
	    }
	}
	if (need_header && gotnow >= needed)
	{
	    request = (auReq *)oci->bufptr;
	    needed = get_req_len(request, client);
	    client->req_len = needed;
	    needed <<= 2;
	}
	if (gotnow < needed)
	{
	    YieldControlNoInput();
	    return 0;
	}
    }
    if (needed == 0)
    {
	    needed = sizeof(auReq);
    }
    oci->lenLastReq = needed;

    /*
     *  Check to see if client has at least one whole request in the
     *  buffer.  If there is only a partial request, treat like buffer
     *  is empty so that select() will be called again and other clients
     *  can get into the queue.   
     */

    gotnow -= needed;
    if (gotnow >= sizeof(auReq)) 
    {
	request = (auReq *)(oci->bufptr + needed);
	if (gotnow >= (result = (get_req_len(request, client) << 2)))
	    BITSET(ClientsWithInput, fd);
	else
	    YieldControlNoInput();
    }
    else
    {
	if (!gotnow)
	    AvailableInput = oc;
	YieldControlNoInput();
    }
    if (++timesThisConnection >= MAX_TIMES_PER)
	YieldControl();
    client->requestBuffer = (pointer)oci->bufptr;
    return needed;
}

/*****************************************************************
 * InsertFakeRequest
 *    Splice a consed up (possibly partial) request in as the next request.
 *
 **********************/

Bool
InsertFakeRequest(client, data, count)
    ClientPtr client;
    char *data;
    int count;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci = oc->input;
    int fd = oc->fd;
    register auReq *request;
    register int gotnow, moveup;

    if (AvailableInput)
    {
	if (AvailableInput != oc)
	{
	    register ConnectionInputPtr aci = AvailableInput->input;
	    if (aci->size > BUFWATERMARK)
	    {
		xfree(aci->buffer);
		xfree(aci);
	    }
	    else
	    {
		aci->next = FreeInputs;
		FreeInputs = aci;
	    }
	    AvailableInput->input = (ConnectionInputPtr)NULL;
	}
	AvailableInput = (OsCommPtr)NULL;
    }
    if (!oci)
    {
	if (oci = FreeInputs)
	    FreeInputs = oci->next;
	else if (!(oci = AllocateInputBuffer()))
	    return FALSE;
	oc->input = oci;
    }
    oci->bufptr += oci->lenLastReq;
    oci->lenLastReq = 0;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if ((gotnow + count) > oci->size)
    {
	char *ibuf;

	ibuf = (char *)xrealloc(oci->buffer, gotnow + count);
	if (!ibuf)
	    return(FALSE);
	oci->size = gotnow + count;
	oci->buffer = ibuf;
	oci->bufptr = ibuf + oci->bufcnt - gotnow;
    }
    moveup = count - (oci->bufptr - oci->buffer);
    if (moveup > 0)
    {
	if (gotnow > 0)
	    bcopy(oci->bufptr, oci->bufptr + moveup, gotnow);
	oci->bufptr += moveup;
	oci->bufcnt += moveup;
    }
    bcopy(data, oci->bufptr - count, count);
    oci->bufptr -= count;
    request = (auReq *)oci->bufptr;
    gotnow += count;
    if ((gotnow >= sizeof(auReq)) &&
	(gotnow >= (int) (get_req_len(request, client) << 2)))
	BITSET(ClientsWithInput, fd);
    else
	YieldControlNoInput();
    return(TRUE);
}

/*****************************************************************
 * ResetRequestFromClient
 *    Reset to reexecute the current request, and yield.
 *
 **********************/

ResetCurrentRequest(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci = oc->input;
    int fd = oc->fd;
    register auReq *request;
    int gotnow, needed;

    if (AvailableInput == oc)
	AvailableInput = (OsCommPtr)NULL;
    oci->lenLastReq = 0;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if (gotnow < sizeof(auReq))
    {
	YieldControlNoInput();
    }
    else
    {
	request = (auReq *)oci->bufptr;
	needed = get_req_len(request, client);
	if (gotnow >= (needed << 2))
	{
	    BITSET(ClientsWithInput, fd);
	    YieldControl();
	}
	else
	    YieldControlNoInput();
    }
}

 /********************
 * FlushClient()
 *    If the client isn't keeping up with us, then we try to continue
 *    buffering the data and set the apropriate bit in ClientsWritable
 *    (which is used by WaitFor in the select).  If the connection yields
 *    a permanent error, or we can't allocate any more space, we then
 *    close the connection.
 *
 **********************/

int
FlushClient(who, oc, extraBuf, extraCount)
    ClientPtr who;
    OsCommPtr oc;
    char *extraBuf;
    int extraCount; /* do not modify... returned below */
{
    register ConnectionOutputPtr oco = oc->output;
    int connection = oc->fd;
    struct iovec iov[3];
    char padBuffer[3];
    long written;
    long padsize;
    long notWritten;
    long todo;

    if (!oco)
	return 0;
    written = 0;
    padsize = padlength[extraCount & 3];
    notWritten = oco->count + extraCount + padsize;
    todo = notWritten;
    while (notWritten) {
	long before = written;	/* amount of whole thing written */
	long remain = todo;	/* amount to try this time, <= notWritten */
	int i = 0;
	long len;

	/* You could be very general here and have "in" and "out" iovecs
	 * and write a loop without using a macro, but what the heck.  This
	 * translates to:
	 *
	 *     how much of this piece is new?
	 *     if more new then we are trying this time, clamp
	 *     if nothing new
	 *         then bump down amount already written, for next piece
	 *         else put new stuff in iovec, will need all of next piece
	 *
	 * Note that todo had better be at least 1 or else we'll end up
	 * writing 0 iovecs.
	 */
#define InsertIOV(pointer, length) \
	len = (length) - before; \
	if (len > remain) \
	    len = remain; \
	if (len <= 0) { \
	    before = (-len); \
	} else { \
	    iov[i].iov_len = len; \
	    iov[i].iov_base = (pointer) + before; \
	    i++; \
	    remain -= len; \
	    before = 0; \
	}

	InsertIOV ((char *)oco->buf, oco->count)
	InsertIOV (extraBuf, extraCount)
	InsertIOV (padBuffer, padsize)

	errno = 0;
	if ((len = _OSWriteV(connection, iov, i)) >= 0)
	{
	    written += len;
	    notWritten -= len;
	    todo = notWritten;
	}
	else if (ETEST(errno)
#ifdef SUNSYSV /* check for another brain-damaged OS bug */
		 || (errno == 0)
#endif
#ifdef EMSGSIZE /* check for another brain-damaged OS bug */
		 || ((errno == EMSGSIZE) && (todo == 1))
#endif
#ifdef SERVER_LOCALCONN /* STREAMS ... */
		|| ((errno == ERANGE) && (todo == 1))
#endif
		)
	{
	    /* If we've arrived here, then the client is stuffed to the gills
	       and not ready to accept more.  Make a note of it and buffer
	       the rest. */
	    BITSET(ClientsWriteBlocked, connection);
	    AnyClientsWriteBlocked = TRUE;

	    if (written < oco->count)
	    {
		if (written > 0)
		{
		    oco->count -= written;
		    bcopy((char *)oco->buf + written,
			  (char *)oco->buf,
			  oco->count);
		    written = 0;
		}
	    }
	    else
	    {
		written -= oco->count;
		oco->count = 0;
	    }

	    if (notWritten > oco->size)
	    {
		unsigned char *obuf;

		obuf = (unsigned char *)xrealloc(oco->buf,
						 notWritten + BUFSIZE);
		if (!obuf)
		{
		    close(connection);
		    MarkClientException(who);
		    oco->count = 0;
		    return(-1);
		}
		oco->size = notWritten + BUFSIZE;
		oco->buf = obuf;
	    }

	    /* If the amount written extended into the padBuffer, then the
	       difference "extraCount - written" may be less than 0 */
	    if ((len = extraCount - written) > 0)
		bcopy (extraBuf + written,
		       (char *)oco->buf + oco->count,
		       len);

	    oco->count = notWritten; /* this will include the pad */
	    /* return only the amount explicitly requested */
	    return extraCount;
	}
#ifdef EMSGSIZE /* check for another brain-damaged OS bug */
	else if (errno == EMSGSIZE)
	{
	    todo >>= 1;
	}
#endif
#ifdef SERVER_LOCALCONN /* STREAMS ... */
	else if (errno == ERANGE)
	{
            todo >>= 1;
	}
#endif
	else
	{
	    close(connection);
	    MarkClientException(who);
	    oco->count = 0;
	    return(-1);
	}
    }

    /* everything was flushed out */
    oco->count = 0;
    /* check to see if this client was write blocked */
    if (AnyClientsWriteBlocked)
    {
	BITCLEAR(ClientsWriteBlocked, oc->fd);
 	if (! ANYSET(ClientsWriteBlocked))
	    AnyClientsWriteBlocked = FALSE;
    }
    if (oco->size > BUFWATERMARK)
    {
	xfree(oco->buf);
	xfree(oco);
    }
    else
    {
	oco->next = FreeOutputs;
	FreeOutputs = oco;
    }
    oc->output = (ConnectionOutputPtr)NULL;
    return extraCount; /* return only the amount explicitly requested */
}

 /********************
 * FlushAllOutput()
 *    Flush all clients with output.  However, if some client still
 *    has input in the queue (more requests), then don't flush.  This
 *    will prevent the output queue from being flushed every time around
 *    the round robin queue.  Now, some say that it SHOULD be flushed
 *    every time around, but...
 *
 **********************/

void
FlushAllOutput()
{
    register int index, base, mask;
    OsCommPtr oc;
    register ClientPtr client;

    if (! NewOutputPending)
	return;

    /*
     * It may be that some client still has critical output pending,
     * but he is not yet ready to receive it anyway, so we will
     * simply wait for the select to tell us when he's ready to receive.
     */
    CriticalOutputPending = FALSE;
    NewOutputPending = FALSE;

    for (base = 0; base < mskcnt; base++)
    {
	mask = OutputPending[ base ];
	OutputPending[ base ] = 0;
	while (mask)
	{
	    index = ffs(mask) - 1;
	    mask &= ~lowbit(mask);
	    if ((index = ConnectionTranslation[(base << 5) + index]) == 0)
		continue;
	    client = clients[index];
	    if (client->clientGone)
		continue;
	    oc = (OsCommPtr)client->osPrivate;
	    if (GETBIT(ClientsWithInput, oc->fd))
	    {
		BITSET(OutputPending, oc->fd); /* set the bit again */
		NewOutputPending = TRUE;
	    }
	    else
		(void)FlushClient(client, oc, (char *)NULL, 0);
	}
    }

}
#endif /* !AMOEBA && !_MINIX */

void
FlushIfCriticalOutputPending()
{
    if (CriticalOutputPending)
	FlushAllOutput();
}

void
SetCriticalOutputPending()
{
    CriticalOutputPending = TRUE;
}

/*****************
 * WriteToClient
 *    Copies buf into ClientPtr.buf if it fits (with padding), else
 *    flushes ClientPtr.buf and buf to client.  As of this writing,
 *    every use of WriteToClient is cast to void, and the result
 *    is ignored.  Potentially, this could be used by requests
 *    that are sending several chunks of data and want to break
 *    out of a loop on error.  Thus, we will leave the type of
 *    this routine as int.
 *****************/

#if !defined(AMOEBA) && !defined(_MINIX)
int
WriteToClient (who, count, buf)
    ClientPtr who;
    char *buf;
    int count;
{
    OsCommPtr oc = (OsCommPtr)who->osPrivate;
    register ConnectionOutputPtr oco = oc->output;
    int padBytes;

    if (!count)
	return(0);

    if (!oco)
    {
	if (oco = FreeOutputs)
	{
	    FreeOutputs = oco->next;
	}
	else if (!(oco = AllocateOutputBuffer()))
	{
	    close(oc->fd);
	    MarkClientException(who);
	    return -1;
	}
	oc->output = oco;
    }

    padBytes =  padlength[count & 3];

    if (oco->count + count + padBytes > oco->size)
    {
	BITCLEAR(OutputPending, oc->fd);
	CriticalOutputPending = FALSE;
	NewOutputPending = FALSE;
	return FlushClient(who, oc, buf, count);
    }

    NewOutputPending = TRUE;
    BITSET(OutputPending, oc->fd);
    bcopy(buf, (char *)oco->buf + oco->count, count);
    oco->count += count + padBytes;
    
    return(count);
}
#endif /* !AMOEBA && !_MINIX */

#ifndef AMOEBA
static ConnectionInputPtr
AllocateInputBuffer()
{
    register ConnectionInputPtr oci;

    oci = (ConnectionInputPtr)xalloc(sizeof(ConnectionInput));
    if (!oci)
	return (ConnectionInputPtr)NULL;
    oci->buffer = (char *)xalloc(BUFSIZE);
    if (!oci->buffer)
    {
	xfree(oci);
	return (ConnectionInputPtr)NULL;
    }
    oci->size = BUFSIZE;
    oci->bufptr = oci->buffer;
    oci->bufcnt = 0;
    oci->lenLastReq = 0;
    return oci;
}

static ConnectionOutputPtr
AllocateOutputBuffer()
{
    register ConnectionOutputPtr oco;

    oco = (ConnectionOutputPtr)xalloc(sizeof(ConnectionOutput));
    if (!oco)
	return (ConnectionOutputPtr)NULL;
    oco->buf = (unsigned char *) xalloc(BUFSIZE);
    if (!oco->buf)
    {
	xfree(oco);
	return (ConnectionOutputPtr)NULL;
    }
    oco->size = BUFSIZE;
    oco->count = 0;
    return oco;
}

void
FreeOsBuffers(oc)
    OsCommPtr oc;
{
    register ConnectionInputPtr oci;
    register ConnectionOutputPtr oco;

#ifndef _MINIX
    if (AvailableInput == oc)
	AvailableInput = (OsCommPtr)NULL;
#else
    if (oci = oc->inputFake)
    {
	if (FreeInputs)
	{
	    xfree(oci->buffer);
	    xfree(oci);
	}
	else
	{
	    FreeInputs = oci;
	    oci->next = (ConnectionInputPtr)NULL;
	    oci->bufptr = oci->buffer;
	    oci->bufcnt = 0;
	    oci->lenLastReq = 0;
	}
    }
#endif
    if (oci = oc->input)
    {
	if (FreeInputs)
	{
	    xfree(oci->buffer);
	    xfree(oci);
	}
	else
	{
	    FreeInputs = oci;
	    oci->next = (ConnectionInputPtr)NULL;
	    oci->bufptr = oci->buffer;
	    oci->bufcnt = 0;
	    oci->lenLastReq = 0;
	}
    }
    if (oco = oc->output)
    {
	if (FreeOutputs)
	{
	    xfree(oco->buf);
	    xfree(oco);
	}
	else
	{
	    FreeOutputs = oco;
	    oco->next = (ConnectionOutputPtr)NULL;
	    oco->count = 0;
	}
    }
#ifdef _MINIX
    if (oco = oc->outputNext)
    {
	if (FreeOutputs)
	{
	    xfree(oco->buf);
	    xfree(oco);
	}
	else
	{
	    FreeOutputs = oco;
	    oco->next = (ConnectionOutputPtr)NULL;
	    oco->count = 0;
	}
    }
#endif
}

void
ResetOsBuffers()
{
    register ConnectionInputPtr oci;
    register ConnectionOutputPtr oco;

    while (oci = FreeInputs)
    {
	FreeInputs = oci->next;
	xfree(oci->buffer);
	xfree(oci);
    }
    while (oco = FreeOutputs)
    {
	FreeOutputs = oco->next;
	xfree(oco->buf);
	xfree(oco);
    }
}
#endif /* AMOEBA */

/*
 * The rest of this file contains the Amoeba and Minix implementations.
 */

#ifdef AMOEBA
#include <amoeba.h>
#include <cmdreg.h>
#include <stdcom.h>
#include <stderr.h>
#include <ampolicy.h>
#include <server/ip/hton.h>
#include <server/ip/types.h>
#include <server/ip/tcpip.h>
#include <server/ip/tcp_io.h>
#include <server/ip/gen/in.h>
#include <server/ip/gen/tcp.h>
#include <server/ip/gen/tcp_io.h>

/*
 * Philip's TCP/IP server silently assumes a
 * maximum buffer size of 30000 bytes.
 */
#define	TCPIP_BUFSIZE	16384

int
ReadRequestFromClient(client)
    ClientPtr client;
{
    register OsCommPtr	oc = (OsCommPtr)client->osPrivate;
    register xReq	*request;
    register int	havenow, needed;
    register char	*p;
    int			stat, n, rv;

    oc->status &= ~REQ_PUSHBACK;
    if (oc->size == 0) {
	if (oc->buffer) {
	    Xfree(oc->buffer);
	    oc->buffer = NULL;
	}
	if ((rv = am_avail(oc, VC_IN)) >= (SIZEOF(xReq))) {
	    /*
	     * Enough available to read first portion of
	     * the request.
	     */
	    oc->buffer = (char *) xalloc(SIZEOF(xReq));
	    if (oc->buffer == NULL) {
		ErrorF("ReadRequestFromClient: out of memory\n");
		return -1;
	    }
	    oc->size = SIZEOF(xReq);
	    n = am_read(oc, oc->buffer, SIZEOF(xReq));
	    if (n != SIZEOF(xReq)) {
		ErrorF("ReadRequestFromClient: got %d wanted %d\n",
		    n, SIZEOF(xReq));
		return -1;
	    }
	} else if (rv < 0) {
	    ErrorF("ReadRequestFromClient: read failed (connection %d)\n",
		oc->number);
	    oc->status |= CONN_KILLED;
	    return -1;
	} else {
	    isItTimeToYield = TRUE;
	    return 0;
	}
    }

    /*
     * See if we have enough in the local buffer,
     * plus what is still in the virtual circuit.
     */
    if ((stat = am_avail(oc, VC_IN)) < 0) { /* oc closed */
	ErrorF("ReadRequestFromClient: read failed (connection %d)\n",
	    oc->number);
	oc->status |= CONN_KILLED;
	return -1;
    }

    havenow = oc->size + stat;
    request = (xReq *)oc->buffer;
    if (havenow < SIZEOF(xReq)) {
NotAllHereYet:
	/* not a whole message yet; return */
	isItTimeToYield = TRUE;
	return 0;
    }

    /*
     * Everything is ok, see how much we need to read
     */
    if (request != 0)
	needed = request_length(request, client);
    else
	needed = -1;
    if (needed <= 0)
	needed = sizeof(xReq);
    if (needed > havenow) {
	if (havenow > oc->size) {
	    if ((oc->buffer = (char *)xrealloc(oc->buffer, havenow)) == 0) {
		ErrorF("ReadRequestFromClient: out of memory\n");
		return -1;
	    }
	    n = am_read(oc, oc->buffer + oc->size, havenow - oc->size);
	    if (n != havenow-oc->size) {
		ErrorF("ReadRequestFromClient: got %d wanted %d\n",
		    n, havenow-oc->size);
		return -1;
	    }
	    oc->size = havenow;
	}
	goto NotAllHereYet;
    }
    if (needed > oc->size) {
	if ((oc->buffer = (char *)Xrealloc(oc->buffer, needed)) == 0) {
	    ErrorF("ReadRequestFromClient: out of memory\n");
	    return -1;
	}
	request = (xReq *)oc->buffer;
	n = am_read(oc, oc->buffer + oc->size, needed - oc->size);
	if (n != needed-oc->size) {
	    ErrorF("ReadRequestFromClient: got %d wanted %d\n",
		n, needed-oc->size);
	    return -1;
	}
    }
    oc->size = 0;
    if (++timesThisConnection >= MAX_TIMES_PER || isItTimeToYield) {
	isItTimeToYield = TRUE;
	timesThisConnection = 0;
    }
    client->requestBuffer = (pointer)oc->buffer;
    return needed;
}

Bool
InsertFakeRequest(client, data, count)
    ClientPtr client;
    char *data;
    int count;
{
    register OsCommPtr      oc = (OsCommPtr)client->osPrivate;

    oc->status |= REQ_PUSHBACK;
    WakeUpMainThread();
    if (oc->size) {
	ErrorF("Warning: InsertFakeRequest(%d): %d already\n", count, oc->size);
	oc->buffer = (char *)xrealloc(oc->buffer, oc->size+count);
	if (oc->buffer == NULL) {
	    ErrorF("InsertFakeClient: out of memory\n");
	    oc->size = 0;
	    oc->buffer = 0;
	    return FALSE;
	}
    } else {
	oc->buffer = (char *)xalloc(count);
	if (oc->buffer == NULL) {
	    ErrorF("InsertFakeRequest: out of memory\n");
	    oc->size = 0;
	    oc->buffer = 0;
	    return FALSE;
	}
    }
    bcopy(data, oc->buffer + oc->size, count);
    oc->size += count;
    return TRUE;
}

ResetCurrentRequest(client)
    ClientPtr client;
{
    register OsCommPtr	oc = (OsCommPtr)client->osPrivate;
    register xReq	*request;

    oc->status |= REQ_PUSHBACK;
    WakeUpMainThread();
    if (oc->size) {
	ErrorF("ResetCurrentRequest: partial request\n");
	return;
    }
    if (oc->buffer == NULL) {
	ErrorF("ResetCurrentRequest: no request\n");
	return;
    }
    request = (xReq *)oc->buffer;
    oc->size = request_length(request, client);
}

int
FlushClient(who, oc, extraBuf, extraCount)
    ClientPtr who;
    OsCommPtr oc;
    char *extraBuf;
    int extraCount;
{
    return 0;
}

void
FlushAllOutput()
{
}

int
WriteToClient (who, count, buf)
    ClientPtr who;
    char *buf;
    int count;
{
    register OsCommPtr oc = (OsCommPtr)who->osPrivate;
    int padBytes;

    if (count == 0) return 0;
    if (count < 0) {
	ErrorF("WriteToClient: count %d < 0?\n", count);
	return 0; /* silly request */
    }

    padBytes = padlength[count & 3];
    if ((count = am_write(oc, buf, count)) < 0)
	oc->status |= CONN_KILLED;
    if (count > 0 && count & 3) {
	if (am_write(oc, "\0\0\0\0", padlength[count & 3]) < 0)
	    oc->status |= CONN_KILLED;
    }
    return count;
}

int
am_avail(oc, which)
    OsCommPtr oc;
    int which;
{
    if (oc->family == FamilyAmoeba)
	return vc_avail(oc->conn.vc, which);
    if (oc->family == FamilyInternet)
	return cb_full(oc->conn.tcp.cb);
    return -1;
}

int
am_read(oc, buffer, size)
    OsCommPtr oc;
    char *buffer;
    int size;
{
    if (oc->family == FamilyAmoeba)
	return vc_readall(oc->conn.vc, buffer, size);
    if (oc->family == FamilyInternet)
	return cb_gets(oc->conn.tcp.cb, buffer, size, size);
    return -1;
}

int
am_write(oc, buffer, size)
    OsCommPtr oc;
    char *buffer;
    int size;
{
    if (oc->family == FamilyAmoeba)
	return vc_write(oc->conn.vc, buffer, size);
    if (oc->family == FamilyInternet) {
	bufsize bsize;
	int count, wrcnt;

	for (count = 0; size > 0; ) {
	    wrcnt = size > TCPIP_BUFSIZE ? TCPIP_BUFSIZE : size;
	    bsize = tcpip_write(&oc->conn.tcp.cap, buffer, wrcnt);
	    if (ERR_STATUS(bsize)) {
		ErrorF("TCP/IP write failed: %s\n",
		    tcpip_why(ERR_CONVERT(bsize)));
		return -1;
	    }
	    if (bsize != wrcnt) {
		ErrorF("TCP/IP write failed (expected %d, wrote %d)\n",
		    (int)bsize, wrcnt);
		return -1;
	    }
	    buffer += bsize;
	    size -= bsize;
	    count += bsize;
	}

	return size;
    }
    return -1;
}

void
am_close(oc, which)
    OsCommPtr oc;
    int which;
{
    if (amDebug) ErrorF("am_close() %s, %d\n",
	oc->family == FamilyAmoeba ? "Amoeba" : "TCP/IP", oc->number);

    if (oc->family == FamilyAmoeba)
	vc_close(oc->conn.vc, which);
    if (oc->family == FamilyInternet) {
	if (oc->conn.tcp.signal != -1)
	    sig_raise(oc->conn.tcp.signal);
	std_destroy(&oc->conn.tcp.cap);
	cb_close(oc->conn.tcp.cb);
	cb_free(oc->conn.tcp.cb);
	oc->conn.tcp.cb = NULL;
    }
}
#endif /* AMOEBA */

#ifdef _MINIX
extern asio_fd_set_t InprogressFdSet;
extern asio_fd_set_t ListenFdSet;
extern asio_fd_set_t ClientFdSet;
extern asio_fd_set_t CompletedFdSet;
extern asio_fd_set_t IgnoreFdSet;
extern asio_fd_set_t GrabFdSet;

extern Bool AnyClientsWithInput;
extern int lastfdesc;                 /* maximum file descriptor */
extern int GrabInProgress;

int
ReadRequestFromClient(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci, oci_r;
    int fd = oc->fd;
    register int gotnow, gotnow_r, needed;
    int result;
    register xReq *request;

    if (GrabInProgress && !ASIO_FD_ISSET(fd, ASIO_READ, &GrabFdSet))
    {
	YieldControl();
	return 0;
    }
    if (ASIO_FD_ISSET(fd, ASIO_READ, &IgnoreFdSet))
    {
	YieldControl();
	return 0;
    }

    ASIO_FD_CLR(fd, ASIO_READ, &CompletedFdSet);
    oci= oc->inputFake;
    if (oci)
    {
    	oci->bufptr += oci->lenLastReq;
	oci->lenLastReq= 0;
    }
    oci= oc->input;
    if (oci)
    {
    	oci->bufptr += oci->lenLastReq;
	oci->lenLastReq= 0;
    }
    for(;;)
    {
	/* Let's check Fake requests first */

	oci= oc->inputFake;
	if (oci)
	{
	    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
	    if (gotnow == 0)
	    {
		/* End of fake request */
		xfree(oci->buffer);
		xfree(oci);
		oc->inputFake= NULL;

		continue;
	    }

	    /* Let's move the data down, if necessary */
	    if (oci->bufptr != oci->buffer)
	    {
		bcopy(oci->bufptr, oci->buffer, gotnow);
		oci->bufptr= oci->buffer;
	    }

	    if (gotnow < sizeof(xReq))
		FatalError("Fake request is too small\n"); 
	    request = (xReq *)oci->bufptr;
	    needed= request_length(request, client);
	    if (needed < sizeof(xReq))
	       needed= sizeof(xReq);
	    else if (needed > MAXBUFSIZE)
	    	FatalError("Fake request is too large\n");
	    if (needed > oci->size)
	    {
		char *ibuf;

		ibuf = (char *)xrealloc(oci->buffer, needed);
		if (!ibuf)
		{
		    YieldControlDeath();
		    ErrorF("ReadRequestFromClient: cannot reallocate buffer\n");
		    return -1;
		}
		oci->size = needed;
		oci->buffer = ibuf;
		oci->bufptr = ibuf;
	    }
	    if (gotnow >= needed)
	    {
		if (++timesThisConnection >= MAX_TIMES_PER)
		    YieldControl();
		ASIO_FD_SET(fd, ASIO_READ, &CompletedFdSet);
		AnyClientsWithInput= TRUE;

		client->requestBuffer = (pointer)oci->bufptr;
	    	oci->lenLastReq= needed;
		ErrorF("ReadRequestFromClient: gotnow = %d; needed = %d\n",
			gotnow, needed);
		return needed;
	    }

	    /* Do we have something in the input buffer, we can use? */
	    oci_r= oc->input;
	    if (oci_r == NULL)
	    {
	    	/* No input buffer, we can make the fake buffer the input 
	    	 * buffer */
	    	if (ASIO_FD_ISSET(fd, ASIO_READ, &InprogressFdSet))
		    FatalError("no input buffer but in progress\n");
		oc->input= oci;
		oc->inputFake= NULL;
		continue;
	    }
	    gotnow_r = oci_r->bufcnt + oci_r->buffer - oci_r->bufptr;
	    if (gotnow_r == 0)
	    {
		/* No input buffer, do we have a read in progress? */
		if (ASIO_FD_ISSET(fd, ASIO_READ, &InprogressFdSet))
		{
		    YieldControlNoInput();
		    ErrorF("ReadRequestFromClient: no read in progress\n");
		    return 0;
		}

		xfree(oci_r->buffer);
		xfree(oci_r);
		oc->input= oci;
		oc->inputFake= NULL;
		continue;
	    }
	    if (gotnow_r > needed-gotnow)
	    	gotnow_r= needed-gotnow;
	    bcopy(oci_r->bufptr, oci->buffer+gotnow, gotnow_r);
	    oci_r->bufptr += gotnow_r;
	    continue;
	}

	/* No fake input */
	oci= oc->input;
	if (!oci)
	{
	    if (oci = FreeInputs)
	    {
		FreeInputs = oci->next;
	    }
	    else if (!(oci = AllocateInputBuffer()))
	    {
		YieldControlDeath();
		ErrorF("ReadRequestFromClient: cannot allocate buffer\n");
		return -1;
	    }
	    oc->input = oci;
	}

	request = (xReq *)oci->bufptr;
	gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
	if ((gotnow < sizeof(xReq)) ||
	    (gotnow < (needed = request_length(request, client))))
	{
	    if ((gotnow < sizeof(xReq)) || (needed < sizeof(xReq)))
	       needed = sizeof(xReq);
	    else if (needed > MAXBUFSIZE)
	    {
		YieldControlDeath();
		ErrorF("ReadRequestFromClient: request too big(%d)\n", needed);
		return -1;
	    }

	    /* Let's check for a read in progress */
	    if (ASIO_FD_ISSET(fd, ASIO_READ, &InprogressFdSet))
	    {
		YieldControlNoInput();
		return 0;
	    }
	    if ((gotnow == 0) ||
		((oci->bufptr - oci->buffer + needed) > oci->size))
	    {
		if ((gotnow > 0) && (oci->bufptr != oci->buffer))
		    bcopy(oci->bufptr, oci->buffer, gotnow);
		if (needed > oci->size)
		{
		    char *ibuf;

		    ibuf = (char *)xrealloc(oci->buffer, needed);
		    if (!ibuf)
		    {
			YieldControlDeath();
			return -1;
		    }
		    oci->size = needed;
		    oci->buffer = ibuf;
		}
		oci->bufptr = oci->buffer;
		oci->bufcnt = gotnow;
	    }
	    result = read(fd, oci->buffer + oci->bufcnt, 
			  oci->size - oci->bufcnt); 
	    if (result <= 0)
	    {
		if ((result < 0) && errno == EINPROGRESS)
		{
		    ASIO_FD_SET(fd, ASIO_READ, &InprogressFdSet);
		    ASIO_FD_SET(fd, ASIO_READ, &ClientFdSet);
		    YieldControlNoInput();
		    return 0;
		}
		YieldControlDeath();
		ErrorF("ReadRequestFromClient: read failed\n");
		ErrorF("result = %d; errno = %d; size-bufcnt = %d\n",
			result, errno, oci->size - oci->bufcnt);
		return -1;
	    }
	    oci->bufcnt += result;
	    gotnow += result;

	    /* free up some space after huge requests */
	    if ((oci->size > BUFWATERMARK) &&
		(oci->bufcnt < BUFSIZE) && (needed < BUFSIZE))
	    {
		char *ibuf;

		ibuf = (char *)xrealloc(oci->buffer, BUFSIZE);
		if (ibuf)
		{
		    oci->size = BUFSIZE;
		    oci->buffer = ibuf;
		    oci->bufptr = ibuf + oci->bufcnt - gotnow;
		}
	    }
	    continue;
	}
	else
		break;
    }

    if (needed < sizeof(xReq))
	needed = sizeof(xReq);

    if (++timesThisConnection >= MAX_TIMES_PER)
	YieldControl();

    client->requestBuffer = (pointer)oci->bufptr;
    oci->lenLastReq= needed;
    ASIO_FD_SET(fd, ASIO_READ, &CompletedFdSet);
    ASIO_FD_SET(fd, ASIO_READ, &ClientFdSet);
    AnyClientsWithInput= TRUE;
    return needed;
}

Bool
InsertFakeRequest(client, data, count)
    ClientPtr client;
    char *data;
    int count;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci;
    int fd = oc->fd;
    register int gotnow, moveup;

    oci= oc->input;
    if (oci)
    {
	oci->bufptr += oci->lenLastReq;
	oci->lenLastReq = 0;
    }
    oci= oc->inputFake;
    if (!oci)
    {
	if (oci = FreeInputs)
	    FreeInputs = oci->next;
	else if (!(oci = AllocateInputBuffer()))
	    return FALSE;
	oc->inputFake = oci;
    }
    oci->bufptr += oci->lenLastReq;
    oci->lenLastReq = 0;
    gotnow = oci->bufcnt + oci->buffer - oci->bufptr;
    if ((gotnow + count) > oci->size)
    {
	char *ibuf;

	ibuf = (char *)xrealloc(oci->buffer, gotnow + count);
	if (!ibuf)
	    return(FALSE);
	oci->size = gotnow + count;
	oci->buffer = ibuf;
	oci->bufptr = ibuf + oci->bufcnt - gotnow;
    }
    moveup = count - (oci->bufptr - oci->buffer);
    if (moveup > 0)
    {
	if (gotnow > 0)
	    bcopy(oci->bufptr, oci->bufptr + moveup, gotnow);
	oci->bufptr += moveup;
	oci->bufcnt += moveup;
    }
    bcopy(data, oci->bufptr - count, count);
    oci->bufptr -= count;
    ASIO_FD_SET(fd, ASIO_READ, &CompletedFdSet);
    AnyClientsWithInput= TRUE;
    return(TRUE);
}

ResetCurrentRequest(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    register ConnectionInputPtr oci;
    int fd = oc->fd;

    oci= oc->inputFake;
    if (oci)
	oci->lenLastReq = 0;
    oci= oc->input;
    if (oci)
	oci->lenLastReq = 0;
    ASIO_FD_SET(fd, ASIO_READ, &CompletedFdSet);
    AnyClientsWithInput= TRUE;
    YieldControl();
}

int
FlushClient(who, oc, extraBuf, extraCount)
    ClientPtr who;
    OsCommPtr oc;
    char *extraBuf;
    int extraCount; /* do not modify... returned below */
{
    register ConnectionOutputPtr oco;
    int fd = oc->fd;
    long padsize;
    char padBuffer[3];
    int newsize, r;

    ASIO_FD_CLR(fd, ASIO_WRITE, &CompletedFdSet);
    padsize = padlength[extraCount & 3];

    /* Insert new data in outputNext */
    if (extraCount + padsize != 0)
    {
	oco= oc->outputNext;
	if (!oco)
	{
	    if (oco = FreeOutputs)
	    {
		FreeOutputs = oco->next;
	    }
	    else if (!(oco = AllocateOutputBuffer()))
	    {
		close(oc->fd);
		CheckListeners();
		MarkClientException(who);
		return -1;
	    }
	    oc->outputNext = oco;
	}
	newsize= oco->count + extraCount + padsize;
	if (newsize > oco->size)
	{
	    unsigned char *obuf;

	    obuf= (unsigned char *)xrealloc(oco->buf, newsize + BUFSIZE);
	    if (obuf == NULL)
	    {
		close(oc->fd);
		CheckListeners();
		MarkClientException(who);
		oco->count = 0;
		return -1;
	    }
	    oco->buf= obuf;
	    oco->size= newsize + BUFSIZE;
	}
	bcopy(extraBuf, oco->buf + oco->count, extraCount);
	oco->count += extraCount;
	bcopy(padBuffer, oco->buf + oco->count, padsize);
	oco->count += padsize;
    }

    for (;;)
    {
	/* If we have a write in progress we can quit imediately */
	if (ASIO_FD_ISSET(fd, ASIO_WRITE, &InprogressFdSet))
		return extraCount;

	oco= oc->output;
	if (oco == NULL)
	{
		/* If we have no output buffer, but do have an outputNext,
		 * move outputNext to output and retry */

		if (oc->outputNext)
		{
			oc->output= oc->outputNext;
			oc->outputNext= NULL;
			continue;
		}

		/* We are done */
		return extraCount;
	}

	/* If the write buffer exists but is empty we can remove that buffer */
	if (oco->count == 0)
	{
	    if (oco->size > BUFWATERMARK)
	    {
		xfree(oco->buf);
		xfree(oco);
	    }
	    else
	    {
		oco->next = FreeOutputs;
		FreeOutputs = oco;
	    }
	    oc->output = (ConnectionOutputPtr)NULL;
	    continue;
	}

	/* We have some work to do */
	r= write(fd, (char *)oco->buf, oco->count);
	if (r > 0)
	{
		if (r == oco->count)
		{
			/* The normal case we assume */
			oco->count= 0;
			continue;
		}
		oco->count -= r;
		bcopy(oco->buf+r, oco->buf, oco->count);
		continue;
	}
	if (r == -1 && errno == EINPROGRESS)
	{
		ASIO_FD_SET(fd, ASIO_WRITE, &InprogressFdSet);
		ASIO_FD_SET(fd, ASIO_WRITE, &ClientFdSet);
		continue;
	}

	/* Now we got an error */
	close(fd);
	CheckListeners();
	MarkClientException(who);
	oco->count = 0;
	return(-1);
    }
}

void
FlushAllOutput()
{
    int i, index;
    register ClientPtr client;
    OsCommPtr oc;

    if (! NewOutputPending)
	return;

    /*
     * It may be that some client still has critical output pending,
     * but he is not yet ready to receive it anyway, so we will
     * simply wait for the select to tell us when he's ready to receive.
     */
    CriticalOutputPending = FALSE;
    NewOutputPending = FALSE;

    for (i = 0; i <= lastfdesc; i++)
    {
    	if (ASIO_FD_ISSET(i, ASIO_WRITE, &CompletedFdSet) &&
    		ASIO_FD_ISSET(i, ASIO_WRITE, &ClientFdSet))
	{
	    ASIO_FD_CLR(i, ASIO_WRITE, &CompletedFdSet);
	    if ((index = ConnectionTranslation[i]) == 0)
		continue;
	    client = clients[index];
	    if (client->clientGone)
		continue;
	    oc = (OsCommPtr)client->osPrivate;
	    if (ASIO_FD_ISSET(i, ASIO_READ, &CompletedFdSet))
	    {
		ASIO_FD_SET(i, ASIO_WRITE, &CompletedFdSet);
		NewOutputPending = TRUE;
	    }
	    else
		(void)FlushClient(client, oc, (char *)NULL, 0);
	}
    }
}

WriteToClient (who, count, buf)
    ClientPtr who;
    int count;
    char *buf;
{
    OsCommPtr oc = (OsCommPtr)who->osPrivate;
    register ConnectionOutputPtr oco;
    int padBytes;

    if (!count)
	return(0);

    oco= oc->output;
    if (!oco)
    {
	if (oco = FreeOutputs)
	{
	    FreeOutputs = oco->next;
	}
	else if (!(oco = AllocateOutputBuffer()))
	{
	    close(oc->fd);
	    CheckListeners();
	    MarkClientException(who);
	    return -1;
	}
	oc->output = oco;
    }
    if (oc->outputNext)
    	oco= oc->outputNext;

    padBytes =  padlength[count & 3];

    if (oco->count + count + padBytes > oco->size)
    {
	CriticalOutputPending = FALSE;
	NewOutputPending = FALSE;
	return FlushClient(who, oc, buf, count);
    }

    NewOutputPending = TRUE;
    ASIO_FD_SET(oc->fd, ASIO_WRITE, &CompletedFdSet);
    ASIO_FD_SET(oc->fd, ASIO_WRITE, &ClientFdSet);
    bcopy(buf, (char *)oco->buf + oco->count, count);
    oco->count += count + padBytes;
    
    return(count);
}

void
UpdateClientIOStatus(fd, operation, result, error)
int fd;
int operation;
int result;
int error;
{
	int client_no;
	ClientPtr client;
	OsCommPtr oc;
	ConnectionInputPtr oci;
	ConnectionOutputPtr oco;

	ASIO_FD_CLR(fd, operation, &InprogressFdSet);
	client_no= ConnectionTranslation[fd];
	client= clients[client_no];
	oc= (OsCommPtr)client->osPrivate;

	switch (operation) {
	case ASIO_READ:
		AnyClientsWithInput= TRUE;
		ASIO_FD_SET(fd, ASIO_READ, &CompletedFdSet);
		if (result == -1)
		{
		    /* Assume the error will happen again on the next read */
		    ErrorF("(warning) read error: %s\n", strerror(error));
		    return;
		}
		oci= oc->input;
		oci->bufcnt += result;
		break;
	case ASIO_WRITE:
		if (result == -1)
		{
		    /* Assume the error will happen again on the next write */
		    ErrorF("(warning) write error: %s\n", strerror(error));
		    return;
		}
		oco= oc->output;
		oco->count -= result;
		if (oco->count != 0)
		    bcopy(oco->buf+result, oco->buf, oco->count);
		FlushClient(client, oc, NULL, 0);
		break;
	default:
		FatalError("UpdateClientIOStatus: oper %d not implemented\n",
			   operation);
	}
}
#endif /* _MINIX */
