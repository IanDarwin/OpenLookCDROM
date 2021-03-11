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
 * $NCDId: @(#)Alibint.c,v 1.23 1994/06/03 17:07:58 greg Exp $
 */

/* Portions derived from */
/*
 * $XConsortium: XlibInt.c,v 11.163 92/07/24 17:34:55 rws Exp $
 */

/* Copyright    Massachusetts Institute of Technology    1985, 1986, 1987 */

/*
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

/*
 *	AlibInt.c - Internal support routines for the C subroutine
 *	interface library (audiolib) to the NCD-AUDIO Protocol V1.0.
 */

#ifdef NETAUDIO
#include "release.h"
#endif /* NETAUDIO */

#include <audio/Alibint.h>
#include <audio/Aos.h>
#include "Alibnet.h"
#include <stdio.h>
#ifdef SYSV	/* fd_set */
#include <sys/socket.h>
#endif

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

#ifdef LACHMAN
#ifdef EMSGSIZE
#undef EMSGSIZE
#endif
#define EMSGSIZE ERANGE
#endif

#ifdef MUSTCOPY

#define STARTITERATE(tpvar,type,start,endcond) \
  { register char *cpvar; \
  for (cpvar = (char *) (start); endcond; ) { \
    type dummy; bcopy (cpvar, (char *) &dummy, SIZEOF(type)); \
    tpvar = &dummy;
#define ITERPTR(tpvar) cpvar
#define RESETITERPTR(tpvar,type,start) cpvar = start
#define INCITERPTR(tpvar,type) cpvar += SIZEOF(type)
#define ENDITERATE }}

#else

#define STARTITERATE(tpvar,type,start,endcond) \
  for (tpvar = (type *) (start); endcond; )
#define ITERPTR(tpvar) (char *)tpvar
#define RESETITERPTR(tpvar,type,start) tpvar = (type *) (start)
#define INCITERPTR(tpvar,type) tpvar++
#define ENDITERATE

#endif /* MUSTCOPY */

typedef union {
    auReply rep;
    char buf[BUFSIZE];
} _AuAlignedBuffer;

static char *_AuAsyncReply (
#if NeedFunctionPrototypes
    AuServer *,		/* server */
    auReply *,		/* rep */
    char *,		/* buf */
    int *, 		/* lenp */
    AuBool		/* discard */
#endif
);

static void _AuEnq(
#if NeedFunctionPrototypes
    AuServer *,			/* server */
    auEvent *,			/* event */
    int				/* who */
#endif
);

/*
 * The following routines are internal routines used by audiolib for protocol
 * packet transmission and reception.
 *
 * _AuIOError(AuServer *) will be called if any sort of system call error occurs.
 * This is assumed to be a fatal condition, i.e., AuIOError should not return.
 *
 * _AuError(AuServer *, auError *) will be called whenever an Au_Error event is
 * received.  This is not assumed to be a fatal condition, i.e., it is
 * acceptable for this procedure to return.  However, AuError should NOT
 * perform any operations (directly or indirectly) on the DISPLAY.
 *
 * Routines declared with a return type of 'AuStatus' return 0 on failure,
 * and non 0 on success.  Routines with no declared return type don't 
 * return anything.  Whenever possible routines that create objects return
 * the object they have created.
 */

static int padlength[4] = {0, 3, 2, 1};
    /* lookup table for adding padding bytes to data that is read from
    	or written to the Au socket.  */

static auReq _dummy_request = {
	0, 0, 0
};

/*
 * This is an OS dependent routine which:
 * 1) returns as soon as the connection can be written on....
 * 2) if the connection can be read, must enqueue events and handle errors,
 * until the connection is writable.
 */
static void
_AuWaitForWritable(aud)
    AuServer *aud;
{
#if !defined(AMOEBA) && !defined(_MINIX)
    AuInt32 r_mask[MSKCNT];
    AuInt32 w_mask[MSKCNT];
    int nfound;

    CLEARBITS(r_mask);
    CLEARBITS(w_mask);

    while (1) {
	BITSET(r_mask, aud->fd);
        BITSET(w_mask, aud->fd);

	do {
	    nfound = select (aud->fd + 1, (fd_set *)r_mask, (fd_set *)w_mask,
			     (fd_set *)NULL, (struct timeval *)NULL);
	    if (nfound < 0 && errno != EINTR)
		_AuIOError(aud);
	} while (nfound <= 0);

	if (_AuANYSET(r_mask)) {
	    _AuAlignedBuffer buf;
	    int pend;
	    register int len;
	    register auReply *rep;

	    /* find out how much data can be read */
	    if (BytesReadable(aud->fd, (char *) &pend) < 0)
		_AuIOError(aud);
	    len = pend;

	    /* must read at least one auEvent; if none is pending, then
	       we'll just block waiting for it */
	    if (len < SIZEOF(auReply)) len = SIZEOF(auReply);
		
	    /* but we won't read more than the max buffer size */
	    if (len > BUFSIZE) len = BUFSIZE;

	    /* round down to an integral number of AuReps */
	    len = (len / SIZEOF(auReply)) * SIZEOF(auReply);

	    _AuRead (aud, buf.buf, (AuInt32) len);

	    STARTITERATE(rep,auReply,buf.buf,len > 0) {
		if (rep->generic.type == Au_Reply) {
		    pend = len;
		    RESETITERPTR(rep,auReply,
				 _AuAsyncReply (aud, rep,
					       ITERPTR(rep), &pend, AuTrue));
		    len = pend;
		} else {
		    if (rep->generic.type == Au_Error)
			_AuError (aud, (auError *)rep);
		    else	/* must be an event packet */
			_AuEnq (aud, (auEvent *)rep, AuEventEnqueuedByUnknown);
		    INCITERPTR(rep,auReply);
		    len -= SIZEOF(auReply);
		}
	    } ENDITERATE
	}
	if (_AuANYSET(w_mask))
	    return;
    }
#else /* AMOEBA || _MINIX */
    /* Its a fatal error when this is called under Amoeba or Minix */
    printf(stderr, "audiolib: _AuWaitForWritable called\n");
    abort();
#endif /* AMOEBA || _MINIX */
}


static void
_AuWaitForReadable(aud)
  AuServer *aud;
{
#if !defined(AMOEBA) && !defined(_MINIX)
    AuUint32 r_mask[MSKCNT];
    int result;
	
    CLEARBITS(r_mask);
    do {
	BITSET(r_mask, aud->fd);
	result = select(aud->fd + 1, (fd_set *)r_mask,
			(fd_set *)NULL, (fd_set *)NULL, (struct timeval *)NULL);
	if (result == -1 && errno != EINTR) _AuIOError(aud);
    } while (result <= 0);
#else /* AMOEBA || _MINIX */
    /* Its a fatal error when this is called under Amoeba or Minix */
    printf(stderr, "audiolib: _AuWaitForReadable called\n");
    abort();
#endif /* AMOEBA || _MINIX */
}


/*
 * _AuFlush - Flush the Au request buffer.  If the buffer is empty, no
 * action is taken.  This routine correctly handles incremental writes.
 * This routine may have to be reworked if int < AuInt32.
 */
void
_AuFlush (aud)
	register AuServer *aud;
{
	register AuInt32 size, todo;
	register int write_stat;
	register char *bufindex;

	if (aud->flags & AuServerFlagsIOError) return;

	size = todo = aud->bufptr - aud->buffer;
	bufindex = aud->bufptr = aud->buffer;
	/*
	 * While write has not written the entire buffer, keep looping
	 * until the entire buffer is written.  bufindex will be incremented
	 * and size decremented as buffer is written out.
	 */
	while (size) {
	    errno = 0;
	    write_stat = WriteToServer(aud->fd, bufindex, (int) todo);
	    if (write_stat >= 0) {
		size -= write_stat;
		todo = size;
		bufindex += write_stat;
	    } else if (ETEST(errno)) {
		_AuWaitForWritable(aud);
#ifdef SUNSYSV
	    } else if (errno == 0) {
		_AuWaitForWritable(aud);
#endif
#ifdef EMSGSIZE
	    } else if (errno == EMSGSIZE) {
		if (todo > 1) 
		  todo >>= 1;
		else
		  _AuWaitForWritable(aud);
#endif
	    } else if (errno != EINTR) {
		/* Write failed! */
		/* errno set by write system call. */
		_AuIOError(aud);
	    }
	}
	aud->last_req = (char *)&_dummy_request;
}

int
_AuEventsQueued (aud, mode)
    register AuServer *aud;
    int mode;
{
	register int len;
	int pend;
	_AuAlignedBuffer buf;
	register auReply *rep;
	
	if (mode == AuEventsQueuedAfterFlush)
	{
	    _AuFlush(aud);
	    if (aud->qlen)
		return(aud->qlen);
	}
	if (aud->flags & AuServerFlagsIOError) return(aud->qlen);
	if (BytesReadable(aud->fd, (char *) &pend) < 0)
	    _AuIOError(aud);
#ifdef AUCONN_CHECK_FREQ
	/* This is a crock, required because FIONREAD or equivalent is
	 * not guaranteed to detect a broken connection.
	 */
	if (!pend && !aud->qlen && ++aud->conn_checker >= AUCONN_CHECK_FREQ)
	{
	    AuUint32 r_mask[MSKCNT];
	    static struct timeval zero_time;

	    aud->conn_checker = 0;
	    CLEARBITS(r_mask);
	    BITSET(r_mask, aud->fd);
	    if ((pend = select(aud->fd + 1, (fd_set *)r_mask, 
		(fd_set *)NULL, (fd_set *)NULL,
			      &zero_time)) != 0)
	    {
		if (pend > 0)
		{
		    if (BytesReadable(aud->fd, (char *) &pend) < 0)
			_AuIOError(aud);
		    /* we should not get zero, if we do, force a read */
		    if (!pend)
			pend = SIZEOF(auReply);
		}
		else if (pend < 0 && errno != EINTR)
		    _AuIOError(aud);
	    }
	}
#endif /* AUCONN_CHECK_FREQ */
	if (!(len = pend))
	    return(aud->qlen);	/* _AuFlush can enqueue events */
      /* Force a read if there is not enough data.  Otherwise,
       * a select() loop at a higher-level will spin undesirably,
       * and we've seen at least one OS that appears to not update
       * the result from FIONREAD once it has returned nonzero.
       */
	if (len < SIZEOF(auReply))
	    len = SIZEOF(auReply);
	else if (len > BUFSIZE)
	    len = BUFSIZE;
	len = (len / SIZEOF(auReply)) * SIZEOF(auReply);
#ifdef AUCONN_CHECK_FREQ
	aud->conn_checker = 0;
#endif
	_AuRead (aud, buf.buf, (AuInt32) len);

	STARTITERATE(rep,auReply,buf.buf,len > 0) {
	    if (rep->generic.type == Au_Reply) {
		pend = len;
		RESETITERPTR(rep,auReply,
			     _AuAsyncReply (aud, rep,
					   ITERPTR(rep), &pend, AuTrue));
		len = pend;
	    } else {
		if (rep->generic.type == Au_Error)
		    _AuError (aud, (auError *)rep);
		else   /* must be an event packet */
		    _AuEnq (aud, (auEvent *)rep, AuEventEnqueuedByUnknown);
		INCITERPTR(rep,auReply);
		len -= SIZEOF(auReply);
	    }
	} ENDITERATE
	return(aud->qlen);
}

/* _AuReadEvents - Flush the output queue,
 * then read as many events as possible (but at least 1) and enqueue them
 */
void
_AuReadEvents(aud)
	register AuServer *aud;
{
	_AuAlignedBuffer buf;
	int pend;
	register int len;
	register auReply *rep;
	AuBool not_yet_flushed = AuTrue;

	do {
	    /* find out how much data can be read */
	    if (BytesReadable(aud->fd, (char *) &pend) < 0)
	    	_AuIOError(aud);
	    len = pend;

	    /* must read at least one auEvent; if none is pending, then
	       we'll just flush and block waiting for it */
	    if (len < SIZEOF(auEvent)) {
	    	len = SIZEOF(auEvent);
		/* don't flush until we block the first time */
		if (not_yet_flushed) {
		    int qlen = aud->qlen;
		    _AuFlush (aud);
		    if (qlen != aud->qlen) return;
		    not_yet_flushed = AuFalse;
		}
	    }
		
	    /* but we won't read more than the max buffer size */
	    if (len > BUFSIZE)
	    	len = BUFSIZE;

	    /* round down to an integral number of AuReps */
	    len = (len / SIZEOF(auEvent)) * SIZEOF(auEvent);

	    _AuRead (aud, buf.buf, (AuInt32) len);

	    STARTITERATE(rep,auReply,buf.buf,len > 0) {
		if (rep->generic.type == Au_Reply) {
		    pend = len;
		    RESETITERPTR(rep,auReply,
				 _AuAsyncReply (aud, rep,
					       ITERPTR(rep), &pend, AuTrue));
		    len = pend;
		} else {
		    if (rep->generic.type == Au_Error)
			_AuError (aud, (auError *) rep);
		    else   /* must be an event packet */
			_AuEnq (aud, (auEvent *)rep, AuEventEnqueuedByUnknown);
		    INCITERPTR(rep,auReply);
		    len -= SIZEOF(auReply);
		}
	    } ENDITERATE
	} while (aud->head == NULL);
}

/* 
 * _AuRead - Read bytes from the socket taking into account incomplete
 * reads.  This routine may have to be reworked if int < AuInt32.
 */
void
_AuRead (aud, data, size)
	register AuServer *aud;
	register char *data;
	register AuInt32 size;
{
	register AuInt32 bytes_read;

	if ((aud->flags & AuServerFlagsIOError) || size == 0) return;
	errno = 0;
	while ((bytes_read = ReadFromServer(aud->fd, data, (int)size))
		!= size) {

	    	if (bytes_read > 0) {
		    size -= bytes_read;
		    data += bytes_read;
		    }
		else if (ETEST(errno)) {
		    _AuWaitForReadable(aud);
		    errno = 0;
		}
#ifdef SUNSYSV
		else if (errno == 0) {
		    _AuWaitForReadable(aud);
		}
#endif
		else if (bytes_read == 0) {
		    /* Read failed because of end of file! */
		    errno = EPIPE;
		    _AuIOError(aud);
		    }

		else  /* bytes_read is less than 0; presumably -1 */ {
		    /* If it's a system call interrupt, it's not an error. */
		    if (errno != EINTR)
		    	_AuIOError(aud);
		    }
	    	 }
}

#ifdef WORD64

/*
 * XXX This is a *really* stupid way of doing this....
 * PACKBUFFERSIZE must be a multiple of 4.
 */

#define PACKBUFFERSIZE 4096


/*
 * _AuRead32 - Read bytes from the socket unpacking each 32 bits
 *            into a AuInt32 (64 bits on a CRAY computer).
 * 
 */
static _doXRead32 (aud, data, size, packbuffer)
        register AuServer *aud;
        register AuInt32 *data;
        register AuInt32 size;
	register char *packbuffer;
{
 AuInt32 *lpack,*lp;
 AuInt32 mask32 = 0x00000000ffffffff;
 AuInt32 maskw, nwords, i, bits;

        _AuReadPad (aud, packbuffer, size);

        lp = data;
        lpack = (AuInt32 *) packbuffer;
        nwords = size >> 2;
        bits = 32;

        for(i=0;i<nwords;i++){
            maskw = mask32 << bits;
           *lp++ = ( *lpack & maskw ) >> bits;
            bits = bits ^32;
            if(bits){
               lpack++;
            }
        }
}

_AuRead32 (aud, data, len)
    AuServer *aud;
    AuInt32 *data;
    AuInt32 len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 2;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	_doXRead32 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) _doXRead32 (aud, data, len, packbuffer);
}



/*
 * _AuRead16 - Read bytes from the socket unpacking each 16 bits
 *            into a AuInt32 (64 bits on a CRAY computer).
 *
 */
static _doXRead16 (aud, data, size, packbuffer)
        register AuServer *aud;
        register short *data;
        register AuInt32 size;
	char *packbuffer;
{
	AuInt32 *lpack,*lp;
	AuInt32 mask16 = 0x000000000000ffff;
	AuInt32 maskw, nwords, i, bits;

        _AuRead(aud,packbuffer,size);	/* don't do a padded read... */

        lp = (AuInt32 *) data;
        lpack = (AuInt32 *) packbuffer;
        nwords = size >> 1;  /* number of 16 bit words to be unpacked */
        bits = 48;
        for(i=0;i<nwords;i++){
            maskw = mask16 << bits;
           *lp++ = ( *lpack & maskw ) >> bits;
            bits -= 16;
            if(bits < 0){
               lpack++;
               bits = 48;
            }
        }
}

_AuRead16 (aud, data, len)
    AuServer *aud;
    short *data;
    AuInt32 len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 1;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	_doXRead16 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) _doXRead16 (aud, data, len, packbuffer);
}

_AuRead16Pad (aud, data, size)
    AuServer *aud;
    short *data;
    AuInt32 size;
{
    int slop = (size & 3);
    short slopbuf[3];

    _AuRead16 (aud, data, size);
    if (slop > 0) {
	_AuRead16 (aud, slopbuf, 4 - slop);
    }
}
#endif /* WORD64 */


/*
 * _AuReadPad - Read bytes from the socket taking into account incomplete
 * reads.  If the number of bytes is not 0 mod 4, read additional pad
 * bytes. This routine may have to be reworked if int < AuInt32.
 */
void
_AuReadPad (aud, data, size)
    	register AuServer *aud;	
	register char *data;
	register AuInt32 size;
{
    	register AuInt32 bytes_read;
	struct iovec iov[2];
	char pad[3];

	if ((aud->flags & AuServerFlagsIOError) || size == 0) return;
	iov[0].iov_len = (int)size;
	iov[0].iov_base = data;
	/* 
	 * The following hack is used to provide 32 bit AuInt32-word
	 * aligned padding.  The [1] vector is of length 0, 1, 2, or 3,
	 * whatever is needed.
	 */

	iov[1].iov_len = padlength[size & 3];
	iov[1].iov_base = pad;
	size += iov[1].iov_len;
	errno = 0;
	while ((bytes_read = ReadvFromServer (aud->fd, iov, 2)) != size) {

	    if (bytes_read > 0) {
		size -= bytes_read;
	    	if ((iov[0].iov_len -= bytes_read) < 0) {
		    iov[1].iov_len += iov[0].iov_len;
		    iov[1].iov_base -= iov[0].iov_len;
		    iov[0].iov_len = 0;
		    }
	    	else
	    	    iov[0].iov_base += bytes_read;
	    	}
	    else if (ETEST(errno)) {
		_AuWaitForReadable(aud);
		errno = 0;
	    }
#ifdef SUNSYSV
	    else if (errno == 0) {
		_AuWaitForReadable(aud);
	    }
#endif
	    else if (bytes_read == 0) {
		/* Read failed because of end of file! */
		errno = EPIPE;
		_AuIOError(aud);
		}
	    
	    else  /* bytes_read is less than 0; presumably -1 */ {
		/* If it's a system call interrupt, it's not an error. */
		if (errno != EINTR)
		    _AuIOError(aud);
		}
	    }
}

/*
 * _AuSend - Flush the buffer and send the client data. 32 bit word aligned
 * transmission is used, if size is not 0 mod 4, extra bytes are transmitted.
 * This routine may have to be reworked if int < AuInt32;
 */
void
_AuSend (aud, data, size)
	register AuServer *aud;
	char *data;
	register AuInt32 size;
{
	struct iovec iov[3];
	static char pad[3] = {0, 0, 0};
           /* XText8 and XText16 require that the padding bytes be zero! */

	AuInt32 skip = 0;
	AuInt32 audbufsize = (aud->bufptr - aud->buffer);
	AuInt32 padsize = padlength[size & 3];
	AuInt32 total = audbufsize + size + padsize;
	AuInt32 todo = total;

	if (aud->flags & AuServerFlagsIOError) return;

	/*
	 * There are 3 pieces that may need to be written out:
	 *
	 *     o  whatever is in the server buffer
	 *     o  the data passed in by the user
	 *     o  any padding needed to 32bit align the whole mess
	 *
	 * This loop looks at all 3 pieces each time through.  It uses skip
	 * to figure out whether or not a given piece is needed.
	 */
	while (total) {
	    AuInt32 before = skip;		/* amount of whole thing written */
	    AuInt32 remain = todo;		/* amount to try this time, <= total */
	    int i = 0;
	    AuInt32 len;

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

	    InsertIOV (aud->buffer, audbufsize)
	    InsertIOV (data, size)
	    InsertIOV (pad, padsize)
    
	    errno = 0;
	    if ((len = WritevToServer(aud->fd, iov, i)) >= 0) {
		skip += len;
		total -= len;
		todo = total;
	    } else if (ETEST(errno)) {
		_AuWaitForWritable(aud);
#ifdef SUNSYSV
	    } else if (errno == 0) {
		_AuWaitForWritable(aud);
#endif
#ifdef EMSGSIZE
	    } else if (errno == EMSGSIZE) {
		if (todo > 1) 
		  todo >>= 1;
		else 
		  _AuWaitForWritable(aud);
#endif
	    } else if (errno != EINTR) {
		_AuIOError(aud);
	    }
	}

	aud->bufptr = aud->buffer;
	aud->last_req = (char *) & _dummy_request;
}

/*
 * _AuAllocID - normal resource ID allocation routine.  A client
 * can roll his own and instatantiate it if he wants, but must
 * follow the rules.
 */
AuID _AuAllocID (aud)
    register AuServer *aud;
{
   AuID id;

   id = aud->resource_id << aud->resource_shift;
   if (id <= aud->resource_mask) {
       aud->resource_id++;
       return (aud->resource_base + id);
   }
   if (id != 0x10000000) {
       (void) fprintf(stderr,
		      "audiolib: resource ID allocation space exhausted!\n");
       id = 0x10000000;
       aud->resource_id = id >> aud->resource_shift;
   }
   return id;
}

/*
 * The hard part about this is that we only get 16 bits from a reply.  Well,
 * then, we have three values that will march aAuInt32, with the following
 * invariant:
 *	aud->last_request_read <= rep->sequenceNumber <= aud->request
 * The right choice for rep->sequenceNumber is the largest that
 * still meets these constraints.
 */

AuUint32
_AuSetLastRequestRead(aud, rep)
    register AuServer *aud;
    register auGenericReply *rep;
{
    register AuUint32	newseq, lastseq;

    newseq = (aud->last_request_read & ~((AuUint32)0xffff)) |
	     rep->sequenceNumber;
    lastseq = aud->last_request_read;
    while (newseq < lastseq) {
	newseq += 0x10000;
	if (newseq > aud->request) {
	    (void) fprintf (stderr, 
	    "audiolib: sequence lost (0x%lx > 0x%lx) in reply type 0x%x!\n",
			    newseq, aud->request, 
			    (unsigned int) rep->type);
	    newseq -= 0x10000;
	    break;
	}
    }

    aud->last_request_read = newseq;
    return(newseq);
}

/*
 * _AuReply - Wait for a reply packet and copy its contents into the
 * specified rep.  Mean while we must handle error and event packets that
 * we may encounter.
 */
AuBool _AuReply (aud, rep, extra, discard, ret_status)
    register AuServer *aud;
    register auReply *rep;
    int extra;		/* number of 32-bit words expected after the reply */
    AuBool discard;	/* should I discard data following "extra" words? */
    AuStatus *ret_status;
{
    /* Pull out the serial number now, so that (currently illegal) requests
     * generated by an error handler don't confuse us.
     */
    AuUint32 cur_request = aud->request;
    AuStatus tmpstatus;

    if (!ret_status)
	ret_status = &tmpstatus;

    *ret_status = AuSuccess;

    if (aud->flags & AuServerFlagsIOError) {
	*ret_status = AuBadConnection;
	return AuFalse;
    }

    _AuFlush(aud);
    while (1) {
	_AuRead(aud, (char *)rep, (AuInt32)SIZEOF(auReply));
	switch ((int)rep->generic.type) {

	    case Au_Reply:
	        /* Reply received.  Fast update for synchronous replies,
		 * but deal with multiple outstanding replies.
		 */
	        if (rep->generic.sequenceNumber == (cur_request & 0xffff))
		    aud->last_request_read = cur_request;
		else {
		    int pend = SIZEOF(auReply);
		    if (_AuAsyncReply(aud, rep, (char *)rep, &pend, AuFalse)
			!= (char *)rep)
			continue;
		}
		if (extra == 0) {
		    if (discard && (rep->generic.length > 0))
		       /* unexpectedly AuInt32 reply! */
		       _AuEatData (aud, rep->generic.length << 2);
		    return AuTrue;
		    }
		if (extra == rep->generic.length) {
		    /* 
		     * Read the extra data into storage immediately following
		     * the GenericReply structure. 
		     */
		    _AuRead (aud, (char *) (NEXTPTR(rep,auReply)),
			    ((AuInt32)extra) << 2);
		    return AuTrue;
		    }
		if (extra < rep->generic.length) {
		    /* Actual reply is AuInt32er than "extra" */
		    _AuRead (aud, (char *) (NEXTPTR(rep,auReply)),
			    ((AuInt32)extra) << 2);
		    if (discard)
		        _AuEatData (aud, (rep->generic.length - extra) << 2);
		    return AuTrue;
		    }
		/* 
		 *if we get here, then extra > rep->generic.length--meaning we
		 * read a reply that's shorter than we expected.  This is an 
		 * error,  but we still need to figure out how to handle it...
		 */
		_AuRead (aud, (char *) (NEXTPTR(rep,auReply)),
			((AuInt32) rep->generic.length) << 2);
		_AuIOError (aud);
		*ret_status = AuBadConnection;
		return AuFalse;

    	    case Au_Error:
	    	{
	        register _AuExtension *ext;
		register AuBool ret = AuFalse;
		AuBool ret_code = AuFalse;
		auError *err = (auError *) rep;
		AuUint32 serial;

		*ret_status = (int) err->errorCode;
		serial = _AuSetLastRequestRead(aud, (auGenericReply *)rep);
		if (serial == cur_request) {
		    /*
		     * Don't die if the caller asked for a ret_status
		     */
		    if (ret_status != &tmpstatus)
			return AuFalse;

#ifdef checkforanyreqsthatexpectreplies
		    /* do not die on "no such font", "can't allocate",
		       "can't grab" failures */
		    switch ((int)err->errorCode) {
		      case AuBadName:
		      case AuBadAlloc:
		      case AuBadAccess:
			return AuFalse;
		    }
#endif
		}
		/* 
		 * we better see if there is an extension who may
		 * want to suppress the error.
		 */
		for (ext = aud->ext_procs; !ret && ext; ext = ext->next) {
		    if (ext->error) 
		       ret = (*ext->error)(aud, err, &ext->codes, &ret_code);
		}
		if (!ret) {
		    _AuError(aud, err);
		    ret_code = AuFalse;
		}
		if (serial == cur_request)
		    return ret_code;
		}
		break;
	    default:
		_AuEnq(aud, (auEvent *) rep, AuEventEnqueuedByReply);
		break;
	    }
	}
}   

static char *
_AuAsyncReply(aud, rep, buf, lenp, discard)
    AuServer *aud;
    register auReply *rep;
    char *buf;
    register int *lenp;
    AuBool discard;
{
    register _AuAsyncHandler *async, *next;
    register int len;
    register AuBool consumed = AuFalse;
    char *nbuf;

    (void) _AuSetLastRequestRead(aud, &rep->generic);
    len = SIZEOF(auReply) + (rep->generic.length << 2);

    for (async = aud->async_handlers; async; async = next) {
	next = async->next;
	if ((consumed = (*async->handler)(aud, rep, buf, *lenp, async->data)) != 0)
	    break;
    }
    if (!consumed) {
	if (!discard)
	    return buf;
	(void) fprintf(stderr, 
		       "audiolib: unexpected async reply (sequence 0x%lx)!\n",
		       aud->last_request_read);
	if (len > *lenp)
	    _AuEatData(aud, len - *lenp);
    }
    if (len >= *lenp) {
	buf += *lenp;
	*lenp = 0;
	return buf;
    }
    *lenp -= len;
    buf += len;
    len = *lenp;
    nbuf = buf;
    while (len > SIZEOF(auReply)) {
	if (*buf == Au_Reply)
	    return nbuf;
	buf += SIZEOF(auReply);
	len -= SIZEOF(auReply);
    }
    if (len > 0 && len < SIZEOF(auReply)) {
	buf = nbuf;
	len = SIZEOF(auReply) - len;
	nbuf -= len;
	bcopy(buf, nbuf, *lenp);
	_AuRead(aud, nbuf + *lenp, (AuInt32)len);
	*lenp += len;
    }
    return nbuf;
}

/*
 * _AuForceRoundTrip - send the request that has just been queued and watch
 * for any errors that it might generate.  Callers should set
 * error_code, major, and minor all to 0 to catch all errors.
 */
AuBool
_AuForceRoundTrip (aud, error_code, majorop, minorop, ret_status)
    AuServer *aud;
    int error_code;
    int majorop;
    int minorop;
    AuStatus *ret_status;
{
    AuUint32 seq = aud->request;	 /* get previous request */
    auGetCloseDownModeReply rep;
    register auReq *req;
    _AuAsyncHandler async;
    _AuAsyncErrorState async_state;

    async_state.min_sequence_number = seq;
    async_state.max_sequence_number = seq;
    async_state.error_code = error_code;
    async_state.major_opcode = majorop;
    async_state.minor_opcode = minorop;
    async_state.error_count = 0;

    _AuEnqAsyncHandler (aud, &async, _AuAsyncErrorHandler, &async_state);

    _AuGetEmptyReq(GetCloseDownMode, req, aud);    /* AuSync() */
    (void) _AuReply (aud, (auReply *)&rep, 0, auTrue, (AuStatus *) NULL);

    _AuDeqAsyncHandler(aud, &async);

    if (ret_status) {
	if (async_state.error_count > 0)
	    *ret_status = async_state.last_error_received;
	else
	    *ret_status = AuSuccess;
    }
    return (async_state.error_count == 0 ? AuTrue : AuFalse);
}




/* Read and discard "n" 8-bit bytes of data */

void _AuEatData (aud, n)
    AuServer *aud;
    register AuUint32 n;
{
#define SCRATCHSIZE 2048
    char buf[SCRATCHSIZE];

    while (n > 0) {
	register AuInt32 bytes_read = (n > SCRATCHSIZE) ? SCRATCHSIZE : n;
	_AuRead (aud, buf, bytes_read);
	n -= bytes_read;
    }
#undef SCRATCHSIZE
}

AuEventEnqHandlerRec *
AuRegisterEventEnqHandler(aud, who, callback, data)
AuServer       *aud;
int             who;
AuEventEnqHandlerCallback callback;
AuPointer       data;
{
    AuEventEnqHandlerRec *handler;

    if (!(handler =
	  (AuEventEnqHandlerRec *) Aumalloc(sizeof(AuEventEnqHandlerRec))))
	return NULL;

    handler->who = who;
    handler->callback = callback;
    handler->data = data;

    _AuAddToLinkedList(aud->eventenqhandlerq, handler);

    return handler;
}

void
AuUnregisterEventEnqHandler(aud, handler)
AuServer       *aud;
AuEventEnqHandlerRec *handler;
{
    _AuRemoveFromLinkedList(aud->eventenqhandlerq, handler);
    Aufree(handler);
}

static void
_AuEventEnqueued(aud, who, event)
AuServer       *aud;
int             who;
AuEvent        *event;
{
    AuEventEnqHandlerRec *p = aud->eventenqhandlerq,
                   *next;

    while (p)
    {
	next = p->next;
	if (p->who == AuEventEnqueuedByAny || p->who == who)
	    (*p->callback) (aud, p, event, p->data);
	p = next;
    }
}

/*
 * _AuEnq - Place event packets on the server's queue.
 */
static void
_AuEnq (aud, event, who)
	register AuServer *aud;
	register auEvent *event;
        int who;
{
	register _AuQEvent *qelt;

/*NOSTRICT*/
	if ((qelt = aud->qfree) != 0) {
		/* If aud->qfree is non-NULL do this, else malloc a new one. */
		aud->qfree = qelt->next;
	}
	else if ((qelt = 
	    (_AuQEvent *) Aumalloc((unsigned)sizeof(_AuQEvent))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		_AuIOError(aud);
	}
	qelt->next = NULL;
	/* go call through server to find proper event reformatter */
	if ((*aud->event_vec[event->u.u.type & 0177])(aud, &qelt->event, event)) {
	    if (aud->tail)	aud->tail->next = qelt;
	    else 		aud->head = qelt;
    
	    aud->tail = qelt;
	    aud->qlen++;
	    _AuEventEnqueued(aud, who, qelt->event);
	} else {
	    /* ignored, or stashed away for many-to-one compression */
	    qelt->next = aud->qfree;
	    aud->qfree = qelt;
	}
}
/*
 * EventToWire in separate file in that often not needed.
 */

/*ARGSUSED*/
AuBool
_AuUnknownWireEvent(aud, re, event)
register AuServer *aud;	/* pointer to server structure */
register AuEvent *re;	/* pointer to where event should be reformatted */
register auEvent *event;	/* wire protocol event */
{
#ifdef notdef
	(void) fprintf(stderr, 
	    "audiolib: unhandled wire event! event number = %d, server = %x\n.",
			event->u.u.type, aud);
#endif
	return(AuFalse);
}

/*ARGSUSED*/
AuStatus
_AuUnknownNativeEvent(aud, re, event)
register AuServer *aud;	/* pointer to server structure */
register AuEvent *re;	/* pointer to where event should be reformatted */
register auEvent *event;	/* wire protocol event */
{
#ifdef notdef
	(void) fprintf(stderr, 
 	   "audiolib: unhandled native event! event number = %d, server = %x\n.",
			re->type, aud);
#endif
	return(0);
}
/*
 * reformat a wire event into an AuEvent structure of the right type.
 */
AuBool
_AuWireToEvent(aud, re, event)
register AuServer *aud;				/* pointer to server
						 * structure */
register AuEvent *re;				/* pointer to where event
						 * should be reformatted */
register auEvent *event;			/* wire protocol event */
{

    re->type = event->u.u.type & 0x7f;
    ((AuAnyEvent *) re)->serial = _AuSetLastRequestRead(aud,
						  (auGenericReply *) event);
    ((AuAnyEvent *) re)->send_event = ((event->u.u.type & 0x80) != 0);
    ((AuAnyEvent *) re)->server = aud;
    ((AuAnyEvent *) re)->time = event->u.u.time;

    /*
     * Ignore the leading bit of the event type since it is set when a client
     * sends an event rather than the server.
     */

    switch (event->u.u.type & 0177)
    {
	case AuEventTypeElementNotify:
	    {
		register AuElementNotifyEvent *ev =
		(AuElementNotifyEvent *) re;
#undef xfer
#define xfer(x) ev->x = event->u.elementNotify.x
		xfer(flow);
		xfer(element_num);
		xfer(kind);
		xfer(prev_state);
		xfer(cur_state);
		xfer(reason);
		xfer(num_bytes);
	    }
	    break;
	case AuEventTypeMonitorNotify:
	    {
		register AuMonitorNotifyEvent *ev =
		(AuMonitorNotifyEvent *) re;
#undef xfer
#define xfer(x) ev->x = event->u.monitorNotify.x
		xfer(flow);
		xfer(element_num);
		xfer(format);
		xfer(num_tracks);
		xfer(count);
		xfer(num_fields);
		xfer(data);
		xfer(data1);
		xfer(data2);
	    }
	    break;
	default:
	    return (_AuUnknownWireEvent(aud, re, event));
    }
    return (AuTrue);
}


#ifndef USL_SHARELIB

static char *_SysErrorMsg (n)
    int n;
{
    extern char *sys_errlist[];
    extern int sys_nerr;
    char *s = ((n >= 0 && n < sys_nerr) ? sys_errlist[n] : "unknown error");

    return (s ? s : "no such error");
}

#endif 	/* USL sharedlibs in don't define for AUD3.2 */


/*
 * _AuDefaultIOError - Default fatal system error reporting routine.  Called 
 * when an Au internal system error is encountered.
 */
static void
_AuDefaultIOError (aud)
	AuServer *aud;
{
	if (errno == EPIPE) {
	    (void) fprintf (stderr,
	"NAS connection to %s broken (explicit kill or server shutdown).\r\n",
			    AuServerString (aud));
	} else {
	    (void) fprintf (stderr, 
			"AuIO:  fatal IO error %d (%s) on audio server \"%s\"\r\n",
			errno, _SysErrorMsg (errno), AuServerString (aud));
	    (void) fprintf (stderr, 
	 "      after %lu requests (%lu known processed) with %d events remaining.\r\n",
			AuServerNextRequest(aud) - 1, AuServerLastKnownRequestProcessed(aud),
			AuServerQLength(aud));

	}
	exit(1);
}


int _AuPrintDefaultError (aud, event, fp)
    AuServer *aud;
    AuErrorEvent *event;
    FILE *fp;
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "audiolib";
    register _AuExtension *ext = (_AuExtension *)NULL;
    _AuExtension *bext = (_AuExtension *)NULL;
    AuGetErrorText(aud, event->error_code, buffer, BUFSIZ);
    AuGetErrorDatabaseText(aud, mtype, "AuError", "Audio Error", mesg, BUFSIZ);
    (void) fprintf(fp, "%s:  %s\n  ", mesg, buffer);
    AuGetErrorDatabaseText(aud, mtype, "MajorCode", "Request Major code %d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->request_code);
    if (event->request_code < 128) {
	sprintf(number, "%d", event->request_code);
	AuGetErrorDatabaseText(aud, "AuRequest", number, "", buffer, BUFSIZ);
    } else {
	for (ext = aud->ext_procs;
	     ext && (ext->codes.major_opcode != event->request_code);
	     ext = ext->next)
	    /* SUPPRESS 530 */
	  ;
	if (ext)
	    strcpy(buffer, ext->name);
	else
	    buffer[0] = '\0';
    }
    (void) fprintf(fp, " (%s)\n", buffer);
    if (event->request_code >= 128) {
	AuGetErrorDatabaseText(aud, mtype, "MinorCode", "Request Minor code %d",
			      mesg, BUFSIZ);
	fputs("  ", fp);
	(void) fprintf(fp, mesg, event->minor_code);
	if (ext) {
	    sprintf(mesg, "%s.%d", ext->name, event->minor_code);
	    AuGetErrorDatabaseText(aud, "AuRequest", mesg, "", buffer, BUFSIZ);
	    (void) fprintf(fp, " (%s)", buffer);
	}
	fputs("\n", fp);
    }
    if (event->error_code >= 128) {
	/* kludge, try to find the extension that caused it */
	buffer[0] = '\0';
	for (ext = aud->ext_procs; ext; ext = ext->next) {
	    if (ext->error_string) 
		(*ext->error_string)(aud, event->error_code, &ext->codes,
				     buffer, BUFSIZ);
	    if (buffer[0]) {
		bext = ext;
		break;
	    }
	    if (ext->codes.first_error &&
		ext->codes.first_error < (int) event->error_code &&
		(!bext || ext->codes.first_error > bext->codes.first_error))
		bext = ext;
	}    
	if (bext)
	    sprintf(buffer, "%s.%d", bext->name,
		    event->error_code - bext->codes.first_error);
	else
	    strcpy(buffer, "Value");
	AuGetErrorDatabaseText(aud, mtype, buffer, "", mesg, BUFSIZ);
	if (mesg[0]) {
	    fputs("  ", fp);
	    (void) fprintf(fp, mesg, event->resourceid);
	    fputs("\n", fp);
	}
	/* let extensions try to print the values */
	for (ext = aud->ext_procs; ext; ext = ext->next) {
	    if (ext->error_values)
		(*ext->error_values)(aud, event, fp);
	}
    }
    AuGetErrorDatabaseText(aud, mtype, "ErrorSerial", "Error Serial #%d", 
			  mesg, BUFSIZ);
    fputs("  ", fp);
    (void) fprintf(fp, mesg, event->serial);
    AuGetErrorDatabaseText(aud, mtype, "CurrentSerial", "Current Serial #%d",
			  mesg, BUFSIZ);
    fputs("\n  ", fp);
    (void) fprintf(fp, mesg, aud->request);
    fputs("\n", fp);
    if (event->error_code == AuBadImplementation) return 0;
    return 1;
}

int _AuDefaultError(aud, event)
	AuServer *aud;
	AuErrorEvent *event;
{
    if (_AuPrintDefaultError (aud, event, stderr) == 0) return 0;
    exit(1);
    /*NOTREACHED*/
}

/*ARGSUSED*/
AuBool _AuDefaultWireError(server, he, we)
    AuServer     *server;
    AuErrorEvent *he;
    auError      *we;
{
    return AuTrue;
}

/*
 * _AuError - upcall internal or user protocol error handler
 */
int _AuError (aud, rep)
    AuServer *aud;
    register auError *rep;
{
    /* 
     * Au_Error packet encountered!  We need to unpack the error before
     * giving it to the user.
     */
    AuEvent event; /* make it a large event */
    register _AuAsyncHandler *async, *next;

    event.auerror.serial = _AuSetLastRequestRead(aud, (auGenericReply *)rep);

    for (async = aud->async_handlers; async; async = next) {
	next = async->next;
	if ((*async->handler)(aud, (auReply *)rep,
			      (char *)rep, SIZEOF(auError), async->data))
	    return 0;
    }

    event.auerror.server = aud;
    event.auerror.type = Au_Error;
    event.auerror.time = rep->time;
    event.auerror.resourceid = rep->resourceID;
    event.auerror.error_code = rep->errorCode;
    event.auerror.request_code = rep->majorCode;
    event.auerror.minor_code = rep->minorCode;
    switch (event.auerror.error_code) {
      default:
	event.auerror.data.l[0] = rep->data0;
	event.auerror.data.l[1] = rep->data1;
	event.auerror.data.l[2] = rep->data2;
	event.auerror.data.l[3] = rep->data3;
    }

    if (aud->error_vec &&
	!(*aud->error_vec[rep->errorCode])(aud, &event.auerror, rep))
	return 0;
    if (aud->funcs.error_handler != NULL) {
      	return ((*aud->funcs.error_handler)(aud, (AuErrorEvent *) &event));	/* upcall */
    } else {
	return _AuDefaultError(aud, (AuErrorEvent *) &event);
    }
}
    
/*
 * _AuIOError - call user connection error handler and exit
 */
int _AuIOError (aud)
    AuServer *aud;
{
    aud->flags |= AuServerFlagsIOError;
    if (aud->funcs.ioerror_handler != NULL)
	(*aud->funcs.ioerror_handler)(aud);
    else
	_AuDefaultIOError(aud);
    exit (1);
}


/*
 * This routine can be used to (cheaply) get some memory within a single
 * audiolib routine for scratch space.  It is reallocated from the same place
 * each time, unless the library needs a large scratch space.
 */
char *_AuAllocScratch (aud, nbytes)
	register AuServer *aud;
	AuUint32 nbytes;
{
	if (nbytes > aud->scratch_length) {
	    if (aud->scratch_buffer) Aufree (aud->scratch_buffer);
	    if ((aud->scratch_buffer = Aumalloc((unsigned) nbytes)) != NULL)
		aud->scratch_length = nbytes;
	    else aud->scratch_length = 0;
	}
	return (aud->scratch_buffer);
}


void 
AuFree (data)
    AuPointer data;
{
	Aufree (data);
}

#ifdef _AUNEEDBCOPYFUNC
void _Aubcopy(b1, b2, length)
    register char *b1, *b2;
    register length;
{
    if (b1 < b2) {
	b2 += length;
	b1 += length;
	while (length--)
	    *--b2 = *--b1;
    } else {
	while (length--)
	    *b2++ = *b1++;
    }
}
#endif

#ifdef _AuDataRoutineIsProcedure
void _AuData (aud, data, len)
	AuServer *aud;
	char *data;
	AuInt32 len;
{
	if (aud->bufptr + (len) <= aud->bufmax) {
		bcopy(data, aud->bufptr, (int)len);
		aud->bufptr += ((len) + 3) & ~3;
	} else {
		_AuSend(aud, data, len);
	}
}
#endif /* _AuDataRoutineIsProcedure */


#ifdef WORD64

/*
 * XXX This is a *really* stupid way of doing this.  It should just use 
 * aud->bufptr directly, taking into account where in the word it is.
 */

/*
 * _AuData16 - Place 16 bit data in the buffer.
 *
 * "aud" is a pointer to a AuServer.
 * "data" is a pointer to the data.
 * "len" is the length in bytes of the data.
 */

static do_AuData16(aud, data, len, packbuffer)
    register AuServer *aud;
    short *data;
    unsigned len;
    char *packbuffer;
{
    AuInt32 *lp,*lpack;
    AuInt32 i, nwords,bits;
    AuInt32 mask16 = 0x000000000000ffff;

        lp = (AuInt32 *)data;
        lpack = (AuInt32 *)packbuffer;

/*  nwords is the number of 16 bit values to be packed,
 *  the low order 16 bits of each word will be packed
 *  into 64 bit words
 */
        nwords = len >> 1;
        bits = 48;

        for(i=0;i<nwords;i++){
	   if (bits == 48) *lpack = 0;
           *lpack ^= (*lp & mask16) << bits;
           bits -= 16 ;
           lp++;
           if(bits < 0){
               lpack++;
               bits = 48;
           }
        }
        _AuData(aud, packbuffer, len);
}

_AuData16 (aud, data, len)
    AuServer *aud;
    short *data;
    unsigned len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 1;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	do_AuData16 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) do_AuData16 (aud, data, len, packbuffer);
}

/*
 * _AuData32 - Place 32 bit data in the buffer.
 *
 * "aud" is a pointer to a AuServer.
 * "data" is a pointer to the data.
 * "len" is the length in bytes of the data.
 */

static do_AuData32 (aud, data, len, packbuffer)
    register AuServer *aud;
    AuInt32 *data;
    unsigned len;
    char *packbuffer;
{
    AuInt32 *lp,*lpack;
    AuInt32 i,bits,nwords;
    AuInt32 mask32 = 0x00000000ffffffff;

        lpack = (AuInt32 *) packbuffer;
        lp = data;

/*  nwords is the number of 32 bit values to be packed
 *  the low order 32 bits of each word will be packed
 *  into 64 bit words
 */
        nwords = len >> 2;
        bits = 32;

        for(i=0;i<nwords;i++){
	   if (bits == 32) *lpack = 0;
           *lpack ^= (*lp & mask32) << bits;
           bits = bits ^32;
           lp++;
           if(bits)
              lpack++;
        }
        _AuData(aud, packbuffer, len);
}

_AuData32 (aud, data, len)
    AuServer *aud;
    AuInt32 *data;
    unsigned len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 2;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	do_AuData32 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) do_AuData32 (aud, data, len, packbuffer);
}

#endif /* WORD64 */



/*
 * aud->qfree - free the queue of events, called by AuCloseServer
 */

void _AuFreeQ (aud)
    AuServer *aud;
{
    register _AuQEvent *qelt = aud->qfree;
  
    while (qelt) {
	register _AuQEvent *qnxt = qelt->next;
	Aufree ((char *) qelt);
	qelt = qnxt;
    }
    aud->qfree = NULL;
    return;
}


/* Make sure this produces the same string as DefineLocal/DefineSelf in xdm.
 * Otherwise, Xau will not be able to find your cookies in the Xauthority file.
 *
 * Note: POSIX says that the ``nodename'' member of utsname does _not_ have
 *       to have sufficient information for interfacing to the network,
 *       and so, you may be better off using gethostname (if it exists).
 */

#if (defined(_POSIX_SOURCE) && !defined(AIXV3)) || defined(hpux) || defined(USG) || defined(SVR4)
#define NEED_UTSNAME
#include <sys/utsname.h>
#endif

/*
 * _AuGetHostname - similar to gethostname but allows special processing.
 */
int _AuGetHostname (buf, maxlen)
    char *buf;
    int maxlen;
{
    int len;

#ifdef NEED_UTSNAME
    struct utsname name;

    uname (&name);
    len = strlen (name.nodename);
    if (len >= maxlen) len = maxlen - 1;
    strncpy (buf, name.nodename, len);
    buf[len] = '\0';
#else
    buf[0] = '\0';
    (void) gethostname (buf, maxlen);
    buf [maxlen - 1] = '\0';
    len = strlen(buf);
#endif /* NEED_UTSNAME */
    return len;
}



#if (MSKCNT > 4)
/*
 * This is a macro if MSKCNT <= 4
 */
int
_AuANYSET(src)
    AuInt32	*src;
{
    int i;

    for (i=0; i<MSKCNT; i++)
	if (src[ i ])
	    return (1);
    return (0);
}
#endif


#ifdef CRAY
#define HAS_FAKE_IOV
/*
 * Cray UniCOS does not have readv and writev so we emulate
 */
#include <sys/socket.h>

int _AuReadV (fd, iov, iovcnt)
int fd;
struct iovec *iov;
int iovcnt;
{
	struct msghdr hdr;

	hdr.msg_iov = iov;
	hdr.msg_iovlen = iovcnt;
	hdr.msg_accrights = 0;
	hdr.msg_accrightslen = 0;
	hdr.msg_name = 0;
	hdr.msg_namelen = 0;

	return (recvmsg (fd, &hdr, 0));
}

int _AuWriteV (fd, iov, iovcnt)
int fd;
struct iovec *iov;
int iovcnt;
{
	struct msghdr hdr;

	hdr.msg_iov = iov;
	hdr.msg_iovlen = iovcnt;
	hdr.msg_accrights = 0;
	hdr.msg_accrightslen = 0;
	hdr.msg_name = 0;
	hdr.msg_namelen = 0;

	return (sendmsg (fd, &hdr, 0));
}

#endif /* CRAY */

#if defined(SYSV) && defined(SYSV386) && !defined(STREAMSCONN)
#define HAS_FAKE_IOV
/*
 * SYSV/386 does not have readv so we emulate
 */
#include <sys/uio.h>

int _AuReadV (fd, iov, iovcnt)
int fd;
struct iovec *iov;
int iovcnt;
{
    int i, len, total;
    char *base;

    errno = 0;
    for (i=0, total=0;  i<iovcnt;  i++, iov++) {
	len = iov->iov_len;
	base = iov->iov_base;
	while (len > 0) {
	    register int nbytes;
	    nbytes = read(fd, base, len);
	    if (nbytes < 0 && total == 0)  return -1;
	    if (nbytes <= 0)  return total;
	    errno = 0;
	    len   -= nbytes;
	    total += nbytes;
	    base  += nbytes;
	}
    }
    return total;
}

#endif /* SYSV && SYSV386 && !STREAMSCONN */

#ifdef STREAMSCONN
#define HAS_FAKE_IOV
/*
 * Copyright 1988, 1989 AT&T, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of AT&T not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  AT&T makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * AT&T DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL AT&T
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/*
 iovec.c (C source file)
	Acc: 575557389 Mon Mar 28 08:03:09 1988
	Mod: 575557397 Mon Mar 28 08:03:17 1988
	Sta: 575557397 Mon Mar 28 08:03:17 1988
	Owner: 2011
	Group: 1985
	Permissions: 664
*/
/*
	START USER STAMP AREA
*/
/*
	END USER STAMP AREA
*/


extern char _AusTypeofStream[];
extern Austream _AusStream[];

#define MAX_WORKAREA 4096
static char workarea[MAX_WORKAREA];



int
_AuReadV (fd, v, n)
    int		fd;
    struct iovec v[];
    int		n;
{
	int i, rc, len, size = 0;
	char * buf = workarea;
	char * p;

	if (n <= 0 || n > 16)
	{
		errno = EINVAL;
		return (-1);
	}
	for (i = 0; i < n; ++i)
	{
		if ((len = v[i].iov_len) < 0 || v[i].iov_base == NULL)
		{
			errno = EINVAL;
			return (-1);
		}
		size += len;
	}
	if ((size > MAX_WORKAREA) && ((buf = malloc (size)) == NULL))
	{
		errno = EINVAL;
		return (-1);
	}
	if((rc = (*_AusStream[_AusTypeOfStream[fd]].ReadFromStream)(fd, buf, size,
							     BUFFERING))> 0)
	{
		for (i = 0, p = buf; i < n; ++i)
		{
			memcpy (v[i].iov_base, p, len = v[i].iov_len);
			p += len;
		}
	}
	if (size > MAX_WORKAREA)
		free (buf);

	return (rc);
}

int
_AuWriteV (fd, v, n)
    int fd;
    struct iovec v[];
    int n;
{
	int i, rc, len, size = 0;
	char * buf = workarea;
	char * p;

	if (n <= 0 || n > 16)
	{
		errno = EINVAL;
		return (-1);
	}
	for (i = 0; i < n; ++i)
	{
		if ((len = v[i].iov_len) < 0 || v[i].iov_base == NULL)
		{
			errno = EINVAL;
			return (-1);
		}
		size += len;
	}

	if ((size > MAX_WORKAREA) && ((buf = malloc (size)) == NULL))
	{
		errno = EINVAL;
		return (-1);
	}
	for (i = 0, p = buf; i < n; ++i)
	{
		memcpy (p, v[i].iov_base, len = v[i].iov_len);
		p += len;
	}
	rc = (*_AusStream[_AusTypeOfStream[fd]].WriteToStream)(fd, buf, size);

	if (size > MAX_WORKAREA)
		free (buf);

	return (rc);
}



#endif /* STREAMSCONN */

#ifndef HAS_FAKE_IOV
int
_AuReadV (fd, v, n)
    int		fd;
    struct iovec v[];
    int		n;
{
	return(readv(fd, v, n));
}

int
_AuWriteV (fd, v, n)
    int fd;
    struct iovec v[];
    int n;
{
	return(writev(fd, v, n));
}
#endif /* HAS_FAKE_IOV */

AuSyncHandlerRec *
AuRegisterSyncHandler(aud, callback, data)
AuServer       *aud;
AuSyncHandlerCallback callback;
AuPointer       data;
{
    AuSyncHandlerRec *handler;

    if (!(handler = (AuSyncHandlerRec *) Aumalloc(sizeof(AuSyncHandlerRec))))
	return NULL;

    handler->callback = callback;
    handler->data = data;

    _AuAddToLinkedList(aud->synchandler, handler);

    return handler;
}

void
AuUnregisterSyncHandler(aud, handler)
AuServer       *aud;
AuSyncHandlerRec *handler;
{
    _AuRemoveFromLinkedList(aud->synchandler, handler);
    Aufree(handler);
}

void
_AuDoSyncHandle(aud)
AuServer       *aud;
{
    AuSyncHandlerRec *p = aud->synchandler,
                   *next;

    while (p)
    {
	next = p->next;
	(*p->callback) (aud, p, p->data);
	p = next;
    }
}
