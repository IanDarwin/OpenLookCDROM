#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "$Header: /crl/audio/AF/lib/AF/RCS/AlibInt.c,v 1.27 1994/03/25 20:03:40 tml Exp $";
#endif /* RCS_ID */
#endif /* LINT */
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
/*
 * $RCS: AlibInt.c,v 11.119 89/12/12 11:50:18 jim Exp $
 */

/* Copyright    Massachusetts Institute of Technology    1985, 1986, 1987
 * Copyright Digital Equipment Corporation, 1990 
 */

/*
 *	AlibInternal.c - Internal support routines for the C subroutine
 *	interface library (Alib)
 */

#include <stdio.h>
#include <unistd.h>			/* read and write. 		*/
#include "Alibint.h"
#include "Aos.h"

static void _EatData32(AFAudioConn *, unsigned long);

#if USG
int _AReadV(), _AWriteV();
#endif 

#if defined(STREAMSCONN) && (!defined(EWOULDBLOCK)) && defined(EAGAIN)
#define EWOULDBLOCK EAGAIN
#endif


/*
 * The following routines are internal routines used by Alib for protocol
 * packet transmission and reception.
 *
 * AIOError(AFAudioConn *) will be called if any sort of system call error occurs.
 * This is assumed to be a fatal condition, i.e., AIOError should not return.
 *
 * AError(AFAudioConn *, XErrorEvent *) will be called whenever an A_Error event is
 * received.  This is not assumed to be a fatal condition, i.e., it is
 * acceptable for this procedure to return.  However, AError should NOT
 * perform any operations (directly or indirectly) on the AFAudioConn.
 *
 * Routines declared with a return type of 'AStatus' return 0 on failure,
 * and non 0 on success.  Routines with no declared return type don't 
 * return anything.  Whenever possible routines that create objects return
 * the object they have created.
 */

extern _AFQEvent *_afqfree;

static int padlength[4] = {0, 3, 2, 1};
    /* lookup table for adding padding bytes to data that is read from
    	or written to the audio socket.  */

static aReq _dummy_request = {
	0, 0, 0
};
/*
 * _AFlushAsync - Flush the audio request buffer.  If the buffer is empty, no
 * action is taken.  This routine correctly handles incremental writes.
 * This routine may have to be reworked if int < long.
 */
_AFlushAsync (aud)
	register AFAudioConn *aud;
{
	register long size, todo;
	register int write_stat;
	register char *bufindex;

	aud = aud->connection;
	if (aud->flags & AlibAudioConnIOError) return(0);

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
	    } else if (errno == EWOULDBLOCK) {
		return(-1);
	    } else if (errno != EINTR) {
		/* Write failed! */
		/* errno set by write system call. */
		_AIOError(aud);
	    }
	}
	aud->last_req = (char *)&_dummy_request;
	return(0);
}
/*
 * _AFlush - Flush the audio request buffer.  If the buffer is empty, no
 * action is taken.  This routine correctly handles incremental writes.
 * This routine may have to be reworked if int < long.
 */
void
_AFlush (register AFAudioConn *aud)
{
	register long size, todo;
	register int write_stat;
	register char *bufindex;

	aud = aud->connection;
	if (aud->flags & AlibAudioConnIOError) return;

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
#ifdef EWOULDBLOCK
	    } else if (errno == EWOULDBLOCK) {
		_AWaitForWritable(aud);
#endif
#ifdef SUNSYSV
	    } else if (errno == 0) {
		_AWaitForWritable(aud);
#endif
#ifdef EMSGSIZE
	    } else if (errno == EMSGSIZE) {
		if (todo > 1) 
		  todo >>= 1;
		else
		  _AWaitForWritable(aud);
#endif
	    } else if (errno != EINTR) {
		/* Write failed! */
		/* errno set by write system call. */
		_AIOError(aud);
	    }
	}
	aud->last_req = (char *)&_dummy_request;
}

void AFFlush (register AFAudioConn *aud)
{
    LockConnection(aud);
    _AFlush (aud);
    UnlockConnection(aud);
}

int
_AEventsQueued (register AFAudioConn *aud, int mode)
{
	register int len;
	int pend;
	char buf[BUFSIZE];
	register aReply *rep;
	
	aud = aud->connection;
	if (mode == AQueuedAfterFlush)
	{
	    _AFlush(aud);
	    if (aud->qlen)
		return(aud->qlen);
	}
	if (aud->flags & AlibAudioConnIOError) return(aud->qlen);
	if (BytesReadable(aud->fd, (char *) &pend) < 0)
	    _AIOError(aud);
	if ((len = pend) < SIZEOF(aReply))
	    return(aud->qlen);	/* _AFlush can enqueue events */
	else if (len > BUFSIZE)
	    len = BUFSIZE;
	len /= SIZEOF(aReply);
	pend = len * SIZEOF(aReply);
	_ARead (aud, buf, (long) pend);

	/* no space between comma and type or else macro will die */
	STARTITERATE (rep,aReply, buf, (len > 0), len--) {
	    if (rep->generic.type == A_Error)
		_AError(aud, (aError *)rep);
	    else   /* must be an event packet */
		_AEnq(aud, (aEvent *) rep);
	}
	ENDITERATE
	return(aud->qlen);
}

/* _AReadEvents - Flush the output queue,
 * then read as many events as possible (but at least 1) and enqueue them
 */
void
_AReadEvents(register AFAudioConn *aud)
{
	char buf[BUFSIZE];
	int pend_not_register; /* because can't "&" a register variable */
	register int pend;
	register aEvent *ev;
	ABool not_yet_flushed = ATrue;

	aud = aud->connection;
	do {
	    /* find out how much data can be read */
	    if (BytesReadable(aud->fd, (char *) &pend_not_register) < 0)
	    	_AIOError(aud);
	    pend = pend_not_register;

	    /* must read at least one aEvent; if none is pending, then
	       we'll just flush and block waiting for it */
	    if (pend < SIZEOF(aEvent)) {
	    	pend = SIZEOF(aEvent);
		/* don't flush until we block the first time */
		if (not_yet_flushed) {
		    int qlen = aud->qlen;
		    _AFlush (aud);
		    if (qlen != aud->qlen) return;
		    not_yet_flushed = AFalse;
		}
	    }
		
	    /* but we won't read more than the max buffer size */
	    if (pend > BUFSIZE)
	    	pend = BUFSIZE;

	    /* round down to an integral number of AReps */
	    pend = (pend / SIZEOF(aEvent)) * SIZEOF(aEvent);

	    _ARead (aud, buf, pend);

	    /* no space between comma and type or else macro will die */
	    STARTITERATE (ev,aEvent, buf, (pend > 0),
			  pend -= SIZEOF(aEvent)) {
		if (ev->u.u.type == A_Error)
		    _AError (aud, (aError *) ev);
		else  /* it's an event packet; enqueue it */
		    _AEnq (aud, ev);
	    }
	    ENDITERATE
	} while (aud->head == NULL);
}

/* 
 * _ARead - Read bytes from the socket taking into account incomplete
 * reads.  This routine may have to be reworked if int < long.
 */
void
_ARead (register AFAudioConn *aud, register char *data,	register long size)
{
	register long bytes_read;

	aud = aud->connection;
	if ((aud->flags & AlibAudioConnIOError) || size == 0) return;
	errno = 0;
	while ((bytes_read = ReadFromServer(aud->fd, data, (int)size))
		!= size) {

	    	if (bytes_read > 0) {
		    size -= bytes_read;
		    data += bytes_read;
		    }
#ifdef EWOULDBLOCK
		else if (errno == EWOULDBLOCK) {
		    _AWaitForReadable(aud);
		    errno = 0;
		}
#endif		
#ifdef SUNSYSV
		else if (errno == 0) {
		    _AWaitForReadable(aud);
		}
#endif
		else if (bytes_read == 0) {
		    /* Read failed because of end of file! */
		    errno = EPIPE;
		    _AIOError(aud);
		    }

		else  /* bytes_read is less than 0; presumably -1 */ {
		    /* If it's a system call interrupt, it's not an error. */
		    if (errno != EINTR)
		    	_AIOError(aud);
		    }
	    	 }
}

/* 
 * _AReadAsync - Read bytes from the socket but returning if EWOULDBLOCK
 */
long
_AReadAsync (aud, data, size, block)
	register AFAudioConn *aud;
	register char *data;
	register long size;
	ABool block;
{
	register long bytes_read;
	register long total_bytes_read = 0;

	aud = aud->connection;
	if ((aud->flags & AlibAudioConnIOError) || size == 0) return(0);
	errno = 0;
	while ((bytes_read = ReadFromServer(aud->fd, data, (int)size))
		!= size) {

	    	if (bytes_read > 0) {
		    size -= bytes_read;
		    total_bytes_read += bytes_read;
		    data += bytes_read;
		} else if (errno == EWOULDBLOCK) {
		    if (total_bytes_read > 0) {
			/* We read a few before blocking */
			if (block) {
			    continue;
			} else {
			    return(total_bytes_read);
			}
		    } else {
			return(-1);
		    }
		} else if (bytes_read == 0) {
		    /* Read failed because of end of file! */
		    errno = EPIPE;
		    _AIOError(aud);
		} else  /* bytes_read is less than 0; presumably -1 */ {
		    /* If it's a system call interrupt, it's not an error. */
		    if (errno != EINTR)
		    	_AIOError(aud);
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
 * _ARead32 - Read bytes from the socket unpacking each 32 bits
 *            into a long (64 bits on a CRAY computer).
 * 
 */
static _doARead32 (
        register AFAudioConn *aud,
        register long *data,
        register long size,
	register char *packbuffer
	)
{
 long *lpack,*lp;
 long mask32 = 0x00000000ffffffff;
 long maskw, nwords, i, bits;

        _AReadPad (aud, packbuffer, size);

        lp = data;
        lpack = (long *) packbuffer;
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

_ARead32 (
    AFAudioConn *aud,
    long *data,
    long len
	)
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 2;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	_doARead32 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) _doARead32 (aud, data, len, packbuffer);
}



/*
 * _ARead16 - Read bytes from the socket unpacking each 16 bits
 *            into a long (64 bits on a CRAY computer).
 *
 */
static _doARead16 (
        register AFAudioConn *aud,
        register short *data,
        register long size,
	char *packbuffer
	)
{
	long *lpack,*lp;
	long mask16 = 0x000000000000ffff;
	long maskw, nwords, i, bits;

        _ARead(aud,packbuffer,size);	/* don't do a padded read... */

        lp = (long *) data;
        lpack = (long *) packbuffer;
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

_ARead16 (
    AFAudioConn *aud,
    short *data,
    long len
	)
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 1;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	_doARead16 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) _doARead16 (aud, data, len, packbuffer);
}

_ARead16Pad (
    AFAudioConn *aud,
    short *data,
    long size
	)
{
    int slop = (size & 3);
    short slopbuf[3];

    _ARead16 (aud, data, size);
    if (slop > 0) {
	_ARead16 (aud, slopbuf, 4 - slop);
    }
}
#endif


/*
 * _AReadPad - Read bytes from the socket taking into account incomplete
 * reads.  If the number of bytes is not 0 mod 32, read additional pad
 * bytes. This routine may have to be reworked if int < long.
 */
void 
_AReadPad (
    	register AFAudioConn *aud,
	register char *data,
	register long size
	)
{
    	register long bytes_read;
	struct iovec iov[2];
	char pad[3];

	if ((aud->flags & AlibAudioConnIOError) || size == 0) return;
	iov[0].iov_len = (int)size;
	iov[0].iov_base = data;
	/* 
	 * The following hack is used to provide 32 bit long-word
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
#ifdef EWOULDBLOCK
	    else if (errno == EWOULDBLOCK) {
		_AWaitForReadable(aud);
		errno = 0;
	    }
#endif
#ifdef SUNSYSV
	    else if (errno == 0) {
		_AWaitForReadable(aud);
	    }
#endif
	    else if (bytes_read == 0) {
		/* Read failed because of end of file! */
		errno = EPIPE;
		_AIOError(aud);
		}
	    
	    else  /* bytes_read is less than 0; presumably -1 */ {
		/* If it's a system call interrupt, it's not an error. */
		if (errno != EINTR)
		    _AIOError(aud);
		}
	    }

}


/*
 * _AReadPadAsync - Read bytes from the socket taking into account incomplete
 * reads and EWOULDBLOCK.  If the number of bytes is not 0 mod 32, read
 * additional pad bytes. This routine may have to be reworked if int < long.
 */
long
_AReadPadAsync (aud, data, size)
    	register AFAudioConn *aud;	
	register char *data;
	register long size;
{
    	register long bytes_read;
	struct iovec iov[2];
	char pad[3];
	long total_read = 0;

	if ((aud->flags & AlibAudioConnIOError) || size == 0) return(0);
	iov[0].iov_len = (int)size;
	iov[0].iov_base = data;
	/* 
	 * The following hack is used to provide 32 bit long-word
	 * aligned padding.  The [1] vector is of length 0, 1, 2, or 3,
	 * whatever is needed.
	 */

	iov[1].iov_len = padlength[size & 3];
	iov[1].iov_base = pad;
	size += iov[1].iov_len;
#ifdef apollo /* stupid sr10.1 bug */
	if (size >= 131072) {
	    bytes_read = _AReadAsync (aud, data, size - iov[1].iov_len, 0);
	    if (iov[1].iov_len) _ARead (aud, pad, iov[1].iov_len);
	    return(bytes_read);
	}
#endif
	errno = 0;
	while ((bytes_read = ReadvFromServer (aud->fd, iov, 2)) != size) {

	    if (bytes_read > 0) {
		total_read += bytes_read;
		size -= bytes_read;
	    	if ((iov[0].iov_len -= bytes_read) < 0) {
		    iov[1].iov_len += iov[0].iov_len;
		    iov[1].iov_base -= iov[0].iov_len;
		    iov[0].iov_len = 0;
		} else {
	    	    iov[0].iov_base += bytes_read;
	    	}
	    } else if (errno == EWOULDBLOCK) {
		if (total_read <= 0) {
		    return(-1);
		}
	    } else if (bytes_read == 0) {
		/* Read failed because of end of file! */
		errno = EPIPE;
		_AIOError(aud);
	    } else  /* bytes_read is less than 0; presumably -1 */ {
		/* If it's a system call interrupt, it's not an error. */
		if (errno != EINTR) {
		    _AIOError(aud);
		}
	    }
	}
	return(total_read + bytes_read);
}

/*
 * _ASend - Flush the buffer and send the client data. 32 bit word aligned
 * transmission is used, if size is not 0 mod 4, extra bytes are transmitted.
 * This routine may have to be reworked if int < long;
 */
void
_ASend (
	register AFAudioConn *aud,
	char *data,
	register long size
	)
{
	struct iovec iov[3];
	static char pad[3] = {0, 0, 0};

	long skip = 0;
	long audbufsize = (aud->bufptr - aud->buffer);
	long padsize = padlength[size & 3];
	long total = audbufsize + size + padsize;
	long todo = total;

	if (aud->flags & AlibAudioConnIOError) return;

	/*
	 * There are 3 pieces that may need to be written out:
	 *
	 *     o  whatever is in the AFAudioConn buffer
	 *     o  the data passed in by the user
	 *     o  any padding needed to 32bit align the whole mess
	 *
	 * This loop looks at all 3 pieces each time through.  It uses skip
	 * to figure out whether or not a given piece is needed.
	 */
	while (total) {
	    long before = skip;		/* amount of whole thing written */
	    long remain = todo;		/* amount to try this time, <= total */
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

	    InsertIOV (aud->buffer, audbufsize)
	    InsertIOV (data, size)
	    InsertIOV (pad, padsize)
    
	    errno = 0;
	    if ((len = WritevToServer(aud->fd, iov, i)) >= 0) {
		skip += len;
		total -= len;
		todo = total;
#ifdef EWOULDBLOCK
	    } else if (errno == EWOULDBLOCK) {
		_AWaitForWritable(aud);
#endif
#ifdef SUNSYSV
	    } else if (errno == 0) {
		_AWaitForWritable(aud);
#endif
#ifdef EMSGSIZE
	    } else if (errno == EMSGSIZE) {
		if (todo > 1) 
		  todo >>= 1;
		else 
		  _AWaitForWritable(aud);
#endif
	    } else if (errno != EINTR) {
		_AIOError(aud);
	    }
	}

	aud->bufptr = aud->buffer;
	aud->last_req = (char *) & _dummy_request;
}

/*
 * _ASendAsync - Flush the buffer and send the client data. 32 bit word aligned
 * transmission is used, if size is not 0 mod 4, extra bytes are transmitted.
 * This routine may have to be reworked if int < long;
 */
long
_ASendAsync (aud, data, size)
	register AFAudioConn *aud;
	char *data;
	register long size;
{
	struct iovec iov[3];
	static char pad[3] = {0, 0, 0};
	long skip = 0, nwritten = 0;
	long audbufsize = (aud->bufptr - aud->buffer);
	long padsize = padlength[size & 3];
	long total = audbufsize + size + padsize;
	long todo = total;
	long before;
	long remain;
	int i;
	long len;

	if (aud->flags & AlibAudioConnIOError) return(0);

	/*
	 * There are 3 pieces that may need to be written out:
	 *
	 *     o  whatever is in the AFAudioConn buffer
	 *     o  the data passed in by the user
	 *     o  any padding needed to 32bit align the whole mess
	 *
	 * This loop looks at all 3 pieces each time through.  It uses skip
	 * to figure out whether or not a given piece is needed.
	 */
	while (total) {
	    before = skip;		/* amount of whole thing written */
	    remain = todo;		/* amount to try this time, <= total */
	    i = 0;

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
		nwritten += len - (i == 2 ? audbufsize : 0);
	    } else if (errno == EWOULDBLOCK) {
		if (nwritten > 0) {
		    /* We wrote a few before blocking */
		    break;
		} else {
		    nwritten = -1;
		    break;
		}
	    } else if (errno != EINTR) {
		_AIOError(aud);
	    }
	}

	aud->bufptr = aud->buffer;
	aud->last_req = (char *) & _dummy_request;
	return(nwritten);
}

/*
 * _AAllocID - normal resource ID allocation routine.  A client
 * can roll his own and instatantiate it if he wants, but must
 * follow the rules.
 */
AID _AAllocID(register AFAudioConn *aud)
{
   return (aud->resource_base + (aud->resource_id++ << aud->resource_shift));
}

/*
 * The hard part about this is that we only get 16 bits from a reply.  Well,
 * then, we have three values that will march along, with the following
 * invariant:
 *	aud->last_request_read <= rep->sequenceNumber <= aud->request
 * The right choice for rep->sequenceNumber is the largest that
 * still meets these constraints.
 */

unsigned long
_ASetLastRequestRead(register AFAudioConn *aud, register aGenericReply *rep)
{
    register unsigned long	newseq, lastseq;

    newseq = (aud->last_request_read & ~((unsigned long)0xffff)) |
	     rep->sequenceNumber;
    lastseq = aud->last_request_read;
    while (newseq < lastseq) {
	newseq += 0x10000;
	if (newseq > aud->request) {
	    (void) fprintf (stderr, 
	    "Alib:  sequence lost (0x%lx > 0x%lx) in reply type 0x%x!\n",
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
 * _AReply - Wait for a reply packet and copy its contents into the
 * specified rep.  Mean while we must handle error and event packets that
 * we may encounter.
 */
AStatus _AReply (
    register AFAudioConn *aud,
    register aReply *rep,
    int extra,		/* number of 32-bit words expected after the reply */
    ABool discard	/* should I discard data following "extra" words? */
	)
{
    /* Pull out the serial number now, so that (currently illegal) requests
     * generated by an error handler don't confuse us.
     */
    unsigned long cur_request = aud->request;

    if (aud->flags & AlibAudioConnIOError) return (0);

    _AFlush(aud);
    while (1) {
	_ARead(aud, (char *)rep, (long)SIZEOF(aReply));
	switch ((int)rep->generic.type) {

	    case A_Reply:
	        /* Reply received.  Fast update for synchronous replies,
		 * but deal with multiple outstanding replies.
		 */
	        if (rep->generic.sequenceNumber == (cur_request & 0xffff))
		    aud->last_request_read = cur_request;
		else
		    (void) _ASetLastRequestRead(aud, &rep->generic);
		if (extra == 0) {
		    if (discard && (rep->generic.length != 0))
		       /* unexpectedly long reply! */
		       _EatData32 (aud, rep->generic.length);
		    return (1);
		    }
		if (extra == rep->generic.length) {
		    /* 
		     * Read the extra data into storage immediately following
		     * the GenericReply structure. 
		     */
		    _ARead (aud, (char *) (NEXTPTR(rep,aReply)),
			    ((long)extra) << 2);
		    return (1);
		    }
		if (extra < rep->generic.length) {
		    /* Actual reply is longer than "extra" */
		    _ARead (aud, (char *) (NEXTPTR(rep,aReply)),
			    ((long)extra) << 2);
		    if (discard)
		        _EatData32 (aud, rep->generic.length - extra);
		    return (1);
		    }
		/* 
		 *if we get here, then extra > rep->generic.length--meaning we
		 * read a reply that's shorter than we expected.  This is an 
		 * error,  but we still need to figure out how to handle it...
		 */
		_ARead (aud, (char *) (NEXTPTR(rep,aReply)),
			((long) rep->generic.length) << 2);
		_AIOError (aud);
		return (0);

    	    case A_Error:
	    	{
	        register _AFExtension *ext;
		register ABool ret = AFalse;
		int ret_code;
		aError *err = (aError *) rep;
		unsigned long serial;

		serial = _ASetLastRequestRead(aud, (aGenericReply *)rep);
		if (serial == cur_request)
			/* do not die on "no such font", "can't allocate",
			   "can't grab" failures */
			switch ((int)err->errorCode) {
			case ABadAlloc:
			case ABadAccess:
				return (0);
			/* 
			 * we better see if there is an extension who may
			 * want to suppress the error.
			 */
			default:
			    ext = aud->ext_procs;
			    while (ext) {
				if (ext->error != NULL) 
				   ret = (*ext->error)
					(aud, err, &ext->codes, &ret_code);
				ext = ext->next;
				}
			    if (ret) return (ret_code);
			    break;
			}
		_AError(aud, err);
		if (serial == cur_request)
		    return(0);
		}
		break;
	    default:
		_AEnq(aud, (aEvent *) rep);
		break;
	    }
	}
}   

/*
 * _AReplyAsync - Get a reply packet and copy its contents into the
 * specified rep.  Mean while we must handle error and event packets that
 * we may encounter.
 */
AStatus _AReplyAsync (aud, rep, extra, discard)
    register AFAudioConn *aud;
    register aReply *rep;
    int extra;		/* number of 32-bit words expected after the reply */
    ABool discard;	/* should I discard data following "extra" words? */
{
    /* Pull out the serial number now, so that (currently illegal) requests
     * generated by an error handler don't confuse us.
     */
    unsigned long cur_request = aud->request;

    if (aud->flags & AlibAudioConnIOError) return (0);

    /* _AFlush(aud); */
    while (1) {
	if (_AReadAsync(aud, (char *)rep, (long)SIZEOF(aReply), 1) == -1) {
	    return(-1);
	}
	switch ((int)rep->generic.type) {

	    case A_Reply:
	        /* Reply received.  Fast update for synchronous replies,
		 * but deal with multiple outstanding replies.
		 */
	        if (rep->generic.sequenceNumber == (cur_request & 0xffff))
		    aud->last_request_read = cur_request;
		else
		    (void) _ASetLastRequestRead(aud, &rep->generic);
		if (extra == 0) {
		    if (discard && (rep->generic.length > 0))
		       /* unexpectedly long reply! */
		       _EatData32 (aud, rep->generic.length);
		    return (1);
		    }
		if (extra == rep->generic.length) {
		    /* 
		     * Read the extra data into storage immediately following
		     * the GenericReply structure. 
		     */
		    _ARead (aud, (char *) (NEXTPTR(rep,aReply)),
			    ((long)extra) << 2);
		    return (1);
		    }
		if (extra < rep->generic.length) {
		    /* Actual reply is longer than "extra" */
		    _ARead (aud, (char *) (NEXTPTR(rep,aReply)),
			    ((long)extra) << 2);
		    if (discard)
		        _EatData32 (aud, rep->generic.length - extra);
		    return (1);
		    }
		/* 
		 *if we get here, then extra > rep->generic.length--meaning we
		 * read a reply that's shorter than we expected.  This is an 
		 * error,  but we still need to figure out how to handle it...
		 */
		_ARead (aud, (char *) (NEXTPTR(rep,aReply)),
			((long) rep->generic.length) << 2);
		_AIOError (aud);
		return (0);

    	    case A_Error:
	    	{
	        register _AFExtension *ext;
		register ABool ret = AFalse;
		int ret_code;
		aError *err = (aError *) rep;
		unsigned long serial;

		serial = _ASetLastRequestRead(aud, (aGenericReply *)rep);
		if (serial == cur_request)
			/* do not die on "no such font", "can't allocate",
			   "can't grab" failures */
			switch ((int)err->errorCode) {
			case ABadAlloc:
			case ABadAccess:
				return (0);
			/* 
			 * we better see if there is an extension who may
			 * want to suppress the error.
			 */
			default:
			    ext = aud->ext_procs;
			    while (ext) {
				if (ext->error != NULL) 
				   ret = (*ext->error)
					(aud, err, &ext->codes, &ret_code);
				ext = ext->next;
				}
			    if (ret) return (ret_code);
			    break;
			}
		_AError(aud, err);
		if (serial == cur_request)
		    return(0);
		}
		break;
	    default:
		_AEnq(aud, (aEvent *) rep);
		break;
	    }
	}
}   


/* Read and discard "n" 8-bit bytes of data */

void _AEatData (AFAudioConn *aud, register unsigned long n)
{
#define SCRATCHSIZE 2048
    char buf[SCRATCHSIZE];

    while (n > 0) {
	register long bytes_read = (n > SCRATCHSIZE) ? SCRATCHSIZE : n;
	_ARead (aud, buf, bytes_read);
	n -= bytes_read;
    }
#undef SCRATCHSIZE
}


/* Read and discard "n" 32-bit words. */

static void _EatData32 (AFAudioConn *aud, unsigned long n)
{
    _AEatData (aud, n << 2);
}


/*
 * _AEnq - Place event packets on the display's queue.
 * note that no squishing of move events in V11, since there
 * is pointer motion hints....
 */
_AEnq (register AFAudioConn *aud, register aEvent *event)
{
	register _AFQEvent *qelt;

/*NOSTRICT*/
	if ( (qelt = _afqfree) != NULL) {
		/* If _afqfree is non-NULL do this, else malloc a new one. */
		_afqfree = qelt->next;
	}
	else if ((qelt = 
	    (_AFQEvent *) Xmalloc((unsigned)sizeof(_AFQEvent))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		_AIOError(aud);
	}
	qelt->next = NULL;
	/* go call through AFAudioConn to find proper event reformatter */
	if ((*aud->event_vec[event->u.u.type & 0177])(aud, &qelt->event, event)) {
	    if (aud->tail)	aud->tail->next = qelt;
	    else 		aud->head = qelt;
    
	    aud->tail = qelt;
	    aud->qlen++;
	} else {
	    /* ignored, or stashed away for many-to-one compression */
	    qelt->next = _afqfree;
	    _afqfree = qelt;
	}
}
/*
 * EventToWire in separate file in that often not needed.
 */

/*ARGSUSED*/
ABool
_AUnknownWireEvent(
	register AFAudioConn *aud,	/* pointer to AFAudioConn structure */
	register AFEvent *re,	/* pointer to where event should be reformatted */
	register aEvent *event	/* wire protocol event */
	)
{
#ifdef notdef
	(void) fprintf(stderr, 
	    "Alib: unhandled wire event! event number = %d, AFAudioConn = %x\n.",
			event->u.u.type, aud);
#endif
	return(AFalse);
}

/*ARGSUSED*/
AStatus
_AUnknownNativeEvent(
	register AFAudioConn *aud,	/* pointer to AFAudioConn structure */
	register AFEvent *re,	/* pointer to where event should be reformatted */
	register aEvent *event	/* wire protocol event */
	)
{
#ifdef notdef
	(void) fprintf(stderr, 
 	   "Alib: unhandled native event! event number = %d, AFAudioConn = %x\n.",
			re->type, aud);
#endif
	return(0);
}
/*
 * reformat a wire event into an AFEvent structure of the right type.
 */
ABool
_AWireToEvent(
	register AFAudioConn *aud,	/* pointer to AFAudioConn structure */
	register AFEvent *re,	/* pointer to where event should be reformatted */
	register aEvent *event	/* wire protocol event */
	)
{

	re->type = event->u.u.type & 0x7f;
	((AFAnyEvent *)re)->serial = _ASetLastRequestRead(aud,
					(aGenericReply *)event);
	((AFAnyEvent *)re)->send_event = ((event->u.u.type & 0x80) != 0);
	((AFAnyEvent *)re)->audioconn = aud;

	
	/* Ignore the leading bit of the event type since it is set when a
		client sends an event rather than the server. */

	switch (event-> u.u.type & 0177) {
	      case APhoneRingEvent:
		{
		register AFPhoneRingEvent *ev = (AFPhoneRingEvent *) re;
		ev->stime.tv_sec = event->u.PhoneRing.sec;
		ev->stime.tv_usec = event->u.PhoneRing.usec;
		ev->time = event->u.PhoneRing.time;
		ev->device = event->u.PhoneRing.device;
		ev->state = event->u.PhoneRing.state;
		}
		break;
	      case APhoneDTMFEvent:
		{
		register AFPhoneDTMFEvent *ev = (AFPhoneDTMFEvent *) re;
		ev->stime.tv_sec = event->u.PhoneDTMF.sec;
		ev->stime.tv_usec = event->u.PhoneDTMF.usec;
		ev->time = event->u.PhoneDTMF.time;
		ev->device = event->u.PhoneDTMF.device;
		ev->state = event->u.PhoneDTMF.state;
		ev->digit = event->u.PhoneDTMF.digit;
		}
		break;
	      case APhoneLoopEvent:
		{
		register AFPhoneLoopEvent *ev = (AFPhoneLoopEvent *) re;
		ev->stime.tv_sec = event->u.PhoneLoop.sec;
		ev->stime.tv_usec = event->u.PhoneLoop.usec;
		ev->time = event->u.PhoneLoop.time;
		ev->device = event->u.PhoneLoop.device;
		ev->state = event->u.PhoneLoop.state;
		}
		break;
	      case APhoneHookSwitchEvent:
		{
		register AFPhoneHookSwitchEvent *ev = (AFPhoneHookSwitchEvent *) re;
		ev->stime.tv_sec = event->u.PhoneHookSwitch.sec;
		ev->stime.tv_usec = event->u.PhoneHookSwitch.usec;
		ev->time = event->u.PhoneHookSwitch.time;
		ev->device = event->u.PhoneHookSwitch.device;
		ev->state = event->u.PhoneHookSwitch.state;
		}
		break;
	      case ADSPEvent:
		{
		register AFDSPEvent *ev = (AFDSPEvent *) re;
		ev->stime.tv_sec = event->u.PhoneRing.sec;
		ev->stime.tv_usec = event->u.PhoneRing.usec;
		ev->time = event->u.DSP.time;
		ev->device = event->u.DSP.device;
		ev->hd[0] = event->u.DSP.hd0;
		ev->hd[1] = event->u.DSP.hd1;
		ev->hd[2] = event->u.DSP.hd2;
		ev->hd[3] = event->u.DSP.hd3;
		ev->hd[4] = event->u.DSP.hd4;
		ev->hd[5] = event->u.DSP.hd5;
		ev->hd[6] = event->u.DSP.hd6;
		ev->hd[7] = event->u.DSP.hd6;
		}
		break;
	      case APropertyEvent:
		{
		register AFPropertyEvent *ev = (AFPropertyEvent *) re;
		ev->device          = event->u.property.device;
		ev->atom            = event->u.property.atom;
		ev->time            = event->u.property.time;
		ev->state           = event->u.property.state;
		}
		break;
	      default:
		return(_AUnknownWireEvent(aud, re, event));
	}
	return(ATrue);
}


static char *_SysErrorMsg (int n)
{
    extern char *sys_errlist[];
    extern int sys_nerr;
    char *s = ((n >= 0 && n < sys_nerr) ? sys_errlist[n] : "unknown error");

    return (s ? s : "no such error");
}


/*
 * _ADefaultIOError - Default fatal system error reporting routine.  Called 
 * when an audio library internal system error is encountered.  
 * Note that the manual
 * specifies that this routine will print an error message and then exit.
 * People who don't like it should install their own error handler.
 */
_ADefaultIOError (AFAudioConn *aud)
{
	(void) fprintf (stderr, 
	 "AIO:  fatal IO error %d (%s) on Audio server \"%s\"\r\n",
			errno, _SysErrorMsg (errno), AudioConnString (aud));
	(void) fprintf (stderr, 
 "      after %lu requests (%lu known processed) with %d events remaining.\r\n",
			ANextRequest(aud) - 1, ALastKnownRequestProcessed(aud),
			AQLength(aud));

	if (errno == EPIPE) {
	    (void) fprintf (stderr,
	 "      The connection was probably broken by a server shutdown or KillClient.\r\n");
	}

	exit (1);
}


int _APrintDefaultError (AFAudioConn *aud, AFErrorEvent *event, FILE *fp)
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "AlibMessage";
    register _AFExtension *ext = (_AFExtension *)NULL;
    AFGetErrorText(aud, event->error_code, buffer, BUFSIZ);
    AFGetErrorDatabaseText(aud, mtype, "AError", "A Error", mesg, BUFSIZ);
    (void) fprintf(fp, "%s:  %s\n  ", mesg, buffer);
    AFGetErrorDatabaseText(aud, mtype, "MajorCode", "Request Major code %d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->request_code);
    if (event->request_code < 128) {
	sprintf(number, "%d", event->request_code);
	AFGetErrorDatabaseText(aud, "ARequest", number, "", buffer, BUFSIZ);
    } else {
	for (ext = aud->ext_procs;
	     ext && (ext->codes.major_opcode != event->request_code);
	     ext = ext->next)
	  ;
	if (ext)
	    strcpy(buffer, ext->name);
	else
	    buffer[0] = '\0';
    }
    (void) fprintf(fp, " (%s)\n  ", buffer);
    AFGetErrorDatabaseText(aud, mtype, "MinorCode", "Request Minor code %d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->minor_code);
    if (ext) {
	sprintf(mesg, "%s.%d", ext->name, event->minor_code);
	AFGetErrorDatabaseText(aud, "ARequest", mesg, "", buffer, BUFSIZ);
	(void) fprintf(fp, " (%s)", buffer);
    }
    fputs("\n  ", fp);
    AFGetErrorDatabaseText(aud, mtype, "ResourceID", "ResourceID 0x%x",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->resourceid);
    fputs("\n  ", fp);
    AFGetErrorDatabaseText(aud, mtype, "ErrorSerial", "Error Serial #%d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->serial);
    fputs("\n  ", fp);
    AFGetErrorDatabaseText(aud, mtype, "CurrentSerial", "Current Serial #%d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, aud->request);
    fputs("\n", fp);
    if (event->error_code == ABadImplementation) return 0;
    return 1;
}

int _ADefaultError(AFAudioConn *aud, AFErrorEvent *event)
{
    if (_APrintDefaultError (aud, event, stderr) == 0) return 0;
    exit(1);
    /*NOTREACHED*/
}


/*
 * _AError - prepare to upcall user protocol error handler
 */
int _AError (AFAudioConn *aud, aError *rep)
{
    AFErrorEvent event;
    /* 
     * A_Error packet encountered!  We need to unpack the error before
     * giving it to the user.
     */

    event.audioconn = aud;
    event.type = A_Error;
    event.serial = _ASetLastRequestRead(aud, (aGenericReply *)rep);
    event.resourceid = rep->resourceID;
    event.error_code = rep->errorCode;
    event.request_code = rep->majorCode;
    event.minor_code = rep->minorCode;
    if (_AErrorFunction != NULL) {
      	return ((*_AErrorFunction)(aud, &event));	/* upcall */
    } else {
	return _ADefaultError(aud, &event);
    }
}
    
/*
 * _AIOError - prepare to upcall user connection error handler
 */
int _AIOError (AFAudioConn *aud)
{
    aud->flags |= AlibAudioConnIOError;
    if (_AIOErrorFunction != NULL) {
	return ((*_AIOErrorFunction)(aud));		/* upcall */
    } else {
	return _ADefaultIOError(aud);			/* exits */
    }
}


/*
 * This routine can be used to (cheaply) get some memory within a single
 * Alib routine for scratch space.  It is reallocated from the same place
 * each time, unless the library needs a large scratch space.
 */
char *_AAllocScratch (register AFAudioConn *aud, unsigned long nbytes)
{
	if (nbytes > aud->scratch_length) {
	    if (aud->scratch_buffer) Xfree (aud->scratch_buffer);
	    if (aud->scratch_buffer = (char*)Xmalloc((unsigned) nbytes))
		aud->scratch_length = nbytes;
	    else aud->scratch_length = 0;
	}
	return (aud->scratch_buffer);
}

#ifdef DataRoutineIsProcedure
void Data (AFAudioConn *aud, char *data, long len)
{
	if (aud->bufptr + (len) <= aud->bufmax) {
		bcopy(data, aud->bufptr, (int)len);
		aud->bufptr += ((len) + 3) & ~3;
	} else {
		_ASend(aud, data, len);
	}
}
#endif


#ifdef WORD64

/*
 * XXX This is a *really* stupid way of doing this.  It should just use 
 * aud->bufptr directly, taking into account where in the word it is.
 */

/*
 * Data16 - Place 16 bit data in the buffer.
 *
 * "aud" is a pointer to a Display.
 * "data" is a pointer to the data.
 * "len" is the length in bytes of the data.
 */

static doData16(aud, data, len, packbuffer)
    register AFAudioConn *aud;
    short *data;
    unsigned len;
    char *packbuffer;
{
    long *lp,*lpack;
    long i, nwords,bits;
    long mask16 = 0x000000000000ffff;

        lp = (long *)data;
        lpack = (long *)packbuffer;
        *lpack = 0;

/*  nwords is the number of 16 bit values to be packed,
 *  the low order 16 bits of each word will be packed
 *  into 64 bit words
 */
        nwords = len >> 1;
        bits = 48;

        for(i=0;i<nwords;i++){
           *lpack ^= (*lp & mask16) << bits;
           bits -= 16 ;
           lp++;
           if(bits < 0){
               lpack++;
               *lpack = 0;
               bits = 48;
           }
        }
        Data(aud, packbuffer, len);
}

Data16 (aud, data, len)
    AFAudioConn *aud;
    short *data;
    unsigned len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 1;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	doData16 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) doData16 (aud, data, len, packbuffer);
}

/*
 * Data32 - Place 32 bit data in the buffer.
 *
 * "aud" is a pointer to a Display.
 * "data" is a pointer to the data.
 * "len" is the length in bytes of the data.
 */

static doData32 (aud, data, len, packbuffer)
    register AFAudioConn *aud;
    long *data;
    unsigned len;
    char *packbuffer;
{
    long *lp,*lpack;
    long i,bits,nwords;
    long mask32 = 0x00000000ffffffff;

        lpack = (long *) packbuffer;
        lp = data;

        *lpack = 0;

/*  nwords is the number of 32 bit values to be packed
 *  the low order 32 bits of each word will be packed
 *  into 64 bit words
 */
        nwords = len >> 2;
        bits = 32;

        for(i=0;i<nwords;i++){
           *lpack ^= (*lp & mask32) << bits;
           bits = bits ^32;
           lp++;
           if(bits){
              lpack++;
              *lpack = 0;
           }
        }
        Data(aud, packbuffer, len);
}

Data32 (aud, data, len)
    AFAudioConn *aud;
    long *data;
    unsigned len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nunits = PACKBUFFERSIZE >> 2;

    for (; len > PACKBUFFERSIZE; len -= PACKBUFFERSIZE, data += nunits) {
	doData32 (aud, data, PACKBUFFERSIZE, packbuffer);
    }
    if (len) doData32 (aud, data, len, packbuffer);
}

#endif



/*
 * _AFreeQ - free the queue of events, called by AFCloseAudioConn when there are
 * no more displays left on the AFAudioConn list
 */

void _AFreeQ ()
{
    register _AFQEvent *qelt = _afqfree;
  
    while (qelt) {
	register _AFQEvent *qnext = qelt->next;
	Xfree ((char *) qelt);
	qelt = qnext;
    }
    _afqfree = NULL;
    return;
}

#ifndef NEED_UTSNAME
extern int gethostname(char *, int);
#endif
/*
 * _AGetHostname - similar to gethostname but allows special processing.
 */
int _AGetHostname (char *buf, int maxlen)
{
    int len;

#ifdef USG
#define NEED_UTSNAME
#endif

#ifdef NEED_UTSNAME
#include <sys/utsname.h>
    /*
     * same host name crock as in server and xinit.
     */
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
#endif
    return len;
}


#if (MSKCNT > 4)
/*
 * This is a macro if MSKCNT <= 4
 */
_AFANYSET(src)
    long	*src;
{
    int i;

    for (i=0; i<MSKCNT; i++)
	if (src[ i ])
	    return (1);
    return (0);
}
#endif


#ifdef CRAY
/*
 * Cray UniCOS does not have readv and writev so we emulate
 */
#include <sys/socket.h>

int _AReadV (fd, iov, iovcnt)
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

int _AWriteV (fd, iov, iovcnt)
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

#endif


#ifdef STREAMSCONN
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
extern char * malloc();
extern char _AsTypeOfStream[];
extern Astream _AsStream[];

int _AReadV (fd, v, n)
    int fd;
    struct iovec *v;
    int n;
{
    int i;				/* iterator */
    int size = 0;			/* return value */

    /* check inputs */
    for (i = 0; i < n; i++) {
	if (v[i].iov_len < 0 || v[i].iov_base == NULL) {
	    errno = EINVAL;
	    return -1;
	}
    }

    for (i = 0; i < n; i++) {
	int len;
	char *p;
	
	for (p = v[i].iov_base, len = v[i].iov_len; len > 0; ) {
	    int rc = ((*_AsStream[_AsTypeOfStream[fd]].ReadFromStream)
		      (fd, p, len, BUFFERING));
	    if (rc > 0) {
		p += rc;
		len -= rc;
		size += rc;
	    } else if (rc < 0) {
		break;
	    }
	}
    }

    return size;
}


int _AWriteV (fd, v, n)
    int fd;
    struct iovec *v;
    int n;
{
    int i;				/* iterator */
    int size = 0;			/* return value */

    /* check inputs */
    for (i = 0; i < n; i++) {
	if (v[i].iov_len < 0 || v[i].iov_base == NULL) {
	    errno = EINVAL;
	    return -1;
	}
    }

    for (i = 0; i < n; i++) {
	int len;
	char *p;
	
	for (p = v[i].iov_base, len = v[i].iov_len; len > 0; ) {
	    int rc = ((*_AsStream[_AsTypeOfStream[fd]].WriteToStream)
		      (fd, p, len));
	    if (rc > 0) {
		p += rc;
		len -= rc;
		size += rc;
	    } else if (rc < 0) {
		break;
	    }
	}
    }

    return size;
}

#include <sys/poll.h>

#define POLLERROR		(POLLHUP | POLLNVAL | POLLERR)
#define PFD(fds, i, x) \
{ \
	if (fds) \
		if (ev & (x)) \
			BITSET (fds, i); \
		else \
			BITCLEAR (fds, i); \
}
#define ERROR(x) \
{ \
	errno = x; \
	return -1; \
}
/*
	simulate BSD select system call with SYSV poll system call
	note that efds parameter is not fully supported (or understood)
*/

extern long ulimit();

int
select (nfds, rfds, wfds, efds, timeout)
int nfds;
unsigned long *rfds;
unsigned long *wfds;
unsigned long *efds;
struct timeval *timeout;
{
	int i, rc, ev, timevalue;
	struct pollfd pfds[NOFILES_MAX];
	static long _NOFILE = 0;

	if (_NOFILE == 0)
		_NOFILE = ulimit(4, (long)0);

 	if (nfds > _NOFILE)
		nfds = _NOFILE;   /* make poll happy */

	for (i = 0; i < nfds; i++)
	{
		ev = 0;

		if (rfds && GETBIT (rfds, i)) ev |= POLLIN;
		if (wfds && GETBIT (wfds, i)) ev |= POLLOUT;
		if (ev || (efds && GETBIT (efds, i)))
			pfds[i].fd = i;
		else
			pfds[i].fd = -1;
		pfds[i].events = ev;
	}
	if (timeout)
		timevalue = timeout->tv_sec * 1000 + timeout->tv_usec / 1000;
	else
		timevalue = -1;

	if ((rc = poll (pfds, (unsigned long)nfds, timevalue)) > 0)
	{
		if (!efds)
			for (i = 0; i < nfds; ++i)
			{
				ev = pfds[i].revents;
				if (ev & POLLERROR)
					ERROR (EBADF);
			}

		for (i = 0; i < nfds; ++i)
		{
			ev = pfds[i].revents;
			PFD (rfds, i, POLLIN);
			PFD (wfds, i, POLLOUT);
			PFD (efds, i, POLLERROR);
		}
	}
	return rc;
}

#endif
