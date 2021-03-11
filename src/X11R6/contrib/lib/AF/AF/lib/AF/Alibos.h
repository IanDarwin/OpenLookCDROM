/************************************************************
Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, provided 
that the above copyright notice appear in all copies and that both that 
copyright notice and this permission notice appear in supporting 
documentation, and that the names of Digital or MIT not be used in 
advertising or publicity pertaining to distribution of the software without 
specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/
/*
 * $RCS: Alibos.h,v 1.23 89/12/18 13:44:00 rws Exp $
 *
 * Alib networking include files for UNIX Systems.
 */

#ifdef att
/*
 * UNIX System V Release 3.2
 */
#include <sys/stropts.h>
#define BytesReadable(fd,ptr) (_ABytesReadable ((fd), (ptr)))
#define MALLOC_0_RETURNS_NULL
#include <sys/ioctl.h>
#include <sys/param.h>
#define MAXSOCKS (NOFILES_MAX)
#define MSKCNT ((MAXSOCKS + 31) / 32)

#else
/*
 * 4.2BSD-based systems
 */
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h>
#include <sys/uio.h>	/* needed for AlibInt.c */
#include <sys/param.h> /* needed for AConn.c */

#ifdef	__alpha
extern int ioctl(int, unsigned long, char *);	/* No proto provided. */
#else
extern int ioctl(int, int, char *);	/* No proto provided. */
#endif

#define BytesReadable(fd, ptr) ioctl ((fd), FIONREAD, (ptr))
#define MSKCNT ((NOFILE + 31) / 32)	/* size of bit array */
#endif


/* Utek leaves kernel macros around in include files (bleah) */
#ifdef dirty
#undef dirty
#endif

#ifdef CRAY
#define WORD64
#define MALLOC_0_RETURNS_NULL
#endif

#if (MSKCNT==1)
#define BITMASK(i) (1 << (i))
#define MASKIDX(i) 0
#endif
#if (MSKCNT>1)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#endif

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

#if (MSKCNT==1)
#define COPYBITS(src, dst) dst[0] = src[0]
#define CLEARBITS(buf) buf[0] = 0
#define MASKANDSETBITS(dst, b1, b2) dst[0] = (b1[0] & b2[0])
#define ORBITS(dst, b1, b2) dst[0] = (b1[0] | b2[0])
#define UNSETBITS(dst, b1) (dst[0] &= ~b1[0])
#define _AFANYSET(src) (src[0])
#endif
#if (MSKCNT==2)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; }
#define MASKANDSETBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]); }
#define ORBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]); }
#define UNSETBITS(dst, b1) {\
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; }
#define _AFANYSET(src) (src[0] || src[1])
#endif
#if (MSKCNT==3)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; \
			     dst[2] = src[2]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; buf[2] = 0; }
#define MASKANDSETBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]);\
		      dst[2] = (b1[2] & b2[2]); }
#define ORBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]);\
		      dst[2] = (b1[2] | b2[2]); }
#define UNSETBITS(dst, b1) {\
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; \
                      dst[2] &= ~b1[2]; }
#define _AFANYSET(src) (src[0] || src[1] || src[2])
#endif
#if (MSKCNT==4)
#define COPYBITS(src, dst) dst[0] = src[0]; dst[1] = src[1]; \
			   dst[2] = src[2]; dst[3] = src[3]
#define CLEARBITS(buf) buf[0] = 0; buf[1] = 0; buf[2] = 0; buf[3] = 0
#define MASKANDSETBITS(dst, b1, b2)  \
                      dst[0] = (b1[0] & b2[0]);\
                      dst[1] = (b1[1] & b2[1]);\
                      dst[2] = (b1[2] & b2[2]);\
                      dst[3] = (b1[3] & b2[3])
#define ORBITS(dst, b1, b2)  \
                      dst[0] = (b1[0] | b2[0]);\
                      dst[1] = (b1[1] | b2[1]);\
                      dst[2] = (b1[2] | b2[2]);\
                      dst[3] = (b1[3] | b2[3])
#define UNSETBITS(dst, b1) \
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; \
                      dst[2] &= ~b1[2]; \
                      dst[3] &= ~b1[3]
#define _AFANYSET(src) (src[0] || src[1] || src[2] || src[3])
#endif

#if (MSKCNT>4)
#define COPYBITS(src, dst) bcopy((caddr_t) src, (caddr_t) dst,\
				 MSKCNT*sizeof(long))
#define CLEARBITS(buf) bzero((caddr_t) buf, MSKCNT*sizeof(long))
#define MASKANDSETBITS(dst, b1, b2)  \
		      { int cri;			\
			for (cri=0; cri<MSKCNT; cri++)	\
		          dst[cri] = (b1[cri] & b2[cri]) }
#define ORBITS(dst, b1, b2)  \
		      { int cri;			\
		      for (cri=0; cri<MSKCNT; cri++)	\
		          dst[cri] = (b1[cri] | b2[cri]) }
#define UNSETBITS(dst, b1) \
		      { int cri;			\
		      for (cri=0; cri<MSKCNT; cri++)	\
		          dst[cri] &= ~b1[cri];  }
/*
 * If MSKCNT>4, then _AFANYSET is a routine defined in AlibInt.c.
 *
 * #define _AFANYSET(src) (src[0] || src[1] || src[2] || src[3] || src[4] ...)
 */
#endif

#ifdef	sgi
#include <malloc.h>
#include <string.h>
#else
#if	defined(__osf__) || defined (mips) || defined (sun)
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#else
char *malloc(), *realloc(), *calloc();
void free();
char *strncpy(), *strncat();
void perror();
void exit();
#endif
#endif

#ifndef	__osf__
int  ffs(int);
void bcopy(char *, char *, int);
void bzero(char *, int);
#endif
int  select();

/*
 * The following definitions can be used for locking requests in multi-threaded
 * address spaces.
 */
#define LockConnection(dis)
#define LockMutex(mutex)
#define UnlockMutex(mutex)
#define UnlockConnection(dis)
#define Xfree(ptr) free((ptr))


/*
 * Note that some machines do not return a valid pointer for malloc(0), in
 * which case we provide an alternate under the control of the
 * define MALLOC_0_RETURNS_NULL.  This is necessary because some
 * Alib code expects malloc(0) to return a valid pointer to storage.
 */
#ifdef MALLOC_0_RETURNS_NULL

# define Xmalloc(size) malloc(((size) > 0 ? (size) : 1))
# define Xrealloc(ptr, size) realloc((ptr), ((size) > 0 ? (size) : 1))
# define Xcalloc(nelem, elsize) calloc(((nelem) > 0 ? (nelem) : 1), (elsize))

#else

# define Xmalloc(size) malloc((size))
# define Xrealloc(ptr, size) realloc((ptr), (size))
# define Xcalloc(nelem, elsize) calloc((nelem), (elsize))

#endif

/*
 *	ReadvFromServer and WritevToSever use struct iovec, normally found
 *	in Berkeley systems in <sys/uio.h>.  See the readv(2) and writev(2)
 *	manual pages for details.
 *
 *	struct iovec {
 *		caddr_t iov_base;
 *		int iov_len;
 *	};
 */
#ifdef USG
#if !defined(CRAY) && !defined(umips)
struct iovec {
    caddr_t iov_base;
    int iov_len;
};
#ifndef __TIMEVAL__
#define __TIMEVAL__
struct timeval {			/* BSD has in <sys/time.h> */
    long tv_sec;
    long tv_usec;
};
#endif
#endif
#endif


#ifdef STREAMSCONN
#include "Astreams.h"

#if (!defined(EWOULDBLOCK)) && defined(EAGAIN)
#define EWOULDBLOCK EAGAIN
#endif

extern char _AsTypeOfStream[];
extern Astream _AsStream[];

#define ReadFromServer(dpy, data, size) \
	(*_AsStream[_AsTypeOfStream[dpy]].ReadFromStream)((dpy), (data), (size), \
						     BUFFERING)
#define WriteToServer(dpy, bufind, size) \
	(*_AsStream[_AsTypeOfStream[dpy]].WriteToStream)((dpy), (bufind), (size))

#else /* else not STREAMSCONN */

/*
 * bsd can read from sockets directly
 */
#define ReadFromServer(dpy, data, size) read((dpy), (data), (size))
#define WriteToServer(dpy, bufind, size) write((dpy), (bufind), (size))

#endif


#ifndef USG
#define _AReadV readv
#define _AWriteV writev
#endif

#define ReadvFromServer(dpy, iov, iovcnt) _AReadV((dpy), (iov), (iovcnt))
#define WritevToServer(dpy, iov, iovcnt) _AWriteV((dpy), (iov), (iovcnt))

#if	!defined(sgi) && !defined(ultrix) && !defined(__osf__)
extern char *index();
#endif
#define SearchString(string, char) index((string), (char))
