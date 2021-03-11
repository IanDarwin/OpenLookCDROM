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
/* $RCS: Alibint.h,v 11.79 89/12/12 12:13:42 jim Exp $ */
/* Copyright 1984, 1985, 1987, 1989  Massachusetts Institute of Technology */
/*
 * Alibint.h - Header definition and support file for the internal
 *	support routines (AlibInternal) used by the C subroutine interface
 *	library (Alib)
 *
 *	Warning, there be dragons here....
 */

#ifndef NEED_EVENTS
#define _AEvent_
#endif

#ifdef USG
#ifndef __TYPES__
#include <sys/types.h>			/* forgot to protect it... */
#define __TYPES__
#endif
#else
#include <sys/types.h>
#endif

/*
 * define the following if you want the Data macro to be a procedure instead
 */
#if defined(CRAY)
#define DataRoutineIsProcedure
#endif

#include <AF/AFlib.h>

#include <AF/audioproto.h>
#include <errno.h>
#include <AF/Alibos.h>

#ifdef __cplusplus			/* do not leave open across includes */
extern "C" {					/* for C++ V2.0 */
#endif

#ifndef NULL
#define NULL 0L
#endif
#define LOCKED 1
#define UNLOCKED 0

extern int errno;			/* Internal system error number. */

extern int _AError(AFAudioConn *, aError *); 
					/* prepare to upcall user handler */
extern int _AIOError(AFAudioConn *);	/* prepare to upcall user handler */
extern int _ADefaultError(AFAudioConn *, AFErrorEvent *);
extern int _ADefaultIOError(AFAudioConn *);
extern int (*_AIOErrorFunction)(AFAudioConn *);	
					/* A system error reporting routine. */
extern int (*_AErrorFunction)(AFAudioConn *, AFErrorEvent *);
					/* A_Error event reporting routine. */
extern void _AEatData(AFAudioConn *, unsigned long); 
					/* swallow data from server */
extern char *_AAllocScratch(AFAudioConn *, unsigned long);
					/* fast memory allocator */
extern unsigned long _ASetLastRequestRead(AFAudioConn *, aGenericReply *);
					/* update aud->last_request_read */
extern int _AGetHostname(char *buf, int maxlen);
					/* get name of this machine */

#ifndef BUFSIZE
#define BUFSIZE 2048			/* A output buffer size. */
#endif
#ifndef EPERBATCH
#define EPERBATCH 8			/* when batching, how many elements */
#endif
#ifndef CHUNKSIZE			/* chuncking size on play and record */
#define CHUNKSIZE 8192			/* default just under smallest socket
					   buffering size */
#endif

/*
 * display flags
 */
#define AlibAudioConnIOError	(1L << 0)
#define AlibAudioConnClosing	(1L << 1)

/*
 * A Protocol packetizing macros.
 */

/*   Need to start requests on 64 bit word boundries
 *   on a CRAY computer so add a NoOp (127) if needed.
 *   A character pointer on a CRAY computer will be non-zero
 *   after shifting right 61 bits of it is not pointing to
 *   a word boundary.
 */
#ifdef WORD64
#define WORD64ALIGN if ((long)aud->bufptr >> 61) {\
           aud->last_req = aud->bufptr;\
           *(aud->bufptr)   = A_NoOperation;\
           *(aud->bufptr+1) =  0;\
           *(aud->bufptr+2) =  0;\
           *(aud->bufptr+3) =  1;\
             aud->request += 1;\
             aud->bufptr += 4;\
         }
#else /* else does not require alignment on 64-bit boundaries */
#define WORD64ALIGN
#endif


/*
 * GetReq - Get the next avilable A request packet in the buffer and
 * return it. 
 *
 * "name" is the name of the request, e.g. CreatePixmap, OpenFont, etc.
 * "req" is the name of the request pointer.
 *
 */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetReq(name, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(a##name##Req)) > aud->bufmax)\
		_AFlush(aud);\
	req = (a##name##Req *)(aud->last_req = aud->bufptr);\
	req->reqType = A_##name;\
	req->length = (SIZEOF(a##name##Req))>>2;\
	aud->bufptr += SIZEOF(a##name##Req);\
	aud->request++

#else  /* non-ANSI C uses empty comment instead of "##" for token concatenation */
#define GetReq(name, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(a/**/name/**/Req)) > aud->bufmax)\
		_AFlush(aud);\
	req = (a/**/name/**/Req *)(aud->last_req = aud->bufptr);\
	req->reqType = A_/**/name;\
	req->length = (SIZEOF(a/**/name/**/Req))>>2;\
	aud->bufptr += SIZEOF(a/**/name/**/Req);\
	aud->request++
#endif

/* GetReqExtra is the same as GetReq, but allocates "n" additional
   bytes after the request. "n" must be a multiple of 4!  */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetReqExtra(name, n, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(a##name##Req) + n) > aud->bufmax)\
		_AFlush(aud);\
	req = (a##name##Req *)(aud->last_req = aud->bufptr);\
	req->reqType = A_##name;\
	req->length = (SIZEOF(a##name##Req) + n)>>2;\
	aud->bufptr += SIZEOF(a##name##Req) + n;\
	aud->request++
#else
#define GetReqExtra(name, n, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(a/**/name/**/Req) + n) > aud->bufmax)\
		_AFlush(aud);\
	req = (a/**/name/**/Req *)(aud->last_req = aud->bufptr);\
	req->reqType = A_/**/name;\
	req->length = (SIZEOF(a/**/name/**/Req) + n)>>2;\
	aud->bufptr += SIZEOF(a/**/name/**/Req) + n;\
	aud->request++
#endif


/*
 * GetResReq is for those requests that have a resource ID 
 * (Device, AContext, etc.) as their single argument.
 * "rid" is the name of the resource. 
 */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetResReq(name, rid, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(aResourceReq)) > aud->bufmax)\
	    _AFlush(aud);\
	req = (aResourceReq *) (aud->last_req = aud->bufptr);\
	req->reqType = A_##name;\
	req->length = 2;\
	req->id = (rid);\
	aud->bufptr += SIZEOF(aResourceReq);\
	aud->request++
#else
#define GetResReq(name, rid, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(aResourceReq)) > aud->bufmax)\
	    _AFlush(aud);\
	req = (aResourceReq *) (aud->last_req = aud->bufptr);\
	req->reqType = A_/**/name;\
	req->length = 2;\
	req->id = (rid);\
	aud->bufptr += SIZEOF(aResourceReq);\
	aud->request++
#endif

/*
 * GetEmptyReq is for those requests that have no arguments
 * at all. 
 */
#if defined(__STDC__) && !defined(UNIXCPP)
#define GetEmptyReq(name, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(aReq)) > aud->bufmax)\
	    _AFlush(aud);\
	req = (aReq *) (aud->last_req = aud->bufptr);\
	req->reqType = A_##name;\
	req->length = 1;\
	aud->bufptr += SIZEOF(aReq);\
	aud->request++
#else
#define GetEmptyReq(name, req) \
        WORD64ALIGN\
	if ((aud->bufptr + SIZEOF(aReq)) > aud->bufmax)\
	    _AFlush(aud);\
	req = (aReq *) (aud->last_req = aud->bufptr);\
	req->reqType = A_/**/name;\
	req->length = 1;\
	aud->bufptr += SIZEOF(aReq);\
	aud->request++
#endif


#define SyncHandle() \
	if (aud->synchandler) (*aud->synchandler)(aud)

/*
 * Data - Place data in the buffer and pad the end to provide
 * 32 bit word alignment.  Transmit if the buffer fills.
 *
 * "aud" is a pointer to a Display.
 * "data" is a pinter to a data buffer.
 * "len" is the length of the data buffer.
 * we can presume buffer less than 2^16 bytes, so bcopy can be used safely.
 */
#ifdef DataRoutineIsProcedure
extern void Data();
#else
#define Data(aud, data, len) \
	if (aud->bufptr + (len) <= aud->bufmax) {\
		bcopy(data, aud->bufptr, (int)len);\
		aud->bufptr += ((len) + 3) & ~3;\
	} else\
		_ASend(aud, data, len)
#endif


/* Allocate bytes from the buffer.  No padding is done, so if
 * the length is not a multiple of 4, the caller must be
 * careful to leave the buffer aligned after sending the
 * current request.
 *
 * "type" is the type of the pointer being assigned to.
 * "ptr" is the pointer being assigned to.
 * "n" is the number of bytes to allocate.
 *
 * Example: 
 *    aTextElt *elt;
 */

#define BufAlloc(type, ptr, n) \
    if (aud->bufptr + (n) > aud->bufmax) \
        _AFlush (aud); \
    ptr = (type) aud->bufptr; \
    aud->bufptr += (n);

/*
 * provide emulation routines for smaller architectures
 */
#ifndef WORD64
#define Data16(aud, data, len) Data((aud), (char *)(data), (len))
#define Data32(aud, data, len) Data((aud), (char *)(data), (len))
#define _ARead16Pad(aud, data, len) _AReadPad((aud), (char *)(data), (len))
#define _ARead16(aud, data, len) _ARead((aud), (char *)(data), (len))
#define _ARead32(aud, data, len) _ARead((aud), (char *)(data), (len))
#endif

#define PackData16(aud,data,len) Data16 (aud, data, len)
#define PackData32(aud,data,len) Data32 (aud, data, len)

/* Alib manual is bogus */
#define PackData(aud,data,len) PackData16 (aud, data, len)

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))


#ifdef MUSTCOPY

/* a little bit of magic */
#define OneDataCard32(aud,dstaddr,srcvar) \
  { aud->bufptr -= 4; Data32 (aud, (char *) &(srcvar), 4); }

#define STARTITERATE(tpvar,type,start,endcond,decr) \
  { register char *cpvar; \
  for (cpvar = (char *) start; endcond; cpvar = NEXTPTR(cpvar,type), decr) { \
    type dummy; bcopy (cpvar, (char *) &dummy, SIZEOF(type)); \
    tpvar = (type *) cpvar;
#define ENDITERATE }}

#else

/* srcvar must be a variable for large architecture version */
#define OneDataCard32(aud,dstaddr,srcvar) \
  { *(unsigned long *)(dstaddr) = (srcvar); }

#define STARTITERATE(tpvar,type,start,endcond,decr) \
  for (tpvar = (type *) start; endcond; tpvar++, decr) {
#define ENDITERATE }

#endif



/*
 * This structure is private to the library.
 */
typedef struct _AFreeFuncs {
    void (*atoms)(AFAudioConn *);            /* _AFreeAtomTable */
} _AFreeFuncRec;

/* */

AID _AAllocID(AFAudioConn *aud);

int _AEnq (AFAudioConn *, aEvent *);
void _AFlush(AFAudioConn *aud);
void _ASend (AFAudioConn *, char *, long);
AStatus _AReply (AFAudioConn *, aReply *, int, ABool);

void _AWaitForWritable(AFAudioConn *aud);
void _AWaitForReadable(AFAudioConn *aud);

void _ARead(AFAudioConn *, char *, long);
void _AReadPad (AFAudioConn *, char *, long);
void _AReadEvents(AFAudioConn *);

void _AFProcessACAttributes (AFAudioConn *, AC, aChangeACAttributesReq *,  
	unsigned long,AFSetACAttributes *);

int _AEventsQueued (AFAudioConn *, int);

int _ADisconnectAudioConn (int);

void _AFreeAudioConnStructure(AFAudioConn *);

void _AFreeExtData (AFExtData *extension);
void _ASendClientPrefix (AFAudioConn *, aConnClientPrefix *, char *, char *);
int _AConnectAudioConn(
    char *display_name,
    char **fullnamep			/* RETURN */,
    int *audnump			/* RETURN */,
    int *screenp			/* RETURN */,
    int *familyp			/* RETURN */,
    int *saddrlenp			/* RETURN */,
    char **saddrp			/* RETURN, freed by caller */
    );

ABool _AWireToEvent(
	AFAudioConn *aud,	/* pointer to AFAudioConn structure */
	AFEvent *re,	/* pointer to where event should be reformatted */
	aEvent *event	/* wire protocol event */
	);

ABool _AUnknownWireEvent(
	register AFAudioConn *aud,	/* pointer to AFAudioConn structure */
	register AFEvent *re,	/* pointer to where event should be reformatted */
	register aEvent *event	/* wire protocol event */
	);
AStatus _AUnknownNativeEvent(	
	AFAudioConn *aud,	/* pointer to AFAudioConn structure */
	AFEvent *re,		/* pointer to where event should be reformatted */
	aEvent *
	);

/* extension hooks */

extern int (*AFESetCloseAudioConn(
    AFAudioConn*			/* audio connection */,
    int					/* extension */,
    int (*) ( AFAudioConn*		/* audio connection */,
              AFExtCodes*		/* codes */
            )				/* proc */    
))(
#ifndef	mips
AFAudioConn*, AFExtCodes*
#endif
);

extern int (*AFESetError(
    AFAudioConn*			/* audio connection */,
    int					/* extension */,
    int (*) ( AFAudioConn*		/* audio connection */,
              aError*			/* err */,
              AFExtCodes*		/* codes */,
              int*			/* ret_code */
            )				/* proc */    
))(
#ifndef mips
AFAudioConn*, aError*, AFExtCodes*, int*
#endif
);

extern char* (*AFESetErrorString(
    AFAudioConn*			/* audio connection */,
    int					/* extension */,
    char* (*) ( AFAudioConn*		/* audio connection */,
                int			/* code */,
                AFExtCodes*		/* codes */,
                char*			/* buffer */,
                int			/* nbytes */
              )				/* proc */	       
))(
#ifndef	mips
AFAudioConn*, int, AFExtCodes*, char*, int
#endif
);

extern int (*AFESetWireToEvent(
    AFAudioConn*			/* audio connection */,
    int					/* event_number */,
    ABool (*) ( AFAudioConn*		/* audio connection */,
               AFEvent*			/* re */,
               aEvent*			/* event */
             )				/* proc */    
))(
#ifndef	mips
AFAudioConn*, AFEvent*, aEvent*
#endif
);

extern int (*AFESetEventToWire(
    AFAudioConn*			/* audio connection */,
    int	               			/* event_number */,
    int (*) ( AFAudioConn*		/* audio connection */,
              AFEvent*			/* re */,
              aEvent*			/* event */
            )				/* proc */   
))(
#ifndef	mips
AFAudioConn*, AFEvent*, aEvent*
#endif
);

#ifdef __cplusplus
}						/* for C++ V2.0 */
#endif


