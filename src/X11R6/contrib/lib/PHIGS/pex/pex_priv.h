/* $XConsortium: pex_priv.h,v 5.6 94/04/17 20:42:16 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef PEX_INTERNAL_H
#define PEX_INTERNAL_H

#include <stdio.h>
#include <X11/Xmd.h>
#define NEED_REPLIES
#include <X11/Xlibint.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include "PEX.h"
#include "PEXproto.h"
#include "pex_md.h"

#ifdef CAT
#undef CAT
#endif
#ifdef CAT3
#undef CAT3
#endif

#if (__STDC__ && !defined(UNIXCPP)) || defined(ANSICPP)
#define CAT(a,b) a##b
#define CAT3(a,b,c) a##b##c
#else
#define CAT(a,b) a/**/b
#define CAT3(a,b,c) a/**/b/**/c
#endif

/** Does compiler know about structure assignment?? (STRUCTASS is
 ** either defined or undefined in pex_md.h)
 **/
#ifdef STRUCTASS
#define PEX_ASSIGN(a,b) a = b
#else
#define PEX_ASSIGN(a,b)			    \
  {					    \
        char *aptr = (char *) &(a);	    \
	char *bptr = (char *) &(b);	    \
	int n = sizeof(a);		    \
        while( n-- )			    \
	    *aptr++ = *bptr++;		    \
  }
#endif

#ifndef DEBUG
#define ASSERT(a)
#else
#define ASSERT(__assertion__)					\
  if (!(__assertion__)) {					\
     fprintf(stderr, "Assertion Failed: File %s, Line %d\n",	\
		__FILE__, __LINE__);				\
     fprintf(stderr, "Assertion: __assertion__\n\n");		\
  }
#endif /* DEBUG */

typedef char* Pointer;

#define PEX_SCRATCH( _s, _size) \
    ((_s)->scratch_size >= (_size) ? (_s)->scratch \
	: PexGrowScratch( (_s), (unsigned)(_size)))

#define PEX_OPCODE(_s) ((_s)->ext_codes->major_opcode)

#define PADDING(n) ( ((n)%4 == 0) ? 0 : 4 - ((n)%4) )

#ifndef MIN
#define MIN( a, b)	(((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX( a, b)	(((a) > (b)) ? (a) : (b))
#endif
#ifndef FABS
#define FABS( f) 	(((f) < 0.0) ? -(f) : (f))
#endif

typedef struct _Pex_srvr_info {
    Display			*display;
    XExtCodes			*ext_codes; /* from XInitExtension */
    unsigned			scratch_size;
    Pointer			scratch;
    struct _Pex_srvr_info	*next;
} Pex_srvr_info;

extern Pex_srvr_info	*Pex_srvr_list;

/* Utility functions */
extern Pex_srvr_info*	PexEntryCheck();
extern Pointer		PexGrowScratch();
extern void		PexClearReply();

/*
 * PEX_REQUEST -- Get the next available X request packet in the buffer.
 *     "name" is the name of the request.
 *     "dpy" is the display pointer
 *     "oc" is the PEX extension major opcode for the specified server
 *     "req" is the request pointer.
 */

#define	PEX_REQUEST(name,dpy,oc,req) \
	if (((dpy)->bufptr + sizeof(CAT3(pex,name,Req))) > (dpy)->bufmax)\
	    _XFlush(dpy);\
	(req) = (CAT3(pex,name,Req) *)((dpy)->last_req = (dpy)->bufptr);\
	(req)->reqType = (oc);\
	(req)->opcode = CAT(PEX_,name);\
	(req)->length = (sizeof(CAT3(pex,name,Req)))>>2;\
	(dpy)->bufptr += sizeof(CAT3(pex,name,Req));\
	(dpy)->request++

/*
 * PEX_VARIABLE_REQUEST -- Similiar to PEX_REQUEST but allocates "n"
 *                       additional bytes after the request.
 * "n" must be a multiple of 4 !!!!
 */

#define	PEX_VARIABLE_REQUEST(name,dpy,oc,n,req) \
    if (((dpy)->bufptr + sizeof(CAT3(pex,name,Req))+(n)) > (dpy)->bufmax)\
	_XFlush(dpy);\
    (req) = (CAT3(pex,name,Req) *) ((dpy)->last_req = (dpy)->bufptr);\
    (req)->reqType = (oc);\
    (req)->opcode = CAT(PEX_,name);\
    (req)->length = (sizeof(CAT3(pex,name,Req))+(n))>>2;\
    (dpy)->bufptr += sizeof(CAT3(pex,name,Req))+(n);\
    (dpy)->request++

/* PEX_VAR_REQUEST and PEX_LOAD_VAR_REQUEST_DATA are meant to be used as a
 * pair.  The former "allocates" and loads the request structures; the
 * latter puts the data in the display buffer, performing multiple copies
 * and flushes until all the data has been loaded.  This allows requests to
 * be larger than the display buffer size.
 */
typedef struct {
    int		size;
    char	*data;
    int		padding;
} Pex_data_vec;

#define	PEX_VAR_REQUEST(name,dpy,oc,n,req) \
    if (((dpy)->bufptr + sizeof(CAT3(pex,name,Req))+(n)) > (dpy)->bufmax)\
	_XFlush(dpy);\
    (req) = (CAT3(pex,name,Req) *) ((dpy)->last_req = (dpy)->bufptr);\
    (req)->reqType = (oc);\
    (req)->opcode = CAT(PEX_,name);\
    (req)->length = (sizeof(CAT3(pex,name,Req))+(n))>>2;\
    (dpy)->bufptr += sizeof(CAT3(pex,name,Req));\
    (dpy)->request++

#define PEX_LOAD_VAR_REQUEST_DATA(_dpy,_num,_vec) \
{   int		_todo, _avail; \
    char	*_dp; \
    int		_i; \
    for ( _i = 0; _i < (_num); _i++ ) { \
	_dp = (_vec)[_i].data; \
	_todo = (_vec)[_i].size; \
	while ( _todo > 0 ) { \
	    _avail = (_dpy)->bufmax - (_dpy)->bufptr; \
	    bcopy( _dp, (_dpy)->bufptr, MIN(_avail,_todo) ); \
	    (_dpy)->bufptr += MIN(_avail,_todo); \
	    (_dp) += MIN(_avail,_todo); \
	    _todo -= _avail; \
	    if ( _todo > 0 ) \
		_XFlush( (_dpy) ); \
	    else \
		(_dpy)->bufptr += (_vec)[_i].padding; \
	} \
    } \
}
/* This function only for test and debug. */
#ifdef USE_LOAD_VAR_FUNC
#define PEX_LOAD_VAR_REQUEST_DATA(a,b,c) pex_load_var_request_data(a,b,c);
static void
pex_load_var_request_data(_dpy,_num,_vec)
    Display		*_dpy;
    int			_num;
    Pex_data_vec	*_vec;
{   int		_todo, _avail;
    char	*_dp;
    int		_i;
    for ( _i = 0; _i < (_num); _i++ ) {
	_dp = (_vec)[_i].data;
	_todo = (_vec)[_i].size;
	while ( _todo > 0 ) {
	    _avail = (_dpy)->bufmax - (_dpy)->bufptr;
	    bcopy( _dp, (_dpy)->bufptr, MIN(_avail,_todo) );
	    (_dpy)->bufptr += MIN(_avail,_todo);
	    (_dp) += MIN(_avail,_todo);
	    _todo -= _avail;
	    if ( _todo > 0 )
		_XFlush( (_dpy) );
	    else
		(_dpy)->bufptr += (_vec)[_i].padding;
	}
    }
}
#endif

/*
 * PEX_RESOURCE_ID_REQUEST -- used for requests that have * a resource ID as
 *                         their single argument.
 *    "rid" is the name of the resource.
 */

#define	PEX_RESOURCE_ID_REQUEST(name,dpy,oc,rid,req) \
	if (((dpy)->bufptr + sizeof(pexResourceReq)) > (dpy)->bufmax) \
		_XFlush(dpy);\
	(req) = (pexResourceReq *) ((dpy)->last_req = (dpy)->bufptr);\
	(req)->reqType = (oc); \
	(req)->opcode = CAT(PEX_,name);\
	(req)->length = 2;\
	(req)->id = (rid);\
	(dpy)->bufptr += sizeof(pexResourceReq);\
	(dpy)->request++

/*
 * PEX_EMPTY_REQUEST -- used for requests that have no arguments at all.
 */

#define PEX_EMPTY_REQUEST(name,dpy,oc,req) \
		if ((dpy->bufptr + sizeof(pexReq)) > dpy->bufmax) \
			_XFlush(dpy); \
		req = (pexReq *) (dpy->last_req = dpy->bufptr); \
		req->reqType = api_globals.PEXReqCode; \
		req->opcode = CAT(PEX_,name); \
		req->length= 1; \
		dpy->bufptr += sizeof(pexReq); \
		dpy->request++

#define	PEX_FP_FORMAT(_f) (_f) = PEXIeee_754_32

#define PEX_FORMATS(_r) \
  { \
	PEX_FP_FORMAT((_r)->fpFormat); \
  }

/* The SyncHandle macro in Xlibint.h counts on the display pointer being
 * named "dpy", this macro doesn't.
 */
#define	PEX_SYNC_HANDLE(dpy) \
    if (dpy->synchandler) (*dpy->synchandler)(dpy)

#define PEX_RESOURCE_ID_NO_REPLY_FUNC(_request) \
    int							\
    CAT(PEX,_request)(display, resource)		\
    Display	       *display;			\
    XID			resource;			\
    {							\
	int			     status = 0;	\
	Pex_srvr_info		    *srvr;		\
	CAT3(pex,_request,Req)	    *req;		\
							\
	LOCK_DISPLAY(display);				\
	if (srvr = PexEntryCheck(display, 1)) {		\
	    PEX_RESOURCE_ID_REQUEST(_request, display,	\
		    PEX_OPCODE(srvr), resource, req);	\
	    status = 1;					\
	}						\
	UNLOCK_DISPLAY(display);			\
	PEX_SYNC_HANDLE(display);			\
	return status;					\
    }


/* These two are defined to let us consistently use all caps for macros
 */
#define LOCK_DISPLAY(dpy) LockDisplay(dpy)
#define UNLOCK_DISPLAY(dpy) UnlockDisplay(dpy)

#endif
