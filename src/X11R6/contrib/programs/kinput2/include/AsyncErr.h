/* $Id: AsyncErr.h,v 1.2 1994/05/16 09:07:20 ishisone Rel $ */
/*
 * Copyright (C) 1992, 1994  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _AsyncErr_h
#define _AsyncErr_h

/*
 * X asyncronous error handler
 *
 * This library provides asyncronous error handler mechanism.
 * It enables setting error handlers for particular requests
 * without using XSync().
 */

/*
 * XAEHandle -- an opaque type used as a 'handle' in this library
 */
typedef struct fe_errdesc_ *XAEHandle;

/*
 * XAEHandler -- an error handler for the async error handler mechanism
 *  This function takes an error event and dispatches it to
 *  the appropriate async error handler.
 *  To enable async error handler mechanism, set this function as
 *  the global error handler using XSetErrorHandler, or just call
 *  XAEInit, which does the same thing.
 */
extern int XAEHandler(
#if NeedFunctionPrototypes
	Display *dpy,
	XErrorEvent *eev
#endif
);

/*
 * XAESetErrorHandler -- XSetErrorHandler replacement
 *   In order to set the global error handler, this function
 *   should be used instead of XSetErrorHandler when using
 *   async error handler mechanism.
 */
extern XErrorHandler XAESetErrorHandler(
#if NeedFunctionPrototypes
	XErrorHandler handler
#endif
);

/*
 * XAEInit -- async error handler mechanism initializer
 */
extern void XAEInit(
#if NeedFunctionPrototypes
	void
#endif
);

/*
 * XAESet -- set an async error handler
 * XAEUnset -- unset handler set by XAEOpen
 *   These function set and unset the specified async error handler.
 *   Errors due to the requests issued between calls of XAESet and the
 *   corresponding XAEUnset are handled by the handler.
 */
extern XAEHandle XAESet(
#if NeedFunctionPrototypes
	Display *dpy,
	int (*handler)(),	/* error handler to be called */
	void (*destroy)(),	/* destroy hook (for freeing cldata etc.) */
	XPointer client_data
#endif
);

extern void XAEUnset(
#if NeedFunctionPrototypes
	XAEHandle handle	/* handle returned previous XAESet() call */
#endif
);

/*
 * XAESetIgnoreErrors -- ignore all errors
 *   This is a convenient function for setting an error handler that
 *   ignores all the errors.  It is equivalent to
 *	XAESet(dpy, ignore_error_handler, NULL, NULL)
 *   where ignore_error_handler is an error handler that simply
 *   ignores all kind of errors.
 */
extern XAEHandle XAESetIgnoreErrors(
#if NeedFunctionPrototypes
	Display *dpy
#endif
);

extern XAEHandle XAESetRecordErrors(
#if NeedFunctionPrototypes
	Display *dpy,
	unsigned long *errorbitsp
#endif
);


/*
 * Replace XSetErrorHandler with XAESetErrorHandler
 */
#define XSetErrorHandler XAESetErrorHandler

#endif /* _AsyncErr_h */
