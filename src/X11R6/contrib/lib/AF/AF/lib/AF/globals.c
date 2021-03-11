/*
 * $RCS: globals.c,v 1.7 89/06/16 16:57:00 jim Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 *
 *                                 Global data
 *
 * This file should contain only those objects which must be predefined.
 */
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
#include "AFlib.h"
#include <sys/param.h>			/* for definition of NULL */


/*
 * If possible, it is useful to have the global data default to a null value.
 * Some shared library implementations are *much* happier if there isn't any
 * global initialized data.
 */
#ifdef NULL_NOT_ZERO			/* then need to initialize */
#define SetZero(t,var,z) t var = z
#else 
#define SetZero(t,var,z) t var
#endif

#ifdef ATTSHAREDLIB			/* then need extra variables */
/*
 * If we need to define extra variables for each global
 */
#if defined(__STDC__) && !defined(UNIXCPP)  /* then ANSI C concatenation */
#define ZEROINIT(t,var,val) SetZero(t,var,val); \
  SetZero (long, _libAF_##var##Flag, 0); \
  SetZero (void *, _libAF_##var##Ptr, NULL)
#else /* else pcc concatenation */
#define ZEROINIT(t,var,val) SetZero(t,var,val); \
  SetZero (long, _libAF_/**/var/**/Flag, 0); \
  SetZero (void *, _libAF_/**/var/**/Ptr, NULL)
#endif

#else /* else not ATTSHAREDLIB */
/*
 * no extra crud
 */
#define ZEROINIT(t,var,val) SetZero (t, var, val)

#endif


/*
 * Error handlers; used to be in AlibInt.c
 */
typedef int (*funcptr1)(AFAudioConn *, AFErrorEvent *);
typedef int (*funcptr2)(AFAudioConn *);
ZEROINIT (funcptr1, _AErrorFunction, NULL);
ZEROINIT (funcptr2, _AIOErrorFunction, NULL);
ZEROINIT (_AFQEvent *, _afqfree, NULL);


/*
 * Debugging information and display list; used to be in OpenConn.c
 */
ZEROINIT (int, _Adebug, 0);
ZEROINIT (AFAudioConn *, _AHeadOfAudioConnList, NULL);




#ifdef STREAMSCONN
/*
 * descriptor block for streams connections
 */
#include "Astreams.h"

char _AsTypeOfStream[100] = { 0 };

extern int SetupLocalStream();
extern int ConnectLocalClient();
extern int CallLocalServer();
extern int ReadLocalStream();
extern int ErrorCall();
extern int WriteLocalStream();
extern int CloseLocalStream(); 
extern int SetupTliStream();
extern int ConnectTliClient();
extern int CallTliServer(); 
extern int ReadTliStream(); 
extern int WriteTliStream();
extern int CloseTliStream();
extern int ErrorCall(); 

Astream _AsStream[] = {
    { 
	SetupLocalStream,
	ConnectLocalClient,
	CallLocalServer,
	ReadLocalStream,
	ErrorCall,
	WriteLocalStream,
	CloseLocalStream,
	NULL
    },
    { 
	SetupTliStream,
	ConnectTliClient,
	CallTliServer, 
	ReadTliStream, 
	ErrorCall, 
	WriteTliStream,
	CloseTliStream,
	NULL
    },
    { 
	SetupTliStream,
	ConnectTliClient,
	CallTliServer, 
	ReadTliStream, 
	ErrorCall, 
	WriteTliStream,
	CloseTliStream,
	NULL
    },
    { 
	NULL,
	NULL, 
	NULL, 
	NULL, 
	NULL,
	NULL,
	NULL,
	NULL
    },
    { 
	NULL,
	NULL,
	NULL,
	NULL,
	NULL, 
	NULL, 
	NULL,
	NULL
    },
    { 
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL, 
	NULL, 
	NULL
    }
};
#endif

#ifdef ATEST1
/*
 * Stuff for input synthesis extension:
 */
/*
 * Holds the two event type codes for this extension.  The event type codes
 * for this extension may vary depending on how many extensions are installed
 * already, so the initial values given below will be added to the base event
 * code that is aquired when this extension is installed.
 *
 * These two variables must be available to programs that use this extension.
 */
int			ATestInputActionType = 0;
int			ATestFakeAckType   = 1;
#endif

/*
 * NOTE: any additional external definition NEED
 * to be inserted BELOW this point!!!
 */

/*
 * NOTE: any additional external definition NEED
 * to be inserted ABOVE this point!!!
 */

