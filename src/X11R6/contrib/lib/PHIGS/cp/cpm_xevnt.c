/* $XConsortium: cpm_xevnt.c,v 4.2 94/04/17 20:41:22 rws Exp $ */

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


#include <X11/Xlib.h>
#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"

typedef struct _Ntfy_entry {
    Display		*display;
    Window		window;
    caddr_t		client_data;
    void		(*callback)();
    struct _Ntfy_entry	*next;
} Ntfy_entry;

#define NTFY_NULL        (Ntfy_entry *)NULL

static Ntfy_entry *dispatch_list[] = {
        NTFY_NULL,	/* Illegal Event 	*/
        NTFY_NULL,	/* Illegal Event	*/
 	NTFY_NULL,	/* KeyPress             */
        NTFY_NULL,	/* KeyRelease           */
        NTFY_NULL,	/* ButtonPress          */
        NTFY_NULL,	/* ButtonRelease      5 */
        NTFY_NULL,	/* MotionNotify         */
        NTFY_NULL,	/* EnterNotify          */
        NTFY_NULL,	/* LeaveNotify          */
        NTFY_NULL,	/* FocusIn              */
        NTFY_NULL,	/* FocusOut          10 */
        NTFY_NULL,	/* KeymapNotify         */
        NTFY_NULL,	/* Expose               */
        NTFY_NULL,	/* GraphicsExpose       */
        NTFY_NULL,	/* NoExpose             */
        NTFY_NULL,	/* VisibilityNotify  15 */
        NTFY_NULL,	/* CreateNotify         */
        NTFY_NULL,	/* DestroyNotify        */
        NTFY_NULL,	/* UnmapNotify          */
        NTFY_NULL,	/* MapNotify            */
        NTFY_NULL,	/* MapRequest        20 */
        NTFY_NULL,	/* ReparentNotify       */
        NTFY_NULL,	/* ConfigureNotify      */
        NTFY_NULL,	/* ConfigureRequest     */
        NTFY_NULL,	/* GravityNotify        */
        NTFY_NULL,	/* ResizeRequest     25 */
        NTFY_NULL,	/* CirculateNotify      */
        NTFY_NULL,	/* CirculateRequest     */
        NTFY_NULL,	/* PropertyNotify       */
        NTFY_NULL,	/* SelectionClear       */
        NTFY_NULL,	/* SelectionRequest  30 */
        NTFY_NULL,	/* SelectionNotify      */
        NTFY_NULL,	/* ColormapNotify       */
        NTFY_NULL,	/* ClientMessage        */
        NTFY_NULL,	/* MappingNotify        */
        NTFY_NULL,	/* Unknown event type   */
};

static int	num_event_types =
		    sizeof(dispatch_list)/sizeof(dispatch_list[0]);

#define UNKNOWN_EVENT_ENTRY (num_event_types - 1)

#define KNOWN_EVENT_TYPE( _et ) \
    ((_et) >= 0 && (_et) < num_event_types - 1)


#ifdef DIAGNOSTIC
static char *eventNames[] = {
        "Illegal Event",
        "Illegal Event",
        "KeyPress",
        "KeyRelease",
        "ButtonPress",
        "ButtonRelease",
        "MotionNotify",
        "EnterNotify",
        "LeaveNotify",
        "FocusIn",
        "FocusOut",
        "KeymapNotify",
        "Expose",
        "GraphicsExpose",
        "NoExpose",
        "VisibilityNotify",
        "CreateNotify",
        "DestroyNotify",
        "UnmapNotify",
        "MapNotify",
        "MapRequest",
        "ReparentNotify",
        "ConfigureNotify",
        "ConfigureRequest",
        "GravityNotify",
        "ResizeRequest",
        "CirculateNotify",
        "CirculateRequest",
        "PropertyNotify",
        "SelectionClear",
        "SelectionRequest",
        "SelectionNotify",
        "ColormapNotify",
        "ClientMessage",
        "MappingNotify",
        "unknown event type"
};
#endif


int
phg_ntfy_register_event( display, window, event_type, cdata, callback )
    Display	*display;
    Window	window;
    int		event_type;
    caddr_t	cdata;
    void	(*callback)();
{
    int		status = 1;

    register	Ntfy_entry	**nep;

    /* Add or replace an entry to the dispatch list. */
    nep = KNOWN_EVENT_TYPE(event_type) ?
	&dispatch_list[event_type] : &dispatch_list[UNKNOWN_EVENT_ENTRY];
    for ( ; *nep; nep = &(*nep)->next ) {
	if ( (*nep)->display == display && (*nep)->window == window
		&& (*nep)->client_data == cdata ) {
	    /* Replace the currently installed callback. */
	    (*nep)->callback = callback;
	    break;
	}
    }
    if ( *nep == NTFY_NULL ) {
	/* This is a new entry, add it. */
	if ( *nep = (Ntfy_entry *)malloc(sizeof(Ntfy_entry)) ) {
	    (*nep)->next = NTFY_NULL;
	    (*nep)->display = display;
	    (*nep)->window = window;
	    (*nep)->client_data = cdata;
	    (*nep)->callback = callback;
	} else
	    status = 0;
    }
    return status;
}


void
phg_ntfy_unregister_event( display, window, event_type, cdata )
    Display	*display;
    Window	window;
    int		event_type;
    caddr_t	cdata;
{
    Ntfy_entry	*ne;

    register	Ntfy_entry	**nep;

    /* Remove an entry from the dispatch list. */
    nep = KNOWN_EVENT_TYPE(event_type) ?
	&dispatch_list[event_type] : &dispatch_list[UNKNOWN_EVENT_ENTRY];
    while ( *nep ) {
	if ( (*nep)->display == display && (*nep)->window == window
		&& (*nep)->client_data == cdata ) {
	    ne = *nep;
	    *nep = (*nep)->next;
	    free( (char *)ne );
	    break;
	} else
	    nep = &(*nep)->next;
    }
}


void
phg_ntfy_unregister_display( display )
    Display	*display;
{
    Ntfy_entry	*ne;

    register	Ntfy_entry	**nep;
    register	int		count;

    /* Remove all entries for this display from the dispatch list. */
    count = num_event_types;
    while ( count-- ) {
	nep = &dispatch_list[count];
	while ( *nep ) {
	    if ( (*nep)->display == display ) {
		ne = *nep;
		*nep = (*nep)->next;
		free( (char *)ne );
	    } else
		nep = &(*nep)->next;
	}
    }
}


void
phg_ntfy_unregister_window( display, window )
    Display	*display;
    Window	window;
{
    Ntfy_entry	*ne;

    register	Ntfy_entry	**nep;
    register	int		count;

    /* Remove all entries for this window from the dispatch list. */
    count = num_event_types;
    while ( count-- ) {
	nep = &dispatch_list[count];
	while ( *nep ) {
	    if ( (*nep)->display == display && (*nep)->window == window ) {
		ne = *nep;
		*nep = (*nep)->next;
		free( (char *)ne );
	    } else
		nep = &(*nep)->next;
	}
    }
}


void
phg_ntfy_dispatch_event( display, event )
    Display	*display;
    XEvent	*event;
{
    Ntfy_entry		*ne;

    ne = KNOWN_EVENT_TYPE(event->type) ?
	dispatch_list[event->type] : dispatch_list[UNKNOWN_EVENT_ENTRY];
    for ( ; ne; ne = ne->next ) {
	if ( ne->display == display && ne->window == event->xany.window )
	    (*ne->callback)( display, event->xany.window, ne->client_data,
		event );
    }
}
