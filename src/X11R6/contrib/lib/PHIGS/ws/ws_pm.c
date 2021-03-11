/* $XConsortium: ws_pm.c,v 5.4 94/04/17 20:42:29 rws Exp $ */

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

/* PEX/PHIGS workstation utility functions for the A model (server side
 * workstations and structure storage).
 */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>


static void
done_button( w, client_data, call_data )
    Widget      w;
    XtPointer   client_data;
    XtPointer   call_data;
{
    Ws          *ws = (Ws *)client_data;

    XtPopdown( ws->msg_shell );
}

static void
create_message_win( ws )
    Ws		*ws;
{
    Widget      box, button;

    /* Create the containing shell. */
    ws->msg_shell = XtVaCreatePopupShell( "message",
	applicationShellWidgetClass, ws->shell, NULL );

    /* Create the containing box. */
    box = XtVaCreateManagedWidget( "box", boxWidgetClass, ws->msg_shell,
	NULL );

    /* Create the done button. */
    button = XtVaCreateManagedWidget( "button", commandWidgetClass, box,
	NULL );
    XtAddCallback( button, XtNcallback, done_button, (XtPointer)ws );
 
    /* Create the label. */
    ws->msg_label = XtVaCreateManagedWidget( "label", labelWidgetClass, box,
	NULL );
}

static void
message( ws, args )
    Ws			*ws;
    Phg_args_message	*args;
{
    if ( !ws->msg_shell )
	create_message_win( ws );

    if ( args->msg_length > 0 ) {
	/* The unmanage and manage is to get the parent box to allow the
	 * label resize.
	 */
	XtUnmanageChild( ws->msg_label );
	XtVaSetValues( ws->msg_label, XtNlabel, (XtArgVal)args->msg, NULL );
	XtManageChild( ws->msg_label );
	XtPopup( ws->msg_shell, XtGrabNone );
    }
}

void
phg_wsx_pm_create_message_win( ws )
    Ws		*ws;
{
    /* Don't really create it yet, wait until it's used. */
    ws->message = message;
}
