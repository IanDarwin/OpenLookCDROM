/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpFlt.c	1.8 94/02/16 */
/******************************************************************
 
              Copyright 1994 by Sun Microsystems, Inc.
              Copyright 1994 by Hewlett-Packard Company
 
Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Sun Microsystems, Inc.
and Hewlett-Packard not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior permission.
Sun Microsystems, Inc. and Hewlett-Packard make no representations about
the suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
 
SUN MICROSYSTEMS INC. AND HEWLETT-PACKARD COMPANY DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
SUN MICROSYSTEMS, INC. AND HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 
  Author: Hiromu Inukai (inukai@Japan.Sun.COM) Sun Microsystems, Inc.
          Hidetoshi Tajima(tajima@kobe.hp.com) Hewlett-Packard Company.
 
******************************************************************/

#ifndef	_XIMPFLT_C_
#define	_XIMPFLT_C_
#include "XimpData.h"

#if NeedFunctionPrototypes
extern void _XimpRegisterDestroyFilter(XIMS, XimpClient *);
extern void _XimpRegisterKeyPressFilter(XIMS, XimpClient *);
extern void _XimpRegisterKeyReleaseFilter(XIMS, XimpClient *);
extern void _XimpUnregisterDestroyFilter(XIMS, XimpClient *);
extern void _XimpUnregisterKeyPressFilter(XIMS, XimpClient *);
extern void _XimpUnregisterKeyReleaseFilter(XIMS, XimpClient *);
#else
extern void _XimpRegisterDestroyFilter();
extern void _XimpRegisterKeyPressFilter();
extern void _XimpRegisterKeyReleaseFilter();
extern void _XimpUnregisterDestroyFilter();
extern void _XimpUnregisterKeyPressFilter();
extern void _XimpUnregisterKeyReleaseFilter();
#endif

static Bool
#if NeedFunctionPrototypes
XimpKeyPressFilter(Display *dpy, Window win,
		   XEvent* ev, XPointer client_data)
#else
XimpKeyPressFilter(dpy, win, ev, client_data)
Display *dpy;
Window win;
XEvent* ev;
XPointer client_data;
#endif
{
    XIMS xims = (XIMS)client_data;
    XIMPCore core = (XIMPCore)xims->protocol;
    XClientMessageEvent event;
    XimpClient *client = core->clients;

    /* Find client from focus window ID */
    while (client != NULL) {
	if (client->focus_window == ev->xany.window) break;
	client = client->next;
    }
    if (client == (XimpClient *)NULL) return False;

    memset(&event, 0, sizeof(XClientMessageEvent));
    event.type		= ClientMessage;
    event.serial	= ev->xany.serial;
    event.send_event	= ev->xany.send_event;
    event.display	= ev->xany.display;
    event.message_type	= core->ximp_request;
    event.window	= ev->xany.window;
    event.format	= 32;

    event.data.l[0]	= (IS_VERSION_40(client) ?
			   XIMP_KEYPRESS4 :
			   XIMP_KEYPRESS3);
    event.data.l[1]	= client->id;
    event.data.l[2]	= ev->xkey.keycode;
    event.data.l[3]	= ev->xkey.state;
    event.data.l[4]	= ev->xkey.time;
    return (_XimpProcKeyPress(xims, (XClientMessageEvent*)&event));
}

static Bool
#if NeedFunctionPrototypes
XimpKeyReleaseFilter(Display *dpy, Window win,
		     XEvent* ev, XPointer client_data)
#else
XimpKeyReleaseFilter(dpy, win, ev, client_data)
Display *dpy;
Window win;
XEvent* ev;
XPointer client_data;
#endif
{
    XIMS xims = (XIMS)client_data;
    XIMPCore core = (XIMPCore)xims->protocol;
    XClientMessageEvent event;
    XimpClient *client = core->clients;

    /* Find client from focus window ID */
    while (client != NULL) {
	if (client->focus_window == ev->xany.window) break;
	client = client->next;
    }
    if (client == (XimpClient *)NULL) return False;

    if (!IS_VERSION_40(client)) return False;

    memset(&event, 0, sizeof(XClientMessageEvent));
    event.type		= ClientMessage;
    event.serial	= ev->xany.serial;
    event.send_event	= ev->xany.send_event;
    event.display	= ev->xany.display;
    event.message_type	= core->ximp_request;
    event.window	= ev->xany.window;
    event.format	= 32;

    event.data.l[0]	= XIMP_KEYRELEASE4;
    event.data.l[1]	= client->id;
    event.data.l[2]	= ev->xkey.keycode;
    event.data.l[3]	= ev->xkey.state;
    event.data.l[4]	= ev->xkey.time;

    return (_XimpProcKeyRelease(xims, (XClientMessageEvent*)&event));
}

static Bool
#if NeedFunctionPrototypes
XimpDestroyFilter(Display *dpy, Window win,
		  XEvent *ev, XPointer client_data)
#else
XimpDestroyFilter(dpy, win, ev, client_data)
Display *dpy;
Window win;
XEvent *ev;
XPointer client_data;
#endif
{
    XIMS xims = (XIMS)client_data;
    XIMPCore core = (XIMPCore)xims->protocol;
    XimpClient *client = core->clients;
    
    /* Find client from focus window ID */
    while (client != NULL) {
	if (client->focus_window == ev->xany.window) break;
	client = client->next;
    }
    if (client == (XimpClient *)NULL) return False;

    if (client->filters & FLT_KEYPRESS)
      _XUnregisterFilter(dpy, client->focus_window,
			 XimpKeyPressFilter, xims);
    if (client->filters & FLT_KEYRELEASE)
      _XUnregisterFilter(dpy, client->focus_window,
			 XimpKeyReleaseFilter, xims);
    if (client->filters & FLT_DESTROY)
	_XUnregisterFilter(dpy, client->focus_window,
			   XimpDestroyFilter, xims);
    _XimpDeleteClient(core, client->id);

    return False;    /* This filter always returns False so that
		      * the clients' X event handler can hook this
		      * event and process accrodingly.
		      */
}

void
#if NeedFunctionPrototypes
_XimpRegisterDestroyFilter(XIMS xims, XimpClient *client)
#else
_XimpRegisterDestroyFilter(xims, client)
XIMS xims;
XimpClient *client;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    XWindowAttributes win_attr;

    if (!(client->filters & FLT_DESTROY)) {
	XGetWindowAttributes(display, client->focus_window, &win_attr);
	XSelectInput(display, client->focus_window,
		     (win_attr.your_event_mask | StructureNotifyMask));
	_XRegisterFilterByType(display, client->focus_window,
			       DestroyNotify, DestroyNotify,
			       XimpDestroyFilter, xims);
	client->filters |= FLT_DESTROY;
    }
}

void
#if NeedFunctionPrototypes
_XimpRegisterKeyPressFilter(XIMS xims, XimpClient *client)
#else
_XimpRegisterKeyPressFilter(xims, client)
XIMS xims;
XimpClient *client;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    XWindowAttributes win_attr;

    if (!(client->filters & FLT_KEYPRESS)) {
	XGetWindowAttributes(display, client->focus_window, &win_attr);
	XSelectInput(display, client->focus_window,
		     (win_attr.your_event_mask | KeyPressMask));
	_XRegisterFilterByType(display, client->focus_window,
			       KeyPress, KeyPress,
			       XimpKeyPressFilter, xims);
	client->filters |= FLT_KEYPRESS;
    }
}

void
#if NeedFunctionPrototypes
_XimpRegisterKeyReleaseFilter(XIMS xims, XimpClient *client)
#else
_XimpRegisterKeyReleaseFilter(xims, client)
XIMS xims;
XimpClient *client;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    XWindowAttributes win_attr;

    if (!IS_VERSION_40(client) ||
	!(client->ev_masks & KeyReleaseMask)) return;

    if (!(client->filters & FLT_KEYRELEASE)) {
	XGetWindowAttributes(display, client->focus_window, &win_attr);
	XSelectInput(display, client->focus_window,
		     (win_attr.your_event_mask | KeyPressMask));
	_XRegisterFilterByType(display, client->focus_window,
			       KeyRelease, KeyRelease,
			       XimpKeyReleaseFilter, xims);
	client->filters |= FLT_KEYRELEASE;
    }
}

void
#if NeedFunctionPrototypes
_XimpUnregisterDestroyFilter(XIMS xims, XimpClient *client)
#else
_XimpUnregisterDestroyFilter(xims, client)
XIMS xims;
XimpClient *client;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    XWindowAttributes win_attr;

    if (client->filters & FLT_DESTROY) {
	XGetWindowAttributes(display, client->focus_window, &win_attr);
	XSelectInput(display, client->focus_window,
		     (win_attr.your_event_mask & ~StructureNotifyMask));
	_XUnregisterFilter(display, client->focus_window,
			   XimpDestroyFilter, xims);
	client->filters &= ~FLT_DESTROY;
    }
}

void
#if NeedFunctionPrototypes
_XimpUnregisterKeyPressFilter(XIMS xims, XimpClient *client)
#else
_XimpUnregisterKeyPressFilter(xims, client)
XIMS xims;
XimpClient *client;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    XWindowAttributes win_attr;

    if (client->filters & FLT_KEYPRESS) {
	XGetWindowAttributes(display, client->focus_window, &win_attr);
	XSelectInput(display, client->focus_window,
		     (win_attr.your_event_mask & ~KeyPressMask));
	_XUnregisterFilter(display, client->focus_window,
			   XimpKeyPressFilter, xims);
	client->filters &= ~FLT_KEYPRESS;
    }
}

void
#if NeedFunctionPrototypes
_XimpUnregisterKeyReleaseFilter(XIMS xims, XimpClient *client)
#else
_XimpUnregisterKeyReleaseFilter(xims, client)
XIMS xims;
XimpClient *client;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    XWindowAttributes win_attr;

    if (!IS_VERSION_40(client)) return;

    if (client->filters & FLT_KEYRELEASE) {
	XGetWindowAttributes(display, client->focus_window, &win_attr);
	XSelectInput(display, client->focus_window,
		     (win_attr.your_event_mask & ~KeyPressMask));
	_XUnregisterFilter(display, client->focus_window,
			   XimpKeyReleaseFilter, xims);
	client->filters &= ~FLT_KEYRELEASE;
    }
}
#endif	/* _XIMPFLT_C_ */
