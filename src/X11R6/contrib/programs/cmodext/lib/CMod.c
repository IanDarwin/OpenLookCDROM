/*

Copyright (c) 1990 - 1994  FUJITSU LIMITED
Copyright (c) 1990 - 1991  PFU Limited

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED AND PFU LIMITED BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
PFU Limited shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and PFU Limited.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp

*/

#ident "@(#)CMod.c	1.3 4/1/91"

#define NEED_EVENTS
#define NEED_REPLIES
#include <X11/Xlibint.h>
#include <X11/extensions/Xext.h>
#include "extutil.h"
#include "cmodstr.h"

static XExtensionInfo *cmod_info;
static /* const */ char *cmod_extension_name = CONTROLMODIFIERS_PROTOCOL_NAME;

#define ControlModifiersCheckExtension(dpy, i, val) \
  XextCheckExtension (dpy, i, cmod_extension_name, val)
#define ControlModifiersSimpleCheckExtension(dpy, i) \
  XextSimpleCheckExtension (dpy, i, cmod_extension_name)

/*
 * find_display - locate the display info block
 */
static int close_display();
static char *error_string();
static Bool wire_to_event();
static Status event_to_wire();
static /* const */ XExtensionHooks cmod_extension_hooks = {
    NULL,				/* create_gc */
    NULL,				/* copy_gc */
    NULL,				/* flush_gc */
    NULL,				/* free_gc */
    NULL,				/* create_font */
    NULL,				/* free_font */
    close_display,			/* close_display */
    wire_to_event,			/* wire_to_event */
    event_to_wire,			/* event_to_wire */
    NULL,				/* error */
    error_string,			/* error_string */
};

static /* const */ char *cmod_error_list[] = {
    "ControlModifiersError",				/* ControlModifiersError */
};

static XEXT_GENERATE_FIND_DISPLAY (find_display, cmod_info,
				   cmod_extension_name, 
				   &cmod_extension_hooks, 
				   ControlModifiersNumberEvents, NULL)

static XEXT_GENERATE_CLOSE_DISPLAY (close_display, cmod_info)

static XEXT_GENERATE_ERROR_STRING (error_string, cmod_extension_name,
				   ControlModifiersNumberErrors, 
				   cmod_error_list)

/*
 * wire_to_event - convert a wire event in network format to a C 
 * event structure
 */
static Bool
wire_to_event (dpy, libevent, netevent)
    Display *dpy;
    XEvent *libevent;
    xEvent *netevent;
{
    XExtDisplayInfo *info = find_display (dpy);

    ControlModifiersCheckExtension (dpy, info, False);

    switch ((netevent->u.u.type & 0x7f) - info->codes->first_event) {
      case ControlModifiersNotify:
	{
	    ControlModifiersEvent *ev;
	    xControlModifiersEvent *event;
    
    	    ev = (ControlModifiersEvent *) libevent;
	    event = (xControlModifiersEvent *) netevent;
    	    ev->type = event->type & 0x7f;
    	    ev->serial = _XSetLastRequestRead(dpy,(xGenericReply *) netevent);
    	    ev->send_event = ((event->type & 0x80) != 0);
    	    ev->display = dpy;
	    ev->change = event->change;
	    ev->state = event->state;
    	    return True;
	}
    }
    return False;
}


/*
 * event_to_wire - convert a C event structure to a wire event in
 * network format
 */
static Status
event_to_wire (dpy, libevent, netevent)
    Display *dpy;
    XEvent  *libevent;
    xEvent  *netevent;
{
    XExtDisplayInfo *info = find_display (dpy);

    ControlModifiersCheckExtension (dpy, info, 0);

    switch ((libevent->type & 0x7f) - info->codes->first_event) {
      case ControlModifiersNotify:
	{
	    ControlModifiersEvent *ev;
	    xControlModifiersEvent *event;
    
    	    ev = (ControlModifiersEvent *) libevent;
	    event = (xControlModifiersEvent *) netevent;
    	    event->type = ev->type;
    	    event->sequenceNumber = (ev->serial & 0xffff);
	    event->change = ev->change;
	    event->state = ev->state;
    	    return 1;
	}
    }
    return 0;
}

Bool
ControlModifiersQueryExtension (dpy, event_base_return, error_base_return)
    Display *dpy;
    int *event_base_return, *error_base_return;
{
    XExtDisplayInfo *info = find_display (dpy);
    
    if (XextHasExtension (info)) {
	*event_base_return = info->codes->first_event;
	*error_base_return = info->codes->first_error;
	return True;
    } else {
	return False;
    }
}

Status
ControlModifiersGetVersion (dpy, major_version_return, minor_version_return,
		      report_mask_return, change_mask_return)
Display *dpy;
int *major_version_return, *minor_version_return;
int *report_mask_return, *change_mask_return;
{
    XExtDisplayInfo *info = find_display(dpy);
    xControlModifiersGetVersionReply rep;
    register xControlModifiersGetVersionReq *req;

    ControlModifiersCheckExtension(dpy, info, 0);

    LockDisplay (dpy);
    ControlModifiersGetReq(ControlModifiersGetVersion, req, info);
    if (!_XReply(dpy, (xReply *)&rep, 0, xTrue)) {
	UnlockDisplay (dpy);
	SyncHandle();
	return(0);
    }
    *major_version_return = rep.majorVersion;
    *minor_version_return = rep.minorVersion;
    *report_mask_return = rep.validReport;
    *change_mask_return = rep.validChange;
    UnlockDisplay (dpy);
    SyncHandle();
    return(1);
}

void
ControlModifiersSetMask(dpy, win, mask)
Display *dpy;
Window win;
CARD32 mask;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xControlModifiersSetMaskReq *req;

    ControlModifiersSimpleCheckExtension(dpy, info);

    LockDisplay(dpy);
    ControlModifiersGetReq (ControlModifiersSetMask, req, info);
    req->reqType = info->codes->major_opcode;
    req->cmodReqType = X_ControlModifiersSetMask;
    req->window = win;
    req->mask = mask;
    UnlockDisplay(dpy);
    SyncHandle();
}

Status
ControlModifiersGetMask(dpy, win, mask_return)
Display *dpy;
Window win;
CARD32	*mask_return;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xControlModifiersGetMaskReq *req;
    xControlModifiersGetMaskReply rep;

    ControlModifiersCheckExtension(dpy, info, 0);

    LockDisplay(dpy);
    ControlModifiersGetReq(ControlModifiersGetMask, req, info);
    req->window = win;
    if (!_XReply(dpy, (xReply *)&rep,
		 (SIZEOF(xControlModifiersGetMaskReply) -
		  SIZEOF(xReply)) >> 2, xTrue)) {
	UnlockDisplay(dpy);
	SyncHandle();
	return(0);
    }
    *mask_return = rep.mask;
    UnlockDisplay(dpy);
    SyncHandle();
    return 1;
}

Status
ControlModifiersGetState(dpy, state_return)
Display *dpy;
CARD32	*state_return;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xControlModifiersGetStateReq *req;
    xControlModifiersGetStateReply rep;

    ControlModifiersCheckExtension(dpy, info, 0);

    LockDisplay(dpy);
    ControlModifiersGetReq(ControlModifiersGetState, req, info);
    if (!_XReply(dpy, (xReply *)&rep,
		 (SIZEOF(xControlModifiersGetStateReply) -
		  SIZEOF(xReply)) >> 2, xTrue)) {
	UnlockDisplay(dpy);
	SyncHandle();
	return(0);
    }
    *state_return = rep.state;
    UnlockDisplay(dpy);
    SyncHandle();
    return 1;
}

void
ControlModifiersChangeState(dpy, change, state)
Display *dpy;
CARD32 change B32;
CARD32 state B32;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xControlModifiersChangeStateReq *req;

    ControlModifiersSimpleCheckExtension(dpy, info);

    LockDisplay(dpy);
    ControlModifiersGetReq (ControlModifiersChangeState, req, info);
    req->reqType = info->codes->major_opcode;
    req->cmodReqType = X_ControlModifiersChangeState;
    req->change = change;
    req->state = state;
    UnlockDisplay(dpy);
    SyncHandle();
}

void
ControlModifiersGrab(dpy)
Display *dpy;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xControlModifiersGrabReq *req;

    ControlModifiersSimpleCheckExtension(dpy, info);

    LockDisplay(dpy);
    ControlModifiersGetReq (ControlModifiersGrab, req, info);
    req->reqType = info->codes->major_opcode;
    req->cmodReqType = X_ControlModifiersGrab;
    UnlockDisplay(dpy);
    SyncHandle();
}

void
ControlModifiersUngrab(dpy)
Display *dpy;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xControlModifiersUngrabReq *req;

    ControlModifiersSimpleCheckExtension(dpy, info);

    LockDisplay(dpy);
    ControlModifiersGetReq (ControlModifiersUngrab, req, info);
    req->reqType = info->codes->major_opcode;
    req->cmodReqType = X_ControlModifiersUngrab;
    UnlockDisplay(dpy);
    SyncHandle();
}
