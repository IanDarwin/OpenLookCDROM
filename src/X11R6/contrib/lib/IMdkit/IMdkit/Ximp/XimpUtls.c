/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpUtls.c	1.5 94/02/16 */
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
#ifndef _XIMPUTLS_C_
#define _XIMPUTLS_C_
#include <X11/Xatom.h>
#define	NEED_EVENTS
#include <X11/Xlibint.h>
#include "XimpData.h"

#if NeedFunctionPrototypes
extern Atom _XimpReplacePropPool(XIMPCore, char *, int);
extern void _XimpSendIMProtocol(XIMPCore, XimpClient *, int,
				unsigned long, unsigned long,
				unsigned long, unsigned long);
#else
extern Atom _XimpReplacePropPool();
extern void _XimpSendIMProtocol();
#endif

static void
#if NeedFunctionPrototypes
GetProtocolVersion(XIMPCore core, Window client_window,
		XimpProtocolVersion *version)
#else
GetProtocolVersion(core, client_window, version)
XIMPCore core;
Window client_window;
XimpProtocolVersion *version;
#endif
{
    Display *display = core->display;
    XIMPAtoms *atoms = (XIMPAtoms*)&core->atoms;
    Atom atom_ret = (Atom)0;
    int format_ret = 0;
    unsigned long nitems_ret = (unsigned long)0;
    unsigned long bytes_ret = (unsigned long)0;
    char *prop_ret = (char*)NULL;

    if (XGetWindowProperty(display, client_window,
			   atoms->version, 0, 10000L, True, XA_STRING,
			   &atom_ret, &format_ret, &nitems_ret, &bytes_ret,
			   (unsigned char**)&prop_ret)
	!= Success) return;
    if (atom_ret == None || atom_ret != XA_STRING) {
	/*
	 * Call XIMP_ERROR because of ambiguity of protocol
	 */
	return;
    }
    if (!strcmp(prop_ret, "XIMP.4.0")) {
	*version = XIMP_VERSION_40;
    } else if (!strcmp(prop_ret, "XIMP.3.5")) {
	*version = XIMP_VERSION_35;
    } else if (!strcmp(prop_ret, "XIMP_1.0")) {
	*version = XIMP_VERSION_40;
    } else if (!strcmp(prop_ret, "XIMP.3.4")) {
	*version = XIMP_VERSION_35;
    } else {
	/*
	 * Call XIMP_ERROR because of unknown protocol version
	 */
    }
    XFree(prop_ret);
}

Bool
#if NeedFunctionPrototypes
_XimpSetMatch(XimpClient *client,
	      CARD32 icid, CARD32 kcode, CARD32 state)
#else
_XimpSetMatch(client, icid, kcode, state)
XimpClient *client;
CARD32 icid, kcode, state;
#endif
{
    if (client->id == icid) {
	client->og_code = kcode;
	client->og_state = state;
	return True;
    }
    return False;
}

Bool
#if NeedFunctionPrototypes
_XimpProcessMatch(XimpClient *client,
		  CARD32 icid, CARD32 kcode, CARD32 state)
#else
_XimpProcessMatch(client, icid, kcode, state)
XimpClient *client;
CARD32 icid, kcode, state;
#endif
{
    if ((client->id == icid) &&
	(client->og_code == kcode) &&
	(client->og_state == state)) {
	return True;
    }
    return False;
}

XimpClient *
#if NeedFunctionPrototypes
_XimpFindClient(XIMPCore core, CARD32 icid, IMPProtocol *call_data)
#else
_XimpFindClient(core, icid, call_data)
XIMPCore core;
CARD32 icid;
IMPProtocol *call_data;
#endif
{
    XimpClient *client = core->clients;

    while (client != NULL) {
	if (client->id == icid)
	  break;
	client = client->next;
    }
    if (client == (XimpClient*)NULL) return (XimpClient*)NULL;

    /* restore client data into current IMPProtocol data */
    if (call_data != (IMPProtocol*)NULL) {
	memset(call_data, 0, sizeof(IMPProtocol));
	call_data->any.focus_win = client->focus_window;
	call_data->any.fwin_sel_mask = client->ev_masks;
	call_data->any.ximp_type_mask = client->ev_flow_type;
	call_data->any.client_win = client->client_window;
    }
    return (XimpClient*)client;
}

XimpClient *
#if NeedFunctionPrototypes
_XimpNewClient(XIMPCore core, XClientMessageEvent *ev)
#else
_XimpNewClient(core, ev)
XIMPCore core;
XClientMessageEvent *ev;
#endif
{
    static CARD16 id = 0;
    XimpClient *client;
    XimpProtocolVersion proto_version;

    GetProtocolVersion(core, (Window)ev->data.l[1], &proto_version);

    if (core->free_clients != NULL) {
	client = core->free_clients;
	core->free_clients = client->next;
    } else {
	client = (XimpClient *)malloc(sizeof(XimpClient));
    }
    client->id = ++id;
    client->client_window = ev->data.l[1];
    client->proto_version = proto_version;
    client->focus_window = client->client_window;
    client->is_conv_on = False;
    if (IS_VERSION_40(client)) {
	client->ev_flow_type = XIMP_FE_TYPE1; /* default */
	client->ev_masks = ev->data.l[4];
    } else {
	client->ev_flow_type = XIMP_FRONTEND; /* default */
	client->ev_masks = (long)0;
    }
    client->filters = FLT_NONE;
    client->next = core->clients;
    core->clients = client;

    return (XimpClient *)client;
}

void
#if NeedFunctionPrototypes
_XimpDeleteClient(XIMPCore core, CARD32 icid)
#else
_XimpDeleteClient(core, icid)
XIMPCore core;
CARD32 icid;
#endif
{
    XimpClient *target = _XimpFindClient(core, icid, (IMPProtocol*)NULL);
    XimpClient *ccp, *ccp0;

    for (ccp = core->clients, ccp0 = NULL;
	 ccp != NULL;
	 ccp0 = ccp, ccp = ccp->next) {
	if (ccp == target) {
	    if (ccp0 == NULL) {
		core->clients = ccp->next;
	    } else {
		ccp0->next = ccp->next;
	    }
	    /* put it back to free list */
	    target->next = core->free_clients;
	    core->free_clients = target;
	    return;
	}
    }
    return;
}

void
#if NeedFunctionPrototypes
_XimpSendByClientMessage(XIMPCore core, CARD32 icid,
			 char *ctext, int length)
#else
_XimpSendByClientMessage(core, icid, ctext, length)
XIMPCore core;
CARD32 icid;
char *ctext;
int length;
#endif
{
#define DATA_ROOM 11
    XClientMessageEvent	event;
    int msglen = DATA_ROOM;	/* default */
    char *ctptr = ctext;
    Ximp_CommitPropRec *msg = (Ximp_CommitPropRec*)&(event.data.l[0]);
    XimpClient *client = _XimpFindClient(core, icid, (IMPProtocol*)NULL);

    if (client == (XimpClient *)NULL) return;
    event.type = ClientMessage;
    event.display = core->display;
    event.message_type = core->ximp_request;
    event.window = client->focus_window;
    event.format = 8;

    while (length > 0) {
	memset(msg, 0, sizeof(Ximp_CommitPropRec));
	msg->icid = icid;
	msg->size = length;
	if (length < DATA_ROOM) msglen = length;

	memmove(msg->ctext, ctptr, msglen);
	XSendEvent(core->display,
		   client->focus_window,
		   False,
		   NoEventMask,
		   (XEvent*)&event);
	length -= DATA_ROOM;
	ctptr += DATA_ROOM;
    }
    XFlush(core->display);
    return;
#undef DATA_ROOM
}

void
#if NeedFunctionPrototypes
_XimpSendByProperty(XIMPCore core, CARD32 icid, char *ctext, int length)
#else
_XimpSendByProperty(core, icid, ctext, length)
XIMPCore core;
CARD32 icid;
char *ctext;
int length;
#endif
{
    XimpClient *client = _XimpFindClient(core, icid, (IMPProtocol*)NULL);

    if (client == (XimpClient *)NULL) return;
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_READPROP4 : XIMP_READPROP3),
			icid,
			_XimpReplacePropPool(core, ctext, length),
			0, 0);
    return;
}

void
#if NeedFunctionPrototypes
_XimpSendIMProtocol(XIMPCore core, XimpClient *client, int type,
		    unsigned long l1, unsigned long l2,
		    unsigned long l3, unsigned long l4)
#else
_XimpSendIMProtocol(core, client, type, l1, l2, l3, l4)
XIMPCore core;
XimpClient *client;
int type;
unsigned long l1, l2, l3, l4;
#endif
{
    Display *display = core->display;
    XEvent event;

    event.xclient.type = ClientMessage;
    event.xclient.window = client->focus_window;
    event.xclient.message_type = core->ximp_request;
    event.xclient.format = 32;
    event.xclient.data.l[0] = type;
    event.xclient.data.l[1] = l1;
    event.xclient.data.l[2] = l2;
    event.xclient.data.l[3] = l3;
    event.xclient.data.l[4] = l4;

    XSendEvent(display, event.xclient.window,
	       False, NoEventMask, &event);
    XFlush(display);
}

static Atom
#if NeedFunctionPrototypes
XimpAtomPool(Display *display, XPropertyEvent *evp)
#else
XimpAtomPool(display, evp)
Display *display;
XPropertyEvent *evp;
#endif
{
#define CALLBACKS_PROPNAME_LEN	64
#define MAX_CALLBACKS_PROP	100
    static int  atom_index = 0;
    static int  number = 0;
    static int  pool_size = 0;
    static char prop_name[CALLBACKS_PROPNAME_LEN] = {0};
    static Atom *pool = (Atom*)NULL;
    Atom   this_atom;
 
    if (pool_size == 0) {
        pool = (Atom *)malloc(sizeof(Atom) * MAX_CALLBACKS_PROP);
        pool_size = MAX_CALLBACKS_PROP;
    }
 
    if (evp == (XPropertyEvent *)NULL) {
        if (atom_index == 0) {
            memset(prop_name, 0, CALLBACKS_PROPNAME_LEN);
            sprintf(prop_name, "%s%X","_XIMP_NORMALPROP_", number++) ;
            this_atom = XInternAtom(display, prop_name, False);
        }
        else {
            this_atom = pool[--atom_index];
        }
    }
    else {
        if (evp->state != PropertyDelete)
                return (Atom)NULL;
        if (atom_index >= pool_size - 1) {
                pool = (Atom *)
                    realloc(pool, sizeof(Atom) * (pool_size + MAX_CALLBACKS_PROP));
                pool_size += MAX_CALLBACKS_PROP;
        }
        pool[atom_index++] = evp->atom;
                return (Atom)NULL;
    }
    return this_atom;
#undef CALLBACKS_PROPNAME_LEN
#undef MAX_CALLBACKS_PROP
}

Atom
#if NeedFunctionPrototypes
_XimpReplacePropPool(XIMPCore core, char *text, int len)
#else
_XimpReplacePropPool(core, text, len)
XIMPCore core;
char *text;
int len;
#endif
{
    Display *display = core->display;
    Window window = core->im_window;
    Atom atom;
 
    atom = XimpAtomPool(display, (XPropertyEvent *)NULL);
    XChangeProperty(display, window, atom, core->atoms.ctext_type, 8,
                    PropModeReplace, (unsigned char *)text, len);
    XFlush(display);
    return (atom);
}

#endif /* _XIMPUTLS_C_ */
