/******************************************************************

         Copyright 1993, 1994 by Hewlett-Packard Company

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Hewlett-Packard not
be used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
Hewlett-Packard Company makes no representations about the suitability
of this software for any purpose.
It is provided "as is" without express or implied warranty.

HEWLETT-PACKARD COMPANY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY SPECIAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

Author:
    Hidetoshi Tajima	Hewlett-Packard Company.
			(tajima@kobe.hp.com)
******************************************************************/
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "FrameMgr.h"
#include "IMdkit.h"
#include "Xi18n.h"
#include "Xi18nX.h"

#if NeedFunctionPrototypes
extern Xi18nClient *_Xi18nFindClient(Xi18n, CARD16);
extern Xi18nClient *_Xi18nNewClient(Xi18n);
extern void _Xi18nDeleteClient(Xi18n, CARD16);
static Bool WaitXConnectMessage(Display*, Window,
				XEvent*, XPointer);
static Bool WaitXIMProtocol(Display*, Window, XEvent*, XPointer);
#else
extern Xi18nClient *_Xi18nFindClient();
extern Xi18nClient *_Xi18nNewClient();
extern void _Xi18nDeleteClient();
static Bool WaitXConnectMessage();
static Bool WaitXIMProtocol();
#endif

static XClient *
#if NeedFunctionPrototypes
NewXClient(Xi18n i18n_core, Window new_client)
#else
NewXClient(i18n_core, new_client)
Xi18n i18n_core;
Window new_client;
#endif
{
    Display *dpy = i18n_core->address.dpy;
    Xi18nClient *client = _Xi18nNewClient(i18n_core);
    XClient *x_client;

    x_client = (XClient *)malloc(sizeof(XClient));
    x_client->client_win = new_client;
    x_client->accept_win = XCreateSimpleWindow(dpy,
					       DefaultRootWindow(dpy),
					       0, 0, 1, 1, 1, 0, 0);
    client->trans_rec = x_client;
    return ((XClient *)x_client);
}

static unsigned char *
#if NeedFunctionPrototypes
ReadXIMMessage(XIMS ims, XClientMessageEvent *ev,
		int *connect_id)
#else
ReadXIMMessage(ims, ev, connect_id)
XIMS ims;
XClientMessageEvent *ev;
int *connect_id;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Xi18nClient *client = i18n_core->address.clients;
    XClient *x_client;

    FrameMgr fm;
    extern XimFrameRec packet_header_fr[];
    register int total_size;
    unsigned char *p = NULL;
    unsigned char *p1;

    while (client != NULL) {
	x_client = (XClient *)client->trans_rec;
	if (x_client->accept_win == ev->window) {
	    *connect_id = client->connect_id;
	    break;
	}
	client = client->next;
    }

    if (ev->format == 8) {	/* ClientMessage only */
	XimProtoHdr *hdr = (XimProtoHdr *)ev->data.b;
	unsigned char *rec = (unsigned char *)(hdr + 1);
	CARD8 major_opcode, minor_opcode;
	CARD16 length;
	extern int _Xi18nNeedSwap(
#if NeedFunctionPrototypes
	     Xi18n, CARD16
#endif
				  );

	if (client->byte_order == '?') {
	    if (hdr->major_opcode == XIM_CONNECT)
	      client->byte_order = (CARD8)rec[0];
	    else
	      return (unsigned char *)NULL;	/* can do nothing */
	}
	/* create FrameMgr */
	fm = FrameMgrInit(packet_header_fr, (char *)hdr,
			  _Xi18nNeedSwap(i18n_core, *connect_id));

	total_size = FrameMgrGetTotalSize(fm);
	/* get data */
	FrameMgrGetToken(fm, major_opcode);
	FrameMgrGetToken(fm, minor_opcode);
	FrameMgrGetToken(fm, length);

	/* free FrameMgr */
	FrameMgrFree(fm);

	if ((p = (unsigned char *)malloc(total_size + length * 4)) == NULL)
	  return (unsigned char *)NULL;
	p1 = p;
	memmove(p1, &major_opcode, sizeof(CARD8)); p1 += sizeof(CARD8);
	memmove(p1, &minor_opcode, sizeof(CARD8)); p1 += sizeof(CARD8);
	memmove(p1, &length, sizeof(CARD16)); p1 += sizeof(CARD16);
	memmove(p1, rec, length * 4);
    } else if (ev->format == 32) { /* ClientMessage and WindowProperty */
	unsigned long length = (unsigned long)ev->data.l[0];
	Atom atom = (Atom)ev->data.l[1];
	int	     return_code;
	Atom	     actual_type_ret;
	int	     actual_format_ret;
	unsigned long    bytes_after_ret;
	unsigned char    *prop;
	unsigned long    nitems;

	return_code = XGetWindowProperty(i18n_core->address.dpy,
					 x_client->accept_win, atom,
					 0L, length * 4,
					 True, AnyPropertyType,
					 &actual_type_ret, &actual_format_ret,
					 &nitems, &bytes_after_ret, &prop);
	if (return_code != Success
	    || actual_format_ret == 0 || nitems == 0) {
	    if (return_code == Success)
	      XFree(prop);
	    return (unsigned char *)NULL;
	}
	if ((p = (unsigned char *)malloc(length * 4)) == NULL)
	  return (unsigned char *)NULL;
	memmove(p, prop, length * 4);
	XFree(prop);
    }
    return (unsigned char *)p;
}

static void
#if NeedFunctionPrototypes
ReadXConnectMessage(XIMS ims, XClientMessageEvent *ev)
#else
ReadXConnectMessage(ims, ev)
XIMS ims;
XClientMessageEvent *ev;
#endif
{
    Xi18n i18n_core = ims->protocol;
    XSpecRec *spec = (XSpecRec *)i18n_core->address.connect_addr;
    XEvent event;
    Display *dpy = i18n_core->address.dpy;
    Window new_client = ev->data.l[0];
    CARD32 major_version = ev->data.l[1];
    CARD32 minor_version = ev->data.l[2];
    XClient *x_client = NewXClient(i18n_core, new_client);

    if (ev->window != i18n_core->address.im_window)
      return;			/* incorrect connection request */

    if (major_version != 0 || minor_version != 0) {
	major_version = minor_version = 0;
	/* Only supporting only-CM & Property-with-CM method */
    }
    _XRegisterFilterByType(dpy, x_client->accept_win,
			   ClientMessage, ClientMessage,
			   WaitXIMProtocol, ims);
    event.xclient.type = ClientMessage;
    event.xclient.display = dpy;
    event.xclient.window = new_client;
    event.xclient.message_type = spec->connect_request;
    event.xclient.format       = 32;
    event.xclient.data.l[0]    = x_client->accept_win;
    event.xclient.data.l[1]    = major_version;
    event.xclient.data.l[2]    = minor_version;
    event.xclient.data.l[3]    = XCM_DATA_LIMIT;

    XSendEvent(dpy, new_client,
	       False, NoEventMask, &event);
    XFlush(dpy);
}

static Bool
#if NeedFunctionPrototypes
Xi18nXBegin(XIMS ims)
#else
Xi18nXBegin(ims)
XIMS ims;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Display *dpy = i18n_core->address.dpy;
    XSpecRec *spec = (XSpecRec *)i18n_core->address.connect_addr;

    spec->xim_request = XInternAtom(i18n_core->address.dpy,
				    _XIM_PROTOCOL, False);
    spec->connect_request = XInternAtom(i18n_core->address.dpy,
					_XIM_XCONNECT, False);

    _XRegisterFilterByType(dpy, i18n_core->address.im_window,
			   ClientMessage, ClientMessage,
			   WaitXConnectMessage, ims);
    return True;
}

static Bool
#if NeedFunctionPrototypes
Xi18nXEnd(XIMS ims)
#else
Xi18nXEnd(ims)
XIMS ims;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Display *dpy = i18n_core->address.dpy;

    _XUnregisterFilter(dpy, i18n_core->address.im_window,
		       WaitXConnectMessage, ims);
    return True;
}

static char *
#if NeedFunctionPrototypes
MakeNewAtom(CARD16 connect_id, char *atomName)
#else
MakeNewAtom(connect_id, atomName)
CARD16 connect_id;
char *atomName;
#endif
{
    static int sequence = 0;
    sprintf(atomName, "_server%d_%d", connect_id,
	    (sequence > 20 ? (sequence = 0) : sequence++));
    return atomName;
}

static Bool
#if NeedFunctionPrototypes
Xi18nXSend(XIMS ims, CARD16 connect_id,
	   unsigned char *reply, long length)
#else
Xi18nXSend(ims, connect_id, reply, length)
XIMS ims;
CARD16 connect_id;
unsigned char *reply;
long length;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Xi18nClient *client = _Xi18nFindClient(i18n_core, connect_id);
    XSpecRec *spec = (XSpecRec *)i18n_core->address.connect_addr;
    XClient *x_client = (XClient *)client->trans_rec;
    XEvent event;
    
    event.type = ClientMessage;
    event.xclient.window = x_client->client_win;
    event.xclient.message_type = spec->xim_request;

    if (length > XCM_DATA_LIMIT) {
	Atom atom;
	char atomName[16];
	Atom actual_type_ret;
	int actual_format_ret;
	int return_code;
	unsigned long nitems_ret;
	unsigned long bytes_after_ret;
	unsigned char *win_data;

	event.xclient.format = 32;
	atom = XInternAtom(i18n_core->address.dpy,
			   MakeNewAtom(connect_id, atomName), False);
	return_code = XGetWindowProperty(i18n_core->address.dpy,
				 x_client->client_win, atom, 0L, 10000L,
				 False, XA_STRING, &actual_type_ret,
				 &actual_format_ret, &nitems_ret,
				 &bytes_after_ret, &win_data);
	if (return_code != Success)
	  return False;
	if (win_data)
	  XFree((char *)win_data);
	XChangeProperty(i18n_core->address.dpy,
			x_client->client_win, atom, XA_STRING, 8,
			PropModeAppend, (unsigned char *)reply, length);
	event.xclient.data.l[0] = length;
	event.xclient.data.l[1] = atom;
    }
    else {
	unsigned char buffer[XCM_DATA_LIMIT];
	int i;

	event.xclient.format = 8;

	/* Clear unused field with NULL */
	memmove(buffer, reply, length);
	for (i = length; i < XCM_DATA_LIMIT; i++) {
	    buffer[i] = (char)0;
	}
	length = XCM_DATA_LIMIT;
	memmove(event.xclient.data.b, buffer, length);
    }
    XSendEvent(i18n_core->address.dpy,
	       x_client->client_win, False, NoEventMask, &event);
    XFlush(i18n_core->address.dpy);
    return True;
}

static Bool
#if NeedFunctionPrototypes
CheckCMEvent(Display *display, XEvent *event, XPointer xi18n_core)
#else
CheckCMEvent(display, event, xi18n_core)
Display	*display;
XEvent	*event;
XPointer xi18n_core;
#endif
{
    Xi18n i18n_core = (Xi18n)((void *)xi18n_core);
    XSpecRec *spec = (XSpecRec *)i18n_core->address.connect_addr;

    if ((event->type == ClientMessage)
	&& (event->xclient.message_type == spec->xim_request))
      return True;
    return False;

}

static Bool
#if NeedFunctionPrototypes
Xi18nXWait(XIMS ims, CARD16 connect_id,
	   CARD8 major_opcode, CARD8 minor_opcode)
#else
Xi18nXWait(ims, connect_id, major_opcode, minor_opcode)
XIMS ims;
CARD16 connect_id;
CARD8 major_opcode;
CARD8 minor_opcode;
#endif
{
    Xi18n i18n_core = ims->protocol;
    XEvent event;
    Xi18nClient *client = _Xi18nFindClient(i18n_core, connect_id);
    XClient *x_client = (XClient *)client->trans_rec;

    for (;;) {
	unsigned char *packet;
	XimProtoHdr *hdr;
	int connect_id_ret;

	XIfEvent(i18n_core->address.dpy, &event,
		 CheckCMEvent, (XPointer)i18n_core);
	if (event.xclient.window == x_client->accept_win) {

	    packet = ReadXIMMessage(ims,
				    (XClientMessageEvent *)&event,
				    &connect_id_ret);
	    hdr = (XimProtoHdr *)packet;

	    if ((hdr->major_opcode == major_opcode) &&
		(hdr->minor_opcode == minor_opcode))
	      return True;
	    else if (hdr->major_opcode == XIM_ERROR)
	      return False;
	}
    }
}

static Bool
#if NeedFunctionPrototypes
Xi18nXDisconnect(XIMS ims, CARD16 connect_id)
#else
Xi18nXDisconnect(ims, connect_id)
XIMS ims;
CARD16 connect_id;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Display *dpy = i18n_core->address.dpy;
    Xi18nClient *client = _Xi18nFindClient(i18n_core, connect_id);
    XClient *x_client = (XClient *)client->trans_rec;

    XDestroyWindow(dpy, x_client->accept_win);
    _XUnregisterFilter(dpy, x_client->accept_win,
		       WaitXIMProtocol, ims);
    XFree(x_client);
    _Xi18nDeleteClient(i18n_core, connect_id);
    return True;
}

Bool
#if NeedFunctionPrototypes
_Xi18nCheckXAddress(Xi18n i18n_core, TransportSW *transSW,
		    char *address)
#else
_Xi18nCheckXAddress(i18n_core, transSW, address)
Xi18n i18n_core;
TransportSW *transSW;
char *address;
#endif
{
    XSpecRec *spec;

    if(!(spec = (XSpecRec *)malloc(sizeof(XSpecRec))))
      return False ;

    i18n_core->address.connect_addr = (XSpecRec *)spec;
    i18n_core->methods.begin = Xi18nXBegin;
    i18n_core->methods.end  = Xi18nXEnd;
    i18n_core->methods.send = Xi18nXSend;
    i18n_core->methods.wait = Xi18nXWait;
    i18n_core->methods.disconnect = Xi18nXDisconnect;
    return True;
}

static Bool
#if NeedFunctionPrototypes
WaitXConnectMessage(Display *dpy, Window win,
		    XEvent *ev, XPointer client_data)
#else
WaitXConnectMessage(dpy, win, ev, client_data)
Display *dpy;
Window win;
XEvent *ev;
XPointer client_data;
#endif
{
    XIMS ims = (XIMS)client_data;
    Xi18n i18n_core = ims->protocol;
    XSpecRec *spec = (XSpecRec *)i18n_core->address.connect_addr;

    if (((XClientMessageEvent *)ev)->message_type ==
	spec->connect_request) {
	ReadXConnectMessage(ims,
			    (XClientMessageEvent *)ev);
	return True;
    }
    return False;
}

static Bool
#if NeedFunctionPrototypes
WaitXIMProtocol(Display *dpy, Window win,
		XEvent *ev, XPointer client_data)
#else
WaitXIMProtocol(dpy, win, ev, client_data)
Display *dpy;
Window win;
XEvent *ev;
XPointer client_data;
#endif
{
    XIMS ims = (XIMS)client_data;
    Xi18n i18n_core = ims->protocol;
    XSpecRec *spec = (XSpecRec *)i18n_core->address.connect_addr;
    extern void _Xi18nMessageHandler(
#if NeedFunctionPrototypes
     XIMS, CARD16, unsigned char *, Bool *
#endif
				     );
    Bool delete = True;
    unsigned char *packet;
    int connect_id;

    if (((XClientMessageEvent *)ev)->message_type ==
	spec->xim_request) {
	packet = ReadXIMMessage(ims,
				(XClientMessageEvent *)ev,
				&connect_id);
	_Xi18nMessageHandler(ims, connect_id, packet, &delete);
	if (delete == True)
	  XFree(packet);
	return True;
    }
    return False;
}
