#ifndef lint
static char *rcsid = "$Id: imxport.c,v 1.10 1994/06/03 04:51:27 ishisone Exp $";
#endif
/*
 * Copyright (c) 1994  Software Research Associates, Inc.
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

#include "im.h"

#include "MyDispatch.h"

#include <X11/Xatom.h>

#if (defined(IM_UNIX_TRANSPORT) || defined(IM_TCP_TRANSPORT))
#include <sys/types.h>
#include <sys/socket.h>
#endif

#ifdef IM_UNIX_TRANSPORT
#include <sys/un.h>
#endif

#ifdef IM_TCP_TRANSPORT
#include <netinet/in.h> 
#endif

extern int errno;

/*
 * X transport version number
 *   This implementation uses:
 *	+ ClientMessage (both single and multiple)
 *	+ Property with notification by ClientMessage
 *   So the major version is 0, minor version is 2.
 *
 * X transport dividing size
 *   This size is the threshold between ClientMessage transfer
 *   and Property transfer.  If the data to be sent is small,
 *   transfer via ClientMessage is faster.  But since single
 *   ClientMessage can transfer only 20bytes, there must be a
 *   certain size above which the transfer via Property is
 *   faster.
 */
#define ServerMajorTransportVersion	0
#define ServerMinorTransportVersion	2
#define XTransportDividingSize		(20 * 5)


static int dummyDispatcher _Pt_((IMConnection *conn));
static void dumpBuf _Pt_((IMBuffer *ibp, char *title));
static Window communicationWindow _Pt_((Widget w));
static IMConnection *newConnection _Pt_((Widget protocol));

#if defined(IM_TCP_TRANSPORT) || defined(IM_UNIX_TRANSPORT)
static void socketinput _Pt_((XtPointer cldata, int *fdp, XtInputId *idp));
static int socketFlush _Pt_((IMConnection *conn));
static void socketShutdown _Pt_((IMConnection *conn));

static IMTransportOps SocketTransportOps = {
    socketFlush,
    socketShutdown,
};
#endif /* IM_TCP_TRANSPORT || IM_UNIX_TRANSPORT */

#ifdef IM_X_TRANSPORT
static void xinput _Pt_((XEvent *ev, XtPointer cldata));
static void xdestroy _Pt_((XEvent *ev, XtPointer cldata));
static int xBrokenPipe _Pt_((Display *dpy, XErrorEvent *eev,
			     XPointer client_data));
static int xFlush _Pt_((IMConnection *conn));
static void xShutdown _Pt_((IMConnection *conn));

static IMTransportOps XTransportOps = {
    xFlush,
    xShutdown,
};
#endif /* IM_X_TRANSPORT */


/*- dummyDispatcher: dummy dispatcher routine -*/
/* ARGSUSED */
static int
dummyDispatcher(conn)
IMConnection *conn;
{
#ifdef DEBUG
    printf("dummyDispatcher -- this function should not be called!\n");
#endif
    return 0;	/* for lint */
}

/*- dumpBuf: dump input/output buffer (for debug) -*/
static void
dumpBuf(ibp, title)
IMBuffer *ibp;
char *title;
{
    int len = IMBUFLEN(ibp);
    unsigned char *data = (unsigned char *)IMBUFDATA(ibp);
    int i;

    printf("%s (%d bytes)", title, len);
    for (i = 0; i < len; i++) {
	if (i % 16 == 0) printf("\n\t");
	printf("%02x ", *data++);
    }
    printf("\n");
}

/*- communicationWindow: create a window for communication with client -*/
static Window
communicationWindow(w)
Widget w;
{
    return XCreateSimpleWindow(XtDisplay(w), XtWindow(w), 0, 0, 1, 1, 0, 0, 0);
}

/*- newConnection: allocate IMConnection structure and initialize -*/
static IMConnection *
newConnection(protocol)
Widget protocol;
{
    IMConnection *conn;
    static int connection_serial_number = 0;

    conn = (IMConnection *)XtMalloc(sizeof(IMConnection));

    conn->serial = ++connection_serial_number;		/* start from 1 */
    conn->dispatcher = dummyDispatcher;

    IMBufInit(&conn->in_buf);
    IMBufInit(&conn->out_buf);

    conn->byte_order = ORDER_UNKNOWN;
    conn->im_list = NULL;
    conn->proto_widget = protocol;

    conn->schedule = 0;
    conn->queue_next = NULL;

    conn->next = NULL;

    return conn;
}

#if defined(IM_TCP_TRANSPORT) || defined(IM_UNIX_TRANSPORT)
/*- socketinput: handler for input from TCP/Unix socket -*/
/* ARGSUSED */
static void
socketinput(cldata, fdp, idp)
XtPointer cldata;
int *fdp;
XtInputId *idp;
{
    IMConnection *conn = (IMConnection *)cldata;
    int fd = conn->transport.priv.sock.fd;
    char buf[4096];
    int n;
    int cond;

    if ((n = read(fd, buf, sizeof(buf))) < 0) {
	cond = TRANSPORT_ERROR;
    } else if (n == 0) {
	cond = TRANSPORT_EOF;
    } else {
	IMBufAdd(&conn->in_buf, buf, n);
	cond = TRANSPORT_OK;
    }

    if (DDEBUG_CONDITION(100)) dumpBuf(IM_INBUF(conn), "** input buffer");

    IMDispatch(conn, cond);
}

/*- socketFlush: output to socket -*/
static int
socketFlush(conn)
IMConnection *conn;
{
    int fd = conn->transport.priv.sock.fd;
    IMBuffer *ibp = IM_OUTBUF(conn);
    int n;

    if ((n = write(fd, ibp->buf, IMBUFLEN(ibp))) < 0) {
	return TRANSPORT_ERROR;
    } else {
	IMBufDiscard(ibp, n);
	return (IMBUFLEN(ibp) == 0) ? TRANSPORT_OK : TRANSPORT_PARTIAL;
    }
}

/*- socketShutdown: close socket and stop input callback associate with it -*/
static void
socketShutdown(conn)
IMConnection *conn;
{
    int fd = conn->transport.priv.sock.fd;

    (void)shutdown(fd, 2);
    (void)close(fd);
    XtRemoveInput(conn->transport.priv.sock.id);
}
#endif /* IM_TCP_TRANSPORT || IM_UNIX_TRANSPORT */

#ifdef IM_X_TRANSPORT
/*- xinput: handler for input via X inter-client communication -*/
static void
xinput(ev, cldata)
XEvent *ev;
XtPointer cldata;
{
    XClientMessageEvent *event = (XClientMessageEvent *)ev;
    IMConnection *conn = (IMConnection *)cldata;
    int cond;
    Atom msg_type;

    TRACE(("imlib:xinput()\n"));

    if (event->type != ClientMessage ||
	event->window != conn->transport.priv.x.server) {
	return;
    }

    msg_type = event->message_type;

    if (event->format == 32) {
	/*
	 * indirect reference -- data resides in a property,
	 * whose name is stored in the event.
	 */
	Atom propatom = event->data.l[1];
	long offset = 0;
	unsigned long remain;

	if (msg_type != IMProtocolAtom(conn->proto_widget)) return;

	do {
	    Atom actualtype;
	    int actualformat;
	    unsigned long nitems;
	    char *data = NULL;

	    XGetWindowProperty(event->display, event->window, propatom,
			       offset, 1000, True, AnyPropertyType,
			       &actualtype, &actualformat, &nitems,
			       &remain, (unsigned char **)&data);
	    if (data == NULL) return;
	    if (actualformat != 8) {
		cond = TRANSPORT_ERROR;
	    } else {
		IMBufAdd(&conn->in_buf, data, (int)nitems);
		offset += nitems;
		cond = TRANSPORT_OK;
	    }
	    XFree(data);
	} while (remain > 0);
    } else if (event->format == 8) {
	if (msg_type != IMProtocolAtom(conn->proto_widget) &&
	    msg_type != IMMoreDataAtom(conn->proto_widget)) {
	    return;
	}
	IMBufAdd(&conn->in_buf, event->data.b, 20);
	cond = TRANSPORT_OK;
    } else {
	return;
    }

    if (DDEBUG_CONDITION(100)) dumpBuf(IM_INBUF(conn), "** input buffer");

    IMDispatch(conn, cond);
}

/*- xdestroy: handler for client comm. window destruction -*/
static void
xdestroy(ev, cldata)
XEvent *ev;
XtPointer cldata;
{
    XDestroyWindowEvent *event = (XDestroyWindowEvent *)ev;
    IMConnection *conn = (IMConnection *)cldata;

    TRACE(("imlib:xdestroy()\n"));

    if (event->type != DestroyNotify ||
	event->window != conn->transport.priv.x.client) {
	return;
    }
    /*
     * Call IMDispatch with TRANSPORT_ERROR, in order to
     * shutdown connection and process queued operations.
     */
    IMDispatch(conn, TRANSPORT_ERROR);
}

/*- xBrokenPipe: asyncronous BadWindow error handler -*/
/* ARGSUSED */
static int
xBrokenPipe(dpy, eev, client_data)
Display *dpy;
XErrorEvent *eev;
XPointer client_data;
{
    TRACE(("xBrokenPipe()\n"));

    if (eev->error_code == BadWindow) {
	/*
	 * Search for the connection using window that caused the error.
	 * Note that we cannot pass the connection via client_data,
	 * Since the connection might be already destroyed by a previous
	 * error.
	 */
	Window bad_win = (Window)eev->resourceid;
	IMConnection *conn;

	conn = IMConnectionList((Widget)client_data);
	while (conn != NULL) {
	    if (bad_win == conn->transport.priv.x.client) {
		DPRINT(("BadWindow on connection #%d\n", conn->serial));
		IMDispatch(conn, TRANSPORT_ERROR);
		break;
	    }
	    conn = conn->next;
	}
	return 0;
    } else {
	return 1;
    }
}

/*- xFlush: output via X inter-client communication mechanism -*/
static int
xFlush(conn)
IMConnection *conn;
{
    IMBuffer *ibp = IM_OUTBUF(conn);
    XClientMessageEvent repl;
    Widget w = conn->proto_widget;
    Window client_win = conn->transport.priv.x.client;
    Display *dpy = XtDisplay(w);
    int length;
    XAEHandle handle;

    if ((length = IMBUFLEN(ibp)) == 0) return TRANSPORT_OK;

    repl.type = ClientMessage;
    repl.window = client_win;

    handle = XAESet(dpy, xBrokenPipe, (void (*)())NULL, (XPointer)w);

    if (IMBUFLEN(ibp) < XTransportDividingSize) {
	char *data = IMBUFDATA(ibp);

	repl.format = 8;
	repl.message_type = IMMoreDataAtom(w);
	while (length > 20) {
	    bcopy(data, repl.data.b, 20);
	    XSendEvent(dpy, client_win, False, NoEventMask, (XEvent *)&repl);
	    data += 20;
	    length -= 20;
	}
	repl.message_type = IMProtocolAtom(w);
	bzero(repl.data.b, 20);
	bcopy(data, repl.data.b, length);
	XSendEvent(dpy, client_win, False, NoEventMask, (XEvent *)&repl);
    } else {
	repl.format = 32;
	repl.message_type = IMProtocolAtom(w);
	repl.data.l[0] = length;
	repl.data.l[1] = IMKi2CommAtom(w);
	XChangeProperty(dpy, client_win, IMKi2CommAtom(w), XA_STRING,
			8, PropModeAppend,
			(unsigned char *)IMBUFDATA(ibp), IMBUFLEN(ibp));
	XSendEvent(dpy, client_win, False, NoEventMask, (XEvent *)&repl);
    }

    XAEUnset(handle);

    IMBufClear(ibp);
    return TRANSPORT_OK;
}

/*- xShutdown: close communication channel by destroying comm. window -*/
static void
xShutdown(conn)
IMConnection *conn;
{
    Display *dpy = XtDisplay(conn->proto_widget);
    MyRemoveAllEventHandler(dpy, conn->transport.priv.x.client);
    MyRemoveAllEventHandler(dpy, conn->transport.priv.x.server);
    XDestroyWindow(dpy, conn->transport.priv.x.server);
}
#endif /* IM_X_TRANSPORT */


/*
 * Public functions
 */

#ifdef IM_TCP_TRANSPORT
int
IMCreateTCPService(portp)
int *portp;
{
    struct sockaddr_in addr;
    int optval = 1;
    int sock;

    TRACE(("IMCreateTCPService(port=%d)\n", *portp));

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
	DPRINT(("socket(PF_INET) failed with %d\n", errno));
	return -1;
    }

#ifdef SO_REUSEADDR
    (void)setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
		     (char *)&optval, sizeof(optval));
#endif /* SO_REUSEADDR */

    bzero((char *)&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(*portp);

    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
	DPRINT(("bind() failed with %d\n", errno));
	return -1;
    }
    if (*portp == 0) {
	int addr_len = sizeof(addr);

	if (getsockname(sock, (struct sockaddr *)&addr, &addr_len) < 0) {
	    DPRINT(("getsockname() failed with %d\n", errno));
	    return -1;
	}
	*portp = ntohs(addr.sin_port);
	TRACE(("\tport=%d\n", *portp));
    }
    if (listen(sock, 4) < 0) {
	DPRINT(("listen() failed with %d\n", errno));
	return -1;
    }
    return sock;
}

IMConnection *
IMTCPConnection(protocol, wellknownfd)
Widget protocol;
int wellknownfd;
{
    IMConnection *conn;
    int fd;
    struct sockaddr_in addr;
    int addrlen = sizeof(addr);
    XtInputId id;

    TRACE(("IMTCPConnection()\n"));

    if ((fd = accept(wellknownfd, (struct sockaddr *)&addr, &addrlen)) < 0) {
	DPRINT(("accept() failed with %d\n", errno));
	return NULL;
    }
    conn = newConnection(protocol);
    conn->transport.ops = &SocketTransportOps;
    conn->transport.priv.sock.fd = fd;
    DDPRINT(2, ("new connection #%d: transport=TCP socket=%d\n",
		conn->serial, fd));

    id = XtAppAddInput(XtWidgetToApplicationContext(protocol), fd,
		       (XtPointer)XtInputReadMask, socketinput,
		       (XtPointer)conn);
    conn->transport.priv.sock.id = id;

    return conn;
}
#endif /* IM_TCP_TRANSPORT */

#ifdef IM_UNIX_TRANSPORT
int
IMCreateUnixService(path)
char *path;
{
    struct sockaddr_un addr;
    int sock;

    TRACE(("IMCreateUnixService(%s)\n", path));
    if ((sock = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) {
	DPRINT(("socket(PF_UNIX) failed with %d\n", errno));
	return -1;
    }

    bzero((char *)&addr, sizeof(addr));
    addr.sun_family = AF_UNIX;
    (void)strcpy(addr.sun_path, path);

    /*
     * Remove socket which is created by the previous process.
     */
    (void)unlink(path);

    if (bind(sock, (struct sockaddr *)&addr, strlen(path) + 2) < 0) {
	DPRINT(("bind() failed with %d\n", errno));
	return -1;
    }
    if (listen(sock, 4) < 0) {
	DPRINT(("listen() failed with %d\n", errno));
	return -1;
    }
    return sock;
}

IMConnection *
IMUnixConnection(protocol, wellknownfd)
Widget protocol;
int wellknownfd;
{
    IMConnection *conn;
    int fd;
    struct sockaddr_un addr;
    int addrlen = sizeof(addr);
    XtInputId id;

    TRACE(("IMUnixConnection()\n"));

    if ((fd = accept(wellknownfd, (struct sockaddr *)&addr, &addrlen)) < 0) {
	DPRINT(("accept() failed with %d\n", errno));
	return NULL;
    }
    conn = newConnection(protocol);
    conn->transport.ops = &SocketTransportOps;
    conn->transport.priv.sock.fd = fd;
    DDPRINT(2, ("new connection #%d: transport=UNIX socket=%d\n",
		conn->serial, fd));

    id = XtAppAddInput(XtWidgetToApplicationContext(protocol), fd,
		       (XtPointer)XtInputReadMask, socketinput,
		       (XtPointer)conn);
    conn->transport.priv.sock.id = id;

    return conn;
}
#endif /* IM_UNIX_TRANSPORT */

#ifdef IM_X_TRANSPORT
IMConnection *
IMXConnection(protocol, xev)
Widget protocol;
XEvent *xev;
{
    XClientMessageEvent *event = (XClientMessageEvent *)xev;
    XClientMessageEvent repl;
    Display *dpy = XtDisplay(protocol);
    Window client_window;
    IMConnection *conn;
    XAEHandle h;

    TRACE(("IMXConnection()\n"));

    if (event->type != ClientMessage ||
	event->display != dpy ||
	event->window != XtWindow(protocol) ||
	event->message_type != IMXConnectAtom(protocol) ||
	event->format != 32) {
	TRACE(("\tinvalid event\n"));
#ifdef DEBUG
	if (event->type != ClientMessage) printf("not ClientMessage\n");
	if (event->display != dpy) printf("wrong display\n");
	if (event->window != XtWindow(protocol)) printf("wrong window\n");
	if (event->message_type != IMXConnectAtom(protocol)) {
	    printf("wrong message type (%ld should be %ld)\n",
		   event->message_type, IMXConnectAtom(protocol));
	}
	if (event->format != 32) printf("wrong format\n");
#endif
	return NULL;
    }
    client_window = event->data.l[0];
    if (!IMValidateWindow(dpy, client_window, (IMWindowProfile *)NULL)) {
	DPRINT(("client window %08lx does not exist\n", client_window));
	return NULL;
    }
    conn = newConnection(protocol);
    conn->transport.ops = &XTransportOps;
    conn->transport.priv.x.client = client_window;
    conn->transport.priv.x.server = communicationWindow(protocol);
    DDPRINT(2, ("new connection #%d: transport=X client=%08lx\n",
		conn->serial, client_window));
    TRACE(("\ttransport version: %ld.%ld\n",
	   event->data.l[1], event->data.l[2]));

    repl.type = ClientMessage;
    repl.window = client_window;
    repl.message_type = IMXConnectAtom(protocol);
    repl.format = 32;
    repl.data.l[0] = conn->transport.priv.x.server;
    repl.data.l[1] = ServerMajorTransportVersion;
    repl.data.l[2] = ServerMinorTransportVersion;
    repl.data.l[3] = XTransportDividingSize;
    /* make it safe... */
    h = XAESetIgnoreErrors(dpy);
    XSendEvent(dpy, client_window, False, NoEventMask, (XEvent *)&repl);
    MyAddEventHandler(dpy, client_window, DestroyNotify, StructureNotifyMask,
		      xdestroy, (XtPointer)conn);
    XAEUnset(h);

    MyAddEventHandler(dpy, conn->transport.priv.x.server,
		      ClientMessage, NoEventMask, xinput, (XtPointer)conn);

    return conn;
}
#endif /* IM_X_TRANSPORT */

int
IMFlush(conn)
IMConnection *conn;
{
    TRACE(("IMFlush(#%d)\n", conn->serial));

    if (DDEBUG_CONDITION(100)) dumpBuf(IM_OUTBUF(conn), "** output buffer");

    return (*conn->transport.ops->flush)(conn);
}

void
IMShutdown(conn)
IMConnection *conn;
{
    TRACE(("IMShutdown(#%d)\n", conn->serial));
    (*conn->transport.ops->shutdown)(conn);
}

void
IMCloseConnection(conn)
IMConnection *conn;
{
    IMIM *imp;

    DDPRINT(2, ("IMCloseConnection(#%d)\n", conn->serial));

    imp = conn->im_list;
    while (imp != NULL) {
	IMIM *next = imp->next;

	IMDestroyIM(imp);
	imp = next;
    }

    IMBufClear(&conn->in_buf);
    IMBufClear(&conn->out_buf);
#ifdef notdef
    destroyAuth(conn->server_auth);
    destroyAuth(conn->client_auth);
#endif
    IMShutdown(conn);

    IMUnregisterConnection(conn);

    XtFree((char *)conn);
}
