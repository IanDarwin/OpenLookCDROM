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
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xlibint.h>
#include "Xtrans.h"
#include "FrameMgr.h"
#include "IMdkit.h"
#include "Xi18n.h"
#include "Xi18nTr.h"

#if NeedFunctionPrototypes
extern Xi18nClient *_Xi18nFindClient(Xi18n, CARD16);
extern Xi18nClient *_Xi18nNewClient(Xi18n);
extern void _Xi18nDeleteClient(Xi18n, CARD16);
static Bool TransRead(XtransConnInfo, char *, int, int *);
static Bool TransWrite(XtransConnInfo, char *, int);
static void Xi18nWaitTransListen(Display *,
				 int, XPointer);
static void Xi18nWaitTransAccept(Display *, int,
				 XPointer);
#else
extern Xi18nClient *_Xi18nFindClient();
extern Xi18nClient *_Xi18nNewClient();
extern void _Xi18nDeleteClient();
static Bool TransRead();
static Bool TransWrite();
static void Xi18nWaitTransListen();
static void Xi18nWaitTransAccept();
#endif

static unsigned char *
#if NeedFunctionPrototypes
ReadTrIMMessage(XIMS ims, int fd, int *connect_id)
#else
ReadTrIMMessage(ims, fd, connect_id)
XIMS ims;
int fd;
int *connect_id;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Xi18nClient *client = i18n_core->address.clients;
    TransClient *tr_client;

    FrameMgr fm;
    extern XimFrameRec packet_header_fr[];
    register int total_size;
    unsigned char *p = NULL;
    unsigned char *pp;
    int read_length;
    XimProtoHdr	*hdr;
    Bool isConnect = False;
    CARD8 major_opcode, minor_opcode;
    CARD16 length;

    while (client != NULL) {
	tr_client = (TransClient *)client->trans_rec;
	if (tr_client->accept_fd == fd) {
	    *connect_id = client->connect_id;
	    break;
	}
	client = client->next;
    }

    if ((hdr = (XimProtoHdr *)malloc(sizeof(hdr))) == NULL)
      return (unsigned char *)NULL;

    if (!TransRead(tr_client->accept_conn, (char *)hdr,
		   sizeof(hdr), &read_length) ||
	read_length != sizeof(hdr)) {
	goto read_error;
    } else {
	if (client->byte_order == '?') {
	    if (hdr->major_opcode == XIM_CONNECT) {
		CARD8 byte_order;
		if (!TransRead(tr_client->accept_conn, (char *)&byte_order,
			       sizeof(CARD8), &read_length) ||
		    read_length != sizeof(CARD8)) {
		    goto read_error;
		}
		isConnect = True;
		client->byte_order = (CARD8)byte_order;
	    } else {
		return (unsigned char *)NULL;	/* can do nothing */
	    }
	}
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
	pp = p;
	memmove(pp, &major_opcode, sizeof(CARD8)); pp += sizeof(CARD8);
	memmove(pp, &minor_opcode, sizeof(CARD8)); pp += sizeof(CARD8);
	memmove(pp, &length, sizeof(CARD16)); pp += sizeof(CARD16);
	XFree(hdr);
	if (!isConnect) {
	    if (length > 0) {
		if (!TransRead(tr_client->accept_conn, (char *)pp,
			       length * 4, &read_length) ||
		    read_length != length * 4) {
		    goto read_error;
		}
	    }
	} else {
	    memmove(pp, &client->byte_order, sizeof(CARD8));
	    pp += sizeof(CARD8);
	    if (!TransRead(tr_client->accept_conn, (char *)pp,
			   length * 4 - sizeof(CARD8), &read_length) ||
		read_length != length * 4 - sizeof(CARD8)) {
		goto read_error;
	    }
	}
    }
    return (unsigned char *)p;
  read_error:
    _XimdXTransDisconnect(tr_client->accept_conn);
    (void)_XimdXTransClose(tr_client->accept_conn);
    _XUnregisterInternalConnection(i18n_core->address.dpy, fd);
    return (unsigned char *)NULL;
}

static Bool 
#if NeedFunctionPrototypes
Xi18nTransBegin(XIMS ims)
#else
Xi18nTransBegin(ims)
XIMS ims;
#endif
{
    Xi18n i18n_core = ims->protocol;
    char *address = i18n_core->address.im_addr;
    TransSpecRec *spec = (TransSpecRec *)i18n_core->address.connect_addr;
    int fd;

    if (((spec->trans_conn = (struct _XtransConnInfo *)
	  _XimdXTransOpenCOTSServer(address)) == NULL) ||
	(_XimdXTransCreateListener(spec->trans_conn, spec->port) != 0)) {
	return False;
    }
    fd = _XimdXTransGetConnectionNumber(spec->trans_conn);
    return _XRegisterInternalConnection(i18n_core->address.dpy, fd,
			(_XInternalConnectionProc)Xi18nWaitTransListen,
			(XPointer)ims);
}

static Bool
#if NeedFunctionPrototypes
Xi18nTransEnd(XIMS ims)
#else
Xi18nTransEnd(ims)
XIMS ims;
#endif
{
    Xi18n i18n_core = ims->protocol;
    TransSpecRec *spec = (TransSpecRec *)i18n_core->address.connect_addr;
    int fd;
    
    fd = _XimdXTransGetConnectionNumber(spec->trans_conn);
    if (fd == 0) return False;
    _XUnregisterInternalConnection(i18n_core->address.dpy, fd);
    _XimdXTransDisconnect(spec->trans_conn);
    (void)_XimdXTransClose(spec->trans_conn);
    return True;
}

static Bool
#if NeedFunctionPrototypes
Xi18nTransSend(XIMS ims, CARD16 connect_id,
		unsigned char *reply, long length)
#else
Xi18nTransSend(ims, connect_id, reply, length)
XIMS ims;
CARD16 connect_id;
unsigned char *reply;
long length;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Xi18nClient *client = _Xi18nFindClient(i18n_core, connect_id);
    TransClient *tr_client = (TransClient *)client->trans_rec;

    if (length > 0)
      if (TransWrite(tr_client->accept_conn, (char *)reply, length) != length)
	return False;

    return True;
}

static Bool
#if NeedFunctionPrototypes
Xi18nTransWait(XIMS ims, CARD16 connect_id,
		CARD8 major_opcode, CARD8 minor_opcode)
#else
Xi18nTransWait(ims, connect_id, major_opcode, minor_opcode)
XIMS ims;
CARD16 connect_id;
CARD8 major_opcode;
CARD8 minor_opcode;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Xi18nClient *client = _Xi18nFindClient(i18n_core, connect_id);
    TransClient *tr_client = (TransClient *)client->trans_rec;
    int fd = _XimdXTransGetConnectionNumber(tr_client->accept_conn);

    for (;;) {
	unsigned char *packet;
	XimProtoHdr *hdr;
	int connect_id_ret;

	packet = ReadTrIMMessage(ims, fd, &connect_id_ret);
	hdr = (XimProtoHdr *)packet;

	if ((hdr->major_opcode == major_opcode) &&
	    (hdr->minor_opcode == minor_opcode))
	  return True;
	else if (hdr->major_opcode == XIM_ERROR)
	  return False;
    }
}

static Bool
#if NeedFunctionPrototypes
Xi18nTransDisconnect(XIMS ims, CARD16 connect_id)
#else
Xi18nTransDisconnect(ims, connect_id)
XIMS ims;
CARD16 connect_id;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Xi18nClient *client = _Xi18nFindClient(i18n_core, connect_id);
    TransClient *tr_client = (TransClient *)client->trans_rec;

    _XimdXTransDisconnect(tr_client->accept_conn);
    (void)_XimdXTransClose(tr_client->accept_conn);
    _XUnregisterInternalConnection(i18n_core->address.dpy,
				   tr_client->accept_fd);
    XFree(tr_client);
    _Xi18nDeleteClient(i18n_core, connect_id);
    return True;
}

static Bool
#if NeedFunctionPrototypes
TransRead(XtransConnInfo accept_conn, char *buf,
	  int buf_len, int *ret_len)
#else
TransRead(accept_conn, buf, buf_len, ret_len)
XtransConnInfo accept_conn;
char *buf;
int buf_len;
int *ret_len;
#endif
{
    int len;

    if ((len = _XimdXTransRead(accept_conn, buf, buf_len)) <= 0)
      return False;
    *ret_len = len;
    return True;
}

static Bool
#if NeedFunctionPrototypes
TransWrite(XtransConnInfo accept_conn, char *buf, int len)
#else
TransWrite(accept_conn, buf, len)
XtransConnInfo accept_conn;
char *buf;
int len;
#endif
{
    register int nbyte;

    while (len > 0) {
	if ((nbyte = _XimdXTransWrite(accept_conn, buf, len)) <= 0)
	  return False;
	len -= nbyte;
	buf += nbyte;
    }
    return True;
}

Bool
#if NeedFunctionPrototypes
_Xi18nCheckTransAddress(Xi18n i18n_core, TransportSW *transSW,
			char *address)
#else
_Xi18nCheckTransAddress(i18n_core, transSW, address)
Xi18n i18n_core;
TransportSW *transSW;
char *address;
#endif
{
    TransSpecRec *spec;
    char *p;
    char *hostname;

    if(!(spec = (TransSpecRec *)malloc(sizeof(TransSpecRec))))
      return False ;

    if (hostname = (char *)malloc(strlen(address) + 1))
      strcpy(hostname, address);

    if (p = (char *)index(hostname, ':')) {
	*p = 0; p++;
	spec->port = p;
    } else {
	XFree(hostname);
	return False;
    }
    i18n_core->address.connect_addr = (TransSpecRec *)spec;
    i18n_core->methods.begin = Xi18nTransBegin;
    i18n_core->methods.end  = Xi18nTransEnd;
    i18n_core->methods.send = Xi18nTransSend;
    i18n_core->methods.wait = Xi18nTransWait;
    i18n_core->methods.disconnect = Xi18nTransDisconnect;
    return True;
}

static TransClient *
#if NeedFunctionPrototypes
NewTrClient(Xi18n i18n_core, XtransConnInfo accept_conn)
#else
NewTrClient(i18n_core, accept_conn)
Xi18n i18n_core;
XtransConnInfo accept_conn;
#endif
{
    Xi18nClient *client = _Xi18nNewClient(i18n_core);
    TransClient *tr_client;

    tr_client = (TransClient *)malloc(sizeof(TransClient));

    tr_client->accept_conn = accept_conn;
    tr_client->accept_fd = _XimdXTransGetConnectionNumber(accept_conn);
    client->trans_rec = tr_client;

    return ((TransClient *)tr_client);
}

static void
#if NeedFunctionPrototypes
Xi18nWaitTransListen(Display *d, int fd, XPointer arg)
#else
Xi18nWaitTransListen(d, fd, arg)
Display *d;
int fd;
XPointer arg;
#endif
{
    XIMS ims =(XIMS)arg;
    Xi18n i18n_core = ims->protocol;
    TransSpecRec *spec = (TransSpecRec *)i18n_core->address.connect_addr;
    XtransConnInfo new_client;
    TransClient *client;
    int status;

    if ((new_client = (struct _XtransConnInfo *)
	 _XimdXTransAccept(spec->trans_conn, &status)) != NULL) {
	client = NewTrClient(i18n_core, new_client);
	(void)_XRegisterInternalConnection(i18n_core->address.dpy,
		     client->accept_fd,
		     (_XInternalConnectionProc)Xi18nWaitTransAccept,
		     (XPointer)ims);
    }
    return;
}

static void
#if NeedFunctionPrototypes
Xi18nWaitTransAccept(Display *d, int fd, XPointer arg)
#else
Xi18nWaitTransAccept(d, fd, arg)
Display *d;
int fd;
XPointer arg;
#endif
{
    XIMS ims =(XIMS)arg;
    extern void _Xi18nMessageHandler(
#if NeedFunctionPrototypes
     XIMS, CARD16, unsigned char *, Bool *
#endif
				     );
    Bool delete = True;
    unsigned char *packet;
    int connect_id;

    packet = ReadTrIMMessage(ims, fd, &connect_id);
    _Xi18nMessageHandler(ims, connect_id, packet, &delete);
    if (delete == True)
      XFree(packet);
    return;
}
