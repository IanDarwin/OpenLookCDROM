#ifndef lint
static char *rcsid = "$Id: improto.c,v 1.5 1994/05/31 07:54:04 ishisone Exp $";
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
#include "IMProtoP.h"

/*
 * This library contains interface functions between imlib and
 * IMProtocol widget.
 */

/*- IMRegisterConnection: register connection to the protocol widget -*/
void
IMRegisterConnection(conn)
IMConnection *conn;
{
    IMProtocolWidget ipw = (IMProtocolWidget)conn->proto_widget;
    
    conn->next = ipw->imp.connection_list;
    ipw->imp.connection_list = conn;
}

/*- IMUnregisterConnection: unregister connection from the protocol widget -*/
void
IMUnregisterConnection(conn)
IMConnection *conn;
{
    IMProtocolWidget ipw = (IMProtocolWidget)conn->proto_widget;
    IMConnection *p, *q;

    p = ipw->imp.connection_list;
    q = NULL;
    while (p != NULL) {
	if (p == conn) {
	    if (q == NULL) {
		ipw->imp.connection_list = p->next;
	    } else {
		q->next = p->next;
	    }
	    /*
	     * If there are no connections, and the no-more-connections
	     * flag is set to true, destroy the protocol widget.
	     */
	    if (ipw->imp.connection_list == NULL &&
		ipw->imp.no_more_connections == True) {
		DPRINT(("IMProtocolWidget no longer necessary. killing..\n"));
		XtDestroyWidget((Widget)ipw);
	    }
	    return;
	}
	q = p;
	p = p->next;
    }
}

/*- IMConnectionList: get connection list -*/
IMConnection *
IMConnectionList(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;

    return ipw->imp.connection_list;
}

/*- IMPushQueue: push connection to the scheduler queue -*/
void
IMPushQueue(conn)
IMConnection *conn;
{
    Widget w = conn->proto_widget;

    conn->queue_next = IM_QUEUE(w);
    IM_QUEUE(w) = conn;
}

/*- IMPopQueue: pop connection from the scheduler queue */
IMConnection *
IMPopQueue(w)
Widget w;
{
    IMConnection *conn;

    conn = IM_QUEUE(w);
    if (conn != NULL) {
	IM_QUEUE(w) = conn->queue_next;
	conn->queue_next = NULL;
    }
    return conn;
}

/*- IMQueueEmpty: returns true if the scheduler queue is empty -*/
int
IMQueueEmpty(w)
Widget w;
{
    return(IM_QUEUE(w) == NULL);
}

/*- IMDefaultForeground: get default foreground color -*/
Pixel
IMDefaultForeground(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;

    return ipw->imp.foreground;
}

/*- IMDefaultBackground: get default background color -*/
Pixel
IMDefaultBackground(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;

    return ipw->core.background_pixel;
}

/*- IMDefaultFontSet: get default font set -*/
char *
IMDefaultFontSet(imp)
IMIM *imp;
{
    IMProtocolWidget ipw = (IMProtocolWidget)imp->connection->proto_widget;

    return ipw->imp.default_fontlist;
}

/*- IMFontBank: get font bank for the specified IM -*/
FontBank
IMFontBank(imp)
IMIM *imp;
{
    IMProtocolWidget ipw = (IMProtocolWidget)imp->connection->proto_widget;

    return ipw->imp.font_bank;
}

/*- IMStatusWidth: get default status width -*/
int
IMStatusWidth(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;

    return ipw->imp.status_width;
}

/*- IMInitHash: initialize IM/IC hash table -*/
void
IMInitHash(w)
Widget w;
{
    IMIM **impp;
    IMIC **icpp;
    int i;

    impp = IM_IMHASH(w);
    icpp = IM_ICHASH(w);
    for (i = 0; i < IM_HASH_SIZE; i++) impp[i] = NULL;
    for (i = 0; i < IC_HASH_SIZE; i++) icpp[i] = NULL;
    IM_LASTIMID(w) = IM_LASTICID(w) = 0;
}

/*- IMIMHash: get IM hash table -*/
IMIM **
IMIMHash(w)
Widget w;
{
    return IM_IMHASH(w);
}

/*- IMICHash: get IC hash table -*/
IMIC **
IMICHash(w)
Widget w;
{
    return IM_ICHASH(w);
}

/*- IMNextIMID: return next IMID (it might be used, though) -*/
unsigned int
IMNextIMID(w)
Widget w;
{
    long id = IM_LASTIMID(w) + 1;	/* use long int to avoid overflow */

    if (id > 65535L) id = 1;
    IM_LASTIMID(w) = (unsigned int)id;
    return (unsigned int)id;
}

/*- IMNextICID: return next ICID (it might be used, though) -*/
unsigned int
IMNextICID(w)
Widget w;
{
    long id = IM_LASTICID(w) + 1;	/* use long int to avoid overflow */

    if (id > 65535L) id = 1;
    IM_LASTICID(w) = (unsigned int)id;
    return (unsigned int)id;
}

/*- IMCtextAtom: get COMPOUND_TEXT atom -*/
Atom
IMCtextAtom(w)
Widget w;
{
    return ATOM_CTEXT(w);
}

/*- IMKi2CommAtom: get _KINPUT2_COMM atom -*/
Atom
IMKi2CommAtom(w)
Widget w;
{
    return ATOM_KI2COMM(w);
}

/*- IMXConnectAtom: get _XIM_XCONNECT atom -*/
Atom
IMXConnectAtom(w)
Widget w;
{
    return ATOM_XCONNECT(w);
}

/*- IMProtocolAtom: get _XIM_PROTOCOL atom -*/
Atom
IMProtocolAtom(w)
Widget w;
{
    return ATOM_PROTOCOL(w);
}

/*- IMMoreDataAtom: get _XIM_MOREDATA atom -*/
Atom
IMMoreDataAtom(w)
Widget w;
{
    return ATOM_MOREDATA(w);
}

/*- IMTriggerKeys: get trigger key list -*/
IMTriggerKey *
IMTriggerKeys(imp, num_triggersp)
IMIM *imp;
int *num_triggersp;
{
    IMProtocolWidget ipw = (IMProtocolWidget)imp->connection->proto_widget;

    *num_triggersp = ipw->imp.num_trigger_keys;
    return ipw->imp.trigger_keys;
}

/*- getConverter: look up converter for the specified locale -*/
IMConverter *
IMGetConverter(w, locale)
Widget w;
char *locale;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;
    IMConverter *conv = &ipw->imp.converter;
    int i;

    for (i = 0; i < conv->num_locales; i++) {
	if (!strcmp(conv->supported_locales[i], locale)) return conv;
    }
    return NULL;
}
