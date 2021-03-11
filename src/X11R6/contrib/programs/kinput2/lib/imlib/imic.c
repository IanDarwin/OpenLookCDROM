#ifndef lint
static char *rcsid = "$Id: imic.c,v 1.6 1994/05/31 06:44:17 ishisone Exp $";
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

#define IMHASHVAL(imp)	((imp)->id % IM_HASH_SIZE)
#define ICHASHVAL(icp)	((icp)->id % IC_HASH_SIZE)

static IMIM *id2IM _Pt_((IMConnection *conn, unsigned int id));
static IMIC *id2IC _Pt_((IMIM *imp, unsigned int id));
static unsigned int newIMID _Pt_((IMConnection *conn));
static unsigned int newICID _Pt_((IMIM *imp));
static void registerIM _Pt_((IMIM *imp));
static void registerIC _Pt_((IMIC *icp));
static int unregisterIM _Pt_((IMIM *imp));
static int unregisterIC _Pt_((IMIC *icp));

/*- id2IM: input-method ID to IM converter -*/
static IMIM *
id2IM(conn, id)
IMConnection *conn;
unsigned int id;
{
    IMIM **imHash = IMIMHash(conn->proto_widget);
    IMIM *imp;

    imp = imHash[id % IM_HASH_SIZE];
    while (imp != NULL) {
	if (imp->id == id) return imp;
	imp = imp->hash_next;
    }
    return NULL;
}

/*- id2IC: input-context ID to IC converter -*/
static IMIC *
id2IC(imp, id)
IMIM *imp;
unsigned int id;
{
    IMIC **icHash = IMICHash(imp->connection->proto_widget);
    IMIC *icp;

    icp = icHash[id % IC_HASH_SIZE];
    while (icp != NULL) {
	if (icp->id == id) return icp;
	icp = icp->hash_next;
    }
    return NULL;
}

/*- newIMID: return unused input-method ID -*/
static unsigned int
newIMID(conn)
IMConnection *conn;
{
    Widget w = conn->proto_widget;
    unsigned int id, id_start;

    id = id_start = IMNextIMID(w);
    do {
	if (id2IM(conn, id) == NULL) return id;	/* unused ID */
    } while ((id = IMNextIMID(w)) != id_start);
    return 0;
}

/*- newICID: return unused input-context ID -*/
static unsigned int
newICID(imp)
IMIM *imp;
{
    Widget w = imp->connection->proto_widget;
    unsigned int id, id_start;

    id = id_start = IMNextICID(w);
    do {
	if (id2IC(imp, id) == NULL) return id;	/* unused ID */
    } while ((id = IMNextICID(w)) != id_start);
    return 0;
}

/*- registerIM: register IM to hash table -*/
static void
registerIM(imp)
IMIM *imp;
{
    IMIM **imHash = IMIMHash(imp->connection->proto_widget);

    imp->hash_next = imHash[IMHASHVAL(imp)];
    imHash[IMHASHVAL(imp)] = imp;
}

/*- registerIC: register IC to hash table -*/
static void
registerIC(icp)
IMIC *icp;
{
    IMIC **icHash = IMICHash(icp->im->connection->proto_widget);

    icp->hash_next = icHash[ICHASHVAL(icp)];
    icHash[ICHASHVAL(icp)] = icp;
}

/*- unregisterIM: remove IM from hash table -*/
static int
unregisterIM(imp)
IMIM *imp;
{
    IMIM **imHash = IMIMHash(imp->connection->proto_widget);
    IMIM *p, *q;

    p = imHash[IMHASHVAL(imp)];
    q = NULL;

    while (p != NULL && p != imp) {
	q = p;
	p = p->hash_next;
    }
    if (p == NULL) return 0;

    if (q != NULL) {
	q->hash_next = p->hash_next;
    } else {
	imHash[IMHASHVAL(imp)] = p->hash_next;
    }
    return 1;
}

/*- unregisterIC: remove IC from hash table -*/
static int
unregisterIC(icp)
IMIC *icp;
{
    IMIC **icHash = IMICHash(icp->im->connection->proto_widget);
    IMIC *p, *q;

    p = icHash[ICHASHVAL(icp)];
    q = NULL;

    while (p != NULL && p != icp) {
	q = p;
	p = p->hash_next;
    }
    if (p == NULL) return 0;

    if (q != NULL) {
	q->hash_next = p->hash_next;
    } else {
	icHash[ICHASHVAL(icp)] = p->hash_next;
    }
    return 1;
}

/*
 * Public functions
 */

IMIM *
IMGetIM(conn, arglen)
IMConnection *conn;
int arglen;
{
    unsigned int id;
    IMIM *imp;

    /* Check argument length */
    if (arglen < 2) {
	IMSendError(conn, IMBadSomething, 0, 0, "input-method ID expected");
	return NULL;
    }

    id = IMGetC16(conn, 0);
    imp = id2IM(conn, id);
    if (imp != NULL && imp->connection == conn)	return imp;
    IMSendError(conn, IMBadSomething, 0, 0, "invalid input-method ID");
    return NULL;
}

IMIC *
IMGetIC(conn, arglen)
IMConnection *conn;
int arglen;
{
    unsigned int imid, icid;
    IMIM *imp;
    IMIC *icp;

    /* Check argument length */
    if (arglen < 4) {
	IMSendError(conn, IMBadSomething, 0, 0, "input-method ID expected");
	return NULL;
    } else if (arglen < 4) {
	IMSendError(conn, IMBadSomething, 0, 0, "input-context ID expected");
	return NULL;
    }

    imid = IMGetC16(conn, 0);
    icid = IMGetC16(conn, 2);

    if ((imp = id2IM(conn, imid)) == NULL || imp->connection != conn) {
	IMSendError(conn, IMBadSomething, 0, 0, "invalid input-method ID");
	return NULL;
    }
    if ((icp = id2IC(imp, icid)) == NULL || icp->im != imp) {
	IMSendError(conn, IMBadSomething, 0, 0, "invalid input-context ID");
	return NULL;
    }
    return icp;
}

IMIM *
IMCreateIM(conn, converter)
IMConnection *conn;
IMConverter *converter;
{
    IMIM *imp;

    imp = XtNew(IMIM);

    imp->id = newIMID(conn);
    imp->connection = conn;
    imp->converter = converter;
    imp->mask = 0;
    imp->ic_list = NULL;

    registerIM(imp);
    imp->next = conn->im_list;
    conn->im_list = imp;

    return imp;
}


IMIC *
IMCreateIC(imp)
IMIM *imp;
{
    IMIC *icp;

    icp = XtNew(IMIC);

    icp->id = newICID(imp);

    /*
     * Initialize data
     */
    icp->im = imp;
    icp->conversion = NULL;
    icp->state = 0;
    icp->pending_events = NULL;
    icp->style = IMSTYLE_SEPARATE;
    icp->common_attr.set_mask = icp->common_attr.change_mask = 0;
    icp->preedit_attr.set_mask = icp->preedit_attr.change_mask = 0;
    icp->status_attr.set_mask = icp->status_attr.change_mask = 0;
    icp->fonts = NULL;
    icp->num_fonts = 0;

    registerIC(icp);
    icp->next = imp->ic_list;
    imp->ic_list = icp;

    return icp;
}

void
IMDestroyIM(imp)
IMIM *imp;
{
    IMIC *icp = imp->ic_list;
    IMIC *icp_next;

    (void)unregisterIM(imp);

    while (icp != NULL) {
	icp_next = icp->next;
	IMDestroyIC(icp);
	icp = icp_next;
    }

    XtFree((char *)imp);
}

void
IMDestroyIC(icp)
IMIC *icp;
{
    IMPendingEvent *pending;
    (void)unregisterIC(icp);

    /*
     * Stop conversion.
     */
    if (icp->state & IC_CONVERTING) {
	IMStopConversion(icp);
    }

    /*
     * Free pending event queue.
     */
    pending = icp->pending_events;
    while (pending != NULL) {
	IMPendingEvent *next = pending->next;

	XtFree((char *)pending);
	pending = next;
    }

    /*
     * Free IC attributes.
     */
    IMFreeICAttributes(icp);

    XtFree((char *)icp);
}

