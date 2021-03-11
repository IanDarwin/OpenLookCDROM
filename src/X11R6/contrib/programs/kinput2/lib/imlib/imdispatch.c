#ifndef lint
static char *rcsid = "$Id: imdispatch.c,v 1.9 1994/06/02 10:36:07 ishisone Exp $";
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
#include "imreq.h"

static int checkRequest _Pt_((IMConnection *conn,
			      int *majorp, int *minorp, int *arglenp));
static void ServerAuthPhase _Pt_((IMConnection *conn));
static void CommunicationPhase _Pt_((IMConnection *conn));
static int initialDispatcher _Pt_((IMConnection *conn));
static int mainDispatcher _Pt_((IMConnection *conn));

/*- checkRequest: read and examine request header -*/
static int
checkRequest(conn, majorp, minorp, arglenp)
IMConnection *conn;
int *majorp;
int *minorp;
int *arglenp;
{
    IMBuffer *ibp = IM_INBUF(conn);
    int len;

    /* check if there is enough data for the request header */
    if (IMBUFLEN(ibp) < 4) return 0;

#ifdef XIM_BC
    if (conn->has_length_bug) {
	len = (int)IMGetC16(conn, 2);
    } else {
	len = (int)IMGetC16(conn, 2) * 4;
    }
#else /* XIM_BC */
    len = (int)IMGetC16(conn, 2) * 4;
#endif /* XIM_BC */

    TRACE(("checkRequest(): length=%d, buflen=%d\n", len, IMBUFLEN(ibp)));

    if (IMBUFLEN(ibp) >= len + 4) {
	/*
	 * All the request data is in the buffer.
	 * Retrieve the request header, and discard it.
	 */
	*majorp = IMGetC8(conn, 0);
	*minorp = IMGetC8(conn, 1);
	*arglenp = len;
	IMBufDiscard(ibp, 4);
	return 1;
    } else {
	return 0;
    }
}

#ifdef XIM_BC
/*- countConnectRequestLen: count length of XIM_CONNECT request -*/
int
countConnectRequestLen(conn)
IMConnection *conn;
{
    IMBuffer *ibp = IM_INBUF(conn);
    int data_length = IMBUFLEN(ibp);
    int num_proto_names;
    int offset;
    int i;

    /*
     * Encoding of XIM_CONNECT request is:
     *
     *	4		request header
     *	1  CARD8	byte order
     *	1		unused
     *  2  CARD16	major protocol version
     *  2  CARD16	minor protocol version
     *	2  CARD16	number of auth protocol names
     *  N  LISTofSTRING	auth protocol names
     *
     * where STRING is:
     *  2  CARD16	length (N)
     *  N  LISTofChar	string
     *  p		pad, p = Pad(2+N)
     */

    /* skip request header */
    data_length -= 4;
    offset = 4;

    if (data_length < 8) return 0;	/* incomplete data */

    /* get the number of auth protocol names */
    num_proto_names = (int)IMGetC16(conn, offset + 6);

    /* skip fixed part */
    data_length -= 8;
    offset += 8;

    /* count variable (i.e. list of string) part */
    for (i = 0; i < num_proto_names; i++) {
	int str_len, pad_len;

	if (data_length < 2) return 0;	/* incomplete data */
	str_len = (int)IMGetC16(conn, offset);
	pad_len = (4 - ((2 + str_len) % 4)) % 4;
	data_length -= 2 + str_len + pad_len;
	offset += 2 + str_len + pad_len;
	if (data_length < 0) return 0;	/* incomplete data */
    }

    return offset - 4;		/* 4 for the header */
}
#endif /* XIM_BC */

/*
 * Phase shifters
 */

/*- ServerAuthPhase: move to server authentication phase -*/
static void
ServerAuthPhase(conn)
IMConnection *conn;
{
    /* not yet implemented */
    IMSendSimpleRequest(conn, XIM_AUTH_NG, 0);
    IMSchedule(conn, SCHED_CLOSE);
}

/*- CommunicationPhase: move to comunication (i.e. main) phase -*/
static void
CommunicationPhase(conn)
IMConnection *conn;
{
    IMPutHeader(conn, XIM_CONNECT_REPLY, 0, 4);
    IMPutC16(conn, XIM_MAJOR_PROTOCOL_VERSION);
    IMPutC16(conn, XIM_MINOR_PROTOCOL_VERSION);
    IMSchedule(conn, SCHED_WRITE);

    conn->dispatcher = mainDispatcher;
}

/*
 * Request dispatchers
 */

/*- initialDispatcher: the initial dispatcher -*/
static int
initialDispatcher(conn)
IMConnection *conn;
{
    IMBuffer *ibp = IM_INBUF(conn);
    int major, minor, arglen;
    int major_protocol, minor_protocol;
    int num_auth;
#ifdef XIM_BC
    int counted_len;
    int field_len;
#endif /* XIM_BC */

    TRACE(("imlib:initialDispatcher(#%d)\n", conn->serial));

    /*
     * The first request must be 'XIM_CONNECT'.
     */
    if (conn->byte_order == ORDER_UNKNOWN) {
	char *p = IMBUFDATA(ibp);

	/*
	 * Check the byte-order first.
	 * In order to do it, we need at least 5 bytes of data.
	 */
	if (IMBUFLEN(ibp) < 5) return 0;

	/*
	 * Check the byte-order byte (5th byte of the data).
	 */
	switch (p[4]) {
	case 0x42:	/* 'B' -- big endian */
	    conn->byte_order = ORDER_BIG;
	    TRACE(("\tbyte order is big endian\n"));
	    break;
	case 0x6c:	/* 'l' -- little endian */
	    conn->byte_order = ORDER_LITTLE;
	    TRACE(("\tbyte order is little endian\n"));
	    break;
	default:	/* invalid request */
	    /*
	     * what can we do here?  since we don't know the byte order
	     * of the client, we cannot send error reply. but we can
	     * send XIM_AUTH_NG, because this request does not have
	     * byte order dependency.
	     */
	    DDPRINT(2, ("invalid byte order field (%c)\n", p[4]));
	    goto send_ng;
	}
    }

#ifdef XIM_BC
    /*
     * Xlib implementation of early X11R6 has a bug in the
     * length field of request header.  The field should
     * represent the length of request data as the number
     * of 4byte units, but the buggy Xlib puts number of
     * bytes instead.
     */

    /* length of data by examining contents */
    counted_len = countConnectRequestLen(conn);	/* in bytes */
    if (counted_len == 0) return 0;		/* incomplete */
    /* length of data by reading length field */
    field_len = IMGetC16(conn, 2);		/* num of 4byte element */

    /*
     * If the request packet comforms to the specification,
     *    field_len * 4 == counted_len
     * but in case of buggy Xlib implementation,
     *    field_len == counted_len
     */
    if (counted_len == field_len * 4) {
	conn->has_length_bug = 0;
    } else if (counted_len == field_len) {
	/* buggy Xlib implementation */
	DPRINT(("connection #%d has length field bug\n", conn->serial));
	conn->has_length_bug = 1;
    } else {
	/* totally broken */
	DPRINT(("connection #%d is broken\n", conn->serial));
	goto send_ng;
    }
#endif

    /*
     * See if the entire data of the request is ready.
     */
    if (!checkRequest(conn, &major, &minor, &arglen)) return 0;

    /*
     * Check the arguments...
     */
    if (major != XIM_CONNECT || minor != 0 || arglen < 8) {
	DPRINT(("invalid initial request (major=%d, minor=%d, arglen=%d\n",
		major, minor, arglen));
	goto send_ng;
    }

    major_protocol = (int)IMGetC16(conn, 2);
    minor_protocol = (int)IMGetC16(conn, 4);
    TRACE(("\tprototol version: major=%d, minor=%d\n",
	   major_protocol, minor_protocol));
    conn->major_protocol_version = major_protocol;
    conn->minor_protocol_version = minor_protocol;

    num_auth = (int)IMGetC16(conn, 6);
    TRACE(("\tnumber of auth protocols: %d\n", num_auth));

    if (major_protocol > XIM_MAJOR_PROTOCOL_VERSION ||
	(major_protocol == XIM_MAJOR_PROTOCOL_VERSION &&
	 minor_protocol > XIM_MINOR_PROTOCOL_VERSION)) {
	DPRINT(("unsupported protocol (%d,%d)\n",
		major_protocol, minor_protocol));
	goto send_ng;
    }

    if (num_auth > 0) {
	ServerAuthPhase(conn);
#ifdef notdef
    } else if (do_client_authentication) {
	ClientAuthPhase(conn);
#endif
    } else {
	CommunicationPhase(conn);
    }

    IMBufDiscard(ibp, arglen);

    return 1;

 send_ng:
    DDPRINT(2, ("send XIM_AUTH_NG to #%d\n", conn->serial));
    IMSendSimpleRequest(conn, XIM_AUTH_NG, 0);
    IMSchedule(conn, SCHED_SHUTDOWN);
    return 0;
}

/*- mainDispatcher: main dispatcher -*/
static int
mainDispatcher(conn)
IMConnection *conn;
{
    IMBuffer *ibp = IM_INBUF(conn);
    int major, minor, arglen;
    IMRequest *req;

    TRACE(("imlib:mainDispatcher(#%d)\n", conn->serial));

    /*
     * When X transport is used, NUL bytes might appear between
     * requests.  So discard them first.
     */
    IMBufDiscardNUL(ibp);

    /*
     * Check if the entire request data is on the input buffer.
     */
    if (!checkRequest(conn, &major, &minor, &arglen)) return 0;

    /*
     * Check major opcode.
     */
    req = IMMainDispatchTable[major];
    if (req == NULL) {
	DDPRINT(2, ("bad major opcode(%d)\n", major));
	IMSendBadProtocol(conn, "Invalid major opcode");
	goto ret;
    }

    /*
     * Then, check minor opcode.
     */
    while (req != NULL) {
	if (req->minor == minor) break;
	req = req->next;
    }
    if (req == NULL) {
	DDPRINT(2, ("bad minor opcode(%d,%d)\n", major, minor));
	IMSendBadProtocol(conn, "Invalid minor opcode");
	goto ret;
    }

    /*
     * Opcode is valid. Call the request processing routine.
     */
    DDPRINT(2, ("** processing %s request...\n", req->name));
    (*req->proc)(conn, major, minor, arglen);

 ret:
    /*
     * Discard the argument portion.
     */
    IMBufDiscard(ibp, arglen);

    return 1;
}

/*
 * Public functions
 */

void
IMSetInitialDispatcher(conn)
IMConnection *conn;
{
    TRACE(("IMSetInitialDispatcher(#%d)\n", conn->serial));
    conn->dispatcher = initialDispatcher;
}

void
IMDispatch(conn, cond)
IMConnection *conn;
int cond;
{
    TRACE(("IMDispatch(#%d)\n", conn->serial));

    switch (cond) {
    case TRANSPORT_OK:
	/*
	 * Call dispatcher while data is ready.
	 */
	while ((*conn->dispatcher)(conn))
	    /* empty body */;

	/*
	 * Do compaction to the input buffer.
	 */
	IMBufCompact(IM_INBUF(conn));
	break;

    case TRANSPORT_ERROR:
	DDPRINT(2, ("transport error\n"));
	IMSchedule(conn, SCHED_SHUTDOWN);
	break;

    case TRANSPORT_EOF:
	DDPRINT(2, ("transport EOF\n"));
	IMSchedule(conn, SCHED_CLOSE);
	break;
    }

    /*
     * If there's something to be done (i.e. scheduled),
     * do it.
     */
    if (!IMQueueEmpty(conn->proto_widget)) {
	IMProcessQueue(conn->proto_widget);
    }
}

void
IMSchedule(conn, type)
IMConnection *conn;
int type;
{
    TRACE(("IMSchedule(#%d, %d)\n", conn->serial, type));

    if (conn->schedule & type) return;	/* already scheduled */

    if (conn->schedule == 0) {
	TRACE(("insert into the queue\n"));
	IMPushQueue(conn);
    }
    conn->schedule |= type;
}

void
IMProcessQueue(w)
Widget w;
{
    IMConnection *conn;
    IMConnection *tmp = NULL;

    TRACE(("IMProcessQueue()\n"));

    while ((conn = IMPopQueue(w)) != NULL) {
	int schedule;

	schedule = conn->schedule;
	conn->schedule = 0;

	if (schedule & SCHED_SHUTDOWN) {
	    /*
	     * This connection is dead.  Don't need to
	     * flush output before closing.
	     */
	    IMCloseConnection(conn);
	} else {
	    if (schedule & SCHED_WRITE) {
		/*
		 * Flush output buffer.
		 */
		int ret;

		ret = IMFlush(conn);
		switch (ret) {
		case TRANSPORT_ERROR:
		    IMCloseConnection(conn);
		    continue;
		case TRANSPORT_PARTIAL:
		    /* to be queued again */
		    conn->schedule = schedule;
		    conn->queue_next = tmp;
		    tmp = conn;
		    continue;
		}
	    }
	    if (schedule & SCHED_CLOSE) {
		IMCloseConnection(conn);
	    }
	}
    }

    /* reschedule */
    while (tmp != NULL) {
	IMConnection *next = tmp->queue_next;

	TRACE(("reschedule #%d\n", tmp->serial));
	IMPushQueue(tmp);
	tmp = next;
    }
}
