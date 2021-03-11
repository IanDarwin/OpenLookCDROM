#ifndef lint
static char *rcsid = "$Id: imrequest.c,v 1.7 1994/06/02 10:35:06 ishisone Exp $";
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

/* in order to include xEvent definition */
#define NEED_EVENTS
#include <X11/Xproto.h>

/*
 * Request handle functions
 */
static void ximDisconnectProc _Pt_((IMConnection *, int, int, int));
static void ximErrorProc _Pt_((IMConnection *, int, int, int));
static void ximOpenProc _Pt_((IMConnection *, int, int, int));
static void ximCloseProc _Pt_((IMConnection *, int, int, int));
static void ximTriggerNotifyProc _Pt_((IMConnection *, int, int, int));
static void ximEncodingNegotiationProc _Pt_((IMConnection *, int, int, int));
static void ximQueryExtensionProc _Pt_((IMConnection *, int, int, int));
static void ximSetIMValuesProc _Pt_((IMConnection *, int, int, int));
static void ximGetIMValuesProc _Pt_((IMConnection *, int, int, int));
static void ximCreateICProc _Pt_((IMConnection *, int, int, int));
static void ximDestroyICProc _Pt_((IMConnection *, int, int, int));
static void ximSetICValuesProc _Pt_((IMConnection *, int, int, int));
static void ximGetICValuesProc _Pt_((IMConnection *, int, int, int));
static void ximSetICFocusProc _Pt_((IMConnection *, int, int, int));
static void ximUnsetICFocusProc _Pt_((IMConnection *, int, int, int));
static void ximForwardEventProc _Pt_((IMConnection *, int, int, int));
static void ximExtForwardKeyeventProc _Pt_((IMConnection *, int, int, int));
static void ximSyncProc _Pt_((IMConnection *, int, int, int));
static void ximSyncReplyProc _Pt_((IMConnection *, int, int, int));
static void ximResetICProc _Pt_((IMConnection *, int, int, int));
static void ximExtMoveProc _Pt_((IMConnection *, int, int, int));

static void ximAlreadyConnectedProc _Pt_((IMConnection *, int, int, int));
static void ximShouldNotBeSentProc _Pt_((IMConnection *, int, int, int));
static void ximAuthPhaseProc _Pt_((IMConnection *, int, int, int));

/*
 * Core request table
 */
static IMRequest coreRequestTbl[] = {
    { "CONNECT", XIM_CONNECT, 0, ximAlreadyConnectedProc },
    { "CONNECT-REPLY", XIM_CONNECT_REPLY, 0, ximShouldNotBeSentProc },
    { "DISCONNECT", XIM_DISCONNECT, 0, ximDisconnectProc },
    { "DISCONNECT-REPLY", XIM_DISCONNECT_REPLY, 0, ximShouldNotBeSentProc },
    { "AUTH-REQUIRED", XIM_AUTH_REQUIRED, 0, ximAuthPhaseProc },
    { "AUTH-REPLY", XIM_AUTH_REPLY, 0, ximAuthPhaseProc },
    { "AUTH-NEXT", XIM_AUTH_NEXT, 0, ximAuthPhaseProc },
    { "AUTH-SETUP", XIM_AUTH_SETUP, 0, ximShouldNotBeSentProc },
    { "AUTH-NG", XIM_AUTH_NG, 0, ximAuthPhaseProc },
    { "ERROR", XIM_ERROR, 0, ximErrorProc },
    { "OPEN", XIM_OPEN, 0, ximOpenProc },
    { "OPEN-REPLY", XIM_OPEN_REPLY, 0, ximShouldNotBeSentProc },
    { "CLOSE", XIM_CLOSE, 0, ximCloseProc },
    { "CLOSE-REPLY", XIM_CLOSE_REPLY, 0, ximShouldNotBeSentProc },
    { "SET-EVENT-MASK", XIM_SET_EVENT_MASK, 0, ximShouldNotBeSentProc },
    { "REGISTER-TRIGGERKEYS", XIM_REGISTER_TRIGGERKEYS, 0,
	  ximShouldNotBeSentProc },
    { "TRIGGER-NOTIFY", XIM_TRIGGER_NOTIFY, 0, ximTriggerNotifyProc },
    { "TRIGGER-NOTIFY-REPLY", XIM_TRIGGER_NOTIFY_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "ENCODING-NEGOTIATION", XIM_ENCODING_NEGOTIATION, 0,
	  ximEncodingNegotiationProc },
    { "ENCODING-NEGOTIATION-REPLY", XIM_ENCODING_NEGOTIATION_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "QUERY-EXTENSION", XIM_QUERY_EXTENSION, 0, ximQueryExtensionProc },
    { "QUERY-EXTENSION-REPLY", XIM_QUERY_EXTENSION_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "SET-IM-VALUES", XIM_SET_IM_VALUES, 0, ximSetIMValuesProc },
    { "SET-IM-VALUES-REPLY", XIM_SET_IM_VALUES_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "GET-IM-VALUES", XIM_GET_IM_VALUES, 0, ximGetIMValuesProc },
    { "GET-IM-VALUES-REPLY", XIM_GET_IM_VALUES_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "CREATE-IC", XIM_CREATE_IC, 0, ximCreateICProc },
    { "CREATE-IC-REPLY", XIM_CREATE_IC_REPLY, 0, ximShouldNotBeSentProc },
    { "DESTROY-IC", XIM_DESTROY_IC, 0, ximDestroyICProc },
    { "DESTROY-IC-REPLY", XIM_DESTROY_IC_REPLY, 0, ximShouldNotBeSentProc },
    { "SET-IC-VALUES", XIM_SET_IC_VALUES, 0, ximSetICValuesProc },
    { "SET-IC-VALUES-REPLY", XIM_SET_IC_VALUES_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "GET-IC-VALUES", XIM_GET_IC_VALUES, 0, ximGetICValuesProc },
    { "GET-IC-VALUES-REPLY", XIM_GET_IC_VALUES_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "SET-IC-FOCUS", XIM_SET_IC_FOCUS, 0, ximSetICFocusProc },
    { "UNSET-IC-FOCUS", XIM_UNSET_IC_FOCUS, 0, ximUnsetICFocusProc },
    { "FORWARD-EVENT", XIM_FORWARD_EVENT, 0, ximForwardEventProc },
    { "SYNC", XIM_SYNC, 0, ximSyncProc },
    { "SYNC-REPLY", XIM_SYNC_REPLY, 0, ximSyncReplyProc },
    { "COMMIT", XIM_COMMIT, 0, ximShouldNotBeSentProc },
    { "RESET-IC", XIM_RESET_IC, 0, ximResetICProc },
    { "RESET-IC-REPLY", XIM_RESET_IC_REPLY, 0, ximShouldNotBeSentProc },
    { "GEOMETRY", XIM_GEOMETRY, 0, ximShouldNotBeSentProc },
    { "STR-CONVERSION", XIM_STR_CONVERSION, 0, ximShouldNotBeSentProc },
    { "STR-CONVERSION-REPLY", XIM_STR_CONVERSION_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "PREEDIT-START", XIM_PREEDIT_START, 0, ximShouldNotBeSentProc },
    { "PREEDIT-START-REPLY", XIM_PREEDIT_START_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "PREEDIT-DRAW", XIM_PREEDIT_DRAW, 0, ximShouldNotBeSentProc },
    { "PREEDIT-CARET", XIM_PREEDIT_CARET, 0, ximShouldNotBeSentProc },
    { "PREEDIT-CARET-REPLY", XIM_PREEDIT_CARET_REPLY, 0,
	  ximShouldNotBeSentProc },
    { "PREEDIT-DONE", XIM_PREEDIT_DONE, 0, ximShouldNotBeSentProc },
    { "PREEDITSTATE", XIM_PREEDITSTATE, 0, ximShouldNotBeSentProc },
    { "STATUS-START", XIM_STATUS_START, 0, ximShouldNotBeSentProc },
    { "STATUS-DRAW", XIM_STATUS_DRAW, 0, ximShouldNotBeSentProc },
    { "STATUS-DONE", XIM_STATUS_DONE, 0, ximShouldNotBeSentProc },
    { NULL, 0, 0, NULL },
};

/*
 * Extension request table
 */
static IMRequest extRequestTbl[] = {
    { "XIM_EXT_SET_EVENT_MASK", XIM_EXT_SET_EVENT_MASK, 0,
	  ximShouldNotBeSentProc, XIM_EXT_SET_EVENT_MASK_MASK },
    { "XIM_EXT_FORWARD_KEYEVENT", XIM_EXT_FORWARD_KEYEVENT, 0,
	  ximExtForwardKeyeventProc, XIM_EXT_FORWARD_KEYEVENT_MASK },
    { "XIM_EXT_MOVE", XIM_EXT_MOVE, 0,
	  ximExtMoveProc, XIM_EXT_MOVE_MASK },
    { NULL, 0, 0, NULL },
};

/*
 * Request dispatch table (global)
 */
IMRequest *IMMainDispatchTable[256];


static void compileTbl _Pt_((IMRequest *req));
static char *ecode2str _Pt_((int code));
static void putTriggerkeyList _Pt_((IMIM *imp));
static void setEventMask _Pt_((IMConnection *conn, unsigned int imid,
			       unsigned int icid, unsigned long forward_mask,
			       unsigned long synchronous_mask));
static int findCtext _Pt_((IMConnection *conn, int arglen));
static IMRequest *getExtension _Pt_((char *name));
static void putExtension _Pt_((IMConnection *conn, IMRequest *req));
static void swapData _Pt_((char *data, char *format));
static int getEvent _Pt_((IMConnection *conn, unsigned int serial,
			  int offset, XEvent *ev));
static void postponeEvent _Pt_((IMIC *icp, int synchronous, XEvent *ev));
static void handleForwardedEvent _Pt_((IMIC *icp, int synchronous, XEvent *ev));
static void processPendingEvents _Pt_((IMIC *icp,
				       IMPendingEvent *pending_event));


/*- compileTbl: assemble IMMainDispatchTbl -*/
static void
compileTbl(req)
IMRequest *req;
{
    while (req->name != NULL) {
	req->next = IMMainDispatchTable[req->major];
	IMMainDispatchTable[req->major] = req;
	req++;
    }
}

/*- ximDisconnectProc: handle XIM_DISCONNECT request -*/
/* ARGSUSED */
static void
ximDisconnectProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp = conn->im_list;
    IMIM *imp_next;

    TRACE(("ximDisconnectProc(#%d)\n", conn->serial));

    if (arglen != 0) {
	DPRINT(("ximDisconnectProc: arglen != 0\n"));
	IMSendBadLength(conn, 0, 0);
	return;
    }

    while (imp != NULL) {
	imp_next = imp->next;
	IMDestroyIM(imp);
	imp = imp_next;
    }
    IMSendSimpleRequest(conn, XIM_DISCONNECT_REPLY, 0);
    IMSchedule(conn, SCHED_CLOSE);
}

/*- ecode2str: error code to error name string converter -*/
static char *
ecode2str(code)
int code;
{
    char *s;

    switch (code) {
    case IMBadAlloc:		s = "BadAlloc"; break;
    case IMBadStyle:		s = "BadStyle"; break;
    case IMBadClientWindow:	s = "BadClientWindow"; break;
    case IMBadFocusWindow:	s = "BadFocusWindow"; break;
    case IMBadArea:		s = "BadArea"; break;
    case IMBadSpotLocation:	s = "BadSpotLocation"; break;
    case IMBadColormap:		s = "BadColormap"; break;
    case IMBadAtom:		s = "BadAtom"; break;
    case IMBadPixel:		s = "BadPixel"; break;
    case IMBadPixmap:		s = "BadPixmap"; break;
    case IMBadName:		s = "BadName"; break;
    case IMBadCursor:		s = "BadCursor"; break;
    case IMBadProtocol:		s = "BadProtocol"; break;
    case IMBadForeground:	s = "BadForeground"; break;
    case IMBadBackground:	s = "BadBackground"; break;
    case IMLocaleNotSupported:	s = "LocaleNotSupported"; break;
    case IMBadSomething:	s = "BadSomething"; break;
    default:			s = "? (undefined error code)"; break;
    }
    return s;
}

/*- ximErrorProc: handle XIM_Error messsage -*/
/* ARGSUSED */
static void
ximErrorProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    unsigned int imid, icid;
    int flag;
    int code;
    int msg_len;
    char msg[256 + 1];

    TRACE(("ximErrorProc(#%d)\n", conn->serial));

    if (arglen < 10) {
	DPRINT(("corrupted XIM_ERROR message (arglen < 10) received\n"));
	return;
    }
    imid = IMGetC16(conn, 0);
    icid = IMGetC16(conn, 2);
    flag = IMGetC16(conn, 4);
    code = IMGetC16(conn, 6);
    msg_len = IMGetC16(conn, 8);
    if (msg_len > 0) {
	if (msg_len + 12 > arglen) {
	    DPRINT(("corrupted XIM_ERROR message (message too long) received\n"));
	    return;
	}
	if (msg_len > 256) msg_len = 256;
	IMGetString(conn, 12, msg, msg_len);
    }
    if (DEBUG_CONDITION) {
	printf("** XIM_ERROR message received:\n");
	if (flag & 1) {
	    printf("\t input-method ID: %d\n", imid);
	} else {
	    printf("\t input-method ID: N/A\n");
	}
	if (flag & 2) {
	    printf("\tinput-context ID: %d\n", icid);
	} else {
	    printf("\tinput-context ID: N/A\n");
	}
	printf("\t      error code: %s\n", ecode2str(code));
	printf("\t   error message: %s\n", msg);
    }
}

/*- putTriggerkeyList: put trigger key list on the output stream -*/
static void
putTriggerkeyList(imp)
IMIM *imp;
{
    IMConnection *conn = imp->connection;
    IMTriggerKey *triggers;
    int num_triggers;
    int i;

    triggers = IMTriggerKeys(imp, &num_triggers);

    IMPutC32(conn, (unsigned long)(12 * num_triggers));
    for (i = 0; i < num_triggers; i++) {
	IMPutC32(conn, triggers[i].keysym);
	IMPutC32(conn, (unsigned long)triggers[i].modifiers);
	IMPutC32(conn, (unsigned long)triggers[i].check_modifiers);
    }
}

/*- setEventMask: put XIM_SET_EVENT_MASK request on the output stream -*/
static void
setEventMask(conn, imid, icid, forward_mask, synchronous_mask)
IMConnection *conn;
unsigned int imid;
unsigned int icid;
unsigned long forward_mask;
unsigned long synchronous_mask;
{
    (void)IMPutHeader(conn, XIM_SET_EVENT_MASK, 0, 12);
    IMPutC16(conn, imid);
    IMPutC16(conn, icid);
    IMPutC32(conn, forward_mask);
    IMPutC32(conn, synchronous_mask);
    IMSchedule(conn, SCHED_WRITE);
}

/*- ximOpenProc: handle XIM_OPEN request -*/
/* ARGSUSED */
static void
ximOpenProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    int locale_len;
    char localename[256 + 1];
    int offset;
    IMConverter *icp;
    IMIM *imp;

    TRACE(("ximOpenProc(#%d)\n", conn->serial));

    if (arglen < 1) {
	DPRINT(("ximOpenProc: arglen < 1\n"));
	IMSendBadLength(conn, 0, 0);
	return;
    }

    locale_len = IMGetC8(conn, 0);
    if (arglen < locale_len + 1) {
	DPRINT(("ximOpenProc: locale too long\n"));
	IMSendBadLength(conn, 0, 0);
	return;
    }

    bcopy(IMBUFDATA(IM_INBUF(conn)) + 1, localename, locale_len);
    localename[locale_len] = '\0';
    TRACE(("\tlocalename: %s\n", localename));

    if ((icp = IMGetConverter(conn->proto_widget, localename)) == NULL) {
	DPRINT(("ximOpenProc: unsupported locale %s\n", localename));
	IMSendError(conn, IMLocaleNotSupported, 0, 0, "unknown locale");
	return;
    }

    imp = IMCreateIM(conn, icp);

    /*
     * Send open reply message.
     */
    offset = IMPutHeader(conn, XIM_OPEN_REPLY, 0, 0);
    IMPutC16(conn, imp->id);
    IMPutIMAttrList(imp);
    IMPutICAttrList(imp);
    IMFinishRequest(conn, offset);

    /*
     * Notify conversion trigger keys.
     *
     * Note:
     * The spec says that XIM_REGISTER_TRIGGERKEYS message should
     * be sent to the client bofore XIM_OPEN_REPLY message, in
     * order to use dynamic event flow model.  However, this
     * does not work with current Xlib implementation.  So we
     * send XIM_REGISTER_TRIGGERKEYS just after XIM_OPEN_REPLY,
     * which works fine.
     */
    offset = IMPutHeader(conn, XIM_REGISTER_TRIGGERKEYS, 0, 0);
    IMPutC16(conn, imp->id);
    IMPutC16(conn, 0);
    putTriggerkeyList(imp);
    IMFinishRequest(conn, offset);
}

/*- ximCloseProc: handle XIM_CLOSE request -*/
/* ARGSUSED */
static void
ximCloseProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp;

    TRACE(("ximCloseProc(#%d)\n", conn->serial));

    if ((imp = IMGetIM(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximCloseProc: arglen != 4\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    IMDestroyIM(imp);
    IMPutHeader(conn, XIM_CLOSE_REPLY, 0, 4);
    IMPutC16(conn, imp->id);
    IMPutPad(conn);
    IMSchedule(conn, SCHED_WRITE);
}

/*- ximTriggerNotifyProc: handle XIM_TRIGGER_NOTIFY request -*/
/* ARGSUSED */
static void
ximTriggerNotifyProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    int flag;
    int index;
    int num_triggers;

    TRACE(("ximTriggerNotifyProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;

    if (arglen != 16) {
	DPRINT(("ximTriggerNotifyProc: arglen != 16\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    flag = (int)IMGetC32(conn,  4);
    index = (int)IMGetC32(conn, 8);
    TRACE(("\tflag=%d, index=%d\n", flag, index));

    /*
     * Check flag and index.
     */

    (void)IMTriggerKeys(icp->im, &num_triggers);
    if (flag != 0 || index >= num_triggers) {
	/* invalid parameter */
	DPRINT(("ximTriggerNotifyProc: invalid trigger spec.\n"));
	IMSendError(conn, IMBadSomething, icp->im->id, icp->id,
		    "invalid trigger key specification");
	return;
    }

    /*
     * If we are already in conversion mode, do nothing special.
     */
    if (!(icp->state & IC_CONVERTING)) {
	/*
	 * Start conversion.
	 */
	if (IMStartConversion(icp) < 0) return;
	
	/*
	 * Send XIM_SET_EVENT_MASK to let the client forward the key events.
	 */
#ifdef notyet
	if (use_on_demand_synchronous_flow_control) {
	    setEventMask(conn, icp->im->id, icp->id,
			 KeyPressMask|KeyReleaseMask,
			 NoEventMask);
	} else {
	    setEventMask(conn, icp->im->id, icp->id,
			 KeyPressMask|KeyReleaseMask,
			 KeyPressMask|KeyReleaseMask);
	}
#else
	/*
	 * Use full synchronous method.
	 */
	TRACE(("\tuse full synchronous method\n"));
	setEventMask(conn, icp->im->id, icp->id,
		     KeyPressMask|KeyReleaseMask,
		     KeyPressMask|KeyReleaseMask);
#endif
    }

    /*
     * Send XIM_TRIGGER_NOTIFY_REPLY request.
     */
    (void)IMPutHeader(conn, XIM_TRIGGER_NOTIFY_REPLY, 0, 4);
    IMPutC16(conn, icp->im->id);
    IMPutC16(conn, icp->id);
    IMSchedule(conn, SCHED_WRITE);
}

/*- findCtext: look up COMPOUND_TEXT from encoding list -*/
static int
findCtext(conn, arglen)
IMConnection *conn;
int arglen;
{
    int offset, name_end;
    int encoding_index;
    unsigned int name_bytes;

    offset = 2;
    name_bytes = IMGetC16(conn, offset);
    offset += 2;
    name_end = offset + name_bytes;
    if (name_end > arglen) return -2;

    /*
     * Examine encodings listed by name.
     */
    encoding_index = 0;
    while (offset < name_end) {
	char str_buf[256 + 1];
	int str_len;

	str_len = IMGetC8(conn, offset++);
	IMGetString(conn, offset, str_buf, str_len);
	if (!strcmp(str_buf, "COMPOUND_TEXT")) {
	    return encoding_index;
	}
	offset += str_len;
	encoding_index++;
    }

    /*
     * Encodings listed by detailed data follow, but
     * I have no idea what they are.
     */
    return -1;
}

/*- ximEncodingNegotiationProc: handle XIM_ENCODING_NEGOTIATION request -*/
/* ARGSUSED */
static void
ximEncodingNegotiationProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp;
    int encoding_index;

    TRACE(("ximEncodingNegotiationProc(#%d)\n", conn->serial));

    if ((imp = IMGetIM(conn, arglen)) == NULL) return;

    if (arglen < 8) {
	DPRINT(("ximEncodingNegotiationProc: arglen < 8\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    /*
     * Always use COMPOUND_TEXT.
     */
    if ((encoding_index = findCtext(conn, arglen)) < -1) {
	/* bad argument error */
	DPRINT(("ximEncodingNegotiationProc: encoding list corrupted\n"));
	IMSendError(conn, IMBadSomething, imp->id, 0,
		    "corrupted argument");
	return;
    }
    TRACE(("\tselected encoding index: %d\n", encoding_index));

    (void)IMPutHeader(conn, XIM_ENCODING_NEGOTIATION_REPLY, 0, 8);
    IMPutC16(conn, imp->id);
    IMPutC16(conn, 0);
    IMPutI16(conn, encoding_index);
    IMPutPad(conn);
    IMSchedule(conn, SCHED_WRITE);
}

/*- getExtension: search named extension -*/
static IMRequest *
getExtension(name)
char *name;
{
    IMRequest *req = extRequestTbl;

    while (req->name != NULL) {
	if (!strcmp(req->name, name)) return req;
	req++;
    }
    return NULL;
}

/*- putExtension: put extension information on the output stream -*/
static void
putExtension(conn, req)
IMConnection *conn;
IMRequest *req;
{
    IMPutC8(conn, req->major);
    IMPutC8(conn, req->minor);
    IMPutC16(conn, (unsigned int)strlen(req->name));
    IMPutPad(conn);
}

/*- ximQueryExtensionProc: handle XIM_QUERY_EXTENSION request -*/
/* ARGSUSED */
static void
ximQueryExtensionProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp;
    int ext_len;
    int cur_off;
    int req_offset;
    int size_offset;
    int list_start, list_end;

    TRACE(("ximQueryExtensionProc(#%d)\n", conn->serial));

    if ((imp = IMGetIM(conn, arglen)) == NULL) return;
    if (arglen < 4) {
	DPRINT(("ximQueryExtensionProc: arglen < 4\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }
    ext_len = IMGetC16(conn, 2);
    if (4 + ext_len > arglen) {
	DPRINT(("ximQueryExtensionProc: extension list too long\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }
    cur_off = 4;

    req_offset = IMPutHeader(conn, XIM_QUERY_EXTENSION_REPLY, 0, 0);
    IMPutC16(conn, imp->id);
    size_offset = IMWritePos(conn);
    IMPutC16(conn, 0);		/* dummy. overwritten afterwards */

    list_start = IMWritePos(conn);

    if (ext_len == 0) {
	IMRequest *ext_req = extRequestTbl;

	while (ext_req->name != NULL) {
	    putExtension(conn, ext_req);
	    ext_req++;
	}

    } else {
	IMRequest *ext_req;

	while (ext_len > 1) {
	    int str_len;
	    char ext_name[256 + 1];

	    str_len = IMGetC8(conn, cur_off);
	    cur_off++;

	    if (str_len + 1 > ext_len) {
		DPRINT(("ximQueryExtensionProc: extension name too long\n"));
		IMCancelRequest(conn, req_offset);
		/* BadString */
		IMSendBadLength(conn, imp->id, 0);
		return;
	    }

	    IMGetString(conn, cur_off, ext_name, str_len);
	    TRACE(("\tclient queries %s extension\n", ext_name));

	    if ((ext_req = getExtension(ext_name)) != NULL) {
		putExtension(conn, ext_req);
		imp->mask |= ext_req->mask;
	    }
	    cur_off += str_len;
	    ext_len -= str_len + 1;
	}
    }

    list_end = IMWritePos(conn);
    IMRewriteC16(conn, size_offset, (unsigned int)(list_end - list_start));

    IMFinishRequest(conn, req_offset);
}

/*- ximSetIMValuesProc: handle XIM_SET_IM_VALUES request -*/
/* ARGSUSED */
static void
ximSetIMValuesProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp;
    char *attr;
    int attr_len;

    TRACE(("ximSetIMValuesProc(#%d)\n", conn->serial));

    if ((imp = IMGetIM(conn, arglen)) == NULL) return;
    if (arglen < 4) {
	DPRINT(("ximSetIMValuesProc: arglen < 4\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    attr_len = IMGetC16(conn, 2);
    attr = IMBUFDATA(IM_INBUF(conn)) + 4;

    if (arglen < attr_len + 4) {
	DPRINT(("ximSetIMValuesProc: attribute len too long\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    if (IMSetIMValues(imp, attr, attr_len, major) < 0) return;

    (void)IMPutHeader(conn, XIM_SET_IM_VALUES_REPLY, 0, 4);
    IMPutC16(conn, imp->id);
    IMPutPad(conn);
    IMSchedule(conn, SCHED_WRITE);
}

/*- ximGetIMValuesProc: handle XIM_GET_IM_VALUES request -*/
/* ARGSUSED */
static void
ximGetIMValuesProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp;
    char *attr;
    int attr_len;
    int offset;

    TRACE(("ximGetIMValuesProc(#%d)\n", conn->serial));

    if ((imp = IMGetIM(conn, arglen)) == NULL) return;
    if (arglen < 4) {
	DPRINT(("ximGetIMValuesProc: arglen < 4\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    attr_len = IMGetC16(conn, 2);
    attr = IMBUFDATA(IM_INBUF(conn)) + 4;
    if (arglen < attr_len + 4 || attr_len % 2 == 1) {
	DPRINT(("ximGetIMValuesProc: attr_len too long or an odd number\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    offset = IMPutHeader(conn, XIM_GET_IM_VALUES_REPLY, 0, 0);
    IMPutC16(conn, imp->id);

    if (IMGetIMValues(imp, attr, attr_len, offset) < 0) return;

    IMFinishRequest(conn, offset);
}

/*- ximCreateICProc: handle XIM_CREATE_IC request -*/
/* ARGSUSED */
static void
ximCreateICProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIM *imp;
    IMIC *icp;
    char *attr_data;
    int attr_len;

    TRACE(("ximCreateICProc(#%d)\n", conn->serial));

    if ((imp = IMGetIM(conn, arglen)) == NULL) return;
    if (arglen < 4) {
	/* BadArglen */
	DPRINT(("ximCreateICProc: arglen < 4\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }

    attr_len = IMGetC16(conn, 2);
    attr_data = IMBUFDATA(IM_INBUF(conn)) + 4;
    if (arglen < attr_len + 4) {
	DPRINT(("ximCreateICProc: attr_len too long\n"));
	IMSendBadLength(conn, imp->id, 0);
	return;
    }
    icp = IMCreateIC(imp);
    if (IMSetICValues(icp, attr_data, attr_len, major) < 0) {
	IMDestroyIC(icp);
	return;
    }
    IMSendRequestWithIC(conn, XIM_CREATE_IC_REPLY, 0, icp);
}

/*- ximDestroyICProc: handle XIM_DESTROY_IC request -*/
/* ARGSUSED */
static void
ximDestroyICProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;

    TRACE(("ximDestroyICProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximDestroyICProc: arglen != 4\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }
    IMDestroyIC(icp);
    IMSendRequestWithIC(conn, XIM_DESTROY_IC_REPLY, 0, icp);
}

/*- ximSetICValuesProc: handle XIM_SET_IC_VALUES request -*/
/* ARGSUSED */
static void
ximSetICValuesProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    char *attr;
    int attr_len;

    TRACE(("ximSetICValuesProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen < 8) {
	DPRINT(("ximSetICValuesProc: arglen < 8\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    attr_len = IMGetC16(conn, 4);
    attr = IMBUFDATA(IM_INBUF(conn)) + 8;

    if (arglen < attr_len + 8) {
	DPRINT(("ximSetICValuesProc: attr_len too long\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    if (IMSetICValues(icp, attr, attr_len, major) < 0) return;

    IMSendRequestWithIC(conn, XIM_SET_IC_VALUES_REPLY, 0, icp);
}

/*- ximGetICValuesProc: handle XIM_GET_IC_VALUES request -*/
/* ARGSUSED */
static void
ximGetICValuesProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    char *attr;
    int attr_len;
    int offset;

    TRACE(("ximGetICValuesProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen < 6) {
	DPRINT(("ximGetICValuesProc: arglen < 6\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    attr_len = IMGetC16(conn, 4);
    attr = IMBUFDATA(IM_INBUF(conn)) + 6;
    if (arglen < attr_len + 6 || attr_len % 2 == 1) {
	DPRINT(("ximGetICValuesProc: attr_len too long or an odd number\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    offset = IMPutHeader(conn, XIM_GET_IC_VALUES_REPLY, 0, 0);
    IMPutC16(conn, icp->im->id);
    IMPutC16(conn, icp->id);

    if (IMGetICValues(icp, attr, attr_len, offset) < 0) return;

    IMFinishRequest(conn, offset);
}

/*- ximSetICFocusProc: handle XIM_SET_IC_FOCUS request -*/
/* ARGSUSED */
static void
ximSetICFocusProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;

    TRACE(("ximSetICFocusProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximSetICFocusProc: arglen != 4\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    IMSetFocus(icp);
}

/*- ximUnsetICFocusProc: handle XIM_UNSET_IC_FOCUS request -*/
/* ARGSUSED */
static void
ximUnsetICFocusProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;

    TRACE(("ximUnsetICFocusProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximUnsetICFocusProc: arglen != 4\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    IMUnsetFocus(icp);
}

/*- swapData: do byte swapping -*/
static void
swapData(data, format)
char *data;
char *format;
{
    unsigned char *p = (unsigned char *)data;
    int x;
    int f;

#define SWAPB(b1, b2)	x = b1; b1 = b2; b2 = x
    while ((f = *format++) != '\0') {
	switch (f) {
	case 'x':
	    p++;
	    break;
	case 'l':
	    SWAPB(p[0], p[3]);
	    SWAPB(p[1], p[2]);
	    p += 4;
	    break;
	case 's':
	    SWAPB(p[0], p[1]);
	    p += 2;
	    break;
	}
    }
#undef SWAPB
}


/*- getEvent: read a wire event and convert it to XEvent format -*/
static int
getEvent(conn, serial, offset, ev)
IMConnection *conn;
unsigned int serial;
int offset;
XEvent *ev;
{
    IMBuffer *ibp = IM_INBUF(conn);
    xEvent wire_event;
    int swap_needed;
    int one = 1;
    char *onep = (char *)&one;

    /*
     * Get wire event from input buffer.
     */
    bcopy(IMBUFDATA(ibp) + offset, (char *)&wire_event, sizeof(wire_event));

    /*
     * Do we need byte swapping?
     */
    if (*onep == 0) {	/* big endian */
	swap_needed = (conn->byte_order == ORDER_LITTLE);
    } else {		/* little endian */
	swap_needed = (conn->byte_order == ORDER_BIG);
    }

#define SWAPEV(format)	if (swap_needed) swapData((char *)&wire_event, format)

    /*
     * Get common part of the wire event.
     */
    SWAPEV("xxs");
    ev->type = wire_event.u.u.type & 0x7f;
    ev->xany.serial = (serial << 16) | wire_event.u.u.sequenceNumber;
    ev->xany.display = XtDisplay(conn->proto_widget);
    ev->xany.send_event = wire_event.u.u.type > 127;

    /*
     * Convert wire format to XEvent format.
     */
    switch (ev->type) {
    case KeyPress:
    case KeyRelease:
	SWAPEV("xxxxllllsssssxx");
#define WIRE	wire_event.u.keyButtonPointer
	ev->xkey.window = WIRE.event;
	ev->xkey.root = WIRE.root;
	ev->xkey.subwindow = WIRE.child;
	ev->xkey.time =  WIRE.time;
	ev->xkey.x = cvtINT16toInt(WIRE.eventX);
	ev->xkey.y = cvtINT16toInt(WIRE.eventY);
	ev->xkey.x_root = cvtINT16toInt(WIRE.rootX);
	ev->xkey.y_root = cvtINT16toInt(WIRE.rootY);
	ev->xkey.state = WIRE.state;
	ev->xkey.keycode = wire_event.u.u.detail;
	ev->xkey.same_screen = WIRE.sameScreen;
#undef WIRE
	break;
    case ButtonPress:
    case ButtonRelease:
	SWAPEV("xxxxllllsssssxx");
#define WIRE	wire_event.u.keyButtonPointer
	ev->xbutton.window = WIRE.event;
	ev->xbutton.root = WIRE.root;
	ev->xbutton.subwindow = WIRE.child;
	ev->xbutton.time = WIRE.time;
	ev->xbutton.x = cvtINT16toInt(WIRE.eventX);
	ev->xbutton.y = cvtINT16toInt(WIRE.eventY);
	ev->xbutton.x_root = cvtINT16toInt(WIRE.rootX);
	ev->xbutton.y_root = cvtINT16toInt(WIRE.rootY);
	ev->xbutton.state = WIRE.state;
	ev->xbutton.button = wire_event.u.u.detail;
	ev->xbutton.same_screen = WIRE.sameScreen;
#undef WIRE
	break;
    case MotionNotify:
	SWAPEV("xxxxllllsssssxx");
#define WIRE	wire_event.u.keyButtonPointer
	ev->xmotion.window = WIRE.event;
	ev->xmotion.root = WIRE.root;
	ev->xmotion.subwindow = WIRE.child;
	ev->xmotion.time = WIRE.time;
	ev->xmotion.x = cvtINT16toInt(WIRE.eventX);
	ev->xmotion.y = cvtINT16toInt(WIRE.eventY);
	ev->xmotion.x_root = cvtINT16toInt(WIRE.rootX);
	ev->xmotion.y_root = cvtINT16toInt(WIRE.rootY);
	ev->xmotion.state = WIRE.state;
	ev->xmotion.is_hint = wire_event.u.u.detail;
	ev->xmotion.same_screen = WIRE.sameScreen;
#undef WIRE
	break;
    default:
	/*
	 * For now, this function deals only Key/Pointer events.
	 */
	return 0;
    }
#undef SWAPEV

    return 1;		/* success */
}

/*- postponeEvent: record events for delayed processing -*/
static void
postponeEvent(icp, synchronous, ev)
IMIC *icp;
int synchronous;
XEvent *ev;
{
    IMPendingEvent *pending;

    pending = XtNew(IMPendingEvent);
    pending->ic = icp;
    pending->synchronous = synchronous;
    bcopy((char *)ev, (char *)&pending->event, sizeof(XEvent));
    pending->next = icp->pending_events;
    icp->pending_events = pending;
}

/*- handleForwardedEvent: deal with forwarded events -*/
static void
handleForwardedEvent(icp, synchronous, ev)
IMIC *icp;
int synchronous;
XEvent *ev;
{
    IMConnection *conn = icp->im->connection;

    TRACE(("handleForwardedEvent()\n"));

    if (icp->state & IC_SYNCING) {
	/*
	 * We are waiting for SYNC_REPLY message.
	 * Postpone event processing.
	 */
	DDPRINT(2, ("forward event processing suspended\n"));
	postponeEvent(icp, synchronous, ev);
	return;
    }

    IMForwardEvent(icp, ev);

#ifdef notyet
    if (synchronous) {
	if (!(icp->state & IC_CONVERTING)) {
	    /*
	     * Stop forwarding key events.
	     */
	    setEventMask(conn, icp->im->id, icp->id, 0L, 0L);
	}
	/*
	 * All the processing is done.  Send XIM_SYNC_REPLY.
	 */
	IMSendRequestWithIC(conn, XIM_SYNC_REPLY, 0, icp);
    } else {
	int cur_pos2;

	cur_pos2 = IMWritePos(conn);
	if (!(icp->state & IC_CONVERTING)) {
	    /*
	     * Stop forwarding key events.
	     */
	    setEventMask(conn, icp->im->id, icp->id, 0L, 0L);
	    /*
	     * Put dummy commit request (for setting sync flag).
	     */
	}
    }
#else
    /*
     * Currently, kinput2 supports only full synchronous method.
     */
    if (!(icp->state & IC_CONVERTING)) {
	/*
	 * Stop forwarding key events.
	 */
	setEventMask(conn, icp->im->id, icp->id, 0L, 0L);
    }
    /*
     * All the processing is done.  Send XIM_SYNC_REPLY.
     */
    IMSendRequestWithIC(conn, XIM_SYNC_REPLY, 0, icp);
#endif
}

/*- ximForwardEventProc: handle XIM_FORWARD_EVENT request -*/
/* ARGSUSED */
static void
ximForwardEventProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    int synchronous;
    unsigned int serial;
    XEvent event;

    TRACE(("ximForwardEventProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 40) {
	DPRINT(("ximForwardEventProc: arglen != 40\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    synchronous = IMGetC16(conn, 4) & 1;
    serial = IMGetC16(conn, 6);
 
    if (getEvent(conn, serial, 8, &event)) {
	handleForwardedEvent(icp, synchronous, &event);
    }
}

/*- ximExtForwardKeyeventProc: handle XIM_EXT_FORWARD_KEYEVENT_MASK request -*/
/* ARGSUSED */
static void
ximExtForwardKeyeventProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    int synchronous;
    int type;
    unsigned int serial;
    XEvent event;

    TRACE(("ximExtForwardKeyeventProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;

    if (!(icp->im->mask & XIM_EXT_FORWARD_KEYEVENT_MASK)) {
	DPRINT(("ximExtForwardKeyeventProc: ext_forward_keyevent disabled\n"));
	IMSendBadProtocol(conn, "ext_forward_keyevent extension not enabled");
	return;
    }

    if (arglen != 16) {
	DPRINT(("ximExtForwardKeyeventProc: arglen != 16\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }

    synchronous = IMGetC16(conn, 4) & 1;
    serial = IMGetC16(conn, 6);
    type = IMGetC8(conn, 8);
    if (type != KeyPress && type != KeyRelease) {
	/* Badtype */
	IMSendError(conn, IMBadSomething, icp->im->id, icp->id,
		    "forwarded event is invalid type");
	return;
    }

    event.type = type;
    event.xkey.serial = serial;			/* ??? */
    event.xkey.keycode = IMGetC8(conn, 9);
    event.xkey.state = IMGetC16(conn, 10);
    event.xkey.time =  IMGetC32(conn, 12);
    event.xkey.window = IMGetC32(conn, 16);
    event.xkey.root = icp->client_profile.root;
    event.xkey.subwindow = None;
    event.xkey.x = 0;
    event.xkey.y = 0;
    event.xkey.x_root = 0;
    event.xkey.y_root = 0;
    event.xkey.same_screen = True;

    handleForwardedEvent(icp, synchronous, &event);
}

/*- ximSyncProc: handle XIM_SYNC request -*/
/* ARGSUSED */
static void
ximSyncProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;

    TRACE(("ximSyncProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximSyncProc: arglen != 4\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }
    IMSendRequestWithIC(conn, XIM_SYNC_REPLY, 0, icp);
}

/*- processPendingEvents: process pending events forwarded by the client -*/
static void
processPendingEvents(icp, pending_event)
IMIC *icp;
IMPendingEvent *pending_event;
{
    /*
     * Pending events list is in reverse order.
     * Processing is done from the tail of the list, using
     * recursive call.
     */
    if (pending_event == NULL) return;
    processPendingEvents(icp, pending_event->next);
    pending_event->next = NULL;
    handleForwardedEvent(icp, pending_event->synchronous,
			 &pending_event->event);
    XtFree((char *)pending_event);
}

/*- ximSyncReplyProc: handle XIM_SYNC_REPLY message -*/
/* ARGSUSED */
static void
ximSyncReplyProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;

    TRACE(("ximSyncReplyProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximSyncReplyProc: arglen != 4\n"));
	return;
    }
    if (icp->state & IC_SYNCING) {
	icp->state &= ~IC_SYNCING;
	processPendingEvents(icp, icp->pending_events);
    }
}

/*- ximResetICProc: handle XIM_RESET_IC message -*/
/* ARGSUSED */
static void
ximResetICProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    char *text = NULL;
    int offset;
    int text_length;

    TRACE(("ximResetICProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    if (arglen != 4) {
	DPRINT(("ximResetICProc: arglen != 4\n"));
	IMSendBadLength(conn, icp->im->id, icp->id);
	return;
    }
    text_length = IMResetIC(icp, &text);

    offset = IMPutHeader(conn, XIM_RESET_IC_REPLY, 0, 0);
    IMPutC16(conn, icp->im->id);
    IMPutC16(conn, icp->id);
    IMPutC16(conn, (unsigned int)text_length);
    if (text_length > 0) IMPutString(conn, text, text_length);
    IMFinishRequest(conn, offset);
    if (text != NULL) XtFree(text);
}

/*- ximExtMoveProc: handle XIM_EXT_MOVE request -*/
/* ARGSUSED */
static void
ximExtMoveProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    IMIC *icp;
    int x, y;

    TRACE(("ximExtMoveProc(#%d)\n", conn->serial));

    if ((icp = IMGetIC(conn, arglen)) == NULL) return;
    x = IMGetI16(conn, 4);
    y = IMGetI16(conn, 6);
    IMMoveLocation(icp, x, y);
}

/*- ximAlreadyConnectedProc: handle duplicate XIM_CONNECT request -*/
/* ARGSUSED */
static void
ximAlreadyConnectedProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    TRACE(("ximAlreadyConnectedProc(#%d)\n", conn->serial));

    DPRINT(("connection #%d sent multiple XIM_CONNECT request\n",
	    conn->serial));
    IMSendBadProtocol(conn, "duplicate XIM_CONNECT message");
}

/*- ximShouldNotBeSentProc: handle requests which clients should not send -*/
/* ARGSUSED */
static void
ximShouldNotBeSentProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    TRACE(("ximShouldNotBeSentProc(#%d)\n", conn->serial));

    DPRINT(("connection #%d sent invalid request\n", conn->serial))
    IMSendBadProtocol(conn, "Clients should not send this request");
}

/*- ximAuthPhaseProc: handle requests for authentication -*/
/* ARGSUSED */
static void
ximAuthPhaseProc(conn, major, minor, arglen)
IMConnection *conn;
int major;
int minor;
int arglen;
{
    TRACE(("ximAuthPhaseProc(#%d)\n", conn->serial));

    DPRINT(("connection #%d sends auth-related request\n", conn->serial));
    IMSendBadProtocol(conn, "not authentication phase");
}

/*
 * Public Function
 */

/*- IMCompileReq: initialize IMMainDispatchTable -*/
void
IMCompileReq()
{
    static int compiled = 0;

    if (compiled) return;
    compileTbl(coreRequestTbl);
    compileTbl(extRequestTbl);
    compiled = 1;
}
