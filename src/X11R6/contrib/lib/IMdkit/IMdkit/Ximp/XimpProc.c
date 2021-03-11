/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpProc.c	1.8 94/02/16 */
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

#ifndef	_XIMPPROC_C_
#define	_XIMPPROC_C_
#include <X11/Xatom.h>
#include "XimpData.h"

#if NeedFunctionPrototypes
extern Bool _XimpGetProperty(XIMPCore, XimpClient *,
			     XIMPICValuesStruct *);
extern Bool _XimpSetProperty(XIMPCore, XimpClient *,
			     XIMPICValuesStruct *);
extern void _XimpSendIMProtocol(XIMPCore, XimpClient *,
				int, unsigned long, unsigned long,
				unsigned long, unsigned long);
extern Atom _XimpReplacePropPool(XIMPCore, char *, int);
extern XimpClient *_XimpFindClient(XIMPCore, CARD32, IMPProtocol*);
extern XimpClient *_XimpNewClient(XIMPCore, XClientMessageEvent *);
extern void _XimpDeleteClient(XIMPCore, CARD32);
extern void _XimpSendByClientMessage(XIMPCore, long, char *, int);
extern void _XimpRegisterDestroyFilter(XIMS, XimpClient *);
extern void _XimpRegisterKeyPressFilter(XIMS, XimpClient *);
extern void _XimpRegisterKeyReleaseFilter(XIMS, XimpClient *);
extern void _XimpUnregisterDestroyFilter(XIMS, XimpClient *);
extern void _XimpUnregisterKeyPressFilter(XIMS, XimpClient *);
extern void _XimpUnregisterKeyReleaseFilter(XIMS, XimpClient *);
#else
extern Bool _XimpGetProperty();
extern Bool _XimpSetProperty();
extern void _XimpSendIMProtocol();
extern Atom _XimpReplacePropPool();
extern XimpClient *_XimpFindClient();
extern XimpClient *_XimpNewClient();
extern void _XimpDeleteClient();
extern void _XimpSendByClientMessage();
extern void _XimpRegisterDestroyFilter();
extern void _XimpRegisterKeyPressFilter();
extern void _XimpRegisterKeyReleaseFilter();
extern void _XimpUnregisterDestroyFilter();
extern void _XimpUnregisterKeyPressFilter();
extern void _XimpUnregisterKeyReleaseFilter();
#endif

static Bool
#if NeedFunctionPrototypes
IsConversionKey(Display *display, unsigned int kcode, unsigned int kstate,
		int count, XIMTriggerKey *trigger)
#else
IsConversionKey(display, kcode, kstate, count, trigger)
Display *display;
unsigned int kcode;
unsigned int kstate;
int count;
XIMTriggerKey *trigger;
#endif
{
    register int i;
    KeySym ksym = XKeycodeToKeysym(display, kcode, 0);

    for (i = 0; i < count; i++) {
	if ((trigger[i].keysym == ksym) &&
	    ((kstate & trigger[i].modifier_mask) == trigger[i].modifier)) {
	    return True;
	}
    }
    return False;
}

static Bool
#if NeedFunctionPrototypes
CheckAltKey(XIMPCore core, XimpClient *client,
	    XClientMessageEvent *ev)
#else
CheckAltKey(core, client, ev)
XIMPCore core;
XimpClient *client;
XClientMessageEvent *ev;
#endif
{
    Display *display = core->display;
    unsigned int kcode = ev->data.l[2];
    unsigned int kstate = ev->data.l[3];

    if (IS_VERSION_40(client)) {
	switch (client->ev_flow_type) {
	  case XIMP_FE_TYPE1:
	  case XIMP_BE_TYPE1:
	    return IsConversionKey(display, kcode, kstate,
				   core->stop_keys.count_keys,
				   core->stop_keys.keylist);
	  default:
	    return False;
	}
    } else {
	return IsConversionKey(display, kcode, kstate,
			       core->stop_keys.count_keys,
			       core->stop_keys.keylist);
    }
}

Bool
#if NeedFunctionPrototypes
_XimpProcKeyPress(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcKeyPress(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPKeyEventStruct	*keypress =
      (XIMPKeyEventStruct*)&(call_data.keyevent);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (CheckAltKey(core, client, ev)) {
	ev->data.l[0] = XIMP_END3; /* dummy */
	return _XimpProcEnd(xims, ev);
    }
    keypress->type	= ev->data.l[0];
    keypress->icid	= ev->data.l[1];
    keypress->keycode	= ev->data.l[2];
    keypress->state	= ev->data.l[3];
    keypress->time = (IS_VERSION_40(client)?
		      ev->data.l[4] : 0);
    if (core->improto) (core->improto)(xims, &call_data);
    if (IS_VERSION_40(client) &&
	(client->ev_flow_type & XIMP_SYNC)) {
	_XimpSetMatch(client,
		      keypress->icid, keypress->keycode, keypress->state);
	_XimpSendIMProtocol(core, client,
			    XIMP_KEYPRESS_RETURN4,
			    keypress->icid,
			    0, 0, 0);
    }
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcKeyRelease(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcKeyRelease(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPKeyEventStruct *keyrelease =
      (XIMPKeyEventStruct*)&(call_data.keyevent);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (!IS_VERSION_40(client)) return False;

    if (CheckAltKey(core, client, ev)) {
	ev->data.l[0] = XIMP_END3; /* dummy */
	return _XimpProcEnd(xims, ev);
    }
    keyrelease->type	= ev->data.l[0];
    keyrelease->icid	= ev->data.l[1];
    keyrelease->keycode	= ev->data.l[2];
    keyrelease->state	= ev->data.l[3];
    keyrelease->time	= ev->data.l[4];

    if (core->improto) (core->improto)(xims, &call_data);

    if (client->ev_flow_type & XIMP_SYNC &&
	_XimpProcessMatch(client, keyrelease->icid,
			  keyrelease->keycode, keyrelease->state)) {
	_XimpSendIMProtocol(core, client,
			    XIMP_KEYRELEASE_RETURN4,
			    keyrelease->icid,
			    0, 0, 0);
    }
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcCreate(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcCreate(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPICValuesStruct *create =
      (XIMPICValuesStruct*)&(call_data.create);
    XimpClient *client;

    if ((client = (XimpClient *)_XimpNewClient(core, ev))
	== NULL) return False;

    memset(&call_data, 0, sizeof(IMPProtocol));
    create->type = ev->data.l[0];
    create->icid = client->id;
    create->fwin_sel_mask = client->ev_masks;
    create->client_win = ev->data.l[1];
    create->input_style = ev->data.l[2];
    create->attr_mask = ev->data.l[3];
    create->pre_values = (Ximp_PreeditPropRec4*)
      malloc(sizeof(Ximp_PreeditPropRec4));
    create->sts_values = (Ximp_StatusPropRec4*)
      malloc(sizeof(Ximp_StatusPropRec4));

    if (!_XimpGetProperty(core, client, create)) {
	/*
	 * Call XIMP_ERROR because of lack of properties
	 */
    }
    if (IS_VERSION_40(client))
      client->ev_flow_type = create->ximp_type_mask;
    /*
     * When focus window setting is deferred, use client window
     * as a fallback default.
     */
    if (!(create->focus_win)) {
	create->focus_win = create->client_win;
    }
    if (core->improto) (core->improto)(xims, &call_data);

    XFree(create->pre_values);
    XFree(create->sts_values);

    if ((IS_VERSION_40(client) && create->attr_mask & XIMP_FOCUS_WIN_MASK4) ||
	(!IS_VERSION_40(client) && create->attr_mask & XIMP_FOCUS_WIN_MASK3))
      client->focus_window = create->focus_win;

    /* register destroy event filter on focus window */
    _XimpRegisterDestroyFilter(xims, client);

    /* If FrontEnd Type2 or Type3, attach filters to focus window. */
    if (IS_VERSION_40(client) &&
	((client->ev_flow_type == XIMP_FE_TYPE2) ||
	 (client->ev_flow_type == XIMP_FE_TYPE3))) {
	_XimpRegisterKeyPressFilter(xims, client);
	_XimpRegisterKeyReleaseFilter(xims, client);
    }
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_CREATE_RETURN4 :
			 XIMP_CREATE_RETURN3),
			create->icid,
			0, 0, 0);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcDestroy(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcDestroy(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPAnyStruct *destroy =
      (XIMPAnyStruct*)&(call_data.destroy);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    destroy->type = ev->data.l[0];
    destroy->icid = ev->data.l[1];
    if (core->improto) (core->improto)(xims, &call_data);

    _XimpDeleteClient(core, destroy->icid);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcStart(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcStart(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPAnyStruct *sprocstart
      = (XIMPAnyStruct*)&(call_data.regkey);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (client->is_conv_on == True) return True; /* just ignore */
    sprocstart->type = ev->data.l[0];
    sprocstart->icid = ev->data.l[1];
    if (core->improto) (core->improto)(xims, &call_data);

    return (xims->methods->preeditStart)(xims, &call_data);
}

Bool
#if NeedFunctionPrototypes
_XimpProcEnd(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcEnd(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPAnyStruct *sprocstop
      = (XIMPAnyStruct*)&(call_data.regkey);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (client->is_conv_on == False) return True; /* just ignore */
    sprocstop->type = ev->data.l[0];
    sprocstop->icid = ev->data.l[1];
    if (core->improto) (core->improto)(xims, &call_data);

    return (xims->methods->preeditEnd)(xims, &call_data);
}

Bool
#if NeedFunctionPrototypes
_XimpProcSetFocus(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcSetFocus(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPAnyStruct *setfocus
      = (XIMPAnyStruct*)&(call_data.setfocus);

    setfocus->type = ev->data.l[0];
    setfocus->icid = ev->data.l[1];

    if (core->improto) (core->improto)(xims, &call_data);

    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcUnsetFocus(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcUnsetFocus(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPAnyStruct *unsetfocus
      = (XIMPAnyStruct*)&(call_data.unsetfocus);

    unsetfocus->type = ev->data.l[0];
    unsetfocus->icid = ev->data.l[1];

    if (core->improto) (core->improto)(xims, &call_data);

    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcClientWin(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcClientWin(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol	call_data;
    XIMPClientWindowStruct *clientwin
      = (XIMPClientWindowStruct*)&(call_data.clientwin);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (!IS_VERSION_40(client)) return False;
    clientwin->type = ev->data.l[0];
    clientwin->icid = ev->data.l[1];
    clientwin->new_client_win = ev->data.l[2];
    clientwin->new_fwin_mask = ev->data.l[3];
    if (core->improto) (core->improto)(xims, &call_data);

    client->ev_masks = clientwin->new_fwin_mask;
    client->client_window = clientwin->new_client_win;
    _XimpSendIMProtocol(core, client,
			XIMP_CLIENT_WINDOW_RETURN4,
			clientwin->icid,
			0, 0, 0);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcFocusWin(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcFocusWin(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPFocusWindowStruct *focuswin =
      (XIMPFocusWindowStruct*)&(call_data.focuswin);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (!IS_VERSION_40(client)) return False;
    focuswin->type = ev->data.l[0];
    focuswin->icid = ev->data.l[1];
    focuswin->new_focus_win = ev->data.l[2];
    focuswin->new_fwin_mask = ev->data.l[3];
    if (core->improto) (core->improto)(xims, &call_data);

    client->ev_masks = focuswin->new_fwin_mask;
    client->focus_window = focuswin->new_focus_win;

    /* Change filters to new focus window. */
    _XimpUnregisterDestroyFilter(xims, client);
    _XimpUnregisterKeyPressFilter(xims, client);
    _XimpUnregisterKeyReleaseFilter(xims, client);
    _XimpRegisterDestroyFilter(xims, client);
    if (client->ev_flow_type == XIMP_FE_TYPE2 ||
	client->ev_flow_type == XIMP_FE_TYPE3 ||
	(client->ev_flow_type == XIMP_FE_TYPE1 && client->is_conv_on)) {
	_XimpRegisterKeyPressFilter(xims, client);
	_XimpRegisterKeyReleaseFilter(xims, client);
    }
    _XimpSendIMProtocol(core, client,
			XIMP_FOCUS_WINDOW_RETURN4,
			focuswin->icid,
			0, 0, 0);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcMove(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcMove(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore	core = (XIMPCore)xims->protocol;
    IMPProtocol	call_data;
    XIMPMoveStruct *move = (XIMPMoveStruct*)&(call_data.move);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    move->type	= ev->data.l[0];
    move->icid	= ev->data.l[1];
    move->x	= ev->data.l[2];
    move->y	= ev->data.l[3];
    if (core->improto) (core->improto)(xims, &call_data);

    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcSetValues(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcSetValues(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPICValuesStruct	*setvalue = (XIMPICValuesStruct*)&(call_data.setvalue);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    setvalue->type = ev->data.l[0];
    setvalue->icid = ev->data.l[1];
    setvalue->attr_mask = ev->data.l[2];
    setvalue->pre_values = (Ximp_PreeditPropRec4*)
      malloc(sizeof(Ximp_PreeditPropRec4));
    setvalue->sts_values = (Ximp_StatusPropRec4*)
      malloc(sizeof(Ximp_StatusPropRec4));

    if (IS_VERSION_40(client))
      setvalue->attr_mask &= ~(XIMP_FOCUS_WIN_MASK4|XIMP_SERVERTYPE_MASK4);

    if (!_XimpGetProperty(core, client, setvalue)) {
	/*
	 * Call XIMP_ERROR because of lack of properties
	 */
    }
    if (core->improto) (core->improto)(xims, &call_data);

    XFree(setvalue->pre_values);
    XFree(setvalue->sts_values);

    /* If focus window is chaged, change filters to new focus window. */
    if (!IS_VERSION_40(client) &&
	setvalue->attr_mask & XIMP_FOCUS_WIN_MASK3) {
	client->focus_window = setvalue->focus_win;
	_XimpUnregisterDestroyFilter(xims, client);
	_XimpUnregisterKeyPressFilter(xims, client);
	_XimpRegisterDestroyFilter(xims, client);
	if (client->ev_flow_type & XIMP_FRONTEND && client->is_conv_on) {
	    _XimpRegisterKeyPressFilter(xims, client);
	}
    }
    return False;
}

Bool
#if NeedFunctionPrototypes
_XimpProcGetValues(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcGetValues(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPICValuesStruct	*getvalue =
      (XIMPICValuesStruct*)&(call_data.getvalue);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    getvalue->type = ev->data.l[0];
    getvalue->icid = ev->data.l[1];
    getvalue->attr_mask = ev->data.l[2];
    if (core->improto) (core->improto)(xims, &call_data);

    if (!_XimpSetProperty(core, client, getvalue)) {
	/*
	 * Call XIMP_ERROR because of lack of properties
	 */
    } 
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_GETVALUE_RETURN4 :
			 XIMP_GETVALUE_RETURN3),
			getvalue->icid,
			0, 0, 0);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcReset(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcReset(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPResetStruct *reset = (XIMPResetStruct*)&(call_data.reset);
    CARD32 flag;
    int length;
    Atom atom_ret = (Atom)0;
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    reset->type = ev->data.l[0];
    reset->icid = ev->data.l[1];
    if (core->improto) (core->improto)(xims, &call_data);

    client->is_conv_on = False;
    if (IS_VERSION_40(client)) {
	_XimpSendIMProtocol(core, client,
			    XIMP_SPROC_STOPPED4,
			    reset->icid,
			    0, 0, 0);
	if (client->ev_flow_type == XIMP_FE_TYPE1 ||
	    client->ev_flow_type == XIMP_FE_TYPE3) {
	    _XimpUnregisterKeyPressFilter(xims, client);
	    _XimpUnregisterKeyReleaseFilter(xims, client);
	}
    } else {
	_XimpSendIMProtocol(core, client,
			    XIMP_PROCESS_END3,
			    reset->icid,
			    0, 0, 0);
	if (client->ev_flow_type == XIMP_FRONTEND) {
	    _XimpUnregisterKeyPressFilter(xims, client);
	}
    }

    /*
     * Only XIMP4.0 supports sending reset string by cmsg.
     */
    if (IS_VERSION_40(client)) {
	length = strlen(reset->ctext);
	if (!reset->ctext) {
	    flag = RESET_NOTHING;
	} else {
	    if (length <= LONG_ENOUGH) {
		_XimpSendByClientMessage(core,
					 reset->icid,
					 reset->ctext,
					 length);
		flag = RESET_BY_CMSG;
	    } else {
		atom_ret = _XimpReplacePropPool(core,
						reset->ctext,
						length);
		flag = RESET_BY_PROP;
	    }
	}
    } else {
	flag = atom_ret = 0;
    }
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_RESET_RETURN4 :
			 XIMP_RESET_RETURN3),
			reset->icid,
			(CARD32)flag, (CARD32)atom_ret, 0);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcEventMask(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcEventMask(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPEventMaskNotifyStruct *evmasknotify =
      (XIMPEventMaskNotifyStruct*)&(call_data.evmasknotify);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    if (!IS_VERSION_40(client)) return False;
    evmasknotify->type = ev->data.l[0];
    evmasknotify->icid = ev->data.l[1];
    evmasknotify->new_fwin_sel_mask = ev->data.l[2];
    if (core->improto) (core->improto)(xims, &call_data);

    client->ev_masks = evmasknotify->new_fwin_sel_mask;
    _XimpSendIMProtocol(core, client,
			XIMP_EVENTMASK_NOTIFY_RETURN4,
			evmasknotify->icid,
			0, 0, 0);
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcExtension(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcExtension(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPAtoms *atoms = (XIMPAtoms*)&core->atoms;
    IMPProtocol call_data;
    XIMPExtensionStruct	*extension =
      (XIMPExtensionStruct*)&(call_data.extension);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;
    extension->type = ev->data.l[0];
    extension->icid = ev->data.l[1];
    if (ev->data.l[2] == atoms->ext_statuswin) {
	if (core->ext_flag & EXT_STATUS) {
	    extension->ext_type = EXT_STATUS;
	    if (core->improto) (core->improto)(xims, &call_data);
	    return True;
	} else {
	    return False;		/* not supported */
	}
    }
    else if (ev->data.l[2] == atoms->ext_backfront) {
	if (core->ext_flag & EXT_BACKFRONT) {
	    extension->ext_type = EXT_BACKFRONT;
	    extension->todo.back_front = ev->data.l[3];
	    client->ev_flow_type = ev->data.l[3];
	    if (core->improto) core->improto(xims, &call_data);
	    return True;
	} else {
	    return False;		/* not supported */
	}
    }
    else if (ev->data.l[2] == atoms->ext_conversion) {
	if (core->ext_flag & EXT_CONV) {
	    extension->ext_type = EXT_CONV;
	    extension->todo.conversion.operation = ev->data.l[3];
	    extension->todo.conversion.mode = ev->data.l[4];
	    if (core->improto) core->improto(xims, &call_data);

	    if (extension->todo.conversion.operation == False) {
		/*
		 * Client wants to get the conversion status.
		 * Need to reply.
		 */
		if (client->is_conv_on !=
		    extension->todo.conversion.mode) {
		    /* Why? Anyhow, obey the IMserver. */
		    client->is_conv_on = extension->todo.conversion.mode;
		}
		_XimpSendIMProtocol(core, client,
				    (IS_VERSION_40(client) ?
				     XIMP_EXTENSION4 : XIMP_EXTENSION3),
				    extension->icid,
				    core->atoms.ext_conversion,
				    (CARD32)client->is_conv_on,
				    0);
	    } else {
		/*
		 * Client wants to set the conversion status.
		 * Need to obey.
		 */
		/* In FrontEnd Type1 of XIMP_40 */
		if (IS_VERSION_40(client) &&
		    client->ev_flow_type == XIMP_FE_TYPE1 &&
		    client->is_conv_on != extension->todo.conversion.mode) {
		    /* Obey the client */
		    client->is_conv_on = extension->todo.conversion.mode;
		    if (client->is_conv_on) {
			_XimpRegisterKeyPressFilter(xims, client);
			_XimpRegisterKeyReleaseFilter(xims, client);
		    } else {
			_XimpUnregisterKeyPressFilter(xims, client);
			_XimpUnregisterKeyReleaseFilter(xims, client);
		    }
		}
		/* In FrontEnd of XIMP_35 */
		if (!IS_VERSION_40(client) &&
		    client->ev_flow_type == XIMP_FRONTEND &&
		    client->is_conv_on != extension->todo.conversion.mode) {
		    /* Obey the client */
		    client->is_conv_on = extension->todo.conversion.mode;
		    if (client->is_conv_on) {
			_XimpRegisterKeyPressFilter(xims, client);
			_XimpRegisterKeyReleaseFilter(xims, client);
		    } else {
			_XimpUnregisterKeyPressFilter(xims, client);
			_XimpUnregisterKeyReleaseFilter(xims, client);
		    }
		}
	    }
	    return True;
	} else {
	    return False;		/* not supported */
	}
    }
    else {
	return False;	/* unknown extensions */
    }
}

Bool
#if NeedFunctionPrototypes
_XimpProcPreStartReturn(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcPreStartReturn(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&(call_data.preedit_cb);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;

    preedit->type = ev->data.l[0];
    preedit->icid = ev->data.l[1];
    preedit->todo.return_value = ev->data.l[2];

    if (core->improto) (core->improto)(xims, &call_data);

    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpProcPreCaretReturn(XIMS xims, XClientMessageEvent *ev)
#else
_XimpProcPreCaretReturn(xims, ev)
XIMS xims;
XClientMessageEvent *ev;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    IMPProtocol call_data;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&(call_data.preedit_cb);
    XimpClient *client = _XimpFindClient(core, (CARD32)ev->data.l[1],
					 &call_data);

    if (client == (XimpClient *)NULL) return False;

    preedit->type = ev->data.l[0];
    preedit->icid = ev->data.l[1];
    preedit->todo.caret.position = ev->data.l[2];

    if (core->improto) (core->improto)(xims, &call_data);

    return True;
}
#endif	/* _XIMPPROC_C_ */
