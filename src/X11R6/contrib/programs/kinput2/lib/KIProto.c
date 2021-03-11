#ifndef lint
static char *rcsid = "$Id: KIProto.c,v 1.46 1994/06/02 05:00:05 ishisone Rel $";
#endif
/*-
 * Copyright (c) 1991  Software Research Associates, Inc.
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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/CharSet.h>
#include "KIProtoP.h"
#include "ConvMgr.h"
#include "OverConv.h"
#include "OffConv.h"
#include "ConvDisp.h"
#include "MyDispatch.h"
#include "CachedFont.h"
#include "ConvProto.h"
#include "AsyncErr.h"

#define DEBUG_VAR debug_KinputProtocol
#include "DebugPrint.h"

#define JINPUT_PROTOCOL_VERSION 0x20000L	/* version 2 */

#define XLC_AUTO_REPLACE	1
#define XLC_ALL_INFORMATION	2
#define XLC_FRAME_INFORMATION	4
#define XLC_OFFSET_INFORMATION	8

/*- resource table -*/
static XtResource resources[] = {
#define offset(field) XtOffset(KinputProtocolWidget, kinput.field)
    { XtNlanguage, XtCLanguage, XtRString, sizeof(String),
	offset(language), XtRString, (XtPointer)NULL },
    { XtNinputObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(inputObjClass), XtRImmediate, (XtPointer)NULL },
    { XtNdisplayObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(displayObjClass), XtRImmediate, (XtPointer)NULL },
    { XtNbackwardCompatible, XtCBackwardCompatible, XtRBoolean, sizeof(Boolean),
	offset(backward_compatible), XtRString, (XtPointer)"False" },
    { XtNxlcConversionStartKey, XtCXlcConversionStartKey, XtRString, sizeof(String),
	offset(xlcstartkey), XtRString, (XtPointer)NULL },
#undef offset
};

static void ConversionRequestProc();
static void ConversionEndRequestProc();
static void ConversionOpenRequestProc();
static void ConversionCloseRequestProc();
static void ConversionCloseProc();
static void ConversionXYNotifyProc();
static void ConversionColorNotifyProc();
static void ConversionFontsNotifyProc();
static void ConversionAttributeNotifyProc();
static void SelectionRequestProc();
static void SelectionClearProc();
static void XlcOnTheSpotChangedProc();

/*- action table -*/
static XtActionsRec actions[] = {
    { "conversion-request",		ConversionRequestProc },
    { "conversion-end-request",		ConversionEndRequestProc },
    { "conversion-open-request",	ConversionOpenRequestProc },
    { "conversion-close-request",	ConversionCloseRequestProc },
    { "conversion-close",		ConversionCloseProc },
    { "conversion-xy-notify",		ConversionXYNotifyProc },
    { "conversion-color-notify",	ConversionColorNotifyProc },
    { "conversion-fonts-notify",	ConversionFontsNotifyProc },
    { "conversion-attribute-notify",	ConversionAttributeNotifyProc },
    { "xlc-on-the-spot-changed",	XlcOnTheSpotChangedProc },
    { "selection-request",		SelectionRequestProc },
    { "selection-clear",		SelectionClearProc },
};

/*- default translation -*/
static char translations[] =
    "<Message>CONVERSION_REQUEST:	conversion-request()\n\
     <Message>CONVERSION_END_REQUEST:	conversion-end-request()\n\
     <Message>CONVERSION_OPEN_REQUEST:	conversion-open-request()\n\
     <Message>CONVERSION_CLOSE_REQUEST:	conversion-close-request()\n\
     <Message>CONVERSION_CLOSE:		conversion-close()\n\
     <Message>CONVERSION_XY_NOTIFY:	conversion-xy-notify()\n\
     <Message>CONVERSION_COLOR_NOTIFY:	conversion-color-notify()\n\
     <Message>CONVERSION_FONTS_NOTIFY:	conversion-fonts-notify()\n\
     <Message>CONVERSION_ATTRIBUTE_NOTIFY: conversion-attribute-notify()\n\
     <Prop>_XLC_ON_THE_SPOT:	xlc-on-the-spot-changed()\n\
     <SelReq>:	selection-request()\n\
     <SelClr>:	selection-clear()";

/*- static function declarations -*/
static void Initialize(), Destroy();
static void Realize();
static Boolean SetValues();

static void getAtoms();
static Boolean ownSelection();

static ConvClient *findClient();
static ConvClient *newClient();
static Widget attachConverter();
static void detachConverter();
static void jinputDetach();
static void deleteClient();

static Boolean isCorrectClientEvent();
static Boolean isCorrectWindowID();

static void getJinputInitialProperty();
static void jinputSendReq();
static void jinputFreeResources();
static void kinput2FreeResources();
static void xlcFreeResources();
static void setAttribute();
static int getFontsByFontAtoms();
static int safeGetAttributeProperty();
static void getAttributeFromProperty1();
static void getAttributeFromProperty();
static void getAttributeFromEvent();
static ConvClient *getXlcDataFromProperty();
static void initializeError();
static int parseKeyEvent();
static char *mystrstr();
static void getDefaultFontHeight();

static void setJinputProperty();
static void setKinput2Property();
static void setXlcProperty();
static void setXlcStatusProperty();
static void setXlcBCKey();

static void sendClientMessage();
static void sendNegativeConversionNotify();
static void sendConversionNotify();
static void sendNegativeConversionOpenNotify();
static void sendConversionOpenNotify();
static void sendColorRequest();
static void sendFontRequest();
static void sendXYRequest();

static void fixCallback();
static void fixProc();
static void jinputFix();
static void endCallback();
static void endProc();
static void jinputEnd();
static void jinputNewTextCallback();
static void xlcEnd();

static void ClientDead();

/*- KinputProtocolClassRec -*/
KinputProtocolClassRec kinputProtocolClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"KinputProtocol",
    /* widget_size		*/	sizeof(KinputProtocolRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	NULL,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* kinputprotocol fields */
    /* empty			*/	0
  }
};

WidgetClass kinputProtocolWidgetClass = (WidgetClass)&kinputProtocolClassRec;

/*
 *+ Core class methods
 */

/*- Initialize: intern Atoms -*/
/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)new;

    if (kpw->kinput.language == NULL) {
	initializeError(new, XtNlanguage);
    } else if (kpw->kinput.inputObjClass == NULL) {
	initializeError(new, XtNinputObjectClass);
    } else if (kpw->kinput.displayObjClass == NULL) {
	initializeError(new, XtNdisplayObjectClass);
    }
    kpw->kinput.language = XtNewString(kpw->kinput.language);
    kpw->kinput.clients = (ConvClient *)NULL;
    getDefaultFontHeight(kpw);
    getAtoms(kpw);
}

/*- Destroy: free allocated memory -*/
static void
Destroy(w)
Widget w;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;

    XtFree(kpw->kinput.language);
}

/*- Realize: own selection -*/
static void
Realize(w, mask, value)
Widget w;
XtValueMask *mask;
XSetWindowAttributes *value;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    CoreWidgetClass super = (CoreWidgetClass)XtClass(w)->core_class.superclass;

    (*super->core_class.realize)(w, mask, value);

    setJinputProperty(kpw);
    setKinput2Property(kpw);
    setXlcProperty(kpw);

    if (!ownSelection(kpw)) {
	String params[1];
	Cardinal num_params;

	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext(w),
			"selectionError", "ownSelection", "WidgetError",
			"%s: can't own selection", params, &num_params);

	XtDestroyWidget(w);
    } else {
	CMPrepareConverter(XtParent(w), XtScreen(w),
			   separateConversionWidgetClass,
			   kpw->kinput.inputObjClass,
			   kpw->kinput.displayObjClass);
    }
}

/*- SetValues: not implemented yet -*/
/* ARGSUSED */
static Boolean
SetValues(cur, req, wid, args, num_args)
Widget cur;
Widget req;
Widget wid;
ArgList args;
Cardinal *num_args;
{
#ifdef notdef
    KinputProtocolWidget old = (KinputProtocolWidget)cur;
    KinputProtocolWidget new = (KinputProtocolWidget)wid;
    Boolean redisplay = False;
#endif

    return False;
}

/*
 *+ atom handling
 */

/*- getAtoms: intern atoms -*/
static void
getAtoms(kpw)
KinputProtocolWidget kpw;
{
    Display *dpy = XtDisplay((Widget)kpw);
    char buf[256];

    (void)sprintf(buf, "_%s_CONVERSION", kpw->kinput.language);
    kpw->kinput.convAtom = XInternAtom(dpy, buf, False);
    if (kpw->kinput.backward_compatible) {
	(void)sprintf(buf, "%s_CONVERSION", kpw->kinput.language);
	kpw->kinput.oldConvAtom = XInternAtom(dpy, buf, False);
    } else {
	kpw->kinput.oldConvAtom = None;
    }

    kpw->kinput.ctextAtom = XA_COMPOUND_TEXT(dpy);

#define MAKEATOM(s)	XInternAtom(dpy, s, False)

    kpw->kinput.convStringAtom = MAKEATOM("CONVERSION_STRING");
    kpw->kinput.convNotifyAtom = MAKEATOM("CONVERSION_NOTIFY");
    kpw->kinput.convEndAtom = MAKEATOM("CONVERSION_END");

    (void)sprintf(buf, "%s_CONVERSION_VERSION", kpw->kinput.language);
    kpw->kinput.convVersionAtom = XInternAtom(dpy, buf, False);
    kpw->kinput.convInitialTypeAtom = MAKEATOM("CONVERSION_INITIAL_TYPE");
    kpw->kinput.convOpenNotifyAtom = MAKEATOM("CONVERSION_OPEN_NOTIFY");
    kpw->kinput.convXYRequestAtom = MAKEATOM("CONVERSION_XY_REQUEST");
    kpw->kinput.convFontsRequestAtom = MAKEATOM("CONVERSION_FONTS_REQUEST");
    kpw->kinput.convColorRequestAtom = MAKEATOM("CONVERSION_COLOR_REQUEST");
    kpw->kinput.convCloseNotifyAtom = MAKEATOM("CONVERSION_CLOSE_NOTIFY");
    kpw->kinput.convAttributeAtom = MAKEATOM("CONVERSION_ATTRIBUTE");
    kpw->kinput.xlcStatusAtom = MAKEATOM("_XLC_STATUS");
    kpw->kinput.xlcOnTheSpotAtom = MAKEATOM("_XLC_ON_THE_SPOT");
    kpw->kinput.xlcBcModifierAtom = MAKEATOM("_XLC_BC_MODIFIER");
    kpw->kinput.xlcBcKeycodeAtom = MAKEATOM("_XLC_BC_KEYCODE");

    /*
     * some of the clients who speak Xlc protocol check the existence
     * of atom "CONVERSION_REQUEST" before starting conversion.
     * so make it at initialization time.
     */
    (void)MAKEATOM("CONVERSION_REQUEST");

#undef MAKEATOM
}

/*- ownSelection: own conversion selection -*/
static Boolean
ownSelection(kpw)
KinputProtocolWidget kpw;
{
    Display *dpy = XtDisplay((Widget)kpw);
    Window w = XtWindow((Widget)kpw);
    Boolean res;

    TRACE(("kinputProtocol:ownSelection()\n"));
    XSetSelectionOwner(dpy, kpw->kinput.convAtom, w, CurrentTime);
    /* 一応 SetSelectionOwner した後 GetSelectionOwner してみて確かめる */
    res = XGetSelectionOwner(XtDisplay((Widget)kpw), kpw->kinput.convAtom) == w;

    if (kpw->kinput.backward_compatible) {
	XSetSelectionOwner(dpy, kpw->kinput.oldConvAtom, w, CurrentTime);
    }

    return res;
}

/*
 *+ client data handling
 */

/*- findClient: get clientdata of given client -*/
static ConvClient *
findClient(kpw, client)
KinputProtocolWidget kpw;
Window client;
{
    register ConvClient *ccp = kpw->kinput.clients;

    while (ccp != NULL) {
	if (ccp->reqwin == client) return ccp;
	ccp = ccp->next;
    }

    return NULL;
}

/*- newClient: get a clientdata for new client -*/
static ConvClient *
newClient(kpw, client, selection)
KinputProtocolWidget kpw;
Window client;
Atom selection;
{
    ConvClient *ccp;

    ccp = XtNew(ConvClient);
    ccp->protocol = unresolved_protocol;
    ccp->style = separate_style;	/* default */
    ccp->protocolwidget = (Widget)kpw;
    ccp->conversion = NULL;
    ccp->reqwin = client;
    ccp->selection = selection;
    ccp->target = None;
    ccp->property = None;
    ccp->esm = ESMethodNone;
    ccp->data = NULL;
    ccp->attrmask = 0L;
    ccp->validattrmask = 0L;
    ccp->start_proc = NULL;
    ccp->detach_proc = NULL;
    ccp->fix_proc = NULL;
    ccp->end_proc = NULL;
    ccp->free_resources = NULL;

    ccp->next = kpw->kinput.clients;
    kpw->kinput.clients = ccp;

    return ccp;
}

/*- attachConverter: attach converter to the client -*/
static Widget
attachConverter(ccp)
ConvClient *ccp;
{
    WidgetClass class;
    KinputProtocolWidget kpw = (KinputProtocolWidget)ccp->protocolwidget;

    if (ccp->conversion != NULL) return ccp->conversion;

    if (ccp->protocol == unresolved_protocol) {
	ccp->protocol = kinput1_protocol;
	ccp->style = separate_style;
    }

    if (ccp->style == overthespot_style) {
	class = overTheSpotConversionWidgetClass;
    } else if (ccp->style == offthespot_style) {
	class = offTheSpotConversionWidgetClass;
    } else {
	class = separateConversionWidgetClass;
    }

    ccp->conversion = CMGetConverter(XtParent(ccp->protocolwidget),
				     ccp->reqwin, class,
				     kpw->kinput.inputObjClass,
				     kpw->kinput.displayObjClass);

    return ccp->conversion;
}

/*- detachConverter: detach converter from client -*/
static void
detachConverter(ccp)
ConvClient *ccp;
{
    TRACE(("detachConverter(window=0x%lx)\n", ccp->reqwin));

    XtRemoveCallback(ccp->conversion, XtNtextCallback,
		     fixCallback, (XtPointer)ccp);
    XtRemoveCallback(ccp->conversion, XtNendCallback,
		     endCallback, (XtPointer)ccp);

    /* call additional detach proc */
    if (ccp->detach_proc != NULL) (*ccp->detach_proc)(ccp);

    CMReleaseConverter(XtParent(ccp->protocolwidget), ccp->conversion);
    ccp->conversion = NULL;
}

/*- jinputDetach: jinput protocol specific detach processing -*/
static void
jinputDetach(client)
ConvClient *client;
{
    XtRemoveCallback(client->conversion, XtNnewTextCallback,
		     jinputNewTextCallback, (XtPointer)client);
}

/*- deleteClient: delete specified client -*/
static void
deleteClient(client)
ConvClient *client;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)client->protocolwidget;
    ConvClient *ccp, *ccp0;

    TRACE(("deleteClient(window=0x%lx)\n", client->reqwin));
    if (client->conversion != NULL) detachConverter(client);
    if (client->free_resources != NULL) (*client->free_resources)(client);

    for (ccp = kpw->kinput.clients, ccp0 = NULL;
	 ccp != NULL;
	 ccp0 = ccp, ccp = ccp->next) {
	if (ccp == client) {
	    if (ccp0 == NULL) {
		kpw->kinput.clients = ccp->next;
	    } else {
		ccp0->next = ccp->next;
	    }
	    XtFree((char *)client);
	    return;
	}
    }
    DPRINT(("deleteClient: cannot find the client in the client list!\n"));
}

/*
 *+ utility functions
 */

/*- isCorrectClientEvent: is the event in correct format? -*/
static Boolean
isCorrectClientEvent(kpw, event)
KinputProtocolWidget kpw;
XEvent *event;
{
    XClientMessageEvent *ev = &(event->xclient);
    Atom atom = (Atom)ev->data.l[0];

    return (ev->window == XtWindow((Widget)kpw) &&
	    ev->format == 32 &&
	    (atom == kpw->kinput.convAtom ||
	     kpw->kinput.backward_compatible && atom == kpw->kinput.oldConvAtom));
}

/*- isCorrectWindowID: is the given window ID valid? -*/
static Boolean
isCorrectWindowID(w, window)
Widget w;
Window window;
{
    int status;
    Window root;
    int x, y;
    unsigned int width, height, bwidth, depth;
    XAEHandle h;

    /*
     * previous version uses XGetWindowAttributes(), which
     * issues both GetWindowAttributes and GetGeometry request.
     * since it is a bit inefficient to issue two requests just
     * for checking if a window exists, I've changed to use
     * XGetGeometry which issues only GetGeometry.
     */
    h = XAESetIgnoreErrors(XtDisplay(w));
    status = XGetGeometry(XtDisplay(w), window, &root, &x, &y,
			  &width, &height, &bwidth, &depth);
    XAEUnset(h);
    return status == 0 ? False : True;
}

/*- getJinputInitialProperty: get property for jinput protocol and set data -*/
static void
getJinputInitialProperty(kpw, ccp, prop, reqtype)
KinputProtocolWidget kpw;
ConvClient *ccp;
Atom prop;
Atom reqtype;
{
    Atom type;
    int format;
    unsigned long nitems;
    long *data;
    unsigned long bytesafter;
    JinputData *jdp = (JinputData *)ccp->data;

    XGetWindowProperty(XtDisplay((Widget)kpw), ccp->reqwin, prop, 0L, 11L,
		       False, reqtype, &type, &format, &nitems,
		       &bytesafter, (unsigned char **)&data);

    if (type != reqtype) {
	DPRINT(("getJinputInitialProperty(): unknown property type (%ld)\n", type));
	return;
    }

    /* data[0]: version number */
    if (data[0] != JINPUT_PROTOCOL_VERSION) {
	DPRINT(("getJinputInitialProperty(): unknown version (0x%lx)\n", data[0]));
	return;
    }
    /* data[1]: requestor window */
    if (data[1] != ccp->reqwin) {
	return;
    }
    /* data[2]: preedit type */
    if (data[2] == 3) {
	/* over-the-spot */
	ccp->style = overthespot_style;
    } else {
	ccp->style = separate_style;
    }
    /* data[3]: fold mode (ignore) */
    /* data[4]: clip mode (ignore) */
    /* data[5]: multi color mode */
    if (data[5] == 0) {
	/* fixed color */
	ccp->attrmask |= CAColor;
	/* data[6]: background */
	ccp->attrs.background = data[6];
	/* data[7]: foreground */
	ccp->attrs.foreground = data[7];
    } else {
	/* query */
	DPRINT(("getJinputInitialProperty(): multi-color mode\n"));
	jdp->state |= JINPUT_MULTI_COLOR;
    }
    /* data[8]: multi font mode */
    if (data[8] == 0) {
	ccp->attrmask |= CAFonts;
	ccp->attrs.num_fonts = 2;
	ccp->attrs.fonts = (XFontStruct **)XtMalloc(sizeof(XFontStruct *) * 2);
	/* data[9]: font1 */
	ccp->attrs.fonts[0] = XQueryFont(XtDisplay((Widget)kpw),
					 (XID)data[9]);	/* should make it safe */
	/* data[10]: font2 */
	ccp->attrs.fonts[1] = XQueryFont(XtDisplay((Widget)kpw),
					 (XID)data[10]);
    } else {
	/* query */
	TRACE(("getJinputInitialProperty(): multi-font mode\n"));
	jdp->state |= JINPUT_MULTI_FONT;
    }
}

/*- jinputSendReq: jinput protocol specific startup processing -*/
static void
jinputSendReq(client)
ConvClient *client;
{
    JinputData *jdp = (JinputData *)client->data;

    XtAddCallback(client->conversion, XtNnewTextCallback,
		  jinputNewTextCallback, (XtPointer)client);

    if (jdp->state & JINPUT_MULTI_COLOR) sendColorRequest(client);
    if (jdp->state & JINPUT_MULTI_FONT) sendFontRequest(client);
    if (client->style != separate_style) sendXYRequest(client);
}

/*- jinputFreeResources: free resources for jinput protocol client -*/
static void
jinputFreeResources(client)
ConvClient *client;
{
    XtFree(client->data);
    if (client->attrmask & CAFonts) {
	/* free fonts */
	XFreeFontInfo((char **)NULL, client->attrs.fonts[0], 1);
	XFreeFontInfo((char **)NULL, client->attrs.fonts[1], 1);
	XtFree((char *)client->attrs.fonts);
    }
}

/*- xlcFreeResources: free resources for xlc protocol client -*/
static void
xlcFreeResources(client)
ConvClient *client;
{
    if (client->validattrmask & CAFonts) {
	/* free fonts */
	XFreeFontInfo((char **)NULL, client->attrs.fonts[0], 1);
	XFreeFontInfo((char **)NULL, client->attrs.fonts[1], 1);
	XtFree((char *)client->attrs.fonts);
    }
}

/*- kinput2FreeResources: free resources for kinput2 protocol client -*/
static void
kinput2FreeResources(client)
ConvClient *client;
{
    if (client->attrmask & CAFonts) {	/* free fonts */
	Cardinal i;
	for (i = 0; i < client->attrs.num_fonts; i++) {
	    CachedFreeFont(XtDisplay(client->protocolwidget),
			   client->attrs.fonts[i]);
	}
	XtFree((char *)client->attrs.fonts);
    }
}

struct proprec {
    Atom prop;
    struct proprec *prev;
};

/*- setAttribute: set conversion attribute (kinput2 only) -*/
static void
setAttribute(client, data, precp, fromevent)
ConvClient *client;
unsigned long *data;
struct proprec *precp;	/* to prevent possible inifinite loop */
Boolean fromevent;
{
    int code = CODE_OF_ATTR(*data);
    int len = LENGTH_OF_ATTR(*data);

#define CHECK_LENGTH(num, name) \
    if (len != num) { DPRINT(("\tlength of %s attribute is wrong\n", name)); break; }

    TRACE(("setAttribute()\n"));
    data++;
    switch (code) {
    case CONVATTR_NONE:
	TRACE(("\tNone:\n"));
	break;
    case CONVATTR_INDIRECT:
	CHECK_LENGTH(1, "IndirectAttribute");
	TRACE(("\tIndirectAttribute:\n"));
	getAttributeFromProperty1(client, data[0], precp, fromevent);
	break;
    case CONVATTR_FOCUS_WINDOW:
	CHECK_LENGTH(1, "FocusWindow");
	client->attrmask |= CAFocusWindow;
	client->attrs.focuswindow = data[0];
	TRACE(("\tFocusWindow: 0x%lx\n", data[0]));
	break;
    case CONVATTR_SPOT_LOCATION:
	CHECK_LENGTH(1, "SpotLocation");
	client->attrmask |= CASpotLocation;
	client->attrs.spotx = UPPER16S(data[0]);
	client->attrs.spoty = LOWER16S(data[0]);
	TRACE(("\tSpotLocation: x=%d, y=%d\n", client->attrs.spotx, client->attrs.spoty));
	break;
    case CONVATTR_CLIENT_AREA:
	CHECK_LENGTH(2, "ClientArea");
	client->attrmask |= CAClientArea;
	client->attrs.clientarea.x = UPPER16S(data[0]);
	client->attrs.clientarea.y = LOWER16S(data[0]);
	client->attrs.clientarea.width = UPPER16U(data[1]);
	client->attrs.clientarea.height = LOWER16U(data[1]);
	TRACE(("\tClientArea: %d,%d-%d,%d\n", client->attrs.clientarea.x,client->attrs.clientarea.y,client->attrs.clientarea.width,client->attrs.clientarea.height));
	break;
    case CONVATTR_STATUS_AREA:
	CHECK_LENGTH(2, "StatusArea");
	client->attrmask |= CAStatusArea;
	client->attrs.statusarea.x = UPPER16S(data[0]);
	client->attrs.statusarea.y = LOWER16S(data[0]);
	client->attrs.statusarea.width = UPPER16U(data[1]);
	client->attrs.statusarea.height = LOWER16U(data[1]);
	TRACE(("\tStatusArea: %d,%d-%d,%d\n", client->attrs.statusarea.x,client->attrs.statusarea.y,client->attrs.statusarea.width,client->attrs.statusarea.height));
	break;
    case CONVATTR_COLORMAP:
	CHECK_LENGTH(1, "Colormap");
	client->attrmask |= CAColormap;
	client->attrs.colormap = data[0];
	TRACE(("\tColormap: 0x%lx\n", data[0]));
	break;
    case CONVATTR_COLOR:
	CHECK_LENGTH(2, "Color");
	client->attrmask |= CAColor;
	client->attrs.foreground = data[0];
	client->attrs.background = data[1];
	TRACE(("\tColor: fg=%ld,bg=%ld\n", data[0], data[1]));
	break;
    case CONVATTR_BACKGROUND_PIXMAP:
	CHECK_LENGTH(1, "BackgroundPixmap");
	client->attrmask |= CABackgroundPixmap;
	client->attrs.background_pixmap = data[0];
	TRACE(("\tBackgroundPixmap: 0x%lx\n", data[0]));
	break;
    case CONVATTR_LINE_SPACING:
	CHECK_LENGTH(1, "LineSpacing");
	client->attrmask |= CALineSpacing;
	client->attrs.linespacing = data[0];
	TRACE(("\tLineSpacing: %ld\n", data[0]));
	break;
    case CONVATTR_FONT_ATOMS:
	if (len < 1) {
	    DPRINT(("length of FontAtoms attribute is less than 1\n"));
	    break;
	}
	if (client->attrmask & CAFonts) {
	    /* fonts set more than once -- free previously specified fonts */
	    Cardinal i;
	    for (i = 0; i < client->attrs.num_fonts; i++) {
		CachedFreeFont(XtDisplay(client->protocolwidget),
			       client->attrs.fonts[i]);
	    }
	    XtFree((char *)client->attrs.fonts);
	}
	client->attrmask |= CAFonts;
	client->attrs.num_fonts =
	  getFontsByFontAtoms(XtDisplay(client->protocolwidget),
			      (Atom *)data, (Cardinal)len,
			      &client->attrs.fonts);
	break;
    case CONVATTR_CURSOR:
	CHECK_LENGTH(1, "Cursor");
	client->attrmask |= CACursor;
	client->attrs.cursor = data[0];
	TRACE(("\tCursor: 0x%lx\n", data[0]));
	break;
    case CONVATTR_INPUT_STYLE:
	if (fromevent) {
	    DPRINT(("InputStyle can't change during conversion\n"));
	    break;
	}
	CHECK_LENGTH(1, "InputStyle");
	if (data[0] == CONVARG_OVERTHESPOT) {
	    client->style = overthespot_style;
	} else if (data[0] == CONVARG_OFFTHESPOT) {
	    client->style = offthespot_style;
	} else {
	    client->style = separate_style;
	}
	TRACE(("\tInputStyle: %ld\n", data[0]));
	break;
    case CONVATTR_EVENT_CAPTURE_METHOD:
	if (fromevent) {
	    DPRINT(("EventCaptureMethod can't change during conversion\n"));
	    break;
	}
	CHECK_LENGTH(1, "EventCaptureMethod");
	if (data[0] == CONVARG_NONE) {
	    client->esm = ESMethodNone;
	} else if (data[0] == CONVARG_SELECT_FOCUS_WINDOW) {
	    client->esm = ESMethodSelectFocus;
	} else {
	    client->esm = ESMethodInputOnly;
	}
	TRACE(("\tEventCaptureMethod: %ld\n", data[0]));
	break;
    case CONVATTR_USE_EXTENSION:
	if (fromevent) {
	    DPRINT(("UseExtension must be specified at conversion startup\n"));
	    break;
	}
	TRACE(("\tUseExtension: not supported\n"));
	break;
    default:
	if (code > 255) {
	    /* private extension code */
	    DPRINT(("\tPrivateExtension (code=%d): not supported\n", code));
	}
    }
#undef CHECK_LENGTH
}

/*- getFontsByFontAtoms: get fonts from 'FONT' atom list (kinput2 only) -*/
static int
getFontsByFontAtoms(dpy, atoms, len, fontsp)
Display *dpy;
Atom *atoms;
Cardinal len;
XFontStruct *** fontsp;
{
    XFontStruct **fonts;
    Cardinal nf, i;

    fonts = (XFontStruct **)XtMalloc(sizeof(XFontStruct *) * len);
    for (nf = 0, i = 0; i < len; i++) {
	if ((fonts[nf] = CachedLoadQueryFontByProp(dpy, atoms[i])) == NULL) {
	    TRACE(("\tcan't load font (atom=%ld)\n", atoms[i]));
	} else {
	    TRACE(("\tfont loaded (atom=%ld)\n", atoms[i]));
	    nf++;
	}
    }

    *fontsp = fonts;
    return nf;
}

/*- safeGetAttributeProperty: get attribute property safely -*/
static int
safeGetAttributeProperty(client, prop, data)
ConvClient *client;
Atom prop;
unsigned long **data;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)client->protocolwidget;
    int err;
    Atom type;
    int format;
    unsigned long nitems;
    unsigned long bytesafter;
    XAEHandle h;

    *data = NULL;
    
    h = XAESetIgnoreErrors(XtDisplay((Widget)kpw));
    err = XGetWindowProperty(XtDisplay((Widget)kpw), client->reqwin, prop,
			     0L, 1000L, False,
			     kpw->kinput.convAttributeAtom,
			     &type, &format, &nitems,
			     &bytesafter, (unsigned char **)data);
    XAEUnset(h);

    if (err) {
	DPRINT(("\tcan't get property value. bad property (0x%lx)?\n", prop));
	return 0;
    }else if (format != 32 || type != kpw->kinput.convAttributeAtom) {
	DPRINT(("\twrong format or type\n"));
	if (*data != NULL) XtFree((char *)*data);
	return 0;
    }

    TRACE(("safeGetAttributeProperty(): returns %ld\n", nitems));
    return nitems;
}

/*- getAttributeFromProperty1: get and set attribute data (kinput2 only) -*/
static void
getAttributeFromProperty1(client, prop, precp, fromevent)
ConvClient *client;
Atom prop;
struct proprec *precp;
Boolean fromevent;
{
    unsigned long *data, *datap;
    int nitems;
    struct proprec prec;

    prec.prop = prop;
    prec.prev = precp;

    /* try to detect loop */
    while (precp != NULL) {
	if (precp->prop == prop) {
	    DPRINT(("loop detected. property=0x%lx\n", prop));
	    return;
	}
	precp = precp->prev;
    }

    data = NULL;
    if ((nitems = safeGetAttributeProperty(client, prop, &data)) <= 0) {
	return;
    }

    datap = data;
    while (nitems > 0) {
	int alen = LENGTH_OF_ATTR(datap[0]) + 1; /* 1 is for the header */

	if (alen > nitems) break;
	setAttribute(client, datap, &prec, fromevent);
	nitems -= alen;
	datap += alen;
    }
    XtFree((char *)data);
}

/*- getAttributeFromProperty: get and set attribute data (kinput2 only) -*/
static void
getAttributeFromProperty(client, prop)
ConvClient *client;
Atom prop;
{
    TRACE(("getAttributeFromProperty(reqwin=0x%lx, prop=%ld)\n", client->reqwin, prop));
    getAttributeFromProperty1(client, prop, (struct proprec *)NULL, False);
}

/*- getAttributeFromEvent: set attribute data specified in an event (kinput2 only) -*/
static void
getAttributeFromEvent(client, ev)
ConvClient *client;
XEvent *ev;
{
    XClientMessageEvent *event = &(ev->xclient);
    unsigned long *data;
    int alen;

    TRACE(("getAttributeFromEvent(client=0x%lx)\n", client->reqwin));

    data = (unsigned long *)&(event->data.l[2]);
    alen = LENGTH_OF_ATTR(data[0]);
    if (alen >= 3) return;

    setAttribute(client, data, (struct proprec *)NULL, True);
}

/*- getXlcDataFromProperty: get and set attribute data (xlc only) -*/
static ConvClient *
getXlcDataFromProperty(kpw, client, initial)
KinputProtocolWidget kpw;
ConvClient *client;
Boolean initial;
{
    unsigned long *data;
    Atom type;
    int format;
    unsigned long nitems;
    unsigned long bytesafter;
    int flag;
    int err;

    TRACE(("getXlcDataFromProperty(initial=%d)\n", initial));
    data = NULL;
    err = XGetWindowProperty(XtDisplay((Widget)kpw), XtWindow((Widget)kpw),
			     kpw->kinput.xlcOnTheSpotAtom,
			     0L, 1000L, True,
			     kpw->kinput.xlcOnTheSpotAtom,
			     &type, &format, &nitems,
			     &bytesafter, (unsigned char **)&data);

    if (err) {
	DPRINT(("\tcan't get property value\n"));
	return NULL;
    } else if (/* format != 32 || */ type != kpw->kinput.xlcOnTheSpotAtom) {
	DPRINT(("\twrong format(%d) or type(%ld)\n", format, type));
	if (data != NULL) XtFree((char *)data);
	return NULL;
    } else if (nitems < 17) {
	/* it seems that the current implementation of Xlc library
	 * (client-side library for XLC protocol) has a bug.
	 * it seems to set number of bytes instead of number of items
	 * when calling XChangeProperty().
	 */
	DPRINT(("\twrong length (%ld)\n", nitems));
	return NULL;
    }

    if (client == NULL) {
	if ((client = findClient(kpw, (Window)data[1])) == NULL) {
	    DPRINT(("\tcan't find the client rec. for the window ID\n"));
	    return NULL;
	}
    } else {
	if (data[1] != client->reqwin) {
	    DPRINT(("\twindow ID isn't the requestor window ID\n"));
	    return NULL;
	}
    }
    TRACE(("\t%ld items read\n", nitems));

    flag = data[0];
    if (flag & XLC_AUTO_REPLACE) {
	/* set auto spot forwarding */
	/* XXX */
	TRACE(("\tauto spot forwarding\n"));
    }
    if (flag & XLC_ALL_INFORMATION) {
	/* foreground / background colors */
	if (!initial &&
	    (client->validattrmask & CAColor) &&
	    client->attrs.foreground == data[5] &&
	    client->attrs.background == data[6]) {
	    client->attrmask &= ~CAColor;
	} else {
	    client->attrs.foreground = data[5];
	    client->attrs.background = data[6];
	    client->attrmask |= CAColor;
	}
	client->validattrmask |= CAColor;
	TRACE(("\tColor: fg=%ld,bg=%ld\n", data[5], data[6]));

	/* fonts */
	if (data[7] == 0L || data[8] == 0L) {
	    /* invalid */
	    if (client->validattrmask & CAFonts) {
		XFreeFontInfo((char **)NULL, client->attrs.fonts[0], 1);
		XFreeFontInfo((char **)NULL, client->attrs.fonts[2], 1);
		XFree((char *)client->attrs.fonts);
		client->validattrmask &= ~CAFonts;
	    }
	    client->attrmask &= ~CAFonts;
	} else {
	    if (!initial &&
		(client->validattrmask & CAFonts) &&
		client->attrs.fonts[0]->fid == data[7] &&
		client->attrs.fonts[1]->fid == data[8]) {
		client->attrmask &= ~CAFonts;
	    } else if (client->validattrmask & CAFonts) {
		if (client->attrs.fonts[0]->fid != data[7]) {
		    XFreeFontInfo((char **)NULL, client->attrs.fonts[0], 1);
		    client->attrs.fonts[0] = XQueryFont(XtDisplay((Widget)kpw),
							(XID)data[7]);
		}
		if (client->attrs.fonts[1]->fid != data[8]) {
		    XFreeFontInfo((char **)NULL, client->attrs.fonts[1], 1);
		    client->attrs.fonts[1] = XQueryFont(XtDisplay((Widget)kpw),
							(XID)data[8]);
		}
		client->attrmask |= CAFonts;
	    } else {
		client->attrs.num_fonts = 2;
		client->attrs.fonts =
		  (XFontStruct **)XtMalloc(sizeof(XFontStruct *) * 2);
		client->attrs.fonts[0] = XQueryFont(XtDisplay((Widget)kpw),
						    (XID)data[7]);
		client->attrs.fonts[1] = XQueryFont(XtDisplay((Widget)kpw),
						    (XID)data[8]);
		client->attrmask |= CAFonts;
	    }
	    client->validattrmask |= CAFonts;
	    TRACE(("\tFonts: 0x%lx, 0x%lx\n", data[7], data[8]));
	}
    }
    if (flag & (XLC_ALL_INFORMATION | XLC_FRAME_INFORMATION)) {
	/* frame (client area) */
	if (!initial &&
	    (client->validattrmask & CAClientArea) &&
	    client->attrs.clientarea.x == data[10] &&
	    client->attrs.clientarea.y == data[11] &&
	    client->attrs.clientarea.width == data[12] &&
	    client->attrs.clientarea.height == data[13]) {
	    client->attrmask &= ~CAClientArea;
	} else {
	    client->attrs.clientarea.x = data[10];
	    client->attrs.clientarea.y = data[11];
	    client->attrs.clientarea.width = data[12];
	    client->attrs.clientarea.height = data[13];
	    if (data[12] == 0 || data[13] == 0) {
		client->attrmask &= ~CAClientArea;
	    } else {
		client->attrmask |= CAClientArea;
	    }
	}
	client->validattrmask |= CAClientArea;
	TRACE(("\tClientArea: %ld,%ld-%ld,%ld\n",
	       data[10],data[11],data[12],data[13]));
    }
    if (flag & (XLC_ALL_INFORMATION | XLC_FRAME_INFORMATION | XLC_OFFSET_INFORMATION)) {
	/* spot location */
	client->attrmask |= CASpotLocation;	/* always set spot location */
	client->attrs.spotx = data[14];
	client->attrs.spoty = data[15] +
	  ((client->validattrmask & CAFonts) ?
		client->attrs.fonts[0]->ascent : kpw->kinput.defaultascent);
	client->validattrmask |= CASpotLocation;
	TRACE(("\tSpotLocation: x=%ld, y=%ld\n", data[14], data[15]));

	/* line spacing */
	if (!initial &&
	    (client->validattrmask & CALineSpacing) &&
	    client->attrs.linespacing == data[16]) {
	    client->attrmask &= ~CALineSpacing;
	} else {
	    client->attrmask |= CALineSpacing;
	    client->attrs.linespacing = data[16];
	}
	client->validattrmask |= CALineSpacing;
	TRACE(("\tLineSpacing: %ld\n", data[16]));
    }

    XtFree((char *)data);
    return client;
}

/*- initializeError: display error message when resource isn't specified -*/
static void
initializeError(w, resname)
Widget w;
String resname;
{
    String params[2];
    Cardinal num_params;

    params[0] = XtClass(w)->core_class.class_name;
    params[1] = resname;
    num_params = 2;
    XtAppErrorMsg(XtWidgetToApplicationContext(w),
		  "initializeError", "noResource", "WidgetError",
		  "%s: resource %s must be specified at widget creation",
		  params, &num_params);
}

/*- parseKeyEvent: parses key event description and get keycode and modmask -*/
static int
parseKeyEvent(dpy, s, codep, modmaskp)
Display *dpy;
char *s;
long *codep;
long *modmaskp;
{
    char *mod, *key;
    char *p;
    KeySym keysym;
    char buf[128];

    /*
     * keyevent description (stored in  argument 's') must be of the
     * following format:
     *		modifier-list<Key>keysym
     * modifier-list is a combination of:
     *		Ctrl, Shift, Lock, Meta, Alt, Mod1, Mod2, Mod3, Mod4, Mod5
     */

    strcpy(buf, s);

    /* find "<Key>" */
    if ((p = mystrstr(buf, "<Key>")) != NULL) {
	key = p + 5;	/* p + strlen("<Key>") */
    } else if ((p = mystrstr(buf, "<KeyPress>")) != NULL) {
	key = p + 10;	/* p + strlen("<KeyPress>") */
    } else if ((p = mystrstr(buf, "<KeyDown>")) != NULL) {
	key = p + 9;	/* p + strlen("<KeyDown>") */
    } else {
	return 0;
    }
    mod = buf;
    *p = '\0';

    /* get modifier mask */
    *modmaskp = 0;
    if (mystrstr(mod, "Shift")) *modmaskp |= ShiftMask;
    if (mystrstr(mod, "Lock")) *modmaskp |= LockMask;
    if (mystrstr(mod, "Ctrl")) *modmaskp |= ControlMask;
    if (mystrstr(mod, "Mod1")) *modmaskp |= Mod1Mask;
    if (mystrstr(mod, "Mod2")) *modmaskp |= Mod2Mask;
    if (mystrstr(mod, "Mod3")) *modmaskp |= Mod3Mask;
    if (mystrstr(mod, "Mod4")) *modmaskp |= Mod4Mask;
    if (mystrstr(mod, "Mod5")) *modmaskp |= Mod5Mask;
    if (mystrstr(mod, "Meta")) *modmaskp |= Mod1Mask;
    if (mystrstr(mod, "Alt")) *modmaskp |= Mod1Mask;

    /* get keycode */
    if ((keysym = XStringToKeysym(key)) == NoSymbol) return 0;
    if ((*codep = (long)XKeysymToKeycode(dpy, keysym)) == 0) return 0;
    return 1;
}

/*- mystrstr: not-so-good implementaion of ANSI strstr() -*/
static char *
mystrstr(s1, s2)
char *s1;
char *s2;
{
    char *p, *q;

    while (*(p = s1++) != '\0') {
	q = s2;
	do {
	    if (*q == '\0') return s1 - 1;
	} while (*p++ == *q++);
    }
    return NULL;
}

/*- getDefaultFontHeight: get default font ascent (for xlc protocol) -*/
static void
getDefaultFontHeight(kpw)
KinputProtocolWidget kpw;
{
    Widget obj;

    obj = XtCreateWidget("displayObj", kpw->kinput.displayObjClass,
			 (Widget)kpw, NULL, 0);
    (void)CDLineHeight(obj, &kpw->kinput.defaultascent);
    XtDestroyWidget(obj);
}

/*
 *+ property handling
 */

/*- setJinputProperty: set version property for jinput protocol -*/
static void
setJinputProperty(kpw)
KinputProtocolWidget kpw;
{
    long version;

    version = JINPUT_PROTOCOL_VERSION;
    XChangeProperty(XtDisplay((Widget)kpw), XtWindow((Widget)kpw),
		    kpw->kinput.convVersionAtom, XA_INTEGER, 32,
		    PropModeReplace, (unsigned char *)&version, 1);
}

/*- setKinput2Property: set version property for kinput2 protocol -*/
static void
setKinput2Property(kpw)
KinputProtocolWidget kpw;
{
    Display *dpy = XtDisplay((Widget)kpw);
    Atom property;
    Atom type;
    unsigned long profile[10];

    property = XInternAtom(dpy, CONVERSION_PROFILE, False);
    type = XInternAtom(dpy, CONVERSION_ATTRIBUTE_TYPE, False);

    profile[0] = CONV_ATTR(CONVPROF_PROTOCOL_VERSION, 1);
    profile[1] = XInternAtom(dpy, PROTOCOL_VERSION, False);
    profile[2] = CONV_ATTR(CONVPROF_SUPPORTED_STYLES, 1);
    profile[3] = CONVARG_ROOTWINDOW|CONVARG_OFFTHESPOT|CONVARG_OVERTHESPOT;

    XChangeProperty(dpy, XtWindow((Widget)kpw),
		    property, type, 32, PropModeReplace,
		    (unsigned char *)profile, 4);
}

/*- setXlcProperty: set initial status property for xlc protocol -*/
static void
setXlcProperty(kpw)
KinputProtocolWidget kpw;
{
    setXlcStatusProperty(kpw, 0);
    setXlcBCKey(kpw);
}

/*- setXlcStatusProperty: set status property for xlc protocol -*/
static void
setXlcStatusProperty(kpw, status)
KinputProtocolWidget kpw;
int status;
{
    XChangeProperty(XtDisplay((Widget)kpw), XtWindow((Widget)kpw),
		    kpw->kinput.xlcStatusAtom, XA_INTEGER, 32,
		    PropModeReplace, (unsigned char *)&status, 1);
}

/*- setXlcBCKey: set conversion start key code property for xlc protocol -*/
static void
setXlcBCKey(kpw)
KinputProtocolWidget kpw;
{
    long code;
    long mask;

    if (kpw->kinput.xlcstartkey == NULL) return;
    if (parseKeyEvent(XtDisplay((Widget)kpw), kpw->kinput.xlcstartkey,
		    &code, &mask) == 0) {
	String params[1];
	Cardinal num_params;

	params[0] = XtClass((Widget)kpw)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext((Widget)kpw),
			"parseError", XtNxlcConversionStartKey, "WidgetError",
			"%s: can't parse coversion start key string",
			params, &num_params);
	return;
    }

    TRACE(("setXlcBCKey(): keycode=%ld, mask=0x%lx\n", code, mask));

    XChangeProperty(XtDisplay((Widget)kpw), XtWindow((Widget)kpw),
		    kpw->kinput.xlcBcModifierAtom, XA_INTEGER, 32,
		    PropModeReplace, (unsigned char *)&mask, 1);
    XChangeProperty(XtDisplay((Widget)kpw), XtWindow((Widget)kpw),
		    kpw->kinput.xlcBcKeycodeAtom, XA_INTEGER, 32,
		    PropModeReplace, (unsigned char *)&code, 1);
}

/*
 *+ event sending
 */

/*- sendClientMessage: send a clientmessage event -*/
static void
sendClientMessage(dpy, window, type, l0, l1, l2, l3, l4)
Display *dpy;
Window window;
Atom type;
long l0, l1, l2, l3, l4;
{
    XEvent event;

    event.xclient.type = ClientMessage;
    event.xclient.window = window;
    event.xclient.message_type = type;
    event.xclient.format = 32;
    event.xclient.data.l[0] = l0;
    event.xclient.data.l[1] = l1;
    event.xclient.data.l[2] = l2;
    event.xclient.data.l[3] = l3;
    event.xclient.data.l[4] = l4;

    XSendEvent(dpy, window, False, NoEventMask, &event);
}

/*- sendNegativeConversionNotify: send negative conversion-notify event -*/
static void
sendNegativeConversionNotify(kpw, window, selection)
KinputProtocolWidget kpw;
Window window;
Atom selection;
{
    TRACE(("sendNegativeConversionNotify(window=0x%lx)\n", window));
    sendClientMessage(XtDisplay((Widget)kpw), window,
		      kpw->kinput.convNotifyAtom,
		      (long)selection, (long)None, 0L, 0L, 0L);
}

/*- sendConversionNotify: send conversion-notify event -*/
static void
sendConversionNotify(ccp)
ConvClient *ccp;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)ccp->protocolwidget;
    long l4;

    TRACE(("sendConversionNotify(reqwin=0x%lx)\n", ccp->reqwin));
    if (ccp->protocol == xlc_protocol) {
	l4 = (long)kpw->kinput.xlcOnTheSpotAtom;
    } else {
	l4 = 0L;
    }
    sendClientMessage(XtDisplay((Widget)kpw), ccp->reqwin,
		      kpw->kinput.convNotifyAtom,
		      (long)ccp->selection, (long)ccp->target,
		      (long)ccp->property, (long)XtWindow(ccp->conversion),
		      l4);
}

/*- sendNegativeConversionOpenNotify: send negative conversion-open-notify event -*/
static void
sendNegativeConversionOpenNotify(kpw, window, selection)
KinputProtocolWidget kpw;
Window window;
Atom selection;
{
    TRACE(("sendNegativeConversionOpenNotify(window=0x%lx)\n", window));
    sendClientMessage(XtDisplay((Widget)kpw), window,
		      kpw->kinput.convOpenNotifyAtom,
		      (long)selection, 0L, (long)XtWindow(kpw), 0L, 0L);
}

/*- sendConversionOpenNotify: send conversion-open-notify event -*/
static void
sendConversionOpenNotify(ccp)
ConvClient *ccp;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)ccp->protocolwidget;

    TRACE(("sendConversionOpenNotify(reqwin=0x%lx)\n", ccp->reqwin));
    sendClientMessage(XtDisplay((Widget)kpw), ccp->reqwin,
		      kpw->kinput.convOpenNotifyAtom,
		      (long)ccp->selection, JINPUT_PROTOCOL_VERSION,
		      (long)XtWindow((Widget)kpw), 1L, 0L);
}

/*- sendColorRequest: send color-request event (jinput only) -*/
static void
sendColorRequest(ccp)
ConvClient *ccp;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)ccp->protocolwidget;

    TRACE(("sendColorRequest(reqwin=0x%lx)\n", ccp->reqwin));
    sendClientMessage(XtDisplay((Widget)kpw), ccp->reqwin,
		      kpw->kinput.convColorRequestAtom,
		      (long)ccp->selection, JINPUT_PROTOCOL_VERSION,
		      (long)XtWindow((Widget)kpw), 0L, 0L);
}

/*- sendFontRequest: send font-request event (jinput only) -*/
static void
sendFontRequest(ccp)
ConvClient *ccp;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)ccp->protocolwidget;

    TRACE(("sendFontRequest(reqwin=0x%lx)\n", ccp->reqwin));
    sendClientMessage(XtDisplay((Widget)kpw), ccp->reqwin,
		      kpw->kinput.convFontsRequestAtom,
		      (long)ccp->selection, JINPUT_PROTOCOL_VERSION,
		      (long)XtWindow((Widget)kpw), 0L, 0L);
}

/*- sendXYRequest: send XY-request event (jinput only) -*/
static void
sendXYRequest(ccp)
ConvClient *ccp;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)ccp->protocolwidget;

    TRACE(("sendXYRequest(reqwin=0x%lx)\n", ccp->reqwin));
    sendClientMessage(XtDisplay((Widget)kpw), ccp->reqwin,
		      kpw->kinput.convXYRequestAtom,
		      (long)ccp->selection, JINPUT_PROTOCOL_VERSION,
		      (long)XtWindow((Widget)kpw), 0L, 0L);
}

/*
 *+ callback procedures
 */

/*- fixCallback: fix callback -*/
/* ARGSUSED */
static void
fixCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    CCTextCallbackArg *arg = (CCTextCallbackArg *)call_data;
    ConvClient *ccp = (ConvClient *)client_data;

    TRACE(("fixCallback(reqwin=0x%lx, length=%d)\n",ccp->reqwin, arg->length));
    fixProc(ccp, arg);
}

/*- fixProc: do actual fix processing -*/
static void
fixProc(client, arg)
ConvClient *client;
CCTextCallbackArg *arg;
{
    /* Property に結果をセットする */
    XChangeProperty(XtDisplay(client->conversion), client->reqwin,
		    client->property, arg->encoding, arg->format,
		    PropModeAppend, (unsigned char *)arg->text, arg->length);

    /* call protocol dependent proc */
    if (client->fix_proc != NULL) (*client->fix_proc)(client, arg);
}

/*- jinputFix: jinput protocol specific fix processing -*/
static void
jinputFix(client)
ConvClient *client;
{
    JinputData *jdp = (JinputData *)client->data;

    /* spotlocation information is no longer valid */
    client->attrmask &= ~CASpotLocation;

    if (jdp->state & JINPUT_MULTI_COLOR) sendColorRequest(client);
    if (jdp->state & JINPUT_MULTI_FONT) sendFontRequest(client);
    if (client->style != separate_style) sendXYRequest(client);
}

/*- endCallback: conversion end callback -*/
/* ARGSUSED */
static void
endCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    ConvClient *ccp = (ConvClient *)client_data;
    int abort = (int)call_data;

    TRACE(("endCallback(reqwin=0x%lx,abort=%s)\n", ccp->reqwin, abort?"True":"False"));
    endProc(ccp, abort);
}

/*- endProc: conversion end processing -*/
static void
endProc(client, abort)
ConvClient *client;
int abort;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)client->protocolwidget;

    if (!abort) {
	/* クライアントに ClientMessage を送って知らせる
	 * イベントのフォーマットは、
	 *	window:		requestor window
	 *	message_type:	"CONVERSION_END"
	 *	format:		32
	 *	data.l[0]:	selection
	 *	data.l[1]:	selection-owner window
	 */
	sendClientMessage(XtDisplay((Widget)kpw), client->reqwin,
			  kpw->kinput.convEndAtom,
			  (long)client->selection, (long)XtWindow((Widget)kpw),
			  0L, 0L, 0L);
    }
    if (client->conversion != NULL) {
	XtRemoveCallback(client->conversion, XtNtextCallback,
			 fixCallback, (XtPointer)client);
	XtRemoveCallback(client->conversion, XtNendCallback,
			 endCallback, (XtPointer)client);
    }

    /* call protocol dependent proc */
    if (client->end_proc != NULL) {
	(*client->end_proc)(client, abort);
    } else {
	deleteClient(client);
    }
}

/*- jinputEnd: jinput specific conversion end processing -*/
/* ARGSUSED */
static void
jinputEnd(client, abort)
ConvClient *client;
Boolean abort;		/* UNUSED */
{
    /* don't delete client until CONVERSION_CLOSE_REQUEST or client dead */
    detachConverter(client);

    client->attrmask &= ~CASpotLocation;
}

/*- jinputNewTextCallback: jinput protocol specific new text callback -*/
/* ARGSUSED */
static void
jinputNewTextCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    ConvClient *ccp = (ConvClient *)client_data;

    TRACE(("jinputNewTextCallback(reqwin=0x%lx)\n",ccp->reqwin));
    if (ccp->style != separate_style) sendXYRequest(ccp);
}

/*- xlcEnd: xlc specific conversion end processing -*/
/* ARGSUSED */
static void
xlcEnd(client, abort)
ConvClient *client;
Boolean abort;		/* UNUSED */
{
    /* don't delete client until CONVERSION_CLOSE or client dead */
    detachConverter(client);

    client->attrmask &= ~CASpotLocation;
}

/*
 *+ ClientMessage event handler
 */

/*- ConversionOpenRequestProc: CONVERSION_OPEN_REQUEST event handler -*/
/* ARGSUSED */
static void
ConversionOpenRequestProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    int version;
    Window reqwin;
    Atom convatom;
    Atom initproperty;
    Atom initpropertytype;
    ConvClient	*ccp;
    JinputData *jdp;

    TRACE(("ConversionOpenRequestProc(window=0x%lx)\n", ev->data.l[2]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }

    convatom = (Atom)ev->data.l[0];
    version = ev->data.l[1];
    reqwin = (Window)ev->data.l[2];
    initproperty = ev->data.l[3];
    initpropertytype = ev->data.l[4];

    /* check the protocol version & initial property type */
    /* also check if the client already opened connection */
    if (version != JINPUT_PROTOCOL_VERSION ||
	initpropertytype != kpw->kinput.convInitialTypeAtom ||
	findClient(kpw, reqwin) != NULL) {
	DPRINT(("ConversionOpenRequestProc(): open denied\n"));
	sendNegativeConversionOpenNotify(kpw, reqwin, convatom);
	return;
    }
    /* check validity of the client window ID */
    if (!isCorrectWindowID(w, reqwin)) {
	DPRINT(("ConversionOpenRequestProc(): requestor window doesn't exist\n"));
	/* nothing to do */
	return;
    }

    ccp = newClient(kpw, reqwin, convatom);
    ccp->protocol = jinput_protocol;
    ccp->esm = ESMethodInputOnly;
    ccp->start_proc = jinputSendReq;
    ccp->detach_proc = jinputDetach;
    ccp->fix_proc = jinputFix;
    ccp->end_proc = jinputEnd;
    ccp->free_resources = jinputFreeResources;
    jdp = XtNew(JinputData);
    jdp->state = 0;
    ccp->data = (XtPointer)jdp;

    getJinputInitialProperty(kpw, ccp, initproperty, initpropertytype);

    /* watch for client destroy */
    MyAddEventHandler(XtDisplay(w), reqwin, DestroyNotify, StructureNotifyMask,
		      ClientDead, (XtPointer)ccp);

    sendConversionOpenNotify(ccp);
}

/*- ConversionRequestProc: CONVERSION_REQUEST event handler -*/
/* ARGSUSED */
static void
ConversionRequestProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    Atom convatom;
    Atom prop;
    ConvClient	*ccp;

    TRACE(("ConversionRequestProc(window=0x%lx)\n", ev->data.l[1]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }

    convatom = (Atom)ev->data.l[0];
    reqwin = (Window)ev->data.l[1];
    prop = (Atom)ev->data.l[4];
    TRACE(("\tatom=0x%lx, reqwin=0x%lx\n", convatom, reqwin));

    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* check validity of the client window ID */
	if (!isCorrectWindowID(w, reqwin)) {
	    DPRINT(("ConversionRequestProc(): requestor window doesn't exist\n"));
	    /* nothing to do */
	    return;
	}
	ccp = newClient(kpw, reqwin, convatom);
	if (prop == kpw->kinput.xlcOnTheSpotAtom) {
	    /* xlc protocol */
	    TRACE(("\txlc protocol\n"));
	    if (getXlcDataFromProperty(kpw, ccp, True)) {
		ccp->protocol = xlc_protocol;
		ccp->style = overthespot_style;
		ccp->esm = ESMethodInputOnly;
		ccp->end_proc = xlcEnd;
		ccp->free_resources = xlcFreeResources;
		MyAddEventHandler(XtDisplay(w), reqwin,
				  DestroyNotify, StructureNotifyMask,
				  ClientDead, (XtPointer)ccp);
	    } else {
		TRACE(("\tchanged to kinput protocol\n"));
		ccp->protocol = kinput1_protocol;
		ccp->style = separate_style;
		ccp->esm = ESMethodInputOnly;
	    }
	} else if (prop != None) {
	    /* kinput2 protocol */
	    TRACE(("\tkinput2 protocol\n"));
	    ccp->protocol = kinput2_protocol;
	    ccp->style = separate_style;
	    ccp->esm = ESMethodInputOnly;
	    ccp->free_resources = kinput2FreeResources;
	    /* ccp->fix_proc = sendKeyCode0 */
	    getAttributeFromProperty(ccp, prop);
	} else {
	    /* old protocol */
	    ccp->protocol = kinput1_protocol;
	    ccp->style = separate_style;
	    ccp->esm = ESMethodInputOnly;
	}
    } else if (ccp->conversion != NULL) {
	/* now converting */
	if (ccp->protocol == xlc_protocol) {
	    /*
	     * xlc protocol uses CONVERSION_REQUEST event to notify
	     * the frontend of changing conversion attribute
	     */
	    return;
	}
	sendNegativeConversionNotify(kpw, reqwin, convatom);
	return;
    }

    if (attachConverter(ccp) == NULL) {
	sendNegativeConversionNotify(kpw, reqwin, convatom);
	return;
    }

    /* set target type (ignore client's request) */
    ccp->target = kpw->kinput.ctextAtom;

    /* use default property if not specified */
    if ((ccp->property = ev->data.l[3]) == None) {
	ccp->property = kpw->kinput.convStringAtom;
    }

    XtAddCallback(ccp->conversion, XtNtextCallback,
		  fixCallback, (XtPointer)ccp);
    XtAddCallback(ccp->conversion, XtNendCallback,
		  endCallback, (XtPointer)ccp);

    /* startup the conversion window */
    XtVaSetValues(ccp->conversion, XtNeventSelectMethod, ccp->esm, NULL);
    CControlStartConversion(ccp->conversion, ccp->reqwin,
			    ccp->attrmask, &ccp->attrs);

    /* send ConversionNotify to the client */
    sendConversionNotify(ccp);

    if (ccp->start_proc != NULL) (*ccp->start_proc)(ccp);
}

/*- ConversionEndRequestProc: CONVERSION_END_REQUEST event handler -*/
/* ARGSUSED */
static void
ConversionEndRequestProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;

    TRACE(("ConversionEndRequestProc(window=0x%lx)\n", ev->data.l[1]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }

    reqwin = (Window)ev->data.l[1];
    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion end request from unknown window\n"));
	return;
    }

    if (ccp->conversion != NULL) {
	CControlEndConversion(ccp->conversion);
	endProc(ccp, False);
    }
}

/*- ConversionCloseRequestProc: CONVERSION_CLOSE_REQUEST event handler (jinput only) -*/
/* ARGSUSED */
static void
ConversionCloseRequestProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;

    TRACE(("ConversionCloseRequestProc(window=0x%lx)\n", ev->data.l[2]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }
    if (ev->data.l[1] != JINPUT_PROTOCOL_VERSION) {
	/* wrong version number */
	DPRINT(("ConversionCloseRequestProc(): unknown version number (0x%lx)\n", ev->data.l[1]));
	return;
    }
    reqwin = ev->data.l[2];
    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion end request from unknown window\n"));
	return;
    }
    if (ccp->protocol != jinput_protocol) {
	DPRINT(("got jinput-specific event from a client that use other protocol\n"));
	return;
    }

    MyRemoveEventHandler(XtDisplay(w), reqwin, DestroyNotify,
			 ClientDead, (XtPointer)ccp);

    deleteClient(ccp);
}

/*- ConversionXYNotifyProc: CONVERSION_XY_NOTIFY event handler (jinput only) -*/
/* ARGSUSED */
static void
ConversionXYNotifyProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;
    JinputData *jdp;

    TRACE(("ConversionXYNotifyProc(window=0x%lx)\n", ev->data.l[2]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }
    if (ev->data.l[1] != JINPUT_PROTOCOL_VERSION) {
	/* wrong version number */
	DPRINT(("ConversionXYNotifyProc(): unknown version number (0x%lx)\n", ev->data.l[1]));
	return;
    }
    reqwin = ev->data.l[2];
    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion end request from unknown window\n"));
	return;
    }
    if (ccp->protocol != jinput_protocol) {
	DPRINT(("got jinput-specific event from a client that use other protocol\n"));
	return;
    }
    jdp = (JinputData *)ccp->data;
    jdp->rawspotx = ev->data.l[3];
    jdp->rawspoty = ev->data.l[4];

    ccp->attrmask |= CASpotLocation;
    ccp->attrs.spotx = jdp->rawspotx;
    ccp->attrs.spoty = jdp->rawspoty +
      ((ccp->attrmask & CAFonts) ? ccp->attrs.fonts[0]->ascent : 0);

    if (ccp->conversion != NULL) {
	CControlChangeAttributes(ccp->conversion, CASpotLocation, &ccp->attrs);
    }
}

/*- ConversionColorNotifyProc: CONVERSION_COLOR_NOTIFY event handler (jinput only) -*/
/* ARGSUSED */
static void
ConversionColorNotifyProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;

    TRACE(("ConversionColorNotifyProc(window=0x%lx)\n", ev->data.l[2]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }
    if (ev->data.l[1] != JINPUT_PROTOCOL_VERSION) {
	/* wrong version number */
	DPRINT(("ConversionColorNotifyProc(): unknown version number (0x%lx)\n", ev->data.l[1]));
	return;
    }
    reqwin = ev->data.l[2];
    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion end request from unknown window\n"));
	return;
    }
    if (ccp->protocol != jinput_protocol) {
	DPRINT(("got jinput-specific event from a client that use other protocol\n"));
	return;
    }
    ccp->attrmask |= CAColor;
    ccp->attrs.background = ev->data.l[3];
    ccp->attrs.foreground = ev->data.l[4];

    if (ccp->conversion != NULL) {
	CControlChangeAttributes(ccp->conversion, CAColor, &ccp->attrs);
    }
}

/*- ConversionFontsNotifyProc: CONVERSION_FONTS_NOTIFY event handler (jinput only) -*/
/* ARGSUSED */
static void
ConversionFontsNotifyProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;
    unsigned long attrmask;

    TRACE(("ConversionFontsNotifyProc(window=0x%lx)\n", ev->data.l[2]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }
    if (ev->data.l[1] != JINPUT_PROTOCOL_VERSION) {
	/* wrong version number */
	DPRINT(("ConversionFontsNotifyProc(): unknown version number (0x%lx)\n", ev->data.l[1]));
	return;
    }
    reqwin = ev->data.l[2];
    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion end request from unknown window\n"));
	return;
    }
    if (ccp->protocol != jinput_protocol) {
	DPRINT(("got jinput-specific event from a client that use other protocol\n"));
	return;
    }

    attrmask = CAFonts;
    if (ccp->attrmask & CAFonts) {
	if (ccp->attrs.fonts[0]->fid != ev->data.l[3]) {
	    XFreeFontInfo((char **)NULL, ccp->attrs.fonts[0], 1);
	    ccp->attrs.fonts[0] = XQueryFont(XtDisplay((Widget)kpw),
					     (XID)ev->data.l[3]);
	}
	if (ccp->attrs.fonts[1]->fid != ev->data.l[4]) {
	    XFreeFontInfo((char **)NULL, ccp->attrs.fonts[1], 1);
	    ccp->attrs.fonts[1] = XQueryFont(XtDisplay((Widget)kpw),
					     (XID)ev->data.l[4]);
	}
    } else {
	ccp->attrmask |= CAFonts;
	ccp->attrs.num_fonts = 2;
	ccp->attrs.fonts = (XFontStruct **)XtMalloc(sizeof(XFontStruct *) * 2);
	ccp->attrs.fonts[0] = XQueryFont(XtDisplay((Widget)kpw),
					 (XID)ev->data.l[3]);
	ccp->attrs.fonts[1] = XQueryFont(XtDisplay((Widget)kpw),
					 (XID)ev->data.l[4]);
    }
    if (ccp->attrmask & CASpotLocation) {
	JinputData *jdp = (JinputData *)ccp->data;
	ccp->attrs.spotx = jdp->rawspotx;
	ccp->attrs.spoty = jdp->rawspoty + ccp->attrs.fonts[0]->ascent;
	attrmask |= CASpotLocation;
    }

    if (ccp->conversion != NULL) {
	CControlChangeAttributes(ccp->conversion, attrmask, &ccp->attrs);
    }
}

/*- ConversionAttributeNotifyProc: CONVERSION_ATTRIBUTE_NOTIFY event handler (kinput2 only) -*/
/* ARGSUSED */
static void
ConversionAttributeNotifyProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;
    XFontStruct **fonts;
    Cardinal nfonts;

    TRACE(("ConversionAttributeNotifyProc(window=0x%lx)\n", ev->data.l[1]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }

    reqwin = (Window)ev->data.l[1];
    TRACE(("\treqwin=0x%lx\n", reqwin));

    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion attribute request from unknown window\n"));
	return;
    } else if (ccp->protocol != kinput2_protocol) {
	DPRINT(("get conversion attribute request from a client that doesn't use kinput2 protocol\n"));
	return;
    } else if (ccp->conversion == NULL) {
	/* not converting (this should not happen) */
	DPRINT(("got conversion attribute request before conversion start\n"));
	return;
    }

    /*
     * special treat for fonts -- because if they are changed,
     * you should release old fonts.
     */
    if (ccp->attrmask & CAFonts) {
	fonts = ccp->attrs.fonts;
	nfonts = ccp->attrs.num_fonts;
    } else {
	fonts = NULL;
	nfonts = 0;
    }

    ccp->attrmask = 0L;
    getAttributeFromEvent(ccp, event);

    /* change it */
    CControlChangeAttributes(ccp->conversion, ccp->attrmask, &ccp->attrs);

    if (ccp->attrmask & CAFonts) {
	if (fonts != NULL) {
	    /* fonts changed -- free old fonts */
	    Cardinal i;
	    for (i = 0; i < nfonts; i++) {
		CachedFreeFont(XtDisplay(w), fonts[i]);
	    }
	    XtFree((char *)fonts);
	}
    } else {
	/* restore fonts data */
	ccp->attrmask = CAFonts;
	ccp->attrs.fonts = fonts;
	ccp->attrs.num_fonts = nfonts;
    }
}

/*- ConversionCloseProc: CONVERSION_CLOSE event handler (xlc only) -*/
/* ARGSUSED */
static void
ConversionCloseProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XClientMessageEvent	*ev = &event->xclient;
    Window reqwin;
    ConvClient	*ccp;

    TRACE(("ConversionCloseProc(window=0x%lx)\n", ev->data.l[2]));
    /* is it a correct event? */
    if (!isCorrectClientEvent(kpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }
    reqwin = ev->data.l[1];
    if ((ccp = findClient(kpw, reqwin)) == NULL) {
	/* request from unknown client. just ignore */
	DPRINT(("got conversion end from unknown window\n"));
	return;
    }

    if (ccp->protocol != xlc_protocol && ccp->protocol != kinput1_protocol) {
	/*
	 * Only XLC protocol uses this event (CONVERSION_CLOSE ClientMessage),
	 * so the checking for kinput1 protocol seems to be unnecessary.
	 * ...Wrong. A client using kinput1 protocol and a client using
	 * XLC protocol with off-the-spot mode can't be distinguishable
	 * until you get this event.
	 */
	DPRINT(("got xlc-specific event from a client that use other protocol\n"));
	return;
    }

    MyRemoveEventHandler(XtDisplay(w), reqwin, DestroyNotify,
			 ClientDead, (XtPointer)ccp);
    if (ccp->conversion != NULL) {
	CControlEndConversion(ccp->conversion);
	endProc(ccp, False);
    }
    if (ccp->protocol == xlc_protocol) deleteClient(ccp);
}

/*
 *+ other event handler
 */

/*- XlcOnTheSpotChangedProc: ProptyNotify of "_XLC_ON_THE_SPOT" handler -*/ 
/* ARGSUSED */
static void
XlcOnTheSpotChangedProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;			/* not used */
Cardinal *num_args;		/* not used */
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    XPropertyEvent *ev = &(event->xproperty);
    ConvClient *client;

    TRACE(("XlcOnTheSpotChangedProc(window=0x%lx)\n", ev->window));

    if (ev->window != XtWindow(w) ||
	ev->atom != kpw->kinput.xlcOnTheSpotAtom) {
	DPRINT(("\tgot invalid PropertyNotify event.\n"));
	return;
    } else if (ev->state != PropertyNewValue) {
	return;
    }
    client = getXlcDataFromProperty(kpw, (ConvClient *)NULL, False);
    if (client == NULL) return;

    if (client->conversion != NULL) {
	CControlChangeAttributes(client->conversion,
				 client->attrmask, &client->attrs);
    }
}

/*- SelectionRequestProc: SelectionRequest event handler -*/
/*ARGSUSED*/
static void
SelectionRequestProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;			/* not used */
Cardinal *num_args;		/* not used */
{
    XSelectionRequestEvent *ev = &(event->xselectionrequest);
    XEvent repl;
    String params[1];
    Cardinal num_params;

    repl.xselection.type = SelectionNotify;
    repl.xselection.requestor = ev->requestor;
    repl.xselection.selection = ev->selection;
    repl.xselection.target = ev->target;
    repl.xselection.property = None;
    repl.xselection.time = ev->time;

    params[0] = XtClass(w)->core_class.class_name;
    num_params = 1;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "selectionError", "SelectionRequest", "WidgetError",
		    "%s: SelectionRequest event received",
		    params, &num_params);

    XSendEvent(ev->display, ev->requestor, False, NoEventMask, &repl);
}

/*- SelectionClearProc: SelectionClear event handler -*/
/* ARGSUSED */
static void
SelectionClearProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    KinputProtocolWidget kpw = (KinputProtocolWidget)w;
    ConvClient	*ccp;
    String params[1];
    Cardinal num_params;

    /* Selection owner changed. kill myself */

    /*
     * send ConversionEnd event to the clients before exit
     */
    for (ccp = kpw->kinput.clients; ccp; ccp = ccp->next) {
	if (ccp->reqwin != None) {
	    if (ccp->conversion != NULL) {
		CControlEndConversion(ccp->conversion);
	    }
	    endProc(ccp, False);
	}
    }

    params[0] = XtClass(w)->core_class.class_name;
    num_params = 1;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "selectionError", "SelectionClear", "WidgetError",
		    "%s: SelectionClear event received",
		    params, &num_params);

    XtDestroyWidget(w);
}

/*- ClientDead: DestroyNotify event handler (jinput and xlc) -*/
static void
ClientDead(ev, data)
XEvent *ev;
XtPointer data;
{
    ConvClient	*ccp = (ConvClient *)data;

    TRACE(("ClientDead(window=0x%lx)\n", ev->xdestroywindow.window));
    if (ev->type != DestroyNotify ||
	ev->xdestroywindow.window != ccp->reqwin) return;

    MyRemoveAllEventHandler(ev->xany.display, ccp->reqwin);
    deleteClient(ccp);
}
