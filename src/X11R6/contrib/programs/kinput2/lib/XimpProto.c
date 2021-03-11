#ifndef lint
static char *rcsid = "$Id: XimpProto.c,v 1.40 1994/06/03 06:35:29 ishisone Rel $";
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

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>

/* this widget needs X11R5 header files... */
#if defined(XlibSpecificationRelease) && XlibSpecificationRelease >= 5

#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/CharSet.h>
#include "XIMProto.h"
#include "XimpProtoP.h"
#include "ConvMgr.h"
#include "InputConv.h"
#include "OverConv.h"
#include "OffConv.h"
#include "MyDispatch.h"
#include "AsyncErr.h"
#include "ParseKey.h"


#define DEBUG_VAR debug_XimpProtocol
#include "DebugPrint.h"


#define PROTOCOL_VERSION_STR	"XIMP.3.5"
#define SERVER_NAME		"kinput2"
#define SERVER_VERSION		"1"
#define VENDOR_NAME		"SRA"


#define PREEDIT_MASK (XIMP_PRE_AREA_MASK|XIMP_PRE_FG_MASK|XIMP_PRE_BG_MASK|\
		      XIMP_PRE_COLORMAP_MASK|XIMP_PRE_BGPIXMAP_MASK|\
		      XIMP_PRE_LINESP_MASK|XIMP_PRE_CURSOR_MASK|\
		      XIMP_PRE_AREANEED_MASK|XIMP_PRE_SPOTL_MASK)
#define STATUS_MASK (XIMP_STS_AREA_MASK|XIMP_STS_FG_MASK|XIMP_STS_BG_MASK|\
		     XIMP_STS_COLORMAP_MASK|XIMP_STS_BGPIXMAP_MASK|\
		     XIMP_STS_LINESP_MASK|XIMP_STS_CURSOR_MASK|\
		     XIMP_STS_AREANEED_MASK|XIMP_STS_WINDOW_MASK)

#define MIN_LINE_SPACING	2
#define MIN_AREA_WIDTH		16
#define MIN_AREA_HEIGHT		10


/*- resource table -*/
static XtResource resources[] = {
#define offset(field) XtOffset(XimpProtocolWidget, ximp.field)
    { XtNlocaleName, XtCLocaleName, XtRString, sizeof(String),
	offset(localename), XtRImmediate, (XtPointer)NULL },
    { XtNserverName, XtCServerName, XtRString, sizeof(String),
	offset(servername), XtRString, (XtPointer)SERVER_NAME },
    { XtNforceDefaultServer, XtCForceDefaultServer, XtRBoolean, sizeof(Boolean),
	offset(forceDefaultServer), XtRImmediate, (XtPointer)False },
    { XtNconversionStartKeys, XtCConversionStartKeys, XtRString, sizeof(String),
	offset(convkeys), XtRImmediate, (XtPointer)NULL },
    { XtNinputObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(inputObjClass), XtRImmediate, (XtPointer)NULL },
    { XtNdisplayObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(displayObjClass), XtRImmediate, (XtPointer)NULL },
    { XtNdefaultFontList, XtCFontList, XtRString, sizeof(String),
	offset(defaultfontlist), XtRImmediate, (XtPointer)NULL },
    { XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	offset(foreground), XtRString, XtDefaultForeground },
    { XtNstatusWidth, XtCStatusWidth, XtRDimension, sizeof(Dimension),
	offset(statuswidth), XtRImmediate, (XtPointer)0 },
#undef offset
};

static void XimpMessageProc();
static void SelectionRequestProc();
static void SelectionClearProc();

/*- action table -*/
static XtActionsRec actions[] = {
    { "ximp-message",		XimpMessageProc },
    { "selection-request",	SelectionRequestProc },
    { "selection-clear",	SelectionClearProc },
};

/*- default translation -*/
static char translations[] =
    "<Message>_XIMP_PROTOCOL:	ximp-message()\n\
     <SelReq>:	selection-request()\n\
     <SelClr>:	selection-clear()";


/*- static function declarations -*/
static void ClassInitialize();
static void Initialize();
static void Destroy();
static void Realize();

static void getAtoms();
static int ownSelection();

static ConvClient *findClient();
static ConvClient *newClient();
static Widget attachConverter();
static void detachConverter();
static void deleteClient();

static Boolean isCorrectClientEvent();
static Boolean isCorrectWindowID();
static void initializeError();
static void checkLocale();
static void fillInDefaultAttributes();
static void computeAreaForStartup();
static void computeAreaForQuery();
static unsigned long makeConvAttributes();
static void getFonts();

static void setProperty();
static void setKeyProperty();
static void getAttributes();
static void getFocusProperty();
static void getPreeditFontProperty();
static void getStatusFontProperty();
static void getPreeditProperty();
static void getStatusProperty();
static Boolean readProperty();
static void setAttributes();
static void setFocusProperty();
static void setPreeditFontProperty();
static void setStatusFontProperty();
static void setPreeditProperty();
static void setStatusProperty();
static void writeProperty();

static void sendClientMessage8();
static void sendClientMessage32();
static void sendKeyEvent();
static void sendErrorEvent();
static void sendCreateRefusal();

static void fixCallback();
static void fixProc();
static void endCallback();
static void endProc();
static void unusedEventCallback();

static void ximpCreateMessageProc();
static void ximpDestroyMessageProc();
static void ximpBeginMessageProc();
static void ximpEndMessageProc();
static void ximpSetFocusMessageProc();
static void ximpUnsetFocusMessageProc();
static void ximpMoveMessageProc();
static void ximpResetMessageProc();
static void ximpSetValueMessageProc();
static void ximpGetValueMessageProc();
static void ximpKeyPressMessageProc();
static void ximpExtensionMessageProc();

static void ClientDead();

/*- XimpProtocolClassRec -*/
XimpProtocolClassRec ximpProtocolClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"XimpProtocol",
    /* widget_size		*/	sizeof(XimpProtocolRec),
    /* class_initialize		*/	ClassInitialize,
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
    /* set_values		*/	NULL,
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
  { /* ximpprotocol fields */
    /* ximp_dummy		*/	0
  }
};

WidgetClass ximpProtocolWidgetClass = (WidgetClass)&ximpProtocolClassRec;

static XimpInputStyle XimpStyles[] = {
    { XIMPreeditPosition|XIMStatusArea, overthespot_style },
    { XIMPreeditPosition|XIMStatusNothing, overthespot_style },
    { XIMPreeditArea|XIMStatusArea, offthespot_style },
    { XIMPreeditNothing|XIMStatusNothing, separate_style },
    { 0 },
};

/*
 *+ Core class methods
 */

/*- ClassInitialize: set supported locale list -*/
static void
ClassInitialize()
{
}
    
/*- Initialize: intern Atoms, get default fonts, etc. -*/
/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)new;

    if (xpw->ximp.localename == NULL) {
	initializeError(new, XtNlocaleName);
    } else if (xpw->ximp.inputObjClass == NULL) {
	initializeError(new, XtNinputObjectClass);
    } else if (xpw->ximp.displayObjClass == NULL) {
	initializeError(new, XtNdisplayObjectClass);
    }

    checkLocale(xpw, xpw->ximp.localename);
    xpw->ximp.localename = XtNewString(xpw->ximp.localename);

    xpw->ximp.servername = XtNewString(xpw->ximp.servername);

    xpw->ximp.clients = NULL;
    xpw->ximp.freeclients = NULL;
    xpw->ximp.icid = 1;
    xpw->ximp.propid = 0;

    if (xpw->ximp.defaultfontlist != NULL) {
	TRACE(("enter default fontlist <%s> into cache\n", xpw->ximp.defaultfontlist));
	/* extract fonts from default font list and enter them into cache */
	xpw->ximp.defaultfontlist = XtNewString(xpw->ximp.defaultfontlist);
	xpw->ximp.deffonts = FontBankGet(xpw->ximp.fontbank,
					 xpw->ximp.defaultfontlist,
					 &xpw->ximp.numdeffonts);
    } else {
	xpw->ximp.deffonts = NULL;
	xpw->ximp.numdeffonts = 0;
    }

    getAtoms(xpw);
}

/*- Destroy: free allocated memory -*/
static void
Destroy(w)
Widget w;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)w;
    ConvClient *client;

    XtFree(xpw->ximp.localename);
    XtFree(xpw->ximp.servername);
    if (xpw->ximp.defaultfontlist != NULL) XtFree(xpw->ximp.defaultfontlist);

    while (xpw->ximp.clients != NULL) {
	endProc(xpw->ximp.clients, False);
	deleteClient(xpw->ximp.clients);
	/*
	 * since deleteClient() removes the given client from client list
	 * and insert it in free list, following commented statement is
	 * not necessary.
	 *
	 * xpw->ximp.clients = xpw->ximp.clients->next;
	 */
    }

    /*
     * now, all the clients are deleted and moved into free list.
     */
    client = xpw->ximp.freeclients;
    while (client != NULL) {
	ConvClient *ccp = client;
	client = client->next;
	XtFree((char *)ccp);
    }

    /*
     * free cached fontlist
     */
    if (xpw->ximp.numdeffonts > 0) {
	FontBankFreeFonts(xpw->ximp.fontbank,
			  xpw->ximp.deffonts,
			  xpw->ximp.numdeffonts);
    }
    FontBankDestroy(xpw->ximp.fontbank);
}

/*- Realize: own selection -*/
static void
Realize(w, mask, value)
Widget w;
XtValueMask *mask;
XSetWindowAttributes *value;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)w;
    CoreWidgetClass super = (CoreWidgetClass)XtClass(w)->core_class.superclass;

    (*super->core_class.realize)(w, mask, value);

    setProperty(xpw);

    if (!ownSelection(xpw)) {
	String params[1];
	Cardinal num_params;

	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext(w),
			"selectionError", "ownSelection", "WidgetError",
			"%s: can't own selection", params, &num_params);

	XtDestroyWidget(w);
    } else {
	DPRINT(("\tselection owner: 0x%lx (%ld)\n", XtWindow(w), XtWindow(w)));
    }
}

/*
 *+ atom handling
 */

/*- getAtoms: intern atoms -*/
static void
getAtoms(xpw)
XimpProtocolWidget xpw;
{
    Display *dpy = XtDisplay((Widget)xpw);
    char buf[256];

#define MAKEATOM(s)	XInternAtom(dpy, s, False)
    (void)sprintf(buf, "_XIMP_%s", xpw->ximp.localename);
    xpw->ximp.selAtom1 = MAKEATOM(buf);
    (void)sprintf(buf, "_XIMP_%s@%s.%d",
		  xpw->ximp.localename,
		  xpw->ximp.servername,
		  DefaultScreen(XtDisplay((Widget)xpw)));
    xpw->ximp.selAtom2 = MAKEATOM(buf);

    xpw->ximp.ctextAtom = XA_COMPOUND_TEXT(dpy);

    xpw->ximp.ximpVersionAtom = MAKEATOM("_XIMP_VERSION");
    xpw->ximp.ximpStyleAtom = MAKEATOM("_XIMP_STYLE");
    xpw->ximp.ximpKeysAtom = MAKEATOM("_XIMP_KEYS");
    xpw->ximp.ximpServerNameAtom = MAKEATOM("_XIMP_SERVERNAME");
    xpw->ximp.ximpServerVersionAtom = MAKEATOM("_XIMP_SERVERVERSION");
    xpw->ximp.ximpVendorNameAtom = MAKEATOM("_XIMP_VENDORNAME");
    xpw->ximp.ximpExtensionsAtom = MAKEATOM("_XIMP_EXTENSIONS");
    xpw->ximp.ximpProtocolAtom = MAKEATOM("_XIMP_PROTOCOL");
    xpw->ximp.ximpFocusAtom = MAKEATOM("_XIMP_FOCUS");
    xpw->ximp.ximpPreeditAtom = MAKEATOM("_XIMP_PREEDIT");
    xpw->ximp.ximpStatusAtom = MAKEATOM("_XIMP_STATUS");
    xpw->ximp.ximpPreeditFontAtom = MAKEATOM("_XIMP_PREEDITFONT");
    xpw->ximp.ximpStatusFontAtom = MAKEATOM("_XIMP_STATUSFONT");
    xpw->ximp.ximpExtXimpBackFrontAtom = MAKEATOM("_XIMP_EXT_XIMP_BACK_FRONT");

#undef MAKEATOM
}

/*- ownSelection: own conversion selection -*/
static int
ownSelection(xpw)
XimpProtocolWidget xpw;
{
    Display *dpy = XtDisplay((Widget)xpw);
    Window w = XtWindow((Widget)xpw);

    TRACE(("ximpProtocol:ownSelection()\n"));

    if (xpw->ximp.forceDefaultServer ||
	XGetSelectionOwner(dpy, xpw->ximp.selAtom1) == None) {
	TRACE(("\tdefault server\n"));
	XSetSelectionOwner(dpy, xpw->ximp.selAtom1, w, CurrentTime);
    }
    TRACE(("\tspecific server\n"));
    XSetSelectionOwner(dpy, xpw->ximp.selAtom2, w, CurrentTime);

    return XGetSelectionOwner(dpy, xpw->ximp.selAtom2) == w;
}

/*
 *+ client data handling
 */

/*- findClient: get clientdata of given client -*/
static ConvClient *
findClient(xpw, id)
XimpProtocolWidget xpw;
int id;
{
    register ConvClient *ccp = xpw->ximp.clients;

    while (ccp != NULL) {
	if (ccp->id == id) return ccp;
	ccp = ccp->next;
    }

    return NULL;
}

/*- newClient: get a clientdata for new client -*/
static ConvClient *
newClient(xpw, client)
XimpProtocolWidget xpw;
Window client;
{
    ConvClient *ccp;

    if (xpw->ximp.freeclients != NULL) {
	/* get one from free list */
	ccp = xpw->ximp.freeclients;
	xpw->ximp.freeclients = ccp->next;
    } else {
	char buf[30];

	ccp = XtNew(ConvClient);
	(void)sprintf(buf, "_XIMP_STRING_%d", xpw->ximp.propid++);
	ccp->property = XInternAtom(XtDisplay((Widget)xpw), buf, False);
    }
    ccp->id = xpw->ximp.icid++;

    ccp->version = NULL;
    ccp->style = separate_style;	/* default */
    ccp->protocolwidget = (Widget)xpw;
    ccp->conversion = NULL;
    ccp->reqwin = client;
    ccp->focuswin = client;		/* default */
    ccp->xpattrs.fontlist = NULL;
    ccp->xsattrs.fontlist = NULL;
    ccp->xattrmask = 0L;
    ccp->defaultsfilledin = False;
    ccp->esm = ESMethodSelectFocus;	/* default */
    ccp->fonts = NULL;
    ccp->num_fonts = 0;
    ccp->resetting = False;
    ccp->event = NULL;

    ccp->next = xpw->ximp.clients;
    xpw->ximp.clients = ccp;

    return ccp;
}

/*- attachConverter: attach converter to the client -*/
static Widget
attachConverter(ccp)
ConvClient *ccp;
{
    WidgetClass class;
    XimpProtocolWidget xpw = (XimpProtocolWidget)ccp->protocolwidget;

    TRACE(("attachConverter(client window=0x%lx)\n", ccp->reqwin));
    if (ccp->conversion != NULL) return ccp->conversion;

    if (ccp->style == overthespot_style) {
	class = overTheSpotConversionWidgetClass;
    } else if (ccp->style == offthespot_style) {
	class = offTheSpotConversionWidgetClass;
    } else {
	class = separateConversionWidgetClass;
    }

    ccp->conversion = CMGetConverter(XtParent(ccp->protocolwidget),
				     ccp->reqwin, class,
				     xpw->ximp.inputObjClass,
				     xpw->ximp.displayObjClass);

    return ccp->conversion;
}

/*- detachConverter: detach converter from client -*/
static void
detachConverter(ccp)
ConvClient *ccp;
{
    TRACE(("detachConverter(client window=0x%lx)\n", ccp->reqwin));

    XtRemoveCallback(ccp->conversion, XtNtextCallback,
		     fixCallback, (XtPointer)ccp);
    XtRemoveCallback(ccp->conversion, XtNendCallback,
		     endCallback, (XtPointer)ccp);
    XtRemoveCallback(ccp->conversion, XtNunusedEventCallback,
		     unusedEventCallback, (XtPointer)ccp);

    CMReleaseConverter(XtParent(ccp->protocolwidget), ccp->conversion);
    ccp->conversion = NULL;
}

/*- deleteClient: delete specified client -*/
static void
deleteClient(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    ConvClient *ccp, *ccp0;

    TRACE(("deleteClient(client window=0x%lx)\n", client->reqwin));

    if (client->conversion != NULL) detachConverter(client);
    if (client->num_fonts > 0) {
	FontBankFreeFonts(xpw->ximp.fontbank,
			  client->fonts, client->num_fonts);
    }
    if (client->xpattrs.fontlist != NULL &&
	client->xpattrs.fontlist != xpw->ximp.defaultfontlist) {
	XFree(client->xpattrs.fontlist);
    }
    if (client->xsattrs.fontlist != NULL) {
	XFree(client->xsattrs.fontlist);
    }
    if (client->version != NULL) XtFree(client->version);

    for (ccp = xpw->ximp.clients, ccp0 = NULL;
	 ccp != NULL;
	 ccp0 = ccp, ccp = ccp->next) {
	if (ccp == client) {
	    if (ccp0 == NULL) {
		xpw->ximp.clients = ccp->next;
	    } else {
		ccp0->next = ccp->next;
	    }
	    /* put it back to free list */
	    client->next = xpw->ximp.freeclients;
	    xpw->ximp.freeclients = client;
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
isCorrectClientEvent(xpw, event)
XimpProtocolWidget xpw;
XEvent *event;
{
    XClientMessageEvent *ev = &(event->xclient);

    return (event->type == ClientMessage &&
	    ev->window == XtWindow((Widget)xpw) &&
	    ev->message_type == xpw->ximp.ximpProtocolAtom &&
	    ev->format == 32);
}

/*- isCorrectWindowID: is the given window ID valid? -*/
static Boolean
isCorrectWindowID(w, window, widthp, heightp)
Widget w;
Window window;
Dimension *widthp;
Dimension *heightp;
{
    XWindowAttributes attr;
    int status;
    XAEHandle h;

    h = XAESetIgnoreErrors(XtDisplay(w));
    status = XGetWindowAttributes(XtDisplay(w), window, &attr);
    XAEUnset(h);

    if (status == 0) return False;

    if (widthp != NULL) *widthp = attr.width;
    if (heightp != NULL) *heightp = attr.height;
    return True;
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

/*- checkLocale: check specified locale is supported -*/
static void
checkLocale(xpw, name)
XimpProtocolWidget xpw;
String name;
{
    XimpProtocolWidgetClass class = (XimpProtocolWidgetClass)XtClass((Widget)xpw);
    FontBank fontbank;

    TRACE(("checkLocale(localename=%s)\n", name));

    fontbank = FontBankCreate(XtDisplay((Widget)xpw), name);
    if (fontbank == NULL) {
	/* not supported locale name */
	String params[2];
	Cardinal num_params;

	params[0] = class->core_class.class_name;
	params[1] = name;
	num_params = 2;
	XtAppErrorMsg(XtWidgetToApplicationContext((Widget)xpw),
		      "initializeError", "localeNotSupported", "WidgetError",
		      "%s: specified locale %s is not supported (yet)",
		      params, &num_params);
    } else {
	xpw->ximp.fontbank = fontbank;
    }
}

/*- fillInDefaultAttributes: fill in unspecified attributes -*/
static void
fillInDefaultAttributes(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XimpPreEditAttributes *xpattr = &client->xpattrs;
    XimpStatusAttributes *xsattr = &client->xsattrs;
    unsigned long xmask = client->xattrmask;

    if (client->defaultsfilledin) return;

    /*
     * Compute reasonable default values for the unspecified
     * attributes except Area and AreaNeeded.
     */
    TRACE(("fillInDefaultAttributes()\n"));
    if (!(xmask & XIMP_FOCUS_WIN_MASK)) {
	client->focuswin = client->reqwin;
	client->focuswidth = client->reqwinwidth;
	client->focusheight = client->reqwinheight;
    }

    if (!(xmask & XIMP_PRE_FONT_MASK)) {
	xpattr->fontlist = xpw->ximp.defaultfontlist;
	getFonts(client, xpattr->fontlist);
	client->xattrmask |= XIMP_PRE_FONT_MASK;
    }
    if (!(xmask & XIMP_STS_FONT_MASK)) {
	xsattr->fontlist = XtNewString(xpattr->fontlist);
    }

    if (!(xmask & XIMP_PRE_FG_MASK)) {
	xpattr->foreground = xpw->ximp.foreground;
    }
    if (!(xmask & XIMP_PRE_BG_MASK)) {
	xpattr->background = xpw->core.background_pixel;
    }
    if (!(xmask & XIMP_PRE_COLORMAP_MASK)) {
	xpattr->colormap = xpw->core.colormap;
    }
    if (!(xmask & XIMP_PRE_BGPIXMAP_MASK)) {
	xpattr->bgpixmap = None;
    }
    if (!(xmask & XIMP_PRE_LINESP_MASK)) {
	Cardinal i;
	XFontStruct *font;
	int maxascent = 0, maxdescent = 0;

	for (i = 0; i < client->num_fonts; i++) {
	    font = client->fonts[i];
	    if (maxascent < font->ascent) maxascent = font->ascent;
	    if (maxdescent < font->descent) maxdescent = font->descent;
	}
	xpattr->linespacing = maxascent + maxdescent;
    }
    if (!(xmask & XIMP_PRE_CURSOR_MASK)) {
	xpattr->cursor = None;		/* ie use parent's cursor */
    }
    if (!(xmask & XIMP_PRE_SPOTL_MASK)) {
	xpattr->spotx = xpattr->spoty = 0;
    }
    if (!(xmask & XIMP_STS_FG_MASK)) {
	xsattr->foreground = xpattr->foreground;
    }
    if (!(xmask & XIMP_STS_BG_MASK)) {
	xsattr->background = xpattr->background;
    }
    if (!(xmask & XIMP_STS_COLORMAP_MASK)) {
	xsattr->colormap = xpattr->colormap;
    }
    if (!(xmask & XIMP_STS_BGPIXMAP_MASK)) {
	xsattr->bgpixmap = xpattr->bgpixmap;
    }
    if (!(xmask & XIMP_STS_LINESP_MASK)) {
	xsattr->linespacing = xpattr->linespacing;
    }
    if (!(xmask & XIMP_STS_CURSOR_MASK)) {
	xsattr->cursor = xpattr->cursor;
    }
    if (!(xmask & XIMP_STS_WINDOW_MASK)) {
	xsattr->statuswin = None;
    }

    client->defaultsfilledin = True;
}

/*- computeAreaForStartup: compute Area for conversion startup -*/
static void
computeAreaForStartup(client)
ConvClient *client;
{
    XimpPreEditAttributes *xpattr = &client->xpattrs;
    XimpStatusAttributes *xsattr = &client->xsattrs;
    unsigned long mask = client->xattrmask;

    TRACE(("computeAreaForStartup(client=0x%lx)\n", client->reqwin));

    if (client->style == separate_style ||
	client->style == overthespot_style) {
	/*
	 * These styles don't need status nor preedit area.
	 * The separate style simpley ignores them, and the
	 * over-the-spot style uses default value if not specified.
	 */
	return;
    }

    if ((mask & XIMP_STS_AREA_MASK) && (mask & XIMP_PRE_AREA_MASK)) return;

    /*
     * Compute default status/pre-edit area based on the AreaNeeded values.
     */
    computeAreaForQuery(client);

    if (!(mask & XIMP_STS_AREA_MASK)) {
	xsattr->areax = 0;
	xsattr->areay = client->reqwinheight - xsattr->neededheight;
	xsattr->areawidth = xsattr->neededwidth;
	xsattr->areaheight = xsattr->neededheight;
    }
    if (!(mask & XIMP_PRE_AREA_MASK)) {
	xpattr->areax = xsattr->areax + xsattr->areawidth;
	xpattr->areay = client->reqwinheight - xpattr->neededheight;
	xpattr->areawidth = xpattr->neededwidth;
	xpattr->areaheight = xpattr->neededheight;
    }
}

/*- computeAreaForQuery: compute Area and AreaNeeded for query from clients -*/
static void
computeAreaForQuery(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XimpPreEditAttributes *xpattr = &client->xpattrs;
    XimpStatusAttributes *xsattr = &client->xsattrs;
    unsigned long mask = client->xattrmask;
    int width, height;
    int maxwidth, maxheight;
    int fontheight;

    TRACE(("computeAreaForQuery(client=0x%lx)\n", client->reqwin));

    if (client->style != separate_style) {
	/*
	 * Get the size of the request window again.
	 * The size was checked when XIMP_CREATE message was received,
	 * but it is possible that the size has changed since then.
	 */
	(void)isCorrectWindowID(client->protocolwidget, client->reqwin,
				&client->reqwinwidth, &client->reqwinheight);
    }

    /*
     * Compute the dimensions of the status region.
     */
    if (client->style != separate_style) {
	/*
	 * Compute AreaNeeded value.
	 */
	fontheight = xsattr->linespacing + 2;
	maxwidth = maxheight = 0;
	if (mask & XIMP_STS_AREANEED_MASK) {
	    maxwidth = xsattr->neededwidth;
	    maxheight = xsattr->neededheight;
	    TRACE(("\tstatus areaNeeded was: (%d,%d)\n", maxwidth, maxheight));
	}

	if (xpw->ximp.statuswidth > 0) {
	    width = xpw->ximp.statuswidth;
	} else {
	    width = client->reqwinwidth / 5;	/* wild guess */
	    if (width < fontheight * 3) {
		width = fontheight * 3;		/* another wild guess */
	    }
	}
	height = fontheight;
    
	if (maxwidth > 0 && width > maxwidth) width = maxwidth;
	if (maxheight > 0 && height > maxheight) height = maxheight;
	if (width < MIN_AREA_WIDTH) width = MIN_AREA_WIDTH;
	if (height < MIN_AREA_HEIGHT) height = MIN_AREA_HEIGHT;
    
	xsattr->neededwidth = width;
	xsattr->neededheight = height;
	TRACE(("\tstatus areaNeeded is now: (%d, %d)\n", width, height));
    
	/*
	 * If client has not specified the status area yet,
	 * supply default value.
	 */
	if (!(mask & XIMP_STS_AREA_MASK)) {
	    xsattr->areax = 0;
	    xsattr->areay = client->reqwinheight - xsattr->neededheight;
	    xsattr->areawidth = xsattr->neededwidth;
	    xsattr->areaheight = xsattr->neededheight;
	}
    }

    /*
     * Compute the dimensions of the pre-edit region.
     */
    if (client->style == offthespot_style) {
	/*
	 * Compute AreaNeeded value.
	 */
	fontheight = xpattr->linespacing + 2;
	maxwidth = maxheight = 0;
	if (mask & XIMP_PRE_AREANEED_MASK) {
	    maxwidth = xpattr->neededwidth;
	    maxheight = xpattr->neededheight;
	    TRACE(("\tpreedit areaNeeded was: (%d,%d)\n", maxwidth, maxheight));
	}

	width = client->reqwinwidth - xsattr->neededwidth;
	height = fontheight;

	if (maxwidth > 0 && width > maxwidth) width = maxwidth;
	if (maxheight > 0 && height > maxheight) height = maxheight;
	if (width < MIN_AREA_WIDTH) width = MIN_AREA_WIDTH;
	if (height < MIN_AREA_HEIGHT) height = MIN_AREA_HEIGHT;

	xpattr->neededwidth = width;
	xpattr->neededheight = height;
	TRACE(("\tpreedit areaNeeded is now: (%d, %d)\n", width, height));

	/*
	 * If client has not specified the status area yet,
	 * supply default value.
	 */
	if (!(mask & XIMP_PRE_AREA_MASK)) {
	    xpattr->areax = xsattr->neededwidth;
	    xpattr->areay = client->reqwinheight - xpattr->neededheight;
	    xpattr->areawidth = xpattr->neededwidth;
	    xpattr->areaheight = xpattr->neededheight;
	}
    } else if (client->style == overthespot_style) {
	/*
	 * No need to calculate AreaNeeded value, which is
	 * ignored by the client.  Just calculate default
	 * Area if not specified.
	 */
	if (!(mask & XIMP_PRE_AREA_MASK)) {
	    xpattr->areax = 0;
	    xpattr->areay = 0;
	    xpattr->areawidth = client->focuswidth;
	    xpattr->areaheight = client->focusheight;
	}
    }
}

/*- makeConvAttributes: -*/
static unsigned long
makeConvAttributes(client, attr)
ConvClient *client;
ConversionAttributes *attr;
{
    XimpPreEditAttributes *xpattr = &client->xpattrs;
    XimpStatusAttributes *xsattr = &client->xsattrs;
    unsigned long xmask = client->xattrmask;
    unsigned long mask;

    TRACE(("makeConvAttributes()\n"));
    mask = 0L;

    /* focus window */
    attr->focuswindow = client->focuswin;
    mask |= CAFocusWindow;

    if (client->style == overthespot_style ||
	client->style == offthespot_style) {

	/* client area */
	if (client->style == offthespot_style ||
	    (xmask & XIMP_PRE_AREA_MASK)) {
	    attr->clientarea.x = xpattr->areax;
	    attr->clientarea.y = xpattr->areay;
	    attr->clientarea.width = xpattr->areawidth;
	    attr->clientarea.height = xpattr->areaheight;
	    mask |= CAClientArea;
	}

	/* foreground/background */
	attr->foreground = xpattr->foreground;
	attr->background = xpattr->background;
	mask |= CAColor;

	/* colormap */
	if (xmask & XIMP_PRE_COLORMAP_MASK) {
	    attr->colormap = xpattr->colormap;
	    mask |= CAColormap;
	}

	/* background pixmap */
	if (xmask & XIMP_PRE_BGPIXMAP_MASK) {
	    attr->background_pixmap = xpattr->bgpixmap;
	    mask |= CABackgroundPixmap;
	}

	/* line spacing */
	if (xmask & XIMP_PRE_LINESP_MASK) {
	    attr->linespacing = xpattr->linespacing;
	    mask |= CALineSpacing;
	}

	/* cursor */
	if (xmask & XIMP_PRE_CURSOR_MASK) {
	    attr->cursor = xpattr->cursor;
	    mask |= CACursor;
	}

	/* status area */
	/* offTheSpotConversion doesn't allow status area left unspecified */
	if (client->style == offthespot_style ||
	    (xmask & XIMP_STS_AREA_MASK)) {
	    attr->statusarea.x = xsattr->areax;
	    attr->statusarea.y = xsattr->areay;
	    attr->statusarea.width = xsattr->areawidth;
	    attr->statusarea.height = xsattr->areaheight;
	    mask |= CAStatusArea;
	}

	/* font */
	attr->fonts = client->fonts;
	attr->num_fonts = client->num_fonts;
	mask |= CAFonts;
    }

    if (client->style == overthespot_style) {
	/* spot location */
	if (xmask & XIMP_PRE_SPOTL_MASK) {
	    attr->spotx = xpattr->spotx;
	    attr->spoty = xpattr->spoty;
	    mask |= CASpotLocation;
	}
    }
    return mask;
}

/*- getFonts: get fonts from specified fontnamelist -*/
static void
getFonts(client, fontnamelist)
ConvClient *client;
String fontnamelist;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;

    TRACE(("getFonts()\n"));
    TRACE(("\tfontnamelist: %s\n", fontnamelist));

    client->fonts = FontBankGet(xpw->ximp.fontbank, fontnamelist,
				&client->num_fonts);
}


/*
 *+ property handling
 */

/*- setProperty: set information properties -*/
static void
setProperty(xpw)
XimpProtocolWidget xpw;
{
    Display *dpy = XtDisplay((Widget)xpw);
    Window win = XtWindow((Widget)xpw);
    XimpInputStyle *xisp;
    unsigned long styles[20];
    Cardinal nstyles;
    unsigned long extensions[10];
    Cardinal nextensions;

    TRACE(("setProperty()\n"));

#define SETPROPERTY(p, t, f, d, n) \
    XChangeProperty(dpy, win, p, t, f, PropModeReplace, (unsigned char *)d, n)

    SETPROPERTY(xpw->ximp.ximpVersionAtom, XA_STRING, 8,
		PROTOCOL_VERSION_STR, strlen(PROTOCOL_VERSION_STR));
    SETPROPERTY(xpw->ximp.ximpServerNameAtom, XA_STRING, 8,
		SERVER_NAME, strlen(xpw->ximp.servername));
    SETPROPERTY(xpw->ximp.ximpServerVersionAtom, XA_STRING, 8,
		SERVER_VERSION, strlen(SERVER_VERSION));
    SETPROPERTY(xpw->ximp.ximpVendorNameAtom, XA_STRING, 8,
		VENDOR_NAME, strlen(VENDOR_NAME));

    for (xisp = XimpStyles, nstyles = 0; xisp->ximstyle != 0;
	 xisp++, nstyles++) {
	styles[nstyles] = xisp->ximstyle;
    }
    SETPROPERTY(xpw->ximp.ximpStyleAtom, xpw->ximp.ximpStyleAtom, 32,
		styles, nstyles);

    nextensions = 0;
    extensions[nextensions++] = xpw->ximp.ximpExtXimpBackFrontAtom;
    SETPROPERTY(xpw->ximp.ximpExtensionsAtom, xpw->ximp.ximpExtensionsAtom, 32,
		extensions, nextensions);

    setKeyProperty(xpw);

#undef SETPROPERTY
}

/*- setKeyProperty: set _XIM_KEYS property -*/
static void
setKeyProperty(xpw)
XimpProtocolWidget xpw;
{
    long data[100];	/* enough */
    char line[256];	/* enough */
    Display *dpy = XtDisplay((Widget)xpw);
    int nkeys = 0;
    String p, q;
    int c;

    if ((p = xpw->ximp.convkeys) != NULL) {
	TRACE(("setKeyProperty(%s)\n", p));
	do {
	    KeySym keysym;
	    long mods, chk_mods;

	    q = line;
	    while ((c = *p++) != '\0' && c != '\n') {
		*q++ = c;
	    }
	    *q = '\0';
	    if (ParseKeyEvent(line, &keysym, &mods, &chk_mods)) {
		data[nkeys * 3] = mods;
		data[nkeys * 3 + 1] = chk_mods;
		data[nkeys * 3 + 2] = keysym;
		nkeys++;
	    }
	} while  (c != '\0');
    }

    XChangeProperty(dpy, XtWindow((Widget)xpw), xpw->ximp.ximpKeysAtom,
		    xpw->ximp.ximpKeysAtom, 32, PropModeReplace,
		    (unsigned char *)data, nkeys * 3);
}

/*- getVersionProperty: get _XIMP_VERSION property -*/
static void
getVersionProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    String data;
    unsigned long len;

    TRACE(("getVersionProperty()\n"));
    if (!readProperty(client, xpw->ximp.ximpVersionAtom, XA_STRING, 8,
		      (unsigned char **)&data, &len)) {
	DPRINT(("can't read _XIMP_VERSION property\n"));
	client->version = NULL;
	return;
    }
    TRACE(("\tclient version is %s\n", data));

    /* what to do? */

    client->version = data;
}

/*- getAttributes: read properties and set conversion attributes -*/
static void
getAttributes(client, mask)
ConvClient *client;
unsigned long mask;
{
    if (mask & XIMP_FOCUS_WIN_MASK) {
	getFocusProperty(client);
    }
    if (mask & XIMP_PRE_FONT_MASK) {
	getPreeditFontProperty(client);
    }
    if (mask & XIMP_STS_FONT_MASK) {
	getStatusFontProperty(client);
    }
    if (mask & PREEDIT_MASK) {
	getPreeditProperty(client, mask & PREEDIT_MASK);
    }
    if (mask & STATUS_MASK) {
	getStatusProperty(client, mask & STATUS_MASK);
    }
}

/*- getFocusProperty: get _XIMP_FOCUS property -*/
static void
getFocusProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    unsigned char *data;
    unsigned long len;
    Window focus;
    Dimension w, h;

    TRACE(("getFocusProperty()\n"));
    if (!readProperty(client, xpw->ximp.ximpFocusAtom, XA_WINDOW, 32,
		      &data, &len)) {
	DPRINT(("can't read _XIMP_FOCUS property\n"));
	return;
    } else if (len != 1) {
	DPRINT(("length of _XIMP_FOCUS property is not 1\n"));
	XtFree((char *)data);
	return;
    }

    focus = *(Window *)data;
    XtFree((char *)data);
    TRACE(("\tfocus window=0x%lx\n", focus));

    if (!isCorrectWindowID((Widget)xpw, focus, &w, &h)) {
	DPRINT(("specified focus window doesn't exist\n"));
	sendErrorEvent(client, XIMP_BadFocusWindow);
	return;
    }

    client->focuswin = focus;
    client->focuswidth = w;
    client->focusheight = h;
    client->xattrmask |= XIMP_FOCUS_WIN_MASK;
}

/*- getPreeditFontProperty: get _XIMP_PREEDITFONT property -*/
static void
getPreeditFontProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    char *data;
    unsigned long len;

    TRACE(("getPreeditFontProperty()\n"));
    if (!readProperty(client, xpw->ximp.ximpPreeditFontAtom, XA_STRING, 8,
		      (unsigned char **)&data, &len)) {
	DPRINT(("can't read _XIMP_PREEDITFONT property\n"));
	return;
    }

    if (client->xpattrs.fontlist != NULL) {
	if (!strcmp(data, client->xpattrs.fontlist)) {
	    XtFree(data);
	    return;
	}
	if (client->xpattrs.fontlist != xpw->ximp.defaultfontlist) {
	    XtFree(client->xpattrs.fontlist);
	}
    }
    client->xpattrs.fontlist = data;
    client->xattrmask |= XIMP_PRE_FONT_MASK;

    /* extract fonts to be used */
    getFonts(client, data);
}

/*- getStatusFontProperty: get _XIMP_STATUSFONT property -*/
static void
getStatusFontProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    unsigned char *data;
    unsigned long len;

    TRACE(("getStatusFontProperty()\n"));
    /*
     * actually kinput2 doesn't use this value. but in order to answer
     * client's GETVALUE request, we'll keep it.
     */
    if (!readProperty(client, xpw->ximp.ximpStatusFontAtom, XA_STRING, 8,
		      &data, &len)) {
	DPRINT(("can't read _XIMP_STATUSFONT property\n"));
	return;
    }

    if (client->xsattrs.fontlist != NULL) {
	XtFree(client->xsattrs.fontlist);
    }
    client->xsattrs.fontlist = (String)data;
    client->xattrmask |= XIMP_STS_FONT_MASK;
}

/*- getPreeditProperty: get _XIMP_PREEDIT property -*/
static void
getPreeditProperty(client, mask)
ConvClient *client;
unsigned long mask;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XimpPreEditAttributes *xpattr = &client->xpattrs;
    unsigned long *data;
    unsigned long len;

    TRACE(("getPreeditProperty()\n"));
    if (!readProperty(client, xpw->ximp.ximpPreeditAtom,
		      xpw->ximp.ximpPreeditAtom, 32,
		      (unsigned char **)&data, &len)) {
	DPRINT(("can't read _XIMP_PREEDIT property\n"));
	return;
    } else if (len < 14) {
	DPRINT(("length of _XIMP_PREEDIT property is less than 14(%ld)\n",len));
	XtFree((char *)data);
	return;
    }

    client->xattrmask |= mask;

    /* data[0]-data[3]: Area.{x,y,width,height} */
    if (mask & XIMP_PRE_AREA_MASK) {
	xpattr->areax = data[0];
	xpattr->areay = data[1];
	xpattr->areawidth = data[2];
	xpattr->areaheight = data[3];
	if (xpattr->areawidth == 0 || xpattr->areaheight == 0) {
	    client->xattrmask &= ~XIMP_PRE_AREA_MASK;
	    DPRINT(("invalid area specified:\n"));
	}
	TRACE(("\tArea: (%ld,%ld)-(%ld,%ld)\n",data[0],data[1],data[2],data[3]));
    }
    /* data[4]: Foreground */
    if (mask & XIMP_PRE_FG_MASK) {
	xpattr->foreground = data[4];
	TRACE(("\tForeground: %ld\n", data[4]));
    }
    /* data[5]: Background */
    if (mask & XIMP_PRE_BG_MASK) {
	xpattr->background = data[5];
	TRACE(("\tBackground: %ld\n", data[5]));
    }
    /* data[6]: Colormap */
    if (mask & XIMP_PRE_COLORMAP_MASK) {
	xpattr->colormap = data[6];
	TRACE(("\tColormap: 0x%lx\n", data[6]));
    }
    /* data[7]: BackgroundPixmap */
    if (mask & XIMP_PRE_BGPIXMAP_MASK) {
	xpattr->bgpixmap = data[7];
	TRACE(("\tBackgroundPixmap: 0x%lx\n", data[7]));
    }
    /* data[8]: LineSpacing */
    if (mask & XIMP_PRE_LINESP_MASK) {
	if (data[8] < MIN_LINE_SPACING) {
	    client->xattrmask &= ~XIMP_PRE_LINESP_MASK;
	    DPRINT(("specified line spacing is too small (%ld)\n", data[8]));
	} else {
	    xpattr->linespacing = data[8];
	    TRACE(("\tLineSpacing: %ld\n", data[8]));
	}
    }
    /* data[9]: Cursor */
    if (mask & XIMP_PRE_CURSOR_MASK) {
	xpattr->cursor = data[9];
	TRACE(("\tCursor: 0x%lx\n", data[9]));
    }
    /* data[10]-data[11]: AreaNeeded.{width,height} */
    if (mask & XIMP_PRE_AREANEED_MASK) {
	xpattr->neededwidth = data[10];
	xpattr->neededheight = data[11];
	TRACE(("\tAreaNeeded: %ld,%ld\n", data[10], data[11]));
    }
    /* data[12]-data[13]: SpotLocation.{x,y} */
    if (mask & XIMP_PRE_SPOTL_MASK) {
	xpattr->spotx = data[12];
	xpattr->spoty = data[13];
        TRACE(("\tSpotLocation: %ld,%ld\n", data[12], data[13]));
    }

    XtFree((char *)data);
}

/*- getStatusProperty: get _XIMP_STATUS property -*/
static void
getStatusProperty(client, mask)
ConvClient *client;
unsigned long mask;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XimpStatusAttributes *xsattr = &client->xsattrs;
    unsigned long *data;
    unsigned long len;

    TRACE(("getStatusProperty()\n"));
    if (!readProperty(client, xpw->ximp.ximpStatusAtom,
		      xpw->ximp.ximpStatusAtom, 32,
		      (unsigned char **)&data, &len)) {
	DPRINT(("can't read _XIMP_STATUS property\n"));
	return;
    } else if (len < 12) {
	DPRINT(("length of _XIMP_STATUS property is less than 12(%ld)\n",len));
	XtFree((char *)data);
	return;
    }

    client->xattrmask |= mask;

    /* data[0]-data[3]: Area.{x,y,width,height} */
    if (mask & XIMP_STS_AREA_MASK) {
	xsattr->areax = data[0];
	xsattr->areay = data[1];
	xsattr->areawidth = data[2];
	xsattr->areaheight = data[3];
	if (xsattr->areawidth == 0 || xsattr->areaheight == 0) {
	    client->xattrmask &= ~XIMP_STS_AREA_MASK;
	    DPRINT(("invalid area specified:\n"));
	} 
	TRACE(("\tArea: (%ld,%ld)-(%ld,%ld)\n",
		data[0],data[1],data[2],data[3]));
    }
    /* data[4]: Foreground */
    if (mask & XIMP_STS_FG_MASK) {
	xsattr->foreground = data[4];
	TRACE(("\tForeground: %ld\n", data[4]));
    }
    /* data[5]: Background */
    if (mask & XIMP_STS_BG_MASK) {
	xsattr->background = data[5];
	TRACE(("\tBackground: %ld\n", data[5]));
    }
    /* data[6]: Colormap */
    if (mask & XIMP_STS_COLORMAP_MASK) {
	xsattr->colormap = data[6];
	TRACE(("\tColormap: 0x%lx\n", data[6]));
    }
    /* data[7]: BackgroundPixmap */
    if (mask & XIMP_STS_BGPIXMAP_MASK) {
	xsattr->bgpixmap = data[7];
	TRACE(("\tBackgroundPixmap: 0x%lx\n", data[7]));
    }
    /* data[8]: LineSpacing */
    if (mask & XIMP_STS_LINESP_MASK) {
	if (data[8] < MIN_LINE_SPACING) {
	    client->xattrmask &= ~XIMP_STS_LINESP_MASK;
	    DPRINT(("specified line spacing is too small (%ld)\n", data[8]));
	} else {
	    xsattr->linespacing = data[8];
	    TRACE(("\tLineSpacing: %ld\n", data[8]));
	}
    }
    /* data[9]: Cursor */
    if (mask & XIMP_STS_CURSOR_MASK) {
	xsattr->cursor = data[9];
	TRACE(("\tCursor: 0x%lx\n", data[9]));
    }
    /* data[10]-data[11]: AreaNeeded.{width,height} */
    if (mask & XIMP_STS_AREANEED_MASK) {
	xsattr->neededwidth = data[10];
	xsattr->neededheight = data[11];
	TRACE(("\tAreaNeeded: %ld,%ld\n", data[10], data[11]));
    }
    /* data[12]: StatusWindowID -- not suppoted by kinput2 */
    if (len > 12 && (mask & XIMP_STS_WINDOW_MASK)) {
	xsattr->statuswin = None;	/* ignore specified value */
	TRACE(("\tStatusWindow(not supported): 0x%lx\n", data[12]));
    }

    XtFree((char *)data);
}

/*- readProperty: read specified property of the client window -*/
static Boolean
readProperty(client, prop, type, format, datapp, lenp)
ConvClient *client;
Atom prop;
Atom type;
int format;
unsigned char **datapp;
unsigned long *lenp;
{
    Atom realtype;
    int realformat;
    unsigned long bytesafter;

    *datapp = NULL;
    /*
     * generally, XGetWindowProperty can generate BadAtom, BadValue and
     * BadWindow errors. but in this case, none of those errors can occur.
     * atoms are valid, offset 0 won't cause BadValue, and window ID is
     * already validated. (strictly speaking, there's a chance of getting
     * BadWindow if the client window destroyed after it was validated.
     * let's forget it for a while :-) so we don't have to be careful to
     * errors.
     */
    (void)XGetWindowProperty(XtDisplay(client->protocolwidget),
			     client->reqwin,
			     prop, 0L, 1000L, True, type,
			     &realtype, &realformat, lenp,
			     &bytesafter, datapp);
    if (realtype == None) {
	/* specified property doesn't exist */
	sendErrorEvent(client, XIMP_BadProperty);
	return False;
    } else if (realtype != type) {
	/* wrong type */
	sendErrorEvent(client, XIMP_BadPropertyType);
	return False;
    } else if (realformat != format) {
	/* wrong format */
	if (*datapp != NULL) XtFree((char *)*datapp);
	*datapp = NULL;
	/* there's no XIMP_BadFormat error. use XIMP_BadPropertyType instead */
	sendErrorEvent(client, XIMP_BadPropertyType);
	return False;
    }
    return True;
}

/*- setAttributes: set properties according to the conversion attributes -*/
static void
setAttributes(client, mask)
ConvClient *client;
unsigned long mask;
{
    if (mask & XIMP_FOCUS_WIN_MASK) {
	setFocusProperty(client);
    }
    if (mask & XIMP_PRE_FONT_MASK) {
	setPreeditFontProperty(client);
    }
    if (mask & XIMP_STS_FONT_MASK) {
	setStatusFontProperty(client);
    }
    if (mask & PREEDIT_MASK) {
	setPreeditProperty(client, mask);
    }
    if (mask & STATUS_MASK) {
	setStatusProperty(client, mask);
    }
}

/*- setFocusProperty: set _XIMP_FOCUS property -*/
static void
setFocusProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;

    TRACE(("setFocusProperty()\n"));
    writeProperty(client, xpw->ximp.ximpFocusAtom, XA_WINDOW, 32,
		  (unsigned char *)&client->focuswin, 1);
}

/*- setPreeditFontProperty: set _XIMP_PREEDITFONT property -*/
static void
setPreeditFontProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;

    TRACE(("setPreeditFontProperty()\n"));
    writeProperty(client, xpw->ximp.ximpPreeditFontAtom, XA_STRING, 8,
		  (unsigned char *)client->xpattrs.fontlist,
		  strlen(client->xpattrs.fontlist));
}

/*- setStatusFontProperty: set _XIMP_STATUSFONT property -*/
static void
setStatusFontProperty(client)
ConvClient *client;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;

    TRACE(("setStatusFontProperty()\n"));
    writeProperty(client, xpw->ximp.ximpStatusFontAtom, XA_STRING, 8,
		  (unsigned char *)client->xsattrs.fontlist,
		  strlen(client->xsattrs.fontlist));
}

/*- setPreeditProperty: set _XIMP_PREEDIT property -*/
static void
setPreeditProperty(client, mask)
ConvClient *client;
unsigned long mask;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XimpPreEditAttributes *xpattr = &client->xpattrs;
    long data[14];

    TRACE(("setPreeditProperty()\n"));

    /* data[0]-data[3]: Area.{x,y,width,height} */
    if (mask & XIMP_PRE_AREA_MASK) {
	data[0] = xpattr->areax;
	data[1] = xpattr->areay;
	data[2] = xpattr->areawidth;
	data[3] = xpattr->areaheight;
    }
    /* data[4]: Foreground */
    if (mask & XIMP_PRE_FG_MASK) {
	 data[4] = xpattr->foreground;
    }
    /* data[5]: Background */
    if (mask & XIMP_PRE_BG_MASK) {
	data[5] = xpattr->background;
    }
    /* data[6]: Colormap */
    if (mask & XIMP_PRE_COLORMAP_MASK) {
	data[6] = xpattr->colormap;
    }
    /* data[7]: BackgroundPixmap */
    if (mask & XIMP_PRE_BGPIXMAP_MASK) {
	data[7] = xpattr->bgpixmap;
    }
    /* data[8]: LineSpacing */
    if (mask & XIMP_PRE_LINESP_MASK) {
	data[8] = xpattr->linespacing;
    }
    /* data[9]: Cursor */
    if (mask & XIMP_PRE_CURSOR_MASK) {
	data[9] = xpattr->cursor;
    }
    /* data[10]-data[11]: AreaNeeded.{width,height} */
    if (mask & XIMP_PRE_AREANEED_MASK) {
	data[10] = xpattr->neededwidth;
	data[11] = xpattr->neededheight;
    }
    /* data[12]-data[13]: SpotLocation.{x,y} */
    if (mask & XIMP_PRE_SPOTL_MASK) {
	data[12] = xpattr->spotx;
	data[13] = xpattr->spoty;
    }

    writeProperty(client, xpw->ximp.ximpPreeditAtom,
		  xpw->ximp.ximpPreeditAtom, 32,
		  (unsigned char *)data, 14);
}

/*- setStautsProperty: set _XIMP_STATUS property -*/
static void
setStatusProperty(client, mask)
ConvClient *client;
unsigned long mask;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XimpStatusAttributes *xsattr = &client->xsattrs;
    long data[13];

    TRACE(("setStatusProperty()\n"));

    /* data[0]-data[3]: Area.{x,y,width,height} */
    if (mask & XIMP_STS_AREA_MASK) {
	data[0] = xsattr->areax = data[0];
	data[1] = xsattr->areay = data[1];
	data[2] = xsattr->areawidth = data[2];
	data[3] = xsattr->areaheight = data[3];
    }
    /* data[4]: Foreground */
    if (mask & XIMP_STS_FG_MASK) {
	data[4] = xsattr->foreground;
    }
    /* data[5]: Background */
    if (mask & XIMP_STS_BG_MASK) {
	data[5] = xsattr->background;
    }
    /* data[6]: Colormap */
    if (mask & XIMP_STS_COLORMAP_MASK) {
	data[6] = xsattr->colormap;
    }
    /* data[7]: BackgroundPixmap */
    if (mask & XIMP_STS_BGPIXMAP_MASK) {
	data[7] = xsattr->bgpixmap;
    }
    /* data[8]: LineSpacing */
    if (mask & XIMP_STS_LINESP_MASK) {
	data[8] = xsattr->linespacing;
    }
    /* data[9]: Cursor */
    if (mask & XIMP_STS_CURSOR_MASK) {
	data[9] = xsattr->cursor;
    }
    /* data[10]-data[11]: AreaNeeded.{width,height} */
    if (mask & XIMP_STS_AREANEED_MASK) {
	data[10] = xsattr->neededwidth;
	data[11] = xsattr->neededheight;
    }
    /* data[12]: StatusWindowID -- not suppoted by kinput2 */
    if (mask & XIMP_STS_WINDOW_MASK) {
	data[12] = xsattr->statuswin;
    }

    writeProperty(client, xpw->ximp.ximpStatusAtom,
		  xpw->ximp.ximpStatusAtom, 32,
		  (unsigned char *)data, 13);
}

/*- writeProperty: write specified property of the client window -*/
static void
writeProperty(client, prop, type, format, datap, len)
ConvClient *client;
Atom prop;
Atom type;
int format;
unsigned char *datap;
int len;
{
    /*
     * generally, XChangeWindowProperty can generate BadAlloc, BadAtom,
     * BadMatch, BadValue and BadWindow errors. but in this case, none of
     * those errors except BadAlloc can occur.  atoms and values to be
     * specified are valid (at least if the program is correct :-), mode
     * PropModeReplace won't cause BadMatch, and window ID is already
     * validated.  so, if we assume amount of memory is infinite :-), we
     * don't have to be careful to errors.
     */
    (void)XChangeProperty(XtDisplay(client->protocolwidget),
			  client->reqwin, prop, type, format,
			  PropModeReplace, datap, len);
}

/*
 *+ event sending
 */

/*- sendClientMessage8: send a clientmessage event (format=8) -*/
static void
sendClientMessage8(client, str, len)
ConvClient *client;
char *str;
int len;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XEvent event;
    
    event.xclient.type = ClientMessage;
    event.xclient.window = client->focuswin;
    event.xclient.message_type = xpw->ximp.ximpProtocolAtom;
    event.xclient.format = 8;

    /* client ID must be stored in network byte order (ie MSB first) */
    event.xclient.data.b[0] = (client->id >> 24) & 0xff;
    event.xclient.data.b[1] = (client->id >> 16) & 0xff;
    event.xclient.data.b[2] = (client->id >> 8) & 0xff;
    event.xclient.data.b[3] = client->id & 0xff;

    event.xclient.data.b[4] = len;

    (void)strncpy(&event.xclient.data.b[5], str, 20 - 5);

    XSendEvent(XtDisplay((Widget)xpw), event.xclient.window,
	       False, NoEventMask, &event);
}

/*- sendClientMessage32: send a clientmessage event (format=32) -*/
static void
sendClientMessage32(client, type, l1, l2, l3, l4)
ConvClient *client;
int type;
unsigned long l1, l2, l3, l4;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;
    XEvent event;

    event.xclient.type = ClientMessage;
    event.xclient.window = client->focuswin;
    event.xclient.message_type = xpw->ximp.ximpProtocolAtom;
    event.xclient.format = 32;
    event.xclient.data.l[0] = type;
    event.xclient.data.l[1] = l1;
    event.xclient.data.l[2] = l2;
    event.xclient.data.l[3] = l3;
    event.xclient.data.l[4] = l4;

    XSendEvent(XtDisplay((Widget)xpw), event.xclient.window,
	       False, NoEventMask, &event);
}

/*- sendKeyEvent: send unused keypress event via clientmessage event -*/
static void
sendKeyEvent(client, keyevent)
ConvClient *client;
XKeyEvent *keyevent;
{
    TRACE(("sendKeyEvent()\n"));
    sendClientMessage32(client, XIMP_KEYPRESS, client->id,
			(unsigned long)keyevent->keycode,
			(unsigned long)keyevent->state, 0L);
}

/*- sendErrorEvent: send error event via clientmessage event -*/
static void
sendErrorEvent(client, error)
ConvClient *client;
int error;
{
    sendClientMessage32(client, XIMP_ERROR, client->id,
			client->event->serial, (unsigned long)error, 0L);
}

/*- sendCreateRefusal: send rejecting message to a CREATE request -*/
static void
sendCreateRefusal(xpw, window)
XimpProtocolWidget xpw;
Window window;
{
    XEvent event;

    event.xclient.type = ClientMessage;
    event.xclient.window = window;
    event.xclient.message_type = xpw->ximp.ximpProtocolAtom;
    event.xclient.format = 32;
    event.xclient.data.l[0] = XIMP_CREATE_RETURN;
    event.xclient.data.l[1] = 0L;
    event.xclient.data.l[2] = 0L;
    event.xclient.data.l[3] = 0L;
    event.xclient.data.l[4] = 0L;

    XSendEvent(XtDisplay((Widget)xpw), window, False, NoEventMask, &event);
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
    XimpProtocolWidget xpw = (XimpProtocolWidget)client->protocolwidget;

    /* check encoding and format */
    if (arg->encoding != xpw->ximp.ctextAtom ||	arg->format != 8) {
	/*
	 * since every conversion object must support COMPOUND_TEXT,
	 * it is a serious error.
	 */
	String params[2];
	Cardinal num_params;

	params[0] = XtClass((Widget)xpw)->core_class.class_name;
	params[1] = xpw->ximp.inputObjClass->core_class.class_name;
	num_params = 2;

	XtAppErrorMsg(XtWidgetToApplicationContext(client->protocolwidget),
		      "encodingError", "convertedString", "WidgetError",
		      "%s: encoding of the converted string is not COMPOUND_STRING. check inputObject %s",
		      params, &num_params);
    }

    /*
     * normaly, converted string can be sent to client either via
     * ClientMessage event or via property.
     * the strategy used here is as follows:
     *     if the string is short enough to fit in a event,
     *     use ClientMessage. else, use property.
     * however in case of reset, the string must be sent via property.
     */
#define MAX_BYTES_IN_A_EVENT	(20 - 4 - 1)

    if (!client->resetting && arg->length <= MAX_BYTES_IN_A_EVENT) {
	TRACE(("\tsending string via event\n"));
	sendClientMessage8(client, arg->text, arg->length);
    } else {
	TRACE(("\tsending string via property\n"));
	XChangeProperty(XtDisplay((Widget)xpw), XtWindow((Widget)xpw),
		    client->property, arg->encoding, arg->format,
		    PropModeAppend, (unsigned char *)arg->text, arg->length);
	/* when resetting, XIMP_READPROP event should not be sent */
	if (!client->resetting) {
	    TRACE(("\tsending XIMP_READPROP message\n"));
	    sendClientMessage32(client, XIMP_READPROP,
				client->id, client->property, 0L, 0L);
	}
    }
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
    if (client->conversion == NULL) return;

    if (!abort) {
	TRACE(("\tsending XIMP_PROCESS_END message\n"));
	sendClientMessage32(client, XIMP_PROCESS_END,
			    client->id, 0L, 0L, 0L);
    }
    detachConverter(client);
}

/*- unusedEventCallback: unused key event callback -*/
/* ARGSUSED */
static void
unusedEventCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    ConvClient *ccp = (ConvClient *)client_data;
    XKeyEvent *ev = (XKeyEvent *)call_data;

    TRACE(("unusedEventCallback(reqwin=0x%lx)\n", ccp->reqwin));
    sendKeyEvent(ccp, ev);
}

/*
 *+ ClientMessage event handler
 */

/*- ximpCreateMessageProc: XIMP_CREATE message handler -*/
static void
ximpCreateMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    Window reqwin;
    XIMStyle inputstyle;
    unsigned long attrmask;
    ConvClient *client;
    XimpInputStyle *styles = XimpStyles;
    Dimension w, h;

    TRACE(("ximpCreateMessageProc(window=0x%lx)\n", ev->data.l[1]));

    reqwin = ev->data.l[1];

    /* check validity of the client window ID */
    if (!isCorrectWindowID((Widget)xpw, reqwin, &w, &h)) {
	DPRINT(("\tspecified window doesn't exist!\n"));
	return;
    }

    inputstyle = ev->data.l[2];

    /* check specified input style */
    while (styles->ximstyle != inputstyle) {
	if (styles->ximstyle == 0L) {
	    /*
	     * client is requesting an input style which kinput2
	     * doesn't support
	     */
	    DPRINT(("\tclient wants unspported input style\n"));
	    sendCreateRefusal(xpw, reqwin);
	    return;
	}
	styles++;
    }

#ifdef DEBUG
    if (DEBUG_CONDITION) {
	printf("\tinputstyle: Preedit");
	if (styles->ximstyle & XIMPreeditArea) printf("Area");
	if (styles->ximstyle & XIMPreeditCallbacks) printf("Callbacks");
	if (styles->ximstyle & XIMPreeditPosition) printf("Position");
	if (styles->ximstyle & XIMPreeditNothing) printf("Nothing");
	if (styles->ximstyle & XIMPreeditNone) printf("None");
	printf(", Status");
	if (styles->ximstyle & XIMStatusArea) printf("Area");
	if (styles->ximstyle & XIMStatusCallbacks) printf("Callbacks");
	if (styles->ximstyle & XIMStatusNothing) printf("Nothing");
	if (styles->ximstyle & XIMStatusNone) printf("None");
	printf("\n");
    }
#endif
    client = newClient(xpw, reqwin);
    client->reqwinwidth = w;
    client->reqwinheight = h;
    client->event = ev;
    client->style = styles->cstyle;

    attrmask = ev->data.l[3];

    getVersionProperty(client);

    /* get conversion attributes */
    getAttributes(client, attrmask);

    /* watch for client destroy */
    MyAddEventHandler(XtDisplay((Widget)xpw), reqwin,
		      DestroyNotify, StructureNotifyMask,
		      ClientDead, (XtPointer)client);

    TRACE(("\tsending XIMP_CREATE_RETURN message\n"));
    sendClientMessage32(client, XIMP_CREATE_RETURN, client->id, 0L, 0L, 0L);
}

/*- ximpDestroyMessageProc: XIMP_DESTROY message handler -*/
static void
ximpDestroyMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;

    TRACE(("ximpDestroyMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;

    MyRemoveEventHandler(XtDisplay((Widget)xpw), client->reqwin, DestroyNotify,
			 ClientDead, (XtPointer)client);

    if (client->conversion != NULL) {
	CControlEndConversion(client->conversion);
	endProc(client, False);
    }
    deleteClient(client);
}

/*- ximpBeginMessageProc: XIMP_BEGIN message handler -*/
static void
ximpBeginMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;
    ConversionAttributes attrs;
    unsigned long attrmask;

    TRACE(("ximpBeginMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;
    if (client->conversion != NULL) {
	/* already converting */
	DPRINT(("\tclient already in conversion mode\n"));
	/* should we send XIMP_BadProtocol error event? */
	return;
    }

    if (attachConverter(client) == NULL) {
	/*
	 * since no appropriate error code is defined,
	 * use BadAlloc...
	 */
	sendErrorEvent(client, XIMP_BadAlloc);
	/*
	 * to let the client select key events again,
	 * send XIMP_PROCESS_END message.
	 */
	sendClientMessage32(client, XIMP_PROCESS_END,
			    client->id, 0L, 0L, 0L);
	return;
    }

    XtAddCallback(client->conversion, XtNtextCallback,
		  fixCallback, (XtPointer)client);
    XtAddCallback(client->conversion, XtNendCallback,
		  endCallback, (XtPointer)client);
    XtAddCallback(client->conversion, XtNunusedEventCallback,
		  unusedEventCallback, (XtPointer)client);

    fillInDefaultAttributes(client);
    computeAreaForStartup(client);
    attrmask = makeConvAttributes(client, &attrs);

    /* start conversion */
    XtVaSetValues(client->conversion, XtNeventSelectMethod, client->esm, NULL);
    CControlStartConversion(client->conversion, client->reqwin,
			    attrmask, &attrs);

    TRACE(("\tsending XIMP_PROCESS_BEGIN message\n"));
    sendClientMessage32(client, XIMP_PROCESS_BEGIN, client->id, 0L, 0L, 0L);
}

/*- ximpEndMessageProc: XIMP_END message handler -*/
static void
ximpEndMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;

    TRACE(("ximpEndMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;
    if (client->conversion == NULL) {
	/* not converting now */
	DPRINT(("\tclient isn't in conversion mode\n"));
	/* should we send XIMP_BadProtocol error event? */
	return;
    }

    CControlEndConversion(client->conversion);
    endProc(client, False);
}

/*- ximpSetFocusMessageProc: XIMP_SETFOCUS message handler -*/
static void
ximpSetFocusMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;

    TRACE(("ximpSetFocusMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;
    /* what to do? */
}

/*- ximpUnsetFocusMessageProc: XIMP_UNSETFOCUS message handler -*/
static void
ximpUnsetFocusMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;

    TRACE(("ximpUnsetFocusMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;
    /* what to do? */
}

/*- ximpMoveMessageProc: XIMP_MOVE message handler -*/
static void
ximpMoveMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;
    ConversionAttributes attrs;

    TRACE(("ximpMoveMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;
    client->xpattrs.spotx = ev->data.l[2];
    client->xpattrs.spoty = ev->data.l[3];
    client->xattrmask |= XIMP_PRE_SPOTL_MASK;
    if (client->conversion != NULL) {
	attrs.spotx = client->xpattrs.spotx;
	attrs.spoty = client->xpattrs.spoty;
	CControlChangeAttributes(client->conversion, CASpotLocation, &attrs);
    }
}

/*- ximpResetMessageProc: XIMP_RESET message handler -*/
static void
ximpResetMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;
    Widget inputobj;

    TRACE(("ximpResetMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;
    client->resetting = True;

    if (client->conversion == NULL) {
	XChangeProperty(XtDisplay((Widget)xpw), XtWindow((Widget)xpw),
		    client->property, xpw->ximp.ctextAtom, 8,
		    PropModeAppend, (unsigned char *)"", 0);
    } else {
	/*
	 * get input object by asking conversion widget of XtNinputObject
	 * resource. however, it is not recommended since protocol widget
	 * should interact with input object only through conversion
	 * widget.
	 */
	CCTextCallbackArg arg;

	XtVaGetValues(client->conversion, XtNinputObject, &inputobj, NULL);
	arg.encoding = xpw->ximp.ctextAtom;
	if (ICGetConvertedString(inputobj, &arg.encoding, &arg.format,
				 &arg.length, &arg.text) >= 0) {
	    fixProc(client, &arg);
	} else {
	    /* there's no string */
	    XChangeProperty(XtDisplay((Widget)xpw), XtWindow((Widget)xpw),
			    client->property, xpw->ximp.ctextAtom, 8,
			    PropModeAppend, (unsigned char *)"", 0);
	}
	ICClearConversion(inputobj);
    }

    TRACE(("\tsending XIMP_RESET_RETURN message\n"));
    sendClientMessage32(client, XIMP_RESET_RETURN, client->id,
			client->property, 0L, 0L);
}

/*- ximpSetValueMessageProc: XIMP_SETVALUE message handler -*/
static void
ximpSetValueMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    unsigned long mask;
    ConversionAttributes attrs;
    unsigned long attrmask;
    int id;

    TRACE(("ximpSetValueMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    mask = ev->data.l[2];
    client->event = ev;
    getAttributes(client, mask); 
    if (client->conversion != NULL) {
	attrmask = makeConvAttributes(client, &attrs);
	CControlChangeAttributes(client->conversion, attrmask, &attrs);
    }
}

/*- ximpChangeMessageProc: XIMP_CHANGE message handler -*/
static void
ximpChangeMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    Atom atom;
    unsigned long mask;
    ConversionAttributes attrs;
    unsigned long attrmask;
    int id;

    TRACE(("ximpChangeMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    atom = (Atom)ev->data.l[2];

    if (atom == xpw->ximp.ximpFocusAtom) {
	mask = XIMP_FOCUS_WIN_MASK;
    } else if (atom == xpw->ximp.ximpPreeditAtom) {
	mask = PREEDIT_MASK;
    } else if (atom == xpw->ximp.ximpStatusAtom) {
	mask = STATUS_MASK;
    } else if (atom == xpw->ximp.ximpPreeditFontAtom) {
	mask = XIMP_PRE_FONT_MASK;
    } else if (atom == xpw->ximp.ximpStatusFontAtom) {
	mask = XIMP_STS_FONT_MASK;
    } else {
	/* invalid property name */
	sendErrorEvent(client, XIMP_BadProperty);
	return;
    }

    client->event = ev;
    getAttributes(client, mask); 
    if (client->conversion != NULL) {
	attrmask = makeConvAttributes(client, &attrs);
	CControlChangeAttributes(client->conversion, attrmask, &attrs);
    }
}

/*- ximpGetValueMessageProc: XIMP_GETVALUE message handler -*/
static void
ximpGetValueMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    unsigned long mask;
    int id;

    TRACE(("ximpGetValueMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    mask = ev->data.l[2];
    client->event = ev;

    fillInDefaultAttributes(client);
    computeAreaForQuery(client);
    setAttributes(client, mask);

    TRACE(("\tsending XIMP_GETVALUE_RETURN message\n"));
    sendClientMessage32(client, XIMP_GETVALUE_RETURN, client->id, 0L, 0L, 0L);
}

/*- ximpKeyPressMessageProc: XIMP_KEYPRESS message handler -*/
static void
ximpKeyPressMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    int id;
    XKeyEvent keyevent;

    TRACE(("ximpKeyPressMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    client->event = ev;

    if (client->conversion == NULL) return;

    /* make a fake keypress event */
    keyevent.type = KeyPress;
    keyevent.serial = ev->serial;
    keyevent.send_event = True;
    keyevent.display = ev->display;
    keyevent.window = client->focuswin;
    keyevent.root = DefaultRootWindow(ev->display);
			/* hope conversion object won't check this field */
    keyevent.subwindow = None;
			/* hope conversion object won't check this field */
    keyevent.time = 0;	/* hope conversion object won't check this field */
    keyevent.x = 0;
    keyevent.y = 0;
    keyevent.x_root = 0;
    keyevent.y_root = 0;
    keyevent.state = ev->data.l[3];
    keyevent.keycode = ev->data.l[2];
    keyevent.same_screen = True;

    /* call action routine */
    XtCallActionProc(client->conversion, "to-inputobj", (XEvent *)&keyevent,
		     (String *)NULL, (Cardinal)0);
}

/*- ximpExtensionMessageProc: XIMP_Extension message handler -*/
static void
ximpExtensionMessageProc(xpw, ev)
XimpProtocolWidget xpw;
XClientMessageEvent *ev;
{
    ConvClient *client;
    Atom extatom;
    int id;

    TRACE(("ximpExtensionMessageProc()\n"));

    id = ev->data.l[1];
    if ((client = findClient(xpw, id)) == NULL) {
	DPRINT(("\tinvalid ID\n"));
	return;
    }
    extatom = ev->data.l[2];	/* extension name */
    client->event = ev;

    if (extatom == xpw->ximp.ximpExtXimpBackFrontAtom) {
	TRACE(("\t_XIMP_EXT_XIMP_BACK_FRONT extension -- "));
	if (client->conversion != NULL) {
	    /* invalid */
	    DPRINT(("invalid (now in conversion mode)\n"));
	    return;
	}
	if (ev->data.l[3] != 0) {
	    /* backend method */
	    TRACE(("backend\n"));
	    client->esm = ESMethodNone;
	} else {
	    TRACE(("frontend\n"));
	    client->esm = ESMethodSelectFocus;
	}
    } else {
	DPRINT(("\tunknown extension atom -- %ld", extatom));
	sendErrorEvent(client, XIMP_BadAtom);
    }
}

/*- XimpMessageProc: _XIMP_PROTOCOL message event handler -*/
/* ARGSUSED */
static void
XimpMessageProc(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    XimpProtocolWidget xpw = (XimpProtocolWidget)w;
    XClientMessageEvent *ev = &event->xclient;
    ConvClient *client;

    TRACE(("XimpMessageProc()\n"));
    /* is it a correct event? */
    if (!isCorrectClientEvent(xpw, event)) {
	/*ignore */
	DPRINT(("got invalid clientmessage event.\n"));
	return;
    }
    switch((int)ev->data.l[0]) {
    case XIMP_CREATE:	  ximpCreateMessageProc(xpw, ev); break;
    case XIMP_DESTROY:	  ximpDestroyMessageProc(xpw, ev); break;
    case XIMP_BEGIN:	  ximpBeginMessageProc(xpw, ev); break;
    case XIMP_END:	  ximpEndMessageProc(xpw, ev); break;
    case XIMP_SETFOCUS:	  ximpSetFocusMessageProc(xpw, ev); break;
    case XIMP_UNSETFOCUS: ximpUnsetFocusMessageProc(xpw, ev); break;
    case XIMP_KEYPRESS:	  ximpKeyPressMessageProc(xpw, ev); break;
    case XIMP_SETVALUE:	  ximpSetValueMessageProc(xpw, ev); break;
    case XIMP_CHANGE:	  ximpChangeMessageProc(xpw, ev); break;
    case XIMP_GETVALUE:	  ximpGetValueMessageProc(xpw, ev); break;
    case XIMP_MOVE:	  ximpMoveMessageProc(xpw, ev); break;
    case XIMP_RESET:	  ximpResetMessageProc(xpw, ev); break;
    case XIMP_EXTENSION:  ximpExtensionMessageProc(xpw, ev); break;
    default:
	DPRINT(("\tunknown command code (%ld) ignored\n", ev->data.l[0]));
	/* assume ev->data.l[1] contains ICID */
	if ((client = findClient(xpw, (int)ev->data.l[1])) != NULL) {
	    client->event = ev;
	    sendErrorEvent(client, XIMP_BadProtocol);
	}
	break;
    }
}

/*
 *+ other event handler
 */

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
    XimpProtocolWidget xpw = (XimpProtocolWidget)w;
    XSelectionClearEvent *ev = (XSelectionClearEvent *)event;
    ConvClient	*ccp;
    String params[1];
    Cardinal num_params;

    /* Selection owner changed */

    if (ev->selection == xpw->ximp.selAtom1) {
	/* someone has become a new default server */
	xpw->ximp.selAtom1 = None;
	return;
    } else if (ev->selection != xpw->ximp.selAtom2) {
	TRACE(("XimpProtocol:SelectionClearProc() SelectionClear event for unknown selection received\n"));
	return;
    }

    /*
     * send ConversionEnd event to the clients before exit
     */
    for (ccp = xpw->ximp.clients; ccp; ccp = ccp->next) {
	if (ccp->reqwin != None) endProc(ccp, False);
    }

    params[0] = XtClass(w)->core_class.class_name;
    num_params = 1;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "selectionError", "SelectionClear", "WidgetError",
		    "%s: SelectionClear event received",
		    params, &num_params);

    XtDestroyWidget(w);
}

/*- ClientDead: DestroyNotify event handler -*/
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

#endif
