#ifndef lint
static char *rcsid = "$Id: IMProto.c,v 1.9 1994/06/03 05:55:03 ishisone Rel $";
#endif
/*- 
 * Copyright (c) 1991, 1994  Software Research Associates, Inc.
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

/*
 * X Input Method Protocol handler is considered to be still in
 * beta testing phase.  Current version has the following
 * restrictions.
 *	- it does not support on-demand-synchronous method.
 *	- it does not support front-end model.
 *	- it does not SetIMValues operation.
 *	- it supports only X, local and TCP transports.
 * Also, there might be various bugs.
 */

#define DEBUG_VAR debug_IMProtocol

#include <ctype.h>
#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xmu/SysUtil.h>
#include "IMProtoP.h"
#include "ParseKey.h"
#include "im.h"


#define SERVER_NAME		"kinput2"
#define UNIX_SOCKET_PATH	"/tmp/.ki2-%d-%d"

/*- resource table -*/
static XtResource resources[] = {
#define offset(field) XtOffset(IMProtocolWidget, imp.field)
    { XtNserverName, XtCServerName, XtRString, sizeof(String),
	offset(server_name), XtRString, (XtPointer)SERVER_NAME },
    { XtNlanguage, XtCLanguage, XtRString, sizeof(String),
	offset(language), XtRImmediate, (XtPointer)NULL },
    { XtNlocales, XtCLocales, XtRString, sizeof(String),
	offset(locales), XtRImmediate, (XtPointer)NULL },
    { XtNinputObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(input_object_class), XtRImmediate, (XtPointer)NULL },
    { XtNdisplayObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(display_object_class), XtRImmediate, (XtPointer)NULL },
    { XtNdefaultFontList, XtCFontList, XtRString, sizeof(String),
	offset(default_fontlist), XtRImmediate, (XtPointer)NULL },
    { XtNconversionStartKeys, XtCConversionStartKeys, XtRString, sizeof(String),
	offset(conversion_start_keys), XtRImmediate, (XtPointer)NULL },
    { XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	offset(foreground), XtRString, XtDefaultForeground },
    { XtNstatusWidth, XtCStatusWidth, XtRDimension, sizeof(Dimension),
	offset(status_width), XtRImmediate, (XtPointer)0 },
    { XtNtransports, XtCTransports, XtRString, sizeof(String),
	offset(transport_list), XtRString, (XtPointer)"tcp,unix,x" },
#undef offset
};

static void Initialize _Pt_((Widget req, Widget new,
			     ArgList args, Cardinal *num_args));
static void Destroy _Pt_((Widget w));
static void Realize _Pt_((Widget w, XtValueMask *mask,
			  XSetWindowAttributes *value));

/*- IMProtocolClassRec -*/
IMProtocolClassRec imProtocolClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"IMProtocol",
    /* widget_size		*/	sizeof(IMProtocolRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
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
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* imProtocol fields */
    /* dummy			*/	0
  }
};

WidgetClass imProtocolWidgetClass = (WidgetClass)&imProtocolClassRec;

static void getAtoms _Pt_((IMProtocolWidget ipw));
static void setProperty _Pt_((IMProtocolWidget ipw));
static int ownSelection _Pt_((IMProtocolWidget ipw));
static Boolean convertSelection _Pt_((Widget w, Atom *selectionp,
				      Atom *targetp, Atom *typep,
				      XtPointer *valuep,
				      unsigned long *lengthp, int *formatp));
static void loseSelection _Pt_((Widget w, Atom *selectionp));
#ifdef IM_TCP_TRANSPORT
static void acceptTCPService _Pt_((XtPointer client_data,
				  int *sourcep, XtInputId *idp));
#endif
#ifdef IM_UNIX_TRANSPORT
static void acceptUnixService _Pt_((XtPointer client_data,
				    int *sourcep, XtInputId *idp));
#endif
#ifdef IM_X_TRANSPORT
static void acceptXService _Pt_((Widget w, XtPointer client_data,
				 XEvent *event, Boolean *continuep));
#endif
static void initializeError _Pt_((Widget w, String resname));
static void setTransport _Pt_((Widget w));
static int makeConverter _Pt_((Widget w));
static void getTriggerKeys _Pt_((Widget w));


/*
 *+ Core class methods
 */

/*- Initialize: intern Atoms, get default fonts, etc. -*/
/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    IMProtocolWidget ipw = (IMProtocolWidget)new;

    TRACE(("IMProtocolWidget:Initialize()\n"));

    /*
     * Check resources which must be specified at the initialization.
     */
#define NULLorEMPTY(p)	((p) == NULL || (p)[0] == '\0')
    if (NULLorEMPTY(ipw->imp.server_name)) {
	initializeError(new, XtNserverName);
    }

    ipw->imp.server_name = XtNewString(ipw->imp.server_name);

    if (NULLorEMPTY(ipw->imp.language)) {
	initializeError(new, XtNlanguage);
    } else if (NULLorEMPTY(ipw->imp.locales)) {
	initializeError(new, XtNlocales);
    } else if (ipw->imp.input_object_class == NULL) {
	initializeError(new, XtNinputObjectClass);
    } else if (ipw->imp.display_object_class == NULL) {
	initializeError(new, XtNdisplayObjectClass);
    }
    ipw->imp.locales = XtNewString(ipw->imp.locales);
#undef NULLorEMPTY

    /*
     * Initialize converter info.
     */
    if (makeConverter(new) < 0) {
	/*
	 * locales is empty.
	 */
	String params[1];
	Cardinal num_params;

	params[0] = XtClass(new)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(new),
		      "initializeError", "invalidValue", "WidgetError",
		      "%s: locale list is empty",
		      params, &num_params);
    }

    /*
     * Create font bank (a bank of cached fonts) and enter
     * default fonts.
     */
    ipw->imp.font_bank = FontBankCreate(XtDisplay(new), ipw->imp.language);
    if (ipw->imp.font_bank == NULL) {
	/*
	 * The specified language is not supported.
	 */
	String params[2];
	Cardinal num_params;

	params[0] = XtClass(new)->core_class.class_name;
	params[1] = ipw->imp.language;
	num_params = 2;
	XtAppErrorMsg(XtWidgetToApplicationContext(new),
		      "initializeError", "invalidValue", "WidgetError",
		      "%s: language %s not supported",
		      params, &num_params);
    }

    if (ipw->imp.default_fontlist != NULL) {
	ipw->imp.default_fontlist = XtNewString(ipw->imp.default_fontlist);

	DDPRINT(2, ("cache default fonts: %s\n", ipw->imp.default_fontlist));
	ipw->imp.default_fonts = FontBankGet(ipw->imp.font_bank,
					     ipw->imp.default_fontlist,
					     &ipw->imp.num_default_fonts);
    } else {
	ipw->imp.default_fonts = NULL;
	ipw->imp.num_default_fonts = 0;
    }

    /*
     * Initialize private data.
     */
    ipw->imp.connection_list = NULL;
    ipw->imp.no_more_connections = False;
    ipw->imp.scheduler_queue = NULL;
    setTransport(new);
    getTriggerKeys(new);
    IMInitHash(new);
    getAtoms(ipw);

    /*
     * Initialilze transport layer.
     */
    /* 1. TCP/IP */
    ipw->imp.tcp_sock = -1;
#ifdef IM_TCP_TRANSPORT
    if (ipw->imp.use_tcp_transport) {
	ipw->imp.tcp_port = 0;	/* let the system choose the port number */
	ipw->imp.tcp_sock = IMCreateTCPService(&ipw->imp.tcp_port);
    }
    if (ipw->imp.tcp_sock >= 0) {
	TRACE(("call XtAppAddInput for tcp socket(%d)\n", ipw->imp.tcp_sock));
	ipw->imp.tcp_id = XtAppAddInput(XtWidgetToApplicationContext(new),
					ipw->imp.tcp_sock,
					(XtPointer)XtInputReadMask,
					acceptTCPService, (XtPointer)ipw);
    }
#endif /* IM_TCP_TRANSPORT */

    /* 2. UNIX domain */
    ipw->imp.unix_sock = -1;
#ifdef IM_UNIX_TRANSPORT
    if (ipw->imp.use_unix_transport) {
	char path[1024];
	(void)sprintf(path, UNIX_SOCKET_PATH, (int)getuid(), (int)getpid());
	ipw->imp.unix_path = XtNewString(path);
	ipw->imp.unix_sock = IMCreateUnixService(ipw->imp.unix_path);
    }
    if (ipw->imp.unix_sock >= 0) {
	TRACE(("call XtAppAddInput for unix socket(%d)\n", ipw->imp.unix_sock));
	ipw->imp.unix_id = XtAppAddInput(XtWidgetToApplicationContext(new),
					 ipw->imp.unix_sock,
					 (XtPointer)XtInputReadMask,
					 acceptUnixService, (XtPointer)ipw);
    }
#endif /* IM_UNIX_TRANSPORT */

#ifdef IM_X_TRANSPORT
    if (ipw->imp.use_x_transport) {
	TRACE(("call XtAddEventHandler for X transport\n"));
	XtAddEventHandler(new, NoEventMask, True, acceptXService,
			  (XtPointer)NULL);
    }
#endif /* IM_X_TRANSPORT */

    /*
     * Compile request dispatching table.
     */
    IMCompileReq();
}

/*- Destroy: free allocated memory -*/
static void
Destroy(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;
    IMConnection *conn;
    int i;

    TRACE(("IMProtocolWidget:Destroy()\n"));

    XtFree(ipw->imp.server_name);
    XtFree(ipw->imp.locales);
    if (ipw->imp.default_fontlist != NULL) XtFree(ipw->imp.default_fontlist);
    if (ipw->imp.trigger_keys != NULL) XtFree((char *)ipw->imp.trigger_keys);

    for (i = 0; i < ipw->imp.converter.num_locales; i++) {
	XtFree(ipw->imp.converter.supported_locales[i]);
    }
    XtFree((char *)ipw->imp.converter.supported_locales);

    /*
     * Close down all connections.
     */
    conn = ipw->imp.connection_list;
    while (conn != NULL) {
	IMConnection *next = conn->next;

	IMCloseConnection(conn);
	conn = next;
    }

    /*
     * Close down TCP/Unix service sockets.
     */
    if (ipw->imp.tcp_sock >= 0) {
	XtRemoveInput(ipw->imp.tcp_id);
	(void)close(ipw->imp.tcp_sock);
    }
    if (ipw->imp.unix_sock >= 0) {
	(void)unlink(ipw->imp.unix_path);
	XtRemoveInput(ipw->imp.unix_id);
	(void)close(ipw->imp.unix_sock);
    }

    /*
     * Unload default fonts.
     */
    if (ipw->imp.num_default_fonts > 0) {
	FontBankFreeFonts(ipw->imp.font_bank,
			  ipw->imp.default_fonts,
			  ipw->imp.num_default_fonts);
    }

    /*
     * Free font bank.
     */
    FontBankDestroy(ipw->imp.font_bank);
}

/*- Realize: own selection -*/
static void
Realize(w, mask, value)
Widget w;
XtValueMask *mask;
XSetWindowAttributes *value;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;
    CoreWidgetClass super = (CoreWidgetClass)XtClass(w)->core_class.superclass;

    TRACE(("IMProtocolWidget:Realize()\n"));

    (*super->core_class.realize)(w, mask, value);

    setProperty(ipw);

    if (ownSelection(ipw) < 0) {
	String params[1];
	Cardinal num_params;

	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext(w),
			"selectionError", "ownSelection", "WidgetError",
			"%s: can't own selection", params, &num_params);

	XtDestroyWidget(w);
    }
}

/*
 *+ Atom, property and selection handling
 */

/*- getAtoms: intern atoms -*/
static void
getAtoms(ipw)
IMProtocolWidget ipw;
{
    Display *dpy = XtDisplay((Widget)ipw);
    char buf[256];

    TRACE(("IMProtocolWidget:getAtoms()\n"));

    (void)strcpy(buf, "@server=");
    (void)strcat(buf, ipw->imp.server_name);
#define MAKEATOM(s)	XInternAtom(dpy, s, False)
    ipw->imp.server_atom = MAKEATOM(buf);
    ipw->imp.ctext_atom = MAKEATOM("COMPOUND_TEXT");
    ipw->imp.locales_atom = MAKEATOM("LOCALES");
    ipw->imp.transport_atom = MAKEATOM("TRANSPORT");
    ipw->imp.ki2comm_atom = MAKEATOM("_KINPUT2_COMM");
    ipw->imp.xim_xconnect = MAKEATOM("_XIM_XCONNECT");
    ipw->imp.xim_protocol = MAKEATOM("_XIM_PROTOCOL");
    ipw->imp.xim_moredata = MAKEATOM("_XIM_MOREDATA");
#undef MAKEATOM
}

/*- setProperty: set XIM_SERVERS property -*/
static void
setProperty(ipw)
IMProtocolWidget ipw;
{
    Display *dpy = XtDisplay((Widget)ipw);
    Atom xim_servers = XInternAtom(dpy, "XIM_SERVERS", False);
    Atom server_atom = ipw->imp.server_atom;
    Window root0 = RootWindow(dpy, 0);
    int op_mode = PropModePrepend;
    int no_registration = 0;
    Atom type;
    int format;
    unsigned long nitems;
    unsigned long bytes_after;
    unsigned char *value;
    unsigned long data;

    TRACE(("IMProtocolWidget:setProperty()\n"));

    /*
     * For atomic operation, grab the server.
     */
#ifndef DEBUG
    XGrabServer(dpy);
#endif

    /*
     * First, check the XIM_SERVERS property's existance.
     * If it exists, examine the contents.
     */
    if (XGetWindowProperty(dpy, root0, xim_servers, 0L, 1024L, False,
			   AnyPropertyType, &type, &format, &nitems,
			   &bytes_after, &value) == Success) {
	if (type != XA_ATOM || format != 32) {
	    /*
	     * The contents of the property is invalid.
	     */
	    DDPRINT(2, ("XIM_SERVERS is corrupted (type=%ld, format=%d)\n",
		       type, format));
	    op_mode = PropModeReplace;
	} else {
	    int i;
	    unsigned long *atoms = (unsigned long *)value;

	    for (i = 0; i < nitems; i++) {
		if (atoms[i] == server_atom) {
		    /*
		     * Already registered.
		     */
		    TRACE(("server is already registered in XIM_SERVERS\n"));
		    no_registration = 1;
		    break;
		}
	    }
	}
	XFree((char *)value);
    }

    if (!no_registration) {
	TRACE(("changing XIM_SERVERS property\n"));
	data = ipw->imp.server_atom;
	XChangeProperty(dpy, root0, xim_servers, XA_ATOM, 32, op_mode,
			(unsigned char *)&data, 1);
    }

#ifndef DEBUG
    XUngrabServer(dpy);
#endif
}

/*- ownSelection: own conversion selection -*/
static int
ownSelection(ipw)
IMProtocolWidget ipw;
{
    Display *dpy = XtDisplay((Widget)ipw);
    Time time = XtLastTimestampProcessed(dpy);


    TRACE(("IMProtocolWidget:ownSelection()\n"));

    if (!XtOwnSelection((Widget)ipw, ipw->imp.server_atom, time,
			convertSelection, loseSelection,
			(XtSelectionDoneProc)NULL)) {
	DPRINT(("cannot own selection"));
	return -1;
    }
    DPRINT(("selection atom:%ld owner: %08lx (%ld)\n", ipw->imp.server_atom,
	    XtWindow((Widget)ipw), XtWindow((Widget)ipw)));
    return 0;
}

/*- convertSelection: convert selections -*/
/* ARGSUSED */
static Boolean
convertSelection(w, selectionp, targetp, typep, valuep, lengthp, formatp)
Widget w;
Atom *selectionp;
Atom *targetp;
Atom *typep;
XtPointer *valuep;
unsigned long *lengthp;
int *formatp;
{
    Display *dpy = XtDisplay(w);
    IMProtocolWidget ipw = (IMProtocolWidget)w;

    TRACE(("IMProtocolWidget:convertSelection()\n"));

    if (*targetp == XInternAtom(dpy, "TARGETS", False)) {
	Atom *targets;

	TRACE(("target is \"TARGETS\"\n"));
	targets = (Atom *)XtMalloc(sizeof(Atom) * 2);
	targets[0] = ATOM_LOCALES(w);
	targets[1] = ATOM_TRANSPORT(w);

	*typep = XA_ATOM;
	*valuep = (XtPointer)targets;
	*lengthp = 2;
	*formatp = 32;
	return True;
    } else if (*targetp == ATOM_LOCALES(w)) {
	char buf[1024];

	TRACE(("target is \"LOCALES\"\n"));
	(void)strcpy(buf, "@locale=");
	(void)strcat(buf, ipw->imp.locales);
	TRACE(("\ttype: STRING, value: %s\n", buf));
	/*
	 * The protocol spec is unclear on the type of the
	 * selection value.  Since R6 sample implementation
	 * uses LOCALES, use it.
	 */
	*typep = *targetp;
	/* *typep = XA_STRING; */
	*valuep = (XtPointer)XtNewString(buf);
	*lengthp = strlen(buf);
	*formatp = 8;
	return True;
    } else if (*targetp == ATOM_TRANSPORT(w)) {
	char buf[1024];
	char hostname[256];

	TRACE(("target is \"TRANSPORT\"\n"));

	XmuGetHostname(hostname, 256);

	(void)strcpy(buf, "@transport=");

#ifdef IM_X_TRANSPORT
	if (ipw->imp.use_x_transport) {
	    (void)strcat(buf, "X/,");
	}
#endif /* IM_X_TRANSPORT */

#ifdef IM_TCP_TRANSPORT
	if (ipw->imp.use_tcp_transport) {
	    char t_buf[1024];
	    (void)sprintf(t_buf, "tcp/%s:%d,", hostname, ipw->imp.tcp_port);
	    (void)strcat(buf, t_buf);
	}
#endif /* IM_TCP_TRANSPORT */

#ifdef IM_UNIX_TRANSPORT
	if (ipw->imp.use_unix_transport) {
	    char u_buf[1024];
	    (void)sprintf(u_buf, "local/%s:%s,", hostname, ipw->imp.unix_path);
	    (void)strcat(buf, u_buf);
	}
#endif /* IM_UNIX_TRANSPORT */

	/* delete trailing comma */
	if (buf[strlen(buf) - 1] == ',') buf[strlen(buf) - 1] = '\0';
	TRACE(("\ttype: STRING, value: %s\n", buf));

	*typep = *targetp;	/* -- see the comment on LOCALES above */
	*valuep = (XtPointer)XtNewString(buf);
	*lengthp = strlen(buf);
	*formatp = 8;
	return True;
    } else {
	DDPRINT(2, ("unknown target atom (%ld)\n", *targetp));
	return False;
    }
}

/*- loseSelection: disable IM protocol handling -*/
/* ARGSUSED */
static void
loseSelection(w, selectionp)
Widget w;
Atom *selectionp;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;

    TRACE(("IMProtocolWidget:loseSelection()\n"));

    /*
     * Someone takes over the selection.  That means
     * another kinput2 process has been started.
     * Let the newly process handle new clients, but
     * as long as existing clients are remained, we have
     * to maintain them.
     */

    if (ipw->imp.connection_list == NULL) {
	/*
	 * There are no clients.  It is OK to destroy protocol handler.
	 */
	XtDestroyWidget(w);
	return;
    }

    ipw->imp.no_more_connections = True;

    /*
     * Close down TCP/Unix service sockets.
     */
    if (ipw->imp.tcp_sock >= 0) {
	TRACE(("\tclose tcp socket\n"));
	XtRemoveInput(ipw->imp.tcp_id);
	(void)close(ipw->imp.tcp_sock);
	ipw->imp.tcp_sock = -1;
    }
    if (ipw->imp.unix_sock >= 0) {
	TRACE(("\tclose unix socket\n"));
	XtRemoveInput(ipw->imp.unix_id);
	(void)close(ipw->imp.unix_sock);
	ipw->imp.unix_sock = -1;
    }
}


/*
 *+ Connection acceptance
 */

#ifdef IM_TCP_TRANSPORT
/*- acceptTCPService: establish connection via TCP transport -*/
/* ARGSUSED */
static void
acceptTCPService(client_data, sourcep, idp)
XtPointer client_data;
int *sourcep;
XtInputId *idp;
{
    IMProtocolWidget ipw = (IMProtocolWidget)client_data;
    IMConnection *conn;

    TRACE(("IMProtocolWidget:acceptTCPService()\n"));

    /*
     * Accept connection request.
     */
    conn = IMTCPConnection((Widget)ipw, *sourcep);

    /*
     * Set dispatcher.
     */
    if (conn != NULL) IMSetInitialDispatcher(conn);

    /*
     * Enter to the connections list.
     */
    if (conn != NULL) IMRegisterConnection(conn);
}
#endif /* IM_TCP_TRANSPORT */

#ifdef IM_UNIX_TRANSPORT
/*- acceptUnixService: establish connection via UNIX domain transport -*/
/* ARGSUSED */
static void
acceptUnixService(client_data, sourcep, idp)
XtPointer client_data;
int *sourcep;
XtInputId *idp;
{
    IMProtocolWidget ipw = (IMProtocolWidget)client_data;
    IMConnection *conn;

    TRACE(("IMProtocolWidget:acceptUnixService()\n"));

    /*
     * Accept connection request.
     */
    conn = IMUnixConnection((Widget)ipw, *sourcep);

    /*
     * Set dispatcher.
     */
    if (conn != NULL) IMSetInitialDispatcher(conn);

    /*
     * Enter to the connections list.
     */
    if (conn != NULL) IMRegisterConnection(conn);
}
#endif /* IM_UNIX_TRANSPORT */

#ifdef IM_X_TRANSPORT
/*- acceptXService: establish connection via X transport -*/
/* ARGSUSED */
static void
acceptXService(w, client_data, event, continuep)
Widget w;
XtPointer client_data;
XEvent *event;
Boolean *continuep;
{
    IMConnection *conn;

    TRACE(("IMProtocolWidget:acceptXService()\n"));

    /*
     * Check if the event is really a connection request.
     */
    if (event->type != ClientMessage) return;
    conn = IMXConnection(w, event);

    /*
     * Set dispatcher.
     */
    if (conn != NULL) IMSetInitialDispatcher(conn);

    /*
     * Enter to the connections list.
     */
    if (conn != NULL) IMRegisterConnection(conn);
}
#endif /* IM_X_TRANSPORT */


/*
 *+ utility functions
 */

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

/*- setTransport: determine which transport to be used -*/
static void
setTransport(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;
    char *p;

    TRACE(("IMProtocolWidget:setTransport(%s)\n", ipw->imp.transport_list));

    ipw->imp.use_tcp_transport = False;
    ipw->imp.use_unix_transport = False;
    ipw->imp.use_x_transport = False;

    p = ipw->imp.transport_list;
    while (*p != '\0') {
	char lower[256];
	char *q;

	while (isspace(*p)) p++;
	if (*p == '\0') break;

	q = lower;
	while (*p != '\0' && *p != ',' && !isspace(*p)) {
	    if (isupper(*p)) {
		*q++ = tolower(*p);
	    } else {
		*q++ = *p;
	    }
	    p++;
	}
	*q = '\0';
	while (isspace(*p)) p++;
	if (*p == ',') p++;

	if (!strcmp(lower, "tcp")) {
	    TRACE(("\tTCP transport\n"));
	    ipw->imp.use_tcp_transport = True;
	} else if (!strcmp(lower, "unix")) {
	    TRACE(("\tUNIX domain transport\n"));
	    ipw->imp.use_unix_transport = True;
	} else if (!strcmp(lower, "x")) {
	    TRACE(("\tX transport\n"));
	    ipw->imp.use_x_transport = True;
	}
    }
}

/*- makeConverter: create converter record -*/
static int
makeConverter(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;
    char *locales[100];
    int num_locales;
    int size;
    char *p;

    TRACE(("IMProtocolWidget:makeConverter()\n"));

    ipw->imp.converter.input_object_class = ipw->imp.input_object_class;
    ipw->imp.converter.display_object_class = ipw->imp.display_object_class;

    p = ipw->imp.locales;
    num_locales = 0;
    do {
	char buf[256];
	char *q = buf;

	while (isspace(*p)) p++;
	if (*p == '\0') break;

	while (*p != '\0' && *p != ',' && !isspace(*p)) *q++ = *p++;
	*q = '\0';
	TRACE(("\tsupported locale: %s\n", buf));
	locales[num_locales++] = XtNewString(buf);
	while (isspace(*p)) p++;
	if (*p == ',') p++;
    } while (*p != '\0' && num_locales < 100);
    TRACE(("\tnumber of supported locales: %d\n", num_locales));

    if (num_locales == 0) return -1;
    ipw->imp.converter.num_locales = num_locales;

    size = sizeof(char *) * num_locales;
    ipw->imp.converter.supported_locales = (char **)XtMalloc(size);
    bcopy((char *)locales, (char *)ipw->imp.converter.supported_locales, size);
    return 0;
}

/*- getTriggerKeys: parse conversion trigger key specification -*/
static void
getTriggerKeys(w)
Widget w;
{
    IMProtocolWidget ipw = (IMProtocolWidget)w;
    char *key_str;
    IMTriggerKey keys[100];
    int num_keys;
    int c;

    TRACE(("IMProtocolWidget:getTriggerKeys()\n"));

    key_str = ipw->imp.conversion_start_keys;
    num_keys = 0;
    TRACE(("\tstart keys: %s\n", key_str));

    if (key_str != NULL) {
	do {
	    char buf[256];
	    char *p = buf;
	    KeySym keysym;
	    long mods, chk_mods;

	    while ((c = *key_str++) != '\0' && c != '\n') {
		*p++ = c;
	    }
	    *p = '\0';
	    if (ParseKeyEvent(buf, &keysym, &mods, &chk_mods)) {
		TRACE(("\tkeysym: %08lx, modifiers: %04lx, check: %04lx\n",
		       keysym, mods, chk_mods));
		keys[num_keys].keysym = keysym;
		keys[num_keys].modifiers = mods;
		keys[num_keys].check_modifiers = chk_mods;
		num_keys++;
	    }
	} while  (c != '\0' && num_keys < 100);
    }
    TRACE(("\tnumber of trigger keys: %d\n", num_keys));
    ipw->imp.num_trigger_keys = num_keys;

    if (num_keys > 0) {
	int size;

	size = sizeof(IMTriggerKey) * num_keys;
	ipw->imp.trigger_keys = (IMTriggerKey *)XtMalloc(size);
	bcopy((char *)keys, (char *)ipw->imp.trigger_keys, size);
    } else {
	ipw->imp.trigger_keys = NULL;
    }
}
