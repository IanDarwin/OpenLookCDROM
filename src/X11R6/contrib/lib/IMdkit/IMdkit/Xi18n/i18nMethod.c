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
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#ifndef NEED_EVENTS
#define NEED_EVENTS
#endif
#include <X11/Xproto.h>
#undef NEED_EVENTS
#include "FrameMgr.h"
#include "IMdkit.h"
#include "Xi18n.h"

#if NeedFunctionPrototypes
extern Xi18nClient *_Xi18nFindClient(Xi18n, CARD16);
static void *xi18n_setup(Display*, XIMArg*);
static Status xi18n_openIM(XIMS);
static Status xi18n_closeIM(XIMS);
static char *xi18n_setIMValues(XIMS, XIMArg*);
static char *xi18n_getIMValues(XIMS, XIMArg*);
static Status xi18n_forwardEvent(XIMS, IMForwardEventStruct*);
static Status xi18n_commit(XIMS, IMCommitStruct*);
static Status xi18n_callCallback(XIMS, IMProtocol*);
static Status xi18n_preeditStart(XIMS, IMProtocol*);
static Status xi18n_preeditEnd(XIMS, IMProtocol*);
#else
extern Xi18nClient *_Xi18nFindClient();
static void *xi18n_setup();
static Status xi18n_openIM();
static Status xi18n_closeIM();
static char *xi18n_setIMValues();
static char *xi18n_getIMValues();
static Status xi18n_forwardEvent();
static Status xi18n_commit();
static Status xi18n_callCallback();
static Status xi18n_preeditStart();
static Status xi18n_preeditEnd();
#endif

IMMethodsRec Xi18n_im_methods = {
    xi18n_setup,
    xi18n_openIM,
    xi18n_closeIM,
    xi18n_setIMValues,
    xi18n_getIMValues,
    xi18n_forwardEvent,
    xi18n_commit,
    xi18n_callCallback,
    xi18n_preeditStart,
    xi18n_preeditEnd,
};

extern Bool _Xi18nCheckXAddress(
#if NeedFunctionPrototypes
     Xi18n, TransportSW *, char *
#endif
				);
extern Bool _Xi18nCheckTransAddress(
#if NeedFunctionPrototypes
     Xi18n, TransportSW *, char *
#endif
				    );

TransportSW _TransR[] = {
    {"X",          1, _Xi18nCheckXAddress},
    {"tcp",        3, _Xi18nCheckTransAddress},
    {"local",      5, _Xi18nCheckTransAddress},
#ifdef DNETCONN
    {"decnet",     6, _Xi18nCheckTransAddress},
#endif
    {(char *)NULL, 0, (Bool (*)())NULL}
};

static Bool
#if NeedFunctionPrototypes
GetInputStyles(Xi18n i18n_core, XIMStyles **p_style)
#else
GetInputStyles(i18n_core, p_style)
Xi18n i18n_core;
XIMStyles **p_style;
#endif
{
    Xi18nAddressRec	*address = (Xi18nAddressRec *)&i18n_core->address;
    XIMStyles		*p;
    int			i;

    p = &address->input_styles;
    if ((*p_style = (XIMStyles *)malloc(sizeof(XIMStyles)
		+ p->count_styles * sizeof(XIMStyle))) == NULL)
      return False;
    (*p_style)->count_styles = p->count_styles;
    (*p_style)->supported_styles = (XIMStyle *)((XPointer)*p_style + sizeof(XIMStyles));
    for (i = 0; i < (int)p->count_styles; i++) {
	(*p_style)->supported_styles[i] = p->supported_styles[i];
    }
    return True;
}

static Bool
#if NeedFunctionPrototypes
GetOnOffKeys(Xi18n i18n_core, long mask, XIMTriggerKeys **p_key)
#else
GetOnOffKeys(i18n_core, mask, p_key)
Xi18n i18n_core;
long mask;
XIMTriggerKeys **p_key;
#endif
{
    Xi18nAddressRec	*address = (Xi18nAddressRec *)&i18n_core->address;
    XIMTriggerKeys	*p;
    int			i;

    if (mask & I18N_ON_KEYS)
      p = &address->on_keys;
    else
      p = &address->off_keys;

    if ((*p_key = (XIMTriggerKeys *)malloc(sizeof(XIMTriggerKeys)
		+ p->count_keys * sizeof(XIMTriggerKey))) == NULL)
      return False;
    (*p_key)->count_keys = p->count_keys;
    (*p_key)->keylist = (XIMTriggerKey *)((XPointer)*p_key + sizeof(XIMTriggerKey));
    for (i = 0; i < (int)p->count_keys; i++) {
	(*p_key)->keylist[i].keysym = p->keylist[i].keysym;
	(*p_key)->keylist[i].modifier = p->keylist[i].modifier;
	(*p_key)->keylist[i].modifier_mask = p->keylist[i].modifier_mask;
    }
    return True;
}

static Bool
#if NeedFunctionPrototypes
GetEncodings(Xi18n i18n_core, XIMEncodings **p_encoding)
#else
GetEncodings(i18n_core, p_encoding)
Xi18n i18n_core;
XIMEncodings **p_encoding;
#endif
{
    Xi18nAddressRec	*address = (Xi18nAddressRec *)&i18n_core->address;
    XIMEncodings	*p;
    int			i;

    p = &address->encoding_list;

    if ((*p_encoding = (XIMEncodings *)malloc(sizeof(XIMEncodings)
		+ p->count_encodings * sizeof(XIMEncoding))) == NULL)
      return False;
    (*p_encoding)->count_encodings = p->count_encodings;
    (*p_encoding)->supported_encodings = (XIMEncoding *)((XPointer)*p_encoding + sizeof(XIMEncodings));
    for (i = 0; i < (int)p->count_encodings; i++) {
	(*p_encoding)->supported_encodings[i]
	  = (char *)malloc(strlen(p->supported_encodings[i]) + 1);
	strcpy((*p_encoding)->supported_encodings[i],
	       p->supported_encodings[i]);
    }
    return True;
}

static char *
#if NeedFunctionPrototypes
ParseArgs(Xi18n i18n_core, int mode, XIMArg *args)
#else
ParseArgs(i18n_core, mode, args)
Xi18n	i18n_core;
int	mode;
XIMArg	*args;
#endif
{
    Xi18nAddressRec *address = (Xi18nAddressRec *)&i18n_core->address;
    XIMArg *p;
    
    if (mode == I18N_OPEN || mode == I18N_SET) {
	for (p = args; p->name != NULL; p++) {
	    if (!strcmp(p->name, IMLocale)) {
		if (address->imvalue_mask & I18N_IM_LOCALE) {
		    return IMLocale;
		}
		address->im_locale = (char *)malloc(strlen(p->value) + 1);
		if (!address->im_locale)
		  return IMLocale;
		strcpy(address->im_locale, p->value);
		address->imvalue_mask |= I18N_IM_LOCALE;
	    }
	    else if (!strcmp(p->name, IMServerTransport)) {
		if (address->imvalue_mask & I18N_IM_ADDRESS) {
		    return IMServerTransport;
		}
		address->im_addr = (char *)malloc(strlen(p->value) + 1);
		if (!address->im_addr)
		  return IMServerTransport;
		strcpy(address->im_addr, p->value);
		address->imvalue_mask |= I18N_IM_ADDRESS;
	    }
	    else if (!strcmp(p->name, IMServerName)) {
		if (address->imvalue_mask & I18N_IM_NAME) {
		    return IMServerName;
		}
		address->im_name = (char *)malloc(strlen(p->value) + 1);
		if (!address->im_name)
		  return IMServerName;
		strcpy(address->im_name, p->value);
		address->imvalue_mask |= I18N_IM_NAME;
	    }
	    else if (!strcmp(p->name, IMServerWindow)) {
		if (address->imvalue_mask & I18N_IMSERVER_WIN) {
		    return IMServerWindow;
		}
		address->im_window = (Window)p->value;
		address->imvalue_mask |= I18N_IMSERVER_WIN;
	    }
	    else if (!strcmp(p->name, IMInputStyles)) {
		if (address->imvalue_mask & I18N_INPUT_STYLES) {
		    return IMInputStyles;
		}
		address->input_styles.count_styles =
		  ((XIMStyles*)p->value)->count_styles;
		address->input_styles.supported_styles =
		  (XIMStyle*)malloc(sizeof(XIMStyle)
				    * address->input_styles.count_styles);
		if (address->input_styles.supported_styles == (XIMStyle*)NULL)
		  return IMInputStyles;
		memmove(address->input_styles.supported_styles,
			((XIMStyles*)p->value)->supported_styles,
			sizeof(XIMStyle) *
			   address->input_styles.count_styles);
		address->imvalue_mask |= I18N_INPUT_STYLES;
	    }
	    else if (!strcmp(p->name, IMProtocolHandler)) {
		address->improto = (IMProtoHandler)p->value;
		address->imvalue_mask |= I18N_IM_HANDLER;
	    }
	    else if (!strcmp(p->name, IMOnKeysList)) {
		if (address->imvalue_mask & I18N_ON_KEYS) {
		    return IMOnKeysList;
		}
		address->on_keys.count_keys =
		  ((XIMTriggerKeys*)p->value)->count_keys;
		address->on_keys.keylist =
		  (XIMTriggerKey*)malloc(sizeof(XIMTriggerKey)
					 * address->on_keys.count_keys);
		if (address->on_keys.keylist == (XIMTriggerKey*)NULL)
		  return IMOnKeysList;
		memmove(address->on_keys.keylist,
			((XIMTriggerKeys*)p->value)->keylist,
			sizeof(XIMTriggerKey) * address->on_keys.count_keys);
		address->imvalue_mask |= I18N_ON_KEYS;
	    }
	    else if (!strcmp(p->name, IMOffKeysList)) {
		if (address->imvalue_mask & I18N_OFF_KEYS) {
		    return IMOffKeysList;
		}
		address->off_keys.count_keys =
		  ((XIMTriggerKeys*)p->value)->count_keys;
		address->off_keys.keylist =
		  (XIMTriggerKey*)malloc(sizeof(XIMTriggerKey)
					 * address->off_keys.count_keys);
		if (address->off_keys.keylist == (XIMTriggerKey*)NULL)
		  return IMOffKeysList;
		memmove(address->off_keys.keylist,
			((XIMTriggerKeys*)p->value)->keylist,
			sizeof(XIMTriggerKey) * address->off_keys.count_keys);
		address->imvalue_mask |= I18N_OFF_KEYS;
	    }
	    else if (!strcmp(p->name, IMEncodingList)) {
		if (address->imvalue_mask & I18N_ENCODINGS) {
		    return IMEncodingList;
		}
		address->encoding_list.count_encodings =
		  ((XIMEncodings*)p->value)->count_encodings;
		address->encoding_list.supported_encodings =
		  (XIMEncoding*)malloc(sizeof(XIMEncoding) *
				       address->encoding_list.count_encodings);
		if (address->encoding_list.supported_encodings
		    == (XIMEncoding*)NULL)
		  return IMEncodingList;
		memmove(address->encoding_list.supported_encodings,
			((XIMEncodings*)p->value)->supported_encodings,
			sizeof(XIMEncoding) *
			  address->encoding_list.count_encodings);
		address->imvalue_mask |= I18N_ENCODINGS;
	    }
	    else if (!strcmp(p->name, IMFilterEventMask)) {
		if (address->imvalue_mask & I18N_FILTERMASK) {
		    return IMFilterEventMask;
		}
		address->filterevent_mask = (long)p->value;
		address->imvalue_mask |= I18N_FILTERMASK;
	    }
	}
	if (mode == I18N_OPEN) {
	    /* check mandatory IM values */
	    if (!(address->imvalue_mask & I18N_IM_LOCALE)) {
		/* locales must be set in IMOpenIM */
		return IMLocale;
	    }
	    if (!(address->imvalue_mask & I18N_IM_ADDRESS)) {
		/* address must be set in IMOpenIM */
		return IMServerTransport;
	    }
	}
    } else if (mode == I18N_GET) {
	for (p = args; p->name != NULL; p++) {
	    if (!strcmp(p->name, IMLocale)) {
		p->value = (char *)malloc(strlen(address->im_locale) + 1);
		if (!p->value)
		  return IMLocale;
		strcpy(p->value, address->im_locale);
	    }
	    else if (!strcmp(p->name, IMServerTransport)) {
		p->value = (char *)malloc(strlen(address->im_addr) + 1);
		if (!p->value)
		  return IMServerTransport;
		strcpy(p->value, address->im_addr);
	    }
	    else if (!strcmp(p->name, IMServerName)) {
		if (address->imvalue_mask & I18N_IM_NAME) {
		    p->value = (char *)malloc(strlen(address->im_name) + 1);
		    if (!p->value)
		      return IMServerName;
		    strcpy(p->value, address->im_name);
		} else {
		    return IMServerName;
		}
	    }
	    else if (!strcmp(p->name, IMServerWindow)) {
		if (address->imvalue_mask & I18N_IMSERVER_WIN) {
		    *((Window *)(p->value)) = address->im_window;
		} else {
		    return IMServerWindow;
		}
	    }
	    else if (!strcmp(p->name, IMInputStyles)) {
		if (GetInputStyles(i18n_core,
				   (XIMStyles **)p->value) == False) {
		    return IMInputStyles;
		}
	    }
	    else if (!strcmp(p->name, IMProtocolHandler)) {
		if (address->imvalue_mask & I18N_IM_HANDLER) {
		    *((IMProtoHandler *)(p->value)) = address->improto;
		} else {
		    return IMProtocolHandler;
		}
	    }
	    else if (!strcmp(p->name, IMOnKeysList)) {
		if (address->imvalue_mask & I18N_ON_KEYS) {
		    if (GetOnOffKeys(i18n_core, I18N_ON_KEYS,
				     (XIMTriggerKeys **)p->value) == False) {
			return IMOnKeysList;
		    }
		} else {
		    return IMOnKeysList;
		}
	    }
	    else if (!strcmp(p->name, IMOffKeysList)) {
		if (address->imvalue_mask & I18N_OFF_KEYS) {
		    if (GetOnOffKeys(i18n_core, I18N_OFF_KEYS,
				     (XIMTriggerKeys **)p->value) == False) {
			return IMOffKeysList;
		    }
		} else {
		    return IMOffKeysList;
		}
	    }
	    else if (!strcmp(p->name, IMEncodingList)) {
		if (address->imvalue_mask & I18N_ENCODINGS) {
		    if (GetEncodings(i18n_core,
				     (XIMEncodings **)p->value) == False) {
			return IMEncodingList;
		    }
		} else {
		    return IMEncodingList;
		}
	    }
	    else if (!strcmp(p->name, IMFilterEventMask)) {
		if (address->imvalue_mask & I18N_FILTERMASK) {
		    *((long *)(p->value)) = address->filterevent_mask;
		} else {
		    return IMFilterEventMask;
		}
	    }
	}
    }
    return NULL;
}

static int
#if NeedFunctionPrototypes
CheckIMName(Xi18n i18n_core)
#else
CheckIMName(i18n_core)
Xi18n i18n_core;
#endif
{
    char *address = i18n_core->address.im_addr;
    int i;

    for (i = 0; _TransR[i].transportname; i++) {
	while (*address == ' ' || *address == '\t') address++;
	if (!strncmp(address, _TransR[i].transportname,
		     _TransR[i].namelen)
	    && address[_TransR[i].namelen] == '/') {
	    if (_TransR[i].checkAddr(i18n_core, &_TransR[i],
			     address+_TransR[i].namelen + 1) == True) {
		return True;
	    } else {
		return False;
	    }
	}
    }
    return False;
}

#ifndef XIM_SERVERS
#define XIM_SERVERS "XIM_SERVERS"
#endif

static int
#if NeedFunctionPrototypes
SetXi18nSelectionOwner(Xi18n i18n_core)
#else
SetXi18nSelectionOwner(i18n_core)
Xi18n i18n_core;
#endif
{
    static Atom XIM_Servers = None;
    Display *dpy = i18n_core->address.dpy;
    Window ims_win = i18n_core->address.im_window;
    Window root = RootWindow(dpy, DefaultScreen(dpy));
    Atom realtype;
    int realformat;
    unsigned long bytesafter;
    long *data;
    unsigned long length;
    Atom atom;
    int i;
    int found;
    int forse = False;
    char buf[256];

    (void)sprintf(buf, "@server=%s", i18n_core->address.im_name);
    if ((atom = XInternAtom(dpy, buf, False)) == 0) {
	return False;
    }
    i18n_core->address.selection = atom;

    if (XIM_Servers == None)
      XIM_Servers = XInternAtom(dpy, XIM_SERVERS, False);
    XGetWindowProperty(dpy, root,
		       XIM_Servers, 0L, 1000000L, False, XA_ATOM,
		       &realtype, &realformat, &length,
		       &bytesafter, (unsigned char **)(&data));
    if (realtype == None) {
	/* specified property doesn't exist yet */
    } else if (realtype != XA_ATOM) {
	/* wrong type */
	return False;
    } else if (realformat != 32) {
	/* wrong format */
	if (data != NULL) XFree((char *)data);
	return False;
    }
    found = False;
    for (i = 0; i < length; i++) {
	if (data[i] == atom) {
	    Window owner;
	    found = True;
	    if ((owner = XGetSelectionOwner(dpy, atom)) != ims_win) {
		if (owner == None || forse == True)
		  XSetSelectionOwner(dpy, atom, ims_win, CurrentTime);
		else
		  return False;
	    }
	    break;
	}
    }
    if (data != NULL) XFree((char *)data);

    if (found == False) {
	XSetSelectionOwner(dpy, atom, ims_win, CurrentTime);
	(void)XChangeProperty(dpy, root, XIM_Servers, XA_ATOM,
			      32, PropModePrepend, (unsigned char *)&atom, 1);
    }
    /* Intern "LOCALES" and "TRANSOPORT" Target Atoms */
    i18n_core->address.Localename = XInternAtom(dpy, LOCALES, False);
    i18n_core->address.Transportname = XInternAtom(dpy, TRANSPORT, False);
    return (XGetSelectionOwner(dpy, atom) == ims_win);
}

/* XIM protocol methods */
static void *
#if NeedFunctionPrototypes
xi18n_setup(Display *dpy, XIMArg *args)
#else
xi18n_setup(dpy, args)
Display *dpy;
XIMArg *args;
#endif
{
    Xi18n i18n_core;
    CARD16 endian = 1;

    if ((i18n_core = (Xi18n)malloc(sizeof(Xi18nCore)))
	== (Xi18n)NULL) {
	return NULL;
    }
    memset(i18n_core, 0, sizeof(Xi18nCore));

    i18n_core->address.dpy = dpy;

    if (ParseArgs(i18n_core, I18N_OPEN, args) != NULL) {
	XFree(i18n_core);
	return NULL;
    }
    if (*(char *)&endian) {
	i18n_core->address.im_byteOrder = 'l';
    } else {
	i18n_core->address.im_byteOrder = 'B';
    }

    /* install IMAttr and ICAttr list in i18n_core */
    _Xi18nInitAttrList(i18n_core);

    /* install IMExtension list in i18n_core */
    _Xi18nInitExtension(i18n_core);

    return i18n_core;
}

static void
#if NeedFunctionPrototypes
ReturnSelectionNotify(Xi18n i18n_core, XSelectionRequestEvent *ev)
#else
ReturnSelectionNotify(i18n_core, ev)
Xi18n i18n_core;
XSelectionRequestEvent *ev;
#endif
{
    XEvent event;
    Display *dpy = i18n_core->address.dpy;
    char buf[256];

    event.type = SelectionNotify;
    event.xselection.requestor = ev->requestor;
    event.xselection.selection = ev->selection;
    event.xselection.target = ev->target;
    event.xselection.time = ev->time;
    event.xselection.property = ev->property;
    if (ev->target == i18n_core->address.Localename) {
	(void)sprintf(buf, "@locale=%s", i18n_core->address.im_locale);
    } else if (ev->target == i18n_core->address.Transportname) {
	(void)sprintf(buf, "@transport=%s", i18n_core->address.im_addr);
    }
    XChangeProperty(dpy, event.xselection.requestor,
		    ev->target, ev->target, 8,
		    PropModeReplace, (unsigned char *)buf, strlen(buf));
    XSendEvent(dpy, event.xselection.requestor, False, NoEventMask, &event);
    XFlush(i18n_core->address.dpy);
}

static Bool
#if NeedFunctionPrototypes
WaitXSelectionRequest(Display *dpy, Window win,
		      XEvent *ev, XPointer client_data)
#else
WaitXSelectionRequest(dpy, win, ev, client_data)
Display *dpy;
Window win;
XEvent *ev;
XPointer client_data;
#endif
{
    XIMS ims = (XIMS)client_data;
    Xi18n i18n_core = ims->protocol;

    if (((XSelectionRequestEvent *)ev)->selection ==
	i18n_core->address.selection) {
	ReturnSelectionNotify(i18n_core,
			      (XSelectionRequestEvent *)ev);
	return True;
    }
    return False;
}

static Status
#if NeedFunctionPrototypes
xi18n_openIM(XIMS ims)
#else
xi18n_openIM(ims)
XIMS ims;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Display *dpy = i18n_core->address.dpy;

    if (!CheckIMName(i18n_core)
	|| !SetXi18nSelectionOwner(i18n_core)
	|| !i18n_core->methods.begin(ims)) {
	XFree(i18n_core->address.im_name);
	XFree(i18n_core->address.im_locale);
	XFree(i18n_core->address.im_addr);
	XFree(i18n_core);
	return False;
    }

    _XRegisterFilterByType(dpy, i18n_core->address.im_window,
			   SelectionRequest, SelectionRequest,
			   WaitXSelectionRequest, ims);
    return True;
}

static Status
#if NeedFunctionPrototypes
xi18n_closeIM(XIMS ims)
#else
xi18n_closeIM(ims)
XIMS ims;
#endif
{
    Xi18n i18n_core = ims->protocol;
    Display *dpy = i18n_core->address.dpy;

    if (!i18n_core->methods.end(ims)) {
	return False;
    }
    _XUnregisterFilter(dpy, i18n_core->address.im_window,
		       WaitXSelectionRequest, ims);
    XFree(i18n_core->address.im_name);
    XFree(i18n_core->address.im_locale);
    XFree(i18n_core->address.im_addr);
    XFree(i18n_core);
    return True;
}

static char *
#if NeedFunctionPrototypes
xi18n_setIMValues(XIMS ims, XIMArg *args)
#else
xi18n_setIMValues(ims, args)
XIMS ims;
XIMArg *args;
#endif
{
    Xi18n i18n_core = ims->protocol;
    char *ret;

    if ((ret = ParseArgs(i18n_core, I18N_SET, args)) != NULL) {
	return ret;
    }
    return NULL;
}

static char *
#if NeedFunctionPrototypes
xi18n_getIMValues(XIMS ims, XIMArg *args)
#else
xi18n_getIMValues(ims, args)
XIMS ims;
XIMArg *args;
#endif
{
    Xi18n i18n_core = ims->protocol;
    char *ret;

    if ((ret = ParseArgs(i18n_core, I18N_GET, args)) != NULL) {
	return ret;
    }
    return NULL;
}

static void
#if NeedFunctionPrototypes
EventToWireEvent(XEvent *ev, xEvent *event, CARD16 *serial)
#else
EventToWireEvent(ev, event, serial)
XEvent *ev;
xEvent *event;			/* wire protocol event */
CARD16 *serial;
#endif
{
    *serial = (CARD16)(ev->xany.serial >> 16);
    event->u.u.sequenceNumber = (CARD16)(ev->xany.serial & (unsigned long)0xffff);

    switch (ev->type) {
      case KeyPress:
      case KeyRelease:
	{
	    XKeyEvent *kev = (XKeyEvent*)ev;

	    event->u.u.type = ev->type;
	    event->u.keyButtonPointer.root = kev->root;
	    event->u.keyButtonPointer.state = kev->state;
	    event->u.keyButtonPointer.time = kev->time;
	    event->u.keyButtonPointer.event = kev->window;
	    event->u.keyButtonPointer.child = kev->subwindow;
	    event->u.keyButtonPointer.eventX = kev->x;
	    event->u.keyButtonPointer.eventY = kev->y;
	    event->u.keyButtonPointer.rootX = kev->x_root;
	    event->u.keyButtonPointer.rootY = kev->y_root;
	    event->u.keyButtonPointer.sameScreen = kev->same_screen;
	    event->u.u.detail = kev->keycode;
	}
    }
}

static Status
#if NeedFunctionPrototypes
xi18n_forwardEvent(XIMS ims, IMForwardEventStruct *call_data)
#else
xi18n_forwardEvent(ims, call_data)
XIMS ims;
IMForwardEventStruct *call_data;
#endif
{
    Xi18n i18n_core = ims->protocol;
    FrameMgr fm;
    extern XimFrameRec forward_event_fr[];
    register int total_size;
    unsigned char *reply = NULL;
    unsigned char *replyp;
    CARD16 serial;
    int event_size;
    Xi18nClient *client = (Xi18nClient *)_Xi18nFindClient(i18n_core,
					  call_data->connect_id);

    /* create FrameMgr */
    fm = FrameMgrInit(forward_event_fr, NULL,
		      _Xi18nNeedSwap(i18n_core, call_data->connect_id));

    total_size = FrameMgrGetTotalSize(fm);
    event_size = sizeof(xEvent);
    reply = (unsigned char *)malloc(total_size + event_size);
    if (!reply) {
	_Xi18nSendMessage(ims, call_data->connect_id,
			  XIM_ERROR, 0, 0, 0);
	return False;
    }
    memset(reply, 0, total_size + event_size);
    FrameMgrSetBuffer(fm, reply);
    replyp = reply;

    call_data->sync_bit = 1;	/* always sync */
    client->sync = True;

    FrameMgrPutToken(fm, call_data->connect_id);
    FrameMgrPutToken(fm, call_data->icid);
    FrameMgrPutToken(fm, call_data->sync_bit);

    replyp += total_size;
    EventToWireEvent(&(call_data->event), (xEvent *)replyp, &serial);

    FrameMgrPutToken(fm, serial);

    _Xi18nSendMessage(ims, call_data->connect_id,
		      XIM_FORWARD_EVENT, 0,
		      reply, total_size + event_size);

    XFree(reply);
    /* free FrameMgr */
    FrameMgrFree(fm);

    return True;
}

static Status
#if NeedFunctionPrototypes
xi18n_commit(XIMS ims, IMCommitStruct *call_data)
#else
xi18n_commit(ims, call_data)
XIMS ims;
IMCommitStruct *call_data;
#endif
{
    Xi18n i18n_core = ims->protocol;
    FrameMgr fm;
    extern XimFrameRec commit_chars_fr[], commit_both_fr[];
    register int total_size;
    unsigned char *reply = NULL;
    CARD16 str_length;

    call_data->flag |= XimSYNCHRONUS; /* always sync */

    if (!(call_data->flag & XimLookupKeySym) &&
	(call_data->flag & XimLookupChars)) {
	/* create FrameMgr */
	fm = FrameMgrInit(commit_chars_fr, NULL,
			  _Xi18nNeedSwap(i18n_core, call_data->connect_id));

	/* set length of STRING8 */
	str_length = strlen(call_data->commit_string);
	FrameMgrSetSize(fm, str_length);
	total_size = FrameMgrGetTotalSize(fm);
	reply = (unsigned char *)malloc(total_size);
	if (!reply) {
	    _Xi18nSendMessage(ims, call_data->connect_id,
			      XIM_ERROR, 0, 0, 0);
	    return False;
	}
	memset(reply, 0, total_size);
	FrameMgrSetBuffer(fm, reply);

	str_length = FrameMgrGetSize(fm);
	FrameMgrPutToken(fm, call_data->connect_id);
	FrameMgrPutToken(fm, call_data->icid);
	FrameMgrPutToken(fm, call_data->flag);
	FrameMgrPutToken(fm, str_length);
	FrameMgrPutToken(fm, call_data->commit_string);
    } else {
	/* create FrameMgr */
	fm = FrameMgrInit(commit_both_fr, NULL,
			  _Xi18nNeedSwap(i18n_core, call_data->connect_id));
	/* set length of STRING8 */
	str_length = strlen(call_data->commit_string);
	if (str_length > 0)
	  FrameMgrSetSize(fm, str_length);

	total_size = FrameMgrGetTotalSize(fm);
	reply = (unsigned char *)malloc(total_size);
	if (!reply) {
	    _Xi18nSendMessage(ims, call_data->connect_id,
			      XIM_ERROR, 0, 0, 0);
	    return False;
	}
	FrameMgrPutToken(fm, call_data->connect_id);
	FrameMgrPutToken(fm, call_data->icid);
	FrameMgrPutToken(fm, call_data->flag);
	FrameMgrPutToken(fm, call_data->keysym);
	if (str_length > 0) {
	  str_length = FrameMgrGetSize(fm);
	  FrameMgrPutToken(fm, str_length);
	  FrameMgrPutToken(fm, call_data->commit_string);
      }
    }
    _Xi18nSendMessage(ims, call_data->connect_id,
		      XIM_COMMIT, 0, reply, total_size);
    /* free FrameMgr */
    FrameMgrFree(fm);
    XFree(reply);

    return True;
}

static int
#if NeedFunctionPrototypes
xi18n_callCallback(XIMS ims, IMProtocol *call_data)
#else
xi18n_callCallback(ims, call_data)
XIMS ims;
IMProtocol *call_data;
#endif
{
    switch (call_data->major_code) {
      case XIM_GEOMETRY:
	return _Xi18nGeometryCallback(ims, call_data);
      case XIM_PREEDIT_START:
	return _Xi18nPreeditStartCallback(ims, call_data);
      case XIM_PREEDIT_DRAW:
	return _Xi18nPreeditDrawCallback(ims, call_data);
      case XIM_PREEDIT_CARET:
	return _Xi18nPreeditCaretCallback(ims, call_data);
      case XIM_PREEDIT_DONE:
	return _Xi18nPreeditDoneCallback(ims, call_data);
      case XIM_STATUS_START:
	return _Xi18nStatusStartCallback(ims, call_data);
      case XIM_STATUS_DRAW:
	return _Xi18nStatusDrawCallback(ims, call_data);
      case XIM_STATUS_DONE:
	return _Xi18nStatusDoneCallback(ims, call_data);
      case XIM_STR_CONVERSION:
	return _Xi18nStringConversionCallback(ims, call_data);
      default:
	return False;
    }
}

/* preeditStart and preeditEnd are used only for Dynamic Event Flow. */
static int
#if NeedFunctionPrototypes
xi18n_preeditStart(XIMS ims, IMProtocol *call_data)
#else
xi18n_preeditStart(ims, call_data)
XIMS ims;
IMProtocol *call_data;
#endif
{
    Xi18n i18n_core = ims->protocol;
    IMPreeditStateStruct *preedit_state =
      (IMPreeditStateStruct *)&call_data->preedit_state;
    long mask;
    int on_key_num = i18n_core->address.on_keys.count_keys;
    int off_key_num = i18n_core->address.off_keys.count_keys;

    if (on_key_num == 0 && off_key_num == 0)
      return False;

    if (i18n_core->address.imvalue_mask & I18N_FILTERMASK)
      mask = i18n_core->address.filterevent_mask;
    else
      mask = DEFAULT_FILTER_MASK;

      _Xi18nSetEventMask(ims, preedit_state->connect_id,
			 preedit_state->connect_id, preedit_state->icid,
			 mask, ~mask);
    return True;
}

static int
#if NeedFunctionPrototypes
xi18n_preeditEnd(XIMS ims, IMProtocol *call_data)
#else
xi18n_preeditEnd(ims, call_data)
XIMS ims;
IMProtocol *call_data;
#endif
{
    Xi18n i18n_core = ims->protocol;
    int on_key_num = i18n_core->address.on_keys.count_keys;
    int off_key_num = i18n_core->address.off_keys.count_keys;
    IMPreeditStateStruct *preedit_state =
      (IMPreeditStateStruct *)&call_data->preedit_state;

    if (on_key_num == 0 && off_key_num == 0)
      return False;

    _Xi18nSetEventMask(ims, preedit_state->connect_id,
		       preedit_state->connect_id, preedit_state->icid,
		       0, 0);
    return True;
}
