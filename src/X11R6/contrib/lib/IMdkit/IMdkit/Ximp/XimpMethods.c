/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpMethods.c	1.3 94/02/16 */
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

#ifndef _XIMPMETHODS_C_
#define _XIMPMETHODS_C_
#include <X11/Xatom.h>
#define	NEED_EVENTS
#include <X11/Xlibint.h>
#include "XimpData.h"

#if NeedFunctionPrototypes
extern XimpClient *_XimpFindClient(XIMPCore, CARD32, IMPProtocol*);
extern void _XimpSendByClientMessage(XIMPCore, long, char *, int);
extern void _XimpSendByProperty(XIMPCore, long, char *, int);
extern void _XimpSendIMProtocol(XIMPCore, XimpClient *, int,
				unsigned long, unsigned long,
				unsigned long, unsigned long);
extern void _XimpRegisterKeyPressFilter(XIMS, XimpClient *);
extern void _XimpRegisterKeyReleaseFilter(XIMS, XimpClient *);
extern void _XimpUnregisterKeyPressFilter(XIMS, XimpClient *);
extern void _XimpUnregisterKeyReleaseFilter(XIMS, XimpClient *);
static void *ximp_setup(Display*, XIMArg*);
static Status ximp_openIM(XIMS);
static Status ximp_closeIM(XIMS);
static char *ximp_setIMValues(XIMS, XIMArg*);
static char *ximp_getIMValues(XIMS, XIMArg*);
static Status ximp_forwardEvent(XIMS, IMPProtocol*);
static Status ximp_commitString(XIMS, IMPProtocol *);
static Status ximp_callCallback(XIMS, IMPProtocol *);
static Status ximp_preeditStart(XIMS, IMPProtocol *);
static Status ximp_preeditEnd(XIMS, IMPProtocol *);
#else
extern XimpClient *_XimpFindClient();
extern void _XimpSendByClientMessage();
extern void _XimpSendByProperty();
extern void _XimpSendIMProtocol();
extern void _XimpRegisterKeyPressFilter();
extern void _XimpRegisterKeyReleaseFilter();
extern void _XimpUnregisterKeyPressFilter();
extern void _XimpUnregisterKeyReleaseFilter();
static void *ximp_setup();
static Status ximp_openIM();
static Status ximp_closeIM();
static char *ximp_setIMValues();
static char *ximp_getIMValues();
static Status ximp_forwardEvent();
static Status ximp_commitString();
static Status ximp_callCallback();
static Status ximp_preeditStart();
static Status ximp_preeditEnd();
#endif

IMMethodsRec Ximp_im_methods = {
    ximp_setup,
    ximp_openIM,
    ximp_closeIM,
    ximp_setIMValues,
    ximp_getIMValues,
    ximp_forwardEvent,
    ximp_commitString,
    ximp_callCallback,
    ximp_preeditStart,
    ximp_preeditEnd,
};

static char *
#if NeedFunctionPrototypes
ParseXimpExtensionArgs(XIMPCore core, int mode, XIMArg *args)
#else
ParseXimpExtensionArgs(core, mode, args)
XIMPCore core;
int	mode;
XIMArg	*args;
#endif
{
    XIMArg	*ptr;

    if (mode == XIMP_OPEN || mode == XIMP_SET) {
	for (ptr = args; ptr->name != NULL; ptr++) {
	    if (!strcmp(ptr->name, XIMPExtStatusWin)) {
		core->ext_flag |= EXT_STATUS;
	    }
	    else if (!strcmp(ptr->name, XIMPExtBackFront)) {
		core->ext_flag |= EXT_BACKFRONT;
		core->ext_backfront = (long)ptr->value;
	    }
	    else if (!strcmp(ptr->name, XIMPExtConversion)) {
		core->ext_flag |= EXT_CONV;
	    }
	}
    } else if (mode == XIMP_GET) {
	for (ptr = args; ptr->name != NULL; ptr++) {
	    if (!strcmp(ptr->name, XIMPExtStatusWin)) {
		*((Bool*)ptr->value) = (core->ext_flag & EXT_STATUS ?
					True : False);
	    }
	    else if (!strcmp(ptr->name, XIMPExtBackFront)) {
		*((long*)ptr->value) = (core->ext_flag & EXT_BACKFRONT ?
					core->ext_backfront : -1);
	    }
	    else if (!strcmp(ptr->name, XIMPExtConversion)) {
		*((Bool*)ptr->value) = (core->ext_flag & EXT_CONV ?
					True : False);
	    }
	}
    }
    return NULL;
}

static char *
#if NeedFunctionPrototypes
ParseXimpArgs(XIMPCore core, int mode, XIMArg *args)
#else
ParseXimpArgs(core, mode, args)
XIMPCore core;
int	mode;
XIMArg	*args;
#endif
{
    XIMArg	*ptr;

    if (mode == XIMP_OPEN || mode == XIMP_SET) {
	for (ptr = args; ptr->name != NULL; ptr++) {
	    if (!strcmp(ptr->name, XIMPVersion)) {
		core->ximp_version = (char*)Xmalloc(strlen((char*)ptr->value) + 1);
		if (core->ximp_version == (char *)NULL)
		  return XIMPVersion;
		strcpy(core->ximp_version, (char*)ptr->value);
	    }
	    else if (!strcmp(ptr->name, XIMPType)) {
		core->ximp_type.num_of_types =
		  ((XIMPTypeRec*)ptr->value)->num_of_types;
		core->ximp_type.types =
		  (unsigned long*)Xmalloc(sizeof(unsigned long) *
					  core->ximp_type.num_of_types);
		if (core->ximp_type.types == (unsigned long*)NULL)
		  return XIMPType;
		memmove(core->ximp_type.types,
			((XIMPTypeRec*)ptr->value)->types,
			sizeof(XIMPTypeRec) * core->ximp_type.num_of_types);
	    }
	    else if (!strcmp(ptr->name, XIMPServerVersion)) {
		core->im_version = (char*)Xmalloc(strlen((char*)ptr->value) + 1);
		if (core->im_version == (char *)NULL)
		  return XIMPServerVersion;
		strcpy(core->im_version, (char*)ptr->value);
	    }
	    else if (!strcmp(ptr->name, XIMPVendorName)) {
		core->vendor = (char*)Xmalloc(strlen((char*)ptr->value) + 1);
		if (core->vendor == (char *)NULL)
		  return XIMPVendorName;
		strcpy(core->vendor, (char*)ptr->value);
	    }
	    else if (!strcmp(ptr->name, XIMPExtension)) {
		char *ret;
		if ((ret = ParseXimpExtensionArgs(core, mode,
						  (XIMArg*)ptr->value))
		    != NULL)
		  return ret;
	    }
	}
	if (mode == XIMP_OPEN) {
	    /* check mandatory IM values */
	}
	return NULL;
    } else if (mode == XIMP_GET) {
	for (ptr = args; ptr->name != NULL; ptr++) {
	    if (!strcmp(ptr->name, XIMPVersion)) {
		ptr->value = (char *)Xmalloc(strlen(core->ximp_version) + 1);
		if (!ptr->value)
		  return XIMPVersion;
		strcpy(ptr->value, core->ximp_version);
	    }
	    else if (!strcmp(ptr->name, XIMPType)) {
		XIMPTypeRec **p_type = (XIMPTypeRec**)(ptr->value);
		unsigned int len;
		*p_type = (XIMPTypeRec*)Xmalloc(sizeof(XIMPTypeRec));
		if (*p_type == (XIMPTypeRec*)NULL)
		  return XIMPType;
		(*p_type)->num_of_types = core->ximp_type.num_of_types;

		len = sizeof(unsigned long) * core->ximp_type.num_of_types;
		(*p_type)->types = (unsigned long*)Xmalloc(len);
		if ((*p_type)->types == (unsigned long*)NULL)
		  return XIMPType;
		memmove((*p_type)->types, core->ximp_type.types, len);
	    }
	    else if (!strcmp(ptr->name, XIMPServerVersion)) {
		ptr->value = (char *)Xmalloc(strlen(core->im_version) + 1);
		if (!ptr->value)
		  return XIMPServerVersion;
		strcpy(ptr->value, core->im_version);
	    }
	    else if (!strcmp(ptr->name, XIMPExtension)) {
		char *ret;
		if ((ret = ParseXimpExtensionArgs(core, mode,
						  (XIMArg*)ptr->value))
		    != NULL)
		  return ret;
	    }
	}
	return NULL;
    }
    return NULL;
}

static char *
#if NeedFunctionPrototypes
ParseArgs(XIMPCore core, int mode, XIMArg *args)
#else
ParseArgs(core, mode, args)
XIMPCore core;
int	mode;
XIMArg	*args;
#endif
{
    XIMArg	*ptr;

    if (mode == XIMP_OPEN || mode == XIMP_SET) {
	for (ptr = args; ptr->name != NULL; ptr++) {
	    if (!strcmp(ptr->name, IMServerWindow)) {
		if (core->imvalue_mask & I18N_IMSERVER_WIN) {
		    return IMServerWindow;
		}
		core->im_window = (Window)ptr->value;
		core->imvalue_mask |= I18N_IMSERVER_WIN;
	    }
	    else if (!strcmp(ptr->name, IMServerName)) {
		if (core->imvalue_mask & I18N_IM_NAME) {
		    return IMServerName;
		}
		core->im_name = (char*)Xmalloc(strlen((char*)ptr->value) + 1);
		if (core->im_name == (char *)NULL)
		  return IMServerName;
		strcpy(core->im_name, (char*)ptr->value);
		core->imvalue_mask |= I18N_IM_NAME;
	    }
	    else if (!strcmp(ptr->name, IMLocale)) {
		if (core->imvalue_mask & I18N_IM_LOCALE) {
		    return IMLocale;
		}
		core->im_locale =
		  (char*)Xmalloc(strlen((char*)ptr->value) + 1);
		if (core->im_locale == (char*)NULL)
		  return IMLocale;
		strcpy(core->im_locale, (char*)ptr->value);
		core->imvalue_mask |= I18N_IM_LOCALE;
	    }
	    else if (!strcmp(ptr->name, IMInputStyles)) {
		if (core->imvalue_mask & I18N_INPUT_STYLES) {
		    return IMInputStyles;
		}
		core->styles.count_styles =
		  ((XIMStyles*)ptr->value)->count_styles;
		core->styles.supported_styles =
		  (XIMStyle*)Xmalloc(sizeof(XIMStyle)
				     * core->styles.count_styles);
		if (core->styles.supported_styles == (XIMStyle*)NULL)
		  return IMInputStyles;
		memmove(core->styles.supported_styles,
			((XIMStyles*)ptr->value)->supported_styles,
			sizeof(XIMStyle) * core->styles.count_styles);
		core->imvalue_mask |= I18N_INPUT_STYLES;
	    }
	    else if (!strcmp(ptr->name, IMOnKeysList)) {
		if (mode == XIMP_SET) {
		    /* set in IMOpenIM if set */
		    return IMOnKeysList;
		}
		core->start_keys.count_keys =
		  ((XIMTriggerKeys*)ptr->value)->count_keys;
		core->start_keys.keylist =
		  (XIMTriggerKey*)Xmalloc(sizeof(XIMTriggerKey)
					  * core->start_keys.count_keys);
		if (core->start_keys.keylist == (XIMTriggerKey*)NULL)
		  return IMOnKeysList;
		memmove(core->start_keys.keylist,
			((XIMTriggerKeys*)ptr->value)->keylist,
			sizeof(XIMTriggerKey) * core->start_keys.count_keys);
		core->imvalue_mask |= I18N_ON_KEYS;
	    }
	    else if (!strcmp(ptr->name, IMOffKeysList)) {
		if (mode == XIMP_SET) {
		    /* set in IMOpenIM if set */
		    return IMOffKeysList;
		}
		core->stop_keys.count_keys =
		  ((XIMTriggerKeys*)ptr->value)->count_keys;
		core->stop_keys.keylist =
		  (XIMTriggerKey*)Xmalloc(sizeof(XIMTriggerKey)
					  * core->stop_keys.count_keys);
		if (core->stop_keys.keylist == (XIMTriggerKey*)NULL)
		  return IMOffKeysList;
		memmove(core->stop_keys.keylist,
			((XIMTriggerKeys*)ptr->value)->keylist,
			sizeof(XIMTriggerKey) * core->stop_keys.count_keys);
		core->imvalue_mask |= I18N_OFF_KEYS;
	    }
	    else if (!strcmp(ptr->name, IMProtocolHandler)) {
		core->improto = (IMProtoHandler)ptr->value;
	    }
	    else if (!strcmp(ptr->name, IMProtocolDepend)) {
		char *ret;
		if (core->imvalue_mask & I18N_PROTO_DEPEND) {
		    return IMProtocolDepend;
		}
		if ((ret = ParseXimpArgs(core, mode,
					 (XIMArg*)ptr->value))
		    != NULL)
		  return ret;
		core->imvalue_mask |= I18N_PROTO_DEPEND;
	    }
	}
	/* check mandatory IM values */
	if (mode == XIMP_OPEN) {
	    if (!(core->imvalue_mask & I18N_IMSERVER_WIN)) {
		/* provides default window */
		core->im_window = XCreateSimpleWindow(core->display,
					      DefaultRootWindow(core->display),
					      0, 0, 1, 1, 1, 0, 0);
		core->imvalue_mask |= I18N_IMSERVER_WIN;
	    }
	    if (!(core->imvalue_mask & I18N_IM_NAME)) {
		/* IMname must be set in IMOpenIM */
		return IMServerName;
	    }
	    if (!(core->imvalue_mask & I18N_IM_LOCALE)) {
		/* locales must be set in IMOpenIM */
		return IMLocale;
	    }
	    if (!(core->imvalue_mask & I18N_INPUT_STYLES)) {
		/* Supported input styles must be set in IMOpenIM */
		return IMInputStyles;
	    }
	    if (!(core->imvalue_mask & I18N_PROTO_DEPEND)) {
		/* Ximp protocol dependent values must be set in IMOpenIM */
		return IMProtocolDepend;
	    }
	}
	return NULL;
    } else if (mode == XIMP_GET) {
	for (ptr = args; ptr->name != NULL; ptr++) {
	    if (!strcmp(ptr->name, IMLocale)) {
		ptr->value = (char *)Xmalloc(strlen(core->im_locale) + 1);
		if (!ptr->value)
		  return IMLocale;
		strcpy(ptr->value, core->im_locale);
	    }
	    else if (!strcmp(ptr->name, IMServerName)) {
		ptr->value = (char *)Xmalloc(strlen(core->im_name) + 1);
		if (!ptr->value)
		  return IMServerName;
		strcpy(ptr->value, core->im_name);
	    }
	    else if (!strcmp(ptr->name, IMServerWindow)) {
		*((Window *)(ptr->value)) = core->im_window;
	    }
	    else if (!strcmp(ptr->name, IMInputStyles)) {
		XIMStyles **p_styles = (XIMStyles**)ptr->value;
		*p_styles = (XIMStyles*)Xmalloc(sizeof(XIMStyles));
		if (*p_styles == NULL)
		  return IMInputStyles;
		(*p_styles)->count_styles = core->styles.count_styles;
		(*p_styles)->supported_styles =
		  (XIMStyle*)Xmalloc(sizeof(XIMStyle) *
				     core->styles.count_styles);
		if ((XIMStyle*)(*p_styles)->supported_styles == NULL)
		  return IMInputStyles;
		memmove((XIMStyle*)(*p_styles)->supported_styles,
			core->styles.supported_styles,
			sizeof(XIMStyle) * (core->styles.count_styles));
	    }
	    else if (!strcmp(ptr->name, IMProtocolHandler)) {
		*((IMProtoHandler *)(ptr->value)) = core->improto;
	    }
	    else if (!strcmp(ptr->name, IMOnKeysList)) {
		if (core->imvalue_mask & I18N_ON_KEYS) {
		    XIMTriggerKeys **p_trigger = (XIMTriggerKeys**)ptr->value;
		    *p_trigger = (XIMTriggerKeys*)Xmalloc(sizeof(XIMTriggerKeys));
		    if (*p_trigger == NULL)
		      return IMOnKeysList;
		    (*p_trigger)->count_keys = core->start_keys.count_keys;
		    (*p_trigger)->keylist =
		      (XIMTriggerKey*)Xmalloc(sizeof(XIMTriggerKey) *
					      core->start_keys.count_keys);
		    if ((XIMTriggerKey*)(*p_trigger)->keylist == NULL)
		      return IMOnKeysList;
		    memmove((XIMTriggerKey*)(*p_trigger)->keylist,
			    core->start_keys.keylist,
			    sizeof(XIMTriggerKey) * (core->start_keys.count_keys));
		} else {
		    return IMOnKeysList;
		}
	    }
	    else if (!strcmp(ptr->name, IMOffKeysList)) {
		if (core->imvalue_mask & I18N_OFF_KEYS) {
		    XIMTriggerKeys **p_trigger = (XIMTriggerKeys**)ptr->value;
		    *p_trigger = (XIMTriggerKeys*)Xmalloc(sizeof(XIMTriggerKeys));
		    if (*p_trigger == NULL)
		      return IMOffKeysList;
		    (*p_trigger)->count_keys = core->stop_keys.count_keys;
		    (*p_trigger)->keylist =
		      (XIMTriggerKey*)Xmalloc(sizeof(XIMTriggerKey) *
					      core->stop_keys.count_keys);
		    if ((XIMTriggerKey*)(*p_trigger)->keylist == NULL)
		      return IMOffKeysList;
		    memmove((XIMTriggerKey*)(*p_trigger)->keylist,
			    core->start_keys.keylist,
			    sizeof(XIMTriggerKey) * (core->start_keys.count_keys));
		} else {
		    return IMOffKeysList;
		}
	    }
	    else if (!strcmp(ptr->name, IMProtocolDepend)) {
		char *ret;
		if ((ret = ParseXimpArgs(core, mode,
					 (XIMArg*)ptr->value))
		    != NULL)
		  return ret;
	    }
	}
	return NULL;
    }
    return NULL;
}

static void *
#if NeedFunctionPrototypes
ximp_setup(Display *display, XIMArg *args)
#else
ximp_setup(display, args)
Display *display;
XIMArg *args;
#endif
{
    XIMPCore	imp_core = (XIMPCore)NULL;
    if ((imp_core = (XIMPCore)Xmalloc(sizeof(XIMPCoreRec))) == NULL)
      return (void *)NULL;
    memset(imp_core, 0, sizeof(XIMPCoreRec));

    imp_core->display = display;
    imp_core->screen = DefaultScreen(display);

    if (ParseArgs(imp_core, XIMP_OPEN, args) != NULL) {
	XFree(imp_core);
	return NULL;
    }
    return (void*)imp_core;
}

static char *
#if NeedFunctionPrototypes
NextLocaleName(char **list)
#else
NextLocaleName(list)
char **list;
#endif
{
    register char *p;
    
    for (p = *list; **list && **list != ','; (*list)++);
    if (**list == ',') {
	**list = '\0';
	(*list)++;
    } else if (p == *list) {
	p = (char *)NULL;
    }
    return(p);
}

static Bool
#if NeedFunctionPrototypes
SetSelectionOwner(XIMPCore core, char *name, Atom *selection)
#else
SetSelectionOwner(core, name, selection)
XIMPCore core;
char *name;
Atom *selection;
#endif
{
    Display *display = core->display;
    Window im_win = core->im_window;
    Window owner;
    *selection = XInternAtom(display, name, False);

    if ((owner = XGetSelectionOwner(display, *selection)) != im_win) {
	if (owner == None) {
	    XSetSelectionOwner(display, *selection, im_win, CurrentTime);
	    return True;
	} else {
	    return False;
	}
    }
    return True;
}

static int
#if NeedFunctionPrototypes
SetPropInAtom(XIMPCore core)
#else
SetPropInAtom(core)
XIMPCore core;
#endif
{
    Display *display = core->display;
    Window im_win = core->im_window;
    XIMPAtoms *atoms = (XIMPAtoms*)&core->atoms;
    Atom extensions[10];
    int ext_number;
    Ximp_KeyList start_keys;
    Ximp_KeyList stop_keys;
    register int i;
    
#define INTERN(s)	XInternAtom(display, s, False)
    atoms->ctext_type = INTERN("COMPOUND_TEXT");
    atoms->version = INTERN(_XIMP_VERSION);
    atoms->supported_styles = INTERN(_XIMP_STYLE);
    atoms->supported_types = INTERN(_XIMP_TYPE);
    atoms->server_name = INTERN(_XIMP_SERVERNAME);
    atoms->server_version = INTERN(_XIMP_SERVERVERSION);
    atoms->vendor_name = INTERN(_XIMP_VENDORNAME);
    atoms->keys = INTERN(_XIMP_KEYS);
    atoms->sproc_started_keys = INTERN(_XIMP_SPROC_STARTED_KEYS);
    atoms->sproc_stopped_keys = INTERN(_XIMP_SPROC_STOPPED_KEYS);
    atoms->extensions = INTERN(_XIMP_EXTENSIONS);
    atoms->focus = INTERN(_XIMP_FOCUS);
    atoms->preedit = INTERN(_XIMP_PREEDIT);
    atoms->status = INTERN(_XIMP_STATUS);
    atoms->preedit_font = INTERN(_XIMP_PREEDITFONT);
    atoms->status_font = INTERN(_XIMP_STATUSFONT);
    atoms->extensions = INTERN(_XIMP_EXTENSIONS);

    atoms->ext_statuswin = INTERN("_XIMP_EXT_XIMP_STATUSWINDOW");
    atoms->ext_backfront = INTERN("_XIMP_EXT_XIMP_BACK_FRONT");
    atoms->ext_conversion = INTERN("_XIMP_EXT_XIMP_CONVERSION");

    atoms->preedit_draw_data = INTERN(_XIMP_PREEDIT_DRAW_DATA);
    atoms->feedbacks = INTERN(_XIMP_FEEDBACKS);
#undef	INTERN
    /* count supported extensions */
    ext_number = 0;
    if (core->ext_flag & EXT_STATUS)
      extensions[ext_number++] = atoms->ext_statuswin;
    if (core->ext_flag & EXT_BACKFRONT)
      extensions[ext_number++] = atoms->ext_backfront;
    if (core->ext_flag & EXT_CONV)
      extensions[ext_number++] = atoms->ext_conversion;

    /* start keys */
    start_keys.count_keys = core->start_keys.count_keys;
    start_keys.keys_list = (Ximp_Key*)Xmalloc(sizeof(Ximp_Key) *
					      core->start_keys.count_keys);
    for (i = 0; i < (int)core->stop_keys.count_keys; i++) {
	start_keys.keys_list[i].modifier =
	  core->start_keys.keylist[i].modifier;
	start_keys.keys_list[i].modifier_mask =
	  core->start_keys.keylist[i].modifier_mask;
	start_keys.keys_list[i].keysym =
	  core->start_keys.keylist[i].keysym;
    }
    /* stop keys */
    stop_keys.count_keys = core->stop_keys.count_keys;
    stop_keys.keys_list = (Ximp_Key*)Xmalloc(sizeof(Ximp_Key) *
					     core->stop_keys.count_keys);
    for (i = 0; i < (int)core->stop_keys.count_keys; i++) {
	stop_keys.keys_list[i].modifier =
	  core->stop_keys.keylist[i].modifier;
	stop_keys.keys_list[i].modifier_mask =
	  core->stop_keys.keylist[i].modifier_mask;
	stop_keys.keys_list[i].keysym =
	  core->stop_keys.keylist[i].keysym;
    }

#define SETPROPERTY(p, t, f, d, n) XChangeProperty(display, im_win, p, t, f, PropModeReplace, (unsigned char *)d, n)
    
    SETPROPERTY(atoms->version, XA_STRING, 8,
		core->ximp_version, strlen(core->ximp_version));
    SETPROPERTY(atoms->supported_styles, atoms->supported_styles, 32,
		core->styles.supported_styles,
		core->styles.count_styles);
    SETPROPERTY(atoms->supported_types, atoms->supported_types, 32,
		core->ximp_type.types,
		core->ximp_type.num_of_types);
    SETPROPERTY(atoms->server_name, XA_STRING, 8,
		core->im_name, strlen(core->im_name));
    SETPROPERTY(atoms->server_version, XA_STRING, 8,
		core->im_version, strlen(core->im_version));
    SETPROPERTY(atoms->vendor_name, XA_STRING, 8,
		core->vendor, strlen(core->vendor));
    SETPROPERTY(atoms->keys, atoms->keys, 32,
		start_keys.keys_list,
		start_keys.count_keys * 3);
    SETPROPERTY(atoms->sproc_started_keys, atoms->sproc_started_keys, 32,
		start_keys.keys_list,
		start_keys.count_keys * 3);
    SETPROPERTY(atoms->sproc_stopped_keys, atoms->sproc_stopped_keys, 32,
		stop_keys.keys_list,
		stop_keys.count_keys * 3);
    SETPROPERTY(atoms->extensions, atoms->extensions, 32,
		extensions, ext_number);
#undef SETPROPERTY
    XFree((char *)start_keys.keys_list);
    XFree((char *)stop_keys.keys_list);
}

static Bool
#if NeedFunctionPrototypes
SetupConnection(XIMPCore core)
#else
SetupConnection(core)
XIMPCore core;
#endif
{
    char *locale_list = core->im_locale;
    Bool found;
    char *p, im_name[64], im_longname[64];
    Atom selection;

    (void)SetPropInAtom(core);

    found = False;
    for (p = NextLocaleName((char **)&locale_list); p != NULL;
	 p = NextLocaleName((char **)&locale_list)) {
	/* short selection name */
	sprintf(im_name, "_XIMP_%s", p);
	if (SetSelectionOwner(core, im_name, &selection) == True) {
	    found = True;
	}
	/* long selection name */
	sprintf(im_longname, "%s@%s.%d",
		im_name, core->im_name, core->screen);
	if (SetSelectionOwner(core, im_longname, &selection) == True) {
	    found = True;
	}
    }
    return found;
}

static Status
#if NeedFunctionPrototypes
ximp_openIM(XIMS xims)
#else
ximp_openIM(xims)
XIMS xims;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    Window win = core->im_window;
    extern Bool _XimpWaitIMProtocol(
#if NeedFunctionPrototypes
    Display*, Window, XEvent*, XPointer
#endif
				    );

    /* Check if all mandatory IM values is registered or not */
    if (core->ximp_version == (Atom)NULL ||
       core->styles.count_styles == 0) {
	return False;
    }
    if (!SetupConnection(core)) {
	/* free other allocated data here */
	XFree(core);
	return False;
    }
    core->ximp_request = XInternAtom(core->display, _XIMP_PROTOCOL, False);
    _XRegisterFilterByType(display, win,
			   ClientMessage, ClientMessage,
			   _XimpWaitIMProtocol, xims);
    return True;
}

static char *
#if NeedFunctionPrototypes
ximp_setIMValues(XIMS xims, XIMArg *args)
#else
ximp_setIMValues(xims, args)
XIMS xims;
XIMArg *args;
#endif
{
    XIMPCore core = xims->protocol;
    char *ret;

    if ((ret = ParseArgs(core, XIMP_SET, args)) != NULL) {
	return ret;
    }
    return NULL;
}

static char *
#if NeedFunctionPrototypes
ximp_getIMValues(XIMS xims, XIMArg *args)
#else
ximp_getIMValues(xims, args)
XIMS xims;
XIMArg *args;
#endif
{
    XIMPCore core = xims->protocol;
    char *ret;

    if ((ret = ParseArgs(core, XIMP_GET, args)) != NULL) {
	return ret;
    }
    return NULL;
}

static Status
#if NeedFunctionPrototypes
ximp_forwardEvent(XIMS xims, IMPProtocol *improto)
#else
ximp_forwardEvent(xims, improto)
XIMS xims;
IMPProtocol *improto;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPKeyEventStruct *keyevent =
      (XIMPKeyEventStruct *)&improto->keyevent;
    XimpClient *client = _XimpFindClient(core, keyevent->icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;

    switch (improto->type) {
      case XIMP_KEYPRESS4:
      case XIMP_KEYPRESS3:
	_XimpSendIMProtocol(core, client,
			    (IS_VERSION_40(client) ?
			     XIMP_KEYPRESS4 : XIMP_KEYPRESS3),
			    keyevent->icid,
			    keyevent->keycode,
			    keyevent->state,
			    (IS_VERSION_40(client) ?
			     keyevent->time : 0));
	if (IS_VERSION_40(client)) {
	    _XimpSetMatch(client,
			  keyevent->icid, keyevent->keycode, keyevent->state);
	}
	break;
      case XIMP_KEYRELEASE4:
	if (_XimpProcessMatch(client, keyevent->icid,
			      keyevent->keycode, keyevent->state)) {
	    _XimpSendIMProtocol(core, client,
				XIMP_KEYRELEASE4,
				keyevent->icid,
				keyevent->keycode,
				keyevent->state,
				keyevent->time);
	}
	break;
    }
    return True;
}

static Status
#if NeedFunctionPrototypes
ximp_commitString(XIMS xims, IMPProtocol *improto)
#else
ximp_commitString(xims, improto)
XIMS xims;
IMPProtocol *improto;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    int	length = strlen(improto->commitstring.ctext);

    if (length > 20) {
	_XimpSendByProperty(core,
			    improto->commitstring.icid,
			    improto->commitstring.ctext,
			    length);
    } else {
	_XimpSendByClientMessage(core,
				 improto->commitstring.icid,
				 improto->commitstring.ctext,
				 length);
    }
    return True;
}

static Status
#if NeedFunctionPrototypes
ximp_callCallback(XIMS xims, IMPProtocol *improto)
#else
ximp_callCallback(xims, improto)
XIMS xims;
IMPProtocol *improto;
#endif
{
    switch (improto->type) {
      case XIMP_GEOMETRY4:
      case XIMP_GEOMETRY3:
	return _XimpGeometryCallback(xims, improto);
      case XIMP_PREEDITSTART4:
      case XIMP_PREEDITSTART3:
	return _XimpPreeditStartCallback(xims, improto);
      case XIMP_PREEDITDONE4:
      case XIMP_PREEDITDONE3:
	return _XimpPreeditDoneCallback(xims, improto);
      case XIMP_PREEDITDRAW4:
      case XIMP_PREEDITDRAW3:
	return _XimpPreeditDrawCallback(xims, improto);
      case XIMP_PREEDITDRAW_CM4:
      case XIMP_PREEDITDRAW_CM3:
	return _XimpPreeditDrawCMCallback(xims, improto);
      case XIMP_PREEDITCARET4:
      case XIMP_PREEDITCARET3:
	return _XimpPreeditCaretCallback(xims, improto);
      case XIMP_STATUSSTART4:
      case XIMP_STATUSSTART3:
	return _XimpStatusStartCallback(xims, improto);
      case XIMP_STATUSDONE4:
      case XIMP_STATUSDONE3:
	return _XimpStatusDoneCallback(xims, improto);
      case XIMP_STATUSDRAW4:
      case XIMP_STATUSDRAW3:
	return _XimpStatusDrawCallback(xims, improto);
      case XIMP_STATUSDRAW_CM4:
      case XIMP_STATUSDRAW_CM3:
	return _XimpStatusDrawCMCallback(xims, improto);
      default:
	return False;
    }
}

static Status
#if NeedFunctionPrototypes
ximp_preeditStart(XIMS xims, IMPProtocol *improto)
#else
ximp_preeditStart(xims, improto)
XIMS xims;
IMPProtocol *improto;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XimpClient *client = _XimpFindClient(core, improto->any.icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;
    if (client->is_conv_on == True) return True; /* just ignore */

    if (IS_VERSION_40(client)) {
	if (client->ev_flow_type == XIMP_FE_TYPE1) {
	    _XimpRegisterKeyPressFilter(xims, client);
	    _XimpRegisterKeyReleaseFilter(xims, client);
	}
    } else {
	if (client->ev_flow_type == XIMP_FRONTEND) {
	    _XimpRegisterKeyPressFilter(xims, client);
	}
    }
    client->is_conv_on = True;
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_SPROC_STARTED4 :
			 XIMP_PROCESS_BEGIN3),
			improto->any.icid,
			0, 0, 0);
    return True;
}

static Status
#if NeedFunctionPrototypes
ximp_preeditEnd(XIMS xims, IMPProtocol *improto)
#else
ximp_preeditEnd(xims, improto)
XIMS xims;
IMPProtocol *improto;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XimpClient *client = _XimpFindClient(core, improto->any.icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;
    if (client->is_conv_on == False) return True; /* just ignore */

    _XimpUnregisterKeyPressFilter(xims, client);
    _XimpUnregisterKeyReleaseFilter(xims, client);
    client->is_conv_on = False;
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_SPROC_STOPPED4 :
			 XIMP_PROCESS_END3),
			improto->any.icid,
			0, 0, 0);
    return True;
}

static void
#if NeedFunctionPrototypes
FreeIMValues(XIMPCore core)
#else
FreeIMValues(core)
XIMPCore core;
#endif
{
    if (core->im_locale != (char*)NULL) {
	XFree(core->im_locale);
    }
    if (core->styles.supported_styles != (XIMStyle*)NULL) {
	XFree(core->styles.supported_styles);
    }
    if (core->start_keys.keylist != (XIMTriggerKey*)NULL) {
	XFree(core->start_keys.keylist);
    }
    if (core->stop_keys.keylist != (XIMTriggerKey*)NULL) {
	XFree(core->stop_keys.keylist);
    }
    if (core->ximp_type.types != (unsigned long *)NULL) {
	XFree(core->ximp_type.types);
    }
    if (core->im_name != (char*)NULL) {
	XFree(core->im_name);
    }
    if (core->ximp_version != (char*)NULL) {
	XFree(core->ximp_version);
    }
    if (core->im_version != (char*)NULL) {
	XFree(core->im_version);
    }
    if (core->vendor != (char*)NULL) {
	XFree(core->vendor);
    }
}

static Status
#if NeedFunctionPrototypes
ximp_closeIM(XIMS xims)
#else
ximp_closeIM(xims)
XIMS xims;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    Window win = core->im_window;
    extern Bool _XimpWaitIMProtocol(
#if NeedFunctionPrototypes
    Display*, Window, XEvent*, XPointer
#endif
				    );

    _XUnregisterFilter(display, win,
		       _XimpWaitIMProtocol, core);
    FreeIMValues(core);
    XFree(core);
    return True;
}
#endif /* _XIMPMETHODS_C_ */
