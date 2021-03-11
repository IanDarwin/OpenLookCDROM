/* $Id: KIProtoP.h,v 1.12 1993/09/16 02:45:33 ishisone Rel $ */
/*
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

#ifndef _KinputProtocolP_h
#define _KinputProtocolP_h

#include "KIProto.h"
#include "ConvCtrl.h"

typedef struct {
    int empty;
} KinputProtocolClassPart;

typedef struct _KinputProtocolClassRec {
    CoreClassPart		core_class;
    KinputProtocolClassPart	kinputprotocol_class;
} KinputProtocolClassRec;

extern KinputProtocolClassRec kinputProtocolClassRec;

typedef enum {
    unresolved_protocol,
    kinput1_protocol,		/* old kinput protocol */
    kinput2_protocol,		/* new kinput protocol */
    jinput_protocol,		/* Matsushita's jinput protocol */
    xlc_protocol		/* Sony's xlc protocol */
} KinputProtocol;

typedef enum {
    offthespot_style,
    overthespot_style,
    separate_style
} ConversionStyle;

typedef struct {
    int		state;
#define JINPUT_MULTI_COLOR	1
#define JINPUT_MULTI_FONT	2
    Position	rawspotx, rawspoty;
} JinputData;

typedef struct _convclient_ {
    KinputProtocol	protocol;
    ConversionStyle	style;
    Widget		protocolwidget;	/* protocol widget */
    Widget		conversion;	/* conversion widget */
    Window		reqwin;		/* conversion requestor window */
    Atom		selection;	/* selection atom */
    Atom		target;		/* type of converted string */
    Atom		property;	/* property to store converted string */
    EventSelectMethod	esm;
    unsigned long	attrmask;
    unsigned long	validattrmask;
    ConversionAttributes attrs;
    XtPointer		data;		/* protocol dependent data */
    void		(*start_proc)();/* protocol dependent proc. routines */
    void		(*detach_proc)();
    void		(*fix_proc)();
    void		(*end_proc)();
    void		(*free_resources)();
    struct _convclient_	*next;
} ConvClient;

typedef struct {
    /* resources */
    String language;
    WidgetClass inputObjClass;
    WidgetClass displayObjClass;
    Boolean backward_compatible;
    String xlcstartkey;
    /* private state */
    ConvClient *clients;
    Position defaultascent;
    /* atoms -- common */
    Atom convAtom;		/* _<language>_CONVERSION */
    Atom oldConvAtom;		/* <language>_CONVERSION (obsolete) */
    Atom ctextAtom;		/* COMPOUND_TEXT */
    Atom convStringAtom;	/* CONVERSION_STRING */
    Atom convNotifyAtom;	/* CONVERSION_NOTIFY */
    Atom convEndAtom;		/* CONVERSION_END */
    /* atoms -- kinput2 specific */
#ifdef notdef
    Atom convInitializeAtom;	/* CONVERSION_INITIALIZE */
#endif
    Atom convAttributeAtom;	/* CONVERSION_ATTRIBUTE */
    /* atoms -- jinput specific (whew!) */
    Atom convVersionAtom;	/* CONVERSION_VERSION */
    Atom convInitialTypeAtom;	/* CONVERSION_INITIAL_TYPE */
    Atom convOpenNotifyAtom;	/* CONVERSION_OPEN_NOTIFY */
    Atom convXYRequestAtom;	/* CONVERSION_XY_REQUEST */
    Atom convFontsRequestAtom;	/* CONVERSION_FONTS_REQUEST */
    Atom convColorRequestAtom;	/* CONVERSION_COLOR_REQUEST */
    Atom convCloseNotifyAtom;	/* CONVERSION_CLOSE_NOTIFY */
    /* atoms -- xlc specific */
    Atom xlcStatusAtom;		/* _XLC_STATUS */
    Atom xlcOnTheSpotAtom;	/* _XLC_ON_THE_SPOT */
    Atom xlcBcModifierAtom;	/* _XLC_BC_MODIFIER */
    Atom xlcBcKeycodeAtom;	/* _XLC_BC_KEYCODE */
} KinputProtocolPart;

typedef struct _KinputProtocolRec {
    CorePart		core;
    KinputProtocolPart	kinput;
} KinputProtocolRec;

#endif
