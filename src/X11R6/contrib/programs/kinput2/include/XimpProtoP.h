/* $Id: XimpProtoP.h,v 1.9 1994/05/17 02:12:42 ishisone Rel $ */
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

#ifndef _XimpProtocolP_h
#define _XimpProtocolP_h

#include "FontBank.h"
#include "XimpProto.h"
#include "ConvCtrl.h"

typedef struct {
    int ximp_dummy;
} XimpProtocolClassPart;

typedef struct _XimpProtocolClassRec {
    CoreClassPart		core_class;
    XimpProtocolClassPart	ximpprotocol_class;
} XimpProtocolClassRec;

extern XimpProtocolClassRec ximpProtocolClassRec;


typedef enum {
    offthespot_style,
    overthespot_style,
    separate_style
} ConversionStyle;

typedef struct {
    XIMStyle		ximstyle;
    ConversionStyle	cstyle;
} XimpInputStyle;

typedef struct {
    Position	areax, areay;
    Dimension	areawidth, areaheight;
    Pixel	foreground, background;
    Colormap	colormap;
    Pixmap	bgpixmap;
    Dimension	linespacing;
    Cursor	cursor;
    Dimension	neededwidth, neededheight;
    Position	spotx, spoty;
    String	fontlist;
} XimpPreEditAttributes;

typedef struct {
    Position	areax, areay;
    Dimension	areawidth, areaheight;
    Pixel	foreground, background;
    Colormap	colormap;
    Pixmap	bgpixmap;
    Dimension	linespacing;
    Cursor	cursor;
    Dimension	neededwidth, neededheight;
    Window	statuswin;
    String	fontlist;
} XimpStatusAttributes;

typedef struct _convclient_ {
    unsigned long	id;		/* ICID */
    String		version;	/* client version string */
    ConversionStyle	style;
    Widget		protocolwidget;	/* protocol widget */
    Widget		conversion;	/* conversion widget */
    Window		reqwin;		/* conversion requestor window */
    Dimension		reqwinwidth;
    Dimension		reqwinheight;
    Window		focuswin;	/* focus window */
    Dimension		focuswidth;
    Dimension		focusheight;
    Atom		property;	/* property to store converted string*/
    XimpPreEditAttributes	xpattrs; /* to keep specified values     */
    XimpStatusAttributes	xsattrs; /*   for GETVALUE request       */
    unsigned long	xattrmask;
    Boolean		defaultsfilledin; /* if def.values are filled in */
    EventSelectMethod	esm;
    XFontStruct		**fonts;
    int			num_fonts;
    Boolean		resetting;	/* now resetting */
    XClientMessageEvent	*event;		/* current processing event */
    struct _convclient_	*next;
} ConvClient;

typedef struct _ximpfcache_ {
    int			sum;
    String		namelist;
    XFontStruct 	**fonts;
    Cardinal		num_fonts;
    struct _ximpfcache_	*next;
} XimpFontListCache;


typedef struct {
    /* resources */
    String localename;
    String servername;
    WidgetClass inputObjClass;
    WidgetClass displayObjClass;
    Boolean forceDefaultServer;
    String defaultfontlist;		/* default font list to be used */
    Pixel foreground;			/* default fg pixel */
    String convkeys;			/* conversion start/end keys */
    Dimension statuswidth;		/* default width of status area */
    /* private state */
    FontBank fontbank;
    XFontStruct **deffonts;
    int numdeffonts;
    ConvClient *clients;
    ConvClient *freeclients;	/* free list */
    unsigned long icid;		/* next ICID */
    int propid;
    /* atoms */
    Atom selAtom1;		/* _XIMP_<localeName> */
    Atom selAtom2;		/* _XIMP_<localeName>@<serverName>.<screen> */
    Atom ctextAtom;		/* COMPOUND_TEXT */
    Atom ximpVersionAtom;	/* _XIMP_VERSION */
    Atom ximpStyleAtom;		/* _XIMP_STYLE */
    Atom ximpKeysAtom;		/* _XIMP_KEYS */
    Atom ximpServerNameAtom;	/* _XIMP_SERVERNAME */
    Atom ximpServerVersionAtom;	/* _XIMP_SERVERVERSION */
    Atom ximpVendorNameAtom;	/* _XIMP_VENDORNAME */
    Atom ximpExtensionsAtom;	/* _XIMP_EXTENSIONS */
    Atom ximpProtocolAtom;	/* _XIMP_PROTOCOL */
    Atom ximpFocusAtom;		/* _XIMP_FOCUS */
    Atom ximpPreeditAtom;	/* _XIMP_PREEDIT */
    Atom ximpStatusAtom;	/* _XIMP_STATUS */
    Atom ximpPreeditFontAtom;	/* _XIMP_PREEDITFONT */
    Atom ximpStatusFontAtom;	/* _XIMP_STATUSFONT */
    Atom ximpExtXimpBackFrontAtom; /* _XIMP_EXT_XIMP_BACK_FRONT */
} XimpProtocolPart;

typedef struct _XimpProtocolRec {
    CorePart		core;
    XimpProtocolPart	ximp;
} XimpProtocolRec;

#endif
