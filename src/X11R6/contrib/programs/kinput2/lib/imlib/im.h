/* $Id: im.h,v 1.6 1994/06/01 06:08:30 ishisone Exp $ */
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

#ifndef _im_h
#define _im_h

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>
#include <X11/Xfuncs.h>

/* macros defined in the system header file cause problem... */
#undef major
#undef minor

#define EXT_DEBUG_VAR	debug_IMProtocol
#include "DebugPrint.h"
#include "AsyncErr.h"
#include "FontBank.h"
#include "ConvCtrl.h"

#include "imprtype.h"
#include "imbuf.h"
#include "imxport.h"
#include "imconst.h"

/*
 * Restrictions of attributes
 */

#define MIN_LINE_SPACING	2
#define MIN_AREA_WIDTH		16
#define MIN_AREA_HEIGHT		10

/*
 * IMDispatcher -- request dispatcher
 */

typedef int (*IMDispatcher)();


/*
 * IMConnection
 */

typedef struct _im_connection_ {
    /* protocol information */
    int major_protocol_version;
    int minor_protocol_version;
#ifdef XIM_BC
    int has_length_bug;
#endif
    int serial;				/* connection serial # (for debug) */
    Widget proto_widget;		/* protocol handler widget */

    /* transport information */
    IMTransport transport;

    int byte_order;			/* byte order of the client */
#define ORDER_UNKNOWN	0		/* order is yet unknown */
#define ORDER_BIG	1		/* big endian */
#define ORDER_LITTLE	2		/* little endian */

    /* communication buffer */
    IMBuffer in_buf;			/* input buffer */
    IMBuffer out_buf;			/* output buffer */
#define IM_INBUF(conn)	(&(conn)->in_buf)
#define IM_OUTBUF(conn)	(&(conn)->out_buf)

    IMDispatcher dispatcher;		/* request dispatch procedure */

    struct _im_im_ *im_list;		/* list of IMs on this connection */

#ifdef notyet
    /* authentication information */
    IMAuth *server_auth;		/* server authentication data */
    IMAuth *client_auth;		/* client authentication data */
#endif /* notyet */

    /* scheduler */
    int schedule;			/* type of operation to be performed */
#define SCHED_WRITE	1		/* flush output buffer */
#define SCHED_CLOSE	2		/* close down connection gently */
#define SCHED_SHUTDOWN	4		/* force shut down */
    struct _im_connection_ *queue_next;	/* output/close/shutdown queue */

    struct _im_connection_ *next;
} IMConnection;


/*
 * IMConverter -- information on the conversion engine
 */
typedef struct {
    char **supported_locales;		/* list of supported locales */
    int num_locales;
    WidgetClass input_object_class;
    WidgetClass display_object_class;
} IMConverter;


/*
 * IMTriggerKey -- conversion trigger key specification
 */
typedef struct {
    KeySym keysym;
    long modifiers;
    long check_modifiers;
} IMTriggerKey;


/*
 * IMExtensionMask -- bit mask of enabled extensions
 */
typedef unsigned long IMExtensionMask;	/* holds up to 32 extensions */


/*
 * IMIM -- per-IM structure
 */

typedef struct _im_im_ {
    unsigned int id;			/* input-method ID */
    IMConnection *connection;		/* connection this IM is on */
    struct _im_im_ *next;
    struct _im_im_ *hash_next;		/* for hash table */
    IMConverter *converter;		/* conversion engine */
    IMExtensionMask mask;		/* enabled extensions */
    struct _im_ic_ *ic_list;		/* list of all ICs belonging to this IM */
} IMIM;


/*
 * IMAttributes and IMPSAttributes -- IC attributes structure
 */

typedef struct {
    unsigned long set_mask;		/* shows attrs that have been set */
    unsigned long change_mask;		/* shows attrs that have changed */
#define ATTR_MASK_INPUT_STYLE	(1<<0)
#define ATTR_MASK_CLIENT	(1<<1)
#define ATTR_MASK_FOCUS		(1<<2)
    XIMStyle input_style;
    Window client;			/* client window */
    Window focus;			/* focus window */
} IMCommonAttributes;

typedef struct {
    unsigned long set_mask;		/* shows attrs that have been set */
    unsigned long change_mask;		/* shows attrs that have changed */
#define ATTR_MASK_AREA		(1<<0)
#define ATTR_MASK_FOREGROUND	(1<<1)
#define ATTR_MASK_BACKGROUND	(1<<2)
#define ATTR_MASK_COLORMAP	(1<<3)
#define ATTR_MASK_STD_COLORMAP	(1<<4)
#define ATTR_MASK_BG_PIXMAP	(1<<5)
#define ATTR_MASK_LINESPACE	(1<<6)
#define ATTR_MASK_CURSOR	(1<<7)
#define ATTR_MASK_AREA_NEEDED	(1<<8)
#define ATTR_MASK_FONT_SET	(1<<9)
#define ATTR_MASK_SPOT_LOCATION	(1<<10)
    XRectangle area;
    Pixel foreground, background;
    Colormap colormap;
    Atom std_colormap;
    Pixmap bg_pixmap;
    Position line_space;
    Cursor cursor;
    XRectangle area_needed;		/* width/height member are unused */
    String font_set;
    XPoint spot_location;		/* not used by status attr. */
} IMPSAttributes;


/*
 * IMPendingEvent -- record of delayed processing event
 */

typedef struct _im_pending_event_ {
    struct _im_ic_ *ic;
    int synchronous;
    XEvent event;
    struct _im_pending_event_ *next;
} IMPendingEvent;


/*
 * IMWindowProfile -- information about window
 */

typedef struct {
    Dimension width;
    Dimension height;
    Window root;
} IMWindowProfile;


/*
 * IMIC -- per-IC structure
 */

typedef struct _im_ic_ {
    unsigned int id;			/* input-context ID */
    IMIM *im;				/* IM this IC belongs to */
    Widget conversion;			/* conversion widget attached
					 * to this IC */
    struct _im_ic_ *next;
    struct _im_ic_ *hash_next;		/* for hash list */
    int state;				/* current IC state */
#define IC_SYNCING	1		/* waiting for SYNC_REPLY message */
#define IC_CONVERTING	2		/* in conversion mode */
    IMPendingEvent *pending_events;	/* pending events */
    int style;				/* input style */
#define IMSTYLE_SEPARATE	0	/* root window style */
#define IMSTYLE_OVER_THE_SPOT	1	/* over-the-spot style */
#define IMSTYLE_OFF_THE_SPOT	2	/* off-the-spot style */
    IMCommonAttributes common_attr;	/* general attributes */
    IMPSAttributes preedit_attr;	/* preedit specific attributes */
    IMPSAttributes status_attr;		/* status specific attributes */
    IMWindowProfile client_profile;	/* client widow profile */
    IMWindowProfile focus_profile;	/* focus window profile */
    XFontStruct **fonts;
    int num_fonts;
} IMIC;


/*
 * IM and IC hash table size
 */

#define IM_HASH_SIZE	32
#define IC_HASH_SIZE	128


/*
 * Public function declarations
 */

#include "imfuncs.h"

#endif /* _im_h */
