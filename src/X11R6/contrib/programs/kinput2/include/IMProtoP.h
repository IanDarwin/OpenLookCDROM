/* $Id: IMProtoP.h,v 1.5 1994/05/31 07:55:06 ishisone Rel $ */
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

#ifndef _IMProtocolP_h
#define _IMProtocolP_h

#include "IMProto.h"
#include "im.h"
#include "FontBank.h"
#include "ConvCtrl.h"

typedef struct {
    int dummy;
} IMProtocolClassPart;

typedef struct _IMProtocolClassRec {
    CoreClassPart		core_class;
    IMProtocolClassPart		imProtocol_class;
} IMProtocolClassRec;

extern IMProtocolClassRec imProtocolClassRec;


typedef struct {
    /* resources */
    String server_name;

    /*
     * It is not difficult to handle multiple languages in a
     * protocol widget, but we've determined not to do so
     * for simplicity (at least for now).
     */
    String language;
    String locales;
    WidgetClass input_object_class;
    WidgetClass display_object_class;
    String default_fontlist;		/* default font list to be used */

    Pixel foreground;			/* default fg pixel */
    String conversion_start_keys;	/* conversion start/end keys */
    Dimension status_width;		/* default width of status area */
    String transport_list;		/* transports to be used */

    /* private state */
    Boolean use_tcp_transport;
    Boolean use_unix_transport;
    Boolean use_x_transport;
    int tcp_port;			/* TCP port number */
    int tcp_sock;			/* TCP socket descriptor */
    XtInputId tcp_id;
    String unix_path;			/* Unix domain socket pathname */
    int unix_sock;			/* Unix domain socket descriptor */
    XtInputId unix_id;
    Boolean no_more_connections;
    IMConnection *connection_list;
    IMConverter converter;
    long last_imid;
    long last_icid;
    IMIM *im_hash[IM_HASH_SIZE];
    IMIC *ic_hash[IC_HASH_SIZE];
    IMConnection *scheduler_queue;
    IMTriggerKey *trigger_keys;		/* conversion start trigger keys */
    int num_trigger_keys;
    FontBank font_bank;
    XFontStruct **default_fonts;
    int num_default_fonts;

    /* atoms */
    Atom xim_xconnect;			/* "_XIM_XCONNECT" */
    Atom xim_protocol;			/* "_XIM_PROTOCOL" */
    Atom xim_moredata;			/* "_XIM_MOREDATA" */
    Atom ctext_atom;			/* "COMPOUND_TEXT" */
    Atom locales_atom;			/* "LOCALES" */
    Atom transport_atom;		/* "TRANSPORT" */
    Atom ki2comm_atom;			/* "_KINPUT2_COMM" */
    Atom server_atom;			/* atom stored in XIM_SERVERS prop. */
} IMProtocolPart;

typedef struct _IMProtocolRec {
    CorePart		core;
    IMProtocolPart	imp;
} IMProtocolRec;

#define IM_IMHASH(w)	(((IMProtocolWidget)(w))->imp.im_hash)
#define IM_ICHASH(w)	(((IMProtocolWidget)(w))->imp.ic_hash)
#define IM_LASTIMID(w)	(((IMProtocolWidget)(w))->imp.last_imid)
#define IM_LASTICID(w)	(((IMProtocolWidget)(w))->imp.last_icid)
#define IM_QUEUE(w)	(((IMProtocolWidget)(w))->imp.scheduler_queue)

#define ATOM_CTEXT(w)	(((IMProtocolWidget)(w))->imp.ctext_atom)
#define ATOM_LOCALES(w)	(((IMProtocolWidget)(w))->imp.locales_atom)
#define ATOM_TRANSPORT(w) (((IMProtocolWidget)(w))->imp.transport_atom)
#define ATOM_KI2COMM(w) (((IMProtocolWidget)(w))->imp.ki2comm_atom)
#define ATOM_XCONNECT(w) (((IMProtocolWidget)(w))->imp.xim_xconnect)
#define ATOM_PROTOCOL(w) (((IMProtocolWidget)(w))->imp.xim_protocol)
#define ATOM_MOREDATA(w) (((IMProtocolWidget)(w))->imp.xim_moredata)

#endif
