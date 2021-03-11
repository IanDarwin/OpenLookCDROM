/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpData.h	1.7 94/02/16 */
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

#ifndef _XIMPDATA_H_
#define _XIMPDATA_H_
#include <X11/Xlib.h>
#define XIMP_40
#include "XIMProto.h"
#include "IMdkit.h"

#define IS_VERSION_40(client) \
		(((XimpClient *)client)->proto_version == XIMP_VERSION_40)

/* XIMP Valid Attribute Name Definition */
#define XIMPVersion		"ximpVersion"
#define XIMPType		"ximpType"
#define XIMPServerVersion	"ximpServerVersion"
#define XIMPVendorName		"ximpVendorName"

#define	XIMPExtension		"ximpExtension"
#define	XIMPExtStatusWin	"ximpExtStatusWin"
#define	XIMPExtBackFront	"ximpExtBackFront"
#define	XIMPExtConversion	"ximpExtConversion"

#define XIMP_OPEN	0
#define XIMP_SET	1
#define XIMP_GET	2
/*
 * Used in supporting extensions
 */
#define EXT_STATUS	(1L<<0)
#define EXT_BACKFRONT	(1L<<1)
#define EXT_CONV	(1L<<2)

/*
 * Active filters on focus window
 */
#define FLT_NONE	(0L)
#define FLT_KEYPRESS	(1L<<0)
#define FLT_KEYRELEASE	(1L<<1)
#define FLT_DESTROY	(1L<<2)

typedef enum {
    XIMP_VERSION_35,
    XIMP_VERSION_40
} XimpProtocolVersion;

#define LONG_ENOUGH 250

typedef enum {
    RESET_NOTHING = 0,
    RESET_BY_CMSG,
    RESET_BY_PROP
} XimpResetFlag;

/*
 * Data interchange structures
 */

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
} XIMPAnyStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    CARD32	keycode;
    CARD32	state;
    CARD32	time;
} XIMPKeyEventStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    CARD32	input_style;
    CARD32	attr_mask;
    Ximp_PreeditPropRec4 *pre_values;
    Ximp_StatusPropRec4  *sts_values;
    char	*pre_font;
    char	*sts_font;
} XIMPICValuesStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    Window	new_client_win;
    long	new_fwin_mask;
} XIMPClientWindowStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    Window	new_focus_win;
    long	new_fwin_mask;
} XIMPFocusWindowStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    INT32	x;
    INT32	y;
} XIMPMoveStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    long	new_fwin_sel_mask;
} XIMPEventMaskNotifyStruct;

typedef struct {
    INT32 operation;
    INT32 mode;
} XimpExtConv;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    long	ext_type;
    union {
	unsigned long back_front; /* for _XIMP_EXT_BACK_FRONT */
	XimpExtConv conversion;	/* for _XIMP_EXT_CONVERSION */
    } todo;
} XIMPExtensionStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    Atom	stored;
} XIMPReadPropStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    char	*ctext;
} XIMPResetStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    char	*ctext;
} XIMPCommitStringStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    INT32	err_req;
    INT32	err_detail;
    INT32	extended;
} XIMPErrorStruct;

/* Callbacks */
typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    union {
	int return_value;			/* PreeditStart */
	XIMPreeditDrawCallbackStruct draw;	/* PreeditDraw */
	XIMPreeditCaretCallbackStruct caret; 	/* PreeditCaret */
    } todo;
} XIMPPreeditCBStruct;

typedef struct {
    INT32	type;
    CARD32	icid;
    Window	focus_win;
    long	fwin_sel_mask;
    CARD32	ximp_type_mask;
    Window	client_win;
    union {
	XIMStatusDrawCallbackStruct draw;	/* StatusDraw */
    } todo;
} XIMPStatusCBStruct;

typedef union _IMPProtocol {
    int				type;
    XIMPAnyStruct		any;
    XIMPKeyEventStruct		keyevent;
    XIMPICValuesStruct		create;
    XIMPICValuesStruct		setvalue;
    XIMPICValuesStruct		getvalue;
    XIMPAnyStruct		destroy;
    XIMPAnyStruct		regkey;
    XIMPAnyStruct		setfocus;
    XIMPAnyStruct		unsetfocus;
    XIMPClientWindowStruct	clientwin;
    XIMPFocusWindowStruct	focuswin;
    XIMPMoveStruct		move;
    XIMPEventMaskNotifyStruct	evmasknotify;
    XIMPExtensionStruct		extension;
    XIMPReadPropStruct		readprop;
    XIMPResetStruct		reset;
    XIMPCommitStringStruct	commitstring;
    XIMPErrorStruct		error;
    XIMPAnyStruct		geometry_cb;
    XIMPPreeditCBStruct		preedit_cb;
    XIMPStatusCBStruct		status_cb;
    long			pad[24];
} IMPProtocol;

typedef int (*IMProtoHandler)(
#if NeedFunctionPrototypes
    XIMS, IMPProtocol*
#endif
			      );

typedef struct _XimpClient {
    CARD32 id;		/* client ID */
    Window focus_window;
    Window client_window;
    long ev_flow_type;		/* event flow type */
    XimpProtocolVersion proto_version;	/* protocol version: 3.5 or 4.0 */
    Bool is_conv_on;	/* holds current conversion state */
    long ev_masks;	/* holds client's interest masks */
    CARD32 og_code;	/* holds keycode to keep matching */
    CARD32 og_state;	/* holds keystate to keep matching */
    long filters;
    struct _XimpClient *next;
} XimpClient;

typedef struct _XIMPTypeRec {
    int	num_of_types;
    unsigned long *types;
} XIMPTypeRec;

typedef struct _XIMPAtoms {
    Atom selection_owner;
    Atom selection_owner2;
    Atom ctext_type;
    Atom version;
    Atom supported_styles;
    Atom supported_types;
    Atom server_name;
    Atom server_version;
    Atom vendor_name;
    Atom keys;
    Atom sproc_started_keys;
    Atom sproc_stopped_keys;
    Atom focus;
    Atom preedit;
    Atom status;
    Atom preedit_font;
    Atom status_font;
    /* for XIMP_EXTENSION */
    Atom extensions;
    Atom ext_statuswin;
    Atom ext_backfront;
    Atom ext_conversion;
    /* for Callbacks */
    Atom preedit_draw_data;
    Atom feedbacks;
} XIMPAtoms;

/* XimpCoreRec structure */
typedef struct _XIMPCoreRec {
    Display	*display;
    int		screen;
    Atom	ximp_request;	/* Ximp message type */
    /* common IM Values */
    long	imvalue_mask;
    Window	im_window;	/* IMServerWindow */
    char	*im_name;	/* IMServerName */
    char	*im_locale;	/* IMLocale */
    XIMStyles	styles;		/* IMInputStyles */
    XIMTriggerKeys start_keys;	/* IMOnKeysList */
    XIMTriggerKeys stop_keys;	/* IMOffKeysList */
    IMProtoHandler improto;	/* IMProtoHandler */
    /* XIMProtocol Dependent IM Values */
    char	*ximp_version;	/* XIMPVersion */
    XIMPTypeRec	ximp_type;	/* XIMPType */
    char	*im_version;	/* XIMPServerVersion */
    char	*vendor;	/* XIMPVendorName */
    /* Atoms */
    XIMPAtoms   atoms;
    /* XIMPExtension */
    CARD32	ext_flag;	/* flag for supported_extensions */
    long	ext_backfront;	/* XIMPExtBackFront value */
    /* clients table */
    XimpClient *clients;
    XimpClient *free_clients;
} XIMPCoreRec, *XIMPCore;
#endif /* _XIMPDATA_H_ */
