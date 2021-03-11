/* $XimpImplementGroup: XimpIm.h, v 1.1 94/05/31 21:16:06 $ */
/******************************************************************

              Copyright 1991, 1992 by FUJITSU LIMITED
	      Copyright 1991, 1992 by Sun Microsystems, Inc.
              Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED,
Sun Microsystems, Inc. and Sony Corporation not be used in advertising 
or publicity pertaining to distribution of the software without specific,
written prior permission.
FUJITSU LIMITED, Sun Microsystems, Inc. and Sony Corporation make no 
representations about the suitability of this software for any purpose.
It is provided "as is" without express or implied warranty.

FUJITSU LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION DISCLAIM 
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU
LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
          Hiromu Inukai        Sun Microsystems, Inc.
          Hideki Hiura         Sun Microsystems, Inc.
	  Makoto Wakamatsu     Sony Corporation

******************************************************************/
/*

Copyright (c) 1991 - 1994  FUJITSU LIMITED
Copyright (c) 1991 - 1994  Sony Corporation

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED AND SONY CORPORATION BE LIABLE FOR
ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
Sony Corporation shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and Sony Corporation.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
	  Makoto Wakamatsu     Sony Corporation

*/

#define XIMP_40
#include "XIMProto.h"

#include "XlcGeneric.h"

#define XIMP_NAME	256

#define XIMP_CREATE_IC	0
#define	XIMP_SET_IC	1
#define	XIMP_START_IC	2

#define FILTERD		True
#define NOTFILTERD	False

#define XIMP_MAXBUF 1024
#define CT_MAX_IN_CM 15

#define COPYMASK(src, dist) {\
	(dist).proto3 = (src).proto3;\
	(dist).proto4 = (src).proto4;\
}

/*
 * XIMP Extension IM data struction 
 */

typedef struct {
/* Selecting the Frontend method or Backend method */
    int		extension_back_front_exist;
    Atom	extension_back_front_id;
/* Handling/Querying IM Server's conversion mode */
    Bool	extension_conversion_exist;
    Atom	extension_conversion_id;
    Bool	extension_conversion;
/* Status Window ID Extension */
    int		extension_statuswindow_exist;
    Atom	extension_statuswindow_id;
/* Candidates List Displaying Extention */
    int		extension_lookup_exist;
    Atom	extension_lookup_id;
    Atom	extension_lookup_start;
    Atom	extension_lookup_start_rep;
    Atom	extension_lookup_draw;
    Atom	extension_lookup_proc;
    Atom	extension_lookup_proc_rep;
/* Control the input mode depending on the native language */
/* - Setting method */
    int		extension_inputmode_exist;
    Atom	extension_inputmode_id;
    int		extension_inputmode_rtn_exist;
    Atom	extension_inputmode_rtn_id;
/* - Locking method */
    int		extension_modelock_exist;
    Atom	extension_modelock_id;
/* - Reporting method */
    int		extension_modechange_exist;
    Atom	extension_modechange_id;
/* Report the keyBinding change */
    int		extension_keychange_exist;
    Atom	extension_keychange_id;
    Atom	off_keys_id;
/* Flushing Preedit buffer */
    int		extension_flush_exist;
    Atom	extension_flush_id;
/* Add XIMP Extension data */
} Ximp_ExtXIMRec;

/*
 * IM dependent data struction 
 */

typedef struct _Ximp_XIM	*Ximp_XIM;

typedef struct {
    int			 reconnection_mode;
    Bool		 is_connected;
    char		*im_name;
    int			 def_svr_mode;
    Ximp_KeyList	*process_start_keys;
    Bool		 use_wchar;
    XIMStyles		*delaybind_styles;
/* for IM Server data */
    Window		 fe_window;
    Window		 owner;
    Atom		 improtocol_id;
    Atom		 version_id;
    Atom		 style_id;
    Atom		 keys_id;
    Atom		 servername_id;
    Atom		 serverversion_id;
    Atom		 vendorname_id;
    Atom		 extension_id;
    Atom		 ctext_id;
    Atom		 focus_win_id;
    Atom		 preedit_atr_id;
    Atom		 status_atr_id;
    Atom		 preeditfont_id;
    Atom		 statusfont_id;
    Atom		 preeditmaxsize_id;
    Atom		 type_id;
    long		*type_list;
    char		*im_proto_vl;
    int			 im_proto_vnum;
    XIMStyles		*im_styles;
    Ximp_KeyList	*im_keyslist;
    Ximp_KeyList	*im_offkeyslist;
    char		*im_server_name;
    char		*im_server_vl;
    char		*im_vendor_name;
    Atom		*im_ext_list;
/* for Force Select KeyRelease */
    Bool		is_forceselectkeyrelease;
/* for CT => MB,WC converter */
    XlcConv		 ctom_conv;
    XlcConv		 ctow_conv;
/* for Extension */
    Ximp_ExtXIMRec	*imtype;
} XIMXimpRec;

/*
 * IM data struction 
 */

typedef struct _Ximp_XIM {
    XIMMethods		 methods;
    XIMCoreRec		 core;
    XIMXimpRec		*ximp_impart;
} Ximp_XIMRec;

/*
 * Candidates List Displaying Extention
 * ( Lookup Callback )
 */

typedef struct {
    XIMCallback     start;
    XIMCallback     done;
    XIMCallback     draw;
    XIMCallback     proc;
} ICExtLookupCallbacks;

/*
 * data block describing the visual attributes associated with an input
 * context
 */

typedef struct {
    XRectangle      area;
    XRectangle      area_needed;
    XPoint          spot_location;
    Colormap        colormap;
    Atom            std_colormap;
    unsigned long   foreground;
    unsigned long   background;
    Pixmap          background_pixmap;
    XFontSet        fontset;
    int             line_space;
    Cursor          cursor;
    XPointer        draw_data;
    ICExtLookupCallbacks callbacks;
} ICExtLookupAttributes;

typedef int Ximp_CBStatus;

#define XIMPCBPREEDITACTIVE    0x00000001
#define XIMPCBSTATUSACTIVE     0x00000002

#define DEFAULTCBSTATUSSTRING "Disconnected"

typedef enum _Ximp_inputmode {
    BEING_BYPASSED  = 0,
    BEING_PREEDITED = 1
} input_mode_t ;

typedef struct {
    int		proto3;
    int		proto4;
} XimpChangeMaskRec, *XimpChangeaMask;

/*
 * XIMP Extension IC data struction 
 */

typedef struct {
/* Extended Callback attribute */
/* - Error callback */
    XIMCallback			 error;
/* - Lookup choices callback */
    Bool			 use_lookup_choices;
    ICExtLookupAttributes	 lookup_attr;
/* - restart callback */
    XIMCallback			 restart;
/* - destroy callback */
    XIMCallback			 destroy;
/* Control the input mode depending on the native language */
/* - Setting method */
    int				 inputmode;
    int				 inputmode_resource;
    XimpChangeMaskRec		 save_mask;
    int				 inputmode_current ;
/* - Locking method */
    int				 lockmode;
    int				 lockmode_resource;
    int				 lockmode_request;
/* - Reporting method */
    int				 use_modechange;
    int				 modechangecommand;
    XIMCallback			 modechange;
/* Report the keyBinding change */
    int				 use_keybindchange;
/* XIMP Vendor Extension data */
    void			*ictype_vendor;
} Ximp_ExtXICRec;

/*
 * IC dependent data struction 
 */

typedef struct {
    long			 icid;
    int				 svr_mode;
    input_mode_t		 input_mode;
    int				 filter_mode;
    long			 value_mask;
    unsigned long		 back_mask;
    Bool			 putback_key_event;
    Window			 back_focus_win;
    char			*res_name,
				*res_class;
    long			 proto3_mask;
    long			 proto4_mask;
    Ximp_PreeditPropRec4	 preedit_attr;
    char			*preedit_font;
    Ximp_StatusPropRec4		 status_attr;
    char			*status_font;
    Ximp_CBStatus		 cbstatus;
    Ximp_ExtXICRec		*ictype;	 /* for Extension */
} XICXimpRec;

/*
 * IC data struction 
 */

typedef struct _Ximp_XIC	*Ximp_XIC;

typedef struct _Ximp_XIC {
    XICMethods	 methods;
    XICCoreRec	 core;
    XICXimpRec	*ximp_icpart;
} Ximp_XICRec;

/*
 * predicate argument
 */

typedef struct {
    Atom	type;
    Window	owner;
    int		request;
    int		protocol;
    ICID	icid;
} XimpCMPredicateArgRec, *XimpCMPredicateArg;

typedef struct {
    Atom	type;
    Window	owner;
    ICID	icid;
    Window	window;
    Atom	atom;
} XimpPNPredicateArgRec, *XimpPNPredicateArg;

typedef struct {
    char	*name;
    int		 (*func)();
} icop_t;

typedef enum {
    XICOpCreate = 1,
    XICOpSet = 2,
    XICOpGet = 3
} XICOp_t;

#define XIMP_INPUT_STYLE	0x0001
#define XIMP_CLIENT_WIN		0x0002
#define XIMP_RES_NAME		0x0004
#define XIMP_RES_CLASS		0x0008
#define XIMP_GEOMETRY_CB        0x0010
#define XIMP_FILTER_EV          0x0020
#define XIMP_PRE_CALLBAK        0x0040
#define XIMP_STS_CALLBAK        0x0080

#define XIMP_CHK_FOCUSWINMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_FOCUS_WIN_MASK4)
#define XIMP_CHK_PREAREAMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_AREA_MASK4)
#define XIMP_CHK_PREAREANEEDMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_AREANEED_MASK4)
#define XIMP_CHK_PRECOLORMAPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_COLORMAP_MASK4)
#define XIMP_CHK_PRESTDCOLORMAPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_STD_COLORMAP_MASK4)
#define XIMP_CHK_PREFGMASK(ic)			(ic->ximp_icpart->proto4_mask & XIMP_PRE_FG_MASK4)
#define XIMP_CHK_PREBGMASK(ic)			(ic->ximp_icpart->proto4_mask & XIMP_PRE_BG_MASK4)
#define XIMP_CHK_PREBGPIXMAPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_BGPIXMAP_MASK4)
#define XIMP_CHK_PRELINESPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_LINESP_MASK4)
#define XIMP_CHK_PRECURSORMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_CURSOR_MASK4)
#define XIMP_CHK_PRESPOTLMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_SPOTL_MASK4)
#define XIMP_CHK_STSAREAMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_AREA_MASK4)
#define XIMP_CHK_STSAREANEEDMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_AREANEED_MASK4)
#define XIMP_CHK_STSCOLORMAPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_COLORMAP_MASK4)
#define XIMP_CHK_STSSTDCOLORMAPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_STD_COLORMAP_MASK4)
#define XIMP_CHK_STSFGMASK(ic)			(ic->ximp_icpart->proto4_mask & XIMP_STS_FG_MASK4)
#define XIMP_CHK_STSBGMASK(ic)			(ic->ximp_icpart->proto4_mask & XIMP_STS_BG_MASK4)
#define XIMP_CHK_STSBGPIXMAPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_BGPIXMAP_MASK4)
#define XIMP_CHK_STSLINESPMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_LINESP_MASK4)
#define XIMP_CHK_STSCURSORMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_CURSOR_MASK4)
#define XIMP_CHK_STSWINDOWMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_WINDOW_MASK4)
#define XIMP_CHK_PREFONTMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_PRE_FONT_MASK4)
#define XIMP_CHK_STSFONTMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_STS_FONT_MASK4)
#define XIMP_CHK_SERVERTYPEMASK(ic)		(ic->ximp_icpart->proto4_mask & XIMP_SERVERTYPE_MASK4)

#define XIMP_CHK_PROP_FOCUS(mask)		( (mask.proto4) & XIMP_FOCUS_WIN_MASK4 )
#define XIMP_CHK_PROP_PREEDIT(mask)		( (mask.proto4) & \
						 (  XIMP_PRE_AREA_MASK4 \
						  | XIMP_PRE_AREANEED_MASK4 \
						  | XIMP_PRE_COLORMAP_MASK4 \
						  | XIMP_PRE_STD_COLORMAP_MASK4 \
						  | XIMP_PRE_FG_MASK4 \
						  | XIMP_PRE_BG_MASK4 \
						  | XIMP_PRE_BGPIXMAP_MASK4 \
						  | XIMP_PRE_LINESP_MASK4 \
						  | XIMP_PRE_CURSOR_MASK4 \
						  | XIMP_PRE_SPOTL_MASK4 ) )
#define XIMP_CHK_PROP_STATUS(mask)		( (mask.proto4) & \
						 (  XIMP_STS_AREA_MASK4 \
						  | XIMP_STS_AREANEED_MASK4 \
						  | XIMP_STS_COLORMAP_MASK4 \
						  | XIMP_STS_STD_COLORMAP_MASK4 \
						  | XIMP_STS_FG_MASK4 \
						  | XIMP_STS_BG_MASK4 \
						  | XIMP_STS_BGPIXMAP_MASK4 \
						  | XIMP_STS_LINESP_MASK4 \
						  | XIMP_STS_CURSOR_MASK4 \
						  | XIMP_STS_WINDOW_MASK4 ) )
#define XIMP_CHK_PROP_PREFONT(mask)		( (mask.proto4) & XIMP_PRE_FONT_MASK4 )
#define XIMP_CHK_PROP_STSFONT(mask)		( (mask.proto4) & XIMP_STS_FONT_MASK4 )
#define XIMP_EQU_PRESPOTLMASK(mask)		( (mask.proto4) == XIMP_PRE_SPOTL_MASK4 )

#define XIMP_UNSET_PROPFOCUS(mask)		{ mask.proto4 &= ~(XIMP_FOCUS_WIN_MASK4); \
						  mask.proto3 &= ~(XIMP_FOCUS_WIN_MASK3); }
#define XIMP_UNSET_PROPPREEDIT(mask)		{ mask.proto4 &= ~( \
						 (  XIMP_PRE_AREA_MASK4 \
						  | XIMP_PRE_AREANEED_MASK4 \
						  | XIMP_PRE_COLORMAP_MASK4 \
						  | XIMP_PRE_STD_COLORMAP_MASK4 \
						  | XIMP_PRE_FG_MASK4 \
						  | XIMP_PRE_BG_MASK4 \
						  | XIMP_PRE_BGPIXMAP_MASK4 \
						  | XIMP_PRE_LINESP_MASK4 \
						  | XIMP_PRE_CURSOR_MASK4 \
						  | XIMP_PRE_SPOTL_MASK4 \
						  | XIMP_PRE_FONT_MASK4 ) ); \
						  mask.proto3 &= ~( \
						 (  XIMP_PRE_AREA_MASK3 \
						  | XIMP_PRE_AREANEED_MASK3 \
						  | XIMP_PRE_COLORMAP_MASK3 \
						  | XIMP_PRE_FG_MASK3 \
						  | XIMP_PRE_BG_MASK3 \
						  | XIMP_PRE_BGPIXMAP_MASK3 \
						  | XIMP_PRE_LINESP_MASK3 \
						  | XIMP_PRE_CURSOR_MASK3 \
						  | XIMP_PRE_SPOTL_MASK3 \
						  | XIMP_PRE_FONT_MASK4 ) ); }
#define XIMP_UNSET_PROPSTATUS(mask)		{ mask.proto4 &= ~( \
						 (  XIMP_STS_AREA_MASK4 \
						  | XIMP_STS_AREANEED_MASK4 \
						  | XIMP_STS_COLORMAP_MASK4 \
						  | XIMP_STS_STD_COLORMAP_MASK4 \
						  | XIMP_STS_FG_MASK4 \
						  | XIMP_STS_BG_MASK4 \
						  | XIMP_STS_BGPIXMAP_MASK4 \
						  | XIMP_STS_LINESP_MASK4 \
						  | XIMP_STS_CURSOR_MASK4 \
						  | XIMP_STS_WINDOW_MASK4 \
						  | XIMP_STS_FONT_MASK4 ) ); \
						  mask.proto3 &= ~( \
						 (  XIMP_STS_AREA_MASK3 \
						  | XIMP_STS_AREANEED_MASK3 \
						  | XIMP_STS_COLORMAP_MASK3 \
						  | XIMP_STS_FG_MASK3 \
						  | XIMP_STS_BG_MASK3 \
						  | XIMP_STS_BGPIXMAP_MASK3 \
						  | XIMP_STS_LINESP_MASK3 \
						  | XIMP_STS_CURSOR_MASK3 \
						  | XIMP_STS_WINDOW_MASK3 \
						  | XIMP_STS_FONT_MASK3 ) ); }

#define XIMP_SET_NULLMASK(mask)			{ mask.proto4                 = NULL; \
						  mask.proto3                 = NULL; }
#define XIMP_SET_NULLMASK2(mask)		{ mask->proto4                 = NULL; \
						  mask->proto3                 = NULL; }

#define XIMP_SET_FOCUSWINMASK(ic)		{ ic->ximp_icpart->proto4_mask |= XIMP_FOCUS_WIN_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_FOCUS_WIN_MASK3; }

#define XIMP_SET_FOCUSWINMASK2(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_FOCUS_WIN_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_FOCUS_WIN_MASK3; \
						  mask->proto4                 |= XIMP_FOCUS_WIN_MASK4; \
						  mask->proto3                 |= XIMP_FOCUS_WIN_MASK3; }
#define XIMP_SET_PREAREAMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_AREA_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_AREA_MASK3; \
						  mask->proto4                 |= XIMP_PRE_AREA_MASK4; \
						  mask->proto3                 |= XIMP_PRE_AREA_MASK3; }
#define XIMP_SET_PREAREANEEDMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_AREANEED_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_AREANEED_MASK3; \
						  mask->proto4                 |= XIMP_PRE_AREANEED_MASK4; \
						  mask->proto3                 |= XIMP_PRE_AREANEED_MASK3; }
#define XIMP_SET_PRECOLORMAPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_COLORMAP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_COLORMAP_MASK3; \
						  mask->proto4                 |= XIMP_PRE_COLORMAP_MASK4; \
						  mask->proto3                 |= XIMP_PRE_COLORMAP_MASK3; }
#define XIMP_SET_PRESTDCOLORMAPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_STD_COLORMAP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_COLORMAP_MASK3; \
						  mask->proto4                 |= XIMP_PRE_STD_COLORMAP_MASK4; \
						  mask->proto3                 |= XIMP_PRE_COLORMAP_MASK3; }
#define XIMP_SET_PREFGMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_FG_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_FG_MASK3; \
						  mask->proto4                 |= XIMP_PRE_FG_MASK4; \
						  mask->proto3                 |= XIMP_PRE_FG_MASK3; }
#define XIMP_SET_PREBGMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_BG_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_BG_MASK3; \
						  mask->proto4                 |= XIMP_PRE_BG_MASK4; \
						  mask->proto3                 |= XIMP_PRE_BG_MASK3; }
#define XIMP_SET_PREBGPIXMAPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_BGPIXMAP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_BGPIXMAP_MASK3; \
						  mask->proto4                 |= XIMP_PRE_BGPIXMAP_MASK4; \
						  mask->proto3                 |= XIMP_PRE_BGPIXMAP_MASK3; }
#define XIMP_SET_PRELINESPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_LINESP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_LINESP_MASK3; \
						  mask->proto4                 |= XIMP_PRE_LINESP_MASK4; \
						  mask->proto3                 |= XIMP_PRE_LINESP_MASK3; }
#define XIMP_SET_PRECURSORMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_CURSOR_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_CURSOR_MASK3; \
						  mask->proto4                 |= XIMP_PRE_CURSOR_MASK4; \
						  mask->proto3                 |= XIMP_PRE_CURSOR_MASK3; }
#define XIMP_SET_PRESPOTLMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_SPOTL_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_SPOTL_MASK3; \
						  mask->proto4                 |= XIMP_PRE_SPOTL_MASK4; \
						  mask->proto3                 |= XIMP_PRE_SPOTL_MASK3; }

#define XIMP_SET_STSAREAMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_STS_AREA_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_AREA_MASK3; \
						  mask->proto4                 |= XIMP_STS_AREA_MASK4; \
						  mask->proto3                 |= XIMP_STS_AREA_MASK3; }
#define XIMP_SET_STSAREANEEDMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_AREANEED_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_AREANEED_MASK3; \
						  mask->proto4                 |= XIMP_STS_AREANEED_MASK4; \
						  mask->proto3                 |= XIMP_STS_AREANEED_MASK3; }
#define XIMP_SET_STSCOLORMAPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_COLORMAP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_COLORMAP_MASK3; \
						  mask->proto4                 |= XIMP_STS_COLORMAP_MASK4; \
						  mask->proto3                 |= XIMP_STS_COLORMAP_MASK3; }
#define XIMP_SET_STSSTDCOLORMAPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_STD_COLORMAP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_COLORMAP_MASK3; \
						  mask->proto4                 |= XIMP_STS_STD_COLORMAP_MASK4; \
						  mask->proto3                 |= XIMP_STS_COLORMAP_MASK3; }
#define XIMP_SET_STSFGMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_STS_FG_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_FG_MASK3; \
						  mask->proto4                 |= XIMP_STS_FG_MASK4; \
						  mask->proto3                 |= XIMP_STS_FG_MASK3; }
#define XIMP_SET_STSBGMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_STS_BG_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_BG_MASK3; \
						  mask->proto4                 |= XIMP_STS_BG_MASK4; \
						  mask->proto3                 |= XIMP_STS_BG_MASK3; }
#define XIMP_SET_STSBGPIXMAPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_BGPIXMAP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_BGPIXMAP_MASK3; \
						  mask->proto4                 |= XIMP_STS_BGPIXMAP_MASK4; \
						  mask->proto3                 |= XIMP_STS_BGPIXMAP_MASK3; }
#define XIMP_SET_STSLINESPMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_LINESP_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_LINESP_MASK3; \
						  mask->proto4                 |= XIMP_STS_LINESP_MASK4; \
						  mask->proto3                 |= XIMP_STS_LINESP_MASK3; }
#define XIMP_SET_STSCURSORMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_CURSOR_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_CURSOR_MASK3; \
						  mask->proto4                 |= XIMP_STS_CURSOR_MASK4; \
						  mask->proto3                 |= XIMP_STS_CURSOR_MASK3; }
#define XIMP_SET_STSWINDOWMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_STS_WINDOW_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_WINDOW_MASK3; \
						  mask->proto4                 |= XIMP_STS_WINDOW_MASK4; \
						  mask->proto3                 |= XIMP_STS_WINDOW_MASK3; }

#define XIMP_SET_PREFONTMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_PRE_FONT_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_PRE_FONT_MASK3; \
						  mask->proto4                 |= XIMP_PRE_FONT_MASK4; \
						  mask->proto3                 |= XIMP_PRE_FONT_MASK3; }
#define XIMP_SET_STSFONTMASK(ic, mask)		{ ic->ximp_icpart->proto4_mask |= XIMP_STS_FONT_MASK4; \
						  ic->ximp_icpart->proto3_mask |= XIMP_STS_FONT_MASK3; \
						  mask->proto4                 |= XIMP_STS_FONT_MASK4; \
						  mask->proto3                 |= XIMP_STS_FONT_MASK3; }
#define XIMP_SET_SERVERTYPEMASK(ic, mask)	{ ic->ximp_icpart->proto4_mask |= XIMP_SERVERTYPE_MASK4; \
						  mask->proto4                 |= XIMP_SERVERTYPE_MASK4; }

#define XIMP_PROTO_MASK(ic, mask)	(ISXimp4(ic)?mask.proto4:mask.proto3)
#define XIMP_PROTO_MASK2(ic)		(ISXimp4(ic)?ic->ximp_icpart->proto4_mask:ic->ximp_icpart->proto3_mask)

#define XIMP_FOCUS_WIN_MASK(ic)		(ISXimp4(ic)?XIMP_FOCUS_WIN_MASK4:XIMP_FOCUS_WIN_MASK3)
#define XIMP_PRE_AREA_MASK(ic)		(ISXimp4(ic)?XIMP_PRE_AREA_MASK4:XIMP_PRE_AREA_MASK3)
#define XIMP_PRE_AREANEED_MASK(ic)	(ISXimp4(ic)?XIMP_PRE_AREANEED_MASK4:XIMP_PRE_AREANEED_MASK3)
#define XIMP_PRE_COLORMAP_MASK(ic)	(ISXimp4(ic)?XIMP_PRE_COLORMAP_MASK4:XIMP_PRE_COLORMAP_MASK3)
#define XIMP_PRE_STD_COLORMAP_MASK(ic)	(ISXimp4(ic)?XIMP_PRE_STD_COLORMAP_MASK4:XIMP_PRE_COLORMAP_MASK3)
#define XIMP_PRE_FG_MASK(ic)		(ISXimp4(ic)?XIMP_PRE_FG_MASK4:XIMP_PRE_FG_MASK3)
#define XIMP_PRE_BG_MASK(ic)		(ISXimp4(ic)?XIMP_PRE_BG_MASK4:XIMP_PRE_BG_MASK3)
#define XIMP_PRE_BGPIXMAP_MASK(ic)	(ISXimp4(ic)?XIMP_PRE_BGPIXMAP_MASK4:XIMP_PRE_BGPIXMAP_MASK3)
#define XIMP_PRE_LINESP_MASK(ic)	(ISXimp4(ic)?XIMP_PRE_LINESP_MASK4:XIMP_PRE_LINESP_MASK3)
#define XIMP_PRE_CURSOR_MASK(ic)	(ISXimp4(ic)?XIMP_PRE_CURSOR_MASK4:XIMP_PRE_CURSOR_MASK3)
#define XIMP_PRE_SPOTL_MASK(ic)		(ISXimp4(ic)?XIMP_PRE_SPOTL_MASK4:XIMP_PRE_SPOTL_MASK3)
#define XIMP_STS_AREA_MASK(ic)		(ISXimp4(ic)?XIMP_STS_AREA_MASK4:XIMP_STS_AREA_MASK3)
#define XIMP_STS_AREANEED_MASK(ic)	(ISXimp4(ic)?XIMP_STS_AREANEED_MASK4:XIMP_STS_AREANEED_MASK3)
#define XIMP_STS_COLORMAP_MASK(ic)	(ISXimp4(ic)?XIMP_STS_COLORMAP_MASK4:XIMP_STS_COLORMAP_MASK3)
#define XIMP_STS_STD_COLORMAP_MASK(ic)	(ISXimp4(ic)?XIMP_STS_STD_COLORMAP_MASK4:XIMP_STS_COLORMAP_MASK3)
#define XIMP_STS_FG_MASK(ic)		(ISXimp4(ic)?XIMP_STS_FG_MASK4:XIMP_STS_FG_MASK3)
#define XIMP_STS_BG_MASK(ic)		(ISXimp4(ic)?XIMP_STS_BG_MASK4:XIMP_STS_BG_MASK3)
#define XIMP_STS_BGPIXMAP_MASK(ic)	(ISXimp4(ic)?XIMP_STS_BGPIXMAP_MASK4:XIMP_STS_BGPIXMAP_MASK3)
#define XIMP_STS_LINESP_MASK(ic)	(ISXimp4(ic)?XIMP_STS_LINESP_MASK4:XIMP_STS_LINESP_MASK3)
#define XIMP_STS_CURSOR_MASK(ic)	(ISXimp4(ic)?XIMP_STS_CURSOR_MASK4:XIMP_STS_CURSOR_MASK3)
#define XIMP_STS_WINDOW_MASK(ic)	(ISXimp4(ic)?XIMP_STS_WINDOW_MASK4:XIMP_STS_WINDOW_MASK3)
#define XIMP_PRE_FONT_MASK(ic)		(ISXimp4(ic)?XIMP_PRE_FONT_MASK4:XIMP_PRE_FONT_MASK3)
#define XIMP_STS_FONT_MASK(ic)		(ISXimp4(ic)?XIMP_STS_FONT_MASK4:XIMP_STS_FONT_MASK3)

#define XIMP_PROP_FOCUS(ic)	( XIMP_FOCUS_WIN_MASK(ic) )
#define XIMP_PROP_PREEDIT(ic)	( XIMP_PRE_AREA_MASK(ic) \
				| XIMP_PRE_FG_MASK(ic) \
				| XIMP_PRE_BG_MASK(ic) \
				| XIMP_PRE_COLORMAP_MASK(ic) \
				| XIMP_PRE_BGPIXMAP_MASK(ic) \
				| XIMP_PRE_LINESP_MASK(ic) \
				| XIMP_PRE_CURSOR_MASK(ic) \
				| XIMP_PRE_AREANEED_MASK(ic) \
				| XIMP_PRE_SPOTL_MASK(ic) )
#define XIMP_PROP_STATUS(ic)	( XIMP_STS_AREA_MASK(ic) \
				| XIMP_STS_FG_MASK(ic) \
				| XIMP_STS_BG_MASK(ic) \
				| XIMP_STS_COLORMAP_MASK(ic) \
				| XIMP_STS_BGPIXMAP_MASK(ic) \
				| XIMP_STS_LINESP_MASK(ic) \
				| XIMP_STS_CURSOR_MASK(ic) \
				| XIMP_STS_AREANEED_MASK(ic) \
				| XIMP_STS_WINDOW_MASK(ic) )
#define XIMP_PROP_PREFONT(ic)	( XIMP_PRE_FONT_MASK(ic) )
#define XIMP_PROP_STSFONT(ic)	( XIMP_STS_FONT_MASK(ic) )

#define XIMP_NOCONNECT		0x0000
#define XIMP_DELAYBINDABLE	0x0001
#define XIMP_RECONNECTABLE	0x0002
#define XIMP_RESTARTABLE	0x0004

#define IS_FORCESELECTKEYRELEASE(im)        (((Ximp_XIM)(im))->ximp_impart->is_forceselectkeyrelease)

#define ISXimp4IM(im) 		(im->ximp_impart->im_proto_vnum >= XIMP_VERSION_NUMBER) 
#define RECONNECTION_MODE(im) 	(((Ximp_XIM)(im))->ximp_impart->reconnection_mode)
#define IS_SERVER_CONNECTED(im)	(((Ximp_XIM)(im))->ximp_impart->is_connected)

#define IS_IC_CONNECTED(ic)	(ic->ximp_icpart->icid)

#define IS_RECONNECTABLE(im)   (RECONNECTION_MODE(im) & XIMP_RECONNECTABLE)
#define MAKE_RECONNECTABLE(im) (RECONNECTION_MODE(im) |= XIMP_RECONNECTABLE)
#define IS_DELAYBINDABLE(im)   (RECONNECTION_MODE(im) & XIMP_DELAYBINDABLE)
#define MAKE_DELAYBINDABLE(im) (RECONNECTION_MODE(im) |= XIMP_DELAYBINDABLE)
#define IS_RESTARTABLE(im)     (RECONNECTION_MODE(im) & XIMP_RESTARTABLE)
#define MAKE_RESTARTABLE(im)   (RECONNECTION_MODE(im) |= XIMP_RESTARTABLE)
#define IS_UNCONNECTABLE(im)   (RECONNECTION_MODE(im) == XIMP_NOCONNECT)
#define MAKE_UNCONNECTABLE(im) (RECONNECTION_MODE(im) = XIMP_NOCONNECT)
#define IS_CONNECTABLE(im)     (RECONNECTION_MODE(im) != XIMP_NOCONNECT)
#define MAKE_CONNECTABLE(im)   (RECONNECTION_MODE(im) = XIMP_RECONNECTABLE|XIMP_DELAYBINDABLE|XIMP_RESTARTABLE)

#define ISXimp4(ic)   (((Ximp_XIM)ic->core.im)->ximp_impart->im_proto_vnum >= XIMP_VERSION_NUMBER)

#define ISFE1(ic)     (((Ximp_XIC)ic)->ximp_icpart->svr_mode == XIMP_FE_TYPE1)
#define ISFE2(ic)     (((Ximp_XIC)ic)->ximp_icpart->svr_mode == XIMP_FE_TYPE2)
#define ISFE3(ic)     (((Ximp_XIC)ic)->ximp_icpart->svr_mode == XIMP_FE_TYPE3)
#define ISBE1(ic)     ((((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_BE_TYPE1) == XIMP_BE_TYPE1)
#define ISBE2(ic)     ((((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_BE_TYPE2) == XIMP_BE_TYPE2)
#define ISSYNCBE1(ic) (((Ximp_XIC)ic)->ximp_icpart->svr_mode == XIMP_SYNC_BE_TYPE1)
#define ISSYNCBE2(ic) (((Ximp_XIC)ic)->ximp_icpart->svr_mode == XIMP_SYNC_BE_TYPE2)
#define ISFRONTEND(ic) (((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_FRONTEND4)
#define ISBACKEND(ic) (((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_BACKEND4)
#define ISTYPE1(ic)   (((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_TYPE1)
#define ISTYPE2(ic)   (((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_TYPE2)
#define ISTYPE3(ic)   (((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_TYPE3)
#define ISSYNC(ic)    (((Ximp_XIC)ic)->ximp_icpart->svr_mode & XIMP_SYNC)

#define ISXIMP3FE(ic) ((((Ximp_XIC)ic)->ximp_icpart->svr_mode & 0x03) == XIMP_FRONTEND_BC_MASK)

#define IS_BEING_PREEDITED(ic)	(((Ximp_XIC)ic)->ximp_icpart->input_mode == BEING_PREEDITED)
#define IS_FABLICATED(ic,ev)	((ic->ximp_icpart->putback_key_event) || (ev->keycode == 0))
#define IS_USE_WCHAR(ic)	(((Ximp_XIM)ic->core.im)->ximp_impart->use_wchar)
    
#define ISCMOf(ev,n,x)		(((XEvent*)ev)->xclient.data.l[n] == (x))
#define ISXIMP_ERROR(ev)	(((XEvent*)ev)->type == ClientMessage && ((XEvent*)ev)->xclient.format == 32 \
				  && (ISCMOf(ev,0,XIMP_ERROR3) || ISCMOf(ev,0,XIMP_ERROR4)))
#define XIMP_SYNCHRONIZE(ic)	 {if(IS_SERVER_CONNECTED((ic->core.im)) && (IS_BEING_PREEDITED(ic)) &&\
				      ((((Ximp_XIC)ic)->ximp_icpart->svr_mode) & XIMP_SYNC ))\
				      _Ximp_Synchronize(ic);}

#define XIMP_PREEDIT_MAX_LONG(ic) (ISXimp4(ic)?XIMP_PREEDIT_MAX_LONG4:XIMP_PREEDIT_MAX_LONG3)
#define XIMP_STATUS_MAX_LONG(ic)  (ISXimp4(ic)?XIMP_STATUS_MAX_LONG4:XIMP_STATUS_MAX_LONG3)

#define  XIMP3_RESET_RETURN_ATOM    2
#define  XIMP4_RESET_RETURN_DETAIL  2
#define  XIMP4_RESET_RETURN_ATOM    3
#define  NO_RESET_DATA              0
#define  RESET_DATA_VIA_CM          1
#define  RESET_DATA_VIA_PROP        2
#define  XIMP_RESET_RETURN_ATOM(ic) (ISXimp4(ic)?XIMP4_RESET_RETURN_ATOM:XIMP3_RESET_RETURN_ATOM)

#define  XIMP_KEYRELEASE(ic)		XIMP_KEYRELEASE4
#define  XIMP_KEYPRESS(ic)		(ISXimp4(ic)?XIMP_KEYPRESS4:XIMP_KEYPRESS3)

#define  XIMP_CREATE(ic)		(ISXimp4(ic)?XIMP_CREATE4:XIMP_CREATE3)
#define  XIMP_DESTROY(ic)		(ISXimp4(ic)?XIMP_DESTROY4:XIMP_DESTROY3)
#define  XIMP_REG_KEY_PRESSED(ic)	(ISXimp4(ic)?XIMP_REG_KEY_PRESSED4:XIMP_BEGIN3)
#define  XIMP_SETFOCUS(ic)		(ISXimp4(ic)?XIMP_SETFOCUS4:XIMP_SETFOCUS3)
#define  XIMP_UNSETFOCUS(ic)		(ISXimp4(ic)?XIMP_UNSETFOCUS4:XIMP_UNSETFOCUS3)
#define  XIMP_CLIENT_WINDOW(ic)		XIMP_CLIENT_WINDOW4
#define  XIMP_FOCUS_WINDOW(ic)		XIMP_FOCUS_WINDOW4
#define  XIMP_MOVE(ic)			(ISXimp4(ic)?XIMP_MOVE4:XIMP_MOVE3)
#define  XIMP_RESET(ic)			(ISXimp4(ic)?XIMP_RESET4:XIMP_RESET3)
#define  XIMP_SETVALUE(ic)		(ISXimp4(ic)?XIMP_SETVALUE4:XIMP_SETVALUE3)
#define  XIMP_GETVALUE(ic)		(ISXimp4(ic)?XIMP_GETVALUE4:XIMP_GETVALUE3)

#define  XIMP_PREEDITSTART_RETURN(ic)	(ISXimp4(ic)?XIMP_PREEDITSTART_RETURN4:XIMP_PREEDITSTART_RETURN3)
#define  XIMP_PREEDITCARET_RETURN(ic)	(ISXimp4(ic)?XIMP_PREEDITCARET_RETURN4:XIMP_PREEDITCARET_RETURN3)
#define  XIMP_SPROC_STARTED(ic)		(ISXimp4(ic)?XIMP_SPROC_STARTED4:XIMP_PROCESS_BEGIN3)
#define  XIMP_SPROC_STOPPED(ic)		(ISXimp4(ic)?XIMP_SPROC_STOPPED4:XIMP_PROCESS_END3)
#define  XIMP_READPROP(ic)		(ISXimp4(ic)?XIMP_READPROP4:XIMP_READPROP3)
#define  XIMP_CLIENT_WINDOW_RETURN(ic)	XIMP_CLIENT_WINDOW_RETURN4
#define  XIMP_FOCUS_WINDOW_RETURN(ic)	XIMP_FOCUS_WINDOW_RETURN4
#define  XIMP_GETVALUE_RETURN(ic)	(ISXimp4(ic)?XIMP_GETVALUE_RETURN4:XIMP_GETVALUE_RETURN3)
#define  XIMP_RESET_RETURN(ic)		(ISXimp4(ic)?XIMP_RESET_RETURN4:XIMP_RESET_RETURN3)
#define  XIMP_CREATE_RETURN(ic)		(ISXimp4(ic)?XIMP_CREATE_RETURN4:XIMP_CREATE_RETURN3)
#define  XIMP_KEYPRESS_RETURN(ic)	XIMP_KEYPRESS_RETURN4
#define  XIMP_KEYRELEASE_RETURN(ic)	XIMP_KEYRELEASE_RETURN4
#define  XIMP_EVENTMASK_NOTIFY(ic)	XIMP_EVENTMASK_NOTIFY4
#define  XIMP_EVENTMASK_NOTIFY_RETURN(ic)	XIMP_EVENTMASK_NOTIFY_RETURN4

#define  XIMP_GEOMETRY(ic)		(ISXimp4(ic)?XIMP_GEOMETRY4:XIMP_GEOMETRY3)
#define  XIMP_PREEDITSTART(ic)		(ISXimp4(ic)?XIMP_PREEDITSTART4:XIMP_PREEDITSTART3)
#define  XIMP_PREEDITDONE(ic)		(ISXimp4(ic)?XIMP_PREEDITDONE4:XIMP_PREEDITDONE3)
#define  XIMP_PREEDITDRAW(ic)		(ISXimp4(ic)?XIMP_PREEDITDRAW4:XIMP_PREEDITDRAW3)
#define  XIMP_PREEDITDRAW_CM(ic)	(ISXimp4(ic)?XIMP_PREEDITDRAW_CM4:XIMP_PREEDITDRAW_CM3)
#define  XIMP_PREEDITDRAW_CM_TINY(ic)	(ISXimp4(ic)?XIMP_PREEDITDRAW_CM_TINY4:XIMP_PREEDITDRAW_CM_TINY3)
#define  XIMP_PREEDITCARET(ic)		(ISXimp4(ic)?XIMP_PREEDITCARET4:XIMP_PREEDITCARET3)
#define  XIMP_STATUSSTART(ic)		(ISXimp4(ic)?XIMP_STATUSSTART4:XIMP_STATUSSTART3)
#define  XIMP_STATUSDONE(ic)		(ISXimp4(ic)?XIMP_STATUSDONE4:XIMP_STATUSDONE3)
#define  XIMP_STATUSDRAW(ic)		(ISXimp4(ic)?XIMP_STATUSDRAW4:XIMP_STATUSDRAW3)
#define  XIMP_STATUSDRAW_CM(ic)		(ISXimp4(ic)?XIMP_STATUSDRAW_CM4:XIMP_STATUSDRAW_CM3)
#define  XIMP_EXTENSION(ic)		(ISXimp4(ic)?XIMP_EXTENSION4:XIMP_EXTENSION3)

/*
 * extern
 */

#define Public		/**/
#define Private		static

extern int	 Ximp_Xim_count;
extern Ximp_XIM	*Ximp_Xim_List;

extern void
_Ximp_CallGeometryCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallPreeditStartCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallPreeditDoneCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallPreeditDrawCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallPreeditDrawCallback2(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallPreeditDrawCallback3(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallPreeditCaretCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallStatusStartCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallStatusDoneCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallStatusDrawCallback(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallStatusDrawCallback2(
#if NeedFunctionPrototypes
	Ximp_XIC, XClientMessageEvent *
#endif
);

extern int
_ximp_ext_do_flush(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern int
_XimpIsNeedMoveProtoMode(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern int
_Ximp_ext_iocpHook(
#if NeedFunctionPrototypes
	Ximp_XIC, char *, XICOp_t, long
#endif
);

extern void
_Ximp_OpenIMResourceExtension(
#if NeedFunctionPrototypes
	Ximp_XIM
#endif
);

extern Bool
_XimpConnectServerExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIM
#endif
);

extern void
_XimpConnectServerFreeExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIM
#endif
);

extern Bool
_Ximp_GetIMValuesExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIM, char *, long
#endif
);

extern Bool
_XimpCreateICVendorHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpCreateICExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpBeforeCreateConnectionsHook(
#if NeedFunctionPrototypes
	Ximp_XIC, XIMArg *
#endif
);

extern void
_Ximp_SetIcValueAfterCreate(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_XimpAfterCreateConnectionsHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_XimpAfterLoseConnectionsHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Ximp_XIC
_XimpBeforeSetClientWindowExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Ximp_XIC
_XimpAfterSetClientWindowExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpSetICValuesExtensionHookCheck(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern char *
_XimpSetICValuesExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC, XIMArg *
#endif
);

extern Bool
_Ximp_SetFocusExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_Ximp_UnSetFocusExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpReconnectICExtensionCheak(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpReconnectICExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpLockedICExtensionCheck(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_ServerDestroyExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_Ximp_SetICExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC, char *, long, int
#endif
);

extern Bool
_Ximp_GetICExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC, char *, long
#endif
);

extern void
_Ximp_AfterCreateExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_ProcExtension(
#if NeedFunctionPrototypes
	Display *, Window, XClientMessageEvent *
#endif
);

extern void
_Ximp_CallRestartCallbackExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_CallDestroyCallbackExtensionHook(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern XIC
_Ximp_CreateIC(
#if NeedFunctionPrototypes
	XIM, XIMArg *
#endif
);

extern Bool
_XimpDestroyICSendMessageCheck(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetFocusWindowFilter(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetFocusWindowProp(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetPreeditAtr(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetPreeditFont(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetStatusAtr(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetStatusFont(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_IM_SendMessage(
#if NeedFunctionPrototypes
	Ximp_XIC, unsigned long, unsigned long, unsigned long, unsigned long
#endif
);

extern XPointer
_Ximp_GetRequestIM(
#if NeedFunctionPrototypes
	Ximp_XIC, unsigned long, Atom, Atom
#endif
);

extern char *
_Ximp_GetICValues(
#if NeedFunctionPrototypes
	XIC, XIMArg *
#endif
);

extern void
_XimpClearPropertyEvent(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern Bool
_XimpPNIfEvent(
#if NeedFunctionPrototypes
	Ximp_XIC, Atom
#endif
);

extern char *
_Ximp_SetICValues(
#if NeedFunctionPrototypes
	XIC, XIMArg *
#endif
);

extern Bool
_XimpSetFocusWindowCheck(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern char *
_Ximp_SetICValueData(
#if NeedFunctionPrototypes
	Ximp_XIC, XIMArg *, int, XimpChangeaMask
#endif
);

extern char *
_Ximp_Strstr(
#if NeedFunctionPrototypes
	char *, char *
#endif
);

extern Ximp_XIC
_Ximp_LookupXIC(
#if NeedFunctionPrototypes
	int, Window
#endif
);

extern Atom
_Ximp_Protocol_id(
#if NeedFunctionPrototypes
	void
#endif
);

extern XIM
_Ximp_OpenIM(
#if NeedFunctionPrototypes
	XLCd, Display *, XrmDatabase, char *, char *
#endif
);

extern Bool
_Ximp_ConnectServer(
#if NeedFunctionPrototypes
	Ximp_XIM
#endif
);

extern int
_Ximp_SetupFree(
#if NeedFunctionPrototypes
	char *, XIMStyles *, long *, Ximp_KeyList *, Ximp_KeyList *, char *, char *, char *, Atom *
#endif
);

extern Bool
_XimpIfEvent(
#if NeedFunctionPrototypes
	Ximp_XIC, XEvent *, Bool (*)(
#if NeedNestedPrototypes
	Display *, XEvent *, char *
#endif
	), char *
#endif
);

extern Bool
_Ximp_CMPredicate32(
#if NeedFunctionPrototypes
	Display *, XEvent *, XPointer
#endif
);

extern int
_Ximp_CombineMultipleCM(
#if NeedFunctionPrototypes
	Ximp_XIC, unsigned char **
#endif
);

extern void
_Ximp_ServerProcessStopped(
#if NeedFunctionPrototypes
	Display *, Window, XClientMessageEvent *
#endif
);

extern char *
_Ximp_MbReset(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern wchar_t *
_Ximp_WcReset(
#if NeedFunctionPrototypes
	XIC
#endif
);

extern int
_Ximp_MbLookupString(
#if NeedFunctionPrototypes
	XIC, XKeyEvent *, char *, int, KeySym *, Status *
#endif
);

extern int
_Ximp_WcLookupString(
#if NeedFunctionPrototypes
	XIC, XKeyEvent *, wchar_t *, int, KeySym *, Status *
#endif
);

extern void
_Ximp_GetFocusWindowSelectMask(
#if NeedFunctionPrototypes
	Ximp_XIC, Window, unsigned long *
#endif
);

extern Bool
_Ximp_ConnectIC(
#if NeedFunctionPrototypes
	Ximp_XIC, int
#endif
);

extern void
_Ximp_ServerProcessStarted(
#if NeedFunctionPrototypes
	Display *, Window, XClientMessageEvent *
#endif
);

extern void
_Ximp_ConvertOn(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_ConvertOff(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_ProcError(
#if NeedFunctionPrototypes
	Ximp_XIC, Display *, Window, XClientMessageEvent *
#endif
);

extern Bool
_Ximp_XimFilter_Keypress(
#if NeedFunctionPrototypes
	Display *, Window, XEvent *, XPointer
#endif
);

extern Bool
_Ximp_XimFilter_Keyrelease(
#if NeedFunctionPrototypes
	Display *, Window, XEvent *, XPointer
#endif
);

extern Bool
_Ximp_XimFilter_Client(
#if NeedFunctionPrototypes
	Display *, Window, XEvent *, XPointer
#endif
);

extern Bool
_Ximp_XimFilter_Destroy(
#if NeedFunctionPrototypes
	Display *, Window, XEvent *, XPointer
#endif
);

extern void
_Ximp_OpenIM_Resource(
#if NeedFunctionPrototypes
	Ximp_XIM
#endif
);

extern void
_Ximp_CreateIC_InitializeRDB(
#if NeedFunctionPrototypes
	Ximp_XIC
#endif
);

extern void
_Ximp_SetValue_Resource(
#if NeedFunctionPrototypes
	Ximp_XIC, XimpChangeaMask
#endif
);

extern int
_Ximp_ctstombs(
#if NeedFunctionPrototypes
	Ximp_XIM, char *, int, char *, int, Status *
#endif
);

extern int
_Ximp_ctstowcs(
#if NeedFunctionPrototypes
	Ximp_XIM, char *, int, wchar_t *, int, Status *
#endif
);

extern int
_Ximp_mbs_charlen(
#if NeedFunctionPrototypes
	Ximp_XIM, char *, int
#endif
);

extern int
_Ximp_LookupMBText(
#if NeedFunctionPrototypes
	Ximp_XIC, XKeyEvent *, unsigned char *, int, KeySym *, XComposeStatus *
#endif
);

extern int
_Ximp_LookupWCText(
#if NeedFunctionPrototypes
	Ximp_XIC, XKeyEvent *, wchar_t *, int, KeySym *, XComposeStatus *
#endif
);

extern int
_XimConvertCharCode(
#if NeedFunctionPrototypes
	char *, int, KeySym, XlcConv
#endif
);
