/* $XimpImplementGroup: XimpExten.c, v 1.1 94/05/31 21:16:04 $ */
/* $XConsortium: XimpExten.c,v 1.7 92/10/19 19:24:25 rws Exp $ */
/******************************************************************

    Copyright 1991, 1992 by FUJITSU LIMITED.
    Copyright 1991, 1992 by Sun Microsystems, Inc.
    Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of FUJITSU LIMITED, Sun
Microsystems, Inc. and Sony Corporation not be used in advertising
or publicity pertaining to distribution of the software without
specific, written prior permission. FUJITSU LIMITED, Sun Microsystems,
Inc. and Sony Corporation make no representations about the suitability
of this software for any purpose.  It is provided "as is" without
express or implied warranty.

FUJITSU LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION DISCLAIMS
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU
LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION BE LIABLE FOR
ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Author: Takashi Fujiwara     FUJITSU LIMITED
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

#define NEED_EVENTS
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "Xlibint.h"
#include "XimpIm.h"

#define		XIM_UNKNOWN_KEYSYM	0x77777777
#define		XIM_UNDETERMINED	0x77777776

/*
 * LookupChoice Region
 */

typedef enum {
    XIMCB_Success,
    XIMCB_FatalError
}               XIMCBResult;

typedef enum {
    DrawUpHorizontally = 0,
    DrawUpVertically = 1
}               DrawUpDirection;

typedef enum {
    XIMOwnsLabel = 0,
    CBOwnsLabel = 1
}               WhoOwnsLabel;

typedef struct {
    int             choice_per_window;	/* Number of choices can be display
					 * in the region */
    int             nrows;
    int             ncolumns;
    DrawUpDirection DrawUpDirection;
    WhoOwnsLabel    WhoOwnsLabel;	/* For callback to tell XIM whether
					 * it wants to control what label
					 * should be for the choices. */
}               LayoutInfo;

typedef enum {
    HasNotBeenNegotiated = 0,
    XIMIsMaster = 1,
    CBIsMaster = 2
} WhoIsMaster;

typedef struct _XIMLookupStartCallbackStruct {
    XKeyEvent      *event;
    WhoIsMaster     WhoIsMaster;/* For callback to tell whether is going to
				 * be a master */
    LayoutInfo     *XIMPreference;
    LayoutInfo     *CBPreference;
}               XIMLookupStartCallbackStruct;

typedef struct _XIMChoiceObject {
    XIMText        *label;
    XIMText        *value;
}               XIMChoiceObject;

typedef struct _XIMLookupDrawCallbackStruct {
    XIMChoiceObject *choices;	/* the lookup choices */
    int             n_choices;	/* Total number of lookup choices */
    int             max_len;	/* Max number of characters per choice item */
    int             index_of_first_candidate;
    int             index_of_last_candidate;
}               XIMLookupDrawCallbackStruct;

typedef struct _XIMLookupProcessCallbackStruct {
    XKeyEvent      *event;
    int             index_of_choice_selected;	/* Set by callback for the
						 * selected choice.
						 * XIM_UNKNOWN_KEYSYM and
						 * XIM_UNDETERMINED are also
						 * possible value. */
}               XIMLookupProcessCallbackStruct;

/*
 * Ximp extension to XIC attributes
 */
#define XNExtXimp_Backfront             "XNExtXimp_Backfront"
#define XNExtXimp_Statuswindow          "XNExtXimp_Statuswindow"
#define XNExtXimp_Conversion	        "XNExtXimp_Conversion"
#define XNExtXimp_Error		        "XNExtXimp_Error"
#define XNExtXimp_LookupAttributes	"XNExtXimp_LookupAttributes"
#define XNExtXimp_LookupStartCallback   "XNExtXimp_LookupStartCallback"
#define XNExtXimp_LookupDrawCallback    "XNExtXimp_LookupDrawCallback"
#define XNExtXimp_LookupDoneCallback    "XNExtXimp_LookupDoneCallback"
#define XNExtXimp_LookupProcessCallback "XNExtXimp_LookupProcessCallback"
#define XNExtXimp_AuxAttributes		"XNExtXimp_AuxAttributes"
#define XNExtXimp_AuxStartCallback      "XNExtXimp_AuxStartCallback"
#define XNExtXimp_AuxDrawCallback       "XNExtXimp_AuxDrawCallback"
#define XNExtXimp_AuxProcessCallback    "XNExtXimp_AuxProcessCallback"
#define XNExtXimp_AuxDoneCallback       "XNExtXimp_AuxDoneCallback"
#define XNExtXimp_LookupBegin	        "XNExtXimp_LookupBegin"
#define XNExtXimp_RestartCallback	"XNExtXimp_RestartCallback"
#define XNExtXimp_DestroyCallback	"XNExtXimp_DestroyCallback"
#define XNExtXimp_InputMode		"XNExtXimp_InputMode"
#define XNExtXimp_ModeLock		"XNExtXimp_ModeLock"
#define XNExtXimp_Flush			"XNExtXimp_Flush"
#define XNExtXimp_ModeChangeCallback	"XNExtXimp_ModeChangeCallback"

#ifdef fujitsu
#define XNExtFujitsu_InputMode		"XNExtFujitsu_InputMode"
#define XNExtFujitsu_ModeLock		"XNExtFujitsu_ModeLock"
#define XNExtFujitsu_Flush		"XNExtFujitsu_Flush"
#define XNExtFujitsu_ModeChangeCallback	"XNExtFujitsu_ModeChangeCallback"
#define XNExtFujitsu_ICID		"XNExtFujitsu_ICID"
#endif

/*
 * Ximp properties for extented XIC attribute
 */
#define XIMP_EXT_XIMP_CONVERSION           "_XIMP_EXT_XIMP_CONVERSION"
#define XIMP_EXT_XIMP_BACK_FRONT           "_XIMP_EXT_XIMP_BACK_FRONT"
#define XIMP_EXT_XIMP_STATUSWINDOW         "_XIMP_EXT_XIMP_STATUSWINDOW"
#define XIMP_EXT_XIMP_ERROR		   "_XIMP_EXT_XIMP_ERROR"
#define XIMP_EXT_XIMP_AUXSTARTCALLBACK     "_XIMP_EXT_XIMP_AUXSTARTCALLBACK"
#define XIMP_EXT_XIMP_AUXDRAWCALLBACK      "_XIMP_EXT_XIMP_AUXDRAWCALLBACK"
#define XIMP_EXT_XIMP_AUXPROCESSCALLBACK   "_XIMP_EXT_XIMP_AUXPROCESSCALLBACK"
#define XIMP_EXT_XIMP_AUXDONECALLBACK      "_XIMP_EXT_XIMP_AUXDONECALLBACK"
#define XIMP_EXT_XIMP_LOOKUPSTARTCALLBACK  "_XIMP_EXT_XIMP_LOOKUPSTARTCALLBACK"
#define XIMP_EXT_XIMP_LOOKUPDRAWCALLBACK   "_XIMP_EXT_XIMP_LOOKUPDRAWCALLBACK"
#define XIMP_EXT_XIMP_LOOKUPDONECALLBACK   "_XIMP_EXT_XIMP_LOOKUPDONECALLBACK"
#define XIMP_EXT_XIMP_LOOKUPPROCESSCALLBACK "_XIMP_EXT_XIMP_LOOKUPPROCESSCALLBACK"
#define XIMP_EXT_XIMP_LOOKUPCHOICES        "_XIMP_EXT_XIMP_LOOKUPCHOICES"

#ifdef EXTCONTROLMODE
#define XIMP_EXT_XIMP_INPUTMODE		   "_XIMP_EXT_FUJITSU_INPUTMODE"
#define XIMP_EXT_XIMP_INPUTMODE_RTN	   "_XIMP_EXT_FUJITSU_INPUTMODE_RTN"
#define XIMP_EXT_XIMP_MODELOCK		   "_XIMP_EXT_FUJITSU_MODELOCK"
#define XIMP_EXT_XIMP_FLUSH		   "_XIMP_EXT_FUJITSU_FLUSH"
#define XIMP_EXT_XIMP_KEYBINDCHANGE	   "_XIMP_EXT_FUJITSU_KEYBINDCHANGE"
#define XIMP_EXT_XIMP_MODECHANGE	   "_XIMP_EXT_FUJITSU_MODECHANGE"
#endif

/*
 * Ximp extension to IM attributes
 */
#define XIMLookupCallbacks	0x4000000L
#define XIMAuxCallbacks		0x8000000L

/*
 * Possible values of XSetICValues(XNExtXimp_Conversion, XXX );
 */
#define	XIMEnable	1
#define	XIMDisable	0

/*
 * Possible values of XSetICValues(XNExtXimp_Backfront, XXX );
 */
#define	IMServBackend	1
#define	IMServFrontend  0

#ifdef EXTCONTROLMODE
/*
 * Possible values of XSetICValues().
 * Control the input mode depending on the native language
 */
#define	XIMP_TOHENKAN		0x00001000
#define	XIMP_HENKAN	XIMP_TOHENKAN
#define	XIMP_TOMUHENKAN		0x00002000
#define	XIMP_MUHENKAN	XIMP_TOMUHENKAN
#define	XIMP_TOZENKAKU		0x00000100
#define	XIMP_ZENKAKU	XIMP_TOZENKAKU
#define	XIMP_TOHANKAKU		0x00000200
#define	XIMP_HANKAKU	XIMP_TOHANKAKU
#define	XIMP_TOKANA		0x00000010
#define	XIMP_KANA	XIMP_TOKANA
#define	XIMP_TOROMAN		0x00000020
#define	XIMP_ROMAN	XIMP_TOROMAN
#define	XIMP_TOHIRAGANA		0x00000001
#define	XIMP_HIRAGANA	XIMP_TOHIRAGANA
#define	XIMP_TOKATAKANA		0x00000002
#define	XIMP_KATAKANA	XIMP_TOKATAKANA
#define	XIMP_TOUPPER		0x00000004
#define	XIMP_UPPER	XIMP_TOUPPER
#define	XIMP_TOLOWER		0x00000008
#define	XIMP_LOWER	XIMP_TOLOWER

#define	XIMP_HANKAKU_HIRAGANA	0x00000100
#define	XIMP_HANKAKU_KATAKANA	0x00000200
#define	XIMP_HANKAKU_UPPER	0x00000400
#define	XIMP_HANKAKU_LOWER	0x00000800
#define	XIMP_ZENKAKU_HIRAGANA	0x00000001
#define	XIMP_ZENKAKU_KATAKANA	0x00000002
#define	XIMP_ZENKAKU_UPPER	0x00000004
#define	XIMP_ZENKAKU_LOWER	0x00000008

#define	XIMP_NOLOCK	( XIMP_MUHENKAN | \
			  XIMP_HANKAKU_HIRAGANA | XIMP_HANKAKU_KATAKANA | \
			  XIMP_HANKAKU_UPPER | XIMP_HANKAKU_LOWER | \
			  XIMP_ZENKAKU_HIRAGANA | XIMP_ZENKAKU_KATAKANA | \
			  XIMP_ZENKAKU_UPPER | XIMP_ZENKAKU_LOWER )

#define	XIMP_LOCK_FULL_MASK	XIMP_NOLOCK
#endif /* EXTCONTROLMODE */

Private int _Ximp_ext_icop();
Private void _Ximp_Ext_ModeChange();

/*
 * Declaration of the entry functions for each extensions.
 */
Private int     ximp_ext_backfront();
Private int     ximp_ext_conversion();
Private int     ximp_ext_statuswindow();
Private int     ximp_ext_error();

#ifdef EXTLOOKUPCALLBACK
Private int     ximp_ext_lookup_begin();
Private int     ximp_ext_lookup_start_callback();
Private int     ximp_ext_lookup_draw_callback();
Private int     ximp_ext_lookup_done_callback();
Private int     ximp_ext_lookup_process_callback();
Private int     ximp_ext_aux_start_callback();
Private int     ximp_ext_aux_draw_callback();
Private int     ximp_ext_aux_process_callback();
Private int     ximp_ext_aux_done_callback();
Private int	nested_list();
#endif

Private int     ximp_ext_restart_callback();
Private int     ximp_ext_destroy_callback();

#ifdef EXTCONTROLMODE
Private int	ximp_ext_inputmode();
Private int	ximp_ext_modelock();
Private int	ximp_ext_flush();
Private int	ximp_ext_modechange();
#endif
#ifdef fujitsu
Private int	ximp_ext_icid();
#endif

/* If you need to extend IC attributes, please add function here */

Private
icop_t icoptbl[] = {
	XNExtXimp_Backfront, ximp_ext_backfront,
	XNExtXimp_Conversion, ximp_ext_conversion,
	XNExtXimp_Statuswindow, ximp_ext_statuswindow,
	XNExtXimp_Error, ximp_ext_error,
#ifdef EXTLOOKUPCALLBACK
    	XNExtXimp_LookupAttributes, nested_list,
	XNExtXimp_LookupStartCallback, ximp_ext_lookup_start_callback,
	XNExtXimp_LookupDrawCallback, ximp_ext_lookup_draw_callback,
	XNExtXimp_LookupDoneCallback, ximp_ext_lookup_done_callback,
	XNExtXimp_LookupProcessCallback, ximp_ext_lookup_process_callback,
    	XNExtXimp_LookupAttributes, nested_list,
	XNExtXimp_AuxStartCallback, ximp_ext_aux_start_callback,
	XNExtXimp_AuxDrawCallback, ximp_ext_aux_draw_callback,
	XNExtXimp_AuxProcessCallback, ximp_ext_aux_process_callback,
	XNExtXimp_AuxDoneCallback, ximp_ext_aux_done_callback,
	XNExtXimp_LookupBegin, ximp_ext_lookup_begin,
#endif
	XNExtXimp_RestartCallback, ximp_ext_restart_callback,
	XNExtXimp_DestroyCallback, ximp_ext_destroy_callback,
#ifdef EXTCONTROLMODE
	XNExtXimp_InputMode, ximp_ext_inputmode,
	XNExtXimp_ModeLock, ximp_ext_modelock,
	XNExtXimp_Flush, ximp_ext_flush,
	XNExtXimp_ModeChangeCallback, ximp_ext_modechange,
#endif
    /*
     * If you need to extend IC attributes, please add attribute/function
     * here
     */
#ifdef fujitsu
#ifdef EXTCONTROLMODE
	XNExtFujitsu_InputMode, ximp_ext_inputmode,
	XNExtFujitsu_ModeLock, ximp_ext_modelock,
	XNExtFujitsu_Flush, ximp_ext_flush,
	XNExtFujitsu_ModeChangeCallback, ximp_ext_modechange,
#endif
	XNExtFujitsu_ICID, ximp_ext_icid,
#endif
	0
};

#define ICOPTBLSIZE ((sizeof(icoptbl)/sizeof(icop_t)) - 1)

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_BACK_FRONT
 */

Private int
ximp_ext_backfront(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    /* For Backward Compatibility */
    if(IS_SERVER_CONNECTED(ic->core.im)) {
	if(!ISXimp4(ic)) {
	    if (!(ext_im->extension_back_front_exist))
		return (False);
	}
    }
    switch (op) {
      case XICOpCreate:
	/* For Backward Compatibility */
	if(ISXimp4(ic)){
	    /* This value is already set in Ximp4.0 */
	    return(True);                           /* Only return(True) */
	}
	if(((int)value) == XIMP_FRONTEND) {
	    ic->ximp_icpart->svr_mode = ((ic->ximp_icpart->svr_mode & ~(XIMP_BACKEND_BC_MASK)) | XIMP_FRONTEND_BC_MASK); 
	    return (True);
	} else if(((int)value) == XIMP_BACKEND) {
	    ic->ximp_icpart->svr_mode = ((ic->ximp_icpart->svr_mode & ~(XIMP_FRONTEND_BC_MASK)) | XIMP_BACKEND_BC_MASK); 
	    return (True);
	}
	break; /*XXX*/

      case XICOpSet:
	return (False);	/* This should set at the IC creation time. */

      case XICOpGet:
	if(ISFRONTEND(ic)) {
	    *((long *) value) = (long)(XIMP_FRONTEND);
	    return (True);
	} else if(ISBACKEND(ic)) {
	    *((long *) value) = (long)(XIMP_BACKEND);
	    return (True);
	}
	break; /*XXX*/
    }
    return (False);
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_CONVERSION
 */
/**
 * Extended protocol for XNExtXimp_Conversion.
 * From Ximp lib to IM Server (To query or to set)
 *	  +-----------------------------------------+
 *	0 | XIMP_EXTENSION                          |
 *	  +-----------------------------------------+
 *	4 | ICID                                    |
 *	  +-----------------------------------------+
 *	8 | Atom ID of XNExtXimp_Conversion         |
 *	  +-----------------------------------------+
 *	12| Operation: Boolean Set(True)/Get(False) |
 *	  +-----------------------------------------+
 *	16| Conversion: Boolean On(True)/Off(False) |
 *	  +-----------------------------------------+
 *
 * From IM Server to Ximp library (reply of querying)
 *	  +-----------------------------------------+
 *	0 | XIMP_EXTENSION                          |
 *	  +-----------------------------------------+
 *	4 | ICID                                    |
 *	  +-----------------------------------------+
 *	8 | Atom ID of XNExtXimp_Conversion         |
 *	  +-----------------------------------------+
 *	12| VOID                                    |
 *	  +-----------------------------------------+
 *	16| Conversion: Boolean On(True)/Off(False) |
 *	  +-----------------------------------------+
 **/

typedef struct {
	Atom	message_type;
	Atom	ext_type;
	Window	owner;
	ICID	icid;
} XimpConversionPredArgRec, *XimpConversionPredArg;

Private Bool
#if NeedFunctionPrototypes
ximp_ext_conversionPredicate(
    Display *d,
    XEvent *ev,
    XPointer arg0
)
#else
ximp_ext_conversionPredicate(d, ev, arg0)
    Display *d;
    XEvent  *ev;
    XPointer arg0;
#endif
{
    XimpConversionPredArg arg = (XimpConversionPredArg) arg0;
    
    Ximp_XIC	ic;
    
    if ((ev->type == ClientMessage) && (ev->xclient.format ==32)) {
	ic = _Ximp_LookupXIC(ev->xclient.data.l[1], ev->xclient.window);
	if (ev->xclient.message_type == arg->message_type  &&
	    ev->xclient.data.l[1] == arg->icid ) {
	    if(ISXIMP_ERROR(ev)) {
		return(True);
	    } else if(ev->xclient.data.l[0] == XIMP_EXTENSION(ic)) {
		if(ev->xclient.data.l[2] == arg->ext_type)
		return(True);
	    }
	}
    } else if (ev->type == DestroyNotify) {
	if (ev->xdestroywindow.window == arg->owner) {
	    return(True);
	}
    }
    return(False);
}

Private int
ximp_ext_conversion(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;
    XEvent	    event;
    XimpConversionPredArgRec Arg;

    if (!(ext_im->extension_conversion_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	if(!IS_SERVER_CONNECTED(ic->core.im)) {
	    if(IS_CONNECTABLE(ic->core.im))
		return(True);
	    return(False);
	}
	/*
	 * Set Conversion mode on/off
	 */
#ifdef EXTCONTROLMODE
	if(ic->ximp_icpart->ictype->lockmode == XIMP_MUHENKAN) {
		return False;
	} else if( ic->ximp_icpart->ictype->lockmode &&
		(!(ic->ximp_icpart->ictype->lockmode & XIMP_MUHENKAN))) {
		return False;
	}
#endif
	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_conversion_id,
		True,	/* SetICVelues */
		value);
	if(ISXimp4(ic)) {
	    if(value == XIMEnable) /* on */
		_Ximp_ConvertOn(ic);
	    else /* off */
		_Ximp_ConvertOff(ic);
	}
	/*
	 * This call expect IM Server to report new conversion state to Ximp
	 * lib right after setting.
	 */
	return True;
    case XICOpGet:
	if(!IS_SERVER_CONNECTED(ic->core.im)) {
	    if(IS_CONNECTABLE(ic->core.im)) {
		*((long *) value) = (long)(ext_im->extension_conversion);
		return(True);
		}
	    return(False);
	}
	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_conversion_id,
		False,	/* GetICVelues */
		value);
	Arg.message_type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	Arg.ext_type     = ext_im->extension_conversion_id;
	Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	Arg.icid = ic->ximp_icpart->icid;
	if( !_XimpIfEvent( ic, &event, ximp_ext_conversionPredicate, (XPointer)&Arg ) )
	    return( False );
	ext_im->extension_conversion = (Bool)event.xclient.data.l[4];
	*((long *) value) = (long)(ext_im->extension_conversion);
	return( True );
    }
    return (False);
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_STATUSWINDOW
 */

Private int
ximp_ext_statuswindow(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if (!(ext_im->extension_statuswindow_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_ERROR
 */

Private int
ximp_ext_error(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->ictype->error.client_data
				= ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->ictype->error.callback
				= ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_restart_callback( ic, op, value )
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch( op ) {
      case XICOpCreate:
      case XICOpSet:
	ic->ximp_icpart->ictype->restart.client_data
					= ((XIMCallback *)value)->client_data;
	ic->ximp_icpart->ictype->restart.callback
					= ((XIMCallback *)value)->callback;
	return( True );
	break;

      case XICOpGet:
	if( ic->ximp_icpart->ictype->restart.callback != NULL ) {
	    ((XIMCallback *)value)->callback
				= ic->ximp_icpart->ictype->restart.callback;
            return( True );
        }
	break;
    }
    return( False );
}

Private int
ximp_ext_destroy_callback( ic, op, value )
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch( op ) {
      case XICOpCreate:
      case XICOpSet:
	ic->ximp_icpart->ictype->destroy.client_data
				= ((XIMCallback *)value)->client_data;
	ic->ximp_icpart->ictype->destroy.callback
				= ((XIMCallback *)value)->callback;
	return( True );
	break;

      case XICOpGet:
	if( ic->ximp_icpart->ictype->destroy.callback != NULL ) {
	    ((XIMCallback *)value)->callback
				= ic->ximp_icpart->ictype->destroy.callback;
            return( True );
        }
	break;
    }
    return( False );
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_LOOKUPCHOICES
 */
#ifdef EXTLOOKUPCALLBACK

Private int
ximp_ext_lookup_start_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->ictype->lookup_attr.callbacks.start.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->ictype->lookup_attr.callbacks.start.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_draw_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->ictype->lookup_attr.callbacks.draw.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->ictype->lookup_attr.callbacks.draw.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_done_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->ictype->lookup_attr.callbacks.done.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->ictype->lookup_attr.callbacks.done.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_process_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->ictype->lookup_attr.callbacks.proc.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->ictype->lookup_attr.callbacks.proc.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_start_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_draw_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_process_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_done_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_begin(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    XIMXimpRec     *im_impart = ((Ximp_XIM)ic->core.im)->ximp_impart;
    Ximp_ExtXIMRec *ext_im;

    ext_im = (Ximp_ExtXIMRec *) im_impart->imtype;
    if (!(ext_im->extension_lookup_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
	ic->ximp_icpart->ictype->use_lookup_choices = True ;
	return(True);
    case XICOpSet:
	if(!IS_IC_CONNECTED(ic)) {
	    return False;
	}
	ic->ximp_icpart->ictype->use_lookup_choices = True ;
	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
			     ext_im->extension_lookup_id,
			     LOOKUP_CHOICES_BEGIN,
			     NULL);
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private void _Ximp_Extlookupstart();
Private void _Ximp_Extlookupdraw();
Private void _Ximp_Extlookupprocess();
Private void _Ximp_Extlookupdone();

Private void
_Ximp_ExtLookup(d, w, ev, ic)
    Display        *d;
    Window          w;
    XClientMessageEvent *ev;
    Ximp_XIC        ic;
{
    switch (ev->data.l[3]) {
    case LOOKUP_CHOICES_START_REQ:
	_Ximp_Extlookupstart(ic, ev);
	break;
    case LOOKUP_CHOICES_DRAW_REQ:
	_Ximp_Extlookupdraw(ic, ev);
	break;
    case LOOKUP_CHOICES_PROCESS_REQ:
	_Ximp_Extlookupprocess(ic, ev, d, w);
	break;
    case LOOKUP_CHOICES_DONE_REQ:
	_Ximp_Extlookupdone(ic, ev);
	break;
    }
    return;
}

Private void
FreeXIMLookupDrawCallbackStruct(p)
    XIMLookupDrawCallbackStruct *p;
{
    register i ;
    for(i = 0 ; i < p->n_choices ; i++){
	if(p->choices[i].label){
	    if(p->choices[i].label->string.multi_byte)
	      Xfree(p->choices[i].label->string.multi_byte);
	    if(p->choices[i].label->feedback)
	      Xfree(p->choices[i].label->feedback);
	    Xfree(p->choices[i].label);
	}
	if(p->choices[i].value){
	    if(p->choices[i].value->string.multi_byte)
	      Xfree(p->choices[i].value->string.multi_byte);
	    if(p->choices[i].value->feedback)
	      Xfree(p->choices[i].value->feedback);
	    Xfree(p->choices[i].value);
	}
    }
    Xfree(p->choices);
    Xfree(p);
}

Private void
_Ximp_Extlookupstart(ic, event)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
{
    register XIMCallback *cb;
    static XClientMessageEvent clmsg;
    XIMLookupStartCallbackStruct xim_start;
    Ximp_ExtXIMRec *ext_im;
    long           *prop;
    Atom            type;
    int             format;
    unsigned long   nitems, after;

    cb = &ic->ximp_icpart->ictype->lookup_attr.callbacks.start;
    if(ic->ximp_icpart->ictype->lookup_attr.draw_data){
	FreeXIMLookupDrawCallbackStruct(ic->ximp_icpart->ictype->lookup_attr.draw_data);
	ic->ximp_icpart->ictype->lookup_attr.draw_data = NULL ;
    }
    if (cb->callback) {
	if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			       event->data.l[4], 0, 8, True, AnyPropertyType,
			       &type, &format, &nitems, &after,
			       (unsigned char **) &prop) == Success) && prop){
	    xim_start.event = (XKeyEvent *) Xmalloc(sizeof(XEvent));
	    bzero(xim_start.event , sizeof(XEvent));
	    xim_start.event->keycode = *prop;
	    xim_start.event->state = *(prop + 1);
	    xim_start.event->window = ic->core.focus_window ;
	    xim_start.WhoIsMaster = *(prop + 2);
	    xim_start.XIMPreference = (LayoutInfo *)Xmalloc(sizeof(LayoutInfo));
	    xim_start.XIMPreference->choice_per_window = *(prop + 3);
	    xim_start.XIMPreference->nrows = *(prop + 4);
	    xim_start.XIMPreference->ncolumns = *(prop + 5);
	    xim_start.XIMPreference->DrawUpDirection = *(prop + 6);
	    xim_start.XIMPreference->WhoOwnsLabel = *(prop + 7);
	    xim_start.CBPreference = (LayoutInfo *) Xmalloc(sizeof(LayoutInfo));
	    XFree((char *) prop);

	    (*cb->callback) (ic, cb->client_data, &xim_start);

	    ext_im = (Ximp_ExtXIMRec *) (((Ximp_XIMRec *) ic->core.im)->ximp_impart->imtype);

	    prop = (long *) Xmalloc(sizeof(long) * 6);
	    *(prop + 0) = xim_start.WhoIsMaster;
	    *(prop + 1) = xim_start.CBPreference->choice_per_window;
	    *(prop + 2) = xim_start.CBPreference->nrows;
	    *(prop + 3) = xim_start.CBPreference->ncolumns;
	    *(prop + 4) = xim_start.CBPreference->DrawUpDirection;
	    *(prop + 5) = xim_start.CBPreference->WhoOwnsLabel;

	    XChangeProperty(ic->core.im->core.display,
			    ic->core.focus_window,
			    ext_im->extension_lookup_start_rep,
			    ext_im->extension_lookup_start_rep,
			    32,
			    PropModeReplace,
			    (unsigned char *)prop,
			    6);

	    XFlush(ic->core.im->core.display);

	    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
			ext_im->extension_lookup_id,
			LOOKUP_CHOICES_START_REP,
			ext_im->extension_lookup_start_rep);

	    XFlush(ic->core.im->core.display);

	    Xfree((char *) prop);
	    Xfree((char *) xim_start.event);
	    Xfree((char *) xim_start.CBPreference);
	    Xfree((char *) xim_start.XIMPreference);
	}
    }
}

Private void
_Ximp_Extlookupdraw(ic, event)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
{
    Ximp_XIM        im = (Ximp_XIM)ic->core.im;
    register XIMCallback *cb;
    static XClientMessageEvent clmsg;
    XIMLookupDrawCallbackStruct *luc_draw;
    Ximp_ExtXIMRec *ext_im;
    long           *prop;
    char           *text, *textaddr;
    long           *feedback;
    XIMText        *textbuf;
    XIMChoiceObject *choicebuf, *chptr;
    Atom            type;
    int             format;
    int             i, l, j;
    int             strnum;
    unsigned long   nitems, after;

    cb = &ic->ximp_icpart->ictype->lookup_attr.callbacks.draw;
    if(ic->ximp_icpart->ictype->lookup_attr.draw_data){
	FreeXIMLookupDrawCallbackStruct(ic->ximp_icpart->ictype->lookup_attr.draw_data);
	ic->ximp_icpart->ictype->lookup_attr.draw_data =  NULL ;
	
    }
    if (!cb->callback)
	return;

    ic->ximp_icpart->ictype->lookup_attr.draw_data =  (XPointer)(luc_draw
      = (XIMLookupDrawCallbackStruct *)Xmalloc(sizeof(XIMLookupDrawCallbackStruct)));
    bzero(luc_draw, sizeof(XIMLookupDrawCallbackStruct));

    if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			   event->data.l[4], 0, 5, True, AnyPropertyType,
			   &type, &format, &nitems, &after,
			   (unsigned char **) &prop) != Success) || !prop)
	return;

    luc_draw->max_len = *prop;
    luc_draw->index_of_first_candidate = *(prop + 1);
    luc_draw->index_of_last_candidate = *(prop + 2);

    if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			   *(prop + 3), 0, 4096, True, AnyPropertyType,
			   &type, &format, &nitems, &after,
			   (unsigned char **) &text) == Success) && text) {

	for (strnum = 0, textaddr = text, i = 0; i < nitems; i++) {
	    j = strlen(textaddr);
	    textaddr += (j + 1);
	    strnum++;
	    i += j;
	}

	choicebuf = (XIMChoiceObject *) Xmalloc(sizeof(XIMChoiceObject)
					       * (strnum / 2));
	bzero(choicebuf, sizeof(XIMChoiceObject) * (strnum / 2));

	for (textaddr = text, chptr = choicebuf, i = 0; i < strnum; i += 2, chptr++) {
	    if (*textaddr) {
		chptr->label = (XIMText *) Xmalloc(sizeof(XIMText));
		bzero(chptr->label, sizeof(XIMText));
		l = j = strlen(textaddr);
		chptr->label->string.multi_byte = (char *) Xmalloc(j);
		bzero(chptr->label->string.multi_byte,j);
		l = _Ximp_ctstombs(im, textaddr, j,
			      chptr->label->string.multi_byte, l, NULL);
		chptr->label->length = l;
		textaddr += j;
	    }
	    textaddr++;

	    if (*textaddr) {
		chptr->value = (XIMText *) Xmalloc(sizeof(XIMText));
		bzero(chptr->value, sizeof(XIMText));
		l = j = strlen(textaddr);
		chptr->value->string.multi_byte = (char *) Xmalloc(j);
		bzero(chptr->value->string.multi_byte,j);
		l = _Ximp_ctstombs(im, textaddr, j,
			      chptr->value->string.multi_byte, l, NULL);
		chptr->value->length = l;
		textaddr += j;
	    }
	    textaddr++;
	}

	luc_draw->choices = choicebuf;
	luc_draw->n_choices = (strnum / 2);
    }
    if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			   *(prop + 4), 0, 4096, True, AnyPropertyType,
			   &type, &format, &nitems, &after,
			   (unsigned char **) &feedback) == Success)&&feedback){

	for (i = l = 0; i < luc_draw->n_choices; i++, l += 2) {
	    if (luc_draw->choices[i].value){
		register len = luc_draw->choices[i].value->length ;
		luc_draw->choices[i].value->feedback =
		  (XIMFeedback *)Xmalloc(sizeof(XIMFeedback) * len );
		if(len == 1){
		    luc_draw->choices[i].value->feedback[0] = feedback[l];
		} else {
		    register j ;
		    for (j = 0 ; j < len ; j++){
			luc_draw->choices[i].value->feedback[j] = feedback[l+1];
		    }
		}
		if (luc_draw->choices[i].label){
		    luc_draw->choices[i].label->feedback = 
		      (XIMFeedback *)Xmalloc(sizeof(XIMFeedback) * len );
		    if(len == 1){
			luc_draw->choices[i].label->feedback[0] = feedback[l];
		    } else {
			register j ;
			for (j = 0 ; j < len ; j++){
			    luc_draw->choices[i].label->feedback[j] = feedback[l];
			}
		    }
		}
	    }
	}
	(*cb->callback) (ic, cb->client_data, luc_draw);
	XFree(text);
	XFree(feedback);
    }
}

Private void
_Ximp_Extlookupprocess(ic, event, d, w)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
    Display        *d;
    Window          w;
{
    register XIMCallback *cb;
    static XClientMessageEvent clmsg;
    XIMLookupProcessCallbackStruct xim_proc;
    Ximp_ExtXIMRec *ext_im;
    long           *prop;
    Atom            type;
    int             format;
    unsigned long   nitems, after;

    cb = &ic->ximp_icpart->ictype->lookup_attr.callbacks.proc;

    if (cb->callback) {
	if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			       event->data.l[4], 0, 2, True, AnyPropertyType,
			       &type, &format, &nitems, &after,
			       (unsigned char **) &prop) == Success)&&prop) {

	    xim_proc.event = (XKeyEvent *)event;
	    xim_proc.event->keycode = ((long *)prop)[0];
	    xim_proc.event->state = ((long *)prop)[1];
	    xim_proc.event->type = KeyPress ;

	    (*cb->callback) (ic, cb->client_data, &xim_proc);

	    ext_im = (Ximp_ExtXIMRec *) (((Ximp_XIMRec *) ic->core.im)->ximp_impart->imtype);

	    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
			ext_im->extension_lookup_id,
			LOOKUP_CHOICES_PROCESS_REP,
			xim_proc.index_of_choice_selected);

	    XFlush(ic->core.im->core.display);
	}
    }
}

Private void
_Ximp_Extlookupdone(ic, event)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
{
    register XIMCallback *cb;

    cb = &ic->ximp_icpart->ictype->lookup_attr.callbacks.done;

    if (cb->callback) {
	(*cb->callback) (ic, cb->client_data, NULL);
    }
    if(ic->ximp_icpart->ictype->lookup_attr.draw_data){
	FreeXIMLookupDrawCallbackStruct(ic->ximp_icpart->ictype->lookup_attr.draw_data);
	ic->ximp_icpart->ictype->lookup_attr.draw_data = NULL ;	
    }
}

Private int
nested_list(ic, op, args)
    Ximp_XIC ic ;
    XICOp_t op;
    XIMArg *args ;
{
    register i ;
    int status ;
    register XIMArg *arg;

    for (arg = args; arg->name && *(arg->name); arg++) {
	if(_Ximp_ext_icop(ic, arg->name, op, arg->value ) == False){
	    return False ;
	}
    }
    return True ;
}  
#endif /* EXTLOOKUPCALLBACK */

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_INPUTMODE
 *      XIMP_EXT_XIMP_INPUTMODE_RTN
 */
#ifdef EXTCONTROLMODE
#define XIMP_EXT_INPUTMODE3	(1L << 29)

Private Bool
#if NeedFunctionPrototypes
ximp_ext_setimodePredicate(
    Display *d,
    XEvent *ev,
    XPointer arg0
)
#else
ximp_ext_setimodePredicate(d, ev, arg0)
    Display *d;
    XEvent  *ev;
    XPointer arg0;
#endif
{
    XimpConversionPredArg arg = (XimpConversionPredArg) arg0;
    
    Ximp_XIC	ic;
    
    if((ev->type == ClientMessage) && (ev->xclient.format == 32)) {
	ic = _Ximp_LookupXIC(ev->xclient.data.l[1], ev->xclient.window);
	if(ev->xclient.message_type == arg->message_type  &&
	    ev->xclient.data.l[1] == arg->icid ) {
	    if(ISXIMP_ERROR(ev)) {
		return(True);
	    } else if(ic && ev->xclient.data.l[0] == XIMP_EXTENSION(ic)) {
		if(ev->xclient.data.l[2] == arg->ext_type)
		return(True);
	    } else if(!ic && ((ev->xclient.data.l[0] == XIMP_EXTENSION3) ||
			       (ev->xclient.data.l[0] == XIMP_EXTENSION4))) {
		if(ev->xclient.data.l[2] == arg->ext_type)
		return(True);
	    }
	}
    } else if(ev->type == DestroyNotify) {
	if(ev->xdestroywindow.window == arg->owner) {
	    return(False);
	}
    }
    return(False);
}

Private int
_ximp_ext_get_inputmode(ic)
    Ximp_XIC	    ic;
{
    int			 mode = 0;
    XPointer		 tmp;
    Ximp_ExtXIMRec	*ext_im 
	= (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if (ext_im->extension_modechange_exist 
		&& ic->ximp_icpart->ictype->use_modechange) {
	return(ic->ximp_icpart->ictype->inputmode_current);
    }
    tmp = (XPointer)_Ximp_GetRequestIM(ic, XIMP_EXT_INPUTMODE3,
			ext_im->extension_inputmode_id, XA_INTEGER);
    
    if(tmp) {
	mode = *(int *)tmp;
	Xfree(tmp) ;
    }
    return(mode);
}

Private int
_ximp_ext_set_inputmode(ic, mode)
    Ximp_XIC	ic;
    int		mode;
{
    XimpConversionPredArgRec  Arg;
    XEvent	event;
    Ximp_ExtXIMRec	*ext_im 
	= (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    /* Check Lock Status */
    if(ic->ximp_icpart->ictype->lockmode == XIMP_MUHENKAN) {
	if(mode & XIMP_TOHENKAN)
	    return False;
    } else if(  ic->ximp_icpart->ictype->lockmode &&
	     (!(ic->ximp_icpart->ictype->lockmode & XIMP_MUHENKAN))) {
	if( mode == XIMP_TOMUHENKAN )
	    return False;
    }

    /* Check Current Status */
    if( ext_im->extension_modechange_exist 
	&& ic->ximp_icpart->ictype->use_modechange) {
	int current_mode;

	current_mode = _ximp_ext_get_inputmode(ic);
	if(mode == current_mode) {
	    return(True);
	}
    }

    if(mode == XIMP_MUHENKAN) {
	if (ic->ximp_icpart->input_mode == BEING_BYPASSED) {
	    return(True);
	}
    }

    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_inputmode_id, mode, NULL);

    if( !(ext_im->extension_inputmode_rtn_exist) ) {
	return(True);
    }

    Arg.message_type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.ext_type = ext_im->extension_inputmode_id;
    Arg.icid = ic->ximp_icpart->icid;

    if(!_XimpIfEvent(ic, &event, ximp_ext_setimodePredicate, (XPointer)&Arg)){
	return(False);
    }

    if(event.xclient.data.l[3] == -1) { /* FEP Returned Error */
	return(False);
    }
    if((event.xclient.data.l[3] & mode) == mode) {
	if(ic->ximp_icpart->ictype->use_modechange)
	    ic->ximp_icpart->ictype->inputmode_current = 
					event.xclient.data.l[3]; 
        if(mode == XIMP_MUHENKAN) {
	    _Ximp_ServerProcessStopped(((Ximp_XIM)ic->core.im)->core.display,
			ic->core.focus_window, (XClientMessageEvent *)&event);
	} else {
            if(ic->ximp_icpart->input_mode == BEING_BYPASSED) {
		_Ximp_ServerProcessStarted(
			((Ximp_XIM)ic->core.im)->core.display,
			ic->core.focus_window, (XClientMessageEvent *)&event);
		if(_XimpIsNeedMoveProtoMode(ic))
		    _Ximp_IM_SendMessage(ic, XIMP_MOVE(ic),
				 ic->ximp_icpart->preedit_attr.SpotLocation.x,
				 ic->ximp_icpart->preedit_attr.SpotLocation.y,
				 NULL);
	    }
	}
    } else {
	return(False);
    } 
    return(True);
}

Private int
ximp_ext_inputmode(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    int		    imode;
    Ximp_ExtXIMRec *ext_im 
	= (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if(IS_SERVER_CONNECTED(ic->core.im)) {
	if(!(ext_im->extension_inputmode_exist)) {
	    return (False);
	}
    }

    switch (op) {
      case XICOpCreate:
	ic->ximp_icpart->ictype->inputmode = (int)value; 
	return (True);

      case XICOpSet:
        if(IS_SERVER_CONNECTED(ic->core.im)) {
	    if(_ximp_ext_set_inputmode(ic, (int)value)) {
		return(True);
	    } else {
		return(False);
	    }
	} else {
	    ic->ximp_icpart->ictype->inputmode = (int)value; 
	    return (True);
	}

      case XICOpGet:
        if(IS_SERVER_CONNECTED(ic->core.im)) {
	    if((imode = _ximp_ext_get_inputmode(ic)) < 0) {
		/* Server Returned Error */
		return(False);
	    } else {
		*(int *)value = imode;
		return(True);
	    }
        } else {
	    *(int *)value = XIMP_TOMUHENKAN;
	    return(True);
        }
    }
    return (False);
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_MODELOCK
 */

Private int
_ximp_ext_set_lockmode(ic, mode)
    Ximp_XIC	    ic;
    int		    mode;
{
    XimpConversionPredArgRec  Arg;
    XEvent	    event;
    int		    real_mode;
    Ximp_ExtXIMRec *ext_im = 
	(Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_modelock_id, mode, NULL);

    Arg.message_type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.ext_type = ext_im->extension_modelock_id;
    Arg.icid = ic->ximp_icpart->icid;

    if(!_XimpIfEvent(ic, &event, ximp_ext_setimodePredicate, (XPointer)&Arg)){
	return(False);
    }

    if(((real_mode = event.xclient.data.l[3]) < 0)
 	|| (real_mode != mode)) {
	return(False);
    }
    return(True);
}

Private int
ximp_ext_modelock(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    int		    lkmode;
    Ximp_ExtXIMRec *ext_im = 
	(Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if(IS_SERVER_CONNECTED(ic->core.im)) {
	if(!(ext_im->extension_modelock_exist)) {
	    return (False);
	}
    }

    switch (op) {
      case XICOpCreate:
	ic->ximp_icpart->ictype->lockmode_request = (int)value; 
	return (True);

      case XICOpSet:
        if(IS_SERVER_CONNECTED(ic->core.im)) {
	    if(_ximp_ext_set_lockmode(ic, value)) {
		ic->ximp_icpart->ictype->lockmode = (int)value; 
		return(True);
	    } else {
		return(False);
	    }
	} else {
	    ic->ximp_icpart->ictype->lockmode = (int)value; 
	    return(True);
	}
      case XICOpGet:
	*(int *)value = ic->ximp_icpart->ictype->lockmode;
	return(True);
    }
    return (False);
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_FLUSH
 */

Public int
_ximp_ext_do_flush(ic)
    Ximp_XIC	    ic;
{
    Ximp_ExtXIMRec *ext_im = 
	(Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if(IS_SERVER_CONNECTED(ic->core.im)) {
	if(!(ext_im->extension_flush_exist)) {
	    return (False);
	}
    } else {
	return (False);
    }

    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_flush_id, NULL, NULL);
    
    return(True);
}

Private int
ximp_ext_flush(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = 
	(Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if(IS_SERVER_CONNECTED(ic->core.im)) {
	if(!(ext_im->extension_flush_exist)) {
	    return (False);
	}
    }
    switch (op) {
      case XICOpCreate:
	return (False);

      case XICOpSet:
	if (value != 1) {
	    return(True);
	}
        if(IS_SERVER_CONNECTED(ic->core.im)) {
	    return(_ximp_ext_do_flush(ic));
	} else {
	    return (True);
	}

      case XICOpGet:
	return (True);
    }
}

/*
 * Ximp extension
 *      XIMP_EXT_XIMP_MODECHANGE
 */

Private int
ximp_ext_modechange(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    XIMCallback    *p_callback;
    Ximp_ExtXIMRec *ext_im = 
	(Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if(IS_SERVER_CONNECTED(ic->core.im)) {
	if(!(ext_im->extension_modechange_exist)) {
	    return (False);
	}
    }

    switch (op) {
      case XICOpCreate:
      case XICOpSet:
      {
	XimpConversionPredArgRec  Arg;
	XEvent event;

	ic->ximp_icpart->ictype->modechange.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->ictype->modechange.callback =
	    ((XIMCallback *) value)->callback;
	ic->ximp_icpart->ictype->use_modechange = True;

	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
					ext_im->extension_modechange_id,
					NULL, NULL);
	Arg.message_type = 
		((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	Arg.ext_type = ext_im->extension_modechange_id;
	Arg.icid = ic->ximp_icpart->icid;
	if(!_XimpIfEvent(ic, &event, ximp_ext_setimodePredicate, 
							(XPointer)&Arg)){
		return(False);
	}
	_Ximp_Ext_ModeChange( ((Ximp_XIM)ic->core.im)->core.display,
				ic->core.focus_window,&event,ic);
	return (True);
      }

      case XICOpGet:
        if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
	   return (False);
        }
        p_callback->client_data =
            ic->ximp_icpart->ictype->modechange.client_data;
        p_callback->callback =
            ic->ximp_icpart->ictype->modechange.callback;
        *((XIMCallback **)(value)) = p_callback;
	return (True);
    }
}

Private void
_Ximp_Ext_ModeChange(d, w, ev, ic)
    Display        *d;
    Window          w;
    XClientMessageEvent *ev;
    Ximp_XIC        ic;
{
    XIMCallback  *cb;
    int new_mode[5];
    new_mode[0] = ev->data.l[3];
    new_mode[1] = ev->data.l[4];
    new_mode[2] = 0;

    cb = &ic->ximp_icpart->ictype->modechange;
    ic->ximp_icpart->ictype->inputmode_current = new_mode[0];
    ic->ximp_icpart->ictype->modechangecommand = new_mode[1];
    if (cb->callback) {
	(*cb->callback) (ic, cb->client_data, new_mode);
    }
    if(_XimpIsNeedMoveProtoMode(ic)){
         _Ximp_IM_SendMessage(ic, XIMP_MOVE(ic),
             ic->ximp_icpart->preedit_attr.SpotLocation.x,
             ic->ximp_icpart->preedit_attr.SpotLocation.y,
             NULL);
    }
}

Private void
_Ximp_Ext_KeyChange(d, w, ev, ic)
    Display        *d;
    Window          w;
    XClientMessageEvent *ev;
    Ximp_XIC        ic;
{
    Atom 		keys_id, actual_type;
    Window		fe_window_id = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    int			actual_format, count, n, i, *prop_int;
    unsigned long	nitems, bytes_after;
    Ximp_KeyList        *keylist;
    int			keylist_alock;

    keylist_alock = 0;
    keys_id = ((Ximp_XIM)ic->core.im)->ximp_impart->keys_id ;
    if( XGetWindowProperty(d, fe_window_id, keys_id, 0L, 1000000L, False,
		keys_id, &actual_type, &actual_format, &nitems,
		&bytes_after, (unsigned char **)(&prop_int)) != Success) {
	return;
    }
    if (!((Ximp_XIM)ic->core.im)->ximp_impart->im_keyslist) {
	if((keylist = (Ximp_KeyList *)Xmalloc(sizeof(Ximp_KeyList))) == NULL) {
	    XFree((XPointer)prop_int );
	    return;
	}
	keylist->keys_list = NULL;
	keylist_alock = 1;
    } else {
	keylist = ((Ximp_XIM)ic->core.im)->ximp_impart->im_keyslist;
    }
    count = nitems / 3;
    if ((keylist->keys_list) && (count > (int)keylist->count_keys)) {
	XFree(keylist->keys_list);
	keylist->keys_list = NULL;
    }
    if (!keylist->keys_list) {
	if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key) * count)) == NULL) {
	    if (keylist_alock) Xfree(keylist);
	    XFree( (XPointer)prop_int );
	    return;
	}
    }
    for(i=0,n=0; n < count; n++) {
	keylist->keys_list[n].modifier        = prop_int[i++];
	keylist->keys_list[n].modifier_mask   = prop_int[i++];
	keylist->keys_list[n].keysym          = prop_int[i++];
    }
    keylist->count_keys = count;
    XFree((XPointer)prop_int);
    ((Ximp_XIM)ic->core.im)->ximp_impart->im_keyslist = keylist;

    keylist_alock = 0;

    keys_id = ((Ximp_XIM)ic->core.im)->ximp_impart->imtype->off_keys_id ;
    if (!keys_id)
	return;
    if( XGetWindowProperty(d, fe_window_id, keys_id, 0L, 1000000L, False,
		keys_id, &actual_type, &actual_format, &nitems,
		&bytes_after, (unsigned char **)(&prop_int)) != Success) {
	return;
    }
    if (!((Ximp_XIM)ic->core.im)->ximp_impart->im_offkeyslist) {
	if((keylist = (Ximp_KeyList *)Xmalloc(sizeof(Ximp_KeyList))) == NULL) {
	    XFree((XPointer)prop_int );
	    return;
	}
	keylist->keys_list = NULL;
	keylist_alock = 1;
    } else {
	keylist = ((Ximp_XIM)ic->core.im)->ximp_impart->im_offkeyslist;
    }
    count = nitems / 3;
    if ((keylist->keys_list) && (count > (int)keylist->count_keys)) {
	XFree(keylist->keys_list);
	keylist->keys_list = NULL;
    }
    if (!keylist->keys_list) {
	if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key) * count)) == NULL) {
	    if (keylist_alock) Xfree(keylist);
	    XFree( (XPointer)prop_int );
	    return;
	}
    }
    for(i=0,n=0; n < count; n++) {
	keylist->keys_list[n].modifier        = prop_int[i++];
	keylist->keys_list[n].modifier_mask   = prop_int[i++];
	keylist->keys_list[n].keysym          = prop_int[i++];
    }
    keylist->count_keys = count;
    XFree((XPointer)prop_int);
    ((Ximp_XIM)ic->core.im)->ximp_impart->im_offkeyslist = keylist;
}

Private void
_Ximp_Ext_ReqKeychangeMsg(ic)
    Ximp_XIC    ic;
{
    XIMXimpRec     *im_impart = ((Ximp_XIM)ic->core.im)->ximp_impart;
    Ximp_ExtXIMRec *ext_im
	= (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if (ext_im->extension_keychange_exist && ic->ximp_icpart->icid) {
    	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_keychange_id, NULL, NULL);
    }
}
#endif /* EXTCONTROLMODE */

#ifdef fujitsu
/*
 * Ximp extension
 *      Get ICID function (XNExtFujitsu_ICID)
 */

Private int
ximp_ext_icid(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
      case XICOpCreate:
	return (False);
      case XICOpSet:
	return(False);
      case XICOpGet:
	    *(long *)value = ic->ximp_icpart->icid ;
	    return(True);
    }
    return (False);
}
#endif /* fujitsu */

/*
 * Ximp extension
 *      Check Function for XIMP_MOVE Protocol
 */

Public int
_XimpIsNeedMoveProtoMode(ic)
    Ximp_XIC        ic;
{
#ifdef EXTCONTROLMODE
#define FX_NO_MOVE 0x1
    int		    command = ic->ximp_icpart->ictype->modechangecommand;
    Ximp_ExtXIMRec *ext_im = 
	(Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if(!(ext_im->extension_modechange_exist )) {
	return (1);
    }
    if((command & FX_NO_MOVE) == FX_NO_MOVE){
	return (0);
    } else
#endif /* EXTCONTROLMODE */
	return (1);
}

/*
 * Following functions are called by Core of Ximp.
 * These are used as fook for extension to Ximp.
 */

#ifndef fujitsu
Public void
_Ximp_CreateIC_InitializeRDB(ic)
    Ximp_XIC	    ic;
{
#ifdef EXTCONTROLMODE
    ic->ximp_icpart->ictype->use_keybindchange = 1;
#endif
    return;
}

Public int
_Ximp_ext_iocpHook(ic, name, op, value)
    Ximp_XIC	    ic;
    char	   *name;
    XICOp_t	    op;
    long	    value;
{
    return False;
}
#endif

Private int
_Ximp_ext_icop(ic, name, op, value)
    Ximp_XIC        ic;
    char           *name;
    XICOp_t         op;
    long            value;
{
    register        i;

    for (i = 0; i < ICOPTBLSIZE; i++) {
	if (name[0] == icoptbl[i].name[0]) {	/* For faster comparison */
	    if (!strcmp(name + 1, icoptbl[i].name + 1)) {
		return ((*(icoptbl[i].func)) (ic, op, value));

	    }
	}
    }
    return _Ximp_ext_iocpHook(ic, name, op, value);
}

/*
 * Resource Extension function is called from XOpenIM().
 */

Public void
_Ximp_OpenIMResourceExtension(im)
    Ximp_XIM        im;
{
    /* Add extension here */
    return;
}

/*
 * Connect Server Extension function is called by XOpenIM().
 */

Public Bool
_XimpConnectServerExtensionHook(im)
    Ximp_XIM        im;
{
    Ximp_ExtXIMRec *ext_im;
    Atom           *atom;
    Atom            back_front;		/* Ximp registered extension */
    Atom            conversion;		/* Ximp registered extension */
    Atom            status_win;		/* Ximp registered extension */
    Atom            lookup = (Atom)NULL;	/* Ximp registered extension */
    Atom	    inputmode = (Atom)NULL;	/* Ximp registered extension */
    Atom	    inputmode_rtn = (Atom)NULL;	/* Ximp registered extension */
    Atom	    modelock = (Atom)NULL;	/* Ximp registered extension */
    Atom	    flush = (Atom)NULL;		/* Ximp registered extension */
    Atom            keychange = (Atom)NULL;	/* Ximp registered extension */
    Atom            modechange = (Atom)NULL;	/* Ximp registered extension */

    /*
     * Create Extension XIM Part (Ximp_ExtXIMRec)
     */
    ext_im = (Ximp_ExtXIMRec *) Xmalloc(sizeof(Ximp_ExtXIMRec));
    if(ext_im == (Ximp_ExtXIMRec *) NULL)
	return False;
    bzero((char *) ext_im, sizeof(Ximp_ExtXIMRec));
    im->ximp_impart->imtype = ext_im;

    /*
     * Extension Initialize
     */
#ifdef fujitsu
#define  _XIMP_EXT_FUJITSU_ENDKEY    "_XIMP_EXT_FUJITSU_ENDKEY"
    /*
     * Fujitsu Extension for XIMP 3.5
     */
    if(!ISXimp4IM(im)) {
	Atom end_keys_id, actual_type;
	Display *dpy = im->core.display;
	Window fe_window_id = im->ximp_impart->fe_window;
	int actual_format, count, n, i, *prop_int;
	unsigned long nitems, bytes_after;
	Ximp_KeyList *offkeylist;

	end_keys_id = XInternAtom(dpy, _XIMP_EXT_FUJITSU_ENDKEY, False);
	if( XGetWindowProperty(dpy, fe_window_id, end_keys_id, 0L, 1000000L,
		False, end_keys_id, &actual_type, &actual_format, &nitems,
		&bytes_after, (unsigned char **)(&prop_int)) == Success) {
	    count = nitems / 3;
	    if(im->ximp_impart->im_offkeyslist){
	        offkeylist = im->ximp_impart->im_offkeyslist;
	    } else {
	        if((offkeylist = (Ximp_KeyList *)Xmalloc(sizeof(Ximp_KeyList)))
			 == NULL) {
		    if(prop_int) XFree( (XPointer)prop_int );
		    return False;
	        }
	        if((offkeylist->keys_list =
		   (Ximp_Key *)Xmalloc(sizeof(Ximp_Key) * count)) == NULL) {
		    Xfree(offkeylist);
		    if(prop_int) XFree( (XPointer)prop_int );
		    return False;
	        }
	    }
	    for(i=0,n=0; n < count; n++) {
	        offkeylist->keys_list[n].modifier        = prop_int[i++];
	        offkeylist->keys_list[n].modifier_mask   = prop_int[i++];
	        offkeylist->keys_list[n].keysym          = prop_int[i++];
	    }
	    offkeylist->count_keys = count;
	    if(prop_int) XFree((XPointer)prop_int);
        } else {
	    if(im->ximp_impart->im_offkeyslist) {
	        if(im->ximp_impart->im_offkeyslist->keys_list) {
		    XFree(im->ximp_impart->im_offkeyslist->keys_list);
	        }
	        XFree(im->ximp_impart->im_offkeyslist);
	    }
	    offkeylist = NULL;
        }
        im->ximp_impart->imtype->off_keys_id = end_keys_id;
        im->ximp_impart->im_offkeyslist = offkeylist;
    }
#endif /* fujitsu */

    /*
     * Backend / Frontend switching
     */
    back_front = XInternAtom(im->core.display,
			     XIMP_EXT_XIMP_BACK_FRONT, True);

    /*
     * Conversion on/off setting/querying
     */
    conversion = XInternAtom(im->core.display,
			     XIMP_EXT_XIMP_CONVERSION, True);

    /*
     * Status Window
     */
    status_win = XInternAtom(im->core.display,
			     XIMP_EXT_XIMP_STATUSWINDOW, True);
#ifdef EXTLOOKUPCALLBACK
    /*
     * Lookup Choice  switching
     */
    lookup = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_LOOKUPCHOICES, True);
#endif /* EXTLOOKUPCALLBACK */

#ifdef EXTCONTROLMODE
    /*
     * Setting mothod
     */
    inputmode = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_INPUTMODE, True);
    inputmode_rtn = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_INPUTMODE_RTN, True);

    /*
     * Locking method
     */
    modelock = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_MODELOCK, True);

    /*
     * Reporting method
     */
    modechange = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_MODECHANGE, True);
    /*
     * Report the keyBinding change
     */
    keychange = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_KEYBINDCHANGE, True);
    /*
     * Flushing Preedit buffer
     */
    flush = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_FLUSH, True);
#endif /* EXTCONTROLMODE */

    for (atom = im->ximp_impart->im_ext_list; *atom; atom++) {
	if (back_front == *atom) {	/* Backend / Frontend */
	    ext_im->extension_back_front_exist = True;
	    ext_im->extension_back_front_id = back_front;

	} else if (conversion == *atom) {
	    ext_im->extension_conversion_exist = True;
	    ext_im->extension_conversion_id = conversion;

	} else if (status_win == *atom) {
	    ext_im->extension_statuswindow_exist = True;
	    ext_im->extension_statuswindow_id = status_win;

	} else if (lookup == *atom) {
	    ext_im->extension_lookup_exist = True;
	    ext_im->extension_lookup_id = lookup;
	    ext_im->extension_lookup_start = XInternAtom(
				 im->core.display,
				   XIMP_EXT_XIMP_LOOKUPSTARTCALLBACK, True);
	    ext_im->extension_lookup_start_rep = XInternAtom(im->core.display,
				 "_XIMP_EXT_XIMP_CHOICE_START_REP", True);

	    ext_im->extension_lookup_proc_rep = XInternAtom(im->core.display,
				 "_XIMP_EXT_XIMP_CHOICE_PROCESS_REP", True);

	} else if (inputmode == *atom) {
	    ext_im->extension_inputmode_exist = True;
	    ext_im->extension_inputmode_id = inputmode;
	} else if (inputmode_rtn == *atom) {
	    ext_im->extension_inputmode_rtn_exist = True;
	    ext_im->extension_inputmode_rtn_id = inputmode_rtn;

	} else if (modelock == *atom) {
	    ext_im->extension_modelock_exist = True;
	    ext_im->extension_modelock_id = modelock;

	} else if (flush == *atom) {
	    ext_im->extension_flush_exist = True;
	    ext_im->extension_flush_id = flush;

	} else if (keychange == *atom) {
	    ext_im->extension_keychange_exist = True;
	    ext_im->extension_keychange_id = keychange;

	} else if (modechange == *atom) {
	    ext_im->extension_modechange_exist = True;
	    ext_im->extension_modechange_id = modechange;

	} else
	    ;/* Add extension here */
    }
    return True;
}

/*
 * Close Server Extension function is called by XCloseIM().
 */

Public void
_XimpConnectServerFreeExtensionHook(im)
    Ximp_XIM        im;
{
    if (im->ximp_impart->imtype)
	Xfree(im->ximp_impart->imtype);
    /* Add extension here */
    return;
}

/*
 * GetIMValues Extension function is called by XGetIMValues().
 */

Public Bool
_Ximp_GetIMValuesExtensionHook(im, name, value)
    Ximp_XIM        im;
    char           *name;
    long            value;
{
    /* Add extension here */
    return (False);
}

/*
 * IC Initialize Extension function is called by XCreateIC().
 */

#ifndef fujitsu
Public Bool
_XimpCreateICVendorHook(ic)
    Ximp_XIC	ic;
{
    return(True);
}
#endif

Public Bool
_XimpCreateICExtensionHook(ic)
    Ximp_XIC        ic;
{
    Ximp_ExtXICRec *ext_ic;
    /*
     * Create Extension XIC Part (Ximp_ExtXICRec)
     */
    ext_ic = (Ximp_ExtXICRec *) Xmalloc(sizeof(Ximp_ExtXICRec));
    if(ext_ic == (Ximp_ExtXICRec *) NULL)
	return False;
    bzero((char *) ext_ic, sizeof(Ximp_ExtXICRec));
    ic->ximp_icpart->ictype = ext_ic;

    /*
     * Extension Initialize
     */
#ifdef EXTCONTROLMODE
    ext_ic->inputmode          = -1;
    ext_ic->inputmode_resource = -1;
#endif /* EXTCONTROLMODE */

    /* Add extension here */

    return _XimpCreateICVendorHook(ic);
}

/*
 * Connect IC Extension function is called by XCreateIC().
 * But this is called after Connection(ICID).
 */

Public void
_Ximp_SetIcValueAfterCreate(ic)
    Ximp_XIC	ic;
{
#ifdef EXTCONTROLMODE
    int		inputmode = ic->ximp_icpart->ictype->inputmode;
    int		lockmode = ic->ximp_icpart->ictype->lockmode_request;

    if (inputmode >= 0) {
	_ximp_ext_set_inputmode(ic, inputmode);
	ic->ximp_icpart->ictype->inputmode = -1;
    }
    if (lockmode) {
	if (_ximp_ext_set_lockmode(ic, lockmode)) {
	    ic->ximp_icpart->ictype->lockmode = lockmode;
	}
    }
#endif
    return;
}

Public void
_XimpAfterCreateConnectionsHook(ic)
    Ximp_XIC        ic;
{
#ifdef EXTCONTROLMODE
    Ximp_ExtXICRec	*ictype = ic->ximp_icpart->ictype;

    if(ictype->inputmode == -1)
	ictype->inputmode = ictype->inputmode_resource;
    if(ictype->lockmode_request == 0)
	ictype->lockmode_request = ictype->lockmode_resource;
    _Ximp_SetIcValueAfterCreate(ic);
#endif
    return;
}

/*
 * _XimpLockedICExtensionCheck() is called by XResetIC()
 * or On/Off Event function.
 */

Public Bool
_XimpLockedICExtensionCheck(ic)
    Ximp_XIC        ic;
{
#ifdef EXTCONTROLMODE
    if (ic->ximp_icpart->ictype->lockmode == XIMP_MUHENKAN) {
	return(True);
     } else if (ic->ximp_icpart->ictype->lockmode &&
		(!(ic->ximp_icpart->ictype->lockmode & XIMP_MUHENKAN))) {
	return(True);
    }
#endif
    return(False);
}

#ifndef fujitsu
/*
 * Connect IC Extension function is called by XCreateIC().
 * But this is called before Connection(ICID).
 */

Public Bool
_XimpBeforeCreateConnectionsHook(ic, values)
    Ximp_XIC         ic;
    XIMArg          *values;
{
    return False;
}

/*
 * Destroy IC (Lose Connection) Extension function.
 * But this is called after protocol of XIMP_DESTROY.
 */

Public void
_XimpAfterLoseConnectionsHook(ic)
    Ximp_XIC        ic;
{
    return;
}

/*
 * Set ClientWindow Extension function is called by XCretaeIC()
 * or XSetICValues().
 * But this is called befoer protocol of XIMP_CLIENT_WINDOW.
 */

Public Ximp_XIC
_XimpBeforeSetClientWindowExtensionHook(ic)
    Ximp_XIC        ic;
{
     return ic;
}

/*
 * Set ClientWindow Extension function is called by XCretaeIC()
 * or XSetICValues().
 * But this is called befoer protocol of XIMP_CLIENT_WINDOW.
 */

Public Ximp_XIC
_XimpAfterSetClientWindowExtensionHook(ic)
    Ximp_XIC        ic;
{
     return ic;
}

/*
 * SetICValues Extension function is called by XSetICValues().
 */

Public Bool
_XimpSetICValuesExtensionHookCheck(ic)
    Ximp_XIC        ic;
{
    return False;
}

Public char *
_XimpSetICValuesExtensionHook(ic, values)
    Ximp_XIC        ic;
    XIMArg          *values;
{
    return ((char *)NULL);
}

/*
 * XSetICFocus Extension function is called by XSetICFocus().
 */

Public Bool
_Ximp_SetFocusExtensionHook(ic)
    Ximp_XIC        ic;
{
    return True;
}

/*
 * XUnSetICFocus Extension function is called by XUnsetICFocus().
 */

Public Bool
_Ximp_UnSetFocusExtensionHook(ic)
    Ximp_XIC        ic;
{
    return True;
}

/*
 * ReconnectIC Extension function is called by KeypressFilter function.
 */

Public Bool
_XimpReconnectICExtensionCheak(ic)
    Ximp_XIC        ic;
{
    return False;
}

Public Bool
_XimpReconnectICExtensionHook(ic)
    Ximp_XIC        ic;
{
    return False;
}

/*
 * IM Server Destroy Extension function is called 
 * by ServerDestroyFilter function.
 */

Public void
_Ximp_ServerDestroyExtensionHook(ic)
    Ximp_XIC        ic;
{
    return;
}
#endif

/*
 * Set IC value Extension function is called by XCreateIC()
 * or XSetICValues().
 */

Public Bool
_Ximp_SetICExtensionHook(ic, name, value, mode)
    Ximp_XIC        ic;
    char           *name;
    long            value;
    int             mode;
{
    if(mode == XIMP_CREATE_IC)
	return _Ximp_ext_icop(ic, name, XICOpCreate, value);
    else
	return _Ximp_ext_icop(ic, name, XICOpSet, value);
}

/*
 * Get IC value Extension function is called by XGetICValues().
 */

Public Bool
_Ximp_GetICExtensionHook(ic, name, value)
    Ximp_XIC        ic;
    char           *name;
    long            value;
{
    return _Ximp_ext_icop(ic, name, XICOpGet, value);
}

/*
 * CreateIC Extension function is called by XCretaeIC().
 * But this is called after protocol of XIMP_CRETAE.
 */


Public void
_Ximp_AfterCreateExtensionHook(ic)
    Ximp_XIC        ic;
{
    Ximp_ExtXIMRec *ext_im
	= (Ximp_ExtXIMRec *)(((Ximp_XIMRec *)ic->core.im)->ximp_impart->imtype);

    /* Backend / Frontend */
    /* For Backward Compatibility */
    if(!ISXimp4(ic)) {
	if (!ISXIMP3FE(ic)) {
		_Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
		ext_im->extension_back_front_id,
		ic->ximp_icpart->svr_mode,
		NULL);
	}
    }

    /* Lookup Choice using check */
    if((ext_im->extension_lookup_exist)) {
	if ( ic->ximp_icpart->ictype->use_lookup_choices ) {
	    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
					ext_im->extension_lookup_id,
					LOOKUP_CHOICES_BEGIN,
					NULL);
	}
    }
#ifdef EXTCONTROLMODE
    /* Reporting method */
    if((ext_im->extension_modechange_exist)) {
	if(ic->ximp_icpart->ictype->use_modechange){
	    XimpConversionPredArgRec  Arg;
	    XEvent event;
	    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION(ic),
					ext_im->extension_modechange_id,
					NULL,
					NULL);
	    Arg.message_type = 
		((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	    Arg.ext_type = ext_im->extension_modechange_id;
	    Arg.icid = ic->ximp_icpart->icid;
	    if(!_XimpIfEvent(ic, &event, ximp_ext_setimodePredicate, 
							(XPointer)&Arg)){
		return;
	    }
	    _Ximp_Ext_ModeChange( ((Ximp_XIM)ic->core.im)->core.display,
				ic->core.focus_window,&event,ic);
	}
    }
    if((ext_im->extension_keychange_exist)) {
    	    if (ic->ximp_icpart->ictype->use_keybindchange)
		_Ximp_Ext_ReqKeychangeMsg(ic);
    }
#endif /* EXTCONTROLMODE */
    /* Add extension here */
    return;
}

/*
 * Callback Extension function is called by XFilterEvent().
 */

Public void
_Ximp_ProcExtension(d, w, ev)
    Display        *d;
    Window          w;
    XClientMessageEvent *ev;
{
    ICID            icid;
    Ximp_XIC        ic;
    Atom            ext_id;
    XIMXimpRec     *im_impart;
    Ximp_ExtXIMRec *ext_im;

    icid = ev->data.l[1];
    ext_id = ev->data.l[2];
    if( (ic = _Ximp_LookupXIC(icid,ev->window)) == NULL )
	return;

    im_impart = ((Ximp_XIM)ic->core.im)->ximp_impart;
    ext_im = (Ximp_ExtXIMRec *) im_impart->imtype;

    if (ext_id == ext_im->extension_lookup_id) {
#ifdef EXTLOOKUPCALLBACK
	if (ext_im->extension_lookup_exist) {
	    _Ximp_ExtLookup(d, w, ev, ic);
	}
	return;
	/* anything to do it? */
#endif
#ifdef EXTCONTROLMODE
    } else if (ext_id == ext_im->extension_keychange_id) {
	_Ximp_Ext_KeyChange(d, w, ev, ic);
    } else if (ext_id == ext_im->extension_modechange_id) {
	_Ximp_Ext_ModeChange(d, w, ev, ic);
#endif
    } else
    /* Add extension here */
        ;
    return;
}

Public void
_Ximp_CallRestartCallbackExtensionHook( xic )
    Ximp_XIC		xic;
{
    register	XIMCallback	*cb;

    cb = &xic->ximp_icpart->ictype->restart;
    if( cb->callback ) {
	(*cb->callback)( xic, cb->client_data, NULL );
    }
}

Public void
_Ximp_CallDestroyCallbackExtensionHook( xic )
    Ximp_XIC		xic;
{
    register	XIMCallback	*cb;

    cb = &xic->ximp_icpart->ictype->destroy;
    if( cb->callback ) {
	(*cb->callback)( xic, cb->client_data, NULL );
    }
}
