/* $XimpImplementGroup: XimpICS.c, v 1.1 94/05/31 21:16:05 $ */
/* $XConsortium: XimpICS.c,v 1.5 92/10/19 19:24:39 rws Exp $ */
/******************************************************************

    Copyright 1991, 1992 by FUJITSU LIMITED.
    Copyright 1991, 1992 by Sun Microsystems, Inc.
    Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of FUJITSU LIMITED, Sun
Microsystems, Inc. and Sony Corporation  not be used in advertising
or publicity pertaining to distribution of the software without
specific, written prior permission.
FUJITSU LIMITED , Sun Microsystems, Inc. and Sony Corporation make no
representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.

FUJITSU LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION DISCLAIM
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU
LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

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

  Author:  Takashi Fujiwara     FUJITSU LIMITED
           Makoto Wakamatsu     Sony Corporation

*/

#define NEED_EVENTS
#include "Xlibint.h"

#include "XimpIm.h"

Private Bool
_Ximp_PNPredicate( d, ev, arg0 )
    Display		*d;
    XEvent		*ev;
    XPointer		 arg0;
{
    XimpPNPredicateArg arg = (XimpPNPredicateArg)arg0;

    if( ev->type == PropertyNotify ) {
	if( ev->xproperty.window == arg->window  &&
	    ev->xproperty.atom == arg->atom  &&
	    ev->xproperty.state == PropertyDelete ) {
	    return(True);
	}
    } else if( ev->type == ClientMessage ) {
	if( ev->xclient.message_type == arg->type  &&
	    ev->xclient.format == 32  &&
	    ev->xclient.data.l[1] == arg->icid  &&
	    ISXIMP_ERROR(ev) )
	    return( True );
    } else if( ev->type == DestroyNotify ) {
	if( ev->xdestroywindow.window == arg->owner ) {
	    return( True );
	}
    }
    return( False );
}

Public Bool
_XimpPNIfEvent( ic, atom )
    Ximp_XIC		ic;
    Atom		atom;
{
    XEvent			event;
    XimpPNPredicateArgRec	Arg;	

    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.icid = ic->ximp_icpart->icid;
    Arg.window = ic->core.client_window;
    Arg.atom = atom;
    XIfEvent( ic->core.im->core.display, &event, _Ximp_PNPredicate, (XPointer)&Arg );
    if( event.type != PropertyNotify ) {
	XPutBackEvent( ic->core.im->core.display, &event );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id );
	if( event.type == DestroyNotify )
	    XPutBackEvent( ic->core.im->core.display, &event );
	else
	    _Ximp_ProcError( ic, ic->core.im->core.display,
					NULL, (XClientMessageEvent *)&event );
	return( False );
    }
    return( True );
}

Public void
_XimpClearPropertyEvent( ic )
    Ximp_XIC	ic;
{
    XEvent			event;
    XimpPNPredicateArgRec	arg;

    arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    arg.icid = ic->ximp_icpart->icid;
    arg.window = ic->core.client_window;
    arg.atom = ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id;
    while( _XimpCheckIfEvent( ic, &event, _Ximp_PNPredicate, (XPointer)&arg ) );

    arg.atom = ((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id;
    while( _XimpCheckIfEvent( ic, &event, _Ximp_PNPredicate, (XPointer)&arg ) );

    arg.atom = ((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id;
    while( _XimpCheckIfEvent( ic, &event, _Ximp_PNPredicate, (XPointer)&arg ) );

    arg.atom = ((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id;
    while( _XimpCheckIfEvent( ic, &event, _Ximp_PNPredicate, (XPointer)&arg ) );
}

Public char *
_Ximp_SetICValues(xic, values)
    XIC		 xic;
    XIMArg	*values;
{
    Ximp_XIC		 ic = (Ximp_XIC)xic;
    char		*ret;
    XimpChangeMaskRec	 change_mask;

    if(_XimpSetICValuesExtensionHookCheck(ic))
	return( _XimpSetICValuesExtensionHook(ic, values) );

    XIMP_SET_NULLMASK(change_mask);

    if(!IS_SERVER_CONNECTED(ic->core.im) && IS_RECONNECTABLE(ic->core.im))
	_Ximp_ConnectServer((Ximp_XIM)ic->core.im);
    if( IS_SERVER_CONNECTED(ic->core.im)  &&  !IS_IC_CONNECTED(ic) )
	if( _Ximp_ConnectIC( ic, XIMP_START_IC ) )
	    if( IS_RESTARTABLE(ic->core.im) )
		_Ximp_CallRestartCallbackExtensionHook( ic );

    if((ret = _Ximp_SetICValueData(ic, values, XIMP_SET_IC, &change_mask)))
	return ret;

    if(   (ic->ximp_icpart->value_mask & XIMP_RES_NAME)
       || (ic->ximp_icpart->value_mask & XIMP_RES_CLASS) )
	_Ximp_SetValue_Resource(ic, &change_mask);
    if(!IS_IC_CONNECTED(ic)) {
	if(XIMP_CHK_PROP_FOCUS(change_mask)) {
	    if(ic->ximp_icpart->filter_mode & 0x1) {
		_XUnregisterFilter (ic->core.im->core.display,
				    ic->ximp_icpart->back_focus_win,
				    _Ximp_XimFilter_Keypress,
				    (XPointer)ic);
		_XUnregisterFilter (ic->core.im->core.display,
				    ic->ximp_icpart->back_focus_win,
				    _Ximp_XimFilter_Keyrelease,
				    (XPointer)ic);
	    }
	    _XRegisterFilterByType (ic->core.im->core.display,
				    ic->core.focus_window,
				    KeyPress, KeyPress,
				    _Ximp_XimFilter_Keypress,
				    (XPointer)ic);
	    _XRegisterFilterByType (ic->core.im->core.display,
				    ic->core.focus_window,
				    KeyRelease, KeyRelease,
				    _Ximp_XimFilter_Keyrelease,
				    (XPointer)ic);
	    ic->ximp_icpart->filter_mode |= 0x1;
	}
	return(ret);
    }

    /* IS_IC_CONNECTED == True */
    if(XIMP_EQU_PRESPOTLMASK(change_mask)) {
	if( IS_BEING_PREEDITED(ic) && _XimpIsNeedMoveProtoMode(ic)) {
	    _Ximp_IM_SendMessage(ic, XIMP_MOVE(ic),
				 ic->ximp_icpart->preedit_attr.SpotLocation.x,
				 ic->ximp_icpart->preedit_attr.SpotLocation.y,
				 NULL);
	}
	return(ret);
    }
    if(XIMP_CHK_PROP_FOCUS(change_mask)) {
	if(ISXimp4(ic)) {
	    _Ximp_SetFocusWindowFilter(ic);
	    XIMP_UNSET_PROPFOCUS(change_mask);
	} else {
	    _Ximp_SetFocusWindowProp(ic);
	    _Ximp_SetFocusWindowFilter(ic);
	}
    }
    if(!(   (ic->core.input_style & XIMPreeditCallbacks)
	 || (ic->core.input_style & XIMPreeditNone) ) ) { 
	if(XIMP_CHK_PROP_PREEDIT(change_mask)) {
	    _Ximp_SetPreeditAtr(ic);
	}
	if(XIMP_CHK_PROP_PREFONT(change_mask)) {
	    _Ximp_SetPreeditFont(ic);
	}
    } else {
	XIMP_UNSET_PROPPREEDIT(change_mask);
    }
    if(!(   (ic->core.input_style & XIMStatusCallbacks)
	 || (ic->core.input_style & XIMStatusNone) ) ) { 
	if(XIMP_CHK_PROP_STATUS(change_mask)) {
	    _Ximp_SetStatusAtr(ic);
	}
	if(XIMP_CHK_PROP_STSFONT(change_mask)) {
	    _Ximp_SetStatusFont(ic);
	}
    } else {
	XIMP_UNSET_PROPSTATUS(change_mask);
    }
    if(XIMP_PROTO_MASK(ic, change_mask)) {
	XWindowAttributes	war;
	long			mask;

	XGetWindowAttributes( ic->core.im->core.display,
			     ic->core.client_window, &war );
	XSelectInput( ic->core.im->core.display, ic->core.client_window,
		     war.your_event_mask | PropertyChangeMask );
	_XimpClearPropertyEvent(ic);
	_Ximp_IM_SendMessage(ic, XIMP_SETVALUE(ic), XIMP_PROTO_MASK(ic,change_mask), NULL, NULL);
	if( XIMP_CHK_PROP_FOCUS(change_mask) )
	    if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id ) ) {
		XIMP_SET_NULLMASK(change_mask);
	    }
	if( XIMP_CHK_PROP_PREEDIT(change_mask) )
	    if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id ) ) {
		XIMP_SET_NULLMASK(change_mask);
	    }
	if( XIMP_CHK_PROP_PREFONT(change_mask) )
	    if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id ) ) {
		XIMP_SET_NULLMASK(change_mask);
	    }
	if( XIMP_CHK_PROP_STATUS(change_mask) )
	    if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id ) ) {
		XIMP_SET_NULLMASK(change_mask);
	    }
	if( XIMP_CHK_PROP_STSFONT(change_mask) )
	    _XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id );
	XSelectInput( ic->core.im->core.display, ic->core.client_window,
		     war.your_event_mask );
    }
    return(ret);
}

Private Bool
_Ximp_PreSetAttributes(ic, attr, vl, mode, change_mask, return_name)
    Ximp_XIC		 	ic;
    Ximp_PreeditPropRec4	*attr;
    XIMArg			*vl;
    int				 mode;
    XimpChangeaMask		 change_mask;
    char			**return_name;
{
    XIMArg			*p;
    XStandardColormap		*colormap_ret;
    int				 list_ret;
    XFontStruct			**struct_list;
    char			**name_list;
    int 			 i, len;
    int				 count;
    char 			*tmp;

    for(p = vl; p && p->name != NULL; p++) {
	if(strcmp(p->name, XNArea)==0) {
	    ic->core.preedit_attr.area.x = ((XRectangle *)p->value)->x;
	    ic->core.preedit_attr.area.y = ((XRectangle *)p->value)->y;
	    ic->core.preedit_attr.area.width = ((XRectangle *)p->value)->width;
	    ic->core.preedit_attr.area.height = ((XRectangle *)p->value)->height;
	    attr->Area.x      = ic->core.preedit_attr.area.x;
	    attr->Area.y      = ic->core.preedit_attr.area.y;
	    attr->Area.width  = ic->core.preedit_attr.area.width;
	    attr->Area.height = ic->core.preedit_attr.area.height;
	    XIMP_SET_PREAREAMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNAreaNeeded)==0) {
	    ic->core.preedit_attr.area_needed.width  = ((XRectangle *)p->value)->width;
	    ic->core.preedit_attr.area_needed.height = ((XRectangle *)p->value)->height;
	    attr->AreaNeeded.width  = ic->core.preedit_attr.area_needed.width;
	    attr->AreaNeeded.height = ic->core.preedit_attr.area_needed.height;
	    XIMP_SET_PREAREANEEDMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNSpotLocation)==0) {
	    ic->core.preedit_attr.spot_location.x = ((XPoint *)p->value)->x;
	    ic->core.preedit_attr.spot_location.y = ((XPoint *)p->value)->y;
	    attr->SpotLocation.x = ic->core.preedit_attr.spot_location.x;
	    attr->SpotLocation.y = ic->core.preedit_attr.spot_location.y;
	    XIMP_SET_PRESPOTLMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNColormap)==0) {
	    ic->core.preedit_attr.colormap = (Colormap)p->value;
	    attr->Colormap = ic->core.preedit_attr.colormap;
	    XIMP_SET_PRECOLORMAPMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNStdColormap)==0) {
	    if( XGetRGBColormaps(ic->core.im->core.display,
				 ic->core.focus_window, &colormap_ret,
				 &count, (Atom)p->value) != 0) {
		ic->core.preedit_attr.std_colormap = (Atom)p->value;
		attr->StdColormap = ic->core.preedit_attr.std_colormap;
		XIMP_SET_PRESTDCOLORMAPMASK(ic, change_mask);
	    } else {
		*return_name = p->name;
		return(False);
	    }
	    
	} else if(strcmp(p->name, XNBackground)==0) {
	    ic->core.preedit_attr.background = (unsigned long)p->value;
	    attr->Background = ic->core.preedit_attr.background;
	    XIMP_SET_PREBGMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNForeground)==0) {
	    ic->core.preedit_attr.foreground = (unsigned long)p->value;
	    attr->Foreground = ic->core.preedit_attr.foreground;
	    XIMP_SET_PREFGMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNBackgroundPixmap)==0) {
	    ic->core.preedit_attr.background_pixmap = (Pixmap)p->value;
	    attr->Bg_Pixmap = ic->core.preedit_attr.background_pixmap;
	    XIMP_SET_PREBGPIXMAPMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNFontSet)==0) {
	    ic->core.preedit_attr.fontset = (XFontSet)p->value;
	    if(p->value != NULL) {
		if(ic->ximp_icpart->preedit_font)
		    Xfree(ic->ximp_icpart->preedit_font);
		list_ret = XFontsOfFontSet(ic->core.preedit_attr.fontset,
					   &struct_list, &name_list);
		for(i = 0, len = 0; i < list_ret; i++) {
		    len += (strlen(name_list[i]) + sizeof(char));
		}
		if((tmp = Xmalloc(len + list_ret + sizeof(char))) == NULL) {
		    *return_name = p->name;
		    return False ;
		}
		tmp[0] = NULL;
		for(i = 0; i < list_ret; i++) {
		    strcat(tmp, name_list[i]);
		    strcat(tmp, ",");
		}
		tmp[len - 1] = NULL;
		ic->ximp_icpart->preedit_font = tmp;
		XIMP_SET_PREFONTMASK(ic, change_mask);
	    } else {
		*return_name = p->name;
		return(False);
	    }
	    
	} else if(strcmp(p->name, XNLineSpace)==0) {
	    ic->core.preedit_attr.line_spacing = (long)p->value;
	    attr->LineSpacing = ic->core.preedit_attr.line_spacing;
	    XIMP_SET_PRELINESPMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNCursor)==0) {
	    ic->core.preedit_attr.cursor = (Cursor)p->value;
	    attr->Cursor = ic->core.preedit_attr.cursor;
	    XIMP_SET_PRECURSORMASK(ic, change_mask);
	    
	} else if(strcmp(p->name, XNPreeditStartCallback)==0) {
	    ic->core.preedit_attr.start_callback.client_data =
		((XIMCallback *)p->value)->client_data;
	    ic->core.preedit_attr.start_callback.callback =
		((XIMCallback *)p->value)->callback;
	    ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
	    
	} else if(strcmp(p->name, XNPreeditDoneCallback)==0) {
	    ic->core.preedit_attr.done_callback.client_data =
		((XIMCallback *)p->value)->client_data;
	    ic->core.preedit_attr.done_callback.callback =
		((XIMCallback *)p->value)->callback;
	    ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
	    
	} else if(strcmp(p->name, XNPreeditDrawCallback)==0) {
	    ic->core.preedit_attr.draw_callback.client_data =
		((XIMCallback *)p->value)->client_data;
	    ic->core.preedit_attr.draw_callback.callback =
		((XIMCallback *)p->value)->callback;
	    ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
	    
	} else if(strcmp(p->name, XNPreeditCaretCallback)==0) {
	    ic->core.preedit_attr.caret_callback.client_data =
		((XIMCallback *)p->value)->client_data;
	    ic->core.preedit_attr.caret_callback.callback =
		((XIMCallback *)p->value)->callback;
	    ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
	}
    }
    return(True);
}

Private Bool
_Ximp_StatusSetAttributes(ic, attr, vl, mode, change_mask, return_name)
    Ximp_XIC			 ic;
    Ximp_StatusPropRec4		*attr;
    XIMArg			*vl;
    int				 mode;
    XimpChangeaMask		 change_mask;
    char			**return_name;
{
	XIMArg			*p;
	XStandardColormap 	*colormap_ret;
	int			 list_ret;
	XFontStruct		**struct_list;
	char			**name_list;
	int 			 i, len;
	int			 count;
	char 			*tmp;

	for(p = vl; p && p->name != NULL; p++) {
		if(strcmp(p->name, XNArea)==0) {
			ic->core.status_attr.area.x = ((XRectangle *)p->value)->x;
			ic->core.status_attr.area.y = ((XRectangle *)p->value)->y;
			ic->core.status_attr.area.width = ((XRectangle *)p->value)->width;
			ic->core.status_attr.area.height = ((XRectangle *)p->value)->height;
			attr->Area.x      = ic->core.status_attr.area.x;
			attr->Area.y      = ic->core.status_attr.area.y;
			attr->Area.width  = ic->core.status_attr.area.width;
			attr->Area.height = ic->core.status_attr.area.height;
			XIMP_SET_STSAREAMASK(ic, change_mask);
		} else if(strcmp(p->name, XNAreaNeeded)==0) {
			ic->core.status_attr.area_needed.width  = ((XRectangle *)p->value)->width;
			ic->core.status_attr.area_needed.height = ((XRectangle *)p->value)->height;
			attr->AreaNeeded.width  = ic->core.status_attr.area_needed.width;
			attr->AreaNeeded.height = ic->core.status_attr.area_needed.height;
			XIMP_SET_STSAREANEEDMASK(ic, change_mask);

		} else if(strcmp(p->name, XNColormap)==0) {
			ic->core.status_attr.colormap = (Colormap)p->value;
			attr->Colormap = ic->core.status_attr.colormap;
			XIMP_SET_STSCOLORMAPMASK(ic, change_mask);

		} else if(strcmp(p->name, XNStdColormap)==0) {
			if(XGetRGBColormaps(ic->core.im->core.display,
					ic->core.focus_window, &colormap_ret,
					&count, (Atom)p->value) !=0) {
				ic->core.status_attr.std_colormap = (Atom)p->value;
				attr->StdColormap = ic->core.status_attr.std_colormap;
				XIMP_SET_STSSTDCOLORMAPMASK(ic, change_mask);
			} else {
				*return_name = p->name;
				return(False);
			}

		} else if(strcmp(p->name, XNBackground)==0) {
			ic->core.status_attr.background = (unsigned long)p->value;
			attr->Background = ic->core.status_attr.background;
			XIMP_SET_STSBGMASK(ic, change_mask);

		} else if(strcmp(p->name, XNForeground)==0) {
			ic->core.status_attr.foreground = (unsigned long)p->value;
			attr->Foreground = ic->core.status_attr.foreground;
			XIMP_SET_STSFGMASK(ic, change_mask);

		} else if(strcmp(p->name, XNBackgroundPixmap)==0) {
			ic->core.status_attr.background_pixmap = (Pixmap)p->value;
			attr->Bg_Pixmap = ic->core.status_attr.background_pixmap;
			XIMP_SET_STSBGPIXMAPMASK(ic, change_mask);

		} else if(strcmp(p->name, XNFontSet)==0) {
			ic->core.status_attr.fontset = (XFontSet)p->value;
			if (p->value != NULL) {
				if(ic->ximp_icpart->status_font)
					Xfree(ic->ximp_icpart->status_font);
				list_ret = XFontsOfFontSet(ic->core.status_attr.fontset,
								&struct_list, &name_list);
				for(i = 0, len = 0; i < list_ret; i++) {
					len += (strlen(name_list[i]) + sizeof(char));
				}
				if((tmp = Xmalloc(len + list_ret + sizeof(char))) == NULL){
				    *return_name = p->name;
				    return False ;
				}
				tmp[0] = NULL;
				for(i = 0; i < list_ret; i++) {
					strcat(tmp, name_list[i]);
					strcat(tmp, ",");
				}
				tmp[len - 1] = NULL;
				ic->ximp_icpart->status_font = tmp;
				XIMP_SET_STSFONTMASK(ic, change_mask);
			} else {
				*return_name = p->name;
				return(False);
			}

		} else if(strcmp(p->name, XNLineSpace)==0) {
			ic->core.status_attr.line_spacing = (long)p->value;
			attr->LineSpacing = ic->core.status_attr.line_spacing;
			XIMP_SET_STSLINESPMASK(ic, change_mask);

		} else if(strcmp(p->name, XNCursor)==0) {
			ic->core.status_attr.cursor = (Cursor)p->value;
			attr->Cursor = ic->core.status_attr.cursor;
			XIMP_SET_STSCURSORMASK(ic, change_mask);

		} else if(strcmp(p->name, XNStatusStartCallback)==0) {
			ic->core.status_attr.start_callback.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.status_attr.start_callback.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_STS_CALLBAK;

		} else if(strcmp(p->name, XNStatusDoneCallback)==0) {
			ic->core.status_attr.done_callback.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.status_attr.done_callback.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_STS_CALLBAK;

		} else if(strcmp(p->name, XNStatusDrawCallback)==0) {
			ic->core.status_attr.draw_callback.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.status_attr.draw_callback.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_STS_CALLBAK;
		}
	}

	return(True);
}

#ifndef fujitsu
Public Bool
_XimpSetFocusWindowCheck(ic)
    Ximp_XIC	 ic;
{
    return True;
}
#endif

Public char *
_Ximp_SetICValueData(ic, values, mode, change_mask)
    Ximp_XIC			 ic;
    XIMArg			*values;
    int				 mode;
    XimpChangeaMask		 change_mask;
{
    XIMArg			*p;
    char			*return_name = NULL;
    XimpCMPredicateArgRec        Arg;			/* for Ximp4.0 */
    XEvent			 event;			/* for Ximp4.0 */

    for(p = values; p->name != NULL; p++) {
	if(strcmp(p->name, XNInputStyle) == 0) {
	    if(mode == XIMP_CREATE_IC) {
		ic->core.input_style = (XIMStyle)p->value;
		ic->ximp_icpart->value_mask |= XIMP_INPUT_STYLE;
	    } else {
		; /* Currently Fixed value */
	    }
	} else if(strcmp(p->name, XNClientWindow)==0) {
	    if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
		Window	client_window_old;
		unsigned long	dummy_mask;

		if(mode == XIMP_SET_IC && (IS_IC_CONNECTED(ic))) {
		    ic = _XimpBeforeSetClientWindowExtensionHook(ic);
		    if(ISXimp4(ic)) {
			_Ximp_GetFocusWindowSelectMask(ic, (Window)p->value, &dummy_mask);
			_Ximp_IM_SendMessage(ic, XIMP_CLIENT_WINDOW(ic), (Window)p->value, dummy_mask, NULL);
			Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
			Arg.icid = ic->ximp_icpart->icid;
			Arg.request = XIMP_CLIENT_WINDOW(ic);
			Arg.protocol = XIMP_CLIENT_WINDOW_RETURN(ic);
			Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
			if(!_XimpIfEvent(ic, &event, _Ximp_CMPredicate32, (XPointer)&Arg))
			    return NULL;
			client_window_old = ic->core.client_window;
			ic->core.client_window = (Window)p->value;
			ic->ximp_icpart->value_mask |= XIMP_CLIENT_WIN;
		    } else {
			long	icid_old, icid_new;

			icid_old = ic->ximp_icpart->icid;
			client_window_old = ic->core.client_window;
			ic->ximp_icpart->value_mask |= XIMP_CLIENT_WIN;
			ic->core.client_window = (Window)p->value;
			if(_Ximp_ConnectIC(ic, mode) == False) {
			    return_name = p->name;
			    ic->ximp_icpart->value_mask &= ~XIMP_CLIENT_WIN;
			    ic->core.client_window = client_window_old;
			    break;
			}
			icid_new = ic->ximp_icpart->icid;
			ic->ximp_icpart->icid = icid_old;
			_Ximp_IM_SendMessage(ic, XIMP_DESTROY(ic), NULL, NULL, NULL);
			ic->ximp_icpart->icid = icid_new;
			XDestroyWindow(ic->core.im->core.display, client_window_old);
			XIMP_SET_NULLMASK2(change_mask);
		    }
		    ic = _XimpAfterSetClientWindowExtensionHook(ic);
		} else { /* XIMP_CREATE_IC | (XIMP_SET_IC && XIMP_START_IC) */
		    client_window_old = ic->core.client_window;
		    ic->core.client_window = (Window)p->value;
		    ic->ximp_icpart->value_mask |= XIMP_CLIENT_WIN;
		    if(!(XIMP_CHK_FOCUSWINMASK(ic))) {
			ic->core.focus_window = ic->core.client_window;
			XIMP_SET_FOCUSWINMASK(ic);
		    }
		}
		if( ic->ximp_icpart->filter_mode & 0x4 ) {
		    _XRegisterFilterByType(ic->core.im->core.display,
					ic->core.client_window,
					ClientMessage, ClientMessage,
					_Ximp_XimFilter_Client,
					(XPointer)ic );
		}
	    } else {
		return_name = p->name;
		break; /* Can't change this value */
	    }
	} else if(strcmp(p->name, XNFocusWindow)==0) {
	    if(IS_IC_CONNECTED(ic) && (mode == XIMP_SET_IC)) {
		Window		new_focus_window = (Window)p->value;
		unsigned long	dummy_mask;
		unsigned long	temp_mask;
		XWindowAttributes	wattr;

		if( _XimpSetFocusWindowCheck(ic) ) {
		  if(ISXimp4(ic)) {
		    XGetWindowAttributes(ic->core.im->core.display, new_focus_window, &wattr);
		    dummy_mask = wattr.your_event_mask;
		    temp_mask = dummy_mask;
		    if(ISFE2(ic) || (ISFE1(ic) && IS_BEING_PREEDITED(ic)))
			dummy_mask &= ~(KeyPressMask | KeyReleaseMask);                         
		    else if(IS_FORCESELECTKEYRELEASE(ic->core.im) && (ISBE2(ic)||(ISBE1(ic)&&IS_BEING_PREEDITED(ic))))
			dummy_mask |= (KeyReleaseMask);
		    _Ximp_IM_SendMessage(ic, XIMP_FOCUS_WINDOW(ic), new_focus_window, temp_mask, NULL);
		    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
		    Arg.icid = ic->ximp_icpart->icid;
		    Arg.request = XIMP_FOCUS_WINDOW(ic);
		    Arg.protocol = XIMP_FOCUS_WINDOW_RETURN(ic);
		    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
		    if(!_XimpIfEvent(ic, &event, _Ximp_CMPredicate32, (XPointer)&Arg))
			return NULL;
		    XSelectInput(ic->core.im->core.display, new_focus_window, dummy_mask);
		    if(ic->ximp_icpart->back_focus_win)
			XSelectInput(ic->core.im->core.display, ic->core.focus_window,
				     ic->ximp_icpart->back_mask);
		    ic->ximp_icpart->back_mask = temp_mask;
		  } else { /* Ximp 3.5 */
		    if(IS_BEING_PREEDITED(ic)) {
			if(ISXIMP3FE(ic)) {
			    XSelectInput(ic->core.im->core.display, ic->core.focus_window,
					 ic->ximp_icpart->back_mask);
			    XGetWindowAttributes(ic->core.im->core.display, new_focus_window, &wattr);
			    dummy_mask = wattr.your_event_mask;
			    ic->ximp_icpart->back_mask = dummy_mask;
			    if(ISXIMP3FE(ic)) {
				dummy_mask &= ~(KeyPressMask | KeyReleaseMask);
			    }
			    XSelectInput(ic->core.im->core.display, new_focus_window, dummy_mask);
			}
		    }
		  }
	        }
		ic->ximp_icpart->back_focus_win = ic->core.focus_window;
		ic->core.focus_window = (Window)p->value;
		XIMP_SET_FOCUSWINMASK2(ic, change_mask);
	    } else {
		ic->ximp_icpart->back_focus_win = ic->core.focus_window;
		ic->core.focus_window = (Window)p->value;
	        if(mode == XIMP_CREATE_IC) {
		    XIMP_SET_FOCUSWINMASK2(ic, change_mask);
		}
	    }
        } else if(strcmp(p->name, XNResourceName)==0) {
            char *tmp;
            tmp = (char *)Xmalloc(strlen((char *)p->value)+1);
            strcpy(tmp,(char *)p->value);
            if(ic->ximp_icpart->res_name) Xfree(ic->ximp_icpart->res_name);
            ic->ximp_icpart->res_name =  tmp ;
            ic->ximp_icpart->value_mask |= XIMP_RES_NAME;
        } else if(strcmp(p->name, XNResourceClass)==0) {
            char *tmp;
            tmp = (char *)Xmalloc(strlen((char *)p->value)+1);
            strcpy(tmp,(char *)p->value);
            if(ic->ximp_icpart->res_class) Xfree(ic->ximp_icpart->res_class);
            ic->ximp_icpart->res_class =  tmp ;
            ic->ximp_icpart->value_mask |= XIMP_RES_CLASS;
	} else if(strcmp(p->name, XNGeometryCallback)==0) {
	    ic->core.geometry_callback.client_data =
		((XIMCallback *)p->value)->client_data;
	    ic->core.geometry_callback.callback =
		((XIMCallback *)p->value)->callback;
	    ic->ximp_icpart->value_mask |= XIMP_GEOMETRY_CB;
	} else if(strcmp(p->name, XNPreeditAttributes)==0) {
	    if( _Ximp_PreSetAttributes(ic,
				       &(ic->ximp_icpart->preedit_attr),
				       p->value, mode, change_mask,
				       &return_name) == False ){
		 break ;
	    }
	} else if(strcmp(p->name, XNStatusAttributes)==0) {
	    if( _Ximp_StatusSetAttributes(ic,
					  &(ic->ximp_icpart->status_attr),
					  p->value, mode, change_mask,
					  &return_name) == False ){
		 break ;
            }
	} else {
	    if( _Ximp_SetICExtensionHook(ic, p->name,
			 (long)p->value, mode) == False ) {
		return_name = p->name;
		break;
	    }
	}
    }
    return(return_name);
}
