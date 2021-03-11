/* $XimpImplementGroup: XimpLkup.c, v 1.1 94/05/31 21:16:06 $ */
/* $XConsortium: XimpLkup.c,v 1.9 92/10/19 19:24:57 rws Exp $ */
/******************************************************************
  
Copyright 1991, 1992 by Sony Corporation
Copyright 1991, 1992 by FUJITSU LIMITED
Copyright 1991, 1992 by Fuji Xerox Co.,Ltd.
Copyright 1991, 1992 by Sun Microsystems, Inc.
  
Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Sony Corporation,
FUJITSU LIMITED, Fuji Xerox Co.,Ltd. and Sun Microsystems, Inc. not 
be used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
Sony Corporation, FUJITSU LIMITED and Fuji Xerox Co.,Ltd. make no
representations about the suitability of this software for any purpose.
It is provided "as is" without express or implied warranty.
  
SONY CORPORATION, FUJITSU LIMITED FUJI XEROX CO.,LTD. AND SUN MICROSYSTEMS,
INC. DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
SONY CORPORATION, FUJITSU LIMITED, FUJI XEROX CO.,LTD. BE LIABLE FOR
ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  
  Author: Masaki Takeuchi      Sony Corporation
          Takashi Fujiwara     FUJITSU LIMITED 
          Kazunori Nishihara   Fuji Xerox Co.,Ltd.
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
#include "Xlibint.h"
#include <X11/Xatom.h>
#include "Xlibnet.h"

#include "XimpIm.h"

#include <netinet/in.h>

Public Bool
_XimpIfEvent( ic, event, predicate, arg )
    Ximp_XIC		 ic;
    XEvent		*event;
    Bool		(*predicate)(
#if NeedNestedPrototypes
	Display*,
	XEvent*,
	char*
#endif
);
    char		*arg;
{
    XIfEvent( ic->core.im->core.display, event, predicate, arg );
    if( event->type == DestroyNotify ) {
	XPutBackEvent( ic->core.im->core.display, event );
	return( False );
    } else if(ISXIMP_ERROR(event)) {
	_Ximp_ProcError( ic, ic->core.im->core.display,
					NULL, (XClientMessageEvent *)event );
	return( False );
    }
    return( True );
}

Public Bool
_XimpCheckIfEvent( ic, event, predicate, arg )
    Ximp_XIC		 ic;
    XEvent		*event;
    Bool		(*predicate)(
#if NeedNestedPrototypes
	Display*,
	XEvent*,
	char*
#endif
);
    char		*arg;
{
    if( !XCheckIfEvent( ic->core.im->core.display, event, predicate, arg ) ) {
	event->type = LASTEvent;
	return( False );
    }
    if( event->type == DestroyNotify ) {
	XPutBackEvent( ic->core.im->core.display, event );
	return( False );
    } else if(ISXIMP_ERROR(event)) {
	_Ximp_ProcError( ic, ic->core.im->core.display,
					NULL, (XClientMessageEvent *)event );
	return( False );
    }
    return( True );
}

Private Bool
_Ximp_CMPredicateAll(d, ev, arg0)
    Display *d;
    XEvent *ev;
    XPointer arg0;
{
    XimpCMPredicateArg arg = (XimpCMPredicateArg)arg0;
    ICID	icid;
    long	nicid;

    if(ev->type == ClientMessage ) {
	if(ev->xclient.message_type == arg->type) {
	    if(ev->xclient.format == 32) {
		if(arg->icid == 0 || ISCMOf(ev,1,arg->icid))
		    return(True);
	    } else {
		nicid = *(unsigned long *)(&(ev->xclient.data.b[0]));
		icid = (ICID)ntohl(nicid);
		if (icid == arg->icid)
		    return(True);
	    }
	}
    } else if( ev->type == DestroyNotify) {
	if( ev->xdestroywindow.window == arg->owner ) {
	    return( True );
	}
    }
    return( False );
}

Public Bool
_Ximp_CMPredicate32(d, ev, arg0)
    Display *d;
    XEvent *ev;
    XPointer arg0;
{
    XimpCMPredicateArg arg = (XimpCMPredicateArg)arg0;

    if(ev->type == ClientMessage ) {
	if(ev->xclient.message_type == arg->type &&
	   ev->xclient.format == 32 ) {
	    if(arg->icid == 0 || ISCMOf(ev,1,arg->icid))
	      if(ISCMOf(ev,0,arg->protocol) ||
		 ISXIMP_ERROR(ev))
		return(True);
	}
    } else if( ev->type == DestroyNotify) {
	if( ev->xdestroywindow.window == arg->owner ) {
	    return( True );
	}
    }
    return( False );
}

Private Bool
_Ximp_CMPredicate8(d, ev, arg0)
    Display *d;
    XEvent *ev;
    XPointer arg0;
{
    ICID	icid;
    long	nicid;
    XimpCMPredicateArg arg = (XimpCMPredicateArg)arg0;

    if (ev->type == ClientMessage) {
	if (ev->xclient.message_type == arg->type) {
	    if (ev->xclient.format == 8) {
		nicid = *(unsigned long *)(&(ev->xclient.data.b[0]));
		icid = (ICID)ntohl(nicid);
		icid = (ICID)(ev->xclient.data.l[0]);
		if (icid == arg->icid)
		  return(True);
	    } else if(arg->icid == 0 || ISCMOf(ev,1,arg->icid))
		if(ISXIMP_ERROR(ev))
		    return(True);
	}
    } else if (ev->type == DestroyNotify) {
	if (ev->xdestroywindow.window == arg->owner) {
	    return(True);
	}
    }
    return(False);
}

Private Bool
combine_multiple_cm( ev, buf, len )
    XClientMessageEvent	*ev;
    unsigned char	**buf;
    int			*len;
{
    register int	i;
    unsigned char	*p;
    register int	rlen = (int)((unsigned char)ev->data.b[4]);
    register int	ct_len;

    ct_len = 5 + ((rlen <= CT_MAX_IN_CM) ? rlen : CT_MAX_IN_CM);

    if( *buf == NULL ) {
	if( (*buf = (unsigned char *)Xmalloc(rlen + 1)) == NULL )
	    return( False );
	*len = rlen;
    }
    for( p = *buf + *len - rlen, i = 5; i < ct_len; i++ )
	*p++ = ev->data.b[i];
    *p = '\0';
    return((rlen <= CT_MAX_IN_CM) ? False : True);
}

Public int
_Ximp_CombineMultipleCM( ic, ct )
    Ximp_XIC		ic;
    unsigned char	**ct;
{
    XimpCMPredicateArgRec	Arg;
    int				len = 0;
    XEvent			ev;

    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.icid = ic->ximp_icpart->icid;

    *ct = NULL;
    while( True ) {
	if( !_XimpIfEvent( ic, &ev, _Ximp_CMPredicate8, (XPointer)&Arg ) ) {
	    if( len != 0 ) {
		XFree( *ct );
		*ct = NULL;
	    }
	    break;
	}
	if( !combine_multiple_cm( &ev, ct, &len ) ) {
	    if( len == 0  &&  ev.xclient.data.b[4] != 0 ) {
		for(;;) {
		    if( _XimpCheckIfEvent( ic, &ev, _Ximp_CMPredicate8,
				      (XPointer)&Arg ) ) {
			if( (unsigned)ev.xclient.data.b[4] <= CT_MAX_IN_CM )
			    break;
		    }
		    else {
			if( ev.type != LASTEvent )
			    break;
		    }
		}
	    }
	    break;
	}
    }
    return( len );
}

Public void
_Ximp_ServerProcessStopped(d, w, ev)
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
{
    ICID		icid = ev->data.l[1];
    Ximp_XIC            ic = _Ximp_LookupXIC(icid, ev->window);
    Bool		is_SelectInput_needed = False;

    if(!ic) return;

    ic->ximp_icpart->input_mode = BEING_BYPASSED;

    if(ISXimp4(ic)) {
	if(ISFE1(ic))
	    is_SelectInput_needed = True;
    } else if(ISXIMP3FE(ic)) {
	    is_SelectInput_needed = True;
    }
    if(is_SelectInput_needed ) {
	unsigned long               dummy_mask;
	XWindowAttributes           ret_attributes;

	XGetWindowAttributes(d, ic->core.focus_window, &ret_attributes);
	dummy_mask = ret_attributes.your_event_mask;
	ic->ximp_icpart->back_mask = dummy_mask;
	if(ISXimp4(ic)) {
	    if(ISFE1(ic))
		dummy_mask |= (KeyPressMask|KeyReleaseMask);
	    else
		    ;
	} else if(ISXIMP3FE(ic)) {
            dummy_mask |= (KeyPressMask|KeyReleaseMask);
	}
	XSelectInput(ic->core.im->core.display, ic->core.focus_window,
	                     dummy_mask);
	XFlush(d);
    }
    return;
}

#define DEFAULT_QUE_SIZE (16)
#define QUE_SIZE_INCREMENT (4)

Private void
_Ximp_Q_align(ic, Arg, enque_proto)
    Ximp_XIC ic;
    XimpCMPredicateArgRec	*Arg;
    Bool enque_proto; /* Flag for enqueing the sync protocol */
{
    static XEvent	*ev_que = NULL;
    static int		que_size = 0;
    int			qued = 0;
    XEvent		ev_buf, *event_return = &ev_buf, *new;

    if (ev_que == (XEvent *)NULL) {
	if (ev_que = (XEvent *)Xcalloc(DEFAULT_QUE_SIZE, sizeof(XEvent))) {
	    que_size = DEFAULT_QUE_SIZE;
	}
    }
    while (True) {
	XIfEvent(ic->core.im->core.display, event_return, _Ximp_CMPredicateAll, (char *)Arg);
	if( event_return->type == DestroyNotify ) {
	    XPutBackEvent( ic->core.im->core.display, event_return );
	    break;
	} else if(ISXIMP_ERROR(event_return)) {
	    XPutBackEvent( ic->core.im->core.display, event_return );
	    break;
	} else if (event_return->type == ClientMessage) {
	    if(event_return->xclient.message_type == Arg->type &&
	       event_return->xclient.format == 32 ) {
		if(Arg->icid == 0 || ISCMOf(event_return,1,Arg->icid))
		  if(ISCMOf(event_return,0,Arg->protocol)) {
		    if (enque_proto)
		      XPutBackEvent( ic->core.im->core.display, event_return );
		    break;
		  }
	    }
	}
	if (qued == que_size) { /* Que is full */
	    if (new = (XEvent *)Xrealloc(ev_que, sizeof(XEvent) * (que_size + QUE_SIZE_INCREMENT))) {
		ev_que = new;
		que_size += QUE_SIZE_INCREMENT;
	    }
	}
	/* If not enough memory for ev_que, some protocol dropped */
	if (qued == que_size) continue;
	/* Enque */
	bcopy(event_return, &ev_que[qued], sizeof(XEvent));
	qued++;
    }
    /* Deque */
    while (qued > 0) {
	qued--;
	XPutBackEvent(ic->core.im->core.display, &ev_que[qued]);
    }
}

Private unsigned char *
_Ximp_Reset( ic, length )
    Ximp_XIC	ic;
    int		*length;
{
    XEvent		 	Message;
    XEvent		 	event;
    XimpCMPredicateArgRec	Arg;
    int				rval;
    Atom			actual_type_return;
    int				actual_format_return;
    unsigned long		nitems;
    unsigned long		bytes_after_return;
    unsigned char		*p = NULL;

    *length = 0;
    if(!(IS_IC_CONNECTED(ic) && IS_BEING_PREEDITED(ic)))
	return( (unsigned char *)NULL );

    if( _XimpLockedICExtensionCheck(ic) )
	return( (unsigned char *)NULL );

    /* ClientMessage Send */
    _Ximp_IM_SendMessage( ic, XIMP_RESET(ic), NULL, NULL, NULL );

    if(ISXimp4(ic) && !ISTYPE2(ic)) {
	Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	Arg.protocol = XIMP_SPROC_STOPPED(ic);
	Arg.icid = ic->ximp_icpart->icid;
	if(_XimpIfEvent(ic, &event, _Ximp_CMPredicate32, (XPointer)&Arg)) {
	    _Ximp_ServerProcessStopped(((Ximp_XIM)ic->core.im)->core.display,
					ic->core.focus_window,
					(XClientMessageEvent *)&event);
	}
    }
    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.protocol = XIMP_RESET_RETURN(ic);
    Arg.icid = ic->ximp_icpart->icid;
    _Ximp_Q_align(ic, &Arg, True);
    if( !_XimpIfEvent( ic, &event, _Ximp_CMPredicate32, (XPointer)&Arg ) )
	return( NULL );
    if( ISXimp4(ic) ) {
	switch( event.xclient.data.l[XIMP4_RESET_RETURN_DETAIL] ){
	  default:
	    break;

	  case NO_RESET_DATA:
	    return( NULL );
	    break;

	  case RESET_DATA_VIA_CM:
	    *length = _Ximp_CombineMultipleCM( ic, &p );
	    return( p );
	    break;

	  case RESET_DATA_VIA_PROP:
	    if( XGetWindowProperty( ic->core.im->core.display,
		      ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window,
		      (Atom)event.xclient.data.l[XIMP_RESET_RETURN_ATOM(ic)],
		      0L, 1000000L, True, AnyPropertyType, &actual_type_return,
		      &actual_format_return, &nitems,
		      &bytes_after_return, &p) == Success ) {
		*length = nitems;
		return( p );
	    }
	    break;
	}
    }
    else {
	if( XGetWindowProperty( ic->core.im->core.display,
		((Ximp_XIM)ic->core.im)->ximp_impart->fe_window,
		(Atom)event.xclient.data.l[XIMP_RESET_RETURN_ATOM(ic)],
		0L, 1000000L, True, AnyPropertyType, &actual_type_return,
		&actual_format_return, &nitems,
		&bytes_after_return, &p ) == Success ) {
	    *length = nitems;
	    return( p );
	}
    }
    return( NULL );
}

Public char *
_Ximp_MbReset( xic )
    XIC			 xic;
{
    Ximp_XIC		 ic = (Ximp_XIC)xic;
    Ximp_XIM		 im = (Ximp_XIM)ic->core.im;
    char		*mb;
    int			 length = 0;
    unsigned char	*ct = _Ximp_Reset( ic, &length );

    if( !ct )
	return( NULL );
    if( (mb = Xmalloc(length+1)) == NULL ) {
	Xfree( ct );
	return( NULL );
    }
    length = _Ximp_ctstombs(im, (char *)ct,
					strlen((char *)ct), mb, length, NULL);
    Xfree( ct );
    mb[length] = '\0';
    return( mb );
}

Public wchar_t *
_Ximp_WcReset( xic )
    XIC			 xic;
{
    Ximp_XIC		 ic = (Ximp_XIC)xic;
    Ximp_XIM		 im = (Ximp_XIM)ic->core.im;
    wchar_t		*wc;
    int			 length = 0;
    unsigned char	*ct = _Ximp_Reset( ic, &length );

    if( !ct )
	return( NULL );
    if( (wc = (wchar_t *)Xmalloc((length+1) * sizeof(wchar_t))) == NULL ) {
	Xfree( ct );
	return( NULL );
    }
    length = _Ximp_ctstowcs(im, (char *)ct,
				strlen((char *)ct), wc, length, NULL);
    Xfree( ct );
    wc[length] = (wchar_t)0;
    return( wc );
}

#define LookupNothing   0
#define	LookupKeypress	1
#define LookupProperty	2
#define LookupMessage	3

static int		 _xim_lookup_sign  = LookupNothing;
static unsigned long	 _xim_message_len  = 0;
static unsigned char	*_xim_message_buff = (unsigned char *)NULL;
static Ximp_XIC		 _xim_message_ic   = (Ximp_XIC)NULL;

Public int
_Ximp_MbLookupString(xic, ev, buffer, bytes, keysym, status)
    XIC			 xic;
    XKeyEvent		*ev;
    char		*buffer;
    int			 bytes;
    KeySym		*keysym;
    Status		*status;
{
    Ximp_XIC		 ic = (Ximp_XIC)xic;
    Ximp_XIM		 im = (Ximp_XIM)ic->core.im;
    XComposeStatus	*comp_status = NULL;
    int			 ret = 0, len;
    unsigned char	*s;
    int			 str_len;
    Status		 sts;

    if(ev->type == KeyPress && ev->keycode == 0) { /* Filter function */
	if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
	    if(status) *status = XLookupNone;
	    _xim_lookup_sign = LookupNothing;
	    _xim_message_len = 0;
	    return(0);
	}
	if ( _xim_message_ic == ic &&
	   ((_xim_lookup_sign == LookupProperty)
				 || (_xim_lookup_sign == LookupMessage))) {
	    s = _xim_message_buff;
	    str_len = _xim_message_len;
	    if ((len = _Ximp_ctstombs(im, (char *)s,
					str_len, buffer, bytes, &sts)) <=  0) {
		ret = 0;
		if(status) *status = XLookupNone;
	    } else if (sts == XBufferOverflow) {
		return(len); /* Immidiately return */
	    } else {
		ret = len;
		if(status) *status = XLookupChars;
	    }
	    XFree( _xim_message_buff );
	    _xim_message_buff = NULL;
	    _xim_message_len = 0;
	    _xim_lookup_sign = LookupNothing;
	    return(ret);
	} else {
	    if(status) *status = XLookupNone;
	    return(0);
	}
    } else if(ev->type == KeyPress) {
	ic->ximp_icpart->putback_key_event = False;
	if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
	    if(status) *status = XLookupNone;
	    return(0);
	}
	ret = _Ximp_LookupMBText(ic,
		ev, (unsigned char *)buffer, bytes, keysym, comp_status);
	if(ret > 0) {
	    if(keysym && *keysym != NoSymbol) {
		if(status) *status = XLookupBoth;
	    } else {
		if(status) *status = XLookupChars;
	    }
	} else {
	    if(keysym && *keysym != NoSymbol) {
		if(status) *status = XLookupKeySym;
	    } else {
		if(status) *status = XLookupNone;
	    }
	}
    } else {
	if (status) *status = XLookupNone;
    }
    return(ret);
}

Public int
_Ximp_WcLookupString(xic, ev, buffer, wlen, keysym, status)
    XIC			xic;
    XKeyEvent		*ev;
    wchar_t		*buffer;
    int			 wlen;
    KeySym		*keysym;
    Status		*status;
{
    Ximp_XIC		 ic = (Ximp_XIC)xic;
    Ximp_XIM		 im = (Ximp_XIM)ic->core.im;
    XComposeStatus	*comp_status = NULL;
    int			 ret, len;
    char		 look[128];
    unsigned char   	*s;
    int			 str_len;
    Status		 sts;

    if(ev->type == KeyPress && ev->keycode == 0) { /* Filter function */
	if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
	    if(status) *status = XLookupNone;
	    _xim_lookup_sign = LookupNothing;
	    _xim_message_len = 0;
	    return(0);
	}
	if ( _xim_message_ic == ic &&
	   ((_xim_lookup_sign == LookupProperty)
			 || (_xim_lookup_sign == LookupMessage))) {
	    s = _xim_message_buff;
	    str_len = _xim_message_len;
	    if ((len = _Ximp_ctstowcs(im, (char *)s,
					str_len, buffer, wlen, &sts)) <=  0) {
		ret = 0;
		if(status) *status = XLookupNone;
	    } else if (sts == XBufferOverflow) {
		return(len); /* Immidiately return */
	    } else {
		ret = len;
		if(status) *status = XLookupChars;
	    }
	    XFree( _xim_message_buff );
	    _xim_message_buff = NULL;
	    _xim_message_len = 0;
	    _xim_lookup_sign = LookupNothing;
	    return(ret);
	} else {
	    if(status) *status = XLookupNone;
	    return(0);
	}
    } else if(ev->type == KeyPress) {
	ic->ximp_icpart->putback_key_event = False;
	if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
	    if(status) *status = XLookupNone;
	    return(0);
	}
	ret = _Ximp_LookupWCText(ic, ev, buffer, wlen, keysym, comp_status);
	if(ret > 0) {
	    if(keysym && *keysym != NoSymbol) {
		if(status) *status = XLookupBoth;
	    } else {
		if(status) *status = XLookupChars;
	    }
	} else {
	    if(keysym && *keysym != NoSymbol) {
		if(status) *status = XLookupKeySym;
	    } else {
		if(status) *status = XLookupNone;
	    }
	}
	if(ret > 0) {
	    if(keysym && *keysym != NoSymbol) {
		if(status) *status = XLookupBoth;
	    } else {
		if(status) *status = XLookupChars;
	    }
	} else {
	    if(keysym && *keysym != NoSymbol) {
		if(status) *status = XLookupKeySym;
	    } else {
		if(status) *status = XLookupNone;
	    }
	}
    } else {
	if (status) *status = XLookupNone;
    }
    return(ret);
}

Private Bool
_Ximp_FocusInput (window, mask)
    Window		window;
    unsigned long	*mask;
{
    int		i;
    Ximp_XIM	pim;
    Ximp_XIC	pic;

    for(i = 0; i < Ximp_Xim_count; i++) {
	if((pim = Ximp_Xim_List[i]) == NULL)
	    continue ;
	for (pic = (Ximp_XIC)pim->core.ic_chain;
	     pic; pic = (Ximp_XIC)pic->core.next) {
	    if(pic->core.focus_window == window && IS_BEING_PREEDITED(pic)) {
		*mask = pic->ximp_icpart->back_mask;
		return(True);
	    }
	}
    }
    return (False);
}

Public void
_Ximp_GetFocusWindowSelectMask(ic, client_window, mask)
    Ximp_XIC		 ic;
    Window		 client_window;
    unsigned long	*mask;
{
    Display	 *d = ic->core.im->core.display;
    XWindowAttributes		ret_attributes;

    if(!(XIMP_CHK_FOCUSWINMASK(ic))) {
	XGetWindowAttributes(d, client_window, &ret_attributes);
	*mask = ret_attributes.your_event_mask;
	return;
    }
    if (!(_Ximp_FocusInput (ic->core.focus_window, mask))) {
	XGetWindowAttributes(d, ic->core.focus_window, &ret_attributes);
	*mask = ret_attributes.your_event_mask;
    }
    return;
}

Private Bool
isRegisterdKey(list, ev)
    Ximp_KeyList *list ;
    XKeyEvent *ev;
{
#define	BUFFLIM		32
    register		i;
    KeySym		keysym;
    char		buff[BUFFLIM];

    XLookupString( (XKeyEvent *)ev, buff, BUFFLIM, &keysym, NULL );

    if(!keysym || list == NULL) return False ;
    for(i = 0 ; i < (int)list->count_keys; i++) {
	if((keysym == list->keys_list[i].keysym)
	   && ((ev->state & list->keys_list[i].modifier_mask)
	       == list->keys_list[i].modifier ) ){
	    return True ;
	}
    }
    return False ;
}

Private Bool
isOnEvent(ic, ev)
    Ximp_XIC ic ;
    XEvent *ev ;
{
    Ximp_KeyList *list = ((Ximp_XIM)ic->core.im)->ximp_impart->im_keyslist ;

    if( _XimpLockedICExtensionCheck(ic) )
	return False;

    if(isRegisterdKey(list, ev)){
	return True ;
    } else if(IS_IC_CONNECTED(ic)){
	return False ;
    }
    list = ((Ximp_XIM)ic->core.im)->ximp_impart->process_start_keys ;
    return isRegisterdKey(list, ev) ;
}

Private Bool
isOffEvent(ic, ev)
    Ximp_XIC ic ;
    XEvent *ev ;
{
    Ximp_KeyList *list = ((Ximp_XIM)ic->core.im)->ximp_impart->im_offkeyslist ;

    if( _XimpLockedICExtensionCheck(ic) ) {
	return False;
    }
    if(isRegisterdKey(list, ev)){
	return True ;
    }
    return False ;
}

Private Bool
_Ximp_ForwardEvent(ic, ev)
    Ximp_XIC ic;
    XKeyEvent *ev;
{
    XimpCMPredicateArgRec	Arg;

    if(ISXimp4(ic)){
	if(ISFE3(ic)){
	    if(ev->window != ic->core.focus_window) {
		_Ximp_IM_SendMessage(ic, XIMP_KEYPRESS(ic),
				     (long)ev->keycode,
				     (long)ev->state, (Time)ev->time );
	    }
	    if(isOffEvent(ic,ev)){
		Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
		Arg.icid = ic->ximp_icpart->icid;
		Arg.protocol = XIMP_SPROC_STOPPED(ic);
		Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
		_Ximp_Q_align(ic, &Arg, True);
	    }
	} else {
	    if(ISFE2(ic) && (ev->window == ic->core.focus_window)) {
		; /* Un expected event */
	    } else {
		_Ximp_IM_SendMessage(ic, XIMP_KEYPRESS(ic),
				     (long)ev->keycode,
				     (long)ev->state, (Time)ev->time );
	    }
	    if( ISSYNC(ic) ) {
		Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
		Arg.icid = ic->ximp_icpart->icid;
		Arg.protocol = XIMP_KEYPRESS_RETURN(ic);
		Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
		_Ximp_Q_align(ic, &Arg, False) ;
	    }
	}
    } else {
	_Ximp_IM_SendMessage(ic, XIMP_KEYPRESS(ic),
			     (long)ev->keycode,
			     (long)ev->state, NULL);
    }
    return(FILTERD);
}

Private void
_Ximp_RegKeyPressed( ic, ev )
    Ximp_XIC	ic;
    XKeyEvent	*ev;
{
    unsigned long		dummy_mask;
    XWindowAttributes		ret_attributes;
    XimpCMPredicateArgRec	Arg;
    Bool			is_SelectInput_needed = False;

    ic->ximp_icpart->input_mode = BEING_PREEDITED;

    if(ISXimp4(ic)) {
	if(ISFE1(ic) || (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic)))
	    is_SelectInput_needed = True;
    } else if(ISXIMP3FE(ic)) {
	is_SelectInput_needed = True;
    }
    if(is_SelectInput_needed) {
	Display *d = ic->core.im->core.display;

	XGetWindowAttributes(d, ic->core.focus_window, &ret_attributes);
	dummy_mask = ret_attributes.your_event_mask;
	ic->ximp_icpart->back_mask = dummy_mask;
	if(ISXimp4(ic)) {
	    if(ISFE1(ic))
		dummy_mask &= ~(KeyPressMask|KeyReleaseMask);
	    else { /* (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic)) */
		dummy_mask |= KeyReleaseMask;
	    }
	} else if(ISXIMP3FE(ic)) {
	    dummy_mask &= ~(KeyPressMask|KeyReleaseMask);
	}
	XSelectInput(ic->core.im->core.display, ic->core.focus_window, dummy_mask);
    }
    _Ximp_IM_SendMessage(ic, XIMP_MOVE(ic),
			 ic->ximp_icpart->preedit_attr.SpotLocation.x,
			 ic->ximp_icpart->preedit_attr.SpotLocation.y,
			 NULL);
    if( (ISXimp4(ic)  &&  ISTYPE2(ic)) ) {
	Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	Arg.icid = ic->ximp_icpart->icid;
	Arg.protocol = XIMP_KEYPRESS_RETURN(ic);
	Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	_Ximp_IM_SendMessage(ic, XIMP_KEYPRESS(ic),
			     (long)ev->keycode,
			     (long)ev->state, (Time)ev->time );
	if( ISSYNC(ic) )
	    _Ximp_Q_align(ic, &Arg, False) ;
    } else {
	_Ximp_IM_SendMessage(ic, XIMP_REG_KEY_PRESSED(ic), NULL, NULL, NULL);
    }
    XFlush(ic->core.im->core.display);
}

Private Bool
_Ximp_KeypressFilter( ic, ev )
    Ximp_XIC	ic;
    XKeyEvent	*ev;
{
    Bool	isFilterd = False;

    if (IS_FABLICATED(ic,ev)){
	ic->ximp_icpart->putback_key_event = False;
	return NOTFILTERD ;
    }
    if(IS_IC_CONNECTED(ic)) {
	if(IS_BEING_PREEDITED(ic)) {
	    return _Ximp_ForwardEvent(ic, ev);
	}
	if(isOnEvent(ic, ev)){
	    _Ximp_RegKeyPressed( ic, ev );
	    return FILTERD;
	}
	return NOTFILTERD;
    } else if(IS_RECONNECTABLE(ic->core.im)) {
	if(isOnEvent(ic, ev)){
	    if(!IS_SERVER_CONNECTED(ic->core.im)) {
		if(!(isFilterd = _Ximp_ConnectServer((Ximp_XIM)ic->core.im)))
		    return(NOTFILTERD);
	    }
	    if( _XimpReconnectICExtensionCheak(ic) ){
	         isFilterd = _XimpReconnectICExtensionHook(ic);
            } else {
                 isFilterd = _Ximp_ConnectIC(ic, XIMP_START_IC);
            }
	}
        if(!isFilterd) return(NOTFILTERD);

	_Ximp_RegKeyPressed( ic, ev );

	if(IS_RESTARTABLE(ic->core.im)) {
	    _Ximp_CallRestartCallbackExtensionHook(ic);
	}
	return FILTERD;
    }
    return(NOTFILTERD);
}

Private Bool
_Ximp_KeyreleaseFilter( ic, ev )
    Ximp_XIC	ic;
    XKeyEvent	*ev;
{
    XimpCMPredicateArgRec	Arg;

    if (IS_FABLICATED(ic,ev)){
	ic->ximp_icpart->putback_key_event = False;
	return NOTFILTERD ;
    }
    if(!IS_IC_CONNECTED(ic))
	return(NOTFILTERD);
    if(ISXimp4(ic)) {
	if(ISFRONTEND(ic) && (!(ic->ximp_icpart->back_mask & KeyReleaseMask))){
	    ic->ximp_icpart->back_mask |= KeyReleaseMask ;
	    _Ximp_IM_SendMessage(ic, XIMP_EVENTMASK_NOTIFY(ic),
				 ic->ximp_icpart->back_mask,
				 NULL, NULL );
	    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	    Arg.icid = ic->ximp_icpart->icid;
	    Arg.protocol = XIMP_EVENTMASK_NOTIFY_RETURN(ic);
	    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	    _Ximp_Q_align(ic, &Arg, False);
	}
	if((ISTYPE1(ic) || ISFE3(ic)) && (!IS_BEING_PREEDITED(ic)))
	    return(NOTFILTERD);
	if(ISFE3(ic) && (ev->window == ic->core.focus_window))
	    return(FILTERD);

	_Ximp_IM_SendMessage(ic, XIMP_KEYRELEASE(ic),
				 (long)ev->keycode,
				 (long)ev->state, (Time)ev->time );
	if( ISSYNC(ic) ) {
	    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	    Arg.icid = ic->ximp_icpart->icid;
	    Arg.protocol = XIMP_KEYRELEASE_RETURN(ic);
	    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	    _Ximp_Q_align(ic, &Arg, False) ;
	}
	return(FILTERD);
    }
    return(NOTFILTERD);
}

Public Bool
_Ximp_ConnectIC(ic, mode)
    Ximp_XIC	ic;
    int		mode;
{
    unsigned long		 mask;
    XEvent			 event;
    XimpCMPredicateArgRec	 Arg;
    long			*type_p;
    Display			*display = ic->core.im->core.display;
    unsigned long		 dummy_mask;
    XWindowAttributes		 ret_attributes;
    Bool			 is_SelectInput_needed = False;

    if(mode == XIMP_CREATE_IC) {  /* XCretaeIC() */
	if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
	    ic->core.client_window = XCreateSimpleWindow( display,
						 DefaultRootWindow(ic->core.im->core.display),
						 0, 0, 1, 1, 1, 0, 0);
	}
    }

    if(!(XIMP_CHK_FOCUSWINMASK(ic))) {
	ic->core.focus_window = ic->core.client_window;
    }

    /* Property Data Set */
    XChangeProperty(display, ic->core.client_window,
		    ((Ximp_XIM)ic->core.im)->ximp_impart->version_id,
		    XA_STRING, 8, PropModeReplace,
		    (unsigned char *)XIMP_PROTOCOL_VERSION, strlen(XIMP_PROTOCOL_VERSION));
    XFlush(display);

    mask  = XIMP_PROTO_MASK2(ic);
    dummy_mask = NULL;
    /* Put XIMP_TYPE Property for Ximp4.0 */
    if(ISXimp4(ic)){
	for(type_p = ((Ximp_XIM)ic->core.im)->ximp_impart->type_list; *type_p; type_p++) {
	    if(*type_p == (long)ic->ximp_icpart->svr_mode)
		    break;
	}
	if(*type_p != NULL) {
	    XChangeProperty(display, ic->core.client_window,
			    ((Ximp_XIM)ic->core.im)->ximp_impart->type_id,
			    ((Ximp_XIM)ic->core.im)->ximp_impart->type_id,
			    32, PropModeReplace, (unsigned char *)type_p, 1);
	    mask |= XIMP_SERVERTYPE_MASK4;
	}
	if(ISFRONTEND(ic)) {
	    if (_Ximp_FocusInput(ic->core.focus_window, &dummy_mask))
		ic->ximp_icpart->back_mask = dummy_mask;
	    else {
		XGetWindowAttributes(display, ic->core.focus_window, &ret_attributes);
		dummy_mask = ret_attributes.your_event_mask;
		ic->ximp_icpart->back_mask = dummy_mask;
	    }
	}
    }

    _Ximp_SetFocusWindowProp(ic);
    _Ximp_SetFocusWindowFilter(ic);
    mask |= XIMP_FOCUS_WIN_MASK(ic);
    if(!(   (ic->core.input_style & XIMPreeditCallbacks)
	 || (ic->core.input_style & XIMPreeditNone) ) ) { 
	if(mask & XIMP_PROP_PREEDIT(ic))
	    _Ximp_SetPreeditAtr(ic);
	if(mask & XIMP_PROP_PREFONT(ic))
	    _Ximp_SetPreeditFont(ic);
    } else {
	mask &= ~(XIMP_PROP_PREEDIT(ic) | XIMP_PROP_PREFONT(ic));
    }
    if(!(   (ic->core.input_style & XIMStatusCallbacks)
	 || (ic->core.input_style & XIMStatusNone) ) ) { 
	if(mask & XIMP_PROP_STATUS(ic))
	    _Ximp_SetStatusAtr(ic);
	if(mask & XIMP_PROP_STSFONT(ic))
	    _Ximp_SetStatusFont(ic);
    } else {
	mask &= ~(XIMP_PROP_STATUS(ic) | XIMP_PROP_STSFONT(ic));
    }
    /* XIMP_CREATE ClientMessage Send */
    _Ximp_IM_SendMessage(ic, XIMP_CREATE(ic), ic->core.input_style, mask,
			dummy_mask);
    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.protocol = XIMP_CREATE_RETURN(ic);
    Arg.icid = 0;
    if(_XimpIfEvent(ic, &event, _Ximp_CMPredicate32, (XPointer)&Arg)){
	ic->ximp_icpart->icid = (ICID)event.xclient.data.l[1];
	if(ISXimp4(ic)){
	    if(ISFE2(ic) || (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE2(ic))) {
		is_SelectInput_needed = True;
		if(ISFE2(ic))
		    dummy_mask &= ~(KeyPressMask | KeyReleaseMask);
		else {  /*  IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE2(ic) */
		    dummy_mask |=   KeyReleaseMask;
		}
	    }
	    ic->ximp_icpart->input_mode = ISTYPE2(ic) ? BEING_PREEDITED : BEING_BYPASSED;
	    _Ximp_AfterCreateExtensionHook(ic);
	    if(is_SelectInput_needed)
		XSelectInput(display, ic->core.focus_window, dummy_mask);
	    return True;
	} else { /* Ximp3.5 */
	    _Ximp_AfterCreateExtensionHook(ic);
	    return True;
	}
    }
    XDeleteProperty(display, ic->core.client_window,
		    ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id);
    if(!((ic->core.input_style & XIMPreeditCallbacks)  ||
	  (ic->core.input_style & XIMPreeditNone)) ) { 
	if(mask & XIMP_PROP_PREEDIT(ic))
	    XDeleteProperty(display, ic->core.client_window,
			    ((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id);
	if(mask & XIMP_PROP_PREFONT(ic))
	    XDeleteProperty(display, ic->core.client_window,
			    ((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id);
    }
    if(!((ic->core.input_style & XIMStatusCallbacks)  ||
	  (ic->core.input_style & XIMStatusNone))) { 
	if(mask & XIMP_PROP_STATUS(ic))
	    XDeleteProperty(display, ic->core.client_window,
			    ((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id);
	if(mask & XIMP_PROP_STSFONT(ic))
	    XDeleteProperty(display, ic->core.client_window,
			    ((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id);
    }
    return False ;
}

Private void
_Ximp_MakeKeypress(d, w, ev)
    Display		*d;
    Window		w;
    XKeyEvent		*ev;
{
    ev->type = KeyPress;
    ev->keycode = 0;
}

Private Bool
_Ximp_ProcKey(d, w, ev, kev, type)
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
    XKeyEvent		*kev;
    int			 type;
{
    ICID		icid = ev->data.l[1];
    Ximp_XIC 		ic;

    if((ic = _Ximp_LookupXIC(icid, ev->window)) == NULL)
	return False;
    kev->type = type;
    kev->serial = ev->serial;
    kev->send_event = False;
    kev->display = ev->display;
    kev->window = ev->window;
    kev->root = DefaultRootWindow(ev->display);
    kev->subwindow = (Window)NULL;
    kev->time = ev->data.l[4];
    kev->x = 0;
    kev->y = 0;
    kev->x_root = 0;
    kev->y_root = 0;
    kev->keycode = ev->data.l[2];
    kev->state = ev->data.l[3];
    kev->same_screen = True;
    if(IS_BEING_PREEDITED(ic)) {
	ic->ximp_icpart->putback_key_event = True;
    }
    return True;
}

Private void
_Ximp_ProcCreateReturn (d, w, ev)
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
{
    ICID	icid;

    icid = ev->data.l[1];
}

Public void
_Ximp_ServerProcessStarted (d, w, ev)
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
{
    ICID		icid = ev->data.l[1];
    Ximp_XIC            ic = _Ximp_LookupXIC(icid, ev->window);
    XWindowAttributes	ret_attributes;
    unsigned long	mask;
    Bool		is_SelectInput_needed = False;

    if(!ic || IS_BEING_PREEDITED(ic))
	return;

    ic->ximp_icpart->input_mode = BEING_PREEDITED;

    if(ISXimp4(ic)) {
	if(ISFE1(ic) || (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic)))
	    is_SelectInput_needed = True;
    } else if(ISXIMP3FE(ic)) {
	is_SelectInput_needed = True;
    }
    if(is_SelectInput_needed) {
	if (_Ximp_FocusInput (ic->core.focus_window, &mask)){
	    ic->ximp_icpart->back_mask = mask;
	} else {
	    XGetWindowAttributes(d, ic->core.focus_window, &ret_attributes);
	    ic->ximp_icpart->back_mask = mask = ret_attributes.your_event_mask;
	}
	if(ISXimp4(ic)) {
	    if(ISFE1(ic))
		mask &= ~(KeyPressMask|KeyReleaseMask);
	    else {  /*  (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic)) */
		mask |= KeyReleaseMask;
	    }
	} else if(ISXIMP3FE(ic)) {
	    mask &= ~(KeyPressMask|KeyReleaseMask);
	}
	XSelectInput(d, ic->core.focus_window, mask);
	XFlush(d);
    }
    return;
}

/* Ximp4.0 */
Public void
_Ximp_ConvertOn (ic)
    Ximp_XIC		ic;
{
    XWindowAttributes	ret_attributes;
    unsigned long	mask;

    if(IS_BEING_PREEDITED(ic))
	return;
    ic->ximp_icpart->input_mode = BEING_PREEDITED;
    if(ISFE1(ic) || (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic))) {
	if (_Ximp_FocusInput (ic->core.focus_window, &mask)){
	    ic->ximp_icpart->back_mask = mask;
	} else {
	    XGetWindowAttributes(ic->core.im->core.display, ic->core.focus_window, &ret_attributes);
	    ic->ximp_icpart->back_mask = mask = ret_attributes.your_event_mask;
	}
	if(ISFE1(ic))
	    mask &= ~(KeyPressMask|KeyReleaseMask);
	else {  /* (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic)) */
	    mask |= KeyReleaseMask;
	}
	XSelectInput(ic->core.im->core.display, ic->core.focus_window, mask);
	XFlush(ic->core.im->core.display);
    }
    if( _XimpIsNeedMoveProtoMode(ic) ) {
	_Ximp_IM_SendMessage(ic, XIMP_MOVE(ic),
			 ic->ximp_icpart->preedit_attr.SpotLocation.x,
			 ic->ximp_icpart->preedit_attr.SpotLocation.y,
			 NULL);
    }
    return;
}

/* Ximp4.0 */
Public void
_Ximp_ConvertOff (ic)
    Ximp_XIC		ic;
{
    ic->ximp_icpart->input_mode = BEING_BYPASSED;
    if(ISFE1(ic) || (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBE1(ic))) {
	XSelectInput(ic->core.im->core.display,
		     ic->core.focus_window, ic->ximp_icpart->back_mask );
	XFlush(ic->core.im->core.display);
    }
    return;
}

Private void
_Ximp_ProcReadProperty (d, w, ev)
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
{
    ICID		icid      = ev->data.l[1];
    Atom		read_prop = ev->data.l[2];
    Ximp_XIC		ic = _Ximp_LookupXIC(icid, ev->window);
    int			rval;
    Atom		actual_type_return;
    int			actual_format_return;
    unsigned long	nitems_return;
    int			i;
    unsigned char	*tmp;
    
    if(!ic) return ;
    rval = XGetWindowProperty( d,
			      ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window,
			      read_prop, 0, 100000L, True,
			      AnyPropertyType, &actual_type_return,
			      &actual_format_return, &_xim_message_len,
			      &nitems_return, &_xim_message_buff );
    if( rval != Success )
	return;
    if( (tmp = (unsigned char *)Xrealloc(_xim_message_buff, _xim_message_len + sizeof(long))) == NULL ) {
	XFree( _xim_message_buff );
	_xim_message_buff = NULL;
	return;
    }
    _xim_message_buff = tmp;
    for( i = 0; i < sizeof(long); i++ )
	_xim_message_buff[_xim_message_len + i] = (char)NULL;

    _xim_message_ic  = ic;
    _xim_lookup_sign = LookupProperty;
}

Public void
_Ximp_ProcError (ic0, d, w, ev)
    Ximp_XIC		ic0;
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
{
    ICID		icid;
    Ximp_XIC		ic;
    unsigned long	data[3];

    /*
     * ToDo:
     *        If you want to process the error from IM server,
     *        you should modify this routine.
     */
    if((icid = ev->data.l[1]) != 0) {
	if((ic = _Ximp_LookupXIC(icid, ev->window)) == NULL)
	    return;
    } else if((ic = ic0) == NULL)
	return;

    if (ic->ximp_icpart->ictype->error.callback) {
	data[0] = ev->data.l[2];
	data[2] = ev->data.l[4];
	if(!ISXIMP_ERROR(ev))
	    data[1] = XIMP_BadProtocol;
	else
	    data[1]  = ev->data.l[3];
	(*ic->ximp_icpart->ictype->error.callback)(ic,
			   ic->ximp_icpart->ictype->error.client_data, data);
    }
}

Private Atom
_Ximp_ProtocolIDOfDisplay(d)
    Display *d;
{
    int         i;
    Ximp_XIM    pim;

    for(i = 0; i < Ximp_Xim_count; i++) {
        if((pim = Ximp_Xim_List[i]) == NULL ) continue ;
        if(d == pim->core.display){
            return (pim->ximp_impart->improtocol_id) ;
        }
    }
    return (NULL);
}

Private Bool
_Ximp_ProtoReceive (d, w, ev, client_data)
    Display		*d;
    Window		 w;
    XClientMessageEvent	*ev;
    XPointer		 client_data;
{
    XKeyEvent kev;
    Ximp_XIC	ic;

    if (ev->message_type != _Ximp_ProtocolIDOfDisplay(d))
      return (False);
    if (ev->format == 32) {
	ic   = _Ximp_LookupXIC((ev->data.l[1]), ev->window);
	if (ic == NULL) 
	    return (False);
	switch (ev->data.l[0]) {
	case XIMP_KEYPRESS4:
	case XIMP_KEYPRESS3:
	    if(ISFE1(ic) && !IS_BEING_PREEDITED(ic)) {
		return(True);
	    }
	    if(_Ximp_ProcKey(d, w, ev, &kev, KeyPress))
		XPutBackEvent(d, (XEvent *)&kev);
	    else
		return(False);
	    break ;
	case XIMP_KEYRELEASE4:
	    if(!ISXimp4(ic)) return True ;
	    if(!IS_BEING_PREEDITED(ic)) return True ;
	    if(!(ic->ximp_icpart->back_mask & KeyReleaseMask)) return True;
	    if(_Ximp_ProcKey(d, w, ev, &kev, KeyRelease))
		XPutBackEvent(d, (XEvent *)&kev);
	    else
		return(False);
	    break ;
	case XIMP_CREATE_RETURN4:
	case XIMP_CREATE_RETURN3:
	    _Ximp_ProcCreateReturn (d, w, ev);
	    break;
	case XIMP_SPROC_STARTED4:
	case XIMP_CONVERSION_BEGIN3:
	    _Ximp_ServerProcessStarted(d, w, ev);
	    break;
	case XIMP_SPROC_STOPPED4:
	case XIMP_CONVERSION_END3:
	    _Ximp_ServerProcessStopped(d, w, ev);
	    break;
	case XIMP_READPROP4:
	case XIMP_READPROP3:
	    if(ISFE1(ic) && !IS_BEING_PREEDITED(ic)) {
		return(True);
	    }
	    _Ximp_ProcReadProperty (d, w, ev);
	    _Ximp_MakeKeypress (d, w, ev);
	    ev->send_event = False ;
	    XPutBackEvent(d, (XEvent *)ev);
	    break ;
	case XIMP_ERROR4:
	case XIMP_ERROR3:
	    _Ximp_ProcError (NULL, d, w, ev);
	    break;
	case XIMP_GEOMETRY4:
	case XIMP_GEOMETRY3:
	    _Ximp_CallGeometryCallback (ic, ev);
	    break;
	case XIMP_PREEDITSTART4:
	case XIMP_PREEDITSTART3:
	    _Ximp_CallPreeditStartCallback (ic, ev);
	    break;
	case XIMP_PREEDITDONE4:
	case XIMP_PREEDITDONE3:
	    _Ximp_CallPreeditDoneCallback (ic, ev);
	    break;
	case XIMP_PREEDITDRAW4:
	case XIMP_PREEDITDRAW3:
	    _Ximp_CallPreeditDrawCallback (ic, ev);
	    break;
	case XIMP_PREEDITDRAW_CM4:
	case XIMP_PREEDITDRAW_CM3:
	    _Ximp_CallPreeditDrawCallback2 (ic, ev);
	    break;
	case XIMP_PREEDITDRAW_CM_TINY4:
	case XIMP_PREEDITDRAW_TINY3:
	    _Ximp_CallPreeditDrawCallback3 (ic, ev);
	    break;
	case XIMP_PREEDITCARET4:
	case XIMP_PREEDITCARET3:
	    _Ximp_CallPreeditCaretCallback (ic, ev);
	    break;
	case XIMP_STATUSSTART4:
	case XIMP_STATUSSTART3:
	    _Ximp_CallStatusStartCallback (ic, ev);
	    break;
	case XIMP_STATUSDONE4:
	case XIMP_STATUSDONE3:
	    _Ximp_CallStatusDoneCallback (ic, ev);
	    break;
	case XIMP_STATUSDRAW4:
	case XIMP_STATUSDRAW3:
	    _Ximp_CallStatusDrawCallback (ic, ev);
	    break;
	case XIMP_STATUSDRAW_CM4:
	case XIMP_STATUSDRAW_CM3:
	    _Ximp_CallStatusDrawCallback2 (ic, ev);
	    break;
	case XIMP_EXTENSION4:
	case XIMP_EXTENSION3:
	    _Ximp_ProcExtension(d, w, ev);
	    break;
	default:
	    break;
	}
    } else if (ev->format == 8) {
	ic = _Ximp_LookupXIC((ICID)ntohl(*(unsigned long *)(&(ev->data.b[0]))),
				ev->window);
	if(ISFE1(ic) && !IS_BEING_PREEDITED(ic)) {
	    return(True);
	}
	XPutBackEvent( d, (XEvent *)ev );
	_xim_message_buff = NULL;
	_xim_message_len = _Ximp_CombineMultipleCM( ic, &_xim_message_buff );
	if( _xim_message_len != 0 ) {
	    _xim_lookup_sign = LookupMessage;
	    _xim_message_ic  = ic;
	    _Ximp_MakeKeypress( d, w, ev );
	    ev->send_event = False;
	    XPutBackEvent( d, (XEvent *)ev );
	}
    }
    return (True);
}

Private  void
_Ximp_CallbackDestroy(ic, w, ev)
    XIC			 ic ;
    Window		 w;
    XEvent		*ev;
{
    XICXimpRec *xic = ((Ximp_XIC)ic)->ximp_icpart ;
    register XIMCallback *pcb = &ic->core.preedit_attr.done_callback;
    register XIMCallback *scb = &ic->core.status_attr.done_callback;

    if((ic->core.input_style & XIMPreeditCallbacks) && pcb->callback && xic->cbstatus & XIMPCBPREEDITACTIVE){
	(*pcb->callback) (ic, pcb->client_data, NULL);
    }
    if((ic->core.input_style & XIMStatusCallbacks) && scb->callback && xic->cbstatus & XIMPCBSTATUSACTIVE){
	register XIMCallback *cb = &ic->core.status_attr.draw_callback;
	XIMStatusDrawCallbackStruct CallData;
	XIMText         cbtext;

	bzero(&CallData, sizeof(XIMStatusDrawCallbackStruct));
	bzero(&cbtext, sizeof(XIMText));
	CallData.data.text = &cbtext;
	cbtext.feedback = NULL;
	cbtext.encoding_is_wchar = False ;
	cbtext.string.multi_byte = DEFAULTCBSTATUSSTRING ;
	cbtext.length = strlen(DEFAULTCBSTATUSSTRING) ;
	(*cb->callback) (ic, scb->client_data, &CallData);
	(*scb->callback) (ic, scb->client_data, NULL);
    }
}

Private Bool
_Ximp_ServerDestroy (d, w, ev, client_data)
    Display		*d;
    Window		 w;
    XEvent		*ev;
    XPointer		 client_data;
{
    register int	 i;
    register XIMXimpRec	*ximp_impart;
    register XIC	 ic;
    Bool                 is_SelectInput_needed = False;
    Bool		 is_server_destroy = False;

    for(i=0; i < Ximp_Xim_count; i++) {
	if(Ximp_Xim_List[i] != NULL  &&
	   Ximp_Xim_List[i]->ximp_impart->fe_window == w)
	    ximp_impart = Ximp_Xim_List[i]->ximp_impart;
	else
	    continue;

	is_server_destroy = True;
	_XUnregisterFilter(d, w, _Ximp_XimFilter_Destroy, (XPointer)NULL);
	_XimpConnectServerFreeExtensionHook(Ximp_Xim_List[i]);
	_Ximp_SetupFree(ximp_impart->im_proto_vl,
			ximp_impart->im_styles,
			ximp_impart->type_list,
			/*
			 * For the reconnection, we should not free those datas.
			 */
			NULL, /* ximp_impart->im_keyslist */
			NULL, /* ximp_impart->im_offkeyslist */
			ximp_impart->im_server_name,
			ximp_impart->im_server_vl,
			ximp_impart->im_vendor_name,
			ximp_impart->im_ext_list);
	IS_SERVER_CONNECTED(Ximp_Xim_List[i]) = False;
	ximp_impart->fe_window = (Window)NULL;
	for(ic = Ximp_Xim_List[i]->core.ic_chain; ic; ic = ic->core.next) {
	    _Ximp_CallbackDestroy(ic, w, ev);
	    _Ximp_CallDestroyCallbackExtensionHook((Ximp_XIC)ic);
	    ((Ximp_XIC)ic)->ximp_icpart->icid = NULL;
	    _Ximp_ServerDestroyExtensionHook((Ximp_XIC)ic);
	    _XUnregisterFilter(d, ic->core.focus_window,
			       _Ximp_XimFilter_Client, NULL);
	    if(ISXimp4(ic)) {
		if(ISFE1(ic) || ISFE2(ic) || (IS_FORCESELECTKEYRELEASE(ic->core.im) && ISBACKEND(ic)))
		    is_SelectInput_needed = True;
	    } else if(ISXIMP3FE(ic)) {
		is_SelectInput_needed = True;
	    }

	    if(is_SelectInput_needed) {
		if(((Ximp_XIC)ic)->ximp_icpart->back_mask)
		    XSelectInput(d, ic->core.focus_window,
				 ((Ximp_XIC)ic)->ximp_icpart->back_mask );
		else {
		    unsigned long               dummy_mask;
		    XWindowAttributes           ret_attributes;

		    XGetWindowAttributes(d, ((Ximp_XIC)ic)->core.focus_window,
					 &ret_attributes);
		    dummy_mask = ret_attributes.your_event_mask;
		    ((Ximp_XIC)ic)->ximp_icpart->back_mask = dummy_mask;
		    if(ISXimp4(ic)) {
			if(ISFE1(ic))
			    dummy_mask |= (KeyPressMask|KeyReleaseMask);
			else
			    ;
		    } else if(ISXIMP3FE(ic)) {
			dummy_mask |= (KeyPressMask|KeyReleaseMask);
		    }
		    XSelectInput(ic->core.im->core.display,
				 ic->core.focus_window, dummy_mask);
		}
	    }
	    ((Ximp_XIC)ic)->ximp_icpart->input_mode = BEING_BYPASSED;
	}
    }
    XFlush (d);
    return is_server_destroy;
}

Public Bool
#if NeedFunctionPrototypes
_Ximp_XimFilter_Keypress (
    Display		*d,
    Window		 w,
    XEvent		*ev,
    XPointer		 client_data
    )
#else
_Ximp_XimFilter_Keypress (d, w, ev, client_data)
    Display		*d;
    Window		 w;
    XEvent		*ev;
    XPointer		 client_data;
#endif
{
    return( _Ximp_KeypressFilter( (Ximp_XIC)client_data, ev ) );
}

Public Bool
#if NeedFunctionPrototypes
_Ximp_XimFilter_Keyrelease (
    Display		*d,
    Window		 w,
    XEvent		*ev,
    XPointer		 client_data
			    )
#else
_Ximp_XimFilter_Keyrelease (d, w, ev, client_data)
    Display		*d;
    Window		 w;
    XEvent		*ev;
    XPointer		 client_data;
#endif
{
    return( _Ximp_KeyreleaseFilter( (Ximp_XIC)client_data, ev ) );
}

/*
 *  _Ximp_XimFilter
 *    Regist _Ximp_XimFilter_Client filter using XRegisterFilterByType
 *    with start_type == end_type  == ClientMessage
 *    Regist _Ximp_XimFilter_Destroy filter using XRegisterFilterByType
 *    with start_type == end_type == DestroyNotify
 */

Public Bool
_Ximp_XimFilter_Client (d, w, ev, client_data)
    Display		*d;
    Window		 w;
    XEvent		*ev;
    XPointer		 client_data;
{
    return(_Ximp_ProtoReceive (d, w, ev, client_data));
}

Public Bool
_Ximp_XimFilter_Destroy (d, w, ev, client_data)
    Display		*d;
    Window		 w;
    XEvent		*ev;
    XPointer	 	 client_data;
{
    return(_Ximp_ServerDestroy (d, w, ev, client_data));
}
