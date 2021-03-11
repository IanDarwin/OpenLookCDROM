/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpHandler.c	1.2 94/02/16 */
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

#ifndef	_XIMPHANDLER_C_
#define	_XIMPHANDLER_C_
#include "XimpData.h"

#if NeedFunctionPrototypes
extern Bool _XimpProcKeyPress(XIMS, XClientMessageEvent*);
extern Bool _XimpProcKeyRelease(XIMS, XClientMessageEvent*);
extern Bool _XimpProcCreate(XIMS, XClientMessageEvent*);
extern Bool _XimpProcDestroy(XIMS, XClientMessageEvent*);
extern Bool _XimpProcStart(XIMS, XClientMessageEvent*);
extern Bool _XimpProcEnd(XIMS, XClientMessageEvent*);
extern Bool _XimpProcSetFocus(XIMS, XClientMessageEvent*);
extern Bool _XimpProcUnsetFocus(XIMS, XClientMessageEvent*);
extern Bool _XimpProcClientWin(XIMS, XClientMessageEvent*);
extern Bool _XimpProcFocusWin(XIMS, XClientMessageEvent*);
extern Bool _XimpProcMove(XIMS, XClientMessageEvent*);
extern Bool _XimpProcSetValues(XIMS, XClientMessageEvent*);
extern Bool _XimpProcGetValues(XIMS, XClientMessageEvent*);
extern Bool _XimpProcReset(XIMS, XClientMessageEvent*);
extern Bool _XimpProcEventMask(XIMS, XClientMessageEvent*);
extern Bool _XimpProcExtension(XIMS, XClientMessageEvent*);
extern Bool _XimpProcPreStartReturn(XIMS, XClientMessageEvent*);
extern Bool _XimpProcPreCaretReturn(XIMS, XClientMessageEvent*);
#else
extern Bool _XimpProcKeyPress();
extern Bool _XimpProcKeyRelease();
extern Bool _XimpProcCreate();
extern Bool _XimpProcDestroy();
extern Bool _XimpProcStart();
extern Bool _XimpProcEnd();
extern Bool _XimpProcSetFocus();
extern Bool _XimpProcUnsetFocus();
extern Bool _XimpProcClientWin();
extern Bool _XimpProcFocusWin();
extern Bool _XimpProcMove();
extern Bool _XimpProcSetValues();
extern Bool _XimpProcGetValues();
extern Bool _XimpProcReset();
extern Bool _XimpProcEventMask();
extern Bool _XimpProcExtension();
extern Bool _XimpProcPreStartReturn();
extern Bool _XimpProcPreCaretReturn();
#endif

Bool
#if NeedFunctionPrototypes
_XimpWaitIMProtocol(Display *dpy, Window window, XEvent *ev,
		    XPointer client_data)
#else
_XimpWaitIMProtocol(dpy, window, ev, client_data)
Display *dpy;
Window window;
XEvent *ev;
XPointer client_data;
#endif
{
    XIMS xims = (XIMS)client_data;
    XIMPCore core = (XIMPCore)xims->protocol;

    if (ev->type == ClientMessage &&
	ev->xclient.message_type == core->ximp_request &&
	ev->xclient.window == core->im_window &&
	ev->xclient.format == 32) {
	switch ((int)ev->xclient.data.l[0]) {
	  case XIMP_KEYPRESS4:
	  case XIMP_KEYPRESS3:
	    return _XimpProcKeyPress(xims, (XClientMessageEvent*)ev);
	  case XIMP_KEYRELEASE4:
	    return _XimpProcKeyRelease(xims, (XClientMessageEvent*)ev);
	  case XIMP_CREATE4:
	  case XIMP_CREATE3:
	    return _XimpProcCreate(xims, (XClientMessageEvent*)ev);
	  case XIMP_DESTROY4:
	  case XIMP_DESTROY3:
	    return _XimpProcDestroy(xims, (XClientMessageEvent*)ev);
	  case XIMP_REG_KEY_PRESSED4:
	  case XIMP_BEGIN3:
	    return _XimpProcStart(xims, (XClientMessageEvent*)ev);
	  case XIMP_END3:
	    return _XimpProcEnd(xims, (XClientMessageEvent*)ev);
	  case XIMP_SETFOCUS4:
	  case XIMP_SETFOCUS3:
	    return _XimpProcSetFocus(xims, (XClientMessageEvent*)ev);
	  case XIMP_UNSETFOCUS4:
	  case XIMP_UNSETFOCUS3:
	    return _XimpProcUnsetFocus(xims, (XClientMessageEvent*)ev);
	  case XIMP_CLIENT_WINDOW4:
	    return _XimpProcClientWin(xims, (XClientMessageEvent*)ev);
	  case XIMP_FOCUS_WINDOW4:
	    return _XimpProcFocusWin(xims, (XClientMessageEvent*)ev);
	  case XIMP_MOVE4:
	  case XIMP_MOVE3:
	    return _XimpProcMove(xims, (XClientMessageEvent*)ev);
	  case XIMP_SETVALUE4:
	  case XIMP_SETVALUE3:
	    return _XimpProcSetValues(xims, (XClientMessageEvent*)ev);
	  case XIMP_GETVALUE4:
	  case XIMP_GETVALUE3:
	    return _XimpProcGetValues(xims, (XClientMessageEvent*)ev);
	  case XIMP_RESET4:
	  case XIMP_RESET3:
	    return _XimpProcReset(xims, (XClientMessageEvent*)ev);
	  case XIMP_EVENTMASK_NOTIFY4:
	    return _XimpProcEventMask(xims, (XClientMessageEvent*)ev);
	  case XIMP_EXTENSION4:
	  case XIMP_EXTENSION3:
	    return _XimpProcExtension(xims, (XClientMessageEvent*)ev);
	  case XIMP_PREEDITSTART_RETURN4:
	  case XIMP_PREEDITSTART_RETURN3:
	    return _XimpProcPreStartReturn(xims, (XClientMessageEvent*)ev);
	  case XIMP_PREEDITCARET_RETURN4:
	  case XIMP_PREEDITCARET_RETURN3:
	    return _XimpProcPreCaretReturn(xims, (XClientMessageEvent*)ev);
	  default:
	    return False;	/* unknown ximp protocol */
	}
    }
    return False;
}
#endif	/* _XIMPHANDLER_C_ */
