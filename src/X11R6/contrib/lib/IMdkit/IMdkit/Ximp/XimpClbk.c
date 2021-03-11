/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpClbk.c	1.8 94/02/16 */
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

#ifndef	_XIMPCLBK_C_
#define	_XIMPCLBK_C_
#include "XimpData.h"

#if NeedFunctionPrototypes
extern XimpClient *_XimpFindClient(XIMPCore, CARD32, IMPProtocol*);
#else
extern XimpClient *_XimpFindClient();
#endif

int
#if NeedFunctionPrototypes
_XimpGeometryCallback(XIMS xims, IMPProtocol *call_data)
#else
_XimpGeometryCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPAnyStruct *geometry =
      (XIMPAnyStruct*)&call_data->geometry_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)geometry->icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_GEOMETRY4 :
			 XIMP_GEOMETRY3),
			geometry->icid,
			0, 0, 0);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpPreeditStartCallback(XIMS xims,
			  IMPProtocol *call_data)
#else
_XimpPreeditStartCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&call_data->preedit_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)preedit->icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_PREEDITSTART4 :
			 XIMP_PREEDITSTART3),
			preedit->icid,
			0, 0, 0);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpPreeditDoneCallback(XIMS xims,
			 IMPProtocol *call_data)
#else
_XimpPreeditDoneCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&call_data->preedit_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)preedit->icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_PREEDITDONE4 :
			 XIMP_PREEDITDONE3),
			preedit->icid,
			0, 0, 0);
    return True;
}

static char *
#if NeedFunctionPrototypes
MakeNewAtom(char *atomName)
#else
MakeNewAtom(atomName)
char *atomName;
#endif
{
    static int sequence = 0;
    sprintf(atomName, "_XIMP_CALLBACKS_%d",
	    (sequence > 20 ? (sequence = 0) : sequence++));
    return atomName;
}

int
#if NeedFunctionPrototypes
_XimpPreeditDrawCallback(XIMS xims,
			 IMPProtocol *call_data)
#else
_XimpPreeditDrawCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    Window ims_window = core->im_window;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&call_data->preedit_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)preedit->icid,
					 (IMPProtocol*)NULL);
    XIMPreeditDrawCallbackStruct *draw =
      (XIMPreeditDrawCallbackStruct *)&preedit->todo.draw;
    CARD32 l[3];
    register int n;
    Atom atom1, atom2, atom3;
    char name[20];

    if (client == (XimpClient *)NULL) return False;

    atom1 = XInternAtom(display, MakeNewAtom(name), False);
    atom2 = XInternAtom(display, MakeNewAtom(name), False);
    atom3 = XInternAtom(display, MakeNewAtom(name), False);

    /* Atom 1 */
    l[0] = draw->caret; l[1] = draw->chg_first; l[2] = draw->chg_length;
    XChangeProperty(display, ims_window, atom1, core->atoms.preedit_draw_data,
		    32, PropModeReplace,
		    (unsigned char *)l, 3);
    /* Atom 2 */
    XChangeProperty(display, ims_window, atom2, core->atoms.ctext_type,
		    8, PropModeReplace,
		    (unsigned char *)draw->text->string.multi_byte,
		    draw->text->length);
    /* Atom 3 */
    for (n = 0; draw->text->feedback[n] != 0; n++) ;
    XChangeProperty(display, ims_window, atom3, core->atoms.feedbacks,
		    32, PropModeReplace,
		    (unsigned char *)draw->text->feedback, n);

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_PREEDITDRAW4 :
			 XIMP_PREEDITDRAW3),
			preedit->icid,
			atom1, atom2, atom3);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpPreeditDrawCMCallback(XIMS xims,
			   IMPProtocol *call_data)
#else
_XimpPreeditDrawCMCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    Window ims_window = core->im_window;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&call_data->preedit_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)preedit->icid,
					 (IMPProtocol*)NULL);
    XIMPreeditDrawCallbackStruct *draw =
      (XIMPreeditDrawCallbackStruct *)&preedit->todo.draw;
    register int n;
    union {
	CARD8  b[12];
	CARD16 s[6];
	CARD32 l[3];
    } data;
    Atom atom3;
    char name[20];

    if (client == (XimpClient *)NULL) return False;

    memset(&data, 0, 12);

    /* count feedbacks */
    for (n = 0; draw->text->feedback[n] != 0; n++) ;

    if (n > 1) {
      /* expecting to never happen */
	atom3 = XInternAtom(display, MakeNewAtom(name), False);
	XChangeProperty(display, ims_window, atom3, core->atoms.feedbacks,
			32, PropModeReplace,
			(unsigned char *)draw->text->feedback, n);
    }

    /* PreeditDrawCBStatus */
    if (draw->text->length == 0)
      data.s[0] |= 0x0001;

    if (n == 0)
      data.s[0] |= 0x0002;
    else if (n > 1)
      data.s[0] |= 0x0004;	/* expecting to never happen */

    /* caret, chg_first, chg_length */
    data.s[1] = (CARD16)draw->caret;
    data.s[2] = (CARD16)draw->chg_first;
    data.s[3] = (CARD16)draw->chg_length;

    /* feedback-value */
    if (n == 1)
      data.l[2] = draw->text->feedback[0];
    else if (n > 1)
      data.l[2] = atom3;

    /* First ClientMessage */
    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_PREEDITDRAW4 :
			 XIMP_PREEDITDRAW3),
			preedit->icid,
			data.l[0], data.l[1], data.l[2]);
    /* Second ClientMessage, containing preedit string  */
    if (draw->text->length != 0)
      _XimpSendByClientMessage(core,
			       preedit->icid,
			       draw->text->string,
			       draw->text->length);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpPreeditCaretCallback(XIMS xims,
			  IMPProtocol *call_data)
#else
_XimpPreeditCaretCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPPreeditCBStruct *preedit =
      (XIMPPreeditCBStruct*)&call_data->preedit_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)preedit->icid,
					 (IMPProtocol*)NULL);
    XIMPreeditCaretCallbackStruct *caret =
      (XIMPreeditCaretCallbackStruct *)&preedit->todo.caret;

    if (client == (XimpClient *)NULL) return False;

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_PREEDITCARET4 :
			 XIMP_PREEDITCARET3),
			preedit->icid,
			caret->position,
			caret->direction,
			caret->style);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpStatusStartCallback(XIMS xims,
			 IMPProtocol *call_data)
#else
_XimpStatusStartCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPStatusCBStruct *status =
      (XIMPStatusCBStruct*)&call_data->status_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)status->icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_STATUSSTART4 :
			 XIMP_STATUSSTART3),
			status->icid,
			0, 0, 0);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpStatusDoneCallback(XIMS xims,
			IMPProtocol *call_data)
#else
_XimpStatusDoneCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPStatusCBStruct *status =
      (XIMPStatusCBStruct*)&call_data->status_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)status->icid,
					 (IMPProtocol*)NULL);
    if (client == (XimpClient *)NULL) return False;

    _XimpSendIMProtocol(core, client,
			(IS_VERSION_40(client) ?
			 XIMP_STATUSDONE4 :
			 XIMP_STATUSDONE3),
			status->icid,
			0, 0, 0);
    return True;
}

int
#if NeedFunctionPrototypes
_XimpStatusDrawCallback(XIMS xims,
			IMPProtocol *call_data)
#else
_XimpStatusDrawCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    Display *display = core->display;
    Window ims_window = core->im_window;
    XIMPStatusCBStruct *status =
      (XIMPStatusCBStruct*)&call_data->status_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)status->icid,
					 (IMPProtocol*)NULL);
    XIMStatusDrawCallbackStruct *draw =
      (XIMStatusDrawCallbackStruct *)&status->todo.draw;
    register int n;
    Atom atom1, atom2;
    char name[20];

    if (client == (XimpClient *)NULL) return False;

    switch (draw->type) {
      case XIMTextType:
	atom1 = XInternAtom(display, MakeNewAtom(name), False);
	atom2 = XInternAtom(display, MakeNewAtom(name), False);
	/* Atom 1 */
	XChangeProperty(display, ims_window, atom1, core->atoms.ctext_type,
			8, PropModeReplace,
			(unsigned char *)draw->data.text->string.multi_byte,
			draw->data.text->length);
	/* Atom 2 */
	for (n = 0; draw->data.text->feedback[n] != 0; n++) ;
	XChangeProperty(display, ims_window, atom2, core->atoms.feedbacks,
			32, PropModeReplace,
			(unsigned char *)draw->data.text->feedback, n);

	_XimpSendIMProtocol(core, client,
			    (IS_VERSION_40(client) ?
			     XIMP_STATUSDRAW4 :
			     XIMP_STATUSDRAW3),
			    status->icid,
			    XIMTextType, atom1, atom2);
	break;
      case XIMBitmapType:
	_XimpSendIMProtocol(core, client,
			    (IS_VERSION_40(client) ?
			     XIMP_STATUSDRAW4 :
			     XIMP_STATUSDRAW3),
			    status->icid,
			    XIMBitmapType, (CARD32)draw->data.bitmap, 0);
	break;
      default:
	return False;
    }
    return True;
}

int
#if NeedFunctionPrototypes
_XimpStatusDrawCMCallback(XIMS xims,
			  IMPProtocol *call_data)
#else
_XimpStatusDrawCMCallback(xims, call_data)
XIMS xims;
IMPProtocol *call_data;
#endif
{
    XIMPCore core = (XIMPCore)xims->protocol;
    XIMPStatusCBStruct *status =
      (XIMPStatusCBStruct*)&call_data->status_cb;
    XimpClient *client = _XimpFindClient(core, (CARD32)status->icid,
					 (IMPProtocol*)NULL);
    XIMStatusDrawCallbackStruct *draw =
      (XIMStatusDrawCallbackStruct *)&status->todo.draw;

    if (client == (XimpClient *)NULL) return False;

    switch (draw->type) {
      case XIMTextType:
	/* First ClientMessage */
	_XimpSendIMProtocol(core, client,
			    (IS_VERSION_40(client) ?
			     XIMP_STATUSDRAW_CM4 :
			     XIMP_STATUSDRAW_CM3),
			    status->icid,
			    XIMTextType, draw->data.text->feedback[0], 0);
	/* Second ClientMessage, containing status string  */
	_XimpSendByClientMessage(core,
				 status->icid,
				 draw->data.text->string,
				 draw->data.text->length);
	break;
      case XIMBitmapType:
	_XimpSendIMProtocol(core, client,
			    (IS_VERSION_40(client) ?
			     XIMP_STATUSDRAW_CM4 :
			     XIMP_STATUSDRAW_CM3),
			    status->icid,
			    XIMBitmapType, (CARD32)draw->data.bitmap, 0);
	break;
      default:
	return False;
    }
    return True;
}
#endif	/* _XIMCLBK_C_ */
