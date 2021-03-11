#ifndef lint
static char *rcsid = "$Id: imconv.c,v 1.7 1994/06/02 02:20:43 ishisone Exp $";
#endif
/*
 * Copyright (c) 1991, 1994  Software Research Associates, Inc.
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

#include "im.h"
#include "ConvMgr.h"
#include "OverConv.h"
#include "OffConv.h"
#include "InputConv.h"

static void fillDefaultAttributesForStartup _Pt_((IMIC *icp));
static void computeAreaForStartup _Pt_((IMIC *icp));
static unsigned long makeConvAttributesForStartup _Pt_((IMIC *icp,
							ConversionAttributes *attrp));
static void commitString _Pt_((IMIC *icp, char *str, int len, int sync));
static void fixCallback _Pt_((Widget w, XtPointer client_data,
			      XtPointer call_data));
static void detachConverter _Pt_((IMIC *icp));
static void endCallback _Pt_((Widget w, XtPointer client_data,
			      XtPointer call_data));
static void unusedEventCallback _Pt_((Widget w, XtPointer client_data,
				      XtPointer call_data));


/*- fillDefaultAttributesForStartup: put default necessary for conv. start -*/
static void
fillDefaultAttributesForStartup(icp)
IMIC *icp;
{
    unsigned long cmask, pmask, smask;

    cmask = ATTR_MASK_FOCUS;

    switch (icp->style) {
    case IMSTYLE_OVER_THE_SPOT:
	pmask = ATTR_MASK_FOREGROUND | ATTR_MASK_BACKGROUND |
	    ATTR_MASK_FONT_SET;
	smask = 0;
	break;
    case IMSTYLE_OFF_THE_SPOT:
	pmask = ATTR_MASK_FOREGROUND | ATTR_MASK_BACKGROUND |
	    ATTR_MASK_FONT_SET;
	smask = ATTR_MASK_AREA;
	break;
    default:
	pmask = 0;
	smask = 0;
    }
    IMFillDefault(icp, cmask, pmask, smask);
}

/*- computeAreaForStartup: compute default area for conversion startup -*/
static void
computeAreaForStartup(icp)
IMIC *icp;
{
    IMPSAttributes *pattr = &icp->preedit_attr;
    IMPSAttributes *sattr = &icp->status_attr;

    TRACE(("imlib:computeAreaForStartup()\n"));

    if (icp->style == IMSTYLE_SEPARATE ||
	icp->style == IMSTYLE_OVER_THE_SPOT) {
	/*
	 * These styles don't need status nor preedit area.
	 * The separate style simpley ignores them, and the
	 * over-the-spot style uses default value if not specified.
	 */
	return;
    }

    if ((pattr->set_mask & ATTR_MASK_AREA) &&
	(sattr->set_mask & ATTR_MASK_AREA)) {
	/*
	 * Both of preedit area and status area has been specified.
	 * No need to compute.
	 */
	return;
    }

    /*
     * Compute default status/pre-edit area based on the AreaNeeded values.
     */
    IMFillDefault(icp, (unsigned long)0,
		  (unsigned long)ATTR_MASK_AREA_NEEDED,
		  (unsigned long)ATTR_MASK_AREA_NEEDED);

    if (!(sattr->set_mask & ATTR_MASK_AREA)) {
	sattr->area = sattr->area_needed;
    }
    if (!(pattr->set_mask & ATTR_MASK_AREA)) {
	pattr->area = pattr->area_needed;
    }
}

/*- makeConvAttributesForStartup: get conv. attrs needed for startup -*/
static unsigned long
makeConvAttributesForStartup(icp, attrp)
IMIC *icp;
ConversionAttributes *attrp;
{
    icp->common_attr.change_mask = icp->common_attr.set_mask;
    icp->preedit_attr.change_mask = icp->preedit_attr.set_mask;
    icp->status_attr.change_mask = icp->status_attr.set_mask;
    return IMMakeConvAttributes(icp, attrp);
}

/*- commitString: commmit converted string to client -*/
static void
commitString(icp, str, len, sync)
IMIC *icp;
char *str;
int len;
int sync;
{
    int offset;
    IMConnection *conn = icp->im->connection;
    unsigned int flag;

    TRACE(("imlib:commitString()\n"));

    if (DDEBUG_CONDITION(5)) {
	unsigned char *p = (unsigned char *)str;
	int i;

	/*
	 * Dump commiting string.
	 */
	printf("* commit string:\n\t");
	for (i = 0; i < len; i++, p++) {
	    if (*p == '\033') {
		printf("ESC ");
	    } else if (*p < ' ') {
		printf("^%c ", *p + '@');
	    } else if (*p == ' ') {
		printf("sp ");
	    } else if (*p >= 0x7f) {
		printf("%x ", *p);
	    } else {
		printf("%c ", *p);
	    }
	}
	printf("\n");
    }

    flag = XIM_FLAG_X_LOOKUP_CHARS;
    if (sync) flag |= XIM_FLAG_SYNCHRONOUS;

    offset = IMPutHeader(conn, XIM_COMMIT, 0, 0);
    IMPutC16(conn, icp->im->id);
    IMPutC16(conn, icp->id);
    IMPutC16(conn, flag);
    IMPutC16(conn, (unsigned int)len);
    IMPutString(conn, str, len);
    IMFinishRequest(conn, offset);
}

/*- fixCallback: fix callback -*/
/* ARGSUSED */
static void
fixCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    IMIC *icp = (IMIC *)client_data;
    IMConnection *conn = icp->im->connection;
    Widget proto = conn->proto_widget;
    Atom ctext = IMCtextAtom(proto);
    CCTextCallbackArg *arg = (CCTextCallbackArg *)call_data;

    TRACE(("imlib:fixCallback()\n"));

    /* check encoding and format */
    if (arg->encoding != ctext || arg->format != 8) {
	/*
	 * since every conversion object must support COMPOUND_TEXT,
	 * it is a serious error.
	 */
	String params[2];
	Cardinal num_params;
	WidgetClass ioc = icp->im->converter->input_object_class;

	params[0] = XtClass(proto)->core_class.class_name;
	params[1] = ioc->core_class.class_name;
	num_params = 2;

	XtAppErrorMsg(XtWidgetToApplicationContext(proto),
		      "encodingError", "convertedString", "WidgetError",
		      "%s: encoding of the converted string is not COMPOUND_STRING. check inputObject %s",
		      params, &num_params);
    }

    /*
     * Send fixed string via XIM_COMMIT message.
     */
    commitString(icp, arg->text, arg->length, 0);	/* ??? sync flag? */
}

/*- detachConverter: detach conversion widget from specified IC -*/
static void
detachConverter(icp)
IMIC *icp;
{
    Widget conv;

    TRACE(("imlib:detachConverter()\n"));

    conv = icp->conversion;
    XtRemoveCallback(conv, XtNtextCallback, fixCallback, (XtPointer)icp);
    XtRemoveCallback(conv, XtNendCallback, endCallback, (XtPointer)icp);
    XtRemoveCallback(conv, XtNunusedEventCallback, unusedEventCallback, (XtPointer)icp);

    CMReleaseConverter(XtParent(icp->im->connection->proto_widget), conv);
    icp->conversion = NULL;
}

/*- endCallback: conversion end callback -*/
/* ARGSUSED */
static void
endCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    IMIC *icp = (IMIC *)client_data;

    TRACE(("imlib:endCallback()\n"));

    if (icp->state & IC_CONVERTING) {
	detachConverter(icp);
	icp->state &= ~IC_CONVERTING;
    }
}

/*- unusedEventCallback: unused key event callback -*/
/* ARGSUSED */
static void
unusedEventCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    IMIC *icp = (IMIC *)client_data;
    IMConnection *conn = icp->im->connection;
    XKeyEvent *ev = (XKeyEvent *)call_data;
    int offset;

    TRACE(("imlib:unusedEventCallback()\n"));

    if (icp->im->mask & XIM_EXT_FORWARD_KEYEVENT_MASK) {
	offset = IMPutHeader(conn, XIM_EXT_FORWARD_KEYEVENT, 0, 0);
	IMPutC16(conn, icp->im->id);
	IMPutC16(conn, icp->id);
	IMPutC16(conn, 0);
	IMPutC16(conn, (unsigned int)(ev->serial & 0xffff));
	IMPutC8(conn, ev->type);
	IMPutC8(conn, (int)ev->keycode);
	IMPutC16(conn, (unsigned int)ev->state);
	IMPutC32(conn, ev->time);
	IMPutC32(conn, ev->window);
	IMFinishRequest(conn, offset);
    } else {
	offset = IMPutHeader(conn, XIM_FORWARD_EVENT, 0, 0);
	IMPutC16(conn, icp->im->id);
	IMPutC16(conn, icp->id);
	IMPutC16(conn, 0);	/* ?? */
	IMPutC16(conn, (unsigned int)((ev->serial >> 16) & 0xffff));

	IMPutC8(conn, ev->type);
	IMPutC8(conn, (int)ev->keycode);
	IMPutC16(conn, (unsigned int)(ev->serial & 0xffff));
	IMPutC32(conn, ev->time);
	IMPutC32(conn, ev->root);
	IMPutC32(conn, ev->window);
	IMPutC32(conn, ev->subwindow);
	IMPutI16(conn, ev->x_root);
	IMPutI16(conn, ev->y_root);
	IMPutI16(conn, ev->x);
	IMPutI16(conn, ev->y);
	IMPutC16(conn, ev->state);
	IMPutC8(conn, ev->same_screen);

	IMFinishRequest(conn, offset);
    }
}


/*
 * Public functions
 */

int
IMStartConversion(icp)
IMIC *icp;
{
    IMIM *imp = icp->im;
    Widget proto = imp->connection->proto_widget;
    Widget converter;
    WidgetClass class;
    unsigned long attrmask;
    ConversionAttributes attrs;

    TRACE(("IMStartConversion()\n"));

    if (icp->state & IC_CONVERTING) return 0;

    /*
     * Check required attributes i.e. client window.
     */
    if (!(icp->common_attr.set_mask & ATTR_MASK_CLIENT)) {
	IMSendError(icp->im->connection, IMBadSomething, icp->im->id, icp->id,
		    "client window required");
	return -1;
    }

    /*
     * Fill in default values for unspecified attributes.
     */
    fillDefaultAttributesForStartup(icp);

    /*
     * Compute Area attributes.
     */
    computeAreaForStartup(icp);

    /*
     * Get appropriate converter class.
     */
    if (icp->style == IMSTYLE_OVER_THE_SPOT) {
	class = overTheSpotConversionWidgetClass;
    } else if (icp->style == IMSTYLE_OFF_THE_SPOT) {
	class = offTheSpotConversionWidgetClass;
    } else {
	class = separateConversionWidgetClass;
    }

    /*
     * Compute conversion attributes to be passed to the converter.
     */
    attrmask = makeConvAttributesForStartup(icp, &attrs);

    /*
     * Attach converter to this IC.
     */
    converter = CMGetConverter(XtParent(proto),
			       icp->common_attr.client, class,
			       imp->converter->input_object_class,
			       imp->converter->display_object_class);
    if (converter == NULL) {
	IMSendError(imp->connection, IMBadSomething, imp->id, icp->id,
		    "can't attach converter to this IC");
	return -1;
    }
    icp->conversion = converter;

    /*
     * Add callback functions.
     */
    XtAddCallback(converter, XtNtextCallback, fixCallback, (XtPointer)icp);
    XtAddCallback(converter, XtNendCallback, endCallback, (XtPointer)icp);
    XtAddCallback(converter, XtNunusedEventCallback, unusedEventCallback, (XtPointer)icp);

    /*
     * Start conversion
     */
    /* !!! if front-end method is used, ESMethodSelectFocus should be used */
    XtVaSetValues(converter, XtNeventSelectMethod, ESMethodNone, NULL);
    CControlStartConversion(converter, icp->common_attr.client,
			    attrmask, &attrs);

    icp->state |= IC_CONVERTING;
    return 0;
}

void
IMStopConversion(icp)
IMIC *icp;
{
    TRACE(("IMStopConversion()\n"));

    if (!(icp->state & IC_CONVERTING)) return;

    /*
     * Terminate conversion.
     */
    CControlEndConversion(icp->conversion);

    /*
     * Detach converter.
     */
    detachConverter(icp);

    icp->state &= ~IC_CONVERTING;
}

int
IMResetIC(icp, preedit_strp)
IMIC *icp;
char **preedit_strp;
{
    int num_bytes = 0;

    TRACE(("IMResetIC()\n"));

    *preedit_strp = NULL;

    if (icp->state & IC_CONVERTING) {
	/*
	 * get input object by asking conversion widget of XtNinputObject
	 * resource. however, it is not recommended since protocol widget
	 * should interact with input object only through conversion
	 * widget.
	 */
	CCTextCallbackArg arg;
	Widget input_obj;
	Widget w = icp->im->connection->proto_widget;

	XtVaGetValues(icp->conversion, XtNinputObject, &input_obj, NULL);
	arg.encoding = IMCtextAtom(w);
	if (ICGetConvertedString(input_obj, &arg.encoding, &arg.format,
				 &arg.length, &arg.text) >= 0) {
	    num_bytes = arg.length;
	    *preedit_strp = (char *)arg.text;
	}
	ICClearConversion(input_obj);
	TRACE(("\twas converting. %d bytes left\n", num_bytes));
    }
    return num_bytes;
}

void
IMForwardEvent(icp, ev)
IMIC *icp;
XEvent *ev;
{
    TRACE(("IMForwardEvent()\n"));

    if (icp->conversion == NULL) return;
    XtCallActionProc(icp->conversion, "to-inputobj", ev,
		     (String *)NULL, (Cardinal)0);
}

/* ARGSUSED */
void
IMSetFocus(icp)
IMIC *icp;
{
    TRACE(("IMSetFocus(ic%d)\n", icp->id));
    if (icp->conversion != NULL) {
	CControlChangeFocus(icp->conversion, 1);
    }
}

/* ARGSUSED */
void
IMUnsetFocus(icp)
IMIC *icp;
{
    TRACE(("IMUnsetFocus(ic%d)\n", icp->id));
    if (icp->conversion != NULL) {
	CControlChangeFocus(icp->conversion, 0);
    }
}
