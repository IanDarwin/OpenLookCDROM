/* canvas.c
 *
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

/* C headers */
#include <stdio.h>

/* Xt toolkit headers */
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

/* Motif widget headers */
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/SelectioB.h>
#include <Xm/ScrollBar.h>

/* DPS library headers */
#include <DPS/dpsXclient.h>
#include <DPS/dpsops.h>
#include <DPS/psops.h>

/* local */
#include "globals.h"
#include "wraps.h"

static struct {
    Pixmap canvas;
    int width;
    int height;
    int offset;
} gb;

static Widget demoCanvas, scrollBar;

void ChangeScrollOffset (widget, client_data, cb)
    Widget widget;
    caddr_t client_data;
    XmScrollBarCallbackStruct *cb;
{
    Arg args[2];
    Dimension width, height;

    gb.offset = cb->value;

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);
 
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
    XFlush(XtDisplay(demoCanvas));
}

void ScrollDemoCanvas (distance)
    int distance;
{
    Arg args[5];
    Dimension width, height;

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    DPSWaitContext(DPSGetCurrentContext());

    if (distance > gb.height) distance = gb.height;

    XCopyArea(XtDisplay(demoCanvas), gb.canvas, gb.canvas, defaultGC, 0,
          distance, gb.width, gb.height-distance, 0, 0);

    XFillRectangle(XtDisplay(demoCanvas), gb.canvas, defaultGC,
          0, gb.height-distance, gb.width, distance);

    gb.offset = gb.height - height; /* scroll to bottom of buffer */
    XtSetArg(args[0], XmNvalue, gb.offset);
    XtSetValues(scrollBar, args, 1);
}

static void SetupBuffers (inbuf, outbuf)
    char *inbuf;
    char *outbuf;
{
    int c;
    int i=0;

    for (c =  65; c < 127; c++) inbuf[i++] = (char) c;
    for (c =  32; c <  65; c++) inbuf[i++] = (char) c;
    for (c = 161; c < 176; c++) inbuf[i++] = (char) c;
    for (c = 177; c < 181; c++) inbuf[i++] = (char) c;
    for (c = 182; c < 190; c++) inbuf[i++] = (char) c;
    for (c = 193; c < 201; c++) inbuf[i++] = (char) c;
    for (c = 205; c < 209; c++) inbuf[i++] = (char) c;
    for (c = 232; c < 236; c++) inbuf[i++] = (char) c;
    for (c = 248; c < 252; c++) inbuf[i++] = (char) c;
    inbuf[i++] = (char) 191;
    inbuf[i++] = (char) 202;
    inbuf[i++] = (char) 203;
    inbuf[i++] = (char) 225;
    inbuf[i++] = (char) 227;
    inbuf[i++] = (char) 241;
    inbuf[i++] = (char) 245;
    inbuf[i] = '\0';
}

void DrawTextWheel (widget, client_data, text)
    Widget widget;
    caddr_t client_data;
    String text;
{
    Arg args[5];
    DPSContext ctxt;
    Dimension width, height;
    float swidth, temp = 0., pssize, sx, dx, dy;

    ctxt = DPSGetCurrentContext();
    PSWSetXOffset(ctxt, 0, gb.height);

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    pssize = GetFontSize();
    PSstringwidth(text, &sx, &temp);
    PStransform(temp, 2.0*((pssize*2)+sx), &temp, &dy);
    PSitransform((float)width, temp, &dx, &temp);

    dx -= pssize;
    PSmoveto(pssize+(dx/2.0), sx+(2.0*pssize));
    ScrollDemoCanvas(-(int)dy);
    PSWtextwheel(text);
    DPSWaitContext(ctxt);
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
}
    
void TextWheelDialog (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    SetUpText("Enter wheel text", "Display PostScript", DrawTextWheel);
}

void ShowFountain(widget, client_data, text)
    Widget widget;
    caddr_t client_data;
    String text;
{
    Arg args[5];
    DPSContext ctxt;
    Dimension width, height;
    int rr;
    float temp = 0., big, small, ln, dy, dsize;
    char *fname;
    extern char *GetFontPName();

    ctxt = DPSGetCurrentContext();
    PSWSetXOffset(ctxt, 0, gb.height);

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    big = GetFontSize(); small = 5.; dsize = 1.; 
    fname = GetFontPName();

    for (ln = big; ln > small; ln -= dsize)
    {
    dsize = (ln > 36.) ? ln * 0.1 : ((ln > 18.) ? 2. : 1);
    rr = dsize;
    dsize = (rr < 1) ? 1. : rr;
    PSmoveto(8., .5*ln);
    PStransform(temp, 1.1*ln, &temp, &dy);
    ScrollDemoCanvas(-(int)dy);
    PSselectfont(fname, ln);
    PSshow(text);
    DPSPrintf(ctxt, " %4.1f 0 rmoveto (%4.1f) show\n", ln, ln);
    DPSWaitContext(ctxt);
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
    }
    ScrollDemoCanvas(50);
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
    PSselectfont(fname, big);
}

void FountainDialog (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    SetUpText("Enter fountain text",
	      "The quick brown fox jumps over the lazy dog",
	      ShowFountain);
}

void DrawOutlineText (widget, client_data, text)
    Widget widget;
    caddr_t client_data;
    String text;
{
    Arg args[5];
    DPSContext ctxt;
    Dimension width, height;
    float temp = 0., dy;

    ctxt = DPSGetCurrentContext();
    PSWSetXOffset(ctxt, 0, gb.height);

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    PStransform(temp, 1.5*GetFontSize(), &temp, &dy);

    PSmoveto(25.0, .5*GetFontSize());
    ScrollDemoCanvas(-(int)dy);
    PSWoutlinetext(text);
    DPSWaitContext(ctxt);
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
}

void Quit (widget, client_data, cb)
    Widget widget;
    caddr_t client_data;
    XmSelectionBoxCallbackStruct *cb;
{
    exit(0);
}

void DrawSlantedText (widget, client_data, text)
    Widget widget;
    caddr_t client_data;
    String text;
{
    Arg args[5];
    DPSContext ctxt;
    Dimension width, height;
    float temp = 0., dy;

    ctxt = DPSGetCurrentContext();
    PSWSetXOffset(ctxt, 0, gb.height);

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    PStransform(temp, 1.5*GetFontSize(), &temp, &dy);

    PSmoveto(25.0, .5*GetFontSize());
    ScrollDemoCanvas(-(int)dy);
    PSWslantedtext(text);
    DPSWaitContext(ctxt);
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
}

void SlantedTextDialog (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    SetUpText("Enter slanted text", "Slanted Text", DrawSlantedText);
}

void OutlineTextDialog (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    SetUpText("Enter outline text", "Outline Text", DrawOutlineText);
}

void DrawQuotation (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    Arg args[5];
    Dimension width, height;
    float pssize, psx, psy, dx, dy;
    DPSContext ctxt;

    ctxt = DPSGetCurrentContext();
    PSWSetXOffset(ctxt, 0, gb.height);

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    pssize = GetFontSize();
    psx = 0.0;
    psy = pssize*1.5*8;

    PStransform(psx, psy, &dx, &dy);

    ScrollDemoCanvas(-(int)dy);

    PSmoveto(25.0, psy-pssize);
    PSWquotation(pssize);

    DPSWaitContext(ctxt);
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
}

void DrawCharSet (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    Arg args[5];
    DPSContext ctxt;
    static char *inbuf, *outbuf;
    char *ptr;
    float widths[256], pswidth, temp = 0., dy, pssize, swidth;
    int i, j;
    Dimension width, height;

    XtSetArg(args[0], XmNwidth, &width);
    XtSetArg(args[1], XmNheight, &height);
    XtGetValues(demoCanvas, args, 2);

    ctxt = DPSGetCurrentContext();
    PSWSetXOffset(ctxt, 0, gb.height);

    if ((inbuf == NULL)||(outbuf == NULL)) {
        inbuf = (char*) malloc(512*(unsigned)sizeof(char));
        outbuf = (char*) malloc(512*(unsigned)sizeof(char));
        SetupBuffers(inbuf, outbuf);
    }

    PSitransform((float)width, temp, &pswidth, &temp);
    PSWStringCharWidths(inbuf, widths); /* get widths of each char in string */

    pssize = GetFontSize();
    pswidth -= 50.0;
    PStransform(temp, pssize*1.2, &temp, &dy);

    for (i=0; (inbuf[i] != '\0');) {
	swidth=0.0;
        ptr = &(inbuf[i]);
        for (j=0; ((swidth < pswidth) && (inbuf[i] != '\0')); j++) {
            swidth += widths[i++];
        } 
        strncpy(outbuf, ptr, j);
        outbuf[j] = '\0';
        PSmoveto(25.0, .5*pssize);
        ScrollDemoCanvas(-(int)dy);
        PSshow(outbuf);
    }

    DPSWaitContext(DPSGetCurrentContext());
    XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
          defaultGC, 0, gb.offset, width, height, 0, 0);
}

void ResizeDemoCanvas (widget, client_data, cb)
    Widget widget;
    caddr_t client_data;
    XmDrawingAreaCallbackStruct *cb;
{
    Arg args[5];
    DPSContext ctxt;
    int i;
    Dimension width, height;

    XtSetArg(args[0], XmNheight, &height);
    XtSetArg(args[1], XmNwidth, &width);
    XtGetValues(demoCanvas, args, 2);

    if (height != gb.height-gb.offset) {
        gb.offset = gb.height - height;
        if (XtWindow(demoCanvas)) {
            XCopyArea(XtDisplay(demoCanvas), gb.canvas, XtWindow(demoCanvas),
                  defaultGC, 0, gb.offset, width, height, 0, 0);
        }
        i=0; XtSetArg(args[i], XmNsliderSize, height);
        i++; XtSetArg(args[i], XmNminimum, 0);
        i++; XtSetArg(args[i], XmNmaximum, gb.height);
        i++; XtSetArg(args[i], XmNincrement, height/4);
        i++; XtSetArg(args[i], XmNvalue, gb.offset);
        i++; XtSetValues(scrollBar, args, i);
    }
}

void RefreshDemoCanvas (widget, client_data, cb)
    Widget widget;
    caddr_t client_data;
    XmDrawingAreaCallbackStruct *cb;
{
    do {
        XCopyArea(XtDisplay(widget), gb.canvas, cb->window, defaultGC,
              cb->event->xexpose.x, cb->event->xexpose.y+gb.offset,
              cb->event->xexpose.width, cb->event->xexpose.height,
              cb->event->xexpose.x, cb->event->xexpose.y);
    } while (XCheckTypedWindowEvent(
          XtDisplay(widget), cb->window, Expose, cb->event));
}

Widget CreateDemoPanel (parent)
    Widget parent;
{
    Arg args[5];
    Widget frame, column, button;
    int i;

    i=0;
    frame = XmCreateFrame(parent, "frame", args, i);
    XtManageChild(frame);

    column = XmCreateRowColumn(frame, "demoPanel", NULL, 0);
    XtManageChild(column);
    
    i=0;
    button = XmCreatePushButtonGadget(column, "charSetButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) DrawCharSet, NULL);

    i=0;
    button = XmCreatePushButtonGadget(column, "quotationButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) DrawQuotation, NULL);

    i=0;
    button = XmCreatePushButtonGadget(column, "textwheelButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) TextWheelDialog, NULL);

    i=0;
    button = XmCreatePushButtonGadget(column, "outlineTextButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) OutlineTextDialog, NULL);

    i=0;
    button = XmCreatePushButtonGadget(column, "slantedTextButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) SlantedTextDialog, NULL);

    i=0;
    button = XmCreatePushButtonGadget(column, "fountainTextButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) FountainDialog, NULL);

    return(frame);
}

Widget CreateDemoCanvas (panel, form)
    Widget panel, form;
{
    Arg args[10];
    int i;

    i=0; XtSetArg(args[i], XmNtopAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNbottomAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNrightAttachment, XmATTACH_FORM);
    i++; scrollBar = XmCreateScrollBar(form, "scrollBar", args, i);
    XtManageChild(scrollBar);

    i=0; XtSetArg(args[i], XmNtopAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNleftWidget, panel);
    i++; XtSetArg(args[i], XmNleftAttachment, XmATTACH_WIDGET);
    i++; XtSetArg(args[i], XmNbottomAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNrightWidget, scrollBar);
    i++; XtSetArg(args[i], XmNrightAttachment, XmATTACH_WIDGET);
    i++; demoCanvas = XmCreateDrawingArea(form, "demoCanvas", args, i);
    XtManageChild(demoCanvas);

    XtAddCallback(scrollBar, XmNdragCallback,
		  (XtCallbackProc) ChangeScrollOffset, NULL);
    XtAddCallback(scrollBar, XmNvalueChangedCallback,
		  (XtCallbackProc) ChangeScrollOffset, NULL);

    XtAddCallback(demoCanvas, XmNresizeCallback,
		  (XtCallbackProc) ResizeDemoCanvas,  NULL);
    XtAddCallback(demoCanvas, XmNexposeCallback,
		  (XtCallbackProc) RefreshDemoCanvas, NULL);

    gb.width = WidthOfScreen(XtScreen(demoCanvas));
    gb.height = 2*HeightOfScreen(XtScreen(demoCanvas));
    gb.offset = 0;
    gb.canvas = XCreatePixmap(XtDisplay(demoCanvas), 
          RootWindowOfScreen(XtScreen(demoCanvas)), gb.width, gb.height, 
          DefaultDepthOfScreen(XtScreen(demoCanvas)));
    XFillRectangle(XtDisplay(demoCanvas), gb.canvas,
          defaultGC, 0, 0, gb.width, gb.height);
    PSWSetXGCDrawable(
          DPSGetCurrentContext(), XGContextFromGC(defaultGC),
		      gb.canvas, 0, gb.height);
    PSinitmatrix();

    return(demoCanvas);
}
