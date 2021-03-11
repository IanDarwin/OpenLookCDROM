/* fontpanel.c
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

#include <math.h>

/* Xt toolkit headers */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>

/* Motif widget headers */
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushBG.h>
#include <Xm/Separator.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>

/* DPS */
#include <DPS/dpsXclient.h>
#include <DPS/FontSB.h>

#include "globals.h"
#include "wraps.h"

#define MAXLINESIZE 256

static char currentfont[256] = "Helvetica";
static float currentsize = 16.0;
static Widget textBox, textLabel, textColumn;

float GetFontSize ()
{
    return(currentsize);
}

char *GetFontPName()
{
    return(currentfont);
}

static void (*actionCallback)();

void DoAction(label, defaultText, okCallback)
    String label, defaultText;
    XtCallbackList okCallback;
{
    String text;
    Arg args[5];
    int i;

    i=0;
    XtSetArg(args[i], XmNvalue, &text); i++;
    XtGetValues(textBox, args, i);

    (*actionCallback)(textBox, (XtPointer) NULL, text);

    i=0;
    XtSetArg(args[i], XmNvalue, ""); i++;
    XtSetValues(textBox, args, i);
    XtSetSensitive(textColumn, False);
}

void SetUpText(label, defaultText, okCallback)
    String label, defaultText;
    void (*okCallback)();
{
    Arg args[5];
    int i;
    XmString xmscratch;

    xmscratch = XmStringCreate(label, XmSTRING_DEFAULT_CHARSET);
    i=0;
    XtSetArg(args[i], XmNlabelString, xmscratch); i++;
    XtSetValues(textLabel, args, i);
    XmStringFree(xmscratch);

    i = 0;
    XtSetArg(args[i], XmNvalue, defaultText); i++;
    XtSetValues(textBox, args, i);

    actionCallback = okCallback;
    XtSetSensitive(textColumn, True);
    XmProcessTraversal(textBox, XmTRAVERSE_CURRENT);
}

/* ARGSUSED */

void CheckFont(widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    FSBValidateCallbackRec *cb = (FSBValidateCallbackRec *) callData;

    if (cb->family_selection != FSBOne && cb->face_selection != FSBOne &&
	cb->size_selection != FSBOne && cb->name_selection != FSBOne) {
	cb->doit = FALSE;
    }
}

void SetFont(widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    FSBCallbackRec *cb = (FSBCallbackRec *) callData;

    (void)strcpy(currentfont, cb->name);
    currentsize = cb->size;
    PSselectfont(currentfont, currentsize);
}

static void PopupFontPanel (widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    static Widget shell = NULL;
    Widget panel;
    Widget parent = (Widget) clientData;

    if (shell == NULL) {
	shell = XtCreatePopupShell("Fonts", transientShellWidgetClass,
				   parent, (ArgList) NULL, 0);

	panel = XtCreateManagedWidget("fontPanel",
				      fontSelectionBoxWidgetClass,
				      shell, (ArgList) NULL, 0);

	FSBSetFontName(panel, currentfont, False);
	FSBSetFontSize(panel, currentsize, False);
	XtAddCallback(panel, XtNvalidateCallback, CheckFont, (XtPointer) NULL);
	XtAddCallback(panel, XtNokCallback, SetFont, (XtPointer) NULL);
	XtAddCallback(panel, XtNapplyCallback, SetFont, (XtPointer) NULL);
    }	

    XtPopup(shell, XtGrabNone);
    XRaiseWindow(XtDisplay(shell), XtWindow(shell));
}

void DesensitizeText(widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    Arg args[5];
    int i;

    i=0;
    XtSetArg(args[i], XmNvalue, ""); i++;
    XtSetValues(textBox, args, i);
    XtSetSensitive(textColumn, False);
}

Widget CreateTextPanel(parent)
    Widget parent;
{
    Arg args[10];
    Widget frame, row, button;
    int i;

    i=0;
    frame = XmCreateFrame(parent, "frame", args, i);
    XtManageChild(frame);

    textColumn = XmCreateRowColumn(frame, "demoPanel", NULL, 0);
    XtManageChild(textColumn);

    i=0;
    textLabel = (Widget) XmCreateLabelGadget(textColumn, "textLabel", args, i);
    XtManageChild(textLabel);

    i=0;
    textBox = XmCreateTextField(textColumn, "textField", args, i);
    XtManageChild(textBox);
    XtAddCallback(textBox, XmNactivateCallback,
		  (XtCallbackProc) DoAction, (XtPointer) NULL);

    i=0;
    row = XmCreateForm(textColumn, "demoPanel", args, i);
    XtManageChild(row);

    i=0;
    XtSetArg(args[i], XmNshowAsDefault, 1); i++;
    XtSetArg(args[i], XmNleftAttachment, XmATTACH_FORM); i++;
    XtSetArg(args[i], XmNrightAttachment, XmATTACH_POSITION); i++;
    XtSetArg(args[i], XmNrightPosition, 50); i++;
    XtSetArg(args[i], XmNtopAttachment, XmATTACH_FORM); i++;
    XtSetArg(args[i], XmNbottomAttachment, XmATTACH_FORM); i++;
    button = XmCreatePushButtonGadget(row, "okButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) DoAction, (XtPointer) NULL);

    i=0;
    XtSetArg(args[i], XmNrightAttachment, XmATTACH_FORM); i++;
    XtSetArg(args[i], XmNleftAttachment, XmATTACH_POSITION); i++;
    XtSetArg(args[i], XmNleftPosition, 50); i++;
    XtSetArg(args[i], XmNtopAttachment, XmATTACH_FORM); i++;
    XtSetArg(args[i], XmNbottomAttachment, XmATTACH_FORM); i++;
    button = XmCreatePushButtonGadget(row, "cancelButton", args, i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  DesensitizeText, (XtPointer) NULL);

    XtSetSensitive(textColumn, False);
    return(frame);
}

Widget CreateFontPanel (parent)
    Widget parent;
{
    Arg args[15];
    Widget column, title, fontBox, demoPanel, textPanel, widget;
    int i, count = 0;
    extern void Quit();

    i=0; XtSetArg(args[i], XmNleftAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNtopAttachment, XmATTACH_FORM);
    column = XmCreateRowColumn(parent, "fontPanel", args, i);
    XtManageChild(column);

    title = XmCreateLabel(column, "title", NULL, 0);
    XtManageChild(title);

    XtManageChild(XmCreateSeparator(column, "separator", NULL, 0));

    i=0;
    widget = XmCreatePushButtonGadget(column, "quitButton", args, i);
    XtManageChild(widget);
    XtAddCallback(widget, XmNactivateCallback, Quit, NULL);

    XtManageChild(XmCreateSeparator(column, "separator2", NULL, 0));

    i=0;
    widget = XmCreatePushButtonGadget(column, "fontButton", args, i);
    XtManageChild(widget);
    XtAddCallback(widget, XmNactivateCallback, PopupFontPanel,
		  (XtPointer) column);

    XtManageChild(XmCreateSeparator(column, "separator3", NULL, 0));

    demoPanel = CreateDemoPanel(column);
    textPanel = CreateTextPanel(column);

    PopupFontPanel((Widget) NULL, (XtPointer) column, (XtPointer) NULL);
    PSselectfont(currentfont, currentsize);
    return(column);
}
