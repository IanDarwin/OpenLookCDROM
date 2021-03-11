/* main.c
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
#include <math.h>

/* Xt toolkit headers */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* Motif widget headers */
#include <Xm/Xm.h>
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/ToggleBG.h>
#include <Xm/FileSB.h>
#include <DPS/FontSB.h>

/* DPS library headers */
#include <DPS/dpsXclient.h>
#include <DPS/dpsops.h>
#include <DPS/psops.h>

#define __MAIN__
#include "globals.h"
#include "canvas.h"
#include "list.h"
#undef  __MAIN__

extern char *getenv();

static char psfilename[256] = "scratchpad.ps";
static char psprinter[256] = "unknown";
static Boolean SendToPrinter = True;

#ifdef _NO_PROTO
#define ARGCAST int
#else
#define ARGCAST void *
#endif

extern void CheckFont(), SetFont(), InitFonts();

static Widget ArrowBoxGetChild (parent, child)
    Widget parent;
    unsigned int child;
{
    Widget childWidget;

    switch (child) {
      case XmARROW_UP:  
          childWidget = XtNameToWidget(parent, "arrowBox.upArrow");
      break;
      case XmARROW_DOWN:  
          childWidget = XtNameToWidget(parent, "arrowBox.downArrow");
      break;
      case XmARROW_LEFT:  
          childWidget = XtNameToWidget(parent, "arrowBox.leftArrow");
      break;
      case XmARROW_RIGHT:  
          childWidget = XtNameToWidget(parent, "arrowBox.rightArrow");
      break;
    }

    return(childWidget); /* may be null if widget not found */
}

static Widget CreateArrowBox (parent, name, wargs, wcount)
    Widget parent;
    char *name;
    Arg *wargs;
    int wcount;
{
    Arg args[10];
    Widget frame, arrowBox, upArrow, downArrow, leftArrow, rightArrow;
    Widget upLeftArrow, upRightArrow, downLeftArrow, downRightArrow;
    int i;

    frame = XmCreateFrame(parent, name, wargs, wcount);

    i=0; 
    arrowBox = XmCreateBulletinBoard(frame, "arrowBox", args, i);
    XtManageChild(arrowBox);

    i=0; 
    upArrow = XmCreateArrowButtonGadget(arrowBox,"upArrow",args,i);
    XtAddCallback(upArrow, XmNactivateCallback, kernLetters, (XtPointer) UP);
    XtManageChild(upArrow);

    i=0;
    leftArrow = XmCreateArrowButtonGadget(arrowBox,"leftArrow",args,i);
    XtAddCallback(leftArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) LEFT);
    XtManageChild(leftArrow);

    i=0;
    rightArrow = XmCreateArrowButtonGadget(arrowBox,"rightArrow",args,i);
    XtAddCallback(rightArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) RIGHT);
    XtManageChild(rightArrow);
 
    i=0;
    downArrow = XmCreateArrowButtonGadget(arrowBox,"downArrow",args,i);
    XtAddCallback(downArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) DOWN);
    XtManageChild(downArrow);

    i=0;
    upLeftArrow = XmCreatePushButtonGadget(arrowBox,"NW",args,i);
    XtAddCallback(upLeftArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) UP_LEFT);
    XtManageChild(upLeftArrow);

    i=0;
    upRightArrow = XmCreatePushButtonGadget(arrowBox,"NE",args,i);
    XtAddCallback(upRightArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) UP_RIGHT);
    XtManageChild(upRightArrow);

    i=0;
    downLeftArrow = XmCreatePushButtonGadget(arrowBox,"SW",args,i);
    XtAddCallback(downLeftArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) DOWN_LEFT);
    XtManageChild(downLeftArrow);
 
    i=0;
    downRightArrow = XmCreatePushButtonGadget(arrowBox,"SE",args,i);
    XtAddCallback(downRightArrow, XmNactivateCallback, kernLetters,
		  (XtPointer) DOWN_RIGHT);
    XtManageChild(downRightArrow);

    return(frame);
}

static void Exit (widget, status, call_data)
    Widget widget;
    int status;
    caddr_t call_data;
{
    exit(status);
}

static void CancelAnyDialog (dialog, toggle, call_data)
    Widget dialog;
    Widget toggle;
    caddr_t call_data;
{
    Arg arg;

    XtUnmanageChild(dialog);
    XtSetArg(arg, XmNset, False);
    XtSetValues(toggle, &arg, 1);
}

static void PopupDialog (button, dialog, cb)
    Widget button;
    Widget dialog;
    caddr_t cb;
{
    XtManageChild(dialog);
}

static void PopdownDialog (widget, dialog, cb)
    Widget widget;
    Widget dialog;
    caddr_t cb;
{
    XtUnmanageChild(dialog);
}


static void PopupFontPanel (widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    static Widget shell = NULL;
    Widget panel;
    Widget parent = (Widget) clientData;
    bboxStruct fontBBox;
    char fontName[256];
    float fontSize;

    if (shell == NULL) {
	shell = XtCreatePopupShell("Fonts", transientShellWidgetClass,
				   parent, (ArgList) NULL, 0);

	panel = XtCreateManagedWidget("fontPanel",
				      fontSelectionBoxWidgetClass,
				      shell, (ArgList) NULL, 0);

	CurrentFontInfo(fontName, &fontSize, &fontBBox);
	FSBSetFontName(panel, fontName, False);
	FSBSetFontSize(panel, fontSize, False);
	XtAddCallback(panel, XtNvalidateCallback, CheckFont, (XtPointer) NULL);
	XtAddCallback(panel, XtNokCallback, SetFont, (XtPointer) NULL);
	XtAddCallback(panel, XtNapplyCallback, SetFont, (XtPointer) NULL);
    }	

    XtPopup(shell, XtGrabNone);
    XRaiseWindow(XtDisplay(shell), XtWindow(shell));
}

static void UnsetToggle(widget, clientData, callData)
    Widget widget;
    XtPointer clientData;
    XtPointer callData;
{
    Widget w = (Widget) clientData;

    XmToggleButtonGadgetSetState(w, False, False);
}

static void UpdateTextRegion (text, client_data, call_data)
    Widget text;
    caddr_t client_data;
    caddr_t call_data;
{
    Arg arg;
    String value;

    XtSetArg(arg, XmNvalue, &value);
    XtGetValues(text, &arg, 1);
    if (SendToPrinter) {
        strcpy(psprinter, value);
    } else {
        strcpy(psfilename, value);
    }
}

static void ChangePrinterOutput (toggle, text, cb)
    Widget toggle;
    Widget text;
    XmToggleButtonCallbackStruct *cb;
{
    Arg arg;
    XmString xmString;
    static Widget oldToggle;

    if (oldToggle == NULL) oldToggle = toggle;

    if (oldToggle != toggle) {
        oldToggle = toggle;
        SendToPrinter = !(SendToPrinter);
        if (SendToPrinter) {
            XtSetArg(arg, XmNvalue, psprinter);
        } else {
            XtSetArg(arg, XmNvalue, psfilename);
        }
        XtSetValues(text, &arg, 1);
    }
}

static Widget CreateColorDialog (parent, name, wargs, wcount)
    Widget parent;
    char *name;
    Arg *wargs;
    int wcount;
{
}

static Widget CreatePrintDialog (parent, name, wargs, wcount)
    Widget parent;
    char *name;
    Arg *wargs;
    int wcount;
{
    Arg args[10];
    Widget printDialog, panel, radioBox, label, selectionBox, text;
    Widget printerButton, fileButton;
    char *printer;
    XmString xmString, xmTitle;
    int i;

    i=0;
    printDialog = XmCreateBulletinBoardDialog(parent, 
          "printDialog", args, i);
    XtSetValues(printDialog, wargs, wcount);

    i=0;
    panel = XmCreateRowColumn(printDialog, "panel", args, i);
    XtManageChild(panel);

    i=0;
    label = XmCreateLabelGadget(panel, "label", args, i);
    XtManageChild(label);

    i=0;
    radioBox = XmCreateRadioBox(panel, "radioBox", args, i);
    XtManageChild(radioBox);

    i=0;
    printerButton = XmCreateToggleButtonGadget(radioBox,
          "printerButton", args, i);
    XtManageChild(printerButton);

    i=0;
    fileButton = XmCreateToggleButtonGadget(radioBox,
         "fileButton", args, i);
    XtManageChild(fileButton);

    XtManageChild(XmCreateSeparator(panel, "separator", NULL, 0));

    if ((printer = getenv("PRINTER")) == NULL) {
        xmString = XmStringCreate(psfilename, XmSTRING_DEFAULT_CHARSET);
    } else {
        (void) strcpy(psprinter, printer);
        psprinter[strlen(psprinter)] = '\0';
        xmString = XmStringCreate(psprinter, XmSTRING_DEFAULT_CHARSET);
    }
    i=0; XtSetArg(args[i], XmNtextString, xmString);
    i++; selectionBox = XmCreateSelectionBox(panel, "selectionBox", args, i);
    XtUnmanageChild(XmSelectionBoxGetChild(selectionBox, XmDIALOG_HELP_BUTTON));
    XtManageChild(selectionBox);

    text = XmSelectionBoxGetChild(selectionBox, XmDIALOG_TEXT);

    XtAddCallback(text, XmNvalueChangedCallback,
		  (XtCallbackProc) UpdateTextRegion, NULL);

    XtAddCallback(printerButton, XmNvalueChangedCallback, 
          (XtCallbackProc) ChangePrinterOutput, text);
    XtAddCallback(fileButton, XmNvalueChangedCallback,
          (XtCallbackProc) ChangePrinterOutput, text);

    XtAddCallback(selectionBox, XmNokCallback,
		  (XtCallbackProc) PopdownDialog, printDialog);
    XtAddCallback(selectionBox, XmNokCallback, PrintPage, printerButton);
    XtAddCallback(selectionBox, XmNcancelCallback,
		  (XtCallbackProc) PopdownDialog, printDialog);

    return(printDialog);
}
    
static Widget CreateOptionDialog (parent, name, wargs, wcount)
    Widget parent;
    char *name;
    Arg *wargs;
    int wcount;
{
    Arg args[10];
    Widget optionDialog, panel, button, dialog, drawOptions, optionBox, font;
    XmString xmString, xmTitle;
    unsigned int color;
    int i;

    /* Create dialog, but let the calling routine manage it */
    i=0;
    optionDialog = XmCreateBulletinBoardDialog(parent, name, args, i);
    XtSetValues(optionDialog, wargs, wcount);

    i=0;
    panel = XmCreateRowColumn(optionDialog, "panel", args, i);
    XtManageChild(panel);

    /* create the quit button, quit dialog, and their callbacks */
    i=0;
    button = XmCreatePushButtonGadget(panel, "quitButton", args, i);
    XtManageChild(button);

    i=0;
    dialog = XmCreateWarningDialog(toplevel, "quitDialog", args, i);
    XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtAddCallback(dialog, XmNokCallback, (XtCallbackProc) Exit, 0);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);

    XtManageChild(XmCreateSeparator(panel, "separator", NULL, 0));

    i=0;
    button=XmCreatePushButtonGadget(panel,"loadButton",args,i);
    XtManageChild(button);

    i=0;
    dialog = XmCreateFileSelectionDialog(toplevel, "loadDialog", args, i);
    XtUnmanageChild(XmFileSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtAddCallback(dialog, XmNokCallback,
		  (XtCallbackProc) PopdownDialog, dialog);
    XtAddCallback(dialog, XmNokCallback, LoadAIFile, 0);
    XtAddCallback(dialog, XmNcancelCallback,
		  (XtCallbackProc) PopdownDialog, dialog);
    XtAddCallback(button, XmNactivateCallback, loadMode, dialog);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);

    i=0;
    button=XmCreatePushButtonGadget(panel,"appendButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback, appendMode, dialog);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);

    i=0;
    button=XmCreatePushButtonGadget(panel,"saveButton",args,i);
    XtManageChild(button);

    i=0;
    dialog = XmCreatePromptDialog(toplevel, "saveDialog", args, i);
    XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtAddCallback(dialog, XmNokCallback, SaveAIFile, 0);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);

    XtManageChild(XmCreateSeparator(panel, "separator", NULL, 0));

    i=0;
    button=XmCreatePushButtonGadget(panel,"printButton",args,i);
    XtManageChild(button);
    dialog = CreatePrintDialog(toplevel, "printDialog", NULL, 0);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);

    i=0;
    button=XmCreatePushButtonGadget(panel,"eraseButton",args,i);
    XtManageChild(button);

    i=0;
    dialog = XmCreateWarningDialog(toplevel, "eraseDialog", args, i);
    XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtAddCallback(dialog, XmNokCallback,
		  (XtCallbackProc) PopdownDialog, dialog);
    XtAddCallback(dialog, XmNokCallback, ErasePage, NULL);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc) PopupDialog, dialog);

    XtManageChild(XmCreateSeparator(panel, "separator", NULL, 0));

    i=0;
    button=XmCreatePushButtonGadget(panel, "selectAllButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback, selectAllCallback, NULL);

    i=0;
    button = XmCreatePushButtonGadget(panel,"fontselButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback, PopupFontPanel,
		  (XtPointer) panel);

    i=0;
    button = XmCreateToggleButtonGadget(panel,"outlineButton",args,i);
    XtManageChild(button);

    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Outline);

    InitFonts();

    /* build the option menu radio box */
    i=0;
    optionBox = XmCreateBulletinBoard(panel, "optionBox", args, i);
    XtManageChild(optionBox);
  
    i=0;
    drawOptions = XmCreateRadioBox(optionBox, "drawOptions", args, i);
    XtManageChild(drawOptions);

    i=0;
    button=XmCreateToggleButtonGadget(drawOptions,"selectButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Select);

    i=0;
    button=XmCreateToggleButtonGadget(drawOptions,"alignButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Reset);

    i=0;
    button=XmCreateToggleButtonGadget(drawOptions,"deleteButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Delete);

    i=0;
    button=XmCreateToggleButtonGadget(drawOptions,"rotateButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Rotate);

    i=0;
    button=XmCreateToggleButtonGadget(drawOptions,"resizeButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Resize);

    i=0;
    button=XmCreateToggleButtonGadget(drawOptions,"slantButton",args,i);
    XtManageChild(button);
    XtAddCallback(button, XmNvalueChangedCallback, ChangeMode,
		  (XtPointer) Mode_Slant);

    button = CreateArrowBox(panel, "arrows", NULL, 0);
    XtManageChild(button);
    return(optionDialog);
}

static DPSContext InitDPS (dpy, debug)
    Display *dpy;
    Boolean debug;
{
    DPSContext ctxt, txtctxt;

    /* Make it possible for this client to start a DPS NX agent,
       if "dpsnx.agent" is on the executable search path. */

    (void) XDPSNXSetClientArg(XDPSNX_AUTO_LAUNCH, (ARGCAST) True);

    ctxt = XDPSCreateSimpleContext(dpy, None, None, None, None,
          DPSDefaultTextBackstop, DPSDefaultErrorProc, NULL);

    if (ctxt == NULL) {
	fprintf(stderr, "\nscratchpad: DPS is not available.\n");
	fprintf(stderr,
          "You need an X server with the DPS extension, or a DPS NX agent.\n");
	exit(1);
    } else DPSSetContext(ctxt);

    if (debug) {
        txtctxt = DPSCreateTextContext(DPSDefaultTextBackstop, 
              DPSDefaultErrorProc);
        DPSChainContext(ctxt, txtctxt);
    }

    return(ctxt);
}

int main (argc, argv)
    int argc;
    char **argv;
{
    Arg args[10];
    DPSContext ctxt;
    Widget form, optionDialog, canvas;
    int i, width, height;
    Boolean debug = False;
   
    toplevel = XtInitialize(NULL, "Scratchpad", NULL, 0, &argc, argv);

    dpy = XtDisplay(toplevel);
    screen = XtScreen(toplevel);

    defaultGC = XCreateGC(dpy, RootWindowOfScreen(screen), 0, NULL);
    XSetForeground(dpy, defaultGC, BlackPixelOfScreen(screen));
    XSetBackground(dpy, defaultGC, WhitePixelOfScreen(screen));

    while (argc > 1) switch (*(argv[--argc]+1)) {
        case 'd':
            debug = True;
        break;
        default:
            fprintf(stderr, "Unknown argument: %s\n", argv[argc]);
            exit(1);
    }

    ctxt = InitDPS(dpy, debug);

    i=0;
    form = XmCreateForm(toplevel, "form", args, i);
    XtManageChild(form);

    i=0; XtSetArg(args[i], XmNtopAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNleftAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNrightAttachment, XmATTACH_FORM);
    i++; XtSetArg(args[i], XmNbottomAttachment, XmATTACH_FORM);
    i++; canvas = CreateCanvas(form, "canvas", args, i);
    XtManageChild(canvas);

    optionDialog = CreateOptionDialog(toplevel, "optionDialog", NULL, 0);

    XtRealizeWidget(toplevel);
    XtManageChild(optionDialog);

    SetGraphicsWindow(canvas);

    XtMainLoop();
}
