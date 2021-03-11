#ifndef lint
static char rcsid[] = "legend.c,v 2.0 1994/05/19 02:01:08 dan Exp";
#endif

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>

#include <X11/StringDefs.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>

#include "mgdiff.h"
#include "externs.h"

/* ARGSUSED */
static void popdown_shell (Widget w, XtPointer closure, XtPointer call_data)
{
    XtPopdown ((Widget) closure);
}

/* ARGSUSED */
static void popup_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    reset_cursor ((Widget) closure);
}

/* ARGSUSED */
static void destroy_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    *((Widget *) closure) = NULL;
}

/* 
 * display a dialog that shows how different text blocks are 
 * color-coded 
 */
void show_legend (Widget parent)
{
    static Widget shell;

    if (shell == NULL) {
	Widget form2a, pane, widget, rc, formd;
	XGCValues gc_values;
	extern GC gcfore[5];

	set_cursor (parent);

	formd = XmCreateFormDialog (parent, "legend", NULL, 0);
	shell = XtParent (formd);
	XtVaSetValues (shell, XmNdeleteResponse, XmDESTROY, NULL);

	add_editres (shell);

	XtAddCallback (shell, XmNpopupCallback, popup_cb, parent);
	XtAddCallback (shell, XmNdestroyCallback, destroy_cb, &shell);

	pane = XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, formd,
				 XmNtopAttachment, XmATTACH_FORM,
				 XmNbottomAttachment, XmATTACH_FORM,
				 XmNleftAttachment, XmATTACH_FORM,
				 XmNrightAttachment, XmATTACH_FORM,
				 XmNsashWidth, 1,
				 XmNsashHeight, 1,
				 NULL);

	rc = XtVaCreateManagedWidget ("rc", xmRowColumnWidgetClass, pane,
				      NULL);

	XGetGCValues (XtDisplay (parent), gcfore[0], GCForeground|GCBackground, &gc_values);
	(void) XtVaCreateManagedWidget ("label1", xmLabelWidgetClass, rc,
					XmNforeground, gc_values.foreground,
					XmNbackground, gc_values.background,
					NULL);

	XGetGCValues (XtDisplay (parent), gcfore[1], GCForeground|GCBackground, &gc_values);
	(void) XtVaCreateManagedWidget ("label2", xmLabelWidgetClass, rc,
					XmNforeground, gc_values.foreground,
					XmNbackground, gc_values.background,
					NULL);

	XGetGCValues (XtDisplay (parent), gcfore[2], GCForeground|GCBackground, &gc_values);
	(void) XtVaCreateManagedWidget ("label3", xmLabelWidgetClass, rc,
					XmNforeground, gc_values.foreground,
					XmNbackground, gc_values.background,
					NULL);

	XGetGCValues (XtDisplay (parent), gcfore[3], GCForeground|GCBackground, &gc_values);
	(void) XtVaCreateManagedWidget ("label4", xmLabelWidgetClass, rc,
					XmNforeground, gc_values.foreground,
					XmNbackground, gc_values.background,
					NULL);

	XGetGCValues (XtDisplay (parent), gcfore[4], GCForeground|GCBackground, &gc_values);
	(void) XtVaCreateManagedWidget ("label5", xmLabelWidgetClass, rc,
					XmNforeground, gc_values.foreground,
					XmNbackground, gc_values.background,
					NULL);

	form2a = XtVaCreateManagedWidget("form2a", xmFormWidgetClass, pane,
					 XmNfractionBase, 5,
					 NULL);
	turn_off_sash_traversal (pane);

	widget = XtVaCreateManagedWidget ("OK", xmPushButtonWidgetClass, form2a,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNleftAttachment, XmATTACH_POSITION,
					  XmNleftPosition, 2,
					  XmNrightAttachment, XmATTACH_POSITION,
					  XmNrightPosition, 3,
					  XmNshowAsDefault, True,
					  XmNdefaultButtonShadowThickness, 1,
					  NULL);
	XtAddCallback(widget, XmNactivateCallback, popdown_shell, shell);

    {
        Dimension w, h;
        XtVaGetValues (widget, XmNwidth, &w, XmNheight, &h, NULL);
        XtVaSetValues (form2a, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
    }
	XtManageChild (pane);
	XtManageChild (formd);
    }

    XtPopup (shell, XtGrabNone);
}
