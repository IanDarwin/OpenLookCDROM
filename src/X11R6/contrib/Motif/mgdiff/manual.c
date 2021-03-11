#ifndef lint
static char rcsid[] = "manual.c,v 2.0 1994/05/19 02:01:09 dan Exp";
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

#include <X11/Xos.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <X11/StringDefs.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>

#include "mgdiff.h"
#include "externs.h"

static char *get_manual_text (char *command);
static void popdown_shell (Widget w, XtPointer closure, XtPointer call_data);
static void popup_cb (Widget w, XtPointer closure, XtPointer call_data);

/* 
 * run the <command> as a shell command and return the output in 
 * dynamically allocated space.  returns NULL if something really bad 
 * goes wrong with the popen.
 */
static char *get_manual_text (char *command)
{
    FILE *f;
    char *retval;
    int size, bytes;
    char buffer[BUFSIZ];

    if ((f = popen (command, "r")) == NULL)
	return (NULL);

    for (size = 0, retval = NULL; !feof (f); ) {
	if ((bytes = fread ((void *) buffer, 1, BUFSIZ, f)) != 0) {
	    if (size)
		retval = (char *) realloc ((void *) retval, size + bytes + 1);
	    else
		retval = (char *) malloc (bytes + 1);
	    (void) memcpy ((void *) &retval[size], (void *) buffer, bytes);
	    retval[size + bytes] = '\0';
	    size += bytes;
	}
    }

    /* 
     * this return value (the exit status of the command) is not that 
     * useful because it's the status of the last command in what is 
     * usually a pipeline.  previous commands in the pipeline can fail 
     * and we would never know it from this value.
     */
    (void) pclose (f);
    return (retval);
}

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

/* 
 * create and display a shell containing a ScrolledText widget 
 * with the manual page for the application
 */
void show_manual_page (Widget parent)
{
    static Widget shell;

    if (shell == NULL) {
	int i;
	Widget form2a, pane, text_w, widget, formd;
	Arg args[10];
	char *value;
	int iserror = 0;
	XmString xms;
	char *cmd;
	static XtResource resources[] = {
	{"manCommand", "ManCommand", XtRString, sizeof (String), 0,
	     XtRString,
#if defined(SYSV) || defined(SVR4)
	     "(man mgdiff | col -bx) 2>&1"
#else
	     "(man mgdiff | col -b) 2>&1"
#endif
	     }};
    
	set_cursor (parent);

	XtGetApplicationResources (parent, &cmd, resources, XtNumber (resources), NULL, 0);

	value = get_manual_text (cmd);

	if (value == NULL) {	/* system calls in popen failed */
	    char buffer[1024];

	    (void) sprintf (buffer, "System routine \"popen\" failed executing\nthis command (resource \"manCommand\"):\n\n\"%s\"", cmd);
	    xms = XmStringCreateLtoR (buffer, XmSTRING_DEFAULT_CHARSET);
	    iserror = 1;
	}
	else if (value[0] == '\0') { /* command produced no output */
	    char buffer[1024];

	    (void) sprintf (buffer, "Shell command (resource \"manCommand\"):\n\n\"%s\"\n\nproduced no output", cmd);
	    xms = XmStringCreateLtoR (buffer, XmSTRING_DEFAULT_CHARSET);
	    iserror = 1;
	}
	else if (strlen (value) < (unsigned int) 80) { /* command produced insufficient output */
	    char buffer[1024];
	    char *tmp;

	    if ((tmp = strrchr (value, '\n')) != NULL)
		*tmp = '\0';
	    (void) sprintf (buffer, "Shell command (resource \"manCommand\"):\n\n    \"%s\"\n\nproduced this output: \n\n    \"%s\"", cmd, value);
	    xms = XmStringCreateLtoR (buffer, XmSTRING_DEFAULT_CHARSET);
	    iserror = 1;
	}

	if (iserror) {
	    Widget dialog;

	    XtSetArg (args[0], XmNmessageString, xms);
	    dialog = XmCreateErrorDialog (parent, "manualerr", args, 1);
	    XmStringFree (xms);

	    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
	    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
	    XtManageChild (dialog);
	    reset_cursor (parent);
	    return;
	}

	formd = XmCreateFormDialog (parent, "manualpage", NULL, 0);
	shell = XtParent (formd);
	XtVaSetValues (shell, XmNdeleteResponse, XmUNMAP, NULL);

	add_editres (shell);

	XtAddCallback (shell, XmNpopupCallback, popup_cb, parent);

	pane = XtVaCreateManagedWidget ("pane", xmPanedWindowWidgetClass, formd,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_FORM,
					XmNsashWidth, 1,
					XmNsashHeight, 1,
					NULL);

	i = 0;
	XtSetArg (args[i], XmNscrollVertical, True); i++;
	XtSetArg (args[i], XmNscrollHorizontal, False); i++;
	XtSetArg (args[i], XmNeditMode, XmMULTI_LINE_EDIT); i++;
	XtSetArg (args[i], XmNeditable, False); i++;
	XtSetArg (args[i], XmNcursorPositionVisible, False); i++;
	XtSetArg (args[i], XmNwordWrap, False); i++;
	XtSetArg (args[i], XmNvalue, value); i++;
	XtSetArg (args[i], XmNtraversalOn, False); i++;
	text_w = XmCreateScrolledText (pane, "help_text", args, i);

	if (value)
	    free (value);

	XtManageChild (text_w);

	form2a = XtVaCreateManagedWidget ("form2a", xmFormWidgetClass, pane,
					  XmNfractionBase,    7,
					  NULL);
	turn_off_sash_traversal (pane);

	widget = XtVaCreateManagedWidget ("OK", xmPushButtonWidgetClass, form2a,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNleftAttachment, XmATTACH_POSITION,
					  XmNleftPosition, 3,
					  XmNrightAttachment, XmATTACH_POSITION,
					  XmNrightPosition, 4,
					  XmNshowAsDefault, True,
					  XmNdefaultButtonShadowThickness, 1,
					  NULL);
	XtAddCallback (widget, XmNactivateCallback, popdown_shell, shell);

    {
        Dimension w, h;
        XtVaGetValues (widget, XmNwidth, &w, XmNheight, &h, NULL);
        XtVaSetValues (form2a, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
    }

	XtManageChild (formd);
    }

    XtPopup (shell, XtGrabNone);
}
