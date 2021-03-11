#ifndef lint
static char rcsid[] = "files.c,v 2.0 1994/05/19 02:01:06 dan Exp";
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
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <ctype.h>
#include <assert.h>

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/FileSB.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>

#include "mgdiff.h"
#include "externs.h"

static void popup_cb (Widget w, XtPointer closure, XtPointer call_data);
static int is_ascii_text (char *filename);
static void filel_both_cb (Widget w, XtPointer closure, XtPointer call_data);
static void filer_both_cb (Widget w, XtPointer closure, XtPointer call_data);
static void cancel_both_cb (Widget w, XtPointer closure, XtPointer call_data);
static void do_before (Widget w);
static void do_after (void);
static void cancel_cb (Widget w, XtPointer closure, XtPointer call_data);
static void file_left_cb (Widget w, XtPointer closure, XtPointer call_data);
static void file_right_cb (Widget w, XtPointer closure, XtPointer call_data);
static void cancel_save_cb (Widget w, XtPointer closure, XtPointer call_data);
static void file_save_cb (Widget w, XtPointer closure, XtPointer call_data);
static int really_save_file (char *filename, Block *bl);
static int write_chunk (FILE *file, Chunk *chunk);

static char *filel_name;
static char *filer_name;
static char *left_label = "Left Selection";
static char *right_label = "Right Selection";

extern Widget toplevel;

/* ARGSUSED */
static void popup_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    reset_cursor ((Widget) closure);
}

/* 
 * quick heuristic to test whether a file's contents are ascii text
 */
static int is_ascii_text (char *filename)
{
    int fd, bytes, i;
    char buffer[1024];

    fd = open (filename, O_RDONLY);
    bytes = read (fd, (void *) buffer, 1024);
    (void) close (fd);

    for (i = 0; i < bytes; i++)
	if (!isascii (buffer[i]))
	    return (0);
    return (1);
}

/* 
 * conduct some tests to determine if the input file is suitable for 
 * processing by our program
 */
int file_tests (Widget w, char *filename)
{
    struct stat buf;
    char *title = "Mgdiff Error";

    if (access (filename, R_OK) != 0) {
	werror (w, title, filename, strerror (errno));
	return (0);
    }

    if (stat (filename, &buf) != 0) {
	werror (w, title, filename, strerror (errno));
	return (0);
    }

    if (!S_ISREG (buf.st_mode)) {
	werror (w, title, filename, "not an ordinary file");
	return (0);
    }

    if (buf.st_size == 0) {
	werror (w, title, filename, "file is empty");
	return (0);
    }

    if (!is_ascii_text (filename)) {
	werror (w, title, filename, "file is not a text file");
	return (0);
    }

    return (1);
}

/* 
 * popup a simple error dialog
 */
void werror (Widget parent, char *title, char *msg1, char *msg2)
{
    Widget dialog;
    char buffer[4096];
    XmString xms;
    Arg args[2];

    (void) sprintf (buffer, "%s: %s", msg1, msg2);
    xms = XmStringCreateLtoR (buffer, XmSTRING_DEFAULT_CHARSET);
    XtSetArg (args[0], XmNmessageString, xms);
    XtSetArg (args[1], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL);
    dialog = XmCreateErrorDialog (parent, "werror", args, 2);
    XmStringFree (xms);
    XtVaSetValues (XtParent (dialog), XtNtitle, title, NULL);

    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
    XtManageChild (dialog);
}

/* 
 * popup a longer error dialog
 */
void werror_long (Widget parent, char *title, char **lines, int numlines)
{
    Widget dialog;
    Arg args[2];
    XmString xms1, xms2, xms4, sep;
    int i;

    sep = XmStringSeparatorCreate ();
    xms1 = NULL;
    for (i = 0; i < numlines; i++) {
	xms4 = XmStringCreateSimple (lines[i]);
	if (xms1 == NULL)
	    xms1 = xms4;
	else {
	    xms2 = XmStringConcat (xms1, xms4);
	    XmStringFree (xms4);
	    XmStringFree (xms1);
	    xms1 = xms2;
	}

	if (i < (numlines - 1)) {
	    XmString xms3;

	    xms3 = XmStringConcat (xms1, sep);
	    XmStringFree (xms1);
	    xms1 = xms3;
	}
    }
    XmStringFree (sep);
    
    XtSetArg (args[0], XmNmessageString, xms1);
    XtSetArg (args[1], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL);
    dialog = XmCreateErrorDialog (parent, "werror", args, 2);
    XmStringFree (xms1);
    XtVaSetValues (XtParent (dialog), XtNtitle, title, NULL);
    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
    XtManageChild (dialog);
}

/* ARGSUSED */
static void filel_both_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    char *filename;

    if (!XmStringGetLtoR (cbs->value, XmSTRING_DEFAULT_CHARSET, &filename))
	return;

    if (file_tests ((Widget) closure, filename)) {
	if (filel_name != NULL)
	    XtFree (filel_name);
	filel_name = filename;
	if (filer_name != NULL) {
	    do_before ((Widget) closure);
	    XmUpdateDisplay (toplevel);
	    process_both_files (filel_name, filel_name, filer_name, filer_name);
	    do_after ();
	}
    }
}

/* ARGSUSED */
static void filer_both_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    char *filename;

    if (!XmStringGetLtoR (cbs->value, XmSTRING_DEFAULT_CHARSET, &filename))
	return;

    if (file_tests ((Widget) closure, filename)) {
	if (filer_name != NULL)
	    XtFree (filer_name);
	filer_name = filename;
	if (filel_name != NULL) {
	    do_before ((Widget) closure);
	    XmUpdateDisplay (toplevel);
	    process_both_files (filel_name, filel_name, filer_name, filer_name);
	    do_after ();
	}
    }
}

/* ARGSUSED */
static void cancel_both_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    toggle_open_sensitive (True);
    toggle_openlr_sensitive (True);
    XFlush (XtDisplay (w));
    XtPopdown ((Widget) closure);
    XtDestroyWidget ((Widget) closure);
    do_after ();
}

static void do_before (Widget w)
{
    Display *display = XtDisplay (w);

    XtPopdown (w);
    XtDestroyWidget (w);
    toggle_open_sensitive (True);
    XFlush (display);
}

static void do_after (void)
{
    if (filel_name != NULL) {
	XtFree (filel_name);
	filel_name = NULL;
    }

    if (filer_name != NULL) {
	XtFree (filer_name);
	filer_name = NULL;
    }
}

/* 
 * dirname delivers all but the last level of the path name in string
 */
static char *dirname (char *path)
{
    char *end, *val;

    if (!path)
	return (NULL);

    val = strdup (path);

    for (end = val; *end; end++)
	;

    for (; end != val; end--) {
	if (*end == '/') {
	    if (end == val) {
		free (val);
		return (strdup ("/"));
	    }
	    else {
		*end = '\0';
		return (val);
	    }
	}
    }

    free (val);
    return (NULL);
}

void open_both_files (Widget parent, char *namel, char *namer)
{
    Widget shell;
    Widget fsb1, fsb2;
    Widget form2b;
    Widget frame1a, frame2a;
    Arg args[2];
    int i;
    char *dir;
    XmString xms;

    shell = XtVaCreatePopupShell ("openfiles", xmDialogShellWidgetClass, parent,
				  XmNallowShellResize, True,
				  XmNdeleteResponse, XmDO_NOTHING,
				  NULL);
    XtAddCallback (shell, XmNpopupCallback, popup_cb, parent);
    form2b = XtVaCreateWidget ("form2b", xmFormWidgetClass, shell, NULL);
    frame1a = XtVaCreateManagedWidget ("frame1a", xmFrameWidgetClass, form2b,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNrightAttachment, XmATTACH_WIDGET,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_FORM,
				       NULL);
    frame2a = XtVaCreateManagedWidget ("frame2a", xmFrameWidgetClass, form2b,
				       XmNleftAttachment, XmATTACH_POSITION,
				       XmNrightAttachment, XmATTACH_FORM,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_FORM,
				       XmNleftPosition, 50,
				       NULL);


    i = 0;
    if ((dir = dirname (namel))) {
	xms = XmStringCreateSimple (dir);
	XtSetArg (args[i], XmNdirectory, xms); i++;
    }
    fsb1 = XmCreateFileSelectionBox (frame1a, "files1", args, i);
    if (dir) {
	XtFree (dir);
	XmStringFree (xms);
    }

    i = 0;
    if ((dir = dirname (namer))) {
	xms = XmStringCreateSimple (dir);
	XtSetArg (args[i], XmNdirectory, xms); i++;
    }
    fsb2 = XmCreateFileSelectionBox (frame2a, "files2", args, i);
    if (dir) {
	XtFree (dir);
	XmStringFree (xms);
    }

    XtAddCallback (fsb1, XmNokCallback, filel_both_cb, shell);
    XtAddCallback (fsb2, XmNokCallback, filer_both_cb, shell);
    XtAddCallback (fsb1, XmNcancelCallback, cancel_both_cb, shell);
    XtAddCallback (fsb2, XmNcancelCallback, cancel_both_cb, shell);

    XtVaSetValues (XmFileSelectionBoxGetChild (fsb1, XmDIALOG_HELP_BUTTON), XmNsensitive, False, NULL);
    XtVaSetValues (XmFileSelectionBoxGetChild (fsb2, XmDIALOG_HELP_BUTTON), XmNsensitive, False, NULL);

    XtVaSetValues (XmFileSelectionBoxGetChild (fsb1, XmDIALOG_SELECTION_LABEL),
		   XtVaTypedArg, XmNlabelString, XmRString,
		   left_label, strlen (left_label) + 1, 
		   NULL);
    XtVaSetValues (XmFileSelectionBoxGetChild (fsb2, XmDIALOG_SELECTION_LABEL),
		   XtVaTypedArg, XmNlabelString, XmRString,
		   right_label, strlen (right_label) + 1, 
		   NULL);

    XtVaSetValues (frame1a, XmNrightWidget, frame2a, NULL);
    XtManageChild (fsb1);
    XtManageChild (fsb2);
    XtManageChild (form2b);

    XmAddWMProtocolCallback (shell,
			     XmInternAtom (XtDisplay (parent), "WM_DELETE_WINDOW", False),
			     cancel_both_cb,
			     (caddr_t) shell);

    XtVaSetValues (shell, XmNallowShellResize, False, NULL);

    XtPopup (shell, XtGrabNone);
}

/* ARGSUSED */
static void cancel_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XtDestroyWidget ((Widget) closure);
    toggle_open_sensitive (True);
}

/* ARGSUSED */
static void file_left_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    char *filename;

    if (!XmStringGetLtoR (cbs->value, XmSTRING_DEFAULT_CHARSET, &filename))
	return;

    if (file_tests ((Widget) closure, filename)) {
	process_left_file (filename, filename);
	cancel_cb (NULL, closure, NULL);
    }
    XtFree (filename);
}

void open_left_file (Widget parent, char *name)
{
    Widget dialog;
    Arg args[2];
    int i;
    char *dir;
    XmString xms;

    i = 0;
    XtSetArg (args[i], XmNdeleteResponse, XmDO_NOTHING); i++;
    if ((dir = dirname (name))) {
	xms = XmStringCreateSimple (dir);
	XtSetArg (args[i], XmNdirectory, xms); i++;
    }
    dialog = XmCreateFileSelectionDialog (parent, "openfile", args, i);
    if (dir) {
	XtFree (dir);
	XmStringFree (xms);
    }
    XtAddCallback (XtParent (dialog), XmNpopupCallback, popup_cb, parent);
    XtAddCallback (dialog, XmNokCallback, file_left_cb, dialog);
    XtAddCallback (dialog, XmNcancelCallback, cancel_cb, dialog);

    XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), XmNsensitive, False, NULL);
    XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_SELECTION_LABEL),
		   XtVaTypedArg, XmNlabelString, XmRString,
		   left_label, strlen (left_label) + 1, 
		   NULL);

    XmAddWMProtocolCallback (XtParent (dialog),
			     XmInternAtom (XtDisplay (parent), "WM_DELETE_WINDOW", False),
			     cancel_cb,
			     (caddr_t) XtParent (dialog));

    XtManageChild (dialog);
}

/* ARGSUSED */
static void file_right_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    char *filename;

    if (!XmStringGetLtoR (cbs->value, XmSTRING_DEFAULT_CHARSET, &filename))
	return;

    if (file_tests ((Widget) closure, filename)) {
	process_right_file (filename, filename);
	cancel_cb (NULL, closure, NULL);
    }
    XtFree (filename);
}

void open_right_file (Widget parent, char *name)
{
    Widget dialog;
    Arg args[2];
    int i;
    char *dir;
    XmString xms;

    i = 0;
    XtSetArg (args[i], XmNdeleteResponse, XmDO_NOTHING); i++;
    if ((dir = dirname (name))) {
	xms = XmStringCreateSimple (dir);
	XtSetArg (args[i], XmNdirectory, xms); i++;
    }
    dialog = XmCreateFileSelectionDialog (parent, "openfile", args, XtNumber (args));
    if (dir) {
	XtFree (dir);
	XmStringFree (xms);
    }

    XtAddCallback (XtParent (dialog), XmNpopupCallback, popup_cb, parent);
    XtAddCallback (dialog, XmNokCallback, file_right_cb, dialog);
    XtAddCallback (dialog, XmNcancelCallback, cancel_cb, dialog);

    XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), XmNsensitive, False, NULL);
    XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_SELECTION_LABEL),
		   XtVaTypedArg, XmNlabelString, XmRString,
		   right_label, strlen (right_label) + 1, 
		   NULL);

    XmAddWMProtocolCallback (XtParent (dialog),
			     XmInternAtom (XtDisplay (parent), "WM_DELETE_WINDOW", False),
			     cancel_cb,
			     (caddr_t) XtParent (dialog));

    XtManageChild (dialog);
}

/* ARGSUSED */
static void cancel_save_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XtDestroyWidget (get_top_shell (w));
}

/* 
 * this callback does the work of saving the merged file differences 
 * and is attached as the "OK" callback to the FileSelectionDialog
 */
static void file_save_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    char *title = "Mgdiff Save Error";
    char *filename;
    int status;
    Widget shell = get_top_shell (w);

    if (!XmStringGetLtoR (cbs->value, XmSTRING_DEFAULT_CHARSET, &filename))
	return;

    if (access (filename, W_OK) == 0) {	/* file exists and can be written */
	char buffer[1024];

	(void) sprintf (buffer, "Overwrite \"%s\"?", filename);
	if (modal_question (w, "Mgdiff Save Question", buffer)) {
	    set_cursor (shell);
	    if ((status = really_save_file (filename, (Block *) closure)) != 0) {
		reset_cursor (shell);
		werror (w, title, filename, strerror (status));
		return;
	    }
	    reset_cursor (shell);
	}
    }
    else {			/* file can't be written to */
	if (errno == ENOENT) {	/* because it doesn't exist */
	    set_cursor (shell);
	    if ((status = really_save_file (filename, (Block *) closure)) != 0) {
		reset_cursor (shell);
		werror (w, title, filename, strerror (status));
		return;
	    }
	    reset_cursor (shell);
	}
	else {			/* for some other reason */
	    werror (w, title, filename, strerror (errno));
	    return;
	}
    }

    XtDestroyWidget (shell);
}

void save_file (Widget parent, Block *b, char *name)
{
    Widget dialog;
    Arg args[3];
    int i;
    char *dir;
    XmString xms;

    i = 0;
    XtSetArg (args[i], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); i++;
    XtSetArg (args[i], XmNdeleteResponse, XmDO_NOTHING); i++;

    if ((dir = dirname (name))) {
	xms = XmStringCreateSimple (dir);
	XtSetArg (args[i], XmNdirectory, xms); i++;
    }
    dialog = XmCreateFileSelectionDialog (parent, "savefile", args, i);
    if (dir) {
	XtFree (dir);
	XmStringFree (xms);
    }

    XtAddCallback (XtParent (dialog), XmNpopupCallback, popup_cb, parent);
    XtAddCallback (dialog, XmNokCallback, file_save_cb, (XtPointer) b);
    XtAddCallback (dialog, XmNcancelCallback, cancel_save_cb, NULL);

    XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), XmNsensitive, False, NULL);

    XmAddWMProtocolCallback (XtParent (dialog),
			     XmInternAtom (XtDisplay (parent), "WM_DELETE_WINDOW", False),
			     cancel_save_cb,
			     (caddr_t) NULL);

    XtManageChild (dialog);
}

/* 
 * write the merged file differences out to a file and return 0 for 
 * success and an errno for failure
 */
static int really_save_file (char *filename, Block *bl)
{
    FILE *file;
    Block *b;
    int status;

    if ((file = fopen (filename, "w")) == NULL)
	return (errno);

    for (b = bl; b != NULL; b = b->next) {
	if ((b->arr[LEFT].type == SAME) && (b->arr[RIGHT].type == SAME)) {
	    if ((status = write_chunk (file, &b->arr[LEFT])) != 0)
		return (status);
	}
	else if ((b->arr[LEFT].type == DIFF) && (b->arr[RIGHT].type == DIFF)) {
	    assert (b->selected != NEITHER);
	    if ((status = write_chunk (file, &b->arr[b->selected])) != 0)
		return (status);
	}
	else if ((b->arr[LEFT].type == INSERT) && (b->arr[RIGHT].type == BLANK)) {
	    assert (b->selected != NEITHER);
	    if (b->selected == LEFT)
		if ((status = write_chunk (file, &b->arr[LEFT])) != 0)
		    return (status);
	}
	else if ((b->arr[LEFT].type == BLANK) && (b->arr[RIGHT].type == INSERT)) {
	    assert (b->selected != NEITHER);
	    if (b->selected == RIGHT)
		if ((status = write_chunk (file, &b->arr[RIGHT])) != 0)
		    return (status);
	}
	else
	    assert (False);
    }

    if (fclose (file) != 0)
	return (errno);

    return (0);
}

/* 
 * write out a chunk of text to a file and return 0 if successful or 
 * the errno if unsuccessful
 */
static int write_chunk (FILE *file, Chunk *chunk)
{
    int i;

    for (i = 0; i < chunk->fsize; i++) {
	if ((chunk->wtext != NULL) && (chunk->wtext[i] != NULL)) {
	    if (fputs (chunk->wtext[i], file) == EOF)
		return (errno);
	    if (fputs ("\n", file) == EOF)
		return (errno);
	}
	else {
	    if (fputs (chunk->text[i], file) == EOF)
		return (errno);
	    if (fputs ("\n", file) == EOF)
		return (errno);
	}
    }

    return (0);
}
