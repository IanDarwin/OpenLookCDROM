/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 *
 *   The X Consortium, and any party obtaining a copy of these files from
 *   the X Consortium, directly or indirectly, is granted, free of charge, a
 *   full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *   nonexclusive right and license to deal in this software and
 *   documentation files (the "Software"), including without limitation the
 *   rights to use, copy, modify, merge, publish, distribute, sublicense,
 *   and/or sell copies of the Software, and to permit persons who receive
 *   copies from any such party to do so.  This license includes without
 *   limitation a license to do the foregoing actions under any patents of
 *   the party supplying this software to the X Consortium.
 */

/* file_browser.c -- use a ScrolledText object to view the
 * contents of arbitrary files chosen by the user from a
 * FileSelectionDialog or from a single-line text widget.
 */
#include <X11/Xos.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget        top, main_w, menubar, menu, rc, text_w, file_w;
    XtAppContext  app;
    XmString      file, open, exit;
    extern void   read_file(), file_cb();
    Arg           args[10];
    int           n;

    XtSetLanguageProc (NULL, NULL, NULL);

    /* initialize toolkit and create toplevel shell */
    top = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* MainWindow for the application -- contains menubar
     * and ScrolledText/Prompt/TextField as WorkWindow.
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, top, NULL);

    /* Create a simple MenuBar that contains one menu */
    file = XmStringCreateLocalized ("File");
    menubar = XmVaCreateSimpleMenuBar (main_w, "menubar",
        XmVaCASCADEBUTTON, file, 'F',
        NULL);
    XmStringFree (file);

    /* Menu is "File" -- callback is file_cb() */
    open = XmStringCreateLocalized ("Open...");
    exit = XmStringCreateLocalized ("Exit");
    menu = XmVaCreateSimplePulldownMenu (menubar, "file_menu", 0, file_cb,
        XmVaPUSHBUTTON, open, 'O', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, exit, 'x', NULL, NULL,
        NULL);
    XmStringFree (open);
    XmStringFree (exit);

    /* Menubar is done -- manage it */
    XtManageChild (menubar);

    rc = XtVaCreateWidget ("work_area", xmRowColumnWidgetClass, main_w, NULL);
    XtVaCreateManagedWidget ("Filename:", xmLabelGadgetClass, rc,
        XmNalignment, XmALIGNMENT_BEGINNING,
        NULL);
    file_w = XtVaCreateManagedWidget ("text_field",
        xmTextFieldWidgetClass, rc, NULL);

    /* Create ScrolledText -- this is work area for the MainWindow */
    n = 0;
    XtSetArg(args[n], XmNrows,      12); n++;
    XtSetArg(args[n], XmNcolumns,   70); n++;
    XtSetArg(args[n], XmNeditable,  False); n++;
    XtSetArg(args[n], XmNeditMode,  XmMULTI_LINE_EDIT); n++;
    XtSetArg(args[n], XmNcursorPositionVisible,  False); n++;
    text_w = XmCreateScrolledText (rc, "text_w", args, n);
    XtManageChild (text_w);

    /* store text_w as user data in "File" menu for file_cb() callback */
    XtVaSetValues (menu, XmNuserData, text_w, NULL);
    /* add callback for TextField widget passing "text_w" as client data */
    XtAddCallback (file_w, XmNactivateCallback, read_file, text_w);

    XtManageChild (rc);

    /* Store the filename text widget to ScrolledText object */
    XtVaSetValues (text_w, XmNuserData, file_w, NULL);

    XmMainWindowSetAreas (main_w, menubar, NULL, NULL, NULL, rc);
    XtRealizeWidget (top);
    XtAppMainLoop (app);
}

/* file_cb() -- "File" menu item was selected so popup a 
 * FileSelectionDialog. 
 */
void
file_cb(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    static Widget dialog;
    Widget text_w;
    extern void read_file();
    int item_no = (int) client_data;

    if (item_no == 1)
        exit (0);  /* user chose Exit */

    if (!dialog) {
        Widget menu = XtParent (widget);
        dialog = XmCreateFileSelectionDialog (menu, "file_sb", NULL, 0);

        /* Get the text widget handle stored as "user data" in File menu */
        XtVaGetValues (menu, XmNuserData, &text_w, NULL);
        XtAddCallback (dialog, XmNokCallback, read_file, text_w);
        XtAddCallback (dialog, XmNcancelCallback, XtUnmanageChild, NULL);
    }
    XtManageChild (dialog);

    XtPopup (XtParent (dialog), XtGrabNone);
    XMapRaised (XtDisplay (dialog), XtWindow (XtParent (dialog)));
}

/* read_file() -- callback routine when the user selects OK in the
 * FileSelection Dialog or presses Return in the single-line text widget.
 * The specified file must be a regular file and readable.
 * If so, it's contents are displayed in the text_w provided as the
 * client_data to this function.
 */
void
read_file(widget, client_data, call_data)
Widget widget;  /* file selection box or text field widget */
XtPointer client_data;
XtPointer call_data;
{
    char *filename, *text;
    struct stat statb;
    FILE *fp;
    Widget file_w;
    Widget text_w = (Widget) client_data; 
    XmFileSelectionBoxCallbackStruct *cbs = 
        (XmFileSelectionBoxCallbackStruct *) call_data;

    if (XtIsSubclass (widget, xmTextFieldWidgetClass)) {
        filename = XmTextFieldGetString (widget);
        file_w = widget; /* this *is* the file_w */
    } 
    else {
        /* file was selected from FileSelectionDialog */
        XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
        /* the user data stored the file_w widget in the text_w */
        XtVaGetValues (text_w, XmNuserData, &file_w, NULL);
    }

    if (!filename || !*filename) { /* nothing typed? */
        if (filename)
            XtFree (filename);
        return;
    }

    /* make sure the file is a regular text file and open it */
    if (stat (filename, &statb) == -1 ||
            (statb.st_mode & S_IFMT) != S_IFREG ||
            !(fp = fopen(filename, "r"))) {
        if ((statb.st_mode & S_IFMT) == S_IFREG)
            perror (filename); /* send to stderr why we can't read it */
        else
            fprintf (stderr, "%s: not a regular file\n", filename);
        XtFree (filename);
        return;
    }

    /* put the contents of the file in the Text widget by allocating
     * enough space for the entire file, reading the file into the
     * allocated space, and using XmTextFieldSetString() to show the file.
     */
    if (!(text = XtMalloc ((unsigned)(statb.st_size + 1)))) {
        fprintf (stderr, "Can't alloc enough space for %s", filename);
        XtFree (filename);
        fclose (fp);
        return;
    }

    if (!fread (text, sizeof (char), statb.st_size + 1, fp))
        fprintf (stderr, "Warning: may not have read entire file!\n");

    text[statb.st_size] = 0; /* be sure to NULL-terminate */

    /* insert file contents in Text widget */
    XmTextSetString (text_w, text);

    /* make sure text field is up to date */
    if (file_w != widget) {
        /* only necessary if activated from FileSelectionDialog */
        XmTextFieldSetString (file_w, filename);
        XmTextFieldSetCursorPosition (file_w, strlen(filename));
    }

    /* free all allocated space and */
    XtFree (text);
    XtFree (filename);
    fclose (fp);
}
