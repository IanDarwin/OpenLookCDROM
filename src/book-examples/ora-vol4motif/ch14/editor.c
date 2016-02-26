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

/* editor.c -- create a full-blown Motif editor application complete
 * with a menubar, facilities to read and write files, text search
 * and replace, clipboard support and so forth.
 */
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/FileSB.h>
#include <X11/Xos.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

Widget text_edit, search_text, replace_text, text_output;

#define FILE_OPEN 0
#define FILE_SAVE 1 
#define FILE_EXIT 2

#define EDIT_CUT 0
#define EDIT_COPY 1
#define EDIT_PASTE 2
#define EDIT_CLEAR 3

#define SEARCH_FIND_NEXT 0
#define SEARCH_SHOW_ALL 1 
#define SEARCH_REPLACE 2
#define SEARCH_CLEAR 3

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext  app_context;
    Widget        toplevel, main_window, menubar, form, search_panel;
    void          file_cb(), edit_cb(), search_cb();
    Arg           args[10];
    int           n = 0;
    XmString      open, save, exit, exit_acc, file, edit, cut,
                  clear, copy, paste, search, next, find, replace;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app_context, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    XmRepTypeInstallTearOffModelConverter ();

    main_window = XtVaCreateWidget ("main_window",
        xmMainWindowWidgetClass, toplevel, NULL);

    /* Create a simple MenuBar that contains three menus */
    file = XmStringCreateLocalized ("File");
    edit = XmStringCreateLocalized ("Edit");
    search = XmStringCreateLocalized ("Search");
    menubar = XmVaCreateSimpleMenuBar (main_window, "menubar",
        XmVaCASCADEBUTTON, file, 'F',
        XmVaCASCADEBUTTON, edit, 'E',
        XmVaCASCADEBUTTON, search, 'S',
        NULL);
    XmStringFree (file);
    XmStringFree (edit);
    XmStringFree (search);

    /* First menu is the File menu -- callback is file_cb() */
    open = XmStringCreateLocalized ("Open...");
    save = XmStringCreateLocalized ("Save...");
    exit = XmStringCreateLocalized ("Exit");
    exit_acc = XmStringCreateLocalized ("Ctrl+C");
    XmVaCreateSimplePulldownMenu (menubar, "file_menu", 0, file_cb,
        XmVaPUSHBUTTON, open, 'O', NULL, NULL,
        XmVaPUSHBUTTON, save, 'S', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, exit, 'x', "Ctrl<Key>c", exit_acc,
        NULL);
    XmStringFree (open);
    XmStringFree (save);
    XmStringFree (exit);
    XmStringFree (exit_acc);

    /* ...create the "Edit" menu --  callback is edit_cb() */
    cut = XmStringCreateLocalized ("Cut");      
    copy = XmStringCreateLocalized ("Copy");    
    clear = XmStringCreateLocalized ("Clear");  
    paste = XmStringCreateLocalized ("Paste");  
    XmVaCreateSimplePulldownMenu (menubar, "edit_menu", 1, edit_cb,
        XmVaPUSHBUTTON, cut, 't', NULL, NULL,
        XmVaPUSHBUTTON, copy, 'C', NULL, NULL,
        XmVaPUSHBUTTON, paste, 'P', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, clear, 'l', NULL, NULL,
        NULL);
    XmStringFree (cut);
    XmStringFree (copy);
    XmStringFree (paste);

    /* create the "Search" menu -- callback is search_cb() */
    next = XmStringCreateLocalized ("Find Next");
    find = XmStringCreateLocalized ("Show All");
    replace = XmStringCreateLocalized ("Replace Text");
    XmVaCreateSimplePulldownMenu (menubar, "search_menu", 2, search_cb,
        XmVaPUSHBUTTON, next, 'N', NULL, NULL,
        XmVaPUSHBUTTON, find, 'A', NULL, NULL,
        XmVaPUSHBUTTON, replace, 'R', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, clear, 'C', NULL, NULL,
        NULL);
    XmStringFree (next);
    XmStringFree (find);
    XmStringFree (replace);
    XmStringFree (clear);

    XtManageChild (menubar);

    /* create a form work are */
    form = XtVaCreateWidget ("form",
        xmFormWidgetClass, main_window, NULL);

    /* create horizontal RowColumn inside the form */
    search_panel = XtVaCreateWidget ("search_panel",
        xmRowColumnWidgetClass, form,
        XmNorientation,     XmHORIZONTAL,
        XmNpacking,         XmPACK_TIGHT,
        XmNtopAttachment,   XmATTACH_FORM,
        XmNleftAttachment,  XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        NULL);
    /* Create two TextField widgets with Labels... */
    XtVaCreateManagedWidget ("Search Pattern:",
        xmLabelGadgetClass, search_panel, NULL);
    search_text = XtVaCreateManagedWidget ("search_text",
        xmTextFieldWidgetClass, search_panel, NULL);
    XtVaCreateManagedWidget ("     Replace Pattern:",
        xmLabelGadgetClass, search_panel, NULL);
    replace_text = XtVaCreateManagedWidget ("replace_text",
        xmTextFieldWidgetClass, search_panel, NULL);
    XtManageChild (search_panel);

    text_output = XtVaCreateManagedWidget ("text_output",
        xmTextFieldWidgetClass, form,
        XmNeditable,              False,
        XmNcursorPositionVisible, False,
        XmNshadowThickness,       0,
        XmNleftAttachment,        XmATTACH_FORM,
        XmNrightAttachment,       XmATTACH_FORM,
        XmNbottomAttachment,      XmATTACH_FORM,
        NULL);

    n = 0;
    XtSetArg (args[n], XmNrows,             10); n++;
    XtSetArg (args[n], XmNcolumns,          80); n++;
    XtSetArg (args[n], XmNeditMode,         XmMULTI_LINE_EDIT); n++;
    XtSetArg (args[n], XmNtopAttachment,    XmATTACH_WIDGET); n++;
    XtSetArg (args[n], XmNtopWidget,        search_panel); n++;
    XtSetArg (args[n], XmNleftAttachment,   XmATTACH_FORM); n++;
    XtSetArg (args[n], XmNrightAttachment,  XmATTACH_FORM); n++;
    XtSetArg (args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (args[n], XmNbottomWidget,     text_output); n++;
    text_edit = XmCreateScrolledText (form, "text_edit", args, n);
    XtManageChild (text_edit);

    XtManageChild (form);
    XtManageChild (main_window);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app_context);
}

/* file_select_cb() -- callback routine for "OK" button in 
 * FileSelectionDialogs.
 */
void
file_select_cb(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    char buf[256], *filename, *text;
    struct stat statb;
    long len;
    FILE *fp;
    int reason = (int) client_data;
    XmFileSelectionBoxCallbackStruct *cbs =
        (XmFileSelectionBoxCallbackStruct *) call_data;

    if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
        return; /* must have been an internal error */

    if (*filename == NULL) {
        XtFree (filename);
        XBell (XtDisplay (text_edit), 50);
        XmTextSetString (text_output, "Choose a file.");
        return; /* nothing typed */
    }

    if (reason == FILE_SAVE) {
        if (!(fp = fopen (filename, "w"))) {
            perror (filename);
            sprintf (buf, "Can't save to %s.", filename);
            XmTextSetString (text_output, buf);
            XtFree (filename);
            return;
        }
        /* saving -- get text from Text widget... */
        text = XmTextGetString (text_edit);
        len = XmTextGetLastPosition (text_edit);
        /* write it to file (check for error) */
        if (fwrite (text, sizeof (char), len, fp) != len)
            strcpy (buf, "Warning: did not write entire file!");
        else {
            /* make sure a newline terminates file */
            if (text[len-1] != '\n')
                fputc ('\n', fp);
            sprintf (buf, "Saved %ld bytes to %s.", len, filename);
        }
    } 
    else { /* reason == FILE_OPEN */
        /* make sure the file is a regular text file and open it */
        if (stat (filename, &statb) == -1 ||
                (statb.st_mode & S_IFMT) != S_IFREG ||
                !(fp = fopen (filename, "r"))) {
            perror (filename);
            sprintf (buf, "Can't read %s.", filename);
            XmTextSetString (text_output, buf);
            XtFree (filename);
            return;
        }
        /* put the contents of the file in the Text widget by
         * allocating enough space for the entire file, reading the
         * file into the space, and using XmTextSetString() to show
         * the file.
         */
        len = statb.st_size;
        if (!(text = XtMalloc ((unsigned)(len+1)))) /* +1 for NULL */
            sprintf (buf, "%s: XtMalloc(%ld) failed", len, filename);
        else {
            if (fread (text, sizeof (char), len, fp) != len)
                sprintf (buf, "Warning: did not read entire file!");
            else
                sprintf (buf, "Loaded %ld bytes from %s.", len, filename);
            text[len] = 0; /* NULL-terminate */
            XmTextSetString (text_edit, text);
        }
    }
    XmTextSetString (text_output, buf); /* purge output message */

    /* free all allocated space. */
    XtFree (text);
    XtFree (filename);
    fclose (fp);
    XtUnmanageChild (dialog);
}

/* popdown_cb() -- callback routine for "Cancel" button. */
void
popdown_cb (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    XtUnmanageChild (w);
}

/* file_cb() -- a menu item from the "File" pulldown menu was selected */
void
file_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    static Widget open_dialog, save_dialog;
    Widget dialog = NULL;
    XmString button, title;
    int reason = (int) client_data;

    if (reason == FILE_EXIT)
        exit (0);

    XmTextSetString (text_output, NULL);   /* clear message area */

    if (reason == FILE_OPEN && open_dialog)
        dialog = open_dialog;
    else if (reason == FILE_SAVE && save_dialog)
        dialog = save_dialog;

    if (dialog) {
        XtManageChild (dialog);
        /* make sure that dialog is raised to top of window stack */
        XMapRaised (XtDisplay (dialog), XtWindow (XtParent (dialog)));
        return;
    }

    dialog = XmCreateFileSelectionDialog (text_edit, "Files", NULL, 0);
    XtAddCallback (dialog, XmNcancelCallback, popdown_cb, NULL);
    XtAddCallback (dialog, XmNokCallback, file_select_cb, reason);
    if (reason == FILE_OPEN) {
        button = XmStringCreateLocalized ("Open");
        title = XmStringCreateLocalized ("Open File");
        open_dialog = dialog;
    } 
    else { /* reason == FILE_SAVE */
        button = XmStringCreateLocalized ("Save");
        title = XmStringCreateLocalized ("Save File");
        save_dialog = dialog;
    }
    XtVaSetValues (dialog,
        XmNokLabelString, button,
        XmNdialogTitle,   title,
        NULL);
    XmStringFree (button);
    XmStringFree (title);
    XtManageChild (dialog);
}

/* search_cb() -- a menu item from the "Search" pulldown menu selected */
void
search_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    char *search_pat, *p, *string, *new_pat, buf[256];
    XmTextPosition pos = 0;
    int len, nfound = 0;
    int search_len, pattern_len;
    int reason = (int) client_data;
    Boolean found = False;

    XmTextSetString (text_output, NULL);   /* clear message area */

    if (reason == SEARCH_CLEAR) {
        pos = XmTextGetLastPosition (text_edit);
        XmTextSetHighlight (text_edit, 0, pos, XmHIGHLIGHT_NORMAL);
        return;
    }

    if (!(string = XmTextGetString (text_edit)) || !*string) {
        XmTextSetString (text_output, "No text to search.");
        return;
    }
    if (!(search_pat = XmTextGetString (search_text)) || !*search_pat) {
        XmTextSetString (text_output, "Specify a search pattern.");
        XtFree (string);
        return;
    }

    new_pat = XmTextGetString (replace_text);
    search_len = strlen (search_pat);
    pattern_len = strlen (new_pat);

    if (reason == SEARCH_FIND_NEXT) {
        pos = XmTextGetCursorPosition (text_edit) + 1;
        found = XmTextFindString (text_edit, pos, search_pat, 
            XmTEXT_FORWARD, &pos);
        if (!found) 
            found = XmTextFindString (text_edit, 0, search_pat, 
                XmTEXT_FORWARD, &pos);
        if (found)
            nfound++;
    }
    else { /* reason == SEARCH_SHOW_ALL || reason == SEARCH_REPLACE */
        do {
            found = XmTextFindString (text_edit, pos, search_pat,
                XmTEXT_FORWARD, &pos);
            if (found) {
                nfound++;
                if (reason == SEARCH_SHOW_ALL) 
                    XmTextSetHighlight (text_edit, pos, pos + search_len,
                        XmHIGHLIGHT_SELECTED);
                else 
                    XmTextReplace (text_edit, pos, pos + search_len, new_pat);
                pos++;
	    }
	}
        while (found);
    }		  

    if (nfound == 0)
        XmTextSetString (text_output, "Pattern not found.");
    else {
        switch (reason) {
            case SEARCH_FIND_NEXT :
                sprintf (buf, "Pattern found at position %ld.", pos);
                XmTextSetInsertionPosition (text_edit, pos);
                break;
            case SEARCH_SHOW_ALL :
                sprintf (buf, "Found %d occurrences.", nfound);
                break;
            case SEARCH_REPLACE :
                sprintf (buf, "Made %d replacements.", nfound);
        }
        XmTextSetString (text_output, buf);
    }
    XtFree (string);
    XtFree (search_pat);
    XtFree (new_pat);
}

/* edit_cb() -- the callback routine for the items in the edit menu */
void
edit_cb(widget, client_data, call_data)
Widget widget;  
XtPointer client_data;
XtPointer call_data;
{
    Boolean result = True;
    int reason = (int) client_data;
    XEvent *event = ((XmPushButtonCallbackStruct *) call_data)->event;
    Time when;

    XmTextSetString (text_output, NULL);   /* clear message area */

    if (event != NULL &&
        reason == EDIT_CUT || reason == EDIT_COPY || reason == EDIT_CLEAR) {
        switch (event->type) {
            case ButtonRelease :
                when = event->xbutton.time;
                break;
            case KeyRelease :
                when = event->xkey.time;
                break;
            default:
                when = CurrentTime;
                break;
        }
    }

    switch (reason) {
        case EDIT_CUT : 
            result = XmTextCut (text_edit, when); 
            break;
        case EDIT_COPY : 
            result = XmTextCopy (text_edit, when); 
            break;
        case EDIT_PASTE : 
            result = XmTextPaste (text_edit);
        case EDIT_CLEAR : 
            XmTextClearSelection (text_edit, when); 
            break;
    }
    if (result == False)
        XmTextSetString (text_output, "There is no selection.");
}
