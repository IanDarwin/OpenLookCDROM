/* Written by Dave Brennan.
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

/* editor_uil.c -- create a full-blown Motif editor application complete
 * with a menubar, facilities to read and write files, text search
 * and replace, clipboard support and so forth.
 */

#include <Mrm/MrmAppl.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

MrmHierarchy  hierarchy;
Cardinal      status;
MrmType       class_code;
static char   buf[256];

static String uid_files[] = { "editor", "menubar", "dialogs" };

XtAppContext  app_context;
Widget toplevel, text_edit, search_text, replace_text, text_output;

static MrmRegisterArg widgets_list[] = {
    { "w_text_edit",     (XtPointer) &text_edit },
    { "w_search_text",   (XtPointer) &search_text },
    { "w_replace_text",  (XtPointer) &replace_text },
    { "w_text_output",   (XtPointer) &text_output },
};

void register_widget(), file_cb(), edit_cb(), search_cb(), file_select_cb();
void popdown_cb();

/* These definitions depend on the order of the menu entries and are
   also defined in the procedures.uih file with the callback decls. */
typedef enum { FILE_OPEN, FILE_SAVE, FILE_EXIT } FileOp;
typedef enum { EDIT_CUT, EDIT_COPY, EDIT_PASTE, EDIT_CLEAR } EditOp;
typedef enum { SEARCH_FIND_NEXT, SEARCH_SHOW_ALL, SEARCH_REPLACE,
               SEARCH_CLEAR } SearchOp;

static MrmRegisterArg callbacks_list[] = {
    { "register_widget", (XtPointer) register_widget },
    { "file_cb",         (XtPointer) file_cb },
    { "edit_cb",         (XtPointer) edit_cb },
    { "search_cb",       (XtPointer) search_cb },
    { "file_select_cb",  (XtPointer) file_select_cb },
    { "popdown_cb",      (XtPointer) popdown_cb },
};

main(argc, argv)
int   argc;
char *argv[];
{
    Widget main_window;

    XtSetLanguageProc (NULL, NULL, NULL);

    MrmInitialize();

    toplevel = XtVaAppInitialize (&app_context, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    status = MrmOpenHierarchyPerDisplay (XtDisplay(toplevel),
        XtNumber(uid_files), uid_files, NULL, &hierarchy);

    if (status != MrmSUCCESS) {
        XtAppError (app_context, "MrmOpenHierarchyPerDisplay failed");
        exit (1);
    }

    MrmRegisterNames (widgets_list, XtNumber (widgets_list));
    MrmRegisterNames (callbacks_list, XtNumber (callbacks_list));
  
    status = MrmFetchWidget (hierarchy, "main_window", toplevel,
        &main_window, &class_code);

    if (status != MrmSUCCESS) {
        XtAppError (app_context, "MrmFetchWidget failed");
        exit (1);
    }


    XtManageChild (main_window);
    XtRealizeWidget (toplevel);

    XtAppMainLoop (app_context);
}

/* routine to display an error dialog */
void
show_error (message)
char *message;
{
    static Widget dialog;
    XmString s;

    if (dialog == NULL) {
          MrmFetchWidget (hierarchy, "error_dialog", toplevel,
              &dialog, &class_code);
	  if (dialog == NULL || ! XmIsMessageBox (dialog)) {
              XtAppError (app_context, "Creation of error dialog failed.");
              exit (1);
	  }
    }

    s = XmStringCreateLocalized (message);
    XtVaSetValues (dialog, XmNmessageString, s, NULL);
    XmStringFree (s);

    XtManageChild (dialog);
}

/* callback routine for "OK" button in FileSelectionDialogs */
void
file_select_cb (dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    char  *filename, *text;
    struct stat statb;
    long len;
    FILE *fp;
    FileOp reason = *((FileOp *) client_data);
    XmFileSelectionBoxCallbackStruct *cbs =
      (XmFileSelectionBoxCallbackStruct *) call_data;

    XmTextSetString (text_output, NULL);  /* clear the message area */

    if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
        return; /* must have been an internal error */

    if (*filename == NULL) {
        XtFree (filename);
        XBell (XtDisplay (text_edit), 50);
        XmTextSetString (text_output, "Choose a file.");
        return; /* nothing typed */
    }

    if (reason == FILE_SAVE) {
        long bytes_written;
        if (!(fp = fopen (filename, "w"))) {
            perror (filename);
            sprintf (buf, "Can't save to %s.", filename);
            show_error (buf);
            XtFree (filename);
            return;
        }
        /* saving -- get text from Text widget... */
        text = XmTextGetString (text_edit);
        len = XmTextGetLastPosition (text_edit);
        /* write it to file (check for error) */

        bytes_written = fwrite (text, sizeof (char), len, fp);
        if (bytes_written != len) {
            strcpy (buf, "Warning: did not write entire file!");
            show_error (buf);
        } 
	else {
            /* make sure a newline terminates file */
            if (text[len-1] != '\n')
                fputc ('\n', fp);
            sprintf (buf, "Saved %ld bytes to %s.", len, filename);
            XmTextSetString (text_output, buf);
        }
    } 
    else {  /* reason == FILE_OPEN */
        /* make sure the file is a regular text file and open it */
        if (stat (filename, &statb) == -1 ||
            (statb.st_mode & S_IFMT) != S_IFREG ||
	    !(fp = fopen (filename, "r"))) {
            perror (filename);
            sprintf (buf, "Can't read %s.", filename);
            show_error (buf);
            XtFree (filename);
            return;
        }
	/* put the contents of the file in the Text widget by
	 * allocating enough space for the entire file, reading the
	 * file into the space, and using XmTextSetString() to show
	 * the file.
	 */
	len = statb.st_size;
	if (!(text = XtMalloc ((unsigned)(len+1)))) { /* +1 for NULL */
	    sprintf (buf, "%s: XtMalloc(%ld) failed.", len, filename);
	    show_error (buf);
        } 
	else {
	    long bytes_read = fread (text, sizeof(char), len, fp);
	    if (bytes_read != len) {
	        sprintf (buf, "Did not read entire file!");
                show_error (buf);
	    }
	    
            sprintf (buf, "Loaded %ld bytes from %s.", bytes_read, filename);
            XmTextSetString (text_output, buf);
            text[len] = 0; /* NULL-terminate */
            XmTextSetString (text_edit, text);
        }
    }

    /* free all allocated space. */
    XtFree (text);
    XtFree (filename);
    fclose (fp);
    XtUnmanageChild (dialog);
}

/* a menu item from the "File" pulldown menu was selected */
void
file_cb (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    static Widget open_dialog, save_dialog;
    FileOp reason = *((FileOp *) client_data);

    if (reason == FILE_EXIT) {
        MrmCloseHierarchy (hierarchy);
        exit (0);
    }

    XmTextSetString (text_output, NULL);  /* clear the message area */

    if (reason == FILE_OPEN) {
        if (open_dialog == NULL)
            MrmFetchWidget (hierarchy, "open_dialog", toplevel,
                &open_dialog, &class_code);
        if (open_dialog)
            XtManageChild (open_dialog);
        else
            show_error ("Creation of the open dialog failed.");
    } 
    else {  /* reason == FILE_SAVE */
        if (save_dialog == NULL)
            MrmFetchWidget (hierarchy, "save_dialog", toplevel,
                &save_dialog, &class_code);
        if (save_dialog)
            XtManageChild (save_dialog);
        else
            show_error ("Creation of the save dialog failed.");
    }
}

/* a menu item from the "Search" pulldown menu was selected */
void
search_cb (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    char *search_pat, *p, *string, *new_pat;
    XmTextPosition pos = 0;
    int len, nfound = 0;
    int search_len, pattern_len;
    SearchOp reason = *((SearchOp *) client_data);
    Boolean found = False;

    XmTextSetString (text_output, NULL);  /* clear the message area */
    
    if (reason == SEARCH_CLEAR) {
        pos = XmTextGetLastPosition (text_edit);
        XmTextSetHighlight (text_edit, 0, pos, XmHIGHLIGHT_NORMAL);
        return;
    }

    if (!(string = XmTextGetString (text_edit)) || !*string) {
        show_error ("No text to search.");
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
    else {  /* reason == SHOW_ALL || reason == SEARCH_REPLACE */
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

    if (nfound == 0) {
        XmTextSetString (text_output, "Pattern not found");
    } else {
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

/* the callback routine for the items in the edit menu */
void
edit_cb (widget, client_data, call_data)
Widget widget;  
XtPointer client_data;
XtPointer call_data;
{
    Boolean result = True;
    EditOp reason = *((EditOp *) client_data);
    XEvent *event = ((XmPushButtonCallbackStruct *) call_data)->event;
    Time when;

    XmTextSetString (text_output, NULL);  /* clear the message area */

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

void
popdown_cb (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    XtUnmanageChild (w);
}

void
register_widget (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget *w_ptr = (Widget *) client_data;
    *w_ptr = w;
}
