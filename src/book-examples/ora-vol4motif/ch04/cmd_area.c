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

/* cmd_area.c -- use a ScrolledText object to view the
 * output of commands input by the user in a Command window.
 */
#include <Xm/Text.h>
#include <Xm/MainW.h>
#include <Xm/Command.h>
#include <stdio.h>         /* For popen() */

/* main() -- initialize toolkit, create a main window, menubar,
 * a Command Area and a ScrolledText to view the output of commands.
 */
main(argc, argv)
int argc;
char *argv[];
{
    Widget        top, main_w, menubar, menu, command_w, text_w;
    XtAppContext  app;
    XmString      file, quit;
    extern void   exec_cmd(), exit();
    Arg           args[5];
    int           n = 0;

    XtSetLanguageProc (NULL, NULL, NULL);

    /* initialize toolkit and create toplevel shell */
    top = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    (void) close (0); /* don't let commands read from stdin */

    /* MainWindow for the application -- contains menubar, ScrolledText
     * and CommandArea (which prompts for filename).
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, top,
        XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE,
        NULL);

    /* Create a simple MenuBar that contains one menu */
    file = XmStringCreateLocalized ("File");
    menubar = XmVaCreateSimpleMenuBar (main_w, "menubar",
        XmVaCASCADEBUTTON, file, 'F',
        NULL);
    XmStringFree (file);

    /* "File" menu has only one item (Quit), so make callback exit() */
    quit = XmStringCreateLocalized ("Quit");
    menu = XmVaCreateSimplePulldownMenu (menubar, "file_menu", 0, exit,
        XmVaPUSHBUTTON, quit, 'Q', NULL, NULL,
        NULL);
    XmStringFree (quit);

    /* Menubar is done -- manage it */
    XtManageChild (menubar);

    /* Create ScrolledText -- this is work area for the MainWindow */
    XtSetArg (args[n], XmNrows,      24); n++;
    XtSetArg (args[n], XmNcolumns,   80); n++;
    XtSetArg (args[n], XmNeditable,  False); n++;
    XtSetArg (args[n], XmNeditMode,  XmMULTI_LINE_EDIT); n++;
    text_w = XmCreateScrolledText (main_w, "text_w", args, n);
    XtManageChild (text_w);

    /* store text_w as user data in "File" menu for file_cb() callback */
    XtVaSetValues (menu, XmNuserData, text_w, NULL);

    /* Create the command area -- this must be a Command class widget */
    file = XmStringCreateLocalized ("Command:");
    command_w = XtVaCreateWidget ("command_w", xmCommandWidgetClass, main_w,
        XmNpromptString, file,
        NULL);
    XmStringFree (file);
    XtAddCallback (command_w, XmNcommandEnteredCallback, exec_cmd, text_w);
    XtManageChild (command_w);

    XmMainWindowSetAreas (main_w, menubar, command_w,
        NULL, NULL, XtParent (text_w));
    XtRealizeWidget (top);
    XtAppMainLoop (app);
}

/* execute the command and redirect output to the ScrolledText window */
void
exec_cmd (cmd_widget, client_data, call_data)
Widget cmd_widget;  /* the command widget itself, not its Text widget */
XtPointer client_data; /* passed the text_w as client_data */
XtPointer call_data;
{
    char *cmd, buf[BUFSIZ];
    XmTextPosition pos;
    FILE *pp, *popen();
    Widget text_w = (Widget) client_data;
    XmCommandCallbackStruct *cbs = 
        (XmCommandCallbackStruct *) call_data;

    XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &cmd);

    if (!cmd || !*cmd) { /* nothing typed? */
        if (cmd)
            XtFree (cmd);
        return;
    }

    /* make sure the file is a regular text file and open it */
    if (!(pp = popen (cmd, "r")))
        perror (cmd);
    XtFree (cmd);
    if (!pp)
        return;

    /* put the output of the command in the Text widget by reading
     * until EOF (meaning that the command has terminated).
     */
    for (pos = 0; fgets (buf, sizeof buf, pp); pos += strlen (buf))
        XmTextReplace (text_w, pos, pos, buf);

    pclose (pp);
}
