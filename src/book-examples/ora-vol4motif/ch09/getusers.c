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

/* getusers.c -- demonstrate a simple ScrolledWindow by showing
 * how it can manage a RowColumn that contains a vertical stack of
 * Form widgets, each of which contains a Toggle, two Labels and
 * a Text widget.  The program fills the values of the widgets
 * using various pieces of information from the password file.
 * Note: there are no callback routines associated with any of the
 * widgets created here -- this is for demonstration purposes only.
 */
#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <pwd.h>

typedef struct {
    String      login;
    int         uid;
    String      name;
    String      homedir;
} UserInfo;

/* use getpwent() to read data in the password file to store
 * information about all the users on the system.  The list is
 * a dynamically grown array, the last of which has a NULL login.
 */
UserInfo *
getusers()
{
    /* extern struct *passwd getpwent(); */
    extern char *strcpy();
    struct passwd *pw;
    UserInfo *users = NULL;
    int n;

    setpwent();

    /* getpwent() returns NULL when there are no more users */
    for (n = 0; pw = getpwent(); n++) {
        /* reallocate the pointer to contain one more entry.  You may choose
         * to optimize by adding 10 entries at a time, or perhaps more?
         */
        users = (UserInfo *) XtRealloc (users, (n+1) * sizeof (UserInfo));
        users[n].login = strcpy (XtMalloc 
            (strlen (pw->pw_name)+1), pw->pw_name);
        users[n].name = strcpy (XtMalloc 
            (strlen (pw->pw_gecos)+1), pw->pw_gecos);
        users[n].homedir = strcpy (XtMalloc 
            (strlen (pw->pw_dir)+1), pw->pw_dir);
        users[n].uid = pw->pw_uid;
    }
    /* allocate one more item and set its login string to NULL */
    users = (UserInfo *) XtRealloc (users, (n+1) * sizeof (UserInfo));
    users[n].login = NULL;
    endpwent();
    return users; /* return new array */
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, sw, main_rc, form, toggle;
    XtAppContext app;
    UserInfo *users;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    /* Create a 500x300 scrolled window.  This value is arbitrary,
     * but happens to look good initially.  It is resizable by the user.
     */
    sw = XtVaCreateManagedWidget ("scrolled_w",
        xmScrolledWindowWidgetClass, toplevel,
        XmNwidth,           500,
        XmNheight,          300,
        XmNscrollingPolicy, XmAUTOMATIC,
        NULL);

    /* RowColumn is the work window for the widget */
    main_rc = XtVaCreateWidget ("main_rc", xmRowColumnWidgetClass, sw, NULL);
    /* load the users from the passwd file */
    if (!(users = getusers())) {
        perror ("Can't read user data info");
        exit (1);
    }
    /* for each login entry found in the password file, create a
     * form containing a toggle button, two labels and a text widget.
     */
    while (users->login) { /* NULL login terminates list */
        char uid[8];
        form = XtVaCreateWidget (NULL, xmFormWidgetClass, main_rc, NULL);
        XtVaCreateManagedWidget (users->login, xmToggleButtonWidgetClass, form,
            XmNalignment,        XmALIGNMENT_BEGINNING,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    15,
            NULL);
        sprintf (uid, "%d", users->uid);
        XtVaCreateManagedWidget (uid, xmLabelGadgetClass, form,
            XmNalignment,        XmALIGNMENT_END,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     15,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    20,
            NULL);
        XtVaCreateManagedWidget (users->name, xmLabelGadgetClass, form,
            XmNalignment,        XmALIGNMENT_BEGINNING,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     20,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    50,
            NULL);
        /* Although the home directory is readonly, it may be longer
         * than expected, so don't use a Label widget.  Use a Text widget
         * so that left-right scrolling can take place.
         */
        XtVaCreateManagedWidget (users->homedir, xmTextWidgetClass, form,
            XmNeditable,         False,
            XmNcursorPositionVisible, False,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     50,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNvalue,            users->homedir,
            NULL);
        XtManageChild (form);
        users++;
    }
    XtManageChild (main_rc);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
