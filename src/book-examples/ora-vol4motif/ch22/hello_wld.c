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

/* hello_world.c --
 * Initialize X Toolkit creating ApplicationShell widget, then create
 * the user interface described in the hello_world.uid file.
 */

#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#include <stdio.h>

/* Global declarations. */
static void quit();

/* Global definitions. */
/* Callback list looks like an action list: */
static MrmRegisterArg callback_list[] = {
    { "quit", (XtPointer) quit },
};

/* error - Print an error message and exit. */
static void
error (message)
char *message;
{
    fprintf (stderr, "hello_world: %s\n", message);
    exit (1);
}

/* quit - The quit callback procedure.  Exits the program. */
static void
quit (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    puts ((char *) client_data);
    exit (0);
}

main (argc, argv)
int argc;
char *argv[];
{
    XtAppContext app_context;
    Widget toplevel, hello_main;
    Cardinal status;
    static String uid_file_list[] = { "hello_world" };
    MrmHierarchy hierarchy;
    MrmType class_code;
    
    XtSetLanguageProc (NULL, NULL, NULL);
    
    MrmInitialize();
    
    toplevel =
        XtVaAppInitialize (&app_context,    /* application context    */
                           "Demos",         /* application class name */
                           NULL, 0,         /* command line options   */
                           &argc, argv,     /* argc and argv          */
                           NULL,            /* fallback resources     */
                           NULL);           /* arg list               */

    status =
        MrmOpenHierarchyPerDisplay (XtDisplay (toplevel),     /* display   */
                                    XtNumber (uid_file_list), /* num files */
                                    uid_file_list,            /* file list */
                                    NULL,                     /* OS data   */
                                    &hierarchy);              /* hierarchy */

    if (status != MrmSUCCESS)
        error ("Unable to open hello_world.uid file.");

    status = MrmRegisterNames (callback_list, XtNumber (callback_list));

    if (status != MrmSUCCESS)
        error ("Unable to register callback functions with Mrm.");

    status = MrmFetchWidget (hierarchy,          /* hierarchy to search */
                             "hello_main",       /* object name         */
                             toplevel,           /* parent              */
                             &hello_main,        /* widget created      */
                             &class_code);       /* widget's class code */

    if (status != MrmSUCCESS)
        error ("Unable to create interface from UID file");

    MrmCloseHierarchy (hierarchy);

    XtManageChild (hello_main);
    XtRealizeWidget (toplevel);

    XtAppMainLoop (app_context);
}
