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

/* tree.c --
 * Program to show the Tree and Panner widgets.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Tree.h>
#include <X11/Xaw/Panner.h>
#include <X11/StringDefs.h>
#include <Mrm/MrmAppl.h>

void pan();

static MrmRegisterArg callback_list[] = {
    { "pan",          (XtPointer) pan },
    /* Add additional callback procedures here... */
};

Widget
XawCreateTreeWidget (parent, name, args, num_args)
Widget parent;
String name;
ArgList args;
Cardinal num_args;
{
    return (XtCreateWidget (name, treeWidgetClass, parent, args, num_args));
}

Widget
XawCreatePannerWidget (parent, name, args, num_args)
Widget parent;
String name;
ArgList args;
Cardinal num_args;
{
    return (XtCreateWidget (name, pannerWidgetClass, parent, args, num_args));
}

void
pan (panner, client_data, call_data)
Widget panner;
XtPointer client_data;
XtPointer call_data;
{
    Widget tree = (Widget) client_data;
    XawPannerReport *report = (XawPannerReport *) call_data;

    /* Should use XtSetValues, but DrawingArea bug prevents us */
    XtMoveWidget (tree, -report->slider_x, -report->slider_y);
}

int
main (argc, argv)
int   argc;
char *argv[];
{
    XtAppContext  app_context;
    Widget        toplevel, root_widget;
    Cardinal      status;
    static String uid_file_list[] = { "tree" };
    MrmType       class_code;
    MrmHierarchy  hierarchy;

    XtSetLanguageProc (NULL, NULL, NULL);
  
    MrmInitialize();

    toplevel = XtVaAppInitialize (&app_context, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    status = MrmOpenHierarchyPerDisplay (XtDisplay (toplevel), 
        XtNumber (uid_file_list), uid_file_list, NULL, &hierarchy);

    if (status != MrmSUCCESS) {
        XtAppError (app_context, "MrmOpenHierarchyPerDisplay failed");
        exit (1);
    }

    MrmRegisterNames (callback_list, XtNumber (callback_list));
    MrmRegisterClass (0, NULL, "XawCreateTreeWidget",
        XawCreateTreeWidget, treeWidgetClass);
    MrmRegisterClass (0, NULL, "XawCreatePannerWidget",
        XawCreatePannerWidget, pannerWidgetClass);

    status = MrmFetchWidget (hierarchy, "root", toplevel, &root_widget,
        &class_code);

    if (status != MrmSUCCESS) {
        XtAppError (app_context, "MrmFetchWidget failed");
        exit (1);
    }

    XtManageChild (root_widget);
    XtRealizeWidget (toplevel);

    XtAppMainLoop (app_context);
}
