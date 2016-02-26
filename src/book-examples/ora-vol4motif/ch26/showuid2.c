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

/* showuid2.c --
 * Program to show the interface defined in a UID file
 * with create, manage, and unmanage callbacks
 */

#include <stdio.h>
#include <Mrm/MrmAppl.h>

Widget        toplevel;
MrmHierarchy  hierarchy;

void quit();
void print();
void manage();
void unmanage();
void create();

static MrmRegisterArg callback_list[] = {
    { "quit",     (XtPointer) quit },
    { "print",    (XtPointer) print },
    { "manage",   (XtPointer) manage },
    { "unmanage", (XtPointer) unmanage },
    { "create",   (XtPointer) create },
    /* Add additional callback procedures here... */
};

typedef struct {
    String root_widget_name;
} app_data_t;

static app_data_t app_data;

static XtResource resources[] = {
    { "root", "Root", XmRString, sizeof(String),
	XtOffsetOf(app_data_t,root_widget_name), XmRString, 
	(XtPointer) "root" },
};

static XrmOptionDescRec options[] = {
    { "-root", "root", XrmoptionSepArg, NULL },
};

void
quit (w, client_data, call_data)
Widget    w;
XtPointer client_data;
XtPointer call_data;
{
    exit (0);
}

void
print (w, client_data, call_data)
Widget    w;
XtPointer client_data;
XtPointer call_data;
{
    char *message = (char *) client_data;
    puts (message);
}

void
manage (w, client_data,call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    String name = (String) client_data;
    Widget target = XtNameToWidget (toplevel, name);
    if (target != NULL)
        XtManageChild (target);
    else
        fprintf (stderr, "Cannot manage widget named %s\n", name);
}

void
unmanage (w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    String name = (String) client_data;
    Widget target = XtNameToWidget (toplevel, name);
    if (target != NULL)
        XtUnmanageChild (target);
    else
        fprintf (stderr, "Cannot unmanage widget named %s\n", name);
}

void
create (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    String *args = (String *) client_data;
    String parent_name = args[0];
    Widget parent;

    /* Get a widget id for the parent widget. */
    if (strcmp (parent_name, "toplevel") != 0)
        parent = XtNameToWidget (toplevel, parent_name);
    else
        parent = toplevel;

    /* If the parent was found try to create the hierarchy. */
    if (parent == NULL) 
        fprintf (stderr, "Create: No such widget `%s'\n", args[0]);
    else {
        String child_name = args[1];
        Widget new_w;
        Cardinal status;
        MrmType class;
 
        status = MrmFetchWidget (hierarchy, child_name, parent, 
            &new_w, &class);
        if (status != MrmSUCCESS)
            fprintf (stderr, "Failed to create hierarchy `%s'\n", child_name);
      }

    /* After the widget is created, this callback can be removed. */
    XtRemoveCallback (w, XmNactivateCallback, create, client_data);
}

int
main (argc, argv)
int   argc;
char *argv[];
{
    XtAppContext  app_context;
    Widget        root_widget;
    Cardinal      status;
    MrmType       class_code;
    
    XtSetLanguageProc (NULL, NULL, NULL);
    
    MrmInitialize();
    
    toplevel = XtVaAppInitialize (&app_context, "Demos", options, 
	XtNumber(options), &argc, argv, NULL, NULL);
    
    XtGetApplicationResources (toplevel, &app_data,
	resources, XtNumber(resources), NULL, 0);
    
    /* Check number of args after Xt and App have removed their options. */
    if (argc < 2) {
        fprintf 
	  (stderr, "usage: showuid [Xt options] [-root name] uidfiles ...\n");
	exit (1);
    }
    
    status = MrmOpenHierarchyPerDisplay (XtDisplay (toplevel), argc - 1,
	argv + 1, NULL, &hierarchy);
    
    if (status != MrmSUCCESS) {
        XtAppError (app_context, "MrmOpenHierarchyPerDisplay failed");
	exit (1);
    }
    
    MrmRegisterNames (callback_list, XtNumber (callback_list));
    
    status = MrmFetchWidget (hierarchy, app_data.root_widget_name, toplevel,
        &root_widget, &class_code);
    
    if (status != MrmSUCCESS) {
        XtAppError (app_context, "MrmFetchWidget failed");
	exit (1);
    }
    
    XtManageChild (root_widget);
    XtRealizeWidget (toplevel);
    
    XtAppMainLoop (app_context);
}
