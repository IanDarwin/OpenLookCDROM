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

/* popups.c -- demonstrate the use of a popup menus in an arbitrary 
 * widget.  Display two PushButtons.  The second one has a popup 
 * menu attached to it that is activated with the third
 * mouse button. 
 */
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/ToggleBG.h>
#include <Xm/ToggleB.h>
#include <Xm/SeparatoG.h>
#include <Xm/RowColumn.h>
#include <Xm/FileSB.h>
#include <Xm/CascadeBG.h>

Widget toplevel;
extern void exit();
void open_dialog_box();

/* callback for pushbutton activation */
void
put_string(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    String str = (String) client_data;

    puts (str);
}

typedef struct _menu_item {
    char        *label;
    WidgetClass *class;
    char         mnemonic;
    char        *accelerator;
    char        *accel_text;
    void       (*callback)();
    XtPointer    callback_data;
    struct _menu_item *subitems;
} MenuItem;

MenuItem file_items[] = {
    { "File Items", &xmLabelGadgetClass, NULL, NULL, NULL, NULL, NULL, NULL },
    { "_sep1", &xmSeparatorGadgetClass, NULL, NULL, NULL, NULL, NULL, NULL },
    { "New", &xmPushButtonGadgetClass, 'N', NULL, NULL,
        put_string, "New", NULL },
    { "Open...", &xmPushButtonGadgetClass, 'O', NULL, NULL,
        open_dialog_box, (XtPointer) XmCreateFileSelectionDialog, NULL },
    { "Save", &xmPushButtonGadgetClass, 'S', NULL, NULL,
        put_string, "Save", NULL },
    { "Save As...", &xmPushButtonGadgetClass, 'A', NULL, NULL,
        open_dialog_box, (XtPointer) XmCreateFileSelectionDialog, NULL },
    { "Exit", &xmPushButtonGadgetClass, 'x', "Ctrl<Key>C", "Ctrl+C",
       exit, NULL, NULL },
    NULL,
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget BuildMenu(), button, rowcol, popup;
    XtAppContext app;
    extern void PostIt();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Build a RowColumn to contain two PushButtons */
    rowcol = XtVaCreateManagedWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    /* The first PushButton is a -gadget-, so we cannot popup a menu
     * from here!
     */
    button = XtVaCreateManagedWidget ("Button 1",
        xmPushButtonGadgetClass, rowcol, NULL);
    XtAddCallback (button, XmNactivateCallback, put_string, "Button 1");

    /* This PushButton is a widget, so it has its own window, so
     * we can pop up a menu from here by adding an event handler
     * specifically for the 3rd mouse button (motif compliance).
     */
    button = XtVaCreateManagedWidget ("Button 2",
        xmPushButtonWidgetClass, rowcol,
        NULL);
    /* it can still have its callback! */
    XtAddCallback (button, XmNactivateCallback, put_string, "Button 2");

    /* build the menu... */
    popup = BuildMenu(button, XmMENU_POPUP, "Stuff", NULL, 
        True, file_items);
    /* Add the event handler (PostIt()) and pass the newly created menu
     * as the client_data.  This is done to avoid using unnecessary globals.
     */
    XtAddEventHandler (button, ButtonPressMask, False, PostIt, popup);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* PostIt() -- event handler for the 3rd mouse button on the 
 * PushButton widget's window.
 */
void
PostIt(pb, client_data, event)
Widget pb;
XtPointer client_data;
XEvent *event;
{
    Widget popup = (Widget) client_data;
    XButtonPressedEvent *bevent = (XButtonPressedEvent *) event;

    if (bevent->button != 3)
        return; 
    /* position the menu at the location of the button press.  If we wanted
     * to position it elsewhere, we could change the x,y fields of the
     * event structure.
     */
    XmMenuPosition (popup, bevent);
    XtManageChild (popup);
}

/* open_dialog_box() -- callback for some of the menu items declared 
 * in the MenuItem struct.   The client data is the creation function
 * for the dialog.  Associate the dialog with the menu
 * item via XmNuserData so we don't have to keep a global and
 * don't have to repeatedly create one.
 */
void
open_dialog_box(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget (*func)() = client_data;
    Widget dialog = NULL;

    /* first see if this menu item's dialog has been created yet */
    XtVaGetValues(w, XmNuserData, &dialog, NULL);

    if (!dialog) {
        /* menu item hasn't been chosen yet -- create the dialog.
         * Use the toplevel as the parent because we don't want the
         * parent of a dialog to be a menu item.
         */
        dialog = (*func)(toplevel, "dialog", NULL, 0);

        XtVaSetValues (XtParent (dialog), XmNtitle, XtName (w), NULL);
	XtVaSetValues (dialog, XmNautoUnmanage, True, NULL);

        /* store the newly created dialog in the XmNuserData for the menu
         * item for easy retrieval next time. (see get-values above.)
         */
        XtVaSetValues (w, XmNuserData, dialog, NULL);
    }

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
    /* If the dialog was already open, XtPopup does nothing.  In
     * this case, at least make sure the window is raised to the top
     * of the window tree (or as high as it can get).
     */
    XRaiseWindow (XtDisplay (dialog), XtWindow (XtParent (dialog)));
}

Widget
BuildMenu(parent, menu_type, menu_title, menu_mnemonic, tear_off, items)
Widget parent;
int menu_type;
char *menu_title, menu_mnemonic;
Boolean tear_off;
MenuItem *items;
{
    Widget menu, cascade, widget;
    int i;
    XmString str;

    if (menu_type == XmMENU_PULLDOWN)
        menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
    else
        menu = XmCreatePopupMenu (parent, "_popup", NULL, 0);
    if (tear_off)
        XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

    if (menu_type == XmMENU_PULLDOWN) {
        str = XmStringCreateLocalized (menu_title);
        cascade = XtVaCreateManagedWidget (menu_title,
            xmCascadeButtonGadgetClass, parent,
            XmNsubMenuId,   menu,
            XmNlabelString, str,
            XmNmnemonic,    menu_mnemonic,
            NULL);
        XmStringFree (str);
    }

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /* If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            widget = BuildMenu (menu, XmMENU_PULLDOWN, items[i].label, 
                items[i].mnemonic, tear_off, items[i].subitems);
        else
            widget = XtVaCreateManagedWidget (items[i].label,
                *items[i].class, menu,
                NULL);
        /* Whether the item is a real item or a cascade button with a
         * menu, it can still have a mnemonic.
         */
        if (items[i].mnemonic)
            XtVaSetValues (widget, XmNmnemonic, items[i].mnemonic, NULL);
        /* any item can have an accelerator, except cascade menus. But,
         * we don't worry about that; we know better in our declarations.
         */
        if (items[i].accelerator) {
            str = XmStringCreateLocalized (items[i].accel_text);
            XtVaSetValues(widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree (str);
        }
        /* again, anyone can have a callback -- however, this is an
         * activate-callback.  This may not be appropriate for all items.
         */
        if (items[i].callback)
	    XtAddCallback(widget,
	        (items[i].class == &xmToggleButtonWidgetClass ||
                items[i].class == &xmToggleButtonGadgetClass) ?
                    XmNvalueChangedCallback : /* ToggleButton class */
                    XmNactivateCallback,      /* PushButton class */
                items[i].callback, items[i].callback_data);
    }
    return menu_type == XmMENU_POPUP ? menu : cascade;
}




