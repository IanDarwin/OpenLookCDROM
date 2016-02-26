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

/* build_option.c -- The final version of BuildMenu() is used to
 * build popup, option, pulldown -and- pullright menus.  Menus are
 * defined by declaring an array of MenuItem structures as usual.
 */
#include <Xm/MainW.h>
#include <Xm/ScrolledW.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

typedef struct _menu_item {
    char        *label;         /* the label for the item */
    WidgetClass *class;         /* pushbutton, label, separator... */
    char         mnemonic;      /* mnemonic; NULL if none */
    char        *accelerator;   /* accelerator; NULL if none */
    char        *accel_text;    /* to be converted to compound string */
    void       (*callback)();   /* routine to call; NULL if none */
    XtPointer    callback_data; /* client_data for callback() */
    struct _menu_item *subitems; /* pullright menu items, if not NULL */
} MenuItem;

/* Build popup, option and pulldown menus, depending on the menu_type.
 * It may be XmMENU_PULLDOWN, XmMENU_OPTION or  XmMENU_POPUP.  Pulldowns
 * return the CascadeButton that pops up the menu.  Popups return the menu.
 * Option menus are created, but the RowColumn that acts as the option
 * "area" is returned unmanaged. (The user must manage it.)
 * Pulldown menus are built from cascade buttons, so this function
 * also builds pullright menus.  The function also adds the right
 * callback for PushButton or ToggleButton menu items.
 */
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

    if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
        menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
    else if (menu_type == XmMENU_POPUP)
        menu = XmCreatePopupMenu (parent, "_popup", NULL, 0);
    else {
        XtWarning ("Invalid menu type passed to BuildMenu()");
        return NULL;
    }
    if (tear_off)
        XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

    /* Pulldown menus require a cascade button to be made */
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
    else if (menu_type == XmMENU_OPTION) {
        /* Option menus are a special case, but not hard to handle */
        Arg args[5];
        int n = 0;
        str = XmStringCreateLocalized (menu_title);
        XtSetArg (args[n], XmNsubMenuId, menu); n++;
        XtSetArg (args[n], XmNlabelString, str); n++;
        /* This really isn't a cascade, but this is the widget handle
         * we're going to return at the end of the function.
         */
        cascade = XmCreateOptionMenu (parent, menu_title, args, n);
        XmStringFree (str);
    }

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /* If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            if (menu_type == XmMENU_OPTION) {
                XtWarning ("You can't have submenus from option menu items.");
                continue;
            } 
            else
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
            XtVaSetValues (widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree (str);
        }

        if (items[i].callback)
            XtAddCallback (widget,
                (items[i].class == &xmToggleButtonWidgetClass ||
                 items[i].class == &xmToggleButtonGadgetClass) ?
                    XmNvalueChangedCallback : /* ToggleButton class */
                    XmNactivateCallback,      /* PushButton class */
                items[i].callback, items[i].callback_data);
    }

    /* for popup menus, just return the menu; pulldown menus, return
     * the cascade button; option menus, return the thing returned
     * from XmCreateOptionMenu().  This isn't a menu, or a cascade button!
     */
    return menu_type == XmMENU_POPUP ? menu : cascade;
}

MenuItem drawing_shapes[] = {
    { "Lines", &xmPushButtonGadgetClass, 'L', NULL, NULL, 0, 0, NULL },
    { "Circles", &xmPushButtonGadgetClass, 'C', NULL, NULL, 0, 0, NULL },
    { "Squares", &xmPushButtonGadgetClass, 'S', NULL, NULL, 0, 0, NULL },
    NULL,
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, pane, sw, drawing_a, menu, option_menu;
    void input();
    XtAppContext app;
    XtWidgetGeometry geom;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel, NULL);

    /* Use a PanedWindow widget as the work area of the main window */
    pane = XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, main_w, NULL);

    /* create the option menu -- don't forget to manage it. */
    option_menu = BuildMenu (pane, XmMENU_OPTION, "Shapes", 
        'S', True, drawing_shapes);
    XtManageChild (option_menu);

    /* Set the OptionMenu so that it can't be resized */
    geom.request_mode = CWHeight;
    XtQueryGeometry (option_menu, NULL, &geom);
    XtVaSetValues (option_menu,
        XmNpaneMinimum, geom.height,
        XmNpaneMaximum, geom.height,
        NULL);

    /* The scrolled window (which contains the drawing area) is a child
     * of the PanedWindow; its sibling, the option menu, cannot be resized,
     * so if the user resizes the toplevel shell, *this* window will resize.
     */
    sw = XtVaCreateManagedWidget ("sw", 
        xmScrolledWindowWidgetClass, pane,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);
    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, sw,
        XmNwidth, 500,
        XmNheight, 500,
        NULL);

    XtManageChild (pane);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
