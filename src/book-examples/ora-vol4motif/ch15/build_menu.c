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

/* build_menu.c -- Demonstrate the BuildPulldownMenu() routine and
 * how it can be used to build pulldown -and- pullright menus.
 * Menus are defined by declaring an array of MenuItem structures.
 */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

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

/* Pulldown menus are built from cascade buttons, so this function
 * also includes pullright menus.  Create the menu, the cascade button
 * that owns the menu, and then the submenu items.
 */
Widget
BuildPulldownMenu(parent, menu_title, menu_mnemonic, tear_off, items)
Widget parent;
char *menu_title, menu_mnemonic;
Boolean tear_off;
MenuItem *items;
{
    Widget PullDown, cascade, widget;
    int i;
    XmString str;

    PullDown = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
    if (tear_off)
        XtVaSetValues (PullDown, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    str = XmStringCreateLocalized (menu_title);
    cascade = XtVaCreateManagedWidget (menu_title,
        xmCascadeButtonGadgetClass, parent,
        XmNsubMenuId,   PullDown,
        XmNlabelString, str,
        XmNmnemonic,    menu_mnemonic,
        NULL);
    XmStringFree (str);

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /* If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            widget = BuildPulldownMenu (PullDown, items[i].label, 
                items[i].mnemonic, tear_off, items[i].subitems);
        else
            widget = XtVaCreateManagedWidget (items[i].label,
                *items[i].class, PullDown,
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
            XtAddCallback(widget,
                (items[i].class == &xmToggleButtonWidgetClass ||
                items[i].class == &xmToggleButtonGadgetClass) ?
                    XmNvalueChangedCallback : /* ToggleButton class */
                    XmNactivateCallback,      /* PushButton class */
                items[i].callback, items[i].callback_data);
    }
    return cascade;
}

/* callback functions for menu items declared later... */
void
set_weight(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int weight = (int) client_data;

    printf ("Setting line weight to %d\n", weight);
}

void
set_color(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    char *color = (char *) client_data;

    printf ("Setting color to %s\n", color);
}

void
set_dot_dash(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int dot_or_dash = (int) client_data;

    printf ("Setting line style to %s\n", dot_or_dash? "dot" : "dash");
}

MenuItem weight_menu[] = {
    { " 1 ", &xmPushButtonGadgetClass, '1', NULL, NULL,
        set_weight, (XtPointer) 1, (MenuItem *) NULL },
    { " 2 ", &xmPushButtonGadgetClass, '2', NULL, NULL,
        set_weight, (XtPointer) 2, (MenuItem *) NULL },
    { " 3 ", &xmPushButtonGadgetClass, '3', NULL, NULL,
        set_weight, (XtPointer) 3, (MenuItem *) NULL },
    { " 4 ", &xmPushButtonGadgetClass, '4', NULL, NULL,
        set_weight, (XtPointer) 4, (MenuItem *) NULL },
    NULL,
};

MenuItem color_menu[] = {
    { "Cyan", &xmPushButtonGadgetClass, 'C', "Alt<Key>C", "Alt+C",
        set_color, (XtPointer) "cyan", (MenuItem *) NULL },
    { "Yellow", &xmPushButtonGadgetClass, 'Y', "Alt<Key>Y", "Alt+Y",
        set_color, (XtPointer) "yellow", (MenuItem *) NULL },
    { "Magenta", &xmPushButtonGadgetClass, 'M', "Alt<Key>M", "Alt+M",
        set_color, (XtPointer) "magenta", (MenuItem *) NULL },
    { "Black", &xmPushButtonGadgetClass, 'B', "Alt<Key>B", "Alt+B",
        set_color, (XtPointer) "black", (MenuItem *) NULL },
    NULL,
};

MenuItem style_menu[] = {
    { "Dash", &xmPushButtonGadgetClass, 'D', NULL, NULL,
        set_dot_dash, (XtPointer) 0, (MenuItem *) NULL },
    { "Dot",  &xmPushButtonGadgetClass, 'o', NULL, NULL,
        set_dot_dash, (XtPointer) 1, (MenuItem *) NULL },
    NULL,
};

MenuItem drawing_menus[] = {
    { "Line Weight", &xmCascadeButtonGadgetClass, 'W', NULL, NULL,
        0, 0, weight_menu },
    { "Line Color", &xmCascadeButtonGadgetClass, 'C', NULL, NULL,
        0, 0, color_menu },
    { "Line Style", &xmCascadeButtonGadgetClass, 'S', NULL, NULL,
        0, 0, style_menu },
    NULL,
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, menubar, drawing_a;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window. 
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);

    menubar = XmCreateMenuBar (main_w, "menubar", NULL, 0);
    BuildPulldownMenu (menubar, "Lines", 'L', True, drawing_menus);
    XtManageChild (menubar);

    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNwidth, 500,
        XmNheight, 500,
        NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
