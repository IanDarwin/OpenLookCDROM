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

/* toggle_box.c -- demonstrate a homebrew ToggleBox.  A static
 * list of strings is used as the basis for a list of toggles.
 * The callback routine toggled() is set for each toggle item.
 * The client data for this routine is set to the enumerated
 * value of the item with respect to the entire list.  This value
 * is treated as a bit which is toggled in "toggles_set" -- a
 * mask that contains a complete list of all the selected items.
 * This list is printed when the PushButton is selected.
 */
#include <Xm/ToggleBG.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/RowColumn.h>

unsigned long toggles_set; /* has the bits of which toggles are set */

char *strings[] = {
    "One", "Two", "Three", "Four", "Five",
    "Six", "Seven", "Eight", "Nine", "Ten",
};

/* A RowColumn is used to manage a ToggleBox (also a RowColumn) and
 * a PushButton with a separator gadget in between.
 */
main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, toggle_box, w;
    XtAppContext app;
    void toggled(), check_bits();
    int i;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget ("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    toggle_box = XtVaCreateWidget ("togglebox",
        xmRowColumnWidgetClass, rowcol,
        XmNpacking,        XmPACK_COLUMN,
        XmNnumColumns,     2,
        NULL);

    /* simply loop thru the strings creating a widget for each one */
    for (i = 0; i < XtNumber (strings); i++) {
        w = XtVaCreateManagedWidget (strings[i],
            xmToggleButtonGadgetClass, toggle_box, NULL);
        XtAddCallback (w, XmNvalueChangedCallback, toggled, i);
    }

    XtVaCreateManagedWidget ("sep",
        xmSeparatorGadgetClass, rowcol, NULL);
    w = XtVaCreateManagedWidget ("Check Toggles",
        xmPushButtonGadgetClass, rowcol, NULL);
    XtAddCallback (w, XmNactivateCallback, check_bits, NULL);

    XtManageChild (rowcol);
    XtManageChild (toggle_box);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* callback for all ToggleButtons. */
void
toggled(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int bit = (int) client_data;
    XmToggleButtonCallbackStruct *toggle_data = 
        (XmToggleButtonCallbackStruct *) call_data;

    if (toggle_data->set) /* if the toggle button is set, flip its bit */
        toggles_set |= (1 << bit);
    else /* if the toggle is "off", turn off the bit. */
        toggles_set &= ~(1 << bit);
}

void
check_bits(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int i;

    printf ("Toggles set:");
    for (i = 0; i < XtNumber (strings); i++)
        if (toggles_set & (1<<i))
            printf (" %s", strings[i]);
    putchar ('\n');
}
