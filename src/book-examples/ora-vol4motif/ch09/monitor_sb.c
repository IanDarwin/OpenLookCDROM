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

/* monitor_sb.c -- demonstrate the ScrollBar callback routines by
 * monitoring the ScrollBar for a ScrolledList.  Functionally, this
 * program does nothing.  However, by tinkering with the Scrolled
 * List and watching the output from the ScrollBar's callback routine,
 * you'll see some interesting behavioral patterns.  By interacting
 * with the *List* widget to cause scrolling, the ScrollBar's callback
 * routine is never called.  Thus, monitoring the scrolling actions
 * of a ScrollBar should not be used to keep tabs on exactly when
 * the ScrollBar's value changes!
 */
#include <Xm/List.h>

/* print the interesting resource values of a scrollbar */
void
scroll_action(scrollbar, client_data, call_data)
Widget scrollbar;
XtPointer client_data;
XtPointer call_data;
{
    XmScrollBarCallbackStruct *cbs = 
        (XmScrollBarCallbackStruct *) call_data;

    printf ("cbs->reason: %s, cbs->value = %d, cbs->pixel = %d\n",
        cbs->reason == XmCR_DRAG? "drag" :
        cbs->reason == XmCR_VALUE_CHANGED? "value changed" :
        cbs->reason == XmCR_INCREMENT? "increment" :
        cbs->reason == XmCR_DECREMENT? "decrement" :
        cbs->reason == XmCR_PAGE_INCREMENT? "page increment" :
        cbs->reason == XmCR_PAGE_DECREMENT? "page decrement" :
        cbs->reason == XmCR_TO_TOP? "top" :
        cbs->reason == XmCR_TO_BOTTOM? "bottom" : "unknown",
        cbs->value, cbs->pixel);
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, list_w, sb;
    XtAppContext  app;
    char *items = "choice0, choice1, choice2, choice3, choice4, \
                   choice5, choice6, choice7, choice8, choice9, \
                   choice10, choice11, choice12, choice13, choice14";

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL, 0);

    list_w = XmCreateScrolledList (toplevel, "list_w", NULL, 0);
    XtVaSetValues (list_w,
        /* Rather than convert the entire list of items into an array
         * of compound strings, let's just let Motif's type converter
         * do it for us and save lots of effort (altho not much time).
         */
        XtVaTypedArg, XmNitems, XmRString, items, strlen (items)+1,
        XmNitemCount, 15,
        XmNvisibleItemCount, 5,
        NULL);
    XtManageChild (list_w);

    /* get the scrollbar from ScrolledWindow associated with Text widget */
    XtVaGetValues (XtParent (list_w), XmNverticalScrollBar, &sb, NULL);
    XtAddCallback (sb, XmNvalueChangedCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNdragCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNincrementCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNdecrementCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNpageIncrementCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNpageDecrementCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNtoTopCallback, scroll_action, NULL);
    XtAddCallback (sb, XmNtoBottomCallback, scroll_action, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
