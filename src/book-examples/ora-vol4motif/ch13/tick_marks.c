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

/* tick_marks.c -- demonstrate a scale widget with tick marks. */

#include <Xm/Scale.h>
#include <Xm/LabelG.h>

#define MAX_VAL 10 /* arbitrary value */

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, scale;
    XtAppContext  app;
    int           i;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    scale = XtVaCreateManagedWidget ("load",
        xmScaleWidgetClass, toplevel,
        XtVaTypedArg,     XmNtitleString, XmRString, "Process Load", 13,
        XmNmaximum,       MAX_VAL * 100,
        XmNminimum,       100,
        XmNvalue,         100,
        XmNdecimalPoints, 2,
        XmNshowValue,     True,
        NULL);
 
    for (i = 0; i < MAX_VAL; i++)
        XtVaCreateManagedWidget ("-", xmLabelGadgetClass, scale, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
