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

/* xcal.c -- display a monthly calendar.  The month displayed is a
 * single Label widget whose text is generated from the output of
 * the "cal" program found on any UNIX machine.  popen() is used
 * to run the program and read its output.  Although this is an
 * inefficient method for getting the output of a separate program,
 * it suffices for demonstration purposes.  A List widget displays
 * the months and the user can provide the year as argv[1].
 */
#include <stdio.h>
#include <X11/Xos.h>
#include <Xm/List.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>

int year;
XmStringTable ArgvToXmStringTable();
void FreeXmStringTable();

char *months[] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, frame, rowcol, label, w;
    XtAppContext app;
    Display *dpy;
    extern void set_month();
    XmFontList fontlist;
    XmFontListEntry entry;
    XFontStruct *font;
    XmStringTable strs;
    int month_no;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a fontlist based on the fonts we're using.  These are the
     * fonts that are going to be hardcoded in the Label and List widgets.
     */
    dpy = XtDisplay (toplevel);
    font = XLoadQueryFont (dpy, "-*-courier-bold-r-*--18-*");
    entry = XmFontListEntryCreate ("tag1", XmFONT_IS_FONT, font);
    fontlist = XmFontListAppendEntry (NULL, entry);
    font = XLoadQueryFont (dpy, "-*-courier-medium-r-*--18-*");
    entry = XmFontListEntryCreate ("tag2", XmFONT_IS_FONT, font);
    fontlist = XmFontListAppendEntry (fontlist, entry);
    XtFree (entry);

    if (argc > 1) {
        month_no = 1;
	year = atoi (argv[1]);
    }
    else {
        long time(), t = time(0);
        struct tm *today = localtime (&t);
        year = 1900 + today->tm_year;
        month_no = today->tm_mon+1;
    }

    /* The RowColumn is the general layout manager for the application.
     * It contains two children: a Label gadget that displays the calendar
     * month, and a ScrolledList to allow the user to change the month.
     */
    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    /* enclose the month in a Frame for decoration. */
    frame = XtVaCreateManagedWidget ("frame",
        xmFrameWidgetClass, rowcol, NULL);
    label = XtVaCreateManagedWidget ("month",
        xmLabelGadgetClass, frame,
        XmNalignment, XmALIGNMENT_BEGINNING,
        XmNfontList,  fontlist,
        NULL);

    /* create a list of month names */
    strs = ArgvToXmStringTable (XtNumber (months), months);
    w = XmCreateScrolledList (rowcol, "list", NULL, 0);
    XtVaSetValues (w,
        XmNitems,      strs,
        XmNitemCount,  XtNumber(months),
        XmNfontList,   fontlist,
        NULL);
    FreeXmStringTable (strs);
    XmFontListFree (fontlist);
    XtAddCallback (w, XmNbrowseSelectionCallback, set_month, label);
    XtManageChild (w);
    XmListSelectPos (w, month_no, True); /* initialize month */

    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* callback function for the List widget -- change the month */
void
set_month(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    register FILE *pp;
    extern FILE *popen();
    char text[BUFSIZ];
    register char *p = text;
    XmString str;
    Widget label = (Widget) client_data;
    XmListCallbackStruct *list_cbs = 
        (XmListCallbackStruct *) call_data;

    /* Ask UNIX to execute the "cal" command and read its output */
    sprintf (text, "cal %d %d", list_cbs->item_position, year);
    if (!(pp = popen (text, "r"))) {
        perror (text);
        return;
    }
    *p = 0;
    while (fgets (p, sizeof (text) - strlen (text), pp))
        p += strlen (p);
    pclose (pp);

    /* display the month using the "tag1" font from the
     * Label gadget's XmNfontList.
     */
    str = XmStringCreateLtoR (text, "tag1");
    XtVaSetValues (label, XmNlabelString, str, NULL);
    XmStringFree (str);
}

/* Convert an array of string to an array of compound strings */
XmStringTable
ArgvToXmStringTable(argc, argv)
int argc;
char **argv;
{
    XmStringTable new =
        (XmStringTable) XtMalloc ((argc+1) * sizeof (XmString));

    if (!new)
        return (XmStringTable) NULL;

    new[argc] = 0;
    while (--argc >= 0)
        new[argc] = XmStringCreate (argv[argc], "tag2");
    return new;
}

/* Free the table created by ArgvToXmStringTable() */
void
FreeXmStringTable(argv)
XmStringTable argv;
{
    register int i;

    if (!argv)
        return;
    for (i = 0; argv[i]; i++)
        XmStringFree (argv[i]);
    XtFree (argv);
}
