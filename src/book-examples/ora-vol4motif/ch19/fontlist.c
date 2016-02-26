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

/* fontlist.c -- demonstrate how to create, add to, and destroy
 * font lists.  The fonts and text displayed are hardcoded in
 * this program and cannot be overriden by user resources.
 */
#include <Xm/Label.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget        	toplevel;
    XtAppContext  	app;
    XmString      	s1, s2, s3, text, tmp;
    XmFontListEntry	entry1, entry2, entry3;
    XmFontList    	fontlist;
    String        	string1 = "This is a string ",
                  	string2 = "that contains three ",
                  	string3 = "separate fonts.";

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    entry1 = XmFontListEntryLoad (XtDisplay (toplevel), 
        "-*-courier-*-r-*--*-120-*", XmFONT_IS_FONT, "TAG1");
    entry2 = XmFontListEntryLoad (XtDisplay (toplevel),
        "-*-courier-bold-o-*--*-140-*", XmFONT_IS_FONT, "TAG2");
    entry3 = XmFontListEntryLoad (XtDisplay (toplevel),
        "-*-courier-medium-r-*--*-180-*", XmFONT_IS_FONT, "TAG3");
    fontlist = XmFontListAppendEntry (NULL, entry1);
    fontlist = XmFontListAppendEntry (fontlist, entry2);
    fontlist = XmFontListAppendEntry (fontlist, entry3);
    XmFontListEntryFree (&entry1);
    XmFontListEntryFree (&entry2);
    XmFontListEntryFree (&entry3);

    s1 = XmStringCreate (string1, "TAG1");
    s2 = XmStringCreate (string2, "TAG2");
    s3 = XmStringCreate (string3, "TAG3");

    /* concatenate the 3 strings on top of each other, but we can only
     * do two at a time.  So do s1 and s2 onto tmp and then do s3.
     */
    tmp = XmStringConcat (s1, s2);
    text = XmStringConcat (tmp, s3);

    XtVaCreateManagedWidget ("label", xmLabelWidgetClass, toplevel,
        XmNlabelString,     text,
        XmNfontList,        fontlist,
        NULL);

    XmStringFree (s1);
    XmStringFree (s2);
    XmStringFree (s3);
    XmStringFree (tmp);
    XmStringFree (text);
    XmFontListFree (fontlist);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
