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

/* icon_pixmap.c -- demonstrate setting the iconPixmap resource
 */
#include <Xm/Xm.h>
#include <X11/bitmaps/mailfull>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel;
    XtAppContext app;
    Pixmap bitmap;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL,
        XmNwidth, 100, /* size is irrelevant -- toplevel is iconified */
        XmNheight, 100, /* it just can't be 0, or Xt complains */
        XmNiconic,     True,
        NULL);

    bitmap = XCreatePixmapFromBitmapData (XtDisplay (toplevel),
        RootWindowOfScreen (XtScreen (toplevel)),
        mailfull_bits, mailfull_width, mailfull_height, 1, 0, 1);

    XtVaSetValues (toplevel,
        XmNiconPixmap, bitmap,
        NULL);
    
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
