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

/* wm_protocol.c -- demonstrate how to add your own protocol to a
 * shell.  The nature of the protocol isn't important; however, it
 * must be registered with the _MOTIF_WM_MESSAGES property on the
 * shell.  We also add a menu item to the window manager frame's
 * window menu to allow the user to activate the protocol, if desired.
 */
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel;
    XtAppContext app;
    Atom MOTIF_MSGS, MY_PROTOCOL;
    void my_proto_callback();
    char buf[64];

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0, 
        &argc, argv, NULL,
        XmNwidth, 100,
        XmNheight, 100,
        NULL);

    /* get the MOTIF_MSGS and MY_PROTOCOL atoms */
    MY_PROTOCOL = XmInternAtom (XtDisplay (toplevel), 
        "_MY_PROTOCOL", False);
    MOTIF_MSGS = XmInternAtom (XtDisplay (toplevel), 
        "_MOTIF_WM_MESSAGES", False);

    /* Add MY_PROTOCOL to the _MOTIF_WM_MESSAGES VendorShell-defined
     * property on the shell.  Add a callback for this protocol.
     */
    XmAddProtocols (toplevel, MOTIF_MSGS, &MY_PROTOCOL, 1);
    XmAddProtocolCallback (toplevel,
        MOTIF_MSGS, MY_PROTOCOL, my_proto_callback, NULL);

    /* allow the user to activate the protocol through the window manager's
     * window menu on the shell.
     */
    sprintf (buf, "MyProtocol _P Ctrl<Key>P f.send_msg %d", MY_PROTOCOL);
    XtVaSetValues (toplevel, XmNmwmMenu, buf, NULL);

    /* create widgets... */

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* called if _MY_PROTOCOL was activated, a client message was sent... */
void
my_proto_callback(widget, client_data, call_data)
Widget widget;    
XtPointer client_data;
XtPointer call_data;
{
    puts ("My protocol got activated!");
}
