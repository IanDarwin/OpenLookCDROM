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

/* draw2.c -- extremely simple drawing program that demonstrates
 * how to draw into an off screen pixmap in order to retain the
 * contents of the DrawingArea widget.  This allows us to redisplay
 * the widget if it needs repainting (expose events).
 */
#include <Xm/DrawingA.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

#define WIDTH 400    /* arbitrary width and height values */
#define HEIGHT 300

Pixmap pixmap; /* used to redraw the DrawingArea */

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, drawing_a, pb;
    XtAppContext app;
    GC gc;
    void drawing_area_callback();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0, 
        &argc, argv, NULL,
        XmNwidth,  WIDTH,
        XmNheight, HEIGHT,
        NULL);

    /* Create a DrawingArea widget. */
    drawing_a = XtVaCreateWidget ("drawing_a",
        xmDrawingAreaWidgetClass, toplevel,
        NULL);
    /* add callback for all mouse and keyboard input events */
    XtAddCallback (drawing_a, XmNinputCallback, drawing_area_callback, NULL);
    XtAddCallback (drawing_a, XmNexposeCallback, drawing_area_callback, NULL);

    gc = XCreateGC (XtDisplay (drawing_a),
        RootWindowOfScreen (XtScreen (drawing_a)), 0, NULL);
    XtVaSetValues (drawing_a, XmNuserData, gc, NULL);

    XSetForeground (XtDisplay (drawing_a), gc,
        WhitePixelOfScreen (XtScreen (drawing_a)));
    /* create a pixmap the same size as the drawing area. */
    pixmap = XCreatePixmap (XtDisplay (drawing_a),
        RootWindowOfScreen (XtScreen (drawing_a)), WIDTH, HEIGHT,
        DefaultDepthOfScreen (XtScreen (drawing_a)));
    /* clear pixmap with white */
    XFillRectangle (XtDisplay (drawing_a), pixmap, gc, 0, 0, WIDTH, HEIGHT);
    /* drawing is now drawn into with "black"; change the gc for future */
    XSetForeground (XtDisplay (drawing_a), gc,
        BlackPixelOfScreen (XtScreen (drawing_a)));

    /* add a pushbutton the user can use to clear the canvas */
    pb = XtVaCreateManagedWidget ("Clear",
        xmPushButtonGadgetClass, drawing_a,
        NULL);
    /* if activated, call same callback as XmNinputCallback. */
    XtAddCallback (pb, XmNactivateCallback, drawing_area_callback, NULL);

    XtManageChild (drawing_a);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* Callback routine for DrawingArea's input and expose callbacks
 * as well as the PushButton's activate callback.  Determine which
 * it is by testing the cbs->reason field.
 */
void
drawing_area_callback(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    static Position x, y;
    XmDrawingAreaCallbackStruct *cbs = 
        (XmDrawingAreaCallbackStruct *) call_data;
    XEvent *event = cbs->event;
    Display *dpy = event->xany.display;

    if (cbs->reason == XmCR_INPUT) {
        /* activated by DrawingArea input event -- draw lines.
         * Button Down events anchor the initial point and Button
         * Up draws from the anchor point to the button-up point.
         */
        if (event->xany.type == ButtonPress) {
            /* anchor initial point (i.e., save its value) */
            x = event->xbutton.x;
            y = event->xbutton.y;
        } else if (event->xany.type == ButtonRelease) {
            /* draw full line; get GC and use in XDrawLine() */
            GC gc;
            XtVaGetValues (widget, XmNuserData, &gc, NULL);
            XDrawLine (dpy, cbs->window, gc, x, y,
                event->xbutton.x, event->xbutton.y);
            /* draw into the pixmap as well for redrawing later */
            XDrawLine (dpy, pixmap, gc, x, y,
                event->xbutton.x, event->xbutton.y);
            x = event->xbutton.x;
            y = event->xbutton.y;
        }
    }

    if (cbs->reason == XmCR_EXPOSE || cbs->reason == XmCR_ACTIVATE) {
        GC gc;
        if (cbs->reason == XmCR_ACTIVATE) /* Clear button pushed */
            widget = XtParent (widget); /* get the DrawingArea widget */
        XtVaGetValues (widget, XmNuserData, &gc, NULL);
        if (cbs->reason == XmCR_ACTIVATE) { /* Clear button pushed */
            /* to clear a pixmap, reverse foreground and background */
            XSetForeground (dpy, gc, WhitePixelOfScreen (XtScreen (widget)));
            /* ...and fill rectangle the size of the pixmap */
            XFillRectangle (dpy, pixmap, gc, 0, 0, WIDTH, HEIGHT);
            /* don't foreget to reset */
            XSetForeground (dpy, gc, BlackPixelOfScreen (XtScreen (widget)));
        }
        /* Note: we don't have to use WIDTH and HEIGHT--we could pull the
         * exposed area out of the event structure, but only if the reason
         * was XmCR_EXPOSE... make it simple for the demo; optimize as needed.
         */
        XCopyArea (dpy, pixmap, event->xany.window, gc,
            0, 0, WIDTH, HEIGHT, 0, 0);
    }
}
