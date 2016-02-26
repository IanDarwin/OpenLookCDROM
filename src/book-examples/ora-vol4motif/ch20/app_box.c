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

/* app_box.c -- make an array of DrawnButtons that, when activated,
 * executes a program.  When the program is running, the drawn button
 * associated with the program is insensitive.  When the program dies,
 * reactivate the button so the user can select it again.
 */
#include <Xm/DrawnB.h>
#include <Xm/RowColumn.h>
#include <signal.h>

#ifndef SYSV
#include <sys/wait.h>
#else
#define SIGCHLD SIGCLD
#endif /* SYSV */

#define MAIL_PROG "/bin/mail"

typedef struct {
    Widget drawn_w;
    char *pixmap_file;
    char *exec_argv[6]; /* 6 is arbitrary, but big enough */
    int pid;
} ExecItem;

ExecItem prog_list[] = {
    { NULL, "terminal",   { "xterm", NULL },               0 },
    { NULL, "flagup",     { "xterm", "-e", MAIL_PROG, NULL }, 0 },
    { NULL, "calculator", { "xcalc", NULL },               0 },
    { NULL, "xlogo64",    { "foo", NULL },                 0 },
};

XtAppContext app;    /* application context for the whole program */
GC gc;               /* used to render pixmaps in the widgets */
void reset(), reset_btn(), redraw_button(), exec_prog();

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol;
    Pixmap pixmap;
    Pixel fg, bg;
    int i;

    /* we want to be notified when child programs die */
    signal (SIGCHLD, reset);
    
    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget ("rowcol", 
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    /* get the foreground and background colors of the rowcol
     * so the gc (DrawnButtons) will use them to render pixmaps.
     */
    XtVaGetValues (rowcol,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);
    gc = XCreateGC (XtDisplay (rowcol),
        RootWindowOfScreen (XtScreen (rowcol)), NULL, 0);
    XSetForeground (XtDisplay (rowcol), gc, fg);
    XSetBackground (XtDisplay (rowcol), gc, bg);

    for (i = 0; i < XtNumber (prog_list); i++) {
        /* the pixmap is taken from the name given in the structure */
        pixmap = XmGetPixmap (XtScreen (rowcol),
            prog_list[i].pixmap_file, fg, bg);

        /* Create a drawn button 64x64 (arbitrary, but sufficient)
         * shadowType has no effect till pushButtonEnabled is false.
         */
        prog_list[i].drawn_w = XtVaCreateManagedWidget ("dbutton",
            xmDrawnButtonWidgetClass, rowcol,
            XmNwidth,             64,
            XmNheight,            64,
            XmNpushButtonEnabled, True,
            XmNshadowType,        XmSHADOW_ETCHED_OUT,
            NULL);
        /* if this button is selected, execute the program */
        XtAddCallback (prog_list[i].drawn_w,
            XmNactivateCallback, exec_prog, &prog_list[i]);

        /* when the resize and expose events come, redraw pixmap */
        XtAddCallback (prog_list[i].drawn_w,
            XmNexposeCallback, redraw_button, pixmap);
        XtAddCallback (prog_list[i].drawn_w,
            XmNresizeCallback, redraw_button, pixmap);
    }

    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* redraw_button() -- draws the pixmap into its DrawnButton
 * using the global GC.  Get the width and height of the pixmap
 * being used so we can either center it in the button or clip it.
 */
void
redraw_button(button, client_data, call_data)
Widget button;
XtPointer client_data;
XtPointer call_data;
{
    Pixmap pixmap = (Pixmap) client_data;
    XmDrawnButtonCallbackStruct *cbs = 
        (XmDrawnButtonCallbackStruct *) call_data;
    int srcx, srcy, destx, desty, pix_w, pix_h;
    int drawsize, border;
    Dimension bdr_w, w_width, w_height;
    short hlthick, shthick;
    Window root;

    /* get width and height of the pixmap. don't use srcx and root */
    XGetGeometry (XtDisplay (button), pixmap, &root, &srcx, &srcx,
        &pix_w, &pix_h, &srcx, &srcx);

    /* get the values of all the resources that affect the entire
     * geometry of the button.  
     */
    XtVaGetValues (button,
        XmNwidth,              &w_width,
        XmNheight,             &w_height,
        XmNborderWidth,        &bdr_w,
        XmNhighlightThickness, &hlthick,
        XmNshadowThickness,    &shthick,
        NULL);

    /* calculate available drawing area, width 1st */
    border = bdr_w + hlthick + shthick;

    /* if window is bigger than pixmap, center it; else clip pixmap */
    drawsize = w_width - 2 * border;
    if (drawsize > pix_w) {
        srcx = 0;
        destx = (drawsize - pix_w) / 2 + border;
    } 
    else {
        srcx = (pix_w - drawsize) / 2;
        pix_w = drawsize;
        destx = border;
    }

    drawsize = w_height - 2 * border;
    if (drawsize > pix_h) {
        srcy = 0;
        desty = (drawsize - pix_h) / 2 + border;
    } 
    else {
        srcy = (pix_h - drawsize) / 2;
        pix_h = drawsize;
        desty = border;
    }

    XCopyArea (XtDisplay (button), pixmap, cbs->window, gc,
        srcx, srcy, pix_w, pix_h, destx, desty);
}

/* exec_proc() -- the button has been pressed; fork() and call
 * execvp() to start up the program.  If the fork or the execvp
 * fails (program not found?), the sigchld catcher will get it
 * and clean up.  If the program is successful, set the button's
 * sensitivity to False (to prevent the user from execing again)
 * and set pushButtonEnabled to False to allow shadowType to work.
 */
void
exec_prog(drawn_w, client_data, call_data)
Widget drawn_w;
XtPointer client_data;
XtPointer call_data;
{
    ExecItem *program = (ExecItem *) client_data;
    XmDrawnButtonCallbackStruct *cbs = 
        (XmDrawnButtonCallbackStruct *) call_data;

    switch (program->pid = fork ()) {
        case 0:  /* child */
            execvp (program->exec_argv[0], program->exec_argv);
            perror (program->exec_argv[0]); /* command not found? */
            _exit (255);
        case -1:
            printf ("fork() failed.\n");
    }

    /* The child is off executing program... parent continues */
    if (program->pid > 0) {
        XtVaSetValues (drawn_w,
            XmNpushButtonEnabled, False,
            NULL);
	XtSetSensitive (drawn_w, False);
    }
}

/* reset() -- a program died, so find out which one it was and
 * reset its corresponding DrawnButton widget so it can be reselected
 */
void
reset()
{
    int pid, i;
#ifdef SYSV
    int status;
#else
    union wait status;
#endif /* SYSV */

    if ((pid = wait (&status)) == -1)
        /* an error of some kind (fork probably failed); ignore it */
        return;

    for (i = 0; i < XtNumber (prog_list); i++)
        if (prog_list[i].pid == pid) {
            /* program died -- now reset item.  But not here! */
            XtAppAddTimeOut (app, 0, reset_btn, prog_list[i].drawn_w);
            return;
        }

    printf ("Pid #%d ???\n", pid); /* error, but not fatal */
}

/* reset_btn() -- reset the sensitivity and pushButtonEnabled resources 
 * on the drawn button.  This cannot be done within the signal
 * handler or we might step on an X protocol packet since signals are
 * asynchronous.  This function is safe because it's called from a timer.
 */
void
reset_btn(drawn_w)
Widget drawn_w;   /* client_data from XtAppAddTimeOut() */
{
    XtVaSetValues(drawn_w,
        XmNpushButtonEnabled, True,
        NULL);
    XtSetSensitive (drawn_w, True);
}
