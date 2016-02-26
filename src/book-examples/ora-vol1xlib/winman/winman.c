/*
 * Copyright 1989 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>

#include <stdio.h>
#include <signal.h>

#include "bitmaps/focus_frame_bi"    /* name must be <= 14 chars for 
                                * sys V compatibility */

/* include file for printing event types */
#include "eventnames.h"

#define MAX_CHOICE 10
#define DRAW 1
#define ERASE 0
#define RAISE 1
#define LOWER 0
#define MOVE 1
#define RESIZE 0
#define NONE 100
#define NOTDEFINED 0
#define BLACK  1
#define WHITE  0

Window focus_window;
Window inverted_pane = NONE;

static char *menu_label[] =
    {
    "Raise",
    "Lower",
    "Move",
    "Resize",
    "CirculateDn",
    "CirculateUp",
    "(De)Iconify",
    "Kybrd Focus",  
    "New Xterm",
    "Exit",
    };


Display *display;
int screen_num;
XFontStruct *font_info;
char icon_name[50];

main()
{
    Window menuwin;
    Window panes[MAX_CHOICE];
    int menu_width, menu_height, x = 0, y = 0, border_width = 4;
    int winindex;
    int cursor_shape;
    Cursor cursor, hand_cursor;
    char *font_name = "9x15";
    int direction, ascent, descent;
    int char_count;
    char *string;
    XCharStruct overall;
    Bool owner_events;
    int pointer_mode;
    int keyboard_mode;
    Window confine_to;
    GC gc, rgc;
    int pane_height;
    Window assoc_win;
    XEvent event;
    unsigned int button;


    if ( (display=XOpenDisplay(NULL)) == NULL )
        {
        (void) fprintf( stderr, "winman: cannot connect to \
                X server %s\n", XDisplayName(NULL));
        exit( -1 );
        }

    screen_num = DefaultScreen(display);

    /* Access font */
    font_info = XLoadQueryFont(display,font_name);

    if (font_info == NULL)
        {
        (void) fprintf( stderr, "winman: Cannot open font %s\n", 
                font_name);
        exit( -1 );
        }

    string = menu_label[6];
    char_count = strlen(string);

    /* determine the extent of each menu pane based 
     * on the font size */
    XTextExtents(font_info, string, char_count, &direction, &ascent, 
            &descent, &overall);

    menu_width = overall.width + 4;
    pane_height = overall.ascent + overall.descent + 4;
    menu_height = pane_height * MAX_CHOICE;

    /* place the window in upper right corner*/
    x = DisplayWidth(display,screen_num) - menu_width - (2*border_width); 
    y = 0;   /* appears at top */

    /* create opaque window */
    menuwin = XCreateSimpleWindow(display, RootWindow(display, 
            screen_num), x, y, menu_width, menu_height, 
            border_width, BlackPixel(display,screen_num), 
            WhitePixel(display,screen_num));

    /* create the choice windows for the text */
    for (winindex = 0; winindex < MAX_CHOICE; winindex++) {
        panes[winindex] = XCreateSimpleWindow(display, menuwin, 0, 
                menu_height/MAX_CHOICE*winindex, menu_width, 
                pane_height, border_width = 1, 
                BlackPixel(display,screen_num), 
                WhitePixel(display,screen_num));
        XSelectInput(display, panes[winindex], ButtonPressMask
                | ButtonReleaseMask |  ExposureMask);
    }

    XSelectInput(display, RootWindow(display, screen_num), 
            SubstructureNotifyMask);

    /* these don't appear until parent (menuwin) is mapped */
    XMapSubwindows(display,menuwin);

    /* create the cursor for the menu */
    cursor = XCreateFontCursor(display, XC_left_ptr);
    hand_cursor = XCreateFontCursor(display, XC_hand2);

    XDefineCursor(display, menuwin, cursor);

    focus_window = RootWindow(display, screen_num);

    /* Create two Graphics Contexts for inverting panes (white on 
     * black).  We invert the panes by changing the background 
     * pixel, clearing the window, and using the GC with the 
     * contrasting color to redraw the text.  Another way is 
     * using XCopyArea.  The default is to generate GraphicsExpose 
     * and NoExpose events to indicate whether the source area 
     * was obscured.  Since the logical function is GXinvert 
     * the destination is also the source.   Therefore if other 
     * windows are obscuring parts of the exposed pane, the 
     * wrong area will be inverted.  Therefore we would need to 
     * handle GraphicsExpose and NoExpose events.  We'll do it the
     * easier way. */

    gc = XCreateGC(display, RootWindow(display, screen_num) , 0, NULL);
    XSetForeground(display, gc, BlackPixel(display, screen_num));
    rgc = XCreateGC(display, RootWindow(display, screen_num) , 0, NULL);
    XSetForeground(display, rgc, WhitePixel(display, screen_num));

    /* map the menu window (and its subwindows) to the screen_num */
    XMapWindow(display, menuwin);

    /* Force child processes to disinherit the TCP file descriptor.
      * This helps the shell command (creating new xterm) forked and 
     * exec'ed from the menu to work properly.  */
    if ((fcntl(ConnectionNumber(display), F_SETFD, 1)) == -1)
        fprintf(stderr, "winman: child cannot disinherit TCP fd");

    /* loop getting events on the menu window and icons */
    while (1)
        {
        /* wait for an event */
        XNextEvent(display, &event);

        /* if expose, draw text in pane if it is pane */
        switch (event.type) {
        case Expose:
            if  (isIcon(event.xexpose.window, event.xexpose.x, 
                    event.xexpose.y, &assoc_win, icon_name, False))
                XDrawString(display, event.xexpose.window, gc, 2, 
                        ascent + 2, icon_name, strlen(icon_name));
            else /* it's a pane, might be inverted */
                {
                if (inverted_pane == event.xexpose.window)
                    paint_pane(event.xexpose.window, panes, gc, rgc, BLACK);
                else
                    paint_pane(event.xexpose.window, panes, gc, rgc, WHITE);
                }
            break;
        case ButtonPress:

            paint_pane(event.xbutton.window, panes, gc, rgc, BLACK);

            button = event.xbutton.button;
            inverted_pane = event.xbutton.window;
        
            /* get the matching ButtonRelease on same button */
            while (1) {
                /* get rid of other presses */
                while (XCheckTypedEvent(display, ButtonPress, 
                        &event))
                    ;
                /* wait for release; if on correct button exit */
                XMaskEvent(display, ButtonReleaseMask, &event);
                if (event.xbutton.button == button) {
                    /* get rid of other releases */
                    while (XCheckTypedEvent(display, ButtonRelease, 
                            &event))
                        ;
                    break;
                }
            }

            /* all events are sent to the grabbing
             * window regardless of
             * whether this is True or False. 
             * owner_events only affects the distribution of
             * events when the pointer is within this
             * application's windows. */
            owner_events = True;    

            /* we don't want pointer or keyboard events
             * frozen in the server */
            pointer_mode = GrabModeAsync;
            keyboard_mode = GrabModeAsync;

            /* we don't want to confine the cursor */
            confine_to = None;
            XGrabPointer(display, menuwin, owner_events, 
                    ButtonPressMask | ButtonReleaseMask, 
                    pointer_mode, keyboard_mode, 
                    confine_to, hand_cursor, CurrentTime);
    

            /* if press and release occured in same window,
             * do command, if not, do nothing */
            if (inverted_pane == event.xbutton.window)
                {
                /* convert window ID to 
                 * window array index  */
                for (winindex = 0; inverted_pane != 
                        panes[winindex]; winindex++) 
                    ;
                switch (winindex)
                    {
                case 0:
                    raise_lower(menuwin, RAISE);
                    break;
                case 1:
                    raise_lower(menuwin, LOWER);
                    break;
                case 2:
                    move_resize(menuwin, hand_cursor, MOVE);
                    break;
                case 3:
                    move_resize(menuwin, hand_cursor, RESIZE);
                    break;
                case 4:
                    circup(menuwin);
                    break;
                case 5:
                    circdn(menuwin);
                    break;
                case 6:
                    iconify(menuwin);
                    break;
                case 7:
                    focus_window = focus(menuwin);
                    break;
                case 8:
                    execute("xterm&");
                    break;

                case 9: /* exit */
                    XSetInputFocus(display, 
                        RootWindow(display,screen_num), 
                        RevertToPointerRoot, 
                        CurrentTime);
                    /* turn all icons back into windows */
                    /* must clear focus highlights */
                    XClearWindow(display, RootWindow(display, screen_num));
                    /* need to change focus border
                     * width back here */

                    XFlush(display);
                    XCloseDisplay(display);
                    exit(1);
                default:
                    (void) fprintf(stderr, 
                            "Something went wrong\n");
                    break;
                    } /* end switch */
                } /* end if */

            /* Invert Back Here (logical function is invert) */
            paint_pane(event.xexpose.window, panes, gc, rgc, WHITE);

            inverted_pane = NONE;
            draw_focus_frame();
            XUngrabPointer(display, CurrentTime);
            XFlush(display);
            break;
        case DestroyNotify:
            /* window we have iconified has died, remove its icon.
             * Don't need to remove window from save set
             * because that is done automatically */
            removeIcon(event.xdestroywindow.window);
            break;
        case CirculateNotify:
        case ConfigureNotify:
        case UnmapNotify:
            /* all these uncover areas of screen_num */
            draw_focus_frame();
            break;
        case CreateNotify:
        case GravityNotify:
        case MapNotify:
        case ReparentNotify:
            /* don't need these but get them anyway 
             * since we need DestroyNotify and UnmapNotify 
             */
            break;
        case ButtonRelease:
            /* throw these way, they are spurious here */
            break;
        case MotionNotify:
            /* throw these way, they are spurious here */
            break;
        default:
            fprintf(stderr, "winman: got unexpected %s event.\n", 
                    event_names[event.type]);
        } /* end switch */
    } /* end menu loop (while) */
} /* end main */

paint_pane(window, panes, ngc, rgc, mode)
Window window;
Window panes[];
GC ngc, rgc;
int mode;
{      
    int win;
    int x = 2, y;
    GC gc;

    if (mode == BLACK) {
        XSetWindowBackground(display, window, BlackPixel(display, 
                screen_num));
        gc = rgc;
    }
    else {
        XSetWindowBackground(display, window, WhitePixel(display, 
                screen_num));
        gc = ngc;
    }
    /* clearing repaints the background */
    XClearWindow(display, window);

    /* find out index of window for label text */
    for (win = 0; window != panes[win]; win++)
        ;

    y = font_info->max_bounds.ascent;

    /* the string length is necessary because strings for
        XDrawString may not be NULL terminated */
    XDrawString(display, window, gc, x, y, menu_label[win], 
            strlen( menu_label[win])); 
}

circup(menuwin)
Window menuwin;
    {
    XCirculateSubwindowsUp(display, RootWindow(display,screen_num));
    XRaiseWindow(display, menuwin);
    }

circdn(menuwin)
Window menuwin;
    {
    XCirculateSubwindowsDown(display, RootWindow(display,screen_num));
    XRaiseWindow(display, menuwin);
    }

raise_lower(menuwin, raise_or_lower)
Window menuwin;
Bool raise_or_lower;
{
    XEvent report;
    int root_x,root_y;
    Window child, root;
    int win_x, win_y;
    unsigned int mask;
    unsigned int button;

    /* wait for ButtonPress, find out which subwindow of root */
    XMaskEvent(display, ButtonPressMask, &report); 
    button = report.xbutton.button;
    XQueryPointer(display, RootWindow(display,screen_num), &root, 
            &child, &root_x, &root_y, &win_x, &win_y, 
            &mask);

    /* if not RootWindow, raise */
    if (child != NULL)
    {
        if (raise_or_lower == RAISE)
            XRaiseWindow(display, child);
        else
            XLowerWindow(display, child);
    
        /* make sure window manager can never be obscured */
        XRaiseWindow(display, menuwin);
    }

    /* wait for ButtonRelease before exiting */
    /* get the matching ButtonRelease on same button */
    while (1)  {
        XMaskEvent(display, ButtonReleaseMask, &report); 
        if (report.xbutton.button == button) {
            break;
        }
    }

    /* throw out any remaining events so we start fresh */
    while (XCheckMaskEvent(display, ButtonReleaseMask | ButtonPressMask, 
            &report))
        ;
}
    
iconify(menuwin)  
Window menuwin;
    {
    XEvent report;
    extern Window focus_window;
    Window assoc_win;
    int press_x,press_y;
    Window child;
    Window root;
    int win_x, win_y;
    unsigned int mask;
    unsigned int button;

    /* wait for ButtonPress, any win */
    XMaskEvent(display, ButtonPressMask, &report); 
    button = report.xbutton.button;

    /* find out which subwindow the mouse was in */
    XQueryPointer(display, RootWindow(display,screen_num), &root,
        &child, &press_x, &press_y, &win_x, &win_y, &mask);

    /* Can't iconify rootwindow or menu window */
    if ((child == NULL) || (child == menuwin)) 
        {
        /* wait for ButtonRelease before exiting */
        while (1)  {
            XMaskEvent(display, ButtonReleaseMask, &report); 
            if (report.xbutton.button == button) break;
            }
        return; 
        }

    /* returned value of isIcon not used here, but it 
      * is elsewhere in the code */
    isIcon(child, press_x, press_y, &assoc_win, icon_name, True);
    /* window selected is unmapped, whether it is icon
     * or main window.  Associated window is then mapped. */
    XUnmapWindow(display, child);
    XMapWindow(display, assoc_win);

    /* wait for ButtonRelease before exiting */
    /* get the matching ButtonRelease on same button */
    while (1)  {
        XMaskEvent(display, ButtonReleaseMask, &report); 
        if (report.xbutton.button == button) break;
    }

    /* throw out any remaining events so we start fresh for next op */
    while (XCheckMaskEvent(display, ButtonReleaseMask | ButtonPressMask, 
            &report))
        ;
}

focus(menuwin) 
Window menuwin;
{
    XEvent report;
    int x,y;
    Window child;
    Window root;
    Window assoc_win;
    extern Window focus_window;
    int win_x, win_y;
    unsigned int mask;
    unsigned int button;
    XWindowAttributes win_attr;
    static int old_width;
    static Window old_focus;
    int status;


    /* wait for ButtonPress, any win */
    XMaskEvent(display, ButtonPressMask, &report); 
    button = report.xbutton.button;

    /* find out which subwindow the mouse was in */

    XQueryPointer(display, RootWindow(display,screen_num), &root,
            &child, &x, &y, &win_x, &win_y, &mask);


    if ((child == menuwin) || (child == NULL) || (isIcon(child, x, y, &assoc_win, 
            icon_name, True)))
        focus_window = RootWindow(display, screen_num);
    else
        focus_window = child;

    if (focus_window != old_focus)  { /* if focus changed */
        /* if not first time set, set border back */
        if  (old_focus != NULL) 
            XSetWindowBorderWidth(display, old_focus, old_width);

        XSetInputFocus(display, focus_window, RevertToPointerRoot, 
                CurrentTime);
        if (focus_window != RootWindow(display, screen_num)) {
            /* get current border width and add one */
            if (!(status = XGetWindowAttributes(display, 
                    focus_window, &win_attr)))
                fprintf(stderr, "winman: can't get attributes for \
                        focus window\n");
            XSetWindowBorderWidth(display, focus_window, 
                    win_attr.border_width + 1);
            /* keep record so we can change it back */
            old_width = win_attr.border_width;
        }
    }

    /* get the matching ButtonRelease on same button */
    while (1)  {
        XMaskEvent(display, ButtonReleaseMask, &report); 
        if (report.xbutton.button == button) break;
    }

    /* throw out any remaining events so we start fresh for next op */
    while (XCheckMaskEvent(display, ButtonReleaseMask | ButtonPressMask, 
            &report))
        ;

    old_focus = focus_window;
    return(focus_window);
}

draw_focus_frame()
{
    XWindowAttributes win_attr;
    int frame_width = 4;
    Pixmap focus_tile;
    GC gc;
    int foreground = BlackPixel(display, screen_num);
    int background = WhitePixel(display, screen_num);
    extern Window focus_window;
    Bool first_time = True;

    if (first_time) {
        /* make Bitmap from bitmap data */
        focus_tile = XCreatePixmapFromBitmapData(display, 
                RootWindow(display,screen_num), 
                focus_frame_bi_bits, focus_frame_bi_width, 
                focus_frame_bi_height, foreground, 
                background, DefaultDepth(display, screen_num));
    
        /* Create Graphics Context */
        gc = XCreateGC(display, RootWindow(display,screen_num), 0, NULL);  
        XSetFillStyle(display, gc, FillTiled);
        XSetTile(display, gc, focus_tile);
        first_time = False;
    }

    /* get rid of old frames */
    XClearWindow(display, RootWindow(display,screen_num));

    /* if focus is RootWindow, no frame drawn */
    if (focus_window == RootWindow(display,screen_num)) return;

    /* get dimensions and position of focus_window*/
    XGetWindowAttributes(display, focus_window, &win_attr); 

    XFillRectangle(display, RootWindow(display,screen_num), gc,
        win_attr.x - frame_width, win_attr.y - frame_width,
        win_attr.width + 2 * (win_attr.border_width + frame_width),
        win_attr.height + 2 * (win_attr.border_width + frame_width));
}

move_resize(menuwin, hand_cursor, move_or_resize) 
Window menuwin;
Cursor hand_cursor;
Bool move_or_resize;
{
    XEvent report;
    XWindowAttributes win_attr;
    int press_x, press_y, release_x, release_y, move_x, move_y;
    static int box_drawn = False;
    int left, right, top, bottom;
    Window root, child;
    Window win_to_configure;
    int win_x, win_y;
    unsigned int mask;
    unsigned int pressed_button;
    XSizeHints size_hints;
    Bool min_size, increment;
    unsigned int width, height;
    int temp_size;
    static GC gc;
    static int first_time = True;

    if (first_time) {
        gc = XCreateGC(display, RootWindow(display,screen_num), 0, NULL);  
        XSetSubwindowMode(display, gc, IncludeInferiors); 
        XSetForeground(display, gc, BlackPixel(display, screen_num));
        XSetFunction(display, gc, GXxor);
        first_time = False;
    }

    /* wait for ButtonPress choosing window to configure */
    XMaskEvent(display, ButtonPressMask, &report); 
    pressed_button = report.xbutton.button;

    /* which child of root was press in? */
    XQueryPointer(display, RootWindow(display,screen_num), &root, 
            &child, &press_x, &press_y, &win_x, 
            &win_y, &mask);
    win_to_configure = child;

    if ((win_to_configure == NULL)  || 
            ((win_to_configure == menuwin) 
            && (move_or_resize == RESIZE)))  {
        /* if in RootWindow or resizing menuwin get 
         * release event and get out */
        while (XCheckMaskEvent(display, ButtonReleaseMask | ButtonPressMask, 
                &report))
            ;
        return;
    }

    /* button press was in a valid subwindow of root */

    /* get original position and size of window */
    XGetWindowAttributes(display, win_to_configure, 
            &win_attr); 

    /* get size hints for the window */
    XGetNormalHints(display, win_to_configure, &size_hints);
    if (size_hints.flags && PMinSize)
        min_size = True;
    if (size_hints.flags && PResizeInc)
        increment = True;

     /* now we need pointer motion events. */
    XChangeActivePointerGrab(display, PointerMotionHintMask | 
            ButtonMotionMask | ButtonReleaseMask | 
            OwnerGrabButtonMask, hand_cursor, CurrentTime);

    /* don't allow other display operations during move,
     * because Xor won't work properly otherwise */
    XGrabServer(display);

    /* move outline of window until button release */
    while  (1) {
        XNextEvent(display, &report); 
        switch (report.type) {
            case ButtonRelease:
                 if (report.xbutton.button == pressed_button) {
                    if (box_drawn)
                        undraw_box(gc, left, top, right, bottom);   

                    XUngrabServer(display);

                    /* get final window position */
                    XQueryPointer(display, RootWindow(display,
                            screen_num), &root, &child, &release_x, 
                            &release_y, &win_x, &win_y, &mask);

                    /* move or resize window */
                    if (move_or_resize == MOVE)
                          XMoveWindow(display, win_to_configure, 
                            win_attr.x + (release_x - press_x), 
                            win_attr.y + (release_y - press_y));
                    else
                           XResizeWindow(display, win_to_configure, 
                            right - left, bottom - top);
                
                    XRaiseWindow(display, win_to_configure);
                    XFlush(display);
                    box_drawn = False;
                    while (XCheckMaskEvent(display, ButtonReleaseMask | ButtonPressMask, 
                            &report))
                        ;
                    return;
                }
                break;
            
            case MotionNotify:
                if (box_drawn == True) 
                    undraw_box(gc, left, top, right, bottom);

                /* can get rid of all MotionNotify events in queue, 
                 * since otherwise the round-trip  delays caused by 
                 * XQueryPointer may cause a backlog
                 * of MotionNotify events, which will cause additional
                * wasted XQueryPointer calls. */
                while (XCheckTypedEvent(display, MotionNotify, 
                        &report));
        
                /* get current mouse position */
                XQueryPointer(display, RootWindow(display,screen_num), 
                        &root, &child, &move_x, &move_y, 
                        &win_x, &win_y, &mask);
        
                if (move_or_resize == MOVE) {
                       left = move_x - press_x + win_attr.x;
                       top = move_y - press_y + win_attr.y;
                       right = left + win_attr.width; 
                       bottom = top + win_attr.height;
                }
                else
                    {
                    if (move_x < win_attr.x) move_x = 0;
                    if (move_y < win_attr.y ) move_y = 0;
                           left = win_attr.x;
                           top = win_attr.y;
                           right = left + win_attr.width + move_x     
                                - press_x; 
                           bottom = top + win_attr.height + move_y 
                                - press_y; 
                    /* must adjust size according to size hints */
                    /* enforce minimum dimensions */
                    width = right - left;
                    height = bottom - top;

                    /* make sure dimension are increment of
                     * width_inc and height_inc and at least
                     * min_width and min_height */
                    for (temp_size = size_hints.min_width; 
                            temp_size < width; 
                            temp_size += size_hints.width_inc)
                        ;
                        
                    for (temp_size = size_hints.min_height; 
                            temp_size < height; 
                            temp_size += size_hints.height_inc)
                        ;
                    /* most applications (xterm inc.)
                     * pad their right and bottom
                     * dimensions by 2 pixels */
                    right = left + temp_size + 2;
                    bottom = top + temp_size + 2;
                    }
        
                draw_box(gc, left, top, right, bottom);
                box_drawn = True;
                break;
            default:
                /* StructureNotify events shouldn't appear here,
                 * because of the ChangeActivePointerGrab
                 * call, but they do for some reason. */
                /* Anyway, it doesn't matter */
                /* fprintf(stderr, "unexpected event type %s\n", 
                        report.type); */
                ;
        } /* end switch */
    } /* end outer while */
} /* end move */

#ifdef SYSV
#ifndef hpux
#define vfork() fork()
#endif /* hpux */
#endif /* SYSV */

/* the following procedure is a copy of the implementation of 
 * system, modified to reset the handling of SIGINT, SIGQUIT, 
 * and SIGHUP before exec-ing */
execute(s)
char *s;
{
    int status, pid, w;
    register int (*istat)(), (*qstat)();

    if ((pid = vfork()) == 0) {
        signal(SIGINT, SIG_DFL);
        signal(SIGQUIT, SIG_DFL);
        signal(SIGHUP, SIG_DFL);
        execl("/bin/sh", "sh", "-c", s, 0);
        _exit(127);
    }
    istat = signal(SIGINT, SIG_IGN);
    qstat = signal(SIGQUIT, SIG_IGN);
    while ((w = wait(&status)) != pid && w != -1)
        ;
    if (w == -1)
        status = -1;
    signal(SIGINT, istat);
    signal(SIGQUIT, qstat);
    return(status);
}
