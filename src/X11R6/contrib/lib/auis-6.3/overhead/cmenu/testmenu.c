/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/cmenu/RCS/testmenu.c,v 2.6 1992/12/15 21:00:33 rr2b R6tape $";
#endif


 

#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/cursorfont.h>

#include <cmenu.h>

AddMenuItems(menu)
    struct cmenu *menu;
{

    int pane;
    int selection;
    long data;
    char label[512];
    int retVal;

    while ((retVal = scanf(" %d %d %d%*[ ]%512[^\n]", &pane, &selection, &data, label)) == 4 || retVal == 3) {

        if (retVal == 3)
            *label = '\0';

        if (pane == 0) {
            if (cmenu_AddPane(menu, label, selection, 0) < 0) {
                fprintf(stderr, "Add pane failed on pane #%d %s\n", selection, label);
                return -1;
            }
        }
        else {

            char *realLabel;

            if (*label == '#')
                realLabel = label + 1;
            else
                realLabel = label;
            if (cmenu_AddSelection(menu, NULL, pane, realLabel, selection, data, cmenu_DisallowDuplicates) < 0) {
                fprintf(stderr, "Add selection failed on pane #%d selection #%d %s\n", pane, selection, label);
                return -1;
            }
            if (*label == '#')
                cmenu_SetActive(menu, NULL, pane, selection, cmenu_Inactive);
        }
    }
}

main(argc, argv)
    int argc;
    char *argv[];
{

    Display *display;
    XSetWindowAttributes attributes;
    Window window;
    Cursor cursor;
    XEvent event;
    struct cmenu *menu;

    if ((display = XOpenDisplay(NULL)) == NULL) {
        fprintf(stderr, "Couln't open display.\n");
        exit(-1);
    }

    if ((cursor = XCreateFontCursor(display, XC_bogosity)) == NULL) {
        fprintf(stderr, "Couln't create cursor.\n");
        exit(-1);
    }

    attributes.background_pixel = WhitePixel(display, DefaultScreen(display));
    attributes.border_pixel = BlackPixel(display, DefaultScreen(display));
    attributes.event_mask = ButtonPressMask | ButtonReleaseMask | ExposureMask;
    attributes.cursor = cursor;

    if ((window = XCreateWindow(display, RootWindow(display, DefaultScreen(display)), 50, 50, 200, 200, 2, CopyFromParent, CopyFromParent, CopyFromParent, CWBackPixel | CWBorderPixel | CWEventMask | CWCursor, &attributes)) == NULL) {
        fprintf(stderr, "Couln't open window.\n");
        exit(-1);
    }

    if ((menu = cmenu_Create(display, window, NULL)) == NULL) {
        fprintf(stderr, "Couldn't create menu.\n");
        exit(-1);
    }

    AddMenuItems(menu);

    XMapRaised(display, window);

    while (1) {

        XNextEvent(display, &event);

        if (event.type == ButtonPress) {

            XButtonPressedEvent *buttonEvent = (XButtonPressedEvent *) &event;
            long data = 0;
            cmenu_Activate(menu, buttonEvent, &data, cmenu_BackgroundPixel, WhitePixel(display, DefaultScreen(display)));
            fprintf(stderr, "Data = %d\n", data);
        }
    }
}

