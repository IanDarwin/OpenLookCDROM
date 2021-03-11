/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ July 1993 ]
 */
#include <stdio.h>
#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xatom.h>
#else
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#endif

#include "ctwm.h"

Window awindow = 0x5c0000d;
char *awspc1 = "lecom", *awspc2 = "root";

main (argc, argv)
int argc;
char **argv;
{
    Display *dpy;
    Window w;
    char   **wlist, **wl, **occupation;
    char   *cur;
    int    status;

    dpy = XOpenDisplay (NULL);
    if (dpy == NULL) {
	fprintf (stderr, "Can't open display\n");
	exit (1);
    }

/****************************************************************/

    if (! CtwmIsRunning) {
	fprintf (stderr, "ctwm is not running\n");
	exit (1);
    }

/****************************************************************/

    wlist = CtwmListWorkspaces (dpy, 0);
    if (wlist == (char**) 0) {
	fprintf (stderr, "cannot obtain workspaces list\n");
	exit (1);
    }
    printf ("list of workspaces : ");
    wl = wlist;
    while (*wl) {
	printf ("\"%s\" ", *wl++);
    }
    printf ("\n");

/****************************************************************/

    cur = CtwmCurrentWorkspace (dpy, 0);
    if (cur == NULL) {
	fprintf (stderr, "cannot obtain current workspace\n");
	exit (1);
    }
    printf ("current workspace : %s\n", cur);

/****************************************************************/

    status = CtwmChangeWorkspace (dpy, 0, awspc1);
    if (! status) {
	fprintf (stderr, "cannot change the current workspace\n");
	exit (1);
    }

/****************************************************************/

    wlist = CtwmCurrentOccupation (dpy, awindow);
    if (wlist == (char**) 0) {
	fprintf (stderr, "cannot obtain occupation of window  %x\n", awindow);
	exit (1);
    }
    printf ("Occupation of window %d : ", awindow);
    wl = wlist;
    while (*wl) {
	printf ("\"%s\" ", *wl++);
    }
    printf ("\n");

/****************************************************************/

    occupation = (char**) malloc (3 * sizeof (char*));
    occupation [0] = awspc1;
    occupation [1] = awspc2;
    occupation [2] = NULL;
    status = CtwmSetOccupation (dpy, awindow, occupation);
    if (! status) {
	fprintf (stderr, "cannot change the occupation of window %x\n", awindow);
    }
    printf ("occupation of window %x changed to \"lecom\", \"root\"\n", awindow);

/****************************************************************/
    status = CtwmAddToCurrentWorkspace (dpy, awindow);
    if (! status) {
	fprintf (stderr, "cannot change the occupation of window %x\n", awindow);
    }
    printf ("window %x now occupy the current workspace\n", awindow);
}

