/* Copyright 1992 by the Andrew Consortium and Carnegie Mellon University. All rights reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/atkatoms.c,v 1.4 1992/12/15 21:27:21 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <class.h>
#include <X11/Xlib.h>

main(argc, argv)
int argc;
char **argv;
{
    char *dpyname;
    Display *dpy;
    if(argc>2) {
	fprintf(stderr, "atkatoms usage: atkatoms [display]\n");
	exit(-1);
    }
    if(argc>1) dpyname=argv[1];
    else dpyname=NULL;
    dpy=XOpenDisplay(dpyname);
    if(dpy==NULL) {
	fprintf(stderr, "atkatoms: couldn't open display '%s'\n", dpyname?dpyname:"(NULL)");
	exit(-1);
    }
    (void) xim_SetupAtoms(dpy, TRUE);
    XFlush(dpy);
    XCloseDisplay(dpy);
}
    
