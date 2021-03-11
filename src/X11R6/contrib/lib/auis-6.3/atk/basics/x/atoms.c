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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/atoms.c,v 1.6 1992/12/15 21:27:21 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <class.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include <atoms.h>
/* Note the order and number of the entries below should be reflected in the atom #defines in atoms.h */
static char *atomnames[]={
    "ATK",
    "INCOMING",
    "TARGETS",
    "TIMESTAMP",
    "MULTIPLE",
    "TEXT",
    "INCR",
    "CLIPBOARD",
    "LENGTH",
    "WM_CHANGE_STATE",
    DROP_PROTOCOL,
    HOST_FILE_NAME,
    "ATK_SHADES",
    "WM_PROTOCOLS",
    "WM_DELETE_WINDOW"
};
#define NUMATOMS (sizeof(atomnames)/sizeof(char *))

static Atom xim_ATOMS;

static Atom *SetAtoms(dpy, buf)
Display *dpy;
char *buf;
{
    Atom *result;
    char *p;
    int i;
    result=(Atom *)malloc(sizeof(Atom)*NUMATOMS);
    if(result==NULL) {
	fprintf(stderr, "xim: couldn't allocate atom cache for display '%s'\n", DisplayString(dpy));
	return NULL;
    }
    p=buf;
    sprintf(p, "%d ", NUMATOMS);
    p+=strlen(p);
    for(i=0;i<NUMATOMS;i++) {
	result[i]=XInternAtom(dpy, atomnames[i], FALSE);
	sprintf(p, "%s %d ", atomnames[i], result[i]);
	p+=strlen(p);
    }
    XChangeProperty(dpy, RootWindow(dpy, DefaultScreen(dpy)), xim_ATOMS, XA_STRING, 8, PropModeReplace, (unsigned char*)buf, strlen(buf));
    return result;
}

Atom *xim_SetupAtoms(dpy, force)
Display *dpy;
boolean force;
{
    int i;
    Atom RetAtom;
    int RetFormat;
    unsigned long RetNumItems;
    unsigned long RetBytesAfter;
    unsigned char *RetData = NULL;
    char buf[1024], *p;

    buf[0]='\0';
    xim_ATOMS=XInternAtom(dpy, "ATK_ATOMS", FALSE);
    if(force || XGetWindowProperty(dpy, RootWindow(dpy, DefaultScreen(dpy)), xim_ATOMS, 0L, 1024L, False, XA_STRING, &RetAtom, &RetFormat, &RetNumItems, &RetBytesAfter,  &RetData)!=Success || RetAtom==None) {
	return SetAtoms(dpy, buf);
    } else {
	Atom *result;
	p=(char *)RetData;
	if(RetData==NULL) return SetAtoms(dpy, buf);
	if(atol(p)!=NUMATOMS) {
	    XFree((char *)RetData);
	    return SetAtoms(dpy, buf);
	}
	result=(Atom *)malloc(sizeof(Atom)*NUMATOMS);
	if(result==NULL) {
	    fprintf(stderr, "xim: couldn't allocate atom cache for display '%s'\n", DisplayString(dpy));
	    return NULL;
	}
	p=index(p, ' ');
	if(p==NULL) return NULL;
	p++;
	while (p && *p) {
	    char *a=index(p, ' ');
	    if(a==NULL) return result;
	    *a='\0';
	    for(i=0;i<NUMATOMS;i++) {
		if(strcmp(p, atomnames[i])==0) {
		    result[i]=(Atom)atol(a+1);
		    break;
		 }
	     }
	     p=index(a+1, ' ');
	     if(p) p++;
	}
	XFree((char *)RetData);
	return result;
    }
}
