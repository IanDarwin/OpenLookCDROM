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
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */
#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include "ctwm.h"

Atom _XA_WM_OCCUPATION;
Atom _XA_WM_CURRENTWORKSPACE;
Atom _XA_WM_WORKSPACESLIST;

/* Note that this doesn't assure that ctwm is really running,
   i should set up a communication via flipping a property */

Bool CtwmIsRunning (dpy, scrnum)
Display	*dpy;
int	scrnum;
{
    unsigned char	*prop;
    unsigned long	bytesafter;
    unsigned long	len;
    Atom		actual_type;
    int			actual_format;

    _XA_WM_WORKSPACESLIST = XInternAtom (dpy, "WM_WORKSPACESLIST", True);
    if (_XA_WM_WORKSPACESLIST == None) return (False);
    if (XGetWindowProperty (dpy, RootWindow (dpy, scrnum), _XA_WM_WORKSPACESLIST, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &prop) != Success) return (False);
    if (len == 0) return (False);
    return (True);
}

char **CtwmListWorkspaces (dpy, scrnum)
Display	*dpy;
int	scrnum;
{
    unsigned char	*prop, *p;
    unsigned long	bytesafter;
    unsigned long	len;
    Atom		actual_type;
    int			actual_format;
    int			state;
    char		**ret;
    int			count;
    int			i, l;

    _XA_WM_WORKSPACESLIST = XInternAtom (dpy, "WM_WORKSPACESLIST", True);

    if (XGetWindowProperty (dpy, RootWindow (dpy, scrnum), _XA_WM_WORKSPACESLIST, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &prop) != Success) return ((char**) 0);
    if (len == 0) return ((char**) 0);

    count = 0;
    p = prop;
    l = 0;
    while (l < len) {
	l += strlen (p) + 1;
	p += strlen (p) + 1;
	count++;
    }
    ret = (char**) malloc ((count + 1) * sizeof (char*));

    p = prop;
    l = 0;
    i = 0;
    while (l < len) {
	ret [i++] = (char*) p;
	l += strlen (p) + 1;
	p += strlen (p) + 1;
    }
    ret [i] = (char*) 0;
    return (ret);
}

char *CtwmCurrentWorkspace (dpy, scrnum)
Display	*dpy;
int	scrnum;
{
    unsigned char	*prop;
    unsigned long	bytesafter;
    unsigned long	len;
    Atom		actual_type;
    int			actual_format;

    _XA_WM_CURRENTWORKSPACE = XInternAtom (dpy, "WM_CURRENTWORKSPACE", True);
    if (_XA_WM_CURRENTWORKSPACE == None) return ((char*) 0);

    if (XGetWindowProperty (dpy, RootWindow (dpy, scrnum), _XA_WM_CURRENTWORKSPACE, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &prop) != Success) return ((char*) 0);
    if (len == 0) return ((char*) 0);
    return ((char*) prop);
}

int CtwmChangeWorkspace (dpy, scrnum, workspace)
Display	*dpy;
int	scrnum;
char	*workspace;
{
    _XA_WM_CURRENTWORKSPACE = XInternAtom (dpy, "WM_CURRENTWORKSPACE", True);
    if (_XA_WM_CURRENTWORKSPACE == None) return (0);

    XChangeProperty (dpy, RootWindow (dpy, scrnum), _XA_WM_CURRENTWORKSPACE, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) workspace, strlen (workspace));
    XFlush (dpy);
    return (1);
}

char **CtwmCurrentOccupation (dpy, window)
Display	*dpy;
Window  window;
{
    unsigned char	*prop, *p;
    unsigned long	bytesafter;
    unsigned long	len;
    Atom		actual_type;
    int			actual_format;
    int			count, l, i;
    char		**ret;

    _XA_WM_OCCUPATION = XInternAtom (dpy, "WM_OCCUPATION", True);
    if (_XA_WM_OCCUPATION == None) return ((char**) 0);

    if (XGetWindowProperty (dpy, window, _XA_WM_OCCUPATION, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &prop) != Success) return ((char**) 0);
    if (len == 0) return ((char**) 0);
    
    count = 0;
    p = prop;
    l = 0;
    while (l < len) {
	l += strlen (p) + 1;
	p += strlen (p) + 1;
	count++;
    }
    ret = (char**) malloc ((count + 1) * sizeof (char*));

    p = prop;
    l = 0;
    i = 0;
    while (l < len) {
	ret [i++] = (char*) p;
	l += strlen (p) + 1;
	p += strlen (p) + 1;
    }
    ret [i] = (char*) 0;
    return (ret);
}

int CtwmSetOccupation (dpy, window, occupation)
Display	*dpy;
Window	window;
char	**occupation;
{
    int		len;
    char	**occ;
    char	*occup, *o;

    _XA_WM_OCCUPATION = XInternAtom (dpy, "WM_OCCUPATION", True);
    if (_XA_WM_OCCUPATION == None) return (0);

    occ = occupation;
    len = 0;
    while (*occ++) {
	len += strlen (*occupation) + 1;
    }
    occup = (char*) malloc (len * sizeof (char));

    o = occup;
    while (*occupation) {
	strcpy (o, *occupation);
	o += strlen (*occupation) + 1;
	occupation++;
    }
    XChangeProperty (dpy, window, _XA_WM_OCCUPATION, XA_STRING, 8, 
		     PropModeReplace, (unsigned char *) occup, len - 1);
    XFlush (dpy);
    free (occup);
    return (1);
}

int CtwmAddToCurrentWorkspace (dpy, window)
Display	*dpy;
Window  window;
{
    unsigned char	*prop, *p;
    unsigned long	bytesafter;
    unsigned long	len;
    Atom		actual_type;
    int			actual_format;
    XWindowAttributes	attr;
    unsigned char	*currentw;

    if (! XGetWindowAttributes (dpy, window, &attr)) return (0);

    _XA_WM_CURRENTWORKSPACE = XInternAtom (dpy, "WM_CURRENTWORKSPACE", True);
    if (_XA_WM_CURRENTWORKSPACE == None) return (0);

    if (XGetWindowProperty (dpy, attr.root, _XA_WM_CURRENTWORKSPACE, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &currentw) != Success) return (0);
    if (len == 0) return (0);

    _XA_WM_OCCUPATION = XInternAtom (dpy, "WM_OCCUPATION", True);
    if (_XA_WM_OCCUPATION == None) return (0);

    if (XGetWindowProperty (dpy, window, _XA_WM_OCCUPATION, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &prop) != Success) return (0);
    if (len == 0) return (0);

    strcpy (prop + len, currentw);
    XChangeProperty (dpy, window, _XA_WM_OCCUPATION, XA_STRING, 8, 
		     PropModeReplace, prop, (int) len + strlen (currentw));
    XFlush (dpy);
    return (1);
}


