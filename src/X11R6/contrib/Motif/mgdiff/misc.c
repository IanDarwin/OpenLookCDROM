#ifndef lint
static char rcsid[] = "misc.c,v 2.0 1994/05/19 02:01:19 dan Exp";
#endif

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <X11/Xos.h>

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <X11/cursorfont.h>
#include <X11/Shell.h>
#include <Xm/SashP.h>

#include "mgdiff.h"

int max (int i, int j)
{
    return ((i > j) ? i : j);
}

int min (int i, int j)
{
    return ((i < j) ? i : j);
}

/* 
 * copy a stream up to the EOF to a file
 */
int copy_to_file (FILE *fin, char *name)
{
    FILE *fout;

    if ((fout = fopen (name, "a")) == NULL)
	return (0);
    while (!feof (fin)) {
	char buffer[BUFSIZ];
	int nitems;

	nitems = fread (buffer, 1, BUFSIZ, fin);
	if (fwrite (buffer, 1, nitems, fout) != nitems)
	    break;
    }
    if (ferror (fin) || ferror (fout)) {
	(void) fclose (fout);
	return (1);
    }
    return ((fclose (fout) == 0));
}

void set_cursor (Widget w)
{
    static Cursor watch = 0;

    if (!watch)
	watch = XCreateFontCursor (XtDisplay (w), XC_watch);

    XDefineCursor (XtDisplay (w), XtWindow (w), watch);
    XmUpdateDisplay (w);
}

void reset_cursor (Widget w)
{
    XUndefineCursor (XtDisplay (w), XtWindow (w));
    XmUpdateDisplay (w);
}

/* 
 * traverse up to the top shell
 */
Widget get_top_shell (Widget w)
{
    while (w && !XtIsWMShell (w))
        w = XtParent (w);
    return (w);
}

#if sun
char *strerror (int errnum)
{
    extern int sys_nerr;
    extern char *sys_errlist[];

    if ((0 < errnum) && (errnum < sys_nerr))
	return (sys_errlist[errnum]);
    else
	return ("");
}
#endif

void add_editres (Widget shell)
{
#if EDITRES && X11R5
    extern void _XEditResCheckMessages (Widget widget,
					XtPointer closure,
					XEvent *event,
					Boolean *continue_to_dispatch);

    XtAddEventHandler (shell, (EventMask) 0, True, _XEditResCheckMessages, NULL);
#endif
}

/* 
 * from Heller's Motif Programming Manual; eliminate the ability to 
 * traverse to a PanedWindow's sashes.
 */
void turn_off_sash_traversal (Widget pane)
{
    WidgetList children;
    int num_children;

    XtVaGetValues (pane, XmNchildren, &children, XmNnumChildren, &num_children, NULL);
    while (num_children-- > 0)
	if (XmIsSash (children[num_children]))
	    XtVaSetValues (children[num_children], XmNtraversalOn, False, NULL);
}
