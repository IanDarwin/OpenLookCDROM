/* scpanner.c - scrollbar / panner box view */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/scpanner.c,v 1.4 1992/12/15 21:43:33 rr2b R6tape $";
#endif

/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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


#include <scpanner.eh>
#include <panner.ih>

boolean scrollandpanner__InitializeObject(classID, self)
struct classheader *classID;
struct scrollandpanner *self;
{
    self->panner = panner_New();
    return TRUE;
}

void scrollandpanner__FinalizeObject(classID, self)
struct classheader *classID;
struct scrollandpanner *self;
{
    panner_Destroy(self->panner);
}

struct scrollandpanner *scrollandpanner__Create(classID, scrollee, location)
struct classheader *classID;
struct view *scrollee;
int location;
{
    struct scrollandpanner *retval=NULL;

    retval = scrollandpanner_New();
    if (retval==NULL) return NULL;

    scrollandpanner_SetView(retval, scrollee);
    scrollandpanner_SetLocation(retval, location);

    return retval;
}

void scrollandpanner__SetScrollee(self, scrollee)
struct scrollandpanner *self;
struct view *scrollee;
{
    super_SetScrollee(self, scrollee);
    panner_SetScrollee(self->panner, scrollee);
}

void scrollandpanner__SetChild(self, child)
struct scrollandpanner *self;
struct view *child;
{
    panner_SetChild(self->panner, child);
    super_SetChild(self, self->panner);
}

struct view *scrollandpanner__GetChild(self)
struct scrollandpanner *self;
{
    return panner_GetChild(self->panner);
}

