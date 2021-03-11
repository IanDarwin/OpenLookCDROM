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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/bpair.c,v 2.13 1993/09/19 00:03:13 rr2b Exp $";
#endif


 

/* bpair.c	

	An lpair without the line between the parts
	and without movable or cursors 

*/


#include <class.h>
#include <graphic.ih>
#include <view.ih>
#include <cursor.ih>

#include <im.ih>
#include <rect.h>

#include <lpair.ih>
#include <bpair.eh>


#define BARWIDTH 0
#define GRAVITY 1

/* Forward Declarations */
static void lpair_ComputeSizes ();
static void lpair_ResetDimensions();

/* the following two routines are needed for FullUpdate 
	They are copied from lpair.c, with deletion of the line-between code,
	code for movable borders, and the calls on DesiredSize   */

static void lpair_ComputeSizes (l)
register struct lpair *l;
{

    int totalsize, i = 0;

/* Find out how much space the two must share. */
    if (l->typex == lpair_VERTICAL)
        totalsize = bpair_GetLogicalWidth(l);
    else
        totalsize = bpair_GetLogicalHeight(l);

/* See if we don't have any space -- actually we should be testing 
	to see if children can fit at all, but for, now a simple test for zero */
    if (totalsize == 0) {
	l->objcvt[0] = l->objcvt[1] = 0;
	return;
    }

    --totalsize;	/* Make room for the bar in the middle. */

    switch(l->sizeform) { /* Allocate space for the 'hard' allocated view. */
        case lpair_PERCENTAGE:
            i = 1;
            l->objcvt[i] = (l->objsize[i] * totalsize) / 100;
            break;
        case lpair_BOTTOMFIXED:
            i = 1;
            l->objcvt[i] = l->objsize[i];
            break;
        case lpair_TOPFIXED:
            i = 0;
            l->objcvt[i] = l->objsize[i];
            break;
    }
/* Give the rest to the other view. */
    l->objcvt[1-i] = totalsize - l->objcvt[i];
}

static void lpair_ResetDimensions(self)
register struct lpair *self;
{

    register int i, x, y;
    register struct  view *child;
    struct rectangle enclosingRect;

    x = 0; y = 0;
    for (i = 0; i < 2; i++) { /* Loop over the two halves of the lpair. */
	child = (struct view *)self->obj[i];
	if (self->typex == lpair_VERTICAL) {
	      rectangle_SetRectSize(&enclosingRect,
			x, y, self->objcvt[i],  lpair_GetLogicalHeight(self));
	      view_InsertView(child, self, &enclosingRect);
	      x += self->objcvt[i] + 2 * BARWIDTH + 1;
	}
	else {
	      rectangle_SetRectSize(&enclosingRect,
			x, y,  lpair_GetLogicalWidth(self), self->objcvt[i]);
	      view_InsertView(child, self, &enclosingRect);
	      y += self->objcvt[i] + 2 * BARWIDTH + 1;
	}
    }
}


	boolean
bpair__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	register struct bpair  *self;
	{
	    /* there is no need for the rest of the code which was here...
	     lpair__InitializeObject will be called before this InitializeObject. */
    self->header.lpair.movable = FALSE;
    return TRUE;
}
	void
bpair__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	register struct bpair  *self;
{
}


void 
bpair__FullUpdate(self, type, left, top, width, height)
	register struct bpair  *self;
	register enum view_UpdateType  type;
	register long  left, top, width, height;
{
/* ( the following, including derogatory comment, is copied from lpair.c) */

			/*  All this code needs changed */
    register struct view * leftTopObject = self->header.lpair.obj[0];
    register struct view * rightBottomObject = self->header.lpair.obj[1];
    struct rectangle r;

    self->header.lpair.movable = FALSE;

    self->header.lpair.needsfull = 0;

    lpair_ComputeSizes((struct lpair *)self);
    lpair_ResetDimensions((struct lpair *)self);	/* reset the child lpair sizes */

    view_GetLogicalBounds(leftTopObject, &r);
    view_FullUpdate(leftTopObject, type, r.left, r.top, r.width, r.height);
    view_GetLogicalBounds(rightBottomObject, &r);
    view_FullUpdate(rightBottomObject, type, r.left, r.top, r.width, r.height);

	/* deleted the line and cursor code */
}
