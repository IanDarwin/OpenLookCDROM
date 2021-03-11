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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/rectlist.c,v 2.8 1992/12/15 21:41:42 rr2b R6tape $";
#endif


 

#include <class.h>
#include <rectlist.eh>

#include <graphic.ih>
#include <view.ih>

struct rlist_rectangle {
    int bottom, top, left,right;
};

#define MAXRECTANGLES 20

/* SCANAGAIN should either be set to -1 or to (oldnum-1).  It should be set to -1 if the rectangles added to the OldList stack ontop of each other. */

#define SCANAGAIN (-1)

static struct rlist_rectangle OldList[MAXRECTANGLES];
static struct rlist_rectangle NewList[MAXRECTANGLES];
static int EndScan = 0;
static int EndOld = 0;
static int EndNew = 0;


static void Intersect(oldnum, newnum)
int oldnum, newnum;  {
    int ob, ot, ol, or, nb, nt, nl, nr, ib, it;
    
    ob = OldList[oldnum].bottom;
    ot = OldList[oldnum].top;
    ol = OldList[oldnum].left;
    or = OldList[oldnum].right;
    nb = NewList[newnum].bottom;
    nt = NewList[newnum].top;
    nl = NewList[newnum].left;
    nr = NewList[newnum].right;

    if (nb <= ot || nt >= ob || nr <= ol || nl >= or) return;
    if (nb <= ob)  {
	OldList[oldnum].top = nb;
	if (nt <= ot)  {
	    it = ot;
	}
	else  {
	    rectlist_AddOldRectangle(nt, ot, ol, or);
	    it = nt;
	}
	NewList[newnum].bottom = it;
	ib = nb;
    }
    else  {
	NewList[newnum].top = ob;
	if (nt < ot)  {
	    rectlist_AddNewRectangle(ot, nt, nl, nr, oldnum + 1);
	    it = ot;
	}
	else  {
	    it = nt;
	}
	OldList[oldnum].bottom = it;
	ib = ob;
    }
    if (nl < ol)  {
	rectlist_AddNewRectangle(ib, it, nl, ol, SCANAGAIN);
    }
    else if (nl > ol)  {
	rectlist_AddOldRectangle(ib, it, ol, nl);
    }
    if (nr < or)  {
	rectlist_AddOldRectangle(ib, it, nr, or);
    }
    else if (nr > or)  {
	rectlist_AddNewRectangle(ib, it, or, nr, SCANAGAIN);
    }
}

void rectlist__ResetList(classID)
struct classheader *classID;
{
    EndScan = 0;
    EndOld = 0;
    EndNew = 0;
}

void rectlist__AddOldRectangle(classID, bottom, top, left, right)
struct classheader *classID;
long bottom, top, left, right;  {
    OldList[EndOld].bottom = bottom;
    OldList[EndOld].top = top;
    OldList[EndOld].left = left;
    OldList[EndOld++].right = right;
}

void rectlist__AddNewRectangle(classID, bottom, top, left, right, startscan) 
struct classheader *classID;
long bottom, top, left, right, startscan; {
    /* This routine adds a new rectangle to the newlist and then intersects it with the elements in the OldList.  If startscan is -1 the intersection i not done.  Otherwise it gives the location in the OldList to start doing the intersection. */

    register int i;
    register int newnum = EndNew++;

    NewList[newnum].bottom = bottom;
    NewList[newnum].top = top;
    NewList[newnum].left = left;
    NewList[newnum].right = right;
    
    if (startscan == -1) return;
    if (startscan == 0) EndScan = EndOld;
    for (i = startscan; i < EndScan && NewList[newnum].bottom != NewList[newnum].top && NewList[newnum].left != NewList[newnum].right; i++)  {
	if (OldList[i].bottom != OldList[i].top && OldList[i].left != OldList[i].right)
	    Intersect(i, newnum);
    }
}

void rectlist__InvertRectangles(classID, vPtr)
struct classheader *classID;
    struct view *vPtr;
{
    register int i;
    struct rectangle invertRect;
    struct graphic *pat;

    for (i = 0; i < EndOld; i++)  {
	if (OldList[i].bottom != OldList[i].top && OldList[i].left != OldList[i].right)  {

	    rectangle_SetRectSize(&invertRect, OldList[i].left, OldList[i].top, OldList[i].right - OldList[i].left, OldList[i].bottom - OldList[i].top);
	    pat = view_BlackPattern(vPtr);
	    view_FillRect(vPtr, &invertRect, pat);
	}
    }    
    for (i = 0; i < EndNew; i++)  {
	if (NewList[i].bottom != NewList[i].top && NewList[i].left != NewList[i].right)  {
	    rectangle_SetRectSize(&invertRect, NewList[i].left, NewList[i].top, NewList[i].right - NewList[i].left, NewList[i].bottom - NewList[i].top);
	    pat = view_BlackPattern(vPtr);
	    view_FillRect(vPtr, &invertRect, pat);

	}
    }    
}

/* To use this code for the selection region:

ResetRectangleList();
AddOldRectangle(TopRectangle);
AddOldRectangle(MiddleRectangle);
AddOldRectangle(BottomRectangle);
AddNewRectangle(NewTopRectangle,0);
AddNewRectangle(NewMiddleRectangle,0);
AddNewRectangle(NewEndRectangle,0);
InvertRectangles();
 */
