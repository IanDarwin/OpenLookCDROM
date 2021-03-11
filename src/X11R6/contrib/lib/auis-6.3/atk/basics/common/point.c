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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/point.c,v 2.6 1992/12/15 21:27:56 rr2b R6tape $";
#endif


 


#include <point.h>

/* These have been macrofied */

/*
long point_X(Pt)
struct point * Pt; {
    return Pt->x;
}

long point_Y(Pt)
struct point * Pt; {
    return Pt->y;
}
*/

void point_SetX(Pt,Value)
struct point * Pt;
long Value; {
    Pt->x=Value;
}

void point_SetY(Pt,Value)
struct point * Pt;
long Value; {
     Pt->y=Value;
}

void point_OffsetPoint(Pt,DeltaX, DeltaY)
struct point * Pt;
long DeltaY;
long DeltaX; {
    Pt->y += DeltaY;
    Pt->x += DeltaX;
}

void point_AddPt(LHS,RHS)
struct point * LHS;
struct point * RHS;{
    LHS->y += RHS->y;
    LHS->x += RHS->x;
}

void point_SubPt(LHS,RHS)
struct point * LHS;
struct point * RHS; {
    LHS->y -= RHS->y;
    LHS->x -= RHS->x;
}

/*
 More macrofied
*/
/*

void point_SetPt(Pt,NewX, NewY)
struct point * Pt;
long NewY;
long NewX;{
    Pt->y = NewY;
    Pt->x = NewX;
}

*/

boolean point_ArePtsEqual(LHS,RHS)
struct point * LHS;
struct point * RHS; {
    return (LHS->x == RHS->x) && (LHS->y == RHS->y);
}

struct point * point_CreatePoint(InitX, InitY)
long InitY;
long InitX; {
    struct point * RetValue;

    RetValue = (struct point *) malloc(sizeof(struct point));
    point_SetPt(RetValue,InitX,InitY);
    return RetValue;
}
