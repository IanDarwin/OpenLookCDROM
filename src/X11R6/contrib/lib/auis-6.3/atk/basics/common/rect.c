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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/rect.c,v 2.9 1993/05/04 01:08:09 susan Exp $";
#endif


 

/*
 * $Log: rect.c,v $
 * Revision 2.9  1993/05/04  01:08:09  susan
 * RCS Tree Split
 *
 * Revision 2.8.1.1  1993/02/02  01:07:29  rr2b
 * new R6tape branch
 *
 * Revision 2.8  1992/12/15  21:28:38  rr2b
 * more disclaimerization fixing
 *
 * Revision 2.7  1992/12/14  20:36:31  rr2b
 * disclaimerization
 *
 * Revision 2.6  1991/09/12  16:00:15  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.5  1989/02/17  18:40:04  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 *
 * Revision 2.4  89/02/08  15:59:18  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/06  19:32:47  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  89/02/06  15:32:53  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.1  88/09/27  14:29:53  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  01:14:35  ghoti
 * *** empty log message ***
 * 
 * Revision 1.1  88/09/01  01:47:18  zs01
 * "initial
 * 
 * Revision 5.0  88/05/29  21:10:14  ajp
 * Up to date - tested as of 5/29/88
 * bumped version number to 5.0
 * named "june88"
 * 
 * Revision 1.12  88/05/22  22:10:34  ajp
 * Update to bring in line with IBM/MIT Release
 * 
 * Revision 1.5  87/12/06  17:46:41  urling
 * cmu update
 * 
 * Revision 1.3  87/11/20  08:58:40  urling
 * cmu update
 * 
 * Revision 1.4  87/11/20  07:13:47  urling
 * *** empty log message ***
 * 
 * Revision 1.3  87/11/19  11:39:30  urling
 * Nov. 19 cmu update
 * 
 * Revision 1.10  87/11/11  19:03:47  wjh
 * added def'n of macro rectangle_GetRectSize
 * corrected the names of macros rectangle_IsEmptyRect & rectangle_IsEqualRect
 * fixed Union and Intersect to know about empty rectangles
 * made some arguments registers
 * fixed PtInRect to exclude points on the bottom and right
 * 
 *
 */

#include <rect.h>

/******************** methods ******************/

	void
rectangle_UnionRect(Result,LHS, RHS)
	register struct rectangle *LHS, *RHS;
	register struct rectangle *Result;
{
	register long left, top;
		/* Result may be same as RHS or LHS, so we
		must defer changing Result until after compute is done */

	if (rectangle_IsEmptyRect(LHS))  {*Result = *RHS; return;}
	if (rectangle_IsEmptyRect(RHS))  {*Result = *LHS; return;}

	left = (LHS->left <  RHS->left) ? LHS->left : RHS->left;
	top = (LHS->top <  RHS->top) ? LHS->top : RHS->top;
	Result->width = (LHS->left+LHS->width <  RHS->left+RHS->width) 
			? RHS->left+RHS->width - left
			: LHS->left+LHS->width - left;
	Result->height = (LHS->top+LHS->height <  RHS->top+RHS->height) 
			? RHS->top+RHS->height - top
			: LHS->top+LHS->height - top;
	Result->left = left;
	Result->top = top;
}

	void
rectangle_IntersectRect(Result,LHS, RHS)
	register struct rectangle *Result;
	register struct rectangle *LHS, *RHS;
{
	register long left, top;
		/* Result may be same as RHS or LHS, so we
		must defer changing Result until after compute is done */

	if (rectangle_IsEmptyRect(LHS) || rectangle_IsEmptyRect(RHS)) 
		{rectangle_EmptyRect(Result);  return;}

	left = (LHS->left <  RHS->left) ? RHS->left : LHS->left;
	top = (LHS->top <  RHS->top) ? RHS->top : LHS->top;
	Result->width = (LHS->left+LHS->width <  RHS->left+RHS->width) 
			? LHS->left+LHS->width - left
			: RHS->left+RHS->width - left;
	Result->height = (LHS->top+LHS->height <  RHS->top+RHS->height) 
			? LHS->top+LHS->height - top
			: RHS->top+RHS->height - top;
	Result->left = left;
	Result->top = top;
}

/*
void rectangle_SetRectSize(LHS, left,top,width,height)
register struct rectangle * LHS;
long top;
long left;
long height;
long width;{
    LHS->top = top;
    LHS->left = left;
    LHS->height = height;
    LHS->width = width;
}
*/

void rectangle_SetRectSides(LHS, left,top,right,bottom)
register struct rectangle * LHS;
long top;
long left;
long bottom;
long right;{
    LHS->top = top;
    LHS->left = left;
    LHS->height = bottom-top;
    LHS->width = right-left;
}

void rectangle_SetRectCorners(LHS, OneCorner, OtherCorner)
register struct rectangle * LHS;
register struct point * OneCorner;
register struct point * OtherCorner; {

    long LeftEdge;
    long RightEdge;
    long TopEdge;
    long BottomEdge;

    LeftEdge = RightEdge = point_X(OneCorner);
    if (point_X(OneCorner) < point_X(OtherCorner)) {
	RightEdge = point_X(OtherCorner);
        }
    else {
	LeftEdge = point_X(OtherCorner);
        }
    TopEdge = BottomEdge = point_Y(OneCorner);
    if (point_Y(OneCorner) < point_Y(OtherCorner)) {
	BottomEdge = point_Y(OtherCorner);
        }
    else {
	TopEdge = point_Y(OtherCorner);
        }
    rectangle_SetRectSides(LHS,LeftEdge,TopEdge,RightEdge,BottomEdge);
}

void rectangle_InsetRect(LHS, DeltaX, DeltaY)
register struct rectangle * LHS;
long DeltaY;
long DeltaX;{
    LHS->top += DeltaY;
    LHS->height -= 2*DeltaY;
    LHS->left += DeltaX;
    LHS->width -= 2*DeltaX;
}

void rectangle_OffsetRect(LHS, DeltaX, DeltaY)
struct rectangle * LHS;
long DeltaY;
long DeltaX;{
    LHS->top += DeltaY;
    LHS->left += DeltaX;
}

void rectangle_EmptyRect(Rect)
struct rectangle * Rect; {
    rectangle_SetRectSize(Rect,0,0,-1,-1);
}

boolean rectangle_IsEmptyRect(TestRect)
struct rectangle * TestRect;{
    return (TestRect->width <= 0) || (TestRect->height <= 0);
}

boolean rectangle_IsEqualRect(LHS, RHS)
struct rectangle * LHS;
struct rectangle * RHS;{
    return ( (LHS->left == RHS->left) && (LHS->top == RHS->top) &&
	     (LHS->width == RHS->width) &&
	     (LHS->height == RHS->height) ) ||
	   (rectangle_IsEmptyRect(LHS) && rectangle_IsEmptyRect(RHS));
}

boolean rectangle_IsEnclosedBy(InnerRect, OuterRect)
/* Tests to see if InnerRect is enclosed by he OuterRect, i.e., InnerRect is
in OuterRect */
struct rectangle * InnerRect;
struct rectangle * OuterRect;{
    if (InnerRect->left < OuterRect->left) return FALSE;
    if (InnerRect->top < OuterRect->top) return FALSE;
    if (InnerRect->left+InnerRect->width > OuterRect->left+OuterRect->width) return FALSE;
    if (InnerRect->top+InnerRect->height > OuterRect->top+OuterRect->height) return FALSE;
    return TRUE;
}

boolean rectangle_IsPtInRect(TestPoint,TestRect)
struct rectangle * TestRect;
struct point * TestPoint; {
    if (point_X(TestPoint) < TestRect->left) return FALSE;
    if (point_Y(TestPoint) < TestRect->top) return FALSE;
    if (point_X(TestPoint) >= TestRect->left+TestRect->width) return FALSE;
    if (point_Y(TestPoint) >= TestRect->top+TestRect->height) return FALSE;
    return TRUE;
}

short rectangle_PtToAngle(SamplePoint,ReferenceRect)
struct rectangle * ReferenceRect;
struct point * SamplePoint; {
    /* I need a precise definition of this to implement it,
	so for now, punt */
    return 30; /* 30 degrees is a nice number */
}


void rectangle_SetLeft(Rect,Value)
struct rectangle * Rect;
long Value; {
    Rect->left = Value;
}

void rectangle_SetRight(Rect,Value)
struct rectangle * Rect;
long Value; {
    Rect->width = Value - Rect->left;
}

void rectangle_SetWidth(Rect,Value)
struct rectangle * Rect;
long Value; {
    Rect->width = Value;
}

void rectangle_SetHeight(Rect,Value)
struct rectangle * Rect;
long Value; {
    Rect->height = Value;
}

void rectangle_SetTop(Rect,Value)
struct rectangle * Rect;
long Value; {
    Rect->top = Value;
}

void rectangle_SetBottom(Rect,Value)
struct rectangle * Rect;
long Value; {
    Rect->height = Value - Rect->top;
}

/************** class procedures *******************/


struct rectangle * rectangle_CreateRectCorners(OneCorner,OtherCorner)
struct point * OneCorner;
struct point * OtherCorner; {
    struct rectangle * RetValue;

    RetValue = (struct rectangle *) malloc(sizeof(struct rectangle));
    rectangle_SetRectCorners(RetValue,OneCorner,OtherCorner);
    return RetValue;
}

struct rectangle * rectangle_CreateRectSize(left,top,width,height)
long top;
long left;
long height;
long width;{
    struct rectangle * RetValue;

    RetValue = (struct rectangle *) malloc(sizeof(struct rectangle));
    rectangle_SetRectSize(RetValue,left,top,width,height);
    return RetValue;
}


struct rectangle * rectangle_CreateRectSides(left,top,right, bottom)
long top;
long left;
long bottom;
long right;{
    struct rectangle * RetValue;

    RetValue = (struct rectangle *) malloc(sizeof(struct rectangle));
    rectangle_SetRectSides(RetValue,left,top,right,bottom);
    return RetValue;
}

struct rectangle * rectangle_Duplicate(Rect)
struct rectangle * Rect; {
    struct rectangle * RetValue;

    RetValue = (struct rectangle *) malloc(sizeof(struct rectangle));
    *RetValue = *Rect;
    return RetValue;
}
