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


 



#ifndef RECTANGLE_DEFINED

#define RECTANGLE_DEFINED

#include <point.h>

struct rectangle {
    long top,left,height,width;
};

struct bothrectangle {
    union  {
	struct rectangle rectformat;
	struct { struct point upperLeft, lowerRightOffset;} pointformat;
    } bothformats;
} ;


#define rectangle_SetRectSize(LHS,sleft,stop,swidth,sheight) \
(    ((LHS)->top = (stop)), \
    ((LHS)->left = (sleft)), \
    ((LHS)->height = (sheight)), \
    ((LHS)->width = (swidth)) )

#define rectangle_Left(Rect)  ((Rect)->left)
#define rectangle_Top(Rect)  ((Rect)->top)
#define rectangle_Right(Rect) ((Rect)->left + (Rect)->width)
#define rectangle_Bottom(Rect) ((Rect)->top + (Rect)->height)
#define rectangle_Width(Rect) ((Rect)->width)
#define rectangle_Height(Rect) ((Rect)->height)
#define rectangle_GetRectSize(r, x, y, w, h) \
		(*(x) = (r)->left, *(y) = (r)->top, \
		*(w) = (r)->width, *(h) = (r)->height)

void rectangle_IntersectRect(/*Result,LHS,RHS*/);
void rectangle_UnionRect(/*Result,LHS,RHS*/);
/*void rectangle_SetRectSize(LHS, left,top,width,height); */
void rectangle_SetRectSides(/*LHS, left,top,right, bottom*/);
void rectangle_SetRectCorners(/*LHS, OneCorner,OtherCorner*/);
void rectangle_InsetRect(/*LHS, DeltaX, DeltaY*/);
void rectangle_OffsetRect(/*LHS, DeltaX, DeltaY*/);
void rectangle_EmptyRect(/*Rect*/);
boolean rectangle_IsEmptyRect(/*TestedRectangle*/);
boolean rectangle_IsEqualRect(/*LHS, RHS*/);
boolean rectangle_IsEnclosedBy(/*InnerRect, OuterRect*/);
boolean rectangle_IsPtInRect(/*TestPoint,TestRect*/);
short rectangle_PtToAngle(/*SamplePoint,ReferenceRect*/);
/*long rectangle_Left(Rect);*/
/*long rectangle_Right(Rect);*/
/*long rectangle_Top(Rect);*/
/*long rectangle_Bottom(Rect);*/
/*long rectangle_Width(Rect);*/
/*long rectangle_Height(Rect);*/
void rectangle_SetLeft(/*Rect,Value*/);
void rectangle_SetRight(/*Rect,Value*/);
void rectangle_SetHeight(/*Rect,Value*/);
void rectangle_SetWidth(/*Rect,Value*/);
void rectangle_SetTop(/*Rect,Value*/);
void rectangle_SetBottom(/*Rect,Value*/);

struct rectangle *rectangle_CreateRectCorners(/*OneCorner,OtherCorner*/);
struct rectangle *rectangle_CreateRectSize(/*left,top,width,height*/);
struct rectangle *rectangle_CreateRectSides(/*left,top,right,bottom*/);
struct rectangle *rectangle_Duplicate(/*Rect*/);

#endif /* RECTANGLE_DEFINED */
