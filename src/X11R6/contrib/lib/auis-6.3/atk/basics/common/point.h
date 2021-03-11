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


 



#ifndef POINT_DEFINED

#define POINT_DEFINED

#include <class.h>

struct point {
    long y,x;
};

#define point_X(thePoint) ((thePoint)->x)
#define point_Y(thePoint) ((thePoint)->y)
/* original unmacroified versions */
/*
long point_X();
long point_Y();
*/
void point_SetX(/*Pt,Value*/);
void point_SetY(/*Pt,Value*/);

void point_OffsetPoint(/*Pt,DeltaX, DeltaY*/);
void point_AddPt(/*LHS,RHS*/);
void point_SubPt(/*LHS,RHS*/);
/*void point_SetPt(LHS, NewX, NewY); */
#define point_SetPt(PtToSet,NewX, NewY) \
(    ((PtToSet)->y = (NewY)), \
     ((PtToSet)->x = (NewX)) )

boolean point_ArePtsEqual(/*LHS,RHS*/);
struct point * point_CreatePoint(/*InitX,InitY*/);
#endif /* POINT_DEFINED */
