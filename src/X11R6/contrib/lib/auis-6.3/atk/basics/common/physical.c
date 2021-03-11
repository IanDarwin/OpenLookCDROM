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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/physical.c,v 2.6 1992/12/15 21:27:56 rr2b R6tape $";
#endif


 

/* This is a utility package only to be used by im and graphic (and
 perhaps other machine dependent modules that I do not yet know about,
 such as the cursor system. This package provides a way to convert
 between the coordinate spaces of a graphic and wm "hardware" */

#include <class.h>
#include <graphic.ih>


#define SINGLEWINDOW 1

long physical_LogicalPtToGlobalX(CoordinateSystem,LogicalPoint)
struct graphic * CoordinateSystem;
struct point * LogicalPoint; {
#if SINGLEWINDOW
    return point_X(LogicalPoint)+
	   point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds);
#else /* SINGLEWINDOW */
    return point_X(LogicalPoint)-
	   rectangle_Left(&CoordinateSystem->localBounds);
#endif /* SINGLEWINDOW */
}   

long physical_LogicalPtToGlobalY(CoordinateSystem,LogicalPoint)
struct graphic * CoordinateSystem;
struct point * LogicalPoint; {
#if SINGLEWINDOW
    return point_Y(LogicalPoint)+
	   point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds);
#else /* SINGLEWINDOW */
    return point_Y(LogicalPoint)-
	   rectangle_Top(&CoordinateSystem->localBounds);
#endif /* SINGLEWINDOW */
}   

void physical_LogicalPtToGlobalPt(CoordinateSystem,tempPoint)
struct graphic * CoordinateSystem;
struct point * tempPoint; {
#if SINGLEWINDOW
    point_OffsetPoint(tempPoint,
	   point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds),
	   point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds));
#else /* SINGLEWINDOW */
    point_OffsetPoint(tempPoint,
	   -rectangle_Left(&CoordinateSystem->localBounds),
	   -rectangle_Top(&CoordinateSystem->localBounds));
#endif /* SINGLEWINDOW */
}   

void physical_GlobalPtToLogicalPt(CoordinateSystem,tempPoint)
struct graphic * CoordinateSystem;
struct point * tempPoint; {
#if SINGLEWINDOW
    point_OffsetPoint(tempPoint,
	   -(point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds)),
	   -(point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds)));
#else /* SINGLEWINDOW */
    point_OffsetPoint(tempPoint,
	   rectangle_Left(&CoordinateSystem->localBounds),
	   rectangle_Top(&CoordinateSystem->localBounds));
#endif /* SINGLEWINDOW */
}   



long physical_LogicalXToGlobalX(CoordinateSystem,LogicalX)
struct graphic * CoordinateSystem;
long LogicalX; {
#if SINGLEWINDOW
    return LogicalX +
	   point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds);
#else /* SINGLEWINDOW */
    return LogicalX -
	   rectangle_Left(&CoordinateSystem->localBounds);
#endif /* SINGLEWINDOW */
}   

long physical_LogicalYToGlobalY(CoordinateSystem,LogicalY)
struct graphic * CoordinateSystem;
long LogicalY; {
#if SINGLEWINDOW
    return LogicalY +
	   point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds);
#else /* SINGLEWINDOW */
    return LogicalY -
	   rectangle_Top(&CoordinateSystem->localBounds);
#endif /* SINGLEWINDOW */
}   

long physical_GlobalXToLogicalX(CoordinateSystem,PhysicalX)
struct graphic * CoordinateSystem;
long PhysicalX; {
#if SINGLEWINDOW
    return PhysicalX - (
	   point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds));
#else /* SINGLEWINDOW */
    return PhysicalX +
	   rectangle_Left(&CoordinateSystem->localBounds);
#endif /* SINGLEWINDOW */
}

long physical_GlobalYToLogicalY(CoordinateSystem,PhysicalY)
struct graphic * CoordinateSystem;
long PhysicalY; {
#if SINGLEWINDOW
    return PhysicalY - (
	   point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds));
#else /* SINGLEWINDOW */
    return PhysicalY +
	   rectangle_Top(&CoordinateSystem->localBounds);
#endif /* SINGLEWINDOW */
}

void physical_LogicalToGlobalRect(CoordinateSystem,TempRect)
struct graphic * CoordinateSystem;
struct rectangle * TempRect; {
#if SINGLEWINDOW
    rectangle_OffsetRect(TempRect,
	   point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds),
	   point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds));
#else /* SINGLEWINDOW */
    rectangle_OffsetRect(TempRect,
	   -rectangle_Left(&CoordinateSystem->localBounds),
	   -rectangle_Top(&CoordinateSystem->localBounds));
#endif /* SINGLEWINDOW */
}

void physical_GlobalToLogicalRect(CoordinateSystem,TempRect)
struct graphic * CoordinateSystem;
struct rectangle * TempRect; {
#if SINGLEWINDOW
    rectangle_OffsetRect(TempRect,
	   -(point_X(&CoordinateSystem->physicalOrigin)-
	   rectangle_Left(&CoordinateSystem->localBounds)),
	   -(point_Y(&CoordinateSystem->physicalOrigin)-
	   rectangle_Top(&CoordinateSystem->localBounds)));
#else /* SINGLEWINDOW */
    rectangle_OffsetRect(TempRect,
	   rectangle_Left(&CoordinateSystem->localBounds),
	   rectangle_Top(&CoordinateSystem->localBounds));
#endif /* SINGLEWINDOW */
}

