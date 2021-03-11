/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 



#define region_PROGRAMMERVERSION 2

class region {
methods:

    XorRegion(struct region * RHS, struct region * Result);
    UnionRegion(struct region * RHS, struct region * Result);
    IntersectRegion(struct region * RHS, struct region * Result);
    SubtractRegion(struct region * RHS, struct region * Result);
    ClearRegion();
    RectRegion(struct rectangle * NewRegnRect);
    DuplicateRegion() returns struct region *;
    OffsetRegion(long DeltaX, long DeltaY);
    InsetRegion(long DeltaX, long DeltaY);

    IsRegionEmpty() returns boolean;
    IsPointInRegion(struct point * TestPt) returns boolean;
    IsRectInRegion(struct rectangle * TestRect) returns boolean;
    AreRegionsEqual(struct region * TestRegion) returns boolean;

    GetBoundingBox(struct rectangle *rect) returns struct rectangle *;

macromethods:
    GetRegionData() ((self)->regionData)    /* Only used in the X code.  probably should subclass region to do this */

classprocedures:
    CreateEmptyRegion() returns struct region *;
    CreateRectRegion(struct rectangle * RegionShape) returns struct region *;
    CreateOvalRegion(struct rectangle * RegionShape) returns struct region *;
    CreatePolyRegion(struct point * PointArray, short PointCount) returns struct region *;
    CopyRegion(struct region * Destination, struct region * Source);
    FinalizeObject(struct region * self);
    InitializeObject(struct region *self) returns boolean;
data:
    struct _XRegion * regionData; /* really XRegion, but I don't want to include all of X just to get the typedef */
};

