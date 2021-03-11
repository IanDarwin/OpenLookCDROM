/* xfunctions.c
 *
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include "math.h"
#include "list.h"

typedef struct parallelogram_Struct {
	float llx;
	float lly;
	float urx;
	float ury;
	float ulx;
	float uly;
	float lrx;
	float lry;
	bboxStruct paraBBox;
} parallelogramStruct;

boolean checkIntersect(bbox, hitRgn)
     bboxStruct *bbox;
     parallelogramStruct *hitRgn;
{
  Region myRegion;
  XPoint mypoints[4];
  int value;

  mypoints[0].x = (int)(hitRgn->llx + 0.5);
  mypoints[0].y = (int)(hitRgn->lly + 0.5);
  mypoints[1].x = (int)(hitRgn->lrx + 0.5);
  mypoints[1].y = (int)(hitRgn->lry + 0.5);
  mypoints[2].x = (int)(hitRgn->urx + 0.5);
  mypoints[2].y = (int)(hitRgn->ury + 0.5);
  mypoints[3].x = (int)(hitRgn->ulx + 0.5);
  mypoints[3].y = (int)(hitRgn->uly + 0.5);

  myRegion = XPolygonRegion(mypoints, 4, WindingRule);
  value = XRectInRegion(myRegion, (int)(bbox->llx + 0.5),
			(int)(bbox->lly + 0.5),
			(unsigned int)(bbox->urx - bbox->llx + 0.5),
			(unsigned int)(bbox->ury - bbox->lly + 0.5));
  if(value == RectangleOut)
    return FALSE;
  else
    return TRUE;
}

void updateClipRgn(bbox, rgn)
     bboxStruct *bbox;
     Region *rgn;
{
  XRectangle rect;

  rect.x = (int)(bbox->llx);
  rect.y = (int)(bbox->ury);
  rect.width = (int)(bbox->urx - bbox->llx) + 2;
  rect.height = (int)(bbox->lly - bbox->ury) + 2;

  XUnionRectWithRegion(&rect, *rgn, *rgn);
}

