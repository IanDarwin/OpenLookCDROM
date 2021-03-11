/* figobj.ch - fig element object */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figobj.ch,v 1.4 1993/05/04 01:18:42 susan Exp $
*/

#include <rect.h>
#include <point.h>

#define figobj_NULLREF (-1)

/* status values for NotifyObservers */
#define figobj_DATACHANGED 1

/* return values for Build() */
enum figobj_Status { 
    figobj_Done,
    figobj_NotDone,
    figobj_Failed
};

/* return values for HitMe(). */
enum figobj_HitVal {
    figobj_Miss,	/* miss (ptref is undefined) */
    figobj_HitInside,	/* inside body (ptref is undefined) */
    figobj_HitBody,	/* on body part (indicated by a ptref) */
    figobj_HitHandle 	/* on handle (indicated by a ptref) */
};

/* Hints for objects to tell the constraint manager what
    a particular handle does for the object. */
enum figobj_HandleType {
    figobj_None,
    figobj_ULCorner,
    figobj_LLCorner,
    figobj_LRCorner,
    figobj_URCorner,
    figobj_MiddleLeft,
    figobj_MiddleRight,
    figobj_MiddleTop,
    figobj_MiddleBottom,
    figobj_Center,
    figobj_LeftEndpoint,
    figobj_RightEndpoint,
    figobj_InteriorPoint,
    figobj_Point
};

struct figobj_Attachment {
    boolean on;
    long rposx, rposy;
    long offx, offy;
};

class figobj : dataobject [dataobj] {

    classprocedures:

      InitializeClass() returns boolean;
      InitializeObject(struct figobj *self) returns boolean;
      FinalizeObject(struct figobj *self);

    overrides:
      Write(FILE *fp, long id, int level) returns long;
      Read(FILE *file, long id) returns long;

    methods:
      WriteBody(FILE *fp);
      ReadBody(FILE *file, boolean recompute) returns long;
      ToolName(struct figtoolview *v, long rock) returns char *;
      ToolModify(struct figtoolview *v, long rock); 
      Instantiate(struct figtoolview *v, long rock) returns struct figobj *;  

      Draw(struct figview *v); 
      Sketch(struct figview *v);
      DrawAttachments(struct figview *v);
      ClearAttachments();
      PrintObject(struct figview *v, FILE *file, char *prefix);
      Select(struct figview *v);
      SetNumHandles(long num);
      Build(enum view_MouseAction action, struct figview *v, long x, long y, long clicks) returns enum figobj_Status;
      HitMe(long x, long y, long delta, long *ptref) returns enum figobj_HitVal;
      BasicHitMe(long x, long y, long delta, long *ptref) returns enum figobj_HitVal;
      Reshape(enum view_MouseAction action, struct figview *v, long x, long y, boolean handle, long ptref) returns boolean;
      AddParts(enum view_MouseAction action, struct figview *v, long x, long y, boolean handle, long ptref) returns boolean;
      DeleteParts(enum view_MouseAction action, struct figview *v, long x, long y, boolean handle, long ptref) returns boolean;
      MoveHandle(x, y, ptref);
      Reposition(long xd, long yd);
      InheritVAttributes(struct figattr *attr, unsigned long mask);
      UpdateVAttributes(struct figattr *attr, unsigned long mask) returns unsigned long;
      RecomputeBounds();
      GetBounds(struct figview *vv) returns struct rectangle *;
      GetSelectedBounds(struct figview *vv) returns struct rectangle *;
      ComputeSelectedBounds();
      UpdateParentBounds();
      SetParent(long parentref, struct *fig ancestor);
      GetHandleType(int ptref) returns enum figobj_HandleType;
      GetCanonicalHandles() returns long *;
      StabilizeAttachments(boolean keepproport);

    macromethods:
      GetParent()  ((self)->parent)
      GetParentRef()  ((self)->parentref)
      GetAncestorFig()  ((self)->figo)
      IsGroup()  ((self)->isgroup)
      IsInset()  ((self)->isinset)
      AttributesUsed()  ((self)->attrused)
      GetHandles()  ((self)->pts)
      GetNumHandles()  ((self)->numpts)
      SetHandle(num, x, y)  (point_SetPt(&((self)->pts[num]), (x), (y)))
      PosX()  ((self)->x)
      PosY()  ((self)->y)
      SetBoundsRect(left, top, width, height)  (rectangle_SetRectSize(&((self)->bounds), (left), (top), (width), (height)))
      GetVAttributes()  ((self)->attr)
      GetIVAttributes()  ((self)->iattr)
      GetReadOnly()  ((self)->figo && figure_GetReadOnly((self)->figo))

      IsAttachmentActive(int num)  ((self)->vas[num].on)
      SetAttachmentActive(int num, boolean val)  ((self)->vas[num].on = (val), (self)->anyattachmentsactive = (self)->anyattachmentsactive || (val))
      SetAttachmentPosX(int num, long pos) ((self)->vas[num].rposx=(pos))
      SetAttachmentOffX(int num, long offset) ((self)->vas[num].offx=(offset))
      GetAttachmentPosX(int num) ((self)->vas[num].rposx)
      GetAttachmentOffX(int num) ((self)->vas[num].offx)
      SetAttachmentPosY(int num, long pos) ((self)->vas[num].rposy=(pos))
      SetAttachmentOffY(int num, long offset) ((self)->vas[num].offy=(offset))
      GetAttachmentPosY(int num) ((self)->vas[num].rposy)
      GetAttachmentOffY(int num) ((self)->vas[num].offy)

      GetHandleX(int num) ((self)->pts[num].x)
      GetHandleY(int num) ((self)->pts[num].y)
      
    data:
      long parentref;
      struct figogrp *parent;
      struct figure *figo;
      boolean isgroup, isinset;
      long attrused;
      long x, y;
      struct figattr *attr;
      struct figattr *iattr;
      struct rectangle bounds, selbounds;
      long pt_size;
      long numpts;
      struct point *pts;
      struct figobj_Attachment *vas;
      boolean anyattachmentsactive;
};
