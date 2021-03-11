/* figure.ch - drawing data object */
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figure.ch,v 1.4 1993/05/04 01:18:42 susan Exp $
*/

#include <rect.h>

#define figure_NULLREF (-1)

/* status values for NotifyObservers */
#define figure_DATACHANGED 1

struct figure_oref {
    struct figobj *o;
    long counter;
    long next;
};

class figure : dataobject [dataobj] {

    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct figure *self) returns boolean;
      FinalizeObject(struct figure *self);

    overrides:
      SetAttributes(struct attributes *attributes);
      Write(FILE *fp, long writeid, int level) returns long;
      Read(FILE *file, long id) returns long;
      ViewName() returns char *;

    methods:
      InsertObject(struct figobj *o, long parent, long depth) returns long;
      DeleteObject(struct figobj *o) returns boolean;
      LinkObjectByRef(long ref, long parent, long depth);
      UnlinkObjectByRef(long ref);
      AlwaysInsertObject(struct figobj *o, long parent, long depth) returns long;
      AlwaysDeleteObject(struct figobj *o) returns boolean;
      AlwaysLinkObjectByRef(long ref, long parent, long depth);
      AlwaysUnlinkObjectByRef(long ref);
      EnumerateObjects(struct rectangle *area, boolean allowoverlap, procedure func, long rock) returns struct figobj *;
      EnumerateObjectGroup(long groupref, struct rectangle *area, boolean allowoverlap, procedure func, long rock) returns struct figobj *;
      EnumerateObjectTree(long groupref, struct rectangle *area, boolean allowoverlap, procedure func, long rock) returns struct figobj *;
      FindRefByObject(struct figobj *o) returns long;
      FindObjectByRef(long ref) returns struct figobj *;
      FindRefByPos(long gref, boolean recursive, enum figobj_HitVal howhit, long delta, long x, long y, long *ptref) returns long;
      FindDepthByRef(long ref) returns long;
      WritePartial(FILE *fp, long writeid, int level, long *list, long listsize, struct point *origin);
      ReadPartial(FILE *file, long id, long focus, struct point *origin) returns long;
      GetOverallBounds() returns struct rectangle *;

    macromethods:
      RootObjRef()  ((self)->root)
      GetObjectCounter()  ((self)->ocounter)
      SetChildBoundMod()  ((self)->bboxdirty = TRUE)
      GetReadOnly()  ((self)->ReadOnly)
      SetReadOnly(val)  ((self)->ReadOnly = (val))
      GetOriginX()  ((self)->originx)
      GetOriginY()  ((self)->originy)
      SetOrigin(x, y) ((self)->originx = (x), (self)->originy = (y))
      GetPrintScaleX() ((self)->printscalex)
      GetPrintScaleY() ((self)->printscaley)
      SetPrintScale(x, y)  ((self)->printscalex = (x), (self)->printscaley = (y))

    data:
      struct figure_oref *objs;
      long objs_size;
      long root;
      boolean ReadOnly;

      long ocounter;
      boolean bboxdirty;
      struct rectangle bbox;
      long originx, originy;

      double printscalex, printscaley;
};

