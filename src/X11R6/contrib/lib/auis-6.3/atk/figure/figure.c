/* figure.c - drawing data object */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
char *figure_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figure.c,v 1.5 1993/05/04 01:18:42 susan Exp $";
#endif

#include <figure.eh>

#include <figobj.ih>
#include <figogrp.ih>

#ifndef _SEPARATEFIGOBJECTS
#include <figorect.ih>
#include <figoell.ih>
#include <figorrec.ih>
#include <figoplin.ih>
#include <figospli.ih>
#include <figotext.ih>
#include <figoins.ih>
#endif

#include <attribs.h>

static void FlattenRefList();

boolean figure__InitializeClass(ClassID)
struct classhdr *ClassID;
{
#ifndef _SEPARATEFIGOBJECTS
    figobj_StaticLoadOnlyThisClass();
    figogrp_StaticLoadOnlyThisClass();
    figorect_StaticLoadOnlyThisClass();
    figoell_StaticLoadOnlyThisClass();
    figorrec_StaticLoadOnlyThisClass();
    figoplin_StaticLoadOnlyThisClass();
    figospli_StaticLoadOnlyThisClass();
    figotext_StaticLoadOnlyThisClass();
    figoins_StaticLoadOnlyThisClass();
#endif

    return TRUE;
}

boolean figure__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figure *self;
{
    struct figogrp *tmp;

    self->ReadOnly = FALSE;
    self->ocounter = 0;

    self->originx = 0;
    self->originy = 0;
    self->printscalex = 1.0;
    self->printscaley = 1.0;

    self->objs_size = 8;
    self->objs = (struct figure_oref *)malloc(self->objs_size * sizeof(struct figure_oref));
    FlattenRefList(self, 0);
    self->root = figure_NULLREF;

    /* create a root group and stick it in */
    tmp = figogrp_New();
    if (!tmp) return FALSE;
    self->root = figure_AlwaysInsertObject(self, tmp, figure_NULLREF, -1);

    return TRUE;
}

void figure__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct figure *self;
{
    int ix;

    for (ix=0; ix<self->objs_size; ix++)
	if (self->objs[ix].o)
	    figobj_Destroy(self->objs[ix].o);

    free(self->objs);
}

char *figure__ViewName(self)
struct figure *self;
{
    return "figview";
}

void figure__SetAttributes(self, attributes)
struct figure *self;
struct attributes *attributes;
{
    while (attributes) {
	if(!strcmp(attributes->key, "readonly")) {
	    self->ReadOnly = attributes->value.integer;
	}
	attributes = attributes->next;
    }
}

static void FlattenRefList(self, ix)
struct figure *self;
long ix;
{
    for (; ix<self->objs_size; ix++) {
	self->objs[ix].o = NULL;
	self->objs[ix].next = figure_NULLREF;
    }
}

long figure__InsertObject(self, o, parent, depth)
struct figure *self;
struct figobj *o;
long parent; 
long depth;
{
    if (figure_GetReadOnly(self))
	return figure_NULLREF;
    return figure_AlwaysInsertObject(self, o, parent, depth);
}

long figure__AlwaysInsertObject(self, o, parent, depth)
struct figure *self;
struct figobj *o;
long parent; /* if figure_NULLREF, put in top-level group, unless there is none. */
long depth; /* 0 = back, 1 = next to back, etc. -1 or a high value means in front. */
{
    long ix, count, oldroot;
    long *pt;

    for (ix=0; ix<self->objs_size; ix++)
	if (!self->objs[ix].o) break;

    if (ix==self->objs_size) {
	self->objs_size *= 2;
	self->objs = (struct figure_oref *)realloc(self->objs, self->objs_size * sizeof(struct figure_oref));
	FlattenRefList(self, ix);
    }

    if (parent==figure_NULLREF)
	parent = self->root;

    if (parent==figure_NULLREF) {
	/* the very first object */
	self->objs[ix].next = figure_NULLREF;
	figobj_SetParent(o, figure_NULLREF, self);
    }
    else {
	struct figogrp *tmp;
	if (!figobj_IsGroup(self->objs[parent].o)) {
	    fprintf(stderr, "fig: tried to insert an object into non-group\n");
	    return figure_NULLREF;
	}
	tmp = (struct figogrp *)(self->objs[parent].o);

	for (pt = figogrp_GetRootPtr(tmp), count=0;
	     *pt != figure_NULLREF && count != depth; 
	     pt = &(self->objs[*pt].next), count++);
	oldroot = *pt;
	*pt = ix;
	self->objs[ix].next = oldroot;
	
	figobj_SetParent(o, parent, self);
	figobj_UpdateParentBounds(o);
    }
    self->objs[ix].o = o;
    self->objs[ix].counter = self->ocounter;
    self->ocounter++;
    figure_SetModified(self);
    return ix;
}

boolean figure__DeleteObject(self, o)
struct figure *self;
struct figobj *o;
{
    if (figure_GetReadOnly(self))
	return FALSE;

    return figure_AlwaysDeleteObject(self, o);
}

/* delete object o and all of its contents. If o is the root group, the fig is emptied and fig->root = NULLREF. */
boolean figure__AlwaysDeleteObject(self, o)
struct figure *self;
struct figobj *o;
{
    long ix;
    long *pt;
    struct figogrp *paro;

    if (figobj_IsGroup(o)) {
	for (ix=figogrp_GetRoot((struct figogrp *)o); ix!=figure_NULLREF; ix=self->objs[ix].next) {
	    figure_AlwaysDeleteObject(self, self->objs[ix].o);
	}
    }

    figobj_UpdateParentBounds(o);
    paro = figobj_GetParent(o);
    if (paro==NULL) {
	ix = self->root;
	self->root = figure_NULLREF;
    }
    else {
	for (pt = figogrp_GetRootPtr(paro); *pt != figure_NULLREF; pt = &(self->objs[*pt].next))
	    if (self->objs[*pt].o == o) break;

	if (*pt == figure_NULLREF) return FALSE;

	ix = *pt;
	*pt = self->objs[*pt].next;
    }

    self->objs[ix].o = NULL;
    figobj_SetParent(o, figure_NULLREF, NULL);

    figure_SetModified(self);

    return TRUE;
}

void figure__LinkObjectByRef(self, ref, parent, depth)
struct figure *self;
long ref;
long parent; /* if figure_NULLREF, put in top-level group, unless there is none. */
long depth; /* 0 = back, 1 = next to back, etc. -1 or a high value means in front. */
{
    if (figure_GetReadOnly(self))
	return;

    figure_AlwaysLinkObjectByRef(self, ref, parent, depth);
}

void figure__AlwaysLinkObjectByRef(self, ref, parent, depth)
struct figure *self;
long ref;
long parent; /* if figure_NULLREF, put in top-level group, unless there is none. */
long depth; /* 0 = back, 1 = next to back, etc. -1 or a high value means in front. */
{
    long *pt;
    long count, oldroot;
    struct figobj *o = self->objs[ref].o;

    if (parent==figure_NULLREF)
	parent = self->root;

    if (parent==figure_NULLREF) {
	/* the very first object */
	self->root = ref;
	self->objs[ref].next = figure_NULLREF;
	figobj_SetParent(o, figure_NULLREF, self);
    }
    else {
	struct figogrp *tmp;
	if (!figobj_IsGroup(self->objs[parent].o)) {
	    fprintf(stderr, "fig: tried to insert an object into non-group\n");
	    return;
	}
	tmp = (struct figogrp *)(self->objs[parent].o);

	for (pt = figogrp_GetRootPtr(tmp), count=0;
	     *pt != figure_NULLREF && count != depth; 
	     pt = &(self->objs[*pt].next), count++);
	oldroot = *pt;
	*pt = ref;
	self->objs[ref].next = oldroot;
	
	figobj_SetParent(o, parent, self);
	figobj_UpdateParentBounds(o);
    }
    figure_SetModified(self);
}

void figure__UnlinkObjectByRef(self, ref)
struct figure *self;
long ref;
{
    if (figure_GetReadOnly(self))
	return;

    figure_AlwaysUnlinkObjectByRef(self, ref);
}

void figure__AlwaysUnlinkObjectByRef(self, ref)
struct figure *self;
long ref;
{
    long *pt;
    struct figogrp *paro;
    struct figobj *o = self->objs[ref].o;

    paro = figobj_GetParent(o);
    if (paro==NULL) {
	return; /* can't unlink root group */
    }

    figobj_UpdateParentBounds(o);

    for (pt = figogrp_GetRootPtr(paro); *pt != figure_NULLREF; pt = &(self->objs[*pt].next))
	if (*pt == ref) break;

    if (*pt == figure_NULLREF) return;

    *pt = self->objs[*pt].next;
    self->objs[ref].next = figure_NULLREF;

    /* we'll leave the parent pointer alone, but don't trust it. */

    figure_SetModified(self);
}

long figure__FindDepthByRef(self, ref)
struct figure *self;
long ref;
{
    long *pt;
    long count;
    struct figogrp *paro;
    struct figobj *o = self->objs[ref].o;

    paro = figobj_GetParent(o);
    if (paro==NULL) {
	return 0; /* root group */
    }

    for (pt = figogrp_GetRootPtr(paro), count=0; 
	  *pt != figure_NULLREF;
	  pt = &(self->objs[*pt].next), count++)
	if (*pt == ref) break;

    if (*pt == figure_NULLREF) return -1;
    return count;
}

static struct figobj *EOT_AllArea(self, grp, callfun, rock)
struct figure *self;
long grp;
boolean (*callfun)();
long rock;
{
    long ix, startval;
    struct figobj *this, *ores;
    boolean res;

    if (grp==figure_NULLREF) {
	startval = self->root;
    }
    else {
	struct figogrp *gr = (struct figogrp *)self->objs[grp].o;
	startval = figogrp_GetRoot(gr);
    }
    for (ix=startval; ix!=figure_NULLREF; ix=self->objs[ix].next) {
	this=self->objs[ix].o;
	res = (*callfun)(this, ix, self, rock);
	if (res)
	    return this;
	if (figobj_IsGroup(this)) {
	    ores = EOT_AllArea(self, ix, callfun, rock);
	    if (ores)
		return ores;
	}
    }
    return NULL;
}

static struct figobj *EOT_OverlapArea(self, grp, area, callfun, rock)
struct figure *self;
long grp;
struct rectangle *area;
boolean (*callfun)();
long rock;
{
    long ix, startval;
    struct figobj *this, *ores;
    boolean res;
    struct rectangle tmp;

    if (grp==figure_NULLREF) {
	startval = self->root;
    }
    else {
	struct figogrp *gr = (struct figogrp *)self->objs[grp].o;
	startval = figogrp_GetRoot(gr);
    }
    for (ix=startval; ix!=figure_NULLREF; ix=self->objs[ix].next) {
	this=self->objs[ix].o;
	rectangle_IntersectRect(&tmp, area, figobj_GetBounds(this, NULL));
	if (!rectangle_IsEmptyRect(&tmp)) {
	    res = (*callfun)(this, ix, self, rock);
	    if (res)
		return this;
	    if (figobj_IsGroup(this)) {
		ores = EOT_OverlapArea(self, ix, area, callfun, rock);
		if (ores)
		    return ores;
	    }
	}
    }
    return NULL;
}

static struct figobj *EOT_IncludeArea(self, grp, area, callfun, rock)
struct figure *self;
long grp;
struct rectangle *area;
boolean (*callfun)();
long rock;
{
    long ix, startval;
    struct figobj *this, *ores;
    boolean res;

    if (grp==figure_NULLREF) {
	startval = self->root;
    }
    else {
	struct figogrp *gr = (struct figogrp *)self->objs[grp].o;
	startval = figogrp_GetRoot(gr);
    }
    for (ix=startval; ix!=figure_NULLREF; ix=self->objs[ix].next) {
	this=self->objs[ix].o;
	if (rectangle_IsEnclosedBy(figobj_GetBounds(this, NULL), area)) {
	    res = (*callfun)(this, ix, self, rock);
	    if (res)
		return this;
	}
	if (figobj_IsGroup(this)) {
	    ores = EOT_IncludeArea(self, ix, area, callfun, rock);
	    if (ores)
		return ores;
	}
    }
    return NULL;
}

/* call callfun on every object in a group in an area. If area is NULL, call callfun on every object in the group. If grp is figure_NULLREF, then every object in the fig is checked.
callfun should be of the form
  boolean callfun(struct figobj *o, long ref, struct figure *self, rock)
If an invocation of callfun returns TRUE, the enumeration halts and EnumerateObjects returns that figobj. Otherwise, EnumerateObjects returns NULL.
*/
struct figobj *figure__EnumerateObjectTree(self, grp, area, allowoverlap, callfun, rock)
struct figure *self;
long grp;
struct rectangle *area;
boolean allowoverlap;
boolean (*callfun)();
long rock;
{
    if (!area) {
	return EOT_AllArea(self, grp, callfun, rock);
    }
    else if (allowoverlap)  {
	return EOT_OverlapArea(self, grp, area, callfun, rock);
    }
    else  {
	return EOT_IncludeArea(self, grp, area, callfun, rock);
    }
}


/* call callfun on every object in an area. If area is NULL, call callfun on every object.
callfun should be of the form
  boolean callfun(struct figobj *o, long ref, struct figure *self, rock)
If an invocation of callfun returns TRUE, the enumeration halts and EnumerateObjects returns that figobj. Otherwise, EnumerateObjects returns NULL.
*/
struct figobj *figure__EnumerateObjectGroup(self, grp, area, allowoverlap, callfun, rock)
struct figure *self;
long grp;
struct rectangle *area;
boolean allowoverlap;
boolean (*callfun)();
long rock;
{
    int ix;
    struct rectangle tmp;
    struct figobj *this;
    boolean res;
    long first;

    if (grp!=figure_NULLREF) {
	struct figogrp *gr = (struct figogrp *)self->objs[grp].o;
	first = figogrp_GetRoot(gr);
    }
    else {
	first = figure_RootObjRef(self);
    }

    if (!area) {
	for (ix=first; ix!=figure_NULLREF; ix=self->objs[ix].next) {
	    this=self->objs[ix].o;
	    res = (*callfun)(this, ix, self, rock);
	    if (res)
		return this;
	}
	return NULL;
    }
    else if (allowoverlap)  {
	for (ix=first; ix!=figure_NULLREF; ix=self->objs[ix].next) {
	    rectangle_IntersectRect(&tmp, area, figobj_GetBounds(this, NULL));
	    if (!rectangle_IsEmptyRect(&tmp)) {
		res = (*callfun)(this, ix, self, rock);
		if (res)
		    return this;
	    }
	}
	return NULL;
    }
    else  {
	for (ix=first; ix!=figure_NULLREF; ix=self->objs[ix].next) {
	    this=self->objs[ix].o;
	    if (rectangle_IsEnclosedBy(figobj_GetBounds(this, NULL), area)) {
		res = (*callfun)(this, ix, self, rock);
		if (res)
		    return this;
	    }
	}
	return NULL;
    }
}

/* call callfun on every object in an area. If area is NULL, call callfun on every object.
callfun should be of the form
  boolean callfun(struct figobj *o, long ref, struct figure *self, rock)
If an invocation of callfun returns TRUE, the enumeration halts and EnumerateObjects returns that figobj. Otherwise, EnumerateObjects returns NULL.
*/
struct figobj *figure__EnumerateObjects(self, area, allowoverlap, callfun, rock)
struct figure *self;
struct rectangle *area;
boolean allowoverlap;
boolean (*callfun)();
long rock;
{
    int ix;
    struct rectangle tmp;
    struct figobj *this;
    boolean res;

    if (!area) {
	for (ix=0; ix<self->objs_size; ix++) {
	    this=self->objs[ix].o;
	    if (this) {
		res = (*callfun)(this, ix, self, rock);
		if (res)
		    return this;
	    }
	}
	return NULL;
    }
    else if (allowoverlap)  {
	for (ix=0; ix<self->objs_size; ix++) {
	    this=self->objs[ix].o;
	    if (this) {
		rectangle_IntersectRect(&tmp, area, figobj_GetBounds(this, NULL));
		if (!rectangle_IsEmptyRect(&tmp)) {
		    res = (*callfun)(this, ix, self, rock);
		    if (res)
			return this;
		}
	    }
	}
	return NULL;
    }
    else  {
	for (ix=0; ix<self->objs_size; ix++) {
	    this=self->objs[ix].o;
	    if (this) {
		if (rectangle_IsEnclosedBy(figobj_GetBounds(this, NULL), area)) {
		    res = (*callfun)(this, ix, self, rock);
		    if (res)
			return this;
		}
	    }
	}
	return NULL;
    }
}

long figure__FindRefByObject(self, o)
struct figure *self;
struct figobj *o;
{
    int ix;

    for (ix=0; ix<self->objs_size; ix++)
	if (self->objs[ix].o == o)
	    return ix;

    return figure_NULLREF;
}
 
struct figobj *figure__FindObjectByRef(self, ref)
struct figure *self;
long ref;
{
    if (ref<0 || ref>=self->objs_size)
	return NULL;

    return self->objs[ref].o;
}

static long FRBPSplot(self, gref, howhit, x, y, delta, ptref)
struct figure *self;
long gref;
enum figobj_HitVal howhit;
long delta;
long x, y;
long *ptref;
{
    long ix, jx;
    enum figobj_HitVal res;
    struct figogrp *grp = (struct figogrp *)self->objs[gref].o;

    for (ix=figogrp_GetRoot(grp); ix!=figure_NULLREF; ix=self->objs[ix].next) {
	res = figobj_HitMe(self->objs[ix].o, x, y, delta, ptref);
	if (((int)res) >= ((int)howhit)) {
	    /* got it */
	    return ix;
	}
	if (figobj_IsGroup(self->objs[ix].o)) {
	    jx = FRBPSplot(self, ix, howhit, x, y, delta, ptref);
	    if (jx != figure_NULLREF) {
		return jx;
	    }
	}
    }
    return figure_NULLREF;
}

/* search the tree under group gref for an object at x, y. If recursive is true, whole tree is searched; otherwise, just that group. howhit determines how good the hit has to be to be returned. *ptref will be set to a handle if HitMe returns one. delta is the allowable distance from a handle (in fig coords). */ 
long figure__FindRefByPos(self, gref, recursive, howhit, delta, x, y, ptref)
struct figure *self;
long gref;
boolean recursive;
enum figobj_HitVal howhit;
long delta;
long x, y;
long *ptref;
{
    int ix;
    long tmp = figobj_NULLREF;
    enum figobj_HitVal res;

    if (gref == figure_NULLREF) {
	for (ix=0; ix<self->objs_size; ix++)
	    if (self->objs[ix].o) {
		res = figobj_HitMe(self->objs[ix].o, x, y, delta, &tmp);
		if (((int)res) >= ((int)howhit)) {
		    /* got it */
		    if (ptref)
			*ptref = tmp;
		    return ix;
		}
	    }
	return figure_NULLREF;
    }
    else if (recursive) {
	ix = FRBPSplot(self, gref, howhit, x, y, delta, &tmp);
	if (ix != figure_NULLREF && ptref)
	    *ptref = tmp;
	return ix;
    }
    else {
	struct figogrp *grp = (struct figogrp *)self->objs[gref].o;
	for (ix=figogrp_GetRoot(grp); ix!=figure_NULLREF; ix=self->objs[ix].next) {
	    res = figobj_HitMe(self->objs[ix].o, x, y, delta, &tmp);
	    if (((int)res) >= ((int)howhit)) {
		/* got it */
		if (ptref)
		    *ptref = tmp;
		return ix;
	    }
	}
	return figure_NULLREF;
    }
}

struct rectangle *figure__GetOverallBounds(self)
struct figure *self;
{
    struct figobj *o = NULL;

    if (!self->bboxdirty) {
	return &(self->bbox);
    } 

    self->bboxdirty = FALSE;
    if (figure_RootObjRef(self) != figure_NULLREF) {
	o = self->objs[figure_RootObjRef(self)].o;
    }
    if (o)
	self->bbox = *(figobj_GetBounds(o, NULL));
    else
	rectangle_EmptyRect(&self->bbox);

    return &(self->bbox);
}

#define LINELENGTH (250)
static char buf[LINELENGTH+1];

static void WriteObject(self, oref, fp, writeid, level)
struct figure *self;
long oref;
FILE *fp;
long writeid;
int level;
{
    struct figobj *o = self->objs[oref].o;
    /* printf("fig: WriteObject(%d): %s\n", oref, class_GetTypeName(o)); */

    if (!figobj_IsGroup(o)) {
	figobj_Write(o, fp, writeid, level+1);
    }
    else {
	long root = figogrp_GetRoot((struct figogrp *)o);
	long ix;

	if (root == figure_NULLREF && figobj_GetParentRef(o) != figure_NULLREF) {
	    return; /* it's an empty group; don't bother writing it out. */
	}

	figobj_Write(o, fp, writeid, level+1);

	for (ix=root; ix!=figure_NULLREF; ix=self->objs[ix].next)
	    WriteObject(self, ix, fp, writeid, level+1);
	fprintf(fp, "#end\n");
    }
}

static long ReadObject(self, o, oref, fp, oid)
struct figure *self;
struct figobj *o; 
long oref;
FILE *fp;
long oid;
{
    long ix, subref, tid;
    char namebuf[100];
    struct figobj *subo;

    ix = figobj_Read(o, fp, oid);
    if (ix!=dataobject_NOREADERROR) return ix;

    /* printf("fig: ReadObject(%d): %s\n", oref, class_GetTypeName(o)); */
    if (!figobj_IsGroup(o))
	return dataobject_NOREADERROR;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;

    while (1) {
	if (!strncmp(buf, "#end", 4)) 
	    return dataobject_NOREADERROR;

	ix = sscanf(buf, "\\begindata{%[^,],%ld}", namebuf, &tid);
	if (ix!=2) return dataobject_BADFORMAT;

	if (!class_IsTypeByName(namebuf, "figobj"))
	    return dataobject_BADFORMAT;
	subo = (struct figobj *)class_NewObject(namebuf);
	if (!subo) return dataobject_OBJECTCREATIONFAILED;

	subref = figure_AlwaysInsertObject(self, subo, oref, -1);
	ix = ReadObject(self, subo, subref, fp, tid);
	if (ix!=dataobject_NOREADERROR) return ix;

	if (fgets(buf, LINELENGTH, fp) == NULL)
	    return dataobject_PREMATUREEOF;
    }
}

long figure__Write(self, fp, writeid, level)
struct figure *self;
FILE *fp;
long writeid;
int level;
{
    if (figure_GetWriteID(self) != writeid) {
	figure_SetWriteID(self, writeid);

	fprintf(fp, "\\begindata{%s,%ld}\n", class_GetTypeName(self), figure_GetID(self));
	fprintf(fp, "$origin %d %d\n", self->originx, self->originy);
	fprintf(fp, "$printscale %f %f\n", self->printscalex, self->printscaley);
	fprintf(fp, "#none\n"); /* this is to make future expansion easier. */
	if (self->root == figure_NULLREF)
	    fprintf(fp, "#empty\n");
	else {
	    WriteObject(self, self->root, fp, writeid, level+1);
	}

	fprintf(fp, "\\enddata{%s,%ld}\n", class_GetTypeName(self), figure_GetID(self));
    }

    return figure_GetID(self);
}

void figure__WritePartial(self, fp, writeid, level, list, listnum, origin)
struct figure *self;
FILE *fp;
long writeid;
int level;
long *list;
long listnum;
struct point *origin;
{
    struct figogrp *tmpgrp;
    int ix;
    struct rectangle *tmp, bbox;

    rectangle_EmptyRect(&bbox);
    for (ix=0; ix<listnum; ix++) {
	tmp = figobj_GetBounds(self->objs[list[ix]].o, NULL);
	rectangle_UnionRect(&bbox, &bbox, tmp);
    }

    fprintf(fp, "\\begindata{%s,%ld}\n", class_GetTypeName(self), figure_GetID(self));
    fprintf(fp, "$origin %d %d\n", bbox.left-64, bbox.top-64);
    if (origin) {
	origin->x = bbox.left-64;
	origin->y = bbox.top-64;
    }
    fprintf(fp, "$printscale %f %f\n", self->printscalex, self->printscaley);
    fprintf(fp, "#none\n");

    /* create a temporary group and set its handlebox to (-1,-1,-1,-1) so that we know that those values are meaningless. */
    tmpgrp = figogrp_New();
    tmpgrp->handlebox.left = (-1);
    tmpgrp->handlebox.top = (-1);
    tmpgrp->handlebox.width = (-1);
    tmpgrp->handlebox.height = (-1);

    figogrp_Write(tmpgrp, fp, writeid, level+1);
    for (ix=0; ix<listnum; ix++) {
	WriteObject(self, list[ix], fp, writeid, level+1);
    }
    fprintf(fp, "#end\n");
    figogrp_Destroy(tmpgrp);

    fprintf(fp, "\\enddata{%s,%ld}\n", class_GetTypeName(self), figure_GetID(self));
}

long figure__Read(self, fp, id)
struct figure *self;
FILE *fp;
long id;
{
    long tid, ix, ref, val1, val2;
    double fal1, fal2;
    char namebuf[100];
    struct figobj *o;

    figure_SetID(self, figure_UniqueID(self)); 

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$origin %d %d", &val1, &val2);
    if (ix!=2) return dataobject_BADFORMAT;
    self->originx = val1;
    self->originy = val2;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$printscale %lf %lf", &fal1, &fal2);
    if (ix!=2) return dataobject_BADFORMAT;
    self->printscalex = fal1;
    self->printscaley = fal2;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    if (strncmp(buf, "#none", 5)) return dataobject_BADFORMAT;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;

    if (figure_RootObjRef(self) != figure_NULLREF)
	figure_AlwaysDeleteObject(self, self->objs[figure_RootObjRef(self)].o);

    if (!strncmp(buf, "#empty", 6)) {

    }
    else {
	ix = sscanf(buf, "\\begindata{%[^,],%ld}", namebuf, &tid);
	if (ix!=2) return dataobject_BADFORMAT;

	if (!class_IsTypeByName(namebuf, "figobj"))
	    return dataobject_BADFORMAT;
	o = (struct figobj *)class_NewObject(namebuf);
	if (!o) return dataobject_OBJECTCREATIONFAILED;
	
	ref = figure_AlwaysInsertObject(self, o, figure_NULLREF, -1);
	self->root = ref;
	ix = ReadObject(self, o, ref, fp, tid);
	if (ix!=dataobject_NOREADERROR) return ix;
    }

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "\\enddata{%[^,],%ld}", namebuf, &tid);
    if (ix!=2) return dataobject_MISSINGENDDATAMARKER;

    if ((id && tid!=id) || strcmp(namebuf, class_GetTypeName(self)))
	return dataobject_MISSINGENDDATAMARKER;

    figure_GetOverallBounds(self);

    return dataobject_NOREADERROR;     
}

/* put stuff from file into focus. The root group of the file is *not* inserted; everything in the root group is copied into the focus group of self. The origin field of the file is returned in origin.
### If the root group of the file has attributes active, they will overwrite the focus group's original attributes. This is not a problem for pasting, since a copied fig has nothing active.
*/
long figure__ReadPartial(self, fp, id, focus, origin)
struct figure *self;
FILE *fp;
long id;
long focus;
struct point *origin;
{
    long tid, ix, val1, val2;
    double fal1, fal2;
    char namebuf[100];
    struct figobj *o;
    struct figogrp *gro;
    /* long unid;
    if (id==0) 
	unid = figure_UniqueID(self);
    else
	unid = id;
    figure_SetID(self, unid);*/ /* ### ??? */

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$origin %d %d", &val1, &val2);
    if (ix!=2) return dataobject_BADFORMAT;
    if (origin) {
	origin->x = val1;
	origin->y = val2;
    }

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "$printscale %lf %lf", &fal1, &fal2);
    if (ix!=2) return dataobject_BADFORMAT;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    if (strncmp(buf, "#none", 5)) return dataobject_BADFORMAT;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;

    if (!strncmp(buf, "#empty", 6)) {

    }
    else {
	ix = sscanf(buf, "\\begindata{%[^,],%ld}", namebuf, &tid);
	if (ix!=2) return dataobject_BADFORMAT;

	if (!class_IsTypeByName(namebuf, "figobj"))
	    return dataobject_BADFORMAT;

	o = figure_FindObjectByRef(self, focus);

	ix = ReadObject(self, o, focus, fp, tid); 

	if (ix!=dataobject_NOREADERROR) return ix;
    }

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    ix = sscanf(buf, "\\enddata{%[^,],%ld}", namebuf, &tid);
    if (ix!=2) return dataobject_MISSINGENDDATAMARKER;

    if ((id && tid!=id) || strcmp(namebuf, class_GetTypeName(self)))
	return dataobject_MISSINGENDDATAMARKER;

    /*printf("figure_ReadPartial: done OK\n");*/
    return dataobject_NOREADERROR; 
}
