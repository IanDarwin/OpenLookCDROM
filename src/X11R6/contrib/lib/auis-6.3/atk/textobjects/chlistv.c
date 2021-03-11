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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textobjects/RCS/chlistv.c,v 1.4 1993/01/19 18:35:45 Zarf Exp $";
#endif

#include "class.h"
#include "text.ih"
#include "view.ih"
#include "chlist.ih"
#include "style.ih"
#include "chlistv.eh"

static boolean chlistview_Debug = FALSE;


void chlistview__SetUpdateRegion(self, pos, len)
struct chlistview *self;
long pos;
long len;
{
    struct textview *tv = (struct textview *) self;
    long i;

    for (i = 0; i < tv->nLines; i++) {
	if (pos < mark_GetEndPos(tv->lines[i].data) && (pos + len) >= mark_GetPos(tv->lines[i].data)) {
	    mark_SetModified(tv->lines[i].data, TRUE);
	}
    }
    if (pos < mark_GetEndPos(tv->predrawn) && (pos + len) >= mark_GetPos(tv->predrawn)) {
	mark_SetModified(tv->predrawn, TRUE);
    }
    if (pos < mark_GetEndPos(tv->prepredrawn) && (pos + len) >= mark_GetPos(tv->prepredrawn)) {
	mark_SetModified(tv->prepredrawn, TRUE);
    }

    chlistview_WantUpdate(self, self);
}


boolean chlistview__InitializeObject(c, self)
struct classheader *c;
struct chlistview *self;
{
    self->highlightedItem = -1;
    self->numStylesAllocated = 0;
    self->normalStyles = NULL;
    self->highlightedStyles = NULL;
    return(TRUE);
}

void chlistview__FinalizeObject(c, self)
struct classheader *c;
struct chlistview *self;
{
    if (self->normalStyles != NULL) {
	free(self->normalStyles);
    }
    if (self->highlightedStyles != NULL) {
	free(self->highlightedStyles);
    }
}

struct view *chlistview__Hit(self, action, x, y, nclicks)
struct chlistview *self;
enum view_MouseAction action;
long x, y, nclicks;
{
    int pos, index, region;
    struct chlist *l;

    super_Hit(self, action, x, y, nclicks);
    l = (struct chlist *) chlistview_GetDataObject(self);
    pos = chlistview_GetDotPosition(self);
    index = chlist_GetIndexByPosition(l, pos, &region, NULL, NULL);
    if (index >= 0) {
	chlistview_HighlightItem(self, index);
	if (l->ItemList[index].proc) {
	    (l->ItemList[index].proc)(l->ItemList[index].rock, l, action, nclicks, index, region);
	}
    }
    return((struct view *) self);
}

void chlistview__ActivateItem(self, pos)
struct chlistview *self;
int pos;
{
    int region, index;
    struct chlist *l;

    l = (struct chlist *) chlistview_GetDataObject(self);
    index = chlist_GetIndexByPosition(l, pos, &region, NULL, NULL);
    if (index >= 0) {
	chlistview_HighlightItem(self, index);
	if (l->ItemList[index].proc) {
	    (l->ItemList[index].proc)(l->ItemList[index].rock, l, view_LeftDown, 1, index, region);
	}
    }
   chlistview_WantInputFocus(self, self);
}

void chlistview__HighlightItem(self, index)
struct chlistview *self;
long index;
{
    if (self->highlightedItem != index) {
	struct chlist *l;
	long len;
	struct listitem *item, *nextItem;

	chlistview_UnhighlightItem(self, self->highlightedItem);

	if (index >= 0) {
	    l = (struct chlist *) chlistview_GetDataObject(self);
	    item = chlist_FindItemByIndex(l, index);
	    if (index == (chlist_GetNumItems(l) - 1)) {
		len = chlist_GetLength(l);
	    }
	    else {
		nextItem = chlist_FindItemByIndex(l, index + 1);
		len = nextItem->loc;
	    }
	    len -= (item->loc + 1);

	    chlistview_SetUpdateRegion(self, item->loc, len);
	    chlistview_WantUpdate(self, self);
	}

	self->highlightedItem = index;
    }
}

void chlistview__UnhighlightItem(self, index)
struct chlistview *self;
long index;
{
    if (self->highlightedItem >= 0) {
	struct chlist *l;
	long len;
	struct listitem *item, *nextItem;

	l = (struct chlist *) chlistview_GetDataObject(self);
	item = chlist_FindItemByIndex(l, self->highlightedItem);
	nextItem = chlist_FindItemByIndex(l, self->highlightedItem + 1);

	len = (self->highlightedItem == chlist_GetNumItems(l) - 1) ? chlist_GetLength(l) : nextItem->loc;
	len -= (item->loc + 1);

	chlistview_SetUpdateRegion(self, item->loc, len);

	self->highlightedItem = -1;
	chlistview_WantUpdate(self, self);
    }
}

struct environment *chlistview__GetStyleInformation(self, sv, pos, length)
struct chlistview *self;
struct text_statevector *sv;
long pos;
long *length;
{
    struct environment *env;
    long index, regionID, size, offset;
    struct style *style;
    struct chlist *l;

    l = (struct chlist *) chlistview_GetDataObject(self);

    env = super_GetStyleInformation(self, sv, pos, length);
    index = chlist_GetIndexByPosition(l, pos, &regionID, &size, &offset);

    if (chlistview_Debug) {
	long len = (length == NULL) ? -1 : *length;

	printf("pos: %d length: %d index: %d regionID: %d size: %d offset: %d\n", pos, len, index, regionID, size, offset);
    }

    if (index >= 0 && regionID >= 0) {
	style = chlistview_GetRegionStyle(self, regionID, FALSE);
	if (style != NULL) {
	    text_ApplyEnvironment(sv, style, NULL);
	}
	if (index == self->highlightedItem) {
	    style = chlistview_GetRegionStyle(self, regionID, TRUE);
	    if (style != NULL) {
		text_ApplyEnvironment(sv, style, NULL);
	    }
	}
	if (length != NULL && (size - offset) < *length) {
	    *length = size - offset;
	}
    }

    return env;
}

struct style *chlistview__GetRegionStyle(self, regionID, highlighted)
struct chlistview *self;
long regionID;
boolean highlighted;
{
    if (self->normalStyles != NULL) {
	if (highlighted) {
	    return self->highlightedStyles[regionID];
	}
	else {
	    return self->normalStyles[regionID];
	}
    }
    return NULL;
}

void chlistview__SetRegionStyles(self, regionID, normalStyle, highlightStyle)
struct chlistview *self;
long regionID;
struct style *normalStyle;
struct style *highlightStyle;
{
    long oldSize;

    if (regionID >= (oldSize = self->numStylesAllocated)) {
	self->numStylesAllocated = regionID + 4;
	if (self->normalStyles == NULL) {
	    self->normalStyles = (struct style **) calloc(self->numStylesAllocated, sizeof(struct style *));
	    self->highlightedStyles = (struct style **) calloc(self->numStylesAllocated, sizeof(struct style *));
	}
	else {
	    self->normalStyles = (struct style **) realloc(self->normalStyles, self->numStylesAllocated * sizeof(struct style *));
	    self->highlightedStyles = (struct style **) realloc(self->highlightedStyles, self->numStylesAllocated * sizeof(struct style *));
	    while (oldSize < self->numStylesAllocated) {
		self->normalStyles[oldSize] = NULL;
		self->highlightedStyles[oldSize] = NULL;
		oldSize++;
	    }
	}
    }

    self->normalStyles[regionID] = normalStyle;
    self->highlightedStyles[regionID] = highlightStyle;
}
	    
		
