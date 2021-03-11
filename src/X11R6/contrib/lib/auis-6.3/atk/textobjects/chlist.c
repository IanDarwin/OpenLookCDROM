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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textobjects/RCS/chlist.c,v 1.4 1993/01/19 18:35:45 Zarf Exp $";
#endif

#include <class.h>
#include <andrewos.h>
#include <chlist.eh>

boolean chlist__InitializeObject(c, self)
struct classheader *c;
struct chlist *self;
{
    self->ItemList = NULL;
    self->numitems = 0;
    self->numitemsallocated = 0;
    self->freeProc = NULL;
    self->numRegions = 1;
    self->numRegionsAllocated = 4;
    self->strRegionNum = 0;
    chlist_SetReadOnly(self, TRUE);
    return(TRUE);
}

void Clear(self)
struct chlist *self;
{
    long i, j;
    struct listitem *item;

    for (i = self->numitems - 1; i >= 0; i--) {
	item = &self->ItemList[i];
	if (self->freeProc != NULL && item->rock != NULL) {
	    (*self->freeProc)(item->rock);
	}
	if (item->regionStrings != NULL) {
	    for (j = 0; j < self->numRegions; j++) {
		if (item->regionStrings[j] != NULL) {
		    free(item->regionStrings[j]);
		}
	    }
	    free(item->regionStrings);
	}
	else if (self->ItemList[i].str != NULL)  {
	    free(self->ItemList[i].str);
	}
    }
    if (self->ItemList != NULL) {
	free(self->ItemList);
    }
}	

void chlist__FinalizeObject(c, self)
struct classheader *c;
struct chlist *self;
{
    Clear(self);
}

void chlist__Clear(self)
struct chlist *self;
{
    Clear(self);
    super_Clear(self);

    self->ItemList = NULL;
    self->numitems = 0;
    self->numitemsallocated = 0;
    self->freeProc = NULL;
    self->numRegions = 1;
    self->numRegionsAllocated = 4;
    self->strRegionNum = 0;
    chlist_SetReadOnly(self, TRUE);
}
    
void chlist__SetRegionStringByIndex(self, index, regionNum, regionStr)
struct chlist *self;
long index;
long regionNum;
char *regionStr;
{
    struct listitem *item;
    long len, oldLen, changeLen = 0;
    long startPos, i;
    char *newstr;

    if (index < 0 || index >= self->numitems) {
	return;
    }
    if (regionNum < 0 || regionNum >= self->numRegions) {
	return;
    }

    item = &self->ItemList[index];

    len = 0;
    newstr = NULL;

    if (regionStr != NULL)  {
	len = strlen(regionStr);
	if (len != 0) {
	    newstr = (char *) malloc(len + 1);
	    strcpy(newstr, regionStr);
	}
    }

    startPos = item->loc;
    if (item->regionStrings != NULL || regionNum != self->strRegionNum) {
	if (item->regionStrings == NULL)  {
	    item->regionStrings = (char **) calloc(self->numRegionsAllocated, sizeof(char *));
	    item->regionStrings[self->strRegionNum] = item->str;
	}

	for (i = 0; i < regionNum; i++) {
	    if (item->regionStrings[i] != NULL) {
		startPos += strlen(item->regionStrings[i]);
	    }
	}

	if (item->regionStrings[regionNum] == NULL) {
	    /* New string */
	    if (len != 0) {
		chlist_AlwaysInsertCharacters(self, startPos, newstr, len);
		changeLen = len;
	    }
	}
	else {
	    oldLen = strlen(item->regionStrings[regionNum]);
	    if (len != 0)  {
		chlist_AlwaysReplaceCharacters(self, startPos, oldLen, newstr, len);
		changeLen = len - oldLen;
	    }
	    else {
		/* Need to check if enivironment goes away */
		chlist_AlwaysDeleteCharacters(self, startPos, oldLen);
		changeLen = -oldLen;
	    }
	    free(item->regionStrings[regionNum]);
	}

	item->regionStrings[regionNum] = newstr;
	if (regionNum == self->strRegionNum) {
	    item->str = newstr;
	}
    }
    else {
	if (item->str != NULL) {
	    oldLen = strlen(item->str);
	    free(item->str);
	}
	else {
	    oldLen = 0;
	}

	if (len != 0) {
	    chlist_AlwaysReplaceCharacters(self, startPos, oldLen, newstr, len);
	    changeLen = len - oldLen;
	}
	else {
	    chlist_AlwaysDeleteCharacters(self, startPos, oldLen);
	    changeLen = -oldLen;
	}
	item->str = newstr;
    }

    if (changeLen != 0) {
	for (i = index + 1; i < self->numitems; i++) {
	    self->ItemList[i].loc += changeLen;
	}
    }
    chlist_NotifyObservers(self, 0);
}

void chlist__SetRegionString(self, str, regionNum, regionStr)
struct chlist *self;
char *str;
long regionNum;
char *regionStr;
{
    chlist_SetRegionStringByIndex(self, chlist_GetIndexByString(self, str), regionNum, regionStr);

}

void chlist__DefineRegion(self, regionNum)
struct chlist *self;
long regionNum;
{
    long i;

    if (regionNum >= self->numRegionsAllocated) {
	/* Need to reallocate region strings */
	self->numRegionsAllocated += 4;

	for (i = 0; i < self->numitems; i++) {
	    if (self->ItemList[i].regionStrings != NULL) {
		self->ItemList[i].regionStrings = (char **) realloc(self->ItemList[i].regionStrings, self->numRegionsAllocated * sizeof(char *));
	    }
	}
    }
    if (regionNum >= self->numRegions) {
	self->numRegions = regionNum + 1;
    }
}

void chlist__DefineStringRegion(self, regionNum)
struct chlist *self;
long regionNum;
{
    long i;

    if (self->strRegionNum != regionNum && regionNum < self->numRegions && regionNum >= 0) {
	for (i = 0; i < self->numitems; i++) {
	    if (self->ItemList[i].regionStrings != NULL) {
		chlist_SetRegionStringByIndex(self, i, regionNum, self->ItemList[i].str);
		chlist_SetRegionStringByIndex(self, i, self->strRegionNum, NULL);
	    }
	    self->ItemList[i].str = self->ItemList[i].regionStrings[regionNum];
	}
	self->strRegionNum = regionNum;
    }
}

boolean chlist__AddItemAtIndex(self, index, str, proc, data)
struct chlist *self;
long index;
char *str;
int (*proc)();
long data;
{
    char *mycopy;
    struct listitem *newbuf;
    int pos, len, i;

    if (self->numitems >= self->numitemsallocated) {
	self->numitemsallocated += 25;
	newbuf = (struct listitem *) malloc(self->numitemsallocated * sizeof(struct listitem));
	if (newbuf == NULL) {
	    return(FALSE);
	}
	if (self->ItemList != NULL) {
	    bcopy(self->ItemList, newbuf, (self->numitemsallocated - 25) * sizeof(struct listitem));
	    free(self->ItemList);
	}
	self->ItemList = newbuf;
    }
    len = strlen(str);
    mycopy = malloc(1+len);
    if (!mycopy) {
	--self->numitems;
	return(FALSE);
    }
    strcpy(mycopy, str);

    if (index >= self->numitems) {
	index = self->numitems;
	pos = chlist_GetLength(self);
    }
    else {
	pos = self->ItemList[index].loc;
	for (i = self->numitems; i > index; i--) {
	    self->ItemList[i] = self->ItemList[i-1];
	    self->ItemList[i].loc += len + 1;
	}
    }

    chlist_AlwaysInsertCharacters(self, pos, str, len);
    chlist_AlwaysInsertCharacters(self, pos+len, "\n", 1);
    self->ItemList[index].str = mycopy;
    self->ItemList[index].proc = proc;
    self->ItemList[index].rock = data;
    self->ItemList[index].loc = pos;
    self->ItemList[index].regionStrings = NULL;
    ++self->numitems;

    chlist_NotifyObservers(self, 0);

    return(TRUE);
}


boolean chlist__AddItemToEnd(self, str, proc, data)
struct chlist *self;
char *str;
int (*proc)();
long data;
{
    return chlist_AddItemAtIndex(self, self->numitems, str, proc, data);
}

struct listitem * chlist__FindItem(self, str)
struct chlist *self;
char *str;
{
    int i;
    i = chlist_GetIndexByString(self,str);
    if (i>= 0) return(&self->ItemList[i]);
    return(NULL);
}

struct listitem * chlist__FindItemByIndex(self, index)
struct chlist *self;
unsigned long index;
{
    if (index < self->numitems) {
        return (&self->ItemList[index]);
    }
    return NULL;
}

procedure chlist__SetFreeProcedure(self, proc)
struct chlist *self;
procedure proc;
{
    procedure oldproc = self->freeProc;

    self->freeProc = proc;

    return(oldproc);
}

long chlist__GetIndexByString(self, str)
struct chlist *self;
char *str;
{
    int i;
    for (i=0; i<self->numitems; ++i) {
	if (!strcmp(self->ItemList[i].str, str)) {
	    return(i);
	}
    }
    return(-1);
}

long chlist__GetIndexByData(self, data)
struct chlist *self;
long data;
{
    int i;
    for (i=0; i<self->numitems; ++i) {
	if (data == self->ItemList[i].rock) {
	    return(i);
	}
    }
    return(-1);
}

boolean chlist__DeleteItemByIndex(self, i)
struct chlist *self;
long i;
{
    int len, j;

    if (i<0 || i >= self->numitems) return(FALSE);
    if ((i+1) < self->numitems) {
	len = self->ItemList[i+1].loc - self->ItemList[i].loc;
    } else {
	len = chlist_GetLength(self) - self->ItemList[i].loc;
    }
    chlist_AlwaysDeleteCharacters(self, self->ItemList[i].loc, len);
    if (self->freeProc != NULL && self->ItemList[i].rock != NULL)  {
        (*self->freeProc)(self->ItemList[i].rock);
    }
    if (self->ItemList[i].regionStrings != NULL) {
	for (j = 0; j < self->numRegions; j++) {
	    if (self->ItemList[i].regionStrings[j] != NULL) {
		free(self->ItemList[i].regionStrings[j]);
	    }
	}
	free(self->ItemList[i].regionStrings);
    }
    else if (self->ItemList[i].str != NULL) {
	free(self->ItemList[i].str);
    }
	
    self->numitems --;
    while (i<self->numitems) {
	self->ItemList[i].str = self->ItemList[i+1].str;
	self->ItemList[i].loc = self->ItemList[i+1].loc - len;
	self->ItemList[i].proc = self->ItemList[i+1].proc;
	self->ItemList[i].rock = self->ItemList[i+1].rock;
	self->ItemList[i].regionStrings = self->ItemList[i+1].regionStrings;
	++i;
    }
    chlist_NotifyObservers(self, 0);
    return(TRUE);
}

boolean chlist__DeleteItem(self, str)
struct chlist *self;
char *str;
{
    int i = chlist_GetIndexByString(self, str);

    return chlist_DeleteItemByIndex(self, i);
}

boolean chlist__ChangeItemByIndex(self, index, newstr)
struct chlist *self;
long index;
char *newstr;
{
    chlist_SetRegionStringByIndex(self, index, self->strRegionNum, newstr);

    return(TRUE);
}

boolean chlist__ChangeItem(self, oldstr, newstr)
struct chlist *self;
char *oldstr, *newstr;
{
    return chlist_ChangeItemByIndex(self, chlist_GetIndexByString(self, oldstr), newstr);
}

boolean chlist__ChangeDataByIndex(self, i, rock)
struct chlist *self;
long i;
long rock;
{
    if (i<0 || i >= self->numitems) return(FALSE);

    if (self->freeProc != NULL && self->ItemList[i].rock != NULL)  {
        (*self->freeProc)(self->ItemList[i].rock);
    }

    self->ItemList[i].rock = rock;

    chlist_NotifyObservers(self, 0);

    return(TRUE);
}

boolean chlist__ChangeData(self, oldstr, rock)
struct chlist *self;
char *oldstr;
long rock;
{
    return chlist_ChangeDataByIndex(self, chlist_GetIndexByString(self, oldstr), rock);
}

long chlist__GetRegionInfoForPosition(self, index, position, size, offset)
struct chlist *self;
long index;
long position;
long *size;
long *offset;
{
    struct listitem *item = &self->ItemList[index];
    long itemPosition = item->loc;
    long len;
    long itemLen, regionSize, regionOffset, regionID;

    if (index < self->numitems - 1) {
	itemLen = self->ItemList[index + 1].loc - item->loc - 1;
    }
    else {
	itemLen = chlist_GetLength(self) - item->loc - 1;
    }

    if (position == item->loc + itemLen) {
	regionSize = 1;
	regionOffset = 0;
	regionID = -1;
    }
    else if (item->regionStrings == NULL) {
	regionID = self->strRegionNum;
	regionSize = itemLen;
	regionOffset = itemPosition - position;
    }
    else {

	long i;

	regionID = -1;

	for (i = 0; i < self->numRegions; i++) {
	    len = (item->regionStrings[i] != NULL) ? strlen(item->regionStrings[i]) : 0;
	    if (itemPosition + len > position) {
		regionID = i;
		regionSize = len;
		regionOffset = position - itemPosition;
		break;
	    }
	    else {
		itemPosition += len;
	    }
	}
    }
    if (size != NULL) {
	*size = regionSize;
    }
    if (offset != NULL) {
	*offset = regionOffset;
    }

    return regionID;
}

long chlist__GetIndexByPosition(self, position, regionID, size, offset)
struct chlist *self;
long position;
long *regionID;
long *size;
long *offset;
{
    long min = 0;
    long max = self->numitems - 1;
    long split;

    while (max > min) {
	split = min + (max-min)/2;
	if (self->ItemList[split].loc > position) {
	    max = split - 1;
	} else {
	    if (min == split) {
		if (self->ItemList[max].loc <= position) {
		    min = max;
		}
		break;
	    }
	    min = split;
	}
    }
    if (min < self->numitems) {
	/* found an item */
	long region = chlist_GetRegionInfoForPosition(self, min, position, size, offset);

	if (regionID != NULL) {
	    *regionID = region;
	}

	return min;
    }
    return -1;
}

void chlist__EnumerateItems(self, index, length, proc, rock)
struct chlist *self;
long index;
long length;
procedure proc;
long rock;
{
    long endIndex = index + length;

    if (endIndex > self->numitems) {
	endIndex = self->numitems;
    }

    while (index != endIndex) {
	if (! (*proc)(self, &self->ItemList[index], index, rock)) {
	    return;
	}
	index++;
    }
}

