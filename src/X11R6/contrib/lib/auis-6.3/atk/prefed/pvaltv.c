/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvaltv.c,v 1.11 1993/12/09 00:09:25 gk5g Exp $";
#endif


#include <andrewos.h>
#include <math.h>
#include <class.h>
#include <prefval.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <observe.ih>
#include <text.ih>
#include <textv.ih>
#include "pvaltv.eh"

#define DATA(self) ((struct prefval *)pvaltv_GetDataObject(self))
#define TEXT(self) (pvaltv_GetText(self))

static struct menulist *pvaltvMenus=NULL;
static struct keymap *pvaltvKeymap=NULL;

static void pvaltvUpdate(self, rock)
struct pvaltv *self;
long rock;
{
    pvaltv_UpdateValue(self);
}

static struct bind_Description pvaltvBindings[]={
    {
	"pvaltv-update",
	"\r",
	13,
	NULL,
	0,
	0,
	pvaltvUpdate,
	"sets the preference value from the text in the entry buffer.",
	"pvaltv"
    },
    {
	NULL,
	NULL,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL
    }
};
    
    
boolean pvaltv__InitializeClass(classID)
struct classheader *classID;
{
    pvaltvMenus=menulist_New();
    pvaltvKeymap=keymap_New();

    if(pvaltvMenus == NULL || pvaltvKeymap==NULL) {
	if(pvaltvMenus != NULL) menulist_Destroy(pvaltvMenus);
	if(pvaltvKeymap != NULL) keymap_Destroy(pvaltvKeymap);
	return FALSE;
    }
    
    bind_BindList(pvaltvBindings, pvaltvKeymap, pvaltvMenus, &pvaltv_classinfo);
    return TRUE;
    
    
}
boolean pvaltv__InitializeObject(classID, self)
struct classheader *classID;
struct pvaltv *self;
{
    self->ks=keystate_Create(self, pvaltvKeymap);
    if(self->ks==NULL) return FALSE;

    self->menulist=menulist_DuplicateML(pvaltvMenus, self);

    if(self->menulist==NULL) {
	keystate_Destroy(self->ks);
	return FALSE;
    }
    return TRUE;
}

void pvaltv__FinalizeObject(classID, self)
struct classheader *classID;
struct pvaltv *self;
{
    if(self->ks) keystate_Destroy(self->ks);
    if(self->menulist) menulist_Destroy(self->menulist);
}

void pvaltv__UpdateText(self, val)
struct pvaltv *self;
long val;
{
    struct prefval *pvd=DATA(self);
    struct text *pvt=TEXT(self);
    struct textview *tv=pvaltv_GetTextView(self);
    char *vs;
    long pos=textview_GetDotPosition(tv);

    if(val==prefval_ValuesChanged || val==prefval_Generic) {
	vs=prefval_PreferenceString(pvd);
	if(vs==NULL) return;
	vs=index(vs, ':');
	if(vs==NULL) return;
	vs++;
	if(*vs==' ') vs++;
	text_Clear(pvt);
	text_InsertCharacters(pvt, 0, vs, strlen(vs));
	textview_SetDotPosition(tv, pos);
	textview_FrameDot(tv, pos);
    }
}

void pvaltv__UpdateValue(self)
struct pvaltv *self;
{
    struct text *pvt=TEXT(self);
    int i;
    long len=text_GetLength(pvt);
    int max=prefval_GetListMax(DATA(self));
    int size=prefval_GetListSize(DATA(self));
    char buf[1024];
    text_CopySubString(pvt, 0, len>sizeof(buf)-1?sizeof(buf)-1:len, buf, FALSE);
    prefval_SetFromPreferenceString(DATA(self), buf);
    prefval_NotifyObservers(DATA(self), prefval_ValuesChanged);
}

void pvaltv__ObservedChanged(self, changed, val)
struct pvaltv *self;
struct prefval *changed;
long val;
{
    if(changed==DATA(self) && val!=observable_OBJECTDESTROYED) {
	pvaltv_UpdateText(self, val);
    }
    super_ObservedChanged(self, changed, val);
}

void pvaltv__SetDataObject(self, d)
struct pvaltv *self;
struct prefval *d;
{
    super_SetDataObject(self, d);
    pvaltv_ObservedChanged(self, d, prefval_Generic);
}

struct keystate *pvaltv__Keys(self)
struct pvaltv *self;
{
    return self->ks;
}

struct menulist *pvaltv__Menus(self)
struct pvaltv *self;
{
    menulist_ClearChain(self->menulist);
    return self->menulist;
}

int pvaltv__Locate(self, pos)
struct pvaltv *self;
long pos;
{
    struct text *t=pvaltv_GetText(self);
    long len=text_GetLength(t);
    long i=0;
    int count=0;
    do {
	long j;
	j=text_Index(t, i, '\n', len-i);
	if(j<0 || (pos>=i && pos<=j)) break;
	count++;
	i=j+1;
    } while(1);
    return count;
}
