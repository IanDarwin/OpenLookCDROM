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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvaltvc.c,v 1.4 1993/12/09 00:09:25 gk5g Exp $";
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
#include "pvaltvc.eh"

#define DATA(self) ((struct prefval *)pvaltvc_GetDataObject(self))
#define TEXT(self) (pvaltvc_GetText(self))

static struct menulist *pvaltvcMenus=NULL;
static struct keymap *pvaltvcKeymap=NULL;

static void pvaltvc__Select(self, ind)
struct pvaltvc *self;
int ind;
{
    int i;
    int max=prefval_GetListMax(DATA(self));
    int size=prefval_GetListSize(DATA(self));

    if(ind<prefval_GetChoiceListSize(DATA(self))) {
	for(i=MIN(size,max-1)-1;i>=0;i--) {
	    struct prefval_value *v= prefval_GetIndexValue(DATA(self), i);
	    prefval_SetIndexValue(DATA(self), i+1, v);
	    prefval_FreeValue(DATA(self), prefval_GetIndexValue(DATA(self), i));
	}
	prefval_SetIndexValue(DATA(self), 0, &DATA(self)->cvalues[prefval_GetChoiceListSize(DATA(self)) - ind - 1]);
	prefval_NotifyObservers(DATA(self), prefval_ValuesChanged);
    }
}


static void pvaltvcUpdate(self, rock)
struct pvaltvc *self;
long rock;
{
    pvaltvc_Select(self, pvaltvc_Locate(self, textview_GetDotPosition(pvaltvc_GetTextView(self))));
}

static struct bind_Description pvaltvcBindings[]={
    {
	"pvaltvc-update",
	" ",
	' ',
	NULL,
	0,
	0,
	pvaltvcUpdate,
	"sets the preference value from the text in the entry buffer.",
	"pvaltvc"
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
    
    
boolean pvaltvc__InitializeClass(classID)
struct classheader *classID;
{
    pvaltvcMenus=menulist_New();
    pvaltvcKeymap=keymap_New();

    if(pvaltvcMenus == NULL || pvaltvcKeymap==NULL) {
	if(pvaltvcMenus != NULL) menulist_Destroy(pvaltvcMenus);
	if(pvaltvcKeymap != NULL) keymap_Destroy(pvaltvcKeymap);
	return FALSE;
    }
    
    bind_BindList(pvaltvcBindings, pvaltvcKeymap, pvaltvcMenus, &pvaltvc_classinfo);
    return TRUE;
    
    
}

boolean pvaltvc__InitializeObject(classID, self)
struct classheader *classID;
struct pvaltvc *self;
{
    self->ks=keystate_Create(self, pvaltvcKeymap);
    if(self->ks==NULL) return FALSE;

    self->menulist=menulist_DuplicateML(pvaltvcMenus, self);

    if(self->menulist==NULL) {
	keystate_Destroy(self->ks);
	return FALSE;
    }
    
    pvaltvc_SetInterfaceView(self, textview_GetApplicationLayer( pvaltvc_GetTextView(self)));
    
    return TRUE;
}

void pvaltvc__FinalizeObject(classID, self)
struct classheader *classID;
struct pvaltvc *self;
{
    if(self->ks) keystate_Destroy(self->ks);
    if(self->menulist) menulist_Destroy(self->menulist);
}

void pvaltvc__UpdateText(self, val)
struct pvaltvc *self;
long val;
{
    struct prefval *pvd=DATA(self);
    struct text *pvt=TEXT(self);
    struct textview *tv=pvaltvc_GetTextView(self);
    char *vs;
    int i;
    long pos=textview_GetDotPosition(tv);
    long len=textview_GetDotLength(tv);
    
    if(val==prefval_ChoicesChanged || val==prefval_Generic) {
	text_Clear(pvt);
	for(i=prefval_GetChoiceListSize(pvd)-1;i>=0;i--) {
	    if(prefval_GetIndexChoiceName(pvd, i)) text_InsertCharacters(pvt, text_GetLength(pvt), prefval_GetIndexChoiceName(pvd, i), strlen(prefval_GetIndexChoiceName(pvd, i)));
	    if(i>0) text_InsertCharacters(pvt, text_GetLength(pvt), "\n", 1);
	}
	if(pos+len<=text_GetLength(pvt)) {
	    textview_SetDotPosition(tv, pos);
	    textview_SetDotLength(tv, len);
	    textview_FrameDot(tv, pos+len);
	    textview_WantUpdate(tv, tv);
	}

    }
}

void pvaltvc__UpdateValue(self)
struct pvaltvc *self;
{
}

 
void pvaltvc__ObservedChanged(self, changed, val)
struct pvaltvc *self;
struct prefval *changed;
long val;
{
    super_ObservedChanged(self, changed, val);  
}


struct keystate *pvaltvc__Keys(self)
struct pvaltvc *self;
{
    return self->ks;
}
	
void pvaltvc__SetDataObject(self, d)
struct pvaltvc *self;
struct prefval *d;
{
    super_SetDataObject(self, d);
    pvaltvc_ObservedChanged(self, d, prefval_Generic);
    text_AddObserver(TEXT(self), self);
}

struct view *pvaltvc__Hit(self, action, x, y, numberOfClicks)
struct pvaltvc *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    struct view *result = super_Hit(self, action, x, y, numberOfClicks);
    struct textview *tv=pvaltvc_GetTextView(self);

    if(action==view_LeftUp) {
	pvaltvc_Select(self, pvaltvc_Locate(self, textview_GetDotPosition(tv)));
    }
    return (result!=(struct view *)tv)?result:(struct view *)self;

}

