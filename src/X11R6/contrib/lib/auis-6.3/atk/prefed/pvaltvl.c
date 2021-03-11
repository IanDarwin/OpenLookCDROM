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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvaltvl.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <math.h>
#include <class.h>

#include "pvaltvl.eh"

#include <prefval.ih>

#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <observe.ih>
#include <text.ih>
#include <textv.ih>

#define DATA(self) ((struct prefval *)pvaltvl_GetDataObject(self))
#define TEXT(self) (pvaltvl_GetText(self))

static struct menulist *pvaltvlMenus=NULL;
static struct keymap *pvaltvlKeymap=NULL;


static void pvaltvlUpdate(self, rock)
struct pvaltvl *self;
long rock;
{
}

static struct bind_Description pvaltvlBindings[]={
    {
	"pvaltvl-update",
	"\r",
	'\r',
	NULL,
	0,
	0,
	pvaltvlUpdate,
	"sets the preference value from the text in the entry buffer.",
	"pvaltvl"
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
    
    
boolean pvaltvl__InitializeClass(classID)
struct classheader *classID;
{
    pvaltvlMenus=menulist_New();
    pvaltvlKeymap=keymap_New();

  /*  if(pvaltvlMenus == NULL || pvaltvlKeymap==NULL) {
	if(pvaltvlMenus != NULL) menulist_Destroy(pvaltvlMenus);
	if(pvaltvlKeymap != NULL) keymap_Destroy(pvaltvlKeymap);
	return FALSE;
    }
    
    bind_BindList(pvaltvlBindings, pvaltvlKeymap, pvaltvlMenus, &pvaltvl_classinfo); */
    return TRUE;
    
    
}
boolean pvaltvl__InitializeObject(classID, self)
struct classheader *classID;
struct pvaltvl *self;
{
    self->ks=keystate_Create(self, pvaltvlKeymap);
    if(self->ks==NULL) return FALSE;

    self->menulist=menulist_DuplicateML(pvaltvlMenus, self);

    if(self->menulist==NULL) {
	keystate_Destroy(self->ks);
	return FALSE;
    }
    
    return TRUE;
}

void pvaltvl__FinalizeObject(classID, self)
struct classheader *classID;
struct pvaltvl *self;
{
    if(self->ks) keystate_Destroy(self->ks);
    if(self->menulist) menulist_Destroy(self->menulist);
}

void pvaltvl__SetDotPosition(self, pos)
struct pvaltvl *self;
long pos;
{
    struct text *t=pvaltvl_GetText(self);
    int count=pvaltvl_Locate(self, pos);
    if(count<prefval_GetListSize(DATA(self))) {
	prefval_SetCurrentItem(DATA(self), prefval_GetListSize(DATA(self)) - count - 1);
    }
    super_SetDotPosition(self, pos);
}

void pvaltvl__UpdateText(self, val)
struct pvaltvl *self;
long val;
{
    struct prefval *pvd=DATA(self);
    struct text *pvt=TEXT(self);
    struct textview *tv=pvaltvl_GetTextView(self);
    char *vs;
    int i;
    if(val==prefval_ValuesChanged || val==prefval_Generic) {
	long pos=textview_GetDotPosition(tv);
	long len=textview_GetDotLength(tv);
	text_Clear(pvt);
	for(i=prefval_GetListSize(pvd)-1;i>=0;i--) {
	    vs=prefval_IndexValueString(pvd, i);
	    if(vs) text_InsertCharacters(pvt, text_GetLength(pvt), vs, strlen(vs));
	    if(i>0) text_InsertCharacters(pvt, text_GetLength(pvt), "\n", 1);
	}
	textview_SetDotPosition(tv, pos);
	textview_SetDotLength(tv, len);
	textview_FrameDot(tv, pos+len);
    }
}

void pvaltvl__UpdateValue(self)
struct pvaltvl *self;
{
    struct prefval *pvd=DATA(self);
    struct text *pvt=TEXT(self);
    struct prefval_value *v;
    struct textview *tv=pvaltvl_GetTextView(self);
    long lines=0, i;
    long tpos=textview_GetTopPosition(tv);
    long dpos=textview_GetDotPosition(tv);
    long pos=0, npos;
    struct prefval_value *pvl;
    
    do {
	long len;
	npos=text_Index(pvt, pos, '\n', text_GetLength(pvt)-pos);
	if(npos>=0) len=npos-pos;
	else len=text_GetLength(pvt)-pos;
	lines++;
	pos=npos+1;
    } while(npos>=0);

    if(lines==0) return;

    if(lines>prefval_GetListMax(pvd)) lines=prefval_GetListMax(pvd);

    pvl=(struct prefval_value *)malloc(sizeof(*pvl)*lines);
    if(pvl==NULL) return;
    
    for(i=0;i<lines;i++) prefval_InitValue(pvd, pvl+i);

    i=lines-1;
    do {
	long len;
	char buf[1024];
	npos=text_Index(pvt, pos, '\n', text_GetLength(pvt)-pos);
	if(npos>=0) len=npos-pos;
	else len=text_GetLength(pvt)-pos;
	if(len>0) {
	    text_CopySubString(pvt, pos, len, buf, FALSE);
	    v=prefval_StringToValue(pvd, buf);
	    prefval_CopyValue(pvd, pvl+i, v);
	} else pvl[i].set=FALSE;
	i--;
	pos=npos+1;
    } while(npos>=0 && i>=0);
    prefval_SetValues(pvd, lines, pvl);
    for(i=0;i<lines;i++) {
	prefval_FreeValue(pvd, pvl+i);
    }
    free(pvl);
    prefval_NotifyObservers(pvd, prefval_ValuesChanged);
    textview_SetTopPosition(tv, tpos);
    textview_SetDotPosition(tv, dpos);
    textview_FrameDot(tv, dpos);
}

 
void pvaltvl__ObservedChanged(self, changed, val)
struct pvaltvl *self;
struct prefval *changed;
long val;
{

    super_ObservedChanged(self, changed, val);
    
    if(val==observable_OBJECTDESTROYED) return;

    if((struct text *)changed==TEXT(self)) pvaltvl_UpdateValue(self);
}


struct keystate *pvaltvl__Keys(self)
struct pvaltvl *self;
{
    return NULL;
}
	
void pvaltvl__SetDataObject(self, d)
struct pvaltvl *self;
struct prefval *d;
{
    super_SetDataObject(self, d);
    pvaltvl_ObservedChanged(self, d, prefval_Generic);
    text_AddObserver(TEXT(self), self);
    prefval_SetCurrentItem(DATA(self), prefval_GetListSize(DATA(self))-1);    
    if(prefval_GetListMax(DATA(self))>1) pvaltvl_SetInterfaceView(self, textview_GetApplicationLayer( pvaltvl_GetTextView(self)));
}
