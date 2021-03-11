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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/pagev.c,v 1.10 1994/04/18 02:43:58 rr2b Exp $";
#endif


 

/* This code is taken , in part, from the switcher inset of N. Borenstein's
Andrew Toolkit Book . It has been modified and used with the permission
of the author */

#include <pagev.eh>
#include <dataobj.ih>
#include <view.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <message.ih>
#include <complete.ih>
#include <filetype.ih>
#include <page.ih>
#include <im.ih>
#include <dataobj.ih>


static void AddSwitchee(), NextSwitchee(),
  SwitchObject(), AddSwitcheeFromFile(),
  PasteSwitchee(),SetCurrentView();

static struct keymap *pagev_keymap = NULL;
static struct menulist *pagev_menulist = NULL;
static struct proctable_Entry *switchobjproc = NULL;

boolean pagev__InitializeClass(c)
struct classheader *c;
{
    struct proctable_Entry *proc = NULL;

    pagev_keymap = keymap_New();
    pagev_menulist = menulist_New();

    proc = proctable_DefineProc("pagev-next-page",
	NextSwitchee, &pagev_classinfo, NULL,
	"Changes the page to look at the next object.");
    keymap_BindToKey(pagev_keymap, "^X^N", proc, 0);
    menulist_AddToML(pagev_menulist,
	"Flip~95,Flip Page~90", proc, NULL, 0);
/*
    proc = proctable_DefineProc("pagev-add-page",
	AddSwitchee, &pagev_classinfo, NULL,
	"Adds a new thing for the page to switch to.");
    keymap_BindToKey(pagev_keymap, "^X^A", proc, 0);
    menulist_AddToML(pagev_menulist,
	"page~95,Add Page~91", proc, NULL, 0);

    proc = proctable_DefineProc("pagev-add-from-file",
	AddSwitcheeFromFile, &pagev_classinfo, NULL,
	"Adds the contents of a file as a new switchee.");
    keymap_BindToKey(pagev_keymap, "^X5", proc, 0);
    menulist_AddToML(pagev_menulist,
	"page~95,Insert File~92", proc, NULL, 0);
*/
    proc = proctable_DefineProc("pagev-paste",
	PasteSwitchee, &pagev_classinfo, NULL,
	"Pastes a switchee from the cut-buffer");
    keymap_BindToKey(pagev_keymap, "^X5", proc, 0);
    menulist_AddToML(pagev_menulist,
	"Flip~95,Paste~80", proc, NULL, 0);

    proctable_DefineProc("pagev-SetCurrentView",
	SetCurrentView, &pagev_classinfo, NULL,
	"Takes a string argument and calls page_SetNowPlayingByName");

    switchobjproc = proctable_DefineProc(
	"pagev-switch-object", SwitchObject,
	&pagev_classinfo, NULL,
	"Switches to a given object.");
    return(TRUE);

}
static void SetCurrentView(self,name)
struct pagev *self;
char *name;
{
    struct page *page = (struct page *)
      pagev_GetDataObject(self);
    page_SetNowPlayingByName(page,name);
}

boolean pagev__InitializeObject(c, self)
struct classheader *c;
struct pagev *self;
{
    self->ks = keystate_Create(self, pagev_keymap);
    self->ml = menulist_DuplicateML(pagev_menulist,
				     self);
    if (!self->ks || !self->ml) return(FALSE);
    self->Firstswitcheroo = NULL;
    self->NowPlaying = NULL;
    return(TRUE);
}

void pagev__FinalizeObject(c, self)
struct classheader *c;
struct pagev *self;
{
    struct pagev_switcheroo *this, *next;

    if (self->ks) {
	keystate_Destroy(self->ks);
	self->ks = NULL;
    }
    if (self->ml) {
	menulist_Destroy(self->ml);
	self->ml = NULL;
    }
    this = self->Firstswitcheroo;
    while (this) {
	view_RemoveObserver(this->v, self);
	if(view_GetDataObject(this->v)) dataobject_RemoveObserver(view_GetDataObject(this->v), self);
	view_Destroy(this->v);
	next = this->next;
	free(this);
	this = next;
    }
}

static boolean CheckRightSwitchee(self, NeedFullRedraw,cp)
struct pagev *self;
boolean *NeedFullRedraw;
struct page_switchee *cp;
{
    struct page *page = (struct page *)
      pagev_GetDataObject(self);
    struct pagev_switcheroo *swtmp;
    boolean pagein;

    if(cp == NULL){
      cp = page->NowPlaying;
      pagein = TRUE;
    }
    else pagein = FALSE;
    *NeedFullRedraw = FALSE;
    if (!cp) {
	self->NowPlaying = NULL;
	*NeedFullRedraw = TRUE;
	return(TRUE);
    }
    if (self->NowPlaying) {
	if (self->NowPlaying->switchee == cp) {
	    return(TRUE);
	}
	for (swtmp = self->Firstswitcheroo;
	     swtmp != NULL;
	     swtmp = swtmp->next) {
	    if (swtmp->switchee == cp) {
		if (self->NowPlaying && pagein) {
		    /* notify of removal */
		    view_UnlinkTree(self->NowPlaying->v);  
		    /* relink so that name stays on arbiterview list */
		    view_LinkTree(self->NowPlaying->v, self);  
		}
		self->NowPlaying = swtmp;
		*NeedFullRedraw = TRUE;
		if(pagein) view_WantInputFocus(self->NowPlaying->v,
				    self->NowPlaying->v);
		return(TRUE);
	    }
	}
    }
    swtmp = (struct pagev_switcheroo *)
      malloc(sizeof(struct pagev_switcheroo));
    if (swtmp) {
	char *cpv=cp->viewname;
	if(class_IsTypeByName(class_GetTypeName(cp->d), "unknown")) {
	    if(cpv==NULL || !class_IsTypeByName(cpv, "unknownv")) cpv="unknownv";
	}
	swtmp->v = (struct view *)
	  class_NewObject(cpv);
	if (swtmp->v) {
	    view_SetDataObject(swtmp->v,
			cp->d);
	    swtmp->next = self->Firstswitcheroo;
	    swtmp->switchee = cp;
	    self->Firstswitcheroo = swtmp;
	    if (self->NowPlaying && pagein) {
		/* notify of removal */
		view_UnlinkTree(self->NowPlaying->v);  
		/* relink so that name stays on arbiterview list */
		view_LinkTree(self->NowPlaying->v, self);  
	    }
	    self->NowPlaying = swtmp;
	    view_LinkTree(self->NowPlaying->v, self);
	    *NeedFullRedraw = TRUE;
	    if(pagein) view_WantInputFocus(self->NowPlaying->v,
				self->NowPlaying->v);
	    view_AddObserver(self->NowPlaying->v,self);
	    return(TRUE);
	}
    }
    return(FALSE);
}

void pagev__FullUpdate(self, type, left, top, width, height)
struct pagev *self;
enum view_UpdateType type;
long left;
long top;
long width;
long height;
{
    struct rectangle Rect;
    boolean NeedFull; /* ignored */

    if (!CheckRightSwitchee(self, &NeedFull, NULL)) return;
    pagev_GetVisualBounds(self, &Rect);
    if (!self->NowPlaying) {
	pagev_MoveTo(self, (Rect.left + Rect.width) / 2,
			    (Rect.top + Rect.height) / 2);
	pagev_DrawString(self, "<No objects>",
	    graphic_BETWEENLEFTANDRIGHT
	    | graphic_BETWEENTOPANDBASELINE);
	return;
    }
    view_InsertView(self->NowPlaying->v, self, &Rect);
    view_FullUpdate(self->NowPlaying->v, type, left,
		     top, width, height);
}

void pagev__Update(self)
struct pagev *self;
{
    boolean NeedFullRedraw;

    if (!CheckRightSwitchee(self, &NeedFullRedraw, NULL)) return;
    if (NeedFullRedraw) {
	struct rectangle Rect;
	pagev_GetVisualBounds(self, &Rect);
	pagev_EraseRect(self, &Rect);
	pagev_FullUpdate(self, view_FullRedraw,
				Rect.left, Rect.top,
				Rect.width, Rect.height);
	return;
    }
    if (self->NowPlaying) view_Update(self->NowPlaying->v);
}

struct view *pagev__Hit(self, action, x, y, numberOfClicks)
struct pagev *self;
enum view_MouseAction action;
long	x;
long	y;
long	numberOfClicks;
{
    if (!self->NowPlaying) {
	pagev_WantInputFocus(self, self);
	return((struct view *) self);
    }
    return(view_Hit(self->NowPlaying->v, action,
		     x, y, numberOfClicks));
}

void pagev__PostKeyState(self, ks)
struct pagev *self;
struct keystate *ks;
{
    self->ks->next = NULL;
    keystate_AddBefore(self->ks, ks);
    super_PostKeyState(self, self->ks);
}

void pagev__PostMenus(self, ml)
struct pagev *self;
struct menulist *ml;
{
    struct page_switchee *sw;
    struct page *page = (struct page *)
      pagev_GetDataObject(self);
    char MenuBuf[200];
    if(page_GetPostMenus(page) == FALSE){
	super_PostMenus(self,ml);
	return;
    }
    menulist_ClearChain(self->ml);
    for (sw = page->FirstSwitchee; sw; sw = sw->next) {
	sprintf(MenuBuf, "Flip~95,%s", page_GetSwitcheeName(page,sw));
	if (self->NowPlaying && (page->NowPlaying == sw)) {
	    menulist_DeleteFromML(self->ml, MenuBuf);
	} else {
	    menulist_AddToML(self->ml, MenuBuf,
			     switchobjproc, (long) sw, 0);
	}
    }
    if (ml) menulist_ChainBeforeML(self->ml, ml, ml);
    super_PostMenus(self, self->ml);
}

void pagev__LinkTree(self, parent)
struct pagev *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if (self->NowPlaying) {
	view_LinkTree(self->NowPlaying->v, self);
    }
}

void pagev__WantInputFocus(self, v)
struct pagev *self;
struct view *v;
{
    if (self->NowPlaying && (v == (struct view *) self)) {
	v = self->NowPlaying->v;
    }
    super_WantInputFocus(self, v);
}

static void AddSwitchee(self)
struct pagev *self;
{
    char ObjName[150], ViewName[150], Label[150];
    struct dataobject *d;
    struct page *sw;

    if (message_AskForString(self, 10, "Object to insert: ", 
			      NULL, ObjName,
			      sizeof(ObjName)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    d = (struct dataobject *) class_NewObject(ObjName);
    if (d == NULL) {
	message_DisplayString(self, 10,
			"Could not create new object; sorry.");
	return;
    }
    if (message_AskForString(self, 10, "View to use: ",
		dataobject_ViewName(d), ViewName,
		sizeof(ViewName)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	dataobject_Destroy(d);
	return;
    }
    if (message_AskForString(self, 10,
			      "Label for this object: ",
			      NULL, Label, sizeof(Label)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	dataobject_Destroy(d);
	return;
    }
    sw = (struct page *) pagev_GetDataObject(self);
    if (!page_AddObject(sw, d, Label, ViewName,page_AFTERCURRENT)) {
	message_DisplayString(self, 10,
		"Object creation failed; sorry!");
	return;
    }
    page_SetNowPlaying(sw, d);
 /*   NextSwitchee(self);
    page_NotifyObservers(sw, observable_OBJECTCHANGED); */
}

static void NextSwitchee(self)
struct pagev *self;
{
    struct page *page = (struct page *)
      pagev_GetDataObject(self);
    page_SetNowPlayingByPosition(page,page_AFTERCURRENT);
}

static void SwitchObject(self, swin)
struct pagev *self;
struct page_switchee *swin; /* really a long */
{
    struct page *page = (struct page *)
      pagev_GetDataObject(self);
    struct page_switchee *sw;

    for (sw= page->FirstSwitchee; sw; sw = sw->next) {
	if (sw == swin) {
	    page_SetNowPlaying(page, sw->d);
	    return;
	}
    }
    message_DisplayString(self, 10,
	"SwitchObject called for nonexistent object.");
}

static void AddSwitcheeFromFile(self)
struct pagev *self;
{
    char FileName[150], ViewName[150], Label[150], *ObjName;
    struct dataobject *d;
    FILE *fp;
    long ID;
    struct page *sw;

    if (completion_GetFilename(self,
		"File to insert as new switchee: ",
		NULL, FileName, sizeof(FileName),
		FALSE, TRUE) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    fp = fopen(FileName, "r");
    if (!fp) {
	message_DisplayString(self, 10, "Could not open file.");
	return;
    }
    ObjName = filetype_Lookup(fp, FileName, &ID, NULL);
    d = (struct dataobject *) class_NewObject(ObjName);
    if (!d) {
	fclose(fp);
	message_DisplayString(self, 10,
		"Could not create new object; sorry.");
	return;
    }
    if (dataobject_Read(d, fp, ID) != dataobject_NOREADERROR) {
	fclose(fp);
	message_DisplayString(self, 10,
		"Read operation failed; sorry.");
	return;
    }
    fclose(fp);
    if (message_AskForString(self, 10,
		"View to use: ", dataobject_ViewName(d),
		ViewName, sizeof(ViewName)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	dataobject_Destroy(d);
	return;
    }
    if (message_AskForString(self, 10,
		"Label for this object: ", NULL, Label,
		sizeof(Label)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	dataobject_Destroy(d);
	return;
    }
    sw = (struct page *) pagev_GetDataObject(self);
    if (!page_AddObject(sw, d, Label, ViewName,page_AFTERCURRENT)) {
	message_DisplayString(self, 10,
		"Object creation failed; sorry!");
	return;
    }
    page_SetNowPlaying(sw, d);
}
static void PasteSwitchee(self)
struct pagev *self;
{
    char FileName[150], ViewName[150], Label[150], *ObjName;
    struct dataobject *d;
    FILE *fp;
    long ID;
    struct page *sw;

    fp = im_FromCutBuffer(pagev_GetIM(self));

    if (!fp) {
	message_DisplayString(self, 10, "Could not open file.");
	return;
    }
    ObjName = filetype_Lookup(fp, FileName, &ID, NULL);
    d = (struct dataobject *) class_NewObject(ObjName);
    if (!d) {
	im_CloseFromCutBuffer(pagev_GetIM(self), fp);
	message_DisplayString(self, 10,
		"Could not create new object; sorry.");
	return;
    }
    if (dataobject_Read(d, fp, ID) != dataobject_NOREADERROR) {
	im_CloseFromCutBuffer(pagev_GetIM(self), fp);
	message_DisplayString(self, 10,
		"Read operation failed; sorry.");
	return;
    }
    im_CloseFromCutBuffer(pagev_GetIM(self), fp);
    strcpy(ViewName,dataobject_ViewName(d));
    if(class_IsTypeByName(class_GetTypeName(d),"cel")) *Label = '\0';
    else if (message_AskForString(self, 10,
		"Label for this object: ", NULL, Label,
		sizeof(Label)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	dataobject_Destroy(d);
	return;
    }
    sw = (struct page *) pagev_GetDataObject(self);
    if (!page_AddObject(sw, d, Label, ViewName,page_AFTERCURRENT)) {
	message_DisplayString(self, 10,
		"Object creation failed; sorry!");
	return;
    }
    page_SetNowPlaying(sw, d);
}

enum view_DSattributes pagev__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct pagev *self;
long width;
long height;
enum view_DSpass pass;
long *dWidth;
long *dHeight;
{
    if(self->NowPlaying && self->NowPlaying->v){
	return view_DesiredSize(self->NowPlaying->v, width  , height  , pass, dWidth, dHeight);
    }
    return super_DesiredSize(self, width, height, pass, dWidth, dHeight);
}

void pagev__InitChildren(self)
struct pagev *self;
{
    struct page_switchee *sw;
    struct pagev_switcheroo *safe;
    int NeedFullRedraw;
    struct page *page = (struct page *)
      pagev_GetDataObject(self);

    safe = self->NowPlaying;
    for (sw = page->FirstSwitchee; sw; sw = sw->next) {
	CheckRightSwitchee(self, &NeedFullRedraw, sw);
/*	if(self->NowPlaying != safe) 
	    view_UnlinkTree(self->NowPlaying->v); */
    }
    self->NowPlaying = safe;
}

void pagev__Print(self, file, processor, finalFormat, topLevel)
struct pagev *self;
FILE *file;
char *processor;
char *finalFormat;
boolean topLevel;
{
    struct page_switchee *sw;
    struct pagev_switcheroo *safe;
    int NeedFullRedraw;
    struct page *page = (struct page *)
      pagev_GetDataObject(self);

    safe = self->NowPlaying;
    for (sw = page->FirstSwitchee; sw; sw = sw->next) {
	if(safe != NULL && safe->switchee == sw){
	    view_Print(safe->v,file, processor, finalFormat, topLevel);
	    if(strcmp(processor,"troff") == 0  && (sw->next != NULL)) fprintf(file,".bp\n");
	    continue;
	}
	CheckRightSwitchee(self, &NeedFullRedraw, sw);
	if(self->NowPlaying && self->NowPlaying->v){
	    view_Print(self->NowPlaying->v,file, processor, finalFormat, topLevel);
	    if(strcmp(processor,"troff") == 0 && (sw->next != NULL)) 
		fprintf(file,".bp\n");
	}
/*	view_UnlinkTree(self->NowPlaying->v); */
    }
    self->NowPlaying = safe;
}
void pagev__ObservedChanged(self, changed, value)
struct pagev *self;
struct observable *changed;
long value;
{
    struct pagev_switcheroo *swtmp,*last;

    if(value == observable_OBJECTDESTROYED){
	if(changed == (struct observable *) pagev_GetDataObject(self)) 
	{
	    pagev_UnlinkTree(self);
	    pagev_Destroy(self);
	    return;
	}
	for (swtmp = self->Firstswitcheroo;
	     swtmp != NULL;
	     swtmp = swtmp->next) {
	    if (swtmp->v == (struct view *) changed) {
		if(self->Firstswitcheroo == swtmp)
		    self->Firstswitcheroo = swtmp->next;
		else
		    last->next = swtmp->next;
		if (self->NowPlaying == swtmp) {
		    self->NowPlaying = NULL;
		}
		page_DeleteObject((struct page *) pagev_GetDataObject(self),swtmp->switchee->d);
		free(swtmp);
		pagev_PostMenus(self, NULL);
		return;
	    }
	    last = swtmp;
	}

    }
    super_ObservedChanged(self, changed, value);
}
