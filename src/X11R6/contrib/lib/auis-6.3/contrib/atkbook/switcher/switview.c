static char *switview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/switcher/RCS/switview.c,v 1.1 1992/10/06 22:21:29 susan R6tape $";

/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
#include <switview.eh>
#include <dataobj.ih>
#include <view.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <message.ih>
#include <complete.ih>
#include <filetype.ih>
#include <switcher.ih>

/* A good exercise for advanced readers:
    Change switview to be a subclass of lpair,
    placing an appropriate toggling BUTTON in the top
    and the object being changed in the bottom.
    Even better exercise:  Make the object able to work
    either that way or the way it does in this implementation. */

static void AddSwitchee(), NextSwitchee(),
  SwitchObject(), AddSwitcheeFromFile();

static struct keymap *switview_keymap = NULL;
static struct menulist *switview_menulist = NULL;
static struct proctable_Entry *switchobjproc = NULL;

boolean switview__InitializeClass(c)
struct classheader *c;
{
    struct proctable_Entry *proc = NULL;

    switview_keymap = keymap_New();
    switview_menulist = menulist_New();

    proc = proctable_DefineProc("switview-next-object",
	NextSwitchee, &switview_classinfo, NULL,
	"Changes the switcher to look at the next object.");
    keymap_BindToKey(switview_keymap, "^X^N", proc, 0);
    menulist_AddToML(switview_menulist,
	"Switcher~95,Next Switchee~90", proc, NULL, 0);

    proc = proctable_DefineProc("switview-add-switchee",
	AddSwitchee, &switview_classinfo, NULL,
	"Adds a new thing for the switcher to switch to.");
    keymap_BindToKey(switview_keymap, "^X^A", proc, 0);
    menulist_AddToML(switview_menulist,
	"Switcher~95,Add Switchee~91", proc, NULL, 0);

    proc = proctable_DefineProc("switview-add-from-file",
	AddSwitcheeFromFile, &switview_classinfo, NULL,
	"Adds the contents of a file as a new switchee.");
    keymap_BindToKey(switview_keymap, "^X5", proc, 0);
    menulist_AddToML(switview_menulist,
	"Switcher~95,Insert File~92", proc, NULL, 0);

    switchobjproc = proctable_DefineProc(
	"switview-switch-object", SwitchObject,
	&switview_classinfo, NULL,
	"Switches to a given object.");
    return(TRUE);

}

boolean switview__InitializeObject(c, self)
struct classheader *c;
struct switview *self;
{
    self->ks = keystate_Create(self, switview_keymap);
    self->ml = menulist_DuplicateML(switview_menulist,
				     self);
    if (!self->ks || !self->ml) return(FALSE);
    self->FirstSwitcheroo = NULL;
    self->NowPlaying = NULL;
    return(TRUE);
}

void switview__FinalizeObject(c, self)
struct classheader *c;
struct switview *self;
{
    struct switcheroo *this, *next;

    if (self->ks) {
	keystate_Destroy(self->ks);
	self->ks = NULL;
    }
    if (self->ml) {
	menulist_Destroy(self->ml);
	self->ml = NULL;
    }
    this = self->FirstSwitcheroo;
    while (this) {
	view_Destroy(this->v);
	next = this->next;
	free(this);
	this = next;
    }
}

static boolean CheckRightSwitchee(self, NeedFullRedraw)
struct switview *self;
boolean *NeedFullRedraw;
{
    struct switcher *switcher = (struct switcher *)
      switview_GetDataObject(self);
    struct switcheroo *swtmp;

    *NeedFullRedraw = FALSE;
    if (!switcher->NowPlaying) {
	self->NowPlaying = NULL;
	return(TRUE);
    }
    if (self->NowPlaying) {
	if (self->NowPlaying->switchee == switcher->NowPlaying) {
	    return(TRUE);
	}
	for (swtmp = self->FirstSwitcheroo;
	     swtmp != NULL;
	     swtmp = swtmp->next) {
	    if (swtmp->switchee == switcher->NowPlaying) {
		if (self->NowPlaying) {
		    view_UnlinkTree(self->NowPlaying->v);
		}
		self->NowPlaying = swtmp;
		view_LinkTree(self->NowPlaying->v, self);
		*NeedFullRedraw = TRUE;
		view_WantInputFocus(self->NowPlaying->v,
				    self->NowPlaying->v);
		return(TRUE);
	    }
	}
    }
    swtmp = (struct switcheroo *)
      malloc(sizeof(struct switcheroo));
    if (swtmp) {
	swtmp->v = (struct view *)
	  class_NewObject(switcher->NowPlaying->viewname);
	if (swtmp->v) {
	    view_SetDataObject(swtmp->v,
			switcher->NowPlaying->d);
	    swtmp->next = self->FirstSwitcheroo;
	    swtmp->switchee = switcher->NowPlaying;
	    self->FirstSwitcheroo = swtmp;
	    if (self->NowPlaying) {
		view_UnlinkTree(self->NowPlaying->v);
	    }
	    self->NowPlaying = swtmp;
	    view_LinkTree(self->NowPlaying->v, self);
	    *NeedFullRedraw = TRUE;
	    view_WantInputFocus(self->NowPlaying->v,
				self->NowPlaying->v);
	    return(TRUE);
	}
    }
    return(FALSE);
}

void switview__FullUpdate(self, type, left, top, width, height)
struct switview *self;
enum view_UpdateType type;
long left;
long top;
long width;
long height;
{
    struct rectangle Rect;
    boolean NeedFull; /* ignored */

    if (!CheckRightSwitchee(self, &NeedFull)) return;
    switview_GetVisualBounds(self, &Rect);
    if (!self->NowPlaying) {
	switview_MoveTo(self, (Rect.left + Rect.width) / 2,
			    (Rect.top + Rect.height) / 2);
	switview_DrawString(self, "<No objects>",
	    graphic_BETWEENLEFTANDRIGHT
	    | graphic_BETWEENTOPANDBASELINE);
	return;
    }
    view_InsertView(self->NowPlaying->v, self, &Rect);
    view_FullUpdate(self->NowPlaying->v, type, left,
		     top, width, height);
}

void switview__Update(self)
struct switview *self;
{
    boolean NeedFullRedraw;

    if (!CheckRightSwitchee(self, &NeedFullRedraw)) return;
    if (NeedFullRedraw) {
	struct rectangle Rect;
	switview_GetVisualBounds(self, &Rect);
	switview_EraseRect(self, &Rect);
	switview_FullUpdate(self, view_FullRedraw,
				Rect.left, Rect.top,
				Rect.width, Rect.height);
	return;
    }
    if (self->NowPlaying) view_Update(self->NowPlaying->v);
}

struct view *switview__Hit(self, action, x, y, numberOfClicks)
struct switview *self;
enum view_MouseAction action;
long	x;
long	y;
long	numberOfClicks;
{
    if (!self->NowPlaying) {
	switview_WantInputFocus(self, self);
	return((struct view *) self);
    }
    return(view_Hit(self->NowPlaying->v, action,
		     x, y, numberOfClicks));
}

void switview__PostKeyState(self, ks)
struct switview *self;
struct keystate *ks;
{
    self->ks->next = NULL;
    keystate_AddBefore(self->ks, ks);
    super_PostKeyState(self, self->ks);
}

void switview__PostMenus(self, ml)
struct switview *self;
struct menulist *ml;
{
    struct switchee *sw;
    struct switcher *switcher = (struct switcher *)
      switview_GetDataObject(self);
    char MenuBuf[200];

    menulist_ClearChain(self->ml);
    for (sw = switcher->FirstSwitchee; sw; sw = sw->next) {
	sprintf(MenuBuf, "Switcher~95,%s", sw->label);
	if (self->NowPlaying && (switcher->NowPlaying == sw)) {
	    menulist_DeleteFromML(self->ml, MenuBuf);
	} else {
	    menulist_AddToML(self->ml, MenuBuf,
			     switchobjproc, (long) sw, 0);
	}
    }
    if (ml) menulist_ChainBeforeML(self->ml, ml, ml);
    super_PostMenus(self, self->ml);
}

void switview__LinkTree(self, parent)
struct switview *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if (self->NowPlaying) {
	view_LinkTree(self->NowPlaying->v, self);
    }
}

void switview__WantInputFocus(self, v)
struct switview *self;
struct view *v;
{
    if (self->NowPlaying && (v == (struct view *) self)) {
	v = self->NowPlaying->v;
    }
    super_WantInputFocus(self, v);
}

static void AddSwitchee(self)
struct switview *self;
{
    char ObjName[150], ViewName[150], Label[150];
    struct dataobject *d;
    struct switcher *sw;

    if (message_AskForString(self, 10, "Object to insert: ", 
			      NULL, ObjName,
			      sizeof(ObjName)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    d = (struct dataobject *) class_NewObject(ObjName);
    if (d == NULL) {
	message_DisplayString(self, 10,
			"Could not create new object, sorry.");
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
    sw = (struct switcher *) switview_GetDataObject(self);
    if (!switcher_AddObject(sw, d, Label, ViewName)) {
	message_DisplayString(self, 10,
		"Object creation failed, sorry!");
	return;
    }
    NextSwitchee(self);
    switcher_NotifyObservers(sw, observable_OBJECTCHANGED);
}

static void NextSwitchee(self)
struct switview *self;
{
    struct switcher *switcher = (struct switcher *)
      switview_GetDataObject(self);
    struct switchee *sw = switcher->NowPlaying;

    if (sw && sw->next) {
	switcher_SetNowPlaying(switcher, sw->next->d);
    } else {
	switcher_SetNowPlaying(switcher,
			       switcher->FirstSwitchee->d);
    }
}

static void SwitchObject(self, swin)
struct switview *self;
struct switchee *swin; /* really a long */
{
    struct switcher *switcher = (struct switcher *)
      switview_GetDataObject(self);
    struct switchee *sw;

    for (sw= switcher->FirstSwitchee; sw; sw = sw->next) {
	if (sw == swin) {
	    switcher_SetNowPlaying(switcher, sw->d);
	    return;
	}
    }
    message_DisplayString(self, 10,
	"SwitchObject called for nonexistent object.");
}

void AddSwitcheeFromFile(self)
struct switview *self;
{
    char FileName[150], ViewName[150], Label[150], *ObjName;
    struct dataobject *d;
    FILE *fp;
    long ID;
    struct switcher *sw;

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
		"Could not create new object, sorry.");
	return;
    }
    if (dataobject_Read(d, fp, ID) != dataobject_NOREADERROR) {
	fclose(fp);
	message_DisplayString(self, 10,
		"Read operation failed, sorry.");
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
    sw = (struct switcher *) switview_GetDataObject(self);
    if (!switcher_AddObject(sw, d, Label, ViewName)) {
	message_DisplayString(self, 10,
		"Object creation failed, sorry!");
	return;
    }
    NextSwitchee(self);
    switcher_NotifyObservers(sw, observable_OBJECTCHANGED);
}
