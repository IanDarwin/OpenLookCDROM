static char *flexview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/flex/RCS/flexview.c,v 1.1 1992/10/06 22:11:55 susan R6tape $";

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
#include <flexview.eh>
#include <flex.ih>
#include <dataobj.ih>
#include <im.ih>
#include <view.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <message.ih>
#include <proctbl.ih>

/* forward declarations */
static boolean CreateViews();
static void DrawMe(), InsertObject(), DeleteObjects(), AlterPair();

static struct keymap *flexview_keymap_noviews = NULL,
    *flexview_keymap_views = NULL;
static struct menulist *flexview_menulist_noviews = NULL,
    *flexview_menulist_views = NULL;

boolean flexview__InitializeClass(c)
struct classinfo *c;
{
    struct proctable_Entry *proc = NULL;

    flexview_keymap_views = keymap_New();
    flexview_keymap_noviews = keymap_New();
    flexview_menulist_views = menulist_New();
    flexview_menulist_noviews = menulist_New();

    proc = proctable_DefineProc("flexview-insert-object",
	InsertObject, &flexview_classinfo, NULL,
	"Inserts a new inset in the flex.");
    keymap_BindToKey(flexview_keymap_noviews, "^X!",
		      proc, 0);
    menulist_AddToML(flexview_menulist_noviews,
	"flex~96,Insert Object", proc, NULL, 0);

    proc = proctable_DefineProc("flexview-alter-pair",
	AlterPair, &flexview_classinfo, NULL,
	"Alters some of the settings of the flex.");
    keymap_BindToKey(flexview_keymap_views, "^X%",
		      proc, 0);
    menulist_AddToML(flexview_menulist_views,
	"flex~96,Alter Pair", proc, NULL, 0);

    proc = proctable_DefineProc("flexview-delete-objects",
	DeleteObjects, &flexview_classinfo, NULL,
	"Delete the children of of the flex.");
    keymap_BindToKey(flexview_keymap_views, "^X&",
		      proc, 0);
    menulist_AddToML(flexview_menulist_views,
	"flex~96,Delete Objects", proc, NULL, 0);

    return(TRUE);
}

boolean flexview__InitializeObject(c, self)
struct classinfo *c;
struct flexview *self;
{
    self->KeystateWithViews =
      keystate_Create(self, flexview_keymap_views);
    self->MenulistWithViews =
      menulist_DuplicateML(flexview_menulist_views, self);
    self->KeystateWithNoViews =
      keystate_Create(self, flexview_keymap_noviews);
    self->MenulistWithNoViews =
      menulist_DuplicateML(flexview_menulist_noviews, self);
    self->oldleftdata = NULL;
    self->oldrightdata = NULL;
    self->leftview = NULL;
    self->rightview = NULL;
    return(TRUE);
}

void flexview__FinalizeObject(c, self)
struct classheader *c;
struct flexview *self;
{
    if (self->KeystateWithViews) {
	keystate_Destroy(self->KeystateWithViews);
	self->KeystateWithViews = NULL;
    }
    if (self->KeystateWithNoViews) {
	keystate_Destroy(self->KeystateWithNoViews);
	self->KeystateWithNoViews = NULL;
    }
    if (self->MenulistWithViews) {
	menulist_Destroy(self->MenulistWithViews);
	self->MenulistWithViews = NULL;
    }
    if (self->MenulistWithNoViews) {
	menulist_Destroy(self->MenulistWithNoViews);
	self->MenulistWithNoViews = NULL;
    }
    /* lpair_FinalizeObject should itself
      take care of the subviews */
}

void flexview__FullUpdate(self,type,left,top,width,height)
struct flexview *self;
enum view_UpdateType type;
long left,top,width,height;
{
    struct flex *flex = (struct flex *)
      flexview_GetDataObject(self);

    if (flex && flex->right) {
	if ((self->rightview != NULL) || CreateViews(self)) {
	    if ((self->oldleftdata != flex->left)
		|| (self->oldrightdata != flex->right)) {
		ResetViews(self);
	    }
	    super_FullUpdate(self,type,left,
			     top,width,height);
	}
    } else {
	DrawMe(self);
    }
}

void flexview__Update(self)
struct flexview *self;
{
    struct flex *flex = (struct flex *)
      flexview_GetDataObject(self);

    if (flex && flex->right) {
	if (self->rightview || CreateViews(self)) {
	    int porf, vorh, movable;

	    if ((self->oldleftdata != flex->left)
		|| (self->oldrightdata != flex->right)) {
		ResetViews(self);
	    }
	    flexview_GetLPState(self, &porf,
				    &vorh, &movable);
	    if ((porf != flex->porf) || (vorh != flex->vorh)
		|| (movable != flex->movable)) {
		flexview_SetLPState(self, flex->porf,
			flex->vorh, flex->movable);
	    }
	    super_Update(self);
	}
    } else {
	DrawMe(self);
    }
}

static void DrawMe(self)
struct flexview *self;
{
    struct rectangle r;
    flexview_GetVisualBounds(self,&r);
    flexview_EraseRect(self, &r);
    flexview_MoveTo(self, (r.left + r.width) / 2,
			 (r.top + r.height) / 2);
    flexview_DrawString(self, "<No objects>",
	graphic_BETWEENLEFTANDRIGHT
	| graphic_BETWEENTOPANDBASELINE);
}

struct view *flexview__Hit(self,action,x,y,numberOfClicks)
struct flexview *self;
enum view_MouseAction action;
long x, y, numberOfClicks;
{
    struct view *v;
    struct flex *flex = (struct flex *)
      flexview_GetDataObject(self);

    if (flex && self->rightview) {
	v = super_Hit(self,action,x,y,numberOfClicks);
	if (v == (struct view *) self) {
	    int porf, vorh, movable, pct;
	    flexview_GetLPState(self, &porf,
				    &vorh, &movable);
	    if (porf == lpair_TOPFIXED) {
		pct = flexview_GetObjSize(self, 0);
	    } else {
		pct = flexview_GetObjSize(self, 1);
	    }
	    flex_SetDisplayParams(flex, porf, vorh,
				      movable, pct);
	    flex_NotifyObservers(flex,
			observable_OBJECTCHANGED);
	}
	return(v);
    }
    flexview_WantInputFocus(self, self);
    return((struct view *) self);
}

void flexview__PostKeyState(self, ks)
struct flexview *self;
struct keystate *ks;
{
    struct keystate *myks;

    if (self->rightview != NULL) {
	myks = self->KeystateWithViews;
    } else {
	myks = self->KeystateWithNoViews;
    }
    myks->next = NULL;
    keystate_AddBefore(myks, ks);
    super_PostKeyState(self, myks);
}

void flexview__PostMenus(self, ml)
struct flexview *self;
struct menulist *ml;
{
    struct menulist *myml;

    if (self->rightview != NULL) {
	myml = self->MenulistWithViews;
    } else {
	myml = self->MenulistWithNoViews;
    }
    menulist_ClearChain(myml);
    if (ml) menulist_ChainBeforeML(myml, ml, ml);
    super_PostMenus(self, myml);
}

enum view_DSattributes flexview__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct flexview *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;
{ 
    if (self->rightview) {
	return(super_DesiredSize(self, width, height, pass,
			desiredwidth, desiredheight));
    }
    *desiredwidth = 200;
    *desiredheight = 100;
    return(view_WidthFlexible | view_HeightFlexible);
}

static boolean CreateViews(self)
struct flexview *self;
{
    struct flex *flex = (struct flex *)
      flexview_GetDataObject(self);

    self->leftview = (struct view *)
      class_NewObject(flex->lvname);
    if (self->leftview == NULL) return(FALSE);
    view_SetDataObject(self->leftview, flex->left);
    self->rightview = (struct view *)
      class_NewObject(flex->rvname);
    if (self->rightview == NULL) return(FALSE);
    view_SetDataObject(self->rightview, flex->right);
    flexview_SetUp(self, self->leftview, self->rightview,
	flex->pct, flex->porf, flex->vorh, flex->movable);
    flexview_SetLPState(self, flex->porf, flex->vorh,
	flex->movable);
    view_WantInputFocus(self->leftview, self->leftview);
    self->oldleftdata = flex->left;
    self->oldrightdata = flex->right;
    return(TRUE);
}

static ResetViews(self)
struct flexview *self;
{
    struct flex *flex = (struct flex *)
      flexview_GetDataObject(self);
    struct view *oldfocus;

    if (self->oldleftdata == flex->right) {
	oldfocus = im_GetInputFocus(flexview_GetIM(self));
	flexview_SetNth(self, 0, self->leftview);
	flexview_SetNth(self, 1, self->rightview);
	self->oldleftdata = flex->left;
	self->oldrightdata = flex->right;
	flexview_WantUpdate(self, self);
	if (oldfocus != NULL) {
	    view_WantInputFocus(oldfocus, oldfocus);
	}
    }
}

static void InsertObject(self)
struct flexview *self;
{
    char objname[250], *defaultviewname, viewname[250];
    struct dataobject *d;
    struct flex *flex;

    if (self->rightview) {
	message_DisplayString(self, 10,
		"You already have objects here!");
	return;
    }
    if (message_AskForString(self, 10,
		"Data object to insert: ", "flex",
		objname, sizeof(objname)) < 0) {
	message_DisplayString(self, 10, "Cancelled.");
	return;
    }
    d = (struct dataobject *) class_NewObject(objname);
    if (d == NULL) {
	message_DisplayString(self, 10,
		"Object creation failed.");
	return;
    }
    defaultviewname = dataobject_ViewName(d);
    if (message_AskForString(self, 10, "View to use: ",
	defaultviewname, viewname, sizeof(viewname)) < 0) {
	return;
    }
    flex = (struct flex *) flexview_GetDataObject(self);
    if (!flex_InsertObject(flex, d, viewname)) {
	message_DisplayString(self, 10,
		"Could not insert object -- sorry.");
	return;
    }
    flex_NotifyObservers(flex, observable_OBJECTCHANGED);
}

static void DeleteObjects(self)
struct flexview *self;
{
    char Prompt[256];
    int result;
    static char *Choices[] = {
	"Delete both sub-objects", "Cancel", NULL};
    struct flex *flex;

    if (!self->rightview) {
	message_DisplayString(self, 10,
		"There is nothing to delete!");
	return;
    }
    flex = (struct flex *)
      flexview_GetDataObject(self);
    sprintf(Prompt,
	 "This pair contains a %s on a %s and a %s on a %s.",
	 class_GetTypeName(self->leftview),
	 class_GetTypeName(flex->left),
	 class_GetTypeName(self->rightview),
	 class_GetTypeName(flex->right));
    if (message_MultipleChoiceQuestion(self, 50, Prompt, 1,
	 &result, Choices, NULL) < 0) return;
    if (result != 0 && result != 1) return;
    /* The following two lines will call
      view_UnlinkTree on self->leftview,
      so we don't need to do it here */
    flexview_SetNth(self, 0, NULL);
    flexview_SetNth(self, 1, NULL);
    view_Destroy(self->leftview);
    view_Destroy(self->rightview);
    self->leftview = self->rightview = NULL;
    self->oldleftdata = NULL;
    self->oldrightdata = NULL;
    flex_DeleteObjects(flex);
    flex_NotifyObservers(flex, observable_OBJECTCHANGED);
    flexview_WantUpdate(self, self);
    flexview_WantInputFocus(self, self);
}

static void AlterPair(self)
struct flexview *self;
{
    struct view *v;
    char *QVec[8];
    int result;
    struct flex *flex = (struct flex *)
      flexview_GetDataObject(self);

    if (!flex || !self->rightview) {
	message_DisplayString(self, 10,
		"You don't have anything to alter here yet.");
	return;
    }
    QVec[0] = "Nothing";
    QVec[1] = "Toggle two views";
    if (flex->vorh == lpair_HORIZONTAL) {
	QVec[2] = "Make split vertical";
    } else {
	QVec[2] = "Make split horizontal";
    }
    if (flex->porf == lpair_PERCENTAGE) {
	QVec[3] = "Keep sizes split by percentages";
    } else {
	QVec[3] = "Make sizes split by percentages";
    }
    if (flex->porf == lpair_TOPFIXED) {
	QVec[4] = "Keep top part fixed size";
    } else {
	QVec[4] = "Make top part fixed size";
    }
    if (flex->porf == lpair_BOTTOMFIXED) {
	QVec[5] = "Keep bottom part fixed size";
    } else {
	QVec[5] = "Make bottom part fixed size";
    }
    if (flex->movable) {
	QVec[6] = "Make split not movable";
    } else {
	QVec[6] = "Make split movable";
    }
    QVec[7] = NULL;
    if (message_MultipleChoiceQuestion(self, 50,
	"What do you want to change?", 0, &result,
	QVec, NULL) < 0) return;
    switch(result) {
	case 0:
	    return; /* Doing nothing */
	case 1:
	    flex_ToggleParts(flex);
	    flex_NotifyObservers(flex,
			observable_OBJECTCHANGED);
	    v = self->leftview;
	    self->leftview = self->rightview;
	    self->rightview = v;
	    break;
	case 2:
	    flex->vorh = !flex->vorh;
	    break;
	case 3:
	    flex->porf = lpair_PERCENTAGE;
	    break;
	case 4:
	    flex->porf = lpair_TOPFIXED;
	    break;
	case 5:
	    flex->porf = lpair_BOTTOMFIXED;
	    break;
	case 6:
	    flex->movable = !flex->movable;
	    break;
    }
    flex_NotifyObservers(flex, observable_OBJECTCHANGED);
}

