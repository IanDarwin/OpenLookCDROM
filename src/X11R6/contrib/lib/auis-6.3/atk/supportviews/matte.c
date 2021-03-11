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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/matte.c,v 2.17 1994/04/15 17:33:19 rr2b Exp $";
#endif


 


#define UNSET 0
#define FUDGE 2
#define matte_ChangeWidth 1
#define matte_ChangeHeight 2

#include <class.h>
#include <bind.ih>
#include <view.ih>
#include <viewref.ih>
#include <menulist.ih>
#include <cursor.ih>
#include <graphic.ih>
#include <im.ih>
#include <keymap.ih>

#include <environ.ih>

#include <matte.eh>

#define RESIZING TRUE
#define DRAWING FALSE

struct menulist *matteMenus;
static struct keymap *matteKeyMap;

static void UpdateCursors(self)
struct matte *self;
{
    switch(self->Moving){
	case matte_ChangeWidth:
	    im_SetProcessCursor(self->widthcursor);
	    break;
	case matte_ChangeHeight:
	     im_SetProcessCursor(self->heightcursor);
	    break;
	default:
	    if(self->WasMoving){
		im_SetProcessCursor(NULL);
		self->WasMoving = FALSE;
	    }
	    else if(self->resizing){
		struct rectangle sb;	
		long width  = matte_GetLogicalWidth(self) ;
		long height = matte_GetLogicalHeight(self) ;

		if(width <= FUDGE || height <= FUDGE) break;
		sb.top = 0; sb.left = width - FUDGE + 1;
		sb.height = height - FUDGE; sb.width = FUDGE -1 ;
		matte_PostCursor(self,&sb,self->widthcursor);
		sb.top = height - FUDGE + 1; sb.left =0;
		sb.height = FUDGE - 1 ; sb.width = width - FUDGE;
		matte_PostCursor(self,&sb,self->heightcursor);
	    }
	    else if(self->WasResizing){
		matte_RetractViewCursors(self,self);
		self->WasResizing = FALSE;
	    }
	    break;
    }
}
void matte__Print(self, file, processor, finalFormat, topLevel)
struct matte *self;
FILE *file;
char *processor;
char *finalFormat;
boolean topLevel;
{
    if(self->child) 
	view_Print(self->child,file, processor, finalFormat, topLevel);
}
enum view_DSattributes matte__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct matte *self;
long width;
long height;
enum view_DSpass pass;
long *dWidth;
long *dHeight;
{
    enum view_DSattributes val;
    long pwidth , pheight ;
    self->sizepending = FALSE;
    if(self->child && self->desw == UNSET && self->desh == UNSET) {
	val = view_DesiredSize(self->child, width -2 , height -2 , pass, dWidth, dHeight);
	*dWidth += 2;
	*dHeight += 2;
	return val;
    }
    pheight = (self->desh != UNSET) ? self->desh : height - 2;
    pwidth = (self->desw != UNSET) ? self->desw : width - 2;
    switch(pass){
	case view_HeightSet:
	    pheight = height -2;
	    if(self->desw != UNSET){
		*dWidth = self->desw;
		return view_Fixed;
	    }
	    break;
	case view_WidthSet:
	    pwidth = width - 2;
	    if(self->desh != UNSET ){
		*dHeight = self->desh;
		return view_Fixed;
	    }
	    break;
	case view_NoSet:	
	    if(self->desh != UNSET && self->desw != UNSET){
		*dHeight = self->desh;
		*dWidth = self->desw;
		return view_Fixed;
	    }
	    if(self->desh != UNSET) pass = view_HeightSet;
	    else if(self->desw != UNSET) pass = view_WidthSet;
    }
    if(self->child )val = view_DesiredSize(self->child, pwidth , pheight , pass, dWidth, dHeight);
    else{
	val = view_HeightFlexible | view_WidthFlexible;
	*dHeight = height - 2; 
	*dWidth = width - 2;
    }
    if(self->desh == UNSET) *dHeight += 2;
    else  *dHeight = self->desh;
    if(self->desw == UNSET) *dWidth += 2;
    else *dWidth = self->desw;
    if(*dWidth > width) *dWidth = width;
    if(*dHeight > height ) *dHeight = height;
    return val;
}

void matte__GetOrigin(self, width, height, originX, originY)
    struct matte *self;
    long width;
    long height;
    long *originX;
    long *originY;
{
    if (self->child != NULL)
        view_GetOrigin(self->child, width, height, originX, originY);
    else
        super_GetOrigin(self, width, height, originX, originY);
}

void matte__WantInputFocus(self, requestor)
struct matte *self;
struct view *requestor;
{
    if(((struct view *)self)==requestor && self->child) super_WantInputFocus(self, self->child);
    else super_WantInputFocus(self, requestor);
}

void matte__ReceiveInputFocus(self)
    struct matte *self;
{
    if(self->child) view_ReceiveInputFocus(self->child);
    matte_WantUpdate(self,self);
}

void matte__LoseInputFocus(self)
    struct matte *self;
{
    if(self->child) view_LoseInputFocus(self->child);
    matte_WantUpdate(self,self);
}

struct view *matte__Hit(self,action,mousex,mousey,numberOfClicks) 
struct matte *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    if((self->child && !self->resizing) || self->ref == NULL)	
	return view_Hit(self->child,action,view_EnclosedXToLocalX(self->child, mousex), view_EnclosedYToLocalY(self->child, mousey),numberOfClicks);

    switch(action){
	case view_LeftDown:
	case view_RightDown:
	    {
	    long width = matte_GetLogicalRight(self);
	    long height = matte_GetLogicalBottom(self);
	    if(width - FUDGE < mousex) self->Moving = matte_ChangeWidth;
	    else if(height - FUDGE < mousey) self->Moving = matte_ChangeHeight;
	    else if(self->child) 
		return view_Hit(self->child,action,view_EnclosedXToLocalX(self->child, mousex), view_EnclosedYToLocalY(self->child, mousey),numberOfClicks);
	    else return FALSE;
	    UpdateCursors(self);
	    return (struct view *) self;
	    }
	case view_RightUp:
	case view_LeftUp:
	    if(self->Moving){
		int move = self->Moving;
		self->Moving = 0;
		self->WasMoving = 1;
		UpdateCursors(self);
		if(move == matte_ChangeWidth && mousex > 0) {
			self->ref->desw = mousex + 1;
			viewref_NotifyObservers(self->ref,0);
		}
		else if(move == matte_ChangeHeight && mousey > 0) {
			self->ref->desh = mousey + 1;
			viewref_NotifyObservers(self->ref,0);
		}
		return (struct view *) self;
	    }
    }
    if(self->Moving) return (struct view *) self;
    return NULL;
}
UpdateDrawing(self)
struct matte *self;
{
    if(self->OldMode != self->drawing){
	struct rectangle enclosingRect;
	matte_SetTransferMode(self,graphic_INVERT);
	enclosingRect.top = 0; enclosingRect.left = 0;
	enclosingRect.width  = matte_GetLogicalWidth(self) -1 ;
	enclosingRect.height = matte_GetLogicalHeight(self) -1 ;
	matte_DrawRect(self,&enclosingRect);
	self->OldMode = self->drawing;
    }
}
void matte__Update(self)
struct matte *self;
{
    if(self->ref && self->desw != self->ref->desw || self->desh != self->ref->desh){
	self->desw = self->ref->desw ;
	self->desh = self->ref->desh ;
	if(!self->sizepending) {
	    super_WantNewSize(self,self); 
	    self->sizepending = TRUE;
	}
	return; /* Might as well, since our parent will be redrawing us again */
    }
    if(self->child) view_Update(self->child);
    UpdateCursors(self);
    UpdateDrawing(self);
}
void matte__FullUpdate(self,type,left,top,width,height)
struct matte *self;
enum view_UpdateType type;
long left,top,width,height;
{
    struct rectangle enclosingRect;
    if(type == view_FullRedraw && self->ref && (self->desw != self->ref->desw || self->desh != self->ref->desh)){
	self->desw = self->ref->desw ;
	self->desh = self->ref->desh ;
	if(!self->sizepending) {
	    super_WantNewSize(self,self); 
	    self->sizepending = TRUE;
	}
	return; /* Might as well, since our parent will be redrawing us again */
    }
    enclosingRect.top = 0; enclosingRect.left = 0;
    enclosingRect.width  = matte_GetLogicalWidth(self) -1 ;
    enclosingRect.height = matte_GetLogicalHeight(self) -1 ;
    if(type != view_Remove){
	UpdateCursors(self);
	if(type != view_MoveNoRedraw){
	    matte_SetTransferMode(self,graphic_WHITE);
	    matte_DrawRect(self,&enclosingRect);
	    matte_SetTransferMode(self,graphic_INVERT);
	    if(self->drawing){ 
		matte_DrawRect(self,&enclosingRect);
	    }
	    self->OldMode = self->drawing;
	}
    }
    enclosingRect.top++; enclosingRect.left++;
    enclosingRect.width--  ;
    enclosingRect.height-- ;

    if(self->child) {
	view_InsertView(self->child, self, &enclosingRect);
	matte_RetractViewCursors(self,self->child);
	view_FullUpdate(self->child,type,left,top,width,height);
    }
}
matte_PromptForViewName(self)
struct matte *self;
{   /* MISSING FUNCTION */
return NULL;
}
struct matte *matte__Create(classID,vr,parent)
struct classheader *classID;
struct viewref *vr;
struct view *parent;
{
    struct matte *self;
    char *viewT=vr->viewType;
    self = matte_New();
    self->ref = vr;
    self->desh = self->ref->desh;
    self->desw = self->ref->desw;
    matte_LinkTree(self,parent);
    
    if(vr->dataObject && class_IsTypeByName(class_GetTypeName(vr->dataObject), "unknown")) {
	if(viewT==NULL || !class_IsTypeByName(viewT, "unknownv")) {
	    viewT="unknownv";
	}
    }
    if(viewT == NULL || !class_IsTypeByName(viewT,"view")){
	if(!matte_PromptForViewName(self)) {
	    matte_Destroy(self);
	    return(NULL);
	}
    }
    if((self->child = (struct view *)class_NewObject(viewT)) == NULL){
	matte_Destroy(self);
	return(NULL);
    }
    if(vr->dataObject)
	view_SetDataObject(self->child ,vr->dataObject);
    view_LinkTree(self->child,(struct view *)self);
    viewref_AddObserver(vr,self);
    view_AddObserver(self->child,self);
    return(self);
}
void matte__SetDataObject(self,dd)
struct matte *self;
struct dataobject *dd;
{
    if(self->child){
	view_SetDataObject(self->child,dd);
    }
}
boolean matte__InitializeObject(classID,self)
struct classheader *classID;
struct matte *self;
{
    self->widthcursor = cursor_Create(self);
    self->heightcursor = cursor_Create(self);
    cursor_SetStandard(self->heightcursor,Cursor_HorizontalBars);
    cursor_SetStandard(self->widthcursor,Cursor_VerticalBars);
    self->Moving = 0; 
    self->WasMoving = 0;
    self->resizing = environ_GetProfileSwitch("ResizeInset", RESIZING);
    self->WasResizing = 0;
    self->ref = NULL;
    self->child = NULL;
    self->desw = self->desh = UNSET;
    self->menus = menulist_DuplicateML(matteMenus, self);
    self->drawing = environ_GetProfileSwitch("DrawInsetBorder", DRAWING);
    self->sizepending = TRUE;
    return TRUE;
}
void matte__SetResizing(self,key)
struct matte *self;
long key;
{
    if(self->resizing == 0){
	self->resizing = 1;
    }
    else {
	self->resizing = 0;
	self->WasResizing = 1;
    }
    matte_WantUpdate(self,self);
}
void matte__SetDrawing(self,key)
struct matte *self;
long key;
{
    self->drawing = !self->drawing;
    matte_WantUpdate(self,self);
}
void matte__PostMenus(self, menulist)
struct matte *self;
struct menulist *menulist;
{
    menulist_ClearChain(self->menus);
    menulist_ChainBeforeML(self->menus, menulist, (long) menulist);
    super_PostMenus(self, self->menus);
}
void matte__WantNewSize(self,requestor)
    struct matte *self;
struct view *requestor;
{

    if(self->ref && (self->ref->desw != UNSET ||  self->ref->desh != UNSET)){
	self->ref->desw = self->ref->desh = UNSET;
      self->desw = self->desh = UNSET;
	viewref_NotifyObservers(self->ref,0);
    }
      if(!self->sizepending) {
	super_WantNewSize(self,self);
	  self->sizepending = TRUE;
      }
}

static struct bind_Description matteBindings[]={
    {"matte-allow-resizing",NULL,0,NULL,0,0,matte__SetResizing,"Allow view to resize" },
    {"matte-allow-drawing",NULL,0,NULL,0,0,matte__SetDrawing,"Draw the view's border" },
    NULL
};

boolean matte__InitializeClass(classID)
    struct classheader *classID;
{
    matteMenus = menulist_New();
    matteKeyMap =  keymap_New();
    bind_BindList(matteBindings, matteKeyMap , matteMenus, &matte_classinfo);
    return TRUE;
}
void matte__ObservedChanged(self, changed, value)
struct matte *self;
struct observable *changed;
long value;
{
    if(changed == (struct observable *)self->ref ) 
    {
	if (value == observable_OBJECTDESTROYED){
	    self->ref = NULL;
 /* assumes the parent is also observing the viewref and will destroy the matte */
/*	    matte_Destroy(self); */
	}
	else if (self->ref && self->desw != self->ref->desw || self->desh != self->ref->desh) 
	    matte_WantUpdate(self,self);
	}
    if(changed == (struct observable *)self->child && value ==  observable_OBJECTDESTROYED){
	self->child = NULL;
	if(self->ref) {
	    struct viewref *vv = self->ref;
	    self->ref = NULL;
	    viewref_RemoveObserver(vv,self);
	    viewref_Destroy(vv);
	}
	else matte_Destroy(self);
    }
}
void matte__LinkTree(self, parent)
struct matte *self;
struct view *parent;
{

    super_LinkTree(self, parent);
    if (self->child != NULL)
        view_LinkTree(self->child, self);
}
void matte__InitChildren(self)
struct matte *self;
{
    if (self->child != NULL)
        view_InitChildren(self->child);
}
void matte__FinalizeObject(classID, self)
struct classheader *classID;
struct matte *self;
{
    if(self->child){
	struct view *child = self->child;
	self->child = NULL;
	view_RemoveObserver(child,self);
	view_Destroy(child);
    }
    if(self->ref){
	viewref_RemoveObserver(self->ref,self);
    }
    cursor_Destroy(self->widthcursor);
    cursor_Destroy(self->heightcursor);
    menulist_Destroy(self->menus);
}
