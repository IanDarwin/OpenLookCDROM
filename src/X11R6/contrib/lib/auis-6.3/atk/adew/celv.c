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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/celv.c,v 2.36 1994/04/17 22:06:26 rr2b Exp $";
#endif


 

#define UNSET 0
#define FUDGE 2
#define celview_ChangeWidth 1
#define celview_ChangeHeight 2
#define STRING 1
#define LONG 2
#define FLOAT 3
#define INITNOW 100
#define VALUE 10
#define ChildMenus 1
#define ClientMenus 2
#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <bind.ih>
#include <view.ih>
#include <cel.ih>
#include <menulist.ih>
#include <cursor.ih>
#include <graphic.ih>
#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <atom.ih>
#include <atomlist.ih>
#include <rm.ih>
#include <valuev.ih>
#include <text.ih>
#include <message.ih>
#include <arbiterv.ih>
#include <arbcon.ih>
#include <filetype.ih>
#include <dataobj.ih>
#include <complete.ih>
#include <proctbl.ih>
#include <celv.eh>
#include <arbiter.ih>
/* #define DEBUG */

#define DataObject(A) (A->header.view.dataobject)
#define Parent(A) (A->header.view.parent)
#define Data(A) ((struct cel *)DataObject(A) )
#define VALUE 10
static struct atom *UNSETFLAG;
#define RESIZING FALSE
#define DRAWING FALSE
struct menulist *celviewMenus;
static struct keymap *celviewKeyMap;
#define DataObject(A) (A->header.view.dataobject)
#define Cel(A) ((struct cel *) DataObject(A))
#define NameSet(V) (((struct view *)V)->name_explicitly_set)
struct overlay  {
struct dataobject *object;
struct view *view;
struct rectangle rect;
long flags;
struct overlay *next;
};
#define SCALEWID
#define DOINDENT(SELF) (SELF->drawing  || SELF->resizing)
#define celview_COVERCHILD 1
static scaleoverlay(self,ov,or)
struct celview *self;
struct overlay *ov;
struct rectangle *or;
{
    struct rectangle *nr = &(self->enclosingRect);
    struct rectangle *sr = &(ov->rect);
/*     if(ov->flags & celview_COVERCHILD) {
	*sr = *or;
	return;
    } */
    if(or->width > 5 && or->height > 5 && nr->width > 5 && nr->height > 5)
    {
	if(or->height != nr->height){
	    sr->top = (sr->top * nr->height) / or->height;
	    sr->height = (sr->height * nr->height) / or->height;
	}
	if(or->width != nr->width){
	    sr->left = (sr->left * nr->width) / or->width;
	    sr->width = (sr->width * nr->width) / or->width;
	}
    }
}
static struct view *PopOverlay(self,v)
struct celview *self;
struct view *v;
{
    if(self->olist){
	struct overlay *o = NULL,*dm;
	dm = self->olist;
	if(v != NULL) {
	    for(;dm != NULL;dm = dm->next){
		if(dm->view == v) break;
		o = dm;
	    }
	    if(dm == NULL) return NULL;
	}
	v = dm->view;
	if(dm != self->olist) o->next = dm->next;
	else{
	    o = dm->next;
	    if((self->olist = o) == NULL){
		self->child  = self->safec ;
		self->truechild = self->safetc;
	    }
	    else self->child = self->truechild = self->olist->view;
	}
	view_UnlinkTree(v);
	self->mode = celview_UpdateView;
	celview_WantUpdate(self,self);
	free(dm);
	return v;
    }
    return NULL;
}
static void UpdateCursors(self)
struct celview *self;
{
    switch(self->Moving){
	case celview_ChangeWidth:
	    im_SetProcessCursor(self->widthcursor);
	    break;
	case celview_ChangeHeight:
	     im_SetProcessCursor(self->heightcursor);
	    break;
	default:
	    if(self->WasMoving){
		im_SetProcessCursor(NULL);
		self->WasMoving = FALSE;
	    }
	    else if(self->resizing){
		struct rectangle sb;	
		long width  = celview_GetLogicalWidth(self) ;
		long height = celview_GetLogicalHeight(self) ;

		if(width <= FUDGE || height <= FUDGE) break;
		sb.top = 0; sb.left = width - FUDGE + 1;
		sb.height = height - FUDGE; sb.width = FUDGE -1 ;
		celview_PostCursor(self,&sb,self->widthcursor);
		sb.top = height - FUDGE + 1; sb.left =0;
		sb.height = FUDGE - 1 ; sb.width = width - FUDGE;
		celview_PostCursor(self,&sb,self->heightcursor);
	    }
	    else if(self->WasResizing){
		celview_RetractViewCursors(self,self);
		self->WasResizing = FALSE;
	    }
	    break;
    }
}
void celview__Print(self, file, processor, finalFormat, topLevel)
struct celview *self;
FILE *file;
char *processor;
char *finalFormat;
boolean topLevel;
{
    if(self->truechild) 
	view_Print(self->truechild,file, processor, finalFormat, topLevel);
}
enum view_DSattributes celview__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct celview *self;
long width;
long height;
enum view_DSpass pass;
long *dWidth;
long *dHeight;
{
    enum view_DSattributes val;
    long pwidth , pheight ,offset;
    self->sizepending = FALSE;
    if(self->vismode == cel_INVISIBLE) {
	*dWidth = 0;
	*dHeight = 0;
	return(view_Fixed);
    }
    if(cel_GetReadCount(Cel(self)) == 0 &&
	(cel_GetDefaultStream(Cel(self)) != NULL ||
	 cel_GetInitFile(Cel(self)) != NULL)){
	cel_InitDefault(Cel(self));
	celview_makeview(self,Cel(self));
    }
/*
    if(self->child && self->mode == celview_FirstUpdate){
	view_LinkTree(self->child,self);
	self->mode = celview_HasView;
    }
*/
    if(self->child && self->desw == UNSET && self->desh == UNSET) {
	if(DOINDENT(self)){
	    val = view_DesiredSize(self->child, width -2 , height -2 , pass, dWidth, dHeight);
	    *dWidth += 2;
	    *dHeight += 2;
	}
	else val = view_DesiredSize(self->child, width , height , pass, dWidth, dHeight);
	return val;
    }
    offset = DOINDENT(self)? 2: 0;
    pheight = (self->desh != UNSET) ? self->desh : height - offset;
    pwidth = (self->desw != UNSET) ? self->desw : width - offset;
    switch(pass){
	case view_HeightSet:
	    pheight = height -offset;
	    if(self->desw != UNSET){
		*dWidth = self->desw;
		return view_Fixed;
	    }
	    break;
	case view_WidthSet:
	    pwidth = width - offset;
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
    else if( cel_GetDefaultStream(Data(self)) || cel_GetInitFile(Data(self)) ){
	val = view_HeightFlexible | view_WidthFlexible;
	 *dWidth = width;
	 *dHeight = height;
	 return val;
    }
    else{
	val = view_HeightFlexible | view_WidthFlexible;
/*
	*dHeight =(self->desh != UNSET) ? self->desh :  height - offset; 
	if(*dHeight  > 1024) *dHeight = 200;
	*dWidth =(self->desw != UNSET) ? self->desw : width - offset;
*/
	*dWidth = 100; *dHeight = 100;
    }
    if(self->desh == UNSET) *dHeight += offset;
    else  *dHeight = self->desh;
    if(self->desw == UNSET) *dWidth += offset;
    else *dWidth = self->desw;
    if(*dWidth > width) *dWidth = width;
    if(*dHeight > height ) *dHeight = height;
    return val;
}
void celview__ReceiveInputFocus(self)
    struct celview *self;
{
    if(self->truechild) view_WantInputFocus(self->truechild,self->truechild);
    else {
	celview_PostMenus(self,NULL);
	self->HasFocus = TRUE;
    }
   celview_WantUpdate(self,self);
}

void celview__LoseInputFocus(self)
    struct celview *self;
{
    if(self->child) view_LoseInputFocus(self->child);
    self->HasFocus = FALSE;
    celview_WantUpdate(self,self);
}


static UpdateDrawing(self)
struct celview *self;
{
    if(self->OldMode != self->drawing || self->child == NULL){
	if(self->child == NULL){
	    celview_SetTransferMode(self,graphic_WHITE);
	    celview_EraseVisualRect(self);
	}
	celview_SetTransferMode(self,graphic_INVERT);
	celview_DrawRect(self,&(self->enclosingRect));
	if(self->child == NULL && self->HasFocus){
	    celview_MoveTo(self,0,0);
	    celview_DrawLineTo(self,self->enclosingRect.width,self->enclosingRect.height);
	    celview_MoveTo(self,0,self->enclosingRect.height);
	    celview_DrawLineTo(self,self->enclosingRect.width,0);
	}
	self->OldMode = self->drawing;
    }
}
struct view *celview__makeview(self,ls)
struct celview *self;
struct cel *ls;
{
    /*    if(ls->application == VALUE && ls->valuename == NULL){
	if(ls->dispatcher == NULL) celview_GetDispatcher(self); */
    char *lv=ls->viewType;
    struct view *truechild;
    if(ls->dataObject && class_IsTypeByName(class_GetTypeName(ls->dataObject), "unknown")) {
	if(lv==NULL || !class_IsTypeByName(lv, "unknownv")) {
	    lv="unknownv";
	}
    }
    self->NeedsRemade = FALSE;
    if(self->child == NULL || self->viewatm != cel_GetViewAtom(ls) || self->dataatm != cel_GetObjectAtom(ls)){
	self->viewatm = cel_GetViewAtom(ls);
	self->dataatm = cel_GetObjectAtom(ls);
	if(lv != NULL && *lv != '\0' /* && ls->dataObject != NULL */ && (truechild = (struct view *) class_NewObject(lv)) )  {
	    /*	celview_RetractCursor(self,self->cursor); */
	    if(ls->application == cel_VALUE && ls->script == NULL) 
		self->promptforparameters = TRUE;
	    if(ls->dataType &&  *ls->dataType && view_CanView(truechild,ls->dataType) == FALSE){
		/* given bad view, so use default */
		view_Destroy(truechild);
		cel_SetViewName(ls,NULL,TRUE);
		truechild = (struct view *) class_NewObject(ls->viewType);
	    }
	    if(truechild){
		struct dataobject *dob;
		self->application = ls->application + 1; /* kicks in code below */
		self->refatm = NULL;
		self->mode = celview_NoUpdate;
		if(ls->dataObject == NULL){
		    if(ls->linkname != NULL && (dob = arbiterview_GetNamedObject(self,ls->linkname)) != NULL &&
		       strcmp(class_GetTypeName(dob),ls->dataType) == 0){
			ls->dataObject = dob;
			dataobject_Reference(dob);
		    }
		    else{
			if(ls->linkname){
			    char buf[128];
			    sprintf(buf,"can't link to %s",ls->linkname);
			    cel_SetLinkName(ls,NULL);
			}
			ls->dataObject = (struct dataobject *)class_NewObject(ls->dataType);
		    }
		} 
		if(ls && ls->dataObject != NULL) 
		    view_SetDataObject(truechild,ls->dataObject);
		view_LinkTree(truechild,self);	
		view_AddObserver(truechild,self);
		self->truechild = truechild;
		menulist_SetMask(self->menus,0);
		self->child = NULL;
		self->mode = celview_FirstUpdate; 
	    }
	    else{
		self->child = self->truechild = NULL;
	    }
	}
	else{
	    self->child = self->truechild = NULL;
	}
	
    }
    if((self->refatm != cel_GetRefAtom(ls)) && ls->refname){
	self->refatm =  cel_GetRefAtom(ls);
	if(self->truechild  && *ls->refname)
	    view_SetName(self->truechild,atomlist_StringToAtomlist(ls->refname));
    }
    if(self->application != ls->application ){
	self->application = ls->application;
	if(self->truechild){
	    if(ls->application == cel_APPLICATION || ls->application == cel_VALUE || self->AddAppLayer){
		if((self->child = view_GetApplicationLayer(self->truechild)) == NULL)
		    self->child = self->truechild;
		else view_LinkTree(self->child,self);
	    }
	    else{
		self->child = self->truechild;
	    }
	}
    }
    if(self->child){
	if(self->promptforparameters) {
#ifdef DEBUG
	    printf("calling GetParameters\n");
#endif /* DEBUG */
	    GetParameters(self);
	}
	if(ls->application == cel_VALUE) 
	    /*PostParameters(self);*/
	    self->NeedsPost = TRUE;
	if(self->HasFocus)
	    view_WantInputFocus(self->truechild,self->truechild);
    }
    else self->mode = 0;
    return self->child;
}
static initchild(self)
struct celview *self;
{
    struct cel *vr = Cel(self);
    if(vr->viewType == NULL) return;
    celview_makeview(self,vr);
}
static char * trunc(c)
char *c;
{
    char *cp;
    if((cp = strrchr(c,'/')) != NULL && *(++cp) != '\0')
	return cp;
    return c;
}
void celview_ReadFile(self,thisFile,iname)
struct celview *self;
FILE *thisFile;
char *iname;
{
    int objectID;
    char *objectName;
    struct cel *ls = Data(self);
    objectName = filetype_Lookup(thisFile, iname, &objectID, NULL); /* For now, ignore attributes. */
    if(objectName == NULL) objectName = "text";
/*    if(class_IsTypeByName("cel",objectName)){ */
    if(strcmp(class_GetTypeName(ls),objectName) == 0){
	cel_Read(ls,thisFile,objectID);
	if(self->arb) arbiterview_InitCell(self->arb,self);
    }
    else{
	if(objecttest(self,objectName,"dataobject") && cel_SetObjectByName(ls,objectName) && ls->dataObject != NULL){
	    char *nm = NULL;
	    struct arbiter *arb = NULL;
	    cel_SetViewName(ls,NULL,TRUE);
	    cel_SetRefName(ls,"");
	    dataobject_Read(ls->dataObject,thisFile,objectID);
	    if(strcmp(objectName,"arbiter") == 0){
		arb = (struct arbiter *) cel_GetObject(ls);
		if((nm = arbiter_GetRefName(arb)) == NULL || *nm == '\0'){
		    if(iname && *iname) arbiter_SetRefName(arb,trunc(iname));
		    else arbiter_SetRefName(arb,objectName);
		}
		cel_SetRefName(ls,"arbcel");
	    }
	    else{
		if(iname && *iname) cel_SetRefName(ls,trunc(iname));
		else cel_SetRefName(ls,objectName);
	    }
	}
    }
    cel_NotifyObservers(ls,0);
}
static void celview_Paste(self)
struct celview *self;
{
    FILE *pasteFile;
    if(self->child ) return;
    pasteFile = im_FromCutBuffer(celview_GetIM(self));
    celview_ReadFile(self,pasteFile,NULL);
    im_CloseFromCutBuffer(celview_GetIM(self), pasteFile);

}
static void celview_PromptForFile(self)
struct celview *self;
{
    char frs[1024];
    FILE *thisFile;
    if(completion_GetFilename(self,"File to insert: ",NULL,frs,1024,FALSE,TRUE) == -1)
	return;
    if ((thisFile = fopen(frs, "r")) == NULL){
            message_DisplayString(self, 0,"can't open file");
	    return;
    }
    celview_ReadFile(self,thisFile,frs);
    fclose(thisFile);
}
static void InitNow(self)
struct celview *self;
{
    if(self->child) return;
    self->mode = INITNOW;
    cel_SetRefName(Cel(self),"");
    self->NeedsRemade  = self->NeedsReinit = TRUE;
    celview_WantUpdate(self,self);
}
void celview__Update(self)
struct celview *self;
{
    struct cel *vr = Cel(self);
    if(self->mode == celview_NoUpdate) return;
    if(self->mode == celview_DoFull ){
	self->mode = celview_HasView;
	celview_FullUpdate(self,view_FullRedraw,0,0,0,0);
/*	view_InsertView(self->child, self, self->olist->rect);
	view_FullUpdate(self->child,view_FullRedraw,0,0,0,0);
	view_WantInputFocus(self->child,self->child);
	UpdateCursors(self);
	UpdateDrawing(self); */
	return;
    }
    if(self->mode == celview_UpdateView){
	self->mode = celview_HasView;
	celview_FullUpdate(self,view_FullRedraw,0,0,0,0);
	return;
    }
/*    if(self->mode == INITNOW){ */
    if(self->NeedsRemade  && self->mode == INITNOW  ){
	self->mode = celview_NoUpdate;
	if((self->arb == NULL && self->TopLevel == FALSE )|| self->NeedsReinit){ 
	    arbcon_InitCel(self,self->arb);
	}
#ifdef DEBUG
	printf(" vt = %s (%d)\n",vr->viewType,vr->dataObject);
#endif /* DEBUG */
 	if( celview_PromptForInfo(self,self->arb,TRUE,FALSE)== -1){
	    self->NeedsReinit = TRUE;
/*	    if(Parent(self)) view_WantInputFocus(Parent(self),Parent(self)); */
	    return; 
	}
#ifdef DEBUG
	printf(" In UPDATE vr->application === %d (%d)\n",vr->application,VALUE);
#endif /* DEBUG */

	if(vr->application == VALUE) self->promptforparameters = 1;
    }
    if(self->child == NULL || self->NeedsRemade) {
	if(celview_makeview(self,vr) )  
	    cel_NotifyObservers(vr,0);
 	if(self->child != NULL) {
	    celview_SetTransferMode(self,graphic_WHITE);
	    celview_ClearClippingRect(self);
	    celview_SetTransferMode(self,graphic_BLACK);
	    super_WantNewSize(self,self); 
	    self->sizepending = TRUE;
	    celview_FullUpdate(self,view_FullRedraw,0,0,0,0); /* ??? */
	    self->NeedsRemade = FALSE;
	}
	else {
	    UpdateDrawing(self);
	}
	return;

    }
    if(Cel(self) && self->desw != Cel(self)->desw || self->desh != Cel(self)->desh){
	self->desw = Cel(self)->desw ;
	self->desh = Cel(self)->desh ;
	if(!self->sizepending) {
	    super_WantNewSize(self,self); 
	    self->sizepending = TRUE;
	}
    }
    if(self->vismode == cel_INVISIBLE) return;
    if(self->child){
	if(self->NeedsPost){
	    if(self->NeedsPost){ 
		PostParameters(self);
		valueview_LookupParameters((struct valueview *)self->truechild);
	    }
	    view_FullUpdate(self->child,view_FullRedraw,0,0,0,0);
	}
	else view_Update(self->child);
    }
    UpdateCursors(self);
    UpdateDrawing(self);

}
#define OFFSET 5
static drawshadow(self,r)
struct celview *self;
struct rectangle *r;
{
    celview_SetTransferMode(self,graphic_INVERT);
    celview_FillRectSize(self,r->left + OFFSET,r->top + r->height,r->width,OFFSET,celview_GrayPattern(self,8,16));
    celview_FillRectSize(self,r->left + r->width,r->top + OFFSET,OFFSET,r->height - OFFSET,celview_GrayPattern(self,8,16));
    celview_SetTransferMode(self,graphic_BLACK);
    celview_DrawRect(self,r);
}
void celview__FullUpdate(self,type,left,top,width,height)
struct celview *self;
enum view_UpdateType type;
long left,top,width,height;
{
    struct rectangle currec,tmprec;
    struct overlay *ov,*lastov;
    currec = self->enclosingRect;
    if(self->mode == celview_NoUpdate) return;
    if(cel_GetReadCount(Cel(self)) == 0 &&
	(cel_GetDefaultStream(Cel(self)) != NULL ||
	 cel_GetInitFile(Cel(self)) != NULL)){
	cel_InitDefault(Cel(self));
    }
    if(self->arb == NULL && !self->TopLevel &&
	(self->arb = arbiterview_FindArb(self->header.view.parent)) != NULL){
	arbiterview_InitCell(self->arb,self);
    }
    if(type == view_FullRedraw && Cel(self) && self->desw != Cel(self)->desw || self->desh != Cel(self)->desh){
	self->desw = Cel(self)->desw ;
	self->desh = Cel(self)->desh ;
	if(!self->sizepending) {
	    super_WantNewSize(self,self); 
	    self->sizepending = TRUE;
	}
	return; /* Might as well, since our parent will be redrawing us again */
    }
    self->enclosingRect.top = 0; self->enclosingRect.left = 0;
    self->enclosingRect.width  = celview_GetLogicalWidth(self) ;
    self->enclosingRect.height = celview_GetLogicalHeight(self) ;
/*    if(DOINDENT(self)){ */
	(self->enclosingRect.width)--;
	(self->enclosingRect.height)--; 
/*    } */
	if(type != view_Remove) {
	    UpdateCursors(self);
	    if(type != view_MoveNoRedraw){
		celview_SetTransferMode(self,graphic_WHITE);
		celview_EraseRect(self,&(self->enclosingRect));
		celview_SetTransferMode(self,graphic_INVERT);

		if(self->drawing){ 
		    celview_DrawRect(self,&(self->enclosingRect));
		} 
	    }
	}
 /*   UpdateDrawing(self); */
    self->OldMode = self->drawing;
/*    self->childRect = self->enclosingRect; */
    celview_GetLogicalBounds(self,&(self->childRect));
    if(DOINDENT(self)){
	self->childRect.top++; self->childRect.left++;
	self->childRect.width--  ;
	self->childRect.height-- ;
	self->childRect.width--  ;
	self->childRect.height-- ;
	top--; left--;
    }
    if(self->child) {
	struct view *child;
	child = (self->olist != NULL) ? self->safec : self->child;
	view_InsertView(child, self, &self->childRect);
	celview_RetractViewCursors(self,child);
	if(self->vismode != cel_INVISIBLE && (self->olist == NULL || !(self->olist->flags & celview_COVERCHILD))){
	    if(self->NeedsPost) PostParameters(self);
	    if(self->mode == celview_FirstUpdate) 
		self->mode = celview_HasView;
	    view_FullUpdate(self->child,type,left,top,width,height);
	}
    }
    else celview_WantUpdate(self,self);
    ov = self->olist;
    lastov = NULL;
    while(ov != NULL){
	for(ov = self->olist;  ov->next != lastov ; ov = ov->next);
	scaleoverlay(self,ov,&currec);
	view_InsertView(ov->view, self, &ov->rect);
	celview_RetractViewCursors(self,ov->view);
	view_FullUpdate(ov->view,view_FullRedraw,0,0,0,0);
	celview_SetClippingRect(self,&(self->enclosingRect));
	tmprec = ov->rect;
	if(tmprec.top < 0) tmprec.top--;
	if(tmprec.left < 0) tmprec.left--;
	tmprec.height++; tmprec.width++;
	tmprec.height++; tmprec.width++;
	drawshadow(self,&tmprec);
	if(ov == self->olist) break;
	lastov = ov;
    }
    if(ov && ov->view){
	view_WantInputFocus(ov->view,ov->view);
    }
#ifdef DEBUG
    fprintf(stdout,"Out of FullUpdate, NP = %s self->script = %d %d\n",self->NeedsPost?"TRUE":"FALSE",self->script,(self->script)?text_GetLength(self->script):0);fflush(stdout);
#endif
}


void celview__SetDataObject(self,dd)
struct celview *self;
struct cel *dd;
{
    self->desw = dd->desw;
    self->desh = dd->desh;
    self->vismode = dd->mode;
    super_SetDataObject(self,dd);
    self->NeedsRemade = TRUE;
    if(dd->application == cel_VALUE) self->NeedsPost = TRUE;
}
boolean celview__InitializeObject(classID,self)
struct classheader *classID;
struct celview *self;
{
    self->widthcursor = cursor_Create(self);
    self->heightcursor = cursor_Create(self);
    cursor_SetStandard(self->heightcursor,Cursor_HorizontalBars);
    cursor_SetStandard(self->widthcursor,Cursor_VerticalBars);
    self->Moving = 0; 
    self->WasMoving = 0;
    self->resizing = RESIZING;  
    self->WasResizing = 0;
    self->child = self->truechild = NULL;
    self->desw = self->desh = UNSET;
    self->menus = menulist_DuplicateML(celviewMenus, self);
    self->drawing = DRAWING;
    self->sizepending = TRUE;
    self->arb = NULL;
    self->promptforparameters = 0;
    self->NeedsPost = FALSE;
    self->vismode = cel_VISIBLE;
    self->NeedsRemade = FALSE;
    self->viewatm = self->dataatm = self->refatm = NULL;
    self->application = -100;
    self->NeedsReinit = FALSE;
    self->hitfunc = NULL;
    self->hitrock = 0;
    self->menulist = NULL;
    self->keymap = NULL;
    self->keystate = NULL;
    self->HasFocus = FALSE;
    self->TopLevel = FALSE;
    self->olist = NULL;
    self->rarb = NULL;
    self->AddAppLayer = FALSE;
    self->script = NULL;
    menulist_SetMask(self->menus,1);
    return TRUE;
}
void celview__SetResizing(self,key)
struct celview *self;
long key;
{
    if(self->resizing == 0){
	self->resizing = 1;
    }
    else {
	self->resizing = 0;
	self->WasResizing = 1;
    }
    if(self->mode == celview_HasView)
	self->mode = celview_DoFull;
    celview_WantUpdate(self,self);
}
void celview__SetDrawing(self,key)
struct celview *self;
long key;
{
    self->drawing = !self->drawing;
    celview_WantUpdate(self,self);
}
void celview__PostMenus(self, menulist)
struct celview *self;
struct menulist *menulist;
{
    menulist_UnchainML(self->menus,ChildMenus);
    if(menulist != NULL && menulist != self->menus)
	menulist_ChainAfterML(self->menus,menulist,ChildMenus);
    super_PostMenus(self, self->menus);
}
void celview__PostKeyState(self,keystate)
struct celview *self;
struct keystate *keystate;
{
    struct keystate *cur = keystate;
    if(self->keystate != NULL && self->keystate->orgMap != self->keymap){
	keystate_Reset(self->keystate);
	keystate_Destroy(self->keystate);
	self->keystate = NULL;
    }
    if(self->keymap != NULL && self->keystate == NULL)
	self->keystate = keystate_Create(self,self->keymap);

    if(self->keystate){
	self->keystate->next = NULL;
	keystate_Reset(self->keystate);
	cur = keystate_AddBefore(self->keystate, cur);
    }
    super_PostKeyState(self, cur);
}
void celview__WantNewSize(self,requestor)
    struct celview *self;
struct view *requestor;
{
    if(Cel(self)){
	if(self->vismode == cel_INVISIBLE && requestor != (struct view *)self) return;
	if((Cel(self)->desw != UNSET ||  Cel(self)->desh != UNSET)){
	    Cel(self)->desw = Cel(self)->desh = UNSET;
	    self->desw = self->desh = UNSET;
	    cel_NotifyObservers(Cel(self),0);
	}
    }
    if(!self->sizepending) {
	super_WantNewSize(self,self);
	self->sizepending = TRUE;
    }
}

static struct bind_Description celviewBindings[]={
    {"celview-Init-Child",NULL,0,"celview,Init Child",0,1,InitNow,"Initialize Child" },
    {"celview-Paste",NULL,0,"celview,Paste",0,1,celview_Paste,"Paste Child" },
    {"celview-InsertFile",NULL,0,"celview,Insert File",0,1,celview_PromptForFile,"Read Child" },
NULL
};
static SetVisible(self)
struct celview *self;
{
    if(Cel(self))
	cel_SetVisible(Cel(self));
}
static SetInvisible(self)
struct celview *self;
{
    if(Cel(self))
	cel_SetInvisible(Cel(self));
}

boolean celview__InitializeClass(classID)
    struct classheader *classID;
{
    celviewMenus = menulist_New();
    celviewKeyMap =  keymap_New();
    bind_BindList(celviewBindings, celviewKeyMap , celviewMenus, &celview_classinfo);
    proctable_DefineProc("celview-set-visible", SetVisible,&celview_classinfo,NULL, "Make celview visible");
    proctable_DefineProc("celview-set-invisible", SetInvisible,&celview_classinfo,NULL, "Make celview invisible");
   UNSETFLAG = atom_Intern("XXXUNDEFINEDXXX");
    return TRUE;
}

static objecttest(self,name,desiredname)
register struct celview  *self;
char *name,*desiredname;
{
    if(class_Load(name) == NULL){
        char foo[640];
        sprintf(foo,"Can't load %s",name);
         message_DisplayString(self, 0, foo);
        return(FALSE);
    }
    if(! class_IsTypeByName(name,desiredname)){
        char foo[640];
        sprintf(foo,"%s is not a %s",name,desiredname);
         message_DisplayString(self, 0, foo);
        return(FALSE);
    }
    return(TRUE);
}
struct types {
    char *str;
    int val;
};
static struct types typearray[] = {
    {"string",STRING},
    {"char *",STRING},
    {"int",LONG},
    {"long",LONG},
    {"float",FLOAT},
    {"",0}
};

static lookuptype(ty)
char *ty;
{
    struct types *tp;
    for(tp = typearray;tp->val != 0; tp++)
	if(*ty == *tp->str && strcmp(ty,tp->str) == 0)
	    return tp->val;
    return 0;
}

static char * atomlisttostring(al)
struct atomlist *al;
/* should be an atomlist call */
{
    static char buf[512];
    char *cp,*alcp;
    struct atoms *at;
    cp = buf;
    for(at = atomlist_TraversalStart(al); at != NULL; at = atomlist_TraversalNext(al,at)){
#ifdef DEBUG
printf("--> %s\n",at->atom->name);
#endif /* DEBUG */
	for(alcp = at->atom->name; *alcp; alcp++)
	    *cp++ = *alcp;
	*cp++ = '.';
    }
    if(cp > buf) cp--;
    *cp = '\0';
#ifdef DEBUG
printf("returning> %s\n",buf);
#endif /* DEBUG */
    return buf;
}   
int celview__ResourceListToString(classID,str,rl)
struct classheader *classID;
char *str;
struct resourceList *rl;
{
#ifdef DEBUG
printf("In RLtoS found = %d %s\n",rl->found,rl->type->name);
#endif /* DEBUG */
    if(!rl->found){
	char *res;
	res = atomlisttostring(rl->name);
#ifdef DEBUG
printf("In RLtoS Not found = %d %s %s\n",rl->found,rl->type->name,res);
#endif /* DEBUG */

	sprintf(str,"[%s] <%s> ()",rl->type->name,res);
#ifdef DEBUG
printf("out of sprintf\n");
#endif /* DEBUG */
    }
    else switch(lookuptype(rl->type->name)){
	case STRING:
	    sprintf(str,"[%s] <%s> (%s)",rl->type->name,atomlisttostring(rl->name),
		    (char *) rl->data);
	    break;
	case FLOAT:
	    sprintf(str,"[%s] <%s> (%f)",rl->type->name,atomlisttostring(rl->name),
		    (float) rl->data);
	    break;
	case LONG:
	    sprintf(str,"[%s] <%s> (%d)",rl->type->name,atomlisttostring(rl->name),
		    rl->data);
	    break;
	default:   
	    sprintf(str,"Unknown Type %s", rl->type->name);
	    return FALSE;
    }
#ifdef DEBUG
printf("leaving RLtoS\n");
#endif /* DEBUG */
return TRUE;
}
void celview__GetManyParameters( self, resources, name, class )
struct celview * self;
struct resourceList * resources;
struct atomlist * name;
struct atomlist * class;
{
    int x;
    super_GetManyParameters(self, resources, name, class ); 
    for (x = 0; resources[x].name != NULL; ++x){
	if(self->NeedsPost || resources[x].data == (long) UNSETFLAG){
	    resources[x].found = FALSE;
	}
    }
#ifdef DEBUG
	fprintf(stdout,"get-many mis match %d %d (%d)\n",self->level,x,self->script);fflush(stdout);
#endif

    if(self->script != NULL && self->level <  x ){
/*	fprintf(stdout,"get-many mis match %d %d \n",self->level,x);fflush(stdout);*/
	/*  # of RESOURCES requested != # of resources stored in cel */
	appendresourceList( self, resources);
    }
}
static appendresourceList( self, resources)
struct celview * self;
struct resourceList * resources;
{   /* append new entries onto the cels text */
    char *buf,tbuf[1024],*obp,*cp,*el;
    struct resourceList rl;
    long len,x;
    boolean fnd[64];
    for(x = 0;x < 64;  x++) fnd[x] = FALSE;
    if(self->script != NULL ){
	len = text_GetLength(self->script);
	if(len > 0){
	    buf = malloc(len + 1);
	    text_CopySubString(self->script,0,len,buf,FALSE);
	    for(cp = buf; cp < (buf + len ); cp = el + 1){
		if((el = strchr(cp,'\n')) == NULL){
		    break;
		}
		*el = '\0';
		if(el == cp){
		    continue;
		}
		if((StringToResourceList(&rl,cp)) == TRUE){
		    strcpy(tbuf,atomlisttostring(rl.name));
		    for (x = 0; resources[x].name != NULL; ++x){
			/* not a very efficient test, but this code is rarely called */
			if(strcmp(tbuf,atomlisttostring(resources[x].name)) == 0){
			   /* fprintf(stdout,"found %s\n", tbuf);fflush(stdout);*/
			    fnd[x] = TRUE;
			    break;
			}
		    }
		}
	    }
	    for (x = 0; resources[x].name != NULL; ++x){
		if(fnd[x] == FALSE){
/*		    fprintf(stdout,"adding  %s\n", atomlisttostring(resources[x].name));fflush(stdout);*/
		    celview_ResourceListToString(tbuf,&resources[x]);
		    cp = strrchr(tbuf,'(');
		    sprintf(cp,"()\n");
		    self->level++;
		    text_InsertCharacters(self->script,0,tbuf,strlen(tbuf));
		}
	    }
	    free(buf);
	}
    }	

}
static editresourceList( self, resources,askres,maxcount )
struct celview * self;
struct resourceList * resources;
int askres,maxcount;
{
    struct resourceList *rl;
    char buf[1024],iname[512],*cp;
    int pf;
/*fprintf(stdout,"In edit res %d\n",maxcount);fflush(stdout);*/
    for(rl = resources; (rl->name != NULL) &&( maxcount-- >= 0); rl++){
	celview_ResourceListToString(buf,rl);
#ifdef DEBUG
	printf("RLTS returned %s\n",buf);
#endif /* DEBUG */
	if(askres){
	    pf = message_AskForString(NULL, 0, buf, 0, iname, sizeof(iname));
	    if (pf < 0 ) continue;
	    if(strlen(iname ) > 0 ){
		cp = strrchr(buf,'(');
		sprintf(cp,"(%s)\n",iname);
	    }
	    else strcat(buf,"\n");
	}
	else {
	    cp = strrchr(buf,'(');
	    sprintf(cp,"()\n");
/*	    strcat(buf,"\n"); */
	}
#ifdef DEBUG
	printf(">> inserting %s",buf);
#endif /* DEBUG */
	text_InsertCharacters(self->script,0,buf,strlen(buf));
    }
}

#define SKIPTO(SRC,C,PT) for(PT = SRC; *PT != C ; PT++) if(*PT == '\0') return FALSE
#define SKIPTOSET(SRC,C,PT,BB) for(PT = SRC; *PT != C ; *BB++ = *PT++) if(*PT == '\0') return FALSE
#define PULLOUT(S1,S2,C1,C2,BUF)     tmp = BUF; *tmp = '\0';\
    SKIPTO(S2,C1,S1); \
    S1++;\
    SKIPTOSET(S1,C2,S2,tmp);\
    *S2++ ; *tmp = '\0'

static boolean StringToResourceList(rl,str)
struct resourceList *rl;
char *str;
{
    char buf[512], *tmp;
    char *start,*end,*cp;
    end = str;
    PULLOUT(start,end,'[',']',buf);
    rl->type = atom_Intern(buf);
    PULLOUT(start,end,'<','>',buf);
    rl->name = atomlist_StringToAtomlist(buf);
    PULLOUT(start,end,'(',')',buf);

    if(*buf == '\0') {
	rl->data = (long) UNSETFLAG;
	return TRUE;
    }
    
    switch(lookuptype(atom_Name(rl->type))){
	case LONG:
	    for(tmp = buf; !isdigit(*tmp); tmp++)
		if(*tmp == '\0' || *tmp == '-')break;
	    if(*tmp == '\0') rl->data =  (long) UNSETFLAG;
	    else rl->data = atol(tmp);
	    break;
	case FLOAT:
	    for(tmp = buf; !isdigit(*tmp); tmp++)
		if(*tmp == '\0'|| *tmp == '-' || *tmp == '.')break;
	    if(*tmp == '\0') rl->data =  (long) UNSETFLAG;
	    else rl->data = (long) atof(tmp);
	    break;
	case STRING:
	    rl->data = (long) atom_Name(atom_Intern(buf));
	    break;
	default:
	    return FALSE;
    }
    return TRUE;
}
static GetParameters(self)
struct celview *self;
{
    struct valueview *wv = (struct valueview *)self->truechild;
    struct resourceList *resources;
    struct cel *ls = Data(self);
    if(ls->script != NULL){
	self->script = ls->script;
	text_Clear(self->script);
    }
    else self->script = text_New();
    self->promptforparameters = 2;
#ifdef DEBUG
printf("entering lookup\n");
#endif /* DEBUG */
    resources = valueview_GetDesiredParameters(wv);
if(self->arb){
    editresourceList( self, resources,FALSE,100 );
}
    valueview_LookupParameters(wv); /* This should call GetManyParameters above */
#ifdef DEBUG
printf("exiting lookup\n");
#endif /* DEBUG */
    self->promptforparameters = 0;
    if(text_GetLength(self->script) == 0){
	if(self->script != ls->script) text_Destroy(self->script);
	self->script = NULL;
#ifdef DEBUG
printf("script is EMPTY\n");
#endif /* DEBUG */
	return;
    }
    ls->script = self->script;
    if(self->arb){
	arbcon_SetCurrentCelview(self);
	arbcon_EditCurrentCelview();
    }
#ifdef DEBUG
printf("setting ls->script %d\n",text_GetLength(ls->script));
#endif /* DEBUG */

}
void celview__PostParameters(self)
struct celview *self;
{
self->NeedsPost = TRUE;
celview_WantUpdate(self,self);
}
/* #define DEBUG  */
static PostParameters(self)
struct celview *self;
{
    int len;
    struct resourceList rl;
    char *buf,*cp,*el;
    struct cel *ls = Data(self);
    if(celview_WantHandler(self,"message") == NULL){
	/* tree node linked yet */
	return;
    }
#ifdef DEBUG
printf("In Postparametes %d\n",(ls->script == NULL)? -1:text_GetLength(ls->script));
#endif /* DEBUG */
    if(ls->script != NULL ){
	self->level = 0;
	len = text_GetLength(ls->script);
	if(len > 0){
	    buf = malloc(len + 1);
	    text_CopySubString(ls->script,0,len,buf,FALSE);
	    for(cp = buf; cp < (buf + len ); cp = el + 1){
		if((el = strchr(cp,'\n')) == NULL){
		    break;
		}
		*el = '\0';
		if(el == cp){
		    continue;
		}
		self->level++;
#ifdef DEBUG
		printf("Calling STRL w/ %s\n",cp);
#endif /* DEBUG */
		if((StringToResourceList(&rl,cp)) == TRUE){
		    view_PostResource(self->truechild,rl.name,rl.type,rl.data);
#ifdef SHOULDNOTDEF
fprintf(stdout,"Posting something \n"); fflush(stdout);
		    celview_PostResource(self,rl.name,rl.type,rl.data); 
#endif
		}
	    }
	    free(buf);
	    self->script = ls->script;
	}
    }	
    self->NeedsPost = 0;
}
int celview__PromptForInfo(self,arb,promptForViewName,changeRefName)
struct celview *self;
struct arbiterview *arb;
boolean promptForViewName,changeRefName;
{

    char iname[100],qz[64], *prompt;
    char viewname[200],refname[256];
    long pf;	
    struct cel *ls = Cel(self);
    if(ls == NULL){
	printf("Cel Has No Dataobject\n");
	return -1;
    }
 /*   im_ForceUpdate(); */
    viewname[0] = '\0'; iname[0] = '\0';
    if(ls->dataObject == NULL){
	pf = message_AskForString(NULL, 0, "Data object to insert here: ", 0, iname, sizeof(iname));
	if (pf < 0) {
	    return -1 ;
	}
	if(strlen(iname)==0)  {
	    promptForViewName = TRUE;
	} 
	else if (strcmp(iname,"value") == 0){
	    promptForViewName =	TRUE;	
	}
	else if(objecttest(self,iname,"dataobject") == FALSE) return -1;
	cel_SetObjectByName(ls,iname);
    }
    prompt = "View to place here (return for default)? ";
    while((ls->viewType == NULL || *(ls->viewType) == '\0') &&  promptForViewName){
	if( message_AskForString (NULL, 0, prompt, 0, viewname, 200) < 0)
	    {
	     return -1;
	}
	if(strlen(viewname) == 0)
	    cel_SetViewName(ls,NULL,TRUE);
	else if(objecttest(self,viewname,"view")) 
	    cel_SetViewName(ls,viewname,FALSE);
	prompt = "Invalid View! View to place here (return for default)? ";
    }
    *refname = '\0';
    if((((ls->refname == NULL || *(ls->refname) == '\0') && self->TopLevel == FALSE)|| changeRefName)) {
	struct atomlist *atm;
#ifdef DEBUG
printf("calling for name , arb = %d\n",arb);
#endif /* DEBUG */
	if( message_AskForString (NULL, 0, "Name for reference ", 0, refname, 200) < 0){
	    return -1;
	}
#ifdef DEBUG
printf("name is %s\n",refname);
#endif /* DEBUG */
	if(arb){
	    while((arbiterview_registername(arb,self,refname)) == FALSE){
		*qz = 'y';
		if( message_AskForString (NULL, 0, "That name is taken.Do you wish another view on that object?[y] ", 0,qz , 64) < 0){
		    return -1;
		}
		if(*qz == 'n' ||  *qz == 'N') {
		    if( message_AskForString (NULL, 0, "New name for reference ", 0, refname, 200) < 0){
			  return -1;
		    }
		}
		else break;
	    }
	}
	else cel_SetRefName(ls,refname);
	atm = atomlist_StringToAtomlist(refname);
	celview_SetName(self,atm); 
    }
    if(strcmp(ls->dataType,"value") == 0) 
	cel_SetApplication(ls,cel_VALUE);
    else if(ls->application == cel_NOTSET){
	*refname = '\0';
	if( message_AskForString (NULL, 0, "Add Application Layer (no)", 0, refname, 200) < 0){
	    return -1;
	}
	if(*refname == 'y' || *refname == 'Y') cel_SetApplication(ls,cel_APPLICATION);
    }
    return 1;
}
void celview__LinkTree(self, parent)
register struct celview *self;
struct view *parent;
{
    struct arbiterview *ab;
    int named = FALSE;
    super_LinkTree(self, parent);
    if(self->child){
	view_LinkTree(self->child,self);
    }
    if(parent == NULL){
	if(self->arb != NULL) {
	    arbiterview_DeleteCell(self->arb,self);
	    self->arb = NULL;
	}
	return;
    }
    if(self->TopLevel || strcmp(class_GetTypeName(parent),"frame") == 0){
	ab = NULL;
	self->TopLevel = TRUE;
	if(Cel(self)) {
	    cel_SetApplication(Cel(self),cel_APPLICATION);
	    cel_SetRefName(Cel(self),"");
	    cel_SetViewName(Cel(self),NULL,TRUE);
	}
    }
    else{
	if(self->rarb) ab = self->rarb;
	else ab =arbiterview_FindArb(parent);
    }

    if(Cel(self)  != NULL && Cel(self)->refname != NULL) named = TRUE;
    if(self->arb == NULL){
	if((self->arb = ab) != NULL)
	    arbiterview_InitCell(self->arb,self);
    }
    else {
	if(self->arb == ab) /* arbiterview_ReInitCell(self->arb,self) Necessary ? */ ;
	else {
	    arbiterview_DeleteCell(self->arb,self);
	    self->arb = ab;
	    if(self->arb) arbiterview_InitCell(self->arb,self);
	}
    }
    if(self->child == NULL && Cel(self) != NULL ){
	if( named ){
	    initchild(self); 
	}
    }
}
void celview__Copy (self)
    register struct celview *self;
{
    FILE *cutFile;
    cutFile = im_ToCutBuffer(celview_GetIM(self));
    cel_Write(Cel(self),cutFile,im_GetWriteID(),0);

    im_CloseToCutBuffer(celview_GetIM(self), cutFile);
}


void celview__FinalizeObject(classID, self)
struct classheader *classID;
struct celview *self;
{
    if(self->olist){
	struct overlay *ov; 
	struct view *vw[128];
	int i;
	for(i = 0,ov = self->olist;  ov != NULL ; ov = ov->next){
	    vw[i++] = ov->view;
	    if(i > 128) break;
	}
	while(i--){
	    view_RemoveObserver(vw[i],self);
	    PopOverlay(self,vw[i]);
	}
    }
    if(self->truechild){
	struct view *child;
	child = self->truechild;
	view_UnlinkTree(child);
	self->child = NULL;
	view_RemoveObserver(child,self);
	view_Destroy(child);
    }
    if(self->menus) menulist_Destroy(self->menus);
}
void celview__InitChildren(self)
struct celview *self;
{
    if(self->child == NULL || self->NeedsRemade) {
	celview_makeview(self,Cel(self));
       }
    if(self->child) 
	view_InitChildren(self->child);
	
}
boolean celview__CanView(self,TypeName)
struct celview *self;
char *TypeName;
{
    return class_IsTypeByName(TypeName,"cel");
}
void celview__SetHitfunc(self,hitfunc,hitrock)
struct celview *self;
struct view * (*hitfunc)();
long hitrock;
{
    self->hitfunc = hitfunc;
    self->hitrock = hitrock;
}
void celview__SetKeymap(self,km)
struct celview *self;
struct keymap *km;
{
    self->keymap = km;
 }

void celview__SetMenulist(self,ml)
struct celview *self;
struct menulist *ml;
{
    menulist_UnchainML(self->menus,ClientMenus);
    if(ml != NULL){
	menulist_ChainBeforeML(self->menus,ml,ClientMenus);
	menulist_SetView(ml,self);
    }
}

void celview__Repost(self)
struct celview *self;
{
    /* question, how to force child to repost it's menus and keystate 
       so that we can add ours?
        answer: If the child does not have the input focus, don't worry
      about it since the posting will happen when it gets it.
      If the child does have the input focus, we request it and then 
      (in celview_ReceiveInputFocus) we request it back for the child,
	  thus getting the post requests we want.
	  */
    struct im *im = celview_GetIM(self);
    struct view *foc;
    if(self->child && im){
	foc = im_GetInputFocus(im);
	if(self->truechild == foc || self->child == foc )
	    celview_WantInputFocus(self,self);
    }
}
void celview__PushOverlay(self,view,rect,flags)
struct celview *self;
struct view *view;
struct rectangle *rect;
long flags;
{
    struct overlay *ov;
    ov = (struct overlay *) malloc(sizeof(struct overlay));
    ov->view = view;
    if(rect) ov->rect = *rect;
    ov->flags = flags;
    ov->next = self->olist;
    if(self->olist == NULL){
	self->safec = self->child ;
	self->safetc = self->truechild ;
    }
    self->child = self->truechild = view;
    self->olist = ov;
    self->mode = celview_DoFull;
    view_AddObserver(view,self); 
    celview_WantUpdate(self,self);
}
struct view *celview__PopOverlay(self,view)
struct celview *self;
struct view *view;
{
if((view = PopOverlay(self,view)) != NULL) view_RemoveObserver(view,self);
return view;
}
void celview__WantUpdate(self,requestor)
struct celview *self;
struct view *requestor;
{
    register struct view *view;
    if(self->olist == NULL || requestor == (struct view *)self){
	super_WantUpdate(self,requestor);
	return;
    }
    /* don't process updates for children that don't belong to the current overlay */
    for(view = requestor; (view != (struct view *) self) && view != NULL; view = view->parent)
	if(view == self->child)
	    super_WantUpdate(self,requestor);

}
void celview__PostCursor(self,rec,c)
struct celview *self;
struct rectangle *rec;
struct cursor *c;
{
    register struct view *view = c->view;
    if(self->olist == NULL || view == (struct view *)self){
	super_PostCursor(self,rec,c);
	return;
    }
    /* don't process posts for children that don't belong to the current overlay */
    for(; (view != (struct view *) self) && view != NULL; view = view->parent)
	if(view == self->child)
	    super_PostCursor(self,rec,c);

}
void celview__ObservedChanged(self, changed, value)
struct celview *self;
struct observable *changed;
long value;
{
    if(changed == (struct observable *)Cel(self)) 
    {
	struct cel *c = Cel(self);
	struct view *parent = ((struct view *) self)->parent;
	if (value == observable_OBJECTDESTROYED){
/* NO LONGER assumes the parent is also observing the cel and will destroy the celview */
	    if(parent == NULL) self->truechild = NULL;
	    else celview_UnlinkTree(self);
	    super_ObservedChanged(self,changed,value); /* sets dataobject to NULL, so remove observer 
							won't be called when the view is destroyed */
	    celview_Destroy(self);
	    return;
	}
	else if(parent == NULL)
	    return;
	else if(value == cel_NeedsRepost){
	    self->NeedsPost = TRUE;
	    celview_WantUpdate(self,self);
	}
	else if(self->viewatm != cel_GetViewAtom(c) || self->dataatm != cel_GetObjectAtom(c) || self->refatm != cel_GetRefAtom(c) || self->application != cel_GetApplication(c)){
	    self->NeedsRemade = TRUE;
	    celview_WantUpdate(self,self);
	}
	else if(self->vismode != Cel(self)->mode) {
	    self->vismode = Cel(self)->mode;
	    celview_WantNewSize(self,self);
	}
	else if (Cel(self) && self->desw != Cel(self)->desw || self->desh != Cel(self)->desh) 
	    celview_WantUpdate(self,self);

    }
    if(value ==  observable_OBJECTDESTROYED ){
	if(self->olist){
	    if(changed == (struct observable *) PopOverlay(self,(struct view *)changed))
		  return;
	}
	else{
	    if(changed == (struct observable *)self->child ||
	       changed == (struct observable *)self->truechild ||
	       changed == (struct observable *)self->safec ||
	       changed == (struct observable *)self->safetc
	       ){
		self->child = NULL;
		self->truechild = NULL;
		celview_UnlinkTree(self);
		celview_Destroy(self); 
	    }
	}
    }
}

struct view *celview__Hit(self,action,mousex,mousey,numberOfClicks) 
struct celview *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    struct view *vw;
    static boolean lasthit;
    lasthit = TRUE;
    if(Cel(self) == NULL )
	return (struct view *)self;
    if(self->olist && (view_EnclosedXToLocalX(self->child, mousex) < 0 || 
	view_EnclosedYToLocalY(self->child, mousey) < 0)){
	if((vw = PopOverlay(self,NULL)) != NULL){
	    view_RemoveObserver(vw,self);
	    view_Destroy(vw);
	}
	return (struct view *) self;
    }

    if( self->vismode == cel_INVISIBLE){
	if (arbiterview_GetCelMode(self->arb))
	    arbcon_SetCurrentCelview(self);
	return (struct view *)self;
    }
    if((self->child && !self->resizing) )	{
	vw = NULL;
	if(self->hitfunc){
	    vw = (*(self->hitfunc))
	      (self,action,
	       view_EnclosedXToLocalX(self->child, mousex),
	       view_EnclosedYToLocalY(self->child, mousey),
	       numberOfClicks,self->hitrock);
	}
	if(vw == NULL)
	    vw = view_Hit(self->child,action,view_EnclosedXToLocalX(self->child, mousex), view_EnclosedYToLocalY(self->child, mousey),numberOfClicks);
	if(self->arb && lasthit ){
	    lasthit = FALSE;
	    if(arbiterview_GetCopyMode(self->arb)){ 
		char buf[256];
		arbcon_SetCurrentCelview(self);
		celview_Copy(self);
		sprintf(buf,"copying %s",cel_GetRefName(Cel(self)));
		message_DisplayString(self,0,buf);
	    }
	    else if (arbiterview_GetCelMode(self->arb))
		arbcon_SetCurrentCelview(self);
	}
	return vw;
    }
    if(self->child == NULL) {
	celview_WantInputFocus(self,self);
    }
    switch(action){
	case view_LeftDown:
	case view_RightDown:
	    {
	    long width = celview_GetLogicalRight(self);
	    long height = celview_GetLogicalBottom(self);
	    if(width - FUDGE < mousex) self->Moving = celview_ChangeWidth;
	    else if(height - FUDGE < mousey) self->Moving = celview_ChangeHeight;
#if 0
	    else if(self->child) {
		vw = view_Hit(self->child,action,view_EnclosedXToLocalX(self->child, mousex), view_EnclosedYToLocalY(self->child, mousey),numberOfClicks);
		if(self->arb && (vw == self->truechild || vw == self->child ))
		    arbcon_SetCurrentCelview(self);
		return vw;
	    }
#endif /* 0 */
	    else break;
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
		if(move == celview_ChangeWidth && mousex > 0) {
			Cel(self)->desw = mousex + 1;
			cel_NotifyObservers(Cel(self),0);
		}
		else if(move == celview_ChangeHeight && mousey > 0) {
			Cel(self)->desh = mousey + 1;
			cel_NotifyObservers(Cel(self),0);
		}
		return (struct view *) self;
	    }
    }
    if(self->Moving || self->child == NULL) return (struct view *) self;
    return NULL;
}
struct basicobject * celview__WantHandler(self, handlerName)
struct celview *self;
char *handlerName;
{
    if(strcmp(handlerName,"arbiterview") == 0 && self->arb) return (struct basicobject *)self->arb;
    return super_WantHandler(self, handlerName);
}

struct celview *celview__GetCelviewFromView(classID,v)
struct classheader *classID;
struct view *v;
{
    if(v == NULL) return NULL;
    for( v = v->parent;v != NULL; v = v->parent){
	if(class_IsTypeByName(class_GetTypeName(v),"celview"))
	    return((struct celview *) v);
    }
    return NULL;
}
 void celview__GetOrigin(self, width, height, originX, originY)
    struct celview *self;
    long width;
    long height;
    long *originX;
    long *originY;
{
    if(self->child) view_GetOrigin(self->child, width, height, originX, originY);
    else super_GetOrigin(self, width, height, originX, originY);
}
