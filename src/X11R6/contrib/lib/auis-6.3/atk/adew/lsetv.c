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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/lsetv.c,v 2.16 1994/04/17 21:52:46 rr2b Exp $";
#endif


 


#include <andrewos.h> /* strings.h */
#include <class.h>
#include <lsetv.eh>
#include <lset.ih>
#include <view.ih>
#include <cursor.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <message.ih>
#include <im.ih>
#include <graphic.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <filetype.ih>
#include <dataobj.ih>
#include <atom.ih>
#include <atomlist.ih>
#include <rm.ih>
#include <valuev.ih>
#include <text.ih>

static struct keymap *newKeymap;
static struct menulist *newMenus;
static struct lsetview *DeleteMode = NULL;

#define DataObject(A) (A->header.view.dataobject)
#define Data(A) ((struct lset *)DataObject(A) )
#define Islinked(self) (((struct view *)self)->parent != NULL && view_GetIM((struct view *)self))
#define lsetview_NeedLink 10
#define VALUE 10
#define CEL 5
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

static struct view *makeview(self,ls)
struct lsetview *self;
struct lset *ls;
{
    char *lv=ls->viewname;
    if(ls->dobj!=NULL && class_IsTypeByName(class_GetTypeName(ls->dobj), "unknown")) {
	if(lv==NULL || !class_IsTypeByName(lv, "unknownv")) lv="unknownv";
    }
    if(lv && *lv!='\0' /* && ls->dobj != NULL */ && (self->child = (struct view *) class_NewObject(lv)))  {
	view_AddObserver(self->child,self);
	lsetview_RetractCursor(self,self->cursor);
	self->mode = lsetview_NoUpdate;
	if(ls && ls->dobj != NULL) 
	    view_SetDataObject(self->child,ls->dobj);
	if(ls->refname && *ls->refname)
	    view_SetName(self->child,atomlist_StringToAtomlist(ls->refname));


	if(ls->application == TRUE){
	    self->app = view_GetApplicationLayer(self->child);
	}
	if(self->app == NULL) self->app = self->child;
	view_LinkTree(self->app,self);
	self->mode = lsetview_FirstUpdate;
	return self->child;
    }
    return NULL;
}
lsetview_SetMode(self,mode)
struct lsetview *self;
int mode;
{
self->mode = mode;
}
struct lsetview *lsetview__Create(classID, level,d,parent)
    struct classheader *classID;
int level;
struct lset *d;
struct view *parent;
{
    struct lsetview *lv;
    if(d == NULL) d = lset_New();
    lv = lsetview_New();
    lsetview_SetDataObject(lv,d);
    lsetview_LinkTree(lv,parent);
   lv->level = level;
    return(lv);
}
static initkids(self,ls)
struct lsetview *self;
struct lset *ls;
{
	struct lsetview *v1,*v2;
	v1 = lsetview_Create(self->level+1,(struct lset *) ls->left,(struct view *)self);
	v2 = lsetview_Create(self->level+1,(struct lset *)ls->right,(struct view *)self);
	if(ls->type == lsetview_MakeHorz)
	    lsetview_HSplit(self,v1,v2,ls->pct,TRUE);
	else
	    lsetview_VSplit(self,v1,v2,ls->pct,TRUE);
	self->mode = lsetview_IsSplit;
	lsetview_WantUpdate(self,self);
    }	
static dolink(self)
struct lsetview *self;
{
    struct lset *ls;
    ls = Data(self);
    if(Islinked(self)){
	self->mode = lsetview_UnInitialized;
	if(ls->left && ls->right)
	    initkids(self,ls);
	else if(makeview(self,ls) )
	    lsetview_WantUpdate(self,self);
    }
}

void lsetview__SetDataObject(self,ls)
struct lsetview *self;
struct lset *ls;
{
    self->revision = ls->revision;
   if(Data(self) != ls) super_SetDataObject(self,ls);
   self->mode = lsetview_NeedLink;
}

struct view *lsetview__Hit(self,action,x,y,numberOfClicks)
struct lsetview *self;
enum view_MouseAction action;
long x, y, numberOfClicks;
{
long size;
int pct;
struct lsetview *v1,*v2;
struct lset *ls;
struct view *vw;
    switch(self->mode){
	case lsetview_NeedLink:
	    return (struct view *)self;	   
        case lsetview_IsSplit:
            vw = super_Hit(self,action,x,y,numberOfClicks);
	    if(DeleteMode && vw == (struct view *) self && !lsetview_IsAncestor(DeleteMode,self)){
		lsetview_DestroyView(self);
		DeleteMode = NULL;
		return (struct view *)self;
	    }
	    return vw;

        case lsetview_MakeHorz:
            size = lsetview_GetLogicalWidth(self);
            pct = 100 - (x * 100 )/ size;
            break;
        case lsetview_MakeVert:
            size = lsetview_GetLogicalHeight(self);
            pct = 100 - (y * 100 )/ size;
            break;
	case lsetview_HasView:
	    if(DeleteMode && !lsetview_IsAncestor(DeleteMode,self)){
		lsetview_DestroyView(self);
		DeleteMode = NULL;
		return (struct view *)self;
	    }
	    return(view_Hit(self->app,action,x,y,numberOfClicks));
        default:
	    if(DeleteMode ){
		message_DisplayString(NULL,0,"Delete Mode Canceled");
		DeleteMode = NULL;
	    }
            if(!(self->HasFocus)) lsetview_WantInputFocus(self,self);
            return((struct view *)self);
    }
    if(action != view_LeftDown) return((struct view *)self);
    v1 = lsetview_Create(self->level+1,NULL,(struct view *)self);
    v2 = lsetview_Create(self->level+1,NULL,(struct view *)self);
    /* update dataobject */

    ls = Data(self);
    ls->left = DataObject(v1);
    ls->right =  DataObject(v2);
    ls->pct = pct;
    ls->type = self->mode;
    if(self->mode == lsetview_MakeHorz)
        lsetview_HSplit(self,v1,v2,pct,TRUE);
    else
        lsetview_VSplit(self,v1,v2,pct,TRUE);
    self->mode = lsetview_IsSplit;
/*    lsetview_WantUpdate(self,self); */
    lset_NotifyObservers(ls,0);

    return((struct view *)self);
}
static objecttest(self,name,desiredname)
register struct lsetview  *self;
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
static lsetview_PlaceApplication(self)
struct lsetview *self;
{
    if(self->child || self->mode == lsetview_IsSplit) return;
    Data(self)->application = TRUE;
    lsetview_PlaceView(self);
}
static lsetview_PlaceCel(self)
struct lsetview *self;
{
    if(self->child || self->mode == lsetview_IsSplit) return;
    Data(self)->application = CEL;
    lsetview_PlaceView(self);
}

static lsetview_PlaceValue(self)
struct lsetview *self;
{
    if(self->child || self->mode == lsetview_IsSplit) return;
    Data(self)->application = VALUE;
    self->promptforparameters = 1;
    lsetview_PlaceView(self);
}
static lsetview_DestroyView(self)
struct lsetview *self;
{
    struct lset *ls = Data(self);
/*    struct lpair *lp = (struct lpair *) self; */
/*
    lset_RemoveObserver(ls,self);
    lset_Finalize(ls);
    lset_Initialize(ls);
*/
    lset_InitializeObject(ls);
    ls->revision = self->revision;
    if(self->app){
	view_UnlinkTree(self->app); 
	view_Destroy(self->app);
    }
    self->HasFocus = 0;
    self->mode = lsetview_UnInitialized;
    self->child = NULL;
    self->app = NULL;
    self->promptforparameters = 0;
    self->pdoc = NULL;
    dolink(self);
    lsetview_WantInputFocus(self,self);
/*    if (lp->obj[0] != NULL)
        view_UnlinkTree(lp->obj[0]);
    if (lp->obj[1] != NULL)
        view_UnlinkTree(lp->obj[1]);
*/
}
static lsetview_PlaceView(self)
struct lsetview *self;
{
/* if(self->level == 0) return; */
/* prompt for dataobject */
/* create object and view */

    char iname[100];
    char viewname[200];
    long pf; 
    struct lset *ls = Data(self);
    boolean promptforname = im_ArgProvided(view_GetIM((&self->header.view)));
    im_ClearArg(self->header.view.imPtr);    
    if(self->child || self->mode == lsetview_IsSplit) return;
   viewname[0] = '\0';
#ifdef DEBUG
printf("in Place View %d\n",ls->application );
#endif /* DEBUG */
    if(ls->application == CEL){
	strcpy(iname,"cel");
	promptforname = FALSE;
    }
    else {
	pf = message_AskForString(self, 0, "Data object to insert here??: ", 0, iname, sizeof(iname));
	if (pf < 0) return -1;
    }
#ifdef DEBUG
printf("Still in Place View\n");
#endif /* DEBUG */
    if(strlen(iname)==0) {
	promptforname = TRUE;
    }
    else if(ls->application == VALUE){
	    sprintf(viewname,"%sV",iname);
	    if(!class_IsTypeByName(iname,"value")) 
		strcpy(iname,"value");
	    if(!class_IsTypeByName(viewname,"valueview")) 
		promptforname = TRUE ;    
	}
    else if(objecttest(self,iname,"dataobject") == FALSE) return;
    if(promptforname){
	if( message_AskForString (self, 0, "View to place here ", 0, viewname, 200) < 0)
	    return  -1;
	if(objecttest(self,viewname,"view") == FALSE) 
	    return ;
    }

    lset_InsertObject(ls, iname,viewname);
/*    if(makeview(self,ls) )  */
	lset_NotifyObservers(ls,0);
}
lsetview_DeleteMode(self)
struct lsetview *self;
{
  self->mode = lsetview_Initialized;
    lsetview_WantUpdate(self,self);
    message_DisplayString(self, 0, "Click on lset to delete");
    DeleteMode = self;
}
lsetview_UnsplitParent(self)
struct lsetview *self;
{
    struct lsetview *parent = (struct lsetview *) self->header.view.parent;
    if(self->child || self->mode == lsetview_IsSplit) return;
    if(parent && class_IsType(self,parent)){
	lsetview_Unsplit(parent,self);
    }
}

lsetview_ReadView(self)
struct lsetview *self;
{
/* prompt for dataobject */

    char iname[100];
    long pf;
    char realName[1024];
    FILE *thisFile;
    im_ClearArg(self->header.view.imPtr);    
    pf = message_AskForString(self, 0, "File to insert here: ", 0, iname, sizeof(iname));
    if (pf < 0) return -1;
    if(strlen(iname)==0){
	message_DisplayString(self, 0, "No file specified");
	return -1;
    }
    filetype_CanonicalizeFilename(realName, iname, sizeof(realName) - 1);

    if ((thisFile = fopen(realName, "r")) == NULL){
            message_DisplayString(self, 0,"can't open file");
	    return -1;
    }
    lsetview_ReadFile(self,thisFile,iname);
    fclose(thisFile);
}
void lsetview__ReadFile(self,thisFile,iname)
struct lsetview *self;
FILE *thisFile;
char *iname;
{
    int objectID;
    char *objectName;
    struct lset *ls = Data(self);
    objectName = filetype_Lookup(thisFile, iname, &objectID, NULL); /* For now, ignore attributes. */
    if(objectName == NULL) objectName = "text";
    if(objecttest(self,objectName,"dataobject") == FALSE) return;
    lset_InsertObject(ls, objectName,NULL);
    if(ls->dobj != NULL) dataobject_Read(ls->dobj,thisFile,objectID);
    lset_NotifyObservers(ls,0);
}
lsetview_Paste(self)
struct lsetview *self;
{
    FILE *pasteFile;
    if(self->child || self->mode == lsetview_IsSplit) return;
    pasteFile = im_FromCutBuffer(lsetview_GetIM(self));
    lsetview_ReadFile(self,pasteFile,NULL);
    im_CloseFromCutBuffer(lsetview_GetIM(self), pasteFile);

}

void lsetview__Update(self)
struct lsetview *self;
{
 /*   lsetview_RestoreGraphicsState(self); */
    struct lset *ls = Data(self);
    switch(self->mode){
	case lsetview_NoUpdate:
	    return ;
	case lsetview_FirstUpdate:
            cursor_SetStandard(self->cursor,Cursor_Arrow);
	case lsetview_UpdateView:
	    /* child view needs reinserted */
	    {
	    struct rectangle rr;
	    long foo1,foo2;
	    lsetview_GetVisualBounds(self,&rr);
	    view_InsertView(self->app, self, &rr);
	    view_RetractViewCursors(self->app,self->app);
	    view_DesiredSize(self->app,rr.width,rr.height,view_NoSet,&foo1,&foo2);
	    view_FullUpdate(self->app, view_FullRedraw,0,0,0,0);

	    if(self->mode == lsetview_FirstUpdate && ls->application == CEL &&
	       self->HasFocus)
		view_WantInputFocus(self->app,self->app);

	    self->mode = lsetview_HasView;
	    return;
	    }
	case lsetview_HasView:
	    view_Update(self->app);
	    return;
	case lsetview_IsSplit:
            if(cursor_IsPosted(self->cursor))
                lsetview_RetractCursor(self,self->cursor);
	    if(self->header.lpair.objsize[1] > 0 &&   ls->pct != self->header.lpair.objsize[1]){
		ls->pct = self->header.lpair.objsize[1];
		lset_NotifyObservers(ls,0);
	    }
            super_Update(self); return ;
        case lsetview_MakeVert:
            cursor_SetStandard(self->cursor,Cursor_HorizontalBars);
            break;
        case lsetview_MakeHorz:
            cursor_SetStandard(self->cursor,Cursor_VerticalBars);
            break;
        case lsetview_Initialized:
            cursor_SetStandard(self->cursor,Cursor_Arrow);
            break;
    }
    if(ls->dobj || ls->left){
	dolink(self);
	return;
    }
    if(self->HasFocus){
        lsetview_SetTransferMode(self,graphic_BLACK);
    }
    else
        lsetview_SetTransferMode(self,graphic_WHITE);
    lsetview_EraseVisualRect(self);
    lsetview_SetTransferMode(self,graphic_INVERT);
   if(!cursor_IsPosted(self->cursor)){
       struct rectangle tr;
       lsetview_GetVisualBounds(self,&tr);
       lsetview_PostCursor(self,&tr,self->cursor);
   }
}
void lsetview__FullUpdate(self,type,left,top,width,height)
struct lsetview *self;
enum view_UpdateType type;
long left,top,width,height;
{
    if(self->mode == lsetview_NeedLink)
	dolink(self);
    if(self->mode == lsetview_HasView){
	struct rectangle rr;
	long foo1,foo2;
/*	lsetview_RestoreGraphicsState(self); */
/*	lsetview_GetVisualBounds(self,&rr); */
	lsetview_GetLogicalBounds(self,&rr);
	view_InsertView(self->app, self, &rr);
	view_RetractViewCursors(self->app,self->app);
	view_DesiredSize(self->app,width,height,view_NoSet,&foo1,&foo2);
	view_FullUpdate(self->app,type,left,top,width,height);
    }
    else if (self->mode == lsetview_IsSplit){
        super_FullUpdate(self,type,left,top,width,height);
    }
    else lsetview_Update(self);
}
void lsetview__ObservedChanged(self, changed, value)
    struct lsetview *self;
    struct observable *changed;
    long value;
    {
	struct lset *ls = Data(self);
	struct lpair *lp = (struct lpair *) self;
	struct view *v;
	if(value ==  observable_OBJECTDESTROYED ){
	    if(changed == (struct observable *)self->child ||
	       changed == (struct observable *) DataObject(self) ){
		self->child = NULL;self->app = NULL;
		lsetview_DestroyView(self);
		/*		self->child = NULL;self->app = NULL;
		lsetview_WantUpdate(self,self); */
	    }
	}
	else if(changed == (struct observable *) DataObject(self)){
	    if(self->revision != ls->revision){
		if(self->mode == lsetview_IsSplit){
		    if(lp->obj[0]){
			v = lp->obj[0];
			lp->obj[0] = NULL;
			view_UnlinkTree(v);
			view_Destroy(v);
		    }
		    if(lp->obj[1]){
			v = lp->obj[1];
			lp->obj[1] = NULL;
			view_UnlinkTree(v);
			view_Destroy(v);
		    }
		}
		if(self->app){
		    view_UnlinkTree(self->app);
		    view_RemoveObserver(self->app,self);
		    view_Destroy(self->app);
		}
		self->HasFocus = 0;
		self->mode = lsetview_UnInitialized;
		self->child = NULL;
		self->app = NULL;
		self->promptforparameters = 0;
		self->pdoc = NULL;
		self->level = 0;
		DeleteMode = NULL;
		cursor_SetStandard(self->cursor,Cursor_Arrow);
		dolink(self);
	    }
	    else if(Data(self)->pct != self->header.lpair.objsize[1]){
		self->header.lpair.objsize[1] =  Data(self)->pct ;
		self->header.lpair.needsfull = 1;
		lsetview_WantUpdate(self,self);
	    }

	    super_ObservedChanged(self, changed, value);
	}
    }
boolean lsetview__Unsplit(self,who)
struct lsetview *self,*who;
{
    struct lpair *lp ;
    struct lsetview *saved;
    struct lset *ll,*oldll;
    lp = (struct lpair *) self;
    if(lp->obj[0] == (struct view *) who)
	saved = (struct lsetview *) lp->obj[1];
    else if (lp->obj[1] == (struct view *) who)
	saved = (struct lsetview *) lp->obj[0];
    else return FALSE;
    if(!class_IsType(saved,self)) return FALSE;
    ll = Data(saved);
    oldll = Data(self);

    oldll->type = ll->type;
    oldll->pct = ll->pct;
    strcpy(oldll->dataname,ll->dataname);
    strcpy(oldll->viewname,ll->viewname);
    strcpy(oldll->refname,ll->refname);
    oldll->dobj = ll->dobj;
    oldll->left = ll->left;
    oldll->right = ll->right;
    oldll->application = ll->application;
    oldll->pdoc = ll->pdoc;
    (oldll->revision)++;
    lset_NotifyObservers(oldll,0);
   /*
    savedlp = (struct lpair *) saved;
    view_UnlinkTree(lp->obj[0]);
    view_UnlinkTree(lp->obj[1]);
    lp->obj[0] = lp->obj[1] = NULL;
    self->mode = saved->mode;
    self->level = saved->level;
    self->child = saved->child;
    self->app = seved->app;
    self->promptforparameters = saved->promptforparameters;
    if(self->mode == lsetview_IsSplit){
	v1 = savedlp->obj[0];
	v2 = savedlp->obj[1];
	savedlp->obj[0] = savedlp->obj[1] = NULL;
	if(ll->type == lsetview_MakeHorz)
	    lsetview_HSplit(self,v1,v2,lp->pct,TRUE);
	else
	    lsetview_VSplit(self,v1,v2,lp->pct,TRUE);
    }
    else if(self->child){
	view_UnlinkTree(self->child);
	view_LinkTree(self->child,self);
    }
    lsetview_Destroy(saved);
    lsetview_Destroy(who);
    lsetview_WantUpdate(self,self);
    */
   
}
void lsetview__ReceiveInputFocus(self)
struct lsetview *self;
{
    if(self->child){
	view_WantInputFocus(self->child,self->child);
	return;
    }
    if(self->mode == lsetview_IsSplit){
	struct lpair *lp = (struct lpair *) self;
	view_WantInputFocus(lp->obj[0],lp->obj[0]);
	return;
    }
    self->HasFocus = 1;
    self->keystate->next = NULL;
    lsetview_PostKeyState(self, self->keystate); 
    lsetview_PostMenus(self,self->menulist);
    lsetview_WantUpdate(self,self);

}
void lsetview__LoseInputFocus(self)
struct lsetview *self;
{
self->HasFocus = 0;
if(self->mode == lsetview_MakeVert || self->mode == lsetview_MakeHorz)
    self->mode = lsetview_Initialized;
lsetview_WantUpdate(self,self);
}
boolean lsetview__InitializeObject(classID, self)
    struct classheader *classID;
struct lsetview *self;

{
    self->HasFocus = 0;
    self->mode = lsetview_UnInitialized;
    self->child = NULL;
    self->app = NULL;
    self->keystate = keystate_Create(self,newKeymap);
    self->menulist = menulist_DuplicateML(newMenus,self);
    self->cursor = cursor_Create(self);
    self->promptforparameters = 0;
    self->pdoc = NULL;
    self->level = 0;
    DeleteMode = NULL;
    return TRUE;
}
static void lsetview_SplitVert(self)
struct lsetview *self;
{
if(self->child || self->mode == lsetview_IsSplit) return;
self->mode = lsetview_MakeHorz;
lsetview_WantUpdate(self,self);
}
static void lsetview_SplitHorz(self)
struct lsetview *self;
{
if(self->child || self->mode == lsetview_IsSplit) return;
self->mode = lsetview_MakeVert;
lsetview_WantUpdate(self,self);
}

void lsetview__LinkTree(self, parent)
struct lsetview *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if(self->child) 
	view_LinkTree(self->child,self);
    if(self->mode == lsetview_NeedLink) dolink(self);
}
void lsetview__WantNewSize(self, requestor)
    struct lsetview *self;
    struct view *requestor;
{
	self->mode = lsetview_UpdateView;
	lsetview_WantUpdate(self,self);
}
boolean lsetview__InitializeClass(classID)
struct classheader *classID;
{
    char *cmdString;
    struct proctable_Entry *tempProc;
    newMenus = menulist_New();
    newKeymap = keymap_New();
    tempProc = proctable_DefineProc(cmdString = "lsetview-Insert-Object", lsetview_PlaceView,&lsetview_classinfo,NULL, "Set the object of a child view");
/*    keymap_BindToKey(newKeymap,cmdString,NULL,"\033\t"); */
/*    menulist_AddToML(newMenus,"lset,Set Object~31",tempProc,NULL,0); */
    tempProc = proctable_DefineProc("lsetview-Destroy-Object", lsetview_DestroyView,&lsetview_classinfo,NULL, "Destroy the child view");
    keymap_BindToKey(newKeymap,"\033d",tempProc,0);


    tempProc = proctable_DefineProc(cmdString = "lsetview-Insert-value", lsetview_PlaceValue,&lsetview_classinfo,NULL, "Set the value of a child view");
/*    keymap_BindToKey(newKeymap,cmdString,NULL,"\033\t"); */
/*    menulist_AddToML(newMenus,"lset,Set value~34",tempProc,NULL,0); */

    tempProc = proctable_DefineProc(cmdString = "lsetview-Insert-application", lsetview_PlaceApplication,&lsetview_classinfo,NULL, "Set the application of a child view");
    menulist_AddToML(newMenus,"lset,Set application~32",tempProc,NULL,0);
    tempProc = proctable_DefineProc(cmdString = "lsetview-Insert-Cel", lsetview_PlaceCel,&lsetview_classinfo,NULL, "Set the Cel of a child view");
    menulist_AddToML(newMenus,"lset,Set Cel~30",tempProc,NULL,0);

    tempProc = proctable_DefineProc(cmdString = "lsetview-Paste", lsetview_Paste,&lsetview_classinfo,NULL, "Fill lset with contents of cut buffer");
    menulist_AddToML(newMenus,"lset,Paste~36",tempProc,NULL,0);


/*
    tempProc = proctable_DefineProc(cmdString = "lsetview-Delete-Mode", lsetview_DeleteMode,&lsetview_classinfo,NULL, "sets delete mode");
*/
    tempProc = proctable_DefineProc(cmdString = "lsetview-Unsplit", lsetview_UnsplitParent,&lsetview_classinfo,NULL, "Unsplits parent lset");
    menulist_AddToML(newMenus,"lset,Unsplit Lset~40",tempProc,NULL,0);/*    keymap_BindToKey(newKeymap,cmdString,NULL,"\033t"); */
    tempProc = proctable_DefineProc(cmdString = "lsetview-Read-File", lsetview_ReadView,&lsetview_classinfo,NULL, "read a file into a child view");
/*    keymap_BindToKey(newKeymap,cmdString,NULL,"\030\t"); */
    menulist_AddToML(newMenus,"lset,Insert File~35",tempProc,NULL,0);

    tempProc = proctable_DefineProc(cmdString = "lsetview-Split-Horz", lsetview_SplitHorz,&lsetview_classinfo,NULL, "split the lpair Horizontally");
/*    keymap_BindToKey(newKeymap,cmdString,NULL,"\033\t"); */
    menulist_AddToML(newMenus,"lset,Split Horizontal~11",tempProc,NULL,0);
    tempProc = proctable_DefineProc(cmdString = "lsetview-Split-Vert", lsetview_SplitVert,&lsetview_classinfo,NULL, "split the lpair Vertically");
/*    keymap_BindToKey(newKeymap,cmdString,NULL,"\033\t"); */
    menulist_AddToML(newMenus,"lset,Split Vertically~10",tempProc,NULL,0);
    return TRUE;
}
void lsetview__InitChildren(self)
struct lsetview *self;
{
    if(self->child) 
	view_InitChildren(self->child);
    else     super_InitChildren(self);

}
boolean lsetview__CanView(self,TypeName)
struct lsetview *self;
char *TypeName;
{
    return class_IsTypeByName(TypeName,"lset");
}
void lsetview__Print(self, file, processor, finalFormat, topLevel)
struct lsetview *self;
FILE *file;
char *processor;
char *finalFormat;
boolean topLevel;
{
    struct lpair *lself = (struct lpair *) self;
    if(self->child) 
	view_Print(self->child,file, processor, finalFormat, topLevel);
    else {
	if (lself->obj[0] != NULL)
	    view_Print(lself->obj[0] ,file, processor, finalFormat, topLevel);
	if (lself->obj[1] != NULL)
	    view_Print(lself->obj[1] ,file, processor, finalFormat, topLevel);
    }
}
void lsetview__FinalizeObject(classID, self)
struct classheader *classID;
struct lsetview *self;
{
    if(self->menulist) menulist_Destroy(self->menulist);
    if(self->app){
	struct view *child = self->child;
	view_UnlinkTree(child);
	self->child = NULL;
	view_RemoveObserver(child,self);
	view_Destroy(child);
    }
}
