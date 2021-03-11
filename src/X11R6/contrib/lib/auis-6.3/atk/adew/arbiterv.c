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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/arbiterv.c,v 2.28 1993/05/04 01:05:17 susan Exp $";
#endif


 

#include <andrewos.h>
#include <class.h>
#include <arbiterv.eh>
#include <dict.ih>
#include <atomlist.ih>
#include <celv.ih>
#include <value.ih>
#include <cel.ih>
#include <rm.ih>
#include <arbiter.ih>
#include <view.ih>
#include <environ.ih>
#include <lsetv.ih>
#include <message.ih>
#include <filetype.ih>
#include <complete.ih>
#include <im.ih>
#include <text.ih>
#include <cursor.ih>
#include <arbcon.ih>
#include <buffer.ih>
#include <frame.ih>
#include <dataobj.ih>

static struct cursor *WaitCursor;

#define INCREMENTSIZE 64
#define INITIALSIZE 128
#define SEPCHAR '_'
#define ExistingObjectDefaultView 2
#define ExistingObjectNamedView 3
#define NewObjectDefaultView 0
#define NewObjectNamedView 1
#define NamedView 0
#define ExistingObject 0
#define SaveAll 0
static struct atom *a_vp;
static struct arbiterview *firstlink , *lastlink;
#define NameSet(V) (((struct view *)V)->name_explicitly_set)
#define DataObject(A) (A->header.view.dataobject)
#define Cel(A) ((struct cel *) DataObject(A))
#define Parent(V) (((struct view *)V)->parent)
#define Arbiter(A) ((struct arbiter *) DataObject(A))
struct celview *arbiterview__lookupname(self,ViewName)
struct arbiterview *self;
char *ViewName;
{
    struct cel *cl;
    char *st;
    struct celview **v;
    register int i ;
    for(v = (self->celviewlist), i = 0; i < self->celcount; v++,i++){	
	cl = Cel((*v));
	st = cel_GetRefName(cl);
	if(*st == *ViewName && strcmp(st,ViewName) == 0){
	    return *v;
	}
    }
    return NULL;
}

void arbiterview__FullUpdate(self,type,left,top,width,height)
struct arbiterview *self;
enum view_UpdateType type;
long left,top,width,height;
{
/*
    if(self->NeedsInit ){
	struct value *v;
	if((v = (struct value *) arbiterview_GetNamedObject(self,"ArbGoButton")) != NULL)
	    value_SetValue(v,1);
	self->NeedsInit = FALSE;
    }
*/
    super_FullUpdate(self,type,left,top,width,height);
}

void arbiterview__InitCell(self,cv)
struct arbiterview *self;
struct celview *cv;
{
    struct cel *cl;
    cl = Cel(cv);
#ifdef DEBUG
printf("Initing %s (%s)- %d\n", cel_GetRefName(cl),class_GetTypeName(cel_GetObject(cl)),cel_GetObject(cl));
#endif /* DEBUG */
    if(cl != NULL && cl->viewType == NULL){
#ifdef DEBUG
	printf("In INITCELL\n");
#endif /* DEBUG */
/*	arbcon_InitCel(cv,self); */
	addlist(self,cv); /* sometimes redundent */
	return;
    }
#if 0
	if((name = arbcon_GetDesiredObjectName(&type)) != NULL){
	    if(type == arbcon_OBJECTNAME){
		if((celv = arbiterview_lookupname(self,name)) != NULL){
		    dob = cel_GetObject(Cel(celv));
		    cel_SetObject(cl,dob);
		    name = class_GetTypeName(dob);
		}
	    }
	    else if(type == arbcon_OBJECTTYPE){
		cel_SetObjectByName(cl,name);
	    }
	}
	vwname = arbcon_GetDesiredViewName(&writein);
	cel_SetViewName(cl,vwname,writein);

	if (arbcon_GetApplicationChoice() && cel_GetApplication(cl) != cel_VALUE){
	    cel_SetApplication(cl,cel_APPLICATION);
	}

	if (arbcon_GetNameChoice()){
	    do{
		sprintf(buf,"%s-%d",name,count++);
	    } while (arbiterview_registername(self,cv,buf)== FALSE);
	    return;
	}
	
#endif /* 0 */
    if(cl && cl->refname != NULL){
	if(arbiterview_registername(self,cv,cl->refname) == FALSE){
	    char *dp,*cp,buf[512];
	    char *name = cl->refname;
	    int count = 0;
	    if((dp = strrchr(name,SEPCHAR) ) != NULL) {
		dp++;
		if(*dp >= '0' && *dp <= '9') count = atoi(dp) + 1;
		dp--;
	    } else count = 1;
	    for(cp = buf; *name != '\0' && name != dp; name++, cp++)
		*cp = *name;
	    do{
		sprintf(cp,"%c%d",SEPCHAR,count++);
	    } while (arbiterview_registername(self,cv,buf)== FALSE);
	}
    }
    else addlist(self,cv);
#ifdef DEBUG
printf("Leaving initcell %s (%s)- %d\n", cel_GetRefName(cl),class_GetTypeName(cel_GetObject(cl)),cel_GetObject(cl));
#endif /* DEBUG */
}


void arbiterview__ArbRead(self,frs)
struct arbiterview *self;
char *frs;
{
/* user code begins here for ArbReadButtonCallBack */
    FILE *f;
    struct celview *cv = (struct celview *) self;
    struct view *child = cv->child;
    struct arbiter *ab;
    if(*frs == '\0' || (f = fopen(frs,"r")) == NULL) {
	sprintf(frs,"ERROR:Can't Read %s",frs);
	message_DisplayString(NULL,0,frs);
	return;
    }
    if(child){
	view_UnlinkTree(child);
	view_Destroy(child);
    }
    ab = Arbiter(self);
 /*   celview_InitializeObject(cv);*/
    cv->child = NULL;
    arbiter_Read(ab,f,0L);
/*    arbiterview_SetDataObject(self,ab); */
    arbiterview_WantInputFocus(self,self);
    fclose(f);
 /*   strcpy(self->filename,frs); */
}
boolean arbiterview__CreateCon(self,EditText)
struct arbiterview *self;
struct text *EditText;
{
    char fnm[64];
    char buf[1060];
    FILE *popen(),*f;
    struct buffer *buff;
    struct dataobject *dat;
    struct view *truechild;
    boolean erase = TRUE;

    if(environ_GetProfileSwitch("SecurityConscious", FALSE)) {
	message_DisplayString(self, 0, "Creating a controller is not allowed under SecurityConsciousness.");
	fprintf(stderr, "Creating a controller is not allowed under SecurityConsciousness.\n");
	return TRUE;
    }
    
    truechild = arbiterview_GetTrueChild(self);
    strcpy(fnm, "/tmp/arbtmpXXXXXX");
    mktemp(fnm);
    im_SetProcessCursor(WaitCursor);

    if(class_IsTypeByName(class_GetTypeName(truechild),"frame") &&
	(buff= frame_GetBuffer(((struct frame *) truechild)))!= NULL &&
	(dat = buffer_GetData(buff)) != NULL){
	int ver;
	ver =  dataobject_GetModified(dat);
	if(buffer_GetWriteVersion(buff) == ver){
	    sprintf(buf,"createcon %s",buffer_GetFilename(buff));
	    erase = FALSE;
	}
	else if(buffer_GetCkpVersion(buff) == ver){
	    sprintf(buf,"createcon %s",buffer_GetCkpFilename(buff));
	    erase = FALSE;
	}
	else {
	    if((f = fopen(fnm,"w")) == NULL) return FALSE;
	    dataobject_Write(dat,f,im_GetWriteID(),0);
	}
    }
    else {
	if((f = fopen(fnm,"w")) == NULL) return FALSE;
	arbiter_Write(Arbiter(self),f,im_GetWriteID(),0);
    }
    if(erase){
	fclose(f);
	sprintf(buf,"createcon %s",fnm);
    }
    text_Clear(EditText);
    if((f = popen(buf,"r")) == NULL) return FALSE;
    text_Read(EditText,f,0);
    text_NotifyObservers(EditText,NULL);
    pclose(f);
    if(erase) unlink(fnm);
    im_SetProcessCursor(NULL);
    return TRUE;
}

void arbiterview__DeleteCell(self,cv)
struct arbiterview *self;
struct celview *cv;
{
    if(deletelist(self,cv))
    	celview_RemoveObserver(cv,self);

}
int arbiterview__registername(self,cv,refname)
struct arbiterview *self;
struct celview *cv;
char *refname;
{
    register struct cel *clp;
    struct cel *cl = Cel(cv);
    struct celview **v;
    int i;
#ifdef DEBUG
printf("In Register name \n");
#endif /* DEBUG */
for(v = (self->celviewlist), i = 0; i < self->celcount; v++,i++){
#if 0
    if(cv == *v) /* already registered */ return TRUE;
#endif /* 0 */
    if(refname == NULL || *refname == '\0') return FALSE;
    clp = Cel((*v));
    if(*refname == *(cel_GetRefName(clp)) && strcmp(refname,cel_GetRefName(clp)) == 0 && cv != *v )
	return FALSE;
}
#ifdef DEBUG
printf("calling addlist\n");
#endif /* DEBUG */
    i = addlist(self,cv);
#ifdef DEBUG
printf("out of addlist, cl = %d i = %d %s %s \n",(long)cl,i,refname,cl->refname);
#endif /* DEBUG */
    if(cl->refname != refname)
	cel_SetRefName(cl,refname);
    arbcon_AddCel(self,cl,TRUE);
    if(self->handler){
	(*(self->handler))(cv,self->hrock);
    }
#ifdef DEBUG
printf("continueing\n");
#endif /* DEBUG */

#ifdef DEBUG
printf(" exiting Register name \n");
#endif /* DEBUG */
    return TRUE;
}
static appendlist(lst,cnt,str)
char **lst;
int cnt;
char *str;
{
    int next = 1;
    while(*str){
	if(*str == ',' || *str == '\n') {
	    *str = '\0';
	    next++;
	}
	else if(*str == '\0') break;
	else if(*str == ' ') ;
	else if(next){
	    lst[cnt++] = str;
	    next = 0;
	}
	str++;
    }
    return cnt;
}
	

boolean arbiterview__InitializeObject(classID,self)
struct classheader *classID;
struct arbiterview *self;
{
#ifndef _IBMR2
    extern char *malloc();
#endif /* _IBMR2 */
    if(lastlink != NULL) lastlink->next = self;
    self->next = NULL;
    lastlink = self;
    if(firstlink == NULL) firstlink = self;
   self->celcount = 0;
     self->celsize = INITIALSIZE;
    self->celviewlist = (struct celview  **) malloc(INITIALSIZE * sizeof(struct celview *));
    if(self->celviewlist == NULL) return FALSE;
    self->viewchoice = self->applicationchoice = NULL;
   self->CelNameVal = self->NameChoice = NULL;
   self->EditText = NULL;
   self->NeedsInit = TRUE;
   self->CopyMode = self->CelMode = FALSE;
   self->handler = NULL;
 /*  arbcon_AddArbiter(self); */
    return TRUE;
}
boolean arbiterview__InitializeClass(ClassID)
struct classheader *ClassID;
{
    firstlink = lastlink = NULL;
    a_vp = atom_Intern("struct dataobject *");
    WaitCursor = cursor_Create(NULL);
    if(WaitCursor) cursor_SetStandard(WaitCursor,Cursor_Wait);
    return TRUE;
}
void arbiterview__FinalizeObject(classID,self)
struct classheader *classID;
struct arbiterview *self;
{
    register struct celview **v;
    register int i = self->celcount;
    for(v = self->celviewlist; i; v++,i--)
	celview_RemoveObserver(*v,self);
    free( self->celviewlist);
    if(self == firstlink) {
	firstlink = self->next;
	if(self == lastlink) lastlink = NULL;
    }
    else {
	struct arbiterview *ep;
	for(ep = firstlink; ep->next != NULL; ep = ep->next)
	    if(ep->next == self) break;
	ep->next = self->next;
	if(self == lastlink) lastlink = ep;
    }
    arbcon_DeleteArbiter(self);
}

static addlist(self,cv)
struct arbiterview *self;
struct celview *cv;
{
    register struct celview **v;
    register int i = 0;
    for(v = (self->celviewlist); i < self->celcount; v++,i++){	
	if(*v == cv) return i;
    }
    *v++ = cv;
    if(++(self->celcount) >= self->celsize){
	self->celsize += INCREMENTSIZE;
	self->celviewlist = (struct celview **)realloc(self->celviewlist,self->celsize* sizeof(struct celview *));
    }
    celview_AddObserver(cv,self);
    arbiterview_NotifyObservers(self,0);
    return self->celcount - 1;
}
static deletelist(self,cv)
struct arbiterview *self;
struct celview *cv;
{
    register struct celview **v;
    register int i = self->celcount;
    int shift ;
    struct cel *cl = Cel(cv);
    shift = 0;
    if(arbcon_currentcelview() == cv) arbcon_SetCurrentCelview(NULL);
#ifdef DEBUG
printf("in deletelist cv = %d\n",(long)cv);
#endif /* DEBUG */
    for(v = self->celviewlist ; i; v++,i--){
	if(shift) *(v - 1) = *v;
	if(*v == cv){
#ifdef DEBUG
printf("Found ref %d \n" ,cl->refname);
#endif /* DEBUG */
	    shift++;
	    if(cl->refname) {
#ifdef DEBUG
printf("found string\n");
#endif /* DEBUG */
		arbcon_DeleteCelview(self,cv);
	    }
	}
    }
    if(shift){
	(self->celcount)--;
    }
/*    if(value_GetValue(self->objectchoice) == ExistingObject)
	objectchoicechanged(self,self->objectchoice,0,ExistingObject); */
    arbiterview_NotifyObservers(self,0);
    return shift;
}

void arbiterview__AddHandler(self,handler,rock)
struct arbiterview *self;
int (*handler)();
long rock;
{
    struct celview **v;
    register int i = self->celcount;

    self->hrock = rock;
    self->handler = handler;
    if(self->handler){
	for(v = self->celviewlist ; i; v++,i--){
	    (*(self->handler))(*v,self->hrock);
	}
    }
}
struct basicobject * arbiterview__WantHandler(self, handlerName)
struct arbiterview *self;
char *handlerName;
{
    if(strcmp(handlerName,"arbiterview") == 0) return (struct basicobject *)self;
    return super_WantHandler(self, handlerName);
}
struct dataobject * arbiterview__GetNamedObject(classID,vw,name)
struct classheader *classID;
struct view *vw;
char *name;
{
    long val;
    char ss[256];
    struct cel *cl;
    char *st;
    struct arbiterview *self;
    struct atomlist *al;
    struct celview **v;
    register int i ;
    struct atom *at;
    char *ObjectName,buf[1024];
    strcpy(buf,name);
    ObjectName = buf;
#ifdef DEBUG
printf("in arb for %s\n",ObjectName);
#endif /* DEBUG */
    if(vw) self = arbiterview_FindArb(vw);
    else {
	if((st = strrchr(buf,arbiterview_SEPCHAR)) != NULL){
	    *st = '\0';
	    ObjectName = ++st;
	    self = arbiterview_FindArbByName(buf);
	}
	else self = NULL;
    }

#ifdef DEBUG
printf("Self = %ld\n",self);
#endif /* DEBUG */
    at = atom_Intern(name);
    if(self){
	for(v = (self->celviewlist), i = 0; i < self->celcount; v++,i++){	
	    cl = Cel((*v));
	    if(at == cel_GetRefAtom(cl)){
#ifdef DEBUG
printf("Found via arbiter %s = %s (%s)- Returning %d\n",cel_GetRefName(cl),ObjectName,class_GetTypeName(cel_GetObject(cl)),cel_GetObject(cl));
#endif /* DEBUG */
		return cel_GetObject(cl);
	    }
	}
	{ /* support the new child finding stuff in arbiter */
	    struct cel *cel;
	    struct arbiter *fcel;
	    if((fcel = (struct arbiter *)  self->header.view.dataobject) != NULL){

		for(cel = arbiter_GetFirst(fcel); cel != (struct cel *)fcel; cel = cel_GetNextChain(cel)){
		    if(at == cel_GetRefAtom(cel)){
			return cel_GetObject(cel);
		    }
		}
	    }
	}
    }
    sprintf(ss,"DOBJ-%s",ObjectName);
    al = atomlist_StringToAtomlist(ss);
    if(rm_GetResource(al,al,a_vp,&val)) {
#ifdef DEBUG
printf("Returning %d\n",val);
#endif /* DEBUG */
	return (struct dataobject *)  val;
    }
#ifdef DEBUG
printf("Returning NULL\n");
#endif /* DEBUG */
    return NULL;
}
struct view * arbiterview__GetNamedView(classID,vw,name)
struct classheader *classID;
struct view *vw;
char *name;
{
    struct celview *v;
    if((v = arbiterview_GetNamedCelview(vw,name)) == NULL) return NULL;
    return celview_GetTrueChild(v);

}

struct celview * arbiterview__GetNamedCelview(classID,vw,name)
struct classheader *classID;
struct view *vw;
char *name;
{

    struct cel *cl;
    char *st;
    struct arbiterview *self;
    struct celview **v;
    register int i ;
    char *ViewName,buf[1024];
    strcpy(buf,name);
    ViewName = buf;
#ifdef DEBUG
printf("in arb for %s\n",ViewName);
#endif /* DEBUG */
    if(vw) self = (struct arbiterview *) arbiterview_FindArb(vw);
    else {
	if((st = strrchr(buf,arbiterview_SEPCHAR)) != NULL){
	    *st = '\0';
	    ViewName = ++st;
	    self = arbiterview_FindArbByName(buf);
	}
	else self = NULL;
    }
#ifdef DEBUG
printf("Self = %ld\n",self);
#endif /* DEBUG */
    if(self){
	for(v = (self->celviewlist), i = 0; i < self->celcount; v++,i++){	
	    cl = Cel((*v));
	    st = cel_GetRefName(cl);
	    if(*st == *ViewName && strcmp(st,ViewName) == 0){
#ifdef DEBUG
printf("Found via arbiter - Returning %d\n",val);
#endif /* DEBUG */
		return *v;
	    }
	}
    }
    return NULL;
}
void arbiterview__ObservedChanged(self, changed, value)
struct arbiterview *self;
struct observable *changed;
long value;
{
    if (value == observable_OBJECTDESTROYED){
	if(deletelist(self,(struct celview *)changed)) return;
    }
    super_ObservedChanged(self, changed, value);
}
void arbiterview__SetDataObject(self,dd)
struct arbiterview *self;
struct arbiter *dd;
{
    arbiter_SetApplication(dd,cel_APPLICATION);
    super_SetDataObject(self,dd);
}
void arbiterview__InitArbcon(self)
struct arbiterview *self;
{
    register struct celview **v;
    struct cel *cl;
    int i;
    for(v = (self->celviewlist), i = 0; i < self->celcount; v++,i++){	
	cl = Cel((*v));
	arbcon_AddCel(self,cl,FALSE);
    }
    arbiterview_SetCelMode(self,TRUE);
}
struct arbiterview *arbiterview__GetFirstLink(ClassID)
struct classheader *ClassID;
{
    return firstlink;
}
long arbiterview__GetArbName(self,buf,buflen)
struct arbiterview *self;
char *buf;
long buflen;
{
    int csize;
    struct buffer *b;
    char *myname;
    struct celview *cv = (struct celview *) self;
    struct view *parent = self->header.view.parent;
    if(Cel(self) == NULL) return buflen;
    myname = cel_GetRefName(Cel(self));
    if(parent != NULL && cv->TopLevel == FALSE &&  cv->arb != NULL) {
	csize = arbiterview_GetArbName(cv->arb,buf,buflen);
    }
    else{
	csize = 0;
	if(myname == NULL || *myname == '\0'){
	    if((b = buffer_FindBufferByData(DataObject(self))) != NULL || 
	       (b = buffer_FindBufferByData(arbiterview_GetTrueChild(self))) != NULL )
		myname = buffer_GetName(b);
	}
    }
    if(myname == NULL || *myname == '\0'){
	myname = "UNNAMED";
    }
    if(strlen(myname) + csize + 2 < buflen){
	if(csize) buf[csize++] = arbiterview_SEPCHAR;
	sprintf(buf + csize,"%s",myname);
    }
   return (csize + strlen(myname));
}
struct arbiterview *arbiterview__FindArbByName(ClassID,str)
struct classheader *ClassID;
char *str;
{
    char buf[2048];
    long len,slen;
    struct arbiterview *ep;
    slen = strlen(str);
    for(ep = firstlink; ep != NULL ; ep = ep->next){
	len = arbiterview_GetArbName(ep,buf,2048);
	if(len == slen && strcmp(buf,str) == 0)
	    return ep;
    }
    return NULL;
}

char *arbiterview__GetDataName(self)
struct arbiterview *self;
{
    return "arbiter";
}
void arbiterview__LinkTree(self, parent)
register struct arbiterview *self;
struct view *parent;
{
    super_LinkTree(self,parent);
    if(parent == NULL){
	arbcon_DeleteArbiter(self);
    }
    else
	arbcon_AddArbiter(self);

}
boolean arbiterview__InTree(self)
struct arbiterview *self;
{
/*  printf("In Intree %d, %s, parent = %d\n",self ,arbiter_GetRefName(Arbiter(self)), ((struct view *)self)->parent);
 printf("cp = %d\n",arbiterview_GetTrueChild(self)->parent);
 if(arbiterview_GetTrueChild(self)->parent) printf("cpp = %d\n",arbiterview_GetTrueChild(self)->parent->parent);
	    fflush(stdout);
*/
    if(((struct view *)self)->parent == NULL){
	if((arbiterview_GetTrueChild(self) != NULL) &&
	   class_IsTypeByName(class_GetTypeName(arbiterview_GetTrueChild(self)),"frame") &&
/*	   (strcmp(class_GetTypeName(arbiterview_GetTrueChild(self)),"frame") == 0) && */
	   arbiterview_GetTrueChild(self)->parent != NULL && 
	   (arbiterview_GetApplication(self) != NULL) &&
	   ((arbiterview_GetApplication(self))->parent != NULL)){
	    fflush(stdout);
	    return TRUE;
	}
	return FALSE;
    }
    if(((struct celview *)self)->arb == NULL) return TRUE;
    return(arbiterview_InTree(((struct celview *)self)->arb));
}

void arbiterview__SetIgnoreUpdates(classID,vw,val)
struct classheader *classID;
struct view *vw;
boolean val;
{
    struct arbiterview *self;
    struct view *fr;
    struct buffer *buf;
    if(vw){
	self = arbiterview_FindArb(vw);
	if(self && Arbiter(self)){
	    arbiter_SetNoSave(Arbiter(self),val);
	}
	fr = arbiterview_GetTrueChild(self);
	if(class_IsTypeByName(class_GetTypeName(fr),"frame")){
	    if((buf = frame_GetBuffer(((struct frame *) fr))) != NULL)
		buffer_SetScratch(buf,val);
	}
    }
}
struct arbiterview *arbiterview__FindArb(classID, vw)
struct classheader *classID;
struct view *vw;
{
    struct arbiterview *self;
    struct celview *cself;
    struct arbiter *arb;
    struct im *im;
    struct frame *frame;
    struct buffer *buf;
    self = NULL;
    if(vw){
	self = (struct arbiterview *) view_WantHandler(vw,"arbiterview");
	if(self == NULL && (im = view_GetIM(vw)) != NULL && 
	   (frame = (struct frame *)(im->topLevel)) != NULL &&
	   class_IsTypeByName(class_GetTypeName(frame),"frame") &&
/*	   strcmp(class_GetTypeName(frame),"frame") == 0  && */
	   frame_GetChildView(frame) != NULL){
	    for(self = firstlink; self != NULL ; self = self->next){
		if(frame == (struct frame *) arbiterview_GetTrueChild(self)){
		    return(self);
		}
	    }
	    arb = arbiter_New();
	    self = arbiterview_New();
	    arbiterview_SetDataObject(self,arb);
	    if((buf = frame_GetBuffer(frame)) == NULL)
		 arbiter_SetRefName(arb,"NoName");
	    else arbiter_SetRefName(arb,buffer_GetName(buf));
	    cself = (struct celview *) self;
	    cself->child = cself->truechild = (struct view *) frame;
	    frame_AddObserver(frame,self); 
	    cself->TopLevel = TRUE;
	}
    }
    return self;
}

