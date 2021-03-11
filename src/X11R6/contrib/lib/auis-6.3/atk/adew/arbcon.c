/* user code begins here for HeaderInfo */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/arbcon.c,v 2.44 1992/12/15 21:25:24 rr2b R6tape $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* user code ends here for HeaderInfo */
#include <andrewos.h>
#include <class.h>
#include <proctbl.ih>
#include <view.ih>
#include <arbiterv.ih>
#include <arbcon.eh>
#include <celv.ih>
#include <controlv.ih>
#include <cel.ih>
/* #include <buttonv.ih>  removed to get past "too many defines" error with a braindead cpp
#include <onoffv.ih>
  */
#include <mentstrv.ih>
#include <value.ih>
#include <clklistv.ih>
/* user code begins here for includes */
#define celview_COVERCHILD 1 /* should be in celv.ch */
#define SEPCHAR '_'
#define ARBCONMESSAGE "Arbcon\nAdew Version 5.0\n"
#include <environ.ih>
#include <complete.ih>
#include <message.ih>
#include <im.ih>
#include <arbiter.ih>
#include <frame.ih>
#include <buffer.ih>
#include <dataobj.ih>
#include <menulist.ih>
#include <bind.ih>
#include <style.ih>
#include <stylesht.ih>
#include <envrment.ih>
#include <cltextv.ih>
#include <text.ih>

void ArbCopyCelCallBack();
void ArbCutCelCallBack();
static struct arbcon *Gself;
static struct arbiterview *OwnArb;
#define INITIALSIZE 512
#define DataObject(A) (A->header.view.dataobject)
#define Cel(A) ((struct cel *) DataObject(A))
#define Arbiter(A) ((struct arbiter *) DataObject(A))
#define Parent(V) (((struct view *)V)->parent)
#define NamedView 0
#define EXISTINGOBJECT 1
#define DEFAULTVIEW 1
#define WRITEINVIEW 0
#define INVISPOS 1
#define VISIBLEPOS 0
#define ARBCONNAME environ_AndrewDir("/lib/arbiters/Arb")
#define LISTNAME environ_AndrewDir("/lib/arbiters/vallist")
#define VWLISTFILE  environ_AndrewDir("/lib/arbiters/viewlist")
static char defaultvwlist[] = "text,fad,table,eq,raster,lookz,lset,page,ness,zip,link,chomp,calc,bush,chart,value bargraphV,value fourwayV,value sliderV,value thumbV,value buttonV,value \
onoffV,value sliderstrV,value thumbstrV,value controlV,value pianoV,value stringV,value enterstrV,value menterstrV,value clicklistV,arbiter" ;
struct menulist *arbconMenus;
static struct atom *atta[7];
static void DoCopy(self,clear)
struct arbcon *self;
boolean clear;
{
   char buf[256],*s,*oldref;
   *buf = '\0';
    if(Gself != NULL){
	if (Gself->currentcelview && (value_GetValue(Gself->ArbLinkCel) == 0)){
	    celview_Copy(Gself->currentcelview);
	    sprintf(buf,"Copying %s",cel_GetRefName(Cel(Gself->currentcelview)));
	    message_DisplayString(NULL,0,buf);
	    clear = FALSE;
	}
	else {
	    FILE *cutFile;
	    struct cel *cl;
	    cl = cel_New();
	    if(setupcel(cl)){
		struct im *mim = im_GetLastUsed();
		cutFile = im_ToCutBuffer(mim);
		if (Gself->currentcelview){
		    oldref = cel_GetRefName(Cel(Gself->currentcelview));
		    if(oldref == NULL) oldref = "";
		    cel_WriteLink(cl,cutFile,im_GetWriteID(),0);
		    if((s = Gself->ViewName) != NULL && *s)
			sprintf(buf,"Copying New %s link to %s",s,oldref);
		    else 
			sprintf(buf,"Copying New link to %s",oldref);
		    clear = FALSE;
		    message_DisplayString(NULL,0,buf);
		}
		else {
		    cel_Write(cl,cutFile,im_GetWriteID(),0);
		    if((s = Gself->ViewName) != NULL && *s)
			sprintf(buf,"Copying new %s-%s inset",Gself->ObjectName,s);
		    else
			sprintf(buf,"Copying new %s object" ,Gself->ObjectName);
		    if(value_GetValue(Gself->ArbApplicationChoice))
			strcat(buf," w/ scroll bar(s)");
		}
		im_CloseToCutBuffer(mim, cutFile);
	    }
	    cel_Destroy(cl);
	}
	if(clear){
	    arbcon_SetCurrentCelview(NULL);
	    SetNotice(buf);
	}
    }
}
static int mystrcmp(s1,s2)
register char *s1,*s2;
{
    if(s1 == NULL && s2 == NULL) return 0;
    if(s1 == NULL || s2 == NULL) return 1;
    return strcmp(s1,s2);
}
static findinlist(lst,cnt,str)
char **lst; 
int cnt;
char *str;
{
    int i;
    for(i = 0; i < cnt; i++,lst++){
	if(*lst == NULL || str == NULL) return -1;
	if(**lst == *str && (*lst == str || strcmp(*lst,str) == 0)){
	    return i;
	}
    }
    return -1;
}
static char *parseobv(str,buf)
register char *str,*buf;
{
    register char *stop = NULL;
    while(*str){
	*buf = *str++;
	if(*buf == ' ') {
	    *buf = '\0';
	    if(stop == NULL || *stop == ' ')
		stop = buf + 1;
	}
	buf++;
    }
    *buf = '\0';
    if(stop && *stop) return stop;
    return NULL;
}
    
static SetNotice(str)
char *str;
{
    if(Gself == NULL) return;
    if(str) {
	int len;
	len = strlen(str);
	strncpy(Gself->arr,str,Gself->arrlen - 1);
	if(len  >= Gself->arrlen - 1){
	    Gself->arr[Gself->arrlen - 2] = '\0';
	    len = Gself->arrlen - 2;
	}
	if(Gself->arr[len - 1] != '\n'){
	    Gself->arr[len++] = '\n';
	    Gself->arr[len] = '\0';
	}
    }
    else {
	char *srs,*vv;
	if((srs = Gself->ObjectName) == NULL || *srs == '\0')
	    sprintf(Gself->arr,"< New Object >");
	else{
	    if((vv = Gself->ViewName) == NULL) vv = "";
	    sprintf(Gself->arr,"< New %s-%s inset>\n",srs,vv);
	}
    }
    value_SetArraySize(Gself->ArbTextEdit,0);
    value_SetString(Gself->ArbTextEdit,"");
    value_SetString(Gself->ArbTextEdit,Gself->arr);
    message_DisplayString(NULL,0,"");
}
static appendlist(lst,cnt,str,TEST)
char **lst;
int cnt;
char *str;
int TEST;
{   /* BUG -- OVERFLOWS NOT DETECTED */
    int next = 1;
    if(TEST){
	if(findinlist(lst,cnt,str) != -1) return cnt;
    }
    while(*str){
	if(*str == ',' || *str == '\n') {
	    *str = '\0';
	    next++;
	}
	else if(*str == '\0') break;
/*	else if(*str == ' ') ; */
	else if(next){
	    lst[cnt++] = str;
	    next = 0;
	}
	str++;
    }
    return cnt;
}
static SetName(cv,abv,name)
struct celview *cv;
struct arbiterview *abv;
char *name;
{

    int count = 0;
    char buf[256],nbuf[256];
    if(abv && cv){
	if(name == NULL || Gself == NULL) return;
	strcpy(nbuf,name);
	name = nbuf;
	if(cv == Gself->currentcelview) 
	    arbcon_DeleteCelview(abv,cv);
	if(arbiterview_registername(abv,cv,name)== FALSE){
	    char *dp,*cp;
	    if((dp = strrchr(name,SEPCHAR) ) != NULL) {
		dp++;
		if(*dp >= '0' && *dp <= '9') count = atoi(dp) + 1;
		dp--;
	    } else count = 1;
	    for(cp = buf; *name != '\0' && name != dp; name++, cp++)
		*cp = *name;
	    do{
		sprintf(cp,"%c%d",SEPCHAR,count++);
	    } while (arbiterview_registername(abv,cv,buf)== FALSE);
	}
    }
}
static long findstring(txt,str)
struct text *txt;
char *str;
{   /* searches text for str by itself on a line */
    register long i,ch,pos,len;
    register char *p;
    if(txt == NULL) return -2 ;
    len = text_GetLength(txt);
    p = str;pos = 0;
    for(i = 0;i < len; i++){
	if((ch = text_GetChar(txt,i)) == '\n'){
	    if (p != NULL && (*p == '\0' || *p == '\n')) return pos;
	    p = str;
	    pos = i + 1;
	}
	else if (p == NULL) continue;
	else if (*p == ch) p++;
	else p = NULL;
    }
    return -1;
}
static void handleclicks(self,ct,position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos,which,type)
struct arbcon *self;
struct cltextview *ct;
long *position;
long *numberOfClicks;
enum view_MouseAction *action;
long *startLeft;
long *startRight;
long *leftPos;
long *rightPos;
long which,type;
{   /* deal with clicks */
    char buf[512];
    int len;
    struct celview *cv;
    struct arbiterview *ab;
    if(type == cltextview_PREPROCESS || Gself == NULL){
	*numberOfClicks = 3;
	return;
    }
    *numberOfClicks = 1;
    len = *rightPos - *leftPos;
    if(*action == view_LeftUp){	
	if(which == 0){
	    if(len > 0){
		text_CopySubString(self->ArbCelList,*leftPos, len,buf,FALSE);
		if(self->CurrentArbiterview != NULL &&
		   (cv = arbiterview_lookupname(self->CurrentArbiterview, buf)) != NULL &&
		   cv != Gself->currentcelview){
		    arbcon_SetCurrentCelview(cv);
		}
	    }
	    if(self->overlay != NULL){
		cltextview_DeleteApplicationLayer(self->ArbCelListView,self->overlay);
		cltextview_Destroy(self->ArbCelListView);
		self->ArbCelListView = NULL;
		self->overlay = NULL;
	    }
	}
	else if(len > 0){
	    text_CopySubString(self->ArbArbList,*leftPos, len,buf,FALSE);
	    if((ab = arbiterview_FindArbByName(buf)) != NULL && ab != Gself->CurrentArbiterview){
		arbcon_SetCurrentArbiterview(ab);
	    }
	}
    }
}
static NewWindow(filename,bflags,AddArb)
char *filename;
int bflags;
boolean AddArb;
{

    struct frame *newFrame;
    struct im *window;
    struct buffer *buffer;
    char *type,*otype;
    if((buffer = buffer_GetBufferOnFile(filename, bflags)) == NULL){
	char buf[1300];
	sprintf(buf,"ERROR: can't create %s",filename);
	message_DisplayString(NULL,0,buf);
	return;
    }
    otype = class_GetTypeName(buffer_GetData(buffer));
    if(strcmp(otype,"arbiter") != 0){
	struct arbiter *ab = NULL;
	struct dataobject *ob,*nb;
	boolean newobject = FALSE;

	nb = ob = buffer_GetData(buffer);
	if(Gself && access(filename,F_OK) == -1 && (type = Gself->ObjectName)!= NULL && *type && strcmp(type,otype) != 0){
	    if(AddArb){
		ab = arbiter_New();
		arbiter_SetObjectByName(ab,type);
		nb = (struct dataobject *) ab;
		newobject = TRUE;
	    }
	    else if(class_IsTypeByName(type,"dataobject")){
		nb = (struct dataobject *)class_NewObject(type);
		newobject = TRUE;
	    }
	}
	else {
	    if(AddArb){
		ab = arbiter_New();
		arbiter_SetObject(ab,ob);
		nb = (struct dataobject *) ab;
	    }
	}
	if(ab) arbiter_SetRefName(ab,"");
	if(nb != ob){
	    buffer_SetData(buffer,nb);
	    if(newobject) dataobject_Destroy(ob);
	}
    }
    if ((newFrame = frame_New()) != NULL) {
	if ((window = im_Create(NULL)) != NULL) {
	    frame_SetCommandEnable(newFrame, TRUE);
	    im_SetView(window, newFrame);
	    frame_PostDefaultHandler(newFrame, "message", frame_WantHandler(newFrame, "message"));
/*
	    if ((tempMessageLine = (struct msghandler *) frame_WantHandler(newFrame, "message")) != NULL) {
		msghandler_DisplayString(messageLine, 0, "");
		messageLine = tempMessageLine;
	    }
*/
	    frame_SetBuffer(newFrame, buffer, TRUE);
	}
	else {
	    fprintf(stderr,"Could not create new window.\n");
	    frame_Destroy(newFrame);
	}
    }
    else {
	fprintf(stderr,"Could not allocate enough memory.\n");
    }
}
arbcon_Create(){
    char foo[1024];
    if(Gself) return;
    strcpy(foo,ARBCONNAME);
    NewWindow(foo,0,FALSE);
}
struct celview *arbcon__currentcelview(ClassID)
struct classheader *ClassID;
{
    if(Gself == NULL) return NULL;
    return Gself->currentcelview;
}
void arbcon__SetCurrentArbiterview(ClassID,ab)
struct classheader *ClassID;
struct arbiterview *ab;
{  
    static char buf[256];
    if(Gself == NULL) return ;
    if(ab == Gself->CurrentArbiterview || ab == OwnArb) return;
    arbcon_SetCurrentCelview(NULL);
    text_Clear(Gself->ArbCelList);
    if(ab == NULL || Arbiter(ab) == NULL){
	Gself->CurrentArbiterview = NULL;
	*buf= '-';
	buf[1] = '\0';
    }
    else {
	arbiterview_GetArbName(ab,buf,256);
	Gself->CurrentArbiterview = ab;
	if(Parent(ab)!= NULL)
	    arbcon_AddArbiter(ab);
	arbiterview_InitArbcon(ab);
#if 0
	if( value_GetValue(Gself->ArbCopyMode) != arbiterview_GetCopyMode(ab))
	    value_SetValue(Gself->ArbCopyMode,arbiterview_GetCopyMode(ab));
#endif /* 0 */
    }
    Gself->ArbiterName = buf;
    text_NotifyObservers(Gself->ArbCelList,0);
}
void arbcon__DeleteArbiter(ClassID,arb)
struct classheader *ClassID;
struct arbiterview *arb;
{
    if(Gself == NULL) return ;
    if(Gself->CurrentArbiterview == arb) arbcon_SetCurrentArbiterview(NULL);
    arbcon_InitArbiters();
}

void arbcon__AddArbiter(ClassID,arb)
struct classheader *ClassID;
struct arbiterview *arb;
{
    char buf[512];
    if(Gself == NULL || arb == OwnArb) return;
    arbiterview_GetArbName(arb,buf,512);
    strcat(buf,"\n");
    if(findstring(Gself->ArbArbList,buf) == -1){
	text_AlwaysInsertCharacters((Gself->ArbArbList),0,buf,strlen(buf));
	text_NotifyObservers(Gself->ArbArbList,0);	
    }
#ifdef DEBUG
printf("Adding %s (%d) to text %d (%s) \n", buf,strlen(buf),Gself->ArbArbList,class_GetTypeName(Gself->ArbArbList)); fflush(stdout);
#endif /* DEBUG */

}
void arbcon__InitArbiters(ClassID)
struct classheader *ClassID;
{
    struct arbiterview *ab,*cab;
    cab = NULL;
    if(Gself == NULL ) return;
    if(Gself->ArbArbList) text_Clear(Gself->ArbArbList);
    for(ab = arbiterview_GetFirstLink(); ab != NULL; ab = ab->next){
	if(arbiterview_InTree(ab)){
	    arbcon_AddArbiter(ab);
	    if(ab != OwnArb && (cab == NULL || ab->celcount > 0)) cab = ab;
	}
	else if(ab == Gself->CurrentArbiterview)
	    arbcon_SetCurrentArbiterview(NULL);
    }
/*    printf("cab = %d, Gself->CurrentArbiterview = %d, ownarb = %d\n\n ",
	    cab,Gself->CurrentArbiterview,OwnArb); fflush(stdout); */
    if(Gself->CurrentArbiterview == NULL && cab != NULL)
	arbcon_SetCurrentArbiterview(cab);
}
static addtypes(cl)
struct cel *cl;
{
/*  Not Currently Supported
    char *obstr,*vwstr;
    int i;
    if((obstr = cel_GetObjectName(cl)) != NULL && *obstr && (i =appendlist(Gself->obnamelist,Gself->obcount,obstr,TRUE)) != Gself->obcount){
	Gself->obcount = i;
	value_SetArraySize(Gself->ArbObjectWheel,Gself->obcount);
    } 
    if((vwstr = cel_GetViewName(cl)) != NULL && *vwstr && ( i =appendlist(Gself->vwlist,Gself->vwcount,vwstr,TRUE)) != Gself->vwcount){
	Gself->vwcount = i;
	value_SetArraySize(Gself->ArbViewWheel,Gself->vwcount);
    }
*/
}
static setupcel(cl)
struct cel *cl;
{
    char *obstr,*vwstr;
    char *name,*str;
    if(Gself == NULL) return FALSE;
    obstr = Gself->ObjectName;
    vwstr = Gself->ViewName;
    str = NULL;
    if(value_GetValue(Gself->ArbLinkCel) && Gself->currentcelview){
	cel_SetObject(cl,Cel(Gself->currentcelview)->dataObject);
	cel_SetLinkName(cl,cel_GetRefName(Cel(Gself->currentcelview)));
	name = cel_GetObjectName(Cel(Gself->currentcelview));
	str =cel_GetRefName(Cel(Gself->currentcelview));
    }
    else if(obstr && *obstr){
/*
	cel_SetObjectByName(cl,obstr);
*/
	cel_SetObjectName(cl,obstr);
	name = obstr;
    }
    else return FALSE;
    if(vwstr && *vwstr){
/*
	if( !class_IsTypeByName(name,"value")){
	    char *cp;
	    for(cp = vwstr; *cp != '\0'; cp++) ;
	    cp--;
	    if(*cp == 'V') return FALSE;
	}
	else cel_SetApplication(cl,cel_VALUE);
*/
	cel_SetViewName(cl,vwstr,FALSE);
    }
    else cel_SetViewName(cl,NULL,TRUE);

    if (value_GetValue(Gself->ArbApplicationChoice) && cel_GetApplication(cl) != cel_VALUE){
#ifdef DEBUG
	printf("setting application\n");
#endif /* DEBUG */
	cel_SetApplication(cl,cel_APPLICATION);
    }
    /*	else  cel_SetApplication(cl,cel_NORMAL); */
    if (str && *str) name = str;
    cel_SetRefName(cl,name);
    addtypes(cl);
    cel_NotifyObservers(cl,0);
    return TRUE;
}

boolean arbcon__InitCel(ClassID,cv,abv)
struct classheader *ClassID;
struct celview *cv;
struct arbiterview *abv;
{
#if 0
    struct cel *cl;
    char  *obstr,*vwstr;
    struct dataobject *dob;
    char *name;
    if((cl = Cel(cv)) == NULL || Gself == NULL) return FALSE;
    if( cl->viewType != NULL && !(cv->NeedsRemade)) return FALSE;
    obstr = Gself->ObjectName;
    vwstr = Gself->ViewName;
    if(value_GetValue(Gself->ArbLinkCel)){
	if((Gself->CurrentArbiterview != abv)  || Gself->currentcelview == NULL || abv == NULL ){
	    /* can't link to objects in other arbiters */
	    return FALSE;
	}
	dob = Cel(Gself->currentcelview)->dataObject;
	cel_SetObject(cl,dob);
	name = class_GetTypeName(dob);
    }
    else if(obstr && *obstr){
	cel_SetObjectByName(cl,obstr);
	name = obstr;
    }
    else return FALSE;
    if(vwstr && *vwstr){
	if(strcmp(name,"value") != 0){
	    char *cp;
	    for(cp = vwstr; *cp != '\0'; cp++) ;
	    cp--;
	    if(*cp == 'V') return FALSE;
	}
	cel_SetViewName(cl,vwstr,FALSE);
    }
    else cel_SetViewName(cl,NULL,TRUE);

    if (value_GetValue(Gself->ArbApplicationChoice) && cel_GetApplication(cl) != cel_VALUE){
#ifdef DEBUG
	printf("setting application\n");
#endif /* DEBUG */
	cel_SetApplication(cl,cel_APPLICATION);
    }
    /*	else  cel_SetApplication(cl,cel_NORMAL); */
    SetName(cv,abv,name);
    addtypes(cl);
    cel_NotifyObservers(cl,0);
    return TRUE;
#else /* 0 */
    return FALSE;
#endif /* 0 */
}

/* REPLACE WITH TEXT_COPYTEXT 
arbcon__EditCurrentCelview SHOULD CAUSE THE CEL TO REINIT ITSELF IF LENGTH OF TEXT = 0
*/
#ifdef NOTUSED
static CopyText(dst,src)
struct text *dst,*src;
{
    char foo[4000];
    int len = text_GetLength(src);
    if(len > 4000) len = 4000;
    text_Clear(dst);
    text_CopySubString(src,0,len,foo,TRUE);
    text_AlwaysInsertCharacters(dst,0,foo,len);
    text_NotifyObservers(dst,NULL);
}
#endif /* NOTUSED */
void arbcon__EditCurrentCelview(ClassID)
struct classheader *ClassID;
{
    struct text *src;
    long i,len,bufsize;
    char foo,*c,*refname,*vtype,buf[256];
    struct cel *cl;
    if(Gself == NULL || Gself->ArbTextEdit == NULL )
	return;
    SetNotice("");
    if(Gself->currentcelview == NULL || (cl = Cel(Gself->currentcelview)) == NULL ){
	return;
    }
    len = 0;
    src = cel_GetScript(cl);
    refname = cel_GetRefName(cl);
    if(refname == NULL) refname = "";
    vtype =cel_GetViewName(cl);
    if(vtype == NULL || *vtype == '\0')
	sprintf(buf,"%s Object. Name@%s\n",cel_GetObjectName(cl),refname);
    else 
	sprintf(buf,"%s-%s Inset. Name@%s\n",cel_GetObjectName(cl),vtype,refname);

    bufsize = strlen(buf);
    if(src) len = text_GetLength(src);
    i = len + bufsize + 2;
    if(Gself->arrlen <= i){
	i++;
	Gself->arr = realloc(Gself->arr,i);
	Gself->arrlen = i;
    }
    strcpy(Gself->arr,buf);
    if(src){
	c = Gself->arr + bufsize;
	for(i = 0; i < len; i++){
	    foo = text_GetChar(src,i);
	    if(foo == '[' || foo == ']' || foo == '<' ||
	       foo == '>' || foo == ')') continue;
	    if(foo == '(') *(c - 1) = '@';
	    else *c++ = foo;
	}
	*c = '\0';
    }
    value_SetArraySize(Gself->ArbTextEdit,0);
    value_SetString(Gself->ArbTextEdit,"");
    value_SetString(Gself->ArbTextEdit,Gself->arr);
    view_WantInputFocus((struct view *)Gself->ArbTextEditView, Gself->ArbTextEditView);
}
static struct celview *currentcelview(ClassID)
struct classheader *ClassID;
{
    if(Gself == NULL) return NULL;
    return Gself->currentcelview;
}
void arbcon__AddCel(ClassID,arb,cl,notify)
struct classheader *ClassID;
struct arbiterview *arb;
struct cel *cl;
boolean notify;
{
    char buf[512];
    struct celview *cv;
    char *str = cel_GetRefName(cl);
    if(str == NULL || *str == '\0' || *str == '-') return;
    strcpy(buf,str);
    strcat(buf,"\n");
    if(Gself == NULL || Gself->ArbCelList == NULL) return;
    if(notify){
	if(arb != Gself->CurrentArbiterview){
	    arbcon_SetCurrentArbiterview(arb);
/*	    return; */
	}
	if(findstring(Gself->ArbCelList,buf) == -1){
	    text_AlwaysInsertCharacters(Gself->ArbCelList,0,buf,strlen(buf));
	    text_NotifyObservers(Gself->ArbCelList,0);
	}
	text_NotifyObservers(Gself->ArbCelList,NULL);
	if(Gself->CurrentArbiterview && ((cv = arbiterview_lookupname(Gself->CurrentArbiterview, str)) != NULL) && cv != Gself->currentcelview){
	    arbcon_SetCurrentCelview(cv);
	}
    }
    else {
	if(findstring(Gself->ArbCelList,buf) == -1){
	    text_AlwaysInsertCharacters(Gself->ArbCelList,0,buf,strlen(buf));
	    text_NotifyObservers(Gself->ArbCelList,0);
	}
    }
    addtypes(cl);
}
void arbcon__DeleteCelview(ClassID,arb,cv)
struct classheader *ClassID;
struct arbiterview *arb;
struct celview *cv;
{
    int loc;
    char *name;
    if(Gself == NULL || cv == NULL) return;

    if(cv == Gself->currentcelview) 
	arbcon_SetCurrentCelview(NULL);
    if(Gself->ArbCelList && 
	Cel(cv) != NULL &&
	cv->arb == Gself->CurrentArbiterview &&
	(name = cel_GetRefName(Cel(cv))) != NULL &&
	*name &&
	(loc = findstring(Gself->ArbCelList,name)) >= 0){
	text_AlwaysDeleteCharacters(Gself->ArbCelList,loc,strlen(name) + 1);
	text_NotifyObservers(Gself->ArbCelList,0);
    }
}
void arbcon__SaveCurrentCelview(ClassID)
struct classheader *ClassID;
{
#if 0
    struct text *src;
    if(Gself == NULL || Gself->ArbText == NULL || Gself->currentcelview == NULL ||
	Cel(Gself->currentcelview) == NULL ||
	(src = cel_GetScript(Cel(Gself->currentcelview))) == NULL) return;
    CopyText( src,Gself->ArbText);
    celview_PostParameters(Gself->currentcelview);
#else /* 0 */
    struct text *src;
    char buf[256],*str,*oldstr,**arr;
    long len,size;
    if(Gself == NULL || Gself->ArbTextEdit == NULL || Gself->currentcelview == NULL ||
	Cel(Gself->currentcelview) == NULL ||
	 value_GetString(Gself->ArbTextEdit) == NULL || value_GetArraySize(Gself->ArbTextEdit) == 0 ||
	value_GetStringArray(Gself->ArbTextEdit) == NULL || *(value_GetString(Gself->ArbTextEdit)) == '\0') return;
/*    CopyText( src,Gself->ArbText); */
    src = cel_GetScript(Cel(Gself->currentcelview));
    size = value_GetArraySize(Gself->ArbTextEdit);
    arr  = value_GetStringArray(Gself->ArbTextEdit);
    size--;
    str = *arr++;
    if(src && arr != NULL && size > 0){
	/* merge arr and value_GetString and write into src */
	long start,end;
	start = end = 0;
	len = text_GetLength(src);
	while((start = text_Index(src,end,'(',len - end))!= EOF){
	    start++;
	    if((end = text_Index(src,start,')',len - start))== EOF) break;
	    if(end== start){
		if(*arr == '\0') continue;
		text_InsertCharacters(src,start,*arr,strlen(*arr));
	    }
	    else if(*arr == '\0') text_DeleteCharacters(src,start,end - start);
	    else text_ReplaceCharacters(src,start,end - start,*arr,strlen(*arr));
	    if(++arr == NULL || --size == 0) break;
	    len = text_GetLength(src);
	}
/*	celview_PostParameters(Gself->currentcelview); */
	cel_NotifyObservers(Cel(Gself->currentcelview),cel_NeedsRepost);
    }
    oldstr = cel_GetRefName(Cel(Gself->currentcelview));

    if(str && *str && oldstr && *oldstr && strcmp(str,oldstr) != 0){
	
	sprintf(buf,"saving attributes and renaming %s to %s",oldstr,str);
	message_DisplayString(NULL,0,buf);
	SetName(Gself->currentcelview,Gself->CurrentArbiterview,str); 
    }
    else {
	sprintf(buf,"saving attributes for %s",oldstr);
	message_DisplayString(NULL,0,buf);
    }

#endif /* 0 */
}

static boolean isarbcon(cv)
struct celview *cv;
{
    struct atom *att,**atp;
    att =  cel_GetRefAtom(Cel(cv));
    for(atp = atta; *atp != NULL; atp++)
	if(*atp == att){
	    return TRUE;
	}
    return FALSE;
    
}

void arbcon__SetCurrentCelview(ClassID,cv)
struct classheader *ClassID;
struct celview *cv;
{
    char *srs;
    char buf[256];
    /* printf("In SetCurrentCelview cv = %d\n",(long)cv); */
   /*  fprintf(stderr,"In scc\n");
    fflush(stderr);
*/
    if(Gself == NULL || Gself->currentcelview == cv ) return;
    if(cv == NULL || Cel(cv) == NULL ||  cel_GetRefName(Cel(cv)) == NULL){
	Gself->currentcelview = NULL;
	if(value_GetValue(Gself->ArbLinkCel))
	    value_SetValue(Gself->ArbLinkCel,FALSE);
	SetNotice("");
    }
    else {
	if(cv->arb == OwnArb) return;
	if(isarbcon(cv)) return;
	Gself->currentcelview = NULL;
	if(value_GetValue(Gself->ArbLinkCel))
	    value_SetValue(Gself->ArbLinkCel,FALSE);
	if(cv->arb != Gself->CurrentArbiterview) 
	    arbcon_SetCurrentArbiterview(cv->arb);
	Gself->currentcelview = cv;
    }
    if(cv != NULL && Cel(cv) != NULL){
	Gself->currentcelview = NULL; /* so enter object won't try to unset */
	if(((srs = cel_GetObjectName(Cel(cv))) != NULL)){
	    Gself->ObjectName = srs;
	}
	else Gself->ObjectName = "";
	if((srs = cel_GetViewName(Cel(cv))) != NULL)
	    Gself->ViewName = srs;
	else Gself->ViewName = "";
	Gself->currentcelview = cv;
	value_SetValue(Gself->ArbApplicationChoice,(cel_GetApplication(Cel(cv))== cel_APPLICATION) ? 1:0);
	/*
	 value_SetValue(Gself->ArbSetVisible,cel_Visible(Cel(cv)) ? VISIBLEPOS: INVISPOS);
	 */
	sprintf(buf,"Setting current celview to %s",cel_GetRefName(Cel(cv)));
	message_DisplayString(NULL,0,buf);
    }
    if(cv) arbcon_EditCurrentCelview();
}
void arbcon__DestroyCurrentCelview(ClassID)
struct classheader *ClassID;
{
    if(Gself != NULL && Gself->currentcelview!= NULL) {
	struct celview *cv;
	cv = Gself->currentcelview;
	/* cel_Destroy(Cel(cv)); */
	celview_UnlinkTree(cv);
	celview_Destroy(cv);
    }
}
static setobview(self,str,docopy)
struct arbcon *self;
char *str;
boolean docopy;
{
    char *vw,*obs,*vws;
    static char buf[128];
    boolean NeedUpdate = FALSE;
    obs = self->ObjectName;
    vws = self->ViewName;
    if(obs == buf) NeedUpdate = TRUE;
    if(str == NULL || *str == '\0' || Gself == NULL) return;
    vw = parseobv(str,buf);
    if(NeedUpdate || obs == NULL || strcmp(obs,buf) != 0){
	if(self->currentcelview != NULL) arbcon_SetCurrentCelview(NULL);
	if(value_GetValue(self->ArbLinkCel) == 1)
	    value_SetValue(self->ArbLinkCel,0);
	self->ObjectName = buf;
    }
    
    if(NeedUpdate || (vws == NULL && vw != NULL) || mystrcmp(vws,vw) != 0){
	if(self->currentcelview != NULL && value_GetValue(self->ArbLinkCel) == 0)
	    arbcon_SetCurrentCelview(NULL);
	Gself->ViewName = vw;
    }
    if(self->currentcelview != NULL && value_GetValue(self->ArbLinkCel) == 0)
	arbcon_SetCurrentCelview(NULL);
     if(docopy)  DoCopy(self,TRUE);
}
static void addobview(v)
struct view *v;
{
     char buf[256],*m,objbuf[256],*vw;
     int which;
     if(Gself == NULL) return;
     if(message_AskForString(NULL,0,"new object to add:",NULL,buf,256) == -1 ) return;
     if(*buf == '\0') return;
     /* should check for valid input here */
     if((which = findinlist(Gself->vwlist,Gself->vwcount,buf)) != -1){
	 /* already on list */
	 setobview(Gself,Gself->vwlist[which],TRUE);
	 return;
     }
     vw = parseobv(buf,objbuf);
     if(!class_IsTypeByName(objbuf,"dataobject")){
	 sprintf(buf,"No known dataobject named %s",objbuf);
	 message_DisplayString(NULL,0,buf);
	 return;
     }
     if(vw && *vw && !class_IsTypeByName(vw,"view")){
	 sprintf(buf,"No known view named %s",vw);
	 message_DisplayString(NULL,0,buf);
	 return;
     }
     m  = malloc(strlen(buf) + 1);
     if (m == NULL) return;
     strcpy(m,buf);
     Gself->vwcount = appendlist(Gself->vwlist,Gself->vwcount,m,FALSE);
     value_SetNotify(Gself->Arbobviewlist,FALSE);
     value_SetArraySize(Gself->Arbobviewlist,Gself->vwcount);
     value_SetNotify(Gself->Arbobviewlist,TRUE);
     value_SetStringArray(Gself->Arbobviewlist,Gself->vwlist);
     setobview(Gself,m,TRUE);
}

static void copy(v)
struct view *v;
{
    if(Gself == NULL) return;
    if(value_GetValue(Gself->ArbLinkCel) == 1)
	value_SetValue(Gself->ArbLinkCel,0);
    else DoCopy(Gself,TRUE);
}
static void copylink(v)
struct view *v;
{
    if(Gself == NULL) return;
    if(Gself->currentcelview == NULL) {
	message_DisplayString(NULL,0,"No current cel to link to");
	return;
    }
    if(value_GetValue(Gself->ArbLinkCel) == 0)
	value_SetValue(Gself->ArbLinkCel,1);
    else  DoCopy(Gself,TRUE);
}
static void cut(v)
struct view *v;
{
    if(Gself == NULL) return;
    ArbCutCelCallBack(Gself,Gself->ArbCutCel,0,0);
}
static void newwin(v)
struct view *v;
{
    char frs[1024],prompt[256],*type;
    if(Gself == NULL) return;
    type = NULL;
    if(Gself && (type = Gself->ObjectName) != NULL && *type){
	sprintf(prompt,"File to read (default object is %s): ",type);
    }
    else sprintf(prompt,"File to read: ");
    if(completion_GetFilename(Gself->v,prompt,"",frs,1024,FALSE,FALSE) == -1)
	return;
    if(type != NULL && *type)
	buffer_SetDefaultObject(type);
    NewWindow(frs,0,FALSE);
    if(type != NULL && *type)
	buffer_SetDefaultObject("text");
}
static void newlist(v)
struct view *v;
{
    char *p;
    arbcon_SetCurrentArbiterview(NULL);
    if((p = environ_GetProfile("ValueFile"))!= NULL && (access(p,4) == 0))
	NewWindow(p,0,FALSE);
    else
	NewWindow(LISTNAME,0,FALSE);
    if(Gself && Gself->CurrentArbiterview)
	arbiterview_SetCopyMode(Gself->CurrentArbiterview,TRUE);
}
#ifdef NOTUSED
static void arbnewwin(v)
struct view *v;
{
    char frs[1024],prompt[256],*type;
    if(Gself == NULL) return;
    if(Gself && (type = Gself->ObjectName) != NULL && *type)
	sprintf(prompt,"File to read into arbiter(default object is %s): ",type);
    else sprintf(prompt,"File to read: ");
    if(completion_GetFilename(Gself->v,prompt,"",frs,1024,FALSE,FALSE) == -1)
	return;
    NewWindow(frs,0,TRUE);
}
#endif /* NOTUSED */
static void init(self)
struct arbcon *self;
{
    if(Gself == NULL) return;
    arbcon_InitArbiters();
    if(self->CurrentArbiterview != NULL && self->ArbCelList) {
	text_Clear(Gself->ArbCelList);
	arbiterview_InitArbcon(self->CurrentArbiterview);
    }
}
static void showcels(v)
struct view *v;
{
    struct rectangle rec;
    struct arbiterview *abv;
    char buf[512];
    if(Gself == NULL) return;
    abv = Gself->arbv;
    init(Gself);
    if(Gself->ArbCelListView != NULL || Gself->CurrentArbiterview == NULL || text_GetLength(Gself->ArbCelList) == 0) {
	message_DisplayString(abv,0,"No Current Cels");
	return;
    }
    Gself->ArbCelListView = cltextview_New();
    cltextview_SetDataObject(Gself->ArbCelListView,Gself->ArbCelList);
    cltextview_AddClickObserver(Gself->ArbCelListView,Gself,handleclicks,0);
    rec.width = arbiterview_GetLogicalWidth(abv);
    rec.height = arbiterview_GetLogicalHeight(abv);
    rec.top = 0;rec.left = 0;
    Gself->overlay = cltextview_GetApplicationLayer(Gself->ArbCelListView);
/*    arbiterview_PushOverlay(abv,Gself->overlay,NULL,celview_COVERCHILD); */
    arbiterview_PushOverlay(abv,Gself->overlay,&rec,celview_COVERCHILD);	
    sprintf(buf,"Click on desired %s cel",Gself->ArbiterName);
    message_DisplayString(abv,0,buf);
}
static void arbchdir(v)
struct view *v;
{
    struct arbiterview *abv;
    char buf[1024],bb[1200];
    int pf;
    if(Gself == NULL) return;
    abv = Gself->arbv;
    *bb = '\0';
    im_GetDirectory(bb);
    pf = completion_GetFilename(abv,"New Directory: ",bb,buf, sizeof(buf),FALSE,FALSE) ;
    if(pf>= 0){
	if(im_ChangeDirectory(buf)== -1){
	    sprintf(bb,"ERROR: Can't cd to %s",buf);
	}
	else sprintf(bb,"New directory is %s",buf);
	message_DisplayString(NULL,0,bb);
    }
}
static void createcon(v)
struct view *v;
{
    struct arbiterview *abv;
    if(Gself == NULL) return;
    if((abv = Gself->CurrentArbiterview) == NULL) {
	message_DisplayString(NULL,0,"No Current Arbiterview"); 
	return;
    }
    arbcon_SetCurrentCelview(NULL);
    arbiterview_CreateCon(abv, (struct text *)Gself->ArbTextEditView->etext);
}

static struct bind_Description arbconBindings[]={
    {"arbcon-copy-cel",NULL,0,"Arbcon~0,Copy Cel~3",0,0,copy,"Copy the current cel"},
    {"arbcon-copy-link",NULL,0,"Arbcon~0,Copy Link~2",0,0,copylink,"Copy link to current cel"},
    {"arbcon-cut-cel",NULL,0,"Arbcon~0,Cut Cel~10",0,0,cut,"Cut the current cel"},
    {"arbcon-new-window",NULL,0,"Arbcon~0,New Window~20",0,0,newwin,"prompt for a file for a new window"},
    {"arbcon-add-object",NULL,0,"Arbcon~0,Add Object~22",0,0,addobview,"prompt for a new object view pair to add to the list"},
    {"arbcon-add-valueview-list",NULL,0,"Arbcon~0,Init Value Window~23",0,0,newlist,"Bring up window of valueviews"},
    {"arbcon-chdir",NULL,0,"ADEW~1,Change Directory~1",0,0,arbchdir,"Change Directory"},
    {"arbcon-creatcon",NULL,0,"ADEW~1,Create Controller~1",0,0,createcon,"Create Controller"},
/* 
    {"arbcon-make-app",NULL,0,"ADEW~2,Make Application~2",0,0,makeapp,"Make Application"},
    {"arbcon-run-app",NULL,0,"ADEW~2,Run Application~2",0,0,runapp,"Run Application"},
*/
/*    {"arbcon-new-arb-window",NULL,0,"Arbcon~0,New Arbiter Window~20",0,0,arbnewwin,"prompt for a file for a new window and surround with an arbiter"},*/
    {"arbcon-show-cels",NULL,0,"Arbcon~0,Show Cels~21",0,0,showcels,"Show cel names"},
    NULL
};
static boolean dolistfile(self,s)
struct arbcon *self;
char *s;
{
    /* open file and append to list of objects */
    FILE *f,*fopen();
    char *buf;
    long size;
    if((f = fopen(s,"r")) == NULL) return FALSE;
    fseek(f,0,2);	
    size = ftell(f);
    fseek(f,0,0);
    if((buf = (char *) malloc(size + 1)) == NULL){
	fclose(f);
	return FALSE;
    }
    fread(buf,1,size,f);
    buf[size] = '\0';
    fclose(f);
    self->vwcount =appendlist(self->vwlist,self->vwcount,buf,FALSE);
    return TRUE;
}
static boolean createGself(self)
struct arbcon *self;
{   /* initialization code */
    FILE *f,*fopen();
    char *p,*m;
    struct classinfo *viewtype = class_Load("view");
    Gself = self;
    controlV_SetAutoInit(FALSE);
    OwnArb = (struct arbiterview *) view_WantHandler(Gself->v,"arbiterview");
    arbiter_SetNoSave(Arbiter(OwnArb),TRUE);
    arbiterview_AddObserver(OwnArb,self);
    self->vwlist = (char **) malloc(INITIALSIZE * sizeof(char *));
    self->vwcount = 0;

    if((p = environ_GetProfile("ObViewList"))!= NULL){
	if((m = malloc(strlen(p)+ 1)) == NULL) return FALSE;
	strcpy(m,p);
	self->vwcount =appendlist(self->vwlist,self->vwcount,m,FALSE);
    }
    if((p = environ_GetProfile("ObViewFile"))!= NULL){
	dolistfile(self,p);
    }
    if(environ_GetProfileSwitch("IgnoreDefaultViewList",FALSE) != TRUE){ 
	char *dflist;
	if((dflist = environ_GetConfiguration("AdewObViewList")) == NULL){
	    if(dolistfile(self,VWLISTFILE))
		dflist = NULL;
	    else dflist = defaultvwlist;
	}
	if(dflist != NULL){
	    if((m = malloc(strlen(dflist)+ 1)) == NULL) return FALSE;
	    strcpy(m,dflist);
	    self->vwcount =appendlist(self->vwlist,self->vwcount,m,FALSE);
	}
    }
    value_SetNotify(self->Arbobviewlist,FALSE);
    value_SetArraySize(self->Arbobviewlist,self->vwcount);
    value_SetNotify(self->Arbobviewlist,TRUE);
    value_SetStringArray(self->Arbobviewlist,self->vwlist);

    atta[0] = atom_Intern("ArbLinkCel");
    atta[1] = atom_Intern("ArbCutCel");
    atta[2] = atom_Intern("ArbApplicationChoice");
    atta[3] = atom_Intern("Arbobviewlist");
    atta[4] = atom_Intern("ArbTextEdit");
    atta[5] = atom_Intern("copycon");
    atta[6] = NULL;
    self->ArbCelListView = NULL; 
    self->ArbCelList = text_New();
    self->ArbArbList = text_New();
    self->ObjectName = "";
    self->ArbiterName = "";
    self->ViewName = "";



    arbconMenus = menulist_New();
    bind_BindList(arbconBindings, NULL , arbconMenus, viewtype);
    
    arbiterview_SetMenulist(self->arbv,arbconMenus);
#ifdef INFOFILE
    if((f = fopen(INFOFILE,"r")) != NULL){
	struct text *txt;
	txt = (struct text *) Gself->ArbTextEditView->etext;
	text_Read(txt,f,1);
	fclose(f);
    }
    else 
#endif /* INFOFILE */
    {
	/* all this to center the title, sigh */
	struct style *Style = NULL;
	struct environment *te;
	char *buf ;
	struct text *txt;

	buf = ARBCONMESSAGE;
	txt = (struct text *) Gself->ArbTextEditView->etext;
	text_AlwaysInsertCharacters(txt,0,buf,strlen(buf));
	if(txt && (Style = stylesheet_Find(txt->styleSheet, "center")) == NULL){
	    Style = style_New();
	    style_SetName(Style, "center");
	    stylesheet_Add(txt->styleSheet, Style);
	    style_SetJustification(Style,style_Centered);
	}
	te = environment_InsertStyle(txt->rootEnvironment, 0, Style, TRUE);
	environment_SetStyle(te, FALSE, FALSE);
	environment_SetLength(te, text_GetLength(txt));
    }
    view_WantInputFocus((struct view *)Gself->ArbTextEditView, Gself->ArbTextEditView);
    view_AddObserver((struct view *)Gself->ArbTextEditView,self);
    init(self);
    return TRUE;
}
/* user code ends here for includes */

static struct arbcon *firstarbcon;
static struct arbcon *FindSelf(v)
struct view *v;
{
	struct arbcon *self,*last = NULL;
	struct arbiterview *arbv =arbiterview_FindArb(v);
	for(self= firstarbcon; self != NULL; self = self->next){
		if(self->arbv == arbv) return self;
		last = self;
		}
	if(Gself != NULL){
	    return Gself;
	}
	self = arbcon_New();
	self->arbv = arbv;
	initself(self,v);
	if(last == NULL) firstarbcon = self;
	else last->next = self;
	return self;
}
static void ArbLinkCelCallBack(self,val,r1,r2)
struct arbcon *self;
struct value *val;
long r1,r2;
{
if(r2 == value_OBJECTDESTROYED) {
	self->ArbLinkCel = NULL;
	self->ArbLinkCelView = NULL;
	return;
}
/* user code begins here for ArbLinkCelCallBack */
#if 0
    if(value_GetValue(val) == 1){
	if(self->currentcelview == NULL){
	    value_SetValue(val,0);
	    message_DisplayString(NULL,0,"No current cel to link to");
	}
	else {
	     DoCopy(self,TRUE);
	}
    }
#endif /* 0 */
    if(self->currentcelview == NULL){
	if(value_GetValue(val) == 1){
	    value_SetValue(val,0);
	    if(Gself)
	    message_DisplayString(NULL,0,"No current cel to link to !");
	}
    }	
    else {
	    DoCopy(self,TRUE);
	}


/* user code ends here for ArbLinkCelCallBack */
}
static void ArbCutCelCallBack(self,val,r1,r2)
struct arbcon *self;
struct value *val;
long r1,r2;
{
if(r2 == value_OBJECTDESTROYED) {
	self->ArbCutCel = NULL;
	self->ArbCutCelView = NULL;
	return;
}
/* user code begins here for ArbCutCelCallBack */
{
    char buf[256];
    if(Gself != NULL && Gself->currentcelview){
	sprintf(buf,"Cutting %s",cel_GetRefName(Cel(Gself->currentcelview)));
	message_DisplayString(NULL,0,buf);
	celview_Copy(Gself->currentcelview);
	arbcon_DestroyCurrentCelview();
	SetNotice(buf);
    }
    else message_DisplayString((Gself == NULL)? NULL : Gself->arbv,0,"No current cel to cut");
}
/* user code ends here for ArbCutCelCallBack */
}
static void ArbApplicationChoiceCallBack(self,val,r1,r2)
struct arbcon *self;
struct value *val;
long r1,r2;
{
if(r2 == value_OBJECTDESTROYED) {
	self->ArbApplicationChoice = NULL;
	self->ArbApplicationChoiceView = NULL;
	return;
}
/* user code begins here for ArbApplicationChoiceCallBack */
    if(Gself == NULL) return;
if (Gself->currentcelview){
    int setto,curval;
    struct cel *cl = Cel(Gself->currentcelview);
    setto = value_GetValue(Gself->ArbApplicationChoice);
    curval = (cel_GetApplication(cl) == cel_APPLICATION);
    if((setto == 0)  !=  (curval == 0)){
	value_SetValue(val,(setto == 0) ? 1:0);
	message_DisplayString(NULL,0,"Can't change application status of existing cel");
    }
}
else DoCopy(self,TRUE);
/* user code ends here for ArbApplicationChoiceCallBack */
}
static void ArbobviewlistCallBack(self,val,r1,r2)
struct arbcon *self;
struct value *val;
long r1,r2;
{
if(r2 == value_OBJECTDESTROYED) {
	self->Arbobviewlist = NULL;
	self->ArbobviewlistView = NULL;
	return;
}
/* user code begins here for ArbobviewlistCallBack */
    setobview(self,value_GetString(val),(value_GetValue(val) == 1));
/* user code ends here for ArbobviewlistCallBack */
}
static void ArbTextEditCallBack(self,val,r1,r2)
struct arbcon *self;
struct value *val;
long r1,r2;
{
if(r2 == value_OBJECTDESTROYED) {
	self->ArbTextEdit = NULL;
	self->ArbTextEditView = NULL;
	return;
}
/* user code begins here for ArbTextEditCallBack */
    arbcon_SaveCurrentCelview();
/* user code ends here for ArbTextEditCallBack */
}
static initself(self,v)
struct arbcon *self;
struct view *v;
{
	self->v = v;
	self->ArbLinkCelView = (struct onoffV *)arbiterview_GetNamedView(v,"ArbLinkCel");
	self->ArbLinkCel = (struct value *)arbiterview_GetNamedObject(v,"ArbLinkCel");
	if(self->ArbLinkCel) value_AddCallBackObserver(self->ArbLinkCel, self,ArbLinkCelCallBack,0);
	if(self->ArbLinkCelView) view_AddObserver((struct view *)self->ArbLinkCelView,self);
	self->ArbCutCelView = (struct buttonV *)arbiterview_GetNamedView(v,"ArbCutCel");
	self->ArbCutCel = (struct value *)arbiterview_GetNamedObject(v,"ArbCutCel");
	if(self->ArbCutCel) value_AddCallBackObserver(self->ArbCutCel, self,ArbCutCelCallBack,0);
	if(self->ArbCutCelView) view_AddObserver((struct view *)self->ArbCutCelView,self);
	self->ArbApplicationChoiceView = (struct onoffV *)arbiterview_GetNamedView(v,"ArbApplicationChoice");
	self->ArbApplicationChoice = (struct value *)arbiterview_GetNamedObject(v,"ArbApplicationChoice");
	if(self->ArbApplicationChoice) value_AddCallBackObserver(self->ArbApplicationChoice, self,ArbApplicationChoiceCallBack,0);
	if(self->ArbApplicationChoiceView) view_AddObserver((struct view *)self->ArbApplicationChoiceView,self);
	self->ArbobviewlistView = (struct clicklistV *)arbiterview_GetNamedView(v,"Arbobviewlist");
	self->Arbobviewlist = (struct value *)arbiterview_GetNamedObject(v,"Arbobviewlist");
	if(self->Arbobviewlist) value_AddCallBackObserver(self->Arbobviewlist, self,ArbobviewlistCallBack,0);
	if(self->ArbobviewlistView) clicklistV_AddObserver(self->ArbobviewlistView,self);
	self->ArbTextEditView = (struct menterstrV *)arbiterview_GetNamedView(v,"ArbTextEdit");
	self->ArbTextEdit = (struct value *)arbiterview_GetNamedObject(v,"ArbTextEdit");
	if(self->ArbTextEdit) value_AddCallBackObserver(self->ArbTextEdit, self,ArbTextEditCallBack,0);
	if(self->ArbTextEditView) view_AddObserver((struct view *)self->ArbTextEditView,self);
}
arbcon_copycon(v,dat)
struct view *v;
 long dat;
{
struct arbcon *self;
if((self = FindSelf(v)) == NULL) return;
/* user code begins here for arbcon_copycon */
if(Gself == NULL){
    if(createGself(self) == FALSE) Gself = NULL;
}
else{
    struct arbiterview *arbv =arbiterview_FindArb(v);
    if(((struct celview *)arbv)->menulist == NULL){
	struct classinfo *viewtype = class_Load("view");
	arbconMenus = menulist_New();
	bind_BindList(arbconBindings, NULL , arbconMenus, viewtype);
	arbiterview_SetMenulist(arbv,arbconMenus);
    }
    /*
      fprintf(stderr,"In CopyCon\n");
      fflush(stderr);
      */
    DoCopy(Gself,TRUE);
}
/* user code ends here for arbcon_copycon */
}
void arbcon__ObservedChanged(self,observed,status)
struct arbcon *self;
struct observable * observed;
long status;
{
/* user code begins here for ObservedChanged */
    if(status == observable_OBJECTDESTROYED &&
	observed == (struct observable * )OwnArb &&
	self == Gself){
	self->ArbTextEditView = NULL;
#ifdef DESTROYFAILS
	OwnArb = NULL;
	Gself = NULL;
#else /*  DESTROYFAILS */
  	arbcon_Destroy(self);  
#endif /*  DESTROYFAILS */
    }
/* user code ends here for ObservedChanged */
    if (status == observable_OBJECTDESTROYED) {
	if (observed == (struct observable *) self->ArbLinkCelView) self->ArbLinkCelView=NULL;
	if (observed == (struct observable *) self->ArbCutCelView) self->ArbCutCelView=NULL;
	if (observed == (struct observable *) self->ArbApplicationChoiceView) self->ArbApplicationChoiceView=NULL;
	if (observed == (struct observable *) self->ArbobviewlistView) self->ArbobviewlistView=NULL;
	if (observed == (struct observable *) self->ArbTextEditView) self->ArbTextEditView=NULL;
    }
}
boolean arbcon__InitializeClass(ClassID)
struct classheader *ClassID;
{
struct classinfo *viewtype = class_Load("view");
firstarbcon = NULL;
proctable_DefineProc("arbcon-copycon",arbcon_copycon, viewtype,NULL,"arbcon copycon");
/* user code begins here for InitializeClass */
proctable_DefineProc("arbcon-create",arbcon_Create, viewtype,NULL,"Create an Arbcon");
Gself= NULL;
/* user code ends here for InitializeClass */
return TRUE;
}
void arbcon__FinalizeObject(ClassID,self)
struct classheader *ClassID;
struct arbcon *self;
{
	if(self->ArbLinkCel) value_RemoveCallBackObserver(self->ArbLinkCel, self);
	if(self->ArbCutCel) value_RemoveCallBackObserver(self->ArbCutCel, self);
	if(self->ArbApplicationChoice) value_RemoveCallBackObserver(self->ArbApplicationChoice, self);
	if(self->Arbobviewlist) value_RemoveCallBackObserver(self->Arbobviewlist, self);
	if(self->ArbTextEdit) value_RemoveCallBackObserver(self->ArbTextEdit, self);
/* user code begins here for FinalizeObject */
	if(self == firstarbcon){
	    firstarbcon = self->next; /* should always be NULL */
	}
	if(self->ArbTextEditView){
	    view_RemoveObserver((struct view *)self->ArbTextEditView,self);
	}
	if(self->ArbCelList)
	    text_Destroy(self->ArbCelList);
	if(self->ArbArbList)
	    text_Destroy(self->ArbArbList);
	    
	if(self == Gself) {
	    Gself = NULL;
	    if(OwnArb){
		arbiterview_RemoveObserver(OwnArb,self);
		OwnArb = NULL;
		controlV_SetAutoInit(TRUE);
	    }
	}
	
/* user code ends here for FinalizeObject */
}
boolean arbcon__InitializeObject(ClassID,self)
struct classheader *ClassID;
struct arbcon *self;
{
self->ArbLinkCel = NULL;
self->ArbLinkCelView = NULL;
self->ArbCutCel = NULL;
self->ArbCutCelView = NULL;
self->ArbApplicationChoice = NULL;
self->ArbApplicationChoiceView = NULL;
self->Arbobviewlist = NULL;
self->ArbobviewlistView = NULL;
self->ArbTextEdit = NULL;
self->ArbTextEditView = NULL;
self->v = NULL;
self->next = NULL;
/* user code begins here for InitializeObject */
{

    self->obsize = self->vwsize =  INITIALSIZE;
    self->currentcelview = NULL;
    self->obcount = self->vwcount = 0;
    self->CurrentArbiterview = NULL;
    self->arrlen = 128;
    self->arr = malloc(self->arrlen);
    self->overlay = NULL;
}
/* user code ends here for InitializeObject */
return TRUE;}
/* user code begins here for Other Functions */
/* user code ends here for Other Functions */
