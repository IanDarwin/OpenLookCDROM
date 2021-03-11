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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textaux/RCS/contentv.c,v 1.22 1993/01/08 16:34:47 rr2b R6tape $";
#endif


 

/*  Modified 3/15/90  cch@mtgzx.att.com
 *  Made it so that any change in dot position scrolls other window.
   Modified so that scrolls to main window put selection on top line.
 */
#include <class.h>
#include <ctype.h>
#include <bind.ih>
#include <view.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <content.ih>
#include <message.ih>
#include <mark.ih>
#include <buffer.ih>
#include <im.ih>
#include <frame.ih>
#include <environ.ih>
#include <print.ih>
#include <complete.ih>
#include <textv.ih>
#include <text.ih>
#include <style.ih>
#include <stylesht.ih>
#include <fontdesc.ih>
#include <proctbl.ih>
#include <tindex.ih>

#include "contentv.eh"
static struct menulist *contentvMenus;
static struct keymap *contentvKeyMap;
static int ScrollTop = FALSE;
#define Data(self) ((struct content *)(((struct view *) self)->dataobject))
#define Text(v)	(struct text *) ((v)->header.view.dataobject)
#define Srctext(self) (Data(self)->srctext)
boolean contentv__InitializeObject(classID,self)
struct classheader *classID;
struct contentv *self;
{
    self->menus = menulist_DuplicateML(contentvMenus, self);
    self->srcview = NULL;
    return TRUE;
}
boolean contentv__FinializeObject(classID,self)
struct classheader *classID;
struct contentv *self;
{
     if(self->menus){
	menulist_Destroy(self->menus);
	self->menus = NULL;
    }
    return TRUE;
}
static void reinit(self,value)
struct contentv *self;
long value;
{
    struct content *ct;
    long  pos = contentv_GetDotPosition(self);

    ct = Data(self);
    content_reinit(ct);
    contentv_SetDotPosition(self,pos);
    contentv_FrameDot(self,pos);
}
static void enumerate(self,value)
struct contentv *self;
long value;
{
    struct content *ct;
    long len,tlen;
    ct = Data(self);
    if((len = contentv_GetDotLength(self)) > 0){
	char buf[128],iname[128];
	long stlen;
	long pos = contentv_GetDotPosition(self);
	tlen = content_GetLength(ct);
	len--;
	while((tlen > pos + len ) && (content_GetChar(ct,pos + len) != '\n')) len++ ;
	len++;
	if((stlen = content_StringToInts(ct,pos,NULL)) > 0 && stlen < 127)
	    content_CopySubString(ct,pos,stlen - 1,buf,FALSE);
	else {
	    buf[0] = '0';
	    buf[1] = '\0';
	}
	stlen = message_AskForString(self,60,"Starting number ",buf, iname, sizeof(iname));
	if (stlen < 0){
	    message_DisplayString(self, 0, "Punt!");
	    return;
	}
	if(strlen(iname) == 0)content_Enumerate(ct,pos,len,NULL);
	else {
	    strcat(iname,"\t");
	    content_Enumerate(ct,pos,len,iname);
	}
    }
    else {
	content_Enumerate(ct,-1,0,NULL);
    }
}
static int LocateInView(v1,v2,v3,dat)
struct view *v1,*v2,*v3;
long dat;
{
    struct mark *m = (struct mark *) dat;
    if(class_IsTypeByName(class_GetTypeName(v2),"textview")){
	struct textview *tv = (struct textview *)v2;
	textview_SetDotPosition(tv,mark_GetPos(m));
	textview_SetDotLength(tv,mark_GetLength(m));
	if (ScrollTop)
	    textview_SetTopPosition(tv, mark_GetPos(m));
	else textview_FrameDot(tv,mark_GetPos(m));
    }
    return 0; /* go through all views */
}
static void locate(self,value)
struct contentv *self;
long value;
{
    struct content *ct;
    struct mark *loc;
    struct buffer *buf;
    ct = Data(self);
    loc = content_locate(ct,contentv_GetDotPosition(self));
    if(loc == NULL) return;

    if(self->srcview)
	LocateInView(NULL,(struct view *)self->srcview,NULL,(long) loc);
    else {
	buf = buffer_FindBufferByData((struct dataobject *)ct->srctext);
	if(buf)
	    buffer_EnumerateViews(buf,LocateInView,(long) loc);
    }
}
static void denumerate(self,value)
struct contentv *self;
long value;
{
    struct content *ct;
    long len,pos,tlen;
    ct = Data(self);
    if((len = contentv_GetDotLength(self)) > 0){
	pos = contentv_GetDotPosition(self);
	tlen = content_GetLength(ct);
	len--;
	while((tlen > pos + len ) && (content_GetChar(ct,pos + len) != '\n')) len++ ;
	len++;
	content_Denumerate(ct,pos,len);
    }
    else content_Denumerate(ct,-1,0);
}
struct contentv_cntr {
struct buffer *buf;
int tc,bc;
};
static int check(fr, rock)
    struct frame *fr;
    struct contentv_cntr *rock;
{
     rock->tc++;
     if(frame_GetBuffer(fr) == rock->buf) rock->bc++;
     return FALSE;
}
static struct frame *getframe(vw)
struct view *vw;
{
    while (vw->parent != NULL){
	vw = vw->parent;
	if(class_IsTypeByName(class_GetTypeName(vw),"frame")){
	    return (struct frame *) vw;
	}
    }
    return NULL;
}
static void destroy(self,value)
struct contentv *self;
long value;
{
    struct buffer *buffer;
    struct frame *fr;
    struct proctable_Entry *pr;
    int (*proc)();
    struct contentv_cntr cc;
    struct content *content = (struct content*) contentv_GetDataObject(self);
    if((pr = proctable_Lookup("frame-delete-window")) != NULL && 
	proctable_Defined(pr) &&
	(buffer = buffer_FindBufferByData(Data(self))) != NULL &&
	(fr = getframe((struct view *) self)) != NULL){
	proc = proctable_GetFunction(pr) ;
	cc.tc = cc.bc = 0;
	cc.buf = buffer;
	frame_Enumerate(check, &cc);
	frame_SetBuffer(fr,NULL,0);
	(*proc)(fr,0);
	if(cc.tc > 1 &&  cc.bc == 1) {
	    content_RemoveObserver(content, buffer);
	    buffer_Destroy(buffer);
	    content_Destroy(content);
	}
    }
}
static void contentv_MakeContents(self)
    register struct textview *self;
{
    contentv_MakeWindow(Text(self));
}
void contentv__GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos)
    struct contentv *self;
    long position;
    long numberOfClicks;
    enum view_MouseAction action;
    long startLeft;
    long startRight;
    long *leftPos;
    long *rightPos;
    {
	super_GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos);
	if(numberOfClicks == 1 && (action == view_LeftUp || action == view_RightUp))
	    locate(self,0);
    }
static boolean findframe(fr,buf)
struct frame *fr;
struct buffer *buf;
{
    if(frame_GetBuffer(fr) == buf) return TRUE;
    return FALSE;
}
static void contentv_doprint(self,type)
register struct contentv *self;
char *type;
{
    struct buffer *bu;
    struct frame *fr;
    struct text *txt;
    struct content *ct;
    struct proctable_Entry *pr;
    int (*proc)();
    char *saveenv = NULL;

    ct = Data(self);
    txt = ct->srctext;
    if(txt == NULL || (bu = buffer_FindBufferByData((struct dataobject *) txt))== NULL) {
	message_DisplayString(self,0,"Can't find buffer for source");
	return;
    }
    if((fr = frame_Enumerate(findframe,(long) bu)) == NULL){
	message_DisplayString(self,0,"Can't find view for source");
	return;
    }
    if((pr = proctable_Lookup(type)) != NULL && proctable_Defined(pr) ){
	saveenv = environ_Get("PrintContents");
	proc = proctable_GetFunction(pr) ;
	environ_Put("PrintContents","yes");
	(*proc)(fr,0);
	if (saveenv)
	    environ_Put("PrintContents", saveenv);
	else
	    environ_Delete("PrintContents");
    }
    else {
	char errstring[50];
	sprintf (errstring, "Can't find proctable entry '%s'.", type);
	message_DisplayString(self, 0, errstring);
	return;
    }
}
static void contentv_PreviewCmd(self)
    struct contentv *self;
{
    contentv_doprint(self,"frame-preview");	
}
static void contentv_PrintCmd(self)
    struct contentv *self;
{
    contentv_doprint(self,"frame-print");
}

static struct bind_Description contentvBindings[]={
    {"contentv-reinit",NULL,0,"Contents~1,Update Contents~20", 0,0,reinit,"reinitialize the headings"},
    {"contentv-enumerate",NULL,0,"Contents~1,Enumerate~1",0,0,enumerate,"Enumerate the heading"},
    {"contentv-denumerate",NULL,0,"Contents~1,Denumerate~2",0,0,denumerate,"Denumerate the heading"},
    {"contentv-destroy",NULL,0,"Quit~99",0,0,destroy,"destroy the table of content"}, 
    {"contentv-delete-window",NULL,0,"Delete Window~89",0,0,destroy,"destroy the table of content"},
    {"contentv-preview",		    NULL,0,	    "File~10,Preview~21",0,0, contentv_PreviewCmd, "Previews document."},
    {"contentv-print",		    NULL,0,	    "File~10,Print~22",0,0, contentv_PrintCmd, "Prints document."},
    NULL
};

void contentv__PostMenus(self, menulist)
struct contentv *self;
struct menulist *menulist;
{
    menulist_ClearChain(self->menus);
    menulist_ChainBeforeML(self->menus, menulist, menulist);
    super_PostMenus(self, self->menus);
}
boolean contentv__InitializeClass(classID)
    struct classheader *classID;
{
    struct classinfo *textviewtype = class_Load("textview");
    contentvMenus = menulist_New();
    contentvKeyMap =  keymap_New();
    bind_BindList(contentvBindings, contentvKeyMap , contentvMenus, &contentv_classinfo);
  proctable_DefineProc("contentv-make-window",contentv_MakeContents,textviewtype,NULL,"Make a table of contents window");
    ScrollTop = environ_GetProfileSwitch("ContentsScrollTop", FALSE);

    return TRUE;
}
static void contentv__MakeWindow(classID,txt)
struct classinfo *classID;
struct text *txt;
{
    char buf[1024];
    struct content *ct;
    struct buffer *buffer;
    struct frame *fr;
    struct im *window;
    if((buffer = buffer_FindBufferByData((struct dataobject *)txt)) != NULL) {
	sprintf(buf,"Contents_%s",buffer_GetName(buffer));
    }
    else sprintf(buf,"Table_of_Contents");
    if((ct = content_New()) == NULL) {
	fprintf(stderr,"Could not allocate enough memory.\n");
	return;
    }
    content_SetSourceText(ct,txt);
    if((buffer = buffer_Create(buf,NULL,"contentv",ct)) == NULL) {
	fprintf(stderr,"Could not allocate enough memory.\n");
	return;
    }
    if ((window = im_Create(NULL)) != NULL) {
	fr = frame_New();
	frame_SetCommandEnable(fr, TRUE);
	im_SetView(window, fr);
	frame_PostDefaultHandler(fr, "message", frame_WantHandler(fr, "message"));
	frame_SetBuffer(fr, buffer, TRUE);
	buffer_SetScratch(buffer,TRUE);
    }
    else {
	fprintf(stderr,"Could not allocate enough memory.\n");
	if(buffer) buffer_Destroy(buffer);
	if(ct) content_Destroy(ct);
    }
}

void contentv__SetDotPosition(self, newpos)
struct contentview *self;
long newpos; {
    super_SetDotPosition(self, newpos);
    locate(self, 0);
}
