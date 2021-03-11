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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/controllers/RCS/helpcon.c,v 2.13 1993/09/22 19:21:36 gk5g Exp $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
 
/* user code ends here for HeaderInfo */
#include <andrewos.h>
#include "class.h"
#include "proctbl.ih"
#include "view.ih"
#include "arbiterv.ih"
#include "helpcon.eh"
#include "celv.ih"
#include "cltextv.ih"
#include "text.ih"
#include "controlv.ih"
#include "cel.ih"
#include "lsetv.ih"
#include "lset.ih"
#include "value.ih"
#include "textv.ih"
#include "stringv.ih"
/* user code begins here for includes */
#include "message.ih"
#include "filetype.ih"
#include "environ.ih"
#include <ctype.h>

/* #define HELPDIR "/usr/andy/help" */
#define HELPDIR environ_AndrewDir("/help") 

static NoteHistory(self)
struct helpcon *self;
{   /* insert current file and position into the history text buffer */
    char buf[256];
    struct text *txt =  self->historytext;
    if(*self->CurrentName == '\0') return;
    sprintf(buf,"%s (%d,%d)\n",self->CurrentName,textview_GetDotPosition(self->bodyView), textview_GetDotLength(self->bodyView));
    text_AlwaysInsertCharacters(txt, text_GetLength(txt),buf,strlen(buf));
    text_NotifyObservers(txt,1);
}
static settopics(self)
struct helpcon *self;
{   /* insert current file and position into the history text buffer */
    DIRENT_TYPE **dl;
    char *cp,*dp;
    long count;
    value_SetString(self->topicschoice,self->CurrentType);
    text_Clear(self->topics);
    if(*(self->CurrentType) == '\0')
	text_AlwaysCopyText(self->topics,0,self->historytext,0,text_GetLength(self->historytext));
    else for(dl = self->dl ,count = self->count; count--; dl++){
	cp = (*dl)->d_name;
	dp = strchr(cp,'.');
	if(dp && strcmp((dp + 1),self->CurrentType) == 0){
	    text_AlwaysInsertCharacters(self->topics, text_GetLength(self->topics),cp,dp - cp);
	    text_AlwaysInsertCharacters(self->topics, text_GetLength(self->topics),"\n",1);
	}
    }
    text_NotifyObservers(self->topics,1);
}

static GetHelpOn(self,buf,type,SaveHistory)
struct helpcon *self;
char *buf,*type;
{   /* look up the file in the help directory and insert in the body text */
    char bb[512],*objectName,*cp;
    FILE *f;
    int objectID;
    if((cp = strchr(buf,'(')) != NULL)
	*(cp++ - 1) = '\0';
    if(type && *type)
	sprintf(bb,"%s/%s.%s",HELPDIR,buf,type);
    else 
	sprintf(bb,"%s/%s",HELPDIR,buf);
    if((f = fopen(bb,"r")) == NULL){
	sprintf(bb,"Can't find help on %s",buf);
	message_DisplayString(NULL,0,bb);
	return FALSE;
    }
    if(SaveHistory) NoteHistory(self);
    if(type && *type)
	sprintf(self->CurrentName,"%s.%s",buf,type);
    else 
	sprintf(self->CurrentName,"%s",buf);
    text_Clear(self->body);
    objectName = filetype_Lookup(f, bb, &objectID, NULL);
    rewind(f);
    if(objectID == 0 && (strcmp(objectName,"text") == 0))
	/* reading old be1 datastream file */
	text_Read(self->body,f,0);
    else text_AlwaysInsertFile(self->body,f,bb,0);
    text_NotifyObservers(self->body,1); 
    fclose(f);
    if(cp){
	textview_SetDotPosition(self->bodyView,atoi(cp));
	if((cp = strchr(cp,',')) != NULL){
	    cp++;
	    textview_SetDotLength(self->bodyView,atoi(cp));
	}
	textview_FrameDot(self->bodyView,textview_GetDotPosition(self->bodyView));
    }
    return TRUE;
}
static void handleclicks(self,cv,position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos,which,type)
struct helpcon *self;
struct cltextview *cv;
long *position;
long *numberOfClicks;
enum view_MouseAction *action;
long *startLeft;
long *startRight;
long *leftPos;
long *rightPos;
long which,type;
{   /* deal with clicks */
    char buf[256],*cp;
    int len;
    int SaveHistory = TRUE;
    if(type == cltextview_PREPROCESS){
	*numberOfClicks = 3;
	return;
    }
    *numberOfClicks = 1;
    len = *rightPos - *leftPos;
    if(*action == view_LeftUp &&  len > 0 && len < 256){	
	if(which != 0){
	    text_CopySubString(self->choice,*leftPos, len,self->CurrentType,FALSE);
	    if(strcmp(self->CurrentType,"History") == 0){
	       *self->CurrentType = '\0';
	       self->ShowHistory = TRUE;
	       }
	    else self->ShowHistory = FALSE;
	    settopics(self);
	}
	else {
	    text_CopySubString(self->topics,*leftPos, len,buf,FALSE);
	    if((! self->ShowHistory) || which != 0){
		if((cp = strchr(buf,'\n')) != NULL) *cp = '\0';
	    }
	    else SaveHistory = FALSE;
	    GetHelpOn(self,buf,(self->ShowHistory ? NULL:self->CurrentType),SaveHistory);
	    text_NotifyObservers(self->body,1);
	    if(!SaveHistory)textview_WantInputFocus(self->bodyView,self->bodyView);
	}
    }
}

/* user code ends here for includes */

static struct helpcon *firsthelpcon;
static struct helpcon *FindSelf(v)
struct view *v;
{
	struct helpcon *self,*last = NULL;
	struct arbiterview *arbv = (struct arbiterview *) view_WantHandler(v,"arbiterview");
	for(self= firsthelpcon; self != NULL; self = self->next){
		if(self->arbv == arbv) return self;
		last = self;
		}
	self = helpcon_New();
	self->arbv = arbv;
	initself(self,v);
	if(last == NULL) firsthelpcon = self;
	else last->next = self;
	return self;
}
static void topicschoiceCallBack(self,val,r1,r2)
struct helpcon *self;
struct value *val;
long r1,r2;
{
/* user code begins here for topicschoiceCallBack */
/* user code ends here for topicschoiceCallBack */
}
static void choicelabelCallBack(self,val,r1,r2)
struct helpcon *self;
struct value *val;
long r1,r2;
{
/* user code begins here for choicelabelCallBack */
/* user code ends here for choicelabelCallBack */
}
static initself(self,v)
struct helpcon *self;
struct view *v;
{
	self->v = v;
	self->topicsView = (struct cltextview *)arbiterview_GetNamedView(v,"topics");
	self->topics = (struct text *)arbiterview_GetNamedObject(v,"topics");
	self->choiceView = (struct cltextview *)arbiterview_GetNamedView(v,"choice");
	self->choice = (struct text *)arbiterview_GetNamedObject(v,"choice");
	self->topicschoiceView = (struct stringV *)arbiterview_GetNamedView(v,"topicschoice");
	self->topicschoice = (struct value *)arbiterview_GetNamedObject(v,"topicschoice");
	if(self->topicschoice) value_AddCallBackObserver(self->topicschoice, self,topicschoiceCallBack,0);
	self->bodyView = (struct textview *)arbiterview_GetNamedView(v,"body");
	self->body = (struct text *)arbiterview_GetNamedObject(v,"body");
	self->choicelabelView = (struct stringV *)arbiterview_GetNamedView(v,"choicelabel");
	self->choicelabel = (struct value *)arbiterview_GetNamedObject(v,"choicelabel");
	if(self->choicelabel) value_AddCallBackObserver(self->choicelabel, self,choicelabelCallBack,0);
}
static helpcon_help(v,dat)
struct view *v;
 long dat;
{
struct helpcon *self;
if((self = FindSelf(v)) == NULL) return;
/* user code begins here for helpcon_help */
{
    int alphasort();
    char *cp,*dp,*c,buf[1024];
    DIRENT_TYPE **dl;
    long count,len;
     if(self->historytext == NULL){
	 /* initialize the history text and set up the click observers */
	 arbiterview_SetIgnoreUpdates(v, TRUE);
	 self->historytext = text_New();
	 self->count = scandir(HELPDIR,&dl,NULL,alphasort);
	 self->dl = dl;
	 if(self->topics) cltextview_AddClickObserver(self->topicsView,self,handleclicks,0);
	 if(self->choice) cltextview_AddClickObserver(self->choiceView,self,handleclicks,1);
	 text_InsertCharacters(self->historytext,0,"History\n\n",9);
	 message_DisplayString(NULL,0,"Help is now initialized");
	 return;
     }
     if(self->bodyView != NULL){
	 char helpchoice[264];
	 int dotlen = textview_GetDotLength(self->bodyView);
	 if( dotlen > 0 && dotlen < 256 ){
	     /* use the selected string as the help topic */
	     text_CopySubString(self->body, textview_GetDotPosition(self->bodyView) ,dotlen,helpchoice,FALSE);
	 }
	 else /* prompt for help topic  */
	     if(message_AskForString(NULL,0,"Enter a keyword: ",NULL,helpchoice,256) < 0) return;
	 for(c = helpchoice; *c != '\0'; c++){
	     if(isupper(*c)) *c = tolower(*c);
	     if(isspace(*c)) {
		 *c = '\0';
		 break;
	     }
	 }
	 len =  strlen(helpchoice);
	 for(dl = self->dl ,count = self->count; count--; dl++){
	     cp = (*dl)->d_name;
	     dp = strchr(cp,'.');
	     if(dp - cp == len && 
		strncmp(cp,helpchoice,len) == 0){
		 GetHelpOn(self,cp,NULL,TRUE);
		 return;
	     }
	 }
	 sprintf(buf,"Can't find help on %s",helpchoice);
	 message_DisplayString(NULL,0,buf);
     }
}
/* user code ends here for helpcon_help */
 }
boolean helpcon__InitializeClass(ClassID)
struct classheader *ClassID;
{
struct classinfo *viewtype = class_Load("view");
firsthelpcon = NULL;
proctable_DefineProc("helpcon-help",helpcon_help, viewtype,NULL,"helpcon help");
/* user code begins here for InitializeClass */
/* user code ends here for InitializeClass */
return TRUE;
}
boolean helpcon__InitializeObject(ClassID,self)
struct classheader *ClassID;
struct helpcon *self;
{
self->topics = NULL;
self->topicsView = NULL;
self->choice = NULL;
self->choiceView = NULL;
self->topicschoice = NULL;
self->topicschoiceView = NULL;
self->body = NULL;
self->bodyView = NULL;
self->choicelabel = NULL;
self->choicelabelView = NULL;
self->v = NULL;
self->next = NULL;
/* user code begins here for InitializeObject */
self->historytext = NULL;
self->ShowHistory = FALSE;
*self->CurrentName = '\0';
/* user code ends here for InitializeObject */
return TRUE;}
/* user code begins here for Other Functions */
/* user code ends here for Other Functions */

