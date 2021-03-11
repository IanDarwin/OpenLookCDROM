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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/fnote.c,v 1.13 1993/11/30 20:50:36 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <envrment.ih>
#include <text.ih>
#include <style.ih>
#include <viewref.ih>
#include <stylesht.ih>
#include <fnote.eh>

struct fnote **stack , **endstack;
struct text *tmptext;
long notecount;
boolean donumber;
struct style *Style = NULL;
struct style *HStyle = NULL;
#define BACKWARD 0
#define FORWARD 1
#define NOPROCESS 2
#define MAXNOTES 500
#define OPEN TRUE
#define CLOSE FALSE
#define NEW -333
#define FLIP 42

struct style *GetStyle(self,txt,openflag)
struct fnote *self;
struct text *txt;
int openflag;
{
    struct style *Style = NULL;
    if(txt != self->parenttext){
	self->hstyle = self->vstyle = NULL;
	self->parenttext = txt;
    }
    if(openflag == OPEN){
	 if(self->vstyle) return self->vstyle;
	 if(txt && (Style = stylesheet_Find(txt->styleSheet,"footnote" )) == NULL){
	    Style = style_New();
	    style_SetName(Style, "footnote");
	    stylesheet_Add(txt->styleSheet, Style);
	    style_SetFontSize(Style,style_PreviousFontSize,-2);
	    style_AddOverBar(Style);
	}
	self->vstyle = Style;
    }
    else {
	if(self->hstyle) return self->hstyle;
	if(txt && (Style = stylesheet_Find(txt->styleSheet, "hfootnote")) == NULL){
	    Style = style_New();
	    style_SetName(Style, "hfootnote");
	    stylesheet_Add(txt->styleSheet, Style);
	    style_AddHidden(Style);
	}
	self->hstyle = Style;
    }
    return Style;
}

void fnote__addenv(self,txt,pos)
struct fnote *self;
struct text *txt;
long pos;
{
    struct style *Style = NULL;
    struct environment *te;
    Style = GetStyle(self,txt,OPEN);
#ifdef DEBUG
printf("In addenv, Style = %d %s,txt = %d, pos = %d\n",Style,style_GetName(Style),txt,pos);
fflush(stdout);
#endif /* DEBUG */
/*    te = environment_InsertStyle(txt->rootEnvironment,pos, Style, TRUE);*/
    te = environment_WrapStyle(txt->rootEnvironment,pos,1,Style);
    environment_SetStyle(te, FALSE, TRUE);
    self->env = te;
    self->loc = pos;
    self->open = OPEN;
}

static boolean doupdate(self,text,pos,env)
struct fnote *self;
struct text *text;
long pos;
struct environment *env;
{
    static struct environment *lastenv = NULL;
    static long lastpos = -1;
    struct style *st;
    struct viewref *vr;
    char *name,*sn;
    boolean retval = FALSE;
    if(env->type == environment_Style) {
	st = env->data.style;
	if(st != NULL && ((sn = style_GetName(st)) != NULL) &&
	   *sn == 'f' && strcmp(sn,"footnote") == 0){
	    lastenv = env;
	    lastpos = pos;
	}
	else lastenv = NULL;
#ifdef DEBUG
printf("In doupdate, Style = %s,lastenv= %d, pos = %d\n",sn,lastenv);
fflush(stdout);
#endif /* DEBUG */
	
    }
    else if(env->type == environment_View){
	vr = env->data.viewref;
	name = class_GetTypeName(vr->dataObject);
	if(self == NULL) {
	    if(!class_IsTypeByName(name,"fnote")){
		return FALSE;
	    }
	    self = (struct fnote *) vr->dataObject;
	    if(stack != NULL && stack < endstack) *stack++ = self;
	}
	else{
	    if(self !=(struct fnote *) vr->dataObject) return FALSE;
	    retval = TRUE;
	}
	/* we have a fnote */
	if(pos != lastpos)
	    fnote_SetEnv(self,NULL); 
	else {
	    fnote_SetEnv(self,lastenv); 
	    fnote_SetLoc(self,lastpos);
	}
	fnote_SetOwnLoc(self,pos);
	if(strcmp(name,"fnote") == 0) self->notecount = notecount++;
	lastenv = NULL;
    }
    return retval;
}
static boolean copy(self,text)
struct fnote *self;
struct text *text;
{   /* This routine should only be called after doupdate has properly updated the env pointers*/
    long len;
    char buf[64];
    if(self->env == NULL) fnote_addenv(self,text,self->ownloc);
    if(tmptext){
	len = (self->loc + environment_GetLength(self->env) - 1 ) - self->ownloc;
	if(donumber){
	    sprintf(buf,"%d\t",self->notecount);
	    /* should probably superscript this number */
	}
	text_InsertCharacters(tmptext,text_GetLength(tmptext),buf,strlen(buf));

	if(self->ownloc > self->loc){
	    text_AlwaysCopyText(tmptext,text_GetLength(tmptext),text,self->loc,self->ownloc - self->loc);
	}
	text_AlwaysCopyText(tmptext,text_GetLength(tmptext),self,0,fnote_GetLength(self));
	text_AlwaysCopyText(tmptext,text_GetLength(tmptext),text,self->ownloc + 1,len);
	if(text_GetChar(tmptext,text_GetLength(tmptext))!= '\n')
	    text_InsertCharacters(tmptext,text_GetLength(tmptext),"\n",1);
    }
    return TRUE;
}
static boolean fnote_open(self,text)
struct fnote *self;
struct text *text;
{   /* This routine should only be called after doupdate has properly updated the env pointers*/
    long oldmod,selfmod;
    if(self->open == OPEN) return TRUE;
    oldmod = text_GetModified(text);
    selfmod = fnote_GetModified(self);
    if(self->env == NULL) fnote_addenv(self,text,self->ownloc);
    environment_SetStyle(self->env, FALSE, TRUE);
    text_AlwaysCopyText(text,self->loc + 1,self,0,fnote_GetLength(self));
    fnote_Clear(self);
    self->open = OPEN;
    text_RestoreModified(text,oldmod);
    fnote_RestoreModified(self,selfmod);
    return TRUE;
}
static boolean fnote_close(self,text)
struct fnote *self;
struct text *text;
{   /* This routine should only be called after doupdate has properly updated the env pointers*/
    long len;
    long oldmod,selfmod;
    if(self->open == CLOSE) return TRUE;
    oldmod = text_GetModified(text);
    selfmod = fnote_GetModified(self);
    if(self->env == NULL) fnote_addenv(self,text,self->ownloc);
    environment_SetStyle(self->env, FALSE, FALSE);
    len = (self->loc + environment_GetLength(self->env) - 1 ) - self->ownloc;
    fnote_AlwaysCopyText(self,fnote_GetLength(self),text,self->ownloc + 1,len);
    text_AlwaysDeleteCharacters(text,self->ownloc + 1,len);
    if(self->ownloc > self->loc){
	fnote_AlwaysCopyText(self,0,text,self->loc,self->ownloc - self->loc);
	text_AlwaysDeleteCharacters(text,self->loc,self->ownloc - self->loc);
    }
    self->open = CLOSE;
    text_RestoreModified(text,oldmod);
    fnote_RestoreModified(self,selfmod);
    return TRUE;
}
long fnote__GetLocLength(self)
struct fnote *self;
{
    if(self->env)
	return environment_GetLength(self->env) - 1;
    return 0L;
}
static DoAll(text,callBack,order)
struct text *text;
boolean (*callBack)();
int order;
{
    struct fnote *st[MAXNOTES];
    stack = st;
    endstack = st + MAXNOTES;
    if(text){
	text_EnumerateEnvironments(text,0,text_GetLength(text),doupdate,NULL);
	switch(order){
	    case BACKWARD:
		while(--stack >= st)
		    (*(callBack))(*stack,text);
		break;
	    case FORWARD:
		for(endstack = st; endstack < stack; endstack++)
		    (*(callBack))(*endstack,text);
		break;
	    default:
		break;
	}
	text_NotifyObservers(text,0);
    }
    stack = NULL;
}

void fnote__CloseAll(classID,text)
struct classheader *classID;
struct text *text;
{
    DoAll(text,fnote_close,BACKWARD);
}

void fnote__Close(self,text)
struct fnote *self;
struct text *text;
{
/* printf("in close text = %d\n",text); */
    if(text){
	text_EnumerateEnvironments(text,0,text_GetLength(text),doupdate,(long)self);
	if (fnote_close(self,text))
	    text_NotifyObservers(text,0);
    }
}
long fnote__CopyNote(self,text,desttext,count,number)
struct fnote *self;
struct text *text,*desttext;
long count;
boolean number;
{
    tmptext = desttext;
    donumber = number;
    if(count < 0) {
	/* flag indicates the env is already set up */
	copy(self,text);
	notecount = self->notecount;
    }
    else {    
	notecount = count;
	if(text){
	    text_EnumerateEnvironments(text,0,text_GetLength(text),doupdate,(long)self);
	    copy(self,text);
	}
    }
   return notecount;
}

int fnote__CopyAll(classID,text,desttext,count,number)
struct classheader *classID;
struct text *text,*desttext;
long count;
boolean number;
{
    tmptext = desttext;
    notecount = count;
    donumber = number;
    DoAll(text,copy,FORWARD);
    tmptext = NULL;
    return notecount;
}
int fnote__UpdateAll(classID,text,count)
struct classheader *classID;
struct text *text;
long count;
{
    tmptext = NULL;
    notecount = count;
    DoAll(text,NULL,NOPROCESS); /* just assign numbers and update the env */
    return notecount;
}
void fnote__OpenAll(classID,text)
struct classheader *classID;
struct text *text;
{
    DoAll(text,fnote_open,BACKWARD);
}

void fnote__Open(self,text)
struct fnote *self;
struct text *text;
{
    if(text){
	text_EnumerateEnvironments(text,0,text_GetLength(text),doupdate,(long)self);
	if (fnote_open(self,text))
	    text_NotifyObservers(text,0);
    }
}
boolean fnote__IsOpen(self)
struct fnote *self;
{
    return (self->open == OPEN); 
 /*   return (fnote_GetLength(self) == 0); */
}
boolean fnote__InitializeObject(ClassID,self)
struct classheader *ClassID;
struct fnote *self;
{
    self->loc = self->ownloc = self->notecount = -1;
    self->parenttext = NULL;
    self->env =	NULL;
    self->vstyle = self->hstyle = NULL;
    self->open = NEW;
    return TRUE;
}
long fnote__Read(self,file,id)
struct fnote *self;
FILE *file;
long id;
{
    long foo;
    foo = super_Read(self,file,id);
    if(fnote_GetLength(self) == 0) self->open = OPEN;
    else self->open = CLOSE;
    return foo;
}
boolean fnote__InitializeClass(ClassID)
struct classheader *ClassID;
{
    stack = NULL;
    endstack = NULL;
    tmptext = NULL;
    notecount = 0;
    donumber = 0;
    Style = NULL;
    HStyle = NULL;
    return TRUE;
}
char * fnote__ViewName(self)
struct fnote *self;
{
    return "fnotev";
}
