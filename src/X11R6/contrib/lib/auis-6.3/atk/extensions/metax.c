/*********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/metax.c,v 1.8 1993/01/08 16:31:54 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <class.h>
#include <stdio.h>

#include <metax.eh>

#include <text.ih>
#include <proctbl.ih>
#include <view.ih>
#include <message.ih>
#include <im.ih>
#include <observe.ih>
#include <buffer.ih>
#include <frame.ih>
#include <framev.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <complete.ih>
#include <environ.ih>
#include <framemsg.ih>
#include <style.ih>
#include <fontdesc.ih>

static struct style *fixed=NULL,*boldulined=NULL,*heading=NULL,*columns=NULL;

struct helpRock {
    procedure HelpWork;
    struct text *text;
    long rock;
    char *partial;
};

static char spaces[]="                               ";
#define MAXFUNLEN sizeof(spaces)

static void LoadClass(partial)
char *partial;
{
    char class[100];
    int i;
    for(i = 0; (i < strlen(partial)) && (partial[i] != '-'); i++) class[i] = partial[i];
    if(i <= 99) {
	class[i]=0;
	(void) class_Load(class);
    }
}

static long match(pe,h)
struct proctable_Entry *pe;
struct helpRock *h;
{
    char buf[1024];
    int len;
    char *name=proctable_GetName(pe);
    char *doc=proctable_GetDocumentation(pe);
    if(!name) name="";
    if(!doc) doc="";
    if(!strncmp(name, h->partial, strlen(h->partial))) {
	bzero(buf,sizeof(buf));
	strncpy(buf,name,254);
	len=strlen(buf);
	if(len<MAXFUNLEN) strcat(buf,spaces+len);
	strcat(buf," ");
	strncat(buf,doc,737);
	strcat(buf,"\n");
	(*h->HelpWork)(h->rock,message_HelpGenericItem,buf,NULL);
    }
    return 0;
}


static void helpProc(partial,myrock,HelpWork,rock)
char *partial;
struct helpRock *myrock;
procedure HelpWork;
long rock;
{
    struct text *t=myrock->text;
    if(!HelpWork) return;
    (*HelpWork)(rock,
		    message_HelpGenericItem,
		    "Proctable Listing\nName\t\t\t\tDocumentation\n",
		    NULL);
    
    myrock->HelpWork=HelpWork;
    myrock->rock=rock;
    myrock->partial=partial;
    (void) LoadClass(partial);
    (void)proctable_Enumerate(match,(long)myrock);
    if(!fixed) return;
    text_SetGlobalStyle(t,fixed);
    text_AddStyle(t,0,17,heading);
    text_AddStyle(t,18,4,boldulined);
    text_AddStyle(t,26,13,boldulined);
    text_AddStyle(t,39,text_GetLength(t)-39,columns);
}

static char *GetProcName(pe)
struct proctable_Entry *pe;
{
    return proctable_GetName(pe)?proctable_GetName(pe):"";
}

static boolean myCompletionWork(pe, data)
struct proctable_Entry *pe;
struct result *data;
{
    completion_CompletionWork(GetProcName(pe), data);
    return FALSE;
}

static enum message_CompletionCode mycomplete(partial, dummyData, buffer, bufferSize)
    char *partial;
    long dummyData; /* Just along for the ride... */
    char *buffer;
    int bufferSize;
{
    struct result result;
    char textBuffer[1024];
    *textBuffer = 0;
    result.partial = partial;
    result.partialLen = strlen(partial);
    result.bestLen = 0;
    result.code = message_Invalid;
    result.best = textBuffer;
    result.max = sizeof(textBuffer) - 1; /* Leave extra room for a NUL. */
    (void) LoadClass(partial);
    (void)proctable_Enumerate(myCompletionWork, &result);

    strncpy(buffer, result.best, bufferSize);
    if (result.bestLen == bufferSize) /* Now make sure buffer ends in a NUL. */
        buffer[result.bestLen] = 0;
    return result.code;
}

static int donothing(vw)
struct view *vw;
{  
    message_DisplayString(vw, 0, "No such function.");
    im_ForceUpdate();
}

static int (*dofunc(func))()
char *func;
{
    struct proctable_Entry *proc = proctable_Lookup(func);
    if(proc) return proctable_GetFunction(proc);
    else return donothing;
}

static boolean getfunction(v,buf,size,prompt,initial)
struct view *v;
char *buf,*prompt,*initial;
int size;
{
    struct helpRock myrock;
    struct framemessage *fmsg=(struct framemessage *)view_WantHandler(v,"message");
    if(!fmsg || !class_IsTypeByName(class_GetTypeName(fmsg),"framemessage")) return;

    if(fmsg) {
	struct buffer *b=frame_GetHelpBuffer(fmsg->frame);
	myrock.text=(struct text *)buffer_GetData(b);
    } else myrock.text=NULL;
    
    if(message_AskForStringCompleted(v, 0, prompt, initial, buf, size, NULL, mycomplete, helpProc, &myrock, message_MustMatch | (initial?0:message_NoInitialString))) {
	message_DisplayString(v,0,"Cancelled");
	return FALSE;
    }
    return TRUE;
}

static boolean getarg(v,arg,size,prompt,initial,result)
struct view *v;
char *arg,*prompt,*initial;
long *result;
{

    if(message_AskForString(v, 0,prompt, initial, arg, size) != 0) return FALSE;
    switch(*arg) {
	case '"':
	    *result=(long)arg+1;
	    break;
	case '#':
	    *result=(long)atol(arg+1);
	    break;
	case '\'':
	    *result=(long) *(arg+1);
	    break;
	default:
	    *result=(long)arg;
    }
    return TRUE;
}

static void metax(tv,argument)
struct view *tv;
long argument;
{
    char cbuf[500];
    struct proctable_Entry *proc;
    struct im *im = view_GetIM(tv);
    if(!getfunction(tv,cbuf,sizeof(cbuf),"Function: ",NULL)) return;
    proc = proctable_Lookup(cbuf);
    if(proc) {
	switch(keystate_DoProc(im->keystate, proc, argument, tv)) {
	case keystate_NoProc:
	    message_DisplayString(im, 0, "Could not load procedure");
	    break;
	case keystate_TypeMismatch:
	    message_DisplayString(im, 0, "Bad command");
	    break;
	}
    }
    else
	donothing((struct view *)tv);
}

static void metax2(tv,argument)
struct view *tv;
long argument;
{
    char cbuf[500], arg[500];
    struct proctable_Entry *proc;
    struct im *im = view_GetIM(tv);
    if(!getfunction(tv,cbuf,sizeof(cbuf),"Function: ",NULL)) return;
    if(!getarg(tv,arg,sizeof(arg),"Argument: ",NULL,&argument))
	return;
    proc = proctable_Lookup(cbuf);
    if(proc) {
	switch(keystate_DoProc(im->keystate, proc, argument, tv)) {
	case keystate_NoProc:
	    message_DisplayString(im, 0, "Could not load procedure");
	    break;
	case keystate_TypeMismatch:
	    message_DisplayString(im, 0, "Bad command");
	    break;
	}
    }
    else
	donothing((struct view *)tv);
}

boolean metax__InitializeClass(classID)
struct classheader *classID;
{
    struct classinfo *info = class_Load("view");
    fixed=style_New();
    if(!fixed) return FALSE;
    boldulined=style_New();
    if(!boldulined) {
	style_Destroy(fixed);
	return FALSE;
    }
    heading=style_New();
    if(!heading) {
	style_Destroy(fixed);
	style_Destroy(boldulined);
	return FALSE;
    }
    columns=style_New();
    if(!columns) {
	style_Destroy(heading);
	style_Destroy(fixed);
	style_Destroy(boldulined);
	return FALSE;
    }
    
    style_SetNewLeftMargin(columns, style_LeftMargin, 550000, style_CM); 
    style_SetNewIndentation(columns, style_LeftMargin, -550000, style_CM);
    style_SetFontSize(heading,style_ConstantFontSize,20);
    style_SetJustification(heading,style_Centered);
    style_SetFontFamily(fixed, "AndyType");
    style_AddNewFontFace(fixed, fontdesc_Fixed);
    style_SetFontSize(fixed,style_ConstantFontSize,10);
    style_Copy(fixed,boldulined);
    style_AddUnderline(boldulined);
    style_AddNewFontFace(boldulined,fontdesc_Bold);
    proctable_DefineProc("metax", metax, info, NULL, "Executes a proctable function by name.");
    proctable_DefineProc("metax-with-arg", metax2, info, NULL, "Executes a function by name, prompting for an argument.");
    return TRUE;
}
