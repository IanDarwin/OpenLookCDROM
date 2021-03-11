/* Copyright 1992 Andrew Toolkit Consortium, Carnegie Mellon University */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nessmv.c,v 1.8 1993/07/23 00:20:51 rr2b Exp $";
#endif






#include <andrewos.h>
#include <class.h>

#include <nessmv.eh>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <complete.ih>
#include <buffer.ih>
#include <im.ih>
#include <frame.ih>
#include <framemsg.ih>
#include <message.ih>
#include <text.ih>
#include <ness.ih>
#include <cursor.ih>

static struct cursor *waitcursor=NULL;

static int waitcount=0;

static void WaitOn()
{
    struct im *last=im_GetLastUsed();
    waitcount++;

    if(waitcount==1) {

	if(last==NULL || waitcursor!=NULL) return;

	waitcursor=cursor_Create(last);

	if(waitcursor!=NULL) {
	    cursor_SetStandard(waitcursor, Cursor_Wait);
	    im_SetProcessCursor(waitcursor);
	}
	
    }
}

static void WaitOff()
{
    waitcount--;
    if(waitcount<=0) {
	waitcount=0;
	if(waitcursor!=NULL) {
	    im_SetProcessCursor(NULL);
	    cursor_Destroy(waitcursor);
	    waitcursor=NULL;
	}
    }
}


static boolean DoAppend(self, src, dest)
struct nessmv *self;
struct ness *src,*dest;
{
    long end;
    ness_SetAccessLevel(dest, ness_codeUV);
    ness_SetNeedsDialogBox(dest, FALSE);
    ness_SetReadOnly(dest, FALSE);

    ness_SetAccessLevel(src, ness_codeUV);
    ness_SetNeedsDialogBox(src, FALSE);
    
    if(!src->compiled) {
	WaitOn();
	message_DisplayString(self, 0, "Testing compilability...");

	im_ForceUpdate();

	if(ness_Compile(src)) {
	    ness_Expose(src);
	    ness_printerrors(src,stderr);
	    message_DisplayString(self, 0, "");
	    message_DisplayString(self, 100, "Error recompiling!");
	    WaitOff();
	    return FALSE;
	}
	WaitOff();
    }

    end=ness_GetLength(dest);

    if(end>0 && ness_GetChar(dest, end-1)!='\n') ness_InsertCharacters(dest, end, "\n", 1);

    ness_CopyTextExactly(dest, end+1, src, 0, ness_GetLength(src));

    end=ness_GetLength(dest);

    if(end>0 && ness_GetChar(dest, end-1)!='\n') ness_InsertCharacters(dest, end, "\n", 1);
    
    WaitOn();
    message_DisplayString(self, 0, "Re-compiling.");

    im_ForceUpdate();

    if(ness_Compile(dest)) {
	ness_Expose(dest);
	ness_printerrors(dest,stderr);
	message_DisplayString(self, 0, "");
	message_DisplayString(self, 100, "Error recompiling! Some bindings may be unavailable.");
	WaitOff();
	return FALSE;
    }
    
    WaitOff();
    
    return TRUE;
}

static void UpdateFile(self, src, b, filename)
struct nessmv *self;
struct ness *src;
struct buffer *b;
char *filename;
{
    struct buffer *ob;
    WaitOn();
    message_DisplayString(self, 0, "Saving...");
    im_ForceUpdate();

    if(buffer_WriteToFile(b, filename, buffer_ReliableWrite|buffer_MakeBackup)<0) {
	message_DisplayString(self, 0, "");
	message_DisplayString(self, 100, "Write failed!");
	WaitOff();
	return;
    }
    buffer_SetIsModified(b, FALSE);
    WaitOff();

    message_DisplayString(self, 0, "Done re-compiling and saving.");

    ob=buffer_FindBufferByData(src);

    if(ob!=NULL) {
	struct frame *f=frame_GetFrameInWindowForBuffer(ob);
	if(f!=NULL) {
	    frame_SetBuffer(f, b, TRUE);
	    buffer_SetIsModified(b, FALSE);
	    buffer_Destroy(ob);
	}
    }
}

struct helpRock {
    procedure HelpWork;
    struct text *text;
    long rock;
    char *partial;
    struct ness *n;
};


static char *GetName(n)
struct ness *n;
{
    char *name=(char *)ness_GetName(n), *p;
    if(name) return name;
    name=ness_GetFilename(n);
    if(name==NULL) return NULL;
    p=rindex(name, '/');
    if(p) return p+1;
    else return name;
}
    
static long match(n,h)
struct ness *n;
struct helpRock *h;
{
    char buf[1024];
    int len;
    char *name=GetName(n);
    if(name==NULL) return 0;
    printf("blah:'%s'\n",name);
    if(!strncmp(name, h->partial, strlen(h->partial))) {
	bzero(buf,sizeof(buf));
	strncpy(buf,name,sizeof(buf)-1);
	(*h->HelpWork)(h->rock,message_HelpListItem,buf,NULL);
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
    struct ness *n=ness_GetList();
    if(!HelpWork) return;
    (*HelpWork)(rock,
		    message_HelpGenericItem,
		    "Ness Scripts\n",
		    NULL);
    
    myrock->HelpWork=HelpWork;
    myrock->rock=rock;
    myrock->partial=partial;
    while(n) {
	if(n!=myrock->n) match(n, myrock);
	n=n->next;
    }
}


static enum message_CompletionCode mycomplete(partial, myrock, buffer, bufferSize)
char *partial;
struct helpRock *myrock;
char *buffer;
int bufferSize;
{
    struct result result;
    char textBuffer[1024];
    struct ness *n=ness_GetList();
    *textBuffer = 0;
    result.partial = partial;
    result.partialLen = strlen(partial);
    result.bestLen = 0;
    result.code = message_Invalid;
    result.best = textBuffer;
    result.max = sizeof(textBuffer) - 1; /* Leave extra room for a NUL. */
    while(n) {
	if(n!=myrock->n && GetName(n)) completion_CompletionWork(GetName(n), &result);
	n=n->next;
    }
    strncpy(buffer, result.best, bufferSize);
    if (result.bestLen == bufferSize) /* Now make sure buffer ends in a NUL. */
	buffer[result.bestLen] = 0;
    return result.code;
}

static char sbuf[1024]=".atkmacros";

static long AskForScript(self, prompt, buf, bufsiz)
struct nessmv *self;
char *prompt;
char *buf;
long bufsiz;
{
    struct ness *n=ness_GetList();
    struct helpRock myrock;
    struct framemessage *fmsg=(struct framemessage *)nessmv_WantHandler(self,"message");
    struct buffer *b;
    if(!fmsg || !class_IsTypeByName(class_GetTypeName(fmsg),"framemessage")) return -1;

    b=frame_GetHelpBuffer(fmsg->frame);
    if(b!=NULL) myrock.text=(struct text *)buffer_GetData(b);
    else return -1;

    myrock.n=(struct ness *)nessmv_GetDataObject(self);

    /* does the default exist? */
    while(n) {
	char *sn=GetName(n);
	if(sn && strcmp(sn, sbuf)==0) break;
	n=n->next;
    }

    if(message_AskForStringCompleted(self, 0, prompt, n?sbuf:NULL, buf, bufsiz, NULL, mycomplete, helpProc, &myrock, message_MustMatch)) {
	message_DisplayString(self,0,"Cancelled.");
	return -1;
    }
    strcpy(sbuf, buf);
    return 0;
}

static void ScriptAppend(self, rock)
struct nessmv *self;
long rock;
{
    char buf[1024];
    char *name=NULL;
    struct ness *n=ness_GetList();
    
    struct buffer *b;
    
    if(rock>255) name=(char *)rock;
    
    if(name==NULL) {
	if(AskForScript(self, "Append to script:", buf, sizeof(buf))<0) return;
	name=buf;
    }
    
    while(n) {
	char *sn=GetName(n);
	if(sn && strcmp(sn, name)==0) break;
	n=n->next;
    }
    
    if(n==NULL) {
	message_DisplayString(self, 0, "Couldn't find the specified Ness script.");
	return;
    }
    
    if(!DoAppend(self, nessmv_GetDataObject(self), n)) return;
 
    if(ness_GetFilename(n)==NULL) {
	message_DisplayString(self, 0, "Ness has no associated file, changes have NOT been saved.");
	return;
    }
    
    b=buffer_FindBufferByData(n);

    /* If we create the buffer here it will NOT
      destroy it's data when it goes away. */
    if(b==NULL) b=buffer_Create(GetName(n), ness_GetFilename(n), NULL, n);

    if(b==NULL) {
	message_DisplayString(self, 0, "Couldn't get a buffer on the Ness, changes have NOT been saved.");
	return;
    }

    UpdateFile(self, nessmv_GetDataObject(self), b, ness_GetFilename(n));
}

static char dbuf[1024]="~/.atkmacros";
 
static char *appendchoices[]={
    "Cancel",
    "Update the existing script. (discard file)",
    "Update the file.",
    NULL
};


static void Append(self, rock)
struct nessmv *self;
long rock;
{
    char buf[1024];
    char *filename=NULL;
    struct buffer *b, *ob;
    struct basicobject *data;
    struct ness *dest, *src=(struct ness *)nessmv_GetDataObject(self);
    struct ness *n=ness_GetList();
    long result=2;
    
    if(rock>255) filename=(char *)rock;
    if(filename==NULL) {
	if(completion_GetFilename(self, "Append to file:", dbuf, buf, sizeof(buf), FALSE, FALSE)<0) {
	    message_DisplayString(self, 0, "Cancelled.");
	    return;
	}
	filename=buf;
    }

    strcpy(dbuf, filename);

    while(n) {
	if(ness_GetFilename(n) && strcmp(ness_GetFilename(n), dbuf)==0) break;
	n=n->next;
    }

    /* If the buffer already exists we leave it's data destroying flag intact. */
    b=buffer_FindBufferByFile(filename);

    if(n && !(b && buffer_GetData(b)==(struct dataobject *)n)) {
	if(message_MultipleChoiceQuestion(self, 100, "Warning a Ness Script with that filename is currently active.", 0, &result, appendchoices, NULL)<0 || result==0) {
	    message_DisplayString(self, 0, "Cancelled!");
	    return;
	}
    }

    switch(result) {
	case 1: /* visiting existing script. */
	    b=buffer_Create(GetName(n), ness_GetFilename(n), NULL, n);
	    break;
	case 2: /* visiting file. */

	    if(b==NULL) {
		b=buffer_GetBufferOnFile(filename, 0);
		/* If we created the buffer we don't want the data to go away when the buffer does. */
		if(b!=NULL) buffer_SetDestroyData(b, FALSE);
	    }

    }
    
    if(b==NULL) {
	message_DisplayString(self, 0, "Couldn't get a buffer on the specified file.");
	return;
    }
    
    if(buffer_GetReadOnly(b)) {
	message_DisplayString(self, 0, "File is read only. Append cancelled.");
	buffer_Destroy(b);
	return;
    }
    
    data=(struct basicobject *)buffer_GetData(b);
    if(strcmp(class_GetTypeName(data), "ness")!=0) {
	message_DisplayString(self, 0, "The named file is not a ness file.");
	buffer_Destroy(b);
	return;
    }
    
    dest=(struct ness *)data;

    if(!DoAppend(self, src, dest)) return;

    UpdateFile(self, src, b, filename);
}

static void Visit(self, rock)
struct nessmv *self;
long rock;
{
    struct ness *n=ness_GetList();
    char buf[1024];
    struct buffer *b;
    struct frame *f;
    char *name=NULL;

    if(rock>255) name=(char *)name;
    
    if(name==NULL) {
	if(AskForScript(self, "Visit script:", buf, sizeof(buf))<0) return;
	else name=buf;
    }
    
    while(n) {
	char *sn=GetName(n);
	if(sn && strcmp(sn, name)==0) break;
	n=n->next;
    }
    
    if(n==NULL) {
	message_DisplayString(self, 0, "Couldn't find the specified Ness script.");
	return;
    }

    b=buffer_FindBufferByData(n);
    
    if(b==NULL) b=buffer_Create(GetName(n), ness_GetFilename(n), NULL, n);

    if(b==NULL) {
	message_DisplayString(self, 0, "Couldn't get a buffer on the selected Ness script.");
	return;
    }

    f=frame_GetFrameInWindowForBuffer(b);

    if(f==NULL) {
	message_DisplayString(self, 0, "Couldn't get a window on the selected Ness script.");
	return;
    }
    if(f!=NULL) {
	struct view *v=frame_GetView(f);
	if(v!=NULL) view_WantInputFocus(v, v);
    }
}
    
    
static struct bind_Description Bindings[]={
    {"nessmv-visit-script",NULL,0,"Ness,Visit Script~50",0,0, Visit,"Visit an existing Ness script." },
    {"nessmv-append-to-file",NULL,0,"Ness,Append to File~40",0,0,Append,"Append the current buffer to a Ness file." },
    {"nessmv-append-to-script",NULL,0,"Ness,Append to Script~45",0,0,ScriptAppend,"Append the current buffer to a Ness script." },
    NULL
};

static struct menulist *menus=NULL;

boolean nessmv__InitializeClass(classID)
struct classheader *classID;
{
    menus = menulist_New();
    bind_BindList(Bindings, NULL , menus, &nessmv_classinfo);
    return TRUE;
}

boolean nessmv__InitializeObject(classID, self)
struct classheader *classID;
struct nessmv *self;
{ 
    self->ml=menulist_DuplicateML(menus, self);
    
    if(self->ml==NULL) return FALSE;

    return TRUE;
}

void nessmv__FinalizeObject(classID, self)
struct classheader *classID;
struct nessmv *self;
{
    if(self->ml) menulist_Destroy(self->ml);
    self->ml=NULL;
}

void nessmv__PostMenus(self, ml)
struct nessmv *self;
struct menulist *ml;
{
    menulist_ClearChain(self->ml);
    if(ml && ml!=self->ml) {
	menulist_ChainAfterML(self->ml, ml, NULL);
    }
    super_PostMenus(self, self->ml);
}

/*
 *    $Log: nessmv.c,v $
 * Revision 1.8  1993/07/23  00:20:51  rr2b
 * Split off a version of CopyText which will copy surrounding
 * styles as well as embedded styles.
 *
 * Revision 1.7  1993/07/21  19:34:22  rr2b
 * Fixed to use CopyTextExactly
 *
 * Revision 1.6  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.5.1.1  1993/02/02  03:04:46  rr2b
 * new R6tape branch
 *
 * Revision 1.5  1992/12/15  21:38:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.4  1992/12/14  20:50:00  rr2b
 * disclaimerization
 *
 * Revision 1.3  1992/11/26  02:42:25  wjh
 * converted CorrectGetChar to GetUnsignedChar
 * moved ExtendShortSign to interp.h
 * remove xgetchar.h; use simpletext_GetUnsignedChar
 * nessrun timing messages go to stderr
 * replaced curNess with curComp
 * replaced genPush/genPop with struct compilation
 * created compile.c to handle compilation
 * moved scope routines to compile.c
 * converted from lex to tlex
 * convert to use lexan_ParseNumber
 * truncated logs to 1992 only
 * use bison and gentlex instead of yacc and lexdef/lex
 *
 * .
 *
 * Revision 1.2  92/06/05  18:39:48  gk5g
 * Fixed some small oversights.
 * .
 * 
 * Revision 1.1  1992/06/05  17:28:27  rr2b
 * Initial revision
 *
 */

