/* Copyright 1992 by the Andrew Toolkit Consortium, All rights Reserved */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nessm.c,v 1.7 1992/12/15 21:38:20 rr2b R6tape $";
#endif


#include <andrewos.h>
#include <class.h>
#include <ctype.h>

#include <proctbl.ih>
#include <message.ih>
#include <ness.ih>
#include <view.ih>
#include <im.ih>
#include <buffer.ih>
#include <frame.ih>
#include <environ.ih>
#include <mark.ih>
#include <stylesht.ih>
#include <style.ih>
#include <nessm.eh>

static char keytmpl[]="DoKeys(currentinputfocus, \"%\")\n";
static char menutmpl[]="(currentinputfocus, %)\n";
static char answertmpl[]="QueueAnswer(\"%\")\n";
static char canceltmpl[]="QueueCancellation()\n";
static char menu[]="menu";
static char keys[]="keys";

static char *mousehits(act)
enum view_MouseAction act;
{
    switch(act) {
	case view_LeftDown:
	    return "mouseleftdown";
	case view_LeftUp:
	    return "mouseleftup";
	case view_LeftMovement:
	    return "mouseleftmove";
	case view_RightDown:
	    return "mouserightdown";
	case view_RightUp:
	    return "mouserightup";
	case view_RightMovement:
	    return "mouserightmove";
	default:
	    return "ERROR";
    }
};

static long lastpos=(-1);
static enum im_EventType lasttype=im_NoEvent;
static long InsertKey(n, m, pos, key)
struct ness *n;
struct mark *m;
long pos;
char key;
{
    if(lasttype!=im_KeyboardEvent) {
	lastpos=pos=mark_GetPos(m)-1;
	ness_AlwaysInsertCharacters(n, pos, keytmpl, strlen(keytmpl));
	pos=ness_Index(n, pos, '%', ness_GetLength(n)-pos);
	ness_AlwaysDeleteCharacters(n, pos, 1);
    }
    if(key=='\0') key=(char)0x80;
    ness_AlwaysInsertCharacters(n,  pos, &key, 1);
    return pos+1;
}

static int InsertProcCall(n, pos, proc, rock)
struct ness *n;
long pos;
struct proctable_Entry *proc;
long rock;
{
    char buf[32];
    char procbuf[256], *p;
    char *procname=proctable_GetName(proc);
    
    if(strlen(procname)>sizeof(procbuf)-1) {
	return -1;
    }
    strcpy(procbuf, procname);
		
    p=procbuf-1;
    while(p=index(p+1, '-')) *p='_';
    lastpos=pos;
    ness_AlwaysInsertCharacters(n, pos, menutmpl, strlen(menutmpl));
    ness_AlwaysInsertCharacters(n, pos, procbuf, strlen(procbuf));
    pos+=strlen(procbuf);
    pos=ness_Index(n, pos, '%', ness_GetLength(n)-pos);
    if(rock>255) {
	ness_AlwaysReplaceCharacters(n, pos, 1, rock, strlen(rock));
	ness_AlwaysInsertCharacters(n, pos+strlen(rock), "\"", 1);
	ness_AlwaysInsertCharacters(n, pos, "\"", 1);
    } else {
	sprintf(buf, "%d", rock);
	ness_AlwaysReplaceCharacters(n, pos, 1, buf, strlen(buf));
    }
    return 0;
}


static void DumpActions(a)
struct action *a;
{
    while(a) {
	printf("a:%x\n",a);
	switch(a->type) {
	    case im_KeyboardEvent:
		printf("key:'%c'\n",a->v.key);
		break;
	    case im_ProcEvent:
		printf("proc %s\n",proctable_GetName(a->v.proc.procTableEntry));
		printf("rock %d '%c'\n",a->v.proc.rock, a->v.proc.rock);
		break;
	    case im_MenuEvent:
		printf("menu proc %s\n",proctable_GetName(a->v.proc.procTableEntry));
		printf("rock %d '%c'\n",a->v.proc.rock, a->v.proc.rock);
		break;
	    case im_MouseEvent:
		printf("mouse event!!\n");
		break;
	    case im_AnswerEvent:
		printf("answer:%x\n",a->v.answer);
		printf("answer:'%s'\n",a->v.answer);
		break;
	    case im_SuspendEvent:
		printf("suspend\n");
		break;
	    case im_ResumeEvent:
		printf("resume\n");
		break;
	    default:
		printf("unhandled type %d\n",a->type);
	}
	a=a->next;
    }
}

#define CNTRL(c) ((c)-'@')

static void RegionToPrintable(n, pos, len)
struct ness *n;
long pos, len;
{
    struct mark *m=ness_CreateMark(n, pos, len);
    long cpos=pos;
    int c;
    while(cpos<mark_GetPos(m)+mark_GetLength(m)) {
	switch(c=ness_GetChar(n, cpos)) {
	    case '"':
	    case '^':
	    case '\\':
		ness_AlwaysInsertCharacters(n, cpos, "\\", 1);
		cpos+=2;
		break;
	    case '\033':
		ness_AlwaysReplaceCharacters(n, cpos, 1, "\\e", 2);
		cpos+=2;
		break;
	    case '\n':
		ness_AlwaysReplaceCharacters(n, cpos, 1, "\\n", 2);
		cpos+=2;
		break;
	    case '\r':
		ness_AlwaysReplaceCharacters(n, cpos, 1, "\\r", 2);
		cpos+=2;
		break;
	    case '\t':
		ness_AlwaysReplaceCharacters(n, cpos, 1, "\\t", 2);
		cpos+=2;
		break;
	    default: 
		if(c>=CNTRL('@') && c<=CNTRL('Z')) {
		    char buf[3];
		    buf[0]='^';
		    buf[1]=c+'@';
		    buf[2]='\0';
		    ness_AlwaysReplaceCharacters(n, cpos, 1, buf, 2);
		    cpos+=2;
		} else {
		    cpos+=1;
		}
	}
    }
    ness_RemoveMark(n, m);
    mark_Destroy(m);
}


static struct action *QueueAnswers(n, look, m)
struct ness *n;
struct action *look;
struct mark *m;
{
    struct action *a=look->next;
    int susplevel=1;
    while(a && a->type!=im_SuspendEvent) a=a->next;
    if(a) a=a->next;
    while(a && susplevel>0) {
	switch(a->type) {
	    case im_SuspendEvent:
		susplevel++;
		break;
	    case im_ResumeEvent:
		if(susplevel>0) susplevel--;
		break;
	    case im_AnswerEvent:
		if(a->v.answer) {
		    long lastpos=mark_GetPos(m)-1;
		    long pos=0;
		    ness_AlwaysInsertCharacters(n, lastpos, answertmpl, strlen(answertmpl));
		    pos=ness_Index(n, lastpos, '%', ness_GetLength(n)-lastpos);
		    ness_AlwaysReplaceCharacters(n, pos, 1, a->v.answer, strlen(a->v.answer));
		    RegionToPrintable(n, pos, strlen(a->v.answer));
		} else {
		    ness_AlwaysInsertCharacters(n, mark_GetPos(m)-1, canceltmpl, strlen(canceltmpl));
		}
		break;
	    default: ;
	}
	a=a->next;
    }
    return a;
}
 
static char *choices[]={
    "Cancel - Don't make any binding.",
    "Install as a menu option.",
    "Install as a key binding.",
    "Edit the script.",
    NULL
};


static void DoConv(self, rock)
struct view *self;
long rock;
{
    struct action *look=im_GetMacro();
    struct ness *n;
    char ch;
    char nbuf[256], finalname[512], reallyfinalname[512];
    char *p;
    long pos;
    char *tname;
    long tlen;
    struct buffer *buffer;
    struct mark *m, *mke;
    long mpos, mkpos;
    long result;
    struct frame *f;
    struct im *im=view_GetIM(self);
    
    if(look==NULL) {
	message_DisplayString(self, 0, "No keyboard macro defined!");
	return;
    }

    n=ness_New();

    if(n==NULL) {
	message_DisplayString(self, 100, "Couldn't create a ness script!");
	return;
    }

    ness_SetAccessLevel(n, ness_codeUV);
#ifdef ROB
    result=ness_ReadNamedFile(n, "/afs/andrew/usr20/rr2b/work/ness/objects/mtmpl.n");
#else
    result=ness_ReadNamedFile(n, environ_AndrewDir("/lib/ness/mtmpl.n"));
#endif
    if(result<0) {
	message_DisplayString(self, 100, "Couldn't read macro template!");
	ness_Destroy(n);
	return;
    }
    
    ness_SetFilename(n, NULL);
    ness_SetNeedsDialogBox(n, FALSE);
    ness_SetReadOnly(n, FALSE);

    
    pos=ness_Index(n, 0, '%', ness_GetLength(n));
    if(pos<0) {
	message_DisplayString(self, 100, "Internal ERROR: couldn't find type name insertion point.");
	ness_Destroy(n);
	return;
    }

    if(im && im_GetInputFocus(im)) self=im_GetInputFocus(im);

    tname=class_GetTypeName(self);
    tlen=strlen(tname);

    /* replace the argument to extend with the right view type */
    ness_AlwaysReplaceCharacters(n, pos, 1, tname, tlen);

    /* find the place to insert the type of binding being done */
    pos+=tlen;
    mkpos=ness_Index(n, pos, '%', ness_GetLength(n)-pos);

    /* find the place to put the binding to be made */
    pos=mkpos+1;
    mpos=ness_Index(n, pos, '%', ness_GetLength(n)-pos);

    /* find where to put the code for the binding. */
    pos=mpos+1;
    pos=ness_Index(n, pos, '%', ness_GetLength(n)-pos)+1;
    
    m=ness_CreateMark(n, pos, 1);
    if(m==NULL) {
	message_DisplayString(self, 0, "Internal ERROR: couldn't allocate mark!");
	ness_Destroy(n);
	return;
    }
    
    mark_SetStyle(m, FALSE, FALSE);

    /* find where to put the end of the type of binding being made */
    pos=ness_Index(n, pos, '%', ness_GetLength(n)-pos);

    ness_AlwaysDeleteCharacters(n, pos, 1);
    mke=ness_CreateMark(n, pos, 0);
    if(mke==NULL) {
	message_DisplayString(self, 0, "Internal ERROR: couldn't allocate mark!");
	ness_RemoveMark(n, m);
	mark_Destroy(m);
	ness_Destroy(n);
	return;
    }
    
    mark_SetStyle(mke, FALSE, FALSE);
    
    lasttype=im_NoEvent;
    while(look) {
	struct action *nlook=NULL;
	switch(look->type) {
	    case im_KeyboardEvent:
		nlook=QueueAnswers(n, look, m);
		pos=InsertKey(n, m, pos, look->v.key);
		look=nlook;
		break;
	    case im_MenuEvent:
		lasttype=look->type;
		nlook=QueueAnswers(n, look, m);
		pos=mark_GetPos(m)-1;		
		if(InsertProcCall(n, pos, look->v.proc.procTableEntry, look->v.proc.rock)<0) {

		    message_DisplayString(self, 100, "ERROR: proctable entry name too long\n");
		    ness_Destroy(n);
		    return;
		}
		look=nlook;
		break;
	    case im_ProcEvent:
		if(look->v.proc.keys->next || !isprint(look->v.proc.keys->v.key)) {

		    nlook=QueueAnswers(n, look, m);
		    pos=mark_GetPos(m)-1;
		    lasttype=look->type;
		    if(InsertProcCall(n, pos, look->v.proc.procTableEntry, look->v.proc.rock)<0) {

			message_DisplayString(self, 100, "ERROR: proctable entry name too long\n");
			ness_Destroy(n);
			return;
		    }
		} else {
		    nlook=QueueAnswers(n, look, m);
		    pos=InsertKey(n, m, pos, look->v.proc.keys->v.key);
		    lasttype=im_KeyboardEvent;
		}
		look=nlook;
		break;
	    case im_MouseEvent:
		nlook=QueueAnswers(n, look, m);
		lasttype=look->type;
		sprintf(nbuf, "DoHit(currentwindow, %s, %d, %d)\n",mousehits(look->v.mouse.action), look->v.mouse.x, look->v.mouse.y);
		lastpos=mark_GetPos(m)-1;
		ness_AlwaysInsertCharacters(n, lastpos, nbuf, strlen(nbuf));
		look=nlook;
		break;
	    default: look=look->next;
	}
    }

    ness_DeleteCharacters(n, mark_GetPos(m)-1, 2);
    ness_RemoveMark(n, m);
    mark_Destroy(m);

    if(message_MultipleChoiceQuestion(self, 100, "Macro Script Generated", 0, &result, choices, NULL)<0 || result==0) {
	message_DisplayString(self, 0, "Cancelled!");
	ness_Destroy(n);
	return;
    }

    buffer_GetUniqueBufferName("NessMacro", nbuf, sizeof(nbuf));

    buffer=buffer_Create(nbuf, NULL, NULL, n);


    if(buffer==NULL) {
	message_DisplayString(self, 100, "ERROR: couldn't get buffer for script.");
	ness_RemoveMark(n, mke);
	mark_Destroy(mke);
	buffer_Destroy(buffer);
	return;
    }

    buffer_SetDestroyData(buffer, TRUE);
    
    switch(result) {
	case 1:

	    if(message_AskForString(self, 0, "Menu Name (eg. Card,Item):", NULL, nbuf, sizeof(nbuf))<0) {
		message_DisplayString(self, 0, "Cancelled!");
		ness_RemoveMark(n, mke);
		mark_Destroy(mke);
		buffer_Destroy(buffer);
		return;
	    }

	    ness_AlwaysReplaceCharacters(n, mpos, 1, nbuf, strlen(nbuf));
	   
	    ness_AlwaysReplaceCharacters(n, mkpos, 1, menu, strlen(menu));

	    ness_AlwaysInsertCharacters(n, mark_GetPos(mke), menu, strlen(menu));
	    ness_RemoveMark(n, mke);
	    mark_Destroy(mke);

	    strcpy(finalname, "Menu:");
	    strcat(finalname, nbuf);
	    
	    buffer_GetUniqueBufferName(finalname, reallyfinalname, sizeof(reallyfinalname));
	    buffer_SetName(buffer, reallyfinalname);

	    if(ness_Compile(n)) ness_printerrors(n, stderr);
	    return;
	case 2:

	    if(message_AskForString(self, 0, "Key Sequence (eg. ^X^L):", NULL, nbuf, sizeof(nbuf))<0) {
		message_DisplayString(self, 0, "Cancelled!");
		ness_RemoveMark(n, mke);
		mark_Destroy(mke);
		buffer_Destroy(buffer);
		return;
	    }
	    ness_AlwaysReplaceCharacters(n, mpos, 1, nbuf, strlen(nbuf));
	    
	    ness_AlwaysReplaceCharacters(n, mkpos, 1, keys, strlen(keys));
	    ness_AlwaysInsertCharacters(n, mark_GetPos(mke), keys, strlen(keys));
	    ness_RemoveMark(n, mke);
	    mark_Destroy(mke);
	    
	    strcpy(finalname, "Keybinding:");
	    strcat(finalname, nbuf);
	    
	    buffer_GetUniqueBufferName(finalname, reallyfinalname, sizeof(reallyfinalname));
	    buffer_SetName(buffer, reallyfinalname);
	    
	    if(ness_Compile(n)) ness_printerrors(n, stderr);
	    return;
	case 3: {
	    ness_AlwaysInsertCharacters(n, mark_GetPos(mke), "%", 1);
	    f=frame_GetFrameInWindowForBuffer(buffer);
	    if(f!=NULL) {

		struct view *v=frame_GetView(f);
		if(v!=NULL) view_WantInputFocus(v, v);
	    }
	    }
    }
    ness_RemoveMark(n, mke);
    mark_Destroy(mke);
}
    
boolean nessm__InitializeClass(classID)
struct classheader *classID;
{
    proctable_DefineProc("nessm-make-macro", DoConv, class_Load("view"), "nessm", "Converts a keyboard macro to ness code.");
    return TRUE;
}

