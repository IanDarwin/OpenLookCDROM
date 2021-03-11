/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvaltvf.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <math.h>
#include <class.h>

#include "pvaltvf.eh"

#include <prefval.ih>

#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <text.ih>
#include <textv.ih>
#include <observe.ih>
#include <message.ih>
#include <complete.ih>
#include <filetype.ih>

#define DATA(self) ((struct prefval *)pvaltvf_GetDataObject(self))
#define TEXT(self) (pvaltvf_GetText(self))

static struct menulist *pvaltvfMenus=NULL;
static struct keymap *pvaltvfKeymap=NULL;


static struct helprock {
    struct pvaltvf *self;
    char *path;
    long ind;
} thelprock;

static void helpfunc(rock, type, item, null)
struct helprock *rock;
enum message_HelpItem type;
char *item;
long null;
{
    char buf[1024];
    struct pvaltvf *self=rock->self;
    if(type!=message_HelpListItem) return;

    if(rock->ind==0) prefval_ClearChoices(DATA(self));

    if(item[0]!='/') {
	char *p;
	strcpy(buf, rock->path);
	p=rindex(buf, '/');
	if(p==NULL) p=buf;
	else *p='\0';
	strcat(p, "/");
	strcat(p, item);
    } else strcpy(buf, item);
    
    prefval_SetChoice(DATA(self), rock->ind, item, prefval_StringToValue(DATA(self), buf));
    rock->ind++;
}

static void pvaltvfHelp(self, rock)
struct pvaltvf *self;
long rock;
{
    char buf[1024], buf2[1024];
    boolean good=TRUE;
    struct textview *tv=pvaltvf_GetTextView(self);
    long pos=textview_GetDotPosition(tv);
    long end;
    
    if(pos>0) {
	while(--pos>=0 && text_GetChar(TEXT(self), pos)!='\n');
	pos++;
    }
    
    end=text_Index(TEXT(self), pos, '\n', text_GetLength(TEXT(self))-pos);
    if(end<0) end=text_GetLength(TEXT(self));
    
    if(end-pos>=sizeof(buf)-1) return;
    
    text_CopySubString(TEXT(self), pos, end-pos, buf, FALSE);

    if(buf[0]=='\0') strcpy(buf, "./");
    filetype_CanonicalizeFilename(buf2, buf, sizeof(buf2)-1);
    thelprock.self=self;
    thelprock.ind=0;
    thelprock.path=buf2;
    completion_FileHelp(buf2, NULL, helpfunc, &thelprock);
    prefval_SortChoices(DATA(self));
    prefval_NotifyObservers(DATA(self), prefval_ChoicesChanged);
}


static void pvaltvfNop(self, rock)
struct pvaltvf *self;
long rock;
{
}

static void pvaltvfComplete(self, rock)
struct pvaltvf *self;
long rock;
{
    char buf[1024], buf2[1024];
    boolean good=TRUE;
    struct textview *tv=pvaltvf_GetTextView(self);
    long pos=textview_GetDotPosition(tv);
    long end;
    if(pos>0) {
	while(--pos>=0 && text_GetChar(TEXT(self), pos)!='\n');
	pos++;
    }
    end=text_Index(TEXT(self), pos, '\n', text_GetLength(TEXT(self))-pos);
    if(end<0) end=text_GetLength(TEXT(self));
    
    if(end-pos>=sizeof(buf)-1) return;
    
    text_CopySubString(TEXT(self), pos, end-pos, buf, FALSE);

    switch(completion_FileComplete(buf, FALSE, buf2, sizeof(buf2))) {
	case message_Complete:
	    message_DisplayString(self, 0, "Unique.");
	    break;
	case message_CompleteValid:
	    message_DisplayString(self, 0, "Others.");
	    break;
	case message_Valid:
	    break;
	case message_Invalid:
	    good=FALSE;
	    message_DisplayString(self, 0, "No match.");
	    break;
    }

    if(good) {
	int len=strlen(buf2);
	text_ReplaceCharacters(TEXT(self), pos, end-pos, buf2, len);
	text_NotifyObservers(TEXT(self), observable_OBJECTCHANGED);
	textview_SetDotPosition(tv, pos+len);
	textview_SetDotLength(tv, 0);
	textview_FrameDot(tv, pos+len);
    }
    
}

static struct bind_Description pvaltvfBindings[]={
    {
	"pvaltvf-complete",
	" ",
	' ',
	NULL,
	0,
	0,
	pvaltvfComplete,
	"Attempts to complete the filename before the cursor.",
	"pvaltvf"
    },/* {
	"pvaltvf-nop",
	"\r",
	'\r',
	NULL,
	0,
	0,
	pvaltvfNop,
	"Does nothing.",
	"pvaltvf"
    }, */
    {
	"pvaltvf-help",
	"?",
	'?',
	NULL,
	0,
	0,
	pvaltvfHelp,
	"Provides a list of possible completions in the choices list.",
	"pvaltvf"
    },
    {
	NULL,
	NULL,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL
    }
};
    
    
boolean pvaltvf__InitializeClass(classID)
struct classheader *classID;
{
    pvaltvfMenus=menulist_New();
    pvaltvfKeymap=keymap_New();

    if(pvaltvfMenus == NULL || pvaltvfKeymap==NULL) {
	if(pvaltvfMenus != NULL) menulist_Destroy(pvaltvfMenus);
	if(pvaltvfKeymap != NULL) keymap_Destroy(pvaltvfKeymap);
	return FALSE;
    }
    
    bind_BindList(pvaltvfBindings, pvaltvfKeymap, pvaltvfMenus, &pvaltvf_classinfo);
    return TRUE;
}

boolean pvaltvf__InitializeObject(classID, self)
struct classheader *classID;
struct pvaltvf *self;
{
    self->ks=keystate_Create(self, pvaltvfKeymap);
    if(self->ks==NULL) return FALSE;

    self->menulist=menulist_DuplicateML(pvaltvfMenus, self);

    if(self->menulist==NULL) {
	keystate_Destroy(self->ks);
	return FALSE;
    }
    
    return TRUE;
}

void pvaltvf__FinalizeObject(classID, self)
struct classheader *classID;
struct pvaltvf *self;
{
    if(self->ks) keystate_Destroy(self->ks);
    if(self->menulist) menulist_Destroy(self->menulist);
}


struct keystate *pvaltvf__Keys(self)
struct pvaltvf *self;
{
    return self->ks;
}
