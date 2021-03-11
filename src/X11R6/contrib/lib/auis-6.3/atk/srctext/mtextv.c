/* File mtextv.c created by R L Quinn
  
   MtextView, a Modula-2 mode for ATK. */
/* Copyright 1988, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsHeader[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/mtextv.c,v 1.9 1994/02/28 21:02:02 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>

#include <im.ih>
#include <message.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
   /*RSKadd*/
#include <environ.ih> /* for autocut preference */
#include <proctbl.ih> /* for autocut preference */

#include "mtext.ih"
#include "mtextv.eh"

/* AutoCut was not made externally visible by txtvcmod, so WE have to check the preference TOO */
static int autocut_mode = -1;	/* uninitialized */
#define IsAutoCutMode() ((autocut_mode == -1 && (autocut_mode = environ_GetProfileSwitch("autocut", FALSE))) || autocut_mode)

static struct keymap *m_Map;
static struct menulist *m_Menus;

void asterisk(),
     definition(),
     implementation();

static struct bind_Description mtextBindings[]={
    {"mtextview-asterisk","*",'*', NULL,0, 0, asterisk,"If preceded by an open-paren, start a comment."},
    {"mtextview-display-definition",NULL,0, "Source Text,Display Definition~30", 0,0, definition, "Find the definition module where selected identifier is declared."},
    {"mtextview-display-implementation",NULL,0, "Source Text,Display Implementation~31", 0,0, implementation, "Find the implementation module where selected procedure's code lies."},
    NULL
};

boolean mtextview__InitializeClass(classID)
struct classheader *classID;
{
    m_Menus = menulist_New();
    m_Map = keymap_New();
    bind_BindList(mtextBindings,m_Map,m_Menus,class_Load("srctextview")); /*RSK92fix*/
    return TRUE;
}

boolean mtextview__InitializeObject(classID, self)
struct classheader *classID;
struct mtextview *self;
{
    self->m_state = keystate_Create(self, m_Map);
    self->m_menus = menulist_DuplicateML(m_Menus, self);
    mtextview_SetBorder(self,5,5); 
    return TRUE;
}

void mtextview__FinalizeObject(classID, self)
struct classheader *classID;
struct mtextview *self;
{
    keystate_Destroy(self->m_state);
    menulist_Destroy(self->m_menus);
}

void mtextview__PostMenus(self, menulist)
struct mtextview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->m_menus, menulist, self);
    super_PostMenus(self, self->m_menus);
}

struct keystate *mtextview__PrependKeyState(self)
struct mtextview *self;
{
    self->m_state->next= NULL;
    return keystate_AddBefore(self->m_state, super_PrependKeyState(self));
}

/* FindDefinitionOrImplementation() isolates the identifier pointed to by the caret and tries to find its corresponding definition module (if implementation is FALSE) or implementation module (if implementation is TRUE) */
static void FindDefinitionOrImplementation(self, implementation)    /*RSK90mod*/
    struct mtextview *self;
    boolean implementation;
    {
    struct mtext *ct = (struct mtext *)self->header.view.dataobject;
    long pos,oldpos;
    char name[256],proc[256],msg[300];
    char filename[256],bufname[256],searchpath[1024];    /*RSK90add*/

    /*extract module name from document*/
    mtextview_CollapseDot(self);
    pos=oldpos=mtextview_GetDotPosition(self);
    while (pos>0 && mtext_IsTokenOrPeriod(ct,mtext_GetChar(ct,pos-1))) pos--;
    oldpos=pos;
    while (mtext_IsTokenChar(ct,name[pos-oldpos]=mtext_GetChar(ct,pos)) && pos-oldpos<256) pos++;
    name[pos-oldpos]='\0';
    /*extract procedure name if present*/
    if (mtext_GetChar(ct,pos)=='.')
	{
	oldpos=(++pos);
	while (mtext_IsTokenChar(ct,proc[pos-oldpos]=mtext_GetChar(ct,pos)) && pos-oldpos<256) pos++;
	proc[pos-oldpos]='\0';
	}
    else
	proc[0]='\0';
    if (strlen(name)>0)
	{
	/*find filename*/    /*RSK90mod*/
	strcpy(filename,name);
	if (implementation)
	    strcat(filename,".mod\0");
	else
	    strcat(filename,".def\0");
	/*find buffer name*/    /*RSK90mod*/
	strcpy(bufname,name);
	if (implementation)
	    strcat(bufname,"-implementation\0");
	else
	    strcat(bufname,"-definition\0");
	/*find search path (environment variables MPath,m2mi, and m2mp, and DEFAULT_SYM_DIR)*/
	strcpy(searchpath,getenv("MPath")); strcat(searchpath,":\0");
	strcat(searchpath,getenv("m2mi")); strcat(searchpath,":\0");
	strcat(searchpath,getenv("m2mp"));
#ifdef DEFAULT_SYM_DIR
	strcat(searchpath,":\0");
	strcat(searchpath,DEFAULT_SYM_DIR);
#else /*DEFAULT_SYM_DIR*/
	message_DisplayString(self, 0, "Your Makefile should define a DEFAULT_SYM_DIR.");
#endif /*DEFAULT_SYM_DIR*/
	if (!mtextview_FindSubFile(self,filename, bufname,proc, searchpath))    /*RSK90mod*/
	    {
	    msg[0]='\0';
	    sprintf(msg, "Couldn't find %s.", filename);
	    message_DisplayString(self, 0, msg);
	    }
	}
    else
	{
	if (implementation)
	    message_DisplayString(self, 0, "Cursor must be on an implementation name.");
	else
	    message_DisplayString(self, 0, "Cursor must be on a definition name.");
	}
    mtext_NotifyObservers(ct, 0);
    }

static void definition(self, key)    /*RSKadd*/
    struct mtextview *self;
    long key;
    {
    FindDefinitionOrImplementation(self,FALSE);
    }

static void implementation(self, key)    /*RSKadd*/
    struct mtextview *self;
    long key;
    {
    FindDefinitionOrImplementation(self,TRUE);
    }

/* any modifications to asterisk should be duplicated in m3textv.c */
static void asterisk(self, key)
struct mtextview *self;
char key; /* must be char for "&" to work. */
{
    struct mtext *ct=(struct mtext *)self->header.view.dataobject;
    int count=im_Argument(mtextview_GetIM(self));
    long pos,oldpos;

    if (mtextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && mtextview_GetDotLength(self)>0)
	im_HandleMenu(mtextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    oldpos= pos= mtextview_CollapseDot(self);
    while (count--) mtext_InsertCharacters(ct,pos++,&key,1);
    if (oldpos && mtext_GetChar(ct,oldpos-1)=='(' && !mtext_GetStyle(ct, oldpos-1) && !mtext_InString(ct,oldpos))
	mtext_WrapStyleNow(ct, oldpos-1,pos-oldpos+1, ct->header.srctext.comment_style, FALSE,TRUE);
    mtextview_SetDotPosition(self, pos);
    mtext_NotifyObservers(ct, 0);
}
