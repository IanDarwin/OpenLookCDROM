/* File m3textv.c created by R L Quinn
   
   M3textView, a Modula-3 mode for ATK. */
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
static char rcsHeader[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/m3textv.c,v 1.10 1994/02/28 21:02:02 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <im.ih>
#include <message.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <envrment.ih>
   /*RSKadd*/
#include <environ.ih> /* for autocut preference */
#include <proctbl.ih> /* for autocut preference */

#include "m3text.ih"
#include "m3textv.eh"

/* AutoCut was not made externally visible by txtvcmod, so WE have to check the preference TOO */
static int autocut_mode = -1;	/* uninitialized */
#define IsAutoCutMode() ((autocut_mode == -1 && (autocut_mode = environ_GetProfileSwitch("autocut", FALSE))) || autocut_mode)

static struct keymap *m3_Map;
static struct menulist *m3_Menus;

void asterisk(),
     interface(),
     module(),
     m3pragma();

static struct bind_Description m3textBindings[]={
    {"m3textview-asterisk","*",'*', NULL,0, 0, asterisk,""},
    {"m3textview-pragma",">",'>', NULL,0, 0, m3pragma, ""},
    {"m3textview-display-interface",NULL,0, "Source Text,Display Interface~30", 0,0, interface, "Find the interface where selected identifier is declared."},
    {"m3textview-display-module",NULL,0, "Source Text,Display Module~31", 0,0, module, "Find the module where selected procedure's code lies."},
    NULL
};

boolean m3textview__InitializeClass(classID)
struct classheader *classID;
{
    m3_Menus = menulist_New();
    m3_Map = keymap_New();
    bind_BindList(m3textBindings,m3_Map,m3_Menus,class_Load("srctextview")); /*RSK92fix*/
    return TRUE;
}

boolean m3textview__InitializeObject(classID, self)
struct classheader *classID;
struct m3textview *self;
{
    self->m3_state = keystate_Create(self, m3_Map);
    self->m3_menus = menulist_DuplicateML(m3_Menus, self);
    m3textview_SetBorder(self,5,5); 
    return TRUE;
}

void m3textview__FinalizeObject(classID, self)
struct classheader *classID;
struct m3textview *self;
{
    keystate_Destroy(self->m3_state);
    menulist_Destroy(self->m3_menus);
}

void m3textview__PostMenus(self, menulist)
struct m3textview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->m3_menus, menulist, self);
    super_PostMenus(self, self->m3_menus);
}

struct keystate *m3textview__PrependKeyState(self)
struct m3textview *self;
{
    self->m3_state->next= NULL;
    return keystate_AddBefore(self->m3_state, super_PrependKeyState(self));
}

/* FindInterfaceOrModule() isolates the identifier pointed to by the caret and tries to find its corresponding interface (if module is FALSE) or module (if module is TRUE) */
static void FindInterfaceOrModule(self, module)    /*RSK90mod*/
    struct m3textview *self;
    boolean module;
    {
    struct m3text *ct = (struct m3text *)self->header.view.dataobject;
    long pos,oldpos;
    char name[256],proc[256],msg[300];
    char filename[256],bufname[256],searchpath[1024];    /*RSK90add*/

    /*extract interface/module name from document*/
    m3textview_CollapseDot(self);
    pos=oldpos=m3textview_GetDotPosition(self);
    while (pos>0 && m3text_IsTokenOrPeriod(ct,m3text_GetChar(ct,pos-1))) pos--;
    oldpos=pos;
    while (m3text_IsTokenChar(ct,name[pos-oldpos]=m3text_GetChar(ct,pos)) && pos-oldpos<256) pos++;
    name[pos-oldpos]='\0';
    /*extract procedure name if present*/
    if (m3text_GetChar(ct,pos)=='.')
	{
	oldpos=(++pos);
	while (m3text_IsTokenChar(ct,proc[pos-oldpos]=m3text_GetChar(ct,pos)) && pos-oldpos<256) pos++;
	proc[pos-oldpos]='\0';
	}
    else
	proc[0]='\0';
    if (strlen(name)>0)
        {
        /*find filename*/    /*RSK90mod*/
        strcpy(filename,name);
	if (module)
	    strcat(filename,"Impl.m3\0");
	else
	    strcat(filename,".i3\0");
	/*find buffer name*/    /*RSK90mod*/
        strcpy(bufname,name);
        if (module)
            strcat(bufname,"-module\0");
        else
	    strcat(bufname,"-interface\0");
	/*find search path (environment variable M3Path & DEFAULT_SY_DIR)*/    /*RSK90mod*/
	strcpy(searchpath,getenv("M3Path"));
#ifdef DEFAULT_SY_DIR
	strcat(searchpath,":\0");
        strcat(searchpath,DEFAULT_SY_DIR);
#else /*DEFAULT_SY_DIR*/
        message_DisplayString(self, 0, "Your Makefile should define a DEFAULT_SY_DIR.");
#endif /*DEFAULT_SY_DIR*/
	if (!m3textview_FindSubFile(self,filename, bufname,proc, searchpath))    /*RSK90mod*/
	    {
	    msg[0]='\0';
	    sprintf(msg, "Couldn't find %s.", filename);
	    message_DisplayString(self, 0, msg);
	    }
	}
    else
	{
	if (module)
	    message_DisplayString(self, 0, "Cursor must be on a module name.");
	else
	    message_DisplayString(self, 0, "Cursor must be on an import name.");
        }
    m3text_NotifyObservers(ct, 0);
    }

static void interface(self, key)    /*RSKadd*/
    struct m3textview *self;
    long key;
    {
    FindInterfaceOrModule(self,FALSE);
    }

static void module(self, key)    /*RSKadd*/
    struct m3textview *self;
    long key;
    {
    FindInterfaceOrModule(self,TRUE);
    }

/* identical to mtext's asterisk() with the exception of pragma-checking */
static void asterisk(self, key)
struct m3textview *self;
char key; /* must be char for "&" to work. */
{
    struct m3text *ct=(struct m3text *)self->header.view.dataobject;
    int count=im_Argument(m3textview_GetIM(self));
    long pos,oldpos;

    if (m3textview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && m3textview_GetDotLength(self)>0)
	im_HandleMenu(m3textview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    oldpos= pos= m3textview_CollapseDot(self);
    while (count--) m3text_InsertCharacters(ct,pos++,&key,1);
    if (oldpos && !m3text_GetStyle(ct, oldpos-1) && !m3text_InString(ct,oldpos)) {
	if(m3text_GetChar(ct,oldpos-1)=='(')
	    m3text_WrapStyleNow(ct, oldpos-1,pos-oldpos+1, ct->header.srctext.comment_style, FALSE,TRUE);
	else if(m3text_GetChar(ct,oldpos-1)=='<')
	    m3text_WrapStyleNow(ct, oldpos-1,pos-oldpos+1, ct->header.srctext.kindStyle[PRAGMA], FALSE,TRUE);
    }
    m3textview_SetDotPosition(self, pos);
    m3text_NotifyObservers(ct, 0);
}

/* m3pragma should be functionally equivalent to paren function in modtextv.c */
static void m3pragma(self, key) /*RSK91mod*/
struct m3textview *self;
char key; /* must be char for "&" to work. */
{
    struct m3text *ct=(struct m3text *)self->header.view.dataobject;
    long openpragma,oldpos=m3textview_GetDotPosition(self);
    if (m3textview_ConfirmReadOnly(self))
	return;
    m3textview_SelfInsert(self,key);
    openpragma= m3text_ReverseBalance(ct,m3textview_GetDotPosition(self));
    if (oldpos && (m3text_GetChar(ct,oldpos-1)=='*') && (m3text_GetStyle(ct,oldpos-1)==ct->header.srctext.kindStyle[PRAGMA]))
	environment_SetStyle(environment_GetEnclosing( ct->header.text.rootEnvironment, oldpos),FALSE,FALSE);/*RSK91add*/
    m3text_NotifyObservers(ct, 0);
}
