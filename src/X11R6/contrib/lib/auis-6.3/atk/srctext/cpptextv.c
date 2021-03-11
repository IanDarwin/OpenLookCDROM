/* File cpptextv.c created by R L Quinn
  
   CpptextView, a C++ mode for ATK.*/
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/cpptextv.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <envrment.ih> /* for comment insertion */
#include <environ.ih> /* for autocut preference */
#include <proctbl.ih> /* for autocut preference */

#include "cpptext.ih"
#include "cpptextv.eh"

/* AutoCut was not made externally visible by txtvcmod, so WE have to check the preference TOO */
static int autocut_mode = -1;	/* uninitialized */
#define IsAutoCutMode() ((autocut_mode == -1 && (autocut_mode = environ_GetProfileSwitch("autocut", FALSE))) || autocut_mode)

static struct keymap *cpp_Map;
static struct menulist *cpp_Menus;

void slash();

static struct bind_Description cpptextBindings[]={
    {"cpptextview-slash","/",'/',NULL,0,0,slash,"Insert a slash, possibly an end comment delimiter or the start of a line comment."},
    NULL
};

boolean cpptextview__InitializeClass(classID)
struct classheader *classID;
{
    cpp_Menus = menulist_New();
    cpp_Map = keymap_New();

    bind_BindList(cpptextBindings,cpp_Map,cpp_Menus,class_Load("srctextview")); /*RSK92fix*/
    return TRUE;
}

boolean cpptextview__InitializeObject(classID, self)
struct classheader *classID;
struct cpptextview *self;
{
    self->cpp_state = keystate_Create(self, cpp_Map);
    self->cpp_menus = menulist_DuplicateML(cpp_Menus, self);
    cpptextview_SetBorder(self,5,5);
    return TRUE;
}

void cpptextview__FinalizeObject(classID, self)
struct classheader *classID;
struct cpptextview *self;
{
    keystate_Destroy(self->cpp_state);
    menulist_Destroy(self->cpp_menus);
}

void cpptextview__PostMenus(self, menulist) 
struct cpptextview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->cpp_menus, menulist, self);
    super_PostMenus(self, self->cpp_menus);
}

struct keystate *cpptextview__PrependKeyState(self)
struct cpptextview *self;
{
    self->cpp_state->next= NULL;
    return keystate_AddBefore(self->cpp_state, super_PrependKeyState(self));
}

/* slash will END an existing comment style if preceded by an asterisk, or start a line comment style if preceded by another slash */ /*RSK92mod*/
static void slash(self, key)
struct cpptextview *self;
char key;
{
    struct cpptext *ct=(struct cpptext *)self->header.view.dataobject;
    int count=im_Argument(cpptextview_GetIM(self));
    long pos,oldpos;
    if (cpptextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && cpptextview_GetDotLength(self)>0)
	im_HandleMenu(cpptextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    oldpos= pos= cpptextview_CollapseDot(self);
    while (count--) cpptext_InsertCharacters(ct, pos++, &key, 1);
    if (oldpos && cpptext_GetChar(ct,oldpos-1)=='*')
	if (cpptext_GetStyle(ct, oldpos+1)==ct->header.srctext.comment_style)
	    /* terminate existing style */
	    environment_SetStyle(environment_GetEnclosing( ct->header.text.rootEnvironment, oldpos+1),FALSE,FALSE);
	else {
	    /* wrap a new style */
	    long start=oldpos-1;
	    while (--start>0) {
		if (cpptext_GetChar(ct,start)=='*')
		    if (cpptext_GetChar(ct,start-1)=='/') {
			/* found start of comment, wrap style */
			if (cpptext_GetStyle(ct,start) && cpptext_GetStyle(ct,start)!=ct->header.srctext.comment_style)
			    /* must be inside a preprocessor directive or some other unknown style. abort */
			    break;
			start--;
			if (cpptext_InString(ct,start))
			    break;
			cpptext_WrapStyleNow(ct, start,oldpos-start+1, ct->header.srctext.comment_style, FALSE,FALSE);
			break;
		    }
		    else if (cpptext_GetChar(ct,start+1)=='/')
			/* uh-oh, found another end of comment! */
			break;
	    }
	}
    else if (oldpos && cpptext_GetChar(ct,oldpos-1)=='/' && !cpptext_GetStyle(ct, oldpos-1) && !cpptext_InString(ct,pos))
	cpptext_WrapStyleNow(ct, oldpos-1,pos-oldpos+1, ct->header.srctext.linecomment_style, FALSE,TRUE);
    cpptextview_SetDotPosition(self,pos);
    cpptextview_FrameDot(self,pos);
    cpptext_NotifyObservers(ct,0);
}
