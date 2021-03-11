/* File modtextv.c created by R L Quinn
   
   ModtextView, a Modula-X mode for ATK. */
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/modtextv.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <envrment.ih> /* for comment insertion */
#include <im.ih>
#include <environ.ih> /* for autocut preference */
#include <proctbl.ih> /* for autocut preference */

#include "modtext.ih"
#include "modtextv.eh"

/* AutoCut was not made externally visible by txtvcmod, so WE have to check the preference TOO */
static int autocut_mode = -1;	/* uninitialized */
#define IsAutoCutMode() ((autocut_mode == -1 && (autocut_mode = environ_GetProfileSwitch("autocut", FALSE))) || autocut_mode)

static struct keymap *mod_Map;
static struct menulist *mod_Menus;

void startPreproc();

static struct bind_Description modtextBindings[]={
    {"modtextview-start-preproc","#",'#', NULL,0,0, startPreproc, "Start preprocessor style if pressed at start of line."},
    {"srctextview-self-insert-reindent","|",'|'},
    {"srctextview-style-string","\"",'"'},
    {"srctextview-style-string","'",'\''},
    NULL
};

boolean modtextview__InitializeClass(classID)
struct classheader *classID;
{
    mod_Menus = menulist_New();
    mod_Map = keymap_New();
    bind_BindList (modtextBindings,mod_Map,mod_Menus,class_Load("srctextview")); /*RSK92fix*/
    return TRUE;
}

boolean modtextview__InitializeObject(classID, self)
struct classheader *classID;
struct modtextview *self;
{
    self->mod_state = keystate_Create(self, mod_Map);
    self->mod_menus = menulist_DuplicateML(mod_Menus, self);
    modtextview_SetBorder(self,5,5); 
    return TRUE;
}

void modtextview__FinalizeObject(classID, self)
struct classheader *classID;
struct modtextview *self;
{
    keystate_Destroy(self->mod_state);
    menulist_Destroy(self->mod_menus);
}

void modtextview__PostMenus(self, menulist)
struct modtextview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->mod_menus, menulist, self);
    super_PostMenus(self, self->mod_menus);
}

struct keystate *modtextview__PrependKeyState(self)
struct modtextview *self;
{
    self->mod_state->next= NULL;
    return keystate_AddBefore(self->mod_state, super_PrependKeyState(self));
}

/* override */
void modtextview__Paren(self, key) /*RSK91mod*/
struct modtextview *self;
char key; /* must be char for "&" to work. */
{
    if (key=='}')
	modtextview_SelfInsertReindent(self,key);
    else if (key==')') {
	struct modtext *ct=(struct modtext *)self->header.view.dataobject;
	long openparen,oldpos=modtextview_GetDotPosition(self);
	modtextview_SelfInsert(self,key);
	openparen= modtext_ReverseBalance(ct,modtextview_GetDotPosition(self));
	if (oldpos && (modtext_GetChar(ct,oldpos-1)=='*') && (modtext_GetStyle(ct,oldpos-1)==ct->header.srctext.comment_style))
	    if (modtext_GetStyle(ct,openparen) != ct->header.srctext.comment_style) /*RSKadd; only stops italicizing if matching "(*" was NOT italicized (not nested)*/
		environment_SetStyle(modtext_GetEnvironment(ct, oldpos), FALSE,FALSE);
    }
    else
	modtextview_SelfInsert(self,key);
    modtextview_MatchParens(self,key);
}

/* override */
/* HandleEndOfLineStyle will terminate both linecomment AND preprocessor styles when a newline is added (if not \quoted) */
void modtextview__HandleEndOfLineStyle(self, pos)
struct modtextview *self;
long pos;
{
    struct modtext *ct=(struct modtext *)self->header.view.dataobject;
    if (ct->preprocessor) {
	if (modtext_GetStyle(ct,pos)==ct->header.srctext.linecomment_style || (modtext_GetStyle(ct,pos)==ct->header.srctext.kindStyle[PREPRC] && !modtext_Quoted(ct,pos))) {
	    long start=pos;
	    while (modtext_GetChar(ct,start-1)=='\n' && !modtext_Quoted(ct,start-1)) --start;
	    environment_Remove(ct->header.text.rootEnvironment, start,pos-start+1, environment_Style, FALSE);
	}
    } else
	super_HandleEndOfLineStyle(self,pos);
}

static void startPreproc(self, key)
struct modtextview *self;
char key;
{
    struct modtext *ct=(struct modtext *)self->header.view.dataobject;
    long pos, oldpos;
    if (modtextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && modtextview_GetDotLength(self)>0)
	im_HandleMenu(modtextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= oldpos= modtextview_CollapseDot(self);
    while (pos>0 && is_whitespace(modtext_GetChar(ct,--pos))) ;
    if (ct->preprocessor && (pos==0 || (modtext_GetChar(ct,pos)=='\n' && !modtext_Quoted(ct,pos))) && !modtext_GetStyle(ct,oldpos)) {
	int count=im_Argument(modtextview_GetIM(self));
	pos= oldpos;
	while (count--) modtext_InsertCharacters(ct,pos++,&key,1);
	modtext_WrapStyleNow(ct, oldpos,pos-oldpos, ct->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
	modtextview_SetDotPosition(self,pos);
	if (modtext_IndentingEnabled(ct))
	    modtext_ReindentLine(ct, pos);
	modtext_NotifyObservers(ct,0);
    }
    else /* not the start of a preprocessor line */
	modtextview_SelfInsert(self,key);
}
