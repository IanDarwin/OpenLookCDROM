/* File ctextv.c created by R L Quinn
  
   CtextView, a C mode for ATK. */
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/ctextv.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>

#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <envrment.ih>
#include <im.ih>
#include <environ.ih> /* for autocut preference */
#include <proctbl.ih> /* for autocut preference */

#include "ctext.ih"
#include "ctextv.eh"
#include "srctext.ih"

/* AutoCut was not made externally visible by txtvcmod, so WE have to check the preference TOO */
static int autocut_mode = -1;	/* uninitialized */
#define IsAutoCutMode() ((autocut_mode == -1 && (autocut_mode = environ_GetProfileSwitch("autocut", FALSE))) || autocut_mode)

static struct keymap *c_Map;
static struct menulist *c_Menus;

void startPreproc();

static struct bind_Description ctextBindings[]={
    {"ctextview-start-preproc","#",'#', NULL,0,0, startPreproc, "Start preprocessor style if pressed at start of line."},
    {"srctextview-self-insert-reindent","{",'{'},
    {"srctextview-start-comment", "*",'*'},
    {"srctextview-end-comment", "/",'/'},
    {"srctextview-style-label",":",':'},
    {"srctextview-style-string","\"",'"'},
    {"srctextview-style-string","'",'\''},
    NULL
};

boolean ctextview__InitializeClass(classID)
struct classheader *classID;
{
    c_Menus = menulist_New();
    c_Map = keymap_New();
    bind_BindList(ctextBindings,c_Map,c_Menus,class_Load("srctextview")); /*RSK92fix*/
    return TRUE;
}

boolean ctextview__InitializeObject(classID, self)
struct classheader *classID;
struct ctextview *self;
{
    self->c_state = keystate_Create(self, c_Map);
    self->c_menus = menulist_DuplicateML(c_Menus, self);
    ctextview_SetBorder(self,5,5);
    return TRUE;
}

void ctextview__FinalizeObject(classID, self)
struct classheader *classID;
struct ctextview *self;
{
    keystate_Destroy(self->c_state);
    menulist_Destroy(self->c_menus);
}

void ctextview__PostMenus(self, menulist)
struct ctextview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->c_menus, menulist, self);
    super_PostMenus(self, self->c_menus);
}

struct keystate *ctextview__PrependKeyState(self)
struct ctextview *self;
{
    self->c_state->next= NULL;
    return keystate_AddBefore(self->c_state, super_PrependKeyState(self));
}

/* override */
/* Paren is overridden so that an end-brace will reindent the line */
void ctextview__Paren(self, key)
struct ctextview *self;
char key; /* must be char for "&" to work. */
{
    struct ctext *ct = (struct ctext *)self->header.view.dataobject;
    long pos=ctextview_GetDotPosition(self);
    if (key=='}')
	ctextview_SelfInsertReindent(self,key);
    else
	ctextview_SelfInsert(self,key);
    if (!ctext_InCommentStart(ct,pos) && !ctext_InString(ct,pos))
	ctextview_MatchParens(self, key);
}

/* override */
/* HandleEndOfLineStyle will terminate both linecomment AND preprocessor styles when a newline is added (if not \quoted) */
void ctextview__HandleEndOfLineStyle(self, pos)
struct ctextview *self;
long pos;
{
    struct ctext *ct=(struct ctext *)self->header.view.dataobject;

    if (ctext_GetStyle(ct,pos)==ct->header.srctext.linecomment_style || (ctext_GetStyle(ct,pos)==ct->header.srctext.kindStyle[PREPRC] && !ctext_Quoted(ct,pos))) {
	long start=pos;
	while (ctext_GetChar(ct,start-1)=='\n' && !ctext_Quoted(ct,start-1)) --start;
	environment_Remove(ct->header.text.rootEnvironment, start,pos-start+1, environment_Style, FALSE);
    }
}

static void startPreproc(self, key)
struct ctextview *self;
char key;
{
    struct ctext *ct=(struct ctext *)self->header.view.dataobject;
    long pos, oldpos;
    if (ctextview_ConfirmReadOnly(self))
	return;
    if (IsAutoCutMode() && ctextview_GetDotLength(self)>0)
	im_HandleMenu(ctextview_GetIM(self), proctable_Lookup("textview-zap-region"), self, 0); /* not a particularly efficient way to call textview_ZapRegionCmd, but what else ya gonna do? */
    pos= oldpos= ctextview_CollapseDot(self);
    while (pos>0 && is_whitespace(ctext_GetChar(ct,--pos))) ;
    if ((pos==0 || (ctext_GetChar(ct,pos)=='\n' && !ctext_Quoted(ct,pos))) && !ctext_GetStyle(ct,oldpos)) {
	int count=im_Argument(ctextview_GetIM(self));
	pos= oldpos;
	while (count--) ctext_InsertCharacters(ct,pos++,&key,1);
	ctext_WrapStyleNow(ct, oldpos,pos-oldpos, ct->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
	ctextview_SetDotPosition(self,pos);
	if (ctext_IndentingEnabled(ct))
	    ctext_ReindentLine(ct, pos);
	ctext_NotifyObservers(ct,0);
    }
    else /* not the start of a preprocessor line */
	ctextview_SelfInsert(self,key);
}
