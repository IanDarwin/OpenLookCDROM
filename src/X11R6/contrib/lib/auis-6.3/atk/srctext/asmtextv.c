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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/asmtextv.c,v 1.7 1993/12/06 22:16:23 gk5g Exp $";
#endif



/*   asmtextView, an Assembly language mode for ATK. */

static char rcsHeader[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/asmtextv.c,v 1.7 1993/12/06 22:16:23 gk5g Exp $";

#include <andrewos.h>
#include <class.h>

#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <message.ih>

#include "asmtext.ih"
#include "asmtextv.eh"

static struct keymap *asm_Map;
static struct menulist *asm_Menus;

static struct bind_Description asmtextBindings[]={
    {"srctextview-start-comment", "*",'*'},
    {"srctextview-end-comment", "/",'/'},
    NULL
};

boolean asmtextview__InitializeClass(classID)
struct classheader *classID;
{
    asm_Menus = menulist_New();
    asm_Map = keymap_New();
    bind_BindList(asmtextBindings,asm_Map,asm_Menus,class_Load("srctextview")); /*RSK92fix*/
    return TRUE;
}

boolean asmtextview__InitializeObject(classID, self)
struct classheader *classID;
struct asmtextview *self;
{
    self->asm_state = keystate_Create(self, asm_Map);
    self->asm_menus = menulist_DuplicateML(asm_Menus, self);
    asmtextview_SetBorder(self,5,5);
    return TRUE;
}

void asmtextview__FinalizeObject(classID, self)
struct classheader *classID;
struct asmtextview *self;
{
    keystate_Destroy(self->asm_state);
    menulist_Destroy(self->asm_menus);
}

void asmtextview__PostMenus(self, menulist)
struct asmtextview *self;
struct menulist *menulist;
{
    menulist_ChainBeforeML(self->asm_menus, menulist, self);
    super_PostMenus(self, self->asm_menus);
}

struct keystate *asmtextview__PrependKeyState(self)
struct asmtextview *self;
{
    self->asm_state->next= NULL;
    return keystate_AddBefore(self->asm_state, super_PrependKeyState(self));
}

/* override */
/* hook all those bang-comment characters that were captured from .ezinit to proctable entries */
/* NOTE! asm_Map, the keymap, is shared by ALL asmtextview objects.  Since THIS asmtext object might not want bang-comment characters that some OTHER asmtext put in the keymap, we must double-check this in bangcomment() */
void asmtextview__SetDataObject( self, dataobj )
struct asmtextview *self;
struct asmtext *dataobj;
{
    char *bang= dataobj->bangComments;
    super_SetDataObject(self,dataobj);

    if (*bang!='\0') {
	/* add bang-comment keys to keymap */
	char ch[2]; ch[1]='\0';
	while (*bang!='\0') {
	    ch[0]= *bang;
	    keymap_BindToKey(asm_Map, ch, proctable_Lookup("srctextview-start-linecomment"), *ch);
            bang++;
	}
/*asmtextview_PostKeyState(self,kstate);*/
    }
}

/* override */
/* only check if c-comments are on */
void asmtextview__StartComment(self,key)
struct asmtextview *self;
char key; /* must be char for "&" to work. */
{
    struct asmtext *ct=(struct asmtext *)self->header.view.dataobject;
    if (asmtext_UseCComments(ct))
	super_StartComment(self,key);
    else
	asmtextview_SelfInsert(self,key);
}

/* override */
/* only check if c-comments are on */
void asmtextview__EndComment(self,key)
struct asmtextview *self;
char key; /* must be char for "&" to work. */
{
    struct asmtext *ct=(struct asmtext *)self->header.view.dataobject;
    if (asmtext_UseCComments(ct))
	super_EndComment(self,key);
    else
	asmtextview_SelfInsert(self,key);
}

/* override */
void asmtextview__StartLineComment(self,key)
struct asmtextview *self;
char key; /* must be char for "&" to work. */
{
    struct asmtext *ct=(struct asmtext *)self->header.view.dataobject;
    /* hold it! make sure THIS asmtext object is the one that mapped this key to be a bang-comment! */ /*RSK91add*/
    if (index(ct->bangComments,key))
	super_StartLineComment(self,key);
    else
	asmtextview_SelfInsert(self,key);
}

/* override */
void asmtextview__Reindent(self)
struct asmtextview *self;
{
    struct asmtext *ct= (struct asmtext *)self->header.view.dataobject;
    long pos= asmtextview_GetDotPosition(self), len= asmtextview_GetDotLength(self);

    if (len>0 && asmtext_IndentingEnabled(ct)) {
	/* a region is selected; grab whole lines and pass to external filter */
	long end= pos+len;
	if (asmtext_GetReadOnly(ct)) {
	    message_DisplayString(self, 0, "Document is read only.  Cannot reindent.\0");
	    return;
	}
	pos= asmtext_GetBeginningOfLine(ct,pos);
	end= asmtext_GetEndOfLine(ct,end)+1;
	asmtextview_SetDotPosition(self,pos);
	asmtextview_SetDotLength(self,end-pos);
	if (asmtext_HasReindentFilter(ct)) {
	    asmtextview_WaitCursorOn(self);
	    if (proctable_Lookup("filter-filter-region-thru-command")==NULL) {
		/* apparently filter gets loaded every time ez is invoked. */
		/* therefore, this never gets called (and never got tested) */
		/* (I'm not even sure class_Load *does* what needs doing in this case */
		class_Load("filter");
	    }
	    im_HandleMenu(asmtextview_GetIM(self), proctable_Lookup("filter-filter-region-thru-command"), self, asmtext_ReindentFilterName(ct));
	    asmtextview_WaitCursorOff(self);
	}
	else
	    message_DisplayString(self,0,"No reindentation filter name was specified in .ezinit");
    }
    else
	/* no region selected; move cursor to next tab stop */
	super_Reindent(self);
    asmtext_NotifyObservers(ct,0);
}
