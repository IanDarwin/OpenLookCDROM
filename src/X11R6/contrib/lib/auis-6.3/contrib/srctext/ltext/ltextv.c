/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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

/* LTextView, a ``Lisp'' mode for BE2. */

#ifndef NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/srctext/ltext/RCS/ltextv.c,v 2.6 1992/12/16 22:15:46 rr2b R6tape $";
#endif

#include <ctype.h>

#include <class.h>

#include <im.ih>
#include <message.ih>
#include <ltext.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>

#include <ltextv.eh>

static long BounceTime = 0;
static struct keymap *L_Map;
static struct menulist *L_Menus;

void paren(), newline(), redo(), tab();
#ifdef NOTUSED
static void indent_paren();
#endif /* NOTUSED */

static struct bind_Description ltextBindings[]={
/*    {"lisp-indent-paren",")",')',NULL,0,0,indent_paren,"Insert a paren, indented properly with balancing."}, */
    {"lisp-balance-paren",")",')',NULL,0,0,paren,"Insert a paren, with balancing."},
    {"lisp-balance-paren","}",'}'},
    {"lisp-balance-paren","]",']'},
    {"lisp-redo-styles","\033r",0,"Lisp,Redo Lisp styles~10",0,0,redo,"Wrap styles around comments and function names in lisp code."},
    {"lisp-redo-styles","\033R",0},
    {"lisp-tab","\t",0,"Lisp,Indent line/region~11",0,0,tab,"Indents the current line or selection region."},
    {"lisp-newline","\n",0,NULL,0,0,newline,"Inserts a newline and indents properly."},
    NULL
};

boolean ltextview__InitializeClass(classID)
    struct classheader *classID;
{
    L_Menus = menulist_New();
    L_Map = keymap_New();

    bind_BindList(ltextBindings,L_Map,L_Menus,&ltextview_classinfo);
    return TRUE;
}

boolean ltextview__InitializeObject(classID, self)
    struct classheader *classID;
    struct ltextview *self;
{
    self->l_state = keystate_Create(self, L_Map);
    self->l_menus = menulist_DuplicateML(L_Menus, self);
    ltextview_SetBorder(self,5,5);
    return TRUE;
}

long ltextview__SetBounceTime(classID, time)
    struct classheader *classID;
    long time;
{
    long retval = BounceTime;

    BounceTime = time;

    return retval;
}


void ltextview__PostKeyState(self, keystate)
struct ltextview *self;
struct keystate *keystate;
{
    keystate_AddBefore(self->l_state, keystate);
    super_PostKeyState(self, self->l_state);
}

void ltextview__PostMenus(self, menulist)
struct ltextview *self;
struct menulist *menulist;
{
    menulist_ChainAfterML(self->l_menus, menulist, 0);
    super_PostMenus(self, self->l_menus);
}


static void redo(self)
struct ltextview *self;
{
    struct ltext *c = (struct ltext *)self->header.view.dataobject;

    ltext_RedoStyles(c);
    ltext_RegionModified(c, 0, ltext_GetLength(c));
    ltext_NotifyObservers(c, 0);
}


static void match_parens(self, key)
struct ltextview *self;
char key;
{
    struct ltext *ct = (struct ltext *)self->header.view.dataobject;
    long start = ltextview_GetDotPosition(self), openparen = ltext_ReverseBalance(ct, start, EOF), pos;
    char buffer[256], *ptr;
    int i, c;

    if (openparen != EOF) {
        if (!ltextview_Visible(self, openparen)) {
            for (pos = openparen; pos > 0; pos--) {
                if (ltext_GetChar(ct, pos) == '\n') {
                    pos++;
                    break;
                }
            }

            ptr = buffer;
            for (i = sizeof(buffer) - 1; i > 0; i--)
                if ((c = ltext_GetChar(ct, pos++)) == '\n' || c == EOF)
                    break;
                else
                    *ptr++ = c;
            *ptr = NULL;

            message_DisplayString(self, 0, buffer);
        }
        else /* if (BounceTime == 0) */ {
            ltextview_SetDotPosition(self, openparen);
            ltextview_SetDotLength(self, start - openparen);
        }
        /* else Deal with bounce */
    }
    else
        switch (key) {
            case '}':
                message_DisplayString(self, 0, "No matching open brace.");
                break;
            case ')':
                message_DisplayString(self, 0, "No matching open parenthesis");
                break;
            case ']':
                message_DisplayString(self, 0, "No matching open bracket.");
                break;
            default:
                message_DisplayString(self, 0, "Parenthesis mis-match.\n");
        }
}
    
static void paren(self, key)
struct ltextview *self;
char key; /* must be char for "&" to work. */
{
    struct ltext *ct = (struct ltext *)self->header.view.dataobject;
    int count = im_Argument(ltextview_GetIM(self)), i, pos;

    ltextview_CollapseDot(self);
    pos = ltextview_GetDotPosition(self);
    
    for (i = 0; i < count; i++, pos++)
        ltext_InsertCharacters(ct, pos, &key, 1);
	
    ltextview_SetDotPosition(self, pos);
    ltextview_FrameDot(self, pos);

    match_parens(self, key);

    ltext_NotifyObservers(ct, 0);
}

#ifdef NOTUSED
static void indent_paren(self, key)
struct ltextview *self;
char key; /* must be char for "&" to work. */
{
    struct ltext *ct = (struct ltext *)self->header.view.dataobject;
    int count = im_Argument(ltextview_GetIM(self)), i, pos = ltextview_GetDotPosition(self);

    ltextview_CollapseDot(self);
    pos = ltextview_GetDotPosition(self);
    
    for (i = 0; i < count; i++, pos++)
        ltext_InsertCharacters(ct, pos, &key, 1);

    ltextview_SetDotPosition(self, pos);
    ltextview_FrameDot(self, pos);

    ltext_ReindentLine(ct,pos);

    match_parens(self, key);

    ltext_NotifyObservers(ct, 0);
}
#endif /* NOTUSED */

static void tab(self, key)
struct ltextview *self;
int key;
{
    struct ltext *ct = (struct ltext *)self->header.view.dataobject;
    int pos = ltextview_GetDotPosition(self), len = ltextview_GetDotLength(self), c = 0;
    struct mark *mark=ltext_CreateMark(ct,pos,len);

    if(len>0)
        ltext_Indent(ct,mark);
    else{
        int	oldPos=pos;

        do
            pos--;
        while (pos>=0 && (c=ltext_GetChar(ct,pos))!='\n' && isspace(c));

        if(pos<0 || c=='\n'){	/* indent line */
            mark_SetPos(mark,pos+1);
	    ltextview_SetDotPosition(self,ltext_Indent(ct,mark));
        }else		/* insert whitespace to next column */
            ltextview_SetDotPosition(self,ltext_TabAndOptimizeWS(ct,oldPos,4));
    }

    ltext_RemoveMark(ct,mark);
    mark_Destroy(mark);

    ltext_NotifyObservers(ct,0);
}


static void newline(self, key)
struct ltextview *self;
long key;
{
    int newlines = im_Argument(ltextview_GetIM(self));
    struct ltext *ct = (struct ltext *)self->header.view.dataobject;
    int c;
    long pos,end;
    struct mark *mark;

    ltextview_CollapseDot(self);
    ltext_ReindentLine(ct,ltextview_GetDotPosition(self));

    end=pos=ltextview_GetDotPosition(self); /* may have changed with ReindentLine */
    while(pos>0 && ((c=ltext_GetChar(ct,pos-1))==' ' || c=='\t'))
        pos--;
    if(pos<end)
        ltext_DeleteCharacters(ct,pos,end-pos);

    while(newlines--)
        ltext_InsertCharacters(ct,pos++,"\n",1);

    mark=ltext_CreateMark(ct,pos,0);

    ltextview_SetDotPosition(self,ltext_Indent(ct,mark));

    ltext_RemoveMark(ct,mark);
    mark_Destroy(mark);

    ltext_NotifyObservers(ct,0);
}
