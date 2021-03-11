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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/srctext/ptext/RCS/ptextv.c,v 1.6 1993/01/08 16:36:06 rr2b R6tape $";
#endif


 

/* ptextview, a ``Pascal'' mode for ATK */

#include <ctype.h>

#include <class.h>

#include <im.ih>
#include <message.ih>
#include <ptext.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <envrment.ih>
#include <ptextv.eh>
#include <textv.ih>

static long BounceTime = 0;
static struct keymap *M_Map;
static struct menulist *M_Menus;

static void parse(), paren(),brace(),newline(),redo(),
     tab(),asterisk(),space(), ptextv_rename();

#define Ptext(Self) \
    ((struct ptext *) (Self)->header.view.dataobject)

static struct bind_Description ptextBindings[]={
    {"ptextview-paren",")",')', NULL,0, 0, paren,
     "Insert a paren, with balancing."},
    {"ptextview-bar","|",'|',NULL,0,0,space,""},
    {"ptextview-period",".",'.',NULL,0,0,space,""},
    {"ptextview-comma", ",", ',', NULL,0,0,space,""},
    {"ptextview-cr","\r", '\n', NULL, 0, 0, space, ""},
    {"ptextview-paren","]",']'},
    {"ptextview-brace","}",'}', NULL,0, 0, brace,
     "Insert a brace indented properly with balancing."},
    {"ptextview-asterisk","*",'*', NULL,0, 0, asterisk,""},
    {"ptextview-startbrace","{",'{',NULL,0,0,asterisk,""},
    {"ptextview-space"," ",' ', NULL,0, 0, space,""},
    {"ptextview-left-paren","(",'(', NULL,0, 0, space,""},
    {"ptextview-left-brace","[",'[', NULL,0, 0, space,""},
    {"ptextview-semicolon",";",';', NULL,0, 0, space,""},
    {"ptextview-redo-styles","\033r",0,
     "Pascal Text,Redo Styles~10", 0,0, redo,
     "Wrap styles around comments and keywords in Pascal code."},
    {"ptextview-redo-styles","\033R",0},
    {"ptextview-tab", "\t", 0,
     "Pascal Text, Indent line/region~11", 0,0, tab,
     "Indents the current line or selection region."},
    {"ptextview-newline", "\n", 0, NULL,0, 0, newline,
     "Inserts a newline and indents properly."},
    {"ptextview-rename-identifier","\033Q",0,
     "Pascal Text,Rename Identifier~20",0,
      0,ptextv_rename,"Rename identifier in selection region."},
    NULL
};

static boolean isident(c)
char c;
{
    return (isalnum(c) || c == '_');
}

boolean ptextview__InitializeClass(classID)
struct classheader *classID;
{
    M_Menus = menulist_New();
    M_Map = keymap_New();

    bind_BindList(ptextBindings,
        M_Map,M_Menus,&ptextview_classinfo);

    return TRUE;
}

boolean ptextview__InitializeObject(classID, self)
struct classheader *classID;
struct ptextview *self;
{
    self->c_state = keystate_Create(self, M_Map);
    self->c_menus = menulist_DuplicateML(M_Menus, self);
    ptextview_SetBorder(self,5,5); 
    return TRUE;
}

long ptextview__SetBounceTime(classID, time)
struct classheader *classID;
long time;
{
    long retval = BounceTime;

    BounceTime = time;

    return retval;
}

void ptextview__PostKeyState(self, keystate)
struct ptextview *self;
struct keystate *keystate;
{
    keystate_AddBefore(self->c_state, keystate);
    super_PostKeyState(self, self->c_state);
}

void ptextview__PostMenus(self, menulist)
struct ptextview *self;
struct menulist *menulist;
{
    menulist_ChainAfterML(self->c_menus, menulist, 0);
    super_PostMenus(self, self->c_menus);

}

static void redo(self)
struct ptextview *self;
{
    struct ptext *pt = Ptext(self);

    ptext_RedoStyles(pt);
    ptext_RegionModified(pt, 0, ptext_GetLength(pt));
    ptext_NotifyObservers(pt, 0);
}

static void match_parens(self, key)
struct ptextview *self;
char key;
{
    struct ptext *pt = Ptext(self);
    long start = ptextview_GetDotPosition(self),
     openparen = ptext_ReverseBalance(pt, start), pos;
    char buffer[256], *ptr;
    int i;

    if (openparen != EOF)
    {
	if (!ptextview_Visible(self, openparen)) {

	    for (pos = openparen; pos > 0; pos--)
	    {
		if (ptext_GetChar(pt, pos) == '\n')
		{
		    pos++;
		    break;
		}
	    }

	    ptr = buffer;
	    for (i = sizeof(buffer) - 1; i > 0; i--)
		if ((*ptr++ = ptext_GetChar(pt, pos++)) == '\n')
		    break;
	    *ptr = NULL;

	    message_DisplayString(self, 0, buffer);
	}
	else /* if (BounceTime == 0) */
	{
	    ptextview_SetDotPosition(self, openparen);
	    ptextview_SetDotLength(self, start - openparen);
	}
	/* else Deal with bounce */
    }
    else
	switch (key)
	{
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
struct ptextview *self;
char key; /* must be char for "&" to work. */
{
    struct ptext *pt = Ptext(self);
    int count = im_Argument(ptextview_GetIM(self)), i, oldpos,pos;
    ptextview_CollapseDot(self);
    oldpos=pos = ptextview_GetDotPosition(self);
    
    if (pos)
        ptext_BackwardCheckWord(pt, pos - 1, 0);

    for (i = 0; i < count; i++, pos++)
	ptext_InsertCharacters(pt, pos, &key, 1);

    if (oldpos&&(ptext_GetChar(pt,oldpos-1)=='*') &&
        (ptext_GetStyle(pt,oldpos-1) == pt->comment_style)) {
	if (environment_Remove(pt->header.text.rootEnvironment,
          oldpos, 2, environment_Style, FALSE))
            ptext_SetModified(pt);
    }

    ptextview_SetDotPosition(self, pos);
    ptextview_FrameDot(self, pos);

    match_parens(self, key);

    ptext_NotifyObservers(pt, 0);
}

static void brace(self, key)
struct ptextview *self;
char key; /* must be char for "&" to work. */
{
    struct ptext *pt = Ptext(self);
    int count = im_Argument(ptextview_GetIM(self)), i, pos, oldpos;

    ptextview_CollapseDot(self);
    oldpos = pos = ptextview_GetDotPosition(self);

    for (i = 0; i < count; i++, pos++)
	ptext_InsertCharacters(pt, pos, &key, 1);

    if (key == '}' && ptext_GetStyle(pt, oldpos) == pt->comment_style)
        if(environment_Remove(pt->header.text.rootEnvironment,
          oldpos,2,environment_Style,FALSE))
            ptext_SetModified(pt);

    ptextview_SetDotPosition(self, pos);
    ptextview_FrameDot(self, pos);

    ptext_ReindentLine(pt,pos);

    match_parens(self, key);

    ptext_NotifyObservers(pt, 0);
}

static void asterisk(self, key)
struct ptextview *self;
char key; /* must be char for "&" to work. */
{
    struct ptext *pt = Ptext(self);
    int count = im_Argument(ptextview_GetIM(self)), i,oldpos, pos = ptextview_GetDotPosition(self);

    ptextview_CollapseDot(self);
    oldpos=pos = ptextview_GetDotPosition(self);

    for (i = 0; i < count; i++, pos++)
	ptext_InsertCharacters(pt, pos, &key, 1);

    if (key == '{')
        ptext_InsertNewStyle(pt,oldpos,count+1, pt->comment_style,TRUE,FALSE);

    else if(oldpos) {
	if(ptext_GetChar(pt,oldpos-1)=='(') {
	    ptext_InsertNewStyle(pt,oldpos,count+1, pt->comment_style,TRUE,FALSE);
	}
    }
    ptextview_SetDotPosition(self, pos);
    
    ptext_NotifyObservers(pt, 0);
}

static void tab(self, key)
struct ptextview *self;
long key;
{
    struct ptext *pt = Ptext(self);
    int pos = ptextview_GetDotPosition(self), len = ptextview_GetDotLength(self), c = 0;
    struct mark *mark=ptext_CreateMark(pt,pos,len);

    if(len>0)
	ptext_Indent(pt,mark);
    else
    {
	int	oldPos=pos;

	do
	    pos--;
	while (pos>=0 && (c=ptext_GetChar(pt,pos))!='\n' && isspace(c));

	if(pos<0 || c=='\n')	/* indent line */
	{
	    mark_SetPos(mark,pos+1);
	    ptextview_SetDotPosition(self,ptext_Indent(pt,mark));
	}
	else		/* insert whitespace to next column */
	    ptextview_SetDotPosition(self,ptext_TabAndOptimizeWS(pt,oldPos,4));
    }

    ptext_RemoveMark(pt,mark);
    mark_Destroy(mark);

    ptext_NotifyObservers(pt,0);
}
static void space(self,key)
struct ptextview *self;
char key;
{
    int oldpos,pos,count=im_Argument(ptextview_GetIM(self));
    struct ptext *pt = Ptext(self);
    ptextview_CollapseDot(self);
    oldpos=pos=ptextview_GetDotPosition(self);
    while(count--) ptext_InsertCharacters(pt,pos++,&key,1);
    if(oldpos) ptext_BackwardCheckWord(pt,oldpos-1,0);
    ptextview_SetDotPosition(self,pos);
    ptext_NotifyObservers(pt,0);
}
    
static void newline(self, key)
struct ptextview *self;
long key;
{
    int newlines = im_Argument(ptextview_GetIM(self));
    struct ptext *pt = Ptext(self);
    int c;
    long pos,end;
    struct mark *mark;

    ptextview_CollapseDot(self);
    pos=ptextview_GetDotPosition(self);
    if(pos) ptext_BackwardCheckWord(pt,pos-1,0);
    ptext_ReindentLine(pt,pos);
    end=pos=ptextview_GetDotPosition(self); /* may have changed with ReindentLine */
    while(pos>0 && ((c=ptext_GetChar(pt,pos-1))==' ' || c=='\t'))
	pos--;
    if(pos<end)
	ptext_DeleteCharacters(pt,pos,end-pos);

    while(newlines--)
	ptext_InsertCharacters(pt,pos++,"\n",1);
    
    mark=ptext_CreateMark(pt,pos,0);

    ptextview_SetDotPosition(self,ptext_Indent(pt,mark));
    ptext_RemoveMark(pt,mark);
    mark_Destroy(mark);
    ptext_NotifyObservers(pt,0);
}

static void ptextv_rename(self, key)
struct ptextview *self;
long key;
{
    struct ptext *pt = Ptext(self);
    int pos, len, newlen;
    boolean esc, discardIdent;
    int insideQuotes;
    char orig[40], rep[40];
    int origlen, replen;

    pos = ptextview_GetDotPosition(self);
    newlen = len = ptextview_GetDotLength(self);

    if (len == 0) {
        message_DisplayString(self, 0, "No region selected\n");
        return;
    }

    if (message_AskForString(self, 0, "Replace identifier: ",
      NULL, orig, sizeof (orig)) < 0 || orig[0] == '\0' ||
      message_AskForString(self, 0, "New string: ", NULL,
      rep, sizeof (rep)) < 0) {
        message_DisplayString(self, 0, "Cancelled.");
        return;
    }

    origlen = strlen(orig);
    replen = strlen(rep);

    /* Skip an identifier that's partially outside region */

    discardIdent =
      (isident(ptext_GetChar(pt, pos)) && pos != 0 &&
       isident(ptext_GetChar(pt, pos - 1)));

    insideQuotes = 0;       /* Likely a correct assumption */
    esc = FALSE;

    for (; len >= origlen; pos++, len--) {
        int c = ptext_GetChar(pt, pos);
        if (esc) {
            esc = FALSE;
            continue;
        }
        if (c == '\\') {
            esc = TRUE;
            continue;
        }
        if (discardIdent) {
            if (isident(c))
                continue;
            discardIdent = FALSE;
        }
        if (insideQuotes) {
            if (c == insideQuotes)
                insideQuotes = 0;
            continue;
        }
        if (c == '"' || c == '\'')
            insideQuotes = c;
        if (! isident(c))
            continue;
        discardIdent = TRUE;
        if (ptext_Strncmp(pt, pos, orig, origlen) == 0 &&
          ! isident(ptext_GetChar(pt, pos + origlen))) {
            ptext_ReplaceCharacters(pt, pos, origlen, rep, replen);
            len -= origlen - replen - 1;
            newlen += replen - origlen;
        }
    }

    ptextview_SetDotLength(self, newlen);
    ptext_NotifyObservers(pt, 0);
}
