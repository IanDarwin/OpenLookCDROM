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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/utils/RCS/strinput.c,v 1.5 1993/09/17 20:32:07 rr2b Exp $";
#endif


/* $Author: rr2b $ */

 
/*
 * strinput.c
 *
 * String input dialogue box thingie.
*/

#include <stdio.h>

#include <class.h>
#include <environ.ih>
#include <fontdesc.ih>
#include <observe.ih>
#include <strinput.eh>
#include <style.ih>
#include <text.ih>
#include <textv.ih>
#include <view.ih>

#define debug(x) /* printf x ; fflush(stdin); */
static struct style *promptStyle = NULL;

boolean strinput__InitializeClass(classID)
struct classheader classID;
{
    if (!promptStyle && !(promptStyle = style_New()))
	return FALSE;
    if(promptStyle) {
	style_AddNewFontFace(promptStyle, fontdesc_Bold);
	style_SetName(promptStyle, "bold");
    }
    return TRUE;
}


boolean strinput__InitializeObject(classID, self)
struct classheader *classID;
struct strinput *self;
{
    self->textobj  = text_New();
    self->textv    = textview_New();
    self->prompt   = NULL;
    textview_SetDataObject(self->textv, self->textobj);
    return TRUE;
}

void strinput__FinalizeObject(classID, self)
struct classheader *classID;
struct strinput *self;
{
    if (self->textv) textview_Destroy(self->textv);
    if (self->textobj) text_Destroy(self->textobj);
    if (self->prompt) free(self->prompt);
}

char *strinput__GetInput(self, length)
struct strinput *self;
int length;
{
    int len,pos;
    char *string;

    len = text_GetLength(self->textobj);
    pos = text_GetFence(self->textobj);
    len = len - pos;
    if (length <= 0) return NULL;

    if (length < len) len = length;
    string = (char *) malloc(len + 1);

    text_CopySubString(self->textobj, pos, len, string, TRUE);
    
    return string;
}

void strinput__SetPrompt(self, string)
struct strinput *self;
char *string;
{
    int fence, len;

    len = (string ? strlen(string) : 0);
    if (string != self->prompt) {
	if(self->prompt)
	    free(self->prompt);
	self->prompt = (char *) malloc((len+1));
	strcpy(self->prompt, (string ? string : ""));
    }

    /* Now to change the text */
    fence = text_GetFence(self->textobj);
    text_ClearFence(self->textobj);
    text_ReplaceCharacters(self->textobj, 0, fence, string, len);
    text_SetFence(self->textobj, len);
    text_AlwaysAddStyle(self->textobj, 0, len - 1, promptStyle);
    /* Now to change the view, iff it's linked into the viewTree */
    textview_SetDotPosition(self->textv, text_GetLength(self->textobj));
    strinput_WantUpdate(self, self->textv);
}	  

void strinput__ClearInput(self)
struct strinput *self;
{
    text_Clear(self->textobj);
    strinput_SetPrompt(self, self->prompt);
    /* Now to change the view, iff it's linked into the viewTree */
    textview_SetDotPosition(self->textv, text_GetLength(self->textobj));
}

void strinput__SetInput(self, string)
struct strinput *self;
char *string;
{
    text_Clear(self->textobj);
    strinput_SetPrompt(self, self->prompt);

    text_InsertCharacters(self->textobj, text_GetLength(self->textobj), 
			   (string ? string : ""), 
			   (string ? strlen(string) : 0));
    /* Now to change the view, iff it's linked into the viewTree */
    textview_SetDotPosition(self->textv, text_GetLength(self->textobj));
}

void strinput__FullUpdate(self, type, left, top, width, height)
struct strinput *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle child;

    strinput_GetLogicalBounds(self, &child);
    textview_InsertView(self->textv, self, &child);
    textview_FullUpdate(self->textv, type, 0, 0, child.width, child.height);
}

struct view *strinput__Hit(self, action, x, y, clicks)
struct strinput *self;
enum view_MouseAction action;
long x, y, clicks;
{
    return textview_Hit(self->textv, action, x, y, clicks);
}

boolean strinput__HaveYouGotTheFocus(self)
struct strinput *self;
{
    return(self->textv->hasInputFocus);
}

void strinput__ReceiveInputFocus(self)
struct strinput *self;
{
    super_ReceiveInputFocus(self);
    strinput_WantInputFocus(self, self->textv);
}

void
strinput__LinkTree( self, parent )
register struct strinput *self;
register struct view *parent;
{
    super_LinkTree(self, parent);
    if(self->textv)
	textview_LinkTree(self->textv, self);
}
