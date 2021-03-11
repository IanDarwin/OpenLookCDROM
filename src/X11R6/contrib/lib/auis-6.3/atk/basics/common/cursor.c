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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/cursor.c,v 2.6 1992/12/15 21:27:21 rr2b R6tape $";
#endif


 

#include <class.h>
#include <fontdesc.ih>
#include <graphic.ih>
#include <view.ih>
#include <im.ih>

#include <cursor.eh>


static char CursorTable[] = {  'a',
    'a','g','x','H','h','l','u','v','w','|',
    'J',':','C','R','M',';','/','^','s','U',
    'D','@','X','~','}' };
#define lookup(A) ((A >= 0 && A < Cursor_TABLELEN)?  CursorTable[A] :CursorTable[0])



void cursor__ChangeShape(self)
struct cursor *self; {
    printf("cursor_ChangeShape: missing method\n");
}


void cursor__SetGlyph(self,fd,ch)
struct cursor *self;
struct fontdesc *fd;
short ch;
{
     if (self->fillFont != fd || self->fillChar != ch)  {
	self->fillFont = fd;
	self->fillChar = ch;
	cursor_ChangeShape(self);
     }
}

void cursor__SetStandard(self,ch)
struct cursor *self;
short ch;
{
    short c = lookup(ch);

     if (self->fillFont != NULL || self->fillChar != c)  {
	self->fillFont = NULL;
	self->fillChar = c;
	cursor_ChangeShape(self);
    }
}

boolean cursor__InitializeObject(classID,self)
struct classheader *classID;
struct cursor *self;
{
	self->view = NULL;
	self->posted = NULL;
	self->windowim = NULL;
	self->next = NULL;
	self->processC = FALSE;
	self->changed = FALSE;
        self->fillChar = 0;
        self->fillFont = NULL;
	return TRUE;
}

struct cursor *cursor__Create(classID, view)
struct classheader *classID;
struct view *view;
{

	struct cursor *c = im_GetCursor();
	c->view = view;
	return(c);
}

void cursor__FinalizeObject(classID,self)
struct classheader *classID;
struct cursor *self;
{
	if(cursor_IsPosted(self)) im_RetractCursor(self->posted,self);
	if(cursor_IsWindowCursor(self)) im_SetWindowCursor(self->windowim,NULL);
	if(cursor_IsProcessCursor(self)) im_SetProcessCursor(NULL);
}
