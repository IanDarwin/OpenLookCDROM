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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrtext.c,v 2.9 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#include <class.h>
#include <entrtext.eh>
#include <stylesht.ih>
#include <style.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#define INITIALSIZE 64
boolean entertext__InitializeObject(classID,self)
struct classheader *classID;
struct entertext *self;
{
    if((self->buf = (char *)malloc(INITIALSIZE)) == NULL)return FALSE;
    self->buflen = 0;
    self->realbuflen = INITIALSIZE;
    *(self->buf) = '\0';
    self->mod = -1;
    self->needswrap = TRUE;
    self->Style = NULL;
    return TRUE;
}
boolean entertext__FinalizeObject(classID,self)
struct classheader *classID;
struct entertext *self;
{
    free(self->buf);
    return TRUE;
}
void entertext__updatebuf(self)
struct entertext *self;
{
    long len = entertext_GetLength(self) + 1;
    self->needswrap = FALSE;
    if(self->realbuflen < len){
	self->buf = (char *) realloc(self->buf,len);
	self->realbuflen = len;
    }
    self->buflen = len;
    self->mod = entertext_GetModified(self);
    len--;
    if(len > 0) entertext_CopySubString(self,0,len,self->buf,FALSE);
    self->buf[len] = '\0';
    entertext_NotifyObservers(self,entertext_BUFCHANGEDFLAG);
    if( entertext_GetGlobalStyle(self) != NULL)
	entertext_SetGlobalStyle(self, NULL);
    self->needswrap = TRUE;
}
boolean entertext__Changed(self)
struct entertext *self;
{
    return (boolean)(self->mod != entertext_GetModified(self));
}
void entertext__SetChars(self,str,len)
struct entertext *self;
char *str;
int len;
{
    self->needswrap = FALSE;
    entertext_Clear(self);
    if(len && str && *str) entertext_InsertCharacters(self,0,str,len);
    self->needswrap = TRUE;
}
static checkstyles(self)
struct entertext *self;
{
    
    if(self->needswrap && entertext_GetGlobalStyle(self) == NULL){
	if ((self->Style = stylesheet_Find(self->header.text.styleSheet, "italic")) == NULL){
		self->Style = style_New();
		style_SetName(self->Style, "italic");
		stylesheet_Add(self->header.text.styleSheet, self->Style);
		style_AddNewFontFace(self->Style, fontdesc_Italic);
	    }
	entertext_SetGlobalStyle(self, self->Style);
    }
}
boolean entertext__InsertCharacters(self, pos, str, len)
struct entertext *self;
long pos;
char *str;
long len;  
{
    if(!super_InsertCharacters(self, pos, str, len)) return FALSE;
    checkstyles(self);
    return TRUE;
}
boolean entertext__DeleteCharacters(self, pos, len)
struct entertext *self;
long pos;
long len;  {
    if(!super_DeleteCharacters(self, pos, len)) return FALSE;
    if(entertext_GetLength(self) > 0)
	checkstyles(self);
    return TRUE;
}

char *entertext__ViewName(self)
struct entertext *self;
{
    return ("etextview");
}
