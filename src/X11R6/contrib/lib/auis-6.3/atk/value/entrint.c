/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrint.c,v 1.1 1993/08/20 20:05:27 susan Exp $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrint.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrint.c,v 1.1 1993/08/20 20:05:27 susan Exp $ ";
#endif /* lint */

#include <class.h>
#include <entrint.eh>
#include <stylesht.ih>
#include <style.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#define INITIALSIZE 64
boolean enterint__InitializeObject(classID,self)
struct classheader *classID;
struct enterint *self;
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
boolean enterint__FinalizeObject(classID,self)
struct classheader *classID;
struct enterint *self;
{
    free(self->buf);
    return TRUE;
}
void enterint__updatebuf(self)
struct enterint *self;
{
    long len = enterint_GetLength(self) + 1;
    self->needswrap = FALSE;
    if (atoi(self->buf) < 0)
      return;
    if(self->realbuflen < len){
	self->buf = (char *) realloc(self->buf,len);
	self->realbuflen = len;
    }
    self->buflen = len;
    self->mod = enterint_GetModified(self);
    len--;
    if(len > 0) enterint_CopySubString(self,0,len,self->buf,FALSE);
    self->buf[len] = '\0';
    enterint_NotifyObservers(self,enterint_BUFCHANGEDFLAG);
    if( enterint_GetGlobalStyle(self) != NULL)
	enterint_SetGlobalStyle(self, NULL);
    self->needswrap = TRUE;
}
boolean enterint__Changed(self)
struct enterint *self;
{
    return (boolean)(self->mod != enterint_GetModified(self));
}
void enterint__SetChars(self,str,len)
struct enterint *self;
char *str;
int len;
{
    self->needswrap = FALSE;
    enterint_Clear(self);
    if(len && str && *str) enterint_InsertCharacters(self,0,str,len);
    self->needswrap = TRUE;
}
static checkstyles(self)
struct enterint *self;
{
    
    if(self->needswrap && enterint_GetGlobalStyle(self) == NULL){
	if ((self->Style = stylesheet_Find(self->header.text.styleSheet, "italic")) == NULL){
		self->Style = style_New();
		style_SetName(self->Style, "italic");
		stylesheet_Add(self->header.text.styleSheet, self->Style);
		style_AddNewFontFace(self->Style, fontdesc_Italic);
	    }
	enterint_SetGlobalStyle(self, self->Style);
    }
}
boolean enterint__InsertCharacters(self, pos, str, len)
struct enterint *self;
long pos;
char *str;
long len;  
{
    if(atoi(str) <=0) {
      enterint_Clear(self);
      return FALSE;
    }
    if(!super_InsertCharacters(self, pos, str, len)) return FALSE;
    checkstyles(self);
    return TRUE;
}
boolean enterint__DeleteCharacters(self, pos, len)
struct enterint *self;
long pos;
long len;  {
    if(!super_DeleteCharacters(self, pos, len)) return FALSE;
    if(enterint_GetLength(self) > 0)
	checkstyles(self);
    return TRUE;
}

char *enterint__ViewName(self)
struct enterint*self;
{
    return ("eintview");
}
