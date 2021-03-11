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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/menttext.c,v 2.16 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#include <andrewos.h>
#include <class.h>
#include <stylesht.ih>
#include <style.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#include <menttext.eh>
#define INITIALSIZE 64

#define Root(self) (((struct text *)self)->rootEnvironment)
#define MyEnvinfo(text,pos) environment_GetInnerMost(Root(self), pos)
#define Enclosed(text,pos) Root(self) != MyEnvinfo(text,pos)

boolean mentertext__InitializeObject(classID,self)
struct classheader *classID;
struct mentertext *self;
{
    if((self->buf = malloc(INITIALSIZE)) == NULL)return FALSE;
    self->buflen = 0;
    self->realbuflen = INITIALSIZE;
    *(self->buf) = '\0';
    self->mod = -1;
    self->needswrap = TRUE;
    self->Style = NULL;
    self->src = NULL;
    self->bufp[0] = "";
    return TRUE;
}
static checkstyles(self)
struct mentertext *self;
{

    if ((self->Style = stylesheet_Find(self->header.text.styleSheet, "italic")) == NULL){
	self->Style = style_New();
	style_SetName(self->Style, "bold");
	stylesheet_Add(self->header.text.styleSheet, self->Style);
	style_AddNewFontFace(self->Style, fontdesc_Bold);
    }
}
int mentertext_GetBufP(self,buf)
struct mentertext *self;
char *buf;
{
    long i,fin,elen,count;
    struct environment *newenv;
    boolean foundlast = FALSE;
    char *c;
    count = i = 0;
    fin = mentertext_GetLength(self);
    while(i < fin && count < 127){ 
	newenv = environment_GetInnerMost(Root(self), i);
	elen = environment_GetNextChange(Root(self), i);
	if(elen + i > fin) elen = (fin  /* +1 */)- i;
	if(newenv == Root(self)){
	    foundlast = TRUE;
	    self->bufp[count++] = buf;
	    mentertext_CopySubString(self,i,elen,buf,FALSE);
	    if((c = index(buf,'\n')) != NULL){
		*c = '\0';
		buf = ++c;
	    }
	    else {
		buf += elen ;
		*buf++ = '\0';
	    }
	}
	else{
	    foundlast = FALSE;
	}
	i += elen;
    }
    if(!foundlast) self->bufp[count++] = "";
    return count;
}
boolean mentertext__FinalizeObject(classID,self)
struct classheader *classID;
struct mentertext *self;
{
    free(self->buf);
    return TRUE;
}
void mentertext__updatebuf(self)
struct mentertext *self;
{
    long len = mentertext_GetLength(self) + 1;
    if(self->realbuflen < len){
	self->buf = realloc(self->buf,len);
	self->realbuflen = len;
    }
    self->buflen = len;
    self->mod = mentertext_GetModified(self);
    len--;
    if(len > 0) {
	self->bufpcount = mentertext_GetBufP(self,self->buf);
	mentertext_NotifyObservers(self,mentertext_BUFCHANGEDFLAG);
    }
}
boolean mentertext__Changed(self)
struct mentertext *self;
{
    return (boolean)(self->mod != mentertext_GetModified(self));
}
void mentertext__SetChars(self,str,len)
struct mentertext *self;
char *str;
int len;
{
    self->src = str;
    self->bufpcount = 0;
    mentertext_Clear(self);
    if(len && str && *str){
	long last , i;
	struct environment *te = NULL;
	boolean founddef;
	mentertext_InsertCharacters(self,0,str,len);
	checkstyles(self);
	founddef = FALSE;
	for(i = 0 ,last = 0; i < len; i++,str++){
	    if(*str == '@' || *str == '\n'){
		if(!founddef){
		    te = environment_InsertStyle(Root(self), last, self->Style, TRUE);
		    environment_SetStyle(te, TRUE, FALSE);
		    environment_SetLength(te, i - last);
		}
		if(*str == '@'){
		    if(!founddef){
			super_DeleteCharacters(self, i, 1);
			super_InsertCharacters(self, i, " ",1);
			environment_SetLength(te, i + 1 - last);
		    }
		    founddef = TRUE;
		}
		else founddef = FALSE;
		last = i + 1;
	    }
	}
	super_DeleteCharacters(self,len - 1,1);
    }
    mentertext_NotifyObservers(self,0);
}

boolean mentertext__InsertCharacters(self, pos, str, len)
struct mentertext *self;
long pos;
char *str;
long len;  
{
    if(Enclosed(self,pos)){
/*
    long n;
	if( (n = environment_GetNextChange(Root(self), pos)) >  100000) return;
	pos += n;
*/
	return FALSE; /* Is this the right return value? --ghoti */
    }
    if(!super_InsertCharacters(self, pos, str, len)) return FALSE;
    return TRUE;
}
boolean mentertext__DeleteCharacters(self, pos, len)
struct mentertext *self;
long pos;
long len;  {
    long n;
    if(Enclosed(self,pos) || Enclosed(self,pos + 1)) return FALSE;
    if( (n = environment_GetNextChange(Root(self), pos)) > len + pos && n < 100000) return FALSE; /* Is this the right return value? --ghoti */
    if(!super_DeleteCharacters(self, pos, len)) return FALSE;
    return TRUE;
}
void mentertext__ClearLine(self,pos)
struct mentertext *self;
long pos;
{
    long i,lasti;
    long len = mentertext_GetLength(self);
    for(i = 0 ,lasti = 0; (i = mentertext_Index(self,i,'\n',len)) != EOF; lasti =i){
	if(i >= pos) break;
    }
    i++;
    if(Enclosed(self,lasti)){
	lasti += environment_GetNextChange(Root(self), lasti);
    }
    if(lasti < i)
	mentertext_DeleteCharacters(self, i, lasti - i);
}


char *mentertext__ViewName(self)
struct mentertext *self;
{
    return ("etextview");
}
