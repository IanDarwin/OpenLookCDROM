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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrstrv.c,v 2.11 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 
#include <class.h>
#include <lpair.ih>
#include <entrtext.ih>
#include <etextv.ih>
#include <observe.ih>
#include <value.ih>
#include <valuev.ih>
#include <sbuttonv.ih>
#include <buttonv.ih>
#include <entrstrv.eh>



struct buttonV *enterstrV__DoHit( self,type,x,y,hits )
     struct enterstrV * self;
     enum view_MouseAction type;
     long x,y,hits;
{
  if (self->etext &&(type == view_RightUp || type == view_LeftUp))
      entertext_updatebuf(self->etext);
  return (struct buttonV *)self;
}
void enterstrV__ObservedChanged(self,changed,value)
struct enterstrV *self;
struct observable *changed;
long value;
{
    struct value *val ;
    char *str,*os;

    val = enterstrV_Value(self);
    if( changed == (struct observable *) self->etext){
	if(value == observable_OBJECTDESTROYED){
	    self->etext = NULL;
	    value_SetString(val,NULL);
	}
	else if(value == entertext_BUFCHANGEDFLAG && self->etext){
	    char *buf = entertext_GetString(self->etext);
	    struct value *val = enterstrV_Value(self);
	    value_SetNotify(val,FALSE);
	    value_SetValue(val,atoi(buf));
	    value_SetNotify(val,TRUE);
	    value_SetString(val,buf);
	}
    }
    else if(self->etext){
	if(val != (struct value *)self->header.view.dataobject){
	    /* ERROR */
	    fflush(stdout);
	    val = (struct value *)self->header.view.dataobject;
	}
	str = value_GetString(val);
	os = entertext_GetString(self->etext);
	if(str == NULL) str = "";
	if(os == NULL) os = "";
	if(str != os ){
	    if(strcmp(str,os) != 0){
		entertext_SetChars(self->etext,str,strlen(str));
		entertext_updatebuf(self->etext);
		return;
	    }
	}
	super_ObservedChanged(self,changed,value);
    }
}
void enterstrV__DrawButtonText(self,text,len,rect,rect2,pushd)
struct enterstrV * self;
char *text;
long len;
struct rectangle *rect,*rect2;
boolean pushd;
{
    struct rectangle r,r2;
    struct buttonV *ss;
    ss = (struct buttonV *) self;
    r = *rect;
    r2 = *rect2;
    if(text && len > 0){
	r.height  = r.height / 2;
	r2.height = r2.height / 2;
	super_DrawButtonText(self,text,len,&r,&r2,pushd);
	r2.top = r2.top + r2.height;
    }
    
    sbuttonv_DrawRectBorder(self,&r2,ss->prefs,TRUE,TRUE,&r);
    etextview_InsertView(self->etextview, self, &r);
    enterstrV_RetractViewCursors(self, self->etextview);
    etextview_FullUpdate(self->etextview,view_FullRedraw, 0, 0, 0, 0);
}

boolean enterstrV__InitializeObject(classID,self)
struct classheader *classID;
struct enterstrV *self;
{
    self->etext = NULL;
    self->etextview = NULL;
    if((self->etext = entertext_New()) == NULL) return FALSE;
    if((self->etextview  = etextview_New()) == NULL) return FALSE;
    etextview_SetDataObject(self->etextview,self->etext);
    entertext_AddObserver(self->etext,self);
    etextview_SetValueview(self->etextview,(struct valueview *)self);
    etextview_LinkTree(self->etextview,self);
    ((struct buttonV *)self)->buttontype = TRUE;
    return TRUE;
}
boolean enterstrV__FinalizeObject(classID,self)
struct classheader *classID;
struct enterstrV *self;
{
    if(self->etext)
	entertext_RemoveObserver(self->etext,self);
    return TRUE;
}
void enterstrV__LinkTree(self,parent)
struct enterstrV *self;
{
    super_LinkTree(self,parent);
    etextview_LinkTree(self->etextview,self);
}
struct view * enterstrV__Hit(self, type, x, y, numberOfClicks)
struct enterstrV * self;
enum view_MouseAction type;
long x, y, numberOfClicks;
{
    if(self->etextview) 
	etextview_WantInputFocus(self->etextview,self->etextview);

    return etextview_Hit(self->etextview, type, etextview_EnclosedXToLocalX(self->etextview, x), etextview_EnclosedYToLocalY(self->etextview, y), numberOfClicks);
}
