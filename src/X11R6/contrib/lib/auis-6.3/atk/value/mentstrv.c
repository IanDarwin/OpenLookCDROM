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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/mentstrv.c,v 2.11 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 
#include <class.h>
#include <lpair.ih>
#include <menttext.ih>
#include <metextv.ih>
#include <observe.ih>
#include <value.ih>
#include <valuev.ih>

#include <mentstrv.eh>



struct buttonV *menterstrV__DoHit( self,type,x,y,hits )
     struct menterstrV * self;
     enum view_MouseAction type;
     long x,y,hits;
{
  if (self->etext &&(type == view_RightUp || type == view_LeftUp))
      mentertext_updatebuf(self->etext);
  return (struct buttonV *)self;
}
void menterstrV__ObservedChanged(self,changed,value)
struct menterstrV *self;
struct observable *changed;
long value;
{
    struct value *val ;
    char *str,*os;
    val = menterstrV_Value(self);
    if( changed == (struct observable *) self->etext){
	if(value == observable_OBJECTDESTROYED){
	    self->etext = NULL;
	    value_SetString(val,NULL);
	}
	else if(value == mentertext_BUFCHANGEDFLAG){
	    long size;
	    char **buf = mentertext_GetStringArray(self->etext);
	    struct value *val = menterstrV_Value(self);

	    size = mentertext_GetArraySize(self->etext);
	    if(size == 0) return;
	    value_SetNotify(val,FALSE);
	    value_SetArraySize(val,size);
	    value_SetNotify(val,TRUE);
	    value_SetStringArray(val,buf);
	}
    }
    else {
	if(val != (struct value *)self->header.view.dataobject){
	    /* ERROR */
	    fflush(stdout);
	    val = (struct value *)self->header.view.dataobject;
	}
	str = value_GetString(val);
	os = mentertext_GetSrcString(self->etext);
	if(str == NULL) {
	    if(os != NULL) mentertext_SetChars(self->etext,NULL,0);
	}
	else if(os == NULL || strcmp(os,str) != 0){
	    mentertext_SetChars(self->etext,str,strlen(str));
	}
    }
}
struct view *menterstrV__GetApplicationLayer(self)
struct menterstrV *self;
{
    struct lpair *lp;	
    struct metextview *ev;
    long w,h;
    if((self->etext = mentertext_New()) == NULL) return (struct view *)self;
    if((ev = metextview_New()) == NULL) return (struct view *)self;
    metextview_SetDataObject(ev,self->etext);
    mentertext_AddObserver(self->etext,self);
    self->etextview = ev;
    h = 40;
    if(((struct view *)self)->parent != NULL){
	/* can't call desired size on unlinked text */
	metextview_LinkTree(ev,self);
	metextview_DesiredSize(ev,500,500,0,&w,&h);
	metextview_UnlinkTree(ev);
    }
    lp = lpair_Create(metextview_GetApplicationLayer(ev),self,h);
    metextview_SetValueview(ev,(struct valueview *)self);
    return (struct view *)lp;
}
boolean menterstrV__InitializeObject(classID,self)
struct classheader *classID;
struct menterstrV *self;
{
    self->etext = NULL;
    self->etextview = NULL;
    return TRUE;
}
boolean menterstrV__FinalizeObject(classID,self)
struct classheader *classID;
struct menterstrV *self;
{
    if(self->etext)
	mentertext_RemoveObserver(self->etext,self);
    return TRUE;
}
void menterstrV__WantInputFocus(self)
struct menterstrV *self;
{
    if(self->etextview) 
	metextview_WantInputFocus(self->etextview,self->etextview);
}
