/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrintv.c,v 1.1 1993/08/20 20:05:27 susan Exp $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrintv.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrintv.c,v 1.1 1993/08/20 20:05:27 susan Exp $ ";
#endif /* lint */
#include <class.h>
#include <lpair.ih>
#include <entrint.ih>
#include <eintv.ih>
#include <observe.ih>
#include <value.ih>
#include <valuev.ih>
#include <sbuttonv.ih>
#include <buttonv.ih>
#include <entrintv.eh>



struct buttonV *enterintV__DoHit( self,type,x,y,hits )
     struct enterintV * self;
     enum view_MouseAction type;
     long x,y,hits;
{
  if (self->eint &&(type == view_RightUp || type == view_LeftUp))
      enterint_updatebuf(self->eint);
  return (struct buttonV *)self;
}
void enterintV__ObservedChanged(self,changed,value)
struct enterintV *self;
struct observable *changed;
long value;
{
    struct value *val ;
    char *str,*os;

    val = enterintV_Value(self);
    if( changed == (struct observable *) self->eint){
	if(value == observable_OBJECTDESTROYED){
	    self->eint = NULL;
	    value_SetString(val,NULL);
	}
	else if(value == enterint_BUFCHANGEDFLAG && self->eint){
	    char *buf = enterint_GetString(self->eint);
	    struct value *val = enterintV_Value(self);
	    value_SetNotify(val,FALSE);
	    value_SetValue(val,atoi(buf));
	    value_SetNotify(val,TRUE);
	    value_SetString(val,buf);
	}
    }
    else if(self->eint){
	if(val != (struct value *)self->header.view.dataobject){
	    /* ERROR */
	    fflush(stdout);
	    val = (struct value *)self->header.view.dataobject;
	}
	str = value_GetString(val);
	os = enterint_GetString(self->eint);
	if(str == NULL) str = "";
	if(os == NULL) os = "";
	if(str != os ){
	    if(strcmp(str,os) != 0){
		enterint_SetChars(self->eint,str,strlen(str));
		enterint_updatebuf(self->eint);
		return;
	    }
	}
	super_ObservedChanged(self,changed,value);
    }
}
void enterintV__DrawButtonText(self,text,len,rect,rect2,pushd)
struct enterintV * self;
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
	r.width  = r.width / 2;
	r2.width = r2.width / 2;
	super_DrawButtonText(self,text,len,&r,&r2,pushd);
	r2.left = r2.left + r2.width;
    }
    
    sbuttonv_DrawRectBorder(self,&r2,ss->prefs,TRUE,TRUE,&r);
    eintview_InsertView(self->eintview, self, &r);
    enterintV_RetractViewCursors(self, self->eintview);
    eintview_FullUpdate(self->eintview,view_FullRedraw, 0, 0, 0, 0);
}

boolean enterintV__InitializeObject(classID,self)
struct classheader *classID;
struct enterintV *self;
{
    self->eint = NULL;
    self->eintview = NULL;
    if((self->eint = enterint_New()) == NULL) return FALSE;
    if((self->eintview  = eintview_New()) == NULL) return FALSE;
    eintview_SetDataObject(self->eintview,self->eint);
    enterint_AddObserver(self->eint,self);
    eintview_SetValueview(self->eintview,(struct valueview *)self);
    eintview_LinkTree(self->eintview,self);
    ((struct buttonV *)self)->buttontype = TRUE;
    return TRUE;
}
boolean enterintV__FinalizeObject(classID,self)
struct classheader *classID;
struct enterintV *self;
{
    if(self->eint)
	enterint_RemoveObserver(self->eint,self);
    return TRUE;
}
void enterintV__LinkTree(self,parent)
struct enterintV *self;
{
    super_LinkTree(self,parent);
    eintview_LinkTree(self->eintview,self);
}
struct view * enterintV__Hit(self, type, x, y, numberOfClicks)
struct enterintV * self;
enum view_MouseAction type;
long x, y, numberOfClicks;
{
    if(self->eintview) 
	eintview_WantInputFocus(self->eintview,self->eintview);

    return eintview_Hit(self->eintview, type, eintview_EnclosedXToLocalX(self->eintview, x), eintview_EnclosedYToLocalY(self->eintview, y), numberOfClicks);
}
