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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/buttonv.c,v 2.18 1993/12/09 00:20:13 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <fontdesc.ih>
#include <rect.h>
#include <value.ih>
#include <buffer.ih>
#include <proctbl.ih>
#include <atom.ih>
#include <atomlist.ih>
#include <graphic.ih>
#include <rm.ih>
#include <view.ih>
#include <point.h>
#include <list.ih>
#include <sbutton.ih>
#include <sbuttonv.ih>
#include <buttonv.eh>

static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;
static struct atomlist *  AL_forecolor;
static struct atomlist *  AL_backcolor;
static struct atomlist *  AL_style;
static struct atom *  A_long;
static struct atom *  A_string;
#define InternAtoms ( \
AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
AL_label = atomlist_StringToAtomlist("label") ,\
AL_forecolor = atomlist_StringToAtomlist("foreground-color") ,\
AL_backcolor = atomlist_StringToAtomlist("background-color") ,\
AL_style = atomlist_StringToAtomlist("style") ,\
A_long = atom_Intern("long") ,\
A_string = atom_Intern("string") )
struct buttonV_rl {
    struct rectangle rect;
    char *string;
    boolean pushed;
    long key,len;
    long width;
};

#define Graphic(A) (((struct view *)(A))->drawable)
#define CLOSEBUTTON TRUE
/****************************************************************/
/*		private functions				*/
/****************************************************************

#define DEFAULTPCT 10

#define MINSIZE 3
/* Rect2 is the inner (Text) region */

#define PROMPTFONT "andysans12b"
#define FONT "andysans"
#define FONTTYPE fontdesc_Bold
#define FONTSIZE 12
#define BUTTONDEPTH 3
#define BUTTONPRESSDEPTH 2
#define TEXTPAD 2
#define buttonV_STRING_END ':'
#define buttonV_STRING_ESCAPE '\\'

static boolean clearrl(rl,self)
struct buttonV_rl *rl;
struct buttonV *self;
{
    if(rl->len > 0) free(rl->string);
    free(rl);
    return TRUE;
}
static vsetrec(rl,self)
struct buttonV_rl *rl;
struct buttonV *self;
{
    if(!self->topdown) self->rtl += -self->rhw;
    rectangle_SetRectSize(&(rl->rect),self->header.valueview.x + self->offset ,self->rtl,
		 self->bsize, self->rhw);
    if(self->topdown) self->rtl += self->rhw;
    if(--(self->bcnt) == 0){
	self->bcnt = self->count / self->columns;
	self->offset += self->bsize;
	self->rtl = self->topdown ?  self->header.valueview.y:self->header.valueview.height;
    }
    return TRUE;
}
void buttonV__HandleStyleString(self,s)
struct buttonV *self;
char *s;
{
    boolean go;
    go = TRUE;
    if(self->mono == -10)
	self->mono = (buttonV_DisplayClass(self) & graphic_Monochrome);

    if(s == NULL) return;
    while(*s != '\0'){
	switch (*s){
	    case 'c':
	    case 'C':
		if(self->mono) go = FALSE;
		else go = TRUE;
		break;
	    case 'M':
	    case 'm':
		if(self->mono) go = TRUE;
		else go = FALSE;
	    case ' ':
	    case '\t':
		break;
	    default:
		if(go && *s <= '9' && *s >= '0'){
		    self->prefs->style = *s - '0';
		    return;
		}
	}
	s++;
    }
}
static wsetrec(rl,self)
struct buttonV_rl *rl;
struct buttonV *self;
{
    rectangle_SetRectSize(&(rl->rect),self->rtl,self->header.valueview.y + self->offset,
		   self->rhw,self->bsize);
    self->rtl += self->rhw;
    if(--(self->bcnt) == 0){
	self->bcnt = self->count / self->columns;
	self->offset += self->bsize;
	self->rtl =  self->header.valueview.x;
    }
    return TRUE;
}
static calcRec(self)
struct buttonV * self;
{
    if(self->columns == 0) return;
    self->offset = 0;
    self->bcnt = self->count / self->columns;
    if(self->vertical){
    self->rtl = self->topdown ? self->header.valueview.y:self->header.valueview.height;
	self->rhw = (self->header.valueview.height * self->columns )/ self->count;
	self->bsize =  self->header.valueview.width / self->columns;
	list_Enumerate(self->list,vsetrec,self);
    }
    else {
	self->rtl =  self->header.valueview.x;
	self->rhw = (self->header.valueview.width  * self->columns )/ self->count;
	self->bsize = self->header.valueview.height / self->columns;
	list_Enumerate(self->list,wsetrec,self);
    }
}
fourwaysort(rl1,rl2)
struct buttonV_rl *rl1,*rl2;
{
   if(rl1->key == 3 && rl2->key == 2) return -1;
    else if (rl2->key == 3 && rl1->key == 2) return 1;
    else return (rl1->key - rl2->key);
}
static void buttonV__CacheSettings(self)
struct buttonV *self;
{
    char tmp[256],*t,*chr;
    struct graphic *my_graphic;
    struct buttonV_rl *rl;
    long i,j;
    long max = 0l;
    self->activefont = fontdesc_Create( self->fontname, fontdesc_Plain, self->fontsize );
    my_graphic = (struct graphic *)buttonV_GetDrawable(self);
    list_Enumerate(self->list,clearrl,self) ;
    list_Clear(self->list);
    i = 0;
    chr = self->label;
    self->mono = (buttonV_DisplayClass(self ) & graphic_Monochrome);
    do{
	j = 0;
	rl = (struct buttonV_rl *)
	  malloc(sizeof(struct buttonV_rl));
	rl->string = "";
	t = tmp;
	if(chr == NULL || *chr == '\0'){
	    if(i < 4 && self->l[i] != NULL){
		strcpy(tmp,self->l[i]);
		j = strlen(tmp);
		t = tmp + j;
	    }
	}
	else {
	    if(i + 1 == self->fixedcount){
		while(*chr != '\0'){
		    *t++ = *chr++;
		    j++;
		}
	    }
	    else {
		while(*chr != buttonV_STRING_END && *chr != '\0'){
		    if(*chr == buttonV_STRING_ESCAPE && *(chr + 1) == buttonV_STRING_END){
			chr++;
		    }
		    *t++ = *chr++;
		    j++;
		}
	    }
	}
	rl->len = j;
	*t = '\0';
	if(j > 0){
	    rl->string = (char *) malloc(j + 1);
	    strcpy(rl->string,tmp);
	}
	rl->width = fontdesc_StringSize(self->activefont,my_graphic, tmp,NULL,NULL);
	if(rl->width > max) max = rl->width;
	rl->pushed = FALSE;
	rl->key = i;
	if(self->fourway) list_InsertSorted(self->list,(char *) rl,fourwaysort);
	else 
	    list_InsertEnd(self->list,(char *)rl);
	if(self->fixedcount > 0){
	    if((i + 1 >= self->fixedcount) && ((i + 1) % self->columns == 0) ) break;
	}
	else if(chr == NULL || *chr == '\0'){
	    if((i + 1) % self->columns == 0)
		break;
	}
	if(chr != NULL && *chr != '\0')chr++;
    }while (++i < 128);
    self->current = NULL;
    self->count = i + 1;
    self->max = max;
}
void buttonV__DrawButtonText(self,text,len,rect,rect2,pushd)
struct buttonV * self;
char *text;
long len;
struct rectangle *rect,*rect2;
boolean pushd;
{
/* assumes '\0' terminated text */
    if(text != NULL && len > 0) 
	sbuttonv_DrawButtonLabel(self, text, rect2, self->prefs,pushd);
}

static void DrawButton(self,rl,left,top,width,height,borderonly,blit)
struct buttonV * self;
struct buttonV_rl *rl;
long left,top,width,height;
boolean borderonly,blit;
{
    struct rectangle Rect,*rect,in;
    char *text ;
    boolean ped ;
    if(rl == NULL) {
	rect = &Rect;
	rectangle_SetRectSize(rect,left,top,width,height);
	ped =  (self->pushed);
	text = "";
    }
    else{
	rect = &(rl->rect);
	ped = (rl->pushed);
	text = rl->string;
    }
    if(text == NULL ) text = "";
    self->prefs->font = self->activefont;
    if(self->buttontype){
	sbuttonv_DrawRectBorder(self,rect,self->prefs,ped,TRUE,&in);
	buttonV_DrawButtonText(self,text,strlen(text),rect,&in,ped);
    }
    else {
	self->si.lit = ped; 
	self->si.label = text;
	sbuttonv_DrawButton(self,&(self->si),rect); 
    }
}
static boolean enclosed(rl,x,y)
struct buttonV_rl *rl;
long x,y;
{
    if(x < rl->rect.left ||
	y < rl->rect.top ||
	x >= rl->rect.left + rl->rect.width ||
	y >= rl->rect.top + rl->rect.height)
	return FALSE;
    return TRUE;
}

static drl(rl,self)
struct buttonV_rl *rl;
struct buttonV *self;
{
    DrawButton(self,rl,0,0,0,0,FALSE,FALSE);
    return TRUE;
}
static findcurrent(rl,self)
struct buttonV_rl *rl;
struct buttonV *self;
{
    return !enclosed(rl,self->x,self->y);
}
static findkey(rl,i)
struct buttonV_rl *rl;
long i;
{
    return !(i == rl->key);
}
static DrawAllButtons(self)
struct buttonV * self;
{
    list_Enumerate(self->list,drl,self);
}

/****************************************************************/
/*		class procedures 				*/
/****************************************************************/

boolean buttonV__InitializeClass(classID)
struct classheader *classID;
{
  InternAtoms;
  return TRUE;
}

/****************************************************************/
/*		instance methods				*/
/****************************************************************/
boolean buttonV__InitializeObject(classID, self )
struct classheader *classID;
struct buttonV * self;
{
    self->label = NULL;
    self->fontname = NULL;
    self->fontsize = 0;
    self->buttontype = FALSE;
    self->pushed = FALSE;
    self->list = list_New();
    self->current = NULL;
    self->fixedcount = 1;
    self->fixedcolumns = 1;
    self->l[0] = self->l[1] = self->l[2] = self->l[3] = NULL;
    self->columns = 1;
    self->vertical = TRUE;
    self->topdown = FALSE;
    self->fourway = FALSE;
    self->si.prefs = sbutton_GetNewPrefs("adew");
    self->prefs = self->si.prefs;
    if(self->prefs == NULL) return FALSE;
    sbutton_InitPrefs(self->prefs,"adew");
    self->si.rock = NULL;
    self->si.trigger = NULL;
    self->mono = -10;
    return TRUE;
}

void buttonV__FinalizeObject(classID, self )
struct classheader *classID;
struct buttonV * self;
{
    sbutton_FreePrefs(self->prefs); 
}

void buttonV__LookupParameters(self)
     struct buttonV * self;
{
  char * fontname;
  long fontsize;
  struct resourceList parameters[7];
  parameters[0].name = AL_label;
  parameters[0].type = A_string;
  parameters[1].name = AL_bodyfont;
  parameters[1].type = A_string;
  parameters[2].name = AL_bodyfont_size;
  parameters[2].type = A_long;
  parameters[3].name = AL_forecolor;
  parameters[3].type = A_string;
  parameters[4].name = AL_backcolor;
  parameters[4].type = A_string;
  parameters[5].name = AL_style;
  parameters[5].type = A_string;
  parameters[6].name = NULL;
  parameters[6].type = NULL;
  buttonV_GetManyParameters(self, parameters, NULL, NULL);

  if (parameters[0].found)
      self->label = (char *)parameters[0].data;
  else
      self->label = "";

  if (parameters[1].found)
    fontname = (char *)parameters[1].data;
  else
    fontname = "andytype";

  if (parameters[2].found)
    fontsize = parameters[2].data;
  else
      fontsize = 10;
  if(self->mono == -10)
      self->mono = (buttonV_DisplayClass(self) & graphic_Monochrome);

 if(!self->mono){
      if (parameters[3].found)
	  self->prefs->colors[sbutton_FOREGROUND] = (char *) parameters[3].data;

      if (parameters[4].found)
	  self->prefs->colors[sbutton_BACKGROUND] = (char *) parameters[4].data;
  }
 
 if (parameters[5].found)
      buttonV_HandleStyleString(self, (char *) parameters[5].data);


  self->fontsize = fontsize;
  self->fontname = fontname;
  buttonV_CacheSettings(self);

}


void buttonV__DrawFromScratch(self,x,y,width,height)
struct buttonV * self;
long x,y,width,height;
{
    if (width > 0 && height > 0)
    {
	calcRec(self);
	DrawAllButtons(self);
	buttonV_DrawNewValue( self );
    }
}


void buttonV__DrawDehighlight(self)
struct buttonV * self;
{
}

void buttonV__DrawHighlight(self)
struct buttonV * self;
{

}


void buttonV__DrawNewValue( self )
     struct buttonV * self;
{
    long i= value_GetValue(buttonV_Value(self));
    struct buttonV_rl *rl = self->current;
    if(self->count > 1){
	if(rl == NULL ||  i  != rl->key){
	    if(rl){  
		rl->pushed = FALSE;
		DrawButton(self,rl,0,0,0,0,FALSE,TRUE);
	    }
	    rl = (struct buttonV_rl *) list_Enumerate(self->list,findkey,i);
	    if(rl){
		rl->pushed = TRUE;
		DrawButton(self,rl,0,0,0,0,FALSE,TRUE);
	    }
	    self->current = rl;
	}
    }
    self->valueset = TRUE;
}

struct buttonV * buttonV__DoHit( self,type,x,y,hits )
struct buttonV * self;
enum view_MouseAction type;
long x,y,hits;
{
    struct buttonV_rl *rl;
    long v;
    rl = self->current;
    if(self->count == 1){
	struct valueview *vself = (struct valueview *) self;
	switch(type){
	    case view_RightUp:
	    case view_LeftUp:
		if(rl){
		    rl->pushed = FALSE;
		    DrawButton(self,rl,0,0,0,0,FALSE,TRUE); 
		}
		value_SetValue(buttonV_Value(self),value_GetValue(buttonV_Value(self)) + 1);
		self->current = NULL;
		return self;
	    case view_LeftDown:
	    case view_RightDown:
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		if(rl && !vself->mouseIsOnTarget){
		    rl->pushed = FALSE;
		    DrawButton(self,rl,0,0,0,0,FALSE,TRUE); 
		    self->current = NULL;
		}
		return self;
	}
    }
    switch(type){
	case view_RightUp:
	case view_LeftUp:
	    if(rl) v = rl->key;
	    else v = -1;
	    if(value_GetValue(buttonV_Value(self)) != v)
		value_SetValue(buttonV_Value(self),v);
	    break;
	case view_LeftMovement:
	case view_RightMovement:
	case view_LeftDown:
	case view_RightDown:
	    if(rl == NULL || !enclosed(rl,x,y)){
		self->x = x;
		self->y = y;
		if(rl){  
		    rl->pushed = FALSE;
		    DrawButton(self,rl,0,0,0,0,FALSE,TRUE);
		}
		self->valueset = FALSE;
		rl = (struct buttonV_rl *) list_Enumerate(self->list,findcurrent,self);
		if(rl == NULL){
		    self->current = NULL;
		    buttonV_DrawNewValue( self );
		    break;
		}
		if(rl){
		    rl->pushed = TRUE;
		    DrawButton(self,rl,0,0,0,0,FALSE,TRUE);
		}
		self->current = rl;
	    }
	    break;
    } 
    return self;
}

struct view * buttonV__Hit(self, type, x, y, numberOfClicks)
     struct buttonV * self;
     enum view_MouseAction type;
     long x, y, numberOfClicks;
     {/* should probably just restore this functionality to valueview,
	with a way to optionly set it */
	 register short sendEvent;
	 struct valueview *vself = (struct valueview *) self;
	 if(((struct valueview *) self)->HasInputFocus == FALSE)
	     buttonV_WantInputFocus(self,self);

	 if (vself->active)
	 {
	     switch (type)
	     {
		 case view_NoMouseEvent:
		     sendEvent = FALSE;
		     break;
		 case view_LeftDown:
		     vself->mouseState = valueview_LeftIsDown;
		     buttonV_Highlight(self);
		     sendEvent = TRUE;
		     self->movedoff = FALSE;
		     break;
		 case view_RightDown:
		     vself->mouseState = valueview_RightIsDown;
		     buttonV_Highlight(self);
		     sendEvent = TRUE;
		     self->movedoff = FALSE;
		     break;
		 case view_LeftUp:
		 case view_RightUp:
		     if(self->movedoff) sendEvent = FALSE;
		     else if (vself->mouseIsOnTarget)
		     {
			 buttonV_Dehighlight(self);
			 sendEvent = TRUE;
		     }
		     else sendEvent = TRUE;
		     break;
		 case view_LeftMovement:
		 case view_RightMovement:
		     if(self->movedoff) sendEvent = FALSE;
		     else  if(buttonV_OnTarget(self,x,y))
		     {
			 buttonV_Highlight(self);
			 sendEvent = TRUE;
		     }
		     else
		     {
			 buttonV_Dehighlight(self);
			/* self->movedoff = TRUE; */
			 if(self->valueset) sendEvent = FALSE;
			 else 
			     sendEvent = TRUE;

		     }
		     break;
	     }
	 }
	 else
	     sendEvent = FALSE;
	 if (sendEvent){
	     return (struct view *) buttonV_DoHit(self, type, x, y, numberOfClicks);
	 }
	 else
	     return (struct view *) self;
     }
