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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/sliderv.c,v 2.13 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#include <class.h>
#include <sliderv.eh>
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
#include <sbutton.ih>
#include <sbuttonv.ih>

#include <ctype.h>

static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;
static struct atomlist *  AL_max_value;
static struct atomlist *  AL_min_value;
static struct atomlist *  AL_forecolor;
static struct atomlist *  AL_backcolor;
static struct atomlist *  AL_readonly;
static struct atomlist *  AL_immediate;
static struct atomlist *  AL_style;

static struct atom *  A_long;
static struct atom *  A_string;

#define InternAtoms ( \
   AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
   AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
   AL_label = atomlist_StringToAtomlist("label") ,\
   AL_readonly = atomlist_StringToAtomlist("read-only") ,\
   AL_immediate = atomlist_StringToAtomlist("immediate-update") ,\
   AL_max_value = atomlist_StringToAtomlist("max_value") ,\
   AL_min_value = atomlist_StringToAtomlist("min_value") ,\
   AL_forecolor = atomlist_StringToAtomlist("shade-color") ,\
   AL_backcolor = atomlist_StringToAtomlist("color") ,\
   AL_style = atomlist_StringToAtomlist("style") ,\
   A_long = atom_Intern("long") ,\
   A_string = atom_Intern("string") )

#define rectangle_TempSet(X,Y,W,H,R) ((R)->top = (Y), (R)->left = (X), \
				      (R)->height = (H), (R)->width = (W), (R))

#define Min(X,Y) ((X) < (Y) ? (X) : (Y))
#define FUDGE 2
#define FUDGE2 4
#define SLIDERWID 20
#define STYLEOPTIONSUPPORTED 1
static void sliderV_HandleStyleString(self,s)
struct sliderV *self;
char *s;
{
    boolean go;
    go = TRUE;
    if(self->mono == -10)
	self->mono = (sliderV_DisplayClass(self) & graphic_Monochrome);

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

/****************************************************************/
/*		private functions				*/
/****************************************************************/

static void CarveFonts(self)
struct sliderV * self;
{
    self->normalfont = fontdesc_Create( self->fontname, fontdesc_Plain, self->fontsize );
    self->boldfont   = fontdesc_Create( self->fontname, fontdesc_Bold,  self->fontsize );
    self->valuefont = fontdesc_Create( "values", fontdesc_Plain, 25);
    self->activefont = self->header.valueview.mouseIsOnTarget ? self->boldfont : self->normalfont;
}  
#define MAXWID 36
#define ADDTOP 3
#define SUBHEIGHT 7
static void getsizes(self)
struct sliderV * self;
{
    struct FontSummary *fs;
    long labelwidth, labelheight, valheight,junk,wp,ww,w ,exsp,exvh;
    fs = fontdesc_FontSummary(self->boldfont,sliderV_GetDrawable(self));
#ifdef DRAWOUTSIDELINES
    exsp = FUDGE2 + FUDGE2;
    exvh = FUDGE + 4;
#else
    exsp = 0;
    exvh = FUDGE + 4;
#endif
/*
    sprintf(buf,"%ld",self->maxval);
    fontdesc_StringSize(self->boldfont,sliderV_GetDrawable(self), buf,&(valwidth),&(junk));
    sprintf(buf,"%ld",self->minval);
    fontdesc_StringSize(self->boldfont,sliderV_GetDrawable(self), buf,&(valwidth1),&(junk));
    if(valwidth1 > valwidth) valwidth = valwidth1;
*/
    valheight = ( fs->newlineHeight == 0) ? fs->maxHeight : fs->newlineHeight;
    if(self->label){
	fontdesc_StringSize(self->boldfont,sliderV_GetDrawable(self), self->label,&(labelwidth),&(junk));
	labelheight = valheight;
    }
    else{
	labelheight = labelwidth = 0;
    }
    ww = self->width - FUDGE2 - FUDGE2 - self->x - self->x;
    wp = self->x + FUDGE2;
    if(ww > MAXWID){
	ww = MAXWID;
	wp = self->x + (self->width - MAXWID) / 2;
    }
    rectangle_SetRectSize(&self->wheelrec,wp,valheight + FUDGE + self->y + ADDTOP,ww, self->height - labelheight - valheight - FUDGE2 - self->y - self->y - SUBHEIGHT);
    rectangle_SetRectSize(&self->labelrec,(sliderV_Width(self) - labelwidth) / 2,self->height - labelheight,labelwidth + FUDGE2,labelheight);
    rectangle_SetRectSize(&self->valrec,self->x + 6, self->y + exvh ,self->width -FUDGE - 12 ,valheight);
    w = self->x + (self->width / 2);
    rectangle_SetRectSize(&self->inwheelrec,w - (self->sliderwidth / 2), rectangle_Top(&self->wheelrec) + 1,self->sliderwidth, rectangle_Height(&self->wheelrec) - exsp);

}
#ifdef DRAWOUTSIDELINES

db(self,foo,fo)
struct sliderV * self;
struct rectangle *foo,*fo;
{
    sliderV_DrawRect(self,foo);   
    sliderV_MoveTo( self, fo->left, fo->top );
    sliderV_DrawLineTo( self, foo->left, foo->top);
    sliderV_MoveTo( self, fo->left + fo->width, fo->top );
    sliderV_DrawLineTo( self, foo->left + foo->width, foo->top);
    sliderV_MoveTo( self, fo->left , fo->top + fo->height);
    sliderV_DrawLineTo( self, foo->left, foo->top + foo->height);
    sliderV_MoveTo( self, fo->left + fo->width, fo->top + fo->height);
    sliderV_DrawLineTo( self, foo->left + foo->width, foo->top + foo->height);

}
static void DrawButton(self,x,y,width,height,pct,drawborder)
     struct sliderV * self;
     long x,y,width,height;
     boolean drawborder;
{
  struct rectangle foo,fo;
  long vcut,wcut;
  if(!drawborder && pct > 0)sliderV_SetClippingRect( self, rectangle_TempSet(x,y,width + 2,height + 1,&fo)  ); 
  rectangle_TempSet(x,y,width,height,&fo);
 /* fo.width--; */  fo.height--;

 if(pct > 0){
  vcut = height /pct ;
  wcut =  width /pct;
  rectangle_TempSet(x + wcut ,y + 1,width - wcut - wcut,height - 3 - vcut - vcut - vcut,&foo);
 }
  else{
      vcut = pct; wcut = pct;
      rectangle_TempSet(x + wcut ,y  - 1,width - wcut - wcut,height - 5 - vcut - vcut - vcut,&foo);
  }
  if(pct <= 0)
      sliderV_EraseRect(self,&foo);
  if(drawborder) sliderV_DrawRect(self,&fo);   
  db(self,&foo,&fo);
}
#endif /* DRAWOUTSIDELINES */

static void DrawLabel(self)
struct sliderV * self;
{
    if(self->label){	
	sliderV_SetTransferMode( self, graphic_COPY);
	sliderV_EraseRect( self,&self->labelrec);

	sliderV_SetTransferMode( self, graphic_COPY );
	sliderV_MoveTo( self,self->x + ( self->width / 2),self->y + self->height);
	sliderV_SetFont( self, self->activefont );
	sliderV_DrawString ( self, self->label,
				   graphic_BETWEENLEFTANDRIGHT | graphic_ATBOTTOM);
    }

}
char *sliderV__GetValueString(self)
struct sliderV * self;
{
    sprintf(self->buf,"%ld",sliderV_GetTmpVal(self));
    return self->buf;
}
static void DrawValue(self)
struct sliderV * self;
{
    char *buf;   
    buf = sliderV_GetValueString(self);
    sliderV_SetTransferMode( self, graphic_COPY);
    sliderV_EraseRect( self,&self->valrec);

    sliderV_SetTransferMode( self, graphic_COPY );
    sliderV_MoveTo( self, self->x + (self->width / 2), self->y + FUDGE + 4);
/*
    sliderV_MoveTo( self, self->x + (self->width / 2),self->valrec.height); */
    sliderV_SetFont( self, self->activefont );
    sliderV_DrawString ( self, buf,
				graphic_BETWEENLEFTANDRIGHT | graphic_ATTOP);

}
#define HGH 3
#define SPACE 6

static void sliderV__Drawslider(self,fullupdate,rr)
struct sliderV * self;
boolean fullupdate;
struct rectangle *rr;
{
    int start,height,st;
    struct rectangle interior,ti;
    struct sbuttonv_view_info vi;
    boolean pushed ;

    st = self->prefs->style;
    sbuttonv_SaveViewState( self, &vi);
   if(st == 4 || st == 2){
	pushed = FALSE;
	if(!fullupdate)
	    sliderV_SetClippingRect(self,&(self->lastdrawn));
	sbuttonv_DrawBorder(self, rr->left,rr->top,
			    rr->width, rr->height,
			    self->prefs,!pushed,TRUE,&interior);
	if(!fullupdate)
	    sliderV_ClearClippingRect(self);
	height =  ((self->tmpval - self->minval )* (rectangle_Height(&interior))) / (self->maxval - self->minval);
	height += (((interior.height - height) * interior.width) / interior.height);
	start = rectangle_Top(&interior) + rectangle_Height(&interior) - height;
	sbuttonv_DrawBorder(self, interior.left, start,
			    interior.width,interior.width, self->prefs,pushed,TRUE,&ti);
	rectangle_SetRectSize(&(self->lastdrawn),interior.left, start,
			      interior.width,interior.width + 4);
#ifdef DRAW_NOTCH
	sbuttonv_DrawBorder(self, ti.left,start + (interior.width / 2) -1,
			    ti.width,2, self->prefs,!pushed,TRUE,NULL);
#endif
    }
    else {
	sliderV_SetTransferMode(self, graphic_COPY);
	if(!self->mono){
	    if(self->prefs->colors[sbutton_FOREGROUND]) 
		sliderV_SetForegroundColor(self, self->prefs->colors[sbutton_FOREGROUND], 0, 0, 0);
	    if(self->prefs->colors[sbutton_BACKGROUND]) 
		sliderV_SetBackgroundColor(self, self->prefs->colors[sbutton_BACKGROUND], 0, 0, 0);
	}
	if(!fullupdate)
	    sliderV_SetClippingRect(self,&(self->lastdrawn));
	sliderV_EraseRect(self,rr);
	sliderV_DrawRect(self,rr);
	if(!fullupdate)
	    sliderV_ClearClippingRect(self);
	interior.top = rr->top + 2; interior.left = rr->left + 2;
	interior.width = rr->width -4; interior.height = rr->height - 4;
	height =  ((self->tmpval - self->minval )* (rectangle_Height(&interior))) / (self->maxval - self->minval);
	height += (((interior.height - height) * interior.width) / interior.height);
	start = rectangle_Top(&interior) + rectangle_Height(&interior) - height;
	rectangle_SetRectSize(&(self->lastdrawn),interior.left, start,
			      interior.width,interior.width);
	sliderV_DrawRect(self, &(self->lastdrawn));
	if(st == 0 || st == 3)
	    sliderV_FillRect(self, &(self->lastdrawn),sliderV_BlackPattern(self));
	else
	    sliderV_DrawRectSize(self,interior.left + 1, start + 1,
			      interior.width - 2,interior.width -2 );
	self->lastdrawn.width += 4;
	self->lastdrawn.height += 3;
    }
   sbuttonv_RestoreViewState( self, &vi);

}


/****************************************************************/
/*		class procedures 				*/
/****************************************************************/




boolean sliderV__InitializeClass(classID)
struct classheader *classID;
{
    InternAtoms;
    return TRUE;
}




#define BADVAL -22222
/****************************************************************/
/*		instance methods				*/
/****************************************************************/
boolean sliderV__InitializeObject(classID, self )
struct classheader *classID;
struct sliderV * self;
{
    self->label = NULL;
    self->fontname = NULL;
    self->fontsize = 0;
    self->maxval = 100;
    self->minval = 0;
    self->increment = 1;
    self->tmpval = BADVAL;
    self->lasty = 0;
    self->rv = 1000000;
    self->granular = self->gran =  0;
    self->doclip = FALSE;
    self->prefs = sbutton_GetNewPrefs("adew");
    if(self->prefs == NULL) return FALSE;
    sbutton_InitPrefs(self->prefs,"adew");
    self->sliderwidth = SLIDERWID;
    self->immediatedefault = FALSE;
    self->readonlydefault = FALSE;
    self->mono = -10;
    return TRUE;
}

void sliderV__FinalizeObject(classID, self )
struct classheader *classID;
struct sliderV * self;
{   
    sbutton_FreePrefs(self->prefs);
}
void sliderV__LookupParameters(self)
struct sliderV * self;
{
    char * fontname;
    long fontsize,diff;
    struct resourceList parameters[11];

    parameters[0].name = AL_label;
    parameters[0].type = A_string;
    parameters[1].name = AL_bodyfont;
    parameters[1].type = A_string;
    parameters[2].name = AL_bodyfont_size;
    parameters[2].type = A_long;
    parameters[3].name = AL_max_value;
    parameters[3].type = A_long;
    parameters[4].name = AL_min_value;
    parameters[4].type = A_long;
    parameters[5].name = AL_forecolor;
    parameters[5].type = A_string;
    parameters[6].name = AL_backcolor;
    parameters[6].type = A_string;
    parameters[7].name = AL_readonly;
    parameters[7].type = A_string;
    parameters[8].name = AL_immediate;
    parameters[8].type = A_string;
#ifdef STYLEOPTIONSUPPORTED
    parameters[9].name = AL_style;
    parameters[9].type = A_string;
    parameters[10].name = NULL;
    parameters[10].type = NULL;
#else
    parameters[9].name = NULL;
    parameters[9].type = NULL;
#endif
    if(self->mono == -10)
	self->mono = (sliderV_DisplayClass(self) & graphic_Monochrome);

    sliderV_GetManyParameters(self, parameters, NULL, NULL);

    if (parameters[0].found)
	self->label = (char *)parameters[0].data;
    else
	self->label = NULL;

    if (parameters[1].found)
	fontname = (char *)parameters[1].data;
    else
	fontname = "andytype";

    if (parameters[2].found)
	fontsize = parameters[2].data;
    else
	fontsize = 10;

    if(parameters[3].found)
	self->maxval = parameters[3].data;
    else
	self->maxval = 100;

    if(parameters[4].found)
	self->minval = parameters[4].data;
    else
	self->minval = 0;

    diff = self->maxval - self->minval;
    if(diff == 0) (self->maxval)++;
    if(diff < 20) self->granular = 6;
    else if(diff < 50) self->granular = 4;
    else if(diff < 100) self->granular = 2;

    else self->granular = 0;

    if (parameters[5].found)
	self->prefs->colors[sbutton_FOREGROUND] = (char *) parameters[5].data;

    if (parameters[6].found)
	self->prefs->colors[sbutton_BACKGROUND] = (char *) parameters[6].data;

    self->readonly = self->readonlydefault;
    if (parameters[7].found){
	char *foo;
	foo = (char *) parameters[7].data;
	while(*foo && isspace(*foo)) foo++;
	if(*foo != '\0'){
	    if(*foo == 't' || *foo == 'T' || *foo == 'y' || *foo == 'Y')
		self->readonly = TRUE;
	    else self->readonly = FALSE;
	}
    }

    self->immediate = self->immediatedefault;
    if (parameters[8].found){
	char *foo;
	foo = (char *) parameters[8].data;
	while(*foo && isspace(*foo)) foo++;
	if(*foo != '\0'){
	    if(*foo == 't' || *foo == 'T' || *foo == 'y' || *foo == 'Y')
		self->immediate = TRUE;
	    else self->immediate = FALSE;
	}
    }
#ifdef STYLEOPTIONSUPPORTED
    if (parameters[9].found)
	sliderV_HandleStyleString(self,(char *) parameters[9].data);
#else
    self->prefs->style = 4;
#endif
    if(self->prefs->style == -1) self->prefs->style = 4;
    if (fontsize != self->fontsize || fontname != self->fontname)
    {
	self->fontsize = fontsize;
	self->fontname = fontname;
	CarveFonts(self);
    }
}


void sliderV__DrawFromScratch(self,x,y,width,height)
struct sliderV * self;
long x,y,width,height;
{
    self->x = x;
    self->y = y;
    self->width = width;
    self->height = height;
    self->doclip = FALSE;

    if (width > 0 && height > 0)
    {
	struct value *w = sliderV_Value(self);
	getsizes(self);
	if(self->tmpval == BADVAL){
	    self->tmpval = value_GetValue(w);
	    if(self->tmpval < self->minval || 
	       self->tmpval > self->maxval){
		value_SetValue(w,self->minval);
		self->tmpval = self->minval;
	    }
	}
	self->activefont = self->header.valueview.mouseIsOnTarget ?
	  self->boldfont : self->normalfont;
#ifdef DRAWOUTSIDELINES
	DrawButton(self,self->x + 5,self->y + 3,self->width - 10,self->wheelrec.top + self->wheelrec.height - 3,-3,TRUE);
#endif 
	DrawValue(self);	
	sliderV_Drawslider(self,TRUE,&self->inwheelrec);
	if (self->label != NULL)
	    DrawLabel(self);
    }
}


void sliderV__DrawDehighlight(self)
struct sliderV * self;
{
    struct value *w = sliderV_Value(self);
    self->activefont = self->normalfont;
    self->tmpval = value_GetValue(w);
    DrawLabel( self );
    DrawValue(self);	
    sliderV_Drawslider(self,TRUE,&self->inwheelrec);
}

void sliderV__DrawHighlight(self)
struct sliderV * self;
{
    struct value *w = sliderV_Value(self);
    self->activefont = self->boldfont;
    DrawLabel( self );
    self->tmpval = value_GetValue(w);
    DrawValue(self);
    sliderV_Drawslider(self,TRUE,&self->inwheelrec);
}


void sliderV__DrawNewValue( self )
struct sliderV * self;
{
    struct value *w = sliderV_Value(self);
    if(self->tmpval != value_GetValue(w)){
	self->tmpval = value_GetValue(w);
	DrawValue(self);
	sliderV_Drawslider(self,TRUE,&self->inwheelrec);
    }
}



struct sliderV * sliderV__DoHit( self,type,x,y,hits )
struct sliderV * self;
enum view_MouseAction type;
long x,y,hits;
{
    struct value *tt = sliderV_Value(self);
    long myval;
    static int moved;
    if(self->readonly) return self;
    switch(type){
	case view_LeftDown:
	case view_RightDown:
	    self->tmpval = value_GetValue(tt);
	    self->lasty = y;
	    moved = 0;
	    break;
	case view_LeftMovement	:
	case view_RightMovement:
	    moved++;
	    if(self->granular){
		myval = self->tmpval;
		self->gran += (self->lasty - y);
		while(self->gran > self->granular){
		    myval++;
		    self->gran -= self->granular;
		}
		while(self->gran < -self->granular){
		    myval--;
		    self->gran += self->granular;
		}
	    }
	    else 
	    myval = self->tmpval + (( self->lasty - y) /* * self->increment */ );
	    
	    if(myval > self->maxval)  myval = self->maxval;
	    else if(myval < self->minval) myval = self->minval;
	    if(myval != self->tmpval){
		self->tmpval = myval;
		DrawValue(self);
		sliderV_Drawslider(self,FALSE,&self->inwheelrec);
	    }

	    self->lasty = y;
	    if(self->immediate)
		value_SetValue(tt,self->tmpval);

	    break;
	case view_LeftUp:
	case view_RightUp:
	    if(moved == 0){
		myval = (type == view_RightUp)? self->tmpval - self->increment : self->tmpval + self->increment ;
		if(myval <= self->maxval &&  myval >= self->minval){
		    self->tmpval = myval;
		    DrawValue(self);
		    self->lasty = y;
		    DrawValue(self);
		    sliderV_Drawslider(self,FALSE,&self->inwheelrec);
		}
	    }
	    tt->string = sliderV_GetValueString(self);
	    value_SetValue(tt,self->tmpval);
	    break;
    }  

    return self;
}





