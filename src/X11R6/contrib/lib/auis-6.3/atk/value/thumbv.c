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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/thumbv.c,v 2.11 1992/12/15 21:47:14 rr2b R6tape $";
#endif


 

#include <class.h>
#include <thumbv.eh>
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
#include <ctype.h>
static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;
static struct atomlist *AL_max_value;
static struct atomlist *AL_min_value;
static struct atomlist *AL_increment;
static struct atomlist *  AL_forecolor;
static struct atomlist *  AL_backcolor;
static struct atomlist *  AL_immediate;

static struct atom *  A_long;
static struct atom *  A_string;

#define InternAtoms ( \
   AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
   AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
   AL_label = atomlist_StringToAtomlist("label") ,\
   AL_max_value = atomlist_StringToAtomlist("max_value") ,\
   AL_immediate = atomlist_StringToAtomlist("immediate-update") ,\
   AL_min_value = atomlist_StringToAtomlist("min_value") ,\
   AL_increment = atomlist_StringToAtomlist("increment") ,\
   AL_forecolor = atomlist_StringToAtomlist("foreground-color")  ,\
   AL_backcolor = atomlist_StringToAtomlist("background-color") ,\
   A_long = atom_Intern("long") ,\
   A_string = atom_Intern("string") )


#define Min(X,Y) ((X) < (Y) ? (X) : (Y))
#define FUDGE 2
#define FUDGE2 4


/****************************************************************/
/*		private functions				*/
/****************************************************************/

static void CarveFonts(self)
struct thumbV * self;
{
    self->normalfont = fontdesc_Create( self->fontname, fontdesc_Plain, self->fontsize );
    self->boldfont   = fontdesc_Create( self->fontname, fontdesc_Bold,  self->fontsize );
    self->valuefont = fontdesc_Create( "values", fontdesc_Plain, 25);
    self->activefont = self->header.valueview.mouseIsOnTarget ? self->boldfont : self->normalfont;
}  
#define MAXWID 36
static void getsizes(self)
struct thumbV * self;
{
    struct FontSummary *fs;
    long labelwidth, labelheight, valheight,junk,wp,ww;
    fs = fontdesc_FontSummary(self->boldfont,thumbV_GetDrawable(self));
/*
    sprintf(buf,"%ld",self->maxval);
    fontdesc_StringSize(self->boldfont,thumbV_GetDrawable(self), buf,&(valwidth),&(junk));
    sprintf(buf,"%ld",self->minval);
    fontdesc_StringSize(self->boldfont,thumbV_GetDrawable(self), buf,&(valwidth1),&(junk));
    if(valwidth1 > valwidth) valwidth = valwidth1;
*/
    valheight = ( fs->newlineHeight == 0) ? fs->maxHeight : fs->newlineHeight;
    if(self->label){
	fontdesc_StringSize(self->boldfont,thumbV_GetDrawable(self), self->label,&(labelwidth),&(junk));
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
    rectangle_SetRectSize(&self->wheelrec,wp,valheight + FUDGE + self->y ,ww, self->height - labelheight - valheight - FUDGE2 - self->y - self->y );
    rectangle_SetRectSize(&self->labelrec,(thumbV_Width(self) - labelwidth) / 2,self->height - labelheight,labelwidth + FUDGE2,labelheight);
    rectangle_SetRectSize(&self->valrec,self->x , self->y + FUDGE ,self->width ,valheight);
    rectangle_SetRectSize(&self->inwheelrec,rectangle_Left(&self->wheelrec) + 1, rectangle_Top(&self->wheelrec) + 1,rectangle_Width(&self->wheelrec) - 2, rectangle_Height(&self->wheelrec) - 2);
}


static void DrawLabel(self)
struct thumbV * self;
{
    if(self->label){	
	thumbV_SetTransferMode( self, graphic_COPY);
	thumbV_EraseRect( self,&self->labelrec);

	thumbV_SetTransferMode( self, graphic_BLACK );
	thumbV_MoveTo( self,self->x + ( self->width / 2),self->y + self->height);
	thumbV_SetFont( self, self->activefont );
	thumbV_DrawString ( self, self->label,
				   graphic_BETWEENLEFTANDRIGHT | graphic_ATBOTTOM);
    }

}
char *thumbV__GetValueString(self)
struct thumbV * self;
{
    sprintf(self->buf,"%ld",thumbV_GetTmpVal(self));
    return self->buf;
}
static void DrawValue(self)
struct thumbV * self;
{
    char *buf;   
    buf = thumbV_GetValueString(self);
    thumbV_SetTransferMode( self, graphic_COPY);
    thumbV_EraseRect( self,&self->valrec);

    thumbV_SetTransferMode( self, graphic_BLACK );
    thumbV_MoveTo( self, self->x + self->width / 2, self->y + FUDGE);
    thumbV_SetFont( self, self->activefont );
    thumbV_DrawString ( self, buf,
				graphic_BETWEENLEFTANDRIGHT | graphic_ATTOP);

}
#define HGH 3
#define SPACE 6
static DrawKnurl(self)
struct thumbV * self;
{
#ifdef USELINES
    long y,x1,x2,end,nl, hn,change,inc,minx,maxx;
    inc =  (self->rv % SPACE);
    thumbV_SetTransferMode( self, graphic_INVERT );
    minx =  10000;
    maxx = 0 ;
    end = rectangle_Bottom(&self->inwheelrec) - 1;
    x1 = rectangle_Left(&self->inwheelrec) + FUDGE;
    x2 = rectangle_Width(&self->inwheelrec) - FUDGE2;
    y = rectangle_Top(&self->inwheelrec) + inc + 1;
    nl = (end - y) / SPACE + 1;
    hn = rectangle_Height(&self->wheelrec) / 7 ;
    if(hn < 1) hn = 1;
    nl = hn;
    change = 3;
    for(; y < end ; y += SPACE){
	if((hn -= SPACE) <0 ){ 
	    change--;
	    if(hn + nl < 0 ) change--;
	    hn = nl;
	}
	x1 += change;
	thumbV_MoveTo(self,x1,y);
	thumbV_DrawLine(self,x2,0);
#if 0
	if( inc == count++ % SPACE){
	    thumbV_MoveTo(self,x1,y + 1);
	    thumbV_DrawLine(self,x2,0);
	}
#endif /* 0 */
	if(minx > x1) minx = x1;
	if(maxx < x2 + x1) maxx = x2 + x1;
    }
    if(minx <  rectangle_Left(&self->wheelrec)) 
	rectangle_Left(&self->wheelrec) = minx;
    if(maxx > rectangle_Right(&self->wheelrec)) 
	rectangle_Width(&self->wheelrec) = maxx -  rectangle_Left(&self->wheelrec) + 3;
#else /* USELINES */
    char ch;
    thumbV_MoveTo(self,(self->width - self->x)/ 2, (self->height - self->y)/ 2);
    thumbV_SetFont(self,self->valuefont);
    ch =  (self->rv % 7)  + 'O';
    thumbV_DrawText(self,&ch,1, 0 );
#endif /* USELINES */

}

static DrawThumbwheel(self,DoAll)
struct thumbV * self;
{
    thumbV_SetTransferMode( self, graphic_COPY );
#if 0
    if(DoAll){
	thumbV_EraseRect( self,&self->wheelrec);
	thumbV_SetTransferMode( self, graphic_BLACK );
	/*	thumbV_DrawRect( self,&self->wheelrec); 
	 thumbV_MoveTo(self,rectangle_Left(&self->wheelrec),rectangle_Top(&self->wheelrec));
	 thumbV_DrawLine(self,0,rectangle_Height(&self->wheelrec));
	 */
    }
    else {
    }
#endif /* 0 */
    thumbV_EraseRect( self,&self->wheelrec);
    DrawKnurl(self);
}



/****************************************************************/
/*		class procedures 				*/
/****************************************************************/




boolean thumbV__InitializeClass(classID)
struct classheader *classID;
{
    InternAtoms;
    return TRUE;
}




#define BADVAL -22222
/****************************************************************/
/*		instance methods				*/
/****************************************************************/
boolean thumbV__InitializeObject(classID, self )
struct classheader *classID;
struct thumbV * self;
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
    self->immediatedefault = FALSE;
    self->foreground = self->background = NULL;
    return TRUE;
}


void thumbV__LookupParameters(self)
struct thumbV * self;
{
    char * fontname;
    long fontsize,diff;
    struct resourceList parameters[10];

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
    parameters[5].name = AL_increment;
    parameters[5].type = A_long;
    parameters[6].name = AL_forecolor;
    parameters[6].type = A_string;
    parameters[7].name = AL_backcolor;
    parameters[7].type = A_string;
    parameters[8].name = AL_immediate;
    parameters[8].type = A_string;
    parameters[9].name = NULL;
    parameters[9].type = NULL;

    thumbV_GetManyParameters(self, parameters, NULL, NULL);

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

    if(parameters[5].found)
	self->increment = parameters[5].data;
    else
	self->increment = 1;

    if (parameters[6].found)
	self->foreground = (char *) parameters[6].data;

    if (parameters[7].found)
	self->background = (char *) parameters[7].data;
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


    diff = self->maxval - self->minval;

    if(diff < 20) self->granular = 6;
    else if(diff < 50) self->granular = 4;
    else if(diff < 100) self->granular = 2;

    else self->granular = 0;

    if (fontsize != self->fontsize || fontname != self->fontname)
    {
	self->fontsize = fontsize;
	self->fontname = fontname;
	CarveFonts(self);
    }
}


void thumbV__DrawFromScratch(self,x,y,width,height)
struct thumbV * self;
long x,y,width,height;
{
    self->x = x;
    self->y = y;
    self->width = width;
    self->height = height;
    if(self->foreground) thumbV_SetForegroundColor(self, self->foreground, 0, 0, 0);
    if(self->background) thumbV_SetBackgroundColor(self, self->background, 0, 0, 0);
    if (width > 0 && height > 0)
    {
	struct value *w = thumbV_Value(self);
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
	DrawValue(self);	
	DrawThumbwheel(self,TRUE);
	if (self->label != NULL)
	    DrawLabel(self);
    }
}


void thumbV__DrawDehighlight(self)
struct thumbV * self;
{
    struct value *w = thumbV_Value(self);
    self->activefont = self->normalfont;
    self->tmpval = value_GetValue(w);
    DrawLabel( self );
    DrawValue(self);	
}

void thumbV__DrawHighlight(self)
struct thumbV * self;
{
    struct value *w = thumbV_Value(self);
    self->activefont = self->boldfont;
    DrawLabel( self );
    self->tmpval = value_GetValue(w);
    DrawValue(self);	
}


void thumbV__DrawNewValue( self )
struct thumbV * self;
{
    struct value *w = thumbV_Value(self);
    if(self->tmpval != value_GetValue(w)){
	self->tmpval = value_GetValue(w);
	DrawValue(self);
    }
}



struct thumbV * thumbV__DoHit( self,type,x,y,hits )
struct thumbV * self;
enum view_MouseAction type;
long x,y,hits;
{
    struct value *tt = thumbV_Value(self);
    long myval;
    static int moved;
    switch(type){
	case view_RightDown:
	case view_LeftDown:
	    self->tmpval = value_GetValue(tt);
	    self->lasty = y;
	    moved = 0;
	    break;
	case view_RightMovement:
	case view_LeftMovement	:
	    moved++;
	    if(self->granular){
		myval = self->tmpval;
		self->gran += ( self->lasty - y);
		while(self->gran > self->granular){
		    myval++;
		    self->gran -= self->granular;
		}
		while(self->gran < -self->granular){
		    myval--;
		    self->gran += self->granular;
		}
	    }
	    else myval = self->tmpval + (( self->lasty - y) * self->increment);
	    if(myval != self->tmpval){
		if(myval > self->maxval) self->tmpval = self->minval;
		else if(myval < self->minval) self->tmpval = self->maxval;
		else self->tmpval = myval;
		DrawValue(self);
		if(self->immediate)
		    value_SetValue(tt,self->tmpval);
	    }
/*	    DrawKnurl(self); */
	    self->rv += ( self->lasty - y);
	    self->lasty = y;
	    DrawThumbwheel(self,FALSE);
	    break;
	case view_RightUp:
	case view_LeftUp:
	    if(moved == 0){
		myval = (type == view_RightUp)? self->tmpval - self->increment : self->tmpval + self->increment ;
		if(myval > self->maxval) self->tmpval = self->minval;
		else if(myval < self->minval) self->tmpval = self->maxval;
		else self->tmpval = myval;
		DrawValue(self);
		self->rv += ( self->lasty - y);
		self->lasty = y;
		DrawThumbwheel(self,FALSE);
	    }
	    tt->string = thumbV_GetValueString(self);
	    value_SetValue(tt,self->tmpval);
	    break;
    }  

    return self;
}





