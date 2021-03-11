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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/getrecv.c,v 2.11 1993/12/09 00:20:13 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <fontdesc.ih>
#include <rect.h>
#include <value.ih>
#include <proctbl.ih>
#include <graphic.ih>
#include <view.ih>
#include <getrecv.eh>

/****************************************************************/
/*		private functions				*/
/****************************************************************/



/****************************************************************/
/*		class procedures 				*/
/****************************************************************/




boolean GetRecV__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}




#define BADVAL -22222
/****************************************************************/
/*		instance methods				*/
/****************************************************************/
boolean GetRecV__InitializeObject(classID, self )
struct classheader *classID;
struct GetRecV * self;
{
    self->x = self->y = self ->width = self->height = 0;
    self->tmpval = NULL;
    self->lasty =self->lastx = self->firsty = self->lasty = 0;
    return TRUE;
}


void GetRecV__LookupParameters(self)
struct GetRecV * self;
{

}
#define IsEqualRect(LHS, RHS)( (LHS->left == RHS->left) && (LHS->top == RHS->top) && (LHS->width == RHS->width) && (LHS->height == RHS->height) )
#define CopyRect(LHS, RHS) rectangle_SetRectSize(LHS, rectangle_Left(RHS),rectangle_Top(RHS),rectangle_Width(RHS),rectangle_Height(RHS))
void GetRecV__DrawFromScratch(self,x,y,width,height)
struct GetRecV * self;
long x,y,width,height;
{
    struct value *w = GetRecV_Value(self);
    if(value_GetUpdateCount(w) == 0){
	/* ignore read in value */
	value_SetValue(w,0);
    }
    if (width > 0 && height > 0)
    {
	self->x = x;
	self->y = y;
	self->width = width;
	self->height = height;
	if(width > ((height * 17) / 22) ) {
	    width = ((height * 17) / 22);
	    GetRecV_SetTransferMode( self, graphic_COPY );	    
	    GetRecV_FillRectSize(self,x + width,y,self->width - width,height,GetRecV_GrayPattern(self,8,16) );
	}
	else{
	    height = ((width * 22 )/17);
	    GetRecV_SetTransferMode( self, graphic_COPY );	    
	    GetRecV_FillRectSize(self,x,y + height, width,self->height - height,GetRecV_GrayPattern(self,8,16) );
	}
	if(self->tmpval == NULL){
	    /* initialize view */
	    if(value_GetValue(w) == 0){
		/* initialize value */
		self->tmpval = (struct GetRecV_recpair *) malloc(sizeof(struct GetRecV_recpair));
		value_SetValue(w,(long) self->tmpval);
		rectangle_SetRectSize(&(self->tmpval->child),x,y,width,height);
	    }
	    else self->tmpval = (struct GetRecV_recpair *) value_GetValue(w);
	}
	else {
	    /* resize child */
	    if(IsEqualRect((&(self->tmpval->parent)),(&(self->tmpval->child))))
	       	rectangle_SetRectSize(&(self->tmpval->child),x,y,width,height);
	    else {
		long xx,yy,ww,hh ;
		float xoff,yoff;
		xoff = (float) width / rectangle_Width(&(self->tmpval->parent)); 
		yoff = (float) height / rectangle_Height(&(self->tmpval->parent));
		rectangle_GetRectSize(&(self->tmpval->child),&xx,&yy,&ww,&hh);
		rectangle_SetRectSize(&(self->tmpval->child),(long) (xoff * xx),(long) (yoff * yy), (long) (xoff * ww), (long) (yoff * hh));
	    }
	}
	rectangle_SetRectSize(&(self->tmpval->parent),x,y,width,height);
	GetRecV_SetTransferMode( self, graphic_INVERT );	    
	GetRecV_DrawRect(self,&(self->tmpval->child));
    }
}

void GetRecV__DrawDehighlight(self)
struct GetRecV * self;
{
    GetRecV_SetTransferMode( self, graphic_COPY );
    GetRecV_EraseRect( self,&(self->tmpval->parent));
    GetRecV_SetTransferMode( self, graphic_INVERT );	    
    GetRecV_DrawRect(self,&(self->tmpval->child));

}
void GetRecV__DrawHighlight(self)
struct GetRecV * self;
{
  /*
    GetRecV_SetTransferMode( self, graphic_COPY );
    GetRecV_EraseRect( self,&(self->tmpval->parent));
    GetRecV_SetTransferMode( self, graphic_INVERT );	    
    GetRecV_DrawRect(self,&(self->tmpval->child));
*/
}
void GetRecV__DrawNewValue( self )
struct GetRecV * self;
{
    GetRecV_SetTransferMode( self, graphic_COPY );
    GetRecV_EraseRect( self,&(self->tmpval->parent));
    GetRecV_SetTransferMode( self, graphic_INVERT );	    
    GetRecV_DrawRect(self,&(self->tmpval->child));

}
#define ABS(A) (((A) > 0) ? (A): -(A))
#define CREC(rec,self) rectangle_SetRectSize(&rec,MIN(self->firstx,self->lastx),MIN(self->firsty,self->lasty),ABS(self->firstx - self->lastx),ABS(self->firsty - self->lasty))
#define OutBounds(SELF,X,Y)((X  + rectangle_Left(&(SELF->tmpval->parent))> rectangle_Width(&(SELF->tmpval->parent))) || (Y + rectangle_Top(&(SELF->tmpval->parent)))> rectangle_Height(&(SELF->tmpval->parent)))
struct GetRecV * GetRecV__DoHit( self,type,x,y,hits )
struct GetRecV * self;
enum view_MouseAction type;
long x,y,hits;
{
    struct value *tt = GetRecV_Value(self);
    struct rectangle rec;
    switch(type){
	case view_LeftDown:
	    if(OutBounds(self,x,y)){
		self->firstx = -1;
		break;
	    }
	    GetRecV_SetTransferMode( self, graphic_COPY );
	    GetRecV_EraseRect( self,&(self->tmpval->parent)); 
	    GetRecV_SetTransferMode( self, graphic_INVERT );	    
	    self->lasty = self->firsty = y;
	    self->lastx = self->firstx = x;
	    break;
	case view_LeftMovement	:   
	    if( OutBounds(self,x,y)) break;
	case view_LeftUp:
            if(self->firstx == -1) break;
	    if(self->lastx != self->firstx || self->lasty != self->firsty){
		CREC(rec,self);
		GetRecV_DrawRect(self,&rec);
	    }
	    if(!OutBounds(self,x,y)){
		self->lastx = x;
		self->lasty = y;
	    }
	    if(type == view_LeftUp && self->lastx == self->firstx && self->lasty == self->firsty){
		CopyRect(&(self->tmpval->child),&(self->tmpval->parent));
		value_SetValue(tt,(long)self->tmpval);
		break;
	    }
	    CREC(rec,self);
	    GetRecV_DrawRect(self,&rec);
	    if(type ==  view_LeftMovement) break;
	    CREC((self->tmpval->child),self);
	    value_SetValue(tt,(long)self->tmpval);
	    break;
    }

    return self;
}





