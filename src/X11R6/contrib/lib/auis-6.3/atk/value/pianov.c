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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/pianov.c,v 2.10 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#include <class.h>
#include <pianov.eh>
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
static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;

static struct atom *  A_long;
static struct atom *  A_string;

#define InternAtoms ( \
   AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
   AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
   AL_label = atomlist_StringToAtomlist("':' separated labels") ,\
   A_long = atom_Intern("long") ,\
   A_string = atom_Intern("string") )


#define Min(X,Y) ((X) < (Y) ? (X) : (Y))
#define FUDGE 2
#define FUDGE2 4


/****************************************************************/
/*		private functions				*/
/****************************************************************/

static void CarveFonts(self)
struct pianoV * self;
{
    self->normalfont = fontdesc_Create( self->fontname, fontdesc_Plain, self->fontsize );
    self->boldfont   = fontdesc_Create( self->fontname, fontdesc_Bold,  self->fontsize );
    self->valuefont = fontdesc_Create( "values", fontdesc_Plain, 25);
    self->activefont = self->header.valueview.mouseIsOnTarget ? self->boldfont : self->normalfont;
}  
static int masks[] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096 };
static int wn[] = { 1,3,5,6,8,10,12,-1};
static int bn[] = { 2,4,0,7,9,11,-1, 0 };
locateHit(self,x,y)
struct pianoV * self;
int x,y;
{
    int hy,*ip;
    float wid,place;
    hy = (self->height + self->y) / 2;
    if(y < hy){ /* posible black note */
	wid = self->width / 21.0;
	for(place = self->x + wid + wid,ip = bn; x > place; ip++,place += wid){
	    place += wid + wid;
	    if(x < place){
		if(*ip> 0 && x > self->x + wid + wid) return (*ip);
		break;
	    }
	}
    }
    wid = self->width / 7.0 ;
    for(place = self->x + wid,ip = wn; x > place && *ip != 12; ip++,place += wid) ;
    return(*ip);
}

parselabels(self,chr)
struct pianoV * self;
char *chr;
{ 
    int i,j;
    for(i = 0 ; i < pianov_NUMLABELS; i++){
	self->label[i] = NULL;
    }
    if(chr == NULL) return;
    i = 0;
    do{
	j = 0;
	self->label[i] = chr;
	while(*chr != ':' && *chr != '\0'){
	    chr++;
	    j++;
	}
	self->lsize[i] = j;
	if(*chr == '\0') break;
	chr++;
    }while (++i < pianov_NUMLABELS);
}
#define NoteOn(self,i) (self->tmpval & masks[i])
#define LabelChanged(self,i) ((self->tmpval & masks[i]) != (self->lastval & masks[i]))
static Drawpiano(self,full)
struct pianoV * self;
boolean full;
{
    int hy,*ip;
    float wid,place;
    if(full){
    pianoV_SetTransferMode( self, graphic_COPY );

    pianoV_EraseRectSize( self,self->x,self->y,self->width,self->height);
    }
    pianoV_SetTransferMode( self, graphic_BLACK );
    if(full) pianoV_DrawRectSize(self,self->x,self->y,self->width,self->height);
    hy = (self->height + self->y) / 2;
    wid = self->width / 21.0;
    for(place = self->x + wid + wid,ip = bn; *ip != -1 ; ip++,place += wid){
	if(*ip){
	    if(full){
		pianoV_MoveTo(self,(int) place,self->y);
		pianoV_DrawLineTo(self,(int) place,hy);
	    }
	    place += wid + wid;
	    if(full){
		pianoV_DrawLineTo(self,(int) place,hy);
		pianoV_DrawLineTo(self,(int) place,self->y);
	    }
	    if(self->label[*ip] && (full ||  LabelChanged(self,*ip))){
		if(!full){
		    pianoV_SetTransferMode( self, graphic_COPY );
		    pianoV_EraseRectSize( self,(int)(place + 1 - wid - wid),self->y + 1,(int)(wid + wid - 2),(int)(hy - 2));
		    pianoV_SetTransferMode( self, graphic_BLACK );
		}
		pianoV_MoveTo(self,(int)(place - wid), self->y + (hy / 2));
		pianoV_SetFont(self,NoteOn(self,*ip)? self->boldfont : self->normalfont);
		pianoV_DrawText(self,self->label[*ip], self->lsize[*ip],
				      graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM );
	    }

	}
	else 	    place += wid + wid;

    }
    wid = self->width / 7.0 ;
    for(place = self->x + wid,ip = wn;*ip != -1; ip++,place += wid) {
	if(full){
	    pianoV_MoveTo(self,(int) place,self->y + self->height);
	    if(*ip != 5)
		pianoV_DrawLineTo(self,(int) place,hy);
	    else 
		pianoV_DrawLineTo(self,(int) place,self->y);
	}
	if(self->label[*ip]  && (full ||  LabelChanged(self,*ip))){
	    if(!full){
		pianoV_SetTransferMode( self, graphic_COPY );
		pianoV_EraseRectSize( self,(int)(place - wid + 1),(int)(hy + 1),(int)(wid - 2),(int)hy - 2);
		pianoV_SetTransferMode( self, graphic_BLACK );
	    }
	    pianoV_MoveTo(self,(int)(place - (wid / 2)), self->y + hy +(hy / 2));
	    pianoV_SetFont(self,NoteOn(self,*ip)? self->boldfont : self->normalfont);
	    pianoV_DrawText(self,self->label[*ip], self->lsize[*ip],
				  graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM );
	}
    }
    self->lastval = self->tmpval;
}

/****************************************************************/
/*		class procedures 				*/
/****************************************************************/




boolean pianoV__InitializeClass(classID)
struct classheader *classID;
{
    InternAtoms;
    return TRUE;
}




#define BADVAL -22222
/****************************************************************/
/*		instance methods				*/
/****************************************************************/
boolean pianoV__InitializeObject(classID, self )
struct classheader *classID;
struct pianoV * self;
{   
    int i;
    for(i = 0 ; i < pianov_NUMLABELS; i++)
	self->label[i] = NULL;
    self->fontname = NULL;
    self->fontsize = 0;
    self->tmpval = self->lastval =0;
    return TRUE;
}


void pianoV__LookupParameters(self)
struct pianoV * self;
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
    parameters[3].name = NULL;
    parameters[3].type = NULL;

    pianoV_GetManyParameters(self, parameters, NULL, NULL);

    if (parameters[0].found)
	parselabels(self,(char *)parameters[0].data);
    else
	parselabels(self,NULL);

    if (parameters[1].found)
	fontname = (char *)parameters[1].data;
    else
	fontname = "andytype";

    if (parameters[2].found)
	fontsize = parameters[2].data;
    else
	fontsize = 10;

    if (fontsize != self->fontsize || fontname != self->fontname)
    {
	self->fontsize = fontsize;
	self->fontname = fontname;
	CarveFonts(self);
    }
}


void pianoV__DrawFromScratch(self,x,y,width,height)
struct pianoV * self;
long x,y,width,height;
{
    self->x = x;
    self->y = y;
    self->width = width;
    self->height = height;
    if (width > 0 && height > 0)
    {
	struct value *w = pianoV_Value(self);
	if(self->tmpval == BADVAL){
	    self->tmpval = value_GetValue(w);
	}
	self->activefont = self->header.valueview.mouseIsOnTarget ?
	  self->boldfont : self->normalfont;
	Drawpiano(self,TRUE);	
    }
}


void pianoV__DrawDehighlight(self)
struct pianoV * self;
{

    struct value *w = pianoV_Value(self);
    self->activefont = self->normalfont;
    self->tmpval = value_GetValue(w);
    Drawpiano(self,FALSE);	

}

void pianoV__DrawHighlight(self)
struct pianoV * self;
{
/*
    struct value *w = pianoV_Value(self);
    self->activefont = self->boldfont;
    self->tmpval = value_GetValue(w);
    Drawpiano(self);	
*/
}


void pianoV__DrawNewValue( self )
struct pianoV * self;
{
    struct value *w = pianoV_Value(self);
    self->tmpval = value_GetValue(w);
    Drawpiano(self,FALSE);	
}

#define flipbit(A,B) ((A & B)? (A & ~B) : (A | B))

struct pianoV * pianoV__DoHit( self,type,x,y,hits )
struct pianoV * self;
enum view_MouseAction type;
long x,y,hits;
{
    struct value *tt = pianoV_Value(self);
    register int tmp,v,m;
    switch(type){
	case view_LeftDown:
	    v = value_GetValue(tt);
	    m = masks[locateHit(self,x,y)];
	    self->tmpval =  flipbit(v,m);
#ifdef DEBUG
printf("m = %d, self->tmpval = %d, v = %d\n",m,self->tmpval,v);
#endif /* DEBUG */
	    Drawpiano(self,FALSE);
	    break;
	case view_LeftMovement:
	    v = value_GetValue(tt);
	    m = masks[locateHit(self,x,y)];
	    tmp =  flipbit(v,m);
	    if(self->tmpval != tmp){
		self->tmpval = tmp;
		Drawpiano(self,FALSE);
	    }
	    break;
	case view_LeftUp:
	    value_SetValue(tt,self->tmpval);
	    break;
    }  

    return self;
}





