/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/fnotev.c,v 1.12 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <text.ih>
#include <textv.ih>
#include <view.ih>
#include <im.ih>
#include <rect.h>
#include <fontdesc.ih>
#include <cursor.ih>
#include <message.ih>
#include <ctype.h>
#include <environ.ih>
#include <fnote.ih>
#include <envrment.ih>
#include <txttroff.ih>
#include <fnotev.eh>

#define FONTNAME "andy"
#define FONTSIZE 16
#define OFNAME "andy"
#define OFSIZE 12
#define SHIM 3
#define DataObject(A) (A->header.view.dataobject)
#define Text(A) ((struct text *) DataObject(A))
#define Fnote(A) ((struct fnote *) DataObject(A))
#define Graphic(A) (((struct view *)(A))->drawable)
#define CANTSWITCHTEXTVIEW 1 /* textview currently doesn't allow itself to switch to a new text */
struct impair {
    struct im *host,*fn;
    struct fnotev *cur;
    struct textview *textview;
    struct view *app;
    struct impair *next;
};
static struct impair *list;
static endnotes = FALSE;
void initci(self)
struct fnotev *self;
{
    if(fnotev_GetDrawable(self) != NULL){
	fontdesc_CharSummary(self->fd,fnotev_GetDrawable(self),*(self->displaystr),&(self->ci[0]));
	fontdesc_CharSummary(self->ofd,fnotev_GetDrawable(self),*(self->displaystr),&(self->ci[1]));
    }
}

void fnotev__GetOrigin(self, width, height, originX, originY)
    struct fnotev *self;
    long width;
    long height;
    long *originX;
    long *originY;
{
    *originX = 0;
    *originY = height - 3;
}
static int clevel = -100;
void fnotev__Print(self, f, process, final, toplevel)
    struct fnotev *self;
    FILE *f;
    char *process;
    char *final;
    int toplevel;
{
	struct text *d;
	long i,doclen,ln,cs,c,addNewLine;
	if(endnotes){
/*
	    fn = Fnote(self);
	    fprintf(f,"\\v'-.3v'\\\\*(Fn\\s-3 %d\\s0\\fP\\v'.3v'",
		    fn->notecount);
*/
	    if(text_GetLength(Text(self)) > 0)
		fprintf(f, "\\**\n");	/* automatic footnote counter string */
	    return;
	}
	addNewLine = 0;
	d = Text(self);
	doclen = text_GetLength(d);
	if(doclen == 0) return;
	fprintf(f, "\\**\n");	/* automatic footnote counter string */
	fprintf(f,".FS\n"); 
	if(clevel == toplevel){
	for(i = 0,ln = 0,cs = 0; i < doclen;i++) {
	    if ((c = text_GetChar(d, i)) == '\n') {
		if(cs++ == 0 && ln == 0) fputc('\n',f);	/* count line Spacing */
		ln = 0;
		fputc('\n',f);
		continue;
	    }
	    else cs = 0;
	    if (ln++ > 80 && (c == ' ' || c == '\t')) {
		/* Don't let lines get too long */
		addNewLine++;
	    } else if (addNewLine) {
		/* Add the newline before the first */
		/* non-blank character (if still needed) */
		if (ln > 80) {
		    fputc('\n', f);
		    ln = 0;
		    addNewLine = 0;
		}
	    }
	    if ((c == '\\') || (ln == 0 && (c == '\'' || c == '.'))) {
		/* quote special characters */
		fputc('\\', f);
		if (c == '.') {
		    fputc('&', f);
		    ln++;
		}
		ln++;
	    }
	    if(!(isprint(c) || isspace(c))) continue;
	    fputc(c, f);
	}
	if(ln > 0) fputc('\n',f);
	}
	else{
	    clevel = toplevel;
	    texttroff_WriteSomeTroff( self, d, f, toplevel, 0);
	}
	fprintf(f,".FE\n");
	clevel = -100;
    }
struct view *fnotev__Hit(self,action,mousex,mousey,numberOfClicks) 
struct fnotev *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{

    if(action == view_LeftUp || action == view_RightUp){
/*	fnotev_WantInputFocus(self,self); */
#if 0
	if(self->fnotetype == -1)
	    fnotev_pushchild(self);
	else{
	    char *ib[2048],*ob[2048];
	    int len;
	    len = text_GetLength(Text(self));
	    if(len > 2047) len = 2047;
	    text_CopySubString(Text(self),0,len,ib,FALSE);
	    if(numberOfClicks == 1 || len == 0 || text_GetReadOnly(Text(self)))
		message_DisplayString(NULL,self->fnotetype,ib);
	    else{
		if(message_AskForString(NULL,self->fnotetype,"new footnote? ",ib,ob,2047) >= 0) 
		    text_ReplaceCharacters(Text(self),0,len,ob,strlen(ob));
	    }
	}
#else 
	struct fnote *fn = Fnote(self);
	if(fnote_IsOpen(fn))
	    fnote_Close(fn,self->parenttext);
	else 
	    fnote_Open(fn,self->parenttext);
	textview_SetDotPosition(self->parentview,fnote_GetLoc(fn) + 1);
	textview_SetDotLength(self->parentview,fnote_GetLocLength(fn));
	fnote_NotifyObservers(fn,0);

#endif
    }	
    return (struct view *) self;
}
#if 0
fnotev_FindLoc(self,parent)
struct fnotev *self;
struct text *parent;
{
    struct text *d,*parent;
    strcut textv *pv;
    
    d = Text(self);
    
    
}
#endif
enum view_DSattributes fnotev__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct fnotev *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;
{
/*    *desiredwidth = 17;
    *desiredheight = 14; */
    if(self->ci[0].width == 0) initci(self);

    if(self->ci[0].width != 0)	*desiredwidth = self->ci[0].width + SHIM /*+ SHIM */;
    else *desiredwidth = 13;
    if(self->ci[0].height != 0) *desiredheight = self->ci[0].height  + SHIM + 1/* + SHIM */;
    else *desiredheight = 11;
    return(view_HeightFlexible | view_WidthFlexible);
}
static struct impair *findwindow(self,pc)
struct fnotev *self;
struct text *pc;
{
    struct im *m;
    struct impair *cim;
#ifdef CANTSWITCHTEXTVIEW
    struct view *oldview,*oldapp;
#endif
    m = fnotev_GetIM(self);
    cim = self->imp;
    if(cim == NULL){
	for(cim = list; cim != NULL; cim = cim->next){
	    if(cim->host == m) break;
	}
	if(cim == NULL) {
	    cim = (struct impair *) malloc(sizeof( struct impair));
	/* these structures are shared and never freed, but few are ever created */
	    if(cim == NULL) return NULL;
	    cim->next = list;
	    list = cim;
	    cim->host = m;
	    self->imp = cim;
	    cim->textview = NULL;
	    cim->app = NULL;
	    cim->fn = NULL; 
	}
    }
    if(cim->fn == NULL){
	if((cim->fn = im_Create(NULL)) == NULL) return NULL;
	im_SetTitle(cim->fn,"Footnotes");
    }
    else {	
	if(cim->cur == self) return cim;
	if(cim->cur){
	    im_RemoveObserver(cim->fn,cim->cur);
	}
    }
#ifdef CANTSWITCHTEXTVIEW
    oldview = (struct view *) cim->textview;
    oldapp = cim->app;
    cim->textview = NULL;
#endif
    if(cim->textview == NULL){
	if((cim->textview = textview_New()) == NULL) return NULL;
	cim->app = textview_GetApplicationLayer(cim->textview);
	im_SetView(cim->fn, cim->app);
    }
    cim->cur = self;
    im_AddObserver(cim->fn,self);
    textview_SetDataObject(cim->textview,pc);
    textview_WantInputFocus(cim->textview,cim->textview);
#ifdef CANTSWITCHTEXTVIEW
    if(oldview){
	if(oldview != oldapp)
	    view_DeleteApplicationLayer(oldview,oldapp);
	view_Destroy(oldview);
    }
#else 
    text_NotifyObservers(pc,0);
    textview_SetDotPosition(cim->textview,0);
    textview_SetDotLength(cim->textview,0);
    cim->textview->force = TRUE; /* bogus, shouldn't be necessary */
    textview_FrameDot(cim->textview,0);
    textview_WantUpdate(cim->textview,cim->textview);
#endif
}
void fnotev__pushchild(self)
struct fnotev *self;
{
    struct text *pc = Text(self);
    if(pc ){
	findwindow(self,pc); 
    }
}
void fnotev__popchild(self)
struct fnotev *self;
{
}

static DoUpdate(self,full)
struct fnotev *self;
boolean full;
{
    struct rectangle enclosingRect;
/*     long xsize,ysize; 
    struct point pt[5]; */
/*
    enclosingRect.top = 0; enclosingRect.left = 0;
    enclosingRect.width  = fnotev_GetVisualWidth(self) -2 ;
    enclosingRect.height = fnotev_GetVisualHeight(self) -2 ;
*/
    if(self->ci[0].width == 0) initci(self);
    fnotev_GetVisualBounds(self,&enclosingRect);
    enclosingRect.width--; enclosingRect.height--; 
    enclosingRect.width--; enclosingRect.height--; 
    fnotev_SetTransferMode(self,graphic_WHITE);
    fnotev_EraseRect(self,&(enclosingRect));
    fnotev_SetTransferMode(self,graphic_COPY);
  /*  fnotev_FillRect(self,&(enclosingRect), fnotev_GrayPattern(self,4,16) );*/
    fnotev_PostCursor(self,&(enclosingRect),self->cursor) ;

#if 0
    enclosingRect.left = enclosingRect.width / 3;
    enclosingRect.top =enclosingRect.height / 3;fontdesc_Create(
    enclosingRect.width  =  enclosingRect.width / 2 ;
    enclosingRect.height = enclosingRect.height / 2 ;
    ysize = enclosingRect.height - enclosingRect.top;
    xsize = enclosingRect.width - enclosingRect.left;
    fnotev_DrawRect(self,&(enclosingRect));
    pt[0].x = enclosingRect.left - 1;
    pt[0].y = enclosingRect.height + enclosingRect.top - ysize;
    pt[1].x = pt[0].x - xsize;
    pt[1].y = pt[0].y ;
    pt[2].x = pt[1].x ;
    pt[2].y = pt[0].y  + ysize + ysize;
    pt[3].x = pt[0].x + xsize;
    pt[3].y = pt[2].y;
    pt[4].x = pt[3].x;
    pt[4].y = enclosingRect.top + enclosingRect.height + 1;
    fnotev_DrawPath(self,pt,5);
#else
    enclosingRect.width++;
    enclosingRect.height++; 
/* fprintf(stderr,"wid = %d, height = %d\n",enclosingRect.width,enclosingRect.height);fflush(stdout);*/
    if(Fnote(self)->open == -333)
	fnote_Open(Fnote(self),self->parenttext);
    if(fnote_IsOpen(Fnote(self))){
	fnotev_SetFont(self,self->ofd);
	fnotev_MoveTo(self,/* self->ci[1].xOriginOffset + */SHIM,/*self->ci[1].yOriginOffset +*/SHIM);
    }
    else{
	fnotev_SetFont(self,self->fd);
	fnotev_MoveTo(self,/*self->ci[0].xOriginOffset + */1,/*self->ci[0].yOriginOffset + */1);
    }
    fnotev_DrawString(self,self->displaystr,(view_ATTOP | view_ATLEFT)); 
    fnotev_DrawRect(self,&(enclosingRect)); 
  /*	fnotev_MoveTo(self,enclosingRect.width / 2,enclosingRect.height / 2);
    fnotev_DrawString(self,"*",(view_BETWEENLEFTANDRIGHT | view_BETWEENTOPANDBOTTOM)); */
    

#endif
}
void fnotev__LinkTree(self, parent)
register struct fnotev *self;
struct view *parent;
{
    super_LinkTree(self,parent);
    while(!class_IsTypeByName(class_GetTypeName(parent),"textview")){
	if((parent = parent->parent) == NULL ) return;
    }
    self->parenttext = (struct text *) view_GetDataObject(parent);
    self->parentview = (struct textview *) parent;
}

#if 0
ismyenv(self,d,pos,env)
struct fnotev *self;
struct text *d;
long pos;
struct environment *env;
{
    if(env->type == environment_View && env->data.viewref->dataObject == DataObject(self)){
	self->pos = pos;
	return TRUE;
    }
}
#endif
void fnotev__ObservedChanged(self, changed, value)
struct fnotev *self;
struct observable *changed;
long value;
{
    if(value == observable_OBJECTDESTROYED){
	if(self->imp && changed == (struct observable *)self->imp->fn){
#if 0
	    /* can't do this code */
	    struct impair *cim,*lim;
	    lim = NULL;
	    for(cim = list; cim != NULL; cim = cim->next){
		if(cim->fn == self->imp->fn) break;
		lim = cim;
	    }
	    if(cim){
		if(cim == list){
		    list = cim->next;
		}
		else if(lim){
		    lim->next = cim->next;
		}
		free(cim);
	    }
#endif
	    self->imp->fn = NULL;
#if 0
	    if(self->imp->textview){
		textview_RemoveObserver(self->imp->textview,self->imp);
		if(self->imp->app && self->imp->app != (struct view *)self->imp->textview) 
		    textview_DeleteApplicationLayer(self->imp->textview, self->imp->app);
		textview_Destroy(self->imp->textview);
		self->imp->textview = NULL;
	    }
#endif
	}
    }
    fnotev_WantUpdate(self,self);
}
void fnotev__FullUpdate(self,type,left,top,width,height)
struct fnotev *self;
enum view_UpdateType type;
long left,top,width,height;
{
    DoUpdate(self,TRUE);
}
void fnotev__Update(self)
struct fnotev *self;
{
    DoUpdate(self,FALSE);
}

boolean fnotev__InitializeObject(classID,self)
struct classheader *classID;
struct fnotev *self;
{
    self->imp = NULL;
    self->fd = fontdesc_Create(FONTNAME,0,FONTSIZE);
    self->ofd = fontdesc_Create(OFNAME,0,OFSIZE);
    self->cursor = cursor_Create(self);
    cursor_SetStandard(self->cursor,Cursor_RightPointer);
    self->fnotetype = environ_GetProfileInt("messagefootnote",-1);
    self->parenttext = NULL;
    self->displaystr = "*";
    self->ci = ( (struct fontdesc_charInfo *) calloc(2,sizeof(struct fontdesc_charInfo)));
    self->ci[0].width = 0;
    return TRUE;
}
void fnotev__FinalizeObject(classID,self)
struct classheader *classID;
struct fnotev *self;
{
    free(self->ci);
}
boolean fnotev__InitializeClass(classID)
struct classheader *classID;
{
    list = NULL;
    endnotes = FALSE;
    return TRUE;
}
void fnotev__SetEndnote(classID,doendnotes)
struct classheader *classID;
boolean doendnotes;
{
endnotes = doendnotes;
}
void fnotev__ReceiveInputFocus(self)
    struct fnotev *self;
{
	super_ReceiveInputFocus(self);
	if(self->parentview){	
	    /* this should only happen when the view is created */
	    textview_SetDotPosition(self->parentview,textview_GetDotPosition(self->parentview) + 1);
	    textview_WantInputFocus(self->parentview,self->parentview);
	}
}

