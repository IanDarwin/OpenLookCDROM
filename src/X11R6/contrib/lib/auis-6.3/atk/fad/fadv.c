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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/fad/RCS/fadv.c,v 2.23 1994/01/30 05:52:43 rr2b Exp $";
#endif

#include <andrewos.h> /* sys/time.h sys/types.h */
#include <sys/stat.h>
#include <class.h>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <graphic.ih>
#include <cursor.ih>
#include <message.ih>
#include <fontdesc.ih>
#include <environ.ih>
#include <bind.ih>
#include <view.ih>
#include <filetype.ih>
#include <im.ih>
#include <complete.ih>
#include <event.ih>
#include <fadv.eh>

#define GetDelay(A) (event_MSECtoTU(20))
#define SetMode(A,B) (A->mode = (B))
#define GetMode(A) (A->mode)
#define IsMode(A,B) (A->mode & B)

#define SAMPLEFONT "/bin/samplefont" /* assume that this samplefont supports the -t mode needed by fad */
#define LINEMODE 0
#define BOXMODE 1
#define ICONMODE 2
#define GROUPMODE 4 /* Not Implemented */
#define LABELMODE 8
#define ANIMATEBUTTONMODE 16
#define WAITMODE 64

#define BOXMODEON
#define	NORESHAPE   /* Won't attempt to scale the drawing to the window */
#define MAXWIDTH 20000
#define MAXHEIGHT 20000
#define	STARTHEIGHT 256	/* initial default height */
#define FRTIME 30 	/* default minimum number of milliseconds between frames */
#define UPDATEICON -10010
#define OLD 1
#define NEW 0
#define PFUG 4
#define min(a,b) (a < b) ? a:b
#define BBUFSIZE 2048
#define ICONFLAG -10
#define ldraw(P,A) if(IsMode(A,BOXMODE))\
   {fadview_MoveTo(P,(long)A->x1,(long)A->y1);\
   fadview_DrawLineTo(P,(long)A->x2,(long)A->y1);\
   fadview_DrawLineTo(P,(long)A->x2,(long)A->y2);\
   fadview_DrawLineTo(P,(long)A->x1,(long)A->y2);\
   fadview_DrawLineTo(P,(long)A->x1,(long)A->y1);}\
   else {fadview_MoveTo(P,(long)A->x1,(long)A->y1);\
   fadview_DrawLineTo(P,(long)A->x2,(long)A->y2);} 

static void idraw();
struct aniinfo {
    struct fadview *self;
    struct anivect *anbuf,*eap;
    struct fad_frame *lf,*sf;
    int i;
};

#define fdraw(A) if (ISICONORLABEL(A->x2)) idraw(self,A); else ldraw(self,A)
#define ObjectOf(ll) ll->header.view.dataobject
#define findpic(ll) ((struct fad *)ObjectOf(ll))
#define TellUser(s) message_DisplayString(self,0,s)
#define BOXTEST(A) IsMode(A,BOXMODE)
static struct keymap *fadviewKeymap;
static struct menulist *fadviewMenulist;
static struct proctable_Entry *FadProc;
static int samplefont_failed ;



static MySetCursor(self,f,i)
struct fadview *self;
struct fontdesc *f;
int i;
{
    cursor_SetGlyph(self->cursor,f,i);
    if(!cursor_IsPosted(self->cursor)){
	struct rectangle tr;
	fadview_GetVisualBounds(self,&tr);
	fadview_PostCursor(self,&tr,self->cursor);
    }
}
static MySetStandardCursor(self,i)
struct fadview *self;
short i;
{
    cursor_SetStandard(self->cursor,i);
    if(!cursor_IsPosted(self->cursor)){
	struct rectangle tr;
	fadview_GetVisualBounds(self,&tr);
	fadview_PostCursor(self,&tr,self->cursor);
    }
}
static int CurrentFrame(self)
struct fadview *self;
{
    register int i;
    register struct fad_frame *fra;
    for(i = 1,fra = findpic(self)->bf;fra != NULL && fra != self->f ; fra = fra->f)
	i++;
    return i;
}
static struct fontdesc *my_DefineFont(fname)
char *fname;
{
    char familyname[256];
    long fontStyle;
    long  fontSize;
    fontdesc_ExplodeFontName(fname,familyname, sizeof(familyname), &fontStyle, &fontSize);
    return fontdesc_Create(familyname,  fontStyle, fontSize);
}
static void UpdateCursor(self)
struct fadview *self;
{
    struct fad *cp;
    static struct fontdesc *i12font = NULL;
    cp = findpic(self);
    if(self->HasFocus) {
	if(IsMode(self,LABELMODE))
	    MySetCursor(self,cp->labelfont,'a');
	else if(IsMode(self,ICONMODE)){
	    if(cp->iconpointnow == NULL && fad_flipicons(cp) == 0){ MySetStandardCursor(self,Cursor_Arrow);
	    return;
	    }
	    MySetCursor(self,cp->currentfont,cp->currenticon);
	}
#ifdef ANIMATEBUTTONMODE
	else if(IsMode(self,ANIMATEBUTTONMODE)){
	    if(i12font == NULL) i12font = my_DefineFont("icon12");
	    MySetCursor(self,i12font,'L');
/*	    MySetStandardCursor(self,Cursor_Octagon); */
	}
#endif /* ANIMATEBUTTONMODE */
	else if(IsMode(self,BOXMODE))
	    MySetStandardCursor(self,Cursor_CrossHairs);
	else if(IsMode(self,WAITMODE))
	    MySetStandardCursor(self,Cursor_Wait);
	else MySetStandardCursor(self,Cursor_Arrow);
    }
    else MySetStandardCursor(self,Cursor_Arrow);
}

enum view_DSattributes fadview__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct fadview *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;

{
    struct fad *cp;
    cp = findpic(self);
    if(pass ==  view_NoSet &&  cp!= NULL && cp->desw > 0 && cp->desh > 0){
	*desiredheight = min(height,cp->desh) ;
	*desiredwidth =  min(width,cp->desw); 
	return( view_WidthFlexible | view_HeightFlexible) ;
    }
    *desiredwidth = width;
    *desiredheight = (height > 2048) ? ((cp) ? cp->desh:STARTHEIGHT) :height;
    return(view_Fixed);
}

static fontinit(cp)
struct fad *cp;
{
    while(cp->initializedfonts < cp->topinmp){
	cp->fontpt[cp->initializedfonts]  = my_DefineFont(cp->inmp[cp->initializedfonts]);
	(cp->initializedfonts)++;
    }
    if(cp->currentfont == NULL && cp->initializedfonts > 1) cp->currentfont = cp->fontpt[cp->initializedfonts - 1] ;
}
HaltAnimation(self)
struct fadview *self;
{
    if(self->nextevent){
	event_Cancel(self->nextevent);
	self->nextevent = NULL;
    }
    if(self->anobj){
	self->f = self->anobj->sf ;
	if(self->anobj->i != 0){
	    /* redraw the last frame */
	    self->Redraw = TRUE;
/*	    fadview_WantUpdate(self,self); */
	}
	else 	self->Redraw = FALSE;
	fadview_FlushGraphics(self);
	free(self->anobj->anbuf);
	free(self->anobj);
	self->FrameChanged = TRUE;
	self->anobj = NULL;
	fadview_WantUpdate(self,self);
    }
}
void	
fadview__Update(self)
struct fadview *self;
{
    struct vector *vc;
    struct fad *cp;
    if(self->removed) return;
    fadview_SetTransferMode(self,graphic_INVERT);
    self->needUpdate = FALSE;
    if(self->anobj) { doan(self->anobj); return ;}
    if(self->DoAnimation){ if(!DoAnimation(self)) return; }
    cp = findpic(self);
    if(cp->labelfont == NULL) cp->labelfont = my_DefineFont(cp->labelfontname);
    if(cp->topinmp != cp->initializedfonts) fontinit(cp);
    if(self->Redraw){
	clearfad(self);
	for(vc = self->f->v; vc != NULL ; vc = vc->v)
	    vecdraw(self,vc);
    }
    else{
	if(self->FocusChanged)	    
	    fadview_DrawRectSize(self,0,0, fadview_GetLogicalWidth(self) -1, fadview_GetLogicalHeight(self)-1);
	self->Redraw = TRUE;
    }	
    self->FocusChanged = FALSE;
    UpdateCursor(self);
    if(self->FrameChanged){
	if(self->mode != WAITMODE) nameframe(self);
	self->FrameChanged = FALSE;
    }
    if(self->mode == WAITMODE) 
	message_DisplayString(self,0,"PICK DESIRED ICONS AND QUIT SAMPLEFONT");

    fadview_FlushGraphics(self);
}
void fadview__FullUpdate(self,type,left,top,width,height)
struct fadview *self;
enum view_UpdateType type;
long left,top,width,height;
{
    if(type == view_MoveNoRedraw){
	UpdateCursor(self);
	return;
    } 
    if(type == view_Remove){
	self->removed = TRUE;
	return;
    }
    self->removed = FALSE;
    self->Redraw = TRUE;
    self->FrameChanged = TRUE;
    recalc(self);
    if(self->anobj) {
	/* reset the ongoing animation */
	HaltAnimation(self);
	self->DoAnimation = TRUE;
    }
    fadview_Update(self);
}
void fadview__aniframe(self,framecount,startat,gofor,mtm)
struct fadview *self;
int framecount,startat,gofor,mtm;
{
    self->framecount = framecount;
    self->startat = startat;
    self->gofor = gofor;
    self->mtm = mtm * 1000;
    self->DoAnimation = TRUE;
    fadview_WantUpdate(self,self);
}
static int dodoan();
queup(self)
struct fadview *self;
{
	struct fad *cp = findpic(self);
	self->nextevent = im_EnqueueEvent(dodoan,self,event_MSECtoTU(cp->frtime));
}

static int
dodoan(self)
struct fadview *self;
{
    self->nextevent = NULL;
    if(self->anobj){
	if(! self->updatedue){
	    queup(self);
	    fadview_WantUpdate(self,self);
	}
	self->updatedue = TRUE;
    }
}

static DoAnimation(self)
struct fadview *self;
{

    struct fad *cp;
    struct fad_frame *lf,*sf;
    struct vector *vc;
    int i;
    float fc;
    struct aniinfo *anobj;
    register struct anivect *ap;
    register struct vector *vp,*nvp;
    struct anivect *anbuf,*endanbuf;

    anobj = (struct aniinfo *) malloc(sizeof(struct aniinfo));
    self->DoAnimation = FALSE;
    cp = findpic(self);
    lf = cp->bf;
    if(self->startat != 0) self->startat--;
    for(i = 0 ; i < self->startat && lf != NULL ; i++)
	lf = lf->f;
    sf = lf;
    fc = (float) self->framecount;
    if(lf == NULL || lf->f == NULL){
	if(self->startat == 0) TellUser("at Last Frame");
	else TellUser("Not that many frames");
	return FALSE;
    }
    anbuf = (struct anivect *) malloc(sizeof(struct anivect) * self->anbufsize);
    if(anbuf == NULL) {
	TellUser("Insufficient Memory To Animate");
	return FALSE;
    }
    endanbuf = anbuf + self->anbufsize -1;
    ap = anbuf;
    mloop:	for(vp = lf->v,nvp = lf->f->v;vp != NULL && nvp != NULL;
    vp = vp->v,nvp = nvp->v){
	if((nvp->p1->x == vp->p1->x) && (nvp->p1->y ==vp->p1->y ) && 
	   (nvp->p2->x ==  vp->p2->x) && (nvp->p2->y == vp->p2->y)) {
	    vp->stat = 'C';
	    continue;
	}
	if((ISICONORLABEL(nvp->p2->x) != ISICONORLABEL(vp->p2->x))){
	    /* don't animate icons or labels that turn into 
			     lines or boxes (or visa-versa) */
	     vp->stat = 'D';
	     continue;
	}
	vp->stat = 'N';
	ap->x1 = (float) vp->p1->x;
	ap->y1 = (float) vp->p1->y;
	ap->x2 = (float) vp->p2->x;
	ap->y2 = (float) vp->p2->y;
	ap->dx1 = ((float) nvp->p1->x - ap->x1) / fc;
	ap->dy1 = ((float) nvp->p1->y - ap->y1) / fc;
	ap->dx2 = ((float) nvp->p2->x - ap->x2) / fc;
	ap->dy2 = ((float) nvp->p2->y - ap->y2) / fc;
	ap->label = vp->label;
	ap->mode = vp->mode;
	if(++ap >= endanbuf){
	    /* ran out of room, reset the size we need and restart */
	    self->anbufsize += 64;
	    free(anbuf);
	    self->DoAnimation = TRUE;
	    fadview_WantUpdate(self,self);
	    return FALSE;
	}
    }
    for(;vp != NULL; vp = vp->v) vp->stat = 'C';
    lf->av = ap;

    if(lf->f->f != NULL && --self->gofor ){
	lf = lf->f;
	goto mloop;
    }
    if(self->f != sf){
	clearfad(self);
	/* graphic_SetTransferMode(self,graphic_INVERT); */
	for(vc = sf->v; vc != NULL ; vc = vc->v)
	    vecdraw(self,vc);
    }
    lf = lf->f;
    anobj->self= self;
    anobj->anbuf = anbuf;
    anobj->eap = anbuf;
    anobj->i = 0;
    anobj->sf = sf;
    anobj->lf = lf;
    self->anobj = anobj;
    doan(anobj);
    queup(self);
    return FALSE;
 /*    */
}
doan(anobj)
struct aniinfo *anobj;
{
    struct fadview *self;
    struct fad_frame *lf,*sf;
    struct vector *vc,*vc1,*ov;
    int i;
    register struct anivect *ap,*eap;


    self= anobj->self;
    eap = anobj->eap;
    lf = anobj->lf;
    sf = anobj->sf;
    i = anobj->i;
    if(i == self->framecount){
	/* advance to next frame */
	i = 0;
	anobj->i = i;
	(self->startat)++;
	for( ap = eap  ;ap != sf->av; ap++)
	    fdraw(ap);
	ov = sf->v;	
	eap = sf->av;
	anobj->eap = eap;
	sf= sf->f;
	anobj->sf = sf;
	/* 	draw any new vectors for next frame */
	for(vc = sf->v,vc1 = ov; vc != NULL && vc1 != NULL; vc = vc->v,vc1 = vc1->v) {
	    if(vc1->stat != 'C') vecdraw(self,vc);	
	    if(vc1->stat == 'D') vecdraw(self,vc1);
	}
	/* 	clear any deleted vectors */
	for(;vc1!= NULL;vc1 = vc1->v)
	    vecdraw(self,vc1);
	for(;vc!= NULL;vc = vc->v)
	    vecdraw(self,vc);
    }
    else i++;
    if(sf == lf){
	/* done */
	HaltAnimation(self);
	return TRUE;
    }
    for( ap = eap  ;ap != sf->av; ap++){
	fdraw(ap);
	ap->x1 += ap->dx1;
	ap->y1 += ap->dy1;
	if(ISICONORLABEL(ap->x2)) {
	    idraw(self,ap);
	}
	else {
	    ap->x2 += ap->dx2;
	    ap->y2 += ap->dy2;
	    ldraw(self,ap);
	}
    }
/*    fprintf(stdout,"drawing %d - %d\n",lf,i); fflush(stdout); */
    if(self->FocusChanged){
	fadview_DrawRectSize(self,0,0, fadview_GetLogicalWidth(self) -1, fadview_GetLogicalHeight(self)-1);
	self->FocusChanged = FALSE;
    }
    fadview_FlushGraphics(self);
    anobj->lf = lf;
    anobj->i = i;
    self->updatedue = FALSE;
    if (self->nextevent == NULL)
	queup(self);

}
void fadview__nextframe(self,cp)
struct fadview *self;
struct fad *cp;
{
    struct fad_frame *lf;
    char resp[64];
    lf = self->f->f;
    if(lf == NULL){
	if(cp->readonly) return;
	if(message_AskForString(self,0,"At end.Create blank frame?[n]",0,resp,63)== -1) 
	    return;
	if(!(*resp == 'y' || *resp == 'Y')) return ;
	lf = fad_newframe(cp);
	self->f->f = lf;
    }
    self->f = lf;
    self->FrameChanged = TRUE;
    fadview_WantUpdate(self,self);
}
void fadview__lastframe(self,cp)
struct fadview *self;
struct fad *cp;
{
    struct fad_frame *lf;
    if(self->f == cp->bf){
	TellUser("At Base Frame");
	return;
    }
    for(lf=cp->bf;lf->f != self->f; lf = lf->f) ;
    self->f  = lf;
    self->FrameChanged = TRUE;
    fadview_WantUpdate(self,self);
}

static recalc(self)
struct fadview *self;
{
    struct fad_frame *frr;
    struct fadpoint *pp;
    struct fad *cp;
#ifndef NORESHAPE
    float xfac,yfac;
#endif /* NORESHAPE */
    /* 	int xmove,ymove; */
    int wd,ht;  
    cp = findpic(self);
    if(cp->w  == fadview_GetLogicalWidth(self)  &&  cp->h == fadview_GetLogicalHeight(self) &&
	cp->ox == 0 && cp->oy == 0)
	return(0);
    if(cp->w == 0 || cp->h == 0) {
	picset(self,0);
	return(1);
    }
#ifdef NORESHAPE
    if(cp->ox == 0 && cp->oy == 0){
	if(cp->fad_square) picset(self,0);
	return(2);
    }
#endif /* NORESHAPE */
    wd = cp->w;
    ht = cp->h;
    if(cp->fad_square){
	ht = min(ht,wd);
	wd = ht;
    }
#ifndef NORESHAPE
    xfac = (float) wd / (float) cp->w ;
    yfac = (float)ht / (float) cp->h ;
#endif /* NORESHAPE */
    for(frr = cp->bf; frr != NULL; frr = frr->f){
	for(pp = frr->p; pp != NULL; pp = pp->p){
	    if(ISICONORLABEL(pp->x)) continue;
#ifdef NORESHAPE
	    pp->x = pp->x - cp->ox/*  + xmove */;
	    pp->y = pp->y - cp->oy /* + ymove */;
#else /* NORESHAPE */
	    pp->x = (long)((float)(pp->x - cp->ox) * xfac) /* + xmove */;
	    pp->y = (long)((float)(pp->y - cp->oy) * yfac) /* + ymove */;
#endif /* NORESHAPE */
	}
    }
    picset(self,1);
    return(2);
}
static picset(self,flag)
struct fadview *self;
int flag;
{
    struct fad *pc = findpic(self);
    if(flag){
	pc->ox = 0 /* self->in.rect.left */;
	pc->oy = 0 /* self->in.rect.top */; 
    }
    if(pc->fad_square){
	pc->w = min(fadview_GetLogicalWidth(self),fadview_GetLogicalHeight(self));
	pc->h = pc->w;
    }
    else {
	pc->w  = fadview_GetLogicalWidth(self);
	pc->h  = fadview_GetLogicalHeight(self);
    }
}
static clearfad(self)
struct fadview *self;
{
    fadview_SetTransferMode(self,graphic_WHITE);
    fadview_EraseVisualRect(self);	
    fadview_SetTransferMode(self,graphic_INVERT);
    if(self->HasFocus){
	fadview_DrawRectSize(self,0,0,fadview_GetLogicalWidth(self)-1,fadview_GetLogicalHeight(self)-1);
    }
}

void fadview__FinalizeObject(classID, self)
struct classheader *classID;
struct fadview *self;
{
    HaltAnimation(self);
    if(self->menulist) menulist_Destroy(self->menulist);

}

boolean fadview__InitializeObject(classID, self)
struct classheader *classID;
struct fadview *self;
{
    self->keystate = keystate_Create(self, fadviewKeymap);
    self->menulist = NULL;
    self->cursor = cursor_Create(self);
    self->DoAnimation = FALSE;
    self->HasFocus = 0;
    self->removed = FALSE;
    self->Moving = 0;
    self->anbufsize = 128;
    self->Redraw = TRUE;    
    self->FrameChanged = TRUE;
    self->animationPending = FALSE;
    self->needUpdate = FALSE;
    self->FocusChanged = FALSE;
    self->mode = LINEMODE;
    self->anobj = NULL;
    self->nextevent = NULL;
    return TRUE;
}

static AddMenus(self,ml,menuProc)
struct fadview *self;
struct menulist *ml;
struct proctable_Entry *menuProc;
{
    int readonly = 0;
    if(self && findpic(self))
	readonly = findpic(self)->readonly;
    if(!readonly) {
	menulist_AddToML(ml,"File,Read Fad~50",menuProc,(long) 'R',0);
	menulist_AddToML(ml,"Icon~5,Next icon~10",menuProc,(long) 'f',0);
	menulist_AddToML(ml,"Icon~5,Previous icon~11",menuProc,(long) 'u',0); 
	menulist_AddToML(ml,"Icon~5,Pick new icons~21",menuProc,(long) 'I',0);
	menulist_AddToML(ml,"Mode~2,Line~1",menuProc,(long) 'N',0);
	menulist_AddToML(ml,"Mode~2,Icon~2",menuProc,(long) 'E',0);
	menulist_AddToML(ml,"Mode~2,Label~3",menuProc,(long) 'L',0);
#ifdef BOXMODEON
	menulist_AddToML(ml,"Mode~2,Box~4",menuProc,(long) 'b',0);
#endif /* BOXMODEON */
#ifdef ANIMATEBUTTONMODE
	menulist_AddToML(ml,"Mode~2,Action Button~5",menuProc,(long) 'B',0);
#endif /* ANIMATEBUTTONMODE */
	menulist_AddToML(ml,"Misc~7,Delete last item~10",menuProc,(long) 'd',0);
	menulist_AddToML(ml,"Misc~7,Delete current frame~11",menuProc,(long) 'D',0);
	menulist_AddToML(ml,"Misc~7,Set Label Font~30",menuProc,(long) 'F',0);
	menulist_AddToML(ml,"Misc~7,Set # animation frames~31",menuProc,(long) '#',0);
	menulist_AddToML(ml,"Misc~7,Set frame delay~32",menuProc,(long) 't',0);
	menulist_AddToML(ml,"Fad~1,Repeat frame~30",menuProc,(long) 'r',0);
    }
    menulist_AddToML(ml,"Fad~1,Pick frame~22",menuProc,(long) 'p',0);
    menulist_AddToML(ml,"Fad~1,Next frame~20",menuProc,(long) 'n',0);
    menulist_AddToML(ml,"Fad~1,Previous frame~21",menuProc,(long) 'l',0);
    menulist_AddToML(ml,"Fad~1,Animate~5",menuProc,(long) 'a',0);
    menulist_AddToML(ml,"Fad~1,Animate portion~6",menuProc,(long) 'A',0);
    /* 	menulist_AddToML(ml,"Fad,Copy last frame",menuProc,(long) 'c'); */
}

void 
fadview__showfad(self,i,cp)
struct fadview *self;
int i;
struct fad *cp;
{
    register struct fad_frame *fra;
    for(fra = cp->bf;fra != NULL && i > 1 ; fra = fra->f)
	i--;
    if(fra != NULL){
	self->f = fra;
	fadview_WantUpdate(self,(struct view *)self);
    }
}

static
KeyIn(self,cr)
struct fadview *self;
long cr;
{
    char frs[256],fff[256];
    int i,startas = 0,gofor = 0;
    struct fad_frame *fra;
    struct vector *vec,*nv;
    struct fadpoint *fp,*lp;
    struct fad *cp = findpic(self);
    if(self->anobj) {
	if(cr == 'a' || cr == 'p' || cr == 'P'){
	    HaltAnimation(self);
	    fadview_WantUpdate(self,self);
	}
	return;
    }
    if(self->mode == WAITMODE)  return;
    switch(cr){
	case 'L':
	    if(IsMode(self,LABELMODE)) 
		SetMode(self,LINEMODE);
	    else
		SetMode(self,LABELMODE);
	    UpdateCursor(self);
	    break;
	case 'F':
	    if(message_AskForString(self,0,"Label Font? ",cp->labelfontname,frs,256)== -1) break;
	    cp->labelfont = my_DefineFont(frs);
	    strcpy(cp->labelfontname,frs);
	    break;
	case 'n':
	    fadview_nextframe(self,cp);
	    break;
	case 'l':
	    fadview_lastframe(self,cp);
	    break;
	case 'a':
	case 'H':
	    fadview_aniframe(self,cp->Frames,0,0,cp->frtime);
	    break;
	case 'm':
	    fadview_aniframe(self,cp->Frames,0,0,0);
	    break;
	case 't':
	    sprintf(frs,"%d",cp->frtime);
	    sprintf(fff,"%d",cp->frtime);
	    if(message_AskForString(self,0," minimum number of milliseconds between frames> ",fff,frs,256) == -1) break;;
	    cp->frtime = atoi(frs);
	    if(cp->frtime > 1000) cp->frtime = 1000;
	    break;
	case 'A':
	    if(startas < 1) startas = 1;
	    sprintf(frs,"%d",startas);
	    sprintf(fff,"%d",startas);
	    if(message_AskForString(self,0,"Starting frame number ",fff,frs,256)== -1) break;
	    startas = atoi(frs);
	    sprintf(frs,"%d",gofor);
	    sprintf(fff,"%d",gofor);
	    if(message_AskForString(self,0,"number of frames ",fff,frs,256) == -1) break;
	    gofor = atoi(frs);
	    fadview_aniframe(self,cp->Frames,startas,gofor,cp->frtime);
	    break;
	case '#':
	    /* set speed */
	    sprintf(frs,"%d",cp->Frames);
	    sprintf(fff,"%d",cp->Frames);
	    if(message_AskForString(self,0,"# of animation frames> ",fff,frs,256)== -1) break;
	    if((i = atoi(frs)) > 0)
		cp->Frames = i;
	    break;
	case 'D':
	    /* delete frame */
	    if(cp->readonly) break;
	    fad_SetModified(cp);
	    fra = self->f;
	    if(self->f == cp->bf){
		if(self->f->f != NULL){
		    self->f = self->f->f;
		}
		else self->f = fad_newframe(cp);
		cp->bf = self->f;
		cp->deleated = fra;
		fad_freeframe(cp,fra);
		fadview_WantUpdate(self,(struct view *)self);
		fad_NotifyObservers(cp,CurrentFrame(self));
		break;
	    }
	    fadview_lastframe(self,cp);
	    if(self->f == NULL) break;
	    if(self->f->f != NULL)  	self->f->f  = self->f->f->f;
	    cp->deleated = fra;
	    fad_freeframe(cp,fra);
	    fad_NotifyObservers(cp,CurrentFrame(self));
	    break;
	case 'd':
	    if(cp->readonly) break;
	    fad_delvector(cp,self->f);
	    fad_SetModified(cp);
	    fad_NotifyObservers(cp,CurrentFrame(self));
	    fadview_WantUpdate(self,(struct view *)self);
	    break;
#ifdef SQUAREWORKS
	case 'T':
	    cp->fad_square = ! cp->fad_square;
	    break;
#endif /* SQUAREWORKS */
#ifdef NOEZ
	case 'S':
	    /* get file to save */

	    if(message_AskForString(self,0,"File to save",cp->fadname,frs,256) == -1 ||
	       *frs == '\0') break;
	    filetype_CanonicalizeFilename(cp->fadname, frs, sizeof(cp->fadname) - 1);
	case 's':
	    /* save file */
	    if(cp->readonly) break;
	    if(*(cp->fadname) == '\0' || (f = fopen(cp->fadname,"w")) == NULL) {
		sprintf(frs,"ERROR:Can't Write %s",cp->fadname);
		TellUser(frs);
		break;
	    }
	    fad_Write(cp,f,0L,0);
	    fclose(f);
	    break;
#endif /* NOEZ */
#ifdef BOXMODEON
	case 'b':
	    if(cp->readonly) break;
	    SetMode(self,BOXMODE);
	    UpdateCursor(self);
	    break;
#endif /* BOXMODEON */
	case 'R':
	    if(cp->readonly) break;
/*
	    if(message_AskForString(self,0,"File to read ",cp->fadname,frs,256) == -1 ||
	       *frs == '\0') break;
	    filetype_CanonicalizeFilename(cp->fadname, frs, sizeof(cp->fadname) - 1);
*/
	    if(completion_GetFilename(self,"File to read: ",NULL,frs,256,FALSE,TRUE) == -1)
		break;
	    strcpy(cp->fadname,frs);
	    fadview_fileread(self,cp->fadname);
	    break;
	case 'p':
	    for(i = 1,fra = cp->bf;fra != NULL && fra != self->f ; fra = fra->f)
		i++;
	    sprintf(frs,"%d",i);
	    sprintf(fff,"%d",i);
	    if(message_AskForString(self,0,"frame #",fff,frs,256) == -1) break;
	    if( (i = atoi(frs)) <= 0) break;
	    self->FrameChanged = TRUE;
	    fadview_showfad(self,i,cp);
	    break;
	case 'r':
	    if(cp->readonly) break;
	    fra = self->f->f;
	    if(fra == NULL){
		fra = fad_newframe(cp);
		self->f->f = fra;
	    }
	    else {
		self->f->f = fad_newframe(cp);
		self->f->f->f = fra;
	    }
	    for(vec  = self->f->v; vec != NULL; vec = vec->v){
		fp=fad_setpoint(cp,vec->p1->x,vec->p1->y,NEW,self->f->f);
		lp=fad_setpoint(cp,vec->p2->x,vec->p2->y,NEW,self->f->f);	
		nv = fad_setvector(cp,fp,lp,self->f->f);
		if(vec->label){
		    nv->label =malloc(strlen( vec->label) + 1);
		    strcpy(nv->label,vec->label);
		}
		nv->mode = vec->mode;
	    }
	    fadview_nextframe(self,cp);
	    self->Redraw = FALSE;
	    break;
	case 'I':
	    if(cp->readonly) break;
	    SetMode(self,ICONMODE);
	    if( !seticon(self)){
		if(self->mode != WAITMODE) SetMode(self,LINEMODE);
		break;
	    }
	    UpdateCursor(self);
	    break;
	case 'f': /* flip to next icon */
	    if(cp->readonly || cp->currentfont == 0) break;
	    if(fad_flipicons(cp) == 0) break;
	    self->mode = ICONMODE;
	    UpdateCursor(self);
	    break;
	case 'u': /* flip to last icon */
	    if(cp->readonly || cp->currentfont == 0) break;
	    if(fad_unflipicons(cp) == 0) break;
	    self->mode = ICONMODE;
	    UpdateCursor(self);
	    break;
	case 'E':	/* Toggle icon mode */
	    if(cp->readonly) break;
	    self->mode = ICONMODE;
	    if(cp->currentfont == 0 && !seticon(self)) {
		if(self->mode != WAITMODE) self->mode = LINEMODE;
		break;
	    }
	    UpdateCursor(self);
	    break;
	case 'P':
	    self->DoAnimation = TRUE;
	    (self->startat)++;
	    fadview_WantUpdate(self,self);
	    break;
	case 'N':
	    self->mode = LINEMODE;
	    UpdateCursor(self);
	    break;
#ifdef ANIMATEBUTTONMODE
	case 'B':   /* toggle animate mode */
	    self->mode = ANIMATEBUTTONMODE;
	    UpdateCursor(self);
	    break;
#endif /* ANIMATEBUTTONMODE */
#ifdef GROUPMODEWORKS
	case 'g':   /* toggle group mode */
	    self->mode = GROUPMODE;
	    UpdateCursor(self);
	    break;
#endif /* GROUPMODEWORKS */
	default:
	    break;
    }
    return/* ((struct view *)self) */;
}
void
fadview__ReceiveInputFocus(self)
struct fadview *self;
{
    self->HasFocus = 1;
    self->FocusChanged = TRUE;
    self->Redraw = FALSE;
    fadview_WantUpdate(self,(struct view *)self);
    self->keystate->next = NULL;
    fadview_PostKeyState(self, self->keystate);
    fadview_PostMenus(self,self->menulist);
}
void
fadview__LoseInputFocus(self)
struct fadview *self;
{
    self->HasFocus = 0;
    self->FocusChanged = TRUE;
    self->Redraw = FALSE;
    fadview_WantUpdate(self,(struct view *)self);
}

static nameframe(self)
struct fadview *self;
{
    char frs[32];
    sprintf(frs,"at frame #%d",CurrentFrame(self));
    TellUser(frs);
}
QueueAnimation(self,action,mousex,mousey)
struct fadview *self;
enum view_MouseAction action;
long mousex, mousey;
{
    struct fadpoint *pt;
    struct vector *vc;
    struct fad *cp = findpic(self);
    if(self->anobj) return(FALSE);
    if(action == view_LeftDown){
	if((pt = fad_setpoint(cp,mousex,mousey,OLD,self->f)) != NULL){
	    for(vc = self->f->v; vc != NULL ; vc = vc->v){
		if(vc->p1 == pt && vc->mode == ANIMATEMODE){
		    self->animationPending = TRUE;
		    return TRUE;
		}
	    }
	}
    }
    else if(self->animationPending ) {
	if(action == view_LeftUp) {
	    self->animationPending = FALSE;
	    fadview_aniframe(self,cp->Frames,0,0,cp->frtime);
	}
	return TRUE;
    }
    return FALSE;
}

	   

struct fadview *fadview__Hit(self,action,mousex,mousey,numberOfClicks) 
struct fadview *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    static struct fadpoint ptmp;
    struct vector *cv;
    struct fad_frame *Cframe;
    struct fad *cpic = findpic(self);
    Cframe = self->f;
    if(QueueAnimation(self,action,mousex,mousey)) return self;
    if(!(self->HasFocus)){
	fadview_WantInputFocus(self,(struct view *)self);
	cpic->fp = NULL;
	return( self);
    }
    if(self->anobj) return(self);
    if(cpic->readonly) {
	if(action == view_LeftUp || action == view_RightUp)	
	    fadview_aniframe(self,cpic->Frames,0,0,cpic->frtime);
	return self;
    }
    if(self->mode == WAITMODE) return self;
    fadview_SetTransferMode(self,graphic_INVERT);
    switch (action) {
	case view_LeftDown:
	    if(cpic->fp != NULL){
		cpic->fp = NULL;
		cpic->pltnum = 0;
	    }
	    else {
		cpic->fp=fad_setpoint(cpic,mousex,mousey,NEW,Cframe);
		cpic->pltnum = 0;
		cpic->lp = NULL;
		/* graphic_SetTransferMode(self,graphic_INVERT); */
	    }
	    break;
	case view_LeftUp:
	    if(cpic->fp == NULL){
		break;
	    }
	    if(cpic->lp) drawlist(self,cpic);
	    cpic->pltnum = 0;
	    if(IsMode(self , LABELMODE)){
		char frs[256];
		int szz;
		if(message_AskForString(self,0,"Label String? ","",frs,256)== -1) break;
		if((szz = strlen(frs)) == 0 ) break;
		cpic->lp = fad_setpoint(cpic,LABELFLAG,LABELFLAG,NEW,Cframe);	
		cv = fad_setvector(cpic,cpic->fp,cpic->lp,Cframe);
		cv->label = malloc(szz + 1);
		strcpy(cv->label,frs);
	    }
	    else if(BOXTEST(self)){
		cpic->lp = fad_setpoint(cpic,mousex,mousey,NEW,Cframe);
		cv = fad_setvector(cpic,cpic->fp,cpic->lp,Cframe);   
		cv->mode = BOXMODE;
	    }
#ifdef ANIMATEBUTTONMODE
	    else if(IsMode(self , ANIMATEBUTTONMODE)){
		cpic->lp = fad_setpoint(cpic,fad_iconnum(cpic,"icon12"),'L',NEW,Cframe);
		cv = fad_setvector(cpic,cpic->fp,cpic->lp,Cframe);
		cv->mode = ANIMATEMODE;
	    }
#endif /* ANIMATEBUTTONMODE */
	    else if(IsMode(self , ICONMODE)){
		cpic->lp = fad_setpoint(cpic,cpic->currentfontindex,cpic->currenticon,NEW,Cframe);	
		cv = fad_setvector(cpic,cpic->fp,cpic->lp,Cframe);
	    }
	    else {
		cpic->lp = fad_setpoint(cpic,mousex,mousey,NEW,Cframe);
		cv = fad_setvector(cpic,cpic->fp,cpic->lp,Cframe);
	    }
	    vecdraw(self,cv);
	    cpic->fp = NULL;
	    cpic->lp = NULL;
	    fad_SetModified(cpic);
	    self->Redraw = FALSE;
	    fad_NotifyObservers(cpic,CurrentFrame(self));
	    break;
	 case view_RightDown:
	     if(cpic->fp != NULL) cpic->fp = NULL;
	      else if((cpic->fp =fad_setpoint(cpic,mousex,mousey,OLD,Cframe)) == NULL){
		  MySetStandardCursor(self,Cursor_Cross );
		  cpic->pltnum = -1;
	      }
	      else {
		  getlist(self,cpic->fp);
		  cpic->lp = cpic->fp;
		  /* graphic_SetTransferMode(self,graphic_INVERT); */
	      }
	     break;
	 case  view_RightUp:
	     if(cpic->fp == NULL){
		 UpdateCursor(self);
		 break;
	     }
	     if(cpic->lp) drawlist(self,cpic);
	     else cpic->lp = &ptmp;
	     self->Redraw = FALSE;
	     if(mousex < 0) {
		 mousex = 0;
	     }
	     if(mousey < 0) {
		 mousey = 0;
	     }
	     cpic->lp->x = mousex;
	     cpic->lp->y = mousey;
	     drawlist(self,cpic);
	     cpic->pltnum = 0;
	     cpic->fp->x = mousex;
	     cpic->fp->y = mousey;
/*	     fadview_WantUpdate(self,(struct view *)self); */
	     cpic->fp = NULL;
	     cpic->lp = NULL;
	     fad_SetModified(cpic);
	     fad_NotifyObservers(cpic,CurrentFrame(self));
	     break;
	 case view_LeftMovement:
	     if(IsMode(self , ICONMODE) && !BOXTEST(self)) break;
	 case view_RightMovement:
	     if(cpic->pltnum == -1 || cpic->fp == NULL) break;
	     if(mousex < 0 || mousey < 0) break;
	     if(cpic->lp) drawlist(self,cpic);
	     else cpic->lp = &ptmp;
	     cpic->lp->x = mousex;
	     cpic->lp->y = mousey;
	     drawlist(self,cpic);
	     break;
	 default :
	     break;
    }
    return(self);
}
static drawlist(self,cpic)
struct fadview *self;
register struct fad *cpic;
{
    register int i;
    static struct vector tempvec;
    if(cpic->lp){
	tempvec.mode = self->mode;
	tempvec.p1 = cpic->fp;
	tempvec.p2 = cpic->lp;
	vecdraw(self,&tempvec);
	fadview_MoveTo(self,cpic->fp->x,cpic->fp->y);
	fadview_DrawLineTo(self,cpic->fp->x,cpic->fp->y);
    }
    for(i =cpic->pltnum; i--;){
	vecdraw(self,cpic->veclist[i]);
    }
}
static getlist(self,ppt)
register struct fadview *self;
register struct fadpoint *ppt;
{
    register int i = 0;
    register struct vector *vc;
    struct fad *cpic = findpic(self);
    for(vc = self->f->v; vc != NULL ; vc = vc->v){
	if(vc->p1 == ppt || (vc->p2 == ppt))cpic->veclist[i++] = vc;
    }
    cpic->pltnum = i;
}

static seticon(self)
struct fadview *self;
{
    char frs[256];
    struct fad *cp = findpic(self);
    if(*(cp->cfname) == '\0') strcpy(cp->cfname,"icon12");
    if(message_AskForString(self,0,"Icon Font ",cp->cfname,frs,256) != -1) {
	strcpy(cp->cfname,frs);
	return(fadview_geticons(self,cp->cfname));
    }
}

static ReadIcons(f,self)
FILE *f;
struct fadview *self;
{
    register int c;
    struct fad *cpic = findpic(self);
    while(( c = getc(f))!= EOF){
	if(c == NULL) break;
	fad_setpoint(cpic, fad_iconnum(cpic,cpic->cfname), c,NEW,self->f);
	if (!FILE_HAS_IO(f)) break;
    }
    if(c == EOF){
	if(cpic->iconpointend == cpic->iconpoints)
	    /* No icons chosen */
	    self->mode = LINEMODE;
	else self->mode = ICONMODE;
	im_RemoveFileHandler(f);
	self->Redraw = FALSE;
	if(pclose(f) != 0){
	    samplefont_failed = 1;
	    if(fadview_geticons(self,cpic->cfname))
		self->mode = ICONMODE;
	}
	else fadview_WantUpdate(self,self);
    }
}
int fadview__geticons(self,s)
struct fadview *self;
char *s;
{
    char bb[512],samp[512],*cp,*andrewdir;
    FILE *ff;
    struct fad *cpic = findpic(self);
    andrewdir = environ_AndrewDir(NULL);
    sprintf(samp,"%s%s",andrewdir,SAMPLEFONT);
    if(samplefont_failed == 3){
	/* see if there is a samplefont program */
	struct stat st;
	samplefont_failed = 1;
	if((stat(samp,&st) == 0) && (st.st_mode & S_IEXEC)){
	    samplefont_failed = 0;
	}
    }
    if(!samplefont_failed){
	message_DisplayString(self,0,"PICK DESIRED ICONS AND QUIT SAMPLEFONT");
	sprintf(bb,"%s -t %s",samp,s);
	if((ff = popen(bb,"r")) != NULL){
	    self->mode = WAITMODE;
	    im_AddFileHandler (ff, ReadIcons, self, 6);
	    return(NULL);
	}
	samplefont_failed = TRUE;
    }
    message_DisplayString(self,0,"");
    if(message_AskForString(self,75,"Characters for icons ","",bb,256)== -1) return(0);
    for(cp = bb; *cp; cp++){
	fad_setpoint(cpic,fad_iconnum(cpic,cpic->cfname),(int)*cp,NEW,self->f);
    }
    return(fad_flipicons(cpic));
}


static void idraw(self,A) 
register struct fadview *self;
register struct anivect *A;
{
    static char cc;
    struct fad *cp = findpic(self);
    fadview_MoveTo(self,(long)A->x1,(long)A->y1);
    if(A->label){
	fadview_SetFont(self,cp->labelfont);
	fadview_DrawString(self,A->label,0);
    }
    else {
	fadview_SetFont(self,cp->fontpt[-((int)(A->x2))]);
	cc = A->y2;
	fadview_DrawText(self,&cc,1,0);
    }
}	
static vecdraw(self,v)
register struct fadview *self;
register struct vector *v;
{
    static char cc;
    register struct fad *cp = findpic(self);
    /* graphic_SetTransferMode(self,graphic_INVERT); */
    fadview_MoveTo(self,v->p1->x,v->p1->y);
    if(v->label){
	fadview_SetFont(self,cp->labelfont);
	fadview_DrawString(self,v->label,0);
    }
    else if(ISICON(v->p2->x)){
	fadview_SetFont(self,cp->fontpt[-(v->p2->x)]);
	cc = v->p2->y;
	fadview_DrawText(self,&cc,1,0);
    }
    else if(BOXTEST(v)){
	fadview_DrawLineTo(self,v->p2->x,v->p1->y);
	fadview_DrawLineTo(self,v->p2->x,v->p2->y);
	fadview_DrawLineTo(self,v->p1->x,v->p2->y);
	fadview_DrawLineTo(self,v->p1->x,v->p1->y);
    }
    else {
	fadview_DrawLineTo(self,v->p2->x,v->p2->y);
    }
}
void
fadview__fileread(self,fnm)
struct fadview *self;
char *fnm;
{
    FILE *ff,*fopen();
    struct fad *cp = findpic(self);
    if((ff = fopen(fnm,"r")) != NULL) {
	fad_FinalizeObject(cp);
	fad_InitializeObject(cp);
	fad_Read(cp ,ff, 0L);
	fclose(ff);
	strcpy(cp->fadname,fnm);
    }
}
void
fadview__Print(self,file, processor,finalFormat,topLevel)
struct fadview *self;
FILE *file;
char *processor,*finalFormat;
boolean topLevel;
{
    struct fad *cp;
    register struct vector *vc;
    cp = findpic(self);
    BeginTroff(file, cp->desh,self);
    for(vc = self->f->v; vc != NULL ; vc = vc->v)
	PrintVec(cp,vc);
    EndTroff(cp->desh);
}

static labelfonttype(self)
struct fadview *self;

{
    struct fad *cp;
    char *c;
    cp = findpic(self);
    c = cp->labelfontname;
    if(c == NULL || *c ==  '\0' || *c == '.') return('R');
    while(*c != '.' && *c != '\0') c++;
    c--;
    switch(*c){
	case 'i' : return('I');
	case 'b': return('B');
	default:
	    return('R');
    }
}
static labelfontsize(self)
struct fadview *self;
{
    struct fad *cp;
    char *c;
    cp = findpic(self);
    c = cp->labelfontname;
    if(c == NULL || *c ==  '\0' || *c == '.') return(10);
    while(*c != '.' && *c != '\0' ){
	if(*c >= '0' && *c <= '9') return(atoi(c));
	c++;
    }
    return(10);
}

/* 
  * Printing stuff.	
  */

#define ScreenXRes 80			/* screen units per inch */
#define ScreenYRes 80
/* typedef float FLOAT; */
static float fxmul, fymul;		/* fractional screen units to device units */
static float /* xmul,*/ ymul;		/* screen units to device units */
static int npoints = 0;			/* points since last reposition */
static float curx, cury;			/* current position in device units */
static long xorg=0, yorg=0;		/* offsets in fractional screen units */

#define FINISHLINE fprintf(printout,"\n.sp -1\n"), npoints=0;
#define STARTLINE  fprintf(printout,"\\h'%0.4fi'\\v'%0.4fi'",curx,cury);

static FILE *printout = NULL;
static
PrintVec(cp,v)
struct fad *cp;
struct vector *v;
{
    if(v->label) {
	xx_MoveTo((v->p1->x - cp->ox )<<16,(v->p1->y - cp->oy) <<16);
	STARTLINE
	  fprintf(printout,"\\\n%s",v->label);
	FINISHLINE
    }
    else if(BOXTEST(v)){
	xx_MoveTo((v->p1->x - cp->ox )<<16,(v->p1->y - cp->oy) <<16);
	xx_DrawTo((v->p2->x- cp->ox )<<16,(v->p1->y- cp->oy)<<16);
	xx_DrawTo((v->p2->x- cp->ox )<<16,(v->p2->y- cp->oy)<<16);
	xx_DrawTo((v->p1->x- cp->ox )<<16,(v->p2->y- cp->oy)<<16);
	xx_DrawTo((v->p1->x- cp->ox )<<16,(v->p1->y- cp->oy)<<16);
    }
    else if(!ISICONORLABEL(v->p2->x)){
	xx_MoveTo((v->p1->x- cp->ox )<<16,(v->p1->y- cp->oy)<<16);
	xx_DrawTo((v->p2->x- cp->ox )<<16,(v->p2->y- cp->oy)<<16);
    }
}


static
xx_MoveTo(x,y)
long x, y;			/* in fractional screen units */
{
    x -= xorg, y -= yorg;
    if (npoints) FINISHLINE;
    curx = x*fxmul;  cury = y*fymul;
}


static
xx_DrawTo(x,y)
long x, y;			/* in fractional screen units */
{
    x -= xorg, y -= yorg;
    if (npoints==0) STARTLINE;
    fprintf(printout,"\\\n\\D'l %0.4fi %0.4fi'",
	     (x*fxmul)-curx, (y*fymul)-cury);
    if (++npoints>=10) FINISHLINE;
    curx = x*fxmul,  cury = y*fymul;
}
static BeginTroff(file,yneed,self)
FILE *file;
int yneed;			/* dots per inch */
struct fadview *self;
{

    printout = file;
    /* dimensions */
    /*    xmul = (FLOAT) 1/ScreenXRes;  */
    ymul = (float) 1/ScreenYRes;
    fxmul = 1/(ScreenXRes*65536.0);
    fymul = 1/(ScreenYRes*65536.0);
    /* save old state */
    fprintf(printout,"\n.nr @f \\n(.f\n");		/* font */
    fprintf(printout,".nr @i \\n(.i\n");		/* indent */
    fprintf(printout,".nr @j \\n(.j\n");		/* justification mode */
    fprintf(printout,".nr @l \\n(.l\n");		/* line length */
    fprintf(printout,".nr @s \\n(.s\n");		/* point size */
    fprintf(printout,".nr @u \\n(.u\n");		/* 0=nofill, 1=fill */
    /* establish new state */
    fprintf(printout,".br\n.sp -1\n");
    fprintf(printout,".cs CW 22\n.fi\n.ad b\n");
    fprintf(printout,".ft %c\n",labelfonttype(self));
    fprintf(printout,".ps %d\n",labelfontsize(self));


    /* need */
    fprintf(printout,".ne %.4fi\n",yneed*ymul);

}

void fadview__ObservedChanged(self, changed, value)
struct fadview *self;
struct observable *changed;
long value;
{
    struct fad *cpic = findpic(self);
    if (value == observable_OBJECTDESTROYED)
	fadview_Destroy(self);
    else {
	if(value == fad_NEWFAD){
	    fadview_showfad(self,1,cpic);
	    if(self->header.view.parent)
		view_WantNewSize(self->header.view.parent,self); /* Yes, it really has to be called this way */
	}
	else if(self->f == cpic->deleated || value == CurrentFrame(self))
	    fadview_showfad(self,value,cpic);
    }
}
void fadview__SetDataObject(self, dataObject)
    struct fadview *self;
    struct dataobject *dataObject;
{
    if (!class_IsTypeByName(class_GetTypeName(dataObject), "fad"))  {
	fprintf(stderr, "Incompatible dataobject associated with fadview\n");
	return;
    }

    super_SetDataObject(self, dataObject);
    self->f = ((struct fad *)dataObject)->f;
    AddMenus(self,fadviewMenulist,FadProc);
    self->menulist = menulist_DuplicateML(fadviewMenulist, self);
    }

static EndTroff(yneed)
{
    if (npoints) FINISHLINE;
    fprintf(printout,".sp %0.4fi\n",((yneed << 16 ) * fymul));

    /* restore old state */
    fprintf(printout,".ft \\n(@f\n");
    fprintf(printout,".in \\n(@iu\n");
    fprintf(printout,".ad \\n(@j\n");
    fprintf(printout,".ll \\n(@lu\n");
    fprintf(printout,".ps \\n(@s\n");
    fprintf(printout,".ie \\n(@u .fi\n.el .nf\n");
    fprintf(printout,".sp 1\n");

    fflush(printout);

    xorg = 0,  yorg = 0;
}
void fadview__WantUpdate(self, requestor)
    struct fadview *self;
    struct view *requestor;
{
    if (self->needUpdate && (struct view *)self == requestor) return;
    super_WantUpdate(self, requestor);
    if((struct view *)self == requestor) self->needUpdate = TRUE;
}

boolean fadview__InitializeClass(classID)
struct classheader *classID;
{
    char *c,buf[2];

    fadviewMenulist = menulist_New();
    fadviewKeymap = keymap_New();

    FadProc = proctable_DefineProc("fadview-keys", KeyIn,&fadview_classinfo,NULL,"get keyed commands"); 
    for(c = "cnlramIfuEdDsRbBNqP",buf[1] = '\0'; *c; c++){
	*buf = *c;
	keymap_BindToKey(fadviewKeymap, buf, FadProc, *c);
    }
    samplefont_failed = 3;
    return TRUE;
}
