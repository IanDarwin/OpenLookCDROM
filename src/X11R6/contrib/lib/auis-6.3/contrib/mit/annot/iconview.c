/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/iconview.c,v 1.15 1993/12/09 00:22:01 gk5g Exp $ */
/* $ACIS:$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/iconview.c,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *iconview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/iconview.c,v 1.15 1993/12/09 00:22:01 gk5g Exp $";
#endif

#include <andrewos.h>
#include <rect.h>
#include <class.h>
#include <text.ih>
#include <textv.ih>
#include <view.ih>
#include <graphic.ih>
#include <fontdesc.ih>
#include <icon.ih>
#include <dataobj.ih>
#include <bind.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <buffer.ih>
#include <im.ih>
#include <iconview.eh>

static struct iconview *First;

/* Global determines if new icon views will draw open or closed. */
static int iconopen = TRUE;    /* default initial closed */

/* default values for closed icon character */
#define ICONFONT "icon"
#define ICONSTYLE fontdesc_Plain
#define ICONPTS 12
#define ICONCHAR '4'
#define TITLEFONT "andysans"
#define TITLESTYLE fontdesc_Plain
#define TITLEPTS 12
#define WIDTH 200
#define HEIGHT 100
#define	HANDLE	17

/****************************************************************/
/*		private functions				*/
/****************************************************************/

/* Draw the iconview in the case when its open (subview visable) */
static void
DrawOpen(self, type, ax, ay, aw, ah)
     struct iconview * self;
     enum view_UpdateType type;
     long ax,ay,aw,ah;  /* area "A"ffected by this fullupdate */
{  
    long x,y,w,h;   /* my coordinate space */
    long cx, cy, cw, ch; /* my "C"hilds coordinate space */
    long handle_height;
    long tx, ty, tw, th; /* "T"itle coordinate space */
    struct FontSummary * titlesummary;
    short * titlefontwidths;
    char * title = (char *)malloc(1024);
    
    if (self->neednewsize) {
	self->neednewsize = 0;
	iconview_WantNewSize(self,self);
	return;
    }



    /* Get my size. Are width and height returning bogosity? */
    x = iconview_GetLogicalLeft(self);
    y = iconview_GetLogicalTop(self);
    w = iconview_GetLogicalWidth(self) - 1;
    h = iconview_GetLogicalHeight(self) - 1;

    /* Draw a frame */
    iconview_SetTransferMode(self, graphic_COPY);
    iconview_DrawRectSize(self, x, y, w, h);

    if (self->titlefont == (struct fontdesc *)0)
	self->titlefont = fontdesc_Create(TITLEFONT, TITLESTYLE, TITLEPTS);
    titlesummary = fontdesc_FontSummary(self->titlefont, self->header.view.drawable);
    titlefontwidths = fontdesc_WidthTable(self->titlefont, self->header.view.drawable);

    /* get the title and calculate its width */
    title = icon_GetTitle((struct icon *) self->header.view.dataobject);
    tw = MIN((string_width(title, self->titlefont, self->header.view.drawable) + (*title ? titlesummary->maxSpacing : 0)), w - 2); /* add one-character-width padding if title is non-null */

    handle_height = MIN(titlesummary->newlineHeight + 2, h);

    th = MIN(titlesummary->newlineHeight, handle_height - 2);
    tx = x + (w - tw) / 2;
    ty = y + (handle_height - th) / 2;

    /* With a gray stripe on top. */
    iconview_FillRectSize(self, x+1, y+1, w-1, handle_height-1, iconview_GrayPattern(self,4,16));

    /* draw the title */
    iconview_SetTransferMode(self, graphic_WHITE);
    iconview_FillRectSize(self, tx, ty, tw, th, graphic_WHITE);
    iconview_SetTransferMode(self, graphic_BLACK);
    iconview_SetFont(self, self->titlefont);
    iconview_MoveTo(self, x + w / 2, y + handle_height / 2);
    iconview_DrawString(self, title, (graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM));

    /* cacluate place for children */
    cx = x + 1; /* one pixel frame */
    cy = y + handle_height;
    cw = w - 2; /* two one pixel sides */
    ch = h - handle_height - 1; /* handle and frame at bottom */

    if (self->child != (struct view *)0) {
	/* is this the right place to do this? (inside my fullupdate)*/
	view_InsertViewSize(self->child, self, cx, cy, cw, ch);

	/* calculate the fullupdate coordinates in childs space */
	ax = ax - cx;
	ay = ay - cy;
	if (ax + aw > cx + cw)
	    aw = cx + cw - ax;
	if (ay + ah > cx + ch)
	    ah = cy + ch - ay;
	
	view_FullUpdate(self->child, type, 0, 0, cw, ch);
    }

    /* record childs extents */
    self->cx = cx;
    self->cy = cy;
    self->cw = cw;
    self->ch = ch;

    /* remember our size so we can rerequest it if necessary */
    iconview_RecommendSize(self, w+1, h+1);

}



/* Draw the iconview when it is closed (subview invisible) */
static void
DrawClosed(self, type, ax, ay, aw, ah)
     struct iconview * self;
     enum view_UpdateType type;
     long ax,ay,aw,ah;  /* area "A"ffected by this fullupdate */
{  
    long x,y;
    struct fontdesc_charInfo iconinfo;
    short iconx, icony;

    if (self->neednewsize) {
	self->neednewsize = 0;
	iconview_WantNewSize(self,self);
	return;
    }
    /* Get my icon font. Is this sure not to return NULL? */
    if (self->iconfont == (struct fontdesc *)0)
	self->iconfont = fontdesc_Create(ICONFONT, ICONSTYLE, ICONPTS);
    fontdesc_CharSummary(self->iconfont, self->header.view.drawable,
			  self->iconchar, &iconinfo);
    iconx = iconinfo.xOriginOffset;
    icony = iconinfo.yOriginOffset;

    /* Get my size. Are width and height returning bogosity? */
    x = iconview_GetLogicalLeft(self);
    y = iconview_GetLogicalTop(self);

    iconview_SetFont(self, self->iconfont);
    iconview_SetTransferMode(self, graphic_BLACK);
    iconview_MoveTo(self, x + iconx, y + icony);
    iconview_DrawText(self, &(self->iconchar), 1, graphic_NOMOVEMENT);
} 


static
SlayChild(self)
    struct iconview * self;
{  
    int twoviews;
    if (self->child != (struct view *)0) {
	if (self->isopen)
	    view_UnlinkTree(self->child);
	twoviews = self->child != self->bottomview;
	view_Destroy(self->child);
	if (twoviews)
	    view_Destroy(self->bottomview);
	self->child = (struct view *)0;
	self->bottomview = (struct view *)0;
    }
}


static
AdoptNewChild(self,dobj)
    struct iconview * self;
    struct icon * dobj;
{  
    long x,y;
    char * viewclass;
    struct dataobject * d;

    SlayChild(self);

    d = icon_GetChild((struct icon *)dobj);

    if (d != (struct dataobject *)0) {
	viewclass = dataobject_ViewName(d);
	self->child = (struct view *)0;
	self->bottomview = (struct view *)class_NewObject(viewclass);
	if (self->bottomview != (struct view *)0) {
	    view_SetDataObject(self->bottomview,d);
	    self->child = view_GetApplicationLayer(self->bottomview);
	    if (self->isopen) {
		view_LinkTree(self->child,self);
		iconview_WantUpdate(self,self);
	    }
	}
    }
    icon_GetSize((struct icon *)dobj, &x, &y);
    iconview_RecommendSize(self,x,y);
    self->neednewsize = 1;
}


string_width(string, font, graphic)
char * string;
struct fontdesc * font;
struct graphic * graphic;
{
    short * widthtable, totalwidth = 0;
    widthtable = fontdesc_WidthTable(font, graphic);
    while (*string) 
	totalwidth = totalwidth + widthtable[*string++];
    return(totalwidth);
}


/****************************************************************/
/*		class procedures				*/
/****************************************************************/

boolean
iconview__InitializeClass(classID)
    struct classheader * classID;
{
First = NULL;
return TRUE;
} 


void iconview__GetOrigin(self, width, height, originX, originY)
    struct view *self;
    long width;
    long height;
    long *originX;
    long *originY;
{
    *originX = 0;
    *originY = 14;
}

boolean
iconview__InitializeObject(classID, self)
    struct classheader * classID;
    struct iconview * self;
{  
    self->isopen = iconopen;
    self->dw = WIDTH;
    self->dh = HEIGHT;
    self->child = (struct view *)0;
    self->bottomview = (struct view *)0;
    self->neednewsize = 0;
    self->next = First;
    self->iconchar = ICONCHAR;
    self->iconfont =  fontdesc_Create(ICONFONT, ICONSTYLE, ICONPTS);
    self->titlefont =  fontdesc_Create(TITLEFONT, TITLESTYLE, TITLEPTS);
    First = self;
    return TRUE;
}


void
iconview__FinalizeObject(classID, self)
    struct classheader * classID;
    struct iconview * self;
{  
    struct iconview *p;
    if(self == First){
	First = self->next;
    }
    else {
	for(p=First; p != NULL ; p = p->next){
	    if(p->next == self){
		p->next = self->next;
		break;
	    }
	}
    }
    SlayChild(self);
}

void
iconview__CloseRelated(classID, v)
struct classheader * classID;
struct view *v;
{
    struct iconview *p;
    struct im *im;

    iconopen = FALSE; /* All new notes draw closed. */

    im = view_GetIM(v);
    if(im == NULL) return;
    for(p=First; p != NULL ; p = p->next){
	if(p->isopen == TRUE && im == iconview_GetIM(p)) iconview_Close(p);
    }
}

void
iconview__OpenRelated(classID, v)
struct classheader * classID;
struct view *v;
{
    struct iconview *p;
    struct im *im;

    iconopen = TRUE; /* All new notes draw open. */

    im = view_GetIM(v);
    if(im == NULL) return;
    for(p=First; p != NULL ; p = p->next){
	if(p->isopen == FALSE && im == iconview_GetIM(p)) iconview_Open(p);
    }
}
/****************************************************************/
/*		instance methods				*/
/****************************************************************/

struct view *
iconview__GetChild(self)
    struct iconview * self;
{
    return self->child;
}

void
iconview__SetChild(self, viewclass)
     struct iconview * self;
     char *viewclass;
{
    struct icon * dobj = (struct icon *)iconview_GetDataObject(self);
    struct dataobject * d;

    SlayChild(self);

    d = icon_GetChild((struct icon *)dobj);

    if (d != (struct dataobject *)0) {
	self->child = (struct view *)0;
	self->bottomview = (struct view *)class_NewObject(viewclass);
	if (self->bottomview != (struct view *)0) {
	    view_SetDataObject(self->bottomview,d);
	    self->child = view_GetApplicationLayer(self->bottomview);
	    if (self->isopen) {
		view_LinkTree(self->child,self);
		iconview_WantUpdate(self,self);
	    }
	}
    }
    iconview_WantUpdate(self, self);
}

void
iconview__Update(self)
     struct iconview * self;
{
    struct rectangle r;

    iconview_SetTransferMode(self, graphic_COPY);
    iconview_EraseVisualRect(self);
    iconview_GetLogicalBounds(self, &r);
    iconview_FullUpdate(self, view_FullRedraw, r.left, r.top, r.width, r.height);
} /* iconview_Update */

void
iconview__FullUpdate(self, type, x, y, w, h)
     struct iconview *self;
     enum view_UpdateType type;
     long x, y, w, h;
{  
    switch(type) {
	case view_FullRedraw:
	case view_PartialRedraw:
	case view_LastPartialRedraw: {
	    struct rectangle r;
	    iconview_GetLogicalBounds(self, &r);
	    if (self->isopen)
		DrawOpen(self, type, r.left, r.top, r.width, r.height);
	    else
		DrawClosed(self, type, r.left, r.top, r.width, r.height);
	    }
	    break;
	case view_MoveNoRedraw:
	case view_Remove:
	    view_FullUpdate(self->child, type, 0, 0, self->cw, self->ch);
	    break;
    }
}


enum view_DSattributes
iconview__DesiredSize(self, w, h, pass, dw, dh)
    struct iconview * self;
    long w, h;
    enum view_DSpass pass;
    long *dw, *dh;
{  
	struct fontdesc_charInfo iconinfo;
	if (!self->isopen) {
	    fontdesc_CharSummary(self->iconfont, self->header.view.drawable,
				 self->iconchar, &iconinfo);
	    *dw = iconinfo.width;
	    *dh = iconinfo.height;
	    return (view_Fixed);
	} else {
	    *dw = self->dw;
	    *dh = self->dh;
	    iconview_DecidedSize(self, self->dw, self->dh);
	    return (view_WidthFlexible | view_HeightFlexible);
	}

}


struct view *
iconview__Hit(self, action, x, y, clicks)
    struct iconview * self;
    enum view_MouseAction action;
    long x,y;
    long clicks;
{  
    if (self->isopen) {
	if (self->child != (struct view *)0 &&
	    x >= self->cx &&
	    y >= self->cy &&
	    x < self->cx + self->cw &&
	    y < self->cy + self->ch) {
	    x -= self->cx;
	    y -= self->cy;
	    return view_Hit(self->child, action, x, y, clicks);
	} else {
	    if (y < self->cy
		&& action == view_LeftUp
		|| action == view_RightUp) {
		struct view *v;
		iconview_Close(self);
		v = (struct view *) self;
		v = v->parent;
		if(v != NULL && strcmp(class_GetTypeName(v),"matte") == 0)
		    v = v->parent;
		if(v)
		    view_WantInputFocus(v,v);
	    }
	    return (struct view *)self;
	}
    } else {
	if (action == view_LeftUp || action == view_RightUp) {
	    iconview_Open(self);
	    if (self->child != (struct view *)0) {
		view_WantInputFocus(self->bottomview,
				    self->bottomview);
	    } else {
		iconview_WantInputFocus(self, self);
	    }

	} else
	    return (struct view *)self;
    }
    return (struct view *)self;
}

void
iconview__ReceiveInputFocus(self)
struct iconview *self;
{
    if (self->isopen && self->child != (struct view *)0) {
	view_WantInputFocus(self->bottomview,
			    self->bottomview);
    } else {
	struct view *parent = ((struct view *)self)->parent;
	while (parent && !class_IsTypeByName (class_GetTypeName(parent), "textview"))
	    parent = parent->parent;
	if (parent) view_WantInputFocus(parent, parent);
    }
}
    

void
iconview__Close(self)
struct iconview *self;
{
    if(self->isopen == TRUE){
	self->isopen = FALSE;
	if (self->child != (struct view *)0)
	    view_UnlinkTree(self->child);
	iconview_WantNewSize(self,self);
    }
}

void
iconview__Open(self)
struct iconview *self;
{
    if(self->isopen == FALSE){
	self->isopen = TRUE;
	iconview_WantNewSize(self,self);
	if (self->child != (struct view *)0) {
	    view_LinkTree(self->child, self);
	}
    }
}

void
iconview__RecommendSize(self, w, h)
    struct iconview * self;
    long w;
    long h;
{  
	if (w > 0 && h > 0) {
	    self->dw = w;
	    self->dh = h;
	}
}



void
iconview__DecidedSize(self, w, h)
    struct iconview * self;
    long w;
    long h;
{  
    icon_SetSize((struct icon *)self->header.view.dataobject,w,h);
}



void
iconview__SetDataObject(self,dobj)
    struct iconview * self;
    struct dataobject * dobj;
{  
    super_SetDataObject(self,dobj);
    AdoptNewChild(self,(struct icon *)dobj);
}


void
iconview__ObservedChanged(self, data, value)
    struct iconview * self;
    struct observable * data;
    long value; 
{  
    if (value == observable_OBJECTDESTROYED) {
	SlayChild(self);
    } else if (data == (struct observable *)iconview_GetDataObject(self)){
	switch(value) {
	  case icon_TitleChanged:
	  case icon_SizeChanged:
	  case icon_ChangedSomehow:
	    iconview_WantUpdate(self, self);
	    break;
	  case icon_ChildChanged:
	    AdoptNewChild(self,(struct icon *)data);
	    break;
	  default:
	    super_ObservedChanged(self, data, value);
	} /* switch */
    } else {
	super_ObservedChanged(self, data, value);
    }
}

void
iconview__LinkTree(self, parent)
    struct iconview * self;
    struct view *parent;
{
	super_LinkTree(self, parent);
	if(self->child) view_LinkTree(self->child, self);
}

void
iconview__SetIconFont(self,iconfont,iconstyle,iconpts)
 struct iconview * self;
 char * iconfont;
 int iconstyle;
 int iconpts;
{
 
    self->iconfont = fontdesc_Create(iconfont, iconstyle, iconpts);
}

#if 0
void
iconview__SetIconFontname (self,name)
 struct iconview * self;
 char *name;
{
     self->iconfont = (struct fontdesc *)0; /* force replacement */
     if (self->iconfontname) free (self->iconfontname);
     self->iconfontname = (char *)malloc(strlen(name) +1);
     bcopy (name, self->iconfontname, strlen(name));
}
#endif 

void
iconview__SetIconChar(self,iconchar)
struct iconview * self;
char iconchar;
{
    self->iconchar = iconchar;
}

void
iconview__SetTitleFont(self,titlefont,titlestyle,titlepts)
 struct iconview * self;
 char * titlefont;
 int titlestyle;
 int titlepts;
{
 
    self->titlefont = fontdesc_Create(titlefont, titlestyle, titlepts);
}
