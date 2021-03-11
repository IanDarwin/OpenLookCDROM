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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/scroll.c,v 1.34 1994/02/01 20:39:29 rr2b Exp $";
#endif
/* Scrollbar code for ATK based on the original scrollbar code for be2. */

#include <andrewos.h>
#include <class.h>
#include <graphic.ih>
#include <view.ih>
#include <updlist.ih>
#include <im.ih>
#include <cursor.ih>
#include <event.ih>
#include <environ.ih>
#include <sbuttonv.ih>
#include <sbutton.ih>
#include <region.ih>
#include <point.h>
#include <rect.h>
#include <scroll.eh>

#define ENDTOBARSPACE 4
#define MINDOTLENGTH 6
#define SMALLDIST 5

#define COLORWINDOWPADDING 1
#define COLORENDZONELENGTH 16
#define COLORBARTOBUTTONSPACE 4
#define COLORBARWIDTH 20
#define COLORDOTWIDTH 6
#define COLORELEVATORWIDTH 16
#define COLORDRAWBORDERS 1
/* #define COLORDOTBOTTOMSHADOW "#666" */

#define MONOWINDOWPADDING 0
#define MONOENDZONELENGTH 12
#define MONOBARTOBUTTONSPACE -1
#define MONOBARWIDTH 18
#define MONODOTWIDTH 8
#define MONOELEVATORWIDTH 18
#define MONODRAWBORDERS 0

#define MONOSTYLE 3
#define MONOBARTOP "#ccc"
#define MONOBARFOREGROUND MONOBARTOP
#define MONOBUTTONSTYLE 4
#define MONOBUTTONTOP "#eee"
#define MONOBUTTONTOPSHADOW MONOBUTTONTOP
#define MONOBUTTONBOTTOMSHADOW MONOBUTTONTOP
#define MONODOTSTYLE 4
#define MONODOTTOP "white"
#define MONODOTTOPSHADOW "black"
#define MONODOTBOTTOMSHADOW "black"

#define PIXELSPERINCH(self) 75


/* The descriptions of the different types of scrollbars */
static int Type[scroll_SIDES] = {scroll_VERT, scroll_VERT, scroll_HORIZ, scroll_HORIZ};    

static int cursortypes[scroll_TYPES]={Cursor_VerticalArrows, Cursor_HorizontalArrows};

static char *InterfaceName[scroll_TYPES] = {"scroll,vertical", "scroll,horizontal"};

#define REALBARWIDTH(self) ((self)->barWidth)
#define WPADDING(x) ((x)->windowPadding)
#define VPADDING(x) ((x)->viewPadding)

/* Useful macros */

#ifndef ABS
#define ABS(x) (((x)<0)?-(x):(x))
#endif

#define SWAP(a,b) (temp)=(a);(a)=(b);(b)=(temp)

#define INTERSECT(top1, bot1, top2, bot2) ((bot1) >= (top2) && (top1) <= (bot2))

#define XYTOHITCOORD(self, side, x, y) ((((side)==scroll__LEFT || (side)==scroll__RIGHT)?(y)-(self)->childrect.top:(x)-(self)->childrect.left))

#define CHILDBARHEIGHT(self, side) ((Type[side]==scroll_VERT)?(self)->childrect.height:(self)->childrect.width)

static struct point arrowpts[]={
    {65, 35},
    {35, 50},
    {65, 65}
};

static struct sbutton_info thebutton={
    NULL,   /* the prefs struct will be filled later */
    "",	    /* the label is empty */
    0,	    /* the rock isn't needed */
    NULL,   /* ditto for the trigger atom */
    FALSE,  /* initially not lit, will be set appropriately */
};

#define DRAWRECT(self, left, top, width, height) scroll_DrawRectSize(self, left, top, (width)-1, (height)-1)
#define FILLRECT(self, left, top, width, height) scroll_FillRectSize(self, left, top, (width), (height), NULL)

static void draw_arrow(self, side, r, dir, lit)
struct scroll *self;
int side;
struct rectangle *r;
int dir;
boolean lit;
{
    struct rectangle t;
    double oldfg[3];

    if (self->emulation) {
        int i;

        scroll_SetTransferMode(self, graphic_WHITE);
        for(i=0;i<10;i++) {
            int offx=0, offy=0, t;
            switch(i) {
                case 0:
                    offx=(-1);
                    break;
                case 1:
                    offx=(1);
                    break;
                case 2:
                    offy=(-1);
                    break;
                case 3:
                    offy=(1);
                    break;
                case 4:
                    offx=(-1);
                    offy=(-1);
                    break;
                case 5:
                    offx=(1);
                    offy=(1);
                    break;
                case 6:
                    offx=(-1);
                    offy=(1);
                    break;
                case 7:
                    offx=(1);
                    offy=(-1);
                    break;
                case 8:
                    scroll_SetTransferMode(self, graphic_BLACK);
                    break;
                case 9:
                    offx=(1);
                    break;
                default: ;
            }
            switch(side) {
                case scroll__LEFT:
                case scroll__RIGHT:
                    scroll_MoveTo(self, offx+r->left+arrowpts[0].x*r->width/100, offy+r->top+(dir<0?r->height:0)+dir*arrowpts[0].y*r->height/100);
                    scroll_DrawLineTo(self, offx+r->left+arrowpts[1].x*r->width/100, offy+r->top+(dir<0?r->height:0)+dir*arrowpts[1].y*r->height/100);
                    scroll_DrawLineTo(self, offx+r->left+arrowpts[2].x*r->width/100, offy+r->top+(dir<0?r->height:0)+dir*arrowpts[2].y*r->height/100);
                    break;
                case scroll__TOP:
                case scroll__BOTTOM:
                    t=offx;
                    offx=offy;
                    offy=t;
                    scroll_MoveTo(self, offx+r->left+(dir<0?r->width:0)+dir*arrowpts[0].y*r->width/100, offy+r->top+arrowpts[0].x*r->height/100);
                    scroll_DrawLineTo(self, offx+r->left+(dir<0?r->width:0)+dir*arrowpts[1].y*r->width/100, offy+r->top+arrowpts[1].x*r->height/100);
                    scroll_DrawLineTo(self, offx+r->left+(dir<0?r->width:0)+dir*arrowpts[2].y*r->width/100, offy+r->top+arrowpts[2].x*r->height/100);
                    break;
            }
        }
        scroll_SetTransferMode(self, graphic_SOURCE);
    }
    else { /* not emulating motif */
        thebutton.prefs=self->buttonprefs;
        thebutton.lit=lit;
        sbuttonv_SafeDrawButton(self, &thebutton, r);
        t.left = r->left + r->width/2 - 3;
        t.width = 6 + (r->width % 2);
        t.top = r->top + r->height/2 - 2;
        t.height = 4 + (r->height % 2);
        thebutton.lit = !lit;
        sbuttonv_SafeDrawButton(self, &thebutton, &t);
    }

    if (!self->drawborder) {
        /* reuse t to calc gray area next to button */
        t = *r;
        switch (side) {
            case scroll__LEFT:
            case scroll__RIGHT:
                t.height = self->buttonSpace;
                if (dir == 1) {
                    t.top += r->height;
                }
                else {
                    t.top -= self->buttonSpace;
                }
                break;
            case scroll__TOP:
            case scroll__BOTTOM:
                t.width = self->buttonSpace;
                if (dir == 1) {
                    t.left += r->width;
                }
                else {
                    t.left -= self->buttonSpace;
                }
                break;
        }

        scroll_GetFGColor(self, oldfg, oldfg+1, oldfg+2);
        scroll_SetFGColor(self, self->mattebackground[0], self->mattebackground[1], self->mattebackground[2]);
        scroll_SetTransferMode(self, graphic_SOURCE);
        if (t.width > 0 && t.height > 0) {
            scroll_FillRect(self, &t, NULL);
        }
        scroll_SetFGColor(self, oldfg[0], oldfg[1], oldfg[2]);
    }
}

/* Creation and Destruction routines. */

boolean scroll__InitializeClass(classID)
struct classheader *classID;
{
 
    return TRUE;
}

static char scrollbar[]="scroll";
static char scrollbarmatte[]="scrollmatte";
static char scrollbarbox[]="scrollbar";
static char scrollbarbutton[]="scrollbutton";
static char scrollbarelevator[]="scrollelevator";
static char scrollbardot[]="scrolldot";

boolean scroll__InitializeObject(classID, self)
struct classheader *classID;
struct scroll *self;
{
    int i;

    self->prefsready = FALSE;

    self->emulation = environ_GetProfileSwitch("MotifScrollbars", FALSE);
    self->startScrollTime = environ_GetProfileInt("StartScrollTime", 0);
    self->minContScrollTime = environ_GetProfileInt("ContScrollTime", 500);
    self->maxContScrollTime = environ_GetProfileInt("MaxContScrollTime", self->minContScrollTime);
    self->endzonereptime = environ_GetProfileInt("ButtonRepeatTime", 100);
    self->adjustScroll = environ_GetProfileSwitch("AdjustScroll", FALSE);
    self->thumbScroll = environ_GetProfileSwitch("ThumbScroll", FALSE);

    self->side=(-1);
    self->lastwidth=(-1);
    self->lastheight=(-1);
    self->dir=scroll_NOWHERE;
    
    self->current.location = 0;
    self->child = self->scrollee = NULL;
    for (i = 0; i < scroll_TYPES; i++) {
        struct scrollbar *bar = &self->current.bar[i];
        bar->total.beg = 0;
        bar->total.end = 1;
        bar->seen.beg = bar->seen.end = bar->dot.beg = bar->dot.end = -1;
	self->fns[i] = NULL;
	self->barcursor[i]=cursor_Create((struct view *)self);
	if(self->barcursor[i]!=NULL) cursor_SetStandard(self->barcursor[i], cursortypes[i]);
    }

    self->desired = self->current;

    self->pending_update = 0;
    
    self->updatelist = updatelist_New();
    if(self->updatelist==NULL) return FALSE;
    
    self->ideal_location = scroll_LEFT;
    
    self->endzone_threshold = 80;
    self->bar_threshold = 0;
    self->endbarSpace = ENDTOBARSPACE;

    self->min_elevator[0] = 5;
    self->min_elevator[1] = 18;
    self->force_full_update = FALSE;
    self->force_get_interface = FALSE;
    self->scrollEvent = NULL;

    if(self->elevatorWidth<=0) self->elevatorWidth=1;
    if(self->dotWidth<=0) self->dotWidth=1;
    if(self->barWidth<4) self->barWidth=4;
    
    self->barbackground[0] = self->barbackground[1] = self->barbackground[2] = -1.0;

    self->cursor = cursor_Create((struct view *)self);

    if(self->cursor!=NULL) cursor_SetStandard(self->cursor, Cursor_Octagon);

    self->mousestate=scroll_NOTHING;
    self->prefs = self->boxprefs = self->matteprefs = self->buttonprefs = self->elevatorprefs = self->dotprefs = NULL;
    return TRUE;
}
    
#define sfree(x)  do { if(x) sbutton_FreePrefs(x); x=NULL; } while (0)

void scroll__FinalizeObject(classID, self)
struct classheader *classID;
struct scroll *self;
{
    int i;
    if (self->child != NULL)
        view_UnlinkTree(self->child);

    if(self->updatelist) updatelist_Destroy(self->updatelist);
    
    if(self->cursor) cursor_Destroy(self->cursor);

    for(i=0;i<scroll_TYPES;i++) if(self->barcursor[i]!=NULL) cursor_Destroy(self->barcursor[i]);
    
    sfree(self->prefs);
    sfree(self->boxprefs);
    sfree(self->matteprefs);
    sfree(self->buttonprefs);
    sfree(self->elevatorprefs);
    sfree(self->dotprefs);
}

struct scroll *scroll__CreateScroller(classID, scrollee, location, name)
struct classheader *classID;
struct view *scrollee;
int location;
char *name;

{
    struct classinfo *oscroll;
    
    struct scroll *retval=NULL;

    if(name==NULL) name=environ_GetProfile("ScrollClass");
    
    if(name!=NULL && (oscroll=class_Load(name))!=NULL && class_IsType(oscroll, &scroll_classinfo)) retval= (struct scroll *)class_NewObject(name);
    
    if(retval==NULL) {
	retval=scroll_New();
	if(retval==NULL) return NULL;
    }

    scroll_SetView(retval, scrollee);
    scroll_SetLocation(retval, location);

    return retval;
}

struct scroll *scroll__Create(classID, scrollee, location)
struct classheader *classID;
struct view *scrollee;
int location;
{
    return scroll__CreateScroller(classID, scrollee, location, NULL);
}


/* State modification routines. */

void scroll__SetView(self, view)
struct scroll *self;
struct view *view;
{

    scroll_SetChild(self, view);
    scroll_SetScrollee(self, view);
}

void scroll__SetChild(self, child)
struct scroll *self;
struct view *child;
{
    if (self->child != child) {
        self->force_full_update = TRUE;
        if (self->child)
            view_UnlinkTree(self->child);
        self->child = child;
        if (child)
            view_LinkTree(child, self);
        scroll_WantUpdate(self, self);
    }
}

void scroll__SetScrollee(self, scrollee)
struct scroll *self;
struct view *scrollee;
{
    if (self->scrollee != scrollee) {
        self->force_get_interface = TRUE;
        self->scrollee = scrollee;
        scroll_WantUpdate(self, self);
    }
}

struct view *scroll__GetChild(self)
struct scroll *self;
{
    return self->child;
}

struct view *scroll__GetScrollee(self)
struct scroll *self;
{
    return self->scrollee;
}

void scroll__SetLocation(self, location)
struct scroll *self;
int location;
{
    self->ideal_location = location;
    scroll_WantUpdate(self, self);  
}

int scroll__GetLocation(self)
struct scroll *self;
{
    return self->ideal_location;
}

int scroll__GetCurrentLocation(self)
struct scroll *self;
{
    return self->current.location;
}

void scroll__SetParameters(self, endzone, bar, without, with)
struct scroll *self;
long endzone, bar;
int without, with;
{
    self->endzone_threshold = endzone;
    self->bar_threshold = bar;
    self->min_elevator[0] = without;
    self->min_elevator[1] = with;
    scroll_WantUpdate(self, self);
}

void scroll__GetParameters(self, endzone, bar, without, with)
struct scroll *self;
long *endzone, *bar;
int *without, *with;
{
    *endzone = self->endzone_threshold;
    *bar = self->bar_threshold;
    *without = self->min_elevator[0];
    *with = self->min_elevator[1];
}

void scroll__SetWidth(self, newWidth)
struct scroll *self;
long newWidth;
{
    self->barWidth = newWidth;
    
    if (self->dotWidth > newWidth - 2) {
	scroll_SetDotWidth(self, newWidth - 2);
    }
    scroll_WantUpdate(self, self);
}

long scroll__GetWidth(self)
struct scroll *self;
{
    return self->barWidth;
}

void scroll__SetDotWidth(self, newWidth)
struct scroll *self;
long newWidth;
{
    if (newWidth > self->barWidth - 2) {
	newWidth = self->barWidth - 2;
    }
    if (newWidth < 0) {
	newWidth = 0;
    }
    self->dotWidth = newWidth;
    scroll_WantUpdate(self, self);
}

long scroll__GetDotWidth(self)
struct scroll *self;
{
    return self->dotWidth;
}


void scroll__SetElevatorWidth(self, newWidth)
struct scroll *self;
long newWidth;
{
    if(newWidth > self->barWidth) newWidth=self->barWidth;

    if(newWidth<0) newWidth=0;

    self->elevatorWidth=newWidth;
    scroll_WantUpdate(self, self);
}

long scroll__GetElevatorWidth(self)
struct scroll *self;
{
    return self->elevatorWidth;
}

void scroll__SetWindowPadding(self, newPadding)
struct scroll *self;
long newPadding;
{
    self->windowPadding=newPadding;
}

void scroll__SetViewPadding(self, newPadding)
struct scroll *self;
long newPadding;
{
    self->viewPadding=newPadding;
}

long scroll__GetWindowPadding(self)
struct scroll *self;
{
    return self->windowPadding;
}

long scroll__GetViewPadding(self)
struct scroll *self;
{
    return self->viewPadding;
}

void scroll__SetEndZoneLength(self, newLength)
struct scroll *self;
long newLength;
{
    self->endzoneLength = newLength;
    if ((self->endzoneLength + self->endbarSpace + self->buttonSpace) * 2 > self->endzone_threshold) {
	self->endzone_threshold = (self->endzoneLength + self->endbarSpace + self->buttonSpace) * 2;
    }
    scroll_WantUpdate(self, self);
}

long scroll__GetEndZoneLength(self)
struct scroll *self;
{
    return self->endzoneLength;
}

void scroll__SetEndToBarSpace(self, newSpace)
struct scroll *self;
long newSpace;
{
    self->endbarSpace = newSpace;
    if ((self->endzoneLength + self->endbarSpace + self->buttonSpace) * 2 > self->endzone_threshold) {
	self->endzone_threshold = (self->endzoneLength + self->endbarSpace + self->buttonSpace) * 2;
    }
    scroll_WantUpdate(self, self);
}


long scroll__GetEndToBarSpace(self)
struct scroll *self;
{
    return self->endbarSpace;
}

/* Interface routines. */

static void get_interface(self, type)
struct scroll *self;
int type;
{
    self->force_get_interface = FALSE;
    if (self->fns[type] == NULL)
        self->fns[type] = (struct scrollfns *)view_GetInterface(self->scrollee, InterfaceName[type]);
}

static void getinfo(self, type, total, seen, dot)
struct scroll *self;
int type;
struct range *total, *seen, *dot;
{
    void (*real_getinfo)();

    get_interface(self, type);

    if (self->fns[type] != NULL && (real_getinfo = self->fns[type]->GetInfo) != NULL)
        real_getinfo(self->scrollee, total, seen, dot);

    if (total->beg == total->end) {
        total->end++;
        *seen = *total;
        dot->beg = dot->end = total->beg;
    }
}

/* Calculation routines. */

static long bar_height(self, side)
struct scroll *self;
int side;
{
    switch(side) {
	case scroll__LEFT:
	case scroll__RIGHT:
	    return self->interiors[side].height;
	case scroll__TOP:
	case scroll__BOTTOM:
	    return self->interiors[side].width;
    }      
}

static void set_frame(self, side, posn, coord)
struct scroll *self;
int side;
int posn;
long coord;
{
    void (*real_setframe)();
    int type = Type[side];

    get_interface(self, type);

    if (self->fns[type] != NULL && (real_setframe = self->fns[type]->SetFrame) != NULL)
	real_setframe(self->scrollee, posn, coord, CHILDBARHEIGHT(self, side));
}

static void endzone(self, side, end, action)
struct scroll *self;
int side;
int end;
enum view_MouseAction action;
{
    void (*real_endzone)();
    int type = Type[side];
    int typedEnd;

    get_interface(self, type);

    if (self->fns[type] != NULL && (real_endzone = self->fns[type]->EndZone) != NULL) {
        if (self->emulation) {
            typedEnd = (end == scroll_TOPENDZONE) ? scroll_MOTIFTOPENDZONE : scroll_MOTIFBOTTOMENDZONE;
        }
    else {
            typedEnd = end;
        }
        real_endzone(self->scrollee, typedEnd, action);
    }
    else {
	if(action==view_LeftDown || action == view_RightDown) {
	    if (end == scroll_TOPENDZONE)
		set_frame(self, side, self->desired.bar[type].total.beg, 0);
	    else
		set_frame(self, side, self->desired.bar[type].total.end, CHILDBARHEIGHT(self, side) >> 2);
	}
    }
}

static int what_is_at(self, side, coord)
struct scroll *self;
int side;
int coord;
{
    long (*real_what)();
    int type = Type[side];

    get_interface(self, type);

    if (self->fns[type] != NULL && (real_what = self->fns[type]->WhatIsAt) != NULL)
        return real_what(self->scrollee, coord, CHILDBARHEIGHT(self, side));
    else
        return 0;
}


static boolean barrects(self, side, boxrect, topbuttonrect, botbuttonrect)
struct scroll *self;
int side;
struct rectangle *boxrect, *topbuttonrect, *botbuttonrect;
{
    long x1, x2, y1, y2;
    long tx1, tx2, ty1, ty2;
    long bx1, bx2, by1, by2;
    long w, h;
    int c=2;
    long tezl=self->endzoneLength + self->buttonSpace;
    boolean result=TRUE;
    long height;
    
    while(c--) {
	switch(side) {
	    case scroll__LEFT:
	    case scroll__RIGHT:
		y1=self->childrect.top + tezl;
		y2=self->childrect.top + self->childrect.height - 1 - tezl;
		height= (y2 - y1 + 1) + 2 * tezl;
		if(height < self->bar_threshold) result=FALSE;
		
		ty1=self->childrect.top;
		ty2=ty1 + self->endzoneLength - 1;
		by2=self->childrect.top + self->childrect.height - 1;
		by1=by2 - self->endzoneLength + 1;
		break;
	    case scroll__TOP:
	    case scroll__BOTTOM:
		x1= self->childrect.left + tezl;
		x2= self->childrect.left + self->childrect.width - 1 - tezl;
		height= (x2 - x1 + 1) + 2 * tezl;
		if(height < self->bar_threshold) result=FALSE;
		tx1=self->childrect.left;
		tx2=tx1 + self->endzoneLength -1;
		bx2=self->childrect.left + self->childrect.width - 1;
		bx1=bx2 - self->endzoneLength + 1;
		break;
	}
	if(self->child) {
	    switch(side) {
		case scroll__LEFT:
		    x1=bx1=tx1=self->fullinterior.left;
		    x2=bx2=tx2=tx1 + REALBARWIDTH(self) - 1;
		    break;
		case scroll__RIGHT:
		    x2=bx2=tx2=self->fullinterior.left + self->fullinterior.width - 1;
		    x1=bx1=tx1=tx2 - REALBARWIDTH(self) + 1;
		    break;
		case scroll__TOP:
		    y1=by1=ty1=self->fullinterior.top;
		    y2=by2=ty2=ty1 + REALBARWIDTH(self) - 1;
		    break;
		case scroll__BOTTOM:
		    y2=by2=ty2=self->fullinterior.top + self->fullinterior.height - 1;
		    y1=by1=ty1=ty2 - REALBARWIDTH(self) + 1;
	    }
	} else {
	    switch(side) {
		case scroll__LEFT:
		case scroll__RIGHT:
		    {
		    int mid=self->fullinterior.left + self->fullinterior.width / 2;
		    x2=bx2=tx2=mid + REALBARWIDTH(self) / 2;
		    x1=bx1=tx1= x2 - REALBARWIDTH(self)  + (REALBARWIDTH(self)&1);
		    }
		    break;
		case scroll__TOP:
		case scroll__BOTTOM:
		    {
		    int mid=self->fullinterior.top + self->fullinterior.height / 2;
		    y2=by2=ty2=mid + REALBARWIDTH(self) / 2;
		    y1=by1=ty1=y2 - REALBARWIDTH(self)  + (REALBARWIDTH(self)&1);
		    }
		    break;
	    }

	}

	if(boxrect) {
	    w=ABS(x1-x2)+1;
	    h=ABS(y1-y2)+1;
	    x1=MIN(x1,x2);
	    y1=MIN(y1,y2);
	    boxrect->left=x1;
	    boxrect->top=y1;
	    boxrect->width=w;
	    boxrect->height=h;
	}
	
	if(height>=self->endzone_threshold && c!=0) {
	    if(topbuttonrect) {
		w=ABS(tx1-tx2)+1;
		h=ABS(ty1-ty2)+1;
		tx1=MIN(tx1,tx2);
		ty1=MIN(ty1,ty2);
		topbuttonrect->left=tx1;
		topbuttonrect->top=ty1;
		topbuttonrect->width=w;
		topbuttonrect->height=h;
	    }

	    if(botbuttonrect) {
		w=ABS(bx1-bx2)+1;
		h=ABS(by1-by2)+1;
		bx1=MIN(bx1,bx2);
		by1=MIN(by1,by2);
		botbuttonrect->left=bx1;
		botbuttonrect->top=by1;
		botbuttonrect->width=w;
		botbuttonrect->height=h;
	    }
	    self->desired.bar[Type[side]].endzones=TRUE;
	    return result;
	} else {
	    self->desired.bar[Type[side]].endzones=FALSE;
	    if(topbuttonrect) topbuttonrect->width=0;
	    if(botbuttonrect) botbuttonrect->width=0;
	    tezl=self->buttonSpace;
	}
    }
    return result;
}


static void compute_inner(self, draw)
struct scroll *self;
boolean draw;
{
    int i;
    int diff;
    int lastlocation;
    struct rectangle r;
    scroll_GetLogicalBounds(self, &r);

    self->desired.location=self->ideal_location;
    diff=REALBARWIDTH(self) + VPADDING(self);
    do {
        lastlocation=self->desired.location;

        self->childrect=r;
	if (self->drawborder) {
            /* around the entire window */
            sbuttonv_DrawRectBorder(self, &self->childrect, self->matteprefs, sbuttonv_BORDEROUT, draw, &self->childrect);

	    self->childrect.left+=WPADDING(self);
	    self->childrect.top+=WPADDING(self);
	    self->childrect.width-=WPADDING(self)*2;
	    self->childrect.height-=WPADDING(self)*2;
	    
            self->fullinterior = self->childrect;

	    if(self->child) for (i = 0; i < scroll_SIDES; i++) {
                if (self->desired.location & (1<<i)) {
                    switch(i) {
                        case scroll__LEFT:
                            self->childrect.left+=diff;
                            self->childrect.width-=diff;
                            break;
                        case scroll__RIGHT:
                            self->childrect.width-=diff;
                            break;
                        case scroll__TOP:
                            self->childrect.top+=diff;
                            self->childrect.height-=diff;
                            break;
                        case scroll__BOTTOM:
                            self->childrect.height-=diff;
                            break;
                    }
                }
            }
            if (self->child && self->childrect.width>0 && self->childrect.height>0) {
                /* around the child's portion */
                sbuttonv_DrawRectBorder(self, &self->childrect, self->matteprefs, sbuttonv_BORDERIN, draw, &self->childrect);
            }
	}
        else {
            self->fullinterior = r;

            for (i = 0; i < scroll_SIDES; i++) {
                if (self->desired.location & (1<<i)) {
                    switch(i) {
                        case scroll__LEFT:
                            self->childrect.left+=diff;
                            self->childrect.width-=diff;
                            break;
                        case scroll__RIGHT:
                            self->childrect.width-=diff;
                            break;
                        case scroll__TOP:
                            self->childrect.top+=diff;
                            self->childrect.height-=diff;
                            break;
                        case scroll__BOTTOM:
                            self->childrect.height-=diff;
                            break;
                    }
                }
            }

            for (i = 0; i < scroll_SIDES; i++) {
                int x1, y1, x2, y2;

                if (self->desired.location & (1<<i)) {
                    switch(i) {
                        case scroll__LEFT:
                            x1 = self->childrect.left - 1;
                            y1 = self->childrect.top;
                            x2 = x1;
                            y2 = y1 + self->childrect.height - 1;
                            break;
                        case scroll__RIGHT:
                            x1 = self->childrect.left + self->childrect.width;
                            y1 = self->childrect.top;
                            x2 = x1;
                            y2 = y1 + self->childrect.height - 1;
                            break;
                        case scroll__TOP:
                            x1 = self->childrect.left;
                            y1 = self->childrect.top - 1;
                            x2 = x1 + self->childrect.width - 1;
                            y2 = y1;
                            break;
                        case scroll__BOTTOM:
                            x1 = self->childrect.left;
                            y1 = self->childrect.top + self->childrect.height;
                            x2 = x1 + self->childrect.width - 1;
                            y2 = y1;
                            break;
                    }
		    if(self->child && draw) {
			scroll_MoveTo(self, x1, y1);
			scroll_DrawLineTo(self, x2, y2);
		    }
                }
            }
	}

	if(self->child) for (i = 0; i < scroll_SIDES; i++)
	    if (self->desired.location & (1<<i)) {
		if(!barrects(self, i, NULL, NULL, NULL)) self->desired.location &= ~(1<<i);
	    }

	if((self->childrect.height<=0 || self->childrect.width<=0) && self->child!=NULL) self->desired.location=0;
    } while (lastlocation != self->desired.location);
}


static void calc_desired(self)
struct scroll *self;
{
    int i, exists[scroll_TYPES];

    for (i = 0; i < scroll_TYPES; i++) 
        exists[i] = 0;

    for (i = 0; i < scroll_SIDES; i++) 
        if (self->desired.location & (1<<i)) 
            exists[Type[i]] = 1;

    for (i = 0; i < scroll_TYPES; i++) 
        if (exists[i]) {
            struct scrollbar *bar = &self->desired.bar[i];
	    getinfo(self, i, &bar->total, &bar->seen, &bar->dot);
        }
}



static long from_range_to_bar(self, side, bar, posn)
struct scroll *self;
int side;
struct scrollbar *bar;
long posn;
{
    long cords = bar_height(self, side) - 2 * self->endbarSpace;
    long retval;

    /* chosen because 1M x 2k pixels will just about overflow in 32 bits */
    if (bar->total.end < 1000000) {
        retval = self->endbarSpace + ((posn - bar->total.beg) * cords + (bar->total.end - bar->total.beg) / 2) / (bar->total.end - bar->total.beg);
    }
    else {
        retval = self->endbarSpace + ((long)(((double)(posn - bar->total.beg)) * (double)cords / ((double)(bar->total.end - bar->total.beg)) + .5));
    }
    return retval;
}

static long from_bar_to_range(self, side, bar, posn)
struct scroll *self;
int side;
struct scrollbar *bar;
long posn;
{
    long height =  bar_height(self, side),
    cords = height - 2*self->endbarSpace,
    retval;

    if (bar->total.end < 1000000) {
	retval = bar->total.beg + (((bar->total.end - bar->total.beg) * (posn - self->endbarSpace) + cords / 2) / cords);
    }
    else {
	retval = ((long) ((double)(bar->total.beg) +
			  ((((double)(bar->total.end - bar->total.beg)) *
			    ((double)(posn - self->endbarSpace))) / (double)cords)
			  + .5));
    }
    return retval;
}


static boolean calc_elevator(self, side, bar, r)
struct scroll *self;
int side;
struct scrollbar *bar;
struct rectangle *r;
{
    long min, height=bar_height(self, side) - self->endbarSpace;
    int diff;
    long x1, x2, y1, y2;
    
    if (bar->seen.end < bar->seen.beg || bar->seen.end < bar->total.beg || bar->seen.beg > bar->total.end) {
	r->left=r->top=r->width=r->height=(-1);
	return FALSE;
    }

    y1=from_range_to_bar(self, side, bar, bar->seen.beg);
    y2=from_range_to_bar(self, side, bar, bar->seen.end);

    if (bar->endzones)
	min = self->min_elevator[1];
    else
	min = self->min_elevator[0];

    if (min > height) min = height;

    if (y2 - y1 < min) {
	y1 = (y1 + y2 - min) / 2;
	if(y1<self->endbarSpace) y1=self->endbarSpace;
	if (y1 + min >= height) {
	    y2 = height  - 1;
	    y1 = y2 - min + 1;
	} else y2 = y1 + min - 1;
    }
    
    switch(side) {
	case scroll__LEFT:
	case scroll__RIGHT:
	    y1+=self->interiors[side].top;
	    y2+=self->interiors[side].top;
	    x1=self->interiors[side].left;
	    x2=self->interiors[side].left + self->interiors[side].width - 1;
	    diff=self->interiors[side].width - self->elevatorWidth;
	    /* the -2 is so that the elevator can overlap
	     the enclosing box by one pixel */
	    if(diff>=-2) {
		x1+=diff/2;
		x2-=diff/2;
	    }
	    break;
	case scroll__TOP:
	case scroll__BOTTOM:
	    x1= y1 + self->interiors[side].left;
	    x2= y2 + self->interiors[side].left;
	    y1=self->interiors[side].top;
	    y2=self->interiors[side].top + self->interiors[side].height - 1;
	    diff=self->interiors[side].height - self->elevatorWidth;
	    /* the -2 is so that the elevator can overlap
	     the enclosing box by one pixel */
	    if(diff>=-2) {
		y1+=diff/2;
		y2-=diff/2;
	    }
	    break;
    }
    r->left=MIN(x1, x2);
    r->top=MIN(y1, y2);
    r->width=ABS(x1-x2)+1;
    r->height=ABS(y1-y2)+1;
    return TRUE;
}

static boolean calc_dot(self, side, bar, r)
struct scroll *self;
int side;
struct scrollbar *bar;
struct rectangle *r;
{
    int diff;

    long x1, y1, x2, y2;
    
    if (bar->dot.end < bar->dot.beg || bar->dot.end < bar->total.beg || bar->dot.beg > bar->total.end) {
	r->left=r->top=r->width=r->height=(-1);
	return FALSE;
    }

    y1 = from_range_to_bar(self, side, bar, bar->dot.beg);
    y2 = from_range_to_bar(self, side, bar, bar->dot.end);

    if (bar->dot.beg == bar->dot.end) {
	y1 -= 1;
	y2 += 2;
    }
    if (y2 - y1 < MINDOTLENGTH) {
	y1 = (y1 + y2 - MINDOTLENGTH) / 2;
	y2 = (y1 + y2 + MINDOTLENGTH) / 2;
    }

    switch(side) {
	case scroll__LEFT:
	case scroll__RIGHT:
	    y1+=self->interiors[side].top;
	    y2+=self->interiors[side].top;
	    x1=self->interiors[side].left;
	    x2=self->interiors[side].left + self->interiors[side].width - 1;
	    diff=self->interiors[side].width - self->dotWidth;
	    if(diff>0) {
		x1+=diff/2;
		x2-=diff/2;
	    }
	    break;
	case scroll__TOP:
	case scroll__BOTTOM:
	    x1= y1 + self->interiors[side].left;
	    x2= y2 + self->interiors[side].left;
	    y1=self->interiors[side].top;
	    y2=self->interiors[side].top + self->interiors[side].height - 1;
	    diff=self->interiors[side].height - self->dotWidth;
	    if(diff>0) {
		y1+=diff/2;
		y2-=diff/2;
	    }
	    break;
    }
    r->left=MIN(x1, x2);
    r->top=MIN(y1, y2);
    r->width=ABS(x1-x2)+1;
    r->height=ABS(y1-y2)+1;
    return TRUE;
}

static void draw_elevator(self, side)
struct scroll *self;
int side;
{
    struct sbuttonv_view_info vi;
    struct rectangle r;
    
    if(calc_elevator(self, side, &self->desired.bar[Type[side]], &r)) {
	sbuttonv_SaveViewState(self, &vi);
	sbuttonv_DrawBorder(self, r.left, r.top, r.width, r.height, self->elevatorprefs, sbuttonv_BORDEROUT, TRUE, NULL);
	sbuttonv_RestoreViewState(self, &vi);
    }
}

static void draw_dot(self, side)
struct scroll *self;
int side;
{
    struct sbuttonv_view_info vi;
    struct rectangle r;
    struct scrollbar *bar= &self->desired.bar[Type[side]];

    if(calc_dot(self, side, bar, &r)) {
        sbuttonv_SaveViewState(self, &vi);
        sbuttonv_DrawBorder(self, r.left, r.top, r.width, r.height, self->dotprefs, sbuttonv_BORDERIN, TRUE, NULL);
        sbuttonv_RestoreViewState(self, &vi);
    }
}

static void move_elevator(self, side)
struct scroll *self;
int side;
{
    struct rectangle ner, oer, ndr, odr, r1, r2;

    boolean eisvisible;
    boolean disvisible;
    
    boolean redraw_elevator=FALSE;
    boolean redraw_dot=FALSE;
    boolean intersect=FALSE;
    
    double oldfg[3];
    
    eisvisible= calc_elevator(self, side, &self->desired.bar[Type[side]], &ner);
    (void)calc_elevator(self, side, &self->current.bar[Type[side]], &oer);
    
    disvisible= calc_dot(self, side, &self->desired.bar[Type[side]], &ndr);
    (void)calc_dot(self, side, &self->current.bar[Type[side]], &odr);
    
    scroll_GetFGColor(self, oldfg, oldfg+1, oldfg+2);
    
    scroll_SetFGColor(self, self->barbackground[0], self->barbackground[1], self->barbackground[2]);
    
     scroll_SetTransferMode(self, graphic_SOURCE);
     
    rectangle_IntersectRect(&r1, &odr, &oer);
    rectangle_IntersectRect(&r2, &odr, &ner);
    if(!rectangle_IsEmptyRect(&r1) || !rectangle_IsEmptyRect(&r2)) intersect=TRUE;
    
    if(odr.left!=ndr.left || odr.top!=ndr.top || odr.width!=ndr.width || odr.height!=ndr.height) {
	scroll_FillRect(self, &odr, NULL);
	redraw_dot=TRUE;
	if(intersect) redraw_elevator=TRUE;	    
    }
    if(oer.left!=ner.left || oer.top!=ner.top || oer.width!=ner.width || oer.height!=ner.height) {
	redraw_elevator=TRUE;
	if(intersect) redraw_dot=TRUE;

	rectangle_IntersectRect(&r1, &oer, &ner);
	if(rectangle_IsEmptyRect(&r1)) scroll_FillRect(self, &oer, NULL);
	else switch(side) {
	    case scroll__LEFT:
	    case scroll__RIGHT:
		if(oer.top<ner.top) scroll_FillRectSize(self, oer.left, oer.top, oer.width, ner.top-oer.top+1, NULL);
		if(oer.top+oer.height>ner.top+ner.height) scroll_FillRectSize(self, oer.left, ner.top+ner.height, oer.width, oer.top+oer.height-ner.top-ner.height, NULL);
		break;
	    case scroll__TOP:
	    case scroll__BOTTOM:
		if(oer.left<ner.left) scroll_FillRectSize(self, oer.left, oer.top, ner.left-oer.left+1, oer.height, NULL);
		if(oer.left+oer.width>ner.left+ner.width) scroll_FillRectSize(self, ner.left+ner.width, oer.top, oer.left+oer.width-ner.left-ner.width, oer.height, NULL);
		break;
	}
    }
    scroll_SetFGColor(self, oldfg[0], oldfg[1], oldfg[2]);
    if(redraw_elevator && eisvisible) draw_elevator(self, side);
    if(redraw_dot && disvisible) draw_dot(self, side);
}


static void draw_bar(self, side)
struct scroll *self;
int side;
{
    struct sbuttonv_view_info vi;
    barrects(self, side, &self->interiors[side], &self->topbutton[side], &self->botbutton[side]);

    sbuttonv_SaveViewState(self, &vi);
    sbuttonv_DrawBorder(self, self->interiors[side].left, self->interiors[side].top, self->interiors[side].width, self->interiors[side].height, self->boxprefs, sbuttonv_BORDERIN, TRUE, &self->interiors[side]);

    sbuttonv_RestoreViewState(self, &vi);
    draw_elevator(self, side);
    draw_dot(self, side);
    if(self->desired.bar[Type[side]].endzones) {
	draw_arrow(self, side, &self->topbutton[side], 1, self->mousestate==scroll_TOPENDZONE && self->side==side);
	draw_arrow(self, side, &self->botbutton[side], -1, self->mousestate==scroll_BOTTOMENDZONE && self->side==side);
    }
}


static void draw_everything(self)
struct scroll *self;
{
    int i;
    struct rectangle r;
    struct sbuttonv_view_info vi;
    struct region *re1, *re2;

    if(self->child) {
	
	scroll_GetLogicalBounds(self, &r);
	
	re1=region_CreateEmptyRegion();
	re2=region_CreateEmptyRegion();

	if(re1==NULL || re2==NULL) {
	    if(re1!=NULL) region_Destroy(re1);
	    if(re2!=NULL) region_Destroy(re2);
	    return;
	}

	region_RectRegion(re1, &r);

	region_RectRegion(re2, &self->childrect);

	region_SubtractRegion(re1, re2, re2);

	scroll_SetClippingRegion(self, re2);
	
	region_Destroy(re1);
	region_Destroy(re2);
    }
    
    sbuttonv_SaveViewState(self, &vi);

    compute_inner(self, TRUE);

    sbuttonv_RestoreViewState(self, &vi);

    for (i = 0; i < scroll_SIDES; i++)
	if (self->desired.location & (1<<i)) {
	    draw_bar(self, i);
	}
}

static void InitPrefs(self)
struct scroll *self;
{
    boolean graphicIsMono;
    boolean mono;

    graphicIsMono = graphic_DisplayClass(scroll_GetDrawable(self)) & graphic_Monochrome;
    mono = environ_GetProfileSwitch("MimicOldScrollbar", graphicIsMono ? TRUE : FALSE);

    self->drawborder = environ_GetProfileSwitch("ScrollDrawBorders", mono ? MONODRAWBORDERS : COLORDRAWBORDERS);
    self->prefs=sbutton_GetNewPrefs(scrollbar);
    if(self->prefs==NULL) return;
    if (mono) {
	sbutton_GetStyle(self->prefs) = MONOSTYLE;
    } 
    sbutton_InitPrefs(self->prefs, scrollbar);

    self->matteprefs=sbutton_DuplicatePrefs(self->prefs, scrollbarmatte);
    if(self->matteprefs==NULL) return;
    sbutton_InitPrefs(self->matteprefs, scrollbarmatte);

    self->boxprefs=sbutton_DuplicatePrefs(self->prefs, scrollbarbox);
    if(self->boxprefs==NULL) return;
    if (mono) {
	sbutton_GetTop(self->boxprefs) = MONOBARTOP;
	sbutton_GetForeground(self->boxprefs) = MONOBARFOREGROUND;
    }
    sbutton_InitPrefs(self->boxprefs, scrollbarbox);

    self->buttonprefs= sbutton_DuplicatePrefs(self->prefs, scrollbarbutton);
    if(self->buttonprefs==NULL) return;
    if (mono) {
	sbutton_GetStyle(self->buttonprefs) = MONOBUTTONSTYLE;
	sbutton_GetTop(self->buttonprefs) = MONOBUTTONTOP;
	sbutton_GetTopShadow(self->buttonprefs) = MONOBUTTONTOPSHADOW;
	sbutton_GetBottomShadow(self->buttonprefs) = MONOBUTTONBOTTOMSHADOW;
    }
    sbutton_InitPrefs(self->buttonprefs, scrollbarbutton);

    self->elevatorprefs= sbutton_DuplicatePrefs(self->prefs, scrollbarelevator);
    if(self->elevatorprefs==NULL) return;
    sbutton_InitPrefs(self->elevatorprefs, scrollbarelevator);

    self->dotprefs= sbutton_DuplicatePrefs(self->prefs, scrollbardot);
    if(self->dotprefs==NULL) return;
    if (mono) {
	sbutton_GetStyle(self->dotprefs) = MONODOTSTYLE;
	sbutton_GetTop(self->dotprefs) = MONODOTTOP;
	sbutton_GetTopShadow(self->dotprefs) = MONODOTTOPSHADOW;
	sbutton_GetBottomShadow(self->dotprefs) = MONODOTBOTTOMSHADOW;
    }
    else {
	/* sbutton_GetBottomShadow(self->dotprefs) = COLORDOTBOTTOMSHADOW; */
    }
    sbutton_InitPrefs(self->dotprefs, scrollbardot);

    self->windowPadding = environ_GetProfileInt("ScrollWindowPadding", mono ? MONOWINDOWPADDING : COLORWINDOWPADDING);

    self->viewPadding = self->windowPadding + 1;

    self->barWidth = environ_GetProfileInt("ScrollBarWidth", mono ? MONOBARWIDTH : COLORBARWIDTH);
    self->dotWidth = environ_GetProfileInt("DotWidth", mono ? MONODOTWIDTH : COLORDOTWIDTH);

    self->elevatorWidth = environ_GetProfileInt("ElevatorWidth", mono ? MONOELEVATORWIDTH : COLORELEVATORWIDTH);

    self->endzoneLength = environ_GetProfileInt("ScrollButtonSize", mono ? MONOENDZONELENGTH : COLORENDZONELENGTH);

    self->buttonSpace = environ_GetProfileInt("ScrollButtonPadding", mono ? MONOBARTOBUTTONSPACE : COLORBARTOBUTTONSPACE);

    sbuttonv_InteriorBGColor(self, self->boxprefs, TRUE, self->barbackground);
    sbuttonv_InteriorBGColor(self, self->matteprefs, TRUE, self->mattebackground);

    self->prefsready=TRUE;
}

/* Overrides of the view routines: */

void scroll__FullUpdate(self, type, left, top, width, height)
struct scroll *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle r;
    int i;

    if(scroll_GetIM(self) && !self->prefsready) {
	    InitPrefs(self);
    }
    
    scroll_GetLogicalBounds(self, &r);
    
    self->pending_update = 0;
    
    if (self->force_get_interface) {
	for (i = 0; i < scroll_TYPES; i++) {
            self->fns[i] = NULL;
        }
    }

    if((type==view_FullRedraw || type==view_MoveNoRedraw ||
	(type!=view_Remove && (self->lastwidth!=r.width ||
		self->lastheight!=r.height))) && self->child) {
	self->lastwidth=r.width;
	self->lastheight=r.height;
	self->childrect=r;
	compute_inner(self, FALSE);
	view_InsertView(self->child, self, &self->childrect);
	
    }
    
    self->force_full_update = FALSE;
    switch(type) {
	case view_Remove:
	    for (i = 0; i < scroll_SIDES; i++)
	    if ((self->desired.location & (1<<i)) && self->barcursor[Type[i]]!=NULL) {
		scroll_RetractCursor(self, self->barcursor[Type[i]]);
	    }
	    if(self->child) view_FullUpdate(self->child, type, left, top, width, height);
	    return;
	case view_MoveNoRedraw:
	    if(self->child) view_FullUpdate(self->child, view_MoveNoRedraw, left, top, width, height);
	    break;
	case view_PartialRedraw:
	    if(self->child) view_FullUpdate(self->child, type, view_EnclosedXToLocalX(self->child, left),  view_EnclosedYToLocalY(self->child,top), width, height);
	    break;
	case view_LastPartialRedraw:
	    if(self->child) view_FullUpdate(self->child, type, view_EnclosedXToLocalX(self->child, left),  view_EnclosedYToLocalY(self->child,top), width, height);
	    calc_desired(self);
	    draw_everything(self);
	    break;
	case view_FullRedraw:
	    if(self->child) view_FullUpdate(self->child, view_FullRedraw, r.left, r.top, r.width, r.height);
	    calc_desired(self);
	    draw_everything(self);
	    break;
	default: ;
    }
    if(type!=view_Remove && type!=view_PartialRedraw) {
	for (i = 0; i < scroll_SIDES; i++)
	    if ((self->desired.location & (1<<i)) && self->barcursor[Type[i]]!=NULL) {
		scroll_RetractCursor(self, self->barcursor[Type[i]]);
		scroll_PostCursor(self, &self->interiors[i], self->barcursor[Type[i]]);
	    }
    }

    self->current = self->desired;
    scroll_FlushGraphics(self);
}


void scroll__Update(self)
struct scroll *self;
{
    int i;
    struct rectangle r;
    scroll_GetLogicalBounds(self, &r);

    if (r.width <= 0 || r.height <= 0) return;

    self->pending_update = 0;
    /* Let the children modify their state however they want. */
    updatelist_Clear(self->updatelist);
    calc_desired(self);

    if (self->current.location != self->desired.location || self->force_full_update) {
	scroll_EraseVisualRect(self);
	scroll_FullUpdate(self, view_FullRedraw, r.left, r.top, r.width, r.height);
    } else {
	for (i = 0; i < scroll_SIDES; i++)
	    if (self->desired.location & (1<<i)) {
		int type = Type[i];
		struct scrollbar *des = &self->desired.bar[type], *cur = &self->current.bar[type];
		if (des->endzones != cur->endzones)
		    /* The entire scrollbar need redrawing? */
		    draw_bar(self, i);
		else if (des->total.beg != cur->total.beg || des->total.end != cur->total.end || des->seen.beg != cur->seen.beg || des->dot.beg != cur->dot.beg || des->seen.end != cur->seen.end || des->dot.end != cur->dot.end) {
		    move_elevator(self, i);
		}
	    }
	self->current = self->desired;
    }
}

void scroll__WantUpdate(self, requestor)
struct scroll *self;
struct view *requestor;
{
    if ((struct view *)self != requestor)
        updatelist_AddTo(self->updatelist, requestor);

    if (!self->pending_update) {
        self->pending_update = 1;
        super_WantUpdate(self, self);
    }
}

static void DoRepeatScroll(self)
struct scroll *self;
{
    if(self->emulation) {
	switch(self->dir) {
	    case scroll_UP:
		set_frame(self, self->side, self->current.bar[Type[self->side]].seen.beg, CHILDBARHEIGHT(self, self->side)-1);
		break;
	    case scroll_DOWN:
		set_frame(self, self->side, self->current.bar[Type[self->side]].seen.end, 0);
	}	
    } else {
	if (self->lastaction==view_LeftDown)
	    set_frame(self, self->side, what_is_at(self, self->side, self->hitcoord), 0);
	else if (self->lastaction==view_RightDown)
	    set_frame(self, self->side, self->current.bar[Type[self->side]].seen.beg, self->hitcoord);
    }
}

static void RepeatScroll(self, cTime)
struct scroll *self;
long cTime;
{
    long timeInterval;

    self->scrollEvent=NULL;
    
    if(self->side == -1 || (self->mousestate!=scroll_MAYBETHUMBING && self->mousestate!=scroll_REPEATING)) {
	return;
    }

    self->mousestate=scroll_REPEATING;
    DoRepeatScroll(self);
    timeInterval = self->minContScrollTime + (((self->maxContScrollTime - self->minContScrollTime) * self->hitcoord) / (PIXELSPERINCH(self) * 10));

    self->scrollEvent = im_EnqueueEvent((procedure) RepeatScroll, (char *) self, event_MSECtoTU(timeInterval));
}

#define PTINRECT(r, x, y) ((x)>=(r)->left && (x)<(r)->left+(r)->width && (y)>=(r)->top && (y)<(r)->top+(r)->height)
#define ENDZONEREPTIME(self) (self->endzonereptime)

static void ScheduleRepeatEndZone();

static void RepeatEndZone(self, cTime)
struct scroll *self;
long cTime;
{
    self->scrollEvent=NULL;
    if(self->mousestate!=scroll_TOPENDZONE && self->mousestate!=scroll_BOTTOMENDZONE) return;
    endzone(self, self->side, self->mousestate, self->lastaction);
    ScheduleRepeatEndZone(self);
}

static void CancelScrollEvent(self)
struct scroll *self;
{
    if(self->scrollEvent) {
	event_Cancel(self->scrollEvent);
	self->scrollEvent=NULL;
    }
}

static void ScheduleRepeatEndZone(self)
struct scroll *self;
{
    CancelScrollEvent(self);
    self->scrollEvent = im_EnqueueEvent((procedure) RepeatEndZone, (char *) self, event_MSECtoTU(ENDZONEREPTIME(self)));
}

static void HandleEndZone(self, action, x, y)
struct scroll *self;
enum view_MouseAction action;
long x, y;
{

    struct rectangle *r;
    int dir;
    
    if(self->side==-1) return;
    
    /* if we're here self->mousestate==scroll_TOPENDZONE or self->mousestate==scroll_BOTTOMENDZONE and self->side is a valid side */
    
    if(self->mousestate==scroll_TOPENDZONE) {
	r = &self->topbutton[self->side];
	dir=1;
    } else {
	r = &self->botbutton[self->side];
	dir=(-1);
    }

    switch(action) {
	case view_LeftMovement:
	case view_RightMovement:
	    if(PTINRECT(r, x, y)) {
		ScheduleRepeatEndZone(self);
		if(!self->lastbuttonstate) draw_arrow(self, self->side, r, dir, self->lastbuttonstate=TRUE);

	    } else {
		CancelScrollEvent(self);
		if(self->lastbuttonstate) draw_arrow(self, self->side, r, dir, self->lastbuttonstate=FALSE);
	    }
	    break;
	case view_LeftUp:
	case view_RightUp:
	    /* the code in scroll_Hit will handle canceling the event */
	    if(self->lastbuttonstate) draw_arrow(self, self->side, r, dir, self->lastbuttonstate=FALSE);
    }
    endzone(self, self->side, self->mousestate, action);
}

static void HandleRepeatMode(self, action, x, y)
struct scroll *self;
enum view_MouseAction action;
long x, y;
{
    long coord;
    
    if(self->side==-1) return;

    
    coord=XYTOHITCOORD(self, self->side, x, y);
    if(self->adjustScroll && self->mousestate!=scroll_MAYBETHUMBING) self->hitcoord=coord;
    
    if(action!=view_LeftUp && action!=view_RightUp) return;

    DoRepeatScroll(self);
    
    im_ForceUpdate();
}

static boolean CheckEndZones(self, action, x, y)
struct scroll *self;
enum view_MouseAction action;
long x, y;
{
    int i;

    /* if we're here self->mousestate==scroll_NOTHING */
    
    if(action!=view_LeftDown && action!=view_RightDown) return FALSE;

    for(i=0;i<scroll_SIDES;i++)
	if((self->desired.location & (1<<i))) {
	    int dir;
	    struct rectangle *r;
	    if(PTINRECT(&self->topbutton[i], x, y)) {
		r = &self->topbutton[i];
		dir=1;
		self->mousestate=scroll_TOPENDZONE;
	    } else if(PTINRECT(&self->botbutton[i], x, y)) {
		r = &self->botbutton[i];
		dir=(-1);
		self->mousestate=scroll_BOTTOMENDZONE;
	    }
	    /* else the mouse state will be scroll_NOTHING */

	    self->lastaction=view_NoMouseEvent;

	    if(self->mousestate!=scroll_NOTHING) {
		self->side=i;
		self->lastbuttonstate=TRUE;
		self->lastaction=action;

		draw_arrow(self, i, r, dir, self->lastbuttonstate);
		ScheduleRepeatEndZone(self);
		endzone(self, i, self->mousestate, action);
		return TRUE;
	    }
	}
    return FALSE;
}

static void HandleThumbing(self, action, x, y)
struct scroll *self;
enum view_MouseAction action;
long x, y;
{
    struct scrollbar *cur, *des;
    long coord=0;
    int i;
    long posn;
    long logicalTop, logicalHeight, logicalPos;

    /* Assure that we have the correct button */
    if (((action == view_LeftMovement || action == view_LeftUp) && (self->lastaction != view_LeftDown)) || ((action == view_RightMovement || action == view_RightUp) && (self->lastaction != view_RightDown)) || self->side==(-1))
	return;

    cur = &self->current.bar[Type[self->side]];
    des = &self->desired.bar[Type[self->side]];

    if (self->side == scroll__LEFT || self->side == scroll__RIGHT) {
	logicalTop = self->interiors[self->side].top;
	logicalHeight = self->interiors[self->side].height;
	logicalPos = y;
    }
    else {
	logicalTop = self->interiors[self->side].left;
	logicalHeight = self->interiors[self->side].width;
	logicalPos = x;
    }
    coord = logicalPos - logicalTop;
    if(coord > logicalHeight) {
	coord=logicalHeight-self->endbarSpace;
	logicalPos=logicalTop+coord;
    }
    
    if (action == view_LeftMovement || action == view_RightMovement) {

	posn = from_bar_to_range(self, self->side, cur, coord);
	if(ABS(posn-cur->seen.beg+self->seenLength/2)>=self->seenLength/10) {
	    int location=self->current.location;
	    
	    des->seen.beg = posn-self->seenLength/2;
	    if (des->seen.beg < des->total.beg) {
		des->seen.beg = des->total.beg;
		des->seen.end = MIN(des->total.end, des->seen.beg + self->seenLength);
	    }
	    else if (des->seen.beg > des->total.end) {
		des->seen.beg = des->total.end;
		des->seen.end = des->total.end;
	    }
	    else
		des->seen.end = MIN(des->total.end, des->seen.beg + self->seenLength);
	    for (i = 0; i < scroll_SIDES; i++)  {
		if ((location & (1<<i)) && Type[i] == Type[self->side])  {
		    if (self->thumbScroll && (self->lastaction==view_LeftDown || (self->emulation && self->lastaction==view_RightDown)))  {
			set_frame(self, self->side, des->seen.beg, 0);
		    } else move_elevator(self, i);
		}
	    }
	    if (self->thumbScroll && (self->lastaction==view_LeftDown || (self->emulation && self->lastaction==view_RightDown))) {
		im_ForceUpdate();
	    } else *cur = *des;
	}
    } else { /* this is an up transition */
	im_SetWindowCursor(self->header.view.imPtr, NULL);
	
	des->seen.beg =  from_bar_to_range(self, self->side, cur, coord) - self->seenLength/2;
	if (des->seen.beg < des->total.beg)
	    des->seen.beg = des->total.beg;
	else if (des->seen.beg > des->total.end) {
	    des->seen.beg = des->total.end;
	}

	set_frame(self, self->side, des->seen.beg, 0);
	im_ForceUpdate();
    }
}

static void CheckBars(self, action, x, y)
struct scroll *self;
enum view_MouseAction action;
long x, y;
{
    int i;
    struct scrollbar *bar=NULL;
    struct rectangle r;
    
    if(action!=view_LeftDown && action!=view_RightDown) return;
    
    for(i=0;i<scroll_SIDES;i++) if(PTINRECT(&self->interiors[i], x, y)) break;

    if(i>=scroll_SIDES) return;
    
    bar= &self->current.bar[Type[self->side=i]];
    
    self->hitcoord=XYTOHITCOORD(self, i, x, y);

    self->mousestate=scroll_REPEATING;
    if (calc_elevator(self, i, bar, &r)) {
	switch(i) {
	    case scroll__LEFT:
	    case scroll__RIGHT:
		r.left=self->interiors[i].left;
		r.width=self->interiors[i].width;
		if(self->hitcoord<r.top) self->dir=scroll_UP;
		else if(self->hitcoord>r.top+r.height) self->dir=scroll_DOWN;
		else self->dir=scroll_NOWHERE;
		break;
	    case scroll__TOP:
	    case scroll__BOTTOM:
		r.top=self->interiors[i].top;
		r.height=self->interiors[i].height;
		if(self->hitcoord<r.left) self->dir=scroll_UP;
		else if (self->hitcoord>r.left+r.width) self->dir=scroll_DOWN;
		else self->dir=scroll_NOWHERE;
	}
	if(PTINRECT(&r, x, y)) {
	    self->mousestate=scroll_MAYBETHUMBING;
	    self->seenLength = bar->seen.end - bar->seen.beg;
	}
    }

    self->side = i;
    self->lastaction=action;
    if(self->emulation && action==view_RightDown) {
	self->seenLength = bar->seen.end - bar->seen.beg;
	self->mousestate=scroll_THUMBING;
	if(self->cursor!=NULL) im_SetWindowCursor(scroll_GetIM(self), self->cursor);
	HandleThumbing(self, view_RightMovement, x, y);
    } else if (self->startScrollTime > 0)  {
	self->scrollEvent = im_EnqueueEvent((procedure)RepeatScroll, (char *) self, event_MSECtoTU(self->startScrollTime));
    }
}

static void MaybeStartThumbing(self, action, x, y)
struct scroll *self;
enum view_MouseAction action;
long x, y;
{
    if(self->side==-1) return;

    if(ABS(XYTOHITCOORD(self, self->side, x, y) - self->hitcoord) <= SMALLDIST) {
	HandleRepeatMode(self, action, x, y);
	return;
    }
    
    if(action==view_LeftDown || action==view_RightDown) return;
    
    self->mousestate=scroll_THUMBING;
    CancelScrollEvent(self);

    if(self->cursor!=NULL) im_SetWindowCursor(scroll_GetIM(self), self->cursor);
    
    HandleThumbing(self, action, x, y);
}


struct view *scroll__Hit(self, action, x, y, num_clicks)
struct scroll *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    switch(self->mousestate) {
	case scroll_THUMBING:
	    HandleThumbing(self, action, x, y);
	    break;
	case scroll_MAYBETHUMBING:
	    MaybeStartThumbing(self, action, x, y);
	    break;
	case scroll_REPEATING:
	    HandleRepeatMode(self, action, x, y);
	    break;
	case scroll_TOPENDZONE:
	case scroll_BOTTOMENDZONE:
	    HandleEndZone(self, action, x, y);
	    break;
	case scroll_NOTHING:
	    if(action!=view_LeftDown && action!=view_RightDown &&
	       action!=view_LeftFileDrop && action!=view_MiddleFileDrop &&
	       action!=view_RightFileDrop) return (struct view *)self;
	    if(self->child && PTINRECT(&self->childrect, x, y)) return view_Hit(self->child, action, view_EnclosedXToLocalX(self->child, x), view_EnclosedYToLocalY(self->child, y), num_clicks);

	    if(CheckEndZones(self, action, x, y)) return (struct view *)self;

	    CheckBars(self, action, x, y);
	default: ;
    }
    
    /* if a button has gone up cancel any scheduled scrolling and set the state back to normal (ie anything can happen). */
    if(action==view_RightUp || action==view_LeftUp) {
	CancelScrollEvent(self);
	self->mousestate=scroll_NOTHING;
	self->side=(-1);
    }

    return (struct view *)self;
}

void scroll__LinkTree(self, parent)
struct scroll *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if (self->child)
	view_LinkTree(self->child, self);
}

void scroll__UnlinkNotification(self, unlinkedTree)
struct scroll *self;
struct view *unlinkedTree;
{

    updatelist_DeleteTree(self->updatelist, unlinkedTree);
    super_UnlinkNotification(self, unlinkedTree);
}
