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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/oscroll.c,v 2.33 1994/02/01 20:35:09 rr2b Exp $";
#endif


/* oscrollbar code for be2. */

#define DEBUG(X)

#include <andrewos.h>
#include <class.h>
#include <graphic.ih>
#include <view.ih>
#include <updlist.ih>
#include <im.ih>
#include <cursor.ih>
#include <event.ih>
#include <environ.ih>
#include <point.h>
#include <rect.h>
#include <oscroll.eh>

/* The physical aspects of the scrollbar that cannot be changed dynamically */
/* percentage overlap when thumbing using the endzones */
#define	THUMBPCT	10

/* The physical aspects of the scrollbar that cannot be changed dynamically */

#define SBORDER 	(TD_GAP + TD_DEPTH)
				/* size of border around scrollbar */

#define INNERWIDTH 	15	/* width of the thumb and arrows */
#define ENDZONELENGTH (emulation?INNERWIDTH+TD_GAP:12)
#define ENDTOBARSPACE 4
#define BARWIDTH (emulation?INNERWIDTH:20)
#define DOTWIDTH 7
#define MINDOTLENGTH 6
#define SMALLDIST 5
#define REALBARWIDTH(self) ((self)->barWidth+(emulation?2*SBORDER:0))

#define PIXELSPERINCH 75

/* The descriptions of the different types of scrollbars */
static int Type[oscroll_SIDES] = {oscroll_VERT, oscroll_VERT, oscroll_HORIZ, oscroll_HORIZ},
    LeftChange[oscroll_SIDES] = {1, 0, 0, 0},
    TopChange[oscroll_SIDES] = {0, 0, 1, 0},
    WidthChange[oscroll_SIDES] = {-1, -1, 0, 0},
    HeightChange[oscroll_SIDES] = {0, 0, -1, -1}, NoLeftChange[oscroll_SIDES] = {TD_GAP, 0, 0, 0},
    NoTopChange[oscroll_SIDES] = {0, 0, TD_GAP, 0},
    NoWidthChange[oscroll_SIDES] = {-TD_GAP, -TD_GAP, 0, 0},
    NoHeightChange[oscroll_SIDES] = {0, 0, -TD_GAP, -TD_GAP};    

static char *InterfaceName[oscroll_TYPES] = {"scroll,vertical", "scroll,horizontal"};


/* Icon infomation */
static short CursorIcon[oscroll_TYPES], ThumbIcon;

static long startScrollTime;
static long minContScrollTime;
static long maxContScrollTime;
static boolean adjustScroll;
static boolean thumbScroll;

/* status codes for from_bar_to_range */
#define OUTSIDEBAR 0
#define INTOPZONE oscroll_TOPENDZONE
#define INSIDEBAR 3
#define INBOTZONE oscroll_BOTTOMENDZONE


/* The different buttons that can be down */
#define NEITHER 0
#define LEFT 1
#define RIGHT 2
#define SMOOTH 4


/* The different states the thumb can be in */
#define NOPE 0
#define MAYBE 1
#define YES 2

/* Useful macros */
#ifndef ABS
#define ABS(x) ((x)<0?-(x):(x))
#endif
#define SWAP(a,b) (temp)=(a);(a)=(b);(b)=(temp)

#define INTERSECT(top1, bot1, top2, bot2) ((bot1) >= (top2) && (top1) <= (bot2))

/* Variables used by elevator redraw code. Statics instead to parameters for speed. */

static struct {
    long top, bot;
    boolean whitep;
} range[2];
static long seen_top, seen_bot, left, right;

/* Motif Emulation variables and #defines */

static int emulation=0;
static short NormalIcon;
static int current_end_state = 0;


/* The thumb direction codes */
#define oscroll_UP 1
#define oscroll_DOWN 2
#define oscroll_NEITHER 3



#define FILLPAT		(oscroll_BlackPattern(self))


/* Creation and Destruction routines. */

boolean oscroll__InitializeClass(classID)
struct classheader *classID;
{
    emulation = environ_GetProfileSwitch("MotifScrollBars", FALSE);
    if(emulation) NormalIcon = Cursor_Arrow;

    CursorIcon[oscroll_VERT] = Cursor_VerticalArrows;
    CursorIcon[oscroll_HORIZ] = Cursor_HorizontalArrows;
    ThumbIcon = Cursor_Octagon;

    startScrollTime = environ_GetProfileInt("StartScrollTime", 0);
    minContScrollTime = environ_GetProfileInt("ContScrollTime", 500);
    maxContScrollTime = environ_GetProfileInt("MaxContScrollTime", minContScrollTime);
    adjustScroll = environ_GetProfileSwitch("AdjustScroll", FALSE);
    thumbScroll = environ_GetProfileSwitch("ThumbScroll", FALSE);
    return TRUE;
}

boolean oscroll__InitializeObject(classID, self)
struct classheader *classID;
struct oscroll *self;
{
    int i;

    self->current.location = 0;
    self->child = self->scrollee = NULL;
    for (i = 0; i < oscroll_TYPES; i++) {
        struct oscrollbar *bar = &self->current.bar[i];
        bar->total.beg = 0;
        bar->total.end = 1;
        bar->seen.beg = bar->seen.end = bar->dot.beg = bar->dot.end = -1;
    }

    self->desired = self->current;

    for (i = 0; i < oscroll_TYPES; i++)
        self->fns[i] = NULL;

    self->pending_update = 0;
    self->updatelist = updatelist_New();
    self->left = self->top = self->width = self->height = 0;
    
    self->ideal_location = oscroll_LEFT;
    
    self->endzone_threshold = 80;
    self->bar_threshold = 0;
    self->endzoneLength = ENDZONELENGTH;
    self->endbarSpace = ENDTOBARSPACE;
    self->barWidth = environ_GetProfileInt("ScrollBarWidth",BARWIDTH);
    self->dotWidth = environ_GetProfileInt("DotWidth", DOTWIDTH);
    self->min_elevator[0] = 5;
    self->min_elevator[1] = 18;
    self->button = NEITHER;
    self->force_full_update = FALSE;
    self->force_get_interface = FALSE;
    self->scrollEvent = NULL;

    if(self->dotWidth<4) self->dotWidth=4;
    if(self->barWidth<8) self->barWidth=8;
    
    if(self->dotWidth>self->barWidth-4) {
	self->dotWidth=self->barWidth-4;
    }

    if(!emulation) {
	for (i = 0; i < oscroll_SIDES; i++) {
	    self->cursor[i] = cursor_Create(self);
	    cursor_SetStandard(self->cursor[i], CursorIcon[Type[i]]);
	}
    } else {
	self->direction = oscroll_NEITHER;
	self->cursor[0] = cursor_Create(self);
	cursor_SetStandard(self->cursor[0], NormalIcon);
    }

    return TRUE;
}
    
void oscroll__FinalizeObject(classID, self)
struct classheader *classID;
struct oscroll *self;
{
    int i;

    if (self->child != NULL)
        view_UnlinkTree(self->child);
    updatelist_Destroy(self->updatelist);
    
    if(!emulation) {
	for (i = 0; i < oscroll_SIDES; i++)
	cursor_Destroy(self->cursor[i]);
    } else {
	cursor_Destroy(self->cursor[0]);
    }
}

struct oscroll *oscroll__Create(classID, scrollee, location)
struct classheader *classID;
struct view *scrollee;
int location;
{
    struct oscroll *retval = oscroll_New();

    oscroll_SetView(retval, scrollee);
    oscroll_SetLocation(retval, location);

    return retval;
}



/* State modification routines. */

void oscroll__SetView(self, view)
struct oscroll *self;
struct view *view;
{

    oscroll_SetChild(self, view);
    oscroll_SetScrollee(self, view);
}

void oscroll__SetChild(self, child)
struct oscroll *self;
struct view *child;
{
    if (self->child != child) {
        self->force_full_update = TRUE;
        if (self->child)
            view_UnlinkTree(self->child);
        self->child = child;
        if (child)
            view_LinkTree(child, self);
        self->thumbing = NOPE;
        oscroll_WantUpdate(self, self);
    }
}

void oscroll__SetScrollee(self, scrollee)
struct oscroll *self;
struct view *scrollee;
{
    if (self->scrollee != scrollee) {
        self->force_get_interface = TRUE;
        self->scrollee = scrollee;
        self->thumbing = NOPE;
        oscroll_WantUpdate(self, self);
    }
}

struct view *oscroll__GetChild(self)
struct oscroll *self;
{
    return self->child;
}

struct view *oscroll__GetScrollee(self)
struct oscroll *self;
{
    return self->scrollee;
}

void oscroll__SetLocation(self, location)
struct oscroll *self;
int location;
{
    if(emulation && self->ideal_location != location) self->force_full_update = TRUE;
    self->ideal_location = location;
    oscroll_WantUpdate(self, self);  
}

int oscroll__GetLocation(self)
struct oscroll *self;
{
    return self->ideal_location;
}

int oscroll__GetCurrentLocation(self)
struct oscroll *self;
{
    return self->current.location;
}

void oscroll__SetParameters(self, endzone, bar, without, with)
struct oscroll *self;
long endzone, bar;
int without, with;
{
    self->endzone_threshold = endzone;
    self->bar_threshold = bar;
    self->min_elevator[0] = without;
    self->min_elevator[1] = with;
    oscroll_WantUpdate(self, self);
}

void oscroll__GetParameters(self, endzone, bar, without, with)
struct oscroll *self;
long *endzone, *bar;
int *without, *with;
{
    *endzone = self->endzone_threshold;
    *bar = self->bar_threshold;
    *without = self->min_elevator[0];
    *with = self->min_elevator[1];
}

void oscroll__SetWidth(self, newWidth)
struct oscroll *self;
long newWidth;
{
    self->barWidth = newWidth;
    
    if (self->dotWidth > newWidth - 2) {
	oscroll_SetDotWidth(self, newWidth - 2);
    }
    oscroll_WantUpdate(self, self);
}

long oscroll__GetWidth(self)
struct oscroll *self;
{
    return self->barWidth;
}

void oscroll__SetDotWidth(self, newWidth)
struct oscroll *self;
long newWidth;
{
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	if (newWidth > self->barWidth - 2) {
	    newWidth = self->barWidth - 2;
	}
	if (newWidth < 0) {
	    newWidth = 0;
	}
	self->dotWidth = newWidth;
	oscroll_WantUpdate(self, self);
    }
}

long oscroll__GetDotWidth(self)
struct oscroll *self;
{
    return self->dotWidth;
}

void oscroll__SetEndZoneLength(self, newLength)
struct oscroll *self;
long newLength;
{
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	self->endzoneLength = newLength;
	if ((self->endzoneLength + self->endbarSpace) * 2 > self->endzone_threshold) {
	    self->endzone_threshold = (self->endzoneLength + self->endbarSpace) * 2;
	}
	oscroll_WantUpdate(self, self);
    }
}

long oscroll__GetEndZoneLength(self)
struct oscroll *self;
{
    return self->endzoneLength;
}

void oscroll__SetEndToBarSpace(self, newSpace)
struct oscroll *self;
long newSpace;
{
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	self->endbarSpace = newSpace;
	if ((self->endzoneLength + self->endbarSpace) * 2 > self->endzone_threshold) {
	    self->endzone_threshold = (self->endzoneLength + self->endbarSpace) * 2;
	}
	oscroll_WantUpdate(self, self);
    }
}


long oscroll__GetEndToBarSpace(self)
struct oscroll *self;
{
    return self->endbarSpace;
}

/* Interface routines. */

static void get_interface(self, type)
struct oscroll *self;
int type;
{
    self->force_get_interface = FALSE;
    if (self->fns[type] == NULL)
        self->fns[type] = (struct oscrollfns *)view_GetInterface(self->scrollee, InterfaceName[type]);
}

static void getinfo(self, type, total, seen, dot)
struct oscroll *self;
int type;
struct orange *total, *seen, *dot;
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

static void set_frame(self, side, posn, coord)
struct oscroll *self;
int side;
int posn;
long coord;
{
    void (*real_setframe)();
    int type = Type[side];
    int realposn = posn;
    static int old[oscroll_SIDES][2]; /* Type x {posn, coord} */

    if(!emulation) {
	/* NORMAL SCROLLBARS */
	get_interface(self, type);

	if (self->fns[type] != NULL && (real_setframe = self->fns[type]->SetFrame) != NULL)
	    real_setframe(self->scrollee, posn, coord, type == oscroll_VERT ? self->height : self->width);
    } else {
	/* MOTIF SCROLLBARS */
	DEBUG(("set_frame: side: %d posn: %d num: %d dem: %d\n",side,posn,coord,
	       (type == oscroll_VERT) ? self->height : self->width));
	DEBUG(("set_frame: oldposn: %d oldnum: %d\n",old[side][0],old[side][1]));

	if (updatelist_UpdatesPending(self->updatelist)) {
	    /* delete the list and add us once */
	    /*	updatelist_Clear(self->updatelist); */
	    updatelist_DeleteTree(self->updatelist, (struct view *)self);
	    if (self->child) {
		updatelist_AddTo(self->updatelist, self->child);
	    }
	}
	get_interface(self, type);


	old[side][0] = posn;
	old[side][1] = coord;

	DEBUG(("end: %d %d\n",
	       self->desired.bar[type].seen.end,
	       self->desired.bar[type].total.end));



	DEBUG(("realposn: %d\n", realposn));

	if (self->fns[type] != NULL &&
	    (real_setframe = self->fns[type]->SetFrame) != NULL)
	    real_setframe(self->scrollee, realposn, coord, type == oscroll_VERT ?
			  self->height : self->width);
    }

}

/* Calculation routines. */

static long bar_height(self, side)
struct oscroll *self;
int side;
{
    switch (Type[side]) {
        case oscroll_VERT: return self->height;
        case oscroll_HORIZ: return self->width;
    }
}

static void endzone(self, side, end, action)
struct oscroll *self;
int side;
int end;
enum view_MouseAction action;
{
    void (*real_endzone)();
    int type = Type[side];
    int didSC = FALSE;

    get_interface(self, type);

    if(!emulation) {
	/* NORMAL SCROLLBARS */
	if (self->fns[type] != NULL && (real_endzone = self->fns[type]->EndZone) != NULL)
	    real_endzone(self->scrollee, end, action);
	else {
	    if (end == oscroll_TOPENDZONE)
		set_frame(self, side, self->desired.bar[type].total.beg, 0);
	    else
		set_frame(self, side, self->desired.bar[type].total.end, bar_height(self, side) >> 2);
	}
    } else {
	/* MOTIF SCROLLBARS */

	if (self->fns[type] != NULL &&
	    (real_endzone = self->fns[type]->EndZone) != NULL)
	    real_endzone(self->scrollee, end, action);
	else {
	    if (end == oscroll_TOPENDZONE) {
		set_frame(self, side, self->desired.bar[type].seen.beg,
			  bar_height(self, side) * (100 - THUMBPCT) / 100);
	    } else
		if (didSC)
		    set_frame(self, side,self->desired.bar[type].seen.end - 100, bar_height(self, side) * THUMBPCT / 100);
		else
		    set_frame(self, side, self->desired.bar[type].seen.end, bar_height(self, side) * THUMBPCT / 100);
	}
    }
}

static int what_is_at(self, side, coord)
struct oscroll *self;
int side;
int coord;
{
    long (*real_what)();
    int type = Type[side];

    get_interface(self, type);

    if (self->fns[type] != NULL && (real_what = self->fns[type]->WhatIsAt) != NULL)
        return real_what(self->scrollee, coord, type == oscroll_VERT ? self->height : self->width);
    else
        return 0;
}

static void rotate(self, side, x, y, scroll_x, scroll_y)
struct oscroll *self;
int side;
long x, y, *scroll_x, *scroll_y;
{
    switch (side) {
        case oscroll__LEFT:
            *scroll_x = x;
            *scroll_y = self->top + y;
            break;
	case oscroll__RIGHT:
		*scroll_x = oscroll_GetLogicalWidth(self) - 1 - x;
		*scroll_y = self->top + y;
	    break;
        case oscroll__TOP:
            *scroll_x = self->left + y;
            *scroll_y = x;
            break;
	case oscroll__BOTTOM:
		*scroll_x = self->left + y;
		*scroll_y = oscroll_GetLogicalHeight(self) - 1 - x;
            break;
    }
}



static long from_range_to_bar(self, side, bar, posn)
struct oscroll *self;
int side;
struct oscrollbar *bar;
long posn;
{
    int endzones = bar->endzones ? self->endzoneLength : 0;
    long cords = bar_height(self, side) - 2*(endzones + self->endbarSpace) - 1;
    long retval;

    /* chosen because 1M x 2k pixels will just about overflow in 32 bits */
    if (bar->total.end < 1000000) {
        retval = endzones + self->endbarSpace 
                + ((posn - bar->total.beg) * cords + (bar->total.end - bar->total.beg) / 2) / (bar->total.end - bar->total.beg);
    }
    else {
        retval = endzones + self->endbarSpace 
                + ((long)(((double)(posn - bar->total.beg)) * (double)cords / ((double)(bar->total.end - bar->total.beg)) + .5));
    }
    return retval;
}


static long from_bar_to_range(self, side, bar, posn, status)
struct oscroll *self;
int side, *status;
struct oscrollbar *bar;
long posn;
{
    int endzones = bar->endzones ? self->endzoneLength : 0;
    long height = bar_height(self, side),
        cords = height - 2*(endzones + self->endbarSpace),
        retval;

    if (bar->total.end < 1000000) {
        retval = bar->total.beg + (((bar->total.end - bar->total.beg) * (posn - self->endbarSpace - endzones) + cords / 2) / cords);
    }
    else {
        retval = ((long) ((double)(bar->total.beg) +
                           ((((double)(bar->total.end - bar->total.beg)) *
                              ((double)(posn - self->endbarSpace - endzones))) / (double)cords)
                           + .5));
    }

    if (posn < 0 || posn >= height)
        *status = OUTSIDEBAR;
    else if (posn < endzones)
        *status = INTOPZONE;
    else if (posn >= height - endzones)
        *status = INBOTZONE;
    else
        *status = INSIDEBAR;

    return retval;
}


static void calc_location(self)
struct oscroll *self;
{
    int i, lastlocation;

    /* This routine generates the desired.location from the ideal_location. 
	In doing so, it destroys the left, top, width, and height fields. 
	This works because the two places this routine is called 
	(oscroll__FullUpdate and oscroll__Update) force a full update 
	of the child if  they have changed. */

    /* start with the ideal configuration. */
    self->desired.location = self->ideal_location;
    do {
        self->left = 0;
        self->top = 0;
        self->width = oscroll_GetLogicalWidth(self);
        self->height = oscroll_GetLogicalHeight(self);
        lastlocation = self->desired.location;

	if(emulation) {
	    /* MOTIF SCROLLBARS */
	    /* give some room for the borders */
	    self->left += TD_BORDER;
	    self->top += TD_BORDER;
	    self->width -= (2*TD_BORDER);
	    self->height -= (2*TD_BORDER);
	}
	
        for (i = 0; i < oscroll_SIDES; i++)
            if (self->desired.location & (1<<i)) {
                if (bar_height(self, i) < self->bar_threshold) {
                    /* If the scrollbar doesn't fit, turn it off. */
                    self->desired.location &= ~(1<<i);
                }
                else {
                    /* Otherwise allocate it's space. */
                    self->left += LeftChange[i] * REALBARWIDTH(self);
                    self->top += TopChange[i] * REALBARWIDTH(self);
                    self->width += WidthChange[i] * REALBARWIDTH(self);
                    self->height += HeightChange[i] * REALBARWIDTH(self);
                }
	    } else if(emulation) {
		/* allocate the space for when the bar ISN'T there */
		self->left += NoLeftChange[i];
		self->top += NoTopChange[i];
		self->width += NoWidthChange[i];
		self->height += NoHeightChange[i];
	    }

        if (self->width < 0 || self->height < 0) {
            /* Turn off all scrollbars if there is no area left over. Happens when you have a very tall and narrow window, for example */
            self->width = oscroll_GetLogicalWidth(self);
            self->height = oscroll_GetLogicalHeight(self);
            self->left = self->top = 0;
            self->desired.location = 0;
        }
    } while (lastlocation != self->desired.location);
    /* Keep turning more and more off until something works. 
	Guaranteed to stop sometime because there are a finite number 
	of scrollbars that can be turned off (since we never turn any back on...) */
}

static void calc_desired(self)
struct oscroll *self;
{
    int i, exists[oscroll_TYPES];

    for (i = 0; i < oscroll_TYPES; i++) 
        exists[i] = 0;

    for (i = 0; i < oscroll_SIDES; i++) 
        if (self->desired.location & (1<<i)) 
            exists[Type[i]] = 1;

    for (i = 0; i < oscroll_TYPES; i++) 
        if (exists[i]) {
            struct oscrollbar *bar = &self->desired.bar[i];
            getinfo(self, i, &bar->total, &bar->seen, &bar->dot);
        }
}


static int calc_dot(self, side, bar, x1, y1, x2, y2)
struct oscroll *self;
int side;
struct oscrollbar *bar;
long *x1, *y1, *x2, *y2;
{

    if (bar->dot.end < bar->dot.beg || bar->dot.end < bar->total.beg || bar->dot.beg > bar->total.end)
        return 0;

    *y1 = from_range_to_bar(self, side, bar, bar->dot.beg);
    *y2 = from_range_to_bar(self, side, bar, bar->dot.end);

    *x1 = (REALBARWIDTH(self) - self->dotWidth) / 2;
    *x2 = *x1 + self->dotWidth - 1;

    if (bar->dot.beg == bar->dot.end) {
        *y1--;
        *y2 += 2;
    }
    else if (*y2 - *y1 < MINDOTLENGTH) {
        *y1 = (*y1 + *y2 - MINDOTLENGTH) / 2;
        *y2 = (*y1 + *y2 + MINDOTLENGTH) / 2;
    }

    return 1;
}

static int calc_elevator(self, side, bar, x1, y1, x2, y2)
struct oscroll *self;
int side;
struct oscrollbar *bar;
long *x1, *y1, *x2, *y2;
{
    int min,
        height = bar_height(self, side),
        extra = self->endbarSpace + (bar->endzones ? self->endzoneLength : 0);

    if (bar->seen.end < bar->seen.beg || bar->seen.end < bar->total.beg || bar->seen.beg > bar->total.end)
        return 0;

    *y1 = from_range_to_bar(self, side, bar, bar->seen.beg);
    *y2 = from_range_to_bar(self, side, bar, bar->seen.end);

    if(!emulation) {
	/* NORMAL SCROLLBARS */
	*x1 = 0;
	*x2 = REALBARWIDTH(self) - 2;
    } else {
	/* MOTIF SCROLLBARS */
	*x1 = SBORDER;
	*x2 = REALBARWIDTH(self) - SBORDER;
    }

    if (bar->endzones)
        min = self->min_elevator[1];
    else
        min = self->min_elevator[0];

    if (min > height - 2*extra)
        min = height - 2*extra;

    if (*y2 - *y1 < min) {
        *y1 = (*y1 + *y2 - min) / 2;
        if (*y1 < extra) {
            *y1 = extra;
            *y2 = *y1 + min - 1;
        }
        else if (*y1 + min >= height - extra) {
            *y2 = height - extra - 1;
            *y1 = *y2 - min + 1;
        }
        else
            *y2 = *y1 + min - 1;
    }
    return 1;
} 

static void rectangle(self, x1, y1, x2, y2, tile)
struct oscroll *self;
int x1, y1, x2, y2;
struct graphic *tile;
{
   struct rectangle rect;

    rectangle_SetRectSize(&rect, MIN(x1,x2), MIN(y1,y2), ABS(x1-x2) + 1, ABS(y1-y2) + 1);
    oscroll_SetTransferMode(self, graphic_COPY);

    oscroll_FillRect(self, &rect, tile);
}


/* MOTIF SCROLLBARS */
static void draw_bar(self, side, tt, tb)
struct oscroll *self;
int side;
long tt, tb;			/* top and bottom of thumb in already-rotated
				   coordianates */
{
    struct oscrollbar *bar = &self->desired.bar[Type[side]];
    long height;
    long x1, y1, x2, y2;

    height = bar_height(self, side);

    /* draw bar above thumb*/
    rotate(self, side, SBORDER , (bar->endzones ? self->endzoneLength : 0),
	   &x1, &y1);
    rotate(self, side, REALBARWIDTH(self) - SBORDER, 0, /* don't care */
	   &x2, &y2);

    if (Type[side] == oscroll_VERT)
	rectangle(self, x1, y1, x2 - 1, tt, FILLPAT);
    else
	rectangle(self, x1, y1 - 1, tt, y2, FILLPAT);

    
    /* draw bar below thumb */
    rotate(self, side, SBORDER, 0, /* don't care */
	   &x1, &y1);
    rotate(self, side, REALBARWIDTH(self) - SBORDER,
	   height - (bar->endzones ? self->endzoneLength : 0),
	   &x2, &y2);

    if (Type[side] == oscroll_VERT)
	rectangle(self, x1, tb, x2 - 1, y2, FILLPAT);
    else
	rectangle(self, tb, y1 - 1, x2, y2, FILLPAT);
}
    
static void draw_thumb_and_bar(self, side, force)
struct oscroll *self;
int side;
boolean force;
{
    long x1, y1, x2, y2;
    static long ox1 = 0, oy1 = 0, ox2 = 0, oy2 = 0;
    int ret;
    struct oscrollbar *bar = &self->desired.bar[Type[side]];

    ret = calc_elevator(self, side, bar, &x1, &y1, &x2, &y2);
    
    if (force || ret) {
	if (ret == 0) {		/* use old numbers */
	    x1 = ox1; y1 = oy1;
	    x2 = ox2; y2 = oy2;
	}

	if (!force && (x1 == ox1) && (y1 == oy1) && (x2 == ox2) && (y2 == oy2))
	    return;
	else {
	    ox1=x1; oy1=y1;
	    ox2=x2; oy2=y2;
	}

        rotate(self, side, x1, y1, &x1, &y1);
        rotate(self, side, x2, y2, &x2, &y2);

	motif_Draw3dBorder((struct view *)self, x1, y1, x2, y2,
			   TRUE, TD_BACKPAT(self));
	/* pass in the top/left and bottom/right of the new thumb */
	if ((Type[side]) == oscroll_VERT)
	    draw_bar(self, side, y1, y2);
	else
	    draw_bar(self, side, x1, x2);
    }
}


#define COPY_PT(s, d) (d).x = (s).x; (d).y = (s).y
#define SWAP_COPY_PT(s, d) (d).x = self->endzoneLength - (s).y; \
    			   (d).y = REALBARWIDTH(self) - (s).x;
    

static void init_arrows(self)
struct oscroll *self;
{
    struct arrow *a, *b;
    int scale3d = TD_DEPTH * 1.84;

    /* do the up-pointing arrow */
    a = &self->arrows[ARROW_UP];
    a->pa[0].x = (REALBARWIDTH(self) >> 1);	a->pa[0].y = INNERWIDTH + scale3d;
    a->pa[1].x = a->pa[0].x;		a->pa[1].y = a->pa[0].y -
						2 * TD_DEPTH;
    a->pa[4].x = SBORDER;		a->pa[4].y = TD_DEPTH;
    a->pa[2].x = a->pa[4].x + scale3d;	a->pa[2].y = a->pa[4].y +
						TD_DEPTH;
    a->pa[5].x = REALBARWIDTH(self)-SBORDER;	a->pa[5].y = TD_DEPTH;
    a->pa[3].x = a->pa[5].x - scale3d;	a->pa[3].y = a->pa[5].y +
						TD_DEPTH;
    /* now the left-pointing arrow, a -90 degree rotation of the up. */
    b = &self->arrows[ARROW_LEFT];
    SWAP_COPY_PT(a->pa[0], b->pa[0]);	SWAP_COPY_PT(a->pa[1], b->pa[1]);
    SWAP_COPY_PT(a->pa[2], b->pa[2]);	SWAP_COPY_PT(a->pa[3], b->pa[3]);
    SWAP_COPY_PT(a->pa[4], b->pa[4]);	SWAP_COPY_PT(a->pa[5], b->pa[5]);
}
    
static void draw_endzones(self, side, height, state)
struct oscroll *self;
int side, height;
int state;			/* which arrow on or off */
{
    long x1, y1, x2, y2;
    struct point pa[6];
    struct point t[4];		/* used for drawing the 3 traps and 1 tri */
    struct graphic *lt, *dk;
    struct arrow *a = NULL;
    static int arrows_inited = FALSE;
    struct oscrollbar *bar = &self->desired.bar[Type[side]];

    if (!bar->endzones)
	return;

    if (!arrows_inited)
	init_arrows(self);
    
    dk = TD_FGPAT(self);
    lt = TD_BGPAT(self);

    /* get a bounding box for the first arrow */
    rotate(self, side, 0, 0, &x1, &y1);
    rotate(self, side, REALBARWIDTH(self), self->endzoneLength, &x2, &y2);

    /* get a set of offsets to use */
    switch (Type[side]) {
        case oscroll_VERT:
	    a = &self->arrows[ARROW_UP];
	    break;
	case oscroll_HORIZ:
	    a = &self->arrows[ARROW_LEFT];
	    break;
	}

    pa[0].x = a->pa[0].x + MIN(x1,x2);
    pa[0].y = MAX(y1,y2) - a->pa[0].y;
    pa[1].x = a->pa[1].x + MIN(x1,x2);
    pa[1].y = MAX(y1,y2) - a->pa[1].y;
    pa[2].x = a->pa[2].x + MIN(x1,x2);
    pa[2].y = MAX(y1,y2) - a->pa[2].y;
    pa[3].x = a->pa[3].x + MIN(x1,x2);
    pa[3].y = MAX(y1,y2) - a->pa[3].y;
    pa[4].x = a->pa[4].x + MIN(x1,x2);
    pa[4].y = MAX(y1,y2) - a->pa[4].y;
    pa[5].x = a->pa[5].x + MIN(x1,x2);
    pa[5].y = MAX(y1,y2) - a->pa[5].y;

    /* draw it */
    COPY_PT(pa[0], t[0]);	COPY_PT(pa[1], t[1]);
    COPY_PT(pa[3], t[2]);	COPY_PT(pa[5], t[3]);
    oscroll_FillPolygon(self, t, 4, (state == INTOPZONE) ? lt : dk);
    
    COPY_PT(pa[4], t[0]);	COPY_PT(pa[2], t[1]);
    oscroll_FillPolygon(self, t, 4, (state == INTOPZONE) ? lt : dk);
    
    COPY_PT(pa[1], t[2]);	COPY_PT(pa[0], t[3]);
    oscroll_FillPolygon(self, t, 4, (state == INTOPZONE) ? dk : lt);
    
    COPY_PT(pa[1], t[1]);	COPY_PT(pa[2], t[2]);	COPY_PT(pa[3],t[0]);
    oscroll_FillPolygon(self, t, 3, TD_BACKPAT(self));


    /* get a bounding box for the second arrow */
    rotate(self, side, 0, height - self->endzoneLength, &x1, &y1);
    rotate(self, side, REALBARWIDTH(self), height, &x2, &y2);


    /* translate the offsets from the other arrow to the
       other end of the scrollbar */
    switch(Type[side]) {

        case oscroll_VERT:
	    /* x's set from above */
	    a = &self->arrows[ARROW_UP];
	    pa[0].y = y1 + a->pa[0].y;
	    pa[1].y = y1 + a->pa[1].y;
	    pa[2].y = y1 + a->pa[2].y;
	    pa[3].y = y1 + a->pa[3].y;
	    pa[4].y = y1 + a->pa[4].y;
	    pa[5].y = y1 + a->pa[5].y;
	    break;

	case oscroll_HORIZ:
	    /* y's set from above */
	    a = &self->arrows[ARROW_LEFT];
	    pa[0].x = x2 - a->pa[0].x;
	    pa[1].x = x2 - a->pa[1].x;
	    pa[2].x = x2 - a->pa[2].x;
	    pa[3].x = x2 - a->pa[3].x;
	    pa[4].x = x2 - a->pa[4].x;
	    pa[5].x = x2 - a->pa[5].x;
	    break;
	}

    /* draw it */
    COPY_PT(pa[0], t[0]);	COPY_PT(pa[1], t[1]);
    COPY_PT(pa[3], t[2]);	COPY_PT(pa[5], t[3]);
    oscroll_FillPolygon(self, t, 4, (state == INBOTZONE) ? lt : dk);
    
    COPY_PT(pa[4], t[0]);	COPY_PT(pa[2], t[1]);
    oscroll_FillPolygon(self, t, 4, (state == INBOTZONE) ? dk : lt);
    
    COPY_PT(pa[1], t[2]);	COPY_PT(pa[0], t[3]);
    oscroll_FillPolygon(self, t, 4, (state == INBOTZONE) ? dk : lt);

    COPY_PT(pa[1], t[1]);	COPY_PT(pa[2], t[2]);	COPY_PT(pa[3],t[0]);
    oscroll_FillPolygon(self, t, 3, TD_BACKPAT(self));
}
			  
static void motif_draw_whole_bar(self, side)
struct oscroll *self;
int side;
{
    struct oscrollbar *bar = &self->desired.bar[Type[side]];
    int height = bar_height(self, side);
    long x1, y1, x2, y2;

    /* clear out area around the scrollbar */
    rotate(self, side, 0, -TD_BORDER - TD_GAP, &x1, &y1);
    rotate(self, side, REALBARWIDTH(self), height + TD_BORDER + TD_GAP, &x2, &y2);
    rectangle(self, x1, y1, x2, y2, TD_BACKPAT(self));

    if (bar->endzones) {
	rotate(self, side, SBORDER, -TD_DEPTH, &x1, &y1);
	rotate(self, side, REALBARWIDTH(self) - SBORDER, self->endzoneLength + SBORDER, &x2, &y2);
	rectangle(self, x1, y1, x2, y2, FILLPAT);

	rotate(self, side, SBORDER, height - SBORDER - self->endzoneLength, &x1, &y1);
	rotate(self, side, REALBARWIDTH(self) - SBORDER, height + TD_DEPTH - 1, &x2, &y2);
	rectangle(self, x1, y1, x2, y2, FILLPAT);
	
	draw_endzones(self, side, height, current_end_state);
    }

    draw_thumb_and_bar(self, side, FALSE);

    /* draw bar border */

    switch (Type[side]) {
        case oscroll_VERT:
    	    rotate(self, side, TD_GAP, -TD_BORDER,
		   &x1, &y1);
	    rotate(self, side, REALBARWIDTH(self) - TD_GAP, height + TD_BORDER,
		   &x2, &y2);
	    break;
        case oscroll_HORIZ:
    	    rotate(self, side, TD_GAP, -TD_BORDER,
		   &x1, &y1);
	    rotate(self, side, REALBARWIDTH(self) - TD_GAP, height + TD_BORDER,
		   &x2, &y2);
	    break;
	}
    motif_Draw3dBorder(self, x1, y1, x2, y2, FALSE, NULL);
}

static void draw_elevator(self, side)
struct oscroll *self;
int side;
{
    long x1, y1, x2, y2;
    long left, top, width, height;
    struct oscrollbar *bar = &self->desired.bar[Type[side]];

    if (calc_elevator(self, side, bar, &x1, &y1, &x2, &y2)) {

        rotate(self, side, x1, y1, &x1, &y1);
        rotate(self, side, x2, y2, &x2, &y2);

        rectangle(self, x1, y1, x2, y2, self->elevatorFill);
        oscroll_SetTransferMode(self, graphic_COPY);
	left = (x1<x2 ? x1 : x2);
	top =  (y1<y2 ? y1 : y2);
	width = abs(x1-x2);
	height = abs(y1-y2);
	oscroll_DrawRectSize(self,left,top,width,height);
/*
        scroll_MoveTo(self, x1, y1);
        scroll_DrawLineTo(self, x2, y1);
        scroll_DrawLineTo(self, x2, y2);
        scroll_DrawLineTo(self, x1, y2);
        scroll_DrawLineTo(self, x1, y1);
*/
    }
}
    
static void draw_dot(self, side)
struct oscroll *self;
int side;
{
    long x1, y1, x2, y2;
    long left, top, width, height;
    struct oscrollbar *bar = &self->desired.bar[Type[side]];

    if (calc_dot(self, side, bar, &x1, &y1, &x2, &y2)) {
	rotate(self, side, x1, y1, &x1, &y1);
	rotate(self, side, x2, y2, &x2, &y2);

	oscroll_SetTransferMode(self, graphic_COPY);
	/* drawing 2 zero-width rects should probably be faster 
	 than the old code that drew one double width rectangle.
	 Also, this code avoids known bugs in some X servers */
	left = (x1<x2 ? x1 : x2);
	top =  (y1<y2 ? y1 : y2);
	width = abs(x1-x2) ;
	height = abs(y1-y2);
	oscroll_DrawRectSize(self,left,top,width,height);
	left++; top++;
	width = MAX((width - 2),1);
	height = MAX((height -2),1);
	oscroll_DrawRectSize(self,left,top,width,height);
    }
}

static void normal_draw_whole_bar(self, side)
struct oscroll *self;
int side;
{
    struct oscrollbar *bar = &self->desired.bar[Type[side]];
    int height = bar_height(self, side);
    long x1, y1, x2, y2;

    oscroll_SetTransferMode(self, graphic_COPY);

    /* line between bar and child */
    rotate(self, side, REALBARWIDTH(self) - 1, 0, &x1, &y1);
    oscroll_MoveTo(self, x1, y1);
    rotate(self, side, REALBARWIDTH(self) - 1, height /*- 1*/, &x1, &y1);
    oscroll_DrawLineTo(self, x1, y1);

    if (bar->endzones) {
        rotate(self, side, 0, 0, &x1, &y1);
        rotate(self, side, REALBARWIDTH(self) - 2, self->endzoneLength - 1, &x2, &y2);
        rectangle(self, x1, y1, x2, y2, self->endzoneFill);

        rotate(self, side, 0, height - self->endzoneLength, &x1, &y1);
        rotate(self, side, REALBARWIDTH(self) - 2, height - 1, &x2, &y2);
        rectangle(self, x1, y1, x2, y2, self->endzoneFill);
    }

    rotate(self, side, 0, bar->endzones ? self->endzoneLength : 0, &x1, &y1);
    rotate(self, side, REALBARWIDTH(self) - 2, 
		height - (bar->endzones ? self->endzoneLength : 0) - 1, &x2, &y2);
    rectangle(self, x1, y1, x2, y2, self->barFill);

    draw_elevator(self, side);
    draw_dot(self, side);
}


static void erase_dot(self, side, top, bot)
struct oscroll *self;
int side;
long top, bot;
{
    int i;
    long x1, x2, y1, y2;

    if (top > bot)
        return;

    for (i = 0; i < 2; i++)
	if (INTERSECT(top, bot, range[i].top, range[i].bot)) {
	    erase_dot(self, side, top, range[i].top-1);
	    erase_dot(self, side, range[i].bot+1, bot);
	    return;
	}

    rotate(self, side, left, top, &x1, &y1);
    rotate(self, side, right, bot, &x2, &y2);
    rectangle(self, x1, y1, x2, y2, 
		INTERSECT(top, bot, seen_top, seen_bot) ? self->elevatorFill : self->barFill);
}

static void draw_end_line(self,x1,y1,x2,y2)
struct oscroll * self;
long x1,y1,x2,y2; {
    long temp;
    /* first order everything */
    if (x1>x2) { temp = x1; x1 = x2; x2 = temp; }
    if (y1>y2) { temp = y1; y1 = y2; y2 = temp; }
    oscroll_MoveTo(self,x1,y1);
    /* Horizontal or vertical? */
    if (x1 == x2) { /* vertical drawing */
	oscroll_DrawLineTo(self,x2,y2+1);
    }
    else {  /* horizontal drawing */
	oscroll_DrawLineTo(self,x2+1,y2);
    }
}

static void move_elevator(self, side)
struct oscroll *self;
int side;
{
    struct oscrollbar *des = &self->desired.bar[Type[side]],
        *cur = &self->current.bar[Type[side]];
    int dot, i;
    long temp, old_top, old_bot, top, bot;

    if (!calc_elevator(self, side, cur, &left, &range[0].top, &right, &range[0].bot)) {
        range[0].top = range[0].bot = -1;
    }
    range[0].whitep = 0;

    if (!calc_elevator(self, side, des, &left, &range[1].top, &right, &range[1].bot)) {
        range[1].top = range[1].bot = -1;
    }
    range[1].whitep = 1;
    seen_top = range[1].top;
    seen_bot = range[1].bot;

    if (INTERSECT(range[0].top, range[0].bot, range[1].top, range[1].bot)) {
        /* The old range and the new range overlap. We don't need to do all the work. */
        if (range[0].top < range[1].top) {
            SWAP(range[0].bot, range[1].top);
            if (range[1].bot < range[1].top) {
                SWAP(range[1].top, range[1].bot);
                range[1].whitep = 0;
            }
        }
        else {
            SWAP(range[0].top, range[1].bot);
            if (range[0].bot < range[0].top) {
                SWAP(range[0].top, range[0].bot);
                range[0].whitep = 1;
            }
        }
    }

    for (i = 0; i < 2; i++) {
        long ul_x, ul_y, ur_x, ur_y, ll_x, ll_y, lr_x, lr_y;
	struct point tmpPoints[4];

        if (range[i].top != range[i].bot) {
            rotate(self, side, left, range[i].top, &ul_x, &ul_y);
            rotate(self, side, right, range[i].top, &ur_x, &ur_y);
            rotate(self, side, left, range[i].bot, &ll_x, &ll_y);
            rotate(self, side, right, range[i].bot, &lr_x, &lr_y);
            if (range[i].whitep) {
                rectangle(self, ul_x, ul_y, lr_x, lr_y, self->elevatorFill);
                oscroll_SetTransferMode(self, graphic_COPY);
		/* One last pixel hack works for vertical scroll bars but I don't know about horizontal -- these calculations confuse me as to how they work horizontally -- maybe the rotations do work correctly, but it seems strange. */
		/* end of old way that almost works */
		/* beginning of new way to be tested */
		/* See which of four cases we are interested in: two sides, top U, bottom U, all four -- note that because of the rotation transform, the hints "ul" for upper left, etc, have meaning only for vertical scrollbars */
		/* Vertical or horizontal? */
		if (ll_x != ul_x) { /* upper left and lower left not vertical => horizontal scroll bars */
		    if ((seen_top == range[i].top) && (seen_bot == range[i].bot) ) {
			/* All four, draw a rectangle */
			oscroll_DrawRectSize(self,ur_x, ur_y, lr_x-ur_x, ul_y-ur_y);
		    }
		    else if (seen_top == range[i].top) {
			/* draw left U */
			point_SetPt(&tmpPoints[0],ll_x+1,ll_y);
			point_SetPt(&tmpPoints[1],ul_x,ul_y);
			point_SetPt(&tmpPoints[2],ur_x,ur_y);
			point_SetPt(&tmpPoints[3],lr_x+1,lr_y);
			oscroll_DrawPath(self,tmpPoints,4);
		    }
		    else if (seen_bot == range[i].bot) {
			/* draw right U */
			point_SetPt(&tmpPoints[0],ur_x,ur_y);
			point_SetPt(&tmpPoints[1],lr_x,lr_y);
			point_SetPt(&tmpPoints[2],ll_x,ll_y);
			point_SetPt(&tmpPoints[3],ul_x,ul_y);
			oscroll_DrawPath(self,tmpPoints,4);
		    }
		    else {
			/* draw two horizontal sides only */
			oscroll_MoveTo(self, ul_x, ul_y);
			oscroll_DrawLineTo(self, ll_x, ll_y);
			oscroll_MoveTo(self, ur_x, ur_y);
			oscroll_DrawLineTo(self, lr_x, lr_y);
		    }
		}
		else { /* vertical scroll bars */
		    if ((seen_top == range[i].top) && (seen_bot == range[i].bot) ) {
			/* All four, draw a rectangle  */
			oscroll_DrawRectSize(self,ul_x, ul_y, ur_x-ul_x, ll_y-ul_y);
		    }
		    else if (seen_top == range[i].top) {
			/* draw top U */
			point_SetPt(&tmpPoints[0],ll_x,ll_y+1);
			point_SetPt(&tmpPoints[1],ul_x,ul_y);
			point_SetPt(&tmpPoints[2],ur_x,ur_y);
			point_SetPt(&tmpPoints[3],lr_x,lr_y+1);
			oscroll_DrawPath(self,tmpPoints,4);
		    }
		    else if (seen_bot == range[i].bot) {
			/* draw bottom U */
			point_SetPt(&tmpPoints[0],ur_x,ur_y);
			point_SetPt(&tmpPoints[1],lr_x,lr_y);
			point_SetPt(&tmpPoints[2],ll_x,ll_y);
			point_SetPt(&tmpPoints[3],ul_x,ul_y);
			oscroll_DrawPath(self,tmpPoints,4);
		    }
		    else {
			/* draw two vertical sides only */
			oscroll_MoveTo(self, ul_x, ul_y);
			oscroll_DrawLineTo(self, ll_x, ll_y + 1 );
			oscroll_MoveTo(self, ur_x, ur_y);
			oscroll_DrawLineTo(self, lr_x, lr_y + 1);
		    }
		}
	    } /* end of test for any white part to be drawn */
            else {
                rectangle(self, ul_x, ul_y, lr_x, lr_y, self->barFill);
                oscroll_SetTransferMode(self, graphic_COPY);
                if (seen_top == range[i].bot) {
		    draw_end_line(self,ll_x,ll_y,lr_x,lr_y);
/*                    oscroll_MoveTo(self, ll_x, ll_y);
                    oscroll_DrawLineTo(self, lr_x, lr_y);  */
                }
                if (seen_bot == range[i].top) {
		    draw_end_line(self,ul_x,ul_y,ur_x,ur_y);
/*                    oscroll_MoveTo(self, ul_x, ul_y);
                    oscroll_DrawLineTo(self, ur_x, ur_y); */
                }
            }
        }
    }

    dot = calc_dot(self, side, des, &left, &top, &right, &bot);
    if (calc_dot(self, side, cur, &left, &old_top, &right, &old_bot) && (!dot || top != old_top || bot != old_bot))
        erase_dot(self, side, old_top, old_bot);

    if (dot)
        draw_dot(self, side);
}


static void 
normal_full_update(self, type, left, top, width, height)
struct oscroll *self;
enum view_UpdateType type;
long left, top, width, height;
{
    int i;
    struct rectangle rect, crect, VB;

    oscroll_GetVisualBounds(self, &VB);
    self->force_full_update = FALSE;
    if (self->force_get_interface)
        for (i = 0; i < oscroll_TYPES; i++)
            self->fns[i] = NULL;

    if (self->child) {
	oscroll_RetractViewCursors(self,self->child);
        rectangle_SetRectSize(&rect, self->left, self->top, self->width, self->height);
        view_InsertView(self->child, self, &rect);
        rectangle_SetRectSize(&crect, left, top, width, height);
        rectangle_IntersectRect(&crect, &crect, &rect);

        view_FullUpdate(self->child, type, crect.left - self->left, crect.top - self->top, crect.width, crect.height);
    }
    /* If visual rectangle is bogus-- leave. */
    if(type == view_Remove || rectangle_IsEmptyRect(&VB) ) return;
    if(type != view_MoveNoRedraw) {
        /* Is it possible that the scrollee has changed his mind about the scrollbar locations now that he has been redrawn? If so, we need to account for his area changing, and recalling his full update. Then what if he changes his mind again? */
        if(self->child) calc_desired(self);
	if (self->left > 0) {
	    if (self->top > 0) {
		rectangle(self, 0, 0, self->left - 1, self->top - 1, self->cornerFill);
	    }
	    if (self->height + self->top < oscroll_GetLogicalHeight(self)) {
		rectangle(self, 0, self->height + self->top, self->left - 1, oscroll_GetLogicalHeight(self) - 1, self->cornerFill);
	    }
	}
	if (self->width + self->left < oscroll_GetLogicalWidth(self)) {
	    int left = self->width + self->left;

	    if (self->top > 0) {
		rectangle(self, left, 0, oscroll_GetLogicalWidth(self) - 1, self->top - 1, self->cornerFill);
	    }
	    if (self->height + self->top < oscroll_GetLogicalHeight(self)) {
		rectangle(self, left, self->height + self->top, oscroll_GetLogicalWidth(self) - 1, oscroll_GetLogicalHeight(self) - 1, self->cornerFill);
	    }
	}
    }
    for (i = 0; i < oscroll_SIDES; i++)
        if (self->desired.location & (1<<i)) {
            self->desired.bar[Type[i]].endzones = bar_height(self, i) >= self->endzone_threshold;
            normal_draw_whole_bar(self, i);
            switch (i) {
                case oscroll__LEFT:
		    rectangle_SetRectSize(&rect, 0, self->top, REALBARWIDTH(self), self->height);
                    break;
                case oscroll__RIGHT:
		    rectangle_SetRectSize(&rect, self->left + self->width, self->top, REALBARWIDTH(self), self->height);
                    break;
                case oscroll__TOP:
		    rectangle_SetRectSize(&rect, self->left, 0, self->width, REALBARWIDTH(self));
                    break;
                case oscroll__BOTTOM:
		    rectangle_SetRectSize(&rect, self->left, self->top + self->height, self->width, REALBARWIDTH(self));
                    break;
            }
            oscroll_PostCursor(self, &rect, self->cursor[i]);
        }
        else
            oscroll_RetractCursor(self, self->cursor[i]);

    if(type != view_MoveNoRedraw){
	self->current = self->desired;
	oscroll_FlushGraphics(self);
    }
}


static void motif_full_update(self, type, left, top, width, height)
struct oscroll *self;
enum view_UpdateType type;
long left, top, width, height;
{
    int i;
    struct rectangle rect, crect, VB;

    oscroll_GetVisualBounds(self, &VB);
    self->force_full_update = FALSE;
    if (self->force_get_interface)
	for (i = 0; i < oscroll_TYPES; i++)
	    self->fns[i] = NULL;

    /* clear out space between views' border and the view */

    motif_DrawBorder(self, self->left - TD_BORDER + TD_DEPTH + 0,
		      self->top - TD_BORDER + TD_DEPTH + 0,
		      self->left + self->width + TD_BORDER - TD_DEPTH - 0,
		      self->top + self->height + TD_BORDER - TD_DEPTH - 0,
		      oscroll_WhitePattern(self), oscroll_WhitePattern(self),
		      NULL, TD_BORDER + TD_DEPTH);

    /* do the border around the view */

    motif_Draw3dBorder(self, self->left - TD_BORDER + 0,
			self->top - TD_BORDER + 0,
			self->left + self->width + TD_BORDER - 0,
			self->top + self->height + TD_BORDER - 0,
			FALSE, NULL);

    if (self->child) {
	rectangle_SetRectSize(&rect, self->left, self->top, self->width,
			      self->height);
	view_InsertView(self->child, self, &rect);
	rectangle_SetRectSize(&crect, left, top, width, height);
	rectangle_IntersectRect(&crect, &crect, &rect);

	view_FullUpdate(self->child, type, crect.left - self->left,
			crect.top - self->top, crect.width, crect.height);
	/* Is it possible that the oscrollee has changed his mind about the
	 scrollbar locations now that he has been redrawn? If so, we need to
	 account for his area changing, and recalling his full update. Then
	     what if he changes his mind again? */
	calc_desired(self);
    }

    /* If visual rectangle is bogus-- leave. */
    if(type == view_Remove || rectangle_IsEmptyRect(&VB) )
	return;

    if (type != view_FullRedraw && type != view_LastPartialRedraw)
	return;

    /* First, draw the border and background */
    for (i = 0; i < oscroll_SIDES; i++)
	if (self->desired.location & (1<<i)) {
	    self->desired.bar[Type[i]].endzones = bar_height(self, i) >=
	      self->endzone_threshold;
	    switch (i) {
		case oscroll__LEFT:
		    rectangle_SetRectSize(&rect, SBORDER, self->top,
					  REALBARWIDTH(self), self->height);
		    break;
		case oscroll__RIGHT:
		    rectangle_SetRectSize(&rect, self->left + self->width + TD_BORDER + SBORDER, self->top,
					  REALBARWIDTH(self), self->height);
		    break;
		case oscroll__TOP:
		    rectangle_SetRectSize(&rect, self->left - TD_BORDER, SBORDER,
					  self->width, REALBARWIDTH(self));
		    break;
		case oscroll__BOTTOM:
		    rectangle_SetRectSize(&rect, self->left - TD_BORDER, self->top + self->height + TD_BORDER + SBORDER,
					  self->width, REALBARWIDTH(self));
		    break;
	    }
	    oscroll_PostCursor(self, &rect, self->cursor[0]);
	} else {		/* the bar isn't there */
	    oscroll_RetractCursor(self, self->cursor[0]);
	    switch (i) {
		case oscroll__LEFT:
		    rectangle(self, 0, 0,
			      TD_GAP, oscroll_GetLogicalHeight(self),
			      TD_BACKPAT(self));
		    break;
		case oscroll__RIGHT:
		    rectangle(self, oscroll_GetLogicalWidth(self), 0,
			      oscroll_GetLogicalWidth(self) - TD_GAP,
			      oscroll_GetLogicalHeight(self), TD_BACKPAT(self));
		    break;
		case oscroll__TOP:
		    rectangle(self, 0, 0, oscroll_GetLogicalWidth(self),
			      TD_GAP, TD_BACKPAT(self));
		    break;
		case oscroll__BOTTOM:
		    rectangle(self, 0, oscroll_GetLogicalHeight(self),
			      oscroll_GetLogicalWidth(self),
			      oscroll_GetLogicalHeight(self) - TD_GAP,
			      TD_BACKPAT(self));
	    }
	}


    /* Next, fill in the corners */

    if (self->left - TD_BORDER > 0) {
	if (self->top - TD_BORDER > 0) {
	    rectangle(self, 0, 0, self->left - TD_BORDER, self->top - TD_BORDER,
		      TD_BACKPAT(self));
	}

	if (self->height+self->top+2*TD_BORDER < oscroll_GetLogicalHeight(self)) {
	    rectangle(self, 0, self->height + self->top + TD_BORDER,
		      self->left - TD_BORDER,
		      oscroll_GetLogicalHeight(self), TD_BACKPAT(self));
	}
    }

    if (self->width + self->left + 2*TD_BORDER< oscroll_GetLogicalWidth(self)) {
	int left = self->width + self->left + TD_BORDER;

	if (self->top - TD_BORDER > 0) {
	    rectangle(self, left, 0, oscroll_GetLogicalWidth(self),
		      self->top - TD_BORDER, TD_BACKPAT(self));
	}

	if (self->height + self->top + TD_BORDER< oscroll_GetLogicalHeight(self)) {
	    rectangle(self, left, self->height + self->top + TD_BORDER,
		      oscroll_GetLogicalWidth(self),
		      oscroll_GetLogicalHeight(self), TD_BACKPAT(self));
	}
    }


    /* Now, draw the oscrollbars themselves */

    for (i = 0; i < oscroll_SIDES; i++)
	if (self->desired.location & (1<<i)) {
	    motif_draw_whole_bar(self, i);
	    draw_thumb_and_bar(self, i, TRUE);
	}

    self->current = self->desired;
    oscroll_FlushGraphics(self);
}


static void full_update(self, type, left, top, width, height)
struct oscroll *self;
enum view_UpdateType type;
long left, top, width, height;
{
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	normal_full_update(self, type, left, top, width, height);
    } else {
	/* MOTIF SCROLLBARS */
	motif_full_update(self, type, left, top, width, height);
    }
}

/* Overrides of the view routines: */

void oscroll__FullUpdate(self, type, left, top, width, height)
struct oscroll *self;
enum view_UpdateType type;
long left, top, width, height;
{
    self->pending_update = 0;
    if (type == view_FullRedraw || type == view_LastPartialRedraw) {

	self->cornerFill = oscroll_GrayPattern(self,environ_GetProfileInt("CornerShade",8),16);
	self->endzoneFill = oscroll_GrayPattern(self,environ_GetProfileInt("EndZoneShade",2),16);
	self->barFill = oscroll_GrayPattern(self,environ_GetProfileInt("BackgroundShade",4),16);
	self->whiteFill =oscroll_WhitePattern(self);
	self->elevatorFill =  oscroll_GrayPattern(self,environ_GetProfileInt("ElevatorShade",0),16);
        calc_location(self);
        calc_desired(self);
    }
    full_update(self, type, left, top, width, height);
}

void normal_scroll__Update(self)
struct oscroll *self;
{
    int i;
    long l, t, w, h;

    l = oscroll_GetVisualLeft(self);
    t = oscroll_GetVisualRight(self);
    w = oscroll_GetVisualWidth(self);
    h = oscroll_GetVisualHeight(self);

    if (w == 0 || h == 0) return;

    self->pending_update = 0;
    /* Let the children modify their state however they want. */
    updatelist_Clear(self->updatelist);

    if (self->thumbing == YES)
	return;

    calc_location(self);
    calc_desired(self);

    /* Is the change so drastic that we need to start from scratch? */
    if (self->current.location != self->desired.location || self->force_full_update) {
	rectangle(self, l, t, w, h, self->whiteFill);
        full_update(self, view_FullRedraw, l, t, w, h);
    }
    else {
        for (i = 0; i < oscroll_SIDES; i++)
            if (self->desired.location & (1<<i)) {
                int type = Type[i];
                struct oscrollbar *des = &self->desired.bar[type], *cur = &self->current.bar[type];

                if (des->endzones != cur->endzones)
                    /* The entire oscrollbar need redrawing? */
                    normal_draw_whole_bar(self, i);
                else if (des->total.beg != cur->total.beg || des->total.end != cur->total.end || des->seen.beg != cur->seen.beg || des->dot.beg != cur->dot.beg || des->seen.end != cur->seen.end || des->dot.end != cur->dot.end) {
                    move_elevator(self, i);
                }
            }
        self->current = self->desired;
    }
}

void motif_scroll__Update(self)
struct oscroll *self;
{
    int i;
    long l, t, w, h;

    l = oscroll_GetVisualLeft(self);
    t = oscroll_GetVisualRight(self);
    w = oscroll_GetVisualWidth(self);
    h = oscroll_GetVisualHeight(self);

    if (w == 0 || h == 0) return;

    self->pending_update = 0;
    /* Let the children modify their state however they want. */
    updatelist_Clear(self->updatelist);

    if (self->thumbing == YES) {
	return;
    }
    calc_location(self);
    calc_desired(self);

    /* Is the change so drastic that we need to start from scratch? */
    if (self->current.location != self->desired.location ||
	self->force_full_update) {
        rectangle(self, l, t, w, h, TD_BACKPAT(self));
        full_update(self, view_FullRedraw, l, t, w, h);
    }
    else {
        for (i = 0; i < oscroll_SIDES; i++)
            if (self->desired.location & (1<<i)) {
		draw_thumb_and_bar(self, i, FALSE);
            }
        self->current = self->desired;
    }
}

void oscroll__Update(self)
struct oscroll *self;
{
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	normal_scroll__Update(self);
    } else {
	/* MOTIF SCROLLBARS */
	motif_scroll__Update(self);
    }
}


void oscroll__WantUpdate(self, requestor)
struct oscroll *self;
struct view *requestor;
{
    if ((struct view *)self != requestor)
        updatelist_AddTo(self->updatelist, requestor);

    if (!self->pending_update) {
        self->pending_update = 1;
        if(((struct view *)self)->parent) view_WantUpdate(((struct view *)self)->parent, self);
    }
}
/*
 * What to do every time the timer repeat fires.
 * We separate this from RepeatScroll so that we can call
 * it right when we get a mouse hit.  This way there is
 * no scroll latency.
 */
static void RepeatEvent(self)
struct oscroll *self;
{
    struct oscrollbar *cur = NULL;
    static long lastcoord = 0;
    

    if (lastcoord == 0)
	lastcoord = self->hitcoord;
    
    if (self->side != -1) {
        cur = &self->current.bar[Type[self->side]];
    }

    if (current_end_state != 0) {
	endzone(self, self->side, (current_end_state == INTOPZONE ?
				   oscroll_TOPENDZONE : oscroll_BOTTOMENDZONE),
				   view_LeftDown);
    } else {
	switch (self->direction) {
	    case oscroll_DOWN:
	        set_frame(self, self->side, what_is_at(self, self->side, self->hitcoord), 0);
		break;
	    case oscroll_UP:
		set_frame(self, self->side, what_is_at(self, self->side, self->hitcoord), bar_height(self, self->side));
		break;
	    }
    }

    draw_thumb_and_bar(self, self->side, TRUE);

}

static void RepeatScroll(self, cTime)
struct oscroll *self;
long cTime;
{
    struct oscrollbar *cur = NULL;
    long timeInterval;

    self->scrollEvent=NULL;
    
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	self->button |= SMOOTH;

	timeInterval = minContScrollTime + (((maxContScrollTime - minContScrollTime) * self->hitcoord) / (PIXELSPERINCH * 10));

	self->scrollEvent = im_EnqueueEvent((procedure) RepeatScroll, (char *) self, event_MSECtoTU(timeInterval));

	if (self->side != -1) {
	    cur = &self->current.bar[Type[self->side]];
	}

	if (self->button == (LEFT | SMOOTH))
	    set_frame(self, self->side, what_is_at(self, self->side, self->hitcoord), 0);
	else if (self->button == (RIGHT | SMOOTH))
	    set_frame(self, self->side, cur->seen.beg, self->hitcoord);

	self->thumbing = NOPE;

    } else {

	timeInterval = minContScrollTime +
	  (((maxContScrollTime - minContScrollTime) * self->hitcoord) /
	   (PIXELSPERINCH * 10));

	self->scrollEvent = im_EnqueueEvent(RepeatScroll, self,
					    event_MSECtoTU(timeInterval));

	RepeatEvent(self);
    }
}


struct view *normal_scroll__Hit(self, action, x, y, num_clicks)
struct oscroll *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    int posn = 0, status, side = 0, delta, i, endzones;
    long coord = 0, temp, y1, y2;
    struct oscrollbar *cur = NULL, *des = NULL;
    long logicalTop, logicalHeight, logicalPos;

    if (action == view_LeftDown || action == view_RightDown) {
	if (self->button != NEITHER)
	    return (struct view *)self;            /* We already are dealing with the other button. */

	if (x >= self->left 
	    && y >= self->top 
	    && x < self->left + self->width 
	    && y < self->top + self->height 
	    && self->child) {
	    struct view *retval = view_Hit(self->child, action, x - self->left, y - self->top, num_clicks);
	    return retval;
	}
	else if (x < self->left && (self->current.location & oscroll_LEFT)) {
	    cur = &self->current.bar[Type[side = oscroll__LEFT]];
	    posn = from_bar_to_range(self, side, cur, self->hitcoord = (y - self->top), &status);
	}
	else if (x >= self->left + self->width && (self->current.location & oscroll_RIGHT)) {
	    cur = &self->current.bar[Type[side = oscroll__RIGHT]];
	    posn = from_bar_to_range(self, side, cur, self->hitcoord = (y - self->top), &status);
	}
	else if (y < self->top && (self->current.location & oscroll_TOP)) {
	    cur = &self->current.bar[Type[side = oscroll__TOP]];
	    posn = from_bar_to_range(self, side, cur, self->hitcoord = (x - self->left), &status);
	}
	else if (y >= self->top + self->height && (self->current.location & oscroll_BOTTOM)) {
	    cur = &self->current.bar[Type[side = oscroll__BOTTOM]];
	    posn = from_bar_to_range(self, side, cur, self->hitcoord = (x - self->left), &status);
	}
	else
	    status = OUTSIDEBAR;

	switch (status) {
	    case OUTSIDEBAR:
		self->thumbing = NOPE;
		/* These -1's are dangerous. I am fairly certain they don't get used outside
		 * of this routine, and that they are handled properly within it. But you never
		 * know...
		 */
		self->side = -1;
		self->button = NEITHER;
		break;
	    case INTOPZONE:
	    case INBOTZONE:
		self->thumbing = NOPE;
		self->side = -1;
		self->button = NEITHER;
		endzone(self, side, status, action);
		break;
	    case INSIDEBAR:
		if (calc_elevator(self, side, cur, &temp, &y1, &temp, &y2) && y1 <= self->hitcoord && self->hitcoord <= y2) {
		    self->thumbing = MAYBE;
		    self->seenLength = cur->seen.end - cur->seen.beg;
		}
		else
		    self->thumbing = NOPE;
		self->side = side;

		if (action == view_LeftDown)
		    self->button = LEFT;
		else
		    self->button = RIGHT;
		self->hitposn = posn;

		if (startScrollTime > 0)  {
		    self->scrollEvent = im_EnqueueEvent((procedure)RepeatScroll, (char *) self, event_MSECtoTU(startScrollTime));
		}

		break;
	}
	return (struct view *)self;
    }

    /* Assure that we have the correct button */
    if (((action == view_LeftMovement || action == view_LeftUp) && ((self->button & LEFT) == 0)) || ((action == view_RightMovement || action == view_RightUp) && ((self->button & RIGHT) == 0)))
	return (struct view *)self;

    if (self->side != -1) {
	cur = &self->current.bar[Type[self->side]];
	des = &self->desired.bar[Type[self->side]];
	if (self->side == oscroll__LEFT || self->side == oscroll__RIGHT) {
	    logicalTop = self->top;
	    logicalHeight = self->height;
	    logicalPos = y;
	}
	else {
	    logicalTop = self->top;
	    logicalHeight = self->height;
	    logicalPos = x;
	}
	coord = logicalPos - logicalTop;
    }

    if (action == view_LeftMovement || action == view_RightMovement) {
	endzones = (cur->endzones ? self->endzoneLength : 0) + self->endbarSpace;

	switch (self->thumbing) {
		long newPos;

	    case MAYBE:
		if (ABS(coord - self->hitcoord) <= SMALLDIST) {
		    break;
		}
		
		self->hitposn = self->current.bar[Type[self->side]].seen.beg;

		self->thumbing = YES;
		if (self->scrollEvent != NULL) {
		    event_Cancel(self->scrollEvent);
		    self->scrollEvent = NULL;
		}
		cursor_SetStandard(self->cursor[self->side], ThumbIcon);
		im_SetWindowCursor(self->header.view.imPtr, self->cursor[self->side]);
		
		/* Fall through into the yes clause */

	    case YES:
		if (coord == self->hitcoord) {
		    /*
		     No movemove in the cursor - handles bottom of the
		     screen which for some reason the following code
			 doesn't get correct
		    */
		    break;
		}

		if (coord < self->hitcoord) {
		    if (self->hitposn == 0) {
			/*
			 The elevator is at the top of the bar and we want to
			 reset the hitcoordinate.
			 */

			if (coord < logicalTop + endzones) {
			    self->hitcoord = logicalTop + endzones;
			}
			else {
			    self->hitcoord = coord;
			}
			break;
		    }
		    else if (logicalPos > (logicalTop + logicalHeight - endzones)) {
			/*
			 We are moving back up from below the scroll bar
			 Do not start moving backwards until we are back in
			 the scroll bar area
			 */

			self->hitcoord = coord;
			break;
		    }
		}

		newPos = from_range_to_bar(self, self->side, cur, cur->seen.beg) +  coord - self->hitcoord;

		if (newPos < logicalTop + endzones)
		    newPos = logicalTop + endzones;
		if (newPos > logicalTop + logicalHeight - endzones)
		    newPos = logicalTop + logicalHeight - endzones;
		posn = from_bar_to_range(self, self->side, cur, newPos, &status);

		if (status == INSIDEBAR) {
		    delta = posn - self->hitposn;
		    if (delta != 0) {
			des->seen.beg += delta;
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


			for (i = 0; i < oscroll_SIDES; i++)  {
			    if ((self->current.location & (1<<i)) && Type[i] == Type[self->side])  {
				move_elevator(self, i);
				if (thumbScroll && self->button == LEFT)  {
				    set_frame(self, self->side, des->seen.beg, 0);
				}
			    }
			}
			*cur = *des;
			self->hitposn = posn;
			self->hitcoord = coord;
		    }
		    else if (self->hitposn == 0 && coord < self->hitcoord) {
			self->hitcoord = coord;
		    }
		}
		break;
	    case NOPE:
		if (adjustScroll)  {
		    self->hitcoord = coord;
		}
		break;
	}
	return (struct view *)self;
    }

    /* The action must be an up transition to get to here. */

    if (self->scrollEvent != NULL) {
	event_Cancel(self->scrollEvent);
	self->scrollEvent = NULL;
    }

    if (self->thumbing != NOPE) {
	cursor_SetStandard(self->cursor[self->side], CursorIcon[Type[self->side]]);
	im_SetWindowCursor(self->header.view.imPtr, NULL);

	if (ABS(coord - self->hitcoord) > SMALLDIST || (self->thumbing == YES && (self->button == RIGHT || ! thumbScroll))) {
	    long newPos = from_range_to_bar(self, self->side, cur, cur->seen.beg) + coord - self->hitcoord;

	    posn = from_bar_to_range(self, self->side, cur, newPos, &status);
	    if (status == INSIDEBAR) {
		delta = posn - self->hitposn;
		des->seen.beg += delta;
		if (des->seen.beg < des->total.beg)
		    des->seen.beg = des->total.beg;
		else if (des->seen.beg > des->total.end) {
		    des->seen.beg = des->total.end;
		}
	    }
	    set_frame(self, self->side, des->seen.beg, 0);
	    oscroll_WantUpdate(self, self);

	    self->thumbing = YES;
	}
    }


    if (self->thumbing != YES) {
	if (adjustScroll)  {
	    self->hitcoord = coord;
	}

	if (self->button == LEFT)
	    set_frame(self, self->side, what_is_at(self, self->side, self->hitcoord), 0);
	else if (self->button == RIGHT)
	    set_frame(self, self->side, cur->seen.beg, self->hitcoord);
    }

    self->button = NEITHER;
    self->thumbing = NOPE;

    return (struct view *)self;
}

struct view *motif_scroll__Hit(self, action, x, y, num_clicks)
struct oscroll *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    int posn = 0, status, side = 0, delta, i, endzones;
    long coord = 0, temp, y1, y2;
    struct oscrollbar *cur = NULL, *des = NULL;
    long logicalTop, logicalHeight, logicalPos;
    static int last_hit_side = 0;

    if (action == view_LeftDown || action == view_RightDown) {
	if (self->button != NEITHER)
	    /* We already are dealing with the other button. */
	    return (struct view *)self;

	if (x >= self->left 
	    && y >= self->top 
	    && x < self->left + self->width 
	    && y < self->top + self->height 
	    && self->child) {
	    struct view *retval = view_Hit(self->child, action, x - self->left,
					   y - self->top, num_clicks);
	    return retval;
	}
	else if (action == view_RightDown)
	    return (struct view *)self;

	else if (x < self->left && (self->current.location & oscroll_LEFT)) {
	    cur = &self->current.bar[Type[side = oscroll__LEFT]];
	    self->hitcoord = y - self->top;
	    posn = from_bar_to_range(self, side, cur, self->hitcoord, &status);
	}
	else if (x >= self->left + self->width &&
		 (self->current.location & oscroll_RIGHT)) {
	    cur = &self->current.bar[Type[side = oscroll__RIGHT]];
	    self->hitcoord = y - self->top;
	    posn = from_bar_to_range(self, side, cur, self->hitcoord, &status);
	}
	else if (y < self->top && (self->current.location & oscroll_TOP)) {
	    cur = &self->current.bar[Type[side = oscroll__TOP]];
	    self->hitcoord = x - self->left;
	    posn = from_bar_to_range(self, side, cur, self->hitcoord, &status);
	}
	else if (y >= self->top + self->height &&
		 (self->current.location & oscroll_BOTTOM)) {
	    cur = &self->current.bar[Type[side = oscroll__BOTTOM]];
	    self->hitcoord = x - self->left;
	    posn = from_bar_to_range(self, side, cur, self->hitcoord, &status);
	}
	else
	    status = OUTSIDEBAR;

	current_end_state = 0;

	switch (status) {
	    case OUTSIDEBAR:
		self->thumbing = NOPE;
		/* These -1's are dangerous. I am fairly certain they don't get used outside
		 * of this routine, and that they are handled properly within it. But you never
		 * know...
		 */
		self->side = -1;
		self->button = NEITHER;
		break;
	    case INTOPZONE:
	    case INBOTZONE:
		self->direction = (status == INTOPZONE) ?
		  oscroll_UP : oscroll_DOWN;

		current_end_state = status;
		last_hit_side = side;
		if ((&self->desired.bar[Type[side]])->endzones)
		    draw_endzones(self, side, bar_height(self, side),
				  current_end_state);
		endzone(self, side, (current_end_state == INTOPZONE ?
				     oscroll_TOPENDZONE : oscroll_BOTTOMENDZONE),
			action);
	    case INSIDEBAR:
		last_hit_side = side;
		if (calc_elevator(self, side, cur, &temp, &y1, &temp, &y2)
		    && y1 <= self->hitcoord && self->hitcoord <= y2) {
		    self->thumbing = MAYBE;
		    self->seenLength = cur->seen.end - cur->seen.beg;
		} else
		    self->thumbing = NOPE;

		self->side = side;

		/* above or below thumb ? */
		self->direction = oscroll_NEITHER;

		/* determine if above or below thumb  */
		if (self->hitcoord < y1)
		    self->direction = oscroll_UP;

		if (self->hitcoord > y2)
		    self->direction = oscroll_DOWN;

		self->side = side;

		if (action == view_LeftDown)
		    self->button = LEFT;
		else
		    self->button = RIGHT;

		self->hitposn = posn;

		RepeatEvent(self);

		if (startScrollTime > 0)  {
		    self->scrollEvent =
		      im_EnqueueEvent(RepeatScroll, self,
				      event_MSECtoTU(startScrollTime));
		}

		break;
	}
	return (struct view *)self;
    }

    /* Assure that we have the correct button */
    if (((action == view_LeftMovement || action == view_LeftUp) &&
	  ((self->button & LEFT) == 0)))
	return (struct view *)self;

    if (self->side != -1) {
	cur = &self->current.bar[Type[self->side]];
	des = &self->desired.bar[Type[self->side]];
	if (self->side == oscroll__LEFT || self->side == oscroll__RIGHT) {
	    logicalTop = self->top;
	    logicalHeight = self->height;
	    logicalPos = y;
	    /*	    coord = y - self->top; */
	} else {
	    logicalTop = self->top;
	    logicalHeight = self->height;
	    logicalPos = x;
	    /*	    coord = x - self->left; */
	}
	coord = logicalPos - logicalTop;
    }

    if (action == view_LeftMovement) {
	endzones = (cur->endzones ? self->endzoneLength : 0);

	switch (self->thumbing) {
		long newPos;

	    case YES:
		DEBUG(("YES hitc: %d coord: %d lt: %d lh: %d lp: %d\n", self->hitcoord, coord, logicalTop, logicalHeight, logicalPos));

		self->direction = oscroll_NEITHER;
		if (coord == self->hitcoord) {
		    /* no movement, punt */
		    break;
		}
		if (coord < self->hitcoord) {
		    if (self->hitposn == 0) {
			/* thumb at top, want to reset hitcoord */
			if (coord < logicalTop + endzones) {
			    self->hitcoord = logicalTop + endzones;
			} else {
			    self->hitcoord = coord;
			}
			break;
		    } else if (logicalPos > (logicalTop + logicalHeight -
					     endzones)) {
			/*
			 We are moving back up from below the scroll bar
			 Do not start moving backwards until we are back in
			 the scroll bar area
			 */
			self->hitcoord = coord;
			break;
		    }
		}
		newPos = from_range_to_bar(self, self->side, cur,
					   cur->seen.beg) + coord -
		  self->hitcoord;
		if (newPos < logicalTop + endzones - 9)
		    newPos = logicalTop + endzones - 9;
		if (newPos > logicalTop + logicalHeight - endzones)
		    newPos = logicalTop + logicalHeight - endzones;
		posn = from_bar_to_range(self, self->side, cur, newPos,
					 &status);
		

		if (status == INSIDEBAR || status == INBOTZONE) {

		    delta = posn - self->hitposn;
		    if (delta != 0) {
			DEBUG(("\tdelta: %d newPos: %d posn: %d y: %d\n", delta, newPos, posn, y));
			des->seen.beg += delta;
			if (des->seen.beg < des->total.beg) {
			    des->seen.beg = des->total.beg;
			    des->seen.end = MIN(des->total.end,
						des->seen.beg +
						self->seenLength);
			} else if (des->seen.beg > des->total.end) {
			    des->seen.beg = des->total.end;
			    des->seen.end = des->total.end;
			} else
			    des->seen.end = MIN(des->total.end,
						des->seen.beg +
						self->seenLength);
			DEBUG(("des.seen.beg: %d\n", des->seen.beg));

			for (i = 0; i < oscroll_SIDES; i++)  {
			    if ((self->current.location & (1<<i)) &&
				Type[i] == Type[self->side])  {
				draw_thumb_and_bar(self, i, FALSE);
				if (thumbScroll) {
				    set_frame(self, self->side, des->seen.beg, 0);
				}
			    }
			}
			*cur = *des;
			self->hitposn = posn;
			self->hitcoord = coord;
		    } else if (self->hitposn == 0 && coord < self->hitcoord) {
			self->hitcoord = coord;
		    }
		}
		break;
	    case MAYBE:
		self->direction = oscroll_NEITHER;
		if (ABS(coord - self->hitcoord) > SMALLDIST) {
		    self->hitcoord = coord;
		    self->hitposn =
		      self->current.bar[Type[self->side]].seen.beg;
		    DEBUG(("MAYBE hc: %d hp: %d y: %d\n", self->hitcoord, self->hitposn, y));
		    self->thumbing = YES;
		    if (self->scrollEvent != NULL) {
			event_Cancel(self->scrollEvent);
			self->scrollEvent = NULL;
		    }
		}
		break;

	    case NOPE:
		if (adjustScroll)  {
		    self->hitcoord = coord;
		}
		break;
	}
	return (struct view *)self;
    }

    /* The action must be an up transition to get to here. */

    current_end_state = 0;

    if ((&self->desired.bar[Type[last_hit_side]])->endzones)
	DEBUG(("last hit side %d\n",last_hit_side));
    draw_endzones(self, last_hit_side, bar_height(self, last_hit_side),
		   current_end_state);

    if (self->scrollEvent != NULL) {
	event_Cancel(self->scrollEvent);
	self->scrollEvent = NULL;
    }

    if (self->thumbing != NOPE) {
	cursor_SetStandard(self->cursor[0], NormalIcon);
	im_SetWindowCursor(self->header.view.imPtr, NULL);

	if (ABS(coord - self->hitcoord) > SMALLDIST) {
	    long newPos = from_range_to_bar(self, self->side, cur,
					    cur->seen.beg) + coord -
	      self->hitcoord;

	    posn = from_bar_to_range(self, self->side, cur, newPos, &status);

	    if (status == INSIDEBAR || status == INBOTZONE) {
		delta = posn - self->hitposn;
		des->seen.beg += delta;
		if (des->seen.beg < des->total.beg)
		    des->seen.beg = des->total.beg;
		else if (des->seen.beg > des->total.end) {
		    des->seen.beg = des->total.end;
		}
	    }

	    set_frame(self, self->side, des->seen.beg, 0);
	    DEBUG(("beg: %d\n", des->seen.beg));
	    oscroll_WantUpdate(self, self);

	    self->button = NEITHER;
	    self->thumbing = NOPE;

	    return (struct view *)self;
	}
    }

    self->thumbing = NOPE;

    if (self->thumbing != YES) {
	if (adjustScroll)  {
	    self->hitcoord = coord;
	}
    }

    if (self->button == LEFT && (action == view_LeftDown ||
				  action == view_LeftMovement))
	set_frame(self, self->side, what_is_at(self, self->side,
					       self->hitcoord), 0);
    else if (self->button == RIGHT)
	set_frame(self, self->side, cur->seen.beg, self->hitcoord);

    self->button = NEITHER;
    self->thumbing = NOPE;

    return (struct view *)self;
}

struct view *oscroll__Hit(self, action, x, y, num_clicks)
struct oscroll *self;
enum view_MouseAction action;
long x, y, num_clicks;
{
    if(!emulation) {
	/* NORMAL SCROLLBARS */
	return normal_scroll__Hit(self, action, x, y, num_clicks);
    } else {
	/* MOTIF SCROLLBARS */
	return motif_scroll__Hit(self, action, x, y, num_clicks);
    }
}

void oscroll__LinkTree(self, parent)
    struct oscroll *self;
    struct view *parent;
{

    super_LinkTree(self, parent);
    if (self->child)
        view_LinkTree(self->child, self);
}

void oscroll__UnlinkNotification(self, unlinkedTree)
    struct oscroll *self;
    struct view *unlinkedTree;
{

    updatelist_DeleteTree(self->updatelist, unlinkedTree);
    if(((struct view *)self)->parent) view_UnlinkNotification(((struct view *)self)->parent, unlinkedTree);
}

/* The 3-D drawing routines */

void motif_Draw3dBorder(v, x1, y1, x2, y2, sense, fillp)
struct view *v;
long x1, y1, x2, y2;
boolean sense;			/* "innie" or "outtie" */
struct graphic *fillp;		/* center fill pattern, NULL for none */
{
    motif_DrawBorder(v, x1, y1, x2, y2,
	       (sense) ? view_GrayPattern(v, TD_BGPATVAL, 16) :
		     view_GrayPattern(v, TD_FGPATVAL, 16),
	       (sense) ? view_GrayPattern(v, TD_FGPATVAL, 16) :
		     view_GrayPattern(v, TD_BGPATVAL, 16),
	       fillp,
	       TD_DEPTH);
}
    

void motif_DrawBorder(v, x1, y1, x2, y2, lt, dk, fillp, depth)
struct view *v;
long x1, y1, x2, y2;		/* enclosing coords */
struct graphic *lt, *dk, *fillp; /* patterns for light, dark and center */
int depth;			/* depth of border */
{

    long left, top, width, height;
    long left2, top2, width2, height2;
    long r2_bot, r_bot;

    left = (x1<x2 ? x1 : x2);
    top =  (y1<y2 ? y1 : y2);
    width = abs(x1-x2);
    height = abs(y1-y2);

    left2 = left + depth;
    top2 = top + depth;
    width2 = width - 2*depth;
    height2 = height - 2*depth;

    view_FillRectSize(v, left, top,
			depth, height, lt);
    
    view_FillRectSize(v, left + width - depth,
			top, depth, height, dk);
    
    r_bot = top + height;
    
    r2_bot = top2 + height2;

    if (fillp)
	view_FillRectSize(v, left2, top2,
			    width2, height2,
			    fillp);
    
    view_FillTrapezoid(v, left2, r2_bot, width2,
			 left, r_bot, width, dk);
    
    view_FillTrapezoid(v, left, top, width,
			 left2, top2, width2, lt);

    
    /* stolen from linkview */

}
