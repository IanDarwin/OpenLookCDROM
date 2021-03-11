/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 


#include <point.h>

#define oscroll_VERSION 3

/* The different locations scrollbars can appear in. For more than one scrollbar, or the values of the single underscore constants together. */
#define oscroll_SIDES 4
#define oscroll__LEFT 0
#define oscroll__RIGHT 1
#define oscroll__TOP 2
#define oscroll__BOTTOM 3
#define oscroll_LEFT (1<<oscroll__LEFT)
#define oscroll_RIGHT (1<<oscroll__RIGHT)
#define oscroll_TOP (1<<oscroll__TOP)
#define oscroll_BOTTOM (1<<oscroll__BOTTOM)

/* The different types of scrollbars supported. Currently only vertical and horizontal */
#define oscroll_TYPES 2
#define oscroll_VERT 0
#define oscroll_HORIZ 1

/* The endzone codes */
#define oscroll_TOPENDZONE 1
#define oscroll_BOTTOMENDZONE 2

/* the arrows in the arrow array */
#define ARROW_UP	0
#define ARROW_LEFT	1

struct arrow {
    struct point pa[6];
};

struct oscrollfns {
    void (*GetInfo)(/* self, total, seen, dot */);
    void (*SetFrame)(/* self, posn, coord, outof */);
    void (*EndZone)(/* self, zone, action */);
    long (*WhatIsAt)(/* self, coord, outof */);
};

struct orange {
    long beg, end;		/* The beggining and the end of the range. */
};

struct oscrollbar {
    struct orange
        total,			/* The total range of the scrollee */
        seen,			/* The portion that is seen */
        dot;			/* The portion that is selected. */
    int endzones;		/* TRUE iff the endzones are displayed. */
};

struct oscrollstate {
    int location;
    struct oscrollbar bar[oscroll_TYPES];
};

class oscroll : scroll {
  overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    WantUpdate(struct view *requestor);
    LinkTree(struct view *parent);
    UnlinkNotification(struct view *unlinkedTree);

    /* A ``location'' is a bitmask of scroll_LEFT, etc. describing the places for the scrollbars to appear. The location returned by GetLocation is the desired location, while GetCurrentLocation returns the real condititions based on the size of the region were in. */
    SetLocation(int location);
    GetLocation() returns int;
    GetCurrentLocation() returns int;

    /* The parameters consist of:
         endzone: The minimum number of pixels the scrollbar must have to display the endzones.
         bar: The minimum number of pixles to try to put a scrollbar in.
         without: The minimum size for the elevator given that the scrollbar is currently without endzones.
         with: The minimum size of the elevator when the scrollbar is displayed with endzones. */
    SetParameters(long endzone, long bar, int without, int with);
    GetParameters(long *endzone, long *bar, int *without, int *with);

    SetWidth(long newWidth);
    GetWidth() returns long;
    SetDotWidth(long newWidth);
    GetDotWidth() returns long;
    SetEndZoneLength(long newHeight);
    GetEndZoneLength() returns long;
    SetEndToBarSpace(long space);
    GetEndToBarSpace() returns long;

    /* The ``child'' is the view directly under the scrollbars, while the ``scrollee'' is the view being scrolled. Normally, both of these are the same, and set with the SetView method. SetChild and SetScrollee can be used to change them individually. */
    SetView(struct view *view);
    SetChild(struct view *child);
    SetScrollee(struct view *scrollee);
    GetChild() returns struct view *;
    GetScrollee() returns struct view *;

  classprocedures:
    InitializeClass() returns boolean;

    /* Allocates a new set of scrollbars, and sets the view and location */
    Create(struct view *view, int location) returns struct oscroll *;
    InitializeObject(struct oscroll *self) returns boolean;
    FinalizeObject(struct oscroll *self);

  data:
    struct view *child;
    struct view *scrollee;
    struct oscrollstate current, desired;/* The state currently displayed and the state to display on the next update */

    struct oscrollfns *fns[oscroll_TYPES];/* Pointers to the interface structures. */

    struct updatelist *updatelist;	/* The list of all decendents that want updates. */

    long left, top, width, height;	/* The coords of the child inset rel to me. Not in the state structure because they are calculated from things in the state structure. */

/* Excuse the structure packing. Efficiency never hurts... */
    short endzone_threshold;		/* if height < this val, endzones disapear. */
    short bar_threshold;		/* if height < this val, bar disapears. */
    short min_elevator[2];		/* min size for elevator w/o and w/ endzones. */

    short endzoneLength;		/* length of the endzone */
    short endbarSpace;			/* space between endzeone and rest of scroll bar */
    short barWidth;			/* Width of the scroll bar */
    short dotWidth;			/* Width of Dot indicator */

    char pending_update;		/* TRUE iff we have an update that is pending */
    char ideal_location;		/* The ideal location(s) for the bars */
    char thumbing;
    char button;
    char force_full_update;             /* TRUE if we must do a full update. */
    char force_get_interface;           /* TRUE if the scrollee has changed since the last time we got the scroll functions. */
    short side;
    int hitcoord;
    int hitposn;
    long seenLength;		/* Used for thumbing to get handle end of the scroll bar */

    struct cursor *cursor[oscroll_SIDES];
    
    struct graphic *cornerFill;
    struct graphic *endzoneFill;
    struct graphic *barFill;
    struct graphic *elevatorFill;
    struct graphic *whiteFill;
    struct event *scrollEvent;
    int direction;		/* used for the endzone arrows */
    /* array[2] of array[6] of points for a left-pointing and an up-pointing
       arrow.   w.r.t. a (0,0) origin at the lower left of a bouding box.
       Basically, offsets to be used when drawing the arrows */
    struct arrow arrows[2];
};

/* Fake a get view routine. */
#define oscroll_GetView oscroll_GetScrollee

void motif_Draw3dBorder();
void motif_DrawBorder();

/* package of routines for doing 3-D boxes and stuff like that */

#define TD_DEPTH 	2	/* width of 3d border */
#define TD_GAP		7	/* space from edge of view to 3d border */
#define TD_BORDER	TD_DEPTH/* distance from edge of view's border to */
				/* edge of view */
/* 8, 12, 16 */

#define TD_FGPATVAL	8	/* recommended foreground pattern */
#define TD_BACKPATVAL	4	/* recommended nominal background pattern */
#define TD_BGPATVAL	0	/* recommended background pattern */

#define TD_FGPAT(self)	(oscroll_GrayPattern(self,TD_FGPATVAL,16))
#define TD_BGPAT(self)	(oscroll_GrayPattern(self,TD_BGPATVAL,16))
#define TD_BACKPAT(self) (oscroll_GrayPattern(self,TD_BACKPATVAL,16))
