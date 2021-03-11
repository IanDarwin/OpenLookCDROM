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

#define scroll_VERSION 3

/* The different locations scrollbars can appear in. For more than one scrollbar, or the values of the single underscore constants together. */
#define scroll_SIDES 4
#define scroll__LEFT 0
#define scroll__RIGHT 1
#define scroll__TOP 2
#define scroll__BOTTOM 3
#define scroll_LEFT (1<<scroll__LEFT)
#define scroll_RIGHT (1<<scroll__RIGHT)
#define scroll_TOP (1<<scroll__TOP)
#define scroll_BOTTOM (1<<scroll__BOTTOM)

/* The different types of scrollbars supported. Currently only vertical and horizontal */
#define scroll_TYPES 2
#define scroll_VERT 0
#define scroll_HORIZ 1

#define scroll_REPEATING (-1)
#define scroll_THUMBING (-2)
#define scroll_MAYBETHUMBING (-3)
#define scroll_NOTHING (0)

/* The endzone codes */
#define scroll_TOPENDZONE 1
#define scroll_BOTTOMENDZONE 2
#define scroll_MOTIFTOPENDZONE (-1)
#define scroll_MOTIFBOTTOMENDZONE (-2)

/* constants for the motif behavior */
#define scroll_NOWHERE 0
#define scroll_UP 1
#define scroll_DOWN 2

struct scrollfns {
    void (*GetInfo)(/* self, total, seen, dot */);
    void (*SetFrame)(/* self, posn, coord, outof */);
    void (*EndZone)(/* self, zone, action */);
    long (*WhatIsAt)(/* self, coord, outof */);
};

struct range {
    long beg, end;		/* The beggining and the end of the range. */
};

struct scrollbar {
    struct range
        total,			/* The total range of the scrollee */
        seen,			/* The portion that is seen */
        dot;			/* The portion that is selected. */
    int endzones;		/* TRUE iff the endzones are displayed. */
};

struct scrollstate {
    int location;
    struct scrollbar bar[scroll_TYPES];
};

class scroll : view {
  overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    WantUpdate(struct view *requestor);
    LinkTree(struct view *parent);
    UnlinkNotification(struct view *unlinkedTree);

  methods:
    /* A ``location'' is a bitmask of scroll_LEFT, etc. describing the places for the scrollbars to appear. The location returned by GetLocation is the desired location, while GetCurrentLocation returns the real condititions based on the size of the region were in. */
    SetLocation(int location);
    GetLocation() returns int;
    GetCurrentLocation() returns int;

    /* The parameters consist of:
         endzone: The minimum number of pixles the scrollbar must have to display the endzones.
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
    
    SetElevatorWidth(long newWidth);
    GetElevatorWidth() returns long;
    SetWindowPadding(long newPaddig);
    GetWindowPadding() returns long;
    SetViewPadding(long newPadding);
    GetViewPadding() returns long;
    
  classprocedures:
    InitializeClass() returns boolean;

    /* Allocates a new set of scrollbars, and sets the view and location */
    Create(struct view *view, int location) returns struct scroll *;
    CreateScroller(struct view *view, int location, char *scrollClass) returns struct scroll *;
    InitializeObject(struct scroll *self) returns boolean;
    FinalizeObject(struct scroll *self);

  data:
    struct view *child;
    struct view *scrollee;
    struct scrollstate current, desired;/* The state currently displayed and the state to display on the next update */

    struct scrollfns *fns[scroll_TYPES];/* Pointers to the interface structures. */

    struct updatelist *updatelist;	/* The list of all decendents that want updates. */

/* Excuse the structure packing. Efficiency never hurts... */
    short endzone_threshold;		/* if height < this val, endzones disapear. */
    short bar_threshold;		/* if height < this val, bar disapears. */
    short min_elevator[2];		/* min size for elevator w/o and w/ endzones. */

    short endzoneLength;		/* length of the endzone */
    short endbarSpace;			/* space between endzeone and rest of scroll bar */
    short buttonSpace;			/* space between button and bar */
    short barWidth;			/* Width of the scroll bar */
    short dotWidth;			/* Width of Dot indicator */
    short elevatorWidth;		/* Width of the elevator */
    short windowPadding;		/* Padding between window border and scrollbar or view */
    short viewPadding;			/* Padding between view  and scrollbars */
    char pending_update;		/* TRUE iff we have an update that is pending */
    char ideal_location;		/* The ideal location(s) for the bars */
    char force_full_update;             /* TRUE if we must do a full update. */
    char force_get_interface;           /* TRUE if the scrollee has changed since the last time we got the scroll functions. */
    short side;
    int hitcoord;
    long seenLength;		/* Used for thumbing to get handle end of the scroll bar */

    struct cursor *cursor;
    struct event *scrollEvent;
    struct sbutton_prefs *prefs, *boxprefs, *matteprefs, *buttonprefs, *elevatorprefs, *dotprefs;
    struct rectangle childrect;
    struct rectangle fullinterior;
    struct rectangle interiors[scroll_SIDES];
    struct rectangle topbutton[scroll_SIDES];
    struct rectangle botbutton[scroll_SIDES];
    boolean lastbuttonstate;
    long lastwidth, lastheight;
    enum view_MouseAction lastaction;
    double barbackground[3];
    double mattebackground[3];
    int mousestate; /* one of scroll_TOPENDZONE, scroll_BOTTOMENDZONE, scroll_REPEATING, scroll_THUMBING, scroll_MAYBETHUMBING, scroll_NOTHING */
    long startScrollTime;
    long minContScrollTime;
    long maxContScrollTime;
    long endzonereptime;
    boolean adjustScroll;
    boolean thumbScroll;
    int dir; /* one of scroll_NOWHERE, scroll_UP or scroll_DOWN */
    boolean emulation;
    boolean drawborder;
    boolean prefsready;
    struct cursor *barcursor[scroll_TYPES];
};
