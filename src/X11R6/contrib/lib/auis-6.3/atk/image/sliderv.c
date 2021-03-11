

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/sliderv.c,v 1.5 1992/12/15 21:36:09 rr2b R6tape $";
#endif
#include <class.h>

#include <dataobj.ih>
#include <view.ih>
#include <graphic.ih>
#include <slider.ih>
#include <scroll.ih>
#include <sliderv.eh>

#define RANGE		10000
#define SKIP		(self->range/10)
#define SMALLSKIP	(SKIP/10)

/* Creation and Destruction routines. */

boolean sliderv__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

boolean sliderv__InitializeObject(classID, self)
struct classheader *classID;
struct sliderv *self;
{
    self->scrollbar = scroll_Create(NULL, scroll_LEFT );
    self->curval = 0;
    self->range = RANGE;
    scroll_SetChild(self->scrollbar, NULL);
    scroll_SetScrollee(self->scrollbar, self);
    return TRUE;
}
    
void sliderv__FinalizeObject(classID, self)
struct classheader *classID;
struct sliderv *self;
{
    if (self->scrollbar) {
	scroll_UnlinkTree(self->scrollbar);
	scroll_Destroy(self->scrollbar);
    }
}

/* Overrides of the view routines: */

void sliderv__FullUpdate(self, type, left, top, width, height)
struct sliderv *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle r;

    if(self->scrollbar) {
       sliderv_GetLogicalBounds(self, &r);
       scroll_InsertView(self->scrollbar, self, &r);
       scroll_FullUpdate(self->scrollbar, type, left, top, width, height);
    }
}

void sliderv__Update(self)
struct sliderv *self;
{
    int i;
    struct rectangle r;

    if(self->scrollbar) scroll_Update(self->scrollbar);
    /* super_Update(self); */
}

void sliderv__WantUpdate(self, requestor)
    struct sliderv *self;
    struct view *requestor;
{
    super_WantUpdate(self, requestor);
    super_WantUpdate(self, self);
}

struct view *
sliderv__Hit(self, action, x, y, numberOfClicks)
    struct sliderv *self;
    enum view_MouseAction action;
    long x, y;
    long numberOfClicks;
{
    if(self->scrollbar) {
	scroll_Hit(self->scrollbar, action, x, y, numberOfClicks);
	return self;
    }
    return NULL;
}

static long sv_whatisat();
static void sv_getinfo(), sv_setframe(), sv_endzone();
static struct scrollfns scrollInterface =
	{ sv_getinfo, sv_setframe, sv_endzone, sv_whatisat };

char *sliderv__GetInterface(self, interfaceName)
struct sliderv *self;
char *interfaceName;
{
    return (char *) &scrollInterface;
}

static void sv_getinfo(self, total, seen, dot)
struct sliderv *self;
struct range *total, *seen, *dot;
{
	total->beg = 0;
	total->end = self->range;

	seen->beg = self->curval;
	seen->end = self->curval + self->range/100;
	dot->beg = dot->end = -1;
}

static long sv_whatisat(self, numerator, denominator)
 struct sliderv *self;
 long numerator, denominator;
{
	register long r = self->curval + (numerator*SKIP)/denominator;
	return r;
}

static void sv_setframe(self, position, numerator, denominator)
    struct sliderv *self;
    long position, numerator, denominator;
{
    sliderv_SetCurval(self, position - (numerator*SKIP)/denominator);
}

static void sv_endzone(self, end, action)
struct sliderv *self;
int end;
enum view_MouseAction action;
{
    if (action == view_LeftDown) {
	if(end == scroll_TOPENDZONE || end == scroll_MOTIFTOPENDZONE) {
		if(self->curval != 0) sv_setframe(self, 0, 0, self->range);
	} else { /* BOTTOM */
		if(self->curval != self->range) sv_setframe(self, self->range, 0, self->range);
	}
    } else if (action == view_RightDown) {
	if(end == scroll_TOPENDZONE || end == scroll_MOTIFTOPENDZONE) {
		sv_setframe(self, self->curval - SMALLSKIP, 0, self->range);
	} else {/* BOTTOM */
		sv_setframe(self, self->curval + SMALLSKIP, 0, self->range);
	}
    }
}

void sliderv__LinkTree(self, parent)
struct sliderv *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if (self->scrollbar)
	scroll_LinkTree(self->scrollbar, self);
}

void sliderv__UnlinkNotification(self, unlinkedTree)
struct sliderv *self;
struct view *unlinkedTree;
{
    super_UnlinkNotification(self, unlinkedTree);
}

/*---------------------*/

void
sliderv__SetCurval(self, curval)
    struct sliderv *self;
    long curval;
{
	if(curval < 0)
		curval = 0;
	if(curval > self->range)
		curval = self->range;
	self->curval = curval;
	sliderv_DoCallback(self, self->curval);
	sliderv_WantUpdate(self, self);
}

long
sliderv__GetCurval(self)
struct sliderv *self;
{
	return self->curval;
}

void
sliderv__SetCallback(self, callback, rock)
    struct sliderv * self;
    long (*callback)();
    long rock;
{
    self->callback = callback;
    self->rock = rock;
}

void
sliderv__DoCallback( self, arg)
    struct sliderv *self;
    long arg;
{
    if(self->callback)
	(*self->callback)(self->rock, arg);
}
