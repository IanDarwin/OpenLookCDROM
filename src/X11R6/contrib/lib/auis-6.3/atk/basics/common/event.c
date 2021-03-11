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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/event.c,v 2.10 1994/02/07 18:40:19 rr2b Exp $";
#endif


 

#include <class.h>
#define EVENT_IMPLEMENTATION
#include <event.eh>

#include <andrewos.h> /* sys/time.h */

static struct event *timerQueue = NULL;
static long currSec;			/* seconds since last StartTimer call */
static long tuBase;
static boolean timeInited = FALSE;
static long MSEC10 = event_MSECtoTU(10);
static long id=1;

static struct event *freeList;

boolean event__InitializeObject(classID, self)
    struct classheader *classID;
    struct event *self;
{
    self->t = event_ENDOFTIME;
    self->proc = NULL;
    self->procdata = NULL;
    self->next = NULL;
    /* at 136 events allocated per second this will overflow in a year.
     Most unix timer's only have granularity down to 1/100th of a second
     so this should be fine. -rr2b */
    self->id=id++;
    if(id==0) id++;
    return TRUE;
}

struct event *event__Allocate(classID)
    struct classheader *classID;
{
    register struct event *e;

    if (freeList == NULL)
	e = (struct event *) malloc(sizeof(struct event));
    else  {
	e = freeList;
	freeList = freeList->next;
    }
    return e;
}

void event__Deallocate(classID, self)
struct classheader *classID;
struct event *self;
{
    self->next = freeList;
    freeList = self;
}

void event__Cancel(classID, self)
struct classheader *classID;
struct event *self;
{
    register struct event *prev = NULL;
    register struct event *x;
    for (x = timerQueue; x != NULL && (long)self != x->id; x=x->next) 
	prev=x;
    if (x == NULL) {
	fprintf(stderr, "Warning: unallocated event cancelled!\n");
	return;
    }
    if (prev)
	prev->next = x->next;
    else
	timerQueue = x->next;
    event_Destroy(x);
}

struct event *event__Enqueue(classID, time, proc, procdata)
    struct classheader *classID;
    long time;
    int (*proc)();
    char *procdata;
{
    register struct event *e;

    e = event_New();
    e->t = time;
    e->proc = proc;
    e->procdata = procdata;
    /* enqueue in sequence by time of happening */

    if (timerQueue == NULL) {
	timerQueue = e;
    }
    else {
	register struct event *prev = NULL,
			*x = timerQueue;

	for (; x != NULL && time > x->t; prev=x, x=x->next) ;
	if (prev) {
	    e->next = prev->next;
	    prev->next=e;
	}
	else {
	    e->next = timerQueue;
	    timerQueue = e;
	}
    }
    return (struct event *)e->id;
}

void event__ForceNext(classID)
    struct classheader *classID;
{
    /* set time so next event will occur
    this routine changes the value that will be
    returned by the next call on now()  */

    if (timerQueue == NULL) return;
    tuBase = timerQueue->t;
}

long event__FirstTime(classID, currentTime)
    struct classheader *classID;
    long currentTime;
{
    /* returns the time remaining to first event on queue */

    return ((timerQueue == NULL) ? event_ENDOFTIME : timerQueue->t - currentTime);
}

void event__StartTimer(classID)
    struct classheader *classID;
{
    /* start timer for elapsed time
    units are   microseconds >>6  (max of 64000 sec) */

    struct osi_Times tp;
    register struct event *e = timerQueue;

    osi_GetTimes(&tp);
    if (timerQueue) 
	if (timeInited) {
	    /* reduce every time by 'now' */

	    register long oldNow, deltaSec;

	    deltaSec = tp.Secs - currSec;
	    currSec += deltaSec;
	    tuBase += event_SECtoTU(deltaSec);
	    oldNow = tuBase + event_USECtoTU(tp.USecs);
	    for (; e; e=e->next)
		e->t -= oldNow;
	} else {
	    /* queue is bogus, clear all times */

	    for ( ; e; e = e->next)
		e->t = 0;
	}
    currSec = tp.Secs;
    tuBase = - event_USECtoTU(tp.USecs);
    timeInited = TRUE;
}

long event__HandleTimer(classID, currentTime)
    struct classheader *classID;
    long currentTime;
{
    /* there are elements on timer queue.  process first if it is
    time (or if it will be time within 10 msec).
    return time to wait before next event  */

    register long twait;
    register struct event *e = timerQueue;
    static long lastCurrentTime;
    
    if (timerQueue == NULL)
	return event_ENDOFTIME;

    twait = e->t - currentTime;		/* time to wait for next event */
    if (twait > MSEC10)
	return (twait);

    /* handle first event on queue */
    lastCurrentTime=currentTime;
    timerQueue = e->next;
    (*e->proc)(e->procdata, currentTime);
    event_Destroy(e);
    
    if (timerQueue == NULL) return event_ENDOFTIME;
    twait = timerQueue->t - lastCurrentTime - MSEC10;
    return (twait>0 ? twait : 0);
}

long event__Now(classID)
    struct classheader *classID;
{
    /* returns time relative to last StartTimer 
    units are   microseconds >>6  (max of 64000 sec) 
    This routine returns timeintervals good for about a day;
    for longer execution times (as in a server) there
    ought to be an ocassional call on StartTimer().
    If times are only used for EnqueueEvent, StartTimer
    can be called at any time, the queue will be updated. */

    struct osi_Times tp;
    register long deltaSec;

    if (! timeInited)
	event_StartTimer(); 
    osi_GetTimes(&tp);
    if ((deltaSec=tp.Secs-currSec) )  {
	/* second hand has skipped to next second
	   record new second hand position and update 
	   the current time value. */

	currSec += deltaSec;
	if (deltaSec>1) 
	    tuBase += event_SECtoTU(deltaSec);
	else tuBase += (1000000>>6);
    }
    return (tuBase + event_USECtoTU(tp.USecs));

}

