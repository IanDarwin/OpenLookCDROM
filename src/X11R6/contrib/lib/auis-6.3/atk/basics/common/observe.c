/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */


/* Copyright 1992 Carnegie Mellon University, All rights reserved.
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

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/observe.c,v 2.17 1993/12/31 04:40:14 rr2b Exp $ */

#ifndef NORCSID
char *observe_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/observe.c,v 2.17 1993/12/31 04:40:14 rr2b Exp $";
#endif

#include <class.h>
#include <observe.eh>
#include <atom.ih>
#include <atomlist.ih>
#include <owatch.ih>

#define INITIALNUMOBSERVERS 4


/* a triggerinstance is a particular function to be called when a trigger fires */
struct triggerinstance {
	struct triggerinstance *next;
	void(*func)();
	struct basicobject *rcvr;
	long rock;
};

/* a triggerhousing is the intersection of a trigger atom with a particular object
	it contains the list of functions to be called when the trigger is pulled */
struct triggerhousing {
	struct triggerhousing *next;
	struct atom *trigger;
	struct triggerinstance *instances;
	long disablecount;	/* enable if zero. disabled if > 0 */
	boolean firepending;	/* set True if fired while disabled */
};

/* a triggerclass is the list of all triggers defined for a class */
struct triggerclass {
	struct triggerclass *next;
	struct classinfo *class;   /* class_GetType(instance) */
	struct atomlist *triggers;
};


static struct triggerclass *Triggers = NULL;	/* the list of defined triggers */




boolean observable__InitializeObject(classID, self)
struct classheader *classID;
struct observable *self;
{
    self->nObservers = 0;
    self->maxObservers = 0;
    self->observers = NULL;
    self->triggers = NULL;
    return TRUE;
}

void observable__FinalizeObject(classID, self)
struct classheader *classID;
struct observable *self;
{
    struct triggerinstance *ti, *tit;
    struct triggerhousing *th, *tht;
    observable_NotifyObservers(self, observable_OBJECTDESTROYED);
    if (self->observers)
	free(self->observers);

    /* free all triggerinstances and triggerhousings hanging from self->triggers */
    for (th = self->triggers; th != NULL; th = tht) {
	for (ti = th->instances; ti != NULL; ti = tit) {
		tit = ti->next;
		free(ti);
	}
	tht = th->next;
	free(th);
    }
}

/* Finds the index of the observer in self observers table.  Returns -1 if observer is not in the list
 */
static int FindObserver(self, observer)
register struct observable *self;
register struct observable *observer;  {
    register int i = 0;
    struct observable **observers;

    for (i = 0, observers = self->observers; i < self->nObservers; i++, observers++)
	if (*observers == observer) return i;
    
    return -1;
}

boolean observable__IsObserver(self, observer)
register struct observable *self;
register struct observable *observer;  {
    return (FindObserver(self, observer) != -1 ? TRUE: FALSE);
}

void observable__AddObserver(self, observer)
register struct observable *self;
register struct observable *observer;  {
    if (self->maxObservers == 0)  {
	self->maxObservers = INITIALNUMOBSERVERS;
	self->observers = (struct observable **) malloc (INITIALNUMOBSERVERS * sizeof(struct observable *));
    }
    else if (FindObserver(self, observer) != -1) return;
    else if (self->nObservers == self->maxObservers)  {
	self->maxObservers += self->maxObservers / 2;
	self->observers = (struct observable **) realloc(self->observers, self->maxObservers * sizeof(struct observable *));
    }
    self->observers[self->nObservers++] = observer;
}

void observable__RemoveObserver(self, observer)
register struct observable *self;
register struct observable *observer;  {
    register int i;

    if ((i = FindObserver(self, observer)) != -1)  {
	while (++i < self->nObservers) {
	    self->observers[i - 1] = self->observers[i];
	}
	self->nObservers -= 1;
    }
}

void observable__NotifyObservers(self, value)
register struct observable *self;
long value;  {
    register struct observable **observers;
    register int i;

    if(observable_ReferenceCount(self)>0) observable_Reference(self);
    /*
      We go backwards since objects may be removed from this list
      as we go along.
      */

    for (i = self->nObservers - 1;  i >= 0; i--) {
	observable_ObservedChanged(self->observers[i], self, value);
    }
    if(observable_ReferenceCount(self)>0) observable_Destroy(self);
}


void observable__ObservedChanged(self, changed, value)
struct observable *self;
struct observable *changed;
long value;  {
    
}
	/* the following methods implement a scheme for "triggers", 
		a set of named messages,  each trigger must first be
		defined with a call to DefineTrigger*/



/* observable__DefineTrigger(classID, classinstance, trigger)
	associate the atom as a possible trigger for the class 
*/
	void
observable__DefineTrigger(classID, classinstance, trigger)
	struct classheader *classID;
	struct basicobject *classinstance;
	struct atom *trigger;
{
	struct triggerclass *tc;
	struct classinfo *info;
	info = class_GetType(classinstance);
	for (tc = Triggers; 
			tc != NULL && tc->class != info;
			tc = tc->next)
		{};
	if (tc == NULL) {
		tc = (struct triggerclass *)malloc(sizeof (struct triggerclass));
		tc->class = info;
		tc->triggers = atomlist_New();
		tc->next = Triggers;
		Triggers = tc;
	}
	if ( ! atomlist_Memberp(tc->triggers, trigger))
		atomlist_Prepend(tc->triggers, trigger);
}

/* observable__ListTriggers(classID, classinstance)
	returns a list of the triggers defined for the class 
	the caller must destroy the list 
*/
	struct atomlist *
observable__ListTriggers(classID, classinstance)
	struct classheader *classID;
	struct basicobject *classinstance;
{
	struct triggerclass *tc;
	struct classinfo *info;
	struct atomlist *result;
	result = atomlist_New();
	info = class_GetType(classinstance);

	for (tc = Triggers; tc != NULL; tc = tc->next)
		if (class_IsType(info, tc->class))
			atomlist_JoinToEnd(result, tc->triggers);
	return result;
}


/* observable__AddRecipient(self, trigger, rcvr, func, rock)
	when the trigger is Pull'ed, the func will be called thus:
		func(rcvr, self, rock)
*/
	boolean
observable__AddRecipient(self, trigger, rcvr, func, rock)
	struct observable *self;
	struct atom *trigger;
	void (*func)();
	struct basicobject *rcvr;
	long rock;
{
	struct triggerclass *tc;
	struct triggerhousing *th;
	struct triggerinstance *ti;
	for (th	= self->triggers; th !=	NULL &&	th->trigger != trigger; th = th->next) {};
	if (th == NULL) {
	    /* find out if the trigger is defined */
	    /* we have to loop through ALL the classes since triggers should be inheritable but some triggers may be defined for the superclass and others for the subclass */
	    for (tc = Triggers;  tc != NULL; tc = tc->next) {
		if(class_IsType(self, tc->class) && atomlist_Memberp(tc->triggers, trigger)) break;
	    };
	    if (tc == NULL)
		/* not defined */
		return FALSE;
	    /* make a new trigger housing: this is self's first recipient for this trigger */
	    th = (struct triggerhousing *)malloc(sizeof(struct triggerhousing));
	    th->trigger = trigger;
	    th->instances = NULL;
	    th->disablecount = 0;
	    th->firepending = FALSE;
	    th->next = self->triggers;
	    self->triggers = th;
	}
	/* th now points to an appropriate triggerhousing */

	/* find out if this is a redefinition */
	for (ti = th->instances; ti != NULL; ti = ti->next)
		if (ti->rcvr == rcvr) {
			ti->func = func;
			ti->rock = rock;
			return TRUE;
		}
	/* add a new trigger instacne */
	ti = (struct triggerinstance *)malloc(sizeof(struct triggerinstance));
	ti->func = func;
	ti->rcvr = rcvr;
	ti->rock = rock;
	ti->next = th->instances;
	th->instances = ti;
	return TRUE;
}

/* observable__DeleteRecipient(self, trigger, rcvr)
	removes the receiver from the list of recipients
*/
	void
observable__DeleteRecipient(self, trigger, rcvr)
	struct observable *self;
	struct atom *trigger;
	struct basicobject *rcvr;
{
	struct triggerhousing *th;
	struct triggerinstance *ti, *pi;
	for (th = self->triggers; th != NULL && th->trigger != trigger; th = th->next) 
		{};
	if (th == NULL)  return;
	
	for (pi = NULL, ti = th->instances; ti != NULL && ti->rcvr != rcvr; pi = ti, ti = ti->next)
		{};
	if (ti == NULL) return;

	if (pi == NULL)
		th->instances = ti->next;
	else
		pi->next = ti->next;
	free(ti);
}


/* observable__PullTrigger(self, trigger)
	call all funcs associated with this trigger on this object
*/
	void
observable__PullTrigger(self, trigger)
	struct observable *self;
	struct atom *trigger;
{
	struct triggerhousing *th;
	struct triggerinstance *ti;
	struct owatch_data *w1;
	for (th = self->triggers; th != NULL && th->trigger != trigger; th = th->next) 
		{};
	if (th == NULL)
		return;
	if (th->disablecount > 0) {
		th->firepending = TRUE;
		return;
	}
	/* add check to see if this object gets destroyed by the
	 trigger recipient. */
	w1=owatch_Create(self);
	for (ti = th->instances; ti != NULL; ti = ti->next) {
	    (ti->func)(ti->rcvr, self, ti->rock);
	    if(!owatch_Check(w1)) return;
	}
	owatch_Delete(w1);
}

	/* if a client is calling a number of operations which would pull a trigger 
		too many times, it can disable the trigger temporarily.  
		It must later Enable the trigger.  At that time one call back 
		is made for the trigger if it has been Pulled one or more times in the interim.  */

/* observable__DisableTrigger(self, trigger)
	until Enabled, this trigger will no produce call backs 
			Enable MUST be called once 
			for each time Disable has been called.
*/
	void
observable__DisableTrigger(self, trigger)
	struct observable *self;
	struct atom *trigger;
{
	struct triggerhousing *th;
	for (th = self->triggers; th != NULL && th->trigger != trigger; th = th->next) 
		{};
	if (th == NULL)
		return;
	if (th->disablecount == 0)
		th->firepending = FALSE;
	th->disablecount ++;
}

/* observable__EnableTrigger(self, trigger)
	this trigger will once again produce call backs
*/
	void
observable__EnableTrigger(self, trigger)
	struct observable *self;
	struct atom *trigger;
{
	struct triggerhousing *th;
	for (th = self->triggers; th != NULL && th->trigger != trigger; th = th->next) 
		{};
	if (th == NULL)
		return;
	if (th->disablecount > 0)
		th->disablecount --;
	else th->disablecount = 0;
	if (th->disablecount == 0  &&  th->firepending)
		observable_PullTrigger(self, trigger);
}

/* observable__DisableCount(self, trigger)
	return the number of outstanding DisableTrigger calls
*/
long observable__DisableCount(self, trigger)
struct observable *self;
struct atom *trigger;
{
    struct triggerhousing *th;
    struct triggerclass *tc;

    for (th = self->triggers; th != NULL && th->trigger != trigger; th = th->next) 
    {};
    if (th != NULL)
	return th->disablecount;

	/* find out if the trigger is defined */
    for (tc = Triggers;  tc != NULL && tc->class != class_GetType(self);
    tc = tc->next)
    {};
    if (tc == NULL || ! atomlist_Memberp(tc->triggers, trigger))
		/* not defined */
	return -1;
	/* is defined, but has no recipients.  Is not disabled */
    return 0;
}
