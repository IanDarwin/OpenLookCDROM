/* $XConsortium: ut_ntfy.c,v 5.8 94/04/17 20:42:22 hersh Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <stdio.h>
#include <signal.h>
#include "phg.h"

#define MAX_SIGNALS	     31
#define MAX_TIMERS	     10
#define MAX_RESOLUTION	    100	    /* of timers in microseconds */

typedef struct _notify_list {
    unsigned long	     client_id;
    void		   (*sig_handler)();
    struct _notify_list     *next;
} notify_list;

typedef struct _timer_list {
    unsigned long	    client_id;
    void		  (*timer_handler)();
    unsigned long	    usecs;
    unsigned long	    ticks;
    unsigned long	    ticks_needed;
    int			    in_use;
    struct _timer_list	   *next;
} timer_list;

static notify_list *notify_list_array[MAX_SIGNALS] = {
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL
};

static timer_list *timer_list_array[2] = {  /* for REAL and VIRTUAL */
    NULL, NULL
};


/** This assumes that the shortest time is in times[0], but the remainder
 ** may be in any order **/
static unsigned long
greatest_common_divisor(list)
timer_list *list;
{
    int count = 0;
    register unsigned long j;
    unsigned long gcd_candidate;
    unsigned long times[MAX_TIMERS];
    register timer_list *trav;
    
    for (trav = list; trav; trav = trav->next)
	times[count++] = (trav->usecs / MAX_RESOLUTION) * MAX_RESOLUTION;

    /** Calculate GCD by sucessively subtracting the resolution from
     ** the smallest value, and seeing if it evenly divides into everything
     **/
    for (gcd_candidate = times[0]; gcd_candidate > 1;
		gcd_candidate -= MAX_RESOLUTION) {
	for (j = 0; j < count; j++) {
	    if (times[j] % gcd_candidate)
		break;
	}
	if (j == count)
	    return gcd_candidate;   /* in microseconds */
    }
    
    return MAX_RESOLUTION;
}

/*ARGSUSED*/
static void							
timer_dispatcher(signal_num)
int		     signal_num;
{
    register	 timer_list	*trav;
	
    if (signal_num == SIGALRM)
	trav = timer_list_array[ITIMER_REAL];
    else
	trav = timer_list_array[ITIMER_VIRTUAL];

    while (trav) {
	if (++trav->ticks == trav->ticks_needed) {
	    trav->ticks = 0;
	    (*trav->timer_handler)(trav->client_id, signal_num);
	}
	trav = trav->next;
    }
#if defined(SYSV) || defined(SVR4)
    /* Have to reinstall the signal handler. */
    (void)signal(signal_num, timer_dispatcher);
#endif
}

/** Single entry point to the PEX client side timer dispatcher.  Four
 ** parameters are needed, a client_id which must be unique amongst
 ** all the registered clients for the specified signal, a timer
 ** handler function whose parameters are describes below, which
 ** timer is to be used (ITIMER_REAL or ITIMER_VIRTUAL), and the number 
 ** of microseconds at which the timer should go off.
 **
 **   The handler itself has the form
 **
 **	void handler(client_id, signal_num)
 **	unsigned long	    client_id;
 **	int		    signal_num;
 **	{
 **	}
 **
 ** Each registered timer handler gets called when the number of microseconds
 ** for it have expired.  A timer handler may be removed by calling this 
 ** function with the client_id and 'which' value as before, but with the
 ** timer_handler == NULL.
 **/

#define	MAX_TIMER_CLIENTS	25
static	timer_list	timer_pool[MAX_TIMER_CLIENTS];

void
phg_register_timer_func(client_id, timer_handler, which, usecs)
unsigned long	client_id;
void	      (*timer_handler)();
int		which;		    /* ITIMER_REAL or ITIMER_VIRTUAL */
unsigned long	usecs;		    /* when alarm should go off */
{
    int		    found;
    timer_list	   *curr = NULL;
    timer_list     *prev = NULL;
    struct itimerval timerval;
    
    if (timer_handler == NULL) {

	/** Remove the specified client's handler **/
	curr = timer_list_array[which];
	found = 0;
	while (curr && !found) {
	    if (curr->client_id == client_id)
		found = 1;
	    else {
		prev = curr;
		curr = curr->next;
	    }
	}
	
	if (!found) {
#ifdef DEBUG
	    (void)fprintf(stderr, 
"phg_register_timer_func(): handler doesn't exist for specified client\n");
#endif /* DEBUG */
	    return;
	}
	    
	if (prev)
	    prev->next = curr->next;
	else
	    timer_list_array[which] = curr->next;

	curr->in_use = 0;
	
    } else {
	
	timer_list *new;

	for ( new = timer_pool; new && new->in_use; new++ ) {
	    if ( new == timer_pool + MAX_TIMER_CLIENTS )
		new = (timer_list *)NULL;
	}
	if ( !new ) {
#ifdef DEBUG
	    (void)fprintf(stderr, 
"phg_register_timer_func(): ran out of timer_pool slots.\n");
#endif /* DEBUG */
	    return;
	}
	new->in_use = 1;
	new->client_id = client_id;
	new->timer_handler = timer_handler;
	new->usecs = usecs;
	new->ticks = new->ticks_needed = 0;

	/** Add the new one so as to preserve the first one
	 ** on the list having the smallest usecs value, which is necessary
	 ** for the GCD function.
	 **/
	curr = timer_list_array[which];
	if (!curr) {
	    timer_list_array[which] = new;
	    new->next = NULL;
	} else if (curr->usecs > usecs) {
	    timer_list_array[which] = new;
	    new->next = curr;
	} else {
	    new->next = curr->next;
	    curr->next = new;
	}
	
	/** (Re)install the signal handler if only one on list **/
	if (timer_list_array[which]->next == NULL)
#if !defined(AIXrt) && !(defined(SYSV) && defined(SYSV386))
	    if (which == ITIMER_REAL)
		(void)signal(SIGALRM, timer_dispatcher);
	    else
		(void)signal(SIGVTALRM, timer_dispatcher);
#else
		(void)signal(SIGALRM, timer_dispatcher);
#endif
		
    }
    
    
    if (timer_list_array[which] == NULL) {
	/** No timers left **/
	timerval.it_value.tv_sec = timerval.it_value.tv_usec = 0;
    } else {
	unsigned long	 gcd;
	timer_list	*trav;
	
	gcd = greatest_common_divisor(timer_list_array[which]);
	timerval.it_value.tv_sec = 
	    timerval.it_interval.tv_sec = gcd /	1000000;
	timerval.it_value.tv_usec = 
	    timerval.it_interval.tv_usec = gcd % 1000000;
	
	/** Now traverse the list and re-figure the ticks_needed field **/
	for (trav = timer_list_array[which]; trav; trav = trav->next) {
	    trav->ticks_needed = trav->usecs / gcd;
	    trav->ticks = 0;
	}
    }
    
    (void)setitimer(which, &timerval, (struct itimerval *)NULL);
    
}

static void							
sig_dispatcher(signal_num)
int		     signal_num;
{
    register	 notify_list	*curr, *tmp;
	
    curr = notify_list_array[signal_num - 1];	
    while (curr) {
	/* curr can get corrupted, so save the next value */
	tmp = curr->next;
	(*curr->sig_handler)(curr->client_id, signal_num, 0);
	curr = tmp;
    }
#if defined(SYSV) || defined(SVR4)
    /* Have to reinstall the signal handler. */
    (void)signal(signal_num, sig_dispatcher);
#endif
}

/** Single entry point to the PEX client side signal dispatcher.  Three
 ** parameters are needed, a client_id which must be unique amongst
 ** all the registered clients for the specified signal, a signal
 ** handler function whose parameters are describes below, and the
 ** signal number that the handler should be registered for.
 **
 **   The handler itself has the form
 **
 **	void handler(client_id, signal_num)
 **	unsigned long	    client_id;
 **	int		    signal_num;
 **	{
 **	}
 **
 ** Each registered signal handler gets called if the specified signal
 ** is detected.  Signal handlers are removed by calling this function
 ** with the client_id and signal_num as before, but with the
 ** sig_handler == NULL.
 **/
void
phg_register_signal_func(client_id, sig_handler, signal_num)
unsigned long	client_id;
void	      (*sig_handler)();
int		signal_num;
{
    int		   found = 0;
    notify_list	  *prev = NULL;
    notify_list   *curr = NULL;
    
    if (sig_handler == NULL) {

	/** Remove the specified client's handler **/
	curr = notify_list_array[signal_num - 1];
	found = 0;
	while (curr && !found) {
	    if (curr->client_id == client_id)
		found = 1;
	    else {
		prev = curr;
		curr = curr->next;
	    }
	}
	
	if (!found) {
#ifdef DEBUG
	    (void)fprintf(stderr, 
"phg_register_signal_func(): handler doesn't exist for specified client\n");
#endif /* DEBUG */
	    return;
	}
	    
	if (prev)
	    prev->next = curr->next;
	else
	    notify_list_array[signal_num - 1] = curr->next;

	free((char *)curr);
	
	if (!notify_list_array[signal_num - 1]) {
	    /** No procs left registered on this signal, so remove it **/
	    (void)signal(signal_num, SIG_DFL);
	}
	
    } else {
	
	/** Add the specified handler for the specified client **/
	
	curr = (notify_list *)malloc(sizeof(notify_list));
	curr->client_id = client_id;
	curr->sig_handler = sig_handler;

	/** Just add to the front of the list **/
	curr->next = notify_list_array[signal_num - 1];
	notify_list_array[signal_num - 1] = curr;
	
	if (notify_list_array[signal_num - 1]->next == NULL) {
	    /** If this is now the only proc on this signal, we
	     ** need to register our dispatcher, since it was
	     ** previously unregistered **/
	    (void)signal(signal_num, sig_dispatcher);
	}
	
    }
    
}
