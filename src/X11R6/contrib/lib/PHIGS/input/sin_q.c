/* $XConsortium: sin_q.c,v 5.2 94/04/17 20:42:06 rws Exp $ */

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

/*
 *  Event queue utilities.
 */

#include "phg.h"
#include "sin.h"
#include "sin_priv.h"
#include "alloc.h"

Sin_input_event *
phg_sin_q_enque_free_event( queue)
    Sin_event_queue     *queue;

    /* Place a free and empty event on the queue and return a pointer to
     * it or NULL if the queue is full.
     */
{
    int				current;
    register Sin_q_element	*element;
    Sin_input_event		*event;

    if ( SIN_Q_FULL( queue) ) {
        event = NULL;
    } else {
        ++queue->count;
        current = SIN_Q_POP_FREE_ELEMENT( queue);
        element = &queue->events[current];
        event = &element->event;
        element->previous = queue->last;
        element->next = queue->events[queue->last].next;
        queue->events[queue->events[queue->last].next].previous = current;
        queue->events[queue->last].next = current;
        queue->last = current;
    }
    return event;
}

Sin_input_event *
phg_sin_q_next_event( queue)
    Sin_event_queue     *queue;

    /* Return a pointer to the first event in the queue.  The event is
     * not dequed.  Use sin_q_deque_event() for that.
     */
{
    Sin_input_event     *event;

    if ( SIN_Q_EMPTY( queue) ) {
        event = NULL;
    } else {
        event = &queue->events[queue->events[queue->last].next].event;
    }
    return event;
}

void
phg_sin_q_deque_event( queue )
    Sin_event_queue     *queue;

    /* Remove the first event from the queue.  Does not return the event.
     * Use sin_q_next_event() prior to calling this function to read the
     * event at the head of the queue.
     */
{
    int     current;

    if ( !SIN_Q_EMPTY( queue) ) {
        --queue->count;
        current = queue->events[queue->last].next;
        queue->events[queue->last].next = queue->events[current].next;
        queue->events[queue->events[current].next].previous = queue->last;
        SIN_Q_PUSH_FREE_ELEMENT( queue, current);
    }
}

static void
delete_event( queue, event_q_id)
    Sin_event_queue     *queue;
    int             event_q_id;

    /* Remove an event from a queue.
     */
{
    int     next, previous;

    if ( !SIN_Q_EMPTY( queue)) {
        --queue->count;
        next = queue->events[event_q_id].next;
        previous = queue->events[event_q_id].previous;
        queue->events[previous].next = next;
        queue->events[next].previous = previous;
        SIN_Q_PUSH_FREE_ELEMENT( queue, event_q_id);
        if ( event_q_id == queue->last) {
            queue->last = previous;
        }
    }
}

void
phg_sin_q_flush_ws( queue, wsid)
    Sin_event_queue	*queue;
    int			wsid;
    
    /* Flush all events on queue from a workstation.
     */
{
    register int    current,
		    next,
		    i,
		    count;

    count = queue->count;
    current = queue->events[queue->last].next;
    for ( i = 0; i < count; i++) {
        next = queue->events[current].next;
        if ( queue->events[current].event.wsid == wsid) {
            delete_event( queue, current);
        }
        current = next;
    }

    if ( queue->overflow == SIN_Q_OVERFLOW_NOT_INQUIRED &&
	queue->overflow_dev.ws == wsid )
	queue->overflow = SIN_Q_OVERFLOW_WS_FLUSHED;

    /*
    if ( SIN_Q_EMPTY( queue))
	SIN_Q_CLEAR_OVERFLOW( queue);
    */
}

void
phg_sin_q_flush_device( queue, wsid, class, num)
    Sin_event_queue     *queue;
    Pint		wsid;
    Pin_class		class;
    Pint		num;

    /* Flush all events on queue from a device.
     */
{
    register int    current_index,
		    next_index,
		    i,
		    count;

    Sin_q_element   *current;

    count = queue->count;
    current_index = queue->events[queue->last].next;
    current = &queue->events[current_index];
    for ( i = 0; i < count; i++) {
        next_index = current->next;
        if ( current->event.wsid == wsid
             && current->event.dev_class == class
             && current->event.dev_num == num) {
            delete_event( queue, current_index);
        }
        current_index = next_index;
        current = &queue->events[current_index];
    }
    /*
    if ( SIN_Q_EMPTY( queue))
	SIN_Q_CLEAR_OVERFLOW( queue);
    */
}

static void
initialize_queue( queue, size, erh )
    register Sin_event_queue     *queue;
    int				size;
    Err_handle			erh;
{
    register int	i;

    queue->erh = erh;
    queue->size = size;
    queue->count = 0;
    queue->last = 0;
    queue->cur_simul_id = 0;
    queue->next_simul_id = 1;
    queue->overflow = SIN_Q_NO_OVERFLOW;
    queue->event_notify_proc = NULL;
    queue->events[queue->last].previous = queue->last;
    queue->events[queue->last].next = queue->last;

    queue->stack_current = -1;
    i = queue->size;
    while ( --i >= 0) {
        SIN_Q_PUSH_FREE_ELEMENT( queue, i);
    }
}

Input_q_handle
phg_sin_q_create( erh )
    Err_handle		erh;
{
    Sin_event_queue	*queue;

    if ( !(queue = (Sin_event_queue*)calloc( 1, sizeof(Sin_event_queue))) ) {
	ERR_BUF( erh, ERR900);

    } else {
	initialize_queue( queue, SIN_Q_SIZE, erh);
    }

    return (Input_q_handle)queue;
}

void
phg_sin_q_destroy( queue )
    Sin_event_queue	*queue;
{
    free( (char *)queue );
}

int
phg_sin_q_overflow_event( queue, event)
    Sin_event_queue	*queue;
    Pevent		*event;
{
    int		status;

    /* Clears the overflow "error state" info, i.e., the device id. */
    if ( queue->overflow == SIN_Q_OVERFLOW_NOT_INQUIRED ) {
	*event = queue->overflow_dev;
	queue->overflow = SIN_Q_OVERFLOW_INQUIRED;
	status = 0;
    } else
        /* SIN_Q_NO_OVERFLOW == 0, which the calling function
         * (phg_cp_inq_inp_overflow) thinks of as no error, so tell
         * it SIN_Q_OVERFLOW_INQUIRED instead.
         * (both states map to the same phigs error, 257)
         */
        status = (queue->overflow == SIN_Q_NO_OVERFLOW) ? 
            SIN_Q_OVERFLOW_INQUIRED : queue->overflow;

    return status;

}

void
phg_sin_q_set_event_notify_proc( queue, proc )
    Sin_event_queue	*queue;
    void		(*proc)();
{
    /* This needs to be a function so that the pointer to it can be loaded
     * at run time and only when needed.  We might also eventually want to
     * do more when the function is set.
     */
    queue->event_notify_proc = proc;
}
