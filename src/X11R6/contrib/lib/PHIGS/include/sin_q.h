/* $XConsortium: sin_q.h,v 5.2 94/04/17 20:41:56 rws Exp $ */

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

#ifndef SIN_Q_H_INCLUDED
#define SIN_Q_H_INCLUDED

#define SIN_Q_SIZE      500

#define SIN_Q_NO_OVERFLOW		0
#define SIN_Q_OVERFLOW_NOT_INQUIRED	1
#define SIN_Q_OVERFLOW_INQUIRED		2
#define SIN_Q_OVERFLOW_WS_FLUSHED	3

#define SIN_Q_FULL( queue) \
    ((queue)->count >= (queue)->size)

#define SIN_Q_EMPTY( queue) \
    ((queue)->count <= 0)

#define SIN_Q_ENOUGH_ROOM( queue, count) \
    ((queue)->count + (count) <= (queue)->size)

#define SIN_Q_NUM_FREE_EVENTS( queue) \
    ((queue)->size - (queue)->count)

#define SIN_Q_SET_OVERFLOW( _queue, _dev) \
    ((_queue)->overflow = SIN_Q_OVERFLOW_NOT_INQUIRED); \
    (_queue)->overflow_dev.ws = (_dev)->wsid; \
    (_queue)->overflow_dev.dev = (_dev)->num; \
    (_queue)->overflow_dev.class = SIN_TO_PHIGS_CLASS((_dev)->class)

#define SIN_Q_CLEAR_OVERFLOW( queue) \
    ((queue)->overflow = SIN_Q_NO_OVERFLOW)

#define SIN_Q_OVERFLOWED( queue) \
    ((queue)->overflow)

#define SIN_Q_PUSH_FREE_ELEMENT( queue, index) \
    (queue)->free_stack[++(queue)->stack_current] = (index)

#define SIN_Q_POP_FREE_ELEMENT( queue) \
    (queue)->free_stack[(queue)->stack_current--]

#define SIN_Q_HEAD_EVENT( _queue ) \
    (&(_queue)->events[(_queue)->events[(_queue)->last].next].event)

#define SIN_Q_NEW_SIMUL_ID( _queue ) \
    ((_queue)->next_simul_id++)

#define SIN_Q_SET_CUR_SIMUL_ID( _queue, _event )\
    ((_queue)->cur_simul_id = (_event)->simul_id)

#define SIN_Q_MORE_SIMUL_EVENTS( _queue ) \
    (!SIN_Q_EMPTY(_queue) && SIN_Q_HEAD_EVENT(_queue)->simul_id > 0 \
	&& (_queue)->cur_simul_id == SIN_Q_HEAD_EVENT(_queue)->simul_id)

typedef struct {
    Ploc3	evt;
} Sin_locator_event_data;

typedef struct {
    Ppick	evt;
} Sin_pick_event_data;

typedef struct {
    Pstroke3	evt;
} Sin_stroke_event_data;

typedef struct {
    Pfloat           value;
} Sin_valuator_event_data;

typedef struct {
    Pchoice	evt;
} Sin_choice_event_data;

typedef struct {
    Phg_string		evt;
} Sin_string_event_data;

typedef union {
    Sin_locator_event_data    locator;
    Sin_pick_event_data       pick;
    Sin_stroke_event_data     stroke;
    Sin_valuator_event_data   valuator;
    Sin_choice_event_data     choice;
    Sin_string_event_data     string;
} Sin_event_data;

typedef struct {
    int                 wsid;
    int			dev_num;
    Pin_class  		dev_class;
    unsigned		simul_id;
    Sin_event_data      data;
} Sin_input_event;

typedef struct {
    int                 previous;
    int			next;
    Sin_input_event     event;
} Sin_q_element;

typedef struct {
    int			count;		/* of events on queue */
    int			size;		/* max length of queue */
    int			last;		/* index of last queue element */
    int			stack_current;
    unsigned		next_simul_id;	/* use for next batch of events */
    unsigned		cur_simul_id;	/* id of last event read from queue */
    int			overflow;	/* !0 if overflow else 0 */
    Pevent		overflow_dev;
    void		(*event_notify_proc)();
    int			free_stack[SIN_Q_SIZE];
    Sin_q_element	events[SIN_Q_SIZE];

    Err_handle		erh;
} Sin_event_queue; /* TODO: Make a new parent structure. */

extern Sin_input_event	*phg_sin_q_enque_free_event();
extern Sin_input_event	*phg_sin_q_next_event();
extern void		phg_sin_q_deque_event();
extern void		phg_sin_q_flush_ws();
extern void		phg_sin_q_flush_device();
extern Input_q_handle	phg_sin_q_create();
extern int		phg_sin_q_overflow_event();
extern void		phg_sin_q_set_event_notify_proc();

#endif
