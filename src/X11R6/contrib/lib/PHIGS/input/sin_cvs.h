/* $XConsortium: sin_cvs.h,v 5.2 94/04/17 20:42:04 rws Exp $ */

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

#ifndef SIN_CVS_H_INCLUDED
#define SIN_CVS_H_INCLUDED

/* Types and declarations for the Sin event and window device table. */

/* Each device can have this many triggers associated with it. */
#define SIN_MAX_DEV_TRIGGERS	10

/* Maximum number of devices of any one class. */
#define SIN_NUM_DEV_NUMS	5
#define SIN_NUM_DEV_CLASSES	6

typedef enum {
    SIN_NULL_TRIGGER		= 0,
    SIN_KEY_PRESS		= 1,
    SIN_KEY_RELEASE		= 2,
    SIN_BUTTON_1_PRESS		= 3,
    SIN_BUTTON_2_PRESS		= 4,
    SIN_BUTTON_3_PRESS		= 5,
    SIN_BUTTON_4_PRESS		= 6,
    SIN_BUTTON_5_PRESS		= 7,
    SIN_BUTTON_1_RELEASE	= 8,
    SIN_BUTTON_2_RELEASE	= 9,
    SIN_BUTTON_3_RELEASE	= 10,
    SIN_BUTTON_4_RELEASE	= 11,
    SIN_BUTTON_5_RELEASE	= 12,
    SIN_PTR_MOVE		= 13,
    SIN_PTR_DRAG		= 14,
    SIN_WIN_ENTER		= 15,
    SIN_WIN_EXIT		= 16,

    SIN_MAX_TRIG_CODE		= 17
} Sin_trig_code;

#define SIN_SHIFT_DOWN	(1L<<0)
#define SIN_CTRL_DOWN	(1L<<1)
#define SIN_BUTTON_DOWN	(1L<<2)
#define SIN_KEY_DOWN	(1L<<3)

typedef struct {
    Sin_trig_code	trigger;
    Sin_window_pt	pt;
    int			keycode;
    unsigned int	flags;
    XEvent		*xevent;
} Sin_cvs_event;

typedef enum {
    SIN_ECHO_DYNAMIC,
    SIN_ECHO_ADD_POINT,
    SIN_ECHO_DELETE_POINT,
    SIN_ECHO_ALL_POINTS
} Sin_stroke_echo_op;

typedef struct {
    int			count;
    int			max_count;
    int			size;	/* Current byte size of "data" area below */
    int			pos;
    union {
	int		ivalue;
	Sin_window_pt	pt;
	Sin_window_pt	*pts;
    } 			data;
} Sin_measure;

typedef enum {
    SIN_EVT_NONE,
    SIN_EVT_STARTED,
    SIN_EVT_DONE,
    SIN_EVT_BREAK
} Sin_event_state;

typedef Sin_trig_code	Sin_trig_list[SIN_MAX_DEV_TRIGGERS+1];

typedef struct {
    int			enabled;
    Sin_event_state	evt_state;
    Sin_measure		*measure;
    Sin_input_device	*sin_dev;
    GC			gc;		/* X graphics context, for echoing */
    void		(*activate)();
    void		(*deactivate)();
    void		(*sample)();
    Sin_trig_list	triggers;
} Dev_data;

typedef struct Sin_trig_op {
    void		(*evt_func)();
    Dev_data		*dev_data;
    struct Sin_trig_op	*next;
} Sin_trig_op;

typedef struct {
    Sin_trig_code	code;
    Sin_trig_op		*ops;
} Sin_trig_data;

typedef struct Sin_window_table {
    struct _Sin_input_ws	*ws;
    Sin_trig_data		trigs[SIN_MAX_TRIG_CODE];
    /* TODO: Make dev_table non-fixed size and only as big as needed. */
    Dev_data		dev_table[SIN_NUM_DEV_CLASSES][SIN_NUM_DEV_NUMS];
} Sin_window_table;

#define SIN_DEVICE_CANVAS_TABLE( _dev ) \
    (_dev)->ws->window_table

#define DEVICE_DATA( _cvs_tbl, _class, _devno ) \
    (&(_cvs_tbl)->dev_table[SIN_CLASS_INDEX((_class))][(_devno) - 1])

#define EVENT_IS_BUTTON( _e ) \
    (  (int)(_e)->trigger >= (int)SIN_BUTTON_1_PRESS \
    && (int)(_e)->trigger <= (int)SIN_BUTTON_5_RELEASE )

#define BUTTON_IS_DOWN( _e ) \
    ((_e)->flags & SIN_BUTTON_DOWN)

#define EVENT_SHIFT_IS_DOWN( _e ) \
    ((_e)->flags & SIN_SHIFT_DOWN)

#define EVENT_CTRL_IS_DOWN( _e ) \
    ((_e)->flags & SIN_CTRL_DOWN)

#endif
