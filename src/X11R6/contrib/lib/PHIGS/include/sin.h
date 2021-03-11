/* $XConsortium: sin.h,v 5.2 94/04/17 20:41:55 rws Exp $ */

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

#ifndef SIN_H_INCLUDED
#define SIN_H_INCLUDED

#define SIN_TRUE	!0
#define SIN_FALSE	 0

typedef enum {
    SIN_REQUEST,
    SIN_REQUEST_PENDING,
    SIN_EVENT,
    SIN_SAMPLE
} Sin_input_mode;

typedef enum {
    SIN_LOCATOR  = 0,
    SIN_PICK     = 1,
    SIN_STROKE   = 2,
    SIN_VALUATOR = 3,
    SIN_CHOICE   = 4,
    SIN_STRING   = 5
} Sin_input_class;

typedef XPoint Sin_window_pt;

typedef struct {
    Sin_window_pt   ll,
                    ur;
} Sin_window_rect;

typedef struct {
    Wst_loc_type	type;
    Sin_window_pt	cur_pos;
    Sin_window_pt       init_pos;
    Ppoint3		wc_pt;
    int			view;
    int			(*resolve)();
    Pline_bundle	ln_bundl;
} Sin_locator_device_data;

typedef struct {
    Wst_pick_type	type;
    Sin_window_pt       init_pos;
    Sin_window_pt       cur_pos;
    Ppick		init_pick;
    Ppick		cur_pick;
    caddr_t		client_data;
    int			(*resolve)();
} Sin_pick_device_data;

typedef struct {
    Wst_stroke_type	type;
    int			count;
    int			init_count;
    int			edit_pos;
    int			buf_size;
    Sin_window_pt	*init_pts;
    Ppoint3		*wc_pts;
    int			view;
    int			(*resolve)();
    Pline_bundle	ln_bundl;
    Pmarker_bundle	mk_bundl;
} Sin_stroke_device_data;

typedef struct {
    Wst_val_type	type;
    float		value;
    float		init_value;
    float		low, high;
    char		*label;
    char		*format;
    char		*low_label;
    char		*high_label;
} Sin_valuator_device_data;

typedef struct {
    Wst_choice_type	type;
    int			init_choice;
    int			cur_choice;	/* 0 ==> no choice */
    int			count;		/* of choices */
    union {
	char			**strings;
	Ppr_switch		*on_off;
    }			choices;
} Sin_choice_device_data;

typedef struct {
    Wst_string_type	type;
    int			buf_size;
    int			edit_pos;
    long		last_pos;
    char		*init_string;
    char		*string;
} Sin_string_device_data;

typedef union {
    Sin_locator_device_data    locator;
    Sin_pick_device_data       pick;
    Sin_stroke_device_data     stroke;
    Sin_valuator_device_data   valuator;
    Sin_choice_device_data     choice;
    Sin_string_device_data     string;
} Sin_device_data;

typedef struct {
    caddr_t		client_data;
    int                 pe_type;
    Sin_window_rect     echo_area;
    Sin_device_data     data;
} Sin_dev_init_data;

typedef struct {
    Sin_window_rect     echo_area;
    union {
	struct {
	    Sin_window_pt	init_pos;
	} locator;
	struct {
	    int			cnt;
	    Sin_window_pt	*init_pts;
	} stroke;
    }			data;
} Sin_enable_data;

typedef struct {
    Sin_input_class	class;
    int			dev_num;
    Sin_enable_data	*enable_data;
    Sin_input_mode	mode;
    int			echo;
} Sin_set_mode_data;

typedef struct {
    Widget      shell;
    Widget      pane;
    Widget      scrollbar;
    Widget      value;
    Widget      label;
    Widget      low;
    Widget      high;
} Sin_valuator_handle;
 
typedef struct {
    Widget      shell;
    Widget      viewport;
    Widget      list;
} Sin_choice_handle;
 
typedef struct {
    Widget      shell;
    Widget      pane;
    Widget      textw;
} Sin_string_handle;
 
typedef union {
    Widget              widget;
    Window      	window;
    Sin_valuator_handle valuator;
    Sin_choice_handle   choice;
    Sin_string_handle   string;
} Sin_item_handle;

typedef struct {
    int		(*create)();
    void	(*reset)();
    void	(*enable)();
    void	(*disable)();
    int		(*init)();
    void	(*sample)();
    void	(*resize)();
    void	(*repaint)();
    void	(*destroy)();
} Sin_device_ops;

typedef struct _Sin_input_ws    *Sin_handle;

typedef struct Sin_input_device {
    int                 wsid;
    Sin_handle          ws;
    int                 num;
    Sin_input_class     class;
    Sin_input_mode      mode;
    int                 pe_type;
    int                 echo_sw;
    Sin_window_rect     echo_area;
    Sin_item_handle	item_handle;
    Sin_device_data     data;
    Sin_device_ops      dev_ops;
    char		*client_data;
    struct {
	unsigned on: 1;
	unsigned buffered : 1;
	unsigned exists : 1;
	unsigned been_up_yet : 1;
    }				flags;
} Sin_input_device;

typedef struct {
    void	(*send_request)();
    int		(*in_viewport)();
} Sin_ws_ops;

typedef struct {
    unsigned		flags;
#define SIN_EVT_ACKNOWLEDGE	0x0001
    int			count;
    int			size;
    Sin_input_device	**devs;
} Sin_buf_data;

#define SIN_WS_RESET_EVENT_BUFFER( _ev ) \
    (_ev)->flags = 0, (_ev)->count = 0

#define SIN_WS_SET_ACKNOWLEDGE( _ws ) \
    (_ws)->event_buffer.flags |= SIN_EVT_ACKNOWLEDGE

typedef struct Sin_notify_data {
    Window                      window;
    caddr_t                     handle;
    void                        (*notify)();
    struct Sin_notify_data      *next;
} Sin_notify_data;

#include "sin_q.h"

typedef struct _Sin_input_ws {
    Err_handle		erh;
    int                 wsid;
    Ws_handle		wsh;
    Wst_input_wsdt	*idt;
    Sin_event_queue	*queue;
    Sin_input_device	*break_device;
    Display		*display;
    Window              input_window;
    Window              output_window;
    Widget		shell;
    Sin_ws_ops		ops;
    Sin_buf_data	event_buffer;	/* holds simultaneous events */
    Sin_notify_data	*notify_list;
    Pnum_in		num_devs;
    Sin_input_device	*devices[6];	/* pointers to arrays of devices */
    struct Sin_window_table	*window_table;
} Sin_input_ws;

typedef struct {
    Sin_event_queue	*queue;
    Display		*display;
    Window              output_window;
    Window              input_window;
    Widget		shell;
    Ws_handle		wsh;
    Wst_input_wsdt	*idt;
    void                (*send_request)();
    int			(*in_viewport)();
} Sin_desc;

/* The context id for Xt input devices. */
extern XContext		phg_sin_device_context_id;

extern Sin_handle	phg_sin_create();
extern void		phg_sin_close();
extern void		phg_sin_dev_stop();
extern void		phg_sin_init_device();
extern void		phg_sin_set_mode();
extern void		phg_sin_repaint();
extern void		phg_sin_resize_dev();
extern void		phg_sin_request();
extern void		phg_sin_sample();

/* Xt action procs for input. */
extern XtActionProc	phg_sin_xt_request_satisfied();
extern XtActionProc	phg_sin_xt_string_event();

#define SIN_CLASS_INDEX( class) \
    ((int)(class))

#define SIN_DEV( ws, class, num) \
    (&(ws)->devices[SIN_CLASS_INDEX(class)][(num)-1])

#define SIN_TO_PHIGS_CLASS( _c ) \
    ((_c) == SIN_LOCATOR ? PIN_LOC \
	: (_c) == SIN_STROKE ? PIN_STROKE \
	    : (_c) == SIN_PICK ? PIN_PICK \
		: (_c) == SIN_VALUATOR ? PIN_VAL \
		    : (_c) == SIN_CHOICE ? PIN_CHOICE : PIN_STRING)

#define SIN_PHIGS_TO_SIN_CLASS( _c ) \
    ((_c) == PIN_LOC ? SIN_LOCATOR \
	: (_c) == PIN_STROKE ? SIN_STROKE \
	    : (_c) == PIN_PICK ? SIN_PICK \
		: (_c) == PIN_VAL ? SIN_VALUATOR \
		    : (_c) == PIN_CHOICE ? SIN_CHOICE : SIN_STRING)

#define SIN_DEV_EXISTS( _dev ) \
    ((_dev) && (_dev)->flags.exists)

#define SIN_EA_WIDTH( _ea ) ((_ea)->ur.x - (_ea)->ll.x)
#define SIN_EA_HEIGHT( _ea ) ((_ea)->ll.y - (_ea)->ur.y)
#define SIN_EA_X( _ea ) ((_ea)->ll.x)
#define SIN_EA_Y( _ea ) ((_ea)->ur.y)

#endif
