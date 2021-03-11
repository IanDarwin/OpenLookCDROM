/* $XConsortium: sin_priv.h,v 5.2 94/04/17 20:42:05 rws Exp $ */

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

#ifndef SIN_INTERNAL_H_INCLUDED
#define SIN_INTERNAL_H_INCLUDED

#define SIN_EVENT_NOT_ENQUED_FLAG	0x1
#define SIN_EVENT_ENQUED_FLAG		0x2
#define SIN_EVENT_NOTIFIED_FLAG		0x4

#define SIN_EVENT_NOT_ENQUED( _s )	((_s) & SIN_EVENT_NOT_ENQUED_FLAG)
#define SIN_EVENT_ENQUED( _s )		((_s) & SIN_EVENT_ENQUED_FLAG)
#define SIN_EVENT_NOTIFIED( _s )	((_s) & SIN_EVENT_NOTIFIED_FLAG)

extern void             phg_sin_ws_enable_device();
extern void             phg_sin_ws_disable_device();
extern void             phg_sin_ws_reset_device();
extern void             phg_sin_ws_send_request();
extern int              phg_sin_ws_enque_events();
extern void		phg_sin_ws_load_event();
extern int		phg_sin_ws_event_buf_init();
extern void		phg_sin_ws_close_event_buf();
extern void		phg_sin_ws_buffer_event();
extern void		phg_sin_ws_flush_event_buffer();
extern int		phg_sin_ws_set_event_func();
extern void		phg_sin_ws_remove_event_func();
extern void		phg_sin_ws_free_notify_list();
extern void		phg_sin_ws_window_event_proc();

extern struct Sin_window_table	*phg_sin_cvs_init();
extern void		phg_sin_cvs_close();
extern int		phg_sin_cvs_create_device();
extern void             phg_sin_cvs_destroy_device();
extern void             phg_sin_cvs_device_enable();
extern void             phg_sin_cvs_device_disable();
extern void             phg_sin_cvs_device_sample();
extern int              phg_sin_cvs_device_initialize();
extern void             phg_sin_cvs_device_repaint();
extern void             phg_sin_cvs_device_resize();

extern void		phg_sin_dev_init_devices();
extern int		phg_sin_dev_create_devices();
extern void		phg_sin_dev_destroy_devices();
extern int		phg_sin_dev_start();

extern void		phg_sin_dev_boot_choice();
extern void		phg_sin_dev_boot_valuator();
extern void		phg_sin_dev_boot_string();

#define SIN_EVENT_IS_WANTED( _dev ) \
    ( (_dev)->sin_dev->mode == SIN_REQUEST_PENDING  || \
      (_dev)->sin_dev->mode == SIN_EVENT )

#define SIN_X_RECT_TO_SIN_RECT( _sw_r, _sin_r) \
    (_sin_r)->ll.x = (_sw_r)->x; \
    (_sin_r)->ll.y = (_sw_r)->y + (_sw_r)->height; \
    (_sin_r)->ur.x = (_sw_r)->x + (_sw_r)->width; \
    (_sin_r)->ur.y = (_sw_r)->y;

#define SIN_POINT_IN_RECT( _p, _r) \
    /* window coords, y increases top to bottom */ \
    (   (_p)->x >= (_r)->ll.x && (_p)->x <= (_r)->ur.x \
     && (_p)->y <= (_r)->ll.y && (_p)->y >= (_r)->ur.y)

#define SIN_POINT_IN_ECHO_AREA( _p, _dev) \
    (SIN_POINT_IN_RECT( (_p), &(_dev)->echo_area))

#define SIN_POINT_IN_WS( _p, _ws) \
    (1) /* not needed yet */

#define SIN_SET_ENABLE_DATA( _dev, _ed) \
    (_dev)->echo_area = (_ed)->echo_area;    \
    switch ((_dev)->class) { \
	case SIN_LOCATOR: \
	    (_dev)->data.locator.init_pos = (_ed)->data.locator.init_pos;\
	    break; \
	case SIN_STROKE: \
	    (_dev)->data.stroke.count = (_ed)->data.stroke.cnt; \
            bcopy( (char *)(_ed)->data.stroke.init_pts, \
		(char *)(_dev)->data.stroke.init_pts, \
		(_dev)->data.stroke.count * sizeof(Sin_window_pt)); \
    }

#define SIN_VALUATOR_SCALE( data) \
    (((data).valuator.high - (data).valuator.low) / (data).valuator.length)

#define SIN_BREAK_DEVICE( ws) \
    ((ws)->break_device)

#define SIN_ENABLE_BREAK( device) \
    SIN_BREAK_DEVICE((device)->ws) = device; \

#define SIN_DISABLE_BREAK( ws) \
    SIN_BREAK_DEVICE((ws)) = NULL; \

#endif
