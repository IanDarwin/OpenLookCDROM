/* $XConsortium: ws_inp.h,v 5.2 94/04/17 20:41:58 rws Exp $ */

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

#ifndef PHG_WS_INP_INCLUDED
#define PHG_WS_INP_INCLUDED

/*
 * Input related definitions.
 */

#include "sin.h"

#define WS_INP_DEV( _wsh, _class, _num ) \
    (&(_wsh)->in_ws.devs._class[(_num)-1])

typedef struct {
    Pint	num;		/* device number */
    Pop_mode	mode;		/* operating mode */
    Pecho_switch	esw;		/* echo switch */
    Ploc3	loc;		/* initial locator information */
    Pint	pet;		/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Ploc_data3	record;		/* data record */
} Ws_inp_loc;	/* modified version of Plocst3 */

typedef struct {
    Pint	num;		/* device number */
    Pop_mode	mode;		/* operating mode */
    Pecho_switch	esw;		/* echo switch */
    Pstroke3	stroke;		/* initial stroke */
    Pint	pet;		/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Pstroke_data3	record;		/* data record */
} Ws_inp_stroke;	/* modified version of Pstrokest3 */

typedef struct {
    Pint		num;		/* device number */
    Pop_mode		mode;		/* operating mode */
    Pecho_switch	esw;		/* echo switch */
    Ppick		pick;		/* initial pick path and status */
    Pint		pet;		/* prompt/echo type */
    Plimit3		e_volume;	/* echo volume */
    Ppick_data3		record;		/* data record */
    Ppath_order		order;		/* path order */
    Pfloat		ap_size;	/* aperture size */
    int			dev_type;	/* PEX pick device type */
    XID			measure;	/* PEX pick measure */
    struct {
	XID	incl;
	XID	excl;
    }			filter;		/* pick filter */
    Ppick_path		scratch_path;	/* scratch path */
} Ws_inp_pick;	/* modified version of Ppickst3 */

typedef struct {
    Pint	num;		/* device number */
    Pop_mode	mode;		/* operating mode */
    Pecho_switch	esw;		/* echo switch */
    Pfloat	val;		/* initial value */
    Pint	pet;		/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Pval_data3	record;		/* data record */
    char	*string_buf;	/* label strings */
} Ws_inp_val;	/* modified version of Pvalst3 */

typedef struct {
    Pint	num;		/* device number */
    Pop_mode	mode;		/* operating mode */
    Pecho_switch	esw;		/* echo switch */
    Pchoice	choice;		/* initial choice number and status */
    Pint	pet;		/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    int		strings_length;	/* length of compressed list of strings */
    Pchoice_data3	record;		/* data record */
} Ws_inp_choice;	/* modified version of Pchoicest3 */

typedef struct {
    Pint	num;		/* device number */
    Pop_mode	mode;		/* operating mode */
    Pecho_switch	esw;		/* echo switch */
    Pint	length;		/* length of initial string, including nul */
    char	*string;	/* initial string */
    Pint	pet;		/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Pstring_data3  record;		/* data record */
} Ws_inp_string;	/* modified version of Pstringst3 */

typedef union {
    Ws_inp_loc		*loc;
    Ws_inp_stroke	*stk;
    Ws_inp_val		*val;
    Ws_inp_choice	*cho;
    Ws_inp_string	*str;
    Ws_inp_pick		*pik;
} Ws_inp_device_handle;

typedef struct _Ws_input_ws {
    Pnum_in			num_devs;
    struct {					/* arrays of devices */
	Ws_inp_loc	*locator;
	Ws_inp_stroke	*stroke;
	Ws_inp_val	*valuator;
	Ws_inp_choice	*choice;
	Ws_inp_string	*string;
	Ws_inp_pick	*pick;
    }				devs;
    Sin_handle			sin_handle;
    Input_q_handle		input_queue;
    caddr_t			scratch;
    unsigned			scratch_size;
} Ws_input_ws;

extern int		phg_ws_inp_resolve_locator();
extern int		phg_ws_input_init();
extern void		phg_ws_input_close();
extern void		phg_ws_inp_set_mode();
extern void		phg_ws_inp_init_device();
extern void		phg_ws_inp_request();
extern void		phg_ws_inp_sample();
extern void		phg_ws_inp_inq_dev_state();
extern void		phg_ws_inp_resize();
extern void		phg_ws_inp_repaint();

extern void		phg_wsa_pm_close();

#endif
