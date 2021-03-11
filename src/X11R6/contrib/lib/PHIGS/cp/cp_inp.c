/* $XConsortium: cp_inp.c,v 5.2 94/04/17 20:41:11 rws Exp $ */

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

/* CP functions related to input operations. */

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include "ws.h"

void
phg_cp_inp_init_dev( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Phg_args_inp_init_dev	*args = &cp_args->data.inp_init_dev;
    Ws_handle			wsh;

    if ( (wsh = CP_GET_WSH(cph, args->wsid)) && wsh->init_device )
	(*wsh->init_device)( wsh, args );
}

void
phg_cp_inp_set_mode( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Phg_args_inp_set_mode	*args = &cp_args->data.inp_set_mode;
    Ws_handle			wsh;

    if ( (wsh = CP_GET_WSH(cph, args->wsid)) && wsh->set_device_mode )
	(*wsh->set_device_mode)( wsh, &args->data );
}

void
phg_cp_inp_request( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Phg_args_inp_request	*args = &cp_args->data.inp_request;
    Ws_handle			wsh;

    if ( (wsh = CP_GET_WSH( cph, args->wsid)) && wsh->request_device )
	    (*wsh->request_device)( wsh, args->class, args->dev, ret );
    else {
	/* Send a Break - request not supported. */
	ret->err = 0;
	switch ( args->class ) {
	    case PHG_ARGS_INP_LOC3:
	    case PHG_ARGS_INP_LOC:
	    case PHG_ARGS_INP_STK3:
	    case PHG_ARGS_INP_STK:
	    case PHG_ARGS_INP_VAL:
	    case PHG_ARGS_INP_STR:
		ret->data.inp_request.status.istat = PIN_STATUS_NO_IN;
		break;
	    case PHG_ARGS_INP_PIK:
		ret->data.inp_request.status.pkstat = PIN_STATUS_NO_IN;
		break;
	    case PHG_ARGS_INP_CHC:
		ret->data.inp_request.status.chstat = PIN_STATUS_NO_IN;
		break;
	}
    }
}

void
phg_cp_inp_sample(cph, cp_args, ret)
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Phg_args_inp_sample		*args = &cp_args->data.inp_sample;
    Ws_handle			wsh;

    if ( (wsh = CP_GET_WSH( cph, args->wsid)) && wsh->sample_device )
	(*wsh->sample_device)( wsh, args->class, args->dev, ret );
}

void
phg_cp_inp_await( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* unused */
    Phg_ret		*ret;
{
    register Pevent			*evid = &ret->data.inp_event.id;
    register Phg_inp_event_data		*ed = &ret->data.inp_event.data;
    register Sin_input_event		*event;
    int					size;

    ret->err = 0;	/* No errors here. */
    if ( (event = phg_sin_q_next_event( (Sin_event_queue *)cph->input_q )) ) {
	if ( SIN_Q_OVERFLOWED((Sin_event_queue*)cph->input_q) ) {
	    ERR_BUF( cph->erh, ERR256);
	}
	evid->ws = event->wsid;
	evid->dev = event->dev_num;
	evid->class = event->dev_class;
	SIN_Q_SET_CUR_SIMUL_ID((Sin_event_queue*)cph->input_q, event);
	switch ( evid->class ) {
	    case PIN_LOC:
		ed->loc = event->data.locator.evt;
		break;
	    case PIN_STROKE:
		/* Copy stroke to scratch space and free the old one. */
		/* TODO: Maybe do this more efficiently, i.e., not do it. */
		size = event->data.stroke.evt.num_points * sizeof(Ppoint3);
		if ( size > 0 && !PHG_SCRATCH_SPACE(&cph->scratch, size)) {
		    ERR_BUF( cph->erh, ERR900);
		    ret->err = ERR900;
		    free((char *)ed->stk.points);
		} else {
		    ed->stk = event->data.stroke.evt;
		    if ( size > 0 ) {
			bcopy( (char *)ed->stk.points, cph->scratch.buf, size);
			free((char *)ed->stk.points);
			ed->stk.points = (Ppoint3*)cph->scratch.buf;
		    }
		}
		break;
	    case PIN_PICK: {
		/* Copy path to scratch space and free the old one. */
		/* TODO: Maybe do this more efficiently, i.e., not do it. */
		Ppick	*pick = &event->data.pick.evt;

		ed->pik = *pick;
		if ( pick->status == PIN_STATUS_OK ) {
		    size = pick->pick_path.depth * sizeof(Ppick_path_elem);
		    if ( size > 0 && !PHG_SCRATCH_SPACE(&cph->scratch, size) ){
			ERR_BUF( cph->erh, ERR900);
			ret->err = ERR900;
			free( (char *)pick->pick_path.path_list);

		    } else if ( size > 0 ) {
			bcopy( (char *)pick->pick_path.path_list,
			    cph->scratch.buf, size);
			free( (char *)pick->pick_path.path_list);
			ed->pik.pick_path.path_list =
			    (Ppick_path_elem*)cph->scratch.buf;
		    }
		}
		} break;
	    case PIN_VAL:
		ed->val = event->data.valuator.value;
		break;
	    case PIN_CHOICE:
		ed->chc = event->data.choice.evt;
		break;
	    case PIN_STRING: {
		/* Copy string to scratch space and free the old one. */
		/* TODO: Maybe do this more efficiently, i.e., not do it. */
		ed->str = event->data.string.evt;
		if ( event->data.string.evt.length > 0 ) {
		    if ( !PHG_SCRATCH_SPACE(&cph->scratch,
			    event->data.string.evt.length) ) {
			ERR_BUF( cph->erh, ERR900);
			ret->err = ERR900;
			free( event->data.string.evt.string);
			ed->str.length = 0;
		    } else {
			strcpy( cph->scratch.buf,
			    event->data.string.evt.string);
			free( event->data.string.evt.string);
			ed->str.string = cph->scratch.buf;
		    }
		}
		} break;
	}
        phg_sin_q_deque_event( (Sin_event_queue *)cph->input_q );

    } else {
	evid->class = PIN_NONE;
	if ( SIN_Q_OVERFLOWED((Sin_event_queue*)cph->input_q) ) {
	    SIN_Q_CLEAR_OVERFLOW((Sin_event_queue*)cph->input_q);
	}
    }
}

void
phg_cp_inp_flush_dev( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Phg_args_inp_flush	*args = &cp_args->data.inp_flush;

    if ( SIN_Q_OVERFLOWED((Sin_event_queue*)cph->input_q) ) {
	ERR_BUF( cph->erh, ERR256);
    }
    phg_sin_q_flush_device( (Sin_event_queue *)cph->input_q, args->wsid,
	args->class, args->dev );
}

void
phg_cp_send_request( cph, ret )
    Cp_handle		cph;
    Phg_ret		*ret;
{
    Phg_args		args;

    args.op = CP_FUNC_OP_INP_REQUEST | CP_OP_RETURN_DATA | CP_OP_POST_DATA;
    phg_cpr_send_ret( cph, &args, ret );
}

void
phg_cp_inq_inp_dev_state( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Phg_args_q_inp_state	*args = &cp_args->data.q_inp_state;
    Ws_handle			wsh;

    if ( (wsh = CP_GET_WSH( cph, args->wsid)) && wsh->inq_inp_dev_state)
	(*wsh->inq_inp_dev_state)( wsh, args->class, args->dev, ret );
}

void
phg_cp_inq_inp_overflow( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* unused */
    Phg_ret		*ret;
{
    int		status;

    if ( (status = phg_sin_q_overflow_event( (Sin_event_queue *)cph->input_q,
	    &ret->data.oflow_event )) )
	ret->err = status == SIN_Q_OVERFLOW_WS_FLUSHED ? ERR258 : ERR257;
    else
	ret->err = 0;
}

void
phg_cp_inq_more_events( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* unused */
    Phg_ret		*ret;
{
    ret->err = 0;
    if ( SIN_Q_MORE_SIMUL_EVENTS((Sin_event_queue*)cph->input_q) )
	ret->data.idata = (Pint)PSIMULT_MORE;
    else
	ret->data.idata = (Pint)PSIMULT_NO_MORE;
}

int
phg_cp_any_inp_device_active( cph )
    register Cp_handle		cph;
{
    /* Returns 1 if any device on any workstation is currently active. */

    register Ws		*ws;

    CPX_FOR_ALL_WS(cph, ws) {
	if ( WS_ANY_INP_DEV_ACTIVE(ws) )
	    return 1;
    }
    return 0;
}
