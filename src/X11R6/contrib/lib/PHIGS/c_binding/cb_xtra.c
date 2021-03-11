/* $XConsortium: cb_xtra.c,v 5.2 94/04/17 20:41:03 rws Exp $ */

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

/* Non-PHIGS functions for the PHIGS C binding.  These are library entry
 * points for use only in debugging/testing.  They are not intended for
 * customer use.
 */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

void
pxxinq_win_info( ws, error_ind, win_info)
    Pint		ws;
    Pint		*error_ind;
    Pxxwininfo		*win_info;
{
    Phg_args			cp_args;
    Phg_ret			ret;
    Psl_ws_info			*ws_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
        if ( PSL_WS_STATE( phg_cur_cph->psl) != PWSOP) {
	    *error_ind = ERR3;

	} else if ( !(ws_info = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	    *error_ind = ERR54;

	} else if ( !( ws_info->wstype->base_type == WST_BASE_TYPE_SUN_TOOL
	    || ws_info->wstype->base_type == WST_BASE_TYPE_SUN_CANVAS
	    || ws_info->wstype->base_type == WST_BASE_TYPE_PTV_TOOL)) {
	    *error_ind = ERR52;
	
	} else {
	    cp_args.data.idata = ws;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_XX_INQ_WIN_INFO, &cp_args, &ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*error_ind = 0;
		win_info->frame_win_number= ret.data.win_info.frame_win_number;
		win_info->cvs_win_number= ret.data.win_info.cvs_win_number;
		win_info->display_size= ret.data.win_info.display_size;
	    }
	}
    }
}

void
pxxpost_event( ws, place, time, event )
    Pint 		ws;	/* IN: workstation id */
    Pxxpostplace	place;	/* IN: window to post event to */
    struct timeval	*time;	/* IN: time to wait before posting */
    Event		*event;	/* IN: event to post */
{
    Phg_args			cp_args;
    Phg_args_xx_post_event	*args = &cp_args.data.xx_post_event;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_phigs_xx_post_event)) {
        if ( PSL_WS_STATE( phg_cur_cph->psl) != PWSOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR3);

	} else if ( !phg_psl_get_ws_info( phg_cur_cph->psl, ws)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR54);

	} else {
	    args->wsid = ws;
	    args->place = place;
	    args->time = *time;
	    args->event = *event;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_XX_POST_EVENT, &cp_args, NULL);
	}
    }
}

int
pxxget_server_time( tloc )
    int		*tloc;
{
    Phg_args	cp_args;
    Phg_ret	ret;

    CP_FUNC( phg_cur_cph, CP_FUNC_OP_XX_GET_TIME, &cp_args, &ret);
    if ( tloc )
	*tloc = (int)ret.data.idata;

    return (int)ret.data.idata;
}

void
pxxws_output_synch( wsid )
    Pint	wsid;
{
    Phg_args	cp_args;
    Phg_ret	ret;

    cp_args.data.idata = wsid;
    CP_FUNC( phg_cur_cph, CP_FUNC_OP_XX_OUTPUT_SYNCH, &cp_args, &ret);
}
