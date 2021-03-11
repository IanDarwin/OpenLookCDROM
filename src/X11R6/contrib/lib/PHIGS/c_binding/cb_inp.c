/* $XConsortium: cb_inp.c,v 5.8 94/04/17 20:40:48 mor Exp $ */

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

/* Input functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"
#include "ws_type.h"

#define CB_WS_CATEGORY( _wsinfo) \
    (((Wst*)_wsinfo->wstype)->desc_tbl.phigs_dt.ws_category)

#define DEVICE_EXISTS( _idt, _class, _num) \
    ((_num) > 0 && (_num) <= (_idt)->num_devs._class)

#define LINE_BUNDLE_VALID( _lb, _dt ) \
    ( (_lb)->colour >= 0 \
     && \
      (_lb)->colour < (_dt)->out_dt.num_colour_indices \
     && \
      phg_cb_int_in_list( (_lb)->type, (_dt)->out_dt.num_linetypes, \
	  (_dt)->out_dt.linetypes) \
    )

/* Special values used for arguments 4 and 5 to input_ws_open */
#define NO_DT_NEEDED	((Wst_phigs_dt	**) NULL)
#define REPORT_ERROR	((Pint	*) NULL)

static Wst_input_wsdt *
input_ws_open( cph, ws, fnid, dtp, err_ind )
    Cp_handle		cph;
    Pint		ws;
    Pint		fnid;
    Wst_phigs_dt	**dtp;	/* non-null if the whole dt is also needed */
    Pint		*err_ind; /* if non-null store error, else report */
{
    Psl_ws_info		*wsinfo;
    Wst_input_wsdt	*idt = NULL;

    if (CB_ENTRY_CHECK( cph, ERR3, fnid)) {
        if ( PSL_WS_STATE( cph->psl) != PWS_ST_WSOP) {
	    if ( !err_ind ) {
		ERR_REPORT( cph->erh, ERR3);
	    } else
		*err_ind = ERR3;

	} else if ( !(wsinfo = phg_psl_get_ws_info( cph->psl, ws))) {
	    if ( !err_ind ) {
		ERR_REPORT( cph->erh, ERR54);
	    } else
		*err_ind = ERR54;
	
	} else if ( !(CB_WS_CATEGORY( wsinfo) == PCAT_OUTIN
	    || CB_WS_CATEGORY( wsinfo) == PCAT_IN) ) {
	    if ( !err_ind ) {
		ERR_REPORT( cph->erh, ERR61);
	    } else
		*err_ind = ERR61;

	} else {
	    idt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.in_dt;
	    if ( dtp )
		*dtp = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	}

    } else if ( err_ind )
	*err_ind = ERR3;

    return idt;
}

static Wst_input_wsdt *
outin_ws_open( cph, ws, fnid, dtp, err_ind )
    Cp_handle		cph;
    Pint		ws;
    Pint		fnid;
    Wst_phigs_dt	**dtp;	/* non-null if the whole dt is also needed */
    Pint		*err_ind; /* if non-null store error, else report */
{
    Psl_ws_info		*wsinfo;
    Wst_input_wsdt	*idt = NULL;

    if (CB_ENTRY_CHECK( cph, ERR3, fnid)) {
        if ( PSL_WS_STATE( cph->psl) != PWS_ST_WSOP) {
	    if ( !err_ind ) {
		ERR_REPORT( cph->erh, ERR3);
	    } else
		*err_ind = ERR3;

	} else if ( !(wsinfo = phg_psl_get_ws_info( cph->psl, ws))) {
	    if ( !err_ind ) {
		ERR_REPORT( cph->erh, ERR54);
	    } else
		*err_ind = ERR54;
	
	} else if ( CB_WS_CATEGORY( wsinfo) != PCAT_OUTIN ) {
	    if ( !err_ind ) {
		ERR_REPORT( cph->erh, ERR60);
	    } else
		*err_ind = ERR60;

	} else {
	    idt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.in_dt;
	    if ( dtp )
		*dtp = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	}

    } else if ( err_ind )
	*err_ind = ERR3;

    return idt;
}

static void
set_mode( ws, class, dev, mode, echo)
    Pint			ws;
    Phg_args_idev_class		class;
    Pint			dev;
    Pop_mode			mode;
    Pecho_switch			echo;
{
    Phg_args				cp_args;
    register Phg_args_inp_set_mode	*args = &cp_args.data.inp_set_mode;

    args->wsid = ws;
    args->data.class = class;
    args->data.dev = dev;
    args->data.mode = mode;
    args->data.echo = echo;
    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INP_SET_MODE, &cp_args, NULL);
}

void
pset_loc_mode(ws, dev, mode, echo)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* locator device number	*/
    Pop_mode	mode;	/* operating mode	*/
    Pecho_switch	echo;	/* echo switch	*/
{
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_set_loc_mode,
		NO_DT_NEEDED, REPORT_ERROR)) {
	if ( DEVICE_EXISTS(idt,loc,dev) ) {
	    set_mode( ws, PHG_ARGS_INP_LOC, dev, mode, echo);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	}
    }
}

void
pset_stroke_mode(ws, dev, mode, echo)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* stroke device number	*/
    Pop_mode	mode;	/* operating mode	*/
    Pecho_switch	echo;	/* echo switch	*/
{
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_set_stroke_mode, NO_DT_NEEDED,
		REPORT_ERROR) ) {
	if ( DEVICE_EXISTS(idt,stroke,dev) ) {
	    set_mode( ws, PHG_ARGS_INP_STK, dev, mode, echo);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	}
    }
}

void
pset_val_mode(ws, dev, mode, echo)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* valuator device number	*/
    Pop_mode	mode;	/* operating mode	*/
    Pecho_switch	echo;	/* echo switch	*/
{
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_set_val_mode, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( DEVICE_EXISTS(idt,val,dev) ) {
	    set_mode( ws, PHG_ARGS_INP_VAL, dev, mode, echo);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	}
    }
}

void
pset_choice_mode(ws, dev, mode, echo)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* choice device number	*/
    Pop_mode	mode;	/* operating mode	*/
    Pecho_switch	echo;	/* echo switch	*/
{
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_set_choice_mode, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( DEVICE_EXISTS(idt,choice,dev) ) {
	    set_mode( ws, PHG_ARGS_INP_CHC, dev, mode, echo);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	}
    }
}

void
pset_pick_mode(ws, dev, mode, echo)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* pick device number	*/
    Pop_mode	mode;	/* operating mode	*/
    Pecho_switch	echo;	/* echo switch	*/
{
    Wst_input_wsdt		*idt;

    if ( idt = outin_ws_open( phg_cur_cph, ws, Pfn_set_pick_mode, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( DEVICE_EXISTS(idt,pick,dev) ) {
	    set_mode( ws, PHG_ARGS_INP_PIK, dev, mode, echo);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	}
    }
}

void
pset_string_mode(ws, dev, mode, echo)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* string device number	*/
    Pop_mode	mode;	/* operating mode	*/
    Pecho_switch	echo;	/* echo switch	*/
{
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_set_string_mode, NO_DT_NEEDED,
   		 REPORT_ERROR)) {
	if ( DEVICE_EXISTS(idt,string,dev) ) {
	    set_mode( ws, PHG_ARGS_INP_STR, dev, mode, echo);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	}
    }
}

void
pawait_event( timeout, ws_id, class,in_num)
    Pfloat	timeout;	/* timeout (seconds)	*/
    Pint        *ws_id;         /* OUT workstation identifier */
    Pin_class   *class;         /* OUT device class           */
    Pint        *in_num;        /* OUT logical input device number */
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Phg_ret_inp_event	*revt = &ret.data.inp_event;
    int			size;
    Ppoint3		*pts;
    Ppick_path_elem	*path;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_await_event)) {
        if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR3);

	} else {
	    cp_args.data.fdata = timeout;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INP_AWAIT, &cp_args, &ret);
	    if ( !ret.err) {
		*ws_id = revt->id.ws;
		*class = revt->id.class;
		*in_num = revt->id.dev;
		/* Some devices need additional work done. */
		switch ( revt->id.class ) {
		    case PIN_STROKE:
			/* Get space for the current event report. */
			size = revt->data.stk.num_points * sizeof(Ppoint3);
			if ( size > 0 ) {
			    if ( !(pts = (Ppoint3*)malloc((unsigned)size)) ) {
				ERR_REPORT( phg_cur_cph->erh, ERR900);
				revt->data.stk.num_points = 0;
			    } else {
				bcopy( (char*)revt->data.stk.points, (char*)pts,
					size);
				revt->data.stk.points = pts;
			    }
			}
			break;
		    case PIN_PICK:
			if ( revt->data.pik.status == PIN_STATUS_OK ) {
			    size = revt->data.pik.pick_path.depth
				* sizeof(Ppick_path_elem);
			    if ( size > 0 ) {
				if ( !(path = (Ppick_path_elem*)
						malloc((unsigned)size)) ) {
				    ERR_REPORT( phg_cur_cph->erh, ERR900);
				    revt->data.pik.status = PIN_STATUS_NONE;
				} else {
				    bcopy(
				      (char*)revt->data.pik.pick_path.path_list,
				      (char*)path, size);
				    revt->data.pik.pick_path.path_list = path;
				}
			    }
			}
			break;
		    case PIN_STRING: {
			char	*str;
			if ( revt->data.str.length > 0 ) {
			    str = malloc((unsigned)revt->data.str.length);
			    strcpy(str, revt->data.str.string);
			    revt->data.str.string = str;
			}
			} break;
		}
		PSL_CLEAR_CUR_EVENT( phg_cur_cph->psl)
		PSL_SET_CUR_EVENT_ID( phg_cur_cph->psl, revt->id);
		if ( revt->id.class != PIN_NONE ) 
		    PSL_SET_CUR_EVENT_DATA( phg_cur_cph->psl, revt->data);

	    } else {
		/* Report errors immediately so user doesn't try to
		 * read garbage.
		 */
		ERR_FLUSH( phg_cur_cph->erh);
	    }
	}
    }
}

static int
pf_device_exists( idt, class, num )
    Wst_input_wsdt	*idt;
    Pin_class		class;
    Pint		num;
{
    int		status = 0;

    switch ( class ) {
	case PIN_LOC:
	    status = DEVICE_EXISTS(idt,loc,num) ? 1 : 0;
	    break;
	case PIN_PICK:
	    status = DEVICE_EXISTS(idt,pick,num) ? 1 : 0;
	    break;
	case PIN_STROKE:
	    status = DEVICE_EXISTS(idt,stroke,num) ? 1 : 0;
	    break;
	case PIN_CHOICE:
	    status = DEVICE_EXISTS(idt,choice,num) ? 1 : 0;
	    break;
	case PIN_VAL:
	    status = DEVICE_EXISTS(idt,val,num) ? 1 : 0;
	    break;
	case PIN_STRING:
	    status = DEVICE_EXISTS(idt,string,num) ? 1 : 0;
	    break;
    }
    return status;
}

void
pflush_events( ws, class, dev )
    Pint	ws;	/* workstation identifier	*/
    Pin_class	class;	/* device class	*/
    Pint	dev;	/* logical input device number	*/
{
    Phg_args		cp_args;
    Phg_args_inp_flush	*args = &cp_args.data.inp_flush;
    Wst_input_wsdt	*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_flush_events, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !pf_device_exists( idt, class, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    args->class = class;
	    args->wsid = ws;
	    args->dev = dev;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_FLUSH_DEV, &cp_args, NULL);
	}
    }
}

static int
gevt_state_and_class_ok( cph, class, fnid)
    Cp_handle	cph;
    Pin_class	class;
{
    int		status = 1;

    if ( !CB_ENTRY_CHECK( cph, ERR3, fnid)) {
	status = 0;

    } else if ( PSL_WS_STATE( cph->psl) != PWS_ST_WSOP) {
	ERR_REPORT( cph->erh, ERR3);
	status = 0;

    } else if ( PSL_CUR_EVENT_CLASS( cph->psl) != class) {
	ERR_REPORT( cph->erh, ERR259);
	status = 0;
    }

    return status;
}

void
pget_loc3( view_ind, loc_pos )
    Pint        *view_ind;      /* OUT view index       */
    Ppoint3     *loc_pos;       /* OUT locator position */
{
    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_LOC, Pfn_get_loc3)) {
	*view_ind = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).view_ind;
	loc_pos->x = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).position.x;
	loc_pos->y = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).position.y;
	loc_pos->z = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).position.z;
    }
}

void
pget_loc( view_ind, loc_pos )
    Pint        *view_ind;      /* OUT view index       */
    Ppoint      *loc_pos;       /* OUT locator position */
{
    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_LOC, Pfn_get_loc) ) {
	*view_ind = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).view_ind;
	loc_pos->x = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).position.x;
	loc_pos->y = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,loc).position.y;
    }
}

void
pget_stroke3( view_ind, stroke )
    Pint        *view_ind;      /* OUT view index       */
    Ppoint_list3 *stroke;       /* OUT stroke           */
{
    register Pstroke3	*stk;

    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_STROKE, Pfn_get_stroke3)) {
	stk = &PSL_CUR_EVENT_DATA( phg_cur_cph->psl, stk);
	*view_ind = stk->view_ind;
	stroke->num_points = stk->num_points;
	if ( stk->num_points > 0 )
	    bcopy( (char*)stk->points, (char*)stroke->points,
		stk->num_points * sizeof(Ppoint3));
    }
}

void
pget_stroke( view_ind, stroke )
    Pint        *view_ind;      /* OUT view index       */
    Ppoint_list *stroke;        /* OUT stroke           */
{
    register Pstroke3		*stk;
    register int		i;

    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_STROKE, Pfn_get_stroke)) {
	stk = &PSL_CUR_EVENT_DATA( phg_cur_cph->psl, stk);
	*view_ind = stk->view_ind;
	stroke->num_points = stk->num_points;
	for ( i = 0; i < stk->num_points; i ++ ) {
	    stroke->points[i].x = stk->points[i].x;
	    stroke->points[i].y = stk->points[i].y;
	}
    }
}

void
pget_val( valuator )
    Pfloat	*valuator;	/* OUT valuator value	*/
{
    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_VAL, Pfn_get_val)) {
	*valuator = PSL_CUR_EVENT_DATA( phg_cur_cph->psl, val);
    }
}

void
pget_choice( in_status, choice )
    Pin_status     *in_status;     /* OUT choice status */
    Pint        *choice;        /* OUT choice        */
{
    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_CHOICE, Pfn_get_choice)) {
	*in_status = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,chc).status;
	*choice = PSL_CUR_EVENT_DATA(phg_cur_cph->psl,chc).choice;
    }
}

void
pget_pick( depth, in_status, rpick)
    Pint		depth;		/* depth of pick path to return	*/
    Pin_status          *in_status;     /* OUT pick status */
    Ppick_path          *rpick;         /* OUT pick path   */
{
    register Ppick	*pick;
    int			depth_limit;

    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_PICK, Pfn_get_pick) ) {
	pick = &PSL_CUR_EVENT_DATA( phg_cur_cph->psl, pik);
	if ((*in_status = pick->status) == PIN_STATUS_OK ) {
	    /* Return the real path depth but only return the number of
	     * elements that the user wants.
	     */
	    rpick->depth = pick->pick_path.depth;
	    depth_limit = MIN( depth, pick->pick_path.depth);
	    if ( depth_limit > 0 )
		bcopy( (char*)pick->pick_path.path_list,
		    (char*)rpick->path_list,
		    depth_limit * sizeof(Ppick_path_elem));
	}
    }
}

void
pget_string( string )
    char	*string;	/* OUT pointer to string of bufsiz for the
				 * device + 1 for the terminator. */
{
    Phg_string	*str;

    if ( gevt_state_and_class_ok( phg_cur_cph, PIN_STRING, Pfn_get_string)) {
	str = &PSL_CUR_EVENT_DATA( phg_cur_cph->psl, str);
	if ( str->length > 0 )
	    strcpy( string, str->string);
	else
	    *string = '\0';
    }
}

static void
sample_device( ws, dev, class, ret)
    Pint			ws;
    Pint			dev;
    Phg_args_idev_class		class;
    Phg_ret			*ret;
{
    Phg_args				cp_args;
    register Phg_args_inp_sample	*args = &cp_args.data.inp_sample;

    args->wsid = ws;
    args->dev = dev;
    args->class = class;
    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INP_SAMPLE, &cp_args, ret);
    if ( ret->err ) {
	/* Report errors immediately so user doesn't try to read garbage.  */
	ERR_FLUSH( phg_cur_cph->erh);
    }
}

void
psample_loc3( ws, dev, view_ind, loc_pos)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pint        *view_ind;      /* OUT view index       */
    Ppoint3     *loc_pos;       /* OUT locator position */
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_loc3, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, loc, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_LOC3, &ret);
	    if ( !ret.err ) {
		*view_ind = ret.data.inp_event.data.loc.view_ind;
		*loc_pos = ret.data.inp_event.data.loc.position;
	    }
	}
    }
}

void
psample_loc( ws, dev, view_ind, loc_pos)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pint        *view_ind;      /* OUT view index       */
    Ppoint      *loc_pos;       /* OUT locator position */
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_loc, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, loc, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_LOC, &ret);
	    if ( !ret.err ) {
		*view_ind = ret.data.inp_event.data.loc.view_ind;
		loc_pos->x = ret.data.inp_event.data.loc.position.x;
		loc_pos->y = ret.data.inp_event.data.loc.position.y;
	    }
	}
    }
}

void
psample_stroke3( ws, dev, view_ind, stroke)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* stroke device number	*/
    Pint        *view_ind;      /* OUT view index       */
    Ppoint_list3 *stroke;       /* OUT locator position */
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_stroke3, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_STK3, &ret);
	    if ( !ret.err ) {
		Pstroke3	*stk = &ret.data.inp_event.data.stk;

		*view_ind = stk->view_ind;
		stroke->num_points = stk->num_points;
		if ( stk->num_points > 0 )
		    bcopy( (char*)stk->points, (char*)stroke->points,
			stk->num_points * sizeof(Ppoint3));
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		stroke->num_points = 0;
	}
    }
}

void
psample_stroke( ws, dev, view_ind, stroke)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* stroke device number	*/
    Pint        *view_ind;      /* OUT view index       */
    Ppoint_list *stroke;       /* OUT locator position */
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;
    register int		i;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_stroke, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_STK, &ret);
	    if ( !ret.err ) {
		register Pstroke3	*stk = &ret.data.inp_event.data.stk;

		*view_ind = stk->view_ind;
		stroke->num_points = stk->num_points;
		for ( i = 0; i < stk->num_points; i ++ ) {
		    stroke->points[i].x = stk->points[i].x;
		    stroke->points[i].y = stk->points[i].y;
		}
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		stroke->num_points = 0;
	}
    }
}

void
psample_val( ws, dev, value)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* valuator device number	*/
    Pfloat      *value;         /* OUT value            */
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_val, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, val, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_VAL, &ret);
	    if ( !ret.err )
		*value = ret.data.inp_event.data.val;
	}
    }
}

void
psample_choice( ws, dev, choice_in_status, choice)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* choice device number	*/
    Pin_status     *choice_in_status; /* OUT choice input status */
    Pint	*choice;	/* OUT choice	*/
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_choice, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, choice, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_CHC, &ret);
	    if ( !ret.err ) {
		*choice_in_status = ret.data.inp_event.data.chc.status
;		*choice = ret.data.inp_event.data.chc.choice;
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*choice_in_status = PIN_STATUS_NONE;
	}
    }
}

void
psample_pick( ws, dev, depth, pick_in_status, rpick)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* pick device number	*/
    Pint	depth;	/* max. depth of returned path	*/
    Pin_status     *pick_in_status; /* OUT pick input status */
    Ppick_path  *rpick;  /* OUT pick path        */
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;
    int				depth_limit;

    if ( idt = outin_ws_open( phg_cur_cph, ws, Pfn_sample_pick, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, pick, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_PIK, &ret);
	    if ( !ret.err ) {
		Ppick	*pick = &ret.data.inp_event.data.pik;

		if ((*pick_in_status = pick->status) == PIN_STATUS_OK ) {
		    /* Return the real path depth but only return the number of
		     * elements that the user wants.
		     */
		    rpick->depth = pick->pick_path.depth;
		    depth_limit = MIN( depth, pick->pick_path.depth);
		    if ( depth_limit > 0 )
			bcopy( (char*)pick->pick_path.path_list,
			    (char*)rpick->path_list,
			    depth_limit * sizeof(Ppick_path_elem));
		}
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*pick_in_status = PIN_STATUS_NONE;
	}
    }
}

void
psample_string( ws, dev, string )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    char	*string;	/* OUT string	*/
{
    Phg_ret			ret;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_sample_string, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, string, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    sample_device( ws, dev, PHG_ARGS_INP_STR, &ret);
	    if ( !ret.err ) {
		if ( ret.data.inp_event.data.str.length > 0 )
		    strcpy( string, ret.data.inp_event.data.str.string);
		else
		    *string = '\0';
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*string = '\0';
	}
    }
}

static void
request_device( ws, dev, class, ret)
    Pint			ws;
    Pint			dev;
    Phg_args_idev_class		class;
    Phg_ret			*ret;
{
    Phg_args				cp_args;
    register Phg_args_inp_request	*args = &cp_args.data.inp_request;

    args->wsid = ws;
    args->dev = dev;
    args->class = class;
    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INP_REQUEST, &cp_args, ret);
    if ( ret->err ) {
	/* Report errors immediately so user doesn't try to read garbage.  */
	ERR_FLUSH( phg_cur_cph->erh);
    }
}

void
preq_loc3( ws, dev, in_status, view_ind, loc_pos)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* locator device number	*/
    Pin_status     *in_status; /* OUT input status */
    Pint        *view_ind;  /* OUT view index   */
    Ppoint3      *loc_pos;   /* OUT locator position */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_loc3, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, loc, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_LOC3, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.istat) != PIN_STATUS_NO_IN ) {
		    *view_ind = req->event.data.loc.view_ind;
		    *loc_pos = req->event.data.loc.position;
		}
	    } else
		/* The Standard doesn't say to do this but it's the only
		 * way the caller can detect a bad locator value without
		 * synchronizing errors.
		 */
		*in_status = PIN_STATUS_NO_IN;
	}
    }
}

void
preq_loc( ws, dev, in_status, view_ind, loc_pos)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* locator device number	*/
    Pin_status     *in_status; /* OUT input status */
    Pint        *view_ind;  /* OUT view index   */
    Ppoint      *loc_pos;   /* OUT locator position */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_loc, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, loc, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_LOC, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.istat) != PIN_STATUS_NO_IN ) {
		    *view_ind = req->event.data.loc.view_ind;
		    loc_pos->x = req->event.data.loc.position.x;
		    loc_pos->y = req->event.data.loc.position.y;
		}
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
	}
    }
}

void
preq_stroke3( ws, dev, in_status, view_ind, stroke)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* stroke device number	*/
    Pin_status     *in_status;  /* OUT input status */
    Pint        *view_ind;   /* OUT view index   */
    Ppoint_list3 *stroke;     /* OUT stroke       */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_stroke3, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_STK3, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.istat) != PIN_STATUS_NO_IN ) {
		    *view_ind = req->event.data.stk.view_ind;
		    stroke->num_points = req->event.data.stk.num_points;
		    if ( stroke->num_points > 0 )
			bcopy( (char*)req->event.data.stk.points,
			    (char*)stroke->points,
			    stroke->num_points * sizeof(Ppoint3));
		}
	    } else {
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
		stroke->num_points = 0;
	    }
	}
    }
}

void
preq_stroke( ws, dev, in_status, view_ind, stroke)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* stroke device number	*/
    Pin_status     *in_status;  /* OUT input status */
    Pint        *view_ind;   /* OUT view index   */
    Ppoint_list *stroke;     /* OUT stroke       */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;
    register int			i;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_stroke, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_STK, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.istat) != PIN_STATUS_NO_IN ) {
		    *view_ind = req->event.data.stk.view_ind;
		    stroke->num_points = req->event.data.stk.num_points;
		    for ( i = 0; i < stroke->num_points; i ++ ) {
			stroke->points[i].x = req->event.data.stk.points[i].x;
			stroke->points[i].y = req->event.data.stk.points[i].y;
		    }
		}
	    } else {
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
		stroke->num_points = 0;
	    }
	}
    }
}

void
preq_val( ws, dev, in_status, value)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* valuator device number	*/
    Pin_status     *in_status;  /* OUT input status */
    Pfloat      *value;      /* OUT value        */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_val, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, val, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_VAL, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.istat) != PIN_STATUS_NO_IN )
		    *value = req->event.data.val;

	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
	}
    }
}

void
preq_choice( ws, dev, in_status, choice)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* choice device number	*/
    Pin_status     *in_status;  /* OUT input status */
    Pint        *choice;     /* OUT choice       */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_choice, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, choice, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_CHC, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.chstat) != PIN_STATUS_NO_IN ) {
		    *choice = req->event.data.chc.choice;
		}
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
	}
    }
}

void
preq_pick( ws, dev, depth, in_status, rpick)
    Pint	ws;	/* workstation identifier	*/
    Pint	dev;	/* pick device number	*/
    Pint	depth;	/* max. depth of returned path	*/
    Pin_status     *in_status;  /* OUT input status */
    Ppick_path  *rpick;       /* OUT pick         */
{
    Phg_ret				ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt			*idt;
    int					depth_limit;

    if ( idt = outin_ws_open( phg_cur_cph, ws, Pfn_req_pick, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, pick, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_PIK, &ret);
	    if ( !ret.err ) {
		Ppick	*pick = &req->event.data.pik;

		if ( (*in_status = req->status.pkstat) == PIN_STATUS_OK ) {
		    /* Return the real path depth but only return the number of
		     * elements that the user wants.
		     */
		    rpick->depth = pick->pick_path.depth;
		    depth_limit = MIN( depth, rpick->depth);
		    if ( depth_limit > 0 )
			bcopy( (char*)pick->pick_path.path_list,
			    (char*)rpick->path_list,
			    depth_limit * sizeof(Ppick_path_elem));
		}
	    } else
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
	}
    }
}

void
preq_string( ws, dev, in_status, string )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    Pin_status     *in_status;  /* OUT input status */
    char       *string;     /* OUT string       */
{
    Phg_ret			ret;
    register Phg_ret_inp_request	*req = &ret.data.inp_request;
    Wst_input_wsdt		*idt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_req_string, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, string, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else {
	    request_device( ws, dev, PHG_ARGS_INP_STR, &ret);
	    if ( !ret.err ) {
		if ( (*in_status = req->status.istat) != PIN_STATUS_NO_IN ) {
		    if ( req->event.data.str.length > 0 )
			strcpy( string, req->event.data.str.string);
		    else
			*string = '\0';
		}
	    } else {
		/* Assign a safe value in case errors are not synchronous. */
		*in_status = PIN_STATUS_NO_IN;
		*string = '\0';
	    }
	}
    }
}

static int
loc_data_rec_ok( cph, pet, rec, dt, ddt )
    Cp_handle			cph;
    Pint			pet;
    Ploc_data			*rec;
    Wst_phigs_dt		*dt;
    Wst_defloc			*ddt;
{
    int		status = 0;
    Pint	err = 0;

    switch ( pet ) {
	case 1:
	case 2:
	case 3:
	    /* No data */
	    return !0;

	default:
	    /* Shouldn't get here, pet should be verified before calling. */
	    ERR_REPORT( cph->erh, ERR260);
	    return 0;
    }

}

static void
init_loc( cph, func_id, ws, dev, init_view, init_pos, pet, ev, rec, cp_args )
    Cp_handle		cph;
    Pint		func_id;
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* valuator device number */
    Pint		init_view;
    Ppoint3		*init_pos;
    Pint		pet;		/* prompt and echo type	*/
    Plimit3		*ev;		/* echo volume pointer	*/
    Ploc_data3		*rec;		/* data record pointer	*/
    Phg_args		*cp_args;
{
    register Phg_args_inp_init_dev	*args = &cp_args->data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;

    if ( idt = input_ws_open( cph, ws, func_id, &dt, REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, loc, dev) ) {
	    ERR_REPORT( cph->erh, ERR250);

	} else if ( !phg_cb_echo_limits_valid( cph, func_id, ws, ev, dt) ) {
	    /* Error has been reported, just fall out of the if. */
	    ;

	} else if ( init_view < 0
	    || init_view >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR114);

	} else {
	    Wst_defloc		*ddt = &idt->locators[dev-1];

	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( cph->erh, ERR253);
		rec = &ddt->record;
		pet = 1;
	    }

	    if ( loc_data_rec_ok( cph, pet, rec, dt, ddt ) ) {
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		args->echo_volume = *ev;
		args->data.loc.init.view_ind = init_view;
		args->data.loc.init.position = *init_pos;
		args->data.loc.rec = *rec;
		args->class = func_id == Pfn_init_loc3 ?
		    PHG_ARGS_INP_LOC3 : PHG_ARGS_INP_LOC ;
		CP_FUNC( cph, CP_FUNC_OP_INP_INIT_DEV, cp_args, NULL);
	    }
	}
    }
}

void
pinit_loc3( ws, dev, init_view_ind, init_loc_pos, pet, echo_volume, record )
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* locator device number */
    Pint		init_view_ind;	/* initial view indicator */
    Ppoint3		*init_loc_pos;	/* initial locator position */
    Pint		pet;		/* prompt and echo type	*/
    register Plimit3	*echo_volume;	/* echo volume pointer	*/
    Ploc_data3		*record;	/* data record pointer	*/
{
    Phg_args		cp_args;

    init_loc( phg_cur_cph, Pfn_init_loc3, ws, dev, init_view_ind,
	init_loc_pos, pet, echo_volume, record, &cp_args );
}

void
pinit_loc( ws, dev, init_view_ind, init_loc_pos, pet, echo_area, record )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pint	init_view_ind;	/* initial view indicator */
    Ppoint	*init_loc_pos;	/* initial locator pointer	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit	*echo_area;	/* echo area pointer	*/
    Ploc_data	*record;	/* data record pointer	*/
{
    Phg_args		cp_args;
    Plimit3		echo_volume;
    Ppoint3		init_pos;

    /* Convert to 3D and pass to generic initialization function. */
    CB_ECHO_AREA_TO_VOLUME( echo_area, &echo_volume);
    init_pos.x = init_loc_pos->x;
    init_pos.y = init_loc_pos->y;
    /* TODO: Treat Ploc_data the same as Ploc_data3.  They're currently
     * the same and it's possible the binding may change.
     */
    init_loc( phg_cur_cph, Pfn_init_loc, ws, dev, init_view_ind,
	&init_pos, pet, &echo_volume, (Ploc_data3 *)record, &cp_args);
}

static int
stroke_data_record_ok( pet, rec, ddt )
    Pint		pet;
    Pstroke_data3	*rec;
    Wst_defstroke	*ddt;
{
    int		status = 1;

    if ( rec->buffer_size < 1 || rec->buffer_size > ddt->max_bufsize )
	status = 0;
    else if ( rec->init_pos < 1 || rec->init_pos > rec->buffer_size )
	status = 0;

    return status;
}

void
pinit_stroke3( ws, dev, init_view_ind, init_stroke, pet, echo_volume, rec )
    Pint	ws;		/* workstation identifier */
    Pint	dev;		/* stroke device number	  */
    Pint	init_view_ind;	/* initial view indicator */
    Ppoint_list3 *init_stroke;	/* initial stroke pointer */
    Pint	pet;		/* prompt and echo type	  */
    Plimit3	*echo_volume;	/* echo volume pointer	  */
    Pstroke_data3 *rec;		/* data record pointer	  */
{
    Phg_args				cp_args;
    register Phg_args_inp_init_dev	*args = &cp_args.data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;

    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_init_stroke3, &dt,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else if ( !phg_cb_echo_limits_valid( phg_cur_cph, Pfn_init_stroke3, ws,
	    echo_volume, dt) ) {
	    /* Error has been reported, just fall out of the if. */
	    ;

	} else if ( init_view_ind < 0
	    || init_view_ind >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR114);

	} else {
	    Wst_defstroke	*ddt = &idt->strokes[dev-1];

	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets ) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( phg_cur_cph->erh, ERR253);
		rec = &(ddt->record);
		pet = 1;
	    }

	    if ( !stroke_data_record_ok( pet, rec, ddt ) ) {
		ERR_REPORT( phg_cur_cph->erh, ERR260);

	    } else if ( init_stroke->num_points > rec->buffer_size ) {
		ERR_REPORT( phg_cur_cph->erh, ERR262);

	    } else {
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		args->echo_volume = *echo_volume;
		args->data.stk.init.view_ind = init_view_ind;
		args->data.stk.init.num_points = init_stroke->num_points;
		args->data.stk.init.points = init_stroke->points;
		args->data.stk.rec = *rec;
		args->class = PHG_ARGS_INP_STK3;
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_INP_INIT_DEV, &cp_args, NULL);
	    }
	}
    }
}

void
pinit_stroke(ws, dev, init_view_ind, init_stroke, pet, echo_area, record )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* stroke device number	*/
    Pint	init_view_ind;	/* initial view indicator */
    Ppoint_list	*init_stroke;	/* initial stroke pointer	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit	*echo_area;	/* echo area pointer	*/
    Pstroke_data *record;	/* data record pointer	*/
{
    Phg_args				cp_args;
    register Phg_args_inp_init_dev	*args = &cp_args.data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;
    Psl_ws_info				*wsinfo;

    args->data.stk.init.points = (Ppoint3*)NULL;
    if ( idt = input_ws_open( phg_cur_cph, ws, Pfn_init_stroke, &dt,
		REPORT_ERROR)) {
	wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws );
	phg_cb_update_DC_size( wsinfo );
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);

	} else if ( !CB_ECHO_AREA_VALID( echo_area) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR254);

	} else if ( !CB_ECHO_AREA_IN_RANGE( echo_area, dt->dev_coords) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR255);

	} else if ( init_view_ind < 0
	    || init_view_ind >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR114);

	} else if ( init_stroke->num_points > 0 &&
	    !(args->data.stk.init.points = (Ppoint3*)malloc((unsigned)
		init_stroke->num_points * sizeof(Ppoint3)))) {
	    ERR_REPORT( phg_cur_cph->erh, ERR900);

	} else {
	    Wst_defstroke	*ddt = &idt->strokes[dev-1];
	    int			i;

	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( phg_cur_cph->erh, ERR253);
		record = (Pstroke_data *)&(ddt->record);
		pet = 1;
	    }

	    if ( !stroke_data_record_ok( pet, (Pstroke_data3 *)record, ddt ) ) {
		ERR_REPORT( phg_cur_cph->erh, ERR260);

	    } else if ( init_stroke->num_points > record->buffer_size ) {
		ERR_REPORT( phg_cur_cph->erh, ERR262);

	    } else {
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		CB_ECHO_AREA_TO_VOLUME( echo_area, &args->echo_volume);
		args->data.stk.init.view_ind = init_view_ind;
		args->data.stk.init.num_points = init_stroke->num_points;
		for ( i = 0; i < init_stroke->num_points; i++ ) {
		    args->data.stk.init.points[i].x = init_stroke->points[i].x;
		    args->data.stk.init.points[i].y = init_stroke->points[i].y;
		}
		args->data.stk.rec.buffer_size = record->buffer_size;
		args->data.stk.rec.init_pos = record->init_pos;
		args->data.stk.rec.x_interval = record->x_interval;
		args->data.stk.rec.y_interval = record->y_interval;
		args->data.stk.rec.time_interval = record->time_interval;
		switch ( pet ) {
		    case 3:
			args->data.stk.rec.pets.pet_r3.marker_attrs =
			    record->pets.pet_r3.marker_attrs;
			break;
		    case 4:
			args->data.stk.rec.pets.pet_r4.line_attrs =
			    record->pets.pet_r4.line_attrs;
			break;
		}
		args->class = PHG_ARGS_INP_STK;
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_INP_INIT_DEV, &cp_args, NULL);
	    }
	}
	if ( args->data.stk.init.points ) 
	    free( (char*)args->data.stk.init.points );
    }
}

static int
val_data_rec_ok( cph, pet, rec, ddt, init )
    Cp_handle			cph;
    Pint			pet;
    Pval_data3			*rec;
    Wst_defval			*ddt;
    Pfloat			init;
{
    int			status = 1;

    /* This is a placeholder for vendor code, commented to make picky 
       compilers happy
    switch ( pet ) {
	default:
	    break;
    }
    */

    if ( !status ) {
	ERR_REPORT( cph->erh, ERR260);
	status = 0;

    } else if ( rec->low > rec->high ) {
	ERR_REPORT( cph->erh, ERR260);
	status = 0;

    } else if ( init < rec->low || init > rec->high ) {
	ERR_REPORT( cph->erh, ERR261);
	status = 0;
    }
    return status;
}

static void
init_val( cph, func_id, ws, dev, init, pet, ev, rec, cp_args )
    Cp_handle		cph;
    Pint		func_id;
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* valuator device number */
    Pfloat		init;		/* initial value	*/
    Pint		pet;		/* prompt and echo type	*/
    Plimit3		*ev;		/* echo volume pointer	*/
    Pval_data3		*rec;		/* data record pointer	*/
    Phg_args		*cp_args;
{
    register Phg_args_inp_init_dev	*args = &cp_args->data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;

    if ( idt = input_ws_open( cph, ws, func_id, &dt, REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, val, dev) ) {
	    ERR_REPORT( cph->erh, ERR250);

	} else if ( !phg_cb_echo_limits_valid( cph, func_id, ws, ev, dt) ) {
	    /* Error has been reported, just fall out of the if. */
	    ;

	} else {
	    Wst_defval		*ddt = &idt->valuators[dev-1];

	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( cph->erh, ERR253);
		rec = &ddt->record;
		pet = 1;
	    }

	    if ( val_data_rec_ok( cph, pet, rec, ddt, init ) ) {
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		args->echo_volume = *ev;
		args->data.val.init = init;
		args->data.val.rec = *rec;
		switch ( pet ) {
		    case -1:
			args->data.val.counts[0] = rec->pets.pet_u1.label ?
			    strlen( rec->pets.pet_u1.label ) + 1: 0;
			args->data.val.counts[1] = rec->pets.pet_u1.format ?
			    strlen( rec->pets.pet_u1.format ) + 1: 0;
			args->data.val.counts[2] = rec->pets.pet_u1.low_label ?
			    strlen( rec->pets.pet_u1.low_label ) + 1: 0;
			args->data.val.counts[3] = rec->pets.pet_u1.high_label ?
			    strlen( rec->pets.pet_u1.high_label ) + 1: 0;
			break;
		}
		args->class = func_id == Pfn_init_val3 ?
		    PHG_ARGS_INP_VAL3 : PHG_ARGS_INP_VAL ;
		CP_FUNC( cph, CP_FUNC_OP_INP_INIT_DEV, cp_args, NULL);
	    }
	}
    }
}

void
pinit_val3( ws, dev, init, pet, echo_volume, record )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* valuator device number	*/
    Pfloat	init;		/* initial value	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit3	*echo_volume;	/* echo volume pointer	*/
    Pval_data3	*record;	/* data record pointer	*/
{
    Phg_args				cp_args;

    init_val( phg_cur_cph, Pfn_init_val3, ws, dev, init, pet,
	echo_volume, record, &cp_args);
}

void
pinit_val( ws, dev, init, pet, echo_area, record )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* valuator device number	*/
    Pfloat	init;		/* initial value	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit	*echo_area;	/* echo area pointer	*/
    Pval_data	*record;	/* data record pointer	*/
{
    Phg_args		cp_args;
    Plimit3		echo_volume;

    CB_ECHO_AREA_TO_VOLUME( echo_area, &echo_volume);
    init_val( phg_cur_cph, Pfn_init_val, ws, dev, init, pet,
	&echo_volume, (Pval_data3 *)record, &cp_args);
}

static int
choice_data_rec_ok( cph, pet, rec, dt, ddt, istat, init, args )
    Cp_handle			cph;
    Pint			pet;
    Pchoice_data3		*rec;
    Wst_phigs_dt                *dt;
    Wst_defchoice		*ddt;
    Pin_status	        	istat;
    Pint			init;
    Phg_args_inp_init_dev	*args;
{
    register int	i, size, cnt;
    register char	**strs, *strlist, *str;
    int			list_count, status = 0;
    int			chk_flag = 0;

    switch ( pet ) {
	case 1:
	    /* No user-specified data. */
	    if ( istat == PIN_STATUS_OK && (init < 1 || init > ddt->choices) ) {
		ERR_REPORT( cph->erh, ERR261);
	    } else
		status = !0;
	    break;

	case 2:
	    /* Count and list of ON/OFF values. */
	    args->data.cho.rec = *rec;
	    list_count = rec->pets.pet_r2.num_prompts;
	    if ( istat == PIN_STATUS_OK && (init < 1 || init > ddt->choices) ) {
		ERR_REPORT( cph->erh, ERR261);

	    } else if ( list_count < 0 || list_count > ddt->choices ) {
		ERR_REPORT( cph->erh, ERR260);

	    } else {
		for ( i = 0; i < list_count; i++ ) {
		    if ( *((rec->pets.pet_r2.prompts)+i) != 0 ||
			*((rec->pets.pet_r2.prompts)+i) != 1) {
			chk_flag = 1;
			ERR_REPORT( cph->erh, ERR260);
			break;
		    }
		}

		if ( chk_flag == 0 )
		    status = !0;		
	    }
	    break;

	case 3:
	    /* Count and list of choice strings. */
	    /* Package the choice strings into the internal format: a
	     * single long array of null terminated strings and a count of the
	     * total length of this array.
	     */
	    args->data.cho.rec = *rec;
	    args->data.cho.string_list_size = 0;
	    list_count = rec->pets.pet_r3.num_strings;
	    if ( list_count > ddt->choices || list_count < 1 ) {
		/* too many or too few choice strings */
		ERR_REPORT( cph->erh, ERR260);

	    /* init <= list_count ==> init <= ddt->choices, by above test. */
	    } else if (istat == PIN_STATUS_OK && (init > list_count || init < 1) ) {
		ERR_REPORT( cph->erh, ERR261);

	    } else {
		strs = rec->pets.pet_r3.strings;
		/* Compute total size of all strings and get space. */
		for ( size = 0, cnt = list_count; cnt > 0; cnt--, strs++ )
		    size += strlen(*strs) + 1; /* 1 for the terminator */
		if ( !(strlist = (char*)malloc((unsigned) size)) ) {
		    ERR_REPORT( cph->erh, ERR900);

		} else {
		    /* Copy all strings into the single array. */
		    strs = rec->pets.pet_r3.strings;
		    args->data.cho.rec.pets.pet_r3.strings
			= (char**)strlist;
		    args->data.cho.string_list_size = size;
		    cnt = list_count;
		    for ( i = 0; i < cnt; i++, strs++ ) {
			str = *strs;
			do
			    *strlist++ = *str;
			while ( *str++ );
		    }
		    status = !0;
		}
	    }
	    break;

	case 5:
	    /* Structure id, + count and list of pickids. */
	    args->data.cho.rec = *rec;
	    list_count = rec->pets.pet_r5.num_pick_ids;
	    if ( istat == PIN_STATUS_OK && (init < 1 || init > ddt->choices) ) {
		ERR_REPORT( cph->erh, ERR261);

	    } else if ( list_count < 0 || list_count > ddt->choices ) {
		ERR_REPORT( cph->erh, ERR261);

	    } else
		status = !0;
	    break;
    }

    return status;
}

static void
init_choice( cph, func_id, ws, dev, istat, init, pet, ev, rec, cp_args )
    Cp_handle		cph;
    Pint		func_id;
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* choice device number	*/
    Pin_status     	istat;		/* initial choice status */
    Pint		init;		/* initial choice	*/
    Pint		pet;		/* prompt and echo type	*/
    Plimit3		*ev;		/* echo volume pointer	*/
    Pchoice_data3	*rec;		/* data record pointer	*/
    Phg_args		*cp_args;
{
    register Phg_args_inp_init_dev	*args = &cp_args->data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;

    if ( idt = input_ws_open( cph, ws, func_id, &dt, REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, choice, dev) ) {
	    ERR_REPORT( cph->erh, ERR250);

	} else if ( !phg_cb_echo_limits_valid( cph, func_id, ws, ev, dt) ) {
	    /* Error has been reported, just fall out of the if. */
	    ;

	} else {
	    Wst_defchoice	*ddt = &idt->choices[dev-1];

	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( cph->erh, ERR253);
		rec = &(ddt->record);
		pet = 1;
	    }

	    if (choice_data_rec_ok(cph, pet, rec, dt, ddt, istat, init, args)){
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		args->echo_volume = *ev;
		args->data.cho.status = istat;
		args->data.cho.init = init;
		args->class = func_id == Pfn_init_choice3 ?
		    PHG_ARGS_INP_CHC3 : PHG_ARGS_INP_CHC ;
		CP_FUNC( cph, CP_FUNC_OP_INP_INIT_DEV, cp_args, NULL);

		/* Free any data allocated when the data record was
		   reformatted (in choice_data_rec_ok()).
		 */
		switch ( pet ) {
		    case 3:
			if ( args->data.cho.string_list_size > 0 )
			    free((char*)
				args->data.cho.rec.pets.pet_r3.strings);
			break;
		}
	    }
	}
    }
}

void
pinit_choice3( ws, dev, istat, init, pet, echo_volume, record )
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* choice device number	*/
    Pin_status     	istat;		/* initial choice status */
    Pint		init;		/* initial choice	*/
    Pint		pet;		/* prompt and echo type	*/
    Plimit3		*echo_volume;	/* echo volume pointer	*/
    Pchoice_data3	*record;	/* data record pointer	*/
{
    Phg_args				cp_args;

    init_choice( phg_cur_cph, Pfn_init_choice3, ws, dev, istat, init,
	pet, echo_volume, record, &cp_args);
}

void 
pinit_choice( ws, dev, istat, init, pet, echo_area, record )
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* choice device number	*/
    Pin_status     	istat;		/* initial choice status */
    Pint		init;		/* initial choice	*/
    Pint		pet;		/* prompt and echo type	*/
    Plimit		*echo_area;	/* echo area pointer	*/
    Pchoice_data	*record;	/* data record pointer	*/
{
    Phg_args		cp_args;
    Plimit3		echo_volume;

    CB_ECHO_AREA_TO_VOLUME( echo_area, &echo_volume);
    init_choice( phg_cur_cph, Pfn_init_choice, ws, dev, istat, init,
	pet, &echo_volume, (Pchoice_data3 *)record, &cp_args);
}

static int
pick_data_rec_ok( cph, pet, rec, dt, ddt, func_id )
    Cp_handle			cph;
    Pint			pet;
    Ppick_data3			*rec;
    Wst_phigs_dt		*dt;
    Wst_defpick			*ddt;
    Pint			func_id;
{
    Pint	status = 1;
    /*
    Ppoint3	*aper;
    */

    /* This is a placeholder for vendor code, commented to make picky 
       compilers happy
    switch ( pet ) {
	default:
	    break;
    }
    */

    if ( !status ) {
	ERR_REPORT( cph->erh, ERR260);
    
    /*
    } else if ( aper->x < 0.0 || aper->y < 0.0 ) {
	ERR_REPORT( cph->erh, ERR260);
	status = 0;

    } else if ( func_id == Pfn_init_pick3 && aper->z < 0.0 ) {
	ERR_REPORT( cph->erh, ERR260);
	status = 0;
    */
    }
    return status;
}

static void
init_pick( cph, func_id, ws, dev, istat, init, pet, ev, rec, order, cp_args )
    Cp_handle		cph;
    Pint		func_id;
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* choice device number	*/
    Pin_status		istat;		/* initial pick status	*/
    Ppick_path		*init;		/* initial pick pointer	*/
    Pint		pet;		/* prompt and echo type	*/
    Plimit3		*ev;		/* echo volume pointer	*/
    Ppick_data3		*rec;		/* data record pointer	*/
    Ppath_order		order;		/* pick path order	*/
    Phg_args		*cp_args;
{
    register Phg_args_inp_init_dev	*args = &cp_args->data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;
    Wst_defpick				*ddt;

    if ( idt = outin_ws_open( cph, ws, func_id, &dt, REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, pick, dev) ) {
	    ERR_REPORT( cph->erh, ERR250);

	} else if ( !phg_cb_echo_limits_valid( cph, func_id, ws, ev, dt) ) {
	    /* Error has been reported, just fall out of the if. */
	    ;

	} else {
	    ddt = &idt->picks[dev-1];
	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( cph->erh, ERR253);
		rec = &(ddt->record);
		pet = 1;
	    }

	    if ( pick_data_rec_ok( cph, pet, rec, dt, ddt, func_id) ) {
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		args->echo_volume = *ev;
		args->class = func_id == Pfn_init_pick3 ?
		    PHG_ARGS_INP_PIK3 : PHG_ARGS_INP_PIK ;

		args->data.pik.init.status = istat;
		if ( istat == PIN_STATUS_OK )
		    args->data.pik.init.pick_path = *init;
		else
		    args->data.pik.init.pick_path.depth = 0;
		args->data.pik.rec = *rec;
		args->data.pik.porder = order;
		CP_FUNC( cph, CP_FUNC_OP_INP_INIT_DEV, cp_args, NULL);
	    }
	}
    }
}


void
pinit_pick3( ws, dev, istat, init, pet, echo_volume, record, order )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* pick device number	*/
    Pin_status	istat;		/* initial pick status	*/
    Ppick_path	*init;		/* initial pick pointer	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit3	*echo_volume;	/* echo volume pointer	*/
    Ppick_data3	*record;	/* data record pointer	*/
    Ppath_order	order;		/* pick path order	*/
{
    Phg_args				cp_args;

    init_pick( phg_cur_cph, Pfn_init_pick3, ws, dev, istat, init, pet,
	echo_volume, record, order, &cp_args);
}

void
pinit_pick( ws, dev, istat, init, pet, echo_area, record, order )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* pick device number	*/
    Pin_status	istat;		/* initial pick status	*/
    Ppick_path	*init;		/* initial pick pointer	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit	*echo_area;	/* echo area pointer	*/
    Ppick_data	*record;	/* data record pointer	*/
    Ppath_order	order;		/* pick path order	*/
{
    Phg_args		cp_args;
    Plimit3		echo_volume;

    CB_ECHO_AREA_TO_VOLUME( echo_area, &echo_volume);
    init_pick( phg_cur_cph, Pfn_init_pick, ws, dev, istat, init, pet,
	&echo_volume, (Ppick_data3 *)record, order, &cp_args);
}

static int
string_data_ok( cph, pet, rec, init_length, ddt )
    Cp_handle			cph;
    Pint			pet;
    Pstring_data3		*rec;
    int				init_length;
    Wst_defstring		*ddt;
{
    int		status = 1;

    /* This is a placeholder for vendor code, commented to make picky 
       compilers happy
    switch ( pet ) {
	default:
	    break;
    }
    */

    if ( !status ) {
	ERR_REPORT( cph->erh, ERR260);

    } else {
	if ( rec->buffer_size > ddt->max_bufsize || rec->buffer_size < 1 )
	    rec->buffer_size = ddt->max_bufsize;

	if ( rec->init_pos > rec->buffer_size ) {
	    ERR_REPORT( cph->erh, ERR260);
	    status = 0;

	/* buffer size does not include the string terminator. */
	} else if ( init_length > rec->buffer_size ) {
	    ERR_REPORT( cph->erh, ERR263);
	    status = 0;

	}
    }
    return status;
}

static void
init_string( cph, func_id, ws, dev, init, pet, ev, rec, cp_args )
    Cp_handle		cph;
    Pint		func_id;
    Pint		ws;		/* workstation identifier */
    Pint		dev;		/* choice device number	*/
    char		*init;		/* initial string */
    Pint		pet;		/* prompt and echo type	*/
    Plimit3		*ev;		/* echo volume pointer	*/
    Pstring_data3	*rec;		/* data record pointer	*/
    Phg_args		*cp_args;
{
    register Phg_args_inp_init_dev	*args = &cp_args->data.inp_init_dev;
    Wst_input_wsdt			*idt;
    Wst_phigs_dt			*dt;
    Wst_defstring			*ddt;
    int					init_length;

    if ( idt = input_ws_open( cph, ws, func_id, &dt, REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, string, dev) ) {
	    ERR_REPORT( cph->erh, ERR250);

	} else if ( !phg_cb_echo_limits_valid( cph, func_id, ws, ev, dt) ) {
	    /* Error has been reported, just fall out of the if. */
	    ;

	} else {
	    ddt = &idt->strings[dev-1];
	    if ( !phg_cb_int_in_list( pet, ddt->num_pets, ddt->pets) ) {
		/* Bad pet, use pet 1 with default data. */
		ERR_REPORT( cph->erh, ERR253);
		rec = &(ddt->record);
		pet = 1;
	    }

	    if ( init )
		init_length = strlen(init);
	    else
		init_length = 0;

	    if ( string_data_ok( cph, pet, rec, init_length, ddt ) ) {
		args->data.str.init.string = init;
		args->data.str.init.length = init ? init_length + 1 : 0;
		args->wsid = ws;
		args->dev = dev;
		args->pet = pet;
		args->echo_volume = *ev;
		args->class = func_id == Pfn_init_string3 ?
		    PHG_ARGS_INP_STR3 : PHG_ARGS_INP_STR ;
		args->data.str.rec = *rec;
		CP_FUNC( cph, CP_FUNC_OP_INP_INIT_DEV, cp_args, NULL);
	    }
	}
    }
}

void
pinit_string3( ws, dev, init, pet, echo_volume, record )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    char	*init;		/* initial string	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit3	*echo_volume;	/* echo volume pointer	*/
    Pstring_data3 *record;	/* data record pointer	*/
{
    Phg_args				cp_args;

    init_string( phg_cur_cph, Pfn_init_string3, ws, dev, init, pet,
	echo_volume, record, &cp_args);
}

void
pinit_string( ws, dev, init, pet, echo_area, record )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    char	*init;		/* initial string	*/
    Pint	pet;		/* prompt and echo type	*/
    Plimit	*echo_area;	/* echo area pointer	*/
    Pstring_data *record;	/* data record pointer	*/
{
    Phg_args		cp_args;
    Plimit3		echo_volume;

    CB_ECHO_AREA_TO_VOLUME( echo_area, &echo_volume);
    init_string( phg_cur_cph, Pfn_init_string, ws, dev, init, pet,
	&echo_volume, (Pstring_data3 *)record, &cp_args);
}

void
pset_pick_filter( ws, dev_num, filter)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev_num;	/* pick device number	*/
    Pfilter     *filter;        /* pick filter          */
{
    Phg_args				cp_args;
    Wst_input_wsdt			*idt;
    register Phg_args_set_filter	*args = &cp_args.data.set_filter;

    if ( idt = outin_ws_open( phg_cur_cph, ws, Pfn_set_pick_filter, NO_DT_NEEDED,
		REPORT_ERROR)) {
	if ( !DEVICE_EXISTS( idt, pick, dev_num) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR250);
	
	} else {
	    args->wsid = ws;
	    args->devid = dev_num;
	    args->type = PHG_ARGS_FLT_PICK;
	    args->inc_set = filter->incl_set;
	    args->exc_set = filter->excl_set;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_FILTER, &cp_args, NULL);
	}
    }
}

#define INQ_STATE( _cph, _cp_args, _ws, _dev, _class, _ret ) \
  { \
    (_cp_args).data.q_inp_state.wsid = (_ws); \
    (_cp_args).data.q_inp_state.dev = (_dev); \
    (_cp_args).data.q_inp_state.class = (_class); \
    CP_FUNC((_cph),CP_FUNC_OP_INQ_INP_DEV_STATE,&(_cp_args),(_ret)); \
  }

static void
inq_loc_state( ws, dev, type, err, ret)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pint	*err;		/* OUT error indicator	*/
    Phg_ret	*ret;
{
    Phg_args			cp_args;
    Wst_input_wsdt		*idt;

    if (idt = input_ws_open( phg_cur_cph, ws, Pfn_INQUIRY, NO_DT_NEEDED, err)) {
	if ( !DEVICE_EXISTS( idt, loc, dev) ) {
	    *err = ERR250;
	} else {
	    cp_args.data.q_inp_state.inq_type = type;
	    INQ_STATE( phg_cur_cph, cp_args, ws, dev, PHG_ARGS_INP_LOC3, ret)
	    if ( ret->err )
		*err = ret->err;
	    else
		*err = 0;
	}
    }
}

void
pinq_loc_st3( ws, dev, type, store, err, op_mode, echo_switch, init_view_ind,
	     init_loc_pos, prompt_echo, echo_vol, loc_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pstore	store; 		/* store handle */
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    Pint        *init_view_ind; /* OUT initial view indicator */
    Ppoint3     *init_loc_pos;  /* OUT initial locator position */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit3     *echo_vol;     /* OUT echo volume        */
    Ploc_data3  **loc_data;     /* OUT data record      */
{
    		Phg_ret		ret;
    register	Plocst3		*state = &ret.data.inp_state.loc;

    inq_loc_state( ws, dev, type, err, &ret );
    if ( !*err ) {
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_view_ind = state->loc.view_ind;
	*init_loc_pos = state->loc.position;
	*prompt_echo = state->pet;
	*echo_vol = state->e_volume;
	*loc_data = &((_Pstore *)store)->data.loc_data3.drec;
	switch ( *prompt_echo ) {
	    /* Only case the ones with data. */
	    case 4:
		(*loc_data)->pets.pet_r4 = state->record.pets.pet_r4;
		break;
	    case 5:
		(*loc_data)->pets.pet_r5 = state->record.pets.pet_r5;
		break;
	    default:
		break;
	}
    }
}

void
pinq_loc_st( ws, dev, type, store, err, op_mode, echo_switch, init_view_ind,
	    init_loc_pos, prompt_echo, echo_area, loc_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pstore	store; 		/* pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    Pint        *init_view_ind; /* OUT initial view indicator */
    Ppoint      *init_loc_pos;  /* OUT initial locator position */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit      *echo_area;     /* OUT echo area        */
    Ploc_data   **loc_data;       /* OUT data record      */
{
    		Phg_ret		ret;
    register	Plocst3		*state = &ret.data.inp_state.loc;

    inq_loc_state( ws, dev, type, err, &ret);
    if ( !*err ) {
	CB_ECHO_VOLUME_TO_AREA( state->e_volume, *echo_area);
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*prompt_echo = state->pet;
	*init_view_ind = state->loc.view_ind;
	init_loc_pos->x = state->loc.position.x;
	init_loc_pos->y = state->loc.position.y;
	*loc_data = &((_Pstore *)store)->data.loc_data.drec;
	switch ( *prompt_echo ) {
	    /* Only case the ones with data. */
	    case 4:
		(*loc_data)->pets.pet_r4.line_attrs = state->record.pets.pet_r4.line_attrs;
		break;
	    case 5:
		(*loc_data)->pets.pet_r5.line_fill_ctrl_flag = state->record.pets.pet_r5.line_fill_ctrl_flag;
		switch ( state->record.pets.pet_r5.line_fill_ctrl_flag ) {
		    case PFLAG_LINE:
			(*loc_data)->pets.pet_r5.attrs.line_attrs/* WARNING: may be ln */ =
			    state->record.pets.pet_r5.attrs.line_attrs/* WARNING: may be ln */;
			break;
		    case PFLAG_FILL:
			(*loc_data)->pets.pet_r5.attrs.int_attrs =
			    state->record.pets.pet_r5.attrs.int_attrs;
			break;
		    case PFLAG_FILL_SET:
			(*loc_data)->pets.pet_r5.attrs.fill_set.int_attrs =
			    state->record.pets.pet_r5.attrs.fill_set.int_attrs;
			(*loc_data)->pets.pet_r5.attrs.fill_set.edge_attrs =
			    state->record.pets.pet_r5.attrs.fill_set.edge_attrs;
			break;
		}
		break;
	    default:
		break;
	}
    }
}

static void
inq_stroke_state( ws, dev, type, err, ret )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pint	*err;		/* OUT error indicator	*/
    Phg_ret	*ret;
{
    Phg_args			cp_args;
    Wst_input_wsdt		*idt;

    if (idt = input_ws_open( phg_cur_cph, ws, Pfn_INQUIRY, NO_DT_NEEDED, err)) {
	if ( !DEVICE_EXISTS( idt, stroke, dev) ) {
	    *err = ERR250;
	} else {
	    cp_args.data.q_inp_state.inq_type = type;
	    INQ_STATE( phg_cur_cph, cp_args, ws, dev, PHG_ARGS_INP_STK3, ret)
	    if ( ret->err )
		*err = ret->err;
	    else
		*err = 0;
	}
    }
}

void
pinq_stroke_st3( ws, dev, type, store, err, op_mode, echo_switch, init_view_ind,
	     init_stroke, prompt_echo, echo_vol, stroke_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* stroke device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pstore	store; 		/* handle to Store object */
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch *echo_switch;  /* OUT echo switch      */
    Pint        *init_view_ind; /* OUT initial view indicator */
    Ppoint_list3 **init_stroke; /* OUT initial stroke   */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit3      *echo_vol;     /* OUT echo area        */
    Pstroke_data3 **stroke_data; /* OUT data record      */
{
		int		size;
    		Phg_ret		ret;
    register	Pstrokest3	*state = &ret.data.inp_state.stroke;

    inq_stroke_state( ws, dev, type, err, &ret);
    if ( !*err ) {
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_view_ind = state->stroke.view_ind;
	*prompt_echo = state->pet;
	*echo_vol = state->e_volume;
	*stroke_data = &((_Pstore *)store)->data.stroke_data3.drec;
	(*stroke_data)->buffer_size = state->record.buffer_size;
	(*stroke_data)->init_pos = state->record.init_pos;
	(*stroke_data)->x_interval = state->record.x_interval;
	(*stroke_data)->y_interval = state->record.y_interval;
	(*stroke_data)->z_interval = state->record.z_interval;
	(*stroke_data)->time_interval = state->record.time_interval;
	switch ( *prompt_echo ) {
	    /* Only case the ones with data. */
	    case 3:
		(*stroke_data)->pets.pet_r3 = state->record.pets.pet_r3;
		break;
	    case 4:
		(*stroke_data)->pets.pet_r4 = state->record.pets.pet_r4;
		break;
	    default:
		break;
	}
	size = state->stroke.num_points * sizeof(Ppoint3);
	*init_stroke = &((_Pstore *)store)->data.stroke_data3.init_stroke;
	(*init_stroke)->num_points = state->stroke.num_points;
	if ( CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
	    /* Copy the initial stroke. */
	    if ( state->stroke.num_points > 0 ) {
		(*init_stroke)->points = (Ppoint3 *)((_Pstore *)store)->buf;
		bcopy( (char *)state->stroke.points,
		    (char *)(*init_stroke)->points, size );
	    }
	}
    }
}

void
pinq_stroke_st( ws, dev, type, store, err, op_mode, echo_switch, init_view_ind,
	     init_stroke, prompt_echo, echo_area, stroke_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* stroke device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pstore	store; 		/* handle to Store object */
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch *echo_switch;  /* OUT echo switch      */
    Pint        *init_view_ind; /* OUT initial view indicator */
    Ppoint_list **init_stroke;  /* OUT initial stroke   */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit      *echo_area;     /* OUT echo area        */
    Pstroke_data **stroke_data; /* OUT data record      */
{
		int		size;
    		Phg_ret		ret;
    register	Pstrokest3	*state = &ret.data.inp_state.stroke;
    register	int		i;

    inq_stroke_state( ws, dev, type, err, &ret);
    if ( !*err ) {
	CB_ECHO_VOLUME_TO_AREA( state->e_volume, *echo_area);
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_view_ind = state->stroke.view_ind;
	*prompt_echo = state->pet;
	*stroke_data = &((_Pstore *)store)->data.stroke_data.drec;
	(*stroke_data)->buffer_size = state->record.buffer_size;
	(*stroke_data)->init_pos = state->record.init_pos;
	(*stroke_data)->x_interval = state->record.x_interval;
	(*stroke_data)->y_interval = state->record.y_interval;
	(*stroke_data)->time_interval = state->record.time_interval;
	switch ( *prompt_echo ) {
	    /* Only case the ones with data. */
	    case 3:
		(*stroke_data)->pets.pet_r3.marker_attrs =
		    state->record.pets.pet_r3.marker_attrs;
		break;
	    case 4:
		(*stroke_data)->pets.pet_r4.line_attrs =
		    state->record.pets.pet_r4.line_attrs;
		break;
	    default:
		break;
	}
	size = state->stroke.num_points * sizeof(Ppoint);
	*init_stroke = &((_Pstore *)store)->data.stroke_data.init_stroke;
	(*init_stroke)->num_points = state->stroke.num_points;
	if ( CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
	    /* Copy the initial stroke. */
	    if ( state->stroke.num_points > 0 ) {
		(*init_stroke)->points = (Ppoint *)((_Pstore *)store)->buf;
		for ( i = 0; i < state->stroke.num_points; i++ ) {
		    (*init_stroke)->points[i].x = state->stroke.points[i].x;
		    (*init_stroke)->points[i].y = state->stroke.points[i].y;
		}
	    }
	}
    }
}

static void
inq_val_state( ws, dev, err, ret )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* locator device number	*/
    Pint	*err;		/* OUT error indicator	*/
    Phg_ret	*ret;
{
    Phg_args			cp_args;
    Wst_input_wsdt		*idt;

    if (idt = input_ws_open( phg_cur_cph, ws, Pfn_INQUIRY, NO_DT_NEEDED, err)) {
	if ( !DEVICE_EXISTS( idt, val, dev) ) {
	    *err = ERR250;
	} else {
	    INQ_STATE( phg_cur_cph, cp_args, ws, dev, PHG_ARGS_INP_VAL3, ret)
	    if ( ret->err )
		*err = ret->err;
	    else
		*err = 0;
	}
    }
}

static void
copy_val_rec( state, err, store, drec )
    Pvalst3	*state;
    Pint	*err;
    _Pstore	*store;
    Pval_data3  *drec;
{
    int		size;

    drec->low = state->record.low;
    drec->high = state->record.high;
    switch ( state->pet ) {
	case -1:
	    size = state->counts[0] + state->counts[1] + state->counts[2] +
		state->counts[3];
		if ( CB_STORE_SPACE( store, size, err ) ) {
		    drec->pets.pet_u1.label = store->buf;
		    if ( state->counts[0] > 0 )
			bcopy( state->record.pets.pet_u1.label,
			    drec->pets.pet_u1.label, state->counts[0] );
		    else
			drec->pets.pet_u1.label = NULL;

		    drec->pets.pet_u1.format =
			drec->pets.pet_u1.label + state->counts[0];
		    if ( state->counts[1] > 0 )
			bcopy( state->record.pets.pet_u1.format,
			    drec->pets.pet_u1.format, state->counts[1] );
		    else
			drec->pets.pet_u1.low_label = NULL;

		    drec->pets.pet_u1.low_label =
			drec->pets.pet_u1.format + state->counts[1];
		    if ( state->counts[2] > 0 )
			bcopy( state->record.pets.pet_u1.low_label,
			    drec->pets.pet_u1.low_label, state->counts[2] );
		    else
			drec->pets.pet_u1.high_label = NULL;

		    drec->pets.pet_u1.high_label =
			drec->pets.pet_u1.low_label + state->counts[2];
		    if ( state->counts[3] > 0 )
			bcopy( state->record.pets.pet_u1.high_label,
			    drec->pets.pet_u1.high_label, state->counts[3] );
		    else
			drec->pets.pet_u1.high_label = NULL;
		}
	    break;
	default:
	    break;
    }
}

void
pinq_val_st3( ws, dev, store, err, op_mode, echo_switch, init_value, 
	    prompt_echo, echo_vol, val_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* valuator device number	*/
    Pstore      store; 	/* OUT pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    Pfloat      *init_value;    /* OUT initial value    */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit3     *echo_vol;      /* OUT echo area        */
    Pval_data3  **val_data;     /* OUT data record      */    
{
    		Phg_ret		ret;
    register	Pvalst3		*state = &ret.data.inp_state.val;

    inq_val_state( ws, dev, err, &ret );
    if ( !*err ) {
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_value = state->val;
	*prompt_echo = state->pet;
	*echo_vol = state->e_volume;
	*val_data = &((_Pstore *)store)->data.val_data3.drec;
	copy_val_rec( state, err, ((_Pstore *)store), *val_data );
    }
}

void
pinq_val_st( ws, dev, store, err, op_mode, echo_switch, init_value, 
	    prompt_echo, echo_area, val_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* valuator device number	*/
    Pstore	store; 	/* OUT pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    Pfloat      *init_value;    /* OUT initial value    */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit      *echo_area;      /* OUT echo area        */
    Pval_data   **val_data;     /* OUT data record      */    
{
    Phg_ret		ret;
    register Pvalst3	*state = &ret.data.inp_state.val;

    inq_val_state( ws, dev, err, &ret);
    if ( !*err ) {
	CB_ECHO_VOLUME_TO_AREA( state->e_volume, *echo_area);
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_value = state->val;
	*prompt_echo = state->pet;
	*val_data = &((_Pstore *)store)->data.val_data.drec;
	copy_val_rec( state, err, ((_Pstore *)store), (Pval_data3 *)*val_data );
    }
}

static void
inq_choice_state( ws, dev, err, ret )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* choice device number */
    Pint	*err;		/* OUT error indicator	*/
    Phg_ret	*ret;
{
    Phg_args		cp_args;
    Wst_input_wsdt	*idt;

    if (idt = input_ws_open( phg_cur_cph, ws, Pfn_INQUIRY, NO_DT_NEEDED, err)) {
	if ( !DEVICE_EXISTS( idt, choice, dev) ) {
	    *err = ERR250;
	} else {
	    INQ_STATE( phg_cur_cph, cp_args, ws, dev, PHG_ARGS_INP_CHC3, ret)
	    if ( ret->err )
		*err = ret->err;
	    else
		*err = 0;
	}
    }
}

void
pinq_choice_st3( ws, dev, store, err, op_mode, echo_switch, init_status,
		init_choice, prompt_echo, echo_vol, choice_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* choice device number	*/
    Pstore	store; 	/* OUT pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    Pin_status     *init_status;   /* OUT initial choice status */
    Pint        *init_choice;   /* OUT initial choice   */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit3     *echo_vol;      /* OUT echo area        */
    Pchoice_data3 **choice_data; /* OUT data record      */
{
    int		size, i, length;
    Phg_ret	ret;
    Pchoicest3	*state = &ret.data.inp_state.choice.state;

    inq_choice_state( ws, dev, err, &ret );
    if ( !*err ) {
	*echo_vol = state->e_volume;
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*prompt_echo = state->pet;
	*init_status = state->choice.status;
	*init_choice = state->choice.choice;
	*choice_data = &((_Pstore *)store)->data.choice_data3.drec;
	switch ( state->pet ) {
	    case 2:
		(*choice_data)->pets.pet_r2.num_prompts =
		    state->record.pets.pet_r2.num_prompts;
		size = (*choice_data)->pets.pet_r2.num_prompts * 
			sizeof(Ppr_switch);
		if ( CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
		    bcopy( (char *)state->record.pets.pet_r2.prompts,
			(char *)(*choice_data)->pets.pet_r2.prompts, size );
		}
		break;
	    case 3:
		(*choice_data)->pets.pet_r3.num_strings =
		    state->record.pets.pet_r3.num_strings;
		length = ret.data.inp_state.choice.length;
		size = length + (*choice_data)->pets.pet_r3.num_strings
		    * sizeof(char*);
		if ( size > 0 && 
		     CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
		    (*choice_data)->pets.pet_r3.strings = 
			(char **)((_Pstore *)store)->buf;
		    (*choice_data)->pets.pet_r3.strings[0] =
			(char *)((*choice_data)->pets.pet_r3.strings +
			(*choice_data)->pets.pet_r3.num_strings);
		    bcopy( (char*) ret.data.inp_state.choice.strings,
			(char*)(*choice_data)->pets.pet_r3.strings[0], 
			length );
		    for ( i = 1; i < (*choice_data)->pets.pet_r3.num_strings;
			  i++ )
			(*choice_data)->pets.pet_r3.strings[i] =
			    (*choice_data)->pets.pet_r3.strings[i-1] + 1 +
			    strlen((*choice_data)->pets.pet_r3.strings[i-1]);
		}
		break;
	    default:
		break;
	}
    }
}

void
pinq_choice_st( ws, dev, store, err, op_mode, echo_switch, init_status,
	       init_choice, prompt_echo, echo_area, choice_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* choice device number	*/
    Pstore	store; 	/* OUT pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    Pin_status     *init_status;   /* OUT initial choice status */
    Pint        *init_choice;   /* OUT initial choice   */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit      *echo_area;      /* OUT echo area        */
    Pchoice_data **choice_data; /* OUT data record      */    
{
    int		size, i, length;
    Phg_ret	ret;
    Pchoicest3	*state = &ret.data.inp_state.choice.state;

    inq_choice_state( ws, dev, err, &ret);
    if ( !*err ) {
	CB_ECHO_VOLUME_TO_AREA( state->e_volume, *echo_area);
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*prompt_echo = state->pet;
	*init_status = state->choice.status;
	*init_choice = state->choice.choice;
	*choice_data = &((_Pstore *)store)->data.choice_data.drec;
	switch ( *prompt_echo ) {
	    case 2:
		(*choice_data)->pets.pet_r2.num_prompts =
		    state->record.pets.pet_r2.num_prompts;
		size = (*choice_data)->pets.pet_r2.num_prompts * sizeof(Ppr_switch);
		if ( CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
		    bcopy( (char *)state->record.pets.pet_r2.prompts,
			(char *)(*choice_data)->pets.pet_r2.prompts, size );
		}
		break;
	    case 3:
		(*choice_data)->pets.pet_r3.num_strings =
		    state->record.pets.pet_r3.num_strings;
		length = ret.data.inp_state.choice.length;
		size = length + (*choice_data)->pets.pet_r3.num_strings
		    * sizeof(char*);
		if ( size > 0 && CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
		    (*choice_data)->pets.pet_r3.strings = (char **)((_Pstore *)store)->buf;
		    (*choice_data)->pets.pet_r3.strings[0] =
			(char *)((*choice_data)->pets.pet_r3.strings +
			(*choice_data)->pets.pet_r3.num_strings);
		    bcopy( (char*) ret.data.inp_state.choice.strings,
			(char*)(*choice_data)->pets.pet_r3.strings[0], length );
		    for ( i = 1; i < (*choice_data)->pets.pet_r3.num_strings; i++ )
			(*choice_data)->pets.pet_r3.strings[i] =
			    (*choice_data)->pets.pet_r3.strings[i-1] + 1 +
			    strlen((*choice_data)->pets.pet_r3.strings[i-1]);
		}
		break;
	    default:
		break;
	}
    }
}

static void
inq_pick_state( ws, dev, type, err, ret )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pint	*err;		/* OUT error indicator	*/
    Phg_ret	*ret;
{
    Phg_args			cp_args;
    Wst_input_wsdt		*idt;

    if (idt = outin_ws_open( phg_cur_cph, ws, Pfn_INQUIRY, NO_DT_NEEDED, err)) {
	if ( !DEVICE_EXISTS( idt, pick, dev) ) {
	    *err = ERR250;
	} else {
	    cp_args.data.q_inp_state.inq_type = type;
	    INQ_STATE( phg_cur_cph, cp_args, ws, dev, PHG_ARGS_INP_PIK3, ret)
	    if ( ret->err )
		*err = ret->err;
	    else
		*err = 0;
	}
    }
}

static void
copy_pick_data( store, err, state, init_pick, filter )
    _Pstore	*store;
    Pint	*err;
    Ppickst3	*state;
    Ppick_path	*init_pick;	/* Note: must be set before calling */
    Pfilter	*filter;	/* Note: must be set before calling */
{
    int		total_size, path_size, infilt_size, exfilt_size;

    path_size = (state->pick.status == PIN_STATUS_OK
	? state->pick.pick_path.depth * sizeof(Ppick_path_elem) : 0);
    infilt_size = state->inclusion_filter.num_ints * sizeof(Pint);
    exfilt_size = state->exclusion_filter.num_ints * sizeof(Pint);
    total_size = path_size + infilt_size + exfilt_size;
    if ( CB_STORE_SPACE( store, total_size, err ) ) {
	init_pick->depth = state->pick.pick_path.depth;
	init_pick->path_list = (Ppick_path_elem *)store->buf;
	if ( path_size > 0 )
	    bcopy( (char*)state->pick.pick_path.path_list,
		(char*)init_pick->path_list, path_size );
	filter->incl_set.num_ints = state->inclusion_filter.num_ints;
	filter->incl_set.ints = (Pint *)store->buf + path_size;
	if ( infilt_size > 0 )
	    bcopy( (char*)state->inclusion_filter.ints,
		(char*)filter->incl_set.ints, infilt_size );
	filter->excl_set.num_ints = state->exclusion_filter.num_ints;
	filter->excl_set.ints = (Pint *)store->buf + path_size +
				    infilt_size;
	if ( exfilt_size > 0 )
	    bcopy( (char*)state->exclusion_filter.ints,
		(char*)filter->excl_set.ints, exfilt_size );
    }
}

void
pinq_pick_st3( ws, dev, type, store, err, op_mode, echo_switch, filter,
    init_status, init_pick, prompt_echo, echo_vol, pick_data, path_order
)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* pick device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pstore	store; 		/* pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch *echo_switch;  /* OUT echo switch      */
    Pfilter	**filter;	/* OUT pick filter */
    Pin_status  *init_status;   /* OUT initial pick status */
    Ppick_path  **init_pick;    /* OUT initial pick path */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit3     *echo_vol;      /* OUT echo area        */
    Ppick_data3 **pick_data;    /* OUT data record      */
    Ppath_order	*path_order;	/* OUT path order */
{
    		Phg_ret		ret;
    register	Ppickst3	*state = &ret.data.inp_state.pick;

    inq_pick_state( ws, dev, type, err, &ret );
    if ( !*err ) {
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_status = state->pick.status;
	*prompt_echo = state->pet;
	*echo_vol = state->e_volume;
	*path_order = state->order;
	*filter = &((_Pstore *)store)->data.pick_data3.filter;
	*init_pick = &((_Pstore *)store)->data.pick_data3.init_pick;
	copy_pick_data( ((_Pstore *)store), err, state, *init_pick, *filter );
	if ( !*err ) {
	    *pick_data = &((_Pstore *)store)->data.pick_data3.drec;
	    /* Copy the data record. */
	    switch ( state->pet ) {
		default:
		    break;
	    }
	}
    }
}

void
pinq_pick_st( ws, dev, type, store, err, op_mode, echo_switch, filter,
    init_status, init_pick, prompt_echo, echo_area, pick_data, path_order)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* pick device number	*/
    Pinq_type	type;		/* type of returned value	*/
    Pstore	store; 		/* pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch *echo_switch;  /* OUT echo switch      */
    Pfilter	**filter;	/* OUT pick filter */
    Pin_status  *init_status;   /* OUT initial pick status */
    Ppick_path  **init_pick;    /* OUT initial pick path */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit      *echo_area;     /* OUT echo area        */
    Ppick_data  **pick_data;    /* OUT data record      */
    Ppath_order	*path_order;	/* OUT path order */
{
    		Phg_ret		ret;
    register	Ppickst3	*state = &ret.data.inp_state.pick;

    inq_pick_state( ws, dev, type, err, &ret );
    if ( !*err ) {
	CB_ECHO_VOLUME_TO_AREA( state->e_volume, *echo_area);
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*init_status = state->pick.status;
	*prompt_echo = state->pet;
	*path_order = state->order;
	*filter = &((_Pstore *)store)->data.pick_data.filter;
	*init_pick = &((_Pstore *)store)->data.pick_data.init_pick;
	copy_pick_data( ((_Pstore *)store), err, state, *init_pick, *filter );
	if ( !*err ) {
	    *pick_data = &((_Pstore *)store)->data.pick_data.drec;
	    /* Copy the data record. */
	    switch ( state->pet ) {
		default:
		    break;
	    }
	}
    }
}

static void
inq_string_state( ws, dev, err, ret )
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    Pint	*err;		/* OUT error indicator	*/
    Phg_ret	*ret;
{
    Phg_args			cp_args;
    Wst_input_wsdt		*idt;

    if (idt = input_ws_open( phg_cur_cph, ws, Pfn_INQUIRY, NO_DT_NEEDED, err)) {
	if ( !DEVICE_EXISTS( idt, string, dev) ) {
	    *err = ERR250;
	} else {
	    INQ_STATE( phg_cur_cph, cp_args, ws, dev, PHG_ARGS_INP_STR3, ret)
	    if ( ret->err )
		*err = ret->err;
	    else
		*err = 0;
	}
    }
}

void
pinq_string_st3( ws, dev, store, err, op_mode, echo_switch, init_string,
	       prompt_echo, echo_vol, string_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    Pstore      store;         /* handle to Store object */
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    char       **init_string;   /* OUT initial string   */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit3     *echo_vol;      /* OUT echo area        */
    Pstring_data3 **string_data;    /* OUT data record      */
{
    		int		size;
    		Phg_ret		ret;
    register	Pstringst3	*state = &ret.data.inp_state.string.state;

    inq_string_state( ws, dev, err, &ret );
    if ( !*err ) {
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*prompt_echo = state->pet;
	*echo_vol = state->e_volume;
	*string_data = &((_Pstore *)store)->data.string_data3.drec;
	(*string_data)->buffer_size = state->record.buffer_size;
	(*string_data)->init_pos = state->record.init_pos;
	size = ret.data.inp_state.string.length;
	if ( CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
	    *init_string = ((_Pstore *)store)->buf;
	    if ( ret.data.inp_state.string.state.string )
		strcpy( *init_string, ret.data.inp_state.string.state.string );
	}
    }
}

void
pinq_string_st( ws, dev, store, err, op_mode, echo_switch, init_string,
	       prompt_echo, echo_area, string_data)
    Pint	ws;		/* workstation identifier	*/
    Pint	dev;		/* string device number	*/
    Pstore	store; 	/* OUT pointer to buffer	*/
    Pint	*err;		/* OUT error indicator	*/
    Pop_mode    *op_mode;       /* OUT operating mode   */
    Pecho_switch       *echo_switch;   /* OUT echo switch      */
    char       **init_string;   /* OUT initial string   */
    Pint        *prompt_echo;   /* OUT prompt/echo type */
    Plimit      *echo_area;      /* OUT echo area        */
    Pstring_data **string_data;    /* OUT data record      */
{
    		int		size;
    		Phg_ret		ret;
    register	Pstringst3	*state = &ret.data.inp_state.string.state;

    inq_string_state( ws, dev, err, &ret);
    if ( !*err ) {
	CB_ECHO_VOLUME_TO_AREA( state->e_volume, *echo_area);
	*op_mode = state->mode;
	*echo_switch = state->esw;
	*prompt_echo = state->pet;
	*string_data = &((_Pstore *)store)->data.string_data.drec;
	(*string_data)->buffer_size = state->record.buffer_size;
	(*string_data)->init_pos = state->record.init_pos;
	size = ret.data.inp_state.string.length;
	if ( CB_STORE_SPACE( ((_Pstore *)store), size, err ) ) {
	    *init_string = ((_Pstore *)store)->buf;
	    if ( ret.data.inp_state.string.state.string )
		strcpy( *init_string, ret.data.inp_state.string.state.string );
	}
    }
}

void
pinq_more_simult_events( error_ind, events )
    Pint        	*error_ind;	/* OUT error indicator	*/
    Pmore_simult_events	*events;	/* OUT simultaneous events	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY) ) {
	*error_ind = ERR2;
    } else {
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_MORE_EVENTS, &cp_args, &ret);
	if ( ret.err )
	    *error_ind = ret.err;
	else {
	    *error_ind = 0;
	    *events = (Pmore_simult_events)ret.data.idata;
	}
    }
}

void
pinq_in_overf( error_ind, ws, class, dev )
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*ws;		/* OUT workstation identifier	*/
    Pin_class	*class;		/* OUT input class	*/
    Pint	*dev;		/* OUT input device number	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY) )
	*error_ind = ERR3;
    else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP )
	*error_ind = ERR3;
    else {
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_INP_OVERFLOW, &cp_args, &ret);
	if ( ret.err )
	    *error_ind =  ret.err;
	else {
	    *error_ind = 0;
	    *ws = ret.data.oflow_event.ws;
	    *class = ret.data.oflow_event.class;
	    *dev = ret.data.oflow_event.dev;
	}
    }
}
