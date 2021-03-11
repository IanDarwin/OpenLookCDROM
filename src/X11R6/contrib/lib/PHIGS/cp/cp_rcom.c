/* $XConsortium: cp_rcom.c,v 5.8 94/04/17 20:41:12 rws Exp $ */

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

/* Open and close monitor CPs */

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"

/* check for both EAGAIN and EWOULDBLOCK, because some supposedly POSIX
 * systems are broken and return EWOULDBLOCK when they should return EAGAIN
 */
#if defined(EAGAIN) && defined(EWOULDBLOCK)
#define ETEST(err) (err == EAGAIN || err == EWOULDBLOCK)
#else
#ifdef EAGAIN
#define ETEST(err) (err == EAGAIN)
#else
#define ETEST(err) (err == EWOULDBLOCK)
#endif
#endif

#ifdef DEBUG
int	cpr_print_trace = 0;
#define	TRACE_GENERAL	(~0)	/* any bit on = trace most CP functions */
#define	TRACE_DATA	0x02	/* trace CP_OP_*_DATA & CP_OP_FORCE_RETURN */
#define	TRACE_AWAIT_RET	0x04	/* trace INP_AWAIT & print "CP RETURN" */
#define	TRACE_SIZES	0x08	/* trace */
#endif /* DEBUG */

#define CPR_EXEC_CMD( cph, args, ret) \
    (*(cph)->funcs[(int)(args)->op & CP_OPCODE_BITS]) ((cph), (args), (ret))

#ifndef PEX_API_SOCKET_IPC
#define CPR_SEND_RET( cph, ret) \
    phg_cpr_send( (cph)->data.monitor.sfd, (char*)(&(ret)->err), \
	sizeof((ret)->err))
#else
#define CPR_SEND_RET( cph, ret) \
    phg_cpr_send( (cph)->data.monitor.sfd, (char*)(ret), sizeof(Phg_ret))
#endif

#define CPR_SEND( cph, p, size) \
    phg_cpr_send( (cph)->data.monitor.sfd, (char*)(p), (size))

#define CPR_RESET_CMD_STATE( cph) \
    (	(cph)->data.monitor.cmd_state = CP_READING_ARGS, \
    	(cph)->data.monitor.cur_cnt = 0	)


Cp_file*
phg_cpr_fdopen( fd)
    int		fd;
{
    register Cp_file	*f;

    if ( f = (Cp_file*)malloc(sizeof(Cp_file))) {
	f->cnt = 0;
	f->fd = fd;
	f->bufsize = BUFSIZ;
	if ( !(f->ptr = f->base = malloc((unsigned)f->bufsize))) {
	    free((char *)f);
	    f = NULL;
	}
    }
    return f;
}

void
phg_cpr_fclose( f)
    Cp_file	*f;
{
    (void)close( f->fd );
    free( f->base );
    free( (char *)f );
}


static int
cpr_fill_buffer( f)
    register Cp_file	*f;
{
    f->ptr = f->base;
    if ( (f->cnt = recv( f->fd, f->base, f->bufsize, 0)) >= 0) {
	return f->cnt;
    } else if (ETEST(errno) || errno == EINTR) {
	return (f->cnt = 0);
    } else {
	return f->cnt;
    }
}

int
phg_cpr_fread( f, ptr, count)
    register Cp_file	*f;
    register char	*ptr;
    register int	count;
{
    register int n, nleft, r;

    if (count <= 0)
	return 0;
    nleft = count;
    for ( ; ; ) {
	if ( f->cnt <= 0) { /* empty buffer */
	    if ( (r = cpr_fill_buffer( f)) <= 0)
		return ( r < 0 ? -(count - nleft) : (count - nleft));
	}
	n = MIN(nleft, f->cnt);
	bcopy( (char *)f->ptr, ptr, n );
	ptr += n;
	f->cnt -= n;
	f->ptr += n;
	if ((nleft -= n) == 0)
	    return (count);
    }
}


static int
wait_for_write(s)
    int s;
{
    unsigned long	wfd[MSKCNT];

    CLEARBITS(wfd);
    BITSET(wfd, s);
    if (select(s+1, NULL, wfd, NULL, (struct timeval *)NULL) < 0 &&
	errno != EINTR){
	perror("phg_cpr_send(select)");
	return 0;
    }
    return 1;
}

int
phg_cpr_send( s, msg, size)
    register int	s;
    register char	*msg;
    register int	size;
{
    register int	c, wc, todo;

    c = 0;
    todo = size;
    while ( todo) {
	if ( (wc = write( s, msg + c, size - c)) >= 0) {
	    c += wc;
	    todo = size - c;
	} else if ( ETEST(errno)) {
	    if (!wait_for_write(s))
		break;
	} else if ( errno == EINTR) {
	    continue;
#ifdef EMSGSIZE
	} else if (errno == EMSGSIZE) {
	    if (todo > 1)
		todo >>= 1;
	    else if (!wait_for_write(s))
		break;
#endif
	} else {
	    perror("phg_cpr_send(write)");
	    break;
	}
    }
    return c;
}


static int
cpr_recv_data( cph, size)
    register Cp_handle		cph;
    register int		size;
{
    register Cp_data_monitor	*r = &cph->data.monitor;
    register int		wc;

    if ( !PHG_SCRATCH_SPACE(&cph->scratch, size)) {
	ERR_BUF( cph->erh, ERR900);
	/* TODO: need to flush the data from the socket */

    } else if ( (wc = phg_cpr_fread( r->infile, cph->scratch.buf + r->cur_cnt,
	size - r->cur_cnt)) > 0) {
	r->cur_cnt += wc;

    } else if ( wc < 0) {
	perror("cpr_recv_data, catastrophic!");
    }
    return r->cur_cnt;
}


static int
inp_init_recv( cph, args, rv )
    Cp_handle		cph;
    Phg_args		*args;
    Cp_recv_vec		rv[];
{
    int		vcount = 0;

    switch ( args->data.inp_init_dev.class ) {
	case PHG_ARGS_INP_STK3:
	case PHG_ARGS_INP_STK: {
	    Pstroke3 *stk = &args->data.inp_init_dev.data.stk.init;
	    if ( stk->num_points > 0 ) {
		vcount = 1;
		rv->size = stk->num_points * sizeof(Ppoint3);
		rv->dpp = (char**)&stk->points;
	    }
	    } break;
	case PHG_ARGS_INP_PIK3:
	case PHG_ARGS_INP_PIK:{
	    Ppick *pick = &args->data.inp_init_dev.data.pik.init;
	    if ( pick->status == PIN_STATUS_OK && pick->pick_path.depth > 0 ) {
		vcount = 1;
		rv->size = pick->pick_path.depth * sizeof(Ppick_path_elem);
		rv->dpp = (char**)&pick->pick_path.path_list;
	    }
	    } break;
	    break;
	case PHG_ARGS_INP_STR3:
	case PHG_ARGS_INP_STR:
	    if ( args->data.inp_init_dev.data.str.init.length > 0 ) {
		vcount = 1;
		rv->size = args->data.inp_init_dev.data.str.init.length;
		rv->dpp =(char**)&args->data.inp_init_dev.data.str.init.string;
	    }
	    break;
	case PHG_ARGS_INP_CHC3:
	case PHG_ARGS_INP_CHC: {
	    Pchoice_data	*rec = &args->data.inp_init_dev.data.cho.rec;
	    switch ( args->data.inp_init_dev.pet ) {
		case 2:
		    if ( rec->pets.pet_r2.num_prompts > 0 ) {
			vcount = 1;
			rv->size = rec->pets.pet_r2.num_prompts*sizeof(Ppr_switch);
			rv->dpp = (char**)&rec->pets.pet_r2.prompts;
		    }
		    break;
		case 3:
		  if ( args->data.inp_init_dev.data.cho.string_list_size > 0) {
		      vcount = 1;
		      rv->size =
			  args->data.inp_init_dev.data.cho.string_list_size;
		      rv->dpp = (char**)&rec->pets.pet_r3.strings;
		  }
		  break;
		case 5:
		    if ( rec->pets.pet_r5.num_pick_ids > 0 ) {
			vcount = 1;
			rv->size = rec->pets.pet_r5.num_pick_ids*sizeof(Pint);
			rv->dpp = (char**)&rec->pets.pet_r5.pick_ids;
		    }
		    break;
	    }
	} break;
	case PHG_ARGS_INP_VAL3:
	case PHG_ARGS_INP_VAL:
	    switch ( args->data.inp_init_dev.pet ) {
	      case -1: {
		Phg_args_init_data	*data = &args->data.inp_init_dev.data;
		if ( data->val.counts[0] > 0 ) {
		    rv[vcount].size = data->val.counts[0];
		    rv[vcount].dpp = (char **)&data->val.rec.pets.pet_u1.label;
		    ++vcount;
		}
		if ( data->val.counts[1] > 0 ) {
		    rv[vcount].size = data->val.counts[1];
		    rv[vcount].dpp = (char **)&data->val.rec.pets.pet_u1.format;
		    ++vcount;
		}
		if ( data->val.counts[2] > 0 ) {
		    rv[vcount].size = data->val.counts[2];
		    rv[vcount].dpp =
			(char **)&data->val.rec.pets.pet_u1.low_label;
		    ++vcount;
		}
		if ( data->val.counts[3] > 0 ) {
		    rv[vcount].size = data->val.counts[3];
		    rv[vcount].dpp =
			(char **)&data->val.rec.pets.pet_u1.high_label;
		    ++vcount;
		}
	      } break;
	    }
	    break;
    }
    return vcount;
}

static int
cpr_get_pre_data( cph, args)
    register Cp_handle		cph;
    register Phg_args		*args;
{
    Cp_recv_vec			rvec[CP_MAX_COM_PTRS];
    register Cp_recv_vec	*rv = rvec;
    int				vcount = 0, status = 1;

    switch ( (int)args->op & CP_OPCODE_BITS) {
	case CP_FUNC_OP_ADD_EL:
	    vcount = 1;
	    rv->size = args->data.add_el.pex_oc.size;
	    rv->dpp = (char **)&args->data.add_el.pex_oc.oc;
	    break;
	case CP_FUNC_OP_MESSAGE:
	    vcount = 1;
	    rv->size = args->data.message.msg_length;
	    rv->dpp = &args->data.message.msg;
	    break;
	case CP_FUNC_OP_AR_ARCHIVE:
	case CP_FUNC_OP_AR_RETRIEVE:
	case CP_FUNC_OP_AR_DELETE:
	    if ( args->data.ar_info.op != PHG_ARGS_AR_ALL) {
		vcount = 1;
	        rv->size = args->data.ar_info.data.num_ints * sizeof(Pint);
	        rv->dpp = (char**)&args->data.ar_info.data.ints;
	    }
	    break;
	case CP_FUNC_OP_SET_FILTER:
	    vcount = 2;
	    rv->size = args->data.set_filter.inc_set.num_ints * sizeof(Pint);
	    rv->dpp = (char**)&args->data.set_filter.inc_set.ints;
	    rv[1].size = args->data.set_filter.exc_set.num_ints * sizeof(Pint);
	    rv[1].dpp = (char**)&args->data.set_filter.exc_set.ints;
	    break;
	case CP_FUNC_OP_AR_OPEN:
	    vcount = 1;
	    rv->size = args->data.ar_open.name_length;
	    rv->dpp = &args->data.ar_open.fname;
	    break;
	case CP_FUNC_OP_OPEN_WS:
	    vcount = 4;
	    rv[0].size = sizeof(Wst);
	    rv[0].dpp = (char **)&args->data.open_ws.type;
	    rv[1].size = args->data.open_ws.wst_buffer_size;
	    rv[1].dpp = (char **)&args->data.open_ws.wst_buffer;
	    rv[2].size = args->data.open_ws.conn_info.display_name_length;
	    rv[2].dpp = (char **)&args->data.open_ws.conn_info.display_name;
	    rv[3].size = args->data.open_ws.wst_display_name_length;
	    rv[3].dpp = (char **)&args->data.open_ws.wst_display_name;
	    break;
	case CP_FUNC_OP_INQ_TEXT_EXTENT:
	    vcount = 1;
	    rv->size = args->data.q_text_extent.length;
	    rv->dpp = &args->data.q_text_extent.str;
	    break;
	case CP_FUNC_OP_INP_INIT_DEV:
	    vcount = inp_init_recv( cph, args, rv);
	    break;
	case CP_FUNC_OP_EL_SEARCH:
	    vcount = 2;
	    rv[0].size = args->data.el_search.incl.num_elem_types
		* sizeof(Pelem_type);
	    rv[0].dpp = (char**)&args->data.el_search.incl.elem_types;
	    rv[1].size = args->data.el_search.excl.num_elem_types
		* sizeof(Pelem_type);
	    rv[1].dpp = (char**)&args->data.el_search.excl.elem_types;
	    break;
	case CP_FUNC_OP_INC_SPA_SEARCH:
            /* TODO: Copy the filters */
	    vcount = 5;
	    rv[0].size = args->data.inc_spa_search.start_path.num_elem_refs
		* sizeof(Pint);
	    rv[0].dpp = (char **)
		&args->data.inc_spa_search.start_path.elem_refs;
	    rv[1].size = args->data.inc_spa_search.nrm_filt.incl_set.num_ints
		* sizeof(Pint);
	    rv[1].dpp = (char **)
		&args->data.inc_spa_search.nrm_filt.incl_set.ints;
	    rv[2].size = args->data.inc_spa_search.nrm_filt.excl_set.num_ints
		* sizeof(Pint);
	    rv[2].dpp = (char **)
		&args->data.inc_spa_search.nrm_filt.excl_set.ints;
	    rv[3].size = args->data.inc_spa_search.inv_filt.incl_set.num_ints
		* sizeof(Pint);
	    rv[3].dpp = (char **)
		&args->data.inc_spa_search.inv_filt.incl_set.ints;
	    rv[4].size = args->data.inc_spa_search.inv_filt.excl_set.num_ints
		* sizeof(Pint);
	    rv[4].dpp = (char **)
		&args->data.inc_spa_search.inv_filt.excl_set.ints;
	    break;
	case CP_FUNC_OP_SET_REP:
	    switch ( args->data.set_rep.type ) {
		case PHG_ARGS_PTREP:
		    vcount = 1;
		    rv[0].size =
			  args->data.set_rep.rep.bundl.ptrep.dims.size_x
		        * args->data.set_rep.rep.bundl.ptrep.dims.size_y
			* sizeof(Pint);
		    rv[0].dpp = (char **)
			&args->data.set_rep.rep.bundl.ptrep.colr_array;
		    break;
		case PHG_ARGS_EXTPTREP:
		    vcount = 1;
		    rv[0].size =
			  args->data.set_rep.rep.bundl.extptrep.dims.size_x
		        * args->data.set_rep.rep.bundl.extptrep.dims.size_y
			* sizeof(Pcoval);
		    rv[0].dpp = (char **)
			&args->data.set_rep.rep.bundl.extptrep.colr_array;
		    break;
	    }
	    break;
	case CP_FUNC_OP_DRAWABLE_PICK:
	    vcount = 2;
	    rv[0].size = args->data.drawable_pick.filter.incl_set.num_ints
		* sizeof(Pint);
	    rv[0].dpp = (char**)&args->data.drawable_pick.filter.incl_set.ints;
	    rv[1].size = args->data.drawable_pick.filter.excl_set.num_ints
		* sizeof(Pint);
	    rv[1].dpp = (char**)&args->data.drawable_pick.filter.excl_set.ints;
	    break;
	case CP_FUNC_OP_MAP_POINTS:
	    vcount = 1;
	    rv[0].size = args->data.map_points.points.num_points
		* sizeof(args->data.map_points.points.points[0]);
	    rv[0].dpp = (char**)&args->data.map_points.points.points;
	    break;
	case CP_FUNC_OP_REDRAW_REGIONS:
	    vcount = 1;
	    rv[0].size = args->data.redraw_regions.num_regions
		* sizeof(args->data.redraw_regions.regions[0]);
	    rv[0].dpp = (char**)&args->data.redraw_regions.regions;
	    break;
    }

    if ( args->data_size > 0 ) {
	register int	i;
	register char	*p = NULL;

	if ( (int)args->op & CP_OP_DATA_FOLLOWS_ARGS ) {
	    if ( cpr_recv_data( cph, (int)args->data_size ) != args->data_size )
		status = 0;		/* didn't get all the data yet */
	    else
		p = cph->scratch.buf;
	} else if ( (int)args->op & CP_OP_DATA_IN_BUFFER ) {
	    p = (char *)&cph->shm_buf->data.buf[cph->shm_buf->data.head];
	}

	if ( p ) {
	    for ( i = 0; i < vcount; i++) {
		*rv[i].dpp = p;
		p += rv[i].size;
	    }
	}
    }

    return status;
}


static int
setup_event_send( event, sv)
    Phg_ret_inp_event		*event;
    register Cp_send_vec	*sv;
{
    int		vcount = 0;

    switch ( event->id.class ) {
	case PIN_LOC:
	case PIN_VAL:
	case PIN_CHOICE:
	default:
	    break;
	case PIN_STROKE:
	    if ( event->data.stk.num_points > 0 ) {
		vcount = 1;
		sv->size = event->data.stk.num_points * sizeof(Ppoint3);
		sv->data = (char*)event->data.stk.points;
	    }
	    break;
	case PIN_PICK:
	    if ( event->data.pik.status == PIN_STATUS_OK
		&& event->data.pik.pick_path.depth > 0 ) {
		vcount = 1;
		sv->size = event->data.pik.pick_path.depth*sizeof(Ppick_path_elem);
		sv->data = (char*)event->data.pik.pick_path.path_list; }
	    break;
	case PIN_STRING:
	    if ( event->data.str.length > 0 ) {
		vcount = 1;
		sv->size = event->data.str.length;
		sv->data = event->data.str.string;
	    }
	    break;
    }
    return vcount;
}

static int
setup_inp_state_send( class, ret, sv )
    Phg_args_idev_class		class;
    Phg_ret_inp_state		*ret;
    Cp_send_vec			*sv;
{
    int		vcount = 0;

    switch ( class ) {
	case PHG_ARGS_INP_STK3:
	case PHG_ARGS_INP_STK: {
	    Pstrokest3		*stkst = &ret->stroke;
	    if ( stkst->stroke.num_points > 0 ) {
		vcount = 1;
		sv->size =  stkst->stroke.num_points * sizeof(Ppoint3);
		sv->data = (char*)stkst->stroke.points;
	    }
	    } break;
	case PHG_ARGS_INP_PIK3:
	case PHG_ARGS_INP_PIK:{
	    Ppickst3		*pickst = &ret->pick;
	    if ( pickst->pick.status == PIN_STATUS_OK ) {
		++vcount;
		sv->size = pickst->pick.pick_path.depth * sizeof(Ppick_path_elem);
		sv->data = (char*)pickst->pick.pick_path.path_list;
		++sv;
	    }
	    if ( pickst->inclusion_filter.num_ints > 0 ) {
		++vcount;
		sv->size = pickst->inclusion_filter.num_ints * sizeof(Pint);
		sv->data = (char*)pickst->inclusion_filter.ints;
		++sv;
	    }
	    if ( pickst->exclusion_filter.num_ints > 0 ) {
		++vcount;
		sv->size = pickst->exclusion_filter.num_ints * sizeof(Pint);
		sv->data = (char*)pickst->exclusion_filter.ints;
	    }
	    } break;
	case PHG_ARGS_INP_STR3:
	case PHG_ARGS_INP_STR: {
	    Phg_ret_string_state	*strst = &ret->string;
	    if ( strst->length > 0 ) {
		vcount = 1;
		sv->size =  strst->length;
		sv->data = strst->state.string;
	    }
	} break;
	case PHG_ARGS_INP_VAL3:
	case PHG_ARGS_INP_VAL:
	    switch ( ret->val.pet ) {
	      case -1:
		if ( ret->val.counts[0] > 0 ) {
		    sv[vcount].size = ret->val.counts[0];
		    sv[vcount].data = ret->val.record.pets.pet_u1.label;
		    ++vcount;
		}
		if ( ret->val.counts[1] > 0 ) {
		    sv[vcount].size = ret->val.counts[1];
		    sv[vcount].data = ret->val.record.pets.pet_u1.format;
		    ++vcount;
		}
		if ( ret->val.counts[2] > 0 ) {
		    sv[vcount].size = ret->val.counts[2];
		    sv[vcount].data = ret->val.record.pets.pet_u1.low_label;
		    ++vcount;
		}
		if ( ret->val.counts[3] > 0 ) {
		    sv[vcount].size = ret->val.counts[3];
		    sv[vcount].data = ret->val.record.pets.pet_u1.high_label;
		    ++vcount;
		}
	        break;
	    }
	    break;
	case PHG_ARGS_INP_CHC3:
	case PHG_ARGS_INP_CHC: {
	    Phg_ret_choice_state	*chst = &ret->choice;
	    Pchoice_data		*rec = &chst->state.record;
	    switch ( chst->state.pet ) {
		case 2:
		    if ( rec->pets.pet_r2.num_prompts > 0 ) {
			vcount = 1;
			sv->size = rec->pets.pet_r2.num_prompts*sizeof(Ppr_switch);
			sv->data = (char*)rec->pets.pet_r2.prompts;
		    }
		    break;
		case 3:
		    if ( chst->length > 0 ) {
			vcount = 1;
			sv->size =  chst->length;
			sv->data = chst->strings;
		    }
		    break;
		case 5:
		    if ( rec->pets.pet_r5.num_pick_ids > 0 ) {
			vcount = 1;
			sv->size = rec->pets.pet_r5.num_pick_ids*sizeof(Pint);
			sv->data = (char*)rec->pets.pet_r5.pick_ids;
		    }
		    break;
	    }
	} break;
    }

    return vcount;
}

static void
cpr_send_post_data( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    register Phg_ret	*ret;
{
    Cp_send_vec			svec[CP_MAX_COM_PTRS];
    register Cp_send_vec	*sv = svec;
    int				vcount = 0;

    switch ( (int)args->op & CP_OPCODE_BITS) {
	case CP_FUNC_OP_AR_GET_NAMES:
	case CP_FUNC_OP_INQ_STRUCT_IDS:
	case CP_FUNC_OP_INQ_INDICES:
	case CP_FUNC_OP_INQ_CONFLICTING:
	case CP_FUNC_OP_INQ_WSS_POSTED_TO:
	    if ( !ret->err && ret->data.int_list.num_ints > 0) {
		vcount = 1;
		sv->size = ret->data.int_list.num_ints * sizeof(Pint);
		sv->data = (char*)ret->data.int_list.ints;
	    }
	    break;
	case CP_FUNC_OP_OPEN_WS:
	    if ( !ret->err) {
		vcount = 2;
		sv[0].size = sizeof(*ret->data.open_ws.wstype);
		sv[0].data = (char *)ret->data.open_ws.wstype;
		sv[1].size = ret->data.open_ws.wstype->buffer_size;
		sv[1].data = (char*)ret->data.open_ws.wstype->buffer;
	    }
	    break;
	case CP_FUNC_OP_INQ_EL_CONTENT:
	    if ( !ret->err) {
		vcount = 1;
		sv->size = ret->data.el_info.pex_oc.size;
		sv->data = (char *)ret->data.el_info.pex_oc.oc;
	    }
	    break;
	case CP_FUNC_OP_INQ_HIERARCHY:
	case CP_FUNC_OP_AR_GET_HIERARCHY:
	    if ( !ret->err) {
		vcount = 2;
		sv->size = ret->data.hierarchy.counts.num_ints * sizeof(Pint);
		sv->data = (char*)ret->data.hierarchy.counts.ints;
		sv[1].size = ret->data.hierarchy.num_pairs * sizeof(Pelem_ref);
		sv[1].data = (char*)ret->data.hierarchy.paths;
	    }
	    break;
	case CP_FUNC_OP_INQ_POSTED:
	    if ( !ret->err && ret->data.postlist.num_postings > 0) {
		vcount = 1;
		sv->size = ret->data.postlist.num_postings * sizeof(Pposted_struct);
		sv->data = (char*)ret->data.postlist.postings;
	    }
	    break;
	case CP_FUNC_OP_INQ_VIEW_REP:
	    if ( !ret->err) {
		vcount = 2;
		sv->size = sizeof(Pview_rep3);
		sv->data = (char*)ret->data.view_rep.cur_rep;
		sv[1].size = sizeof(Pview_rep3);
		sv[1].data = (char*)ret->data.view_rep.req_rep;
	    }
	    break;
	case CP_FUNC_OP_INP_AWAIT:
	    if ( !ret->err)
		vcount = setup_event_send( &ret->data.inp_event, sv);
	    break;
	case CP_FUNC_OP_INP_SAMPLE:
	    if ( !ret->err)
		vcount = setup_event_send( &ret->data.inp_event, sv);
	    break;
	case CP_FUNC_OP_INP_REQUEST:
	    if ( !ret->err && !ret->data.inp_request.brake)
		vcount = setup_event_send(&ret->data.inp_request.event, sv);
	    break;
	case CP_FUNC_OP_INQ_INP_DEV_STATE:
	    if ( !ret->err )
		vcount = setup_inp_state_send( args->data.q_inp_state.class,
		    &ret->data.inp_state, sv);
	    break;
	case CP_FUNC_OP_INQ_FILTER:
	    if ( !ret->err ) {
		if ( ret->data.filter.incl.num_ints > 0 ) {
		    sv->size = ret->data.filter.incl.num_ints * sizeof(Pint);
		    sv->data = (char*)ret->data.filter.incl.ints;
		    vcount = 1;
		}
		if ( ret->data.filter.excl.num_ints > 0 ) {
		    sv[vcount].size =
			ret->data.filter.excl.num_ints * sizeof(Pint);
		    sv[vcount].data = (char*)ret->data.filter.excl.ints;
		    ++vcount;
		}
	    }
	    break;
        case CP_FUNC_OP_INC_SPA_SEARCH:
	    if ( !ret->err ) {
                vcount = 1;
                sv->size = ret->data.inc_spa_search.path.num_elem_refs
		    * sizeof(Pelem_ref);
                sv->data = (char*)ret->data.inc_spa_search.path.elem_refs;
	    }
            break;
	case CP_FUNC_OP_INQ_REPRESENTATION:
	    switch ( args->data.q_rep.rep_type ) {
		case PHG_ARGS_EXTPTREP:
		    vcount = 1;
		    sv[0].size = ret->data.rep.extptrep.dims.size_x
		        * ret->data.rep.extptrep.dims.size_y * sizeof(Pcoval);
		    sv[0].data = (char *) ret->data.rep.extptrep.colr_array;
		    break;
	    }
	    break;
	case CP_FUNC_OP_DRAWABLE_PICK:
	    if ( ret->data.drawable_pick.status == PIN_STATUS_OK ) {
		vcount = 1;
		sv[0].size = ret->data.drawable_pick.pick.depth
		    * sizeof(Ppick_path_elem);
		sv[0].data = (char*)ret->data.drawable_pick.pick.path_list;
	    }
	    break;
	case CP_FUNC_OP_MAP_POINTS:
	    vcount = 1;
	    sv[0].size = ret->data.map_points.points.num_points
		* sizeof(ret->data.map_points.points.points[0]);
	    sv[0].data = (char*)ret->data.map_points.points.points;
    }

    if ( vcount > 0) {
	register int	i, size;

	for ( i = 0, size = 0; i < vcount; i++)
	    size += sv[i].size;

#ifdef DEBUG
	/* print out size info about the return block */
	if ( cpr_print_trace & TRACE_SIZES )
	    fprintf(stderr, "\tRETURNED_DATA %d\n", size);
#endif /* DEBUG */

	if ( size > cph->shm_buf->ret.buf_size ) {
	    if ( (int)args->op & CP_OP_RET_DATA_IN_SOCKET ) {
		for ( i = 0; i < vcount; i++)
		    (void)CPR_SEND( cph, sv[i].data, sv[i].size);
	    } else {
		/* Do it on the next call. */
		args->op |= CP_OP_RET_DATA_IN_SOCKET;
	    }

	} else if ( size > 0 ) {
	    /* Copy data to the shared memory buffer. */
	    register char	*p = (char *)cph->shm_buf->ret.buf;

	    /* TODO: worry about alignment of each sub-block. */
	    for ( i = 0; i < vcount; i++ ) {
		bcopy( sv[i].data, p, sv[i].size );
		p += sv[i].size;
	    }
	}
    }
}


static void
cpr_deque_shm_data( cph, size )
    Cp_handle		cph;
    unsigned int	size;
{
    Cp_shm_buf		*buf = cph->shm_buf;

    CP_GRAB_SHM_LOCK(buf,data)
    buf->data.head += CP_DATA_BUF_SIZE_NEEDED(cph,size);
    if ( buf->data.head  == buf->data.tail )
	buf->data.head = buf->data.tail = 0;
    CP_RELEASE_SHM_LOCK(buf,data)
}


void
phg_cpr_send_ret( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    /* NOTE: This is the same code as in cpr_exec_cmd() below.
     * This function is called by the child to return data that the parent
     * is waiting for but that is the result of an event from the child,
     * i.e., the cpc_exec_cmd returned without sending the reply.  Request
     * input is a good example of this.
     */
    if ( (int)args->op & CP_OP_RETURN_DATA) {
#ifndef PEX_API_SOCKET_IPC
	bcopy( (char *)ret, (char *)&cph->shm_buf->ret.data, sizeof(Phg_ret) );
#endif
	if ( (int)args->op & CP_OP_POST_DATA )
	    cpr_send_post_data( cph, args, ret );

	if ( CPR_SEND_RET(cph, ret) )
	    if ( (int)args->op & CP_OP_RET_DATA_IN_SOCKET )
		cpr_send_post_data( cph, args, ret );
    }
}

#ifdef DEBUG

static void
print_op( op )
    Cp_func_op	op;
{
    char	*str, buf[10];

    switch ( (int)op & CP_OPCODE_BITS ) {
    case CP_FUNC_OP_OPEN_WS: str = "OPEN_WS";break;
    case CP_FUNC_OP_CLOSE_WS: str = "CLOSE_WS";break;
    case CP_FUNC_OP_WS_REDRAW_ALL: str = "WS_REDRAW_ALL";break;
    case CP_FUNC_OP_WS_UPDATE: str = "WS_UPDATE";break;
    case CP_FUNC_OP_SET_DISP_STATE: str = "SET_DISP_STATE";break;
    case CP_FUNC_OP_MESSAGE: str = "MESSAGE";break;
    case CP_FUNC_OP_SET_REP: str = "SET_REP";break;
    case CP_FUNC_OP_SET_FILTER: str = "SET_FILTER";break;
    case CP_FUNC_OP_SET_COLOUR_MODEL: str = "SET_COLOUR_MODEL";break;
    case CP_FUNC_OP_SET_HLHSR_MODE: str = "SET_HLHSR_MODE";break;
    case CP_FUNC_OP_SET_VIEW_INPUT_PRIO: str = "SET_VIEW_INPUT_PRIO";break;
    case CP_FUNC_OP_SET_WS_WIN: str = "SET_WS_WIN";break;
    case CP_FUNC_OP_SET_WS_VP: str = "SET_WS_VP";break;
    case CP_FUNC_OP_ADD_EL: str = "ADD_EL";break;
    case CP_FUNC_OP_COPY_ALL_ELS: str = "COPY_ALL_ELS";break;
    case CP_FUNC_OP_OPEN_STRUCT: str = "OPEN_STRUCT";break;
    case CP_FUNC_OP_CLOSE_STRUCT: str = "CLOSE_STRUCT";break;
    case CP_FUNC_OP_SET_EL_PTR: str = "SET_EL_PTR";break;
    case CP_FUNC_OP_SET_EDIT_MODE: str = "SET_EDIT_MODE";break;
    case CP_FUNC_OP_DELETE_EL: str = "DELETE_EL";break;
    case CP_FUNC_OP_DELETE_STRUCT: str = "DELETE_STRUCT";break;
    case CP_FUNC_OP_DELETE_STRUCT_NET: str = "DELETE_STRUCT_NET";break;
    case CP_FUNC_OP_DELETE_ALL_STRUCTS: str = "DELETE_ALL_STRUCTS";break;
    case CP_FUNC_OP_CHANGE_STRUCT_ID: str = "CHANGE_STRUCT_ID";break;
    case CP_FUNC_OP_CHANGE_STRUCT_REFS: str = "CHANGE_STRUCT_REFS";break;
    case CP_FUNC_OP_CHANGE_STRUCT_IDREFS: str = "CHANGE_STRUCT_IDREFS";break;
    case CP_FUNC_OP_POST_STRUCT: str = "POST_STRUCT";break;
    case CP_FUNC_OP_UNPOST_STRUCT: str = "UNPOST_STRUCT";break;
    case CP_FUNC_OP_UNPOST_ALL: str = "UNPOST_ALL";break;
    case CP_FUNC_OP_AR_OPEN: str = "AR_OPEN";break;
    case CP_FUNC_OP_AR_CLOSE: str = "AR_CLOSE";break;
    case CP_FUNC_OP_AR_ARCHIVE: str = "AR_ARCHIVE";break;
    case CP_FUNC_OP_AR_RETRIEVE: str = "AR_RETRIEVE";break;
    case CP_FUNC_OP_AR_DELETE: str = "AR_DELETE";break;
    case CP_FUNC_OP_AR_GET_NAMES: str = "AR_GET_NAMES";break;
    case CP_FUNC_OP_AR_GET_HIERARCHY: str = "AR_GET_HIERARCHY";break;
    case CP_FUNC_OP_INP_INIT_DEV: str = "INP_INIT_DEV";break;
    case CP_FUNC_OP_INP_SET_MODE: str = "INP_SET_MODE";break;
    case CP_FUNC_OP_INP_REQUEST: str = "INP_REQUEST";break;
    case CP_FUNC_OP_INP_SAMPLE: str = "INP_SAMPLE";break;
    case CP_FUNC_OP_INP_AWAIT:
		/* print long string of polls by AWAIT EVENT, only if . . . */
		if ( cpr_print_trace & TRACE_AWAIT_RET ) {
		    str = "INP_AWAIT"; break;
		} else
		    return;	/* Don't print for boring AWAIT EVENT ops. */
    case CP_FUNC_OP_FLUSH_DEV: str = "FLUSH_DEV";break;
    case CP_FUNC_OP_SET_ERR_HAND_MODE: str = "SET_ERR_HAND_MODE";break;
    case CP_FUNC_OP_EL_SEARCH: str = "EL_SEARCH";break;
    case CP_FUNC_OP_INC_SPA_SEARCH: str = "INC_SPA_SEARCH";break;

    case CP_FUNC_OP_INQ_STRUCT_STATUS: str = "INQ_STRUCT_STATUS ";break;
    case CP_FUNC_OP_INQ_STRUCT_IDS: str = "INQ_STRUCT_IDS ";break;
    case CP_FUNC_OP_INQ_EL_PTR: str = "INQ_EL_PTR ";break;
    case CP_FUNC_OP_INQ_EL_TYPE_SIZE: str = "INQ_EL_TYPE_SIZE ";break;
    case CP_FUNC_OP_INQ_EL_CONTENT: str = "INQ_EL_CONTENT ";break;

    case CP_FUNC_OP_INQ_CONFLICTING: str = "INQ_CONFLICTING";break;
    case CP_FUNC_OP_INQ_INDICES: str = "INQ_INDICES";break;
    case CP_FUNC_OP_INQ_FILTER: str = "INQ_FILTER";break;
    case CP_FUNC_OP_INQ_POSTED: str = "INQ_POSTED";break;
    case CP_FUNC_OP_INQ_INP_DEV_STATE: str = "INQ_INP_DEV_STATE";break;
    case CP_FUNC_OP_INQ_WSS_POSTED_TO: str = "INQ_WSS_POSTED_TO";break;
    case CP_FUNC_OP_INQ_HIERARCHY: str = "INQ_HIERARCHY";break;
    case CP_FUNC_OP_INQ_REPRESENTATION: str = "INQ_REPRESENTATION";break;
    case CP_FUNC_OP_INQ_VIEW_REP: str = "INQ_VIEW_REP";break;
    case CP_FUNC_OP_INQ_HLHSR_MODE: str = "INQ_HLHSR_MODE";break;
    case CP_FUNC_OP_INQ_DISP_UPDATE_STATE: str = "INQ_DISP_UPDATE_STATE";break;
    case CP_FUNC_OP_INQ_COLOUR_MODEL: str = "INQ_COLOUR_MODEL";break;
    case CP_FUNC_OP_INQ_WS_XFORM: str = "INQ_WS_XFORM";break;
    case CP_FUNC_OP_INQ_INP_OVERFLOW: str = "INQ_INP_OVERFLOW";break;
    case CP_FUNC_OP_INQ_MORE_EVENTS: str = "INQ_MORE_EVENTS";break;
    case CP_FUNC_OP_INQ_TEXT_EXTENT: str = "INQ_TEXT_EXTENT";break;

    case CP_FUNC_OP_EMERG_CLOSE: str = "EMERG_CLOSE";break;
    case CP_FUNC_OP_CLOSE_PHIGS: str = "CLOSE_PHIGS";break;

    default:
	sprintf( buf, "%d\n", (int)op & CP_OPCODE_BITS);
	str = buf;
	break;
    }

    fprintf(stderr, "%s%c", str,
    		(cpr_print_trace & TRACE_SIZES) && ((int)op & CP_OP_PRE_DATA)
    		? ' ' : '\n');

    if ( (cpr_print_trace & TRACE_DATA)  && ((int)op & ~CP_OPCODE_BITS) ) {
	fprintf( stderr, "\t");
	if ( (int)op & CP_OP_PRE_DATA )
	    fprintf( stderr, "PRE_DATA ");
	if ( (int)op & CP_OP_RETURN_DATA )
	    fprintf( stderr, "RETURN_DATA ");
	if ( (int)op & CP_OP_POST_DATA)
	    fprintf( stderr, "POST_DATA ");
	if ( (int)op & CP_OP_FORCE_RETURN)
	    fprintf( stderr, "FORCE_RETURN ");
	if ( (int)op & CP_OP_SYNC)
	    fprintf( stderr, "SYNC");	/* last one needs no trailing space */
	fprintf( stderr, "\n");
    }
}

#endif /* DEBUG */

static int
cpr_exec_cmd( cph, args, ret)
    register Cp_handle		cph;
    register Phg_args		*args;
    register Phg_ret		*ret;
{

#ifdef DEBUG
    if ( cpr_print_trace & TRACE_GENERAL )
	print_op( (Cp_func_op)args->op );
#endif /* DEBUG */

    ret->err = 0;
    if ( (int)args->op & CP_OP_PRE_DATA) {
	cph->data.monitor.cmd_state = CP_EXECUTING_CMD;
	if ( cpr_get_pre_data( cph, args) <= 0)
	    return 0;
	CPR_RESET_CMD_STATE( cph);
    }

    CPR_EXEC_CMD( cph, args, ret);

    if ( (int)args->op & CP_OP_SYNC) {
	int		sync;
	(void)CPR_SEND( cph, &sync, sizeof(sync));
    }

    if ( (int)args->op & CP_OP_RETURN_DATA ) {
	if ( (int)args->op & CP_OP_POST_DATA )
	    cpr_send_post_data( cph, args, ret );

	if ( CPR_SEND_RET(cph, ret) )
	    if ( (int)args->op & CP_OP_RET_DATA_IN_SOCKET )
		cpr_send_post_data( cph, args, ret );
    }

    if ( (int)args->op & CP_OP_DATA_IN_BUFFER )
	cpr_deque_shm_data( cph , args->data_size );

    return 1;
}

static int	timer_expired;

static void
cpr_timeout()
{
    timer_expired = 1;
}

#define CPC_MORE_COMMANDS(_buf) \
    ((_buf)->args.tail != (_buf)->args.head)

#ifndef PEX_API_SOCKET_IPC
int
phg_cpr_rcv_cmd_shm( cph )
    Cp_handle		cph;
{
    int				cmd_done = 0;

    register Cp_data_monitor	*r = &cph->data.monitor;
    register Phg_args		*args;
    register int		force_return = 0;
    register Phg_ret		*ret = &cph->shm_buf->ret.data;

    /* Set up so that this function returns after a short
     * period of time.  This is required so that window system interactions
     * can be processed when the parent is sending a constant stream of
     * commands.
     */
    timer_expired = 0;
    phg_register_timer_func((unsigned long)cph, cpr_timeout, ITIMER_VIRTUAL,
	(unsigned long)(r->cmd_timeout.it_interval.tv_sec * 1000000 +
	r->cmd_timeout.it_interval.tv_usec));

    do {
	if( r->cmd_state == CP_EXECUTING_CMD) {
	    args = &cph->shm_buf->args.buf[cph->shm_buf->args.head];
	    cmd_done = cpr_exec_cmd( cph, args, ret );

	} else if ( r->cmd_state == CP_READING_ARGS ) {
	    CP_CHECK_SHM_LOCK(cph->shm_buf,args)
	    CP_GRAB_SHM_LOCK(cph->shm_buf,args)
	    if ( CPC_MORE_COMMANDS(cph->shm_buf) ) {
		CP_RELEASE_SHM_LOCK(cph->shm_buf,args)
		r->cur_cnt = 0;
		args = &cph->shm_buf->args.buf[cph->shm_buf->args.head];
		cph->erh->cur_func_num = args->funcnum;
		cmd_done = cpr_exec_cmd( cph, args, ret );
	    } else {
		CP_RELEASE_SHM_LOCK(cph->shm_buf,args)
	    }
	}

	if ( cmd_done ) {
	    /* Deque the args. */
	    CP_GRAB_SHM_LOCK(cph->shm_buf,args)
	    ++cph->shm_buf->args.head;
	    /* Reset to use the top of the queue if it's now empty. */
	    if ( cph->shm_buf->args.head == cph->shm_buf->args.tail )
		cph->shm_buf->args.head = cph->shm_buf->args.tail = 0;
	    CP_RELEASE_SHM_LOCK(cph->shm_buf,args)

	    if ( timer_expired )
		force_return = 1;
	}

    } while ( !force_return
	&& (CPC_MORE_COMMANDS(cph->shm_buf) || r->infile->cnt > 0) );

    phg_register_timer_func((unsigned long)cph, (void(*)())NULL,
	ITIMER_VIRTUAL, (unsigned long)0);

#ifdef DEBUG
    if ( cpr_print_trace & TRACE_AWAIT_RET )
	fprintf( stderr, "CP RETURN\n");
#endif /* DEBUG */

    return( force_return ? 1 : 0);
}


#else /* PEX_API_SOCKET_IPC defined */
int
phg_cpr_rcv_cmd_socket( cph )
    Cp_handle		cph;
{
    int				cmd_done = 0;

    register int		wc;
    register Cp_data_monitor	*r = &cph->data.monitor;
    register Phg_args		*args = r->arg_buf;
    register int		force_return = 0;

    /* Set up so that this function returns after a short
     * period of time.  This is so that window system interactions
     * can be processed when the parent is sending a constant stream of
     * commands.  (Note: On single processor machines this probably isn't
     * necessary when using socket IPC between client and monitor.)
     */
    timer_expired = 0;
    phg_register_timer_func((unsigned long)cph, cpr_timeout, ITIMER_VIRTUAL,
	(unsigned long)(r->cmd_timeout.it_interval.tv_sec * 1000000 +
	r->cmd_timeout.it_interval.tv_usec));

    do {
	if( r->cmd_state == CP_EXECUTING_CMD) {
	    cmd_done = cpr_exec_cmd( cph, args, r->ret_buf);

	} else if ( r->cmd_state == CP_READING_ARGS) {
	    if ( (wc = phg_cpr_fread( r->infile,
		((char*)args) + r->cur_cnt,
		sizeof(*args) - r->cur_cnt)) > 0) {

		r->cur_cnt += wc;
		if ( r->cur_cnt == sizeof(*args)) {
		    r->cur_cnt = 0;
		    cph->erh->cur_func_num = args->funcnum;
		    cmd_done = cpr_exec_cmd( cph, args, r->ret_buf);
		}

	    } else if ( wc < 0) {
		perror("phg_cpr_rcv_cmd_socket, catastrophic!");
		break;
	    }
	}

	if ( cmd_done ) {
	    if ( timer_expired )
		force_return = 1;
	}
    } while ( !force_return && r->infile->cnt > 0 );

    phg_register_timer_func((unsigned long)cph, (void(*)())NULL,
	ITIMER_VIRTUAL, (unsigned long)0);

#ifdef DEBUG
    if ( cpr_print_trace & TRACE_AWAIT_RET )
	fprintf( stderr, "CP RETURN\n");
#endif /* DEBUG */

    return( force_return ? 1 : 0);
}
#endif /* ! PEX_API_SOCKET_IPC defined */
