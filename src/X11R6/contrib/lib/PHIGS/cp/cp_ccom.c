/* $XConsortium: cp_ccom.c,v 5.20 94/04/17 20:41:10 eswu Exp $ */

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

/* Open and close client CPs */

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include <sys/socket.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#ifndef PEX_API_SOCKET_IPC
#include <sys/ipc.h>
#include <sys/shm.h>
#endif /* ! PEX_API_SOCKET_IPC */
#ifdef AIXV3
#include <sys/ioctl.h>
#endif

#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <sys/wait.h>
#else
#define _POSIX_SOURCE
#include <sys/wait.h>
#undef _POSIX_SOURCE
#endif
# define waitSig(w)	WTERMSIG(w)
typedef int		waitType;
#else
#ifdef SYSV
#ifdef SYSV386
#ifdef ISC
typedef short pid_t;
#endif
# include <sys/wait.h>
#endif
# define waitSig(w)	((w) & 0xff)
typedef int		waitType;
#else
# include	<sys/wait.h>
# define waitSig(w)	((w).w_T.w_Termsig)
typedef union wait	waitType;
#endif
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif

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

#define CPC_SEND( cph, msg, size) \
    phg_cpxc_send( (cph)->data.client.sfd, (char*)(msg), (size))

#define CPC_RECV( cph, msg, size) \
    phg_cpxc_recv( (cph)->data.client.sfd, (char*)(msg), (size))

#ifndef PEX_API_SOCKET_IPC
/* Shared memory is the default. */
#define CPC_SEND_ARGS( cph, args) \
    cpc_enque_args((cph), (args))

#define CPC_RECV_RET( cph, ret) \
    cpc_rcv_ret_data( (cph), (ret) )

#else /* PEX_API_SOCKET_IPC defined */
#define CPC_SEND_ARGS( cph, args) \
    (phg_cpxc_send( (cph)->data.client.sfd, (char*)(args), sizeof(Phg_args)) \
	== sizeof(Phg_args) ?  1 : 0)

#define CPC_RECV_RET( cph, ret) \
    (phg_cpxc_recv( (cph)->data.client.sfd, (char*)(ret), sizeof(Phg_ret)) \
	== sizeof(Phg_ret) ?  1 : 0)
#endif /* !PEX_API_SOCKET_IPC defined */

#define CPC_GET_ARGS_BUF( _cph ) \
    ((_cph)->shm_buf->args.tail < (_cph)->shm_buf->args.buf_size \
	? (_cph)->shm_buf->args.tail++ : -1)

static int
wait_for_write(s)
    int s;
{
    unsigned long	wfd[MSKCNT];

    CLEARBITS(wfd);
    BITSET(wfd, s);
    if (select(s+1, NULL, wfd, NULL, (struct timeval *)NULL) < 0 &&
	errno != EINTR){
	perror("phg_cpxc_send(select)");
	return 0;
    }
    return 1;
}

int
phg_cpxc_send( s, msg, size)
    int			s;
    register char	*msg;
    register int	size;
{
    register int	c, wc, todo;

    c = 0;
    todo = size;
    while ( todo) {
	if ( (wc = write( s, msg + c, todo)) >= 0) {
	    c += wc;
	    todo = size - c;
	} else if (ETEST(errno)) {
		if (!wait_for_write(s))
		    break;
	} else if (errno == EINTR) {
		continue;
#ifdef EMSGSIZE
	} else if (errno == EMSGSIZE) {
	    if (todo > 1)
		todo >>= 1;
	    else if (!wait_for_write(s))
		break;
#endif
	} else {
		perror("phg_cpxc_send(write)");
		break;
	}
    }
    return c;
}


static int
cpc_enque_args( cph, args )
    Cp_handle	cph;
    Phg_args	*args;
{
    int		buf_pos;

    /* Get a free spot in the args queue and copy the args to it. */
    do {
	CP_GRAB_SHM_LOCK(cph->shm_buf,args)
	if ( (buf_pos = CPC_GET_ARGS_BUF(cph)) < 0 ) {
	    CP_RELEASE_SHM_LOCK(cph->shm_buf,args)
	    CP_MICROSLEEP(10000)	/* no room yet, wait a short while */
	} else {
	    bcopy( (char *)args, (char *)&cph->shm_buf->args.buf[buf_pos],
		sizeof(*args) );
	    CP_RELEASE_SHM_LOCK(cph->shm_buf,args)
	}
    } while ( buf_pos < 0 );

    return 1;
}

int
phg_cpxc_recv( s, msg, size)
    int			s;
    register char	*msg;
    register int	size;
{
    register int	c, wc;

    c = 0;
    while ( c < size) {
	if ( (wc = read( s, msg + c, size - c)) > 0) {
	    c += wc;
	} else if ( wc < 0) {
	    if (ETEST(errno))
		break;
	    else if ( errno == EINTR)
		continue;
	    else {
		perror("phg_cpxc_recv(read)");
		break;
	    }
	}
    }
    return c;
}


static int
inp_init_send( cph, args, sv)
    Cp_handle		cph;
    Phg_args		*args;
    Cp_send_vec		sv[];
{
    int			vcount = 0;

    switch ( args->data.inp_init_dev.class ) {
	case PHG_ARGS_INP_STK3:
	case PHG_ARGS_INP_STK: {
	    Pstroke3 *stk = &args->data.inp_init_dev.data.stk.init;
	    if ( stk->num_points > 0 ) {
		vcount = 1;
		sv->size = stk->num_points * sizeof(Ppoint3);
		sv->data = (char*)stk->points;
	    }
	} break;
	case PHG_ARGS_INP_PIK3:
	case PHG_ARGS_INP_PIK:{
	    Ppick *pick = &args->data.inp_init_dev.data.pik.init;
	    if ( pick->status == PIN_STATUS_OK && pick->pick_path.depth > 0 ) {
		vcount = 1;
		sv->size = pick->pick_path.depth * sizeof(Ppick_path_elem);
		sv->data = (char*)pick->pick_path.path_list;
	    }
	} break;
	case PHG_ARGS_INP_CHC3:
	case PHG_ARGS_INP_CHC: {
	    Pchoice_data	*rec = &args->data.inp_init_dev.data.cho.rec;
	    switch ( args->data.inp_init_dev.pet ) {
		case 2:
		    if ( rec->pets.pet_r2.num_prompts > 0 ) {
			vcount = 1;
			sv->size = rec->pets.pet_r2.num_prompts
			    * sizeof(Ppr_switch);
			sv->data = (char*)rec->pets.pet_r2.prompts;
		    }
		    break;
		case 3:
		  if ( args->data.inp_init_dev.data.cho.string_list_size > 0) {
		      vcount = 1;
		      sv->size =
			  args->data.inp_init_dev.data.cho.string_list_size;
		      sv->data = (char*)rec->pets.pet_r3.strings;
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
	case PHG_ARGS_INP_STR3:
	case PHG_ARGS_INP_STR:
	    if ( args->data.inp_init_dev.data.str.init.length > 0 ) {
		vcount = 1;
		sv->size = args->data.inp_init_dev.data.str.init.length;
		sv->data = args->data.inp_init_dev.data.str.init.string;
	    }
	    break;
	case PHG_ARGS_INP_VAL3:
	case PHG_ARGS_INP_VAL:
	    switch ( args->data.inp_init_dev.pet ) {
	      case -1: {
		Phg_args_init_data	*data = &args->data.inp_init_dev.data;
		if ( data->val.counts[0] > 0 ) {
		    sv[vcount].size = data->val.counts[0];
		    sv[vcount].data = data->val.rec.pets.pet_u1.label;
		    ++vcount;
		}
		if ( data->val.counts[1] > 0 ) {
		    sv[vcount].size = data->val.counts[1];
		    sv[vcount].data = data->val.rec.pets.pet_u1.format;
		    ++vcount;
		}
		if ( data->val.counts[2] > 0 ) {
		    sv[vcount].size = data->val.counts[2];
		    sv[vcount].data = data->val.rec.pets.pet_u1.low_label;
		    ++vcount;
		}
		if ( data->val.counts[3] > 0 ) {
		    sv[vcount].size = data->val.counts[3];
		    sv[vcount].data = data->val.rec.pets.pet_u1.high_label;
		    ++vcount;
		}
	      } break;
	    }
	    break;
    }
    return vcount;
}

static int
cpc_get_data_buf( cph, size )
    Cp_handle	cph;
    int		size;
{
    int		bufpos, dsize;

    dsize = CP_DATA_BUF_SIZE_NEEDED(cph,size);
    if ( cph->shm_buf->data.tail + dsize < CP_SHM_DATA_BUF_SIZE ) {
	bufpos = cph->shm_buf->data.tail;
	cph->shm_buf->data.tail += dsize;
    } else
	bufpos = -1;

    return bufpos;
}


static void
cpc_enque_shm_data( cph, vcount, sv, size )
    Cp_handle			cph;
    int				vcount;
    register Cp_send_vec	*sv;
    int				size;
{
    int		bufpos;
    char	*buf;

    register int	i;

    /* TODO: Ensure alignment of all data blocks, not just the first one. */
    do {
	CP_GRAB_SHM_LOCK(cph->shm_buf,data)
	if ((bufpos = cpc_get_data_buf(cph,size)) < 0) {
	    CP_RELEASE_SHM_LOCK(cph->shm_buf,data)
	    CP_MICROSLEEP(10000) /* no room yet, wait a short while */
	} else {
	    buf = (char *)&cph->shm_buf->data.buf[bufpos];
	    for ( i = 0; i < vcount; i++ ) {
		bcopy( sv[i].data, buf, sv[i].size );
		buf += sv[i].size;
	    }
	    CP_RELEASE_SHM_LOCK(cph->shm_buf,data)
	}
    } while ( bufpos < 0 );
}

static int
cpc_send_pre_data( cph, args)
    Cp_handle		cph;
    register Phg_args	*args;
{
    Cp_send_vec			svec[CP_MAX_COM_PTRS];
    register Cp_send_vec	*sv = svec;
    int				vcount = 0, status = 1;

    switch ( (int)args->op & CP_OPCODE_BITS) {
	case CP_FUNC_OP_ADD_EL:
	    vcount = 1;
	    sv->size = args->data.add_el.pex_oc.size;
	    sv->data = (char *)args->data.add_el.pex_oc.oc;
	    break;
	case CP_FUNC_OP_MESSAGE:
	    vcount = 1;
	    sv->size = args->data.message.msg_length;
	    sv->data = args->data.message.msg;
	    break;
	case CP_FUNC_OP_AR_ARCHIVE:
	case CP_FUNC_OP_AR_RETRIEVE:
	case CP_FUNC_OP_AR_DELETE:
	    if ( args->data.ar_info.op != PHG_ARGS_AR_ALL) {
		vcount = 1;
	        sv->size = args->data.ar_info.data.num_ints * sizeof(Pint);
	        sv->data = (char*)args->data.ar_info.data.ints;
	    }
	    break;
	case CP_FUNC_OP_SET_FILTER:
	    vcount = 2;
	    sv->size = args->data.set_filter.inc_set.num_ints * sizeof(Pint);
	    sv->data = (char*)args->data.set_filter.inc_set.ints;
	    sv[1].size = args->data.set_filter.exc_set.num_ints * sizeof(Pint);
	    sv[1].data = (char*)args->data.set_filter.exc_set.ints;
	    break;
	case CP_FUNC_OP_AR_OPEN:
	    vcount = 1;
	    sv->size = args->data.ar_open.name_length;
	    sv->data = args->data.ar_open.fname;
	    break;
	case CP_FUNC_OP_OPEN_WS:
	    vcount = 4;
	    sv[0].size = sizeof(Wst);
	    sv[0].data = (char *)args->data.open_ws.type;
	    sv[1].size = args->data.open_ws.wst_buffer_size;
	    sv[1].data = (char *)args->data.open_ws.type->buffer;
	    sv[2].size = args->data.open_ws.conn_info.display_name_length;
	    sv[2].data = args->data.open_ws.conn_info.display_name;
	    sv[3].size = args->data.open_ws.wst_display_name_length;
	    sv[3].data = args->data.open_ws.type->desc_tbl.xwin_dt.display_name;
	    break;
	case CP_FUNC_OP_INQ_TEXT_EXTENT:
	    vcount = 1;
	    sv->size = args->data.q_text_extent.length;
	    sv->data = args->data.q_text_extent.str;
	    break;
	case CP_FUNC_OP_INP_INIT_DEV:
	    vcount = inp_init_send( cph, args, sv);
	    break;
	case CP_FUNC_OP_EL_SEARCH:
	    vcount = 2;
	    sv[0].size = args->data.el_search.incl.num_elem_types
		* sizeof(Pelem_type);
	    sv[0].data = (char*)args->data.el_search.incl.elem_types;
	    sv[1].size = args->data.el_search.excl.num_elem_types
		* sizeof(Pelem_type);
	    sv[1].data = (char*)args->data.el_search.excl.elem_types;
	    break;
	case CP_FUNC_OP_INC_SPA_SEARCH:
	    vcount = 5;
	    sv[0].size = args->data.inc_spa_search.start_path.num_elem_refs
		* sizeof(Pint);
	    sv[0].data = (char *)
		args->data.inc_spa_search.start_path.elem_refs;
	    sv[1].size = args->data.inc_spa_search.nrm_filt.incl_set.num_ints
		* sizeof(Pint);
	    sv[1].data = (char *)
		args->data.inc_spa_search.nrm_filt.incl_set.ints;
	    sv[2].size = args->data.inc_spa_search.nrm_filt.excl_set.num_ints
		* sizeof(Pint);
	    sv[2].data = (char *)
		args->data.inc_spa_search.nrm_filt.excl_set.ints;
	    sv[3].size = args->data.inc_spa_search.inv_filt.incl_set.num_ints
		* sizeof(Pint);
	    sv[3].data = (char *)
		args->data.inc_spa_search.inv_filt.incl_set.ints;
	    sv[4].size = args->data.inc_spa_search.inv_filt.excl_set.num_ints
		* sizeof(Pint);
	    sv[4].data = (char *)
		args->data.inc_spa_search.inv_filt.excl_set.ints;
	    break;
	case CP_FUNC_OP_SET_REP:
	    switch ( args->data.set_rep.type ) {
		case PHG_ARGS_PTREP:
		    vcount = 1;
		    sv[0].size =
			  args->data.set_rep.rep.bundl.ptrep.dims.size_x
		        * args->data.set_rep.rep.bundl.ptrep.dims.size_y
			* sizeof(Pint);
		    sv[0].data = (char *)
			args->data.set_rep.rep.bundl.ptrep.colr_array;
		    break;
		case PHG_ARGS_EXTPTREP:
		    vcount = 1;
		    sv[0].size =
			  args->data.set_rep.rep.bundl.extptrep.dims.size_x
		        * args->data.set_rep.rep.bundl.extptrep.dims.size_y
			* sizeof(Pcoval);
		    sv[0].data = (char *)
			args->data.set_rep.rep.bundl.extptrep.colr_array;
		    break;
	    }
	    break;
	case CP_FUNC_OP_DRAWABLE_PICK:
	    vcount = 2;
	    sv[0].size = args->data.drawable_pick.filter.incl_set.num_ints
		* sizeof(Pint);
	    sv[0].data = (char*)args->data.drawable_pick.filter.incl_set.ints;
	    sv[1].size = args->data.drawable_pick.filter.excl_set.num_ints
		* sizeof(Pint);
	    sv[1].data = (char*)args->data.drawable_pick.filter.excl_set.ints;
	    break;
	case CP_FUNC_OP_MAP_POINTS:
	    vcount = 1;
	    sv[0].size = args->data.map_points.points.num_points
		* sizeof(args->data.map_points.points.points[0]);
	    sv[0].data = (char*)args->data.map_points.points.points;
	    break;
	case CP_FUNC_OP_REDRAW_REGIONS:
	    vcount = 1;
	    sv[0].size = args->data.redraw_regions.num_regions
		* sizeof(args->data.redraw_regions.regions[0]);
	    sv[0].data = (char*)args->data.redraw_regions.regions;
	    break;
    }

    if ( vcount > 0) {
	register int	i;

	/* Determine total size of data. */
	args->data_size = 0;
	for ( i = 0; i < vcount; i++)
	    args->data_size += sv[i].size;

	/* Copy or send the data. */
	if ( args->data_size > cph->shm_buf->data.buf_size ) {
	    /* Data won't fit in buffer, have to use the socket. */
	    if ( (int)args->op & CP_OP_DATA_FOLLOWS_ARGS ) {
		for ( i = 0; i < vcount; i++)
		    if ( CPC_SEND(cph,sv[i].data,sv[i].size) != sv[i].size) {
			status = 0;
			break; /* out of for loop */
		    }
	    } else {
		/* Do it on the next call. */
		args->op |= CP_OP_DATA_FOLLOWS_ARGS;
	    }

	} else if ( args->data_size > 0 ) {
	    /* Copy data to the shared memory buffer. */
	    args->op |= CP_OP_DATA_IN_BUFFER;
	    cpc_enque_shm_data( cph, vcount, sv, (int)args->data_size );
	}
    }

    return status;
}


static int
setup_event_recv( event, rv)
    Phg_ret_inp_event		*event;
    register Cp_recv_vec	*rv;
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
		rv->size = event->data.stk.num_points * sizeof(Ppoint3);
		rv->dpp = (char**)&event->data.stk.points;
	    }
	    break;
	case PIN_PICK:
	    if ( event->data.pik.status == PIN_STATUS_OK
		&& event->data.pik.pick_path.depth > 0 ) {
		vcount = 1;
		rv->size = event->data.pik.pick_path.depth*sizeof(Ppick_path_elem);
		rv->dpp = (char**)&event->data.pik.pick_path.path_list; }
	    break;
	case PIN_STRING:
	    if ( event->data.str.length > 0 ) {
		vcount = 1;
		rv->size = event->data.str.length;
		rv->dpp = (char**)&event->data.str.string;
	    }
	    break;
    }
    return vcount;
}

static int
setup_inp_state_recv( class, ret, rv )
    Phg_args_idev_class		class;
    Phg_ret_inp_state		*ret;
    Cp_recv_vec			*rv;
{
    int		vcount = 0;

    switch ( class ) {
	case PHG_ARGS_INP_STK3:
	case PHG_ARGS_INP_STK: {
	    Pstrokest3	*stkst = &ret->stroke;
	    if ( stkst->stroke.num_points > 0 ) {
		vcount = 1;
		rv->size =  stkst->stroke.num_points * sizeof(Ppoint3);
		rv->dpp = (char**)&stkst->stroke.points;
	    }
	} break;
	case PHG_ARGS_INP_PIK3:
	case PHG_ARGS_INP_PIK:{
	    Ppickst3	*pickst = &ret->pick;
	    if ( pickst->pick.status == PIN_STATUS_OK ) {
		++vcount;
		rv->size = pickst->pick.pick_path.depth * sizeof(Ppick_path_elem);
		rv->dpp = (char**)&pickst->pick.pick_path.path_list;
		++rv;
	    }
	    if ( pickst->inclusion_filter.num_ints > 0 ) {
		++vcount;
		rv->size = pickst->inclusion_filter.num_ints * sizeof(Pint);
		rv->dpp = (char**)&pickst->inclusion_filter.ints;
		++rv;
	    }
	    if ( pickst->exclusion_filter.num_ints > 0 ) {
		++vcount;
		rv->size = pickst->exclusion_filter.num_ints * sizeof(Pint);
		rv->dpp = (char**)&pickst->exclusion_filter.ints;
	    }
	} break;
	case PHG_ARGS_INP_STR3:
	case PHG_ARGS_INP_STR: {
	    Phg_ret_string_state	*strst = &ret->string;
	    if ( strst->length > 0 ) {
		vcount = 1;
		rv->size =  strst->length;
		rv->dpp = (char**)&strst->state.string;
	    }
	} break;
	case PHG_ARGS_INP_VAL3:
	case PHG_ARGS_INP_VAL:
	    switch ( ret->val.pet ) {
	      case -1:
		if ( ret->val.counts[0] > 0 ) {
		    rv[vcount].size = ret->val.counts[0];
		    rv[vcount].dpp =
			(char **)&ret->val.record.pets.pet_u1.label;
		    ++vcount;
		}
		if ( ret->val.counts[1] > 0 ) {
		    rv[vcount].size = ret->val.counts[1];
		    rv[vcount].dpp =
			(char **)&ret->val.record.pets.pet_u1.format;
		    ++vcount;
		}
		if ( ret->val.counts[2] > 0 ) {
		    rv[vcount].size = ret->val.counts[2];
		    rv[vcount].dpp =
			(char **)&ret->val.record.pets.pet_u1.low_label;
		    ++vcount;
		}
		if ( ret->val.counts[3] > 0 ) {
		    rv[vcount].size = ret->val.counts[3];
		    rv[vcount].dpp =
			(char **)&ret->val.record.pets.pet_u1.high_label;
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
			rv->size = rec->pets.pet_r2.num_prompts
			    * sizeof(Ppr_switch);
			rv->dpp = (char**)&rec->pets.pet_r2.prompts;
		    }
		    break;
		case 3:
		    if ( chst->length > 0 ) {
			vcount = 1;
			rv->size =  chst->length;
			rv->dpp = (char**)&chst->strings;
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
    }

    return vcount;
}

static int
cpc_get_post_data( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    Cp_recv_vec			rvec[CP_MAX_COM_PTRS];
    register Cp_recv_vec	*rv = rvec;
    int				vcount = 0, status = 1;

    switch ( (int)args->op & CP_OPCODE_BITS) {
	case CP_FUNC_OP_AR_GET_NAMES:
	case CP_FUNC_OP_INQ_STRUCT_IDS:
	case CP_FUNC_OP_INQ_INDICES:
	case CP_FUNC_OP_INQ_CONFLICTING:
	case CP_FUNC_OP_INQ_WSS_POSTED_TO:
	    if ( !ret->err) {
		vcount = 1;
		rv->size = ret->data.int_list.num_ints * sizeof(Pint);
		rv->dpp = (char**)&ret->data.int_list.ints;
	    }
	    break;
	case CP_FUNC_OP_OPEN_WS:
	    if ( !ret->err) {
		vcount = 2;
		rv[0].size = sizeof(*ret->data.open_ws.wstype);
		rv[0].dpp = (char**)&ret->data.open_ws.wstype;
		rv[1].size = ret->data.open_ws.wst_buffer_size;
		rv[1].dpp = (char**)&ret->data.open_ws.wst_buffer;
	    }
	    break;
	case CP_FUNC_OP_INQ_EL_CONTENT:
	    if ( !ret->err ) {
		vcount = 1;
		rv->size = ret->data.el_info.pex_oc.size;
		rv->dpp = (char **)&ret->data.el_info.pex_oc.oc;
	    }
	    break;
	case CP_FUNC_OP_INQ_HIERARCHY:
	case CP_FUNC_OP_AR_GET_HIERARCHY:
	    if ( !ret->err) {
		vcount = 2;
		rv->size = ret->data.hierarchy.counts.num_ints * sizeof(Pint);
		rv->dpp = (char**)&ret->data.hierarchy.counts.ints;
		rv[1].size = ret->data.hierarchy.num_pairs * sizeof(Pelem_ref);
		rv[1].dpp = (char**)&ret->data.hierarchy.paths;
	    }
	    break;
	case CP_FUNC_OP_INQ_POSTED:
	    if ( !ret->err) {
		vcount = 1;
		rv->size = ret->data.postlist.num_postings * sizeof(Pposted_struct);
		rv->dpp = (char**)&ret->data.postlist.postings;
	    }
	    break;
	case CP_FUNC_OP_INQ_VIEW_REP:
	    if ( !ret->err) {
		vcount = 2;
		rv->size = sizeof(Pview_rep3);
		rv->dpp = (char**)&ret->data.view_rep.cur_rep;
		rv[1].size = sizeof(Pview_rep3);
		rv[1].dpp = (char**)&ret->data.view_rep.req_rep;
	    }
	    break;
	case CP_FUNC_OP_INP_AWAIT:
	case CP_FUNC_OP_INP_SAMPLE:
	    if ( !ret->err)
		vcount = setup_event_recv( &ret->data.inp_event, rv);
	    break;
	case CP_FUNC_OP_INP_REQUEST:
	    if ( !ret->err && !ret->data.inp_request.brake)
		vcount = setup_event_recv( &ret->data.inp_request.event, rv);
	    break;
	case CP_FUNC_OP_INQ_INP_DEV_STATE:
	    if ( !ret->err )
		vcount = setup_inp_state_recv( args->data.q_inp_state.class,
		    &ret->data.inp_state, rv);
	    break;
	case CP_FUNC_OP_INQ_FILTER:
	    if ( !ret->err ) {
		if ( ret->data.filter.incl.num_ints > 0 ) {
		    rv->size = ret->data.filter.incl.num_ints * sizeof(Pint);
		    rv->dpp = (char**)&ret->data.filter.incl.ints;
		    vcount = 1;
		}
		if ( ret->data.filter.excl.num_ints > 0 ) {
		    rv[vcount].size =
			ret->data.filter.excl.num_ints * sizeof(Pint);
		    rv[vcount].dpp = (char**)&ret->data.filter.excl.ints;
		    ++vcount;
		}
	    }
	    break;
	case CP_FUNC_OP_INC_SPA_SEARCH:
            if ( !ret->err ) {
                vcount = 1; 
                rv->size = ret->data.inc_spa_search.path.num_elem_refs
		    * sizeof(Pelem_ref);
                rv->dpp = (char**)&ret->data.inc_spa_search.path.elem_refs; 
            }
	    break;
	case CP_FUNC_OP_INQ_REPRESENTATION:
	    switch ( args->data.q_rep.rep_type ) {
		case PHG_ARGS_EXTPTREP:
		    vcount = 1;
		    rv[0].size = ret->data.rep.extptrep.dims.size_x
		        * ret->data.rep.extptrep.dims.size_y * sizeof(Pcoval);
		    rv[0].dpp = (char **) &ret->data.rep.extptrep.colr_array;
		    break;
	    }
	    break;
	case CP_FUNC_OP_DRAWABLE_PICK:
	    if ( ret->data.drawable_pick.status == PIN_STATUS_OK ) {
		vcount = 1;
		rv[0].size = ret->data.drawable_pick.pick.depth
		    * sizeof(Ppick_path_elem);
		rv[0].dpp = (char**)&ret->data.drawable_pick.pick.path_list;
	    }
	    break;
	case CP_FUNC_OP_MAP_POINTS:
	    vcount = 1;
	    rv[0].size = ret->data.map_points.points.num_points
		* sizeof(ret->data.map_points.points.points[0]);
	    rv[0].dpp = (char**)&ret->data.map_points.points.points;
    }

    if ( vcount > 0) {
	register int	i, size;
	register char	*p = NULL;

	for ( i = 0, size = 0; i < vcount; i++)
	    size += rv[i].size;

	if ( size > cph->shm_buf->ret.buf_size ) {
	    /* Not enough space in buffer, use the socket. */
	    if ( !PHG_SCRATCH_SPACE(&cph->scratch, size)) {
		ERR_BUF( cph->erh, ERR900);
		status = 0;
	    } else if ( CPC_RECV( cph, cph->scratch.buf, size) != size) {
		status = 0;
	    } else
		p = cph->scratch.buf;

	} else {
	    p = (char *)cph->shm_buf->ret.buf;
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
cpc_rcv_ret_data( cph, ret )
    Cp_handle		cph;
    Phg_ret		*ret;
{
    int		err, status;

    /* All the data is in shared memory.  ret->err, however, has also been
     * written to the socket to synchronize the processes: this client
     * process blocks on the read until all the data has been put in the
     * buffer by the monitor.
     */
    if ( phg_cpxc_recv( cph->data.client.sfd, (char *)&err,
	    sizeof(err) ) == sizeof(err) ) {
	bcopy( (char *)&cph->shm_buf->ret.data, (char *)ret, sizeof(*ret) );
	status = 1;
    } else
	status = 0;

    return status;
}

static void
cpc_exec_cmd( cph, args, ret )
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    args->funcnum = cph->erh->cur_func_num;
    if ( !cph->flags.monitor_active )
	return;

    if ( (int)args->op & CP_OP_PRE_DATA ) {
	if ( !cpc_send_pre_data( cph, args)) {
	    if ( ret)
		ret->err = ERRN50;
	    ERR_REPORT( cph->erh, ERRN50);
	    return;
	}
    }

    if ( !CPC_SEND_ARGS( cph, args)) {
	if ( ret)
	    ret->err = ERRN50;
	ERR_REPORT( cph->erh, ERRN50);
	return;
    }

    if ( (int)args->op & CP_OP_DATA_FOLLOWS_ARGS ) {
	if ( !cpc_send_pre_data( cph, args)) {
	    if ( ret)
		ret->err = ERRN50;
	    ERR_REPORT( cph->erh, ERRN50);
	    return;
	}
    }

    if ( (int)args->op & CP_OP_SYNC) {
	int	sync;
	if ( CPC_RECV( cph, &sync, sizeof(sync)) != sizeof(sync)) {
	    ERR_REPORT( cph->erh, ERRN50);
	    return;
	} else
	    ERR_FLUSH( cph->erh);
	    /* errors detected between here and the application are handled
	     * immediately
	     */
    }

    if ( (int)args->op & CP_OP_RETURN_DATA) {
	if ( !CPC_RECV_RET( cph, ret)) {
	    ERR_REPORT( cph->erh, ret->err = ERRN50);
	    return;
	}
    }

    if ( (int)args->op & CP_OP_POST_DATA) {
	if ( !cpc_get_post_data( cph, args, ret)) {
	    ERR_REPORT( cph->erh, ret->err = ERRN50);
	    return;
	}
    }
}


void
phg_cpc_class_B( cph, args)
    Cp_handle		cph;
    Phg_args		*args;
{
    /* Only sends command and fixed size data. */
    if ( cph->flags.err_sync )
	args->op |= CP_OP_SYNC;
    cpc_exec_cmd( cph, args, (Phg_ret *)NULL);
}

void
phg_cpc_class_C( cph, args)
    Cp_handle		cph;
    Phg_args		*args;
{
    /* Sends command and variable size data. */
    args->op |= CP_OP_PRE_DATA;
    if ( cph->flags.err_sync )
	args->op |= CP_OP_SYNC;
    cpc_exec_cmd( cph, args, (Phg_ret *)NULL);
}

void
phg_cpc_class_D( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    /* Sends command and fixed size data, receives fixed size return data.  */
    args->op |= CP_OP_RETURN_DATA;
    cpc_exec_cmd( cph, args, ret);
}

void
phg_cpc_class_E( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    /* Sends command and fixed size data, receives variable size return
     * data.
     */
    args->op |= CP_OP_RETURN_DATA | CP_OP_POST_DATA;
    cpc_exec_cmd( cph, args, ret);
}

void
phg_cpc_class_CD( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    /* Sends variable size data and receives fixed size reply. */
    args->op |= CP_OP_PRE_DATA | CP_OP_RETURN_DATA;
    cpc_exec_cmd( cph, args, ret);
}

void
phg_cpc_class_CE( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    /* Sends variable size data and receives variable size reply. */
    args->op |= CP_OP_PRE_DATA | CP_OP_RETURN_DATA | CP_OP_POST_DATA;
    cpc_exec_cmd( cph, args, ret);
}

static void
cpc_rebuild_wst( args, retd )
    Phg_args_open_ws	*args;
    Phg_ret_open_ws	*retd;
{
    Wst		old_wst, *wst = retd->wstype;

    bcopy( (char *)wst, (char *)&old_wst, sizeof(Wst) );
    wst->buffer = retd->wst_buffer;
    phg_wst_copy_buf_pointers( &old_wst, wst );
    wst->desc_tbl.xwin_dt.display_name = args->wst_display_name;
}


void
phg_cpc_class_SPECIAL( cph, args, ret )
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    void	(*class_func)();

    /* Perform any pre-exec operations. */
    switch ( (int)args->op & CP_OPCODE_BITS ) {
	case CP_FUNC_OP_CLOSE_WS:
	    /* Force a sync so that resources in the monitor are destroyed
	     * before their parents are destroyed in this process.
	     */
	    args->op |= CP_OP_SYNC;
	    class_func = phg_cpc_class_B;
	    break;
	case CP_FUNC_OP_OPEN_WS:
	    class_func = phg_cpc_class_CE;
	    break;
	case CP_FUNC_OP_WS_SYNCH:
	    args->op |= CP_OP_SYNC;
	    class_func = phg_cpc_class_B;
	    break;
    }

    (*class_func)( cph, args, ret );

    /* Perform any post-exec operations. */
    switch ( (int)args->op & CP_OPCODE_BITS ) {
	case CP_FUNC_OP_OPEN_WS:
	    if ( !ret->err )
		cpc_rebuild_wst( &args->data.open_ws, &ret->data.open_ws );
	    break;
    }
}

#define FLOAT_TIME( _t) \
    ((_t).tv_sec + (_t).tv_usec * 1.0e-6)

void
phg_cpc_await_event( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    Pevent		*id = &ret->data.inp_event.id;
    register int	timed_out = 0;
    register double	end_time;
    struct timeval	cur_time;

    args->op |= CP_OP_RETURN_DATA | CP_OP_POST_DATA;
    if ( cph->flags.err_sync )
	args->op |= CP_OP_SYNC;

    cur_time.tv_sec = 0;
    for ( cpc_exec_cmd( cph, args, ret);
	id->class == PIN_NONE && !timed_out;
	cpc_exec_cmd( cph, args, ret)) {

        if ( cur_time.tv_sec == 0) {  /* first pass */
            if ( args->data.fdata <= 0.0) {
                timed_out = !0;
            } else {
                /* Calculate when we should return. */
                gettimeofday( &cur_time, (struct timezone *)NULL);
                end_time = FLOAT_TIME( cur_time) + args->data.fdata;
            }
        } else {    /* spin our wheels for a while */
            CP_MICROSLEEP(10000)
            gettimeofday( &cur_time, (struct timezone *)NULL);
            if ( FLOAT_TIME(cur_time) >= end_time)
                timed_out = !0;
        }
    }    
}

void
phg_cpc_inp_request( cph, args, ret)
    Cp_handle		cph;
    Phg_args		*args;
    Phg_ret		*ret;
{
    /* Get the return code to be sure the request got set up, then wait for
     * the requested event to be delivered.
     */
    args->op |= CP_OP_RETURN_DATA;
    if ( cph->flags.err_sync )
	args->op |= CP_OP_SYNC;
    cpc_exec_cmd( cph, args, ret);

    if ( !ret->err ) {
	if ( !CPC_RECV_RET( cph, ret)) {
	    ERR_REPORT( cph->erh, ret->err = ERRN50);

	} else if ( !cpc_get_post_data( cph, args, ret)) {
	    ERR_REPORT( cph->erh, ret->err = ERRN50);
	}
    }
}

#if !defined(X_NOT_POSIX) || (defined(SYSV) && defined(SYSV386))
#define WAIT_FOR_IT(p,s,o) waitpid(p,s,o)
#else
#define WAIT_FOR_IT(p,s,o) wait3(s, o, (struct rusage *) 0)
#endif

static void
sig_wait( child_pid, sig, when)
    int			child_pid;
    int			sig;
    int			when;
{
    /* This SIGCHLD handler is used while the child is being created and
     * while it is being shut down.
     */

    int			pid;
    waitType		status;

    while ( (pid = WAIT_FOR_IT ( child_pid, &status, WNOHANG)) > 0) {
	if ( pid == child_pid) {
	    phg_register_signal_func((unsigned long)child_pid,
		(void(*)())NULL, sig);
	    break;
	}
    }
}


static void
sig_chld_proc( cph, sig, when)
    Cp_handle		cph;
    int			sig;
    int			when;
{
    /* This SIGCHLD handler used ONLY after the child is installed and
     * communicating okay.
     */

    int			pid;
    waitType		status;

    while ( (pid = WAIT_FOR_IT ( cph->data.client.child_pid, &status, WNOHANG)) > 0) {
	if ( pid == cph->data.client.child_pid) {
	    if ( WIFEXITED(status)) {
		/* user hit a frame menu quit button, kill the application */
		exit(3);
	    } else if ( WIFSIGNALED(status)) {
		fprintf( stderr, "PEX API: child died on signal %d\n",
		    waitSig(status));
		 exit(2);
	    }
	}
    }
}


static void
sig_proc( cph, sig, when)	/* for debug only */
    Cp_handle		cph;
    int			sig;
    int			when;
{
    (void)fprintf( stderr, "sig_proc called with sig %d\n", sig);
}


static void
install_signal_funcs( cph)
    Cp_handle	cph;
{
    /* set SIGCHLD catcher *after* successful exec so that it's not called
     * because of an error in the monitor startup.
     */
    phg_register_signal_func( (unsigned long)cph->data.client.child_pid, 
	(void(*)())NULL, SIGCHLD);
    phg_register_signal_func( (unsigned long)cph, sig_chld_proc, SIGCHLD);
}

#ifdef	DEBUG
static void
set_debug_signal_funcs( cph)	/* for debug only */
    Cp_handle	cph;
{
    int i;

    for ( i = 1; i < 32; i ++) {
	switch ( i) {
	    case SIGKILL:
#ifndef AIXrt
	    case SIGSTOP:
#endif
	    case SIGCHLD:
		break;

	    default:
		phg_register_signal_func((unsigned long)cph, sig_proc, i);
		break;
	}
    }
}
#endif	/* DEBUG */

/* Wait for the child to get going */
#define	NORMAL_WAIT_FOR_CHILD		60		/* seconds */

static int
handshake_child( cph, fd, pid )
    Cp_handle	cph;
    int		fd, pid;
{
    char		c;
    int			status;
    struct timeval	waittime;
    unsigned long	rmask[MSKCNT];
    char		*env_var_value;

    waittime.tv_sec = NORMAL_WAIT_FOR_CHILD; waittime.tv_usec = 0;
#ifdef	DEBUG
    env_var_value = getenv("PHIGS_CHILD_WAIT");
    if ( env_var_value != (char *) NULL ) {
	waittime.tv_sec = atoi( env_var_value );
	fprintf( stderr, "PHIGS_CHILD_WAIT:\tdebug phigsmon %d", pid );
	fprintf( stderr, "\tset phigsmon_wait=0\tto continue.\n" );
    }
#endif	/* DEBUG */

    CLEARBITS(rmask);
    do {
	BITSET(rmask, fd);
	status = select( fd+1, rmask, NULL, NULL, &waittime );
	if( status == 1 )
	    return ( (read( fd, &c, sizeof(c)) == sizeof(c)) && (c == 'p') );
	else if ( status < 0 && errno == EINTR )
	    continue;	/* don't abort just because some signal comes in */
	else if ( status < 0 )
	    return 0;	/* Select failed */
    } while ( status != 0 );

    return 0;
}


#ifndef PEX_API_SOCKET_IPC
static int
attach_shared_mem( cph, fd )
    Cp_handle	cph;
    int		fd;
{
    /* The shared memory segment is created in the child and attached to
     * the parent here.  Once attached, the segment is "removed" so that it
     * will go away when the parent and child terminate.  If not "removed"
     * it will hang around forever.  See the note in the child for more
     * info.
     */

    int			shmid, status = 0;
    struct timeval	waittime;
    extern int		errno;
    unsigned long	rmask[MSKCNT];

    CLEARBITS(rmask);
    BITSET(rmask, fd);
    waittime.tv_sec = 30; waittime.tv_usec = 0;
    if ( (select( fd+1, rmask, NULL, NULL, &waittime) == 1)
	&& (read( fd, (char *)&shmid, sizeof(shmid)) == sizeof(shmid)) ) {
	cph->shm_buf = (Cp_shm_buf *)shmat(shmid,(char *)NULL,0);
	if ( cph->shm_buf != (Cp_shm_buf *)-1 )
	    status = 1;
#ifdef	DEBUG
	else
	    fprintf(stderr, "PARENT: errno is %d after shmat\n", errno);
#endif	/* DEBUG */

	/* "Remove" the shm segment as long as we have its id.
	 * fprintf(stderr, "PARENT: removing: status=%d shmid=%d\n",
	 *					status, shmid);
	 */
	(void)shmctl( shmid, IPC_RMID, (struct shmid_ds *)NULL );
    }
    return status;
}

#else /* PEX_API_SOCKET_IPC defined */
static Cp_shm_buf	dummy_shm_buf;

static int
dummy_attach( cph, fd )
    Cp_handle	cph;
    int		fd;
{
    /* Shared memory not used, just attach a small fake buffer with control
     * values set to force all data to go through the socket.
     */
    cph->shm_buf = &dummy_shm_buf;
    /* Zero buffer sizes force the data to be sent through the socket. */
    cph->shm_buf->args.buf_size = 0;
    cph->shm_buf->data.buf_size = 0;
    cph->shm_buf->ret.buf_size = 0;
    return 1;
}
#endif /* ! PEX_API_SOCKET_IPC */


#ifdef DEBUG
#define DEBUG_PHIGSMON_SOCKET_COMMAND "/tmp/phigs_command"
#define DEBUG_PHIGSMON_SOCKET_ERROR "/tmp/phigs_error"
#include <sys/un.h>
#endif

#if defined(hpux) || defined(macII)
#include <sys/un.h> 
#define socketpair _socketpair
static int socketpair(domain, s_type, protocol, sv)
  int domain, s_type, protocol;
  int sv[2];
{
    struct sockaddr_un name;
    int socket_size, fd;

    if ((sv[0] = socket(domain, s_type, protocol)) < 0)
	return -1;
    if ((fd = socket(domain, s_type, protocol)) < 0) {
	close(sv[0]);
	return -1;
    }
    tmpnam(name.sun_path);
    name.sun_family = AF_UNIX;
#ifdef BSD44SOCKETS /* unlikely */
    name.sun_len = strlen(name.sun_path);
    socket_size = SUN_LEN(&name);
#else
    socket_size = sizeof(name.sun_family) + strlen(name.sun_path) + 1;
#endif
    if (bind(fd, &name, socket_size) < 0 ||
	listen(fd, 1) < 0 ||
	connect(sv[0], &name, socket_size) < 0 ||
	(sv[1] = accept(fd, &name, &socket_size)) < 0) {
	unlink(name.sun_path);
	close(sv[0]);
	close(fd);
	return -1;
    }
    unlink(name.sun_path);
    close(fd);
    return 0;
}
#endif /* hpux or macII */

#if defined(SYSV) && defined(SYSV386)

/* Another losing OS */
#include <netinet/in.h>
#include <net/errno.h>

static int socketpair(domain, s_type, protocol, sv)
  int domain, s_type, protocol;
  int sv[2];
{
    struct sockaddr_in insock;
    int socket_size, fd, port_num, s;

    if ((sv[0] = socket(PF_INET, s_type, protocol)) < 0)
	return -1;
    if ((fd = socket(PF_INET, s_type, protocol)) < 0) {
	close(sv[0]);
	return -1;
    }

    /* Port number of 6100 is arbitrary, (100 above the X server port). */
    bzero ((char *)&insock, sizeof (insock));
#ifdef BSD44SOCKETS /* unlikely */
    insock.sin_len = sizeof(insock);
#endif
    insock.sin_family = AF_INET;
    port_num = 6100;
    insock.sin_port = htons((unsigned short)(port_num));
    insock.sin_addr.s_addr = htonl(INADDR_ANY);
    socket_size = sizeof(struct sockaddr_in);

    /* Bind until we find a free address or encounter an error. */
    while ((s = bind(fd, &insock, sizeof(insock))) < 0 && errno == EADDRINUSE)
	insock.sin_port = htons((unsigned short)(++port_num));

    if (s < 0 ||
	listen(fd, 1) < 0 ||
	connect(sv[0], &insock, socket_size) < 0 ||
	(sv[1] = accept(fd, &insock, &socket_size)) < 0) {
	close(sv[0]);
	close(fd);
	return -1;
    }
    close(fd);
    return 0;
}
#endif /* SYSV && SYSV386 */

static int
fork_monitor( cph, argv )
    Cp_handle	cph;
    char	**argv;
{
    register int	cnt;
    int			status = 0, pid, sp[2], spe[2];
    char		*monitor;
    extern	char	*phg_path();
#ifndef PEX_API_SOCKET_IPC
    int			(*attach_buf)() = attach_shared_mem;
#else
    extern int		dummy_attach();
    int			(*attach_buf)() = dummy_attach;
#endif

#ifdef DEBUG

#ifdef BSD44SOCKETS
#define Connect(fd,addr) {\
    struct sockaddr_un	un_addr; \
\
    strcpy (un_addr.sun_path, addr);\
    un_addr.sun_len = strlen(un_addr.sun_path);\
    un_addr.sun_family = AF_UNIX;\
    fd = socket (AF_UNIX, SOCK_STREAM, 0);\
    if (connect (fd, &un_addr, SUN_LEN(&un_addr)))\
    {\
	perror ("phigsmon connect");\
	abort ();\
    }\
}
#else /* !BSD44SOCKETS */
#define Connect(fd,addr) {\
    struct sockaddr_un	un_addr; \
\
    strcpy (un_addr.sun_path, addr);\
    un_addr.sun_family = AF_UNIX;\
    fd = socket (AF_UNIX, SOCK_STREAM, 0);\
    if (connect (fd, &un_addr, sizeof (short) + strlen (addr)))\
    {\
	perror ("phigsmon connect");\
	abort ();\
    }\
}
#endif /* !BSD44SOCKETS */

    Connect (cph->data.client.sfd, DEBUG_PHIGSMON_SOCKET_COMMAND);
    Connect (cph->erh->data.client.sfd, DEBUG_PHIGSMON_SOCKET_ERROR);
    if (read (cph->data.client.sfd, &pid, sizeof (pid)) != sizeof (pid))
    {
	perror ("phigsmon pid talk");
	abort ();
    }
#else
    if ( !(monitor = phg_path( PHG_SERVER_NAME, cph->erh, 1))) {
	ERR_REPORT( cph->erh, ERRN51);

    } else if ( socketpair( AF_UNIX, SOCK_STREAM, 0, sp) < 0) {
	perror( "fork_monitor(socketpair)");
	ERR_REPORT( cph->erh, ERRN1);

    } else if ( socketpair( AF_UNIX, SOCK_STREAM, 0, spe) < 0) {
	(void)close( sp[0]); (void)close( sp[1]);
	perror( "fork_monitor(socketpair)");
	ERR_REPORT( cph->erh, ERRN1);

    } else {
	if ( (pid = fork()) < 0) {
	    perror( "fork_monitor(fork)");
	    ERR_REPORT( cph->erh, ERRN2);

	} else if ( pid == 0) {	/* child */
	    if ( dup2( sp[1], 0) < 0) {		/* command channel */
		perror("fork_monitor(dup2-1)");
		_exit(0);
	    } else if ( dup2( spe[1], 1) < 0) {	/* error channel */
		perror("fork_monitor(dup2-2)");
		_exit(0);
	    } else {
		/* close fd's on exec, leave stderr open */
#if defined(hpux) || defined(SVR4)
		cnt = _NFILE - 1;
#else
		cnt = getdtablesize() - 1;
#endif	/* hpux */
		for ( ; cnt > 2; cnt--)
		    (void)fcntl( cnt, F_SETFD, 1);
		argv[0] = PHG_SERVER_NAME;
		execv( monitor, argv);
		perror("fork_monitor(execv)");
		_exit(0);
	    }

	} else if ( pid > 0) {	/* parent */
	    /* Load the fd for error reporting from the child.  This
	     * ensures that errors buffered in the child will get flushed
	     * when ERR_REPORT() is called below.
	     */
	    phg_register_signal_func((unsigned long)pid, sig_wait, SIGCHLD);
	    (void)close( sp[1]); (void)close( spe[1]);
	}
	cph->erh->data.client.sfd = spe[0];
	cph->data.client.sfd = sp[0];
    }
#endif
    {
#ifdef AIXV3
        int     fdflags;
        fdflags = 1;
        (void)ioctl (cph->erh->data.client.sfd, FIONBIO, &fdflags);
#else /* AIXV3 */
	/* ultrix reads hang on Unix sockets, hpux reads fail */
#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux))
	(void)fcntl(cph->erh->data.client.sfd, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
	int	fdflags;
	fdflags = 1;
	(void)ioctl (cph->erh->data.client.sfd, FIOSNBIO, &fdflags);
#else
	(void)fcntl(cph->erh->data.client.sfd, F_SETFL, FNDELAY);
#endif
#endif
#endif /* AIXV3 */
    }
    if (   handshake_child( cph, cph->data.client.sfd, pid )
	    && (*attach_buf)( cph, cph->data.client.sfd ) ) {
	cph->data.client.child_pid = pid;
	status = 1;
    } else {
	/* The following message was once used to help debug failures
	 * in attaching shared memory IPC (versus other errors
	 * causing ERRN1 to be raised).
	 * perror(
"PARENT: (handshake_child && attach_shared_mem) failed, so ERR -1.");
	 */
	ERR_REPORT( cph->erh, ERRN1);
	(void)close( cph->data.client.sfd); (void)close( cph->erh->data.client.sfd);
	cph->erh->data.client.sfd = -1; 
	cph->data.client.sfd = -1;
    }

    return status;
}

int
phg_cpxc_start_monitor( cph, argv )
    Cp_handle	cph;
    char	**argv;
{
    int 	status = 0;

    /* set the child_pid field to a known value until we know the child is
     * listening.  This avoids any inadvertantly TRUE comparisons in the
     * SIGCHLD handler.
     */
    cph->data.client.child_pid = -1;

    /*
    set_debug_signal_funcs( cph);
    */

    if ( fork_monitor( cph, argv )) {
	install_signal_funcs( cph);
	cph->flags.monitor_active = 1;
	status = 1;
    }

    return status;
}


void
phg_cpc_close( cph, args )
    Cp_handle		cph;
    Phg_args		*args;	/* may be NULL */
{
    Phg_ret		ret;

    /* Send something to the child and wait for a return to ensure that any
     * buffered operations are completed before killing the child.
     */
    if ( args ) {
	args->op |= CP_OP_RETURN_DATA;
	cpc_exec_cmd( cph, args, &ret);
    }
    ERR_FLUSH( cph->erh);

    phg_register_signal_func((unsigned long)cph, (void(*)())NULL, SIGCHLD);
    phg_register_signal_func((unsigned long)cph->data.client.child_pid,
	sig_wait, SIGCHLD);

    (void)kill( cph->data.client.child_pid, SIGKILL);
    (void)close( cph->erh->data.client.sfd);
    (void)close( cph->data.client.sfd);
}


/* Old code that may be useful in the future. */

/* 29 Dec, 1989 */
#ifdef RET_DATA_IN_SHARED_MEM
    while ( cph->shm_buf->ret.state != CP_RET_DATA_READY )
	CP_MICROSLEEP(1000)
    bcopy( &cph->shm_buf->ret.data, ret, sizeof(*ret) );
    cph->shm_buf->ret.state = CP_RET_DATA_UNLOCKED;
#endif

