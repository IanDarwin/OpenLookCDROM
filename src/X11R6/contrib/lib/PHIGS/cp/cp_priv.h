/* $XConsortium: cp_priv.h,v 5.9 94/04/17 20:41:11 rws Exp $ */

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

#ifndef PHG_CP_INTERNAL_H_INCLUDED
#define PHG_CP_INTERNAL_H_INCLUDED

#if !defined(SVR4) && !defined(sgi) && !defined(hpux)
extern char	*shmat();
#endif

extern int	phg_pex_synchronize;

#define PHG_SERVER_NAME		"phigsmon"

/* Flags and masking constants for server function calls */
#define CP_OPCODE_BITS			0x0000ffff
#define CP_OP_PRE_DATA			0x00010000
#define CP_OP_POST_DATA			0x00020000
#define CP_OP_RETURN_DATA		0x00040000
#define CP_OP_SYNC			0x00080000
#define CP_OP_FORCE_RETURN		0x00100000
#define CP_OP_DATA_IN_BUFFER		0x00200000
#define CP_OP_DATA_FOLLOWS_ARGS		0x00400000
#define CP_OP_RET_DATA_IN_SOCKET	0x00800000

#define CP_INIT_SCRATCH_SIZE	8096

#define CP_MICROSLEEP( interval) \
    {   \
        struct timeval  sleep_interval; \
        sleep_interval.tv_sec = 0;  \
        sleep_interval.tv_usec = interval;    \
	while ( select( 0, 0, 0, 0, &sleep_interval) == -1 ) ; \
    }

#define CP_GET_WSH( _cph, _wsid ) \
    (phg_cpx_ws_exists((_cph),CPX_BY_WSID,(Cpx_css_srvr *)NULL,(_wsid)))

#define CP_ADD_ARH( cph, arh) \
    {register int _i; \
     for ( _i = 0; _i < MAX_NO_OPEN_ARFILES; _i++) \
	 if ( !(cph)->arhl[_i]) { \
	     (cph)->arhl[_i] = (arh); \
	     break; \
	 } \
    }

#define CP_REM_ARH( cph, arh) \
    {register int _i; \
     for ( _i = 0; _i < MAX_NO_OPEN_ARFILES; _i++) \
	 if ( (cph)->arhl[_i] == (arh)) { \
	     (cph)->arhl[_i] = NULL; \
	     break; \
	 } \
    }

#define CP_GET_ARH( cph, arid, arh) \
    {register int _i; \
     register Ar_handle	_a; \
     arh = NULL; \
     for ( _i = 0; _i < MAX_NO_OPEN_ARFILES; _i++) \
	 if ( (_a = (cph)->arhl[_i]) && _a->fid == (arid)) { \
	     (arh) = _a; \
	     break; \
	 } \
    }

#define CP_FOR_ALL_ARH( cph, arh) \
    {register int _i; \
     for ( _i = 0; _i < MAX_NO_OPEN_ARFILES; _i++) \
	 if ( (arh) = (cph)->arhl[_i]) {

#define CP_END_FOR_ALL_ARH	}}

#define CP_GRAB_SHM_LOCK(_shmb,_type) \
    {while ( (_shmb)->_type.lock ) \
	CP_MICROSLEEP(10); \
     (_shmb)->_type.lock = 1; \
    }

#define CP_RELEASE_SHM_LOCK(_shmb,_type) \
    (_shmb)->_type.lock = 0;

#ifdef	DEBUG
#define CP_CHECK_SHM_LOCK(_shmb,_type) \
    { if( (_shmb)->_type.lock != 0 && (_shmb)->_type.lock != 1 )	\
	fprintf(stderr, "CHILD checking _type.lock: shm_buf is %#x\n",	\
			(_shmb));\
    }
#else
#define CP_CHECK_SHM_LOCK(_shmb,_type)	/* do no testing for production */
#endif	/* DEBUG */

#define CP_DATA_BUF_SIZE_NEEDED(_cph,_size) \
    (( (_size) % sizeof((_cph)->shm_buf->data.buf[0]) == 0 ) \
       ? \
	    (_size) / sizeof((_cph)->shm_buf->data.buf[0]) \
       : \
	    ((_size) / sizeof((_cph)->shm_buf->data.buf[0])) + 1)


/* If *cb_ptr is non-null, and not already in callback list,
 * then increment cb_ptr, adding *cb_ptr to callback list.
 * (*cb_ptr) is trash.  Valid data in callback[0] thru *(cb_ptr-1).
 */
#define	CP_ADD_WSH_TO_CALLBACK_LIST(callback,cb_ptr)			\
    {if (*cb_ptr) {							\
	register	Ws_handle	*_dup = cb_ptr;			\
	while (--_dup >= callback) {	/* Is the previous element... */\
	    if (*_dup == *cb_ptr) {	/* ...the same as current? */	\
		cb_ptr--;		/* Leaves cb_ptr same, 'cause */\
		break;			/* of cb_ptr++ after break */	\
	    }								\
	}								\
	cb_ptr++;							\
    }}

typedef struct {
    int		size;
    char	*data;
} Cp_send_vec;

typedef struct {
    int		size;
    char	**dpp;		/* where to put the data pointer */
} Cp_recv_vec;

#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef OPEN_MAX
#ifdef SVR4
#define OPEN_MAX 256
#else
#ifdef MIN
#undef MIN
#endif
#ifdef MAX
#undef MAX
#endif
#include <sys/param.h>
#ifndef MIN
#define MIN( a, b)	(((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX( a, b)	(((a) > (b)) ? (a) : (b))
#endif
#ifdef NOFILE
#define OPEN_MAX NOFILE
#else
#define OPEN_MAX NOFILES_MAX
#endif
#ifdef FLOAT
#undef FLOAT	/* hp9000s300 defines FLOAT in <sys/param.h> */
#endif
#endif
#endif

#if OPEN_MAX > 256
#undef OPEN_MAX
#define OPEN_MAX 256
#endif

#define MSKCNT ((OPEN_MAX + 31) / 32)

#if (MSKCNT==1)
#define BITMASK(i) (1 << (i))
#define MASKIDX(i) 0
#endif
#if (MSKCNT>1)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#endif

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)

#if (MSKCNT==1)
#define COPYBITS(src, dst) dst[0] = src[0]
#define CLEARBITS(buf) buf[0] = 0
#endif
#if (MSKCNT==2)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; }
#endif
#if (MSKCNT==3)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; \
			     dst[2] = src[2]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; buf[2] = 0; }
#endif
#if (MSKCNT==4)
#define COPYBITS(src, dst) dst[0] = src[0]; dst[1] = src[1]; \
			   dst[2] = src[2]; dst[3] = src[3]
#define CLEARBITS(buf) buf[0] = 0; buf[1] = 0; buf[2] = 0; buf[3] = 0
#endif

#if (MSKCNT>4)
#define COPYBITS(src, dst) bcopy((caddr_t) src, (caddr_t) dst,\
				 MSKCNT*sizeof(long))
#define CLEARBITS(buf) bzero((caddr_t) buf, MSKCNT*sizeof(long))
#endif


extern void
    phg_cp_open_ws(),
    phg_cp_close_ws(),
    phg_cp_ws_redraw_all(),
    phg_cp_ws_update(),
    phg_cp_set_disp_state(),
    phg_cp_message(),
    phg_cp_set_rep(),
    phg_cp_set_filter(),
    phg_cp_set_colour_model(),
    phg_cp_set_hlhsr_mode(),
    phg_cp_set_view_input_prio(),
    phg_cp_set_ws_win(),
    phg_cp_set_ws_vp(),
    phg_cp_add_el(),
    phg_cp_copy_all_els(),
    phg_cp_open_struct(),
    phg_cp_close_struct(),
    phg_cp_set_el_ptr(),
    phg_cp_set_edit_mode(),
    phg_cp_delete_el(),
    phg_cp_delete_struct(),
    phg_cp_delete_struct_net(),
    phg_cp_delete_all_structs(),
    phg_cp_change_struct_id(),
    phg_cp_change_struct_refs(),
    phg_cp_change_struct_idrefs(),
    phg_cp_post_struct(),
    phg_cp_unpost_struct(),
    phg_cp_unpost_all(),
    phg_cp_ar_open(),
    phg_cp_ar_close(),
    phg_cp_ar_archive(),
    phg_cp_ar_retrieve(),
    phg_cp_ar_delete(),
    phg_cp_ar_get_names(),
    phg_cp_ar_get_hierarchy(),
    phg_cp_inp_init_dev(),
    phg_cp_inp_set_mode(),
    phg_cp_inp_request(),
    phg_cp_inp_sample(),
    phg_cp_inp_await(),
    phg_cp_inp_flush_dev(),
    phg_cp_set_err_hand_mode(),
    phg_cp_el_search(),
    phg_cp_inc_spa_search(),
    phg_cp_emerg_close(),

    /* Inquiries */
    phg_cp_inq_struct_status(),
    phg_cp_inq_struct_ids(),
    phg_cp_inq_el_ptr(),
    phg_cp_inq_el_type_size(),
    phg_cp_inq_el_content(),
    phg_cp_inq_hierarchy(),
    phg_cp_inq_text_extent(),
    phg_cp_inq_indices(),
    phg_cp_inq_ar_conflicting(),
    phg_cp_inq_posted(),
    phg_cp_inq_ws_posted_to(),
    phg_cp_inq_representation(),
    phg_cp_inq_view_rep(),
    phg_cp_inq_ws_xform(),
    phg_cp_inq_disp_update_state(),
    phg_cp_inq_inp_dev_state(),
    phg_cp_inq_inp_overflow(),
    phg_cp_inq_ws_filter(),
    phg_cp_inq_hlhsr_mode(),
    phg_cp_inq_colour_model(),
    phg_cp_inq_more_events(),
    phg_cp_inq_win_info();

/* PM functions */
extern void
    phg_cpm_close_ws(),
    phg_cpm_open_ws(),
    phg_ntfy_dispatch_event();

extern void
    phg_cp_destroy_wst_list(),
    phg_cpm_load_monitor_funcs();

extern void
    phg_cpr_fclose(),
    phg_cpr_send_ret();

extern int
    phg_cpr_rcv_cmd_shm(),
    phg_cpr_rcv_cmd_socket();

extern int
    phg_cpxc_recv(),
    phg_cpr_fread(),
    phg_cpr_send();

extern Cp_file*
    phg_cpr_fdopen();

extern void
    phg_cpc_close();

extern void
    phg_cpc_close(),
    phg_cpc_class_B(),
    phg_cpc_class_C(),
    phg_cpc_class_D(),
    phg_cpc_class_E(),
    phg_cpc_class_CD(),
    phg_cpc_class_CE(),
    phg_cpc_class_SPECIAL(),
    phg_cpc_await_event(),
    phg_cpc_inp_request();

#endif
