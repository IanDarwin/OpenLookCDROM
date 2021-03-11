/* $XConsortium: cpx_clnt.c,v 5.10 94/04/17 20:41:25 mor Exp $ */

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

/* CPX code that is used only in the client process. */

#define NEED_EVENTS

#include <X11/Xlibint.h>
#include "phg.h"
#include "cp.h"
#include "ar.h"
#include "ws.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"
#include <X11/Xatom.h>

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif



static void
cpxc_load_funcs( cph )
    Cp_handle	cph;
{
    register Cp_func	*f = cph->funcs;

    /* Control functions. */
    f[(int)CP_FUNC_OP_SET_ERR_HAND_MODE]	= phg_cpx_set_err_hand_mode;
    f[(int)CP_FUNC_OP_ERROR_SYNC]		= phg_cpxc_error_sync;
    f[(int)CP_FUNC_OP_EMERG_CLOSE]		= phg_cpxc_emerg_close;
    f[(int)CP_FUNC_OP_CLOSE_PHIGS]		= phg_cpxc_close;

    /* Workstation functions. */
    f[(int)CP_FUNC_OP_OPEN_WS]		= phg_cpxc_open_ws;
    f[(int)CP_FUNC_OP_CLOSE_WS]		= phg_cpxc_close_ws;
    f[(int)CP_FUNC_OP_WS_REDRAW_ALL]	= phg_cpx_ws_redraw_all;
    f[(int)CP_FUNC_OP_WS_UPDATE]	= phg_cpx_ws_update;
    f[(int)CP_FUNC_OP_SET_DISP_STATE]	= phg_cpx_ws_set_disp_state;
    f[(int)CP_FUNC_OP_MESSAGE]		= phg_cpx_message;
    f[(int)CP_FUNC_OP_SET_REP]		= phg_cpx_set_rep;
    f[(int)CP_FUNC_OP_SET_FILTER]	= phg_cpx_set_filter;
    f[(int)CP_FUNC_OP_SET_COLOUR_MODEL]	= phg_cpx_set_colour_model;
    f[(int)CP_FUNC_OP_SET_HLHSR_MODE]	= phg_cpx_set_hlhsr_mode;
    f[(int)CP_FUNC_OP_SET_VIEW_INPUT_PRIO] = phg_cpx_set_view_input_priority;
    f[(int)CP_FUNC_OP_SET_WS_WIN]	= phg_cpx_set_ws_win;
    f[(int)CP_FUNC_OP_SET_WS_VP]	= phg_cpx_set_ws_vp;
    f[(int)CP_FUNC_OP_POST_STRUCT]	= phg_cpx_post_struct;
    f[(int)CP_FUNC_OP_UNPOST_STRUCT]	= phg_cpx_unpost_struct;
    f[(int)CP_FUNC_OP_UNPOST_ALL]	= phg_cpx_unpost_all;
    f[(int)CP_FUNC_OP_INP_INIT_DEV]	= phg_cpx_inp_init_dev;
    f[(int)CP_FUNC_OP_INP_SET_MODE]	= phg_cpx_inp_set_mode;
    f[(int)CP_FUNC_OP_INP_REQUEST]	= phg_cpx_inp_request;
    f[(int)CP_FUNC_OP_INP_SAMPLE]	= phg_cpx_inp_sample;
    f[(int)CP_FUNC_OP_INQ_INDICES]		= phg_cpx_inq_indices;
    f[(int)CP_FUNC_OP_INQ_FILTER]		= phg_cpx_inq_filter;
    f[(int)CP_FUNC_OP_INQ_POSTED]		= phg_cpx_inq_posted;
    f[(int)CP_FUNC_OP_INQ_INP_DEV_STATE]	= phg_cpx_inq_inp_dev_state;
    f[(int)CP_FUNC_OP_INQ_REPRESENTATION]	= phg_cpx_inq_rep;
    f[(int)CP_FUNC_OP_INQ_VIEW_REP]		= phg_cpx_inq_view_rep;
    f[(int)CP_FUNC_OP_INQ_HLHSR_MODE]		= phg_cpx_inq_hlhsr_mode;
    f[(int)CP_FUNC_OP_INQ_DISP_UPDATE_STATE]	= phg_cpx_inq_disp_update_state;
    f[(int)CP_FUNC_OP_INQ_COLOUR_MODEL]	= phg_cpx_inq_colour_model;
    f[(int)CP_FUNC_OP_INQ_WS_XFORM]		= phg_cpx_inq_ws_xform;
    f[(int)CP_FUNC_OP_INQ_WIN_INFO]		= phg_cpx_inq_win_info;
    f[(int)CP_FUNC_OP_XX_INQ_DPY_AND_DRAWABLE]	= phg_cpx_inq_dpy_and_drawable;
    f[(int)CP_FUNC_OP_DRAWABLE_PICK]	= phg_cpx_ws_drawable_pick;
    f[(int)CP_FUNC_OP_MAP_POINTS]	= phg_cpx_ws_map_points;
    f[(int)CP_FUNC_OP_REDRAW_REGIONS]	= phg_cpx_ws_redraw_regions;
    f[(int)CP_FUNC_OP_WS_SYNCH]	= phg_cpx_ws_synch;
    f[(int)CP_FUNC_OP_INQ_COLR_MAP_METH_ST]	= phg_cpx_inq_colr_map_meth_st;

    /* CSS functions. */
    f[(int)CP_FUNC_OP_ADD_EL]		= phg_cpxc_add_el;
    f[(int)CP_FUNC_OP_COPY_ALL_ELS]	= phg_cpxc_copy_all_els;
    f[(int)CP_FUNC_OP_OPEN_STRUCT]	= phg_cpxc_open_struct;
    f[(int)CP_FUNC_OP_CLOSE_STRUCT]	= phg_cpxc_close_struct;
    f[(int)CP_FUNC_OP_SET_EL_PTR]	= phg_cpxc_set_el_ptr;
    f[(int)CP_FUNC_OP_SET_EDIT_MODE]	= phg_cpxc_set_edit_mode;
    f[(int)CP_FUNC_OP_DELETE_EL]	= phg_cpxc_delete_el;
    f[(int)CP_FUNC_OP_DELETE_STRUCT]	= phg_cpxc_delete_struct;
    f[(int)CP_FUNC_OP_DELETE_STRUCT_NET]= phg_cpxc_delete_struct_net;
    f[(int)CP_FUNC_OP_DELETE_ALL_STRUCTS]	= phg_cpxc_delete_all_structs;
    f[(int)CP_FUNC_OP_CHANGE_STRUCT_ID]	= phg_cpxc_change_struct_id;
    f[(int)CP_FUNC_OP_CHANGE_STRUCT_REFS]	= phg_cpx_change_struct_refs;
    f[(int)CP_FUNC_OP_CHANGE_STRUCT_IDREFS]	= phg_cpx_change_struct_idrefs;
    f[(int)CP_FUNC_OP_EL_SEARCH]	= phg_cpx_el_search;
    f[(int)CP_FUNC_OP_INC_SPA_SEARCH]   = phg_cpx_inc_spa_search;
    f[(int)CP_FUNC_OP_INQ_STRUCT_STATUS ]	= phg_cpx_inq_struct_status;
    f[(int)CP_FUNC_OP_INQ_STRUCT_IDS ]		= phg_cpx_inq_struct_ids;
    f[(int)CP_FUNC_OP_INQ_EL_PTR ]		= phg_cpx_inq_el_ptr;
    f[(int)CP_FUNC_OP_INQ_EL_TYPE_SIZE ]	= phg_cpx_inq_el_type_size;
    f[(int)CP_FUNC_OP_INQ_EL_CONTENT ]		= phg_cpx_inq_el_content;
    f[(int)CP_FUNC_OP_INQ_WSS_POSTED_TO]	= phg_cpxc_inq_wss_posted_to;
    f[(int)CP_FUNC_OP_INQ_HIERARCHY]		= phg_cpx_inq_hierarchy;
    f[(int)CP_FUNC_OP_INQ_TEXT_EXTENT]		= phg_cpx_inq_text_extent;

    /* Archive functions. */
    f[(int)CP_FUNC_OP_AR_OPEN]		= phg_cpx_ar_open;
    f[(int)CP_FUNC_OP_AR_CLOSE]		= phg_cpx_ar_close;
    f[(int)CP_FUNC_OP_AR_ARCHIVE]	= phg_cpx_ar_archive;
    f[(int)CP_FUNC_OP_AR_RETRIEVE]	= phg_cpx_ar_retrieve;
    f[(int)CP_FUNC_OP_AR_DELETE]	= phg_cpx_ar_delete;
    f[(int)CP_FUNC_OP_AR_GET_NAMES]	= phg_cpx_ar_get_names;
    f[(int)CP_FUNC_OP_AR_GET_HIERARCHY]	= phg_cpx_ar_get_hierarchy;
    f[(int)CP_FUNC_OP_INQ_CONFLICTING]		= phg_cpx_inq_ar_conflicting;

    /* Workstation independent input functions. */
    if ( cph->flags.monitor_active ) {
	/* These all go directly to the monitor. */
	f[(int)CP_FUNC_OP_INP_AWAIT]		= phg_cpc_await_event;
	f[(int)CP_FUNC_OP_FLUSH_DEV]		= phg_cpc_class_B;
	f[(int)CP_FUNC_OP_INQ_INP_OVERFLOW]	= phg_cpc_class_D;
	f[(int)CP_FUNC_OP_INQ_MORE_EVENTS]	= phg_cpc_class_D;
    } else {
	/* No input available if monitor not active. */
	f[(int)CP_FUNC_OP_INP_AWAIT]		= phg_cpx_dummy_await_event;
	f[(int)CP_FUNC_OP_FLUSH_DEV]		= phg_cpx_dummy_flush_dev;
	f[(int)CP_FUNC_OP_INQ_INP_OVERFLOW]	= phg_cpx_dummy_inq_overflow;
	f[(int)CP_FUNC_OP_INQ_MORE_EVENTS]	= phg_cpx_dummy_inq_more_events;
    }
}


#define DEFAULT_SERVER_NAME	((char*)NULL)

#define DISPLAY_SPECIFIED( _open_info ) \
    ( (_open_info) && (_open_info)->display )

#define MONITOR_USED( _open_info ) \
    ( !(_open_info) || !(_open_info)->flags.no_monitor )

#define FORCE_CLIENT_SS( _open_info ) \
    ( (_open_info) && (_open_info)->flags.force_client_SS )

Display*
determine_default_display( cph, open_info, name, class, pex_info )
    Cp_handle		cph;
    Pxphigs_info	*open_info;
    char		*name, *class;
    Phg_pex_ext_info	*pex_info;
{
    Display		*display = (Display *)NULL;
    char		*server_name = DEFAULT_SERVER_NAME;
    char		*str_type;
    char		*nd, *cd;
    XrmValue		value;

    if ( DISPLAY_SPECIFIED(open_info) )
	display = open_info->display;
    else {
	/* Determine the server name and open it. */
	if ( open_info && open_info->rmdb ) {
	    unsigned	size;

	    /* Attempt to get server name from RMDB. */
	    size = strlen( name ) + 1 + strlen( class ) + 1
		+ 16 /* 2 * strlen( ".display" ) */;
	    if ( nd = malloc( size ) ) {
		cd = nd + strlen( name ) + 1 + 8;
		strcpy( nd, name ); strcat( nd, ".display" );
		strcpy( cd, class ); strcat( cd, ".display" );
		if ( XrmGetResource( open_info->rmdb, nd, cd,
			&str_type, &value ) == True )
		    server_name = value.addr;
		free( nd );
	    }
	}

	if ( !(display = XOpenDisplay( XDisplayName(server_name) )) ) {
	    ERR_BUF( cph->erh, ERRN200 );
	}
    }

    if ( display ) {
	if ( !phg_utx_pex_supported( display, pex_info ) ) {
	    ERR_BUF( cph->erh, ERRN201 );
	    if ( !DISPLAY_SPECIFIED(open_info) )
		XCloseDisplay( display );
		display = (Display *)NULL;
	}
    }

    return display;
}

Cpx_css_srvr*
create_css_srvr( cph, open_info, display, pex_info )
    Cp_handle		cph;
    Pxphigs_info	*open_info;
    Display		*display;
    Phg_pex_ext_info	*pex_info;
{
    Cpx_css_srvr	*css_srvr = (Cpx_css_srvr *)NULL;

    if ( PEX_WS_SUPPORT(pex_info) && !FORCE_CLIENT_SS(open_info) ) {
	css_srvr = phg_cpa_init( cph, display, pex_info );
    } else {
	css_srvr = phg_cpbc_init( cph );
    }

    if ( css_srvr )
	css_srvr->flags.master = 1;

    return css_srvr;
}

static char**
build_argv( cph, open_info, name, class )
    Pxphigs_info	*open_info;
    Cp_handle		cph;
    char		*name, *class;
{
    char	**argv;
    int		i, argc = 4;

    if ( open_info && open_info->args.argv )
	argc += *open_info->args.argc_p;

    if ( !PHG_SCRATCH_SPACE(&cph->scratch, argc * sizeof(char *)) )
	return (char **)NULL;

    /* Leave the zero-th entry for the exec code. */
    argv = (char **)cph->scratch.buf;
    argv[1] = name;
    argv[2] = class;
    if ( open_info && open_info->args.argv ) {
	for ( i = 0; i < *open_info->args.argc_p; i++ )
	    argv[3+i] = open_info->args.argv[i];
	argv[3+i] = NULL;
    } else
	argv[3] = NULL;

    return argv;
}

#define PHG_CP_ADD_WSTS \
    ( phg_cp_add_wst( cph, (Wst_handle)phigs_ws_type_x_tool ) \
     && \
      phg_cp_add_wst( cph, (Wst_handle)phigs_ws_type_x_drawable ) \
    )

Cp_handle
phg_cpxc_open( err_file, open_info )
    char		*err_file;
    Pxphigs_info	*open_info;
{
    Cp_handle		cph;
    char		**argv;
    Phg_pex_ext_info	pex_info;
    Display		*default_display = (Display *)NULL;
    Cpx_css_srvr	*css_srvr = (Cpx_css_srvr *)NULL;
    char		*name, *class;
    Err_handle		(*err_init)();

    name = open_info && open_info->appl_id.name
	? open_info->appl_id.name : "phigs";
    class = open_info && open_info->appl_id.class_name
	? open_info->appl_id.class_name : "Phigs";

    if ( !(cph = (Cp_handle)calloc( 1, sizeof(Cp_struct))) ) {
	ERR_HANDLE( ERR900, Pfn_open_phigs, err_file );
	goto abort;
    }
    if ( FORCE_CLIENT_SS(open_info) )
	cph->flags.force_client_SS = 1;

    if ( !PHG_CP_ADD_WSTS ) {
	ERR_HANDLE( ERR900, Pfn_open_phigs, err_file );
	goto abort;
    }

    if ( !(cph->psl = phg_psl_init()) ) {
	ERR_HANDLE( ERR900, Pfn_open_phigs, err_file );
	goto abort;
    }

    err_init = MONITOR_USED(open_info) ?
	phg_err_init_client : phg_err_init_local;
    if ( !(cph->erh = (*err_init)( err_file )) )
	goto abort;

    cph->scratch.buf = NULL; cph->scratch.size = 0;
    assure(CP_INIT_SCRATCH_SIZE > 0)
    if ( !PHG_SCRATCH_SPACE(&cph->scratch, CP_INIT_SCRATCH_SIZE)) {
	ERR_BUF( cph->erh, ERR900 );
	goto abort;
    }

    /* Exec the PM if the application hasn't requested it not be. */
    if ( MONITOR_USED(open_info) ) {
	cph->configuration = CP_REMOTE;
	if ( !(argv = build_argv( cph, open_info, name, class )) ) {
	    ERR_BUF( cph->erh, ERR900 );
	    goto abort;
	}
	if ( phg_cpxc_start_monitor( cph, argv ) )
	    cph->flags.monitor_active = 1;
	else
	    goto abort;
    } else
	cph->configuration = CP_LOCAL;

    /* Need to have a default display to get started.  Don't always need
     * it for the CSS but the WDT and PDT code always need it.
     */
    default_display = determine_default_display( cph, open_info, name,
	class, &pex_info );
    if ( !default_display )
	goto abort;

    phg_cpx_instance_connection( cph, default_display,
	DISPLAY_SPECIFIED(open_info) ? 0 : 1 );
    if ( getenv( "PEX_SI_API_SYNC" ) ) {
	cph->flags.err_sync = 1;
	XSynchronize( default_display, 1 );
    }

    /* Create and link in the master CSS server. */
    css_srvr = create_css_srvr( cph, open_info, default_display, &pex_info );
    if ( !css_srvr ) {
	/* Any errors will have been reported already. */
	goto abort;
    }
    phg_cpx_link_css_srvr( cph, css_srvr );

    /* Initialize the PHIGS description table. */
    if ( !phg_pdt_init( default_display, (Window)0, cph->erh, &cph->pdt ) )
	/* Any errors will have been reported already. */
	goto abort;

    /* Initialize the workstation description tables. */
    if ( !phg_wst_init( cph->erh, default_display,
	    name, class, open_info ? open_info->rmdb : (XrmDatabase)NULL,
	    cph->flags.monitor_active ? PCAT_OUTIN : PCAT_OUT) )
	/* Any errors will have been reported already. */
	goto abort;

    /* Looks like we can start up.  Finish the initialization. */
    cpxc_load_funcs( cph );

    /* Close the default display if the API opened it and it's not being
     * used for structure storage.
     */
    if ( !DISPLAY_SPECIFIED(open_info) && css_srvr->type == CPX_CLNT_SS )
	phg_cpx_release_connection( cph, default_display );

    return( cph );

abort:
    /* Undo everything that was done. */
    if ( cph ) {
	if ( css_srvr ) {
	    phg_cpx_unlink_css_srvr( cph, css_srvr );
	    if ( css_srvr->destroy )
		(*css_srvr->destroy)( cph, css_srvr );
	}
	if ( default_display )
	    phg_cpx_release_connection( cph, default_display );
	if ( cph->psl )
	    phg_psl_destroy( cph->psl );
	if ( cph->wst_list )
	    phg_cp_destroy_wst_list( cph );
	if ( cph->flags.monitor_active )
	    phg_cpc_close( cph, (Phg_args*)NULL );
	phg_pdt_clear( &cph->pdt );
	if ( cph->erh )
	    ERR_DESTROY( cph->erh );
	if ( cph->scratch.buf )
	    free( (char *)cph->scratch.buf );
	if ( cph )
	    free( (char *)cph );
    }

    return( (Cp_handle)NULL );
}


void
phg_cpxc_close( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    register	Cpx_css_srvr	*css_srvr;

    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
	if ( css_srvr->close_phigs )
	    (*css_srvr->close_phigs)( cph, css_srvr );
    }
    phg_cpx_destroy_all_css_servers( cph );
    if ( cph->psl )
	phg_psl_destroy( cph->psl );
    if ( cph->wst_list )
	phg_cp_destroy_wst_list( cph );
    if ( cph->flags.monitor_active )
	phg_cpc_close( cph, cp_args );
    phg_pdt_clear( &cph->pdt );
    if ( cph->erh )
	ERR_DESTROY( cph->erh );
    if ( cph->scratch.buf )
	free( (char *)cph->scratch.buf );
    free( (char *)cph );
}

static Cpx_css_srvr*
cpxc_get_css_srvr( cph, args )
    Cp_handle		cph;
    Phg_args_open_ws	*args;
{
    int			is_new = 0;
    Cpx_css_srvr	*css_srvr = (Cpx_css_srvr *)NULL;
    Phg_pex_ext_info	pex_info;
    Pint		err = 0;
    Display		*display = (Display *)NULL;

    if ( cph->flags.force_client_SS ) {
	/* See if a type B css_srvr exists. */
	if ( !(css_srvr = phg_cpx_css_srvr_exists( cph,
		CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS )) ) {
	    if ( !(css_srvr = phg_cpbc_init( cph )) )
		return (Cpx_css_srvr *)NULL;
	    else
		is_new = 1;
	}

    } else if ( !(css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_NAME, args->conn_info.display_name )) ) {

	/* There's no existing CSS server around.  Make a new one but avoid
	 * opening up new connections by sharing existing ones.
	 * Determine which display connection to use as follows:
	 *    1) Use an existing connection if the display pointer is
	 *       specified and we already have a connection using that
	 *       pointer.
	 *    2) Use an existing connection if we already have a
	 *       connection to the named server.
	 *    3) Create a new connection using the specified display
	 *       pointer if the server it's connected to supports PEX.
	 *    4) Create a new connection to the named server.
	 */
	if ( (args->conn_info.display &&
		(display = phg_cpx_connection_exists( cph, CPX_BY_DISPLAY,
		    args->conn_info.display )))
	    || (display = phg_cpx_connection_exists( cph, CPX_BY_NAME,
		    args->conn_info.display_name )) ) {
		/* We know PEX is supported on this display because it's in
		 * the CPs connection list.
		 */
		(void)phg_utx_pex_supported( display, &pex_info );
		phg_cpx_instance_connection( cph, display, 0 );

	} else if ( args->conn_info.display &&
		phg_utx_pex_supported( args->conn_info.display, &pex_info ) ) {
	    display = args->conn_info.display;
	    phg_cpx_instance_connection( cph, display, 0 );

	} else if ( display = phg_utx_open_pex_display(
		args->conn_info.display_name, &pex_info, &err ) ) {
		phg_cpx_instance_connection( cph, display, 1 );

	} else {
	    /* Couldn't find an existing connection and unable to open a
	     * new one.
	     */
	    ERR_BUF( cph->erh, err );
	    return (Cpx_css_srvr *)NULL;
	}

	/* Determine the level of PEX support. */
	if ( PEX_WS_SUPPORT(&pex_info) ) {
	    if ( cph->flags.err_sync )
		XSynchronize( display, 1 );
	    if ( css_srvr = phg_cpa_init( cph, display, &pex_info ) )
		is_new = 1;
	    else
		phg_cpx_release_connection( cph, display );

	} else {
	    /* See if a type B css_srvr exists. */
	    if ( !(css_srvr = phg_cpx_css_srvr_exists( cph,
		    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS )) ) {
		is_new = 1;
		css_srvr = phg_cpbc_init( cph );
	    }
	    /* Don't need a connection for this case until a workstation is
	     * opened.
	     */
	    phg_cpx_release_connection( cph, display );
	}
    }

    if ( css_srvr && is_new ) {
	phg_cpx_link_css_srvr( cph, css_srvr );
	if ( !phg_cpx_load_structures( cph, css_srvr ) )
	    phg_cpx_release_css_srvr( cph, css_srvr );
    }

    return css_srvr;
}


void
phg_cpxc_open_ws( cph, cp_args, ret )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Phg_ret			*ret;
{
    Cpx_css_srvr	*css_srvr = (Cpx_css_srvr *)NULL;
    Ws			*ws = (Ws *)NULL;
    Phg_args_open_ws	*args = &cp_args->data.open_ws;

    ret->err = -1;
    if ( !(css_srvr = cpxc_get_css_srvr( cph, args )) )
	return;

    if ( !(ws = (*css_srvr->open_ws)( cph, cp_args, ret, css_srvr )) )
	goto abort;

    /* Link the ws to the cp. */
    CPX_ADD_TO_LIST(Ws,cph->ws_list,ws)
    return;

abort:
    if ( css_srvr ) {
	if ( ws && css_srvr->close_ws ) {
	    Phg_args	d_args;
	    d_args.data.idata = args->wsid;
	    d_args.op = (unsigned)CP_FUNC_OP_CLOSE_WS;
	    (*css_srvr->close_ws)( cph, &d_args, ws );
	}
	phg_cpx_release_css_srvr( cph, css_srvr );
    }
    ret->err = -1;
}


void
phg_cpxc_close_ws( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws			*ws;
    Cpx_css_srvr	*css_srvr;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata )) {
	css_srvr = ws->css_srvr;
	phg_cpx_unlink_ws( cph, ws );
	(*css_srvr->close_ws)( cph, cp_args, ws );
	phg_cpx_release_css_srvr( cph, css_srvr );
    }
}


void
phg_cpxc_open_struct( cph, cp_args, ret )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Phg_ret			*ret;
{
    register	Cpx_css_srvr	*css_srvr;

    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
	(*css_srvr->open_struct)( cph, cp_args, ret, css_srvr );
	if ( ret->err )
	    break;
    }

    if ( !ret->err )
	cph->psl->open_struct = cp_args->data.idata;
    else if ( css_srvr ) { /* srvr will be non-null if an error occurred. */
	/* Something went wrong.  Clean up any structures created before
	 * the error.
	 */
	Cpx_css_srvr	*tsrvr;	/* the server with the error */

	CPX_FOR_ALL_SERVERS(cph,tsrvr) {
	    if ( tsrvr == css_srvr )
		break;
	    (*css_srvr->delete_struct)( cph, cp_args, tsrvr );
	}
    }
}


void
phg_cpxc_error_sync( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    (void)phg_cpx_sync_servers( cph, cp_args );
    if ( cph->flags.monitor_active )
	phg_cpc_class_B( cph, cp_args );
}


static int
copy_struct_a_to_a( cph, ssrvr, dsrvr, ssp, dsp )
    Cp_handle		cph;
    Cpx_css_srvr	*ssrvr, *dsrvr;
    Cpa_struct_data	*ssp, *dsp;
{
    CARD16	s_emode;
    CARD32	s_count, s_length;
    CARD32	d_count;
    CARD32	s_eptr;
    caddr_t	buf;

    PEXGetStructureInfo( ssrvr->display, ssp->xid, &s_emode, &s_eptr,
	&s_count, &s_length, (CARD16*)NULL );
    if ( s_count > 0 ) {
	/* Copy the elements from the existing server to the new one. */
	if ( !PEXFetchElements( ssrvr->display, ssp->xid,
		(CARD16)PEXBeginning, (INT32)0, (CARD16)PEXEnd, (INT32)0,
		&buf, &s_count ) ) {
	    ERR_BUF( cph->erh, ERRN202 );
	    return 0;

	} else {
	    (void)PEXStoreElements( dsrvr->display, dsp->xid, s_count,
		4 * s_length, buf );
	    /* See if it worked. */
	    (void)PEXGetStructureInfo( dsrvr->display, dsp->xid, (CARD16*)NULL,
		(CARD32*)NULL, &d_count, (CARD32*)NULL, (CARD16*)NULL );
	    if ( d_count != s_count ) {
		ERR_BUF( cph->erh, ERRN202 );
		return 0;
	    }
	}
    }

    /* Set the destination structure to the same state as the source. */
    (void)PEXSetElementPointer( dsrvr->display, dsp->xid, PEXBeginning,
	(INT32)s_eptr );
    (void)PEXSetEditingMode( dsrvr->display, dsp->xid, s_emode );

    return 1;
}


int
phg_cpx_full_copy_a_to_a( cph, ssrvr, dsrvr )
    Cp_handle		cph;
    Cpx_css_srvr	*ssrvr, *dsrvr;
{
    register	Cpa_struct_data	*ssp, *dsp;

    /* Create all the structures first so that references to them can be
     * created.
     */
    CPA_FOR_ALL_STRUCTS( ssrvr, ssp )
	if ( !(dsp = phg_cpa_create_struct( cph, dsrvr, ssp->sid,
		(Pedit_mode *)NULL, (Pint)0 )) )
	    return 0;
	phg_cpa_link_struct( cph, dsrvr, dsp );
    CPA_END_FOR_ALL_STRUCTS

    /* Copy the structure contents. */
    CPA_FOR_ALL_STRUCTS( ssrvr, ssp )
	dsp = phg_cpa_struct_exists( cph, dsrvr, CPX_BY_SID, ssp->sid,
	    CPA_STRUCT_OP_CHECK );
	if ( !copy_struct_a_to_a( cph, ssrvr, dsrvr, ssp, dsp ) )
	    return 0;
    CPA_END_FOR_ALL_STRUCTS

    return 1;
}


static int
align_struct_state( cph, ssrvr, dsrvr )
    Cp_handle		cph;
    Cpx_css_srvr	*ssrvr, *dsrvr;
{
    Phg_args		cp_args;
    Phg_ret		ret;

    /* Open the open structure and set the correct structure state. */
    if ( cph->psl->phg_struct_state == PSTRUCT_ST_STOP ) {
	cp_args.op = (unsigned)CP_FUNC_OP_OPEN_STRUCT;
	cp_args.data.idata = cph->psl->open_struct;
	(*dsrvr->open_struct)( cph, &cp_args, &ret, dsrvr );
	if ( ret.err )
	    return 0;

	cp_args.op = (unsigned)CP_FUNC_OP_INQ_EL_PTR;
	(*ssrvr->inq_el_ptr)( cph, &cp_args, &ret, ssrvr );
	cp_args.op = (unsigned)CP_FUNC_OP_SET_EL_PTR;
	cp_args.data.set_el_ptr.op = PHG_ARGS_SETEP_ABS;
	cp_args.data.set_el_ptr.data = ret.data.idata;
	(*dsrvr->set_el_ptr)( cph, &cp_args, &ret, dsrvr );
    }

    /* Set the correct edit mode. */
    cp_args.op = (unsigned)CP_FUNC_OP_SET_EDIT_MODE;
    cp_args.data.idata = (Pint)cph->psl->edit_mode;
    (*dsrvr->set_edit_mode)( cph, &cp_args, dsrvr );

    return 1;
}


int
phg_cpx_full_copy_a_to_b( cph, ssrvr, dsrvr )
    Cp_handle		cph;
    Cpx_css_srvr	*ssrvr, *dsrvr;
{
    Phg_args	cp_args;
    Phg_ret	ret;
    CARD32	el_count, count;
    caddr_t	buf;

    register	Cpa_struct_data	*ssp;
    register	int		i;

    /* Create all the structures and copy the data to them. */
    cp_args.op = (unsigned)CP_FUNC_OP_SET_EDIT_MODE;
    cp_args.data.idata = (Pint)PEDIT_INSERT;
    (*dsrvr->set_edit_mode)( cph, &cp_args, dsrvr );

    CPA_FOR_ALL_STRUCTS( ssrvr, ssp )
	cp_args.op = (unsigned)CP_FUNC_OP_OPEN_STRUCT;
	cp_args.data.idata = ssp->sid;
	(*dsrvr->open_struct)( cph, &cp_args, &ret, dsrvr );
	if ( ret.err )
	    return 0;

	PEXGetStructureInfo( ssrvr->display, ssp->xid, (CARD16 *)NULL,
	    (CARD32 *)NULL, &el_count, (CARD32 *)NULL, (CARD16*)NULL );
	cp_args.op = (unsigned)CP_FUNC_OP_ADD_EL;
	for ( i = 1; i <= el_count; i++ ) {
	    if ( !PEXFetchElements( ssrvr->display, ssp->xid,
		    (CARD16)PEXBeginning, (INT32)i,
		    (CARD16)PEXBeginning, (INT32)i, &buf, &count ) ) {
		ERR_BUF( cph->erh, ERRN202 );
		return 0;
	    }
	    cp_args.data.add_el.pex_oc.size =
		((pexElementInfo *)buf)->length * sizeof(CARD32);
	    cp_args.data.add_el.pex_oc.oc = (pexElementInfo *)buf;
	    (*dsrvr->add_el)( cph, &cp_args, dsrvr );
	}

	cp_args.op = (unsigned)CP_FUNC_OP_CLOSE_STRUCT;
	(*dsrvr->close_struct)( cph, &cp_args, dsrvr );
    CPA_END_FOR_ALL_STRUCTS
    return align_struct_state( cph, ssrvr, dsrvr );
}


int
phg_cpx_full_copy_b_to_a( cph, ssrvr, dsrvr )
    Cp_handle		cph;
    Cpx_css_srvr	*ssrvr, *dsrvr;
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Pint		num_els;
    CARD32		d_count;

    register	Cpa_struct_data	*dsp;
    register	int		i;

    /* Get the list of existing structure ids. */
    cp_args.op = (unsigned)CP_FUNC_OP_INQ_STRUCT_IDS;
    (*ssrvr->inq_struct_ids)( cph, &cp_args, &ret, ssrvr );
    if ( ret.err )
	return 0;
    if ( ret.data.int_list.num_ints == 0 )
	return 1;

    /* Create all the structures so that we can create references to them. */
    for ( i = 0; i < ret.data.int_list.num_ints; i++ ) {
	if ( !(dsp = phg_cpa_create_struct( cph, dsrvr,
		ret.data.int_list.ints[i], (Pedit_mode *)NULL, (Pint)0 )) )
	    return 0;
	phg_cpa_link_struct( cph, dsrvr, dsp );
    }

    CPA_FOR_ALL_STRUCTS( dsrvr, dsp )
	cp_args.op = (unsigned)CP_FUNC_OP_INQ_EL_CONTENT;
	cp_args.data.q_el_data.struct_id = dsp->sid;
	num_els = 0;
	do {	/* until we get an "element doesn't exist" or other error */
	    cp_args.data.q_el_data.el_id = num_els + 1;
	    (*ssrvr->inq_el_content)( cph, &cp_args, &ret, ssrvr );
	    if ( ret.err == 0 ) {
		(void)PEXStoreElements( dsrvr->display, dsp->xid, (CARD32)1,
		    (CARD32)ret.data.el_info.pex_oc.size,
		    (char *)ret.data.el_info.pex_oc.oc );
		++num_els;
	    }
	} while ( ret.err == 0 );
	if ( ret.err != ERR202 )
	    return 0;

	/* See if all the copying worked for this structure. */
	(void)PEXGetStructureInfo( dsrvr->display, dsp->xid, (CARD16*)NULL,
	    (CARD32*)NULL, &d_count, (CARD32*)NULL, (CARD16*)NULL );
	if ( d_count != num_els ) {
	    ERR_BUF( cph->erh, ERRN202 );
	    return 0;
	}
    CPA_END_FOR_ALL_STRUCTS

    return align_struct_state( cph, ssrvr, dsrvr );
}


void
phg_cpxc_add_el( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    /* TODO: "IGNORING FUNCTION" issue: need to recover if the element
     * can't be created on all servers.  Seems hard to do since existing
     * element data might get replaced when Store Elements (or equivalent)
     * is called.
     */

    register	Cpx_css_srvr	*css_srvr;

    /* Trap OCs that are too large for any of the connected servers. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
	if ( css_srvr->type == CPX_SRVR_SS ) {
	    if ( cp_args->data.add_el.pex_oc.oc->length
		    > css_srvr->display->max_request_size ) {
		ERR_BUF( cph->erh, ERRN501);
		return;
	    }
	}
    }

    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->add_el)( cph, cp_args, css_srvr );
}


void
phg_cpxc_close_struct( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->close_struct)( cph, cp_args, css_srvr );
}


void
phg_cpxc_delete_all_structs( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    /* TODO: "IGNORING FUNCTION" issue: need to be sure the open structure
     * can be reopened for ALL servers before deleting any structures.
     */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->delete_all_structs)( cph, cp_args, css_srvr );
}


void
phg_cpxc_delete_struct( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    /* TODO: "IGNORING FUNCTION" issue: need to be sure the open structure
     * can be reopened for ALL servers before deleting any structures.
     */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->delete_struct)( cph, cp_args, css_srvr );
}


void
phg_cpxc_set_edit_mode( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    /* Set the mode for the open structure on all servers. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->set_edit_mode)( cph, cp_args, css_srvr );
}


void
phg_cpxc_set_el_ptr( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    /* Set the pointer for the open structure on all servers. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->set_el_ptr)( cph, cp_args, css_srvr );
}


void
phg_cpxc_copy_all_els( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    /* TODO: "IGNORING FUNCTION" issue: should recover if elements can't
     * be copied on all servers.
     */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->copy_all_els)( cph, cp_args, css_srvr );
}


void
phg_cpxc_delete_el( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    /* Delete the element on all servers. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->delete_el)( cph, cp_args, css_srvr );
}


void
phg_cpxc_delete_struct_net( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr		*css_srvr;

    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->delete_struct_net)( cph, cp_args, css_srvr );
}


void
phg_cpxc_inq_wss_posted_to( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Cpx_css_srvr	*css_srvr;
    Pint		count = 0;
    unsigned int	size = 0;
    Pint		*list;
    int			i;

    ret->err = 0;
    /* Collect them from all the servers. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
	(*css_srvr->inq_wss_posted_to)( cph, cp_args, ret, css_srvr );
	if ( ret->err != 0 )
	    break;
	else if ( ret->data.int_list.num_ints > 0 ) {
	    size += (count + ret->data.int_list.num_ints) * sizeof(Pint);
	    if ( size > cph->scratch.size
		    && !(cph->scratch.buf = realloc(cph->scratch.buf, size)) ) {
		ret->err = ERR900;
		break;
	    } else {
		list = (Pint *)cph->scratch.buf;
		for ( i = 0; i < ret->data.int_list.num_ints; i++ )
		    list[count++] = ret->data.int_list.ints[i];
	    }
	}
    }
    if ( ret->err == 0 ) {
	ret->data.int_list.num_ints = count;
	ret->data.int_list.ints = list;
    }
}


void
phg_cpxc_change_struct_id( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;
    Phg_args		d_cp_args;
    Phg_ret		ret;

    /* Determine if the old struct is posted anywhere. */
    cp_args->data.change_struct.posted = 0;
    d_cp_args.op = (unsigned)CP_FUNC_OP_INQ_WSS_POSTED_TO;
    d_cp_args.funcnum = cp_args->funcnum;
    d_cp_args.data.idata = cp_args->data.change_struct.orig_id;
    ret.err = 0;
    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
	(*css_srvr->inq_wss_posted_to)( cph, &d_cp_args, &ret, css_srvr );
	if ( ret.err != 0 ) 
	    if (ret.err == ERR201)
		ret.data.int_list.num_ints = 0;
	    else 
		return;
	if ( ret.data.int_list.num_ints > 0 ) {
	    cp_args->data.change_struct.posted = 1;
	    break; /* out of loop on all servers, we now know it's posted */
	}
    }

    /* Tell all the servers to change the struct ID. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->change_struct_id)( cph, cp_args, css_srvr );
}


void
phg_cpxc_emerg_close( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;
    Ar_handle		arh;
    Ws_handle		ws;
    Phg_args		cp_args2;

    if ( PSL_STRUCT_STATE( cph->psl ) == PSTRUCT_ST_STOP ) {
	CP_FUNC( cph, CP_FUNC_OP_CLOSE_STRUCT, &cp_args2, NULL );
	PSL_STRUCT_STATE( cph->psl ) == PSTRUCT_ST_STCL;
    }

    CP_FOR_ALL_ARH(cph, arh)
	cp_args2.data.idata = arh->arid;
	CP_FUNC( cph, CP_FUNC_OP_AR_CLOSE, &cp_args2, NULL );
    CP_END_FOR_ALL_ARH

    CPX_FOR_ALL_WS(cph,ws) {
	cp_args2.data.ws_update.wsid = ws->id;
	cp_args2.data.ws_update.flag = PFLAG_PERFORM;
	CP_FUNC( cph, CP_FUNC_OP_WS_UPDATE, &cp_args2, NULL );

	cp_args2.data.idata = ws->id;
	CP_FUNC( cph, CP_FUNC_OP_CLOSE_WS, &cp_args2, NULL );
    }
}

