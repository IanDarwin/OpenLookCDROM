/* $XConsortium: cpx_util.c,v 5.5 94/04/17 20:41:28 mor Exp $ */

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

/* CPX utility functions.  Used in both client and monitor processes. */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "cp_priv.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXtempl.h"
#include "phigspex.h"
#include "alloc.h"


Ws_handle
phg_cpx_ws_exists( cph, id_type, css_srvr, id )
    Cp_handle		cph;
    int			id_type;
    Cpx_css_srvr	*css_srvr;	/* only used if BY XID */
    int			id;
{
    /* Returns pointer to ws info if it exists. */

    register Ws_handle		ws;

    switch ( id_type ) {
	case CPX_BY_WSID:
	    CPX_FOR_ALL_WS(cph,ws) {
		if ( ws->id == (Pint)id )
		    break;
	    }
	    break;
	case CPX_BY_XID:
	    CPX_FOR_ALL_WS(cph,ws) {
		if ( ws->css_srvr == css_srvr && ws->rid == (XID)id )
		    break;
	    }
	    break;
    }

    return ws;
}


void
phg_cpx_link_css_srvr( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    CPX_ADD_TO_LIST(Cpx_css_srvr,cph->css_srvr_list, css_srvr)
}


void
phg_cpx_unlink_css_srvr( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    register	Cpx_css_srvr	**srvrp;

    /* Remove the srvr from the list of connected servers. */
    for ( srvrp = &cph->css_srvr_list; *srvrp; srvrp = &(*srvrp)->next ) {
	if ( *srvrp == css_srvr ) {
	    *srvrp = (*srvrp)->next;
	    break;
	}
    }
}


static void
destroy_css_srvr( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    Display	*display = css_srvr->display;
    char	*vendor_string = css_srvr->pex_info.vendor;

    if ( css_srvr->destroy )
	(*css_srvr->destroy)( cph, css_srvr );
    if ( display )
	phg_cpx_release_connection( cph, display );
    if ( vendor_string )
	XFree( vendor_string );
}


void
phg_cpx_destroy_all_css_servers( cph )
    Cp_handle		cph;
{
    register	Cpx_css_srvr	*css_srvr;
    register    Cpx_css_srvr    *n_css_srvr;

    css_srvr = cph->css_srvr_list;
    while ( css_srvr ) {
	n_css_srvr = css_srvr->next;
	destroy_css_srvr( cph, css_srvr );
	css_srvr = n_css_srvr;
    }

    cph->css_srvr_list = (Cpx_css_srvr *)NULL;
}


void
phg_cpx_release_css_srvr( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    register	Ws	*ws;

    if ( css_srvr->flags.master )
	return;	/* never destroy the master server. */

    /* See if it's still being used by other workstations. */
    CPX_FOR_ALL_WS(cph,ws) {
	if ( ws->css_srvr == css_srvr )
	    break;
    }

    if ( !ws ) {
	/* Remove the css_srvr, it's no longer used. */
	phg_cpx_unlink_css_srvr( cph, css_srvr );
	destroy_css_srvr( cph, css_srvr );
    }
}


Cpx_css_srvr*
phg_cpx_css_srvr_exists( cph, id_type, id )
    Cp_handle		cph;
    int			id_type;
    caddr_t		id;
{
    char		*name;
    Cpx_css_srvr	*css_srvr = (Cpx_css_srvr *)NULL;

    switch ( id_type ) {
	case CPX_BY_DISPLAY:
	    CPX_FOR_ALL_SERVERS(cph,css_srvr)
		if ( css_srvr->display == (Display *)id )
		    break;
	    break;

	case CPX_BY_NAME:
	    name = XDisplayName(id);
	    CPX_FOR_ALL_SERVERS(cph,css_srvr)
		if ( css_srvr->display
			&& !strcmp(name, DisplayString(css_srvr->display)) )
		    break;
	    break;

	case CPX_BY_SS_TYPE:
	    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
		if ( css_srvr->type = (Cpx_css_srvr_type)id )
		    break;
	    }
	    break;
    }
    return css_srvr;
}


int
phg_cpx_load_structures( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    int		status = 0;
    int		(*copy)() = (int(*)())NULL;

    register	Cpx_css_srvr	*msrvr;

    CPX_MASTER_SERVER( cph, msrvr )
    if ( msrvr == css_srvr )
	return 1;

    switch ( css_srvr->type ) {
	case CPX_SRVR_SS:
	    copy = msrvr->full_copy_to_type_a; break;
	case CPX_CLNT_SS:
	    copy = msrvr->full_copy_to_type_b; break;
    }

    if ( copy )
	status = (*copy)( cph, msrvr, css_srvr );
    else
	status = 1; /* no need for a copy */

    return status;
}


void
phg_cpx_unlink_ws( cph, ws )
    Cp_handle		cph;
    Ws	*ws;
{
    register	Ws	**wsp;

    /* Remove from the list of workstations. */
    for ( wsp = &cph->ws_list; *wsp; wsp = &(*wsp)->next ) {
	if ( *wsp == ws ) {
	    *wsp = (*wsp)->next;
	    break;
	}
    }
}



int
phg_pdt_init( display, uwin, erh, pdt )
    Display		*display;
    Window		uwin;
    Err_handle		erh;
    Phg_desc_tbl	*pdt;
{
    int			i;
    Window		win;
    CARD16		types[2];
    CARD32		*idc_data, count, *counts;
    pexEnumTypeIndex	*enum_data;
    ALLOC_DECLARE(5);

    if ( !display || !pdt || !erh )
	return 0;
    win = uwin ? uwin : RootWindow(display,DefaultScreen(display));

    /* Some of this is API dependent and has to be hardcoded. */
    pdt->max_num_open_workstations = PDT_MAX_SIM_OPEN_WS;
    pdt->max_num_open_archives = PDT_MAX_SIM_OPEN_ARFILES;

    assure(PDT_NUM_CHAR_SETS > 0 );
    if ( !ALLOCATED(pdt->char_sets.ints = (Pint * )
	    malloc(PDT_NUM_CHAR_SETS * sizeof(Pint))) ) {
	ERR_BUF( erh, ERR900 );
	goto abort;
    }
    pdt->char_sets.num_ints = PDT_NUM_CHAR_SETS;
    pdt->char_sets.ints[0] = PCS_ASCII;

    types[0] = PEXIDMaxNameSetNames;
    types[1] = PEXIDMaxModelClipPlanes;
    if ( !PEXGetImpDepConstants( display, win, (CARD16)2, types,
	    (char **)&idc_data ) ) {
	ERR_BUF( erh, ERR900 );	/* TODO: use phg_pex_errno. */
	goto abort;
    }
    pdt->max_num_names_for_nameset = idc_data[0];
    pdt->max_num_model_clip_volumes = idc_data[1];
    pdt->max_length_normal_iss_filter = PDT_MAX_ISS_FILTERS;
    pdt->max_length_inverted_iss_filter = PDT_MAX_ISS_FILTERS;

    types[0] = PEXETGSE;
    types[1] = PEXETModelClipOperator;
    if ( !PEXGetEnumeratedTypeInfo( display, win, (CARD32)2, types,
	    (pexBitmask)0, &count, (char **)&counts ) ) {
	ERR_BUF( erh, ERR900 );	/* TODO: use phg_pex_errno. */
	goto abort;
    }
    if ( counts[0] > 0 && !ALLOCATED(pdt->gses.id_facs = (Pgse_id_dep * )
	    malloc((unsigned)(counts[0] * sizeof(Pgse_id_dep)))) ) {
	ERR_BUF( erh, ERR900 );
	goto abort;
    }
    if ( counts[1] > 0 &&  !ALLOCATED(pdt->model_clip_ops.ints = (Pint * )
	    malloc((unsigned)(counts[1] * sizeof(Pint)))) ) {
	ERR_BUF( erh, ERR900 );
	goto abort;
    }

    if ( !PEXGetEnumeratedTypeInfo( display, win, (CARD32)2, types,
		(pexBitmask)1, &count, (char **)&enum_data ) ) {
	ERR_BUF( erh, ERR900 );	/* TODO: use phg_pex_errno. */
	goto abort;
    }

    pdt->gses.num_id_facs = *(CARD32 *)enum_data;
    enum_data += sizeof(CARD32)/sizeof(*enum_data);
    for ( i = 0; i < pdt->gses.num_id_facs; i++, enum_data++ ) {
	pdt->gses.id_facs[i].id = *enum_data;
	/* Since they're server dependent they're workstation dependent. */
	pdt->gses.id_facs[i].ind = PWS_DEP;
    }
    if ( pdt->gses.num_id_facs %2 != 0 ) ++enum_data;

    pdt->model_clip_ops.num_ints = *(CARD32 *)enum_data;
    enum_data += sizeof(CARD32)/sizeof(*enum_data);
    for ( i = 0; i < pdt->model_clip_ops.num_ints; i++, enum_data++ ) {
	pdt->model_clip_ops.ints[i] = *enum_data;
    }

    return 1;

abort:
    ALLOC_FREE;
    bzero( (char *)pdt, sizeof(*pdt) );
    return 0;
}


void
phg_pdt_clear( pdt )
    Phg_desc_tbl	*pdt;
{
    if ( pdt ) {
	if ( pdt->char_sets.num_ints > 0 && pdt->char_sets.ints )
	    free( (char *)pdt->char_sets.ints );
	if ( pdt->gses.num_id_facs > 0 && pdt->gses.id_facs )
	    free( (char *)pdt->gses.id_facs );
	if ( pdt->model_clip_ops.num_ints > 0 && pdt->model_clip_ops.ints )
	    free( (char *)pdt->model_clip_ops.ints );
	bzero( (char *)pdt, sizeof(*pdt) );
    }
}

void
phg_cpx_sync_servers( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Cpx_css_srvr	*css_srvr;
    Ws			*ws;
    int			on_off;

    on_off = (Perrsync)cp_args->data.idata == PERRSYNC_ON ? 1 : 0;
    cph->flags.err_sync = on_off;

    /* Enable or disable synchronization on all attached displays. */
    CPX_FOR_ALL_SERVERS(cph,css_srvr) {
	if ( css_srvr->display )
	    XSynchronize( css_srvr->display, on_off );
    }
    CPX_FOR_ALL_WS(cph,ws) {
	if ( ws->display )
	    XSynchronize( ws->display, on_off );
    }
}


void
phg_cpx_get_win_info( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    Window		root;
    int			x, y;
    unsigned int	width, height, bwidth, depth;

    ret->err = 0;
    (void)XGetGeometry( ws->display, ws->drawable_id, &root, &x, &y,
	&width, &height, &bwidth, &depth );
    ret->data.win_info.display_size.dc_units = PDC_OTHER;
    ret->data.win_info.display_size.size_raster.size_x = width;
    ret->data.win_info.display_size.size_raster.size_y = height;
    ret->data.win_info.display_size.size_raster.size_z = depth;
    ret->data.win_info.display_size.size_dc.size_x = 
	ret->data.win_info.display_size.size_raster.size_x;
    ret->data.win_info.display_size.size_dc.size_y = 
	ret->data.win_info.display_size.size_raster.size_y;
    ret->data.win_info.display_size.size_dc.size_z = 
    ws->type->desc_tbl.phigs_dt.dev_coords[2];
}


Display*
phg_cpx_connection_exists( cph, id_type, id )
    Cp_handle		cph;
    int			id_type;
    caddr_t		id;
{
    char			*name;
    Cp_display_connection	*dcon = (Cp_display_connection *)NULL;

    switch ( id_type ) {
	case CPX_BY_DISPLAY:
	    CPX_FOR_ALL_DISPLAYS(cph,dcon)
		if ( dcon->display == (Display *)id )
		    break;
	    break;

	case CPX_BY_NAME:
	    name = XDisplayName(id);
	    CPX_FOR_ALL_DISPLAYS(cph,dcon)
		if ( !strcmp(name, DisplayString(dcon->display)) )
		    break;
	    break;
    }
    return (dcon ? dcon->display : (Display *)NULL);
}


void
phg_cpx_instance_connection( cph, display, opened_by_api )
    Cp_handle	cph;
    Display	*display;
    int		opened_by_api;
{
    Cp_display_connection	*dcon = (Cp_display_connection *)NULL;

    CPX_FOR_ALL_DISPLAYS(cph,dcon) {
	if ( dcon->display == display ) {
	    ++dcon->instance_count;
	    break;
	}
    }
    if ( !dcon ) {
	/* A new connection. */
	dcon = (Cp_display_connection *)calloc( 1, sizeof(*dcon) );
	dcon->display = display;
	dcon->instance_count = 1;
	if ( opened_by_api )
	    dcon->flags.opened_by_api = 1;
	BITSET(cph->fd_masks, ConnectionNumber(display));
	if ( ConnectionNumber(display) > cph->max_fd )
	    cph->max_fd = ConnectionNumber(display);
	CPX_ADD_TO_LIST(Cp_display_connection,cph->displays,dcon)
    }
}


void
phg_cpx_release_connection( cph, display )
    Cp_handle	cph;
    Display	*display;
{
    register Cp_display_connection	**dconp, *old;

    for ( dconp = &cph->displays; *dconp; dconp = &(*dconp)->next ) {
	if ( (*dconp)->display == display ) {
	    --(*dconp)->instance_count;
	    if ( (*dconp)->instance_count <= 0 ) {
		old = *dconp;
		*dconp = old->next;
		BITCLEAR(cph->fd_masks, ConnectionNumber(old->display));
		if ( old->flags.opened_by_api )
		    XCloseDisplay( old->display );
		free( (char *)old );
	    }
	    break;
	}
    }
}
