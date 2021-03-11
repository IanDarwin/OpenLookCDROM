/* $XConsortium: wsx_lut.c,v 5.4 94/04/17 20:42:33 mor Exp $ */

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

/* PEX/PHIGS workstation utility functions. */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"


void
phg_wsx_destroy_LUTs( ws )
    Ws		*ws;
{
    register	Display		*display = ws->display;
    register	Ws_output_ws	*ows = &ws->out_ws;

    if ( ows->lut.marker )
	(void)PEXFreeLookupTable( display, ows->lut.marker );
    if ( ows->lut.line )
	(void)PEXFreeLookupTable( display, ows->lut.line );
    if ( ows->lut.text )
	(void)PEXFreeLookupTable( display, ows->lut.text );
    if ( ows->lut.interior )
	(void)PEXFreeLookupTable( display, ows->lut.interior );
    if ( ows->lut.edge )
	(void)PEXFreeLookupTable( display, ows->lut.edge );
    if ( ows->lut.colour )
	(void)PEXFreeLookupTable( display, ows->lut.colour );
    if ( ows->lut.depth_cue )
	(void)PEXFreeLookupTable( display, ows->lut.depth_cue );
    if ( ows->lut.light_source )
	(void)PEXFreeLookupTable( display, ows->lut.light_source );
    if ( ows->lut.colour_approx )
	(void)PEXFreeLookupTable( display, ows->lut.colour_approx );
    if ( ows->lut.pattern )
	(void)PEXFreeLookupTable( display, ows->lut.pattern );
    if ( ows->lut.font )
	(void)PEXFreeLookupTable( display, ows->lut.font );
    if ( ows->lut.view )
	(void)PEXFreeLookupTable( display, ows->lut.view );

    if ( ows->htab.marker )
	phg_ut_htab_destroy( ows->htab.marker, (void(*)())NULL );
    if ( ows->htab.line )
	phg_ut_htab_destroy( ows->htab.line, (void(*)())NULL );
    if ( ows->htab.text )
	phg_ut_htab_destroy( ows->htab.text, (void(*)())NULL );
    if ( ows->htab.interior )
	phg_ut_htab_destroy( ows->htab.interior, (void(*)())NULL );
    if ( ows->htab.edge )
	phg_ut_htab_destroy( ows->htab.edge, (void(*)())NULL );
    if ( ows->htab.colour )
	phg_ut_htab_destroy( ows->htab.colour, (void(*)())NULL );
    if ( ows->htab.depth_cue )
	phg_ut_htab_destroy( ows->htab.depth_cue, (void(*)())NULL );
    if ( ows->htab.light_source )
	phg_ut_htab_destroy( ows->htab.light_source, (void(*)())NULL );
    if ( ows->htab.colour_approx )
	phg_ut_htab_destroy( ows->htab.colour_approx, (void(*)())NULL );
    if ( ows->htab.pattern )
	phg_ut_htab_destroy( ows->htab.pattern, (void(*)())NULL );
    if ( ows->htab.font )
	phg_ut_htab_destroy( ows->htab.font, (void(*)())NULL );
    if ( ows->htab.view )
	phg_ut_htab_destroy( ows->htab.view, (void(*)())NULL );

    if ( ows->nset.hlt_incl )
	(void)PEXFreeNameSet( display, ows->nset.hlt_incl );
    if ( ows->nset.hlt_excl )
	(void)PEXFreeNameSet( display, ows->nset.hlt_excl );
    if ( ows->nset.invis_incl )
	(void)PEXFreeNameSet( display, ows->nset.invis_incl );
    if ( ows->nset.invis_excl )
	(void)PEXFreeNameSet( display, ows->nset.invis_excl );
    if ( ows->nset.drawable_pick_incl )
	(void)PEXFreeNameSet( display, ows->nset.drawable_pick_incl );
    if ( ows->nset.drawable_pick_excl )
	(void)PEXFreeNameSet( display, ows->nset.drawable_pick_excl );

    ows->lut.marker = 0;
    ows->lut.line = 0;
    ows->lut.text = 0;
    ows->lut.interior = 0;
    ows->lut.edge = 0;
    ows->lut.colour = 0;
    ows->lut.depth_cue = 0;
    ows->lut.light_source = 0;
    ows->lut.colour_approx = 0;
    ows->lut.pattern = 0;
    ows->lut.font = 0;
    ows->lut.view = 0;

    ows->htab.marker = (Hash_table)NULL;
    ows->htab.line = (Hash_table)NULL;
    ows->htab.text = (Hash_table)NULL;
    ows->htab.interior = (Hash_table)NULL;
    ows->htab.edge = (Hash_table)NULL;
    ows->htab.colour = (Hash_table)NULL;
    ows->htab.depth_cue = (Hash_table)NULL;
    ows->htab.light_source = (Hash_table)NULL;
    ows->htab.colour_approx = (Hash_table)NULL;
    ows->htab.pattern = (Hash_table)NULL;
    ows->htab.font = (Hash_table)NULL;
    ows->htab.view = (Hash_table)NULL;

    ows->nset.hlt_incl = 0;
    ows->nset.hlt_excl = 0;
    ows->nset.invis_incl = 0;
    ows->nset.invis_excl = 0;
    ows->nset.drawable_pick_incl = 0;
    ows->nset.drawable_pick_excl = 0;
}


static int
create_table( ws, table_type, hash_size, err, lut_id, htab )
    Ws			*ws;
    pexTableType	table_type;
    int			hash_size;
    Pint		*err;
    XID			*lut_id;
    Hash_table		*htab;
{
    int		status = 0;
    CARD32	count;
    CARD16	*indices;

    (void)PEXCreateLookupTable( ws->display, ws->drawable_id,
	*lut_id = XAllocID(ws->display), table_type );
    if ( !PEXGetDefinedIndices( ws->display, *lut_id, &count, &indices ))
	*err = ERRN202, *lut_id = 0;
    else if ( !(*htab = phg_ut_htab_create( hash_size )) )
	*err = ERR900;
    else
	status = 1;

    return status;
}

#define HASH_SIZE	10

int
phg_wsx_create_LUTs( ws, create_view_LUT, err )
    register	Ws	*ws;
    int		create_view_LUT;
    Pint	*err;
{
    int		status = 1;
    CARD32	count, *names;

    register	Display		*display = ws->display;
    register	Ws_output_ws	*ows = &ws->out_ws;

    /* Create them all. */
    if ( !create_table( ws, PEXMarkerBundleLUT, HASH_SIZE, err,
	    &ows->lut.marker, &ows->htab.marker ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXLineBundleLUT, HASH_SIZE, err, &ows->lut.line,
	    &ows->htab.line ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXTextBundleLUT, HASH_SIZE, err, &ows->lut.text,
	    &ows->htab.text ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXInteriorBundleLUT, HASH_SIZE, err,
	    &ows->lut.interior, &ows->htab.interior ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXEdgeBundleLUT, HASH_SIZE, err, &ows->lut.edge,
	    &ows->htab.edge ) ) {
	status = 0; goto end;
    }

    /* TODO: make the hash size dependent on the number of colours. */
    if ( !create_table( ws, PEXColourLUT, 100, err, &ows->lut.colour,
	    &ows->htab.colour ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXDepthCueLUT, HASH_SIZE, err,
	    &ows->lut.depth_cue, &ows->htab.depth_cue ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXLightLUT, HASH_SIZE, err,
	    &ows->lut.light_source, &ows->htab.light_source ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXColourApproxLUT, HASH_SIZE, err,
	    &ows->lut.colour_approx, &ows->htab.colour_approx ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXPatternLUT, HASH_SIZE, err, &ows->lut.pattern,
	    &ows->htab.pattern ) ) {
	status = 0; goto end;
    }

    if ( !create_table( ws, PEXTextFontLUT, HASH_SIZE, err, &ows->lut.font,
	    &ows->htab.font ) ) {
	status = 0; goto end;
    }

    if ( create_view_LUT ) {
	if ( !create_table( ws, PEXViewLUT, 20, err, &ows->lut.view,
		&ows->htab.view ) ) {
	    status = 0; goto end;
	}
    }


    (void)PEXCreateNameSet( display, ows->nset.hlt_incl = XAllocID(display) );
    if ( !PEXGetNameSet( display, ows->nset.hlt_incl, &count, &names ) ) {
	ows->nset.hlt_incl = 0; status = 0; goto end;
    }

    (void)PEXCreateNameSet( display, ows->nset.hlt_excl = XAllocID(display) );
    if ( !PEXGetNameSet( display, ows->nset.hlt_excl, &count, &names ) ) {
	ows->nset.hlt_excl = 0; status = 0; goto end;
    }

    (void)PEXCreateNameSet( display, ows->nset.invis_incl = XAllocID(display) );
    if ( !PEXGetNameSet( display, ows->nset.invis_incl, &count, &names ) ) {
	ows->nset.invis_incl = 0; status = 0; goto end;
    }

    (void)PEXCreateNameSet( display, ows->nset.invis_excl = XAllocID(display) );
    if ( !PEXGetNameSet( display, ows->nset.invis_excl, &count, &names ) ) {
	ows->nset.invis_excl = 0; status = 0; goto end;
    }

    (void)PEXCreateNameSet( display, ows->nset.drawable_pick_incl
	= XAllocID(display) );
    if ( !PEXGetNameSet( display, ows->nset.drawable_pick_incl, &count,
	    &names ) ) {
	ows->nset.drawable_pick_incl = 0; status = 0; goto end;
    }

    (void)PEXCreateNameSet( display, ows->nset.drawable_pick_excl
	= XAllocID(display) );
    if ( !PEXGetNameSet( display, ows->nset.drawable_pick_excl, &count,
	    &names ) ) {
	ows->nset.drawable_pick_excl = 0; status = 0; goto end;
    }

end:
    if ( !status ) {
	/* Destroy the ones successfully made. */
	phg_wsx_destroy_LUTs( ws );
    }

    return status;
}


void
phg_wsx_lut_update_htab( ws, htab, lut_id )
    Ws			*ws;
    Hash_table		htab;
    pexLookupTable	lut_id;
{
    CARD32		count;
    pexTableIndex	*indices;

    register int 	i;

    if ( !PEXGetDefinedIndices( ws->display, lut_id, &count, &indices ) ) {
	ERR_BUF( ws->erh, ERR900 );	/* TODO: use phg_pex_errno */
    } else {
	for ( i = 0; i < count; i++ ) {
	    if ( !phg_ut_htab_add_entry( htab, (int)indices[i],
		    (caddr_t)NULL ) ) {
		ERR_BUF( ws->erh, ERR900 );
	    }
	}
    }
}

int
phg_wsx_init_LUTs( ws, init_views )
    register	Ws	*ws;
    int		init_views;
{
    CARD16		count;
    CARD32		size;
    pexTableIndex	start;
    pexLookupTable	lut_id;
    Hash_table		htab;
    Phg_args_rep_data	rep;
    Ws_output_ws	*ows = &ws->out_ws;
    Wst_output_wsdt	*odt = &ws->type->desc_tbl.phigs_dt.out_dt;

    register int 	i;

    if ( init_views ) {
	register	pexViewEntry	*pvb;

	/* Set the predefined views to the values specified in the WDT,
	 * and initialize all the other views to the same settings as view 0.
	 */
	lut_id = ows->lut.view;
	htab = ows->htab.view;
	start = 0;

	count = ws->type->desc_tbl.phigs_dt.num_view_indices;
	size = count * sizeof(pexViewEntry);
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	pvb = (pexViewEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, pvb++ ) {
	    if ( i < ws->type->desc_tbl.phigs_dt.num_predefined_views )
		/* Set a predefined view. */
		phg_utx_view_entry_to_pex(
		    &ws->type->desc_tbl.phigs_dt.default_views[i], pvb );
	    else
		/* Set a non-predefined view to the same setting as view 0. */
		bcopy( ws->scratch.buf /* view 0 */, pvb, sizeof(*pvb) );
	}

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id);
    }
    
    {	/* Line Reps */
	pexLineBundleEntry	*plb;
	Pline_bundle_plus	*lb = odt->default_polyline_bundle_table;

	lut_id = ows->lut.line;
	htab = ows->htab.line;
	start = 1;

	count = odt->num_predefined_polyline_indices;
	rep.bundl.extlnrep = *lb;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_EXTLNREP, &rep,
	    (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	plb = (pexLineBundleEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, lb++ )
	    plb = (pexLineBundleEntry *)phg_utx_line_bndl_to_pex( lb, plb );

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }
    
    {	/* Marker Reps */
	pexMarkerBundleEntry	*pmb;
	Pmarker_bundle_plus	*mb = odt->default_polymarker_bundle_table;

	lut_id = ows->lut.marker;
	htab = ows->htab.marker;
	start = 1;

	count = odt->num_predefined_polymarker_indices;
	rep.bundl.extmkrep = *mb;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_EXTMKREP, &rep,
	    (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	pmb = (pexMarkerBundleEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, mb++ )
	    pmb = (pexMarkerBundleEntry *)phg_utx_marker_bndl_to_pex( mb, pmb );

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }
    
    {	/* Text Reps */
	pexTextBundleEntry	*ptb;
	Ptext_bundle_plus	*tb = odt->default_text_bundle_table;

	lut_id = ows->lut.text;
	htab = ows->htab.text;
	start = 1;

	count = odt->num_predefined_text_indices;
	rep.bundl.exttxrep = *tb;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_EXTTXREP, &rep,
	    (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	ptb = (pexTextBundleEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, tb++ )
	    ptb = (pexTextBundleEntry *)phg_utx_text_bndl_to_pex( tb, ptb );

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }
    
    {	/* Interior Reps */
	pexInteriorBundleEntry	*pib;
	Pint_bundle_plus	*ib = odt->default_interior_bundle_table;

	lut_id = ows->lut.interior;
	htab = ows->htab.interior;
	start = 1;

	count = odt->num_predefined_interior_indices;
	rep.bundl.extinterrep = *ib;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_EXTINTERREP, &rep,
	    (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	pib = (pexInteriorBundleEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, ib++ )
	    pib = (pexInteriorBundleEntry *)
		phg_utx_interior_bndl_to_pex( ib, pib );

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }
    
    {	/* Edge Reps */
	pexEdgeBundleEntry	*peb;
	Pedge_bundle_plus	*eb = odt->default_edge_bundle_table;

	lut_id = ows->lut.edge;
	htab = ows->htab.edge;
	start = 1;

	count = odt->num_predefined_edge_indices;
	rep.bundl.extedgerep = *eb;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_EXTEDGEREP, &rep,
	    (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	peb = (pexEdgeBundleEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, eb++ )
	    peb = (pexEdgeBundleEntry *)phg_utx_edge_bndl_to_pex(eb,peb);

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }
    
    {	/* Depth Cue Reps */
	pexDepthCueEntry	*pdc;
	Pdcue_bundle		*dc = odt->default_depth_cue_table;

	lut_id = ows->lut.depth_cue;
	htab = ows->htab.depth_cue;
	start = 0;

	count = odt->num_predefined_depth_cue_indices;
	rep.bundl.dcuerep = *dc;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_DCUEREP, &rep, (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	pdc = (pexDepthCueEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, dc++ )
	    pdc = (pexDepthCueEntry *)phg_utx_dcue_entry_to_pex( dc, pdc );

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }
    
    {	/* Light Source Reps */
	pexLightEntry		*pls;
	Plight_src_bundle	*ls = odt->default_light_src_table;

	lut_id = ows->lut.light_source;
	htab = ows->htab.light_source;
	start = 1;

	count = odt->num_predefined_light_src_indices;
	rep.bundl.lightsrcrep = *ls;	/* used only to get the size */
	size = phg_utx_lut_entry_size( PHG_ARGS_LIGHTSRCREP, &rep,
	    (Pgcolr *)NULL );
	/* All predefined entries are assumed to have the same colour type. */
	size *= count;
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	pls = (pexLightEntry *)ws->scratch.buf;

	for ( i = 0; i < count; i++, ls++ )
	    pls = (pexLightEntry *)phg_utx_light_entry_to_pex( ls , pls );

	(void)PEXSetTableEntries( ws->display, lut_id, start, count, size,
	    (char *)ws->scratch.buf );
	phg_wsx_lut_update_htab( ws, htab, lut_id );
    }

    /* colour approx is set in phg_wsx_setup_colormap */

    /* Load the other defined LUT indices into the hash tables. */
    phg_wsx_lut_update_htab( ws, ws->out_ws.htab.colour, ws->out_ws.lut.colour);
    phg_wsx_lut_update_htab(ws,ws->out_ws.htab.pattern,ws->out_ws.lut.pattern);
    phg_wsx_lut_update_htab( ws, ws->out_ws.htab.font, ws->out_ws.lut.font );

    return 1;
}

static int
extend_bundle( ws, rep_type, rep, extrep )
    Ws			*ws;
    Phg_args_rep_type	rep_type;
    Phg_args_rep_data	*rep, *extrep;
{
    int			status = 1;
    pexLookupTable	lut_id;
    CARD16		type, entry_status;
    char		*buf;
    Ws_output_ws	*ows = &ws->out_ws;

    /* Get the existing entry data and replace the non-extended fields with
     * the specified values.
     */
    switch ( rep_type ) {
	case PHG_ARGS_LNREP: lut_id = ows->lut.line; break;
	case PHG_ARGS_MKREP: lut_id = ows->lut.marker; break;
	case PHG_ARGS_TXREP: lut_id = ows->lut.text; break;
	case PHG_ARGS_INTERREP: lut_id = ows->lut.interior; break;
	case PHG_ARGS_EDGEREP: lut_id = ows->lut.edge; break;
	case PHG_ARGS_PTREP: lut_id = ows->lut.pattern; break;
    }
    if ( !PEXGetTableEntry( ws->display, lut_id,
	    (pexTableIndex)rep->index, (CARD16)PEXSetValue,
	    &type, &entry_status, (char **)&buf ) ) {
	ERR_BUF( ws->erh, ERR900 );
	status = 0;
    } else {
	switch ( rep_type ) {
	    case PHG_ARGS_LNREP:
		(void)phg_utx_line_bndl_from_pex( (pexLineBundleEntry *)buf,
		    &extrep->bundl.extlnrep );
		extrep->bundl.extlnrep.type = rep->bundl.lnrep.type;
		extrep->bundl.extlnrep.width = rep->bundl.lnrep.width;
		extrep->bundl.extlnrep.colr.type = PINDIRECT;
		extrep->bundl.extlnrep.colr.val.ind
		    = rep->bundl.lnrep.colr_ind;
		break;
	    case PHG_ARGS_MKREP:
		(void)phg_utx_marker_bndl_from_pex( (pexMarkerBundleEntry *)buf,
		    &extrep->bundl.extmkrep );
		extrep->bundl.extmkrep.type = rep->bundl.mkrep.type;
		extrep->bundl.extmkrep.size = rep->bundl.mkrep.size;
		extrep->bundl.extmkrep.colr.type = PINDIRECT;
		extrep->bundl.extmkrep.colr.val.ind
		    = rep->bundl.mkrep.colr_ind;
		break;
	    case PHG_ARGS_TXREP:
		(void)phg_utx_text_bndl_from_pex( (pexTextBundleEntry *)buf,
		    &extrep->bundl.exttxrep );
		extrep->bundl.exttxrep.font = rep->bundl.txrep.font;
		extrep->bundl.exttxrep.prec = rep->bundl.txrep.prec;
		extrep->bundl.exttxrep.char_expan = rep->bundl.txrep.char_expan;
		extrep->bundl.exttxrep.char_space = rep->bundl.txrep.char_space;
		extrep->bundl.exttxrep.colr.type = PINDIRECT;
		extrep->bundl.exttxrep.colr.val.ind
		    = rep->bundl.txrep.colr_ind;
		break;
	    case PHG_ARGS_INTERREP:
		(void)phg_utx_interior_bndl_from_pex(
		    (pexInteriorBundleEntry *)buf,
		    &extrep->bundl.extinterrep );
		extrep->bundl.extinterrep.style = rep->bundl.interrep.style;
		extrep->bundl.extinterrep.style_ind =
		    rep->bundl.interrep.style_ind;
		extrep->bundl.extinterrep.colr.type = PINDIRECT;
		extrep->bundl.extinterrep.colr.val.ind
		    = rep->bundl.interrep.colr_ind;
		break;
	    case PHG_ARGS_EDGEREP:
		(void)phg_utx_edge_bndl_from_pex( (pexEdgeBundleEntry *)buf,
		    &extrep->bundl.extedgerep );
		extrep->bundl.extedgerep.flag = rep->bundl.edgerep.flag;
		extrep->bundl.extedgerep.type = rep->bundl.edgerep.type;
		extrep->bundl.extedgerep.width = rep->bundl.edgerep.width;
		extrep->bundl.extedgerep.colr.type = PINDIRECT;
		extrep->bundl.extedgerep.colr.val.ind
		    = rep->bundl.edgerep.colr_ind;
		break;
	    case PHG_ARGS_PTREP: {
		int	i, count;
		(void)phg_utx_pattern_entry_from_pex( (pexPatternEntry *)buf,
		    &extrep->bundl.extptrep );
		count = rep->bundl.ptrep.dims.size_x
		    * rep->bundl.ptrep.dims.size_y;
		extrep->bundl.extptrep.dims = rep->bundl.ptrep.dims;
		extrep->bundl.extptrep.type = PINDIRECT;
		for ( i = 0; i < count; i++ )
		    extrep->bundl.extptrep.colr_array[i].ind
			= rep->bundl.ptrep.colr_array[i];
		} break;
	}
    }
    return status;
}

static int
free_x_colours(ws, attrs, new_method, pcae)
    Ws			*ws;
    XWindowAttributes	*attrs;
    Pint		new_method;	/* phigs map method */
    pexColourApproxEntry	*pcae; 	/* old pex colour approx entry*/
{
    /* given old and new mapping methods for an entry,
     * determine if the Xcolors associated with the old
     * entry need to be freed before setting the new entry
     * and free them if they do
     */
    unsigned long	plane_mask;
    unsigned long	pixel;
    Pint		old_method;
    register int i;

    if ( pcae->approxType == PEXColourRange)
	old_method = PCOLR_MAP_PSEUDO;
    else if (ws->true_colr_map_count && (ws->true_colr_map_base_pixel == pcae->basePixel))
	old_method = PCOLR_MAP_TRUE;
    else
	old_method = PCOLR_MAP_PSEUDO_N;

    /* never delete old true colors */
    if (old_method == PCOLR_MAP_TRUE)
	return False;
    else {

	pixel = pcae->basePixel;

	if ( (attrs->visual->class == PseudoColor) ||
	     (attrs->visual->class == GrayScale) ) {
	    /* colors allocated with XAllocColorCells (for PSEUDO) */
	    unsigned int	nplanes;

	    /* calculate the number of planes needed */
	    for (nplanes=0, i=1; i<(pcae->max1+1); i=i<<1, nplanes++);

	    for (plane_mask=0, i=0; i<nplanes; i++)
		plane_mask |= (pcae->mult1 << i);
	} else if (attrs->visual->class == DirectColor) {
	    /* colors allocated with XAllocColorPlanes  (for PSEUDO_N) */
	    unsigned int red_planes, green_planes, blue_planes;
	    unsigned int nred = pcae->max1 + 1;
	    unsigned int ngreen = pcae->max2 + 1;
	    unsigned int nblue = pcae->max3 + 1;

	    /* calculate the number of planes needed */
	    for ( red_planes = 0, i = 1; i < nred; i=i<<1, red_planes++ );
	    for ( green_planes = 0, i = 1; i < ngreen; i=i<<1, green_planes++ );
	    for ( blue_planes = 0, i = 1; i < nblue; i=i<<1, blue_planes++);
	    plane_mask = 0; 
	    for (i=0; i<red_planes; i++)
		plane_mask |= (pcae->mult1 << i);
	    for (i=0; i<green_planes; i++)
		plane_mask |= (pcae->mult2 << i);
	    for (i=0; i<blue_planes; i++)
		plane_mask |= (pcae->mult3 << i);
	}

	XFreeColors( ws->display, attrs->colormap, &pixel, 1, plane_mask);
	return True;
	}
}

static int
create_colr_mapping_entry(ws, attrs, cmr, ncolors, xcolors, pcae)
    Ws			*ws;
    XWindowAttributes	*attrs;
    Phg_colr_map_rep	*cmr;
    int			*ncolors;
    XColor		**xcolors;
    pexColourApproxEntry	*pcae;
{
    Display		*dpy = ws->display;
    register XColor	*xcolor;
    int			err;

    switch ( cmr->method ) {
	case PCOLR_MAP_TRUE:
	    pcae->approxType = PEXColourSpace;
	    pcae->approxModel = ws->current_colour_model;
	    if (err = phg_wsx_get_true_colors(ws, attrs, ncolors, xcolors, pcae)) {
	        ERR_BUF( ws->erh, err ); 
		goto fail;	
	    }
	    ws->true_colr_map_count++;
	break;

	case PCOLR_MAP_PSEUDO:
	{
	    unsigned int nplanes;
	    unsigned long *plane_masks;
	    unsigned long pixel;
	    register Pcolr_rep	*pcolr;
	    register unsigned int i;

	    *ncolors = cmr->rec.meth_r2.colrs.num_colr_reps;

	    /* calculate the number of planes needed */
	    for ( nplanes = 0, i = 1; i < *ncolors; i=i<<1, nplanes++ );

	    /* this must be freed in parent procedure */
	    if ( !( *xcolors = xcolor = (XColor *)Malloc(
			(nplanes * sizeof(unsigned long)) + 
			(sizeof(XColor) * *ncolors)) ) ) {
		ERR_BUF( ws->erh, ERR900 ); 
		goto fail;	
	    }
	    plane_masks = (unsigned long *)(xcolor + *ncolors);

	    /* use XAllocColorPlanes if using a DirectColor visual 
	     * we don't support ColourRange on DirectColor
	     */
	    if (!XAllocColorCells( dpy, attrs->colormap, True, plane_masks,
		nplanes, &pixel, 1 )) {
		ERR_BUF( ws->erh, ERR125 ); 
		free( (char *)xcolors );
		*ncolors = 0;
		goto fail;
	    }

    	    pcae->approxType = PEXColourRange;
    	    pcae->approxModel = cmr->rec.meth_r2.colr_model;
	    pcae->dither = True;
	    pcae->weight1 = cmr->rec.meth_r2.weights.floats[0];
	    pcae->weight2 = cmr->rec.meth_r2.weights.floats[1];
	    pcae->weight3 = cmr->rec.meth_r2.weights.floats[2];
	    pcae->max1 = *ncolors - 1;
	    /* max2, max3 not used */

	    /* mult1 is the low-order plane mask for PseudoColor
	     * visuals. mult1, mult2, mult3 are the red, green,
	     * and blue masks for DirectColor visuals.
	     */
	    /* find low order plane mask. it may always be in [0],
	     * but I'm not sure, so look for it
	     */
	    for (i=0, pcae->mult1 = plane_masks[0]; i<nplanes; i++)
		if (plane_masks[i] < pcae->mult1)
			pcae->mult1 = plane_masks[i];
	    pcae->mult2 = 0;
	    pcae->mult3 = 0;
	    pcae->basePixel = pixel;

	    for (i=0, pcolr=cmr->rec.meth_r2.colrs.colr_reps,xcolor = *xcolors; 
		 i< *ncolors; i++, pcolr++, xcolor++ ) {
		xcolor->flags = DoRed | DoGreen | DoBlue;
		xcolor->pixel = i * pcae->mult1 + pixel;
		X_CONV_COLOR_FROM_Pcolr_rep( pcolr, xcolor );
	    }
	    break;
	}
#ifdef PEX_SI_API_DIRECT_VISUAL
/* PSEUDO-N is best done on a DirectColor visual
 * PEX-SI doesn't suppport DirectColor visuals, but here is
 * some untested code in case you want to try
 * This is for a DirectColor visual, though theoretically, it
 * could be done on other visuals, too.
 */
	case PCOLR_MAP_PSEUDO_N:
	{
	    unsigned int nred = cmr->rec.meth_r3.colr_lists.lists[0].num_floats;
	    unsigned int ngreen = cmr->rec.meth_r3.colr_lists.lists[1].num_floats;
	    unsigned int nblue = cmr->rec.meth_r3.colr_lists.lists[2].num_floats;
	    unsigned int red_planes, green_planes, blue_planes;
	    unsigned long pixel, red_mask, green_mask, blue_mask;
	    register int i; 
	    register unsigned long red_shift, green_shift, blue_shift;
	    register float	*pred, *pgreen, *pblue;


	    /* calculate the number of planes needed */
	    for ( red_planes = 0, i = 1; i < nred; i=i<<1, red_planes++ );
	    for ( green_planes = 0, i = 1; i < ngreen; i=i<<1, green_planes++ );
	    for ( blue_planes = 0, i = 1; i < nblue; i=i<<1, blue_planes++ );

	    /* max of three color lists */
	    *ncolors = WS_MAX_3( nred, ngreen, nblue );
	    if ( !( *xcolors = xcolor = (XColor *)Malloc(
			 *ncolors * sizeof(XColor)) ) ) {
		ERR_BUF( ws->erh, ERR900 ); 
		goto fail;	
	    }

	    /* use alloc color cells and load the mults values and
	     * colors differently if using pseduocolor or grayscale
	     * visuals. we don't support colour space on those visuals
	     */
	    if (!XAllocColorPlanes( dpy, attrs->colormap, True, &pixel, 1,
			red_planes, green_planes, blue_planes, 
			&red_mask, &green_mask, &blue_mask )) {
		ERR_BUF( ws->erh, ERR125 ); 
		free( (char *)xcolors );
		*ncolors = 0;
		goto fail;
	    }
    	    pcae->approxType = PEXColourSpace;
    	    pcae->approxModel = cmr->rec.meth_r3.colr_model;
	    pcae->dither = True;

	    pcae->max1 = nred - 1;
	    pcae->max2 = ngreen - 1;
	    pcae->max3 = nblue - 1;

	    for (red_shift = 0; !(red_mask & (1<<red_shift)); red_shift++);
	    for (green_shift = 0; !(green_mask & (1<<green_shift)); green_shift++);
	    for (blue_shift = 0; !(blue_mask & (1<<blue_shift)); blue_shift++);
	    pcae->mult1 = 1 << red_shift;
	    pcae->mult2 = 1 << green_shift;
	    pcae->mult3 = 1 << blue_shift;
	    /* weights aren't used */
	    pcae->basePixel = pixel;

	    for (i=0, xcolor = *xcolors;
		 pred=cmr->rec.meth_r3.colr_lists.lists[0].floats, 
		 pgreen=cmr->rec.meth_r3.colr_lists.lists[1].floats, 
		 pblue=cmr->rec.meth_r3.colr_lists.lists[2].floats, 
		 i< *ncolors; i++, xcolor++ ) {
		xcolor->flags = DoRed | DoGreen | DoBlue;
		xcolor->pixel = (i << red_shift) | 
			       (i << green_shift) | 
			       (i << blue_shift) | pixel;
		if (i < nred)  {
		    X_CONV_COLOR_FROM_Pfloat( *pred, xcolor->red);
		    pred++;
		}
		else xcolor->red = 0.0;
		if ( i < ngreen ) {
		    X_CONV_COLOR_FROM_Pfloat( *pgreen, xcolor->green);
		    pred++;
		}
		else xcolor->green = 0.0;
		if ( i < nblue ) {
		    X_CONV_COLOR_FROM_Pfloat( *pblue, xcolor->blue);
		    pred++;
		}
		else xcolor->blue = 0.0;
	    }
	    break;
	}
#endif
    }
    return True;
fail:
    return False;
}

void
phg_wsx_set_LUT_entry( ws, type, rep, gcolr )
    Ws			*ws;
    Phg_args_rep_type	type;
    Phg_args_rep_data	*rep;
    Pgcolr		*gcolr;	/* for colour LUT only */
{
    unsigned		size, pattern_count;
    pexLookupTable	lut_id;
    Phg_args_rep_data	scratch_rep;
    Hash_table		htab;
    int			max_entries, max_entry_error = ERR103;
    char		*buf;
    pexTableIndex	index = rep->index;
    Ws_output_ws	*ows = &ws->out_ws;
    Wst_output_wsdt	*odt = &ws->type->desc_tbl.phigs_dt.out_dt;
    XColor		*xcolors;
    XWindowAttributes	attrs;
    int			ncolors;

    /* If this is a non-extended bundle setting operation then first extend
     * the bundle by getting the current values of the extended fields.
     */
    switch ( type ) {
	case PHG_ARGS_LNREP:
	    (void)extend_bundle( ws, type, rep, &scratch_rep );
	    rep = &scratch_rep;
	    break;

	case PHG_ARGS_MKREP:
	    (void)extend_bundle( ws, type, rep, &scratch_rep );
	    rep = &scratch_rep;
	    break;

	case PHG_ARGS_TXREP:
	    (void)extend_bundle( ws, type, rep, &scratch_rep );
	    rep = &scratch_rep;
	    break;

	case PHG_ARGS_INTERREP:
	    (void)extend_bundle( ws, type, rep, &scratch_rep );
	    rep = &scratch_rep;
	    break;

	case PHG_ARGS_EDGEREP:
	    (void)extend_bundle( ws, type, rep, &scratch_rep );
	    rep = &scratch_rep;
	    break;

	case PHG_ARGS_PTREP:
	    pattern_count = rep->bundl.ptrep.dims.size_x
		* rep->bundl.ptrep.dims.size_y;
	    if ( pattern_count > 0 && !(scratch_rep.bundl.extptrep.colr_array
		= (Pcoval *)
		    Malloc((unsigned)pattern_count*sizeof(Pcoval))) ) {
		ERR_BUF( ws->erh, ERR900 );
		return;
	    }
	    if ( extend_bundle( ws, type, rep, &scratch_rep ) )
		rep = &scratch_rep;
	    else
		goto end;
	    break;
    }

    size = phg_utx_lut_entry_size( type, rep, gcolr );
    if ( size > 0 && !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	ERR_BUF( ws->erh, ERR900 );
	goto end;
    }

    buf = ws->scratch.buf; /* to keep lint quiet */
    switch ( type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	    lut_id = ows->lut.line;
	    htab = ows->htab.line;
	    max_entries = odt->num_polyline_bundle_entries;
	    (void)phg_utx_line_bndl_to_pex( &rep->bundl.extlnrep,
		(pexLineBundleEntry *)buf );
	    break;

	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	    lut_id = ows->lut.marker;
	    htab = ows->htab.marker;
	    max_entries = odt->num_polymarker_bundle_entries;
	    (void)phg_utx_marker_bndl_to_pex( &rep->bundl.extmkrep,
		(pexMarkerBundleEntry *)buf );
	    break;

	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	    lut_id = ows->lut.text;
	    htab = ows->htab.text;
	    max_entries = odt->num_text_bundle_entries;
	    (void)phg_utx_text_bndl_to_pex( &rep->bundl.exttxrep,
		(pexTextBundleEntry *)buf );
	    break;

	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	    lut_id = ows->lut.interior;
	    htab = ows->htab.interior;
	    max_entries = odt->num_interior_bundle_entries;
	    (void)phg_utx_interior_bndl_to_pex( &rep->bundl.extinterrep,
		(pexInteriorBundleEntry *)buf );
	    break;

	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	    lut_id = ows->lut.edge;
	    htab = ows->htab.edge;
	    max_entries = odt->num_edge_bundle_entries;
	    (void)phg_utx_edge_bndl_to_pex( &rep->bundl.extedgerep,
		(pexEdgeBundleEntry *)buf );
	    break;

	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	    lut_id = ows->lut.pattern;
	    htab = ows->htab.pattern;
	    max_entries = odt->num_pattern_table_entries;
	    (void)phg_utx_pattern_entry_to_pex( &rep->bundl.extptrep,
		(pexPatternEntry *)buf );
	    break;

	case PHG_ARGS_COREP:
	    lut_id = ows->lut.colour;
	    htab = ows->htab.colour;
	    max_entries = odt->num_colour_indices;
	    (void)phg_utx_colour_entry_to_pex( gcolr,
		(pexColourSpecifier *)buf );
	    break;

	case PHG_ARGS_DCUEREP:
	    lut_id = ows->lut.depth_cue;
	    htab = ows->htab.depth_cue;
	    max_entries = odt->num_depth_cue_bundle_entries;
	    (void)phg_utx_dcue_entry_to_pex( &rep->bundl.dcuerep,
		(pexDepthCueEntry *)buf );
	    break;

	case PHG_ARGS_LIGHTSRCREP:
	    lut_id = ows->lut.light_source;
	    htab = ows->htab.light_source;
	    max_entries = odt->num_light_src_bundle_entries;
	    (void)phg_utx_light_entry_to_pex( &rep->bundl.lightsrcrep,
		(pexLightEntry *)buf );
	    break;

	case PHG_ARGS_VIEWREP:
	    lut_id = ows->lut.view;
	    htab = ows->htab.view;
	    max_entries = ws->type->desc_tbl.phigs_dt.num_view_indices;
	    max_entry_error = ERR150;
	    (void)phg_utx_view_entry_to_pex( &rep->bundl.viewrep,
		(pexViewEntry *)buf );
	    break;
	case PHG_ARGS_COLRMAPREP:  {
    	    CARD16		table_type, entry_status;
    	    pexColourApproxEntry	*cur_entry;

	    lut_id = ows->lut.colour_approx;
	    htab = ows->htab.colour_approx;
	    max_entries = odt->num_colr_mapping_entries;

	    XGetWindowAttributes(ws->display, ws->drawable_id, &attrs);

	    /* get current entry first so colors can be freed  */
    	    if ( !PEXGetTableEntry( ws->display, lut_id, 
	            (pexTableIndex)index, PEXSetValue, &table_type,
	       	    &entry_status, (char **)&cur_entry ) ) {
	    	ERR_BUF( ws->erh, ERR900 );
	        goto end;
	    } else {
		if ( entry_status != PEXDefaultEntry )
		    (void *)free_x_colours(ws, &attrs,
		            rep->bundl.colrmaprep.method, cur_entry);

		/* returns list of xcolors to store and pexColourApproxEntry */
		if ( !create_colr_mapping_entry(ws, &attrs, 
			    &rep->bundl.colrmaprep, 
			    &ncolors, &xcolors, (pexColourApproxEntry *)buf)) {
		    goto end;
		}
	    }
	    break;
	}
    }

    /* See if the entry exists already. */
    if ( phg_ut_htab_get_entry( htab, (int)index, (caddr_t *)NULL ) ) {
	/* The entry exists, so just set the LUT. */
	(void)PEXSetTableEntries( ws->display, lut_id, index, (CARD16)1,
	    (CARD32)size, buf );

    } else {
	/* The entry doesn't exist yet, so check to see if it will
	 * exceed the number of entries allowed.  Create it if it won't.
	 */
	CARD16		table_type;
	CARD16		entry_status;
	char		*q_data;

	if ( phg_ut_htab_num_entries( htab ) >= max_entries ) {
	    ERR_BUF( ws->erh, max_entry_error );
	    goto end;
	}

	(void)PEXSetTableEntries( ws->display, lut_id, index, (CARD16)1,
	    (CARD32)size, buf );

	/* See if it was created.  If it was, add it to the hash table. */
	if ( !PEXGetTableEntry( ws->display, lut_id, index,
		PEXSetValue, &table_type, &entry_status, &q_data )
		|| entry_status == PEXDefaultEntry ) {
	    ERR_BUF( ws->erh, max_entry_error );
	    goto end;
	}

	if ( !phg_ut_htab_add_entry( htab, index, (caddr_t)NULL ) ) {
	    PEXDeleteTableEntries( ws->display, lut_id, (int)index, (CARD16)1 );
	    ERR_BUF( ws->erh, ERR900 );
	    goto end;
	}
    }

end:
    /* Perform any required cleanup. */
    switch( type ) {
	case PHG_ARGS_PTREP:
	    if ( pattern_count > 0 )
		free( (char *)scratch_rep.bundl.extptrep.colr_array );
	    break;
        case PHG_ARGS_COLRMAPREP:
            if (ncolors) {
                /* TODO: clear window */
                XStoreColors( ws->display, attrs.colormap, xcolors, ncolors);
                /* TODO: redraw picture */
                free ( (char *)xcolors );
            }
            break;
    }
    return;
}

void
phg_wsx_inq_LUT_entry( ws, index, type, rep_type, ret, gcolr, vrep )
    Ws			*ws;
    Pint		index;
    Pinq_type		type;	/* set or realized */
    Phg_args_rep_type	rep_type;
    Phg_ret		*ret;
    Pgcolr		*gcolr;	/* for colour rep only */
    Pview_rep3		*vrep;	/* for view rep only */
{
    pexLookupTable	lut_id;
    CARD16		table_type, entry_status;
    Hash_table		htab;
    char		*buf;
    Ws_output_ws	*ows = &ws->out_ws;

    switch ( rep_type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	    lut_id = ows->lut.line;
	    htab = ows->htab.line;
	    break;
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	    lut_id = ows->lut.marker;
	    htab = ows->htab.marker;
	    break;
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	    lut_id = ows->lut.text;
	    htab = ows->htab.text;
	    break;
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	    lut_id = ows->lut.interior;
	    htab = ows->htab.interior;
	    break;
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	    lut_id = ows->lut.edge;
	    htab = ows->htab.edge;
	    break;
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	    lut_id = ows->lut.pattern;
	    htab = ows->htab.pattern;
	    break;
	case PHG_ARGS_COREP:
	    lut_id = ows->lut.colour;
	    htab = ows->htab.colour;
	    break;
	case PHG_ARGS_DCUEREP:
	    lut_id = ows->lut.depth_cue;
	    htab = ows->htab.depth_cue;
	    break;
	case PHG_ARGS_LIGHTSRCREP:
	    lut_id = ows->lut.light_source;
	    htab = ows->htab.light_source;
	    break;
	case PHG_ARGS_VIEWREP:
	    lut_id = ows->lut.view;
	    htab = ows->htab.view;
	    break;
	case PHG_ARGS_COLRMAPREP:
	    lut_id = ows->lut.colour_approx;
	    htab = ows->htab.colour_approx;
	    break;
    }

    /* See if it's been defined yet. */
    if ( !phg_ut_htab_get_entry( htab, index, (caddr_t *)NULL ) ) {
	/* It hasn't.  See if this is an error or not. */
	if ( type == PINQ_REALIZED
		&& phg_ut_htab_get_entry( htab, 1, (caddr_t *)NULL ) ) {
	    index = 1;
	} else {
	    if ( type == PINQ_REALIZED && ( rep_type == PHG_ARGS_PTREP ||
		rep_type == PHG_ARGS_EXTPTREP ) ) {
		ret->err = ERR109;
	    } else {
		ret->err = ERR101;
	    }
	    return;
	}
    }

    if ( !PEXGetTableEntry( ws->display, lut_id, (pexTableIndex)index,
	    (CARD16)PEX_CONV_FROM_Pinq_type(type), &table_type,
	    &entry_status, &buf ) ) {
	ret->err = ERR900;

    } else {
	Phg_ret_rep	*rep = &ret->data.rep;
	unsigned	size;
	switch ( rep_type ) {
	    case PHG_ARGS_LNREP:
	    case PHG_ARGS_EXTLNREP:
		(void)phg_utx_line_bndl_from_pex(
		    (pexLineBundleEntry *)buf, &rep->extlnrep );
		break;
	    case PHG_ARGS_MKREP:
	    case PHG_ARGS_EXTMKREP:
		(void)phg_utx_marker_bndl_from_pex(
		    (pexMarkerBundleEntry *)buf, &rep->extmkrep );
		break;
	    case PHG_ARGS_TXREP:
	    case PHG_ARGS_EXTTXREP:
		(void)phg_utx_text_bndl_from_pex(
		    (pexTextBundleEntry *)buf, &rep->exttxrep );
		break;
	    case PHG_ARGS_INTERREP:
	    case PHG_ARGS_EXTINTERREP:
		(void)phg_utx_interior_bndl_from_pex(
		    (pexInteriorBundleEntry *)buf, &rep->extinterrep );
		break;
	    case PHG_ARGS_EDGEREP:
	    case PHG_ARGS_EXTEDGEREP:
		(void)phg_utx_edge_bndl_from_pex(
		    (pexEdgeBundleEntry *)buf, &rep->extedgerep );
		break;
	    case PHG_ARGS_PTREP:
	    case PHG_ARGS_EXTPTREP:
		size = ((pexPatternEntry *)buf)->numx
		    * ((pexPatternEntry *)buf)->numy * sizeof(Pcoval);
		if ( size > 0 && !(rep->extptrep.colr_array = (Pcoval *)
			PHG_SCRATCH_SPACE( &ws->scratch, size )) )
		    ret->err = ERR900;
		else
		    (void)phg_utx_pattern_entry_from_pex(
			(pexPatternEntry *)buf, &rep->extptrep );
		break;
	    case PHG_ARGS_COREP:
		(void)phg_utx_colour_entry_from_pex(
		    (pexColourSpecifier *)buf, gcolr );
		break;
	    case PHG_ARGS_DCUEREP:
		(void)phg_utx_dcue_entry_from_pex(
		    (pexDepthCueEntry *)buf, &rep->dcuerep );
		break;
	    case PHG_ARGS_LIGHTSRCREP:
		(void)phg_utx_light_entry_from_pex(
		    (pexLightEntry *)buf, &rep->lightsrcrep );
		break;
	    case PHG_ARGS_VIEWREP:
		(void)phg_utx_view_entry_from_pex(
		    (pexViewEntry *)buf, vrep );
		break;
	    case PHG_ARGS_COLRMAPREP: {
		pexColourApproxEntry *pcmb = (pexColourApproxEntry *)buf;

		switch( pcmb->approxType ) {
		    case PEXColourSpace:
		       /* don't need space for TRUE */
		       if ( !ws->true_colr_map_count ||
			    (ws->true_colr_map_base_pixel != pcmb->basePixel) ) {
			    size = 3 * sizeof( Pfloat_list ) +
				(pcmb->max1 + pcmb->max2 + pcmb->max3 + 3)
				* sizeof( Pfloat );

		            if ( size > 0 && !(rep->colrmaprep.rec.meth_r3.colr_lists.lists = 
				(Pfloat_list *)PHG_SCRATCH_SPACE( &ws->scratch, size )) )
		    		ret->err = ERR900;
			    else {
				rep->colrmaprep.rec.meth_r3.colr_lists.lists[0].floats = 
					(Pfloat *)(rep->colrmaprep.rec.meth_r3.colr_lists.lists + 3);
				rep->colrmaprep.rec.meth_r3.colr_lists.lists[1].floats = 
				    rep->colrmaprep.rec.meth_r3.colr_lists.lists[0].floats + pcmb->max1 + 1;
				rep->colrmaprep.rec.meth_r3.colr_lists.lists[2].floats = 
				    rep->colrmaprep.rec.meth_r3.colr_lists.lists[1].floats + pcmb->max2 + 1;
			    }
			}
			break;
		    case PEXColourRange:
			size = 3 * sizeof( Pfloat) +
			    (pcmb->max1 + 1) * sizeof( Pcolr_rep );
		        if ( size > 0 && !(rep->colrmaprep.rec.meth_r2.weights.floats = 
				(Pfloat *)PHG_SCRATCH_SPACE( &ws->scratch, size )) )
		    		ret->err = ERR900;
			else {
			    rep->colrmaprep.rec.meth_r2.colrs.colr_reps =
				(Pcolr_rep *)(rep->colrmaprep.rec.meth_r2.weights.floats + 3);
			}
			break;
		}

		if ( !phg_wsx_colr_mapping_entry_from_pex(ws,
		    (pexColourApproxEntry *)buf, &rep->colrmaprep ) )
		    ret->err = ERR900;
		break;
		}
	}
    }
}


void
phg_wsx_inq_LUT_indices( ws, type, ret )
    Ws			*ws;
    Phg_args_rep_type	 type;
    Phg_ret		*ret;
{
    CARD32		count;
    pexTableIndex	*indices;
    pexLookupTable	lut_id;

    register	int	i;
    register	Pint	*list;

    ret->err = 0;
    switch ( type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP: lut_id = ws->out_ws.lut.line; break;
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP: lut_id = ws->out_ws.lut.marker; break;
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP: lut_id = ws->out_ws.lut.text; break;
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP: lut_id = ws->out_ws.lut.interior; break;
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP: lut_id = ws->out_ws.lut.edge; break;
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP: lut_id = ws->out_ws.lut.pattern; break;
	case PHG_ARGS_DCUEREP: lut_id = ws->out_ws.lut.depth_cue; break;
	case PHG_ARGS_LIGHTSRCREP: lut_id = ws->out_ws.lut.light_source; break;
	case PHG_ARGS_COREP: lut_id = ws->out_ws.lut.colour; break;
	case PHG_ARGS_VIEWREP: lut_id = ws->out_ws.lut.view; break;
	case PHG_ARGS_COLRMAPREP: lut_id = ws->out_ws.lut.colour_approx; break;
    }

    if ( !PEXGetDefinedIndices( ws->display, lut_id, &count, &indices ) )
	ret->err = ERR900;	/* TODO: use phg_pex_errno. */
    else {
	if (count > 0 ) {
	    if ( PHG_SCRATCH_SPACE(&ws->scratch, count*sizeof(Pint)) ) {
		ret->data.int_list.num_ints = count;
		ret->data.int_list.ints = list = (Pint *)ws->scratch.buf;
		for ( i = 0; i < count; i++, list++, indices++ )
		    *list = *indices;
	    } else {
		ret->data.int_list.num_ints = 0;
		ret->err = ERR900;
	    }
	} else
	    ret->data.int_list.num_ints = 0;
    }
}


void
phg_wsx_set_name_set( ws, type, devid, inc_set, exc_set )
    Ws			*ws;
    Phg_args_flt_type	type;
    Pint		devid;
    Pint_list		*inc_set;
    Pint_list		*exc_set;
{
    pexNameSet		incl_id, excl_id;
    CARD16		incl_count, excl_count;

    register	int	i;
    register	pexName	*incl, *excl;
    register	Pint	*phincl, *phexcl;

    /* Determine the PEX name set id. */
    switch ( type ) {
	case PHG_ARGS_FLT_HIGH:
	    incl_id = ws->out_ws.nset.hlt_incl;
	    excl_id = ws->out_ws.nset.hlt_excl;
	    break;
	case PHG_ARGS_FLT_INVIS:
	    incl_id = ws->out_ws.nset.invis_incl;
	    excl_id = ws->out_ws.nset.invis_excl;
	    break;
	case PHG_ARGS_FLT_DRAWABLE_PICK:
	    incl_id = ws->out_ws.nset.drawable_pick_incl;
	    excl_id = ws->out_ws.nset.drawable_pick_excl;
	    break;
	case PHG_ARGS_FLT_PICK: {
	    Ws_inp_pick		*pick = &ws->in_ws.devs.pick[devid-1];

	    incl_id = pick->filter.incl;
	    excl_id = pick->filter.excl;
	} break;
    }

    /* Convert the filter to PEX format.  Clamp the lengths to the maximum
     * supported by the server.
     */
    incl_count = MIN(inc_set->num_ints,
	ws->type->desc_tbl.xwin_dt.max_num_names_for_nameset);
    excl_count = MIN(exc_set->num_ints,
	ws->type->desc_tbl.xwin_dt.max_num_names_for_nameset);
    phincl = inc_set->ints;
    phexcl = exc_set->ints;
    if ( PHG_SCRATCH_SPACE( &ws->scratch,
	    (incl_count + excl_count) * sizeof(*incl)) ) {
	incl = (pexName *)ws->scratch.buf;
	excl = incl + incl_count;
	for ( i = 0; i < incl_count; i++ )
	    incl[i] = (pexName)phincl[i];
	for ( i = 0; i < excl_count; i++ )
	    excl[i] = (pexName)phexcl[i];
    } else {
	ERR_BUF( ws->erh, ERR900 );
	return;
    }

    /* Set the name set. */
    (void)PEXChangeNameSet( ws->display, incl_id, (CARD16)PEXNSReplace,
	incl_count, incl );
    (void)PEXChangeNameSet( ws->display, excl_id, (CARD16)PEXNSReplace,
	excl_count, excl );
}


void
phg_wsx_inq_name_set( ws, type, devid, ret )
    Ws			*ws;
    Phg_args_flt_type	type;
    Pint		devid;
    Phg_ret		*ret;
{
    pexNameSet	incl_id, excl_id;
    pexName	*incl, *excl;
    CARD32	incl_count, excl_count;

    register	int	i;
    register	Pint	*phincl, *phexcl;

    switch ( type ) {
	case PHG_ARGS_FLT_HIGH:
	    incl_id = ws->out_ws.nset.hlt_incl;
	    excl_id = ws->out_ws.nset.hlt_excl;
	    break;
	case PHG_ARGS_FLT_INVIS:
	    incl_id = ws->out_ws.nset.invis_incl;
	    excl_id = ws->out_ws.nset.invis_excl;
	    break;
	case PHG_ARGS_FLT_PICK: {
	    Ws_inp_pick		*pick = &ws->in_ws.devs.pick[devid-1];

	    incl_id = pick->filter.incl;
	    excl_id = pick->filter.excl;
	} break;
    }

    ret->err = 0;
    if ( !PEXGetNameSet( ws->display, incl_id, &incl_count, &incl ) )
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else if (!PEXGetNameSet(ws->display, excl_id, &excl_count, &excl) )
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else {
	ret->data.filter.incl.num_ints = incl_count;
	ret->data.filter.excl.num_ints = excl_count;
	if ( (incl_count + excl_count) > 0 ) {
	    if ( !PHG_SCRATCH_SPACE( &ws->scratch,
		    (incl_count + excl_count) * sizeof(Pint)) ) {
		ret->err = ERR900;
		ret->data.filter.incl.num_ints = 0;
		ret->data.filter.excl.num_ints = 0;
	    } else {
		phincl = (Pint *)ws->scratch.buf;
		phexcl = phincl + incl_count;
		ret->data.filter.incl.ints = phincl;
		ret->data.filter.excl.ints = phexcl;
		for ( i = 0; i < excl_count; i++ )
		    phexcl[i] = excl[i];
		(void)PEXGetNameSet( ws->display, incl_id, &incl_count,
		    &incl );
		for ( i = 0; i < incl_count; i++ )
		    phincl[i] = incl[i];
	    }
	}
    }
}
