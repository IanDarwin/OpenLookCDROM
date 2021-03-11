/* $XConsortium: wsb.c,v 5.9 94/04/17 20:42:31 mor Exp $ */

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

/* PEX/PHIGS workstation utility functions for the B model (client side
 * workstations and structure storage).
 */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "css.h"
#include "alloc.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"


static void
wsb_load_funcs( ws )
    Ws 		*ws;
{
    ws->close = phg_wsb_close_ws;
    ws->redraw_all = phg_wsb_redraw_all;
    ws->conditional_redraw = phg_wsb_conditional_redraw;
    ws->repaint_all = phg_wsb_repaint_all;
    ws->make_requested_current = phg_wsb_make_requested_current;
    ws->update = phg_wsb_update;
    ws->set_disp_update_state = phg_wsb_set_disp_update_state;
    ws->set_rep = phg_wsb_set_rep;
    ws->set_filter = phg_wsb_set_filter;
    ws->set_colour_model = (void(*)())NULL;	/* state set by CP */
    ws->set_hlhsr_mode = phg_wsb_set_hlhsr_mode;
    ws->set_view_input_priority = phg_wsb_set_view_input_priority;
    ws->set_ws_window = phg_wsb_set_ws_window;
    ws->set_ws_vp = phg_wsb_set_ws_vp;
    ws->delete_el_for_repl = (void(*)())NULL;	/* not used */
    ws->add_el = phg_wsb_add_el;
    ws->copy_struct = phg_wsb_copy_struct;
    ws->close_struct = phg_wsb_close_struct;
    ws->move_ep = (void(*)())NULL;	/* not used */
    ws->delete_el = phg_wsb_delete_el;
    ws->delete_struct = phg_wsb_delete_struct;
    ws->delete_struct_net = phg_wsb_delete_struct_net;
    ws->delete_all_structs = phg_wsb_delete_all_structs;
    ws->post = phg_wsb_post;
    ws->unpost = phg_wsb_unpost;
    ws->unpost_all = phg_wsb_unpost_all;
    ws->change_posting = phg_wsb_change_posting;
    ws->drawable_pick = phg_wsb_drawable_pick;
    ws->map_points = phg_wsb_map_points;
    ws->redraw_regions = phg_wsb_redraw_regions;

    ws->inq_view_indices = phg_wsb_inq_view_indices;
    ws->inq_bundle_indices = phg_wsx_inq_LUT_indices;
    ws->inq_posted = phg_wsb_inq_posted;
    ws->inq_representation = phg_wsb_inq_rep;
    ws->inq_view_rep = phg_wsb_inq_view_rep;
    ws->inq_ws_xform = phg_wsb_inq_ws_xform;
    ws->inq_disp_update_state = phg_wsb_inq_disp_update_state;
    ws->inq_filter = phg_wsb_inq_filter;
    ws->inq_hlhsr_mode = phg_wsb_inq_hlhsr_mode;
    ws->inq_colour_model = (void(*)())NULL;	/* have CP return it */
}

/* 
 * Tables that determine what update action is valid at a give point
 * in time.  The table has 3 axes:
 *	[Time] [Modification Mode] [Deferral Mode].
 */

static  Ws_action_table default_action_table =
{
    {   /* PHG_TIME_NOW */  
	{   /* NIVE */   
	    PHG_UPDATE_ACCURATE, 		/* ASAP */
	    PHG_UPDATE_IF_IG,	 		/* BNIG */
	    PHG_UPDATE_IF_IL, 			/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	},
	{   /* UWOR */
	    PHG_UPDATE_ACCURATE, 		/* ASAP */
	    PHG_UPDATE_IF_IG, 			/* BNIG */
	    PHG_UPDATE_IF_IL, 			/* BNIL */
	    PHG_UPDATE_UWOR, 			/* ASTI */
	    PHG_UPDATE_UWOR			/* WAIT */
	},
	{   /* UQUM */
	    PHG_UPDATE_ACCURATE, 		/* ASAP */
	    PHG_UPDATE_IF_IG, 			/* BNIG */
	    PHG_UPDATE_IF_IL, 			/* BNIL */
	    PHG_UPDATE_UQUM, 			/* ASTI */
	    PHG_UPDATE_UQUM			/* WAIT */
	}, 
    }, 

    {   /* PHG_TIME_BIG */  
	{   /* NIVE */   
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_ACCURATE, 		/* BNIG */
	    PHG_UPDATE_NOTHING, 		/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
	{   /* UWOR */ 
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_ACCURATE, 		/* BNIG */
	    PHG_UPDATE_NOTHING, 		/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
	{   /* UQUM */   
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_ACCURATE, 		/* BNIG */
	    PHG_UPDATE_NOTHING, 		/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
    }, 	    
    {   /* PHG_TIME_BIL */  
	{   /* NIVE */   
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_ACCURATE, 		/* BNIG */
	    PHG_UPDATE_ACCURATE, 		/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
	{   /* UWOR */ 
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_ACCURATE, 		/* BNIG */
	    PHG_UPDATE_ACCURATE, 		/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
	{   /* UQUM */   
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_ACCURATE, 		/* BNIG */
	    PHG_UPDATE_ACCURATE, 		/* BNIL */
	    PHG_UPDATE_NOTHING, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
    }, 
    {   /* PHG_TIME_ATI */  
	{   /* NIVE */   
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_IF_INCORRECT, 		/* BNIG */
	    PHG_UPDATE_IF_INCORRECT, 		/* BNIL */
	    PHG_UPDATE_ACCURATE, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	}, 
	{   /* UWOR */ 
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_IF_INCORRECT, 		/* BNIG */
	    PHG_UPDATE_IF_INCORRECT, 		/* BNIL */
	    PHG_UPDATE_ACCURATE, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	},
	{   /* UQUM */   
	    ASSURE_CORRECT, 			/* ASAP */
	    PHG_UPDATE_IF_INCORRECT, 		/* BNIG */
	    PHG_UPDATE_IF_INCORRECT, 		/* BNIL */
	    PHG_UPDATE_ACCURATE, 		/* ASTI */
	    PHG_UPDATE_NOTHING			/* WAIT */
	},
    }
};


static
void
init_update_state( ws, wst )
    Ws	*ws;
    Wst	*wst;
{   
    register Ws_output_ws	*ows = &ws->out_ws;
    register Wsb_output_ws	*owsb = &ows->model.b;

    ows->def_mode = ws->type->desc_tbl.phigs_dt.out_dt.deferral_mode;
    ows->mod_mode = ws->type->desc_tbl.phigs_dt.out_dt.modification_mode;

    owsb->update_action_table = (Ws_action_table_ptr)default_action_table;

    /* cache action for time "NOW" */
    owsb->now_action = (*owsb->update_action_table)
	[(int)PHG_TIME_NOW][(int)ows->mod_mode][(int)ows->def_mode];

    owsb->vis_rep = PVISUAL_ST_CORRECT;
    owsb->surf_state = PSURF_EMPTY;
}


static int
init_view_table( ws, wst )
    Ws	*ws;
    Wst	*wst;
{   
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    register Ws_view_entry	*view;
    register Pview_rep3		*predef_view;
    register int		i, max_pd_view;

    ALLOC_DECLARE(5);

    /* Allocate and initiailize the view table to correspond to the
     * predefined views in the PEX view LUT.  This code puts a limit on the
     * number of view entries that can be defined.  This limit may
     * be less than the server's limit.
     */
    owsb->num_views = ws->type->desc_tbl.phigs_dt.num_view_indices;

    if ( !ALLOCATED( owsb->views = (Ws_view_entry *)
	    malloc((unsigned)(owsb->num_views * sizeof(Ws_view_entry)))) ) {
	ERR_BUF( ws->erh, ERR900 );
	return 0;
    }

    owsb->top_view = 0;
    if ( !ALLOCATED( owsb->view_priorities = (Ws_view_priority *)
	    malloc((unsigned)(owsb->num_views * sizeof(Ws_view_priority)))) ) {
	ERR_BUF( ws->erh, ERR900 );
	ALLOC_FREE;
	return 0;
    }

    owsb->num_pending_views = 0;
    if ( !ALLOCATED( owsb->pending_views = (Ws_pending_view *)
	    malloc((unsigned)(owsb->num_views * sizeof(Ws_pending_view)))) ) {
	ERR_BUF( ws->erh, ERR900 );
	ALLOC_FREE;
	return 0;
    }

    max_pd_view = wst->desc_tbl.phigs_dt.num_predefined_views - 1;
    predef_view = wst->desc_tbl.phigs_dt.default_views;
    view = owsb->views;
    /* Load the predefined views. */
    for ( i = 0; i <= max_pd_view; i++, predef_view++, view++ ) {
	view->pending = PUPD_NOT_PEND;
	bcopy( (char *)predef_view->ori_matrix,(char *) view->vom,
	    sizeof(Pmatrix3) );
	bcopy( (char *)predef_view->map_matrix, (char *)view->vmm,
	    sizeof(Pmatrix3) );
	view->clip_limit = predef_view->clip_limit;
	view->xy_clip = predef_view->xy_clip;
	view->back_clip = predef_view->back_clip;
	view->front_clip = predef_view->front_clip;
	/* view->npc_to_wc not computed until needed in input code. */
	view->npc_to_wc_state = WS_INV_NOT_CURRENT;
    }

    /* Load the available but not predefined views. */
    for ( /* use existing index */ ; i < owsb->num_views; i++, view++ )
	*view = owsb->views[0];
    
    /* Initialize the view transformation priorities.  The list is
     * terminated at top and bottom by -1.
     */
    for ( i = 0; i < owsb->num_views; i++ ) {
	owsb->view_priorities[i].higher = i - 1;
	owsb->view_priorities[i].lower = i + 1;
    }
    owsb->view_priorities[owsb->num_views - 1].lower = -1;
    
    return 1;
}


static int
init_output_state( ws )
    Ws		*ws;
{
    Wst		*wst = ws->type;

    register Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    if ( !init_view_table( ws, wst ) )
	return 0;

    /* Initialize the workstation transform. */
    owsb->req_ws_window.x_min = 0.0;
    owsb->req_ws_window.x_max = 1.0;
    owsb->req_ws_window.y_min = 0.0;
    owsb->req_ws_window.y_max = 1.0;
    owsb->req_ws_window.z_min = 0.0;
    owsb->req_ws_window.z_max = 1.0;
    owsb->ws_window = owsb->req_ws_window;
    owsb->ws_window_pending = PUPD_NOT_PEND;

    owsb->req_ws_viewport.x_min = 0.0;
    owsb->req_ws_viewport.x_max = ws->ws_rect.width;
    owsb->req_ws_viewport.y_min = 0.0;
    owsb->req_ws_viewport.y_max = ws->ws_rect.height;
    owsb->req_ws_viewport.z_min = 0.0;
    owsb->req_ws_viewport.z_max = 1.0;
    owsb->ws_viewport = owsb->req_ws_viewport;
    owsb->ws_viewport_pending = PUPD_NOT_PEND;

    phg_wsx_compute_ws_transform( &owsb->ws_window, &owsb->ws_viewport,
	&owsb->ws_xform );

    /* Initialize the list of posted structs. */
    owsb->posted.lowest.higher = &owsb->posted.highest;
    owsb->posted.lowest.lower = NULL;
    owsb->posted.highest.higher = NULL;
    owsb->posted.highest.lower = &owsb->posted.lowest;

    /* Initialize other miscellaneous output state. */
    owsb->cur_hlhsr_mode = PHIGS_HLHSR_MODE_NONE;
    owsb->req_hlhsr_mode = PHIGS_HLHSR_MODE_NONE;
    owsb->hlhsr_mode_pending = PUPD_NOT_PEND;

    return 1;
}

static int
wsb_init_resources( ws )
    Ws		*ws;
{
    CARD16		count;
    CARD32		size, *rid_list;
    pexTableIndex	start;
    pexBitmask		pmask[PEXMSPipeline], rmask;
    CARD32		*card32_p;
    pexReflectionAttr	*refl_attr, *bf_refl_attr;
    pexFloatColour	*spec_colr, *bf_spec_colr;
    pexViewport		*vp;

    register int		i;
    register Ws_view_entry	*view;
    register pexViewEntry	*pex_view;

    /* Initialize all the predefined table entries from the WDT. */
    if ( !phg_wsx_init_LUTs( ws, 1 ) )
	return 0;

    /* Initialize the Pipeline Context.  Since PEX and PHIGS defaults
     * are different for reflection attrs and culling mode, set those in
     * PEX when creating the context.
     */
    size = 0;
    bzero((char *)pmask, sizeof(pmask));
    PEX_BITSET(pmask,PEXPCSurfaceReflAttr);
	size += sizeof(*refl_attr) + sizeof(*spec_colr);
    refl_attr = (pexReflectionAttr *)ws->scratch.buf;
    spec_colr = (pexFloatColour *)(refl_attr + 1);
    refl_attr->ambient = 1.0;
    refl_attr->diffuse = 1.0;
    refl_attr->specular = 1.0;
    refl_attr->specularConc = 0.0;
    refl_attr->transmission = 0.0;
    refl_attr->specularColour.colourType = PEXRgbFloatColour;
    spec_colr->first = spec_colr->second = spec_colr->third = 1.0;
    PEX_BITSET(pmask,PEXPCBfSurfaceReflAttr);
	size += sizeof(*bf_refl_attr) + sizeof(*bf_spec_colr);
    bf_refl_attr = (pexReflectionAttr *)(spec_colr + 1);
    *bf_refl_attr = *refl_attr;
    bf_spec_colr = (pexFloatColour *)(bf_refl_attr + 1);
    *bf_spec_colr = *spec_colr;
    PEX_BITSET(pmask,PEXPCCullingMode); size += sizeof(CARD32);
    card32_p = (CARD32 *)(bf_spec_colr + 1);
    *card32_p = 0;
    (void)PEXChangePipelineContext( ws->display, ws->out_ws.model.b.pipeline,
	pmask, size, (char *)ws->scratch.buf );

    /* Initialize the renderer. */
    size = 0;
    bzero((char *)&rmask, sizeof(rmask));
    rid_list = (XID *)ws->scratch.buf;
    /* The way the bits are defined in PEX.h doesn't allow the use of
     * PEX_BITSET().
     */
    rmask |= PEXRDPipelineContext; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.model.b.pipeline;
    rmask |= PEXRDMarkerBundle; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.marker;
    rmask |= PEXRDTextBundle; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.text;
    rmask |= PEXRDLineBundle; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.line;
    rmask |= PEXRDInteriorBundle; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.interior;
    rmask |= PEXRDEdgeBundle; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.edge;
    rmask |= PEXRDViewTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.view;
    rmask |= PEXRDColourTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.colour;
    rmask |= PEXRDDepthCueTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.depth_cue;
    rmask |= PEXRDLightTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.light_source;
    rmask |= PEXRDColourApproxTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.colour_approx;
    rmask |= PEXRDPatternTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.pattern;
    rmask |= PEXRDTextFontTable; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.lut.font;
    rmask |= PEXRDHighlightIncl; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.nset.hlt_incl;
    rmask |= PEXRDHighlightExcl; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.nset.hlt_excl;
    rmask |= PEXRDInvisibilityIncl; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.nset.invis_incl;
    rmask |= PEXRDInvisibilityExcl; size += sizeof(CARD32);
	*rid_list++ = ws->out_ws.nset.invis_excl;
    rmask |= PEXRDHlhsrMode; size += sizeof(CARD32);
	*rid_list++ = (CARD32)PEX_CONV_PHIGS_HLHSR_MODE(PHIGS_HLHSR_MODE_NONE);
    rmask |= PEXRDViewport; size += sizeof(pexViewport);
	vp = (pexViewport *)rid_list++;
	vp->minval.x = ws->out_ws.model.b.req_ws_viewport.x_min;
	vp->minval.y = ws->out_ws.model.b.req_ws_viewport.y_min;
	vp->minval.z = ws->out_ws.model.b.req_ws_viewport.z_min;
	vp->maxval.x = ws->out_ws.model.b.req_ws_viewport.x_max;
	vp->maxval.y = ws->out_ws.model.b.req_ws_viewport.y_max;
	vp->maxval.z = ws->out_ws.model.b.req_ws_viewport.z_max;
	vp->useDrawable = 0;
    /* No need to set the NPC volume since the PEX default is correct. */
    (void)PEXChangeRenderer( ws->display, ws->rid, rmask, size,
	(char *)ws->scratch.buf );

    return 1;
}

static int
wsb_create_resources( ws )
    Ws		*ws;
{
    pexBitmask		pmask[PEXMSPipeline];
    pexBitmask		dyn_tbls, dyn_nsets, dyn_attrs;
    Pint		err;
    CARD16		*card16_p;
    Drawable            draw;
    int                 numbufs, mbuf_event_base, mbuf_error_base;;


    /* Create the LUTs, renderer and pipeline context. */
    if ( !phg_wsx_create_LUTs( ws, 1, &err ) ) {
	ERR_BUF( ws->erh, err );
	return 0;
    }

    /* Create the pipeline context. */
    ws->out_ws.model.b.pipeline = XAllocID(ws->display);
    (void)PEXCreatePipelineContext( ws->display, ws->out_ws.model.b.pipeline,
	(pexBitmask *)NULL, (CARD32)0, (char *)NULL );

    /* Make an inquiry to see if the create worked. */
    /* TODO: Remove when GetPipeline fixed.
    bzero((char *)pmask, sizeof(pmask));
    PEX_BITSET(pmask,PEXPCCullingMode);
    if ( !PEXGetPipelineContext( ws->display, ws->out_ws.model.b.pipeline,
	    pmask, (char **)&card16_p ) ) {
	ERR_BUF( ws->erh, ERRN202 );
	return 0;
    }
    */

    /* Check the Workstation buffer mode and do the double buffer
       create here if necessary This is simpler than on the workstation
       since this PHIGS implementation only lets the Double Buffers
       be created when the workstation is created, so no change action
       need be done
    */

    /* initialize the local drawable variable */
    draw = ws->drawable_id;
    ws->out_ws.model.b.has_double_buffer = FALSE;

    if (ws->type->desc_tbl.xwin_dt.buffer_mode == PHIGS_BUF_DOUBLE) {
      if (XmbufQueryExtension(ws->display, &mbuf_event_base, &mbuf_error_base)){

        /* the Multi-Buffer extension is there */
        numbufs = XmbufCreateBuffers(ws->display, ws->drawable_id, 2,
                     MultibufferUpdateActionBackground,
                     MultibufferUpdateHintFrequent,
                     &ws->out_ws.model.b.double_drawable[0]);
        if  (numbufs == 2) {
          /* got the buffers OK */
          ws->out_ws.model.b.front = 0;
          ws->out_ws.model.b.back = 1;
          ws->out_ws.model.b.has_double_buffer = TRUE;
          draw = ws->out_ws.model.b.double_drawable[1];
          /* this isn't implemented yet
            XmbufClearBufferArea(ws->display, draw, 0, 0, 0, 0, False);
          */
        } else
          /* buffers didn't get created correctly, bag them  */
          XmbufDestroyBuffers(ws->display, ws->drawable_id);
      }
    }


    /* Create the renderer, initializing the necessary attributes. */
    ws->rid = XAllocID(ws->display);
    (void)PEXCreateRenderer( ws->display, ws->rid, draw,
	(pexBitmask)0, (CARD32)0, (char *)NULL );

    /* Make an inquiry to see if the create worked. */
    if ( !PEXGetRendererDynamics( ws->display, ws->rid, &dyn_tbls,
	    &dyn_nsets, &dyn_attrs ) ) {
	ERR_BUF( ws->erh, ERRN202 );
	return 0;
    }

    return 1;
}


static void
wsb_update_wdt( ws )
    Ws		*ws;
{
    /* Update the dynamics flags -- everything IRG for client-side WSs. */
    ws->type->desc_tbl.phigs_dt.out_dt.view_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.polyline_bundle_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.polymarker_bundle_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.text_bundle_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.interior_bundle_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.edge_bundle_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.pattern_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.colour_rep = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.ws_xform = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.highlight_filter = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.invis_filter = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.hlhsr_mode = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.struct_content_mod = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.post = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.unpost = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.struct_delete = PDYN_IRG;
    ws->type->desc_tbl.phigs_dt.out_dt.ref_mod = PDYN_IRG;
}


Ws*
phg_wsb_open_ws( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    void		wsb_destroy_ws();
    Phg_args_open_ws	*args = &cp_args->data.open_ws;
    char		*avlist[4];
    Ws			*ws;
    XWindowAttributes	wattr;
    Phg_pex_ext_info	pex_info;

    ret->err = -1;
    if ( !(ws = phg_wsx_create( cph, args, css_srvr )) )
	return ws;

    switch ( args->type->base_type ) {
	case WST_BASE_TYPE_X_DRAWABLE:
	    ws->display = args->conn_info.display;
	    ws->drawable_id = args->conn_info.drawable_id;
	    phg_cpx_instance_connection( cph, ws->display, 0 );
	    break;

	case WST_BASE_TYPE_X_TOOL:
	    /* Get a display connection for it. */
	    if ( (ws->display = phg_cpx_connection_exists( cph, CPX_BY_NAME,
		    args->conn_info.display_name )) ) {
		/* We know PEX is supported on this display because it's in
		 * the CPs connection list.
		 */
		phg_cpx_instance_connection( cph, ws->display, 0 );
	    } else if ( ws->display = phg_utx_open_pex_display(
		    args->conn_info.display_name, &pex_info, &ret->err ) ) {
		phg_cpx_instance_connection( cph, ws->display, 1 );
	    } else {
		ERR_BUF( ws->erh, ret->err );
		goto abort;
	    }

	    /* Create the window. */
	    if ( !phg_wsx_setup_tool( ws, &args->conn_info, args->type ) )
		goto abort;
	    break;
    }

    (void)XGetWindowAttributes( ws->display, ws->drawable_id, &wattr );
    WS_SET_WS_RECT( ws, &wattr )

    /* Build an accurate WDT after the window is open. */
    avlist[0] = PHIGS_X_DISPLAY_WINDOW;
	avlist[1] = (char *)ws->display;
	avlist[2] = (char *)ws->drawable_id;
    avlist[3] = (char *)0;
    if ( ws->type = phg_wst_create( cph->erh, args->type, avlist ) ) {
	ws->type->wsid = ws->id;
	ws->type->bound_status = WST_BOUND;
	wsb_update_wdt( ws );
    } else
	goto abort;

    ws->current_colour_model =
	ws->type->desc_tbl.phigs_dt.out_dt.default_colour_model;
    ws->category = ws->type->desc_tbl.phigs_dt.ws_category;
    ws->out_ws.model.b.cssh = css_srvr->model.b.cssh;
    if ( !init_output_state( ws ) )
	goto abort;
    init_update_state( ws, ws->type );

    /* Create and initialize all the PEX resources. */
    if ( !wsb_create_resources( ws ) )
	goto abort;
    if ( !wsb_init_resources( ws ) )
	goto abort;

    if ( !phg_wsx_setup_colormap( ws, &ret->err ) ) {
	ERR_BUF(ws->erh, ret->err);
	goto abort;
    }

    wsb_load_funcs( ws );

    /* Fill in the WDT fields that depend on an open workstaton. */
    ws->type->desc_tbl.phigs_dt.out_dt.num_display_priorities = 0;
    ws->type->desc_tbl.phigs_dt.dev_coords[0] = ws->ws_rect.width;
    ws->type->desc_tbl.phigs_dt.dev_coords[1] = ws->ws_rect.height;
    ws->type->desc_tbl.phigs_dt.dev_addrs_units[0] = ws->ws_rect.width;
    ws->type->desc_tbl.phigs_dt.dev_addrs_units[1] = ws->ws_rect.height;

    /* Fill in the return data. */
    ret->err = 0;
    ret->data.open_ws.wstype = ws->type;
    ret->data.open_ws.wst_buffer = ws->type->buffer;
    ret->data.open_ws.wst_buffer_size = ws->type->buffer_size;
    ret->data.open_ws.drawable_id = ws->drawable_id;
    ret->data.open_ws.overlay_id = ws->input_overlay_window;

    return ws;

abort:
    wsb_destroy_ws( ws );
    return (Ws *)NULL;
}


void
wsb_free_all_posted( owsb )
    Wsb_output_ws	*owsb;
{
    register	Ws_post_str	*cur, *end;

    cur = owsb->posted.lowest.higher;
    end = &owsb->posted.highest;
    while ( cur != end ) {
	cur = cur->higher;
	free( (char *)cur->lower );
    }
    owsb->posted.lowest.higher = end;
    end->lower = &owsb->posted.lowest;
}


void
wsb_destroy_ws( ws )
    Ws		*ws;
{
    if ( ws ) {
	if ( ws->out_ws.model.b.views )
	    free( (char *)ws->out_ws.model.b.views );
	if ( ws->out_ws.model.b.pending_views )
	    free( (char *)ws->out_ws.model.b.pending_views );
	if ( ws->out_ws.model.b.view_priorities )
	    free( (char *)ws->out_ws.model.b.view_priorities );
	if ( ws->display ) {
	    if ( ws->drawable_id )
		phg_wsx_release_window( ws );
	    if ( ws->out_ws.model.b.pipeline )
		PEXFreePipelineContext( ws->display,
		    ws->out_ws.model.b.pipeline );
	    if ( ws->rid )
		PEXFreeRenderer( ws->display, ws->rid );
	    phg_wsx_destroy_LUTs( ws );
	    XFlush( ws->display );
	    phg_cpx_release_connection( ws->cph, ws->display );
	}
	phg_wsx_destroy( ws );
    }
}


void
phg_wsb_close_ws( ws )
    Ws		*ws;
{
    if ( ws ) {
	wsb_free_all_posted( &ws->out_ws.model.b );
	wsb_destroy_ws( ws );
    }
}


void
phg_wsb_redraw_all( ws, clear_control )
    Ws		*ws;
    Pctrl_flag	clear_control;
{
    (*ws->make_requested_current)( ws );
    (*ws->repaint_all)( ws, clear_control, 0, (XRectangle *)NULL );
    ws->out_ws.model.b.vis_rep = PVISUAL_ST_CORRECT;
}



/* Make all "requested" and pending data current. */
void
phg_wsb_make_requested_current( ws )
    Ws		*ws;
{
    Ws_view_entry	*view;
    Ws_pending_view	*req_view;
    pexViewEntry	pex_view;
    pexViewport		vp;
    pexNpcSubvolume	win;

    register Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    /* WS transform */
    if ( owsb->ws_window_pending == PUPD_PEND
	    || owsb->ws_viewport_pending == PUPD_PEND ) {
	if ( owsb->ws_window_pending == PUPD_PEND ) {
	    owsb->ws_window = owsb->req_ws_window;
	    owsb->ws_window_pending = PUPD_NOT_PEND;
	    win.minval.x = owsb->ws_window.x_min;
	    win.minval.y = owsb->ws_window.y_min;
	    win.minval.z = owsb->ws_window.z_min;
	    win.maxval.x = owsb->ws_window.x_max;
	    win.maxval.y = owsb->ws_window.y_max;
	    win.maxval.z = owsb->ws_window.z_max;
	    (void)PEXChangeRenderer( ws->display, ws->rid,
		(pexBitmask)PEXRDNpcSubvolume,
		(CARD32)sizeof(pexNpcSubvolume), (char *)&win );
	}

	if ( owsb->ws_viewport_pending == PUPD_PEND ) {
	    owsb->ws_viewport = owsb->req_ws_viewport;
	    owsb->ws_viewport_pending = PUPD_NOT_PEND;
	    vp.minval.x = owsb->ws_viewport.x_min;
	    vp.minval.y = owsb->ws_viewport.y_min;
	    vp.minval.z = owsb->ws_viewport.z_min;
	    vp.maxval.x = owsb->ws_viewport.x_max;
	    vp.maxval.y = owsb->ws_viewport.y_max;
	    vp.maxval.z = owsb->ws_viewport.z_max;
	    vp.useDrawable = 0;
	    (void)PEXChangeRenderer( ws->display, ws->rid,
		(pexBitmask)PEXRDViewport,
		(CARD32)sizeof(pexViewport), (char *)&vp );
	}

	phg_wsx_compute_ws_transform( &owsb->ws_window, &owsb->ws_viewport,
	    &owsb->ws_xform );
    }

    /* View table */
    if ( owsb->num_pending_views > 0 ) {
	req_view = owsb->pending_views;
	while ( owsb->num_pending_views > 0 ) {
	    view = &owsb->views[req_view->id];
	    /*Set it locally. */
	    view->pending = PUPD_NOT_PEND;
	    bcopy( (char *)req_view->view.ori_matrix, (char *)view->vom,
		sizeof(Pmatrix3) );
	    bcopy( (char *)req_view->view.map_matrix, (char *)view->vmm,
		sizeof(Pmatrix3) );
	    view->clip_limit = req_view->view.clip_limit;
	    view->xy_clip = req_view->view.xy_clip;
	    view->back_clip = req_view->view.back_clip;
	    view->front_clip = req_view->view.front_clip;
	    view->npc_to_wc_state = WS_INV_NOT_CURRENT;

	    /* Set it in the server.  Can't set them all as a block with one
	     * request because the pending views may not be contiguous.
	     */
	    (void)phg_utx_view_entry_to_pex( &req_view->view, &pex_view );
	    (void)PEXSetTableEntries( ws->display, ws->out_ws.lut.view,
		(pexTableIndex)req_view->id, (CARD16)1,
		(CARD32)sizeof(pex_view), (char *)&pex_view );
	    ++req_view;
	    --owsb->num_pending_views;
	}
    }

    /* Other pending data */
    if ( owsb->hlhsr_mode_pending == PUPD_PEND) {
	 CARD32     new_mode;

	 owsb->cur_hlhsr_mode = owsb->req_hlhsr_mode;
	 owsb->hlhsr_mode_pending = PUPD_NOT_PEND;

	 new_mode = (CARD32)PEX_CONV_PHIGS_HLHSR_MODE(owsb->cur_hlhsr_mode);
         (void)PEXChangeRenderer( ws->display, ws->rid,
                                 (pexBitmask)PEXRDHlhsrMode,
                                 (CARD32)sizeof(CARD32), (char *)&new_mode);

    }

    /* Make it all take effect. */
    XFlush( ws->display );
}


void
phg_wsb_repaint_all( ws, clear_control, num_rects, exposure_rects )
    Ws		*ws;
    Pctrl_flag	clear_control;
    int		num_rects;		/* may be 0 */
    XRectangle	*exposure_rects;	/* may be NULL */
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    int                 tmp;
    Drawable            draw;

    register int	i;

    /* assuming this stuff does clear then don't do it when using Double Buff
       since the swap will take care of that
    */
    if ((clear_control == PFLAG_ALWAYS || owsb->surf_state == PSURF_NOT_EMPTY)
       && !ws->out_ws.model.b.has_double_buffer) {
	/* TODO: Need a way to "clear" the window that uses the zero-th
	 * entry in the WS colour table and runs it through colour mapping.
	 */
	if ( num_rects > 0 ) {
	    for ( i = 0; i < num_rects; i++ )
		XClearArea( ws->display, ws->drawable_id,
		    exposure_rects[i].x, exposure_rects[i].x,
		    exposure_rects[i].width, exposure_rects[i].height,
		    False );
	} else
	    XClearWindow( ws->display, ws->drawable_id );
    }
    owsb->surf_state = PSURF_EMPTY;

    /* set the drawable correctly */
    if (ws->out_ws.model.b.has_double_buffer)
        draw = ws->out_ws.model.b.double_drawable[ws->out_ws.model.b.back];
    else  /* single buffer */
        draw = ws->drawable_id;

    phg_wsb_traverse_all_postings( ws, draw );

    /* now swap the buffers and update the drawable indices */
    if (ws->out_ws.model.b.has_double_buffer) {
          XmbufDisplayBuffers(ws->display, 1, &draw, 0, 0);
          tmp = ws->out_ws.model.b.front;
          ws->out_ws.model.b.front  = ws->out_ws.model.b.back;
          ws->out_ws.model.b.back = tmp;
    }

    /* Redraw input prompts & echos of any active input devices. */
    if ( ws->input_repaint && WS_ANY_INP_DEV_ACTIVE(ws) )
	(ws->input_repaint)( ws, num_rects, exposure_rects );

    XFlush( ws->display );
}


void
phg_wsb_traverse_all_postings( ws, draw )
    Ws		*ws;
    Drawable            draw;
{
    register Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    register Ws_post_str	*post_str, *end;

    WSB_CHECK_POSTED(&owsb->posted)
    if( WSB_SOME_POSTED(&owsb->posted) ) {
	/* Set up for complete traversal. */
	post_str = owsb->posted.lowest.higher;
	end = &(owsb->posted.highest);
	PEXBeginRendering( ws->display, ws->rid, draw);
	while ( post_str != end ) {
	    phg_wsb_traverse_net( ws, post_str->structh );
	    post_str = post_str->higher;
	}
	PEXEndRendering( ws->display, ws->rid, PEXOn );
	XFlush( ws->display );
	owsb->surf_state = PSURF_NOT_EMPTY;
    }
}


void
phg_wsb_traverse_net( ws, structp )
    Ws_handle		ws;
    Struct_handle	structp;
{
    register	El_handle	el;

    PEXBeginStructure( ws->display, ws->rid, (CARD32)structp->struct_id );
    el = structp->first_el;
    while ( 1 ) {	/* termination test is at the bottom */
	switch ( el->eltype ) {
	    case PELEM_NIL:
		break;
	    case PELEM_EXEC_STRUCT:
		phg_wsb_traverse_net( ws, (Struct_handle)el->eldata.ptr );
		break;
	    default:
		PEXRenderOutputCommands( ws->display, ws->rid, (CARD32)1,
		    (CARD32)(((pexElementInfo *)el->eldata.ptr)->length
			* sizeof(CARD32)),
		    (char *)el->eldata.ptr );
		break;
	}

	if ( el == structp->last_el )
	    break;  /* out of the while over all elements in struct */
	el = el->next;
    }
    PEXEndStructure( ws->display, ws->rid );
}


static int
wsb_visible_element_type( el )
    El_handle	el;
{
    int		status = 1;	/* almost all are */

    switch ( el->eltype ) {
	case PELEM_APPL_DATA:
	case PELEM_LABEL:
	case PELEM_PICK_ID:
	    status = 0;
	    break;
    }

    return status;
}

void
phg_wsb_add_el( ws )
    Ws	*ws;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    El_handle		cur_el = CSS_CUR_ELP(owsb->cssh);

    assure(CSS_CUR_STRUCTP(owsb->cssh)); /* A structure must be open */
    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	default:
	    if ( wsb_visible_element_type( cur_el ) )
		(*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


int
phg_wsb_asti_update( ws, clear_control )
    Ws		*ws;
    Pctrl_flag	clear_control;
{
    /* Returns non-zero if redraw occurred. */

    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    switch ( (*owsb->update_action_table)
	    [(int)PHG_TIME_ATI]
	    [(int)ws->out_ws.mod_mode]
	    [(int)ws->out_ws.def_mode] ) {
	case PHG_UPDATE_IF_INCORRECT:
	case PHG_UPDATE_IF_IL:
	case PHG_UPDATE_IF_IG:
	    if ( owsb->vis_rep == PVISUAL_ST_CORRECT )
		break;
	    /* else fall through to PHG_UPDATE_ACCURATE case */
	case PHG_UPDATE_ACCURATE:
	    (*ws->redraw_all)( ws, clear_control );
	    return 1;

	case PHG_UPDATE_UQUM:
	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	    break;
    }

    return 0;
}

void
phg_wsb_close_struct( ws, structh )
    Ws			*ws;
    Struct_handle	structh;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    /* First, do processing that is independent of screen output */
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	default:
	    break;
    }

    /* Updates are implementation dependent in ASTI mode.  This is one
     * of the cases where we do an ASTI update;  we're hopefully doing the
     * application a favor.
     */
    (void)phg_wsb_asti_update( ws, PFLAG_COND );
}


static void
wsb_update_a_posting( ws, posting )
    Ws			*ws;
    Ws_post_str 	*posting;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


void
phg_wsb_post( ws, structh, priority, first_posting )
    Ws			*ws;
    Struct_handle	structh;
    Pfloat		priority;
    int			first_posting;	/* 0 if already posted */
{
    Ws_post_str 	*start;

    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    register 	Ws_post_str 	*cur, *end;
    register 	Ws_post_str 	*new;


    if ( !first_posting ) {
	/* Check to see if structure is already posted. */
	cur = owsb->posted.lowest.higher;
	end = &owsb->posted.highest;
	while ( cur != end && cur->structh != structh )
	    cur = cur->higher;
    }

    /* The structure is already_posted if (cur != end). */
    if ( !first_posting && cur != end ) {
	if( cur->higher != end && priority >= cur->higher->disp_pri ) {
	    start = end->lower;
	    assure(start == owsb->posted.highest.lower);
	    end = cur->higher;	/* insert betw. cur->higher & posted.highest */
	} else if ( cur->lower != &owsb->posted.lowest
		&& priority < cur->lower->disp_pri ) {
	    /* Will insert between start and cur->lower. */
	    start = cur->lower;
	    end = &owsb->posted.lowest;
	} else {
	    /* This is a reposting with the same *relative* prio. */
	    cur->disp_pri = priority;
	    return;
	}

	/* Struct is posted.  Remove it, but re-use its Ws_post_str entry */
	cur->lower->higher = cur->higher;
	cur->higher->lower = cur->lower;
	new = cur;

    } else {
	/* Struct is not currently posted, malloc an element. */
	if ( !(new = (Ws_post_str *)malloc(sizeof(Ws_post_str))) ) {
	    ERR_BUF( ws->erh, ERR900);
	    return;
	}
	start = owsb->posted.highest.lower;
	end = &owsb->posted.lowest;
    }

    /* Now figure out where to insert it, working backwards from start
     * to end
     */
    cur = start;
    while ( cur != end && cur->disp_pri > priority )
	cur = cur->lower;	/* if priorities equal, new after cur */
    /* insert new element w/prio >= than cur's, so cur->higher will be new */
    new->lower = cur;
    new->higher = cur->higher;
    cur->higher = new;
    new->higher->lower = new;
    new->structh = structh;
    new->disp_pri = priority;

    if ( structh->num_el != 0 )
	wsb_update_a_posting( ws, new );
}


/* This function only called from the css for change struct ids/refs -
 * it is used to change the structure pointers referencing a given
 * structure, because the way the change struct ids/refs functions work
 * is by changing struct_id fields rather than copying whole structures,
 * so any lists using structure pointers to reference specific structures
 * have to be changed to use the correct pointers.
 */

void
phg_wsb_change_posting( ws, unpost, post )
    Ws			*ws;
    Struct_handle	unpost, post;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    register 	Ws_post_str 	*cur, *end;

    cur = owsb->posted.lowest.higher;
    end = &owsb->posted.highest;
    while ( cur != end && cur->structh != unpost )
	cur = cur->higher;

    if ( cur != end ) {
	if ( post ) {
	    /* if the structure to be "posted" is already posted, remove it */
	    phg_wsb_change_posting( ws, post, (Struct_handle)NULL );
	    /* Change posted structure from "unpost" to "post", same priority*/
	    cur->structh = post;
	} else {
	    /* Post is NULL - just remove Ws_post_str entry for unpost. */
	    cur->lower->higher = cur->higher;
	    cur->higher->lower = cur->lower;
	    free( (char *)cur );
	}
    }
}


/* Search the list of posted structures for this one.
 * If found, return pointer to next-higher-priority posted structure element.
 * (Remember that that could be the dummy element owsb->posted.highest)
 * If not, return NULL.
 */
static Ws_post_str*
wsb_unpost_struct_if_found( owsb, structh )
    Wsb_output_ws	*owsb;
    Struct_handle	 structh;
{
    register 	Ws_post_str	*cur, *end;

    cur = owsb->posted.lowest.higher;
    end = &owsb->posted.highest;
    while ( cur != end && cur->structh != structh )
	cur = cur->higher;
    if ( cur != end ) {
	/* Found it -- now delete it */
	cur->lower->higher = cur->higher;
	cur->higher->lower = cur->lower;
	end = cur->higher;	/* Save this around the free */
	free( (char *)cur );
	return end;
    } else
	return (Ws_post_str*)NULL;
}


void
phg_wsb_unpost( ws, structh )
    Ws			*ws;
    Struct_handle	structh;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    if ( !wsb_unpost_struct_if_found( owsb, structh ) )
	/* Tried to unpost structure that wasn't there; but that's okay. */
	return;

    if ( structh->num_el != 0 ) {
	WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
	switch ( owsb->now_action ) {
	    case_PHG_UPDATE_ACCURATE_or_IF_Ix:
		(*ws->redraw_all)( ws, PFLAG_COND );
		break;

	    case PHG_UPDATE_UWOR:
	    case PHG_UPDATE_NOTHING:
	    case PHG_UPDATE_UQUM:
		owsb->vis_rep = PVISUAL_ST_DEFER;
		break;
	}
    }
}


void
phg_wsb_unpost_all( ws )
    Ws    *ws;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    wsb_free_all_posted( owsb );
    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


void
phg_wsb_delete_all_structs( ws )
    Ws		*ws;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    phg_wsb_unpost_all( ws );
}


int
phg_wsb_delete_struct( ws, structh, flag )
    Ws			*ws;
    Struct_handle	structh;
    Ws_delete_flag	flag;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    int			call_again = 0;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    if ( flag == WS_PRE_CSS_DELETE ) {
		(void)wsb_unpost_struct_if_found( owsb, structh );
		call_again = 1;
	    } else
		(*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	    (void)wsb_unpost_struct_if_found( owsb, structh );
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }

    return call_again;
}


int
phg_wsb_delete_struct_net( ws, structh, reff, flag )
    Ws			*ws;
    Struct_handle	 structh;
    Pref_flag		 reff;
    Ws_delete_flag	 flag;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    int			call_again = 0;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    if ( flag == WS_PRE_CSS_DELETE ) {
		(void)wsb_unpost_struct_if_found( owsb, structh );
		call_again = 1;
	    } else
		(*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	default:
	    (void)wsb_unpost_struct_if_found( owsb, structh );
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
    return call_again;
}


void
phg_wsb_copy_struct( ws, first_el )
    Ws		*ws;
    El_handle	first_el;
{
    (*ws->conditional_redraw)( ws );
}


/* Delete elements elh1 through elh2, inclusive, in structure structh */
int
phg_wsb_delete_el( ws, structh, elh1, elh2, flag )
    Ws			*ws;
    Struct_handle	structh;
    El_handle		elh1, elh2;
    Ws_delete_flag	flag;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    int			call_again = 0;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	default:
	    if ( flag == WS_PRE_CSS_DELETE ) {
		if ( elh1 == elh2 && !wsb_visible_element_type( elh1 ) )
		    call_again = 0; /* avoid second call. */
		else
		    call_again = 1;
	    } else		/* POST_CSS_DELETE */
		(*ws->redraw_all)(ws, PFLAG_COND);
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	case PHG_UPDATE_UQUM:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }

    return call_again;
}


/* Called by CP after difficult operations like change struct refs/ids.
 * Redraws workstation, if that is permitted, else DEFERs.
 */
void
phg_wsb_conditional_redraw( ws )
Ws		*ws;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_ALWAYS );
	    break;

	case PHG_UPDATE_UQUM:
	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


/* Resolves PHG_UPDATE_IF_Ix to the ASTI now_action, if no input device active.
 *
 * Check this function before every (out_ws->now_action) use.
 * Then use case_PHG_UPDATE_ACCURATE_or_IF_Ix in switch (out_ws->now_action),
 * so that PHG_UPDATE_IF_Ix acts as PHG_UPDATE_ACCURATE (i.e., ASAP)
 * while appicable input devices are (still) active.
 */
void
phg_wsb_resolve_now_action( ws, now_action_ptr )
    Ws			*ws;
    Ws_update_action	*now_action_ptr;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    switch ( *now_action_ptr ) {
      case PHG_UPDATE_IF_IL:
	/* If none active, treat like ASTI until next bnig_update(ws,ws) */
	if ( !WS_ANY_INP_DEV_ACTIVE(ws) ) {
	    *now_action_ptr = (*owsb->update_action_table)
		[(int)PHG_TIME_NOW][(int)ws->out_ws.mod_mode][(int)PDEFER_ASTI];
	}
	break;

      case PHG_UPDATE_IF_IG:
	/* If none active, treat like ASTI until next bnig_update(ws,*) */
	if ( !phg_cp_any_inp_device_active( ws->cph ) ) {
	    *now_action_ptr = (*owsb->update_action_table)
		[(int)PHG_TIME_NOW][(int)ws->out_ws.mod_mode][(int)PDEFER_ASTI];
	}
	break;
    }
}


void
phg_wsb_update( ws, flag )
    Ws		*ws;
    Pregen_flag	flag;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    if ( flag != PFLAG_POSTPONE && owsb->vis_rep != PVISUAL_ST_CORRECT ) 
	(*ws->redraw_all)( ws, PFLAG_COND );
    else
	(*ws->make_requested_current)( ws );
}


void
phg_wsb_set_disp_update_state( ws, def_mode, mod_mode )
    Ws		*ws;
    Pdefer_mode	def_mode;
    Pmod_mode	mod_mode;
{
    Ws_update_action	previous_now_action;

    register	Ws_output_ws	*out_ws = &ws->out_ws;
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    out_ws->def_mode = def_mode;
    out_ws->mod_mode = mod_mode;

    previous_now_action = owsb->now_action;
    owsb->now_action = (*owsb->update_action_table)
	[(int)PHG_TIME_NOW][(int)out_ws->mod_mode][(int)out_ws->def_mode];
    assure(owsb->now_action != PHG_UPDATE_IF_INCORRECT);

    if ( owsb->now_action != previous_now_action ) {
	WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
	switch ( owsb->now_action ) {
	    case_PHG_UPDATE_ACCURATE_or_IF_Ix:
		if( owsb->vis_rep != PVISUAL_ST_CORRECT )
		    (*ws->redraw_all)( ws, PFLAG_COND );
		break;
	}
    }
}


void
phg_wsb_set_hlhsr_mode( ws, mode )
    Ws		*ws;
    Pint	mode;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    if( ws->type->desc_tbl.phigs_dt.num_hlhsr_modes == 1 )
	return; /* No need to update if the user asks for what he has! */

    owsb->req_hlhsr_mode = mode;
    owsb->hlhsr_mode_pending = PUPD_PEND;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_UQUM:
	case PHG_UPDATE_NOTHING:
	default:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


void
phg_wsb_set_ws_window( ws, two_d, limits )
    Ws		*ws;
    int		two_d;
    Plimit3	*limits;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    owsb->ws_window_pending = PUPD_PEND;
    if ( two_d ) {	/* leave the z values as they are */
	owsb->req_ws_window.x_min = limits->x_min;
	owsb->req_ws_window.x_max = limits->x_max;
	owsb->req_ws_window.y_min = limits->y_min;
	owsb->req_ws_window.y_max = limits->y_max;
    } else
	owsb->req_ws_window = *limits;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UQUM:
	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	default:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


void
phg_wsb_set_ws_vp( ws, two_d, limits )
    Ws		*ws;
    int		two_d;
    Plimit3	*limits;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    owsb->ws_viewport_pending = PUPD_PEND;
    if ( two_d ) {	/* leave the z values as they are */
	owsb->req_ws_viewport.x_min = limits->x_min;
	owsb->req_ws_viewport.x_max = limits->x_max;
	owsb->req_ws_viewport.y_min = limits->y_min;
	owsb->req_ws_viewport.y_max = limits->y_max;
    } else
	owsb->req_ws_viewport = *limits;

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	case PHG_UPDATE_UQUM:
	case PHG_UPDATE_UWOR:
	case PHG_UPDATE_NOTHING:
	default:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}


void
phg_wsb_set_view_input_priority( ws, index, ref_index, priority )
    Ws			*ws;
    register Pint	index;
    Pint		ref_index;
    Prel_pri		priority;
{
    Pint			old;

    register	Ws_view_priority	*ref, *idx;
    register	Pint			*highest;
    register	Ws_view_priority	*vp;

    vp = ws->out_ws.model.b.view_priorities;
    highest = &ws->out_ws.model.b.top_view;
    idx = &vp[index];
    ref = &vp[ref_index];

    if ( priority == PPRI_LOWER ) {
	if ( ref->lower != index ) {
	    if ( index == *highest)
		*highest = idx->lower;

	    old = ref->lower;
	    if ( ref->lower != -1 )	/* if ref not lowest priority */
		vp[ref->lower].higher = index;
	    ref->lower = index;

	    if ( idx->higher != -1 )	 /* if idx not highest priority */
		vp[idx->higher].lower = idx->lower;
	    if ( idx->lower != -1 )	 /* if idx not lowest priority */
		vp[idx->lower].higher = idx->higher;
	    idx->higher = ref_index;
	    idx->lower = old;
	}
	/* Don't need to do anything if priority is already as desired. */

    } else if ( ref->higher != index ) {	/* PPRI_HIGHER */
	if ( ref_index == *highest )
	    *highest = index;
	else if ( index == *highest )
	    *highest = idx->lower;

	old = ref->higher;
	if ( ref->higher != -1 )
	    vp[ref->higher].lower = index;
	ref->higher = index;

	if ( idx->higher != -1 )
	    vp[idx->higher].lower = idx->lower;
	if ( idx->lower != -1 )
	    vp[idx->lower].higher = idx->higher;
	idx->lower = ref_index;
	idx->higher = old;
    }

    /* Has no effect on the screen */
}


void
phg_wsb_set_rep( ws, type, rep )
    Ws			*ws;
    Phg_args_rep_type	type;
    Phg_args_rep_data	*rep;
{
    register	Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    register	int		i;

    switch ( type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	case PHG_ARGS_DCUEREP:
	case PHG_ARGS_LIGHTSRCREP:
	case PHG_ARGS_COLRMAPREP:
	    phg_wsx_set_LUT_entry( ws, type, rep, (Pgcolr*)NULL );
	    break;

	case PHG_ARGS_VIEWREP:
	    /* Add it to the list of pending views. */
	    if ( owsb->views[rep->index].pending == PUPD_NOT_PEND )
		i = owsb->num_pending_views++;
	    else {
		/* Find the existing pending entry so it can be replaced. */
		for ( i = 0; i < owsb->num_pending_views; i++ )
		    if ( owsb->pending_views[i].id == rep->index )
			break;
	    }
	    owsb->pending_views[i].id = rep->index;
	    owsb->pending_views[i].view = rep->bundl.viewrep;
	    owsb->views[rep->index].pending = PUPD_PEND;
	    break;

	case PHG_ARGS_COREP: {
	    Pgcolr	gcolr;

	    /* Store in current colour model. */
	    gcolr.type = ws->current_colour_model;
	    gcolr.val.general.x = rep->bundl.corep.rgb.red;
	    gcolr.val.general.y = rep->bundl.corep.rgb.green;
	    gcolr.val.general.z = rep->bundl.corep.rgb.blue;
	    phg_wsx_set_LUT_entry( ws, type, rep, &gcolr );
	    } break;
    }

    WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
    switch ( owsb->now_action ) {
	case_PHG_UPDATE_ACCURATE_or_IF_Ix:
	    (*ws->redraw_all)( ws, PFLAG_COND );
	    break;

	default:
	case PHG_UPDATE_UQUM:
	case PHG_UPDATE_UWOR:
	    owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;

	case PHG_UPDATE_NOTHING:
	    /* Defer if rep has PENDING flag, or if screen could be affected. */
	    if ( WSB_SOME_POSTED(&owsb->posted) || (type == PHG_ARGS_VIEWREP) )
		 owsb->vis_rep = PVISUAL_ST_DEFER;
	    break;
    }
}

void
phg_wsb_set_filter( ws, type, devid, inc_set, exc_set )
    Ws			*ws;
    Phg_args_flt_type	type;
    Pint		devid;
    Pint_list		*inc_set;
    Pint_list		*exc_set;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    phg_wsx_set_name_set( ws, type, devid, inc_set, exc_set );

    if ( (type == PHG_ARGS_FLT_HIGH || type == PHG_ARGS_FLT_INVIS)
	    && WSB_SOME_POSTED(&owsb->posted) ) {
	WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &owsb->now_action)
	switch ( owsb->now_action ) {
	    case_PHG_UPDATE_ACCURATE_or_IF_Ix:
		(*ws->redraw_all)( ws, PFLAG_COND );
		break;

	    case PHG_UPDATE_UQUM:
	    case PHG_UPDATE_UWOR:
	    case PHG_UPDATE_NOTHING:
	    default:
		owsb->vis_rep = PVISUAL_ST_DEFER;
		break;
	}
    }
}


void
phg_wsb_inq_view_indices( ws, ret )
    Ws		*ws;
    Phg_ret	*ret;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    register	Pint			*list, view;
    register	int			i;
    register	Ws_view_priority	*prio = owsb->view_priorities;

    if ( !PHG_SCRATCH_SPACE( &ws->scratch, owsb->num_views * sizeof(Pint))) {
	ret->err = ERR900;

    } else {
	ret->err = 0;
	ret->data.int_list.num_ints = owsb->num_views;
	ret->data.int_list.ints = list = (Pint *)ws->scratch.buf;
	view = owsb->top_view;
	i = 0;
	while ( view != -1 ) {
	    list[i++] = view;
	    view = prio[view].lower;
	}
    }
}


void
phg_wsb_inq_posted( ws, ret )
    Ws		*ws;
    Phg_ret	*ret;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    register	Ws_post_str	*cur, *end;
    register	int		cnt;
    register	Pposted_struct	*list;

    cur = owsb->posted.lowest.higher;
    end = &owsb->posted.highest;

    /* Count them */
    cnt = 0;
    while ( cur != end ) {
	++cnt;
	cur = cur->higher;
    }

    ret->err = 0;
    ret->data.postlist.num_postings = cnt;
    if ( cnt > 0 ) {
	if ( PHG_SCRATCH_SPACE(&ws->scratch, cnt * sizeof(Pposted_struct)) ) {
	    ret->data.postlist.postings = list =
		(Pposted_struct *)ws->scratch.buf;
	    cur = owsb->posted.lowest.higher;
	    while ( cur != end ) {
		list->id = cur->structh->struct_id;
		list++->disp_pri = cur->disp_pri;
		cur = cur->higher;
	    }

	} else {
	    ret->err = ERR900;
	    ret->data.postlist.num_postings = 0;
	}
    }
}


void
phg_wsb_inq_view_rep( ws, index, ret )
    Ws		*ws;
    Pint	index;
    Phg_ret	*ret;
{
    register int		i;
    register Pview_rep3		*cr;
    register Ws_view_entry	*cv;
    register Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    register Phg_ret_view_rep	*vr = &ret->data.view_rep;

    ret->err = 0;
    vr->update_state = owsb->views[index].pending;
    /* Load the "current" view. */
    if ( !PHG_SCRATCH_SPACE( &ws->scratch, sizeof(Pview_rep3) ) ) {
	ret->err = ERR900;
	return;
    } else {
	vr->cur_rep = cr = (Pview_rep3 *)ws->scratch.buf;
	cv = &owsb->views[index];
	cr->clip_limit = cv->clip_limit;
	cr->xy_clip = cv->xy_clip;
	cr->back_clip = cv->back_clip;
	cr->front_clip = cv->front_clip;
	bcopy( (char *)cv->vom, (char *)cr->ori_matrix, sizeof(Pmatrix3) );
	bcopy( (char *)cv->vmm, (char *)cr->map_matrix, sizeof(Pmatrix3) );
    }

    /* Load the "requested" view. */
    if ( vr->update_state == PUPD_NOT_PEND )	/* save some time */
	vr->req_rep = vr->cur_rep;
    else {
	/* Find the requested entry in the pending view list. */
	for ( i = 0; i < owsb->num_pending_views; i++ ) {
	    if ( owsb->pending_views[i].id == index ) {
		cr = &owsb->pending_views[i].view;
		break;	/* we've found it, no need to keep searching */
	    }
	}
	vr->req_rep = cr;
    }
}


void
phg_wsb_inq_ws_xform( ws, ret )
    Ws		*ws;
    Phg_ret	*ret;
{
    register Phg_ret_ws_tran3	*wsxf = &ret->data.ws_xform;
    register Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    ret->err = 0;
    wsxf->state =
	   owsb->ws_window_pending == PUPD_PEND
	|| owsb->ws_viewport_pending == PUPD_PEND
	    ? PUPD_PEND : PUPD_NOT_PEND;
    wsxf->req_window = owsb->req_ws_window;
    wsxf->req_viewport = owsb->req_ws_viewport;
    wsxf->cur_window = owsb->ws_window;
    wsxf->cur_viewport = owsb->ws_viewport;
}


void
phg_wsb_inq_disp_update_state( ws, ret)
    Ws			*ws;
    Phg_ret		*ret;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    ret->err = 0;
    ret->data.update_state.def_mode = ws->out_ws.def_mode;
    ret->data.update_state.mod_mode = ws->out_ws.mod_mode;
    ret->data.update_state.display_surf = owsb->surf_state;
    ret->data.update_state.state = owsb->vis_rep;
}


void
phg_wsb_inq_hlhsr_mode( ws, ret )
    Ws			*ws;
    Phg_ret		*ret;
{
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;

    ret->err = 0;
    ret->data.hlhsr_mode.state =  owsb->hlhsr_mode_pending;
    ret->data.hlhsr_mode.cur_mode = owsb->cur_hlhsr_mode;
    ret->data.hlhsr_mode.req_mode = owsb->req_hlhsr_mode;
}


void
phg_wsb_inq_rep( ws, index, how, rep_type, ret )
    Ws			*ws;
    Pint		index;
    Pinq_type		how;		/* set or realized */
    Phg_args_rep_type	rep_type;
    Phg_ret		*ret;
{
    ret->err = 0;
    switch ( rep_type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	case PHG_ARGS_DCUEREP:
	case PHG_ARGS_LIGHTSRCREP:
	case PHG_ARGS_COLRMAPREP:
	/* View rep is done elsewhere. */
	    phg_wsx_inq_LUT_entry( ws, index, how, rep_type, ret,
		(Pgcolr *)NULL, (Pview_rep3 *)NULL );
	    break;

	case PHG_ARGS_COREP: {
	    Pgcolr		src_gcolr, gcolr;
	    Pcolr_rep	*cb = &ret->data.rep.corep;

	    /* Need to convert to current colour model. */
	    phg_wsx_inq_LUT_entry( ws, index, how, rep_type, ret,
		&src_gcolr, (Pview_rep3 *)NULL );
	    gcolr.type = ws->current_colour_model;
	    (void)phg_utx_convert_colour( &src_gcolr, &gcolr,
		&ws->type->desc_tbl.phigs_dt.out_dt.chroma_info );
	    cb->rgb.red = gcolr.val.general.x;
	    cb->rgb.green = gcolr.val.general.y;
	    cb->rgb.blue = gcolr.val.general.z;
	    } break;
    }
}


static void
update_inv_view_xform( view )
    Ws_view_entry	*view;
{
     /* Calculate the inverse xform, if necessary. */
    if ( view->npc_to_wc_state == WS_INV_NOT_CURRENT ) {
	phg_mat_mul( view->npc_to_wc, view->vmm, view->vom );
	phg_mat_inv( view->npc_to_wc );
	view->npc_to_wc_state = WS_INV_CURRENT;
    }
}


int
phg_wsb_map_initial_points( ws, view_index, num_pts, wc_pts, dwbl_pts )
    Ws		*ws;
    Pint	view_index;
    Pint	*num_pts;
    Ppoint3	*wc_pts;
    XPoint	*dwbl_pts;
{
    Ppoint3	scratch[20];	/* enough for most cases */
    Pmatrix3	wc_to_npc;
    Ppoint3	dc_pt;

    register	Ppoint3		*npc_pts = (Ppoint3 *)NULL;
    register	int		i;
    register	Ws_view_entry	*view;
    register	Ws_xform	*wsxf = &ws->out_ws.model.b.ws_xform;

    /* Transform the initial points to NPC and check that they all fit
     * in the clip limits of the specified view.  Then transform and map
     * them to drawable coordinates.
     */
    if ( *num_pts <= 0 )
	return 0;
    if ( *num_pts <= sizeof(scratch)/sizeof(scratch[0]) )
	npc_pts = scratch;
    else if ( *num_pts > sizeof(scratch)/sizeof(scratch[0]) ) {
	if ( !(npc_pts = (Ppoint3 *)malloc( (unsigned)(*num_pts *
		sizeof(Ppoint3)) )) ) {
	    *num_pts = 0;
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
    }

    view = &ws->out_ws.model.b.views[view_index];
    phg_mat_mul( wc_to_npc, view->vmm, view->vom );
    if ( !phg_tranpts3( wc_to_npc, *num_pts, wc_pts, npc_pts ) ) {
	*num_pts = 0;
	return 0;
    }

    for ( i = 0; i < *num_pts; i++ ) {
	if ( !WS_PT_IN_LIMIT( &view->clip_limit, &npc_pts[i] ) ) {
	    *num_pts = 0;
	    break;
	} else {
	    WS_NPC_TO_DC( wsxf, &npc_pts[i], &dc_pt )
	    WS_DC_TO_DRWBL2( ws, &dc_pt, &dwbl_pts[i] );
	}
    }

    if ( npc_pts && npc_pts != scratch )
	free( (char *)npc_pts );

    return ( *num_pts > 0 ? 1 : 0 );
}


int
phg_wsb_resolve_locator( ws, dc_pt, determine_z, view_index, wc_pt )
    Ws			*ws;
    pexDeviceCoord      *dc_pt;
    int			determine_z;	/* ignored */
    Pint		*view_index;
    Ppoint3		*wc_pt;
{
    Ppoint3		npc_pt;
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    Ws_view_priority	*priorities = owsb->view_priorities;
    Ws_xform		*wsxf = &owsb->ws_xform;
    Plimit3		*ws_win = &owsb->ws_window;
    int			status = 0;

    register Ws_view_entry	*view;

    /* Apply the inverse WS transform and see if it's in the ws window.
     * Can't just check against the viewport boundaries because the
     * window may be smaller if the aspect ratios are different.
     */
    WS_DC_TO_NPC2(wsxf, dc_pt, &npc_pt)
    if ( npc_pt.x >= ws_win->x_min && npc_pt.x <= ws_win->x_max
	&& npc_pt.y >= ws_win->y_min && npc_pt.y <= ws_win->y_max ) {

	/* Find the highest priority view that contains the point. */
	for ( *view_index = owsb->top_view; *view_index != -1;
		*view_index = priorities[*view_index].lower ) {
	    view = &owsb->views[*view_index];
	    if ( WS_PT_IN_LIMIT2(&view->clip_limit, &npc_pt) ) {
		/* Assign the clip window minimum to Z. */
		npc_pt.z = view->clip_limit.z_min;

		/* Calculate the inverse xform if necessary. */
		if ( view->npc_to_wc_state == WS_INV_NOT_CURRENT )
		    update_inv_view_xform( view );
  
		/* Map point to WC if xform invertible. */
		if ( view->npc_to_wc_state == WS_INV_CURRENT ) {
		    if ( phg_tranpt3( &npc_pt, view->npc_to_wc, wc_pt ) ) {
			status = 1;
			break;	/* out of the for on view index */
		    }
		}
	    }
	}
    }
    return status;
}


static int
wsb_resolve_stroke( ws, two_d, dc_ll, dc_ur, view_index )
    Ws			*ws;
    int			two_d;	/* input points are 2D */
    pexDeviceCoord	*dc_ll, *dc_ur;
    Pint		*view_index;	/* resolved view index */

    /* Returns 1 if stroke resolvable, else 0 */
{
    Ppoint3		npc_ll, npc_ur;
    Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    Ws_view_priority	*priorities = owsb->view_priorities;
    Ws_xform		*wsxf = &owsb->ws_xform;
    Plimit3		*ws_win = &owsb->ws_window;
    int			status = 0, in_limit;

    register Ws_view_entry	*view;

    /* Apply the inverse WS transform and see if the bounding box is in
     * the ws window.  Can't just check against the viewport boundaries
     * because the window may be smaller if the aspect ratios are different.
     */
    in_limit = 0;
    if ( two_d ) {
	WS_DC_TO_NPC2(wsxf, dc_ll, &npc_ll)
	WS_DC_TO_NPC2(wsxf, dc_ur, &npc_ur)
	in_limit = WS_PT_IN_LIMIT2(ws_win, &npc_ll)
	    && WS_PT_IN_LIMIT2(ws_win, &npc_ur);
    } else {
	WS_DC_TO_NPC(wsxf, dc_ll, &npc_ll)
	WS_DC_TO_NPC(wsxf, dc_ur, &npc_ur)
	in_limit = WS_PT_IN_LIMIT(ws_win, &npc_ll)
	    && WS_PT_IN_LIMIT(ws_win, &npc_ur);
    }

    if ( in_limit ) {
	/* Find the highest priority view that contains the bounding box. */
	for ( *view_index = owsb->top_view; *view_index != -1;
		*view_index = priorities[*view_index].lower ) {
	    view = &owsb->views[*view_index];
	    in_limit = 0;
	    if ( two_d )
		in_limit = WS_PT_IN_LIMIT2(&view->clip_limit, &npc_ll)
		    && WS_PT_IN_LIMIT2(&view->clip_limit, &npc_ur);
	    else
		in_limit = WS_PT_IN_LIMIT(&view->clip_limit, &npc_ll)
		    && WS_PT_IN_LIMIT(&view->clip_limit, &npc_ur);

	    if ( in_limit ) {
		/* Found the view.  Calculate its inverse transform if
		 * necessary.
		 */
		if ( view->npc_to_wc_state == WS_INV_NOT_CURRENT )
		    update_inv_view_xform( view );

		if ( view->npc_to_wc_state == WS_INV_CURRENT ) {
		    status = 1;
		    break;  /* break out of the view loop when view found */
		}
	    }
	}
    }
    return status;
}


static void
wsb_transform_stroke( ws, view_index, two_d, num_pts, dc_pts, wc_pts )
		Ws		*ws;
		Pint		view_index;
		int		two_d;
    register	int		num_pts;
    register	pexDeviceCoord	*dc_pts;
		Ppoint_list3	*wc_pts;
{
    register int		i;
    register Ppoint3		*npc_pts;
    register Ws_xform		*wsxf = &ws->out_ws.model.b.ws_xform;
    register Ws_view_entry	*view = &ws->out_ws.model.b.views[view_index];

    /* Shouldn't call this function with num_pts == 0. */
    if ( !(npc_pts = (Ppoint3 *)
	    malloc( (unsigned)(num_pts * sizeof(Ppoint3)) )) ) {
	wc_pts->num_points = 0;
	ERR_BUF( ws->erh, ERR900 );
	return;
    }

    /* Transform the points to npc and add the z value. */
    for ( i = 0; i < num_pts; i++ ) {
	if ( two_d ) {
	    WS_DC_TO_NPC2(wsxf, &dc_pts[i], &npc_pts[i])
	    /* Assign the back clip limit. */
	    npc_pts[i].z = view->clip_limit.z_min;
	} else {
	    WS_DC_TO_NPC(wsxf, &dc_pts[i], &npc_pts[i])
	}
    }

    /* Transform the points to wc. */
    if ( !phg_tranpts3( view->npc_to_wc, num_pts, npc_pts, wc_pts->points ) )
	wc_pts->num_points = 0;

    free( (char *)npc_pts );
}


int
phg_wsb_resolve_stroke( ws, num_pts, dc_pts, determine_z, view_index, wc_pts )
		Ws		*ws;
    register	int		num_pts;
		pexDeviceCoord	*dc_pts;
		int		determine_z;	/* ignored */
		Pint		*view_index;
		Ppoint_list3	*wc_pts;
{
    pexDeviceCoord	ll, ur;
    int			status = 0, two_d = determine_z;

    register pexDeviceCoord	*dp;
    register int		i, xmin, xmax, ymin, ymax, zmin, zmax;

    /* Get the bounding box of all the points. */
    xmin = dc_pts->x; xmax = dc_pts->x;
    ymin = dc_pts->y; ymax = dc_pts->y;
    if ( !two_d )
	zmin = dc_pts->z, zmax = dc_pts->z;
    for ( i = 1, dp = &dc_pts[1]; i < num_pts; i++, dp++ ) {
	if ( dp->x < xmin )
	    xmin = dp->x;
	else if ( dp->x > xmax )
	    xmax = dp->x;

	if ( dp->y < ymin )
	    ymin = dp->y;
	else if ( dp->y > ymax )
	    ymax = dp->y;
	
	if ( !two_d ) {
	    /* The incoming points have Z values. */
	    if ( dp->z < zmin )
		zmin = dp->z;
	    else if ( dp->z > zmax )
		zmax = dp->z;
	}
    }
    ll.x = xmin; ll.y = ymax;
    ur.x = xmax; ur.y = ymin;
    if ( !two_d )
	ur.x = xmax, ur.y = ymin;

    /* Resolve and transform the points.  Don't change the current
     * measure if the points can't be resolved.
     */
    if ( wsb_resolve_stroke( ws, two_d, &ll, &ur, view_index ) ) {
	wc_pts->num_points = num_pts;
	wsb_transform_stroke( ws, *view_index, two_d, num_pts, dc_pts, wc_pts );
	status = 1;
    }

    return status;
}


int
phg_wsb_resolve_pick( ws, dev, echo, dc_pt, pick )
    Ws			*ws;
    Ws_inp_pick		*dev;
    int			echo;
    pexDeviceCoord      *dc_pt;
    Ppick		*pick;
{
    register Wsb_output_ws	*owsb = &ws->out_ws.model.b;
    register Ws_post_str	*post_str, *end;
    pexDeviceCoord2D		dc_vol[2];
    Ppoint			npc_pt[2];
    pexPickElementRef		*pickPath;
    int				pickDepth;
    int				betterPick, i;

    CARD32			pickDataBytes;

    struct {
	pexEnumTypeIndex pickType;
	CARD16		 unused;
	union {
	    pexPD_DC_HitBox	dcHitBox;
	    pexPD_NPC_HitVolume npcHitVolume;
	} pickRec;
    } pickData;
    
    WSB_CHECK_POSTED (&owsb->posted)

    if (WSB_SOME_POSTED (&owsb->posted)) {

	/*
	 * Use pick rendering to get the pick results.  Call BeginPickOne, do
	 * a complete traversal (starting with highest priority structure,
	 * then call EndPickOne to get the pick results.
	 */

	if (dev->dev_type == PEXPickDeviceDC_HitBox) {

	    pickData.pickType = PEXPickDeviceDC_HitBox;

	    pickData.pickRec.dcHitBox.position.x = dc_pt->x;
	    pickData.pickRec.dcHitBox.position.y = dc_pt->y;
	    pickData.pickRec.dcHitBox.distance = dev->ap_size;

	    pickDataBytes = 4 + sizeof (pexPD_DC_HitBox);

	} else {

	    pickData.pickType = PEXPickDeviceNPC_HitVolume;

	    dc_vol[0].x = dc_pt->x - dev->ap_size;
	    dc_vol[0].y = dc_pt->y - dev->ap_size;
	    dc_vol[1].x = dc_pt->x + dev->ap_size;
	    dc_vol[1].y = dc_pt->y + dev->ap_size;

	    WS_DC_TO_NPC2 (&owsb->ws_xform, &dc_vol[0], &npc_pt[0])
	    WS_DC_TO_NPC2 (&owsb->ws_xform, &dc_vol[1], &npc_pt[1])

	    pickData.pickRec.npcHitVolume.minval.x = npc_pt[0].x;
	    pickData.pickRec.npcHitVolume.minval.y = npc_pt[0].y;
	    pickData.pickRec.npcHitVolume.maxval.x = npc_pt[1].x;
	    pickData.pickRec.npcHitVolume.maxval.y = npc_pt[1].y;
	    pickData.pickRec.npcHitVolume.minval.z = owsb->ws_window.z_min;
	    pickData.pickRec.npcHitVolume.maxval.z = owsb->ws_window.z_max;

	    pickDataBytes = 4 + sizeof (pexPD_NPC_HitVolume);
	}

	PEXChangeRenderer (ws->display, ws->rid,
		(pexBitmask) PEXRDPickInclusion, (CARD32) sizeof (pexNameSet),
		(char *) &(dev->filter.incl));

	PEXChangeRenderer (ws->display, ws->rid,
		(pexBitmask) PEXRDPickExclusion, (CARD32) sizeof (pexNameSet),
		(char *) &(dev->filter.excl));


	PEXBeginPickOne (ws->display, ws->rid, ws->drawable_id, -1,
		PEXLast, pickDataBytes, &pickData);

	post_str = owsb->posted.highest.lower;
	end = &(owsb->posted.lowest);

	while (post_str != end) {
	    phg_wsb_traverse_net (ws, post_str->structh);
	    post_str = post_str->lower;
	}

	PEXEndPickOne (ws->display, ws->rid, &betterPick,
	    &pickPath, &pickDepth);
    }


    if (!pickDepth) {

    	pick->status = PIN_STATUS_NONE;
    	pick->pick_path.depth = 0;
    	pick->pick_path.path_list = (Ppick_path_elem *) NULL;

    } else {

	/*
	 * The protocol pick element ref data structure has its fields
	 * layed out in a different order than the PHIGS data structure.
	 * We must repack the data into PHIGS format.
	 */

	for (i = 1; i < pickDepth; i++) {
	    Ppick_path_elem *dst = (Ppick_path_elem *) &pickPath[i];
	    pexPickElementRef src;

	    src = pickPath[i];
	    dst->struct_id = src.sid;
	    dst->pick_id = src.pickid;
	    dst->elem_pos = src.offset;
	}

	/*
	 * order = bottom first?
	 */

	if (dev->order == PORDER_BOTTOM_FIRST) {
	    int			head, tail;
	    pexPickElementRef 	temp;

	    head = 1;
	    tail = pickDepth - 1;

	    for (i = 0; i < (pickDepth - 1) / 2; i++) {
		temp = pickPath[head];
		pickPath[head] = pickPath[tail];
		pickPath[tail] = temp;
		head++;
		tail--;
	    }
	}

	/*
	 * return status and pick path
	 */

	pick->status = PIN_STATUS_OK;
	pick->pick_path.depth = pickDepth - 1;
	pick->pick_path.path_list = (Ppick_path_elem *) &(pickPath[1]);
    }

    return 1;
}

void
phg_wsb_inq_filter( ws, type, ret )
    Ws			*ws;
    Phg_args_flt_type	type;
    Phg_ret		*ret;
{
    phg_wsx_inq_name_set( ws, type, (Pint)0, ret );
}


void
phg_wsb_drawable_pick( ws, args, ret )
    Ws				*ws;
    Phg_args_drawable_pick	*args;
    Phg_ret			*ret;
{
    ret->err = ERRN500;
    ERR_BUF( ws->erh, ret->err );
    ret->data.drawable_pick.status = PIN_STATUS_NO_IN;
    ret->data.drawable_pick.pick.depth = 0;
    ret->data.drawable_pick.pick.path_list = (Ppick_path_elem *)NULL;
}


void
phg_wsb_map_points( ws, args, ret )
    Ws				*ws;
    Phg_args_map_points		*args;
    Phg_ret			*ret;
{
    pexDeviceCoord	*dc_pts;

    register int	i;

    ret->err = 0;
    ret->data.map_points.view_index = 0;
    ret->data.map_points.points.num_points = 0;
    ret->data.map_points.points.points = (Ppoint3 *)NULL;

    /* Allocate space for both the DC and WC points. */
    if ( !PHG_SCRATCH_SPACE( &ws->scratch,
	    (unsigned)(args->points.num_points
		* (sizeof(Ppoint) + sizeof(Ppoint3))) ) ) {
	ERR_BUF( ws->erh, ERR900 );
	return;
    }
    dc_pts = (pexDeviceCoord *)ws->scratch.buf;
    ret->data.map_points.points.points =
	(Ppoint3 *)(dc_pts + args->points.num_points);

    /* Convert the points to DC. */
    phg_wsx_update_ws_rect( ws );
    for ( i = 0; i < args->points.num_points; i++ ) {
	/* Z coord is already in DC. */
	WS_DRWBL_TO_DC2(ws, &args->points.points[i], &dc_pts[i]);
	dc_pts[i].z = args->points.points[i].z;
    }

    /* Convert the DC points to WC. */
    if ( !phg_wsb_resolve_stroke( ws, args->points.num_points, dc_pts, 0,
	    &ret->data.map_points.view_index,
	    &ret->data.map_points.points ) ) {
	ret->data.map_points.points.num_points = 0;
    }
}

void
phg_wsb_redraw_regions( ws, args )
    Ws				*ws;
    Phg_args_redraw_regions	*args;
{
    pexDeviceRect	*pex_rects;
    pexBitmask		rmask;
    CARD32		*card32_p;

    if ( args->num_regions <= 0 )
	return;

    if ( !(pex_rects = (pexDeviceRect *)PHG_SCRATCH_SPACE( &ws->scratch,
	    sizeof(CARD32) +
	    (unsigned)(args->num_regions * sizeof(*pex_rects)) )) ) {
	ERR_BUF( ws->erh, ERR900 );
	return;
    }
    card32_p = (CARD32 *)ws->scratch.buf;
    pex_rects = (pexDeviceRect *)(card32_p + 1);

    *card32_p = args->num_regions;
    phg_wsx_convert_rects( ws, args->num_regions, args->regions, pex_rects );
    rmask = PEXRDClipList;
    (void)PEXChangeRenderer( ws->display, ws->rid, rmask,
	(CARD32)(sizeof(CARD32) + args->num_regions * sizeof(pexDeviceRect)),
	(char *)card32_p );
    (*ws->repaint_all)( ws, PFLAG_COND, args->num_regions, args->regions );

    /* Reset the renderer's clip list. */ 
    *card32_p = 0;
    (void)PEXChangeRenderer( ws->display, ws->rid, rmask,(CARD32)sizeof(CARD32),
	(char *)card32_p );
}
