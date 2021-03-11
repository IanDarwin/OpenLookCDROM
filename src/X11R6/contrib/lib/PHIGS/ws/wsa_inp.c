/* $XConsortium: wsa_inp.c,v 5.4 94/04/17 20:42:30 hersh Exp $ */

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

/* PEX/PHIGS workstation input utility functions for the A model (server
 * side workstations and structure storage).  Code in this file must be
 * independent of any PM code.
 */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"


int
phg_wsa_resolve_stroke( ws, num_pts, dc_pts, determine_z, view_index, wc_pts )
    Ws			*ws;
    int			num_pts;
    pexDeviceCoord      *dc_pts;
    int			determine_z;
    Pint		*view_index;
    Ppoint_list3	*wc_pts;
{
    int			status = 0;
    CARD16		view;
    CARD32		count;
    pexCoord3D		*pex_points;
    pexViewport		*vp;
    Pfloat		z_DC;
    pexBitmask		mask[PEXMSGetWksInfo];

    register int	i;

    if ( determine_z ) {
	bzero((char *)mask, sizeof(mask));   
	PEX_BITSET(mask, PEXPWCurWksViewport);
	if ( PEXGetWksInfo( ws->display, ws->rid, mask, (char **)&vp ) ) {
	    z_DC = vp->maxval.z;
	    for ( i = 0; i < num_pts; i++ )
		dc_pts[i].z = z_DC;
	}
    }

    if ( PEXMapDCtoWC( ws->display, ws->rid, (CARD32)num_pts, dc_pts, &view,
	    &count, &pex_points ) && count > 0 ) {
	for ( i = 0; i < count; i++ ) {
	    wc_pts->points[i].x = pex_points[i].x;
	    wc_pts->points[i].y = pex_points[i].y;
	    wc_pts->points[i].z = pex_points[i].z;
	}
	wc_pts->num_points = count;
	*view_index = view;
	status = 1;

    } else {
	wc_pts->num_points = 0;
    }

    return status;
}


int
phg_wsa_resolve_locator( ws, dc_pt, determine_z, view_index, wc_pt )
    Ws			*ws;
    pexDeviceCoord      *dc_pt;
    int			determine_z;
    Pint		*view_index;
    Ppoint3		*wc_pt;
{
    Ppoint_list3	wc_pts;

    wc_pts.points = wc_pt;
    return phg_wsa_resolve_stroke( ws, 1, dc_pt, determine_z, view_index,
	&wc_pts );
}


int
phg_wsa_resolve_pick( ws, dev, dc_pt, status, depth, path )
    Ws			*ws;
    Ws_inp_pick		*dev;
    pexDeviceCoord	*dc_pt;
    CARD16		*status;
    CARD32		*depth;
    pexPickPath		**path;
{
    pexViewport		*vp;
    pexNpcSubvolume	*win;
    Plimit3		ws_win, ws_vp;
    Ws_xform		ws_xform;
    pexBitmask		mask[PEXMSGetWksInfo];
    pexPD_DC_HitBox	dc_box;
    pexPD_NPC_HitVolume npc_volume;
    pexDeviceCoord2D	dc_vol[2];
    Ppoint		npc_pt[2];
    char		*buf;

    *depth = 0;
    *status = PEXNoPick;

    /* Compute the aperture. */
    bzero((char *)mask, sizeof(mask));   
    switch ( dev->dev_type ) {
	case PEXPickDeviceDC_HitBox:
	    PEX_BITSET(mask, PEXPWCurWksViewport);
	    if ( PEXGetWksInfo( ws->display, ws->rid, mask, (char **)&vp )){
		ws_vp.x_min = vp->minval.x; ws_vp.x_max = vp->maxval.x;
		ws_vp.y_min = vp->minval.y; ws_vp.y_max = vp->maxval.y;
		if ( !WS_PT_IN_LIMIT2( &ws_vp, dc_pt ) )
		    return 0;

		/* Compute the aperture. */
		dc_box.position.x = dc_pt->x;
		dc_box.position.y = dc_pt->y;
		dc_box.distance = dev->ap_size;

		(void)PEXUpdatePickMeasure( ws->display, dev->measure,
		    (CARD32)sizeof(dc_box), (CARD8 *)&dc_box );
	    } else
		return 0;
	    break;

	case PEXPickDeviceNPC_HitVolume:
	    /* Compute the workstation transform, transform the DC point
	     * and DC aperture size to an NPC volume.
	     */
	    PEX_BITSET(mask, PEXPWCurNpcSubvolume);
	    PEX_BITSET(mask, PEXPWCurWksViewport);
	    if ( PEXGetWksInfo( ws->display, ws->rid, mask, (char **)&win)){
		/* Build the current workstation transform. */
		vp = (pexViewport *)(win + 1);
		ws_win.x_min = win->minval.x; ws_win.x_max = win->maxval.x;
		ws_win.y_min = win->minval.y; ws_win.y_max = win->maxval.y;
		ws_win.z_min = win->minval.z; ws_win.z_max = win->maxval.z;
		ws_vp.x_min = vp->minval.x; ws_vp.x_max = vp->maxval.x;
		ws_vp.y_min = vp->minval.y; ws_vp.y_max = vp->maxval.y;
		ws_vp.z_min = vp->minval.z; ws_vp.z_max = vp->maxval.z;
		if ( !WS_PT_IN_LIMIT2( &ws_vp, dc_pt ) )
		    return 0;

		/* Compute the aperture. */
		dc_vol[0].x = dc_pt->x - dev->ap_size;
		dc_vol[0].y = dc_pt->y - dev->ap_size;
		dc_vol[1].x = dc_pt->x + dev->ap_size;
		dc_vol[1].y = dc_pt->y + dev->ap_size;
		phg_wsx_compute_ws_transform( &ws_win, &ws_vp, &ws_xform );
		WS_DC_TO_NPC2(&ws_xform, &dc_vol[0], &npc_pt[0])
		WS_DC_TO_NPC2(&ws_xform, &dc_vol[1], &npc_pt[1])
		npc_volume.minval.x = npc_pt[0].x;
		npc_volume.minval.y = npc_pt[0].y;
		npc_volume.maxval.x = npc_pt[1].x;
		npc_volume.maxval.y = npc_pt[1].y;
		/* The volume extends the full depth of the window. */
		npc_volume.minval.z = win->minval.z;
		npc_volume.maxval.z = win->maxval.z;

		(void)PEXUpdatePickMeasure( ws->display, dev->measure,
		    (CARD32)sizeof(npc_volume), (CARD8 *)&npc_volume );
	    } else
		return 0;
	    break;
    }

    mask[0] = PEXPDPickStatus | PEXPDPickPath;
    if ( !PEXGetPickMeasure( ws->display, dev->measure, mask[0], &buf ) )
	return 0;	/* TODO: use phg_pex_errno */

    *status = (CARD16) *((CARD32 *)buf);
    *depth = *((CARD32 *)(buf + sizeof(CARD32)));
    *path = (pexPickPath *)(buf + 2 * sizeof(CARD32));

    return 1;
}


int
phg_wsa_pick_enable( ws, dev, init_path_size, init_path )
    Ws			*ws;
    Ws_inp_pick		*dev;
    CARD32		init_path_size;
    char		*init_path;	/* must be PEX encoded */
{
    pexBitmask		mask = 0;
    CARD32		size, *card32_p;
    XID			*xid_p;
    CARD16		*card16_p;
    INT16		*int16_p;
    pexViewport		*ev;
    char		buffer[6 * sizeof(CARD32) + sizeof(pexViewport)];

    register char	*buf = buffer;

    /* Set the device data and create a pick measure for this interaction. */
    mask |= PEXPDPickStatus;
    card32_p = (CARD32 *)buf; 
    *card32_p = dev->pick.status == PIN_STATUS_OK ? PEXOk : PEXNoPick;
    buf += sizeof(CARD32);

    mask |= PEXPDPickPathOrder;
    card32_p = (CARD32 *)buf; 
    *card32_p = dev->order == PORDER_TOP_FIRST ? PEXTopFirst : PEXBottomFirst;
    buf += sizeof(CARD32);

    mask |= PEXPDPickIncl;
    xid_p = (XID *)buf; 
    *xid_p = dev->filter.incl;
    buf += sizeof(CARD32);

    mask |= PEXPDPickExcl;
    xid_p = (XID *)buf; 
    *xid_p = dev->filter.excl;
    buf += sizeof(CARD32);

    mask |= PEXPDPickPromptEchoType;
    card32_p = (CARD32 *)buf; 
    *card32_p = (CARD32)dev->pet;
    buf += sizeof(CARD32);

    mask |= PEXPDPickEchoVolume;
    ev = (pexViewport *)buf; 
    ev->minval.x = dev->e_volume.x_min;
    ev->minval.y = dev->e_volume.y_min;
    ev->minval.z = dev->e_volume.z_min;
    ev->maxval.x = dev->e_volume.x_max;
    ev->maxval.y = dev->e_volume.y_max;
    ev->maxval.z = dev->e_volume.z_max;
    ev->useDrawable = 0;
    buf += sizeof(pexViewport);

    mask |= PEXPDPickEchoSwitch;
    card32_p = (CARD32 *)buf; 
    *card32_p = dev->esw == PSWITCH_ECHO ? PEXEcho : PEXNoEcho;
    buf += sizeof(CARD32);

    (void)PEXChangePickDevice( ws->display, ws->rid, dev->dev_type, mask,
															(CARD32)sizeof(buffer), buffer );

    if ( dev->pick.status == PIN_STATUS_OK ) {
	mask = PEXPDPickPath;
	(void)PEXChangePickDevice( ws->display, ws->rid, dev->dev_type, mask,
	    init_path_size, init_path );
    }

    (void)PEXCreatePickMeasure( ws->display, ws->rid,
	dev->measure = XAllocID(ws->display), (pexEnumTypeIndex)dev->dev_type );
    XFlush( ws->display );

    return 1;
}


void
phg_wsa_pick_disable( ws, dev )
    Ws			*ws;
    Ws_inp_pick		*dev;
{
    /* Free and clear the pick measure. */
    if ( dev->measure ) {
	(void)PEXFreePickMeasure( ws->display, dev->measure );
	dev->measure = 0;
    }
}
