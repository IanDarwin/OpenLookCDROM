/* $XConsortium: wsx_util.c,v 5.7 94/04/17 20:42:34 rws Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.


Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 
All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Sun Microsystems
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

*/

/* PEX/PHIGS workstation utility functions. */

#define NEED_EVENTS

#include <X11/Xlibint.h>
#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"
#include <X11/Xatom.h>



/*
 * 				    WARNING
 *
 * This is a ICCCM routine.  It will reference the new fields
 * in the XStandardColormap structure.
 */

static Status
ICCCM_XGetStandardColormap (display, w, cmap, property)
    Display *display;
    Window w;
    XStandardColormap *cmap;
    Atom property;		/* XA_RGB_BEST_MAP, etc. */
{
    Status stat;			/* return value */
    XStandardColormap *stdcmaps;	/* will get malloced value */
    int nstdcmaps;			/* count of above */

    stat = XGetRGBColormaps (display, w, &stdcmaps, &nstdcmaps, property);
    if (stat) {
	XStandardColormap *use;

	if (nstdcmaps > 1) {
	    VisualID vid;
	    Screen *sp;
	    int i;

	    sp = _XScreenOfWindow (display, w);
	    if (!sp) {
		if (stdcmaps) Xfree ((char *) stdcmaps);
		return FALSE;
	    }
	    vid = sp->root_visual->visualid;

	    for (i = 0; i < nstdcmaps; i++) {
		if (stdcmaps[i].visualid == vid) break;
	    }

	    if (i == nstdcmaps) {	/* not found */
		Xfree ((char *) stdcmaps);
		return FALSE;
	    }
	    use = &stdcmaps[i];
	} else {
	    use = stdcmaps;
	}
	
	cmap->colormap	 = use->colormap;
	cmap->red_max	 = use->red_max;
	cmap->red_mult	 = use->red_mult;
	cmap->green_max	 = use->green_max;
	cmap->green_mult = use->green_mult;
	cmap->blue_max	 = use->blue_max;
	cmap->blue_mult	 = use->blue_mult;
	cmap->base_pixel = use->base_pixel;
	cmap->visualid   = use->visualid;
	cmap->killid     = use->killid;

	Xfree ((char *) stdcmaps);	/* don't need alloced memory */
    }
    return stat;
}



/*  Ideally, what we want to be able to do is take these steps (before a window
 *  is created) :
 *
 *	Find the most appropriate visual for our needs
 *	See if there is a StandardColormap property defined
 *	    on that visual.
 *	Set up the colormap on that visual, or use an existing one.
 *
 *  The problem, though, is that X doesn't provide a mechanism by
 *  which StandardColormap properties can be queried on an individual
 *  visual class if a window on that visual doesn't exist.  Therefore,
 *  we have to go heavily heuristic, and the heuristics used here are
 *  certainly open to tweaking.
 */


int
phg_wsx_find_best_visual(ws, wst, best_visual, cmap, err)
    Ws		*ws;
    Wst		*wst;
    Visual	*best_visual;
    Colormap	*cmap;
    Pint	*err;
{
    Display		*dpy = ws->display;
    XVisualInfo		*available_visuals, *the_best, template;
    XStandardColormap	 std;
    int			 nvisuals, i;
    Atom		 best_map;
    Atom		 pex_map;
    Visual		*default_visual;

    *err = 0;
    default_visual = DefaultVisual(dpy, DefaultScreen(dpy));
    *cmap = DefaultColormap( dpy, DefaultScreen(dpy) );

    /* use default visual if it has a standard colormap defined
     * regardless of the visual type
     * try for BEST std colormap first, then DEFAULT
     */
    if ((default_visual->class == StaticGray) ||
       (default_visual->class == GrayScale))
	best_map = XA_RGB_GRAY_MAP;
    else
	best_map = XA_RGB_BEST_MAP;

    /* look to see if there is an application defined std colormap
     * if there is, use it's colormap, but still find the best visual
     */
    pex_map = wst->desc_tbl.xwin_dt.colormap_property_atom;
    if (ICCCM_XGetStandardColormap(dpy, DefaultRootWindow(dpy),  &std,
			pex_map) && (std.colormap)) {
	*cmap = std.colormap;
	goto get_best_visual;
    }

    if (ICCCM_XGetStandardColormap(dpy, DefaultRootWindow(dpy),  &std,
			best_map) && (std.colormap)) {
	/* map already exists */
        template.visualid = std.visualid;
        available_visuals = XGetVisualInfo(dpy, VisualIDMask,
				       &template, &nvisuals);

	*best_visual = *available_visuals->visual;
	*cmap = std.colormap;
        XFree((caddr_t)available_visuals);
	return ( True );
    }

    if (ICCCM_XGetStandardColormap(dpy, DefaultRootWindow(dpy),
		&std, XA_RGB_DEFAULT_MAP) && (std.colormap)) {
	/* map already exists */
        template.visualid = std.visualid;
        available_visuals = XGetVisualInfo(dpy, VisualIDMask,
				       &template, &nvisuals);

	*best_visual = *available_visuals->visual;
        XFree((caddr_t)available_visuals);
	*cmap = std.colormap;
	return ( True );
    }

get_best_visual:
        /* no standard colormap so pick best visual
	 *
         *  Best Visual is that with the highest numbered class.
         *  This relies on the fact that the class values are ordered:
         *    DirectColor > TrueColor > PseudoColor > StaticColor
         *
         *  However, only choose a higher numbered class if its got at
         *  least as many cmap entries as what's currently best
	 *  Note: colour mapping is only supported on PseudoColor visuals
         */
        template.screen = DefaultScreen(dpy);
        available_visuals = XGetVisualInfo(dpy, VisualScreenMask,
				       &template, &nvisuals);

        the_best = &available_visuals[0];
        for (i = 1; i < nvisuals; i++) {
	    if (  (available_visuals[i].class > the_best->class) &&
	          (available_visuals[i].colormap_size >= the_best->colormap_size))
	        the_best = &available_visuals[i];
        }

	*best_visual = *the_best->visual;
    	XFree((caddr_t)available_visuals);

	/* If the best visual is different from the root window visual */
	/* then we must put the right kind of Colormap in cmap so that */
	/* the CreateWindow will work.                                 */
	if ( best_visual->visualid != default_visual->visualid)
	{
		*cmap = XCreateColormap( dpy,
					RootWindow( dpy, DefaultScreen(dpy) ),
					best_visual, AllocNone );
	}

    return ( True );
}




/* define max number of colors and number of planes for color cube */
/* use a 2/3/2 (planes) cube (or  4/8/4 in terms of # of colors) if
 * possible. This puts in 128 colors. However, Sun has a "feature"
 * that if the last entry in the colormap is the same as the first
 * entry, it auto-magically reverses the last entry. If this cube
 * is allocated to go in the end of the colormap (which has always
 * happened for me), then that causes the last color (white) to be
 * reversed (to black).  What a nice feature :). Therefore, when using
 * Sun's, mudge this to do a 2/2/2 cube so it won't be as likely to
 * end up at the end of the colormap.
 */
#ifdef sun

#define REDS_128	4
#define GREENS_128	4
#define BLUES_128	4

#define RED_PLANES	2
#define GREEN_PLANES	2
#define BLUE_PLANES	2

#else
#define REDS_128	4
#define GREENS_128	8
#define BLUES_128	4

#define RED_PLANES	2
#define GREEN_PLANES	3
#define BLUE_PLANES	2

#endif


int
phg_wsx_get_true_colors(ws, attrs, ncolors, xcolors, pcae)
    Ws                  *ws;
    XWindowAttributes   *attrs;
    int                 *ncolors;
    XColor              **xcolors;
    pexColourApproxEntry        *pcae;
{
    Display		    *dpy = ws->display;
    Drawable		     pex_map_window;
    XStandardColormap	     std;
    Atom		     best_map;
    Atom		     pex_map;
    int			     err;
    register int	     i;
    register XColor	    *xcolor;

    if ((attrs->visual->class == StaticGray) ||
       (attrs->visual->class == GrayScale))
	best_map = XA_RGB_GRAY_MAP;
    else
	best_map = XA_RGB_BEST_MAP;

    /* look for predefined maps first */
    pex_map = ws->type->desc_tbl.xwin_dt.colormap_property_atom;
    if (ICCCM_XGetStandardColormap(dpy, DefaultRootWindow(dpy),  &std,
		pex_map) && (attrs->visual->visualid == std.visualid)) {
	/* colors are already in the colormap */
	*ncolors = 0;
	goto set_colour_approx_table;
    } else if (ICCCM_XGetStandardColormap(dpy, DefaultRootWindow(dpy), &std,
		best_map) && (attrs->visual->visualid == std.visualid)) {
	/* colors are already in the colormap */
	*ncolors = 0;
	goto set_colour_approx_table;
    } else if (ICCCM_XGetStandardColormap(dpy, DefaultRootWindow(dpy), &std,
		XA_RGB_DEFAULT_MAP) && (attrs->visual->visualid == std.visualid)) {
	/* colors are already in the colormap */
	*ncolors = 0;
	goto set_colour_approx_table;
    }

    /* no predefined maps found, so make our own color cube */
    std.colormap = attrs->colormap;
    std.visualid = attrs->visual->visualid;

    /* define color entries */
    switch(attrs->visual->class) {
	case StaticGray:
	case StaticColor:
#ifndef PEX_SI_API_GRAY_VISUAL
	case GrayScale:
#endif
	    /* can't set entries */
	    return ( ERRN168 );
	    break;
#ifdef PEX_SI_API_GRAY_VISUAL
/* PEX-SI doesn't support GrayScale visuals.
 * Here's untested code in case you want to try it
 */
	case GrayScale:
	/* set up gray scale, r=g=b */
	{
	    unsigned int nplanes;
	    unsigned long *plane_masks;

	    *ncolors = REDS_128 * GREENS_128 * BLUES_128;
	    nplanes = RED_PLANES + BLUE_PLANES + GREEN_PLANES;
	    if ( !(*xcolors = (XColor *)Malloc(nplanes * sizeof(unsigned long) +
	   		*ncolors * sizeof(XColor))) )
	        return ( ERR900 );

	    plane_masks = (unsigned long *)(*xcolors + *ncolors);

	    if (!XAllocColorCells( dpy, attrs->colormap, True,
			plane_masks, nplanes, &std.base_pixel, 1 )) {
		    if (!(std.colormap = XCreateColormap( dpy,
				ws->drawable_id, attrs->visual, AllocNone ))) {
			err = ERRN169;
			goto fail;
		    } else {
	    		if (!XAllocColorCells( dpy, std.colormap, True,
				plane_masks, nplanes, &std.base_pixel, 1 )) {
			    XFreeColormap( dpy, std.colormap );
			    err = ERRN169;
			    goto fail;
			}
			else {
			    XSetWindowColormap( dpy, ws->drawable_id,
				std.colormap);
			    attrs->colormap = std.colormap;
			}
		    }
	    }

	    /* find low order plane mask. it may always be in [0],
	     * but I'm not sure, so look for it
	     */
	    for (i=0, std.red_mult = plane_masks[0]; i<nplanes; i++)
		if (plane_masks[i] < std.red_mult)
		    std.red_mult = plane_masks[i];

	    /* setting up a gray ramp into a PEX ColourSpace doesn't
	     * really make sense, so try something else if you can
	     */
	    std.green_mult = std.red_mult * REDS_128;
	    std.blue_mult = std.green_mult * GREENS_128;

    	    std.red_max = REDS_128 - 1;
	    std.green_max = GREENS_128 - 1;
	    std.blue_max = BLUES_128 - 1;

	    /* use nplanes as temp value */
	    nplanes = X_MAX_COLOR_VALUE / (*ncolors - 1);

	    for (xcolor = *xcolors, i = 0; i < *ncolors; i++, xcolor++) {
		xcolor->flags = DoRed;
		xcolor->pixel = std.base_pixel + i;
		xcolor->red = i * nplanes;
		xcolor->green = xcolor->blue = xcolor->red;
	    }
	    break;
	}
#endif
	case PseudoColor:
	/* set up pseudo color cube */
	{
	    unsigned int nplanes;
	    unsigned long *plane_masks;
	    register int j, k;

	    *ncolors = REDS_128 * GREENS_128 * BLUES_128;
	    nplanes = RED_PLANES + GREEN_PLANES + BLUE_PLANES;
	    if ( !(*xcolors = (XColor *)Malloc(nplanes * sizeof(unsigned long) +
	   		*ncolors * sizeof(XColor))) )
	        return ( ERR900 );

	    plane_masks = (unsigned long *)(*xcolors + *ncolors);

	    if (!XAllocColorCells( dpy, attrs->colormap, True,
			plane_masks, nplanes, &std.base_pixel, 1 )) {
		    if (!(std.colormap = XCreateColormap( dpy,
				ws->drawable_id, attrs->visual, AllocNone ))) {
			err = ERRN169;
			goto fail;
		    } else {
	    		if (!XAllocColorCells( dpy, std.colormap, True,
				plane_masks, nplanes, &std.base_pixel, 1 )) {
			    XFreeColormap( dpy, std.colormap );
			    err = ERRN169;
			    goto fail;
			}
			else {
			    XSetWindowColormap( dpy, ws->drawable_id,
				std.colormap);
			    attrs->colormap = std.colormap;
			}
		    }
	    }

	    /* find low order plane mask. it may always be in [0],
	     * but I'm not sure, so look for it
	     */
	    for (i=0, std.red_mult = plane_masks[0]; i<nplanes; i++)
		if (plane_masks[i] < std.red_mult)
		    std.red_mult = plane_masks[i];

	    std.green_mult = std.red_mult * REDS_128;
	    std.blue_mult = std.green_mult * GREENS_128;

    	    std.red_max = REDS_128 - 1;
	    std.green_max = GREENS_128 - 1;
	    std.blue_max = BLUES_128 - 1;

	    xcolor = *xcolors;
	    for (i = 0; i <= std.red_max; i++) /* red loop */
	        for (j = 0; j <= std.green_max; j++) /* green loop */
	            for (k = 0; k <= std.blue_max; k++) { /* blues */
			xcolor->flags = DoRed | DoGreen | DoBlue;
			xcolor->pixel = std.base_pixel +
					i * std.red_mult +
					j * std.green_mult +
					k * std.blue_mult;
			xcolor->red = i * X_MAX_COLOR_VALUE / std.red_max;
			xcolor->green = j * X_MAX_COLOR_VALUE / std.green_max;
			xcolor->blue = k * X_MAX_COLOR_VALUE / std.blue_max;
			xcolor++;
	    	    }
	    break;
	}

	case DirectColor:
	/* set up direct color cube */
	{
            unsigned long red_mask, green_mask, blue_mask;
            register unsigned long red_shift, green_shift, blue_shift;
	    int red_planes, green_planes, blue_planes;
	    int red_max, green_max, blue_max;
	    XVisualInfo template, *visual_info;
	    int nvisuals;

            template.visualid = attrs->visual->visualid;
            visual_info = XGetVisualInfo(dpy, VisualIDMask,
					 &template, &nvisuals);

	    *ncolors = visual_info->colormap_size;
	    if ( !(*xcolors = (XColor *)Malloc(*ncolors * sizeof(XColor)) ) )
	        return ( ERR900 );

	    /*
	     * Figure out the number of planes for each component
	     * Figure out the number of colors for each component
             * Note: This code has been tested with servers supporting
             * DirectColor visuals with equal component depths (e.g., 8 8 8),
             * but has not been tested with unequal component depths.
             */
            red_planes = green_planes = blue_planes = 0;
            red_max = green_max = blue_max = 1;
	    for(red_mask = visual_info->red_mask;
                red_mask; red_mask = red_mask >> 1)
		if (red_mask & 1) { red_planes++; red_max *= 2; }
	    for(green_mask = visual_info->green_mask;
                green_mask; green_mask = green_mask >> 1)
		if (green_mask & 1) { green_planes++; green_max *= 2; }
	    for(blue_mask = visual_info->blue_mask;
                blue_mask; blue_mask = blue_mask >> 1)
		if (blue_mask & 1) { blue_planes++; blue_max *= 2; }

	    if (!XAllocColorPlanes( dpy, attrs->colormap, True,
			&std.base_pixel, 1,
			red_planes, green_planes, blue_planes,
			&red_mask, &green_mask, &blue_mask)) {
		    if (!(std.colormap = XCreateColormap( dpy,
				ws->drawable_id, attrs->visual, AllocNone ))) {
			err = ERRN169;
			goto fail;
		    } else {
	    		if (!XAllocColorPlanes( dpy, std.colormap, True,
				&std.base_pixel, 1,
				red_planes, green_planes, blue_planes,
				&red_mask, &green_mask, &blue_mask)) {
			    XFreeColormap( dpy, std.colormap );
			    err = ERRN169;
			    goto fail;
			}
			else {
			    XSetWindowColormap( dpy, ws->drawable_id,
				std.colormap);
			    attrs->colormap = std.colormap;
			}
		    }
	    }

	    /* find low order bit position of the planes */
            for (red_shift = 0; !(red_mask & (1<<red_shift)); red_shift++)
;
            for (green_shift = 0; !(green_mask & (1<<green_shift)); green_shift++);
            for (blue_shift = 0; !(blue_mask & (1<<blue_shift)); blue_shift++);

	    std.red_mult = 1 << red_shift;
	    std.green_mult = 1 << green_shift;
	    std.blue_mult = 1 << blue_shift;

    	    std.red_max = red_max - 1;
	    std.green_max = green_max - 1;
	    std.blue_max = blue_max - 1;

	    for (xcolor = *xcolors, i = 0; i < *ncolors; i++, xcolor++) {
		xcolor->flags = 0;
                xcolor->pixel = std.base_pixel;
                if (i < red_max) {
		    xcolor->flags |= DoRed ;
                    xcolor->pixel |= (i << red_shift);
		    xcolor->red = i * X_MAX_COLOR_VALUE / std.red_max;
                    }
                if (i < green_max) {
		    xcolor->flags |= DoGreen ;
                    xcolor->pixel |= (i << green_shift);
		    xcolor->green = i * X_MAX_COLOR_VALUE / std.green_max;
                    }
                if (i < blue_max) {
		    xcolor->flags |= DoBlue;
                    xcolor->pixel |= (i << blue_shift);
		    xcolor->blue = i * X_MAX_COLOR_VALUE / std.blue_max;
                    }
	    }
	    break;
	}
	case TrueColor:
	    *ncolors = 0;
	    break;
    }
	
set_colour_approx_table:
    /* TRUE is always loaded as a ColourSpace regardless of visual type */
    pcae->approxType	= PEXColourSpace;
    pcae->approxModel	= PEXColourApproxRGB;
    pcae->max1		= std.red_max;
    pcae->max2		= std.green_max;
    pcae->max3		= std.blue_max;
    pcae->dither	= True;
    pcae->mult1		= std.red_mult;
    pcae->mult2		= std.green_mult;
    pcae->mult3		= std.blue_mult;
    pcae->basePixel	= std.base_pixel;
    pcae->weight1	= 0.0;	/* unused for ColourSpace */
    pcae->weight2	= 0.0;	/* unused for ColourSpace */
    pcae->weight3	= 0.0;	/* unused for ColourSpace */

    if ( *ncolors )
	XChangeProperty( dpy, DefaultRootWindow(dpy), pex_map,
	                 XA_RGB_COLOR_MAP, 32, PropModeReplace,
	                 (unsigned char *)&std,
			sizeof(XStandardColormap)/sizeof(long) );

    XSetWindowColormap( dpy, ws->drawable_id, std.colormap);

    /* keep track of how many true colours were defined */

    ws->type->desc_tbl.phigs_dt.out_dt.num_true_colours =
	 (pcae->max1 + 1) * (pcae->max2 + 1) * (pcae->max3 + 1);

    return ( 0 );
fail:
    free( (char *)*xcolors );
    return ( err );
}



int
phg_wsx_setup_colormap(ws, err)
    Ws		*ws;
    Pint	*err;
{
    /* The window has a colormap. We need to set up the default
     * colour mapping and color approximation entries and set
     * colors in the colormap if needed.
     */
    XWindowAttributes	     attrs;
    XColor		    *xcolors;
    pexColourApproxEntry     pcae;
    int                      ncolors;
	
    *err = 0;

    XGetWindowAttributes(ws->display, ws->drawable_id, &attrs);

    if (*err = phg_wsx_get_true_colors(ws, &attrs, &ncolors, &xcolors, &pcae))
	return ( False );

    if (!PEXSetTableEntries(ws->display, ws->out_ws.lut.colour_approx,
	    (pexTableIndex)0, (CARD16)1, (CARD32)sizeof(pcae),
		    (char *)&pcae)) {
		*err = ERRN254;
		return ( False );
	    }
    if (ncolors) {
        XStoreColors( ws->display, attrs.colormap, xcolors, ncolors );
        free( (char *)xcolors );
    }

    ws->true_colr_map_count = 1;
    ws->true_colr_map_base_pixel = pcae.basePixel;
	
    phg_wsx_lut_update_htab( ws, ws->out_ws.htab.colour_approx,
	ws->out_ws.lut.colour_approx );

    return ( True );
}



caddr_t
phg_wsx_colr_mapping_entry_from_pex( ws, pcmb, cmb )
    		Ws			*ws;
    register	pexColourApproxEntry	*pcmb;
    register	Phg_colr_map_rep	*cmb;
{
    Display	*dpy = ws->display;
    XWindowAttributes	attrs;
    int		ncolrs;
    XColor	*xcolrs;
    register XColor	*xcolr;
    register unsigned int	i;


    XGetWindowAttributes(dpy, ws->drawable_id, &attrs);
    switch ( pcmb->approxType ) {
	case PEXColourSpace:
	    if ( ws->true_colr_map_count &&
		 (ws->true_colr_map_base_pixel == pcmb->basePixel) ) {
	        cmb->method = PCOLR_MAP_TRUE;
	        /* no data */
	    } else {
		/* PSEUDO_N is best used with DirectColor visuals
		 * PEX-SI doesn't support DirectColor visuals,
		 * but here's some untested code in case you want
		 * to try it
		 */
	    	int		nred = pcmb->max1 + 1;
	    	int		ngreen = pcmb->max2 + 1;
	    	int		nblue = pcmb->max3 + 1;
		Pfloat	*pred, *pgreen, *pblue;

	        ncolrs = WS_MAX_3( nred, ngreen, nblue );
	        cmb->method = PCOLR_MAP_PSEUDO_N;
		cmb->rec.meth_r3.colr_model = pcmb->approxModel;
		cmb->rec.meth_r3.colr_lists.num_lists = 3;
	        cmb->rec.meth_r3.colr_lists.lists[0].num_floats = nred;
	        cmb->rec.meth_r3.colr_lists.lists[1].num_floats = ngreen;
	        cmb->rec.meth_r3.colr_lists.lists[2].num_floats = nblue;
	    if ( !( xcolrs = (XColor *)Malloc(ncolrs * sizeof(XColor)) ) ) {
		goto fail;
	    }

	    for (i=0, xcolr=xcolrs; i< ncolrs; i++, xcolr++ ) {
		xcolr->flags = DoRed | DoGreen | DoBlue;
		xcolr->pixel = i * pcmb->mult1 + i * pcmb->mult2 +
			       i * pcmb->mult3 + pcmb->basePixel;
	    }
	    XQueryColors( dpy, attrs.colormap, xcolrs, ncolrs );

	    /* put x colors in data record */
	    for (i=0, xcolr=xcolrs;
		 pred=cmb->rec.meth_r3.colr_lists.lists[0].floats,
		 pgreen=cmb->rec.meth_r3.colr_lists.lists[1].floats,
		 pblue=cmb->rec.meth_r3.colr_lists.lists[2].floats,
		 i< ncolrs; i++, xcolr++ ) {
		if ( i < nred ) {
		    X_CONV_COLOR_TO_Pfloat( xcolr->red, *pred );
		    pred++;
		}
		if ( i < ngreen ) {
		    X_CONV_COLOR_TO_Pfloat( xcolr->green, *pgreen );
		    pgreen++;
		}
		if ( i < nblue ) {
		    X_CONV_COLOR_TO_Pfloat( xcolr->blue, *pblue );
		    pblue++;
		}
	    }
	    free( (char *)xcolrs );
	    }
	    break;
	case PEXColourRange:
	{
	    register Pcolr_rep	*pcolr;

	    ncolrs = pcmb->max1 + 1;
	    cmb->method = PCOLR_MAP_PSEUDO;
    	    cmb->rec.meth_r2.colr_model = pcmb->approxModel;
	    cmb->rec.meth_r2.weights.num_floats = 3;
	    cmb->rec.meth_r2.weights.floats[0] = pcmb->weight1;
	    cmb->rec.meth_r2.weights.floats[1] = pcmb->weight2;
	    cmb->rec.meth_r2.weights.floats[2] = pcmb->weight3;
	    cmb->rec.meth_r2.colrs.num_colr_reps = ncolrs;

	    if ( !(xcolrs = (XColor *)Malloc(ncolrs * sizeof(XColor))) ) {
		goto fail;
	    }

	    for (i=0, xcolr=xcolrs; i< ncolrs; i++, xcolr++ ) {
		xcolr->flags = DoRed | DoGreen | DoBlue;
		/* not efficient */
		xcolr->pixel = i * pcmb->mult1 + pcmb->basePixel;
	    }
	    XQueryColors( dpy, attrs.colormap, xcolrs, ncolrs );

	    /* convert xcolrs to pcolrs */
	    for (i=0, xcolr=xcolrs, pcolr=cmb->rec.meth_r2.colrs.colr_reps;
		i<ncolrs; i++, xcolr++, pcolr++) {
		X_CONV_COLOR_TO_Pcolr_rep(xcolr, pcolr);
	    }
	    free( (char *)xcolrs );
	    break;
	}
    }
    return ( (caddr_t)pcmb );
fail:
    return( (caddr_t) NULL );

}


Ws*
phg_wsx_create( cph, args, css_srvr )
    Cp_handle		cph;
    Phg_args_open_ws	*args;
    Cpx_css_srvr	*css_srvr;
{
    Ws			*ws;

    if ( !(ws = (Ws *)calloc( 1, sizeof(Ws))) ) {
	ERR_BUF( cph->erh, ERR900 );

    } else if ( !PHG_SCRATCH_SPACE( &ws->scratch, 8096 )) {
	ERR_BUF( cph->erh, ERR900 );
	free( (char *)ws );
	ws = (Ws *)NULL;

    } else {
	ws->css_srvr = css_srvr;
	ws->cph = cph;
	ws->erh = cph->erh;
	ws->id = args->wsid;
    }

    return ws;
}


void
phg_wsx_destroy( ws )
    Ws		*ws;
{
    if ( ws->type ) {
	ws->type->bound_status = WST_UNBOUND;
	phg_wst_destroy( ws->type );
    }
    if ( ws->scratch.buf )
	free( ws->scratch.buf );
    free( (char *)ws );
}


void
phg_wsx_update_ws_rect( ws )
    Ws		*ws;
{
    XWindowAttributes   wattr;

    XGetWindowAttributes( ws->display, ws->drawable_id, &wattr );
    WS_SET_WS_RECT( ws, &wattr )
}


#define LEN_X(l)	((l)->x_max - (l)->x_min)
#define LEN_Y(l)	((l)->y_max - (l)->y_min)
#define LEN_Z(l)	((l)->z_max - (l)->z_min)

void
phg_wsx_compute_ws_transform( ws_win, ws_vp, ws_xform )
    Plimit3	*ws_win;
    Plimit3	*ws_vp;
    Ws_xform	*ws_xform;
{
    float	sx, sy, sz, sxy;	/* scale factors */

    /* Compute the scale factors along each axis and the
     * chose the smallest factor because we want to
     * scale each axis by the same amount to preserve the
     * XY aspect ratio; the Z scale maps the full extent of the
     * ws. window to the ws. viewport.
     */
    sx = LEN_X(ws_vp)/LEN_X(ws_win);
    sy = LEN_Y(ws_vp)/LEN_Y(ws_win);
    sz = LEN_Z(ws_vp)/LEN_Z(ws_win);

    sxy = MIN(sx, sy);

    /*
     * Compute the scale and offest as follows:
     * 		1) translate the ws window to the origin
     * 		2) scale by the scale factor
     * 		3) translate to ws viewport.
     *
     * The resulting matrix has just scale and translation components
     * which can be expressed as:
     */
    ws_xform->scale.x = ws_xform->scale.y = sxy;
    ws_xform->scale.z = sz;

    ws_xform->offset.x = ws_vp->x_min - (ws_win->x_min * sxy);
    ws_xform->offset.y = ws_vp->y_min - (ws_win->y_min * sxy);
    ws_xform->offset.z = ws_vp->z_min - (ws_win->z_min * sz);
}

#undef LEN_X
#undef LEN_Y
#undef LEN_Z


int
phg_wsx_build_exposure_rects( display, window, ws, first_event, prects, xrects )
    Display		*display;
    Window		window;
    Ws			*ws;
    XEvent		*first_event;
    pexDeviceRect	**prects;
    XRectangle		**xrects;
{
    int			x, y, num_rects = 0;
    unsigned int	w, h, bw, depth;
    int			count;
    Window		root_win;
    XEvent		event;
    unsigned		size;

    register	int		i;
    register	XEvent		*evp = first_event;
    register	pexDeviceRect	*pex_rects;
    register	XRectangle	*x_rects;

    /* For an expose event, we want to collect up all of the exposed
     * regions and translate them into absolute coordinates.
     */

    count = 1 + evp->xexpose.count;
    size = count * (sizeof(XRectangle) + sizeof(pexDeviceRect));
    if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
	ERR_BUF( ws->erh, ERR900 );
	return 0;
    }
    x_rects = (XRectangle *)ws->scratch.buf;

    XGetGeometry(display, (Drawable)window,
	&root_win, &x, &y, &w, &h, &bw, &depth);

    /* Collect the exposed regions. */
    i = 0;
    do {
	if ( i >= count ) {
	    /* evp->xexpose.count is unreliable. */
	    size = (count *=2) *  (sizeof(XRectangle) + sizeof(pexDeviceRect));
	    if ( !PHG_SCRATCH_SPACE( &ws->scratch, size ) ) {
		ERR_BUF( ws->erh, ERR900 );
		return 0;
	    }
	    x_rects = (XRectangle *)ws->scratch.buf;
	}

	x_rects[i].x = evp->xexpose.x;
	x_rects[i].y = evp->xexpose.y;
	x_rects[i].width = evp->xexpose.width;
	x_rects[i].height = evp->xexpose.height;
	i++;
    } while (XCheckTypedWindowEvent(display, window, Expose, evp = &event));
    num_rects = i;

    /* Build the PEX regions. */
    pex_rects = (pexDeviceRect *)(x_rects + num_rects);
    for ( i = 0; i < num_rects; i++ ) {
	pex_rects[i].xmin = x_rects[i].x;
	pex_rects[i].xmax = pex_rects[i].xmin + x_rects[i].width;
	pex_rects[i].ymax = h - x_rects[i].y;
	pex_rects[i].ymin = pex_rects[i].ymax - x_rects[i].height;
    }

    *xrects = x_rects;
    *prects = pex_rects;
    return num_rects;
}

void
phg_wsx_convert_rects( ws, num_rects, x_rects, pex_rects )
    Ws			*ws;
    int			num_rects;
    XRectangle		*x_rects;
    pexDeviceRect	*pex_rects;
{
    int                 x, y;
    unsigned int        w, h, bw, depth;
    Window              root_win;

    register int	i;

    XGetGeometry( ws->display, ws->drawable_id,
	&root_win, &x, &y, &w, &h, &bw, &depth);

    for ( i = 0; i < num_rects; i++ ) {
	pex_rects[i].xmin = x_rects[i].x;
	pex_rects[i].xmax = pex_rects[i].xmin + x_rects[i].width;
	pex_rects[i].ymax = h - x_rects[i].y;
	pex_rects[i].ymin = pex_rects[i].ymax - x_rects[i].height;
    }
}
