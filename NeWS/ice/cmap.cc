/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Raster.h"

extern "C" {
void		bcopy(char *, char *, int);
}

extern void	ice_err(char *, int);

XColor colors[PSEUDOCOLOR_MAPSZ];

void
cmap_defcolors(Colormap map, int size)
{
	int i;

	for (i= 0; i < size; i++)
		def_colors[i].pixel= i;
	XQueryColors(dpy, map, def_colors, size);
	return;
}

void
cmap_copyrange(Colormap amap, Colormap bmap, int start, int size)
{
	int i;

	for (i= 0; i < size; i++)
		colors[i].pixel= i+start;
	XQueryColors(dpy, amap, colors, size);
	XStoreColors(dpy, bmap, colors, size);
	return;
}

void
cmap_copy(Colormap amap, Colormap bmap)
{
	int i;

	for (i= 0; i < PSEUDOCOLOR_MAPSZ; i++)
		colors[i].pixel= i;
	XQueryColors(dpy, amap, colors, PSEUDOCOLOR_MAPSZ);

	/* preserve fg and bg so LXT objects remain readable! */
	bcopy((char *) &(def_colors[(int) fg_pixel]), (char *) &(colors[(int) fg_pixel]), sizeof(XColor));
	bcopy((char *) &(def_colors[(int) bg_pixel]), (char *) &(colors[(int) bg_pixel]), sizeof(XColor));

	XStoreColors(dpy, bmap, colors, PSEUDOCOLOR_MAPSZ);
	return;
}

unsigned long
cmap_lookup(unsigned char r, unsigned char g, unsigned char b, int size)
{
	unsigned long pix;
	int i, mindiff, rdiff, gdiff, bdiff, diff;

	pix= 0;
	i= 0;
	if ((r == (def_colors[i].red >> 8)) &&
	    (g == (def_colors[i].green >> 8)) &&
	    (b == (def_colors[i].blue >> 8)))
		return (unsigned long) i;
	rdiff= r-(def_colors[i].red >> 8);
	if (rdiff < 0)
		rdiff= -rdiff;
	gdiff= g-(def_colors[i].green >> 8);
	if (gdiff < 0)
		gdiff= -gdiff;
	bdiff= b-(def_colors[i].blue >> 8);
	if (bdiff < 0)
		bdiff= -bdiff;
	mindiff= rdiff+gdiff+bdiff;

	for (i++; i < size; i++) {
		if ((r == (def_colors[i].red >> 8)) &&
		    (g == (def_colors[i].green >> 8)) &&
		    (b == (def_colors[i].blue >> 8)))
			return (unsigned long) i;
		rdiff= r-(def_colors[i].red >> 8);
		if (rdiff < 0)
			rdiff= -rdiff;
		gdiff= g-(def_colors[i].green >> 8);
		if (gdiff < 0)
			gdiff= -gdiff;
		bdiff= b-(def_colors[i].blue >> 8);
		if (bdiff < 0)
			bdiff= -bdiff;
		diff= rdiff+gdiff+bdiff;
		if (mindiff > diff) {
			mindiff= diff;
			pix= (unsigned long) i;
		}
	}

	return pix;
}

void
cmapdef_proc(Menu *m, Menu_item *mi)
{
	if (curr_cmap == def_cmap) {
		ice_err("Default colormap is already active.", NONFATAL);
		return;
	}

	cmap_copy(def_cmap, cmap);
	curr_cmap= def_cmap;

	return;
}

void
cmapras_proc(Menu *m, Menu_item *mi)
{
	Raster *ras;
	Colormap map;

	if ((ras= (Raster *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Raster *) NULL) {
		ice_err("Cannot locate selected raster.", NONFATAL);
		return;
	}
	if ((map= ras->getcolormap()) == None) {
		ice_err("Cannot locate raster colormap.", NONFATAL);
		return;
	}
	if (curr_cmap == map) {
		ice_err("Selected raster colormap is already active.", NONFATAL);
		return;
	}

	cmap_copy(map, cmap);
	curr_cmap= map;

	return;
}
