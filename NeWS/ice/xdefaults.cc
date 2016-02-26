/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern "C" {
char *			strcpy(char *, char *);
int			strlen(char *);
char *			strncmp(char *, char *, int);
char *			strncpy(char *, char *, int);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);

void
get_xdefaults(char *name)
{
	char *g, *f, *bw, *fgc, *bgc, *bdrc;
	int geom_mask, screen;
	XColor fg, bg, bdr;

	/* geometry */
	pg_framewidth= PAGE_FRAMEWIDTH;
	pg_frameheight= PAGE_FRAMEHEIGHT;
	pg_frameleft= pg_frametop= 100;
	if ((g= XGetDefault(dpy, name, "Geometry")) == (char *) NULL)
		(void) strcpy(geometry, GEOMETRY);
	else
		(void) strncpy(geometry, g, MAX_GEOMLEN);
	geom_mask= XParseGeometry(geometry, &pg_frameleft, &pg_frametop, &pg_framewidth, &pg_frameheight);

	/* font */
	if ((f= XGetDefault(dpy, name, "Font")) == (char *) NULL)
		(void) strcpy(fontnm, FONT);
	else
		(void) strncpy(fontnm, f, MAX_FILENMLEN);
	if ((font= XLoadQueryFont(dpy, fontnm)) == NULL) {
		(void) fprintf(stderr, "%s: cannot load font %s\n", progname, DisplayString(dpy), fontnm);
		exit(-1);
	}
	fontht= font->max_bounds.ascent+font->max_bounds.descent;
	charwd= XTextWidth(font, "X", strlen("X"));

	/* border width */
	if ((bw= XGetDefault(dpy, name, "BorderWidth")) == (char *) NULL)
		border_wd= BORDER_WIDTH;
	else {
		border_wd= atoi(bw);
		if (border_wd <= 0)
			border_wd= BORDER_WIDTH;
	}

	/* foreground, background and border colors */
	screen= DefaultScreen(dpy);
	fg_pixel= bdr_pixel= black_pixel;
	bg_pixel= white_pixel;
	fgc= XGetDefault(dpy, name, "Foreground");
	bgc= XGetDefault(dpy, name, "Background");
	bdrc= XGetDefault(dpy, name, "Border");
	switch (DefaultDepth(dpy, screen)) {
	case 1:
		if (fgc) {
			if (!strncmp(fgc, "w", 1) || !strncmp(fgc, "W", 1)) {
				fg_pixel= white_pixel;
				bg_pixel= black_pixel;
			}
			else {
				fg_pixel= black_pixel;
				bg_pixel= white_pixel;
			}
		}
		else if (bgc) {
			if (!strncmp(bgc, "w", 1) || !strncmp(bgc, "W", 1)) {
				bg_pixel= white_pixel;
				fg_pixel= black_pixel;
			}
			else {
				bg_pixel= black_pixel;
				fg_pixel= white_pixel;
			}
		}
		if (bdrc) {
			if (!strncmp(bdrc, "w", 1) ||  !strncmp(bdrc, "W", 1))
				bdr_pixel= white_pixel;
			else
				bdr_pixel= black_pixel;
		}
		break;

	case 8:
		if (fgc && XParseColor(dpy, cmap, fgc, &fg))
			fg_pixel= cmap_lookup((unsigned char) (fg.red >> 8), (unsigned char) (fg.green >> 8), (unsigned char) (fg.blue >> 8), PSEUDOCOLOR_MAPSZ);
		if (bgc && XParseColor(dpy, cmap, bgc, &bg))
			bg_pixel= cmap_lookup((unsigned char) (bg.red >> 8), (unsigned char) (bg.green >> 8), (unsigned char) (bg.blue >> 8), PSEUDOCOLOR_MAPSZ);
		if (bdrc && XParseColor(dpy, cmap, bdrc, &bdr))
			bdr_pixel= cmap_lookup((unsigned char) (bdr.red >> 8), (unsigned char) (bdr.green >> 8), (unsigned char) (bdr.blue >> 8), PSEUDOCOLOR_MAPSZ);
		break;
	default:
		break;
	}
	return;
}
