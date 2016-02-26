/*
 * Copyright 1989 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <stdio.h>

extern Display *display;
extern int screen_num;
extern unsigned long foreground_pixel, background_pixel, border_pixel;

#define MAX_COLORS 3

get_colors()
{
        int default_depth;
        Visual *default_visual;
	static char *name[] = {"Red", "Yellow", "Green"};
	XColor exact_defs[MAX_COLORS];
	Colormap default_cmap;
	int ncolors = MAX_COLORS;
	int plane_masks[1];
	int colors[MAX_COLORS];
	int i;
	XVisualInfo visual_info;
	int class;

	class = PseudoColor;
	default_depth = DefaultDepth(display, screen_num);
        default_visual = DefaultVisual(display, screen_num);
	default_cmap   = DefaultColormap(display, screen_num);
	if (default_depth == 1) {
		/* must be StaticGray, use black and white */
		border_pixel = BlackPixel(display, screen_num);
		background_pixel = WhitePixel(display, screen_num);
		foreground_pixel = BlackPixel(display, screen_num);
		return(0);
	}

	if (!XMatchVisualInfo(display, screen_num, default_depth, PseudoColor, &visual_info)) {
		if (!XMatchVisualInfo(display, screen_num, default_depth, DirectColor, &visual_info)) {
		/* No PseudoColor visual available at default_depth.
		 * Some applications might try for a GrayScale visual 
		 * here if they can use gray to advantage, before 
		 * giving up and using black and white.
		 */
		border_pixel = BlackPixel(display, screen_num);
		background_pixel = WhitePixel(display, screen_num);
		foreground_pixel = BlackPixel(display, screen_num);
		return(0);
		}
	}

	/* got PseudoColor visual at default_depth */

	/* The visual we found is not necessarily the 
	 * default visual, and therefore it is not necessarily
	 * the one we used to create our window.  However,
	 * we now know for sure that color is supported, so the
	 * following code will work (or fail in a controlled way).
	 */

	/* allocate as many cells as we can */
	ncolors = MAX_COLORS;
	while (1) {
   		if (XAllocColorCells (display, default_cmap, False, plane_masks, 0, colors, ncolors))
      			break;
	ncolors--;
	if (ncolors = 0)
		fprintf(stderr, "basic: couldn't allocate read/write colors\n");
		exit(0);
	}

	printf("basic: allocated %d read/write color cells\n", ncolors);

	for (i = 0; i < ncolors; i++) {
			if (!XParseColor (display, default_cmap, name[i], &exact_defs[i])) {
			fprintf(stderr, "basic: color name %s not in database", name[i]);
			exit(0);
		}

		/* set pixel value in struct to the allocated one */
		exact_defs[i].pixel = colors[i];
	}

	/* this sets the color of read/write cell */
	XStoreColors (display, default_cmap, exact_defs, ncolors);
	border_pixel = colors[0];
	background_pixel = colors[1];
	foreground_pixel = colors[2];
}
