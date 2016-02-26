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
extern unsigned long foreground, background_pixel, overlay_pixel_1, overlay_pixel_2;
extern unsigned long overlay_plane_mask;

#define MAX_COLORS 2
#define MAX_PLANES 1
#define MAX_CELLS 4       /* MAX_COLORS * 2 ^ MAX_PLANES */
#define CANNOT_OVERLAY 0
#define CAN_OVERLAY 1

static char *visual_class[] = {
"StaticGray",
"GrayScale",
"StaticColor",
"PseudoColor",
"TrueColor",
"DirectColor"
};

int
get_colors()
{
        int default_depth;
	static char *name[] = {"Red", "Yellow", "Green", "Green"};
	XColor exact_defs[MAX_CELLS];
	Colormap default_cmap;
	int ncolors = 4;
	int plane_masks[MAX_PLANES];
	int colors[MAX_COLORS];
	int i;
	XVisualInfo visual_info;
	int class;

	default_depth = DefaultDepth(display, screen_num);
	default_cmap   = DefaultColormap(display, screen_num);
	if (default_depth == 1) {
		/* must be StaticGray, use black and white */
		background_pixel = WhitePixel(display, screen_num);
		foreground = BlackPixel(display, screen_num);
		printf("using black and white\n");
		return(CANNOT_OVERLAY);
	}

	if (!XMatchVisualInfo(display, screen_num, default_depth, PseudoColor, &visual_info)) {
		if (!XMatchVisualInfo(display, screen_num, default_depth, DirectColor, &visual_info)) {
		/* No PseudoColor or TrueColor visual available at default_depth.
		 * Some applications might try for a GrayScale visual 
		 * here if they can use gray to advantage, before 
		 * giving up and using black and white.
		 */
                background_pixel = WhitePixel(display, screen_num);
               	foreground = BlackPixel(display, screen_num);
		printf("using black and white\n");
		return(CANNOT_OVERLAY);
		}
	}

	/* got PseudoColor or TrueColor visual at default depth */

	/* The visual we found is not necessarily the 
	 * default visual, and therefore it is not necessarily
	 * the one we used to create our window.  However,
	 * we now know for sure that color is supported, so the
	 * following code will work (or fail in a controlled way).
	 */

   	if (XAllocColorCells (display, default_cmap, False, plane_masks, 1, colors, 2) == 0) {
		/* Can't get enough read/write cells to overlay.
		 * Try at least to get three colors. */
   		if (XAllocColorCells (display, default_cmap, False, plane_masks, 0, colors, 3) == 0) {
			/* Can't even get that.  Give up and
			 * use black and white */
               		background_pixel = WhitePixel(display, screen_num);
               		foreground = BlackPixel(display, screen_num);
			printf("using black and white\n");
			return(CANNOT_OVERLAY);
		}
		else
			ncolors = 3;
	}
      	
	/* allocated three or four colorcells succesfully,
	 * now set their colors - three and four
	 * are set to the same RGB values */
	for (i = 0; i < ncolors; i++)
	{
		if (!XParseColor (display, default_cmap, name[i], &exact_defs[i])) {
			fprintf(stderr, "basic: color name %s not in database", name[i]);
			exit(0);
		}
		/* this needed before calling XStoreColors */
			exact_defs[i].flags = DoRed | DoGreen | DoBlue;
	}
	printf("got RGB values\n");

	/* set pixel value in struct to the allocated ones */
	exact_defs[0].pixel = colors[0];
	exact_defs[1].pixel = colors[1];
	exact_defs[2].pixel = colors[0] | plane_masks[0];
	exact_defs[3].pixel = colors[1] | plane_masks[0];

	/* this sets the color of the read/write cells */
	XStoreColors (display, default_cmap, exact_defs, ncolors);
	printf("stored colors\n");

	background_pixel = exact_defs[0].pixel;
	foreground = exact_defs[1].pixel;
	printf("set f and g\n");
	if (ncolors == 4) {
               	overlay_pixel_1 = exact_defs[2].pixel;
               	overlay_pixel_2 = exact_defs[3].pixel;
               	overlay_plane_mask = plane_masks[0];
		printf("set can\n");
		return(CAN_OVERLAY);
	}
	else {
		/* this must be used as a normal color, not overlay */
		overlay_pixel_1 = exact_defs[2].pixel;
		printf("set can't\n");
		return(CANNOT_OVERLAY);
	}
}
