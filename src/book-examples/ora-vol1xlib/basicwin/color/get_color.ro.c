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
extern Screen *screen_ptr;
extern unsigned long foreground_pixel, background_pixel, border_pixel;
extern char *progname;

#define MAX_COLORS 3

static char *visual_class[] = {
"StaticGray",
"GrayScale",
"StaticColor",
"PseudoColor",
"TrueColor",
"DirectColor"
};

get_colors()
{
        int default_depth;
        Visual *default_visual;
	static char *name[] = {"Red", "Yellow", "Green"};
	XColor exact_def;
	Colormap default_cmap;
	int ncolors = 0;
	int colors[MAX_COLORS];
	int i = 5;
	XVisualInfo visual_info;
	
	/* Try to allocate colors for PseudoColor, TrueColor, 
	 * DirectColor, and StaticColor.  Use black and white
	 * for StaticGray and GrayScale */

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

	while (!XMatchVisualInfo(display, screen_num, default_depth, /* visual class */i--, &visual_info))
		;
	printf("%s: found a %s class visual at default_depth.\n", progname, visual_class[++i]);
	
	if (i < 2) {
		/* No color visual available at default_depth.
		 * Some applications might call XMatchVisualInfo
		 * here to try for a GrayScale visual 
		 * if they can use gray to advantage, before 
		 * giving up and using black and white.
		 */
		border_pixel = BlackPixel(display, screen_num);
		background_pixel = WhitePixel(display, screen_num);
		foreground_pixel = BlackPixel(display, screen_num);
		return(0);
	}

	/* otherwise, got a color visual at default_depth */

	/* The visual we found is not necessarily the 
	 * default visual, and therefore it is not necessarily
	 * the one we used to create our window.  However,
	 * we now know for sure that color is supported, so the
	 * following code will work (or fail in a controlled way).
	 * Let's check just out of curiosity: */
	if (visual_info.visual != default_visual)
		printf("%s: PseudoColor visual at default depth is not default visual!\nContinuing anyway...\n", progname);

	for (i = 0; i < MAX_COLORS; i++) {
		printf("allocating %s\n", name[i]);
		if (!XParseColor (display, default_cmap, name[i], &exact_def)) {
			fprintf(stderr, "%s: color name %s not in database", progname, name[i]);
			exit(0);
		}
		printf("The RGB values from the database are %d, %d, %d\n", exact_def.red, exact_def.green, exact_def.blue);
   		if (!XAllocColor(display, default_cmap, &exact_def)) {
			fprintf(stderr, "%s: can't allocate color: all colorcells allocated and no matching cell found.\n", progname);
		exit(0);
		}
		printf("The RGB values actually allocated are %d, %d, %d\n", exact_def.red, exact_def.green, exact_def.blue);
		colors[i] = exact_def.pixel;
		ncolors++;
	}

	printf("%s: allocated %d read-only color cells\n", progname, ncolors);

	border_pixel = colors[0];
	background_pixel = colors[1];
	foreground_pixel = colors[2];
	return(1);
}
