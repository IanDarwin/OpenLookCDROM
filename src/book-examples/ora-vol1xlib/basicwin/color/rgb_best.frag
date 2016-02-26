	Display *display;
	int screen;
	XStandardColormap best_map_info;	   /* structure to fill */
	unsigned long whitepixel;	   /* computed pixel value for white */
	Colormap colormap;
	Window window;
	Status status;
	XSetWindowAttributes attrib;	   /* so we can set colormap */
	unsigned long attribmask;

	/* Open Display, etc. */

	if (status = XGetStandardColormap(display, RootWindow(display, 
			screen), &best_map_info, XA_RGB_BEST_MAP) == 0);
		printf("%s: can't get standard colormap", argv[0]);

	attrib.colormap = best_map_info.colormap;

	whitepixel = best_map_info.base_pixel  +
			(best_map_info.red_max * best_map_info.red_mult) +
			(best_map_info.green_max * best_map_info.green_mult) +
			(best_map_info.blue_max * best_map_info.blue_mult);

	attrib.background_pixel = whitepixel;

	attribmask = CWBackPixel | CWColormap;

	XChangeWindowAttributes(display, window, attribmask, &attrib);
