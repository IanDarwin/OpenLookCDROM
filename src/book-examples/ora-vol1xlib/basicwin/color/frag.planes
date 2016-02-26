	#define PIXELS 256
	Display *display;
	int screen;
	int contig = False;		/* non-contiguous planes */
	unsigned long pixels[PIXELS];	/* return of pixel values */

	/* number of independent pixel values allocate */
	unsigned int ncolors = PIXELS;	

	XColor defs[2048];	/* PIXELS * 2^maxplanes where maxplanes
		 * is the largest of nred, ngreen, and nblue */

	/* number of planes to allocate for each primary */
	unsigned int nreds = 3, ngreens = 3, nblues = 2;	

	/* returned masks, which bits of pixel value for each primary */
	unsigned long red_mask, green_mask, blue_mask;	

	Colormap colormap;
	Status status;

	/* open display, etc. */
	/* get or create large DirectColor colormap */

	while (status = XAllocColorPlanes(display, colormap, 
			contig, pixels, ncolors, nreds, ngreens, nblues, 
			&red_mask, &green_mask, &blue_mask) == 0) {
		{
		/* Make contig False if it was True,
		 * reduce value of ncolors,
		 * reduce value of nreds, ngreens and/or nblues,
		 * or try allocating new map,
		 * break when you give up */
		break;
		}
	if (status == 0)
		{
		fprintf(stderr, "%s: couldn't allocate requested colorcells", 
				argv[0]);
		exit(-1);
		}

	/* define desired colors in defs */

	while (status = XStoreColors(display, colormap, defs, 
			ncolors) == 0)
		{
		fprintf(stderr, "%s: can't store colors", argv[0]);
		/* try to fix problem here, exit or break */
		exit(-1);
		}
		
	/* draw your shaded stuff! */
