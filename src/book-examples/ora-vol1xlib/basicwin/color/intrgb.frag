	XStandardColormap \f(CWbest_map_info\fP;
	unsigned long red, green, blue;
	unsigned long pixelvalue;
	int status;

	status = \f(CWXGetStandardColormap\fP(display, \f(CWRootWindow\fP(display, 
		screen), &best_map_info, XA_RGB_BEST_MAP);

	if (!status)
		{
		printf("%s: specified standard colormap not available", 
				argv[0]);
		exit(-1);
		}

	pixelvalue = \f(CWbest_map_info\fP.base_pixel +
			(red * \f(CWbest_map_info\fP.red_mult) +
			(green * \f(CWbest_map_info\fP.green_mult) +
			(blue * \f(CWbest_map_info\fP.blue_mult);
