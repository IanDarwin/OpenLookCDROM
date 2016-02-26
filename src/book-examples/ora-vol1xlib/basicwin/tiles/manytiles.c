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

#include "../bitmaps/icon_bitmap"
#define BITMAPDEPTH 1

/* Display and screen are used as arguments to nearly every Xlib routine, 
 * so it simplifies routine calls to declare them global.  If there were 
 * additional source files, these variables would be declared extern in
 * them. */
Display *display;
int screen;

char *ProgramName;

/* values for window_size in main, is window big enough to be useful? */
#define SMALL 1
#define OK 0
#define NUMSTIPS  40

static char *filename[] = {
"1x1",
"2x2",
"black",
"boxes",
"cntr_ptr",
"cntr_ptrmsk",
"cross_weave",
"dimple1",
"dimple3",
"dot",
"flagdown",
"flagup",
"flipped_gray",
"gray",
"gray1",
"gray3",
"icon",
"left_ptr",
"left_ptrmsk",
"light_gray",
"opendot",
"opendotMask",
"right_ptr",
"right_ptrmsk",
"root_weave",
"scales",
"sipb",
"star",
"starMask",
"stipple",
"target",
"tie_fighter",
"wide_weave",
"weird_size",
"wingdogs",
"woman",
"xfd_icon",
"xlogo16",
"xlogo32",
"xlogo64"
};

usage ()
{
    fprintf (stderr, 
             "usage:  %s [-display host:server.screen] [-geometry geom]\n", ProgramName);
    exit (1);
}


void main(argc, argv)
int argc;
char **argv;
{
	Window win;
	unsigned int width, height, x, y;     /* window size and position */
	unsigned int borderwidth = 4;	      /* four pixels */
	int i;   /* for counter */
	unsigned int display_width, display_height;
	unsigned int icon_width, icon_height;
	char *window_name = "Basic Window Program";
	char *icon_name = "basicwin";
	Pixmap icon_pixmap;
	Pixmap stipple[NUMSTIPS];
	unsigned int stip_width[NUMSTIPS], stip_height[NUMSTIPS];
	char fullfilename[256];
	XSizeHints size_hints;
	XEvent report;
	GC gc;
	XFontStruct *font_info;
	char *display_name = NULL;
	char *geom = NULL;

	int window_size = 0;    /* OK, or too SMALL to display contents */

	ProgramName = argv[0];
	for (i = 1; i < argc; i++) {
	char *arg = argv[i];
	
	if (arg[0] == '-') {
	switch (arg[1]) {
		case 'd':                 /* -display host:server.screen */
			if (++i >= argc) usage ();
			display_name = argv[i];
			continue;
		case 'g':                 /* -geometry geom */
			if (++i >= argc) usage ();
			geom = argv[i];
			continue;
		default:
			usage ();
		/* doesn't return */
		}
	} else
		/* get bitmap file names here */
		usage ();
	}

	/* connect to X server */

	if ( (display=XOpenDisplay(display_name)) == NULL )
	{
		(void) fprintf( stderr, 
				"basicwin: cannot connect to X server %s\\n",
				XDisplayName(display_name));
		exit( -1 );
	}



	/* get screen size from display structure macro */
	screen = DefaultScreen(display);
	display_width = DisplayWidth(display, screen);
	display_height = DisplayHeight(display, screen);

	/* place window */
	x = 0, y = 0;

	/* set filenames */

	for (i = 0; i < NUMSTIPS; i++) {
		strcpy(fullfilename, "/usr/include/X11/bitmaps/");
		strcat(fullfilename, filename[i]);
		if (create_read_stipple(&stipple[i], fullfilename, &stip_width[i], &stip_height[i]) != BitmapSuccess) {
			fprintf(stderr, "basic: can't read bitmap %d\n",i);
			exit(0);
		}
	}

	/* size window with enough room for text */
	width = display_width/2, height = 4 * display_height/5;

	/* create opaque window */
	win = XCreateSimpleWindow(display, RootWindow(display,screen), x, y, 
			width, height, borderwidth, BlackPixel(display,
	    		screen), WhitePixel(display,screen));


	/* Create pixmap of depth 1 (bitmap) for icon */
	icon_pixmap = XCreateBitmapFromData(display, win, icon_bitmap_bits, 
			icon_bitmap_width, icon_bitmap_height);


	/* Set resize hints */
	size_hints.flags = PPosition | PSize | PMinSize;
	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = width;
	size_hints.height = height;
	size_hints.min_width = 350;
	size_hints.min_height = 250;

	/* set Properties for window manager (always before mapping) */
	XSetStandardProperties(display, win, window_name, icon_name, 
	    icon_pixmap, argv, argc, &size_hints);


	/* Select event types wanted */
	XSelectInput(display, win, ExposureMask | KeyPressMask | 
			ButtonPressMask | StructureNotifyMask);

	load_font(&font_info);

	/* create GC for text and drawing */
	get_GC(win, &gc, font_info);

	/* Display window */
	XMapWindow(display, win);


	/* get events, use first to display text and graphics */
	while (1)  {
		XNextEvent(display, &report);
		switch  (report.type) {
		case Expose:
			/* get all other Expose events on the queue */
			while (XCheckTypedEvent(display, Expose, &report));
			if (window_size == SMALL)
			       TooSmall(win, gc, font_info);
			else {
				/* place text in window */
			       place_stips(win, gc, stipple, stip_width, stip_height, font_info, width, height);
			}
			break;
		case ConfigureNotify:
			/* window has been resized, change width and
			 * height to send to place_text and place_graphics
			 * in next Expose */
			width = report.xconfigure.width;
			height = report.xconfigure.height;
			if ((width < size_hints.min_width) || 
					(height < size_hints.min_height))
				window_size = SMALL;
			else
				window_size = OK;
			break;
		case ButtonPress:
			/* trickle down into KeyPress (no break) */
		case KeyPress:
			XUnloadFont(display, font_info->fid);
			XFreeGC(display, gc);
			XCloseDisplay(display);
			exit(1);
		default:
			/* all events selected by StructureNotifyMask
			 * except ConfigureNotify are thrown away here,
			 * since nothing is done with them */
			break;
		} /* end switch */
	} /* end while */
}

get_GC(win, gc, font_info)
Window win;
GC *gc;
XFontStruct *font_info;
{
	unsigned long valuemask = 0; /* ignore XGCvalues and use defaults */
	XGCValues values;
	unsigned int line_width = 6;
	int line_style = LineOnOffDash;
	int cap_style = CapRound;
	int join_style = JoinRound;
	int dash_offset = 0;
	static char dash_list[] = {
		12, 24	};
	int list_length = 2;

	/* Create default Graphics Context */
	*gc = XCreateGC(display, win, valuemask, &values);

	/* specify font */
	XSetFont(display, *gc, font_info->fid);

	/* specify black foreground since default may be white on white */
	XSetForeground(display, *gc, BlackPixel(display,screen));
	XSetBackground(display, *gc, WhitePixel(display,screen));
}

load_font(font_info)
XFontStruct **font_info;
{
	char *fontname = "9x15";

	/* Access font */
	if ((*font_info = XLoadQueryFont(display,fontname)) == NULL)
	{
		(void) fprintf( stderr, "Basic: Cannot open 9x15 font\\n");
		exit( -1 );
	}
}


place_stips(win, gc, stipple, stip_width, stip_height, font_info, window_width, window_height)
Window win;
GC gc;
Pixmap stipple[];
unsigned int stip_width[], stip_height[];
XFontStruct *font_info;
unsigned int window_width, window_height;
{
	int i;
	int dest_x, dest_y;
	int string_x = 5, string_y = 12;
	int text_height;
	int direction, ascent, descent;
	XCharStruct overall;

	text_height = font_info->max_bounds.ascent + font_info->max_bounds.descent;

	for (i = 120; i < window_width; i += 120 ) {
		XDrawLine(display, win, gc, i, 0, i, window_height);
	}

	for (i = 94; i < window_height; i += 94 ) {
		XDrawLine(display, win, gc, 0, i, window_width, i);
	}

	for (i = 0; i < NUMSTIPS; i++) {
		if (string_x > window_width - 35)  {
			string_x = 4;
			string_y += 94;
		}
		XDrawString(display, win, gc, string_x, string_y, filename[i], strlen(filename[i]));
		dest_y = string_y + font_info->max_bounds.descent + 4;
		dest_x = string_x + 2;
		XCopyPlane(display, stipple[i], win, gc, 0, 0, stip_width[i], stip_height[i], dest_x, dest_y, 1);
		XTextExtents(font_info, filename[i], strlen(filename[i]),
				&direction, &ascent, &descent, &overall);
		string_x += 120;
	}
	
}

TooSmall(win, gc, font_info)
Window win;
GC gc;
XFontStruct *font_info;
{
	char *string1 = "Too Small";
	int y_offset, x_offset;

	y_offset = font_info->max_bounds.ascent + 2;
	x_offset = 2;

	/* output text, centered on each line */
	XDrawString(display, win, gc, x_offset, y_offset, string1, 
			strlen(string1));
}

#define name_width 16
#define name_height 16
#define name_x_hot	8
#define name_y_hot	8
static char name_bits[] =
	{
	0xf81f, 0xe3c7, 0xcff3, 0x9ff9,
	0xbffd, 0x33cc, 0x7ffe, 0x7ffe,
	0x7e7e, 0x7ffe, 0x37ec, 0xbbdd,
	0x9c39, 0xcff3, 0xe3c7, 0xf81f
	};



create_included_stipple(stip, width, height)
Pixmap *stip; /* returned created stipple */
unsigned int *width, *height;  /* returned */
{
	if (*stip = XCreateBitmapFromData(display, RootWindow(display, screen), 
			name_bits, name_width, name_height) == NULL)
		return(False);
	*width = name_width;
	*height = name_height;
	return(True);
}

create_read_stipple(stip, filename, width, height)
Pixmap *stip;  /* returned created stipple */
char *filename;
unsigned int *width, *height;  /* returned */
{
	int depth = 1;
	int value;
	int x_hot, y_hot;  /* don't care about these unless for cursor */

	value = XReadBitmapFile(display, RootWindow(display, screen), 
			filename, width, height, stip, &x_hot, &y_hot);
	if (value == BitmapFileInvalid)
		fprintf(stderr, "Filename %s contains invalid bitmap data\\n", filename);
	else if (value == BitmapOpenFailed)
		fprintf(stderr, "Filename %s could not be opened\\n", filename);
	else if (value == BitmapNoMemory)
		fprintf(stderr, "Not enough memory to allocate pixmap\\n");
	return(value);
	/* returns BitmapSuccess if everything worked */
}
