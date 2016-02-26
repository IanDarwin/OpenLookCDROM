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
#include <X11/Xatom.h>

#include <stdio.h>

#include "../bitmaps/icon_bitmap"

#define BITMAPDEPTH 1
#define TOO_SMALL 0
#define BIG_ENOUGH 1

/* These are used as arguments to nearly every Xlib routine, so it saves 
 * routine arguments to declare them global.  If there were 
 * additional source files, they would be declared extern there. */
Display *display;
int screen_num;

static char *progname; /* name this program was invoked by */

void main(argc, argv)
int argc;
char **argv;
{
	Window win, wint;
	unsigned int width, height;	/* window size */
	int x, y; 	/* window position */
	unsigned int border_width = 4;	/* four pixels */
	unsigned int display_width, display_height;
	unsigned int icon_width, icon_height;
	char *window_name = "Basic Window Program";
	char *icon_name = "basicwin";
	Pixmap icon_pixmap;
	XSizeHints size_hints;
	XIconSize *size_list;
	int count;
	XEvent report;
	GC gc;
	XFontStruct *font_info;
	char *display_name = NULL;
	int window_size = BIG_ENOUGH;	/* or TOO_SMALL to display contents */
        XSetWindowAttributes attrib;
        unsigned long valuemask;


	progname = argv[0];

	/* connect to X server */
	if ( (display=XOpenDisplay(display_name)) == NULL )
	{
		(void) fprintf( stderr, "%s: cannot connect to X server %s\n", 
				progname, XDisplayName(display_name));
		exit( -1 );
	}

	/* get screen size from display structure macro */
	screen_num = DefaultScreen(display);
	display_width = DisplayWidth(display, screen_num);
	display_height = DisplayHeight(display, screen_num);

	/* Note that in a real application, x and y would default to 0
	 * but would be settable from the command line or resource database.  
	 */
	x = y = 0;

	/* size window with enough room for text */
	width = display_width/3, height = display_height/4;

	/* create opaque window */
	win = XCreateSimpleWindow(display, RootWindow(display,screen_num), 
			x, y, width, height, border_width, BlackPixel(display,
			screen_num), WhitePixel(display,screen_num));

        attrib.background_pixmap = ParentRelative;
        valuemask = CWBackPixmap;
        XChangeWindowAttributes(display, win, valuemask, &attrib);

        attrib.win_gravity = NorthEastGravity;
        attrib.background_pixmap = None;
        valuemask = CWWinGravity | CWBackPixmap;
        wint = XCreateSimpleWindow(display, win, 2 * width/3,0, width/3,
		height/3, border_width, BlackPixel(display,screen_num), 
		WhitePixel(display,screen_num));

        XChangeWindowAttributes(display, wint, valuemask, &attrib);

	/* Create pixmap of depth 1 (bitmap) for icon */
	icon_pixmap = XCreateBitmapFromData(display, win, icon_bitmap_bits, 
			icon_bitmap_width, icon_bitmap_height);

	/* Set size hints for window manager.  The window manager may
	 * override these settings.  Note that in a real
	 * application if size or position were set by the user
	 * the flags would be UPosition and USize, and these would
	 * override the window manager's preferences for this window. */

#ifdef X11R3
	size_hints.flags = PPosition | PSize | PMinSize;
	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = width;
	size_hints.height = height;
	size_hints.min_width = 300;
	size_hints.min_height = 200;

#else /* X11R4 or later */
	/* x, y, width, and height hints are now taken from
	 * the actual settings of the window when mapped. Note
	 * that PPosition and PSize must be specified anyway. */
	size_hints.flags = PPosition | PSize | PMinSize;
	size_hints.min_width = 300;
	size_hints.min_height = 200;
#endif

#ifdef X11R3
	/* set Properties for window manager (always before mapping) */
	XSetStandardProperties(display, win, window_name, icon_name, 
			icon_pixmap, argv, argc, &size_hints);

#else /* X11R4 or later */
	{
	XWMHints wm_hints;
	XClassHint class_hints;

	/* format of the window name and icon name 
	 * arguments has changed in R4 */
	XTextProperty windowName, iconName;
	/* These calls store window_name and icon_name into
	 * XTextProperty structures and set their other 
	 * fields properly. */
	if (XStringListToTextProperty(&window_name, 1, &windowName) == 0) {
		(void) fprintf( stderr, "%s: structure allocation for windowName failed.\n", 
				progname);
		exit(-1);
	}
		
	if (XStringListToTextProperty(&icon_name, 1, &iconName) == 0) {
		(void) fprintf( stderr, "%s: structure allocation for iconName failed.\n", 
				progname);
		exit(-1);
	}

	wm_hints.initial_state = NormalState;
	wm_hints.input = True;
	wm_hints.icon_pixmap = icon_pixmap;
	wm_hints.flags = StateHint | IconPixmapHint | InputHint;

	class_hints.res_name = progname;
	class_hints.res_class = "Basicwin";

	XSetWMProperties(display, win, &windowName, &iconName, 
			argv, argc, &size_hints, &wm_hints, 
			&class_hints);
	}
#endif

	/* Select event types wanted */
	XSelectInput(display, win, ExposureMask | KeyPressMask | 
			ButtonPressMask | StructureNotifyMask);


	load_font(&font_info);

	/* create GC for text and drawing */
	getGC(win, &gc, font_info);

	/* Display window */
	XMapWindow(display, win);
	XMapWindow(display, wint);

	/* get events, use first to display text and graphics */
	while (1)  {
		XNextEvent(display, &report);
		switch  (report.type) {
		case Expose:
			/* unless this is the last contiguous expose,
			 * don't draw the window */
			if (report.xexpose.count != 0)
				break;

			/* if window too small to use */
			if (window_size == TOO_SMALL)
				TooSmall(win, gc, font_info);
			else {
				/* place text in window */
				draw_text(win, gc, font_info, width, height);

				/* place graphics in window, */
				draw_graphics(win, gc, width, height);
			}
			break;
		case ConfigureNotify:
			/* window has been resized, change width and
			 * height to send to draw_text and draw_graphics
			 * in next Expose */
			width = report.xconfigure.width;
			height = report.xconfigure.height;
			if ((width < size_hints.min_width) || 
					(height < size_hints.min_height))
				window_size = TOO_SMALL;
			else
				window_size = BIG_ENOUGH;
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

getGC(win, gc, font_info)
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
	static char dash_list[] = {12, 24};
	int list_length = 2;

	/* Create default Graphics Context */
	*gc = XCreateGC(display, win, valuemask, &values);

	/* specify font */
	XSetFont(display, *gc, font_info->fid);

	/* specify black foreground since default window background is 
	 * white and default foreground is undefined. */
	XSetForeground(display, *gc, BlackPixel(display,screen_num));

	/* set line attributes */
	XSetLineAttributes(display, *gc, line_width, line_style, 
			cap_style, join_style);

	/* set dashes */
	XSetDashes(display, *gc, dash_offset, dash_list, list_length);
}

load_font(font_info)
XFontStruct **font_info;
{
	char *fontname = "9x15";

	/* Load font and get font information structure. */
	if ((*font_info = XLoadQueryFont(display,fontname)) == NULL)
	{
		(void) fprintf( stderr, "%s: Cannot open 9x15 font\n", 
				progname);
		exit( -1 );
	}
}

draw_text(win, gc, font_info, win_width, win_height)
Window win;
GC gc;
XFontStruct *font_info;
unsigned int win_width, win_height;
{
	char *string1 = "Hi! I'm a window, who are you?";
	char *string2 = "To terminate program; Press any key";
	char *string3 = "or button while in this window.";
	char *string4 = "Screen Dimensions:";
	int len1, len2, len3, len4;
	int width1, width2, width3;
	char cd_height[50], cd_width[50], cd_depth[50];
	int font_height;
	int initial_y_offset, x_offset;


	/* need length for both XTextWidth and XDrawString */
	len1 = strlen(string1);
	len2 = strlen(string2);
	len3 = strlen(string3);

	/* get string widths for centering */
	width1 = XTextWidth(font_info, string1, len1);
	width2 = XTextWidth(font_info, string2, len2);
	width3 = XTextWidth(font_info, string3, len3);

	font_height = font_info->ascent + font_info->descent;

	/* output text, centered on each line */
	XDrawString(display,win,gc,(win_width - width1)/2, font_height, string1, len1);
	XDrawString(display,win,gc,(win_width - width2)/2, (int)(win_height - 35),string2,len2);
	XDrawString(display,win,gc,(win_width - width3)/2, (int)(win_height - 15),string3,len3);

	/* copy numbers into string variables */
	(void) sprintf(cd_height, " Height - %d pixels", 
			DisplayHeight(display,screen_num));
	(void) sprintf(cd_width, " Width  - %d pixels", 
			DisplayWidth(display,screen_num));
	(void) sprintf(cd_depth, " Depth  - %d plane(s)", 
			DefaultDepth(display, screen_num));

	/* reuse these for same purpose */
	len4 = strlen(string4);
	len1 = strlen(cd_height);
	len2 = strlen(cd_width);
	len3 = strlen(cd_depth);

	/* To center strings vertically, we place the first string
	 * so that the top of it is two font_heights above the center
	 * of the window.  Since the baseline of the string is what we
	 * need to locate for XDrawString, and the baseline is one
	 * font_info->ascent below the top of the character,
	 * the final offset of the origin up from the center of the 
	 * window is one font_height + one descent. */

	initial_y_offset = win_height/2 - font_height - font_info->descent;
	x_offset = (int) win_width/4;
	XDrawString(display, win, gc, x_offset, (int) initial_y_offset, 
			string4,len4);

	XDrawString(display, win, gc, x_offset, (int) initial_y_offset + 
			font_height,cd_height,len1);
	XDrawString(display, win, gc, x_offset, (int) initial_y_offset + 
			2 * font_height,cd_width,len2);
	XDrawString(display, win, gc, x_offset, (int) initial_y_offset + 
			3 * font_height,cd_depth,len3);
}

draw_graphics(win, gc, window_width, window_height)
Window win;
GC gc;
unsigned int window_width, window_height;
{
	int x, y;
	int width, height;

	height = window_height/2;
	width = 3 * window_width/4;
	x = window_width/2 - width/2;  /* center */
	y = window_height/2 - height/2;
	XDrawRectangle(display, win, gc, x, y, width, height);
}

TooSmall(win, gc, font_info)
Window win;
GC gc;
XFontStruct *font_info;
{
	char *string1 = "Too Small";
	int y_offset, x_offset;

	y_offset = font_info->ascent + 2;
	x_offset = 2;

	/* output text, centered on each line */
	XDrawString(display, win, gc, x_offset, y_offset, string1, 
			strlen(string1));
}
