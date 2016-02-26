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
#include <X11/keysym.h>

#include <stdio.h>

#include "../bitmaps/icon_bitmap"
#define BITMAPDEPTH 1
#define SMALL 1
#define OK 0
#define MAX_POPUP_STRING_LENGTH 40
#define MAX_MAPPED_STRING_LENGTH 10

/* These are used as arguments to nearly every Xlib routine, so it saves 
 * routine arguments to declare them global.  If there were 
 * additional source files, they would be declared extern there. */
Display *display;
int screen;

void main(argc, argv)
int argc;
char **argv;
{
	Window win;
	unsigned int width, height, x = 0, y = 0; 	/* window size and position */
	unsigned int border_width = 4;	/* four pixels */
	unsigned int display_width, display_height;
	unsigned int icon_width, icon_height;
	char *window_name = "basicwin";
	char *icon_name = "";
	Pixmap icon_pixmap;
	XSizeHints size_hints;
	XIconSize *size_list;
	int count;
	XEvent report;
	GC gc;
	XFontStruct *font_info;
	char *display_name = NULL;
	int window_size = 0;	/* OK, or too SMALL to display contents */

	/* the following are for pop-up window */
	static Window pop_win;
	char buffer[MAX_MAPPED_STRING_LENGTH];
	int bufsize=MAX_MAPPED_STRING_LENGTH;
	int start_x, start_y;
	KeySym keysym;
	XComposeStatus compose;
	int totalcount;
	unsigned int pop_width, pop_height;
	char string[MAX_POPUP_STRING_LENGTH];
	int popped = False;
	int length;

	/* added for key rebinding test */
KeySym modlist[2];   /* array of modifier keysyms */
unsigned int string_length;
unsigned int list_length;

	/* connect to X server */

	if ( (display=XOpenDisplay(display_name)) == NULL )
	{
		(void) fprintf( stderr, "basicwin: cannot connect to X server %s\n", XDisplayName(display_name));
		exit( -1 );
	}

	/* get screen size from display structure macro */
	screen = DefaultScreen(display);
	display_width = DisplayWidth(display, screen);
	display_height = DisplayHeight(display, screen);

	/* note that x and y are 0, since the default position of the window
	 * is the top left corner of the root. This is fine since the window
	 * manager often allows the user to position the window before mapping it. */

	/* size window with enough room for text */
	width = display_width/3, height = display_height/4;

	/* create opaque window */
	win = XCreateSimpleWindow(display, RootWindow(display,screen), x, y, width, height, border_width, BlackPixel(display,
	    screen), WhitePixel(display,screen));


/* map Shift-F1 to "STOP"  */
string_length = 4;
list_length = 1;
modlist[1] = XK_Shift_R;        /* Do right shift key */
XRebindKeysym(display, XK_F1, modlist, list_length, "STOP", string_length);
modlist[1] = XK_Shift_L;        /* Do left shift key */
XRebindKeysym(display, XK_F1, modlist, list_length, "STOP", string_length);

/* map Control-Shift-F1 to "ABORT"  */
string_length = 5;
list_length = 2;
modlist[1] = XK_Shift_R; modlist[2] = XK_Control_R; /* Both Right Pressed */
XRebindKeysym(display, XK_F1, modlist, list_length, "STOP", string_length);
modlist[1] = XK_Shift_L; modlist[2] = XK_Control_R; /* Left Shift, 
                                        Right Control */
XRebindKeysym(display, XK_F1, modlist, list_length, "STOP", string_length);
modlist[1] = XK_Shift_R; modlist[2] = XK_Control_L; /* Right Shift, 
                                        Left Control */
XRebindKeysym(display, XK_F1, modlist, list_length, "STOP", string_length);
modlist[1] = XK_Shift_L; modlist[2] = XK_Control_L; /* Both Left Pressed */
XRebindKeysym(display, XK_F1, modlist, list_length, "STOP", string_length);


	/* Create pixmap of depth 1 (bitmap) for icon */
	icon_pixmap = XCreateBitmapFromData(display, win, icon_bitmap_bits, icon_bitmap_width, icon_bitmap_height);

	/* Set resize hints */
	size_hints.flags = PPosition | PSize | PMinSize;
	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = width;
	size_hints.height = height;
	size_hints.min_width = 300;
	size_hints.min_height = 200;

	/* set Properties for window manager (always before mapping) */
	XSetStandardProperties(display, win, window_name, icon_name, 
	    icon_pixmap, argv, argc, &size_hints);

	/* Select event types wanted */
	XSelectInput(display, win, ExposureMask | KeyPressMask | ButtonPressMask | StructureNotifyMask);

	load_font(&font_info);

	/* create GC for text and drawing */
	getGC(win, &gc, font_info);

	/* Display window */
	XMapWindow(display, win);

	/* get events, use first to display text and graphics */
	while (1)  {
		XNextEvent(display, &report);
		switch  (report.type) {
		case Expose:
			printf("got expose event\n");
			if (report.xexpose.window == pop_win) {
				if (popped)
					XDrawString(display, pop_win, gc, start_x, 
						start_y, string, strlen(string));
			}
			else { /* it's the main window */
				while (XCheckTypedEvent(display, Expose, &report));
				if (window_size == SMALL)
					TooSmall(win, gc, font_info);
				else {
					/* place text in window */
					place_text(win, gc, font_info, width, height);
	
					/* place graphics in window, */
					place_graphics(win, gc, width, height);
				}
			}
			break;
		case ConfigureNotify:
			printf("got configure event\n");
			/* window has been resized, change width and
			 * height to send to place_text and place_graphics
			 * in next Expose */
			width = report.xconfigure.width;
			height = report.xconfigure.height;
			if ((width < size_hints.min_width) || (height < size_hints.min_height))
				window_size = SMALL;
			else
				window_size = OK;
			break;
		case ButtonPress:
			/* put up popup window */
			if (!pop_win) {  /* create it */
			pop_width = MAX_POPUP_STRING_LENGTH * font_info->max_bounds.width + 4;
			pop_height = font_info->max_bounds.ascent + font_info->max_bounds.descent + 4;
			pop_win = XCreateSimpleWindow(display, win, x, y, pop_width, pop_height, border_width, BlackPixel(display,
	    				screen), WhitePixel(display, screen));
			/* calculate starting position of string in window (start_x, start_y) */
			start_x = 2;
			start_y = font_info->max_bounds.ascent + 2;
			XSelectInput(display, pop_win, ExposureMask | KeyPressMask);
			XMapWindow(display, pop_win);
			popped = True;
			break;
			}
			/* trickle down into KeyPress on second press (no break) */
		case KeyPress:
			if (report.xkey.window == win) {
				XUnloadFont(display, font_info->fid);
				XFreeGC(display, gc);
				XCloseDisplay(display);
				exit(1);
			}
			else {
/* pop-up a dialog box big enough for a line in specified font (which
 * was already opened).  If this is the first call of 
 * this routine, create a window of the appropriate size; if a 
 * repeat call, resize the window to accommodate the prompt 
 * and user-provided text.  Get characters until you encounter
 * a carriage return, return number of characters.  Deal with 
 * backspaces, etc.  Deal with Expose events on all windows 
 * associated with this application.  Deal with keyboard remapping. */

				count = XLookupString(&report, buffer, bufsize, &keysym, 
						&compose);
				/* now need to do the right thing with
				 * every keysym possibility, as minimum: */
				if ((keysym == XK_Return) || (keysym == XK_KP_Enter) || (keysym == XK_Linefeed)) {
					XUnmapWindow(display, pop_win);
					popped = False;
					printf("string is %s\n", string);
					break;
				}
				else if (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9))
				  || ((keysym >= XK_space) && (keysym <= XK_asciitilde)))
 					{
					if (strlen(string) + strlen(buffer)
					   >= MAX_POPUP_STRING_LENGTH)
						XBell(display, 100);
					else 
						strcat(string, buffer);
					}
				else if ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R))
					;/* do nothing because its a modifier key */
				else if ((keysym >= XK_F1) && (keysym <= XK_F35))
					if (buffer == NULL)
						printf("Unmapped function key\n");
					else if (strlen(string) + strlen(buffer)
					   >= MAX_POPUP_STRING_LENGTH)
						XBell(display, 100);
					else 
						strcat(string, buffer);
						
				else if ((keysym == XK_BackSpace) || (keysym == XK_Delete)) {
					if ((length = strlen(string)) > 0) {
						string[length - 1] = NULL;
						XClearWindow(display, pop_win);
					}
					else
						XBell(display, 100);
				}
				else {
					printf("keysym %s is not handled\n", XKeysymToString(keysym));
					XBell(display, 100);
				}
				XDrawString(display, pop_win, gc, start_x, 
					start_y, string, strlen(string));
				/* clear buffer so multi-character strings
				 * are properly supported, even though
				 * they can only be deleted on char at a
				 * time */
				buffer[1] = NULL;
				break;
			}
		case MappingNotify:
		 	XRefreshKeyboardMapping(&report);
			break;
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

	/* specify black foreground since default may be white on white */
	XSetForeground(display, *gc, BlackPixel(display,screen));

	/* set line attributes */
	XSetLineAttributes(display, *gc, line_width, line_style, cap_style, join_style);

	/* set dashes to be line_width in length */
	XSetDashes(display, *gc, dash_offset, dash_list, list_length);
}

load_font(font_info)
XFontStruct **font_info;
{
	char *fontname = "9x15";

	/* Access font */
	if ((*font_info = XLoadQueryFont(display,fontname)) == NULL)
	{
		(void) fprintf( stderr, "Basic: Cannot open 9x15 font\n");
		exit( -1 );
	}
}

place_text(win, gc, font_info, win_width, win_height)
Window win;
GC gc;
XFontStruct *font_info;
unsigned int win_width, win_height;
{
	int y = 20; 	/* offset from corner of window*/
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

	/* output text, centered on each line */
	XDrawString(display,win,gc,(win_width - width1)/2,y,string1,len1);
	XDrawString(display,win,gc,(win_width - width2)/2, (int)(win_height - 35),string2,len2);
	XDrawString(display,win,gc,(win_width - width3)/2, (int)(win_height - 15),string3,len3);

	/* copy numbers into string variables */
	(void) sprintf(cd_height, " Height - %d pixels", DisplayHeight(display,screen));
	(void) sprintf(cd_width, " Width  - %d pixels", DisplayWidth(display,screen));
	(void) sprintf(cd_depth, " Depth  - %d plane(s)", DefaultDepth(display, screen));

	/* reuse these for same purpose */
	len4 = strlen(string4);
	len1 = strlen(cd_height);
	len2 = strlen(cd_width);
	len3 = strlen(cd_depth);

	font_height = font_info->max_bounds.ascent + font_info->max_bounds.descent;

	/* To center strings vertically, we place the first string
	 * so that the top of it is two font_heights above the center
	 * of the window.  Since the baseline of the string is what we
	 * need to locate for XDrawString, and the baseline is one
	 * font_info->max_bounds.ascent below the top of the chacter,
	 * the final offset of the origin up from the center of the 
	 * window is one font_height + one descent. */

	initial_y_offset = win_height/2 - font_height - font_info->max_bounds.descent;
	x_offset = (int) win_width/4;
	XDrawString(display, win, gc, x_offset, (int) initial_y_offset, string4,len4);

	XDrawString(display, win, gc, x_offset, (int) initial_y_offset + font_height,cd_height,len1);
	XDrawString(display, win, gc, x_offset, (int) initial_y_offset + 2 * font_height,cd_width,len2);
	XDrawString(display, win, gc, x_offset, (int) initial_y_offset + 3 * font_height,cd_depth,len3);
}


place_graphics(win, gc, window_width, window_height)
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

	y_offset = font_info->max_bounds.ascent + 2;
	x_offset = 2;

	/* output text, centered on each line */
	XDrawString(display, win, gc, x_offset, y_offset, string1, strlen(string1));
}

