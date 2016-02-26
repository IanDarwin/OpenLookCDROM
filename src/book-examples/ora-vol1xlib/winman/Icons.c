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
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#include <stdio.h>

extern Display *display;
extern int screen_num;
extern XFontStruct *font_info;

typedef struct _windowList {
	struct _windowList *next;
	Window window;
	Window icon;
	Bool own;
	char *icon_name;
} WindowListRec, *WindowList;
	
WindowList Icons = NULL;

Bool isIcon(win, x, y, assoc, icon_name, makeicon)
Window win;
int x, y;
Window *assoc;
char *icon_name;
Bool makeicon;
{
	WindowList win_list;
	Window makeIcon();

	/* go through linked list of window-icon structures */	
	for (win_list = Icons; win_list; win_list = win_list->next) {
		if (win == win_list->icon) { /* win is icon */
			*assoc = win_list->window; 
			strcpy(icon_name, win_list->icon_name);
			return(True);
		}
		if (win == win_list->window) { /* win is main window */
			*assoc = win_list->icon; 
			strcpy(icon_name, win_list->icon_name);
			return(False);
		}
	}
	/* window not in list means icon not created yet.
	 * Create icon and add main window to save set
	 * in case window manager dies */
        if (makeicon) {
		*assoc = makeIcon(win, x, y, icon_name);
		XAddToSaveSet(display, win);
	}
	return(False);
}

/* NOT NECESSARY DUE TO SAVE SET
this is called when the window manager exits gracefully 
 * to turn all icons back into windows
clearIcons()
{
	WindowList win_list;


	go through linked list of window-icon structures 
	for (win_list = Icons; win_list; win_list = win_list->next) {
		XUnmapWindow(display, win_list->icon);
		XMapWindow(display, win_list->window);
	}
}
*/

removeIcon(window)
Window window;
{
	WindowList win_list, win_list1;

	for (win_list = Icons; win_list; win_list = win_list->next) 
		if (win_list->window == window) {
			if (win_list->own) 
				XDestroyWindow(display, win_list->icon);
			break;
		}
	if (win_list) {
		if (win_list==Icons) Icons = Icons->next;
		else 
			for (win_list1 = Icons; win_list1->next; 
					win_list1 = win_list1->next) 
				if (win_list1->next == win_list) {
					win_list1->next = win_list->next;
					break;
				};
	}
}

char *
getIconName(window)
Window window;
{
    char *name;

    if (XGetIconName( display, window, &name )) return( name );

    if (XFetchName( display, window, &name )) return( name );

    return( "Icon" );
}

char *
getDefaultIconSize(window, icon_w, icon_h)
Window window;
int *icon_w, *icon_h;
{
	/* Determine the size of the icon window.  */ 
	char *icon_name;

	icon_name = getIconName(window);

	*icon_h = font_info->ascent + font_info->descent + 4;
	*icon_w = XTextWidth(font_info, icon_name, strlen(icon_name));

	return(icon_name);
}

Window makeIcon(window, x, y, icon_name_return)
Window window;	/* associated window. */
int x, y;	/* current mouse position. */
char *icon_name_return;
{
	int icon_x, icon_y;	/* Icon U. L. X and Y coordinates. */
	int icon_w, icon_h;	/* Icon width and height. */
	int icon_bdr;	/* Icon border width. */
	int depth;	/* for XGetGeometry */
	Window root;	/* for XGetGeometry */
	XSetWindowAttributes icon_attrib;	/* for icon creation */
	unsigned long icon_attrib_mask;
	XWMHints *wmhints;	/* see if icon position provided */
	XWMHints *XGetWMHints();
	Window finishIcon();
	char *icon_name;
 
	/*
	* Process window manager hints.
	* If icon window hint exists, use it directly
	* If icon pixmap hint exists, get its size
	* otherwise, get default size.
	* If icon position hint exists, use it
	* otherwise, use the position passed (current mouse position)
	*/ 
	if (wmhints = XGetWMHints(display, window)) {
		if (wmhints->flags&IconWindowHint)
			/* icon window was passed; use it as is */
			return(finishIcon(window, wmhints->icon_window, 
				False, icon_name));
		else if (wmhints->flags&IconPixmapHint) 
		{ 
			/* Pixmap was passed.
			 * Determine size of icon 
			 * window from pixmap. Only 
			 * icon_w and icon_h are significant. */ 
    			if (!XGetGeometry(display, wmhints->icon_pixmap, 
				&root, &icon_x, &icon_y,
				&icon_w, &icon_h, &icon_bdr, &depth)) {
        			fprintf(stderr, "winman: client passed invalid \
						icon pixmap." );
        			return( NULL );
    			}
			else {
				icon_attrib.background_pixmap = wmhints->icon_pixmap;
				icon_attrib_mask = CWBorderPixel|CWBackPixmap;
			}
		}
		/* else no window or pixmap passed */
		else {
			icon_name = getDefaultIconSize(window, &icon_w, &icon_h);
			icon_attrib_mask = CWBorderPixel | CWBackPixel;
			icon_attrib.background_pixel = (unsigned long) 
					WhitePixel(display,screen_num);
		}
	}
	/* else no hints at all exist */
	else {
		icon_name = getDefaultIconSize(window, &icon_w, &icon_h);
		icon_attrib_mask = CWBorderPixel | CWBackPixel;
	}
	/* Pad sizes. */
	icon_w += 2;
	icon_h += 2;

	strcpy(icon_name_return, icon_name);
	
	/* Set the icon border attributes.  */ 
	icon_bdr = 2;
	icon_attrib.border_pixel = (unsigned long) 
			BlackPixel(display,screen_num);
 
	/* If icon position hint exists, get it.
	 * This also checks to see if wmhints is NULL,
	 * which it will be if WMHints were never set at all */
	if (wmhints && (wmhints->flags&IconPositionHint)) 
	{
		 icon_x = wmhints->icon_x;
		 icon_y = wmhints->icon_y;
	} 
	else 
	{
		/* put it where the mouse was */
		 icon_x = x;
		 icon_y = y;
	}

	/* Create the icon window.  */
	return(finishIcon(window, XCreateWindow(display, 
			RootWindow(display, screen_num),
			icon_x, icon_y, icon_w, icon_h,
			icon_bdr, 0, CopyFromParent, CopyFromParent,
			icon_attrib_mask, &icon_attrib),
			True, icon_name));
}

Window finishIcon(window, icon, own, icon_name)
Window window, icon;
Bool own;	/* whether winman created the icon window */
char *icon_name;
{
	WindowList win_list;
	Cursor ManCursor;


	/* if icon window didn't get created, return failure */
	 if (icon == NULL) return(NULL);

	/*
	 * Use the man cursor whenever the mouse is in the icon window.
	 */
	ManCursor = XCreateFontCursor(display, XC_man);
	XDefineCursor(display, icon, ManCursor);
		
	 /* Select events for the icon window */
	XSelectInput(display, icon, ExposureMask);
		
	/*
	 * Set the event window's icon window to be the new icon window.
	 */
	win_list = (WindowList) malloc(sizeof(WindowListRec));
	win_list->window = window;
	win_list->icon = icon;
	win_list->own = own;
	win_list->icon_name = icon_name;
	win_list->next = Icons;
	Icons = win_list;

	return(icon);
}

