/*
 * i18n_output.c
 *
 * Written by David Flanagan.  Copyright 1991, O'Reilly && Associates.
 *
 *    The X Consortium, and any party obtaining a copy of these files from
 *    the X Consortium, directly or indirectly, is granted, free of charge, a
 *    full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *    nonexclusive right and license to deal in this software and
 *    documentation files (the "Software"), including without limitation the
 *    rights to use, copy, modify, merge, publish, distribute, sublicense,
 *    and/or sell copies of the Software, and to permit persons who receive
 *    copies from any such party to do so.  This license includes without
 *    limitation a license to do the foregoing actions under any patents of
 *    the party supplying this software to the X Consortium.
 *
 * This program demonstrates the use of some of the X11R5 internationalized
 * text output functions.  To use it, set the LANG environment variable
 * (if you are on a Unix system) to a desired locale, and invoke the
 * program with a filename as the single command line argument.  The file
 * should contain text in the encoding of the locale you set.  This
 * program creates a very simple window and displays a single line of
 * the file centered in the window.  Typing any key while the mouse
 * is in the window will cause the next line of the file to be
 * displayed, and clicking the mouse in the window will cause the program
 * to exit.
 *
 * A good source of example text to use with this program is from the
 * input methods contributed to X11R5.  Try the file:
 *     contrib/im/Xsi/Wnn/manual/0/contents
 * with locale ja_JP.ujis.  (If your version of Xlib uses the
 * Ximp implementation of the X internationalization features, the
 * locale name will probably be different.)
 *
 * This program has not been tested with the Ximp implementation.
 */


#include <stdio.h>
#include <X11/Xlib.h>
/*
 * include <locale.h> or the non-standard X substitutes,
 * depending on the X_LOCALE compilation flag
 */        
#include <X11/Xlocale.h>  


/*
 * This function draws a specified multi-byte string centered in
 * a specified region of a window.  
 */
void DrawCenteredMbString(dpy, w, fontset, gc, str, num_bytes,x,y,width,height)
Display *dpy;
Window w;
XFontSet fontset;
GC gc;
char *str;
int num_bytes;
int x, y, width, height;
{
    int escapement;
    XRectangle boundingbox;
    XRectangle dummy;
    int originx, originy;

   /* 
    * figure out how big the string will be.
    * We should be able to pass NULL instead of &dummy, but
    * XmbTextExtents is buggy in the Xsi implementation.
    */
    (void) XmbTextExtents(fontset, str, num_bytes,
			  &dummy, &boundingbox);

    /* XmbTextExtents should return the escapement, but it is buggy. */
    /* Note that escapement is the same as boundingbox.width */
    escapement = XmbTextEscapement(fontset, str, num_bytes);
       
   /*
    * The string we want to center may be drawn left-to-right,
    * right-to-left, or some of both, so computing the 
    * drawing origin is a little tricky.  The bounding box's x
    * and y coordinates are the upper left hand corner and are
    * relative to the drawing origin.
    * if boundingbox.x is 0, the the string is pure left-to-right.
    * If it is equal to -boundingbox.width then the string is pure
    * right-to-left, but it may not be either of these, so what
    * we've got to do is choose the origin so that the bounding box
    * is centered in the window without assuming that the orgin is
    * at one end or another of the string:
    *     originx + boundingbox.x = x + (width - escapement)/2
    * and similarly for the baseline of the text:
    *     originy + boundingbox.y = y + (height - boundingbox.height)/2
    */
    originx = x + (width - escapement)/2 - boundingbox.x;
    originy = y + (height - boundingbox.height)/2 - boundingbox.y;

   /*
    * now draw the string
    */
    XmbDrawImageString(dpy, w, fontset, gc, 
                       originx, originy,
                       str, num_bytes);
}



main(argc, argv)
int argc;
char *argv[];
{
    Display *dpy;
    int screen;
    Window win;
    GC gc;
    XGCValues gcv;
    XEvent event;
    XFontSet fontset;
    char *program_name = argv[0];
    char **missing_charsets;
    int num_missing_charsets = 0;
    char *default_string;
    int width = 0;
    int height = 0;
    FILE *f;
    char linebuf[500];
    int i;
    
    /*
     * The error messages in this program are all in English.
     * In a truely internationalized program, they would not
     * be hardcoded; they would be looked up in a database of
     * some sort.
     */

    if (setlocale(LC_ALL, "") == NULL) {
        (void) fprintf(stderr, "%s: cannot set locale.\n",program_name);
        exit(1);
    }

    if (argc != 2) {
	(void) fprintf(stderr, "Usage: %s filename\n", argv[0]);
	exit(1);
    }

    if ((f = fopen(argv[1], "r")) == NULL) {
	perror(argv[0]);
	exit(1);
    }
    
    if ((dpy = XOpenDisplay(NULL)) == NULL) {
	(void) fprintf(stderr, "%s: cannot open Display.\n", program_name);
	exit(1);
    }

    if (!XSupportsLocale()) {
        (void) fprintf(stderr, "%s: X does not support locale \"%s\".\n",
                       program_name, setlocale(LC_ALL, NULL));
        exit(1);
    }

    if (XSetLocaleModifiers("") == NULL) {
        (void) fprintf(stderr, "%s: Warning: cannot set locale modifiers.\n",
                                argv[0]);
    }

    /*
     * Create the fontset.
     * Choose the fontlist carefully so as not to require font scaling.
     */
    fontset = XCreateFontSet(dpy,
			     "-adobe-helvetica-*-r-*-*-*-120-*-*-*-*-*-*,\
                              -misc-fixed-*-r-*-*-*-130-*-*-*-*-*-*",
			     &missing_charsets, &num_missing_charsets,
			     &default_string);
    
    /*
     * if there are charsets for which no fonts can
     * be found, print a warning message.  
     */
    if (num_missing_charsets > 0) {
	(void)fprintf(stderr, "%s: The following charsets are missing:\n",
		      program_name);
	for(i=0; i < num_missing_charsets; i++)
	    (void)fprintf(stderr, "%s: \t%s\n", program_name,
			  missing_charsets[i]);
	XFreeStringList(missing_charsets);
	
	(void)fprintf(stderr, "%s: The string \"%s\" will be used in place\n",
		      program_name, default_string);
	(void)fprintf(stderr, "%s: of any characters from those sets.\n",
		      program_name);
    }

    screen = DefaultScreen(dpy);
    win = XCreateSimpleWindow(dpy, RootWindow(dpy, screen), 0, 0, 400, 100,
			      2,WhitePixel(dpy,screen),BlackPixel(dpy,screen));
    XSelectInput(dpy, win, ExposureMask | KeyPressMask |
		 ButtonPressMask | StructureNotifyMask);
    XMapWindow(dpy,win);

    gc = XCreateGC(dpy,win,0,&gcv);
    XSetForeground(dpy,gc,WhitePixel(dpy,screen));
    XSetBackground(dpy,gc,BlackPixel(dpy,screen));

    if (fgets(linebuf, 500, f) == 0) exit(0);

    while(1) {
        XNextEvent(dpy, &event);
        switch (event.type) {
        case Expose:
            if (event.xexpose.count == 0)
		DrawCenteredMbString(dpy, win, fontset, gc,
				     linebuf, strlen(linebuf),
				     0, 0, width, height);
            break;
	case ConfigureNotify:
	    width = event.xconfigure.width;
	    height = event.xconfigure.height;
	    break;
        case KeyPress:
	    if (fgets(linebuf, 500, f) == 0) exit(0);
	    XClearWindow(dpy, win);
	    DrawCenteredMbString(dpy, win, fontset, gc,
				 linebuf, strlen(linebuf),
				 0, 0, width, height);
	    break;
        case ButtonPress:
            exit(0);
        }
    }
}


