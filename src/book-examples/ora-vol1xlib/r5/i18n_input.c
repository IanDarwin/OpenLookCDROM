/*
 * i18n_input.c
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
 * This program demonstrates some of the X11R5 internationalized text
 * input functions.  It creates a very simple window, connects to an
 * input method, and displays composed text obtained by calling
 * XwcLookupString.  It backspaces when it receives the Backspace or
 * Delete keysyms.
 *
 * To run this program successfully, you must have an input method running.
 * Because there are no input methods as part of the core X11R5
 * distribution, this may be difficult.  If your Xlib uses the Xsi
 * implementation of the X11R5 internationalization features, you can
 * use the input method in contrib/im/Xsi.  In order to run this program,
 * I had to do the following:
 * 
 *     1) build everything in contrib/im/Xsi.
 *     2) install everything in contrib/im/Xsi.  This involved
 *        installing a number of files under /usr/local/lib/wnn, and
 *        adding a new user "wnn" to the /etc/passwd file.
 *     3) start the "translation server" contrib/im/Xsi/Wnn/jserver/jserver
 *     4) start the "input manager" contrib/im/Xsi/Xwnmo/xwnmo/xwnmo
 *        which was also installed in /usr/bin/X11.
 *     5) set the XMODIFIERS environment variable to "@im=_XWNMO".
 *     6) set the LANG environment variable to something appropriate,
 *        ja_JP.ujis, for example.
 *
 * With these steps accomplished, I was able to run the program and type
 * Latin characters, but I was never able to figure out how to actually
 * make use of the input method to input Japanese.   Since the Xsi input
 * method is contributed software, it may have been updated since this
 * program was written, and the above list may no longer be correct.
 *
 * This program has not been tested with the Ximp implementation.
 *
 * Finally, note that this program contains a work-around for a bug
 * in the Xsi implementation of XwcLookupString.  If you are using
 * the Ximp implementation, or if the bug has been fixed in your Xlib,
 * you will need to undo the workaround.  See the comment below, near
 * the call to XwcLookupString.
 */

#include <stdio.h>
#include <malloc.h>    
#include <X11/Xlib.h>
#include <X11/keysym.h>
/*
 * include <locale.h> or the non-standard X substitutes
 * depending on the X_LOCALE compilation flag
 */        
#include <X11/Xlocale.h>  

/*
 * This function chooses the "more desirable" of two input styles.  The
 * style with the more complicated PreEdit style is returned, and if the
 * styles have the same PreEdit styles, then the style with the more
 * complicated Status style is returned.  There is no "official" way to
 * order interaction styles.  This one makes the most sense to me.  
 * This is a long procedure for a simple heuristic.
 */
XIMStyle ChooseBetterStyle(style1,style2)
XIMStyle style1, style2;
{
    XIMStyle s,t;
    XIMStyle preedit = XIMPreeditArea | XIMPreeditCallbacks |
	XIMPreeditPosition | XIMPreeditNothing | XIMPreeditNone;
    XIMStyle status = XIMStatusArea | XIMStatusCallbacks |
	XIMStatusNothing | XIMStatusNone;

    if (style1 == 0) return style2;
    if (style2 == 0) return style1;
    if ((style1 & (preedit | status)) == (style2 & (preedit | status)))
	return style1;
    
    s = style1 & preedit;
    t = style2 & preedit;
    if (s != t) {
	if (s | t | XIMPreeditCallbacks)
	    return (s == XIMPreeditCallbacks)?style1:style2;
	else if (s | t | XIMPreeditPosition)
	    return (s == XIMPreeditPosition)?style1:style2;
	else if (s | t | XIMPreeditArea)
	    return (s == XIMPreeditArea)?style1:style2;
	else if (s | t | XIMPreeditNothing)
	    return (s == XIMPreeditNothing)?style1:style2;
    }
    else { /* if preedit flags are the same, compare status flags */
	s = style1 & status;
	t = style2 & status;
	if (s | t | XIMStatusCallbacks)
	    return (s == XIMStatusCallbacks)?style1:style2;
	else if (s | t | XIMStatusArea)
	    return (s == XIMStatusArea)?style1:style2;
	else if (s | t | XIMStatusNothing)
	    return (s == XIMStatusNothing)?style1:style2;
    }
}

void GetPreferredGeometry(ic, name, area)
XIC ic;
char *name;           /* XNPreEditAttributes or XNStatusAttributes */
XRectangle *area;     /* the constraints on the area */
{
    XVaNestedList list;

    list = XVaCreateNestedList(0, XNAreaNeeded, area, NULL);

    /* set the constraints */
    XSetICValues(ic, name, list, NULL);

    /* Now query the preferred size */
    /* The Xsi input method, Xwnmo, seems to ignore the constraints, */
    /* but we're not going to try to enforce them here. */
    XGetICValues(ic, name, list, NULL);
    XFree(list);
}

void SetGeometry(ic, name, area)
XIC ic;
char *name;           /* XNPreEditAttributes or XNStatusAttributes */
XRectangle *area;     /* the actual area to set */
{
    XVaNestedList list;

    list = XVaCreateNestedList(0, XNArea, area, NULL);
    XSetICValues(ic, name, list, NULL);
    XFree(list);
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
    XIM im;
    XIC ic;
    XIMStyles *im_supported_styles;
    XIMStyle app_supported_styles;
    XIMStyle style;
    XIMStyle best_style;
    XVaNestedList list;
    long im_event_mask;
    XRectangle preedit_area;
    XRectangle status_area;
    char *program_name = argv[0];
    char **missing_charsets;
    int num_missing_charsets = 0;
    char *default_string;
    wchar_t string[200];
    int str_len = 0;
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
			     2, WhitePixel(dpy,screen),BlackPixel(dpy,screen));

    gc = XCreateGC(dpy,win,0,&gcv);
    XSetForeground(dpy,gc,WhitePixel(dpy,screen));
    XSetBackground(dpy,gc,BlackPixel(dpy,screen));

    /* Connect to an input method.  */
    /* In this example, we don't pass a resource database */
    if ((im = XOpenIM(dpy, NULL, NULL, NULL)) == NULL) {
	(void)fprintf(stderr, "Couldn't open input method\n");
	exit(1);
    }

    /* set flags for the styles our application can support */
    app_supported_styles = XIMPreeditNone | XIMPreeditNothing | XIMPreeditArea;
    app_supported_styles |= XIMStatusNone | XIMStatusNothing | XIMStatusArea;

    /* figure out which styles the IM can support */
    XGetIMValues(im, XNQueryInputStyle, &im_supported_styles, NULL);

    /*
     * now look at each of the IM supported styles, and
     * chose the "best" one that we can support.
     */
    best_style = 0;
    for(i=0; i < im_supported_styles->count_styles; i++) {
	style = im_supported_styles->supported_styles[i];
	if ((style & app_supported_styles) == style) /* if we can handle it */
	    best_style = ChooseBetterStyle(style, best_style);
    }


    /* if we couldn't support any of them, print an error and exit */
    if (best_style == 0) {
	(void)fprintf(stderr, "%s: application and program do not share a\n",
		      argv[0]);
	(void)fprintf(stderr, "%s: commonly supported interaction style.\n",
		      argv[0]);
	exit(1);
    }
    
    /*
     * Now go create an IC using the style we chose.
     * Also set the window and fontset attributes now.
     */
    list = XVaCreateNestedList(0,XNFontSet,fontset,NULL);
    ic = XCreateIC(im,
		   XNInputStyle, best_style,
		   XNClientWindow, win,
		   XNPreeditAttributes, list,
		   XNStatusAttributes, list,
		   NULL);
    XFree(list);
    if (ic == NULL) {
	(void) fprintf(stderr, "Couldn't create input context\n");
	exit(1);
    }

    XGetICValues(ic, XNFilterEvents, &im_event_mask, NULL);
    XSelectInput(dpy,win, ExposureMask | KeyPressMask
		 | StructureNotifyMask | im_event_mask);

    XSetICFocus(ic);

    XMapWindow(dpy,win);

    while(1) {
	int buf_len = 10;
	wchar_t *buffer = (wchar_t *)malloc(buf_len * sizeof(wchar_t));
	int len;
	KeySym keysym;
	Status status;
	Bool redraw = False;

	XNextEvent(dpy, &event);
	if (XFilterEvent(&event, None))
	    continue;

        switch (event.type) {
        case Expose:
	    /* draw the string at a hard-coded location */
	    if (event.xexpose.count == 0)
		XwcDrawString(dpy, win, fontset, gc, 10, 50, string, str_len);
            break;
        case KeyPress:
	    len = XwcLookupString(ic, &event, buffer, buf_len,
				  &keysym, &status);
	    /*
	     * Workaround:  the Xsi implementation of XwcLookupString
	     * returns a length that is 4 times too big.  If this bug
	     * does not exist in your version of Xlib, remove the
	     * following line, and the similar line below.
	     */
	    len = len / 4;

	    if (status == XBufferOverflow) {
		buf_len = len;
		buffer = (wchar_t *)realloc((char *)buffer,
					    buf_len * sizeof(wchar_t));
		len = XwcLookupString(ic, &event, buffer, buf_len,
				      &keysym, &status);
		/* Workaround */
		len = len / 4;
	    }
	    
	    redraw = False;
	    
	    switch (status) {
	    case XLookupNone:
		break;
	    case XLookupKeySym:
	    case XLookupBoth:
		/* Handle backspacing, and <Return> to exit */
		if ((keysym == XK_Delete) || (keysym == XK_BackSpace)) {
		    if (str_len > 0) str_len--;
		    redraw = True;
		    break;
		}
		if (keysym == XK_Return) exit(0);
		if (status == XLookupKeySym) break;  
	    case XLookupChars:
		for(i=0; i < len; i++) 
		    string[str_len++] = buffer[i];
		redraw = True;
		break;
	    }
	    
	    /* do a very simple-minded redraw, if needed */
	    if (redraw) {
		XClearWindow(dpy, win);
		XwcDrawString(dpy, win, fontset, gc, 10, 50, string, str_len);
	    }
	    break;
	case ConfigureNotify:
	    /*
	     * When the window is resized, we should re-negotiate the
	     * geometry of the Preedit and Status area, if they are used
	     * in the interaction style.
	     */
	    if (best_style & XIMPreeditArea) {
		preedit_area.width = event.xconfigure.width*4/5;
		preedit_area.height = 0;
		GetPreferredGeometry(ic, XNPreeditAttributes, &preedit_area);
		preedit_area.x = event.xconfigure.width - preedit_area.width;
		preedit_area.y = event.xconfigure.height - preedit_area.height;
		SetGeometry(ic, XNPreeditAttributes, &preedit_area);
	    }
	    if (best_style & XIMStatusArea) {
		status_area.width = event.xconfigure.width/5;
		status_area.height = 0;
		GetPreferredGeometry(ic, XNStatusAttributes, &status_area);
		status_area.x = 0;
		status_area.y = event.xconfigure.height - status_area.height;
		SetGeometry(ic, XNStatusAttributes, &status_area);
	    }
	    break;
        }
    }
}


