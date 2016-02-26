/*
 * scalefonts.c
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
 * This program demonstrates the use of scalable fonts in X11R5.
 * Invoke it with a fully specified scalable font name as its
 * only argument.  It will load that font scaled to 7 point,
 * 11 point, 15 point, and 19 point precisely at the resolution
 * of your screen.  It opens a very simple window and draws
 * the font name at each of those point sizes.
 *
 */

#include <stdio.h>
#include <X11/Xlib.h>

/*
 * This routine returns True only if the passed name is a well-formed
 * XLFD style font name with a pixel size, point size, and average
 * width (fields 7,8, and 12) of "0".
 */ 
Bool IsScalableFont(name) 
char *name; 
{
    int i, field;
    
    if ((name == NULL) || (name[0] != '-')) return False;
    
    for(i = field = 0; name[i] != '\0' && field <= 14; i++) {
	if (name[i] == '-') {
	    field++;
	    if ((field == 7) || (field == 8) || (field == 12))
		if ((name[i+1] != '0') || (name[i+2] != '-'))
		    return False;
	}
    }
    
    if (field != 14) return False;
    else return True;
}


/*
 * This routine is passed a scalable font name and a point size.
 * It returns an XFontStruct for the given font scaled to the 
 * specified size and the exact resolution of the screen.
 * The font name is assumed to be a well-formed XLFD name,
 * and to have pixel size, point size, and average width fields
 * of "0" and implementation dependent x-resolution and y- 
 * resolution fields.  Size is specified in tenths of points.
 * Returns NULL if the name is malformed or no such font exists.
 */
XFontStruct *LoadQueryScalableFont(dpy, screen, name, size)
Display *dpy;
int screen;
char *name;
int size;
{
    int i,j, field;
    char newname[500];        /* big enough for a long font name */
    int res_x, res_y;         /* resolution values for this screen */
    
    /* catch obvious errors */
    if ((name == NULL) || (name[0] != '-')) return NULL;
    
    /* calculate our screen resolution in dots per inch. 25.4mm = 1 inch */
    res_x = DisplayWidth(dpy, screen)/(DisplayWidthMM(dpy, screen)/25.4);
    res_y = DisplayHeight(dpy, screen)/(DisplayHeightMM(dpy, screen)/25.4);
    
    /* copy the font name, changing the scalable fields as we do so */
    for(i = j = field = 0; name[i] != '\0' && field <= 14; i++) {
	newname[j++] = name[i];
	if (name[i] == '-') {
	    field++;
	    switch(field) {
	    case 7:  /* pixel size */
	    case 12: /* average width */
		/* change from "-0-" to "-*-" */
		newname[j] = '*'; 
		j++;
		if (name[i+1] != '\0') i++;
		break;
	    case 8:  /* point size */
		/* change from "-0-" to "-<size>-" */
		(void)sprintf(&newname[j], "%d", size);
		while (newname[j] != '\0') j++;
		if (name[i+1] != '\0') i++;
		break;
	    case 9:  /* x resolution */
	    case 10: /* y resolution */
		/* change from an unspecified resolution to res_x or res_y */
		(void)sprintf(&newname[j], "%d", (field == 9) ? res_x : res_y);
		while(newname[j] != '\0') j++;
		while((name[i+1] != '-') && (name[i+1] != '\0')) i++;
		break;
	    }
	}
    }
    newname[j] = '\0';
    
    /* if there aren't 14 hyphens, it isn't a well formed name */
    if (field != 14) return NULL;
    
    return XLoadQueryFont(dpy, newname);
}

int sizes[] = {70,110,150,190};

main(argc, argv)
int argc;
char *argv[];
{
    Display *d;
    int screen;
    Window w;
    GC gc;
    XGCValues gcv;
    XFontStruct *f[4];
    XEvent event;
    int i;
    
    if (argc != 2) {
	(void) fprintf(stderr, "Usage: %s fontname\n", argv[0]);
	exit(1);
    }

    if (!IsScalableFont(argv[1])) {
	(void) fprintf(stderr,
		       "%s: not a fully specified scalable font name.\n",
		       argv[0]);
	exit(1);
    }
    
    if ((d = XOpenDisplay(NULL)) == NULL) {
	(void) fprintf(stderr, "%s: can't open display.\n", argv[0]);
	exit(1);
    }
    
    screen = DefaultScreen(d);
    w = XCreateSimpleWindow(d, RootWindow(d, screen), 0, 0, 600, 250,
			    3, WhitePixel(d,screen),BlackPixel(d,screen));
    
    for(i=0; i < 4; i++)
	f[i] = LoadQueryScalableFont(d, screen, argv[1], sizes[i]);
    
    gc = XCreateGC(d,w,0,&gcv);
    XSetForeground(d,gc,WhitePixel(d,screen));

    XSelectInput(d, w, ExposureMask);
    XMapWindow(d,w);

    while(1) {
	XNextEvent(d, &event);
	switch(event.type) {
	case Expose:
	    if (event.xexpose.count == 0) {
		for(i=0; i<4; i++) {
		    XSetFont(d,gc,f[i]->fid);
		    XDrawString(d,w,gc,10,10+30*i,argv[1], strlen(argv[1]));
		}
	    }
	    break;
	}
    }
}
