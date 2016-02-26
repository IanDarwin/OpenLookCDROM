/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 *
 *   The X Consortium, and any party obtaining a copy of these files from
 *   the X Consortium, directly or indirectly, is granted, free of charge, a
 *   full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *   nonexclusive right and license to deal in this software and
 *   documentation files (the "Software"), including without limitation the
 *   rights to use, copy, modify, merge, publish, distribute, sublicense,
 *   and/or sell copies of the Software, and to permit persons who receive
 *   copies from any such party to do so.  This license includes without
 *   limitation a license to do the foregoing actions under any patents of
 *   the party supplying this software to the X Consortium.
 */

/* app_scroll.c - Displays bitmaps specified on the command line.  All
 * bitmaps are drawn into a pixmap, which is rendered into a DrawingArea
 * widget, which is used as the work window for a ScrolledWindow.  This
 * method is only used to demonstrate application-defined scrolling for
 * the motif ScrolledWindow.  Automatic scrolling is much simpler, but
 * does not allow the programmer to impose incremental scrolling units.
 *
 * The bitmaps are displayed in an equal number of rows and columns if
 * possible.
 *
 * Example:
 *  app_scroll /usr/include/X11/bitmaps/*
 */

#include <stdio.h>
#include <strings.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>

#ifdef max  /* just in case--we don't know, but these are commonly set */
#undef max  /* by arbitrary unix systems.  Also, we cast to int! */
#endif
/* redefine "max" and "min" macros to take into account "unsigned" values */
#define max(a,b) ((int)(a)>(int)(b)?(int)(a):(int)(b))
#define min(a,b) ((int)(a)<(int)(b)?(int)(a):(int)(b))

/* don't accept bitmaps larger than 100x100 .. This value is arbitrarily
 * chosen, but is sufficiently large for most images.  Handling extremely
 * large bitmaps would eat too much memory and make the interface awkward.
 */
#define MAX_WIDTH  100
#define MAX_HEIGHT 100

typedef struct {
    char *name;
    int len; /* strlen(name) */
    unsigned int width, height;
    Pixmap bitmap;
} Bitmap;

/* get the integer square root of n -- used to calculate an equal
 * number of rows and colums for a given number of elements.
 */
int_sqrt(n)
register int n;
{
    register int i, s = 0, t;
    for (i = 15; i >= 0; i--) {
        t = (s | (1L << i));
        if (t * t <= n)
            s = t;
    }
    return s;
}

/* Global variables */
Widget drawing_a, vsb, hsb;
Pixmap pixmap; /* used as the image for DrawingArea widget */
Display *dpy;
Dimension view_width = 300, view_height = 300;
int rows, cols;
unsigned int cell_width, cell_height;
unsigned int pix_hoffset, pix_voffset, sw_hoffset, sw_voffset;
void redraw();

main(argc, argv)
int argc;
char *argv[];
{
    extern char *strcpy();
    XtAppContext app;
    Widget toplevel, scrolled_w;
    Bitmap *list = (Bitmap *) NULL;
    GC gc;
    char *p;
    XFontStruct *font;
    int i = 0, total = 0;
    unsigned int bitmap_error;
    int j, k;
    void scrolled(), expose_resize();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtAppInitialize (&app, argv[0], NULL, 0,
        &argc, argv, NULL, NULL, 0);
    dpy = XtDisplay (toplevel);

    font = XLoadQueryFont (dpy, "fixed");

    /* load bitmaps from filenames specified on command line */
    while (*++argv) {
        printf ("Loading \"%s\"...", *argv), fflush (stdout);
        if (i == total) {
            total += 10; /* allocate bitmap structures in groups of 10 */
            if (!(list = (Bitmap *) XtRealloc (list, total * sizeof (Bitmap))))
                XtError ("Not enough memory for bitmap data");
        }
        /* read bitmap file using standard X routine.  Save the resulting
         * image if the file isn't too big.
         */
        if ((bitmap_error = XReadBitmapFile (dpy, DefaultRootWindow (dpy),
                *argv, &list[i].width, &list[i].height, &list[i].bitmap,
                &j, &k)) == BitmapSuccess) {
            /* Get just the base filename (minus leading pathname)
             * We save this value for later use when we caption the bitmap.
             */
            if (p = rindex (*argv, '/'))
                p++;
            else
                p = *argv;
            if (list[i].width > MAX_WIDTH || list[i].height > MAX_HEIGHT) {
                printf ("%s: bitmap too big\n", p);
                XFreePixmap (dpy, list[i].bitmap);
                continue;
            }
            list[i].len = strlen (p);
            list[i].name = p;  /* we'll be getting it later */
            printf ("Size: %dx%d\n", list[i].width, list[i].height);
            i++;
        } else {
            printf ("Couldn't load bitmap: \"%s\": ", *argv);
            switch (bitmap_error) {
                case BitmapOpenFailed : puts ("Open failed."); break;
                case BitmapFileInvalid : puts ("Bad file format."); break;
                case BitmapNoMemory : puts ("Not enough memory."); break;
            }
        }
    }
    if ((total = i) == 0) {
        puts ("Couldn't load any bitmaps.");
        exit (1);
    }
    printf ("Total bitmaps loaded: %d\n", total);
    /* calculate size for pixmap by getting the dimensions of each. */
    printf ("Calculating sizes for pixmap..."), fflush (stdout);
    for (i = 0; i < total; i++) {
        if (list[i].width > cell_width)
            cell_width = list[i].width;
        if (list[i].height > cell_height)
            cell_height = list[i].height;
        /* the bitmap's size is one thing, but its caption may exceed it */
        if ((j = XTextWidth (font, list[i].name, list[i].len)) > cell_width)
            cell_width = j;
    }
    /* compensate for font in the vertical dimension; add a 6 pixel padding */
    cell_height += 6 + font->ascent + font->descent;
    cell_width += 6;
    cols = int_sqrt (total);
    rows = (total + cols-1)/cols;

    printf ("Creating pixmap area of size %dx%d (%d rows, %d cols)\n",
        cols * cell_width, rows * cell_height, rows, cols);

    /* Create a single, 1-bit deep pixmap */
    if (!(pixmap = XCreatePixmap (dpy, DefaultRootWindow (dpy),
            cols * cell_width + 1, rows * cell_height + 1, 1)))
        XtError ("Can't Create pixmap");

    if (!(gc = XCreateGC (dpy, pixmap, NULL, 0)))
        XtError ("Can't create gc");
    XSetForeground(dpy, gc, 0); /* 1-bit deep pixmaps use 0 as background */
    /* Clear the pixmap by setting the entire image to 0's */
    XFillRectangle (dpy, pixmap, gc, 0, 0,
        cols * cell_width, rows * cell_height);
    XSetForeground (dpy, gc, 1); /* Set the foreground to 1 (1-bit deep) */
    XSetFont (dpy, gc, font->fid); /* to print bitmap filenames (captions) */

    /* Draw the grid lines between bitmaps */
    for (j = 0; j <= rows * cell_height; j += cell_height)
        XDrawLine (dpy, pixmap, gc, 0, j, cols * cell_width, j);
    for (j = 0; j <= cols * cell_width; j += cell_width)
        XDrawLine (dpy, pixmap, gc, j, 0, j, rows*cell_height);

    /* Draw each of the bitmaps into the big picture */
    for (i = 0; i < total; i++) {
        int x = cell_width * (i % cols);
        int y = cell_height * (i / cols);
        XDrawString (dpy, pixmap, gc, x + 5, y + font->ascent,
            list[i].name, list[i].len);
        XCopyArea (dpy, list[i].bitmap, pixmap, gc,
            0, 0, list[i].width, list[i].height,
            x + 5, y + font->ascent + font->descent);
        /* Once we copy it into the big picture, we don't need the bitmap */
        XFreePixmap (dpy, list[i].bitmap);
    }
    XtFree (list); /* don't need the array of structs anymore */
    XFreeGC (dpy, gc); /* nor do we need this GC */

    /* Create automatic Scrolled Window */
    scrolled_w = XtVaCreateManagedWidget ("scrolled_w",
        xmScrolledWindowWidgetClass, toplevel,
        XmNscrollingPolicy, XmAPPLICATION_DEFINED, /* default values */
        XmNvisualPolicy,    XmVARIABLE,            /* specified for clarity */
        NULL);

    /* Create a drawing area as a child of the ScrolledWindow.
     * The DA's size is initialized (arbitrarily) to view_width and
     * view_height.  The ScrolledWindow will expand to this size.
     */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, scrolled_w,
        XmNwidth,       view_width,
        XmNheight,      view_height,
        NULL);

    XtAddCallback (drawing_a, XmNexposeCallback, expose_resize, NULL);
    XtAddCallback (drawing_a, XmNresizeCallback, expose_resize, NULL);

    /* Application-defined ScrolledWindows won't create their own
     * ScrollBars.  So, we create them ourselves as children of the
     * ScrolledWindow widget.  The vertical ScrollBar's maximum size is
     * the number of rows that exist (in unit values).  The horizontal
     * ScrollBar's maximum width is represented by the number of columns.
     */
    vsb = XtVaCreateManagedWidget ("vsb", xmScrollBarWidgetClass, scrolled_w,
        XmNorientation, XmVERTICAL,
        XmNmaximum,     rows,
        XmNsliderSize,  min (view_height / cell_height, rows),
        XmNpageIncrement, max ((view_height / cell_height) - 1, 1),
        NULL);
    if (view_height / cell_height > rows)
        sw_voffset = (view_height - rows * cell_height) / 2;
    hsb = XtVaCreateManagedWidget ("hsb", xmScrollBarWidgetClass, scrolled_w,
        XmNorientation, XmHORIZONTAL,
        XmNmaximum,     cols,
        XmNsliderSize,  min (view_width / cell_width, cols),
        XmNpageIncrement, max ((view_width / cell_width) - 1, 1),
        NULL);
    if (view_width / cell_width > cols)
        sw_hoffset = (view_width - cols * cell_width) / 2;

    /* Allow the ScrolledWindow to initialize itself accordingly...*/
    XmScrolledWindowSetAreas (scrolled_w, hsb, vsb, drawing_a);

    /* use same callback for both ScrollBars and all callback reasons */
    XtAddCallback (vsb, XmNvalueChangedCallback, scrolled, XmVERTICAL);
    XtAddCallback (hsb, XmNvalueChangedCallback, scrolled, XmHORIZONTAL);
    XtAddCallback (vsb, XmNdragCallback, scrolled, XmVERTICAL);
    XtAddCallback (hsb, XmNdragCallback, scrolled, XmHORIZONTAL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* React to scrolling actions.  Reset position of ScrollBars; call redraw()
 * to do actual scrolling.  cbs->value is ScrollBar's new position.
 */
void
scrolled(scrollbar, client_data, call_data)
Widget scrollbar;
XtPointer client_data;
XtPointer call_data;
{
    int orientation = (int) client_data; /* XmVERTICAL or XmHORIZONTAL */
    XmScrollBarCallbackStruct *cbs = 
        (XmScrollBarCallbackStruct *) call_data;

    if (orientation == XmVERTICAL) {
        pix_voffset = cbs->value * cell_height;
        if (((rows * cell_height) - pix_voffset) > view_height)
            XClearWindow (dpy, XtWindow (drawing_a));
    } else {
        pix_hoffset = cbs->value * cell_width;
        if (((cols * cell_width) - pix_hoffset) > view_width)
            XClearWindow (dpy, XtWindow (drawing_a));
    }
    redraw (XtWindow (drawing_a));
}

/* This function handles both expose and resize (configure) events.
 * For XmCR_EXPOSE, just call redraw() and return.  For resizing,
 * we must calculate the new size of the viewable area and possibly
 * reposition the pixmap's display and position offsets.  Since we
 * are also responsible for the ScrollBars, adjust them accordingly.
 */
void
expose_resize(drawing_a, client_data, call_data)
Widget drawing_a;
XtPointer client_data;
XtPointer call_data;
{
    Dimension new_width, new_height, oldw, oldh;
    Boolean do_clear = False;
    XmDrawingAreaCallbackStruct *cbs = 
        (XmDrawingAreaCallbackStruct *) call_data;

    if (cbs->reason == XmCR_EXPOSE) {
        redraw (cbs->window);
        return;
    }
    oldw = view_width;
    oldh = view_height;

    /* Unfortunately, the cbs->event field is NULL, so we have to have
     * get the size of the drawing area manually.  A misdesign of
     * the DrawingArea widget--not a bug (technically).
     */
    XtVaGetValues (drawing_a,
        XmNwidth,  &view_width,
        XmNheight, &view_height,
        NULL);

    /* Get the size of the viewable area in "units lengths" where
     * each unit is the cell size for each dimension.  This prevents
     * rounding error for the pix_voffset and pix_hoffset values later.
     */
    new_width = view_width / cell_width;
    new_height = view_height / cell_height;

    /* When the user resizes the frame bigger, expose events are generated,
     * so that's not a problem, since the expose handler will repaint the
     * whole viewport.  However, when the window resizes smaller, no
     * expose event is generated.  The window does not need to be
     * redisplayed if the old viewport was smaller than the pixmap.
     * (The existing image is still valid--no redisplay is necessary.)
     * The window WILL need to be redisplayed if:
     *  1) new view size is larger than pixmap (pixmap needs to be centered).
     *  2) new view size is smaller than pixmap, but the OLD view size was
     *     larger than pixmap.
     */
    if ((int) new_height >= rows) {
        /* The height of the viewport is taller than the pixmap, so set
         * pix_voffset = 0, so the top origin of the pixmap is shown,
         * and the pixmap is centered vertically in viewport.
         */
        pix_voffset = 0;
        sw_voffset = (view_height - rows * cell_height)/2;
        /* Case 1 above */
        do_clear = True;
        /* scrollbar is maximum size */
        new_height = rows;
    } else {
        /* Pixmap is larger than viewport, so viewport will be completely
         * redrawn on the redisplay.  (So, we don't need to clear window.)
         * Make sure upper side has origin of a cell (bitmap).
         */
        pix_voffset = min (pix_voffset, (rows-new_height) * cell_height);
        sw_voffset = 0; /* no centering is done */
        /* Case 2 above */
        if (oldh > rows * cell_height)
            do_clear = True;
    }
    XtVaSetValues (vsb,
        XmNsliderSize,    max (new_height, 1),
        XmNvalue,         pix_voffset / cell_height,
        XmNpageIncrement, max (new_height-1, 1),
        NULL);

    /* identical to vertical case above */
    if ((int) new_width >= cols) {
        /* The width of the viewport is wider than the pixmap, so set
         * pix_hoffset = 0, so the left origin of the pixmap is shown,
         * and the pixmap is centered horizontally in viewport.
         */
        pix_hoffset = 0;
        sw_hoffset = (view_width - cols * cell_width)/2;
        /* Case 1 above */
        do_clear = True;
        /* scrollbar is maximum size */
        new_width = cols;
    } else {
        /* Pixmap is larger than viewport, so viewport will be completely
         * redrawn on the redisplay.  (So, we don't need to clear window.)
         * Make sure left side has origin of a cell (bitmap).
         */
        pix_hoffset = min (pix_hoffset, (cols-new_width)*cell_width);
        sw_hoffset = 0;
        /* Case 2 above */
        if (oldw > cols * cell_width)
            do_clear = True;
    }
    XtVaSetValues (hsb,
        XmNsliderSize,    max (new_width, 1),
        XmNvalue,         pix_hoffset / cell_width,
        XmNpageIncrement, max (new_width-1, 1),
        NULL);

    if (do_clear) {
        /* XClearWindow() doesn't generate an ExposeEvent */
        XClearArea (dpy, cbs->window, 0, 0, 0, 0, True);
                              /* all 0's means the whole window */
    }
}

void
redraw(window)
Window window;
{
    static GC gc; /* static variables are *ALWAYS* initialized to NULL */
    if (!gc) { /* !gc means that this GC hasn't yet been created. */
        /* We create our own gc because the other one is based on a 1-bit
         * bitmap and the drawing area window might be color (multiplane).
         * Remember, we're rendering a multiplane pixmap, not the original
         * single-plane bitmaps!
         */
        gc = XCreateGC (dpy, window, NULL, 0);
        XSetForeground (dpy, gc, BlackPixelOfScreen (XtScreen (drawing_a)));
        XSetBackground (dpy, gc, WhitePixelOfScreen (XtScreen (drawing_a)));
    }
    if (DefaultDepthOfScreen (XtScreen (drawing_a)) > 1)
        XCopyPlane (dpy, pixmap, window, gc, pix_hoffset, pix_voffset,
            view_width, view_height, sw_hoffset, sw_voffset, 1L);
    else
        XCopyArea (dpy, pixmap, window, gc, pix_hoffset, pix_voffset,
            view_width, view_height, sw_hoffset, sw_voffset);
}
