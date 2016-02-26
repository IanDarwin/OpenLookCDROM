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

/* xshowbitmap.c -- displays a set of bitmaps specified on the command 
 * line, from a pipe, or typed into stdin.  Bitmaps must be specified 
 * as file names.
 *
 * Usage: xshowbitmap
 *   -s       sorts the bitmaps in order of size with largest first
 *   -v       verbose mode for when input is redirected to stdin
 *   -w       width of viewport window
 *   -h       height of viewport window
 *   -fg      foreground color
 *   -bg      background color
 *   -label   labels each bitmap with its corresponding filename; default
 *   -nolabel doesn't label each bitmap with its filename
 *   -grid N  line width for grid between bitmaps; defaults to 1
 *   -rows N  number of rows; cannot be used with -cols
 *   -cols N  number of columns; cannot be used with -rows
 *   -fn font font for bitmap filenames
 *   -bw max-width  excludes bitmaps larger than this width; defaults to 64
 *   -bh max-height excludes bitmaps larger than this height; defaults to 64
 *   -        indicates to read from stdin; piping doesn't require the '-'
 *            argument
 *            no arguments reads from stdin
 *
 * Example usage:
 *  xshowbitmaps /usr/include/X11/bitmaps/*
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>

#ifdef max
#undef max
#endif
#define max(a,b) ((int)(a)>(int)(b)?(int)(a):(int)(b))
#define min(a,b) ((int)(a)<(int)(b)?(int)(a):(int)(b))

typedef struct {
    char *name;
    int len; 
    unsigned int width, height;
    Pixmap bitmap;
} Bitmap;

/* Resrcs is an object that contains global variables that we want the
 * user to be able to initialize through resources or command line options.
 * XtAppInitialize() initializes the fields in this data structure to values
 * indicated by the XrmOptionsDescRec structure defined later.
 */
struct _resrcs {
    Boolean      sort;                    /* sort the bitmaps */
    Boolean      verbose;                 /* loading bitmaps verbosely */
    Boolean      label_bitmap;            /* whether to label bitmaps */
    int          max_width, max_height;   /* largest allowable bitmap */
    unsigned int grid;                    /* line width between bitmaps */
    Pixel        fg, bg;                  /* colors of bitmaps */
    XFontStruct *font;                    /* font for bitmap labels */
    Dimension    view_width, view_height; /* initial clip window size */
    int          rows, cols;              /* forcefully set #rows/cols */
} Resrcs;

/* .Xdefaults or app-defaults resources.  The last field in each structure
 * is used as the default values for the field in the Resrcs struct above.
 */
static XtResource resources[] = {
    { "sort", "Sort", XmRBoolean, sizeof (Boolean),
        XtOffsetOf (struct _resrcs, sort), XmRImmediate, False },
    { "verbose", "Verbose", XmRBoolean, sizeof (Boolean),
        XtOffsetOf (struct _resrcs,verbose), XmRImmediate, False },
    { "labelBitmap", "LabelBitmap", XmRBoolean, sizeof (Boolean),
        XtOffsetOf (struct _resrcs, label_bitmap), XmRImmediate, 
        (char *) True },
    { "grid", "Grid", XmRInt, sizeof (int),
        XtOffsetOf (struct _resrcs, grid), XmRImmediate, (char *) 1 },
    { "bitmapWidth", "BitmapWidth", XmRInt, sizeof (int),
        XtOffsetOf (struct _resrcs, max_width), XmRImmediate, (char *) 64 },
    { "bitmapHeight", "BitmapHeight", XmRInt, sizeof (int),
        XtOffsetOf (struct _resrcs, max_height), XmRImmediate, (char *) 64 },
    { XmNfont, XmCFont, XmRFontStruct, sizeof (XFontStruct *),
        XtOffsetOf (struct _resrcs, font), XmRString, XtDefaultFont },
    { XmNforeground, XmCForeground, XmRPixel, sizeof (Pixel),
        XtOffsetOf (struct _resrcs, fg), XmRString, XtDefaultForeground },
    { XmNbackground, XmCBackground, XmRPixel, sizeof (Pixel),
        XtOffsetOf (struct _resrcs, bg), XmRString, XtDefaultBackground },
    { "view-width", "View-width", XmRDimension, sizeof (Dimension),
        XtOffsetOf (struct _resrcs, view_width), XmRImmediate, 
        (char *) 500 },
    { "view-height", "View-height", XmRDimension, sizeof (Dimension),
        XtOffsetOf (struct _resrcs, view_height), XmRImmediate, 
        (char *) 300 },
    { "rows", "Rows", XmRInt, sizeof (int),
        XtOffsetOf (struct _resrcs, rows), XmRImmediate, 0 },
    { "cols", "Cols", XmRInt, sizeof (int),
        XtOffsetOf (struct _resrcs, cols), XmRImmediate, 0 },
};

/* If the following command line args (1st field) are found, set the
 * associated resource values (2nd field) to the given value (4th field).
 */
static XrmOptionDescRec options[] = {
    { "-sort", "sort", XrmoptionNoArg, "True" },
    { "-v", "verbose", XrmoptionNoArg, "True" },
    { "-fn", "font", XrmoptionSepArg, NULL },
    { "-fg", "foreground", XrmoptionSepArg, NULL },
    { "-bg", "background", XrmoptionSepArg, NULL },
    { "-w", "view-width", XrmoptionSepArg, NULL },
    { "-h", "view-height", XrmoptionSepArg, NULL },
    { "-rows", "rows", XrmoptionSepArg, NULL },
    { "-cols", "cols", XrmoptionSepArg, NULL },
    { "-bw", "bitmapWidth", XrmoptionSepArg, NULL },
    { "-bh", "bitmapHeight", XrmoptionSepArg, NULL },
    { "-bitmap_width", "bitmapWidth", XrmoptionSepArg, NULL },
    { "-bitmap_height", "bitmapHeight", XrmoptionSepArg, NULL },
    { "-label", "labelBitmap", XrmoptionNoArg, "True" },
    { "-nolabel", "labelBitmap", XrmoptionNoArg, "False" },
    { "-grid", "grid", XrmoptionSepArg, NULL },
};

/* size_cmp() -- used by qsort to sort bitmaps into alphabetical order 
 * This is used when the "sort" resource is true or when -sort is given.
 */
size_cmp(b1, b2)
Bitmap *b1, *b2;
{
    int n = (int) (b1->width * b1->height) - (int) (b2->width * b2->height);
    if (n)
        return n;
    return strcmp (b1->name, b2->name);
}

/* int_sqrt() -- get the integer square root of n.  Used to put the 
 * bitmaps in an equal number of rows and colums.
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

/* global variables that are not changable thru resources or command
 * line options.
 */
Widget drawing_a, vsb, hsb;
Pixmap pixmap; /* used the as image for Label widget */
GC gc;
Display *dpy;
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
    char buf[128], *p;
    XFontStruct *font;
    int istty = isatty(0), redirect = !istty, i = 0, total = 0;
    unsigned int bitmap_error;
    int j, k;
    void scrolled(), expose_resize();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtAppInitialize (&app, "XShowbitmap",
        options, XtNumber (options), &argc, argv, NULL, NULL, 0);
    dpy = XtDisplay (toplevel);

    XtGetApplicationResources (toplevel, &Resrcs,
        resources, XtNumber (resources), NULL, 0);

    if (Resrcs.rows && Resrcs.cols)
        XtWarning ("You can't specify both rows *and* columns.");

    font = Resrcs.font;

    /* check to see if we have to load the bitmaps from stdin */
    if (!argv[1] || !strcmp(argv[1], "-")) {
        printf ("Loading bitmap names from standard input. ");
        if (istty) {
            puts ("End with EOF or .");
            redirect++;
        } 
        else
            puts ("Use -v to view bitmap names being loaded.");
    } 
    else if (!istty && strcmp(argv[1], "-")) {
        printf ("%s: either use pipes or specify bitmap names.\n",
            argv[0]);
        exit (1);
    }

    /* Now, load the bitmap file names */
    while (*++argv || redirect) {
        if (!redirect)
            /* this may appear at the end of a list of filenames */
            if (!strcmp (*argv, "-"))
                redirect++; /* switch to stdin prompting */
            else
                (void) strcpy (buf, *argv);
        if (redirect) {
            if (istty)
                printf ("Bitmap file: "), fflush(stdout);
            if (!fgets (buf, sizeof buf - 1, stdin) || !strcmp (buf, ".\n"))
                break;
            buf[strlen (buf) - 1] = 0; /* plug a null at the newline */
        }
        if (!buf[0])
            continue;
        if (Resrcs.verbose)
            printf ("Loading \"%s\"...", buf), fflush(stdout);
        if (i == total) {
            total += 10; /* allocate bitmap structures in groups of 10 */
            if (!(list = (Bitmap *) XtRealloc 
                    (list, total * sizeof (Bitmap))))
                XtError ("Not enough memory for bitmap data");
        }
        if ((bitmap_error = XReadBitmapFile (dpy, DefaultRootWindow(dpy), 
                buf, &list[i].width, &list[i].height, &list[i].bitmap,
                &j, &k)) == BitmapSuccess) {
            if (p = rindex (buf, '/'))
                p++;
            else
                p = buf;
            if (Resrcs.max_height && list[i].height > Resrcs.max_height ||
                Resrcs.max_width && list[i].width > Resrcs.max_width) {
                printf ("%s: bitmap too big\n", p);
                XFreePixmap (dpy, list[i].bitmap);
                continue;
            }
            list[i].len = strlen (p);
            list[i].name = strcpy (XtMalloc (list[i].len + 1), p);
            if (Resrcs.verbose)
                printf ("size: %dx%d\n", list[i].width, list[i].height);
            i++;
        } 
        else {
            printf ("Couldn't load bitmap: ");
            if (!istty && !Resrcs.verbose)
                printf("\"%s\": ", buf);
            switch (bitmap_error) {
                case BitmapOpenFailed : puts ("open failed."); break;
                case BitmapFileInvalid : puts ("bad file format."); break;
                case BitmapNoMemory : puts ("not enough memory."); break;
            }
        }
    }
    if ((total = i) == 0) {
        puts ("couldn't load any bitmaps.");
        exit (1);
    }
    printf ("Total bitmaps loaded: %d\n", total);
    if (Resrcs.sort) {
        printf ("Sorting bitmaps...");
        fflush (stdout);
        qsort (list, total, sizeof (Bitmap), size_cmp);
        putchar ('\n');
    }

    /* calculate size for pixmap by getting the dimensions of each bitmap. */
    printf ("Calculating sizes for pixmap...");
    fflush (stdout);
    for (i = 0; i < total; i++) {
        if (list[i].width > cell_width)
            cell_width = list[i].width;
        if (list[i].height > cell_height)
            cell_height = list[i].height;
        if (Resrcs.label_bitmap && (j = XTextWidth 
                (font, list[i].name, list[i].len)) > cell_width)
            cell_width = j;
    }

    /* Compensate for vertical font height if label_bitmap is true.
     * Add value of grid line weight and a 6 pixel padding for aesthetics.
     */
    cell_height += Resrcs.grid + 6 + 
        Resrcs.label_bitmap * (font->ascent + font->descent);
    cell_width += Resrcs.grid + 6;

    /* if user didn't specify row/column layout figure it out ourselves.
     * optimize layout by making it "square".
     */
    if (!Resrcs.rows && !Resrcs.cols) {
        Resrcs.cols = int_sqrt (total);
        Resrcs.rows = (total + Resrcs.cols - 1) / Resrcs.cols;
    } 
    else if (Resrcs.rows)
        /* user specified rows -- figure out columns */
        Resrcs.cols = (total + Resrcs.rows - 1) / Resrcs.rows;
    else
        /* user specified cols -- figure out rows */
        Resrcs.rows = (total + Resrcs.cols - 1) / Resrcs.cols;

    printf ("Creating pixmap area of size %dx%d (%d rows, %d cols)\n",
        Resrcs.cols * cell_width, Resrcs.rows * cell_height,
        Resrcs.rows, Resrcs.cols);

    if (!(pixmap = XCreatePixmap (dpy, DefaultRootWindow(dpy),
            Resrcs.cols * cell_width, Resrcs.rows * cell_height,
            DefaultDepthOfScreen (XtScreen (toplevel)))))
        XtError ("Can't Create pixmap.");

    if (!(gc = XCreateGC (dpy, pixmap, NULL, 0)))
        XtError ("Can't create gc.");
    XSetForeground (dpy, gc, Resrcs.bg); /* init GC's foreground to bg */
    XFillRectangle (dpy, pixmap, gc, 0, 0,
        Resrcs.cols * cell_width, Resrcs.rows * cell_height);
    XSetForeground (dpy, gc, Resrcs.fg);
    XSetBackground (dpy, gc, Resrcs.bg);
    XSetFont (dpy, gc, font->fid);
    if (Resrcs.grid) {
        if (Resrcs.grid != 1)
            /* Line weight of 1 is faster when left as 0 (the default) */
            XSetLineAttributes (dpy, gc, Resrcs.grid, 0, 0, 0);
        for (j = 0; j <= Resrcs.rows * cell_height; j += cell_height)
            XDrawLine (dpy, pixmap, gc, 0, j, Resrcs.cols * cell_width, j);
        for (j = 0; j <= Resrcs.cols * cell_width; j += cell_width)
            XDrawLine (dpy, pixmap, gc, j, 0, j, Resrcs.rows * cell_height);
    }

    /* Draw each of the bitmaps into the big picture */
    for (i = 0; i < total; i++) {
        int x = cell_width * (i % Resrcs.cols);
        int y = cell_height * (i / Resrcs.cols);
        if (Resrcs.label_bitmap)
            XDrawString (dpy, pixmap, gc,
                x + 5 + Resrcs.grid / 2, y + font->ascent + Resrcs.grid / 2,
                list[i].name, list[i].len);
        if (DefaultDepthOfScreen (XtScreen (toplevel)) > 1)
            XCopyPlane (dpy, list[i].bitmap, pixmap, gc,
                0, 0, list[i].width, list[i].height,
                x + 5 + Resrcs.grid / 2,
                y + font->ascent + font->descent + Resrcs.grid / 2, 1L);
        else
            XCopyArea (dpy, list[i].bitmap, pixmap, gc,
                0, 0, list[i].width, list[i].height,
                x + 5 + Resrcs.grid / 2,
                y + font->ascent + font->descent + Resrcs.grid / 2);
        XFreePixmap (dpy, list[i].bitmap);
        XtFree (list[i].name);
    }
    XtFree (list);

    /* Now we get into the Motif stuff */

    /* Create automatic Scrolled Window */
    scrolled_w = XtVaCreateManagedWidget ("scrolled_w",
        xmScrolledWindowWidgetClass, toplevel,
        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
        XmNvisualPolicy,    XmVARIABLE,
        XmNshadowThickness, 0,
        NULL);

    /* Create a drawing area as a child of the ScrolledWindow.
     * The DA's size is initialized (arbitrarily) to view_width and
     * view_height.  The ScrolledWindow will expand to this size.
     */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, scrolled_w,
        XmNwidth,       Resrcs.view_width,
        XmNheight,      Resrcs.view_height,
        NULL);
    XtAddCallback (drawing_a, XmNexposeCallback, expose_resize, NULL);
    XtAddCallback (drawing_a, XmNresizeCallback, expose_resize, NULL);

    /* Application-defined ScrolledWindows won't create their own
     * ScrollBars.  So, we create them ourselves as children of the
     * ScrolledWindow widget.  The vertical ScrollBar's maximum size is
     * the number of rows that exist (in unit values).  The horizontal
     * ScrollBar's maximum width is represented by the number of columns.
     */
    vsb = XtVaCreateManagedWidget ("vsb", 
        xmScrollBarWidgetClass, scrolled_w,
        XmNorientation,   XmVERTICAL,
        XmNmaximum,       Resrcs.rows,
        XmNsliderSize,    min (Resrcs.view_height / cell_height, Resrcs.rows),
        NULL);
    if (Resrcs.view_height / cell_height > Resrcs.rows)
        sw_voffset = (Resrcs.view_height - Resrcs.rows * cell_height) / 2;
    hsb = XtVaCreateManagedWidget ("hsb", 
        xmScrollBarWidgetClass, scrolled_w,
        XmNorientation,   XmHORIZONTAL,
        XmNmaximum,       Resrcs.cols,
        XmNsliderSize,    min (Resrcs.view_width / cell_width, Resrcs.cols),
        NULL);
    if (Resrcs.view_width / cell_width > Resrcs.cols)
        sw_hoffset = (Resrcs.view_width - Resrcs.cols * cell_width) / 2;

    /* Allow the ScrolledWindow to initialize itself accordingly...*/
    XmScrolledWindowSetAreas (scrolled_w, hsb, vsb, drawing_a);

    XtAddCallback (vsb, XmNvalueChangedCallback, scrolled, XmVERTICAL);
    XtAddCallback (hsb, XmNvalueChangedCallback, scrolled, XmHORIZONTAL);
    XtAddCallback (vsb, XmNdragCallback, scrolled, XmVERTICAL);
    XtAddCallback (hsb, XmNdragCallback, scrolled, XmHORIZONTAL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* scrolled() -- react to scrolling actions; cbs->value is ScrollBar's 
 * new position.
*/
void
scrolled(scrollbar, client_data, call_data)
Widget scrollbar;
XtPointer client_data;
XtPointer call_data;
{
    int orientation = (int) client_data;
    XmScrollBarCallbackStruct *cbs = 
        (XmScrollBarCallbackStruct *) call_data;

    if (orientation == XmVERTICAL)
        pix_voffset = cbs->value * cell_height;
    else
        pix_hoffset = cbs->value * cell_width;
    redraw (XtWindow (drawing_a));
}

/* expose_resize() -- handles both expose and resize (configure) events.
 * For XmCR_EXPOSE, just call redraw() and return.  For resizing,
 * we must calculate the new size of the viewable area and possibly
 * reposition the pixmap's display and position offset.  Since we
 * are also responsible for the ScrollBars, adjust them accordingly.
 */
void
expose_resize(drawing_a, client_data, call_data)
Widget drawing_a;
XtPointer client_data;
XtPointer call_data;
{
    XmDrawingAreaCallbackStruct *cbs = 
        (XmDrawingAreaCallbackStruct *) call_data;
    Dimension view_width, view_height, oldw, oldh;
    int do_clear = 0;

    if (cbs->reason == XmCR_EXPOSE) {
        redraw (cbs->window);
        return;
    }
    oldw = Resrcs.view_width;
    oldh = Resrcs.view_height;

    /* Unfortunately, the cbs->event field is NULL, we have to have
     * get the size of the drawing area manually.
     */
    XtVaGetValues (drawing_a,
        XmNwidth,  &Resrcs.view_width,
        XmNheight, &Resrcs.view_height,
        NULL);

    /* Get the size of the viewable area in "units lengths" where
     * each unit is the cell size for each dimension.  This prevents
     * rounding error for the {vert,horiz}_start values later.
     */
    view_width = Resrcs.view_width / cell_width;
    view_height = Resrcs.view_height / cell_height;

    /* When the user resizes the frame bigger, expose events are generated,
     * so that's not a problem, since the expose handler will repaint the
     * whole viewport.  However, when the window resizes smaller, then no
     * expose event is generated.  In this case, the window does not need
     * to be redisplayed if the old viewport was smaller than the pixmap.
     * (The existing image is still valid--no redisplay is necessary.)
     * The window WILL need to be redisplayed if:
     *  1) new view size is larger than pixmap (pixmap needs to be centered).
     *  2) new view size is smaller than pixmap, but the OLD view size was
     *     larger than pixmap.
     */
    if ( (int) view_height >= Resrcs.rows) {
        /* The height of the viewport is taller than the pixmap, so set
         * pix_voffset = 0, so the top origin of the pixmap is shown,
         * and the pixmap is centered vertically in viewport.
         */
        pix_voffset = 0;
        sw_voffset = (Resrcs.view_height - Resrcs.rows * cell_height) / 2;
        /* Case 1 above */
        do_clear = 1;
        /* scrollbar is maximum size */
        view_height = Resrcs.rows;
    } 
    else {
        /* Pixmap is larger than viewport, so viewport will be completely
         * redrawn on the redisplay.  (So, we don't need to clear window.)
         * Make sure upper side has origin of a cell (bitmap).
         */
        pix_voffset = min (pix_voffset, 
            (Resrcs.rows-view_height) * cell_height);
        sw_voffset = 0; /* no centering is done */
        /* Case 2 above */
        if (oldh > Resrcs.rows * cell_height)
            do_clear = 1;
    }
    XtVaSetValues (vsb,
        XmNsliderSize,    max (view_height, 1),
        XmNvalue,         pix_voffset / cell_height,
        XmNpageIncrement, max (view_height - 1, 1),
        NULL);

    /* identical to vertical case above */
    if ( (int) view_width >= Resrcs.cols) {
        /* The width of the viewport is wider than the pixmap, so set
         * pix_hoffset = 0, so the left origin of the pixmap is shown,
         * and the pixmap is centered horizontally in viewport.
         */
        pix_hoffset = 0;
        sw_hoffset = (Resrcs.view_width - Resrcs.cols * cell_width) / 2;
        /* Case 1 above */
        do_clear = 1;
        /* scrollbar is maximum size */
        view_width = Resrcs.cols;
    } 
    else {
        /* Pixmap is larger than viewport, so viewport will be completely
         * redrawn on the redisplay.  (So, we don't need to clear window.)
         * Make sure left side has origin of a cell (bitmap).
         */
        pix_hoffset = min (pix_hoffset, 
            (Resrcs.cols - view_width) * cell_width);
        sw_hoffset = 0;
        /* Case 2 above */
        if (oldw > Resrcs.cols * cell_width)
            do_clear = 1;
    }
    XtVaSetValues (hsb,
        XmNsliderSize,    max (view_width, 1),
        XmNvalue,         pix_hoffset / cell_width,
        XmNpageIncrement, max (view_width - 1, 1),
        NULL);

    if (do_clear)
        /* XClearWindow() doesn't generate an ExposeEvent */
        XClearArea (dpy, cbs->window, 0, 0, 0, 0, True);
}

void
redraw(window)
Window window;
{
    XCopyArea (dpy, pixmap, window, gc, pix_hoffset, pix_voffset,
        Resrcs.view_width, Resrcs.view_height, sw_hoffset, sw_voffset);
}
