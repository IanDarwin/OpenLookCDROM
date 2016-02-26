/*
 * disp_fonts2.c -- displays a collection of fonts in a canvas.
 */
#include <X11/X.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <xview/xv_xrect.h>

#ifdef SVR4
#define srandom( SEED )         srand( SEED )
#define random( SEED )          rand( SEED )
#endif

GC      gc;
char *font_names[] = {
    "courier", "courier-bold", "courier-16", "courier-oblique-19",
    "lucidasans", "lucidasans-bold", "lucidasans-9", "lucidasans-italic",
    "palatino-roman", "palatino-bold-14", "palatino-roman-7", "palatino-italic",
    "times-bold", "times-bold-16", "times-roman", "times-roman-10", "times-italic-14",
    "charter-black", "charter-black-13", "charter-black-italic",
    "helvetica-20", "gillsans", "gillsans-bold", "gillsans-bolditalic",
    "newcenturyschlbk-roman", "newcenturyschlbk-bold-15",
    "rockwell", "rockwell-italic-16", "symbol", "symbol-16",
    "symbol-19", "bookman-demi", "bookman-light-19",
    "avantgarde-book", "avantgarde-demi", "avantgarde-demioblique"
};
Xv_Font fonts[sizeof(font_names)/ sizeof(char *)];

main(argc, argv)
int      argc;
char    *argv[];
{
    Display     *dpy;
    Frame       frame;
    Canvas      canvas;
    XGCValues   gcvalues;
    Xv_Font     font;
    int         i;
    void        my_repaint_proc();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            argv[0],
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);

    canvas = (Canvas)xv_create(frame, CANVAS,
        XV_WIDTH,               500,
        XV_HEIGHT,              300,
        CANVAS_X_PAINT_WINDOW,  TRUE,
        CANVAS_REPAINT_PROC,    my_repaint_proc,
        NULL);

    window_fit(frame);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);
    for (i = 0; i < sizeof font_names / sizeof (char *); i++)
        fonts[i] = (Xv_Font)xv_find(frame, FONT,
            FONT_NAME, font_names[i],
            NULL);

    gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
    gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
    gcvalues.graphics_exposures = False;
    gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
        GCForeground | GCBackground | GCGraphicsExposures, &gcvalues);

    srandom(time(0));
    xv_main_loop(frame);
}

void
my_repaint_proc(canvas, pw, dpy, xwin, xrects)
Canvas canvas;
Xv_Window pw;
Display *dpy;
Window xwin;
Xv_xrectlist *xrects;
{
    int i, x, y;
    int width = (int)xv_get(canvas, XV_WIDTH);
    int height = (int)xv_get(canvas, XV_HEIGHT);
    XFontStruct *fi;

    x = -90;
    y = 0;
    for (i = 0; i < sizeof font_names / sizeof (char *); i++)
        if (fonts[i]) {
	    if ( x+200 < width ) {
	      x += 100;
	      y += 20;
	    }
	    else {
	      x  = 10; 
	      y -= 15;
	    }
            fi = (XFontStruct *)xv_get(fonts[i], FONT_INFO);
            XSetFont(dpy, gc, fi->fid);
            XDrawString(dpy, xwin, gc, x, y,
                font_names[i], strlen(font_names[i]));
                /* "Hello World", 11); */
        }
}
