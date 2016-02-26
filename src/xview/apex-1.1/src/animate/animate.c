#include <stdio.h>
#include <ctype.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>            /* for <sys/time.h> */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/notify.h>
#include <xview/svrimage.h>
#include "tb_show_bmp"
#include "tb_prev_bmp"
#include "tb_next_bmp"
#include "tb_hide_bmp"

Frame            frame;
Display          *dpy;
GC               gc;
Window           canvas_win;
Notify_value     animate();
struct itimerval timer;

Server_image *images;
int cImages;
int height, width;
int time_value, fPlaying = 0,iFrame = 0;

static void play_proc()
{
	fPlaying = 1;
    timer.it_value.tv_usec = (time_value + 20) * 5000;
    timer.it_interval.tv_usec = (time_value + 20) * 5000;
    notify_set_itimer_func(frame, animate,
            ITIMER_REAL, &timer, NULL);

}
static void prev_proc()
{
	iFrame = (iFrame == 0) ? cImages-1 : iFrame-1;
	XCopyPlane( dpy, (Pixmap)xv_get(images[iFrame], SERVER_IMAGE_PIXMAP),
		canvas_win, gc, 0, 0, width, height, 0, 0, 1);
}

static void next_proc()
{
	iFrame = (iFrame+1)%cImages;
	XCopyPlane( dpy, (Pixmap)xv_get(images[iFrame], SERVER_IMAGE_PIXMAP),
		canvas_win, gc, 0, 0, width, height, 0, 0, 1);
}	

static void pause_proc()
{
		fPlaying = 0;
        notify_set_itimer_func(frame, NOTIFY_FUNC_NULL,
            ITIMER_REAL, NULL, NULL);
}

main(argc, argv)
int     argc;
char    *argv[];
{
    Panel       panel;
    Canvas      canvas;
    XGCValues   gcvalues;
    Xv_Font     _font;
    XFontStruct *font;
    void        adjust_speed(), change_glyph();
	char szFilename[255];
	int i;
	Server_image play, prev, next, pause;
    extern void exit();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	if( argc != 3 ){ 
		fprintf(stderr, "Usage: %s <num_frames> <frame_base_name>\n", argv[0]);
		exit(1);
	}
	
	if( !(cImages = atoi(argv[1])) ) 
		exit(0);
	images = (Server_image *)calloc(cImages, sizeof(Server_image));

	for( i=0; i< cImages; i++ ) {
		sprintf(szFilename, "%s.%d", argv[2], i);
		images[i] = (Server_image)xv_create( NULL, SERVER_IMAGE,
			SERVER_IMAGE_BITMAP_FILE, szFilename,
			NULL );
	}

	play = (Server_image)xv_create(NULL, SERVER_IMAGE,
			SERVER_IMAGE_X_BITS, tb_show_bmp_bits,
			XV_WIDTH, tb_show_bmp_width,
			XV_HEIGHT, tb_show_bmp_height,
			NULL );

	prev = (Server_image)xv_create(NULL, SERVER_IMAGE,
			SERVER_IMAGE_X_BITS, tb_prev_bmp_bits,
			XV_WIDTH, tb_prev_bmp_width,
			XV_HEIGHT, tb_prev_bmp_height,
			NULL );

	next = (Server_image)xv_create(NULL, SERVER_IMAGE,
			SERVER_IMAGE_X_BITS, tb_next_bmp_bits,
			XV_WIDTH, tb_next_bmp_width,
			XV_HEIGHT, tb_next_bmp_height,
			NULL );

	pause = (Server_image)xv_create(NULL, SERVER_IMAGE,
			SERVER_IMAGE_X_BITS, tb_hide_bmp_bits,
			XV_WIDTH, tb_hide_bmp_width,
			XV_HEIGHT, tb_hide_bmp_height,
			NULL );

    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            "apeX Animate v1.1",
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);

    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT,           PANEL_HORIZONTAL,
        XV_WIDTH,               width = (int)xv_get(images[0], XV_WIDTH),
        NULL);
    xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      exit,
        NULL);

	xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE, play,
		PANEL_NOTIFY_PROC, play_proc,
		NULL );

	xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE, prev,
		PANEL_NOTIFY_PROC, prev_proc,
		NULL );

	xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE, next,
		PANEL_NOTIFY_PROC, next_proc,
		NULL );

	xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE, pause,
		PANEL_NOTIFY_PROC, pause_proc,
		NULL );

	xv_set(panel, PANEL_LAYOUT, PANEL_VERTICAL );

    xv_create(panel, PANEL_SLIDER,
        PANEL_LABEL_STRING,     "Animation Rate",
        PANEL_VALUE,            1,
        PANEL_MAX_VALUE,        120,
		PANEL_TICKS, 		10,
        PANEL_NOTIFY_PROC,      adjust_speed,
        NULL);

    window_fit_height(panel);

    canvas = (Canvas)xv_create(frame, CANVAS,
		XV_X, 0,
        XV_WIDTH,               width = (int)xv_get(images[0], XV_WIDTH),
        XV_HEIGHT,              height = (int)xv_get(images[0], XV_HEIGHT),
        CANVAS_X_PAINT_WINDOW,  TRUE,
		WIN_BELOW, panel,
        NULL);
    canvas_win = (Window)xv_get(canvas_paint_window(canvas), XV_XID);

    window_fit(frame);

    dpy = (Display *)xv_get(frame, XV_DISPLAY);

    gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
    gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
    gcvalues.graphics_exposures = False;
    gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
        GCForeground | GCBackground | GCGraphicsExposures,
        &gcvalues);

    xv_main_loop(frame);
}


/*ARGSUSED*/
Notify_value
animate()
{	
	XCopyPlane( dpy, (Pixmap)xv_get(images[iFrame], SERVER_IMAGE_PIXMAP),
		canvas_win, gc, 0, 0, width, height, 0, 0, 1);
	iFrame++;
	iFrame %= cImages;
    return NOTIFY_DONE;
}

void
adjust_speed(item, value)
Panel_item item;
int value;
{
	time_value = value;
	if( fPlaying ){
        timer.it_value.tv_usec = (value + 20) * 5000;
        timer.it_interval.tv_usec = (value + 20) * 5000;
        notify_set_itimer_func(frame, animate,
            ITIMER_REAL, &timer, NULL);
	}
}
