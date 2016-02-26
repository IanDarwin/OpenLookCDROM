/* $Id: xvlib.c,v 1.25 92/07/24 20:53:12 pturner Exp Locker: pturner $
 *
 * driver for xlib for gr
 *
 */

#include <stdio.h>

#ifdef XVIEW

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>

#endif

#ifdef MOTIF

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>

#endif

#include "externs.h"
#include "patterns.h"

/* external variables */
extern Display *disp;
extern GC gc;
extern GC gcxor;
extern GC gcclr;
extern Window xwin;
extern XGCValues gc_val;

extern int use_colors;
extern int use_defaultcmap;
extern int revflag;		/* defined in main.c */
extern int inwin;

extern double devcharsize;
extern double charsize;

#define NUM_COLORS 256

unsigned char red[NUM_COLORS], green[NUM_COLORS], blue[NUM_COLORS];
unsigned long colors[NUM_COLORS];
XColor xc[NUM_COLORS];
int ncolors;
Colormap cmap, mycmap;

#ifdef XVIEW

extern Xv_Window paint_window;
extern Cms cms;
Xv_singlecolor cmscolors[256];

#endif

#ifdef MOTIF

extern Widget canvas;
extern Widget app_shell;
XColor cmscolors[256];

#endif

#define MINCOLOR 0
#define MAXLINEW 15

extern int maxcolors;

double devtoworldx(), devtoworldy();

static void flush_pending();

static int xlibcolor = 1;
static int xliblinewidth = 0;
static int xlibdmode;
static int xlibfont = 0;
static int xliblinestyle = 1;
static int doublebuff = 0;	/* no double buffering by default */
static Pixmap backbuff, displaybuff;
int win_h, win_w;

extern int backingstore;
Pixmap backpix;

/*
 * the following is a tunable parameter and may
 * need to be adjusted
 */
#ifdef HIRES
double xlibcharsize = 0.80;

#else
double xlibcharsize = 0.60;

#endif

double xconv(), yconv();

void drawxlib();
void xlibdoublebuffer();
void xlibinit_tiles();

void get_xlib_dims(w, h)
    int *w, *h;
{
#ifdef MOTIF
    Arg args;
    Dimension ww, wh;

    XtSetArg(args, XmNwidth, &ww);
    XtGetValues(canvas, &args, 1);
    XtSetArg(args, XmNheight, &wh);
    XtGetValues(canvas, &args, 1);
    *w = ww;
    *h = wh;
#endif
}

static void xlibinit()
{
    double wx1, wx2, wy1, wy2;
    static int inc = 1;
    extern int invert, overlay, doclear, bgcolor;

#ifdef MOTIF
    Arg args;
    Dimension ww, wh;

#endif

#ifdef XVIEW
    win_h = (int) xv_get(paint_window, XV_HEIGHT);
    win_w = (int) xv_get(paint_window, XV_WIDTH);
#endif

#ifdef MOTIF
    disp = XtDisplay(canvas);
    xwin = XtWindow(canvas);
    XtSetArg(args, XmNwidth, &ww);
    XtGetValues(canvas, &args, 1);
    XtSetArg(args, XmNheight, &wh);
    XtGetValues(canvas, &args, 1);
    win_w = ww;
    win_h = wh;
#endif

    devwidth = win_w;
    devheight = win_h;
    wx1 = DisplayWidth(disp, DefaultScreen(disp));
    wx2 = DisplayWidthMM(disp, DefaultScreen(disp));
    wy1 = DisplayHeight(disp, DefaultScreen(disp));
    wy2 = DisplayHeightMM(disp, DefaultScreen(disp));
    devwidthmm = (int) (wx2 / wx1 * win_w);
    devheightmm = (int) (wy2 / wy1 * win_h);
    if (inc) {
	gc = DefaultGC(disp, DefaultScreen(disp));
	gc_val.foreground = WhitePixel(disp, DefaultScreen(disp));
	if (invert) {
	    gc_val.function = GXinvert;
	} else {
	    gc_val.function = GXxor;
	}
	gcxor = XCreateGC(disp, xwin, GCFunction | GCForeground, &gc_val);
	gc_val.foreground = colors[0];
	gc_val.function = GXcopy;
	gcclr = XCreateGC(disp, xwin, GCFunction | GCForeground, &gc_val);
	xlibinit_tiles();
	if (backingstore) {
	    backpix = XCreatePixmap(disp, DefaultRootWindow(disp), win_w, win_h, DisplayPlanes(disp, DefaultScreen(disp)));
	}
	inc = 0;
    }
    if (doublebuff) {
	xlibdoublebuffer(doublebuff);
	displaybuff = backbuff;
    } else {
	displaybuff = xwin;
    }
    if (doclear && !overlay) {
	xlibsetcolor(bgcolor);
	XFillRectangle(disp, displaybuff, gc, 0, 0, win_w, win_h);
	if (backingstore) {
	    XFillRectangle(disp, backpix, gc, 0, 0, win_w, win_h);
	}
    }
}

void xlibdoublebuffer(mode)
    int mode;
{
    extern int inwin;

    doublebuff = mode;
    if (!inwin) {
	return;
    }
    if (mode) {
	if (!backbuff) {
	    backbuff = XCreatePixmap(disp, DefaultRootWindow(disp), win_w, win_h, DisplayPlanes(disp, DefaultScreen(disp)));
	}
	displaybuff = backbuff;
    } else {
	if (backbuff) {
	    XFreePixmap(disp, backbuff);
	    backbuff = NULL;
	    displaybuff = xwin;
	}
    }
}

void xlibfrontbuffer(mode)
    int mode;
{
    extern int inwin;

    if (!inwin) {
	return;
    }
    if (mode) {
	displaybuff = xwin;
    } else {
	if (doublebuff && backbuff) {
	    displaybuff = backbuff;
	}
    }
}

void xlibbackbuffer(mode)
    int mode;
{
    extern int inwin;

    if (!inwin) {
	return;
    }
    if (mode && doublebuff && backbuff) {
	displaybuff = backbuff;
    } else {
	displaybuff = xwin;
    }
}

void xlibswapbuffer()
{
    extern int inwin;

    if (!inwin) {
	return;
    }
    if (doublebuff && backbuff) {
	XCopyArea(disp, displaybuff, xwin, gc, 0, 0, win_w, win_h, 0, 0);
    }
}

void refresh_from_backpix()
{
    if (backpix) {
	XCopyArea(disp, backpix, xwin, gc, 0, 0, win_w, win_h, 0, 0);
    }
}

void resize_backpix()
{
    XFreePixmap(disp, backpix);
    backpix = XCreatePixmap(disp, DefaultRootWindow(disp), win_w, win_h, DisplayPlanes(disp, DefaultScreen(disp)));
}

static int xlib_write_mode = 1;

void set_write_mode(m)
    int m;
{
    flush_pending();
    xlib_write_mode = m;
}

void xlibsetmode(mode)
    int mode;
{
    switch (mode) {
    case 1:
	xlibinit();
	break;
    case 2:
	flush_pending();
	if (doublebuff && backbuff) {
	    xlibswapbuffer();
	}
	break;
    }
}

/*
 * fix for dotted/dashed linestyles
 */
#define MAXL 1024
static int npending;
XPoint polypoints[MAXL];

static unsigned char solid[1] = {1};
static unsigned char dotted[2] = {3, 1};
static unsigned char shortdashed[2] = {3, 3};
static unsigned char longdashed[2] = {7, 7};
static unsigned char dot_dashed[4] = {1, 3, 7, 3};

static unsigned char *dash_list[] = {
    solid,
    dotted,
    shortdashed,
    longdashed,
    dot_dashed
};

static int dash_list_length[] = {1, 2, 2, 2, 4};

static void flush_pending()
{
    int i;

    if (npending > 1) {
	if (xlib_write_mode) {
	    XDrawLines(disp, displaybuff, gc, polypoints, npending, CoordModeOrigin);
	    if (backingstore) {
		XDrawLines(disp, backpix, gc, polypoints, npending, CoordModeOrigin);
	    }
	} else {
	    XDrawLines(disp, displaybuff, gcclr, polypoints, npending, CoordModeOrigin);
	    if (backingstore) {
		XDrawLines(disp, backpix, gcclr, polypoints, npending, CoordModeOrigin);
	    }
	}
    }
    npending = 0;
}

static int x1, y1;

void drawxlib(x, y, mode)
    int x, y, mode;
{
    if (mode) {
	polypoints[npending].x = x;
	polypoints[npending].y = win_h - y;
	npending++;
	if (npending == MAXL) {
	    flush_pending();
	    polypoints[npending].x = x;
	    polypoints[npending].y = win_h - y;
	    npending = 1;
	}
    } else {
	if ((x == x1 && y == y1)) {
	    return;
	} else {
	    flush_pending();
	    polypoints[npending].x = x;
	    polypoints[npending].y = win_h - y;
	    npending = 1;
	}
    }
    x1 = x;
    y1 = y;
}

int xconvxlib(x)
    double x;
{
    return ((int) (win_w * xconv(x)));
}

int yconvxlib(y)
    double y;
{
    return ((int) (win_h * yconv(y)));
}

void xlibsetfont(n)
    int n;
{
    flush_pending();
    x1 = y1 = 99999;
    hselectfont(xlibfont = n);
}

/*
 * initialize_cms_data()
 *    Initialize the colormap segment data and setup the RGB values.
 */
void initialize_cms_data()
{
    int i, del;

    /* white  */
    red[0] = 255;
    green[0] = 255;
    blue[0] = 255;
    /* black    */
    red[1] = 0;
    green[1] = 0;
    blue[1] = 0;
    /* red    */
    red[2] = 255;
    green[2] = 0;
    blue[2] = 0;
    /* green  */
    red[3] = 0;
    green[3] = 255;
    blue[3] = 0;
    /* blue   */
    red[4] = 0;
    green[4] = 0;
    blue[4] = 255;
    /* yellow */
    red[5] = 255;
    green[5] = 255;
    blue[5] = 0;
    /* brown  */
    red[6] = 188;
    green[6] = 143;
    blue[6] = 143;
    /* gray   */
    red[7] = 220;
    green[7] = 220;
    blue[7] = 220;
    /* violet  */
    red[8] = 148;
    green[8] = 0;
    blue[8] = 211;
    /* cyan  */
    red[9] = 0;
    green[9] = 255;
    blue[9] = 255;
    /* magenta  */
    red[10] = 255;
    green[10] = 0;
    blue[10] = 211;
    /* orange  */
    red[11] = 255;
    green[11] = 138;
    blue[11] = 0;
    /* blue violet  */
    red[12] = 114;
    green[12] = 33;
    blue[12] = 188;
    /* maroon  */
    red[13] = 103;
    green[13] = 7;
    blue[13] = 72;
    /* turquoise  */
    red[14] = 72;
    green[14] = 209;
    blue[14] = 204;
    /* forest green  */
    red[15] = 85;
    green[15] = 192;
    blue[15] = 52;
    del = (maxcolors - 16) / 3;
    for (i = 16; i < maxcolors; i++) {
	red[i] = (i - 16) * 4 * ((i - 16) < del);
	green[i] = (i - 16) * 3 * ((i - 16) < 2 * del);
	blue[i] = (i - 16) * 2 * ((i - 16) <= maxcolors);
    }
    for (i = 0; i < maxcolors; i++) {
	cmscolors[i].red = red[i];
	cmscolors[i].green = green[i];
	cmscolors[i].blue = blue[i];
    }
}

/* NOTE: not called by xvgr */
void xlibinitcmap()
{
    int i;

    ncolors = DisplayCells(disp, DefaultScreen(disp));
    if (ncolors > 256) {
	ncolors = 256;
    }
    if (ncolors > 16) {
	cmap = DefaultColormap(disp, DefaultScreen(disp));
	for (i = 0; i < ncolors; i++) {
	    xc[i].pixel = i;
	    xc[i].flags = DoRed | DoGreen | DoBlue;
	}
	if (!use_defaultcmap) {
	    XQueryColors(disp, cmap, xc, ncolors);
	    mycmap = XCreateColormap(disp, xwin, DefaultVisual(disp, DefaultScreen(disp)), AllocAll);
	} else {
	    mycmap = cmap;
	}
	for (i = 2; i < maxcolors; i++) {
	    xc[i].red = red[i] << 8;
	    xc[i].green = green[i] << 8;
	    xc[i].blue = blue[i] << 8;
	    if (use_defaultcmap) {
		if (!XAllocColor(disp, cmap, &xc[i])) {
		    fprintf(stderr, " Can't allocate color\n");
		}
	    }
	    colors[i] = xc[i].pixel;
	}
	if (!use_defaultcmap) {
	    XStoreColors(disp, mycmap, xc, ncolors);
	}
    }
    if (revflag) {
	colors[1] = WhitePixel(disp, DefaultScreen(disp));
	colors[0] = BlackPixel(disp, DefaultScreen(disp));
    } else {
	colors[0] = WhitePixel(disp, DefaultScreen(disp));
	colors[1] = BlackPixel(disp, DefaultScreen(disp));
    }
}

void xlibsetcmap(i, r, g, b)
    int i, r, g, b;
{
    XColor xct;

    red[i] = r;
    green[i] = g;
    blue[i] = b;
    cmscolors[i].red = red[i];
    cmscolors[i].green = green[i];
    cmscolors[i].blue = blue[i];
    if (inwin && use_colors > 4 && i >= 2) {
	xct.green = g << 8;
	xct.blue = b << 8;
	xct.red = r << 8;
	xct.flags = DoRed | DoGreen | DoBlue;
	xct.pixel = colors[i];
	xct.pad = 0;

#ifdef XVIEW
	xv_set(cms,
	       CMS_COLOR_COUNT, 1,
	       CMS_INDEX, i,
	       CMS_X_COLORS, &xct,
	       NULL);
#endif
#ifdef MOTIF
	XStoreColor(disp, mycmap, &xct);
#endif
    }
}

int xlibsetlinewidth(c)
    int c;
{
    flush_pending();
    x1 = y1 = 99999;
    if (c) {
	c = c % MAXLINEW;
	if (c == 0)
	    c = 1;
	if (xliblinestyle == 1) {
	    XSetLineAttributes(disp, gc, c - 1 == 0 ? 0 : c, LineSolid, CapButt, JoinRound);
	} else {
	    XSetLineAttributes(disp, gc, c - 1 == 0 ? 0 : c, LineOnOffDash, CapButt, JoinRound);
	}
    }
    return (xliblinewidth = c);
}

int xlibsetlinestyle(style)
    int style;
{
    flush_pending();
    x1 = y1 = 99999;
    if (style > 1 && xliblinewidth) {
	XSetLineAttributes(disp, gc, xliblinewidth - 1 ? 0 : xliblinewidth, LineOnOffDash, CapButt, JoinRound);
	XSetDashes(disp, gc, 0, dash_list[style - 1], dash_list_length[style - 1]);
    } else if (style == 1 && xliblinewidth) {
	XSetLineAttributes(disp, gc, xliblinewidth - 1 ? 0 : xliblinewidth, LineSolid, CapButt, JoinRound);
    }
    return (xliblinestyle = style);
}

int xlibsetcolor(c)
    int c;
{
    flush_pending();
    x1 = y1 = 99999;
    c = c % maxcolors;
    if (use_colors > 4) {
	XSetForeground(disp, gc, colors[c]);
    } else {
	XSetForeground(disp, gc, colors[c != 0]);
    }
    xlibcolor = c;
    return c;
}

void xlibdrawtic(x, y, dir, updown)
    int x, y, dir, updown;
{
    switch (dir) {
    case 0:
	switch (updown) {
	case 0:
	    drawxlib(x, y, 0);
	    drawxlib(x, y + devxticl, 1);
	    break;
	case 1:
	    drawxlib(x, y, 0);
	    drawxlib(x, y - devxticl, 1);
	    break;
	}
	break;
    case 1:
	switch (updown) {
	case 0:
	    drawxlib(x, y, 0);
	    drawxlib(x + devyticl, y, 1);
	    break;
	case 1:
	    drawxlib(x, y, 0);
	    drawxlib(x - devyticl, y, 1);
	    break;
	}
	break;
    }
}

void dispstrxlib(x, y, rot, s, just, fudge)
    int x, y, rot, just, fudge;
    char *s;
{
    flush_pending();
    x1 = y1 = 99999;
    puthersh(x, y, xlibcharsize * charsize, rot, just, xlibcolor, drawxlib, s);
    flush_pending();
    x1 = y1 = 99999;
}

#define MAXPATTERNS 16

static int patno = 0;

static Pixmap tiles[30];
static Pixmap curtile;

void xlibinit_tiles()
{
    int i;
    Pixmap ptmp;

    for (i = 0; i < MAXPATTERNS; i++) {
	tiles[i] = XCreatePixmap(disp, xwin, 16, 16, DisplayPlanes(disp, DefaultScreen(disp)));
    }
    for (i = 0; i < MAXPATTERNS; i++) {
	if (tiles[i] == NULL) {
	    printf("bad tile %d\n", i);
	} else {
	    XFillRectangle(disp, tiles[i], gcclr, 0, 0, 16, 16);
	}
    }
    ptmp = XCreateBitmapFromData(disp, xwin, pat0_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[0], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat1_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[1], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat2_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[2], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat3_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[3], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat4_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[4], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat5_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[5], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat6_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[6], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat7_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[7], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat8_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[8], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat9_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[9], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat10_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[10], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat11_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[11], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat12_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[12], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat13_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[13], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat14_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[14], gc, 0, 0, 16, 16, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, pat15_bits, 16, 16);
    XCopyPlane(disp, ptmp, tiles[15], gc, 0, 0, 16, 16, 0, 0, 1);
/*
    ptmp = XCreateBitmapFromData(disp, xwin, bm17_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[16], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm18_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[17], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm19_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[18], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm20_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[19], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm21_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[20], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm22_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[21], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm23_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[22], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm24_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[23], gc, 0, 0, 6, 6, 0, 0, 1);
    ptmp = XCreateBitmapFromData(disp, xwin, bm25_bits, 6, 6);
    XCopyPlane(disp, ptmp, tiles[24], gc, 0, 0, 6, 6, 0, 0, 1);
*/
    curtile = tiles[0];
}

int xlibsetpat(k)
    int k;
{
    patno = k;
    if (k > MAXPATTERNS) {
	k = 1;
    }
    if (patno != 0) {
	curtile = tiles[k - 1];
    }
}

void xlibfill(n, px, py)
    int n;
    int px[], py[];

{
    int i, x, y;
    XPoint *p;

    p = (XPoint *) calloc(n, sizeof(XPoint));
    if (p == NULL) {
	return;
    }
    if (patno == 0) {
	return;
    }
    XSetFillStyle(disp, gc, FillTiled);
    XSetTile(disp, gc, curtile);
    for (i = 0; i < n; i++) {
	p[i].x = px[i];
	p[i].y = win_h - py[i];
    }
    XFillPolygon(disp, displaybuff, gc, p, n, Nonconvex, CoordModeOrigin);
    if (backingstore) {
	XFillPolygon(disp, backpix, gc, p, n, Nonconvex, CoordModeOrigin);
    }
    XSetFillStyle(disp, gc, FillSolid);
    cfree(p);
}

void xlibfillcolor(n, px, py)
    int n;
    int px[], py[];

{
    int i, x, y;
    XPoint *p;

    p = (XPoint *) calloc(n, sizeof(XPoint));
    if (p == NULL) {
	return;
    }
    for (i = 0; i < n; i++) {
	p[i].x = px[i];
	p[i].y = win_h - py[i];
    }
    XFillPolygon(disp, displaybuff, gc, p, n, Nonconvex, CoordModeOrigin);
    if (backingstore) {
	XFillPolygon(disp, backpix, gc, p, n, Nonconvex, CoordModeOrigin);
    }
    cfree(p);
}

void xlibdrawarc(x, y, r)
    int x, y, r;
{
    XDrawArc(disp, displaybuff, gc, x - r, win_h - (y + r), 2 * r, 2 * r, 0, 360 * 64);
    if (backingstore) {
	XDrawArc(disp, backpix, gc, x - r, win_h - (y + r), 2 * r, 2 * r, 0, 360 * 64);
    }
}

void xlibfillarc(x, y, r)
    int x, y, r;
{
    XFillArc(disp, displaybuff, gc, x - r, win_h - (y + r), 2 * r, 2 * r, 0, 360 * 64);
    if (backingstore) {
	XFillArc(disp, backpix, gc, x - r, win_h - (y + r), 2 * r, 2 * r, 0, 360 * 64);
    }
}

void xlibdrawellipse(x, y, xm, ym)
    int x, y, xm, ym;
{
    XDrawArc(disp, displaybuff, gc, x - xm, win_h - (y + ym), 2 * xm, 2 * ym, 0, 360 * 64);
    if (backingstore) {
	XDrawArc(disp, backpix, gc, x - xm, win_h - (y + ym), 2 * xm, 2 * ym, 0, 360 * 64);
    }
}

void xlibfillellipse(x, y, xm, ym)
    int x, y, xm, ym;
{
    XFillArc(disp, displaybuff, gc, x - xm, win_h - (y + ym), 2 * xm, 2 * ym, 0, 360 * 64);
    if (backingstore) {
	XFillArc(disp, backpix, gc, x - xm, win_h - (y + ym), 2 * xm, 2 * ym, 0, 360 * 64);
    }
}

void xlibleavegraphics()
{
    flush_pending();
    x1 = y1 = 99999;
    xlibsetmode(2);
}

int xlibinitgraphics(dmode)
{
    npending = 0;
    x1 = 99999;
    y1 = 99999;
    xlibdmode = dmode;
    xlibsetmode(1);
    devorient = 1;
    devconvx = xconvxlib;
    devconvy = yconvxlib;
    vector = drawxlib;
    devwritestr = dispstrxlib;
    devsetcolor = xlibsetcolor;
    devsetfont = xlibsetfont;
    devsetline = xlibsetlinestyle;
    devsetlinew = xlibsetlinewidth;
    devdrawtic = xlibdrawtic;
    devsetpat = xlibsetpat;
    devdrawarc = xlibdrawarc;
    devfillarc = xlibfillarc;
    devdrawellipse = xlibdrawellipse;
    devfillellipse = xlibfillellipse;
    devfill = xlibfill;
    devfillcolor = xlibfillcolor;
    devleavegraphics = xlibleavegraphics;
    devxticl = 12;
    devyticl = 12;
    devarrowlength = 12;
    devsymsize = 6;
    devcharsize = xlibcharsize;
    (*devsetcolor) (1);
    xlibsetlinestyle(1);
    return 0;
}

/*
 * cursors
 */

#include <X11/cursorfont.h>

static Cursor wait_cursor;
static Cursor line_cursor;
static Cursor find_cursor;
static Cursor move_cursor;
static Cursor text_cursor;
static Cursor kill_cursor;
static int cur_cursor = -1;
void set_cursor();

void set_wait_cursor(w)
#ifdef MOTIF
    Widget w;

#else
    int w;

#endif
{
    XDefineCursor(disp, xwin, wait_cursor);
#ifdef MOTIF
    if (w != NULL) {
	XDefineCursor(disp, xwin, wait_cursor);
    }
    XtFlush();
#endif
}

void unset_wait_cursor(w)
#ifdef MOTIF
    Widget w;

#else
    int w;

#endif
{
    if (cur_cursor == -1) {
	XUndefineCursor(disp, xwin);
#ifdef MOTIF
	if (w != NULL) {
	    XUndefineCursor(disp, xwin);
	}
#endif
    } else {
	set_cursor(cur_cursor);
    }
}

void set_cursor(c)
    int c;
{
    XUndefineCursor(disp, xwin);
    cur_cursor = -1;
    switch (c) {
    case 0:
	XDefineCursor(disp, xwin, line_cursor);
	cur_cursor = 0;
	break;
    case 1:
	XDefineCursor(disp, xwin, find_cursor);
	cur_cursor = 1;
	break;
    case 2:
	XDefineCursor(disp, xwin, text_cursor);
	cur_cursor = 2;
	break;
    case 3:
	XDefineCursor(disp, xwin, kill_cursor);
	cur_cursor = 3;
	break;
    case 4:
	XDefineCursor(disp, xwin, move_cursor);
	cur_cursor = 4;
	break;
    }
}

void init_cursors()
{
    wait_cursor = XCreateFontCursor(disp, XC_watch);
    line_cursor = XCreateFontCursor(disp, XC_crosshair);
    find_cursor = XCreateFontCursor(disp, XC_hand2);
    text_cursor = XCreateFontCursor(disp, XC_xterm);
    kill_cursor = XCreateFontCursor(disp, XC_pirate);
    move_cursor = XCreateFontCursor(disp, XC_fleur);
    cur_cursor = -1;
}
