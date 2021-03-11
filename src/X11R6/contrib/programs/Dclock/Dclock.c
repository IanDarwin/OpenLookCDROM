/*
 * Dclock.c -- a digital clock widget.
 * Copyright (c) 1988 Dan Heller <argv@z-code.com>
 *     Modified  1991 Herve Soulard <soulard@sor.inria.fr>
 *          - support for English and French language
 *          - support for audio on Sparc station
 * 
 * Copyright (c) 1988-1994  Dan Heller
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Except as contained in this notice, the name of the X Consortium shall
 * not be used in advertising or otherwise to promote the sale, use or
 * other dealings in this Software without prior written authorization
 * from the X Consortium.
 */

#include <stdio.h>
#include <sys/time.h>
#include <X11/IntrinsicP.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include "DclockP.h"

#ifdef sun
#include <sys/ioctl.h>
#include <sun/audioio.h>
#endif

static void
    Initialize(), Resize(), Realize(), Destroy(), Redisplay(), GetGC(),
    invert_bitmap(), build_segments(), timeout(), toggle_bell(),
    toggle_reverse_video(), toggle_scroll(), toggle_seconds(),
    toggle_military_time(), toggle_date(), make_number(), set_alarm(),
    show_date(), scroll_time(), toggle_alarm(), outline_digit(),
    toggle_fade(), toggle_tails(), toggle_language();

#ifdef sun
static void play_sound();
#endif

#define BORDER		5
#define CLOCK_WIDTH	256
#define CLOCK_HEIGHT	80
#define DATE_FMT	"%W, %M %d"
#define when		break;case
#define otherwise	break;default


static Boolean SetValues(), show_time();
static Dimension winwidth = CLOCK_WIDTH;
static Dimension winheight = CLOCK_HEIGHT;
static Boolean false = False;
static Boolean true = True;
static int fade_rate = 50;
static double x_ratio, y_ratio;
static Pixmap old_pix[4];
static int old_digs[4];
static struct tm before;
static char *saved_date;
static cur_position;	/* outline current digit for setting alarm */
static struct { int hrs, mins; } Alarm;

static char defaultTranslations[] =
    "<Key>b:		toggle-bell()		\n\
     <Key>j:		toggle-scroll()		\n\
     <Key>f:		toggle-fade()		\n\
     <Key>r:		toggle-reverse-video()	\n\
     <Key>s:		toggle-seconds()	\n\
     <Key>m:		toggle-military-time()	\n\
     <Key>d:		toggle-date()		\n\
     <Key>t:		toggle-tails()		\n\
     <Key>a:		toggle-alarm()		\n\
     <Key>l:		toggle-language()	\n\
     <BtnDown>:		set-alarm()";

static XtActionsRec actionsList[] = {
    { "toggle-bell",		toggle_bell		},
    { "toggle-scroll",		toggle_scroll		},
    { "toggle-fade",		toggle_fade		},
    { "toggle-reverse-video",	toggle_reverse_video	},
    { "toggle-seconds",		toggle_seconds		},
    { "toggle-military-time",	toggle_military_time	},
    { "toggle-date",		toggle_date		},
    { "toggle-tails",		toggle_tails		},
    { "toggle-alarm",		toggle_alarm		},
    { "toggle-language",	toggle_language		},
    { "set-alarm",		set_alarm		},
};

static XtResource resources[] = {
    { XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	XtOffset(Widget,core.width), XtRDimension, (caddr_t)&winwidth },
    { XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	XtOffset(Widget,core.height), XtRDimension, (caddr_t)&winheight },
    { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(DclockWidget,dclock.foreground), XtRString, "Black"},
    { XtNreverseVideo, XtCReverseVideo, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.reverse), XtRBoolean, (caddr_t)&false},
    { XtNtails, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.tails), XtRBoolean, (caddr_t)&false},
    { XtNfade, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.fade), XtRBoolean, (caddr_t)&false},
    { XtNfadeRate, XtCTime, XtRInt, sizeof (int),
	XtOffset(DclockWidget,dclock.fade_rate), XtRInt, (caddr_t)&fade_rate},
    { XtNscroll, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.scroll), XtRBoolean, (caddr_t)&true},
    { XtNdisplayTime, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.display_time), XtRBoolean, (caddr_t)&true},
    { XtNalarm, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.alarm), XtRBoolean, (caddr_t)&false},
    { XtNalarmTime, XtCTime, XtRString, sizeof (char *),
	XtOffset(DclockWidget,dclock.alarm_time), XtRString, "00:00" },
    { XtNbell, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.bell), XtRBoolean, (caddr_t)&false},
    { XtNseconds, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.seconds), XtRBoolean, (caddr_t)&false},
    { XtNmilitaryTime, XtCBoolean, XtRBoolean, sizeof (Boolean),
	XtOffset(DclockWidget,dclock.miltime), XtRBoolean, (caddr_t)&false},
    { XtNdate, XtCString, XtRString, sizeof (String),
	XtOffset(DclockWidget,dclock.date_fmt), XtRString, NULL},
    { XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	XtOffset(DclockWidget,dclock.font), XtRString, "fixed"},
    { XtNlanguage, XtCString, XtRString, sizeof (String),
	XtOffset(DclockWidget,dclock.language), XtRString, "English"},
    { XtNhours_snd, XtCString, XtRString, sizeof (String),
	XtOffset(DclockWidget,dclock.hours_snd), XtRString, "Bell"},
    { XtNhalf_hours_snd, XtCString, XtRString, sizeof (String),
	XtOffset(DclockWidget,dclock.half_hours_snd), XtRString, "Bell"},
    { XtNalarm_snd, XtCString, XtRString, sizeof (String),
	XtOffset(DclockWidget,dclock.alarm_snd), XtRString, "Bell"},
};

DclockClassRec dclockClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"Dclock",
    /* widget_size		*/	sizeof(DclockRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actionsList,
    /* num_actions		*/	XtNumber(actionsList),
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	NULL,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultTranslations,
    /* query_geometry		*/	NULL,
    }
};
WidgetClass dclockWidgetClass = (WidgetClass) &dclockClassRec;


#ifdef NO_USLEEP
#define usleep(x)	{ struct timeval st_delay;			\
			  st_delay.tv_usec = (x); st_delay.tv_sec = 0;	\
			  select(32, NULL, NULL, NULL, &st_delay); }
#endif


/*
 * These stipples give different densities for the
 * different stages of fading.
 */
static unsigned char stpl_1_8th[] =
{
    0x80, 0x80, 0x08, 0x08, 0x80, 0x80, 0x08, 0x08,
    0x80, 0x80, 0x08, 0x08, 0x80, 0x80, 0x08, 0x08,
    0x80, 0x80, 0x08, 0x08, 0x80, 0x80, 0x08, 0x08,
    0x80, 0x80, 0x08, 0x08, 0x80, 0x80, 0x08, 0x08
};

static unsigned char stpl_1_4th[] =
{
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22,
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22,
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22,
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22
};

static unsigned char stpl_3_8ths[] =
{
    0xA2, 0xA2, 0x15, 0x15, 0xA8, 0xA8, 0x45, 0x45,
    0x2A, 0x2A, 0x51, 0x51, 0x8A, 0x8A, 0x54, 0x54,
    0xA2, 0xA2, 0x15, 0x15, 0xA8, 0xA8, 0x45, 0x45,
    0x2A, 0x2A, 0x51, 0x51, 0x8A, 0x8A, 0x54, 0x54
};

static unsigned char stpl_one_half[] =
{
    0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA,
    0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA,
    0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA,
    0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA
};

/*
 * fade_stip[] eventually contains the pixmaps used for each
 * iteration of fading (initialized in Initialize()).
 */
#define FADE_ITER	8
static Pixmap fade_stip[FADE_ITER];

#define MAX_PTS	        6	/* max # of pts per segment polygon */
#define NUM_SEGS	7	/* number of segments in a digit */

/*
 * These constants give the bit positions for the segmask[]
 * digit masks.
 */
#define TOP		0
#define MIDDLE		1
#define BOTTOM		2
#define TOP_LEFT	3
#define BOT_LEFT	4
#define TOP_RIGHT	5
#define BOT_RIGHT	6

#define msk(i)		(1 << (i))

/*
 * segmask[n] contains a bitmask of the segments which
 * should be lit for digit 'n'.
 */
int segmask[10] =
{
/* 0 */ msk(TOP) | msk(TOP_RIGHT) | msk(BOT_RIGHT) | msk(BOTTOM)
	  | msk(BOT_LEFT) | msk(TOP_LEFT),
/* 1 */	msk(TOP_RIGHT) | msk(BOT_RIGHT),
/* 2 */ msk(TOP) | msk(TOP_RIGHT) | msk(MIDDLE) | msk(BOT_LEFT)
	  | msk(BOTTOM),
/* 3 */ msk(TOP) | msk(TOP_RIGHT) | msk(MIDDLE) | msk(BOT_RIGHT)
	  | msk(BOTTOM),
/* 4 */ msk(TOP_LEFT) | msk(MIDDLE) | msk(TOP_RIGHT) | msk(BOT_RIGHT),
/* 5 */ msk(TOP) | msk(TOP_LEFT) | msk(MIDDLE) | msk(BOT_RIGHT)
	  | msk(BOTTOM),
/* 6 */ msk(TOP_LEFT) | msk(BOT_LEFT) | msk(MIDDLE) | msk(BOTTOM)
	  | msk(BOT_RIGHT),
/* 7 */ msk(TOP) | msk(TOP_RIGHT) | msk(BOT_RIGHT),
/* 8 */ msk(TOP_LEFT) | msk(BOT_LEFT) | msk(MIDDLE) | msk(TOP)
	  | msk(BOTTOM) | msk(TOP_RIGHT) | msk(BOT_RIGHT),
/* 9 */ msk(TOP) | msk(TOP_LEFT) | msk(TOP_RIGHT) | msk(MIDDLE)
	  | msk(BOT_RIGHT)
};

/*
 * num_segment_pts[s] indicates the number of vertices on the
 * polygon that describes segment 's'
 */
int num_segment_pts[NUM_SEGS] = { 4, 6, 4, 4, 4, 4, 4 };

typedef XPoint segment_pts[NUM_SEGS][MAX_PTS];

segment_pts tiny_segment_pts;
segment_pts norm_segment_pts;

/* ARGSUSED */
static void
Initialize (request, new)
DclockWidget   request;
DclockWidget   new;
{
    int n;
    Display *dpy = XtDisplay(new);
    Drawable root;

    root = RootWindow(dpy, DefaultScreen(dpy));

    if (new->dclock.reverse)
	new->core.background_pixel = !new->core.background_pixel;

    if (new->dclock.alarm_time) {
	if (sscanf(new->dclock.alarm_time, "%2d:%2d",
		&Alarm.hrs, &Alarm.mins) != 2 || Alarm.hrs >= 24 ||
		Alarm.mins >= 60) {
	    XtWarning("Alarm Time is in incorrect format.");
	    new->dclock.alarm_time = "00:00";
	    Alarm.hrs = Alarm.mins = 0;
	}
	new->dclock.alarm_time =
	    strcpy(XtMalloc(strlen(new->dclock.alarm_time)+1),
				   new->dclock.alarm_time);
    }

    if (new->dclock.fade)
	new->dclock.scroll = False;

    GetGC(new);

    fade_stip[1] = XCreateBitmapFromData(dpy, root, stpl_1_8th, 16, 16);
    fade_stip[2] = XCreateBitmapFromData(dpy, root, stpl_1_4th, 16, 16);
    fade_stip[3] = XCreateBitmapFromData(dpy, root, stpl_3_8ths, 16, 16);
    fade_stip[4] = XCreateBitmapFromData(dpy, root, stpl_one_half, 16, 16);
    invert_bitmap(stpl_1_8th, 16, 16);
    invert_bitmap(stpl_1_4th, 16, 16);
    invert_bitmap(stpl_3_8ths, 16, 16);
    fade_stip[5] = XCreateBitmapFromData(dpy, root, stpl_3_8ths, 16, 16);
    fade_stip[6] = XCreateBitmapFromData(dpy, root, stpl_1_4th, 16, 16);
    fade_stip[7] = XCreateBitmapFromData(dpy, root, stpl_1_8th, 16, 16);

    for (n = 1; n != FADE_ITER; ++n)
	if (!fade_stip[n])
	{
	    fprintf(stderr, "Couldn't create stipple!\n");
	    exit(1);
	}

    if (!new->dclock.date_fmt || !*new->dclock.date_fmt)
	saved_date = DATE_FMT;
    if (new->dclock.date_fmt && !*new->dclock.date_fmt)
	new->dclock.date_fmt = NULL;

    /* Shouldn't be necessary, but play it safe anyway */
    for (n = 0; n < 10; n++)
	new->dclock.digits[n] = new->dclock.tiny_digits[n] = 0;
    new->dclock.colon[0] = new->dclock.colon[1] = 0;

    new->dclock.interval_id = (XtIntervalId)NULL;
}

static void
GetGC(w)
DclockWidget w;
{
    XGCValues  	xgcv;
    XtGCMask	gc_mask =
		    GCGraphicsExposures | GCFont | GCForeground | GCBackground;

    xgcv.font = w->dclock.font->fid;
    xgcv.graphics_exposures = FALSE;

    if (w->dclock.reverse) {
	xgcv.foreground = w->core.background_pixel;
	xgcv.background = w->dclock.foreground;
    } else {
	xgcv.foreground = w->dclock.foreground;
	xgcv.background = w->core.background_pixel;
    }

    w->dclock.foreGC = XtGetGC ((Widget) w, gc_mask, &xgcv);

    /* the background is the reverse of the foreground -- so reverse twice */
    if (w->dclock.reverse) {
	xgcv.foreground = w->dclock.foreground;
	xgcv.background = w->core.background_pixel;
    } else {
	xgcv.foreground = w->core.background_pixel;
	xgcv.background = w->dclock.foreground;
    }
    w->dclock.backGC = XtGetGC ((Widget) w, gc_mask, &xgcv);
}


static void
invert_bitmap(bm, h, w)
unsigned char *bm;
int h, w;
{
    int i, *wp;

    for (wp = (int *) bm, i = (h*w) / (8*sizeof(int)); i != 0; --i, ++wp)
	*wp = ~(*wp);
}

static void
Realize (w, valueMask, attrs)
Widget w;
XtValueMask *valueMask;
XSetWindowAttributes *attrs;
{
    *valueMask |= CWBitGravity;
    attrs->bit_gravity = ForgetGravity;
    XtCreateWindow(w, InputOutput, (Visual *)CopyFromParent, *valueMask, attrs);
    Resize(w);
}

static void
Destroy (w)
DclockWidget w;
{
    int n;

    if (w->dclock.interval_id != (XtIntervalId)NULL)
	XtRemoveTimeOut(w->dclock.interval_id);
    XtReleaseGC (w, w->dclock.foreGC);
    XtReleaseGC (w, w->dclock.backGC);
    for (n = 0; n < 10; n++) {
	XFreePixmap(XtDisplay(w), w->dclock.digits[n]);
	XFreePixmap(XtDisplay(w), w->dclock.tiny_digits[n]);
    }
    if (w->dclock.colon[0])
	XFreePixmap(XtDisplay(w), w->dclock.colon[0]);
    if (w->dclock.colon[1])
	XFreePixmap(XtDisplay(w), w->dclock.colon[1]);
}
 
/* ARGSUSED */
static void
Resize  (w)
DclockWidget    w;
{
    int i, digit_w, digit_h;
    Pixmap pix;
    GC gc = w->dclock.foreGC;

    if (!XtIsRealized(w))
	return;

    winwidth = w->core.width;
    winheight = w->core.height;

    x_ratio = (double)winwidth / CLOCK_WIDTH;
    y_ratio = (double)winheight / CLOCK_HEIGHT;

    if (w->dclock.date_fmt || !w->dclock.display_time)
	/* make win temporarily shorter so digits will fit on top of date */
	winheight -= w->dclock.font->ascent + w->dclock.font->descent;

    digit_w = winwidth/(4 - !w->dclock.seconds) - (int)(x_ratio*BORDER*5);

    if (w->dclock.miltime)
	digit_w -= digit_w/5;
    digit_h = winheight - (int)(y_ratio * BORDER*2);
    w->dclock.digit_w = digit_w;
    w->dclock.digit_h = digit_h;

    if (w->dclock.tails)
    {
	segmask[6] |=  msk(TOP);
	segmask[9] |=  msk(BOTTOM);
    }
    else
    {
	segmask[6] &=  ~msk(TOP);
	segmask[9] &=  ~msk(BOTTOM);
    }

    build_segments(norm_segment_pts, digit_w, digit_h);

    if (w->dclock.seconds)
	build_segments(tiny_segment_pts, digit_w/2, digit_h/2);

    for (i = 0; i < 10; i++) {
	/* Make the big digit */
	if (w->dclock.digits[i])
	    XFreePixmap(XtDisplay(w), w->dclock.digits[i]);
	w->dclock.digits[i] =
	      XCreatePixmap(XtDisplay(w), XtWindow(w), digit_w, digit_h,
			    DefaultDepthOfScreen(XtScreen(w)));
	XFillRectangle(XtDisplay(w), w->dclock.digits[i],
		       w->dclock.backGC, 0, 0, digit_w, digit_h);
	make_number(w, w->dclock.digits[i], gc, i, norm_segment_pts);

	/* make smaller version of this digit for use by "seconds" */
	if (w->dclock.tiny_digits[i])
	    XFreePixmap(XtDisplay(w), w->dclock.tiny_digits[i]);
	if (w->dclock.seconds)
	{
	    w->dclock.tiny_digits[i] =
			XCreatePixmap(XtDisplay(w), XtWindow(w),
					digit_w/2, digit_h/2,
					DefaultDepthOfScreen(XtScreen(w)));
	    XFillRectangle(XtDisplay(w), w->dclock.tiny_digits[i],
			   w->dclock.backGC, 0, 0, digit_w/2, digit_h/2);
	    make_number(w, w->dclock.tiny_digits[i], gc, i, tiny_segment_pts);
	}
	else
	    w->dclock.tiny_digits[i] = NULL;
    }
    /* The colon[0] area is blank */
    if (w->dclock.colon[0])
	XFreePixmap(XtDisplay(w), w->dclock.colon[0]);
    w->dclock.colon[0] =
	XCreatePixmap(XtDisplay(w), XtWindow(w), digit_w, digit_h,
		      DefaultDepthOfScreen(XtScreen(w)));

    XFillRectangle(XtDisplay(w), w->dclock.colon[0],
	w->dclock.backGC, 0, 0, digit_w,digit_h);

    /* colon[1] area has two squares */
    if (w->dclock.colon[1])
	XFreePixmap(XtDisplay(w), w->dclock.colon[1]);
    w->dclock.colon[1] = XCreatePixmap(XtDisplay(w), XtWindow(w),
	(int)(30*x_ratio), digit_h,
	DefaultDepthOfScreen(XtScreen(w)));

    XFillRectangle(XtDisplay(w), w->dclock.colon[1],
    	w->dclock.backGC, 0, 0, (int)(30*x_ratio), digit_h);

    XFillArc(XtDisplay(w), w->dclock.colon[1], gc,
	(int)(15*x_ratio), digit_h/3, digit_w/7, digit_w/7,
	0, 360 * 64);
    XFillArc(XtDisplay(w), w->dclock.colon[1], gc,
	(int)(15*x_ratio), (2*digit_h)/3, digit_w/7, digit_w/7,
	0, 360 * 64);

    /* to optimize scrolling information (see scroll_time()) */
    old_pix[0] = w->dclock.digits[0];
    old_pix[1] = old_pix[2] = old_pix[3] = 0;
    old_digs[0] = old_digs[1] = old_digs[2] = old_digs[3] = 0;

    if (w->dclock.date_fmt || !w->dclock.display_time)
	/* restore size */
	winheight += w->dclock.font->ascent + w->dclock.font->descent;
}


static void
build_segments(seg_pts, w, h)
segment_pts seg_pts;
int w, h;
{
    XPoint *pts;

    pts = seg_pts[TOP];
    pts[0].x = 2;
    pts[0].y = pts[1].y = 0;
    pts[1].x = w-2;
    pts[3].x = 2+6*x_ratio;
    pts[3].y = pts[2].y = 6*y_ratio;
    pts[2].x = w - pts[3].x;

    pts = &(seg_pts[MIDDLE][0]);
    pts[0].x = 2;
    pts[0].y = h/2 - 1;
    pts[1].x = 6*x_ratio;
    pts[1].y = h/2 - 3*y_ratio;
    pts[2].x = w-pts[1].x;
    pts[2].y = pts[1].y;
    pts[3].x = w-2;
    pts[3].y = h/2 - 1;
    pts[4].x = pts[2].x;
    pts[4].y = h/2 + 3*y_ratio;
    pts[5].x = pts[1].x;
    pts[5].y = pts[4].y;

    pts = &(seg_pts[BOTTOM][0]);
    pts[0].x = 2;
    pts[0].y = pts[1].y = h;
    pts[1].x = w-2;
    pts[3].x = 6*x_ratio;
    pts[3].y = pts[2].y = h - 6*y_ratio;
    pts[2].x = w - pts[3].x;

    pts = &(seg_pts[TOP_LEFT][0]);
    pts[0].x = pts[1].x = 0;
    pts[0].y = 2;
    pts[1].y = h/2-2;
    pts[2].x = pts[3].x = 6*x_ratio;
    pts[2].y = h/2 - 5*y_ratio;
    pts[3].y = 8*y_ratio;

    pts = &(seg_pts[BOT_LEFT][0]);
    pts[0].x = pts[1].x = 0;
    pts[0].y = h/2;
    pts[1].y = h;
    pts[2].x = pts[3].x = 6*x_ratio;
    pts[3].y = h/2 + 5*y_ratio;
    pts[2].y = h - 8*y_ratio;

    pts = &(seg_pts[TOP_RIGHT][0]);
    pts[0].x = pts[1].x = w;
    pts[0].y = 2;
    pts[1].y = h/2-2;
    pts[2].x = pts[3].x = w-6*x_ratio;
    pts[2].y = h/2 - 5*y_ratio;
    pts[3].y = 8*y_ratio;

    pts = &(seg_pts[BOT_RIGHT][0]);
    pts[0].x = pts[1].x = w;
    pts[0].y = h/2;
    pts[1].y = h;
    pts[2].x = pts[3].x = w-6*x_ratio;
    pts[3].y = h/2 + 5*y_ratio;
    pts[2].y = h - 8*y_ratio;
}


static void
make_number(dw, pix, gc, n, seg_pts)
DclockWidget dw;
Pixmap pix;
GC gc;
int n;
segment_pts seg_pts;
{
    Display *dpy = XtDisplay(dw);
    int i;

    for (i = 0; i != NUM_SEGS; ++i)
	if (segmask[n] & msk(i))
	    XFillPolygon(dpy, pix, gc, seg_pts[i], num_segment_pts[i],
			     Convex, CoordModeOrigin);
}


static void
make_fade_number(dw, pix, gc, on_msk, turn_on_msk, turn_off_msk, iter)
DclockWidget dw;
Pixmap pix;
GC gc;
int on_msk, turn_on_msk, turn_off_msk;
int iter;
{
    Display *dpy = XtDisplay(dw);
    Pixmap on_stipple = fade_stip[iter],
	   off_stipple = fade_stip[FADE_ITER - iter];
    int i;

    for (i = 0; i != NUM_SEGS; ++i)
    {
	if (on_msk & msk(i))
	{
	    XSetFillStyle(dpy, gc, FillSolid);
	    XFillPolygon(dpy, pix, gc, norm_segment_pts[i],
			 num_segment_pts[i], Convex, CoordModeOrigin);
	}
	else if (turn_on_msk & msk(i))
	{
	    XSetStipple(dpy, gc, on_stipple);
	    XSetFillStyle(dpy, gc, FillOpaqueStippled);
	    XFillPolygon(dpy, pix, gc, norm_segment_pts[i],
			 num_segment_pts[i], Convex, CoordModeOrigin);
	}
	else if (turn_off_msk & msk(i))
	{
	    XSetStipple(dpy, gc, off_stipple);
	    XSetFillStyle(dpy, gc, FillOpaqueStippled);
	    XFillPolygon(dpy, pix, gc, norm_segment_pts[i],
			 num_segment_pts[i], Convex, CoordModeOrigin);
	}
    }

    XSetFillStyle(dpy, gc, FillSolid);
}


/* ARGSUSED */
static void
Redisplay  (w)
DclockWidget    w;
{
    Boolean save_scroll = w->dclock.scroll;
    Boolean save_fade = w->dclock.fade;
    long t;

    if (!XtIsRealized(w))
	return;

    if (w->dclock.interval_id != (XtIntervalId)NULL) {
	XtRemoveTimeOut(w->dclock.interval_id);
	w->dclock.interval_id = (XtIntervalId)NULL;
    }
    XFillRectangle(XtDisplay(w), XtWindow(w), w->dclock.backGC,
	0, 0, w->core.width, w->core.height);
    before.tm_min = before.tm_hour = before.tm_wday = -1;
    old_pix[0] = w->dclock.digits[0];
    old_pix[1] = old_pix[2] = old_pix[3] = 0;
    old_digs[0] = old_digs[1] = old_digs[2] = old_digs[3] = 0;
    w->dclock.scroll = FALSE;
    w->dclock.fade = FALSE;
    (void) show_time(w);
    w->dclock.scroll = save_scroll;
    w->dclock.fade = save_fade;
    if (w->dclock.display_time)
	if (w->dclock.seconds)
	    w->dclock.interval_id = XtAddTimeOut((unsigned long)1000, timeout, (XtPointer)w);
	else {
	    t = time(0);
	    w->dclock.interval_id =
		XtAddTimeOut((unsigned long)(60 - (t % 60)) * 1000, timeout, (XtPointer)w);
	}
}

static Boolean
show_time(w)
DclockWidget w;
{
    char buf[11];
    Boolean alarm_went_off = False;
    long t = time(0);
    register struct tm *l_time = localtime(&t);
    int digit_w = w->dclock.digit_w;
    int digit_h = w->dclock.digit_h;
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    GC gc = w->dclock.foreGC;

    if (w->dclock.display_time == True) {
	if (w->dclock.miltime)
	    (void) sprintf(buf, "%02d%02d", l_time->tm_hour, l_time->tm_min);
	else
	    (void) sprintf(buf, "%02d%02d",
		 (l_time->tm_hour) ? ((l_time->tm_hour <= 12) ?
		      l_time->tm_hour : l_time->tm_hour - 12) : 12,
		  l_time->tm_min);
    } else
	/* display the alarm time */
	(void) sprintf(buf, "%02d%02d", Alarm.hrs, Alarm.mins);

    XCopyArea(dpy,
	w->dclock.colon[!w->dclock.seconds || l_time->tm_sec & 1],
	win, gc, 0, 0, (int)(30*x_ratio), digit_h,
	(int)(!w->dclock.miltime * -digit_w * .75 +
		2*BORDER*x_ratio) + 2 * digit_w,
	(int)(BORDER*y_ratio));

    if (l_time->tm_min != before.tm_min || l_time->tm_hour != before.tm_hour)
	scroll_time(w, buf);

    if (w->dclock.seconds) {
	XCopyArea(dpy, w->dclock.tiny_digits[l_time->tm_sec/10], win, gc,
	    0, 0, digit_w/2, digit_h/2,
	    winwidth - 2*(digit_w/2 + (int)(BORDER*x_ratio)),
	    (int)(BORDER*y_ratio));
	XCopyArea(dpy, w->dclock.tiny_digits[l_time->tm_sec%10], win, gc,
	    0, 0, digit_w/2, digit_h/2,
	    winwidth - digit_w/2 - (int)(BORDER*x_ratio),
	    (int)(BORDER*y_ratio));
    }

    if (w->dclock.date_fmt && before.tm_wday != l_time->tm_wday)
	show_date(w, l_time);

    before = *l_time;

    if (w->dclock.alarm && Alarm.hrs == l_time->tm_hour &&
	Alarm.mins == l_time->tm_min && l_time->tm_sec < 5) {
        if (!strcmp(w->dclock.alarm_snd, "Bell")) {
            XBell(dpy, 50);
            XBell(dpy, 50);
        }
        else {
#ifdef sun
            if (l_time->tm_sec < 1)
                play_sound(w->dclock.alarm_snd, 120);
#else
            XBell(dpy, 50);
            XBell(dpy, 50);
#endif
        }
        toggle_reverse_video(w);
        alarm_went_off = True;
    } else
	/* if alarm didn't go off, check for hour/half-hour bell */
	if (w->dclock.bell && (!w->dclock.seconds || l_time->tm_sec == 0)) {
            if (l_time->tm_min == 0 && l_time->tm_sec < 2) {
                if (!strcmp(w->dclock.hours_snd, "Bell")) {
                    XBell(dpy, 50);
                    XBell(dpy, 50);
                }
                else {
#ifdef sun
                    play_sound(w->dclock.hours_snd, 80);
#else
                    XBell(dpy, 50);
                    XBell(dpy, 50);
#endif
                }
            }

            if (l_time->tm_min == 30 && l_time->tm_sec < 2) {
                if (!strcmp(w->dclock.half_hours_snd, "Bell"))
                    XBell(dpy, 50);
                else {
#ifdef sun
                    play_sound(w->dclock.half_hours_snd, 50);
#else
                    XBell(dpy, 50);
#endif
                }
            }
	}
    return alarm_went_off;
}


static void
scroll_time(w, p)
DclockWidget w;
register char *p;
{
    int chgd[4], J = winheight - BORDER*2*y_ratio + 1;
    register int i, j, incr;
    int digit_w = w->dclock.digit_w;
    int digit_h = w->dclock.digit_h;
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    GC gc = w->dclock.foreGC;
    Pixmap new_pix[4];
    int new_digs[4];

#define x ((int)(!w->dclock.miltime * -digit_w * .75 + ((i+1)*BORDER + \
	(i>1)*30)*x_ratio) + i*digit_w)
#define y (int)(BORDER * y_ratio)

    for (i = 0; i < 4; i++)
    {
	new_digs[i] = *p++ - '0';
	new_pix[i] = w->dclock.digits[new_digs[i]];
    }

    if (w->dclock.scroll)
    {
	for (i = 0; i < 4; i++)    /* if pixmaps don't match, scroll it */
	    chgd[i] = (new_pix[i] != old_pix[i]);

	if (w->dclock.date_fmt || !w->dclock.display_time)
	    J -= w->dclock.font->ascent + w->dclock.font->descent;

	if ((incr = J / 30) < 1)
	    incr = 1;

	for (j = 0; j <= J; j += incr)
	    for (i = 0; i < 4; i++)
		if (chgd[i]) {
		    if (old_pix[i])
			XCopyArea(dpy, old_pix[i], win, gc,
			    0, j, digit_w, digit_h - j, x, y);
		    if (i || new_pix[i] == w->dclock.digits[1]
			  || w->dclock.miltime)
			XCopyArea(dpy, new_pix[i], win, gc,
			    0, 0, digit_w, j, x, y + digit_h - j);
		    else
			XCopyArea(dpy, w->dclock.colon[0], win, gc,
			    0, 0, x+5, y + digit_h - j, digit_w, j);
		}
    }
    else if (w->dclock.fade)
    {
	Pixmap tmp_pix[4];
	int oldmask, newmask;
	int stay_on[4], turn_on[4], turn_off[4];
	unsigned long fade_rate = w->dclock.fade_rate * 1000;

	for (i = 0; i < 4; i++)    /* if pixmaps don't match, fade it */
	    if (chgd[i] = (new_pix[i] != old_pix[i]))
	    {
		tmp_pix[i] = XCreatePixmap(dpy, win, digit_w, digit_h,
				DefaultDepthOfScreen(XtScreen(w)));
		oldmask = segmask[old_digs[i]];
		if (i == 0 && old_digs[i] == 0 && !w->dclock.miltime)
		    oldmask = 0;
		newmask = segmask[new_digs[i]];
		if (i == 0 && new_digs[i] == 0 && !w->dclock.miltime)
		    newmask = 0;
		stay_on[i] = oldmask & newmask;
		turn_on[i] = ~oldmask & newmask;
		turn_off[i] = oldmask & ~newmask;
	    }
	    else
		tmp_pix[i] = NULL;
 
	for (j = 1; j != FADE_ITER; ++j)
	{
	    for (i = 0; i < 4; i++)
		if (chgd[i])
		{
		    XFillRectangle(dpy, tmp_pix[i], w->dclock.backGC,
				   0, 0, digit_w, digit_h);
		    make_fade_number(w, tmp_pix[i], gc, stay_on[i],
				     turn_on[i], turn_off[i], j);
		    XCopyArea(dpy, tmp_pix[i], win, gc, 0, 0,
				digit_w, digit_h, x, y);
		}

	    XFlush(dpy);
	    usleep(fade_rate);
	}

	for (i = 0; i < 4; ++i)
	    if (tmp_pix[i])
		XFreePixmap(dpy, tmp_pix[i]);
    }

    for (i = 0; i < 4; i++) {
	if (i || new_pix[0] == w->dclock.digits[1] || w->dclock.miltime)
	    XCopyArea(dpy, new_pix[i], win, gc, 0,0, digit_w, digit_h, x,y);
	else
	    XCopyArea(dpy, w->dclock.colon[0], win, gc,
		0,0, digit_w,digit_h, x+5,y);
	if (!w->dclock.display_time && i == cur_position)
	    outline_digit(w, cur_position, True);
    }
#undef x
#undef y
    for (i = 0; i < 4; i++)
    {
	old_pix[i] = new_pix[i];
	old_digs[i] = new_digs[i];
    }
}

static char *monthsE[] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
    "Aug", "Sep", "Oct", "Nov", "Dec"
};
static char *MonthsE[] = {
    "January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"
};
static char *daysE[] = {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
};
static char *DaysE[] = {
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday", "Sunday"
};

static char *monthsF[] = {
    "Janv", "Fevr", "Mars", "Avri", "Mai", "Juin", "Juil",
    "Aout", "Sept", "Octo", "Nove", "Dece"
};
static char *MonthsF[] = {
    "Janvier", "Fevrier", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Decembre"
};
static char *daysF[] = {
    "Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam", "Dim"
};
static char *DaysF[] = {
    "Dimanche", "Lundi", "Mardi", "Mercredi",
    "Jeudi", "Vendredi", "Samedi", "Dimanche"
};

static void
show_date(w, now)
DclockWidget w;
struct tm *now;
{
    char datestr[128];
    register char *datep = datestr, *p;
    int x;

    if (!w->dclock.display_time)
	datep += strlen(strcpy(datep, "Push HERE to Set/Unset Alarm"));
    else for (p = w->dclock.date_fmt; p && *p; p++) {
	if (*p != '%')
	    *datep++ = *p;
	else switch (*++p) {
            when 'M':
                if (!strcmp(w->dclock.language, "English"))
                    datep += strlen(strcpy(datep, MonthsE[now->tm_mon]));
                else
                    datep += strlen(strcpy(datep, MonthsF[now->tm_mon]));
            when 'm':
                if (!strcmp(w->dclock.language, "English"))
                    datep += strlen(strcpy(datep, monthsE[now->tm_mon]));
                else
                    datep += strlen(strcpy(datep, monthsF[now->tm_mon]));
            when 'W':
                if (!strcmp(w->dclock.language, "English"))
                    datep += strlen(strcpy(datep, DaysE[now->tm_wday]));
                else
                    datep += strlen(strcpy(datep, DaysF[now->tm_wday]));
            when 'w':
                if (!strcmp(w->dclock.language, "English"))
                    datep += strlen(strcpy(datep, daysE[now->tm_wday]));
                else
                    datep += strlen(strcpy(datep, daysF[now->tm_wday]));
            when 'd':
                if (now->tm_mday >= 10)
                    *datep++ = (now->tm_mday / 10 + '0');
                *datep++ = now->tm_mday % 10 + '0';
            when 'Y':
                *datep++ = '1', *datep++ = '9';
            /* fall thru */
            case 'y':
                *datep++ = now->tm_year / 10 + '0';
                *datep++ = now->tm_year % 10 + '0';
            when '%':
                *datep++ = *p;
            otherwise: ; /* nothing */
        }
    }
    *datep = 0;

    x = (w->core.width - XTextWidth(w->dclock.font, datestr, datep-datestr))/2;
    if (x < 2)
	x = 2;

    /* remove what was there in case the whole thing isn't overwritten */
    XFillRectangle(XtDisplay(w), XtWindow(w), w->dclock.backGC,
	0, winheight - (w->dclock.font->ascent + w->dclock.font->descent),
	winwidth, w->dclock.font->ascent + w->dclock.font->descent);

    XDrawString(XtDisplay(w), XtWindow(w), w->dclock.foreGC,
	x, winheight - BORDER, datestr, (int)(datep - datestr));

    if (w->dclock.alarm)
	XDrawString(XtDisplay(w), XtWindow(w), w->dclock.foreGC,
	    BORDER, winheight - BORDER, "*", 1);
}

static void
timeout(w, id)
DclockWidget w;
XtIntervalId *id;
{
    Boolean alarm_went_off = show_time(w);
    w->dclock.interval_id =
	XtAddTimeOut((unsigned long)((alarm_went_off || w->dclock.seconds)? 1000 : 60000),
			timeout, (XtPointer)w);
}

/* ARGSUSED */
static Boolean
SetValues (current, request, new)
DclockWidget current, request, new;
{
    Boolean do_redraw = False;

    if (current->dclock.display_time != new->dclock.display_time) {
	before.tm_min = before.tm_hour = before.tm_wday = -1;
	new->dclock.miltime = True; /* old state needs to be saved */
	do_redraw = True;
    }
    if (current->dclock.alarm_time != new->dclock.alarm_time) {
	if (sscanf(new->dclock.alarm_time, "%2d:%2d",
		&Alarm.hrs, &Alarm.mins) != 2 || Alarm.hrs >= 24 ||
		Alarm.mins >= 60) {
	    XtWarning("Alarm Time is in incorrect format.");
	    new->dclock.alarm_time = "00:00";
	    Alarm.hrs = Alarm.mins = 0;
	}
	new->dclock.alarm_time =
	    strcpy(XtMalloc(strlen(new->dclock.alarm_time)+1),
				   new->dclock.alarm_time);
	do_redraw = True;
    }

    if (new->dclock.foreground != current->dclock.foreground
    ||  new->core.background_pixel != current->core.background_pixel
    ||  new->dclock.reverse != current->dclock.reverse
    ||  new->dclock.tails != current->dclock.tails
    ||  new->dclock.fade != current->dclock.fade
    ||  new->dclock.miltime != current->dclock.miltime) {
	XtReleaseGC (current, current->dclock.foreGC);
	XtReleaseGC (current, current->dclock.backGC);
	GetGC(new);
	Resize(new); /* pixmaps need to be redrawn */
	do_redraw = True;
    }
    if (new->dclock.seconds != current->dclock.seconds) {
	if (current->dclock.interval_id != (XtIntervalId)NULL) {
	    XtRemoveTimeOut(current->dclock.interval_id);
	    current->dclock.interval_id = (XtIntervalId)NULL;
	}
	Resize(new);
	do_redraw = True;
    }
    if (new->dclock.date_fmt != current->dclock.date_fmt) {
	do_redraw = True;
	before.tm_wday = -1;
    }
    if (new->dclock.alarm != current->dclock.alarm)
	do_redraw = True;

    return do_redraw;
}

static void
toggle_date(w)
DclockWidget w;
{
    char *tmp;

    if (!w->dclock.display_time) {
	XBell(XtDisplay(w), 50);
	return;
    }
    tmp = w->dclock.date_fmt;
    w->dclock.date_fmt = saved_date;
    saved_date = tmp;
    before.tm_wday = -1;
    Resize(w);
    Redisplay(w);
}

static void
toggle_bell(w)
DclockWidget w;
{
    if (w->dclock.bell = !w->dclock.bell)
	XBell(XtDisplay(w), 50);
}

static void
toggle_scroll(w)
DclockWidget w;
{
    w->dclock.scroll = !w->dclock.scroll;
}

static void
toggle_reverse_video(w)
DclockWidget w;
{
    Arg arg;

    XtSetArg(arg, XtNreverseVideo, !w->dclock.reverse);
    XtSetValues(w, &arg, 1);
}

static void
toggle_military_time(w)
DclockWidget w;
{
    Arg arg;

    if (!w->dclock.display_time) {
	XBell(XtDisplay(w), 50);
	return;
    }
    XtSetArg(arg, XtNmilitaryTime, !w->dclock.miltime);
    XtSetValues(w, &arg, 1);
}

static void
toggle_seconds(w)
DclockWidget w;
{
    Arg arg;

    if (!w->dclock.display_time) {
	XBell(XtDisplay(w), 50);
	return;
    }
    XtSetArg(arg, XtNseconds, !w->dclock.seconds);
    XtSetValues(w, &arg, 1);
}

static void
toggle_fade(w)
DclockWidget w;
{
    Arg arg;

    XtSetArg(arg, XtNfade, !w->dclock.fade);
    XtSetValues(w, &arg, 1);
    if (w->dclock.fade && w->dclock.scroll)
	toggle_scroll(w);
}

static void
toggle_tails(w)
DclockWidget w;
{
    Arg arg;

    XtSetArg(arg, XtNtails, !w->dclock.tails);
    XtSetValues(w, &arg, 1);
}

static void
toggle_alarm(w)
DclockWidget w;
{
    Arg arg;

    XtSetArg(arg, XtNalarm, !w->dclock.alarm);
    XtSetValues(w, &arg, 1);
}

static void
toggle_language(w)
DclockWidget w;
{
    Arg arg;

    if (!strcmp(w->dclock.language, "English"))
        XtSetArg(arg, XtNlanguage, "French");
    else
        XtSetArg(arg, XtNlanguage, "English");
    XtSetValues(w, &arg, 1);
    Redisplay(w);
}

static void
set_alarm(w, event)
DclockWidget w;
XButtonEvent *event;
{
    static saved_secs, saved_miltime;

    if (event->button == 3) {
	if (!(w->dclock.display_time = !w->dclock.display_time)) {
	    XtRemoveTimeOut(w->dclock.interval_id);
	    saved_secs = w->dclock.seconds, w->dclock.seconds = False;
	    saved_miltime = w->dclock.miltime, w->dclock.miltime = True;
	    w->dclock.interval_id = (XtIntervalId)NULL;
	} else {
	    w->dclock.seconds = saved_secs;
	    w->dclock.miltime = saved_miltime;
	}
	Resize(w);
	Redisplay(w);
    } else if (!w->dclock.display_time &&
	    (event->button == 1 || event->button == 2)) {
	/* get the digit under the position (1-4) the mouse is over
	 * and increment (possibly wrap around) to next digit.
	 */
	int i, x, y = (int)((BORDER/2)*y_ratio);
	/* first check to see if user toggles the alarm */
	if (event->y >=
		winheight - (w->dclock.font->ascent + w->dclock.font->descent))
	    toggle_alarm(w);
	else for (i = 0; i < 4; i++) {
	    x = (int)(((i+1)*BORDER + (i>1)*30)*x_ratio) + i*w->dclock.digit_w;
	    x -= (int)(BORDER * x_ratio / 2);
	    if (event->x < x + w->dclock.digit_w) {
		if (cur_position == i) {
		    int digit = w->dclock.alarm_time[i>1?i+1:i] - '0';
		    int mod;
		    switch (i) {
			when 0:
			    if (Alarm.hrs > 13 && digit == 1)
				digit++;
			    mod = 3;
			    before.tm_hour = -1;
			when 1 :
			    mod = (Alarm.hrs < 20)? 10 : 4;
			    before.tm_hour = -1;
			when 2:
			    mod = 6;
			    before.tm_min = -1;
			when 3 :
			    mod = 10;
			    before.tm_min = -1;
		    }
		    if (event->button == 1)
			digit = (digit + 1) % mod;
		    else if (--digit < 0)
			digit = mod-1;
		    w->dclock.alarm_time[i>1?i+1:i] = digit + '0';
		    (void) sscanf(w->dclock.alarm_time, "%2d:%2d",
			    &Alarm.hrs, &Alarm.mins);
		    (void) show_time(w);
		} else {
		    outline_digit(w, cur_position, False);
		    outline_digit(w, cur_position = i, True);
		}
		break;
	    }
	}
    } else
	XBell(XtDisplay(w), 50);
}

static void
outline_digit(w, i, draw_it)
DclockWidget w;
int i;
Boolean draw_it;
{
    int x, y;

    x = (i * (BORDER*x_ratio + w->dclock.digit_w))
	+ ((i > 1) ? 30*x_ratio : 0)
	+ 0.5 * BORDER * x_ratio;
    y = 0.5 * BORDER * y_ratio;

    XDrawRectangle(XtDisplay(w), XtWindow(w),
	draw_it? w->dclock.foreGC : w->dclock.backGC,
	x, y,
	w->dclock.digit_w + (int)(BORDER*x_ratio),
	w->dclock.digit_h + (int)(BORDER*y_ratio));
}

#ifdef sun
static void
play_sound(name, volume)
char *name;
int volume;
{
    audio_info_t soundPar;
    audio_info_t oldSoundPar;
    int sAudio;
    int fd;
    unsigned char buffer[BUFSIZ];
    int cc;

    if ((sAudio = open ("/dev/audio", O_WRONLY)) != -1) {
        if ((fd = open (name, O_RDONLY)) != -1) {
            ioctl(sAudio, AUDIO_GETINFO, &oldSoundPar);
            AUDIO_INITINFO(&soundPar);
            soundPar.play.port = AUDIO_SPEAKER;
            soundPar.play.gain = volume;
            ioctl(sAudio, AUDIO_SETINFO, &soundPar);
            while ((cc = read(fd, buffer, sizeof (buffer))) > 0)
                write(sAudio, buffer, cc);
            close(fd);
            ioctl(sAudio, AUDIO_SETINFO, &oldSoundPar);
            close(sAudio);
        }
    }
}
#endif
