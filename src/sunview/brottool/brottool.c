/*
** 	Overview:	Plot stuff in window...
*/

#include <stdio.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include "brottool.h"

/*
static void resize_proc();
static void repaint_proc();
*/
static Notify_value my_notice_destroy();
static short bits[16] = { 0x8000, 0x4000, 0x2000, 0x1000, 0x800,
			  0x400, 0x200, 0x100, 0x80, 0x40, 0x20,
			  0x10, 0x8, 0x4, 0x2, 0x1 };

static short icon_image[] = {
#include "brottool.icon"
};
DEFINE_ICON_FROM_IMAGE(brot_icon, icon_image);


extern Notify_error notify_dispatch();
void do_batch();

/* twice as wide as tall */
static int Xmax = MAX_X_OFF;
static int Ymax = MAX_Y_OFF;
static int Stop;		/* Are we stopped? */
static int my_done;		/* set in my_notice_destroy() */
static int linebytes;		/* to get Y index into display array */
static int a, b, c;		/* seed values */
static int factor;		/* scaling factor */
static int count;		/* number of STEP points plotted */
static int xoff, yoff;		/* current X and Y offsets */
static int sx, sy;		/* current values of X and Y */
static char *image;		/* pointer to display array (NOT screen) */
static Pixwin *pw;		/* screen pixwin */
static Pixrect *Mypr;		/* display array pixrect */

/* pointers to panel items */
static Panel_item panel_item_A, panel_item_B, panel_item_C;
static Panel_item panel_item_X, panel_item_Y, panel_stop_toggle;

/* All the notify procs for the panel items call this one procedure.
** It merely changes the values and restarts the display (unless the
** item is the "Stop" button, in which case the display isn't restarted).
*/

change_proc(item, value, event)
Panel_item item;
int value;
Event *event;
{
    if (item == panel_item_A)
	a = value;
    else if (item == panel_item_B)
	b = value;
    else if (item == panel_item_C)
	c = value << SHIFT;
    else if (item == panel_item_X)
	xoff = value;
    else if (item == panel_item_Y)
	yoff = value;
    else if (item == panel_stop_toggle) {
	Stop = value;
	return;
    } else
	factor = SHIFT - value;
    /* could just use bzero() on "image", but Sun OS 3.2 bzero()
    ** is broken for > 256K (64K * 4 bytes/long)
    */
    pr_rop(Mypr, 0, 0, Xmax, Ymax, PIX_CLR | PIX_DONTCLIP, 0, 0, 0);
    sx = 0; sy = 0; count = 0;		/* start over */
}

main(argc, argv)
int argc;
char **argv;
{
    int Xsize;			/* panel items scaled to frame size */
    Frame frame;		/* the tool */
    Panel panel;		/* the control panel */
    Canvas canvas;		/* the canvas for drawing */
    Panel_item panel_iters;	/* for number of STEP iterations */
    char panel_iter_string[30];	/* ditto */

    frame = window_create(NULL, FRAME,
			  FRAME_LABEL, "BrotTool",
			  FRAME_ARGC_PTR_ARGV, &argc, argv,
			  FRAME_ICON, &brot_icon,
			  FRAME_SUBWINDOWS_ADJUSTABLE, FALSE,
			  WIN_ERROR_MSG, "Can't create frame",
			  0);

    /* get width of the frame to scale panel items by */
    Xsize = ((Rect *) window_get(frame, WIN_RECT))->r_width;

    panel = window_create(frame, PANEL, 
		      PANEL_LABEL_BOLD, TRUE,
		      0);

    panel_item_A = panel_create_item(panel, PANEL_SLIDER,
		      PANEL_VALUE, DEF_A,
		      PANEL_MIN_VALUE, (-4 << SHIFT),
		      PANEL_MAX_VALUE, (4 << SHIFT),
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_LABEL_STRING, "Seed 'A': ",
		      PANEL_NOTIFY_LEVEL, PANEL_DONE,
		      PANEL_SHOW_VALUE, TRUE,
		      PANEL_SLIDER_WIDTH, Xsize-200,
		      PANEL_SHOW_RANGE, FALSE,
		      0);
    panel_item_B = panel_create_item(panel, PANEL_SLIDER,
		      PANEL_VALUE, DEF_B,
		      PANEL_MIN_VALUE, (-4 << SHIFT),
		      PANEL_MAX_VALUE, (4 << SHIFT),
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_LABEL_STRING, "Seed 'B': ",
		      PANEL_NOTIFY_LEVEL, PANEL_DONE,
		      PANEL_SHOW_VALUE, TRUE,
		      PANEL_SLIDER_WIDTH, Xsize-200,
		      PANEL_SHOW_RANGE, FALSE,
		      0);
    panel_item_C = panel_create_item(panel, PANEL_SLIDER,
		      PANEL_VALUE, DEF_C >> SHIFT,
		      PANEL_MIN_VALUE, (-4 << SHIFT),
		      PANEL_MAX_VALUE, (4 << SHIFT),
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_LABEL_STRING, "Seed 'C': ",
		      PANEL_NOTIFY_LEVEL, PANEL_DONE,
		      PANEL_SHOW_VALUE, TRUE,
		      PANEL_SLIDER_WIDTH, Xsize-200,
		      PANEL_SHOW_RANGE, FALSE,
		      0);

    panel_item_X = panel_create_item(panel, PANEL_SLIDER,
		      PANEL_VALUE, DEF_X_OFF,
		      PANEL_MIN_VALUE, 0,
		      PANEL_MAX_VALUE, MAX_X_OFF,
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_LABEL_STRING, "X Offset: ",
		      PANEL_NOTIFY_LEVEL, PANEL_DONE,
		      PANEL_SHOW_VALUE, TRUE,
		      PANEL_SLIDER_WIDTH, Xsize-370,
		      PANEL_SHOW_RANGE, FALSE,
		      0);

    /* This is a little strange because there are no PANEL_CHOICE_STRINGS.
    ** Instead, we use the PANEL_LABEL_STRING to label our button.  This
    ** is easier than loading a pixfont to get the string to come up bold
    ** like the rest of the panel items.
    */
    panel_stop_toggle = panel_create_item(panel, PANEL_TOGGLE,
		      PANEL_LABEL_STRING, "Stop",
		      PANEL_CHOICE_STRINGS, "", 0,
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_TOGGLE_VALUE, 0, FALSE,
		      0);
    panel_create_item(panel, PANEL_CYCLE,
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_LABEL_STRING, " Scale:",
		      PANEL_CHOICE_STRINGS, "1", "2", "4", "8", "16",
			  "32", "64", "128", 0,
		      PANEL_VALUE, DEF_FAC,
		      0);

    panel_item_Y = panel_create_item(panel, PANEL_SLIDER,
		      PANEL_VALUE, DEF_Y_OFF,
		      PANEL_MIN_VALUE, 0,
		      PANEL_MAX_VALUE, MAX_Y_OFF,
		      PANEL_NOTIFY_PROC, change_proc,
		      PANEL_LABEL_STRING, "Y Offset: ",
		      PANEL_NOTIFY_LEVEL, PANEL_DONE,
		      PANEL_SHOW_VALUE, TRUE,
		      PANEL_SLIDER_WIDTH, Xsize-370,
		      PANEL_SHOW_RANGE, FALSE,
		      0);

    panel_create_item(panel, PANEL_MESSAGE,
		      PANEL_LABEL_STRING, "Points:",
		      0);
    panel_iters = panel_create_item(panel, PANEL_MESSAGE,
		      PANEL_LABEL_STRING, "    0",
		      0);
    (void) sprintf(panel_iter_string, "x %d", STEP);
    panel_create_item(panel, PANEL_MESSAGE,
		      PANEL_LABEL_STRING, panel_iter_string,
		      0);

    window_fit_height(panel);	/* squeeze everything together */

    canvas = window_create(frame, CANVAS,
			   /* CANVAS_RESIZE_PROC, resize_proc, */
			   /* CANVAS_REPAINT_PROC, repaint_proc, */
			   CANVAS_RETAINED, FALSE,
			   CANVAS_AUTO_CLEAR, FALSE,
			   CANVAS_WIDTH, Xmax,
			   CANVAS_HEIGHT, Ymax,
			   CANVAS_AUTO_SHRINK, FALSE,
			   WIN_VERTICAL_SCROLLBAR, scrollbar_create(0),
			   WIN_HORIZONTAL_SCROLLBAR, scrollbar_create(0),
			   WIN_ERROR_MSG, "Can't create canvas",
			   0);
    
    pw = canvas_pixwin(canvas);

    (void) notify_interpose_destroy_func(frame, my_notice_destroy);

    /* initialize defaults */
    a = DEF_A; b = DEF_B; c = DEF_C; factor = SHIFT - DEF_FAC;
    xoff = DEF_X_OFF; yoff = DEF_Y_OFF; Stop = FALSE;

    /* allocate the memory pixrect for the actual display */
    if ((Mypr = mem_create(Xmax, Ymax, 1)) == NULL) {
	fprintf(stderr, "%s: not enough memory for display array", argv[0]);
	exit(1);
    }

    image = (char *) ((struct mpr_data *) Mypr->pr_data)->md_image;
    linebytes = ((struct mpr_data *) Mypr->pr_data)->md_linebytes;
    window_set(frame, WIN_SHOW, TRUE, 0);

    while (TRUE) {
	(void) notify_dispatch();	/* any windowing to do? */
	if (my_done)
	    break;

	/* Might be easier to call window_main_loop()? */
	if (Stop) {		/* don't draw, sleep for 2/10 sec */
	    usleep(200000);
	    continue;
	}
	do_batch(&sx, &sy);
	(void) sprintf(panel_iter_string, "%5d", ++count);
	panel_set(panel_iters,
		  PANEL_LABEL_STRING, panel_iter_string,
		  0);
    }
}

/* NOT NECESSARY...
static void
resize_proc()
{
}
*/

/* Since do_batch() periodically updates the screen anyway,
** we don't need this at all...
static void
repaint_proc()
{
    pw_write(pw, 0, 0, Xmax, Ymax, PIX_SRC, Mypr, 0, 0);
}
*/

static Notify_value
my_notice_destroy(frame, status)
Frame frame;
Destroy_status status;
{
    if (status != DESTROY_CHECKING) {
	my_done = 1;
	(void) notify_stop();
    }
    return(notify_next_destroy_func(frame, status));
}

/* The basic equation is:
**
**	tmp = y - SIGN(x) * sqrt(fabs(b*x-c));
**	y = a - x;
**	x = tmp;
*/

void
do_batch()
{
    register int tmp, ix, iy, ifactor, i, ibytes;
    register char *ia, *ib, *ic;
    register short *ttmp;

    ia = (char *) a; ib = (char *) b; ic = (char *) c;
    ix = sx; iy = sy;
    ibytes = linebytes;
    ifactor = factor;

    for (i=STEP; i--; ) {
	tmp = iy; tmp >>= ifactor; tmp += yoff;		/* y */
	if (tmp < Ymax && tmp >= 0) {
	    tmp *= ibytes;
	    ttmp = (short *) (image + tmp);
	    tmp = ix; tmp >>= ifactor; tmp += xoff;	/* x */
	    if (tmp < Xmax && tmp >= 0)
		*(ttmp + (tmp >> 4)) |= bits[tmp & 0x0f];
	}
	tmp = (int) ib;
	tmp *= ix;
	tmp -= (int) ic;
	if (tmp < 0)
	    tmp = -tmp;
	tmp = f_sqrt(tmp);
	if (ix < 0)
	    tmp = -tmp;
	tmp = iy - tmp;
	iy = (int) ia - ix;
	ix = tmp;
    }

    /* Maybe this would be faster if we kept track of the canvases
    ** current viewport and only did a pw_write of that area.  For
    ** now, this appears to be fast enough.
    */
    pw_write(pw, 0, 0, Xmax, Ymax, PIX_SRC, Mypr, 0, 0);

    sx = ix;	/* save the x value for the next call */
    sy = iy;	/* save the y value for the next call */
}
