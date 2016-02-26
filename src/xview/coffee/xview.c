#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/icon.h>
#include "coffee.h"

/*
 * This file contains all the window-system dependant code.
 *
 * To write an alternate window interface (Motif, XVT, mgr, or whatever),
 * you'd have to write a file called (say) xm.c, with the following four
 * functions: WSinit, WSrun, WSiconify, and WSset_icon.
 * Declare the pushbutton callbacks shown below and the timer callback.
 * The routine WSinit() does all the work of creating windows, buttons, etc.,
 * The routine WSrun() manages the window system event loop.
 * The routine WSiconify() is called (whether window system or not!) from
 *	mainline code, to close the window to an icon.
 * The routine WSchange_icon() is called from main to change the icon.
 *
 * Obviously this could be simpler if you wanted to smush the main line
 * code and the window system together into one larger, less portable mess.
 */

/* Global (to this file) window system variables (widgets, baseFrames, etc). */
static Frame frame;
static Panel panel;
static Icon frame_icon;
struct bitmaps {
	Server_image image;
	unsigned short bits[256];
} bitmaps[NSTATES] = {
	{ 0,  {
#include "unkn.icon"
	} },
	{ 0,  {
#include "full.icon"
	} },
	{ 0,  {
#include "half.icon"
	} },
	{ 0,  {
#include "yuch.icon"
	} },
	{ 0,  {
#include "empty.icon"
	} },
	};

/* These are the push-button callbacks */
extern void set_full(), set_half(), set_ugh(), set_empty();
/* And the timer callback */
extern void checker();

/* Tell window system to call us periodically, to check the status. */
static int itime_interval = 10 /*30*/;

static void
start_timer()
{
	static struct itimerval timer;
	extern void checker();

	timer.it_value.tv_sec = itime_interval;
	timer.it_interval.tv_sec = itime_interval;
	notify_set_itimer_func(frame,
		(Notify_func)checker,
		ITIMER_REAL, &timer, NULL);
}

static int WindowSystemRunning = 0;

/*
 * This routine sets up the windows and controls (buttons, etc.)
 */
void
WSinit(int *argc, char **argv)
{
	int i, fd;
	struct info *curr_state;

	printf("Starting X-coffee...\n");

	xv_init(XV_INIT_ARGC_PTR_ARGV, argc, argv, NULL);

	WindowSystemRunning = 1;

	frame = xv_create(NULL, FRAME,
		FRAME_CLOSED, TRUE,
		XV_LABEL, "X-Coffee Status",
		XV_NULL);

	panel = xv_create(frame, PANEL,
		XV_NULL);

	(void) xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Full",
		PANEL_NOTIFY_PROC,	set_full,
		XV_NULL);
	(void) xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Half",
		PANEL_NOTIFY_PROC,	set_half,
		XV_NULL);
	(void) xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Ugh",
		PANEL_NOTIFY_PROC,	set_ugh,
		XV_NULL);
	(void) xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Empty",
		PANEL_NOTIFY_PROC,	set_empty,
		XV_NULL);

	/* Trim the sizes of the panel and frame to the above controls */
	window_fit(panel);
	window_fit(frame);

	/*
	 * Icon stuff: make up a server image for each state, stuff 1 into
	 * the "frame_icon" (will switch them around later), then
	 * connect the icon onto the baseFrame.
	 */
	for (i=0; i<NSTATES; i++)
	    bitmaps[i].image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, bitmaps[i].bits,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);

	frame_icon = xv_create(XV_NULL, ICON,
		XV_NULL);

	xv_set(frame,
		FRAME_ICON, frame_icon,
		XV_NULL);

	checker();		/* set it to right icon initially */

	start_timer();
}

/*
 * This routine drives the event loop.
 */
void
WSrun()
{
	xv_main_loop(frame);

}


/* Called from mainline to set the icon */
void
WSset_icon(enum coff_stat status, char *who, long when)
{
	struct tm *t;
	char wwbuf[128];			/* who, when */

	t = localtime(&when);
	sprintf(wwbuf, "%02d:%02d %s", t->tm_hour, t->tm_min, who);

	xv_set(frame_icon,
		ICON_IMAGE, bitmaps[status].image,
		ICON_LABEL, wwbuf,
		XV_NULL);
}

/* Called from mainline to close the window to an icon */
void
WSiconify()
{
	struct info *curr_state;

	if (!WindowSystemRunning)
		return;
	
	checker();		/* Ensure the icon is up to date */

	xv_set(frame,
		FRAME_CLOSED,	TRUE,
		XV_NULL);
}

