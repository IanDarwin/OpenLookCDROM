/* $Id: monwin.c,v 1.2 91/10/13 08:02:18 pturner Exp Locker: pturner $
 *
 * monitor Panel
 *
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/sel_attrs.h>
#include <xview/textsw.h>

#include "globals.h"

extern Frame main_frame;

Frame mon_frame = (Frame) 0;
Panel mon_panel;
Panel mon_textsw;

/*
 * Panel item declarations
 */

/*
 * Event and Notify proc declarations
 */
static int mon_Done_notify_proc();

/*
 * Create the files Frame and the files Panel
 */
void create_monitor_frame()
{
    FILE *p;
    int i = 0;
    char buf[256];

    if (mon_frame) {
	xv_set(mon_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    mon_frame = (Frame) xv_create(main_frame, FRAME,
				  FRAME_LABEL, "Monitor",
				  XV_WIDTH, 500,
				  XV_HEIGHT, 300,
				  NULL);
    mon_panel = (Panel) xv_create(mon_frame, PANEL,
/*
				    PANEL_LAYOUT, PANEL_VERTICAL,
*/
				  NULL);

    (void) xv_create(mon_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, mon_Done_notify_proc,
		     NULL);
    window_fit_height(mon_panel);
    mon_textsw = (Textsw) xv_create(mon_frame, TEXTSW,
				    XV_WIDTH, 500,
				    XV_HEIGHT, 300,
				    NULL);
    window_fit(mon_frame);
    xv_set(mon_frame, WIN_SHOW, TRUE, 0);
}				/* end create_mon_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int mon_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(mon_frame, WIN_SHOW, FALSE, 0);
    return XV_OK;
}

void stufftext(s)
    char *s;
{
    extern int inwin;

    if (inwin) {
	create_monitor_frame();
	textsw_insert(mon_textsw, s, strlen(s));
    } else {
	printf(s);
    }
}
