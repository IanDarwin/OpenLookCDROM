/*
 * painter.c -- a short example using the XView/NeWS library
 * PostScript Code and ideas taken from Lavallee's painter script.
 * TODO - save a point list, and redraw it in the event proc.
 */

#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xvps/pscanvas.h>
#include "painter.h"

void
pscanvas_repaint_proc(pscanvas, newstoken, dpy, xid, xrects)
PScanvas	pscanvas;
NeWStoken	newstoken;
Display	*dpy;
Window	xid;
Xv_xrectlist *xrects;
{
	xv_set(pscanvas, PSCANVAS_CLIPRECTS, xrects, NULL);
	/* pscanvas_flip(); */
	painter_repaint();
}

main(argc, argv)
int  argc;
char **argv;
{
	Frame frame;
	Menu menu;
	Panel panel;
	void menu_proc();
	int menu_hit();
	Canvas pcan;

	xv_init(XV_INIT_ARGS, argc, argv, 0);

	/* Make a baseframe */
	frame=xv_create(NULL, FRAME,
		FRAME_LABEL, "Toy XVPS Paint Program (use snapshot to save)",
		NULL);

	/* Panel is just to hold buttons */
	panel = (Panel) xv_create(frame, PANEL,
		XV_HEIGHT, 80,
		NULL);

	menu = (Menu) xv_create(NULL, MENU,
		MENU_NOTIFY_PROC, menu_proc,
		MENU_STRINGS,		"Clear", "Save...", NULL,
		NULL);
	(void) xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Edit",
		PANEL_NOTIFY_PROC,	menu_hit,
		PANEL_MENU_ITEM,	menu,
		NULL);

	/* And finally a PSCANVAS to scribble on */
	pcan = (Canvas) xv_create(frame, PSCANVAS,
		PSCANVAS_REPAINT_PROC, pscanvas_repaint_proc,
		PSCANVAS_SYNC, 5,
		NULL);

	/* Now call some procs that send stuff */
	DEFINE_paintit();
	DEFINE_setxy();

	/* Now call a CPS proc that loads our main program */
	painter_main();

	window_fit(pcan);
	window_fit(frame);

	/* And finally the canonical event loop... */
	xv_main_loop(frame);
}

int
menu_hit()
{
	return XV_OK;
}

void
menu_proc(menu, item)
Menu menu;
Menu_item item;
{
	fprintf(stderr, "Menu item %s\n", xv_get(item, MENU_STRING));

	/* TODO actions... */
}
