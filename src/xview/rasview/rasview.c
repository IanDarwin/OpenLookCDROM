/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#ifndef lint
static char     sccsid[] = "@(#)rasview.c	2.12 91/10/15 Copyright 1989 Sun Microsystems.";
#endif

#include <stdio.h>
#include <rasterfile.h>
#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/notice.h>
#include <xview/panel.h>
#include <xview/pixwin.h>
#include <xview/scrollbar.h>
#include <xview/textsw.h>
#include <pixrect/pr_io.h>
#include <xview/dragdrop.h>
#include <gdd.h>
#include <gfm.h>
#include "rasview_ui.h"

/*
 * Instance XV_KEY_DATA key.  An instance is a set of related user interface
 * objects.  A pointer to an object's instance is stored under this key in
 * every object.  This must be a global variable.
 */
Attr_attribute	INSTANCE;

static rasview_win_objects	*Rasview;
static gfm_popup_objects	*Gfm;
static Pixrect			*Pr;		/* current pixrect */
int				Size = 0;
char				*Buf = NULL;
char				*File = NULL;
static Xv_window		Canvas_pixwin;	/* canvas' pixwin */
static Scrollbar		Canvas_hscrollbar; /* canvas' horiz scrollbar */
static Scrollbar		Canvas_vscrollbar; /* canvas' vert scrollbar */

void	rasview_initialize();
void	rasview_file_close();
int	rasview_file_load();
int	gfm_proc();
void	unmap_drag_data();

void
main(argc, argv)
	int             argc;
	char          **argv;
{

	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);
	INSTANCE = xv_unique_key();

	/*
	 * Initialize user interface components.
	 */
	rasview_initialize(NULL);

	/*
	 * Initialize the Drag Drop package.
	 */
	gdd_init_dragdrop(Rasview->win);
	
	/*
	 * Turn control over to XView.
	 */
	xv_main_loop(Rasview->win);
	unmap_drag_data();
	exit(0);
}

/*
 * Initialize rasview module.
 */
void
rasview_initialize(owner)
	Xv_opaque       owner;
{
	Rasview = rasview_win_objects_initialize(NULL, owner);
	Gfm = gfm_initialize(NULL, Rasview->win, "Rasview: Load");
	Canvas_pixwin = (Xv_window) xv_get(Rasview->canvas,
					   CANVAS_NTH_PAINT_WINDOW, 0);
	Canvas_hscrollbar = (Scrollbar) xv_get(Rasview->canvas,
					       WIN_HORIZONTAL_SCROLLBAR);
	Canvas_vscrollbar = (Scrollbar) xv_get(Rasview->canvas,
					       WIN_VERTICAL_SCROLLBAR);
	xv_set(Rasview->canvas,
	       CANVAS_AUTO_SHRINK, FALSE,
	       CANVAS_AUTO_EXPAND, TRUE,
	       CANVAS_X_PAINT_WINDOW, FALSE,
	       0);

}

/*
 * Close a raster file.
 */
void
rasview_file_close()
{
	if (!Pr)
		return;

	pw_writebackground(Canvas_pixwin, 0, 0,
			   (int) xv_get(Rasview->canvas, CANVAS_WIDTH, 0),
			   (int) xv_get(Rasview->canvas, CANVAS_HEIGHT, 0),
			   PIX_CLR);
	pr_destroy(Pr);
	Pr = NULL;

	/*
	 * Set the drop target to show that there is no available data to
	 * drag.
	 */
	xv_set(Rasview->drop_target1, PANEL_DROP_FULL, FALSE, 0);

	xv_set(Rasview->win, FRAME_RIGHT_FOOTER, "", 0);
}

/*
 * Load a raster file.
 */
int
rasview_file_load(file)
	char           *file;
{
	FILE           *fp;
	colormap_t      colormap;
	char           *p;
	struct stat	statbuf;

	/*
	 * Open file.
	 */
	rasview_file_close();
	if ((fp = fopen(file, "r")) == NULL ||
	    (Pr = pr_load(fp, &colormap)) == NULL ||
	    Pr->pr_depth != 1) {
		Event           event;
		char            buf[MAXPATHLEN];

		sprintf(buf, "Cannot load %s.", file);
		notice_prompt(Rasview->win, &event,
			      NOTICE_MESSAGE_STRINGS, buf, 0,
			      NOTICE_BUTTON_YES, "Continue",
			      0);
		(void) fclose(fp);
		return GFM_ERROR;
	} else
	{
		unmap_drag_data();
		fstat(fileno(fp), &statbuf);
		Size = statbuf.st_size;
		Buf = mmap(0, Size, PROT_READ, MAP_PRIVATE, fileno(fp), 0);
		(void) fclose(fp);
	}


	/*
	 * Reset canvas width/height and scrollbars.
	 */
	xv_set(Rasview->canvas,
	       CANVAS_WIDTH, Pr->pr_size.x,
	       CANVAS_HEIGHT, Pr->pr_size.y,
	       0);

	xv_set(Canvas_hscrollbar, SCROLLBAR_VIEW_START, 0, 0);
	xv_set(Canvas_vscrollbar, SCROLLBAR_VIEW_START, 0, 0);

	/*
	 * Display the image.
	 */
	pw_rop(Canvas_pixwin, 0, 0, Pr->pr_size.x, Pr->pr_size.y, PIX_SRC, Pr, 0, 0);

	/*
	 * Set the drop target to show that there is available data to
	 * drag.
	 */
	xv_set(Rasview->drop_target1, PANEL_DROP_FULL, TRUE, 0);

	/*
	 * Set the current directory/file name.
	 */
	if (p = strrchr(file, '/'))
		xv_set(Rasview->win, FRAME_RIGHT_FOOTER, p + 1, 0);
	else
		xv_set(Rasview->win, FRAME_RIGHT_FOOTER, file, 0);

	File = (char *) xv_get(Rasview->win, FRAME_RIGHT_FOOTER);

	return GFM_OK;
}

/*
 * Menu handler for `file_menu (Load...)'.
 */
Menu_item
rasview_load_handler(item, op)
	Menu_item       item;
	Menu_generate   op;
{
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		gfm_activate(Gfm, NULL, NULL, NULL, gfm_proc, NULL, GFM_LOAD);
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Callback for file chooser
 */
int
gfm_proc(ip, dir, file)
	gfm_popup_objects	*ip;
	char			*dir;
	char			*file;
{
	char	buf[MAXPATHLEN];

	sprintf(buf, "%s/%s", dir, file);
	return rasview_file_load(buf);
}

/*
 * Menu handler for `file_menu (Close)'.
 */
Menu_item
rasview_close_handler(item, op)
	Menu_item       item;
	Menu_generate   op;
{
	switch (op) {
	case MENU_DISPLAY:
		xv_set(item, MENU_INACTIVE, Pr == NULL, 0);
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		rasview_file_close();
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}


/*
 * Repaint callback function for `canvas'.
 */
/*ARGSUSED*/
void
rasview_canvas_repaint(canvas, paint_window, rects)
	Canvas          canvas;
	Xv_window       paint_window;
	Rectlist       *rects;
{
	Rect           *r;
	Rectnode       *rnode;

	if (!Pr)
		return;

	for (rnode = rects->rl_head; rnode; rnode = rnode->rn_next) {
		r = &(rnode->rn_rect);
		pw_rop(paint_window, r->r_left, r->r_top,
		       r->r_width, r->r_height, PIX_SRC,
		       Pr, r->r_left, r->r_top);
	}
}

/*
 * Drop callback function for `canvas'.
 */
void
canvas_drop_function(item, event, drop_info)
	Xv_opaque	item;
	Event		*event;
	GDD_DROP_INFO	*drop_info;
{
	char	buf[MAXPATHLEN];

	if (drop_info->tmpfile && (strcmp(drop_info->tmpfile, "") != 0 ))
		sprintf(buf,"%s", drop_info->tmpfile);
	else if (drop_info->filename && (strcmp(drop_info->filename, "") != 0 ))
		strcpy(buf, drop_info->filename);
	else
	{
		/* Drop Error */
		return;
	}

	rasview_file_load(buf);
}


/*
 * Drag callback function for `drop_target1'.
 */
void
rasview_drag_function(item, event, drop_info, drag_state)
	Xv_opaque	item;
	Event		*event;
	GDD_DROP_INFO	*drop_info;
	int		drag_state;
{
	switch (drag_state) {
	case GDD_DRAG_STARTED:
		drop_info->data_label = File;
		drop_info->data = Buf;
		drop_info->length = Size;
		break;

	case GDD_DRAG_COMPLETED:
		break;

	}
}

void
unmap_drag_data()
{
	if (Buf && Size > 0)
	{
		munmap(Buf, Size);
		Buf = NULL;
		File = NULL;
		Size = 0;
	}
}
