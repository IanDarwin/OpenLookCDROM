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
static char     sccsid[] = "@(#)tree.c	2.3 91/10/15 Copyright 1991 Sun Microsystems";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cms.h>
#include <xview/xv_xrect.h>
#include <xview/textsw.h>
#include <group.h>
#include <gdd.h>
#include <gcm.h>
#include <math.h>
#include "tree_ui.h"

void	initialize_graphics();
void	get_info_from_props();
void	set_props_from_info();
int	color_index();
void	draw_fractal();
void	draw_line();
void	draw_tree();

typedef struct {
	int	clear;
	int	bark_color;
	int	leaf_color;
	float	stem_thickness;
	float	leaf_size;
	float	growth_left;
	float	growth_right;
	float	dir;
	float	dir_left;
	float	dir_right;
} TREE_INFO;

tree_window_objects	*Tree_window;
tree_props_objects	*Tree_props;
TREE_INFO		Tree_info;
TREE_INFO		Tree_defaults;

Display	*Tree_display;
XID	Tree_xid;
GC	Bark_gc;
GC	Leaf_gc;
GC	Current_gc;
int	X_pos = 0;
int	Y_pos = 0;

Attr_attribute	INSTANCE;

#define	SIND(degrees)	(sin((double)(degrees * (M_PI/180.0))))
#define	COSD(degrees)	(cos((double)(degrees * (M_PI/180.0))))

main(argc, argv)
	int	argc;
	char	**argv;
{
	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	INSTANCE = xv_unique_key();
	
	/*
	 * Initialize user interface components.
	 */
	Tree_window = tree_window_objects_initialize(NULL, NULL);
	Tree_props = tree_props_objects_initialize(NULL, Tree_window->window);
	gcm_initialize_colors(Tree_window->canvas, "", "");

	/*
	 * Save defaults from prop sheet away and set current
	 * information to the defaults.
	 */
	get_info_from_props(&Tree_defaults);
	Tree_info = Tree_defaults;
	initialize_graphics(&Tree_info, Tree_window->window, Tree_window->canvas);
	
	/*
	 * Turn control over to XView.
	 */
	xv_main_loop(Tree_window->window);
	exit(0);
}

/*
 * Notify callback function for `defaults'.
 */
void
factory_defaults(item, event)
	Panel_item	item;
	Event		*event;
{
	set_props_from_info(&Tree_defaults);
}

/*
 * Notify callback function for `draw'.
 */
void
draw(item, event)
	Panel_item	item;
	Event		*event;
{
	draw_tree(&Tree_info);
}

/*
 * Notify callback function for `apply'.
 */
void
apply_properties(item, event)
	Panel_item	item;
	Event		*event;
{
	get_info_from_props(&Tree_info);
	draw_tree(&Tree_info);
}

/*
 * Notify callback function for `properties'.
 */
void
tree_window_properties_notify_callback(item, event)
	Panel_item	item;
	Event		*event;
{
	xv_set(Tree_props->props, XV_SHOW, TRUE, 0);
}
/*
 * Get current settings from props sheet.
 */
void
get_info_from_props(ti)
	TREE_INFO	*ti;
{
	ti->dir = (float)xv_get(Tree_props->dir, PANEL_VALUE);
	ti->dir_left = (float)xv_get(Tree_props->dir_left, PANEL_VALUE);
	ti->dir_right = (float)xv_get(Tree_props->dir_right, PANEL_VALUE);

	ti->growth_left = (float)xv_get(Tree_props->growth_left, PANEL_VALUE);
	ti->growth_right = (float)xv_get(Tree_props->growth_right, PANEL_VALUE);

	ti->bark_color =
		color_index((char *)xv_get(Tree_props->bark_color, PANEL_VALUE));

	ti->leaf_color =
		color_index((char *)xv_get(Tree_props->leaf_color, PANEL_VALUE));

	ti->leaf_size = (float)xv_get(Tree_props->leaf_size, PANEL_VALUE);

	ti->clear = (int)xv_get(Tree_props->clear, PANEL_VALUE);
}

void
set_props_from_info(ti)
	TREE_INFO	*ti;
{
	xv_set(Tree_props->dir, PANEL_VALUE, (int)ti->dir, NULL);
	xv_set(Tree_props->dir_left, PANEL_VALUE, (int)ti->dir_left, NULL);
	xv_set(Tree_props->dir_right, PANEL_VALUE, (int)ti->dir_right, NULL);

	xv_set(Tree_props->growth_left, PANEL_VALUE, (int)ti->growth_left, NULL);
	xv_set(Tree_props->growth_right, PANEL_VALUE, (int)ti->growth_right, NULL);

	/* MOOSE - fix me! */
	xv_set(Tree_props->bark_color, PANEL_VALUE, "Brown", NULL);
	xv_set(Tree_props->leaf_color, PANEL_VALUE, "Forest Green", NULL);

	xv_set(Tree_props->leaf_size, PANEL_VALUE, (int)ti->leaf_size, NULL);
	xv_set(Tree_props->clear, PANEL_VALUE, ti->clear, NULL);
}

void
initialize_graphics(ti, win, canvas)
	TREE_INFO	*ti;
	Xv_opaque	win;
	Xv_opaque	canvas;
{
	Cms		cms;
	XGCValues	bark_values;
	XGCValues	leaf_values;

	cms = xv_get(win, WIN_CMS);

	Tree_display = (Display *)xv_get(win, XV_DISPLAY);
	Tree_xid = (XID)xv_get(canvas_paint_window(canvas), XV_XID);

	ti->stem_thickness = sqrt((float)xv_get(canvas, XV_WIDTH)) - 3.0;

	bark_values.background = xv_get(cms, CMS_BACKGROUND_PIXEL);
	bark_values.foreground = ti->bark_color;
	bark_values.line_width = (int)ti->stem_thickness;
	Bark_gc = XCreateGC(Tree_display, Tree_xid,
		GCForeground|GCBackground, &bark_values);

	leaf_values.background = xv_get(cms, CMS_BACKGROUND_PIXEL);
	leaf_values.foreground = ti->leaf_color;
	Leaf_gc = XCreateGC(Tree_display, Tree_xid,
		GCForeground|GCBackground, &leaf_values);
}

int
color_index(s)
	char	*s;
{
	Cms			cms;
	int			index;
	static unsigned long	*pixel_table;

	if (!pixel_table) {
		cms = xv_get(Tree_window->canvas, WIN_CMS);
		pixel_table = (unsigned long *)xv_get(cms, CMS_INDEX_TABLE);
	}

	if ((index = gcm_color_index(s)) == -1)
		index = gcm_color_index("black");

	return pixel_table[index];
}

void
draw_tree(ti)
	TREE_INFO	*ti;
{
	int		width = (int)xv_get(Tree_window->canvas, XV_WIDTH);
	int		height = (int)xv_get(Tree_window->canvas, XV_HEIGHT);

	Current_gc = Bark_gc;
	ti->stem_thickness = sqrt((double)width) - 3;

	if (ti->clear) {
		XSetForeground(Tree_display, Bark_gc, color_index("Background"));
		XFillRectangle(Tree_display, Tree_xid, Bark_gc,
				0, 0, width, height);
		XSetForeground(Tree_display, Bark_gc, ti->bark_color);
	}

	XSetForeground(Tree_display, Bark_gc, ti->bark_color);
	XSetForeground(Tree_display, Leaf_gc, ti->leaf_color);

	X_pos = width/2;
	Y_pos = 10;

	draw_fractal(Tree_window->canvas, MIN(width, height) * 0.25,
		ti->growth_left / 100.0,
		ti->growth_right / 100.0,
		ti->stem_thickness, (ti->dir - 180) + 90,
		ti->dir_left, ti->dir_right,
		MAX(ti->leaf_size, 1), 0, 0);
}

void
draw_fractal(canvas, initial_stem, gl, gr, stem_width, dir, dl, dr, leaf_size, x, y)
	Xv_opaque	canvas;
	float		initial_stem;
	float		gl, gr;
	float		stem_width;
	float		dir, dl, dr;
	float		leaf_size;
	int		x;
	int		y;
{
	int	old_x;
	int	old_y;

	if (initial_stem < leaf_size)
		return;

	x = X_pos;
	y = Y_pos;

	if (stem_width > 2.0) {
		Current_gc = Bark_gc;
		stem_width *= 0.66;
		XSetLineAttributes(Tree_display, Current_gc,
			(int)(stem_width + 0.5), LineSolid, CapButt, JoinMiter);
	} else if (stem_width < 2.0) {
		stem_width = 2.0;
		Current_gc = Leaf_gc;
	}


	old_x = X_pos;
	old_y = Y_pos;
	X_pos += (int) ((initial_stem * SIND(dir)) + 0.5);
	Y_pos += (int) ((initial_stem * COSD(dir)) + 0.5);

	draw_line(old_x, old_y, X_pos, Y_pos);

	draw_fractal(canvas, initial_stem * gl,
		     gl, gr, stem_width, dir - dl, dl, dr, leaf_size, x, y);
	draw_fractal(canvas, initial_stem * gr,
		     gl, gr, stem_width, dir + dr, dl, dr, leaf_size, x, y);

	X_pos = x;
	Y_pos = y;
}

void
draw_line(x1, y1, x2, y2)
	int	x1, y1;
	int	x2, y2;
{
	int	height = (int)xv_get(Tree_window->canvas, XV_HEIGHT);

	XDrawLine(Tree_display, Tree_xid, Current_gc,
		  x1, height - y1, x2, height - y2);
}
