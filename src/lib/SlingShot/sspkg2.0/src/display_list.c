/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */


#ifndef lint
#ifdef sccs
static  char sccsid[] = "@(#) display_list.c 1.43 92/10/20 ";
#endif
#endif


#include <math.h>
#include <sspkg/drawobj.h>
#include <sspkg/canshell.h>
#include <sspkg/disp_list.h>
#include "rectobj_impl.h"
#include "drawobj_impl.h"

#ifndef __STDC__
#define const	/* */
#endif


/*
 * internal display list routines
 */ 
static void
dl_convert_rrect(out, dl_arg, x, y, w, h)
	Rect           *out;
	Display_list_arg *dl_arg;
	double          x, y, w, h;
{
	short           x0 = dl_convert_rx(dl_arg, x);
	short           y0 = dl_convert_ry(dl_arg, y);
	short           x1 = dl_convert_rx(dl_arg, x + w);
	short           y1 = dl_convert_ry(dl_arg, y + h);

	out->r_top = MIN(y0, y1);
	out->r_left = MIN(x0, x1);
	out->r_width = abs(x1 - x0);
	out->r_height = abs(y1 - y0);
}

/*
 * paint callback data structures
 */

typedef struct {
	Display_list_cmd	cmd;
	short			x1, y1, x2, y2;
} Ds_line_data;

typedef struct {
	Display_list_cmd	cmd;
	double			x1, y1, x2, y2;
} Dd_line_data;


typedef struct {
	Display_list_cmd	cmd;
	double			x, y, width, height;
	int			start, stop;
} Dd_arc_data;

typedef struct {
	Display_list_cmd	cmd;
	short			x, y, width, height;
	int			start, stop;
} Ds_arc_data;


typedef struct {
	Display_list_cmd	cmd;
	short			x, y, width, height;
} Ds_rect_data;

typedef struct {
	Display_list_cmd	cmd;
	double			x, y, width, height;
} Dd_rect_data;


typedef struct {
	Display_list_cmd	cmd;
	short			x, y;
	int			length;
	char			chars[1];	/* embedded */
} Ds_string_data;

typedef struct {
	Display_list_cmd	cmd;
	double			x, y;
	int			length;
	char			chars[1];	/* embedded  */
} Dd_string_data;


typedef struct {
	Display_list_cmd	cmd;
	int			npoints;
	XPoint			points[1];	/* embedded  */
} Ds_poly_data;

typedef struct {
	Display_list_cmd	cmd;
	int             	npoints;
	DPoint          	points[1];	/* embedded  */
} Dd_poly_data;


typedef struct {
	Display_list_cmd	cmd;
	short			left;
	short			top;
	Server_image		image;
	Server_image		mask;
} Ds_image_data;

typedef struct {
	Display_list_cmd	cmd;
	double			left;
	double			top;
	Server_image		image;
	Server_image		mask;
} Dd_image_data;


typedef struct {
	Display_list_cmd	cmd;

	enum {
		Set_LineStyle,
		Set_LineWidth,
		Set_Color,
		Set_BgColor,
		Set_Tile,
		Set_FillStyle,
		Set_Font,
		Set_Stipple,
		Set_Cms,
	} type;

	union {
		int		linestyle;
		int		linewidth;
		short		color;
		int		fillstyle;
		Xv_Font		font;
		Cms		cms;
		Server_image	server_image;
	} data;
} Dl_gc_data;


/*
 * paint callbacks
 */

static void
Ddrawline(dl_arg, data)
	Display_list_arg *dl_arg;
	Dd_line_data   *data;
{
	int             x1 = dl_convert_rx(dl_arg, data->x1);
	int             y1 = dl_convert_ry(dl_arg, data->y1);
	int             x2 = dl_convert_rx(dl_arg, data->x2);
	int             y2 = dl_convert_ry(dl_arg, data->y2);

	XDrawLine(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		  x1, y1,
		  x2, y2);
}

static void
Ddrawrect(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dd_rect_data   *data;
{
	Rect            a;

	dl_convert_rrect(&a, dl_arg, data->x, data->y, data->width, data->height);

	XDrawRectangle(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		       a.r_left, a.r_top,
		       a.r_width, a.r_height);
}


static void
Dfillrect(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dd_rect_data   *data;
{
	Rect            a;

	dl_convert_rrect(&a, dl_arg, data->x, data->y, data->width, data->height);

	XFillRectangle(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
			a.r_left, a.r_top, 
			a.r_width, a.r_height);
}

static int
Dmapfillrect(dl_arg, data, event)
	Display_list_arg *dl_arg;
	Dd_rect_data	*data;
	Event		*event;
{
	Rect            a;

	dl_convert_rrect(&a, dl_arg, data->x, data->y, data->width, data->height);
	if(rect_includespoint(&a, event_x(event), event_y(event)))
		return TRUE;
	return FALSE;
}

static void
Ddrawarc(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dd_arc_data    *data;
{
	Rect            a;

	dl_convert_rrect(&a, dl_arg, data->x, data->y, data->width, data->height);

	XDrawArc(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		 a.r_left, a.r_top, 
		 a.r_width, a.r_height,
		 data->start, data->stop);
}


static void
Dfillarc(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dd_arc_data    *data;
{
	Rect            a;

	dl_convert_rrect(&a, dl_arg, data->x, data->y, data->width, data->height);

	XFillArc(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		 a.r_left, a.r_top,
		 a.r_width, a.r_height,
		 data->start, data->stop);
}

static int
Dmapstring(dl_arg, data, event)
	Display_list_arg *dl_arg;
	Dd_string_data *data;
	Event		*event;
{
	int	x = dl_convert_rx(dl_arg, data->x);
	int	y = dl_convert_ry(dl_arg, data->y) - dl_arg->font_info->ascent;
	int	width = XTextWidth(dl_arg->font_info,data->chars, data->length);
	int	height = dl_arg->font_info->ascent + dl_arg->font_info->descent;

	if(x > event_x(event))
		return FALSE;
	if(y > event_y(event))
		return FALSE;
	if(x+width < event_x(event))
		return FALSE;
	if(y+height < event_y(event))
		return FALSE;
	return TRUE;
}


static void
Ddrawstring(dl_arg, data)
	Display_list_arg *dl_arg;
	Dd_string_data *data;
{
	int             x = dl_convert_rx(dl_arg, data->x);
	int             y = dl_convert_ry(dl_arg, data->y);

	XDrawString(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		    x, y,
		    data->chars,
		    data->length);
}


static void
Ddrawlines(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dd_poly_data   *data;
{
	int             i, j, k;
	static int      size;
	XPoint         *points = (XPoint *) malloc(sizeof(XPoint) * data->npoints);

	/* FIXME: size should depend on current display */
	if (size == 0)
		size = (XMaxRequestSize(dl_arg->dpy) - 3) / 2;

	for (i = 0; i < data->npoints; i++) {
		points[i].x = (short) dl_convert_rx(dl_arg, data->points[i].x);
		points[i].y = (short) dl_convert_ry(dl_arg, data->points[i].y);
	}

	for (j = 0, k = data->npoints; j < data->npoints; j += size, k -= size)
		XDrawLines(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
			   points + j,
			   (k < size) ? k : size,
			   CoordModeOrigin);

	free((char *) points);
}


static void
Dfillpoly(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dd_poly_data   *data;
{
	int             i;
	XPoint         *points = (XPoint *) malloc(sizeof(XPoint) * data->npoints);

	for (i = 0; i < data->npoints; i++) {
		points[i].x = (short) dl_convert_rx(dl_arg, data->points[i].x);
		points[i].y = (short) dl_convert_ry(dl_arg, data->points[i].y);
	}

	XFillPolygon(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		     points, data->npoints,
		     Complex, CoordModeOrigin);

	free((char *) points);
}

/*
 * Following swiped from Sedgewick's "Algorithms".  Warning: there are some
 * documented problems with these algorithms.
 */
/* same_side determines if a two points stratle a line */
static double
same_side(linep1, linep2, point1, point2)
	DPoint linep1, linep2; /* endpoints of the line */
	DPoint point1, point2;
{
	double dx, dy, dx1, dx2, dy1, dy2;

	dx = linep2.x - linep1.x;
	dy = linep2.y - linep1.y;

	dx1 = point1.x - linep1.x;
	dy1 = point1.y - linep1.y;

	dx2 = point2.x - linep2.x;
	dy2 = point2.y - linep2.y;

	return (dx*dy1-dy*dx1)*(dx*dy2-dy*dx2);
}
/* intersect_line determines if two lines intersect */
static int
intersect(line1a, line1b, line2a, line2b)
	DPoint line1a, line1b, line2a, line2b;
{
	return ((same_side(line1a, line1b, line2a, line2b) <= 0) &&
		(same_side(line2a, line2b, line1a, line1b) <= 0));
}


static int
Dmapfillpoly(dl_arg, data, event)
	Display_list_arg *dl_arg;
	Dd_poly_data	*data;
	Event		*event;
{
	DPoint lta, ltb;
	int count = 0;
	int i;


	/* lta and ltb form horizontal test line */
	lta.x = dl_convert_i2rx(dl_arg->drawarea, event_x(event));
	lta.y = dl_convert_i2ry(dl_arg->drawarea, event_y(event));
#ifdef MAXFLOAT
	ltb.x = MAXFLOAT;
#else
	ltb.x = 1.5e+20;
#endif
	ltb.y = lta.y;

	for(i=0; i<data->npoints-1; i++) {
		if(intersect(lta, ltb, data->points[i], data->points[i+1]))
			count ^= 1;
	}
	/* do last one by hand */
	if(intersect(lta, ltb, data->points[i], data->points[0]))
		count ^= 1;

	return count;
}


static void
paint_image(dl_arg, image, mask, left, top)
	Display_list_arg *dl_arg;
	Server_image	image;
	Server_image	mask;
	int		left;
	int		top;
{
	GC		gc;
	Drawable        pixmap;
	int		screen;
 
        pixmap = (Drawable) xv_get(image, SERVER_IMAGE_PIXMAP);

	XSetGraphicsExposures(dl_arg->dpy, dl_arg->gc, False);  
        if(mask) {
		gc = XCreateGC(dl_arg->dpy, dl_arg->win_xid, 0, 0);
		XCopyGC(dl_arg->dpy, dl_arg->gc, -1, gc);
                XSetClipMask(dl_arg->dpy, gc,
                        (Drawable) xv_get(mask, SERVER_IMAGE_PIXMAP));
                XSetClipOrigin(dl_arg->dpy, gc, left, top);
        } else
		gc = dl_arg->gc;

	screen = RECTOBJ_PRIVATE(dl_arg->drawarea)->shared_info->screen_number;
	if( xv_get(dl_arg->canvas_shell, XV_DEPTH) !=
		 xv_get(image, SERVER_IMAGE_DEPTH))

		XCopyPlane(dl_arg->dpy,
			pixmap,
			dl_arg->win_xid,
			dl_arg->gc,
			0, 0,
			(int) xv_get(image, XV_WIDTH),
			(int) xv_get(image, XV_HEIGHT),
			(int) left, (int) top,
			1);
	else
		XCopyArea(dl_arg->dpy,
			pixmap,
			dl_arg->win_xid,
			dl_arg->gc,
			0, 0,
			(int) xv_get(image, XV_WIDTH),
			(int) xv_get(image, XV_HEIGHT),
			(int) left, (int) top);

	if(mask)
                XFreeGC(dl_arg->dpy, gc);
	XSetGraphicsExposures(dl_arg->dpy, dl_arg->gc, True);
}


static void
Ddrawimage(dl_arg, data)
	Display_list_arg *dl_arg;
	Dd_image_data	*data;
{
	paint_image(dl_arg, data->image, data->mask, 
		(int) dl_convert_rx(dl_arg, data->left),
		(int) dl_convert_ry(dl_arg, data->top));
}


static int
Dmapimage(dl_arg, data, event)
	Display_list_arg *dl_arg;
	Dd_image_data	*data;
	Event		*event;
{
	int	x = dl_convert_rx(dl_arg, data->left);
	int	y = dl_convert_ry(dl_arg, data->top);
	int	width;
	int	height;

	if(x > event_x(event) ||
	   y > event_y(event))
		return FALSE;

	width = (int) xv_get(data->image, XV_WIDTH);
	if(x+width < event_x(event))
		return FALSE;

	height = (int) xv_get(data->image, XV_HEIGHT);
	if(y+height < event_x(event))
		return FALSE;

	return TRUE;
}


/*
 * paint callbacks for 0..10000
 */

static void
Sdrawline(dl_arg, data)
	Display_list_arg    *dl_arg;
	Ds_line_data   *data;
{
	int           x1 = dl_convert_x(dl_arg, data->x1);
	int           y1 = dl_convert_y(dl_arg, data->y1);
	int           x2 = dl_convert_x(dl_arg, data->x2);
	int           y2 = dl_convert_y(dl_arg, data->y2);

	XDrawLine(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		  x1, y1,
		  x2, y2);
}


static void
Sdrawrect(dl_arg, data)
	Display_list_arg *dl_arg;
	Ds_rect_data   *data;
{
	int             x = dl_convert_x(dl_arg, data->x);
	int             y = dl_convert_y(dl_arg, data->y);
	unsigned int    width = dl_convert_width(dl_arg, data->width);
	unsigned int    height = dl_convert_height(dl_arg, data->height);

	XDrawRectangle(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		       x, y,
		       width, height);
}


static void
Sfillrect(dl_arg, data)
	Display_list_arg *dl_arg;
	Ds_rect_data   *data;
{
	int             x = dl_convert_x(dl_arg, data->x);
	int             y = dl_convert_y(dl_arg, data->y);
	unsigned int    width = dl_convert_width(dl_arg, data->width);
	unsigned int    height = dl_convert_height(dl_arg, data->height);

	XFillRectangle(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		       x, y,
		       width, height);
}


static void
Sdrawarc(dl_arg, data)
	Display_list_arg *dl_arg;
	Ds_arc_data    *data;
{
	int             x = dl_convert_x(dl_arg, data->x);
	int             y = dl_convert_y(dl_arg, data->y);
	unsigned int    width = dl_convert_width(dl_arg, data->width);
	unsigned int    height = dl_convert_height(dl_arg, data->height);

	XDrawArc(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		 x, y,
		 width, height,
		 data->start, data->stop);
}


static void
Sfillarc(dl_arg, data)
	Display_list_arg *dl_arg;
	Ds_arc_data    *data;
{
	int             x = dl_convert_x(dl_arg, data->x);
	int             y = dl_convert_y(dl_arg, data->y);
	unsigned int    width = dl_convert_width(dl_arg, data->width);
	unsigned int    height = dl_convert_height(dl_arg, data->height);

	XFillArc(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		 x, y,
		 width, height,
		 data->start, data->stop);
}


static void
Sdrawstring(dl_arg, data)
	Display_list_arg *dl_arg;
	Ds_string_data *data;
{
	int             x = dl_convert_x(dl_arg, data->x);
	int             y = dl_convert_y(dl_arg, data->y);

	XDrawString(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		    x, y,
		    data->chars,
		    data->length);
}

static void
Sdrawlines(dl_arg, data)
	Display_list_arg    *dl_arg;
	Ds_poly_data   *data;
{
	int             i, j, k;
	static int      size;

	XPoint         *points = (XPoint *) malloc(sizeof(XPoint) * data->npoints);

	/* FIXME: size should depend on current display */
	if (size == 0)
		size = (XMaxRequestSize(dl_arg->dpy) - 3) / 2;

	for (i = 0; i < data->npoints; i++) {
	  points[i].x = (short) dl_convert_x(dl_arg, (int) data->points[i].x);
	  points[i].y = (short) dl_convert_y(dl_arg, (int) data->points[i].y);
	}

	for (j = 0, k = data->npoints; j < data->npoints; j += size, k -= size)
		XDrawLines(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
			   points + j,
			   (k < size) ? k : size,
			   CoordModeOrigin);
	free((char *) points);
}


static void
Sfillpoly(dl_arg, data)
	Display_list_arg    *dl_arg;
	Ds_poly_data   *data;
{
	int             i;
	XPoint         *points = (XPoint *) malloc(sizeof(XPoint) * data->npoints);

	for (i = 0; i < data->npoints; i++) {
	  points[i].x = (short) dl_convert_x(dl_arg, (int) data->points[i].x);
	  points[i].y = (short) dl_convert_y(dl_arg, (int) data->points[i].y);
	}

	XFillPolygon(dl_arg->dpy, dl_arg->win_xid, dl_arg->gc,
		     points, data->npoints,
		     Complex, CoordModeOrigin);
	free((char *) points);
}


static void
Sdrawimage(dl_arg, data)
	Display_list_arg *dl_arg;
	Ds_image_data	*data;
{
	paint_image(dl_arg, data->image, data->mask, 
		(int) dl_convert_x(dl_arg, (int) data->left),
		(int) dl_convert_y(dl_arg, (int) data->top));
}


static int
map_update_gc(dl_arg, data, event)
	Display_list_arg    *dl_arg;
	Dl_gc_data     *data;
	Event		*event;
{
	extern Xv_Font drawarea_get_font();

	if(data->type == Set_Font) {
		dl_arg->font = drawarea_get_font(
			dl_arg->drawarea, 
			data->data.font);
		dl_arg->font_info = 
			(XFontStruct*) xv_get(dl_arg->font, FONT_INFO);
	}

	return FALSE;
}

static void
update_gc(dl_arg, data)
	Display_list_arg    *dl_arg;
	Dl_gc_data     *data;
{
	XGCValues	gcv;

	switch (data->type) {
	case Set_LineStyle:
		gcv.line_style = data->data.linestyle;
		XChangeGC( dl_arg->dpy, dl_arg->gc,
			(long) GCLineStyle, &gcv);
		break;

	case Set_LineWidth:
		gcv.line_width = data->data.linewidth;
		XChangeGC( dl_arg->dpy, dl_arg->gc,
			(long) GCLineWidth, &gcv);
		break;

	case Set_Color: {
		short color;
		Rectobj_info *rinfo = RECTOBJ_PRIVATE(dl_arg->drawarea);

		if(rinfo->shared_info->cms == dl_arg->cms) {
			color = data->data.color;
			if(color >= rinfo->shared_info->num_colors)
				color = rinfo->fg_color;
			XSetForeground(dl_arg->dpy, dl_arg->gc,
					pixel_fg(rinfo->shared_info, color));
		} else {
			/* not using WIN_CMS */
			XSetForeground(dl_arg->dpy, dl_arg->gc,
				(unsigned long) xv_get(dl_arg->cms, 
					CMS_PIXEL, data->data.color));
		}
		}
		break;

	case Set_BgColor: {
		short color;
		Rectobj_info *rinfo = RECTOBJ_PRIVATE(dl_arg->drawarea);

		if(rinfo->shared_info->cms == dl_arg->cms) {
			color = data->data.color;
			if(color >= rinfo->shared_info->num_colors)
				color = rinfo->bg_color;
			XSetBackground(dl_arg->dpy, dl_arg->gc,
					pixel_bg(rinfo->shared_info, color));
		} else {
			/* not using WIN_CMS */
			XSetBackground(dl_arg->dpy, dl_arg->gc,
				(unsigned long) xv_get(dl_arg->cms, 
					CMS_PIXEL, data->data.color));
		}
		}
		break;

	case Set_FillStyle:
		XSetFillStyle(dl_arg->dpy, dl_arg->gc, 
				data->data.fillstyle);
		break;

	case Set_Font:
		{
		extern Xv_Font drawarea_get_font();

		dl_arg->font = drawarea_get_font(
					dl_arg->drawarea, 
					data->data.font);
		dl_arg->font_info = 
				(XFontStruct*) xv_get(dl_arg->font, FONT_INFO);
		XSetFont(dl_arg->dpy, dl_arg->gc, 
				(XID) xv_get(dl_arg->font, XV_XID));
		}
		break;

	case Set_Stipple:
		XSetStipple(dl_arg->dpy, dl_arg->gc, 
			(XID)	((data->data.server_image) ?
				xv_get(data->data.server_image, XV_XID) : NULL));
		break;

	case Set_Tile:
		XSetTile(dl_arg->dpy, dl_arg->gc, 
			(XID)	((data->data.server_image) ? 
				xv_get(data->data.server_image, XV_XID) : NULL));
		break;

	case Set_Cms:
		dl_arg->cms = data->data.cms;
		break;
	}
}


/*
 * public interface
 */

void
#ifdef __STDC__
VDrawLine(Drawarea drawarea, short x1, short y1, short x2, short y2)
#else
VDrawLine(drawarea, x1, y1, x2, y2)
	Drawarea        drawarea;
	short		x1, y1, x2, y2;
#endif
{
	Ds_line_data   *ptr;
	static const Display_list_vec vec = {
			Sdrawline,
			NULL,
			NULL,
	};

	ptr = (Ds_line_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof (Ds_line_data));
	ptr->x1 = x1;
	ptr->y1 = y1;
	ptr->x2 = x2;
	ptr->y2 = y2;
}


void
#ifdef __STDC__
VDrawRectangle(Drawarea drawarea, short x, short y, short width, short height)
#else
VDrawRectangle(drawarea, x, y, width, height)
	Drawarea        drawarea;
	short		x, y, width, height;
#endif
{
	Ds_rect_data   *ptr;
	static const Display_list_vec vec = {
			Sdrawrect,
			NULL,
			NULL,
	};

	ptr = (Ds_rect_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof (Ds_rect_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
}


void
#ifdef __STDC__
VFillRectangle(Drawarea drawarea, short x, short y, short width, short height)
#else
VFillRectangle(drawarea, x, y, width, height)
	Drawarea        drawarea;
	short           x, y, width, height;
#endif
{
	Ds_rect_data   *ptr;
	static const Display_list_vec vec = {
			Sfillrect,
			NULL,
			NULL,
	};

	ptr = (Ds_rect_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof (Ds_rect_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
}


void
#ifdef __STDC__
VDrawArc(Drawarea drawarea, short x, short y, short width, short height, 
	int start, int stop)
#else
VDrawArc(drawarea, x, y, width, height, start, stop)
	Drawarea        drawarea;
	short		x, y, width, height;
	int		start, stop;
#endif
{
	Ds_arc_data    *ptr;
	static const Display_list_vec vec = {
			Sdrawarc,
			NULL,
			NULL,
	};

	ptr = (Ds_arc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_arc_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
	ptr->start = start;
	ptr->stop = stop;
}


void
#ifdef __STDC__
VFillArc(Drawarea drawarea, short x, short y, short width, short height, 
	int start, int stop)
#else
VFillArc(drawarea, x, y, width, height, start, stop)
	Drawarea        drawarea;
	short           x, y, width, height;
	int		start, stop;
#endif
{
	Ds_arc_data    *ptr;
	static const Display_list_vec vec = {
			Sfillarc,
			NULL,
			NULL,
	};

	ptr = (Ds_arc_data*) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_arc_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
	ptr->start = start;
	ptr->stop = stop;
}

void
#ifdef __STDC__
VDrawString(Drawarea drawarea, short x, short y, char *string, int length)
#else
VDrawString(drawarea, x, y, string, length)
	Drawarea        drawarea;
	short           x, y;
	int             length;
	char           *string;
#endif
{
	Ds_string_data *ptr;
	static const Display_list_vec vec = {
			Sdrawstring,
			NULL,
			NULL,
	};

	ptr = (Ds_string_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_string_data) + length);
	ptr->x = x;
	ptr->y = y;
	ptr->length = length;
	strncpy(ptr->chars, string, length);
}


void
VFillPoly(drawarea, points, npoints)
	Drawarea        drawarea;
	XPoint		*points;
	int             npoints;
{
	Ds_poly_data   *ptr;
	int             i;
	static const Display_list_vec vec = {
			Sfillpoly,
			NULL,
			NULL,
	};

	ptr = (Ds_poly_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_poly_data) + sizeof(XPoint) * (npoints - 1));

	ptr->npoints = npoints;
	memcpy((char*) &ptr->points[0], (char*) points, 
			sizeof(XPoint) * npoints);
}


void
VDrawPoly(drawarea, points, npoints)
	Drawarea        drawarea;
	XPoint		*points;
	int             npoints;
{
	Ds_poly_data   *ptr;
	int             i;
	static const Display_list_vec vec = {
			Sdrawlines,
			NULL,
			NULL,
	};

	ptr = (Ds_poly_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_poly_data) + sizeof(XPoint) * npoints);

	memcpy((char*) &ptr->points[0], (char*) points, 
			sizeof(XPoint) * npoints);

	ptr->points[npoints] = points[0]; /* closes the shape */
	ptr->npoints = npoints+1;
}


void
VDrawLines(drawarea, points, npoints)
	Drawarea	drawarea;
	XPoint		*points;
	int		npoints;
{
	Ds_poly_data   *ptr;
	int             i;
	static const Display_list_vec vec = {
			Sdrawlines,
			NULL,
			NULL,
	};

	ptr = (Ds_poly_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_poly_data) + sizeof(XPoint) * (npoints-1));

	memcpy((char*) &ptr->points[0], (char*) points, 
			sizeof(XPoint) * npoints);

	ptr->npoints = npoints;
}



void
#ifdef __STDC__
VDrawImage(Drawarea drawarea, 
	short left, short top,
	Server_image image, Server_image mask)
#else
VDrawImage(drawarea, left, top, image, mask)
	Drawarea	drawarea;
	Server_image	image, mask;
	short		left, top;
#endif
{
	Ds_image_data   *ptr;
	static const Display_list_vec vec = {
			Sdrawimage,
			NULL,
			NULL,
	};

	ptr = (Ds_image_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Ds_image_data));

	ptr->left = left;
	ptr->top = top;
	ptr->image = image;
	ptr->mask = mask;
}


/*
 * double versions 
 */

void
DDrawLine(drawarea, x1, y1, x2, y2)
	Drawarea        drawarea;
	double          x1, y1, x2, y2;
{
	Dd_line_data   *ptr;
	static const Display_list_vec vec = {
			Ddrawline,
			NULL,
			NULL,
	};

	ptr = (Dd_line_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_line_data));
	ptr->x1 = x1;
	ptr->y1 = y1;
	ptr->x2 = x2;
	ptr->y2 = y2;
}


void
DDrawRectangle(drawarea, x, y, width, height)
	Drawarea        drawarea;
	double          x, y, width, height;
{
	Dd_rect_data   *ptr;
	static const Display_list_vec vec = {
			Ddrawrect,
			NULL,
			NULL,
	};

	ptr = (Dd_rect_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_rect_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
}


void
DFillRectangle(drawarea, x, y, width, height)
	Drawarea        drawarea;
	double          x, y, width, height;
{
	Dd_rect_data   *ptr;
	static const Display_list_vec vec = {
			Dfillrect,
			Dmapfillrect,
			NULL,
	};

	ptr = (Dd_rect_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_rect_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
}


void
DDrawArc(drawarea, x, y, width, height, start, stop)
	Drawarea        drawarea;
	double          x, y, width, height;
	int		start, stop;
{
	Dd_arc_data    *ptr;
	static const Display_list_vec vec = {
			Ddrawarc,
			NULL,
			NULL,
	};

	ptr = (Dd_arc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_arc_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
	ptr->start = start;
	ptr->stop = stop;
}


void
DFillArc(drawarea, x, y, width, height, start, stop)
	Drawarea        drawarea;
	double          x, y, width, height;
	int		start, stop;
{
	Dd_arc_data    *ptr;
	static const Display_list_vec vec = {
			Dfillarc,
			NULL,
			NULL,
	};

	ptr = (Dd_arc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec, 
			sizeof(Dd_arc_data));
	ptr->x = x;
	ptr->y = y;
	ptr->width = width;
	ptr->height = height;
	ptr->start = start;
	ptr->stop = stop;
}


void
DDrawString(drawarea, x, y, string, length)
	Drawarea        drawarea;
	double          x, y;
	int             length;
	char           *string;
{
	Dd_string_data *ptr;
	static const Display_list_vec vec = {
			Ddrawstring,
			Dmapstring,
			NULL,
	};

	ptr = (Dd_string_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_string_data) + length);
	ptr->x = x;
	ptr->y = y;
	ptr->length = length;
	strncpy(ptr->chars, string, length);
}


void
DFillPoly(drawarea, points, npoints)
	Drawarea        drawarea;
	DPoint		*points;
	int             npoints;
{
	Dd_poly_data   *ptr;
	int             i;
	static const Display_list_vec vec = {
			Dfillpoly,
			Dmapfillpoly,
			NULL,
	};

	ptr = (Dd_poly_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_poly_data) + sizeof(DPoint) * (npoints-1));

	ptr->npoints = npoints;
	memcpy((char*) &ptr->points[0], (char*) points,
			sizeof(DPoint) * npoints);
}


void
DDrawPoly(drawarea, points, npoints)
	Drawarea        drawarea;
	DPoint		*points;
	int             npoints;
{
	Dd_poly_data   *ptr;
	int             i;
	static const Display_list_vec vec = {
			Ddrawlines,
			NULL,
			NULL,
	};

	ptr = (Dd_poly_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_poly_data) + sizeof(DPoint) * npoints);

	ptr->npoints = npoints+1;
	memcpy((char*) &ptr->points[0], (char*) points,
			sizeof(DPoint) * npoints);

	ptr->points[npoints] = points[0]; /* closes the shape */
}


void
DDrawLines(drawarea, points, npoints)
	Drawarea        drawarea;
	DPoint          points[];
	int             npoints;
{
	Dd_poly_data   *ptr;
	int             i;
	static const Display_list_vec vec = {
			Ddrawlines,
			NULL,
			NULL,
	};

	ptr = (Dd_poly_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_poly_data) + sizeof(DPoint) * (npoints-1));

	ptr->npoints = npoints;
	memcpy((char*) &ptr->points[0], (char*) points,
			sizeof(DPoint) * npoints);
}


void
DDrawImage(drawarea, left, top, image, mask)
	Drawarea	drawarea;
	double		left, top;
	Server_image	image, mask;
{
	Dd_image_data   *ptr;
	static const Display_list_vec vec = {
			Ddrawimage,
			Dmapimage,
			NULL,
	};

	ptr = (Dd_image_data *) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(Dd_image_data));

	ptr->left = left;
	ptr->top = top;
	ptr->image = image;
	ptr->mask = mask;
}


static const Display_list_vec update_vec = {
	update_gc,
	map_update_gc,
	NULL,
};

void
VSetFont(drawarea, font)
	Drawarea        drawarea;
	Xv_Font         font;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_Font;
	ptr->data.font = font;
}


void
VSetCms(drawarea, cms)
	Drawarea        drawarea;
	Cms		cms;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_Cms;
	ptr->data.cms = cms;
}


void
VSetTile(drawarea, server_image)
	Server_image    server_image;
	Drawarea        drawarea;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_Tile;
	ptr->data.server_image = server_image;
}


void
VSetStipple(drawarea, server_image)
	Server_image    server_image;
	Drawarea        drawarea;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_Stipple;
	ptr->data.server_image = server_image;
}


void
VSetFillStyle(drawarea, fillstyle)
	Drawarea        drawarea;
	int             fillstyle;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_FillStyle;
	ptr->data.fillstyle = fillstyle;
}


void
VSetLineWidth(drawarea, linewidth)
	Drawarea        drawarea;
	int		linewidth;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_LineWidth;
	ptr->data.linewidth = linewidth;
}


void
VSetLineStyle(drawarea, linestyle)
	Drawarea        drawarea;
	int		linestyle;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_LineStyle;
	ptr->data.linestyle = linestyle;
}


void
VSetColor(drawarea, color)
	Drawarea        drawarea;
	int		color;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_Color;
	ptr->data.color = color;
}


void
VSetBgColor(drawarea, color)
	Drawarea        drawarea;
	int		color;
{
	Dl_gc_data     *ptr;

	ptr = (Dl_gc_data *) display_list_append(drawarea, 
			(Display_list_vec*) &update_vec,
			sizeof(Dl_gc_data));
	ptr->type = Set_BgColor;
	ptr->data.color = color;
}


typedef struct {
	Display_list_cmd	cmd;
	Xv_opaque		key;
} V_mark_data;

static void
vSetMarkRenderProc(dl_arg, mark_data)
	Display_list_arg	*dl_arg;
	V_mark_data	*mark_data;
{
	dl_arg->current_mark_key = mark_data->key;
}
	
static int
vSetMarkMapProc(dl_arg, mark_data, event)
	Display_list_arg *dl_arg;
	V_mark_data	*mark_data;
	Event		*event;
{
	dl_arg->current_mark_key = mark_data->key;
	return FALSE;
}

void
VSetMarkKey(drawarea, key)
	Drawarea	drawarea;
	Xv_opaque	key;
{
	V_mark_data	*ptr;
	static const Display_list_vec vec = {
			vSetMarkRenderProc,
			vSetMarkMapProc,
			NULL,
	};

	ptr = (V_mark_data*) display_list_append(drawarea, 
			(Display_list_vec*) &vec,
			sizeof(V_mark_data));

	ptr->key = key;
}


void
VFlush(drawarea)
	Drawarea        drawarea;
{
	Rectobj_info   *rinfo = RECTOBJ_PRIVATE(drawarea);
	Xv_window       xv_win;
	Xv_xrectlist    xrects;
	extern	void	drawarea_render_display_list();

	if (!rinfo->shared_info)
		return;

	/* 
	 * The following should use the general repaint strategy, but in 
	 * those cases where the application knows that it is the only 
	 * thing that needs refreshing, this is faster - it only paints
	 * that which has changed.
	 */
		
	xrects.count = 1;
	xrects.rect_array[0] = *(XRectangle *) & rinfo->rect;

	CANVAS_EACH_PAINT_WINDOW(rinfo->shared_info->canvas_shell, xv_win)
		drawarea_render_display_list(drawarea,
				      rinfo->shared_info->dpy,
				      xv_get(xv_win, XV_XID),
				      &xrects,
				      1);
		rectobj_paint_children(drawarea,
				       rinfo->shared_info->dpy,
				       xv_get(xv_win, XV_XID),
				       &xrects);
	CANVAS_END_EACH
}

