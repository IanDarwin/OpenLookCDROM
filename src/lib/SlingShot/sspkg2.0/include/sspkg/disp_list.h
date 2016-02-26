/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef DISP_LIST_DEFINED
#define DISP_LIST_DEFINED

#include <xview/font.h>
#include <xview/cms.h>
#include <xview/svrimage.h>
/* @(#) disp_list.h 1.18 92/11/08  */

/*
 * Routines and data structures for defining custom drawarea display 
 * list rendering functions.
 */

typedef struct {
	void		(*render)();
	int		(*map_event)();
	void		*assoc;		/* client handle for associating data */
} Display_list_vec;

typedef struct {
	Display_list_vec *vec;
	short		size;
} Display_list_cmd;

typedef struct {
	Drawarea	drawarea;
	Canvas_shell	canvas_shell;
	Rect		rect;			/* rect of drawarea */
	double		x_m, x_b, y_m, y_b;     /* interpolation */
	Xv_Font		font;
	XFontStruct	*font_info;
	Xv_opaque	current_mark_key;
	void		*assoc;			/* client handle */

	/* following only set during rendering */
	Display		*dpy;
	XID		win_xid;
	GC		gc;
	Cms		cms;

	/* private fields */
	int		index;
	void		*ext;
} Display_list_arg;


/* 
 * Generalized interface for adding to display list.
 */
EXTERN_FUNCTION(Display_list_cmd *display_list_append, (Drawarea, Display_list_vec*, int));

/*
 * Display list traversal functions.
 */
EXTERN_FUNCTION(Display_list_cmd *drawarea_init_traverse, (Drawarea, Display_list_arg*));
EXTERN_FUNCTION(Display_list_cmd *drawarea_next_traverse, (Display_list_arg*));

/*
 * Convert from "real" coordinate system to Xlib x, y
 */
#define dl_convert_rx(d, x)     ((d)->x_m * (x) + (d)->x_b)
#define dl_convert_ry(d, y)     ((d)->y_m * (y) + (d)->y_b)


/*
 * Convert from 0...10000 coordinate space to Xlib width, height, x, y
 */
#define dl_convert_width(d, w)  \
                ((int)((double)(w) * (double)((d)->rect.r_width-1) * .0001))

#define dl_convert_height(d, h) \
                ((int)((double)(h) * (double)((d)->rect.r_height-1) * .0001))

#define dl_convert_x(d, x)      \
                ((int)(dl_convert_width((d),(x)) + (d)->rect.r_left))

#define dl_convert_y(d, y)      \
                ((int)(dl_convert_height((d),(y)) + (d)->rect.r_top))


/*
 * #define wX2v(r, n)      (((double)(n)*10000./(double)((r)->r_width-1)))
 * #define hX2v(r, n)      (((double)(n)*10000./(double)((r)->r_height-1)))
 * #define xX2v(r, x)      ((wX2v((r),(x) - (r)->r_left))) 
 * #define yX2v(r, y)      ((hX2v((r),(y) - (r)->r_top)))
 */


typedef struct {
	double	x, y;
} DPoint;


/* drawarea built in display list functions */
EXTERN_FUNCTION(void VDrawLine, (Drawarea, short, short, short, short));
EXTERN_FUNCTION(void VDrawRectangle, (Drawarea, short, short, short, short));
EXTERN_FUNCTION(void VFillRectangle, (Drawarea, short, short, short, short));
EXTERN_FUNCTION(void VDrawArc, (Drawarea, short, short, short, short, int, int));
EXTERN_FUNCTION(void VFillArc, (Drawarea, short, short, short, short, int, int));
EXTERN_FUNCTION(void VDrawString, (Drawarea, short, short, char*, int));
EXTERN_FUNCTION(void VFillPoly, (Drawarea, XPoint*, int));
EXTERN_FUNCTION(void VDrawPoly, (Drawarea, XPoint*, int));
EXTERN_FUNCTION(void VDrawLines, (Drawarea, XPoint*, int));
EXTERN_FUNCTION(void VDrawImage, (Drawarea, short, short, Server_image, Server_image));
EXTERN_FUNCTION(void DDrawLine, (Drawarea, double, double, double, double));
EXTERN_FUNCTION(void DDrawRectangle, (Drawarea, double, double, double, double));
EXTERN_FUNCTION(void DFillRectangle, (Drawarea, double, double, double, double));
EXTERN_FUNCTION(void DDrawArc, (Drawarea, double, double, double, double, int, int));
EXTERN_FUNCTION(void DFillArc, (Drawarea, double, double, double, double, int, int));
EXTERN_FUNCTION(void DDrawString, (Drawarea, double, double, char*, int));
EXTERN_FUNCTION(void DFillPoly, (Drawarea, DPoint*, int));
EXTERN_FUNCTION(void DDrawPoly, (Drawarea, DPoint*, int));
EXTERN_FUNCTION(void DDrawLines, (Drawarea, DPoint*, int));
EXTERN_FUNCTION(void DDrawImage, (Drawarea, double, double, Server_image, Server_image));
EXTERN_FUNCTION(void VSetFont, (Drawarea, Xv_Font));
EXTERN_FUNCTION(void VSetCms, (Drawarea, Cms));
EXTERN_FUNCTION(void VSetTile, (Drawarea, Server_image));
EXTERN_FUNCTION(void VSetStipple, (Drawarea, Server_image));
EXTERN_FUNCTION(void VSetFillStyle, (Drawarea, int));
EXTERN_FUNCTION(void VSetLineWidth, (Drawarea, int));
EXTERN_FUNCTION(void VSetLineStyle, (Drawarea, int));
EXTERN_FUNCTION(void VSetColor, (Drawarea, int));
EXTERN_FUNCTION(void VSetBgColor, (Drawarea, int));
EXTERN_FUNCTION(void VSetMarkKey, (Drawarea, Xv_opaque));
EXTERN_FUNCTION(void VFlush, (Drawarea));
EXTERN_FUNCTION(void VClear, (Drawarea));
EXTERN_FUNCTION(double dl_convert_i2rx, (Drawarea, int)); 
EXTERN_FUNCTION(double dl_convert_i2ry, (Drawarea, int));

#endif

