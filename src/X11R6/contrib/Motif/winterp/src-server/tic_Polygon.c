/* -*-C-*-
********************************************************************************
*
* File:         tic_Polygon.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tic_Polygon.c,v 2.1 1994/06/06 15:41:06 npm Exp $
* Description:  TANGO:POLYGON_IMAGE_CLASS (subclass of TANGO:IMAGE_CLASS).
* Author:       Niels P. Mayer
* Created:      Sat May 15 21:42:02 1993
* Modified:     Sun Jun  5 14:28:10 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/tic_Polygon.c,v 2.1 1994/06/06 15:41:06 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "winterp.h"
#include "w_funtab.h"
#include "tango.h"


/*******************************************************************************
 * Special routine to get args from image description list. This 
 * is used both by Tango_Ellispe_Image_Class_Method_ISNEW() and
 * Xtango_Create_Composite_Image()
 *******************************************************************************/
Boolean Xtango_Get_Polygon_Image_Args(err_string, err_lval, loc_x_float, loc_y_float, lval_color,
				      vertex_seq_len, x_vertex_sequence, y_vertex_sequence, fill_float)
     char*  *err_string;	/* If routine returns FALSE, returns a string w/ error message */
     LVAL   *err_lval;		/* If routine returns FALSE, returns an LVAL w/ eroneous value */
     double *loc_x_float;	/* If routine returns TRUE, returns x_location */
     double *loc_y_float;	/* If routine returns TRUE, returns y_location */
     LVAL   *lval_color;	/* If routine returns TRUE, returns color LVAL (STRING/FIXNUM) */
     int    *vertex_seq_len;	/* If routine returns TRUE, returns # of vertices */
     double x_vertex_sequence[];/* If routine returns TRUE, writes x-axes of vertices here */
     double y_vertex_sequence[];/* If routine returns TRUE, writes y-axes of vertices here */
     double *fill_float;	/* If routine returns TRUE, returns fill-value */
{
  extern LVAL s_unbound;
  LVAL lval_arg;
  int i;

  /*
   * get <location_coord> from argument stack
   */
  if (moreargs()) {
    lval_arg = nextarg();
    if (complexp(lval_arg) && floatp(getelement(lval_arg, 0)) && floatp(getelement(lval_arg, 1))) {
      *loc_x_float = (double) getflonum(getelement(lval_arg, 0));
      *loc_y_float = (double) getflonum(getelement(lval_arg, 1));
    }
    else {
      *err_string = string_err_msg_bad_loc_coord;
      if (!complexp(lval_arg))
	*err_lval = lval_arg;
      else if (!floatp(getelement(lval_arg, 0)))
	*err_lval = getelement(lval_arg, 0);
      else if (!floatp(getelement(lval_arg, 1)))
	*err_lval = getelement(lval_arg, 1);
      else
	*err_lval = lval_arg;
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_loc_coord;
    *err_lval = s_unbound;
    return (FALSE);
  }

  /* get a sequence of <offset_coord> specifying the vertices of the polygon */
  for (i = 0; (moreargs() && complexp(*xlargv)); i++) {
    lval_arg = nextarg();
    if (i < TANGO_VERTEX_SEQ_MAX_SIZE) {
      if (floatp(getelement(lval_arg, 0)) && floatp(getelement(lval_arg, 1))) {
	x_vertex_sequence[i] = (double) getflonum(getelement(lval_arg, 0));
	y_vertex_sequence[i] = (double) getflonum(getelement(lval_arg, 1));
      }
      else {
	*err_string = string_err_msg_bad_poly_offset;
	if (!floatp(getelement(lval_arg, 0)))
	  *err_lval = getelement(lval_arg, 0);
	else if (!floatp(getelement(lval_arg, 1)))
	  *err_lval = getelement(lval_arg, 1);
	else
	  *err_lval = lval_arg;
	return (FALSE);
      }
    }
    else {
      *err_string = string_err_msg_too_many_poly_offsets;
      *err_lval = lval_arg;
      return (FALSE);
    }
  }
  if (i)
    *vertex_seq_len = i + 1;
  else {			/* make sure there's at least one vertex. */
    *err_string = string_err_msg_too_few_poly_offsets;
    *err_lval = s_unbound;
    return (FALSE);
  }

  /*
   * get <tango_color> from argument stack
   */
  if (moreargs()) {
    lval_arg = nextarg();
    if (stringp(lval_arg) || fixp(lval_arg))
      *lval_color = lval_arg;
    else {
      *err_string = string_err_msg_bad_color;
      *err_lval = lval_arg;
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_color;
    *err_lval = s_unbound;
    return (FALSE);
  }
  
  /*
   * get <fill_float> from argument stack
   */
  if (moreargs()) {
    lval_arg = nextarg();
    if (floatp(lval_arg))
      *fill_float = (double) getflonum(lval_arg);
    else {
      *err_string = string_err_msg_bad_fill;
      *err_lval = lval_arg;
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_fill;
    *err_lval = s_unbound;
    return (FALSE);
  }

  return (TRUE);
}


/*****************************************************************************
 * (send TANGO:POLYGON_IMAGE_CLASS :new
 *       [:show] [<visible_kwd>] <tango_widget>
 *       <location_coord>
 *       <vertex-offset-coord-0>
 *       [<vertex-offset-coord-1> [<vertex-offset-coord-2>
 * 			       ...
 * 			          [<vertex-offset-coord-N>]]]
 *       <tango_color>
 *       <fill_float>)
 * 	==> RETURNS an <tango_image> object.
 * 
 * [:show] -- OPTIONAL :show keyword. If present, displays image
 * immediately, else the image will be displayed along with the next
 * animation frame. See :TAP_SHOW :TX_DELAY.
 * 
 * [<visible_kwd>] -- OPTIONAL :VISIBLE or :INVISIBLE keyword. If
 * omitted, :VISIBLE is assumed. See also :TX_VISIBLE, :TAP_VIS_TOGGLE.
 * 
 * <tango_widget> -- an instance of TANGO:WIDGET_CLASS.
 * 
 * <location_coord> -- the location for placing the image. A
 * COMPLEX number, of form #C(<loc_x> <loc_y>), where <loc_x> is a
 * FLONUM, typically [0.0 - 1.0] representing the X-axis location;
 * <loc_y> is a FLONUM, typically [0.0 - 1.0] representing the Y-axis
 * location.
 * 
 * <vertex-offset-coord-N> -- for i=0 to 6, this represents a COMPLEX
 * number of form #C(<offset-x> #<offset-y>) where <offset-x> represents
 * the x-offset and <offset-y> represents the y-offset of the polygon
 * vertex from its <location_coord>. These offsets can be either
 * positive or negative. Xtango polygon images support polygons with
 * eight or less sides.  Therefore the sequence of offsets must be <= 7
 * elements -- the first vertex is implicity <0,0> and is not to be
 * included in the sequence.
 * 
 * <tango_color> -- The color of the image; it can either be a string
 * color name, or a FIXNUM representing the tango pixel value. If given
 * a string, it is looked up in the X11 file /usr/lib/X11/rgb.txt.
 * 
 * <fill_float> -- the fill value of the image, a FLONUM between 0.0 and
 * 1.0. 0.0 corresponds to an unfilled outline and 1.0 corresponds to
 * 100 per cent fill in the given color.  (Forty graduated fill styles
are actually implemented.)
 * ==========================================================================
 * TANGO_IMAGE
 * TANGOimage_create(TANGO_IMAGE_TYPE_POLYGON,lx,ly,vis,col,vert,vx,vy,fill)
 *	double lx,ly;
 *	int vis;
 *	TANGO_COLOR col;
 *	int vert;
 *	double vx[], vy[];
 *	double fill;
 ****************************************************************************/
LVAL Tango_Polygon_Image_Class_Method_ISNEW()
{
  TANGO_IMAGE image;
  WINTERP_TANGO_CONTEXT context;
  LVAL o_widget;
  char*  err_string;
  LVAL   err_lval;
  double loc_x_float;
  double loc_y_float;
  LVAL   lval_color;
  int    vertex_seq_len;
  double x_vertex_sequence[TANGO_VERTEX_SEQ_MAX_SIZE];
  double y_vertex_sequence[TANGO_VERTEX_SEQ_MAX_SIZE];
  double fill_float;

  /* get <self> from argument stack */
  LVAL self
    = Tcls_Get_OBJECT_Arg_Returning_TANGOIMAGEOBJ();

  /* get OPTIONAL [:show] from argument stack */
  int show_p
    = (moreargs() && (*xlargv == k_SHOW))
      ? (nextarg(), TRUE)
	: FALSE;

  /* get OPTIONAL [<visible_kwd>] from argument stack */
  int visible_p
    = (moreargs() && ((*xlargv == k_VISIBLE) || (*xlargv == k_INVISIBLE)))
      ? ((nextarg() == k_VISIBLE) ? TRUE : FALSE)
	: TRUE;

  /* get <tango_widget> from argument stack */
  Widget widget_id
    = Xtango_Get_TANGO_WIDGETOBJ_Returning_Validated_WidgetID(&o_widget);

  /* get image-specific arguments */
  if (!Xtango_Get_Polygon_Image_Args(&err_string, &err_lval,
				     &loc_x_float, &loc_y_float,
				     &lval_color,
				     &vertex_seq_len, x_vertex_sequence, y_vertex_sequence,
				     &fill_float))
    xlerror(err_string, err_lval);

  xllastarg();

  context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

  image = TANGOimage_create(TANGO_IMAGE_TYPE_POLYGON,
			    loc_x_float,
			    loc_y_float,
			    visible_p,
			    Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error(lval_color),
			    vertex_seq_len,
			    x_vertex_sequence,
			    y_vertex_sequence,
			    fill_float);
  
  Tcls_Initialize_TANGOIMAGEOBJ(self, image, context);

  if (show_p)
    TAPshow(image);

  Xtango_Restore_Context();

  return (self);
}


/******************************************************************************
 * (send <tango_polygon_image> :IMAGE_LOC <image_part>)
 * 	==> returns a COMPLEX number #C(<x> <y>), where <x> and 
 * 	    <y> are FLONUMS representing the location of the image.
 * 
 * <image_part> is either a KEYWORD SYMBOL or FIXNUM [0-7]
 * 	0	-- the "location", aka, the 0th vertex.
 * 	1	-- the 1st vertex
 * 	...        ...
 * 	7	-- the 7th vertex
 * 
 * 	:CTR	-- center
 * 	:NW	-- north west
 * 	:NE	-- north east
 * 	:E	-- east
 * 	:SE	-- south east
 * 	:S	-- south
 * 	:SW     -- south west
 * 	:W      -- west
 * 
 * This routine returns a COMPLEX FLONUM #C(<x> <y>) that corresponds to
 * the location of the given <image_part> of the given
 * <tango_polygon_image>.  Valid <image_part> include :CTR (center),
 * and the compass directions :NW, :N, :NE, :E, :SE, :S, :SW, :W. These
 * locations represent points on the bounding box of the polygon. If
 * <image_part> is a FIXNUM [0..7], then the location returned is the
 * actual location of the vertex, where vertex 0 is <location_coord> and
 * vertex 1 thru 7 represent the locations of the other vertices.
 * ----------------------------------------------------------------------------
 * TANGO_LOC
 * TANGOimage_loc (image,part) 
 *    TANGO_IMAGE image;
 *    TANGO_PART_TYPE part;
 ******************************************************************************/
LVAL Tango_Polygon_Image_Class_Method_IMAGE_LOC()
{
  TANGO_IMAGE image;
  LVAL o_image;
  TANGO_PART_TYPE part;
  int vertex_idx;
  LVAL lval_vertex_idx;

  image	= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);

  if (moreargs() && (fixp(*xlargv)))
    vertex_idx = getfixnum((lval_vertex_idx = nextarg()));
  else {
    vertex_idx = -1;		/* tell code below that we'll be calling TANGOimage_loc() */
    part = Xtango_Get_TANGO_PART_TYPE_Arg();
  }
  xllastarg();

  if (vertex_idx == -1) {	/* this case occurs only when a TANGO_PART_TYPE arg is given... */
    double loc_x, loc_y;
    TANGO_LOC loc;
    Xtango_Save_Set_Context(get_tangoimageobj_context(o_image));
    Xtango_Check_Initialized();
    loc = TANGOimage_loc(image, part);
    TANGOloc_inquire(loc, &loc_x, &loc_y);
    XtFree((char*) loc);
    Xtango_Restore_Context();
    return (newdcomplex(loc_x, loc_y));
  }
  else {
    TANGO_POLYGON_PTR polygon = (TANGO_POLYGON_PTR) image->object;
    if ((vertex_idx < 0) || (vertex_idx > 7))
      xlerror("Invalid tango polygon image vertex index", lval_vertex_idx);
    else if (vertex_idx > (polygon->sides - 1)) /* remember that polygon->sides is 1 greater than # of elts in polygon->vertex[] */
      xlerror("Tango polygon image vertex index greater than number of vertices", lval_vertex_idx);
    else if (vertex_idx == 0)
      return (newdcomplex((double) image->loc[0],
			  (double) image->loc[1]));
    else			/* 1 <= vertex_idx <= 7 */
      return (newdcomplex((double) (image->loc[0] + polygon->vertex[vertex_idx - 1][0]),
			  (double) (image->loc[1] + polygon->vertex[vertex_idx - 1][1])));
  }
}


/*****************************************************************************
 * (
 *	#C(<location-x> <location-y>)
 *	#C(<vert-0-x> <vert-0-y>)
 *		...
 *	#C(<vert-N-x> <vert-N-y>)
 *	<tango_color>
 *	<fill_float>
 * )
 ****************************************************************************/
LVAL Tango_Polygon_Image_Get_Values(image)
     TANGO_IMAGE image;
{
  LVAL lval_location, lval_vertex, lval_color, lval_fill, lval_result;
  int i;
  TANGO_POLYGON_PTR polygon = (TANGO_POLYGON_PTR) image->object;

  /* protect some pointers */
  xlstkcheck(5);
  xlsave(lval_location);
  xlsave(lval_vertex);
  xlsave(lval_color);
  xlsave(lval_fill);
  xlsave(lval_result);

  lval_fill = cvflonum((FLOTYPE) polygon->fill);
  lval_result = cons(lval_fill, lval_result);

  lval_color = cvfixnum((FIXTYPE) polygon->color);
  lval_result = cons(lval_color, lval_result);

  for (i = polygon->sides - 2 ; (i >= 0) ; i--) { /* we're consing vertices, so fetch vertices backwards... */
    lval_vertex = newdcomplex((double) polygon->vertex[i][0], (double) polygon->vertex[i][1]);
    lval_result = cons(lval_vertex, lval_result);
  }

  lval_location = newdcomplex((double) image->loc[0], (double) image->loc[1]);
  lval_result = cons(lval_location, lval_result);

  /* restore the stack */
  xlpopn(5);

  return(lval_result);
}


/*****************************************************************************
 * (send <polygon-image> :storeon)
 *	==> returns list (send TANGO:POLYGON_IMAGE_CLASS :new <visibility_kwd>
 *				*TANGO_WIDGET*
 *				#C(<location-x> <location-y)
 *				#C(<vert-0-x> <vert-0-y>)
 *					...
 *				#C(<vert-N-x> <vert-N-y>)
 *				<tango_color>
 *				<fill_float>)
 ****************************************************************************/
LVAL Tango_Polygon_Image_Class_Method_STOREON()
{
  extern LVAL s_SEND, k_NEW, s_TANGO_W; /* wc_Xtango.c */
  LVAL o_image, lval_result;
  TANGO_IMAGE image;

  xlsave1(lval_result);		/* protect from gc */

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  lval_result = Tango_Polygon_Image_Get_Values(image);

  lval_result = cons(s_TANGO_W,
		     lval_result);
  lval_result = cons((image->visible) ? k_VISIBLE : k_INVISIBLE,
		     lval_result);
  lval_result = cons(k_NEW,
		     lval_result);
  lval_result = cons(Tcls_Get_TANGOIMAGECLASS_Symbol_From_TANGOIMAGEOBJ(o_image),
		     lval_result);
  lval_result = cons(s_SEND,
		     lval_result);

  xlpop();
  return (lval_result);
}


/*****************************************************************************
 *
 ****************************************************************************/
Tic_Polygon_Init()
{
  LVAL o_TANGO_POLYGON_IMAGE_CLASS;

  /*--------------- create 'Class' instance 'TANGO:POLYGON_IMAGE_CLASS' --------------*/
  o_TANGO_POLYGON_IMAGE_CLASS =
    Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS("TANGO:POLYGON_IMAGE_CLASS",
					     TANGO_IMAGE_TYPE_POLYGON);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":ISNEW",
	   FTAB_Tango_Polygon_Image_Class_Method_ISNEW);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":STOREON",
	   FTAB_Tango_Polygon_Image_Class_Method_STOREON);

  /* :IMAGE_LOC methods differ among subclasses of TANGO:IMAGE_CLASS; most
   * use Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(), but
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS, and
   * TANGO:SPLINE_IMAGE_CLASS use their own methods.
   * Thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":IMAGE_LOC",
	   FTAB_Tango_Polygon_Image_Class_Method_IMAGE_LOC);

  /* Message :TX_COLOR applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:BITMAP_IMAGE_CLASS, thus can't add it to
   * TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_COLOR",
	   FTAB_Tango_Image_Class_Method_TX_COLOR);

  /* Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
   * thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_FILL",
	   FTAB_Tango_Image_Class_Method_TX_FILL);

  /* Message :TX_RESIZE doesn't apply to all subclasses of TANGO:IMAGE_CLASS, 
   * therefore can't add it to TANGO:IMAGE_CLASS metaclass. In particular,
   * :TX_RESIZE message doesn't exist on TANGO:POLYLINE_IMAGE_CLASS,
   * TANGO:POLYGON_IMAGE_CLASS, TANGO:SPLINE_IMAGE_CLASS, 
   * TANGO:BITMAP_IMAGE_CLASS, TANGO:TEXT_IMAGE_CLASS.
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE",
	   FTAB_Tango_Non_Poly_Image_Class_Method_TX_RESIZE);
   */

  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE1",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE1);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE2",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE2);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE3",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE3);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE4",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE4);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE5",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE5);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE6",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE6);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_RESIZE7",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE7);

  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB1",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB1);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB2",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB2);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB3",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB3);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB4",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB4);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB5",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB5);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB6",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB6);
  xladdmsg(o_TANGO_POLYGON_IMAGE_CLASS, ":TX_GRAB7",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB7);
}
