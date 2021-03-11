/* -*-C-*-
********************************************************************************
*
* File:         tic_Bitmap.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tic_Bitmap.c,v 2.1 1994/06/06 15:41:10 npm Exp $
* Description:  TANGO:BITMAP_IMAGE_CLASS (subclass of TANGO:IMAGE_CLASS).
* Author:       Niels P. Mayer
* Created:      Sat May 15 21:32:19 1993
* Modified:     Sun Jun  5 14:24:34 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/tic_Bitmap.c,v 2.1 1994/06/06 15:41:10 npm Exp $";

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

static Boolean Xtango_Bitmap_Array_to_C(/* LVAL lisp_val, char* *err_string, LVAL *err_lval, int* *result_bitmaps, int *num_bitmaps, int *bitmap_width, int *bitmap_height */);

/*******************************************************************************
 * Special routine to get args from image description list. This 
 * is used both by Tango_Bitmap_Image_Class_Method_ISNEW() and
 * Xtango_Create_Composite_Image()
 *******************************************************************************/
Boolean Xtango_Get_Bitmap_Image_Args(err_string, err_lval, loc_x_float, loc_y_float,
				     bitmaps, num_bitmaps, bitmap_width, bitmap_height)
     char*  *err_string;	/* If routine returns FALSE, returns a string w/ error message */
     LVAL   *err_lval;		/* If routine returns FALSE, returns an LVAL w/ eroneous value */
     double *loc_x_float;	/* If routine returns TRUE, returns x_location */
     double *loc_y_float;	/* If routine returns TRUE, returns y_location */
     int*   *bitmaps;		/* If routine returns TRUE, returns 3D array of bitmaps */
     int    *num_bitmaps;	/* If routine returns TRUE, returns number of bitmaps */
     int    *bitmap_width;	/* If routine returns TRUE, returns width of bitmaps */
     int    *bitmap_height;	/* If routine returns TRUE, returns height of bitmaps */
{
  extern LVAL s_unbound;
  LVAL lval_arg;

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

  /*
   * get <bitmap_array> from argument stack
   */
  if (moreargs()) {
    if (!Xtango_Bitmap_Array_to_C(nextarg(), err_string, err_lval, bitmaps, num_bitmaps, bitmap_width, bitmap_height)) { /* returns a 3-D array bitmaps[][][] */
      /* *err_string = --- set by Xtango_Bitmap_Array_to_C() */
      /* *err_lval   = --- set by Xtango_Bitmap_Array_to_C() */
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_bitmap_vector;
    *err_lval   = s_unbound;
    return (FALSE);
  }

  /* NOTE: if any args added past here, or any error returns occur after this point
     you should remember to free up 'bitmaps' malloc'd by Xtango_Bitmap_Array_to_C().
     If no errors get signalled, 'bitmaps' gets freed by
     Tango_Bitmap_Image_Class_Method_ISNEW() */

  return (TRUE);
}


/*****************************************************************************
 * (send TANGO:BITMAP_IMAGE_CLASS :new
 *	[:show]		--> OPTIONAL :show keyword. If present, displays image immediately, else will be displayed when next frame displayed. See :TAP_SHOW :TX_DELAY.
 *	[<visible_kwd>] --> OPTIONAL :VISIBLE or :INVISIBLE keyword. If omitted, :VISIBLE is assumed. See also :TX_VISIBLE, :TAP_VIS_TOGGLE
 *	<tango_widget>	--> instance of TANGO:WIDGET_CLASS
 *	<location_coord>--> COMPLEX #C(<loc_x loc_y)
 *	<bitmap_array>  --> 3-D array of FIXNUM (TANGO_COLOR).
 *	)
 *		==> RETURNS an <tango_image> object.
 *                           --------------------                     
 * [:show] -- OPTIONAL :show keyword. If present, displays image
 * immediately, else the image will be displayed along with the next
 * animation frame. See :TAP_SHOW :TX_DELAY.
 * 
 * [<visible_kwd>] -- OPTIONAL :VISIBLE or :INVISIBLE keyword. If
 * omitted, :VISIBLE is assumed. See also :TX_VISIBLE, :TAP_VIS_TOGGLE.
 * 
 * <tango_widget> -- an instance of TANGO:WIDGET_CLASS
 * 
 * <location_coord> -- the location for placing the bitmap image. A
 * COMPLEX number, of form #C(<loc_x> <loc_y>), where
 * <loc_x> is a FLONUM, typically [0.0 - 1.0] representing the X-axis
 * location; <loc_y> is a FLONUM, typically [0.0 - 1.0] representing the
 * Y-axis location.
 * 
 * <bitmap_array> -- a 3-D array of FIXNUMs (TANGO_COLOR). Each FIXNUM
 * represents a Pixel (TANGO_COLOR) value. For example, a "movie"
 * consisting of 3 4x4 bitmaps is represented by the following:
 * 	#(#(#(1 1 1 1)
 * 	     #(7 7 7 7) 
 * 	     #(7 7 7 7) 
 *  	     #(7 7 7 7)) 
 *  	   #(#(7 7 7 1) 
 *  	     #(7 7 1 7) 
 *  	     #(7 1 7 7) 
 *  	     #(1 7 7 7)) 
 *  	   #(#(7 7 7 1) 
 *  	     #(7 7 7 1) 
 *  	     #(7 7 7 1) 
 *  	     #(7 7 7 1)))
 * ==========================================================================
 * TANGO_IMAGE
 * TANGOimage_create(TANGO_IMAGE_TYPE_BITMAP...)
 ****************************************************************************/
LVAL Tango_Bitmap_Image_Class_Method_ISNEW()
{
  TANGO_IMAGE image;
  WINTERP_TANGO_CONTEXT context;
  LVAL o_widget;
  char*  err_string;
  LVAL   err_lval;
  double loc_x_float;
  double loc_y_float;
  int num_bitmaps, bitmap_width, bitmap_height;
  int* bitmaps;

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
  if (!Xtango_Get_Bitmap_Image_Args(&err_string, &err_lval,
				    &loc_x_float, &loc_y_float,
				    &bitmaps, &num_bitmaps,
				    &bitmap_width, &bitmap_height))
    xlerror(err_string, err_lval);

  /* NOTE: if any args added past here, or any error returns occur after this point
     you should remember to free up 'bitmaps' malloc'd by Xtango_Bitmap_Array_to_C().
     If no errors get signalled, 'bitmaps' gets freed below. */

  if (xlargc != 0) {		/* was xllastarg(), but need to free 'bitmaps' prior to signaling error */
    XtFree((char*) bitmaps);
    xltoomany();
  }

  context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

  image = TANGOimage_create(TANGO_IMAGE_TYPE_BITMAP,
			    loc_x_float,
			    loc_y_float,
			    visible_p,
			    bitmaps,
			    num_bitmaps,
			    bitmap_width,
			    bitmap_height);

  XtFree((char*) bitmaps);

  Tcls_Initialize_TANGOIMAGEOBJ(self, image, context);

  if (show_p)
    TAPshow(image);

  Xtango_Restore_Context();

  return (self);
}


/*****************************************************************************
 * The following is the lisp representation (passed in lisp_val) of an array
 * containing 3 4x4 pixmaps....
 *
 *               #(#(#(1 1 1 1)
 * 		     #(7 7 7 7) 
 * 		     #(7 7 7 7) 
 * 		     #(7 7 7 7)) 
 * 
 * 		   #(#(7 7 7 1) 
 * 		     #(7 7 1 7) 
 * 		     #(7 1 7 7) 
 * 		     #(1 7 7 7)) 
 * 
 * 		   #(#(7 7 7 1) 
 * 		     #(7 7 7 1) 
 * 		     #(7 7 7 1) 
 * 		     #(7 7 7 1)))
 *
 * This function returns a malloc'd array of ints of size
 * (num * width * height) which is passed to the C xtango bitmap functions.
 * It also returns the size parameters <num>, <width>, and <height>.
 * 
 * The caller must explicitly XtFree() the results of this procedure.
 ****************************************************************************/
static Boolean Xtango_Bitmap_Array_to_C(lisp_val, err_string, err_lval, result_bitmaps, num_bitmaps, bitmap_width, bitmap_height)
     LVAL	lisp_val;
     char*	*err_string;	/* If routine returns FALSE, returns a string w/ error message */
     LVAL	*err_lval;	/* If routine returns FALSE, returns an LVAL w/ eroneous value */
     int*	*result_bitmaps;
     int	*num_bitmaps;
     int	*bitmap_width;
     int	*bitmap_height;
{
  int n, y, x;
  int bitmaps_size, height_size, width_size;
  int* bitmaps;
  LVAL bitmap_vector, row_vector, elt;

  if (vectorp(lisp_val)) {	/* 'lisp_val' == #(<bitmap-1> ... <bitmap-n>);
				   <bitmap-i> == #(<row-1> ... <row-y>);
				   <row-i>    == #(<col-0> <col-1> ... <col-x>) */
    *num_bitmaps = bitmaps_size = getsize(lisp_val);
    bitmap_vector = getelement(lisp_val, 0);
    if (vectorp(bitmap_vector)) {
      *bitmap_height = height_size = getsize(bitmap_vector);
      row_vector = getelement(bitmap_vector, 0);
      if (vectorp(row_vector)) {
	*bitmap_width = width_size = getsize(row_vector);
	/* elt = getelement(row_vector, 0); */
      }
      else {
	*err_string = string_err_msg_bad_bitmap_row;
	*err_lval   = row_vector;
	return (FALSE);
      }
    }
    else {
      *err_string = string_err_msg_bad_bitmap;
      *err_lval   = bitmap_vector;
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_bitmap_vector;
    *err_lval   = lisp_val;
    return (FALSE);
  }

  if (bitmaps_size > MAX_BITMAPS) {
    *err_string = string_err_msg_too_many_bitmaps;
    *err_lval   = lisp_val;
    return (FALSE);
  }

  bitmaps = (int*) XtMalloc((unsigned) (bitmaps_size * width_size * height_size 
					* sizeof(int)));

  for (n = 0 ; (n < bitmaps_size) ; n++) {
    bitmap_vector = getelement(lisp_val, n);
    if (vectorp(bitmap_vector)) {
      if (getsize(bitmap_vector) != height_size) {
	XtFree((char*) bitmaps);
	*err_string = string_err_msg_bitmaps_of_different_heights;
	*err_lval   = bitmap_vector;
	return (FALSE);
      }
      for (y = 0 ; (y < height_size) ; y++) {
	row_vector = getelement(bitmap_vector, y);
	if (vectorp(row_vector)) {
	  if (getsize(row_vector) != width_size) {
	    XtFree((char*) bitmaps);
	    *err_string = string_err_msg_bitmaps_of_different_widths;
	    *err_lval   = row_vector;
	    return (FALSE);
	  }
	  for (x = 0 ; (x < width_size) ; x++) {
	    elt = getelement(row_vector, x);
	    if (fixp(elt))
	      bitmaps[(n * width_size * height_size) + (y * width_size) + x]
		= (int) getfixnum(elt);
	    else {
	      XtFree((char*) bitmaps);
	      *err_string = string_err_msg_bad_bitmap_array_elt;
	      *err_lval   = elt;
	      return (FALSE);
	    }
	  }
	}
	else {
	  XtFree((char*) bitmaps);
	  *err_string = string_err_msg_bad_bitmap_row;
	  *err_lval   = row_vector;
	  return (FALSE);
	}
      }
    }
    else {
      XtFree((char*) bitmaps);
      *err_string = string_err_msg_bad_bitmap;
      *err_lval   = bitmap_vector;
      return (FALSE);
    }
  }

  *result_bitmaps = bitmaps;
  return (TRUE);
}


/*****************************************************************************
 *
 ****************************************************************************/
LVAL Xtango_Pixmap_To_Lisp_2D_Array(src_pix, src_x, src_y, src_width, src_height)
     Pixmap	  src_pix;
     int	  src_x;
     int	  src_y;
     unsigned int src_width;
     unsigned int src_height;
{
  int x, y;
  LVAL lval_row_array, lval_result;
  XImage* xi = XGetImage(TANGO__data->display, src_pix, src_x, src_y, src_width, src_height,
			 AllPlanes, XYPixmap);

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(lval_row_array);
  xlsave(lval_result);

  lval_result = newvector((unsigned) src_height);
  for (y = 0; (y < src_height); ++y) {
    lval_row_array = newvector((unsigned) src_width);
    setelement(lval_result, y, lval_row_array);
    for (x = 0; (x < src_width); ++x) {
      setelement(lval_row_array, x, cvfixnum((FIXTYPE) Xtango_Pixel_To_TANGO_COLOR(XGetPixel(xi, x, y))));
    }
  }

  XDestroyImage(xi);

  /* restore the stack */
  xlpopn(2);

  return (lval_result);
}

/*****************************************************************************
 *
 ****************************************************************************/
LVAL Tango_Bitmap_Image_Get_Values(image)
     TANGO_IMAGE image;
{
  LVAL lval_location, lval_bmaps, lval_result;
  TANGO_BITMAP_PTR bmaps = (TANGO_BITMAP_PTR) image->object;
  int i;

  /* protect some pointers */
  xlstkcheck(3);
  xlsave(lval_location);
  xlsave(lval_bmaps);
  xlsave(lval_result);

  lval_bmaps = newvector((unsigned) bmaps->used); /* bmaps->used is set to the number of bitmap-movie-frames available in the given TANGO_BITMAP_IMAGE */
  for (i = 0; (i < bmaps->used); ++i) {
    setelement(lval_bmaps, i,
	       Xtango_Pixmap_To_Lisp_2D_Array(bmaps->bmap[i], /* source Pixmap */
					      0, 0, /* source x,y */
					      bmaps->width, bmaps->height)); /* source width*height */
  }
  lval_result = cons(lval_bmaps, lval_result);  

  lval_location = newdcomplex((double) image->loc[0], (double) image->loc[1]);
  lval_result = cons(lval_location, lval_result);

  /* restore the stack */
  xlpopn(3);

  return(lval_result);
}


/*****************************************************************************
 * (send <bitmap-image> :storeon)
 *	==> returns list (send TANGO:BITMAP_IMAGE_CLASS :new <visibility_kwd>
 *				*TANGO_WIDGET*
 *				#C(<location-x> <location-y)
 *				#( <bitmap-array-1> <bitmap-array-2> ... ))
 ****************************************************************************/
LVAL Tango_Bitmap_Image_Class_Method_STOREON()
{
  extern LVAL s_SEND, k_NEW, s_TANGO_W; /* wc_Xtango.c */
  LVAL o_image, lval_result;
  TANGO_IMAGE image;

  xlsave1(lval_result);		/* protect from gc */

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  lval_result = Tango_Bitmap_Image_Get_Values(image);

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


/******************************************************************************
 *
 ******************************************************************************/
Tic_Bitmap_Init()
{
  LVAL o_TANGO_BITMAP_IMAGE_CLASS;

  /*--------------- create 'Class' instance 'TANGO:BITMAP_IMAGE_CLASS' ---------------*/
  o_TANGO_BITMAP_IMAGE_CLASS =
    Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS("TANGO:BITMAP_IMAGE_CLASS",
					     TANGO_IMAGE_TYPE_BITMAP);
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":ISNEW",
	   FTAB_Tango_Bitmap_Image_Class_Method_ISNEW);
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":TX_SHUFFLE",
	   FTAB_Tango_Bitmap_Image_Class_Method_TX_SHUFFLE);
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":STOREON",
	   FTAB_Tango_Bitmap_Image_Class_Method_STOREON);

  /* :IMAGE_LOC methods differ among subclasses of TANGO:IMAGE_CLASS; most
   * use Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(), but
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS, and
   * TANGO:SPLINE_IMAGE_CLASS use their own methods.
   * Thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":IMAGE_LOC",
	   FTAB_Tango_Non_Poly_Image_Class_Method_IMAGE_LOC);

  /* Message :TX_COLOR applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:BITMAP_IMAGE_CLASS, thus can't add it to
   * TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":TX_COLOR",
           FTAB_Tango_Image_Class_Method_TX_COLOR);
   */

  /* Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
   * thus can't add it to TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":TX_FILL",
	   FTAB_Tango_Image_Class_Method_TX_FILL);
   */

  /* Message :TX_RESIZE doesn't apply to all subclasses of TANGO:IMAGE_CLASS, 
   * therefore can't add it to TANGO:IMAGE_CLASS metaclass. In particular,
   * :TX_RESIZE message doesn't exist on TANGO:POLYLINE_IMAGE_CLASS,
   * TANGO:POLYGON_IMAGE_CLASS, TANGO:SPLINE_IMAGE_CLASS, 
   * TANGO:BITMAP_IMAGE_CLASS, TANGO:TEXT_IMAGE_CLASS.
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":TX_RESIZE",
	   FTAB_Tango_Non_Poly_Image_Class_Method_TX_RESIZE);
   */
}
