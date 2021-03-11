/* -*-C-*-
********************************************************************************
*
* File:         tic_GIF.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tic_GIF.c,v 2.1 1994/06/06 15:41:08 npm Exp $
* Description:  TANGO:GIF_IMAGE_CLASS (subclass of TANGO:IMAGE_CLASS).
* Author:       Niels P. Mayer
* Created:      Sat May 15 21:32:19 1993
* Modified:     Sun Jun  5 14:26:14 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/tic_GIF.c,v 2.1 1994/06/06 15:41:08 npm Exp $";

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
#include "widgets/xgif.h"

extern Display* display;	/* winterp.c */
extern Screen*  screen;		/* winterp.c */
extern Colormap colormap;	/* winterp.c */


/*******************************************************************************
 * Special routine to get args from image description list. This 
 * is used both by Tango_GIF_Image_Class_Method_ISNEW() and
 * Xtango_Create_Composite_Image().
 *
 * If this routine returns 'True', then the returned value *alloc_cols is an
 * array (of length *num_alloc_cols) of Pixel values allocated by XAllocColor().
 * When the GIF image here is no longer being displayed or used, the caller
 * should deallocate the colors via
 * XFreeColors(disp, colormap, alloc_cols, num_alloc_cols, 0L).
 * 'alloc_cols' should be deallocated via XtFree() once this is done.
 *******************************************************************************/
Boolean Xtango_Get_GIF_Image_Args(err_string, err_lval, loc_x_float, loc_y_float, ximage, num_alloc_cols, alloc_cols)
     char*          *err_string;     /* If routine returns FALSE, returns a string w/ error message */
     LVAL           *err_lval;	     /* If routine returns FALSE, returns an LVAL w/ eroneous value */
     double         *loc_x_float;    /* If routine returns TRUE, returns x_location */
     double         *loc_y_float;    /* If routine returns TRUE, returns y_location */
     XImage*	    *ximage;	     /* If routine returns TRUE, returns XImage* representing the GIF */
     int            *num_alloc_cols; /* If routine returns TRUE, gives the number of colors allocated by XAllocColor() */
     unsigned long* *alloc_cols;     /* If routine returns TRUE, gives the array of pixel values alloc'd by XAllocColor() */
{
  extern LVAL s_unbound;
  extern LVAL k_verbose;	/* from xlisp/xlglob.c, init'd in xlisp/xlinit.c */
  extern LVAL k_NOVERBOSE;	/* from utils.c */
  LVAL lval_arg;
  LVAL lval_fname;
  Bool quiet_p;

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
   * get <gif_file_name> STRING from argument stack
   */
  if (moreargs()) {
    lval_fname = nextarg();
    if (stringp(lval_fname)) {
      /* txform GIF-file to XImage below, after retrieving optional :verbose arg */
    }
    else {			/* non-string GIF-file name given */
      *err_string = string_err_msg_bad_gif_fname;
      *err_lval   = lval_fname;
      return (FALSE);
    }
  }
  else {			/* missing GIF-file name arg */
    *err_string = string_err_msg_bad_gif_fname;
    *err_lval   = s_unbound;
    return (FALSE);
  }

  /*
   * retrieve optional :verbose arg
   */
  if (moreargs()) {
    lval_arg = nextarg();
    if (k_verbose == lval_arg) {
      quiet_p = False;		/* :VERBOSE arg --> don't be quiet */
    }
    else if (k_NOVERBOSE ==  lval_arg) {
      quiet_p = True;		/* :NOVERBOSE arg --> be quiet */
    }
    else {			/* bad :VERBOSE/:NOVERBOSE arg... */
      *err_string = string_err_msg_bad_gif_verbose_arg;
      *err_lval   = lval_arg;
      return (FALSE);
    }
  }
  else {
    quiet_p = True;		/* No :VERBOSE arg --> be quiet... */
  }

  if (GIF_To_XImage(display, screen, colormap, getstring(lval_fname), quiet_p,
		    err_string, ximage, num_alloc_cols, alloc_cols)) {
    /* *ximage         -- set by GIF_To_XImage() */
    /* *num_alloc_cols -- set by GIF_To_XImage() */
    /* *alloc_cols     -- set by GIF_To_XImage() */
    return (TRUE);
  }
  else {
    /* *err_string was set by GIF_To_XImage() */
    *err_lval = lval_fname;
    return (FALSE);
  }
}


/*****************************************************************************
 * (send TANGO:GIF_IMAGE_CLASS :new
 *       [:show] [<visible_kwd>] <tango_widget>
 *       <location_coord>
 *       <gif_file_name>
 *       [:VERBOSE|:NOVERBOSE]
 *       )
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
 * <gif_file_name> -- a STRING, the full path to the GIF file.
 * 
 * [:VERBOSE|:NOVERBOSE] -- optional keyword. If :NOVERBOSE or
 * no argument passed, information on the GIF is not printed.
 * If :VERBOSE argument passed, then information on the size of
 * the GIF, number of colors allocated exactly and number of colors
 * approximated, is printed to stdout.
 * 
 * Note: GIF colormaps are allocated exactly if your global colormap has
 * enough free colors. If not, then it attempts to substitute the
 * closest fit of colors from the global colormap.  For the applications
 * the author has considered, it was decided not to allocate a private
 * colormap for GIF images so that exact colors could be achieved on an
 * 8 bit system. This is because the GIF image is just one image among
 * many other tango images and Motif widgets that need to share a
 * colormap. For such situations, having the system colors go
 * "technicolor" in order to display GIF colors exactly was deemed
 * inappropriate...
 * 
 * Automatic memory/resource management note: colors allocated for a
 * particular GIF image are deallocated when the GIF image is
 * :TX_DELETE'd or when the TANGO:WIDGET_CLASS instance displaying the
 * GIF image gets destroyed. Likewise, the GIF data is also destroyed.
 * If you are running out of colors, you may find that you can reclaim
 * colors by explicitly destroying the image and forcing a garbage
 * collect. Likewise, if you are running out of client-side or
 * server-side memory, you will aslo want to destroy uneeded images.
 * Here's what you need to do:
 * 	(send <gif-image> :tx_delete :perform)
 * 	(gc)
 * ==========================================================================
 * TANGO_IMAGE
 * TANGOimage_create(TANGO_IMAGE_TYPE_PIXMAP,lx,ly,vis, ......)
 ****************************************************************************/
LVAL Tango_GIF_Image_Class_Method_ISNEW()
{
  TANGO_IMAGE    image;
  WINTERP_TANGO_CONTEXT context;
  LVAL           o_widget;
  char*          err_string;
  LVAL           err_lval;
  double         loc_x_float;
  double         loc_y_float;
  XImage*        ximage;	/* the Ximage */
  int            num_alloc_cols; /* the number of colors allocated by XAllocColor() for displaying the GIF */
  unsigned long* alloc_cols;	/* the array of pixel values alloc'd by XAllocColor() for displaying the GIF */

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
  if (!Xtango_Get_GIF_Image_Args(&err_string, &err_lval,
				 &loc_x_float, &loc_y_float,
				 &ximage, &num_alloc_cols, &alloc_cols))
    xlerror(err_string, err_lval);

  /* NOTE: if any args added past here, or any error returns occur after this point
     you should remember to free up 'ximage' created in Xtango_Get_GIF_Image_Args()
     via XDestroyImage(). If no errors get signalled, 'ximage' gets freed below... */

  if (xlargc != 0) {		/* was xllastarg(), but need to free ximage and dealloc colors prior to signaling error */
    XDestroyImage(ximage);	/* Destroy the XImage 'ximage' and the image data ximage->data */
    if (num_alloc_cols > 0)	/* Free up any colors allocated for the GIF... */
      XFreeColors(display, colormap, alloc_cols, num_alloc_cols, 0L);
    XtFree((char*) alloc_cols);
    xltoomany();
  }

  context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

  image = TANGOimage_create(TANGO_IMAGE_TYPE_PIXMAP,
			    loc_x_float,
			    loc_y_float,
			    visible_p,
			    ximage,
			    num_alloc_cols,
			    alloc_cols);

  XDestroyImage(ximage);	/* Destroy the XImage 'ximage' and the image data ximage->data */

  Tcls_Initialize_TANGOIMAGEOBJ(self, image, context);

  if (show_p)
    TAPshow(image);

  Xtango_Restore_Context();

  return (self);
}


/*****************************************************************************
 *  (
 *	<gif-file-name>
 *   )
 ****************************************************************************/
LVAL Tango_GIF_Image_Get_Values(image)
     TANGO_IMAGE image;
{
  /* TODO!! */
  return(NIL);
}


/*****************************************************************************
 * (send <GIF-image> :storeon)
 *	==> returns list (send TANGO:GIF_IMAGE_CLASS :new <visibility_kwd>
 *				*TANGO_WIDGET*
 *				#C(<location-x> <location-y)
 *                        )
 *
 * TODO: this method needs to return the GIF file name.
 ****************************************************************************/
LVAL Tango_GIF_Image_Class_Method_STOREON()
{
  extern LVAL s_SEND, k_NEW, s_TANGO_W; /* wc_Xtango.c */
  LVAL o_image, lval_result;
  TANGO_IMAGE image;

  xlsave1(lval_result);		/* protect from gc */

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  lval_result = Tango_GIF_Image_Get_Values(image);

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
Tic_GIF_Init()
{
  LVAL o_TANGO_GIF_IMAGE_CLASS;

  /*--------------- create 'Class' instance 'TANGO:GIF_IMAGE_CLASS' ---------------*/
  o_TANGO_GIF_IMAGE_CLASS =
    Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS("TANGO:GIF_IMAGE_CLASS",
					     TANGO_IMAGE_TYPE_PIXMAP);
  xladdmsg(o_TANGO_GIF_IMAGE_CLASS, ":ISNEW",
	   FTAB_Tango_GIF_Image_Class_Method_ISNEW);

  /* use the same :STOREON method as for TANGO:BITMAP_IMAGE_CLASS */
/* TODO:
   xladdmsg(o_TANGO_GIF_IMAGE_CLASS, ":STOREON",
	   FTAB_Tango_GIF_Image_Class_Method_STOREON);
*/

  /* :IMAGE_LOC methods differ among subclasses of TANGO:IMAGE_CLASS; most
   * use Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(), but
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS, and
   * TANGO:SPLINE_IMAGE_CLASS use their own methods.
   * Thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_GIF_IMAGE_CLASS, ":IMAGE_LOC",
	   FTAB_Tango_Non_Poly_Image_Class_Method_IMAGE_LOC);

  /* Message :TX_COLOR applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:BITMAP_IMAGE_CLASS, thus can't add it to
   * TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGO_GIF_IMAGE_CLASS, ":TX_COLOR",
           FTAB_Tango_Image_Class_Method_TX_COLOR);
   */

  /* Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
   * thus can't add it to TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGO_GIF_IMAGE_CLASS, ":TX_FILL",
	   FTAB_Tango_Image_Class_Method_TX_FILL);
   */

  /* Message :TX_RESIZE doesn't apply to all subclasses of TANGO:IMAGE_CLASS, 
   * therefore can't add it to TANGO:IMAGE_CLASS metaclass. In particular,
   * :TX_RESIZE message doesn't exist on TANGO:POLYLINE_IMAGE_CLASS,
   * TANGO:POLYGON_IMAGE_CLASS, TANGO:SPLINE_IMAGE_CLASS, 
   * TANGO:BITMAP_IMAGE_CLASS, TANGO:TEXT_IMAGE_CLASS.
  xladdmsg(o_TANGO_GIF_IMAGE_CLASS, ":TX_RESIZE",
	   FTAB_Tango_Non_Poly_Image_Class_Method_TX_RESIZE);
   */
}
