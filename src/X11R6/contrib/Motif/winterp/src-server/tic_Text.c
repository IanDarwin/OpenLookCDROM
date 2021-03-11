/* -*-C-*-
********************************************************************************
*
* File:         tic_Text.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tic_Text.c,v 2.1 1994/06/06 15:41:04 npm Exp $
* Description:  TANGO:TEXT_IMAGE_CLASS (subclass of TANGO:IMAGE_CLASS).
* Author:       Niels P. Mayer
* Created:      Sat May 15 21:46:45 1993
* Modified:     Sun Jun  5 14:29:50 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/tic_Text.c,v 2.1 1994/06/06 15:41:04 npm Exp $";

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
Boolean Xtango_Get_Text_Image_Args(err_string, err_lval, loc_x_float, loc_y_float, lval_color,
				   font_string, text_string, centered_p)
     char*  *err_string;	/* If routine returns FALSE, returns a string w/ error message */
     LVAL   *err_lval;		/* If routine returns FALSE, returns an LVAL w/ eroneous value */
     double *loc_x_float;	/* If routine returns TRUE, returns x_location */
     double *loc_y_float;	/* If routine returns TRUE, returns y_location */
     LVAL   *lval_color;	/* If routine returns TRUE, returns color LVAL (STRING/FIXNUM) */
     char*  *font_string;	/* If routine returns TRUE, returns font string */
     char*  *text_string;	/* If routine returns TRUE, returns text string */
     int    *centered_p;	/* If routine returns TRUE, returns located-by-center predicate */
{
  extern LVAL  k_TANGO_PART_TYPE_C; /* from t_utils.c/Xtango_Get_TANGO_PART_TYPE_Arg() */
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

  /* get OPTIONAL [:ctr] from argument stack */
  *centered_p
    = (moreargs() && (*xlargv == k_TANGO_PART_TYPE_C))
      ? (nextarg(), TRUE)
	: FALSE;

  /*
   * get <text_str> from argument stack
   */
  if (moreargs()) {
    lval_arg = nextarg();
    if (stringp(lval_arg))
      *text_string = getstring(lval_arg);
    else {
      *err_string = string_err_msg_bad_text;
      *err_lval = lval_arg;
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_text;
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
   * get <font_string> from argument stack
   */
  if (moreargs()) {
    lval_arg = nextarg();
    if (null(lval_arg))		/* if NIL given, pass NULL indicating that xtango uses default font */
      *font_string = (char*) NULL;
    else if (stringp(lval_arg))
      *font_string = getstring(lval_arg);
    else {
      *err_string = string_err_msg_bad_fontstr;
      *err_lval = lval_arg;
      return (FALSE);
    }
  }
  else {
    *err_string = string_err_msg_bad_fontstr;
    *err_lval = s_unbound;
    return (FALSE);
  }

  return (TRUE);
}



/*****************************************************************************
 * (send TANGO:TEXT_IMAGE_CLASS :new
 *       [:show] [<visible_kwd>] <tango_widget>
 *       <location_coord> [:CTR]
 *       <text_str>
 *       <tango_color>
 *       <font_str>
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
 * <tango_widget> -- an instance of TANGO:WIDGET_CLASS
 * 
 * <location_coord> -- the location for placing the text image. A
 * COMPLEX number, of form #C(<loc_x> <loc_y>), where
 * <loc_x> is a FLONUM, typically [0.0 - 1.0] representing the X-axis
 * location; <loc_y> is a FLONUM, typically [0.0 - 1.0] representing the
 * Y-axis location.
 * 
 * [:CTR] -- an optional KEYWORD. If this keyword is supplied,
 * then <location_coord> above refers to the center of the text image.
 * If :CTR keyword not supplied, then <location_coord> refers
 * to the lower left corner of the text image.
 * 
 * <text_string> is the text to be displayed by the the tango image.
 * Special characters such as tab, newline, carriage-return, etc
 * are rendered by the associated character specified by <font_string>
 * rather than tabbing, or starting a new line of text. See
 * "note" below for hints on creating multiline text.
 * 
 * <tango_color> -- The color of the image; it can either be a string
 * color name, or a FIXNUM representing the tango pixel value. If given
 * a string, it is looked up in the X11 file /usr/lib/X11/rgb.txt.
 * 
 * <font_string> a STRING representing the font used for drawing.
 * If the value of <font_string> is NIL, then Xtango's default
 * font is used.
 * ==========================================================================
 * TANGO_IMAGE
 * TANGOimage_create(TANGO_IMAGE_TYPE_TEXT,lx,ly,vis,col,font,title,ctr)
 *	double lx,ly;
 *	int vis;
 *	TANGO_COLOR col;
 *	char* font;
 *	char* title;
 *	int ctr;
 ****************************************************************************/
LVAL Tango_Text_Image_Class_Method_ISNEW()
{
  TANGO_IMAGE image;
  WINTERP_TANGO_CONTEXT context;
  LVAL o_widget;
  char*  err_string;
  LVAL   err_lval;
  double loc_x_float;
  double loc_y_float;
  LVAL   lval_color;
  char*  font_string;
  char*  text_string;
  int	 centered_p;

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
  if (!Xtango_Get_Text_Image_Args(&err_string, &err_lval,
				  &loc_x_float, &loc_y_float,
				  &lval_color,
				  &font_string, &text_string,
				  &centered_p))
    xlerror(err_string, err_lval);

  xllastarg();

  context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

  image = TANGOimage_create(TANGO_IMAGE_TYPE_TEXT,
			    loc_x_float,
			    loc_y_float,
			    visible_p,
			    Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error(lval_color),
			    font_string,
			    text_string,
			    centered_p);

  Tcls_Initialize_TANGOIMAGEOBJ(self, image, context);

  if (show_p)
    TAPshow(image);

  Xtango_Restore_Context();

  return (self);
}


/*****************************************************************************
 *  (
 *	#C(<location-x> <location-y) [:ctr]
 *	<text_str>
 *	<tango_color>
 *	<font_str>
 *  )
 ****************************************************************************/
LVAL Tango_Text_Image_Get_Values(image)
     TANGO_IMAGE image;
{
  extern LVAL k_TANGO_PART_TYPE_C; /* from t_utils.c/Xtango_Get_TANGO_PART_TYPE_Arg() */
  LVAL lval_location, lval_text, lval_color, lval_font, lval_result;
  TANGO_TEXT_PTR text = (TANGO_TEXT_PTR) image->object;

  /* protect some pointers */
  xlstkcheck(5);
  xlsave(lval_location);
  xlsave(lval_text);
  xlsave(lval_color);
  xlsave(lval_font);
  xlsave(lval_result);

  if (text->font_name[0] == 0) {
    lval_result = cons(NIL, lval_result); /* NIL --> default font... */
  }
  else {
    lval_font = cvstring(text->font_name);
    lval_result = cons(lval_font, lval_result);
  }

  lval_color = cvfixnum((FIXTYPE) text->color);
  lval_result = cons(lval_color, lval_result);

  lval_text = cvstring(text->text);
  lval_result = cons(lval_text, lval_result);

  if (text->orient) {
    lval_result = cons(k_TANGO_PART_TYPE_C, lval_result);
  }

  lval_location = newdcomplex((double) image->loc[0], (double) image->loc[1]);
  lval_result = cons(lval_location, lval_result);

  /* restore the stack */
  xlpopn(5);

  return(lval_result);
}


/*****************************************************************************
 * (send <text-image> :storeon)
 *	==> returns list (send TANGO:TEXT_IMAGE_CLASS :new <visibility_kwd>
 *				*TANGO_WIDGET*
 *				#C(<location-x> <location-y) [:ctr]
 *				<text_str>
 *				<tango_color>
 *				<font_str>)
 ****************************************************************************/
LVAL Tango_Text_Image_Class_Method_STOREON()
{
  extern LVAL s_SEND, k_NEW, s_TANGO_W; /* wc_Xtango.c */
  LVAL o_image, lval_result;
  TANGO_IMAGE image;

  xlsave1(lval_result);		/* protect from gc */

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  lval_result = Tango_Text_Image_Get_Values(image);

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
Tic_Text_Init()
{
  LVAL o_TANGO_TEXT_IMAGE_CLASS;

  /*--------------- create 'Class' instance 'TANGO:TEXT_IMAGE_CLASS' -----------------*/
  o_TANGO_TEXT_IMAGE_CLASS =
    Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS("TANGO:TEXT_IMAGE_CLASS",
					     TANGO_IMAGE_TYPE_TEXT);
  xladdmsg(o_TANGO_TEXT_IMAGE_CLASS, ":ISNEW",
	   FTAB_Tango_Text_Image_Class_Method_ISNEW);
  xladdmsg(o_TANGO_TEXT_IMAGE_CLASS, ":STOREON",
	   FTAB_Tango_Text_Image_Class_Method_STOREON);

  /* :IMAGE_LOC methods differ among subclasses of TANGO:IMAGE_CLASS; most
   * use Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(), but
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS, and
   * TANGO:SPLINE_IMAGE_CLASS use their own methods.
   * Thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_TEXT_IMAGE_CLASS, ":IMAGE_LOC",
	   FTAB_Tango_Non_Poly_Image_Class_Method_IMAGE_LOC);

  /* Message :TX_COLOR applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:BITMAP_IMAGE_CLASS, thus can't add it to
   * TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_TEXT_IMAGE_CLASS, ":TX_COLOR",
	   FTAB_Tango_Image_Class_Method_TX_COLOR);

  /* Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
   * thus can't add it to TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGO_TEXT_IMAGE_CLASS, ":TX_FILL",
	   FTAB_Tango_Image_Class_Method_TX_FILL);
   */

  /* Message :TX_RESIZE doesn't apply to all subclasses of TANGO:IMAGE_CLASS, 
   * therefore can't add it to TANGO:IMAGE_CLASS metaclass. In particular,
   * :TX_RESIZE message doesn't exist on TANGO:POLYLINE_IMAGE_CLASS,
   * TANGO:POLYGON_IMAGE_CLASS, TANGO:SPLINE_IMAGE_CLASS, 
   * TANGO:BITMAP_IMAGE_CLASS, TANGO:TEXT_IMAGE_CLASS.
  xladdmsg(o_TANGO_TEXT_IMAGE_CLASS, ":TX_RESIZE",
	   FTAB_Tango_Non_Poly_Image_Class_Method_TX_RESIZE);
   */
}
